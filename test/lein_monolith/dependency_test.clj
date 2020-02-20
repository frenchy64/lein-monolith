(ns lein-monolith.dependency-test
  (:require
    [clojure.test :refer [deftest testing is]]
    [lein-monolith.dependency :as dep]))


(deftest coordinate-utilities
  (testing "condense-name"
    (is (nil? (dep/condense-name nil)))
    (is (= 'lein-monolith (dep/condense-name 'lein-monolith/lein-monolith)))
    (is (= 'example/foo (dep/condense-name 'example/foo))))
  (testing "project-name"
    (is (nil? (dep/project-name nil)))
    (is (= 'foo (dep/project-name {:group "foo", :name "foo"})))
    (is (= 'example/bar (dep/project-name {:group "example", :name "bar"}))))
  (testing "resolve-name"
    (let [projects '[foo baz/foo example/bar example/baz]]
      (is (nil? (dep/resolve-name projects 'qux)))
      (is (nil? (dep/resolve-name projects 'example/qux)))
      (is (= '[foo/baz bar/baz] (dep/resolve-name '[foo/baz bar/baz bar/qux] 'baz)))
      (is (= 'foo (dep/resolve-name projects 'foo)))
      (is (= 'foo (dep/resolve-name projects 'foo/foo)))
      (is (= 'example/bar (dep/resolve-name projects 'bar)))
      (is (= 'baz/foo (dep/resolve-name projects 'baz/foo)))
      (is (= 'example/baz (dep/resolve-name projects 'baz)))))
  (testing "unscope-coord"
    (is (= '[example/foo "1.0"] (dep/unscope-coord '[example/foo "1.0"])))
    (is (= '[example/bar "0.5.0" :exclusions [foo]]
           (dep/unscope-coord '[example/bar "0.5.0" :exclusions [foo]])))
    (is (= '[example/bar "0.5.0" :exclusions [foo]]
           (dep/unscope-coord '[example/bar "0.5.0" :exclusions [foo] :scope :test])))
    (is (= '[example/baz "0.1.0-SNAPSHOT"]
           (dep/unscope-coord '[example/baz "0.1.0-SNAPSHOT" :scope :test])))
    (is (= '[example/baz "0.1.0-SNAPSHOT" :classifier "test"]
           (dep/unscope-coord '[example/baz "0.1.0-SNAPSHOT" :scope :test :classifier "test"]))))
  (testing "source-metadata"
    (is (nil? (dep/dep-source [:foo "123"])))
    (is (= [:foo "123"] (dep/with-source [:foo "123"] 'example/bar)))
    (is (= 'example/bar (dep/dep-source (dep/with-source [:foo "123"] 'example/bar))))))


(deftest dependency-mapping
  (let [projects '{foo/a {:dependencies []}
                   foo/b {:dependencies [[foo/a "1.0.0"]]}
                   foo/c {:dependencies [[foo/a "1.0.0"]]}
                   foo/d {:dependencies [[foo/b "1.0.0"]
                                         [foo/c "1.0.0"]]}}]
    (is (= '{foo/a #{}, foo/b #{foo/a}, foo/c #{foo/a}, foo/d #{foo/b foo/c}}
           (dep/dependency-map projects))))
  (let [projects '{foo/a {:dependencies []}
                   foo/b {:dependencies [] :profiles {:test {:dependencies [[foo/a "1.0.0"]]}}}}]
    (is (= `{foo/a #{}, foo/b #{foo/a}}
           (dep/dependency-map projects))))
  (let [projects '{foo/a {:dependencies [[foo/b "1.0.0"]]}
                   foo/b {:dependencies []
                          :profiles {:test {:dependencies [[foo/a "1.0.0"]]}}}}]
    (is (= `{foo/a #{foo/b},
             foo/b #{foo/a}}
           (dep/dependency-map projects))))
  )


(deftest upstream-dependency-closure
  (let [deps {:a #{}, :b #{:a}, :c #{:a :b} :x #{:b} :y #{:c}}]
    (is (= #{:a} (dep/upstream-keys deps :a)))
    (is (= #{:a :b} (dep/upstream-keys deps :b)))
    (is (= #{:a :b :c} (dep/upstream-keys deps :c)))
    (is (= #{:a :b :x} (dep/upstream-keys deps :x)))
    (is (= #{:a :b :c :y} (dep/upstream-keys deps :y))))
  (let [deps '{a #{b}, b #{a}}]
    (is (= '#{a b} (dep/upstream-keys deps 'a)))
    (is (= '#{a b} (dep/upstream-keys deps 'b)))))


(deftest downstream-dependency-closure
  (let [deps {:a #{}, :b #{:a}, :c #{:a :b} :x #{:b} :y #{:c}}]
    (is (= #{:a :b :c :x :y} (dep/downstream-keys deps :a)))
    (is (= #{:b :c :x :y} (dep/downstream-keys deps :b)))
    (is (= #{:c :y} (dep/downstream-keys deps :c)))
    (is (= #{:x} (dep/downstream-keys deps :x)))
    (is (= #{:y} (dep/downstream-keys deps :y))))
  (let [deps {:a #{}, :b #{:a}, :c #{:a}, :d #{:b :c}}]
    (is (= #{:a :b :c :d} (dep/downstream-keys deps :a))))
  (let [deps '{a #{b}, b #{a}}]
    (is (= '#{a b} (dep/downstream-keys deps 'a)))
    (is (= '#{a b} (dep/downstream-keys deps 'b)))))

(defmacro is-warn-with-msgs [regex cmd]
  `(let [regexs# ~regex
         str# (with-out-str
                (binding [*err* *out*]
                  ~cmd))]
     (run! (fn [regex#]
             (is (re-find regex# str#)))
           (if (vector? regexs#)
             regexs#
             [regexs#]))))

(deftest topological-sorting
  (let [deps {:a #{}, :b #{:a}, :c #{:a :b} :x #{:b} :y #{:c}}]
    (is (= [:a :b :c :x :y] (dep/topological-sort deps)))
    (is (= [:b :c :x] (dep/topological-sort deps [:x :c :b]))))
  (let [deps {:a #{:b}, :b #{:c}, :c #{:a}}]
    (is (thrown-with-msg? Exception #"cycle detected"
          (dep/topological-sort deps))))
  (let [deps '{a #{a}}]
    (is (thrown-with-msg? Exception #"cycle detected"
          (dep/topological-sort deps))))
  (let [deps '{a #{b}, b #{a}}]
    (is (thrown-with-msg? Exception #"cycle detected"
          (dep/topological-sort deps))))
  ; these self-reference cases are weird
  (let [deps '{a #{^{::dep/profile :test} a}}]
    (is-warn-with-msgs
      [#"a depends on a in its :test profile"
       #"Resolving the cycle by building a and a in an arbitrary order relative to one-another"]
      (= '[a] (dep/topological-sort deps))))
  (let [deps '{a #{^{::dep/profile :test} a b}
               b #{}}]
    
    (is-warn-with-msgs
      [#"a depends on a in its :test profile"
       #"Resolving the cycle by building a and a in an arbitrary order relative to one-another"]
      (is (= '[b a] (dep/topological-sort deps)))))
  (let [deps '{a #{a b},
               b #{}}]
    (is (thrown-with-msg? Exception #"cycle detected"
          (dep/topological-sort deps))))
  ;b goes first because a uses it at runtime
  (let [deps '{a #{b}, b #{^{::dep/profile :test} a}}]
    (is-warn-with-msgs
      [#"b is a normal dependency of a"
       #"b depends on a in its :test profile"
       #"Resolving the cycle by building b before a"]
      (= '[b a] (dep/topological-sort deps))))
  ;both roots
  (let [deps '{a #{^{::dep/profile :test} b},
               b #{^{::dep/profile :test} a}}]
    (is-warn-with-msgs
      [#"a depends on b in its :test profile"
       #"b depends on a in its :test profile"
       #"Resolving the cycle by building a and b in an arbitrary order relative to one-another."]
      (= '[a b] (dep/topological-sort deps))))
  )
