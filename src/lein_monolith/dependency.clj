(ns lein-monolith.dependency
  "Functions for working with dependency coordinates and graphs."
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [lein-monolith.color :refer [colorize]]
    [leiningen.core.main :as lein]))


;; ## Coordinate Functions

(defn condense-name
  "Simplifies a dependency name symbol with identical name and namespace
  components to a symbol with just a name."
  [sym]
  (when sym
    (cond-> (if (= (namespace sym) (name sym))
              (symbol (name sym))
              sym)
      (symbol? sym) (with-meta (meta sym)))))


(defn project-name
  "Extracts the (condensed) project name from a project definition map."
  [project]
  (when project
    (condense-name (symbol (:group project) (:name project)))))


(defn resolve-name
  "Given a set of valid project names, determine the match for the named
  project. This can be used to resolve the short name (meaning, no namespace)
  to a fully-qualified project name. Returns a resolved key from
  `project-names`, a collection of multiple matching keys, or nil if the
  resolution fails."
  [project-names sym]
  (let [valid-keys (set project-names)]
    (cond
      (valid-keys sym)
      sym

      (valid-keys (condense-name sym))
      (condense-name sym)

      (nil? (namespace sym))
      (let [candidates (filter #(= (name %) (name sym)) valid-keys)]
        (if (= 1 (count candidates))
          (first candidates)
          (seq candidates)))

      :else nil)))


(defn resolve-name!
  "Resolves a symbol to a single project name, or calls abort if no or multiple
  projects match."
  [project-names sym]
  (let [result (resolve-name project-names sym)]
    (cond
      (nil? result)
      (lein/abort "Could not resolve" sym "to any monolith subproject!")

      (coll? result)
      (lein/abort "Name" sym "resolves to multiple monolith subprojects:"
                  (str/join " " (sort result)))

      :else result)))


(defn unscope-coord
  "Removes the `:scope` entry from a leiningen dependency coordinate vector,
  if it is present. Preserves any metadata on the coordinate."
  [coord]
  (-> coord
      (->> (partition-all 2)
           (mapcat #(when-not (= :scope (first %)) %)))
      (vec)
      (with-meta (meta coord))))


(defn with-source
  "Attaches metadata to a dependency vector which notes the source project."
  [dependency project-name]
  (vary-meta dependency assoc :monolith/project project-name))


(defn dep-source
  "Retrieves the project which pulled in the dependency from metadata on the
  spec vector."
  [dependency]
  (:monolith/project (meta dependency)))



;; ## Dependency Graphs

(defn- collect-dependencies
  "Merges the project's top-level dependencies with all dependencies listed in
  the project's profiles to ensure the project has the proper dependency closure
  for compilation ordering."
  [project]
  (into #{}
        (map (comp condense-name first))
        (concat
          (:dependencies project)
          (mapcat (fn [[pname profile]]
                    (map (fn [d]
                           {:pre [(vector? d)]}
                           (update d 0
                                   (fn [sym]
                                     (cond-> sym
                                       (symbol? sym)
                                       (vary-meta assoc ::profile pname)))))
                         (:dependencies profile)))
                  (:profiles project)))))

(defn dependency-map
  "Converts a map of project names to definitions into a map of project names
  to sets of projects that node depends on."
  [projects]
  (->>
    (vals projects)
    (map collect-dependencies)
    (zipmap (keys projects))))


(defn upstream-keys
  "Returns a set of the keys which are upstream of a given node in the
  dependency map. Includes the root value itself."
  [dependencies root]
  (loop [result #{}
         queue (conj (clojure.lang.PersistentQueue/EMPTY) root)]
    (cond
      ; Nothing left to process.
      (empty? queue) result

      ; Already seen this node.
      (contains? result (peek queue))
      (recur result (pop queue))

      ; Add next set of dependencies.
      :else
      (let [node (peek queue)
            deps (dependencies node)]
        (recur (conj result node)
               (into (pop queue) (set/difference deps result)))))))


(defn downstream-keys
  "Returns a set of the keys which are downstream of a given node in the
  dependency map. Includes the root value itself."
  [dependencies root]
  (let [deps-on (fn deps-on
                  [n]
                  (set (keep (fn [[k deps]] (when (deps n) k))
                             dependencies)))]
    (loop [result #{}
           queue (conj (clojure.lang.PersistentQueue/EMPTY) root)]
      (cond
        ; Nothing left to process.
        (empty? queue) result

        ; Already seen this node, deps are either present or already queued.
        (contains? result (peek queue))
        (recur result (pop queue))

        ; Add next set of dependencies.
        :else
        (let [node (peek queue)
              consumers (deps-on node)]
          (recur (conj result node)
                 (into (pop queue) (set/difference consumers result))))))))

(defn- unqualify-profile [dep]
  (if (and (vector? dep)
           (-> dep (nth 0) #{::profile}))
    (nth dep 1)
    dep))

(defn- qualify-profile [dep]
  (if-some [profile (-> dep meta ::profile)]
    [::profile dep]
    dep))

(defn topological-sort
  "Returns a sequence of the keys in the map `m`, ordered such that no key `k1`
  appearing before `k2` satisfies `(contains? (upstream-keys m k1) k2)`. In
  other words, earlier keys do not transitively depend on any later keys."
  ([m]
   (let [;used synchronously, rethink approach if parallelizing this function
         warned-cycle (atom nil)
         topological-sort
         (fn topological-sort [m]
           (when (seq m)
             ; allow cycles between regular dependencies and profile
             ; dependencies. for example, `a` is a root dependency
             ; in the following examples.
             ; 
             ; + a (test profile)
             ;   + b (test profile)
             ;     + a (no profile)
             ; + a (test profile)
             ;   + a (no profile)
             (if-some [roots
                       (not-empty
                         (let [find-roots (fn [f]
                                            (into #{}
                                                  (map unqualify-profile)
                                                  (apply set/difference
                                                         (set (keys m))
                                                         (map #(into #{}
                                                                     (map f)
                                                                     %)
                                                              (vals m)))))]
                           ;first attempt to find roots without
                           ; special treatment to profile dependencies
                           (or (not-empty (find-roots identity))
                               (let [roots (find-roots qualify-profile)]
                                 ;we resolved the cycle by treating profiles specially,
                                 ; so emit a warning.
                                 (when (and (seq roots)
                                            (not @warned-cycle))
                                   (some (fn [root]
                                           (when-let [cyclic-profile-dep
                                                      (some
                                                        ; find the dep that cycles
                                                        (fn [d]
                                                          (when (contains? (m d) root)
                                                            d))
                                                        (m root))]
                                             (let [dep-profile (-> cyclic-profile-dep
                                                                   meta ::profile)
                                                   reverse-dep-profile (-> ((m cyclic-profile-dep) root)
                                                                           meta
                                                                           ::profile)]
                                               (binding [*out* *err*]
                                                 (println
                                                   (let [msg
                                                         (str "WARNING: "
                                                              root
                                                              " and "
                                                              cyclic-profile-dep
                                                              " form a dependency cycle! "
                                                              "(Only reporting first cycle, may be more)\n"
                                                              "- "
                                                              (if dep-profile
                                                                (str root
                                                                     " depends on "
                                                                     cyclic-profile-dep
                                                                     " in its "
                                                                     dep-profile
                                                                     " profile")
                                                                (str cyclic-profile-dep
                                                                     " is a normal dependency of "
                                                                     root))
                                                              "\n"
                                                              "- "
                                                              (if reverse-dep-profile
                                                                (str cyclic-profile-dep
                                                                     " depends on "
                                                                     root
                                                                     " in its "
                                                                     reverse-dep-profile
                                                                     " profile")
                                                                (str cyclic-profile-dep
                                                                     " is a normal dependency of "
                                                                     root))
                                                              "\n"
                                                              (str "Resolving the cycle by "
                                                                   (if (every? roots [root cyclic-profile-dep])
                                                                     (str "building " root " and " cyclic-profile-dep
                                                                          " in an arbitrary order relative to one-another.")
                                                                     (str "building " cyclic-profile-dep " before " root "."))))]
                                                     (->> msg
                                                          str/split-lines
                                                          (map #(str "[lein-monolith] " %))
                                                          (str/join "\n"))))))
                                             (reset! warned-cycle true)))
                                         roots))
                                   roots))))]
               ; Note that 'roots' here are keys which no other keys depend on, hence
               ; should appear *later* in the sequence.
               (concat (topological-sort (apply dissoc m roots))
                       (sort roots))
               (throw (ex-info "Cannot sort the keys in the given map, cycle detected!"
                               {:input m})))))]
     (topological-sort m)))
  ([m ks]
   (filter (set ks) (topological-sort m))))



;; ## Dependency Resolution

(defn sourced-dependencies
  "Given a project map, returns a sequence of dependency coordinates with
  metadata tracking the source."
  [project]
  (let [pn (project-name project)]
    (map #(with-source % pn) (:dependencies project))))


(defn select-dependency
  "Given a dependency name and a collection of specs for that dependency, either
  select one for use or return nil on conflicts."
  [dep-name specs]
  (let [specs (map unscope-coord specs)
        default-choice (first specs)
        projects-for-specs (reduce (fn [m d]
                                     (update m d (fnil conj []) (dep-source d)))
                                   {} specs)]
    (if (= 1 (count (distinct specs)))
      ; Only one (unique) dependency spec declared, use it.
      default-choice
      ; Multiple versions or specs declared! Warn and use the default.
      (do
        (->> (str "WARN: Multiple dependency specs found for "
                  (condense-name dep-name) " in "
                  (count (distinct (map dep-source specs)))
                  " projects - using " (pr-str default-choice) " from "
                  (dep-source default-choice))
             (colorize :red)
             (lein/warn))
        (doseq [[spec projects] projects-for-specs]
          (lein/warn (format "%-50s from %s"
                             (pr-str spec)
                             (str/join " " (sort projects)))))
        (lein/warn "")
        default-choice))))
