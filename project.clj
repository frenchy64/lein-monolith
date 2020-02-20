; # version number
; everything before -classifier-cycle are the same as the upstream library.
; y in classifier-cycle-y should be incremented every time we release a new
; version based on the same upstream dep.
(defproject threatgrid/lein-monolith "1.4.1-SNAPSHOT-classifier-cycle-0"
  :description "Leiningen plugin for managing subrojects within a monorepo."
  :url "https://github.com/amperity/lein-monolith"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}

  :eval-in-leiningen true
  :deploy-branches ["classifier-cycle"]

  :dependencies
  [[manifold "0.1.8"]
   [rhizome "0.2.9"]]

  :hiera
  {:vertical false
   :show-external true
   :cluster-depth 1
   :ignore-ns #{clojure manifold}}

  :profiles
  {:dev {:plugins [[lein-cloverage "1.0.9"]]
         :dependencies [[org.clojure/clojure "1.9.0"]]}})
