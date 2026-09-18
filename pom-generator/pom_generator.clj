(ns pom-generator
  "Turns a `clojure -Strace` log into a flat, fully pinned pom.xml.

  GitHub's dependency graph only speaks Maven, so the versions it sees are
  whatever Maven resolves. Handing it the project's direct dependencies and
  letting Maven work out the rest reports Maven's answer, not tools.deps', and
  the two disagree - Maven takes the nearest declaration, tools.deps runs its
  own resolution. Every disagreement is either a vulnerability the graph never
  learns about or an alert that can never be closed.

  So instead we resolve with tools.deps, then write out every artifact it
  actually selected, pinned, with each node's children excluded so Maven has
  nothing left to decide."
  (:require [clojure.data.xml :as xml]
            [clojure.tools.deps.tree :as tree]))

(defn children [deps-node]
  (rest (tree-seq :children
                  (fn [{:keys [children]}]
                    (map (fn [[k v]] (assoc v :name k)) children))
                  deps-node)))

(defn- exclude-children [deps-node]
  (-> deps-node
      (update-in [:coord :exclusions] into (keys (:children deps-node)))
      (dissoc :children)))

(defn dedupe-libs
  "Collapses repeated coordinates, keeping the last version seen.

  A lib can be reached by several paths and appear more than once in the
  trace. The trace is ordered, so the last entry is the one resolution
  settled on - verified against the classpath. Emitting both would make the
  pom ambiguous and Maven would warn that the coordinates must be unique.
  Exclusions are unioned so nothing a dropped node pruned comes back."
  [deps]
  (->> deps
       (reduce (fn [acc {:keys [lib] :as dep}]
                 (assoc acc lib (if-let [prev (get acc lib)]
                                  (update-in dep [:coord :exclusions]
                                             into (get-in prev [:coord :exclusions]))
                                  dep)))
               {})
       vals
       (sort-by (comp str :lib))))

(defn effective-deps [trace]
  (let [tree (tree/trace->tree trace)]
    (->> (children tree)
         (map exclude-children)
         (remove #(-> % :coord :git/url))
         (filter :include)
         dedupe-libs)))

(defn exclusion-element [exclusion]
  (xml/element :exclusion {}
               (xml/element :groupId {} (namespace exclusion))
               (xml/element :artifactId {} (str (name exclusion)))))

(defn dependency-element [{dep-name :lib :keys [coord]}]
  (xml/element
   :dependency {}
   (xml/element :groupId {} (namespace dep-name))
   (xml/element :artifactId {} (str (name dep-name)))
   (xml/element :version {} (str (:mvn/version coord)))
    ;; Artifacts published as a pom with no jar - junixsocket-core is one -
    ;; make Maven hunt for a jar that was never released, and dependency:tree
    ;; fails to resolve. The trace records the packaging as :extension.
   (when-let [extension (:extension coord)]
     (xml/element :type {} extension))
   (when-let [exclusions (seq (:exclusions coord))]
     (xml/element :exclusions {} (map exclusion-element (sort exclusions))))))

(defn deps->pom [deps repository destination]
  (let [tags (xml/element
              :project {:xmlns "http://maven.apache.org/POM/4.0.0"
                        :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
                        :xsi:schemaLocation "http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd"}
              (xml/element :modelVersion {} "4.0.0")
              (xml/element :packaging {} "jar")
              (xml/element :groupId {} repository)
              (xml/element :artifactId {} repository)
              (xml/element :version {} "0.1.0")
              (xml/element :name {} repository)
              (xml/element :dependencies {} (map dependency-element deps))
              (xml/element
               :repositories {}
               (xml/element
                :repository {}
                (xml/element :id {} "central")
                (xml/element :url {} "https://repo1.maven.org/maven2"))
               (xml/element
                :repository {}
                (xml/element :id {} "clojars")
                (xml/element :url {} "https://clojars.org/repo"))
               (xml/element
                :repository {}
                (xml/element :id {} "jboss")
                (xml/element :url {} "https://repository.jboss.org/maven2"))))]
    (spit destination (xml/indent-str tags))))

(defn generate-pom
  "Writes `destination` from the trace at `trace-path`.

  Both paths are taken as arguments so the project's own deps.edn is never
  touched; an earlier version swapped it out under the project and left it
  renamed whenever generation failed partway."
  [{:keys [repository trace-path destination]
    :or {trace-path "trace.edn" destination "pom.xml"}}]
  (let [deps (effective-deps (read-string (slurp trace-path)))]
    (deps->pom deps repository destination)
    (println "Wrote" destination "with" (count deps) "dependencies")))
