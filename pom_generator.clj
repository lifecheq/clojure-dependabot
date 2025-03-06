(ns pom-generator
  (:require [clojure.data.xml :as xml]
            [clojure.tools.deps.tree :as tree]))

(defn children [deps-node]
  (rest (tree-seq :children
                  (fn [{:keys [children]}]
                    (map (fn [[k v]] (assoc v :name k)) children))
                  deps-node)))

(defn- exclude-children [deps-node]
  #_(when (= 'io.xapix/paos (:name deps-node))
      (tap> {:deps-node deps-node
             :exclusions (map :name (children deps-node))}))
  (-> deps-node
      (update-in [:coord :exclusions] into (keys (:children deps-node)))
      (dissoc :children)))

(defn effective-deps [trace]
  (let [tree (tree/trace->tree trace)]
    (->> (children tree)
         (map exclude-children)
         (remove #(-> % :coord :git/url))
         (filter :include))))

(defn exclusion-element [exclusion]
  (xml/element :exclusion {}
               (xml/element :groupId {} (namespace exclusion))
               (xml/element :artifactId {} (str (name exclusion)))))

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
               (xml/element
                 :dependencies {}
                 (for [{dep-name :lib :as dep} deps]
                   (xml/element
                    :dependency {}
                    (xml/element :groupId {} (namespace dep-name))
                    (xml/element :artifactId {} (str (name dep-name)))
                    (xml/element :version {} (str (-> dep :coord :mvn/version)))
                    (when-let [exclusions (-> dep :coord :exclusions)]
                      (xml/element :exclusions {} (map exclusion-element exclusions))))))

               (xml/element :build {}
                            (xml/element :sourceDirectory {} "src-shared"))

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
                   (xml/element :url {} "https://repository.jboss.org/maven2"))

                 ))
        xml-str (xml/indent-str tags)]

    (println xml-str)
    (spit destination xml-str)))

(defn generate-pom [{:keys [repository trace-path]
                     :or {trace-path "trace.edn"}}]
  (-> (effective-deps (read-string (slurp trace-path)))
      (deps->pom repository "pom.xml")))

#_{:clj-kondo/ignore [:unresolved-namespace]}
(comment
  (clojure.repl.deps/add-lib 'djblue/portal {:mvn/version "RELEASE"})
  (map :name (children (tree/trace->tree (read-string (slurp "../utwig/trace.edn")))))
  (effective-deps (read-string (slurp "../utwig/trace.edn")))
  (generate-pom {:repository "utwig" :trace-path "../utwig/trace.edn" })
  ;; Testing this is relatively easy:
  ;; copy this file into utwig/pom-generator/pom_generator.clj (or whichever project you want)
  ;; (so - pom-generator is in the same level as src, src-shared etc.)
  ;; in terminal (in the root directory of the project) run:
  ;; Generate a trace file in utwig using
  ;;     clojure -X:deps prep
  ;;     clojure -A:app -Strace
  ;; Then run the generate-pom form
  :.)
