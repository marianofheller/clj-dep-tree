(ns clj-dep-tree.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [clojure.string :as s]
            [cheshire.core :as json]
            [flatland.ordered.set :as ordered]
            [dotenv :refer [env]]))


(def relevant-prefixes (s/split (env "RELEVANT_PREFIXES") #","))

(defn- relevant-file-filter
  []
  (reify java.io.FileFilter
    (accept [_ file] (and
                      (not (.contains (.getPath file) "node_modules"))
                      (not (.contains (.getPath file) "dist"))
                      (or (. file (isDirectory)) (= (.getName file) "package.json"))))))

(defn dir-seq
  [dir]
  (tree-seq
   (fn [^java.io.File f] (. f (isDirectory)))
   (fn [^java.io.File d] (seq (. d (listFiles (relevant-file-filter)))))
   dir))



(defn parse-packages [files]
  (let [packages (atom {})]
    (println "Parsing directory")
    (doall (map
            #(->> %
                  (io/reader)
                  (json/parse-stream)
                  ((fn [m]
                     (let [all-keys (concat
                                     (keys (get m "dependencies"))
                                     (keys (get m "devDependencies")))

                           relevant-keys (filter
                                          (fn [k] (some
                                                   (fn [prefix] (s/starts-with? k prefix))
                                                   relevant-prefixes))
                                          all-keys)]
                       (when-let [package-name (get m "name")]
                         (swap! packages assoc package-name (set relevant-keys)))))))
            files))
    (println "Parsed directory. Found:" (count @packages) "relevant dependencies.")
    @packages))

(defn sub-trace [name trace]
  (let [vtrace (into [] trace)
        idx (.indexOf vtrace name)]
    (if (= idx -1)
      nil
      (subvec vtrace idx))))

(defn distinct-by
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                 ((fn [[x :as xs] seen]
                    (when-let [s (seq xs)]
                      (let [v (f x)]
                        (if (contains? seen v)
                          (recur (rest s) seen)
                          (cons x (step (rest s) (conj seen v)))))))
                  xs seen)))]
     (step coll #{}))))

(defn eval-acyclic [parsed-packages root]
  (let [cycle-trace (atom nil)
        deps-trace (atom nil)
        previous (atom (ordered/ordered-set))
        dfs (fn dfs [node]
              (swap! previous conj node)
              (let [folder (fn
                             ([] true)
                             ([acc] acc)
                             ([acc, child]
                              (if (or
                                   (contains? @previous child)
                                   (not (dfs child)))
                                (reduced false)
                                acc)))
                    result (reduce folder true (get parsed-packages node))]
                (when (and (not result) (nil? @deps-trace) (nil? @cycle-trace))
                  (swap! deps-trace (fn [_] @previous))
                  (swap! cycle-trace (fn [_] (sub-trace node @previous))))
                (swap! previous disj node)
                result))]
    {:name root :is-acyclic (dfs root) :deps-trace @deps-trace :cycle-trace @cycle-trace}))



(defn glhf [dir]
  (let [parsed-packages (doall (->> dir
                                    (clojure.java.io/file)
                                    (dir-seq)
                                    (filter #(not (.isDirectory %)))
                                    (parse-packages)))

        results (doall (->> parsed-packages
                            (pmap (fn [[name _deps]] (eval-acyclic parsed-packages name)))
                            (filter #(not (:is-acyclic %1)))
                            (distinct-by #(set (:cycle-trace %1)))))]

    (println "Parsed dependencies Found:" (count results) "cyclic dependencies")
    (run! (fn [result]
            (println "Package" (:name result) "deps are not acyclic:" (:cycle-trace result))
            result)
          results)))


;; ===============================================
;; FIXME: reporting show cycle better
;; FIXME: dedup sets in different order
;; FIXME: optimization lookup? Measure too. Hay q ver si se puede hacer ya q ejecuta con un pmap?

(defn dir-exists?
  [path]
  (let [f (io/as-file path)]
    (and  (.exists f) (.isDirectory f))))

(def cli-options
  ;; An option with a required argument
  [["-d" "--directory DIRECTORY" "Path to directory to analyze"
    :id :directory
    :missing "-d Target path is mandatory"
    :validate [dir-exists? "Target directory must exist"]]
   ["-h" "--help"]])

(defn -main [& args]
  (let [opts (cli/parse-opts args cli-options)]
    (if-let [errors (seq (:errors opts))]
      (do (println errors) (System/exit 1))
      (glhf (get-in opts [:options :directory])))
    (System/exit 0)))