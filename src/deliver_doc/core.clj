(ns deliver-doc.core
  (:use [environ.core]
        [subversion-clj.core]
        [subversion-clj.wc :only [current-revision]]
        [nio2.dir-seq]
        [nio2.files]
        [nio2.watch :only [watch-seq]]
        [clj-time.coerce :only [from-date]]
        [clj-time.core :only [epoch]])
  (:require [nio2.io :as io2]
            [clj-time.format :as f]
            [clojure.string :as string])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [org.tmatesoft.svn.core.wc SVNRevision]
           [org.tmatesoft.svn.core SVNDepth]))

(def workspace (env :svn-workspace))
(def viewspace (env :view-workspace))
(def fmt (f/formatter "yyyyMMdd"))
(def repo (repo-for (.. (io2/path (env :svn-repo)) toUri toString)))

(defn calc-commit-times [path]
  (loop [updated-at-same-day 0
         rev-seq (->> (revisions-for repo (into-array String [(.toString path)]) 1 -1)
                   reverse)
         std-date nil]
    (let [rev (first rev-seq)
          date (from-date (:time rev))]
      (if (= (f/unparse fmt date) (f/unparse fmt (or std-date date)))
        (if (empty? (rest rev-seq))
          (str (f/unparse fmt date) "_" (format "%02d" (inc updated-at-same-day)))
          (recur (inc updated-at-same-day) (rest rev-seq) date)) 
        (str (f/unparse fmt date) "_" (format "%02d" updated-at-same-day))))))

(defn link-name [path]
  (let [filename (.. path getFileName toString)]
    (if (re-find #"\d{8}_\d{2}\.[^\.]+$" filename)
      filename
      (let [[basename extension] (string/split filename #"\.(?=[^\.]+$)")]
        (str basename "_" (calc-commit-times path) "." extension)))))

(defn- delete-link [path]
  (Files/deleteIfExists path)
  (let [filename (.. path getFileName toString)
        [basename extension] (string/split filename #"\.(?=[^\.]+$)")
        matcher (path-matcher (str "regex:" basename "_\\d{8}_\\d{2}\\.[^\\.]+"))]
    (->> (dir-seq-filter (.. path getParent)
           #(matcher (.getFileName %)))
      (map #(Files/deleteIfExists %))
      doall)))

(defn revisions-path-seq [from-rev]
  (->> (revisions-for repo (into-array String []) from-rev -1)
    (map :changes)
    (map (fn [changes]
           (->> changes (filter #(and (= (first %) "file") (not= (last %) :delete))) (map second))))
    flatten
    distinct
    (map #(io2/path workspace %))))

(defn svn-path-seq [root]
  (->> (io2/path root)
    (tree-seq directory? (fn [d] (dir-seq-filter d #(not= ".svn" (.. % toFile getName)))))
    (filter #(not (directory? %)))))

(defn update-link [from-rev]
  (with-client-manager
    (let [cli (.getUpdateClient *client-manager*)]
      (.doUpdate cli (.. (io2/path workspace) toFile) SVNRevision/HEAD SVNDepth/INFINITY true false)))

  (println "update-link:" from-rev)
  (when from-rev
    (doseq [rev (revisions-for repo (into-array String []) from-rev -1)] 
      (doseq [change (:changes rev)]
        (when (= (last change) :delete)
          (delete-link (io2/path viewspace (second change)))))))
  

  (doseq [src-path (if from-rev
                     (revisions-path-seq from-rev)
                     (svn-path-seq workspace))]
    (try
      (let [relative-path (.. (io2/path workspace) (relativize src-path))
            dir  (.. relative-path getParent toFile getPath)
            file (link-name relative-path)
            link-path (io2/path viewspace dir file)]
        (create-directories! (parent link-path))
        (delete-link (io2/path viewspace dir (.. relative-path getFileName toString)))
        (Files/createLink link-path src-path))
      (catch Exception ex (do (println "Skip " src-path) (.printStackTrace ex)) ))))

(defn -main [& args]
  (let [db-file (io2/path (env :svn-repo) "db")
        revision (atom (with-client-manager (current-revision workspace)))]
    (when-not (.. (io2/path viewspace) toFile exists)
      (create-directories! (io2/path viewspace))
      (update-link nil))
    (doseq [ev (watch-seq db-file :create)]
      (when (= (.toString (:path ev)) "current")
        (let [new-revision (-> (slurp (.toFile (io2/path-resolve db-file (:path ev))))
                             read-string)]
          (update-link (inc @revision))
          (reset! revision new-revision))))))

