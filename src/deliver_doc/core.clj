(ns deliver-doc.core
  (:use [environ.core]
        [subversion-clj.core]
        [nio2.dir-seq]
        [nio2.files]
        [clj-time.coerce :only [from-date]]
        [clj-time.core :only [epoch]])
  (:require [nio2.io :as io2]
            [clj-time.format :as f]
            [clojure.string :as string])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def workspace (env :svn-workspace))
(def viewspace (env :view-workspace))
(def fmt (f/formatter "yyyyMMdd"))
(def repo (repo-for (.. (io2/path (env :svn-repo) toUri toString)))

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

(defn svn-path-seq [root]
  (->> (io2/path root)
    (tree-seq directory? (fn [d] (dir-seq-filter d #(not= ".svn" (.. % toFile getName)))))
    (filter #(not (directory? %)))))

(defn -main [& args]
  (doseq [src-path (svn-path-seq workspace)]
    (try
      (let [relative-path (.. (io2/path workspace) (relativize src-path))
            dir  (.. relative-path getParent toFile getPath)
            file (link-name relative-path)
            link-path (io2/path viewspace dir file)]
        (create-directories! (parent link-path))
        (Files/createLink link-path src-path))
      (catch Exception ex (println "Skip " src-path)))))

