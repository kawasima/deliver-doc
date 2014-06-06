(ns deliver-doc.core
  (:use [subversion-clj.core]
        [nio2.dir-seq]
        [nio2.files]
        [clj-time.coerce :only [from-date]]
        [clj-time.core :only [epoch]])
  (:require [nio2.io :as io2]
            [clj-time.format :as f]
            [clojure.string :as string])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def fmt (f/formatter "yyyyMMdd"))
(def repo (repo-for (.. (io2/path "dev-resources/repos") toUri toString)))

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
    (if (re-matches #"\d{8}_\d{2}\.[^\.]+$" filename)
      filename
      (let [[basename extension] (string/split filename #"\.(?=[^\.]+$)")]
        (str basename "_" (calc-commit-times path) "." extension)))))

(defn svn-path-seq [root]
  (->> (io2/path root)
    (tree-seq directory? (fn [d] (dir-seq-filter d #(not= ".svn" (.. % toFile getName)))))
    (filter #(not (directory? %)))))

(defn -main [& args]
  (doseq [src-path (svn-path-seq "target/repos")]
    (let [relative-path (.. (io2/path "target/repos") (relativize src-path))
          dir  (.. relative-path getParent toFile getPath)
          file (link-name relative-path)
          link-path (io2/path "target/repos-ro" dir file)]
      (create-directories! (parent link-path))
      (Files/createLink link-path src-path))))

