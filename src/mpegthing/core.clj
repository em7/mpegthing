(ns mpegthing.core
  "My implementation of Peg game, according to chapter 5 of book Clojure for the
  brave and true."
  (:require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers."
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri
  "Lazy sequence of triangular numbers."
  (tri*))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc."
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n."
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos1 in row1,
  positions 2 and 3 in row 2 etc."
  [pos]
  (inc (count (take-while #(> pos %) tri))))

;; page 114, defn connect

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

