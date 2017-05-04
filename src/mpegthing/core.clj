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

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (> destination max-pos)
    ;; don't modify the board for connections which would lead
    ;; out of the board
    board
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]]))) 

;; The board map should look like this:
;; {1 {:pegged true, :connections {6 3, 4 2}}, ;; 1 is connected to 6 through 3 and with 4 through 2
;; ...,
;; :rows 5}

(defn connect-right
  [board max-pos pos]
  (let [neighbor      (inc pos)
        destination   (inc neighbor)]
    ;; make connection only when there is a place to the right
    ;; if neighbor or pos are triangular, it means they are at the edge
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row          (row-num pos)
        neighbor     (+ row pos)
        destination  (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row          (row-num pos)
        neighbor     (+ 1 row pos)
        destination  (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and perform connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

;; page 117 (144) Moving Pegs

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board."
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position."
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))


;; (def *board* (atom (new-board 5)))
;; (swap! *board* #(remove-peg % 4))
       
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

