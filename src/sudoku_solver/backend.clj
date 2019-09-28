(ns sudoku-solver.backend
  (:require [clojure.string :as string]
            [hiccup.core :as markup]
            [hiccup.form :as form]))

;;;;;;;;; Backend Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def size 9)

(defn get-coord [row col]
  "take 1-indexed row, col and give coordinate string"
  (str (char (+ 96 row)) col))

(defn get-xy [coord]
  "take coordinate string, give 1-indexed [row col]"
  (let [row-char (get coord 0)
        col-char (get coord 1)
        row (- (int row-char) 96)
        col (Character/digit col-char 10)]
    [row col]))

(defn row-peers [coord]
  (let [[row-n col-n] (get-xy coord)]
    (for [col (range 1 10)]
      (get-coord row-n col))))

(defn vicinity-peers [coord]
  (let [[row-n col-n] (get-xy coord)
        col-3 (quot (dec col-n) 3)
        row-3 (quot (dec row-n) 3)
        start-col (inc (* 3 col-3))
        start-row (inc (* 3 row-3))]
    (for [row (range start-row (+ start-row 3))
          col (range start-col (+ start-col 3))]
      (get-coord row col))))

(defn col-peers [coord]
  (let [[row-n col-n] (get-xy coord)]
    (for [row (range 1 10)]
      (get-coord row col-n))))

(defn row-values [matrix coord]
  (map #(get matrix %) (row-peers coord)))

(defn vicinity-values [matrix coord]
  (map #(get matrix %) (vicinity-peers coord)))

(defn col-values [matrix coord]
  (map #(get matrix %) (col-peers coord)))


;;;;;;;;;;;;; constraint propagation functions ;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare assign eliminate)

(defn peers [pos]
  (remove #(= % pos)
          (concat (vicinity-peers pos)
                  (col-peers pos)
                  (row-peers pos))))

(defn uninitialized-matrix []
  (into
   {}
   (for [y (range 1 10)
         x (range 1 10)
         :let [coord (get-coord y x)]]
     [coord "123456789"])))
(defn candidate-locations [matrix pos value]
  "takes value and returns list of list of coords in unit which could contains value"
  (for [unit (conj []
                   (vicinity-peers pos)
                   (row-peers pos)
                   (col-peers pos))]
    (for [loc unit
          :when (and
                 (not (= loc pos))
                 (string/includes? (get matrix loc) value))]
      loc)))

(defn propagate-in [matrix pos value]
  "Check if any unit containing pos is reduces to one possible location for a value"
  (if-not matrix
    false
    (loop [result matrix
           rem-units (candidate-locations matrix pos value)]
      (if-not (seq rem-units)
        result
        (if (= 0 (count (first rem-units)))
          false
          (if (= 1 (count (first rem-units)))
            (if-let [new-matrix
                     (assign result (first (first rem-units)) value)]
              (recur new-matrix (rest rem-units))
              false)
            (recur result (rest rem-units))))))))

(defn propagate-out [matrix pos]
  "Propagate a new constraint out from pos to its units"
  (let [value (get matrix pos)]
    (loop [result matrix
           rem-peers (peers pos)]
      (if-let [peer (first rem-peers)]
        (if (= peer pos)
          (recur result (rest rem-peers))
          (if-let [new-matrix (eliminate result peer value)]
            (recur new-matrix (rest rem-peers))
            false))
        result))))

(defn elim [matrix from value]
  "Remove value from association with from in matrix"
  (assoc matrix from (string/replace (get matrix from) (re-pattern value) "")))


(defn eliminate [matrix from value]
  "Elim and propagate"
  (if-not (string/includes? (get matrix from) value)
    matrix
    (let [new-matrix (elim matrix from value)
          remaining-count (count (get new-matrix from))]
      (cond
        (= 0 remaining-count) false ;; no remaining possible values
        ;; we have our value and need to propagate constraint
        (= 1 remaining-count) (propagate-in (propagate-out new-matrix from) from value)
        :else new-matrix))))

(defn assign [matrix pos value]
  "Eliminate all other values associated with pos then propagate"
  (if-not matrix
    false
    (let [current_val (get matrix pos)
          other_vals (string/replace current_val (re-pattern value) "")]
      (loop [result matrix
             rem other_vals]
        (if-not result
          false
          (if (= 0 (count rem))
            result
            (recur (eliminate result pos (str (first rem))) (rest rem))))))))

(defn parse-grid [matrix]
  "parse from partially-filled in solution to description of constraints"
  (loop [result (uninitialized-matrix)
         rem matrix]
    (if-not result
      false
      (if-not (seq rem)
        result
        (let [pair (first rem)
              coord (first pair)
              vals (second pair)]
          (if-not (seq vals)
            (recur result (rest rem))
            (recur (assign result coord vals) (rest rem))))))))

(defn search [matrix]
  "Returns solution or nil if none found"
  (when matrix
    (if-let [unsolved (seq (filter #(not (= 1 (count (second %)))) matrix))]
      (let [max-constr (apply min-key #(count (get matrix %))
                              (keys unsolved))]
        (some identity
              (for [candidate (get matrix max-constr)]
                (search (assign matrix max-constr (str candidate))))))
      matrix)))

(defn solve [matrix]
  "Returns solution or nil if contradiction found"
  (search (parse-grid matrix)))

;;;;;;;;;;;;; validation functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-valid [coll]
  "Take a collection of strings representing a row/col/vicinity"
  (let [elts (filter seq coll)]
    (= elts (distinct elts))))

(defn valid-sudoku-cell?
  [matrix coord]
  (every? identity
          (map #(check-valid (% matrix coord))
               [row-values col-values vicinity-values])))

(defn valid-sudoku
  [matrix]
  (every?
   identity
   (for [y (range 1 10)
         x (range 1 10)
         :let [coord (get-coord y x)
               cell (matrix coord)]
         :when (seq cell)]
     (valid-sudoku-cell? matrix coord))))

;;;;;;;;;;;;;;;;;;; HTML generation function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-table []
  "make hiccup markup for sudoku input table"
  (into [:table {:border "2px solid;"}]
        (for [y (range 1 (inc size))]
          (if (= (rem y 3) 0)
            (into [:tr]
                  (for [x (range 1 (inc size))]
                    (if (= (rem x 3) 0)
                      [:td
                       {:style "border-right:2px solid; border-bottom:2px solid"}
                       (form/text-field {:size 1} (get-coord y x))]
                      [:td
                       {:style "border-bottom:2px solid"}
                       (form/text-field {:size 1} (get-coord y x))])))
            (into [:tr]
                  (for [x (range 1 (inc size))]
                    (if (= (rem x 3) 0)
                      [:td
                       {:style "border-right:2px solid"}
                       (form/text-field {:size 1} (get-coord y x))]
                      [:td (form/text-field {:size 1} (get-coord y x))])))))))

(defn display-matrix [matrix]
  "make hiccup markup for sudoku solution"
  (into [:table {:border "2px solid;"}]
        (for [y (range 1 (inc size))]
          (if (= 0 (rem y 3))
            (into [:tr]
                  (for [x (range 1 (inc size))]
                    (if (= 0 (rem x 3))
                      [:td
                       {:style "border-right:2px solid; border-bottom:2px solid"}
                       (matrix (get-coord y x))]
                      [:td {:style "border-bottom:2px solid"} (matrix (get-coord y x))])))
            (into [:tr]
                  (for [x (range 1 (inc size))]
                    (if (= 0 (rem x 3))
                      [:td {:style "border-right:2px solid;"}
                       (matrix (get-coord y x))]
                      [:td (matrix (get-coord y x))])))))))

;;;;;;;;;;;;;; Debug function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-matrix [matrix]
  (print "\n")
  (for [y (range 1 10)]
    (do (print "|")
        (apply print
               (for [x (range 1 10)]
                 (let [num (matrix (get-coord y x))
                       repr (if (= num "") "_" num)]
                   (cond
                     (= (rem x 3) 0) (str repr \|)
                     :else repr)
                   )))
        (print "\n")
        (when (= (rem y 3) 0) (print "\n")))))


(def empty-matrix
  {"c9" "", "e6" "", "b3" "", "d4" "", "a3" "", "c8" "", "f2" "", "h3" "", "i9" "", "f7" "", "b4" "", "e9" "", "a9" "", "e2" "", "f6" "", "g3" "", "f3" "", "d6" "", "b7" "", "d9" "", "h8" "", "d5" "", "f4" "", "d1" "", "i3" "", "g2" "", "h2" "", "e4" "", "a7" "", "d3" "", "g4" "", "e1" "", "i5" "", "i6" "", "a6" "", "b5" "", "d7" "", "a8" "", "d2" "", "b9" "", "h4" "", "g7" "", "e3" "", "f8" "", "i1" "", "i7" "", "g8" "", "c2" "", "d8" "", "b6" "", "g5" "", "f9" "", "a4" "", "f1" "", "e7" "", "h5" "", "g6" "", "i4" "", "c3" "", "a1" "", "b2" "", "g9" "", "e5" "", "a5" "", "e8" "", "i8" "", "c4" "", "h9" "", "h1" "", "i2" "", "g1" "", "h7" "", "c5" "", "f5" "", "c6" "", "a2" "", "c7" "", "c1" "", "b1" "", "h6" "", "b8" ""})

(def full-matrix
  {"c9" "1", "e6" "1", "b3" "1", "d4" "1", "a3" "5", "c8" "1", "f2" "1", "h3" "4", "i9" "1", "f7" "1", "b4" "1", "e9" "6", "a9" "1", "e2" "1", "f6" "1", "g3" "1", "f3" "3", "d6" "5", "b7" "1", "d9" "1", "h8" "3", "d5" "1", "f4" "2", "d1" "4", "i3" "1", "g2" "6", "h2" "1", "e4" "1", "a7" "1", "d3" "1", "g4" "5", "e1" "1", "i5" "1", "i6" "9", "a6" "1", "b5" "1", "d7" "3", "a8" "1", "d2" "1", "b9" "1", "h4" "1", "g7" "1", "e3" "1", "f8" "8", "i1" "1", "i7" "7", "g8" "1", "c2" "7", "d8" "1", "b6" "1", "g5" "1", "f9" "1", "a4" "3", "f1" "1", "e7" "1", "h5" "1", "g6" "1", "i4" "1", "c3" "1", "a1" "1", "b2" "1", "g9" "9", "e5" "7", "a5" "1", "e8" "1", "i8" "1", "c4" "1", "h9" "1", "h1" "1", "i2" "1", "g1" "1", "h7" "1", "c5" "1", "f5" "1", "c6" "1", "a2" "1", "c7" "5", "c1" "1", "b1" "8", "h6" "1", "b8" "2"})

(def test-matrix-1
  {"c9" "6", "e6" "", "b3" "", "d4" "", "a3" "2", "c8" "", "f2" "", "h3" "9", "i9" "", "f7" "", "b4" "2", "e9" "", "a9" "4", "e2" "", "f6" "3", "g3" "", "f3" "4", "d6" "", "b7" "", "d9" "", "h8" "", "d5" "2", "f4" "9", "d1" "5", "i3" "", "g2" "", "h2" "5", "e4" "", "a7" "8", "d3" "", "g4" "7", "e1" "", "i5" "", "i6" "9", "a6" "", "b5" "", "d7" "", "a8" "", "d2" "8", "b9" "", "h4" "", "g7" "2", "e3" "6", "f8" "6", "i1" "1", "i7" "", "g8" "", "c2" "1", "d8" "3", "b6" "", "g5" "3", "f9" "", "a4" "", "f1" "", "e7" "7", "h5" "", "g6" "", "i4" "8", "c3" "7", "a1" "", "b2" "", "g9" "", "e5" "4", "a5" "", "e8" "", "i8" "", "c4" "", "h9" "1", "h1" "", "i2" "", "g1" "", "h7" "", "c5" "", "f5" "1", "c6" "5", "a2" "3", "c7" "9", "c1" "", "b1" "8", "h6" "", "b8" "7"})

(def test-matrix-2
  {"c9" "6", "e6" "", "b3" "", "d4" "", "a3" "2", "c8" "", "f2" "", "h3" "9", "i9" "", "f7" "", "b4" "2", "e9" "", "a9" "4", "e2" "", "f6" "3", "g3" "", "f3" "4", "d6" "", "b7" "", "d9" "", "h8" "", "d5" "2", "f4" "9", "d1" "5", "i3" "", "g2" "", "h2" "5", "e4" "", "a7" "8", "d3" "", "g4" "7", "e1" "", "i5" "", "i6" "9", "a6" "", "b5" "", "d7" "", "a8" "", "d2" "8", "b9" "", "h4" "", "g7" "2", "e3" "6", "f8" "6", "i1" "1", "i7" "", "g8" "", "c2" "1", "d8" "3", "b6" "", "g5" "3", "f9" "", "a4" "", "f1" "", "e7" "7", "h5" "", "g6" "", "i4" "8", "c3" "7", "a1" "", "b2" "", "g9" "", "e5" "4", "a5" "", "e8" "", "i8" "", "c4" "", "h9" "1", "h1" "", "i2" "", "g1" "", "h7" "", "c5" "", "f5" "1", "c6" "5", "a2" "3", "c7" "9", "c1" "", "b1" "8", "h6" "", "b8" "7"})

(def test-matrix-3
  {"c9" "", "e6" "", "b3" "", "d4" "", "a3" "5", "c8" "", "f2" "", "h3" "4", "i9" "", "f7" "", "b4" "", "e9" "6", "a9" "", "e2" "1", "f6" "", "g3" "", "f3" "3", "d6" "5", "b7" "", "d9" "", "h8" "3", "d5" "", "f4" "2", "d1" "4", "i3" "", "g2" "6", "h2" "", "e4" "", "a7" "", "d3" "", "g4" "5", "e1" "", "i5" "", "i6" "9", "a6" "", "b5" "", "d7" "3", "a8" "", "d2" "", "b9" "", "h4" "", "g7" "", "e3" "", "f8" "8", "i1" "", "i7" "7", "g8" "", "c2" "7", "d8" "", "b6" "", "g5" "", "f9" "", "a4" "3", "f1" "", "e7" "", "h5" "", "g6" "", "i4" "", "c3" "", "a1" "", "b2" "", "g9" "9", "e5" "7", "a5" "", "e8" "", "i8" "", "c4" "", "h9" "", "h1" "", "i2" "", "g1" "", "h7" "", "c5" "1", "f5" "", "c6" "", "a2" "", "c7" "5", "c1" "", "b1" "8", "h6" "", "b8" "2"})


(def parsed-test-matrix-1
  {"c9" "123456789",
   "e6" "123456789",
   "b3" "123456789",
   "d4" "123456789",
   "a3" "3",
   "c8" "123456789",
   "f2" "123456789",
   "h3" "123456789",
   "i9" "123456789",
   "f7" "123456789",
   "b4" "123456789",
   "e9" "123456789",
   "a9" "9",
   "e2" "123456789",
   "f6" "123456789",
   "g3" "123456789",
   "f3" "123456789",
   "d6" "123456789",
   "b7" "123456789",
   "d9" "123456789",
   "h8" "123456789",
   "d5" "123456789",
   "f4" "123456789",
   "d1" "123456789",
   "i3" "123456789",
   "g2" "123456789",
   "h2" "123456789",
   "e4" "123456789",
   "a7" "7",
   "d3" "123456789",
   "g4" "123456789",
   "e1" "123456789",
   "i5" "123456789",
   "i6" "123456789",
   "a6" "6",
   "b5" "123456789",
   "d7" "123456789",
   "a8" "8",
   "d2" "123456789",
   "b9" "123456789",
   "h4" "123456789",
   "g7" "123456789",
   "e3" "123456789",
   "f8" "123456789",
   "i1" "123456789",
   "i7" "123456789",
   "g8" "123456789",
   "c2" "123456789",
   "d8" "123456789",
   "b6" "123456789",
   "g5" "123456789",
   "f9" "123456789",
   "a4" "4",
   "f1" "123456789",
   "e7" "123456789",
   "h5" "123456789",
   "g6" "123456789",
   "i4" "123456789",
   "c3" "123456789",
   "a1" "1",
   "b2" "123456789",
   "g9" "123456789",
   "e5" "123456789",
   "a5" "5",
   "e8" "123456789",
   "i8" "123456789",
   "c4" "123456789",
   "h9" "123456789",
   "h1" "123456789",
   "i2" "123456789",
   "g1" "123456789",
   "h7" "123456789",
   "c5" "123456789",
   "f5" "123456789",
   "c6" "123456789",
   "a2" "123456789",
   "c7" "123456789",
   "c1" "123456789",
   "b1" "123456789",
   "h6" "123456789",
   "b8" "123456789"})

;; (defn solve-sudoku
;;   [matrix]
;;   (loop [rem matrix
;;          check-coord (first-empty rem)
;;          row (row-values check-coord)
;;          col (col-values check-coord)
;;          vicinity (vicinity-values check-coord rem)]))
