(ns sudoku-solver.backend
  (:require [hiccup.core :as markup]
            [hiccup.form :as form]))

;; Backend Functions

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

(defn check-valid [coll]
  "Take a collection of strings representing a row/col/vicinity"
  (let [elts (filter seq coll)]
    (= elts (distinct elts))))

(defn get-row [matrix coord]
  (let [[row-n col-n] (get-xy coord)]
    (for [col (range 1 10)]
      (get matrix (get-coord row-n col)))))

(defn get-vicinity [matrix coord]
  (let [[row-n col-n] (get-xy coord)
        col-3 (quot (dec col-n) 3)
        row-3 (quot (dec row-n) 3)
        start-col (inc (* 3 col-3))
        start-row (inc (* 3 row-3))]
    (for [row (range start-row (+ start-row 3))
          col (range start-col (+ start-col 3))]
      (get matrix (get-coord row col)))))

(defn get-col [matrix coord]
  (let [[row-n col-n] (get-xy coord)]
    (for [row (range 1 10)]
      (get matrix (get-coord row col-n)))))

(defn get-empty [matrix]
  (second
   (first
    (drop-while #(not (nil? (seq (first %))))
                (for [y (range 1 10)
                      x (range 1 10)]
                  [(get matrix (get-coord y x)) (get-coord y x)])))))

(defn valid-sudoku-cell?
  [matrix coord]
  (every? identity
          (map #(check-valid (% matrix coord))
               [get-row get-col get-vicinity])))

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

(defn sudoku-try-val
  [matrix coord val]
  (let [new-matrix (assoc matrix coord val)]
    (valid-sudoku-cell? new-matrix coord)))

(defn get-candidates [matrix coord]
  (let [used-vals (set (concat (get-row matrix coord)
                               (get-col matrix coord)
                               (get-vicinity matrix coord)))]
    (for [x (range 1 10)
      :let [str-x (str x)]
      :when (not (contains? used-vals str-x))]
      str-x)))

(defn solver [matrix]
  ;; search first empty cell
  ;; return matrix if none empty
  ;;
  ;; found first empty => try values until first valid
  ;; If there's a valid value:
  ;;   Recur with value filled in
  ;; Else:
  ;;   Return empty map
  ;; (print (print-matrix matrix))
  (if-let [empty-coord (get-empty matrix)]
    (loop
        [acc matrix
         candidates (get-candidates matrix empty-coord)
         candidate (first candidates)]
      (if (nil? candidate) nil
          (if-let [soln (solver (assoc matrix empty-coord candidate))]
            soln
            (recur matrix (drop 1 candidates) (first candidates)))))
    matrix))

;; HTML generation function

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

;; Debug function

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

;; (defn solve-sudoku
;;   [matrix]
;;   (loop [rem matrix
;;          check-coord (first-empty rem)
;;          row (get-row check-coord)
;;          col (get-col check-coord)
;;          vicinity (get-vicinity check-coord rem)]))
