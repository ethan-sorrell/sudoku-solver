(ns sudoku-solver.backend-test
  (:require [sudoku-solver.backend :refer :all]
            [clojure.test :as t]))

(t/deftest backend-tests
  (t/testing "Helper functions"
    (t/is (= "a1" (get-coord 1 1)))
    (t/is (= (row-peers "a1") (row-peers "a2")))
    (t/is (= (col-peers "a3") (col-peers "c3")))
    (t/is (= (vicinity-peers "a1") (vicinity-peers "c3"))))
  (t/testing "Constraint Propagation Functions"))
