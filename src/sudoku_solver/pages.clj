(ns sudoku-solver.pages
  (:require [hiccup.core :as markup]
            [hiccup.form :as form]
            [sudoku-solver.backend :as backend]))

(defn form-test
  [request]
  (markup/html
   [:style
    "table, th, td {border-collapse: collapse;}"
    "th, td {padding: 3px;}"
    "td {text-align:center; width:48px; height:48px;}"]
   [:h1 "Input Your Sudoku Problem:"]
   ;; Second form: Table
   (form/form-to
    [:post "post-result"]
    (backend/make-table)
    (form/submit-button "Solve!"))))

(defn display-table
  [request]
  (let [{:keys [params uri]} request]
    (markup/html
     [:style
      "table, th, td {border-collapse: collapse;}"
      "th, td {padding: 3px;}"
      "td {text-align:center; width:48px; height:48px;}"]
     [:h1 "Solution:"]
     (backend/display-matrix (backend/solver params)))))
