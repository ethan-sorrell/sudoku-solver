(ns sudoku-solver.pages
  (:require [hiccup.core :as markup]
            [hiccup.form :as form]
            [sudoku-solver.backend :as backend]))

(defn form-page
  [request]
  (markup/html
   [:style
    "table, th, td {border-collapse: collapse;}"
    "th, td {padding: 3px;}"
    "td {text-align:center; width:48px; height:48px;}"]
   [:h1 "Input Your Sudoku Problem:"]
   ;; Input Table
   (form/form-to
    [:post "post-result"]
    (backend/make-table)
    (form/submit-button "Solve!"))))

(def invalid-puzzle-page
  (markup/html
   [:h1 "Sorry, the puzzle you entered was invalid."]
   [:a {:href "/"} "Return to home page"]))

(defn result-page
  [request]
  (let [{:keys [params uri]} request]
    (if (backend/valid-sudoku params)
      (markup/html
       [:style
        "table, th, td {border-collapse: collapse;}"
        "th, td {padding: 3px;}"
        "td {text-align:center; width:48px; height:48px;}"]
       [:h1 "Solution:"]
       (backend/display-matrix (backend/solve params)))
      ;; else
      invalid-puzzle-page)))
