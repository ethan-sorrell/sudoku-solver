(ns sudoku-solver.pages
  (:require [hiccup.core :as markup]
            [hiccup.form :as form]
            [sudoku-solver.backend :as backend]))

(defn welcome
  [request]
  {:status 200
   :body (markup/html [:h1 "Homepage"] [:p "This is a homepage"])
   :headers {}})

(defn form-test
  [request]
  (markup/html
   [:style
    "table, th, td {border-collapse: collapse;}"
    "th, td {padding: 3px;}"
    "td {text-align:center; width:48px; height:48px;}"]
   [:h1 "Input your Sudoku Problem."]
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
     (backend/display-matrix (backend/solver params)))))
