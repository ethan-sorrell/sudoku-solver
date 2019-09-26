(ns sudoku-solver.pages
  (:require [clojure.string :as string]
            [hiccup.core :as markup]
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

(defmacro stdout-and-output
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [r# ~@body]
         {:result r#
          :str    (str s#)}))))

(defn result-page
  [request]
  (let [{:keys [params uri]} request]
    (if (backend/valid-sudoku params)
      (let [result-map (stdout-and-output (time (backend/solve params)))
            {soln :result time-elapsed :str} result-map]
        (markup/html
         [:style
          "table, th, td {border-collapse: collapse;}"
          "th, td {padding: 3px;}"
          "td {text-align:center; width:48px; height:48px;}"]
         [:h1 "Solution:"]
         (if soln
           (backend/display-matrix soln)
           [:p "No valid solution."]
           )
         [:h1 (string/replace time-elapsed #"\"" "")]))
      ;; else
      invalid-puzzle-page)))
