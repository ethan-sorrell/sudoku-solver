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
   [:h1 "Test"]
   ;; First form: Simple Text Input
   [:form {:method "POST" :action "post-submit"} "First param:<br>"
    [:input {:type "text" :name "firstparam" :value "test-value"}]
    (form/submit-button "submit.")]
   ;; Different representation of first form
   (form/form-to [:post "post-submit"] "First param:<br>"
                 (form/text-field "firstparam")
                 (form/submit-button "submit."))
   ;; Second form: Table
   (form/form-to
    [:post "post-result"]
    (backend/make-table)
    (form/submit-button "submit table"))))

(defn display-result
  [request]
  (let [{:keys [params uri]} request
        table (for [y (range 1 10)]
                (for [x (range 1 10)]
                  (get params (backend/get-coord y x))))]
    (markup/html
     [:h1 table])))

(defn display-test
  [request]
  (let [{:keys [params uri]} request
        param-name (get params "firstparam")]
    (markup/html
     [:h1 "Received " (str param-name) "."])))
