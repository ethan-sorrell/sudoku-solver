(ns sudoku-solver.core
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.reload :refer [wrap-reload]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.handler.dump :refer [handle-dump]]
            [compojure.core :refer [defroutes GET POST]]
            [hiccup.core :as markup]
            [hiccup.form :as form]
            [compojure.route :refer [not-found]]
            [sudoku-solver.backend :as backend]
            [sudoku-solver.pages :as pages]))

;; Routing
(defroutes routes
  (GET "/" req (pages/form-page req))
  (POST "/post-result" req (pages/result-page req))
  (not-found "<h1>Error</h1>
<p>Page not found</p>"))

;; Handler
(def app (wrap-params routes))

;; Main
(defn -dev-main
  "A simple webserver with live reloading"
  [port-number]
  (jetty/run-jetty
   (wrap-reload #'app)
   {:port (Integer. port-number)}))

(defn -main
  "A simple webserver using Ring & Jetty"
  [port-number]
  (jetty/run-jetty
   app
   {:port (Integer. port-number)}))
