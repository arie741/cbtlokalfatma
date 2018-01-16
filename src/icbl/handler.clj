(ns icbl.handler
  (:gen-class)
  (:require [compojure.core :refer [defroutes]]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [noir.util.middleware :as noir-middleware]
            [ring.adapter.jetty :refer :all]
            [icbl.routes.home :refer [home-routes]]
            [icbl.routes.teacher :refer [teacher-routes]]
            [icbl.routes.admin :refer [admin-routes]]
            [icbl.routes.ortu :refer [ortu-routes]]
            [icbl.routes.maintenance :refer [maintenance-routes]]
            [icbl.routes.tesmagang :refer [tesmagang-routes]]
            [icbl.routes.adminmagang :refer [adminmagang-routes]]
            ))

(defn init []
  (println "icbl is starting"))

(defn destroy []
  (println "icbl is shutting down"))

(defroutes app-routes
  (route/resources "/")
  (route/not-found "Not Found"))

(def app (noir-middleware/app-handler
       [home-routes
        teacher-routes
        admin-routes
        ortu-routes
        maintenance-routes
        tesmagang-routes
        adminmagang-routes
        app-routes
        ]))

(defn -main []
  (run-jetty app {:port 8080}))
