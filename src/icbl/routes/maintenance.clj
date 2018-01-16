(ns icbl.routes.maintenance
  (:require [compojure.core :refer :all]
            ;[clojure.string :as st]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            ;[noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            ;[clojure.data.json :as json]
            ;[icbl.routes.teacher :as teacher]
            ))

(defn logout []
  (do
   (session/clear!)
   (resp/redirect "/maintenance")))

(defn handle-login [pass]
  (let [vpass (:pass (db/get-data (str "select pass from admin where id='maintenance'") 1))
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))]
    (if (= vpass pass)
        (do
          (session/put! :id "maintenance")
          (session/put! :status 3)
          (session/put! :ip ip)
          (layout/render "maintenance/work.html"))
        (layout/render "maintenance/home.html" {:error "Password Salah!"}))))

(defn maintenance-home []
  (layout/render "maintenance/home.html")
  )

(defroutes maintenance-routes

  (GET "/maintenance" []
      (maintenance-home))

  (GET "/maintenance-home" []
       (layout/render "maintenance/work.html"))

  (GET "/maintenance-logout" []
       (logout))

  (POST "/maintenance-login" [pass]
      (handle-login pass))

;;   (GET "/admin-home" []
;;        (layout/render "admin/work.html"))

;;   (GET "/admin-logout" []
;;        (logout))

;;   (POST "/admin-login" [pass]
;;       (handle-login pass))
  )
