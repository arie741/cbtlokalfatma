(ns icbl.routes.ortu
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            ;[noir.validation :as vali]
            ;[noir.util.crypt :as crypt]
            [noir.response :as resp]
            [icbl.models.db :as db]
            [noir.session :as session]
            [icbl.models.share :as share]
            [icbl.routes.home :as home]
            ))

(defn ortu-login []
  (layout/render "share/login.html" {:action "/ortu-login"}))

(defn handle-ortu-login [nis pass]
  (let [user (db/get-data (str "select nis,passortu,nama from users where nis='" nis "'") 1)
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))
        ]
      (if user
         (if (= pass (user :passortu))
           (do
             (session/put! :id nis)
             (session/put! :nama (user :nama))
             (session/put! :ip ip)
             (session/put! :status 5)
             (layout/render "ortu/home.html"))
           (layout/render "share/login.html"
                          {:error "Password Salah!" :nis nis :action "/ortu-login"}))
         (layout/render "share/login.html"
                          {:error "Tidak ada user dengan NIS tersebut!"
                           :nis nis
                           :action "/ortu-login"}))
    ))

(defn ortu-ganti-pw [pwlama pwbaru1 pwbaru2]
  (let [pwnow (:passortu (db/get-data (str "select passortu from users where nis='" (session/get :id) "'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru1) 5))
        (layout/render "ortu/ganti-pw.html" {:error "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru1 pwbaru2)
          (try (db/update-data "users" (str "nis='" (session/get :id) "'") {:passortu pwbaru2})
                 (do
                    (session/clear!)
                    (resp/redirect "/ortu"))
               (catch Exception ex
                  (layout/render "ortu/ganti-pw.html" {:error "Gagal ganti password!"})))
          (layout/render "ortu/ganti-pw.html" {:error "Password tidak sesuai!"})))))

(defroutes ortu-routes
  (GET "/ortu" [] (ortu-login))

  (POST "/ortu-login" [nis pass]
       (handle-ortu-login nis pass))

  (GET "/ortu-logout" []
        (share/logout "/ortu"))

  (GET "/ortu-ganti-pw" []
        (layout/render "ortu/ganti-pw.html"))
  (POST "/ortu-ganti-pw1" [pwlama pwbaru1 pwbaru2]
        (ortu-ganti-pw pwlama pwbaru1 pwbaru2))
)
