(ns icbl.routes.tesmagang
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [clojure.string :as st]
            [icbl.models.db :as db]
            [noir.session :as session]
            [icbl.models.share :as share]
            ))

(defn magang-login []
  (layout/render "share/login.html" {:action "/tm-login"}))

(defn handle-magang-login [nis pass]
  (let [user (db/get-data (str "select nis,password,nama from usermagang where nis='" nis "'") 1)
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))
        ]
      (if user
         (if (= pass (user :password))
           (do
             (session/put! :id nis)
             (session/put! :nama (user :nama))
             (session/put! :ip ip)
             (layout/render "tesmagang/home.html"))
           (layout/render "share/login.html"
                          {:error "Password Salah!" :nis nis :action "/tm-login"}))
         (layout/render "share/login.html"
                          {:error "Tidak ada user dengan ID tersebut!"
                           :nis nis :action "/tm-login"}))))

(defn handle-tm-kodeto [kodeto]
  (let [pre (subs kodeto 0 1)
        kd (subs kodeto 1 (count kodeto))]
     (if (not= pre "M")
       (layout/render "tesmagang/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto})
       (let [data (db/get-data (str "select * from magangproset where kode='" kd "'") 1)]

         (if (and data (= (data :status) "1"))
           (let [jsoal (data :jsoal)
                 vjaw (partition 5 (interleave (range 1 (inc jsoal))
                                               (read-string (data :jenis))
                                               (read-string (data :upto))
                                               (if (data :pretext) (read-string (data :pretext)) (repeat jsoal "-"))
                                               (if (data :sound) (read-string (data :sound)) (repeat jsoal "-"))))
                 ;vjaw-acak vjaw
                 vjaw1 (if (= "1" (data :acak)) (shuffle vjaw) vjaw)
                 nsoal (vec (map #(first %) vjaw1))
                 njenis (vec (map #(second %) vjaw1))
                 nupto (vec (map #(nth % 2) vjaw1))
                 npretext (vec (map #(nth % 3) vjaw1))
                 nsound (vec (map #(last %) vjaw1))
                 page "tesmagang/tryout.html"
                 ]
                ;(println vjaw)
                (layout/render page {:data data
                                     :nsoal nsoal
                                     :njenis njenis
                                     :nupto nupto
                                     :npretext npretext
                                     :nsound nsound
                                     :kodeto kodeto}))
           (layout/render "tesmagang/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto}))
    ))))

(defn handle-tm-to-lanjutan [kode]
  (let [pre (subs kode 0 1)
        remko (subs kode 1 (count kode))
        tabel "magangproset"
        data (db/get-data (str "select keterangan from " tabel " where kode='" remko "'") 1)]
  (layout/render "tesmagang/tryout-lanjutan.html" {:data data :kodeto kode})))

(defn handle-tm-simpan-jawaban [kode jawab ni]
  (let [nis ni
        prekode (subs kode 0 1)
        remkode (subs kode 1 (count kode))
        jawaban (st/split jawab #":")
        tdata "magangproset"
        dproset (db/get-data (str "select * from " tdata " where kode='" remkode "'") 1)
        ada (db/get-data (str "select nis from datamagang where nis='" nis "' and kode='" kode "'") 1)

        jsoal (count jawaban)
        kunci (read-string (:kunci dproset))

        jbenar (loop [jb 0, i 0]
                          (if (= i jsoal)
                              jb
                              (recur (if (= (jawaban i) (kunci i)) (inc jb) jb) (inc i))))
        jkosong (count (filter #(= % "-") jawaban))
        jsalah (- jsoal (+ jbenar jkosong))
        skala (:skala dproset)
        nbenar (:nbenar dproset)
        nsalah (:nsalah dproset)
        nilai (/ (Math/round (* (/ (+ (* jbenar nbenar) (* jsalah nsalah)) (* jsoal nbenar)) skala 100.0)) 100.0)
        vkd kode
        ]
         (if (not ada)
             (try (db/insert-data "datamagang"
                                            {:nis nis
                                             :kode vkd
                                             :jawaban (str jawaban)
                                             :nilai nilai
                                             :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
              {:nilai nilai :skala skala}
               ;{:nilai nil}
              (catch Exception ex
                {:nilai nil :skala skala}))
             (try (db/update-data-1 "datamagang"
                                    ["nis=? AND kode=?" nis vkd]
                                      {:nis nis
                                       :kode vkd
                                       :jawaban (str jawaban)
                                       :nilai nilai
                                       :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
               {:nilai nilai :skala skala}
               (catch Exception ex
                {:nilai nil :skala skala}))
           )))

(defroutes tesmagang-routes
  (GET "/magang" [] (magang-login))
  (POST "/tm-login" [nis pass]
        (handle-magang-login nis pass))

  (GET "/tm-logout" []
       (share/logout "/magang"))

  (POST "/tm-logout" []
        (share/logout "/magang"))

  (POST "/tm-no-lstore" []
        (layout/render "tesmagang/kode1.html"))

  (POST "/tm-lstore" []
        (layout/render "tesmagang/kode2.html"))

  (POST "/tm-kodeto" [kodeto]
        (handle-tm-kodeto kodeto))

    (POST "/tm-tryout-lanjutan" [kode]
        (handle-tm-to-lanjutan kode))

  (POST "/tm-tryout-baru" []
        (layout/render "tesmagang/kode1.html"))

;;   (GET "/tm-simpan/:kode/:jawaban/:nis" [kode jawaban nis]
;;       (resp/json (handle-tm-simpan-jawaban kode jawaban nis)))

  (POST "/tm-simpan" [kode jawaban nis]
        (resp/json (handle-tm-simpan-jawaban kode jawaban nis)))

)
