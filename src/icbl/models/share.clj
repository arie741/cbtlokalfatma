(ns icbl.models.share
  (:require
    [noir.session :as session]
    [noir.response :as resp]
    [icbl.models.db :as db]
    [icbl.views.layout :as layout]))

(defn logout [page]
  (do
   (session/clear!)
   (resp/redirect page)))

(defn hitung-nilai [jawaban kunci nbenar nsalah skala]
  (let [jsoal (count kunci)
        jbenar (loop [jb 0, i 0]
                          (if (= i jsoal)
                              jb
                              (recur (if (= (subs jawaban i (inc i)) (kunci i)) (inc jb) jb) (inc i))))
        jkosong (count (filter #(= % \-) (vec jawaban)))
        jsalah (- jsoal (+ jbenar jkosong))
        nilai (/ (Math/round (* (/ (+ (* jbenar nbenar) (* jsalah nsalah)) (* jsoal nbenar)) skala 100.0)) 100.0)]
    nilai))

(defn hitung-ulang [kode]
  (let [prekode (subs kode 0 1)
        remkode (subs kode 1 (count kode))
        tdata (if (= (subs kode 0 1) "B") "bankproset" "proset")
        dproset (db/get-data (str "select * from " tdata " where kode='" remkode "'") 1)
        dtest (db/get-data (str "select nis,jawaban from dataus where kode='" kode "'") 2)
        kunci (vec (map str (seq (:kunci dproset))))
        skala (:skala dproset)
        nbenar (:nbenar dproset)
        nsalah (:nsalah dproset)]
        ;(println (nth dtest 0))
        ;(do
        (doseq [dsiswa dtest]
              (let [vjawab (dsiswa :jawaban)
                    vnis (dsiswa :nis)]
                   (db/update-data "dataus" (str "kode='" kode "' and nis='" vnis "'")
                            {:nilai (hitung-nilai vjawab kunci nbenar nsalah skala)})))
          ;(layout/render "admin/pesan.html" {:pesan "Hitung ulang sudah selesai!"})
    ))

(defn create32bit []
  (let [vc (vec "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        cvc (count vc)]
    (loop [i 0 x []]
      (if (= i 32)
        (apply str x)
        (recur (inc i)
               (conj x (rand-nth vc)))))))

(defn create-kode [nchar]
  (let [vc (vec "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
        cvc (count vc)]
    (loop [i 0 x []]
      (if (= i nchar)
        (apply str x)
        (recur (inc i)
               (conj x (rand-nth vc)))))))


