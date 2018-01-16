(ns icbl.models.updatedb
    (:require [clojure.java.jdbc :as sql]
              [icbl.models.db :as db]
              [clojure.string :as st])
    (:import (java.io File)))

(def db1
{:subprotocol "postgresql"
:subname "//localhost:5432/cbtzencen"
:user "tosmp"
:password "tosmp2000"})

(defmacro with-db1 [f & body]
`(sql/with-connection ~db1 (~f ~@body)))

(defn get-data1 [query toggle]
  (if (= toggle 1)
  (with-db1 sql/with-query-results res [query] (first res))
  (with-db1 sql/with-query-results res [query] (doall res))))

(defn update-data-2
  "table: table name
  query: where-params
  data: column to update"
  [table query data]
  (with-db1 sql/update-values (keyword table) query data))

(defn update [id pelajaran]
  (let [datafat (get-data1 (str "select * from proset where id='" id "'") 2)
        ;maxkode (db/get-data (str "select MAX(kode) from bankproset where pelajaran='" pelajaran "'") 1)
        ;mkd (if (= (:max maxkode) nil) 0 (:max maxkode))
        ;datatambah (filter #(> (:kode %) mkd) datafat)
        datatambah datafat
        cdt (count datatambah)
       ]
    (loop [i 0]
      (if (= i cdt)
        "selesai"
        (do
          (db/insert-data "bankproset" {:kode (:kode (nth datatambah i))
                                        :jsoal (:jsoal (nth datatambah i))
                                        :waktu (:waktu (nth datatambah i))
                                        :pelajaran pelajaran
                                        :keterangan (str (:pelajaran (nth datatambah i)) " " (:keterangan (nth datatambah i)))
                                        :status (:status (nth datatambah i))
                                        :id id
                                        :kunci (:kunci (nth datatambah i))
                                        :jenis (:jenis (nth datatambah i))
                                        :upto (:upto (nth datatambah i))
                                        :acak (:acak (nth datatambah i))
                                        :skala 10
                                        :nbenar 1
                                        :nsalah 0
                                        })
          (recur (inc i))))
    )
    )
  )

(defn modify-ps []
  (let [data (db/get-data (str "select jsoal,kode from bankproset") 2)]
    (doseq [x data] (db/update-data-1 "bankproset" ["kode=?" (:kode x)] {:pretext (str (vec (repeat (:jsoal x) "-")))
                                                                         :sound (str (vec (repeat (:jsoal x) "-")))}))))

(defn insert-data-pig [file]
  (let [data (slurp file)
        sdata (st/split data #"\n")
        vdata (map #(st/split % #",") (if (not (vector? sdata)) (st/split data #"\r") sdata))
        ;coba (spit (str vdata) "coba.txt")
        ]
        (loop [i 1]
          (if (= i (count vdata))
            "selesai"
            (do
              (let [kodejur_ada (db/get-data (str "select kodejur from pg where kodejur='" ((nth vdata i) 1) "'") 1)]
                (if (not kodejur_ada)
                  (db/insert-data "pg" {:kelompok ((nth vdata i) 0)
                                           :kodejur ((nth vdata i) 1)
                                           :jurusan ((nth vdata i) 2)
                                           :ptn ((nth vdata i) 3)
                                           :kodeptn ((nth vdata i) 4)
                                           :nilaimin ((nth vdata i) 5)

                                           })))
              (recur (inc i)))))))

(defn test-input [file]
  (let [data (slurp file)
        sdata (st/split data #"\n")
        vdata (map #(st/split % #",") (if (not (vector? sdata)) (st/split data #"\r") sdata))]
    vdata))

(defn convert-nm []
  (let [data (db/get-data "select distinct nilaimin from pg1" 2)
        cdata (count data)]
    (loop [i 0]
      (if (= i cdata)
        "selesai"
        (do
          (db/update-data-1 "pg1" ["nilaimin=?" (:nilaimin (nth data i))] {:nm (read-string (:nilaimin (nth data i)))})
          (recur (inc i)))))))

(defn updatedb-buat-kode []
  (let [letters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890"
        sufik (loop [a [], i 0]
                (if (= i 32)
                  (apply str a)
                  (recur (conj a (rand-nth letters)) (inc i))))
        ]
    sufik))

(defn insert-all-kodex [dbase]
  (let [data (get-data1 (str "select kode from " dbase) 2)]
    (doseq [x data] (update-data-2 dbase ["kode=?" (:kode x)] {:kodex (updatedb-buat-kode)}))))

(defn rename-dir-soal [dbase]
  (let [data (if (= dbase "proset")
                 (db/get-data "select kode,kodex,id from proset where status='0'" 2)
                 (db/get-data "select kode,kodex,kodepel from bankproset where status='0'" 2))
        ]
    (if (= dbase "proset")
        (doseq [x data]
               (.renameTo (File. (str "resources/public/proset/" (:id x) "/" (:kode x)))
                          (File. (str "resources/public/proset/" (:id x) "/" (:kodex x)))))
        (doseq [x data]
               (.renameTo (File. (str "resources/public/bankproset/" (:kodepel x) "/" (:kode x)))
                          (File. (str "resources/public/bankproset/" (:kodepel x) "/" (:kodex x)))))
      )))
