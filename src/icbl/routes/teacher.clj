(ns icbl.routes.teacher
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            [clojure.data.json :as json]
            [icbl.models.share :as share]
            )
 (:import (java.io File)))

(defn teacher-num-to-str [number dk]
  (-> (format (str "%." dk "f") (* number 1.0))
      (clojure.string/replace #"\." ",")))

(defn teacher-home []
  (layout/render "teacher/home.html"))

(defn teacher-login [id pass]
  (let [user (db/get-data (str "select * from teacher where id='" id "'") 1)
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))]
    (if user
        (if (= pass (user :pass))
        (do
          (session/put! :id (user :id))
          (session/put! :nama (user :nama))
          (session/put! :status 2)
          (session/put! :ip ip)
          (layout/render "teacher/work.html"))
        (layout/render "teacher/home.html" {:error "Password Salah!" :id id}))
      (layout/render "teacher/home.html" {:error "ID tersebut tidak ada!"}))))

(defn teacher-ganti-pw [id]
  (let [user (db/get-data (str "select pass from teacher where id='" id "'") 1)]
    (layout/render "teacher/ganti-pw.html" {:pass (user :pass)})))

(defn handle-teacher-ganti-pw [id p1 p2]
  (let [user (db/get-data (str "select id, pass from teacher where id='" id "'") 1)]
    (if (= p1 (user :pass))
      (do
        (db/update-data "teacher" (str "id='" id "'") {:pass p2})
        (layout/render "teacher/pesan.html" {:pesan "Berhasil ubah password!"}))
      (layout/render "teacher/pesan.html" {:pesan "Password tidak diubah, karena password lama salah!"}))))

(defn teacher-logout []
  (do
   (session/clear!)
   (resp/redirect "/teacher")))

(defn handle-teacher-buat-proset [pel ket jsoal waktu jumpil]
  (try
      (db/insert-data "proset" {:id (session/get :id)
                                :pelajaran pel
                                :keterangan ket
                                :jsoal (Integer/parseInt jsoal)
                                :waktu (Integer/parseInt waktu)
                                :jumpil jumpil
                                :kunci (apply str (repeat (Integer/parseInt jsoal) "-"))
                                :jenis (apply str (repeat (Integer/parseInt jsoal) "1"))
                                :upto (apply str (repeat (Integer/parseInt jsoal) "-"))
                                ;:pretext (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                ;:sound (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                :acak "0"
                                :status "0"
                                :skala 10
                                :nbenar 1
                                :nsalah 0
                                :kodex (share/create-kode 32)})
      (layout/render "teacher/pesan.html" {:pesan (str "Berhasil daftarkan proset!")})
      (catch Exception ex
                  (layout/render "teacher/pesan.html" {:pesan (str "Gagal daftarkan proset! error: " ex)}))))

(defn teacher-lihat-proset [id]
  (let [data (db/get-data (str "select kode,pelajaran,keterangan,jsoal,waktu,status from proset where id='" id
                               "' order by kode desc") 2)]
    (layout/render "teacher/lihat-proset.html" {:data data :kset "L"})))

(defn teacher-edit-proset [kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from proset where kode='" postkode "'") 1)]
    (layout/render "teacher/edit-proset.html" {:datum datum :kode kode})))

(defn teacher-update-proset [kode pel ket jsoal waktu jumpil skala nbenar nsalah acak status]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select kunci,jenis,upto,pretext,sound,status,kodex,id from proset where kode='" postkode "'") 1)
        oldkunci (datum :kunci)
        oldjenis (datum :jenis)
        oldupto (datum :upto)
        oldpretext (if (datum :pretext) (read-string (datum :pretext)) nil)
        oldsound (if (datum :sound) (read-string (datum :sound)) nil)
        oldstatus (datum :status)
        cok (count oldkunci)
        vjsoal (Integer/parseInt jsoal)
        newkunci (cond
                   (= vjsoal cok) oldkunci
                   (< vjsoal cok) (subs oldkunci 0 vjsoal)
                   :else (str oldkunci (apply str (repeat (- vjsoal cok) "-"))))
        newjenis (cond
                   (= vjsoal cok) oldjenis
                   (< vjsoal cok) (subs oldjenis 0 vjsoal)
                   :else (str oldjenis (apply str (repeat (- vjsoal cok) "1"))))
        newupto (cond
                   (= vjsoal cok) oldupto
                   (< vjsoal cok) (subs oldupto 0 vjsoal)
                   :else (str oldupto (apply str (repeat (- vjsoal cok) "-"))))
        newpretext (if oldpretext
                     (cond
                     (= vjsoal cok) (str oldpretext)
                     (< vjsoal cok) (str (vec (take vjsoal oldpretext)))
                     :else (str (vec (concat oldpretext (repeat (- vjsoal cok) "-"))))) nil)
         newsound (if oldsound
                    (cond
                     (= vjsoal cok) (str oldsound)
                     (< vjsoal cok) (str (vec (take vjsoal oldsound)))
                     :else (str (vec (concat oldsound (repeat (- vjsoal cok) "-"))))) nil)
        id (datum :id)
        kodex (datum :kodex)
        ]
  (try
    (db/update-data "proset" (str "kode='" postkode "'")
                    {:pelajaran pel :keterangan ket
                     :jsoal vjsoal
                     :waktu (Integer/parseInt waktu)
                     :jumpil jumpil
                     :acak acak
                     :status status
                     :kunci newkunci
                     :jenis newjenis
                     :upto newupto
                     :pretext newpretext
                     :sound newsound
                     :skala (Integer/parseInt skala)
                     :nbenar (Integer/parseInt nbenar)
                     :nsalah (Integer/parseInt nsalah)})
    (do
    (if (not= status oldstatus)
      (if (= status "0")
        (.renameTo (File. (str "resources/public/proset/" id "/" postkode))
                   (File. (str "resources/public/proset/" id "/" kodex)))
        (.renameTo (File. (str "resources/public/proset/" id "/" kodex))
                   (File. (str "resources/public/proset/" id "/" postkode))))
      nil)
    (layout/render "teacher/pesan.html" {:pesan (str "Berhasil update proset!")})
      )
    (catch Exception ex
                  (layout/render "teacher/pesan.html" {:pesan (str "Gagal update proset! error: " ex)})))))

(defn teacher-pilih-proset [ks id act]
  (let [data (if (= ks "L")
               (db/get-data (str "select * from proset where id='" id "' order by kode desc") 2)
               (db/get-data (str "select bpguru.kode,pelajaran,keterangan,jsoal,waktu,status from bpguru
                                 INNER JOIN bankproset ON bpguru.kode=bankproset.kode where idguru='" id "'
                                 order by kode desc") 2)
               )]
    (layout/render "teacher/pilih-proset.html" {:data data :action act :kset ks})))

(defn teacher-pilih-proset-sekaligus [id act target]
  (let [data (db/get-data (str "select * from proset where id='" id "' order by kode desc") 2)]
    (layout/render "teacher/pilih-proset-sekaligus.html" {:data data :action act
                                                          :target target
                                                          :kset "L"})))

(defn teacher-upload-file [kode id]
  (let [datum (db/get-data (str "select status,kodex from proset where kode='" kode "'") 1)
        status (datum :status)
        kodex (datum :kodex)]
    (if (= status "0")
      (do
        (io/create-path (str "resources/public/proset/" id "/" kodex) true)
        (layout/render "teacher/upload.html" {:kode kode})))))

(defn handle-teacher-upload [id kode file]
  (let [datum (db/get-data (str "select kodex from proset where kode='" kode "'") 1)
        kodex (datum :kodex)]
    (try
      (if (vector? file)
        (doseq [i file]
            (io/upload-file (str "resources/public/proset/" id "/" kodex) i))
        (io/upload-file (str "resources/public/proset/" id "/" kodex) file))
      (layout/render "teacher/pesan.html" {:pesan "Berhasil upload file!"})
      (catch Exception ex
                    (layout/render "teacher/pesan.html" {:pesan (str "Gagal upload file! error: " ex)}))
      )))

(defn teacher-buat-kunci [kode]
  (let [datum (db/get-data (str "select jsoal from proset where kode='" kode "'") 1)]
    (layout/render "teacher/buat-kunci.html" {:kode kode :jsoal (datum :jsoal)})))

(defn teacher-save-kunci [kunci jenis upto pretext sound kode]
  (try
    (db/update-data "proset" (str "kode='" kode "'") {:kunci kunci :jenis jenis :upto upto :pretext pretext :sound sound})
    (layout/render "teacher/pesan.html" {:pesan "Kunci berhasil disimpan!"})
    (catch Exception ex
                  (layout/render "teacher/pesan.html" {:pesan (str "Gagal simpan kunci! error: " ex)}))))

(defn teacher-edit-kunci [kode act]
  (let [datum (db/get-data (str "select kunci,jsoal,jenis,upto,pretext,sound from proset where kode='" kode"'") 1)]
    (layout/render "teacher/edit-kunci.html" {:kunci (datum :kunci)
                                              :jsoal (datum :jsoal)
                                              :jenis (datum :jenis)
                                              :upto (datum :upto)
                                              :pretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                              :sound (if (datum :sound) (read-string (datum :sound)) nil)
                                              :kode kode
                                              :action act})))

(defn teacher-pilih-kelas [kode act]
  (let [allkelas (db/get-data (str "select dataus.nis as nis,kelas from dataus INNER JOIN users
                                 ON dataus.nis=users.nis where dataus.kode='" kode "'") 2)
        vkelas (conj (sort (distinct (map #(:kelas %) allkelas))) "SEMUA")]
    (layout/render "teacher/pilih-kelas.html" {:kelas vkelas :kode kode :action act})))

(defn teacher-hasil-test [kode kelas html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        ckode (count kode)
        tdata (if (= prekode "B") "bankproset" "proset")
        mdata (db/get-data (str "select kode,pelajaran,keterangan,jsoal from " tdata " where kode='" postkode "'") 1)
;;         data (db/get-data (str "select dataus.nis as nis,nama,kelas,nilai,jawaban from dataus INNER JOIN " tdata "
;;                                ON " tdata ".kode=to_number(substring(dataus.kode,2," ckode "),'999999999')
;;                                INNER JOIN users ON users.nis=dataus.nis
;;                                where " tdata ".kode='" postkode "' order by nilai desc") 2)
        data (if (= kelas "SEMUA")
                 (db/get-data (str "select dataus.nis as nis,nama,kelas,nilai,jawaban from dataus INNER JOIN
                               users ON users.nis=dataus.nis WHERE dataus.kode='" kode "' order by nilai desc") 2)
                 (db/get-data (str "select dataus.nis as nis,nama,kelas,nilai,jawaban from dataus INNER JOIN
                               users ON users.nis=dataus.nis WHERE dataus.kode='" kode "' and kelas='" kelas "'
                               order by nilai desc") 2))
        data1 (map #(update-in %1 [:nilai] teacher-num-to-str 2) data)
        kunci (:kunci (db/get-data (str "select kunci from " tdata " where kode='" postkode "'") 1))]
    (layout/render html {:data data1 :mdata mdata :kunci kunci :kode kode})))

(defn teacher-test-detail-siswa [nis kode]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata (if (= prekode "B") "bankproset" "proset")
        pkt (db/get-data (str "select kode,pelajaran,keterangan,jsoal from " tdata " where kode='" postkode "'") 1)
        kunci (:kunci (db/get-data (str "select kunci from " tdata " where kode='" postkode "'") 1))
        data (db/get-data (str "select dataus.nis as nis,kode,jawaban,nilai,nama from dataus
                               INNER JOIN users ON dataus.nis=users.nis
                               where kode='" kode "' and dataus.nis='" nis "'") 1)
        jawaban (data :jawaban)
        jsoal (pkt :jsoal)
        benar (count (filter true? (map #(= %1 %2) (vec kunci) (vec jawaban))))
        kosong (count (filter true? (map #(= % \-) (vec jawaban))))
        salah (- jsoal (+ benar kosong))
         ]
        (layout/render "teacher/nilai-detail-siswa.html"
                       {:data data
                        :pelajaran (pkt :pelajaran)
                        :keterangan (pkt :keterangan)
                        ;:kodesoal kodesoal
                        :benar benar
                        :salah salah
                        :kosong kosong
                        :kunci kunci
                        :jawaban jawaban
                        :kode kode
                        })))

(defn teacher-lihat-soal [nomer kode]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata (if (= prekode "B") "bankproset" "proset")
        id (if (= prekode "B")
           (:kodepel (db/get-data (str "select kodepel from bankproset where kode='" postkode "'") 1))
           (:id (db/get-data (str "select id from proset where kode='" postkode "'") 1)))]
    (layout/render "teacher/lihat-soal.html" {:id id :kode kode :nomer nomer
                                              :postkode postkode :tabel tdata})))

(defn hitung-bsk [no kun dt]
  (loop [[k b s] [0 0 0], j 0]
       (if (= j (count dt))
           [k b s]
           (recur
             (cond
                (= (subs (:jw (nth dt j)) no (inc no)) "-") [(inc k) b s]
                (= (subs (:jw (nth dt j)) no (inc no)) kun) [k (inc b) s]
                :else [k b (inc s)])
             (inc j))
         )))

(defn teacher-abs [kode html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata (if (= prekode "B") "bankproset" "proset")
        ;pkt (db/get-data (str "select kode,pelajaran,keterangan from " tdata " where kode='" postkode "'") 1)
        pkt (if (= prekode "B")
              (db/get-data (str "select kode,pelajaranbs.pelajaran as pelajaran,keterangan from bankproset
                                inner join pelajaranbs on bankproset.kodepel=pelajaranbs.nomer
                                where kode='" postkode "'") 1)
              (db/get-data (str "select kode,pelajaran,keterangan from proset where kode='" postkode "'") 1))
        data (db/get-data (str "select jawaban as jw from dataus where kode='" kode "'") 2)
        kunci (:kunci (db/get-data (str "select kunci from " tdata " where kode='" postkode "'") 1))

        jsoal (count kunci)
        vhasil (loop [hsl [], i 0]
                     (if (= i jsoal)
                         hsl
                         (let [v (hitung-bsk i (subs kunci i (inc i)) data)] (recur (conj hsl v) (inc i)))))
        ]
        (layout/render html {:pelajaran (pkt :pelajaran)
                             :paket (pkt :keterangan)
                             :kode kode
                             :peserta (count data)
                             :hasil vhasil})))

(defn teacher-abs-tk [kode html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata (if (= prekode "B") "bankproset" "proset")
        ;pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci from " tdata " where kode='" postkode "'") 1)
        pkt (if (= prekode "B")
              (db/get-data (str "select kode,pelajaranbs.pelajaran as pelajaran,keterangan,kunci from bankproset
                                inner join pelajaranbs on bankproset.kodepel=pelajaranbs.nomer
                                where kode='" postkode "'") 1)
              (db/get-data (str "select kode,pelajaran,keterangan,kunci from proset where kode='" postkode "'") 1))
        kunci (pkt :kunci)
        ;;;Analisis Tingkat Kesulitan
        datatk (db/get-data (str "select jawaban as jwtk, nilai from dataus
                                 where kode='" kode "' order by nilai desc") 2)
        jdatatk (count datatk)
        jU (Math/round (* jdatatk 0.25))
        datatk1 (map #(% :jwtk) (concat (take jU datatk) (drop (- jdatatk jU) datatk)))
        cdatatk1 (count datatk1)
        vtk (loop [vt [] i 0]
              (if (= i (count kunci))
                  vt
                  (let [kun (subs kunci i (inc i))
                        jbi (count
                              (filter (fn [x] (= x true))
                                (map #(= kun (subs % i (inc i))) datatk1)))
                        tk (/ (Math/round (/ jbi cdatatk1 0.01)) 100.0)] (recur (conj vt tk) (inc i)))))

        ;;;
        ]
      (layout/render html {:pelajaran (pkt :pelajaran)
                           :paket (pkt :keterangan)
                           :kode kode
                           :peserta (count datatk)
                           :hasil vtk})))

(defn teacher-abs-dp [kode html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata (if (= prekode "B") "bankproset" "proset")
        ;pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci from " tdata " where kode='" postkode "'") 1)
        pkt (if (= prekode "B")
              (db/get-data (str "select kode,pelajaranbs.pelajaran as pelajaran,keterangan,kunci from bankproset
                                inner join pelajaranbs on bankproset.kodepel=pelajaranbs.nomer
                                where kode='" postkode "'") 1)
              (db/get-data (str "select kode,pelajaran,keterangan,kunci from proset where kode='" postkode "'") 1))
        kunci (pkt :kunci)
        ;;;Analisis Daya Pemisah
        datatk (db/get-data (str "select jawaban as jwtk, nilai from dataus
                                 where kode='" kode "' order by nilai desc") 2)
        jdatatk (count datatk)
        jU (Math/round (* jdatatk 0.25))
        datatkU (map #(% :jwtk) (take jU datatk))
        datatkL (map #(% :jwtk) (drop (- jdatatk jU) datatk))

        vdp (loop [dp [] i 0]
              (if (= i (count kunci))
                  dp
                  (let [kun (subs kunci i (inc i))
                        jbiU (count
                               (filter (fn [x] (= x true))
                                 (map #(= kun (subs % i (inc i))) datatkU)))
                        jbiL (count
                               (filter (fn [x] (= x true))
                                 (map #(= kun (subs % i (inc i))) datatkL)))
                        tdp (/ (Math/round (/ (- jbiU jbiL) jU 0.01)) 100.0)] (recur (conj dp tdp) (inc i)))))

        ;;;
        ]
      (layout/render html {:pelajaran (pkt :pelajaran)
                           :paket (pkt :keterangan)
                           :kode kode
                           :peserta (count datatk)
                           :hasil vdp})))

(defn hitung-abc [no kun dt jp]
  (if (= jp "4")
    (loop [[a b c d k] [0 0 0 0 0], j 0]
         (if (= j (count dt))
             [kun a b c d k]
             (recur
               (cond
                  (= (subs (:jw (nth dt j)) no (inc no)) "-") [a b c d (inc k)]
                  (= (subs (:jw (nth dt j)) no (inc no)) "A") [(inc a) b c d k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "B") [a (inc b) c d k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "C") [a b (inc c) d k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "D") [a b c (inc d) k]
                 :else [a b c d (inc k)]
                )
               (inc j))))

    (loop [[a b c d e k] [0 0 0 0 0 0], j 0]
         (if (= j (count dt))
             [kun a b c d e k]
             (recur
               (cond
                  (= (subs (:jw (nth dt j)) no (inc no)) "-") [a b c d e (inc k)]
                  (= (subs (:jw (nth dt j)) no (inc no)) "A") [(inc a) b c d e k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "B") [a (inc b) c d e k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "C") [a b (inc c) d e k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "D") [a b c (inc d) e k]
                  (= (subs (:jw (nth dt j)) no (inc no)) "E") [a b c d (inc e) k]
                 :else [a b c d e (inc k)]
                )
               (inc j))))))

(defn teacher-dayakecoh [kode html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata (if (= prekode "B") "bankproset" "proset")
        ;pkt (db/get-data (str "select kode,pelajaran,keterangan,kunci,jumpil from " tdata " where kode='" postkode "'") 1)
        pkt (if (= prekode "B")
              (db/get-data (str "select kode,pelajaranbs.pelajaran as pelajaran,keterangan,kunci,jumpil from bankproset
                                inner join pelajaranbs on bankproset.kodepel=pelajaranbs.nomer
                                where kode='" postkode "'") 1)
              (db/get-data (str "select kode,pelajaran,keterangan,kunci,jumpil from proset where kode='" postkode "'") 1))
        data (db/get-data (str "select jawaban as jw from dataus where kode='" kode "'") 2)
        ;kunci (:kunci (db/get-data (str "select kunci from " tdata " where kode='" postkode "'") 1))
        kunci (pkt :kunci)
        jumpil (pkt :jumpil)
        jsoal (count kunci)
        vhasil (loop [hsl [], i 0]
                     (if (= i jsoal)
                         hsl
                         (let [v (hitung-abc i (subs kunci i (inc i)) data jumpil)] (recur (conj hsl v) (inc i)))))
        ]
        (layout/render html {:pelajaran (pkt :pelajaran)
                             :paket (pkt :keterangan)
                             :kode kode
                             :peserta (count data)
                             :jumpil jumpil
                             :hasil vhasil})))

(defn teacher-view-soal [kode html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tabel (if (= prekode "B") "bankproset" "proset")
        datum (db/get-data (str "select * from " tabel " where kode='" postkode "'") 1)
        status (datum :status)
        kodesoal (if (= status "0") (datum :kodex) postkode)]
    (layout/render html {:datum datum
                         :nsoal (vec (range 1 (inc (datum :jsoal))))
                         :kategori "1"
                         :kodesoal kodesoal
                         :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                         :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                         :kode kode })))

(defn teacher-lihat-sekaligus [kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from proset where kode='" postkode "'") 1)
        status (datum :status)
        kodesoal (if (= status "0") (datum :kodex) postkode)]
    (layout/render "teacher/view-soal-sekaligus.html" {:datum datum
                                                       :kode kode
                                                       :kodesoal kodesoal
                                                       :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                                       :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                                       ;soalpath "http://localhost/resources/public"
                                                       })))

(defn teacher-arrange-rekap [id subjek]
  (let [data1 (db/get-data (str "select kode,pelajaran,keterangan from proset where id='" id "'
                               and status='1' order by keterangan, pelajaran") 2)
        data2 (db/get-data (str "select bpguru.kode as kode,pelajaran,keterangan from bpguru
                                INNER JOIN bankproset ON bpguru.kode=bankproset.kode
                                where bpguru.idguru='" id "' order by keterangan, pelajaran") 2)
        data3 (map #(assoc-in % [:kode] (str "L" (% :kode))) data1)
        data4 (map #(assoc-in % [:kode] (str "B" (% :kode))) data2)
        data (vec (concat data3 data4))]

    (layout/render "teacher/rekap-test.html" {:data (json/write-str data) :subjek subjek})))

(defn teacher-save-rekap [subj tes id]
  (try
    (db/insert-data "rekap" {:id id :subjek subj :tests tes})
    (layout/render "teacher/pesan.html" {:pesan "Data rekapitulasi berhasil disimpan!"})
    (catch Exception ex
                  (layout/render "teacher/pesan.html" {:pesan (str "Gagal simpan data rekapitulasi! error: " ex)}))))

(defn teacher-confirm-hapus [kode]
  (let [vkode (subs kode 1 (count kode))
        proset (db/get-data (str "select kode,pelajaran,keterangan from proset where kode='" vkode "'") 1)]
    (layout/render "teacher/confirm-hapus.html" {:kode kode
                                                 :pelajaran (proset :pelajaran)
                                                 :keterangan (proset :keterangan)})))
(defn teacher-hapus-set [kode]
  (try
    (db/delete-data "proset" (str "kode='" (subs kode 1 (count kode)) "'"))
    (layout/render "teacher/pesan.html" {:pesan (str "Soal dengan kode " kode " berhasil dihapus!")})
    (catch Exception ex
      (layout/render "teacher/pesan.html" {:pesan (str "Gagal Hapus Proset! error " ex)}))
    ))

(defn teacher-pilih-rekap [id act]
  (let [data (db/get-data (str "select * from rekap where id='" id "'") 2)]
    (layout/render "teacher/list-rekap.html" {:data data :action act})))

(defn teacher-edit-rekap [id kode]
  (let [dset1 (vec (db/get-data (str "select kode,pelajaran,keterangan from proset where id='" id "'
                               and status='1' order by keterangan, pelajaran") 2))
        dset2 (db/get-data (str "select bpguru.kode as kode,pelajaran,keterangan from bpguru
                                INNER JOIN bankproset ON bpguru.kode=bankproset.kode
                                where bpguru.idguru='" id "' order by keterangan, pelajaran") 2)
        dset3 (map #(assoc-in % [:kode] (str "L" (% :kode))) dset1)
        dset4 (map #(assoc-in % [:kode] (str "B" (% :kode))) dset2)
        dset (vec (concat dset3 dset4))
        drekap (db/get-data (str "select subjek, tests from rekap where kode='" kode "'") 1)]

    (layout/render "teacher/edit-rekap.html" {:dset (json/write-str dset)
                                              :drekap (json/write-str (read-string (drekap :tests)))
                                              :subjek (drekap :subjek)
                                              :kode kode})))

(defn teacher-save-rekap-edit [tes sub ko]
  (try
    (db/update-data "rekap" (str "kode='" ko "'")
                    {:subjek sub
                     :tests tes})
    (layout/render "teacher/pesan.html" {:pesan (str "Berhasil Update Rekapitulasi test dengan kode " ko)})
    (catch Exception ex
      (layout/render "teacher/pesan.html" {:pesan (str "Gagal Update Rekapitulasi! error " ex)}))))

(defn cbobot [mtes mn]
  (let [vk (filter #(= (read-string (% :kode)) (mn :kode)) mtes)
        bobot (read-string ((first vk) :bobot))]
    (* bobot (mn :nilai) 0.01)))

;(cbobot [{:kode 1 :bobot "50"}{:kode 6 :bobot "50"}] {:kode 1 :nilai 30})

(defn teacher-hasil-rekap [kode]
  (let [drekap (db/get-data (str "select kode,subjek,tests from rekap where kode='" kode "'") 1)
        vtes (read-string (drekap :tests))
        vtes1 (map (fn [s] (s :kode)) vtes)
        vtes2 (map (fn [s] (str "kode='" s "'")) vtes1)
        vtes3 (apply str (interpose " or " vtes2))
        data (db/get-data (str "select dataus.nis,kode,nilai,nama  from dataus inner join users on dataus.nis=users.nis
                               where " vtes3 " order by nis,kode") 2)
        data1 (partition-by #(:nis %) data)
        data2 (loop [v [] i 0]
                (if (= i (count data1))
                  v
                  (recur (conj v {:nis (:nis (first (nth data1 i)))
                                  :nama (:nama (first (nth data1 i)))
                                  :nilai (/ (Math/round (* 100 (reduce + (map #(cbobot vtes %) (nth data1 i))))) 100.0)})
                         (inc i))))
        data3 (sort-by #(% :nama) data2)
        ]
    (layout/render "teacher/hasil-rekap.html" {:data data3 :subjek (drekap :subjek) :kode kode})))

(defn bobotrs [mtes mn]
  (let [vk (filter #(= (read-string (% :kode)) (mn :kode)) mtes)]
    (read-string ((first vk) :bobot))))

(defn teacher-rekap-siswa [nis kode]
  (let [drekap (db/get-data (str "select tests from rekap where kode='" kode "'") 1)
        vtes (read-string (drekap :tests))
        vtes1 (map (fn [s] (read-string (s :kode))) vtes)
        vtes2 (map (fn [s] (str "dataus.kode=" s)) vtes1)
        vtes3 (apply str (interpose " or " vtes2))
        data (db/get-data (str "select dataus.nis,dataus.kode,nilai,nama,keterangan  from dataus inner join users on dataus.nis=users.nis
                               inner join proset on dataus.kode=proset.kode
                               where (" vtes3 ") and dataus.nis='" nis "' order by nis,kode") 2)
        data1 (map #(dissoc % :nis :nama) data)
        data2 (map #(assoc % :nbobot (* (% :nilai) (bobotrs vtes %) 0.01)) data1)
        ntotal (reduce + (map #(% :nbobot) data2))
        nama ((first data) :nama)
        vtes4 (map (fn [s] (str "kode=" s)) vtes1)
        vtes5 (apply str (interpose " or " vtes4))
        dtest (db/get-data (str "select kode,keterangan from proset where (" vtes5 ") order by kode asc") 2)
        dtest1 (map #(assoc % :bobot (bobotrs vtes %)) dtest)
        ]
    (layout/render "teacher/rekap-siswa.html" {:nama nama
                                               :nis nis
                                               :dtest dtest1
                                               :dsiswa data2
                                               :ntotal (/ (Math/round (* 100 ntotal)) 100.0)})))

(defn handle-teacher-search-proset [nopel ket act]
  (let [Uket (clojure.string/upper-case ket)
        data (db/get-data (str "select kode,pelajaranbs.pelajaran as pelajaran,keterangan,jsoal,waktu,status from bankproset
                               inner join pelajaranbs on bankproset.kodepel=pelajaranbs.nomer where
                               kodepel='" nopel "' and upper(keterangan) LIKE '%" Uket "%'
                               order by keterangan") 2)]
    (layout/render "admin/list-proset.html" {:data data :action act :kodepel nopel :ket ket
                                             :target "_blank"})))

(defn handle-teacher-lihat-soal-bp [pel kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from bankproset where kode='" postkode "'") 1)
        status (datum :status)
        kodesoal (if (= status "0") (datum :kodex) postkode)]
    (layout/render "admin/view-soal-sekaligus.html" {:datum datum
                                                       :pel pel
                                                       :kode kode
                                                       :kodesoal kodesoal
                                                       :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                                       :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                                       ;soalpath "http://localhost/resources/public"
                                                       })))

(defn teacher-search-proset-bp [act]
  (let [data (db/get-data "select * from pelajaranbs order by pelajaran" 2)]
    (layout/render "admin/search-proset.html" {:act act :data data})))

(defn handle-teacher-catat-bp [ko]
  (let [kode (subs ko 1 (count ko))
        ckode (db/get-data (str "select kode from bpguru where kode='" kode "' and
                                idguru='" (session/get :id) "'") 1)]
    (if ckode
      (layout/render "teacher/pesan.html" {:pesan "Kode tersebut sudah tercatat!"})
      (let [data (db/get-data (str "select kode from bankproset where kode='" kode "'") 1)]
        (if data
          (try
            (db/insert-data "bpguru"
                        {:idguru (session/get :id)
                         :kode (Integer/parseInt kode)})
            (layout/render "teacher/pesan.html" {:pesan (str "Berhasil mencatat Kode Bank Set Soal dengan kode " ko)})
            (catch Exception ex
            (layout/render "teacher/pesan.html" {:pesan (str "Gagal mencatat kode Bank Set Soal! error " ex)})))
          (layout/render "teacher/catat-bp.html" {:kode (str "B" kode) :error "Tidak ada kode tersebut!"}))))))

(defn handle-teacher-lihat-catatan-bp [id act]
  (let [data (db/get-data (str "select bpguru.kode,pelajaran,keterangan,jsoal,waktu,status from bpguru INNER JOIN
                               bankproset ON bpguru.kode=bankproset.kode where idguru='" id "' order by kode") 2)]
    (if data
      (layout/render "admin/list-proset.html" {:data data
                                               :action act
                                               :target ""
                                               })
      (layout/render "teacher/pesan.html" {:pesan "Tidak ada catatan di buku!"}))))

(defn handle-teacher-lihat-soal-bp-1 [kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from bankproset where kode='" postkode "'") 1)
        status (datum :status)
        kodesoal (if (= status "0") (datum :kodex) postkode)]
    (layout/render "admin/view-soal-sekaligus.html" {:datum datum
                                                     :pel (:pelajaran datum)
                                                     :kode kode
                                                     :kodesoal kodesoal
                                                     :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                                     :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                                     ;soalpath "http://localhost/resources/public"
                                                       })))

(defn handle-teacher-hapus-bp-1 [kode]
  (try
    (db/delete-data "bpguru" (str "kode='" kode "' and idguru='" (session/get :id) "'"))
    (handle-teacher-lihat-catatan-bp (session/get :id) "/teacher-hapus-bp-1")
    (catch Exception ex
      (layout/render "teacher/pesan.html" {:pesan str "Gagal menghapus data, error:" ex}))))

(defn handle-teacher-bikin-soal [kd]
  (layout/render "teacher/bikin-soal-html.html" {:kode kd}))

(defn handle-teacher-simpan-soal-html [soal nf kd id]
  (do
    (spit (str "resources/public/proset/" id "/" (subs kd 1 (count kd)) "/" nf) soal)
    (layout/render "teacher/bikin-soal-html.html" {:kode kd})))

(defn teacher-simpan-kode [kode act]
  (try
    (db/insert-data "sesibahas" {:kodesoal kode :id (session/get :id)})
    (let [kodebahas (:nomer (db/get-data
                                (str "select nomer from sesibahas where kodesoal='" kode "'") 1))]
    (layout/render "teacher/kode-bahas.html" {:kode kode :kodebahas kodebahas :action act}))
    ;(layout/render "teacher/kode-bahas.html" {:kode kode :kodebahas "12"})
    (catch Exception ex
      (layout/render "teacher/pesan.html" {:pesan str "Gagal membuat kode pembahasan, error:" ex}))))

(defn teacher-selesai-bahas [id]
  (do
    (db/delete-data "sesibahas" (str "id='" id "'"))
    (layout/render "teacher/pesan.html" {:pesan "Selesai Bahas Soal!"})))

(defroutes teacher-routes
  (GET "/teacher" []
       (teacher-home))
  (POST "/teacher-login" [id pass]
        (teacher-login id pass))
  (GET "/teacher-home" []
       (layout/render "teacher/work.html"))
  (GET "/teacher-ganti-pw" []
       (teacher-ganti-pw (session/get :id)))
  (POST "/teacher-ganti-pw" [pass1 pass2]
        (handle-teacher-ganti-pw (session/get :id) pass1 pass2))
  (GET "/teacher-logout" []
       (teacher-logout))

  (GET "/teacher-buat-proset" []
       (layout/render "teacher/buat-proset.html"))
  (POST "/teacher-buat-proset" [pel ket jsoal waktu jumpil]
        (handle-teacher-buat-proset pel ket jsoal waktu jumpil))

  (GET "/teacher-lihat-proset" []
       (teacher-lihat-proset (session/get :id)))
  (POST "/teacher-edit-proset" [kode]
        (teacher-edit-proset kode))
  (POST "/teacher-update-proset" [kode pel ket jsoal waktu jumpil skala nbenar nsalah acak status]
        (teacher-update-proset kode pel ket jsoal waktu jumpil skala nbenar nsalah acak status))

  (GET "/teacher-upload-file" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-upload-file"))
  (POST "/teacher-upload-file" [kode]
        (teacher-upload-file (subs kode 1 (count kode)) (session/get :id)))
  (POST "/teacher-upload" [kode file]
        (handle-teacher-upload (session/get :id) kode file))

  (GET "/teacher-buat-kunci" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-buat-kunci"))
  (POST "/teacher-buat-kunci" [kode]
        (teacher-buat-kunci (subs kode 1 (count kode))))


  (GET "/teacher-edit-kunci" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-edit-kunci"))
  (POST "/teacher-edit-kunci" [kode]
        (teacher-edit-kunci (subs kode 1 (count kode)) "/teacher-save-kunci"))
  (POST "/teacher-save-kunci" [kunci jenis upto pretext sound kode]
        (teacher-save-kunci kunci jenis upto (str "[" pretext "]") (str "[" sound "]") kode))

  (GET "/teacher-hasil-testL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-pilih-kelas"))
  (GET "/teacher-hasil-testB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-pilih-kelas"))
  (POST "/teacher-pilih-kelas" [kode]
        (teacher-pilih-kelas kode "/teacher-hasil-test"))
  (POST "/teacher-hasil-test" [kode kelas]
        (teacher-hasil-test kode kelas "teacher/hasil-test.html"))
  (POST "/teacher-test-detail-siswa" [nis kode]
        (teacher-test-detail-siswa nis kode))
  (POST "/teacher-lihat-soal" [nomer kode]
        (teacher-lihat-soal nomer kode))

  (GET "/teacher-absL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-abs"))
  (GET "/teacher-absB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-abs"))
  (POST "/teacher-abs" [kode]
        (teacher-abs kode "teacher/hasil-abs.html"))
  (GET "/teacher-abs-tkL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-abs-tk"))
  (GET "/teacher-abs-tkB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-abs-tk"))
  (POST "/teacher-abs-tk" [kode]
        (teacher-abs-tk kode "teacher/hasil-abs-tk.html"))
  (GET "/teacher-abs-dpL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-abs-dp"))
  (GET "/teacher-abs-dpB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-abs-dp"))
  (POST "/teacher-abs-dp" [kode]
        (teacher-abs-dp kode "teacher/hasil-abs-dp.html"))
  (GET "/teacher-dayakecohL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-dayakecoh"))
  (GET "/teacher-dayakecohB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-dayakecoh"))
  (POST "/teacher-dayakecoh" [kode]
        (teacher-dayakecoh kode "teacher/hasil-dayakecoh.html"))

  (GET "/teacher-hasil-test-excelL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-pilih-kelas-excel"))
  (GET "/teacher-hasil-test-excelB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-pilih-kelas-excel"))
  (POST "/teacher-pilih-kelas-excel" [kode]
        (teacher-pilih-kelas kode "/teacher-hasil-test-excel"))
  (POST "/teacher-hasil-test-excel" [kode kelas]
       (teacher-hasil-test kode kelas "teacher/hasil-test-excel.html"))

  (GET "/teacher-abs-excelL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-abs-excel"))
  (GET "/teacher-abs-excelB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-abs-excel"))
  (POST "/teacher-abs-excel" [kode]
        (teacher-abs kode "teacher/hasil-abs-excel.html"))
  (GET "/teacher-abs-tk-excelL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-abs-tk-excel"))
  (GET "/teacher-abs-tk-excelB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-abs-tk-excel"))
  (POST "/teacher-abs-tk-excel" [kode]
        (teacher-abs-tk kode "teacher/hasil-abs-tk-excel.html"))
  (GET "/teacher-abs-dp-excelL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-abs-dp-excel"))
  (GET "/teacher-abs-dp-excelB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-abs-dp-excel"))
  (POST "/teacher-abs-dp-excel" [kode]
        (teacher-abs-dp kode "teacher/hasil-abs-dp-excel.html"))

  (GET "/teacher-adk-excelL" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-adk-excel"))
  (GET "/teacher-adk-excelB" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-adk-excel"))
  (POST "/teacher-adk-excel" [kode]
        (teacher-dayakecoh kode "teacher/hasil-adk-excel.html"))

  (GET "/teacher-lihat-soal" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-view-soal"))
  (POST "/teacher-view-soal" [kode]
        (teacher-view-soal kode "teacher/view-soal.html"))

  (GET "/teacher-rekap-test" []
       (layout/render "teacher/write-subjek.html"))
  (POST "/teacher-rekap-test" [subjek]
        (teacher-arrange-rekap (session/get :id) subjek))
  (POST "/teacher-save-rekap" [subjek tests]
        (teacher-save-rekap subjek tests (session/get :id)))

  (GET "/teacher-edit-rekap" []
       (teacher-pilih-rekap (session/get :id) "/teacher-edit-rekap"))
  (POST "/teacher-edit-rekap" [kode]
        (teacher-edit-rekap (session/get :id) kode))
  (POST "/teacher-save-rekap-edit" [tests subjek kode]
        (teacher-save-rekap-edit tests subjek kode))

  (GET "/teacher-hasil-rekap" []
       (teacher-pilih-rekap (session/get :id) "/teacher-hasil-rekap"))
  (POST "/teacher-hasil-rekap" [kode]
        (teacher-hasil-rekap kode))

  (GET "/teacher-hapus-set" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-confirm-hapus"))
  (POST "/teacher-confirm-hapus" [kode]
        (teacher-confirm-hapus kode))
  (POST "/teacher-confirm-fback" [kode yn]
        (if (= yn "Y") (teacher-hapus-set kode) (layout/render "teacher/work.html")))
  (POST "/teacher-hapus-set" [kode]
        (teacher-hapus-set kode))

  (POST "/teacher-rekap-siswa" [nis kode]
        (teacher-rekap-siswa nis kode))

  (GET "/teacher-lihat-sekaligus" []
       (teacher-pilih-proset-sekaligus (session/get :id) "/teacher-lihat-sekaligus" "_blank"))
  (POST "/teacher-lihat-sekaligus" [kode]
        (teacher-lihat-sekaligus kode))

  (GET "/teacher-lihat-bp" []
       (teacher-search-proset-bp "/teacher-search-proset"))
  (POST "/teacher-search-proset" [nopel ket]
      (handle-teacher-search-proset nopel ket "/teacher-lihat-soal-bp"))
  (POST "/teacher-lihat-soal-bp" [nopel kode]
        (handle-teacher-lihat-soal-bp nopel kode))

  (GET "/teacher-catat-bp" []
      (layout/render "teacher/catat-bp.html"))
  (POST "/teacher-catat-bp" [kode]
      (handle-teacher-catat-bp kode))

  (GET "/teacher-lihat-catatan-bp" []
       (handle-teacher-lihat-catatan-bp (session/get :id) "/teacher-lihat-soal-bp-1"))
  (POST "/teacher-lihat-soal-bp-1" [kode]
      (handle-teacher-lihat-soal-bp-1 kode))

  (GET "/teacher-hapus-bp" []
       (handle-teacher-lihat-catatan-bp (session/get :id) "/teacher-hapus-bp-1"))
  (POST "/teacher-hapus-bp-1" [kode]
        (handle-teacher-hapus-bp-1 (subs kode 1 (count kode))))

  (GET "/teacher-bikin-soal-html" []
       (teacher-pilih-proset "L" (session/get :id) "/teacher-bikin-soal-html"))
  (POST "/teacher-bikin-soal-html" [kode]
        (handle-teacher-bikin-soal kode))
  (POST "/teacher-simpan-soal-html" [soal namafile kode]
        (handle-teacher-simpan-soal-html soal namafile kode (session/get :id)))

  (GET "/teacher-bahas-sss" []
       (teacher-pilih-proset-sekaligus (session/get :id) "/teacher-simpan-kode" ""))
  (POST "/teacher-simpan-kode" [kode]
        (teacher-simpan-kode kode "/teacher-bahas-set"))
  (POST "/teacher-bahas-set" [kode kodebahas]
        (teacher-view-soal kode "teacher/view-soal.html"))
  (GET "/teacher-selesai-bahas" []
       (teacher-selesai-bahas (session/get :id)))
  (GET "/teacher-bahas-cb" []
       (teacher-pilih-proset "B" (session/get :id) "/teacher-simpan-kodeB"))
  (POST "/teacher-simpan-kodeB" [kode]
        (teacher-simpan-kode kode "/teacher-bahas-setB"))
  (POST "/teacher-bahas-setB" [kode kodebahas]
        (teacher-view-soal kode "admin/view-soal.html"))
)

