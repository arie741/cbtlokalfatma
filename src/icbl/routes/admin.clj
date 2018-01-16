(ns icbl.routes.admin
  (:require [compojure.core :refer :all]
            [clojure.string :as st]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            [clojure.data.json :as json]
            [icbl.routes.teacher :as teacher]
            [icbl.models.share :as share]
            [dk.ative.docjure.spreadsheet :refer :all]
            [ring.util.response :refer [file-response]]
            )
  (:import (java.io File)))

(defn num-to-str [number dk]
  (-> (format (str "%." dk "f") (* number 1.0))
      (clojure.string/replace #"\." ",")))

(defn admin-home []
  (layout/render "admin/home.html")
  )

(defn handle-login [pass]
  (let [vpass (:pass (db/get-data (str "select pass from admin where id='admin'") 1))
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))]
    (if (= vpass pass)
        (do
          (session/put! :id "admin")
          (session/put! :status 1)
          (session/put! :ip ip)
          (layout/render "admin/work.html"))
        (layout/render "admin/home.html" {:error "Password Salah!"}))))

(defn handle-list-nama [nm]
  (let [upnm (clojure.string/upper-case nm)
        cdata (:jumlah (db/get-data (str "select count(*) as jumlah from users where upper(nama) LIKE '%" upnm "%'") 1))
        data (db/get-data (str "select nis,nama,kelas from users where upper(nama) LIKE '%" upnm "%'
                               order by nama LIMIT 15") 2)]
       (if data
         (layout/render "admin/list-siswa-nama.html"
                        {:data data :cdata cdata :urut "nama" :vnama upnm :page 0})
         (layout/render "admin/pesan.html" {:pesan "Tidak ada nama tersebut!"}))
    ))

(defn list-siswa-newpage [urut newpage vnama cdata]
  (let [data (db/get-data (str "select nis,nama,kelas from users where upper(nama)
                               LIKE '%" vnama "%' order by " urut " LIMIT 15 OFFSET "
                               (* (read-string newpage) 15)) 2)]
    (layout/render "admin/list-siswa-nama.html"
                   {:data data :cdata (read-string cdata)
                    :urut urut :vnama vnama :page (read-string newpage)})))

(defn handle-do-edit-siswa [nis]
  (let [datum (db/get-data (str "select * from users where nis='" nis "'") 1)
        ;daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)
        ]
    (layout/render "admin/edit-data-siswa.html"
                 {:datum datum
                  ;:daftarkelas daftarkelas
                  }
                   )))

(defn handle-update-data-siswa [nislama nisbaru nama kelas email pass passortu]
  (try (db/update-data-1 "users"
                              ["nis=?" nislama]
                                      {:nis nisbaru
                                       :nama nama
                                       :kelas kelas
                                       :email email
                                       :password pass
                                       :passortu passortu})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah data siswa!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data siswa!"}))))

(defn handle-delete-data-siswa [nis]
  (try (db/delete-data "users" (str "nis='" nis "'"))
       (layout/render "admin/pesan.html"
                      {:pesan (str "Berhasil menghapus data siswa dengan nis = " nis)})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal menghapus data siswa! error: " ex)}))))

(defn handle-ganti-pw-admin [pwlama pwbaru pwbaru1]
  (let [pwnow (:pass (db/get-data (str "select pass from admin where id='admin'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru) 5))
        (layout/render "admin/pesan.html" {:pesan "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru pwbaru1)
          (try (db/update-data-1 "admin" ["id=?" "admin"] {:pass pwbaru})
                 (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah password admin!"})
               (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))
          (layout/render "admin/pesan.html" {:pesan "Gagal mengubah data password admin!"})))))

(defn lihat-guru []
  (let [data (db/get-data (str "select * from teacher order by nama asc") 2)]
    (layout/render "admin/lihat-guru.html" {:data data})))

(defn handle-edit-guru [id]
  (let [datum (db/get-data (str "select * from teacher where id='" id "'") 1)]
    (layout/render "admin/edit-guru.html" {:datum datum})))

(defn handle-update-guru [id nama pass]
  (try
    (db/update-data "teacher" (str "id='" id "'")
       {:nama nama
        :pass pass
        })
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil Update Data Guru!")})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Update Data Guru error: " ex)}))))

(defn daftarkan-guru []
  (layout/render "admin/daftarkan-guru.html"))

(defn handle-daftarkan-guru [id nama]
  (do
    (io/create-path (str "resources/public/proset/" id) true)
    (try
      (db/insert-data "teacher" {:nama nama :id id :pass "abcde"})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan Bapak/Ibu " nama " dengan ID " id)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Daftarkan Guru error: " ex)})))))

(defn admin-pilih-guru [act]
  (let [data (db/get-data (str "select nama,id from teacher order by nama") 2)]
    (layout/render "admin/pilih-guru.html" {:action act :data data})))

(defn admin-set-ip []
  (let [ip (:ipnumber (db/get-data "select ipnumber from ip" 1))]
    (layout/render "admin/change-ip.html" {:ip ip})))

(defn admin-update-ip [ip]
  (try
      (db/update-data-1 "ip" ["no=?" 1] {:ipnumber ip})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil ubah IP Server menjadi " ip)})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal Ubah IP Server error: " ex)}))))

(defn logout []
  (do
   (session/clear!)
   (resp/redirect "/admin")))

(defn handle-admin-buat-proset [nopel ket jsoal waktu jumpil]
  (try
      (db/insert-data "bankproset"
                               {:id (session/get :id)
                                :kodepel (read-string nopel)
                                :keterangan ket
                                :jsoal (Integer/parseInt jsoal)
                                :waktu (Integer/parseInt waktu)
                                :kunci (apply str (repeat (Integer/parseInt jsoal) "-"))
                                :jenis (apply str (repeat (Integer/parseInt jsoal) "1"))
                                :upto (apply str (repeat (Integer/parseInt jsoal) "-"))
                                ;:pretext (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                ;:sound (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                :jumpil jumpil
                                :acak "0"
                                :status "0"
                                :skala 10
                                :nbenar 1
                                :nsalah 0
                                :kodex (share/create-kode 32)})
      (layout/render "admin/pesan.html" {:pesan (str "Berhasil daftarkan proset!")})
      (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal daftarkan proset! error: " ex)}))))

(defn handle-admin-search-proset [nopel ket act target]
  (let [Uket (clojure.string/upper-case ket)
        data (db/get-data (str "select kode,pelajaranbs.pelajaran as pelajaran,keterangan,jsoal,waktu,status from bankproset
                               inner join pelajaranbs on bankproset.kodepel=pelajaranbs.nomer where
                               kodepel='" nopel "' and upper(keterangan) LIKE '%" Uket "%'
                               order by keterangan") 2)
        ]
    (layout/render "admin/list-proset.html" {:data data :action act :kodepel nopel :ket ket
                                             :target target})))

(defn handle-admin-pilih-sekolah [kode act]
  (let [sekolah (db/get-data (str "select kode,nasek from sekolah order by kode") 2)]
    (layout/render "admin/view-sekolah.html" {:kodesoal kode :data sekolah :action act})))

(defn admin-pilih-kelas [kosek kodesoal act]
  (let [allkelas (db/get-data (str "select distinct kelas from dataus INNER JOIN users
                                 ON dataus.nis=users.nis where dataus.kode='" kodesoal "'
                                   and dataus.nis LIKE '" kosek "%'") 2)
        vkelas (conj (sort (map #(:kelas %) allkelas)) "SEMUA")]
    (layout/render "admin/pilih-kelas.html" {:kelas vkelas :kodesoal kodesoal :kosek kosek :action act})))

(defn admin-hasil-test [kodesoal kosek kelas html]
  (let [prekode (subs kodesoal 0 1)
        postkode (subs kodesoal 1 (count kodesoal))
        ;ckode (count kodesoal)
        ;tdata (if (= prekode "B") "bankproset" "proset")
        mdata (db/get-data (str "select kode,pelajaran,keterangan,jsoal from bankproset where kode='" postkode "'") 1)
;;         data (db/get-data (str "select dataus.nis as nis,nama,kelas,nilai,jawaban from dataus INNER JOIN " tdata "
;;                                ON " tdata ".kode=to_number(substring(dataus.kode,2," ckode "),'999999999')
;;                                INNER JOIN users ON users.nis=dataus.nis
;;                                where " tdata ".kode='" postkode "' order by nilai desc") 2)
        data (if (= kelas "SEMUA")
                 (db/get-data (str "select dataus.nis as nis,nama,kelas,nilai,jawaban from dataus INNER JOIN
                               users ON users.nis=dataus.nis WHERE dataus.kode='" kodesoal "' and
                                   dataus.nis LIKE '" kosek "%' order by nilai desc") 2)
                 (db/get-data (str "select dataus.nis as nis,nama,kelas,nilai,jawaban from dataus INNER JOIN
                               users ON users.nis=dataus.nis WHERE dataus.kode='" kodesoal "' and kelas='" kelas "'
                                   and dataus.nis LIKE '" kosek "%' order by nilai desc") 2))
        ;data1 (map #(num-to-str (:nilai %) 2) data)
        data1 (map #(update-in %1 [:nilai] num-to-str 2) data)
        kunci (:kunci (db/get-data (str "select kunci from bankproset where kode='" postkode "'") 1))]
    ;(println data2)
    (layout/render html {:data data1 :mdata mdata :kunci kunci :kode kodesoal})))

(defn admin-edit-proset [kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from bankproset where kode='" postkode "'") 1)]
    (layout/render "admin/edit-proset.html" {:datum datum :kode kode})))

(defn admin-update-proset [kode ket jsoal waktu jumpil skala nbenar nsalah acak status]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select kunci,jenis,upto,pretext,sound,kodepel,status,kodex from bankproset where kode='" postkode "'") 1)
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
        kodepel (datum :kodepel)
        kodex (datum :kodex)
        ]
  (try
    (db/update-data "bankproset" (str "kode='" postkode "'")
                    {:keterangan ket
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
        (.renameTo (File. (str "resources/public/bankproset/" kodepel "/" postkode))
                   (File. (str "resources/public/bankproset/" kodepel "/" kodex)))
        (.renameTo (File. (str "resources/public/bankproset/" kodepel "/" kodex))
                   (File. (str "resources/public/bankproset/" kodepel "/" postkode))))
      nil)
    (layout/render "admin/pesan.html" {:pesan (str "Berhasil update proset!")})
      )
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal update proset! error: " ex)})))))

(defn admin-upload-file [kode kodepel]
  (let [datum (db/get-data (str "select status,kodex from bankproset where kode='" kode "'") 1)
        status (datum :status)
        kodex (datum :kodex)]
    (if (= status "0")
      (do
        (io/create-path (str "resources/public/bankproset/" kodepel "/" kodex) true)
        (layout/render "admin/upload.html" {:kode kode :kodepel kodepel}))
      (layout/render "admin/pesan.html" {:pesan "Status Nol-kan dulu sebelum upload files!"}))))

(defn handle-admin-upload [kodepel kode file]
  (let [datum (db/get-data (str "select kodex from bankproset where kode='" kode "'") 1)
        kodex (datum :kodex)]
    (try
      (if (vector? file)
        (doseq [i file]
            (io/upload-file (str "resources/public/bankproset/" kodepel "/" kodex) i))
        (io/upload-file (str "resources/public/bankproset/" kodepel "/" kodex) file))
        (layout/render "admin/pesan.html" {:pesan "Berhasil upload file!"})
       (catch Exception ex
                    (layout/render "admin/pesan.html" {:pesan (str "Gagal upload file! error: " ex)}))
      )))


(defn admin-edit-kunci [kode act]
  (let [datum (db/get-data (str "select kunci,jsoal,jenis,upto,pretext,sound from bankproset where kode='" kode"'") 1)]
    (layout/render "teacher/edit-kunci.html" {:kunci (datum :kunci)
                                              :jsoal (datum :jsoal)
                                              :jenis (datum :jenis)
                                              :upto (datum :upto)
                                              :pretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                              :sound (if (datum :sound) (read-string (datum :sound)) nil)
                                              :kode kode
                                              :action act})))

(defn admin-save-kunci [kunci jenis upto pretext sound kode]
  (try
    (db/update-data "bankproset" (str "kode='" kode "'") {:kunci kunci :jenis jenis :upto upto :pretext pretext :sound sound})
    (layout/render "admin/pesan.html" {:pesan "Kunci berhasil disimpan!"})
    (catch Exception ex
                  (layout/render "admin/pesan.html" {:pesan (str "Gagal simpan kunci! error: " ex)}))))

(defn admin-view-soal [kodepel kode]
  (let [datum (db/get-data (str "select * from bankproset where kode='" kode "'") 1)
        status (datum :status)
        kodesoal (if (= status "0") (datum :kodex) kode)]
    (layout/render "admin/view-soal.html" {:datum datum
                                             :nsoal (vec (range 1 (inc (datum :jsoal))))
                                             ;:pel pel
                                             :kodesoal kodesoal
                                             :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                             :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                             ;:soalpath "http://127.0.0.1/resources/public"
                                             })))

(defn admin-lihat-sekaligus [kodepel kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from bankproset where kode='" postkode "'") 1)
        status (datum :status)
        kodesoal (if (= status "0") (datum :kodex) postkode)]
    (layout/render "admin/view-soal-sekaligus.html" {:datum datum
                                                       ;:kodepel kodepel
                                                       :kode kode
                                                       :kodesoal kodesoal
                                                       :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                                       :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                                       ;soalpath "http://localhost/resources/public"
                                                       })))

(defn admin-search-proset [act]
  (let [data (db/get-data "select * from pelajaranbs order by pelajaran" 2)]
    (layout/render "admin/search-proset.html" {:act act :data data})))

(defn admin-confirm-hapus [kode ket]
  (let [vkode (subs kode 1 (count kode))
        proset (db/get-data (str "select kode,keterangan from bankproset where kode='" vkode "'") 1)]
    (layout/render "admin/confirm-hapus.html" {:kode kode
                                               ;:pelajaran (proset :pelajaran)
                                               :keterangan (proset :keterangan)
                                               ;:pel pel
                                               :ket ket})))

(defn admin-hapus-set [ket kode]
  (try
    (db/delete-data "bankproset" (str "kode='" kode "'"))
    (layout/render "admin/pesan.html" {:pesan (str "Set Soal dengan kode B" kode " berhasil dihapus!" )})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal Hapus Proset! error " ex)}))
    ))

(defn handle-admin-tambah-kelas [kls]
  (let [Ukls (clojure.string/upper-case kls)
        data (db/get-data (str "select namakelas from kelas where UPPER(namakelas)='" Ukls "'") 1)]
        (if data
          (layout/render "admin/pesan.html" {:pesan (str "Kelas " Ukls " sudah ada!")})
          (try
            (db/insert-data "kelas" {:namakelas Ukls})
            (layout/render "admin/pesan.html" {:pesan (str "Berhasil menambah kelas dengan nama " Ukls)})
            (catch Exception ex
              (layout/render "admin/pesan.html" {:pesan "Gagal menambah kelas!"}))))))

(defn admin-view-kelas []
  (let [data (db/get-data "select * from kelas order by namakelas" 2)]
    (layout/render "admin/view-kelas.html" {:data data})))

(defn admin-edit-kelas [no]
  (let [datum (db/get-data (str "select nomer,namakelas from kelas where nomer='" no "'") 1)]
    (layout/render "admin/edit-kelas.html" {:datum datum})))

(defn admin-update-kelas [no nklas]
  (try
    (db/update-data "kelas" (str "nomer='" no "'") {:namakelas nklas})
    (layout/render "admin/pesan.html" {:pesan "Berhasil update nama kelas!"})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal update kelas! Error: " ex)}))))

(defn handle-admin-tambah-pelajaran [pel]
  (let [Upel (clojure.string/upper-case pel)
        data (db/get-data (str "select pelajaran from pelajaranbs where UPPER(pelajaran)='" Upel "'") 1)]
        (if data
          (layout/render "admin/pesan.html" {:pesan (str "Pelajaran " Upel " sudah ada!")})
          (try
            (db/insert-data "pelajaranbs" {:pelajaran Upel})
            (layout/render "admin/pesan.html" {:pesan (str "Berhasil menambah pelajaran dengan nama " Upel)})
            (catch Exception ex
              (layout/render "admin/pesan.html" {:pesan "Gagal menambah pelajaran!"}))))))

(defn admin-view-pelajaran []
  (let [data (db/get-data "select * from pelajaranbs order by pelajaran" 2)]
    (layout/render "admin/view-pelajaran.html" {:data data})))

(defn admin-edit-pelajaran [no]
  (let [datum (db/get-data (str "select nomer,pelajaran from pelajaranbs where nomer='" no "'") 1)]
    (layout/render "admin/edit-pelajaran.html" {:datum datum})))

(defn admin-update-pelajaran [no pel]
  (try
    (db/update-data "pelajaranbs" (str "nomer='" no "'") {:pelajaran pel})
    (layout/render "admin/pesan.html" {:pesan "Berhasil update pelajaran!"})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal update pelajaran! Error: " ex)}))))

(defn handle-input-siswa [file]
  (let [data (slurp (:tempfile file))
        ;sdata (st/split data #"\n")
        sdata (st/split (st/replace data #"\n" "") #"\r")
        ;vdata (map #(st/split % #",") (if (not (vector? sdata)) (st/split data #"\r") sdata))
        vdata (map #(st/split % #",") sdata)
        ;coba (spit (str vdata) "coba.txt")
        ]
        (loop [i 0]
          (if (= i (count vdata))
            (layout/render "admin/pesan.html" {:pesan "Menambah data siswa telah selesai!"})
            (do
              (let [nis_ada (db/get-data (str "select nis from users where nis='" (st/trimr ((nth vdata i) 0)) "'") 1)]
                (if (not nis_ada)
                  (db/insert-data "users" {:nis (st/trimr ((nth vdata i) 0))
                                           :nama ((nth vdata i) 1)
                                           :kelas ((nth vdata i) 2)
                                           ;:email ((nth vdata i) 3)
                                           ;:NPSN ((nth vdata i) 4)
                                           ;:password (if ((nth vdata i) 3) ((nth vdata i) 3) "12345")
                                           :password "12345"
                                           :passortu "abcde"
                                           })))
              (recur (inc i)))))))

(defn admin-delete-siswa [act]
  (let [data (db/get-data "select nis,nama,kelas from users order by kelas,nis" 2)]
    (layout/render "admin/list-all-siswa.html" {:data data :action act :judul "HAPUS SISWA" :ket "menghapus"})))
(defn handle-hapus-siswa [nis]
  (do
    (db/delete-data "users" (str "nis='" nis "'"))
    (admin-delete-siswa "/admin-delete-siswa")))

(defn admin-hapus-guru [act]
  (let [data (db/get-data "select id,nama from teacher order by nama" 2)]
    (layout/render "admin/list-all-guru.html" {:data data :action act :judul "HAPUS GURU" :ket "menghapus"})))
(defn handle-hapus-guru [id]
  (do
    (db/delete-data "teacher" (str "id='" id "'"))
    (admin-hapus-guru "/admin-hapus-guru")))

(defn admin-tambah-sekolah [kode sekolah npsn]
  (try
     (db/insert-data "sekolah" {:kode kode :nasek sekolah :npsn npsn})
     (layout/render "admin/pesan.html" {:pesan (str "Berhasil menambah sekolah dengan nama " sekolah)})
     (catch Exception ex
     (layout/render "admin/pesan.html" {:pesan "Gagal menambah Sekolah!"}))))

(defn admin-view-sekolah []
  (let [data (db/get-data "select * from sekolah order by nasek" 2)]
    (layout/render "admin/view-sekolah-edit.html" {:data data})))

(defn admin-edit-sekolah [kode]
  (let [data (db/get-data (str "select * from sekolah where kode='" kode "'") 1)]
    (layout/render "admin/edit-sekolah.html" {:datum data})))

(defn admin-update-sekolah [kode nasek npsn]
  (try
    (db/update-data "sekolah" (str "kode='" kode "'") {:nasek nasek :npsn npsn})
    (layout/render "admin/pesan.html" {:pesan "Berhasil Update Sekolah!"})
    (catch Exception ex
     (layout/render "admin/pesan.html" {:pesan "Gagal Update Sekolah!"}))))

(defn admin-registrasi-siswa [nis nama kelas email]
  (let [data (db/get-data (str "select nis from users where nis='" nis "'") 1)]
    (if data
        (layout/render "admin/registrasi-siswa.html"
                       {:nis nis
                        :namaku nama
                        :kelas kelas
                        :email email
                        :error "NIS tersebut sudah terdaftar!"})
        (try
          (db/insert-data "users" {:nis nis
                                   :nama nama
                                   :kelas kelas
                                   :email email
                                   :password "12345"
                                   :passortu "abcde"})
          (layout/render "admin/pesan.html" {:pesan "Berhasil registrasi siswa!"})
          (catch Exception ex
             (layout/render "admin/pesan.html" {:pesan "Gagal registrasi siswa!"}))))))

(defn admin-tambah-paket-ppdb [ket mat ipa ind ing]
  (try
    (db/insert-data "simppdb" {:keterangan ket
                               :kodemat mat
                               :kodeipa ipa
                               :kodeind ind
                               :kodeing ing
                                })
    (layout/render "admin/pesan.html" {:pesan "Berhasil menambah paket ppdb!"})
          (catch Exception ex
             (layout/render "admin/pesan.html" {:pesan (str "Gagal menambah paket ppdb! error" ex)}))))

(defn admin-list-paket-ppdb []
  (let [data (db/get-data "select * from simppdb order by keterangan" 2)]
       (layout/render "admin/list-paket-ppdb.html" {:data data})))

(defn admin-edit-paket-ppdb [kode]
  (let [datum (db/get-data (str "select * from simppdb where kode='" kode "'") 1)]
    (layout/render "admin/edit-paket-ppdb.html" {:datum datum})))

(defn admin-update-paket-ppdb [kode ket mat ipa ind ing]
  (try (db/update-data-1 "simppdb"
                              ["kode=?" (read-string kode)]
                                      {:keterangan ket
                                       :kodemat mat
                                       :kodeipa ipa
                                       :kodeind ind
                                       :kodeing ing})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah paket PPDB!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan (str "Gagal mengubah paket PPDB! error:" ex)}))))

(defn admin-delete-paket-ppdb [kode]
  (try (db/delete-data "simppdb" (str "kode='" kode "'"))
       (layout/render "admin/pesan.html"
                      {:pesan (str "Berhasil menghapus paket PPDB!")})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal menghapus paket PPDB! error: " ex)}))))

(defn admin-tambah-sma-ppdb []
  (let [daerah (db/get-data "select * from kodedaerah order by daerah asc" 2)]
       (layout/render "admin/tambah-sma-ppdb.html" {:daerah daerah})))

(defn admin-tambah-sma-ppdb1 [kode sek nm]
  (try
    (db/insert-data "pgsma" {:kodedaerah (read-string kode)
                             :sekolah sek
                             :nm (read-string nm)})
    (layout/render "admin/pesan.html" {:pesan "Berhasil menambah data SMA!"})
          (catch Exception ex
             (layout/render "admin/pesan.html" {:pesan (str "Gagal menambah data SMA! error" ex)}))))

(defn admin-list-sma-ppdb []
  (let [data (db/get-data "select * from pgsma order by kodedaerah,sekolah" 2)]
       (layout/render "admin/list-sma-ppdb.html" {:data data})))

(defn admin-edit-sma-ppdb [sekolah]
  (let [daerah (db/get-data "select * from kodedaerah order by daerah" 2)
        datasek (db/get-data (str "select * from pgsma where sekolah='" sekolah "'") 1)]
    (layout/render "admin/edit-sma-ppdb.html" {:daerah daerah :datasek datasek})))

(defn admin-update-sma-ppdb [sekolah kode nm]
  (try (db/update-data-1 "pgsma"
                              ["sekolah=?" sekolah]
                                      {:kodedaerah (read-string kode)
                                       :nm (read-string nm)})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah data SMA!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan (str "Gagal mengubah data SMA! error:" ex)}))))

(defn admin-delete-sma-ppdb [sekolah]
  (try (db/delete-data "pgsma" (str "sekolah='" sekolah "'"))
       (layout/render "admin/pesan.html"
                      {:pesan (str "Berhasil menghapus data SMA!")})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal menghapus data SMA! error: " ex)}))))

(defn admin-tambah-daerah [daerah]
  (try
    (db/insert-data "kodedaerah" {:daerah daerah})
    (layout/render "admin/pesan.html" {:pesan "Berhasil menambah data daerah!"})
          (catch Exception ex
             (layout/render "admin/pesan.html" {:pesan (str "Gagal menambah data daerah! error" ex)}))))

(defn admin-list-daerah-ppdb []
  (let [data (db/get-data "select * from kodedaerah order by daerah" 2)]
    (layout/render "admin/list-daerah.html" {:data data})))

(defn admin-edit-daerah-ppdb [kode]
  (let [datum (db/get-data (str "select * from kodedaerah where kode='" kode "'") 1)]
    (layout/render "admin/edit-daerah-ppdb.html" {:datum datum})))

(defn admin-update-daerah-ppdb [kode daerah]
  (try (db/update-data-1 "kodedaerah"
                              ["kode=?" (read-string kode)]
                                      {:daerah daerah})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah nama daerah!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan (str "Gagal mengubah nama daerah! error:" ex)}))))

(defn admin-tambah-paket-sbmptn [ket tkpa jurusan kelompok]
  (try
    (db/insert-data "simsbmptn" {:keterangan ket
                                 :kodetkpa tkpa
                                 :kodeipaorips jurusan
                                 :kelompok kelompok
                                })
    (layout/render "admin/pesan.html" {:pesan "Berhasil menambah paket SBMPTN!"})
          (catch Exception ex
             (layout/render "admin/pesan.html" {:pesan (str "Gagal menambah paket SBMPTN! error" ex)}))))

(defn admin-list-paket-sbmptn []
  (let [data (db/get-data "select * from simsbmptn order by keterangan" 2)]
       (layout/render "admin/list-paket-sbmptn.html" {:data data})))

(defn admin-edit-paket-sbmptn [kode]
  (let [datum (db/get-data (str "select * from simsbmptn where kode='" kode "'") 1)]
    (layout/render "admin/edit-paket-sbmptn.html" {:datum datum})))

(defn admin-update-paket-sbmptn [kode ket tkpa ipaorips kelompok]
  (try (db/update-data-1 "simsbmptn"
                              ["kode=?" (read-string kode)]
                                      {:keterangan ket
                                       :kodetkpa tkpa
                                       :kodeipaorips ipaorips
                                       :kelompok kelompok})
               (layout/render "admin/pesan.html" {:pesan "Berhasil mengubah paket SBMPTN!"})
               (catch Exception ex
                (layout/render "admin/pesan.html" {:pesan (str "Gagal mengubah paket SBMPTN! error:" ex)}))))

(defn admin-delete-paket-sbmptn [kode]
  (try (db/delete-data "simsbmptn" (str "kode='" kode "'"))
       (layout/render "admin/pesan.html"
                      {:pesan (str "Berhasil menghapus paket SBMPTN!")})
    (catch Exception ex
      (layout/render "admin/pesan.html" {:pesan (str "Gagal menghapus paket SBMPTN! error: " ex)}))))

(defn admin-hitung-ulang-hasil [kode]
  (try
    (share/hitung-ulang kode)
    (layout/render "admin/pesan.html" {:pesan "Selesai hitung ulang hasil test!"})
    (catch Exception ex
          (layout/render "admin/pesan.html" {:pesan (str "Gagal hitung ulang hasil! error:" ex)}))))

(defn admin-ppdb-sekolah [act]
  (let [data (db/get-data  "select * from simppdb order by keterangan" 2)]
    (layout/render "admin/view-paket-ppdb.html" {:data data :action act})))

(defn admin-pilih-sekolah-ppdb [kodepaket act]
  (let [data (db/get-data "select kode,nasek from sekolah order by nasek" 2)]
    (layout/render "admin/view-sekolah-ppdb.html" {:data data :kodepaket kodepaket :action act})))

(defn admin-pilih-kelas-ppdb [kosek kodepaket act]
  (let [vkode (db/get-data (str "select kodemat,kodeipa,kodeind,kodeing from simppdb where kode='" kodepaket "'") 1)
        kmat (:kodemat vkode)
        kipa (:kodeipa vkode)
        kind (:kodeind vkode)
        king (:kodeing vkode)
        allkelas (db/get-data (str "select distinct kelas from dataus INNER JOIN users
                                 ON dataus.nis=users.nis where (dataus.kode='" kmat "'
                                   or dataus.kode='" kipa "' or
                                   dataus.kode='" kind "'
                                   or dataus.kode='" king "')
                                   and dataus.nis LIKE '" kosek "%'") 2)
        vkelas (conj (sort (map #(:kelas %) allkelas)) "SEMUA")]
    (layout/render "admin/pilih-kelas-ppdb.html" {:kelas vkelas :kodepaket kodepaket :kosek kosek :action act})))

(defn admin-jbenar [sa sb]
  (let [seq-sa (seq sa)
        seq-sb (seq sb)]
    (reduce + (map #(if (= %1 %2) 1 0) seq-sa seq-sb))))

(defn admin-jkosong [sa]
  (count (filter #(= \- %) (seq sa))))

(defn admin-jsalah [sa sb]
  (reduce + (map #(if (and (not= \- %1) (not= %1 %2)) 1 0) (seq sa) (seq sb))))

(defn admin-proses-laporan-ppdb [kosek kodepaket kelas mode]
  (let [sekolah (:nasek (db/get-data (str "select nasek from sekolah where kode='" kosek "'") 1))
        ckosek (count kosek)
        paket (:keterangan (db/get-data (str "select keterangan from simppdb where kode='" kodepaket "'") 1))
        vkodepaket (db/get-data (str "select kodemat,kodeipa,kodeind,kodeing from simppdb where kode='" kodepaket "'") 1)
        kodemat (:kodemat vkodepaket)
        kodeipa (:kodeipa vkodepaket)
        kodeind (:kodeind vkodepaket)
        kodeing (:kodeing vkodepaket)
        skodemat (subs kodemat 1 (count kodemat))
        skodeipa (subs kodeipa 1 (count kodeipa))
        skodeind (subs kodeind 1 (count kodeind))
        skodeing (subs kodeing 1 (count kodeing))
        kuncimat (:kunci (db/get-data (str "select kunci from bankproset where kode='" skodemat "'") 1))
        kunciipa (:kunci (db/get-data (str "select kunci from bankproset where kode='" skodeipa "'") 1))
        kunciind (:kunci (db/get-data (str "select kunci from bankproset where kode='" skodeind "'") 1))
        kunciing (:kunci (db/get-data (str "select kunci from bankproset where kode='" skodeing "'") 1))
        vsiswa (if (= kelas "SEMUA")
                   (db/get-data (str "select distinct dataus.nis as nis,nama,kelas from dataus
                                 inner join users on dataus.nis=users.nis where
                                 (kode='" kodemat "' or kode='" kodeipa "' or
                                 kode='" kodeind "' or kode='" kodeing "')
                                 and dataus.nis like '" kosek "%'" ) 2)
                   (db/get-data (str "select distinct dataus.nis as nis,nama,kelas from dataus
                                 inner join users on dataus.nis=users.nis where
                                 (kode='" kodemat "' or kode='" kodeipa "' or
                                 kode='" kodeind "' or kode='" kodeing "')
                                 and dataus.nis like '" kosek "%' and users.kelas='" kelas "'") 2)
                 )
        csis (count vsiswa)
        vnmat (if (= kelas "SEMUA")
              (db/get-data (str "select nis,jawaban,nilai from dataus where kode='" kodemat "'
                                and nis like '" kosek "%'") 2)
              (db/get-data (str "select dataus.nis as nis,jawaban,nilai from dataus inner join users
                                on dataus.nis=users.nis where dataus.kode='" kodemat "'
                                and dataus.nis like '" kosek "%' and users.kelas='" kelas "'") 2))
        vnipa (if (= kelas "SEMUA")
              (db/get-data (str "select nis,jawaban,nilai from dataus where kode='" kodeipa "'
                                and nis like '" kosek "%'") 2)
              (db/get-data (str "select dataus.nis as nis,jawaban,nilai from dataus inner join users
                                on dataus.nis=users.nis where kode='" kodeipa "'
                                and dataus.nis like '" kosek "%' and users.kelas='" kelas "'") 2))
        vnind (if (= kelas "SEMUA")
              (db/get-data (str "select nis,jawaban,nilai from dataus where kode='" kodeind "'
                                and nis like '" kosek "%'") 2)
              (db/get-data (str "select dataus.nis as nis,jawaban,nilai from dataus inner join users
                                on dataus.nis=users.nis where kode='" kodeind "'
                                and dataus.nis like '" kosek "%' and users.kelas='" kelas "'") 2))
        vning (if (= kelas "SEMUA")
              (db/get-data (str "select nis,jawaban,nilai from dataus where kode='" kodeing "'
                                and nis like '" kosek "%'") 2)
              (db/get-data (str "select dataus.nis as nis,jawaban,nilai from dataus inner join users
                                on dataus.nis=users.nis where kode='" kodeing "'
                                and dataus.nis like '" kosek "%' and users.kelas='" kelas "'") 2))
        vnmat1 (map (fn [x] {:nis (:nis x)
                             :nilai (:nilai x)
                             :B (admin-jbenar (:jawaban x) kuncimat)
                             :S (admin-jsalah (:jawaban x) kuncimat)
                             :K (admin-jkosong (:jawaban x))}) vnmat)
        vnipa1 (map (fn [x] {:nis (:nis x)
                             :nilai (:nilai x)
                             :B (admin-jbenar (:jawaban x) kunciipa)
                             :S (admin-jsalah (:jawaban x) kunciipa)
                             :K (admin-jkosong (:jawaban x))}) vnipa)
        vnind1 (map (fn [x] {:nis (:nis x)
                             :nilai (:nilai x)
                             :B (admin-jbenar (:jawaban x) kunciind)
                             :S (admin-jsalah (:jawaban x) kunciind)
                             :K (admin-jkosong (:jawaban x))}) vnind)
        vning1 (map (fn [x] {:nis (:nis x)
                             :nilai (:nilai x)
                             :B (admin-jbenar (:jawaban x) kunciing)
                             :S (admin-jsalah (:jawaban x) kunciing)
                             :K (admin-jkosong (:jawaban x))}) vning)

        daftar (loop [a [] i 0]
                 (if (= i csis)
                   a
                   (let [m-sis (nth vsiswa i)
                         m-mat (first (filter #(= (:nis m-sis) (:nis %)) vnmat1))
                         m-ipa (first (filter #(= (:nis m-sis) (:nis %)) vnipa1))
                         m-ind (first (filter #(= (:nis m-sis) (:nis %)) vnind1))
                         m-ing (first (filter #(= (:nis m-sis) (:nis %)) vning1))
                         ntot (+ (if m-mat (:nilai m-mat) 0)
                                 (if m-ipa (:nilai m-ipa) 0)
                                 (if m-ind (:nilai m-ind) 0)
                                 (if m-ing (:nilai m-ing) 0))
                         nis (:nis m-sis)
                         nama (:nama m-sis)
                         kelas (:kelas m-sis)
                         nmat (if m-mat (:nilai m-mat) " ")
                         Bmat (if m-mat (:B m-mat) " ")
                         Smat (if m-mat (:S m-mat) " ")
                         Kmat (if m-mat (:K m-mat) " ")
                         nipa (if m-ipa (:nilai m-ipa) " ")
                         Bipa (if m-ipa (:B m-ipa) " ")
                         Sipa (if m-ipa (:S m-ipa) " ")
                         Kipa (if m-ipa (:K m-ipa) " ")
                         nind (if m-ind (:nilai m-ind) " ")
                         Bind (if m-ind (:B m-ind) " ")
                         Sind (if m-ind (:S m-ind) " ")
                         Kind (if m-ind (:K m-ind) " ")
                         ning (if m-ing (:nilai m-ing) " ")
                         Bing (if m-ing (:B m-ing) " ")
                         Sing (if m-ing (:S m-ing) " ")
                         King (if m-ing (:K m-ing) " ")
                         ]
                     (recur (conj a {:nis (subs nis ckosek (count nis))
                                     :nama nama :ntot ntot :kelas kelas
                                     :nmat (if (= nmat " ") " " (num-to-str nmat 2)) :Bmat Bmat :Smat Smat :Kmat Kmat
                                     :nipa (if (= nipa " ") " " (num-to-str nipa 2)) :Bipa Bipa :Sipa Sipa :Kipa Kipa
                                     :nind (if (= nind " ") " " (num-to-str nind 2)) :Bind Bind :Sind Sind :Kind Kind
                                     :ning (if (= ning " ") " " (num-to-str ning 2)) :Bing Bing :Sing Sing :King King})
                            (inc i)))))
        daftar1 (reverse (sort-by :ntot daftar))
        daftar2 (map #(update-in %1 [:ntot] num-to-str 2) daftar1)
        ]
    (if (= mode 1)
    (layout/render "admin/hasil-test-ppdb.html" {:data (vec daftar2)
                                                 :paket paket
                                                 :sekolah sekolah})
    (let [datax (vec daftar2)
          vdata (map (fn [x] [(:nis x)
                              (:nama x)
                              (:kelas x)
                              (:Bmat x) (:Smat x) (:Kmat x) (:nmat x)
                              (:Bipa x) (:Sipa x) (:Kipa x) (:nipa x)
                              (:Bind x) (:Sind x) (:Kind x) (:nind x)
                              (:Bing x) (:Sing x) (:King x) (:ning x)
                              (:ntot x)]) datax)
          header [["NIS","NAMA","KELAS","MATEMATIKA","","","","SAINS","","","","INDONESIA","","","","INGGRIS","","","","TOTAL"]
                  ["" "" "" "B" "S" "K" "NIL" "B" "S" "K" "NIL" "B" "S" "K" "NIL" "B" "S" "K" "NIL" ""]
                   ]
          dataxcel (vec (concat header vdata))
          vkls (st/replace kelas #" " "")
          filename (str kosek "-" kodepaket "-" vkls ".xlsx")
          wb (create-workbook vkls dataxcel)]
      (save-workbook! (str "dokumen/" filename) wb)
      ;(layout/render "admin/pesan.html" {:pesan "coba lihat di dokumen/coba1.xls"})
      (resp/redirect (str "/files/" filename))
      )
    )))

;;;routes
(defroutes admin-routes

  (GET "/files/:filename" [filename]
       (file-response (str "dokumen/" filename)))

  (GET "/admin" []
      (admin-home))

  (GET "/admin-home" []
       (layout/render "admin/work.html"))

  (GET "/admin-logout" []
       (logout))

  (POST "/admin-login" [pass]
      (handle-login pass))

  (GET "/admin-registrasi-siswa" []
       (layout/render "admin/registrasi-siswa.html"))
  (POST "/admin-registrasi-siswa" [nis nama kelas email]
        (admin-registrasi-siswa nis nama kelas email))

  (GET "/edit-siswa" []
       (layout/render "admin/search-siswa.html"))
  (POST "/list-siswa-newpage" [urut newpage vnama cdata]
        (list-siswa-newpage urut newpage vnama cdata))
  (POST "/edit-siswa" [nama]
        (handle-list-nama nama))
  (POST "/do-edit-siswa" [nis]
        (handle-do-edit-siswa nis))
  (POST "/update-data-siswa" [nislama nisbaru nama kelas email pass passortu]
        (handle-update-data-siswa nislama nisbaru nama kelas email pass passortu))
  (POST "/delete-data-siswa" [nislama]
        (handle-delete-data-siswa nislama))
  (GET "/admin-delete-siswa" []
       (admin-delete-siswa "/admin-delete-siswa"))
  (POST "/admin-delete-siswa" [nis]
        (handle-hapus-siswa nis))

  (GET "/admin-tambah-kelas" []
       (layout/render "admin/tambah-kelas.html"))
  (POST "/admin-tambah-kelas" [kelas]
        (handle-admin-tambah-kelas kelas))

  (GET "/admin-edit-kelas" []
       (admin-view-kelas))
  (POST "/admin-edit-kelas" [nomer]
      (admin-edit-kelas nomer))
  (POST "/admin-update-kelas" [nomer namakelas]
      (admin-update-kelas nomer namakelas))

  (GET "/admin-tambah-pelajaran" []
       (layout/render "admin/tambah-pelajaran.html"))
  (POST "/admin-tambah-pelajaran" [pelajaran]
        (handle-admin-tambah-pelajaran pelajaran))

  (GET "/admin-edit-pelajaran" []
       (admin-view-pelajaran))
  (POST "/admin-edit-pelajaran" [nomer]
      (admin-edit-pelajaran nomer))
  (POST "/admin-update-pelajaran" [nomer pelajaran]
      (admin-update-pelajaran nomer pelajaran))

  (GET "/ganti-pw-admin" []
       (layout/render "admin/ganti-pw-admin.html"))
  (POST "/ganti-pw-admin" [pwlama pwbaru pwbaru1]
        (handle-ganti-pw-admin pwlama pwbaru pwbaru1))

  (GET "/lihat-guru" []
       (lihat-guru))
  (POST "/edit-guru" [id]
        (handle-edit-guru id))
  (POST "/update-guru" [id nama pass]
        (handle-update-guru id nama pass))
  (GET "/admin-hapus-guru" []
       (admin-hapus-guru "/admin-hapus-guru"))
  (POST "/admin-hapus-guru" [id]
        (handle-hapus-guru id))

  (GET "/daftarkan-guru" []
       (daftarkan-guru))
  (POST "/daftarkan-guru" [id nama]
        (handle-daftarkan-guru id nama))

  (GET "/admin-hasil-testL" []
       (admin-pilih-guru "/admin-pilih-proset"))
  (POST "/admin-pilih-proset" [id]
        (teacher/teacher-pilih-proset "L" id "/teacher-pilih-kelas"))

  (GET "/admin-hasil-testB" []
       (admin-search-proset "/admin-hasil-test-search"))
  (POST "/admin-hasil-test-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-pilih-sekolahB" ""))
  (POST "/admin-pilih-sekolahB" [kode]
       (handle-admin-pilih-sekolah kode "/admin-pilih-kelasB"))
  (POST "/admin-pilih-kelasB" [kosek kodesoal]
        (if (= kosek "SEMUA")
          (admin-hasil-test kodesoal "" "SEMUA" "admin/hasil-test.html")
          (admin-pilih-kelas kosek kodesoal "/admin-hasil-testB")))
  (POST "/admin-hasil-testB" [kodesoal kosek kelas]
         (admin-hasil-test kodesoal kosek kelas "admin/hasil-test.html"))

  (GET "/admin-paket-ppdb" []
       (admin-ppdb-sekolah "/admin-pilih-sekolah-ppdb"))
  (POST "/admin-pilih-sekolah-ppdb" [kode]
        (admin-pilih-sekolah-ppdb kode "/admin-pilih-kelas-ppdb"))
  (POST "/admin-pilih-kelas-ppdb" [kosek kodepaket]
        (admin-pilih-kelas-ppdb kosek kodepaket "/admin-proses-laporan-ppdb"))
  (POST "/admin-proses-laporan-ppdb" [kosek kodepaket kelas]
        (admin-proses-laporan-ppdb kosek kodepaket kelas 1))

  ;;Analisis Butir Soal
  (GET "/admin-abs" []
        (admin-pilih-guru "/admin-pilih-proset-absbsk"))
  (POST "/admin-pilih-proset-absbsk" [id]
        (teacher/teacher-pilih-proset "L" id "/teacher-abs"))

  (GET "/admin-abs-tk" []
       (admin-pilih-guru "/admin-pilih-proset-abstk"))
  (POST "/admin-pilih-proset-abstk" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-tk"))

  (GET "/admin-abs-dp" []
       (admin-pilih-guru "/admin-pilih-proset-absdp"))
  (POST "/admin-pilih-proset-absdp" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-dp"))

  (GET "/admin-dayakecoh" []
       (admin-pilih-guru "/admin-pilih-proset-absdk"))
   (POST "/admin-pilih-proset-absdk" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-dayakecoh"))

  (GET "/admin-absB" []
       (admin-search-proset "/admin-absB-search"))
  (POST "/admin-absB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-absB" ""))
  (POST "/admin-absB" [kode]
        (teacher/teacher-abs kode "teacher/hasil-abs.html"))

  (GET "/admin-abs-tkB" []
       (admin-search-proset "/admin-abs-tkB-search"))
  (POST "/admin-abs-tkB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-abs-tkB" ""))
  (POST "/admin-abs-tkB" [kode]
        (teacher/teacher-abs-tk kode "teacher/hasil-abs-tk.html"))

  (GET "/admin-abs-dpB" []
       (admin-search-proset "/admin-abs-dpB-search"))
  (POST "/admin-abs-dpB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-abs-dpB" ""))
  (POST "/admin-abs-dpB" [kode]
        (teacher/teacher-abs-dp kode "teacher/hasil-abs-dp.html"))

  (GET "/admin-dayakecohB" []
       (admin-search-proset "/admin-dayakecohB-search"))
  (POST "/admin-dayakecohB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-dayakecohB" ""))
  (POST "/admin-dayakecohB" [kode]
        (teacher/teacher-dayakecoh kode "teacher/hasil-dayakecoh.html"))

  ;;Simpan ke Excel

  (GET "/admin-hasil-test-excel" []
       (admin-pilih-guru "/admin-pilih-proset-excel"))
  (POST "/admin-pilih-proset-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-pilih-kelas-excel"))

  (GET "/admin-abs-excel" []
        (admin-pilih-guru "/admin-pilih-proset-abs-excel"))
   (POST "/admin-pilih-proset-abs-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-excel"))

  (GET "/admin-abs-tk-excel" []
        (admin-pilih-guru "/admin-pilih-proset-abstk-excel"))
   (POST "/admin-pilih-proset-abstk-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-tk-excel"))

  (GET "/admin-abs-dp-excel" []
        (admin-pilih-guru "/admin-pilih-proset-absdp-excel"))
   (POST "/admin-pilih-proset-absdp-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-abs-dp-excel"))

  (GET "/admin-adk-excel" []
        (admin-pilih-guru "/admin-pilih-proset-adk-excel"))
   (POST "/admin-pilih-proset-adk-excel" [id]
       (teacher/teacher-pilih-proset "L" id "/teacher-adk-excel"))

  (GET "/admin-hasil-test-excelB" []
       (admin-search-proset "/admin-hasil-test-excelB-search"))
  (POST "/admin-hasil-test-excelB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-pilih-sekolah-excelB" ""))
  (POST "/admin-pilih-sekolah-excelB" [kode]
       (handle-admin-pilih-sekolah kode "/admin-pilih-kelas-excelB"))
  (POST "/admin-pilih-kelas-excelB" [kosek kodesoal]
        (if (= kosek "SEMUA")
          (admin-hasil-test kodesoal "" "SEMUA" "teacher/hasil-test-excel.html")
          (admin-pilih-kelas kosek kodesoal "/admin-hasil-test-excelB")))
  (POST "/admin-hasil-test-excelB" [kodesoal kosek kelas]
        (admin-hasil-test kodesoal kosek kelas "teacher/hasil-test-excel.html"))

  (GET "/admin-abs-excelB" []
       (admin-search-proset "/admin-abs-excelB-search"))
  (POST "/admin-abs-excelB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-abs-excelB" ""))
  (POST "/admin-abs-excelB" [kode]
        (teacher/teacher-abs kode "teacher/hasil-abs-excel.html"))

  (GET "/admin-abs-tk-excelB" []
       (admin-search-proset "/admin-abs-tk-excelB-search"))
  (POST "/admin-abs-tk-excelB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-abs-tk-excelB" ""))
  (POST "/admin-abs-tk-excelB" [kode]
        (teacher/teacher-abs-tk kode "teacher/hasil-abs-tk-excel.html"))

  (GET "/admin-abs-dp-excelB" []
       (admin-search-proset "/admin-abs-dp-excelB-search"))
  (POST "/admin-abs-dp-excelB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-abs-dp-excelB" ""))
  (POST "/admin-abs-dp-excelB" [kode]
        (teacher/teacher-abs-dp kode "teacher/hasil-abs-dp-excel.html"))

  (GET "/admin-adk-excelB" []
       (admin-search-proset "/admin-adk-excelB-search"))
  (POST "/admin-adk-excelB-search" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-adk-excelB" ""))
  (POST "/admin-adk-excelB" [kode]
        (teacher/teacher-dayakecoh kode "teacher/hasil-adk-excel.html"))

  (GET "/admin-paket-ppdb-excell" []
       (admin-ppdb-sekolah "/admin-pilih-sekolah-ppdb-excell"))
  (POST "/admin-pilih-sekolah-ppdb-excell" [kode]
        (admin-pilih-sekolah-ppdb kode "/admin-pilih-kelas-ppdb-excell"))
  (POST "/admin-pilih-kelas-ppdb-excell" [kosek kodepaket]
        (admin-pilih-kelas-ppdb kosek kodepaket "/admin-proses-laporan-ppdb-excell"))
  (POST "/admin-proses-laporan-ppdb-excell" [kosek kodepaket kelas]
        (admin-proses-laporan-ppdb kosek kodepaket kelas 2))

  (GET "/admin-set-ip" []
       (admin-set-ip))
  (POST "/admin-change-ip" [ipnumber]
        (admin-update-ip ipnumber))

  (GET "/admin-buat-proset" []
       (let [data (db/get-data  "select nomer,pelajaran from pelajaranbs order by pelajaran" 2)]
         (layout/render "admin/buat-proset.html" {:data data})))
  (POST "/admin-buat-proset" [nopel ket jsoal waktu jumpil]
        (handle-admin-buat-proset nopel ket jsoal waktu jumpil))

  (GET "/admin-search-proset" []
       (admin-search-proset "/admin-search-proset1"))
  (POST "/admin-search-proset1" [nopel ket]
        (handle-admin-search-proset nopel ket "/admin-edit-proset" ""))
  (POST "/admin-edit-proset" [kode]
        (admin-edit-proset kode))
  (POST "/admin-update-proset" [kode ket jsoal waktu jumpil skala nbenar nsalah acak status]
         (admin-update-proset kode ket jsoal waktu jumpil skala nbenar nsalah acak status))

  (GET "/admin-upload-file" []
       (admin-search-proset "/admin-pilih-proset1"))
  (POST "/admin-pilih-proset1" [nopel ket]
       (handle-admin-search-proset nopel ket "/admin-upload-file1" ""))
  (POST "/admin-upload-file1" [kode kodepel]
        (admin-upload-file (subs kode 1 (count kode)) kodepel))
  (POST "/admin-upload" [kodepel kode file]
        (handle-admin-upload kodepel kode file))

  (GET "/admin-edit-kunci" []
       (admin-search-proset "/admin-edit-kunci-search"))
  (POST "/admin-edit-kunci-search" [nopel ket]
      (handle-admin-search-proset nopel ket "/admin-edit-kunci1" ""))
  (POST "/admin-edit-kunci1" [kode]
        (admin-edit-kunci (subs kode 1 (count kode)) "/admin-save-kunci"))
  (POST "/admin-save-kunci" [kunci jenis upto pretext sound kode]
        (admin-save-kunci kunci jenis upto (str "[" pretext "]") (str "[" sound "]") kode))

  (GET "/admin-lihat-soal" []
       (admin-search-proset "/admin-lihat-soal-search"))
  (POST "/admin-lihat-soal-search" [nopel ket]
      (handle-admin-search-proset nopel ket "/admin-lihat-soal1" ""))
  (POST "/admin-lihat-soal1" [kodepel kode]
        (admin-view-soal kodepel (subs kode 1 (count kode))))

  (GET "/admin-lihat-sekaligus" []
       (admin-search-proset "/admin-lihat-sekaligus-search"))
  (POST "/admin-lihat-sekaligus-search" [nopel ket]
      (handle-admin-search-proset nopel ket "/admin-lihat-sekaligus1" "_blank"))
  (POST "/admin-lihat-sekaligus1" [kodepel kode]
        (admin-lihat-sekaligus kodepel kode))

  (GET "/admin-hapus-set" []
       (admin-search-proset "/admin-hapus-set-search"))
  (POST "/admin-hapus-set-search" [nopel ket]
      (handle-admin-search-proset nopel ket "/admin-confirm-hapus" ""))
  (POST "/admin-confirm-hapus" [kode ket]
        (admin-confirm-hapus kode ket))
  (POST "/admin-confirm-fback" [kode ket yn]
        (if (= yn "Y") (admin-hapus-set ket (subs kode 1 (count kode))) (layout/render "admin/work.html")))
  (POST "/admin-hapus-set1" [ket kode]
        (admin-hapus-set ket (subs kode 1 (count kode))))

  (GET "/admin-input-siswa" []
       (layout/render "admin/input-siswa.html"))
  (POST "/admin-input-siswa" [file]
        (handle-input-siswa file))

  (GET "/admin-tambah-sekolah" []
       (layout/render "admin/tambah-sekolah.html"))
  (POST "/admin-tambah-sekolah" [kode sekolah npsn]
       (admin-tambah-sekolah kode sekolah npsn))
  (GET "/admin-edit-sekolah" []
       (admin-view-sekolah))
  (POST "/admin-edit-sekolah" [kode]
        (admin-edit-sekolah kode))
  (POST "/admin-update-sekolah" [kode nasek npsn]
        (admin-update-sekolah kode nasek npsn))

  (GET "/admin-tambah-paket-ppdb" []
       (layout/render "admin/tambah-paket-ppdb.html"))
  (POST "/admin-tambah-paket-ppdb" [ket mat ipa ind ing]
        (admin-tambah-paket-ppdb ket mat ipa ind ing))

  (GET "/admin-edit-paket-ppdb" []
       (admin-list-paket-ppdb))
  (POST "/admin-edit-paket-ppdb" [kode]
        (admin-edit-paket-ppdb kode))
  (POST "/admin-update-paket-ppdb" [kode ket mat ipa ind ing]
        (admin-update-paket-ppdb kode ket mat ipa ind ing))
  (POST "/admin-delete-paket-ppdb" [kode]
        (admin-delete-paket-ppdb kode))

  (GET "/admin-tambah-sma-ppdb" []
       (admin-tambah-sma-ppdb))

  (POST "/admin-tambah-sma-ppdb" [kodedaerah sekolah nm]
        (admin-tambah-sma-ppdb1 kodedaerah sekolah nm))

  (GET "/admin-edit-sma-ppdb" []
       (admin-list-sma-ppdb))
  (POST "/admin-edit-sma-ppdb" [sekolah]
        (admin-edit-sma-ppdb sekolah))

  (POST "/admin-update-sma-ppdb" [sekolah kode nm]
        (admin-update-sma-ppdb sekolah kode nm))
  (POST "/admin-delete-sma-ppdb" [sekolah]
        (admin-delete-sma-ppdb sekolah))

  (GET "/admin-tambah-daerah-ppdb" []
       (layout/render "admin/tambah-daerah.html"))
  (POST "/admin-tambah-daerah" [daerah]
        (admin-tambah-daerah daerah))

  (GET "/admin-edit-daerah-ppdb" []
       (admin-list-daerah-ppdb))
  (POST "/admin-edit-daerah-ppdb" [kode]
        (admin-edit-daerah-ppdb kode))
  (POST "/admin-update-daerah-ppdb" [kode daerah]
        (admin-update-daerah-ppdb kode daerah))

  (GET "/admin-tambah-paket-sbmptn" []
       (layout/render "admin/tambah-paket-sbmptn.html"))
  (POST "/admin-tambah-paket-sbmptn" [ket tkpa jurusan kelompok]
        (admin-tambah-paket-sbmptn ket tkpa jurusan kelompok))

  (GET "/admin-edit-paket-sbmptn" []
       (admin-list-paket-sbmptn))
  (POST "/admin-edit-paket-sbmptn" [kode]
        (admin-edit-paket-sbmptn kode))
  (POST "/admin-update-paket-sbmptn" [kode ket tkpa ipaorips kelompok]
        (admin-update-paket-sbmptn kode ket tkpa ipaorips kelompok))
  (POST "/admin-delete-paket-sbmptn" [kode]
        (admin-delete-paket-sbmptn kode))

  (GET "/admin-hitung-ulang-hasil" []
       (layout/render "admin/input-kode.html"))
  (POST "/admin-hitung-ulang-hasil" [kode]
        (admin-hitung-ulang-hasil kode))
  )
