(ns icbl.routes.adminmagang
  (:require [compojure.core :refer :all]
            [clojure.string :as st]
            [icbl.views.layout :as layout]
            [noir.response :as resp]
            [noir.io :as io]
            [icbl.models.db :as db]
            [noir.session :as session]
            [clojure.data.json :as json]
            [icbl.routes.teacher :as teacher]
            ))

(defn num-to-str [number dk]
  (-> (format (str "%." dk "f") (* number 1.0))
      (clojure.string/replace #"\." ",")))

(defn adminmagang-home []
  (layout/render "adminmagang/home.html")
  )

(defn handle-adminmagang-login [pass]
  (let [vpass (:pass (db/get-data (str "select pass from admin where id='adminmagang'") 1))
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))]
    (if (= vpass pass)
        (do
          (session/put! :id "adminmagang")
          (session/put! :status 4)
          (session/put! :ip ip)
          (layout/render "adminmagang/work.html"))
        (layout/render "adminmagang/home.html" {:error "Password Salah!"}))))

(defn adminmagang-logout []
  (do
   (session/clear!)
   (resp/redirect "/adminmagang")))

(defn adminmagang-registrasi-siswa [nis nama kelas email]
  (let [data (db/get-data (str "select nis from usermagang where nis='" nis "'") 1)]
    (if data
        (layout/render "adminmagang/registrasi-siswa.html"
                       {:nis nis
                        :namaku nama
                        :kelas kelas
                        :email email
                        :error "ID tersebut sudah terdaftar!"})
        (try
          (db/insert-data "usermagang" {:nis nis
                                   :nama nama
                                   :kelas kelas
                                   :email email
                                   :password "12345"
                                   ;:passortu "abcde"
                                        })
          (layout/render "adminmagang/pesan.html" {:pesan "Berhasil registrasi Peserta!"})
          (catch Exception ex
             (layout/render "adminmagang/pesan.html" {:pesan "Gagal registrasi Peserta!"}))))))

(defn adminmagang-handle-list-nama [nm]
  (let [upnm (clojure.string/upper-case nm)
        cdata (:jumlah (db/get-data (str "select count(*) as jumlah from usermagang where upper(nama) LIKE '%" upnm "%'") 1))
        data (db/get-data (str "select nis,nama,kelas from usermagang where upper(nama) LIKE '%" upnm "%'
                               order by nama LIMIT 15") 2)]
       (if data
         (layout/render "adminmagang/list-siswa-nama.html"
                        {:data data :cdata cdata :urut "nama" :vnama upnm :page 0})
         (layout/render "adminmagang/pesan.html" {:pesan "Tidak ada nama tersebut!"}))))

(defn adminmagang-handle-do-edit-siswa [nis]
  (let [datum (db/get-data (str "select * from usermagang where nis='" nis "'") 1)
        ;daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)
        ]
    (layout/render "adminmagang/edit-data-siswa.html"
                 {:datum datum
                  ;:daftarkelas daftarkelas
                  })))

(defn adminmagang-handle-update-data-siswa [nislama nisbaru nama kelas email pass passortu]
  (try (db/update-data-1 "usermagang"
                              ["nis=?" nislama]
                                      {:nis nisbaru
                                       :nama nama
                                       :kelas kelas
                                       :email email
                                       :password pass
                                       ;:passortu passortu
                                       })
               (layout/render "adminmagang/pesan.html" {:pesan "Berhasil mengubah data Peserta!"})
               (catch Exception ex
                (layout/render "adminmagang/pesan.html" {:pesan "Gagal mengubah data Peserta!"}))))

(defn adminmagang-list-siswa-newpage [urut newpage vnama cdata]
  (let [data (db/get-data (str "select nis,nama,kelas from usermagang where upper(nama)
                               LIKE '%" vnama "%' order by " urut " LIMIT 15 OFFSET "
                               (* (read-string newpage) 15)) 2)]
    (layout/render "adminmagang/list-siswa-nama.html"
                   {:data data :cdata (read-string cdata)
                    :urut urut :vnama vnama :page (read-string newpage)})))

(defn adminmagang-delete-siswa [act]
  (let [data (db/get-data "select nis,nama,kelas from usermagang order by kelas,nis" 2)]
    (layout/render "adminmagang/list-all-siswa.html" {:data data :action act :judul "HAPUS SISWA" :ket "menghapus"})))
(defn adminmagang-handle-hapus-siswa [nis]
  (do
    (db/delete-data "usermagang" (str "nis='" nis "'"))
    (adminmagang-delete-siswa "/adminmagang-delete-siswa")))

(defn adminmagang-tambah-pelajaran [pel]
  (let [Upel (clojure.string/upper-case pel)
        data (db/get-data (str "select pelajaran from pelajaranmagang where UPPER(pelajaran)='" Upel "'") 1)]
        (if data
          (layout/render "adminmagang/pesan.html" {:pesan (str "Pelajaran " Upel " sudah ada!")})
          (try
            (db/insert-data "pelajaranmagang" {:pelajaran Upel})
            (layout/render "adminmagang/pesan.html" {:pesan (str "Berhasil menambah pelajaran dengan nama " Upel)})
            (catch Exception ex
              (layout/render "adminmagang/pesan.html" {:pesan "Gagal menambah pelajaran!"}))))))

(defn adminmagang-view-pelajaran []
  (let [data (db/get-data "select * from pelajaranmagang order by pelajaran" 2)]
    (layout/render "adminmagang/view-pelajaran.html" {:data data})))

(defn adminmagang-edit-pelajaran [no]
  (let [datum (db/get-data (str "select nomer,pelajaran from pelajaranmagang where nomer='" no "'") 1)]
    (layout/render "adminmagang/edit-pelajaran.html" {:datum datum})))

(defn adminmagang-update-pelajaran [no pel]
  (try
    (db/update-data "pelajaranmagang" (str "nomer='" no "'") {:pelajaran pel})
    (layout/render "adminmagang/pesan.html" {:pesan "Berhasil update pelajaran!"})
    (catch Exception ex
      (layout/render "adminmagang/pesan.html" {:pesan (str "Gagal update pelajaran! Error: " ex)}))))

(defn handle-ganti-pw-adminmagang [pwlama pwbaru pwbaru1]
  (let [pwnow (:pass (db/get-data (str "select pass from admin where id='adminmagang'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru) 5))
        (layout/render "adminmagang/pesan.html" {:pesan "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru pwbaru1)
          (try (db/update-data-1 "admin" ["id=?" "adminmagang"] {:pass pwbaru})
                 (layout/render "adminmagang/pesan.html" {:pesan "Berhasil mengubah password admin magang!"})
               (catch Exception ex
                  (layout/render "adminmagang/pesan.html" {:pesan "Gagal mengubah data password admin magang!"})))
          (layout/render "adminmagang/pesan.html" {:pesan "Gagal mengubah password admin magang!"})))))

(defn adminmagang-buat-proset [nopel ket jsoal waktu jumpil]
  (let [vjenis (if (= "0" jumpil) "2" "1")]
    (try
        (db/insert-data "magangproset"
                                 {:id (session/get :id)
                                  :kodepel (read-string nopel)
                                  :keterangan ket
                                  :jsoal (Integer/parseInt jsoal)
                                  :waktu (Integer/parseInt waktu)
                                  :kunci (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                  :jenis (str (vec (repeat (Integer/parseInt jsoal) vjenis)))
                                  :upto (str (vec (repeat (Integer/parseInt jsoal) jumpil)))
                                  ;:pretext (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                  ;:sound (str (vec (repeat (Integer/parseInt jsoal) "-")))
                                  :jumpil jumpil
                                  :acak "0"
                                  :status "0"
                                  :skala 100
                                  :nbenar 1
                                  :nsalah 0})
        (layout/render "adminmagang/pesan.html" {:pesan (str "Berhasil daftarkan proset!")})
        (catch Exception ex
                    (layout/render "adminmagang/pesan.html" {:pesan (str "Gagal daftarkan proset! error: " ex)})))))

(defn handle-adminmagang-search-proset [nopel ket act target]
  (let [Uket (clojure.string/upper-case ket)
        data (db/get-data (str "select kode,pelajaranmagang.pelajaran as pelajaran,keterangan,jsoal,waktu,status from magangproset
                               inner join pelajaranmagang on magangproset.kodepel=pelajaranmagang.nomer where
                               kodepel='" nopel "' and upper(keterangan) LIKE '%" Uket "%'
                               order by keterangan") 2)
        ]
    (layout/render "adminmagang/list-proset.html" {:data data :action act :kodepel nopel :ket ket
                                             :target target})))

(defn adminmagang-search-proset [act]
  (let [data (db/get-data "select * from pelajaranmagang order by pelajaran" 2)]
    (layout/render "adminmagang/search-proset.html" {:act act :data data})))

(defn adminmagang-upload-file [kode kodepel]
  (do
    (io/create-path (str "resources/public/magangproset/" kodepel "/" kode) true)
    (layout/render "adminmagang/upload.html" {:kode kode :kodepel kodepel})))

(defn handle-adminmagang-upload [kodepel kode file]
  (try
    (if (vector? file)
      (doseq [i file]
          (io/upload-file (str "resources/public/magangproset/" kodepel "/" kode) i))
      (io/upload-file (str "resources/public/magangproset/" kodepel "/" kode) file))
      (layout/render "adminmagang/pesan.html" {:pesan "Berhasil upload file!"})
     (catch Exception ex
                  (layout/render "adminmagang/pesan.html" {:pesan (str "Gagal upload file! error: " ex)}))
    ))

(defn adminmagang-edit-kunci [kode act]
  (let [datum (db/get-data (str "select kunci,jsoal,jumpil,jenis,upto,pretext,sound from magangproset where kode='" kode"'") 1)]
    (layout/render "adminmagang/edit-kunci.html"
                                             {:kunci (read-string (datum :kunci))
                                              :jsoal (datum :jsoal)
                                              :jumpil (datum :jumpil)
                                              :jenis (read-string (datum :jenis))
                                              :upto (read-string (datum :upto))

                                              :pretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                              :sound (if (datum :sound) (read-string (datum :sound)) nil)
                                              :kode kode
                                              :action act})))

(defn adminmagang-save-kunci [kunci jenis upto pretext sound kode]
  ;(println kunci)
  (try
    (db/update-data "magangproset" (str "kode='" kode "'") {:kunci (str (st/split kunci #":"))
                                                            :jenis (str (st/split jenis #","))
                                                            :upto (str (st/split upto #","))
                                                            :pretext (str (st/split pretext #":"))
                                                            :sound (str (st/split sound #":"))})
    (layout/render "adminmagang/pesan.html" {:pesan "Kunci berhasil disimpan!"})
    (catch Exception ex
                  (layout/render "adminmagang/pesan.html" {:pesan (str "Gagal simpan kunci! error: " ex)}))))

(defn adminmagang-edit-proset [kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from magangproset where kode='" postkode "'") 1)]
    (layout/render "adminmagang/edit-proset.html" {:datum datum :kode kode})))

(defn adminmagang-update-proset [kode ket jsoal waktu jumpil skala nbenar nsalah acak status]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select kunci,jenis,upto,pretext,sound from magangproset where kode='" postkode "'") 1)
        oldkunci (read-string (datum :kunci))
        oldjenis (read-string (datum :jenis))
        oldupto (read-string (datum :upto))
        oldpretext (if (datum :pretext) (read-string (datum :pretext)) nil)
        oldsound (if (datum :sound) (read-string (datum :sound)) nil)
        cok (count oldkunci)
        vjsoal (Integer/parseInt jsoal)
        newkunci (cond
                   (= vjsoal cok) (str oldkunci)
                   (< vjsoal cok) (str (vec (take vjsoal oldkunci)))
                   :else (str (vec (concat oldkunci (repeat (- vjsoal cok) "-")))))
        newjenis (cond
                   (= vjsoal cok) (str oldjenis)
                   (< vjsoal cok) (str (vec (take vjsoal oldjenis)))
                   :else (str (vec (concat oldjenis (repeat (- vjsoal cok) "1")))))
        newupto (cond
                   (= vjsoal cok) (str oldupto)
                   (< vjsoal cok) (str (vec (take vjsoal oldupto)))
                   :else (str (vec (concat oldupto (repeat (- vjsoal cok) jumpil)))))
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

        ]
  (try
    (db/update-data "magangproset" (str "kode='" postkode "'")
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
    (layout/render "adminmagang/pesan.html" {:pesan (str "Berhasil update proset!")})
    (catch Exception ex
                  (layout/render "adminmagang/pesan.html" {:pesan (str "Gagal update proset! error: " ex)})))))

(defn adminmagang-view-soal [kodepel kode]
  (let [datum (db/get-data (str "select * from magangproset where kode='" kode "'") 1)]
    (layout/render "adminmagang/view-soal.html" {:datum datum
                                             :nsoal (vec (range 1 (inc (datum :jsoal))))
                                             :kategori "1"
                                             ;:pel pel
                                             :kunci (read-string (datum :kunci))
                                             :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                             :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                             ;:soalpath "http://127.0.0.1/resources/public"
                                             })))

(defn adminmagang-lihat-sekaligus [kodepel kode]
  (let [postkode (subs kode 1 (count kode))
        datum (db/get-data (str "select * from magangproset where kode='" postkode "'") 1)]
    (layout/render "adminmagang/view-soal-sekaligus.html" {:datum datum
                                                       ;:kodepel kodepel
                                                       :kode kode
                                                       :kunci (read-string (datum :kunci))
                                                       :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                                       :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                                       ;soalpath "http://localhost/resources/public"
                                                       })))

(defn adminmagang-confirm-hapus [kode ket]
  (let [vkode (subs kode 1 (count kode))
        proset (db/get-data (str "select kode,keterangan from magangproset where kode='" vkode "'") 1)]
    (layout/render "adminmagang/confirm-hapus.html" {:kode kode
                                               ;:pelajaran (proset :pelajaran)
                                               :keterangan (proset :keterangan)
                                               ;:pel pel
                                               :ket ket})))

(defn adminmagang-hapus-set [ket kode]
  (try
    (db/delete-data "magangproset" (str "kode='" kode "'"))
    (layout/render "adminmagang/pesan.html" {:pesan (str "Set Soal dengan kode B" kode " berhasil dihapus!" )})
    (catch Exception ex
      (layout/render "adminmagang/pesan.html" {:pesan (str "Gagal Hapus Proset! error " ex)}))
    ))

(defn adminmagang-hasil-test [kode html]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        mdata (db/get-data (str "select kode,keterangan,jsoal,kunci from magangproset where kode='" postkode "'") 1)
        data (db/get-data (str "select datamagang.nis as nis,nama,kelas,nilai,jawaban from datamagang INNER JOIN
                               usermagang ON usermagang.nis=datamagang.nis WHERE datamagang.kode='" kode "'
                                   order by nilai desc") 2)
        data1 (map #(update-in %1 [:nilai] num-to-str 2) data)
        kunci (read-string (:kunci mdata))]
    ;(println data2)
    (layout/render html {:data data1 :mdata mdata :kunci kunci :kode kode})))

(defn adminmagang-test-detail-siswa [nis kode]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata "magangproset"
        pkt (db/get-data (str "select kode,keterangan,jsoal,kunci from " tdata " where kode='" postkode "'") 1)
        data (db/get-data (str "select datamagang.nis as nis,kode,jawaban,nilai,nama from datamagang
                               INNER JOIN usermagang ON datamagang.nis=usermagang.nis
                               where kode='" kode "' and datamagang.nis='" nis "'") 1)
        jawaban (read-string (data :jawaban))
        jsoal (pkt :jsoal)
        vkunci (read-string (pkt :kunci))
        benar (count (filter true? (map #(= %1 %2) vkunci jawaban)))
        kosong (count (filter true? (map #(= % "-") jawaban)))
        salah (- jsoal (+ benar kosong))
         ]
        (layout/render "adminmagang/nilai-detail-siswa.html"
                       {:data data
                        ;:pelajaran (pkt :pelajaran)
                        :keterangan (pkt :keterangan)
                        ;:kodesoal kodesoal
                        :benar benar
                        :salah salah
                        :kosong kosong
                        :kunci vkunci
                        :jawaban jawaban
                        :kode kode
                        })))

(defn adminmagang-lihat-soal [nomer kode]
  (let [prekode (subs kode 0 1)
        postkode (subs kode 1 (count kode))
        tdata "magangproset"
        id (:kodepel (db/get-data (str "select kodepel from magangproset where kode='" postkode "'") 1))]
    (layout/render "adminmagang/lihat-soal.html" {:id id :kode kode :nomer nomer
                                              :postkode postkode :tabel tdata})))

(defroutes adminmagang-routes

  (GET "/adminmagang" []
      (adminmagang-home))

  (GET "/adminmagang-home" []
       (layout/render "adminmagang/work.html"))

  (GET "/adminmagang-logout" []
       (adminmagang-logout))

  (POST "/adminmagang-login" [pass]
      (handle-adminmagang-login pass))

  (GET "/adminmagang-registrasi-siswa" []
       (layout/render "adminmagang/registrasi-siswa.html"))
  (POST "/adminmagang-registrasi-siswa" [nis nama kelas email]
        (adminmagang-registrasi-siswa nis nama kelas email))

  (GET "/adminmagang-edit-siswa" []
       (layout/render "adminmagang/search-siswa.html"))
  (POST "/adminmagang-list-siswa-newpage" [urut newpage vnama cdata]
        (adminmagang-list-siswa-newpage urut newpage vnama cdata))
  (POST "/adminmagang-edit-siswa" [nama]
        (adminmagang-handle-list-nama nama))
  (POST "/adminmagang-do-edit-siswa" [nis]
        (adminmagang-handle-do-edit-siswa nis))
  (POST "/adminmagang-update-data-siswa" [nislama nisbaru nama kelas email pass passortu]
        (adminmagang-handle-update-data-siswa nislama nisbaru nama kelas email pass passortu))
  (GET "/adminmagang-delete-siswa" []
       (adminmagang-delete-siswa "/adminmagang-delete-siswa"))
  (POST "/adminmagang-delete-siswa" [nis]
        (adminmagang-handle-hapus-siswa nis))

  (GET "/adminmagang-tambah-pelajaran" []
       (layout/render "adminmagang/tambah-pelajaran.html"))
  (POST "/adminmagang-tambah-pelajaran" [pelajaran]
        (adminmagang-tambah-pelajaran pelajaran))

  (GET "/adminmagang-edit-pelajaran" []
       (adminmagang-view-pelajaran))
  (POST "/adminmagang-edit-pelajaran" [nomer]
      (adminmagang-edit-pelajaran nomer))
  (POST "/adminmagang-update-pelajaran" [nomer pelajaran]
      (adminmagang-update-pelajaran nomer pelajaran))

  (GET "/ganti-pw-adminmagang" []
       (layout/render "adminmagang/ganti-pw-admin.html"))
  (POST "/ganti-pw-adminmagang" [pwlama pwbaru pwbaru1]
        (handle-ganti-pw-adminmagang pwlama pwbaru pwbaru1))

  (GET "/adminmagang-buat-proset" []
       (let [data (db/get-data  "select nomer,pelajaran from pelajaranmagang order by pelajaran" 2)]
         (layout/render "adminmagang/buat-proset.html" {:data data})))
  (POST "/adminmagang-buat-proset" [nopel ket jsoal waktu jumpil]
        (adminmagang-buat-proset nopel ket jsoal waktu jumpil))

  (GET "/adminmagang-upload-file" []
       (adminmagang-search-proset "/adminmagang-pilih-proset1"))
  (POST "/adminmagang-pilih-proset1" [nopel ket]
       (handle-adminmagang-search-proset nopel ket "/adminmagang-upload-file1" ""))
  (POST "/adminmagang-upload-file1" [kode kodepel]
        (adminmagang-upload-file (subs kode 1 (count kode)) kodepel))
  (POST "/adminmagang-upload" [kodepel kode file]
        (handle-adminmagang-upload kodepel kode file))

  (GET "/adminmagang-edit-kunci" []
       (adminmagang-search-proset "/adminmagang-edit-kunci-search"))
  (POST "/adminmagang-edit-kunci-search" [nopel ket]
      (handle-adminmagang-search-proset nopel ket "/adminmagang-edit-kunci1" ""))
  (POST "/adminmagang-edit-kunci1" [kode]
        (adminmagang-edit-kunci (subs kode 1 (count kode)) "/adminmagang-save-kunci"))
  (POST "/adminmagang-save-kunci" [kunci jenis upto pretext sound kode]
        (adminmagang-save-kunci kunci jenis upto pretext sound kode))

  (GET "/adminmagang-edit-set-soal" []
       (adminmagang-search-proset "/adminmagang-search-proset1"))
  (POST "/adminmagang-search-proset1" [nopel ket]
        (handle-adminmagang-search-proset nopel ket "/adminmagang-edit-proset" ""))
  (POST "/adminmagang-edit-proset" [kode]
        (adminmagang-edit-proset kode))
  (POST "/adminmagang-update-proset" [kode ket jsoal waktu jumpil skala nbenar nsalah acak status]
         (adminmagang-update-proset kode ket jsoal waktu jumpil skala nbenar nsalah acak status))

  (GET "/adminmagang-lihat-soal" []
       (adminmagang-search-proset "/adminmagang-lihat-soal-search"))
  (POST "/adminmagang-lihat-soal-search" [nopel ket]
      (handle-adminmagang-search-proset nopel ket "/adminmagang-lihat-soal1" ""))
  (POST "/adminmagang-lihat-soal1" [kodepel kode]
        (adminmagang-view-soal kodepel (subs kode 1 (count kode))))

  (GET "/adminmagang-lihat-sekaligus" []
       (adminmagang-search-proset "/adminmagang-lihat-sekaligus-search"))
  (POST "/adminmagang-lihat-sekaligus-search" [nopel ket]
      (handle-adminmagang-search-proset nopel ket "/adminmagang-lihat-sekaligus1" "_blank"))
  (POST "/adminmagang-lihat-sekaligus1" [kodepel kode]
        (adminmagang-lihat-sekaligus kodepel kode))

  (GET "/adminmagang-hapus-set" []
       (adminmagang-search-proset "/adminmagang-hapus-set-search"))
  (POST "/adminmagang-hapus-set-search" [nopel ket]
      (handle-adminmagang-search-proset nopel ket "/adminmagang-confirm-hapus" ""))
  (POST "/adminmagang-confirm-hapus" [kode ket]
        (adminmagang-confirm-hapus kode ket))
  (POST "/adminmagang-confirm-fback" [kode ket yn]
        (if (= yn "Y") (adminmagang-hapus-set ket (subs kode 1 (count kode))) (layout/render "admin/work.html")))

  (GET "/adminmagang-hasil-test" []
       (adminmagang-search-proset "/adminmagang-hasil-test-search"))
  (POST "/adminmagang-hasil-test-search" [nopel ket]
       (handle-adminmagang-search-proset nopel ket "/adminmagang-hasil-test" ""))

;;   (POST "/adminmagang-pilih-kelas" [kodesoal]
;;         (if (= kosek "SEMUA")
;;           (admin-hasil-test kodesoal "" "SEMUA" "admin/hasil-test.html")
;;           (admin-pilih-kelas kosek kodesoal "/admin-hasil-testB")))
  (POST "/adminmagang-hasil-test" [kode]
         (adminmagang-hasil-test kode "adminmagang/hasil-test.html"))
  (POST "/adminmagang-test-detail-siswa" [nis kode]
        (adminmagang-test-detail-siswa nis kode))
  (POST "/adminmagang-lihat-soal" [nomer kode]
        (adminmagang-lihat-soal nomer kode))
)
