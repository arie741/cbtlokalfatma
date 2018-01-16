(ns icbl.routes.home
  (:require [compojure.core :refer :all]
            [icbl.views.layout :as layout]
            ;[noir.validation :as vali]
            ;[noir.util.crypt :as crypt]
            [noir.response :as resp]
            [icbl.models.db :as db]
            [noir.session :as session]
            [icbl.models.share :as share]
            ))

(defn home-num-to-str [number dk]
  (-> (format (str "%." dk "f") (* number 1.0))
      (clojure.string/replace #"\." ",")))

(defn handle-login [nis pass]
  (let [user (db/get-data (str "select nis,password,nama from users where nis='" nis "'") 1)
        ip (:ipnumber (db/get-data "select ipnumber from ip where no=1" 1))
        ]
      (if user
         (if (= pass (user :password))
           (do
             (session/put! :id nis)
             (session/put! :nama (user :nama))
             (session/put! :ip ip)
             (session/put! :status 4)
             (layout/render "home/home.html"))
           (layout/render "share/login.html"
                          {:error "Password Salah!" :nis nis :action "/home-login"}))
         (layout/render "share/login.html"
                          {:error "Tidak ada user dengan NIS tersebut!"
                           :nis nis :action "/home-login"}))
    ))

(defn acak-soal
  [dt]
  (let [a (group-by #(nth % 3) dt)
        b (filter #(= "-" (first %)) a)
        b1 (group-by #(last %) (second (first b)))
        b2 (filter #(= "-" (first %)) b1)
        b3 (map #(second %) b2)

        c (filter #(not= "-" (first %)) b1)
        c1 (map #(second %) c)

        d (filter #(not= "-" (first %)) a)
        e (map #(second %) d)
        f (concat (first b3) e)
        g (concat c1 (shuffle f))
        h (partition 5 (flatten g))
        ]
    ;(println b)
    h
    ))

(defn handle-kodeto1 [kodeto]
  (let [pre (subs kodeto 0 1)
        kd (subs kodeto 1 (count kodeto))
        kdns (clojure.string/replace kd #" " "")
        f-nkd (= (count kd) (count kdns))
        f-int-kd (integer? (read-string kd))]
     (if (or (and (not= pre "B") (not= pre "L")) (not f-nkd) (not f-int-kd))
       (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto})
       (let [data (if (= pre "B")
                      (db/get-data (str "select * from bankproset where kode='" kd "'") 1)
                      (db/get-data (str "select * from proset where kode='" kd "'") 1))]
          (cond 
            (and data (= (data :status) "1")) (let [jsoal (data :jsoal)
                                                   vjaw (partition 5 (interleave (range 1 (inc jsoal)) (data :jenis) (data :upto)
                                                                                 (if (data :pretext) (read-string (data :pretext)) (repeat jsoal "-"))
                                                                                 (if (data :sound) (read-string (data :sound)) (repeat jsoal "-"))))
                                                   ;vjaw-acak vjaw
                                                   vjaw1 (if (= "1" (data :acak)) (shuffle vjaw) vjaw)
                                                   nsoal (vec (map #(first %) vjaw1))
                                                   njenis (vec (map #(second %) vjaw1))
                                                   nupto (apply str (map #(str (nth % 2)) vjaw1))
                                                   npretext (vec (map #(nth % 3) vjaw1))
                                                   nsound (vec (map #(last %) vjaw1))
                                                   ;;page (if (= pre "B") "home/tryoutB.html" "home/tryout.html")
                                                   page "home/tryout.html"
                                                   ]
                                                  ;(println vjaw)
                                                  (layout/render page {:data data
                                                                       :nsoal nsoal
                                                                       :njenis njenis
                                                                       :nupto nupto
                                                                       :npretext npretext
                                                                       :nsound nsound
                                                                       :kodeto kodeto}))
            (and data (= (data :status) "2")) (let [sklh (->>
                                                            (session/get :id)
                                                            (apply str)
                                                            (take 3)
                                                            (apply str))
                                                    kelas (:kelas (db/get-data (str "select * from users where nis='" (session/get :id) "'") 1))
                                                    ses (:sessions (db/get-data (str "select * from bankproset where kode='" kd "'") 1))]
                                                (if (empty? (filter #(= sklh (first %)) (read-string ses)))
                                                  (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto})
                                                  (if (empty? (filter #(= kelas (second %)) (read-string ses)))
                                                    (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto})
                                                    (let [jsoal (data :jsoal)
                                                       vjaw (partition 5 (interleave (range 1 (inc jsoal)) (data :jenis) (data :upto)
                                                                                     (if (data :pretext) (read-string (data :pretext)) (repeat jsoal "-"))
                                                                                     (if (data :sound) (read-string (data :sound)) (repeat jsoal "-"))))
                                                       ;vjaw-acak vjaw
                                                       vjaw1 (if (= "1" (data :acak)) (shuffle vjaw) vjaw)
                                                       nsoal (vec (map #(first %) vjaw1))
                                                       njenis (vec (map #(second %) vjaw1))
                                                       nupto (apply str (map #(str (nth % 2)) vjaw1))
                                                       npretext (vec (map #(nth % 3) vjaw1))
                                                       nsound (vec (map #(last %) vjaw1))
                                                       ;;page (if (= pre "B") "home/tryoutB.html" "home/tryout.html")
                                                       page "home/tryout.html"
                                                       ]
                                                      ;(println vjaw)
                                                      (layout/render page {:data data
                                                                           :nsoal nsoal
                                                                           :njenis njenis
                                                                           :nupto nupto
                                                                           :npretext npretext
                                                                           :nsound nsound
                                                                           :kodeto kodeto})))))
            :else (layout/render "home/kode1.html" {:error "Paket Soal dengan kode tersebut tidak ada!" :kodeto kodeto}))
    ))))


(defn handle-simpan-jawaban [kode jawaban ni]
  (let [nis ni
        prekode (subs kode 0 1)
        remkode (subs kode 1 (count kode))
        tdata (if (= (subs kode 0 1) "B") "bankproset" "proset")
        dproset (db/get-data (str "select * from " tdata " where kode='" remkode "'") 1)
        ada (db/get-data (str "select nis from dataus where nis='" nis "' and kode='" kode "'") 1)

        jsoal (count jawaban)
        kunci (vec (map str (seq (:kunci dproset))))

        jbenar (loop [jb 0, i 0]
                          (if (= i jsoal)
                              jb
                              (recur (if (= (subs jawaban i (inc i)) (kunci i)) (inc jb) jb) (inc i))))
        jkosong (count (filter #(= % \-) (vec jawaban)))
        jsalah (- jsoal (+ jbenar jkosong))
        skala (:skala dproset)
        nbenar (:nbenar dproset)
        nsalah (:nsalah dproset)
        nilai (/ (Math/round (* (/ (+ (* jbenar nbenar) (* jsalah nsalah)) (* jsoal nbenar)) skala 100.0)) 100.0)
        vkd kode
        ]
         (if (not ada)
             (try (db/insert-data "dataus"  {:nis nis
                                             :kode vkd
                                             :jawaban jawaban
                                             :nilai nilai
                                             :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
              {:nilai nilai :skala skala}
               ;{:nilai nil}
              (catch Exception ex
                {:nilai nil :skala skala}))
             (try (db/update-data-1 "dataus"
                                    ["nis=? AND kode=?" nis vkd]
                                      {:nis nis
                                       :kode vkd
                                       :jawaban jawaban
                                       :nilai nilai
                                       :tanggal (java.sql.Timestamp. (.getTime (java.util.Date.)))})
               {:nilai nilai :skala skala}
               (catch Exception ex
                {:nilai nil :skala skala}))
           )))

(defn home-login []
  (layout/render "share/login.html" {:action "/home-login"}))

(defn home []
  (layout/render "home/home.html"))

(defn home-registrasi-siswa []
  (let [daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)
        kelas (:namakelas (first daftarkelas))]
    (layout/render "share/registrasi-siswa.html" {:daftarkelas daftarkelas :kelas kelas})))

(defn handle-reg-siswa [nis nama kelas email pw1 pw2]
  (let [user (db/get-data (str "select nis from users where nis='" nis "'") 1)
        daftarkelas (db/get-data "select namakelas from kelas order by namakelas asc" 2)]
    (if user
      (layout/render "share/registrasi-siswa.html"
                     {:error "NIS tersebut sudah terdaftar!"
                      :nis nis :vnama nama :kelas kelas :email email :daftarkelas daftarkelas})
      (if (not= pw1 pw2)
          (layout/render "share/registrasi-siswa.html"
                         {:error "Kata Sandi tidak cocok!"
                          :nis nis :vnama nama :kelas kelas :email email :daftarkelas daftarkelas})
          (if (< (count pw1) 5)
              (layout/render "share/registrasi-siswa.html"
                             {:error "Kata sandi paling sedikit 5 digit!"
                              :nis nis :vnama nama :kelas kelas :email email :daftarkelas daftarkelas})
              (do
                (db/insert-data "users" {:nis nis :kelas kelas :email email :password pw1 :nama nama})
                (session/put! :id nis)
                (session/put! :nama nama)
                (layout/render "share/login.html"))))
      )))

(defn handle-to-lanjutan [kode]
  (let [pre (subs kode 0 1)
        remko (subs kode 1 (count kode))
        tabel (if (= pre "B") "bankproset" "proset")
        data (db/get-data (str "select pelajaran,keterangan from " tabel " where kode='" remko "'") 1)]
  (layout/render "home/tryout-lanjutan.html" {:data data :kodeto kode})))

(defn home-ganti-pw-siswa [pwlama pwbaru1 pwbaru2]
  (let [pwnow (:password (db/get-data (str "select password from users where nis='" (session/get :id) "'") 1))]
    (if (or (not= pwlama pwnow) (< (count pwbaru1) 5))
        (layout/render "home/ganti-pw.html" {:error "Password Lama tidak benar atau password baru kurang dari lima huruf!"})
        (if (= pwbaru1 pwbaru2)
          (try (db/update-data "users" (str "nis='" (session/get :id) "'") {:password pwbaru2})
                 (do
                    (session/clear!)
                    (resp/redirect "/"))
               (catch Exception ex
                  (layout/render "home/ganti-pw.html" {:error "Gagal ganti password!"})))
          (layout/render "home/ganti-pw.html" {:error "Password tidak sesuai!"})))))

(defn handle-lihat-hasil [nis salam]
  (let [data (db/get-data
               (str
                 ;"select tanggal,kode,nilai from dataus where nis='" nis "' order by tanggal desc"
                 "select dataus.kode,nilai,to_char(tanggal,'DD-MM-YYYY') as stanggal,keterangan from dataus
                  inner join bankproset on to_number (substring (dataus.kode,2,4),'999999')=bankproset.kode
                  where nis='" nis "' order by keterangan,tanggal desc") 2)
        data1 (map #(update-in %1 [:nilai] home-num-to-str 2) data)]
    (layout/render "home/list-nilai.html" {:salam salam :data data1})))

(defn handle-detail-set [kode nis]
  (let [pre (subs kode 0 1)
        remko (subs kode 1 (count kode))
        tabel (if (= pre "B") "bankproset" "proset")
        ket (db/get-data (str "select kode,pelajaran,keterangan from " tabel "
                              where kode='" remko "'") 1)
        nilai (db/get-data (str "select nilai from  dataus  where kode='" kode "'
                                and nis='" nis "'") 1)]
    (layout/render "home/detail-set.html" {:ket ket :nilai (home-num-to-str (nilai :nilai) 2)})))

(defn home-view-soal [kodesoal kodebahas]
  (let [prekode (subs kodesoal 0 1)
        postkode (subs kodesoal 1 (count kodesoal))
        tabel (if (= "L" prekode) "proset" "bankproset")
        html (if (= "L" prekode) "home/view-soal.html" "home/view-soalB.html")
        datum (db/get-data (str "select * from " tabel " where kode='" postkode "'") 1)]
       (layout/render html {:datum datum
                                             :kategori "1"
                                             :nsoal (vec (range 1 (inc (datum :jsoal))))
                                             :npretext (if (datum :pretext) (read-string (datum :pretext)) nil)
                                             :nsound (if (datum :sound) (read-string (datum :sound)) nil)
                                             :kode kodebahas
                                             })))

(defn home-kodebahas [kodebahas]
  (let [kodesoal (db/get-data
                (str "select kodesoal from sesibahas where nomer='" kodebahas "'") 1)]
    (if kodesoal
      (home-view-soal (:kodesoal kodesoal) kodebahas)
      (layout/render "home/nomer.html" {:kodebahas kodebahas :error "Tidak ada kode pembahasan tersebut!"}))))

(defn home-pilih-paket []
  (let [data (db/get-data "select kode,kodetkpa,kodeipaorips,keterangan from simsbmptn order by keterangan" 2)]
    (layout/render "home/pilih-paket.html" {:data data})))

(defn home-proses-paket [kode]
  (let [paket (db/get-data (str "select * from simsbmptn where kode='" kode "'") 1)
        kelompok (:kelompok paket)
        kodetkpa (:kodetkpa paket)
        kodeipaorips (:kodeipaorips paket)
        nilaitkpa (db/get-data (str "select nilai from dataus where nis='" (session/get :id) "'
                                    and kode='" kodetkpa "'") 1)
        nilaiipaorips (db/get-data (str "select nilai from dataus where nis='" (session/get :id) "'
                                       and kode='" kodeipaorips "'") 1)]
    (if (and nilaitkpa nilaiipaorips)
        (let [ntot (/ (+ (:nilai nilaitkpa) (:nilai nilaiipaorips)) 2.0)
              univ (db/get-data (str "select distinct ptn from pg1 where nm <='" ntot "' and
                                     kelompok='" kelompok "' order by ptn") 2)
              funiv (if univ (:ptn (first univ)) "VOID")
              jurusan (if univ
                          (db/get-data (str "select kodejur,jurusan,nm from pg1 where ptn='" funiv "' and
                                        nm<='" ntot "' and kelompok='" kelompok "' order by nm desc") 2)
                          "VOID")]
          (if univ
            (layout/render "home/list-jurusan.html" {:ntot (format (str "%." 2 "f") (* ntot 1.0))
                                                     :jurusan jurusan
                                                     :univ univ
                                                     :kelompok kelompok
                                                     :sainsoshum (if (= kelompok "1") "SAINTEK" "SOSHUM")
                                                     :ntkpa (format (str "%." 2 "f") (* (:nilai nilaitkpa) 1.0))
                                                     :nipaorips (format (str "%." 2 "f") (* (:nilai nilaiipaorips) 1.0))
                                                     :ptn funiv})
            (layout/render "home/pesan.html" {:pesan "Nilai tidak memenuhi syarat di semua universitas!"})))
         (layout/render "home/pesan.html" {:pesan "Nilai ujian tidak lengkap!"}))))

(defn home-next-univ [ptn ntot ntkpa nipaorips kelompok]
  (let [univ (db/get-data (str "select distinct ptn from pg1 where nm <='" ntot "' and
                                     kelompok='" kelompok "' order by ptn") 2)
        jurusan (db/get-data (str "select kodejur,jurusan,nm from pg1 where ptn='" ptn "' and
                                        nm<='" ntot "' and kelompok='" kelompok "' order by nm desc") 2)]
    (layout/render "home/list-jurusan.html" {:ntot ntot
                                             :jurusan jurusan
                                             :univ univ
                                             :kelompok kelompok
                                             :sainsoshum (if (= kelompok "1") "SAINTEK" "SOSHUM")
                                             :ntkpa ntkpa
                                             :nipaorips nipaorips
                                             :ptn ptn})))

(defn home-pilih-paket-ppdb []
  (let [data (db/get-data "select kode,kodemat,kodeipa,kodeind,kodeing,keterangan from simppdb order by keterangan" 2)]
    (layout/render "home/pilih-paket-ppdb.html" {:data data})))

(defn home-proses-paket-ppdb [kode]
  (let [paket (db/get-data (str "select * from simppdb where kode='" kode "'") 1)
        kodemat (:kodemat paket)
        kodeipa (:kodeipa paket)
        kodeind (:kodeind paket)
        kodeing (:kodeing paket)
        nilaimat (db/get-data (str "select nilai from dataus where nis='" (session/get :id) "'
                                    and kode='" kodemat "'") 1)
        nilaiipa (db/get-data (str "select nilai from dataus where nis='" (session/get :id) "'
                                       and kode='" kodeipa "'") 1)
        nilaiind (db/get-data (str "select nilai from dataus where nis='" (session/get :id) "'
                                       and kode='" kodeind "'") 1)
        nilaiing (db/get-data (str "select nilai from dataus where nis='" (session/get :id) "'
                                       and kode='" kodeing "'") 1)]
    (if (and nilaimat nilaiipa nilaiind nilaiing)
        (let [ntot (+ (:nilai nilaimat) (:nilai nilaiipa) (:nilai nilaiind) (:nilai nilaiing))
              daerah (db/get-data (str "select distinct pgsma.kodedaerah as kode, daerah from pgsma
                                     inner join kodedaerah on pgsma.kodedaerah=kodedaerah.kode
                                     where nm <='" ntot "' order by daerah") 2)
              fkode (if daerah (:kode (first daerah)) "VOID")
              fdaerah (if daerah (:daerah (first daerah)) "VOID")
              sekolah (if daerah
                          (db/get-data (str "select sekolah,nm from pgsma where kodedaerah='" fkode "' and
                                        nm<='" ntot "' order by nm desc") 2)
                          "VOID")]
          (if daerah
            (layout/render "home/list-sma.html" {:ntot (format (str "%." 2 "f") (* ntot 1.0))
                                                     :daerah daerah
                                                     :sekolah sekolah
                                                     :nmat (format (str "%." 2 "f") (* (:nilai nilaimat) 1.0))
                                                     :nipa (format (str "%." 2 "f") (* (:nilai nilaiipa) 1.0))
                                                     :nind (format (str "%." 2 "f") (* (:nilai nilaiind) 1.0))
                                                     :ning (format (str "%." 2 "f") (* (:nilai nilaiing) 1.0))
                                                     :wilayah fdaerah
                                                     :kodedaerah fkode
                                                     })
            (layout/render "home/pesan.html" {:pesan "Nilai tidak memenuhi syarat di semua Daerah!"})))
         (layout/render "home/pesan.html" {:pesan "Nilai ujian tidak lengkap!"}))))

(defn home-next-daerah [kode ntot nmat nipa nind ning]
  (let [daerah (db/get-data (str "select distinct pgsma.kodedaerah as kode, daerah from pgsma
                                     inner join kodedaerah on pgsma.kodedaerah=kodedaerah.kode
                                     where nm <='" ntot "' order by daerah") 2)
        sekolah (db/get-data (str "select sekolah,nm from pgsma where kodedaerah='" kode "' and
                                        nm<='" (read-string ntot) "' order by nm desc") 2)
        wilayah (db/get-data (str "select daerah from kodedaerah where kode='" kode "'") 1)]
    (layout/render "home/list-sma.html" {:ntot ntot
                                          :daerah daerah
                                             :sekolah sekolah
                                             :nmat nmat
                                             :nipa nipa
                                             :nind nind
                                             :ning ning
                                             :wilayah (:daerah wilayah)
                                             :kodedaerah kode})))

(defn home-get-sekolah [kode ntot]
  (db/get-data (str "select sekolah,nm from pgsma where kodedaerah='" kode "' and
                                        nm<='" ntot "' order by nm desc") 2))

(defn home-get-jurusan [ptn kelompok ntot]
  (db/get-data (str "select kodejur,jurusan,nm from pg1 where ptn='" ptn "' and
                                        nm<='" ntot "' and kelompok='" kelompok "' order by nm desc") 2))

(defroutes home-routes
  (GET "/" [] (home-login))
  (GET "/home" []
       (home))
  (POST "/home-login" [nis pass]
       (handle-login nis pass))

  (GET "/home-logout" []
       (share/logout "/"))

  (POST "/home-logout" []
        (share/logout "/"))

  (GET "/registrasi-siswa" []
       (home-registrasi-siswa))
  (POST "/registrasi-siswa" [nis nama kelas email pass1 pass2]
        (handle-reg-siswa nis nama kelas email pass1 pass2))

  (POST "/home-no-lstore" []
        (layout/render "home/kode1.html"))
  (POST "/home-input-kode" []
        (layout/render "home/kode1.html"))

  (GET "/home-input-nomer" []
        (layout/render "home/nomer.html"))
  (POST "/home-kodebahas" [kodebahas]
        (home-kodebahas kodebahas))

  (GET "/home-ganti-pw" []
        (layout/render "home/ganti-pw.html"))
  (POST "/home-ganti-pw1" [pwlama pwbaru1 pwbaru2]
        (home-ganti-pw-siswa pwlama pwbaru1 pwbaru2))

  (GET "/home-lihat-hasil" []
        (handle-lihat-hasil (session/get :id) "Selamat Datang "))

  (POST "/home-detail-set" [kode]
        (handle-detail-set kode (session/get :id)))

  (POST "/home-lstore" []
        (layout/render "home/kode2.html"))

  (POST "/home-kodeto" [kodeto]
        (handle-kodeto1 kodeto))

    (POST "/home-tryout-lanjutan" [kode]
        (handle-to-lanjutan kode))

  (POST "/home-tryout-baru" []
        (layout/render "home/kode1.html"))

  (GET "/home-sim-sbmptn" []
        (home-pilih-paket))
  (POST "/home-proses-paket" [kode]
        (home-proses-paket kode))
  (POST "/home-next-univ" [ptn ntot ntkpa nipaorips kelompok]
        (home-next-univ ptn (read-string ntot) ntkpa nipaorips kelompok))

  (GET "/home-sim-ppdb" []
        (home-pilih-paket-ppdb))
  (POST "/home-proses-paket-ppdb" [kode]
        (home-proses-paket-ppdb kode))
  (POST "/home-next-daerah" [kode ntot nmat nipa nind ning]
        (home-next-daerah kode ntot nmat nipa nind ning))

  (GET "/simpan/:kode/:jawaban/:nis" [kode jawaban nis]
       ;(println (str kode " " jawaban))
      (resp/json (handle-simpan-jawaban kode jawaban nis)))

  (GET "/get-sekolah/:kode/:ntot" [kode ntot]
       (resp/json (home-get-sekolah kode ntot)))

  (GET "/get-jurusan/:ptn/:kelompok/:ntot" [ptn kelompok ntot]
       (resp/json (home-get-jurusan ptn kelompok ntot)))

)
