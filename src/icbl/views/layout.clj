(ns icbl.views.layout
  (:require [noir.session :as session]
            [selmer.parser :as parser]
            [ring.util.response :refer [content-type response]]
            [compojure.response :refer [Renderable]]))


(def template-folder "icbl/views/templates/")

(defn utf-8-response [html]
    (content-type (response html) "text/html; charset=utf-8"))


(deftype RenderablePage [template params]
  Renderable
  (render [this request]
    (->> (assoc params
          :context (:context request)
          :user (session/get :id)
          :nama (session/get :nama)
          :status (session/get :status)
          :path (str "http://" (session/get :ip) "/resources/public")
           )
        (parser/render-file (str template-folder template))
        utf-8-response)))

(defn render [template & [params]]
    (RenderablePage. template params))
