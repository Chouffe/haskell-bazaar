(ns haskell-bazaar-frontend.analytics
  (:require
    [haskell-bazaar-frontend.utils :as utils]))

;; TODO: read from environment variable or from the backend or something
(def tracking-id "UA-129140929-1")

;; TODO: spec it out
(defn page-view! [{:keys [title path]}]
  (let [params (clj->js {:page_title title :page_path path})]
    (.gtag js/window "config" tracking-id params)))

;; TODO: spec it out
(defn event! [{:keys [action category label value]}]
  (let [params (clj->js (utils/remove-nils {:event_category category
                                            :event_label label
                                            :value value}))]
    (.gtag js/window "event" action params)))

; (page-view {:title "search" :path "/search"})
; (event {:action "search"
;         :category "haskell"
;         :label "label"
;         :value "monad"})
