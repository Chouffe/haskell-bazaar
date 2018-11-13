(ns haskell-bazaar-frontend.analytics)

(def tracking-id "UA-129140929-1")

(defn page-view! [{:keys [title path]}]
  (let [params (clj->js {:page_title title :page_path path})]
    (.gtag js/window "config" tracking-id params)))

(defn event! [{:keys [action category label value]}]
  (let [params (clj->js {:event_category category
                         :event_label label
                         :value value})]
    (.gtag js/window "event" action params)))

; (page-view {:title "search" :path "/search"})
; (event {:action "search"
;         :category "haskell"
;         :label "label"
;         :value "monad"})
