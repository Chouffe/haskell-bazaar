(ns haskell-bazaar-frontend.analytics
  "Google Analytics utils"
  (:require
    [haskell-bazaar-frontend.utils :as utils]))

;; TODO: read from environment variable or from the backend or something
;; TODO: make it private
(def tracking-id "UA-129140929-1")

;; TODO: spec it out
(defn page-view!
  "send a page-view hit to google analytics
  >>> (page-view {:title \"search\" :path \"/search\"})
  "
  [{:keys [title path]}]
  (let [params (clj->js {:page_title title :page_path path})]
    (.gtag js/window "config" tracking-id params)))

;; TODO: spec it out
(defn event!
  "send an event to google analytics
  >>> (event {:action \"search\" :category \"haskell\" :label \"label\" :value \"monad\"})"
  [{:keys [action category label value]}]
  (let [params (clj->js (utils/remove-nils {:event_category category
                                            :event_label label
                                            :value value}))]
    (.gtag js/window "event" action params)))
