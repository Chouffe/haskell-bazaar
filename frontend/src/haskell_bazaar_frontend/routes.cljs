(ns haskell-bazaar-frontend.routes
  (:require-macros
    [secretary.core :refer [defroute]])
  (:require
    [goog.events :as events]
    [clojure.string :as string]
    [re-frame.core :as re-frame]
    [secretary.core :as secretary]
    [haskell-bazaar-frontend.db :as db])
  (:import
    [goog History]
    [goog.history EventType]))

(defn set-config! []
  (secretary/set-config! :prefix "#"))

(defroute "/" [_]
  (re-frame/dispatch [:tab :landing-page])
  (re-frame/dispatch [:set-search-query nil]))

(defroute "/search" [query-params]
  (let [{:keys [q tab]} query-params]
    ;; TODO: sanitize `q`
    (when-not (string/blank? q)
      (re-frame/dispatch [:set-search-query q])
      (re-frame/dispatch [:tab :search])
      (re-frame/dispatch [:datascript/search-later {:q q :ms 400}]))))

(defn make-history!
  "Returns a Goog.History object"
  []
  (doto (History.)
    (events/listen EventType.NAVIGATE
                   (fn [event] (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn nav! [history token]
  (.setToken history token))
