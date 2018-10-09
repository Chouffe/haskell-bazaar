(ns haskell-bazaar-frontend.routes
  (:require-macros
    [secretary.core :refer [defroute]])
  (:require
    [goog.events :as events]
    [re-frame.core :as re-frame]
    [secretary.core :as secretary]
    [haskell-bazaar-frontend.db :as db])
  (:import
    [goog History]
    [goog.history EventType]))

(defn set-config! []
  (secretary/set-config! :prefix "#"))

(defroute "/search" [query-params]
  (let [{:keys [q tab]} query-params]
    ;; TODO: sanitize `q`
    (when q
      (re-frame/dispatch [:set-search-query q])
      (re-frame/dispatch [:api-search q]))
    (when-let [showing-kw (some-> tab keyword)]
      (re-frame/dispatch [:set-showing showing-kw]))))

(defn make-history!
  "Returns a Goog.History object"
  []
  (doto (History.)
    (events/listen EventType.NAVIGATE
                   (fn [event] (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

(defn nav! [history token]
  (.setToken history token))

(def filters
  (mapv
    (fn [title] [title (db/title->showing title)])
    ["all" "videos" "papers" "blog posts" "tutorials"]))
