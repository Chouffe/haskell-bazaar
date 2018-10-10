(ns haskell-bazaar-frontend.fx
  (:require
    [re-frame.core :as re-frame]
    [haskell-bazaar-frontend.routes :as routes]))

(defn init! [history]
  (re-frame/reg-fx
    :navigate
    (fn [url]
      (routes/nav! history url))))

;; TODO: move up the cache atom
(let [cache (atom {})]

  (re-frame/reg-cofx
    :cache
    (fn [coeffects _]
      (assoc coeffects :cache @cache)))

  (re-frame/reg-fx
    :cache
    (fn [{:keys [k v]}]
      (swap! cache assoc k v)
      (.log js/console "Performing side cache side effect!")
      (.log js/console k)
      (.log js/console v)
      (.log js/console @cache))))
