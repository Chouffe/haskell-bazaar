(ns haskell-bazaar-frontend.fx
  (:require
    [re-frame.core :as re-frame]
    [haskell-bazaar-frontend.routes :as routes]))

(defn init! [history cache]

  (re-frame/reg-fx
    :navigate
    (fn [url]
      (routes/nav! history url)))

  (re-frame/reg-cofx
    :cache
    (fn [coeffects _]
      (assoc coeffects :cache @cache)))

  (re-frame/reg-fx
    :cache
    (fn [{:keys [k v]}]
      (swap! cache assoc k v))))
