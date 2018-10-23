(ns haskell-bazaar-frontend.fx
  (:require
    [re-frame.core :as re-frame]
    [haskell-bazaar-frontend.routes :as routes]))

(defn init! [history cache]

  ;; TODO: add a TTL mechanism here
  (re-frame/reg-cofx
    :local-store
    (fn [coeffects local-store-key]
      (assoc coeffects
             :local-store
             (js->clj (.getItem js/localStorage local-store-key)))))

  ;; TODO: add a TTL mechanism here
  (re-frame/reg-fx
    :local-store
    (fn [[local-store-key value]]
      (js->clj (.setItem js/localStorage local-store-key value))))

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
