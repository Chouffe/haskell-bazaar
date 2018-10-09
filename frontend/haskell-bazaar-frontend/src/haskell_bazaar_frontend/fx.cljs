(ns haskell-bazaar-frontend.fx
  (:require
    [re-frame.core :as re-frame]
    [haskell-bazaar-frontend.routes :as routes]))

(defn init! [history]
  (re-frame/reg-fx
    :navigate
    (fn [url]
      (routes/nav! history url))))
