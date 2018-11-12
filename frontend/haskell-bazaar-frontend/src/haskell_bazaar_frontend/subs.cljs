(ns haskell-bazaar-frontend.subs
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-sub
  :search-query
  (fn [db _]
    (:search-query db)))

(re-frame/reg-sub
  :search-loading
  (fn [db _]
    (:search-loading db)))

(re-frame/reg-sub
  :items
  (fn [db _]
    (:items db)))

(re-frame/reg-sub
  :autocomplete-tags
  (fn [db _]
    (->> db
         :search-items
         vals
         (mapcat #(->> % :tags (map :name)))
         )))

(re-frame/reg-sub
  :search-items
  (fn [db _]
    (:search-items db)))

(re-frame/reg-sub
  :showing
  (fn [db _]
    (:showing db)))
