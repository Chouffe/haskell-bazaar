(ns haskell-bazaar-frontend.events
  (:require
    [re-frame.core :as re-frame]
    [day8.re-frame.http-fx]   ;; Register the http-xhrio effect handler
    [ajax.core :as ajax]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.db :as db]
    [haskell-bazaar-frontend.utils :as utils]))

(def interceptors
  [(when goog.DEBUG re-frame/debug)])

(re-frame/reg-event-db
  :initialize-db
  interceptors
  (fn [db _]
    db/default-db))

(re-frame/reg-event-fx
  :api-keywords
  interceptors
  (fn [{:keys [db]} _]
    {:db db
     :http-xhrio
     {:method :get
      :uri (api/keywords (api/base-url (:environment db)))
      :response-format api/response-format
      :on-success [:api-keywords-success]
      :on-failure [:api-keywords-failure]}}))

(re-frame/reg-event-fx
  :api-search
  interceptors
  (fn [{:keys [db]} [_ search-query]]
    {:db (assoc db :search-loading true)
     :http-xhrio
     {:method :get
      :uri (api/search (api/base-url (:environment db)) search-query)
      :response-format api/response-format
      :on-success [:api-search-success]
      :on-failure [:api-search-failure]}}))

(re-frame/reg-event-db
  :api-search-success
  interceptors
  (fn [db [_ results]]
    (-> db
        (assoc :search-loading false)
        (update :items #(merge % (utils/uuid-coll->hashmap results))))))

(re-frame/reg-event-db
  :api-keywords-success
  interceptors
  (fn [db [_ results]]
    (assoc db :keywords results)))

(re-frame/reg-event-db
  :api-keywords-failure
  interceptors
  (fn [db [_ results]]
    (.log js/console "ERROR Loading keywords!!")))

(re-frame/reg-event-db
  :api-search-failure
  interceptors
  (fn [db [_ results]]
    (-> db
        (assoc :search-error results)
        (assoc :search-loading false))))

(re-frame/reg-event-db
  :set-search-query
  interceptors
  (fn [db [_ new-search-query]]
    (assoc db :search-query new-search-query)))
