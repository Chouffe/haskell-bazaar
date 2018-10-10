(ns haskell-bazaar-frontend.events
  (:require
    [cljs.spec.alpha :as s]

    [ajax.core :as ajax]
    [day8.re-frame.http-fx]   ;; Register the http-xhrio effect handler
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.db :as db]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]))

;; Interceptors

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db)) {}))))

;; now we create an interceptor using `after`
(def check-spec-interceptor (re-frame/after (partial check-and-throw :haskell-bazaar-frontend.db/app-state)))

(def interceptors
  [(when goog.DEBUG check-spec-interceptor)
   (when goog.DEBUG re-frame/debug)])

(re-frame/reg-event-db
  :initialize-db
  interceptors
  (fn [db _]
    db/default-db))

(re-frame/reg-event-fx
  :api-keywords
  interceptors
  (fn [{:keys [db]} _]
    {:http-xhrio
     {:method :get
      :uri (api/keywords (api/base-url (:environment db)))
      :response-format api/response-format
      :on-success [:api-keywords-success]
      :on-failure [:api-keywords-failure]}}))

;; TODO: define higher order functions for handling the caching
(re-frame/reg-event-fx
  :api-search
  [(re-frame/inject-cofx :cache) interceptors]
  (fn [{:keys [db cache]} [_ search-query]]
    (when-not (get cache search-query)
      {:db (assoc db :search-loading true)
       :http-xhrio
       {:method :get
        :uri (api/search (api/base-url (:environment db)) search-query)
        :response-format api/response-format
        :on-success [:api-search-success search-query]
        :on-failure [:api-search-failure]}})))

(re-frame/reg-event-fx
  :api-search-success
  interceptors
  (fn [{:keys [db]} [_ search-query results]]
    {:cache {:k search-query :v results}
     :db (-> db
             (assoc :search-loading false)
             (update :items #(merge % (utils/uuid-coll->hashmap results))))}))

(re-frame/reg-event-db
  :api-keywords-success
  interceptors
  (fn [db [_ results]]
    (assoc db :keywords results)))

(re-frame/reg-event-db
  :api-keywords-failure
  interceptors
  (fn [db [_ results]]
    ;; TODO: handle more gracefully
    (.log js/console "ERROR Loading keywords!!")
    db))

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

(re-frame/reg-event-fx
  :navigate-search
  interceptors
  (fn [{:keys [db]} [_ search-query]]
    ;; TODO: url encode the search query?
    {:navigate (str "/search"
                    "?q=" search-query
                    "&tab=" (name (:showing db)))}))

(re-frame/reg-event-fx
  :navigate
  interceptors
  (fn [_ [_ url]]
    {:navigate url}))

(re-frame/reg-event-fx
  :set-showing
  interceptors
  (fn [{:keys [db]} [_ showing-kw]]
    {:db (assoc db :showing showing-kw)
     :dispatch [:navigate-search (:search-query db)]}))
