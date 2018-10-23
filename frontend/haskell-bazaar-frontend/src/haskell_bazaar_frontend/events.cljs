(ns haskell-bazaar-frontend.events
  (:require
    [cljs.spec.alpha :as s]
    [clojure.string :as string]

    [ajax.core :as ajax]
    [day8.re-frame.http-fx]   ;; Register the http-xhrio effect handler
    [re-frame.core :as re-frame]
    [datascript.core :as d]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.db :as db]
    [haskell-bazaar-frontend.ds :as ds]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.stubs :as stubs]
    [haskell-bazaar-frontend.utils :as utils]))

;; Interceptors

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db)) {}))))

;; now we create an interceptor using `after`
(def check-spec-interceptor
  (re-frame/after
    (partial check-and-throw :haskell-bazaar-frontend.db/app-state)))

(def interceptors
  [(when goog.DEBUG check-spec-interceptor)
   (when goog.DEBUG re-frame/debug)])

(re-frame/reg-event-db
  :db/initialize
  interceptors
  (fn [db [_ env]]
    (db/default-db env)))

(re-frame/reg-event-fx
  :datascript/initialize
  [(re-frame/inject-cofx :local-store "datascript-facts") interceptors]
  (fn [{:keys [local-store] :as coeffects} [_ env]]
    (cond
      ;; Always fetch api-items in dev mode
      (= env :dev)      {:dispatch [:api-items]}
      (seq local-store) {:datascript/transact (cljs.reader/read-string local-store)}
      :else             {:dispatch [:api-items]})))

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

(re-frame/reg-event-fx
  :datascript/search
  [(re-frame/inject-cofx :datascript) interceptors]
  (fn [{:keys [datascript db]} [_ search-query]]
    (let [search-item-results (->> search-query
                                   (ds/search datascript)
                                   utils/uuid-coll->hashmap)]
      {:db (-> db
               (assoc :search-loading false)
               (assoc :search-items search-item-results))})))

;; TODO: define higher order functions for handling the caching
(re-frame/reg-event-fx
  :api-search
  [(re-frame/inject-cofx :cache) interceptors]
  (fn [{:keys [db cache]} [_ search-query]]
    (when-not (and (string/blank? search-query) (get cache search-query))
      {:db (assoc db :search-loading true)
       :http-xhrio
       {:method :get
        :uri (api/search (api/base-url (:environment db)) search-query)
        :response-format api/response-format
        :on-success [:api-search-success search-query]
        :on-failure [:api-search-failure]}})))

(re-frame/reg-event-fx
  :api-items
  [interceptors]
  (fn [{:keys [db]} _]
    {:db (assoc db :search-loading true)
     :http-xhrio
     {:method :get
      :uri (-> db :environment api/base-url api/items)
      :response-format api/response-format
      :on-success [:api-items-success]
      :on-failure [:api-items-failure]}}))

(re-frame/reg-event-fx
  :api-items-success
  (fn [{:keys [db]} [_ results]]
    (let [all-facts (->> results
                         (map ds/item->facts)
                         flatten
                         distinct)]
      {:db (assoc db :search-loading false)
       :local-store ["datascript-facts" all-facts]
       :datascript/transact all-facts})))

(re-frame/reg-event-fx
  :api-items-failure
  (fn [{:keys [db]} results]
    (.log js/console "ERROR Loading keywords!!")
    {}))

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
