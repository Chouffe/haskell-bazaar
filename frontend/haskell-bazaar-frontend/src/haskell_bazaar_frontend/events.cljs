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
      (seq local-store) {:datascript/transact local-store}
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
  :api-feedback
  interceptors
  (fn [{:keys [db]} [_ message]]
    {:http-xhrio
     {:method :post
      :params {:message message}
      :uri (api/feedback (api/base-url (:environment db)))
      :format (ajax/json-request-format)
      :response-format api/response-format
      :on-success [:api-feedback-success]
      :on-failure [:api-feedback-failure]}}))

(re-frame/reg-event-fx
  :datascript/search
  [(re-frame/inject-cofx :datascript) interceptors]
  (fn [{:keys [datascript db]} [_ search-query]]
    (when-not (string/blank? search-query)
      (let [search-item-results (->> search-query
                                     (ds/search datascript)
                                     utils/uuid-coll->hashmap)]
        {:db (assoc db
                    :search-items search-item-results
                    :search-loading false)}))))

(re-frame/reg-event-fx
  :datascript/search-later
  [interceptors]
  (fn [_ [_ {:keys [q ms]}]]
    {:dispatch-later [{:ms ms :dispatch [:datascript/search q]}]}))

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
    {:db (assoc db :tab :search)
     :navigate (str "/search?q=" search-query)}))

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

(re-frame/reg-event-fx
  :hljs/code-block
  interceptors
  (fn [_ [_ dom-node]]
    {:highlight-code-block dom-node}))

;; Modals
(re-frame/reg-event-db
  :modal/close
  interceptors
  (fn [db _]
    (dissoc db :modal)))

(re-frame/reg-event-db
  :modal/open
  interceptors
  (fn [db [_ modal-kw]]
    (assoc db :modal modal-kw)))

(re-frame/reg-event-fx
  :ui/focus
  interceptors
  (fn [_ [_ query-selector]]
    {:ui/focus query-selector}))

(re-frame/reg-event-db
  :tab
  interceptors
  (fn [db [_ tab-kw]]
    (assoc db :tab tab-kw)))

(re-frame/reg-event-fx
  :analytics/page-view
  (fn [db [_ {:keys [title path] :as params}]]
    {:gtag/page-view params}))

(re-frame/reg-event-fx
  :analytics/event
  (fn [db [_ {:keys [action category label value] :as params}]]
    {:gtag/event params}))
