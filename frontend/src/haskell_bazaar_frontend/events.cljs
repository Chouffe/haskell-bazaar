(ns haskell-bazaar-frontend.events
  (:require
    [clojure.string :as string]
    [cljs.spec.alpha :as s]

    [ajax.core :as ajax]
    [datascript.core :as d]
    [day8.re-frame.http-fx]   ;; Register the http-xhrio effect handler
    [re-frame.core :as re-frame]

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

;; Google Analytics Interceptor

(defn effects->gtag [{:keys [http-xhrio navigate] :as effects}]
  (cond
    (not (nil? http-xhrio))
    (let [{:keys [method uri]} http-xhrio]
      {:gtag/event {:action "http-xhrio"
                    :category (name method)
                    :label uri
                    :value uri}})
    :else {}))

;; TODO: spec it out
(defn tab->path [tab]
  (get {:landing-page "/" :search "/search"} tab "/"))

;; TODO: spec it out
(defn modal->path [modal]
  (get {:feedback "/modal-feedback"
        :mailing-list "/modal-mailing-list"
        :donate "/modal-donate"
        } modal "/"))

(defn coeffects->gtag [{:keys [event] :as coeffects}]
  (let [[event-kw & args] event]
    (case event-kw
      :navigate-search
      (let [tab-kw :search]
        {:gtag/page-view {:title (name tab-kw) :path (tab->path tab-kw)}})

      :navigate
      (let [[path _] args]
        {:gtag/page-view {:title path :path path}})

      :tab
      (let [[tab-kw _] args]
        {:gtag/page-view {:title (name tab-kw) :path (tab->path tab-kw)}})

      :modal/open
      (let [[modal-kw _] args]
        {:gtag/page-view {:title (str "modal-open-" (name modal-kw))
                          :path (modal->path modal-kw)}})

      {})))

(def gtrack
  (re-frame.core/->interceptor
    :id      :gtrack
    :after   (fn [{:keys [coeffects effects] :as context}]
               (->> (effects->gtag effects)
                    (merge (coeffects->gtag coeffects))
                    (update context :effects merge)))))

(def interceptors
  [(when-not ^boolean goog.DEBUG gtrack)              ;; Track in :prod
   (when ^boolean goog.DEBUG check-spec-interceptor)  ;; Check Spec in :dev
   (when ^boolean goog.DEBUG re-frame/debug)])        ;; Debug in :dev

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
      ; (seq local-store) {:datascript/transact local-store}
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
  :api-mailing-list-subscribe
  interceptors
  (fn [{:keys [db]} [_ email]]
    {:http-xhrio
     {:method :post
      :params {:email_address email}
      :uri (api/mailing-list-subscribe (api/base-url (:environment db)))
      :format (ajax/json-request-format)
      :response-format api/response-format
      :on-success [:api-mailing-list-subscribe-success]
      :on-failure [:api-mailing-list-subscribe-failure]}}))

(re-frame/reg-event-fx
  :datascript/search
  [(re-frame/inject-cofx :datascript) interceptors]
  (fn [{:keys [datascript db]} [_ search-query]]
    (when-not (or (string/blank? search-query)
                  (< (count search-query) 2))
      (let [search-item-results (->> search-query
                                     (ds/search datascript)
                                     utils/uuid-coll->hashmap)]
        {:http-xhrio
         {:method :get
          :uri (api/search (api/base-url (:environment db)) search-query)
          :format (ajax/json-request-format)
          :response-format api/response-format
          :on-success [:datascript-search-success]
          :on-failure [:datascript-search-failure]}
         :db (assoc db
                    :search-items search-item-results
                    :search-loading false)}))))

(re-frame/reg-event-fx
  :datascript-search-success
  [interceptors]
  (fn [_ _]
    {}))

(re-frame/reg-event-fx
  :datascript-search-failure
  [interceptors]
  (fn [_ _]
    {}))

(re-frame/reg-event-fx
  :datascript/search-later
  [interceptors]
  (fn [_ [_ {:keys [q ms]}]]
    {:scroll-to [0 0]
     :dispatch-later [{:ms ms :dispatch [:datascript/search q]}]}))

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
      ;; TODO: remove search loading
      {:db (assoc db :search-loading false)
       :local-store ["datascript-facts" all-facts]
       :dispatch-later [{:ms 100 :dispatch [:search-populate-source]}]
       :datascript/transact all-facts})))

(re-frame/reg-event-fx
  :api-items-failure
  (fn [{:keys [db]} results]
    (.log js/console "ERROR Loading keywords!!")
    {}))

(defn enriched-result-item->source-item [[k {:keys [title]}]]
  {:description title :title k})

(re-frame/reg-event-fx
  :search-populate-source
  [(re-frame/inject-cofx :datascript) interceptors]
  (fn [{:keys [datascript db]} _]
    (let [enriched-results nil #_(mapv enriched-result-item->source-item (:search-enriched-results db))]
      {:db (->> datascript
                ((juxt ds/all-tags-names ds/all-authors-full-names))
                flatten
                distinct
                (mapv (fn [s] {:title s}))
                (assoc db :search-source))})))

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

(re-frame/reg-event-fx
  :ui/blur
  interceptors
  (fn [_ [_ query-selector]]
    {:ui/blur query-selector}))

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

;; Tracks
(re-frame/reg-event-db
  :track/select
  interceptors
  (fn [db [_ track-kw selection-kw]]
    (assoc-in db [:tracks track-kw :track-selection] selection-kw)))
