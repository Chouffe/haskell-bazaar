(ns haskell-bazaar-frontend.fx
  "collection of `fx` and `cofx` for the frontend.
  it allows to use local-storage, use Browser History, Browser Navigation,
  caching mechanism, ..."
  (:require
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.local-storage :as local-storage]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]))

(defn init!
  "initialize effects and coeffects for the following effectful actions
  * Browser Navigation
  * API caching
  * Local Storage

  `history`: Goog.History object
  `cache`: atom for memoizing api calls and store return values
  `local-storage-ttl`: integer that represents time in seconds for the
  local storage key to expire"
  [history cache local-storage-ttl]

  (re-frame/reg-cofx
    :local-store
    (fn [coeffects local-store-key]
      (let [local-store-value (local-storage/get-item local-store-key)
            {:keys [value expiration-date]} (cljs.reader/read-string local-store-value)
            now (js/Date.)]

        (cond
          (> expiration-date now) (assoc coeffects :local-store value)

          local-store-value (do
                              (local-storage/remove-item! local-store-key)
                              coeffects)

          :else coeffects))))

  (re-frame/reg-fx
    :local-store
    (fn [[local-store-key value]]
      (->> {:value value
	    :expiration-date (utils/add-seconds local-storage-ttl)}
	   pr-str
	   (local-storage/set-item! local-store-key))))

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

(re-frame/reg-fx
  :highlight-code-block
  (fn [dom-node]
    (.highlightBlock js/hljs dom-node)))
