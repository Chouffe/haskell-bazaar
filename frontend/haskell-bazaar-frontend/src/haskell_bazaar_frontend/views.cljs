(ns haskell-bazaar-frontend.views
  (:require [re-frame.core :as re-frame]))

;; TODO: move to utils or js interop
(defn target-value [e]
  (-> e .-target .-value))

(defn search-box-button [on-click]
  (let [search-loading (re-frame/subscribe [:search-loading])]
    [:button {:type "button"
              :on-click on-click
              :disabled @search-loading}
     (if @search-loading "Loading" "Search")]))

(defn search-box
  [{:keys [on-click-factory on-change on-key-down]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div
     [:input {:type "text"
              :on-change on-change
              :on-key-down on-key-down}]
     [search-box-button (on-click-factory @search-query)]]))

;; Move to utils
(def keystrokes
  {:enter 13
   :esc   27})

;; TODO: have different dispatchers for test environment for instance
;; Super easy to mock
(def dispatchers
  {:search-box
   {:on-change
    (fn [e] (re-frame/dispatch [:set-search-query (target-value e)]))

    :on-key-down
    (fn [e]
      (when (= (:enter keystrokes) (.-which e))
        (re-frame/dispatch [:api-search (target-value e)])))

    :on-click-factory
    (fn [search-query]
      (fn [e] (re-frame/dispatch [:api-search search-query])))

    }})

(defn item-tag [{:keys [name]}]
  [:strong name])

(defn item-tags [tags]
  (->> tags
       distinct
       (sort <)
       (mapv (fn [tag] [item-tag tag]))
       (interpose ", ")
       (into [:span "tags: "])))

(defn item-author [{:keys [firstName lastName uuid]}]
  [:span (str firstName " " lastName)])

(defn item-authors [authors]
  (->> authors
       distinct
       (sort <)
       (mapv (fn [author] [item-author author]))
       (interpose ", ")
       (into [:span])))

;; TODO: use icons instead?
(defn item-type [t]
  (case t
    "Video" "video"
    ""))

(defn search-result-item
  [{:keys [uuid url authors title type description tags]}]
  [:span
   title
   [:br]
   [item-authors authors]
   [:br]
   [:span description]
   [:br]
   [item-tags tags]
   [:br]
   [item-type type]])

(defn search-results-list
  []
  (let [items (re-frame/subscribe [:items])]
    [:ul
     (->> (vals @items)
          (mapv (fn [item] [:li ^{:key (:uuid item)}
                            [search-result-item item]]))
          (into [:ul]))]))

(defn search-filters []
  [:ul
   [:li "All"]
   [:li "Videos"]
   [:li "Papers"]
   [:li "Blog Post"]
   [:li "Tutorials"]])

(defn ui [dispatchers]
  [:div
   [search-box (:search-box dispatchers)]
   [search-filters]
   [search-results-list]])
