(ns haskell-bazaar-frontend.views
  (:require
    [re-frame.core :as re-frame]
    [haskell-bazaar-frontend.routes :as routes]))

;; TODO: move to utils or js interop
(defn target-value [e]
  (-> e .-target .-value))

(defn search-box-button [on-click]
  (let [search-loading (re-frame/subscribe [:search-loading])]
    [:button.search-box-button
     {:type "button"
      :on-click on-click
      :disabled @search-loading}
     (if @search-loading "Loading" "Search")]))

(defn search-box
  [{:keys [on-click-factory on-change on-key-down]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div.search-box
     [:input.search-box-input
      {:type "text"
       :autoComplete "off"
       :value @search-query
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
  {:filters
   {:on-click-factory
    (fn [showing-kw]
      (fn [e]
        (re-frame/dispatch [:set-showing showing-kw])))}

   :search-box
   {:on-change
    (fn [e]
      (re-frame/dispatch [:set-search-query (target-value e)]))

    :on-key-down
    (fn [e]
      (when (= (:enter keystrokes) (.-which e))
        (re-frame/dispatch [:navigate-search (target-value e)])))

    :on-click-factory
    (fn [search-query]
      (fn [e] (re-frame/dispatch [:navigate-search search-query])))

    }})

(defn item-tag [{:keys [name]}]
  [:span name])

(defn item-tags [tags]
  (->> tags
       distinct
       (sort <)
       (mapv (fn [tag] [item-tag tag]))
       (interpose ", ")
       (into [:span [:span.item-section "tags: "]])))

(defn item-author [{:keys [firstName lastName uuid]}]
  [:span (str firstName " " lastName)])

(defn item-authors [authors]
  (let [title (if (> (count (distinct authors)) 1) "authors" "author")]
  (->> authors
       distinct
       (sort <)
       (mapv (fn [author] [item-author author]))
       (interpose ", ")
       (into [:span [:span.item-section (str title ": ")]]))))

;; TODO: use icons instead?
(defn item-type [t]
  [:span
   [:span.item-section "Type: "]
   (case t
     "Video" "video"
     "")])

(defn search-result-item
  [{:keys [uuid url authors title type description tags]}]
  [:ul.item
   [:li.title title]
   [:li.description description]
   [:li.authors [item-authors authors]]
   [:li.tags [item-tags tags]]
   [:li.type [item-type type]]])

(defn search-results-list
  []
  (let [items (re-frame/subscribe [:items])]
    (->> @items
         vals
         (mapv (fn [item] [:li ^{:key (:uuid item)}
                           [search-result-item item]]))
         (into [:ul]))))

(defn search-filters-item
  [on-click title showing-kw showing]
  (if (= showing-kw showing)
    [:li.active title]
    [:li {:on-click on-click} title]))

(defn search-filters [{:keys [on-click-factory]}]
  (let [showing (re-frame/subscribe [:showing])]
    (->> routes/filters
         (mapv (fn [[title showing-kw]]
                 [search-filters-item
                  (on-click-factory showing-kw)
                  title
                  showing-kw
                  @showing]))
         (into [:ul.search-filters]))))

(defn ui [dispatchers]
  [:div
   [:div.topnav
    [search-box (:search-box dispatchers)]
    [:div.bar
     [search-filters (:filters dispatchers)]]]
   [:div.results
    [search-results-list]]])
