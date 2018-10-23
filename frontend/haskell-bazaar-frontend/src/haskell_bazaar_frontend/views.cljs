(ns haskell-bazaar-frontend.views
  (:require
    [re-frame.core :as re-frame]
    [clojure.string :as string]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]))

(defn search-box-button [on-click]
  (let [search-loading (re-frame/subscribe [:search-loading])]
    [:button.search-box-button
     {:type "button"
      :on-click on-click
      :disabled @search-loading}
     (if @search-loading
       [:i.fa.fa-spinner]
       [:i.fa.fa-search])]))

(defn search-box
  [{:keys [on-click-factory on-change on-key-up]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div.search-box
     [:input.search-box-input
      {:type "text"
       :autoComplete "off"
       :name "search-box"
       :value @search-query
       :on-change on-change
       :on-key-up on-key-up}]

     [search-box-button (on-click-factory @search-query)]]))

;; Move to utils
(def keystrokes
  {:enter 13
   :esc   27})

;; TODO: have different dispatchers for test environment for instance
;; Very easy to mock
(def dispatchers
  {:filters
   {:on-click-factory
    (fn [showing-kw]
      (fn [e]
        (re-frame/dispatch [:set-showing showing-kw])))}

   :search-box
   {:on-change
    (fn [e]
      (re-frame/dispatch [:set-search-query (utils/target-value e)])
      ;; TODO: add a delay or something
      (re-frame/dispatch [:datascript/search (utils/target-value e)])
      )

    :on-key-up
    (fn [e]
      (when (= (:enter keystrokes) (.-which e))
        (re-frame/dispatch [:navigate-search (utils/target-value e)])))

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
     :Video "video"
     "")])

(defn search-result-item
  [base-url {:keys [uuid authors title type description tags]}]
  [:ul.item
  [:li.actions
   [:ul.actions-list
    [:li.action-link
     [:a {:target "_blank"
          :href (api/item-url base-url uuid)}
      [:i.fa.fa-link]]]
    ;; TODO: save button to local storage
    #_[:li.action-save
     [:i.fa.fa-heart-o.fa]]]] [:li.title title]
   [:li.description description]

   [:li.authors [item-authors authors]]
   [:li.tags [item-tags tags]]
   [:li.type [item-type (:item-type type)]]])

(defn- filter-items [showing-kw]
  (if (= showing-kw :all)
    identity
    (partial filter
             (fn [{:keys [type]}] (= (string/lower-case (name (:item-type type)))
                                     (name showing-kw))))))

(defn search-results-list
  [base-url]
  (let [items (re-frame/subscribe [:search-items])
        showing (re-frame/subscribe [:showing])]
    (->> @items
         vals
         ((filter-items @showing))
         (mapv (fn [item] [:li.search-container-item
                           ^{:key (:uuid item)}
                           [search-result-item base-url item]]))
         (into [:ul.search-container]))))

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

(defn ui [dispatchers base-url]
  [:div
   [:div.topnav
    [search-box (:search-box dispatchers)]
    [:div.bar
     [search-filters (:filters dispatchers)]]]
   [:div.results
    [search-results-list base-url]]])
