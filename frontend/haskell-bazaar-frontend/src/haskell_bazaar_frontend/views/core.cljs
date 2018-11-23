(ns haskell-bazaar-frontend.views.core
  (:require
    [clojure.string :as string]

    [reagent.core :as reagent]
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.ui :as ui]
    [haskell-bazaar-frontend.views.modal :as modal]))


;; TODO: have different dispatchers for test environment for instance
(def dispatchers
  {:filters
   {:on-click-factory
    (fn [showing-kw]
      (fn [e]
        (re-frame/dispatch [:set-showing showing-kw])))}

   :landing-page-search
   {:on-submit
    (fn [search-query]
      (fn [e]
        (.preventDefault e)
        (.stopPropagation e)
        (re-frame/dispatch [:navigate-search @search-query])))

    :onResultSelect
    (fn [event data]
      (let [selected-result (get-in (js->clj data) ["result" "title"])]
        (re-frame/dispatch [:navigate-search selected-result])))

    ; :onStoppedTyping
    ; #(re-frame/dispatch [:datascript/search %])

    :onSearchChange
    #(re-frame/dispatch [:set-search-query (utils/target-value %)])}

   :search
   {:onResultSelect
    (fn [event data]
      (let [selected-result (get-in (js->clj data) ["result" "title"])]
        (re-frame/dispatch [:datascript/search selected-result])
        (re-frame/dispatch [:set-search-query selected-result])))

    :onStoppedTyping
    #(re-frame/dispatch [:datascript/search %])

    :onSearchChange
    #(re-frame/dispatch [:set-search-query (utils/target-value %)])
    }})

(defn item-tag [{:keys [name]}]
  (into [:span]
        (->> (string/split name #" ")
             (string/join "-")
             (str "#"))))

(defn item-tags [tags]
  (when (seq tags)
    (->> tags
         distinct
         (sort <)
         (mapv (fn [tag] [item-tag tag]))
         (interpose " ")
         (into [:span.tags]))))

(defn item-author [{:keys [firstName lastName uuid]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    (utils/highlight-string-match @search-query (str firstName " " lastName))))

(defn item-description [description]
  (let [search-query (re-frame/subscribe [:search-query])]
    (let [truncated-description (utils/trunc description 500)]
      [:span.md.description
       (if (= description truncated-description)
         (utils/highlight-string-match @search-query description)
         (utils/highlight-string-match @search-query (str truncated-description "...")))])))

(defn item-authors [authors]
  (when (seq authors)
    (->> authors
         distinct
         (sort <)
         (mapv (fn [author] [item-author author]))
         (interpose ", ")
         (into [:span.authors]))))

(defn search-result-item
  [base-url {:keys [uuid authors title type description tags]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:li.search-item
     [:a.lg {:target "_blank" :href (api/item-url base-url uuid @search-query)}
      (utils/highlight-string-match @search-query title)]
     (when (seq authors)
       [:span.md
        (str " ‧ ")
        [item-authors authors]])
     [:br]
     [item-description description]
     [:br]
     [item-tags tags]]))

(defn- filter-items [showing-kw]
  (if (= showing-kw :all)
    identity
    (partial filter
             (fn [{:keys [type]}] (= (string/lower-case (name (:item-type type)))
                                     (name showing-kw))))))

(defn search-results-nothing-found
  []
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div#no-results-found
     [:p "No results were found for "
      [:strong @search-query]]
     "Suggestions:"
     [:ul.ui.list
      [:li "Make sure all words are spelled correctly"]
      [:li "Try different keywords"]
      [:li "Try fewer keywords"]]]))

(defn see-also-item
  [s]
  (let [event-params {:action "see-also"
                      :category "ui"
                      :label s
                      :value s}]
    [:div.item [:a {:href (str "/#/search?q=" s)
                    :on-click #(re-frame/dispatch [:analytics/event event-params])} s]]))

(defn see-also []
  (let [search-query (re-frame/subscribe [:search-query])
        items (re-frame/subscribe [:search-items])]
    [:div#see-also
     [:div "See also:"]
     (let [authors (->> @items
                        vals
                        (mapcat :authors)
                        (map #(str (:firstName %) " " (:lastName %)))
                        (remove nil?)
                        (remove (partial = @search-query))
                        frequencies
                        (sort-by second >)
                        (take 2))
           tags (->> @items
                     vals
                     (mapcat :tags)
                     (map :name)
                     (remove nil?)
                     (remove (partial = @search-query))
                     frequencies
                     (sort-by second >)
                     (take 3))]
       (->> [tags authors]
            (reduce concat)
            (mapv (fn [[s n]] [see-also-item s]))
            (into [:div.ui.horizontal.list])))]))

(defn search-results-list
  [base-url]
  (let [items (re-frame/subscribe [:search-items])]
    (if-not (seq @items)
      [search-results-nothing-found]
      [:div.found-results
       (->> @items
            vals
            (mapv (fn [item] [search-result-item base-url item]))
            (into [:ul.search-items]))
       [see-also]])))

(defn enriched-result [{:keys [title body]}]
  [:div.results-definition
   [:div.results-definition-body
    [:h1 title]
    (utils/transform-extended-hiccup body)]])

(defn filter-results [search-query results]
  (->> results
       (filter (fn [{:keys [title]}] (utils/re-pattern? search-query title)))
       (into [])))

(defn search [{:keys [autofocus? id]}]
  (let [timeout (atom nil)]
    (reagent/create-class
      {:component-did-mount
       (fn [e]
         (when autofocus?
           (re-frame/dispatch [:ui/focus (str "#" id " input")])))

       :reagent-render
       (fn [{:keys [source onResultSelect onSearchChange onStoppedTyping]}]
         (let [search-query (re-frame/subscribe [:search-query])
               filtered-source (if-not (string/blank? @search-query)
                                 (filter-results @search-query source) source)]
           [:div {:id id}
            [:> ui/search
             (merge
               (when-not (string/blank? @search-query)
                 {:defaultValue @search-query})
               (when-not (nil? @search-query)
                 {:value @search-query})
               {:results filtered-source
                ; TODO: should we add categories?
                ; :category true
                :name "fluid"
                :selectFirstResult true
                :fluid true
                :placeholder "Eg. Monad, Applicative, Lens, Category Theory"
                :showNoResults false
                :onResultSelect onResultSelect
                :minCharacters 2
                :onSearchChange onSearchChange}
               (when-not (nil? onStoppedTyping)
                 {:onSearchChange
                  (utils/wrap-stop-typing {:timeout timeout
                                           :ms 500
                                           :onStoppedTyping onStoppedTyping
                                           :onSearchChange onSearchChange})}))]]))})))

(defn footer []
  [:div.ui.vertical.footer.segment.centerd
   [:p {:style {:text-align "center"}} "Built with "
    [:> ui/icon {:name "heart"}]
    " for the Haskell community"]
   [:> ui/divider]
   [:div.ui.center.aligned.container
    [:div.ui.horizontal.list.relaxed
     [:a.item {:on-click #(re-frame/dispatch [:navigate "/"])
               :style {:cursor "pointer"}}
      "Home"]
     [:a.item {:style {:cursor "pointer"}
               :on-click #(re-frame/dispatch [:modal/open :mailing-list])}
      "Subscribe"]
     [:a.item {:style {:cursor "pointer"}
          :on-click #(re-frame/dispatch [:modal/open :feedback])}
      "Contact"]
     [:a.item {:style {:cursor "pointer"}
               :on-click #(re-frame/dispatch [:modal/open :donate])}
      "Donate"]]]])

(defmulti tab-pannel (fn [params] (:tab params)))

(defmethod tab-pannel :default [args]
  [:div [:p [:strong "Error 404: "] "Page Not Found"]])

(defmethod tab-pannel :landing-page [{:keys [dispatchers base-url]}]
  (let [search-query (re-frame/subscribe [:search-query])
        search-source (re-frame/subscribe [:search-source])]
    [:div#landing-page
     [:div.header
      [:> ui/container
       [:img.ui.small.circular.centered.image
        {:src "images/haskell-bazaar-logo.svg"
         :alt "Haskell Bazaar Logo"}]
       [:form
        {:on-submit
         ((get-in dispatchers [:landing-page-search :on-submit]) search-query)}
        [search (merge (:landing-page-search dispatchers)
                       {:id           "landing-page-search-box"
                        :autofocus?   true
                        :source       @search-source
                        :search-query @search-query})]]
       [:h1.center.aligned.header.tag-line
        "The search engine for "
        [:strong "Haskell"]
        " resources!"]]]
     #_[:div#page-1 "TODO"]]))



(defmethod tab-pannel :search
  [{:keys [dispatchers base-url]}]
  (let [search-query (re-frame/subscribe [:search-query])
        search-source (re-frame/subscribe [:search-source])
        search-enriched-results (re-frame/subscribe [:search-enriched-results])]
    [:div
     [:div.topnav
      [:> ui/container
       [search (merge (:search dispatchers)
                      {:id           "search-box"
                       :autofocus?   false
                       :source       @search-source
                       :search-query @search-query})]]]
       (when-let [enriched-result-data
                  (get @search-enriched-results @search-query)]
         [:div.enriched-result
          [:> ui/container
           ;; TODO: rename enriched result
           [enriched-result enriched-result-data]]])
       [:div.search-results
        [:> ui/container
         [search-results-list base-url]]]]))

(defn ui [dispatchers base-url]
  (let [tab (re-frame/subscribe [:tab])
        modal-kw (re-frame/subscribe [:modal])]
    [:div
     [modal/modal]
     [tab-pannel {:dispatchers dispatchers :base-url base-url :tab @tab}]
     [footer]]))
