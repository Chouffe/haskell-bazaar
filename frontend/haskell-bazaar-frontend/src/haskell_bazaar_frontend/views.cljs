(ns haskell-bazaar-frontend.views
  (:require
    [clojure.string :as string]

    [reagent.core :as reagent]
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.ui :as ui]))


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

    :onSearchChange
    (fn [e]
      (re-frame/dispatch [:set-search-query (utils/target-value e)])
      (re-frame/dispatch [:datascript/search (utils/target-value e)]))}

   :search
   {:onResultSelect
    (fn [event data]
      (let [selected-result (get-in (js->clj data) ["result" "title"])]
        (re-frame/dispatch [:datascript/search selected-result])
        (re-frame/dispatch [:set-search-query selected-result])))

    :onSearchChange
    (fn [e]
      (re-frame/dispatch [:set-search-query (utils/target-value e)])
      (re-frame/dispatch [:datascript/search (utils/target-value e)]))
    }
   ;; TODO: unused, remove
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
     [:a.lg {:target "_blank" :href (api/item-url base-url uuid)}
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

(defn search-results-list
  [base-url]
  (let [items (re-frame/subscribe [:search-items])
        showing (re-frame/subscribe [:showing])]
    (->> @items
         vals
         ((filter-items @showing))
         (mapv (fn [item] [search-result-item base-url item]))
         (into [:ul.search-items]))))

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

;; TODO: move to datascript and write queries for it
(def definitions
  {"monad" {:title "Monad Typeclass"
            :body [:code-block
                   "class Applicative m => Monad m where\n  (>>=) :: m a -> (a -> m b) -> m b\n  return :: a -> m a"]}
   "functor" {:title "Functor Typeclass"
              :body [:code-block
                     "class Functor f where\n  fmap :: (a -> b) -> f a -> f b"]}
   "applicative" {:title "Applicative Functor Typeclass"
                  :body [:code-block
                         "class Functor f => Applicative f where\n  pure :: a -> f a\n  (<*>) :: f (a -> b) -> f a -> f b"]}
   "monoid" {:title "Monoid Typeclass"
             :body [:code-block
                    "class Semigroup a => Monoid a where\n  mempty :: a\n  (<>) :: a -> a -> a"]}
   "lens" {:title "Lens Type"
           :body
           [:span
            [:p "A Lens a b can be seen as getter and setter as first approximation"]
            [:code-block "data Lens a b = Lens\n  { get :: a -> b\n  , set :: a -> b -> a\n  }"]
            [:p "Below is the general Lens type definition"]
            [:code-block "type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t"]]}})

(defn results-definition [{:keys [title body]}]
  [:div.results-definition
   [:div.results-definition-body
    [:h1 title]
    (utils/transform-extended-hiccup body)]])

(def source
  [
   {
    :title "monad"
    :description "Monad type class"
   }
   {
    :title "functor"
    :description "Functor type class"
   }
   {
    :title "applicative"
    :description "Applicative type class"}])

(def source-2
  {:tags
   {:name "tag"
    :results [{:title "Monad"
                :description "Monad type class"}
               {:title "Monoid"
                :description "Monoid type class"}]}
   :authors {:name "author"
             :results [{:title "Simon Peyton Jones"}
                       {:title "Simon Marlow"}
                       {:title "Rich Hickey"}]}})

(defn filter-results [search-query results]
  (into []
        (filter (fn [{:keys [title]}]
                  (utils/re-pattern? search-query title)) results)))

(defn feedback-modal []
  (let [state (reagent/atom {:message nil :sent? false})]
    (fn []
      [:> ui/modal {:open true
                    :onClose #(re-frame/dispatch [:modal/close])}
       [:> ui/modal-header "Leave us your Feedback"]
       [:> ui/modal-content
        [:> ui/modal-description
         [:div.ui.form
          [:div.field
           [:textarea {:on-change #(swap! state assoc :message (utils/target-value %))
                       :placeholder "Please leave us a message here. If you want us to follow up fo not forget to include your email address in your message"}]]]
         [:br]
         [:div.ui.center.aligned
          (if (:sent? @state)
            [:p "Thanks for your valuable feedback!"]
            [:button.ui.primary.button
             (merge
               {:class "disabled"}
               (when-not (string/blank? (:message @state))
                 {:class "enabled"
                  :on-click #(do
                               (swap! state assoc :sent? true)
                               (re-frame/dispatch [:api-feedback (:message @state)]))}))
             "Submit"])]]]])))

;; TODO: use a multimethod instead
(defn modal []
  (let [modal-kw (re-frame/subscribe [:modal])]
    (when-let [modal @modal-kw]
      (case modal
        :feedback [feedback-modal]
        [:div]))))

(defn search [{:keys [source onResultSelect onSearchChange autofocus? id]}]
  (reagent/create-class
    {:component-did-mount
     (fn [e]
       (when autofocus?
         (re-frame/dispatch [:ui/focus (str "#" id " input")])))

     :reagent-render
     (fn [{:keys [source onResultSelect onSearchChange autofocus? id]}]
       (let [search-query (re-frame/subscribe [:search-query])
             filtered-source (if-not (string/blank? @search-query)
                               (filter-results @search-query source) source)]
         [:div {:id id}
          [:> ui/search
           (merge
             (when-not (string/blank? @search-query)
               {:defaultValue @search-query})
             {:results filtered-source
              ; TODO: should we add categories?
              ; :category true
              :name "fluid"
              :selectFirstResult true
              :fluid true
              :placeholder "Eg. Monad, Applicative, Lens, Category Theory"
              :showNoResults false
              :onResultSelect onResultSelect
              :onSearchChange onSearchChange
              })]]))}))

(defn footer []
  [:div.ui.vertical.footer.segment
   [:> ui/divider]
   [:div.ui.center.aligned.container
    [:p "Built with "
     [:> ui/icon {:name "heart"}]
     " for the Haskell community"]
    [:p
     [:a {:on-click #(re-frame/dispatch [:modal/open :feedback])}
      "Leave us some feedback here"]]]])

(defmulti tab-pannel (fn [params] (:tab params)))

(defmethod tab-pannel :default [args] (.log js/console args) [:div "Hello World"])

(defmethod tab-pannel :landing-page [{:keys [dispatchers base-url]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div#landing-page
     ;; TODO: verical align content
     [:div.header
      [:> ui/container
       [:img.ui.small.circular.centered.image
        {:src "images/haskell-bazaar-logo.png"
         :alt "Haskell Bazaar Logo"}]
       [:form
        {:on-submit
         ((get-in dispatchers [:landing-page-search :on-submit]) search-query)}
        [search (merge (:landing-page-search dispatchers)
                       {:id "landing-page-search-box"
                        :autofocus? true
                        :source source
                        :search-query @search-query})]]
       [:h1.center.aligned.header.tag-line
        "The search engine for "
        [:strong "Haskell"]
        " resources!"]]]
     #_[:div#page-1 "TODO"]]))

(defmethod tab-pannel :search [{:keys [dispatchers base-url]}]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div
     [:div.topnav
      [:> ui/container
       [search (merge (:search dispatchers)
                      {:id "search-box"
                       :autofocus? false
                       :source source
                       :search-query @search-query})]]]
       (when-let [definition (get definitions @search-query)]
         [:div.enriched-result
          [:> ui/container
           ;; TODO: rename enriched result
           [results-definition definition]]])
       [:div.search-results
        [:> ui/container
         [search-results-list base-url]]]]))

(defn ui [dispatchers base-url]
  (let [tab (re-frame/subscribe [:tab])]
    [:div
     [modal]
     [tab-pannel {:dispatchers dispatchers :base-url base-url :tab @tab}]
     [footer]]))
