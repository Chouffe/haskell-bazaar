(ns haskell-bazaar-frontend.views
  (:require
    [reagent.core :as reagent]
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



(defn results-definition [{:keys [title body]}]
  [:div.results-definition
   [:div.results-definition-body
    [:h1 title]
    (utils/transform-extended-hiccup body)]])

;; TODO: move to datascript and write queries for it
(def definitions
  {"monad" {:name "monad"
            :title "Monad Typeclass"
            :body [:code-block
                   "class Applicative m => Monad m where\n  (>>=) :: m a -> (a -> m b) -> m b\n  return :: a -> m a"]}
   "functor" {:name "functor"
              :title "Functor Typeclass"
              :body [:code-block
                     "class Functor f where\n  fmap :: (a -> b) -> f a -> f b"]}
   "applicative" {:name "applicative"
                  :title "Applicative Typeclass"
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

(defn ui [dispatchers base-url]
  (let [search-query (re-frame/subscribe [:search-query])]
    [:div
     [:div.topnav
      [search-box (:search-box dispatchers)]
      #_[:div.bar
         [search-filters (:filters dispatchers)]]]
     (when-let [definition (get definitions @search-query)]
       [results-definition definition])
     [:div.results
      [search-results-list base-url]]]))


