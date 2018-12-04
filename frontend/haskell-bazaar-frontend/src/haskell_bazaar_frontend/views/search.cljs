(ns haskell-bazaar-frontend.views.search
(:require
    [clojure.string :as string]

    [reagent.core :as reagent]
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.ui :as ui]
    [haskell-bazaar-frontend.views.modal :as modal]))

(defn filter-results [search-query results]
  (->> results
       (filter (fn [{:keys [title]}] (utils/re-pattern? search-query title)))
       (into [])))

(defn search [{:keys [autofocus? id on-submit search-query]}]
  (let [local-state (atom {:timeout nil})]
    (reagent/create-class
      {:component-did-mount
       (fn [e]
         #_(-> js/document
             (.querySelector (str "#" id " i.link"))
             (.addEventListener "click"
                                (on-submit id search-query) e))

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
                :selectFirstResult false
                :icon "search link"
                :fluid true
                :placeholder "Eg. Monad, Applicative, Lens, Category Theory"
                :showNoResults false
                :onResultSelect onResultSelect
                :minCharacters 1
                :onSearchChange onSearchChange}
               (when-not (nil? onStoppedTyping)
                 {:onSearchChange
                  (utils/wrap-stop-typing {:local-state local-state
                                           :ms 500
                                           :onStoppedTyping onStoppedTyping
                                           :onSearchChange onSearchChange})}))]]))})))

(defn search-form
  [{:keys [on-submit onSearchChange onResultSelect id autofocus?] :as params}]
  (let [search-query (re-frame/subscribe [:search-query])
        search-source (re-frame/subscribe [:search-source])]
    [:form {:on-submit (on-submit id @search-query)}
     [search (merge (select-keys params [:on-submit
                                         :onSearchChange
                                         :onResultSelect
                                         :onStoppedTyping])
                    {:id           id
                     :autofocus?   autofocus?
                     :source       @search-source
                     :search-query @search-query})]]))
