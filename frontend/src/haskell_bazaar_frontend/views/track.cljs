(ns haskell-bazaar-frontend.views.track
(:require
    [clojure.string :as string]

    [reagent.core :as reagent]
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.api :as api]
    [haskell-bazaar-frontend.routes :as routes]
    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.ui :as ui]
    [haskell-bazaar-frontend.views.modal :as modal]))


(defn level-steps
  [track-level]
  [:div.ui.four.attached.steps.mini
   [:div.step
    (merge
      {:on-click #(re-frame/dispatch [:track/select :level :beginner])}
      (when (= track-level :beginner)
        {:class "active"}))
    [:div.content
     [:div.title "Beginner"]

     #_[:div.description "You are just starting with Haskell and Funtional Programming"]]]
   [:div.step
    (merge
      {:on-click #(re-frame/dispatch [:track/select :level :elementary])}
      (when (= track-level :elementary)
        {:class "active"}))
    [:div.content
     [:div.title "Elementary"]
     #_[:div.description "You can read a basic Haskell program"]]]
   [:div.step
    (merge
      {:on-click #(re-frame/dispatch [:track/select :level :intermediate])}
      (when (= track-level :intermediate)
        {:class "active"}))
    [:div.content
     [:div.title "Intermediate"]
     #_[:div.description "You master the basics and want to expand your knowledge"]]]
   [:div.step
    (merge
      {:on-click #(re-frame/dispatch [:track/select :level :advanced])}
      (when (= track-level :advanced)
        {:class "active"}))
    [:div.content
     [:div.title "Advanced"]
     #_[:div.description "You are already a well grounded Haskeller"]]]])

(defmulti level-body-impl identity)

;; TODO: finish
(defmethod level-body-impl :beginner [_]
  [:div.content.ui

   [:h3 "Concepts to explore"]
   [:ul
    [:li [:a {:href "#/search?q=function"} "Functions"]]
    [:li [:a {:href "#/search?q=list"} "Lists"]]
    [:li [:a {:href "#/search?q=tuple"} "Tuples"]]
    [:li [:a {:href "#/search?q=io"} "Simple Input/Output"]]]

   [:h3 "Blog posts to read"]

   [:h3 "Book chapters to read"]
   [:img.bordered.ui.small.image.left.floated
    {:src "https://images-eu.ssl-images-amazon.com/images/I/412gjoBtAxL.jpg"
     :alt "Learn you a Haskell for great good"
     :title "Learn you a Haskell for great good"}]
   [:p "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."]
   [:div.ui.hidden.divider]
   [:img.bodered.ui.small.image.right.floated
    {:src "http://haskellbook.com/assets/img/book-cover-front.png"
     :alt "Haskell Programming from first principles"
     :title "Haskell Programming from first principles"}]
   [:p "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."]
   [:div {:style {:clear "both"}}]

   ])

(defmethod level-body-impl :elementary [_]
  [:div "Hello Elementary"])

(defmethod level-body-impl :intermediate [_]
  [:div "Hello Intermeditate"])

(defmethod level-body-impl :advanced [_]
  [:div "Hello Advanced"])

(defn level-body [track-level]
  [:div.ui.container {:id "track-level"}
   [level-body-impl track-level]])

(defn level []
  (let [track-level (re-frame/subscribe [:track-level-selection])]
    [:div
     [level-steps @track-level]
     [level-body @track-level]]))
