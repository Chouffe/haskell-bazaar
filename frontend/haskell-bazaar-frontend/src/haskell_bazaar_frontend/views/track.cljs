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
  [:div.ui.list
   [:a.item
    [:i.right.triange.icon]
    [:div.content
     [:div.header "Type Basics"]
     [:div.description "Lorem Ipsum"]]]])

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
