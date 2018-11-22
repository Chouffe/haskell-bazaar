(ns haskell-bazaar-frontend.views.modal
  (:require
    [clojure.string :as string]

    [reagent.core :as reagent]
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.ui :as ui]))

(defn mailing-list-form
  []
  (let [state (reagent/atom {:email nil :sent? false})
        id-form "mailing-list-form"]
    (reagent/create-class
      {:component-did-mount
       #(re-frame/dispatch [:ui/focus (str "#" id-form " input")])

       :reagent-render
       (fn []
         (let [{:keys [sent? email]} @state]
           [:div.ui.form {:id id-form}
            [:p "Leave your email address below to get updates from Haskell Bazaar"]
            [:div.ui.left.icon.input.action
             [:input {:type "text"
                      :placeholder "Email Address"
                      :on-change #(swap! state assoc :email (utils/target-value %))}]
             [:i.icon.envelope]
             (if (and (not sent?) email (utils/validate-email email))
               [:button.ui.button.primary
                {:on-click #(do
                              (swap! state assoc :sent? true)
                              (re-frame/dispatch [:api-mailing-list-subscribe email]))}
                "Subscribe"]
               [:button.ui.button.disabled "Subscribe"])]
            (when sent?
              [:p "Thank you for subscribing to Haskell Bazaar"])]))})))

 (defn feedback-form
  []
  (let [state (reagent/atom {:message nil :sent? false})
        id-form "feedback-form"]
    (reagent/create-class
      {:component-did-mount
       #(re-frame/dispatch [:ui/focus (str "#" id-form " textarea")])

       :reagent-render
       (fn []
         [:div.ui.form {:id id-form}
          [:div.field
           [:textarea {:on-change
                       #(swap! state assoc :message (utils/target-value %))
                       :placeholder "Leave us a message here. If you want us to follow up with you, do not forget to include your email address in your message"}]]
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
              "Submit"])]])})))

;; Modals
(defmulti modal-impl identity)

(defmethod modal-impl :mailing-list [_]
  [:> ui/modal {:open true :onClose #(re-frame/dispatch [:modal/close])}
   [:> ui/modal-header "Subscription"]
   [:> ui/modal-content
    [:> ui/modal-description
     [mailing-list-form]]]])

(defmethod modal-impl :feedback [_]
  [:> ui/modal {:open true :onClose #(re-frame/dispatch [:modal/close])}
   [:> ui/modal-header "Contact / Feedback"]
   [:> ui/modal-content
    [:> ui/modal-description
     [feedback-form]]]])

(defn modal []
  (let [modal-kw (re-frame/subscribe [:modal])]
    (if-not @modal-kw
      [:div]
      [modal-impl @modal-kw])))

