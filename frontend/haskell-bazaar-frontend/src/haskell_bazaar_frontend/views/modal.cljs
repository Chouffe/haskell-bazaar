(ns haskell-bazaar-frontend.views.modal
  (:require
    [clojure.string :as string]

    [reagent.core :as reagent]
    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.ui :as ui]))

(defmulti modal identity)

(defmethod modal :mailing-list [_]
  (let [state (reagent/atom {:email nil :sent? false})]
    (fn []
      (let [{:keys [sent? email]} @state]
        [:> ui/modal {:open true :onClose #(re-frame/dispatch [:modal/close])}
         [:> ui/modal-header "Subscription"]
         [:> ui/modal-content
          [:> ui/modal-description
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
              [:button.ui.button.disabled "Subscribe"])
            (when sent?
              [:p "Thank you for subscribing to the mailing-list"])]]]]))))

(defmethod modal :feedback [_]
  (let [state (reagent/atom {:message nil :sent? false})]
    (fn []
      [:> ui/modal {:open true :onClose #(re-frame/dispatch [:modal/close])}
       [:> ui/modal-header "Contact / Feedback"]
       [:> ui/modal-content
        [:> ui/modal-description
         [:div.ui.form
          [:div.field
           [:textarea {:on-change #(swap! state assoc :message (utils/target-value %))
                       :placeholder "Leave us a message here. If you want us to follow up with you, do not forget to include your email address in your message"}]]]
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
             "Submit"])]]]]))
  )
