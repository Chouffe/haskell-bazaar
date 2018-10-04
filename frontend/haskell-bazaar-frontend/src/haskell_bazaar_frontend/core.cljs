(ns haskell-bazaar-frontend.core
  (:require [ajax.core :as ajax]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]

            [haskell-bazaar-frontend.events]  ;; Register events
            [haskell-bazaar-frontend.subs]    ;; Register subscriptions
            [haskell-bazaar-frontend.views :as views]
            ))

;; Should only be activated in dev mode

(defonce app-setup!
  (do
    (when goog.DEBUG
      (enable-console-print!)) ;; println is now console.log

    ;; Setting initial db
    (re-frame/dispatch-sync [:initialize-db])

    ;; Loading keywords for auto completion
    (re-frame/dispatch [:api-keywords])))

;; ---------
;; app-state
;; ---------

(defn ^:export run
  []
  ;; Mounting React component
  (reagent/render [views/ui views/dispatchers]
                  (js/document.getElementById "app")))

;; ---------------
;; Figwheel reload
;; ---------------

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (run))
