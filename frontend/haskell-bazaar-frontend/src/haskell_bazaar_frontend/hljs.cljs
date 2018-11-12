(ns haskell-bazaar-frontend.hljs
  (:require [re-frame.core :as re-frame]))

(re-frame/reg-event-fx
  ::code-block
  (fn [_ [_ dom-node]]
    {:highlight-code-block dom-node}))
