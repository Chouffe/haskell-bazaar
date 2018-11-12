(ns haskell-bazaar-frontend.hljs
  )

(re-frame/reg-event-fx
  ::code-block
  interceptors
  (fn [_ [_ dom-node]]
    {:highlight-code-block dom-node}))
