(ns haskell-bazaar-frontend.environment)

(defn environment []
  (if goog.DEBUG :dev :prod))
