(ns haskell-bazaar-frontend.local-storage)

(defn set-item!
  "Set `key' in browser's localStorage to `val`."
  [key val]
  (js->clj (.setItem (.-localStorage js/window) key val)))

(defn get-item
  "Returns value of `key' from browser's localStorage."
  [key]
  (js->clj (.getItem (.-localStorage js/window) key)))

(defn remove-item!
  "Remove the browser's localStorage value for the given `key`"
  [key]
  (js->clj (.removeItem (.-localStorage js/window) key)))
