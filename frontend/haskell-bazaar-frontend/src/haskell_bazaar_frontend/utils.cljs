(ns haskell-bazaar-frontend.utils)

(defn coll->hashmap [key-fn]
  (fn [xs]
    (reduce (fn [x y] (assoc x (key-fn y) y)) {} xs)))

(def uuid-coll->hashmap (coll->hashmap :uuid))

(defn url-encode
  [string]
  (some-> string str (js/encodeURIComponent) (.replace "+" "%20")))
