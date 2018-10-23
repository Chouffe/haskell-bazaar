(ns haskell-bazaar-frontend.utils
  (:require [clojure.walk :as w]))

(defn coll->hashmap [key-fn]
  (fn [xs]
    (reduce (fn [x y] (assoc x (key-fn y) y)) {} xs)))

(def uuid-coll->hashmap (coll->hashmap :uuid))

(defn url-encode
  [string]
  (some-> string str (js/encodeURIComponent) (.replace "+" "%20")))

(defn target-value [e]
  (-> e .-target .-value))

(defn traverse-keys [k-fn]
  (fn [m]
    (letfn [(inner-fn [[k v]]
              (cond
                (map? v)    [(k-fn k) ((traverse-keys k-fn) v)]
                (vector? v) [(k-fn k) (mapv (traverse-keys k-fn) v)]
                :else       [(k-fn k) v]))]
      (w/walk inner-fn identity m))))

(def ns-keyword->keyword (comp keyword name))

(defn re-pattern?
  "re-find case insensitive `e` in `m`"
  [e m]
  (re-find (re-pattern (str "(?i)" e)) m))

(defn add-seconds
  ([s] (add-seconds (js/Date.) s))
  ([d s] (js/Date. (+ (.getTime d) (* 1000 s)))))
