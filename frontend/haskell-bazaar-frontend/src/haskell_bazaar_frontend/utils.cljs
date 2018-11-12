(ns haskell-bazaar-frontend.utils
  (:require
    [clojure.walk :as w]
    [re-frame.core :as re-frame]
    [reagent.core :as reagent]))

(defn coll->hashmap [key-fn]
  (fn [xs]
    (reduce (fn [x y] (assoc x (key-fn y) y)) {} xs)))

(def uuid-coll->hashmap (coll->hashmap :uuid))

(defn url-encode
  [string]
  (some-> string
          str
          js/encodeURIComponent
          (.replace "+" "%20")))

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

(defn sanitize-re [s]
  (let [forbidden-chars #{\} \{ \( \) \. \* \+ \-}]
    (->> s
         (filter (complement forbidden-chars))
         (apply str))))

(defn re-pattern?
  "re-find case insensitive `e` in `m`"
  [e m]
  (re-find (re-pattern (str "(?i)" (sanitize-re e))) m))

(defn add-seconds
  ([s] (add-seconds (js/Date.) s))
  ([d s] (js/Date. (+ (.getTime d) (* 1000 s)))))

(defn focus!
  [input-name-str]
  (-> js/document
      (.querySelector (str "input[name='" input-name-str "']"))
      .focus))

(defn rmatch [search-query s]
  (let [pat (re-pattern (str "(?i)(.*)(" search-query ")(.*)"))]
    (re-matches pat s)))

(defn highlight-string-match [search-query s]
  (if-let [[_ prefix match suffix] (rmatch (sanitize-re search-query) s)]
    [:span prefix [:strong match] suffix]
    s))

(defn trunc
  "truncates string to `n` characters"
  [s n]
  (subs s 0 (min (count s) n)))

(defn code-block [code]
  (reagent/create-class
    {:component-did-mount
     (fn [this] (re-frame/dispatch [:hljs/code-block (reagent/dom-node this)]))

     :reagent-render
     (fn [code] [:pre [:code.haskell.hljs code]])}))

; TODO: abstract over a set of rules and dispatchers
(defn transform-extended-hiccup [v]
  (cond
    (or (keyword? v) (string? v) (integer? v)) v

    (and (vector? v) (= (first v) :code-block))
    (let [[_ body] v]
      [code-block body])

    (and (vector? v) (keyword? (first v)))
    (let [[k & args] v]
      (into [k] (mapv transform-extended-hiccup args)))

    :else v))
