(ns haskell-bazaar-frontend.ui
  (:require
    [cljsjs.semantic-ui-react]
    [goog.object]
    [reagent.core :as reagent]))

(def semantic-ui js/semanticUIReact)

(defn component
  "get a component from sematic-ui-react:

  (component \"Button\")
  (component \"Menu\" \"Item\")"
  [k & ks]
  (if (seq ks)
    (apply goog.object/getValueByKeys semantic-ui k ks)
    (goog.object/get semantic-ui k)))

(def container      (component "Container"))
(def grid           (component "Grid"))
(def column         (component "Column"))
(def segment        (component "Segment"))
(def search         (component "Search"))

