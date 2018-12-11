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

(def button             (component "Button"))
(def column             (component "Column"))
(def container          (component "Container"))
(def divider            (component "Divider"))
(def grid               (component "Grid"))
(def icon               (component "Icon"))
(def input              (component "Input"))
(def modal              (component "Modal"))
(def modal-content      (component "Modal" "Content"))
(def modal-description  (component "Modal" "Description"))
(def modal-header       (component "Modal" "Header"))
(def search             (component "Search"))
(def segment            (component "Segment"))
