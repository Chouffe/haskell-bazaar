(ns haskell-bazaar-frontend.ds
  (:require
    [clojure.string :as str]
    [clojure.walk :as w]
    [re-frame.core :as re-frame]

    [datascript.core :as d]

    [haskell-bazaar-frontend.stubs :as stubs]
    [haskell-bazaar-frontend.utils :as utils]))

;; Datascript Schema
(def schema
  {:tag/name                    {:db/cardinality :db.cardinality/one
                                 :db/unique      :db.unique/identity}

   ; Authors
   :author/firstName            {:db/cardinality :db.cardinality/one}
   :author/lastName             {:db/cardinality :db.cardinality/one}
   :author/fullName             {:db/cardinality :db.cardinality/one}
   :author/uuid                 {:db/unique      :db.unique/identity}

   ; Items
   :item/uuid                   {:db/unique      :db.unique/identity}
   :item/authors                {:db/cardinality :db.cardinality/many
                                 :db/valueType   :db.type/ref}
   :item/title                  {:db/cardinality :db.cardinality/one}
   :item/type                   {:db/cardinality :db.cardinality/one
                                 :db/valueType   :db.type/ref}
   :item/created-at             {:db/cardinality :db.cardinality/one}
   :item/description            {:db/cardinality :db.cardinality/one}
   :item/tags                   {:db/cardinality :db.cardinality/many
                                 :db/valueType   :db.type/ref}

   ; Item Type #{:Video :Paper ...}
   :item-type                   {:db/cardinality :db.cardinality/one}})

(defn init! [conn]

  (re-frame/reg-fx
    :datascript/transact
    (fn [facts]
      (d/transact! conn facts)))

  (re-frame/reg-cofx
    :datascript
    (fn [coeffects _]
      (assoc coeffects :datascript (d/db conn)))))

(defn author->facts
  [{:keys [firstName lastName uuid]}]
  {:db/id            uuid
   :author/uuid      uuid
   :author/firstName firstName
   :author/lastName  lastName
   :author/fullName  (str firstName " " lastName)})

(defn tag->facts
  [{:keys [name]}]
  {:db/id name
   :tag/name name})

(defn item-type->facts
  [item-type]
  {:db/id (str item-type)
   :item-type (keyword item-type)})

(defn item->facts
  [{:keys [uuid authors title type description tags created_at]}]
  (let [tag-facts (mapv tag->facts tags)
        author-facts (mapv author->facts authors)
        item-type-facts (item-type->facts type)]
    (flatten
      [tag-facts
       author-facts
       item-type-facts
       {:item/uuid        uuid
        :item/authors     (mapv (fn [a] (select-keys a [:db/id])) author-facts)
        :item/title       title
        :item/type        (select-keys item-type-facts [:db/id])
        :item/created-at  (js/Date. created_at)
        :item/description description
        :item/tags        (mapv (fn [t] (select-keys t [:db/id])) tag-facts)}])))

(defn persist-search-results!
  [conn search-results]
  (->> search-results
       (map item->facts)
       flatten
       distinct
       (d/transact! conn)))

(defn find-items-by-title
  [db query-string]
  (d/q '[:find [?i ...]
         :in $ ?query-string ?re-pattern
         :where
         [?i :item/title ?title]
         [(?re-pattern ?query-string ?title)]]
       db
       query-string
       utils/re-pattern?))

(defn find-items-by-description
  [db query-string]
  (d/q '[:find [?i ...]
         :in $ ?query-string ?re-pattern
         :where
         [?i :item/description ?description]
         [(?re-pattern ?query-string ?description)]]
       db
       query-string
       utils/re-pattern?))

;; Tags
(defn find-items-by-tag
  [db query-string]
  (d/q '[:find [?i ...]
         :in $ ?query-string ?re-pattern
         :where
         [?t :tag/name ?tag]
         [?i :item/tags ?t]
         [(?re-pattern ?query-string ?tag)]]
       db
       query-string
       utils/re-pattern?))

;; Authors
(defn find-items-by-author
  [db query-string]
  (d/q '[:find [?i ...]
         :in $ ?query-string ?re-pattern  [?author-attribute ...]
         :where
         [?a ?author-attribute ?attribute]
         [?i :item/authors ?a]
         [(?re-pattern ?query-string ?attribute)]]
       db
       query-string
       utils/re-pattern?
       [:author/fullName :author/firstName :author/lastName]))

;; REPL iteration
;; TODO: remove
; (def conn (d/create-conn schema))

;; Insert data in datascript db
; (persist-search-results! conn stubs/search-result)

; Datascript queries
; (find-items-by-tag @conn "type")
; (find-items-by-author @conn "simon")
; (find-items-by-author @conn "pey")
; (find-items-by-title @conn "ivOry")
; (find-items-by-description @conn "evolution")

; (defn hydrate-items
;   [db entity-ids]
;   (map (fn [[id]] (d/pull db item-pull-pattern id)) entity-ids))

(def item-pull-pattern
  '[:item/uuid
    :item/description
    :item/title
    {:item/type [:item-type]}
    {:item/tags [:tag/name]}
    {:item/authors [:author/firstName
                    :author/lastName
                    :author/uuid]}])

;; TODO: add a sort function by relevance function
(defn search [db search-query]
  (->> search-query
       ((juxt (partial find-items-by-tag db)
              (partial find-items-by-author db)
              (partial find-items-by-title db)
              (partial find-items-by-description db)))
       flatten
       distinct
       (map (fn [id] (d/pull db item-pull-pattern id)))
       (map (utils/traverse-keys utils/ns-keyword->keyword))))
