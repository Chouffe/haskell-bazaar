(ns haskell-bazaar-frontend.db
  (:require
    [cljs.spec.alpha :as s]

    [re-frame.core :as re-frame]

    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.environment :as env]
    [haskell-bazaar-frontend.stubs :as stubs]))

;; TODO: move to a util namespace
(def showing->title
  {:all "all"
   :videos "videos"
   :papers "papers"
   :blog-posts "blog posts"
   :tutorials "tutorials"})

;; Clojure.Spec
;; ------------

;; Environment
(s/def ::environment #{:prod :dev :test})

;; Search
(s/def ::search-query (s/nilable string?))
(s/def ::search-error (s/nilable string?))
(s/def ::search-loading boolean?)
(s/def ::showing (set (keys showing->title)))
(s/def ::sorted #{:date})

;; Keyword
(s/def ::name string?)
(s/def ::kind #{"tag", "author"})
(s/def ::keyword (s/or
                   :author (s/keys :req-un [::kind ::firstName ::lastName])
                   :tag (s/keys :req-un [::kind ::name])))
(s/def ::keywords (s/coll-of ::keyword))

;; Tags
(s/def ::tag (s/keys :req-un [::name]))
(s/def ::tags (s/coll-of ::tag))

;; Author
(s/def ::lastName string?)
(s/def ::firstName string?)
(s/def ::author (s/keys :req-un [::lastName ::uuid ::firstName]))
(s/def ::authors (s/coll-of ::author :kind vector?)) ;; TODO: add :distinct true here

;; Item
(s/def ::uuid string?)
(s/def ::title string?)
(s/def ::description string?)
(s/def ::type #{"Video"})
(s/def ::item
  (s/keys :req-un [::uuid
                   ::authors
                   ::title
                   ::type
                   ::description
                   ::tags]))
(s/def ::items (s/map-of ::uuid ::item))

;; App-state
(s/def ::app-state
  (s/keys :req-un [::environment
                   ::search-query
                   ::search-error
                   ::search-loading
                   ::showing
                   ::sorted
                   ::keywords
                   ::items]))

;; TODO: add caching layer for search already performed in an interceptor
(defn default-db [env]
  {:environment    env
   :search-query   nil
   :search-error   nil
   :search-loading false
   :showing        :all
   :sorted         :date
   :keywords       []
   ; :items          (utils/uuid-coll->hashmap stubs/search-result)
   :items          {}
   })

(def title->showing
  (zipmap (vals showing->title) (keys showing->title)))
