(ns haskell-bazaar-frontend.db
  (:require
    [cljs.spec.alpha :as s]

    [re-frame.core :as re-frame]
    [datascript.core :as d]

    [haskell-bazaar-frontend.utils :as utils]
    [haskell-bazaar-frontend.environment :as env]
    [haskell-bazaar-frontend.stubs :as stubs]))

;; Clojure.Spec
;; ------------

;; Environment
(s/def ::environment #{:prod :dev :test})

;; Search
(s/def ::search-query (s/nilable string?))
(s/def ::search-error (s/nilable string?))
(s/def ::search-loading boolean?)
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
; (s/def ::type #{"Video"})
(s/def ::type (s/keys :req-un [::item-type]))
(s/def ::item-type #{:Video :Tutorial :Book :Article :Paper})
(s/def ::item
  (s/keys :req-un [::uuid
                   ::title
                   ::type
                   ::description]
          :opt [::tags ::authors]))
(s/def ::items (s/map-of ::uuid ::item))
(s/def ::search-item
  (s/keys :req-un [::uuid
                   ::title
                   ::type
                   ::description]
          :opt [::tags ::authors]))
(s/def ::search-items (s/map-of ::uuid ::search-item))
(s/def ::modal #{:feedback :mailing-list :donate})
(s/def ::tab #{:search :landing-page})

;; App-state
(s/def ::app-state
  (s/keys :req-un [::environment
                   ::search-query
                   ::search-error
                   ::search-loading
                   ::search-items
                   ::sorted
                   ::keywords
                   ::items
                   ::tab]
          :opt-un [::modal]))

(def search-enriched-results
  {"monad" {:title "Monad Typeclass"
            :body [:code-block
                   "class Applicative m => Monad m where\n  (>>=) :: m a -> (a -> m b) -> m b\n  return :: a -> m a"]}
   "functor" {:title "Functor Typeclass"
              :body [:code-block
                     "class Functor f where\n  fmap :: (a -> b) -> f a -> f b"]}
   "applicative" {:title "Applicative Functor Typeclass"
                  :body [:code-block
                         "class Functor f => Applicative f where\n  pure :: a -> f a\n  (<*>) :: f (a -> b) -> f a -> f b"]}
   "monoid" {:title "Monoid Typeclass"
             :body [:code-block
                    "class Semigroup a => Monoid a where\n  mempty :: a\n  (<>) :: a -> a -> a"]}
   "lens" {:title "Lens Type"
           :body
           [:span
            [:p "A Lens a b can be seen as getter and setter as first approximation"]
            [:code-block "data Lens a b = Lens\n  { get :: a -> b\n  , set :: a -> b -> a\n  }"]
            [:p "Below is the general Lens type definition"]
            [:code-block "type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t"]]}})

;; For categories
; (def source-2
;   {:tags
;    {:name "tag"
;     :results [{:title "Monad"
;                 :description "Monad type class"}
;                {:title "Monoid"
;                 :description "Monoid type class"}]}
;    :authors {:name "author"
;              :results [{:title "Simon Peyton Jones"}
;                        {:title "Simon Marlow"}
;                        {:title "Rich Hickey"}]}})

(defn default-db [env]
  {:environment    env
   :search-query   nil
   :search-error   nil
   :search-loading false
   ;; TODO: spec me out!
   :search-source  {}
   ;; TODO: spec me out!
   :search-enriched-results search-enriched-results
   :search-items   {}
   :sorted         :date
   ;; TODO: deprecate :keywords?
   :keywords       []
   ;; TODO: deprecate :items
   :items          {}
   :tab            :landing-page
   ; :items          (utils/uuid-coll->hashmap stubs/search-result)
   })
