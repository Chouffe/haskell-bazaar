(ns haskell-bazaar-frontend.stubs)

(def keywords
  [{:kind "tag" :name "type class"}
   {:kind "tag" :name "functor"}
   {:kind "tag" :name "applicative"}
   {:kind "tag" :name "monad"}
   {:kind "tag" :name "category theory"}
   {:kind "tag" :name "type inference"}
   {:kind "tag" :name "free monad"}
   {:kind "tag" :name "effect"}
   {:kind "tag" :name "extensible effect"}
   {:lastName "Peyton Jones" :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2" :kind "author" :firstName "Simon"}
   {:lastName "Kmett" :uuid "dedadd6d-8042-41d2-a4b5-21f43a36bf8f" :kind "author" :firstName "Edward"}])

(def search-result
  [{:uuid "e1c02dec-2746-4a9c-a591-95c970f57ce6"
    :url "https://www.youtube.com/watch?v=re96UgMk6GQ"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}
	      {:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Escape from the ivory tower: the Haskell journey"
    :type "Video"
    :description "In this talk Simon discusses Haskell’s birth and evolution, including some of the research and engineering challenges he faced in design and implementation."
    :tags [{:name "monad"}
	   {:name "type class"}]}

   {:uuid "fdab10af-04cb-4112-bc3d-ea877d737e7c"
    :url "https://www.youtube.com/watch?v=6COvD8oynmI"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Adventure with Types in Haskell"
    :type "Video"
    :description "Recorded at Oregon Programming Languages Summer School 2013"
    :tags [{:name "type inference"}]}])
