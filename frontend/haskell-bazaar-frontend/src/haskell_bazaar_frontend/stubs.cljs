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

   {:uuid "2dab10af-04cb-4112-bc3d-ea877d737e7c"
    :url "https://www.youtube.com/watch?v=6COvD8oynmI"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Adventure with Types in Haskell"
    :type "Video"
    :description "Recorded at Oregon Programming Languages Summer School 2013"
    :tags [{:name "type inference"}]}

   {:uuid "3dab10af-04cb-4112-bc3d-ea877d737e7c"
    :url "https://www.youtube.com/watch?v=6COvD8oynmI"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Adventure with Types in Haskell"
    :type "Video"
    :description "Recorded at Oregon Programming Languages Summer School 2013"
    :tags [{:name "type inference"}]}

   {:uuid "4dab10af-04cb-4112-bc3d-ea877d737e7c"
    :url "https://www.youtube.com/watch?v=6COvD8oynmI"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Adventure with Types in Haskell"
    :type "Video"
    :description "Recorded at Oregon Programming Languages Summer School 2013"
    :tags [{:name "type inference"}]}
   {:uuid "fdab10af-04cb-4112-bc3d-ea877d737e7c"
    :url "https://www.youtube.com/watch?v=6COvD8oynmI"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Adventure with Types in Haskell"
    :type "Video"
    :description "Recorded at Oregon Programming Languages Summer School 2013"
    :tags [{:name "type inference"}]}

   {:uuid "5dab10af-04cb-4112-bc3d-ea877d737e7c"
    :url "https://www.youtube.com/watch?v=6COvD8oynmI"
    :authors [{:lastName "Peyton Jones"
	       :uuid "f4ecaa79-5740-432e-82b4-519ba3e750e2"
	       :firstName "Simon"}]
    :title "Adventure with Types in Haskell"
    :type "Video"
    :description "Recorded at Oregon Programming Languages Summer School 2013"
    :tags [{:name "type inference"}]}])

(def ds-facts
  [{:db/id "monad" :tag/name "monad"}
   {:db/id "type class" :tag/name "type class"}
   {:db/id "ed6cc7d1-953b-4870-aa60-46db2b661275" :author/uuid "ed6cc7d1-953b-4870-aa60-46db2b661275" :author/firstName "Simon" :author/lastName "Peyton Jones"
 :author/fullName "Simon Peyton Jones"} {:db/id "Video"
 :item-type :Video} {:item/uuid "30bd0a31-6622-4a4f-8fdf-ea9790353ff2"
 :item/authors [{:db/id "ed6cc7d1-953b-4870-aa60-46db2b661275"} {:db/id "ed6cc7d1-953b-4870-aa60-46db2b661275"}]
 :item/title "Escape from the ivory tower: the Haskell journey"
 :item/type {:db/id "Video"}
 :item/created-at #inst "2017-03-01T00:00:00.000-00:00"
 :item/description "In this talk Simon discusses Haskell’s birth and evolution
 including some of the research and engineering challenges he faced in design and implementation."
 :item/tags [{:db/id "monad"} {:db/id "type class"}]} {:db/id "type inference"
 :tag/name "type inference"} {:item/uuid "d4764fe6-862c-4943-841b-0998ccaa9966"
 :item/authors [{:db/id "ed6cc7d1-953b-4870-aa60-46db2b661275"}]
 :item/title "Adventure with Types in Haskell"
 :item/type {:db/id "Video"}
 :item/created-at #inst "2013-07-01T00:00:00.000-00:00"
 :item/description "Recorded at Oregon Programming Languages Summer School 2013"
 :item/tags [{:db/id "type inference"}]} {:db/id "dependency injection"
 :tag/name "dependency injection"} {:db/id "design pattern"
 :tag/name "design pattern"} {:db/id "be27fae0-8c85-45b2-9e86-685af4b9be39"
 :author/uuid "be27fae0-8c85-45b2-9e86-685af4b9be39"
 :author/firstName "Jasper"
 :author/lastName "Van der Jeugt"
 :author/fullName "Jasper Van der Jeugt"} {:db/id "Tutorial"
 :item-type :Tutorial} {:item/uuid "f6af8441-5764-49da-b1a7-f13f4c1d14af"
 :item/authors [{:db/id "be27fae0-8c85-45b2-9e86-685af4b9be39"} {:db/id "be27fae0-8c85-45b2-9e86-685af4b9be39"}]
 :item/title "Haskell Design Patterns: The Handle Pattern"
 :item/type {:db/id "Tutorial"}
 :item/created-at #inst "2018-03-08T00:00:00.000-00:00"
 :item/description "A neat and simple way to build services in Haskell"
 :item/tags [{:db/id "dependency injection"} {:db/id "design pattern"}]} {:db/id "property based testing"
 :tag/name "property based testing"} {:db/id "testing"
 :tag/name "testing"} {:db/id "quickcheck"
 :tag/name "quickcheck"} {:db/id "0e612a56-6817-4899-b1cb-912940308596"
 :author/uuid "0e612a56-6817-4899-b1cb-912940308596"
 :author/firstName "Joe"
 :author/lastName "Nelson"
 :author/fullName "Joe Nelson"} {:item/uuid "c2e53ed0-06ab-45d8-a1e5-ef63ad1cfa9e"
 :item/authors [{:db/id "0e612a56-6817-4899-b1cb-912940308596"} {:db/id "0e612a56-6817-4899-b1cb-912940308596"} {:db/id "0e612a56-6817-4899-b1cb-912940308596"}]
 :item/title "The Design and Use of QuickCheck"
 :item/type {:db/id "Tutorial"}
 :item/created-at #inst "2017-01-14T00:00:00.000-00:00"
 :item/description "QuickCheck is the grandfather of property-based testing libraries. Despite being imitated in over thirty languages
 the original implementation remains pre-eminent due to the type system and consistent logic of the Haskell language in which it is written."
 :item/tags [{:db/id "property based testing"} {:db/id "testing"} {:db/id "quickcheck"}]} {:db/id "lens"
 :tag/name "lens"} {:db/id "df5c3cd7-a0c8-46fe-b21f-07be119985cb"
 :author/uuid "df5c3cd7-a0c8-46fe-b21f-07be119985cb"
 :author/firstName "Gabriel"
 :author/lastName "Gonzales"
 :author/fullName "Gabriel Gonzales"} {:item/uuid "00600f4f-3e9d-494a-a221-94981318e5aa"
 :item/authors [{:db/id "df5c3cd7-a0c8-46fe-b21f-07be119985cb"}]
 :item/title "Lenses"
 :item/type {:db/id "Tutorial"}
 :item/created-at #inst "2012-01-28T00:00:00.000-00:00"
 :item/description "Lens tour and tutorial"
 :item/tags [{:db/id "lens"}]} {:db/id "b13d3c75-395f-4b69-8597-6a37afb73c58"
 :author/uuid "b13d3c75-395f-4b69-8597-6a37afb73c58"
 :author/firstName "Artyom"
 :author/lastName "Kazak"
 :author/fullName "Artyom Kazak"} {:item/uuid "b839544e-ca98-442e-90e4-52b5e999f3d2"
 :item/authors [{:db/id "b13d3c75-395f-4b69-8597-6a37afb73c58"}]
 :item/title "Lens Over Tea #1"
 :item/type {:db/id "Tutorial"}
 :item/created-at #inst "2016-01-01T00:00:00.000-00:00"
 :item/description "lenses 101
 traversals 101
 and some implementation details"
 :item/tags [{:db/id "lens"}]} {:db/id "traversal"
 :tag/name "traversal"} {:db/id "7613d49f-0fb4-4d92-a048-a4e0fa6a46c5"
 :author/uuid "7613d49f-0fb4-4d92-a048-a4e0fa6a46c5"
 :author/firstName "Haskell"
 :author/lastName "WikiBook"
 :author/fullName "Haskell WikiBook"} {:db/id "Book"
 :item-type :Book} {:item/uuid "612d20a2-e5ec-4e78-aca9-9e5e3e87b430"
 :item/authors [{:db/id "7613d49f-0fb4-4d92-a048-a4e0fa6a46c5"} {:db/id "7613d49f-0fb4-4d92-a048-a4e0fa6a46c5"}]
 :item/title "Lenses and functional references"
 :item/type {:db/id "Book"}
 :item/created-at #inst "2018-06-19T00:00:00.000-00:00"
 :item/description "This chapter is about functional references. By \"references\"
 we mean they point at parts of values
 allowing us to access and modify them. By \"functional\"
 we mean they do so in a way that provides the flexibility and composability we came to expect from functions. We will study functional references as implemented by the powerful lens library. lens is named after lenses
 a particularly well known kind of functional reference. Beyond being very interesting from a conceptual point of view
 lenses and other functional references allow for several convenient and increasingly common idioms
 put into use by a number of useful libraries."
 :item/tags [{:db/id "traversal"} {:db/id "lens"}]} {:db/id "applicative"
 :tag/name "applicative"} {:db/id "functor"
 :tag/name "functor"} {:db/id "fdbaf717-9509-42cc-b78f-4948403ead77"
 :author/uuid "fdbaf717-9509-42cc-b78f-4948403ead77"
 :author/firstName "Aditya"
 :author/lastName "Bhargava"
 :author/fullName "Aditya Bhargava"} {:db/id "Article"
 :item-type :Article} {:item/uuid "32bc182c-352c-431b-8bfe-71c9a5c3c6d0"
 :item/authors [{:db/id "fdbaf717-9509-42cc-b78f-4948403ead77"} {:db/id "fdbaf717-9509-42cc-b78f-4948403ead77"} {:db/id "fdbaf717-9509-42cc-b78f-4948403ead77"}]
 :item/title "Functors
 Applicatives
 And Monads In Pictures"
 :item/type {:db/id "Article"}
 :item/created-at #inst "2013-04-17T00:00:00.000-00:00"
 :item/description "Visualize functors
 applicative and monads with pictures"
 :item/tags [{:db/id "monad"} {:db/id "applicative"} {:db/id "functor"}]} {:db/id "aeson"
 :tag/name "aeson"} {:db/id "json"
 :tag/name "json"} {:item/uuid "0dc0b65d-8690-41e7-afae-39089aab5e61"
 :item/authors [{:db/id "b13d3c75-395f-4b69-8597-6a37afb73c58"} {:db/id "b13d3c75-395f-4b69-8597-6a37afb73c58"}]
 :item/title "Aeson: the tutorial"
 :item/type {:db/id "Tutorial"}
 :item/created-at #inst "2016-01-01T00:00:00.000-00:00"
 :item/description "Aeson is the most widely used library for parsing JSON (in Haskell
 of course
 I'm not talking about the whole world). It's also hopelessly magical for people who try to learn it by looking at provided examples
 and existing tutorials don't help that much
 so I wrote my own. It's supposed to be the most comprehensive Aeson tutorial on the web; if there's anything missing from it
 send me an email! (So far the only thing I haven't covered is using lenses with Aeson
 but this is coming soon.)"
 :item/tags [{:db/id "aeson"} {:db/id "json"}]}])
