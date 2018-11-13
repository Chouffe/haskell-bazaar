-- Running the file is idempotent
BEGIN TRANSACTION;

-- Delete all data
DELETE FROM item_tag;
DELETE FROM tag;

DELETE FROM item_author;
DELETE FROM author;

DELETE FROM item;

-- Insert all data
INSERT INTO "item" (id, title, description, url, item_type, created_at) VALUES
    (1, 'Escape from the ivory tower: the Haskell journey', 'In this talk Simon discusses Haskell’s birth and evolution, including some of the research and engineering challenges he faced in design and implementation.', 'https://www.youtube.com/watch?v=re96UgMk6GQ', 'Video', '2017-03-01')
  , (2, 'Adventure with Types in Haskell', 'Recorded at Oregon Programming Languages Summer School 2013', 'https://www.youtube.com/watch?v=6COvD8oynmI', 'Video', '2013-07-01')
  , (3, 'Haskell Design Patterns: The Handle Pattern', 'A neat and simple way to build services in Haskell', 'https://jaspervdj.be/posts/2018-03-08-handle-pattern.html', 'Tutorial', '2018-03-08')
  , (4, 'The Design and Use of QuickCheck', 'QuickCheck is the grandfather of property-based testing libraries. Despite being imitated in over thirty languages, the original implementation remains pre-eminent due to the type system and consistent logic of the Haskell language in which it is written.', 'https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html', 'Tutorial', '2017-01-14')
  , (5, 'Lenses', 'Lens tour and tutorial', 'http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers_28.html', 'Tutorial', '2012-01-28')
  , (6, 'Lens Over Tea #1', 'lenses 101, traversals 101, and some implementation details', 'https://artyom.me/lens-over-tea-1', 'Tutorial', '2016-01-01')
  , (7, 'Lenses and functional references', 'This chapter is about functional references. By "references", we mean they point at parts of values, allowing us to access and modify them. By "functional", we mean they do so in a way that provides the flexibility and composability we came to expect from functions. We will study functional references as implemented by the powerful lens library. lens is named after lenses, a particularly well known kind of functional reference. Beyond being very interesting from a conceptual point of view, lenses and other functional references allow for several convenient and increasingly common idioms, put into use by a number of useful libraries.', 'https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references', 'Book', '2018-06-19')
  , (8, 'Functors, Applicatives, And Monads In Pictures', 'Visualize functors, applicative and monads with pictures', 'http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html', 'Article', '2013-04-17')
  , (9, 'Aeson: the tutorial', 'Aeson is the most widely used library for parsing JSON (in Haskell, of course, I''m not talking about the whole world). It''s also hopelessly magical for people who try to learn it by looking at provided examples, and existing tutorials don''t help that much, so I wrote my own. It''s supposed to be the most comprehensive Aeson tutorial on the web; if there''s anything missing from it, send me an email! (So far the only thing I haven''t covered is using lenses with Aeson, but this is coming soon.)', 'https://artyom.me/aeson', 'Tutorial', '2016-01-01')
  ;

INSERT INTO "tag" (id, name) VALUES
    (1, 'type class')
  , (2, 'functor')
  , (3, 'applicative')
  , (4, 'monad')
  , (5, 'category theory')
  , (6, 'type inference')
  , (7, 'free monad')
  , (8, 'effect')
  , (9, 'extensible effect')
  , (10, 'quickcheck')
  , (11, 'testing')
  , (12, 'property based testing')
  , (13, 'design pattern')
  , (14, 'dependency injection')
  , (15, 'lens')
  , (16, 'traversal')
  , (17, 'json')
  , (18, 'aeson')
  ;

-- TODO: add a nickname or handle?
-- , (10, 'jaspervdj')
-- , (11, 'begriffs')
INSERT INTO "author" (id, first_name, last_name) VALUES
    (1, 'Simon', 'Peyton Jones')
  , (2, 'Edward', 'Kmett')
  , (3, 'Jasper', 'Van der Jeugt')
  , (4, 'Joe', 'Nelson')
  , (5, 'Gabriel', 'Gonzales')
  , (6, 'Artyom', 'Kazak')
  , (7, 'Aditya', 'Bhargava')
  , (8, 'Haskell', 'WikiBook')
  ;

INSERT INTO "item_author" (id, item_id, author_id) VALUES
    (1, 1, 1)
  , (2, 2, 1)
  , (3, 3, 3)
  , (4, 4, 4)
  , (5, 5, 5)
  , (6, 6, 6)
  , (7, 7, 8)
  , (8, 8, 7)
  , (9, 9, 6)
  ;

INSERT INTO "item_tag" (id, item_id, tag_id) VALUES
    (1, 1, 1)
  , (2, 1, 4)
  , (3, 2, 6)
  , (4, 3, 13)
  , (5, 3, 14)
  , (6, 4, 10)
  , (7, 4, 11)
  , (8, 4, 12)
  , (9, 5, 15)
  , (10, 6, 15)
  , (11, 7, 15)
  , (12, 7, 16)
  , (13, 8, 2)
  , (14, 8, 3)
  , (15, 8, 4)
  , (16, 9, 17)
  , (17, 9, 18)
  ;

COMMIT;
