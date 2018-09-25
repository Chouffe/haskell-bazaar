BEGIN TRANSACTION;

-- Enable UUID plugin
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- Tables Creations
CREATE TABLE IF NOT EXISTS "item" (
  "id"          INTEGER   PRIMARY KEY,
  "title"       VARCHAR   NOT NULL,
  "description" VARCHAR   NULL,
  "url"         VARCHAR   NOT NULL,
  "item_type"   VARCHAR   NOT NULL,
  "created_at"  DATE      NULL      DEFAULT NULL,
  "uuid"        UUID      NOT NULL  DEFAULT UUID_GENERATE_V4()
);

CREATE TABLE IF NOT EXISTS "author" (
  "id"         INTEGER PRIMARY KEY,
  "first_name" VARCHAR NOT NULL,
  "last_name"  VARCHAR NOT NULL,
  "uuid"       UUID    NOT NULL DEFAULT UUID_GENERATE_V4()
);

CREATE TABLE IF NOT EXISTS "item_author" (
  "id"        INTEGER PRIMARY KEY,
  "item_id"   INTEGER NOT NULL REFERENCES "item",
  "author_id" INTEGER NOT NULL REFERENCES "author",
  CONSTRAINT  "unique_item_author" UNIQUE ("item_id","author_id")
);

CREATE TABLE IF NOT EXISTS "tag" (
  "id"   INTEGER PRIMARY KEY,
  "name" VARCHAR NOT NULL
);

CREATE TABLE IF NOT EXISTS "item_tag" (
  "id"       INTEGER PRIMARY KEY,
  "item_id"  INTEGER NOT NULL REFERENCES "item",
  "tag_id"   INTEGER NOT NULL REFERENCES "tag",
  CONSTRAINT "unique_item_tag" UNIQUE ("item_id","tag_id")
);

-- Data Insertion
INSERT INTO "item" (id, title, description, url, item_type, created_at) VALUES
  (1, 'Escape from the ivory tower: the Haskell journey', 'In this talk Simon discusses Haskell’s birth and evolution, including some of the research and engineering challenges he faced in design and implementation.', 'https://www.youtube.com/watch?v=re96UgMk6GQ', 'Video', '2017-03-01'),
  (2, 'Adventure with Types in Haskell', 'Recorded at Oregon Programming Languages Summer School 2013', 'https://www.youtube.com/watch?v=6COvD8oynmI', 'Video', '2013-07-01');

INSERT INTO "tag" (id, name) VALUES
  (1, 'type class'),
  (2, 'functor'),
  (3, 'applicative'),
  (4, 'monad'),
  (5, 'category theory'),
  (6, 'type inference'),
  (7, 'free monad'),
  (8, 'effect'),
  (9, 'extensible effect');

INSERT INTO "author" (id, first_name, last_name) VALUES
  (1, 'Simon', 'Peyton Jones'),
  (2, 'Edward', 'Kmett');

INSERT INTO "item_author" (id, item_id, author_id) VALUES
  (1, 1, 1),
  (2, 2, 1);

INSERT INTO "item_tag" (id, item_id, tag_id) VALUES
  (1, 1, 1),
  (2, 1, 4),
  (3, 2, 6);

COMMIT;

-- INSERT INTO "item" (id, title, description, url, item_type, created_at) VALUES (4, 'title', 'description', 'url', 'Video', '2018-03-01') ;
