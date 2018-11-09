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

COMMIT;
