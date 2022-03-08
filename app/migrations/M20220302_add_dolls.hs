{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module M20220302_add_dolls where

import           Data.String.QQ
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Database.Rivet.V0

migrate :: Migration IO ()
migrate = sql up down

up :: Text
up = T.pack $
  [s|

DROP TABLE IF EXISTS dolls CASCADE;
CREATE TABLE dolls (
  id serial primary key,
  name text not null,
  otakuworld_url text unique,
  hash text not null unique,
  location text unique,
  error text,
  created_at timestamptz default now(),
  updated_at timestamptz default now()
);

DROP TABLE IF EXISTS artists CASCADE;
CREATE TABLE artists (
  id serial primary key,
  name text not null,
  email text,
  website text,
  user_id integer references users(id),
  created_at timestamptz default now(),
  updated_at timestamptz default now()
);

DROP TABLE IF EXISTS doll_artists CASCADE;
CREATE TABLE doll_artists (
  id serial primary key,
  doll_id integer references dolls(id),
  artist_id integer references artists(id),
  created_at timestamptz default now()
);

CREATE UNIQUE INDEX ON dolls (hash);
CREATE UNIQUE INDEX ON dolls (otakuworld_url);

DROP FUNCTION IF EXISTS set_updated_at() CASCADE;
CREATE FUNCTION set_updated_at() RETURNS trigger AS $$
  BEGIN
    NEW.updated_at := now();
    RETURN NEW;
  END
$$ LANGUAGE plpgsql;

CREATE TRIGGER dolls_updated
  BEFORE UPDATE ON dolls
  FOR EACH ROW
  EXECUTE PROCEDURE set_updated_at();

CREATE TRIGGER artists_updated
  BEFORE UPDATE ON artists
  FOR EACH ROW
  EXECUTE PROCEDURE set_updated_at();

  |]

down :: Text
down = T.pack $
  [s|

DROP TABLE IF EXISTS doll_artists CASCADE;
DROP TABLE IF EXISTS dolls CASCADE;
DROP TABLE IF EXISTS artists CASCADE;

  |]