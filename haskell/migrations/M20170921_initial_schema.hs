{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module M20170921_initial_schema where

import           Data.String.QQ
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Database.Rivet.V0

migrate :: Migration IO ()
migrate = sql up "DROP TABLE users CASCADE;"


up :: Text
up = T.pack $
  [s|

DROP TABLE IF EXISTS users CASCADE;
CREATE TABLE users (
  id serial primary key,
  username text not null,
  email text not null,
  password text not null,
  created_at timestamptz,
  updated_at timestamptz
);

DROP SEQUENCE IF EXISTS user_ids;
CREATE SEQUENCE user_ids START 1;

DROP EXTENSION IF EXISTS pgcrypto;
CREATE EXTENSION pgcrypto;

DROP FUNCTION IF EXISTS on_record_insert() CASCADE;
CREATE FUNCTION on_record_insert() RETURNS trigger AS $$
  DECLARE
    id_sequence VARCHAR;
  BEGIN
    SELECT TG_ARGV[0] INTO id_sequence;
    NEW.id := nextval(id_sequence);
    NEW.password := crypt(NEW.password, gen_salt('bf', 8));
    NEW.created_at := now();
    NEW.updated_at := now();
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;

DROP FUNCTION IF EXISTS on_record_update() CASCADE;
CREATE FUNCTION on_record_update() RETURNS trigger AS $$
  BEGIN
    NEW.id := OLD.id;
    IF crypt(NEW.password, OLD.password) = OLD.password THEN
      NEW.password := OLD.password;
    ELSE
      NEW.password := crypt(NEW.password, gen_salt('bf', 8));
    END IF;
    NEW.created_at := OLD.created_at;
    NEW.updated_at := now();
    RETURN NEW;
  END
$$ LANGUAGE plpgsql;

CREATE TRIGGER users_insert
  BEFORE INSERT ON users
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_insert('user_ids');

CREATE TRIGGER users_updated
  BEFORE UPDATE ON users
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_update();

  |]
