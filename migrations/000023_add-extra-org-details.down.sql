BEGIN;

ALTER TABLE organisations DROP COLUMN website;
ALTER TABLE organisations DROP COLUMN local_phone;
ALTER TABLE organisations DROP COLUMN international_phone;
ALTER TABLE organisations DROP COLUMN email;
ALTER TABLE organisations DROP COLUMN x;
ALTER TABLE organisations DROP COLUMN facebook;

COMMIT;
