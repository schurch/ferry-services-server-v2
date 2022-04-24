BEGIN;

ALTER TABLE locations ADD COLUMN latitude NUMERIC NULL;
ALTER TABLE locations ADD COLUMN longitude NUMERIC NULL;
UPDATE locations SET latitude = ST_X(coordinate);
UPDATE locations SET longitude = ST_Y(coordinate);
ALTER TABLE locations ALTER COLUMN latitude SET NOT NULL;
ALTER TABLE locations ALTER COLUMN longitude SET NOT NULL;
ALTER TABLE locations DROP COLUMN coordinate;

ALTER TABLE vessels ADD COLUMN latitude NUMERIC NULL;
ALTER TABLE vessels ADD COLUMN longitude NUMERIC NULL;
UPDATE vessels SET latitude = ST_X(coordinate);
UPDATE vessels SET longitude = ST_Y(coordinate);
ALTER TABLE vessels ALTER COLUMN latitude SET NOT NULL;
ALTER TABLE vessels ALTER COLUMN longitude SET NOT NULL;
ALTER TABLE vessels DROP COLUMN coordinate;

COMMIT;
