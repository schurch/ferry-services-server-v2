BEGIN;

ALTER TABLE locations ADD COLUMN coordinate geometry(POINT, 4326) NULL;
UPDATE locations SET coordinate = ST_SetSRID(ST_Point(latitude, longitude), 4326);
ALTER TABLE locations ALTER COLUMN coordinate SET NOT NULL;
ALTER TABLE locations DROP COLUMN latitude;
ALTER TABLE locations DROP COLUMN longitude;

ALTER TABLE vessels ADD COLUMN coordinate geometry(POINT, 4326) NULL;
UPDATE vessels SET coordinate = ST_SetSRID(ST_Point(latitude, longitude), 4326);
ALTER TABLE vessels ALTER COLUMN coordinate SET NOT NULL;
ALTER TABLE vessels DROP COLUMN latitude;
ALTER TABLE vessels DROP COLUMN longitude;

COMMIT;
