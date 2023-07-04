BEGIN;

ALTER TABLE services ADD COLUMN organisation TEXT NOT NULL DEFAULT 'CalMac';
UPDATE services s SET organisation = o.name FROM organisations o WHERE s.organisation_id = o.organisation_id;
ALTER TABLE services DROP COLUMN IF EXISTS organisation_id;

ALTER TABLE vessels DROP COLUMN IF EXISTS organisation_id;

DROP TABLE IF EXISTS organisations;

DELETE FROM service_locations WHERE service_id = 2000 AND location_id = 64;
DELETE FROM service_locations WHERE service_id = 2000 AND location_id = 65;
DELETE FROM services WHERE service_id = 2000;
DELETE FROM locations WHERE location_id = 64;
DELETE FROM locations WHERE location_id = 65;

COMMIT;
