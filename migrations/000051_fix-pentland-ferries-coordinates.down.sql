BEGIN;

UPDATE locations
SET coordinate = ST_SetSRID(ST_Point(58.641464, -3.228096), 4326)
WHERE location_id = 98;

UPDATE locations
SET coordinate = ST_SetSRID(ST_Point(58.824266, -2.958273), 4326)
WHERE location_id = 99;

COMMIT;
