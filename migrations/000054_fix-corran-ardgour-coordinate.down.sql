UPDATE locations
SET coordinate = ST_SetSRID(ST_Point(56.72270, -5.24278), 4326)
WHERE location_id = 101;
