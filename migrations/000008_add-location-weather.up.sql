BEGIN;

CREATE TABLE location_weather (
    location_id INTEGER PRIMARY KEY REFERENCES locations (location_id),
    description TEXT NOT NULL,
    icon TEXT NOT NULL,
    temperature NUMERIC NOT NULL,
    wind_speed NUMERIC NOT NULL,
    wind_direction NUMERIC NOT NULL,
    updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

DELETE FROM locations WHERE location_id IN (1,2,5,6);

COMMIT;
