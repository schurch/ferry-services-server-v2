BEGIN;

CREATE TABLE rail_departures (
    departure_crs TEXT NOT NULL,
    departure_name TEXT NOT NULL,
    destination_crs TEXT NOT NULL,
    destination_name TEXT NOT NULL,
    scheduled_departure_time TIME NOT NULL,
    estimated_departure_time TEXT NOT NULL,
    cancelled BOOLEAN NOT NULL,
    platform TEXT NULL,
    location_id INTEGER NOT NULL REFERENCES locations (location_id),
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY(departure_crs, destination_crs, scheduled_departure_time)
);

COMMIT;
