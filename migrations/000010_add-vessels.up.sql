CREATE TABLE vessels (
    mmsi INTEGER PRIMARY KEY ,
    name TEXT NOT NULL,
    speed NUMERIC NOT NULL,
    course NUMERIC NOT NULL,
    latitude NUMERIC NOT NULL,
    longitude NUMERIC NOT NULL,
    last_received TIMESTAMPTZ NOT NULL,
    updated TIMESTAMPTZ NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);