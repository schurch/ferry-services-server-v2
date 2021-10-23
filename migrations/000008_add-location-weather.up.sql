CREATE TABLE location_weather (
    location_weather_id SERIAL PRIMARY KEY,
    location_id INTEGER NOT NULL UNIQUE REFERENCES locations (location_id),
    description TEXT NOT NULL,
    icon TEXT NOT NULL,
    temperature NUMERIC NOT NULL,
    wind_speed NUMERIC NOT NULL,
    wind_direction NUMERIC NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);
