CREATE TABLE services (
    service_id INTEGER PRIMARY KEY,
    sort_order INTEGER NOT NULL,
    area TEXT NOT NULL,
    route TEXT NOT NULL,
    status INTEGER NOT NULL,
    additional_info TEXT NULL,
    disruption_reason TEXT NULL,
    last_updated_date TIMESTAMPTZ NULL,
    updated TIMESTAMPTZ NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);