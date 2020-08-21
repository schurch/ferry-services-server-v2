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

CREATE TABLE installations (
    installation_id UUID PRIMARY KEY,
    device_token TEXT NOT NULL,
    device_type TEXT NOT NULL,
    endpoint_arn TEXT NOT NULL,
    updated TIMESTAMPTZ NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE installation_services (
    installation_id UUID NOT NULL REFERENCES installations (installation_id),
    service_id INTEGER NULL REFERENCES services (service_id),
    PRIMARY KEY(installation_id, service_id)
);