CREATE TABLE tx2_service_mappings (
    service_id INT NOT NULL REFERENCES services(service_id) ON DELETE CASCADE,
    service_code TEXT NOT NULL,
    created TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (service_id, service_code)
);

CREATE INDEX tx2_service_mappings_service_code_idx
    ON tx2_service_mappings(service_code);
