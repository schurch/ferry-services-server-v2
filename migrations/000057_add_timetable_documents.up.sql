BEGIN;

CREATE TABLE timetable_documents (
    timetable_document_id SERIAL PRIMARY KEY,
    organisation_id INTEGER NOT NULL REFERENCES organisations (organisation_id),
    title TEXT NOT NULL,
    source_url TEXT NOT NULL UNIQUE,
    content_hash TEXT NULL,
    content_type TEXT NULL,
    content_length BIGINT NULL,
    last_seen_at TIMESTAMPTZ NOT NULL,
    updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE timetable_document_services (
    timetable_document_id INTEGER NOT NULL REFERENCES timetable_documents (timetable_document_id) ON DELETE CASCADE,
    service_id INTEGER NOT NULL REFERENCES services (service_id),
    PRIMARY KEY (timetable_document_id, service_id)
);

CREATE INDEX timetable_documents_organisation_id_idx
    ON timetable_documents (organisation_id);

CREATE INDEX timetable_document_services_service_id_idx
    ON timetable_document_services (service_id);

COMMIT;
