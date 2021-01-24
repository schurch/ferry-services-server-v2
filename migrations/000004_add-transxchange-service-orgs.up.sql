CREATE TABLE serviced_organisations (
    serviced_organisation_code TEXT PRIMARY KEY,
    name TEXT
);

CREATE TABLE serviced_organisation_working_days (
  serviced_organisation_code TEXT NOT NULL REFERENCES serviced_organisations (serviced_organisation_code),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  PRIMARY KEY(serviced_organisation_code, start_date, end_date)
);

ALTER TABLE vehicle_journeys ADD COLUMN non_operation_serviced_organisation_code TEXT NULL;