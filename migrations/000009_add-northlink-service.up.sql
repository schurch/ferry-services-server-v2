ALTER TABLE services ADD COLUMN organisation TEXT NOT NULL DEFAULT 'CalMac';
ALTER TABLE services ALTER COLUMN organisation DROP DEFAULT;

ALTER TABLE installation_services DROP CONSTRAINT installation_services_installation_id_fkey,
ADD CONSTRAINT installation_services_installation_id_fkey FOREIGN KEY (installation_id) REFERENCES installations(installation_id) ON DELETE CASCADE;

INSERT INTO services (service_id, sort_order, area, route, organisation, status, updated) VALUES (1000, '24', 'ORKNEY & SHETLAND', 'Scrabster - Stromness / Aberdeen - Kirkwall - Lerwick', 'NorthLink', -99, now());

INSERT INTO locations (location_id, name, latitude, longitude) VALUES (59, 'Scrabster', 58.61252430922174, -3.545534457670658);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (60, 'Stromness', 58.96421632953152, -3.2954037645564296);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (61, 'Aberdeen', 57.145172030019346, -2.091230925036305);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (62, 'Kirkwall', 58.99989823953207, -2.9742511887813246);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (63, 'Lerwick', 60.16270380403575, -1.1589239389948884);

INSERT INTO service_locations (service_id, location_id) VALUES (1000, 59);
INSERT INTO service_locations (service_id, location_id) VALUES (1000, 60);
INSERT INTO service_locations (service_id, location_id) VALUES (1000, 61);
INSERT INTO service_locations (service_id, location_id) VALUES (1000, 62);
INSERT INTO service_locations (service_id, location_id) VALUES (1000, 63);
