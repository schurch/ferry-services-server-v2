BEGIN;

INSERT INTO organisations (organisation_id, name, website, local_phone, international_phone, email, x, facebook) VALUES (4, 'Shetland Ferries', 'https://www.shetland.gov.uk/ferries', '01806 244200', NULL, 'ferries@shetland.gov.uk', 'https://twitter.com/ShetIslandsCll', 'https://www.facebook.com/ShetlandIslandsCouncil/');

-- SF 2
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (3000, 'BLUEMULL SOUND', 'Gutcher - Belmont - Hamars Ness', 4, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (66, 'Gutcher', ST_SetSRID(ST_Point(60.67302336320226, -0.9964222948573699), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (67, 'Belmont', ST_SetSRID(ST_Point(60.683245697249724, -0.966809460926674), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (68, 'Hamars Ness', ST_SetSRID(ST_Point(60.62920742111861, -0.931458525094521), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (3000, 66);
INSERT INTO service_locations (service_id, location_id) VALUES (3000, 67);
INSERT INTO service_locations (service_id, location_id) VALUES (3000, 68);

-- SF 1
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (3001, 'YELL', 'Toft - Ulsta', 4, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (69, 'Toft', ST_SetSRID(ST_Point(60.46679304368115, -1.2068379688066269), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (70, 'Ulsta', ST_SetSRID(ST_Point(60.49638392192762, -1.1585186085702022), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (3001, 69);
INSERT INTO service_locations (service_id, location_id) VALUES (3001, 70);

-- SF 4
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (3002, 'BRESSAY', 'Lerwick - Bressay', 4, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (71, 'Lerwick', ST_SetSRID(ST_Point(60.15592026531444, -1.1427881939348081), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (72, 'Bressay', ST_SetSRID(ST_Point(60.15720064370729, -1.1232174110338675), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (3002, 71);
INSERT INTO service_locations (service_id, location_id) VALUES (3002, 72);

-- SF 5
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (3003, 'WHALSAY', 'Laxo - Symbister', 4, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (73, 'Laxo', ST_SetSRID(ST_Point(60.352141839514786, -1.1702833067214111), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (74, 'Symbister', ST_SetSRID(ST_Point(60.34191010787282, -1.028640592585364), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (3003, 73);
INSERT INTO service_locations (service_id, location_id) VALUES (3003, 74);

-- SF 3
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (3004, 'SKERRIES', 'Laxo - Symbister - Skerries - Vidlin - Lerwick', 4, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (75, 'Skerries', ST_SetSRID(ST_Point(60.42273340335759, -0.7511959108313978), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (76, 'Vidlin', ST_SetSRID(ST_Point(60.37282293039965, -1.1293572113730086), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (3004, 71);
INSERT INTO service_locations (service_id, location_id) VALUES (3004, 73);
INSERT INTO service_locations (service_id, location_id) VALUES (3004, 74);
INSERT INTO service_locations (service_id, location_id) VALUES (3004, 75);
INSERT INTO service_locations (service_id, location_id) VALUES (3004, 76);

COMMIT;
