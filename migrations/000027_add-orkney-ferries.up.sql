BEGIN;

INSERT INTO organisations (organisation_id, name, website, local_phone, international_phone, email, x, facebook) VALUES (5, 'Orkney Ferries', 'https://www.orkneyferries.co.uk/', '01856 872044', '+44 1856 872044', 'info@orkneyferries.co.uk', 'https://twitter.com/OrkneyFerries', 'https://www.facebook.com/OrkneyFerriesLtd/');

-- OF5
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4000, 'EDAY', 'Kirkwall - Eday - Stronsay - Sanday - Rapness', 5, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (77, 'Kirkwall', ST_SetSRID(ST_Point(58.98646780169982, -2.9603495092811527), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (78, 'Eday', ST_SetSRID(ST_Point(59.15554743669383, -2.7473148574977544), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (79, 'Stronsay', ST_SetSRID(ST_Point(59.14267302491262, -2.597842732772849), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (80, 'Sanday', ST_SetSRID(ST_Point(59.190785969770495, -2.6964419915877955), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (81, 'Rapness', ST_SetSRID(ST_Point(59.248702844714344, -2.858451682387489), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (4000, 77);
INSERT INTO service_locations (service_id, location_id) VALUES (4000, 78);
INSERT INTO service_locations (service_id, location_id) VALUES (4000, 79);
INSERT INTO service_locations (service_id, location_id) VALUES (4000, 80);
INSERT INTO service_locations (service_id, location_id) VALUES (4000, 81);

-- OF5
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4001, 'SANDAY', 'Kirkwall - Eday - Stronsay - Sanday - Rapness', 5, -99, now());
INSERT INTO service_locations (service_id, location_id) VALUES (4001, 77);
INSERT INTO service_locations (service_id, location_id) VALUES (4001, 78);
INSERT INTO service_locations (service_id, location_id) VALUES (4001, 79);
INSERT INTO service_locations (service_id, location_id) VALUES (4001, 80);
INSERT INTO service_locations (service_id, location_id) VALUES (4001, 81);

-- OF5
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4002, 'STRONSAY', 'Kirkwall - Eday - Stronsay - Sanday - Rapness', 5, -99, now());
INSERT INTO service_locations (service_id, location_id) VALUES (4002, 77);
INSERT INTO service_locations (service_id, location_id) VALUES (4002, 78);
INSERT INTO service_locations (service_id, location_id) VALUES (4002, 79);
INSERT INTO service_locations (service_id, location_id) VALUES (4002, 80);
INSERT INTO service_locations (service_id, location_id) VALUES (4002, 81);

-- OF5
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4003, 'WESTRAY', 'Kirkwall - Eday - Stronsay - Sanday - Rapness', 5, -99, now());
INSERT INTO service_locations (service_id, location_id) VALUES (4003, 77);
INSERT INTO service_locations (service_id, location_id) VALUES (4003, 78);
INSERT INTO service_locations (service_id, location_id) VALUES (4003, 79);
INSERT INTO service_locations (service_id, location_id) VALUES (4003, 80);
INSERT INTO service_locations (service_id, location_id) VALUES (4003, 81);

-- OF4
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4004, 'SHAPINSAY', 'Kirkwall - Shapinsay', 5, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (82, 'Kirkwall Shapinsay', ST_SetSRID(ST_Point(58.98530772397149, -2.9604274613197075), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (83, 'Shapinsay', ST_SetSRID(ST_Point(59.031464993313435, -2.90954899323327964), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (4004, 82);
INSERT INTO service_locations (service_id, location_id) VALUES (4004, 83);

-- OF1
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4005, 'GRAEMSAY', 'Stromness - Graemsay - Hoy', 5, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (84, 'Stromness', ST_SetSRID(ST_Point(58.9640947625072, -3.295304261784716), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (85, 'Graemsay', ST_SetSRID(ST_Point(58.93119306928956, -3.2672566315248606), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (86, 'Hoy', ST_SetSRID(ST_Point(58.916302446449635, -3.3118362066164497), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (4005, 84);
INSERT INTO service_locations (service_id, location_id) VALUES (4005, 85);
INSERT INTO service_locations (service_id, location_id) VALUES (4005, 86);

-- OF2
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4006, 'HOUTON', 'Houton - Flotta - Lyness - Longhope', 5, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (87, 'Houton', ST_SetSRID(ST_Point(58.917335897314416, -3.1838163644115665), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (88, 'Flotta', ST_SetSRID(ST_Point(58.83807286115842, -3.1282277823106046), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (89, 'Lyness', ST_SetSRID(ST_Point(58.83353714926902, -3.191641626189918), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (90, 'Longhope', ST_SetSRID(ST_Point(58.80042265386166, -3.207042537317379), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (4006, 87);
INSERT INTO service_locations (service_id, location_id) VALUES (4006, 88);
INSERT INTO service_locations (service_id, location_id) VALUES (4006, 89);
INSERT INTO service_locations (service_id, location_id) VALUES (4006, 90);

-- OF3
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4007, 'ROUSAY, EGILSAY & WYRE', 'Tingwall - Rousay - Egilsay - Wyre', 5, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (91, 'Wyre', ST_SetSRID(ST_Point(59.12483221195805, -2.9724840291593946), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (92, 'Rousay', ST_SetSRID(ST_Point(59.13061386489054, -2.9861748085213837), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (93, 'Tingwall', ST_SetSRID(ST_Point(59.08867039132888, -3.0434806516423203), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (94, 'Egilsay', ST_SetSRID(ST_Point(59.15580615476281, -2.942737599063998), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (4007, 91);
INSERT INTO service_locations (service_id, location_id) VALUES (4007, 92);
INSERT INTO service_locations (service_id, location_id) VALUES (4007, 93);
INSERT INTO service_locations (service_id, location_id) VALUES (4007, 94);

-- OF7
INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (4008, 'PIEROWALL - PAPA WESTRAY', 'Westray Pierowall - Papa Westray', 5, -99, now());
INSERT INTO locations (location_id, name, coordinate) VALUES (95, 'Pierowall', ST_SetSRID(ST_Point(59.3232945827641, -2.9746761333592557), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (96, 'Papa Westray', ST_SetSRID(ST_Point(59.32685487460761, -2.8915579942483736), 4326));
INSERT INTO service_locations (service_id, location_id) VALUES (4008, 95);
INSERT INTO service_locations (service_id, location_id) VALUES (4008, 96);

COMMIT;
