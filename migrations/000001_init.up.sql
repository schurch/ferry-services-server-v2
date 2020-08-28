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
    device_type INTEGER NOT NULL,
    endpoint_arn TEXT NOT NULL,
    updated TIMESTAMPTZ NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE installation_services (
    installation_id UUID NOT NULL REFERENCES installations (installation_id),
    service_id INTEGER NULL REFERENCES services (service_id),
    PRIMARY KEY(installation_id, service_id)
);

CREATE TABLE locations (
    location_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    latitude NUMERIC NOT NULL,
    longitude NUMERIC NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE service_locations (
    service_id INTEGER NULL REFERENCES services (service_id),
    location_id INTEGER NULL REFERENCES locations (location_id),
    PRIMARY KEY(service_id, location_id)
);

INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (14, '1', 'ARDNAMURCHAN', 'Tobermory (TOB) - Kilchoan (KIC)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (5, '2', 'ARRAN', 'Ardrossan (ARD) - Brodick (BRO)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (6, '3', 'ARRAN', 'Claonaig (CLA) - Lochranza (LRA)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (21, '4', 'BARRA and ERISKAY', 'Ardmhor, Barra (AMH) - Eriskay (ERI)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (7, '11', 'CUMBRAE', 'Largs (LAR) - Millport, Cumbrae Slip (CUM)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (20, '5', 'BARRA ', 'Oban (OBA) - Castlebay (CAS)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (4, '6', 'BUTE', 'Colintraive (CTR) - Rhubodach (RHU)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (3, '7', 'BUTE', 'Wemyss Bay (WEM) - Rothesay (ROT)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (16, '8', 'COLL and TIREE', 'Oban (OBA) - Coll (CLL) - Tiree (TIR)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (10, '9', 'COLONSAY', 'Oban (OBA) - Colonsay (CSA) - Port Askaig (PAS) - Kennacraig (KEN)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (1, '10', 'COWAL & DUNOON', 'Gourock (GOU) - Dunoon (DUN)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (8, '12', 'GIGHA', 'Tayinloan (TAY) - Gigha (GIG)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (24, '13', 'HARRIS', 'Uig (UIG) - Tarbert (TAR)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (13, '14', 'IONA', 'Fionnphort (FIO) - Iona (ION)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (9, '15', 'ISLAY', 'Kennacraig (KEN) - Port Askaig (PAS), Kennacraig (KEN) - Port Ellen (PEL)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (38, '16', 'KERRERA', 'Gallanach (GAL) - Kerrera (KER)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (2, '17', 'COWAL and KINTYRE', 'Tarbert, Loch Fyne (TLF) - Portavadie (POR)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (25, '18', 'LEWIS', 'Ullapool (ULL) - Stornoway (STO)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (35, '19', 'LEWIS FREIGHT', 'Freight Ullapool -Stornoway ', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (15, '20', 'LISMORE', 'Oban (OBA) - Lismore (LIS)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (11, '21', 'MULL', 'Oban (OBA) - Craignure (CRA)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (12, '22', 'MULL', 'Lochaline (LAL) - Fishnish (FIS)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (22, '23', 'NORTH UIST', 'Uig, Skye (UIG) - Lochmaddy (LMA)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (23, '24', 'NORTH UIST and HARRIS', 'Berneray (BER) - Leverburgh (LEV)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (17, '25', 'RAASAY', 'Sconser (SCO) - Raasay (RAA)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (18, '26', 'SKYE', 'Mallaig (MAL) - Armadale (ARM)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (19, '27', 'SMALL ISLES', 'Mallaig (MAL) - Small Isles (SIS)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (37, '28', 'SOUTH UIST', 'Mallaig (MAL) / Oban (OBA) - Lochboisdale (LBO)', -99, now());
INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (39, '29', 'KILCREGGAN & ROSNEATH', 'Gourock (GOU) - Kilcreggan (KIL)', -99, now());
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (1, 'Glasgow Central', 55.860524, -4.258041);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (2, 'Ardrossan Harbour', 55.639868, -4.821088);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (3, 'Ardrossan', 55.640516, -4.823062);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (4, 'Brodick', 55.576606, -5.139172);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (5, 'Glasgow Central', 55.860524, -4.258041);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (6, 'Wemyss Bay', 55.876138, -4.889059);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (7, 'Wemyss Bay', 55.87573, -4.8908);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (8, 'Rothesay', 55.83848, -5.05421);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (9, 'Stornoway', 58.206822, -6.386586);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (10, 'Ullapool Bay', 57.894939, -5.160442);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (11, 'Largs', 55.794945, -4.871013);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (12, 'Cumbrae Slip', 55.78671, -4.898246);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (13, 'Tobermory', 56.6233, -6.06334);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (14, 'Kilchoan', 56.68855, -6.09396);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (15, 'Tarbert (Loch Fyne)', 55.86615, -5.40376);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (16, 'Lochranza', 55.707714, -5.301985);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (17, 'Barra', 57.0084, -7.40475);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (18, 'Eriskay', 57.07091, -7.30825);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (19, 'Oban', 56.41158, -5.47725);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (20, 'Castlebay', 56.953846, -7.488251);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (21, 'Lochboisdale', 57.152201, -7.303969);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (22, 'Colintraive', 55.923353, -5.152758);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (23, 'Rhubodach', 55.92067, -5.15888);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (24, 'Coll', 56.615059, -6.524241);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (25, 'Tiree', 56.508324, -6.798924);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (26, 'Colonsay', 56.06858, -6.18819);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (27, 'Port Askaig', 55.847707, -6.105102);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (28, 'Kennacraig', 55.8067, -5.4834);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (29, 'Portavadie', 55.87649, -5.31654);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (30, 'Tayinloan', 55.65755, -5.6691);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (31, 'Gigha', 55.678637, -5.733597);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (32, 'Uig', 57.58634, -6.376447);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (33, 'Tarbert', 57.896848, -6.798668);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (34, 'Fionnphort', 56.325611, -6.369403);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (35, 'Iona', 56.330225, -6.392257);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (36, 'Port Ellen', 55.62781, -6.18981);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (37, 'Lismore', 56.511159, -5.492068);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (38, 'Craignure', 56.47074, -5.70629);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (39, 'Lochmaddy', 57.596518, -7.157672);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (40, 'Berneray', 57.702302, -7.180422);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (41, 'Leverburgh', 57.76654, -7.025258);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (42, 'Sconser', 57.313967, -6.110329);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (43, 'Raasay', 57.351034, -6.082454);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (44, 'Mallaig', 57.006834, -5.828069);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (45, 'Armadale', 57.064596, -5.894743);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (46, 'Eigg', 56.8772, -6.12995);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (47, 'Muck', 56.83364, -6.226905);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (48, 'Rum', 57.010862, -6.264991);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (49, 'Canna', 57.056171, -6.490296);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (50, 'Lochaline', 56.53666, -5.77502);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (51, 'Fishnish', 56.51472, -5.81032);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (52, 'Claonaig', 55.75106, -5.38787);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (53, 'Campbeltown', 55.424047, -5.599388);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (54, 'Gallanach', 56.396995, -5.510373);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (55, 'Kerrera', 56.399859, -5.517498);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (56, 'Gourock', 55.959938, -4.814372);
INSERT INTO locations (location_id, name, latitude, longitude) VALUES (57, 'Dunoon', 55.947006, -4.921328);
INSERT INTO service_locations (service_id, location_id) VALUES (14, 13);
INSERT INTO service_locations (service_id, location_id) VALUES (14, 14);
INSERT INTO service_locations (service_id, location_id) VALUES (5, 3);
INSERT INTO service_locations (service_id, location_id) VALUES (5, 4);
INSERT INTO service_locations (service_id, location_id) VALUES (6, 15);
INSERT INTO service_locations (service_id, location_id) VALUES (6, 16);
INSERT INTO service_locations (service_id, location_id) VALUES (6, 52);
INSERT INTO service_locations (service_id, location_id) VALUES (21, 17);
INSERT INTO service_locations (service_id, location_id) VALUES (21, 18);
INSERT INTO service_locations (service_id, location_id) VALUES (20, 19);
INSERT INTO service_locations (service_id, location_id) VALUES (20, 20);
INSERT INTO service_locations (service_id, location_id) VALUES (20, 21);
INSERT INTO service_locations (service_id, location_id) VALUES (3, 7);
INSERT INTO service_locations (service_id, location_id) VALUES (3, 8);
INSERT INTO service_locations (service_id, location_id) VALUES (4, 22);
INSERT INTO service_locations (service_id, location_id) VALUES (4, 23);
INSERT INTO service_locations (service_id, location_id) VALUES (16, 19);
INSERT INTO service_locations (service_id, location_id) VALUES (16, 24);
INSERT INTO service_locations (service_id, location_id) VALUES (16, 25);
INSERT INTO service_locations (service_id, location_id) VALUES (10, 19);
INSERT INTO service_locations (service_id, location_id) VALUES (10, 26);
INSERT INTO service_locations (service_id, location_id) VALUES (10, 27);
INSERT INTO service_locations (service_id, location_id) VALUES (10, 28);
INSERT INTO service_locations (service_id, location_id) VALUES (2, 15);
INSERT INTO service_locations (service_id, location_id) VALUES (2, 29);
INSERT INTO service_locations (service_id, location_id) VALUES (7, 11);
INSERT INTO service_locations (service_id, location_id) VALUES (7, 12);
INSERT INTO service_locations (service_id, location_id) VALUES (8, 30);
INSERT INTO service_locations (service_id, location_id) VALUES (8, 31);
INSERT INTO service_locations (service_id, location_id) VALUES (24, 32);
INSERT INTO service_locations (service_id, location_id) VALUES (24, 33);
INSERT INTO service_locations (service_id, location_id) VALUES (13, 34);
INSERT INTO service_locations (service_id, location_id) VALUES (13, 35);
INSERT INTO service_locations (service_id, location_id) VALUES (9, 27);
INSERT INTO service_locations (service_id, location_id) VALUES (9, 28);
INSERT INTO service_locations (service_id, location_id) VALUES (9, 36);
INSERT INTO service_locations (service_id, location_id) VALUES (25, 9);
INSERT INTO service_locations (service_id, location_id) VALUES (25, 10);
INSERT INTO service_locations (service_id, location_id) VALUES (35, 9);
INSERT INTO service_locations (service_id, location_id) VALUES (35, 10);
INSERT INTO service_locations (service_id, location_id) VALUES (15, 19);
INSERT INTO service_locations (service_id, location_id) VALUES (15, 37);
INSERT INTO service_locations (service_id, location_id) VALUES (11, 19);
INSERT INTO service_locations (service_id, location_id) VALUES (11, 38);
INSERT INTO service_locations (service_id, location_id) VALUES (12, 50);
INSERT INTO service_locations (service_id, location_id) VALUES (12, 51);
INSERT INTO service_locations (service_id, location_id) VALUES (22, 32);
INSERT INTO service_locations (service_id, location_id) VALUES (22, 39);
INSERT INTO service_locations (service_id, location_id) VALUES (23, 40);
INSERT INTO service_locations (service_id, location_id) VALUES (23, 41);
INSERT INTO service_locations (service_id, location_id) VALUES (17, 42);
INSERT INTO service_locations (service_id, location_id) VALUES (17, 43);
INSERT INTO service_locations (service_id, location_id) VALUES (18, 44);
INSERT INTO service_locations (service_id, location_id) VALUES (18, 45);
INSERT INTO service_locations (service_id, location_id) VALUES (19, 44);
INSERT INTO service_locations (service_id, location_id) VALUES (19, 46);
INSERT INTO service_locations (service_id, location_id) VALUES (19, 47);
INSERT INTO service_locations (service_id, location_id) VALUES (19, 48);
INSERT INTO service_locations (service_id, location_id) VALUES (19, 49);
INSERT INTO service_locations (service_id, location_id) VALUES (37, 21);
INSERT INTO service_locations (service_id, location_id) VALUES (37, 44);
INSERT INTO service_locations (service_id, location_id) VALUES (38, 54);
INSERT INTO service_locations (service_id, location_id) VALUES (38, 55);
INSERT INTO service_locations (service_id, location_id) VALUES (1, 56);
INSERT INTO service_locations (service_id, location_id) VALUES (1, 57);
