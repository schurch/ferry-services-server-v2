BEGIN;

ALTER TABLE organisations ADD COLUMN website TEXT NULL;
ALTER TABLE organisations ADD COLUMN local_phone TEXT NULL;
ALTER TABLE organisations ADD COLUMN international_phone TEXT NULL;
ALTER TABLE organisations ADD COLUMN email TEXT NULL;
ALTER TABLE organisations ADD COLUMN x TEXT NULL;
ALTER TABLE organisations ADD COLUMN facebook TEXT NULL;

UPDATE organisations SET website = 'https://calmac.co.uk/' WHERE organisation_id = 1;
UPDATE organisations SET local_phone = '0800 066 5000' WHERE organisation_id = 1;
UPDATE organisations SET international_phone = '+44 1475 650 397' WHERE organisation_id = 1;
UPDATE organisations SET email = 'enquiries@calmac.co.uk' WHERE organisation_id = 1;
UPDATE organisations SET x = 'https://x.com/calmacferries' WHERE organisation_id = 1;
UPDATE organisations SET facebook = 'https://www.facebook.com/calmacferries' WHERE organisation_id = 1;

UPDATE organisations SET website = 'https://www.northlinkferries.co.uk' WHERE organisation_id = 2;
UPDATE organisations SET local_phone = '0800 111 4422' WHERE organisation_id = 2;
UPDATE organisations SET international_phone = '+44 1856 885 500' WHERE organisation_id = 2;
UPDATE organisations SET email = 'reservations@northlinkferries.co.uk' WHERE organisation_id = 2;
UPDATE organisations SET x = 'https://x.com/NLFerries' WHERE organisation_id = 2;
UPDATE organisations SET facebook = 'https://www.facebook.com/NorthLinkFerries/' WHERE organisation_id = 2;

UPDATE organisations SET website = 'https://www.western-ferries.co.uk/' WHERE organisation_id = 3;
UPDATE organisations SET local_phone = '01369 704452' WHERE organisation_id = 3;
UPDATE organisations SET international_phone = 'enquiries@western-ferries.co.uk' WHERE organisation_id = 3;
UPDATE organisations SET email = '' WHERE organisation_id = 3;
UPDATE organisations SET x = 'https://x.com/Western_Ferries' WHERE organisation_id = 3;
UPDATE organisations SET facebook = 'https://www.facebook.com/WesternFerries' WHERE organisation_id = 3;

COMMIT;
