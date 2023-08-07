BEGIN;

UPDATE organisations SET international_phone = NULL WHERE organisation_id = 3;
UPDATE organisations SET email = 'enquiries@western-ferries.co.uk' WHERE organisation_id = 3;

COMMIT;
