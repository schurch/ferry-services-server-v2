{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Vessels
  ( saveVessel,
    getVessels,
    getServiceVessels,
  )
where

import App.Env (Application)
import Control.Monad (void)
import Database.Connection (withConnection)
import Database.SQLite.Simple
  ( execute,
    query_,
  )
import Database.SQLite.Simple.QQ (sql)
import Types

saveVessel :: Vessel -> Application ()
saveVessel vessel = withConnection $ \connection -> void $ do
  execute
    connection
    [sql|
      INSERT INTO vessels (mmsi, name, speed, course, latitude, longitude, last_received, updated, organisation_id)
        VALUES (?,?,?,?,?,?,?,?,?)
        ON CONFLICT (mmsi) DO UPDATE
          SET name = excluded.name,
              speed = excluded.speed,
              course = excluded.course,
              latitude = excluded.latitude,
              longitude = excluded.longitude,
              last_received = excluded.last_received,
              updated = excluded.updated,
              organisation_id = excluded.organisation_id
    |]
    ( vesselMmsi vessel,
      vesselName vessel,
      vesselSpeed vessel,
      vesselCourse vessel,
      coordinateLatitude $ vesselCoordinate vessel,
      coordinateLongitude $ vesselCoordinate vessel,
      vesselLastReceived vessel,
      vesselUpdated vessel,
      vesselOrganisationID vessel
    )

getVessels :: Application [Vessel]
getVessels = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT mmsi, name, speed, course, latitude, longitude, last_received, updated, organisation_id
      FROM vessels
    |]

getServiceVessels :: Application [ServiceVessel]
getServiceVessels = withConnection $ \connection ->
  query_
    connection
    [sql|
      WITH bounding_box AS (
        SELECT
          sl.service_id,
          MIN(l.latitude) - 0.02 AS min_latitude,
          MAX(l.latitude) + 0.02 AS max_latitude,
          MIN(l.longitude) - 0.02 AS min_longitude,
          MAX(l.longitude) + 0.02 AS max_longitude
        FROM locations l
        JOIN service_locations sl ON l.location_id = sl.location_id
        GROUP BY sl.service_id
      )
      SELECT s.service_id, v.mmsi, v.name, v.speed, v.course, v.latitude, v.longitude, v.last_received, v.updated, v.organisation_id
      FROM vessels v, bounding_box b
      JOIN services s on s.service_id = b.service_id
      WHERE v.latitude BETWEEN b.min_latitude AND b.max_latitude
        AND v.longitude BETWEEN b.min_longitude AND b.max_longitude
        AND s.organisation_id = v.organisation_id;
    |]
