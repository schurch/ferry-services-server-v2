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
import Database.PostgreSQL.Simple
  ( execute,
    query_,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Types
  ( ServiceVessel,
    Vessel,
  )

saveVessel :: Vessel -> Application ()
saveVessel vessel = withConnection $ \connection -> void $ do
  execute
    connection
    [sql|
      INSERT INTO vessels (mmsi, name, speed, course, coordinate, last_received, updated, organisation_id)
        VALUES (?,?,?,?,?,?,?,?)
        ON CONFLICT (mmsi) DO UPDATE
          SET name = excluded.name,
              speed = excluded.speed,
              course = excluded.course,
              coordinate = excluded.coordinate,
              last_received = excluded.last_received,
              updated = excluded.updated,
              organisation_id = excluded.organisation_id
    |]
    vessel

getVessels :: Application [Vessel]
getVessels = withConnection $ \connection ->
  query_
    connection
    [sql|
      SELECT mmsi, name, speed, course, coordinate, last_received, updated, organisation_id
      FROM vessels
    |]

getServiceVessels :: Application [ServiceVessel]
getServiceVessels = withConnection $ \connection ->
  query_
    connection
    [sql|
      WITH bounding_box AS (
        SELECT sl.service_id, ST_Expand(ST_Extent(l.coordinate), 0.02) AS bounds
        FROM locations l
        JOIN service_locations sl ON l.location_id = sl.location_id
        GROUP BY sl.service_id
      )
      SELECT s.service_id, v.mmsi, v.name, v.speed, v.course, v.coordinate, v.last_received, v.updated, v.organisation_id
      FROM vessels v, bounding_box b
      JOIN services s on s.service_id = b.service_id
      WHERE v.coordinate && b.bounds AND s.organisation_id = v.organisation_id;
    |]
