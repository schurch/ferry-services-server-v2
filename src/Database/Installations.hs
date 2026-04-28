{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Database.Installations
  ( createInstallation,
    addServiceToInstallation,
    deleteServiceForInstallation,
    getServicesForInstallation,
    updatePushEnabled,
    getInstallationWithID,
    getIntererestedInstallationsForServiceID,
    deleteInstallationWithID,
  )
where

import App.Env (Application)
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Database.Connection (withConnection)
import Database.PostgreSQL.Simple
  ( Only (Only),
    execute,
    query,
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Types
import Types
  ( DeviceType,
    Installation,
  )

createInstallation ::
  UUID -> String -> DeviceType -> String -> UTCTime -> Application ()
createInstallation installationID deviceToken deviceType awsSNSEndpointARN time =
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          INSERT INTO installations (installation_id, device_token, device_type, endpoint_arn, updated)
            VALUES (?,?,?,?,?)
            ON CONFLICT (installation_id) DO UPDATE
              SET installation_id = excluded.installation_id,
                  device_token = excluded.device_token,
                  device_type = excluded.device_type,
                  endpoint_arn = excluded.endpoint_arn,
                  updated = excluded.updated
        |]
        (installationID, deviceToken, deviceType, awsSNSEndpointARN, time)

addServiceToInstallation :: UUID -> Int -> Application ()
addServiceToInstallation installationID serviceID =
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          INSERT INTO installation_services (installation_id, service_id)
          VALUES (?,?)
          ON CONFLICT DO NOTHING
        |]
        (installationID, serviceID)

deleteServiceForInstallation :: UUID -> Int -> Application ()
deleteServiceForInstallation installationID serviceID =
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          DELETE FROM installation_services WHERE installation_id = ? AND service_id = ?
        |]
        (installationID, serviceID)

getServicesForInstallation :: UUID -> Application [Types.Service]
getServicesForInstallation installationID =
  withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT s.service_id, s.area, s.route, s.status, s.additional_info, s.disruption_reason, s.organisation_id, s.last_updated_date, s.updated
        FROM services s
        JOIN installation_services i ON s.service_id = i.service_id
        WHERE i.installation_id = ? AND s.visible = TRUE
        ORDER BY s.area, s.route
      |]
      (Only installationID)

updatePushEnabled :: UUID -> Bool -> Application ()
updatePushEnabled installationID pushEnabled = do
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          UPDATE Installations SET push_enabled = ? WHERE installation_id = ?
        |]
        (pushEnabled, installationID)

getInstallationWithID :: UUID -> Application (Maybe Installation)
getInstallationWithID installationID = do
  results <- withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.push_enabled, i.updated
        FROM installations i
        WHERE installation_id = ?
      |]
      (Only installationID)
  return $ listToMaybe results

getIntererestedInstallationsForServiceID ::
  Int -> Application [Installation]
getIntererestedInstallationsForServiceID serviceID =
  withConnection $ \connection ->
    query
      connection
      [sql|
        SELECT i.installation_id, i.device_token, i.device_type, i.endpoint_arn, i.push_enabled, i.updated
        FROM installation_services s
        JOIN installations i ON s.installation_id = i.installation_id
        WHERE s.service_id = ? AND i.push_enabled = TRUE
      |]
      (Only serviceID)

deleteInstallationWithID :: UUID -> Application ()
deleteInstallationWithID installationID = do
  deleteInstallationServicesWithID installationID
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          DELETE FROM installations WHERE installation_id = ?
        |]
        (Only installationID)

deleteInstallationServicesWithID :: UUID -> Application ()
deleteInstallationServicesWithID installationID =
  withConnection $ \connection ->
    void $
      execute
        connection
        [sql|
          DELETE FROM installation_services WHERE installation_id = ?
        |]
        (Only installationID)
