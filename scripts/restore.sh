#!/bin/bash

DB_CONNECTION=''
BACKUP_DIR='/home/stefanchurch/ferry-services-server/backups'
DB_CONTAINER_NAME='ferry-services-db-prod'

docker run --rm --user $(id -u):$(id -g) --network container:"$DB_CONTAINER_NAME"-v "$BACKUP_DIR:/tmp" postgres:alpine bash -c "gunzip -c /tmp/$1 | psql $DB_CONNECTION"
