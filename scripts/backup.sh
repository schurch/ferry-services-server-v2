#!/bin/bash

DB_CONNECTION=''
BACKUP_DIR='/home/stefanchurch/ferry-services-server/backups'
DB_CONTAINER_NAME='ferry-services-db-prod'
BACKUP_NAME=`date +%y-%m-%d-%H%M%S`

docker run --rm --user $(id -u):$(id -g) --network container:"$DB_CONTAINER_NAME" -v "$BACKUP_DIR:/tmp" postgres:alpine bash -c "pg_dump $DB_CONNECTION --clean | gzip > /tmp/$BACKUP_NAME-backup.gz"
