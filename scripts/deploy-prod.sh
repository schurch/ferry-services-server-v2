#!/bin/bash

set -euo pipefail

# Deploy
docker-compose -f docker-compose-prod.yml stop
docker-compose -f docker-compose-prod.yml rm -vf
docker-compose -f docker-compose-prod.yml pull
docker-compose -f docker-compose-prod.yml up -d

# Migrate
set -a
source ./envfile.docker-prod
set +a

docker run --rm \
  -v "$(pwd)"/migrations:/migrations \
  --network container:ferry-services-db-prod \
  migrate/migrate \
  -path=/migrations/ \
  -database "postgres://${POSTGRES_USER}:${POSTGRES_PASSWORD}@localhost:5432/${POSTGRES_DB}?sslmode=disable" \
  up

# Cleanup
docker image prune -f >/dev/null 2>&1 || true
