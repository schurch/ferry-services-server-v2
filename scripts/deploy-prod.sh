#!/bin/bash

# Deploy
docker-compose -f docker-compose-prod.yml stop
docker-compose -f docker-compose-prod.yml rm -vf
docker-compose -f docker-compose-prod.yml pull
docker-compose -f ./docker-compose-prod.yml up -d

# Cleanup
docker rmi $(docker images -f "dangling=true" -q) 2> /dev/null