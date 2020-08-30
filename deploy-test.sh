#!/bin/bash

# Deploy
docker-compose -f docker-compose-test.yml stop
docker-compose -f docker-compose-test.yml rm -vf
docker-compose -f docker-compose-test.yml pull
docker-compose -f ./docker-compose-test.yml up -d

# Cleanup
docker rmi $(docker images -f "dangling=true" -q) 2> /dev/null