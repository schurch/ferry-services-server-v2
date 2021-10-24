#!make

-include envfile.local
export $(shell sed 's/=.*//' envfile.local)

default: server

.PHONY: build
build:
	stack build

.PHONY: build-release
build-release:
	echo ${DOCKER_HUB_PASSWORD} | docker login -u ${DOCKER_HUB_USERNAME} --password-stdin
	stack docker pull
	stack --docker build
	docker build -f docker/server.Dockerfile -t stefanchurch/ferry-services-server:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-server-exe/ferry-services-server-exe" .
	docker build -f docker/scraper.Dockerfile -t stefanchurch/ferry-services-scraper:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-scraper-exe/ferry-services-scraper-exe" .
	docker build -f docker/weather-fetcher.Dockerfile -t stefanchurch/ferry-services-weather-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-weather-fetcher-exe/ferry-services-weather-fetcher-exe" .
	docker push stefanchurch/ferry-services-server:latest
	docker push stefanchurch/ferry-services-scraper:latest
	docker push stefanchurch/ferry-services-weather-fetcher:latest

.PHONY: watch
watch:
	stack build --file-watch

.PHONY: server
server: build
	stack exec ferry-services-server-exe

.PHONY: scraper
scraper: build
	stack exec ferry-services-scraper-exe

.PHONY: weather-fetcher
weather-fetcher: build
	stack exec ferry-services-weather-fetcher-exe
