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
	docker build -f server.Dockerfile -t stefanchurch/ferry-services-server:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-server-exe/ferry-services-server-exe" .
	docker build -f scraper.Dockerfile -t stefanchurch/ferry-services-scraper:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-scraper-exe/ferry-services-scraper-exe" .
	docker build -f transxchange-fetcher.Dockerfile -t stefanchurch/transxchange-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/transxchange-fetcher-exe/transxchange-fetcher-exe" .
	docker push stefanchurch/ferry-services-server:latest
	docker push stefanchurch/ferry-services-scraper:latest
	docker push stefanchurch/transxchange-fetcher:latest

.PHONY: watch
watch:
	stack build --file-watch

.PHONY: server
server: build
	stack exec ferry-services-server-exe

.PHONY: scraper
scraper: build
	stack exec ferry-services-scraper-exe

.PHONY: transxchange-fetcher
transxchange-fetcher: build
	stack exec transxchange-fetcher-exe
