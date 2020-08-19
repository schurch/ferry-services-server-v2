#!make

-include envfile.local
export $(shell sed 's/=.*//' envfile.local)

default: server

.PHONY: build
build:
	stack build

.PHONY: watch
watch:
	stack build --file-watch

.PHONY: server
server: build
	stack exec ferry-services-server-exe

.PHONY: scraper
scraper: build
	stack exec ferry-services-scraper-exe
