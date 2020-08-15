#!make

default: server

.PHONY: build
build:
	stack build

.PHONY: watch
watch:
	stack build --file-watch

.PHONY: server
server:
	$(MAKE) build
	stack exec ferry-services-server-exe
