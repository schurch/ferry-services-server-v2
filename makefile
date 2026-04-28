#!make

define setup_env
	$(eval include $(1))
	$(eval export)
	$(if $(strip $(LOG_LEVEL)),,$(eval export LOG_LEVEL=Info))
endef

SDK_PATH := $(shell command -v xcrun >/dev/null 2>&1 && xcrun --show-sdk-path 2>/dev/null)
STACK_ENV_PREFIX :=

ifneq ($(SDK_PATH),)
STACK_ENV_PREFIX = C_INCLUDE_PATH="$(SDK_PATH)/usr/include/ffi"
endif

default: server

.PHONY:
dev-env: 
	$(call setup_env, envfile.local)

.PHONY:
test-env: 
	$(call setup_env, envfile-test.local)

.PHONY:
build:
	$(STACK_ENV_PREFIX) stack build

.PHONY:
build-release:
	echo ${DOCKER_HUB_PASSWORD} | docker login -u ${DOCKER_HUB_USERNAME} --password-stdin
	stack docker pull
	stack --docker build
	docker build -f docker/server.Dockerfile -t stefanchurch/ferry-services-server:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-server-exe/ferry-services-server-exe" .
	docker build -f docker/scraper.Dockerfile -t stefanchurch/ferry-services-scraper:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-scraper-exe/ferry-services-scraper-exe" .
	docker build -f docker/weather-fetcher.Dockerfile -t stefanchurch/ferry-services-weather-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-weather-fetcher-exe/ferry-services-weather-fetcher-exe" .
	docker build -f docker/vessel-fetcher.Dockerfile -t stefanchurch/ferry-services-vessel-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-vessel-fetcher-exe/ferry-services-vessel-fetcher-exe" .
	docker build -f docker/transxchange-ingester.Dockerfile -t stefanchurch/ferry-services-transxchange-ingester:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-transxchange-ingester-v2-exe/ferry-services-transxchange-ingester-v2-exe" .
	docker build -f docker/rail-departure-fetcher.Dockerfile -t stefanchurch/ferry-services-rail-departure-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-rail-departure-fetcher-exe/ferry-services-rail-departure-fetcher-exe" .
	docker push stefanchurch/ferry-services-server:latest
	docker push stefanchurch/ferry-services-scraper:latest
	docker push stefanchurch/ferry-services-weather-fetcher:latest
	docker push stefanchurch/ferry-services-vessel-fetcher:latest
	docker push stefanchurch/ferry-services-transxchange-ingester:latest
	docker push stefanchurch/ferry-services-rail-departure-fetcher:latest

.PHONY: watch
watch:
	$(STACK_ENV_PREFIX) stack build --file-watch

.PHONY: server
server: build dev-env
	stack exec ferry-services-server-exe

.PHONY: scraper
scraper: build dev-env
	stack exec ferry-services-scraper-exe

.PHONY: weather-fetcher
weather-fetcher: build dev-env
	stack exec ferry-services-weather-fetcher-exe

.PHONY: vessel-fetcher
vessel-fetcher: build dev-env
	stack exec ferry-services-vessel-fetcher-exe

.PHONY: transxchange-ingester-v2
transxchange-ingester-v2: build dev-env
	stack exec ferry-services-transxchange-ingester-v2-exe

.PHONY: transxchange-ingester
transxchange-ingester: transxchange-ingester-v2

.PHONY: rail-departure-fetcher
rail-departure-fetcher: build dev-env
	stack exec ferry-services-rail-departure-fetcher-exe

.PHONY: tests
tests: test-env
	psql "$(DB_CONNECTION)" -v ON_ERROR_STOP=1 -c 'DO $$$$ DECLARE app_table record; BEGIN FOR app_table IN SELECT schemaname, tablename FROM pg_tables WHERE schemaname = '"'"'public'"'"' AND tablename <> '"'"'spatial_ref_sys'"'"' LOOP EXECUTE format('"'"'DROP TABLE IF EXISTS %I.%I CASCADE'"'"', app_table.schemaname, app_table.tablename); END LOOP; END $$$$; DROP TYPE IF EXISTS day_of_week;'
	migrate -source file://migrations -database "$(DB_CONNECTION)" up
	$(STACK_ENV_PREFIX) stack test

.PHONY: tests-json
tests-json:
	$(STACK_ENV_PREFIX) stack test --test-arguments '--match "JSON Tests"'

.PHONY: bootstrap-dev
bootstrap-dev:
	./scripts/bootstrap-dev.sh

.PHONY: install-migrate
install-migrate:
	./scripts/install-migrate.sh

.PHONY: install-system-deps
install-system-deps:
	./scripts/install-system-deps.sh
