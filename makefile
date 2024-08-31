#!make

define setup_env
	$(eval include $(1))
	$(eval export)
endef

default: server

.PHONY:
dev-env: 
	$(call setup_env, envfile.local)

.PHONY:
test-env: 
	$(call setup_env, envfile-test.local)

.PHONY:
build:
	C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi" stack build

.PHONY:
build-release:
	echo ${DOCKER_HUB_PASSWORD} | docker login -u ${DOCKER_HUB_USERNAME} --password-stdin
	stack docker pull
	stack --docker build
	docker build -f docker/server.Dockerfile -t stefanchurch/ferry-services-server:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-server-exe/ferry-services-server-exe" .
	docker build -f docker/scraper.Dockerfile -t stefanchurch/ferry-services-scraper:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-scraper-exe/ferry-services-scraper-exe" .
	docker build -f docker/weather-fetcher.Dockerfile -t stefanchurch/ferry-services-weather-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-weather-fetcher-exe/ferry-services-weather-fetcher-exe" .
	docker build -f docker/vessel-fetcher.Dockerfile -t stefanchurch/ferry-services-vessel-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-vessel-fetcher-exe/ferry-services-vessel-fetcher-exe" .
	docker build -f docker/transxchange-ingester.Dockerfile -t stefanchurch/ferry-services-transxchange-ingester:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-transxchange-ingester-exe/ferry-services-transxchange-ingester-exe" .
	docker build -f docker/rail-departure-fetcher.Dockerfile -t stefanchurch/ferry-services-rail-departure-fetcher:latest --build-arg BIN_DIR="$(shell stack --docker path --dist-dir)/build/ferry-services-rail-departure-fetcher-exe/ferry-services-rail-departure-fetcher-exe" .
	docker push stefanchurch/ferry-services-server:latest
	docker push stefanchurch/ferry-services-scraper:latest
	docker push stefanchurch/ferry-services-weather-fetcher:latest
	docker push stefanchurch/ferry-services-vessel-fetcher:latest
	docker push stefanchurch/ferry-services-transxchange-ingester:latest
	docker push stefanchurch/ferry-services-rail-departure-fetcher:latest

.PHONY: watch
watch:
	stack build --file-watch

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

.PHONY: transxchange-ingester
transxchange-ingester: build dev-env
	stack exec ferry-services-transxchange-ingester-exe

.PHONY: rail-departure-fetcher
rail-departure-fetcher: build dev-env
	stack exec ferry-services-rail-departure-fetcher-exe

.PHONY: tests
tests: test-env
	psql -d ferry-services-test -U stefanchurch -c 'DROP TABLE IF EXISTS vessels; DROP TABLE IF EXISTS service_locations; DROP TABLE IF EXISTS installation_services; DROP TABLE IF EXISTS installations; DROP TABLE IF EXISTS location_weather; DROP TABLE IF EXISTS rail_departures; DROP TABLE IF EXISTS locations; DROP TABLE IF EXISTS schema_migrations; DROP TABLE IF EXISTS days_of_non_operation; DROP TABLE IF EXISTS days_of_operation; DROP TABLE IF EXISTS vehicle_journeys; DROP TABLE IF EXISTS journey_patterns CASCADE; DROP TABLE IF EXISTS lines; DROP TABLE IF EXISTS transxchangeservice_services; DROP TABLE IF EXISTS transxchange_services; DROP TABLE IF EXISTS operators; DROP TABLE IF EXISTS journey_pattern_timing_links; DROP TABLE IF EXISTS journey_pattern_sections; DROP TABLE IF EXISTS routes; DROP TABLE IF EXISTS route_links; DROP TABLE IF EXISTS route_sections; DROP TABLE IF EXISTS stop_points; DROP TYPE IF EXISTS day_of_week; DROP TABLE IF EXISTS serviced_organisation_working_days; DROP TABLE IF EXISTS serviced_organisations; DROP TABLE IF EXISTS services; DROP TABLE IF EXISTS organisations;'
	migrate -source file://migrations -database "postgres://stefanchurch@localhost:5432/ferry-services-test?sslmode=disable" up
	C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi" stack test
