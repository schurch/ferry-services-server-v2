#!make

define setup_env
	$(eval include $(1))
	$(eval export)
	$(if $(strip $(LOG_LEVEL)),,$(eval export LOG_LEVEL=Info))
endef

SDK_PATH := $(shell command -v xcrun >/dev/null 2>&1 && xcrun --show-sdk-path 2>/dev/null)
STACK_ENV_PREFIX :=

ifneq ($(SDK_PATH),)
STACK_ENV_PREFIX += C_INCLUDE_PATH="$(SDK_PATH)/usr/include/ffi"
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
	docker_dist_dir="$$(stack --docker path --dist-dir)"; \
	docker build -f docker/ferry-services.Dockerfile -t stefanchurch/ferry-services:latest \
		--build-arg SERVER_BIN="$$docker_dist_dir/build/ferry-services-server-exe/ferry-services-server-exe" \
		--build-arg SCRAPER_BIN="$$docker_dist_dir/build/ferry-services-scraper-exe/ferry-services-scraper-exe" \
		--build-arg TIMETABLE_DOCUMENT_SCRAPER_BIN="$$docker_dist_dir/build/ferry-services-timetable-document-scraper-exe/ferry-services-timetable-document-scraper-exe" \
		--build-arg WEATHER_FETCHER_BIN="$$docker_dist_dir/build/ferry-services-weather-fetcher-exe/ferry-services-weather-fetcher-exe" \
		--build-arg VESSEL_FETCHER_BIN="$$docker_dist_dir/build/ferry-services-vessel-fetcher-exe/ferry-services-vessel-fetcher-exe" \
		--build-arg TRANSXCHANGE_INGESTER_BIN="$$docker_dist_dir/build/ferry-services-transxchange-ingester-v2-exe/ferry-services-transxchange-ingester-v2-exe" \
		--build-arg RAIL_DEPARTURE_FETCHER_BIN="$$docker_dist_dir/build/ferry-services-rail-departure-fetcher-exe/ferry-services-rail-departure-fetcher-exe" \
		--build-arg OFFLINE_SNAPSHOT_GENERATOR_BIN="$$docker_dist_dir/build/ferry-services-offline-snapshot-generator-exe/ferry-services-offline-snapshot-generator-exe" \
		.
	docker push stefanchurch/ferry-services:latest

.PHONY: watch
watch:
	$(STACK_ENV_PREFIX) stack build --file-watch

.PHONY: server
server: build dev-env
	stack exec ferry-services-server-exe

.PHONY: scraper
scraper: build dev-env
	stack exec ferry-services-scraper-exe

.PHONY: timetable-document-scraper
timetable-document-scraper: build dev-env
	stack exec ferry-services-timetable-document-scraper-exe

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

.PHONY: offline-snapshot-generator
offline-snapshot-generator: build dev-env
	stack exec ferry-services-offline-snapshot-generator-exe

.PHONY: rail-departure-fetcher
rail-departure-fetcher: build dev-env
	stack exec ferry-services-rail-departure-fetcher-exe

.PHONY: tests
tests: test-env
	case "$(DB_CONNECTION)" in *.sqlite|*.sqlite3|*.db) rm -f "$(DB_CONNECTION)" "$(DB_CONNECTION)-wal" "$(DB_CONNECTION)-shm" ;; *) echo "Refusing to reset non-SQLite DB_CONNECTION: $(DB_CONNECTION)" >&2; exit 1 ;; esac
	sqlite3 "$(DB_CONNECTION)" ".read sqlite/schema.sql"
	sqlite3 "$(DB_CONNECTION)" ".read sqlite/seed.sql"
	$(STACK_ENV_PREFIX) stack test

.PHONY: tests-json
tests-json:
	$(STACK_ENV_PREFIX) stack test --test-arguments '--match "JSON Tests"'

.PHONY: db-reset
db-reset: dev-env
	case "$(DB_CONNECTION)" in *.sqlite|*.sqlite3|*.db) rm -f "$(DB_CONNECTION)" "$(DB_CONNECTION)-wal" "$(DB_CONNECTION)-shm" ;; *) echo "Refusing to reset non-SQLite DB_CONNECTION: $(DB_CONNECTION)" >&2; exit 1 ;; esac
	sqlite3 "$(DB_CONNECTION)" ".read sqlite/schema.sql"
	sqlite3 "$(DB_CONNECTION)" ".read sqlite/seed.sql"

.PHONY: db-shell
db-shell: dev-env
	sqlite3 "$(DB_CONNECTION)"

.PHONY: db-backup
db-backup:
	./scripts/backup.sh

.PHONY: doctor
doctor:
	./scripts/doctor.sh

.PHONY: bootstrap-dev
bootstrap-dev:
	./scripts/bootstrap-dev.sh

.PHONY: install-system-deps
install-system-deps:
	./scripts/install-system-deps.sh
