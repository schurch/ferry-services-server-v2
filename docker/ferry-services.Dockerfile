FROM node:22-bookworm-slim

ARG SERVER_BIN
ARG SCRAPER_BIN
ARG TIMETABLE_DOCUMENT_SCRAPER_BIN
ARG WEATHER_FETCHER_BIN
ARG VESSEL_FETCHER_BIN
ARG TRANSXCHANGE_INGESTER_BIN
ARG RAIL_DEPARTURE_FETCHER_BIN
ARG OFFLINE_SNAPSHOT_GENERATOR_BIN

RUN mkdir -p /opt/ferry-services/scripts

WORKDIR /opt/ferry-services

RUN apt-get update && \
  apt-get install -y sqlite3 netbase ca-certificates gzip && \
  rm -rf /var/lib/apt/lists/*

COPY ./public ./public
COPY ./sqlite ./sqlite
COPY scripts/fetch-corran-page.mjs /opt/ferry-services/scripts/fetch-corran-page.mjs
COPY scripts/fetch-corran-facebook-text.mjs /opt/ferry-services/scripts/fetch-corran-facebook-text.mjs

COPY ${SERVER_BIN} /opt/ferry-services/ferry-services-server-exe
COPY ${SCRAPER_BIN} /opt/ferry-services/ferry-services-scraper-exe
COPY ${TIMETABLE_DOCUMENT_SCRAPER_BIN} /opt/ferry-services/ferry-services-timetable-document-scraper-exe
COPY ${WEATHER_FETCHER_BIN} /opt/ferry-services/ferry-services-weather-fetcher-exe
COPY ${VESSEL_FETCHER_BIN} /opt/ferry-services/ferry-services-vessel-fetcher-exe
COPY ${TRANSXCHANGE_INGESTER_BIN} /opt/ferry-services/ferry-services-transxchange-ingester-v2-exe
COPY ${RAIL_DEPARTURE_FETCHER_BIN} /opt/ferry-services/ferry-services-rail-departure-fetcher-exe
COPY ${OFFLINE_SNAPSHOT_GENERATOR_BIN} /opt/ferry-services/ferry-services-offline-snapshot-generator-exe

RUN npm init -y >/dev/null 2>&1 && \
  npm install --omit=dev playwright@1.52.0 && \
  npx playwright install --with-deps chromium && \
  rm -rf /var/lib/apt/lists/*

CMD ["/opt/ferry-services/ferry-services-server-exe"]
