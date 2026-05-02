FROM node:22-bookworm-slim

ARG BIN_DIR

RUN mkdir -p /opt/ferry-services

WORKDIR /opt/ferry-services

RUN apt-get update && \
  apt-get install -y libpq5 netbase ca-certificates && \
  rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/ferry-services/scripts

COPY scripts/fetch-corran-page.mjs /opt/ferry-services/scripts/fetch-corran-page.mjs
COPY scripts/fetch-corran-facebook-text.mjs /opt/ferry-services/scripts/fetch-corran-facebook-text.mjs
COPY $BIN_DIR .

RUN cd /opt/ferry-services && \
  npm init -y >/dev/null 2>&1 && \
  npm install --omit=dev playwright@1.52.0 && \
  npx playwright install --with-deps chromium && \
  rm -rf /var/lib/apt/lists/*

CMD ["/opt/ferry-services/ferry-services-scraper-exe"]
