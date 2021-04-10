#!/usr/bin/env sh

BIOPORTAL_API_KEY="${BIOPORTAL_API_KEY}" \
BIOPORTAL_RATE_LIMIT="15" \
BIOPORTAL_HOST="data.bioontology.org" \
SCHEMA_POSTGRES_HOST=localhost \
SCHEMA_POSTGRES_USER=postgres \
SCHEMA_POSTGRES_PASSWORD=password \
JWT_SECRET_KEY=testkey \
API_SERVICE_URL=https://dev-api-use1.pennsieve.net \
MODEL_SERVICE_URL=https://dev-concepts-service-use1.dev.pennsieve.net \
PREFIX_URL=http://0.0.0.0/model-schema \
sbt console
