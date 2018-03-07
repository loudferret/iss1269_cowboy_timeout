#!/bin/dash

curl -v -i --ipv4 \
    -H "Content-Type: application/json" \
    -X POST \
    --data @simple.json \
    'http://localhost:8567/mqw/v1/pub/raw?from=meteo'

