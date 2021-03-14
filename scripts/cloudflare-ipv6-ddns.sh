#!/usr/bin/env bash

set -e

DOMAIN=$1
TOKEN=$2

if [ -z $DOMAIN ]; then
    >&2 echo "Domain is require!"
    exit 1
fi

if [ -z $TOKEN ]; then
    >&2 echo "Token is require!"
    exit 1
fi

API="https://api.cloudflare.com/client/v4/zones"
CONTENT_TYPE='Content-Type:application/json'
AUTH_HEADER="Authorization: Bearer $TOKEN"

# Find the Zone information.
ZONE_DATA=$(curl -sX GET "$API" -H "$CONTENT_TYPE" -H "$AUTH_HEADER")
ZONE=$(echo $ZONE_DATA | jq '.result[0].id' -r)

# Find the record id for the domain.
RECORD_DATA=$(curl -sX GET "$API/$ZONE/dns_records" -H "$CONTENT_TYPE" -H "$AUTH_HEADER")
RECORD=$(echo $RECORD_DATA | jq ".result[] | select(.name == \"$DOMAIN\") | .id" -r)

NAME=$(echo $DOMAIN | cut -f 1 -d '.')
IP_V6="2400:6180:100:d0::9d7:c001"
TTL=120

DATA=$(cat <<EOF
{
    "type": "AAAA",
    "name": "$NAME",
    "content": "$IP_V6"
}
EOF
)

curl -sX PUT "$API/$ZONE/dns_records/$RECORD" \
    -H "$CONTENT_TYPE" \
    -H "$AUTH_HEADER" \
    --data "$DATA"  | jq
