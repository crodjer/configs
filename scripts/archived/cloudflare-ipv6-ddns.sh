#!/usr/bin/env bash

set -e

DOMAIN=$1
TOKEN=$2
INTERFACE=$3

log () { 
    >&2 echo "[cf-ddns] $(date -Is)" $@
}

if [ -z "$(command -v curl)" ]; then
    log "'curl' is required!"
    exit 1
fi

if [ -z "$(command -v jq)" ]; then
    log "'jq' is required!"
    exit 1
fi

if [ -z $DOMAIN ]; then
    log "Domain is require!"
    exit 1
fi

if [ -z $TOKEN ]; then
    log echo "Token is require!"
    exit 1
fi

API="https://api.cloudflare.com/client/v4/zones"
CONTENT_TYPE='Content-Type:application/json'
AUTH_HEADER="Authorization: Bearer $TOKEN"

if [ -z $INTERFACE ]; then
    INTERFACE=$(ip -6 route ls | grep default | head -1 | grep -Po '(?<=dev )(\S+)')
fi
IP_V6="$(ip -6 -br a show dev $INTERFACE scope global | awk '{ print $3 }' | cut -d / -f 1)"
TTL=120

if [ -z "$IP_V6" ]; then
    log "Couldn't find the assigned IP v6"
    exit 1
fi

LAST_UPDATE_FILE=/tmp/cf-ddns-last-update-$DOMAIN.txt
LAST_UPDATE_FILE=/tmp/cf-ddns-last-update-$DOMAIN.txt
if [ -f "$LAST_UPDATE_FILE" ]; then
    LAST_UPDATE=$(cat $LAST_UPDATE_FILE)
fi

if [ "$LAST_UPDATE" == "$IP_V6" ]; then
   log "Last updated record matches, skipping update!"
   exit 0
fi

# Find the Zone information.
log "Getting zone data..."
ZONE_DATA=$(curl -sX GET "$API" -H "$CONTENT_TYPE" -H "$AUTH_HEADER")
ZONE=$(echo $ZONE_DATA | jq '.result[0].id' -r)
log "..done!"

# Find the record id for the domain.
log "Getting record data..."
RECORDS_DATA=$(curl -sX GET "$API/$ZONE/dns_records" -H "$CONTENT_TYPE" -H "$AUTH_HEADER")
RECORD_DATA=$(echo $RECORDS_DATA | jq -c ".result[] | select(.name == \"$DOMAIN\")")
RECORD=$(echo $RECORD_DATA | jq ".id" -r )
log "..done!"

NAME=$(echo $DOMAIN | cut -f 1 -d '.')
EXISTING=$(echo $RECORD_DATA | jq ".content" -r )

if [ "$EXISTING" == "$IP_V6" ]; then
   log "Existing record matches, skipping update."
   exit 0
fi

DATA=$(cat <<EOF
{
    "type": "AAAA",
    "name": "$NAME",
    "content": "$IP_V6"
}
EOF
)

log "Updating records..."
curl -sX PUT "$API/$ZONE/dns_records/$RECORD" \
   -H "$CONTENT_TYPE" \
   -H "$AUTH_HEADER" \
   --data "$DATA"
echo $IP_V6 >> /tmp/cf-ddns-last-update-$DOMAIN.txt
log "..done!"
