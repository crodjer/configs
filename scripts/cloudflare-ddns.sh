#!/usr/bin/env bash

DOMAIN=$1
TOKEN=$2

log () { 
    >&2 echo "[cf-ddns] $(date -Is)" $@
}

usage()
{
cat << EOF
usage: $0 options

This script updates the DNS records on cloudflare for a domain. Supports an
IPV4 and IPV6 updates.

OPTIONS:
   -h               show this message.
   -d <DOMAIN>      domain
   -t <TOKEN>       cloudflare token
   -i <INTERFACE>   interface (if using IPv6)
   -6               use IPv6
   -d               dry run, won't actually update anything
EOF
}

while getopts "hd:t:i:6n" OPTION
do
     case $OPTION in
         h)
             usage
             exit 0
             ;;
         d)
             DOMAIN=$OPTARG
             ;;
         t)
             TOKEN=$OPTARG
             ;;
         i)
             INTERFACE=$OPTARG
             ;;
         6)
             USE_IPV6=true
             ;;
         n)
             DRY_RUN=true
             ;;
         ?)
             usage
             exit 1
             ;;
     esac
done

API="https://api.cloudflare.com/client/v4/zones"
CONTENT_TYPE='Content-Type:application/json'
AUTH_HEADER="Authorization: Bearer $TOKEN"

if [ "$USE_IPV6" == true ]; then
    if [ -z $INTERFACE ]; then
        INTERFACE=$(ip -6 route ls | grep default | head -1 | grep -Po '(?<=dev )(\S+)')
    fi
    IP="$(ip -6 -br a show dev $INTERFACE 2> /dev/null | awk '{ print $3 }' | cut -d / -f 1)"
    RECORD_TYPE="AAAA"
else
    IP=$(curl -s 'https://api.ipify.org')
    RECORD_TYPE="A"
fi

if [ -z "$IP" ]; then
    log "No IP Found!"
    exit 1
fi

LAST_UPDATE_FILE=/tmp/cf-ddns-last-update-$DOMAIN.txt
if [ -f "$LAST_UPDATE_FILE" ]; then
    LAST_UPDATE=$(cat $LAST_UPDATE_FILE)
fi

if [ "$LAST_UPDATE" == "$IP" ]; then
   log "Last updated record matches, skipping update!"
   exit 0
fi

# Find the Zone information.
log "Getting zone data..."
ZONE_DATA=$(curl -sX GET "$API" -H "$CONTENT_TYPE" -H "$AUTH_HEADER")
ZONE=$(echo $ZONE_DATA | jq '.result[0].id' -r)

if [ -n "ZONE" ]; then
    log "..done!"
else
    log "Failed to get zone data!"
    exit 1
fi

# Find the record id for the domain.
log "Getting record data..."
RECORDS_DATA=$(curl -sX GET "$API/$ZONE/dns_records" -H "$CONTENT_TYPE" -H "$AUTH_HEADER")
RECORD_DATA=$(echo $RECORDS_DATA | jq -c ".result[] | select(.name == \"$DOMAIN\")")
RECORD=$(echo $RECORD_DATA | jq ".id" -r )
if [ -n "RECORD" ]; then
    log "..done!"
else
    log "Failed to get record data!"
    exit 1
fi

NAME=$(echo $DOMAIN | cut -f 1 -d '.')
EXISTING=$(echo $RECORD_DATA | jq ".content" -r )

if [ "$EXISTING" == "$IP" ]; then
   log "Existing record matches, skipping update."
   exit 0
fi

DATA=$(cat <<EOF
{
    "type": "$RECORD_TYPE",
    "name": "$NAME",
    "content": "$IP"
}
EOF
)

log "Updating records..."
if [ "$DRY_RUN" != "true" ]; then
    curl -sX PUT "$API/$ZONE/dns_records/$RECORD" \
       -H "$CONTENT_TYPE" \
       -H "$AUTH_HEADER" \
       --data "$DATA"
    echo $IP >> /tmp/cf-ddns-last-update-$DOMAIN.txt
fi
log "..done!"
