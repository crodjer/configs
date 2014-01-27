#!/usr/bin/env bash

################################################################################
# yDown uses youtube-dl (http://rg3.github.com/youtube-dl/) for getting the
# download urls from video site and then passes them to downloaders in this
# order: mcurl > ariar2 > wget, based on which ever is installed.
# Usage: ydown [downloader args] url
################################################################################

VIDEO_URL="${@:$#}"
DOWNLOADER_ARGS="${@:1:$#-1}"

if [ ! "$VIDEO_URL" ];then
    echo "Error: No video url supplied."
    echo "Usage: ydown [downloader args] url"
    exit 1
fi

VIDEO_DOWNLOAD_INFO=$(cat <<EOF
`youtube-dl "$VIDEO_URL" -g  -e --get-filename`
EOF
)

if [ ! "$VIDEO_DOWNLOAD_INFO" ]; then
    exit 1
fi

TITLE=$(echo "$VIDEO_DOWNLOAD_INFO" | sed -n 1p  | sed 's/[^a-zA-Z0-9]\+/-/g')
URL=$(echo "$VIDEO_DOWNLOAD_INFO" | sed -n 2p)
VIDEONAME=$(echo "$VIDEO_DOWNLOAD_INFO" | sed -n 3p)

FILENAME="$TITLE-$VIDEONAME"
FILENAME=$(echo ${FILENAME,,})

echo $TITLE $URL $VIDEONAME

# command -v aria2c &> /dev/null && aria2c -o "$FILENAME" "$URL"
