#!/bin/bash

id=$1
date=$(date +%F)
year=$(date +%Y)
title="${@:2}"
site=$HOME/workspace/site
path=$site/drafts/$year/$id.mkd

if [ -z $id ]; then
    echo "Post id is required." >&2
    exit 1
fi

template=$(
    cat <<EOF
---
title: $title
date: $date
tags:
author: Rohan
---
EOF
)

mkdir -p $(dirname $path)
echo "$template" > $path
$EDITOR $path
