#!/bin/bash

id=$1
date=$(date +%F)
title="${@:2}"
site=$HOME/projects/rohanjain.in
path=$site/drafts/$id.mkd

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

# mkdir -p $(dirname $path)
echo "$template" > $path
$EDITOR $path
