#/bin/bash

# Author: Jurica Bradaric <jbradaric at gmail.com>
# A simple script to use custom stylesheets for websites.
# Similar to the Stylish extension for Firefox.
# I hope someone finds it useful. All comments and suggestions
# are welcome.

XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-"$HOME/.config/"}

# Set the path to the .css files
# Every custom css style must be in a file named web_address.css
# e.g. reddit.com.css
STYLE_PATH=$XDG_CONFIG_HOME/uzbl/styles
DEFAULT_STYLE=${STYLE_PATH}/default.css

# Directory where temporary styles will be stored.
STYLES_TEMP_DIR=/tmp/$USER-styles/
mkdir -p $STYLES_TEMP_DIR

STYLESHEET_SET=0

for i in $STYLE_PATH/*.css; do
    stylesheet=$(basename $i '.css')
    if [[ "$UZBL_URI" =~ "${stylesheet}" ]]; then

        stylesheet_path=${STYLE_PATH}/${stylesheet}.css
        if [ -f $DEFAULT_STYLE ]; then
            # Create a temporary stylesheet based on default and the override.
            tmp_stylesheet_path=$STYLES_TEMP_DIR/${stylesheet}.css
            cat $stylesheet_path $DEFAULT_STYLE > $tmp_stylesheet_path
            stylesheet_path=$tmp_stylesheet_path
        fi

        echo "set stylesheet_uri = file://$stylesheet_path" > "$UZBL_FIFO"
        STYLESHEET_SET=1
        break
    fi
done

if [ $STYLESHEET_SET -eq 0 ]; then
    echo "set stylesheet_uri = file://$DEFAULT_STYLE" > "$UZBL_FIFO"
fi

exit 0
