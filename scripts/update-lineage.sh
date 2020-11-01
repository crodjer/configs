#!/usr/bin/env bash

set -e

codename=$1

GAPPS_PLATFORM=${GAPPS_PLATFORM:-arm64}
GAPPS_VERSION=${GAPPS_VERSION:-10.0}
GAPPS_VARIENT=${GAPPS_VARIENT:-nano}

lineage_page=https://download.lineageos.org/$codename
pattern="https://mirrorbits.lineageos.org/full/$codename/[0-9]+/[^>]+-$codename-signed.zip"

echo "Getting LineageOS URL..."
lineage_url=$(curl -s $lineage_page | grep -Eo "$pattern"  | head -1)
echo "$lineage_url"

echo "Getting GApps URL..."
gapps_pattern="https://downloads.sourceforge.net/project/opengapps/$GAPPS_PLATFORM/[0-9]+/open_gapps-$GAPPS_PLATFORM-$GAPPS_VERSION-$GAPPS_VARIENT-[0-9]+.zip"
gapps_url=$(curl -s https://api.opengapps.org/list | grep -Eo $gapps_pattern | head -1)
echo "$gapps_url"

work_dir="/tmp/lineage-updates-$codename"
mkdir -p $work_dir

cd $work_dir
echo "Downloading LineageOS for $codename..."
aria2c -c "$lineage_url"

echo "Downloading OpenGApps for $GAPPS_PLATFORM:$GAPPS_VERSION:$GAPPS_VARIENT..."
aria2c -c "$gapps_url"

lineage_zip=$(basename $lineage_url)
gapps_zip=$(basename $gapps_url)

echo "Please reboot your phone into recovery connect to this machine."
echo "Create a TWRP backup."
read -p "Press enter once done..."

echo "Pushing zips to recovery via adb..."
adb -d push $lineage_zip $gapps_zip /tmp/

echo "Installing Lineage..."
adb -d shell twrp install /tmp/$lineage_zip
echo "Installing GApps..."
adb -d shell twrp install /tmp/$gapps_zip

echo "Cleaning Cache/Dalvik"
adb -d shell twrp wipe cache
adb -d shell twrp wipe dalvik
