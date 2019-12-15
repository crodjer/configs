#!/usr/bin/env bash

set -e

codename=$1

GAPPS_PLATFORM=${GAPPS_PLATFORM:-arm64}
GAPPS_VERSION=${GAPPS_VERSION:-9.0}
GAPPS_VARIENT=${GAPPS_VARIENT:-nano}

lineage_page=https://download.lineageos.org/$codename
pattern="https://mirrorbits.lineageos.org/full/$codename/[^>]+-$codename-signed.zip"

echo "Getting LineageOS URL..."
lineage_url=$(curl -s $lineage_page | grep -Po "$pattern"  | head -1)
echo "$lineage_url"

echo "Getting GApps URL..."
gapps_pattern="https://downloads.sourceforge.net/project/opengapps/$GAPPS_PLATFORM/[0-9]+/open_gapps-$GAPPS_PLATFORM-$GAPPS_VERSION-$GAPPS_VARIENT-[0-9]+.zip"
gapps_url=$(curl -s https://api.opengapps.org/list | grep -Eo $gapps_pattern | head -1)
echo "$gapps_url"

# work_dir=$(mktemp --tmpdir -d lineageos-update-XXXX)
work_dir="/tmp/lineage-updates-$codename"
mkdir -p $work_dir

cd $work_dir
echo "Downloading LineageOS for $codename..."
aria2c -c "$lineage_url"

echo "Downloading OpenGApps for $GAPPS_PLATFORM:$GAPPS_VERSION:$GAPPS_VARIENT..."
aria2c -c "$gapps_url"

lineage_zip=$(basename $lineage_url)
gapps_zip=$(basename $gapps_url)

if [ "$(curl -s "$lineage_url?sha256")" !=  "$(sha256sum $lineage_zip)" ]; then
    >&2 echo "LineageOS SHA256 Sum Mismatch!"
    exit 1
fi
if [ "$(curl -sL $gapps_url.md5)" !=  "$(md5sum $gapps_zip)" ]; then
    >&2 echo "OpenGApps SHA256 Sum Mismatch!"
    exit 1
fi

echo "Please reboot your phone into recovery connect to this machine."
read -p "Press enter once done..."

echo "Pushing zips to recovery via adb..."
adb push $lineage_zip $gapps_zip /tmp/

echo "Installing Lineage"
adb shell twrp install $lineage_zip
echo "Installing GApps"
adb shell twrp install $gapps_zip

echo "Cleaning Cache/Dalvik"
adb shell twrp wipe cache
adb shell twrp wipe dalvik

echo "Updating Hosts to block ads."
adb shell mount -o rw /dev/block/bootdevice/by-name/system /system
adb shell cp /system/etc/hosts /system/etc/hosts.bk
wget https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts -O $work_dir/hosts
adb push $work_dir/hosts /system/etc/hosts
adb shell umount /system
