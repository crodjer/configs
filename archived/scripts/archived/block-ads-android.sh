#!/usr/bin/env bash
echo "Updating Hosts to block ads."
adb shell mount -o rw /dev/block/bootdevice/by-name/system /system
adb shell cp /system/etc/hosts /system/etc/hosts.bk
wget https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts -O $work_dir/hosts
adb push $work_dir/hosts /system/etc/hosts
adb shell umount /system
