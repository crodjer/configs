#!/bin/bash

## Important
# If you have already installed the uswsusp program please (back it up) purge
# it out and let this script install it.
# Otherwise dpkg doesn't reconfigures the package for a changed swapfile
# correctly. And then the script will anyways purge the old one.

# Some parameters
SWAP_SIZE="3g"
CLEANUP=
DRY_RUN=

# Filename with location of configs
ETC="/etc/"
USWSUSP_CONF="uswsusp.conf"
SYSCTL_CONF="sysctl.conf"
FSTAB="fstab"
HIBERNATOR_BACKUP="/var/backups/hibernator/"
BACKUP_LOCK="/var/backups/hibernator/.lock"
SWAP="/swap"

# The current swappiness
_SWAPPINESS=`sysctl vm.swappiness | sed -e 's/ //g'`

# Function reconfigure the uswsusp package
reconfigure_uswsusp() {
    # Reconfigure the package noninteractively and hope it will work
    dpkg-reconfigure -f nointractive -p critical uswsusp 2> /dev/null

    RESUME_OFFSET="$@"
    if [[ ! `grep "$RESUME_OFFSET" $ETC$USWSUSP_CONF` ]]; then
        echo "Reconfiguring didn't work, doing a purge and install instead..."
        apt-get purge -y uswsusp
        apt-get install -y uswsusp
    fi

}

# Do a cleanup and exit
cleanup(){
    echo 'Cleaning up...'
    # Disabling the swap file
    swapoff $1
    SWAPPINESS=`sysctl -w $2`

    # Cleanup
    rm $1
    exit 1
}

usage()
{
cat << EOF
usage: $0 options

This script hibernates the system to a file using the uswsusp package,
while taking care of all the steps from this guide:
    http://wiki.debian.org/Hibernation/Hibernate_Without_Swap_Partition

This also provides an option to cleanup on resume so that the space can be
retrieved back in case of low space systems (as myself with a small 60G SSD).

Tested only on debian sid.

OPTIONS:
   -h      show this message.
   -f      cleanup after resume.
           will free the swap space, but take more time for next resume.
   -s      swap size (Eg: 3g, 1024m etc). A cleanup should have been done
           from previous hibernate for this to work.
           default=3g
   -f      swap file name, location
           default=/swap
   -d      dry run, won't hibernate but do all the rest operations. Mostly
           usefull for cleanup.
   -c      only do a cleanup and exit.
EOF
}

while getopts "hfs:dc" OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
             ;;
         f)
             CLEANUP=true
             ;;
         s)
             SWAP_SIZE=$OPTARG
             ;;
         f)
             SWAP=$OPTARG
             ;;
         d)
             DRY_RUN=true
             ;;
         c)
             cleanup $SWAP $_SWAPPINESS 2> /dev/null
             exit 1
             ;;

         ?)
             # Normnal Hibernate
             ;;
     esac
done

echo 'Initiating swap file...'
if [ ! -f $SWAP ]; then
    # Allocate a new swap
    fallocate -l $SWAP_SIZE $SWAP

    # Set up swap area in /swap
    NEW_SWAP=`mkswap $SWAP`
fi

# Don't allow kernel to use the swap file for swapping
SWAPPINESS=`sysctl -w vm.swappiness=1`

# Turn the swap on
swapon $SWAP 2> /dev/null

echo 'Setting up configurations...'
if [ ! -f $BACKUP_LOCK ]; then

    # Backup config files
    if [ ! -d $HIBERNATOR_BACKUP ]; then
        mkdir -p $HIBERNATOR_BACKUP
    fi

    touch $BACKUP_LOCK
    #cp "$ETC$USWSUSP_CONF" "$HIBERNATOR_BACKUP$USWSUSP_CONF"
    cp "$ETC$SYSCTL_CONF" "$HIBERNATOR_BACKUP$SYSCTL_CONF"
    cp "$ETC$FSTAB" "$HIBERNATOR_BACKUP$FSTAB"

    # Update the configs
    #echo "$USWSUSP_CONF_TEXT$RESUME_OFFSET" > "$ETC$USWSUSP_CONF"
    echo "vm.swappiness=1" >> "$ETC$SYSCTL_CONF"

    # Update the fstab
    echo "$SWAP           swap            swap    defaults        0       0" >> "$ETC$FSTAB"
fi

# Check if s2disk command is awailable
# The above reinstallation will not necessarily be there in future.
if ! which s2disk >/dev/null; then
    echo "Program 'uswsusp' is not installed."
    # Because of the existing swapfile and /swap entry in fstab, uswsusp
    # install should add resume from file support in the kernel.
    # For deiban, this is the command
    apt-get install -y uswsusp
fi

RESUME_OFFSET="`swap-offset $SWAP`"
if [[ ! `grep "$RESUME_OFFSET" $ETC$USWSUSP_CONF` ]]; then
    # The swapfile and configuration do not match, so reconfigure
    reconfigure_uswsusp $RESUME_OFFSET
fi

# The user should be in powerdev group for suspending to disk.
if [[ ! `groups $USER` == *powerdev* ]]; then
    echo 'Adding you to the `powerdev` group...'
    usermod -G powerdev $USER
fi

echo 'Hibernating...'
# Save the session to disk (swap file)
if [ ! $DRY_RUN ]; then
    s2disk
fi

echo 'Resuming...'

if [ ! $CLEANUP ]; then
    # Restore old swappiness only if the swap doesn't need to be deleted
    SWAPPINESS=`sysctl -w $_SWAPPINESS`
fi

#cp "$HIBERNATOR_BACKUP$USWSUSP_CONF" "$ETC$USWSUSP_CONF"
cp "$HIBERNATOR_BACKUP$SYSCTL_CONF" "$ETC$SYSCTL_CONF"
cp "$HIBERNATOR_BACKUP$FSTAB" $ETC$FSTAB

#rm "$HIBERNATOR_BACKUP$USWSUSP_CONF"
rm "$HIBERNATOR_BACKUP$SYSCTL_CONF"
rm "$HIBERNATOR_BACKUP$FSTAB"

rm $BACKUP_LOCK

if [ ! "$OLD_CONFIGS" = "$NEW_CONFIGS"  ]; then
    echo "$OLD_CONFIGS"
    echo "$NEW_CONFIGS"
fi

if [ $CLEANUP ]; then
    cleanup $SWAP $_SWAPPINESS
fi
