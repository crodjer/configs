# Borrowed from yeban/rooter.sh
# cd to the project root (identified by the presence of a SCM directory, like
# .git, or .bzr)
#
# INSTALL:
#   * put something like this in your .zshrc:
#     . /path/to/rooter.zsh
#
#   This installs the 'cdr' command for use.
#
# USE:
#   $ pwd
#   /home/yeban/drizzle/drizzle/libdrizzle-2.0/libdrizzle
#   $ cdr
#   $ pwd
#   /home/yeban/src/drizzle/drizzle

# array of some known SCM directories
scmdirs=('.git' '.bzr' '.hg')

# return true if the given directory has an SCM subdirectory
_has_scm_dir(){
    if [[ -d $1 ]]; then
        for scmdir in ${scmdirs[@]}; do
            if [[ -d "$1/$scmdir" ]]; then
                return 0
            fi
        done
    fi

    return 1
}

# return the project root of the given directory
_get_root_dir(){
    local dir=$1

    if [[ -d $dir ]]; then
        while [ $dir != "/" ]
        do
            if _has_scm_dir $dir; then
                echo $dir; return 0
            fi
            dir=$(dirname $dir)
        done
    fi

    return 1
}

# cd to the project root of the current working directory
cdr(){
    cd $(_get_root_dir $(pwd))
}
