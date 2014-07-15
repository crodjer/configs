#/usr/bin/env sh

file_pattern=$1
command=${@:2}

events="modify,attrib,moved_to,create,delete"
inotifywait -mre $events --format '%w%f' $1 | while read file_name; do
    _current_key="$(date +"%s")-$file_name"

    if [ "$_prev_key" != "$_current_key" ]; then
        # This helps us prevent firing the command multiple times, because
        # inotify raises multiple events
        _prev_key="$_current_key"

        $(echo $command | sed "s#{file}#$file_name#g" )
    fi
done
