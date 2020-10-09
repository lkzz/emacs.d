#!/usr/bin/env bash

pid=$(ps -ef | grep -i "emacs --daemon" | grep -v grep | awk '{print $2}')
if [[ "$pid" = "" ]];then
    echo "emacs daemon not running"
else
    echo "emacs daemon(pid: $pid) will be killed"
    kill -9 $pid
fi

echo "emacs daemon starting"
emacs --daemon
