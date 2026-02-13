#!/bin/bash

TMUXCONF=/home/kfp/quicklisp/local-projects/spadlib/tmux/src/fricas_tmux.conf

WORKDIR=/tmp
FRICAS_CMD="rlwrap fricas -nosman"
EDITOR_CMD="emacs -nw"
GRAPHICS_CMD="gnuplot"
PAGER_CMD="lynx"

tmux select-layout even-vertical
tmux select-layout even-horizontal

tmux -f $TMUXCONF new-session -d -s FriCAS -n Main
tmux send -t FriCAS:Main "cd $WORKDIR" Enter
tmux send -t FriCAS:Main "$FRICAS_CMD" Enter


tmux split-window -h
tmux send "$GRAPHICS_CMD" Enter


tmux split-window  -v -t 1
tmux send  "$EDITOR_CMD" Enter


tmux split-window  -v -t 3
tmux send  "$PAGER_CMD" Enter


tmux attach -t FriCAS:Main

# show pane numbers: C-a q
# FRICAS: )system tmux lsp , send -t 1 ...
# )system tmux kill-server ---> ok