#!/bin/sh

amixer -D pulse sget Master | grep "Left:" | cut -d' ' -f7 | cut -d'[' -f2 | cut -d']' -f1
