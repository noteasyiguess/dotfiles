#!/bin/sh

if [ $# -lt 2 ]; then
   echo "Format: $0 audio_file video_file" 1>&2
   exit 1
fi

audio=$1
video=$2
out="audio_video.mkv"

ffmpeg -i "$video" -i "$audio" -c:v copy -c:a aac "$out" -shortest
