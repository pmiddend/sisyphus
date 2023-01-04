#!/usr/bin/env bash

for i in 32 48 96 144 192 240 256; do
    echo "Generating ${i}x${i}"
    inkscape -w "$i" -h "$i" sis.svg -o sis-"$i".png
done
