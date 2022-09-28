#!/bin/bash
# author: phga
# date: 2020-04-11
# desc: script to convert bdf font to otb (thanks pango/Harfbuzz)
# the version with fontforge (ff-convert.py) resulted in weird spacing
# depends on: yay -S fonttosfnt
files=$(echo $@ | xargs)
[ -z "$files" ] && echo "convert.sh bdf/t0-XX-uni.bdf bdf/t0-...." && exit
[ ! -d "./otb" ] && mkdir "./otb"
for f in $files; do
    name=$(echo $f | grep -o -e "[a-zA-Z0-9_-]*.bdf")
    [ -z "$name" ] && echo "No bdf files with that name exists: $f" && continue
    echo "Converting ${name}"
    fonttosfnt -b -o "./otb/"${name:0:-4}".otb" $f
done
echo "Cleaning up .bak files"
find * -type f -name *.bak -print0 | xargs -r -0 rm
echo "Cleaning up the font cache, REALLY!!"
fc-cache -r