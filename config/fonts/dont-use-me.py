#!/bin/python
# author: phga
# date: 2020-04-11
# desc: script to convert bdf font to otb (thanks pango/Harfbuzz)
# depends on: p -S fontforge
import sys
import os
import fontforge
sys.argv.pop(0)
if not os.path.exists("./otb"):
    os.makedirs("./otb")
for f in sys.argv:
    print(f)
    font = fontforge.open(f)
    # output format is determined by file extension (otb)
    font.generate("./otb/"+f[:-3]+"otb")
    # free the memory
    font.close()
