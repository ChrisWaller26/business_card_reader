#### Libraries ####

from PIL import Image
import numpy as np
import pandas as pd
import os
import glob
import pytesseract
import argparse
import cv2
import re

#### Image Splitter ####

def crop(input, output, rows, cols):
    im = Image.open(input)
    imgwidth, imgheight = im.size
    height, width = int(imgheight / rows), int(imgwidth / cols)
    
    for row in range(rows):
        for col in range(cols):
            box = (col * width, row * height, (col + 1) * width, (row + 1) * height)
            try:
                im.crop(box).save(output + "_" + str(row + 1) + "_" + str(col + 1) + ".png")
            except:
                pass

crop("Input/Business_Cards_01.jpg", 
     "Output/Business_Cards_01", 5, 5)

#### Text Reader ####

pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'

def read_files(paths):
    i = 0
    out = []
    im_files = glob.glob(paths)
    for file in im_files:
        out_str = pytesseract.image_to_string(Image.open(file))
        out_str_spl = re.split("[\||\n]", out_str)
        out_str_spl_ws = list(filter(None, [x.strip(' ') for x in out_str_spl]))
        out += [out_str_spl_ws]
        i += 1
    return(out)

output = read_files("Output/*.png")
