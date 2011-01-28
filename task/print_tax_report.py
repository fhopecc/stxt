# coding=utf-8
from __future__ import with_statement
from wget import wget
import Image, ImageSequence
import sys, os, urllib
def download(docno):
    fs = ["#{docno}-01.tif", "#{docno}之附件.tif"]
    url = 'http://www.fdc.gov.tw/public/doc/%s-01.tif' % docno[0:-1]
    local = os.path.join('tmp', '%s.tif' % docno[0:-1])
    wget(url, local)

def split_print(docno):
    local = os.path.join('tmp', '%s.tif' % docno[0:-1])
    index = 1
    im = tiffopen(local)
    index = 1
    for frame in ImageSequence.Iterator(im):
        if index in [1, 4, 7, 9, 12]:
            jpg = os.path.join('tmp', 'frame%d.jpg' % index)
            frame.save(jpg)
            cmd = r'rundll32 shimgvw.dll,ImageView_PrintTo %s sharp' 
            # rundll32 print image, should be given absolute path
            os.system(cmd % os.path.abspath(jpg))
        index = index + 1

def tiffopen(filename):
    cmd = r'"c:\Program Files\GnuWin32\bin\tiffcp.exe" -c none %s temp.tif' 
    print cmd
    os.system(cmd % filename)
    im = Image.open("temp.tif")
    #os.remove("temp.tif") # on windows, you have to load the data first
    return im

def usage():
    usage =  u'依據給定文號下載並列印退稅快報\n'
    usage += u'用法: %s docno \n'
    usage += u'docno: 公文文號\n'
    return usage % os.path.basename(sys.argv[0]) 

if __name__ == '__main__':
    try:
        if len(sys.argv) == 1: raise IndexError()
        elif len(sys.argv) == 2:
            download(sys.argv[1])
            split_print(sys.argv[1])
    except IndexError:
        print usage()
