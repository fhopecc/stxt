# coding=utf8
from __future__ import with_statement
from optparse import OptionParser
import sys, os, re, pyPdf

def getPDFContent(path):
    content = ""
    # Load PDF into pyPDF
    pdf = pyPdf.PdfFileReader(file(path, "rb"))
    # Iterate pages
    for i in range(0, pdf.getNumPages()):
        # Extract text from page and add to content
        content += pdf.getPage(i).extractText() + u"\n"
    # Collapse whitespace
    #content = u" ".join(content.replace("\xa0", " ").strip().split())
    return content

#import pdb; pdb.set_trace()
'''
0.1:可抽取 pdf 
'''
if __name__ == "__main__":
    usage = u"usage: %prog file [options]"
    parser = OptionParser(usage, version="%prog 1.1", 
             description=u"抽取文件中的文字"
        )
    parser.add_option("-t", "--top", type='int', dest="top", 
                      help=u"指定訊息格式")

    (options, args) = parser.parse_args()

    path = args[0]
    with open(path + ".txt", 'wb') as f:
        f.write(getPDFContent(path))
