# coding=utf-8
import sys, os, re, win32com
from win32com.client import Dispatch, constants, DispatchEx
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from spark import GenericASTTraversal

class MSWordOut(GenericASTTraversal):
    def __init__(self, ast):
        GenericASTTraversal.__init__(self, ast)
        msword = DispatchEx('Word.Application')
        msword.Visible = 1	# 1表示要顯示畫面，若為0則不顯示畫面。

        self.word = msword
        self.doc  = msword.Documents.Add() # 開啟一個新的文件。
        self.range	= self.doc.Range()
        # 設定字型為標楷體
        self.range.Font.Name = u"標楷體".encode('cp950')  
        self.preorder()
        #para = self.doc.Paragraphs(1).Range
        #para.Paragraphs(1).Style.Font.Size = 18
        #para.Paragraphs(1).Style.Font.Bold = 1

    def n_doc(self, node):
        para = self.doc.Paragraphs.Last
        #para.Style.Font.Name = u"標楷體".encode('cp950')  
        #para.Style.Font.Size = 18
        #para.Style.Font.Bold = 1
        para.Style.Font.Size = 12
        para.Style.Font.Bold = 0
        para.Range.InsertAfter(node.title)

    def n_sect(self, node):
        #import pdb;pdb.set_trace()
        self.doc.Paragraphs.Add() 
        para = self.doc.Paragraphs.Last
        #para.Format.LeftIndent = 24 * (node.height)
        if node.height == 1:
            para.Format.LeftIndent = 20
            para.Format.FirstLineIndent = -20
        elif node.height == 2:
            para.Format.LeftIndent = 40 
            para.Format.FirstLineIndent = -28
        elif node.height == 3:
            para.Format.LeftIndent = 64
            para.Format.FirstLineIndent = -16
        elif node.height == 4:
            para.Format.LeftIndent = 84
            para.Format.FirstLineIndent = -16
        elif node.height == 5:
            para.Format.LeftIndent = 108
            para.Format.FirstLineIndent = -16

        para.Range.InsertAfter(self.sect_num(node))
        #if node.height > 1 and node.order == 0:
        #elif node.height > 1 and \
        #     node.sibling(node.order-1).type == 'para' :
        #    para.Indent()

    def n_para(self, node):
        para = self.doc.Paragraphs.Last
        if node.order != 0:
            self.doc.Paragraphs.Add()
            para = self.doc.Paragraphs.Last
            para.Format.FirstLineIndent = 0
        para.Range.InsertAfter(node.value)
        
    def sect_num(self, node):
        cbd = [u'零',u'壹',u'貳',u'參',u'肆',u'伍',u'陸',u'柒',
               u'捌',u'玖',u'拾',u'拾壹', u'拾貳',
               u'拾參',u'拾肆',u'拾伍',u'拾陸','柒','捌','玖','拾']
        cd = [u'零',u'一',u'二',u'三',u'四',u'五',u'六',u'七',
              u'八',u'九',u'十', u'十一','十二','十三','十四',
              '十五','十六','十七','十九','二十'
              '二十一','二十二','二十三','二十四','二十五','二十六',
              '二十七','二十八','二十九', '三十', 
              '三十一','三十二','三十三','三十四','三十五','三十六',
              '三十七','三十八','三十九'
             ]
        n = int(node.numbers[-1])
        if node.level == 1:
            return cd[n].encode('big5') + '.'
        elif node.level == 2:
            return '(' + cd[n].encode('big5') + ')' + ' '
        elif node.level == 3:
            return str(n) + '.'
        elif node.level == 4:
            return '(' + str(n) + ')' + ' '
        else:
            return str(n) + '.'
