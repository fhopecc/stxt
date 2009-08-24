#!/usr/bin/python
# -*- encoding: utf-8 -*-

import re
import os

def normalize(pathname):
	pathname = re.sub(r'([\.:]){2,}', r'\1', pathname)
	pathname = re.sub(r'\.$', '', pathname)
	return pathname

def basename(pathname):
	i = pathname.rfind('.') + 1
	return pathname[i:]
