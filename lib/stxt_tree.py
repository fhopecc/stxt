# coding=utf8
import sys
class TreeNode(object):
  def __init__(self):
    self.parent, self.children, self.name = None, [], str(id(self))
    self.unvisited = []
  def append(self, *nodes):
    for n in nodes: 
      n.parent = self
      self.children.append(n)
    return self
  def isRoot(self):
    return self.parent is None
  def height(self):
    if self.isRoot(): return 0
    c, h = self, 0
    while c.parent:
      c = c.parent
      h += 1
    return h
  def _dfs(self, unvisited):
    for c in self.children:
      c._dfs(unvisited)
    unvisited.append(self)
  def dfs(self):
    unvisited = []
    self._dfs(unvisited)
    #print unvisited
    for n in unvisited:
      yield n
class ParseTreeNode(TreeNode):
  def __init__(self, type, token=None):
    TreeNode.__init__(self)
    self.type, self.token= type, token, 
    if not token is None: 
      self.name, self.value = token.name, token.value
  def print_type_tree(self, level_limit=10):
    if self.height() < level_limit:
      print '*' * self.height() + self.type
    for c in self.children: c.print_type_tree(level_limit)
  def print_postfix_tree(self):
    for c in self.children: c.print_postfix_tree()
    print '*' * self.height() + self.type
  def to_doctree(self):
    if self.type in ('book'):
      n = DocTreeNode(self.type)
      if len(self.children) == 2:
        n.append(*self.children[0].to_doctree())
      n.number_children()
      n.count_occurence()
      return n
    elif self.type in ('sect1s', 'content1', 'content2'):
      l = [self.children[0].to_doctree()]
      for c in self.children[1].to_doctree():
        l.append(c)
      return l
    elif self.type in ('list'):
      n = DocTreeNode(self.type)
      n.append(self.children[0].to_doctree())
      for c in self.children[1].to_doctree():
        n.append(c)
      return n
    elif self.type in ('sect1s_', 'content1_', 'content2_', \
                       'list_'):
      if len(self.children) == 2:
        l = [self.children[0].to_doctree()]
        for c in self.children[1].to_doctree():
          l.append(c)
        return l
      return []
    elif self.type in ('sect1', 'sect2'):
      n = DocTreeNode(self.type)
      h = self.children[0]
      n.name, n.title = h.name, h.value
      for c in self.children[1].to_doctree():
        n.append(c)
      return n
    elif self.type in ('PARA', 'LISTITEM'):
      return DocTreeNode(self.type, self.value)
    elif self.type in ('block'):
      return self.children[0].to_doctree()
    elif self.type in ('code'):
      n = DocTreeNode(self.type)
      h = self.children[0]
      n.name, n.title = h.name, h.value
      n.append(self.children[1].to_doctree())
      return n
    elif self.type in ('CODEBLOCK'):
      return DocTreeNode(self.type, self.value, self.token)
    else:
      raise ValueError, "no definition for " + self.type
class DocTreeNode(ParseTreeNode):
  def __init__(self, type, value='', token=None,**attr):
    ParseTreeNode.__init__(self, type, token)
    self.value, self.title = value, ''
    self.number, self.occurence = None, 0
  def __str__(self):
    m = "%s:\n[\n%s\n]" % (self.type, self.value)
    for c in self.children:
      m+=str(c)
    return m
  def __repr__(self):
    return str(self)
  def section_number(self):
    if self.number is None: return '' 
    else: 
      if self.parent.number is None: return str(self.number)
      else: return self.parent.section_number()+'.'+str(self.number)
  def number_children(self):
    cs = self.children
    if self.type in ('book'):
      for i, c in enumerate([c for c in cs if c.type == 'sect1']):
        c.number = i + 1
        c.number_children()
    elif self.type in ('sect1'):
      for i, c in enumerate([c for c in cs if c.type == 'sect2']):
        c.number = i + 1
  def _count_occurence(self, type, o=0):
    for c in self.children:
      if c.type in [type]:
        o = o+1
        c.occurence = o
      o = c._count_occurence(type, o)
    return o
  def count_occurence(self):
    for type in ['code', 'table']:
      if self.type in [type]:
        o = o+1
        self.occurence = o
      self._count_occurence(type)
  def print_tree(self):
    out = '+' * self.height() + self.type + '[' + self.name + ']' \
           +'#'+str(self.occurence)+'#' + self.section_number() + self.title + '\n'
    for c in self.children: out += c.print_tree()
    return out
