import os.path
def resource(f):
  bs = os.path.basename(f)
  bs = os.path.splitext(bs)
  return bs[0]
def view_path(f, a):
  res = resource(f)
  return os.path.join(os.path.dirname(__file__), '..', 'views', res,
      a + '.html')
def layout_path(r):
 return os.path.join(os.path.dirname(__file__), '..', 'views', 
        'layout', r + '.html')
