import unittest, re, os, sys
from webob import Request
basedir = os.path.dirname(__file__).replace(os.sep + 'lib','')
sys.path.append(basedir)

def isplural(noun):
    if re.match(r'.*s$', noun):
        return True
    else:
        return False

def get_name(path, method, req=None):
    m = re.match(r'/(?P<resource>\w+)(/(?P<id>\w+)(/(?P<action>\w+))?)?', path)
    if not m:
        return ('index_handler', 'index', None) 
    resource = m.group('resource')
    if m:
        id = m.group('id')
        print req
        if not id:
            try:
                id = req.GET['id']
            except:
                id = None
        action = m.group('action')
    else:
        raise SyntaxError("invalid path:<" + path + ">")
    
    handler = resource + "_handler"
    
    if id:
        if method == 'GET':
            if action:
                method = action
            elif id == 'new':
                method = id
                return (handler, method) 
            else:
                method= 'show' 
        elif method == 'POST':
            method= 'update' 
        elif method == 'PUT':
            method= 'create' 
        elif method == 'DELETE':
            method= 'destroy'
        return (handler, method, id) 
    else:
        if method == 'GET':
            return (handler, 'index', None) 

def route(environ, start_response):
    req = Request(environ)
    hn, mn, id = get_name(environ['PATH_INFO'], 
            environ['REQUEST_METHOD'], req)
    environ['rest.id'] = id
    h = __import__('handler.' + hn, fromlist=['handler'])
    h = reload(h) # in optimize mode, this must be take off
    m = h.__getattribute__(mn)
    return m.__call__(environ, start_response)

class UnitTest(unittest.TestCase):
    def test_is_plural(self):
        self.assertTrue(isplural('users'))
        self.assertFalse(isplural('user'))

    def test_get_name(self):
        self.assertEqual(get_name('/mlogs', 'GET'), 
                                        ('mlogs_handler', 'index', None))   
        self.assertEqual(get_name('/mlogs/11', 'GET'), 
                                        ('mlogs_handler', 'show', '11'))    
        self.assertEqual(get_name('/mlogs/11/edit', 'GET'), 
                                        ('mlogs_handler', 'edit', '11')) 
        self.assertEqual(get_name('/mlogs/11', 'POST'), 
                                        ('mlogs_handler', 'update', '11'))
        self.assertEqual(get_name('/mlogs/new', 'GET'), 
                                        ('mlogs_handler', 'new'))     

        self.assertEqual(get_name('/mlogs/11', 'PUT'), 
                                        ('mlogs_handler', 'create', '11'))

        self.assertEqual(get_name('/mlogs/11', 'DELETE'), 
                                        ('mlogs_handler', 'destroy', '11'))     
    def testBasedir(self):
        self.assertEqual(r'D:\stxt\web', basedir)
        self.assert_(r'D:\stxt\web' in sys.path)
if __name__ == "__main__":
    unittest.main()
