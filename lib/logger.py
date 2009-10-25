# coding=utf-8
import logging, os
from logging import info
from logging import error
from logging import handlers
class CP950Formatter(logging.Formatter):
    def format(self, record):
        str = logging.Formatter.format(self, record)
        return str.decode('utf8').encode('cp950')

dblog = os.path.join(os.path.dirname(__file__),'..','db.log')
logging.basicConfig(level=logging.DEBUG,
            format='%(asctime)s %(levelname)s %(message)s',
            filename=dblog)

console = logging.StreamHandler()
console.setLevel(logging.INFO)
# set a format which is simpler for console use
#formatter = Formatter('%(name)-12s: %(levelname)-8s %(message)s')
formatter = CP950Formatter('%(message)s')
# tell the handler to use this format
console.setFormatter(formatter)
# add the handler to the root logger
logging.getLogger('').addHandler(console)
