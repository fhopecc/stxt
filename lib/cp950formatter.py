import logging
class CP950Formatter(logging.Formatter):
    def format(self, record):
        str = logging.Formatter.format(self, record)
        return str.decode('utf8', 'ignore').encode('cp950', 'ignore')


