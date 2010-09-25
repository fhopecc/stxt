from google.appengine.ext import db
import datetime
import time

SIMPLE_TYPES = (int, long, float, bool, dict, basestring, list)

def to_dict(model):
    output = {}

    for key, prop in model.properties().iteritems():
        value = getattr(model, key)

        if value is None or isinstance(value, SIMPLE_TYPES):
            output[key] = value
        elif isinstance(value, datetime.date):
            # Convert date/datetime to ms-since-epoch ("new Date()").
            dt = datetime.datetime(value.year, value.month, value.day)
            ms = time.mktime(dt.utctimetuple()) * 1000
            ms += getattr(value, 'microseconds', 0) / 1000
            output[key] = int(ms)
        elif isinstance(value, db.Model):
            output[key] = to_dict(value)
        else:
            output[key] = 'user no support'
            #str(prop)
            #raise ValueError('cannot encode ' + repr(prop))

    return output

def date2json(d):
    'month value whose range starts with zero, so Nov = 10, Dec = 11'
    return 'new Date(%d, %d, %d)' % (d.year, d.month - 1, d.day)

