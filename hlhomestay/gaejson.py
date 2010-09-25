# Simple gae Model json Serialization
# Copyright 2009 Max Battcher. Licensed for use under the Ms-pl.
from django.utils import simplejson as json
from google.appengine.api import users
from google.appengine.ext import db
import datetime

class GaeEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, db.Model):
            return dict([(name, getattr(obj, name)) for name
                in obj.properties().keys()])
        elif isinstance(obj, datetime.date):
            return obj.strftime('%Y-%m-%dT%H:%M:%S')

        elif isinstance(obj, users.User):
            return {
                'nickname': obj.nickname(),
                'email': obj.email(),
                'user_id': obj.user_id(),
                'federated_identity': obj.federated_identity(),
                'federated_provider': obj.federated_provider(),
            }
        return super(GaeEncoder, self).default(obj)

