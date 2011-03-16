#!/usr/bin/env python

"""
Sample rest service for mod_log_rest

Author: Brendon Crawford <brendon@last.vc>
"""

import json
import logging
import uuid
from tornado.ioloop import IOLoop
from tornado.web import RequestHandler, Application
import settings


logging.basicConfig(level=logging.INFO)


def main():
    """
    Main
    
    Returns: int
    """
    application = Application(_routes())
    application.listen(settings.LISTEN_PORT, settings.LISTEN_HOST)
    IOLoop.instance().start()
    return 0


def _routes():
    """
    Define URL routes
    
    Returns: list
        List of tuples
    """
    out = [
        (r'/messages/add/?', AdderHandler)
    ]
    return out


class Handler(RequestHandler):
    """
    Handler Super Class for App
    """
    
    def dump(self, status, req_type, **val):
        out = val
        out['status'] = status
        out['req_type'] = req_type
        out['response_id'] = uuid.uuid4().hex
        return self.write(json.dumps(val))


class AdderHandler(Handler):
    """
    Handler to Add New Nodes
    """

    def post(self):
        """
        Get Request
        
        Returns: bool
        """
        subject = None
        body = None
        from_jid = None
        to_jid = None
        if self.request.arguments.has_key('subject'):
            subject = self.get_argument('subject')
        if self.request.arguments.has_key('body'):
            body = self.get_argument('body')
        if self.request.arguments.has_key('from_jid'):
            from_jid = self.get_argument('from_jid')
        if self.request.arguments.has_key('to_jid'):
            to_jid = self.get_argument('to_jid')
        self.act(from_jid, to_jid, body, subject)
        self.dump(100, 1)
        return True

    def act(self, from_jid, to_jid, body, subject):
        """
        Perform action
        """
        logging.info("ADD:\nfrom_jid:%s\nto_jid:%s\nsubject:%s\nbody:%s\n" % \
                     (from_jid, to_jid, subject, body))
        return True


if __name__ == "__main__":
    exit(main())


