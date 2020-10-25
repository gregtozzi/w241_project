"""Data Fetcher for mailchimp

Prereq: install the mailchimp marketing client lib
   * pip install git+https://github.com/mailchimp/mailchimp-marketing-python.git
or see https://github.com/mailchimp/mailchimp-marketing-python
"""

import mailchimp_marketing as MailchimpMarketing
from mailchimp_marketing.api_client import ApiClientError
import hashlib
import csv
import logging
import sys
import getopt

LOGGER = logging.getLogger(__name__)
LOGGER.setLevel('INFO')

argv = sys.argv

try:
    opts, args = getopt.getopt(sys.argv[1:], "k:vpc:", ["api_key=", "count="])
except getopt.GetoptError as err:
    print(err) # will print something like "option -a not recognized"
    sys.exit(2)

api_key = None
PLAINTEXT = False
count = 1000

for o, a in opts:
    if o == "-v":
        logging.basicConfig(level=logging.INFO)
    elif o in ("-p"):
        PLAINTEXT = True
    elif o in ("-k", "--api_key"):
        api_key = a
    elif o in ("-c", "--count"):
        count = int(a)

if api_key is None:
    LOGGER.error("must supply api_key")

# test api connection
try:
    client = MailchimpMarketing.Client()
    client.set_config({
        "api_key": api_key,
        "server": "us2"
    })
    response = client.ping.get()
    LOGGER.info('Connection ok %s', response)
except ApiClientError as error:
    LOGGER.error(error)
    exit(1)

# magic information about the 4 campaigns
CAMPAIGNS = {
    'treatment 1': {'name': 'Catalyst Circle Wave 1 Sub1 SendA',
                    'id': '1a5990cd4a',
                    'web_id': '3554636',
                    'treatment': 1},
    'treatment 2': {'name': 'Catalyst Circle Wave 1 Sub1 SendB',
                    'id': '509a6509ad',
                    'web_id': '3554604',
                    'treatment': 2},
    'treatment 3': {'name': 'Catalyst Circle Wave 1 Sub2 SendA',
                    'id': '9ad4397dd8',
                    'web_id': '3554628',
                    'treatment': 3},
    'treatment 4': {'name': 'Catalyst Circle Wave 1 Sub2 SendB',
                    'id': '800f593130',
                    'web_id': '3554640',
                    'treatment': 4}
}

fieldnames = ['email', 'treatment', 'open', 'open_time']
csvwriter = csv.DictWriter(sys.stdout,fieldnames=fieldnames)
csvwriter.writeheader()

for c in CAMPAIGNS.values():
    treat_no = c['treatment']
    campaign_id = c['id']
    # no need to page the results because we are under their maximum of 1000
    email_activity = client.reports.get_email_activity_for_campaign(campaign_id=campaign_id,count=count)
    LOGGER.info('Total items %s total returned %s', email_activity['total_items'], len(email_activity['emails']))
    for e in email_activity['emails']:
        email_address = e['email_address']
        email_hash = hashlib.sha256(email_address.encode('utf-8')).hexdigest()
        opens = [a for a in e['activity'] if a['action'] == 'open']
        if len(opens) > 0:
            email_open = 1
            email_open_time = opens[0]['timestamp']
        else:
            email_open = 0
            email_open_time = ''
        row = {}
        if PLAINTEXT:
            row['email'] = email_address
        else:
            row['email'] = email_hash
        row['treatment'] = treat_no
        row['open'] = email_open
        row['open_time'] = email_open_time        
        csvwriter.writerow(row)
