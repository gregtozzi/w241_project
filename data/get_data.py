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
    print(err) 
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

EXPECTED_ACTIONS = ['open', 'click', 'bounce']
fieldnames = ['email', 'treatment']
for n in EXPECTED_ACTIONS:
    fieldnames.append(n)
    fieldnames.append(n + '_time')
    fieldnames.append(n + '_count')
csvwriter = csv.DictWriter(sys.stdout,fieldnames=fieldnames)
csvwriter.writeheader()



def process_actions(actions):
    email_actions_count = len(actions)
    if len(actions) > 0:
        email_actions = 1
        email_actions_time = actions[0]['timestamp']
    else:
        email_actions = 0
        email_actions_time = ''
    return (email_actions, email_actions_time, email_actions_count)

for c in CAMPAIGNS.values():
    treat_no = c['treatment']
    campaign_id = c['id']
    # no need to page the results because we are under their maximum of 1000
    email_activity = client.reports.get_email_activity_for_campaign(campaign_id=campaign_id,count=count)
    LOGGER.info('Total items %s total returned %s', email_activity['total_items'], len(email_activity['emails']))
    for e in email_activity['emails']:
        email_address = e['email_address']
        email_hash = hashlib.sha256(email_address.encode('utf-8')).hexdigest()

        row = {}
        if PLAINTEXT:
            row['email'] = email_address
        else:
            row['email'] = email_hash
        row['treatment'] = treat_no

        for n in EXPECTED_ACTIONS:
            b, t, c = process_actions([a for a in e['activity'] if a['action'] == n])
            row[n] = b
            row[n + '_time'] = t
            row[n + '_count'] = c            
        
        other_actions = [a for a in e['activity'] if a['action'] not in EXPECTED_ACTIONS]
        if len(other_actions) > 0:
            LOGGER.error('unexpected actions for user %s: %s', row['email'], str(other_actions))
        
        csvwriter.writerow(row)
