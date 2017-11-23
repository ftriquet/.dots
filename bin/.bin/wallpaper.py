#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import requests
import sys
import shutil
import datetime
import os
import pprint

print('Fetching image url')
d = requests.get('https://artpip.com/api/featured').json()
featured_id = d['featured']['artwork']
featured = next((x for x in d['artworks'] if x['_id'] == featured_id), None)
if featured is None:
  print('No featured image, exiting')
  sys.exit(0)

today = str(datetime.datetime.now()).split(' ')[0]
pprint.pprint(featured)
featured_url = featured['url']
r = requests.get(featured_url, stream=True)
if r.status_code is 200:
  print('Downloading image')
  with open('{}/Pictures/daily_wallpaper_{}.png'.format(os.environ['HOME'], today), 'wb') as f:
    r.raw.decode_content = True
    shutil.copyfileobj(r.raw, f)

print('Done')
