#!/usr/bin/env python

import json
import sys
import re

try:
    dotenv = sys.argv[1]
except IndexError as e:
    dotenv = '.env'

with open(dotenv, 'r') as f:
    content = f.readlines()

contentList = [x.strip().split('#')[0].split('=', 1) for x in content if '=' in x.split('#')[0]]
contentDict = dict(contentList)
for k, v in contentList:
    for i, x in enumerate(v.split('$')[1:]):
        key = re.findall(r'\w+', x)[0]
        v = v.replace('$' + key, contentDict[key])
    contentDict[k] = v.strip()

print(json.dumps(contentDict))