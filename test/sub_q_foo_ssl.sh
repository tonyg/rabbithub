#!/bin/bash

curl -k -v -d "hub.mode=subscribe&hub.callback=http://localhost:4567/sub2&hub.topic=foo&hub.verify=sync&hub.lease_seconds=600" https://guest:guest@localhost:15671/subscribe/q/foo

