#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://10.1.1.8:4567/sub2&hub.topic=foo&hub.verify=sync&hub.lease_seconds=600" http://guest:guest@localhost:55670/subscribe/q/foo

