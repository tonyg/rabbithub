#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://10.1.1.10:4567/sub2&hub.topic=foo&hub.verify=sync&hub.lease_seconds=600" http://guest:guest@localhost:15670/subscribe/q/foo

