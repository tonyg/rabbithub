#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://localhost:4567/sub2&hub.topic=foo&hub.verify=sync&hub.lease_seconds=60000" http://guest:guest@localhost:15670/subscribe/q/foo

