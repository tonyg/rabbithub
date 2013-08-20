#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://localhost:4567/sub1&hub.topic=foo&hub.verify=sync&hub.verify=async&hub.lease_seconds=600" http://guest:guest@localhost:15670/subscribe/x/amq.direct
