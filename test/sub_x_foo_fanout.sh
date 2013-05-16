#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://10.1.1.10:4567/sub1&hub.topic=foo&hub.verify=sync&hub.verify=async" http://guest:guest@localhost:15670/subscribe/x/amq.fanout
