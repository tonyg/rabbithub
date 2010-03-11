#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://localhost:8888/sub1&hub.secret=$2&hub.topic=$1&hub.verify=sync&hub.verify=async" http://guest:guest@localhost:8000/subscribe/x/amq.direct
