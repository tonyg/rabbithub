#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://localhost:8888/sub1&hub.topic=foo&hub.verify=sync,async" http://guest:guest@localhost:8000/subscribe/x/amq.direct
