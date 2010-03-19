#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://localhost:8888/sub1&hub.secret=$3&hub.topic=$2&hub.verify=sync&hub.verify=async" http://guest:guest@localhost:55672/rabbithub/subscribe/$1
