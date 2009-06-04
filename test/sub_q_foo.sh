#!/bin/bash

curl -vd "hub.mode=subscribe&hub.callback=http://localhost:8888/sub2&hub.topic=foo&hub.verify=sync" http://guest:guest@localhost:8000/subscribe/q/foo
