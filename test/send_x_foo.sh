#!/bin/bash

curl -v -d "$1" http://guest:guest@localhost:55670/endpoint/x/amq.direct?hub.topic=foo
