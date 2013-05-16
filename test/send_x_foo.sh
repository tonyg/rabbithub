#!/bin/bash

curl -v -d "$1" http://guest:guest@localhost:15670/endpoint/x/amq.direct?hub.topic=foo
