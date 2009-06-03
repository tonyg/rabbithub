#!/bin/bash

curl -d "$1" http://guest:guest@localhost:8000/endpoint/x/amq.direct?hub.topic=foo
