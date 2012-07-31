#!/bin/bash

# curl -d "$1" http://guest:guest@localhost:55670/endpoint/q/foo?hub.topic=foo
curl -d "$1" http://guest:guest@localhost:55670/endpoint/q/foo
