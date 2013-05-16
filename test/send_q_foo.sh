#!/bin/bash

curl -d "$1" http://guest:guest@localhost:15670/endpoint/q/foo?hub.topic=rk
