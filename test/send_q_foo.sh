#!/bin/bash

curl -d "$1" http://guest:guest@localhost:8000/endpoint/q/foo?hub.topic=rk
