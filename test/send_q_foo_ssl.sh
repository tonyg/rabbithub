#!/bin/bash

curl -k -v -d "$1" https://guest:guest@localhost:15671/endpoint/q/foo?hub.topic=rk
