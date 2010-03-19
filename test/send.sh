#!/bin/bash

curl -d "$1" http://guest:guest@localhost:55672/rabbithub/endpoint/$2?hub.topic=$3
