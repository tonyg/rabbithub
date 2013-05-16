#!/bin/bash

curl "http://guest:guest@localhost:15670/endpoint/$1?hub.mode=generate_token&hub.intended_use=$2"
