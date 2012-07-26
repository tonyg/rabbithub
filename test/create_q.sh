#!/bin/bash

curl -v -X PUT http://guest:guest@localhost:55670/endpoint/q/"$1"
