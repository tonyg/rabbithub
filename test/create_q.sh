#!/bin/bash

curl -v -X PUT http://guest:guest@localhost:15670/endpoint/q/"$1"
