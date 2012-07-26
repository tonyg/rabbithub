#!/bin/bash

curl -v -X DELETE http://guest:guest@localhost:55670/endpoint/q/"$1"
