#!/bin/bash

curl -v -X DELETE http://guest:guest@localhost:15670/endpoint/q/"$1"
