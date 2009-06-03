#!/bin/bash

curl -X DELETE http://guest:guest@localhost:8000/endpoint/q/"$1"
