#!/bin/bash

curl -X PUT http://guest:guest@localhost:8000/endpoint/x/"$1"
