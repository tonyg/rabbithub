#!/bin/bash

token=$(curl -fv "http://guest:guest@localhost:8000/endpoint/q/foo?hub.mode=generate_token&hub.intended_use=subscribe")
if [ $? = 0 ]
then
    curl -vd "hub.mode=subscribe&hub.callback=http://localhost:8000/endpoint/q/foo&hub.topic=foo&hub.verify=sync,async&${token}" http://guest:guest@localhost:8000/subscribe/x/amq.direct
else
    echo "Token generation failed"
fi

