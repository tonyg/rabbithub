#!/bin/bash

token=$(curl -fv "http://guest:guest@localhost:15670/endpoint/q/foo?hub.mode=generate_token&hub.intended_use=unsubscribe")
if [ $? = 0 ]
then
    curl -vd "hub.mode=unsubscribe&hub.callback=http://localhost:15670/endpoint/q/foo&hub.topic=foo&hub.verify=sync&hub.verify=async&${token}" http://guest:guest@localhost:15670/subscribe/x/amq.direct
else
    echo "Token generation failed"
fi

