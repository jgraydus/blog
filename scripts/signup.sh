#!/bin/bash

# usage:
# ./signup <EMAIL> <PASSWORD>

function signup() {

if [[ -z $1 ]]; then
    local email='test@test.com'
else
    local email=$1
fi

if [[ -z $2 ]]; then
    local password='test'
else
    local password=$2
fi

local result=$(curl -v --no-progress-meter \
    http://localhost:8000/api/signup \
    -H "Content-Type: application/json" \
    -d "{ \"emailAddress\": \"${email}\", \"password\": \"${password}\" }" 2>&1)

if [[ -z $(echo "$result" | grep "200 OK") ]]; then
   echo "FAILURE"
   echo "reason: $(echo "$result" | tail -n 1)" 
else
   echo "SUCCESS"
   echo "userId: $(echo "$result" | tail -n 1)"
   # HTTP messages end with CRLF per the spec, hence the extra sed to remove \r
   local auth_token="$(echo "$result" | \
                       grep "authorization=" | \
                       sed -r 's/.*authorization=(.*)$/\1/' | \
                       sed -r 's/\r//')"
   echo "auth token: $auth_token"
   export SANDBOX_AUTH_TOKEN=$auth_token
fi

}

signup $1 $2
