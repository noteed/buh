#! /bin/bash

# Generate SSH key pair with no passphrase.
ssh-keygen -t rsa \
  -C buh-insecure@example.com \
  -f buh_insecure_id_rsa \
  -N "" \
  > /dev/null
