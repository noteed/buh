FROM ubuntu:12.04

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8 

RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y vim

# Install an SSH server.
RUN DEBIAN_FRONTEND=noninteractive apt-get -q -y install openssh-server
# No idea why this directory is not created, but sshd needs it.
RUN mkdir /var/run/sshd

# Install buh dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y libgmp10
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y git

# The buh user is the one used on the backend to run buh.
RUN useradd -m -s /bin/bash buh
RUN mkdir -p /home/buh/.ssh
ADD buh_insecure_id_rsa.pub /home/buh/.ssh/authorized_keys
RUN chown -R buh:buh /home/buh/.ssh

ADD dist/build/buh/buh /usr/bin/buh
