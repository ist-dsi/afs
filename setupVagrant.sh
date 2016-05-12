#!/bin/bash

# Install the latest version of VirtualBox


# Install the latest version of Vagrant
curl -L https://releases.hashicorp.com/vagrant/1.8.1/vagrant_1.8.1_x86_64.deb > vagrant.deb
dpkg --force-confnew -i vagrant.deb