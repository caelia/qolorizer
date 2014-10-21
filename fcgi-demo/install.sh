#!/bin/sh

install_base=$1

if [ "${install_base}" == "" ]; then
    echo "Usage: ./install.sh <TARGET_DIR>"
    exit 127
fi

install -d ${install_base}/images/base
#install index.html ${install_base}
install images/base/*.png ${install_base}/images/base

csc qolorizer.fcgi.scm

install qolorizer.fcgi ${install_base}
