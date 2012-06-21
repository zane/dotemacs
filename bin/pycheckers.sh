#!/bin/bash

# http://reinout.vanrees.org/weblog/2010/05/11/pep8-pyflakes-emacs.html

# epylint "$1" 2>/dev/null
pyflakes "$1"
# pep8 --ignore=E501,E221,E701,E202,E241,E203 --repeat "$1"
true
