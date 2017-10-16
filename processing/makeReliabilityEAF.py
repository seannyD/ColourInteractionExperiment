#!/usr/bin/env python
# -*- coding: utf-8 -*-

# For Colour experiment
# Take a set of eafs, and create csv files where each line is one sign, with data on where it came from etc.

# Requires pympi.   https://github.com/dopefishh/pympi
#  Only works with python3 due to utf-8 issues


import pympi
import glob
import sys
import csv

r = []

with open("../data/processing/RandomVariants.csv", 'rb') as csvfile:
	reader = csv.reader(csvfile, delimiter=' ', quotechar='|')
	for row in reader:
		r.append(row)
		
print r

