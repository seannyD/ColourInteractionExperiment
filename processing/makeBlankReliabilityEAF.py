#!/usr/bin/env python
# -*- coding: utf-8 -*-


# Requires pympi.   https://github.com/dopefishh/pympi
#  Only works with python3 due to utf-8 issues


import pympi
import glob
import sys
import csv
import copy

r = []

o = open("../data/reliability/RandomVariants_Balanced_ForConnie.csv")
d = o.read()
o.close()

rows = [x.replace('"',"").split(",") for x in d.split("\n")]

header = rows[0]

print(header)

fileList = dict()
currentFilename = "XXX"
for row in rows[1:-1]:
	filename = "../data/Kangsuk's annotation/one/"+row[header.index("filename")]
	data = (row[header.index("sign_start")],row[header.index("sign_end")])	
	try:
		fileList[filename].append(data)
	except:
		fileList[filename] = [data]


#print(fileList)

tierNames = ["TryMarked","T0","Tminus1","Teach"]



for k in list(fileList.keys()):
	file = fileList[k]
	fn = k
	fn = fn[fn.rindex('/')+1:]
	filename = k
	
	print(filename)


	
	eaffile = pympi.Eaf(filename)
	
	tiersToRemove = list(eaffile.get_tier_names())
	
	for tier in tierNames:
		eaffile.add_tier(tier)

	for trial in file:
		print(int(trial[1]))
		for tier in tierNames:
			eaffile.add_annotation(tier, int(trial[0]), int(trial[1]))

	for tier in tiersToRemove:
		eaffile.remove_tier(tier)
	
	outfile = "../data/reliability/BLANK_"+fn
	print(outfile)
	eaffile.to_file(outfile)
	
	