#!/usr/bin/env python
# -*- coding: utf-8 -*-

# For Colour experiment
# Take a set of eafs, and create csv files where each line is one sign, with data on where it came from etc.

# Requires pympi.   https://github.com/dopefishh/pympi
#  Only works with python3 due to utf-8 issues


import pympi
import glob
import sys

turnmargin = 50
trialmargin = 100



ignoreIconicityFiles= ["Colour_game_01_1_Jordan-India_After_one_week.eaf","Colour_game_01_2_Jordan-Indonesia_After_one_week.eaf","Colour_game_01_3_Indonesia-India_After_one_week.eaf","Colour_game_01_3_Jordan-Nepal_After_one_week.eaf","Colour_game_04_2_Jordan-Indonesia_After_three_week.eaf", "Colour_game_04_2_Nepal-India_After_three_weeks.eaf","Colour_game_04_3_Jordan-India_After_three_weeks.eaf"]

ignoreCheck = ["Colour_game_04_1_Indonesia-India_After_three_week.eaf"]

eaffolder = "../data/Kangsuk's annotation/"
resultsfolder = '../data/processedData/'

foldersToProcess = ["one/","three/"]


def processFile(eafpath):

	res = "file,player,turnID,turnType,turnStart,turnEnd,turnLength,modality,signalStart,signalEnd,signalLength, signalType\n"

	eaffile = pympi.Eaf(eafpath)
	filename = eafpath
	if filename.count("/")>0:
		filename = eafpath[eafpath.rindex("/")+1:]

	
	for t in sorted(eaffile.get_tier_names()):
		print(t)

	
	#getTrialLengths(eaffile,filename)

	
	return getVariants(eaffile,filename)



def getTrialLengths(eaffile,filename):
	
	out = [["filename",'tier','start','end','length','value']]
	for tier in [u"Color_1(자원)",u"Color_2(자원)"]:
		annotations = eaffile.get_annotation_data_for_tier(tier)
		for a in annotations:
			out.append([filename,tier,a[0],a[1], a[1]-a[0],a[2]])

	writeData(out,"lengths.csv")


def getSignBits(sign_value):
	sign_value = sign_value.replace('"',"").replace("\n","").replace("\r","").replace(",","")
	sign_bits = sign_value.split(":")
	if len(sign_bits)==2:
		val,notes =  (sign_bits[1],sign_bits[0])
		if val.count("(") > 0:
			notes += "#" +val[val.index("(")+1:].replace(")","")
			val = val[:val.index("(")]
		return (val,notes)
	else:
		return (sign_value,"NA")


def getVariantHeader():

	return( [[
		"filename",
		"director",
		"speaker",
		"trial_value",
		"trial_start",
		"trial_end",
		"trial_length",  # length
		"sign_start",
		"sign_end",
		"sign_length", # length
		"sign_value",
		"sign_notes",
		"iconic",
		"signOrigin",
		"T0Check"
		]])
		
def getTierNames(eaffile):
	colourTierName = "Color_"+director+"(자원)"
	if not colourTierName in eaffile.get_tier_names():
		colourTierName = "COLOR_"+director+"(자원)"
	

def getVariants(eaffile,filename):
	out = []
	for director in ["1","2"]:
		
		director_name = director
		
		colourTierNames = ["Color_"+director+"(자원)","COLOR_"+director+"(자원)","Simulus_"+director,"Stimulus_"+director, "Color(Stimulus)_"+director]
		
		
		
		colourTierName = [x for x in colourTierNames if x in eaffile.get_tier_names()][0]	
		
		trials = eaffile.get_annotation_data_for_tier(colourTierName)
		
		for trial_start, trial_end, trial_value in trials:
			
			for speaker in ["1","2"]:
				
				speaker_name = speaker
				
				signTierNames = ["Formal category/Sign_"+speaker, "Formal Category/ SIGN(RH)_"+speaker]
				signTierName = [x for x in signTierNames if x in eaffile.get_tier_names()][0]
				
				signs = eaffile.get_annotation_data_between_times(signTierName, trial_start - trialmargin, trial_end + trialmargin)
		
				for sign_start, sign_end, sign_value in signs:
					
					iconic = getChildTiersBetweenTimes(eaffile, "Iconicity_"+speaker, sign_start, sign_end)
					signOriginTierNames = ["Sign_Orign_"+speaker,"Sign(RH)_Origin_"+speaker,"Sign_Orign_"+speaker+speaker, "Sign_Orign("+{"2":"LH","1":"RH"}[speaker]+")_"+speaker,"Sign_Orign("+{"2":"LH","1":"RH"}[speaker]+")_1","Sign_Orign(RH)_2","Sign_Origin_"+speaker]
					

					print(signOriginTierNames)
					signOriginTierName = [x for x in signOriginTierNames if x in eaffile.get_tier_names()][0]	
					
					signOrigin = getChildTiersBetweenTimes(eaffile, signOriginTierName, sign_start, sign_end)
					
					# try marker tiers can cover more than one sign
					tryMarkerTierName = "Try-marker_"+speaker
					tryCandidates = eaffile.get_annotation_data_between_times(
						tryMarkerTierName, 
						trial_start-turnmargin,
						trial_end+turnmargin)
					tryCandidates = [x for x in tryCandidates if (x[0]-turnmargin) < sign_start and (x[1]+turnmargin) > sign_end]
					if(len(tryCandidates)>0):
						tryMarker = tryCandidates[0][2]
					else:
						tryMarker = "No"
					
					sign_value, sign_notes = getSignBits(sign_value)
					
					out.append([
						filename,
						director_name,
						speaker_name,
						trial_value,
						trial_start,
						trial_end,
						trial_end-trial_start,  # length
						sign_start,
						sign_end,
						sign_end - sign_start, # length
						sign_value,
						sign_notes,
						iconic[2],
						signOrigin[2],
						"FALSE"
					])
				
				checkTierNames = ["T0/Check_"+speaker, "repair initiation_"+speaker,"Repair initiation_"+speaker,"T0/check_"+speaker]
				
				checkTierName = [x for x in checkTierNames if x in eaffile.get_tier_names()]
				
				if (not (filename in ignoreCheck)) and len(checkTierName)>0:
					checkTierName = checkTierName[0]
					checks = eaffile.get_annotation_data_between_times(checkTierName, trial_start - trialmargin, trial_end + trialmargin)
					for check_start, check_end, check_value in checks:
					
						iconic = [-1,-1,"NA",""]
						print (filename)
						if not filename in ignoreIconicityFiles:
							iconic = getChildTiersBetweenTimes(eaffile, "Iconicity of T0_"+speaker, check_start, check_end)
					
						check_value, check_notes = getSignBits(check_value)
					
						out.append([
							filename,
							director_name,
							speaker_name,
							trial_value,
							trial_start,
							trial_end,
							trial_end-trial_start,  # length
							check_start,
							check_end,
							check_end - check_start, # length
							check_value,
							check_notes,
							iconic[2],
							"NA",
							"TRUE"
						])
		
	return(out)








def getChildTiersBetweenTimes(eaffile, tier, sign_start, sign_end):
	anx = eaffile.get_annotation_data_between_times(tier, sign_start-turnmargin, sign_end+turnmargin)
	for a in anx:
		if a[0] > (sign_start-turnmargin) and a[1] < (sign_end+turnmargin):
			return a
	return (-1,-1,"","")

def writeData(list,filename):
	list2 = list2csv(list)
	o = open(filename,'w')
	o.write(list2)
	o.close()	

def list2csv(list):
	return u"\n".join([u",".join([str(y) for y in x]) for x in list])


res = getVariantHeader()

for folder in foldersToProcess:
	files = glob.glob(eaffolder+folder+'*.eaf')
	print(folder,files)
	for eafpath in files:
		print(eafpath)
		fileOut = processFile(eafpath)
		print( len(fileOut))
		for oxx in fileOut:
			res.append(oxx)

writeData(res,resultsfolder+"variants.csv")