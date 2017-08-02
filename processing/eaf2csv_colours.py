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
teachmargin = 50  # changed from 200


ignoreIconicity = True
#ignoreIconicityFiles= ["Colour_game_01_1_Jordan-India_After_one_week.eaf","Colour_game_01_2_Jordan-Indonesia_After_one_week.eaf","Colour_game_01_3_Indonesia-India_After_one_week.eaf","Colour_game_01_3_Jordan-Nepal_After_one_week.eaf","Colour_game_04_2_Jordan-Indonesia_After_three_week.eaf", "Colour_game_04_2_Nepal-India_After_three_weeks.eaf","Colour_game_04_3_Jordan-India_After_three_weeks.eaf"]

ignoreCheck = []#["Colour_game_04_1_Indonesia-India_After_three_week.eaf"]

eaffolder = "../data/Kangsuk's annotation/"
resultsfolder = '../data/processedData/'

foldersToProcess = ["one/","three/"]

def getChildTiersBetweenTimes(eaffile, tier, sign_start, sign_end):
	anx = eaffile.get_annotation_data_between_times(tier, sign_start-turnmargin, sign_end+turnmargin)
	for a in anx:
		if a[0] > (sign_start-turnmargin) and a[1] < (sign_end+turnmargin):
			return a
	return [-1,-1,"NA",""]

def writeData(list,filename):
	list2 = list2csv(list)
	o = open(filename,'w')
	o.write(list2)
	o.close()	

def list2csv(list):
	return u"\n".join([u",".join([str(y) for y in x]) for x in list])


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
		#if val.count("(") > 0:
		#	notes += "#" +val[val.index("(")+1:].replace(")","")
		#	val = val[:val.index("(")]
		return (val,notes)
	else:
		return (sign_value,"NA")


def getVariantHeader():

	return( [[
		"filename",
		"trialID",
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
		"T0Check",
		"TryMarked",
		"Indexicality",
		"Teach",
		"T_minus_1",
		"T0"
		]])
		
def getTierNames(eaffile):
	colourTierName = "Color_"+director+"(자원)"
	if not colourTierName in eaffile.get_tier_names():
		colourTierName = "COLOR_"+director+"(자원)"
	

## Main function to extract data from a file
def getVariants(eaffile,filename):
	out = []
	trialID = 0
	for director in ["1","2"]:
		
		director_name = director
		
		colourTierNames = ["Color_"+director+"(자원)","COLOR_"+director+"(자원)","Simulus_"+director,"Stimulus_"+director, "Color(Stimulus)_"+director]
		
		
		
		colourTierName = [x for x in colourTierNames if x in eaffile.get_tier_names()][0]	
		
		trials = eaffile.get_annotation_data_for_tier(colourTierName)
		
		for trial_start, trial_end, trial_value in trials:
			trialID += 1
			for speaker in ["1","2"]:
				listener = "1"
				if speaker == "1":
					listener = "2"
				
				speaker_name = speaker
				
				signTierNames = ["Formal category/Sign_"+speaker, "Formal Category/ SIGN(RH)_"+speaker, "Formal Category/Sign_2"]
				signTierName = [x for x in signTierNames if x in eaffile.get_tier_names()][0]
				
				signs = eaffile.get_annotation_data_between_times(signTierName, trial_start - trialmargin, trial_end + trialmargin)
		
				for sign_start, sign_end, sign_value in signs:
					
					###########
					# Iconicity
					if not ignoreIconicity:
						iconic = getChildTiersBetweenTimes(eaffile, "Iconicity_"+speaker, sign_start, sign_end)
					else:
						iconic = [-1,-1,"NA"]
					
					###########
					# Sign origin
					signOriginTierNames = ["Sign_Orign_"+speaker,"Sign(RH)_Origin_"+speaker,"Sign_Orign_"+speaker+speaker, "Sign_Orign("+{"2":"LH","1":"RH"}[speaker]+")_"+speaker,"Sign_Orign("+{"2":"LH","1":"RH"}[speaker]+")_1","Sign_Orign(RH)_2","Sign_Origin_"+speaker]
					

					print(signOriginTierNames)
					signOriginTierName = [x for x in signOriginTierNames if x in eaffile.get_tier_names()][0]	
					
					signOrigin = getChildTiersBetweenTimes(eaffile, signOriginTierName, sign_start, sign_end)
					
					############
					# T-1 value
					#Find annotations in the T-1 tier (named "T-1_Repair") that occur 
					# in the times of the sign annotation (+ or - 50 milliseconds).
					# If there is an annotation, and it is "Yes" 
					#   then *this sign is a problem source*
					
					#tm1_tierNames = ["T-1_Repair_"+speaker]
					#tm1_tierName = [x for x in tm1_tierNames if x in eaffile.get_tier_names()][0]
					tm1_tierName = "T-1_Repair_"+speaker
					tm1Candidates = eaffile.get_annotation_data_between_times(
						tm1_tierName, 
						trial_start-turnmargin,
						trial_end+turnmargin)
					tm1Candidates = [x for x in tm1Candidates if (x[0]-turnmargin) < sign_start and (x[1]+turnmargin) > sign_end]
					if len(tm1Candidates)>0:
						tm1 = tm1Candidates[0][2]
					else:
						tm1 = "No"
					
					
					
					###########
					# Try Markers					
					# try marker annotations can cover more than one sign
					tryMarkerTierName = "Try-marker_"+speaker
					tryCandidates = eaffile.get_annotation_data_between_times(
						tryMarkerTierName, 
						trial_start-turnmargin,
						trial_end+turnmargin)
					tryCandidates = [x for x in tryCandidates if (x[0]-turnmargin) < sign_start and (x[1]+turnmargin) > sign_end]
					if(len(tryCandidates)>0):
						tryMarker = tryCandidates[0][2]
						#if tryMarker == "":
						#	tryMarker = "Yes"
					else:
						tryMarker = "No"
					
					
					###########
					# Indexicality
					
					indexicalityTierNames = ["Indexical_"+speaker, "indexical_"+speaker]
					indexicalityTierName = [x for x in indexicalityTierNames if x in eaffile.get_tier_names()][0]
					indexicality = getChildTiersBetweenTimes(eaffile, indexicalityTierName, sign_start, sign_end)

					###########
					# Teaching
					# teach annotations can cover more than one sign
					teachMarkerTierNames = ["Teach Sign_"+speaker, "Teach_"+speaker]
					teachMarkerTierName = [x for x in teachMarkerTierNames if x in eaffile.get_tier_names()][0]
					teachCandidates = eaffile.get_annotation_data_between_times(
						teachMarkerTierName, 
						trial_start-trialmargin,
						trial_end+trialmargin)
					teachCandidates = [x for x in teachCandidates if (x[0]-teachmargin) < sign_start and (x[1]+teachmargin) > sign_end]
					if(len(teachCandidates)>0):
						teach = teachCandidates[0][2]
						#if teach == "":
						#	teach = "Yes"
					else:
						teach = "No"

					
					sign_value, sign_notes = getSignBits(sign_value)
					
					out.append([
						filename,
						trialID,
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
						"FALSE",
						tryMarker,
						indexicality[2],
						teach,
						tm1,
						"No", #T0 check
					])
				
				
				
				# T0/Check tiers
				#  These are produced by the listener
				#  Check tiers (OIRs etc.) from other player are on a different tier
				#  can be independent from colour signs, so list them as separate cases
				
				
				checkTierNames = ["T0/Check_"+listener, "repair initiation_"+listener,"Repair initiation_"+listener,"T0/check_"+listener]
				
				checkTierName = [x for x in checkTierNames if x in eaffile.get_tier_names()]
				
				if (not (filename in ignoreCheck)) and len(checkTierName)>0:
				
					#For each annotation in the check tier ("T0/Check_1", "repair 
					# initiation_1","Repair initiation_1","T0/check_1","T0/Check_2",
					#  "repair initiation_2","Repair initiation_2","T0/check_2"
					#  )(within this trial):
	
				
					checkTierName = checkTierName[0]
					checks = eaffile.get_annotation_data_between_times(checkTierName, trial_start - trialmargin, trial_end + trialmargin)
					for check_start, check_end, check_value in checks:
					
						#		All of these signs *are T0Check signs*
						is_t0Check = "Yes"
					
					
						#Find annotations in the candidate understanding tier ("T0_1_candidate(understand)”, 
						#“T0_2_candidate(understand)") that occur within the times
						# 	of the sign annotation (+ or - 50 milliseconds).
						#If there is an annotation, and the annotation label is "Yes",
						# then *this is a candidate understanding form*				
						
											#  T0_1_canditate(understand)	
						t0_check_tierNames = ["T0_"+listener+"_candidate(understand)", "T0_"+listener+"_canditate(understand)"]
						t0_check_tierName = [x for x in t0_check_tierNames if x in eaffile.get_tier_names()][0]
						
						t0_check_labels = getChildTiersBetweenTimes(eaffile, t0_check_tierName, check_start, check_end)
						if len(t0_check_labels)>0:
							t0_candidateUnderstanding = t0_check_labels[2]
						else:
							t0_candidateUnderstanding = "No"
						
					
					
						iconic = [-1,-1,"NA",""]
						
						
						#if not filename in ignoreIconicityFiles:
						if not ignoreIconicity:
							print (filename)
							for x in sorted(eaffile.get_tier_names()):
								print(x)
							iconicityTierNames = ["T0_"+listener+"_iconicity","Iconicity of T0_"+listener,"T0_"+listener+"_Iconicity"]
							iconicityTierName = [x for x in iconicityTierNames if x in eaffile.get_tier_names()][0]
							iconic = getChildTiersBetweenTimes(eaffile, iconicityTierName, check_start, check_end)
						
						### get origin of check
						
						checkOriginTierName = ["T0_"+listener+"_Origin"]
						checkOriginTierName = checkOriginTierName[0]
						checkOrigin = getChildTiersBetweenTimes(eaffile, checkOriginTierName, check_start, check_end)
						
						
						# this is wrong - we don't want the sign origin, but the origin of
						# the sign used in the repair
						#print(signOriginTierNames)
						#signOriginTierName = [x for x in signOriginTierNames if x in eaffile.get_tier_names()][0]	
						#signOrigin = getChildTiersBetweenTimes(eaffile, signOriginTierName, sign_start, sign_end)
					

						# Indexicality 
						
						# TODO: should this be the speaker????
						check_indexicalityTierNames = ["T0_Indexical_"+listener, "T0_indexical_"+listener,"T0_indixical_"+listener,"T0_"+listener+"_indexical"]
						print(filename)
						check_indexicalityTierName = [x for x in check_indexicalityTierNames if x in eaffile.get_tier_names()][0]
						check_indexicality = getChildTiersBetweenTimes(eaffile, check_indexicalityTierName, check_start, check_end)
					
						# TODO: does this need to be done for listeners?

					
						###
						check_value, check_notes = getSignBits(check_value)
					
						out.append([
							filename,
							trialID,
							director_name,
							listener,  # it's the listener who's speaking in a t0
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
							checkOrigin[2],  # sign origin
							"TRUE",
							"No",  # try marker
							check_indexicality[2],
							"No",#check_teach
							"No",# T-1
							is_t0Check
						])
		
	return(out)










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