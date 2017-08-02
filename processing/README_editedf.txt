This is a description of how the program extracts data from the ELAN files.


For each ELAN file, do the following:

Find the colour tiers (named "Color_1(자원)", "Color_2(자원)"  or "COLOR_1(자원)", "COLOR_2(자원)","Simulus_1","Simulus_2","Stimulus_1","Stimulus_2", "Color(Stimulus)_1","Color(Stimulus)_2")

Actually all we need are the stimulus tiers: "Simulus_1","Simulus_2","Stimulus_1","Stimulus_2", "Color(Stimulus)_1","Color(Stimulus)_2"
and also "Stimulus_11", "Stimulus_22[R11]"


This tier has annotations that mark the start and end of each trial (one picture).

For each annotation in the colour tiers.
	Find all the annotations between the start and end times of the colour annotation (+ or - 100 milliseconds), for the following tiers:
	
	SIGN TIERS:
	"Formal category/Sign_1" (participant 1)
	"Formal category/Sign_2" (participant 2)
	These annotations are signs produced by the signers
	
	For each of these signs:
	
		Get the label of the sign annotation:  *This is the variant name*
	
		Get the origin of the sign: 
			Look for annotations in the sign origin tier (named e.g. Sign_Orign_1, Sign_Orign_2[R12]) that occur within the times of the sign annotation (+ or - 50 milliseconds).
		
		Get the try marking value:
			Find annotations in the try marker tier (named "Try-marker_1") [R13]that occur in the times of the sign annotation (+ or - 50 milliseconds).
			If there is an annotation, and it is either "Yes" or blank (""), [R14]then *this sign is try-marked*
		
		Get the indexicality:
			Find annotations in the indexicality tier ("Indexical_1", "Indexical_2") that occur within the times of the sign annotation (+ or - 50 milliseconds).
			Get the values of the first of these annotations
			(in the most recent statistical analysis, indexicality is decided from the excel file with the list of all variants and whether each is indexical or not)
		[R15]
		Get the teaching value:
			Find annotations in the teaching tier ("Teach Sign_1", "Teach Sign_2") that occur within the times of the sign annotation (+ or - 200 milliseconds).
			Get the first of these annotations.
			If there is an annotation, and the annotation label is either "Yes" or blank (""), [R16]then *this is a teaching context*
			
		All of these signs are *not T0Check signs*
		
		Take all of this information and make one line in the results file spreadsheet for it.
			
	
	CHECK TIERS[R17]:
	(e.g. "T0/Check_1", "repair initiation_1","Repair initiation_1","T0/check_1","T0/Check_2", "repair initiation_2","Repair initiation_2","T0/check_2" )
	
	For each annotation in the check tier (within this trial):
		
		All of these signs *are T0Check signs*
		
		Get the origin of the check sign: 
			(as above)
			
		Get the indexicality of the check sign:
			(as above)
			
		Get the teaching value:
			(as above)
			
		Take all of this information and make one line in the results file spreadsheet for it.
	
				
The output of the program is a spreadsheet called "variants.csv" where each line is a particular occurrence of a sign, and each column contains information on that sign (source file, trial number, start time, end time, origin, indexicality, teaching, checking etc.
				
				
[R11]Kangsuk add tier names that are missing but also contain the stimulus items.
[R12]Make sure Sean has the right list.
[R13]Make sure Sean has the right list
[R14]Actually empty annotations of the try-marker tier should not be included they are NOT trymarked.
 
My question: does this also take into account signerhood? 

Also KS questions:what are the exact rules for overlap?
[R15]Ok so we'll not check for this in the ELAN scripts
[R16]Actually this would not be a teaching context.
[R17]Please note that the tiers T-1_repair_ and T-1_repair_2 are  not T0 check s but rather problem sources (T-1)
