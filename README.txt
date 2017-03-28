BRADLEY BAKER
Classification and Feature-Selection Analysis of AD Datasets using R
*******************************************************************
This README provides the basic instructions for running the scripts for the analysis named "Classification and Feature-Selection Analysis of AD Datasets" in its current permutation.

All coding and testing thus far have been performed on R 3.0.2 in RStudio version 0.97.551 for Windows 64-bit. This code has not been tested in full for other versions of R, R-STudio or for other Operating Systems. 
This code was tested on a computer with the following specifications:
	Manufacturer : ALIENWARE
	Model : ALIENWARE M14X
	Processsor : INTEL(R) CORE(TM) i7-2630QM
		CPU @ 2.00GHz
	Installed memory (RAM) : 8.00GB
	System type : 64-bit OS
 It has not yet been tested on any other machines.
This code has not been tested for compatibility with any libraries other than those required by the code. The following libraries are used in the code and MUST BE INSTALLED PRIOR TO RUNNING:
	knitr
	GEOquery
	stringr
	FSelector
	preprocessCore
	audio
	RWeka
Not installing the above libraries will cause the code to throw exceptions on running. The functions in this code have not been tested for masking against other libraries. Installing and implementing libraries other than those mentioned above may result in strange behavior. 

Running this code in full requires more than 5GB of free space on your hard-drive.  

****************************************************************

PREPARING THE DIRECTORY
- Close all unnecessary programs.

- Unzip the provided folder 'runningFolder' into your working directory. 
Open R-Studio.

- Clear your workspace, run the garbage-collector, and, if necessary, restart your R-Session to reallocate memory to the OS. 

- Set 'runningFolder' (or its renamed equivalent) as your working directory.

- Type source("main.R") into the console, and press ENTER. This will load some functions and variables into the R-Workspace. These are important and necessary for running the program correctly. 

- Type init_workspace() into the console, and press ENTER. This will load the other scripts, placing their functions and a couple fields into the workspace. This also makes sure you have all necessary libraries loaded. It will
throw an exception if you do not have a required library installed. 


RUNNING THE ANALYSIS
- Once you have done all of the above, you have some choices. You may run the fullAnalysis, which takes quite some time, or you may run the analysis on a subset of datasets, on all datasets but only some genes, and finally on some datasets, but only some genes. 

- the 'runFullAnalysis' command is what you'll need to run any of these (even the not-really-full analyses). 

- the 'runFullAnalysis' can take up to four different arguments, like so:
	
	runFullAnalysis(namesOfSets,type="full",subs=0,subType=NULL)
		
		+ 'namesOfSets' is a character vector containing the GEO IDS of all the sets you wish to perform the analysis on. All of the datasets initially intended for analysis are loaded into the queryVector vector, while all of the datasets included in the reported results are included in the queryCF vector. Enter the names of the vectors to see the names included therein. Enter these vectors or any subset of these vectors as the namesOfSets argument to perform the analysis on the corresponding datasets.  
		
		+ 'type' is a character vector of size 1 (like a string, really), which indicates the type of analysis being performed. There are currently three options available for this argument: "full", indicating an analysis of complete datasets, "sub", indicating an analysis of sets subsetted by geneID, and "nopp", which indicates that the user doesn't wish to load datasets from the GEO database and perform the preprocessing steps. If the user enters "nopp" and has never run the code before, did not unzip the folder correctly, or deleted the "PP" folder or its contents at some point, the analysis will not run.
		
		+'subs' indicates the number of genes to include in a subsetted dataset. If "sub" is not entered as the type, this argument will be ignored.
		
		+'subType' is an argument used for testing subsetting successes. By default, it enters at the "groups" string, meaning that the preProcessing function returns a vector which facilitates B-Enrichment and dataset grouping. Entering the "data" string will cause the preProcessing function to return the preprocessed dataset as a data frame. This feature is only available for subsets.

- here are three versions of the command - the first will perform the full analysis on all datasets, the second will perform an analysis on  1000-sized subsets of all datasets, and the third will perform a full analysis on two randomly sampled datasets from the list of available sets:

	1. runFullAnalysis(queryCF)
	2. runFullAnalysis(queryCF,type="sub",subs=1000)
	3. runFullAnalysis(sample(queryCF,2))
	

- let the analysis run in full. It will play a sound upon completion. If it throws an exception, try another type of analysis, or send an email to Bradley.Baker12@NCF.edu for more info.

- try not to run too many programs while the analysis is running. There will be times when the computations get fairly complex. 

- if the computations are getting too complex or the program fills up your memory, restart your R session, reload 'main.R' and type init_workspace(). Try running the analysis on a subset. 


PLOTTING
- The 'resulter.R' script loads commands into the workspace which allow you to do several types of plotting. A readme and automatic runner for plotting will appear in a later version.


UPCOMING FEATURES
- Full B-Enrichment.
- More plotting and results output.
- Plotting of Normalized vs. Non-Normalized Datasets.
- Automatization of plotting.
- Subsetting of Datasets by Sample.
- Re-implementation of abandoned measures and classifiers.
- Cleaner directory creation and travel.
- More in-code documentation.
- Better directory names.
- More clear function and variable names.
- More complete dependency chart, with links to individual Documentation for all functions and variables.
