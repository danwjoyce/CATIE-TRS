# CATIE-TRS
R code for analysing CATIE trial data for treatment resistance

Author: dwj
-----------

Last Updated : 18th April 2018
------------------------------

Status : Not finished (evolving) as code finalised.


# 1. Installation / Directory Structure

From a root directory, set up two sub-directories:

  * Code
  * Data

Execute the project from the "Code" directory (all paths in the scripts are relative to this).  
You may need to change the scripts for the path delimeter (e.g. "/" versus "\\") depending on platform/OS.


# 2. Code

"Code" should contain:

  1. `load_all_tables.R` -- this reads the subset of CATIE "raw" data files in, preparing them as data.frames for the main processing scripts.  See Data Conversion below.
  
  2. `preprocess_trajectories.R` -- does a majority of the processing to establish 'trajectories' of symptoms, treatments and social/occupational functioning (idiosyncratic code that heavily relies on the CATIE data dictionary; contains code that produces the SOFAS / PSP proxy measure from CATIE's scales)
  
  3. `tabulate_cases.R` -- with the output of preprocess_trajectories.R, builds two large tables containing the data required to estimate incidence rates, and a similar table for missing data analyses.

Essentially, 1) and 2) are are data-munging, with 3) computing the relevant tabulated case data that can be used in analyses.

A "walk-through" R notebook `demo_determining_TRS.Rmd` elaborates on the code in c) with an illustrated example for a TRS case.


# 3. Data

This directory should contain the relevant subset of CATIE data tables/files.
The NIMH clinical trials database delivers them as a large ZIP of .txt files, which we individually loaded into LibreOffice, and exported as CSV.


	Root directory / working directory
           |- Data
                 |
                 |- demographic_baseline_meds.csv
                 |- dgsposys01.csv
                 |- doses_of_drugs.csv
                 |- keyvars01.csv
                 |- macvlnce01.csv
                 |- neurobatt01.csv
                 |- panss01.csv
                 |- qol01.csv
                 |- scid_ph01.csv

# 4. Data Conversion

The CATIE data is delivered as either .txt or Excel spreadsheets. Delimeters used in the source data are at times tab, and others comma.  For consistency across the project using R, we decided not to use libraries for reading Excel spreadsheets, which we have had mixed experiences with (sometimes slow, unpredictable across files with the same .xlsx file extensions and finally, the Java dependencies required can be difficult to get working across different OS platforms).  So, our code requires that the 9 CATIE data files are converted to CSV first.

For this, we simply load them into LibreOffice and export as CSV.

**More to follow**

# 5. Data Errors / Integrity Issues

During analyses, we found a clinically incongurent descriptive statistic for medication doses. 
On investigating this, we noted that in the CATIE source file "doses_of_drugs", for the following participants:

  * src_subject_id = 1931 :
    * For columns `phase_ct` = `Phase 3`, `Visit` = `Visit17`
    * Columns/variables : `medad10` = 800 and `medad13` = 5
    *  Which implies Fluphenazine = 800mg and Quetiapine = 5mg
    *  We manually corrected this row, so that medad10 = 5 and medad13 = 800.


  * src_subject_id = 2897 : 
    * For columns `phase_ct` = `Phase 3`, `Visit` = `Visit17` 	
    * Columns/variables : `medad10` = 600 and `medad13` = 10
    * Which implies Fluphenazine = 600 and Clozapine = 10mg
    * We manually corrected this row, so that `medad10` = 10 and `medad13` = 600

  * src_subject_id = 1729 : 
    * For columns `phase_ct` = `Phase 3`, `Visit` = `EndofPhase3`
    * Columns/variables : `medad10` = 300 and `medad13` = 15
    * Which implies Fluphenazine = 300mg, Clozapine = 15mg 300
    * We manually corrected this row, so that `medad10` = 15 and `medad13` = 300mg

These manual edits need to be made to the CATIE source files for consistency with our analyses.

# 6. Dependencies

a) Libraries
These analyses scripts make use of the following R packages, which need to be installed:

	- dplyr
	- reshape2

b) Scripts
The main executable script is :
	- UPDATE

The dependencies are (see source() statements)

	- load_all_tables.R 		: none
	- preprocess_trajectories.R  	: load_all_tables.R
	- tabulate_cases.R		: pre_process_trajectories.R

The example walk-through notebook `demo_determining_TRS.Rmd` depends on `preprocess_trajectories.R`

# 8. Comments

The code in `preprocess_trajectories.R` is idiosyncratic and tightly dependent on the specific structure/data for the CATIE repository (delivered in the NIMH clinical trials download).  Notably, interrogating the 9 CATIE source data files (to derive the relevant data for the TRS project) requires extensive cross-referencing to the `eCRFs.pdf` which acts as a kind of data dictionary for the CATIE data set.


