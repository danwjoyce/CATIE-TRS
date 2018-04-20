# -- dwj : 15th April 2018

# -- Load required tables for CATIE TRS analysis from source CATIE data

# -- root path is assumed current working directory
# -- Modify this for delimeters specific to OS (Linux used throughout)
data.path <- "../Data/"

# -- For each data source, we began by converting the .xlxs file to CSV to avoid 
#    use of Excel interfaces in R, which have been variable in performance and usability and often
#    require Java which can be difficult to get working across Windows / Linux platforms.

# 1 --  Demographic and baseline data --------------------------------------------------------------------------------
#       In CATIE download, this is "demographic_baseline_meds.xlsx" and we 
#       convert simply using LibreOffice Calc to read and then directly export to CSV
all_content = readLines(paste( data.path, "demographic_baseline_meds.csv", sep = ""))
skip_second = all_content[-2]
demog.tab = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = TRUE)


# 2 --  Medication data ----------------------------------------------------------------------------------------------
#      We converted "doses_of_drugs.xlsx" as for baseline/demographics above.
#
#      Later in our analysis, we noted a transcription error, which we corrected:
#         Corrections applied as follows -- 
#           ID = 1931 : Phase 3 Fluphenazine = 5mg, Quetiapine = 800 (transposed in original)
#           ID = 2897 : Phase 3 Fluphenazine = 10, Clozapine should be 400 and 600 (two records, transposed in original)
#           ID = 1729 : Phase 3 Fluphenazine = 15, Clozapine should be 300 (two records, transposed in original)

#     So, the "doses_of_drugs.csv" below represents *our* corrected version of the original data - if re-running this
#     script, ensure the above changes are made to the source CATIE file "doses_of_drugs.xlxs" before converting to CSV
  drug.doses <- read.csv( paste( data.path, "doses_of_drugs.csv", sep = ""), header = TRUE )
  drug.doses <- drug.doses[2:dim( drug.doses )[1], ]      #remove 2nd row of sheet as contains explanatory text

  drug.doses$ID <- as.numeric( as.character( drug.doses$src_subject_id ) )
  
  
# 3 --  PANSS data --------------------------------------------------------------------------------------------------
#       We converted "PANSS.xlsx" to CSV as for the other data above
  all_content = readLines(paste( data.path, "panss01.csv", sep = ""))
  skip_second = all_content[-2]
  temp.PANSS.tab = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = TRUE)
  
  PANSS.tab <- with( temp.PANSS.tab, temp.PANSS.tab[ , c("src_subject_id", "phase_ct", "visday", "truncvis",
                                               "pos_p1","pos_p2","pos_p3","pos_p4","pos_p5","pos_p6","pos_p7",
                                               "neg_n1","neg_n2","neg_n3","neg_n4","neg_n5","neg_n6","neg_n7",
                                               "gps_g1","gps_g2","gps_g3","gps_g4","gps_g5","gps_g6","gps_g7",
                                               "gps_g8","gps_g9","gps_g10","gps_g11","gps_g12","gps_g13",
                                               "gps_g14","gps_g15","gps_g16")
                                          ]
                     )
  
# 4 --  Neurocognitive data --------------------------------------------------------------------------------------------------
#       We converted "neurobatt01.xlsx" to CSV as for the other data above
  all.content    <- readLines(paste( data.path, "neurobatt01.csv", sep = ""))
  skip_second    <- all.content[-2]
  cogtest.tab    <- read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = TRUE)

# 5 --  Quality of Life data -------------------------------------------------------------------------------------------------
#       Here, we converted the "qol01.txt" to CSV via LibreOffice; note this appears to have
#       left the file as tab delimited (?why) but can be read successfully using tab = "\t"
## all_content = read.csv(paste( data.path, "qol01_edited2.csv", sep = "") ,sep = ",", header = TRUE )
  all.content    <- readLines(paste( data.path, "qol01.csv", sep = ""))
  skip_second    <- all.content[-2]
  QoL.temp       <- read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = TRUE, sep = "\t") 
  
  QoL.tab <- with( QoL.temp, QoL.temp[ , c("src_subject_id", "phase_ct", "visday", "truncvis",
                                 "qol01a", "qol01b", "qol01c",         
                                 "qol01d", "qol01e", "qol01f", "qol01g", "qol02",
                                 "qol03", "qol04", "qol05", "qol06", "qol07",         
                                 "qol08", "qol09", "qol10", "qol11", "qol12",             
                                 "qol13", "qol14", "qol15", "qol16", "qol17",             
                                 "qol18", "qol19", "qol20", "qol21", "qol22",             
                                 "qol23a", "qol23b", "qol23c", "qol23d", "qol24a",
                                 "qol24b", "qol24c", "qol24d", "qol25a", "qol25b",            
                                 "qol25c", "qol25d", "qol25e", "qol25f", "qol25g",            
                                 "qol25h", "qol25ia", "qol25ib", "qol25ic", "qol25id",
                                 "qol25ie", "qol25if", "qol25ig", "qol25j1", "qol25j2",            
                                 "qol25j3", "qol25j4", "qol26a", "qol26aa", "qol26b",             
                                 "qol26ba", "qol26c", "qol26ca", "qol26d", "qol26da",            
                                 "qol26e", "qol26ea", "qol27a", "qol27b", "qol27c",             
                                 "qol28a", "qol28b", "qol28c", "qol28d", "qol28e",             
                                 "qol29a", "qol29b", "qol29c", "qol29d", "qol30a",             
                                 "qol30b", "qol30c", "qol30d", "qol31",
                                 "intr_rel", "inst_rol", "intr_fou", "com_obj") ]
  )

# 6 --  MacArthur Violence data ---------------------------------------------------------------------------------------------
#       This is used as part of the PSP/SOFAS aggregate measure we derive later on
  all_content <- read.csv(paste( data.path, "macvlnce01.csv", sep = "") ,sep = ",", header = TRUE )
  viol.tab <- with( all_content, all_content[ , c("src_subject_id", "phase_ct", "visday", "truncvis",
                                                  "mac1a", "mac2a", "mac3a", "mac4a", "mac5a", "mac6a",
                                                  "mac7a", "mac8a", "mac9a", "mac10a", "mac11a",  
                                                  "mac12a", "mac13a", "mac14a", "mac15a", "mac16a", "mac17a",
                                                  "mac18a","mac19a","mac19b","mac19c","mac19d") ]
  )

# 7 --  SCID data at baseline -----------------------------------------------------------------------------------------------
#       As before, converted to CSV via LibreOffice 
  SCID = read.csv(paste( data.path, "scid_ph01.csv", sep = "") ,sep = ",", header = TRUE )
  
  SCID.tab <- with( SCID, SCID[ c("src_subject_id",
                                  "scid03",  ## depression in past 5 years
                                  "scid05",  ## alcohol dependence in past 5 years
                                  "scid07",  ## alcohol abuse in past 5 years
                                  "scid09",  ## drug dependence in past 5 years
                                  "scid11",  ## drug abuse in past 5 years
                                  "scid13",  ## OCD in past 5 years
                                  "scid15"  ## other anxiety disorder past 5 years
                                  ) ] )
  


# 8 --  Baseline PMHx data  ------------------------------------------------------------------------------------------------
#       As before, converted to CSV via LibreOffice 
  pmhx <- read.csv(paste( data.path, "dgsposys01.csv", sep = "") ,sep = ",", header = TRUE )
  pmhx <- pmhx[ which( pmhx$truncvis == 0 ), ]
  pmhx <- with( pmhx, pmhx[ c("src_subject_id", "current_diagnosis", "status") ])
  
  levels( pmhx$current_diagnosis )
  
  # -- create a codified table of PMHx / current dx for each participant to use in analysis
  all.ids <- unique( pmhx$src_subject_id )
  
  pmhx.tab <- data.frame( ID = all.ids, 
                          COPD     = rep(NA, length( all.ids)),
                          DM       = rep(NA, length( all.ids)),
                          HepABC   = rep(NA, length( all.ids)),
                          Lipid    = rep(NA, length( all.ids)),
                          HTN      = rep(NA, length( all.ids)),
                          IHD      = rep(NA, length( all.ids)),
                          OsteoArth= rep(NA, length( all.ids)),
                          Osteopor = rep(NA, length( all.ids)),
                          STI      = rep(NA, length( all.ids))
                        )
  
  for( i in 1:length( pmhx.tab$ID ) ) {
    thisID <- pmhx.tab$ID[i]
    dxs        <- pmhx$current_diagnosis[ which( pmhx$src_subject_id == thisID) ]
    dxs.status <- pmhx$status[ which( pmhx$src_subject_id == thisID) ]
    
    dxs.tab <-  setNames(as.list(dxs.status), dxs)
    
    # because the data is not organised in a consistent fashion, we have to inspect each Dx individually
    pmhx.tab$COPD[i]       <- as.numeric( dxs.tab["Chronic Obstructive Pulmonary Disease"] )
    pmhx.tab$DM[i]         <- as.numeric( dxs.tab["Diabetes (Type I or II)"] )
    pmhx.tab$HepABC[i]     <- as.numeric( dxs.tab["Hepatitis A,B,C"] )
    pmhx.tab$Lipid[i]      <- as.numeric( dxs.tab["Hyperlipidemia"] )
    pmhx.tab$HTN[i]        <- as.numeric( dxs.tab["Hypertension"] )
    pmhx.tab$IHD[i]        <- as.numeric( dxs.tab["Ischemic Heart Disease"] )
    pmhx.tab$OsteoArth[i]  <- as.numeric( dxs.tab["Osteoarthritis"] )
    pmhx.tab$Osteopor[i]   <- as.numeric( dxs.tab["Osteoporosis"] )
    pmhx.tab$STI[i]        <- as.numeric( dxs.tab["Sexually Transmitted Infectious Disease"] )
  }

# 9 -- Keyvars table ------------------------------------------------------------------------------------------
  keyvars <- read.csv(paste0( data.path, "keyvars01.csv" ), header = TRUE, stringsAsFactors = FALSE)

# -- treatment columns to extract from keyvar
  rx.cols <- c("src_subject_id", "treat_1",  "treat_1a", "treat11a",
               "dose_1",   "dose_1a",  "treat_1b", "dose_1b", 
               "treat_2e", "treat_2t", "treat2",
               "dose_2e", "dose_2t",
               "treat_31", "treat_32", "treat_3", "lastphas")
  
  subRx <- data.frame( keyvars[ , rx.cols] )
  subRx$ID <- as.numeric( as.character( subRx$src_subject_id ) )
  subRx$LastPhase <- factor( subRx$lastphas )
  # -- nb : we leave keyvars native table in the environment for later use.
  
# -- Tidy up environment
  rm( temp.PANSS.tab )
  rm( skip_second )
  rm( all_content )
  rm( SCID )
  rm( pmhx )

