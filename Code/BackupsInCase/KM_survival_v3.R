# -- dwj - 15/4/2018 tidied version of preprocessing for CATIE TRS using TRRIP

# -- Depends on :
#       TRS_CATIE_v3.R
#
# -- Produces :
#       1) survivalData_tabulated.csv -- a table of all participants including TRRIP criteria met/not met, times
#       2) missingData_tabulated.csv  -- similar to survivalData_tabulated, but formatteds for missing data analysis
#       3) all_particiapant_IDs.csv   -- a simple list of all participant IDs.

# -- preprocessing steps :
source("TRS_CATIE_v3.R")
require(reshape2)

# -- Workaround to make dealing with different factor levels easier --------------------------
  x <- TRS.sofas.traj$phase_ct
  y <- TRS.sx.traj$phase
  z <- TRS.rx.traj$phase
  allvalues <- unique(union(z,union(x,y)))
  TRS.sofas.traj$phase_ct <- factor(x, levels = allvalues)
  TRS.sx.traj$phase <- factor(y, levels = allvalues)
  TRS.rx.traj$phase <- factor(z, levels = allvalues)
  
  # -- need to rename SOFAS fields
  TRS.sofas.traj$ID <- TRS.sofas.traj$src_subject_id
  TRS.sofas.traj$phase <- TRS.sofas.traj$phase_ct


## -- Build tabulated data for survival analysis ---------------------------------------------
  
# -- IDs for participants missing Sx, SOF and Rx data
missing.Sx <- c() 
missing.SOF <- c()
missing.Rx <- c()

# -- main tabulated data for survival / incidence analysis
tab.Surv <- data.frame( ID = numeric,        # patient ID
                        time.inTrial = numeric, # time spent in trial

                        onset.Sx  = numeric,    # time (day) Sx triggered above threshold
                        onset.SOF = numeric,    # time (day) SOF triggered above threshold
                        onset.Rx  = numeric,    # time (day) >= 2 adequate trial triggered above threshold

                        time.onset.TRS = numeric, # time at which Sx *and* SOF triggered above threshold
                        
                        time.TRS  = numeric, # time *confirmed* TRS (if at all) : some day after onset.XX times
                                             # NB : when adeq Rx > 2, we check the Sx and SOF for response, if none
                                             # we "call TRS" at this time
                        
                        # summary of treatments
                        numAdeq = numeric,   # number of adequate trials
                        durAdeq = numeric,   # duration of adequate trials
                        totalRx = numeric,   # total drug trials (whether adequate or not)

                        # overall flags...
                        status.rx = numeric,     # TRS criteria met on Rx >= 2
                        status.sx = numeric,     # TRS criteria status on symptoms
                        status.sof = numeric,    # TRS status on SOF
                        status.TRS = numeric,    # status : 1 = censored (did not convert on exiting trial)
                                                 #          2 = converted to TRS on exiting trial
                        
                        # TRS domains
                        TRS.pos = numeric,       # resistance in pos domain
                        TRS.neg = numeric,       # resistance in neg domain
                        
                        # data completely missing
                        missing.Sx = numeric,
                        missing.Rx = numeric,
                        missing.SOF = numeric
                        
)

allIDs <- unique( demog.tab$src_subject_id )

# -- make warnings errors that halt execution for debugging
options(warn = 2)

# -- Some helper functions to make the following big loop easier to read ------------------
  CreateNewRow <- function() {
    return( 
      data.frame( 
        ID = thisID,
        time.inTrial = keyvars$es_day[ which( keyvars$src_subject_id == thisID ) ],    # use end of study day as default
        
        onset.Sx  = NA,
        onset.SOF = NA,
        onset.Rx  = NA,
        
        time.onset.TRS = NA,
        
        time.TRS  = NA,
        
        numAdeq = NA,
        durAdeq = NA,
        totalRx = NA,
        
        status.rx = NA,
        status.sx = NA,
        status.sof = NA,
        status.TRS = NA,
        
        TRS.pos = 0,       # resistance in pos domain
        TRS.neg = 0,       # resistance in neg domain
        
        missing.Sx = 0,
        missing.Rx = 0,
        missing.SOF = 0
      )
    )
  }

# -- Function to check if a given participant is missing Rx, Sx and SOF data (to the extent we cannot)
#    robustly process the participant further
CheckZeroRecords <- function( thisRow, thisRx, thisSx, thisSOF ) {
  # -- first, catch case where patient drops out with no real data
  # -- NB : assumes missing.Rx, missing.Sx and missing.SOF are in global environment.
  
  # -- we do this by looking at Rx trajectory : 
  if ( nrow( thisRx ) < 1 )  { ## no treatments recorded at all
    thisRow$status.rx <- NA
    thisRow$totalRx   <- NA
    noFurtherFlag <- 1    ## tell rest of loop to not bother
    missing.Rx <- c( missing.Rx, thisID )
    thisRow$missing.Rx <- 1
  }
  
  # -- second, catch situation where Sx is missing - so we cannot assess response to treatment
  if ( nrow( thisSx ) < 1 ) {
    thisRow$status.sx <- NA
    noFurtherFlag <- 1    ## tell rest of loop to not bother
    missing.Sx <- c( missing.Sx, thisID )
    thisRow$missing.Sx <- 1
  }
  
  # -- third, catch situation where SOF is missing - so we cannot assess social/occ functioning
  if ( nrow( thisSOF ) < 2 ) {
    thisRow$status.sof <- NA
    missing.SOF <- c( missing.SOF, thisID )
    thisRow$missing.SOF <- 1
  }
  
  return( list( thisRow, noFurtherFlag ) )
  
}

# -- This is fairly ugly code which is difficult to follow.
# -- The best way is to manually set the loop counter and trace through lines.
#
# -- Now track each participant ------------------------------------------------------------------------------------------
  for ( i in 1:length( allIDs ) ) {
    
    # -- for this participant ...
    thisID <- allIDs[i]
    
    # -- fetch Sx, SOF and Rx (stored in TRS.sx.traj, TRS.sofas.traj, and TRS.rx.traj) 
    # -- all output from "TRS_CATIE_v3.R"
    thisSx  <- TRS.sx.traj[ which( TRS.sx.traj$ID == thisID ), ]
    thisSOF <- TRS.sofas.traj[ which( TRS.sofas.traj$ID == thisID ), ]
    thisRx  <- TRS.rx.traj[ which( TRS.rx.traj$ID == thisID ), ]
  
    noFurtherFlag <- 0
  
    # -- build a blank data.frame to store this participant
    thisRow <- CreateNewRow()

    # -- check for "zero" records for this participant
    check.zero    <- CheckZeroRecords( thisRow, thisRx, thisSx, thisSOF )
    noFurtherFlag <- check.zero[[2]]
    thisRow       <- check.zero[[1]]

    # -- if we have treatment (Rx) data, then process Sx trajectory
    if ( noFurtherFlag == 0 ) {
      
      # -- there is at least one participant who's last.visit.day in thisRx is later than the thisSx.visday
      #    and thisSOF.visday : capture this, and set last.visit.day equal to the last day of thisSx.visday
      if ( thisRx$last.visit.day[ nrow(thisRx) ] > thisSx$visday[nrow(thisSx)] ) {
        thisRx$last.visit.day[ nrow(thisRx) ] <- thisSx$visday[nrow(thisSx)]
      }
      
      # -- order SOF by visday
      thisSOF <- thisSOF[ order( thisSOF$visday ), ]
      # -- remove duplicated SOF rows
      thisSOF <- thisSOF[ !duplicated( thisSOF ), ]
      
      # -- Same for symptoms 
      thisSx  <- thisSx[ order( thisSx$visday ), ]
      thisSx  <- thisSx[ !duplicated( thisSx ), ]
      # -- and treatments (for which there are never any duplicates)
      thisRx  <- thisRx[ order( thisRx$last.visit.day ), ]
     
      # -- join a subset of columns in thisSx and thisSOF with thisRx to compile TRS status 
      temp <- left_join( thisSx, 
                         thisSOF[ , names(thisSOF) %in% c("visday","TRS.sofas.traj") ],
                         by = "visday" )  
      
      temp <- left_join( temp, thisRx[ names(thisRx) %in% c("rxDuration","adeqTrial","cumAdeqRx","cumRx","last.visit.day") ],
                         by = c("visday" = "last.visit.day") )
      
      # --------------- time at which participant *first* triggers thresholds for SOF and Sx and Rx
      temp$onsetSxFlag <- ifelse(   ( temp$TRS.pos == 1 |
                                      temp$TRS.neg == 1 ), 1, 0
      )
      
      # -- flag if the participant reached *BOTH* Sx and SOF criteria
      temp$SOF.and.Sx.TRS <- ifelse( temp$onsetSxFlag== 1 & temp$TRS.sofas.traj == 1, 1, 0 )
      
      # -- filter on cumAdeqRx
      temp2 <- temp[ which( !is.na( temp$cumAdeqRx ) ), ]
      
      # -- Store number of adequate trials, duration of adequate trials and total treatments
      numAdeq <- max( thisRx$cumAdeqRx )
      durAdeq <- sum( thisRx$rxDuration[ which( thisRx$adeqTrial == 1 ) ] )
      totalRx <- max( thisRx$cumRx )
      
      # flag if ever meets SOF and Sx criteria together
      idxEver <- which( temp2$SOF.and.Sx.TRS == 1)
      # -- idxEver : contains rows idxs for when Sx + SOF > threshold
      
      # -- if NEVER has Sx and SOF criteria together then set variables to record non-TRS ...
      if( length( idxEver ) == 0 ) {
                
                idxFirstSx  <- which( temp$onsetSxFlag == 1 )
                idxFirstSOF <- which( temp$TRS.sofas.traj == 1 )
                idxFirstRx  <- which( thisRx$adeqTrial == 1 )
                if( length( idxFirstSx ) == 0 ) {
                  onset.Sx <- NA
                } else {
                  onset.Sx  <- temp$visday[ min( idxFirstSx ) ]
                }
                
                if( length( idxFirstSOF ) == 0 ) {
                  onset.SOF <- NA
                } else {
                  onset.SOF <- temp$visday[ min( idxFirstSOF ) ]
                }
        
                # -- as never reaches criteria for Rx >= 2        
                onset.Rx <- NA
                
                time.onset.TRS <- NA
                time.TRS       <- NA
                
                status.rx  <- NA
                status.sx  <- NA
                status.sof <- NA
                status.TRS <- 1    # censored - never met TRS criteria
                
                TRS.pos = 0        # obviously, cannot meet resistance in pos domain
                TRS.neg = 0        # or in neg domain
      }
      
      # -- if this participant DOES meet Sx and SOF criteria together ... establish TRS status
      if ( length( idxEver ) > 0 ) {
        # -- now, ascertain the BASELINE for PANSS change, then when adeqRx >= 2 and

        # -- compute time at which Sx and SOF were "at risk" -- i.e. above threshold, but NOT YET had adequate treatment
        onset.Sx  <- temp$visday[ min( which( temp$onsetSxFlag == 1 ) ) ]
        onset.SOF <- temp$visday[ min( which( temp$TRS.sofas.traj == 1 ) ) ]
        
        # -- and then time at which adequate trials passes threshold >= 2
        onset.rx.idx <- which( temp$cumAdeqRx >= 2 )
        if ( length( onset.rx.idx ) == 0 ) { 
          # -- in this case, participant NEVER had >= 2 adequate trials
          onset.Rx  <- NA
        } else {
          # -- otherwise, store time and which they completed >= 2 adequate trials
          onset.Rx  <- temp$visday[ min( onset.rx.idx ) ]        
        }
        
        # -- compute time at which Sx and SOF were at risk together (but not necessarily the Adequate Rx criteria met)
        time.onset.TRS <- temp$visday[ min( which( temp$SOF.and.Sx.TRS == 1 ) ) ]
        
        # -- extract the PANSS scores at time.onset.TRS : this will be the baseline for symptom change measurement
        baselinePANSS <- temp[ which( temp$visday == time.onset.TRS ), c("visday", "adjPos", "adjNeg", "adjGen", "adjTot") ]
        
        # -- extract the PANSS scores for followup 
        # -- this will be after 0, 1 or 2 adequate trials and will be the follow-up time for symptom change measurement
        
        # -- never had 2 adequate trials, so we cannot record they were TRS
        if ( is.na( onset.Rx ) ) {
          # -- but we still require some measurement of symptom change
          # -- if they did not have 2 adequate trials, but numAdeq > 0 ==> after one adequate trial, we record PANSS for response
          if ( numAdeq > 0 ) {
            followupPANSS <- temp[ min( which( temp$cumAdeqRx == 1 ) ), c("visday", "adjPos", "adjNeg", "adjGen", "adjTot") ]
          } else 
            # -- NEVER had even one adequate trial, so use end of trial
            # -- find last day with PANSS scores
            {
              tempPANSS <- temp[
                                  complete.cases( temp[ , c("visday", "adjPos", "adjNeg", "adjGen", "adjTot") ] ),
                                  c("visday", "adjPos", "adjNeg", "adjGen", "adjTot")
                                ]
              followupPANSS <- tempPANSS[ nrow( tempPANSS ), ]
            }
        } else 
          # -- Had TWO full adequate trials, and therefore
          # -- use onset.Rx day as the follow up AFTER 2 adequate trials
          { 
            followupPANSS <- temp[ which( temp$visday == onset.Rx ), c("visday", "adjPos", "adjNeg", "adjGen", "adjTot") ]
          }
        
        # -- just incase, clear up duplicate panss rows
        followupPANSS <- followupPANSS[ !duplicated( followupPANSS ), ]
        baselinePANSS <- baselinePANSS[ !duplicated( baselinePANSS ), ]
        
        # -- percentage change in symptoms : Leucht et al, 2009
        deltaPANSS <- 100 * ( baselinePANSS[ c("adjPos", "adjNeg", "adjTot") ] - followupPANSS[ c("adjPos", "adjNeg", "adjTot") ] ) /
                                followupPANSS[ c("adjPos", "adjNeg", "adjTot") ]
        
        # -- Now, if deltaPANSS >= 20% ==> improvement threshold, therefore NOT TRS on that PANSS domain
        TRS.change.PANSS.criteria <- ifelse( deltaPANSS >= 20, 0, 1 )
        
        # -- Record time at which SOF, absolute Sx and cumAdeqRx >= 2
        idxTimeTRS     <- which( temp2$SOF.and.Sx.TRS == 1 & temp2$cumAdeqRx >= 2 )
        if ( length( idxTimeTRS ) == 0 ) 
          # -- never became TRS
          { 
            time.TRS       <- NA
          } else 
          # -- became TRS
          {
            time.TRS       <- temp2$visday[ min( idxTimeTRS  ) ]
          }
        
        # -- record IF EVER met Rx >= 2 criteria
        status.rx  <- ifelse( !is.na( onset.Rx ) & onset.Rx  > 0, 1, 0 )
        
        # -- meets change criteria ? 1 => the < 20% change on total *and* one domain 
        status.sx <- ifelse( 
                                ( TRS.change.PANSS.criteria[3] == 1 ) &   # PANSS total change < 20%
                                ( TRS.change.PANSS.criteria[1] == 1 | TRS.change.PANSS.criteria[2] == 1 ),   # either Pos or Neg domains are resistant
                                1, 0 )
        
        # -- absolute criteria threshold STILL met at followup time ONLY if time.TRS is *not* NA
        # -- So, check that time.TRS is not NA (==> simultaneously met absolute Sx and Rx conditions)
        # -- AND that the change in symptoms was actually < 20% -- status.sx.delta
        if( !is.na( time.TRS ) & status.sx == 1 ) {
          # -- the SOF + absolute Sx + Rx criteria were ALL met 
          status.TRS <- 2
        } else {
          # -- participant is treatment response / censored
          status.TRS <- 1
        }
          
        status.sof <- ifelse( !is.na( onset.SOF ) & onset.SOF > 0, 1, 0 )
        TRS.pos = TRS.change.PANSS.criteria[1]       # resistance in pos domain 1,0
        TRS.neg = TRS.change.PANSS.criteria[2]       # resistance in neg domain 1,0
        
      }
  
      # -- complete thisRow and store
      thisRow$onset.Sx    <- onset.Sx
      thisRow$onset.SOF   <- onset.SOF
      thisRow$onset.Rx    <- onset.Rx
      
      thisRow$time.onset.TRS <- time.onset.TRS
      thisRow$time.TRS       <- time.TRS
      thisRow$status.TRS     <- status.TRS
      
      thisRow$numAdeq     <- numAdeq
      thisRow$durAdeq     <- durAdeq
      
      thisRow$totalRx     <- totalRx
      thisRow$status.rx   <- status.rx
      thisRow$status.sx   <- status.sx
      thisRow$status.sof  <- status.sof
      thisRow$TRS.pos     <- TRS.pos
      thisRow$TRS.neg     <- TRS.neg
  
    }
    
    tab.Surv <- rbind( tab.Surv, thisRow )
    
  }
  
  options(warn = 0)
  
# -- for ease of use/presentation, convert event time to years
tab.Surv$time.inTrialYrs <- tab.Surv$time.inTrial / 365
tab.Surv$time.onset.TRSYrs  <- tab.Surv$time.onset.TRS / 365
tab.Surv$time.TRSYrs <- tab.Surv$time.TRS / 365

# -------------------------------------------- Finished processing trajectories through trial for all participants.

# -------------------------------------
# Descriptive numbers on TRS cases
# -------------------------------------
cat(
  paste("Full TRS Cases  = ", length( which( tab.Surv$status.TRS == 2 ) ) ),
  paste("Cases meeting SOF and Sx at follow up, but who had ONLY 1 adequate trials = ", 
        length( which( tab.Surv$status.sx == 1 & tab.Surv$status.sof == 1 & tab.Surv$numAdeq == 1 ) ) ),
  paste("Cases meeting SOF and Sx at follow up, but who had ZERO adequate trials = ", 
        length( which( tab.Surv$status.sx == 1 & tab.Surv$status.sof == 1 & tab.Surv$numAdeq == 0 ) ) ),
  sep = "\n"
)


# ------------------------------------------------------------------------------------------------------------------
# -- Fetch and collate demographics and baseline data necessary to reproduce the baseline analysis used in descriptives 
#    from original CATIE paper -- Lieberman (2005) NEJM -- reproducing table 1 pp. 1213 
  
demog.keep.vars <- c("src_subject_id",
                     "interview_age",
                     "gender",
                     "race",       # white or black / african american
                     "hispanic",   # Hispanic, latino or Spanich origin
                     "educ_yrs",   # educationin years
                     "das1ms",     # martical status
                     "employ2",    # fulltime employment or not (1 = yes, 0 = no)
                     "exacer",     # exacerbation / crisis stabilisation / admission in past 3 months
                     
                     # we have already collected PANSS so don't reproduce here
                     
                     "cgi_sev",    # clinical global impression score, 4 = moderately ill, 7 = most ill, 1 = not ill at all
                     
                     # pscyhiatric hx
                     "yrs_trt",    # years since first treated for scid17a emotional/behav problem
                     "yrs_pres",   # years since first Rx with antipsychotic
                     
                     ## SCID diagnosis in past 5 yrs -- sourced elsewhere
                     
                     ## Baseline antipsychotic meds
                     "olz_0",
                     "quet_0",   ## baseline meds
                     "risp_0",
                     "zip_0",
                     "hal_0",
                     "deca_0",
                     "per_0",
                     "othr_0"
) 

# -- retain demographics we need for analysis / missing data
demog.X <- demog.tab[, names(demog.tab) %in% demog.keep.vars ]
# -- strip duplicates
demog.X <- demog.X[ !duplicated( demog.X), ]

# -- recode variables in demog.X for comptibility with Table 1, NEJM 2005 paper 
demog.X$gender  <- ifelse( demog.X$gender == "M", 1, 0 )    # 1 == Male, 0 == female

# -- `race` is more complex : code for White, Black, Other and then hispanic / Latino / spanish ethnicity separately
  demog.X$raceCoded <- rep("Other",nrow(demog.X))
  demog.X$raceCoded[ which( demog.X$race == "Black or African American") ] <- "Black"
  demog.X$raceCoded[ which( demog.X$race == "White") ] <- "White"
  demog.X$raceCoded[ which( demog.X$hispanic == 1) ] <- "Hispanic"
  demog.X$raceCoded <- as.factor( demog.X$raceCoded )
  demog.X <- demog.X[ , !( names( demog.X ) %in% c("race","hispanic") ) ]

# -- marital status
  demog.X$marital <- rep(rep("Other",nrow(demog.X)))
  demog.X$marital[ which( demog.X$das1ms == "Married" ) ] <- "Married"
  demog.X$marital[ which( demog.X$das1ms == "Never married" ) ] <- "NeverMarried"
  demog.X$marital[ which( demog.X$das1ms == "Separated" ) ] <- "PrevMarried"
  demog.X$marital[ which( demog.X$das1ms == "Widowed" ) ] <- "PrevMarried"
  demog.X$marital[ which( demog.X$das1ms == "Divorced" ) ] <- "PrevMarried"
  demog.X$marital <- as.factor( demog.X$marital )
  demog.X <- demog.X[ , !( names( demog.X ) %in% c("das1ms") ) ]
  
# -- get baseline PANSS data
panss.X <- PANSS.tab[ PANSS.tab$visday == 1, ]
# -- strip duplicates
panss.X <- panss.X[ !duplicated( panss.X), ]

# -- compute total domains P, N and G : for convenience 
panss.PNG <- data.frame( src_subject_id = panss.X$src_subject_id,
                         P  = rowSums( panss.X[ , 5:11 ] ),
                         N  = rowSums( panss.X[ , 12:18 ] ),
                         G  = rowSums( panss.X[ , 19:34 ] )
)

# -- join panss and demographics at baseline
res <- inner_join(panss.PNG, demog.X)

# -- next join PMHx-diagnoses (see original NEJM paper 2005 - where these variables where used in descriptives)
res <- inner_join(res, pmhx.tab, by = c("src_subject_id" = "ID" ) )

# -- next, join with SCID dx
res <- inner_join(res, SCID.tab, by = c("src_subject_id" = "src_subject_id" ) )

# now join with global tab.Surv
res <- inner_join(res, tab.Surv, by = c("src_subject_id" = "ID"))

# -- and tidy up
tab.Surv <- res
rm( res )

# -- rename ID column for convenience
names(tab.Surv)[names(tab.Surv) == 'src_subject_id'] <- 'ID'

# -- remove duplicated rows due to > 1 entry for baseline (only one case, ID = 2780 from visual inspection)
tab.Surv <- tab.Surv[ !duplicated( tab.Surv$ID ), ]

# -- some housekeeping : tidy up data.frame col names to be meaningful
newNames <- c("ID", "P", "N", "G", "age", "sex", 
              "exac3mo", "yrsEduc", "CGIsev", "employFT", "yrsFirstTx", "yrsFrstAntiPsyRx",
              "olzB0", "quetB0", "rispB0", "zipB0", "halB0", "decaB0",
              "perB0", "otherB0", "race", "marital", "COPD", "DM",
              "HepABC", "Lipid", "HTN", "IHD", "OsteoArth", "Osteopor",
              "STI", "Depression", "alcDep_5yrs", "alcAbuse_5yrs", "drugDep_5yrs",  "drugAbuse_5yrs",              
              "OCD_5yrs", "anxDis_5yrs", 
              
              "time.inTrial", "onset.Sx",
              "onset.SOF", "onset.Rx", "time.onset.TRS", "time.TRS", "numAdeq",             
              "durAdeq", "totalRx", "status.rx", "status.sx", "status.sof",          
              "status.TRS", "TRS.pos", "TRS.neg", "missing.Sx", "missing.Rx",
              "missing.SOF", "time.inTrialYrs", "time.onset.TRSYrs", "time.TRSYrs")
            
names( tab.Surv ) <- newNames

# -- variables to REMOVE for predicting missing-ness at follow up
removePreds <- c("ID", "onset.Sx", "onset.SOF", "onset.Rx",
                 "time.onset.TRS", "time.TRS", "numAdeq", 
                 "durAdeq", "totalRx", "status.rx",
                 "status.sx", "status.sof", "status.TRS",
                 "TRS.pos", "TRS.neg", "time.inTrial", "time.onset.TRSYrs", "time.TRSYrs",
                 "missing.Sx", "missing.Rx", "missing.SOF")

# -- missing data table : we get rid of removePreds
missingTab <- tab.Surv[ , which( !( names( tab.Surv ) %in% removePreds ) ) ]
allIDs <- tab.Surv$ID

# -- add in missing flags to use later
missingTab$missingFU.rx      <- tab.Surv$missing.Rx
missingTab$missingFU.sx      <- tab.Surv$missing.Sx
missingTab$missingFU.sof     <- tab.Surv$missing.SOF
missingTab$missingFU.any     <- ifelse( 
                                          ( missingTab$missingFU.rx == 1 |
                                            missingTab$missingFU.sx == 1 |
                                            missingTab$missingFU.sof == 1 ), 1, 0 )

# -- store missingTab (for missing data analyses)
# -- tab.Surv (for survival analyses, and more)
# -- allIDs (just a vector off all participant IDs)
save( missingTab, tab.Surv, allIDs, file = paste0( data.path, "survival_tabulated_data_v3.RData") )

# -- save in non-proprietary format
write.csv( missingTab, file = paste0( data.path, "missingData_tabulated_v3.csv"), row.names = FALSE )
write.csv( tab.Surv,   file = paste0( data.path, "survivalData_tabulated_v3.csv"), row.names = FALSE )
write.csv( allIDs,     file = paste0( data.path, "all_participant_IDs.csv"), row.names = FALSE)
