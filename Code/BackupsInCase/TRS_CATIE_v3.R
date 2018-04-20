# --  dwj : 15th April 2018
# --  Cleaned version of initial processing script, annotated
#
# --  Depends on:
#         load_all_tables.R

# --  Directory structure required:
#         Root
#           |
#           |- Code
#                 |
#                 |- load_all_tables.R
#           |- Data
#                 |
#                 |- demographic_baseline_meds.csv
#                 |- dgsposys01.csv
#                 |- doses_of_drugs.csv
#                 |- keyvars01.csv
#                 |- macvlnce01.csv
#                 |- neurobatt01.csv
#                 |- PANSS.csv
#                 |- qol01.csv
#                 |- scid_ph01.csv

# -- This script assumes the current working directory is Root/Code.

# -- Outline of the TRRIP concensus (Howes et al, 2016) --------------------------------------
#
# -- 1: Current symptoms -- 
#     a) BPRS, PANSS etc. at least Moderate Severity
#     b) Duration : >= 12 weeks
#     d) subjective distress - NOT required
#     c) Functioning - moderate impairment on validated scale SOFAS or CGI??
#
# -- At least moderate (>= 4) in two symptoms in each POS, NEG and GEN domains
#    OR at least one severe >= 6.
#
# -- So, treatment resistance positive = 2 sx at 4+, or 1 symptom at 6+
#    and a patient can be treatment resistant in more than one domain (POS, NEG and GEN)

# -- At CATIE baseline, first episode patients were excluded, so included patients have symptoms present > 3 years and had not
#    started antipsychotic treatment in the preceding 12 months (i.e. had been treated for longer)

# -- Setup environment
  rm(list = ls())
  # -- Paths (assumes working directory is root)
  root.path <- "./"
  data.path <- "../Data/"
  setwd( root.path )
  source( paste( root.path, "load_all_tables.R", sep = "") )

  require(plyr)
  require(dplyr)

# -- 1 : Social and occupational functioning (SOFAS/PSP proxy score) 
#        Here, we define the four "domains" in the PSP version of SOFAS
#        in terms of the variables available in CATIE's multiple instruments
  
  A.social_act.func.QoL.tab <- c("inst_rol")   # lower scores worse 
  # -- we take SOFAS socially useful activity/study as the CATIE QoL instrumental role variable

  B.pers.social.QoL.tab     <- c("intr_rel")
  # -- SOFAS personal and social activity as the CATIE QoL interpersonal relations variable

  C.self_care.QoL.tab       <- c("qol24a", ## Wash two or more times daily 0 = NO, 1 = YES, 2 = No opport.
                                 "qol24b", ## Clean room (score as above)
                                 "qol24c", ## Do own laundry (score as above)
                                 "qol24d", ## help to do chores (score as above)
                                 "qol25j1", ## remind for appts
                                 "qol25j2", ## take to appts
                                 "qol25j3", ## remind for meds
                                 "qol25j4", ## give meds
                                 "qol26a", ## independent living = 1
                                 "qol26b", ## minimally supported accom = 1
                                 "qol26c", ## moderately supported accom = 1
                                 "qol26d", ## extremely restrictive accom = 1
                                 "qol26e"  ## homeless = 1
                               )
  # -- self care (in SOFAS/PSP) is an aggregate of a number of variables in the CATIE instruments
  
  # -- for disturbed/aggressive behaviour in SOFAS/PSP we use the MacArthur items:  
  D.disturb.aggress.viol.tab <- c("mac2a", "mac4a", "mac6a", "mac8a", "mac10a","mac12a","mac14a","mac16a","mac17a",        
                                 "mac18a" )
  # -- MacArthur tool : sum of only macxx items recording *patient* directing aggression outwards (rather than 
  #    victim or target of aggression ), removing 97 and 98 entries.

# -- Now code the domains for pseudo-SOFAS
  id.cols <- c("src_subject_id", "phase_ct", "visday", "truncvis")
  temp.SOFAS <- QoL.tab[ , c(id.cols, A.social_act.func.QoL.tab, B.pers.social.QoL.tab, C.self_care.QoL.tab )  ] 
  temp.viol  <- viol.tab[ , c(id.cols, D.disturb.aggress.viol.tab ) ]

# -- Join temp.SOFAS and temp.viol
SOFAS <- left_join( temp.SOFAS, temp.viol )


# -- Aggressive / Disturbed behaviour -- Domain D for SOFAS / PSP
  # -- we use the sum of the MacArthur Abbreviated Community Aggression
  #    where a 1 indicates an act of aggression in any of the categories.
  #    the sum is a proxy for aggression across all categories - total score = 0 ... 10
  t.sofas <- SOFAS[ , D.disturb.aggress.viol.tab ]
  t.sofas[ t.sofas > 1 ] <- 0    
  # -- we assume codes 97 and 98 (indicating refused or forgotten) are 0 so at worst, we are likely underestimating magnitude
  sum.viol <- rowSums( t.sofas, na.rm = TRUE )
  SOFAS$D.aggress <- sum.viol

# -- Socially useful Activity and/or Study -- Domain A for SOFAS/PSP
  # -- we use the Instrumental Role subset of Heinrich QOL (1984)
  #    Scoring (from Heinrich et al 1984) : 
  #      * 6-5 == excellent / little impairment
  #      * we use < 3 as moderate (e.g. 2.9 would be moderate, 4.9 - 4.3 would be mild)
  #      * 1-0 == severe impairment
  SOFAS$A.social <- SOFAS[ , c("inst_rol")]

# -- Personal and Social function -- Domain B for SOFAS/PSP
  # -- We use the Interpersonal Relations subset of Heinrich QOL
  #    Scores as above for Activity/Study Domain A
  SOFAS$B.personal <- SOFAS[ , c("intr_rel")]

# -- Compute the self-care element : this is tougher because it's not cleanly captured by any of the Heinrich QOL scores in CATIE 
  # -- Needs to be assembled (rather arbitratily) as follows :
  t.self <- SOFAS[ , C.self_care.QoL.tab ]
  # -- split into three areas :
  t.home <- t.self[,c("qol26a", "qol26b", "qol26c", "qol26d", "qol26e")]  # home status (independent, minimal restric, moderate restric, extreme restrict, homeless)
  t.care <- t.self[,c("qol24a", "qol24b", "qol24c", "qol24d")]            # self care
  t.indep<- t.self[,c("qol25j1", "qol25j2", "qol25j3", "qol25j4")]        # independence - needing help with reminders / taking to appts and reminders/ taking meds

# -- for the home/independence status (in past 30 days), take the worst (rightmost) column as the estimate.
  C.home <- rep(0, NROW( t.home ))
  for( i in 1:NROW( t.home ) ) {
    C.home[i] <- 6 - max( t.home[i,] * c(1,2,3,4,5) )
  }
  # -- The resulting score C.home is as follows:
  #     : 5 -- best (fully independent)
  #     : 4 -- minimally supported accom
  #     : 3 -- moderate supported accom
  #     : 2 -- extremely restricted accom
  #     : 1 -- homeless
  #  NB : 6 ==> no data entered

# -- for self care, 0 = No (poor), 1 = yes (good) and 2 = no opportunity (suggesting worse independent function)
  C.selfcare <- rep(0, NROW( t.care ))
  for( i in 1:NROW( t.care ) ) {
    C.selfcare[i] <- length( which( t.care[i,] == 1) )
  }
  # -- The resulting score for C.selfcare is then as follows:
  #     : 4 -- best (wash, laundry, clean room, do chores )
  #     : 3 -- 3 of the above
  #     : 2 -- manages only 2 of above
  #     : 1 -- managed only 1 of above
  #     : 0 -- very poor, manages NONE of above

# -- For independence, count number of 1's in row of 4 x 0/1 
#     1 => Yes, (worse function) needs help, 
#     0 => no does not need help (better function)
  C.indep <- rep(0, NROW( t.indep ))
  for( i in 1:NROW( t.indep ) ) {
    C.indep[i] <- 4 - length( which( t.indep[i,] == 1) )
  }
  # -- The resulting score for C.indep is then: 
  #     : 4 -- best : fully independent
  #     : 0 -- worst : requires help in all domains

# -- Arrange scores in same rank order (best - worst)
SOFAS$C.accom <- C.home            # 5 = best, 0 = worst, 6 = no data
SOFAS$C.selfcare <- C.selfcare     # 4 = best, 0 = worst
SOFAS$C.indep    <- C.indep        # 4 = best, 0 = worst


# -- PANSS scores and tresholds -------------------------------------------------------------------------

# -- define criteria as a function
  aboveThresh <- function( V ) {
    four <- length( which( V >= 4) )
    six  <- length( which( V >= 6 ) )
    if( four >= 2 | six >= 1 ) {
      ## meets threshold
      return( 1 )
    } else {
      ## does not meet threshold
      return( 0 )
    }
  }

  panss_TRS <- function( P ) {
    # -- takes a table of PANSS scores (cols = P1, P2, ... N1, N2, ... G1, G2, .... G16 and rows = participants)
    #    (which represent ALL PANSS ratings over the entire trial for every participant)
    #    and computes a TRS *absolute* symptom inclusion profile using aboveThresh()
    #    NOTE : absolute column references used - so the table MUST be in P, N and G format
    #    with cols 1 ... 7 = Positive 1-7, 8 ... 14 = Negative and 15 ... 30 = general
    
    ## 1) positive symptoms
    P.pos          <- data.matrix( P[ , 1:7] )
    TRS.POS        <- as.numeric( apply( P.pos, MARGIN = 1, FUN = aboveThresh ) )
    total.POS      <- as.numeric( apply( P.pos, MARGIN = 1, FUN = sum, na.rm = TRUE ) )
    adj.total.POS  <- total.POS - 7
    ## note adj.total.POS is baseline 7 adjusted to enable computation of change later
    
    P.neg          <- data.matrix( P[ , 8:14] )
    TRS.NEG        <- as.numeric( apply( P.neg, MARGIN = 1, FUN = aboveThresh ) )
    total.NEG      <- as.numeric( apply( P.neg, MARGIN = 1, FUN = sum, na.rm = TRUE ) )
    adj.total.NEG  <- total.NEG - 7
    
    P.gen          <- data.matrix( P[ , 15:30] )
    TRS.GEN        <- as.numeric( apply( P.gen, MARGIN = 1, FUN = aboveThresh ) ) 
    total.GEN      <- as.numeric( apply( P.gen, MARGIN = 1, FUN = sum, na.rm = TRUE ) ) 
    adj.total.GEN  <- total.GEN - 16
      
    ## return a data.frame
    return( data.frame( TRS.pos = TRS.POS, total.POS = total.POS, adj.total.POS = adj.total.POS,
                        TRS.neg = TRS.NEG, total.NEG = total.NEG, adj.total.NEG = adj.total.NEG, 
                        TRS.gen = TRS.GEN, total.GEN = total.GEN, adj.total.GEN = adj.total.GEN ))
  }
  
  panss_5Factor <- function( P ) {
    # -- takes a table of PANSS scores (cols = P1, P2, ... N1, N2, ... G1, G2, .... G16 and rows = participants)
    #    and computes the Five Factor model for each participant.  Legacy code.
    five.factor <- list( neg = c("neg_n3","neg_n6","neg_n2","neg_n4","neg_n1","gps_g7"), 
                         disorg = c("pos_p2","gps_g11","gps_g10","gps_g13","neg_n5","neg_n7","gps_g5"),
                         excite = c("gps_g8","gps_g14","pos_p7","pos_p4"),
                         pos = c("pos_p1","gps_g9","pos_p3","pos_p6","pos_p5"),
                         depanx = c("gps_g2","gps_g3","gps_g6","gps_g4") 
    )
    
    # compute domain scores without adjustment
    P.neg          <- rowSums( P[ , five.factor$neg ] )
    P.disorg       <- rowSums( P[ , five.factor$disorg ] )
    P.excite       <- rowSums( P[ , five.factor$excite ] )
    P.pos          <- rowSums( P[ , five.factor$pos ] )
    P.depanx       <- rowSums( P[ , five.factor$depanx ] )
    
    ## return a data.frame
    return( data.frame( ID = P$src_subject_id, phase = P$phase_ct, visday = P$visday, truncvis = P$truncvis,
                             F5.pos = P.pos, F5.neg = P.neg, F5.disorg = P.disorg, F5.excite = P.excite, F5.depanx = P.depanx))
  }
  
  # -- convert phases to a factor
  PANSS.tab$phase_ct <- factor(PANSS.tab$phase_ct, levels(PANSS.tab$phase_ct)[c(6,1:5)])
  trialPhases <- levels( PANSS.tab$phase_ct )
  
  # -- Numerical code for phase labels:
  numeric.trialPhases <- c("Pre-Rand" = 1, "Phase 1/1A" = 2, "Phase 1B" = 2,  "Phase 2" = 3, "Phase 3" = 4, "Phase 4" = 4)
  
  # -- Assemble a data frame of PANSS absolute inclusion crtieria over every participant at each recorded assessment
  TRS.sx <- panss_TRS( PANSS.tab[,5:NCOL(PANSS.tab)] )
  TRS.sx <- cbind( PANSS.tab[ , 1:4], TRS.sx )
  
  PANSS.F5 <- panss_5Factor( PANSS.tab )

# -- Functional impairment --------------------------------------------------------------------------------------------
#     This represents a compromise given no SOFAS / PSP in CATIE
#     The PSP operationalises the SOFAS by specifying four areas (A-D) and then placing patients
#     on a SOFAS-like 0-100 scale based on meeting criteria in A through D (See Morosini et al 2000, Acta Psychiatrica Scandinavica)

#     We have a table of SOFAS scores (derived above) in the following column order
#         [A.social, B.personal, C.accom, C.selfcare, C.indep, D.aggress]

# --  The TRIPP concensus requires 60 or less (moderate) impairment on SOFAS.
#     We apply the (following) rubric to work out the proxy SOFAS score based on Morosini's
#     operationalised version, the PSP.
#
#     - To score 60 or less on the PSP/SOFAS (=moderate overall impairment)
#       the participant must have 'marked' impairment in one of A-C, with / without 'manifest' difficulty in D
#
#     - To score 50 or less, two of A-C or severe in one of A-C, with/without 'manifest' difficult in D
#
#     - To score 40 of less, 'severe' difficulties in one of A-C, with 'marked' difficulties in at least one of A-C
#       OR 'marked' dificulties in D.

  sofasAboveThresh <- function( V ) {
    # -- Takes a row vector V of values and applies the following logic over the four domains in PSP/SOFAS 
    
    # -- Section D - aggresion / disturbed behaviour
    #       This is relatively easy to score, because 'marked' is
    #       defined as including breaking/wrecking objects, insulting behaviour, and 
    #       'severe' is frequent verbal threats or frequent physical assaults.
    #
    # -- However, Morosini defines 'Occasional' (rather than frequent) as 3 or more incidents.
    #     -- So, we can operationalise this as D.aggress >= 3.  
    #        We use the MacArthur Community Violence scores in CATIE, which conveniently scores separate and increasing magnitude
    #        of aggression --> violence with a minimum of "throwing objects" and a maximum of using a knife/firearms.
    #     -- If more than 3 categories are "yes", this implies *at least* occasional aggression making
    #        section D at least 'marked' if not severe.
    thresh.D <- ifelse( V$D.aggress >= 3, 1, 0 )
    
    # -- Section A : socially useful activity and study
    #    We use the Heinrich 1984 QOL item Instrumental Role
    #    which ranges from 0-1 (severe) through 6-5 excellent
    #    So, we operationalise this as 'marked' (==1) if score of <= 3 
    thresh.A <- ifelse( V$A.social <= 3, 1, 0 )
    
    # -- Section B : Personal and social
    #    Similar rubric to Section A, but we use Heinrich's QOL Interpersonal relations score
    #    Again, we set 'marked' flag (==1) if score <= 3
    thresh.B <- ifelse( V$B.personal <= 3, 1, 0)
    
    # -- Section C: Self-care.
    #    More complicated, because there are a number of QOL items to consider
    #    -- 'marked' in this context (morosini 2000) is "able to [do something] without
    #    professional or social help' 
    #
    #    -- 'severe' is "unable to perform any role in that area [self-care] if not professionally helped".
    
    #    We can't disambiguate between marked and severe, but the data in C.accom, C.selfcare and C.indep
    #    tell us if the patient would be *at least* marked which is the threshold requirement
    
    #    So, to rate as 'marked' level, the patient (morosini 2000) must have at least problems in 
    #    self care as well as at least some help to maintain normal level.
    #    We suggest, then, that V$C.selfcare <= 3 (range 0 worst, 4 best) implies deficit in at least one area
    #    of chores, self care, laundry or clean room.  Further, if V$C.indep <= 3 this implies help in at least
    #    one area. Finally, if the patient lives in <= 3 accomodation (moderately supported) this implies
    #    help from professionals or at worst, homelessness.
    #
    # -- Final Rubric : ( selfcare <= 3 AND indep <= 3 ) OR ( accom <= 3 ) ==> marked impairement
    thresh.C <- ifelse( ( V$C.selfcare <= 3 & V$C.indep <= 3 ) | V$C.accom <=3, 1, 0 )
    
    ## Rule : from Morosini 2000 (our version underestimates impairment overall because of )
    ## -- Overall : Moderate or worse if ...
    ##    thresh.D == 1 (at least by above definition)
    ##    OR thresh.A OR thresh.B OR thresh.C == 1 (marked)
    A.to.C <- thresh.A + thresh.B + thresh.C   ## if any is 1, then sum > 1
    moderate.sofas <- ifelse( A.to.C >= 1 | thresh.D == 1, 1, 0  )
    
    return( moderate.sofas  )
  }
  
  # -- assembled the SOFAS for every participant and every visit / assessment
  TRS.sofas.traj <- sofasAboveThresh( SOFAS[, c("A.social", "B.personal", "C.accom", "C.selfcare", "C.indep", "D.aggress")] ) 
  TRS.sofas.traj <- cbind( SOFAS[ , c("src_subject_id", "phase_ct", "visday", "truncvis",
                                 "A.social", "B.personal", "C.accom", "C.selfcare", "C.indep", "D.aggress")], TRS.sofas.traj )

# -- Adequate treatment -------------------------------------------------------------------------------------------

# -- Lookup table: codes for the medications available in each phase
  phase1 <-  c("1" = "Olanzapine", "2" = "Quetiapine", "3" = "Risperidone", "4" = "Ziprasidone", "5" = "Perphenazine")
  phase2  <- c("1" = "Olanzapine", "2" = "Quetiapine", "3" = "Risperidone", "4" = "Ziprasidone", "5" = "Perphenazine",
               "6" = "Clozapine")
  
  phase3  <- c("1" = "Olanzapine", "2" = "Quetiapine", "3" = "Risperidone", "4" = "Ziprasidone", "5" = "Perphenazine",
               "6" = "Clozapine", "7" = "Fluephenazine-D", "8" = "Aripiprazole")


# -- we'll use a simple structure to record a participants medication / treatment history
  setClass("rxRecord",
           slots = list(ID = "numeric", 
                        all.phases.rx.num = "data.frame",
                        all.phases.rx.lbl = "data.frame",
                        phase1 = "data.frame",
                        phase1b = "data.frame",
                        phase2 = "data.frame",
                        phase31 = "data.frame",
                        phase32 = "data.frame",
                        rxElig = "data.frame",
                        rxN = "numeric",
                        eligibleRxN = "numeric"))
  

  # -- this function is the key unit of processing for each participant  
  computePhaseDrugs <- function( subRow, subDose, thisID ) {
  
    # -- extract treatment from each phase
    rx.phase1  <- phase1[ subRow$treat11a   ]
    rx.phase1b <- phase1[ subRow$treat_1b  ]
    rx.phase2  <- phase2[ subRow$treat2    ]
    rx.phase31 <- phase3[ subRow$treat_31  ]
    rx.phase32 <- phase3[ subRow$treat_32  ]
    
    phases.rx <- c("Phase1" = subRow$treat11a, "Phase1b" = subRow$treat_1b,
                   "Phase2" = subRow$treat2,  "Phase31" = subRow$treat_31, "Phase32" = subRow$treat_32 )
    
    phases.rx.lbl <- data.frame(Phase1 = rx.phase1, Phase1b = rx.phase1b,
                       Phase2 = rx.phase2, Phase31 = rx.phase31, Phase32 = rx.phase32, stringsAsFactors = FALSE )
    
    rx.hx <- new("rxRecord", ID = thisID, all.phases.rx.num = data.frame( phases.rx ),
                 all.phases.rx.lbl = phases.rx.lbl)
    
    # -- for each phase : 
    if( !is.na( phases.rx["Phase1"] ) ) {
      this.rx <- subDose[ subDose$phase_ct == "Phase 1/1A", 
                                         c("ID", "phase_ct","visitid", "visit", "visday", "dose1", "medad14") ]
      if( !nrow( this.rx ) < 1 ) { # data not missing
        names(this.rx) <- c("ID", "PhaseLbl", "VisitID", "VisitLbl", "VisitDay","Dose","Adherence")
        this.rx$RxNumeric <- as.numeric( phases.rx["Phase1"] )
        this.rx$RxLbl     <- rx.phase1
        
        # replace NA with 0, because if no dose record, assume 0 and we'll exclude later as ineligible
        this.rx$Dose[ which( is.na( this.rx$Dose ) ) ] <- 0 
        rx.hx@phase1 <- this.rx
      }
    }
    
    if( !is.na( phases.rx["Phase1b"] ) ) {
      this.rx <- subDose[ subDose$phase_ct == "Phase 1B", 
                          c("ID", "phase_ct","visitid", "visit", "visday", "capsules", "medad14") ]
      
      if( !nrow( this.rx ) < 1 ) { # data not missing
        # note : there is NO recorded dose for phase1B (don't know why) so we have to infer from schedule
        # in study protocol pp39
        # Then, capsules * drug dose = total dose
        mg.dose <- switch( rx.phase1b,
                              "Olanzapine" = 7.5,
                              "Quetiapine" = 200,
                              "Risperidone" = 1.5,
                              "Ziprasidone" = 40,
                              "Perpenazine" = 8,
                               stop("Error! Cannot compute Phase1b drug dose") ) 
    
        this.rx$capsules <- this.rx$capsules * mg.dose    
        names(this.rx) <- c("ID", "PhaseLbl", "VisitID", "VisitLbl", "VisitDay","Dose","Adherence")
        this.rx$RxNumeric <- as.numeric( phases.rx["Phase1b"] )
        this.rx$RxLbl     <- rx.phase1b
        this.rx$Dose[ which( is.na( this.rx$Dose ) ) ] <- 0 
        rx.hx@phase1b     <- this.rx
      }
    }
    
    if( !is.na( phases.rx["Phase2"] ) ) {
      
      # slight hitch here : if Clozapine in phase 2, then we need medad07 (not dose 2 !!!)
      this.rx <- subDose[ subDose$phase_ct == "Phase 2", 
                          c("ID", "phase_ct","visitid", "visit", "visday", "dose2", "medad14") ]
      
      if( !nrow( this.rx ) < 1 ) { # data not missing
        names(this.rx) <- c("ID", "PhaseLbl", "VisitID", "VisitLbl", "VisitDay","Dose","Adherence")
        this.rx$RxNumeric <- as.numeric( phases.rx["Phase2"] )
        this.rx$RxLbl     <- rx.phase2
        this.rx$Dose[ which( is.na( this.rx$Dose ) ) ] <- 0 
        rx.hx@phase2      <- this.rx
      }
    }
    
    if( !is.na( phases.rx["Phase31"] ) ) {
      # use subDose
      # drug.1 <- subDose$medad08
      # drug.2 <- subDose$medad11
      # dose.1 <- subDose$medad10
      # dose.2 <- subDose$medad13
      
      this.rx <- subDose[ subDose$phase_ct == "Phase 3", 
                          c("ID", "phase_ct","visitid", "visit", "visday", "medad10", "medad14") ]
      if( !nrow( this.rx ) < 1 ) { # data not missing
        names(this.rx) <- c("ID", "PhaseLbl", "VisitID", "VisitLbl", "VisitDay","Dose","Adherence")
        this.rx$RxNumeric <- as.numeric( phases.rx["Phase31"] )
        this.rx$RxLbl <- rx.phase31
        this.rx$Dose[ which( is.na( this.rx$Dose ) ) ] <- 0 
        rx.hx@phase31 <- this.rx
      }
    }
    
    if( !is.na( phases.rx["Phase32"] ) ) {
      this.rx <- subDose[ subDose$phase_ct == "Phase 3", 
                          c("ID", "phase_ct","visitid", "visit", "visday", "medad13", "medad14") ]
      if( !nrow( this.rx ) < 1 ) { # data not missing
        names(this.rx) <- c("ID", "PhaseLbl", "VisitID", "VisitLbl", "VisitDay","Dose","Adherence")
        this.rx$RxNumeric <- as.numeric( phases.rx["Phase32"] )
        this.rx$RxLbl <- rx.phase32
        this.rx$Dose[ which( is.na( this.rx$Dose ) ) ] <- 0 
        rx.hx@phase32 <- this.rx
      }
    }
    
    #set eligibility number to 0 for all records until processed later by summary_phase
    rx.hx@rxN <- 0
    
    return( rx.hx )
  }


# -- For all participants, compute drug allocations throughout the trial
#    This results in a data.frame with one row per participant, and columsn being the names
#    of medications allocated in each phase (1 through 3)
  
  allIDs <- unique( subRx$ID )
  allDrugs <- vector("list", length(allIDs))

  pb <- txtProgressBar(min = 1, max = length(allIDs), initial = 0)
  for( i in 1:length( allIDs ) ) {
    setTxtProgressBar(pb, i)
    thisID <- allIDs[i]
    subRow   <- subRx[ subRx$ID == thisID, ]
    subDose  <- drug.doses[ drug.doses$ID == thisID, ]
    
    test <- computePhaseDrugs( subRow, subDose, thisID )
    allDrugs[[i]] <- test

  }
  close(pb)
  

# -- The data sheets (SPCs) used to compute the mid-range dosing thresholds
  # ziprasidone : http://labeling.pfizer.com/ShowLabeling.aspx?id=584
  # Olanzapine : http://pi.lilly.com/us/zyprexa-pi.pdf
  # Risperidone : http://www.janssen.com/us/sites/www_janssen_com_usa/files/products-documents/risperdal-prescribing-information.pdf
  # Perphenazine : https://www.medicines.org.uk/emc/medicine/22596
  # Quetiapine : https://www.medicines.org.uk/emc/medicine/2295 and http://www.azpicentral.com/seroquel/seroquel.pdf
  # CLozapine : https://www.medicines.org.uk/emc/medicine/32564
  # Fluphenazine depot : http://www.medicines.org.uk/emc/medicine/6956/SPC/Modecate+Injection+25mg+ml
  # Aripiprazole : https://www.accessdata.fda.gov/drugsatfda_docs/label/2014/021436s038,021713s030,021729s022,021866s023lbl.pdf

# -- Lookup table of midrange doses
SPC.min <- list("Ziprasidone" = 120,          ## SPC : 20--100mg BD ==> 40--200mg TDD ==> midpoint is 40 + (200-40)/2 = 120
                "Olanzapine" = 12.5,          ## SPC : 10-15mg TDD ==> midpoint is 10 + (15-10)/2 = 12.5
                "Risperidone" = 10,           ## SPC : 4--16mg TDD ==> midpoint is 4 + (16-4)/2 = 
                "Perphenazine" = 18,          ## SPC : 4mg TDS ==> 12mg TDD to max of 24mg TDD => midpoint is 12 + (24-12)/2 = 18
                "Quetiapine" = 450,           ## SPC : 150mg -- 750mg daily ==> midpoint is 150 + (750 - 150)/2
                "Clozapine" = 325,            ## SPC : 200-450mg daily ==> midpoint is 200 + (450-200)/2
                "Fluephenazine-D" = 56.25,    ## SPC : 12.5mg -- 100mg two weekly ==> midpoint is 12.5 + (100-12.5)/2
                "Aripiprazole" = 20)          ## SPC : 10-30mg daily ==> midpoint is 10 + (30-10)/2


# -- Function to summarise a phase
  summary_phase <- function( df, last.phase.day ) {
    # -- pass any phase record (phase slots class rxRecord) and this will summarise the
    #    adherence, minimum and mean and SD dose and assess if this phase is eligible for a successful trial
    
    # catch missing data
    if( nrow( df ) == 0 ) { 
      return( NULL )
    }
    
    # compute difference of days for duration calc
    df$rxDur <- diff( c( last.phase.day, df$VisitDay ) )
    
    accept.dose <- as.numeric( SPC.min[ df$RxLbl[1] ] )
    
    # select rows where : adherence == 1 and dose is adequate.
    df$adqDose.rx   <- ifelse( df$Dose >= accept.dose, 1, 0 )
    df$AdherDose.rx <- ifelse( df$Adherence == 1 & df$adqDose.rx == 1, 1, 0 )
    
    # meets criteria on time ?
    duration      <- sum( df$rxDur[ which( df$AdherDose.rx == 1) ] )
    if( duration >= 42 ) {  
        # at least some of the drug trial meets criteria for duration
        # so record dose information for those rows 
        mean.dose     <- mean( df$Dose[ which( df$AdherDose.rx == 1) ] )
        SD.dose       <- sd( df$Dose[ which( df$AdherDose.rx == 1) ] )
        quantile.dose <- quantile( df$Dose[ which( df$AdherDose.rx == 1) ] )
        adeqTrial     <- 1   # we know this is true, because AdherDose == 1 iff Aherence and Dose are above threshold
      
    } else {
        # if not eligible on adherence and dose, then just summarise for record keeping
        mean.dose     <- mean( df$Dose )
        SD.dose       <- sd( df$Dose )
        quantile.dose <- quantile( df$Dose )
        adeqTrial       <- 0
        duration        <- sum( df$rxDur )
    }
    
    return( data.frame( 
                  drugLbl = df$RxLbl[1], meanDose = mean.dose, sdDose = SD.dose,
                  minDose = quantile.dose[1], 
                  medianDose = quantile.dose[3],
                  maxDose = quantile.dose[5],
                  IQR = quantile.dose[4] - quantile.dose[2],
                  rxDuration = duration, 
                  adeqTrial = adeqTrial,
                  phase = factor( df$PhaseLbl[1], levels = trialPhases ),
                  last.visit.day = df$VisitDay[ nrow(df) ]
                        )
    )
     
  }

# -- For each participant in allDrugs, summarise the treatment given ----------------------------------------------
  for( i in 1:length( allDrugs ) ) {
    
         # retrieve the record:
         thisRx <- allDrugs[[i]]
  
         # inspect phase1, phase1b, phase2, phase31 and phase32 for adherence as well as
         # adequate dosing
         
         store.rx <- data.frame()
  
         # with phase1
         ph1 <- summary_phase( thisRx@phase1, 1 )
         
         if( !is.null( ph1 ) ) {
            last.phase.day <- thisRx@phase1$VisitDay[ nrow(thisRx@phase1) ]
            store.rx <- rbind( store.rx, ph1 )
         }
         
         # phase1b
         if( nrow( thisRx@phase1b ) > 0 ) {
           ph1b <- summary_phase( thisRx@phase1b, last.phase.day )
           if( !is.null( ph1b ) ) {
            store.rx <- rbind( store.rx, ph1b )
            last.phase.day <- thisRx@phase1b$VisitDay[ nrow(thisRx@phase1b) ]
           }
         } else {
           ph1b <- NULL
         } 
         
         # phase 2
         if( nrow( thisRx@phase2 ) > 0 ) {
           ph2 <- summary_phase( thisRx@phase2, last.phase.day )
           if( !is.null( ph2 ) ) {
              store.rx <- rbind( store.rx, ph2 )
              last.phase.day <- thisRx@phase2$VisitDay[ nrow(thisRx@phase2) ]
           }
         } else {
           ph2 <- NULL
         }
         
         # phase 31
         if( nrow( thisRx@phase31 ) > 0 ) {
           ph31 <- summary_phase( thisRx@phase31, last.phase.day )
           if( !is.null( ph31  ) ) {
             store.rx <- rbind( store.rx, ph31 )
           }
         }
  
         if( nrow( thisRx@phase32 ) > 0 ) {
           ph32 <- summary_phase( thisRx@phase32, last.phase.day )
           if( !is.null( ph32 ) ) {
             store.rx <- rbind( store.rx, ph32 )
           }
         }
         
         thisRx@rxElig <- store.rx
         allDrugs[[i]] <- thisRx
         
    
  }

# -- loop over records in allDrugs, compute total trials and eligible adequate trials including phase at which this occurs
  for( i in 1:length( allDrugs ) ) {
    thisSub <- allDrugs[[i]]@rxElig
    
    if( nrow( thisSub ) > 0 ) {
      thisSub <- thisSub[ order( thisSub$phase ), ]
      
      thisSub$numericPhase <- numeric.trialPhases[ thisSub$phase ]
      # recall : numeric phase
      # Pre-Rand Phase 1/1A   Phase 1B    Phase 2    Phase 3    Phase 4 
      # 1          2          2           3          4          4 
      # now everything in trial-order, we can compute cumulative *adequate* trials -- BUT NOT necessarily unique
    
      # 0 => drug same as previous phase, 1 ==> different
      thisSub$diffDrugs <- c( 0, diff( as.numeric( thisSub$drugLbl ) ) )
      
      thisSub$cumAdeqRx <- rep(0, nrow(thisSub))
      thisSub$cumRx     <- rep(0, nrow(thisSub))
      
      if( thisSub$adeqTrial[1] == 1 ) { thisSub$cumAdeqRx[1] <- 1 }
      
      thisSub$cumRx[1] <- 1
      
      if( nrow( thisSub ) > 1 ) {
        for( j in 2:nrow( thisSub ) ) {
          
          if( thisSub$diffDrugs[j] == 1) { 
            thisSub$cumRx[j] <- thisSub$cumRx[j-1] + 1 
          } else { 
            thisSub$cumRx[j] <- thisSub$cumRx[j-1]   
          }
          
          if( thisSub$adeqTrial[j] == 1 & thisSub$diffDrugs[j] == 1 ) {
            thisSub$cumAdeqRx[j] <- thisSub$cumAdeqRx[j-1] + 1
          } else { 
            thisSub$cumAdeqRx[j] <- thisSub$cumAdeqRx[j-1] 
          }
          
        } # end for j
      } # end  if( nrow( thisSub ) > 1 )
    } # end if( nrow( thisSub ) > 0 )
    
    allDrugs[[i]]@rxElig <- thisSub
    if( nrow( thisSub ) > 0 ) {
      allDrugs[[i]]@rxN          <- length( unique( thisSub$drugLbl ) )    # total Rx trials, NOT necessarily adequate
      allDrugs[[i]]@eligibleRxN  <- max( thisSub$cumAdeqRx )
    } else {
      allDrugs[[i]]@rxN    <- 0
      allDrugs[[i]]@eligibleRxN  <- 0
    }
  }

# -- Finally, build the final tabulated list of all participant's medication "trajectories" throughout trial -----
  TRS.rx.traj <- data.frame()
  for( i in 1:length(allDrugs) ) {
    # this subject ID and summary of rxs
    thisID <- allDrugs[[i]]@ID
    thisRx <- allDrugs[[i]]@rxElig
    if( nrow( thisRx ) > 0 ) {
      # append ID
      thisRx$ID <- rep(thisID, nrow( thisRx ))
      TRS.rx.traj <- rbind( TRS.rx.traj, thisRx )
    }
  } 

# -- Build a final tabulated list of all participant's Symptoms throughout trial ---------------------------------
uni.IDs <- unique( TRS.sx$src_subject_id )
TRS.sx.traj <- data.frame( ID = numeric(),
                           phase = factor(),# character(),
                           truncvis = numeric(),
                           visday = numeric(),
                           TRS.pos = numeric(),  #absolute symptom criteria POS, NEG and GEN
                           TRS.neg = numeric(),
                           TRS.gen = numeric(),
                           adjPos  = numeric(),
                           adjNeg  = numeric(),
                           adjGen  = numeric(),
                           adjTot  = numeric()
)

for( i in 1:length( uni.IDs ) ) {
  thisID <- uni.IDs[i]
  # retrieve all rows relevant to thisID from TRS.sx
  thisSubIdx    <- which(TRS.sx$src_subject_id == thisID)
  thisSub       <- TRS.sx[ thisSubIdx, ]
  thisSubIdx.F5 <- which(PANSS.F5$ID == thisID)
  thisSub.F5    <- PANSS.F5[ thisSubIdx.F5, ] 
  
  if( nrow( thisSub ) < 2 ) {
    # no progression data to estimate trajectory i.e. subject left study very early with only one record/assessment
    
    thisEntry <- data.frame( ID = thisID,
                             phase = thisSub$phase_ct,
                             truncvis = thisSub$truncvis,
                             visday = thisSub$visday,
                             TRS.pos = NA,  #absolute symptom criteria POS, NEG and GEN
                             TRS.neg = NA,
                             TRS.gen = NA,
                             adjPos  = NA,
                             adjNeg  = NA,
                             adjGen  = NA,
                             adjTot  = NA
                             )
  } else {
    # organise into ascending visit order, so changes can be computed
    thisSub    <- thisSub[ order( thisSub$truncvis), ]
    thisSub.F5 <- thisSub.F5[ order( thisSub.F5$truncvis), ]
    
    # compute trajectory
    # percentage decrease in total PANSS (Leucht 2009)
    traj.totalPANSS <- (thisSub$total.POS + thisSub$total.NEG + thisSub$total.GEN) - 30
    traj.totalPANSS.F5 <- (thisSub.F5$F5.pos + thisSub.F5$F5.neg + thisSub.F5$F5.disorg + thisSub.F5$F5.excite + thisSub.F5$F5.depanx) - 27

    # absolute criteria for pos, neg and gen domains as per aboveThresh function (see above for explaination)
    absTRS.pos <- thisSub$TRS.pos
    absTRS.neg <- thisSub$TRS.neg
    absTRS.gen <- thisSub$TRS.gen

    thisEntry <- data.frame( ID = thisID,
                             phase = thisSub$phase_ct,
                             truncvis = thisSub$truncvis,
                             visday = thisSub$visday,
                             TRS.pos = absTRS.pos,
                             TRS.neg = absTRS.neg,
                             TRS.gen = absTRS.gen,
                             adjPos  = thisSub$adj.total.POS,
                             adjNeg  = thisSub$adj.total.NEG,
                             adjGen  = thisSub$adj.total.GEN,
                             adjTot  = traj.totalPANSS                            
    )
    
  }
  TRS.sx.traj <- rbind( TRS.sx.traj, thisEntry )
}




