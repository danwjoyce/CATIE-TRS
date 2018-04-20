Data Conversion Checks


dwj : April 16th 2018


# Purpose
This document describes how to check that the source CATIE data has been converted to CSV to be compatible with the scripts.

After converting your own copy of the source CATIE data to CSV, they are loaded and prepared by `load_all_tables.R`

You can check that your conversion to CSV is compatible with the scripts by running `load_all_tables.R` on your converted CATIE data, then examining the individual 9 tables in the R environment using `head()` and `dim()` to check the first few records, column headers and overall dimensions to make sure they agree.  Below is the expected output for each of the 9 data sources after being loaded and cleaned by `load_all_tables.R`.

---

# 1. For "demographic_baseline_meds.csv"

	> head( demog.tab )

	  collection_id dataset_id       subjectkey src_subject_id interview_age interview_date gender white black native asian pacific  race
	1          2081       8974 NDAR_INVAA412BBY           2341            43             NA      M     1     0      0    No      No White
	2          2081       8974 NDAR_INVAA412BBY           2341            43             NA      M     1     0      0    No      No White
	3          2081       8974 NDAR_INVAA412BBY           2341            43             NA      M     1     0      0    No      No White
	4          2081       8974 NDAR_INVAA412BBY           2341            43             NA      M     1     0      0    No      No White
	5          2081       8974 NDAR_INVAA412BBY           2341            43             NA      M     1     0      0    No      No White
	6          2081       8974 NDAR_INVAB125GA8           2667            24             NA      M     1     0      0    No      No White
	  race_c hispanic exacer td        das1ms marital2 sigother psysoc_89              curreduccompleted demo4a educ_yrs par_educ demo5a
	1      1        0      1  1       Widowed        0        0         N   Did not complete high school      4        4        1      1
	2      1        0      1  1       Widowed        0        0         N   Did not complete high school      4        4        1      1
	3      1        0      1  1       Widowed        0        0         N   Did not complete high school      4        4        1      1
	4      1        0      1  1       Widowed        0        0         N   Did not complete high school      4        4        1      1
	5      1        0      1  1       Widowed        0        0         N   Did not complete high school      4        4        1      1
	6      1        0      1  0 Never married        0        0         N Some college, did not graduate     NA       13        4     NA
	  cgi_sev cgi_cat b1_panss employ employ2 qol01a qol01b qol01c qol01d qol01e qol01f qol01g scid17a yrs_trt scid18a yrs_pres scid19 scid20
	1       4       0       78      3       0      1      0      1      0      0      0      0      16      27      20       23      4      4
	2       4       0       78      3       0      1      0      1      0      0      0      0      16      27      20       23      4      4
	3       4       0       78      3       0      1      0      1      0      0      0      0      16      27      20       23      4      4
	4       4       0       78      3       0      1      0      1      0      0      0      0      16      27      20       23      4      4
	5       4       0       78      3       0      1      0      1      0      0      0      0      16      27      20       23      4      4
	6       3       0       52      3       0      1      0      0      0      0      0      0      22       2      22        2      1      1
	  anti_beh beh_cat sitetype rescreen scrn_dy olz_0 quet_0 risp_0 zip_0 hal_0 deca_0 per_0 othr_0 allothr0 none_0 olzi_0 queti_0 rispi_0
	1        5       4       UC        0     -20     1      1      0     0     0      0     0      0        0      0      0       0       0
	2        5       4       UC        0     -20     1      1      0     0     0      0     0      0        0      0      0       0       0
	3        5       4       UC        0     -20     1      1      0     0     0      0     0      0        0      0      0       0       0
	4        5       4       UC        0     -20     1      1      0     0     0      0     0      0        0      0      0       0       0
	5        5       4       UC        0     -20     1      1      0     0     0      0     0      0        0      0      0       0       0
	6        2       2       UC       NA      -2     1      0      0     0     0      0     0      0        0      0      1       0       0
	  dualmed0 allothi0 switch me0a                             soc              prf_term me2 me3 me3a any_mhx ran002 dema6 dema7 dema9 living1
	1        1        0      2    1                   EYE DISORDERS            STRABISMUS   0   0   NA      NA     NA    NA    NA    NA      NA
	2        1        0      2    1         IMMUNE SYSTEM DISORDERS DRUG HYPERSENSITIVITY   1  NA   NA      NA     NA    NA    NA    NA      NA
	3        1        0      2    1 SURGICAL AND MEDICAL PROCEDURES        APPENDICECTOMY   0  NA   NA      NA     NA    NA    NA    NA      NA
	4        1        0      2    1 SURGICAL AND MEDICAL PROCEDURES  EYE MUSCLE OPERATION   0  NA   NA      NA     NA    NA    NA    NA      NA
	5        1        0      2    1 SURGICAL AND MEDICAL PROCEDURES   JOINT OPERATION NOS   0  NA   NA      NA     NA    NA    NA    NA      NA
	6        0        0      2   NA                                                        NA  NA   NA      NA     NA    NA    NA    NA      NA
	  strata trt visit fseqno daysrz version_form site treatment_name wtliab2
	1     NA  NA    NA     NA     NA           NA   NA             NA      NA
	2     NA  NA    NA     NA     NA           NA   NA             NA      NA
	3     NA  NA    NA     NA     NA           NA   NA             NA      NA
	4     NA  NA    NA     NA     NA           NA   NA             NA      NA
	5     NA  NA    NA     NA     NA           NA   NA             NA      NA
	6     NA  NA    NA     NA     NA           NA   NA             NA      NA
		                                                               collection_title
	1 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	2 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	3 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	4 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	5 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	6 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia

	> dim( demog.tab )
	[1] 6678   87

---

# 2. For "doses_of_drugs.csv"

	> head( drug.doses )

	  collection_id dataset_id       subjectkey src_subject_id interview_age interview_date gender visitid  visit truncvis phase_ct visday
	2          2081       8974 NDAR_INVAB125GA8           2667            NA             NA      M     800 Visit2        2  Phase 2     33
	3          2081       8974 NDAR_INVAB125GA8           2667            NA             NA      M    1000 Visit3        3  Phase 2     61
	4          2081       8974 NDAR_INVAB125GA8           2667            NA             NA      M    1200 Visit4        4  Phase 2     89
	5          2081       8974 NDAR_INVAB125GA8           2667            NA             NA      M    1600 Visit6        6  Phase 3    147
	6          2081       8974 NDAR_INVAB125GA8           2667            NA             NA      M    1800 Visit7        7  Phase 3    174
	7          2081       8974 NDAR_INVAB125GA8           2667            NA             NA      M    2000 Visit8        8  Phase 3    201
	  medad01 medad03 medad06 medad07 msa04 msa04b medad08 medad10 medad11 medad13 medad14 capsules dose1 dose2 dose3 presreg1 presreg2
	2       2     100       2      NA     1     NA      NA      NA      NA      NA       1        2    NA    15    NA       NA       NA
	3       2     100       2      NA     1     NA      NA      NA      NA      NA       1        2    NA    15    NA       NA       NA
	4       2      91       4      NA     1     NA      NA      NA      NA      NA       1        4    NA    30    NA       NA       NA
	5       3      NA      NA      NA    NA     NA       1      15       3       2       1       NA    NA    NA    NA       NA       NA
	6       3      NA      NA      NA    NA     NA       1      15       3       2       1       NA    NA    NA    NA       NA       NA
	7       3      NA      NA      NA    NA     NA       1      15       3       2       1       NA    NA    NA    NA       NA       NA
	  avgcaps1 avgdose1 moddose1 catcaps1 avgcaps2 avgdose2 moddose2 catcaps2 avgdose3 moddose3 max1 max2 max3 madh12a madh12b madh13 treat_1
	2       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA   NA   NA   NA      NA      NA     NA      NA
	3       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA   NA   NA   NA      NA      NA     NA      NA
	4       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA   NA    1   NA      NA      NA     NA      NA
	5       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA   NA   NA   NA      NA      NA     NA      NA
	6       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA   NA   NA   NA      NA      NA     NA      NA
	7       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA   NA   NA   NA      NA      NA     NA      NA
	  treat2 treat_3 dur_1 dur_2 dur_3 lowdose highdose dose first_d last_d first_c last_c first_v last_v dse_adj1 dse_adj2 compl_1 compl_1c
	2     NA      NA    NA    NA    NA      NA       NA   NA      NA     NA      NA     NA      NA     NA       NA       NA      NA       NA
	3     NA      NA    NA    NA    NA      NA       NA   NA      NA     NA      NA     NA      NA     NA       NA       NA      NA       NA
	4     NA      NA    NA    NA    NA      NA       NA   NA      NA     NA      NA     NA      NA     NA       NA       NA      NA       NA
	5     NA      NA    NA    NA    NA      NA       NA   NA      NA     NA      NA     NA      NA     NA       NA       NA      NA       NA
	6     NA      NA    NA    NA    NA      NA       NA   NA      NA     NA      NA     NA      NA     NA       NA       NA      NA       NA
	7     NA      NA    NA    NA    NA      NA       NA   NA      NA     NA      NA     NA      NA     NA       NA       NA      NA       NA
	  compl_2 compl_2c compl_3 compl_3c b1_day b2_day b3_day boc_day e1_day e2_day e3_day eoc_day es_day b3_cgic b3_resp
	2      NA       NA      NA       NA     NA     NA     NA      NA     NA     NA     NA      NA     NA      NA      NA
	3      NA       NA      NA       NA     NA     NA     NA      NA     NA     NA     NA      NA     NA      NA      NA
	4      NA       NA      NA       NA     NA     NA     NA      NA     NA     NA     NA      NA     NA      NA      NA
	5      NA       NA      NA       NA     NA     NA     NA      NA     NA     NA     NA      NA     NA      NA      NA
	6      NA       NA      NA       NA     NA     NA     NA      NA     NA     NA     NA      NA     NA      NA      NA
	7      NA       NA      NA       NA     NA     NA     NA      NA     NA     NA     NA      NA     NA      NA      NA
		                                                               collection_title   ID
	2 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia 2667
	3 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia 2667
	4 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia 2667
	5 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia 2667
	6 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia 2667
	7 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia 2667

	> dim( drug.doses )
	[1] 16707    81

---

# 3. For "panss01.csv"

	> head( PANSS.tab )
	  src_subject_id phase_ct visday truncvis pos_p1 pos_p2 pos_p3 pos_p4 pos_p5 pos_p6 pos_p7 neg_n1 neg_n2 neg_n3 neg_n4 neg_n5 neg_n6 neg_n7
	1           2341 Pre-Rand      1        0      3      4      6      2      1      3      3      4      4      3      4      4      3      2
	2           2667  Phase 3    407       15      3      1      1      2      1      2      1      1      1      1      1      2      1      1
	3           2667  Phase 3    489       18      3      2      1      1      3      1      1      1      1      1      2      2      1      1
	4           2667  Phase 3    147        6      3      1      1      2      1      2      1      3      1      1      2      1      1      1
	5           2667  Phase 3    314       12      3      1      1      1      1      2      1      3      1      1      1      1      1      1
	6           2667  Phase 3    225        9      3      1      2      1      4      2      1      1      1      1      1      1      1      1
	  gps_g1 gps_g2 gps_g3 gps_g4 gps_g5 gps_g6 gps_g7 gps_g8 gps_g9 gps_g10 gps_g11 gps_g12 gps_g13 gps_g14 gps_g15 gps_g16
	1      1      3      2      2      2      5      2      1      1       2       1       2       3       1       1       3
	2      1      2      1      1      1      1      1      1      3       1       1       1       1       1       1       1
	3      1      2      1      2      1      2      1      1      2       1       1       1       1       2       1       1
	4      1      3      1      3      1      1      1      1      1       1       2       1       1       1       1       1
	5      1      2      1      2      1      3      1      1      1       1       3       1       1       1       1       1
	6      1      3      1      3      1      1      1      1      3       1       1       1       1       1       1       2

	> dim( PANSS.tab )
	[1] 8849   34

---

# 4. For "neurobatt01.csv"

	> head( cogtest.tab )
	  collection_id dataset_id       subjectkey src_subject_id interview_date interview_age gender visitid      visit truncvis phase_ct visday
	1          2081       8976 NDAR_INVAA412BBY           2341             NA            NA      M     400   Baseline        0 Pre-Rand      1
	2          2081       8976 NDAR_INVAB125GA8           2667             NA            NA      M     400   Baseline        0 Pre-Rand      1
	3          2081       8976 NDAR_INVAB125GA8           2667             NA            NA      M     800     Visit2        2  Phase 2     33
	4          2081       8976 NDAR_INVAB125GA8           2667             NA            NA      M    1600     Visit6        6  Phase 3    147
	5          2081       8976 NDAR_INVAB125GA8           2667             NA            NA      M   30000 EndofStudy       18  Phase 3    489
	6          2081       8976 NDAR_INVAB164FVP           2808             NA            NA      F     400   Baseline        0 Pre-Rand      1
	    protocol bat01 bat02 bat03 bat04 bat05 bat06 bat07 bat08 bat09 bat10 bat11 bat12 neur1 wrat_read_raw neur2a neur3a neur3b neur3c neur4a
	1 Catieschiz     0     0     0     0     0     0     0     0     0     0     0     0     1            28      0      8      7     13     20
	2 Catieschiz     0     0     0     0     0     0     0     0     0     0     0     0     1            52      0     21     13     19     21
	3               NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA     1            NA      1     14     11     20     20
	4               NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA     1            NA      1     10     14     14     18
	5               NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA     1            NA      1     17      9     20     17
	6 Catieschiz     0     0     0     0     0     0     0     0     0     0     0     0     1            44      0     10      5     11     10
	  neur4b neur4c neur5 neur7a neur7b neur7c neur8 neur9 neur6 neur10a neur10b neur11a neur11b neur11c neur11d neur11e neur11f neur12a
	1     10      9    23      6      9     11    21    43    13      15      17   0.589   0.192   0.105     502     526     526       1
	2     16     14    27      7      8     10    29    56     8      18      20   3.963   3.630   3.963     432     518     487       1
	3     19     15    20      7      7      8    29    67    16      17      21   4.257   3.963   3.360     418     479     490       1
	4     13     13    23      4      7      9    27    64    18      19      20   3.630   2.751   2.343     485     498     511       1
	5     16     10    28      5      7      9    21    72    22      15      20   3.963   4.257   3.669     458     469     471       1
	6      9      5    21      4      0      4    27    39    17      10      10   1.287   0.879   0.220     607     550     599       1
	  neur12b neur12c neur13a neur13b neur13c neur8_c wrat3 p_flag neur5_s neur6_s neur7a_s neur7b_s neur7c_s neur9_s neu11a_s neu11b_s
	1      17      23      NA      NA      NA      21     4      1       1       1        1        1        1       0       -2       -2
	2      14      14      10       2       0      29     5      1       2       0        1        1        1       1        2        2
	3       9      13       4       4       0      29     1      1       0       1        1        0        0       2        2        2
	4      10      13       4       4       0      27     1      1       1       2        0        0        1       2        1        1
	5      12      15       4       5       0      21     1      1       2       3        0        0        1       3        2        3
	6      28      26      13       3       9      27     5      1       1       2        0       -3       -1       0       -1       -1
	  neu11c_s neu12_sr mneur34 mneur10 mneur13 n_flag verbal_s vigil_s speed_s reason_s memory_s neuro_ss
	1       -1        0       0       1      NA      1    1.252  -1.658   0.682    1.159    0.645    0.547
	2        4        1       2       2       0      1    1.140   2.816   2.188    1.049    0.197    1.907
	3        3        1       2       2       1      1    0.654   2.760   2.478    0.875    1.329    2.087
	4        2        1       1       2       1      1    0.156   1.550   2.111    1.190    1.570    1.698
	5        3        1       1       1       1      1    0.373   2.925   2.266    1.902    2.021    2.443
	6       -1        0       0      -1       0      1   -1.851  -1.076  -0.442    0.680    0.930   -0.437
		                                                               collection_title
	1 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	2 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	3 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	4 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	5 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	6 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia

	> dim( cogtest.tab )
	[1] 4250   79

---

# 5. For "qol01.csv"

	> head( QoL.tab )
	  src_subject_id   phase_ct visday truncvis qol01a qol01b qol01c qol01d qol01e qol01f qol01g qol02 qol03 qol04 qol05 qol06 qol07 qol08
	1           2341   Pre-Rand      1        0      1      0      1      0      0      0      0     5     0     0     1     2     0     0
	2           2667   Pre-Rand      1        0      1      0      0      0      0      0      0     6     5     4     5     6     6     6
	3           2667    Phase 3    147        6      1      0      0      0      0      0      0     6     6     6     6     6     6     5
	4           2667    Phase 3    314       12      1      0      0      0      0      0      0     6     6     6     6     5     6     6
	5           2667 Phase 1/1A     19        1      1      0      0      0      0      0      0     6     5     4     4     5     4     5
	6           2667    Phase 2    117        5      1      0      0      0      0      0      0     6     5     5     5     4     5     6
	  qol09 qol10 qol11 qol12 qol13 qol14 qol15 qol16 qol17 qol18 qol19 qol20 qol21 qol22 qol23a qol23b qol23c qol23d qol24a qol24b qol24c
	1     0     0     0     0     9     0     6     4     4     4     5     4     5     5      0      1      1      0      1      1      1
	2     0     1     5     6     9     6     6     4     6     0     5     4     6     6      1      0      1      1      1      1      1
	3     0     2     5     6     9     3     6     2     6     4     5     4     6     6      1      1      1      0      1      1      0
	4     0     4     6     6     6     4     6     2     6     5     5     4     6     6      1      1      1      1      1      1      1
	5     0     4     4     6     6     3     5     3     4     4     5     5     6     6      1      1      1      1      1      1      1
	6     2     3     2     1     3     0     3     4     3     4     5     4     5     4      0      1      1      0      1      1      1
	  qol24d qol25a qol25b qol25c qol25d qol25e qol25f qol25g qol25h qol25ia qol25ib qol25ic qol25id qol25ie qol25if qol25ig qol25j1 qol25j2
	1      1      1      1      0      4      4      4      4      1       0       1       0       0       0       0       0       0       1
	2      1      1      1      1      3      3      3      4      1       0       1       0       0       0       0       0       1       0
	3      1      1      1      1      4      3      3      4      1       0       1       0       0       0       0       0       1       0
	4      1      1      1      1      4      3      3      4      1       0       1       0       0       0       0       0       1       0
	5      1      1      1      1      3      3      2      4      1       0       1       0       0       0       0       0       0       0
	6      1      1      1      1      4      3      3      4      1       0       1       0       0       0       0       0       1       0
	  qol25j3 qol25j4 qol26a qol26aa qol26b qol26ba qol26c qol26ca qol26d qol26da qol26e qol26ea qol27a qol27b qol27c qol28a qol28b qol28c
	1       0       0      1      14      0      NA      0      NA      1      16      0      NA      0      0      0      0      0      0
	2       0       0      1      30      0      NA      0      NA      0      NA      0      NA      0      0      0      0      0      0
	3       1       0      1      30      0      NA      0      NA      0      NA      0      NA      2      2      2      0      0      0
	4       0       0      1      30      0      NA      0      NA      0      NA      0      NA      1      0      0      0      0      1
	5       0       0      1      30      0      NA      0      NA      0      NA      0      NA      0      0      0      0      0      0
	6       1       0      1      30      0      NA      0      NA      0      NA      0      NA      0      0      0      0      0      0
	  qol28d qol28e qol29a qol29b qol29c qol29d qol30a qol30b qol30c qol30d qol31 intr_rel inst_rol intr_fou com_obj
	1      0      0      0      0      0      0      1      1      1      1     6    1.000     0.00 4.000000     4.5
	2      0      1      0      0      0      0      1      1      1      1     4    4.750     3.00 4.857143     4.5
	3      1      0      0      0      0      0      1      1      1      1     5    5.125     3.25 4.714286     4.5
	4      1      0      0      0      0      0      1      1      1      1     6    5.125     5.50 5.000000     4.5
	5      0      0      0      0      0      0      1      1      1      1     4    4.125     5.00 4.428571     5.0
	6      0      1      0      0      0      0      1      1      1      1     4    4.750     2.25 3.285714     4.5

	> dim( QoL.tab )
	[1] 4966   90

---

# 6. For "macvlnce01.csv"

	> head( viol.tab )
	  src_subject_id   phase_ct visday truncvis mac1a mac2a mac3a mac4a mac5a mac6a mac7a mac8a mac9a mac10a mac11a mac12a mac13a mac14a mac15a
	1           2341   Pre-Rand      1        0     0     0     0     0     0     0     0     0     0      0      0      0      0      0      0
	2           2667   Pre-Rand      1        0     0     0     0     0     0     0     0     0     0      0      0      0      0      0      0
	3           2667    Phase 3    147        6     0     0     0     0     0     0     0     0     0      0      0      0      0      0      0
	4           2667    Phase 3    314       12     0     0     0     0     0     0     0     0     0      0      0      0      0      0      0
	5           2667 Phase 1/1A     19        1     0     0     0     0     0     0     0     0     0      0      0      0      0      0      0
	6           2667    Phase 2    117        5     0     0     0     0     0     0     0     0     0      0      0      0      0      0      0
	  mac16a mac17a mac18a mac19a mac19b mac19c mac19d
	1      0      0      0      0      0      0      0
	2      0      0      0      0      0      0      0
	3      0      0      0      0      0      0      0
	4      0      0      0      0      0      0      0
	5      0      0      0      0      0      0      0
	6      0      0      0      0      0      0      0

	> dim( viol.tab )
	[1] 5009   26

---

# 7. For "scid_ph01.csv"

	> head( SCID.tab )
	  src_subject_id scid03 scid05 scid07 scid09 scid11 scid13 scid15
	1           2341      1      0      0      0      0      0      0
	2           2667      0      0      0      0      1      0      0
	3           2808      0      0      0      0      0      0      0
	4           2748      0      0      0      0      0      0      0
	5           1117      0      0      0      0      0      0      0
	6           2676      1      0      0      0      0      0      0

	> dim( SCID.tab )
	[1] 1460    8

---

# 8. For "dgsposys01.csv"

Note, there is quite a lot of post-processing in `load_all_tables.R` for this data

	> head( pmhx.tab )
	    ID COPD DM HepABC Lipid HTN IHD OsteoArth Osteopor STI
	1 2341    0  0      0     0   0   0         0        0   0
	2 2667    0  0      0     0   0   0         0        0   0
	3 2808    0  0      1     0   0   0         0        0   0
	4 2748    0  1      1     0   0   0         0        1   0
	5 1117    0  0      0     1   0   0         0        0   0
	6 2676    0  1      0     1   1   0         0        0   0

	> dim( pmhx.tab )
	[1] 1448   10

---

# 9. For "keyvars01.csv"

	> head( keyvars )
	  collection_id dataset_id       subjectkey src_subject_id interview_date interview_age gender siteno sitepool scrnpop multscrn failreas
	1          2081       8975 NDAR_INVAA412BBY           2341             NA            NA      M    114      114       1       NA       NA
	2          2081       8975 NDAR_INVAB125GA8           2667             NA            NA      M    107      107       1       NA       NA
	3          2081       8975 NDAR_INVAB164FVP           2808             NA            NA      F    157      157       1       NA       NA
	4          2081       8975 NDAR_INVAB580MK8           2748             NA            NA      M    141      141       1       NA       NA
	5          2081       8975 NDAR_INVAB636BG0           1117             NA            NA      M    133      133       1       NA       NA
	6          2081       8975 NDAR_INVAC199RGN           2676             NA            NA      F    102      102       1       NA       NA
	  comp_s dcr_s es_day   lastphas in_fu rand_1 rand_1a rand1_1a b1_day e1_day ittpop_1 comp_1 comp_1a comp1_1a dcr1_1 dcr2_1        aedsc_1
	1      0     2    188 Phase 1/1A    NA     NA       1        1      1    188        1     NA       0        0      4     NA               
	2      1    NA    489    Phase 3    NA      1      NA        1      1     19        1      0      NA        0      2     NA               
	3      1    NA    505   Phase 1B    NA      1      NA        1      1     42        1      0      NA        0      3     NA Extrapyramidal
	4      1    NA    558 Phase 1/1A    NA     NA       1        1      1    558        1     NA       1        1     NA     NA               
	5      1    NA    530 Phase 1/1A    NA      1      NA        1      1    530        1      1      NA        1     NA     NA               
	6      0     1    486 Phase 1/1A    NA      1      NA        1      1    486        1      0      NA        0      1     NA               
	  rand_1b b1b_day e1b_day ittpop1b comp_1b dcr1_1b dcr2_1b aedsc_1b rand_2e rand_2t rand2 b2_day e2_day ittpop_2 comp_2e comp_2t comp2_et
	1      NA      NA      NA        0      NA      NA      NA               NA      NA     0     NA     NA        0      NA      NA       NA
	2      NA      NA      NA        0      NA      NA      NA               NA       1     1     19    117        1      NA       0        0
	3       1      42     505        1       1      NA      NA               NA      NA     0     NA     NA        0      NA      NA       NA
	4      NA      NA      NA        0      NA      NA      NA               NA      NA     0     NA     NA        0      NA      NA       NA
	5      NA      NA      NA        0      NA      NA      NA               NA      NA     0     NA     NA        0      NA      NA       NA
	6      NA      NA      NA        0      NA      NA      NA               NA      NA     0     NA     NA        0      NA      NA       NA
	  dcr1_2 dcr2_2 aedsc_2 in_3 b3_day e3_day comp_3 dcr1_3 dcr2_3 aedsc_3 phase1a treat_1 treat_1a treat11a zip_1 zip_1a zpr_cort dose_1
	1     NA     NA           NA     NA     NA     NA     NA     NA               1      NA        3        3    NA      1        1     NA
	2      2     NA            1    117    489      1     NA     NA               0       4       NA        4     1     NA        1      1
	3     NA     NA           NA     NA     NA     NA     NA     NA               0       5       NA        5     1     NA        1      2
	4     NA     NA           NA     NA     NA     NA     NA     NA               1      NA        1        1    NA      1        1     NA
	5     NA     NA           NA     NA     NA     NA     NA     NA               0       1       NA        1     0     NA        0      1
	6     NA     NA           NA     NA     NA     NA     NA     NA               0       5       NA        5     1     NA        1      2
	  dose_1a randreg1 treat_1b dose_1b treat_2e treat_2t treat2 zip_2e zip_2t ph2_arm dose_2e dose_2t randreg2 treat_31 treat_32 treat_3 set1
	1       1        2       NA      NA       NA       NA     NA     NA     NA      NA      NA      NA       NA       NA       NA      NA    0
	2      NA        2       NA      NA       NA        1      1     NA      1       2      NA       1        2        1        3       9    0
	3      NA        1        1       1       NA       NA     NA     NA     NA      NA      NA      NA       NA       NA       NA      NA    1
	4       1        2       NA      NA       NA       NA     NA     NA     NA      NA      NA      NA       NA       NA       NA      NA    0
	5      NA        2       NA      NA       NA       NA     NA     NA     NA      NA      NA      NA       NA       NA       NA      NA    1
	6      NA        1       NA      NA       NA       NA     NA     NA     NA      NA      NA      NA       NA       NA       NA      NA    1
	  set2 set3 set4 action_1 comp_2 action_2 action_3 comp_oc aedsc_oc dcr1_oc in_oc rand_3 sitetype care_st stdy_dur boc_day eoc_day treat3
	1    1    1    0       NA     NA       NA       NA      NA       NA      NA    NA     NA       NA      NA       NA      NA      NA     NA
	2    0    1    1       NA     NA       NA       NA      NA       NA      NA    NA     NA       NA      NA       NA      NA      NA     NA
	3    0    0    1       NA     NA       NA       NA      NA       NA      NA    NA     NA       NA      NA       NA      NA      NA     NA
	4    1    1    0       NA     NA       NA       NA      NA       NA      NA    NA     NA       NA      NA       NA      NA      NA     NA
	5    1    0    0       NA     NA       NA       NA      NA       NA      NA    NA     NA       NA      NA       NA      NA      NA     NA
	6    0    0    1       NA     NA       NA       NA      NA       NA      NA    NA     NA       NA      NA       NA      NA      NA     NA
		                                                               collection_title
	1 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	2 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	3 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	4 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	5 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia
	6 Clinical Antipsychotic Trials of Intervention Effectiveness (CATIE) for Schizophrenia

	> dim( keyvars )
	[1] 1460  100

---

Further : the derived table subRx

	> head( subRx )
	  src_subject_id treat_1 treat_1a treat11a dose_1 dose_1a treat_1b dose_1b treat_2e treat_2t treat2 dose_2e dose_2t treat_31 treat_32
	1           2341      NA        3        3     NA       1       NA      NA       NA       NA     NA      NA      NA       NA       NA
	2           2667       4       NA        4      1      NA       NA      NA       NA        1      1      NA       1        1        3
	3           2808       5       NA        5      2      NA        1       1       NA       NA     NA      NA      NA       NA       NA
	4           2748      NA        1        1     NA       1       NA      NA       NA       NA     NA      NA      NA       NA       NA
	5           1117       1       NA        1      1      NA       NA      NA       NA       NA     NA      NA      NA       NA       NA
	6           2676       5       NA        5      2      NA       NA      NA       NA       NA     NA      NA      NA       NA       NA
	  treat_3   lastphas   ID  LastPhase
	1      NA Phase 1/1A 2341 Phase 1/1A
	2       9    Phase 3 2667    Phase 3
	3      NA   Phase 1B 2808   Phase 1B
	4      NA Phase 1/1A 2748 Phase 1/1A
	5      NA Phase 1/1A 1117 Phase 1/1A
	6      NA Phase 1/1A 2676 Phase 1/1A

	> dim( subRx )
	[1] 1460   19



