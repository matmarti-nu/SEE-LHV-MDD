
* Defining the path to databases

global data5p1 	"Path to core data"

* Saving the results in a log file
*log using 	"$data/CBCL categories.smcl", replace

***********************************************************************************************************
/** Databases

1.   mental-health\mh_p_cbcl: 						Summary scores 
2.   mental-health\mh_p_cbcl: 						Raw data/CBCL
3.   mental-health\mh_y_ksads_bg:					KSADS, Youth Diagnostic Interview - Background items
4.   mental-health\mh_p_ksads_bg:					KSADS, Parent Diagnostic Interview - Background items
5.   gender-identity-sexual-health\gish_y_sex: 		Sexual orientation
6.   physical-health\ph_y_anthro: 					Youth Anthropometrics Modified From PhenX
7.   abcd-general\abcd_y_lt: 						Data on IDs and weights
8.   genetics\gen_y_zygrat:							Genetic zigosity
9.   genetics\gen_y_pihat:							Genetic information
10.  abcd-general\abcd_p_demo:						Demographics
11.  abcd-general\abcd_y_lf: 						Latent variables (SES, social, perinatal)
12.  culture-environment\ce_y_dm:					Youth discrimination
13.  mental-health\mh_y_bisbas: 					Youth Behavioral Inhibition/Behavioral Approach System Scales Modified from PhenX (BIS/BAS)
14a. imaging\mri_y_smr_vol_dsk:						Structural MRI - Cortical structures
14b. imaging\mri_y_smr_vol_dsk:						Structural MRI - Subcortical structures
14c. imaging\mri_y_qc_incl:							Recommended inclusion
15.  culture-environment\ce_y_crpbi:		 		Parental Acceptance/warmth 
16.  culture-environment\ce_y_fes: 					Family conflict
17.  culture-environment\ce_y_pm:					Parental Monitoring Survey
18.  culture-environment\ce_y_mnbs:					Multidimensional neglectful behavior scale
19.  mental-health\mh_p_asr: 						Parents mental health (summary)
20.  mental-health\mh_p_asr: 						Parents mental health (items) - Not included for now
21.  mental-health\mh_y_poa: 						Positive emotions and affective well-being in past week (6, 12, and 18 months follow-up, subsample)
22.  culture-environment\ce_p_psb:					Parent prosocial behavior surveys
23.  culture-environment\ce_y_psb:					Youth prosocial behavior survey
24.  culture-environment\ce_y_wps: 					Children's will to problem solving (one-year follow-up)
25.  physical-health\ph_y_yrb:						Physical activity (3 indicators)
26.  physical-health\ph_p_saiq:						Involvement in sport activities
27.  mental-health\mh_p_ssrs: 						Parent Short Social Responsiveness Scale (Autism)
28a. mental-health\mh_p_ksads_cd:					Parent KSADS Conduct Disorder
28b. mental-health\mh_y_ksads_cd:					Youth KSADS Conduct Disorder (2-year follow-up)
29.  mental-health\mh_p_ksads_ss: 					Mental Health - Symptoms and Diagnoses Parents
30.  mental-health\mh_y_ksads_ss: 					Mental Health - Symptoms and Diagnoses Youth
31.  mental-health\mh_y_pps:						Prodromal Psychosis Scale
32.  mental-health\mh_t_bpm:	 					Brief Problems Monitor, teachers
33.  mental-health\mh_y_bpm:						Brief Problems Monitor, youth
34.  neurocognition\nc_y_nihtb:						Youth NIH TB Summary Scores (Fluid, crystallized, and total cognitive abilities)
35.  culture-environment\ce_p_nsc:					Neighboorhood safety, parents
36.  culture-environment\ce_y_nsc:					Neighboorhood safety, youth
37.  physical-health\ph_p_mhx: 						Medical History Questionnaire (MHX) [and longitudinal vars]
38.  mental-health\mh_p_abcl:						Adult Behavior Checklist
39.  culture-environment\ce_y_srpf:					School Risk and Protective Factors Survey
40.  mental-health\mh_p_ksads_ptsd: 				Parent Diagnostic Interview for DSM-5 (KSADS) Traumatic Events
41.  mental-health\mh_p_fhx:						Parent Family History Summary Scores
42.  physical-health\ph_p_meds: 				 	Medications Survey Inventory Modified from PhenX (PMP)
43.  physical-health\ph_p_sds:						Parent Sleep Disturbance Scale for Children (SDS)
44.  mental-health\mh_p_eatq:						Early Adolescent Temperament Questionnaire
45.  substance-use\su_y_sui:						Youth Substance Use Interview
46.  neurocognition\nc_y_ravlt:						Pearson Scores - Rey Delayed Recall Test
47.  imaging\mri_y_tfmr_nback_beh:					Behavioral performance measures for nBack task fMRI (working memory)
48.  mental-health\mh_p_le:							Parent Life Events
49.  mental-health\mh_y_le:							Youth Life Events
50.  culture-environment\ce_y_pbp:					Youth Peer Behavior Profile
51.  culture-environment\ce_y_pnh:					Youth Peer Network Health Protective Scaler
52.  culture-environment\ce_y_rpi:					Youth Resistance to Peer Influence
53.  mental-health\mh_y_peq:						Youth Peer Experiences Questionnaire
54.  mental-health\mh_y_cbb:						Youth Cyberbullying
55.  mental-health\mh_p_ders:						Social Development Parent Difficulties in Emotion Regulation
56.  mental-health\mh_y_erq:						Youth Emotion Regulation Questionnaire
57.  neurocognition\nc_y_est:						Youth Emotional Stroop Task (nc_y_est)
58.  neurocognition\nc_y_sit:						Social Influence Task
59.  culture-environment\ce_p_comc:					Parent PhenX Community Cohesion
60.  mental-health\mh_y_or:							Other Resilience (Close friends)
61.  linked-external-data\led_l_adi: 				Residential History Derived Scores
62.  linked-external-data\led_l_socmob:				Social Mobility (Opportunity Atlas)
63.  linked-external-data\lled_l_coi:				Child Opportunity Index
64.  linked-external-data\led_l_censusret:			Census Return (Social Capital)
65.  linked-external-data\led_l_rentmort:			Rent and mortgage statistics
66.  physical-health\ph_p_pds:						Puberty Scale 
67.  mental-health\mh_y_7up:						Symptoms of Mania
68.  mental-health\mh_y_ksads_si:					Suicidality, youth reports
69.  mental-health\mh_p_ksads_si:					Suicidality, caregiver reports
70.  mental-health\mh_p_ksads_hi:					Homocidality, caregiver reports
71.  neurocognition\nc_y_bird:						Behavioral indicator of resilience
72.  neurocognition\nc_y_wisc:						Weschsler Intelligence Scale
73.  culture-environment\ce_p_fes: 					Family conflict
74.  novel-technologies\nt_p_psq:					Screen time questionnaire, caregiver data and report
75.  novel-technologies\nt_p_stq:					Screen time questionnaire, youth data and caregiver report
76.  novel-technologies\nt_y_st:					Screen time questionnaire, youth data and report
77.  led_l_seda_demo_s:								School characteristics
78.  led_l_seda_pool_s:								School achievement (math+reading), school level
79.  led_l_seda_pool_d:  							School achievement (math+reading), district level
80.  led_l_seda_math_d: 							School achievement (math), district level
81:  led_l_seda_read_d:								School achievement (reading), district level
82.  culture-environment\ce_p_sag:					School attendance and grades, caregiver report
83.  culture-environment\ce_y_sag:					School attendance and grades, youth report
84.  covid youth:									COVID questionnaire, youth report
84.  covid caregiver:								COVID questionnaire, caregiver report
86.  Research site ID:								Research site IDs and State characteristics
87.  mri_y_qc_incl:									Inclusion of brain imaging data

**/

**************************************************************************************
**************************************** 1 *******************************************
**************************************************************************************

**** Loading data on CBCL - Summary
clear
/*import delimited "$data4p0\abcd_cbcls01.txt", varnames(1) encoding(Big5) 
** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day

*/
import delimited "$data5p1\mental-health\mh_p_cbcl.csv"

keep 	src_subject_id eventname													///
		cbcl_scr_syn_anxdep_r cbcl_scr_syn_withdep_r cbcl_scr_syn_somatic_r 		///
		cbcl_scr_syn_social_r cbcl_scr_syn_thought_r cbcl_scr_syn_attention_r 		///
		cbcl_scr_syn_rulebreak_r cbcl_scr_syn_aggressive_r cbcl_scr_syn_internal_r 	///
		cbcl_scr_syn_external_r cbcl_scr_syn_totprob_r cbcl_scr_dsm5_depress_r 		///
		cbcl_scr_dsm5_anxdisord_r cbcl_scr_dsm5_somaticpr_r cbcl_scr_dsm5_adhd_r 	///
		cbcl_scr_dsm5_opposit_r cbcl_scr_dsm5_conduct_r cbcl_scr_07_sct_r 			///
		cbcl_scr_07_ocd_r cbcl_scr_07_stress_r										///
		cbcl_scr_syn_anxdep_t cbcl_scr_syn_withdep_t cbcl_scr_syn_somatic_t 		///
		cbcl_scr_syn_social_t cbcl_scr_syn_thought_t cbcl_scr_syn_attention_t 		///
		cbcl_scr_syn_rulebreak_t cbcl_scr_syn_aggressive_t cbcl_scr_syn_internal_t 	///
		cbcl_scr_syn_external_t cbcl_scr_syn_totprob_t cbcl_scr_dsm5_depress_t 		///
		cbcl_scr_dsm5_anxdisord_t cbcl_scr_dsm5_somaticpr_t cbcl_scr_dsm5_adhd_t 	///
		cbcl_scr_dsm5_opposit_t cbcl_scr_dsm5_conduct_t cbcl_scr_07_sct_t 			///
		cbcl_scr_07_ocd_t cbcl_scr_07_stress_t
		
		
tempfile cbclsum
save `cbclsum'



**** Loading the database
clear
/*
import delimited "$data\abcd_cbcl01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
import delimited "$data5p1\mental-health\mh_p_cbcl.csv"


**************************************************************************************
**************************************** 2 *******************************************
**************************************************************************************
** CBCL profile

*******************
*Syndrome scales - 	Anxious/Depressed, Withdrawn/Depressed, Somatic Complaints -> Internalizing Behavior
*+					Social problems, Thought Problems, Attention Problems
*+					Rul-Breaking Behavior, Aggressive Behavior -> Externalizing Behavior


global anxdepress2 		"cbcl_q14_p cbcl_q29_p cbcl_q30_p cbcl_q31_p cbcl_q32_p cbcl_q33_p cbcl_q35_p cbcl_q45_p cbcl_q50_p cbcl_q52_p cbcl_q71_p cbcl_q91_p cbcl_q112_p"
global withdranx 		"cbcl_q05_p cbcl_q42_p cbcl_q65_p cbcl_q69_p cbcl_q75_p cbcl_q102_p cbcl_q103_p cbcl_q111_p"
global somatic1 		"cbcl_q47_p cbcl_q49_p cbcl_q51_p cbcl_q54_p cbcl_q56a_p cbcl_q56b_p cbcl_q56c_p cbcl_q56d_p cbcl_q56e_p cbcl_q56f_p cbcl_q56g_p"
global internalizing 	$anxdepress2 $withdranx $somatic1
global socialprob		"cbcl_q11_p cbcl_q12_p cbcl_q25_p cbcl_q27_p cbcl_q34_p cbcl_q36_p cbcl_q38_p cbcl_q48_p cbcl_q62_p cbcl_q64_p cbcl_q79_p"
global thoughtprob		"cbcl_q09_p cbcl_q18_p cbcl_q40_p cbcl_q46_p cbcl_q58_p cbcl_q59_p cbcl_q60_p cbcl_q66_p cbcl_q70_p cbcl_q76_p cbcl_q83_p cbcl_q84_p cbcl_q85_p cbcl_q92_p cbcl_q100_p"
global attentionprob	"cbcl_q01_p cbcl_q04_p cbcl_q08_p cbcl_q10_p cbcl_q13_p cbcl_q17_p cbcl_q41_p cbcl_q61_p cbcl_q78_p cbcl_q80_p"
global rulebreaker		"cbcl_q02_p cbcl_q26_p cbcl_q28_p cbcl_q39_p cbcl_q43_p cbcl_q63_p cbcl_q67_p cbcl_q72_p cbcl_q73_p cbcl_q81_p cbcl_q82_p cbcl_q90_p cbcl_q96_p cbcl_q99_p cbcl_q101_p cbcl_q105_p cbcl_q106_p"
global aggressive		"cbcl_q03_p cbcl_q16_p cbcl_q19_p cbcl_q20_p cbcl_q21_p cbcl_q22_p cbcl_q23_p cbcl_q37_p cbcl_q57_p cbcl_q68_p cbcl_q86_p cbcl_q87_p cbcl_q88_p cbcl_q89_p cbcl_q94_p cbcl_q95_p cbcl_q97_p cbcl_q104_p"
global externalizing	$rulebreaker $aggressive

****************
** CBCL Score 1: Summing across all responses
egen cbcl_anxdep1 = rowtotal($anxdepress2)
egen cbcl_witanx1 = rowtotal($withdranx)
egen cbcl_somatc1 = rowtotal($somatic1)
egen cbcl_intern1 = rowtotal($internalizing)
egen cbcl_social1 = rowtotal($socialprob)
egen cbcl_though1 = rowtotal($thoughtprob)
egen cbcl_attent1 = rowtotal($attentionprob)
egen cbcl_rulebr1 = rowtotal($rulebreaker)
egen cbcl_aggres1 = rowtotal($aggressive)
egen cbcl_extern1 = rowtotal($externalizing)

****************
** CBCL Score 2: Summing across dichotomized responses (Not true = 0; often or sometimes true = 1)
foreach v of varlist $anxdepress2 $withdranx $somatic1 $socialprob $thoughtprob $attentionprob $rulebreaker $aggressive {
	rename `v' `v'temp
	gen `v' = cond(`v'temp>=1,1,0)
}

egen cbcl_anxdep2 = rowtotal($anxdepress2)
egen cbcl_witanx2 = rowtotal($withdranx)
egen cbcl_somatc2 = rowtotal($somatic1)
egen cbcl_intern2 = rowtotal($internalizing)
egen cbcl_social2 = rowtotal($socialprob)
egen cbcl_though2 = rowtotal($thoughtprob)
egen cbcl_attent2 = rowtotal($attentionprob)
egen cbcl_rulebr2 = rowtotal($rulebreaker)
egen cbcl_aggres2 = rowtotal($aggressive)
egen cbcl_extern2 = rowtotal($externalizing)

foreach v of varlist $anxdepress2 $withdranx $somatic1 $socialprob $thoughtprob $attentionprob $rulebreaker $aggressive {
	drop `v'
	rename `v'temp `v'
}

****************
** CBCL Score 3: Principal Component analysis
pca $anxdepress2
predict cbcl_anxdep3
pca $withdranx
predict cbcl_witanx3
pca $somatc1
predict cbcl_somatc3
pca $internalizing
predict cbcl_intern3
pca $socialprob
predict cbcl_social3
pca $thoughtprob
predict cbcl_though3
pca $attentionprob
predict cbcl_attent3
pca $rulebreaker
predict cbcl_rulebr3
pca $aggressive
predict cbcl_aggres3
pca $externalizing
predict cbcl_extern3

****************
** DMS Score 4: Percentile of CBCL Score 1

foreach v in cbcl_anxdep cbcl_witanx cbcl_somatc cbcl_intern cbcl_social cbcl_though cbcl_attent cbcl_rulebr cbcl_aggres cbcl_extern {
	xtile 	`v'4a = `v'1 if eventname=="baseline_year_1_arm_1", nq(100) 
	xtile 	`v'4b = `v'1 if eventname=="1_year_follow_up_y_arm_1", nq(100) 
	xtile 	`v'4c = `v'1 if eventname=="2_year_follow_up_y_arm_1", nq(100) 
	gen 	`v'4 = `v'4a if eventname=="baseline_year_1_arm_1"
	replace	`v'4 = `v'4b if eventname=="1_year_follow_up_y_arm_1"
	replace	`v'4 = `v'4c if eventname=="2_year_follow_up_y_arm_1"
	drop `v'4a `v'4b `v'4c
}

**************************************************************************************
**************************************************************************************
** DSM Oriented Scales - Depressive Problems, Anxiety Problems, Somatic Problems,
*+						Oppositional Defiant Problems, Conduct Problems

global depressive 		"cbcl_q05_p cbcl_q14_p cbcl_q18_p cbcl_q24_p cbcl_q35_p cbcl_q52_p cbcl_q54_p cbcl_q76_p cbcl_q77_p cbcl_q91_p cbcl_q100_p cbcl_q102_p cbcl_q103_p"
global anxious 			"cbcl_q11_p cbcl_q29_p cbcl_q30_p cbcl_q31_p cbcl_q45_p cbcl_q47_p cbcl_q50_p cbcl_q71_p cbcl_q112_p"
global somatic2 		"cbcl_q56a_p cbcl_q56b_p cbcl_q56c_p cbcl_q56d_p cbcl_q56e_p cbcl_q56f_p cbcl_q56g_p"
global attentiondef 	"cbcl_q04_p cbcl_q08_p cbcl_q10_p cbcl_q41_p cbcl_q78_p cbcl_q93_p cbcl_q104_p"
global oppositional 	"cbcl_q03_p cbcl_q22_p cbcl_q23_p cbcl_q86_p cbcl_q95_p"
global conductprob		"cbcl_q15_p cbcl_q16_p cbcl_q21_p cbcl_q26_p cbcl_q28_p cbcl_q37_p cbcl_q39_p cbcl_q43_p cbcl_q57_p cbcl_q67_p cbcl_q72_p cbcl_q81_p cbcl_q82_p cbcl_q90_p cbcl_q97_p cbcl_q101_p cbcl_q106_p"

****************
** DMS Score 1: Summing across all responses
egen dms_depres1 = rowtotal($depressive)
egen dms_anxiou1 = rowtotal($anxious)
egen dms_somati1 = rowtotal($somatic2)
egen dms_attent1 = rowtotal($attentiondef)
egen dms_opposi1 = rowtotal($oppositional)
egen dms_conduc1 = rowtotal($conductprob)

****************
** DMS Score 2: Summing across dichotomized responses (Not true = 0; often or sometimes true = 1)
foreach v of varlist $depressive $anxious $somatic2 $attentiondef $oppositional $conductprob {
	rename `v' `v'temp
	gen `v' = cond(`v'temp>=1,1,0)
}

egen dms_depres2 = rowtotal($depressive)
egen dms_anxiou2 = rowtotal($anxious)
egen dms_somati2 = rowtotal($somatic2)
egen dms_attent2 = rowtotal($attentiondef)
egen dms_opposi2 = rowtotal($oppositional)
egen dms_conduc2 = rowtotal($conductprob)

foreach v of varlist $depressive $anxious $somatic2 $attentiondef $oppositional $conductprob {
	drop `v'
	rename `v'temp `v'
}

****************
** DMS Score 3: Principal Component analysis
pca $depressive 
predict dms_depres3
pca $anxious 
predict dms_anxiou3
pca $somatic2 
predict dms_somati3
pca $attentiondef 
predict dms_attent3
pca $oppositional 
predict dms_opposi3
pca $conductprob
predict dms_conduc3

****************
** DMS Score 4: Percentile of DMS Score 1 (Check do-file accountability paper - PSU)

foreach v in dms_depres dms_anxiou dms_somati dms_attent dms_opposi dms_conduc {
	xtile 	`v'4a = `v'1 if eventname=="baseline_year_1_arm_1", nq(100) 
	xtile 	`v'4b = `v'1 if eventname=="1_year_follow_up_y_arm_1", nq(100) 
	xtile 	`v'4c = `v'1 if eventname=="2_year_follow_up_y_arm_1", nq(100) 
	gen 	`v'4 = `v'4a if eventname=="baseline_year_1_arm_1"
	replace	`v'4 = `v'4b if eventname=="1_year_follow_up_y_arm_1"
	replace	`v'4 = `v'4c if eventname=="2_year_follow_up_y_arm_1"
	drop `v'4a `v'4b `v'4c
}

*rename sex gender

keep src_subject_id eventname														///
		$socialprob $rulebreaker $aggressive $oppositional $conductprob 			///
		$depressive $anxious $somatic2 $attentiondef $oppositional $conductprob		///
		dms_depres1 dms_anxiou1 dms_somati1 dms_attent1 dms_opposi1 dms_conduc1		///
		dms_depres2 dms_anxiou2 dms_somati2 dms_attent2 dms_opposi2 dms_conduc2		///
		cbcl_anxdep1 cbcl_witanx1 cbcl_somatc1 $anxdepress2 $withdranx $somatic1 	///
		$internalizing cbcl_anxdep2 cbcl_witanx2 cbcl_somatc2 cbcl_q01_p

		
tempfile cbclsc
save `cbclsc'


**************************************************************************************
**************************************** 3 *******************************************
**************************************************************************************
clear
/*
import delimited "$data\abcd_yksad01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\mental-health\mh_y_ksads_bg.csv"

keep 	src_subject_id eventname											///
		kbi_y_sex_orient kbi_y_sex_orient_probs ksads_bully_raw_26 			///
		kbi_y_trans_id kbi_y_trans_prob kbi_y_grade_repeat 					///
		kbi_y_drop_in_grades kbi_y_det_susp kbi_gender kbi_grades_repeated*
		
tempfile bullyy
save `bullyy'
		
**************************************************************************************
**************************************** 4 *******************************************
**************************************************************************************
**** Loading data on problems with bullyings, parents
/*clear
import delimited "$data\dibf01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\mental-health\mh_p_ksads_bg.csv"

keep 	src_subject_id eventname													///
		kbi_p_c_bully kbi_p_conflict kbi_p_c_age_services 							///
		kbi_p_c_age_services_bl_dk kbi_p_c_best_friend kbi_p_c_best_friend_len		///
		kbi_p_c_gay kbi_p_c_gay_problems kbi_p_c_trans kbi_p_c_trans_problems		///
		kbi_p_c_reg_friend_group_len kbi_ss_c_mental_health_p  						///
		kbi_p_c_school_setting kbi_p_c_spec_serv___1 kbi_p_c_spec_serv___2			///
		kbi_p_c_spec_serv___3 kbi_p_c_spec_serv___4 kbi_p_c_spec_serv___5			///
		kbi_p_c_spec_serv___6 kbi_p_c_spec_serv___7 kbi_p_c_spec_serv___8			///
		kbi_p_c_spec_serv___9 kbi_p_c_spec_serv___10 kbi_p_c_scheck7				///
		kbi_p_c_school_setting kbi_p_how_well_c_school kbi_p_grades_in_school 		///
		kbi_p_c_drop_in_grades
		
tempfile bullyp
save `bullyp'


**************************************************************************************
**************************************** 5 *******************************************
**************************************************************************************
**** Loading data on sexual orientation, youth
clear
import delimited "$data5p1\gender-identity-sexual-health\gish_y_sex.csv"

keep 	src_subject_id eventname											///
		gish2_had_bf_gf gish2_want_bf_gf gish2_sexual_attract gish2_girls 	///
		gish2_boys gish2_nonbinary_1 gish2_how_old gish2_spend_time			///
		gish2_want_type_rd
		
		
tempfile genderID
save `genderID'


**************************************************************************************
**************************************** 6 *******************************************
**************************************************************************************
**** Loading data on Youth Anthropometrics Modified From PhenX
/*clear
import delimited "$data\abcd_ant01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\physical-health\ph_y_anthro.csv"

gen bodyw_kg 	=  anthroweightcalc/2.205
gen bodyh_m 	=  anthroheightcalc/39.37

gen 	bmi =  bodyw_kg/(bodyh_m^2)
replace bmi = . if bmi>55

* Definition of obsesity for Children and teens: https://www.cdc.gov/obesity/childhood/defining.html

sum bmi if eventname=="baseline_year_1_arm_1", detail
_pctile bmi if eventname=="baseline_year_1_arm_1", percentiles(5,85,95)

gen undr_weight = cond(bmi<=r(r1),1,0) 					if eventname=="baseline_year_1_arm_1"
gen norm_weight = cond(bmi>r(r1) & bmi<=r(r2),1,0)		if eventname=="baseline_year_1_arm_1"
gen over_weight = cond(bmi>r(r2) & bmi<r(r3),1,0)		if eventname=="baseline_year_1_arm_1"
gen obes_weight = cond(bmi>=r(r3) & bmi<.,1,0)			if eventname=="baseline_year_1_arm_1"

sum bmi if eventname=="1_year_follow_up_y_arm_1", detail
_pctile bmi if eventname=="1_year_follow_up_y_arm_1", percentiles(5,85,95)

replace undr_weight = cond(bmi<=r(r1),1,0) 					if eventname=="1_year_follow_up_y_arm_1"
replace norm_weight = cond(bmi>r(r1) & bmi<=r(r2),1,0)		if eventname=="1_year_follow_up_y_arm_1"
replace over_weight = cond(bmi>r(r2) & bmi<r(r3),1,0)		if eventname=="1_year_follow_up_y_arm_1"
replace obes_weight = cond(bmi>=r(r3) & bmi<.,1,0)			if eventname=="1_year_follow_up_y_arm_1"

sum bmi if eventname=="2_year_follow_up_y_arm_1", detail
_pctile bmi if eventname=="2_year_follow_up_y_arm_1", percentiles(5,85,95)

replace undr_weight = cond(bmi<=r(r1),1,0) 					if eventname=="2_year_follow_up_y_arm_1"
replace norm_weight = cond(bmi>r(r1) & bmi<=r(r2),1,0)		if eventname=="2_year_follow_up_y_arm_1"
replace over_weight = cond(bmi>r(r2) & bmi<r(r3),1,0)		if eventname=="2_year_follow_up_y_arm_1"
replace obes_weight = cond(bmi>=r(r3) & bmi<.,1,0)			if eventname=="2_year_follow_up_y_arm_1"

sum bmi if eventname=="3_year_follow_up_y_arm_1", detail
_pctile bmi if eventname=="3_year_follow_up_y_arm_1", percentiles(5,85,95)

replace undr_weight = cond(bmi<=r(r1),1,0) 					if eventname=="3_year_follow_up_y_arm_1"
replace norm_weight = cond(bmi>r(r1) & bmi<=r(r2),1,0)		if eventname=="3_year_follow_up_y_arm_1"
replace over_weight = cond(bmi>r(r2) & bmi<r(r3),1,0)		if eventname=="3_year_follow_up_y_arm_1"
replace obes_weight = cond(bmi>=r(r3) & bmi<.,1,0)			if eventname=="3_year_follow_up_y_arm_1"

sum bmi if eventname=="4_year_follow_up_y_arm_1", detail
_pctile bmi if eventname=="4_year_follow_up_y_arm_1", percentiles(5,85,95)

replace undr_weight = cond(bmi<=r(r1),1,0) 					if eventname=="4_year_follow_up_y_arm_1"
replace norm_weight = cond(bmi>r(r1) & bmi<=r(r2),1,0)		if eventname=="4_year_follow_up_y_arm_1"
replace over_weight = cond(bmi>r(r2) & bmi<r(r3),1,0)		if eventname=="4_year_follow_up_y_arm_1"
replace obes_weight = cond(bmi>=r(r3) & bmi<.,1,0)			if eventname=="4_year_follow_up_y_arm_1"


keep 	src_subject_id eventname													///
		anthroheightcalc anthroweightcalc anthro_waist_cm undr_weight norm_weight	///
		over_weight obes_weight bodyw_kg bodyh_m bmi
		
tempfile anthro
save `anthro'


**************************************************************************************
**************************************** 7 *******************************************
**************************************************************************************
**** Loading data on Weights, site ID, and other IDs
/*clear
import delimited "$data\acspsw03.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\abcd-general\abcd_y_lt.csv"

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 
* Usually, first day of school: September 10
* Usually, last day of school: June 20

gen 		summer=1 if interview_month==6 & interview_day>=20
replace 	summer=1 if interview_month==7 | interview_month==8
replace 	summer=1 if interview_month==9 & interview_day<=10
replace 	summer=0 if summer==.

keep 	src_subject_id eventname 													///
		site_id_l rel_family_id rel_birth_id school_id district_id interview_date 	///
		interview_age visit_type
		
tempfile weight
save `weight'


**************************************************************************************
**************************************** 8 *******************************************
**************************************************************************************
**** Loading data on genetics, zigosity
clear
import delimited "$data5p1\genetics\gen_y_zygrat.csv"

keep 	src_subject_id eventname zyg_ss_fam_no-zyg_ss_twin2_pguid
		
tempfile zigosity
save `zigosity'


**************************************************************************************
**************************************** 9 *******************************************
**************************************************************************************
**** Loading data on genetics

clear
import delimited "$data5p1\genetics\gen_y_pihat.csv"

keep 	src_subject_id eventname 												///
		rel_group_id rel_ingroup_order rel_relationship rel_same_sex 			///
		genetic_pi_hat_1 genetic_pi_hat_2 genetic_pi_hat_3 genetic_pi_hat_4 	///
		genetic_zygosity_status_1 genetic_zygosity_status_2 					///
		genetic_zygosity_status_3 genetic_zygosity_status_4 					///
		genetic_paired_subjectid_1 genetic_paired_subjectid_2 					///
		genetic_paired_subjectid_3 genetic_paired_subjectid_4
		
tempfile genetics
save `genetics'


**************************************************************************************
**************************************** 10 ******************************************
**************************************************************************************
**** Loading data on Demographics
/*clear
import delimited "$data\abcd_lt01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\abcd-general\abcd_p_demo.csv"

*******************
** Foreigners
gen 	foreign_p = 0 if demo_prnt_16==0 | demo_prnt_origin_v2==189
replace foreign_p = 1 if demo_prnt_origin_v2!=189 & demo_prnt_origin_v2!=777 & demo_prnt_origin_v2!=999 & demo_prnt_origin_v2!=.  

gen 	foreign_y = 0 if demo_origin_v2==189
replace foreign_y = 1 if demo_origin_v2!=189 & demo_origin_v2!=777 & demo_origin_v2!=999 & demo_origin_v2!=. 

gen country_op = .
gen country_oy = .

* African Origin
foreach ccode in 3 5 19 23 27 28 29 31 33 34 38 39 40 41 48 52 54 55 57 62 65 69 70 87 95 96 97 102 103 106 109 110 117 118 120 126 127 144 147 149 151 152 157 158 160 166 168 174 177 180 184 196 197 {
	replace country_op = 1 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 1 if demo_origin_v2==`ccode'
}

* South Asia Origin
foreach ccode in 14 20 76 122 162 {
	replace country_op = 2 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 2 if demo_origin_v2==`ccode'
}
* East Asia Origin
*Timor-Leste considered Asia
foreach ccode in 25 30 36 77 92 104 105 112 115 119 128 132 138 153 159 172 175 194 {
	replace country_op = 3 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 3 if demo_origin_v2==`ccode'
}

* Middle East Origin
* Georgia is not part of the UE. They are negotiating though
* Israel in ME
foreach ccode in 1 8 11 13 78 79 81 85 86 90 91 94 130 131 133 148 171 173 182 187 190 195 {
	replace country_op = 4 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 4 if demo_origin_v2==`ccode'
}

* Central America Origin
foreach ccode in 6 12 15 18 41 44 49 50 53 67 68 72 73 83 108 125 134 163 164 165 179 {
	replace country_op = 5 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 5 if demo_origin_v2==`ccode'
}

* South America Origin
* Guyana/Suriname considered South America, even though culturally they are more similar to the Caribe
foreach ccode in 7 21 24 35 37 51 71 136 137 167 188 193 198 {
	replace country_op = 6 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 6 if demo_origin_v2==`ccode'
}

* Oceania Origin
foreach ccode in 9 58 88 121 124 135 145 156 178 183 191 {
	replace country_op = 7 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 7 if demo_origin_v2==`ccode'
}

* Eastern Europe Origin
* Cyprus considered culturally and politically European
foreach ccode in 2 16 22 26 43 45 46 74 84 89 93 98 99 101 113 116 150 154 155 181 186 {
	replace country_op = 8 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 8 if demo_origin_v2==`ccode'
}
* Western Europe Origin
foreach ccode in 4 10 17 60 61 64 66 80 82 100 107 114 123 139 140 142 143 146 161 170 185 192 {
	replace country_op = 9 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 9 if demo_origin_v2==`ccode'
}
* Northern Europe Origin
foreach ccode in 47 56 59 75 129 169 {
	replace country_op = 10 if demo_prnt_origin_v2==`ccode'
	replace country_oy = 10 if demo_origin_v2==`ccode'
}
* Canadian origin
replace country_op = 11 if demo_prnt_origin_v2==32
replace country_oy = 11 if demo_origin_v2==32

* Mexican origin
replace country_op = 12 if demo_prnt_origin_v2==111
replace country_oy = 12 if demo_origin_v2==111

label define country_op 1 "Africa" 2 "South Asia" 3 "East Asia" 4 "Middle East" 5 "Central America" 6 "South America" 7 "Oceania" 8 "Eastern Europe" 9 "Western Europe" 10 "North Europe" 11 "Canada" 12 "Mexico"
label values country_op country_op 
label define country_oy 1 "Africa" 2 "South Asia" 3 "East Asia" 4 "Middle East" 5 "Central America" 6 "South America" 7 "Oceania" 8 "Eastern Europe" 9 "Western Europe" 10 "North Europe" 11 "Canada" 12 "Mexico"
label values country_oy country_oy 


*** Race
egen aux_race = rowtotal(demo_race_a_p___10 demo_race_a_p___11 demo_race_a_p___12 demo_race_a_p___13 demo_race_a_p___14 demo_race_a_p___15 demo_race_a_p___16 demo_race_a_p___17 demo_race_a_p___18 demo_race_a_p___19 demo_race_a_p___20 demo_race_a_p___21 demo_race_a_p___22 demo_race_a_p___23 demo_race_a_p___24 demo_race_a_p___25),m
gen mix_race = cond(aux_race>1 & aux_race<., 1, 0) if aux_race!=.

*** Marital status
gen 	married = 1 if demo_prnt_marital_v2==1 & eventname=="baseline_year_1_arm_1"
replace married = 1 if demo_prnt_marital_v2_l==1 & eventname!="baseline_year_1_arm_1"
replace married = 2 if demo_prnt_marital_v2==2 & eventname=="baseline_year_1_arm_1"
replace married = 2 if demo_prnt_marital_v2_l==2 & eventname!="baseline_year_1_arm_1"
replace married = 3 if demo_prnt_marital_v2==3 & eventname=="baseline_year_1_arm_1"
replace married = 3 if demo_prnt_marital_v2_l==3 & eventname!="baseline_year_1_arm_1"
replace married = 4 if demo_prnt_marital_v2==4 & eventname=="baseline_year_1_arm_1"
replace married = 4 if demo_prnt_marital_v2_l==4 & eventname!="baseline_year_1_arm_1"
replace married = 5 if demo_prnt_marital_v2==5 & eventname=="baseline_year_1_arm_1"
replace married = 5 if demo_prnt_marital_v2_l==5 & eventname!="baseline_year_1_arm_1"
replace married = 6 if demo_prnt_marital_v2==6 & eventname=="baseline_year_1_arm_1"
replace married = 6 if demo_prnt_marital_v2_l==6 & eventname!="baseline_year_1_arm_1"

label define married 1 "Married" 2 "Widowed" 3 "Divorced" 4 "Separated" 5 "Never married" 6 "Living with partner" 
label values married married
*777 "Refused to answer"

*** Education level primary caregiver
gen 	educ_lev = 1 if demo_prnt_ed_v2==0 & eventname=="baseline_year_1_arm_1"
replace educ_lev = 1 if demo_prnt_ed_v2_l==0 & eventname=="1_year_follow_up_y_arm_1"
replace educ_lev = 1 if demo_prnt_ed_v2_2yr_l==0 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_lev = 2 if demo_prnt_ed_v2>0 & demo_prnt_ed_v2<=14 & eventname=="baseline_year_1_arm_1"
replace educ_lev = 2 if demo_prnt_ed_v2_l>0 & demo_prnt_ed_v2_l<=14 & eventname=="1_year_follow_up_y_arm_1"
replace educ_lev = 2 if ((demo_prnt_ed_v2_2yr_l>0 & demo_prnt_ed_v2_2yr_l<=14) | (demo_prnt_ed_v2_2yr_l>=22 & demo_prnt_ed_v2_2yr_l<=23)) & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_lev = 3 if demo_prnt_ed_v2==15 & eventname=="baseline_year_1_arm_1"
replace educ_lev = 3 if demo_prnt_ed_v2_l==15 & eventname=="1_year_follow_up_y_arm_1"
replace educ_lev = 3 if demo_prnt_ed_v2_2yr_l==15 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_lev = 4 if demo_prnt_ed_v2>15 & demo_prnt_ed_v2<=17 & eventname=="baseline_year_1_arm_1"
replace educ_lev = 4 if demo_prnt_ed_v2_l>15 & demo_prnt_ed_v2_l<=17 & eventname=="1_year_follow_up_y_arm_1"
replace educ_lev = 4 if demo_prnt_ed_v2_2yr_l>15 & demo_prnt_ed_v2_2yr_l<=17 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_lev = 5 if demo_prnt_ed_v2==18 & eventname=="baseline_year_1_arm_1"
replace educ_lev = 5 if demo_prnt_ed_v2_l==18 & eventname=="1_year_follow_up_y_arm_1"
replace educ_lev = 5 if demo_prnt_ed_v2_2yr_l==18 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_lev = 6 if demo_prnt_ed_v2>18 & demo_prnt_ed_v2<=21 & eventname=="baseline_year_1_arm_1"
replace educ_lev = 6 if demo_prnt_ed_v2_l>18 & demo_prnt_ed_v2_l<=21 & eventname=="1_year_follow_up_y_arm_1"
replace educ_lev = 6 if demo_prnt_ed_v2_2yr_l>18 & demo_prnt_ed_v2_2yr_l<=21 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

label define educ_lev 1 "No school" 2 "High school" 3 "Some College" 4 "Associate Degree" 5 "College" 6 "Masters or more"
label values educ_lev educ_lev

* Education level partner
gen 	educ_levpt = 1 if demo_prtnr_ed_v2==0 & eventname=="baseline_year_1_arm_1"
replace educ_levpt = 1 if demo_prtnr_ed_v2_l==0 & eventname=="1_year_follow_up_y_arm_1"
replace educ_levpt = 1 if demo_prtnr_ed_v2_2yr_l==0 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_levpt = 2 if demo_prtnr_ed_v2>0 & demo_prtnr_ed_v2<=14 & eventname=="baseline_year_1_arm_1"
replace educ_levpt = 2 if demo_prtnr_ed_v2_l>0 & demo_prtnr_ed_v2_l<=14 & eventname=="1_year_follow_up_y_arm_1"
replace educ_levpt = 2 if ((demo_prtnr_ed_v2_2yr_l>0 & demo_prtnr_ed_v2_2yr_l<=14) | (demo_prtnr_ed_v2_2yr_l>=22 & demo_prtnr_ed_v2_2yr_l<=23)) & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_levpt = 3 if demo_prtnr_ed_v2==15 & eventname=="baseline_year_1_arm_1"
replace educ_levpt = 3 if demo_prtnr_ed_v2_l==15 & eventname=="1_year_follow_up_y_arm_1"
replace educ_levpt = 3 if demo_prtnr_ed_v2_2yr_l==15 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_levpt = 4 if demo_prtnr_ed_v2>15 & demo_prtnr_ed_v2<=17 & eventname=="baseline_year_1_arm_1"
replace educ_levpt = 4 if demo_prtnr_ed_v2_l>15 & demo_prtnr_ed_v2_l<=17 & eventname=="1_year_follow_up_y_arm_1"
replace educ_levpt = 4 if demo_prtnr_ed_v2_2yr_l>15 & demo_prtnr_ed_v2_2yr_l<=17 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_levpt = 5 if demo_prtnr_ed_v2==18 & eventname=="baseline_year_1_arm_1"
replace educ_levpt = 5 if demo_prtnr_ed_v2_l==18 & eventname=="1_year_follow_up_y_arm_1"
replace educ_levpt = 5 if demo_prtnr_ed_v2_2yr_l==18 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

replace educ_levpt = 6 if demo_prtnr_ed_v2>18 & demo_prtnr_ed_v2<=21 & eventname=="baseline_year_1_arm_1"
replace educ_levpt = 6 if demo_prtnr_ed_v2_l>18 & demo_prtnr_ed_v2_l<=21 & eventname=="1_year_follow_up_y_arm_1"
replace educ_levpt = 6 if demo_prtnr_ed_v2_2yr_l>18 & demo_prtnr_ed_v2_2yr_l<=21 & (eventname!="baseline_year_1_arm_1" | eventname!="1_year_follow_up_y_arm_1")

label define educ_levpt 1 "No school" 2 "High school" 3 "Some College" 4 "Associate Degree" 5 "College" 6 "Masters or more"
label values educ_levpt educ_levpt

*** Income
gen 	fam_income = 2500 if demo_comb_income_v2==1 & eventname=="baseline_year_1_arm_1"
replace fam_income = 2500 if demo_comb_income_v2_l==1 & eventname!="baseline_year_1_arm_1"
replace fam_income = 8500 if demo_comb_income_v2==2 & eventname=="baseline_year_1_arm_1"
replace fam_income = 8500 if demo_comb_income_v2_l==2 & eventname!="baseline_year_1_arm_1"
replace fam_income = 14000 if demo_comb_income_v2==3 & eventname=="baseline_year_1_arm_1"
replace fam_income = 14000 if demo_comb_income_v2_l==3 & eventname!="baseline_year_1_arm_1"
replace fam_income = 20500 if demo_comb_income_v2==4 & eventname=="baseline_year_1_arm_1"
replace fam_income = 20500 if demo_comb_income_v2_l==4 & eventname!="baseline_year_1_arm_1"
replace fam_income = 30000 if demo_comb_income_v2==5 & eventname=="baseline_year_1_arm_1"
replace fam_income = 30000 if demo_comb_income_v2_l==5 & eventname!="baseline_year_1_arm_1"
replace fam_income = 42500 if demo_comb_income_v2==6 & eventname=="baseline_year_1_arm_1"
replace fam_income = 42500 if demo_comb_income_v2_l==6 & eventname!="baseline_year_1_arm_1"
replace fam_income = 62500 if demo_comb_income_v2==7 & eventname=="baseline_year_1_arm_1"
replace fam_income = 62500 if demo_comb_income_v2_l==7 & eventname!="baseline_year_1_arm_1"
replace fam_income = 87500 if demo_comb_income_v2==8 & eventname=="baseline_year_1_arm_1"
replace fam_income = 87500 if demo_comb_income_v2_l==8 & eventname!="baseline_year_1_arm_1"
replace fam_income = 150000 if demo_comb_income_v2==9 & eventname=="baseline_year_1_arm_1"
replace fam_income = 150000 if demo_comb_income_v2_l==9 & eventname!="baseline_year_1_arm_1"
replace fam_income = 250000 if demo_comb_income_v2==10 & eventname=="baseline_year_1_arm_1"
replace fam_income = 250000 if demo_comb_income_v2_l==10 & eventname!="baseline_year_1_arm_1"

replace demo_roster_v2=15 if demo_roster_v2>15 & demo_roster_v2<.
replace demo_roster_v2_l=15 if demo_roster_v2_l>15 & demo_roster_v2_l<.

gen 	n_fam = demo_roster_v2 if eventname=="baseline_year_1_arm_1"
replace n_fam = demo_roster_v2_l if eventname!="baseline_year_1_arm_1"
*if demo_roster_v2 <=15

gen fam_income_pc = fam_income/n_fam 

local i = 1
levelsof eventname, local(waves)
foreach w of local waves{
	xtile q_fam_income_pc_`i' = fam_income_pc if eventname=="`w'", nq(5)
	xtile d_fam_income_pc_`i' = fam_income_pc if eventname=="`w'", nq(10)
	
local i = `i'+1	
}
label define demo_comb_income_v2 1 "Less than 5,000" 2 "5,000 through 11,999" 3 "12,000 through 15,999" 4 "16,000 through 24,999" 5 "$25,000 through $34,999" 6 "35,000 through 49,999" 7 "50,000 through 74,999" 8 "$75,000 through $99,999" 9 "100,000 through $199,999" 10 "$200,000 and greater"
label values demo_comb_income_v2 demo_comb_income_v2 

*** Make ends meet
foreach mem in demo_fam_exp1_v2 demo_fam_exp2_v2 demo_fam_exp3_v2 demo_fam_exp4_v2 demo_fam_exp5_v2 demo_fam_exp6_v2 demo_fam_exp7_v2 {
	replace `mem'=. if `mem'==777
}
egen mem1 = rowtotal(demo_fam_exp1_v2 demo_fam_exp2_v2 demo_fam_exp3_v2 demo_fam_exp4_v2 demo_fam_exp5_v2 demo_fam_exp6_v2 demo_fam_exp7_v2)
gen mem2 = cond(demo_fam_exp1_v2==1 | demo_fam_exp2_v2==1 | demo_fam_exp3_v2==1 | demo_fam_exp4_v2==1 | demo_fam_exp5_v2==1 | demo_fam_exp6_v2==1 | demo_fam_exp7_v2==1, 1, 0)


*** Family variables, baseline
label define demo_prim 1 "Childs Biological Mother" 2 "Childs Biological Father" 3 "Adoptive Parent" 4 "Childs Custodial" 5 "Other"
label values demo_prim demo_prim 

gen 	both_bioparents = 1 if (demo_prim==1 | demo_prim==2) & (fam_roster_2c_v2==1 | fam_roster_3c_v2==1 | fam_roster_4c_v2==1 | fam_roster_5c_v2==1 | fam_roster_6c_v2==1 | fam_roster_7c_v2==1 | fam_roster_8c_v2==1 | fam_roster_9c_v2==1 | fam_roster_10c_v2==1 | fam_roster_11c_v2==1 | fam_roster_12c_v2==1 | fam_roster_13c_v2==1 | fam_roster_14c_v2==1 | fam_roster_15c_v2==1)
replace both_bioparents = 0 if both_bioparents!=1 & fam_roster_2c_v2!=.

gen 	both_adoparents = 1 if (demo_prim==3) & (fam_roster_2c_v2==1 | fam_roster_3c_v2==1 | fam_roster_4c_v2==1 | fam_roster_5c_v2==1 | fam_roster_6c_v2==1 | fam_roster_7c_v2==1 | fam_roster_8c_v2==1 | fam_roster_9c_v2==1 | fam_roster_10c_v2==1 | fam_roster_11c_v2==1 | fam_roster_12c_v2==1 | fam_roster_13c_v2==1 | fam_roster_14c_v2==1 | fam_roster_15c_v2==1)
replace both_adoparents = 0 if both_adoparents!=1 & fam_roster_2c_v2!=.

gen 	both_cusparents = 1 if (demo_prim==4) & (fam_roster_2c_v2==1 | fam_roster_3c_v2==1 | fam_roster_4c_v2==1 | fam_roster_5c_v2==1 | fam_roster_6c_v2==1 | fam_roster_7c_v2==1 | fam_roster_8c_v2==1 | fam_roster_9c_v2==1 | fam_roster_10c_v2==1 | fam_roster_11c_v2==1 | fam_roster_12c_v2==1 | fam_roster_13c_v2==1 | fam_roster_14c_v2==1 | fam_roster_15c_v2==1)
replace both_cusparents = 0 if both_cusparents!=1 & fam_roster_2c_v2!=.

gen 	both_othparents = 1 if (demo_prim==4) & (fam_roster_2c_v2==1 | fam_roster_3c_v2==1 | fam_roster_4c_v2==1 | fam_roster_5c_v2==1 | fam_roster_6c_v2==1 | fam_roster_7c_v2==1 | fam_roster_8c_v2==1 | fam_roster_9c_v2==1 | fam_roster_10c_v2==1 | fam_roster_11c_v2==1 | fam_roster_12c_v2==1 | fam_roster_13c_v2==1 | fam_roster_14c_v2==1 | fam_roster_15c_v2==1)
replace both_othparents = 0 if both_cusparents!=1 & fam_roster_2c_v2!=.

gen 	one_bioparent = 1 if (demo_prim==1 | demo_prim==2) & (fam_roster_2c_v2!=1 & fam_roster_3c_v2!=1 & fam_roster_4c_v2!=1 & fam_roster_5c_v2!=1 & fam_roster_6c_v2!=1 & fam_roster_7c_v2!=1 & fam_roster_8c_v2!=1 & fam_roster_9c_v2!=1 & fam_roster_10c_v2!=1 & fam_roster_11c_v2!=1 & fam_roster_12c_v2!=1 & fam_roster_13c_v2!=1 & fam_roster_14c_v2!=1 & fam_roster_15c_v2!=1)
replace one_bioparent = 0 if one_bioparent!=1 & fam_roster_2c_v2!=.

gen 	one_adoparent = 1 if (demo_prim==3) & (fam_roster_2c_v2!=1 & fam_roster_3c_v2!=1 & fam_roster_4c_v2!=1 & fam_roster_5c_v2!=1 & fam_roster_6c_v2!=1 & fam_roster_7c_v2!=1 & fam_roster_8c_v2!=1 & fam_roster_9c_v2!=1 & fam_roster_10c_v2!=1 & fam_roster_11c_v2!=1 & fam_roster_12c_v2!=1 & fam_roster_13c_v2!=1 & fam_roster_14c_v2!=1 & fam_roster_15c_v2!=1)
replace one_adoparent = 0 if one_adoparent!=1 & fam_roster_2c_v2!=.

gen 	one_cusparent = 1 if (demo_prim==4) & (fam_roster_2c_v2!=1 & fam_roster_3c_v2!=1 & fam_roster_4c_v2!=1 & fam_roster_5c_v2!=1 & fam_roster_6c_v2!=1 & fam_roster_7c_v2!=1 & fam_roster_8c_v2!=1 & fam_roster_9c_v2!=1 & fam_roster_10c_v2!=1 & fam_roster_11c_v2!=1 & fam_roster_12c_v2!=1 & fam_roster_13c_v2!=1 & fam_roster_14c_v2!=1 & fam_roster_15c_v2!=1)
replace one_cusparent = 0 if one_cusparent!=1 & fam_roster_2c_v2!=.

gen 	one_othparent = 1 if (demo_prim==4) & (fam_roster_2c_v2!=1 & fam_roster_3c_v2!=1 & fam_roster_4c_v2!=1 & fam_roster_5c_v2!=1 & fam_roster_6c_v2!=1 & fam_roster_7c_v2!=1 & fam_roster_8c_v2!=1 & fam_roster_9c_v2!=1 & fam_roster_10c_v2!=1 & fam_roster_11c_v2!=1 & fam_roster_12c_v2!=1 & fam_roster_13c_v2!=1 & fam_roster_14c_v2!=1 & fam_roster_15c_v2!=1)
replace one_othparent = 0 if one_othparent!=1 & fam_roster_2c_v2!=.

local n = 1
foreach sib in fam_roster_2c_v2 fam_roster_3c_v2 fam_roster_4c_v2 fam_roster_5c_v2 fam_roster_6c_v2 fam_roster_7c_v2 fam_roster_8c_v2 fam_roster_9c_v2 fam_roster_10c_v2 fam_roster_11c_v2 fam_roster_12c_v2 fam_roster_13c_v2 fam_roster_14c_v2 fam_roster_15c_v2{
	gen bio_sibling`n' = 1 if `sib'==3
	gen ado_sibling`n' = 1 if `sib'==5
	gen ste_sibling`n' = 1 if `sib'==7
	
	local n = `n'+1
}

egen bio_siblings = rowtotal(bio_sibling1-bio_sibling14)
egen ado_siblings = rowtotal(ado_sibling1-ado_sibling14)
egen ste_siblings = rowtotal(ste_sibling1-ste_sibling14)
egen tot_siblings = rowtotal(bio_siblings ado_siblings ste_siblings)

gen any_sibling = cond(tot_siblings>0,1,0)

foreach sib in bio_siblings ado_siblings ste_siblings tot_siblings any_sibling{
	replace `sib' = . if fam_roster_2c_v2==.
}

*** Family variables, after baseline
gen 	both_bioparents_l = 1 if (demo_prim_l==1 | demo_prim_l==2) & (fam_roster_2c_v2_l==1 | fam_roster_3c_v2_l==1 | fam_roster_4c_v2_l==1 | fam_roster_5c_v2_l==1 | fam_roster_6c_v2_l==1 | fam_roster_7c_v2_l==1 | fam_roster_8c_v2_l==1 | fam_roster_9c_v2_l==1 | fam_roster_10c_v2_l==1 | fam_roster_11c_v2_l==1 | fam_roster_12c_v2_l==1 | fam_roster_13c_v2_l==1 | fam_roster_14c_v2_l==1 | fam_roster_15c_v2_l==1)
replace both_bioparents_l = 0 if both_bioparents_l!=1 & fam_roster_2c_v2_l!=.

gen 	both_adoparents_l = 1 if (demo_prim_l==3) & (fam_roster_2c_v2_l==1 | fam_roster_3c_v2_l==1 | fam_roster_4c_v2_l==1 | fam_roster_5c_v2_l==1 | fam_roster_6c_v2_l==1 | fam_roster_7c_v2_l==1 | fam_roster_8c_v2_l==1 | fam_roster_9c_v2_l==1 | fam_roster_10c_v2_l==1 | fam_roster_11c_v2_l==1 | fam_roster_12c_v2_l==1 | fam_roster_13c_v2_l==1 | fam_roster_14c_v2_l==1 | fam_roster_15c_v2_l==1)
replace both_adoparents = 0 if both_adoparents_l!=1 & fam_roster_2c_v2_l!=.

gen 	both_cusparents_l = 1 if (demo_prim_l==4) & (fam_roster_2c_v2_l==1 | fam_roster_3c_v2_l==1 | fam_roster_4c_v2_l==1 | fam_roster_5c_v2_l==1 | fam_roster_6c_v2_l==1 | fam_roster_7c_v2_l==1 | fam_roster_8c_v2_l==1 | fam_roster_9c_v2_l==1 | fam_roster_10c_v2_l==1 | fam_roster_11c_v2_l==1 | fam_roster_12c_v2_l==1 | fam_roster_13c_v2_l==1 | fam_roster_14c_v2_l==1 | fam_roster_15c_v2_l==1)
replace both_cusparents_l = 0 if both_cusparents_l!=1 & fam_roster_2c_v2!=.

gen 	both_othparents_l = 1 if (demo_prim_l==4) & (fam_roster_2c_v2_l==1 | fam_roster_3c_v2_l==1 | fam_roster_4c_v2_l==1 | fam_roster_5c_v2_l==1 | fam_roster_6c_v2_l==1 | fam_roster_7c_v2_l==1 | fam_roster_8c_v2_l==1 | fam_roster_9c_v2_l==1 | fam_roster_10c_v2_l==1 | fam_roster_11c_v2_l==1 | fam_roster_12c_v2_l==1 | fam_roster_13c_v2_l==1 | fam_roster_14c_v2_l==1 | fam_roster_15c_v2_l==1)
replace both_othparents_l = 0 if both_cusparents_l!=1 & fam_roster_2c_v2_l!=.

gen 	one_bioparent_l = 1 if (demo_prim_l==1 | demo_prim_l==2) & (fam_roster_2c_v2_l!=1 & fam_roster_3c_v2_l!=1 & fam_roster_4c_v2_l!=1 & fam_roster_5c_v2_l!=1 & fam_roster_6c_v2_l!=1 & fam_roster_7c_v2_l!=1 & fam_roster_8c_v2_l!=1 & fam_roster_9c_v2_l!=1 & fam_roster_10c_v2_l!=1 & fam_roster_11c_v2_l!=1 & fam_roster_12c_v2_l!=1 & fam_roster_13c_v2_l!=1 & fam_roster_14c_v2_l!=1 & fam_roster_15c_v2_l!=1)
replace one_bioparent_l = 0 if one_bioparent_l!=1 & fam_roster_2c_v2_l!=.

gen 	one_adoparent_l = 1 if (demo_prim_l==3) & (fam_roster_2c_v2_l!=1 & fam_roster_3c_v2_l!=1 & fam_roster_4c_v2_l!=1 & fam_roster_5c_v2_l!=1 & fam_roster_6c_v2_l!=1 & fam_roster_7c_v2_l!=1 & fam_roster_8c_v2_l!=1 & fam_roster_9c_v2_l!=1 & fam_roster_10c_v2_l!=1 & fam_roster_11c_v2_l!=1 & fam_roster_12c_v2_l!=1 & fam_roster_13c_v2_l!=1 & fam_roster_14c_v2_l!=1 & fam_roster_15c_v2_l!=1)
replace one_adoparent_l = 0 if one_adoparent_l!=1 & fam_roster_2c_v2_l!=.

gen 	one_cusparent_l = 1 if (demo_prim_l==4) & (fam_roster_2c_v2_l!=1 & fam_roster_3c_v2_l!=1 & fam_roster_4c_v2_l!=1 & fam_roster_5c_v2_l!=1 & fam_roster_6c_v2_l!=1 & fam_roster_7c_v2_l!=1 & fam_roster_8c_v2_l!=1 & fam_roster_9c_v2_l!=1 & fam_roster_10c_v2_l!=1 & fam_roster_11c_v2_l!=1 & fam_roster_12c_v2_l!=1 & fam_roster_13c_v2_l!=1 & fam_roster_14c_v2_l!=1 & fam_roster_15c_v2_l!=1)
replace one_cusparent_l = 0 if one_cusparent_l!=1 & fam_roster_2c_v2_l!=.

gen 	one_othparent_l = 1 if (demo_prim_l==4) & (fam_roster_2c_v2_l!=1 & fam_roster_3c_v2_l!=1 & fam_roster_4c_v2_l!=1 & fam_roster_5c_v2_l!=1 & fam_roster_6c_v2_l!=1 & fam_roster_7c_v2_l!=1 & fam_roster_8c_v2_l!=1 & fam_roster_9c_v2_l!=1 & fam_roster_10c_v2_l!=1 & fam_roster_11c_v2_l!=1 & fam_roster_12c_v2_l!=1 & fam_roster_13c_v2_l!=1 & fam_roster_14c_v2_l!=1 & fam_roster_15c_v2_l!=1)
replace one_othparent_l = 0 if one_othparent_l!=1 & fam_roster_2c_v2_l!=.

local n = 1
foreach sib in fam_roster_2c_v2 fam_roster_3c_v2 fam_roster_4c_v2 fam_roster_5c_v2 fam_roster_6c_v2 fam_roster_7c_v2 fam_roster_8c_v2 fam_roster_9c_v2 fam_roster_10c_v2 fam_roster_11c_v2 fam_roster_12c_v2 fam_roster_13c_v2 fam_roster_14c_v2 fam_roster_15c_v2{
	gen bio_sibling_l`n' = 1 if `sib'_l==3
	gen ado_sibling_l`n' = 1 if `sib'_l==5
	gen ste_sibling_l`n' = 1 if `sib'_l==7
	
	local n = `n'+1
}

egen bio_siblings_l = rowtotal(bio_sibling_l1-bio_sibling_l14)
egen ado_siblings_l = rowtotal(ado_sibling_l1-ado_sibling_l14)
egen ste_siblings_l = rowtotal(ste_sibling_l1-ste_sibling_l14)
egen tot_siblings_l = rowtotal(bio_siblings_l ado_siblings_l ste_siblings_l)

gen any_sibling_l = cond(tot_siblings_l>0,1,0)

foreach sib in bio_siblings ado_siblings ste_siblings tot_siblings any_sibling{
	replace `sib'_l = . if fam_roster_2c_v2_l==.
}

replace bio_siblings = bio_siblings_l if eventname!="baseline_year_1_arm_1"
replace ado_siblings = ado_siblings_l if eventname!="baseline_year_1_arm_1"
replace ste_siblings = ste_siblings_l if eventname!="baseline_year_1_arm_1"
replace tot_siblings = tot_siblings_l if eventname!="baseline_year_1_arm_1"
replace any_sibling = any_sibling_l if eventname!="baseline_year_1_arm_1"

*** Religion
gen religion = demo_yrs_2 if demo_yrs_2<=4

label define religion 1 "Not at all" 2 "Not very" 3 "Somewhat" 4 "Very"
label values religion religion 

rename q_fam_income_pc_5 q_fam_income_base
rename d_fam_income_pc_5 d_fam_income_base

keep 	src_subject_id eventname demoi_p_select_language___1-demo_med_insur_e_p 	///
		race_ethnicity acs_raked_propensity_score bio_siblings ado_siblings 		///
		ste_siblings tot_siblings any_sibling religion married fam_income			///
		educ_lev educ_levpt q_fam_income_* d_fam_income_* country_op country_oy 	///
		foreign_p foreign_y fam_income_pc married
		
tempfile demographics
save `demographics'


**************************************************************************************
**************************************** 11 ******************************************
**************************************************************************************
**** Loading data on Latent factors
clear
import delimited "$data5p1\abcd-general\abcd_y_lf.csv"

keep 	src_subject_id eventname latent_factor_ss_general_ses 						///
		latent_factor_ss_social latent_factor_ss_perinatal

tempfile latent
save `latent'


**************************************************************************************
**************************************** 12 *******************************************
**************************************************************************************
**** Loading data on discrimination
/*clear
import delimited "$data\abcd_ydmes01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\culture-environment\ce_y_dm.csv"

keep 	src_subject_id eventname dim_yesno_q1-dim_y_ss_mean_nt
		
tempfile discrimin
save `discrimin'


**************************************************************************************
**************************************** 13 ******************************************
**************************************************************************************
**** Loading data on temperamental traits
/*clear
import delimited "$data\abcd_bisbas01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\mental-health\mh_y_bisbas.csv"
** Scale scoring from: https://www.phenxtoolkit.org/protocols/view/540601#:~:text=The%20behavioral%20inhibition%20system%20(BIS,strongly%20agree%20to%20strongly%20disagree).

* Behavioral Activation System (BAS) 
egen bas_drive = rowtotal(bisbas13_y bisbas14_y bisbas15_y bisbas16_y)
egen bas_funsk = rowtotal(bisbas17_y bisbas17_y bisbas18_y bisbas19_y bisbas20_y)
egen bas_rewar = rowtotal(bisbas8_y bisbas10_y bisbas9_y bisbas11_y bisbas12_y)

* Behavioral Inhibition System (BIS)
egen bis = rowtotal(bisbas1_y bisbas2_y bisbas3_y bisbas4_y bisbas5r_y bisbas6_y bisbas7_y)

keep 	src_subject_id eventname												///
		bas_drive bas_funsk bas_rewar bis bis_y_ss_bis_sum bis_y_ss_bas_rr 		///
		bis_y_ss_bas_drive bis_y_ss_bas_fs bis_y_ss_bism_sum bis_y_ss_basm_rr 	///
		bis_y_ss_basm_drive
		
		
tempfile bisbas
save `bisbas'


**************************************************************************************
************************************** 14 ********************************************
**************************************************************************************
**** Loading data on Structural MRI

**** A. Loading the database (Regions of Interest - ROI, Cortical structures)
/*clear
import delimited "$data\abcd_smrip10201.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\imaging\mri_y_smr_vol_dsk.csv"

keep 	src_subject_id eventname												///
		smri_vol_cdk_totallh smri_vol_cdk_totalrh smri_vol_cdk_total 			///
		smri_vol_cdk_insulalh smri_vol_cdk_insularh smri_vol_cdk_rrmdfrlh 		///
		smri_vol_cdk_rrmdfrrh smri_vol_cdk_frpolerh smri_vol_cdk_frpolelh 		///
		smri_vol_cdk_lobfrlh smri_vol_cdk_lobfrrh smri_vol_cdk_mobfrlh 			///
		smri_vol_cdk_mobfrrh smri_vol_cdk_cdacaterh smri_vol_cdk_cdacatelh		///
		smri_vol_cdk_parahpallh smri_vol_cdk_parahpalrh smri_vol_cdk_cdmdfrlh	///
		smri_vol_cdk_cdmdfrrh smri_vol_cdk_sufrlh smri_vol_cdk_sufrrh			///
		smri_vol_cdk_rracatelh smri_vol_cdk_rracaterh
		
tempfile smri1
save `smri1'


**** B. Loading the database (Regions of Interest - ROI, Subcortical structures)
/*clear
import delimited "$data\abcd_smrip10201.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 
*/

clear
import delimited "$data5p1\imaging\mri_y_smr_vol_aseg.csv"

keep 	src_subject_id eventname																///
		smri_vol_scs_cbwmatterlh smri_vol_scs_ltventriclelh smri_vol_scs_inflatventlh 			///
		smri_vol_scs_crbwmatterlh smri_vol_scs_crbcortexlh smri_vol_scs_tplh  					///
		smri_vol_scs_caudatelh smri_vol_scs_putamenlh smri_vol_scs_pallidumlh 		 			///
		smri_vol_scs_3rdventricle smri_vol_scs_4thventricle smri_vol_scs_bstem 			 		///
		smri_vol_scs_hpuslh smri_vol_scs_amygdalalh smri_vol_scs_csf smri_vol_scs_lesionlh 	 	///
		smri_vol_scs_aal smri_vol_scs_vedclh smri_vol_scs_cbwmatterrh 							///
		smri_vol_scs_ltventriclerh smri_vol_scs_inflatventrh smri_vol_scs_crbwmatterrh 			///
		smri_vol_scs_crbcortexrh smri_vol_scs_tprh smri_vol_scs_caudaterh 						///
		smri_vol_scs_putamenrh smri_vol_scs_pallidumrh smri_vol_scs_hpusrh 						///
		smri_vol_scs_amygdalarh smri_vol_scs_lesionrh smri_vol_scs_aar smri_vol_scs_vedcrh 		///
		smri_vol_scs_wmhint smri_vol_scs_wmhintlh smri_vol_scs_wmhintrh smri_vol_scs_ccps 		///
		smri_vol_scs_ccmidps smri_vol_scs_ccct smri_vol_scs_ccmidat smri_vol_scs_ccat 			///
		smri_vol_scs_wholeb smri_vol_scs_latventricles smri_vol_scs_allventricles 				///
		smri_vol_scs_intracranialv smri_vol_scs_suprateialv smri_vol_scs_subcorticalgv 
		
		
tempfile smri2
save `smri2'


**** C. Recommended inclusion
clear
import delimited "$data5p1\imaging\mri_y_qc_incl.csv"

tempfile smri_rinc
save `smri_rinc'

**************************************************************************************
*************************************** 15 *******************************************
**************************************************************************************
**** Loading data on Parental Acceptance (Also see family conflict: abcd_fes01)
/*clear
import delimited "$data\crpbi01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\culture-environment\ce_y_crpbi.csv"
		
tempfile paccept
save `paccept'


**************************************************************************************
*************************************** 16 *******************************************
**************************************************************************************
**** Loading data on Family conflict
/*clear
import delimited "$data\abcd_fes01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\culture-environment\ce_y_fes.csv"

egen family_conf = rowtotal(fes_youth_q1 fes_youth_q2 fes_youth_q3 fes_youth_q4 fes_youth_q5 fes_youth_q6 fes_youth_q7 fes_youth_q8 fes_youth_q9), miss
egen fam_confav = rowmean(fes_youth_q1 fes_youth_q2 fes_youth_q3 fes_youth_q4 fes_youth_q5 fes_youth_q6 fes_youth_q7 fes_youth_q8 fes_youth_q9)
		
tempfile fesy
save `fesy'


**************************************************************************************
*************************************** 17 *******************************************
**************************************************************************************
**** Loading data on Parental Monitoring Survey
/*clear
import delimited "$data\pmq01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1


** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\culture-environment\ce_y_pm.csv"

egen p_monitor = rowmean(parent_monitor_q1_y parent_monitor_q2_y parent_monitor_q3_y parent_monitor_q4_y parent_monitor_q5_y)
	
tempfile parent_mon
save `parent_mon'


**************************************************************************************
*************************************** 18 *******************************************
**************************************************************************************
**** Loading data on Parental Neglectful Behavior
clear
import delimited "$data5p1\culture-environment\ce_y_mnbs.csv"
	
tempfile parent_neglect
save `parent_neglect'


************************************************************************************************************
******************************************   19   **********************************************************
************************************************************************************************************
**** Loading the database - Parent Adult Self Report Scores Aseba (ASR) - Summary scores
/*clear
import delimited "$data\abcd_asrs01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1


** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\mental-health\mh_p_asr.csv"


**************************************************************************************
*Syndrome scales - 	Anxious/Depressed, Withdrawn/Depressed, Somatic Complaints -> Internalizing Behavior
*+					Social problems, Thought Problems, Attention Problems
*+					Rul-Breaking Behavior, Aggressive Behavior -> Externalizing Behavior


global anxdepress2 		"asr_q12_p asr_q13_p asr_q14_p asr_q22_p asr_q31_p asr_q33_p asr_q34_p asr_q35_p asr_q45_p asr_q47_p asr_q50_p asr_q52_p asr_q71_p asr_q91_p asr_q103_p asr_q107_p asr_q112_p asr_q113_p"
global withdranx 		"asr_q25_p asr_q30_p asr_q42_p asr_q48_p asr_q60_p asr_q65_p asr_q67_p asr_q69_p asr_q111_p"
global somatic1 		"asr_q51_p asr_q54_p asr_q56a_p asr_q56b_p asr_q56c_p asr_q56d_p asr_q56e_p asr_q56f_p asr_q56g_p asr_q56h_p asr_q56i_p asr_q100_p"
global internalizing 	$anxdepress2 $withdranx $somatic1
global thoughtprob		"asr_q09_p asr_q18_p asr_q36_p asr_q40_p asr_q46_p asr_q63_p asr_q66_p asr_q70_p asr_q84_p asr_q85_p"
global attentionprob	"asr_q01_p asr_q08_p asr_q11_p asr_q17_p asr_q53_p asr_q59_p asr_q61_p asr_q64_p asr_q78_p asr_q101_p asr_q102_p asr_q105_p asr_q108_p asr_q119_p asr_q121_p"
global aggressive		"asr_q03_p asr_q05_p asr_q16_p asr_q28_p asr_q37_p asr_q55_p asr_q57_p asr_q68_p asr_q81_p asr_q86_p asr_q87_p asr_q95_p asr_q97_p asr_q116_p asr_q118_p"
global rulebreaker		"asr_q06_p asr_q20_p asr_q23_p asr_q26_p asr_q39_p asr_q41_p asr_q43_p asr_q76_p asr_q82_p asr_q90_p asr_q92_p asr_q114_p asr_q117_p asr_q122_p"
global intrusive		"asr_q07_p asr_q19_p asr_q74_p asr_q93_p asr_q94_p asr_q104_p"
global externalizing	$rulebreaker $aggressive $intrusive

**************************************************************************************
** DSM Oriented Scales - Depressive Problems, Anxiety Problems, Somatic Problems,
*+						Oppositional Defiant Problems, Conduct Problems

global depressive 		"asr_q14_p asr_q18_p asr_q24_p asr_q35_p asr_q52_p asr_q54_p asr_q60_p asr_q77_p asr_q78_p asr_q91_p asr_q100_p asr_q102_p asr_q103_p asr_q107_p"
global anxious 			"asr_q22_p asr_q29_p asr_q45_p asr_q50_p asr_q72_p asr_q112_p"
global somatic2 		"asr_q56a_p asr_q56b_p asr_q56c_p asr_q56d_p asr_q56e_p asr_q56f_p asr_q56g_p asr_q56h_p asr_q56i_p"
global avoidantpers 	"asr_q25_p asr_q42_p asr_q47_p asr_q67_p asr_q71_p asr_q75_p asr_q111_p"
global attentiondef 	"asr_q01_p asr_q08_p asr_q10_p asr_q36_p asr_q41_p asr_q59_p asr_q61_p asr_q89_p asr_q105_p asr_q108_p asr_q115_p asr_q118_p asr_q119_p"
global antisocpers		"asr_q03_p asr_q05_p asr_q16_p asr_q21_p asr_q23_p asr_q26_p asr_q28_p asr_q37_p asr_q39_p asr_q43_p asr_q57_p asr_q76_p asr_q82_p asr_q92_p asr_q95_p asr_q97_p asr_q101_p asr_q114_p asr_q120_p asr_q122_p"


keep 	src_subject_id eventname 														///
		$thoughtprob $attentionprob $rulebreaker $aggressive $intrusive 				///
		$depressive $anxious $somatic2 $avoidantpers $attentiondef $antisocpers			///
		asr_scr_perstr_r asr_scr_perstr_t asr_scr_perstr_total asr_scr_perstr_nm 		///
		asr_scr_anxdep_r asr_scr_anxdep_t asr_scr_anxdep_total asr_scr_anxdep_nm 		///
		asr_scr_withdrawn_r asr_scr_withdrawn_t asr_scr_withdrawn_total 				///
		asr_scr_withdrawn_nm asr_scr_somatic_r asr_scr_somatic_t 						///
		asr_scr_somatic_total asr_scr_somatic_nm asr_scr_thought_r 						///
		asr_scr_thought_t asr_scr_thought_total asr_scr_thought_nm 						///
		asr_scr_attention_r asr_scr_attention_t asr_scr_attention_total 				///
		asr_scr_attention_nm asr_scr_aggressive_r asr_scr_aggressive_t 					///
		asr_scr_aggressive_total asr_scr_aggressive_nm asr_scr_rulebreak_r 				///
		asr_scr_rulebreak_t asr_scr_rulebreak_total asr_scr_rulebreak_nm 				///
		asr_scr_intrusive_r asr_scr_intrusive_t asr_scr_intrusive_total 				///
		asr_scr_intrusive_nm asr_scr_internal_r asr_scr_internal_t 						///
		asr_scr_internal_total asr_scr_internal_nm asr_scr_external_r 					///
		asr_scr_external_t asr_scr_external_total asr_scr_external_nm 					///
		asr_scr_totprob_r asr_scr_totprob_t asr_scr_totprob_total asr_scr_totprob_nm 	///
		asr_scr_depress_r asr_scr_depress_t asr_scr_depress_total asr_scr_depress_nm 	///
		asr_scr_anxdisord_r asr_scr_anxdisord_t asr_scr_anxdisord_total 				///
		asr_scr_anxdisord_nm asr_scr_somaticpr_r asr_scr_somaticpr_t 					///
		asr_scr_somaticpr_total asr_scr_somaticpr_nm asr_scr_avoidant_r 				///
		asr_scr_avoidant_t asr_scr_avoidant_total asr_scr_avoidant_nm 					///
		asr_scr_adhd_r asr_scr_adhd_t asr_scr_adhd_total asr_scr_adhd_nm 				///
		asr_scr_antisocial_r asr_scr_antisocial_t asr_scr_antisocial_total 				///
		asr_scr_antisocial_nm asr_scr_inattention_r asr_scr_inattention_t 				///
		asr_scr_inattention_total asr_scr_inattention_nm asr_scr_hyperactive_r 			///
		asr_scr_hyperactive_t asr_scr_hyperactive_total asr_scr_hyperactive_nm			
		
tempfile asrs
save `asrs'	


**************************************************************************************
**************************************** 20 ******************************************
**************************************************************************************
** ASR profile
/*
clear
import delimited "$data5p1\mental-health\mh_p_asr.csv"


*******************
*Syndrome scales - 	Anxious/Depressed, Withdrawn/Depressed, Somatic Complaints -> Internalizing Behavior
*+					Social problems, Thought Problems, Attention Problems
*+					Rul-Breaking Behavior, Aggressive Behavior -> Externalizing Behavior


global anxdepress2 		"asr_q12_p asr_q13_p asr_q14_p asr_q22_p asr_q31_p asr_q33_p asr_q34_p asr_q35_p asr_q45_p asr_q47_p asr_q50_p asr_q52_p asr_q71_p asr_q91_p asr_q103_p asr_q107_p asr_q112_p asr_q113_p"
global withdranx 		"asr_q25_p asr_q30_p asr_q42_p asr_q48_p asr_q60_p asr_q65_p asr_q67_p asr_q69_p asr_q111_p"
global somatic1 		"asr_q51_p asr_q54_p asr_q56a_p asr_q56b_p asr_q56c_p asr_q56d_p asr_q56e_p asr_q56f_p asr_q56g_p asr_q56h_p asr_q56i_p asr_q100_p"
global internalizing 	$anxdepress2 $withdranx $somatic1
global thoughtprob		"asr_q09_p asr_q18_p asr_q36_p asr_q40_p asr_q46_p asr_q63_p asr_q66_p asr_q70_p asr_q84_p asr_q85_p"
global attentionprob	"asr_q01_p asr_q08_p asr_q11_p asr_q17_p asr_q53_p asr_q59_p asr_q61_p asr_q64_p asr_q78_p asr_q101_p asr_q102_p asr_q105_p asr_q108_p asr_q119_p asr_q121_p"
global aggressive		"asr_q03_p asr_q05_p asr_q16_p asr_q28_p asr_q37_p asr_q55_p asr_q57_p asr_q68_p asr_q81_p asr_q86_p asr_q87_p asr_q95_p asr_q97_p asr_q116_p asr_q118_p"
global rulebreaker		"asr_q06_p asr_q20_p asr_q23_p asr_q26_p asr_q39_p asr_q41_p asr_q43_p asr_q76_p asr_q82_p asr_q90_p asr_q92_p asr_q114_p asr_q117_p asr_q122_p"
global intrusive		"asr_q07_p asr_q19_p asr_q74_p asr_q93_p asr_q94_p asr_q104_p"
global externalizing	$rulebreaker $aggressive $intrusive

****************
** CBCL Score 1: Summing across all responses
egen asr_anxdep1 = rowtotal($anxdepress2)
egen asr_witanx1 = rowtotal($withdranx)
egen asr_somatc1 = rowtotal($somatic1)
egen asr_intern1 = rowtotal($internalizing)
egen asr_though1 = rowtotal($thoughtprob)
egen asr_attent1 = rowtotal($attentionprob)
egen asr_rulebr1 = rowtotal($rulebreaker)
egen asr_aggres1 = rowtotal($aggressive)
egen asr_extern1 = rowtotal($externalizing)
egen asr_intrsv1 = rowtotal($intrusive)

****************
** CBCL Score 2: Summing across dichotomized responses (Not true = 0; often or sometimes true = 1)
foreach v of varlist $anxdepress2 $withdranx $somatic1 $thoughtprob $attentionprob $rulebreaker $aggressive $intrusive {
	rename `v' `v'temp
	gen `v' = cond(`v'temp>=1,1,0)
}

egen asr_anxdep2 = rowtotal($anxdepress2)
egen asr_witanx2 = rowtotal($withdranx)
egen asr_somatc2 = rowtotal($somatic1)
egen asr_intern2 = rowtotal($internalizing)
egen asr_though2 = rowtotal($thoughtprob)
egen asr_attent2 = rowtotal($attentionprob)
egen asr_rulebr2 = rowtotal($rulebreaker)
egen asr_aggres2 = rowtotal($aggressive)
egen asr_extern2 = rowtotal($externalizing)
egen asr_intrsv2 = rowtotal($intrusive)

foreach v of varlist $anxdepress2 $withdranx $somatic1 $thoughtprob $attentionprob $rulebreaker $aggressive $intrusive {
	drop `v'
	rename `v'temp `v'
}

****************
** CBCL Score 3: Principal Component analysis
pca $anxdepress2
predict asr_anxdep3
pca $withdranx
predict asr_witanx3
pca $somatc1
predict asr_somatc3
pca $internalizing
predict asr_intern3
pca $thoughtprob
predict asr_though3
pca $attentionprob
predict asr_attent3
pca $rulebreaker
predict asr_rulebr3
pca $aggressive
predict asr_aggres3
pca $externalizing
predict asr_extern3
pca $intrusive
predict asr_intrsv3

****************
** DMS Score 4: Percentile of ASR Score 1

foreach v in asr_anxdep asr_witanx asr_somatc asr_intern asr_though asr_attent asr_rulebr asr_aggres asr_extern asr_intrsv {
	xtile 	`v'4a = `v'1 if eventname=="baseline_year_1_arm_1", nq(100) 
	xtile 	`v'4c = `v'1 if eventname=="2_year_follow_up_y_arm_1", nq(100) 
	gen 	`v'4 = `v'4a if eventname=="baseline_year_1_arm_1"
	replace	`v'4 = `v'4c if eventname=="2_year_follow_up_y_arm_1"
	drop `v'4a `v'4c
}

**************************************************************************************
**************************************************************************************
** DSM Oriented Scales - Depressive Problems, Anxiety Problems, Somatic Problems,
*+						Oppositional Defiant Problems, Conduct Problems

global depressive 		"asr_q14_p asr_q18_p asr_q24_p asr_q35_p asr_q52_p asr_q54_p asr_q60_p asr_q77_p asr_q78_p asr_q91_p asr_q100_p asr_q102_p asr_q103_p asr_q107_p"
global anxious 			"asr_q22_p asr_q29_p asr_q45_p asr_q50_p asr_q72_p asr_q112_p"
global somatic2 		"asr_q56a_p asr_q56b_p asr_q56c_p asr_q56d_p asr_q56e_p asr_q56f_p asr_q56g_p asr_q56h_p asr_q56i_p"
global avoidantpers 	"asr_q25_p asr_q42_p asr_q47_p asr_q67_p asr_q71_p asr_q75_p asr_q111_p"
global attentiondef 	"asr_q01_p asr_q08_p asr_q10_p asr_q36_p asr_q41_p asr_q59_p asr_q61_p asr_q89_p asr_q105_p asr_q108_p asr_q115_p asr_q118_p asr_q119_p"
global antisocpers		"asr_q03_p asr_q05_p asr_q16_p asr_q21_p asr_q23_p asr_q26_p asr_q28_p asr_q37_p asr_q39_p asr_q43_p asr_q57_p asr_q76_p asr_q82_p asr_q92_p asr_q95_p asr_q97_p asr_q101_p asr_q114_p asr_q120_p asr_q122_p"

alpha $depressive if eventname=="baseline_year_1_arm_1"
alpha $depressive if eventname=="2_year_follow_up_y_arm_1"

****************
** DMS Score 1: Summing across all responses
egen asr_dsm_depres1 = rowtotal($depressive)
egen asr_dsm_anxiou1 = rowtotal($anxious)
egen asr_dsm_somati1 = rowtotal($somatic2)
egen asr_dsm_avoidt1 = rowtotal($avoidantpers)
egen asr_dsm_attent1 = rowtotal($attentiondef)
egen asr_dsm_antiso1 = rowtotal($antisocpers)

****************
** DMS Score 2: Summing across dichotomized responses (Not true = 0; often or sometimes true = 1)
foreach v of varlist $depressive $anxious $somatic2 $avoidantpers $attentiondef $antisocpers {
	rename `v' `v'temp
	gen `v' = cond(`v'temp>=1,1,0)
}

egen asr_dsm_depres2 = rowtotal($depressive)
egen asr_dsm_anxiou2 = rowtotal($anxious)
egen asr_dsm_somati2 = rowtotal($somatic2)
egen asr_dsm_avoidt2 = rowtotal($avoidantpers)
egen asr_dsm_attent2 = rowtotal($attentiondef)
egen asr_dsm_antiso2 = rowtotal($antisocpers)

foreach v of varlist $depressive $anxious $somatic2 $avoidantpers $attentiondef $antisocpers {
	drop `v'
	rename `v'temp `v'
}

****************
** DMS Score 3: Principal Component analysis
pca $depressive 
predict asr_dsm_depres3
pca $anxious 
predict asr_dsm_anxiou3
pca $somatic2 
predict asr_dsm_somati3
pca $avoidantpers 
predict asr_dsm_avoidt3
pca $attentiondef 
predict asr_dsm_attent3
pca $antisocpers
predict asr_dsm_antiso3

****************
** DMS Score 4: Percentile of DMS Score 1 (Check do-file accountability paper - PSU)

foreach v in asr_dsm_depres asr_dsm_anxiou asr_dsm_somati asr_dsm_attent asr_dsm_avoidt asr_dsm_antiso {
	xtile 	`v'4a = `v'1 if eventname=="baseline_year_1_arm_1", nq(100) 
	xtile 	`v'4c = `v'1 if eventname=="2_year_follow_up_y_arm_1", nq(100) 
	gen 	`v'4 = `v'4a if eventname=="baseline_year_1_arm_1"
	replace	`v'4 = `v'4c if eventname=="2_year_follow_up_y_arm_1"
	drop `v'4a `v'4c
}

keep subjectkey src_subject_id interview_date interview_age gender eventname		///
		$thoughtprob $attentionprob $rulebreaker $aggressive $intrusive 			///
		$depressive $anxious $somatic2 $avoidantpers $attentiondef $antisocpers		///
		asr_dsm_depres1 asr_dsm_anxiou1 asr_dsm_somati1 asr_dsm_attent1 asr_dsm_avoidt1 asr_dsm_antiso1		///
		asr_dsm_depres2 asr_dsm_anxiou2 asr_dsm_somati2 asr_dsm_attent2 asr_dsm_avoidt2 asr_dsm_antiso2		///
		asr_anxdep1 asr_witanx1 asr_somatc1 $anxdepress2 $withdranx $somatic1 		///
		$internalizing asr_anxdep2 asr_witanx2 asr_somatc2

		
tempfile pasri
save `pasri'
*/


**************************************************************************************
*************************************** 21 *******************************************
**************************************************************************************
**** Loading data on Positive emotionality
/*clear
*import delimited "$data2p0\abcd_ytbpai01.txt", varnames(1) encoding(Big5) 
import delimited "$data\abcd_ytbpai01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\mental-health\mh_y_poa.csv"
		
tempfile posemo
save `posemo'


**************************************************************************************
*************************************** 22 *******************************************
**************************************************************************************
**** Loading data on Parent prosocial behavior surveys (about their children)
/*clear
import delimited "$data\psb01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\culture-environment\ce_p_psb.csv"
		
tempfile prosocp
save `prosocp'


**************************************************************************************
*************************************** 23 *******************************************
**************************************************************************************
**** Loading data on Youth prosocial behavior survey
/*clear
import delimited "$data\abcd_psb01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/
clear
import delimited "$data5p1\culture-environment\ce_y_psb.csv"
		
tempfile prosocy
save `prosocy'


**************************************************************************************
*************************************** 24 *******************************************
**************************************************************************************
**** Loading data on Children's will to problem solving (one-year follow-up)
/*clear
import delimited "$data\abcd_ywpss01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\culture-environment\ce_y_wps.csv"
		
tempfile ywill
save `ywill'


**************************************************************************************
*************************************** 25 *******************************************
**************************************************************************************
**** Loading data on Physical activity (3 indicators)
/*clear
import delimited "$data\abcd_yrb01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\physical-health\ph_y_yrb.csv"
		
tempfile phyactiv
save `phyactiv'
		

**************************************************************************************
*************************************** 26 *******************************************
**************************************************************************************
**** Loading data on Involvement in sport activities
/*clear
import delimited "$data\abcd_saiq02.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/ 

clear
*import delimited "$data5p1\physical-health\ph_y_saiq.csv"
import delimited "$data5p1\physical-health\ph_p_saiq.csv"

gen 	nosportp = sai_p_activities___29 if eventname=="baseline_year_1_arm_1"
replace nosportp = sai_p_activities_l___29 if eventname!="baseline_year_1_arm_1"

label var nosportp "Has not participated EVER in any of 28 sports"

keep 	src_subject_id eventname nosportp
		
tempfile sportact
save `sportact'

**************************************************************************************
*************************************** 27 *******************************************
**************************************************************************************
**** Loading data on Autism
/*clear
import delimited "$data\abcd_pssrs01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\mental-health\mh_p_ssrs.csv"
		
tempfile autis
save `autis'


**************************************************************************************
*************************************** 28 *******************************************
**************************************************************************************
**** Loading data on Conduct Disorder
/*clear
import delimited "$data\abcd_pksadscd01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

** A. Caregiver report
clear
import delimited "$data5p1\mental-health\mh_p_ksads_cd.csv"

keep 	src_subject_id eventname												///
		ksads_cdr_478_p ksads_cdr_479_p ksads_cdr_480_p ksads_cdr_487_p 		///
		ksads_cdr_518_p ksads_cdr_473_p ksads_cdr_474_p ksads_cdr_475_p 		///
		ksads_cdr_476_p ksads_cdr_477_p ksads_cdr_527_p ksads_cdr_528_p 		///
		ksads_cdr_486_p

		
tempfile conddp
save `conddp'


** B. Youth
clear
import delimited "$data5p1\mental-health\mh_y_ksads_cd.csv"

keep 	src_subject_id eventname												///
		ksads_cdr_raw_478_t ksads_cdr_raw_479_t ksads_cdr_raw_480_t ksads_cdr_raw_487_t 		///
		ksads_cdr_raw_518_t ksads_cdr_raw_473_t ksads_cdr_raw_474_t ksads_cdr_raw_475_t 		///
		ksads_cdr_raw_476_t ksads_cdr_raw_477_t ksads_cdr_raw_527_t ksads_cdr_raw_528_t 		///
		ksads_cdr_raw_486_t

		
tempfile conddt
save `conddt'


**************************************************************************************
*************************************** 29 *******************************************
**************************************************************************************
**** Loading data on mental health diagnoses and symptoms (parental report)
/*clear
import delimited "$data\abcd_ksad01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1


** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\mental-health\mh_p_ksads_ss.csv"

*********************
*** Diagnoses
describe ksads_1_843_p ksads_1_840_p ksads_1_846_p ksads_2_835_p ksads_2_836_p ksads_2_831_p ksads_2_832_p ksads_2_830_p ksads_2_838_p ksads_3_848_p ksads_4_826_p ksads_4_828_p ksads_4_851_p ksads_4_849_p ksads_5_906_p ksads_5_857_p ksads_6_859_p ksads_7_861_p ksads_7_909_p ksads_8_863_p ksads_8_911_p ksads_9_867_p ksads_10_913_p ksads_10_869_p ksads_11_917_p ksads_11_919_p ksads_12_927_p ksads_12_925_p ksads_13_938_p ksads_13_929_p ksads_13_932_p ksads_13_930_p ksads_13_935_p ksads_13_942_p ksads_13_941_p ksads_14_853_p ksads_15_901_p ksads_16_897_p ksads_16_898_p ksads_17_904_p ksads_19_891_p ksads_20_893_p ksads_20_874_p ksads_20_872_p ksads_20_889_p ksads_20_878_p ksads_20_877_p ksads_20_875_p ksads_20_876_p ksads_20_879_p ksads_20_873_p ksads_20_871_p ksads_21_923_p ksads_21_921_p ksads_22_969_p ksads_23_946_p ksads_23_954_p ksads_23_945_p ksads_23_950_p ksads_23_947_p ksads_23_948_p ksads_23_949_p ksads_23_952_p ksads_23_955_p ksads_23_951_p ksads_23_953_p ksads_24_967_p ksads_25_915_p ksads_25_865_p


foreach v of varlist ksads_1_843_p ksads_1_840_p ksads_1_846_p ksads_2_835_p ksads_2_836_p ksads_2_831_p ksads_2_832_p ksads_2_830_p ksads_2_838_p ksads_3_848_p ksads_4_826_p ksads_4_828_p ksads_4_851_p ksads_4_849_p ksads_5_906_p ksads_5_857_p ksads_6_859_p ksads_7_861_p ksads_7_909_p ksads_8_863_p ksads_8_911_p ksads_9_867_p ksads_10_913_p ksads_10_869_p ksads_11_917_p ksads_11_919_p ksads_12_927_p ksads_12_925_p ksads_13_938_p ksads_13_929_p ksads_13_932_p ksads_13_930_p ksads_13_935_p ksads_13_942_p ksads_13_941_p ksads_14_853_p ksads_15_901_p ksads_16_897_p ksads_16_898_p ksads_17_904_p ksads_19_891_p ksads_20_893_p ksads_20_874_p ksads_20_872_p ksads_20_889_p ksads_20_878_p ksads_20_877_p ksads_20_875_p ksads_20_876_p ksads_20_879_p ksads_20_873_p ksads_20_871_p ksads_21_923_p ksads_21_921_p ksads_22_969_p ksads_23_946_p ksads_23_954_p ksads_23_945_p ksads_23_950_p ksads_23_947_p ksads_23_948_p ksads_23_949_p ksads_23_952_p ksads_23_955_p ksads_23_951_p ksads_23_953_p ksads_24_967_p ksads_25_915_p ksads_25_865_p{
	replace `v'=. if `v'>1
}
/*
rename ksads_4_829_p delusionsp_p
rename ksads_4_828_p delusionsp_c
rename ksads_4_850_p psychotic_p
rename ksads_4_849_p psychotic_c
rename ksads_4_826_p hallucina_c
rename ksads_4_827_p hallucina_p
rename ksads_4_851_p uschizoph_c
rename ksads_4_852_p uschizoph_p
rename ksads_13_938_p eatingdp_c 
rename ksads_13_940_p eatingdp_p
rename ksads_14_853_p adhdp_p
rename ksads_14_854_p adhdp_c
*/

* Schizo-related problems
clonevar delusionsp_p = ksads_4_829_p
clonevar delusionsp_c = ksads_4_828_p 
clonevar psychotic_p = ksads_4_850_p 
clonevar psychotic_c = ksads_4_849_p 
clonevar hallucina_c = ksads_4_826_p 
clonevar hallucina_p = ksads_4_827_p 
clonevar uschizoph_c = ksads_4_851_p 
clonevar uschizoph_p = ksads_4_852_p 

* Mania-related problems
clonevar bpimanic_c = ksads_2_830_p
clonevar bpidepre_c = ksads_2_831_p
clonevar bpihyman_c = ksads_2_832_p
clonevar bpiidepre_c = ksads_2_836_p
clonevar bpiihyman_c = ksads_2_835_p
clonevar bpunspec_c = ksads_2_838_p


*********************
*** Symptoms
global depress_present "ksads_1_1_p ksads_1_3_p ksads_1_5_p ksads_1_171_p ksads_1_167_p ksads_1_163_p ksads_1_169_p ksads_1_175_p ksads_1_173_p ksads_1_181_p ksads_1_165_p ksads_1_161_p ksads_1_159_p ksads_1_183_p ksads_1_157_p ksads_1_177_p ksads_22_141_p ksads_1_179_p"
global bipolar_present "ksads_2_12_p ksads_2_209_p ksads_2_195_p ksads_2_14_p ksads_2_217_p ksads_2_215_p ksads_2_201_p ksads_2_213_p ksads_2_9_p ksads_2_7_p ksads_2_191_p ksads_2_189_p ksads_2_197_p ksads_2_207_p ksads_2_199_p ksads_2_210_p ksads_2_203_p ksads_2_205_p ksads_2_192_p"
global mooddys_present "ksads_3_229_p ksads_3_226_p"
global psychos_present "ksads_4_258_p ksads_4_230_p ksads_4_252_p ksads_4_254_p ksads_4_243_p ksads_4_241_p ksads_4_237_p ksads_4_239_p ksads_4_256_p ksads_4_247_p ksads_4_245_p ksads_4_16_p ksads_4_234_p ksads_4_18_p ksads_4_236_p ksads_4_235_p ksads_4_232_p ksads_4_248_p ksads_4_250_p"
global panic_present "ksads_5_261_p ksads_5_263_p ksads_5_20_p ksads_5_265_p ksads_5_267_p ksads_5_269_p"
global agaroph_present "ksads_6_274_p ksads_6_276_p ksads_6_278_p ksads_6_22_p ksads_6_272_p"
global phobias_present "ksads_9_34_p ksads_9_37_p ksads_9_39_p ksads_9_41_p ksads_9_43_p"
global sepanx_present "ksads_7_300_p ksads_7_26_p ksads_7_287_p ksads_7_291_p ksads_7_293_p ksads_7_289_p ksads_7_295_p ksads_7_24_p ksads_7_281_p ksads_7_285_p ksads_7_297_p ksads_7_283_p"
global socanx_present "ksads_8_309_p ksads_8_29_p ksads_8_30_p ksads_8_311_p ksads_8_307_p ksads_8_303_p ksads_8_301_p"
global genanx_present "ksads_10_45_p ksads_10_320_p ksads_10_324_p ksads_10_328_p ksads_10_326_p ksads_10_47_p ksads_10_322_p"
global obscomp_present "ksads_11_343_p ksads_11_331_p ksads_11_48_p ksads_11_335_p ksads_11_341_p ksads_11_50_p ksads_11_339_p ksads_11_347_p ksads_11_333_p ksads_11_345_p ksads_11_337_p"
global social_probprpt "ksads_5_20_p ksads_5_261_p ksads_6_22_p ksads_6_272_p ksads_8_29_p ksads_8_307_p ksads_9_41_p ksads_9_34_p ksads_10_45_p ksads_10_320_p ksads_11_48_p ksads_11_50_p ksads_12_52_p ksads_12_56_p ksads_12_60_p ksads_12_62_p"
global eatdis_present "ksads_13_469_p ksads_13_470_p ksads_13_473_p ksads_13_471_p ksads_13_74_p ksads_13_472_p ksads_13_475_p ksads_13_68_p ksads_13_66_p ksads_13_70_p ksads_13_72_p"
global adhd_present "ksads_14_398_p ksads_14_403_p ksads_14_405_p ksads_14_406_p ksads_14_76_p ksads_14_396_p ksads_14_397_p ksads_14_404_p ksads_14_85_p ksads_14_77_p ksads_14_84_p ksads_14_401_p ksads_14_80_p ksads_14_81_p ksads_14_400_p ksads_14_88_p ksads_14_399_p ksads_14_408_p ksads_14_394_p ksads_14_395_p ksads_14_407_p ksads_14_402_p ksads_14_409_p ksads_14_429_p"
global oppdef_present "ksads_15_95_p ksads_15_436_p ksads_15_435_p ksads_15_433_p ksads_15_93_p ksads_15_432_p ksads_15_91_p ksads_15_438_p ksads_15_437_p ksads_15_434_p"
global condu_present "ksads_16_449_p ksads_16_463_p ksads_16_453_p ksads_16_461_p ksads_16_465_p ksads_16_98_p ksads_16_104_p ksads_16_102_p ksads_16_457_p ksads_16_455_p ksads_16_451_p ksads_16_106_p ksads_16_100_p ksads_16_447_p ksads_16_459_p"
global autism_present "ksads_18_116_p ksads_18_112_p ksads_18_114_p"
global alcoh_present "ksads_19_500_p ksads_19_499_p ksads_19_508_p ksads_19_493_p ksads_19_481_p ksads_19_501_p ksads_19_483_p ksads_19_491_p ksads_19_489_p ksads_19_485_p ksads_19_497_p ksads_19_122_p ksads_19_495_p ksads_19_120_p ksads_19_498_p ksads_19_486_p"
global drugs_present "ksads_20_126_p ksads_20_545_p ksads_20_590_p ksads_20_620_p ksads_20_665_p ksads_20_575_p ksads_20_650_p ksads_20_560_p ksads_20_521_p ksads_20_635_p" /* There are more symptoms to be added */
global ptsd_present "ksads_21_355_p ksads_21_373_p ksads_21_359_p ksads_21_365_p ksads_21_137_p ksads_21_369_p ksads_21_357_p ksads_21_349_p ksads_21_371_p ksads_21_134_p ksads_21_375_p ksads_21_385_p ksads_21_367_p ksads_21_383_p ksads_21_135_p ksads_21_377_p ksads_21_353_p ksads_21_139_p ksads_21_391_p ksads_21_351_p ksads_21_363_p ksads_21_361_p ksads_21_379_p ksads_21_387_p ksads_21_389_p ksads_21_381_p" 
global suic_present "ksads_23_813_p ksads_23_814_p ksads_23_809_p ksads_23_149_p ksads_23_812_p ksads_23_808_p ksads_23_807_p ksads_23_143_p ksads_23_145_p ksads_23_810_p ksads_23_147_p ksads_23_811_p ksads_23_815_p ksads_24_967_p ksads_24_153_p ksads_24_151_p" 
global symp_past "ksads_22_142_p ksads_1_158_p ksads_1_160_p ksads_1_182_p ksads_1_178_p ksads_1_164_p ksads_1_162_p ksads_23_150_p ksads_23_148_p ksads_23_820_p ksads_23_819_p ksads_23_818_p ksads_1_170_p ksads_1_166_p ksads_1_2_p ksads_1_4_p ksads_1_6_p ksads_1_1_p ksads_1_172_p ksads_1_168_p ksads_1_170_p ksads_1_166_p ksads_4_259_p ksads_4_231_p ksads_4_238_p ksads_4_240_p ksads_4_257_p ksads_4_246_p ksads_4_17_p ksads_4_19_p ksads_4_249_p ksads_4_251_p ksads_4_233_p ksads_2_196_p ksads_2_13_p ksads_2_198_p ksads_2_211_p ksads_2_214_p ksads_2_203_p ksads_2_207_p ksads_7_25_p ksads_7_284_p ksads_7_282_p ksads_7_27_p ksads_7_286_p ksads_7_288_p ksads_7_290_p ksads_7_292_p ksads_5_21_p ksads_5_262_p ksads_6_23_p ksads_6_273_p ksads_8_31_p ksads_8_308_p ksads_9_42_p ksads_9_35_p ksads_10_46_p ksads_10_321_p ksads_11_49_p ksads_11_51_p ksads_12_53_p ksads_12_61_p ksads_13_476_p ksads_13_477_p ksads_13_75_p ksads_13_480_p ksads_13_69_p ksads_13_67_p ksads_13_71_p ksads_13_73_p ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p ksads_15_92_p ksads_15_439_p ksads_15_440_p ksads_15_94_p ksads_15_96_p ksads_15_442_p ksads_15_436_p ksads_15_441_p ksads_16_105_p ksads_16_103_p ksads_16_460_p ksads_16_462_p ksads_16_466_p ksads_16_452_p ksads_16_464_p ksads_16_448_p ksads_16_454_p ksads_16_450_p ksads_16_99_p ksads_16_107_p ksads_16_456_p ksads_16_458_p ksads_16_101_p ksads_23_822_p ksads_23_144_p ksads_23_818_p ksads_23_825_p ksads_23_150_p ksads_23_821_p ksads_23_817_p ksads_23_816_p ksads_23_146_p ksads_23_819_p ksads_23_148_p ksads_23_820_p ksads_23_824_p ksads_2_204_p ksads_2_208_p ksads_2_202_p ksads_2_200_p ksads_1_180_p ksads_1_184_p ksads_1_174_p ksads_1_176_p"


foreach v of varlist $depress_present $bipolar_present $mooddys_present $psychos_present $panic_present $agaroph_present $phobias_present $sepanx_present $socanx_present $genanx_present $obscomp_present $social_probprpt $eatdis_present $adhd_present $oppdef_present $condu_present $autism_present $alcoh_present $drugs_present $ptsd_present $suic_present $symp_past{
	replace `v'=. if `v'==555 			/* No answer because failed to answer Level 1 screening */
	replace `v'=0 if `v'==888			/* No answer because failed to pass Level 1 screening */
}

sum $depress_present if eventname=="baseline_year_1_arm_1"
sum $depress_present if eventname=="1_year_follow_up_y_arm_1"
sum $depress_present if eventname=="2_year_follow_up_y_arm_1"


sum $psychos_present if eventname=="baseline_year_1_arm_1"
sum $psychos_present if eventname=="1_year_follow_up_y_arm_1"


*****************************************************************************************************************************
*****************************************************************************************************************************
/* Variables at baseline and one-year follow-up:
- ksads_4_16_p    int     %10.0g                Symptom - Hallucinations Present
- ksads_4_18_p    int     %10.0g                Symptom - Persecutory Delusions Past two weeks Present
- ksads_13_74_p   int     %10.0g                Symptom - Binge Eating Present
- ksads_13_68_p   int     %10.0g                Symptom - Emaciation Present
- ksads_13_66_p   int     %10.0g                Symptom - Fear of becoming obese Present
- ksads_13_70_p   int     %10.0g                Symptom - Weight control vomiting Present
- ksads_13_72_p   int     %10.0g                Symptom - Weight control other (laxatives exercise dieting pills) Present 徆

*/

global dsm5p_axdppy "ksads_1_843_p ksads_1_840_p ksads_1_846_p ksads_2_835_p ksads_2_836_p ksads_2_831_p ksads_2_832_p ksads_2_830_p ksads_2_838_p ksads_3_848_p ksads_4_826_p ksads_4_828_p ksads_4_851_p ksads_4_849_p ksads_5_906_p ksads_5_857_p ksads_6_859_p ksads_6_908_p ksads_7_861_p ksads_7_909_p ksads_8_863_p ksads_8_911_p ksads_9_867_p ksads_10_913_p ksads_10_869_p ksads_11_917_p ksads_11_919_p ksads_12_925_p ksads_12_927_p ksads_13_938_p ksads_13_929_p ksads_13_932_p ksads_13_930_p ksads_13_935_p ksads_13_942_p ksads_13_941_p ksads_17_904_p ksads_21_923_p ksads_21_921_p ksads_21_923_p ksads_25_915_p ksads_25_865_p"
global dsm5p_devdis "ksads_14_853_p ksads_18_903_p"
global dsm5p_condis "ksads_15_901_p ksads_16_897_p ksads_16_898_p"
global dsm5p_subdis "ksads_19_891_p ksads_19_895_p ksads_20_893_p ksads_20_874_p ksads_20_872_p ksads_20_889_p ksads_20_878_p ksads_20_877_p ksads_20_875_p ksads_20_876_p ksads_20_879_p ksads_20_873_p ksads_20_871_p"
global dsm5p_suibeh "ksads_23_946_p ksads_23_954_p ksads_23_945_p ksads_23_950_p ksads_23_947_p ksads_23_948_p ksads_23_949_p ksads_23_952_p ksads_23_955_p ksads_23_951_p ksads_23_953_p ksads_24_967_p"

*sum $dsm5p_conduct if eventname=="baseline_year_1_arm_1"
*sum $dsm5p_conduct if eventname=="1_year_follow_up_y_arm_1"

*****************************************************************************************************************************
*****************************************************************************************************************************
/* Variables at baseline and two-year follow-up:
*/
*** Depression
** Present
egen dpr_eating = rowmax(ksads_1_171_p ksads_1_167_p ksads_1_169_p ksads_1_165_p)
egen dpr_sleep = rowmax(ksads_1_157_p ksads_22_141_p)
egen dpr_motor = rowmax(ksads_1_175_p ksads_1_173_p)
egen dpr_energy = rowmax(ksads_1_159_p)
egen drp_worth = rowmax(ksads_1_181_p ksads_1_177_p)
egen drp_decis = rowmax(ksads_1_163_p ksads_1_161_p)
egen drp_suic = rowmax(ksads_23_149_p ksads_23_147_p ksads_23_811_p ksads_23_810_p ksads_23_809_p)
*ksads_23_815_p
egen dpr_dprirr = rowmax(ksads_1_1_p ksads_1_3_p)
egen dpr_eat2 = rowmax(ksads_1_165_p ksads_1_169_p)

* Major depressive disorder (MDD)
egen dsm5_mdd_nm = rownonmiss(ksads_1_1_p ksads_1_3_p ksads_1_5_p dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic)
egen dsm5_mdd = rowtotal(ksads_1_1_p ksads_1_3_p ksads_1_5_p dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic) if dsm5_mdd_nm==10, miss
egen Dsm5_mdd = rowmean(ksads_1_1_p ksads_1_3_p ksads_1_5_p dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic) if dsm5_mdd_nm==10

egen mdd_first = rowmax(ksads_1_1_p ksads_1_3_p ksads_1_5_p)
egen mdd_second = rowtotal(dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic)
gen mdd_diagn = (mdd_first==1 & dsm5_mdd>=5) if dsm5_mdd<.

* Vargas and Mittal
egen drp_selfinj = rowmax(ksads_23_807_p ksads_23_808_p)
egen dsm5_mddvm = rowtotal(ksads_1_1_p ksads_1_3_p ksads_1_5_p ksads_22_141_p ksads_23_143_p ksads_23_145_p ksads_23_147_p ksads_23_149_p ksads_1_157_p ksads_1_159_p ksads_1_161_p ksads_1_163_p ksads_1_165_p ksads_1_167_p ksads_1_169_p ksads_1_171_p ksads_1_173_p ksads_1_175_p ksads_1_177_p ksads_1_179_p ksads_1_181_p ksads_1_183_p), miss


* Dysthymia/persistent depression
egen dsm5_dys = rowtotal(dpr_dprirr dpr_eat2 dpr_energy ksads_1_181_p drp_decis ksads_1_179_p), miss
egen Dsm5_dys = rowmean(dpr_dprirr dpr_eat2 dpr_energy ksads_1_181_p drp_decis ksads_1_179_p)

** Past

egen drp_selfinjP = rowmax(ksads_23_816_p ksads_23_817_p)

egen dsm5_mddvmP = rowtotal(ksads_1_2_p ksads_1_4_p ksads_1_6_p ksads_22_142_p ksads_23_144_p ksads_23_146_p ksads_23_148_p ksads_23_150_p ksads_1_158_p ksads_1_160_p ksads_1_162_p ksads_1_164_p ksads_1_166_p ksads_1_168_p ksads_1_170_p ksads_1_172_p ksads_1_174_p ksads_1_176_p ksads_1_178_p ksads_1_180_p ksads_1_182_p ksads_1_184_p), miss

egen dpr_eatingP = rowmax(ksads_1_172_p ksads_1_168_p ksads_1_170_p ksads_1_166_p)
egen dpr_sleepP = rowmax(ksads_22_142_p ksads_1_158_p)
egen dpr_motorP = rowmax(ksads_1_176_p ksads_1_174_p)
egen dpr_energyP = rowmax(ksads_1_160_p)
egen drp_worthP = rowmax(ksads_1_182_p ksads_1_178_p)
egen drp_decisP = rowmax(ksads_1_164_p ksads_1_162_p)
egen drp_suicP = rowmax(ksads_23_150_p ksads_23_148_p ksads_23_820_p ksads_23_819_p ksads_23_818_p)

egen dpr_dprirrP = rowmax(ksads_1_2_p ksads_1_4_p)
egen dpr_eat2P = rowmax(ksads_1_170_p ksads_1_166_p)

* Major depressive disorder (MDD)
egen dsm5_mddP_nm = rownonmiss(ksads_1_2_p ksads_1_4_p ksads_1_6_p dpr_eatingP dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP)
egen dsm5_mddP = rowtotal(ksads_1_2_p ksads_1_4_p ksads_1_6_p dpr_eatingP dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP) if dsm5_mddP_nm==10, miss
egen Dsm5_mddP = rowmean(ksads_1_2_p ksads_1_4_p ksads_1_6_p dpr_eatingP dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP) if dsm5_mddP_nm==10


** Past and Present
egen dpr_eatingp = rowmax(ksads_1_172_p ksads_1_168_p ksads_1_170_p ksads_1_166_p ksads_1_171_p ksads_1_167_p ksads_1_169_p ksads_1_165_p)
egen dpr_sleepp = rowmax(ksads_22_142_p ksads_1_158_p ksads_1_157_p ksads_22_141_p)
egen dpr_motorp = rowmax(ksads_1_176_p ksads_1_174_p)
egen dpr_energyp = rowmax(ksads_1_160_p ksads_1_159_p)
egen drp_worthp = rowmax(ksads_1_182_p ksads_1_178_p ksads_1_181_p ksads_1_177_p)
egen drp_decisp = rowmax(ksads_1_164_p ksads_1_162_p ksads_1_163_p ksads_1_161_p)
egen drp_suicp = rowmax(ksads_23_150_p ksads_23_148_p ksads_23_820_p ksads_23_819_p ksads_23_818_p ksads_23_149_p ksads_23_147_p ksads_23_811_p ksads_23_815_p ksads_23_810_p ksads_23_809_p)

egen dpr_dprirrp = rowmax(ksads_1_2_p ksads_1_4_p ksads_1_1_p ksads_1_3_p)
egen dpr_eat2p = rowmax(ksads_1_170_p ksads_1_166_p ksads_1_165_p ksads_1_169_p)

* Major depressive disorder (MDD)
egen dsm5_mddp = rowtotal(ksads_1_2_p ksads_1_4_p ksads_1_6_p ksads_1_1_p ksads_1_3_p ksads_1_5_p dpr_eatingp dpr_sleepp dpr_motorp dpr_energyp drp_worthp drp_decisp drp_suicp), miss
egen Dsm5_mddp = rowmean(ksads_1_2_p ksads_1_4_p ksads_1_6_p ksads_1_1_p ksads_1_3_p ksads_1_5_p dpr_eatingp dpr_sleepp dpr_motorp dpr_energyp drp_worthp drp_decisp drp_suicp)


* Dysthymia/persistent depression
egen dsm5_dysp = rowtotal(dpr_dprirr dpr_eat2p dpr_energyp ksads_1_181_p ksads_1_182_p drp_decisp ksads_1_179_p ksads_1_180_p), miss
egen Dsm5_dysp = rowmean(dpr_dprirr dpr_eat2p dpr_energyp ksads_1_181_p ksads_1_182_p drp_decisp ksads_1_179_p ksads_1_180_p)

***************************************************************************************************************************************
*** Bipolar
** Present
egen mnc_racingt = rowmax(ksads_2_201_p ksads_2_199_p)
egen mnc_activity = rowmax(ksads_2_203_p ksads_2_207_p)

* Manic or hypomanic episode
egen dsm5_manic = rowtotal(ksads_2_195_p ksads_2_12_p ksads_2_197_p mnc_racingt ksads_2_209_p mnc_activity ksads_2_213_p), miss
egen Dsm5_manic = rowmean(ksads_2_195_p ksads_2_12_p ksads_2_197_p mnc_racingt ksads_2_209_p mnc_activity ksads_2_213_p)

** Past and Present
egen mnc_racingtp = rowmax(ksads_2_202_p ksads_2_200_p ksads_2_201_p ksads_2_199_p)
egen mnc_activityp = rowmax(ksads_2_203_p ksads_2_207_p ksads_2_204_p ksads_2_208_p)

* Manic or hypomanic episode
egen dsm5_manicp = rowtotal(ksads_2_196_p ksads_2_13_p ksads_2_198_p ksads_2_211_p ksads_2_214_p ksads_2_195_p ksads_2_12_p ksads_2_197_p mnc_racingtp ksads_2_209_p mnc_activityp ksads_2_213_p), miss
egen Dsm5_manicp = rowmean(ksads_2_196_p ksads_2_13_p ksads_2_198_p ksads_2_211_p ksads_2_214_p ksads_2_195_p ksads_2_12_p ksads_2_197_p mnc_racingtp ksads_2_209_p mnc_activityp ksads_2_213_p)


***************************************************************************************************************************************
*** Mood dysregulation (too few cases - but symptoms might still increase)
** Present
egen dsm5_moodd = rowtotal(ksads_3_229_p ksads_3_226_p), miss
egen Dsm5_moodd = rowmean(ksads_3_229_p ksads_3_226_p)


***************************************************************************************************************************************
*** Psychosis

** Present
egen delhal_drug = rowmax(ksads_4_254_p ksads_4_243_p)
egen delhal_affc = rowmax(ksads_4_252_p ksads_4_241_p)

egen dsm5_psyc = rowtotal(ksads_4_258_p ksads_4_230_p ksads_4_237_p ksads_4_239_p ksads_4_256_p ksads_4_247_p ksads_4_245_p ksads_4_16_p ksads_4_234_p ksads_4_18_p ksads_4_236_p ksads_4_235_p ksads_4_232_p ksads_4_248_p ksads_4_250_p), miss
clonevar dsm5_psycr = dsm5_psyc
replace dsm5_psycr = 0 if delhal_drug==1 | delhal_affc==1
egen Dsm5_psyc = rowmean(ksads_4_258_p ksads_4_230_p ksads_4_237_p ksads_4_239_p ksads_4_256_p ksads_4_247_p ksads_4_245_p ksads_4_16_p ksads_4_234_p ksads_4_18_p ksads_4_236_p ksads_4_235_p ksads_4_232_p ksads_4_248_p ksads_4_250_p)
clonevar Dsm5_psycr = Dsm5_psyc 
replace Dsm5_psycr = 0 if delhal_drug==1 | delhal_affc==1


** Past and Present
egen delhal_drugp = rowmax(ksads_4_255_p ksads_4_244_p ksads_4_254_p ksads_4_243_p)
egen delhal_affcp = rowmax(ksads_4_253_p ksads_4_242_p ksads_4_252_p ksads_4_241_p)

egen dsm5_psycp = rowtotal(ksads_4_259_p ksads_4_231_p ksads_4_238_p ksads_4_240_p ksads_4_257_p ksads_4_246_p ksads_4_17_p ksads_4_19_p ksads_4_249_p ksads_4_251_p ksads_4_233_p ksads_4_258_p ksads_4_230_p ksads_4_237_p ksads_4_239_p ksads_4_256_p ksads_4_247_p ksads_4_245_p ksads_4_16_p ksads_4_234_p ksads_4_18_p ksads_4_236_p ksads_4_235_p ksads_4_232_p ksads_4_248_p ksads_4_250_p), miss
clonevar dsm5_psycrp = dsm5_psycp
replace dsm5_psycrp = 0 if delhal_drugp==1 | delhal_affcp==1
egen Dsm5_psycp = rowmean(ksads_4_259_p ksads_4_231_p ksads_4_238_p ksads_4_240_p ksads_4_257_p ksads_4_246_p ksads_4_17_p ksads_4_19_p ksads_4_249_p ksads_4_251_p ksads_4_233_p ksads_4_258_p ksads_4_230_p ksads_4_237_p ksads_4_239_p ksads_4_256_p ksads_4_247_p ksads_4_245_p ksads_4_16_p ksads_4_234_p ksads_4_18_p ksads_4_236_p ksads_4_235_p ksads_4_232_p ksads_4_248_p ksads_4_250_p)
clonevar Dsm5_psycrp = Dsm5_psycp
replace Dsm5_psycrp = 0 if delhal_drugp==1 | delhal_affcp==1


** Past 
egen delhal_drugP = rowmax(ksads_4_255_p ksads_4_244_p)
egen delhal_affcP = rowmax(ksads_4_253_p ksads_4_242_p)

egen dsm5_psycP = rowtotal(ksads_4_259_p ksads_4_231_p ksads_4_238_p ksads_4_240_p ksads_4_257_p ksads_4_246_p ksads_4_17_p ksads_4_19_p ksads_4_249_p ksads_4_251_p ksads_4_233_p), miss
clonevar dsm5_psycrP = dsm5_psycP
replace dsm5_psycrP = 0 if delhal_drugP==1 | delhal_affcP==1
egen Dsm5_psycP = rowmean(ksads_4_259_p ksads_4_231_p ksads_4_238_p ksads_4_240_p ksads_4_257_p ksads_4_246_p ksads_4_17_p ksads_4_19_p ksads_4_249_p ksads_4_251_p ksads_4_233_p)
clonevar Dsm5_psycrP = Dsm5_psycp
replace Dsm5_psycrP = 0 if delhal_drugP==1 | delhal_affcP==1


***************************************************************************************************************************************
*** Separation anxiety

** Present
egen dsm5_sepx = rowtotal(ksads_7_24_p ksads_7_283_p ksads_7_281_p ksads_7_26_p ksads_7_285_p ksads_7_287_p ksads_7_289_p ksads_7_291_p), miss
egen Dsm5_sepx = rowmean(ksads_7_24_p ksads_7_283_p ksads_7_281_p ksads_7_26_p ksads_7_285_p ksads_7_287_p ksads_7_289_p ksads_7_291_p)

** Past and Present
egen dsm5_sepxp = rowtotal(ksads_7_25_p ksads_7_284_p ksads_7_282_p ksads_7_27_p ksads_7_286_p ksads_7_288_p ksads_7_290_p ksads_7_292_p ksads_7_24_p ksads_7_283_p ksads_7_281_p ksads_7_26_p ksads_7_285_p ksads_7_287_p ksads_7_289_p ksads_7_291_p), miss
egen Dsm5_sepxp = rowmean(ksads_7_25_p ksads_7_284_p ksads_7_282_p ksads_7_27_p ksads_7_286_p ksads_7_288_p ksads_7_290_p ksads_7_292_p ksads_7_24_p ksads_7_283_p ksads_7_281_p ksads_7_26_p ksads_7_285_p ksads_7_287_p ksads_7_289_p ksads_7_291_p)

** Past
egen dsm5_sepxP = rowtotal(ksads_7_25_p ksads_7_284_p ksads_7_282_p ksads_7_27_p ksads_7_286_p ksads_7_288_p ksads_7_290_p ksads_7_292_p), miss
egen Dsm5_sepxP = rowmean(ksads_7_25_p ksads_7_284_p ksads_7_282_p ksads_7_27_p ksads_7_286_p ksads_7_288_p ksads_7_290_p ksads_7_292_p)


***************************************************************************************************************************************
*** Anxiety problems

** Present
egen dsm5_anxpb = rowtotal(ksads_5_20_p ksads_5_261_p ksads_6_22_p ksads_6_272_p ksads_8_29_p ksads_8_307_p ksads_9_41_p ksads_9_34_p ksads_10_45_p ksads_10_320_p ksads_11_48_p ksads_11_50_p ksads_12_52_p ksads_12_60_p), miss
egen Dsm5_anxpb = rowmean(ksads_5_20_p ksads_5_261_p ksads_6_22_p ksads_6_272_p ksads_8_29_p ksads_8_307_p ksads_9_41_p ksads_9_34_p ksads_10_45_p ksads_10_320_p ksads_11_48_p ksads_11_50_p ksads_12_52_p ksads_12_60_p)

** Past and Present
egen dsm5_anxpbp = rowtotal(ksads_5_21_p ksads_5_262_p ksads_6_23_p ksads_6_273_p ksads_8_31_p ksads_8_308_p ksads_9_42_p ksads_9_35_p ksads_10_46_p ksads_10_321_p ksads_11_49_p ksads_11_51_p ksads_12_53_p ksads_12_61_p ksads_5_20_p ksads_5_261_p ksads_6_22_p ksads_6_272_p ksads_8_29_p ksads_8_307_p ksads_9_41_p ksads_9_34_p ksads_10_45_p ksads_10_320_p ksads_11_48_p ksads_11_50_p ksads_12_52_p ksads_12_60_p), miss
egen Dsm5_anxpbp = rowmean(ksads_5_21_p ksads_5_262_p ksads_6_23_p ksads_6_273_p ksads_8_31_p ksads_8_308_p ksads_9_42_p ksads_9_35_p ksads_10_46_p ksads_10_321_p ksads_11_49_p ksads_11_51_p ksads_12_53_p ksads_12_61_p ksads_5_20_p ksads_5_261_p ksads_6_22_p ksads_6_272_p ksads_8_29_p ksads_8_307_p ksads_9_41_p ksads_9_34_p ksads_10_45_p ksads_10_320_p ksads_11_48_p ksads_11_50_p ksads_12_52_p ksads_12_60_p)


***************************************************************************************************************************************
*** Eating disorders

** Present
egen dsm5_eatd = rowtotal(ksads_13_469_p ksads_13_470_p ksads_13_74_p ksads_13_475_p ksads_13_68_p ksads_13_66_p ksads_13_70_p ksads_13_72_p), miss
egen Dsm5_eatd = rowmean(ksads_13_469_p ksads_13_470_p ksads_13_74_p ksads_13_475_p ksads_13_68_p ksads_13_66_p ksads_13_70_p ksads_13_72_p)

** Past and Present
egen dsm5_eatdp = rowtotal(ksads_13_476_p ksads_13_477_p ksads_13_75_p ksads_13_480_p ksads_13_69_p ksads_13_67_p ksads_13_71_p ksads_13_73_p ksads_13_469_p ksads_13_470_p ksads_13_74_p ksads_13_475_p ksads_13_68_p ksads_13_66_p ksads_13_70_p ksads_13_72_p), miss
egen Dsm5_eatdp = rowmean(ksads_13_476_p ksads_13_477_p ksads_13_75_p ksads_13_480_p ksads_13_69_p ksads_13_67_p ksads_13_71_p ksads_13_73_p ksads_13_469_p ksads_13_470_p ksads_13_74_p ksads_13_475_p ksads_13_68_p ksads_13_66_p ksads_13_70_p ksads_13_72_p)


***************************************************************************************************************************************
*** ADHD

** Present
egen dsm5_adhdina_nm = rownonmiss(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p)
egen dsm5_adhdina = rowtotal(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p) if dsm5_adhdina_nm==9
egen Dsm5_adhdina = rowmean(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p) if dsm5_adhdina_nm==9

egen dsm5_adhdimp_nm = rownonmiss(ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p)
egen dsm5_adhdimp = rowtotal(ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p) if dsm5_adhdimp_nm==9
egen Dsm5_adhdimp = rowmean(ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p) if dsm5_adhdimp_nm==9

egen dsm5_adhd_nm = rownonmiss(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p)
egen dsm5_adhd = rowtotal(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p) if dsm5_adhd_nm==18
egen Dsm5_adhd = rowmean(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p) if dsm5_adhd_nm==18

** Past
egen dsm5_adhdinaP_nm = rownonmiss(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p)
egen dsm5_adhdinaP = rowtotal(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p) if dsm5_adhdinaP_nm==9
egen Dsm5_adhdinaP = rowmean(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p) if dsm5_adhdinaP_nm==9

egen dsm5_adhdimpP_nm = rownonmiss(ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p)
egen dsm5_adhdimpP = rowtotal(ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p) if dsm5_adhdimpP_nm==9
egen Dsm5_adhdimpP = rowmean(ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p) if dsm5_adhdimpP_nm==9

egen dsm5_adhdP_nm = rownonmiss(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p)

egen dsm5_adhdP = rowtotal(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p) if dsm5_adhdP_nm==18

egen Dsm5_adhdP = rowmean(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p) if dsm5_adhdP_nm==18


** Past and Present
egen dsm5_adhdinap = rowtotal(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p), miss
egen Dsm5_adhdinap = rowmean(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p)

egen dsm5_adhdimpp = rowtotal(ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p), miss
egen Dsm5_adhdimpp = rowmean(ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p)

egen dsm5_adhdp = rowtotal(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p), miss
egen Dsm5_adhdp = rowmean(ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p)


***************************************************************************************************************************************
*** ODD

** Present
egen dsm5_odd_nm = rownonmiss(ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p)
egen dsm5_odd = rowtotal(ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p) if dsm5_odd_nm==8
egen Dsm5_odd = rowmean(ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p) if dsm5_odd_nm==8

** Past and Present
egen dsm5_oddp = rowtotal(ksads_15_92_p ksads_15_439_p ksads_15_440_p ksads_15_94_p ksads_15_96_p ksads_15_442_p ksads_15_436_p ksads_15_441_p ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p), miss
egen Dsm5_oddp = rowmean(ksads_15_92_p ksads_15_439_p ksads_15_440_p ksads_15_94_p ksads_15_96_p ksads_15_442_p ksads_15_436_p ksads_15_441_p ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p)


** Past 
egen dsm5_oddP_nm = rownonmiss(ksads_15_92_p ksads_15_439_p ksads_15_440_p ksads_15_94_p ksads_15_96_p ksads_15_442_p ksads_15_436_p ksads_15_441_p)
egen dsm5_oddP = rowtotal(ksads_15_92_p ksads_15_439_p ksads_15_440_p ksads_15_94_p ksads_15_96_p ksads_15_442_p ksads_15_436_p ksads_15_441_p) if dsm5_oddP_nm==8
egen Dsm5_oddP = rowmean(ksads_15_92_p ksads_15_439_p ksads_15_440_p ksads_15_94_p ksads_15_96_p ksads_15_442_p ksads_15_436_p ksads_15_441_p) if dsm5_oddP_nm==8


***************************************************************************************************************************************
*** Conduct disorder

** Present
egen dsm5_condd_nm = rownonmiss(ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p)
egen dsm5_condd = rowtotal(ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p) if dsm5_condd_nm==15
egen Dsm5_condd = rowmean(ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p) if dsm5_condd_nm==15

** Past and Present
egen dsm5_conddp = rowtotal(ksads_16_105_p ksads_16_103_p ksads_16_460_p ksads_16_462_p ksads_16_466_p ksads_16_452_p ksads_16_464_p ksads_16_448_p ksads_16_454_p ksads_16_450_p ksads_16_99_p ksads_16_107_p ksads_16_456_p ksads_16_458_p ksads_16_101_p ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p), miss
egen Dsm5_conddp = rowmean(ksads_16_105_p ksads_16_103_p ksads_16_460_p ksads_16_462_p ksads_16_466_p ksads_16_452_p ksads_16_464_p ksads_16_448_p ksads_16_454_p ksads_16_450_p ksads_16_99_p ksads_16_107_p ksads_16_456_p ksads_16_458_p ksads_16_101_p ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p)

** Past
egen dsm5_conddP_nm = rownonmiss(ksads_16_105_p ksads_16_103_p ksads_16_460_p ksads_16_462_p ksads_16_466_p ksads_16_452_p ksads_16_464_p ksads_16_448_p ksads_16_454_p ksads_16_450_p ksads_16_99_p ksads_16_107_p ksads_16_456_p ksads_16_458_p ksads_16_101_p)
egen dsm5_conddP = rowtotal(ksads_16_105_p ksads_16_103_p ksads_16_460_p ksads_16_462_p ksads_16_466_p ksads_16_452_p ksads_16_464_p ksads_16_448_p ksads_16_454_p ksads_16_450_p ksads_16_99_p ksads_16_107_p ksads_16_456_p ksads_16_458_p ksads_16_101_p) if dsm5_conddP_nm==15
egen Dsm5_conddP = rowmean(ksads_16_105_p ksads_16_103_p ksads_16_460_p ksads_16_462_p ksads_16_466_p ksads_16_452_p ksads_16_464_p ksads_16_448_p ksads_16_454_p ksads_16_450_p ksads_16_99_p ksads_16_107_p ksads_16_456_p ksads_16_458_p ksads_16_101_p) if dsm5_conddP_nm==15



***************************************************************************************************************************************
*** Internalizing symptoms (Panic + agoraphobia + phobic disorders + separation anxiety + Social anxiety + GAD + OCD)
** Agoraphobia not available in caregiver's report
egen dsm5_ints_nm = rownonmiss(ksads_5_20_p ksads_5_261_p ksads_5_263_p ksads_5_265_p ksads_5_267_p ksads_5_269_p ksads_7_300_p ksads_7_26_p ksads_7_287_p ksads_7_291_p ksads_7_293_p ksads_7_289_p ksads_7_295_p ksads_7_24_p ksads_7_281_p ksads_7_285_p ksads_7_297_p ksads_7_283_p ksads_9_34_p ksads_9_37_p ksads_9_39_p ksads_9_41_p ksads_9_43_p ksads_8_309_p ksads_8_29_p ksads_8_30_p ksads_8_311_p ksads_8_307_p ksads_8_303_p ksads_8_301_p ksads_10_45_p ksads_10_320_p ksads_10_324_p ksads_10_328_p ksads_10_326_p ksads_10_47_p ksads_10_322_p ksads_11_343_p ksads_11_331_p ksads_11_48_p ksads_11_335_p ksads_11_341_p ksads_11_50_p ksads_11_339_p ksads_11_347_p ksads_11_333_p ksads_11_345_p ksads_11_337_p)
egen dsm5_ints = rowtotal(ksads_5_20_p ksads_5_261_p ksads_5_263_p ksads_5_265_p ksads_5_267_p ksads_5_269_p ksads_7_300_p ksads_7_26_p ksads_7_287_p ksads_7_291_p ksads_7_293_p ksads_7_289_p ksads_7_295_p ksads_7_24_p ksads_7_281_p ksads_7_285_p ksads_7_297_p ksads_7_283_p ksads_9_34_p ksads_9_37_p ksads_9_39_p ksads_9_41_p ksads_9_43_p ksads_8_309_p ksads_8_29_p ksads_8_30_p ksads_8_311_p ksads_8_307_p ksads_8_303_p ksads_8_301_p ksads_10_45_p ksads_10_320_p ksads_10_324_p ksads_10_328_p ksads_10_326_p ksads_10_47_p ksads_10_322_p ksads_11_343_p ksads_11_331_p ksads_11_48_p ksads_11_335_p ksads_11_341_p ksads_11_50_p ksads_11_339_p ksads_11_347_p ksads_11_333_p ksads_11_345_p ksads_11_337_p) if dsm5_ints_nm==48
egen Dsm5_ints = rowmean(ksads_5_20_p ksads_5_261_p ksads_5_263_p ksads_5_265_p ksads_5_267_p ksads_5_269_p ksads_7_300_p ksads_7_26_p ksads_7_287_p ksads_7_291_p ksads_7_293_p ksads_7_289_p ksads_7_295_p ksads_7_24_p ksads_7_281_p ksads_7_285_p ksads_7_297_p ksads_7_283_p ksads_9_34_p ksads_9_37_p ksads_9_39_p ksads_9_41_p ksads_9_43_p ksads_8_309_p ksads_8_29_p ksads_8_30_p ksads_8_311_p ksads_8_307_p ksads_8_303_p ksads_8_301_p ksads_10_45_p ksads_10_320_p ksads_10_324_p ksads_10_328_p ksads_10_326_p ksads_10_47_p ksads_10_322_p ksads_11_343_p ksads_11_331_p ksads_11_48_p ksads_11_335_p ksads_11_341_p ksads_11_50_p ksads_11_339_p ksads_11_347_p ksads_11_333_p ksads_11_345_p ksads_11_337_p) if dsm5_ints_nm==48


***************************************************************************************************************************************
*** Externalizing symptoms (CD + ODD + ADHD)
egen dsm5_exts_nm = rownonmiss(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p)
egen dsm5_exts = rowtotal(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p) if dsm5_exts_nm==41
egen Dsm5_exts = rowmean(ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p ksads_15_91_p ksads_15_432_p ksads_15_433_p ksads_15_93_p ksads_15_95_p ksads_15_435_p ksads_15_436_p ksads_15_434_p ksads_16_104_p ksads_16_102_p ksads_16_459_p ksads_16_461_p ksads_16_465_p ksads_16_451_p ksads_16_463_p ksads_16_447_p ksads_16_453_p ksads_16_449_p ksads_16_98_p ksads_16_106_p ksads_16_455_p ksads_16_457_p ksads_16_100_p) if dsm5_exts_nm==41


***************************************************************************************************************************************
*** Suicide ideation/plan/attempt

** Present
egen dsm5_suicb = rowtotal(ksads_23_813_p ksads_23_814_p ksads_23_809_p ksads_23_149_p ksads_23_812_p ksads_23_808_p ksads_23_807_p ksads_23_143_p ksads_23_145_p ksads_23_810_p ksads_23_147_p ksads_23_811_p ksads_23_815_p), miss
egen Dsm5_suicb = rowmean(ksads_23_813_p ksads_23_814_p ksads_23_809_p ksads_23_149_p ksads_23_812_p ksads_23_808_p ksads_23_807_p ksads_23_143_p ksads_23_145_p ksads_23_810_p ksads_23_147_p ksads_23_811_p ksads_23_815_p)

** Past and Present
egen dsm5_suicbp = rowtotal(ksads_23_822_p ksads_23_144_p ksads_23_818_p ksads_23_825_p ksads_23_150_p ksads_23_821_p ksads_23_817_p ksads_23_816_p ksads_23_146_p ksads_23_819_p ksads_23_148_p ksads_23_820_p ksads_23_824_p ksads_23_813_p ksads_23_814_p ksads_23_809_p ksads_23_149_p ksads_23_812_p ksads_23_808_p ksads_23_807_p ksads_23_143_p ksads_23_145_p ksads_23_810_p ksads_23_147_p ksads_23_811_p ksads_23_815_p), miss
egen Dsm5_suicbp = rowmean(ksads_23_822_p ksads_23_144_p ksads_23_818_p ksads_23_825_p ksads_23_150_p ksads_23_821_p ksads_23_817_p ksads_23_816_p ksads_23_146_p ksads_23_819_p ksads_23_148_p ksads_23_820_p ksads_23_824_p ksads_23_813_p ksads_23_814_p ksads_23_809_p ksads_23_149_p ksads_23_812_p ksads_23_808_p ksads_23_807_p ksads_23_143_p ksads_23_145_p ksads_23_810_p ksads_23_147_p ksads_23_811_p ksads_23_815_p)

** Past 
egen dsm5_suicbP = rowtotal(ksads_23_822_p ksads_23_144_p ksads_23_818_p ksads_23_825_p ksads_23_150_p ksads_23_821_p ksads_23_817_p ksads_23_816_p ksads_23_146_p ksads_23_819_p ksads_23_148_p ksads_23_820_p ksads_23_824_p), miss
egen Dsm5_suicbP = rowmean(ksads_23_822_p ksads_23_144_p ksads_23_818_p ksads_23_825_p ksads_23_150_p ksads_23_821_p ksads_23_817_p ksads_23_816_p ksads_23_146_p ksads_23_819_p ksads_23_148_p ksads_23_820_p ksads_23_824_p)


global dsm5p_axdppy "ksads_1_843_p ksads_1_840_p ksads_1_846_p ksads_2_835_p ksads_2_836_p ksads_2_831_p ksads_2_832_p ksads_2_830_p ksads_2_838_p ksads_3_848_p ksads_4_826_p ksads_4_828_p ksads_4_851_p ksads_4_849_p ksads_5_906_p ksads_5_857_p ksads_6_859_p ksads_6_908_p ksads_7_861_p ksads_7_909_p ksads_8_863_p ksads_8_911_p ksads_9_867_p ksads_10_913_p ksads_10_869_p ksads_11_917_p ksads_11_919_p ksads_12_925_p ksads_12_927_p ksads_13_938_p ksads_13_929_p ksads_13_932_p ksads_13_930_p ksads_13_935_p ksads_13_942_p ksads_13_941_p ksads_17_904_p ksads_21_923_p ksads_21_921_p ksads_21_923_p ksads_25_915_p ksads_25_865_p"
global dsm5p_devdis "ksads_14_853_p ksads_18_903_p"
global dsm5p_condis "ksads_15_901_p ksads_16_897_p ksads_16_898_p"
global dsm5p_subdis "ksads_19_891_p ksads_19_895_p ksads_20_893_p ksads_20_874_p ksads_20_872_p ksads_20_889_p ksads_20_878_p ksads_20_877_p ksads_20_875_p ksads_20_876_p ksads_20_879_p ksads_20_873_p ksads_20_871_p"
global dsm5p_suibeh "ksads_23_946_p ksads_23_954_p ksads_23_945_p ksads_23_950_p ksads_23_947_p ksads_23_948_p ksads_23_949_p ksads_23_952_p ksads_23_955_p ksads_23_951_p ksads_23_953_p ksads_24_967_p"

global dsm5_MDD "ksads_1_1_p ksads_1_3_p ksads_1_5_p ksads_22_141_p ksads_23_143_p ksads_23_145_p ksads_23_147_p ksads_23_149_p ksads_1_157_p ksads_1_159_p ksads_1_161_p ksads_1_163_p ksads_1_165_p ksads_1_167_p ksads_1_169_p ksads_1_171_p ksads_1_173_p ksads_1_175_p ksads_1_177_p ksads_1_179_p ksads_1_181_p ksads_1_183_p ksads_22_142_p ksads_23_144_p ksads_23_146_p ksads_23_148_p ksads_23_150_p ksads_1_158_p ksads_1_160_p ksads_1_162_p ksads_1_164_p ksads_1_166_p ksads_1_168_p ksads_1_170_p ksads_1_172_p ksads_1_174_p ksads_1_176_p ksads_1_178_p ksads_1_180_p ksads_1_182_p ksads_1_184_p"

global dsm5_ADHD "ksads_14_394_p ksads_14_76_p ksads_14_395_p ksads_14_396_p ksads_14_397_p ksads_14_398_p ksads_14_399_p ksads_14_80_p ksads_14_400_p ksads_14_401_p ksads_14_84_p ksads_14_402_p ksads_14_404_p ksads_14_403_p ksads_14_408_p ksads_14_405_p ksads_14_406_p ksads_14_407_p ksads_14_410_p ksads_14_78_p ksads_14_411_p ksads_14_412_p ksads_14_413_p ksads_14_414_p ksads_14_415_p ksads_14_82_p ksads_14_416_p ksads_14_417_p ksads_14_86_p ksads_14_418_p ksads_14_420_p ksads_14_419_p ksads_14_424_p ksads_14_421_p ksads_14_422_p ksads_14_423_p"


*xml_tab SUPP1, save("$results\Supplement1_Caregivers.xml") t(Symptom Data Availability) replace


keep 	src_subject_id eventname												///
		$dsm5p_axdppy $dsm5p_devdis $dsm5p_condis $dsm5p_subdis $dsm5p_suibeh	///
		ksads_22_969_p delusionsp_c psychotic_c hallucina_c uschizoph_c			///
		bpimanic_c bpidepre_c bpihyman_c bpiidepre_c bpiihyman_c bpunspec_c		///
		Dsm5_mdd Dsm5_dys Dsm5_manic Dsm5_psyc Dsm5_psycr Dsm5_sepx 			///
		Dsm5_anxpb Dsm5_eatd Dsm5_adhdina Dsm5_adhdimp Dsm5_adhd Dsm5_odd 		///
		Dsm5_condd Dsm5_suicb 													///
		Dsm5_mddp Dsm5_dysp Dsm5_manicp Dsm5_psycp  							///
		Dsm5_sepxp Dsm5_anxpbp Dsm5_eatdp Dsm5_adhdinap Dsm5_adhdimpp 			///
		Dsm5_adhdp Dsm5_oddp Dsm5_conddp Dsm5_suicbp							///
		dsm5_mdd dsm5_dys dsm5_manic dsm5_psyc dsm5_psycr dsm5_sepx 			///
		dsm5_anxpb dsm5_eatd dsm5_adhdina dsm5_adhdimp dsm5_adhd dsm5_odd 		///
		dsm5_condd dsm5_suicb 													///
		dsm5_mddp dsm5_dysp dsm5_manicp dsm5_psycp  							///
		dsm5_sepxp dsm5_anxpbp dsm5_eatdp dsm5_adhdinap dsm5_adhdimpp 			///
		dsm5_adhdp dsm5_oddp dsm5_conddp dsm5_suicbp dpr_eating dpr_sleep 		///
		dpr_motor dpr_energy drp_worth drp_decis drp_suic mdd_first mdd_second 	///
		mdd_diagn dsm5_mddvm $dsm5_MDD $dsm5_ADHD dsm5_adhdinaP Dsm5_adhdinaP 	///
		dsm5_adhdimpP Dsm5_adhdimpP dsm5_adhdP Dsm5_adhdP dsm5_mddvmP 			///
		dsm5_mddP Dsm5_mddP ksads_1_2_p ksads_1_4_p ksads_1_6_p dpr_eatingP 	///
		dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP		///
		dsm5_sepxP Dsm5_sepxP delhal_drugP delhal_affcP delhal_drug delhal_affc ///
		delhal_drugp delhal_affcp dsm5_psycr Dsm5_psycr dsm5_psycrp Dsm5_psycrp ///
		dsm5_psycrP Dsm5_psycrP Dsm5_psycP dsm5_oddP Dsm5_oddP dsm5_suicbP 		///
		Dsm5_suicbP dsm5_conddP Dsm5_conddP dsm5_exts Dsm5_exts dsm5_ints		///
		Dsm5_ints
*Dsm5_psycprp dsm5_psycprp
		
tempfile dsm5dgnsp
save `dsm5dgnsp'


**************************************************************************************
*************************************** 30 *******************************************
**************************************************************************************
**** Loading data on mental health diagnoses and symptoms (youth report)
/*clear
import delimited "$data\abcd_ksad501.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1


** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

/*
** Diagnoses
* Ideation-passive
rename ksads_23_946_t sideapy_c
rename ksads_23_957_t sideapy_p

* Ideation-active on specific
rename ksads_23_947_t sideaay_c
rename ksads_23_958_t sideaay_p

* Ideation-active method
rename ksads_23_948_t sideamy_c
rename ksads_23_959_t sideamy_p

* Ideation-active intent
rename ksads_23_949_t sideaiy_c
rename ksads_23_960_t sideaiy_p

* Ideation-active plan
rename ksads_23_950_t sidealy_c
rename ksads_23_961_t sidealy_p

* Attempt
rename ksads_23_953_t sattemy_c
rename ksads_23_965_t sattemy_p
*/

clear
import delimited "$data5p1\mental-health\mh_y_ksads_ss.csv"

*** Symptoms
global depress_present "ksads_1_171_t ksads_1_167_t ksads_1_3_t ksads_1_163_t ksads_1_169_t ksads_1_175_t ksads_1_173_t ksads_1_181_t ksads_1_165_t ksads_1_1_t ksads_1_5_t ksads_1_161_t ksads_1_159_t ksads_1_183_t ksads_1_157_t ksads_1_177_t ksads_1_179_t ksads_22_141_t ksads_23_149_t ksads_23_147_t"
global bipolar_present "ksads_2_12_t ksads_2_14_t ksads_2_7_t ksads_2_9_t ksads_2_189_t ksads_2_191_t ksads_2_192_t ksads_2_195_t ksads_2_197_t ksads_2_199_t ksads_2_201_t ksads_2_203_t ksads_2_205_t ksads_2_207_t ksads_2_209_t ksads_2_210_t ksads_2_213_t ksads_2_215_t ksads_2_217_t ksads_2_219_t"
global mooddys_present "ksads_3_226_t ksads_3_227_t ksads_3_228_t ksads_3_229_t"
global psychos_present "ksads_4_258_t ksads_4_230_t ksads_4_252_t ksads_4_254_t ksads_4_243_t ksads_4_241_t ksads_4_237_t ksads_4_239_t ksads_4_256_t ksads_4_247_t ksads_4_245_t ksads_4_16_t ksads_4_234_t ksads_4_18_t ksads_4_236_t ksads_4_235_t ksads_4_232_t ksads_4_248_t ksads_4_250_t"
global panic_present "ksads_5_261_t ksads_5_263_t ksads_5_20_t ksads_5_265_t ksads_5_267_t ksads_5_269_t"
global agaroph_present "ksads_6_274_t ksads_6_276_t ksads_6_278_t ksads_6_22_t ksads_6_272_t"
global phobias_present "ksads_9_34_t ksads_9_37_t ksads_9_39_t ksads_9_41_t ksads_9_43_t"
global sepanx_present "ksads_7_300_t ksads_7_26_t ksads_7_287_t ksads_7_291_t ksads_7_293_t ksads_7_289_t ksads_7_295_t ksads_7_24_t ksads_7_281_t ksads_7_285_t ksads_7_297_t ksads_7_283_t"
global socanx_present "ksads_8_309_t ksads_8_29_t ksads_8_30_t ksads_8_311_t ksads_8_307_t ksads_8_303_t ksads_8_301_t"
global genanx_present "ksads_10_45_t ksads_10_320_t ksads_10_324_t ksads_10_328_t ksads_10_326_t ksads_10_47_t ksads_10_322_t"
global obscomp_present "ksads_11_343_t ksads_11_331_t ksads_11_48_t ksads_11_335_t ksads_11_341_t ksads_11_50_t ksads_11_339_t ksads_11_347_t ksads_11_333_t ksads_11_345_t ksads_11_337_t"
global social_probprpt "ksads_5_20_t ksads_5_261_t ksads_6_22_t ksads_6_272_t ksads_8_29_t ksads_8_307_t ksads_9_41_t ksads_9_34_t ksads_10_45_t ksads_10_320_t ksads_11_48_t ksads_11_50_t ksads_12_52_t ksads_12_56_t ksads_12_60_t ksads_12_62_t"
global eatdis_present "ksads_13_469_t ksads_13_470_t ksads_13_473_t ksads_13_471_t ksads_13_74_t ksads_13_472_t ksads_13_475_t ksads_13_68_t ksads_13_66_t ksads_13_70_t ksads_13_72_t"
global adhd_present "ksads_14_398_t ksads_14_403_t ksads_14_405_t ksads_14_406_t ksads_14_76_t ksads_14_396_t ksads_14_397_t ksads_14_404_t ksads_14_85_t ksads_14_77_t ksads_14_84_t ksads_14_401_t ksads_14_80_t ksads_14_81_t ksads_14_400_t ksads_14_88_t ksads_14_399_t ksads_14_408_t ksads_14_394_t ksads_14_395_t ksads_14_407_t ksads_14_402_t ksads_14_409_t ksads_14_429_t"
global oppdef_present "ksads_15_95_t ksads_15_436_t ksads_15_435_t ksads_15_433_t ksads_15_93_t ksads_15_432_t ksads_15_91_t ksads_15_438_t ksads_15_437_t ksads_15_434_t"
global condu_present "ksads_16_449_t ksads_16_463_t ksads_16_453_t ksads_16_461_t ksads_16_465_t ksads_16_98_t ksads_16_104_t ksads_16_102_t ksads_16_457_t ksads_16_455_t ksads_16_451_t ksads_16_106_t ksads_16_100_t ksads_16_447_t ksads_16_459_t"
global autism_present "ksads_18_116_t ksads_18_112_t ksads_18_114_t"
global alcoh_present "ksads_19_500_t ksads_19_499_t ksads_19_508_t ksads_19_493_t ksads_19_481_t ksads_19_501_t ksads_19_483_t ksads_19_491_t ksads_19_489_t ksads_19_485_t ksads_19_497_t ksads_19_122_t ksads_19_495_t ksads_19_120_t ksads_19_498_t ksads_19_486_t"
global drugs_present "ksads_20_126_t ksads_20_545_t ksads_20_590_t ksads_20_620_t ksads_20_665_t ksads_20_575_t ksads_20_650_t ksads_20_560_t ksads_20_521_t ksads_20_635_t" /* There are more symptoms to be added */
global suic_present "ksads_23_807_t ksads_23_808_t ksads_23_809_t ksads_23_810_t ksads_23_811_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t ksads_23_143_t ksads_23_145_t"
global symp_past "ksads_22_142_t ksads_1_158_t ksads_1_160_t ksads_1_182_t ksads_1_178_t ksads_1_164_t ksads_1_162_t ksads_23_150_t ksads_23_148_t ksads_23_820_t ksads_23_819_t ksads_23_818_t ksads_1_170_t ksads_1_166_t ksads_1_2_t ksads_1_4_t ksads_1_6_t ksads_1_1_t ksads_1_172_t ksads_1_168_t ksads_1_170_t ksads_1_166_t ksads_4_259_t ksads_4_231_t ksads_4_238_t ksads_4_240_t ksads_4_257_t ksads_4_246_t ksads_4_17_t ksads_4_19_t ksads_4_249_t ksads_4_251_t ksads_4_233_t ksads_2_196_t ksads_2_13_t ksads_2_198_t ksads_2_211_t ksads_2_214_t ksads_2_203_t ksads_2_207_t ksads_7_25_t ksads_7_284_t ksads_7_282_t ksads_7_27_t ksads_7_286_t ksads_7_288_t ksads_7_290_t ksads_7_292_t ksads_5_21_t ksads_5_262_t ksads_6_23_t ksads_6_273_t ksads_8_31_t ksads_8_308_t ksads_9_42_t ksads_9_35_t ksads_10_46_t ksads_10_321_t ksads_11_49_t ksads_11_51_t ksads_12_53_t ksads_12_61_t ksads_13_476_t ksads_13_477_t ksads_13_75_t ksads_13_480_t ksads_13_69_t ksads_13_67_t ksads_13_71_t ksads_13_73_t ksads_14_410_t ksads_14_78_t ksads_14_411_t ksads_14_412_t ksads_14_414_t ksads_14_415_t ksads_14_82_t ksads_14_416_t ksads_14_417_t ksads_14_86_t ksads_14_418_t ksads_14_420_t ksads_14_419_t ksads_14_424_t ksads_14_421_t ksads_14_422_t ksads_14_423_t ksads_15_92_t ksads_15_439_t ksads_15_440_t ksads_15_94_t ksads_15_96_t ksads_15_442_t ksads_15_436_t ksads_15_441_t ksads_16_105_t ksads_16_103_t ksads_16_460_t ksads_16_462_t ksads_16_466_t ksads_16_452_t ksads_16_464_t ksads_16_448_t ksads_16_454_t ksads_16_450_t ksads_16_99_t ksads_16_107_t ksads_16_456_t ksads_16_458_t ksads_16_101_t ksads_23_822_t ksads_23_144_t ksads_23_818_t ksads_23_825_t ksads_23_150_t ksads_23_821_t ksads_23_817_t ksads_23_816_t ksads_23_146_t ksads_23_819_t ksads_23_148_t ksads_23_820_t ksads_23_824_t ksads_2_204_t ksads_2_208_t ksads_2_202_t ksads_2_200_t ksads_1_180_t ksads_1_184_t ksads_1_174_t ksads_1_176_t ksads2_3_212_t ksads2_3_213_t ksads2_3_214_t ksads2_3_215_t ksads_8_31_t ksads_8_313a_t ksads_8_302_t ksads_8_304_t ksads_8_308_t ksads_8_310_t ksads_8_312_t ksads_8_863_t ksads_8_864_t ksads_10_46_t ksads_10_330_t ksads_10_321_t ksads_10_323_t ksads_10_325_t ksads_10_327_t ksads_10_329_t ksads_10_869_t ksads_10_870_t ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_833_t ksads_2_835_t ksads_2_836_t ksads_2_838_t ksads_1_840_t ksads_1_843_t ksads_1_842_t ksads_1_845_t ksads_2_834_t ksads_2_837_t ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_23_816_t ksads_23_817_t ksads_23_818_t ksads_23_819_t ksads_23_820_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t"

foreach v of varlist $depress_present $bipolar_present $mooddys_present $psychos_present $panic_present $agaroph_present $phobias_present $sepanx_present $socanx_present $genanx_present $obscomp_present $social_probprpt $eatdis_present $adhd_present $oppdef_present $condu_present $autism_present $alcoh_present $drugs_present $suic_present $symp_past{
	replace `v'=. if `v'==555 			/* No answered because failed to answer Level 1 screening */
	replace `v'=0 if `v'==888			/* No answered because failed to pass Level 1 screening */
}


*****************************************************
*****************************************************
* Major depressive disorder (MDD)

**********
** Present
egen dpr_eating = rowmax(ksads_1_171_t ksads_1_167_t ksads_1_169_t ksads_1_165_t)
egen dpr_sleep = rowmax(ksads_1_157_t ksads_22_141_t)
egen dpr_motor = rowmax(ksads_1_175_t ksads_1_173_t)
egen dpr_energy = rowmax(ksads_1_159_t)
egen drp_worth = rowmax(ksads_1_181_t ksads_1_177_t)
egen drp_decis = rowmax(ksads_1_163_t ksads_1_161_t)
egen drp_suic = rowmax(ksads_23_149_t ksads_23_147_t ksads_23_811_t ksads_23_815_t ksads_23_810_t ksads_23_809_t)

egen dsm5y_mdd_nm = rownonmiss(ksads_1_1_t ksads_1_3_t ksads_1_5_t dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic)
egen dsm5y_mdd = rowtotal(ksads_1_1_t ksads_1_3_t ksads_1_5_t dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic) if dsm5y_mdd_nm==10, miss
egen Dsm5y_mdd = rowmean(ksads_1_1_t ksads_1_3_t ksads_1_5_t dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic) if dsm5y_mdd_nm==10

egen mddy_first = rowmax(ksads_1_1_t ksads_1_3_t ksads_1_5_t)
egen mddy_second = rowtotal(dpr_eating dpr_sleep dpr_motor dpr_energy drp_worth drp_decis drp_suic)
gen mddy_diagn = (mddy_first==1 & dsm5y_mdd>=5) if dsm5y_mdd<.

/* Diagnosed: ksads_1_840_t */

*******
** Past
egen dpr_eatingP = rowmax(ksads_1_172_t ksads_1_168_t ksads_1_170_t ksads_1_166_t)
egen dpr_sleepP = rowmax(ksads_22_142_t ksads_1_158_t)
egen dpr_motorP = rowmax(ksads_1_176_t ksads_1_174_t)
egen dpr_energyP = rowmax(ksads_1_160_t)
egen drp_worthP = rowmax(ksads_1_182_t ksads_1_178_t)
egen drp_decisP = rowmax(ksads_1_164_t ksads_1_162_t)
egen drp_suicP = rowmax(ksads_23_150_t ksads_23_148_t ksads_23_820_t ksads_23_819_t ksads_23_818_t)


* Major depressive disorder (MDD)
egen dsm5y_mddP_nm = rownonmiss(ksads_1_2_t ksads_1_4_t ksads_1_6_t dpr_eatingP dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP)
egen dsm5y_mddP = rowtotal(ksads_1_2_t ksads_1_4_t ksads_1_6_t dpr_eatingP dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP) if dsm5y_mddP_nm==10, miss
egen Dsm5y_mddP = rowmean(ksads_1_2_t ksads_1_4_t ksads_1_6_t dpr_eatingP dpr_sleepP dpr_motorP dpr_energyP drp_worthP drp_decisP drp_suicP) if dsm5y_mddP_nm==10

/* Diagnosed: ksads_1_842_t */


*******************************************************
*******************************************************
* Dysthymia/persistent depression

**********
** Present

egen dpr_dprirr = rowmax(ksads_1_1_t ksads_1_3_t)
egen dpr_eat2 = rowmax(ksads_1_165_t ksads_1_169_t)

egen dsm5y_dys_nm = rownonmiss(dpr_dprirr dpr_eat2 dpr_sleep dpr_energy ksads_1_181_t drp_decis ksads_1_179_t)
egen dsm5y_dys = rowtotal(dpr_dprirr dpr_eat2 dpr_sleep dpr_energy ksads_1_181_t drp_decis ksads_1_179_t) if dsm5y_dys_nm==7, miss
egen Dsm5y_dys = rowmean(dpr_dprirr dpr_eat2 dpr_sleep dpr_energy ksads_1_181_t drp_decis ksads_1_179_t) if dsm5y_dys_nm==7

/* Diagnosed: ksads_1_843_t */

*******
** Past 
egen dpr_dprirrP = rowmax(ksads_1_2_t ksads_1_4_t)
egen dpr_eat2P = rowmax(ksads_1_170_t ksads_1_166_t)

egen dsm5y_dysP_nm = rownonmiss(dpr_dprirrP dpr_eat2P dpr_sleepP dpr_energyP ksads_1_182_t drp_decisP ksads_1_180_t)
egen dsm5y_dysP = rowtotal(dpr_dprirrP dpr_eat2P dpr_sleepP dpr_energyP ksads_1_182_t drp_decisP ksads_1_180_t) if dsm5y_dysP_nm==7, miss
egen Dsm5y_dysP = rowmean(dpr_dprirrP dpr_eat2P dpr_sleepP dpr_energyP ksads_1_182_t drp_decisP ksads_1_180_t) if dsm5y_dysP_nm==7

/* Diagnosed: ksads_1_845_t */


*******************************************************
*******************************************************
* Sum of all symptoms of depression (Vargas and Mittal)

**********
** Present

egen drp_selfinj = rowmax(ksads_23_807_t ksads_23_808_t)
egen dsm5y_mddvm_m = rowtotal(ksads_1_1_t ksads_1_3_t ksads_1_5_t ksads_22_141_t ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_1_157_t ksads_1_159_t ksads_1_161_t ksads_1_163_t ksads_1_165_t ksads_1_167_t ksads_1_169_t ksads_1_171_t ksads_1_173_t ksads_1_175_t ksads_1_177_t ksads_1_179_t ksads_1_181_t ksads_1_183_t), miss

egen dsm5y_mddvm_nm = rownonmiss(ksads_1_1_t ksads_1_3_t ksads_1_5_t ksads_22_141_t ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_1_157_t ksads_1_159_t ksads_1_161_t ksads_1_163_t ksads_1_165_t ksads_1_167_t ksads_1_169_t ksads_1_171_t ksads_1_173_t ksads_1_175_t ksads_1_177_t ksads_1_179_t ksads_1_181_t ksads_1_183_t)

egen dsm5y_mddvm = rowtotal(ksads_1_1_t ksads_1_3_t ksads_1_5_t ksads_22_141_t ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_1_157_t ksads_1_159_t ksads_1_161_t ksads_1_163_t ksads_1_165_t ksads_1_167_t ksads_1_169_t ksads_1_171_t ksads_1_173_t ksads_1_175_t ksads_1_177_t ksads_1_179_t ksads_1_181_t ksads_1_183_t) if dsm5y_mddvm_nm==22

*******
** Past
egen drpy_selfinjP = rowmax(ksads_23_816_t ksads_23_817_t)

egen dsm5y_mddvmP_nm = rownonmiss(ksads_1_2_t ksads_1_4_t ksads_1_6_t ksads_22_142_t ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_1_158_t ksads_1_160_t ksads_1_162_t ksads_1_164_t ksads_1_166_t ksads_1_168_t ksads_1_170_t ksads_1_172_t ksads_1_174_t ksads_1_176_t ksads_1_178_t ksads_1_180_t ksads_1_182_t ksads_1_184_t) 

egen dsm5y_mddvmP = rowtotal(ksads_1_2_t ksads_1_4_t ksads_1_6_t ksads_22_142_t ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_1_158_t ksads_1_160_t ksads_1_162_t ksads_1_164_t ksads_1_166_t ksads_1_168_t ksads_1_170_t ksads_1_172_t ksads_1_174_t ksads_1_176_t ksads_1_178_t ksads_1_180_t ksads_1_182_t ksads_1_184_t) if dsm5y_mddvmP_nm==22


***************************************************************************************************************************************
*** Bipolar

describe $bipolar_present
/*
	ksads_2_12_t:	Decreased Need for Sleep
	ksads_2_14_t:	Hypersexuality

	ksads_2_7_t:	Elevated Mood
	ksads_2_9_t:	Explosive Irritability	
	
	ksads_2_189_t:	Preliminary substance rule out bipolar
	ksads_2_191_t:	Elevated / Euphoric Mood
	ksads_2_192_t:	Manic Irritability
	ksads_2_195_t:	Grandiosity
	ksads_2_197_t:	Pressured Speech
	ksads_2_199_t:	Racing Thoughts
	ksads_2_201_t:	Flight of Ideas
	ksads_2_203_t:	Increased Goal Directed Activity
	ksads_2_205_t:	Increased Energy
	ksads_2_207_t:	Psychomotor Agitation
	ksads_2_209_t:	Distractibility
	ksads_2_210_t:	Increased Distractibility
	ksads_2_213_t:	Excessive Involvement in high risk activities
	ksads_2_215_t:	Impairment in functioning due to bipolar
	ksads_2_217_t:	Hospitalized due to Bipolar Disorder
	ksads_2_219_t:	Lasting at least one week						(Mania)
	ksads_2_221_t:	Lasting at least 4 days  						(Hypomania)
	
*/

**********
** Present
egen mnc_racingt = rowmax(ksads_2_201_t ksads_2_199_t)
egen mnc_activity = rowmax(ksads_2_203_t ksads_2_207_t)

* Manic or hypomanic episode
egen dsm5y_mania_nm = rownonmiss(ksads_2_12_t ksads_2_14_t ksads_2_7_t ksads_2_9_t ksads_2_189_t ksads_2_191_t ksads_2_192_t ksads_2_195_t ksads_2_197_t ksads_2_199_t ksads_2_201_t ksads_2_203_t ksads_2_205_t ksads_2_207_t ksads_2_209_t ksads_2_210_t ksads_2_213_t ksads_2_215_t ksads_2_217_t)

egen dsm5y_mania = rowtotal(ksads_2_12_t ksads_2_14_t ksads_2_7_t ksads_2_9_t ksads_2_189_t ksads_2_191_t ksads_2_192_t ksads_2_195_t ksads_2_197_t ksads_2_199_t ksads_2_201_t ksads_2_203_t ksads_2_205_t ksads_2_207_t ksads_2_209_t ksads_2_210_t ksads_2_213_t ksads_2_215_t ksads_2_217_t) if dsm5y_mania_nm==19

egen Dsm5y_mania = rowmean(ksads_2_12_t ksads_2_14_t ksads_2_7_t ksads_2_9_t ksads_2_189_t ksads_2_191_t ksads_2_192_t ksads_2_195_t ksads_2_197_t ksads_2_199_t ksads_2_201_t ksads_2_203_t ksads_2_205_t ksads_2_207_t ksads_2_209_t ksads_2_210_t ksads_2_213_t ksads_2_215_t ksads_2_217_t) if dsm5y_mania_nm==19

/* Diagnoses: ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_835_t ksads_2_836_t */

*******
** Past

egen dsm5y_maniaP_nm = rownonmiss(ksads_2_13_t ksads_2_15_t ksads_2_8_t ksads_2_10_t ksads_2_190_t ksads_2_193_t ksads_2_194_t ksads_2_196_t ksads_2_198_t ksads_2_200_t ksads_2_202_t ksads_2_204_t ksads_2_206_t ksads_2_208_t ksads_2_211_t ksads_2_212_t ksads_2_214_t ksads_2_216_t ksads_2_218_t)

egen dsm5y_maniaP = rowtotal(ksads_2_13_t ksads_2_15_t ksads_2_8_t ksads_2_10_t ksads_2_190_t ksads_2_193_t ksads_2_194_t ksads_2_196_t ksads_2_198_t ksads_2_200_t ksads_2_202_t ksads_2_204_t ksads_2_206_t ksads_2_208_t ksads_2_211_t ksads_2_212_t ksads_2_214_t ksads_2_216_t ksads_2_218_t) if dsm5y_maniaP_nm==19

egen Dsm5y_maniaP = rowmean(ksads_2_13_t ksads_2_15_t ksads_2_8_t ksads_2_10_t ksads_2_190_t ksads_2_193_t ksads_2_194_t ksads_2_196_t ksads_2_198_t ksads_2_200_t ksads_2_202_t ksads_2_204_t ksads_2_206_t ksads_2_208_t ksads_2_211_t ksads_2_212_t ksads_2_214_t ksads_2_216_t ksads_2_218_t) if dsm5y_maniaP_nm==19

/* Diagnoses: ksads_2_833_t ksads_2_834_t ksads_2_837_t */

***************************************************************************************************************************************
*** Disruptive Mood Dysregulation

describe ksads_3_226_t ksads_3_227_t ksads_3_228_t ksads_3_229_t
/*
	ksads_3_226_t:  Temper outbursts occur 3 or more times per week						- DSM-5 Criteria C (to assess evidence of Disruptive Mood Dysregulation Disorder)
	ksads_3_227_t:	Present at least 12 months 											- DSM-5 Criteria E
	ksads_3_228_t:	No 3 month period without symptoms									- DSM-5 Criteria D (Similar to D, describing persistency of irritability)
	ksads_3_229_t:	Temper/irritability present in at least 2 settings					- DSM-5 Criteria F
	
	DSM-5 Criteria not in the database:
	- A: Severe recurrent temper outbursts verbal or behavioral that are grossly out of proportion to the situation or provocation
	- B: Temper outbursts are inconsistent with developmental level
	- G: Not before age 6 or after 18 
	- H: Age of onset before 10
	- I Manic or hypomanic are not met
	- J: Not explained by other mental health disorders
	- I: Not explained by physiological effects of a substance or another medical or neurological condition
*/

**********
** Present

egen dsm5y_moodd_nm = rownonmiss(ksads_3_226_t ksads_3_227_t ksads_3_228_t ksads_3_229_t)
egen dsm5y_moodd = rowtotal(ksads_3_226_t ksads_3_227_t ksads_3_228_t ksads_3_229_t) if dsm5y_moodd_nm==4
egen Dsm5y_moodd = rowmean(ksads_3_226_t ksads_3_227_t ksads_3_228_t ksads_3_229_t) if dsm5y_moodd_nm==4

/* Diagnosis ksads_3_848_t */

*******
** Past - No data

egen dsm5y_mooddP_nm = rownonmiss(ksads2_3_212_t ksads2_3_213_t ksads2_3_214_t ksads2_3_215_t)
egen dsm5y_mooddP = rowtotal(ksads2_3_212_t ksads2_3_213_t ksads2_3_214_t ksads2_3_215_t) if dsm5y_mooddP_nm==4
egen Dsm5y_mooddP = rowmean(ksads2_3_212_t ksads2_3_213_t ksads2_3_214_t ksads2_3_215_t) if dsm5y_mooddP_nm==4

/* Diagnosis ksads2_3_804_t */


***************************************************************************************************************************************
*** Social Anxiety

describe $socanx_present
/*
	ksads_8_29_t:   Fear of Social Situations    										- DSM-5 Criteria A (to assess evidence of Social Anxiety Disorder)
	ksads_8_30_t:	Duration (at least 6 months) 										- DSM-5 Criteria F
	ksads_8_301_t:	Social situations invariably provoke anxiety						- DSM-5 Criteria C
	ksads_8_303_t:	Social situations avoided or endured with distress					- DSM-5 Criteria D
	ksads_8_307_t:	Social fear is excessive given threat or sociocultural context		- DSM-5 Criteria E
	ksads_8_309_t: 	Impairment in functioning due to social anxiety						- DSM-5 Criteria G
	ksads_8_311_t:	Clinically significant distress due to Social Anxiety				- No DSM-5 criteria
	
	DSM-5 Criteria not in the database:
	- B: Fears he/she/they will show anxiety symptoms that will be negatively evaluated
	- H: Not attributable to the physiological effects of a substance or another medical condition
	- I: The fear and anxiety not better explained by the symptoms of another mental health disorder 
	- J: Not related to another medical condition (e.g. desfigurement from burns or injury)
*/

sum ksads_8_29_t ksads_8_30_t ksads_8_301_t ksads_8_303_t ksads_8_307_t ksads_8_309_t ksads_8_311_t

** Present

egen dsm5y_sanx_nm = rownonmiss(ksads_8_29_t ksads_8_30_t ksads_8_301_t ksads_8_303_t ksads_8_307_t ksads_8_309_t ksads_8_311_t)
egen dsm5y_sanx = rowtotal(ksads_8_29_t ksads_8_30_t ksads_8_301_t ksads_8_303_t ksads_8_307_t ksads_8_309_t ksads_8_311_t) if dsm5y_sanx_nm==7
egen Dsm5y_sanx = rowmean(ksads_8_29_t ksads_8_30_t ksads_8_301_t ksads_8_303_t ksads_8_307_t ksads_8_309_t ksads_8_311_t) if dsm5y_sanx_nm==7

/* Diagnosis: ksads_8_863_t */

** Past

egen dsm5y_sanxP_nm = rownonmiss(ksads_8_31_t ksads_8_313a_t ksads_8_302_t ksads_8_304_t ksads_8_308_t ksads_8_310_t ksads_8_312_t)
egen dsm5y_sanxP = rowtotal(ksads_8_31_t ksads_8_313a_t ksads_8_302_t ksads_8_304_t ksads_8_308_t ksads_8_310_t ksads_8_312_t) if dsm5y_sanxP_nm==7
egen Dsm5y_sanxP = rowmean(ksads_8_31_t ksads_8_313a_t ksads_8_302_t ksads_8_304_t ksads_8_308_t ksads_8_310_t ksads_8_312_t) if dsm5y_sanxP_nm==7

/* Diagnosis: ksads_8_864_t */

***************************************************************************************************************************************
*** Generalized Anxiety

describe $genanx_present
/*
	ksads_10_45_t: 	Excessive worries more days than not								- DSM-5 Criteria A (to assess evidence of Generalized Anxiety Disorder)
	ksads_10_47_t: 	Worrying has lasted at least 6 months								- DSM-5 Criteria C (at least some symptoms for the past six months)
	ksads_10_320_t: Excessive worries across breadth of domains							- DSM-5 Criteria C
	ksads_10_322_t:	Worry associated with defined symptom(s)							- DSM-5 Criteria C
	ksads_10_324_t: Difficulty controlling worries										- DSM-5 Criteria B
	ksads_10_326_t:	Impairment in functioning due to worries							- DSM-5 Criteria D
	ksads_10_328_t:	Clinically significant distress due to worries						- DSM-5 Criteria D
	
	DSM-5 Criteria not in the database:
	- E: Not attributable to the physiological effects of a substance or another medical condition
	- I: Not better accounted by the symptoms of another mental health disorder 
*/
	
sum ksads_10_45_t ksads_10_47_t ksads_10_320_t ksads_10_324_t ksads_10_328_t ksads_10_326_t ksads_10_322_t 

**********
** Present

egen dsm5y_ganx_nm = rownonmiss(ksads_10_45_t ksads_10_47_t ksads_10_320_t ksads_10_322_t ksads_10_324_t ksads_10_326_t ksads_10_328_t)
egen dsm5y_ganx = rowtotal(ksads_10_45_t ksads_10_47_t ksads_10_320_t ksads_10_322_t ksads_10_324_t ksads_10_326_t ksads_10_328_t) if dsm5y_ganx_nm==7
egen Dsm5y_ganx = rowmean(ksads_10_45_t ksads_10_47_t ksads_10_320_t ksads_10_322_t ksads_10_324_t ksads_10_326_t ksads_10_328_t) if dsm5y_ganx_nm==7

/* Diagnosed: ksads_10_869_t */

*******
** Past

egen dsm5y_ganxP_nm = rownonmiss(ksads_10_46_t ksads_10_330_t ksads_10_321_t ksads_10_323_t ksads_10_325_t ksads_10_327_t ksads_10_329_t)
egen dsm5y_ganxP = rowtotal(ksads_10_46_t ksads_10_330_t ksads_10_321_t ksads_10_323_t ksads_10_325_t ksads_10_327_t ksads_10_329_t) if dsm5y_ganxP_nm==7
egen Dsm5y_ganxP = rowmean(ksads_10_46_t ksads_10_330_t ksads_10_321_t ksads_10_323_t ksads_10_325_t ksads_10_327_t ksads_10_329_t) if dsm5y_ganxP_nm==7

/* Diagnosed: ksads_10_870_t */


***************************************************************************************************************************************
*** Any diagnosis

**********
** Present

sum ksads_1_840_t ksads_1_843_t ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_835_t ksads_2_836_t ksads_8_863_t ksads_10_869_t
gen any_idiagnosis = (ksads_1_840_t==1 | ksads_1_843_t==1 | ksads_8_863_t==1 | ksads_10_869_t==1) if ksads_1_840_t!=. & (eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1")
gen any_idiagnosis_b = (ksads_1_840_t==1 | ksads_1_843_t==1 | ksads_2_830_t==1 | ksads_2_831_t==1 | ksads_2_832_t==1 | ksads_2_835_t==1 | ksads_2_836_t==1 | ksads_8_863_t==1 | ksads_10_869_t==1) if ksads_1_840_t!=. & (eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1")

*******
** Past

sum ksads_1_842_t ksads_1_845_t ksads_2_833_t ksads_2_834_t ksads_2_837_t ksads_8_864_t ksads_10_870_t
gen any_idiagnosisP = (ksads_1_842_t==1 | ksads_1_845_t==1 | ksads_8_864_t==1 | ksads_10_870_t==1) if ksads_1_842_t!=. & (eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1")
gen any_idiagnosis_bP = (ksads_1_842_t==1 | ksads_1_845_t==1 | ksads_2_833_t==1 | ksads_2_834_t==1 | ksads_2_837_t==1 | ksads_8_864_t==1 | ksads_10_870_t==1) if ksads_1_842_t!=. & (eventname=="baseline_year_1_arm_1" | eventname=="2_year_follow_up_y_arm_1")


***************************************************************************************************************************************
*** Internalizing symptoms (Panic + agoraphobia + phobic disorders + separation anxiety + Social anxiety + GAD + OCD)
** Only social anxiety and GAD are available here in youth's reports

egen dsm5y_ints_nm = rownonmiss(ksads_8_309_t ksads_8_29_t ksads_8_30_t ksads_8_311_t ksads_8_307_t ksads_8_303_t ksads_8_301_t ksads_10_45_t ksads_10_320_t ksads_10_324_t ksads_10_328_t ksads_10_326_t ksads_10_47_t ksads_10_322_t)
egen dsm5y_ints = rowtotal(ksads_8_309_t ksads_8_29_t ksads_8_30_t ksads_8_311_t ksads_8_307_t ksads_8_303_t ksads_8_301_t ksads_10_45_t ksads_10_320_t ksads_10_324_t ksads_10_328_t ksads_10_326_t ksads_10_47_t ksads_10_322_t) if dsm5y_ints_nm==14
egen Dsm5y_ints = rowmean(ksads_8_309_t ksads_8_29_t ksads_8_30_t ksads_8_311_t ksads_8_307_t ksads_8_303_t ksads_8_301_t ksads_10_45_t ksads_10_320_t ksads_10_324_t ksads_10_328_t ksads_10_326_t ksads_10_47_t ksads_10_322_t) if dsm5y_ints_nm==14


***************************************************************************************************************************************
*** Suicide ideation/plan/attempt

describe $suic_present

/*
	ksads_23_143_t:	Self injurious behavior
	ksads_23_145_t:	Wishes/Better off dead
	ksads_23_147_t:	Suicidal Ideation
	ksads_23_149_t:	Suicidal Attempt
	
	ksads_23_807_t:	Self-injury, intent to die
	ksads_23_808_t:	Self-Injury, thought could die from behavior
	ksads_23_809_t:	Suicidal ideation thought of method
	ksads_23_810_t:	Suicidal ideation, intent to act
	ksads_23_811_t:	Suicidal ideation, specific plan
	ksads_23_812_t:	Suicidal behavior, made preparations
	ksads_23_813_t:	Aborted or interrupted suicide attempts
	ksads_23_814_t:	Method of actual suicide attempt
	ksads_23_815_t:	Suicide attempt, thought could die
	
*/

**********
** Present

* SIPA: Self-injury, and suicidal ideation, plans, and attempts
egen dsm5y_sipa_nm = rownonmiss(ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_23_807_t ksads_23_808_t ksads_23_809_t ksads_23_810_t ksads_23_811_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t)
egen dsm5y_sipa = rowtotal(ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_23_807_t ksads_23_808_t ksads_23_809_t ksads_23_810_t ksads_23_811_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t) if dsm5y_sipa_nm==13
egen Dsm5y_sipa = rowmean(ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_23_807_t ksads_23_808_t ksads_23_809_t ksads_23_810_t ksads_23_811_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t) if dsm5y_sipa_nm==13

* Self-injury
egen dsm5y_sinj_nm = rownonmiss(ksads_23_143_t ksads_23_807_t ksads_23_808_t)
egen dsm5y_sinj = rowtotal(ksads_23_143_t ksads_23_807_t ksads_23_808_t) if dsm5y_sinj_nm==3
egen Dsm5y_sinj = rowmean(ksads_23_143_t ksads_23_807_t ksads_23_808_t) if dsm5y_sinj_nm==3

* Suicidal ideation
egen dsm5y_side_nm = rownonmiss(ksads_23_145_t ksads_23_147_t ksads_23_809_t ksads_23_810_t ksads_23_811_t)
egen dsm5y_side = rowtotal(ksads_23_145_t ksads_23_147_t ksads_23_809_t ksads_23_810_t ksads_23_811_t) if dsm5y_side_nm==5
egen Dsm5y_side = rowmean(ksads_23_145_t ksads_23_147_t ksads_23_809_t ksads_23_810_t ksads_23_811_t) if dsm5y_side_nm==5

* Suicidal attempts
egen dsm5y_satt_nm = rownonmiss(ksads_23_149_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t)
egen dsm5y_satt = rowtotal(ksads_23_149_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t) if dsm5y_satt_nm==5
egen Dsm5y_satt = rowmean(ksads_23_149_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t) if dsm5y_satt_nm==5


*******
** Past

* SIPA: Self-injury, and suicidal ideation, plans, and attempts
egen dsm5y_sipaP_nm = rownonmiss(ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_23_816_t ksads_23_817_t ksads_23_818_t ksads_23_819_t ksads_23_820_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t)
egen dsm5y_sipaP = rowtotal(ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_23_816_t ksads_23_817_t ksads_23_818_t ksads_23_819_t ksads_23_820_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t) if dsm5y_sipaP_nm==13
egen Dsm5y_sipaP = rowmean(ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_23_816_t ksads_23_817_t ksads_23_818_t ksads_23_819_t ksads_23_820_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t) if dsm5y_sipaP_nm==13

* Self-injury
egen dsm5y_sinjP_nm = rownonmiss(ksads_23_144_t ksads_23_816_t ksads_23_817_t)
egen dsm5y_sinjP = rowtotal(ksads_23_144_t ksads_23_816_t ksads_23_817_t) if dsm5y_sinjP_nm==3
egen Dsm5y_sinjP = rowmean(ksads_23_144_t ksads_23_816_t ksads_23_817_t) if dsm5y_sinjP_nm==3

* Suicidal ideation
egen dsm5y_sideP_nm = rownonmiss(ksads_23_146_t ksads_23_148_t ksads_23_818_t ksads_23_819_t ksads_23_820_t)
egen dsm5y_sideP = rowtotal(ksads_23_146_t ksads_23_148_t ksads_23_818_t ksads_23_819_t ksads_23_820_t) if dsm5y_sideP_nm==5
egen Dsm5y_sideP = rowmean(ksads_23_146_t ksads_23_148_t ksads_23_818_t ksads_23_819_t ksads_23_820_t) if dsm5y_sideP_nm==5

* Suicidal attempts
egen dsm5y_sattP_nm = rownonmiss(ksads_23_150_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t)
egen dsm5y_sattP = rowtotal(ksads_23_150_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t) if dsm5y_sattP_nm==5
egen Dsm5y_sattP = rowmean(ksads_23_150_t ksads_23_821_t ksads_23_822_t ksads_23_823_t ksads_23_824_t) if dsm5y_sattP_nm==5


************************************************************************************************
************************************************************************************************

global dsm5y_mentalh "ksads_4_826_t ksads_4_828_t ksads_2_830_t ksads_2_832_t ksads_2_835_t ksads_2_836_t ksads_2_838_t ksads_1_840_t ksads_1_843_t ksads_1_846_t ksads_4_849_t ksads_4_851_t ksads_14_853_t ksads_5_857_t ksads_6_859_t ksads_7_861_t ksads_8_863_t ksads_25_865_t ksads_9_867_t ksads_10_869_t ksads_20_871_t ksads_20_872_t ksads_20_873_t ksads_20_874_t ksads_20_875_t ksads_20_876_t ksads_20_877_t ksads_20_878_t ksads_20_879_t ksads_20_889_t ksads_19_891_t ksads_20_893_t ksads_19_895_t ksads_16_897_t ksads_16_898_t ksads_15_901_t ksads_17_904_t ksads_5_906_t ksads_6_908_t ksads_7_909_t ksads_8_911_t ksads_10_913_t ksads_25_915_t ksads_11_917_t ksads_11_919_t ksads_21_921_t ksads_21_923_t ksads_12_925_t ksads_12_927_t ksads_13_929_t ksads_13_932_t ksads_13_935_t ksads_13_938_t ksads_13_941_t ksads_23_945_t ksads_23_946_t ksads_23_947_t ksads_23_948_t ksads_23_949_t ksads_23_950_t ksads_23_951_t ksads_23_952_t ksads_23_953_t ksads_23_954_t ksads_23_955_t ksads_24_967_t ksads_22_969_t"

foreach v of varlist $dsm5y_mentalh {
	replace `v'=. if `v'>1
}
   
global dsm5y_axdppy "ksads_1_840_t ksads_1_843_t ksads_1_846_t ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_835_t ksads_2_836_t ksads_2_838_t ksads_3_848_t ksads_4_826_t ksads_4_828_t ksads_4_849_t ksads_4_851_t ksads_5_857_t ksads_5_906_t ksads_6_859_t ksads_6_908_t ksads_7_861_t ksads_7_909_t ksads_8_863_t ksads_8_911_t ksads_13_942_t ksads_9_867_t ksads_10_869_t ksads_10_913_t ksads_11_917_t ksads_11_919_t ksads_12_925_t ksads_12_927_t ksads_13_929_t ksads_13_930_t ksads_13_932_t ksads_13_935_t ksads_13_938_t ksads_13_941_t ksads_13_942_t ksads_17_904_t ksads_21_921_t ksads_21_923_t ksads_25_865_t ksads_25_915_t"
global dsm5y_devdis "ksads_14_853_t ksads_18_903_t"
global dsm5y_condis "ksads_15_901_t ksads_16_897_t ksads_16_898_t "
global dsm5y_subdis "ksads_19_891_t ksads_19_895_t ksads_20_871_t ksads_20_872_t ksads_20_873_t ksads_20_874_t ksads_20_875_t ksads_20_876_t ksads_20_877_t ksads_20_878_t ksads_20_879_t ksads_20_889_t ksads_20_893_t"
global dsm5y_suibeh "ksads_23_945_t ksads_23_946_t ksads_23_947_t ksads_23_948_t ksads_23_949_t ksads_23_950_t ksads_23_951_t ksads_23_952_t ksads_23_953_t ksads_23_954_t ksads_23_955_t ksads_24_967_t"

*sum $dsm5y_mentalh if eventname=="baseline_year_1_arm_1"
*sum $dsm5y_mentalh if eventname=="1_year_follow_up_y_arm_1"
global dsm5y_MDD "ksads_1_1_t ksads_1_3_t ksads_1_5_t ksads_22_141_t ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_1_157_t ksads_1_159_t ksads_1_161_t ksads_1_163_t ksads_1_165_t ksads_1_167_t ksads_1_169_t ksads_1_171_t ksads_1_173_t ksads_1_175_t ksads_1_177_t ksads_1_179_t ksads_1_181_t ksads_1_183_t dpry_eating dpry_sleep dpry_motor dpry_energy drpy_worth drpy_decis drpy_suic ksads_1_2_t ksads_1_4_t ksads_1_6_t ksads_22_142_t ksads_23_144_t ksads_23_146_t ksads_23_148_t ksads_23_150_t ksads_1_158_t ksads_1_160_t ksads_1_162_t ksads_1_164_t ksads_1_166_t ksads_1_168_t ksads_1_170_t ksads_1_172_t ksads_1_174_t ksads_1_176_t ksads_1_178_t ksads_1_180_t ksads_1_182_t ksads_1_184_t dpry_eatingP dpry_sleepP dpry_motorP dpry_energyP drpy_worthP drpy_decisP drpy_suicP"

global dsm5y_INT "dsm5y_dys Dsm5y_dys ksads_1_843_t dpr_dprirr dpr_eat2 dsm5y_dysP Dsm5y_dysP ksads_1_845_t dpr_dprirrP dpr_eat2P dsm5y_mania Dsm5y_mania dsm5y_maniaP Dsm5y_maniaP ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_833_t ksads_2_835_t ksads_2_836_t dsm5y_moodd Dsm5y_moodd dsm5y_mooddP Dsm5y_mooddP dsm5y_sanx Dsm5y_sanx dsm5y_sanxP Dsm5y_sanxP ksads_8_863_t ksads_8_864_t dsm5y_ganx Dsm5y_ganx ksads_10_869_t dsm5y_ganxP Dsm5y_ganxP ksads_10_870_t dsm5y_sipa Dsm5y_sipa dsm5y_sinj Dsm5y_sinj dsm5y_side Dsm5y_side dsm5y_satt Dsm5y_satt dsm5y_sipaP Dsm5y_sipaP dsm5y_sinjP Dsm5y_sinjP dsm5y_sideP Dsm5y_sideP dsm5y_sattP Dsm5y_sattP"

rename dpr_eating dpry_eating 
rename dpr_sleep dpry_sleep 
rename dpr_motor dpry_motor 
rename dpr_energy dpry_energy 
rename drp_worth drpy_worth 
rename drp_decis drpy_decis 
rename drp_suic drpy_suic

rename dpr_eatingP dpry_eatingP
rename dpr_sleepP dpry_sleepP
rename dpr_motorP dpry_motorP
rename dpr_energyP dpry_energyP
rename drp_worthP drpy_worthP
rename drp_decisP drpy_decisP
rename drp_suicP drpy_suicP

label var ksads_1_840_t "Diagnosis of MDD, present"
label var ksads_1_843_t "Diagnosis of Dyshthymia, present"
label var ksads_2_830_t "Diagnosis of Bipolar I (m), present"
label var ksads_2_831_t "Diagnosis of Bipolar I (d), present"
label var ksads_2_832_t "Diagnosis of Bipolar I (hm), present"
label var ksads_2_835_t "Diagnosis of Bipolar II (hm), present"
label var ksads_2_836_t "Diagnosis of Bipolar II (d), present"
label var ksads_8_863_t "Diagnosis of Social Anxiety, present"
label var ksads_10_869_t "Diagnosis of Generalized Anxiety, present"

label var ksads_1_842_t "Diagnosis of MDD, past"
label var ksads_1_845_t "Diagnosis of Dyshthymia, past"
label var ksads_2_833_t "Diagnosis of Bipolar I (m), past"
label var ksads_2_834_t "Diagnosis of Bipolar I (d), past"
label var ksads_2_837_t "Diagnosis of Bipolar II (hm), past"
label var ksads_8_864_t "Diagnosis of Social Anxiety, past"
label var ksads_10_870_t "Diagnosis of Generalized Anxiety, past"
			
global dsm5y_DIAG "any_idiagnosis any_idiagnosis_b any_idiagnosisP any_idiagnosis_bP ksads_2_836_t ksads_1_840_t ksads_1_843_t ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_835_t ksads_8_863_t ksads_10_869_t ksads_1_842_t ksads_1_845_t ksads_2_833_t ksads_2_834_t ksads_2_837_t ksads_8_864_t ksads_10_870_t"

global dsm5y_sipa "ksads_23_807_t ksads_23_808_t ksads_23_809_t ksads_23_810_t ksads_23_811_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t"

keep 	src_subject_id eventname												///
		$dsm5y_axdppy $dsm5y_devdis $dsm5y_condis $dsm5y_subdis $dsm5y_suibeh	///
		ksads_22_969_t Dsm5y_mdd Dsm5y_dys dsm5y_mdd							///
		dpry_eating dpry_sleep dpry_motor dpry_energy drpy_worth drpy_decis 	///
		drpy_suic mddy_first mddy_second mddy_diagn dsm5y_mddvm $dsm5y_MDD		///
		$dsm5y_DIAG $dsm5y_INT dsm5y_mddvmP Dsm5y_mddP $dsm5y_sipa				///
		dsm5y_ints Dsm5y_ints

tempfile dsm5dgnsy
save `dsm5dgnsy'


**************************************************************************************
*************************************** 31 *******************************************
**************************************************************************************
**** Loading data on Prodromal Psychosis Scale
/*clear
import delimited "$data\pps01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1


** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\mental-health\mh_y_pps.csv"
		
tempfile prodromalp
save `prodromalp'


**************************************************************************************
*************************************** 32 *******************************************
**************************************************************************************
**** Loading data on Brief Problem Monitor, teacher

clear
import delimited "$data5p1\mental-health\mh_t_bpm.csv"
		
tempfile bpm_teacher
save `bpm_teacher'


**************************************************************************************
*************************************** 33 *******************************************
**************************************************************************************
**** Loading data on Brief Problem Monitor, youth

clear
import delimited "$data5p1\mental-health\mh_y_bpm.csv"
		
tempfile bpm_youth
save `bpm_youth'


**************************************************************************************
*************************************** 34 *******************************************
**************************************************************************************
**** Loading data on Youth NIH TB Summary Scores (Cognitive abilities)
/*clear
import delimited "$data\abcd_tbss01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1


** Interview Date
gen interview_day = substr(interview_date, 4,2)
gen interview_month = substr(interview_date, 1,2)
gen interview_year = substr(interview_date, 7,4)

destring interview_day interview_month interview_year, replace 

** Identifying periods on the Panel data
sort subjectkey interview_year interview_month interview_day
*/

clear
import delimited "$data5p1\neurocognition\nc_y_nihtb.csv"

* NIH Toolbox Picture Sequence Memory Test (Episodic Memory)
	sum nihtbx_picture_uncorrected if eventname=="baseline_year_1_arm_1"
	gen seq_memory 	= (nihtbx_picture_uncorrected - r(mean))/r(sd)

* NIH Toolbox Flanker Inhibitory Control and Attention Test (Arrows or fishes pointing left or right)
	sum nihtbx_flanker_uncorrected if eventname=="baseline_year_1_arm_1"
	gen flanker 	= (nihtbx_flanker_uncorrected - r(mean))/r(sd)

* NIH Toolbox Pattern Comparison Processing Speed Test (Do the pictures look the same?)	
	sum nihtbx_pattern_uncorrected if eventname=="baseline_year_1_arm_1"
	gen procspeed 	= (nihtbx_pattern_uncorrected - r(mean))/r(sd)
	
* NIH Toolbox List
	sum nihtbx_list_uncorrected if eventname=="baseline_year_1_arm_1"
	gen list_memo 	= (nihtbx_list_uncorrected - r(mean))/r(sd)

* NIH Toolbox Cardsort
	sum nihtbx_cardsort_uncorrected if eventname=="baseline_year_1_arm_1"
	gen cardsort 	= (nihtbx_cardsort_uncorrected - r(mean))/r(sd)
	
* NIH Toolbox Picture Vocabulary Test (Matching the picture with the word)
	sum nihtbx_picvocab_uncorrected if eventname=="baseline_year_1_arm_1"
	gen vocabulary 	= (nihtbx_picvocab_uncorrected - r(mean))/r(sd)	

* NIH Toolbox Oral Reading Recognition Test (Reading the words on the screen)
destring nihtbx_reading_fc, replace force	
	sum nihtbx_reading_uncorrected if eventname=="baseline_year_1_arm_1"
	gen reading 	= (nihtbx_reading_uncorrected - r(mean))/r(sd)	

* NIH Toolbox Fluid Composite Fully-Corrected T-score	
	sum nihtbx_fluidcomp_fc if eventname=="baseline_year_1_arm_1"
	gen fluid_i 	= (nihtbx_reading_uncorrected - r(mean))/r(sd)	
	
* NIH Toolbox Crystallized Composite Fully-Corrected T-score
destring nihtbx_cryst_fc, replace force
	sum nihtbx_cryst_fc if eventname=="baseline_year_1_arm_1"
	gen crystal_i 	= (nihtbx_cryst_fc - r(mean))/r(sd)		
	
keep 	src_subject_id eventname												///
		seq_memory flanker procspeed vocabulary reading	fluid_i crystal_i		///
		nihtbx_cryst_fc nihtbx_fluidcomp_fc nihtbx_totalcomp_fc					///
		nihtbx_reading_fc list_memo cardsort nihtbx_fluidcomp_uncorrected 		///
		nihtbx_cryst_uncorrected nihtbx_totalcomp_uncorrected					///
		nihtbx_totalcomp_agecorrected nihtbx_cryst_agecorrected 				///
		nihtbx_fluidcomp_agecorrected	
		
tempfile cogn_ab
save `cogn_ab'


**************************************************************************************
*************************************** 35 *******************************************
**************************************************************************************
**** Loading data on Latent variables 
clear
import delimited "$data5p1\culture-environment\ce_p_nsc.csv"
		
tempfile ncrimep
save `ncrimep'


**************************************************************************************
*************************************** 36 *******************************************
**************************************************************************************
**** Loading data on neighborhood safety (Youth)
clear
import delimited "$data5p1\culture-environment\ce_y_nsc.csv"
		
tempfile ncrimey
save `ncrimey'


**************************************************************************************
*************************************** 37 *******************************************
**************************************************************************************
**** Loading data on Medical History Questionnaire (MHX)

clear
import delimited "$data5p1\physical-health\ph_p_mhx.csv"

keep 	src_subject_id eventname													///
		medhx_2a medhx_2b medhx_2c medhx_2d medhx_2e medhx_2f medhx_2g medhx_2h 	///
		medhx_2i medhx_2j medhx_2k medhx_2l medhx_2m medhx_2n medhx_2o medhx_2p 	///
		medhx_2q medhx_2r medhx_2s													///
		medhx_2a_l medhx_2b_l medhx_2c_l medhx_2d_l medhx_2e_l medhx_2f_l 			///
		medhx_2g_l medhx_2h_l medhx_2i_l medhx_2j_l medhx_2k_l medhx_2l_l 			///
		medhx_2m_l medhx_2n_l medhx_2o_l medhx_2p_l medhx_2q_l medhx_2r_l 			///
		medhx_2_notes2_l
		
tempfile phillbl
save `phillbl'


**************************************************************************************
*************************************** 38 *******************************************
**************************************************************************************
**** Loading data on Adult Behavior Checklist
clear
import delimited "$data5p1\mental-health\mh_p_abcl.csv"

*Adaptive functioning scales - 		Friends, personal strengths
*Syndrome scales - 					Anxious/Depressed, Withdrawn, Somatic Complaints -> Internalizing Behavior
*+									Thought Problems, Attention Problems
*+									Aggressive Behavior, Rule-Breaking Behavior, Intrusive -> Externalizing Behavior
*DSM oriented scales - 				Depressive problems, anxiety problems, somatic problems
*+									Avoidant personality ADHD, antisocial personality
*2014 scales - 						Sluggish Cognitive Tempo, Obsessive-Compulsive

global friend_ab 		"abcl_num_friends_p abcl_contact_p abcl_get_along_p abcl_visited_p"
global pstren_ab 		"abcl_q02_p abcl_q04_p abcl_q15_p abcl_q49_p abcl_q73_p abcl_q88_p abcl_q98_p abcl_q106_p abcl_q109_p abcl_q110_p abcl_q123_p"
global anxdep_ab 		"abcl_q12_p abcl_q14_p abcl_q31_p abcl_q33_p abcl_q34_p abcl_q35_p abcl_q45_p abcl_q47_p abcl_q50_p abcl_q52_p abcl_q71_p abcl_q103_p abcl_q107_p abcl_q112_p"
global withdr_ab 		"abcl_q25_p abcl_q30_p abcl_q42_p abcl_q48_p abcl_q60_p abcl_q65_p abcl_q67_p abcl_q69_p abcl_q111_p"
global somati_ab 		"abcl_q51_p abcl_q54_p abcl_q56a_p abcl_q56b_p abcl_q56c_p abcl_q56d_p abcl_q56e_p abcl_q56f_p abcl_q56g_p"
global intern_ab 		$anxdep_ab $withdr_ab $somati_ab
global though_ab		"abcl_q09_p abcl_q18_p abcl_q40_p abcl_q66_p abcl_q70_p abcl_q80_p abcl_q84_p abcl_q85_p abcl_q91_p"
global attent_ab		"abcl_q01_p abcl_q08_p abcl_q11_p abcl_q13_p abcl_q17_p abcl_q53_p abcl_q59_p abcl_q61_p abcl_q64_p abcl_q78_p abcl_q96_p abcl_q101_p abcl_q102_p abcl_q105_p abcl_q108_p abcl_q119_p abcl_q121_p"
global toprob_ab 		$though_ab $attent_ab
global aggres_ab		"abcl_q03_p abcl_q05_p abcl_q16_p abcl_q28_p abcl_q37_p abcl_q55_p abcl_q57_p abcl_q68_p abcl_q81_p abcl_q86_p abcl_q87_p abcl_q95_p abcl_q97_p abcl_q113_p abcl_q116_p abcl_q118_p"
global rulebr_ab		"abcl_q06_p abcl_q23_p abcl_q26_p abcl_q39_p abcl_q41_p abcl_q43_p abcl_q76_p abcl_q82_p abcl_q90_p abcl_q92_p abcl_q114_p abcl_q117_p abcl_q122_p"
global intrus_ab		"abcl_q07_p abcl_q19_p abcl_q74_p abcl_q93_p abcl_q94_p abcl_q104_p"
global extern_ab		$aggres_ab $rulebr_ab $intrus_ab
global dsmdep_ab 		"abcl_q14_p abcl_q18_p abcl_q24_p abcl_q35_p abcl_q52_p abcl_q54_p abcl_q60_p abcl_q77_p abcl_q78_p abcl_q91_p abcl_q96_p abcl_q100_p abcl_q102_p abcl_q103_p abcl_q107_p"
global dsmanx_ab 		"abcl_q22_p abcl_q29_p abcl_q45_p abcl_q50_p abcl_q72_p abcl_q112_p"
global dsmsom_ab 		"abcl_q56a_p abcl_q56b_p abcl_q56c_p abcl_q56d_p abcl_q56e_p abcl_q56f_p abcl_q56g_p"
global dsmavo_ab 		"abcl_q25_p abcl_q42_p abcl_q47_p abcl_q67_p abcl_q71_p abcl_q75_p abcl_q111_p"
global dsmadh_ab 		"abcl_q01_p abcl_q08_p abcl_q10_p abcl_q36_p abcl_q41_p abcl_q59_p abcl_q61_p abcl_q89_p abcl_q105_p abcl_q108_p abcl_q115_p abcl_q118_p abcl_q119_p"
global dsmant_ab 		"abcl_q03_p abcl_q05_p abcl_q16_p abcl_q21_p abcl_q23_p abcl_q26_p abcl_q28_p abcl_q37_p abcl_q39_p abcl_q43_p abcl_q57_p abcl_q76_p abcl_q82_p abcl_q92_p abcl_q95_p abcl_q97_p abcl_q101_p abcl_q114_p abcl_q120_p abcl_q122_p"
global sluggi_ab 		"abcl_q13_p abcl_q17_p abcl_q54_p abcl_q83_p abcl_q102_p"
global obscom_ab 		"abcl_q09_p abcl_q31_p abcl_q32_p abcl_q52_p abcl_q66_p abcl_q84_p abcl_q85_p abcl_q112_p"

****************
** ABCL Raw Score: Summing across all responses
egen 	abcl_friend_n = rownonmiss($friend_ab)
egen 	abcl_friend = rowtotal($friend_ab), miss
replace abcl_friend = . if abcl_friend_n!=4

egen 	abcl_pstren_n = rownonmiss($pstren_ab)
egen 	abcl_pstren = rowtotal($pstren_ab), miss
replace abcl_pstren = . if abcl_pstren_n!=11

egen 	abcl_anxdep_n = rownonmiss($anxdep_ab)
egen 	abcl_anxdep = rowtotal($anxdep_ab), miss
replace abcl_anxdep = . if abcl_anxdep_n!=14

egen 	abcl_withdr_n = rownonmiss($withdr_ab)
egen 	abcl_withdr = rowtotal($withdr_ab), miss
replace abcl_withdr =. if abcl_withdr_n!=9

egen 	abcl_somati_n = rownonmiss($somati_ab)
egen 	abcl_somati = rowtotal($somati_ab), miss
replace abcl_somati = . if abcl_somati_n!=9

egen 	abcl_intern_n = rownonmiss($intern_ab)
egen 	abcl_intern = rowtotal($intern_ab), miss
replace abcl_intern = . if abcl_intern_n!=32

egen 	abcl_though_n = rownonmiss($though_ab)
egen 	abcl_though = rowtotal($though_ab), miss
replace abcl_though = . if abcl_though_n!=9

egen 	abcl_attent_n = rownonmiss($attent_ab)
egen 	abcl_attent = rowtotal($attent_ab), miss
replace abcl_attent = . if abcl_attent_n!=17

egen 	abcl_toprob_n = rownonmiss($toprob_ab)
egen 	abcl_toprob = rowtotal($toprob_ab), miss
replace abcl_toprob = . if abcl_toprob_n!=26

egen 	abcl_aggres_n = rownonmiss($aggres_ab)
egen 	abcl_aggres = rowtotal($aggres_ab), miss
replace abcl_aggres = . if abcl_aggres_n!=16

egen 	abcl_rulebr_n = rownonmiss($rulebr_ab)
egen 	abcl_rulebr = rowtotal($rulebr_ab), miss
replace abcl_rulebr = . if abcl_rulebr_n!=13

egen 	abcl_intrus_n = rownonmiss($intrus_ab)
egen 	abcl_intrus = rowtotal($intrus_ab), miss
replace abcl_intrus = . if abcl_intrus_n!=6

egen 	abcl_extern_n = rownonmiss($extern_ab)
egen 	abcl_extern = rowtotal($extern_ab), miss
replace abcl_extern = . if abcl_extern_n!=35

egen 	abcl_dsmdep_n = rownonmiss($dsmdep_ab)
egen 	abcl_dsmdep = rowtotal($dsmdep_ab), miss
replace abcl_dsmdep = . if abcl_dsmdep_n!=15

egen 	abcl_dsmanx_n = rownonmiss($dsmanx_ab)
egen 	abcl_dsmanx = rowtotal($dsmanx_ab), miss
replace abcl_dsmanx = . if abcl_dsmanx_n!=6

egen 	abcl_dsmsom_n = rownonmiss($dsmsom_ab)
egen 	abcl_dsmsom = rowtotal($dsmsom_ab), miss
replace abcl_dsmsom = . if abcl_dsmsom_n!=7

egen 	abcl_dsmavo_n = rownonmiss($dsmavo_ab)
egen 	abcl_dsmavo = rowtotal($dsmavo_ab), miss
replace abcl_dsmavo = . if abcl_dsmavo_n!=7

egen 	abcl_dsmadh_n = rownonmiss($dsmadh_ab)
egen 	abcl_dsmadh = rowtotal($dsmadh_ab), miss
replace abcl_dsmadh = . if abcl_dsmadh_n!=13

egen 	abcl_dsmant_n = rownonmiss($dsmant_ab)
egen 	abcl_dsmant = rowtotal($dsmant_ab), miss
replace abcl_dsmant = . if abcl_dsmant_n!=20

egen 	abcl_sluggi_n = rownonmiss($sluggi_ab)
egen 	abcl_sluggi = rowtotal($sluggi_ab), miss
replace abcl_sluggi = . if abcl_sluggi_n!=5

egen 	abcl_obscom_n = rownonmiss($obscom_ab)
egen 	abcl_obscom = rowtotal($obscom_ab), miss
replace abcl_obscom = . if abcl_obscom_n!=8

keep 	src_subject_id eventname													///
		abcl_friend abcl_pstren abcl_anxdep abcl_withdr abcl_somati abcl_intern 	///
		abcl_though abcl_attent abcl_toprob abcl_aggres abcl_rulebr abcl_intrus 	///
		abcl_extern abcl_dsmdep abcl_dsmanx abcl_dsmsom abcl_dsmavo abcl_dsmadh 	///
		abcl_dsmant abcl_sluggi abcl_obscom 										///
		abcl_lived_with_p-abcl_scr_prob_critical_nt
		
tempfile adcl
save `adcl'


**************************************************************************************
*************************************** 39 *******************************************
**************************************************************************************
**** Loading data on School Risk and Protective Factors Survey
clear
import delimited "$data5p1\culture-environment\ce_y_srpf.csv"

egen school_positive = rowmean(school_2_y school_3_y school_4_y school_5_y school_6_y school_7_y)
egen st_engaged = rowmean(school_12_y school_15_y school_17_y)

label var school_positive 	"school_2_y to school_7_y"
label var st_engaged 		"school_12_y to school_17_y"

		
tempfile school_rk
save `school_rk'


**************************************************************************************
*************************************** 40 *******************************************
**************************************************************************************
**** Loading data on Parent Diagnostic Interview for DSM-5 (KSADS) Traumatic Events
clear
import delimited "$data5p1\mental-health\mh_p_ksads_ptsd.csv"
	
tempfile ptsd_x
save `ptsd_x'


**************************************************************************************
*************************************** 41 *******************************************
**************************************************************************************
**** Loading data on Parent Family History Summary Scores
clear
import delimited "$data5p1\mental-health\mh_p_fhx.csv"

keep 	src_subject_id eventname													///
		famhx_ss_fath_prob_alc_p famhx_ss_moth_prob_alc_p famhx_ss_parent_alc_p 	///
		famhx_ss_fath_prob_dg_p famhx_ss_moth_prob_dg_p famhx_ss_parent_dg_p 		///
		famhx_ss_fath_prob_dprs_p famhx_ss_patgf_prob_dprs_p 						///
		famhx_ss_patgm_prob_dprs_p famhx_ss_moth_prob_dprs_p 						///
		famhx_ss_matgf_prob_dprs_p famhx_ss_matgm_prob_dprs_p 						///
		famhx_ss_momdad_dprs_p famhx_ss_parent_dprs_p famhx_ss_fath_prob_ma_p 		///
		famhx_ss_moth_prob_ma_p famhx_ss_parent_ma_p 								///
		famhx_ss_fath_prob_vs_p famhx_ss_moth_prob_vs_p famhx_ss_parent_vs_p 		///
		famhx_ss_fath_prob_trb_p famhx_ss_patgf_prob_trb_p 							///
		famhx_ss_patgm_prob_trb_p famhx_ss_moth_prob_trb_p 							///
		famhx_ss_matgf_prob_trb_p famhx_ss_matgm_prob_trb_p 						///
		famhx_ss_momdad_trb_p famhx_ss_parent_trb_p									///
		famhx_ss_fath_prob_nrv_p famhx_ss_moth_prob_nrv_p famhx_ss_parent_nrv_p 	///
		famhx_ss_fath_prob_prf_p famhx_ss_moth_prob_prf_p famhx_ss_parent_prf_p 	///
		famhx_ss_fath_prob_hspd_p famhx_ss_moth_prob_hspd_p famhx_ss_parent_hspd_p 	///
		famhx_ss_fath_prob_scd_p famhx_ss_moth_prob_scd_p famhx_ss_parent_scd_p		///
		famhx_ss_patgf_prob_ma_p famhx_ss_patgm_prob_ma_p famhx_ss_matgf_prob_ma_p 	///
		famhx_ss_matgm_prob_ma_p famhx_ss_patgf_prob_vs_p famhx_ss_patgm_prob_vs_p 	///
		famhx_ss_matgf_prob_vs_p famhx_ss_matgm_prob_vs_p 						 	///
		famhx_ss_momdad_ma_p famhx_ss_momdad_vs_p  									///
		famhx_ss_momdad_nrv_p famhx_ss_patgf_prob_nrv_p famhx_ss_patgm_prob_nrv_p 	///
		famhx_ss_matgf_prob_nrv_p famhx_ss_matgm_prob_nrv_p

tempfile family_hist
save `family_hist'


**************************************************************************************
*************************************** 42 *******************************************
**************************************************************************************
**** Loading data on Medications Survey Inventory Modified from PhenX (PMP)
clear
import delimited "$data5p1\physical-health\ph_p_meds.csv"

* Loxapine: 	210242|1182290|943952|1170082   141868|219367|213110|6475 5049937	1367515 5049939	1367517 1506684	329943
* Olanzapine: 	6358152|6376625|6345626|6378526|6369646	754503
* Quetiapine: 	4257181|4258845|6363252|2404286|3740427	616483
* Asenapine: 	6837385 | 1606336
* Clotiapine: 	1225361|9270511|78931|1156514|10326746|1225362|4258941|1911459	207586|2058200|2620|207585|238031|1250452
* Clozapine: 	10324714|79097|3723592|1176921	1723989|216418|2626

* Prozac: 1182485 | 1182486 | 1182487 | 58827
* Ritalin: 1185343 | 1185344
* Sertraline: 328670 | 328671
* Fluoxetine: 647554 | 647555

* Medication treating ADHD: Methylphenidate; Concerta; Vyvanse; dexmethylphenidate; lisdexamfetamine; Ritalin; atomoxetine; Strattera; Quillivant; QuilliChew; Metadate; Methamphetamine; Lisdexamfetamine; Intuniv; Focalin; Evekeo; Dyanavel; Dextroamphetamine; Daytrana; Cotempla; guafacine; atomoxetine; Dexmethylphenidate; Adderall; Amphetamine

* Medication treating depression: Zoloft; Trazodone; Sertraline, Prozac, Mirtazapine, Lexapro, Imipramine; Fluvoxamine, Fluoxetine, Escitalopram, Desvenlafaxine, Citalopram, Quetiapine, Celexa, Bupropion, Amitriptyline, Abilify, Amoxapine

* Schizophrenia, bipolar, irritability: Risperidone, Risperdal, Quetiapine, Lithium, Clozapine, Clotiapine, Asenapine, Olanzapine, Loxapine, Abilify, quetiapine, aripiprazole

* Anxiety: Hydroxyzine, Diazepam, Clonazepam, Buspar, Buspirone

forvalues n=1/15{
	split med`n'_rxnorm_p, gen(med`n'_rxnorm_padd)
	rename med`n'_rxnorm_padd2 medication`n'
	forvalues i = 0/9  {
		replace medication`n' = subinstr(medication`n', "`i'", "",.)
	}
		replace medication`n' = subinstr(medication`n', ".", "",.)
		replace medication`n' = subinstr(medication`n', "/", "",.)
		replace medication`n' = med`n'_rxnorm_padd4 if medication`n'=="" & med`n'_rxnorm_padd4!=""
		
		replace medication`n' = proper(medication`n')
		drop med`n'_rxnorm_padd*
}

** Medication for depression (18 drugs identified)
gen depr_med = 0
foreach M in Zoloft Trazodone Sertraline Prozac Mirtazapine Lexapro Imipramine Fluvoxamine Fluoxetine Escitalopram Desvenlafaxine Citalopram Quetiapine Celexa Bupropion Amitriptyline Abilify Amoxapine{
	forvalues N=1/15{
		replace depr_med = 1 if medication`N'=="`M'"
	}
}

** Medication for ADHD (24 drugs identified)
gen adhd_med = 0
foreach M in Methylphenidate Concerta Vyvanse Dexmethylphenidate Lisdexamfetamine Ritalin Atomoxetine Strattera Quillivant QuilliChew Metadate Methamphetamine Intuniv Focalin Evekeo Dyanavel Dextroamphetamine Daytrana Cotempla Guafacine Atomoxetine Dexmethylphenidate Adderall Amphetamine{
	forvalues N=1/15{
		replace adhd_med = 1 if medication`N'=="`M'"
	}
}

** Medication for Schizophrenia, bipolar, irritability (12 drugs identified)
gen thog_med = 0
foreach M in Risperidone Risperdal Quetiapine Lithium Clozapine Clotiapine Asenapine Olanzapine Loxapine Abilify Quetiapine Aripiprazole{
	forvalues N=1/15{
		replace thog_med = 1 if medication`N'=="`M'"
	}
}
	
keep 	src_subject_id  eventname brought_medications								///
		med1_rxnorm_p med2_rxnorm_p med3_rxnorm_p med4_rxnorm_p med5_rxnorm_p 		///
		med6_rxnorm_p med7_rxnorm_p med8_rxnorm_p med9_rxnorm_p med10_rxnorm_p 		///
		med11_rxnorm_p med12_rxnorm_p med13_rxnorm_p med14_rxnorm_p med15_rxnorm_p  ///
		med_otc_1_rxnorm_p med_otc_2_rxnorm_p med_otc_3_rxnorm_p 					///
		med_otc_4_rxnorm_p med_otc_5_rxnorm_p med_otc_6_rxnorm_p 					///
		med_otc_7_rxnorm_p med_otc_8_rxnorm_p med_otc_9_rxnorm_p 					///
		med_otc_10_rxnorm_p med_otc_11_rxnorm_p med_otc_12_rxnorm_p 				///
		med_otc_13_rxnorm_p med_otc_14_rxnorm_p med_otc_15_rxnorm_p 				///		
		caff_24 caff_ago depr_med adhd_med thog_med
	
tempfile medication
save `medication'


**************************************************************************************
*************************************** 43 *******************************************
**************************************************************************************
**** Loading data on Parent Sleep Disturbance Scale for Children (SDS)
clear
import delimited "$data5p1\physical-health\ph_p_sds.csv"

egen sleepdisturb = rowmean(sleepdisturb1_p-sleepdisturb26_p)

keep 	src_subject_id eventname 									///
		sleepdisturb sds_p_ss_dims-sds_p_ss_total_nt
	
tempfile sleepdis
save `sleepdis'


**************************************************************************************
*************************************** 44 *******************************************
**************************************************************************************
**** Loading data on Early Adolescent Temperament Questionnaire
clear
import delimited "$data5p1\mental-health\mh_p_eatq.csv"

tempfile temperam
save `temperam'


**************************************************************************************
*************************************** 45 *******************************************
**************************************************************************************
**** Loading data on Youth Substance Use Interview, Baseline
clear
import delimited "$data5p1\substance-use\su_y_sui.csv"

keep 	src_subject_id eventname 													///
		tlfb_alc tlfb_tob tlfb_mj tlfb_mj_synth tlfb_bitta tlfb_caff 				///
		tlfb_inhalant tlfb_rx_misuse tlfb_list_yes_no tlfb_alc_sip 					///
		tlfb_alc_use tlfb_tob_puff tlfb_cig_use 									///
		su_caff_ss_sum su_caff_ss_sum_l su_caff_ss_sum_nt su_caff_ss_sum_l_nt 		///
		tlfb_alc_l tlfb_tob_l tlfb_mj_l tlfb_mj_synth_l tlfb_bitta_l tlfb_caff_l 	///
		tlfb_inhalant_l tlfb_rx_misuse_l tlfb_list_yes_no_l tlfb_alc_sip_l 			///
		tlfb_alc_use_l tlfb_tob_puff_l tlfb_cig_use_l 								///
		su_first_nicotine_1b_calc tlfb_tob_puff_l 
	
tempfile subsuse
save `subsuse'


**************************************************************************************
*************************************** 46 *******************************************
**************************************************************************************
**** Loading data on Pearson scores - Rey Delayed Recall Test
clear
import delimited "$data5p1\neurocognition\nc_y_ravlt.csv"

* Eighth trial after second set of words and 30-minute pause
	gen ravlt_raw = pea_ravlt_ld_trial_vii_tc - pea_ravlt_ld_trial_vii_tr - pea_ravlt_ld_trial_vii_ti
	sum ravlt_raw if eventname=="baseline_year_1_arm_1"
	gen ravlt_sd = (ravlt_raw - r(mean))/r(sd)


keep 	src_subject_id eventname ravlt_raw ravlt_sd
	
tempfile pearson_t
save `pearson_t'


**************************************************************************************
*************************************** 47 *******************************************
**************************************************************************************
**** Loading data on Behavioral performance measures for nBack task fMRI (working memory)
clear
import delimited "$data5p1\imaging\mri_y_tfmr_nback_beh.csv"
*import delimited "$data5p1\imaging\mri_y_tfmr_nback_rec_beh.csv"

sum tfmri_nb_all_beh_ctotal_rate if eventname=="baseline_year_1_arm_1"
	gen nback_rate = (tfmri_nb_all_beh_ctotal_rate - r(mean))/r(sd)
	
sum tfmri_nb_all_beh_ctotal_mrt if eventname=="baseline_year_1_arm_1"
	gen nback_speed = (tfmri_nb_all_beh_ctotal_mrt - r(mean))/r(sd)
	
sum tfmri_nb_all_beh_ctotal_stdrt if eventname=="baseline_year_1_arm_1"
	gen nback_acc = -1*((tfmri_nb_all_beh_ctotal_stdrt - r(mean))/r(sd))


keep 	src_subject_id eventname 												///
		tfmri_nb_all_beh_ctotal_rate tfmri_nb_all_beh_ctotal_mrt 				///
		tfmri_nb_all_beh_ctotal_stdrt nback_rate nback_speed nback_acc			///
		tfmri_nb_all_beh_ctotal_nt
		
tempfile nback_perf
save `nback_perf'


** Negative
clear
import delimited "$data5p1\imaging\mri_y_tfmr_nback_ngfvntf_aseg.csv"
*import delimited "$data5p1\imaging\mri_y_tfmr_nback_rec_beh.csv"

keep 	src_subject_id eventname 												///
		tfmri_nback_all_224 tfmri_nback_all_238
		
tempfile nback_perfNeg
save `nback_perfNeg'

** Positive
clear
import delimited "$data5p1\imaging\mri_y_tfmr_nback_psfvntf_aseg.csv"
*import delimited "$data5p1\imaging\mri_y_tfmr_nback_rec_beh.csv"

keep 	src_subject_id eventname 												///
		tfmri_nback_all_254 tfmri_nback_all_268
		
tempfile nback_perfPos
save `nback_perfPos'

**************************************************************************************
*************************************** 48 *******************************************
**************************************************************************************
**** Loading data on Life Events, caregiver's report
clear
import delimited "$data5p1\mental-health\mh_p_le.csv"
		
tempfile p_levents
save `p_levents'


**************************************************************************************
*************************************** 49 *******************************************
**************************************************************************************
**** Loading data on Life Events, youth's report
clear
import delimited "$data5p1\mental-health\mh_y_le.csv"
		
tempfile y_levents
save `y_levents'


**************************************************************************************
*************************************** 50 *******************************************
**************************************************************************************
**** Loading data on Youth Peer Behavior Profile
clear
import delimited "$data5p1\culture-environment\ce_y_pbp.csv"
		
tempfile peer_influence
save `peer_influence'


**************************************************************************************
*************************************** 51 *******************************************
**************************************************************************************
**** Loading data on Youth Peer Network Health Protective Scaler
clear
import delimited "$data5p1\culture-environment\ce_y_pnh.csv"
		
tempfile peerh_influence
save `peerh_influence'

	
**************************************************************************************
*************************************** 52 *******************************************
**************************************************************************************
**** Loading data on Youth Resistance to Peer Influence
clear
import delimited "$data5p1\culture-environment\ce_y_rpi.csv"
		
tempfile peer_resistance
save `peer_resistance'

	
**************************************************************************************
*************************************** 53 *******************************************
**************************************************************************************
**** Loading data on Youth Resistance to Peer Influence
clear
import delimited "$data5p1\mental-health\mh_y_peq.csv"
		
tempfile peer_experience
save `peer_experience'


**************************************************************************************
*************************************** 54 *******************************************
**************************************************************************************
**** Loading data on Cyberbullying
clear
import delimited "$data5p1\mental-health\mh_y_cbb.csv"
		
tempfile peer_cyberbullying
save `peer_cyberbullying'


**************************************************************************************
*************************************** 55 *******************************************
**************************************************************************************
**** Loading data on Social Development Difficulties in Emotion Regulation, Caregiver
clear
import delimited "$data5p1\mental-health\mh_p_ders.csv"
		
tempfile emotion_regp
save `emotion_regp'


**************************************************************************************
*************************************** 56 *******************************************
**************************************************************************************
**** Loading data on Social Development Difficulties in Emotion Regulation, Youth
clear
import delimited "$data5p1\mental-health\mh_y_erq.csv"
		
tempfile emotion_regy
save `emotion_regy'	


**************************************************************************************
*************************************** 57 *******************************************
**************************************************************************************
**** Loading data on Youth Emotional Stroop Task
clear
import delimited "$data5p1\neurocognition\nc_y_est.csv"
		
tempfile emotion_stroop
save `emotion_stroop'	

/*54. abcd_yest01:			Youth Emotional Stroop Task
Emotional Stroop Task - The emotional Stroop task (Stroop, 1935) measures cognitive control 
under conditions of emotional salience (see Başgöze et al., 2015; Banich et al., 2019). 
The task-relevant dimension is an emotional word, which participants categorize as either a 
"good" feeling (happy, joyful) or a "bad" feeling (angry, upset). 
The task-irrelevant dimension is an image, which is of a teenager’s face with either a happy or 
an angry facial expression.  Trials are of two types. On congruent trials, the word and facial 
emotion are of the same valence (e.g. a happy face paired with word "joyful"). 
The location of the word varies from trial-to-trial, presented either on the top of the image 
or at the bottom. On incongruent trials, the word and facial expression are of different 
valence (e.g., a happy face paired with word "angry").  Participants work through 2 test blocks: 
one block consists of 50% congruent and 50% incongruent trials; the other consists of 25% incongruent 
trials and 75% congruent trials. The composition of the former type of block helps individuals 
keep the task set in mind more so than the latter (Kane & Engle, 2003). The 25% incongruent/75% 
congruent block is always administered first, followed by the 50% incongruent/50% congruent block. 
Accuracy and response times for congruent versus incongruent trials for the total task and 
within each emotion subtype (happy/joyful; angry/upset) are calculated. Relative difficulties 
with cognitive control are indexed by lower accuracy rates and longer reaction times for 
incongruent relative to congruent trials. */


**************************************************************************************
*************************************** 58 *******************************************
**************************************************************************************
**** Loading data on Social Influence Task
clear
import delimited "$data5p1\neurocognition\nc_y_sit.csv"

tempfile socialinfluence_task
save `socialinfluence_task'


**************************************************************************************
*************************************** 59 *******************************************
**************************************************************************************
**** Loading data on Parent PhenX Community Cohesion
clear
import delimited "$data5p1\culture-environment\ce_p_comc.csv"
		
tempfile neigh_cohesion
save `neigh_cohesion'


**************************************************************************************
*************************************** 60 *******************************************
**************************************************************************************
**** Loading data on Other Resilience (Close friends)
clear
import delimited "$data5p1\mental-health\mh_y_or.csv"
		
tempfile close_friendsr
save `close_friendsr'				

**************************************************************************************
*************************************** 61 *******************************************
**************************************************************************************
**** Loading data on Residential History Derived Scores
clear
import delimited "$data5p1\linked-external-data\led_l_adi.csv"
		
tempfile residential_h
save `residential_h'		
	

**************************************************************************************
*************************************** 62 *******************************************
**************************************************************************************
**** Loading data on Social Opportunity
clear
import delimited "$data5p1\linked-external-data\led_l_socmob.csv"
		
tempfile social_mobility
save `social_mobility'
	
	
**************************************************************************************
*************************************** 63 *******************************************
**************************************************************************************
**** Loading data on Child Opportunity Index
clear
import delimited "$data5p1\linked-external-data\led_l_coi.csv"

tempfile child_opportunity
save `child_opportunity'	


**************************************************************************************
*************************************** 64 *******************************************
**************************************************************************************
**** Loading data on Census Return (Social Capital)
clear
import delimited "$data5p1\linked-external-data\led_l_censusret.csv"
		
tempfile social_capital
save `social_capital'	


**************************************************************************************
*************************************** 65 *******************************************
**************************************************************************************
**** Loading data on Rent and Mortgage
clear
import delimited "$data5p1\linked-external-data\led_l_rentmort.csv"
		
tempfile rent_mortgage
save `rent_mortgage'		


**************************************************************************************
*************************************** 66 *******************************************
**************************************************************************************
**** Loading data on Youth Pubertal Development Scale and Menstrual Cycle Survey History (PDMS)
clear
import delimited "$data5p1\physical-health\ph_y_pds.csv"

foreach v of varlist pds_ht2_y pds_skin2_y pds_bdyhair_y pds_f4_2_y pds_f5_y pds_ht2_y pds_skin2_y pds_bdyhair_y pds_m4_y pds_m5_y{
    replace `v'=. if `v'==777 | `v'==999 
}

egen pubertyf_v = rowmean(pds_ht2_y pds_skin2_y pds_bdyhair_y pds_f4_2_y pds_f5_y) if pds_sex_y==2
egen pubertym_v = rowmean(pds_ht2_y pds_skin2_y pds_bdyhair_y pds_m4_y pds_m5_y) if pds_sex_y==1

gen 	puberty_v = pubertyf_v if pds_sex_y==2
replace puberty_v = pubertym_v if pds_sex_y==1

keep 	src_subject_id eventname 											///
		pds_sex_y-menstrualcycle6_y pubertyf_v pubertym_v puberty_v			///
		pds_y_ss_male_category pds_y_ss_female_category
		
tempfile puberty
save `puberty'	


**************************************************************************************
*************************************** 67 *******************************************
**************************************************************************************
**** Loading data on Youth Symptoms of Mania
clear
import delimited "$data5p1\mental-health\mh_y_7up.csv"

tempfile mania7up
save `mania7up'	


**************************************************************************************
*************************************** 68 *******************************************
**************************************************************************************
**** Loading data on suicidality 
clear
import delimited "$data5p1\mental-health\mh_y_ksads_si.csv"

tempfile suicidaly_y
save `suicidaly_y'	


**************************************************************************************
*************************************** 69 *******************************************
**************************************************************************************
**** Loading data on suicidality, caregiver report
clear
import delimited "$data5p1\mental-health\mh_p_ksads_si.csv"

tempfile suicidaly_p
save `suicidaly_p'	


**************************************************************************************
*************************************** 70 *******************************************
**************************************************************************************
**** Loading data on homocidality
clear
import delimited "$data5p1\mental-health\mh_p_ksads_hi.csv"

tempfile homocidality
save `homocidality'	

**************************************************************************************
*************************************** 71 *******************************************
**************************************************************************************
**** Loading data on resiliency
clear
import delimited "$data5p1\neurocognition\nc_y_bird.csv"

tempfile bi_resiliency
save `bi_resiliency'	


**************************************************************************************
*************************************** 72 *******************************************
**************************************************************************************
**** Loading data on weschsler intelligence
clear
import delimited "$data5p1\neurocognition\nc_y_wisc.csv"

tempfile weschsleri
save `weschsleri'


**************************************************************************************
*************************************** 73 *******************************************
**************************************************************************************
**** Loading data on weschsler intelligence
clear
import delimited "$data5p1\culture-environment\ce_p_fes.csv"

tempfile family_confp
save `family_confp'


**************************************************************************************
*************************************** X *******************************************
**************************************************************************************
**** Loading data on EARS, post assessment - caregiver
clear
import delimited "$data5p1\novel-technologies\nt_p_ears_qtn.csv"

tempfile ears_p_pa
save `ears_p_pa'


**************************************************************************************
*************************************** 74 *******************************************
**************************************************************************************
**** Loading data on screen time questionnaire, caregiver
clear
import delimited "$data5p1\novel-technologies\nt_p_psq.csv"

tempfile screent_q_pp
save `screent_q_pp'


**************************************************************************************
*************************************** 75 *******************************************
**************************************************************************************
**** Loading data on screen time questionnaire, caregiver on youth
clear
import delimited "$data5p1\novel-technologies\nt_p_stq.csv"

tempfile screent_q_py
save `screent_q_py'


**************************************************************************************
*************************************** X *******************************************
**************************************************************************************
**** Loading data on device usage statistics
clear
import delimited "$data5p1\novel-technologies\nt_y_ears.csv"

tempfile ears_y_use
save `ears_y_use'


**************************************************************************************
*************************************** X *******************************************
**************************************************************************************
**** Loading data on device usage statistics
clear
import delimited "$data5p1\novel-technologies\nt_y_ears_qtn.csv"

tempfile ears_y_pa
save `ears_y_pa'


**************************************************************************************
*************************************** 76 *******************************************
**************************************************************************************
**** Loading data on screen time questionnaire, youth reports and data
clear
import delimited "$data5p1\novel-technologies\nt_y_st.csv"

tempfile screent_q_y
save `screent_q_y'


**************************************************************************************
*************************************** 77 *******************************************
**************************************************************************************
**** Loading data on school demographics
clear
import delimited "$data5p1\linked-external-data\led_l_seda_demo_s.csv"

tempfile school_demo
save `school_demo'


**************************************************************************************
*************************************** 78 *******************************************
**************************************************************************************
**** Loading data on school achievement (math+reading), school level
clear
import delimited "$data5p1\linked-external-data\led_l_seda_pool_s.csv"

tempfile school_acad
save `school_acad'

**************************************************************************************
*************************************** 78 *******************************************
**************************************************************************************
**** Loading data on school achievement (math+reading), districte level
clear
import delimited "$data5p1\linked-external-data\led_l_seda_pool_d.csv"

tempfile district_acad
save `district_acad'

**************************************************************************************
*************************************** 80 *******************************************
**************************************************************************************
**** Loading data on school achievement (math), districte level
clear
import delimited "$data5p1\linked-external-data\led_l_seda_math_d.csv"

tempfile district_acad_a
save `district_acad_a'

**************************************************************************************
*************************************** 81 *******************************************
**************************************************************************************
**** Loading data on school achievement (reading), districte level
clear
import delimited "$data5p1\linked-external-data\led_l_seda_read_d.csv"

tempfile district_acad_b
save `district_acad_b'

**************************************************************************************
*************************************** 82 *******************************************
**************************************************************************************
**** Loading data on grades and attendance, caregiver report
clear
import delimited "$data5p1\culture-environment\ce_p_sag.csv"

tempfile grades_attendance_p
save `grades_attendance_p'


**************************************************************************************
*************************************** 83 *******************************************
**************************************************************************************
**** Loading data on grades and attendance, youth report
clear
import delimited "$data5p1\culture-environment\ce_y_sag.csv"

tempfile grades_attendance_y
save `grades_attendance_y'


**************************************************************************************
*************************************** 84 *******************************************
**************************************************************************************
**** Loading data on Covid19 RRR - Youths
clear
import delimited "C:\Users\mpmar\Desktop\PhD\Projects\ABCD Study\2 Data\ABCDData5_1\covid RR\yabcdcovid19questionnaire01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

keep 	subjectkey src_subject_id eventname interview_date					///
		afraid_against_mask_2_cv-worry_y_cv
		
tempfile covid19y
save `covid19y'

**************************************************************************************
*************************************** 85 *******************************************
**************************************************************************************
**** Loading data on Covid19 RRR - Parents
clear
import delimited "C:\Users\mpmar\Desktop\PhD\Projects\ABCD Study\2 Data\ABCDData5_1\covid RR\pabcdcovid19questionnaire01.txt", varnames(1) encoding(Big5) 

** Labeling variables
foreach var of varlist * {
  label variable `var' "`=`var'[1]'"
  replace `var'="" if _n==1
  destring `var', replace
}

drop in 1

keep 	subjectkey src_subject_id eventname interview_date				///
		cv_p_select_language___1-su_p_cig_loc_cv___888
		
tempfile covid19p
save `covid19p'


merge 1:1 subjectkey src_subject_id eventname using `covid19y'
drop _merge


tempfile covid19db
save `covid19db'

**************************************************************************************
*************************************** 86 *******************************************
**************************************************************************************
**** Loading data on Research Sites
clear
import delimited "$data5p1\ABCD_SiteStates.csv", varnames(1)

tempfile sites_IDs
save `sites_IDs'

clear
import delimited "$data5p1\ABCD_ResearchSitesLocation.csv", varnames(1)

rename ïsite_id_l site_id_l

tempfile sites_charac
save `sites_charac'

clear
import delimited "$data5p1\ABCD_StatesDataW23.csv", varnames(1)

tempfile states_charac
save `states_charac'


use `sites_IDs', clear
merge 1:1 site_id_l using `sites_charac'
drop _merge
merge n:1 state using `states_charac'
drop _merge

tempfile SITES
save `SITES'


**************************************************************************************
*************************************** 87 *******************************************
**************************************************************************************
**** Loading data on brain data inclusion
clear
import delimited "$data5p1\imaging\mri_y_qc_incl.csv"

tempfile mri_inclusion
save `mri_inclusion'


**************************************************************************************
**************************************************************************************
**************************************************************************************
**** Merging the databases

use `cbclsc', clear
merge 1:1 src_subject_id eventname using `cbclsum'
drop _merge
merge 1:1 src_subject_id eventname using `bullyy'
drop _merge
merge 1:1 src_subject_id eventname using `bullyp'
drop _merge
merge 1:1 src_subject_id eventname using `genderID'
drop _merge
merge 1:1 src_subject_id eventname using `anthro'
drop _merge
merge 1:1 src_subject_id eventname using `weight'
drop _merge
merge 1:1 src_subject_id eventname using `zigosity'
drop _merge
merge 1:1 src_subject_id eventname using `genetics'
drop _merge
merge 1:1 src_subject_id eventname using `demographics'
drop _merge
merge 1:1 src_subject_id eventname using `latent'
drop _merge
merge 1:1 src_subject_id eventname using `discrimin'
drop _merge
merge 1:1 src_subject_id eventname using `smri1'
drop _merge
merge 1:1 src_subject_id eventname using `smri2'
drop _merge
merge 1:1 src_subject_id eventname using `smri_rinc'
drop _merge
merge 1:1 src_subject_id eventname using `paccept'
drop _merge
merge 1:1 src_subject_id eventname using `fesy'
drop _merge
merge 1:1 src_subject_id eventname using `parent_mon'
drop _merge
merge 1:1 src_subject_id eventname using `parent_neglect'
drop _merge
merge 1:1 src_subject_id eventname using `asrs'
drop _merge
*merge 1:1 src_subject_id eventname using `pasri'
*drop _merge
merge 1:1 src_subject_id eventname using `posemo'
drop _merge
merge 1:1 src_subject_id eventname using `prosocp'
drop _merge
merge 1:1 src_subject_id eventname using `prosocy'
drop _merge
merge 1:1 src_subject_id eventname using `ywill'
drop _merge
merge 1:1 src_subject_id eventname using `phyactiv'
drop _merge
merge 1:1 src_subject_id eventname using `sportact'
drop _merge
merge 1:1 src_subject_id eventname using `autis'
drop _merge
merge 1:1 src_subject_id eventname using `conddp'
drop _merge
merge 1:1 src_subject_id eventname using `conddt'
drop _merge
merge 1:1 src_subject_id eventname using `dsm5dgnsp'
drop _merge
merge 1:1 src_subject_id eventname using `dsm5dgnsy'
drop _merge
merge 1:1 src_subject_id eventname using `prodromalp'
drop _merge
merge 1:1 src_subject_id eventname using `bpm_teacher'
drop _merge
merge 1:1 src_subject_id eventname using `bpm_youth'
drop _merge
merge 1:1 src_subject_id eventname using `cogn_ab'
drop _merge
merge 1:1 src_subject_id eventname using `ncrimep'
drop _merge
merge 1:1 src_subject_id eventname using `ncrimey'
drop _merge
merge 1:1 src_subject_id eventname using `phillbl'
drop _merge
merge 1:1 src_subject_id eventname using `adcl'
drop _merge
merge 1:1 src_subject_id eventname using `school_rk'
drop _merge
merge 1:1 src_subject_id eventname using `ptsd_x'
drop _merge
merge 1:1 src_subject_id eventname using `family_hist'
drop _merge
merge 1:1 src_subject_id eventname using `medication'
drop _merge
merge 1:1 src_subject_id eventname using `sleepdis'
drop _merge
merge 1:1 src_subject_id eventname using `temperam'
drop _merge
merge 1:1 src_subject_id eventname using `subsuse'
drop _merge
merge 1:1 src_subject_id eventname using `pearson_t'
drop _merge
merge 1:1 src_subject_id eventname using `nback_perf'
drop _merge
merge 1:1 src_subject_id eventname using `nback_perfNeg'
drop _merge
merge 1:1 src_subject_id eventname using `nback_perfPos'
drop _merge
merge 1:1 src_subject_id eventname using `p_levents'
drop _merge
merge 1:1 src_subject_id eventname using `y_levents'
drop _merge
merge 1:1 src_subject_id eventname using `peer_influence'
drop _merge
merge 1:1 src_subject_id eventname using `peerh_influence'
drop _merge
merge 1:1 src_subject_id eventname using `peer_resistance' 
drop _merge
merge 1:1 src_subject_id eventname using `peer_experience'
drop _merge
merge 1:1 src_subject_id eventname using `peer_cyberbullying'
drop _merge
merge 1:1 src_subject_id eventname using `emotion_regp'
drop _merge
merge 1:1 src_subject_id eventname using `emotion_regy'
drop _merge
merge 1:1 src_subject_id eventname using `emotion_stroop'
drop _merge
merge 1:1 src_subject_id eventname using `socialinfluence_task'
drop _merge
merge 1:1 src_subject_id eventname using `neigh_cohesion'
drop _merge
merge 1:1 src_subject_id eventname using `close_friendsr'
drop _merge
merge 1:1 src_subject_id eventname using `residential_h'
drop _merge
merge 1:1 src_subject_id eventname using `social_mobility'
drop _merge
merge 1:1 src_subject_id eventname using `child_opportunity'
drop _merge
merge 1:1 src_subject_id eventname using `social_capital'
drop _merge
merge 1:1 src_subject_id eventname using `rent_mortgage'
drop _merge
merge 1:1 src_subject_id eventname using `puberty'
drop _merge
merge 1:1 src_subject_id eventname using `mania7up'
drop _merge
merge 1:1 src_subject_id eventname using `suicidaly_y'
drop _merge
merge 1:1 src_subject_id eventname using `suicidaly_p' 
drop _merge
merge 1:1 src_subject_id eventname using `homocidality'
drop _merge	
merge 1:1 src_subject_id eventname using `bi_resiliency'
drop _merge	
merge 1:1 src_subject_id eventname using `weschsleri'
drop _merge	
merge 1:1 src_subject_id eventname using `family_confp'
drop _merge	
merge 1:1 src_subject_id eventname using `screent_q_pp'
drop _merge	 
merge 1:1 src_subject_id eventname using `screent_q_py'
drop _merge	
merge 1:1 src_subject_id eventname using `screent_q_y'
drop _merge	
merge 1:1 src_subject_id eventname using `school_demo'
drop _merge	
merge 1:1 src_subject_id eventname using `school_acad' 
drop _merge	
merge 1:1 src_subject_id eventname using `district_acad' 
drop _merge	
merge 1:1 src_subject_id eventname using `district_acad_a' 
drop _merge	
merge 1:1 src_subject_id eventname using `district_acad_b'
drop _merge	
merge 1:1 src_subject_id eventname using `grades_attendance_p'
drop _merge	
merge 1:1 src_subject_id eventname using `grades_attendance_y'
drop _merge	
merge 1:1 src_subject_id eventname using `mri_inclusion'
drop _merge	
merge n:1 site_id_l using `SITES'
drop _merge	

** Appending Covid19 data
append using `covid19db'

** Formatting database
gen 	wave1 = 1 if eventname=="baseline_year_1_arm_1"
replace wave1 = 2 if eventname=="1_year_follow_up_y_arm_1"
replace wave1 = 3 if eventname=="2_year_follow_up_y_arm_1"
replace wave1 = 4 if eventname=="3_year_follow_up_y_arm_1"
replace wave1 = 5 if eventname=="4_year_follow_up_y_arm_1"

gen 	wave2 = 1 if eventname=="baseline_year_1_arm_1"
replace wave2 = 2 if eventname=="6_month_follow_up_arm_1"
replace wave2 = 3 if eventname=="1_year_follow_up_y_arm_1"
replace wave2 = 4 if eventname=="18_month_follow_up_arm_1"
replace wave2 = 5 if eventname=="2_year_follow_up_y_arm_1"
replace wave2 = 6 if eventname=="30_month_follow_up_arm_1"
replace wave2 = 7 if eventname=="3_year_follow_up_y_arm_1"
replace wave2 = 8 if eventname=="42_month_follow_up_arm_1"
replace wave2 = 9 if eventname=="4_year_follow_up_y_arm_1"

gen 	wave3 = 1 if eventname=="baseline_year_1_arm_1"
replace wave3 = 2 if eventname=="1_year_follow_up_y_arm_1"
replace wave3 = 3 if eventname=="18_month_follow_up_arm_1"
replace wave3 = 4 if eventname=="2_year_follow_up_y_arm_1"
replace wave3 = 5 if eventname=="30_month_follow_up_arm_1"
replace wave3 = 6 if eventname=="covid19_cv1_arm_2"
replace wave3 = 7 if eventname=="covid19_cv2_arm_2"
replace wave3 = 8 if eventname=="covid19_cv3_arm_2"
replace wave3 = 9 if eventname=="3_year_follow_up_y_arm_1"
replace wave3 = 10 if eventname=="42_month_follow_up_arm_1"
replace wave3 = 11 if eventname=="4_year_follow_up_y_arm_1"


********************************************************************************
** Grade configuration (info at baseline: Min 3 and Max 8) 

gen 	gc_bl = 1 if led_sch_seda_s_maxgrd==3
replace gc_bl = 2 if led_sch_seda_s_maxgrd==4
replace gc_bl = 3 if led_sch_seda_s_maxgrd==5
replace gc_bl = 4 if led_sch_seda_s_maxgrd==6
replace gc_bl = 5 if led_sch_seda_s_maxgrd==7
replace gc_bl = 6 if led_sch_seda_s_maxgrd==8

egen grade_conf = mean(gc_bl), by(src_subject_id)

label define grade_conf 1 "K3" 2 "K4" 3 "K5" 4 "K6" 5 "K7" 6 "K8"
label values grade_conf grade_conf

gen 	grade_c = 0 if grade_conf==6
replace grade_c = 1 if grade_conf==3 | grade_conf==4

label var grade_c "K5 or K6 (vs. K8)"


gen 	gradec_k5 = 0 if grade_conf==6
replace gradec_k5 = 1 if grade_conf==3

gen 	gradec_k6 = 0 if grade_conf==6
replace gradec_k6 = 1 if grade_conf==4

gen 	gradec_k5k6 = 0 if grade_conf==6
replace gradec_k5k6 = 1 if grade_conf==3 | grade_conf==4

label var gradec_k5 "K5 (vs. K8)"
label var gradec_k6 "K6 (vs. K8)"
label var gradec_k5k6 "K5 or K6 (vs. K8)"


********************************************************************************
** Public schools

sum led_sch_seda_s_charter led_sch_seda_s_urbanicity led_sch_seda_s_magnet

label define led_sch_seda_s_type 1 "Regular schools" 2 "Special education" 3 "Vocational" 4 "Other alternatives"
label values led_sch_seda_s_type led_sch_seda_s_type 

label define kbi_p_c_school_setting 1 "Not in school" 2 "Public school" 3 "Private school" 4 "Vocational" 5 "Cyber school" 6 "Home school" 7 "For students with emotional problems" 8 "Other" 9 "Charter school"
label values kbi_p_c_school_setting kbi_p_c_school_setting 

gen pub_schx = (kbi_p_c_school_setting==2) if wave1==1
egen public_school = mean(pub_schx), by(src_subject_id)


********************************************************************************
** Date

gen interv_date = date(interview_date, "MDY")
format interv_date  %dM_d,_CY


********************************************************************************
** Saving the database

save "Path to where data will be stored\NAME_DATABASE.dta", replace
