
***** Defining the path to databases

global data 		"PATH TO THE ABCD DATA RELEASE 5.1 - ALREADY PROCESSED USING DO-FILE: 'Procesisng ABCD data 5_1 release'"
global results 		"PATH TO FOLDER WHERE RESULTS WILL BE STORED"

/* To run this code, please make sure that within the folder in the path '$results' above, there are three other folders:
	- Longitudinal associations
	- Moderation analysis
	- ThreeWayInteraction
 */

**************************************************************************************
******************************* 1. Loading the data **********************************
**************************************************************************************
** Loading the data

use "$data\NAME_DATABASE.dta", clear 			

**** Exclusion restrictions for structural imaging data

* Structural MRI
foreach v of varlist smri_vol_cdk_cdacatelh-smri_vol_scs_subcorticalgv {
    replace `v' = . if (imgincl_t1w_include == 0) | (imgincl_t2w_include == 0)
}

* fMRI nBack task
foreach v of varlist tfmri_nb_all_beh_ctotal_mrt tfmri_nb_all_beh_ctotal_nt tfmri_nb_all_beh_ctotal_rate tfmri_nb_all_beh_ctotal_stdrt tfmri_nback_all_224 tfmri_nback_all_238 tfmri_nback_all_254 tfmri_nback_all_268{
	replace `v' = . if imgincl_nback_include == 0 
}

egen family_rel = max(rel_relationship), by(src_subject_id)
*keep if family_rel==0
gen 	sibling = 0 if family_rel==0
replace sibling = 1 if family_rel>0


**************************************************************************************
******************************** 2. Main variables ***********************************
**************************************************************************************
***** Bullying definitions

/*
1. Poor peer relations index 1: Poor peer relations reported in Zucker (1997); Devries (2015); Levitan (2019)

2. Peer rejection index 2: van Gent (2011)
3. Teased a lot (Parents): cbcl_q38_p
4. Bullying index (Parents): b_invbul_p2 
	* cbcl_q16_p: Cruelty, bullying, or meanness to others
	* cbcl_q37_p: Gets in many fights
	* cbcl_q57_p: Physically attacks people
	* cbcl_q94_p: Teases a lot
5. Victimization index (Parents): b_invvic_p2
	* cbcl_q25_p: Doesn't get along with other kids
	* cbcl_q34_p: Feels others are out to get him/her
	* cbcl_q38_p: Gets teased a lot
	* cbcl_q48_p: Not liked by other kids
6. Discriminated against (Youth): discr_ag
	* dim_yesno_q1: In the past 12 months, have you felt discriminated against: because of your race
	* dim_yesno_q2: In the past 12 months, have you felt discriminated against: because you are (or where you are from)
	* dim_yesno_q3: In the past 12 months, have you felt discriminated against: because someone thought you were gay
	* dim_yesno_q4: In the past 12 months, have you felt discriminated against: because of your weight
*/
**** 1. Bullying involvement
* kbi_p_c_bully: 	Does your child have any problems with bullying at school or in your neighborhood?
* ksads_bully_~26: 	Do you have any problems with bullying at school or in your neighborhood?

*************************
** 1. Poor peer relations

egen peer1 	= rowmean(cbcl_q25_p cbcl_q38_p cbcl_q48_p)
egen peer1b = rowtotal(cbcl_q25_p cbcl_q38_p cbcl_q48_p), miss

alpha cbcl_q25_p cbcl_q38_p cbcl_q48_p if eventname=="baseline_year_1_arm_1"

***************************************************************************************************************************** 
*** 2. Peer rejection

egen peer2 	= rowmean(cbcl_q25_p cbcl_q34_p cbcl_q38_p cbcl_q48_p)
egen peer2b = rowtotal(cbcl_q25_p cbcl_q34_p cbcl_q38_p cbcl_q48_p), miss

alpha cbcl_q25_p cbcl_q34_p cbcl_q38_p cbcl_q48_p if eventname=="baseline_year_1_arm_1"

***************************************************************************************************************************** 
*** 3. Bully others 1

gen peer3 	= ksads_cdr_478_p

***************************************************************************************************************************** 
*** 4. Bully others 2

** Aggressiveness
* cbcl_q16_p: Cruelty, bullying, or meanness to others
* cbcl_q57_p: Physically attacks people
* cbcl_q94_p: Teases a lot
 
egen peer4 	= rowmean(cbcl_q16_p cbcl_q57_p cbcl_q94_p cbcl_q97_p)
egen peer4b = rowtotal(cbcl_q16_p cbcl_q57_p cbcl_q94_p cbcl_q97_p), miss

*alpha cbcl_q16_p cbcl_q57_p cbcl_q94_p cbcl_q97_p if eventname=="baseline_year_1_arm_1", item
alpha cbcl_q16_p cbcl_q57_p cbcl_q94_p cbcl_q97_p if eventname=="baseline_year_1_arm_1"

gen 	feels_lone = 1 if cbcl_q12_p==1 | cbcl_q12_p==2
replace feels_lone = 0 if cbcl_q12_p==0

global bullying_vars 	"peer1 peer4 feels_lone cbcl_q25_p cbcl_q38_p cbcl_q48_p"


******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Parental acceptance definitions (and family conflict)

*** 1. Family: p_accept12 (Maximum of total scores between primary and secondary caregiver)
	* crpbi_parent1_y (Youth): Makes me feel better after talking over my worries with him/her
	* crpbi_parent2_y (Youth): Smiles at me very often
	* crpbi_parent3_y (Youth): Is able to make me feel better when I am upset
	* crpbi_parent4_y (Youth): Believes in showing his/her love for me
	* crpbi_parent5_y (Youth): Is easy to talk tot_siblings

egen non_missf = rownonmiss(crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y) if  eventname=="baseline_year_1_arm_1"
egen non_misss = rownonmiss(crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y crpbi_caregiver16_y) if  eventname=="baseline_year_1_arm_1"
	
egen p_acceptance = rowmean(crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y crpbi_caregiver16_y) if (non_missf==4 | non_missf==5) & (non_misss==4 | non_misss==5)
egen first_acc = rowmean(crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y) if (non_missf==4 | non_missf==5)
egen secon_acc = rowmean(crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y crpbi_caregiver16_y) if (non_misss==4 | non_misss==5)
gen p_accept12 = (first_acc+secon_acc)/2

alpha 	crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y 	///
		crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y 	///
		crpbi_caregiver16_y if eventname=="baseline_year_1_arm_1"
		
alpha 	crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y 	///
		if eventname=="baseline_year_1_arm_1"		

egen both_accept = rowmin(first_acc secon_acc)
egen any_accept = rowmax(first_acc secon_acc)
egen aver_accept = rowmean(first_acc secon_acc)
egen sum_accept = rowtotal(first_acc secon_acc)

*Who is the second caregiver?
label define crpbi_caregiver2_y 1 "mother" 2 "father" 3 "grandmother" 4 "grandfather" 5 "aunt" 6 "uncle" 7 "other"
label values crpbi_caregiver2_y crpbi_caregiver2_y 

egen mom_accept_aux1 = rowmean(crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y) if crpbi_caregiver2_y>=2 & crpbi_caregiver2_y<.
egen mom_accept_aux2 = rowmean(crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y crpbi_caregiver16_y) if crpbi_caregiver2_y==1
gen 	mom_accept = mom_accept_aux1 if crpbi_caregiver2_y>=2 & crpbi_caregiver2_y<.
replace mom_accept = mom_accept_aux2 if crpbi_caregiver2_y==1

egen dad_accept_aux1 = rowmean(crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y) if crpbi_caregiver2_y==1 | (crpbi_caregiver2_y>=3 & crpbi_caregiver2_y<.)
egen dad_accept_aux2 = rowmean(crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y crpbi_caregiver16_y) if crpbi_caregiver2_y==2
gen 	dad_accept = dad_accept_aux1 if crpbi_caregiver2_y==1 | (crpbi_caregiver2_y>=3 & crpbi_caregiver2_y<.)
replace dad_accept = dad_accept_aux2 if crpbi_caregiver2_y==2

gen 	getal_careg = 1 if kbi_p_conflict==1
replace getal_careg = 0 if kbi_p_conflict==2 | kbi_p_conflict==3

rename crpbi_y_ss_parent pcwarmth /* turn on if using ABCD 5.1 */

** Family conflict

alpha 	fes_youth_q1 fes_youth_q2 fes_youth_q3 fes_youth_q4 fes_youth_q5 fes_youth_q6 		///
		fes_youth_q7 fes_youth_q8 fes_youth_q9 if eventname=="baseline_year_1_arm_1"
		

global family_vars 	"p_acceptance first_acc secon_acc aver_accept dad_accept mom_accept fam_confav getal_careg pcwarmth crpbi_parent1_y crpbi_parent2_y crpbi_parent3_y crpbi_parent4_y crpbi_parent5_y crpbi_caregiver12_y crpbi_caregiver13_y crpbi_caregiver14_y crpbi_caregiver15_y crpbi_caregiver16_y fes_youth_q1 fes_youth_q2 fes_youth_q3 fes_youth_q4 fes_youth_q5 fes_youth_q6 fes_youth_q7 fes_youth_q8 fes_youth_q9"

******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Close friends

egen n_friends = rowtotal(resiliency5a_y resiliency6a_y), miss
egen n_cfriends = rowtotal(resiliency5b_y resiliency6b_y), miss

global friend_vars 	"n_friends n_cfriends"

******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** School environment

gen 	getal_teacher = 1 if school_3_y==3 | school_3_y==4
replace getal_teacher = 0 if school_3_y==1 | school_3_y==2

rename school_positive sch_pos
egen school_positive = rowmean(school_2_y school_3_y school_4_y school_5_y school_7_y school_10_y)

alpha 	school_2_y school_3_y school_4_y school_5_y school_7_y school_10_y		///
		if eventname=="baseline_year_1_arm_1"

alpha 	school_2_y school_3_y school_4_y school_5_y school_7_y school_10_y		///
		if eventname=="1_year_follow_up_y_arm_1"

** Opportunities for prosocial behavior
*school_2_y: In my school, students have lots of chances to help decide things like class activities
*school_5_y: There are lots of chances for students in my school to get involved in sports (and others)
*school_10_y: There are lots of chances to be part of class discussions or activities

** Rewards for prosocial behavior
*school_3_y: I get along with my teachers
*school_4_y: My teacher(s) notices when I am doing a good job and lets me know about it
*school_7_y: The school lets my parents know when I have done something well

global school_vars 	"getal_teacher school_positive st_engaged school_2_y school_3_y school_4_y school_5_y school_7_y school_10_y"


******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Neighboorhood environment

foreach v of varlist comc_phenx_close_knit_p-comc_phenx_budget_p{
	replace `v' = . if `v'>5
}
egen nhood_cohes = rowmean(comc_phenx_close_knit_p-comc_phenx_budget_p)

global nhood_vars 	"neighborhood_crime_y nhood_cohes reshist_addr1_adi_wsum reshist_addr1_adi_perc"
*reshist_addr1_adi_in_dis reshist_addr1_adi_pov reshist_addr1_adi_b138


******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Emotion regulation

global emoreg_vars 	"poa_y_ss_sum tfmri_nb_all_beh_ctotal_nt"


******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Traumatic events  (History of trauma exposure)

** Categories (Thompson et al. 2022)
* 1. Accidents requiring medical attention
egen Tr_accidents = rowmax(ksads_ptsd_raw_754_p ksads_ptsd_raw_755_p)

* 2. Natural Disasters 
egen Tr_ndisasters = rowmax(ksads_ptsd_raw_756_p ksads_ptsd_raw_757_p)

* 3. Witnessing community violence
egen Tr_communityv = rowmax(ksads_ptsd_raw_758_p ksads_ptsd_raw_759_p ksads_ptsd_raw_760_p)

* 4. Witnessing domestic violence
egen Tr_domesticv = rowmax(ksads_ptsd_raw_764_p ksads_ptsd_raw_765_p ksads_ptsd_raw_766_p)

* 5. Physical victimization
egen Tr_physicalv = rowmax(ksads_ptsd_raw_761_p ksads_ptsd_raw_762_p ksads_ptsd_raw_763_p)

* 6. Sexual trauma
egen Tr_sexual = rowmax(ksads_ptsd_raw_767_p ksads_ptsd_raw_768_p ksads_ptsd_raw_769_p)

* 7. Sudden death of a loved one
egen Tr_sdeathlo = rowmax(ksads_ptsd_raw_770_p)

* Number of experiences (0, 1, 2, 3 or more)
egen Trauma_ctgrs_nm = rownonmiss(Tr_accidents Tr_ndisasters Tr_communityv Tr_domesticv Tr_physicalv Tr_sexual Tr_sdeathlo)
egen Trauma_ctgrs = rowtotal(Tr_accidents Tr_ndisasters Tr_communityv Tr_domesticv Tr_physicalv Tr_sexual Tr_sdeathlo) if Trauma_ctgrs_nm==7

recode Trauma_ctgrs (7 6 5 4 = 3), gen(Trauma_ctgrs_r)

egen Trauma_nm = rownonmiss(ksads_ptsd_raw_754_p ksads_ptsd_raw_755_p ksads_ptsd_raw_756_p ksads_ptsd_raw_757_p ksads_ptsd_raw_758_p ksads_ptsd_raw_759_p ksads_ptsd_raw_760_p ksads_ptsd_raw_761_p ksads_ptsd_raw_762_p ksads_ptsd_raw_763_p ksads_ptsd_raw_764_p ksads_ptsd_raw_765_p ksads_ptsd_raw_766_p ksads_ptsd_raw_767_p ksads_ptsd_raw_768_p ksads_ptsd_raw_769_p ksads_ptsd_raw_770_p)

egen Trauma_eva = rowtotal(ksads_ptsd_raw_754_p ksads_ptsd_raw_755_p ksads_ptsd_raw_756_p ksads_ptsd_raw_757_p ksads_ptsd_raw_758_p ksads_ptsd_raw_759_p ksads_ptsd_raw_760_p ksads_ptsd_raw_761_p ksads_ptsd_raw_762_p ksads_ptsd_raw_763_p ksads_ptsd_raw_764_p ksads_ptsd_raw_765_p ksads_ptsd_raw_766_p ksads_ptsd_raw_767_p ksads_ptsd_raw_768_p ksads_ptsd_raw_769_p ksads_ptsd_raw_770_p) if Trauma_nm==17

** Items
*ksads_ptsd_raw_754_p: 		A car accident in which your child or another person in the car was hurt bad enough to require medical attention
*ksads_ptsd_raw_755_p: 		Another significant accident for which your child needed specialized and intensive medical treatment
*ksads_ptsd_raw_756_p: 		Witnessed or caught in a fire that caused significant property damage or personal injury
*ksads_ptsd_raw_757_p: 		Witnessed or caught in a natural disaster that caused significant property damage or personal injury
*ksads_ptsd_raw_758_p: 		Witnessed or present during an act of terrorism (e.g., Boston marathon bombing)
*ksads_ptsd_raw_759_p: 		Witnessed death or mass destruction in a war zone
*ksads_ptsd_raw_760_p: 		Witnessed someone shot or stabbed in the community
*ksads_ptsd_raw_761_p: 		Shot, stabbed, or beaten brutally by a non-family member
*ksads_ptsd_raw_762_p: 		Shot, stabbed, or beaten brutally by a grown up in the home
*ksads_ptsd_raw_763_p:		Beaten to the point of having bruises by a grown up in the home
*ksads_ptsd_raw_764_p:		A non-family member threatened to kill your child
*ksads_ptsd_raw_765_p:		A family member threatened to kill your child
*ksads_ptsd_raw_766_p:		Witness the grownups in the home push, shove or hit one another
*ksads_ptsd_raw_767_p:		A grown up in the home touched your child in their privates, had your child touch their privates, or did other sexual things to your child
*ksads_ptsd_raw_768_p:		An adult outside your family touched your child in their privates, had your child touch their privates or did other sexual things to your child
*ksads_ptsd_raw_769_p:		A peer forced your child to do something sexually
*ksads_ptsd_raw_770_p:		Learned about the sudden unexpected death of a loved one	(Almost 25% of children suffered it)

global Traumas_LE	"Tr_accidents Tr_ndisasters Tr_communityv Tr_domesticv Tr_physicalv Tr_sexual Tr_sdeathlo Trauma_ctgrs Trauma_ctgrs_r Trauma_eva"


******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Demographics

	* White: 							White race
		egen white = max(demo_race_a_p___10), by(src_subject_id)
		egen care_white = max(demo_prnt_race_a_v2___10), by(src_subject_id)

	* Black:							Black race
		egen black = max(demo_race_a_p___11), by(subjectkey)
		egen care_black = max(demo_prnt_race_a_v2___11), by(src_subject_id)

	* Native:							Native race
		gen 	native_aux = 1 if demo_race_a_p___12==1 | demo_race_a_p___13==1 | demo_race_a_p___14==1 | demo_race_a_p___15==1 | demo_race_a_p___16==1 | demo_race_a_p___17==1
		replace native_aux = 0 if native_aux==.
		egen 	native = max(native_aux), by(src_subject_id)

	* Asian:							Asian race
		gen 	asian_aux = 1 if demo_race_a_p___18==1 | demo_race_a_p___19==1 | demo_race_a_p___20==1 | demo_race_a_p___21==1 | demo_race_a_p___22==1 | demo_race_a_p___23==1 | demo_race_a_p___24==1
		replace asian_aux = 0 if asian_aux==.
		egen 	asian = max(asian_aux), by(src_subject_id)

	* Other race: 				BIS/BAS: BAS drive:  bisbas13_y + bisbas14_y + bisbas15_y + bisbas16_y; Validat
		egen other_r = max(demo_race_a_p___25), by(src_subject_id)

	* Hispanic: 					BIS/BAS: BAS Fun Seeking: bisbas17_y + bisbas18_y + bisbas19_y + bisbas20_y; Val
		gen 	hisp_lat_aux = 1 if demo_ethn_v2==1
		replace hisp_lat_aux = 0 if demo_ethn_v2==2
		egen 	hisp_lat = max(hisp_lat_aux), by(src_subject_id)
	
	* Has a partner
	gen 	partner = 1 if demo_prnt_prtnr_v2==1
	replace	partner = 0 if demo_prnt_prtnr_v2==2
	
	* Married + living with a partner
	gen 	cohabit = 1 if demo_prnt_marital_v2==1 | demo_prnt_marital_v2==6
	replace cohabit = 0 if demo_prnt_marital_v2>=2 & demo_prnt_marital_v2<=5

	
******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** History of problems
	
	** History of Nervous breakdown
	rename famhx_ss_fath_prob_nrv_p 	fatherh_nrvsb
	rename famhx_ss_moth_prob_nrv_p 	motherh_nrvsb
	rename famhx_ss_parent_nrv_p 		fmtherh_nrvsb
	rename famhx_ss_patgf_prob_nrv_p 	patgraf_nrvsb
	rename famhx_ss_patgm_prob_nrv_p 	patgram_nrvsb
	rename famhx_ss_matgf_prob_nrv_p 	matgraf_nrvsb
	rename famhx_ss_matgm_prob_nrv_p 	matgram_nrvsb

	egen floadp_nrvsb = rowtotal(fatherh_nrvsb motherh_nrvsb), miss
	egen floadgp_nrvsb = rowtotal(patgraf_nrvsb patgram_nrvsb matgraf_nrvsb matgram_nrvsb), miss

	gen 	hist_nrvsb = 0 if floadp_nrvsb==0 & floadgp_nrvsb==0
	replace hist_nrvsb = 1 if floadp_nrvsb>=1 & floadp_nrvsb<. & floadgp_nrvsb==0
	replace hist_nrvsb = 2 if floadp_nrvsb==0 & floadgp_nrvsb>=1 & floadgp_nrvsb<.
	replace hist_nrvsb = 3 if floadp_nrvsb>=1 & floadp_nrvsb<. & floadgp_nrvsb>=1 & floadgp_nrvsb<.

	label define hist_nrvsb 0 "No family load" 1 "Parental load" 2 "Grand parents generation load" 3 "Both parents and grandparents load" 
	label values hist_nrvsb hist_nrvsb

	** History of tought problems
	* Mania
	rename famhx_ss_fath_prob_ma_p 	fatherh_mania
	rename famhx_ss_moth_prob_ma_p 	motherh_mania
	rename famhx_ss_parent_ma_p 	fmtherh_mania
	rename famhx_ss_patgf_prob_ma_p patgraf_mania
	rename famhx_ss_patgm_prob_ma_p patgram_mania
	rename famhx_ss_matgf_prob_ma_p matgraf_mania
	rename famhx_ss_matgm_prob_ma_p matgram_mania

	egen floadp_mania = rowtotal(fatherh_mania motherh_mania), miss
	egen floadgp_mania = rowtotal(patgraf_mania patgram_mania matgraf_mania matgram_mania), miss

	gen 	hist_mania = 0 if floadp_mania==0 & floadgp_mania==0
	replace hist_mania = 1 if floadp_mania>=1 & floadp_mania<. & floadgp_mania==0
	replace hist_mania = 2 if floadp_mania==0 & floadgp_mania>=1 & floadgp_mania<.
	replace hist_mania = 3 if floadp_mania>=1 & floadp_mania<. & floadgp_mania>=1 & floadgp_mania<.

	label define hist_mania 0 "No family load" 1 "Parental load" 2 "Grand parents generation load" 3 "Both parents and grandparents load" 
	label values hist_mania hist_mania

	* Paranoia
	rename famhx_ss_fath_prob_vs_p 	fatherh_paran
	rename famhx_ss_moth_prob_vs_p 	motherh_paran
	rename famhx_ss_parent_vs_p 	fmtherh_paran
	rename famhx_ss_patgf_prob_vs_p patgraf_paran
	rename famhx_ss_patgm_prob_vs_p patgram_paran
	rename famhx_ss_matgf_prob_vs_p matgraf_paran
	rename famhx_ss_matgm_prob_vs_p matgram_paran

	egen floadp_paran = rowtotal(fatherh_paran motherh_paran), miss
	egen floadgp_paran = rowtotal(patgraf_paran patgram_paran matgraf_paran matgram_paran), miss

	gen 	hist_paran = 0 if floadp_paran==0 & floadgp_paran==0
	replace hist_paran = 1 if floadp_paran>=1 & floadp_paran<. & floadgp_paran==0
	replace hist_paran = 2 if floadp_paran==0 & floadgp_paran>=1 & floadgp_paran<.
	replace hist_paran = 3 if floadp_paran>=1 & floadp_paran<. & floadgp_paran>=1 & floadgp_paran<.

	label define hist_paran 0 "No family load" 1 "Parental load" 2 "Grand parents generation load" 3 "Both parents and grandparents load" 
	label values hist_paran hist_paran


	** Depressive symptoms
	rename famhx_ss_fath_prob_dprs_p 	fatherh_dprsv
	rename famhx_ss_moth_prob_dprs_p 	motherh_dprsv
	rename famhx_ss_parent_dprs_p 		fmtherh_dprsv
	rename famhx_ss_patgf_prob_dprs_p 	patgraf_dprsv
	rename famhx_ss_patgm_prob_dprs_p 	patgram_dprsv
	rename famhx_ss_matgf_prob_dprs_p 	matgraf_dprsv
	rename famhx_ss_matgm_prob_dprs_p 	matgram_dprsv

	egen floadp_dprsv = rowtotal(fatherh_dprsv motherh_dprsv), miss
	egen floadgp_dprsv = rowtotal(patgraf_dprsv patgram_dprsv matgraf_dprsv matgram_dprsv), miss


	gen 	hist_dprsv = 0 if floadp_dprsv==0 & floadgp_dprsv==0
	replace hist_dprsv = 1 if floadp_dprsv>=1 & floadp_dprsv<. & floadgp_dprsv==0
	replace hist_dprsv = 2 if floadp_dprsv==0 & floadgp_dprsv>=1 & floadgp_dprsv<.
	replace hist_dprsv = 3 if floadp_dprsv>=1 & floadp_dprsv<. & floadgp_dprsv>=1 & floadgp_dprsv<.

	label define hist_dprsv 0 "No family load" 1 "Parental load" 2 "Grand parents generation load" 3 "Both parents and grandparents load" 
	label values hist_dprsv hist_dprsv
	*famhx_ss_fath_prob_dprs_p famhx_ss_moth_prob_dprs_p famhx_ss_momdad_dprs_p famhx_ss_parent_dprs_p

	* Externalizing symptoms
	rename famhx_ss_fath_prob_trb_p 	fatherh_trbl
	rename famhx_ss_moth_prob_trb_p 	motherh_trbl
	rename famhx_ss_parent_trb_p 		fmtherh_trbl
	rename famhx_ss_patgf_prob_trb_p 	patgraf_trbl
	rename famhx_ss_patgm_prob_trb_p 	patgram_trbl
	rename famhx_ss_matgf_prob_trb_p 	matgraf_trbl
	rename famhx_ss_matgm_prob_trb_p 	matgram_trbl

	egen floadp_trbl = rowtotal(fatherh_trbl motherh_trbl), miss
	egen floadgp_trbl = rowtotal(patgraf_trbl patgram_trbl matgraf_trbl matgram_trbl), miss


	gen 	hist_trbl = 0 if floadp_trbl==0 & floadgp_trbl==0
	replace hist_trbl = 1 if floadp_trbl>=1 & floadp_trbl<. & floadgp_trbl==0
	replace hist_trbl = 2 if floadp_trbl==0 & floadgp_trbl>=1 & floadgp_trbl<.
	replace hist_trbl = 3 if floadp_trbl>=1 & floadp_trbl<. & floadgp_trbl>=1 & floadgp_trbl<.

	label define hist_trbl 0 "No family load" 1 "Parental load" 2 "Grand parents generation load" 3 "Both parents and grandparents load" 
	label values hist_trbl hist_trbl


	* At least one parent (or both) is category 3
	foreach f in famhx_ss_parent_alc_p famhx_ss_parent_dg_p fmtherh_dprsv fmtherh_mania fmtherh_paran fmtherh_trbl fmtherh_nrvsb famhx_ss_parent_prf_p famhx_ss_parent_hspd_p famhx_ss_parent_scd_p{
		*replace `f' = 4 if `f'==3
		replace `f' = 3 if `f'==-1 | `f'==-2 
	}

	egen internal_p = rowmax(fmtherh_dprsv fmtherh_nrvsb)
	egen thougth_p = rowmax(fmtherh_mania fmtherh_paran)
	egen external_p = rowmax(fmtherh_trbl famhx_ss_parent_alc_p famhx_ss_parent_dg_p)

******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Mental health children
gen 	any_dschiz = 1 if uschizoph_c==1 | hallucina_c==1 | delusionsp_c==1 | psychotic_c==1
replace any_dschiz = 0 if any_dschiz==. & (uschizoph_c==0 | hallucina_c==0 | delusionsp_c==0 | psychotic_c==0)

* Psychosis severity
gen 	pps_ss_mean_severityc = pps_ss_mean_severity
replace pps_ss_mean_severityc = 0 if pps_ss_mean_severity==. & pps_y_ss_number==0


** Diagnoses of MDD

rename ksads_1_840_p dgnsmdd_p 
rename ksads_1_840_t dgnsmdd_y

global health_speccbcl 	"cbcl_scr_syn_external_t cbcl_scr_syn_internal_t cbcl_scr_syn_internal_r cbcl_scr_syn_thought_r cbcl_scr_syn_attention_r cbcl_scr_syn_external_r"
*global health_subfact 	"sex_problems;eating_problems;fear;distress;mania;substance_abuse;antisocial_behavior"
global health_syndcbcl 	"cbcl_scr_dsm5_depress_r cbcl_scr_dsm5_anxdisord_r cbcl_scr_dsm5_somaticpr_r cbcl_scr_dsm5_adhd_r cbcl_scr_dsm5_opposit_r cbcl_scr_07_sct_r cbcl_scr_07_ocd_r cbcl_scr_07_stress_r"
global health_syndcbclt "cbcl_scr_dsm5_depress_t cbcl_scr_dsm5_anxdisord_t cbcl_scr_dsm5_somaticpr_t cbcl_scr_dsm5_adhd_t cbcl_scr_dsm5_opposit_t cbcl_scr_07_sct_t cbcl_scr_07_ocd_t cbcl_scr_07_stress_t"

global dsm5_MDD "ksads_1_1_p ksads_1_3_p ksads_1_5_p ksads_22_141_p ksads_23_143_p ksads_23_145_p ksads_23_147_p ksads_23_149_p ksads_1_157_p ksads_1_159_p ksads_1_161_p ksads_1_163_p ksads_1_165_p ksads_1_167_p ksads_1_169_p ksads_1_171_p ksads_1_173_p ksads_1_175_p ksads_1_177_p ksads_1_179_p ksads_1_181_p ksads_1_183_p"

global dsm5y_MDD "ksads_1_1_t ksads_1_3_t ksads_1_5_t ksads_22_141_t ksads_23_143_t ksads_23_145_t ksads_23_147_t ksads_23_149_t ksads_1_157_t ksads_1_159_t ksads_1_161_t ksads_1_163_t ksads_1_165_t ksads_1_167_t ksads_1_169_t ksads_1_171_t ksads_1_173_t ksads_1_175_t ksads_1_177_t ksads_1_179_t ksads_1_181_t ksads_1_183_t dpry_eating dpry_sleep dpry_motor dpry_energy drpy_worth drpy_decis drpy_suic"

** Non CBCL scales
* Sip of alcohol
gen 	sip_alcohol = tlfb_alc_sip if eventname=="baseline_year_1_arm_1"
replace sip_alcohol = tlfb_alc_sip_l if eventname!="baseline_year_1_arm_1"

* Tried cigarrettes
gen 	puff_cig = tlfb_tob_puff if eventname=="baseline_year_1_arm_1"
replace puff_cig = tlfb_tob_puff_l if eventname!="baseline_year_1_arm_1"

global health_syndr 	"Dsm5_mdd Dsm5_mddP dsm5_mddvm dsm5_mddvmP Dsm5_dys Dsm5_manic Dsm5_psyc Dsm5_psycr Dsm5_sepx Dsm5_anxpb Dsm5_eatd Dsm5_adhdina Dsm5_adhdimp Dsm5_adhd Dsm5_odd Dsm5_condd Dsm5_suicb Dsm5y_mdd Dsm5y_mddP dsm5y_mddvm dsm5y_mddvmP Dsm5y_dys dsm5y_mdd dsm5y_dys pps_ss_mean_severityc mddy_diagn dsm5_odd dsm5_oddp dsm5_condd dsm5_conddp dsm5_conddP dsm5_oddP dsm5_adhd dsm5_adhdinaP dsm5_adhdimpP dsm5_adhdP dsm5y_mania Dsm5y_mania dsm5y_maniaP Dsm5y_maniaP dsm5y_moodd Dsm5y_moodd dsm5y_sanx Dsm5y_sanx dsm5y_sanxP Dsm5y_sanxP dsm5y_ganx Dsm5y_ganx dsm5y_ganxP Dsm5y_ganxP dsm5y_sipa Dsm5y_sipa dsm5y_sinj Dsm5y_sinj dsm5y_side Dsm5y_side dsm5y_satt Dsm5y_satt dsm5y_sipaP Dsm5y_sipaP dsm5y_sinjP Dsm5y_sinjP dsm5y_sideP Dsm5y_sideP dsm5y_sattP Dsm5y_sattP ksads_2_830_t ksads_2_831_t ksads_2_832_t ksads_2_833_t ksads_2_834_t ksads_2_835_t ksads_2_836_t ksads_2_837_t ksads_1_842_t ksads_1_843_t ksads_1_845_t ksads_8_863_t ksads_8_864_t ksads_10_869_t ksads_10_870_t any_idiagnosis any_idiagnosis_b any_idiagnosisP any_idiagnosis_bP dgnsmdd_p dgnsmdd_y dsm5y_ints dsm5_ints dsm5_exts ksads_14_853_p ksads_15_901_p ksads_16_897_p ksads_16_898_p"
*drpy_suic Dsm5y_manic Dsm5y_anxpb Dsm5y_suicb pgbi_p_ss_score dsm5y_mooddP Dsm5y_mooddP 
** Syndromes/disorders
global health_psychs 	"pps_y_ss_number pps_ss_mean_severityc psychotic_c any_dschiz"
global health_mania 	"pgbi_p_ss_score bpimanic_c bpidepre_c bpihyman_c bpiidepre_c bpiihyman_c bpunspec_c"

global asr_depre 		"asr_q14_p asr_q18_p asr_q24_p asr_q35_p asr_q52_p asr_q54_p asr_q60_p asr_q77_p asr_q78_p asr_q91_p asr_q100_p asr_q102_p asr_q103_p asr_q107_p"


******************************************************************************************************************************
******************************************************************************************************************************
******************************************************************************************************************************
***** Brain structure

*************************
***** Cortical regions

*** Frontal lobe: mPFC, dlPFC, ACC, OFC

** Regions in the Prefrontal Cortex

* Frontal pole (part of the dorso lateral PFC)
gen fropl_ic = (smri_vol_cdk_frpolelh)/smri_vol_scs_intracranialv 		/* Frontal pole */
gen fropr_ic = (smri_vol_cdk_frpolerh)/smri_vol_scs_intracranialv 		/* Frontal pole */
gen frop_ic = (fropl_ic+fropr_ic)/2 

* Caudal Middle Frontal Cortex (MFC towards the tail)
gen cmfcl_ic = smri_vol_cdk_cdmdfrlh/smri_vol_scs_intracranialv 		/* Caudal Middle Frontal Cortex */
gen cmfcr_ic = smri_vol_cdk_cdmdfrrh/smri_vol_scs_intracranialv 		/* Caudal Middle Frontal Cortex */
gen cmfc_ic = (cmfcl_ic+cmfcr_ic)/2

* Rostral Middle Frontal Cortex (MFC towards the nose/beak)
gen rmfcl_ic = smri_vol_cdk_rrmdfrlh/smri_vol_scs_intracranialv 		/* Rostral Middle Frontal Cortex */
gen rmfcr_ic = smri_vol_cdk_rrmdfrrh/smri_vol_scs_intracranialv 		/* Rostral Middle Frontal Cortex */
gen rmfc_ic = (rmfcl_ic+rmfcr_ic)/2

* Superior Frontal Cortex
gen sfcl_ic = smri_vol_cdk_sufrlh/smri_vol_scs_intracranialv 			/* Superior Frontal Cortex */
gen sfcr_ic = smri_vol_cdk_sufrrh/smri_vol_scs_intracranialv 			/* Superior Frontal Cortex */
gen sfc_ic = (rmfcl_ic+rmfcr_ic)/2

** Anterior Cingulate Cortex
* Caudal Anterior Cingulate Cortex (ACC towards the tail)
gen caccl_ic = smri_vol_cdk_cdacatelh/smri_vol_scs_intracranialv 		/* Caudal anterior cingulate cortex */
gen caccr_ic = smri_vol_cdk_cdacaterh/smri_vol_scs_intracranialv 		/* Caudal anterior cingulate cortex */
gen cacc_ic = (caccl_ic+caccr_ic)/2

clonevar vcaccl = smri_vol_cdk_cdacatelh
clonevar vcaccr = smri_vol_cdk_cdacaterh

* Rostral Anterior Cingulate Cortex (ACC towards the nose/beak)
gen raccl_ic = smri_vol_cdk_rracatelh/smri_vol_scs_intracranialv 		/* Rostral anterior cingulate cortex */
gen raccr_ic = smri_vol_cdk_rracaterh/smri_vol_scs_intracranialv 		/* Rostral anterior cingulate cortex */
gen racc_ic = (raccl_ic+raccr_ic)/2

clonevar vraccl = smri_vol_cdk_rracatelh
clonevar vraccr = smri_vol_cdk_rracaterh

** Orbitofrontal Cortex (OFC)
* Medial Orbitofrontal Cortex (OFC around the midline)
gen morbfl_ic = (smri_vol_cdk_mobfrlh)/smri_vol_scs_intracranialv 		/* Medial orbitofrontal */
gen morbfr_ic = (smri_vol_cdk_mobfrrh)/smri_vol_scs_intracranialv 		/* Medial orbitofrontal */
gen morbf_ic = (morbfl_ic+morbfr_ic)/2

clonevar vmorbfl = smri_vol_cdk_mobfrlh
clonevar vmorbfr = smri_vol_cdk_mobfrrh

* Lateral Orbitofrontal Cortex (OFC away from the midline)
gen lorbfl_ic = (smri_vol_cdk_lobfrlh)/smri_vol_scs_intracranialv 		/* Lateral orbitofrontal */
gen lorbfr_ic = (smri_vol_cdk_lobfrrh)/smri_vol_scs_intracranialv 		/* Lateral orbitofrontal */
gen lorbf_ic = (lorbfl_ic+lorbfr_ic)/2

clonevar vlorbfl = smri_vol_cdk_lobfrlh
clonevar vlorbfr = smri_vol_cdk_lobfrrh

* Insula
gen insul_ic = smri_vol_cdk_insulalh/smri_vol_scs_intracranialv 		/* Insula subdivisions not available - Mid and posterior seem to be more important for mental health */
gen insur_ic = smri_vol_cdk_insularh/smri_vol_scs_intracranialv 		/* Insula subdivisions not available - Mid and posterior seem to be more important for mental health */
gen insu_ic = (insul_ic+insur_ic)/2 	

clonevar vinsul = smri_vol_cdk_insulalh
clonevar vinsur = smri_vol_cdk_insularh

***************************
** Subcortical regions
gen amygl_ic = smri_vol_scs_amygdalalh/smri_vol_scs_intracranialv		/* Amygdala */
gen amygr_ic = smri_vol_scs_amygdalarh/smri_vol_scs_intracranialv		/* Amygdala */
gen amyg_ic = (amygl_ic+amygr_ic)/2 	

clonevar vamygrl = smri_vol_scs_amygdalalh
clonevar vamygrr = smri_vol_scs_amygdalarh

gen hippl_ic = smri_vol_scs_hpuslh/smri_vol_scs_intracranialv			/* Hippocampus */
gen hippr_ic = smri_vol_scs_hpusrh/smri_vol_scs_intracranialv			/* Hippocampus */
gen hipp_ic = (hippl_ic+hippr_ic)/2 

clonevar vhippl = smri_vol_scs_hpuslh
clonevar vhippr = smri_vol_scs_hpusrh

gen thall_ic = smri_vol_scs_tplh/smri_vol_scs_intracranialv				/* Thalamus (subdivisions not available. It seems dorsomedial is relevant for mental health) */
gen thalr_ic = smri_vol_scs_tprh/smri_vol_scs_intracranialv				/* Thalamus (subdivisions not available. It seems dorsomedial is relevant for mental health) */
gen thal_ic = (thall_ic+thalr_ic)/2 	
	

***************************
** Basal Ganglia regions
gen stril_ic = (smri_vol_scs_putamenlh+smri_vol_scs_caudatelh+smri_vol_scs_aal)/smri_vol_scs_intracranialv
gen strir_ic = (smri_vol_scs_putamenrh+smri_vol_scs_caudaterh+smri_vol_scs_aar)/smri_vol_scs_intracranialv
gen stri_ic = (stril_ic+strir_ic)/2 	

gen dstril_ic = (smri_vol_scs_putamenlh+smri_vol_scs_caudatelh)/smri_vol_scs_intracranialv
gen dstrir_ic = (smri_vol_scs_putamenrh+smri_vol_scs_caudaterh)/smri_vol_scs_intracranialv
gen dstri_ic = (dstril_ic+dstrir_ic)/2 	

gen vstril_ic = (smri_vol_scs_aal)/smri_vol_scs_intracranialv 
gen vstrir_ic = (smri_vol_scs_aar)/smri_vol_scs_intracranialv 
gen vstri_ic = (vstril_ic+vstrir_ic)/2 	

gen dvstril_ic = (smri_vol_scs_putamenlh+smri_vol_scs_aal)/smri_vol_scs_intracranialv 
gen dvstrir_ic = (smri_vol_scs_putamenrh+smri_vol_scs_aar)/smri_vol_scs_intracranialv
gen dvstri_ic = (dvstril_ic+dvstrir_ic)/2 	

gen putamenl_ic = (smri_vol_scs_putamenlh)/smri_vol_scs_intracranialv
gen putamenr_ic = (smri_vol_scs_putamenrh)/smri_vol_scs_intracranialv
gen putamen_ic = (putamenl_ic+putamenr_ic)/2 

gen caudatel_ic = (smri_vol_scs_caudatelh)/smri_vol_scs_intracranialv
gen caudater_ic = (smri_vol_scs_caudaterh)/smri_vol_scs_intracranialv
gen caudate_ic = (caudatel_ic+caudater_ic)/2 	

clonevar vvstril = smri_vol_scs_aal
clonevar vvstrir = smri_vol_scs_aar 

clonevar vputamenl = smri_vol_scs_putamenlh 
clonevar vputamenr = smri_vol_scs_putamenrh 

clonevar vcaudatel = smri_vol_scs_caudatelh 
clonevar vcaudater = smri_vol_scs_caudaterh

***************************
** Amygdala reactivity

rename tfmri_nback_all_224 amygl_rn
rename tfmri_nback_all_238 amygr_rn
rename tfmri_nback_all_254 amygl_rp
rename tfmri_nback_all_268 amygr_rp

label var amygl_rn "Amygdala (left) reactivity to negative faces (relative to neutral)"
label var amygr_rn "Amygdala (right) reactivity to negative faces (relative to neutral)"
label var amygl_rp "Amygdala (left) reactivity to positive faces (relative to neutral)"
label var amygr_rp "Amygdala (right) reactivity to positive faces (relative to neutral)"

global brain_cortical 		"frop_ic rmfc_ic cmfc_ic sfcl_ic caccl_ic caccr_ic raccl_ic raccr_ic morbfl_ic morbfr_ic lorbfl_ic lorbfr_ic insul_ic insur_ic vcaccl vcaccr vraccl vraccr vlorbfl vlorbfr vinsul vinsur"
global brain_subcortical 	"amygl_ic amygr_ic hippl_ic hippr_ic hipp_ic thal_ic smri_vol_scs_intracranialv vhippl vhippr vamygrl vamygrr"
global basal_ganglia 		"vstri_ic putamen_ic caudate_ic vvstril vvstrir vputamenl vputamenr vcaudatel vcaudater"
global amygdala_react 		"amygl_rn amygr_rn amygl_rp amygr_rp"


**************************************************************************************
************************* 3. Setting up the data *************************************
**************************************************************************************

**************************************************************************************
** Regression adjustments

egen acs_raked_propensity_score_ay = max(acs_raked_propensity_score), by(subjectkey)
global weights			"acs_raked_propensity_score_ay"

encode src_subject_id, gen(id_subj)

gen 	period = 1 if eventname=="baseline_year_1_arm_1"
replace period = 2 if eventname=="1_year_follow_up_y_arm_1"
replace period = 3 if eventname=="2_year_follow_up_y_arm_1"
keep if period !=.

gen 	sex = 1 if demo_sex_v2==2  
replace sex = 0 if demo_sex_v2==1 

gen age = interview_age

global control_vars 	"hist_nrvsb hist_paran hist_mania hist_dprsv hist_trbl fam_income_pc white sex age asr_scr_somatic_r internal_p thougth_p external_p famhx_ss_parent_alc_p famhx_ss_parent_dg_p fmtherh_dprsv fmtherh_mania fmtherh_paran fmtherh_trbl fmtherh_nrvsb famhx_ss_parent_prf_p famhx_ss_parent_hspd_p famhx_ss_parent_scd_p sibling asr_scr_perstr_t asr_scr_anxdep_t asr_scr_withdrawn_t asr_scr_somatic_t asr_scr_thought_t asr_scr_attention_t asr_scr_aggressive_t asr_scr_rulebreak_t asr_scr_intrusive_t asr_scr_internal_t asr_scr_external_t asr_scr_totprob_t asr_scr_depress_t asr_scr_anxdisord_t asr_scr_somaticpr_t asr_scr_avoidant_t asr_scr_adhd_t asr_scr_antisocial_t asr_scr_inattention_t asr_scr_hyperactive_t educ_lev puberty_v partner cohabit depr_med adhd_med thog_med"
global dsm5y_sipa 		"ksads_23_807_t ksads_23_808_t ksads_23_809_t ksads_23_810_t ksads_23_811_t ksads_23_812_t ksads_23_813_t ksads_23_814_t ksads_23_815_t"
global elevel			"rel_family_id site_id_l rel_relationship family_rel"

*trauma_ev trauma_eva trauma_evb Trauma_ev Trauma_eva Trauma_evb Trauma_sex Trauma_osex 

* Keeping relevant variables
keep id_subj period $health_speccbcl $health_syndcbcl $health_syndcbclt $health_syndr $bullying_vars $family_vars $friend_vars $school_vars $nhood_vars $emoreg_vars $Traumas_LE $dsm5y_MDD $brain_subcortical $amygdala_react $control_vars $elevel $weights $dsm5y_sipa
*$asr_depre
reshape wide $health_speccbcl $health_syndcbcl $health_syndcbclt $health_syndr $bullying_vars $family_vars $friend_vars $school_vars $nhood_vars $emoreg_vars $Traumas_LE $dsm5y_MDD $brain_subcortical $amygdala_react $control_vars $elevel $weights $dsm5y_sipa, i(id_subj) j(period)
*$asr_depre 

tab 	site_id_l1, gen(site_dum)
encode 	site_id_l1, gen(site_cat)

*drop if age3==.
drop if site_dum22==1

**************************************************************************************
**** Standardizing and centering relevant variables

replace fam_income_pc1 = fam_income_pc1/1000

global tostd 	"cbcl_scr_syn_external_t cbcl_scr_syn_internal_t cbcl_scr_syn_internal_r cbcl_scr_syn_thought_r cbcl_scr_syn_attention_r cbcl_scr_syn_external_r cbcl_scr_dsm5_depress_t cbcl_scr_dsm5_depress_r cbcl_scr_dsm5_anxdisord_r cbcl_scr_dsm5_somaticpr_r cbcl_scr_dsm5_adhd_r cbcl_scr_dsm5_opposit_r cbcl_scr_07_sct_r cbcl_scr_07_ocd_r cbcl_scr_07_stress_r Dsm5_mdd Dsm5_dys Dsm5_manic Dsm5_psyc Dsm5_psycr Dsm5_sepx Dsm5_anxpb Dsm5_eatd Dsm5_adhdina Dsm5_adhdimp Dsm5_adhd Dsm5_odd Dsm5_condd Dsm5_suicb Dsm5y_mdd Dsm5y_dys dsm5y_mdd dsm5y_dys dsm5y_mania Dsm5y_mania dsm5y_maniaP Dsm5y_maniaP dsm5y_moodd Dsm5y_moodd dsm5y_sanx Dsm5y_sanx dsm5y_sanxP Dsm5y_sanxP dsm5y_ganx Dsm5y_ganx dsm5y_ganxP Dsm5y_ganxP dsm5y_sipa Dsm5y_sipa dsm5y_sinj Dsm5y_sinj dsm5y_side Dsm5y_side dsm5y_satt Dsm5y_satt dsm5y_sipaP Dsm5y_sipaP dsm5y_sinjP Dsm5y_sinjP dsm5y_sideP Dsm5y_sideP dsm5y_sattP Dsm5y_sattP pps_ss_mean_severityc peer1 peer4 p_acceptance first_acc secon_acc aver_accept dad_accept mom_accept fam_confav asr_scr_internal_t asr_scr_totprob_t asr_scr_somatic_r asr_scr_depress_t asr_scr_thought_t asr_scr_anxdisord_t asr_scr_adhd_t asr_scr_aggressive_t asr_scr_avoidant_t asr_scr_external_t asr_scr_attention_t tfmri_nb_all_beh_ctotal_nt n_cfriends school_positive reshist_addr1_adi_wsum hippl_ic hippr_ic hipp_ic dsm5y_mddvm dsm5_mddvm pcwarmth"

*dsm5y_mooddP Dsm5y_mooddP 

drop acs_raked_propensity_score_ay2 acs_raked_propensity_score_ay3 site_id_l2 sex2 age2 fam_income_pc2

foreach v in $tostd {
	egen `v'sd1 = std(`v'1)
	cap egen `v'sd2 = std(`v'2)
	cap egen `v'sd3 = std(`v'3)
	
	forvalues t=1/3{
		sum `v'`t'
		gen `v'c`t' = `v'`t' - r(mean)
	}
	
}

gen neighbor = reshist_addr1_adi_wsum1

tab educ_lev1, gen(educ_lev1)

tab hist_dprsv1, gen(hist_dprsv1)
tab hist_nrvsb1, gen(hist_nrvsb1)
tab hist_trbl1, gen(hist_trbl1)
tab hist_paran1, gen(hist_paran1)

global covars "age1 sex1 white1 puberty_v1 neighbor Trauma_eva1 hist_dprsv12 hist_dprsv13 hist_dprsv14 cohabit1 educ_lev12 educ_lev13 educ_lev14 educ_lev15"
global covarsG "age1 white1 puberty_v1 neighbor Trauma_eva1 hist_dprsv12 hist_dprsv13 hist_dprsv14 cohabit1 educ_lev12 educ_lev13 educ_lev14 educ_lev15"
global covarsD "age1 sex1 white1 puberty_v1 neighbor Trauma_eva1 hist_dprsv11 hist_dprsv12 hist_dprsv13 hist_dprsv14 cohabit1 educ_lev11 educ_lev12 educ_lev13 educ_lev14 educ_lev15"

* Transformations
replace hippl_icc1 = hippl_icc1 * 1000
replace hippr_icc1 = hippr_icc1 * 1000

replace hipp_ic1  = hipp_ic1  * 1000
replace hippl_ic1 = hippl_ic1 * 1000
replace hippr_ic1 = hippr_ic1 * 1000

replace hipp_ic3  = hipp_ic3  * 1000
replace hippl_ic3 = hippl_ic3 * 1000
replace hippr_ic3 = hippr_ic3 * 1000

replace asr_scr_depress_t1 = asr_scr_depress_t1/10

global Hvars "hipp_ic1 hippl_ic1 hippr_ic1"


********************************************************************************
******************************* 4. Results *************************************
********************************************************************************


** Outcome variable
gen 		d_mddy3 = dsm5y_mddvm3 - dsm5y_mddvm1
clonevar 	d_mddy1 = dsm5y_mddvm1

* Transformations - square root
gen 		sqrt_mddyP = sqrt(dsm5y_mddvmP1)
gen 		sqrt_mddy1 = sqrt(dsm5y_mddvm1)
gen 		sqrt_mddy3 = sqrt(dsm5y_mddvm3)
gen 		dsqrt_mddy3 = sqrt_mddy3 - sqrt_mddy1
clonevar 	dsqrt_mddy1 = sqrt_mddy1

gen dmddy_diagn3 = mddy_diagn3 - mddy_diagn1


********************************************************************************
************************ Longitudinal Associations *****************************

************************
* COMPLETE CASE ANALYSIS 
*
	cap erase "${results}/Longitudinal associations/LA_CC.xls"
	cap erase "${results}/Longitudinal associations/LA_CC.txt"

*** No covariates

	matrix LC		= J(9,10,.)
	
	local n = 1
	foreach X in peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 {
	
	mixed d_mddy3 `X' d_mddy1 || site_cat: || rel_family_id1:
			quietly summ d_mddy3 if e(sample)==1
			local Ym = r(mean)
			quietly summ `X' if e(sample)==1
			local Xm = r(mean)		
			local NF = e(N_g)[1,2]
			outreg2 using "${results}/Longitudinal associations/LA_CC.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
			
		matrix SDM1 = vecdiag(e(V))
		local DF1 = e(N)-e(df_c)
		
		matrix LC[1,`n'] = e(b)[1,1]
		matrix LC[2,`n'] = sqrt(SDM1[1,1])
		matrix LC[3,`n'] = `DF1'
		matrix LC[4,`n'] = e(b)[1,1]/sqrt(SDM1[1,1])
		matrix LC[5,`n'] = 2*ttail(`DF1', abs(e(b)[1,1]/sqrt(SDM1[1,1])))
		matrix LC[6,`n'] = e(b)[1,1] - invttail(`DF1',0.025)*sqrt(SDM1[1,1])
		matrix LC[7,`n'] = e(b)[1,1] + invttail(`DF1',0.025)*sqrt(SDM1[1,1])
		matrix LC[8,`n'] = 0.75
		matrix LC[9,`n'] = 1
	
	local n = `n'+1
	}
	
	mixed d_mddy3 peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 d_mddy1 || site_cat: || rel_family_id1:
			quietly summ d_mddy3 if e(sample)==1
			local Ym = r(mean)
			quietly summ peer1sd1 if e(sample)==1
			local Xm = r(mean)		
			local NF = e(N_g)[1,2]
			outreg2 using "${results}/Longitudinal associations/LA_CC.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
		
		matrix SDM1 = vecdiag(e(V))
		local DF1 = e(N)-e(df_c)
		
		forvalues j=1/5{
			matrix LC[1,`=5+`j''] = e(b)[1,`j']
			matrix LC[2,`=5+`j''] = sqrt(SDM1[1,`j'])
			matrix LC[3,`=5+`j''] = `DF1'
			matrix LC[4,`=5+`j''] = e(b)[1,`j']/sqrt(SDM1[1,`j'])
			matrix LC[5,`=5+`j''] = 2*ttail(`DF1', abs(e(b)[1,`j']/sqrt(SDM1[1,`j'])))
			matrix LC[6,`=5+`j''] = e(b)[1,`j'] - invttail(`DF1',0.025)*sqrt(SDM1[1,`j'])
			matrix LC[7,`=5+`j''] = e(b)[1,`j'] + invttail(`DF1',0.025)*sqrt(SDM1[1,`j'])
			matrix LC[8,`=5+`j''] = `j' + 0.25
			matrix LC[9,`=5+`j''] = 2
		}
	
*** Covariates 

	matrix LCC		= J(9,10,.)
	
	local n = 1
	foreach X in peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 {
	
	mixed d_mddy3 `X' d_mddy1 $covars || site_cat: || rel_family_id1:
			quietly summ d_mddy3 if e(sample)==1
			local Ym = r(mean)
			quietly summ `X' if e(sample)==1
			local Xm = r(mean)		
			local NF = e(N_g)[1,2]
			outreg2 using "${results}/Longitudinal associations/LA_CC.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
			
		matrix SDM1 = vecdiag(e(V))
		local DF1 = e(N)-e(df_c)
		
		matrix LCC[1,`n'] = e(b)[1,1]
		matrix LCC[2,`n'] = sqrt(SDM1[1,1])
		matrix LCC[3,`n'] = `DF1'
		matrix LCC[4,`n'] = e(b)[1,1]/sqrt(SDM1[1,1])
		matrix LCC[5,`n'] = 2*ttail(`DF1', abs(e(b)[1,1]/sqrt(SDM1[1,1])))
		matrix LCC[6,`n'] = e(b)[1,1] - invttail(`DF1',0.025)*sqrt(SDM1[1,1])
		matrix LCC[7,`n'] = e(b)[1,1] + invttail(`DF1',0.025)*sqrt(SDM1[1,1])
		matrix LCC[8,`n'] = 0.75
		matrix LCC[9,`n'] = 1
	
	local n = `n'+1
	}
	
	mixed d_mddy3 peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 d_mddy1 $covars || site_cat: || rel_family_id1:
			quietly summ d_mddy3 if e(sample)==1
			local Ym = r(mean)
			quietly summ peer1sd1 if e(sample)==1
			local Xm = r(mean)		
			local NF = e(N_g)[1,2]
			outreg2 using "${results}/Longitudinal associations/LA_CC.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
		
		matrix SDM1 = vecdiag(e(V))
		local DF1 = e(N)-e(df_c)
		
		forvalues j=1/5{
			matrix LCC[1,`=5+`j''] = e(b)[1,`j']
			matrix LCC[2,`=5+`j''] = sqrt(SDM1[1,`j'])
			matrix LCC[3,`=5+`j''] = `DF1'
			matrix LCC[4,`=5+`j''] = e(b)[1,`j']/sqrt(SDM1[1,`j'])
			matrix LCC[5,`=5+`j''] = 2*ttail(`DF1', abs(e(b)[1,`j']/sqrt(SDM1[1,`j'])))
			matrix LCC[6,`=5+`j''] = e(b)[1,`j'] - invttail(`DF1',0.025)*sqrt(SDM1[1,`j'])
			matrix LCC[7,`=5+`j''] = e(b)[1,`j'] + invttail(`DF1',0.025)*sqrt(SDM1[1,`j'])
			matrix LCC[8,`=5+`j''] = `j' + 0.25
			matrix LCC[9,`=5+`j''] = 2
		}


** Plotting the coefficients

* No covariates
	matrix LCs = LC[1...,1..5]'	
	coefplot matrix(LCs[,1]), ci((LCs[,6] LCs[,7])) xline(0, lcolor(red%40))

	local rownames PV FC CD PS PW
	matrix rownames LCs = `rownames'

	matrix LCa = LC[1...,6..10]'
	matrix rownames LCa = `rownames'

	coefplot matrix(LCs[,1]), ci((LCs[,6] LCs[,7])) xline(0, lcolor(red%40)) 	///
				|| matrix(LCa[,1]), ci((LCa[,6] LCa[,7])) xline(0, lcolor(red%40)) 	

	coefplot 	(matrix(LCs[,1]), ci((LCs[,6] LCs[,7]))) 						///
				(matrix(LCa[,1]), ci((LCa[,6] LCa[,7])) 						///
				xline(0, lcolor(red%40))), 											///
				xtitle("Estimates")													///
				coeflabels(PV = "Peer victimization" FC = "Family conflict"			///
				CD = "Caregiver's depression" PS = "Prosocial school"				///
				PW = "Parental warmth") 											///
				legend(rows(2) order(2 "No covariates" 4 "No covariates and including all experiences")) 	///
				 saving("$results/Longitudinal associations/LA_CC_nocovs.gph", replace)
				 
* Covariates
	matrix LCCs = LCC[1...,1..5]'
	coefplot matrix(LCCs[,1]), ci((LCCs[,6] LCCs[,7])) xline(0, lcolor(red%40))

	local rownames PV FC CD PS PW
	matrix rownames LCCs = `rownames'

	matrix LCCa = LCC[1...,6..10]'
	matrix rownames LCCa = `rownames'

	coefplot matrix(LCCs[,1]), ci((LCCs[,6] LCCs[,7])) xline(0, lcolor(red%40)) 	///
				|| matrix(LCCa[,1]), ci((LCCa[,6] LCa[,7])) xline(0, lcolor(red%40)) 	

	coefplot 	(matrix(LCCs[,1]), ci((LCCs[,6] LCCs[,7]))) 						///
				(matrix(LCCa[,1]), ci((LCCa[,6] LCCa[,7])) 							///
				xline(0, lcolor(red%40))), 											///
				xtitle("Estimates")													///
				coeflabels(PV = "Peer victimization" FC = "Family conflict"			///
				CD = "Caregiver's depression" PS = "Prosocial school"				///
				PW = "Parental warmth") 											///
				legend(rows(2) order(2 "Including covariates" 4 "Including covariates and all experiences")) 	///
				 saving("$results/Longitudinal associations/LA_CC_covs.gph", replace)
*/				 

********************************************************************************
************************ Moderation effects of LHV *****************************

************************
* COMPLETE CASE ANALYSIS
*
	cap erase "${results}\Moderation analysis/MLHV_CC_nocovs.xls"
	cap erase "${results}\Moderation analysis/MLHV_CC_nocovs.txt"

*** No covariates

	local depvar d_mddy d_mddy d_mddy d_mddy d_mddy
	local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
	local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
	local pname PV FC CD PS PW

	local i = 1
	local j : word count `indvar'
		while `i' <= `j' {
			
			local y : word `i' of `depvar'
			local x : word `i' of `indvar'
			local r : word `i' of `xrnge'
			local p : word `i' of `pname'
			
		
		mixed `y'3 c.`x'##c.hippl_icc1 `y'1 || site_cat: || rel_family_id1:
				quietly summ `y'3 if e(sample)==1
				local Ym = r(mean)
				quietly summ `x' if e(sample)==1
				local Xm = r(mean)		
				local NF = e(N_g)[1,2]
				outreg2 using "${results}\Moderation analysis/MLHV_CC_nocovs.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
				*
					** Difference of simple slopes
					sum hippl_icc1
					local h1sd = r(sd)
					
					margins, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))
					matrix SS = r(table)
					
					local ss_b1 	= SS[1,1]
					local ss_b2 	= SS[1,2]
					local ss_b3 	= SS[1,3]
					local ss_b4 	= SS[1,4]
					local ss_se_b1 	= SS[2,1]
					local ss_se_b2 	= SS[2,2]
					local ss_se_b3 	= SS[2,3]
					local ss_se_b4 	= SS[2,4]
					local N_obs		= `e(N)'
					
					di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
					
					forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "t"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .001 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.001 local stars_b`c' "***"
							else local stars_b`c' ""
					}
					
					di "-2 SD`stars_b1'"
					di "-1 SD`stars_b2'"
					di "1 SD`stars_b3'"
					di "2 SD`stars_b4'"
					
					local ss_b1 = round(`ss_b1',.001)
					local ss_b2 = round(`ss_b2',.001)
					local ss_b3 = round(`ss_b3',.001)
					local ss_b4 = round(`ss_b4',.001)
					local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
					local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)	
					
						if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
						else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
						else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
						else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
						else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
						else local RANGE ""
					
					di "`x' - `RANGE'"
					
						if 		"`x'" == "peer11" local LYNAME "Peer victimization"
						else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
						else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
						else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
						else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
						else local LYNAME ""
					
					di "`x' - `LYNAME'"
					
					** Graph of simple slopes
						
						sum hippl_icc1
						local h1sd = r(sd)
						
						margins, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))							
												
						marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
						ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
						ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
						yscale(range(-.4(.4)1.2)) ylabel(-.4(.4)1.2, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))						///
						ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0", size(medlarge))									///
						title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
						legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
						plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
						plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
						plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
						"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
						"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
						"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
						saving("${results}\Moderation analysis/Plots_`p'_LHV_SEE_CC_nocovs.gph", replace)
						
						graph export "${results}\Moderation analysis/Plot_`p'_LHV_SEE_CC_nocovs.png", as(jpg) name("Graph") quality(90) replace	
		
		local i = `i' + 1 
		}
	
***************
*** Covariates

	** Original operationalization
	cap erase "${results}\Moderation analysis/MLHV_CC_covs.xls"
	cap erase "${results}\Moderation analysis/MLHV_CC_covs.txt"

	local depvar d_mddy d_mddy d_mddy d_mddy d_mddy
	local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
	local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
	local pname PV FC CD PS PW

	local i = 1
	local j : word count `indvar'
		while `i' <= `j' {
			
			local y : word `i' of `depvar'
			local x : word `i' of `indvar'
			local r : word `i' of `xrnge'
			local p : word `i' of `pname'
			
		
		mixed `y'3 c.`x'##c.hippl_icc1 `y'1 $covars || site_cat: || rel_family_id1:
				quietly summ `y'3 if e(sample)==1
				local Ym = r(mean)
				quietly summ `x' if e(sample)==1
				local Xm = r(mean)		
				local NF = e(N_g)[1,2]
				outreg2 using "${results}\Moderation analysis/MLHV_CC_covs.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
				*
					** Difference of simple slopes
					sum hippl_icc1
					local h1sd = r(sd)
					
					margins, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))
					matrix SS = r(table)
					
					local ss_b1 	= SS[1,1]
					local ss_b2 	= SS[1,2]
					local ss_b3 	= SS[1,3]
					local ss_b4 	= SS[1,4]
					local ss_se_b1 	= SS[2,1]
					local ss_se_b2 	= SS[2,2]
					local ss_se_b3 	= SS[2,3]
					local ss_se_b4 	= SS[2,4]
					local N_obs		= `e(N)'
					
					di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
					
					forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "t"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .001 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.001 local stars_b`c' "***"
							else local stars_b`c' ""
					}
					
					di "-2 SD`stars_b1'"
					di "-1 SD`stars_b2'"
					di "1 SD`stars_b3'"
					di "2 SD`stars_b4'"
					
					local ss_b1 = round(`ss_b1',.001)
					local ss_b2 = round(`ss_b2',.001)
					local ss_b3 = round(`ss_b3',.001)
					local ss_b4 = round(`ss_b4',.001)
					local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
					local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)	
					
						if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
						else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
						else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
						else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
						else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
						else local RANGE ""
					
					di "`x' - `RANGE'"
					
						if 		"`x'" == "peer11" local LYNAME "Peer victimization"
						else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
						else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
						else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
						else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
						else local LYNAME ""
					
					di "`x' - `LYNAME'"
					
					** Graph of simple slopes
						
						sum hippl_icc1
						local h1sd = r(sd)
						
						margins, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))							
												
						marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
						ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
						ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
						yscale(range(-.4(.4)1.2)) ylabel(-.4(.4)1.2, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))						///
						ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0", size(medlarge))									///
						title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
						legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
						plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
						plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
						plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
						"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
						"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
						"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
						saving("${results}\Moderation analysis/Plots_`p'_LHV_SEE_CC_covs.gph", replace)
						
						graph export "${results}\Moderation analysis/Plot_`p'_LHV_SEE_CC_covs.png", as(jpg) name("Graph") quality(90) replace	
		
		local i = `i' + 1 
		}	
	
*/

********************************************************************************
*************** Three-way interaction LHV x SEE x sex **************************

global covarsG "age1 white1 puberty_v1 neighbor Trauma_eva1 hist_dprsv12 hist_dprsv13 hist_dprsv14 cohabit1 educ_lev12 educ_lev13 educ_lev14 educ_lev15"

************************
* COMPLETE CASE ANALYSIS
*
*** No covariates
*
	cap erase "${results}/ThreeWayInteraction/TWI_CC_nocovs.xls"
	cap erase "${results}/ThreeWayInteraction/TWI_CC_nocovs.txt"
	
	local depvar d_mddy d_mddy d_mddy d_mddy d_mddy
	local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
	local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
	local pname PV FC CD PS PW

	local i = 1
	local j : word count `indvar'
		while `i' <= `j' {
			
			local y : word `i' of `depvar'
			local x : word `i' of `indvar'
			local r : word `i' of `xrnge'
			local p : word `i' of `pname'
			
			matrix TWIscc1`i' = J(9,1,.)
			
		mixed `y'3 c.`x'##c.hippl_icc1##i.sex1 `y'1 || site_cat: || rel_family_id1:
				quietly summ `y'3 if e(sample)==1
				local Ym = r(mean)
				quietly summ `x' if e(sample)==1
				local Xm = r(mean)		
				local NF = e(N_g)[1,2]
				outreg2 using "${results}/ThreeWayInteraction/TWI_CC_nocovs.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append

					** Plot of sex-related differences
						matrix SDM1 = vecdiag(e(V))
						local DF1 = e(N)-e(df_c)

						matrix TWIscc1`i'[1,1] = e(b)[1,11]
						matrix TWIscc1`i'[2,1] = sqrt(SDM1[1,11])
						matrix TWIscc1`i'[3,1] = `DF1'
						matrix TWIscc1`i'[4,1] = e(b)[1,11]/sqrt(SDM1[1,11])
						matrix TWIscc1`i'[5,1] = 2*ttail(`DF1', abs(e(b)[1,11]/sqrt(SDM1[1,11])))
						matrix TWIscc1`i'[6,1] = e(b)[1,11] - invttail(`DF1',0.025)*sqrt(SDM1[1,11])
						matrix TWIscc1`i'[7,1] = e(b)[1,11] + invttail(`DF1',0.025)*sqrt(SDM1[1,11])
						matrix TWIscc1`i'[8,1] = 0.75 + `i' - 1
						matrix TWIscc1`i'[9,1] = 1	

					*
					** Difference of simple slopes
					forvalues G=0/1{
						sum hippl_icc1
						local h1sd = r(sd)
						
						margins, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'') sex1==`G')
						matrix SS = r(table)
						
						local ss_b1 	= SS[1,1]
						local ss_b2 	= SS[1,2]
						local ss_b3 	= SS[1,3]
						local ss_b4 	= SS[1,4]
						local ss_se_b1 	= SS[2,1]
						local ss_se_b2 	= SS[2,2]
						local ss_se_b3 	= SS[2,3]
						local ss_se_b4 	= SS[2,4]
						local N_obs		= `e(N)'
						
						di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
						
						forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 local stars_b`c' "***"
							else local stars_b`c' ""
						}
						
						di "-2 SD`stars_b1'"
						di "-1 SD`stars_b2'"
						di "1 SD`stars_b3'"
						di "2 SD`stars_b4'"
						
						local ss_b1 = round(`ss_b1',.001)
						local ss_b2 = round(`ss_b2',.001)
						local ss_b3 = round(`ss_b3',.001)
						local ss_b4 = round(`ss_b4',.001)
						local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
						local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)	
						
							if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
							else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
							else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
							else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
							else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
							else local RANGE ""
						
						di "`x' - `RANGE'"
						
							if 		"`x'" == "peer11" local LYNAME "Peer victimization"
							else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
							else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
							else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
							else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
							else local LYNAME ""
						
						di "`x' - `LYNAME'"

							if 		"`G'" == "0" local GNAME "Boys"
							else if	"`G'" == "1" local GNAME "Girls"
							else local GNAME ""
						
						di "`G' - `GNAME'"
						
						** Graph of simple slopes - sex is "`GNAME'"
							
							sum hippl_icc1
							local h1sd = r(sd)
							
							margins, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'') sex1=`G')							
													
							marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
							ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
							ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
							yscale(range(-1.2(.6)2.4)) ylabel(-1.2(.6)2.4, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))					///
							ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0 - `GNAME'", size(medlarge))							///
							title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
							legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
							plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
							plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
							plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
							"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
							"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
							"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
							saving("${results}/ThreeWayInteraction/Plots_`p'_TWI_`GNAME'_SEE_CC_nocovs.gph", replace)
							
							graph export "${results}/ThreeWayInteraction/Plot_`p'_TWI_`GNAME'_SEE_CC_nocovs.png", as(jpg) name("Graph") quality(90) replace	
					}						
		
		local i = `i' + 1 
		}
*/
***************
*** Covariates
*
	cap erase "${results}/ThreeWayInteraction/TWI_LHVsex_CC_covs.xls"
	cap erase "${results}/ThreeWayInteraction/TWI_LHVsex_CC_covs.txt"
	
	** Original operationalization
	local depvar d_mddy d_mddy d_mddy d_mddy d_mddy
	local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
	local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
	local pname PV FC CD PS PW

	local i = 1
	local j : word count `indvar'
		while `i' <= `j' {
			
			local y : word `i' of `depvar'
			local x : word `i' of `indvar'
			local r : word `i' of `xrnge'
			local p : word `i' of `pname'
			
			matrix TWIsCcc1`i' = J(9,1,.)
			
		mixed `y'3 c.`x'##c.hippl_icc1##i.sex1 `y'1 $covarsG || site_cat: || rel_family_id1:
				quietly summ `y'3 if e(sample)==1
				local Ym = r(mean)
				quietly summ `x' if e(sample)==1
				local Xm = r(mean)		
				local NF = e(N_g)[1,2]
				outreg2 using "${results}/ThreeWayInteraction/TWI_LHVsex_CC_covs.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
				
					** Plot of sex-related differences
						matrix SDM1 = vecdiag(e(V))
						local DF1 = e(N)-e(df_c)

						matrix TWIsCcc1`i'[1,1] = e(b)[1,11]
						matrix TWIsCcc1`i'[2,1] = sqrt(SDM1[1,11])
						matrix TWIsCcc1`i'[3,1] = `DF1'
						matrix TWIsCcc1`i'[4,1] = e(b)[1,11]/sqrt(SDM1[1,11])
						matrix TWIsCcc1`i'[5,1] = 2*ttail(`DF1', abs(e(b)[1,11]/sqrt(SDM1[1,11])))
						matrix TWIsCcc1`i'[6,1] = e(b)[1,11] - invttail(`DF1',0.025)*sqrt(SDM1[1,11])
						matrix TWIsCcc1`i'[7,1] = e(b)[1,11] + invttail(`DF1',0.025)*sqrt(SDM1[1,11])
						matrix TWIsCcc1`i'[8,1] = 0.75 + `i' - 1
						matrix TWIsCcc1`i'[9,1] = 1	

					
					** Difference of simple slopes
					forvalues G=0/1{
						sum hippl_icc1
						local h1sd = r(sd)
						
						margins, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'') sex1==`G')
						matrix SS = r(table)
						
						local ss_b1 	= SS[1,1]
						local ss_b2 	= SS[1,2]
						local ss_b3 	= SS[1,3]
						local ss_b4 	= SS[1,4]
						local ss_se_b1 	= SS[2,1]
						local ss_se_b2 	= SS[2,2]
						local ss_se_b3 	= SS[2,3]
						local ss_se_b4 	= SS[2,4]
						local N_obs		= `e(N)'
						
						di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
						
						forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 local stars_b`c' "***"
							else local stars_b`c' ""
						}
						
						di "-2 SD`stars_b1'"
						di "-1 SD`stars_b2'"
						di "1 SD`stars_b3'"
						di "2 SD`stars_b4'"
						
						local ss_b1 = round(`ss_b1',.001)
						local ss_b2 = round(`ss_b2',.001)
						local ss_b3 = round(`ss_b3',.001)
						local ss_b4 = round(`ss_b4',.001)
						local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
						local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)	
						
							if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
							else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
							else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
							else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
							else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
							else local RANGE ""
						
						di "`x' - `RANGE'"
						
							if 		"`x'" == "peer11" local LYNAME "Peer victimization"
							else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
							else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
							else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
							else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
							else local LYNAME ""
						
						di "`x' - `LYNAME'"

							if 		"`G'" == "0" local GNAME "Boys"
							else if	"`G'" == "1" local GNAME "Girls"
							else local GNAME ""
						
						di "`G' - `GNAME'"
						
						** Graph of simple slopes - sex is "`GNAME'"
							
							sum hippl_icc1
							local h1sd = r(sd)
							
							margins, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'') sex1=`G')							
													
							marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
							ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
							ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
							yscale(range(-1.2(.6)2.4)) ylabel(-1.2(.6)2.4, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))					///
							ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0 - `GNAME'", size(medlarge))							///
							title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
							legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
							plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
							plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
							plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
							"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
							"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
							"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
							saving("${results}/ThreeWayInteraction/Plots_`p'_TWI_`GNAME'_SEE_CC_covs_Y1.gph", replace)
							
							graph export "${results}/ThreeWayInteraction/Plot_`p'_TWI_`GNAME'_SEE_CC_covs_Y1.png", as(jpg) name("Graph") quality(90) replace	
					}						
		
		local i = `i' + 1 
		}
	
	
* Three-way interactions in Figures

foreach T in TWIscc1 TWIsCcc1{
	*TWIsmi1 TWIsCmi1
	
	matrix `T' = `T'1,`T'2,`T'3,`T'4,`T'5
	
	matrix TWIci = `T''
	svmat2 TWIci 

	twoway (bar TWIci1 TWIci8) (rcap TWIci6 TWIci7 TWIci8),							///
			xlabel(.75 `" "Peer" "victimization" "' 1.75 `" "Family" "conflict" "' 	///
			2.75 `" "Caregiver`=char(39)'s" "depression" "' 					 	///
			3.75 `" "Prosocial" "school" "' 4.75 `" "Parental" "warmth" "', noticks ///
			labsize(small)) xtitle("") legend(order(1 "Point estimate" 2 "95% CI")) ///
			legend(rows(1) position(6) symy(3) symx(10) forces textw(20) 			///
			size(small)) xsc(outergap(*5)) ylabel(, angle(0))						///
			ytitle("Three-way interaction estimate", size(small)) 					///
			ysc(titlegap(2)) 														///
			saving("${results}/ThreeWayInteraction/RAW_TWI_`T'.gph", replace)
	
	drop TWIci1-TWIci9
}	
	
*/


********************************************************************************
************************ Moderation effects of LHV *****************************
*************** Sensitivity analysis using multiple imputation *****************

******************************
* MULTIPLE IMPUTATION ANALYSIS
*

	tempfile MI_DB
	save `MI_DB'
	set seed 594015206
	
	preserve			
				
		***** Multiple imputation set up

		use `MI_DB', clear
		egen all_missing = rownonmiss(d_mddy3 peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 hippl_icc1 d_mddy1 $covars)
		keep if all_missing!=0
						
		mi set wide
		mi misstable summarize d_mddy3 peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 hippl_icc1 d_mddy1 $covars
		mi register imputed d_mddy3 peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 hippl_icc1 d_mddy1 $covars
		mi impute mvn d_mddy3 peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1 hippl_icc1 d_mddy1 $covars, add(10)

		***** Estimations

		*** No covariates
		
		local depvar d_mddy d_mddy d_mddy d_mddy d_mddy
		local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
		local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
		local pname PV FC CD PS PW
		
		local i = 1
		local j : word count `indvar'
			while `i' <= `j' {
				
				local y : word `i' of `depvar'
				local x : word `i' of `indvar'
				local r : word `i' of `xrnge'
				local p : word `i' of `pname'
					
					matrix SSL`p'1 = J(10,4,.)
					
					matrix Emi`p'b1 	= J(2,8,.)						
						
						***** SEE x Left hemisphere
						
						mi estimate: mixed `y'3 c.`x'##c.hippl_icc1 `y'1 || site_cat: || rel_family_id1:
						matrix Emi`p'a1 = e(b_mi)\vecdiag(e(V_mi))
						matrix SDM = vecdiag(e(V_mi))
						
						forvalues e=1/8{
							matrix Emi`p'b1[1,`e'] = e(b_mi)[1,`e']/sqrt(SDM[1,`e'])
							matrix Emi`p'b1[2,`e'] = 2*ttail(e(df_r_mi), abs(e(b_mi)[1,`e']/sqrt(SDM[1,`e'])))
						}
						
						matrix Emi`p'1 = Emi`p'a1\Emi`p'b1
						
						** Difference of simple slopes
						sum hippl_icc1
						local h1sd = r(sd)
						
						mimrgns, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'')) cmdmargins
						matrix SS = r(table)
						
						local ss_b1 	= SS[1,1]
						local ss_b2 	= SS[1,2]
						local ss_b3 	= SS[1,3]
						local ss_b4 	= SS[1,4]
						local ss_se_b1 	= SS[2,1]
						local ss_se_b2 	= SS[2,2]
						local ss_se_b3 	= SS[2,3]
						local ss_se_b4 	= SS[2,4]
						local N_obs		= `e(N_mi)'
						
						di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
						
						forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "t"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .001 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.001 local stars_b`c' "***"
							else local stars_b`c' ""
						}
						
						di "-2 SD`stars_b1'"
						di "-1 SD`stars_b2'"
						di "1 SD`stars_b3'"
						di "2 SD`stars_b4'"
						
						matrix SSL`p'1[1,1] = `ss_b1'
						matrix SSL`p'1[2,1] = `ss_se_b1'
						matrix SSL`p'1[1,2] = `ss_b2'
						matrix SSL`p'1[2,2] = `ss_se_b2'
						matrix SSL`p'1[1,3] = `ss_b2'-`ss_b1'
						matrix SSL`p'1[2,3] = sqrt((`ss_se_b2')^2 + (`ss_se_b1')^2)
						local pv_diff =  (`ss_b2'-`ss_b1')/sqrt((`ss_se_b2')^2 + (`ss_se_b1')^2)
						matrix SSL`p'1[1,4] = 2*normal(-abs(`pv_diff'))
						
						local ss_b1 = round(`ss_b1',.001)
						local ss_b2 = round(`ss_b2',.001)
						local ss_b3 = round(`ss_b3',.001)
						local ss_b4 = round(`ss_b4',.001)
						local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
						local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)
						
						di `max_ss_b1_b2_b3_b4'
						di `max_interv'
						
								if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
								else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
								else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
								else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
								else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
								else local RANGE ""
							
							di "`x' - `RANGE'"
							
								if 		"`x'" == "peer11" local LYNAME "Peer victimization"
								else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
								else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
								else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
								else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
								else local LYNAME ""
							
							di "`x' - `LYNAME'"
							
							** Graph of simple slopes
								
								sum hippl_icc1
								local h1sd = r(sd)
								
								mimrgns, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'')) cmdmargins						
														
								marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
								ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
								ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
								yscale(range(0(.2)1)) ylabel(0(.2)1, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))								///
								ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0", size(medlarge))									///
								title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
								legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
								plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
								plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
								plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
								"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
								"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
								"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
								saving("${results}\Moderation analysis/Plots_`p'_LHV_SEE_MI_nocovs.gph", replace)
								
								graph export "${results}\Moderation analysis/Plot_`p'_LHV_SEE_MI_nocovs.png", as(jpg) name("Graph") quality(90) replace
					
					matrix NMlH`p'1a = 21\e(N_g)[1,2]\e(N_mi)
					mi estimate: reg `x'
					matrix NMlH`p'1b = e(b_mi)\NMlH`p'1a
					mi estimate: reg `y'3
					matrix NMlH`p'1 = e(b_mi)\NMlH`p'1b
					*/
				local i = `i' + 1 
			}
	
		****************
		*** Covariates
		
		local depvar d_mddy d_mddy d_mddy d_mddy d_mddy
		local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
		local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
		local pname PV FC CD PS PW
		
		local i = 1
		local j : word count `indvar'
			while `i' <= `j' {
				
				local y : word `i' of `depvar'
				local x : word `i' of `indvar'
				local r : word `i' of `xrnge'
				local p : word `i' of `pname'
					
					matrix SSLC`p'1 = J(10,4,.)
					
					matrix EmiC`p'b1 	= J(2,22,.)						
						
						***** SEE x Left hemisphere
						
						mi estimate: mixed `y'3 c.`x'##c.hippl_icc1 `y'1 $covars || site_cat: || rel_family_id1:
						matrix EmiC`p'a1 = e(b_mi)\vecdiag(e(V_mi))
						matrix SDM = vecdiag(e(V_mi))
						
						forvalues e=1/22{
							matrix EmiC`p'b1[1,`e'] = e(b_mi)[1,`e']/sqrt(SDM[1,`e'])
							matrix EmiC`p'b1[2,`e'] = 2*ttail(e(df_r_mi), abs(e(b_mi)[1,`e']/sqrt(SDM[1,`e'])))
						}
						
						matrix EmiC`p'1 = EmiC`p'a1\EmiC`p'b1
						*
						** Difference of simple slopes
						sum hippl_icc1
						local h1sd = r(sd)
						
						mimrgns, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'')) cmdmargins
						matrix SS = r(table)
						
						local ss_b1 	= SS[1,1]
						local ss_b2 	= SS[1,2]
						local ss_b3 	= SS[1,3]
						local ss_b4 	= SS[1,4]
						local ss_se_b1 	= SS[2,1]
						local ss_se_b2 	= SS[2,2]
						local ss_se_b3 	= SS[2,3]
						local ss_se_b4 	= SS[2,4]
						local N_obs		= `e(N_mi)'
						
						di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
						
						forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "t"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .001 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.001 local stars_b`c' "***"
							else local stars_b`c' ""
						}
						
						di "-2 SD`stars_b1'"
						di "-1 SD`stars_b2'"
						di "1 SD`stars_b3'"
						di "2 SD`stars_b4'"
						
						matrix SSLC`p'1[1,1] = `ss_b1'
						matrix SSLC`p'1[2,1] = `ss_se_b1'
						matrix SSLC`p'1[1,2] = `ss_b2'
						matrix SSLC`p'1[2,2] = `ss_se_b2'
						matrix SSLC`p'1[1,3] = `ss_b2'-`ss_b1'
						matrix SSLC`p'1[2,3] = sqrt((`ss_se_b2')^2 + (`ss_se_b1')^2)
						local pv_diff =  (`ss_b2'-`ss_b1')/sqrt((`ss_se_b2')^2 + (`ss_se_b1')^2)
						matrix SSLC`p'1[1,4] = 2*normal(-abs(`pv_diff'))
						
						local ss_b1 = round(`ss_b1',.001)
						local ss_b2 = round(`ss_b2',.001)
						local ss_b3 = round(`ss_b3',.001)
						local ss_b4 = round(`ss_b4',.001)
						local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
						local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)
						
						di `max_ss_b1_b2_b3_b4'
						di `max_interv'
						
								if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
								else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
								else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
								else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
								else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
								else local RANGE ""
							
							di "`x' - `RANGE'"
							
								if 		"`x'" == "peer11" local LYNAME "Peer victimization"
								else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
								else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
								else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
								else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
								else local LYNAME ""
							
							di "`x' - `LYNAME'"
							
							** Graph of simple slopes
								
								sum hippl_icc1
								local h1sd = r(sd)
								
								mimrgns, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd'')) cmdmargins						
														
								marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
								ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
								ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
								yscale(range(0(.2)1)) ylabel(0(.2)1, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))								///
								ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0", size(medlarge))									///
								title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
								legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
								plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
								plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
								plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
								"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
								"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
								"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
								saving("${results}\Moderation analysis/Plots_`p'_LHV_SEE_MI_covs.gph", replace)
								
								graph export "${results}\Moderation analysis/Plot_`p'_LHV_SEE_MI_covs.png", as(jpg) name("Graph") quality(90) replace
					
					matrix NMlH`p'C1a = 21\e(N_g)[1,2]\e(N_mi)
					mi estimate: reg `x'
					matrix NMlH`p'C1b = e(b_mi)\NMlH`p'C1a
					mi estimate: reg `y'3
					matrix NMlH`p'C1 = e(b_mi)\NMlH`p'C1b
				*/
				local i = `i' + 1 
			}
	
	restore
	
***********************
*** Exporting results
	
	matrix EMmi = EmiPV1,EmiFC1,EmiCD1,EmiPS1,EmiPW1
		
	preserve
		svmat EMmi
		keep if EMmi1!=.
		keep EMmi*
	
		export excel using "${results}\Moderation analysis/MLHV_MI_nocovs.xlsx", replace firstr(var)
	restore
	
	matrix SSL = SSLPV1,SSLFC1,SSLCD1,SSLPS1,SSLPW1
	
	preserve
		svmat SSL
		keep if SSL1!=.
		keep SSL*

		export excel using "${results}\Moderation analysis/MLHV_MI_nocovs_SS.xlsx", replace firstr(var)
	restore		
	
	matrix NMlH = NMlHPV1,NMlHFC1,NMlHCD1,NMlHPS1,NMlHPW1
	
	preserve
		svmat NMlH
		keep if NMlH1!=.
		keep NMlH*

		export excel using "${results}\Moderation analysis/MLHV_MI_nocovs_N.xlsx", replace firstr(var)
	restore	
	
	
	matrix EMmiC = EmiCPV1,EmiCFC1,EmiCCD1,EmiCPS1,EmiCPW1
		
	preserve
		svmat EMmiC
		keep if EMmiC1!=.
		keep EMmiC*
	
		export excel using "${results}\Moderation analysis/MLHV_MI_covs.xlsx", replace firstr(var)
	restore
	
	matrix SSLC = SSLCPV1,SSLCFC1,SSLCCD1,SSLCPS1,SSLCPW1
	
	preserve
		svmat SSLC
		keep if SSLC1!=.
		keep SSLC*

		export excel using "${results}\Moderation analysis/MLHV_MI_covs_SS.xlsx", replace firstr(var)
	restore
	
	matrix NMlHC = NMlHPVC1,NMlHFCC1,NMlHCDC1,NMlHPSC1,NMlHPWC1
	
	preserve
		svmat NMlHC
		keep if NMlHC1!=.
		keep NMlHC*

		export excel using "${results}\Moderation analysis/MLHV_MI_covs_N.xlsx", replace firstr(var)
	restore
	


********************************************************************************
************************ Moderation effects of LHV *****************************
********** Sensitivity analysis using square root transformation ***************

************************
* COMPLETE CASE ANALYSIS
*
	cap erase "${results}\Moderation analysis/MLHV_CCsqrt_nocovs.xls"
	cap erase "${results}\Moderation analysis/MLHV_CCsqrt_nocovs.txt"

*** No covariates

	local depvar dsqrt_mddy dsqrt_mddy dsqrt_mddy dsqrt_mddy dsqrt_mddy
	local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
	local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
	local pname PV FC CD PS PW

	local i = 1
	local j : word count `indvar'
		while `i' <= `j' {
			
			local y : word `i' of `depvar'
			local x : word `i' of `indvar'
			local r : word `i' of `xrnge'
			local p : word `i' of `pname'
			
		
		mixed `y'3 c.`x'##c.hippl_icc1 `y'1 || site_cat: || rel_family_id1:
				quietly summ `y'3 if e(sample)==1
				local Ym = r(mean)
				quietly summ `x' if e(sample)==1
				local Xm = r(mean)		
				local NF = e(N_g)[1,2]
				outreg2 using "${results}\Moderation analysis/MLHV_CCsqrt_nocovs.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
				*
					** Difference of simple slopes
					sum hippl_icc1
					local h1sd = r(sd)
					
					margins, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))
					matrix SS = r(table)
					
					local ss_b1 	= SS[1,1]
					local ss_b2 	= SS[1,2]
					local ss_b3 	= SS[1,3]
					local ss_b4 	= SS[1,4]
					local ss_se_b1 	= SS[2,1]
					local ss_se_b2 	= SS[2,2]
					local ss_se_b3 	= SS[2,3]
					local ss_se_b4 	= SS[2,4]
					local N_obs		= `e(N)'
					
					di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
					
					forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "t"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .001 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.001 local stars_b`c' "***"
							else local stars_b`c' ""
					}
					
					di "-2 SD`stars_b1'"
					di "-1 SD`stars_b2'"
					di "1 SD`stars_b3'"
					di "2 SD`stars_b4'"
					
					local ss_b1 = round(`ss_b1',.001)
					local ss_b2 = round(`ss_b2',.001)
					local ss_b3 = round(`ss_b3',.001)
					local ss_b4 = round(`ss_b4',.001)
					local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
					local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)	
					
						if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
						else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
						else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
						else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
						else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
						else local RANGE ""
					
					di "`x' - `RANGE'"
					
						if 		"`x'" == "peer11" local LYNAME "Peer victimization"
						else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
						else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
						else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
						else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
						else local LYNAME ""
					
					di "`x' - `LYNAME'"
					
					** Graph of simple slopes
						
						sum hippl_icc1
						local h1sd = r(sd)
						
						margins, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))							
												
						marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
						ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
						ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
						yscale(range(0(.2)1)) ylabel(0(.2)1, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))								///
						ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0", size(medlarge))									///
						title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
						legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
						plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
						plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
						plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
						"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
						"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
						"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
						saving("${results}\Moderation analysis/Plots_`p'_LHV_SEE_CCsqrt_nocovs.gph", replace)
						
						graph export "${results}\Moderation analysis/Plot_`p'_LHV_SEE_CCsqrt_nocovs.png", as(jpg) name("Graph") quality(90) replace	
		
		local i = `i' + 1 
		}
	
***************
*** Covariates

	** Original operationalization
	cap erase "${results}\Moderation analysis/MLHV_CCsqrt_covs.xls"
	cap erase "${results}\Moderation analysis/MLHV_CCsqrt_covs.txt"

	local depvar dsqrt_mddy dsqrt_mddy dsqrt_mddy dsqrt_mddy dsqrt_mddy
	local indvar peer11 fam_confav1 asr_scr_depress_t1 school_positive1 pcwarmth1
	local xrnge 0(1)2 0(0.25)1 5(1)10 1(1)4 1(1)3
	local pname PV FC CD PS PW

	local i = 1
	local j : word count `indvar'
		while `i' <= `j' {
			
			local y : word `i' of `depvar'
			local x : word `i' of `indvar'
			local r : word `i' of `xrnge'
			local p : word `i' of `pname'
			
		
		mixed `y'3 c.`x'##c.hippl_icc1 `y'1 $covars || site_cat: || rel_family_id1:
				quietly summ `y'3 if e(sample)==1
				local Ym = r(mean)
				quietly summ `x' if e(sample)==1
				local Xm = r(mean)		
				local NF = e(N_g)[1,2]
				outreg2 using "${results}\Moderation analysis/MLHV_CCsqrt_covs.xls", addstat(YMean, `Ym', XMean, `Xm', NFamilies, `NF') dec(3) label nocons alpha(0.001, 0.01, 0.05, 0.1) symbol(***,**,*,t) append
				*
					** Difference of simple slopes
					sum hippl_icc1
					local h1sd = r(sd)
					
					margins, dydx(`x') at(hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))
					matrix SS = r(table)
					
					local ss_b1 	= SS[1,1]
					local ss_b2 	= SS[1,2]
					local ss_b3 	= SS[1,3]
					local ss_b4 	= SS[1,4]
					local ss_se_b1 	= SS[2,1]
					local ss_se_b2 	= SS[2,2]
					local ss_se_b3 	= SS[2,3]
					local ss_se_b4 	= SS[2,4]
					local N_obs		= `e(N)'
					
					di 2*ttail(`N_obs', abs(`=`ss_b1'/`ss_se_b1''))
					
					forvalues c=1/4{
							if 		`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .1 local stars_b`c' ""
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .1 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .05 local stars_b`c' "t"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= .05 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .01 local stars_b`c' "*"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.01 & `=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' > .001 local stars_b`c' "**"
							else if	`=2*ttail(`N_obs', abs(`=`ss_b`c''/`ss_se_b`c'''))' <= 0.001 local stars_b`c' "***"
							else local stars_b`c' ""
					}
					
					di "-2 SD`stars_b1'"
					di "-1 SD`stars_b2'"
					di "1 SD`stars_b3'"
					di "2 SD`stars_b4'"
					
					local ss_b1 = round(`ss_b1',.001)
					local ss_b2 = round(`ss_b2',.001)
					local ss_b3 = round(`ss_b3',.001)
					local ss_b4 = round(`ss_b4',.001)
					local max_ss_b1_b2_b3_b4 = max(`ss_b1', `ss_b2',`ss_b3',`ss_b4')
					local max_interv = round(`max_ss_b1_b2_b3_b4'/8,.01)	
					
						if 		"`x'" == "peer11" local RANGE "0 0.25 0.50 0.75 1.00 1.25 1.50 1.75 2.00"
						else if	"`x'" == "fam_confav1" local RANGE "0 0.20 0.40 0.60 0.80 1.00"
						else if	"`x'" == "asr_scr_depress_t1" local RANGE "5.00 5.25 5.50 5.75 6.00 6.25 6.50 6.75 7.00 7.25 7.50 7.75 8.00 8.25 8.50 8.75 9.00 9.25 9.50 9.75 10.00"
						else if	"`x'" == "school_positive1" local RANGE "1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00"
						else if	"`x'" == "pcwarmth1" local RANGE "1 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00"
						else local RANGE ""
					
					di "`x' - `RANGE'"
					
						if 		"`x'" == "peer11" local LYNAME "Peer victimization"
						else if	"`x'" == "fam_confav1" local LYNAME "Family conflict"
						else if	"`x'" == "asr_scr_depress_t1" local LYNAME "Caregiver depressive symptoms"
						else if	"`x'" == "school_positive1" local LYNAME "School prosociality"
						else if	"`x'" == "pcwarmth1" local LYNAME "Parental warmth"
						else local LYNAME ""
					
					di "`x' - `LYNAME'"
					
					** Graph of simple slopes
						
						sum hippl_icc1
						local h1sd = r(sd)
						
						margins, at(`x'=(`RANGE') hippl_icc1=(`=-2*`h1sd'' `=-1*`h1sd'' `=1*`h1sd'' `=2*`h1sd''))							
												
						marginsplot, x(`x') graphregion(color(white)) recastci(rarea) 																///
						ci1opts(lcolor(gs16) lpattern("#") color(cranberry%25)) ci2opts(lcolor(gs16) lpattern("#") color(cranberry%12))				///
						ci3opts(lcolor(gs16) lpattern("#") color(navy%12)) ci4opts(lcolor(gs16) lpattern("#") color(navy%25))						///
						yscale(range(0(.2)1)) ylabel(0(.2)1, labgap(*1) labsize(medlarge) angle(0))	plotr(lcolor(none))								///
						ytitle("Change in MDD symptoms", size(medlarge)) xtitle("`LYNAME' at T0", size(medlarge))									///
						title("") xscale(range(`r')) xlabel(`r', labsize(medlarge))																	///
						legend(cols(1) rows(2) position(6) region(lstyle(none)) size(medlarge))														///
						plot1opts(lcolor(cranberry) lpattern("l") lwidth(thick)) plot2opts(lcolor(cranberry%50) lpattern("-") lwidth(thick))		///
						plot3opts(lcolor(navy%50) lpattern("-") lwidth(thick)) plot4opts(lcolor(navy) lpattern("l") lwidth(thick))					///
						plot(, label("-2SD Left HV (`ss_b1'{superscript:`stars_b1'})"  																///
						"-1SD Left HV (`ss_b2'{superscript:`stars_b2'})" 																			///
						"+1SD Left HV (`ss_b3'{superscript:`stars_b3'})"																			///
						"+2SD Left HV (`ss_b4'{superscript:`stars_b4'})", labsize(medlarge))) plotopts(msymbol(i)) 									///
						saving("${results}\Moderation analysis/Plots_`p'_LHV_SEE_CCsqrt_covs.gph", replace)
						
						graph export "${results}\Moderation analysis/Plot_`p'_LHV_SEE_CCsqrt_covs.png", as(jpg) name("Graph") quality(90) replace	
		
		local i = `i' + 1 
		}	
	

*/
