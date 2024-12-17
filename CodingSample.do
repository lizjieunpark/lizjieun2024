********************************************************************************		

* This script file is a sample version for the submission purpose only,
*therefore is an 'excerpt' of full coding script for a research project.
* Data is assumed not to contain full sample and is different from what is 
*used for actual analysis. 

* The research project is to analyze impact of labor market information on 
*married individual's decision to allocate time using instrumental variable 
*approach. It attempts to explore other mechanisms addressing individual's 
*response to stereotypes, here specifically through leisure synchronization.

********************************************************************************		
		
			
	* Initialization and load packages		
	clear all
	version 18 
	macro drop _all 	
	set more off
	set varabbrev off
	set logtype text 
	set linesize 80
	ssc install reghdfe, replace 

					
**********************************
***** 	DATA PROCESSING   	******
**********************************
	cap log close
	log using 0_1data_ind, replace text
						
		use uktus15_individual, clear 
			sort serial 
			
	* data review 			
		describe 
		summarize 
		
		* restrict to fully productive responses
			keep if DMFlag ==1 | DMFlag==2
			
		* survey setting
			svyset psu [pweigh=ind_wt], strata(strata)		
		
		* drop missing for demographics 		
			keep if dmarsta>0 | dmarsta==2 // for cohabitating couples 
			keep if dhiqual>0 // drop 'no answer / don't know
			keep if dsic>0 	// drop 'no answer / don't know, not applicable
			keep if !missing(dagegrp)
							
		* get unique ids for each obs (each ind within hh)						
			tostring serial pnum, gen(serial_str pnum_str)			
			gen str5 zero_id = "00"			
			gen casenop = serial_str + zero_id + pnum_str
				describe casenop 
				list serial pnum casenop in 1/20, clean 
							
		* age var recoding 		
			tab DVAge, miss 
			tab dagegrp, miss 
			drop dagegrp 			
			
			recode DVAge (15/19=2) (20/24=3) (25/29=4) (30/34=5) ///
						 (35/39=6) (40/44=7) (45/49=8) (50/54=9) (55/59=10) /// 
						 (60/64=11) (65/69=12) (70/99=13), gen(dagegrp)			
			cap label drop dagegrp
			label def dagegrp 2 "15-19" 3 "20-24" 4 "25-29" 5 "30-34" 6 "35-39" 7 "40-44" 8 "45-49" 9 "50-54" 10 "55-59" 11 "60-64" 12 "65-69" 13 "70+"
			label val dagegrp dagegrp 
			
			recode DVAge (15/17=1) (18/20=2) (21/23=3) (24/26=4) ///
						 (27/29=5) (30/32=6) (33/35=7) (36/38=8) ///
						 (39/41=9) (42/44=10) (45/47=11) (48/50=12) ///
						 (51/53=13) (54/56=14) (57/59=15) ///
						 (60/62=16) (63/65=17) (66/68=18) ///
						 (69/71=19) (72/74=20) (75/99=21) ///
						 , gen(age2) 					 	
			cap label drop age2
			label def age2 1 "15-17" 2 "18-20" 3 "21-23" 4 "24-26" 5 "27-29" 6 "30-32" 7 "33-35" 8 "36-38" 9 "39-41" 10 "42-44" 11 "45-47" 12 "48-50" 13 "51-53" 14 "54-56" 15 "57-59" 16 "60-62" 17 "63-65" 18 "66-68" 19 "69-71" 20 "72-74" 21 "75+"
			label val age2 age2
			tab age2, miss
	
		* Satisfaction subsection recoding			
			label def Sat 0 "0_NA"
		foreach var in SatLeis SatInc SatPart SatJob SatSoc {
			recode `var' (-9 -8 -1 = 0) 
			label val `var' Sat
			tab `var', miss 
		}
			
		* individual's total income recoding	
			recode 	NetWkly (-1=0)
				tab NetWkly, missing
			recode 	OJWkly (-9 -1 =0)
				tab OJWkly, missing 
			recode 	SENetPay (-9 -8 -1 = 0)		
				tab SENetPay, missing 			
				
			* recalculate earnings for consistent unit with lfs			
				replace NetWkly = (NetWkly / 30 ) * 7
				replace OJWkly 	= (OJWkly / 30 ) * 7 
				rename NetWkly netwkly 
				rename OJWkly net2wkly 						
			egen netinc = rowtotal(netwkly net2wkly)
				tab 	netinc, miss
				replace netinc = netinc+1
				gen 	lninc = log(netinc)

					
	* num children comparison to NumChild 
			capture drop NumChild_new NumChild2 
			egen 		NumChild_new = rowtotal(DM016 DM1619)
				tab 	NumChild_new, miss			
				label var NumChild_new "Number of Children aged <19, calculated manually"
	
		* identifying partner number
			// Relate* = 1 Spouse, =2 civil partner, =3 partner				
			gen partnerno = 0
				forvalues i=1/10 {
					replace partnerno=`i' if Relate`i' == 1 | Relate`i' == 3
				}
					tab partnerno, miss					
			tab partnerno pnum if partnerno>0, miss 			
	
		* get year 		
			gen year = 2014
	
		* keep relevant vars only 		
			keep serial strata psu pnum year ind_wt DMSex DVAge dsic netinc lninc NumChild_new partnerno dukcntr dhiqual dagegrp dmarsta Sat* SprtWee* SprtNum* DomWork-OthSport Volunt net2wkly netwkly OJNetPay SENetPay NetPay MarStat casenop 
			
		sort serial pnum 
			label data "cleaned uktus2015 individual interview (svyset, restriction, couple)"
		compress 
		datasignature set, reset 
		save 0_1clean_tus_i, replace 
		
log close 
	
	
************************************
****** 		FINAL MERGING	  ******
************************************		
capture log close  
log using 0_1data_merge_both , replace text
	
		
		* merge uktus set and lfs set
			
			* final check and recasting types of id variables			
				use 0_1_1merge_uktus, clear 
					describe casenop				// str11 
					describe serial 				// long (numeric)
						tostring serial, replace 	// now str8
						describe serial
					describe pnum 					// byte (numeric)			
				isid casenop 
				duplicates report casenop serial pnum 
				describe					
				datasignature set, reset 
				save 0_1_1merge_uktus, replace 
							
				use 0_1_2merge_lfs, clear 
				datasignature confirm 	
						describe casenop serial pnum	// str15, str13, byte						
					isid casenop 
					duplicates report casenop serial pnum 									
					describe 
							
			* merge 		
				merge 1:1 casenop using 0_1_1merge_uktus, nogen	
					describe 				
					isid casenop
					duplicates report casenop serial pnum 				
					codebook serial casenop pnum 
					
				* keep relevant vars only			
				keep serial pnum casenop source ///
					female age educ age2 educ2 region industry year ///
					coresid anychildren NumChild_new partnerno ///
					marsta DVAge ///
					netinc lninc netwkly net2wkly ///
					pw upw leis pwratio upwratio leisratio /// 
					socent sports artshobs volun rest media /// 
					socentratio sportsratio artshobsratio volunratio ///
					restratio mediaratio ///
					Volunt DomWork Cinema Theatre Museum Librar HistSite SportEv /// 
					EatDrink EntHome ///
					LeisNum* LeisWee* /// 
					Swim KeepFit Cycling TeamGame Racquet Golf Walking Running OthSport /// 
					SprtNum* SprtWee2* ///
					Satis SatInc SatLeis SatPart SatJob ///
					DMFlag DayNum_DiaryDay DiaryDay_Act DiaryType ///
					PIWT PWT strata psu ind_wt dia_wt_* ///
					daynum ddayw ndays
						 
						 
			* tag consistent labels for main variables' values 								
				label def industry 1 "Agriculture & fishing" 2 "Energy & water" 3 "Manufacturing" 4 "Construction" 5 " Distribution, hotels & restaurants" 6 "Transport & communication" 7 "Banking, finance & insurance etc" 8 "Public admin, educ & health" 9 "Other services" 10 "Workplace outside UK"
				label val industry industry
					tab industry, miss 				
				foreach var in source female anychildren coresid partnerno educ region {
					tab `var', miss
				}
					label var educ "Completed Higher Educ?"			
				foreach var in year female anychildren coresid partnerno educ region {					
					tab `var' source, miss 
				} 							
				drop if missing(female)		
			describe 
						
************************************
****** 		DATA PREP		  ******
************************************			
capture log close 
log using 0_2data_prep, replace

			use 0_1_3merge_both, clear 
		
		
			* get couple indexing number			
				tab partnerno source, miss	
				tab partnerno pnum, miss 
			gen coupleno = 0 			
				replace coupleno = 1 if pnum == 1 & partnerno==2
				replace coupleno = 2 if pnum == 1 & partnerno==3
				replace coupleno = 3 if pnum == 1 & partnerno==4
				replace coupleno = 4 if pnum == 1 & partnerno==5
				replace coupleno = 5 if pnum == 1 & partnerno==6
				replace coupleno = 6 if pnum == 1 & partnerno==7			
				replace coupleno = 1 if pnum == 2 & partnerno==1 
				replace coupleno = 7 if pnum == 2 & partnerno==3
				replace coupleno = 8 if pnum == 2 & partnerno==4
				replace coupleno = 9 if pnum == 2 & partnerno==6				
				replace coupleno = 2 if pnum == 3 & partnerno==1
				replace coupleno = 7 if pnum == 3 & partnerno==2
				replace coupleno = 10 if pnum == 3 & partnerno==4
				replace coupleno = 11 if pnum == 3 & partnerno==5
				replace coupleno = 12 if pnum == 3 & partnerno==6
				replace coupleno = 13 if pnum == 3 & partnerno==7
				replace coupleno = 14 if pnum == 3 & partnerno==8				
				replace coupleno = 3 if pnum == 4 & partnerno==1
				replace coupleno = 8 if pnum == 4 & partnerno==2
				replace coupleno = 10 if pnum == 4 & partnerno==3
				replace coupleno = 15 if pnum == 4 & partnerno==5
				replace coupleno = 16 if pnum == 4 & partnerno==6
				replace coupleno = 17 if pnum == 4 & partnerno==8 	
				replace coupleno = 4 if pnum == 5 & partnerno == 1
				replace coupleno = 11 if pnum == 5 & partnerno == 3
				replace coupleno = 15 if pnum == 5 & partnerno == 4
				replace coupleno = 18 if pnum == 5 & partnerno == 6 
				replace coupleno = 19 if pnum == 5 & partnerno == 7
				replace coupleno = 20 if pnum == 5 & partnerno == 8 
				replace coupleno = 21 if pnum == 5 & partnerno == 9 			
				replace coupleno = 5 if pnum == 6 & partnerno == 1
				replace coupleno = 9 if pnum == 6 & partnerno == 2
				replace coupleno = 12 if pnum == 6 & partnerno == 3
				replace coupleno = 16 if pnum == 6 & partnerno == 4
				replace coupleno = 18 if pnum == 6 & partnerno == 5
				replace coupleno = 22 if pnum == 6 & partnerno == 7 			
				replace coupleno = 6 if pnum == 7 & partnerno == 1
				replace coupleno = 13 if pnum == 7 & partnerno == 3
				replace coupleno = 19 if pnum == 7 & partnerno == 5
				replace coupleno = 22 if pnum == 7 & partnerno == 6
				replace coupleno = 23 if pnum == 7 & partnerno == 8			
				replace coupleno = 14 if pnum == 8 & partnerno == 3
				replace coupleno = 17 if pnum == 8 & partnerno == 4 
				replace coupleno = 20 if pnum == 8 & partnerno == 5 
				replace coupleno = 23 if pnum == 8 & partnerno == 7 
				replace coupleno = 24 if pnum == 8 & partnerno == 9 			
				replace coupleno = 21 if pnum == 9 & partnerno == 5 
				replace coupleno = 24 if pnum == 9 & partnerno == 8 
				replace coupleno = 25 if pnum == 9 & partnerno == 10 			
				replace coupleno = 25 if pnum == 10 & partnerno == 9 					
			tab coupleno, miss
				label var coupleno "Index of couple"
				tab coupleno source, miss
			
		
			* get couple's total income 		
				bysort serial coupleno: egen coupleinc = total(netinc) 
					tab  coupleinc, miss
					label var coupleinc "total earnings from both partners"		
				bysort serial coupleno: egen lncoupleinc = total(lninc) 
					label var lncoupleinc "log total earnings from both partners"
					notes lncoupleinc : total(lninc) 
				
				
			* get individual's relative income within couple 			
					bysort serial pnum: gen relinc = netinc / coupleinc 
						codebook relinc 
						label var relinc "share of earnings relative to couple's total"
						count if !missing(relinc) 
							count if coupleinc !=0 //26283 
							tab relinc coupleinc if relinc==. & coupleinc==0, miss 												
					gen lnrelinc = lninc / lncoupleinc 
						codebook lnrelinc 
						label var lnrelinc "share of ln(earnings) relative to couple's total log earnings" 
						notes lnrelinc : lnrelinc = log(netinc) / log(coupleinc)
						count if !missing(lnrelinc)
						count if lnrelinc !=0 
						tab lnrelinc lncoupleinc if lnrelinc==. & lncoupleinc==0, miss						
					gen lnrelinc2 = log(relinc)
					label var lnrelinc2 "ln(relative income)"
					notes lnrelinc2 : lnrelinc2 = log(relinc), relinc=netinc/coupleinc 
			
						
			* get couple's total time spent on main cats 					
				foreach var in pw upw leis socent sports artshobs volun rest media {					
					bysort serial coupleno: egen couple`var' = total(`var')
						codebook couple`var' 
						label var couple`var' "total time spent on `var' category from both partners"					
				}							
				foreach var in pw upw leis socent sports artshobs volun rest media {
							tab couple`var' if couple`var'>2880
							
					* get individual's relative time spent, for couples 				
					bysort serial pnum: gen rel`var' = `var'/couple`var'
						codebook rel`var'
						label var rel`var' "share of time spent on `var' category relative to couple's total"
						tab rel`var' source if missing(rel`var'), miss										
					}		
				sort casenop 
	
		
************************************
****** 		DESCRIPTIVE		  ******
************************************		

	use 0_2ready, clear 
		datasignature confirm 					
		describe 
		order serial pnum casenop source partnerno coupleno coresid female age educ region industry anychildren netinc lninc pw upw leis socent sports artshobs volun rest media *ratio coupleinc relinc lnrelinc couple* rel*
	
	
		* marriage market, local labor market, and ratio calculation group vars		 
				 egen mmarket = group( age 	educ  region 		 year )
				 egen lmarket = group( age 	educ 		industry year )
				 egen ratiogrp = group(		educ  region industry year )
				 egen demogrp = group( age  educ  region) 
					label var mmarket 	"MarriageMarket" 
					label var lmarket 	"LaborMarket" 
					label var ratiogrp 	"PopulationProportionOfIndustry"
					label var demogrp 	"DemographicGroup"
					note mmarket  : by {age educ region year} 
					note lmarket  : by {age educ industry year}
					note ratiogrp : by {educ region industry year} 
					note demogrp  : by {age educ region}
					describe mmarket lmarket ratiogrp demogrp
					summarize mmarket lmarket ratiogrp demogrp					
		 
			
		* assortativeness in matching 				
			tab coupleno , miss			
			frame change default 
				cap frame drop partner
				frame copy default partner		
			frame partner {																
				keep if coupleno>0
				drop if missing(educ)
				drop if missing(industry)					
					
			* get indicator dummy whether partner's info is still available after filtering					
				bysort serial coupleno: /// 
					gen withpartner = cond(_N>1, 1, 0)
						label var withpartner "Partner's Info after filtering"
						label def withpartner 0 "0_No" 1 "1_Yes"
						label val withpartner withpartner							
						tab withpartner, miss
						tab withpartner source, miss 	// some still available 
										
			* get partner's info (age, educ, indstry)				
				foreach var in age educ industry age2 educ2 {
					bysort serial coupleno: /// 
						egen total`var' = total(`var')							
					gen partner`var' = .
						replace partner`var' = total`var' - `var'
						replace partner`var' = . if partner`var'==0			
					tab total`var', miss
					tab partner`var', miss										
					}			
			} 		
			
			
			* Tables - matching patterns by educ, age, industry		
		frame change partner 				
			cap drop matrix T			
			tab educ partnereduc, miss row nofreq matcell(T_educ) matrow(row_educ) matcol(col_educ)			
				matrix list T_educ 
				matrix define one = J(colsof(T_educ), 1, 1)
				matrix rowtot = T_educ * one 
				matrix list rowtot 
				matrix rowpct_educ = inv(diag(rowtot)) * T_educ 
				matrix list rowpct_educ	
			frmttable using assortativeness , statmat(rowpct_educ) colwidth(60) ///
			pretext("Table 1: Marriage Markets - Fraction of Women Marrying Men in Particular Age, Education and Industry Groups, from 2008 to 2014")  /// 
				title("Education Group") ctitle("", "Husband's Education" \"Wife's Education", "High School or Less", "Some College or More") rtitle( "High School or Less"\ "Some College or More") replace 			
									
			tab age partnerage if 2<age & 2< partnerage & age<10 & partnerage<10, miss row nofreq matcell(T_age) matrow(row_age) matcol(col_age)				
				matrix list T_age 
				matrix define one = J(colsof(T_age), 1, 1) 
				matrix rowtot = T_age * one 
				matrix list rowtot 
				matrix rowpct_age = inv(diag(rowtot)) * T_age
				matrix list rowpct_age 
			frmttable using assortativeness , statmat(rowpct_age) colwidth(60) ///
				title("Age Group") ctitle("", "Husband's Age" \ "Wife's Age", "20-24", "25-29" ,"30-34" ,"35-39" ,"40-44" ,"45-49" , "50-54") rtitle("20-24" \"25-29" \"30-34" \"35-39" \"40-44" \"45-49" \ "50-54" ) addtable
			
			tab educ2 partnereduc2,  row nofreq matcell(T_educ2) matrow(row_educ2) matcol(col_educ2)		
				matrix list T_educ2 
				matrix define one = J(colsof(T_educ2), 1, 1)
				matrix rowtot = T_educ2 * one 
				matrix list rowtot 
				matrix rowpct_educ2 = inv(diag(rowtot)) * T_educ2 
				matrix list rowpct_educ2			
			frmttable using assortativeness , statmat(rowpct_educ2) colwidth(60) ///
			pretext("Table 1: Marriage Markets - Fraction of Women Marrying Men in Particular Age, Education and Industry Groups, from 2008 to 2014")  /// 
				title("Detail Education Group") ctitle("", "Husband's Education" \"Wife's Education", "Less than High School", "High School" "College" "College or More") rtitle( "Less than High School"\ "High School" \ "College" \ "College or More") addtable 
								
			tab age2 partnerage2 if 2<age2 & 2< partnerage2 & age2<15 & partnerage2<15, miss row nofreq matcell(T_age2) matrow(row_age2) matcol(col_age2)
				matrix list T_age2 
				matrix define one = J(colsof(T_age2), 1, 1) 
				matrix rowtot = T_age2 * one 
				matrix list rowtot 
				matrix rowpct_age2 = inv(diag(rowtot)) * T_age2
				matrix list rowpct_age2 		
			frmttable using assortativeness , statmat(rowpct_age2) colwidth(60) ///
				title("Detail Age Group") ctitle("", "Husband's Age" \ "Wife's Age", "21-23" , "24-26" 5 "27-29" , "30-32" , "33-35" , "36-38" , "39-41" , "42-44" , "45-47" , "48-50" , "51-53" , "54-56") rtitle("21-23" \ "24-26" \ "27-29" \ "30-32" \ "33-35" \ "36-38" \ "39-41" \ "42-44" \ "45-47" \ "48-50" \ "51-53" \ "54-56" ) addtable
						
			compress
			label data "merged dataset with mmarket, lmarket, ratio included"
			datasignature set, reset 
			save 1_1clean_market, replace			
		frame change default			


*********************************
****** 	  FIRST STAGE	   ******
*********************************
	 use 1_2leistype, clear
		datasignature confirm 
		
		* income by vingintiles (n_p=20)		
			* actual income part (rhs)
		preserve
			keep if year==2014	
			collapse 	(mean) mean_l=netinc (p5) p5_l=netinc ///
						(p10) p10_l=netinc (p15) p15_l=netinc ///
						(p20) p20_l=netinc (p25) p25_l=netinc ///
						(p30) p30_l=netinc (p35) p35_l=netinc ///
						(p40) p40_l=netinc (p45) p45_l=netinc ///
						(p50) p50_l=netinc (p55) p55_l=netinc ///
						(p60) p60_l=netinc (p65) p65_l=netinc ///
						(p70) p70_l=netinc (p75) p75_l=netinc ///
						(p80) p80_l=netinc (p85) p85_l=netinc ///
						(p90) p90_l=netinc (p95) p95_l=netinc, ///
							by(lmarket female)					 
			tempfile incq_l
			save `incq_l'		 
		restore 		
		preserve 		
		keep if year==2014		
			collapse 	(mean) lnmean_l=lninc (p5) lnp5_l=lninc ///
						(p10) lnp10_l=lninc (p15) lnp15_l=lninc ///
						(p20) lnp20_l=lninc (p25) lnp25_l=lninc ///
						(p30) lnp30_l=lninc (p35) lnp35_l=lninc ///
						(p40) lnp40_l=lninc (p45) lnp45_l=lninc ///
						(p50) lnp50_l=lninc (p55) lnp55_l=lninc ///
						(p60) lnp60_l=lninc (p65) lnp65_l=lninc ///
						(p70) lnp70_l=lninc (p75) lnp75_l=lninc ///
						(p80) lnp80_l=lninc (p85) lnp85_l=lninc ///
						(p90) lnp90_l=lninc (p95) lnp95_l=lninc, ///
							by(lmarket female)		 
			tempfile lnincq_l
			save `lnincq_l'	
		restore 
			
			* predicted income part (lhs)	
		preserve 
			keep if  year==2014
			collapse 	(mean) mean_m=netinc (p5) p5_m=netinc ///
						(p10) p10_m=netinc (p15) p15_m=netinc ///
						(p20) p20_m=netinc (p25) p25_m=netinc ///
						(p30) p30_m=netinc (p35) p35_m=netinc ///
						(p40) p40_m=netinc (p45) p45_m=netinc ///
						(p50) p50_m=netinc (p55) p55_m=netinc ///
						(p60) p60_m=netinc (p65) p65_m=netinc ///
						(p70) p70_m=netinc (p75) p75_m=netinc ///
						(p80) p80_m=netinc (p85) p85_m=netinc ///
						(p90) p90_m=netinc (p95) p95_m=netinc, ///
							by(mmarket female) 
			tempfile incq_m
			save `incq_m'		
		restore 		
		preserve 
			keep if  year==2014
			collapse 	(mean) lnmean_m=lninc (p5) lnp5_m=lninc ///
						(p10) lnp10_m=lninc (p15) lnp15_m=lninc ///
						(p20) lnp20_m=lninc (p25) lnp25_m=lninc ///
						(p30) lnp30_m=lninc (p35) lnp35_m=lninc ///
						(p40) lnp40_m=lninc (p45) lnp45_m=lninc ///
						(p50) lnp50_m=lninc (p55) lnp55_m=lninc ///
						(p60) lnp60_m=lninc (p65) lnp65_m=lninc ///
						(p70) lnp70_m=lninc (p75) lnp75_m=lninc ///
						(p80) lnp80_m=lninc (p85) lnp85_m=lninc ///
						(p90) lnp90_m=lninc (p95) lnp95_m=lninc, ///
							by(mmarket female)	 
			tempfile lnincq_m
			save `lnincq_m'	
		restore 
		
		* get proportion of individuals by gender of each industry 				 
		 preserve 			
			keep if year == 2008 	// base year = 2008		 			
			bysort ratiogrp female: egen frac 		= count(casenop)
			bysort industry female: egen totN 		= count(casenop)
			bysort casenop: 		gen ratioTot 	= frac/totN 						
				collapse (mean) ratioJ=ratioTot, by(industry female)
		tempfile ratio 
		save `ratio'					
		restore 				
		merge m:1 lmarket female  using `incq_l'	, nogen update
		merge m:1 lmarket female  using `lnincq_l'	, nogen update
		merge m:1 mmarket female  using `incq_m'	, nogen update
		merge m:1 mmarket female  using `lnincq_m'	, nogen update
		merge m:1 industry female using `ratio'		, nogen update 
		
		
		* first stage		
		cap eststo clear

			* moment: mean		
			label var mean_m "Mean"
			label var ratioJ "Fraction"
			label var mean_l "Mean"
			label var mean_m "Mean"
			label var lnmean_l "Mean"
			label var lnmean_m "Man"		
			forvalues p=5(5)95 {				 
				 label var p`p'_m "`p'th"
				 label var p`p'_l "`p'th"
				 label var lnp`p'_m "`p'th"
				 label var lnp`p'_l "`p'th"
			}		
			tostring coupleno, gen(coupleno_str)
			gen str5 zero_id = "000"
			gen casecoupleno = serial + zero_id + coupleno_str 
			desc casecoupleno 
			list serial coupleno pnum casenop casecoupleno in 1/20, clean
			drop zero_id coupleno_str
			
			preserve 
				keep if female ==1 
				eststo mean1: 	 reg mean_m c.ratioJ#c.mean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region) 				
				eststo mean1ln : reg lnmean_m c.ratioJ#c.lnmean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region)		
				eststo mean1_2:  reg mean_m c.ratioJ#c.mean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust casecoupleno) 			
				eststo mean1ln2: reg lnmean_m c.ratioJ#c.lnmean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust casecoupleno)
			restore 					
			preserve			
				keep if female == 0 				
				eststo mean0: 	 reg mean_m c.ratioJ#c.mean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region) 		
				eststo mean0ln:  reg lnmean_m c.ratioJ#c.lnmean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region) 				
				eststo mean0_2:  reg mean_m c.ratioJ#c.mean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust casecoupleno)				
				eststo mean0ln2: reg lnmean_m c.ratioJ#c.lnmean_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust casecoupleno) 				
			restore 
			
			
			* moment: percentiles			
			preserve 				
				keep if female==1
				local q = 10
				forvalues v=10(20)90 {	
					display _newline _newline "< Quantile is p`v'; est`q' >"					
					eststo est`q': reg p`v'_m c.ratioJ#c.p`v'_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region) 					
					eststo est`q'ln: reg lnp`v'_m c.ratioJ#c.lnp`v'_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region)			
				local ++q
				}					
			restore 				
			preserve			
				keep if female==0				
				local q = 20
				forvalues v=10(20)90 {					
					display _newline _newline "< Quantile is p`v'; est`q' >"				
					eststo est`q': reg p`v'_m c.ratioJ#c.p`v'_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region) 					
					eststo est`q'ln: reg lnp`v'_m c.ratioJ#c.lnp`v'_l mmarket educ2 region year i.year#i.educ2 i.year#i.age2, vce(clust region) 
					local ++q				
				}
			restore 
			
			
	* tables 	
	esttab mean1* est2* using firststage.csv, b se ar2 eform ///
			nocons obslast sfmt(fmt(3)) ///
			keep(c.ratioJ#c.mean_l c.ratioJ#c.p10_l c.ratioJ#c.p30_l c.ratioJ#c.p50_l c.ratioJ#c.p70_l c.ratioJ#c.p90_l) /// 
			interaction("x") label nonotes ///
			prehead("Dependent variable: Selected Percentiles of Predicted lnWomensIncome") ///
			title("First Stage Regression") replace		
	esttab mean0* est1* using firststage.csv, b se ar2 eform ///
			nocons obslast sfmt(fmt(3)) ///
			keep(c.ratioJ#c.mean_l c.ratioJ#c.p10_l c.ratioJ#c.p30_l c.ratioJ#c.p50_l c.ratioJ#c.p70_l c.ratioJ#c.p90_l) /// 
			interaction("x") label nonotes ////
			prehead("Dependent variable: Selected Percentiles of Predicted lnMensIncome") ///
			title("First Stage Regression") append 				
				datasignature set, reset 		
			save 2_1firststage, replace
			
				
********************************
****** 	  MAIN MODEL	  ******
********************************		
cap log close 
log using 3_2_1sample_couples, replace 

frame couple {
	
	use 3_0secondstage, clear 	
	bysort serial: egen couple = count(casenop) 
	keep if coresid==1
	keep if couple==2	
	tab couple coresid, m 
	
local absorb0 "i.age#i.year i.educ#i.year coupleinc anychildren mmarket year educ region age"
local model0 "c.prwomenmore##i.female##i.coresid"
local modelleis0 "c.prwomenmore##i.female##i.coresid##i.leissync"

// DV - individual : pw, upw, leis
cap eststo clear
foreach var in pw upw leis {
			
		* SE clustered at the level of region
		qui eststo `var'_r:  	reghdfe `var' `model0' 			   	, absorb(`absorb0') vce(clust region) noomit noempty nofootnote 
		reghdfe , cformat(%9.3f) noomit noempty nofootnote 
		qui eststo `var'd_r: 	reghdfe `var' `model0'##c.leissync_d 	, absorb(`absorb0') vce(clust region) noomit noempty nofootnote 
		reghdfe , cformat(%9.3f) noomit noempty nofootnote 
		qui eststo `var'l_r:		reghdfe `var' `model0'##c.leissync_l 	, absorb(`absorb0') vce(clust region) noomit noempty nofootnote 
		reghdfe , cformat(%9.3f) noomit noempty nofootnote 
		qui eststo `var'm_r:		reghdfe `var' `model0'##c.leissync_m 	, absorb(`absorb0') vce(clust region) noomit noempty nofootnote 
		reghdfe , cformat(%9.3f) noomit noempty nofootnote 			
		qui eststo `var'll_r:		reghdfe `var' `modelleis0' 			 , absorb(`absorb0') vce(clust region) noomit noempty nofootnote 
		reghdfe , cformat(%9.3f)  nofootnote 	
}
}

esttab upw*_r using tables_couple.csv, b(3) se(3) not r2 ar2 obslast nodepvar mtitle interaction(" X ") ///
noomitted nobase varwidth(20) wrap drop( ///
1.female leissync_d c.prwomenmore#c.leissync_d 1.female#c.leissync_d leissync_l c.prwomenmore#c.leissync_l 1.female#c.leissync_l leissync_m c.prwomenmore#c.leissync_m 1.female#c.leissync_m 1.leissync 1.female#1.leissync 1.leissync#c.prwomenmore _cons ) /// 
 nonote addnotes("Note: Standard errors clustered at the level of region is reported.") ///
title("Table 1. Time Spent on Non-market Work, Relative Income and Leisure Synchronization") ///
prehead("DependentVar: Time Spent on Non-market Work") /// 
varlabels(1.female#c.prwomenmore PrWomenEarnsMore 1.female#c.prwomenmore#c.leissync_d LeisSynchDemographic 1.female#c.prwomenmore#c.leissync_l LeisSynchLaborMarket 1.female#c.prwomenmore#c.leissync_m LeisSynchMarriage 1.female#1.leissync#c.prwomenmore LeisSynch) ///
 replace 

frame change default 
use 1_2leistype, clear 
datasignature confirm 

cap drop couple 
bysort serial: egen couple = count(casenop) 

keep if couple==2
keep if coresid==1 
tab couple coresid, m

drop if relpw==.
drop if lnrelinc==.
drop if relupw==. 
			
			
********************************
****** 		FIGURES		  ******
********************************

* Relative ln(income)
twoway 	hist lnrelinc if female == 0, ///
				fraction bin(50) color(none) lco(black) || /// 
		hist lnrelinc if female == 1, ///
				fraction bin(50) color(stone%70) lco(gray) || /// 
		, xlab(0(.1)1, nogrid labsize(small)) ylab(, labsize(small) nogrid) /// 
		legend(ring(0) pos(12) row(1) symx(*.5) order(2 1) ///
				label(2 Female) label(1 Male)) /// 
		xtitle("Share of Couple's Total Earning (log-scale)", ///
				size(small) margin(small)) /// 
		ytitle("Fraction" , size(small)) /// 
		xline(.5, lcolor(gs10) lpa(shortdash)) /// 
		saving(lnrelinc_both_hist, replace)
twoway 	hist lnrelinc if leissync==1 & female==0, ///
			fraction bin(50) color(none) lco(black) || /// 
		hist lnrelinc if leissync==1 & female==1, ///
			fraction bin(50) color(stone%70) lco(gray) || /// 
		, xlab(0(.1)1, nogrid labsize(small)) ylab(0(.05).4, nogrid labsize(small)) ///
		legend(order(2 1) label(2 Female) label(1 Male) ///
				ring(0) pos(12) row(1) symx(*.5)) ///
		xline(.5, lpa(shortdash) lco(gs10)) /// 
		ytitl("Fraction") xtitle("Share of Total Earnings Within Couple (log-scale)") /// 
		title("Leisure Synchronized with Partner") /// 
		saving(lnrelinc_sync1_hist, replace)
twoway 	hist lnrelinc if leissync==0 & female==0 , ///
				fraction bin(50) color(none) lco(black) || /// 
		hist lnrelinc if leissync==0 & female==1 , ///
				fraction bin(50) color(stone%70) lco(gray) bin(50) || /// 
		, xlab(0(.1)1, nogrid labsize(small)) ylab(0(.05).4, nogrid labsize(small)) ///
		legend(order(2 1) label(2 Female) label(1 Male) ///
				ring(0) pos(12) row(1) symx(*.5)) ///
		xline(.5, lpa(shortdash) lco(gs10)) /// 
		ytitl("Fraction") xtitle("Share of Total Earnings Within Couple (log-scale)") /// 
		title("Leisure Not Synchronized with Partner") ///
		saving(lnrelinc_sync0_hist, replace) 
graph combine lnrelinc_sync1_hist.gph lnrelinc_sync0_hist.gph lnrelinc_both_hist.gph,  hole(3)
graph export lnrelinc_sync_hist.png, replace 
					
* density estimate for relative income by leis sync and gender 
*female partner's share of total earnings withinc ouple by leisure synchronization
kdensity lnrelinc if leissync==1 & female==1, ///
			kernel(triangle) bwidth(.01225) color(black*.8) /// 
			xlab(0(.1)1, nogrid labsize(small)) ylab(0(1)10, nogrid labsize(small)) /// 
			xtitle("Women's Share of Total Earnings Within Couple (log-scale)") /// 
			ytitle("Density") ///
			xline(.5, lpa(shortdash) lco(gs10)) /// 
			title("Leisure Synchronized with Partner", margin(small) ) ///
			note("") /// 
			saving(lnrelinc_female_synchd_kde, replace)
kdensity lnrelinc if leissync==0 & female==1, ///
			kernel(triangle) bwidth(.01225) color(black*.8) /// 
			xlab(0(.1)1, nogrid labsize(small)) ylab(0(1)10, nogrid labsize(small)) /// 
			xtitle("Women's Share of Total Earnings Within Couple (log-scale)") /// 
			ytitle("Density") ///
			xline(.5, lpa(shortdash) lco(gs10)) ///
			title("Leisure Not Synchronized with Partner", margin(small) ) ///
			note("") ///
			saving(lnrelinc_female_notsynchd_kde, replace)
graph combine lnrelinc_female_synchd_kde.gph lnrelinc_female_notsynchd_kde.gph lnrelinc_female_hist.gph, hole(3) imargin(0 0 0 0) graphregion(margin(l=10 r=10)) 
graph export lnrelinc_female_leissync.png, replace 

kdensity lnrelinc if leissync==1 & female==0, ///
			kernel(triangle) bwidth(.01225) color(black*.8) /// 
			xlab(0(.1)1, nogrid labsize(small)) ylab(0(1)10, nogrid labsize(small)) /// 
			xtitle("Men's Share of Total Earnings Within Couple (log-scale)") /// 
			ytitle("Density") ///
			xline(.5, lpa(shortdash) lco(gs10)) /// 
			title("Leisure Synchronized with Partner", margin(small) ) ///
			note("") /// 
			saving(lnrelinc_male_synchd_kde, replace)
kdensity lnrelinc if leissync==0 & female==0, ///
			kernel(triangle) bwidth(.01225) color(black*.8) /// 
			xlab(0(.1)1, nogrid labsize(small)) ylab(0(1)10, nogrid labsize(small)) /// 
			xtitle("Men's Share of Total Earnings Within Couple (log-scale)") /// 
			ytitle("Density") ///
			xline(.5, lpa(shortdash) lco(gs10)) ///
			title("Leisure Not Synchronized with Partner", margin(small) ) ///
			note("") ///
			saving(lnrelinc_male_notsynchd_kde, replace)
graph combine lnrelinc_male_synchd_kde.gph lnrelinc_male_notsynchd_kde.gph lnrelinc_male_hist.gph, hole(3) imargin(0 0 0 0) graphregion(margin(l=10 r=10))
graph export lnrelinc_male_leissync.png, replace






clear 
exit 




