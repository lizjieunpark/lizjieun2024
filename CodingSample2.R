#########
# This script file is a sample version for the submission purpose only, therefore is an 'excerpt' of full coding script for a research project.
# Data is assumed not to contain full sample and is different from what is used for actual analysis.

# The research project is to analyze impact of labor market information on married individual's decision to allocate time using instrumental variable approach. It attempts to explore other mechanisms addressing individual's response to stereotypes, here specifically through leisure synchronization.
#########

# load packages
#install.packages(c("haven", "plm", "quantreg", "plyr", dplyr", "ggplot2", "data.table", "purrr", "estimatr", "glmnet", "randomizr", "gplots", "sjPlot", "sjmisc", "sjlabelled"))

library(haven)                                                                  #stata file import
library(data.table)                                                             #wrangling
library(tidyverse)                                                              #wrangling
library(tidyr)                                                                  #wrangling
library(ggplot2)                                                                #data visualization
library(car)                                                                    #EDA
library(foreign)                                                                #EDA
library(purrr)                                                                  #wrangling
library(glmnet)                                                                 #LASSO and RIDGE reg
library(randomizr)                                                              #randomization
library(dplyr)                                                                  #wrangling
library(sjPlot)                                                                 #result report
library(sjmisc)                                                                 #result report
library(sjlabelled)                                                             #result report
library(gplots)                                                                 #exploring (plotmeans)
library(plm)                                                                    #panel data estimator 



# setup working directory
getwd()
setwd("{OUTPATH}")


# load data and restrict sample (UKTUS)
## to the day diary was filled out was ordinary day, with respondent's individual interview and time diary are both productive 
data_w <- read_dta('uktus15_diary_wide.dta') %>% 
  filter(DiaryType<3, KindOfDay>0, Ordinary==1)%>%
  arrange(serial) 
data_v <- read_dta('uktus15_dv_time_vars.dta') %>% 
  filter(DiaryType<3, DiaryType>0, KindOfDay>0, Ordinary==1) %>%
  arrange(serial) 
data_hh <- read_dta('uktus15_household.dta')  %>% 
  arrange(serial)
data_i <- read_dta('uktus15_individual.dta')  %>% 
  arrange(serial)
data_l <- read_dta('uktus15_diary_ep_long.dta') %>% 
  filter(DiaryType<3, KindOfDay>0, Ordinary==1)%>%
  arrange(serial)


lfs14 <- read_dta('lfsp_od14_eul.dta')                                          # t = 2014
lfs08 <- read_dta('lfsp_od08_end_user.dta')                                     # base year 2008 (6 years before UKTUS; income reported) 


# LFS income var wrangling
ADULT2014 <- lfs14 %>% filter(CAIND==1, HOUT<=20)                               # restrict to those reported to be adults; HOUT==11, 12, 20; 12=econ non-active. 
ADULT2008 <- lfs08 %>% filter(CAIND==1, HOUT<=20)



# indicators
#identify intimate partner for individuals, if reported, excluding civil union (same-sex)
partnerno_df <- data_i %>% 
  select(serial, pnum, Relate1:Relate10) %>%
  mutate(partnerno = case_when(
    Relate1 == 1.0 ~ 1,
    Relate1 == 3.0 ~ 1,
    Relate2 == 1.0 ~ 2,
    Relate2 == 3.0 ~ 2,
    Relate3 == 1.0 ~ 3,
    Relate3 == 3.0 ~ 3,
    Relate4 == 1.0 ~ 4,
    Relate4 == 3.0 ~ 4,
    Relate5 == 1.0 ~ 5,
    Relate5 == 3.0 ~ 5,
    Relate6 == 1.0 ~ 6,
    Relate6 == 3.0 ~ 6,
    Relate7 == 1.0 ~ 7,
    Relate7 == 3.0 ~ 7,
    Relate8 == 1.0 ~ 8,
    Relate8 == 3.0 ~ 8,
    Relate9 == 1.0 ~ 9,
    Relate9 == 3.0 ~ 9,
    Relate10 == 1.0 ~ 10,
    Relate10 == 3.0 ~ 10,
    TRUE ~ NA_real_
  )) %>% arrange(serial)                                                        # otherwise, respondents not living with intimate partner or singles will be indexed with 'NA'. 

#indexing couples within household
partnerno_df <- partnerno_df %>%                                                # as there exists households with multiple couples (i.e., different generations of family)
  rowwise() %>%
  mutate(coupleno = case_when(
    pnum==1 && partnerno==2 ~ 'a',
    pnum==1 && partnerno==3 ~ 'b',
    pnum==1 && partnerno==4 ~ 'c',
    pnum==1 && partnerno==5 ~ 'd',
    
    pnum==2 && partnerno==1 ~ 'a', 
    pnum==2 && partnerno==3 ~ 'e',
    pnum==2 && partnerno==4 ~ 'f',
    pnum==2 && partnerno==6 ~ 'g',
    
    pnum==3 && partnerno==1 ~ 'b',
    pnum==3 && partnerno==2 ~ 'e',
    pnum==3 && partnerno==4 ~ 'h',
    
    pnum==4 && partnerno==1 ~ 'c',
    pnum==4 && partnerno==2 ~ 'f',
    pnum==4 && partnerno==3 ~ 'h',
    pnum==4 && partnerno==5 ~ 'i',
    
    pnum==5 && partnerno==1 ~ 'd',
    pnum==5 && partnerno==4 ~ 'i',
    pnum==5 && partnerno==6 ~ 'j',
    
    pnum==6 && partnerno==2 ~ 'g',
    pnum==6 && partnerno==5 ~ 'j'
  )) %>% arrange(serial)

data_i <- data_i %>% full_join(partnerno_df)


#total net salary/wage
income <- data_i %>% 
  select(serial, pnum, NetWkly, SENetPay, OJWkly) %>%                           # weekly labor earning from main job, self-employment, and other jobs
  rowwise() %>%
  mutate(
    mainwkly = case_when(
      NetWkly < 0 ~ 0, 
      TRUE ~ as.numeric(NetWkly)),
    sewkly = case_when(
      SENetPay < 0 ~ 0, 
      TRUE ~ as.numeric(SENetPay*(7/30))),                                      # 7/30 = divide monthly earning by 30 (days) multiplied by 7 (days) = weekly earning from SE
    ojwkly = case_when(
      OJWkly < 0 ~ 0, 
      TRUE ~ as.numeric(OJWkly)),
    netinc = sum(mainwkly, sewkly, ojwkly)
  ) 

##income %>% filter(netinc==0) %>% group_by(pnum) %>% summarise(obs=n())        # obs=7565: no. of ind. with no earning 
data_i <- data_i %>% full_join(income)



# number of days the time-use is reported  for
data_v <- data_v %>%
  mutate(nddayw=case_when(
    DiaryDay_Act <= 0 ~ 0, 
    DiaryDay_Act > 0 ~ 1,
    TRUE ~ NA_real_
  )) %>%
  group_by(serial, pnum) %>%
  nest() %>%
  mutate(ndays=map(.x=data, .f=~sum(.x$nddayw, na.rm=T))) %>%
  unnest(cols=c(data, ndays))

## data_v %>% group_by(ndays) %>% tally()                                       # 1: 2283, 2: 8624. 


# time-allocation categories
tus <- data_v %>%
  select(serial, pnum, ddayw, ndays, dml1_0:dml4_9999) %>% 
  rowwise() %>% 
  mutate(pw=sum(dml1_1, dml2_91)) %>%
  mutate(edu=sum(dml1_2, dml2_92)) %>%
  mutate(hcap=sum(pw, edu)) %>%                                                 # includes activity related to employment but not for salary/wage or edu
  mutate(pc=sum(dml1_0, dml3_901)) %>%                                          # sleep, eating, other personal care
  mutate(upw=sum(dml1_3, dml2_93)) %>%                                          # total upw
  mutate(chores=sum(dml2_31, dml2_33, dml2_36, dml3_936, dml3_937)) %>%
  mutate(housemgmt=sum(dml2_32, dml2_34, dml2_35, dml2_37, dml3_931)) %>%
  mutate(cc=sum(dml2_38, dml3_923, dml3_938)) %>%                               # care for hh member children, related travel
  mutate(ec=sum(dml2_39, dml3_939)) %>%                                         # care for hh member adult, related travel
  mutate(leis=sum(dml1_5, dml1_6, dml1_7, dml1_8, dml2_95, dml2_96, dml2_97)    # all for recreational, leisure purposes 
  ) %>%
  mutate(leis2=sum(dml1_4, dml1_5, dml1_6, dml1_7, dml1_8, dml2_94, dml2_95,    # all for recreational incl volunteers 
                   dml2_96, dml2_97)) %>% 
  mutate(sleep=sum(dml1_0, dml2_1)) %>%
  mutate(inwork=ifelse(pw>0, 1, 0)) %>%
  mutate(                                                                       # disaggregate by specific activities
    rest=sum(dml2_53),                                                          # rest - time out
    social=sum(dml3_510, dml3_511, dml3_512, dml3_513, dml3_514, dml3_519,      # socializing, visit/receive, feasts, telephone conversation, other specified, and travel to visit and for socializing 
               dml3_950, dml3_951),                                             
    ent=sum(dml3_520, dml3_521, dml3_522, dml3_523, dml3_524, dml3_525,         # cinema, theatre/concert, art exhibition and museum, library, sport events, other, and related travels
            dml3_529, dml3_952),                                                
    sports=sum(dml2_60, dml2_61, dml2_62, dml2_63, dml3_982, dml3_961,          # physical exercises, ball games, productivie exercises, sports related activities, and related travels 
               dml3_962, dml3_963),                                              
    artshobbies=sum(dml2_70, dml2_71, dml2_72, dml2_73, dml3_971, dml3_972),    # arts and hobbies, computing, games, and related travels
    media=sum(dml1_8),                                                          # mass media incl. reading, TV/video/DVD, radio/music
    vol=sum(dml1_4, dml2_94)                                                    # volunteer, participatory, organization work, related travel 
  ) %>% 
  mutate(
    capInt=sum(
      dml3_513, dml3_514, dml3_521, dml3_522, dml3_523, dml3_613, dml3_615, 
      dml3_616, dml3_617, dml3_710, dml3_711, dml3_712, dml3_713, dml3_714, 
      dml3_716, dml3_717, dml3_719, dml3_722, dml3_723, dml3_724, dml3_725, 
      dml3_733, dml3_734, dml3_821, dml3_822, dml3_830, dml3_831, dml3_832), 
    timeInt=sum(
      dml3_400, dml3_410, dml3_411, dml3_412, dml3_419, dml3_420, dml3_421, 
      dml3_422, dml3_423, dml3_424, dml3_425, dml3_426, dml3_427, dml3_428, 
      dml3_429, dml3_430, dml3_431, dml3_432, dml3_439, dml3_500, dml3_510, 
      dml3_511, dml3_512, dml3_519, dml3_520, dml3_524, dml3_529, dml3_531, 
      dml3_600, dml3_610, dml3_611, dml3_612, dml3_614, dml3_619, dml3_620, 
      dml3_621, dml3_622, dml3_629, dml3_631, dml3_700, dml3_715, dml3_730,
      dml3_731, dml3_732, dml3_739, dml3_810, dml3_811, dml3_812, dml3_819)
  ) %>% 
  #leis2 includes volunteer/civic participation activities, which is not necessarily always purported for recreation
  mutate(
    upwratio=upw/1440, choresratio=chores/1440, 
    hmgmtratio=housemgmt/1440, ccratio=cc/1440, ccupw=cc/upw, 
    pwratio=pw/1440, hcapratio=hcap/1440,
    leisratio=leis/1440, leisratio2=leis2/1440, pcratio=pc/1440, 
    capratio0=capInt/1440, capratio1=capInt/leis, capratio2=capInt/leis2,
    timeratio0=timeInt/1440, timeratio1=timeInt/leis, 
    timeratio2 = timeInt/leis2
  ) %>%                                                                         # ratio
  arrange(serial) %>%
  group_by(ndays, serial, pnum) %>%
  nest() %>%
  mutate(
    pwwkly=map(.x=data, .f=~sum(.x$pw)), eduwkly=map(.x=data, .f=~sum(.x$edu)), 
    hcapwkly=map(.x=data, .f=~sum(.x$hcap)), pcwkly=map(.x=data, .f=~sum(.x$pc)), 
    upwwkly=map(.x=data, .f=~sum(.x$upw)), choreswkly=map(.x=data, .f=~sum(.x$chores)), 
    housemgmtwkly=map(.x=data, .f=~sum(.x$housemgmt)), ccwkly=map(.x=data, .f=~sum(.x$cc)), 
    ecwkly=map(.x=data, .f=~sum(.x$ec)), leiswkly=map(.x=data, .f=~sum(.x$leis)), 
    leis2wkly=map(.x=data, .f=~sum(.x$leis2)), restwkly=map(.x=data, .f=~sum(.x$rest)), 
    socialwkly=map(.x=data, .f=~sum(.x$pw)), entwkly=map(.x=data, .f=~sum(.x$ent)), 
    sportswkly=map(.x=data, .f=~sum(.x$sports)), artshobwkly=map(.x=data, .f=~sum(.x$artshobbies)),
    mediawkly=map(.x=data, .f=~sum(.x$media)), volwkly=map(.x=data, .f=~sum(.x$vol)), 
    capIntwkly=map(.x=data, .f=~sum(.x$capInt)), timeIntwkly=map(.x=data, .f=~sum(.x$timeInt))
  ) %>%
  unnest(cols=c(data, pwwkly, eduwkly, hcapwkly, pcwkly, upwwkly, choreswkly, 
                housemgmtwkly, ccwkly, ecwkly, leiswkly, leis2wkly, restwkly, 
                socialwkly, entwkly, sportswkly, artshobwkly, mediawkly, 
                volwkly, capIntwkly, timeIntwkly)) %>%
  mutate(
    upwratiowkly=(upwwkly/(1440*ndays)), choresratiowkly=(choreswkly/(1440*ndays)), 
    hmgmtratiowkly=(housemgmtwkly/(1440*ndays)), ccratiowkly=(ccwkly/(1440*ndays)), 
    pwratiowkly=pwwkly/(1440*ndays), hcapratiowkly=hcapwkly/(1440*ndays), 
    leisratiowkly=leiswkly/(1440*ndays), leisratio2wkly=leis2wkly/(1440*ndays), 
    pcratiowkly=pcwkly/(1440*ndays), capratio0wkly=capIntwkly/(1440*ndays), 
    capratio1wkly=capIntwkly/leiswkly, capratio2wkly=capIntwkly/leis2wkly, 
    timeratio0wkly=timeIntwkly/(1440*ndays), timeratio1wkly=timeIntwkly/leis, 
    timeratio2wkly=timeIntwkly/leis2)
##all(is.numeric(tus$inwork))                                                   # class check for inwork
##sapply(tus, class)                                                            # class check 1

data_v <- data_v %>% full_join(tus)                                             # merge



#relative earning to total household income
relinc_hh <- data_i %>% select(serial, pnum, netinc, Income) %>%
  filter(Income>0) %>% 
  mutate(relinc_hh=netinc/Income) %>%                                           # earning share to total household income which includes non-salaries such as gvt benefits
  arrange(serial)                                            
data_i <- data_i %>% full_join(relinc_hh)

rm(relinc_hh)                                                                   


check_ndays <- data_w %>%                                                       # mutate number of days individual reported for time-use for this dataset
  select(serial, pnum, daynum, ddayw) %>%
  mutate(nddayw=case_when(
    ddayw==1 ~ 1, 
    ddayw==2 ~ -2, 
    ddayw==3 ~ 4,
    TRUE ~ NA_real_
  )) %>%
  group_by(serial, pnum) %>%
  nest() %>%
  mutate(
    nddayw_cal= map(.x=data, .f=~sum(.x$nddayw, na.rm=T))
  ) %>%
  unnest(cols=c(data, nddayw_cal)) %>%
  mutate(ndays=case_when(
    nddayw_cal==1 ~ 1, 
    nddayw_cal==-2 ~ 1, 
    nddayw_cal==4 ~ 1, 
    nddayw_cal==-1 ~ 2, 
    nddayw_cal==5 ~ 2, 
    nddayw_cal==2 ~ 2
  )) %>% 
  select(serial, pnum, daynum, ddayw, ndays)

data_l <- data_l %>% full_join(check_ndays) %>% fill(ndays)


jtime_FT0 <- data_l %>%                                                         # leisure time excluding volunteer activities 
  select(serial, pnum, daynum, eptime, whatdoing, WithSpouse, ddayw, ndays) %>%
  filter(whatdoing>=5000, whatdoing<=8320, WithSpouse==1) %>%
  rowwise() %>%
  mutate(jtime=eptime*WithSpouse) %>%
  group_by(serial, pnum) %>%
  nest() %>%
  mutate(jtime_ft0=map(.x=data, .f=~(sum(.x$jtime, na.rm=T)))) %>%
  unnest(cols=c(data, jtime_ft0)) %>%
  mutate(jtime_ft=jtime_ft0/ndays, jt_ftratio=jtime_ft/1440) %>% 
  relocate(jtime_ft, jt_ftratio, .after=eptime)

jtime_FTVol <- data_l %>%                                                       # leisure time inclusive of volunteer activities
  select(serial, pnum, daynum, eptime, whatdoing, WithSpouse, ddayw, ndays) %>%
  filter(whatdoing>=4000, whatdoing<=8320, WithSpouse==1) %>%
  rowwise() %>%
  mutate(jtime=eptime*WithSpouse) %>%
  group_by(serial, pnum) %>%
  nest() %>%
  mutate(jtime_ft0=map(.x=data, .f=~(sum(.x$jtime, na.rm=T)))) %>%
  unnest(cols=c(data, jtime_ft0)) %>%
  mutate(jtime_ftvol=jtime_ft0/ndays, jt_ftvolratio=jtime_ftvol/1440) %>% 
  relocate(jtime_ftvol, jt_ftvolratio, .after=eptime)

jtime_leis <- jtime_FT0 %>% full_join(jtime_FTVol)                              # merge subsets 



# merge into one dataset
adult_all <- data_w %>% 
  full_join(data_i) %>% full_join(data_v) %>% full_join(data_hh) %>% 
  full_join(jtime_leis) %>%
  select(serial, pnum, DiaryType, DiaryDate_Act, KindOfDay, ddayw, ndays,
         DMSex, DVAge, MarStat, dmarsta, dhiqual, dnrkid04, dagegrp, WorkSta, 
         duresmc, dgorpaf, dukcntr, dsic, DM014, DM016, DM510, DM1115, DM1619, 
         partnerno, coupleno, netinc, mainwkly, sewkly, ojwkly,
         #jtime_tot, jtratio, 
         jtime_ft, jt_ftratio, jtime_ftvol, jt_ftvolratio,  
         pw, upw, leis, leis2, pc, edu, hcap, cc, ec, sleep,
         chores, housemgmt, rest, social, ent, sports, artshobbies, media, vol,
         capInt, timeInt, inwork,
         upwratio, leisratio, leisratio2, pwratio, pcratio, hcapratio, ccupw, 
         capratio0, capratio1, capratio2, timeratio0, timeratio1, timeratio2,
         pwwkly, eduwkly, hcapwkly, pcwkly, upwwkly, choreswkly, housemgmtwkly, 
         ccwkly, ecwkly, leiswkly, leis2wkly, restwkly, socialwkly, entwkly, 
         sportswkly, artshobwkly, mediawkly, volwkly, capIntwkly, timeIntwkly,
         upwratiowkly, choresratiowkly, hmgmtratiowkly, ccratiowkly, 
         pwratiowkly, hcapratiowkly, leisratiowkly, leisratio2wkly, pcratiowkly, 
         capratio0wkly, capratio1wkly, capratio2wkly,
         timeratio0wkly, timeratio1wkly, timeratio2wkly,
         relinc_hh,
         SatPart, SatJob, SatInc, SatLeis, SatSoc, SatBal,
         Theatre, Librar, Walking, Museum, EntHome, Swim, Cinema, HistSite, 
         EatDrink, KeepFit, Running, Racquet, Cycling, SportEv, TeamGame, Golf 
  ) %>% 
  distinct(serial, pnum, ddayw, DiaryDate_Act, .keep_all=TRUE)  %>%
  mutate(
    year = 2014                                                                 
  )



# restrict sample and mutate total time within couple by main categories
#to individuals with intimate partners only (including same-sex)# for faster calculation
couple_all <- adult_all %>% filter(dmarsta==2) %>%                              # 2==married/cohabitating (incl. same-sex)
  rowwise() %>% 
  group_by(serial, coupleno) %>% 
  nest() %>%
  mutate(LeisBoth = map(.x=data, .f=~sum(.x$leiswkly, na.rm=T)), 
         Leis2Both = map(.x=data, .f=~sum(.x$leis2wkly, na.rm=T)),
         CCBoth=map(.x=data, .f=~sum(.x$ccwkly, na.rm=T)), 
         ChoresBoth=map(.x=data, .f=~sum(.x$choreswkly, na.rm=T)), 
         HmgmtBoth=map(.x=data, .f=~sum(.x$housemgmtwkly, na.rm=T)), 
         PCBoth=map(.x=data, .f=~sum(.x$pcwkly, na.rm=T)), 
         PWBoth=map(.x=data, .f=~sum(.x$pwwkly, na.rm=T)), 
         UpwBoth=map(.x=data, .f=~sum(.x$upwwkly, na.rm=T)), 
         HcapBoth=map(.x=data, .f=~sum(.x$hcapwkly, na.rm=T)), 
         EduBoth=map(.x=data, .f=~sum(.x$eduwkly, na.rm=T)), 
         IncBoth=map(.x=data, .f=~sum(.x$netinc, na.rm=T)), 
         CapBoth=map(.x=data, .f=~sum(.x$capIntwkly, na.rm=T)),
         TimeBoth=map(.x=data, .f=~sum(.x$timeIntwkly, na.rm=T))) %>% 
  unnest(cols=c(
    data, LeisBoth, Leis2Both, PWBoth, UpwBoth, CapBoth, TimeBoth, 
    CCBoth, ChoresBoth, HmgmtBoth, PCBoth, HcapBoth, EduBoth, IncBoth)) %>%
  rowwise() %>%
  mutate(leisrel=(leiswkly/LeisBoth), 
         leis2rel=(leis2wkly/Leis2Both), choresrel=(choreswkly/ChoresBoth), 
         hmgmtrel=(housemgmtwkly/HmgmtBoth), ccrel=(ccwkly/CCBoth), 
         pwrel=(pwwkly/PWBoth), pcrel= pcwkly/PCBoth, upwrel=(upwwkly/UpwBoth), 
         hcaprel=(hcapwkly/HcapBoth), edurel=(eduwkly/EduBoth), 
         increl=(netinc/IncBoth), caprel=(capIntwkly/CapBoth), 
         timerel=(timeIntwkly/TimeBoth)) %>% 
  ungroup() 




# remove
rm(check_ndays) 
rm(jtime_FT0)
rm(jtime_FTVol)





# load data and restrict samples (LFS)
## to those who reported net weekly income with period covering the reported amount
lfs14 <- read_dta('lfsp_od14_eul.dta')                                          # t = 2014
lfs08 <- read_dta('lfsp_od08_end_user.dta')                                     # base year 2008 (6 years before UKTUS; income reported) 


ADULT2014 <- lfs14 %>% filter(CAIND==1, HOUT<=20)                               # restrict to those reported to be adults; HOUT==11, 12, 20; 12=econ non-active. 
ADULT2008 <- lfs08 %>% filter(CAIND==1, HOUT<=20)


## for year 2014 
INCOME2014 <- ADULT2014 %>%                                                     # mutating income for everyone who reported relev info
  select(CASENOP, HSERIALP, PERSNO, RELHRP6, FAMUNIT, TOTHRS, NETWK, NETWK2, 
         SEX, AGE, AGES, MARSTA, HIQUL11D, HIQUAL11, COUNTRY, IN0792EM, 
         FDPCH2, FDPCH4, FDPCH15, FDPCH16, FDPCH19) %>% 
  filter(NETWK>=0) %>% 
  rowwise() %>%
  mutate(
    NETWK2N=case_when(                                                          # to include those who did not report income from second job but have reported for main job
      NETWK2 < 0 ~ 0,
      NETWK2 <= 99996 ~ as.numeric(NETWK2),
      TRUE ~ 0
    ), 
    TOTHRSN=case_when(
      TOTHRS < 0 ~ 0, 
      TOTHRS <=97 ~ as.numeric(TOTHRS), 
      TOTHRS == 98 ~ NA_real_,                                                  # away from job 
      TRUE ~ NA_real_
    ),
    NETWKTOT=sum(NETWK, NETWK2N)                                                # and derive the total net weekly income 
  ) 

#INCOME2014 %>% count(FDPCH2)                                                   # checking if children variables are selected 


## for year 2008
INCOME2008 <- ADULT2008 %>%                                                     # mutating income for everyone who reported relev info
  select(CASENOP, HSERIALP, PERSNO, RELHRP6, FAMUNIT, TOTHRS, NETWK, NETWK2, 
         SEX, AGE, AGES, MARSTA, HIQUAL8D, HIQUAL8, COUNTRY, INDSECT, 
         FDPCH2, FDPCH4, FDPCH15, FDPCH16, FDPCH19) %>%                         # edu: HIQUAL8D; industry: INDSECT
  filter(NETWK>0) %>%                                                           # NETWK==0 does not exist (deliberately by data producer)
  rowwise() %>%
  mutate(
    NETWK2N=case_when(                                                          # to include those who did not report income from second job but have reported for main job
      NETWK2 < 0 ~ 0,
      NETWK2 <= 99996 ~ as.numeric(NETWK2),
      TRUE ~ 0
    ), 
    TOTHRSN=case_when(
      TOTHRS < 0 ~ 0, 
      TOTHRS <=97 ~ as.numeric(TOTHRS), 
      TOTHRS == 98 ~ NA_real_,                                                  # away from job 
      TRUE ~ NA_real_
    ),
    NETWKTOT=sum(NETWK, NETWK2N)                                                # and derive the total net weekly income 
  ) 



# get partner number - for 2014 
test1 <- INCOME2014 %>% 
  filter(RELHRP6<2) %>%
  group_by(HSERIALP, FAMUNIT) %>% nest() %>%
  mutate(
    couplecheck=map(data, ~sum(.x$RELHRP6), na.rm=T), 
    persnosum=map(data, ~sum(.x$PERSNO), na.rm=T)) %>%
  unnest(cols=c(data, couplecheck, persnosum)) %>%
  group_by(HSERIALP, FAMUNIT) %>% add_tally(., name="numind") %>%
  filter(numind>1, couplecheck>0) %>%
  rowwise() %>%
  mutate(PARTNERNO=persnosum-PERSNO) %>% 
  arrange(HSERIALP)

test2 <- INCOME2014 %>%
  filter(RELHRP6==7) %>%
  group_by(HSERIALP, FAMUNIT) %>% nest() %>%
  mutate(
    couplecheck=map(data, ~sum(.x$RELHRP6), na.rm=T), 
    persnosum=map(data, ~sum(.x$PERSNO), na.rm=T)
  ) %>%
  unnest(cols=c(data, couplecheck, persnosum)) %>%
  group_by(HSERIALP, FAMUNIT) %>%
  add_tally(., name="numind") %>%
  filter(couplecheck==14) %>%
  rowwise() %>%
  mutate(PARTNERNO=persnosum-PERSNO) %>%
  arrange(HSERIALP) 

test3 <- INCOME2014 %>%
  filter(RELHRP6==3 | RELHRP6==6) %>%
  group_by(HSERIALP, FAMUNIT) %>% nest() %>%
  mutate(
    couplecheck=map(data, ~sum(.x$RELHRP6), na.rm=T), 
    persnosum=map(data, ~sum(.x$PERSNO), na.rm=T),
  ) %>%
  unnest(cols=c(data, couplecheck, persnosum)) %>%
  group_by(HSERIALP, FAMUNIT) %>%
  add_tally(., name="numind") %>%
  filter(couplecheck==9) %>%
  filter(numind==2) %>%
  rowwise() %>%
  mutate(PARTNERNO=persnosum-PERSNO) %>%
  arrange(HSERIALP) 

test4 <- INCOME2014 %>%
  filter(RELHRP6==12 | RELHRP6==15) %>%
  group_by(HSERIALP, FAMUNIT) %>% nest() %>%
  mutate(
    couplecheck=map(data, ~sum(.x$RELHRP6), na.rm=T), 
    persnosum=map(data, ~sum(.x$PERSNO), na.rm=T)) %>%
  unnest(cols=c(data, couplecheck, persnosum)) %>%
  group_by(HSERIALP, FAMUNIT) %>% add_tally(., name="numind") %>%
  filter(couplecheck==27 ) %>%
  rowwise() %>%
  mutate(PARTNERNO=persnosum-PERSNO) %>%
  arrange(HSERIALP)

test5 <- INCOME2014 %>%
  filter(RELHRP6==11) %>%
  group_by(HSERIALP, FAMUNIT) %>% nest() %>%
  mutate(
    couplecheck=map(data, ~sum(.x$RELHRP6), na.rm=T), 
    persnosum=map(data, ~sum(.x$PERSNO), na.rm=T)) %>%
  unnest(cols=c(data, couplecheck, persnosum)) %>%
  group_by(HSERIALP, FAMUNIT) %>% 
  add_tally(., name="numind") %>%
  filter(couplecheck==22) %>%
  rowwise() %>%
  mutate(PARTNERNO=persnosum-PERSNO) %>%
  arrange(HSERIALP) 

INCOME2014 <- INCOME2014 %>% full_join(test1) %>% full_join(test2) %>% 
  full_join(test3) %>% full_join(test4) %>% full_join(test5) %>% 
  distinct(CASENOP, .keep_all=T) %>% arrange(CASENOP)                           # merge subsets

rm(test1); rm(test2); rm(test3); rm(test4); rm(test5)



# get couple number - for year 2014
INCOME2014 <- INCOME2014 %>%
  rowwise() %>%
  mutate(
    COUPLENO=case_when(
      PERSNO==1 & PARTNERNO==2 ~ 'a', 
      PERSNO==1 & PARTNERNO==3 ~ 'b',
      PERSNO==1 & PARTNERNO==4 ~ 'c',
      PERSNO==1 & PARTNERNO==5 ~ 'd',
      PERSNO==1 & PARTNERNO==6 ~ 'e',
      PERSNO==1 & PARTNERNO==7 ~ 'f',
      
      PERSNO==2 & PARTNERNO==1 ~ 'a',
      PERSNO==2 & PARTNERNO==3 ~ 'g',
      PERSNO==2 & PARTNERNO==4 ~ 'h',
      PERSNO==2 & PARTNERNO==6 ~ 'i',
      
      PERSNO==3 & PARTNERNO==1 ~ 'b',
      PERSNO==3 & PARTNERNO==2 ~ 'g',
      PERSNO==3 & PARTNERNO==4 ~ 'j',
      PERSNO==3 & PARTNERNO==5 ~ 'k',
      PERSNO==3 & PARTNERNO==7 ~ 'l',
      
      PERSNO==4 & PARTNERNO==1 ~ 'c',
      PERSNO==4 & PARTNERNO==2 ~ 'h',
      PERSNO==4 & PARTNERNO==3 ~ 'j',
      PERSNO==4 & PARTNERNO==5 ~ 'm',
      PERSNO==4 & PARTNERNO==6 ~ 'n',
      
      PERSNO==5 & PARTNERNO==1 ~ 'd',
      PERSNO==5 & PARTNERNO==3 ~ 'k',
      PERSNO==5 & PARTNERNO==4 ~ 'm',
      PERSNO==5 & PARTNERNO==6 ~ 'o',
      PERSNO==5 & PARTNERNO==7 ~ 'p',
      PERSNO==5 & PARTNERNO==8 ~ 'q',
      
      PERSNO==6 & PARTNERNO==1 ~ 'e',
      PERSNO==6 & PARTNERNO==2 ~ 'i',
      PERSNO==6 & PARTNERNO==4 ~ 'n',
      PERSNO==6 & PARTNERNO==5 ~ 'o',
      PERSNO==6 & PARTNERNO==7 ~ 'r',
      
      PERSNO==7 & PARTNERNO==1 ~ 'f',
      PERSNO==7 & PARTNERNO==3 ~ 'l',
      PERSNO==7 & PARTNERNO==5 ~ 'p',
      PERSNO==7 & PARTNERNO==6 ~ 'r',
      PERSNO==7 & PARTNERNO==8 ~ 's',
      
      PERSNO==8 & PARTNERNO==5 ~ 'q',
      PERSNO==8 & PARTNERNO==7 ~ 's',
      PERSNO==8 & PARTNERNO==9 ~ 't',
      
      PERSNO==9 & PARTNERNO==8 ~ 't',
      
      TRUE ~ NA_character_)) 


INCOME2014 <- INCOME2014 %>% 
  relocate(CASENOP, .before=FAMUNIT) %>%
  relocate(PERSNO, PARTNERNO, COUPLENO, .after=HSERIALP)                        # relocating columns for easier look up



# couple's total i) income and ii) time spent on main job (paid or unpaid but market work)
INCOME2014 <- INCOME2014 %>%
  rowwise() %>%
  group_by(HSERIALP, COUPLENO) %>% nest() %>%                                   # calculate total hh income and paid work time spent by spouses and their individual share
  # this calculation includes total income in household regardless of individual's marital status;
  # use !is.na(COUPLENO), !is.na(PARTNERNO) to focus on couples only 
  mutate(
    INCHH=map(data, ~sum(.x$NETWKTOT), na.rm=T),
    PWTHH=map(data, ~sum(.x$TOTHRSN), na.rm=T)
  ) %>%
  unnest(cols=c(data, INCHH, PWTHH)) %>%
  rowwise() %>%
  group_by(HSERIALP, COUPLENO) %>% nest() %>%
  mutate(
    increl=map(data, ~(.x$NETWKTOT/.x$INCHH)),
    pwrel=map(data, ~(.x$TOTHRSN/.x$PWTHH))
  ) %>%
  unnest(cols=c(data, increl, pwrel)) %>% 
  rowwise() %>%
  mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))


#check
#INCOME %>% filter(!is.na(COUPLENO), !is.na(PARTNERNO)) %>% group_by(SEX) %>% tally # couples: n=5767



# replace NaN values for paid work time and earning share for individuals, as NaN is resulted from NETWKTOT==0 or TOTHRS==0. 
##INCOME2014 %>% filter(is.nan(PWTSH)) %>% group_by(SEX) %>% tally                # count how many have TOTHRSN==0 (due to job away, etc); n=693; replace with 0

INCOME2014 <- INCOME2014 %>%
  rowwise() %>% 
  mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))




# SORTING
# mutating relevant re-grouping factor variables
adult_all <- adult_all %>% 
  group_by(serial, ddayw, coupleno) %>% nest() %>% 
  mutate(TOTAGE=map(data, .f=~sum(.x$DVAge))) %>% 
  unnest(cols=c(data, TOTAGE)) %>%
  rowwise() %>%
  mutate(PAGE=TOTAGE-DVAge) %>%                                                 # and get partner's age by subtracting individual's age
  rowwise() %>% 
  mutate(
    NEDUGRP1=case_when(                                                         # grouping by edu qualification following categories in Bertrand et al (2015) for the purpose of assortativeness 
      dhiqual== 1 ~ 1,
      dhiqual== 2 ~ 2, 
      dhiqual== 3 ~ 2,
      dhiqual== 4 ~ 2, 
      dhiqual== 5 ~ 2, 
      TRUE ~ 0
    ), 
  ) %>% 
  
  rowwise() %>%
  group_by(serial, ddayw, coupleno) %>% nest() %>%
  mutate(
    TOTEDU1=map(.x=data, .f=~sum(.x$NEDUGRP1, na.rm=T))
  ) %>% 
  unnest(cols=c(data, TOTEDU1)) %>% 
  
  rowwise() %>% 
  mutate(
    PEDU1=(TOTEDU1-NEDUGRP1)) %>% 
  
  rowwise() %>% 
  mutate(                                                                       # grouping by ages following categories in Bertrand et al (2015)
    NAGEGRP=case_when(
      DVAge<0 ~ 0,
      DVAge>=22 & DVAge<=31 ~ 1, 
      DVAge<=41 ~ 2, 
      DVAge<=51 ~ 3, 
      DVAge>=52 ~ 4,
      TRUE ~ NA_real_
    ),
    PAGEGRP = case_when(
      PAGE>=24 & PAGE<=33 ~ -4,
      PAGE<=43 ~ 5,
      PAGE<=53 ~ 6,
      PAGE>=54 ~ 7,
      TRUE ~ NA_real_
    ), 
    NEDUGRP = case_when(                                                        # grouping by edu qualification for the purpose of demograhpic grouping later, as in Bertrand et al (2015) 
      dhiqual == 1 ~ 1, 
      dhiqual == 2 | dhiqual == 3 ~ 2, 
      dhiqual == 4 ~ 3, 
      TRUE ~ 0
    ), 
    PEDUGRP1 = case_when(
      PEDU1==1 ~ 3,
      PEDU1==2 ~ 4,
      TRUE ~ NA_real_
    ),
    NCOUNTRY = case_when(
      dukcntr == 1 ~ 1, 
      dukcntr == 2 ~ 2, 
      dukcntr == 3 ~ 3, 
      dukcntr == 4 ~ 4, 
      TRUE ~ 0 
    ), 
    INDSECT = case_when(                                                        # 9 industries; and those not included in any of them are indexed as 0 
      dsic == 1 ~ 1, 
      dsic == 2 ~ 3, # manufacturing == 3 in INDSECT in LFS 2008 and 2014
      dsic == 3 ~ 2, # energy and water supply ==2 in INDSECT in LFS 2008 and 2014 
      dsic>=3 & dsic<=9 ~ as.numeric(dsic), # all else are same, compatible with LFS category index
      TRUE ~ 0
    )
  ) 

#check
adult_all %>% count(dsic)

# mutating factor var for presence of children and their age 
adult_all <- adult_all %>% 
  rowwise() %>%
  mutate(
    anyChildren = case_when(                                                    # reported presence of any children in family under 19
      dnrkid04 > 0 | DM014 >0 | DM016 >0 | DM510 >0 | DM1115 >0 | DM1619 > 0 ~ 1, 
      TRUE ~ 0
    ),
    child16 = case_when(                                                        # reported any children in family under 16
      DM016 > 0 ~ 1, 
      TRUE ~ 0
    ), 
    child15 = case_when(                                                        # reported any children in family under 15
      dnrkid04 >0 | DM014 > 0 | DM1115 >0 ~ 1, 
      TRUE ~ 0
    ), 
    child4 = case_when(                                                         # reported any children in family under 5
      dnrkid04 >0 ~ 1, 
      TRUE ~ 0
    )
  )

adult_all %>% count(anyChildren)                                                # why is this working and not working from time to time???? 
practice_final %>% count(mmgrp)
practice_final %>% count(dg)


# rename variables of UKTUS to bind with LFS (preliminary wrangling before constructing instrumented avg wage and bartik inst. )
adult_all <- adult_all %>% 
  rename(
    SEX=DMSex, AGE=DVAge, PARTNERNO=partnerno, COUPLENO=coupleno, 
    HSERIALP=serial, PERSNO=pnum, NETWKTOT=netinc
  ) %>% ungroup()


# bind three different datasets
## before that we select and mutate vars that are relevant with instrumental variables
adult_allB <- adult_all %>% 
  ungroup() %>% 
  select(
    HSERIALP, PERSNO,                                                           # identification of individuals 
    NETWKTOT,                                                                   # our income variable 
    SEX, AGE, NEDUGRP, NEDUGRP1, NAGEGRP, NCOUNTRY, INDSECT,                    # individual characteristics and new groupings 
    PARTNERNO, COUPLENO,                                                        # couple number and partner's number index for couples within household 
    anyChildren, child16, child15, child4,                                      # control variable for presence of children
    PAGE, PEDU1, PAGEGRP, PEDUGRP1                                              # edu and age of partners and partner's grouping, for assortativeness reporting 
  ) %>%
  rowwise() %>%
  mutate(
    CASENOP = sum(HSERIALP*1000, PERSNO)
  ) %>%
  distinct(CASENOP, .keep_all=T) 

INCOME2014B <- INCOME2014 %>%
  select(
    HSERIALP, PERSNO,                                                           # identification of individuals 
    NETWKTOT,                                                                   # our income variable 
    SEX, AGE, NEDUGRP, NEDUGRP1, NAGEGRP, NCOUNTRY, INDSECT,                    # individual characteristics and new groupings 
    PARTNERNO, COUPLENO,                                                        # couple number and partner's number index for couples within household 
    anyChildren, child16, child15, child4,                                      # control variable for presence of children
    PAGE, PEDU1, PAGEGRP, PEDUGRP1,                                             # edu and age of partners and partner's grouping, for assortativeness reporting 
    CASENOP  
  ) %>% 
  rowwise() %>%
  mutate(
    HSERIALP = as.numeric(HSERIALP),                                            # this dataset has HSERIALP as character. 
    CASENOP = as.numeric(CASENOP)
  ) %>% ungroup() %>%
  rowwise() %>%
  mutate(
    year=2014
  )


INCOME2014B %>% count(CASENOP)     # check 

## first check if there are duplicate CASENOP between uktus and INCOME2014 datasets
practice <- INCOME2014B %>% group_by(CASENOP) %>% add_tally
any(practice$n>1)                                                               # must return [False] so that we can use CASENOP as unique individual identifier 
rm(practice)                                                                    # to prevent any future confusion 

## now bind the two 
## and check if there are any duplicate in CASENOP so that we can make use of it as a unique individual identifier
practice <- adult_allB %>% bind_rows(INCOME2014B) %>% group_by(CASENOP) %>% 
  add_tally(., name="duplicate") 
any(practice$duplicate>1) # must return [false]
rm(practice)

## now bind the two with the name of the df to be used 
INCOME2014B <- adult_allB %>% bind_rows(INCOME2014B) %>% 
  distinct(CASENOP, .keep_all=T)




# year 2014 total, assortativeness in equilibrium: for reporting purpose 
## AGE sorting 
age_2014F <- INCOME2014B %>%
  filter(!is.na(PARTNERNO), !is.na(COUPLENO)) %>%                               # restrict to couples only 
  filter(SEX==2) %>%                                                            # for female only; this is necessary and sufficient to calculate assortation by 'ratioage'
  rowwise() %>%
  mutate(
    MM_AGE=NAGEGRP*PAGEGRP
  ) %>%                                                                       # marriage market by age = indiv's age group x partner's age group = 16 subgroups with different results
  group_by(NAGEGRP) %>% add_tally(., name="AGEGRPSUB") %>%                      # mutate number of female per age grp
  group_by(NAGEGRP, MM_AGE) %>% add_tally(., name="MM_AGESUB") %>%              # mutate number of female per age marr. market
  select(HSERIALP, PERSNO, MM_AGESUB, AGEGRPSUB, PAGEGRP, 
         NAGEGRP, MM_AGE) %>%
  rowwise() %>%
  mutate(RATIOAGE=(MM_AGESUB/AGEGRPSUB)) %>%                                    # mutate ratio per age marr. market
  distinct(NAGEGRP, PAGEGRP, .keep_all=T) %>% 
  select(NAGEGRP, PAGEGRP, MM_AGE, MM_AGESUB, AGEGRPSUB, RATIOAGE)              # for reporting 



## EDU sorting
edu_2014F <- INCOME2014B %>% 
  filter(!is.na(COUPLENO), !is.na(PARTNERNO)) %>% 
  filter(SEX==2) %>%                                                            # restrict to each gender for calculation purpose (identify marr. market)
  mutate(
    MM_EDU = NEDUGRP1*PEDUGRP1
  ) %>%                                                                         # marriage market by edu = indiv's age group x partner's age group = 16 subgroups with different results 
  group_by(NEDUGRP1) %>% add_tally(., name="EDUGRPSUB") %>%                     # mutate number of female per edu grp
  group_by(NEDUGRP1, MM_EDU) %>% add_tally(., name="MM_EDUSUB") %>%             # mutate number of female per edu marr. market
  select(HSERIALP, PERSNO, MM_EDUSUB, EDUGRPSUB, PEDUGRP1) %>%
  rowwise() %>% 
  mutate(RATIOEDU=(MM_EDUSUB/EDUGRPSUB))  %>%                                   # mutate ratio per edu marr. market
  arrange(HSERIALP) %>%
  distinct(NEDUGRP1, PEDUGRP1, .keep_all=T) %>%
  select(NEDUGRP1, PEDUGRP1, MM_EDU, MM_EDUSUB, EDUGRPSUB, RATIOEDU)



# grouping by marriage market 
## for year 2014, total 
INCOME2014B <- INCOME2014B %>%
  mutate(mmgrp=case_when(                                                       # 3 edugrps, 4 agegrps, 4 regional groups
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==1 ~ 'A1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==1 ~ 'A2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==1 ~ 'A3',
    
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==2 ~ 'B1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==2 ~ 'B2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==2 ~ 'B3',
    
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C3',
    
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==4 ~ 'D1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==1 ~ 'E1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==1 ~ 'E2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==1 ~ 'E3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==2 ~ 'F1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==2 ~ 'F2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==2 ~ 'F3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==3 ~ 'G1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==3 ~ 'G2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==3 ~ 'G3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==4 ~ 'H1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==4 ~ 'H2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==4 ~ 'H3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==1 ~ 'I1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==1 ~ 'I2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==1 ~ 'I3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==2 ~ 'J1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==2 ~ 'J2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==2 ~ 'J3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==3 ~ 'K1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==3 ~ 'K2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==3 ~ 'K3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==4 ~ 'L1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==4 ~ 'L2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==4 ~ 'L3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==1 ~ 'M1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==1 ~ 'M2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==1 ~ 'M3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==2 ~ 'N1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==2 ~ 'N2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==2 ~ 'N3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==3 ~ 'O1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==3 ~ 'O2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==3 ~ 'O3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==4 ~ 'P1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==4 ~ 'P2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==4 ~ 'P3',
    
    TRUE ~ '0'
  ))


## for year 2008
INCOME2008 <- INCOME2008 %>%
  mutate(mmgrp=case_when(                                                       # 3 edugrps, 4 agegrps, 4 regional groups
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==1 ~ 'A1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==1 ~ 'A2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==1 ~ 'A3',
    
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==2 ~ 'B1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==2 ~ 'B2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==2 ~ 'B3',
    
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C3',
    
    NEDUGRP==1 & NAGEGRP==1 & NCOUNTRY==4 ~ 'D1',
    NEDUGRP==2 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C2',
    NEDUGRP==3 & NAGEGRP==1 & NCOUNTRY==3 ~ 'C3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==1 ~ 'E1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==1 ~ 'E2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==1 ~ 'E3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==2 ~ 'F1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==2 ~ 'F2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==2 ~ 'F3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==3 ~ 'G1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==3 ~ 'G2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==3 ~ 'G3',
    
    NEDUGRP==1 & NAGEGRP==2 & NCOUNTRY==4 ~ 'H1',
    NEDUGRP==2 & NAGEGRP==2 & NCOUNTRY==4 ~ 'H2',
    NEDUGRP==3 & NAGEGRP==2 & NCOUNTRY==4 ~ 'H3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==1 ~ 'I1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==1 ~ 'I2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==1 ~ 'I3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==2 ~ 'J1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==2 ~ 'J2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==2 ~ 'J3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==3 ~ 'K1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==3 ~ 'K2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==3 ~ 'K3',
    
    NEDUGRP==1 & NAGEGRP==3 & NCOUNTRY==4 ~ 'L1',
    NEDUGRP==2 & NAGEGRP==3 & NCOUNTRY==4 ~ 'L2',
    NEDUGRP==3 & NAGEGRP==3 & NCOUNTRY==4 ~ 'L3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==1 ~ 'M1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==1 ~ 'M2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==1 ~ 'M3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==2 ~ 'N1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==2 ~ 'N2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==2 ~ 'N3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==3 ~ 'O1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==3 ~ 'O2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==3 ~ 'O3',
    
    NEDUGRP==1 & NAGEGRP==4 & NCOUNTRY==4 ~ 'P1',
    NEDUGRP==2 & NAGEGRP==4 & NCOUNTRY==4 ~ 'P2',
    NEDUGRP==3 & NAGEGRP==4 & NCOUNTRY==4 ~ 'P3',
    
    TRUE ~ '0'
  ))


# grouping by demographic group
## for year 2014
INCOME2014B <- INCOME2014B %>% 
  mutate(dg = case_when(                                                        #3 edugrps, 4 agegrps, 9 indsect grps. 
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==1 ~ 'A1',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==2 ~ 'A2',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==3 ~ 'A3',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==4 ~ 'A4',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==5 ~ 'A5',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==6 ~ 'A6',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==7 ~ 'A7',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==8 ~ 'A8',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==9 ~ 'A9',
    
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==1 ~ 'A10',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==2 ~ 'A11',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==3 ~ 'A12',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==4 ~ 'A13',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==5 ~ 'A14',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==6 ~ 'A15',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==7 ~ 'A16',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==8 ~ 'A17',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==9 ~ 'A18',
    
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==1 ~ 'A19',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==2 ~ 'A20',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==3 ~ 'A21',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==4 ~ 'A22',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==5 ~ 'A23',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==6 ~ 'A24',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==7 ~ 'A25',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==8 ~ 'A26',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==9 ~ 'A27',
    
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==1 ~ 'A28',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==2 ~ 'A29',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==3 ~ 'A30',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==4 ~ 'A31',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==5 ~ 'A32',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==6 ~ 'A33',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==7 ~ 'A34',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==8 ~ 'A35',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==9 ~ 'A36',
    
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==1 ~ 'B1',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==2 ~ 'B2',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==3 ~ 'B3',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==4 ~ 'B4',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==5 ~ 'B5',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==6 ~ 'B6',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==7 ~ 'B7',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==8 ~ 'B8',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==9 ~ 'B9',
    
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==1 ~ 'B10',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==2 ~ 'B11',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==3 ~ 'B12',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==4 ~ 'B13',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==5 ~ 'B14',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==6 ~ 'B15',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==7 ~ 'B16',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==8 ~ 'B17',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==9 ~ 'B18',
    
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==1 ~ 'B19',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==2 ~ 'B20',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==3 ~ 'B21',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==4 ~ 'B22',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==5 ~ 'B23',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==6 ~ 'B24',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==7 ~ 'B25',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==8 ~ 'B26',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==9 ~ 'B27',
    
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==1 ~ 'B28',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==2 ~ 'B29',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==3 ~ 'B30',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==4 ~ 'B31',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==5 ~ 'B32',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==6 ~ 'B33',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==7 ~ 'B34',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==8 ~ 'B35',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==9 ~ 'B36',
    
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==1 ~ 'C1',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==2 ~ 'C2',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==3 ~ 'C3',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==4 ~ 'C4',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==5 ~ 'C5',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==6 ~ 'C6',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==7 ~ 'C7',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==8 ~ 'C8',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==9 ~ 'C9',
    
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==1 ~ 'C10',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==2 ~ 'C11',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==3 ~ 'C12',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==4 ~ 'C13',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==5 ~ 'C14',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==6 ~ 'C15',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==7 ~ 'C16',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==8 ~ 'C17',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==9 ~ 'C18',
    
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==1 ~ 'C19',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==2 ~ 'C20',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==3 ~ 'C21',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==4 ~ 'C22',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==5 ~ 'C23',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==6 ~ 'C24',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==7 ~ 'C25',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==8 ~ 'C26',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==9 ~ 'C27',
    
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==1 ~ 'C28',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==2 ~ 'C29',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==3 ~ 'C30',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==4 ~ 'C31',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==5 ~ 'C32',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==6 ~ 'C33',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==7 ~ 'C34',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==8 ~ 'C35',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==9 ~ 'C36',
    
    TRUE ~ '0'
  ))



## for year 2008
INCOME2008 <- INCOME2008 %>% 
  mutate(dg = case_when(                                                        #3 edugrps, 4 agegrps, 9 indsect grps. 
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==1 ~ 'A1',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==2 ~ 'A2',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==3 ~ 'A3',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==4 ~ 'A4',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==5 ~ 'A5',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==6 ~ 'A6',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==7 ~ 'A7',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==8 ~ 'A8',
    NEDUGRP==1 & NAGEGRP==1 & INDSECT==9 ~ 'A9',
    
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==1 ~ 'A10',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==2 ~ 'A11',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==3 ~ 'A12',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==4 ~ 'A13',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==5 ~ 'A14',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==6 ~ 'A15',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==7 ~ 'A16',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==8 ~ 'A17',
    NEDUGRP==1 & NAGEGRP==2 & INDSECT==9 ~ 'A18',
    
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==1 ~ 'A19',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==2 ~ 'A20',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==3 ~ 'A21',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==4 ~ 'A22',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==5 ~ 'A23',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==6 ~ 'A24',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==7 ~ 'A25',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==8 ~ 'A26',
    NEDUGRP==1 & NAGEGRP==3 & INDSECT==9 ~ 'A27',
    
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==1 ~ 'A28',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==2 ~ 'A29',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==3 ~ 'A30',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==4 ~ 'A31',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==5 ~ 'A32',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==6 ~ 'A33',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==7 ~ 'A34',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==8 ~ 'A35',
    NEDUGRP==1 & NAGEGRP==4 & INDSECT==9 ~ 'A36',
    
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==1 ~ 'B1',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==2 ~ 'B2',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==3 ~ 'B3',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==4 ~ 'B4',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==5 ~ 'B5',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==6 ~ 'B6',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==7 ~ 'B7',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==8 ~ 'B8',
    NEDUGRP==2 & NAGEGRP==1 & INDSECT==9 ~ 'B9',
    
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==1 ~ 'B10',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==2 ~ 'B11',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==3 ~ 'B12',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==4 ~ 'B13',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==5 ~ 'B14',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==6 ~ 'B15',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==7 ~ 'B16',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==8 ~ 'B17',
    NEDUGRP==2 & NAGEGRP==2 & INDSECT==9 ~ 'B18',
    
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==1 ~ 'B19',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==2 ~ 'B20',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==3 ~ 'B21',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==4 ~ 'B22',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==5 ~ 'B23',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==6 ~ 'B24',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==7 ~ 'B25',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==8 ~ 'B26',
    NEDUGRP==2 & NAGEGRP==3 & INDSECT==9 ~ 'B27',
    
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==1 ~ 'B28',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==2 ~ 'B29',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==3 ~ 'B30',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==4 ~ 'B31',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==5 ~ 'B32',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==6 ~ 'B33',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==7 ~ 'B34',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==8 ~ 'B35',
    NEDUGRP==2 & NAGEGRP==4 & INDSECT==9 ~ 'B36',
    
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==1 ~ 'C1',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==2 ~ 'C2',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==3 ~ 'C3',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==4 ~ 'C4',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==5 ~ 'C5',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==6 ~ 'C6',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==7 ~ 'C7',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==8 ~ 'C8',
    NEDUGRP==3 & NAGEGRP==1 & INDSECT==9 ~ 'C9',
    
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==1 ~ 'C10',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==2 ~ 'C11',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==3 ~ 'C12',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==4 ~ 'C13',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==5 ~ 'C14',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==6 ~ 'C15',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==7 ~ 'C16',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==8 ~ 'C17',
    NEDUGRP==3 & NAGEGRP==2 & INDSECT==9 ~ 'C18',
    
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==1 ~ 'C19',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==2 ~ 'C20',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==3 ~ 'C21',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==4 ~ 'C22',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==5 ~ 'C23',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==6 ~ 'C24',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==7 ~ 'C25',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==8 ~ 'C26',
    NEDUGRP==3 & NAGEGRP==3 & INDSECT==9 ~ 'C27',
    
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==1 ~ 'C28',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==2 ~ 'C29',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==3 ~ 'C30',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==4 ~ 'C31',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==5 ~ 'C32',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==6 ~ 'C33',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==7 ~ 'C34',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==8 ~ 'C35',
    NEDUGRP==3 & NAGEGRP==4 & INDSECT==9 ~ 'C36',
    
    TRUE ~ '0'
  ))




# instrument variable
##instrument wage variable without percentile (non-bartik)
## grouping by 0) new edu/age/region grouping, 1) demographic group, 2) marriage market, and 3) ratio for instrumenting avg wage

# 1) (pred wage construction) ratio (proportion) group: for base year (2008)
## ratio from year 2008, which is the base year 
PROPORTION <- INCOME2008 %>% 
  group_by(INDSECT) %>% add_tally(., name='n_j') %>%
  group_by(INDSECT, SEX, NEDUGRP, NCOUNTRY) %>% add_tally(., name='n_jg') %>%
  group_by(NEDUGRP, NCOUNTRY) %>% add_tally(., name='n_er') %>%
  rowwise() %>%
  mutate(
    ratio_jg=n_jg/n_j,                                                          # this is NOT the variable we are going to use to construct predicted average wage
    ratio_er=n_jg/n_er                                                          # but this! (N_(ind x gender x edu x region)/N_(edu x region): gender-specific effect)
  ) %>% 
  ungroup() %>% 
  distinct(INDSECT, NEDUGRP, NCOUNTRY, SEX, n_jg, n_er, ratio_jg, ratio_er) 

INCOME2014B <- INCOME2014B %>% full_join(PROPORTION) 
# check: PROPORTION %>% count(ratio_jg)


INCOME2008 %>% count(year) # check if INCOME2008 has any time variable indicating the year 
class(INCOME2008$CASENOP)
class(INCOME2008$HSERIALP) # check if both of the vars here are of same class witht hose in adult_all for full_join : character. 

INCOME2008 <- INCOME2008 %>% 
  group_by(INDSECT) %>% add_tally(., name='n_j') %>% 
  rowwise() %>% 
  group_by(INDSECT, SEX, NEDUGRP, NCOUNTRY) %>% add_tally(., name='n_jg') %>%
  ungroup() %>% rowwise() %>% 
  group_by(NEDUGRP, NCOUNTRY) %>% add_tally(., name='n_er') %>% ungroup() %>% 
  rowwise() %>%
  mutate(
    ratio_jg=n_jg/n_j,
    ratio_er=n_jg/n_er
  ) %>%
  ungroup() %>%
  distinct(CASENOP, .keep_all=T) %>%
  rowwise() %>%
  mutate(
    year=2008, 
    CASENOP = as.numeric(CASENOP), 
    HSERIALP = as.numeric(HSERIALP)
  ) %>% ungroup()


# 2) (pred wage construction) demographic group and (actual) marriage market group

## for year 2008
w_pred08 <- INCOME2008 %>%
  rowwise() %>%
  group_by(INDSECT, SEX, NEDUGRP, NAGEGRP) %>% nest() %>%
  mutate(
    w_dg08 = map(data, ~mean(.x$NETWKTOT)) 
  ) %>%
  unnest(cols=c(data, w_dg08)) %>% ungroup() %>% 
  rowwise() %>%
  mutate(
    w_mtpred08jg = mean(w_dg08*ratio_jg),
    w_mtpred08er = mean(w_dg08*ratio_er)
  ) %>% ungroup() %>% 
  rowwise() %>% 
  group_by(NCOUNTRY, NEDUGRP, NAGEGRP, SEX) %>% nest() %>%
  mutate(
    w_mtpred08jg2 = map(data, ~sum(.x$w_mtpred08jg)),
    w_mtpred08er2 = map(data, ~sum(.x$w_mtpred08er)),
    w_mtpred08jg_p = map(data, ~sum((.x$w_dg08)*(.x$ratio_jg))), 
    w_mtpred08er_p = map(data, ~sum((.x$w_dg08)*(.x$ratio_er)))
  ) %>%
  unnest(cols=c(
    data, w_mtpred08jg2, w_mtpred08er2, w_mtpred08jg_p, w_mtpred08er_p)) %>% 
  ungroup()

#check_pred08 %>% group_by(w_mtpred2) %>% tally

w_act08 <- INCOME2008 %>% 
  group_by(NEDUGRP, NAGEGRP, NCOUNTRY, INDSECT, SEX) %>% nest() %>%
  mutate(
    w_mtact08 = map(data, ~mean(.x$NETWKTOT))
  ) %>%
  unnest(cols=c(data, w_mtact08)) %>% 
  rowwise() %>%
  group_by(NEDUGRP, NAGEGRP, NCOUNTRY, SEX) %>% nest() %>%
  mutate(
    w_mtact08_2 = map(data, ~mean(.x$NETWKTOT))
  ) %>%
  unnest(cols=c(data, w_mtact08_2)) %>% ungroup()

#check 
#select(NEDUGRP, NAGEGRP, INDSECT, NCOUNTRY, SEX, ratio_jg, NETWKTOT,
#w_mtact08) %>%
#distinct(NEDUGRP, NAGEGRP, INDSECT, NCOUNTRY, SEX, w_mtact08)

check_2008 <- w_act08 %>% full_join(w_pred08) 


## for year 2014
w_pred14 <- INCOME2014B %>% 
  rowwise() %>% group_by(NEDUGRP, NAGEGRP, INDSECT, SEX) %>% nest() %>%
  mutate(
    w_dg14 = map(data, ~mean(.x$NETWKTOT)) 
  ) %>%
  unnest(cols=c(data, w_dg14)) %>% ungroup() %>% 
  rowwise() %>%
  mutate(
    w_mtpred14jg_raw=w_dg14*ratio_jg
  ) %>%
  rowwise() %>%
  mutate(
    w_mtpred14jg = mean(w_dg14*ratio_jg),
    w_mtpred14er = mean(w_dg14*ratio_er)
  ) %>% ungroup() %>% 
  rowwise() %>%
  group_by(SEX, NEDUGRP, NAGEGRP, NCOUNTRY) %>% nest() %>%
  mutate(
    w_mtpred14jg2 = map(data, ~sum(.x$w_mtpred14jg)),
    w_mtpred14er2 = map(data, ~sum(.x$w_mtpred14er)),
    w_mtpred14jg_p = map(data, ~sum((.x$w_dg14)*(.x$ratio_jg))),
    w_mtpred14er_p = map(data, ~sum((.x$w_dg14)*(.x$ratio_er)))
  ) %>%
  unnest(cols=c(data, w_mtpred14jg2, w_mtpred14er2, w_mtpred14jg_p, w_mtpred14er_p)) %>% ungroup()


w_act14 <- INCOME2014B %>% 
  rowwise() %>% 
  group_by(NCOUNTRY, NEDUGRP, NAGEGRP, SEX, INDSECT) %>% nest() %>%
  mutate(
    w_mtact14 = map(data, ~mean(.x$NETWKTOT))
  ) %>%
  unnest(cols=c(data, w_mtact14)) %>% ungroup() %>% 
  group_by(NCOUNTRY, NEDUGRP, NAGEGRP, SEX) %>% nest() %>%
  mutate(
    w_mtact14_2 = map(data, ~mean(.x$NETWKTOT))
  ) %>%
  unnest(cols=c(data, w_mtact14_2)) %>% ungroup()


check_2014 <- w_act14 %>% full_join(w_pred14) 
#2008: sex, nedugrp, nagegrp, ncountry, indsect, casenop, hserialp, persno, relhrp6, famunit, tothrs, netwk, netwk2, age, ages, marsta, hiqual8d, hiqual8, country, netwk2n, tothrsn, netwktot, nedugrp1, mmgrp, n_j, n_jg, n_er, ratio_jg, ratio_er, w_mtact08, w_mtact08_2, w_dg08, w_mtpred08_1, w_mtpred08_2, w_mtpred08_3, w_mtpred08_4, w_mtpred08_p1, w_mtpred08_p2, year, 
#2014: sex, nedugrp, nagegrp, ncountry, indsect, casenop, hserialp, persno, coupleno, partnerno, relhrp6, casenop, famunit, tothrs, netwk, netwk2, age, ages, marsta, hiqul11d, hiqual11, country, in0792em, netwk2n, tothrsn, netwktot, couplecheck, persnosum, numind, inchh, pwthh, incsh, pwtsh, nedugrp1, mmgrp, n_j, n_jg, ratio_jg, ratio_er, w_mtact14, w_dg14, w_mtpred14, w_mtpred14_1, w_mtpred14_3, w_mtpred14_4, w_mtpred14_p1, w_mtpred14_p2, year 


check_2008b <- check_2008 %>%
  select(SEX, NEDUGRP, NEDUGRP1, NAGEGRP, NCOUNTRY, INDSECT, CASENOP, HSERIALP, 
         PERSNO, NETWKTOT, mmgrp, n_jg, n_er, ratio_jg, ratio_er, 
         w_mtact08, w_mtact08_2, w_dg08, w_mtpred08jg, w_mtpred08er,  
         w_mtpred08jg2, w_mtpred08er2, w_mtpred08jg_p, w_mtpred08er_p, 
         year) # is this selection necessary? why not keep it broader?? ==> because of different no. of vars.  
check_2008b <- rename(check_2008b, w_mtact=w_mtact08, w_mtact2=w_mtact08_2, 
                      w_mtpredjg=w_mtpred08jg, w_mtpreder=w_mtpred08er, 
                      w_mtpredjg2= w_mtpred08jg2, w_mtpreder2 = w_mtpred08er2, 
                      w_mtpredjg_p = w_mtpred08jg_p, 
                      w_mtpreder_p = w_mtpred08er_p, 
                      w_dg=w_dg08) # not only for the easier calculation, but also for the binding with 2014 year dataset. 

check_2014b <- check_2014 %>%
  select(SEX, NEDUGRP, NEDUGRP1, NAGEGRP, NCOUNTRY, INDSECT, CASENOP, HSERIALP, 
         PERSNO, NETWKTOT, mmgrp, n_jg, n_er, ratio_jg, ratio_er, 
         w_mtact14, w_mtact14_2, w_dg14, w_mtpred14jg, w_mtpred14er, 
         w_mtpred14jg2, w_mtpred14er2, w_mtpred14jg_p, w_mtpred14er_p, 
         year)
check_2014b <- rename(check_2014b, w_mtact=w_mtact14, w_mtact2=w_mtact14_2, 
                      w_mtpredjg=w_mtpred14jg, w_mtpreder=w_mtpred14er, 
                      w_mtpredjg2= w_mtpred14jg2, w_mtpreder2 = w_mtpred14er2, 
                      w_mtpredjg_p = w_mtpred14jg_p, 
                      w_mtpreder_p = w_mtpred14er_p,  
                      w_dg=w_dg14)

check_both <- check_2008b %>% bind_rows(check_2014b)


## first stage, non-bartik 

lm(w_mtact ~ ratio_jg*w_dg+factor(year)+factor(mmgrp)-1, check_both) %>% summary()
lm(w_mtact ~ ratio_er*w_dg+factor(year)+factor(mmgrp)-1, check_both) %>% summary()
lm(w_mtact ~ w_mtpreder+factor(year)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpredjg+factor(year)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpreder_p + factor(year)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpredjg_p + factor(year)+factor(mmgrp)-1, check_both) %>% summary

lm(w_mtact2 ~ ratio_er*w_dg + factor(year)-1, check_both) %>% summary 
lm(w_mtact2 ~ w_mtpreder2 + factor(year)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpredjg2 + factor(year)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpreder_p + factor(year)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpredjg_p + factor(year)+factor(mmgrp)-1, check_both) %>% summary

lm(w_mtact14 ~ w_mtpred14er+factor(mmgrp)-1, check_2014) %>% summary #low rsqard, but coeff are still high enough, above 1, and significant.
lm(w_mtact08 ~ w_mtpred08er+factor(mmgrp)-1, check_2008) %>% summary # low rsqrd, but coeff are still high enoug, above 1, and significant.


lm(w_mtact ~ w_mtpreder+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpreder2 + factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpreder2+factor(year)+ factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpreder + factor(year)+ factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+ factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_dg*ratio_er+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_dg*ratio_er+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpreder_p+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpreder_p+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary


lm(w_mtact14 ~ ratio_jg*w_dg14, check_2014) %>% summary # first model; r-sqrd=0.94; coef sign; coef=0.1561
lm(w_mtact14 ~ ratio_er*w_dg14, check_2014) %>% summary # r-sqrd=0.9396; coef not significant, coef=-0.002901
lm(w_mtact14 ~ w_mtpred14, check_2014) %>% summary # r-squared too small (0.03865)
lm(w_mtact14 ~ w_mtpred14_2, check_2014) %>% summary # r-squared too small (0.03229); coef significant; coef=0.71276
lm(w_mtact14 ~ w_mtpred14_3, check_2014) %>% summary # r-squared too small (0.00409); coef significant; coef=0.1153
lm(w_mtact14 ~ w_mtpred14_4, check_2014) %>% summary # r-squared too small (0.02274); coef significant; coef<0.001
lm(w_mtact14_2 ~ ratio_jg*w_dg14, check_2014) %>% summary # r-squared too small (0.7499); coef significant; coef<0.5
lm(w_mtact14_2 ~ ratio_er*w_dg14, check_2014) %>% summary # r-squared too small (0.7722); coef significant; coef=1.55
lm(w_mtact14_2 ~ w_mtpred14_1, check_2014) %>% summary # r-squared too small (0.01808); coef significant; coef=0.46295
lm(w_mtact14_2 ~ w_mtpred14_2, check_2014) %>% summary # r-squared too small (0.0198); coef significant; coef=0.22022
lm(w_mtact14_2 ~ w_mtpred14_3, check_2014) %>% summary # r-squared too small (0.01401); coef significant; coef<0.001
lm(w_mtact14_2 ~ w_mtpred14_4, check_2014) %>% summary # r-squared too small (0.03041); coef significant; coef<0.001

lm(w_mtact14 ~ w_mtpred14_p1, check_2014) %>% summary # four models below from here have r squared value too small, and coeff as well. 
lm(w_mtact14 ~ w_mtpred14_p2, check_2014) %>% summary
lm(w_mtact14_2 ~ w_mtpred14_p1, check_2014) %>% summary
lm(w_mtact14_2 ~ w_mtpred14_p2, check_2014) %>% summary


lm(w_mtact08 ~ ratio_jg*w_dg08, check_2008) %>% summary() # first model; r-sqrd=0.9219, p<0.001, coeff significant; coef=0.451482
lm(w_mtact08 ~ ratio_er*w_dg08, check_2008) %>% summary # r sqrd =0.9198, but coef significant at <0.001; coeff=0.101596
lm(w_mtact08 ~ w_mtpred08_1, check_2008) %>% summary # r-squared too small (0.1907)
lm(w_mtact08 ~ w_mtpred08_2, check_2008) %>% summary # r-squared too small (0.06676)
lm(w_mtact08 ~ w_mtpred08_3, check_2008) %>% summary # r-squared too small (0.02862)
lm(w_mtact08 ~ w_mtpred08_4, check_2008) %>% summary # r-squared too small (0.06022)
lm(w_mtact08_2 ~ ratio_jg*w_dg08, check_2008) %>% summary # r-sqrd=0.7702, p<0.001, coef significant but not as much as first; 
lm(w_mtact08_2 ~ ratio_er*w_dg08, check_2008) %>% summary # r squared is less than first model (0.7831), but highest coeff (1.429)
lm(w_mtact08_2 ~ w_mtpred08_1, check_2008) %>% summary # r squared is too small (0.1385)
lm(w_mtact08_2 ~ w_mtpred08_2, check_2008) %>% summary # r squared is too small (0.07781)
lm(w_mtact08_2 ~ w_mtpred08_3, check_2008) %>% summary # r squared is too small (0.03799)
lm(w_mtact08_2 ~ w_mtpred08_4, check_2008) %>% summary # r-squared too small (0.07995)

lm(w_mtact08 ~ w_mtpred08_p1, check_2008) %>% summary # four models below from here have r squared value too small;
lm(w_mtact08 ~ w_mtpred08_p2, check_2008) %>% summary
lm(w_mtact08_2 ~ w_mtpred08_p1, check_2008) %>% summary
lm(w_mtact08_2 ~ w_mtpred08_p2, check_2008) %>% summary


check_2008f <- check_2008 %>% filter(SEX==2)
check_2008m <- check_2008 %>% filter(SEX==1)
lm(w_mtact08 ~ w_dg08*ratio_jg, check_2008f) %>% summary #(R^2==0.9313, p<0.001, coeff==0.021)<- not significant  
lm(w_mtact08 ~ w_dg08*ratio_er, check_2008f) %>% summary #(R^2==0.9313, p<0.001, coeff==-0.016676)<- not significant 
lm(w_mtact08 ~ w_mtpred08er2, check_2008f) %>% summary #(R^2==0.1076, p<0.001, coeff==1.18362)<- significant
lm(w_mtact08 ~ w_mtpred08_2, check_2008f) %>% summary #(R^2==0.2112, p<0.001, coeff==0.7461)<- significant
lm(w_mtact08 ~ w_mtpred08_3, check_2008f) %>% summary #(R^2<0.009312, p<0.001, coeff<0.0001)<- significant
lm(w_mtact08 ~ w_mtpred08_4, check_2008f) %>% summary #(R^2==0.2719, p<0.001, coeff<0.0001)<- significant
lm(w_mtact08_2 ~  ratio_jg*w_dg08, check_2008f) %>% summary #(R^2==0.777, p<0.001, coeff=1.228)<- significant
lm(w_mtact08_2 ~  ratio_er*w_dg08, check_2008f) %>% summary #(R^2==0.8031, p<0.001, coeff=1.423)<- significant
rm(check_2008f)
# better to do with panel data model with marriage market fixed effects. 

check_bothf <- check_both %>% filter(SEX==2) 
check_bothm <- check_both %>% filter(SEX==1) 

lm(w_mtact ~ w_mtpreder + factor(year)-1, check_bothf) %>% summary # coeff=0.99452, very significant, rsqrd0.8524
lm(w_mtact ~ w_mtpreder + factor(year)-1, check_bothm) %>% summary # coeff=03.85995, very significant, rsqrd0.8755


# check fixed effect, time-fixed effect, random effect (marriage-market and year)



# explore data
coplot(NETWKTOT ~ SEX|NEDUGRP, type="l", check_both)
coplot(NETWKTOT ~ SEX|NAGEGRP, type="b", check_both)
scatterplot(NETWKTOT ~ SEX|NEDUGRP, boxplots=F, smooth=T, reg.line=F, check_both)

library(gplots)
plotmeans(NETWKTOT ~ mmgrp, main="heterogeneity across mmgrp", INCOME2008)
plotmeans(NETWKTOT ~ mmgrp, main="heterogeneity across mmgrp", check_both)
plotmeans(w_mtact ~ mmgrp, main="heterogeneity across mmgrp", check_both)
plotmeans(w_mtpreder_p ~ mmgrp, main="heterogeneity across sex", check_both)
detach("package:gplots")


# OLS regression
ols <- lm(w_mtact08_2 ~ ratio_er*w_dg08, check_2008)
summary(ols)
yhat <- ols$fitted
plot(check_2008$w_mtact08_2, check_2008$ratio_er*check_2008$w_dg08, xlab="pred", ylab="actual")
abline(lm(check_2008$w_mtact08_2 ~ check_2008$ratio_er*check_2008$w_dg08), lwd=3, col="red")


ols2 <- lm(y2~ratio_er*x0, check_both)  
summary(ols2) #r-sqrd=0.7799, coeff=1.407, significant 


# fixed effects using LSDV
fixed.dum <- lm(w_mtact08_2 ~ ratio_er*w_dg08 + factor(mmgrp)-1, check_2008)
summary(fixed.dum) # coeff<0.7, rsqrd=0.9813

fixed.dum.new <- lm(y2 ~ ratio_er*x0+factor(mmgrp)-1, check_both)
summary(fixed.dum.new) # rsqrd=0.9801, coeff=0.9806, significant. 

fixed.dum.new2 <- lm(log(y2)~ratio_er*log(x0) + factor(mmgrp)+factor(year)-1, check_both)
summary(fixed.dum.new2) #rsqrd=0.9994, coeff=0.722541, significant. 

fixed.dum.new3 <- lm(log(y2)~log(x2)+factor(mmgrp)+factor(year)-1, check_both)
summary(fixed.dum.new3) # rsqrd=0.9984, coeff=-0.015588, significant. 

fixed.dum.new4 <- lm(log(y2)~log(x3)+factor(mmgrp)+factor(year)-1, check_both)
summary(fixed.dum.new4) # rsqrd=0.9983, coeff=-0.066251, significant. 


fixed.dum2 <- lm(w_mtact08_2 ~ w_mtpred08_1 + factor(mmgrp)-1, check_2008) #%>% summary() #coef=1.08002; rsqrd=0.9596
fixed.dum3 <- lm(w_mtact08_2 ~ w_mtpred08_2 + factor(mmgrp)-1, check_2008) #coef=-0.48290; rsqrd=0.9552
lm(w_mtact08_2 ~ w_mtpred08_3 + factor(mmgrp)-1, check_2008) %>% summary #coef<-0.001; rsqrd=0.9728
fixed.dum4 <- lm(w_mtact08_2 ~ w_mtpred08_4 + factor(mmgrp)-1, check_2008) #coef<-0.001; rsqrd=0.9698
lm(w_mtact08_2 ~ w_mtpred08_p1 + factor(mmgrp)-1, check_2008) %>% summary #coef<0.001; rsqrd=0.9728
fixed.dum5 <- lm(w_mtact08_2 ~ w_mtpred08_p2 + factor(mmgrp)-1, check_2008) #coef<-0.001; rsqrd=0.9698


yhat <- fixed.dum$fitted
yhat2 <- fixed.dum2$fitted
scatterplot(yhat ~ check_2008$w_mtpred08_1|check_2008$mmgrp, boxplots=F, xlab="actual", ylab='yhat', smooth=F)
abline(lm(check_2008$w_mtact08_2 ~ check_2008$w_mtpred08_2), lwd=3, col="red")

yhat.new <- fixed.dum.new$fitted
scatterplot(yhat.new~check_both$y2|check_both$mmgrp, boxplots=F, xlab="actual", ylab="yhat", smooth=F)
abline(lm(check_both$y2 ~ check_both$x2), lwd=3, col="red")

tab_model(ols, fixed.dum, fixed.dum2, fixed.dum3, fixed.dum4, fixed.dum5, auto.label = FALSE, show.ci = FALSE)

tab_model(ols2, fixed.dum.new, auto.label=T, show.ci=F, collapse.se=T, p.style = "stars")


# defining labels for the reported table 
tab_model(
  m1, m2, 
  pred.labels = c("Intercept", "Age (Carer)", "Hours per Week", "Gender (Carer)",
                  "Education: middle (Carer)", "Education: high (Carer)", 
                  "Age (Older Person)"),
  dv.labels = c("First Model", "M2"),
  string.pred = "Coeffcient",                                                    
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)



# fixed effects using plm (panel data estimators)
fixed <- plm(w_mtact08_2 ~ ratio_er*w_dg08, data=check_2008, index=c("mmgrp"), model="within")
summary(fixed)
fixed2 <- plm(w_mtact08_2 ~ w_mtpred08_p2, data=check_2008, index=c("mmgrp"), model="within")
summary(fixed2)
fixef(fixed)

pFtest(fixed, ols) # p-value<0.001
pFtest(fixed2, ols) #p-value==1, should not use w_mtpred08_4 fixed effect model
## use ratio_er*w_dg08. 


fixed.new <- plm(y2 ~ x0*ratio_er, check_both, index=c("mmgrp"), model="within")
summary(fixed.new) # coef=0.098059, significant; rsqrd=0.61148
fixed.new2 <- plm(y2 ~ x2, check_both, index=c("mmgrp"), model="within")
summary(fixed.new2) # not intuitive; coeff<0; rsqrd=0.0520

pFtest(fixed.new, ols2) # p<0.001; should use fixed effect model. 


# random effects using plm
random <- plm(w_mtact08_2 ~ ratio_er*w_dg08, data=check_2008, index=c("mmgrp"), model="random")
summary(random)

random2 <- plm(y2 ~ ratio_er*x0, check_both, index=c("mmgrp"), model="random")
summary(random2)

# hausman test comparing fixed effect vs random effect model 
phtest(fixed, random) #p<0.001; go with fixed effects. 
phtest(fixed.new, random2) # p<0.001; go with fixed effect model .


# time-fixed effects
fixed.time <- plm(y2 ~ ratio_er*x0 + factor(year), check_both, index=c("mmgrp", "year"), model="within")
summary(fixed.time) # coef=0.92103, significant. rsqrd=0.61482. 


#report using tab_model()
tab_model(ols2, fixed.dum.new, fixed.dum.new2, auto.label = T, show.ci = FALSE, show.se=T, show.p=T)
## this is the matter of at what level do I wish to cluster the standard error. for the x2, it is clustered at marriage market; for x0*ratio_er, it is clustered at individual level. 


# bartik instrument - instrument by percentiles of income distribution (bin=0.05)

prob=seq(0.05, 0.95, by=0.05)                                                   # setting quantile values

INCOME2008 <- INCOME2008 %>% 
  rowwise() %>%
  group_by(NEDUGRP, NAGEGRP, NCOUNTRY, SEX) %>% nest() %>%
  mutate(
    wmtact08_005=map(data, ~quantile(.x$NETWKTOT, probs=prob[1])), 
    wmtact08_010=map(data, ~quantile(.x$NETWKTOT, probs=prob[2])), 
    wmtact08_015=map(data, ~quantile(.x$NETWKTOT, probs=prob[3])), 
    wmtact08_020=map(data, ~quantile(.x$NETWKTOT, probs=prob[4])), 
    wmtact08_025=map(data, ~quantile(.x$NETWKTOT, probs=prob[5])), 
    wmtact08_030=map(data, ~quantile(.x$NETWKTOT, probs=prob[6])), 
    wmtact08_035=map(data, ~quantile(.x$NETWKTOT, probs=prob[7])), 
    wmtact08_040=map(data, ~quantile(.x$NETWKTOT, probs=prob[8])), 
    wmtact08_045=map(data, ~quantile(.x$NETWKTOT, probs=prob[9])), 
    wmtact08_050=map(data, ~quantile(.x$NETWKTOT, probs=prob[10])), 
    wmtact08_055=map(data, ~quantile(.x$NETWKTOT, probs=prob[11])), 
    wmtact08_060=map(data, ~quantile(.x$NETWKTOT, probs=prob[12])), 
    wmtact08_065=map(data, ~quantile(.x$NETWKTOT, probs=prob[13])), 
    wmtact08_070=map(data, ~quantile(.x$NETWKTOT, probs=prob[14])), 
    wmtact08_075=map(data, ~quantile(.x$NETWKTOT, probs=prob[15])), 
    wmtact08_080=map(data, ~quantile(.x$NETWKTOT, probs=prob[16])), 
    wmtact08_085=map(data, ~quantile(.x$NETWKTOT, probs=prob[17])), 
    wmtact08_090=map(data, ~quantile(.x$NETWKTOT, probs=prob[18])), 
    wmtact08_095=map(data, ~quantile(.x$NETWKTOT, probs=prob[19])), 
  ) %>%
  unnest(cols=c(
    data, wmtact08_095, wmtact08_090, wmtact08_085, wmtact08_080, wmtact08_075, 
    wmtact08_070, wmtact08_065, wmtact08_060, wmtact08_055, wmtact08_050, 
    wmtact08_045, wmtact08_040, wmtact08_035, wmtact08_030, wmtact08_025, 
    wmtact08_020, wmtact08_015, wmtact08_010, wmtact08_005 )) %>%
  rowwise() %>% 
  group_by(dg, SEX) %>% nest() %>%
  mutate(
    wdg08_005=map(data, ~quantile(.x$NETWKTOT, probs=prob[1])), 
    wdg08_010=map(data, ~quantile(.x$NETWKTOT, probs=prob[2])), 
    wdg08_015=map(data, ~quantile(.x$NETWKTOT, probs=prob[3])), 
    wdg08_020=map(data, ~quantile(.x$NETWKTOT, probs=prob[4])), 
    wdg08_025=map(data, ~quantile(.x$NETWKTOT, probs=prob[5])), 
    wdg08_030=map(data, ~quantile(.x$NETWKTOT, probs=prob[6])), 
    wdg08_035=map(data, ~quantile(.x$NETWKTOT, probs=prob[7])), 
    wdg08_040=map(data, ~quantile(.x$NETWKTOT, probs=prob[8])), 
    wdg08_045=map(data, ~quantile(.x$NETWKTOT, probs=prob[9])), 
    wdg08_050=map(data, ~quantile(.x$NETWKTOT, probs=prob[10])), 
    wdg08_055=map(data, ~quantile(.x$NETWKTOT, probs=prob[11])), 
    wdg08_060=map(data, ~quantile(.x$NETWKTOT, probs=prob[12])), 
    wdg08_065=map(data, ~quantile(.x$NETWKTOT, probs=prob[13])), 
    wdg08_070=map(data, ~quantile(.x$NETWKTOT, probs=prob[14])), 
    wdg08_075=map(data, ~quantile(.x$NETWKTOT, probs=prob[15])), 
    wdg08_080=map(data, ~quantile(.x$NETWKTOT, probs=prob[16])), 
    wdg08_085=map(data, ~quantile(.x$NETWKTOT, probs=prob[17])), 
    wdg08_090=map(data, ~quantile(.x$NETWKTOT, probs=prob[18])), 
    wdg08_095=map(data, ~quantile(.x$NETWKTOT, probs=prob[19])), 
  ) %>%
  unnest(cols=c(
    data, wdg08_095, wdg08_090, wdg08_085, wdg08_080, wdg08_075, 
    wdg08_070, wdg08_065, wdg08_060, wdg08_055, wdg08_050, 
    wdg08_045, wdg08_040, wdg08_035, wdg08_030, wdg08_025, 
    wdg08_020, wdg08_015, wdg08_010, wdg08_005)) %>%
  rowwise() %>%
  mutate(
    wmtpred08_005=wdg08_005*ratio_er, 
    wmtpred08_010=wdg08_010*ratio_er, 
    wmtpred08_015=wdg08_015*ratio_er, 
    wmtpred08_020=wdg08_020*ratio_er, 
    wmtpred08_025=wdg08_025*ratio_er, 
    wmtpred08_030=wdg08_030*ratio_er, 
    wmtpred08_035=wdg08_035*ratio_er, 
    wmtpred08_040=wdg08_040*ratio_er, 
    wmtpred08_045=wdg08_045*ratio_er, 
    wmtpred08_050=wdg08_050*ratio_er, 
    wmtpred08_055=wdg08_055*ratio_er, 
    wmtpred08_060=wdg08_060*ratio_er, 
    wmtpred08_065=wdg08_065*ratio_er, 
    wmtpred08_070=wdg08_070*ratio_er, 
    wmtpred08_075=wdg08_075*ratio_er, 
    wmtpred08_080=wdg08_080*ratio_er, 
    wmtpred08_085=wdg08_085*ratio_er, 
    wmtpred08_090=wdg08_090*ratio_er,
    wmtpred08_095=wdg08_095*ratio_er,
  ) %>%
  group_by(mmgrp, SEX) %>%  nest() %>%
  mutate(
    wmtpred08_005_2=map(data, ~sum(.x$wdg08_005*.x$ratio_er)),
    wmtpred08_010_2=map(data, ~sum(.x$wdg08_010*.x$ratio_er)),
    wmtpred08_015_2=map(data, ~sum(.x$wdg08_015*.x$ratio_er)),
    wmtpred08_020_2=map(data, ~sum(.x$wdg08_020*.x$ratio_er)),
    wmtpred08_025_2=map(data, ~sum(.x$wdg08_025*.x$ratio_er)),
    wmtpred08_030_2=map(data, ~sum(.x$wdg08_030*.x$ratio_er)),
    wmtpred08_035_2=map(data, ~sum(.x$wdg08_035*.x$ratio_er)),
    wmtpred08_040_2=map(data, ~sum(.x$wdg08_040*.x$ratio_er)),
    wmtpred08_045_2=map(data, ~sum(.x$wdg08_045*.x$ratio_er)),
    wmtpred08_050_2=map(data, ~sum(.x$wdg08_050*.x$ratio_er)),
    wmtpred08_055_2=map(data, ~sum(.x$wdg08_055*.x$ratio_er)),
    wmtpred08_060_2=map(data, ~sum(.x$wdg08_060*.x$ratio_er)),
    wmtpred08_065_2=map(data, ~sum(.x$wdg08_065*.x$ratio_er)),
    wmtpred08_070_2=map(data, ~sum(.x$wdg08_070*.x$ratio_er)),
    wmtpred08_075_2=map(data, ~sum(.x$wdg08_075*.x$ratio_er)),
    wmtpred08_080_2=map(data, ~sum(.x$wdg08_080*.x$ratio_er)),
    wmtpred08_085_2=map(data, ~sum(.x$wdg08_085*.x$ratio_er)),
    wmtpred08_090_2=map(data, ~sum(.x$wdg08_090*.x$ratio_er)),
    wmtpred08_095_2=map(data, ~sum(.x$wdg08_095*.x$ratio_er)),
  ) %>%
  unnest(cols=c(
    data, wmtpred08_005_2, wmtpred08_010_2, wmtpred08_015_2, wmtpred08_020_2,
    wmtpred08_025_2, wmtpred08_030_2, wmtpred08_035_2, wmtpred08_040_2,
    wmtpred08_045_2, wmtpred08_050_2, wmtpred08_055_2, wmtpred08_060_2,
    wmtpred08_065_2, wmtpred08_070_2, wmtpred08_075_2, wmtpred08_080_2,
    wmtpred08_085_2, wmtpred08_090_2, wmtpred08_095_2
  )) %>% 
  #distinct(SEX, NAGEGRP, NEDUGRP, NCOUNTRY, INDSECT, .keep_all=T)
  group_by(mmgrp, SEX) %>% nest() %>%
  mutate(
    w_mtact08 = map(data, ~mean(.x$NETWKTOT))
  ) %>%
  unnest(cols=c(data, w_mtact08)) 



lm(wmtact08_005 ~ wdg08_005*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary # act correlated with predicted bartik
lm(wmtact08_010 ~ wdg08_010*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_030 ~ wdg08_030*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_050 ~ wdg08_050*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_070 ~ wdg08_070*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_090 ~ wdg08_090*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_095 ~ wdg08_095*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary

lm(wmtact08_010 ~ wmtpred08_010_2 + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_095 ~ wmtpred08_095_2 + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_090 ~ wmtpred08_090_2 + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_070 ~ wmtpred08_070_2 + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_050 ~ wmtpred08_050_2 + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_030 ~ wmtpred08_030_2 + factor(mmgrp)-1, INCOME2008) %>% summary
lm(wmtact08_010 ~ wmtpred08_010_2 + factor(mmgrp)-1, INCOME2008) %>% summary





# randomly drawing from dg and assignining potential income 

## before that, for bartik (pth percentile) grouping - mutate new vars

practice <- INCOME2008 %>% 
  rowwise() %>% 
  mutate(
    grp005 = if_else(NETWKTOT<=wdg08_005, 1, 0),
    grp010 = if_else(wdg08_005<NETWKTOT && NETWKTOT<=wdg08_010, 1, 0),
    grp015 = if_else(wdg08_010<NETWKTOT && NETWKTOT<=wdg08_015, 1, 0),
    grp020 = if_else(wdg08_015<NETWKTOT && NETWKTOT<=wdg08_020, 1, 0),
    grp025 = if_else(wdg08_020<NETWKTOT && NETWKTOT<=wdg08_025, 1, 0),
    grp030 = if_else(wdg08_025<NETWKTOT && NETWKTOT<=wdg08_030, 1, 0),
    grp035 = if_else(wdg08_030<NETWKTOT && NETWKTOT<=wdg08_035, 1, 0),
    grp040 = if_else(wdg08_035<NETWKTOT && NETWKTOT<=wdg08_040, 1, 0),
    grp045 = if_else(wdg08_040<NETWKTOT && NETWKTOT<=wdg08_045, 1, 0),
    grp050 = if_else(wdg08_045<NETWKTOT && NETWKTOT<=wdg08_050, 1, 0),
    grp055 = if_else(wdg08_050<NETWKTOT && NETWKTOT<=wdg08_055, 1, 0),
    grp060 = if_else(wdg08_055<NETWKTOT && NETWKTOT<=wdg08_060, 1, 0),
    grp065 = if_else(wdg08_060<NETWKTOT && NETWKTOT<=wdg08_065, 1, 0),
    grp070 = if_else(wdg08_065<NETWKTOT && NETWKTOT<=wdg08_070, 1, 0),
    grp075 = if_else(wdg08_070<NETWKTOT && NETWKTOT<=wdg08_075, 1, 0),
    grp080 = if_else(wdg08_075<NETWKTOT && NETWKTOT<=wdg08_080, 1, 0),
    grp085 = if_else(wdg08_080<NETWKTOT && NETWKTOT<=wdg08_085, 1, 0),
    grp090 = if_else(wdg08_085<NETWKTOT && NETWKTOT<=wdg08_090, 1, 0),
    grp095 = if_else(wdg08_090<NETWKTOT && NETWKTOT<=wdg08_095, 1, 0)
  ) %>% ungroup()


w_dgpot08_female <- practice %>%
  filter(SEX==2) %>% 
  rowwise() %>% 
  group_by(dg, grp005, grp010, grp015, grp020, grp025, grp030, grp035, 
           grp040, grp045, grp050, grp055, grp060, grp065, grp070, grp075, 
           grp080, grp085, grp090, grp095) %>% nest() %>% 
  mutate(
    w_pot08f=map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T)), 
    w_pot08f_pth = map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T))) %>%
  unnest(cols=c(data, w_pot08f, w_pot08f_pth)) %>% ungroup

w_dgpot08_male <- practice %>% 
  filter(SEX==1) %>%
  rowwise() %>% 
  group_by(dg, grp005, grp010, grp015, grp020, grp025, grp030, grp035, 
           grp040, grp045, grp050, grp055, grp060, grp065, grp070, grp075, 
           grp080, grp085, grp090, grp095) %>% nest() %>% 
  mutate(
    w_pot08m=map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T)), 
    w_pot08m_pth = map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T))) %>%
  unnest(cols=c(data, w_pot08m, w_pot08m_pth)) %>% ungroup


w_pot08 <- w_dgpot08_female %>% bind_rows(w_dgpot08_male)



# another, but primary approach =====

## create modified sample function
newsample <- function(x, ...){
  if(all(is.na(x))){
    return(NA)
  }
  return(sample(x[!is.na(x)], ...))
}

## and apply it to subsample

w_pot08_female <- w_pot08 %>%
  rowwise() %>% 
  group_by(dg) %>% nest() %>%
  mutate(
    w_pot08f_m = map(data, ~newsample(.x$w_pot08m, nrow(.), replace=T)),
    w_pot08f_pth_m = map(data, ~newsample(.x$w_pot08m_pth, nrow(.), replace=T))
  ) %>%
  unnest(cols=c(data, w_pot08f_m, w_pot08f_pth_m )) %>% ungroup() 

w_pot08 <- w_pot08_female %>% 
  rowwise() %>% 
  group_by(dg) %>% nest() %>%
  mutate(
    w_pot08m_f = map(data, ~newsample(.x$w_pot08f, nrow(.), replace=T)),
    w_pot08m_pth_f = map(data, ~newsample(.x$w_pot08f_pth, nrow(.), replace=T))
  ) %>%
  unnest(cols=c(data,w_pot08m_f, w_pot08m_pth_f )) %>%
  filter(!is.na(w_pot08m_f), !is.na(w_pot08f_m)) %>% ungroup() # need to filter them out in order to calculate the difference and prevent error from happening during the calculation  


# now check pr(woman earns more than man) in a given marriage market
prwm08 <- w_pot08 %>% 
  rowwise() %>% 
  mutate(
    w_diff = if_else((w_pot08m_f - w_pot08f_m)>0, 1, 0 ),
    w_diff_pth = if_else((w_pot08m_pth_f - w_pot08f_pth_m)>0, 1, 0)
  ) %>% 
  ungroup() %>% 
  group_by(mmgrp) %>% nest() %>%
  mutate(
    n_mmgrp = map(data, ~nrow(.)),
    n_wmore = map(data, ~sum(.x$w_diff)),
    n_wmore_pth_sum = map(data, ~sum(.x$w_diff_pth))
  ) %>% 
  unnest(cols=c(data, n_mmgrp, n_wmore, n_wmore_pth_sum)) %>%
  ungroup() %>% 
  rowwise() %>% 
  group_by(mmgrp, grp005, grp010, grp015, grp020, grp025, grp030, grp035, 
           grp040, grp045, grp050, grp055, grp060, grp065, grp070, grp075, 
           grp080, grp085, grp090, grp095) %>% nest() %>% 
  mutate(
    n_wmore_pth_per = map(data, ~sum(.x$w_diff_pth)),
    n_mmgrp_pth_per = map(data, ~nrow(.))
  ) %>% 
  unnest(
    cols=c(data, n_wmore_pth_per, n_mmgrp_pth_per)
  ) %>% 
  ungroup() %>% 
  rowwise() %>%
  mutate(
    prwmore=(n_wmore/n_mmgrp),
    prwmore_pth=(n_wmore_pth_sum/n_mmgrp), 
    prwmore_pth_per = n_wmore_pth_per/n_mmgrp_pth_per
  ) %>% ungroup()




# for year 2014
INCOME2014B <- INCOME2014B %>% 
  rowwise() %>%
  group_by(NEDUGRP, NAGEGRP, NCOUNTRY, SEX) %>% nest() %>%
  mutate(
    wmtact14_005=map(data, ~quantile(.x$NETWKTOT, probs=prob[1], na.rm=T)), 
    wmtact14_010=map(data, ~quantile(.x$NETWKTOT, probs=prob[2], na.rm=T)), 
    wmtact14_015=map(data, ~quantile(.x$NETWKTOT, probs=prob[3], na.rm=T)), 
    wmtact14_020=map(data, ~quantile(.x$NETWKTOT, probs=prob[4], na.rm=T)), 
    wmtact14_025=map(data, ~quantile(.x$NETWKTOT, probs=prob[5], na.rm=T)), 
    wmtact14_030=map(data, ~quantile(.x$NETWKTOT, probs=prob[6], na.rm=T)), 
    wmtact14_035=map(data, ~quantile(.x$NETWKTOT, probs=prob[7], na.rm=T)), 
    wmtact14_040=map(data, ~quantile(.x$NETWKTOT, probs=prob[8], na.rm=T)), 
    wmtact14_045=map(data, ~quantile(.x$NETWKTOT, probs=prob[9], na.rm=T)), 
    wmtact14_050=map(data, ~quantile(.x$NETWKTOT, probs=prob[10], na.rm=T)), 
    wmtact14_055=map(data, ~quantile(.x$NETWKTOT, probs=prob[11], na.rm=T)), 
    wmtact14_060=map(data, ~quantile(.x$NETWKTOT, probs=prob[12], na.rm=T)), 
    wmtact14_065=map(data, ~quantile(.x$NETWKTOT, probs=prob[13], na.rm=T)), 
    wmtact14_070=map(data, ~quantile(.x$NETWKTOT, probs=prob[14], na.rm=T)), 
    wmtact14_075=map(data, ~quantile(.x$NETWKTOT, probs=prob[15], na.rm=T)), 
    wmtact14_080=map(data, ~quantile(.x$NETWKTOT, probs=prob[16], na.rm=T)), 
    wmtact14_085=map(data, ~quantile(.x$NETWKTOT, probs=prob[17], na.rm=T)), 
    wmtact14_090=map(data, ~quantile(.x$NETWKTOT, probs=prob[18], na.rm=T)), 
    wmtact14_095=map(data, ~quantile(.x$NETWKTOT, probs=prob[19], na.rm=T)), 
  ) %>%
  unnest(cols=c(
    data, wmtact14_095, wmtact14_090, wmtact14_085, wmtact14_080, wmtact14_075, 
    wmtact14_070, wmtact14_065, wmtact14_060, wmtact14_055, wmtact14_050, 
    wmtact14_045, wmtact14_040, wmtact14_035, wmtact14_030, wmtact14_025, 
    wmtact14_020, wmtact14_015, wmtact14_010, wmtact14_005 )) %>%
  ungroup() %>% 
  rowwise() %>% 
  group_by(dg, SEX) %>% nest() %>%
  mutate(
    wdg14_005=map(data, ~quantile(.x$NETWKTOT, probs=prob[1], na.rm=T)), 
    wdg14_010=map(data, ~quantile(.x$NETWKTOT, probs=prob[2], na.rm=T)), 
    wdg14_015=map(data, ~quantile(.x$NETWKTOT, probs=prob[3], na.rm=T)), 
    wdg14_020=map(data, ~quantile(.x$NETWKTOT, probs=prob[4], na.rm=T)), 
    wdg14_025=map(data, ~quantile(.x$NETWKTOT, probs=prob[5], na.rm=T)), 
    wdg14_030=map(data, ~quantile(.x$NETWKTOT, probs=prob[6], na.rm=T)), 
    wdg14_035=map(data, ~quantile(.x$NETWKTOT, probs=prob[7], na.rm=T)), 
    wdg14_040=map(data, ~quantile(.x$NETWKTOT, probs=prob[8], na.rm=T)), 
    wdg14_045=map(data, ~quantile(.x$NETWKTOT, probs=prob[9], na.rm=T)), 
    wdg14_050=map(data, ~quantile(.x$NETWKTOT, probs=prob[10], na.rm=T)), 
    wdg14_055=map(data, ~quantile(.x$NETWKTOT, probs=prob[11], na.rm=T)), 
    wdg14_060=map(data, ~quantile(.x$NETWKTOT, probs=prob[12], na.rm=T)), 
    wdg14_065=map(data, ~quantile(.x$NETWKTOT, probs=prob[13], na.rm=T)), 
    wdg14_070=map(data, ~quantile(.x$NETWKTOT, probs=prob[14], na.rm=T)), 
    wdg14_075=map(data, ~quantile(.x$NETWKTOT, probs=prob[15], na.rm=T)), 
    wdg14_080=map(data, ~quantile(.x$NETWKTOT, probs=prob[16], na.rm=T)), 
    wdg14_085=map(data, ~quantile(.x$NETWKTOT, probs=prob[17], na.rm=T)), 
    wdg14_090=map(data, ~quantile(.x$NETWKTOT, probs=prob[18], na.rm=T)), 
    wdg14_095=map(data, ~quantile(.x$NETWKTOT, probs=prob[19], na.rm=T)), 
  ) %>%
  unnest(cols=c(
    data, wdg14_095, wdg14_090, wdg14_085, wdg14_080, wdg14_075, 
    wdg14_070, wdg14_065, wdg14_060, wdg14_055, wdg14_050, 
    wdg14_045, wdg14_040, wdg14_035, wdg14_030, wdg14_025, 
    wdg14_020, wdg14_015, wdg14_010, wdg14_005)) %>%
  ungroup() %>% 
  rowwise() %>%
  mutate(
    wmtpred14_005=wdg14_005*ratio_er, 
    wmtpred14_010=wdg14_010*ratio_er, 
    wmtpred14_015=wdg14_015*ratio_er, 
    wmtpred14_020=wdg14_020*ratio_er, 
    wmtpred14_025=wdg14_025*ratio_er, 
    wmtpred14_030=wdg14_030*ratio_er, 
    wmtpred14_035=wdg14_035*ratio_er, 
    wmtpred14_040=wdg14_040*ratio_er, 
    wmtpred14_045=wdg14_045*ratio_er, 
    wmtpred14_050=wdg14_050*ratio_er, 
    wmtpred14_055=wdg14_055*ratio_er, 
    wmtpred14_060=wdg14_060*ratio_er, 
    wmtpred14_065=wdg14_065*ratio_er, 
    wmtpred14_070=wdg14_070*ratio_er, 
    wmtpred14_075=wdg14_075*ratio_er, 
    wmtpred14_080=wdg14_080*ratio_er, 
    wmtpred14_085=wdg14_085*ratio_er, 
    wmtpred14_090=wdg14_090*ratio_er,
    wmtpred14_095=wdg14_095*ratio_er,
  ) %>%
  ungroup() %>% 
  group_by(mmgrp, SEX) %>%  nest() %>%
  mutate(
    wmtpred14_005_2=map(data, ~sum(.x$wdg14_005*.x$ratio_er)),
    wmtpred14_010_2=map(data, ~sum(.x$wdg14_010*.x$ratio_er)),
    wmtpred14_015_2=map(data, ~sum(.x$wdg14_015*.x$ratio_er)),
    wmtpred14_020_2=map(data, ~sum(.x$wdg14_020*.x$ratio_er)),
    wmtpred14_025_2=map(data, ~sum(.x$wdg14_025*.x$ratio_er)),
    wmtpred14_030_2=map(data, ~sum(.x$wdg14_030*.x$ratio_er)),
    wmtpred14_035_2=map(data, ~sum(.x$wdg14_035*.x$ratio_er)),
    wmtpred14_040_2=map(data, ~sum(.x$wdg14_040*.x$ratio_er)),
    wmtpred14_045_2=map(data, ~sum(.x$wdg14_045*.x$ratio_er)),
    wmtpred14_050_2=map(data, ~sum(.x$wdg14_050*.x$ratio_er)),
    wmtpred14_055_2=map(data, ~sum(.x$wdg14_055*.x$ratio_er)),
    wmtpred14_060_2=map(data, ~sum(.x$wdg14_060*.x$ratio_er)),
    wmtpred14_065_2=map(data, ~sum(.x$wdg14_065*.x$ratio_er)),
    wmtpred14_070_2=map(data, ~sum(.x$wdg14_070*.x$ratio_er)),
    wmtpred14_075_2=map(data, ~sum(.x$wdg14_075*.x$ratio_er)),
    wmtpred14_080_2=map(data, ~sum(.x$wdg14_080*.x$ratio_er)),
    wmtpred14_085_2=map(data, ~sum(.x$wdg14_085*.x$ratio_er)),
    wmtpred14_090_2=map(data, ~sum(.x$wdg14_090*.x$ratio_er)),
    wmtpred14_095_2=map(data, ~sum(.x$wdg14_095*.x$ratio_er)),
  ) %>%
  unnest(cols=c(
    data, wmtpred14_005_2, wmtpred14_010_2, wmtpred14_015_2, wmtpred14_020_2,
    wmtpred14_025_2, wmtpred14_030_2, wmtpred14_035_2, wmtpred14_040_2,
    wmtpred14_045_2, wmtpred14_050_2, wmtpred14_055_2, wmtpred14_060_2,
    wmtpred14_065_2, wmtpred14_070_2, wmtpred14_075_2, wmtpred14_080_2,
    wmtpred14_085_2, wmtpred14_090_2, wmtpred14_095_2
  )) %>% 
  ungroup() %>% rowwise() %>% 
  #distinct(SEX, NAGEGRP, NEDUGRP, NCOUNTRY, INDSECT, .keep_all=T)
  group_by(mmgrp, SEX) %>% nest() %>%
  mutate(
    w_mtact14 = map(data, ~mean(.x$NETWKTOT))
  ) %>%
  unnest(cols=c(data, w_mtact14)) %>% ungroup()



# randomly drawing from dg and assignining potential income 
## before that, for bartik (pth percentile) grouping - mutate new vars

practice <- INCOME2014B %>% 
  rowwise() %>% 
  mutate(
    grp005 = if_else(NETWKTOT<=wdg14_005, 1, 0),
    grp010 = if_else(wdg14_005<NETWKTOT && NETWKTOT<=wdg14_010, 1, 0),
    grp015 = if_else(wdg14_010<NETWKTOT && NETWKTOT<=wdg14_015, 1, 0),
    grp020 = if_else(wdg14_015<NETWKTOT && NETWKTOT<=wdg14_020, 1, 0),
    grp025 = if_else(wdg14_020<NETWKTOT && NETWKTOT<=wdg14_025, 1, 0),
    grp030 = if_else(wdg14_025<NETWKTOT && NETWKTOT<=wdg14_030, 1, 0),
    grp035 = if_else(wdg14_030<NETWKTOT && NETWKTOT<=wdg14_035, 1, 0),
    grp040 = if_else(wdg14_035<NETWKTOT && NETWKTOT<=wdg14_040, 1, 0),
    grp045 = if_else(wdg14_040<NETWKTOT && NETWKTOT<=wdg14_045, 1, 0),
    grp050 = if_else(wdg14_045<NETWKTOT && NETWKTOT<=wdg14_050, 1, 0),
    grp055 = if_else(wdg14_050<NETWKTOT && NETWKTOT<=wdg14_055, 1, 0),
    grp060 = if_else(wdg14_055<NETWKTOT && NETWKTOT<=wdg14_060, 1, 0),
    grp065 = if_else(wdg14_060<NETWKTOT && NETWKTOT<=wdg14_065, 1, 0),
    grp070 = if_else(wdg14_065<NETWKTOT && NETWKTOT<=wdg14_070, 1, 0),
    grp075 = if_else(wdg14_070<NETWKTOT && NETWKTOT<=wdg14_075, 1, 0),
    grp080 = if_else(wdg14_075<NETWKTOT && NETWKTOT<=wdg14_080, 1, 0),
    grp085 = if_else(wdg14_080<NETWKTOT && NETWKTOT<=wdg14_085, 1, 0),
    grp090 = if_else(wdg14_085<NETWKTOT && NETWKTOT<=wdg14_090, 1, 0),
    grp095 = if_else(wdg14_090<NETWKTOT && NETWKTOT<=wdg14_095, 1, 0)
  ) %>% ungroup()


w_dgpot14_female <- practice %>%
  filter(SEX==2) %>% 
  rowwise() %>% 
  group_by(dg, grp005, grp010, grp015, grp020, grp025, grp030, grp035, 
           grp040, grp045, grp050, grp055, grp060, grp065, grp070, grp075, 
           grp080, grp085, grp090, grp095) %>% nest() %>% 
  mutate(
    w_pot14f=map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T)), 
    w_pot14f_pth = map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T))) %>%
  unnest(cols=c(data, w_pot14f, w_pot14f_pth)) %>% ungroup()


w_dgpot14_male <- practice %>% 
  filter(SEX==1) %>%
  rowwise() %>% 
  group_by(dg, grp005, grp010, grp015, grp020, grp025, grp030, grp035, 
           grp040, grp045, grp050, grp055, grp060, grp065, grp070, grp075, 
           grp080, grp085, grp090, grp095) %>% nest() %>% 
  mutate(
    w_pot14m=map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T)), 
    w_pot14m_pth = map(data, ~sample(.x$NETWKTOT, nrow(.), replace=T))) %>%
  unnest(cols=c(data, w_pot14m, w_pot14m_pth)) %>% ungroup() 


w_pot14 <- w_dgpot14_female %>% bind_rows(w_dgpot14_male)




## and apply it to subsample

w_pot14_female <- w_pot14 %>%
  rowwise() %>% 
  group_by(dg) %>% nest() %>%
  mutate(
    w_pot14f_m = map(data, ~newsample(.x$w_pot14m, nrow(.), replace=T)),
    w_pot14f_pth_m = map(data, ~newsample(.x$w_pot14m_pth, nrow(.), replace=T))
  ) %>%
  unnest(cols=c(data, w_pot14f_m, w_pot14f_pth_m )) %>% ungroup() #it works!


w_pot14 <- w_pot14_female %>% 
  rowwise() %>% 
  group_by(dg) %>% nest() %>%
  mutate(
    w_pot14m_f = map(data, ~newsample(.x$w_pot14f, nrow(.), replace=T)),
    w_pot14m_pth_f = map(data, ~newsample(.x$w_pot14f_pth, nrow(.), replace=T))
  ) %>%
  unnest(cols=c(data,w_pot14m_f, w_pot14m_pth_f )) %>% ungroup() %>%
  filter(!is.na(w_pot14m_f), !is.na(w_pot14f_m))


# now check pr woman earns more than man in a given marriage market
prwm14 <- w_pot14 %>% 
  rowwise() %>% 
  mutate(
    w_diff = if_else((w_pot14m_f - w_pot14f_m)>0, 1, 0 ),
    w_diff_pth = if_else((w_pot14m_pth_f - w_pot14f_pth_m)>0, 1, 0)
  ) %>% 
  ungroup() %>% rowwise() %>% 
  group_by(mmgrp) %>% nest() %>%
  mutate(
    n_mmgrp = map(data, ~nrow(.)),
    n_wmore = map(data, ~sum(.x$w_diff)),
    n_wmore_pth_sum = map(data, ~sum(.x$w_diff_pth))
  ) %>% 
  unnest(cols=c(data, n_mmgrp, n_wmore, n_wmore_pth_sum)) %>% 
  ungroup() %>% rowwise() %>%
  group_by(mmgrp, grp005, grp010, grp015, grp020, grp025, grp030, grp035, 
           grp040, grp045, grp050, grp055, grp060, grp065, grp070, grp075, 
           grp080, grp085, grp090, grp095) %>% nest() %>% 
  mutate(
    n_wmore_pth_per = map(data, ~sum(.x$w_diff_pth)),
    n_mmgrp_pth_per = map(data, ~nrow(.))
  ) %>% 
  unnest(
    cols=c(data, n_wmore_pth_per, n_mmgrp_pth_per)
  ) %>% 
  ungroup() %>% rowwise() %>%
  mutate(
    prwmore=(n_wmore/n_mmgrp),
    prwmore_pth=(n_wmore_pth_sum/n_mmgrp), 
    prwmore_pth_per = n_wmore_pth_per/n_mmgrp_pth_per
  ) %>% ungroup()


## check: prwm08 %>% count(year)
## check: prwm08 %>% filter(is.na(mmgrp)) %>% count(mmgrp)

prwm14 <- prwm14 %>% 
  rowwise() %>% 
  mutate(year=2014) %>% 
  ungroup() %>% 
  select(CASENOP, HSERIALP, PERSNO, mmgrp, dg, year, SEX, NEDUGRP, NAGEGRP, NCOUNTRY, 
         INDSECT, grp005:grp095, NETWKTOT,
         prwmore, prwmore_pth, prwmore_pth_per) %>% 
  
  
  prwm08 <- prwm08 %>%
  rowwise() %>%
  mutate(year=2008) %>%
  ungroup() %>% 
  select(CASENOP, HSERIALP, PERSNO, mmgrp, dg, year, SEX, NEDUGRP, NAGEGRP, NCOUNTRY, 
         INDSECT, grp005:grp095, NETWKTOT,
         prwmore, prwmore_pth, prwmore_pth_per)


prwm_both <- prwm08 %>% bind_rows(prwm14)

practice <- prwm_both %>% group_by(CASENOP) %>% add_tally(., name="duplicate")
any(practice$duplicate>1) # returns [true]!!! ==> so an individual for two diff years exist. but we cannot address them all. so let's just go with these, and make sure to use both CASENOP and year. 
any(practice$duplicate>2) # returns [false]!!!



# check if we our instrument actually is constructed and that there may be any odds. 
prwm_both %>% group_by(year) %>% summarise(prwmore=mean(prwmore_pth), sd=sd(prwmore_pth))
prwm_both %>% summarise(prwmore=mean(prwmore), sd=sd(prwmore))
prwm_both %>% filter(!is.na(prwmore_pth)) %>% count(year, SEX)
prwm_both %>% count(prwmore)
prwm_both %>% count(prwmore_pth)
prwm_both %>% count(prwmore_pth_per)

practice_final %>% group_by(year) %>% summarise(mean=mean(prwmore_pth), sd=sd(prwmore_pth))
prwm08 %>% summarise(mean=mean(prwmore_pth), sd=sd(prwmore_pth))
practice_final %>% 
  filter(!is.na(upwrel), !is.na(pwrel), !is.na(leis2rel), !is.na(jt_ftratio)) %>% 
  group_by(year, SEX) %>% summarise(
    upwrel=mean(upwrel), pwrel=mean(pwrel), leis2rel=mean(leis2rel), jt_ftratio=mean(jt_ftratio),
    upwratiowkly=mean(upwratiowkly), pwratiowkly=mean(pwratiowkly), leisratio2wkly=mean(leisratio2wkly)) 

practice_final %>%
  filter(!is.na(upwrel), !is.na(pwrel), !is.na(leis2rel), !is.na(jt_ftratio)) %>% 
  group_by(year, SEX) %>% summarise(
    pwrel=sd(pwrel), leis2rel=sd(leis2rel), jt_ftratio=sd(jt_ftratio), 
    upwratiowkly=sd(upwratiowkly), pwratiowkly=sd(pwratiowkly), leisratio2wkly=sd(leisratio2wkly)) 

practice_final %>% 
  filter(!is.na(upwrel), !is.na(pwrel), !is.na(leis2rel), !is.na(jt_ftratio)) %>% 
  group_by(year) %>% count(SEX)

practice_final %>%
  group_by(year, SEX) %>% count(same, diff)

INCOME2008 %>% ungroup() %>%filter(!is.na(NETWKTOT)) %>% group_by(SEX) %>%
  summarise(mean=mean(NETWKTOT), sd=sd(NETWKTOT))
INCOME2014B %>% filter(anyChildren==0) %>% 
  summarise(child4m=mean(child16), child4sd=sd(child16))

practice_final %>% filter(!is.na(increl)) %>% 
  group_by(year) %>% summarise(increl=mean(increl), sd=sd(increl))
INCOME2014B %>% count(increl)

prac <- practice_final %>% 
  ungroup() %>% filter(year==2008) %>% 
  rowwise() %>%
  mutate(
    times=if_else(SEX==2, 1, 0)
  ) %>% ungroup() %>% rowwise() %>%
  mutate(
    wifeearnsmore=if_else((increl*times)>0.5, 1, 0)
  ) %>% ungroup()

prac2 <- practice_final %>% 
  filter(year==2014) %>% 
  ungroup() %>% rowwise() %>%
  mutate(
    times=if_else(SEX==2, 1, 0)
  ) %>% ungroup() %>% rowwise() %>%
  mutate(
    wifeearnsmore=if_else((increl*times)>0.5, 1, 0)
  ) %>% ungroup()

prac %>% ungroup() %>% filter(!is.na(wifeearnsmore)) %>% 
  summarise(wifeearnsmore=mean(wifeearnsmore), sd=sd(wifeearnsmore))
prac2 %>% ungroup() %>% filter(!is.na(wifeearnsmore)) %>% 
  summarise(wifeearnsmore=mean(wifeearnsmore), sd=sd(wifeearnsmore))

INCOME2008 %>% ungroup() %>% filter(year==2008) %>% count(SEX)
prac2 %>% count(SEX)

practice_final %>% filter(SatPart>0, SatPart<8) %>% group_by(SEX) %>%
  summarise(mean=mean(SatPart), sd=sd(SatPart))





## probably this part below is not needed; but we will run this only so that we can refer back to values such as gendered.      
first <- rest_d %>% bind_rows(visit_d) %>% bind_rows(sportev_d) %>%             # and merge them altogether; this is a bit inefficient since we could have calculated 'd' after merging. 
  bind_rows(phone_d) %>% bind_rows(social_d) %>% bind_rows(cinema_d) %>% 
  bind_rows(theatre_d) %>% bind_rows(museum_d) %>% bind_rows(libr_d) %>%
  bind_rows(entmnt_d) %>% bind_rows(walk_d) %>% bind_rows(run_d) %>%
  bind_rows(bike_d) %>% bind_rows(ball_d) %>% bind_rows(fit_d) %>%
  bind_rows(water_d) %>% bind_rows(physical_d) %>% bind_rows(art_d) %>%
  bind_rows(comp_d) %>% bind_rows(solo_d) %>% bind_rows(tgthgame_d) %>% 
  bind_rows(compgame_d) %>% bind_rows(games_d) %>% bind_rows(read_d) %>%
  bind_rows(watch_d) %>% bind_rows(listen_d) %>% bind_rows(media_d)
#leissubgrp %>% count(DMSex)
#leissubgrp2 %>% count(serial)


first2 <- first %>% filter(forcalculationp_a==1) %>% 
  select(
    rest1, visitfriends, sportev, phone, social1, cinema, theatre, museum, 
    libr, entmnt, walkhike, keepfit, watersprt, physical, art, computing, 
    sologame, read, watch, listen, media1, sportev, running, bikeskiskate, 
    ballgame, tgthgame, compgame, games, gendered
  )


# then we assign 'feminine' or 'masculine'
practice_leissubgrp <- leissubgrp2 %>% 
  ungroup() %>% rowwise() %>% 
  mutate(
    feminine=if_else(
      sum(rest1, visitfriends, sportev, phone, social1, cinema, theatre, museum, 
          libr, entmnt, walkhike, keepfit, watersprt, physical, art, computing, 
          sologame, read, watch, listen, media1)> 0, 1, 0
    ), 
    masculine=if_else(
      sum(
        sportev, running, bikeskiskate, ballgame, tgthgame, compgame, games
      ) > 0, 1, 0
    ), 
    female = if_else(DMSex==2, 1, 0), 
    male = if_else(DMSex==1, 1, 0)
  ) %>%
  ungroup() %>% rowwise() %>%
  mutate(
    diff=case_when(
      masculine*female == 1 ~ 1, 
      feminine*male == 1 ~ 1, 
      TRUE ~ 0
    ), 
    same=case_when(
      feminine*female == 1 ~ 1, 
      masculine*male == 1 ~ 1, 
      TRUE ~ 0
    )
  ) %>% ungroup() %>% rowwise() %>%
  mutate(
    both = if_else(diff*same==1, 1, 0)
  )


practice_leissubgrp %>% count(same)
practice_leissubgrp %>% count(same, diff)
practice_leissubgrp %>% count(both, female)
practice_leissubgrp %>% count(female, same, diff) #actually, there are more people who seem to participate in 
## those who participate in different leisure bu not in the same leisure == 1756
## but among them, female is only 6 and male are the rest; leisure flexibility is inclined (?) towards male groups. 



leissubgrp2 <- leissubgrp2 %>%
  rowwise() %>%
  mutate(
    anyChildren = case_when(                                                    # reported presence of any children in family under 19
      dnrkid04 > 0 | DM016 >0 | DM510 >0 | DM1115 >0 | DM1619 > 0 ~ 1, 
      TRUE ~ 0
    ),
    child16 = case_when(                                                        # reported any children in family under 16
      DM016 > 0 ~ 1, 
      TRUE ~ 0
    ), 
    child15 = case_when(                                                        # reported any children in family under 15
      dnrkid04 >0 | DM1115 >0 ~ 1, 
      TRUE ~ 0
    ), 
    child4 = case_when(                                                         # reported any children in family under 5
      dnrkid04 >0 ~ 1, 
      TRUE ~ 0
    )
  )




# 1) rename couple_all vars and select the variables that are only necessary 

## remove labels that conflict with each other when getting merged ==> do not know how to work around with this yet. let's work on this later. 
### refer to: https://www.rdocumentation.org/packages/labelled/versions/2.10.0/topics/remove_labels
install.packages("labelled")
library(labelled)
?remove_val_labels() 
remove_val_labels(adult_all$SEX)
remove_val_labels(prwm_both$SEX)
var_label(adult_all$SEX) <- NULL
var_label(prwm_both$SEX) <- NULL

#lm(upwrel ~ prwmore*MALE+factor(mmgrp)+factor(diff)-1, practice_final) %>% summary
#lm(leisrel ~ prwmore*FEMALE+child16+factor(mmgrp)+factor(diff)-1, practice_final) %>% summary
#lm(leisrel ~ prwmore+diff*same+child16+factor(mmgrp), practice_finalf) %>% summary



#adult_all %>% count(anyChildren)
#adult_all %>% filter(!is.na(PARTNERNO), !is.na(COUPLENO), !is.na(jtime_ft)) %>% count(jtime_ft)
#adult_all %>% count(jt_ftratio)
#jtime_ft, jt_ftratio, jtime_ftvol, jt_ftvolratio,

#practice_bartik %>% count(jtime_ft)
#adult_all %>% filter(!is.na(jtime_ft)) %>% count(jtime_ft)

adult_all %>% count(pcratiowkly)

practice_bartik <- prwm_both %>% full_join(adult_all) %>% 
  ungroup() %>% 
  group_by(HSERIALP, PERSNO) %>% 
  fill(upwwkly, .direction="downup") %>% 
  fill(pwwkly, .direction="downup") %>%
  fill(leiswkly, .direction="downup") %>% 
  fill(leis2wkly, .direction="downup") %>% 
  fill(ccwkly, .direction="downup") %>% 
  fill(choreswkly, .direction="downup") %>% 
  fill(housemgmtwkly, .direction="downup") %>% 
  fill(pcwkly, .direction="downup") %>% 
  fill(hcapwkly, .direction="downup") %>% 
  fill(eduwkly, .direction="downup") %>% 
  fill(NETWKTOT, .direction="downup") %>% 
  fill(COUPLENO, .direction="downup") %>%
  fill(PARTNERNO, .direction="downup") %>% 
  fill(mmgrp, .direction="downup") %>%
  fill(dg, .direction="downup") %>% 
  fill(upwratio, .direction="downup") %>% 
  fill(leisratio, .direction="downup") %>% 
  fill(pwratio, .direction="downup") %>% 
  fill(pcratio, .direction="downup") %>% 
  fill(upwratiowkly, .direction="downup") %>%
  fill(leisratiowkly, .direction="downup") %>% 
  fill(pwratiowkly, .direction="downup") %>% 
  fill(pcratiowkly, .direction="downup") %>% 
  fill(sleep, .direction="downup") %>% 
  fill(SatPart, .direction="downup") %>% 
  fill(SatSoc, .direction="downup") %>% 
  fill(SatLeis, .direction="downup") %>% 
  fill(SatJob, .direction="downup") %>% 
  fill(SatInc, .direction="downup") %>% 
  #fill(all_vars(ends_with("ratio")), .direction="downup") %>%
  #fill(all_vars(ends_with("wkly")), .direction="downup") %>% 
  #fill(all_vars(starts_with("Sat")), .direction="downup") %>% 
  fill(anyChildren, .direction="downup") %>% 
  fill(child16, .direction="downup") %>%
  fill(child15, .direction="downup") %>%
  fill(child4, .direction="downup") %>%
  fill(jtime_ft, .direction="downup") %>% 
  fill(jt_ftratio, .direction="downup") %>%
  fill(jtime_ftvol, .direction="downup") %>% 
  fill(jt_ftvolratio, .direction="downup") %>% 
  ungroup() %>% 
  distinct(HSERIALP, PERSNO, .keep_all=T)




#jtime_ft, jt_ftratio, jtime_ftvol, jt_ftvolratio,
practice_bartik %>% count(year)
practice_bartik %>% count(jtime_ft)
practice_bartik %>% filter(is.na(upwwkly)) %>% count(upwwkly)
practice_bartik %>% count(upwwkly)
practice_bartik %>% count(pwwkly)
practice_bartik %>% count(leiswkly)
practice_bartik %>% filter(is.na(HSERIALP)) %>% count(HSERIALP)
practice_bartik %>% filter(!is.na(upwwkly)) %>% count(SEX)
practice_bartik %>% count(anyChildren)
practice_bartik %>% count(child16)
practice_bartik %>% count(child15)
practice_bartik %>% count(child16)
practice_bartik %>% count(child4)
practice_bartik %>% count(CASENOP)


bartik_couple <- practice_bartik %>% 
  filter(!is.na(COUPLENO), !is.na(PARTNERNO)) %>%                               # restrict to couples only for relative within hh 
  filter(!is.na(leiswkly)) %>%                                                  # restrict to uktus dataset only (as lfs does not contain time relative information )
  rowwise() %>% 
  group_by(HSERIALP, COUPLENO) %>% nest() %>% 
  mutate(
    LeisBoth = map(.x=data, .f=~sum(.x$leiswkly, na.rm=T)), 
    Leis2Both = map(.x=data, .f=~sum(.x$leis2wkly, na.rm=T)),
    CCBoth=map(.x=data, .f=~sum(.x$ccwkly, na.rm=T)), 
    ChoresBoth=map(.x=data, .f=~sum(.x$choreswkly, na.rm=T)), 
    HmgmtBoth=map(.x=data, .f=~sum(.x$housemgmtwkly, na.rm=T)), 
    PCBoth=map(.x=data, .f=~sum(.x$pcwkly, na.rm=T)), 
    PWBoth=map(.x=data, .f=~sum(.x$pwwkly, na.rm=T)), 
    UpwBoth=map(.x=data, .f=~sum(.x$upwwkly, na.rm=T)), 
    HcapBoth=map(.x=data, .f=~sum(.x$hcapwkly, na.rm=T)), 
    EduBoth=map(.x=data, .f=~sum(.x$eduwkly, na.rm=T)), 
    IncBoth=map(.x=data, .f=~sum(.x$NETWKTOT, na.rm=T)), 
    #CapBoth=map(.x=data, .f=~sum(.x$capIntwkly, na.rm=T)),
    #TimeBoth=map(.x=data, .f=~sum(.x$timeIntwkly, na.rm=T))
  ) %>% 
  unnest(
    cols=c(
      data, LeisBoth, Leis2Both, PWBoth, UpwBoth, #CapBoth, TimeBoth, 
      CCBoth, ChoresBoth, HmgmtBoth, PCBoth, HcapBoth, EduBoth, IncBoth)
  ) %>% ungroup() %>% rowwise() %>% 
  mutate(
    leisrel=(leiswkly/LeisBoth), leis2rel=(leis2wkly/Leis2Both), 
    choresrel=(choreswkly/ChoresBoth), hmgmtrel=(housemgmtwkly/HmgmtBoth), 
    ccrel=(ccwkly/CCBoth), pwrel=(pwwkly/PWBoth), pcrel= pcwkly/PCBoth, 
    upwrel=(upwwkly/UpwBoth), hcaprel=(hcapwkly/HcapBoth), 
    edurel=(eduwkly/EduBoth), increl=(NETWKTOT/IncBoth), 
    #caprel=(capIntwkly/CapBoth), timerel=(timeIntwkly/TimeBoth)
  ) %>% ungroup()

#bartik_couple %>% filter(pcrel>0.8) %>% count(pcrel)                           # check; 
bartik_couple %>% count(mmgrp)                                                  # check; 
bartik_couple %>% count(dg)                                                     # check; 
bartik_couple %>% count(prwmore) 
bartik_couple %>% count(upwrel)
bartik_couple %>% count(increl)
bartik_couple %>% filter(is.na(upwrel)) %>% count(upwrel)
bartik_couple %>% filter(!is.na(leisrel)) %>% count(leisrel)
bartik_couple %>% filter(is.na(upwratio)) %>% count(upwratio)
bartik_couple %>% count(anyChildren)
bartik_couple %>% count(child16)
bartik_couple %>% count(NETWKTOT)
bartik_couple %>% count(CASENOP)

#bartik_couple <- bartik_couple %>% 
#rowwise() %>% 
#mutate(
#CASENOP=sum(HSERIALP*1000+PERSNO)
#) %>% ungroup()


## rename practice_leissubgrp datasets compatible with bartik_couple 
practice_leissubgrp <- practice_leissubgrp %>% 
  rename(
    SEX=DMSex, PARTNERNO=partnerno, COUPLENO=coupleno, 
    HSERIALP=serial, PERSNO=pnum
  ) #%>% arrange(CASENOP) #%>% 
#select(
#SEX, PARTNERNO, COUPLENO, HSERIALP, PERSNO, 
#rest1:media1, a:l1, gendered, diff, same, feminine, masculine
#) # is the 'select()' part needed? 

#practice_leissubgrp %>% count(CASENOP)


## then join practice_leissubgrp with bartik_couple
practice_final <- bartik_couple %>% full_join(practice_leissubgrp) %>%
  group_by(HSERIALP, PERSNO) %>% 
  fill(leisrel, .direction="downup") %>% 
  fill(leis2rel, .direction="downup") %>% 
  fill(choresrel, .direction="downup") %>% 
  fill(hmgmtrel, .direction="downup") %>% 
  fill(ccrel, .direction="downup") %>% 
  fill(pwrel, .direction="downup") %>% 
  fill(pcrel, .direction="downup") %>% 
  fill(upwrel, .direction="downup") %>% 
  fill(hcaprel, .direction="downup") %>% 
  fill(edurel, .direction="downup") %>% 
  fill(increl, .direction="downup") %>% 
  fill(same, .direction="downup") %>%
  fill(diff, .direction="downup") %>%
  fill(prwmore, .direction="downup") %>%
  fill(COUPLENO, .direction="downup") %>%
  fill(PARTNERNO, .direction="downup") %>% 
  fill(feminine, .direction="downup") %>%
  fill(masculine, .direction="downup") %>%
  fill(jtime_ft, .direction="downup") %>%
  fill(jt_ftratio, .direction="downup") %>%
  fill(jtime_ftvol, .direction="downup") %>%
  fill(jt_ftvolratio, .direction="downup") %>% 
  ungroup() %>% 
  select(-c(female, male)) #%>% 
distinct(HSERIALP, PERSNO, .keep_all = T) %>% filter(!is.na(CASENOP))

#practice_final2 <- practice_final %>% distinct(HSERIALP, PERSNO, .keep_all=T) %>% 
#group_by(CASENOP) %>% add_tally(., name="duplicate") 
#any(practice_final2$duplicate>1)
#rm(practice_final2)

practice_final %>% filter(!is.na(jtime_ft)) %>% count(SEX)

practice_final %>% count(year)
practice_final1 <- practice_final %>% 
  group_by(CASENOP) %>% add_tally(., name="duplicate") %>% ungroup
any(practice_final1$duplicate>1) # must be [FALSE]
rm(practice_final1)





# bind three different datasets <= prob no need! ==== 
## before that we select vars that are relevant with instrumental variables

adult_allB <- adult_all %>% ungroup() %>% 
  select(
    HSERIALP, PERSNO,                                                           # identification of individuals 
    NETWKTOT,                                                                   # our income variable 
    SEX, AGE, NEDUGRP, NEDUGRP1, NAGEGRP, NCOUNTRY, INDSECT,                    # individual characteristics and new groupings 
    PARTNERNO, COUPLENO,                                                        # couple number and partner's number index for couples within household 
    anyChildren, child16, child15, child4,                                      # control variable for presence of children
    PAGE, PEDU1, PAGEGRP, PEDUGRP1  )    




## 2) then join them now 
practice_bartik <- prwm_both %>% full_join(couple)



# full_join tus, leistype dataset (mainly from data_v, data_w, data_l)

## rename tus_leistype dataset
tus_leistype <- tus_leistype %>%
  distinct(serial, pnum, .keep_all=T) %>% 
  rename(
    HSERIALP=serial, PERSNO=pnum, SEX==DMSex, COUPLENO=coupleno, 
    PARTNERNO=partnerno) %>%
  select()

#tus_leistype %>% group_by(rest1) %>% tally





# main empirical specification and estimation model ====

## 1) upwrel ~ prwmore + genderedleis + sameasgender + diffasgender + factor(mmgrp, year, child); 
## 2) leisratio ~ prwmore + genderedleis + sameasgender + diffasgender + factor(mmgrp, year, child); 
## 3) partnersatisfaction ~ prwmore + genderedleis + sameasgender + diffasgende + factor(mmgrp, year, child); 
## 4) 

## 1) upwrel = DV
practice_final <- practice_final %>% 
  rowwise() %>% 
  mutate(
    FEMALE=if_else(SEX==2, 1, 0),
    MALE = if_else(SEX==1, 1, 0)) %>% ungroup()

practice_finalf <- practice_final %>% filter(SEX==2)
practice_finalm <- practice_final %>% filter(SEX==1)
practice_final %>% count(year)

lm(jt_ftratio ~ prwmore_pth*FEMALE*pwratiowkly*upwratiowkly+factor(mmgrp)-1+factor(child4)-1, practice_final) %>% summary #reverse reg reports rsqrd=1; prwmore indeed has a very strong first stage. 
lm(jtime_ft ~ prwmore_pth*leis2wkly*MALE+factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary
lm(leisratio2wkly ~ prwmore_pth*upwratiowkly*pwratiowkly*FEMALE*jt_ftratio + factor(mmgrp)-1, practice_final) %>% summary

lm(leisratio2wkly ~ FEMALE*prwmore_pth*upwratiowkly*pwratiowkly*jt_ftratio*diff*increl + factor(mmgrp)+factor(child4)-1, practice_final) %>% summary

lm(leis2rel ~ FEMALE*prwmore_pth*pwrel*upwrel*increl*jt_ftratio+factor(mmgrp)+factor(child4)-1, practice_final) %>% summary


lm(leisrel ~ prwmore_pth*pwrel*FEMALE*jt_ftratio+factor(mmgrp)-1+factor(child4)-1, practice_final) %>% summary
lm(leisrel ~ prwmore_pth*pwrel*MALE*jt_ftratio+factor(mmgrp)-1+factor(child4)-1, practice_final) %>% summary

lm(diff ~ prwmore_pth*pwrel*FEMALE*jt_ftratio+factor(mmgrp)-1+factor(child4)-1, practice_final) %>% summary
lm(diff ~ FEMALE*prwmore_pth*upwratiowkly*pwratiowkly*leisratio2wkly*increl*jt_ftratio+factor(mmgrp)+factor(child4)-1, practice_final) %>% summary


#practice_final %>% count(leisrel)

lm(upwrel~prwmore_pth+factor(mmgrp)-1, practice_final) %>% summary #rsqred:0.7946, significant, coeff=1.4972452
lm(upwrel ~ prwmore_pth*pwrel*leis2rel*FEMALE*jt_ftratio*diff*increl + factor(mmgrp)+factor(child4)-1, practice_final) %>% summary # rsqrd: 0.8261, significant(*), coeff=-0.2023613
lm(upwrel ~ prwmore_pth*pwrel*leis2rel*jt_ftratio*both*increl + factor(mmgrp)+factor(child4)-1, practice_finalf) %>% summary # rsqrd: 0.8261, significant(*), coeff=-0.2023613
lm(upwrel ~ prwmore_pth*pwrel*leis2rel*jt_ftratio*diff*increl + factor(mmgrp)+factor(child4)-1, practice_finalf) %>% summary # rsqrd: 0.8261, significant(*), coeff=-0.2023613
lm(upwrel ~ prwmore_pth*pwrel*leis2rel*jt_ftratio*diff*increl + factor(mmgrp)+factor(child4)-1, practice_finalm) %>% summary # rsqrd: 0.8261, significant(*), coeff=-0.2023613
lm(upwwkly ~ prwmore_pth*pwwkly*leiswkly*FEMALE*jt_ftratio*diff + factor(mmgrp)+factor(child4)-1, practice_final) %>% summary #rsqred:0.7444,significant, coeff=0.4449
lm(upwratio ~ prwmore_pth*pwratiowkly*leisratio2wkly*FEMALE*jt_ftratio*both + factor(mmgrp)+factor(child4)-1, practice_final) %>% summary # rsqrd: 0.8261, significant(*), coeff=-0.2023613

lm(upwratio ~ prwmore + factor(mmgrp)-1, practice_finalf) %>% summary #rsqred:0.7444,significant, coeff=0.4449
lm(upwrel ~ prwmore+factor(mmgrp)-1, practice_finalf) %>% summary #rsqred:0.8856, significant, coeff=1.742334
lm(upwrel ~ prwmore_pth+factor(mmgrp)-1, practice_finalf) %>% summary #rsqrd:0.8856, significant, coeff=-0.06355
lm(upwrel ~ prwmore_pth+factor(mmgrp)-1, practice_finalf) %>% summary #rsqrd:0.8856, significant(*), coeff=-0.06355
lm(upwrel ~ prwmore_pth*FEMALE+factor(mmgrp)-1, practice_final) %>% summary #rsqrd:0.826, significant(*), coeff=-0.14021

lm(upwrel ~ diff*same*FEMALE+ factor(mmgrp)-1, practice_final) %>% summary
practice_final <- practice_final %>% 
  lm(upwrel ~ FEMALE*prwmore_pth*pwrel*leis2rel*increl*jt_ftratio*diff+factor(child4)+ factor(mmgrp)-1, practice_final) %>% summary
lm(upwrel ~ prwmore_pth*pwrel*leis2rel*increl*jt_ftratio*diff+factor(child4)+ factor(mmgrp)-1, practice_finalf) %>% summary
lm(upwrel ~ prwmore*diff + factor(child4)+factor(mmgrp)-1, practice_finalf) %>% summary

lm(upwrel ~ prwmore*diff*same+factor(mmgrp)-1, practice_finalf) %>% summary

lm(diff ~ prwmore + factor(mmgrp)-1, practice_final) %>% summary  #rsqrd: 0.6898, significant, coeff=1.35385
lm(diff ~ prwmore*FEMALE + factor(child4)+factor(mmgrp)-1, practice_final) %>% summary # rsqrd: 0.8269, significant, coeff=0.57837. 
lm(same ~ prwmore*FEMALE + factor(child4)+factor(mmgrp)-1, practice_final) %>% summary # rsqrd: 0.8542, significant, coeff=-0.82068. 
lm(diff ~ prwmore + factor(mmgrp)-1, practice_finalf) %>% summary #rsqrd: 0.4165, significant (*), coeff=0.825000
lm(same ~ prwmore+factor(mmgrp)-1, practice_finalf) %>% summary # rsqrd: 0.9995, significant, coeff=2.2
lm(diff*same ~ prwmore+factor(mmgrp)-1, practice_finalf) %>% summary #rsqrd:0.4165, significant, coeff=0.82500
lm(diff*same ~ prwmore*FEMALE*jt_ftratio*leis2rel*pwrel*upwrel+factor(mmgrp)-1+factor(anyChildren), practice_final) %>% summary # not significant. 

lm(diff ~ prwmore*MALE + factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary
lm(same ~ prwmore*MALE + factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary
lm(upwrel ~ prwmore*FEMALE + factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary
lm(upwrel ~ prwmore + factor(mmgrp)+factor(anyChildren)-1, practice_finalf) %>% summary #significant, coeff=1.74, rsqrd 0.8865
lm(upwrel ~ prwmore + factor(mmgrp)+factor(anyChildren)-1, practice_finalm) %>% summary # significant, coeff=1.10, rsqrd=0.7192
lm(pwrel ~ prwmore + factor(mmgrp)+factor(child4)-1, practice_finalm) %>% summary
lm(leisrel ~ prwmore*FEMALE+factor(mmgrp)+factor(child4)-1, practice_final) %>% summary


lm(increl ~ FEMALE*prwmore_pth*diff*jt_ftratio*pwrel*leis2rel*upwrel +factor(mmgrp)-1+factor(anyChildren), practice_final) %>% summary
lm(increl ~ FEMALE*prwmore_pth*diff*jt_ftratio + factor(child4)+factor(mmgrp)-1, practice_final) %>% summary
lm(increl ~ prwmore_pth*FEMALE + factor(mmgrp)-1, practice_final) %>% summary
lm(increl ~ prwmore + factor(mmgrp)+factor(anyChildren)-1, practice_finalf) %>% summary

lm(pwrel ~ prwmore*FEMALE + factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary

practice_final %>% filter(is.na(prwmore)) %>% count(prwmore, diff)

#check <- practice_final %>% filter(!is.na(COUPLENO), !is.na(PARTNERNO)) #n=5589. 
#check %>% count(same, diff, prwmore)
#check %>% filter(is.na(prwmore)) %>% count(prwmore, diff)
#rm(check)



lm(leisrel ~ prwmore*MALE+factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary #coeff=0.089231
lm(leisrel ~ prwmore*FEMALE+child16+factor(mmgrp)-1, practice_final) %>% summary #coeff=-0.089231
lm(leisrel ~ prwmore+child16+factor(mmgrp)-1, practice_final) %>% summary #coeff=1.37888
lm(leisrel ~ prwmore+child16+factor(mmgrp)-1, practice_final) %>% summary #coeff=1.13481

lm(leisratio~ prwmore+child16+factor(mmgrp)-1, practice_finalcouplef) %>% summary
lm(leisratio~prwmore+child16+factor(mmgrp)-1, practice_finalcouplem) %>% summary

lm(leisratio~prwmore+factor(mmgrp)-1, practice_finalf) %>% summary #rsqred:0.7908, significant, coeff=0.58246
lm(leisrel~prwmore+factor(mmgrp)-1, practice_finalf) %>% summary #rsqred:0.835, significant, coeff=1.13481
lm(pwrel~prwmore+factor(mmgrp)-1, practice_finalf) %>% summary #rsqred:0.5935, not significant, coeff=0.06286

lm(upwrel~prwmore+factor(mmgrp)-1, practice_finalm) %>% summary #rsqred:0.7173, significant, coeff=1.15
lm(upwratio~prwmore+factor(mmgrp)-1, practice_finalm) %>% summary # rsqred:0.5932, not significant, coeff=0.272817
lm(leisratio~prwmore+factor(mmgrp)-1, practice_finalm) %>% summary # rsqred:0.7843, significant, coeff=0.586667
lm(leisrel~prwmore+factor(mmgrp)-1, practice_finalm) %>% summary # rsqred:0.8868, significant, coeff=1.378880 ***** this is much more likely than women; counterintuitive. 
lm(pwrel~prwmore+factor(mmgrp)-1, practice_finalm) %>% summary #rsqred:0.7949, significant, coeff=2.2 ***** this is stronger than women; another evidence that gender identity plays a huge role. 
lm(pwratio~prwmore+factor(mmgrp)-1, practice_finalm) %>% summary #rsqred: 0.4602, not significant, coeff=0.2780

lm(upwratio ~ prwmore*FEMALE + factor(mmgrp)-1, practice_final) %>% summary #rsqred:0.6928, not significant, coeff=-0.0612213 
lm(upwrel ~ prwmore*FEMALE+factor(mmgrp)-1, practice_final) %>% summary #rsqrd:0.8261, significant coeff, coeff=-0.241770
lm(upwratio ~ prwmore + factor(mmgrp)-1, practice_final) %>% summary #rsqrd:0.6568, significant, coeff=0.593903
lm(upwrel~prwmore + factor(mmgrp)-1, practice_final) %>% summary #rsqrd:0.7946, significant coeff, coeff=2.67365

lm(pwratio ~ prwmore_pth*FEMALE+prwmore_pth*MALE+ factor(child4)+factor(mmgrp)-1, practice_final) %>% summary
lm(pwrel ~ prwmore_pth*FEMALE*upwrel*leis2rel*increl*diff*jt_ftratio+factor(anyChildren)+factor(mmgrp)-1, practice_final) %>% summary
lm(pwrel ~ prwmore_pth*upwrel*leis2rel*diff*jt_ftratio+factor(anyChildren)+factor(mmgrp)-1, practice_finalf) %>% summary

adult_all %>% count(dukcntr)
adult_all %>% count(NEDUGRP)
adult_all %>% count(NAGEGRP)
adult_all %>% count(INDSECT)

# 3) satisfaction 
practice_satisfaction <- practice_final %>% filter(SatPart>0, SatPart<8) 
satisfactionf <- practice_satisfaction %>% filter(SEX==2)
satisfactionm <- practice_satisfaction %>% filter(SEX==1)

lm(SatPart ~ prwmore_pth*FEMALE*upwrel*pwrel*jt_ftratio*diff+factor(child4)-1+factor(mmgrp)-1, practice_satisfaction) %>% summary
lm(SatPart ~ prwmore_pth*upwrel*pwrel*jt_ftratio*diff+factor(child4)-1+factor(mmgrp)-1, satisfactionf) %>% summary
lm(SatPart ~ prwmore*pwrel*upwrel*leis2rel*increl*same*jt_ftratio+factor(child4)-1+factor(mmgrp)-1, satisfactionm) %>% summary


#bartik_couple %>% count(SatInc)>0
#bartik_couple %>% count(SatSoc)
#bartik_couple %>% count(SatLeis) 
#bartik_couple %>% count(SatBal)>0
#bartik_couple %>% count(SatJob)<8

#bartik_couple <- bartik_couple %>% filter(SatJob>0, SatJob<8, SatBal>0, SatInc>0)
#bartik_couplef <- bartik_couple %>% filter(SEX==2)
#bartik_couplem <- bartik_couple %>% filter(SEX==1)

practice_leis <- practice_final %>% filter(SatLeis>0, SatLeis<8)
practice_bal <- practice_final %>% filter(SatBal>0, SatBal<8) 
practice_job <- practice_final %>% filter(SatJob>0, SatJob<8)

lm(SatLeis ~ prwmore*FEMALE*leisratio2wkly*upwratiowkly*pwratiowkly*diff*same*both + factor(mmgrp)+factor(anyChildren)-1, practice_leis) %>% summary #rsqrd:0.9186, significant coeff, coeff=23.1349; SE=2.0048
lm(SatLeis ~ prwmore+factor(mmgrp)-1, practice_leis) %>% summary #rsqrd: 0.542, significant coeff, coeff=15.71429, SE=4.53224 **** noisier, significantly less than women 

lm(SatBal ~ prwmore*FEMALE*leisratio2wkly*upwratiowkly*pwratiowkly + factor(mmgrp)+factor(anyChildren)-1, practice_bal) %>% summary #rsqrd: 0.9, significant (**), coeff=13.75, SE=4.3089 (noisy)
lm(SatBal ~ prwmore + factor(mmgrp)-1, practice_bal) %>% summary #rsqrd:0.359, not significant, coef==3.92857. 

lm(SatJob ~ prwmore*FEMALE*upwratiowkly*pwratiowkly*diff*same*both +factor(anyChildren)+ factor(mmgrp)-1, practice_job) %>% summary # rsqrd:0.9352, significant, coeff=13.7500, SE=3.9885. 
lm(SatJob ~ prwmore+ factor(mmgrp)-1, practice_job) %>% summary # rsqrd:0.5654, significant, coeff=18.52041, SE=5.27342. **rsqrd is smaller but coeff is much larger than woman. 


lm(jt_ftratio ~ prwmore*FEMALE*leisratio2wkly*upwratiowkly*pwratiowkly*both + factor(mmgrp)+factor(anyChildren)-1, practice_final) %>% summary



## plm() returning error; 
#a <- table(index(bartik_couple), useNA = "ifany") 
#a <- as.data.frame(a)
#a <- a %>% rename(freq=a[1,])
#a %>% filter(Freq!=1) %>% count(Freq) ## does not exist - all rows have non-NAs. 

#table(index(bartik_couple$mmgrp), useNA="ifany")

bartik_couple <- bartik_couple %>% filter(!is.na(mmgrp))
#bartik_couple$mmgrp <- as.character(bartik_couple$mmgrp)
#bartik_couple$anyChildren <- as.character(bartik_couple$anyChildren)
#bartik_couple$year <- as.character(bartik_couple$year)

any(table(index(bartik_couple$mmgrp), useNA = "ifany") > 1)
bartik_couple %>% filter(upwrel>0) %>% group_by(upwrel, prwmore) %>% tally
class(bartik_couple$mmgrp)
bartik_couple <- bartik_couple %>% filter(!is.na(mmgrp))
bartik_couple %>% filter(is.na(year)) %>% tally

plm(formula=upwrel ~ prwmore+factor(anyChildren), data=practice_final, #na.action=T, 
    index=c("mmgrp"), 
    model="within" )

pbartik_couple <- pdata.frame(bartik_couple, index=c("mmgrp", "anyChildren"))

any(table(index(bartik_couple$anyChildren), useNA="ifany")>1)


pdata <- pdata.frame(bartik_couple, index = c("anyChildren", "mmgrp"))
a<-index(pdata)


?plm()

bartik_couple %>% count(prwmore)

bartik_couple <- bartik_couple %>% filter(!is.na(mmgrp))

bartik_couple %>% filter(is.na(mmgrp)) %>% count(mmgrp)

plm(upwrel~prwmore, data=practice_bartik, model="within") %>% summary





# empirical analysis results to report commands 

## assortativeness in matching for marriage formation (coalition formation) 
## year 2014 total, assortativeness in equilibrium: for reporting purpose 
## AGE sorting 
age_2014F <- INCOME2014B %>%
  filter(!is.na(PARTNERNO), !is.na(COUPLENO)) %>%                               # restrict to couples only 
  filter(SEX==2) %>%                                                            # for female only; this is necessary and sufficient to calculate assortation by 'ratioage'
  rowwise() %>%
  mutate(
    MM_AGE=NAGEGRP*PAGEGRP
  ) %>%                                                                       # marriage market by age = indiv's age group x partner's age group = 16 subgroups with different results
  group_by(NAGEGRP) %>% add_tally(., name="AGEGRPSUB") %>%                      # mutate number of female per age grp
  group_by(NAGEGRP, MM_AGE) %>% add_tally(., name="MM_AGESUB") %>%              # mutate number of female per age marr. market
  select(HSERIALP, PERSNO, MM_AGESUB, AGEGRPSUB, PAGEGRP, 
         NAGEGRP, MM_AGE) %>%
  rowwise() %>%
  mutate(RATIOAGE=(MM_AGESUB/AGEGRPSUB)) %>%                                    # mutate ratio per age marr. market
  distinct(NAGEGRP, PAGEGRP, .keep_all=T) %>% 
  select(NAGEGRP, PAGEGRP, MM_AGE, MM_AGESUB, AGEGRPSUB, RATIOAGE)              # for reporting 



## EDU sorting
edu_2014F <- INCOME2014B %>% 
  filter(!is.na(COUPLENO), !is.na(PARTNERNO)) %>% 
  filter(SEX==2) %>%                                                            # restrict to each gender for calculation purpose (identify marr. market)
  mutate(
    MM_EDU = NEDUGRP1*PEDUGRP1
  ) %>%                                                                         # marriage market by edu = indiv's age group x partner's age group = 16 subgroups with different results 
  group_by(NEDUGRP1) %>% add_tally(., name="EDUGRPSUB") %>%                     # mutate number of female per edu grp
  group_by(NEDUGRP1, MM_EDU) %>% add_tally(., name="MM_EDUSUB") %>%             # mutate number of female per edu marr. market
  select(HSERIALP, PERSNO, MM_EDUSUB, EDUGRPSUB, PEDUGRP1) %>%
  rowwise() %>% 
  mutate(RATIOEDU=(MM_EDUSUB/EDUGRPSUB))  %>%                                   # mutate ratio per edu marr. market
  arrange(HSERIALP) %>%
  distinct(NEDUGRP1, PEDUGRP1, .keep_all=T) %>%
  select(NEDUGRP1, PEDUGRP1, MM_EDU, MM_EDUSUB, EDUGRPSUB, RATIOEDU)



## heterogeneity across demographic groups
# explore data -> not sure about this part 
coplot(NETWKTOT ~ SEX|NEDUGRP, type="l", check_both)
coplot(NETWKTOT ~ SEX|NAGEGRP, type="b", check_both)
scatterplot(NETWKTOT ~ SEX|NEDUGRP, boxplots=F, smooth=T, reg.line=F, check_both)


# visualization
library(gplots)
plotmeans(
  NETWKTOT ~ mmgrp, main="Heterogeneity across Marriage Market", 
  xlab="Marriage  Market", ylab="Weekly Labor Income", n.label=F, 
  check_both)
plotmeans(w_mtact ~ mmgrp, main="Heterogeneity across Marriage Market", 
          xlab="Marriage  Market", ylab="Weekly Labor Income", n.label=F, 
          check_both)
plotmeans(NETWKTOT ~ dg, main="Heterogeneity across Demographic Group", 
          xlab="Demographic Group", ylab="Weekly Labor Income", n.label=F, 
          INCOME2014B)
plotmeans(NETWKTOT ~ NAGEGRP, main="Heterogeneity across Age Group", 
          xlab="Age Groups", ylab="Weekly Labor Income", n.label=F, 
          INCOME2014B)
plotmeans(NETWKTOT ~ NCOUNTRY, main="Heterogeneity across Region",
          xlab="Regional Groups", ylab="Weekly Labor Income", INCOME2014B)
plotmeans(NETWKTOT ~ NEDUGRP, main="heterogeneity across Qualificatioin",
          xlab="Education Groups", ylab="Weekly Labor Income", INCOME2014B)
plotmeans(NETWKTOT ~ INDSECT, main="Heterogeneity across Industry for Women", 
          xlab="Industry", ylab="Weekly Labor Income", INCOME2014Bf)
plotmeans(NETWKTOT ~ INDSECT, main="Heterogeneity across Industry for Men", 
          xlab="Industry", ylab="Weekly Labor Income", INCOME2014Bm)

coplot(NETWKTOT ~ INDSECT|SEX, type="b", INCOME2014B)
scatterplot(NETWKTOT ~ NEDUGRP|NCOUNTRY, botplots=F, smooth=F, reg.line=F, INCOME2014B)
detach("package:gplots")
?plotmeans()



## non-percentile first stage
##MAIN with all fixed effects (except relative income): rsqrd>0.95, p<0.001, coeff=1.69141
check_bothf <- check_both %>% filter(SEX==2)
check_bothm <- check_both %>% filter(SEX==1)

wopercall <- lm(w_mtact2 ~ w_dg*ratio_er+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) 
wopercf <- lm(w_mtact2 ~ w_dg*ratio_er+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_bothf) 
wopercm <- lm(w_mtact2 ~ w_dg*ratio_er+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_bothm) 
summary(wopercf)
summary(wopercm)
summary(wopercall)

lm(w_mtact ~ w_mtpreder+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_mtpreder+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_bothf) %>% summary
lm(w_mtact ~ w_mtpreder+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_bothm) %>% summary
lm(w_mtact2 ~ w_mtpreder2+factor(year)+ factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpreder2+factor(year)+ factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_bothf) %>% summary
lm(w_mtact2 ~ w_mtpreder2+factor(year)+ factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_bothm) %>% summary
lm(w_mtact2 ~ w_mtpreder_p+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpreder+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact ~ w_dg*ratio_er+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary
lm(w_mtact2 ~ w_mtpredjg2+factor(year)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)+factor(mmgrp)-1, check_both) %>% summary


# percentile first stage
## 2018
m5<- lm(wmtact08_005 ~ wdg08_005*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary # act correlated with predicted bartik
m10 <- lm(wmtact08_010 ~ wdg08_010*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
m30 <- lm(wmtact08_030 ~ wdg08_030*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
m50 <- lm(wmtact08_050 ~ wdg08_050*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
m70 <- lm(wmtact08_070 ~ wdg08_070*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
m90 <- lm(wmtact08_090 ~ wdg08_090*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary
m95 <- lm(wmtact08_095 ~ wdg08_095*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary

lm(wmtact08_010 ~ wdg08_010*ratio_er + factor(mmgrp)-1, INCOME2008) %>% summary 

practice08 <- INCOME2008 %>% 
  select(CASENOP, HSERIALP, PERSNO, SEX, wmtact08_005:wmtact08_095, 
         wdg08_005:wdg08_095, wmtpred08_005:wmtpred08_095, 
         wmtpred08_005_2:wmtpred08_095_2, ratio_er, mmgrp, year, 
         NEDUGRP, NAGEGRP, NCOUNTRY, INDSECT) %>%
  rename(
    wmtact_005=wmtact08_005, wmtact_010=wmtact08_010, wmtact_015=wmtact08_015, 
    wmtact_020=wmtact08_020, wmtact_025=wmtact08_025, wmtact_030=wmtact08_030, 
    wmtact_035=wmtact08_035, wmtact_040=wmtact08_040, wmtact_045=wmtact08_045, 
    wmtact_050=wmtact08_050, wmtact_055=wmtact08_055, wmtact_060=wmtact08_060, 
    wmtact_065=wmtact08_065, wmtact_070=wmtact08_070, wmtact_075=wmtact08_075, 
    wmtact_080=wmtact08_080, wmtact_085=wmtact08_085, wmtact_090=wmtact08_090, 
    wmtact_095=wmtact08_095, 
    wdg_005=wdg08_005, wdg_010=wdg08_010, wdg_015=wdg08_015, 
    wdg_020=wdg08_020, wdg_025=wdg08_025, wdg_030=wdg08_030, 
    wdg_035=wdg08_035, wdg_040=wdg08_040, wdg_045=wdg08_045, 
    wdg_050=wdg08_050, wdg_055=wdg08_055, wdg_060=wdg08_060, 
    wdg_065=wdg08_065, wdg_070=wdg08_070, wdg_075=wdg08_075, 
    wdg_080=wdg08_080, wdg_085=wdg08_085, wdg_090=wdg08_090, 
    wdg_095=wdg08_095, 
    wmtpred_005=wmtpred08_005, wmtpred_010=wmtpred08_010, wmtpred_015=wmtpred08_015, 
    wmtpred_020=wmtpred08_020, wmtpred_025=wmtpred08_025, wmtpred_030=wmtpred08_030, 
    wmtpred_035=wmtpred08_035, wmtpred_040=wmtpred08_040, wmtpred_045=wmtpred08_045, 
    wmtpred_050=wmtpred08_050, wmtpred_055=wmtpred08_055, wmtpred_060=wmtpred08_060, 
    wmtpred_065=wmtpred08_065, wmtpred_070=wmtpred08_070, wmtpred_075=wmtpred08_075, 
    wmtpred_080=wmtpred08_080, wmtpred_085=wmtpred08_085, wmtpred_090=wmtpred08_090, 
    wmtpred_095=wmtpred08_095,
    wmtpred_005_2=wmtpred08_005_2, wmtpred_010_2=wmtpred08_010_2, wmtpred_015_2=wmtpred08_015_2, 
    wmtpred_020_2=wmtpred08_020_2, wmtpred_025_2=wmtpred08_025_2, wmtpred_030_2=wmtpred08_030_2, 
    wmtpred_035_2=wmtpred08_035_2, wmtpred_040_2=wmtpred08_040_2, wmtpred_045_2=wmtpred08_045_2, 
    wmtpred_050_2=wmtpred08_050_2, wmtpred_055_2=wmtpred08_055_2, wmtpred_060_2=wmtpred08_060_2, 
    wmtpred_065_2=wmtpred08_065_2, wmtpred_070_2=wmtpred08_070_2, wmtpred_075_2=wmtpred08_075_2, 
    wmtpred_080_2=wmtpred08_080_2, wmtpred_085_2=wmtpred08_085_2, wmtpred_090_2=wmtpred08_090_2, 
    wmtpred_095_2=wmtpred08_095_2,
  )

practice14 <- INCOME2014B %>% 
  select(CASENOP, HSERIALP, PERSNO, SEX, wmtact14_005:wmtact14_095, 
         wdg14_005:wdg14_095, wmtpred14_005:wmtpred14_095, 
         wmtpred14_005_2:wmtpred14_095_2, ratio_er, mmgrp, year, 
         NEDUGRP, NAGEGRP, NCOUNTRY, INDSECT
  ) %>%
  rename(
    wmtact_005=wmtact14_005, wmtact_010=wmtact14_010, wmtact_015=wmtact14_015, 
    wmtact_020=wmtact14_020, wmtact_025=wmtact14_025, wmtact_030=wmtact14_030, 
    wmtact_035=wmtact14_035, wmtact_040=wmtact14_040, wmtact_045=wmtact14_045, 
    wmtact_050=wmtact14_050, wmtact_055=wmtact14_055, wmtact_060=wmtact14_060, 
    wmtact_065=wmtact14_065, wmtact_070=wmtact14_070, wmtact_075=wmtact14_075, 
    wmtact_080=wmtact14_080, wmtact_085=wmtact14_085, wmtact_090=wmtact14_090, 
    wmtact_095=wmtact14_095, 
    wdg_005=wdg14_005, wdg_010=wdg14_010, wdg_015=wdg14_015, 
    wdg_020=wdg14_020, wdg_025=wdg14_025, wdg_030=wdg14_030, 
    wdg_035=wdg14_035, wdg_040=wdg14_040, wdg_045=wdg14_045, 
    wdg_050=wdg14_050, wdg_055=wdg14_055, wdg_060=wdg14_060, 
    wdg_065=wdg14_065, wdg_070=wdg14_070, wdg_075=wdg14_075, 
    wdg_080=wdg14_080, wdg_085=wdg14_085, wdg_090=wdg14_090, 
    wdg_095=wdg14_095, 
    wmtpred_005=wmtpred14_005, wmtpred_010=wmtpred14_010, wmtpred_015=wmtpred14_015, 
    wmtpred_020=wmtpred14_020, wmtpred_025=wmtpred14_025, wmtpred_030=wmtpred14_030, 
    wmtpred_035=wmtpred14_035, wmtpred_040=wmtpred14_040, wmtpred_045=wmtpred14_045, 
    wmtpred_050=wmtpred14_050, wmtpred_055=wmtpred14_055, wmtpred_060=wmtpred14_060, 
    wmtpred_065=wmtpred14_065, wmtpred_070=wmtpred14_070, wmtpred_075=wmtpred14_075, 
    wmtpred_080=wmtpred14_080, wmtpred_085=wmtpred14_085, wmtpred_090=wmtpred14_090, 
    wmtpred_095=wmtpred14_095,
    wmtpred_005_2=wmtpred14_005_2, wmtpred_010_2=wmtpred14_010_2, wmtpred_015_2=wmtpred14_015_2, 
    wmtpred_020_2=wmtpred14_020_2, wmtpred_025_2=wmtpred14_025_2, wmtpred_030_2=wmtpred14_030_2, 
    wmtpred_035_2=wmtpred14_035_2, wmtpred_040_2=wmtpred14_040_2, wmtpred_045_2=wmtpred14_045_2, 
    wmtpred_050_2=wmtpred14_050_2, wmtpred_055_2=wmtpred14_055_2, wmtpred_060_2=wmtpred14_060_2, 
    wmtpred_065_2=wmtpred14_065_2, wmtpred_070_2=wmtpred14_070_2, wmtpred_075_2=wmtpred14_075_2, 
    wmtpred_080_2=wmtpred14_080_2, wmtpred_085_2=wmtpred14_085_2, wmtpred_090_2=wmtpred14_090_2, 
    wmtpred_095_2=wmtpred14_095_2,
  )

practice_0814 <- practice08 %>% bind_rows(practice14)

practice_0814f <- practice_0814 %>% filter(SEX==2)
practice_0814m <- practice_0814 %>% filter(SEX==1)



practice_0814f %>% count(year)
f1<- lm(wmtact_005 ~ wdg_005*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f) 
f2<- lm(wmtact_020 ~ wdg_020*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f) 
f3 <- lm(wmtact_035 ~ wdg_035*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f)
f4 <- lm(wmtact_050 ~ wdg_050*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f) 
f5 <- lm(wmtact_065 ~ wdg_065*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f) 
f6 <- lm(wmtact_080 ~ wdg_080*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f) 
f7 <- lm(wmtact_095 ~ wdg_095*ratio_er + factor(mmgrp)+factor(year*NEDUGRP*NAGEGRP*NCOUNTRY)-1, practice_0814f) 
summary(f1)
summary(f2)
summary(f3)
summary(f4)
summary(f5)
summary(f6)
summary(f7)

pl=c(
  "wdg_005:ratio_er"="5th", "wdg_020:ratio_er"="20th", "wdg_035:ratio_er"="35th",
  "wdg_050:ratio_er"="50th", "wdg_065:ratio_er"="65th", "wdg_080:ratio_er"="80th",
  "wdg_095:ratio_er"="95th"
)
tab_model(f1, f2, f3, f4, f5, f6, f7, 
          auto.label=F, 
          pred.labels=pl,
          show.ci=F, collapse.se=T, p.style="stars", 
          show.std=T
)

m1<- lm(wmtact_005 ~ wmtpred_005 + factor(mmgrp)+factor(year)-1, practice_0814m) 
m2<- lm(wmtact_020 ~ wmtpred_020 + factor(mmgrp)+factor(year)-1, practice_0814m) 
m3 <- lm(wmtact_035 ~ wmtpred_035 + factor(mmgrp)+factor(year)-1, practice_0814m)
m4 <- lm(wmtact_050 ~ wmtpred_050 + factor(mmgrp)+factor(year)-1, practice_0814m)
m5 <- lm(wmtact_065 ~ wmtpred_065 + factor(mmgrp)+factor(year)-1, practice_0814m)
m6 <- lm(wmtact_085 ~ wmtpred_085 + factor(mmgrp)+factor(year)-1, practice_0814m) 
m7 <- lm(wmtact_095 ~ wmtpred_095 + factor(mmgrp)+factor(year)-1, practice_0814m) 
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)

pl2=c(
  "wmtpred_005"="5th", "wmtpred_020"="20th", "wmtpred_035"="35th",
  "wmtpred_050"="50th", "wmtpred_065"="65th", "wmtpred_080"="80th",
  "wmtpred_095"="95th"
)

tab_model(m1, m2, m3, m4, m5, m6, m7, 
          auto.label=F, 
          pred.labels=pl2,
          show.ci=F, collapse.se=T, p.style="stars", 
          show.std=T)

tab_model(wopercall, wopercf, wopercm,
          #pred.labels=pl, 
          auto.label=T, 
          show.ci = F, 
          collapse.se=T,
          p.style="stars", 
          show.std=T,
          show.stat=T)


#per-percentile group graphs
new <- lm(wmtact_005 ~ wmtpred_005+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred = wmtpred_005, interval = TRUE, plot.points = TRUE, jitter = 0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_005"), scale=T, robust=T)
new <- lm(wmtact_005 ~ wmtpred_005+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_005, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_005"), scale=T, robust=T)

new <- lm(wmtact_020 ~ wmtpred_020+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred=wmtpred_020, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_020"), scale=T, robust=T)
new <- lm(wmtact_020 ~ wmtpred_020+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_020, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_020"), scale=T, robust=T)

new <- lm(wmtact_035 ~ wmtpred_035+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred=wmtpred_035, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_035"), scale=T, robust=T)
new <- lm(wmtact_035 ~ wmtpred_035+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_035, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_035"), scale=T, robust=T)

new <- lm(wmtact_050 ~ wmtpred_050+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred=wmtpred_050, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_050"), scale=T, robust=T)
new <- lm(wmtact_050 ~ wmtpred_050+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_050, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_050"), scale=T, robust=T)

new <- lm(wmtact_065 ~ wmtpred_065+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred=wmtpred_065, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_065"), scale=T, robust=T)
new <- lm(wmtact_065 ~ wmtpred_065+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_065, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_065"), scale=T, robust=T)

new <- lm(wmtact_080 ~ wmtpred_080+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred=wmtpred_080, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_080"), scale=T, robust=T)
new <- lm(wmtact_080 ~ wmtpred_080+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_080, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_080"), scale=T, robust=T)

new <- lm(wmtact_095 ~ wmtpred_095+factor(mmgrp)-1, practice_0814f)
effect_plot(new, pred=wmtpred_095, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_095"), scale=T, robust=T)
new <- lm(wmtact_095 ~ wmtpred_095+factor(mmgrp)-1, practice_0814m)
effect_plot(new, pred=wmtpred_095, interval=T, plot.points=T, jitter=0.05)
plot_summs(new, coeffs=c("predicted wage"="wmtpred_095"), scale=T, robust=T)









