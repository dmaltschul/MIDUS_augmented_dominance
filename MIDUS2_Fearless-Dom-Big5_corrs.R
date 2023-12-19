## ---------------------------
##
## Script name: MIDUS2_Fearless-Dom-Big5_corrs
##
## Purpose of script: A single workflow that takes personality data from the
## MIDUS2 sample and examines the correlations between dominance ("agency") and
## the Big 5 (as measured by the MIDI and MPI).
##
## Author: Dr Drew M Altschul
##
## Date Created: 2023-12-04
##
## Copyright (c) Drew M Altschul, 2023
## Email: dmaltschil@gmail.com
##
## ---------------------------
##
## Notes: Users should set their own working directory, and incorporate their 
##  own functions for importing MIDUS2 data file formats (e.g. SAS, SPSS).
##
## ---------------------------

## set working directory for Mac and PC

#setwd("")      # For the user to implement.

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## load up the packages we will need: 
require(data.table)
require(Hmisc)
require(lavaan)
require(psych)

## ---------------------------


### Read-in and process MIDUS2 data

## Replace the line below with code to load MIDUS2 project1
midus2 <- as.data.table(readRDS('./data/merged.RDS'))
gc()


vars=c( # the variables to select out of the entire dataset
  'M2ID',
  # Personality
  'B1SAGENC','B1SEXTRA','B1SAGREE','B1SCONS2','B1SOPEN','B1SNEURO',
  
  'B1SE6A', #Outgoing'
  'B1SE6B', #Helpful'
  'B1SE6C', #Moody',
  'B1SE6D', #Organized'
  'B1SE6E', #Selfconfident'
  'B1SE6F', #Friendly'
  'B1SE6G', #Warm'
  'B1SE6H', #Worrying'
  'B1SE6I',#Responsible',
  'B1SE6J',#Forceful'
  'B1SE6K',#Lively',
  'B1SE6L',#Caring',
  'B1SE6M', #Nervous'
  'B1SE6N', #Creative'
  'B1SE6O', #Assertive',
  'B1SE6P', #Hardworking',
  'B1SE6Q', #Imaginative',
  'B1SE6R', #Softhearted',
  'B1SE6S', #Calm'
  'B1SE6T', #Outspoken',
  'B1SE6U', #Intelligent',
  'B1SE6V', #Curious',
  'B1SE6W', #Active',
  'B1SE6X', #Careless',
  'B1SE6Y', #Broadminded','
  'B1SE6Z', #Sympathetic',
  'B1SE6AA', #Talkative',
  'B1SE6BB', #Sophisticated',
  'B1SE6CC', #Adventurous',
  'B1SE6DD', #Dominant'
  'B1SE6EE', #Thorough
  #MPI EXTRAS
  'B1SE7E', #IN MOST SOCIAL SITUATIONS I LIKE TO HAVE SOMEONE ELSE TAKE THE LEAD
  'B1SE7J', #I AM QUITE EFFECTIVE AT TALKING PEOPLE INTO THINGS.
  'B1SE7N', #I AM VERY GOOD AT INFLUENCING PEOPLE
  'B1SE7DD', #WHEN IT IS TIME TO MAKE DECISIONS, OTHERS USUALLY TURN TO ME
  #PWB EXTRA
  'B1SE1A' #I AM NOT AFRAID TO VOICE MY OPINIONS, EVEN WHEN THEY ARE IN OPPOSITION TO THE OPINIONS OF MOST PEOPLE
)

midus2 = midus2[, ..vars] # remove extraneous variables


## Cleaning 
midus2$B1SEXTRA[midus2$B1SEXTRA ==8] <- NA
midus2$B1SAGREE[midus2$B1SAGREE ==8] <- NA
midus2$B1SCONS2[midus2$B1SCONS2 ==8] <- NA
midus2$B1SOPEN[midus2$B1SOPEN ==8] <- NA
midus2$B1SNEURO[midus2$B1SNEURO ==8] <- NA
midus2$B1SAGENC[midus2$B1SAGENC ==8] <- NA

midus2$B1SE6A[midus2$B1SE6A ==8] <- NA
midus2$B1SE6B[midus2$B1SE6B ==8] <- NA
midus2$B1SE6C[midus2$B1SE6C ==8] <- NA
midus2$B1SE6D[midus2$B1SE6D ==8] <- NA
midus2$B1SE6E[midus2$B1SE6E ==8] <- NA
midus2$B1SE6F[midus2$B1SE6F ==8] <- NA
midus2$B1SE6G[midus2$B1SE6G ==8] <- NA
midus2$B1SE6H[midus2$B1SE6H ==8] <- NA
midus2$B1SE6I[midus2$B1SE6I ==8] <- NA
midus2$B1SE6J[midus2$B1SE6J ==8] <- NA
midus2$B1SE6K[midus2$B1SE6K ==8] <- NA
midus2$B1SE6L[midus2$B1SE6L ==8] <- NA
midus2$B1SE6M[midus2$B1SE6M ==8] <- NA
midus2$B1SE6N[midus2$B1SE6N ==8] <- NA
midus2$B1SE6O[midus2$B1SE6O ==8] <- NA
midus2$B1SE6P[midus2$B1SE6P ==8] <- NA
midus2$B1SE6Q[midus2$B1SE6Q ==8] <- NA
midus2$B1SE6R[midus2$B1SE6R ==8] <- NA
midus2$B1SE6S[midus2$B1SE6S ==8] <- NA
midus2$B1SE6T[midus2$B1SE6T ==8] <- NA
midus2$B1SE6U[midus2$B1SE6U ==8] <- NA
midus2$B1SE6V[midus2$B1SE6V ==8] <- NA
midus2$B1SE6W[midus2$B1SE6W ==8] <- NA
midus2$B1SE6X[midus2$B1SE6X ==8] <- NA
midus2$B1SE6Y[midus2$B1SE6Y ==8] <- NA
midus2$B1SE6Z[midus2$B1SE6Z ==8] <- NA
midus2$B1SE6AA[midus2$B1SE6AA ==8] <- NA
midus2$B1SE6BB[midus2$B1SE6BB ==8] <- NA
midus2$B1SE6CC[midus2$B1SE6CC ==8] <- NA
midus2$B1SE6DD[midus2$B1SE6DD==8] <- NA
midus2$B1SE6EE[midus2$B1SE6EE ==8] <- NA
midus2$B1SE7E[midus2$B1SE7E ==8] <- NA
midus2$B1SE7J[midus2$B1SE7J ==8] <- NA
midus2$B1SE7N[midus2$B1SE7N ==8] <- NA
midus2$B1SE7DD[midus2$B1SE7DD ==8] <- NA
midus2$B1SE1A[midus2$B1SE1A ==8] <- NA



newnames = c('M2ID', # for renaming the MIDUS2 var codes to informative labels
             'Dom','Ext','Agr','Con','Opn','Neu',
             'Outgoing','Helpful','Moody','Organized','Selfconfident','Friendly','Warm','Worrying',
             'Responsible','Forceful','Lively','Caring','Nervous','Creative','Assertive','Hardworking',
             'Imaginative','Softhearted','Calm','Outspoken','Intelligent','Curious','Active','Careless',
             'Broadminded','Sympathetic','Talkative','Sophisticated','Adventurous','Dominant','Thorough',
             'like_to_lead','talk_people_into','influencing','decisions','not_afraid_to_speak')

setnames(midus2,vars,newnames)


persCols = c('Outgoing','Helpful','Moody','Organized','Selfconfident','Friendly','Warm','Worrying',
             'Responsible','Forceful','Lively','Caring','Nervous','Creative','Assertive','Hardworking',
             'Imaginative','Softhearted','Calm','Outspoken','Intelligent','Curious','Active','Careless',
             'Broadminded','Sympathetic','Talkative','Sophisticated','Adventurous','Dominant','Thorough',
             'like_to_lead','talk_people_into','influencing','decisions')
# reverse code relevant items
midus2[,(persCols):= 5 - midus2[,..persCols]]

midus2$not_afraid_to_speak = 8 - midus2$not_afraid_to_speak



### Basic correlation analyses
## using the pre-constructed variables

cormat1 = rcorr(x=as.matrix(midus2[,c('Dom','Ext','Agr','Con','Opn','Neu')]), type="spearman")

cormat1$r



### CFA correlations

## Building up the constructs, starting with D
m.D.1 <-'
D =~ Dominant + Outspoken + Assertive + Forceful + Selfconfident 

'

f.D.1 = cfa(m.D.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.D.1, fit.measures = c('chisq','df','SRMR','CFI'))


m.E.1 <-'
E =~ Lively + Friendly + Active + Talkative + Outgoing

'

f.E.1 = cfa(m.E.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.E.1, fit.measures = c('chisq','df','SRMR','CFI'))


m.O.1 <-'
O =~ Creative + Imaginative + Curious + Broadminded + Intelligent + Adventurous + Sophisticated

'

m.O.1 = cfa(m.O.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.O.1, fit.measures = c('chisq','df','SRMR','CFI'))


m.C.1 <- '
C =~ Organized + Hardworking + Careless + Thorough + Responsible

'

f.C.1 = cfa(m.C.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.C.1, fit.measures = c('chisq','df','SRMR','CFI'))


m.A.1 <- '
A =~ Helpful + Caring + Softhearted + Sympathetic + Warm

'

f.A.1 = cfa(m.A.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.A.1, fit.measures = c('chisq','df','SRMR','CFI'))


m.N.1 <- '
N =~ Nervous + Worrying + Moody + Calm

'

f.N.1 = cfa(m.N.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.N.1, fit.measures = c('chisq','df','SRMR','CFI'))


## All-together now

m.all.1 <- '
D =~ Dominant + Outspoken + Assertive + Forceful + Selfconfident
E =~ Lively + Friendly + Active + Talkative + Outgoing
O =~ Creative + Imaginative + Curious + Broadminded + Intelligent + Adventurous #+ Sophisticated
C =~ Organized + Hardworking + Careless + Thorough + Responsible
A =~ Helpful + Caring + Softhearted + Sympathetic + Warm
N =~ Nervous + Worrying + Moody + Calm
'

f.all.1 = cfa(m.all.1, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.all.1, fit.measures = c('chisq','df','SRMR','CFI'))


## O variants
m.O.1a <-'
O =~ Creative + Imaginative + Curious + Broadminded + Intelligent + Adventurous

'
f.O.1a = cfa(m.O.1a, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1a, fit.measures = c('chisq','df','SRMR','CFI'))

m.O.1b <-'
O =~ Creative + Imaginative + Curious + Broadminded + Intelligent + Sophisticated

'
f.O.1b = cfa(m.O.1b, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1b, fit.measures = c('chisq','df','SRMR','CFI'))

m.O.1c<-'
O =~ Creative + Imaginative + Curious + Broadminded + Adventurous + Sophisticated

'
f.O.1c = cfa(m.O.1c, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1c, fit.measures = c('chisq','df','SRMR','CFI'))

m.O.1d <-'
O =~ Creative + Imaginative + Curious + Intelligent + Adventurous + Sophisticated

'
f.O.1d = cfa(m.O.1d, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1d, fit.measures = c('chisq','df','SRMR','CFI'))

m.O.1e <-'
O =~ Creative + Imaginative + Broadminded + Intelligent + Adventurous + Sophisticated

'
f.O.1e = cfa(m.O.1e, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1e, fit.measures = c('chisq','df','SRMR','CFI'))

m.O.1f <-'
O =~ Creative + Curious + Broadminded + Intelligent + Adventurous + Sophisticated

'
f.O.1f = cfa(m.O.1f, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1f, fit.measures = c('chisq','df','SRMR','CFI'))

m.O.1g <-'
O =~ Imaginative + Curious + Broadminded + Intelligent + Adventurous + Sophisticated

'
f.O.1g = cfa(m.O.1g, midus2, estimator="WLSMV", ordered=TRUE)
fitMeasures(f.O.1g, fit.measures = c('chisq','df','SRMR','CFI'))

# model f ('imaginative' removed) is the best new fit


## Retry broader model
m.all.1a <- '
D =~ Dominant + Outspoken + Assertive + Forceful + Selfconfident
E =~ Lively + Friendly + Active + Talkative + Outgoing
O =~ Creative + Curious + Broadminded + Intelligent + Adventurous + Sophisticated
C =~ Organized + Hardworking + Careless + Thorough + Responsible
A =~ Helpful + Caring + Softhearted + Sympathetic + Warm
N =~ Nervous + Worrying + Moody + Calm
'

f.all.1a = cfa(m.all.1a, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.all.1a, fit.measures = c('chisq','df','SRMR','CFI')) # 



## Building up D with more indicators
m.D.2 <-'
D =~ Dominant + Outspoken + Assertive + Forceful + Selfconfident +
not_afraid_to_speak + like_to_lead + talk_people_into + decisions

'

f.D.2 = cfa(m.D.2, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.D.2, fit.measures = c('chisq','df','SRMR','CFI'))

summary(f.D.2)



m.all.2 <- '
D =~ Dominant + Outspoken + Assertive + Forceful + Selfconfident +
not_afraid_to_speak + like_to_lead + talk_people_into + decisions
E =~ Lively + Friendly + Active + Talkative + Outgoing
O =~ Creative + Curious + Broadminded + Sophisticated + Intelligent + Adventurous
C =~ Organized + Hardworking + Careless + Thorough + Responsible
A =~ Helpful + Caring + Softhearted + Sympathetic + Warm
N =~ Nervous + Worrying + Moody + Calm
'


f.all.2 = cfa(m.all.2, midus2, estimator="WLSMV", ordered=TRUE)

fitMeasures(f.all.2, fit.measures = c('chisq','df','SRMR','CFI'))

summary(f.all.2)



### Exploratory analyses

## Correlation matrix with adjusted D and O vars

midus2$OpnNew = midus2$Opn*7 - midus2$Imaginative

midus2$DomNew = midus2$Dominant + midus2$Outspoken + midus2$Assertive + 
  midus2$Forceful + midus2$Selfconfident + midus2$not_afraid_to_speak +
  midus2$decisions + midus2$talk_people_into + midus2$like_to_lead
 
#cor(midus2$Dom, midus2$DomNew, use="pairwise.complete")


cormat2 = rcorr(x=as.matrix(midus2[,c('DomNew','Ext','Agr','Con','OpnNew','Neu')]), type="spearman")

cormat2$r


## Omega analysis with original and augmented D

omega(midus2[,c('Dominant','Outspoken','Assertive','Forceful','Selfconfident')])

omega(midus2[,c('Dominant','Outspoken','Assertive','Forceful','Selfconfident',
               'not_afraid_to_speak','like_to_lead','talk_people_into','decisions')])



