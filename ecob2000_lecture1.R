#Marvin Harricharran 
#Hw 1

x <- 1:50
w <- 1 + sqrt(x)/2
example1 <- data.frame(x=x, y= x + rnorm(x)*w)
attach(example1)

fm <- lm(y ~ x)
summary(fm)

## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.5372 -1.5831  0.2436  1.4965  7.8226 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.45583    0.90898  -0.501    0.618    
## x            0.99171    0.03102  31.967   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.166 on 48 degrees of freedom
## Multiple R-squared:  0.9551, Adjusted R-squared:  0.9542 
## F-statistic:  1022 on 1 and 48 DF,  p-value: < 2.2e-16

lrf <- lowess(x, y)
plot(x, y)
lines(x, lrf$y)
abline(0, 1, lty=3)
abline(coef(fm))

detach()


getwd()
setwd("C:/Users/marvi/Downloads/Household_Pulse_data_ph4c2")

load("Household_Pulse_data_ph4c2.RData")
load("Household_Pulse_data_ph4c2.RData")
#glimpse(acs2017_ny) try this later
Household_Pulse_data[1:10,1:6]
attach(Household_Pulse_data)
summary(Household_Pulse_data)

summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])
# here i want to find average ages of men and women
mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])

# TBIRTH_YEAR    RHISPANIC RRACE     EEDUC        MS EGENID_BIRTH
# 1         1976 Not Hispanic Black  bach deg separated       female
# 2         1961 Not Hispanic White assoc deg  divorced       female
# 3         1988 Not Hispanic White   adv deg   married       female
# 4         1956 Not Hispanic White assoc deg   married         male
# 5         1970 Not Hispanic White  bach deg   married         male
# 6         1984 Not Hispanic White   adv deg   married       female
# 7         1968     Hispanic White some coll  divorced         male
# 8         1967 Not Hispanic White   adv deg   married         male
# 9         1972 Not Hispanic White some coll   married       female
# 10        1956 Not Hispanic White  bach deg   married         male
# > attach(Household_Pulse_data)
# > summary(Household_Pulse_data)
# TBIRTH_YEAR          RHISPANIC       RRACE                EEDUC               MS        EGENID_BIRTH  
# Min.   :1936   Not Hispanic:64658   White:57562   less than hs:  585   NA       :  477   male  :31510  
# 1st Qu.:1957   Hispanic    : 6494   Black: 6345   some hs     : 1282   married  :39339   female:39642  
# Median :1969                        Asian: 3379   HS diploma  :10094   widowed  : 4551                 
# Mean   :1970                        Other: 3866   some coll   :15415   divorced :11585                 
# 3rd Qu.:1983                                      assoc deg   : 7614   separated: 1300                 
# Max.   :2006                                      bach deg    :19137   never    :13900                 
# adv deg     :17025                                   
# GENID_DESCRIBE       SEXUAL_ORIENTATION                      KIDS_LT5Y    
# NA         :36702   NA            :  969    NA                        :63953  
# male       :15089   gay or lesbian: 2386    Yes children under 5 in HH: 7199  
# female     :18830   straight      :62989                                      
# transgender:  147   bisexual      : 2756                                      
# other      :  384   something else: 1177                                      
# dont know     :  875                                      
# 
# KIDS_5_11Y                        KIDS_12_17Y   
# NA                       :60784   NA                        :60641  
# Yes children 5 - 11 in HH:10368   Yes children 12 - 17 in HH:10511  
# 
# 
# 
# 
# 
# ENROLLNONE                     WRKLOSSRV    
# NA                                :66789   NA                    : 1558  
# children not in any type of school: 4363   yes recent HH job loss: 6685  
# no recent HH job loss :62909  
# 
# 
# 
# 
# ANYWORK                     KINDWORK    
# NA                           : 1819   NA                 :30263  
# yes employment in last 7 days:41672   work for govt      : 6654  
# no employment in last 7 days :27661   work for private co:23400  
# work for nonprofit : 5025  
# self employed      : 5011  
# work in family biz :  799  
# 
# RSNNOWRKRV                                   TWDAYS     
# NA                                            :44596   NA                                : 2550  
# concerned about spreading                     :16005   had 1-2 telework days in past week: 6797  
# caring for elderly                            : 3830   had 3-4 telework days in past week: 4617  
# employer closed because covid                 : 2835   had 5+ telework days in past week :11121  
# sick or disabled                              : 1022   had no telework days in past week :46067  
# am/was sick w covid or caring for sick w covid: 1021                                             
# (Other)                                       : 1843                                             
# ANXIOUS     
# NA                                             : 5480  
# no anxiety over past 2 wks                     :37174  
# several days anxiety over past 2 wks           :19618  
# more than half the days anxiety over past 2 wks: 4329  
# nearly every day anxiety                       : 4551  
# 
# 
# WORRY      
# NA                                             : 5580  
# no worry over past 2 wks                       :43087  
# several days worried over past 2 wks           :15509  
# more than half the days worried over past 2 wks: 3386  
# nearly every day worry                         : 3590  
# 
# 
# INTEREST    
# NA                                                  : 5566  
# no days in past 2 wks with little interest in things:45234  
# several days over past 2 wks                        :14309  
# more than half the days over past 2 wks             : 3240  
# nearly every day                                    : 2803  
# 
# 
# DOWN                                           MHLTH_NEED   
# NA                                     : 5600   NA                                       :51723  
# no days in past 2 wks feeling depressed:44300   all children need mental health treatment: 1324  
# several days over past 2 wks           :15438   some but not all children                : 2266  
# more than half the days over past 2 wks: 2965   no, none of the children                 :15839  
# nearly every day                       : 2849                                                    
# 
# 
# MHLTH_GET    
# NA                                                    :67609  
# all children get the mental health treatment they need: 2743  
# some but not all children                             :  347  
# no, none of the children                              :  453  
# 
# 
# 
# MHLTH_SATISFD  
# NA                                                     :68076  
# satisfied with all the mental health treatment they get: 2061  
# some but not all                                       :  816  
# no, none                                               :  199  
# 
# 
# 
# MHLTH_DIFFCLT                                  SOCIAL1     
# NA                                               :67608   NA                                 : 6563  
# not difficult to get kids mental health treatment: 1244   always get social emotional support:19032  
# somewhat difficult                               : 1332   usually                            :23055  
# very difficult                                   :  719   sometimes                          :12604  
# unable to get treatment due to difficulty        :  186   rarely                             : 5844  
# did not try to get                               :   63   never                              : 4054  
# 
# SOCIAL2                                                              SUPPORT1    
# NA           : 6518   NA                                                          : 6579  
# always lonely: 2381   talk on phone w family friends neighbors less than once week:11831  
# usually      : 4847   1 or 2 per week                                             :18093  
# sometimes    :17999   3 or 4 per week                                             :14001  
# rarely       :23362   5 + per week                                                :20648  
# never        :16045                                                                       
# 
# SUPPORT2    
# NA                                                  : 6638  
# get together w friends or family less than once week:25546  
# 1 or 2 per week                                     :25665  
# 3 or 4 per week                                     : 8552  
# 5 + per week                                        : 4751  
# 
# 
# SUPPORT3    
# NA                                                               : 6703  
# attend church or religious ceremony never or less than 1 per year:35992  
# 1 to 3 per year                                                  : 8433  
# 4 to 11 per year                                                 : 3948  
# 12+ times per year                                               :16076  
# 
# 
# SUPPORT4    
# NA                                      : 7353  
# attend clubs or orgs less than once week:50214  
# 1 or 2 per week                         :10676  
# 3 or 4 per week                         : 2909  
# 
# 
# 
# SUPPORT1EXP   
# NA                                                                :11402  
# text on phone w friends or family or neighbors less than once week: 7990  
# 1 or 2 per week                                                   :10780  
# 3 or 4 per week                                                   :40980  
# 5 + per week                                                      :    0  
# 
# 
# CURFOODSUF                                                  CHILDFOOD    
# NA                            : 8111   NA                                                 :63887  
# had enough food               :42018   often kids not eating enough because couldnt afford:  370  
# had enough but not what wanted:16391   sometimes kids not eating enough                   : 1425  
# sometimes not enough food     : 3442   kids got enough food                               : 5470  
# often not enough food         : 1190                                                              
# 
# 
# PRICESTRESS                              TENURE     
# NA                               :21815   NA                           : 9149  
# very stressed about price changes:18189   housing owned free and clear :15998  
# Moderate stress price changes    :13888   housing owned with mortgage  :29700  
# a little stress price changes    :13278   housing rented               :15439  
# no stress                        : 3982   housing occupied without rent:  866  
# 
# 
# LIVQTRRV                RENTCUR                     MORTCUR     
# live in detached 1 family          :41925   NA             :55782   NA                 :41545  
# NA                                 : 9749   current on rent:13906   current on mortgage:28370  
# live in bldg w 5+ apts             : 8145   behind on rent : 1464   behind on mortgage : 1237  
# live in 1 family attached to others: 4758                                                      
# live in mobile home                : 2637                                                      
# live in building with 3-4 apts     : 2115                                                      
# (Other)                            : 1823                                                      
# EVICT                                             FORCLOSE    
# NA                                        :69734   NA                                        :69931  
# very likely evicted in next 2 months      :  190   very likely forclosed in next 2 months    :   52  
# somewhat likely evicted in next 2 months  :  354   somewhat likely forclosed in next 2 months:  182  
# not very likely evicted in next 2 months  :  394   not very likely forclosed in next 2 months:  452  
# not at all likely evicted in next 2 months:  480   not at all forclosed in next 2 months     :  535  
# 
# 
# RECVDVACC                                  HADCOVIDRV   
# NA                 :10284   NA                                :10717  
# yes got vaxx       :52519   yes tested + or doc told had Covid:36478  
# no did not get vaxx: 8349   no                                :23957  
# 
# 
# 
# 
# LONGCOVID                                        SYMPTOMS    
# NA                                 :36298   NA                                     :34850  
# had symptoms 3mo or more Long Covid: 9685   had no covid symptoms although tested +: 1350  
# no                                 :25169   had mild Covid symptoms                :13155  
# had moderate Covid symptoms            :16783  
# had severe Covid symptoms              : 5014  
# 
# 
# INCOME             EST_ST                        PRIVHLTH    
# NA                      :12256   California: 5215   has private health ins:45139  
# HH income $100k - 149   :10444   Texas     : 3456   no private health ins :12251  
# HH income $50k - 74.9   : 9461   Washington: 2892   NA                    :13762  
# HH income $75 - 99.9    : 7844   Florida   : 2613                                 
# HH income $200k +       : 7480   Michigan  : 2033                                 
# HH income less than $25k: 6782   Arizona   : 2018                                 
# (Other)                 :16885   (Other)   :52925                                 
# PUBHLTH            REGION      Num_kids_Pub_School Num_kids_Priv_School
# has public health ins:24916   Northeast:10731   Min.   :0.0         Min.   :0.00        
# no public health ins :30126   South    :23680   1st Qu.:1.0         1st Qu.:1.00        
# NA                   :16110   Midwest  :15071   Median :2.0         Median :1.00        
# West     :21670   Mean   :1.7         Mean   :1.26        
# 3rd Qu.:2.0         3rd Qu.:2.00        
# Max.   :4.0         Max.   :2.00        
# NA's   :58066       NA's   :69107       
# Num_kids_homeschool
# Min.   :0.00       
# 1st Qu.:1.00       
# Median :1.00       
# Mean   :1.22       
# 3rd Qu.:2.00       
# Max.   :2.00       
# NA's   :70079      
# > 
# > summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1936    1957    1970    1970    1983    2006 
# > summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1936    1956    1969    1970    1983    2006 
# > summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1936    1983    1992    1988    1998    2006 
# > summary(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1936    1962    1978    1976    1990    2006 
# > summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    1936    1956    1969    1970    1983    2006 
# > # here i want to find average ages of men and women
# > mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
# [1] 1970.346
# > sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
# [1] 15.56546
# > mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
# [1] 1969.691
# > sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
# [1] 16.09923

#1. The name of the members in my group are Alicia Persad, Flamur Kukaj, and Abdullah Al Iman.

#2. Somethings that surprised me about the data were the average education rates and the relation to the household income. Im not as surpirsed to see the median age and the relative household income to be so high. 

#3. Some questions I have are I would like to know more about the correlation between the average age and the response menatal health and the current working status.
