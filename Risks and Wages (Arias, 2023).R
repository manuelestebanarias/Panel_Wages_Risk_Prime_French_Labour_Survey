################################################################################
################################################################################
########  A panel analysis for French Employment Survey 2018 : #################
########     Wages and risks of accident in the industry       #################
################################################################################
################################################################################
#################################### By: Manuel Esteban Arias      #############
################################################################################


################################################################################
########  Preprocessing : ######################################################
################################################################################

############## Importing libraries #############################################
library(haven)
library(readr)
library(plm)
library(dplyr)
library(tidyr)
library(foreign)
library(xtable)
library(stargazer)

############# Importing database ###############################################
raw_data <- read_dta('C:/Users/manue/Downloads/EE_2018/EE_2018/Bases/EEC_2018INDIV.dta')

############# Preparing Data ###################################################
# Holding only the relevant variables
Variables=c("ident","noi","annee", "trim","cjrf", "acteu6","apobar","chpub","nafg004un","pub3fp",
            "nbsalb","depeta","nafentg021n","nafentg004n","nafg017un","am2nb",
            "csp","csep","contra","chgtpost","datant","dchantm","etpp","nbtot",
            "qprc","prim","prims","valpre","cite2011a","datdip","dip","catau2010r",
            "chron","dep","sexe","regio","metrodom","age","immi","nateu28","nfrred","salmee",
            "salred","empanh","santgen","hhc")

data0 <- raw_data[, Variables]
#rm(raw_data)



#Creating variables for id and for time
data0$id<-paste(data0$ident,data0$noi,sep = "")
data0$time<-paste(data0$annee,data0$trim,sep = "")
# Holding only relevant individuals
# Let's hold only the indivuduals that have 4 periods of records
data1 <- data0 %>%
  group_by(id) %>%
  filter(n_distinct(time) == 4) %>%
  ungroup()

#Full time workers
data1 <- data1 %>% 
  group_by(id) %>% 
  mutate(fulltime = ifelse(any(etpp == "1"), 1, 0))
data1<-data1[data1$fulltime == 1, ]

#Drop unnecessary  variables
data1<-subset(data1,select=-c(am2nb,acteu6))

#setting the format of numeric variables
data1$salmee = as.numeric(as.character(data1$salmee))
data1$salred = as.numeric(as.character(data1$salred))
data1$age = as.numeric(as.character(data1$age))
data1$valpre = as.numeric(as.character(data1$valpre))
data1$empanh = as.numeric(as.character(data1$empanh))
data1$hhc = as.numeric(as.character(data1$hhc))
data1$trim = as.numeric(as.character(data1$trim))
data1$datdip = as.numeric(as.character(data1$datdip))


#Filtering only acceptable data of income
data1$salmee <- ifelse(data1$salmee %in% c(9999998, 9999999), NA, data1$salmee)
data1$valpre <- ifelse(data1$valpre %in% c(9999998, 9999999), NA, data1$valpre)



# Sort the data by id and trim
data1 <- data1 %>% arrange(id, trim)

# Fill missing values of salmee with the previous non-missing value within each id
data1 <- data1 %>%  group_by(id) %>%   fill(salmee, .direction = "down")
data1 <- data1 %>%  group_by(id) %>%   fill(hhc, .direction = "down")
data1 <- data1 %>%  group_by(id) %>%   fill(salred, .direction = "down")
data1 <- data1 %>%  group_by(id) %>%   fill(nafg004un, .direction = "down")
# Fill remaining missing values of salmee with the next non-missing value within each id
data1 <- data1 %>%   group_by(id) %>%   fill(salmee, .direction = "up")
data1 <- data1 %>%   group_by(id) %>%   fill(hhc, .direction = "up")
data1 <- data1 %>%   group_by(id) %>%   fill(salred, .direction = "up")
data1 <- data1 %>%   group_by(id) %>%   fill(nafg004un, .direction = "up")

data1$empanh<- ifelse(is.na(data1$empanh), 0, data1$empanh)
data1$valpre<- ifelse(is.na(data1$valpre), 0, data1$valpre)



# Create a new column called "income" that sets the value of "salmee"
data1 <- data1 %>%   mutate(income = salmee)

data1$income_hour <- ifelse(is.na(data1$income) | is.na(data1$hhc), 0, data1$income/(4*data1$hhc))
data1$incomeTotal <- ifelse(is.na(data1$salmee), ifelse(is.na(data1$valpre), 0, data1$valpre/12), 
                            ifelse(is.na(data1$valpre), data1$salmee, 
                                   data1$salmee + (data1$valpre/12)))
data1$incomeTotal_hour <- ifelse(is.na(data1$incomeTotal) | is.na(data1$hhc), 0, data1$incomeTotal/(4*data1$hhc))

#keeping only the prople with income
data <- data1[!is.na(data1$income), ]

#Is the panel  balanced?
table(data$time)
#20181 20182 20183 20184 
#4165  4165  4165  4165  

###Creating an index ( based on the whole 2018 Survey) of risk
data0$empanh = as.numeric(as.character(data0$empanh))
library(dplyr)
library(tidyr)

table0 <- data0 %>%
  group_by(csp) %>%
  summarise(
    categories = unique(csp),
    rat_count = 100*(sum(!is.na(empanh) & empanh >= 0 & empanh <= 8) / n()),
    rat_sum = 100*(sum(empanh, na.rm = TRUE) / n())
  )
table0 <- table0[table0$csp != "10", ]
table0$rat_count_nom<- scale(table0$rat_count, center = FALSE, scale = diff(range(table0$rat_count)))
table0$rat_sum_nom<- scale(table0$rat_sum, center = FALSE, scale = diff(range(table0$rat_sum)))

data <- data %>% left_join(table0, by = c("csp" = "categories"))

# create a named vector with the replacement values
sub_vals <- c( "010" = 0, "100" = 1, "244" = 2, "253" = 1, "254" = 1, "344" = 0, "353" = 0, "354" = 1, "444" = 2, "540" = 2, "550" = 2, "640" = 3, "650" = 4, "740" = 5, "750" = 5, "760" = 5, "840" = 8)

# use replace() to substitute the values in the 'col' column
data$educ<- ifelse(data$cite2011a %in% names(sub_vals), sub_vals[as.character(data$cite2011a)], 0)
data$exp<- 2018-data$datdip
data$exp_sqr<- (2018-data$datdip)*(2018-data$datdip)
data$sexe<-ifelse(data$sexe!= "1",1,0)
data$sexe<-ifelse(is.na(data$sexe), 0, data$sexe)
data$sexe<-ifelse(data$sexe=="", 0, data$sexe)
data$industry<-ifelse(data$nafg004un== "ET",1,0)
data <- data %>% arrange(id, trim)
data <- data %>%  group_by(id) %>%   fill(industry, .direction = "down")
data <- data %>%  group_by(id) %>%   fill(industry, .direction = "up")
#The 3rd quartile of the rat_sum_nom id 0.40980119
data$risk<-ifelse(data$rat_sum_nom> 0.40980119,1,0)


################################################################################
########  Descriptive statistics : #############################################
################################################################################

#FIRST TABLE: Table1 (summary of numeric variables)
table1 <- as.data.frame(unclass(summary(data[,c("incomeTotal_hour","rat_sum_nom","exp","educ","salmee","valpre","empanh","hhc")])))
xtable(table1, type = "latex", file = "table1.tex")

#FIRST TABLE: Table2 (distribution of occupations)
occupations= c("Non renseigné","Agriculteurs sur petite exploitation",
               "Agriculteurs sur moyenne exploitation","Agriculteurs sur grande exploitation",
               "Artisans", "Commerçants et assimilés", "Chefs d'entreprise 10 salariés ou +",
               "Professions libérales", "Cadres de la fonction publique",
               "Professeurs, professions scientifiques",
               "Professions: info., arts et spectacles",
               "Cadres admin. et comme. d'entreprise",
               "Ingénieurs et cadres tech. d'entreprise",
               "Professeurs écoles, institut.et assimil.",
               "Professions interm. santé/travail social",
               "Clergé, religieux", "Professions interm. admin. fonction pu",
               "Professions interm. admin. comm. entrep.",
               "Techniciens", "Contremaîtres, agents de maîtrise",
               "Empl. civils et agents service fonction pub.",
               "Policiers et militaires","Employés administratifs d'entreprise",
               "Employés de commerce","Personnels services dir. aux particuliers",
               "Ouvriers qualifiés de type industriel",
               "Ouvriers qualifiés de type artisanal",
               "Chauffeurs","Ouvriers qualifiés: manu., maga., transp.",
               "Ouvriers non qualifiés de type industriel",
               "Ouvriers non qualifiés de type artisanal",
               "Ouvriers agricoles")


table2<-table(data$csp, data$time)
rownames(table2) = occupations
table2<-round((table2/4165) * 100, 2)
xtable(table2, type = "latex", file = "table2.tex")

#FIRST TABLE:Table3 (distribution of education)
Degrees= c("vide","Aucun diplôme reconnu,ou étude formelle",
           "Primaire, CEP, Certi. de formation générale",
           "Brevet des collèges (CFES, DNB, BEPC)",
           "Certificat d'éducation professionnelle, EFAA",
           "Bac général, bac techno, BT, BTA",
           "CAP-BEP, BP et équivalent",
           "Bac pro, BMA, dipl. santé social niveau bac",
           "Capacité en droit, DAEU",
           "Deug, propédeutique",
           "BTS, DUT et autres bac+2",
           "Lic., Maîtrise - général",
           "Lic. pro, Maîtrise - professionnel",
           "DEA, Magistère, masters (recherche)",
           "DESS, mast.(pro), dipl. grande école, doc. santé",
           "Masters non différenciés",
           "Doctorat de recherche, hors santé")

table3<-table(data$cite2011a, data$time)
rownames(table3) = Degrees
table3<-round((table3/4165) * 100, 2)
xtable(table3, type = "latex", file = "table3.tex")

hist(data1$empanh,xlab = "Weight",col = "cadetblue3",border = "cadetblue4")
################################################################################
########  Results : ############################################################
################################################################################

############# Unconditional wage gap ###########################################

reg_s11<-plm(log(data$incomeTotal) ~ data$rat_count_nom+ data$industry
             , data=data, index=c("id","time"),
             model="fd")
reg_s12<-plm(log(data$incomeTotal) ~ data$rat_count_nom+ data$industry
             , data=data, index=c("id","time"),
             model="random")
reg_s13<-plm(log(data$incomeTotal) ~ data$rat_count_nom+data$industry
             , data=data, index=c("id","time"),
             model="within")

reg_s14<-plm(log(data$incomeTotal) ~ data$rat_count_nom+data$educ+data$exp+data$exp_sqr+data$sexe
             , data=data, index=c("id","time"),
             model="random")
reg_s15<-plm(log(data$incomeTotal) ~ data$rat_count_nom+data$educ+data$exp+data$exp_sqr+data$sexe
             , data=data, index=c("id","time"),
             model="within")
reg_s16<-plm(log(data$incomeTotal) ~ data$rat_count_nom+data$educ+data$exp+data$exp_sqr+data$sexe
             , data=data, index=c("id","time"),
             model="fd")

############### Qualification vs unqualification ###############################
data_2 <- data[data$industry== 1, ]

data_3 <- data[data$industry!= 1, ]


reg_s21<-plm(log(data_2$incomeTotal_hour) ~ data_2$risk
             , data=data_2, index=c("id","time"),
             model="random")
reg_s24<-plm(log(data_2$incomeTotal_hour) ~ data_2$risk+data_2$educ+data_2$exp+data_2$exp_sqr+data_2$sexe
             , data=data_2, index=c("id","time"),
             model="random")

reg_s31 <- plm(log(data_3$incomeTotal_hour) ~ data_3$risk 
               , data=data_3, index=c("id","time")
               , model="random")

reg_s34<-plm(log(data_3$incomeTotal_hour) ~ data_3$risk+data_3$educ+data_3$exp+data_3$exp_sqr+data_3$sexe
             , data=data_3, index=c("id","time"),
             model="random")

############### geographic  and legal controls###############################

reg_s17<-plm(log(data$incomeTotal_hour) ~ data$risk+data$educ+data$exp
             +data$exp_sqr+data$industry+data$sexe
             , data=data, index=c("id","time"),
             model="random")
reg_s18<-plm(log(data$incomeTotal_hour) ~ data$risk+data$educ+data$exp
             +data$exp_sqr+data$industry+data$sexe+data$regio
             , data=data, index=c("id","time"),
             model="random")
reg_s19<-plm(log(data$incomeTotal_hour) ~ data$risk+data$educ+data$exp
             +data$exp_sqr+data$industry+data$sexe+data$contra
             , data=data, index=c("id","time"),
             model="random")
############### compensation value estimation ##################################

reg_s120<-plm(log(data$incomeTotal_hour) ~ data$educ+data$exp
             +data$exp_sqr+data$industry+data$sexe+ data$risk+ data$risk*data$industry
             , data=data, index=c("id","time"),
             model="random")





