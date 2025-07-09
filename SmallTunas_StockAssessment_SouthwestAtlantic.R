#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#xx#x#xx#x#xx#x#x#x#x#x#
# Small tunas stock assessment.............
# Southeast Atlantic----
# Stock Assessment Continuum Tool (prev. Stock Synthesis Data-Limited Tool)
# Catch + Length approaches 
# Scenarios of observed+reconstructed lengths 
# Scenarios of Selectivity changes over time and Recruitment
# Sensitivity of Life history data
# by Matheus Lourenco
#x#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#x#x#xx#x#x#x#x#x

#first clean the space 
rm(list = ls())

# R packages...............................
#please check if you have all packages below..

######@> Package list...
#install.packages("readxl")
library(readxl)
#install.packages("writexl")
library(writexl)
#install.packages("plyr")
library(plyr)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages('ggplot2')
library(ggplot2)
# the development version from github LBSPR
#install.packages("devtools") #you need the devtools to install the LBSPR
#devtools::install_github("AdrianHordyk/LBSPR",force = TRUE)
library(LBSPR)
#install.packages("TropFishR")
library(TropFishR)
#install.packages("devtools")
library(devtools)
# The package must be downloaded from: https://github.com/James-Thorson-NOAA/FishLife#
#devtools::install_local("C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas/FishLife-main")
library(FishLife)
#---------------------------------------- Machine Learning packages --------------------------------------------------#
#Python must be installed. version:3.10 is compatible with Keras: https://www.python.org/downloads/release/python-3100/
#Click in "Windows installer (64-bit)" and select "add Python 3.10 to PATH" to be used in any terminal
#Install Anaconda: https://www.anaconda.com/download/
#Install Rtools(34): https://cran.r-project.org/bin/windows/Rtools/
#installing Keras steps..
#install.packages("keras") 
#library(keras)
#install_keras(method = "virtualenv", python_version = "3.10") #now you can install KERAS API in R
#testing 
library(keras)
to_categorical(0:3)
#tensorflow (neural networks back-end)
library(tensorflow)
library(keras)
#---------------------------------------------------------------------------------------------------------------------#
#packages for parallel processing---
#install.packages("foreach")
library(foreach)
#install.packages("doParallel")
library(doParallel)
#graphic visualization
#install.packages('gridExtra')
library(gridExtra)
#--------------------------------------------------------#
#packages for Stock Assessment via Stock Synthesis (SS3)
#install.packages("remotes")
#remotes::install_github("r4ss/r4ss")
library(r4ss)



#---------------------------Control options-------------------------------#

#if you want to test different lstm models set=TRUE (it takes some time**)
run_lstm_loop=FALSE

#-------------------------------------------------------------------------#


#directory
setwd("C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas")
dir=getwd()
  
  #loading data....
  #------------------------Life History----------------------------#
  smtlh<- readxl::read_xlsx("smt_lifehistory_final.xlsx", sheet = 1)
  #------------------------Mean lengths----------------------------#
  smtml<- read.csv("smt_mean_pred.csv",  sep=",", dec=".")
  #----------------------- Length Frequency -----------------------#
  smtlenobs<- read.csv("smt_freq_obs.csv", sep=",", dec=".")
  smtlensim<- read.csv("smt_dist_sim.csv", sep=",", dec=".")
  #----------------------- Catch data -----------------------------#
  smtct<- read.csv("smt_catch.csv", sep=",", dec=".")
  #----------------------------------------------------------------#  
  

  #--------------List of evaluated species-----------------#
  # BLF blackfin tuna (Thunnus atlanticus);
  # BLT bullet tuna (Auxis rochei);
  # BON Atlantic bonito (Sarda sarda);
  # BRS serra Spanish mackerel (Scomberomorus brasiliensis);
  # CER cero mackerel (Scomberomorus regalis); *no information*
  # FRI frigate tuna (Auxis thazard);
  # KGM king mackerel (Scomberomorus cavalla);
  # LTA little tunny (Euthynnus alletteratus);
  # WAH wahoo (Acanthocybium solandri)
  # DOL (Coryphaena hippurus)
  #--------------------------------------------------------#
  
  #species with avaliable length and catch data for AT-SW
  sp<- c("BLF", "BRS", "DOL", "FRI", "KGM", "LTA","WAH")
  
# ------------------- Life History Exploratory Analysis --------------------------#

unique(smtlh$specie) #list of avaiable life history species data
table(smtlh$specie, smtlh$stock)  

#boxplot for the main life history parameters
#factored data set (pivot-longer)
smtlh_long <- tidyr::pivot_longer(
  smtlh,
  cols = c(linf, k, t0,t0_pauly,tmax,tmax_taylor, m, m_algaraja,m_pauly, mk,mk_algaraja,mk_pauly, lm50),
  names_to = "par",
  values_to = "value"
)
 
  #----------------------#
  #Plot for each parameter
  #----------------------#

#plot GAM boxplot life history data (Linf)
p1<-ggplot2:: ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock=="AT-SW",par=="linf"), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    #show.legend = TRUE)+
    position = "identity",
    lwd=0.75,
    outlier.shape=8,
    outlier.size = 0.8,
    outlier.alpha = 0.5)+
  geom_point(data=dplyr::filter(smtlh_long,stock=="AT-SW",par=="linf"),
    aes(x=factor(par),y=value),size=1.5,col="gray40")+
  facet_wrap(~codsp,scales = "free")+
  labs(x="Parameter",y="Value",col="",linetype='',fill='',caption = "AT-SW Stock")+
  #scale_y_continuous() +
  #scale_x_discrete(drop=FALSE,breaks=c(as.character(seq(1950,2022,10))))+
  theme_classic(base_size = 14) %+replace%
  theme(strip.background=element_blank(),
    plot.margin =unit(c(0.05,0.05,0.05,0.05),"mm"))
p1

ggplot2::ggsave("lifehistory_boxplot_Linf.png",plot=p1, device = "png", units = "cm",
  width = 15, height = 15)


#plot GAM boxplot life history data (k)
p2<-ggplot2:: ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock=="AT-SW",par=="k"), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    #show.legend = TRUE)+
    position = "identity",
    lwd=0.75,
    outlier.shape=8,
    outlier.size = 0.8,
    outlier.alpha = 0.5)+
  geom_point(data=dplyr::filter(smtlh_long,stock=="AT-SW",par=="k"),
    aes(x=factor(par),y=value),size=1.5,col="gray40")+
  facet_wrap(~codsp,scales = "free")+
  labs(x="Parameter",y="Value",col="",linetype='',fill='',caption = "AT-SW Stock")+
  #scale_y_continuous() +
  #scale_x_discrete(drop=FALSE,breaks=c(as.character(seq(1950,2022,10))))+
  theme_classic(base_size = 14) %+replace%
  theme(strip.background=element_blank(),
    plot.margin =unit(c(0.05,0.05,0.05,0.05),"mm"))
p2

ggplot2::ggsave("lifehistory_boxplot_k.png",plot=p2, device = "png", units = "cm",
  width = 15, height = 15)



#plot GAM boxplot life history data (to, to_pauly)
p3 <- ggplot2::ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("t0", "t0_pauly")), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    lwd=0.75,
    outlier.shape=8,
    outlier.size=0.8,
    outlier.alpha=0.5) +
  geom_point(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("t0", "t0_pauly")),
    aes(x=factor(par), y=value), 
    size=1.5, 
    col="gray40") +
  facet_wrap(~codsp, scales="free") +
  labs(x="Parameter", y="Value", col="", linetype="", fill="", caption="AT-SW Stock") +
  theme_classic(base_size=14) %+replace% 
  theme(strip.background=element_blank(),
    plot.margin=unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

p3

ggplot2::ggsave("lifehistory_boxplot_t0.png",plot=p3, device = "png", units = "cm",
  width = 15, height = 15)


#plot GAM boxplot life history data (tmax, tmax_taylor)
p4 <- ggplot2::ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("tmax", "tmax_taylor")), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    lwd=0.75,
    outlier.shape=8,
    outlier.size=0.8,
    outlier.alpha=0.5) +
  geom_point(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("tmax", "tmax_taylor")),
    aes(x=factor(par), y=value), 
    size=1.5, 
    col="gray40") +
  facet_wrap(~codsp, scales="free") +
  labs(x="Parameter", y="Value", col="", linetype="", fill="", caption="AT-SW Stock") +
  theme_classic(base_size=14) %+replace% 
  theme(strip.background=element_blank(),
    plot.margin=unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

p4

ggplot2::ggsave("lifehistory_boxplot_tmax.png",plot=p4, device = "png", units = "cm",
  width = 15, height = 15)

#plot GAM boxplot life history data (m, m_algaraja,m_pauly)
p5 <- ggplot2::ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("m", "m_algaraja","m_pauly")), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    lwd=0.75,
    outlier.shape=8,
    outlier.size=0.8,
    outlier.alpha=0.5) +
  geom_point(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("m", "m_algaraja","m_pauly")),
    aes(x=factor(par), y=value), 
    size=1.5, 
    col="gray40") +
  facet_wrap(~codsp, scales="free") +
  labs(x="Parameter", y="Value", col="", linetype="", fill="", caption="AT-SW Stock") +
  theme_classic(base_size=14) %+replace% 
  theme(strip.background=element_blank(),
    plot.margin=unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

p5

ggplot2::ggsave("lifehistory_boxplot_m.png",plot=p5, device = "png", units = "cm",
  width = 15, height = 15)


#plot GAM boxplot life history data (mk,mk_algaraja,mk_pauly)
p6 <- ggplot2::ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c('mk',"mk_algaraja","mk_pauly")), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    lwd=0.75,
    outlier.shape=8,
    outlier.size=0.8,
    outlier.alpha=0.5) +
  geom_point(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("mk","mk_algaraja","mk_pauly")),
    aes(x=factor(par), y=value), 
    size=1.5, 
    col="gray40") +
  facet_wrap(~codsp, scales="free") +
  labs(x="Parameter", y="Value", col="", linetype="", fill="", caption="AT-SW Stock") +
  theme_classic(base_size=14) %+replace% 
  theme(strip.background=element_blank(),
    plot.margin=unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

p6

ggplot2::ggsave("lifehistory_boxplot_mk.png",plot=p6, device = "png", units = "cm",
  width = 15, height = 15)


#plot GAM boxplot life history data (lm50)
p7 <- ggplot2::ggplot() +
  geom_boxplot(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c('lm50')), 
    aes(x=factor(par), y=value),
    col="gray40",
    fill="gray60",
    alpha=0.6,
    lwd=0.75,
    outlier.shape=8,
    outlier.size=0.8,
    outlier.alpha=0.5) +
  geom_point(data=dplyr::filter(smtlh_long, stock == "AT-SW", par %in% c("lm50")),
    aes(x=factor(par), y=value), 
    size=1.5, 
    col="gray40") +
  facet_wrap(~codsp, scales="free") +
  labs(x="Parameter", y="Value", col="", linetype="", fill="", caption="AT-SW Stock") +
  theme_classic(base_size=14) %+replace% 
  theme(strip.background=element_blank(),
    plot.margin=unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

p7

ggplot2::ggsave("lifehistory_boxplot_lm50.png",plot=p7, device = "png", units = "cm",
  width = 15, height = 15)
#----------------------------------------------------------------------


#------------------------------------------------
# Selecting the best life history parameters
#------------------------------------------------
# Z-score function 
zscore <- function(x) {
  if (all(is.na(x)) || length(na.omit(x)) <= 1 || sd(x, na.rm = TRUE) == 0) {
    return(rep(0, length(x)))  # Return 0 if sd=0 or length(x)=1
  } else {
    z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    return(z)
  }
}

                        #------------------#
                        # BLF life history #
                        #------------------#

#filtering for BLF ALL stocks
BLF_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50,tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(stock == "AT-SW", codsp == "BLF") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax= zscore(tmax),
    z_wl_a = zscore(wl_a),
    z_wl_b = zscore(wl_b)
  ) %>%
  # Selecting based on 2 standard deviations distance
  mutate(
    linf = ifelse(abs(z_linf) <= 2, linf, NA),
    k = ifelse(abs(z_k) <= 2, k, NA),
    t0 = ifelse(abs(z_t0) <= 2, t0, NA),
    t0_pauly = ifelse(abs(z_t0_pauly) <= 2, t0_pauly, NA),
    m = ifelse(abs(z_m) <= 2, m, NA),
    m_algaraja = ifelse(abs(z_m_algaraja) <= 2, m_algaraja, NA),
    m_pauly = ifelse(abs(z_m_pauly) <= 2, m_pauly, NA),
    mk = ifelse(abs(z_mk) <= 2, mk, NA),
    mk_algaraja = ifelse(abs(z_mk_algaraja) <= 2, mk_algaraja, NA),
    mk_pauly = ifelse(abs(z_mk_pauly) <= 2, mk_pauly, NA),
    lm50 = ifelse(abs(z_lm50) <= 2, lm50, NA),
    lm95 = lm50 * 1.1, 
    tmax = ifelse(abs(z_tmax) <= 2, tmax, NA),
    wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
    wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95,tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b), is.na)) %>%
  dplyr::mutate(tmax=6)

#print all filtered parameters  
cat("Filtered Life history data for BLF: ", "\n",
    "Linf= ", na.omit(BLF_filtered$linf), "\n",
    "k= ", na.omit(BLF_filtered$k), "\n",
    "t0= ", na.omit(BLF_filtered$t0), "\n",
    "t0_pauly= ",na.omit(BLF_filtered$t0_pauly), "\n",
    "m= ", na.omit(BLF_filtered$m), "\n",
    "m_algaraja= ",na.omit(BLF_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(BLF_filtered$m_pauly), "\n",
    "mk= ",na.omit(BLF_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(BLF_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(BLF_filtered$mk_pauly), "\n",
    "l50= ", na.omit(BLF_filtered$lm50), "\n",
    "l95= ", na.omit(BLF_filtered$lm95), "\n",
    "tmax= ",na.omit(BLF_filtered$tmax), "\n",
    "wl_a= ", na.omit(BLF_filtered$wl_a), "\n",
    "wl_b= ", na.omit(BLF_filtered$wl_b), "\n",
    "-------------------------------")


                      #------------------#
                      # BRS life history #  
                      #------------------#
#filtering for AT-SW and BRS
BRS_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(stock == "AT-SW", codsp == "BRS") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax= zscore(tmax),
    z_wl_a= zscore(wl_a),
    z_wl_b=zscore(wl_b)
  ) %>%
  #Selecting based on 2 standard deviations distance
  mutate(linf= ifelse(abs(z_linf)<=2, linf, NA),
         k= ifelse(abs(z_k)<=2, k, NA),
         t0= ifelse(abs(z_t0)<=2, t0, NA),
         t0_pauly= ifelse(abs(z_t0_pauly)<=2, t0_pauly, NA),
         m= ifelse(abs(z_m)<=2, m, NA),
         m_algaraja=ifelse(abs(z_m_algaraja)<=2, m_algaraja, NA),
         m_pauly= ifelse(abs(z_m_pauly)<=2, m_pauly, NA),
         mk= ifelse(abs(z_mk)<=2, mk, NA),
         mk_algaraja= ifelse(abs(z_mk_algaraja)<=2, mk_algaraja, NA),
         mk_pauly=ifelse(abs(z_mk_pauly)<=2, mk_pauly, NA),
         lm50= ifelse(abs(z_lm50)<=2, lm50, NA),
         lm95 = lm50 * 1.1,  
         tmax= ifelse(abs(z_tmax)<=2, tmax, NA),
         wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
         wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95,tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95,tmax, wl_a, wl_b), is.na))

#print all filtered parameters  
cat("Filtered Life history data for BRS: ", "\n",
    "Linf= ", na.omit(BRS_filtered$linf), "\n",
    "k= ", na.omit(BRS_filtered$k), "\n",
    "t0= ", na.omit(BRS_filtered$t0), "\n",
    "t0_pauly= ",na.omit(BRS_filtered$t0_pauly), "\n",
    "m= ", na.omit(BRS_filtered$m), "\n",
    "m_algaraja= ",na.omit(BRS_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(BRS_filtered$m_pauly), "\n",
    "mk= ",na.omit(BRS_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(BRS_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(BRS_filtered$mk_pauly), "\n",
    "l50= ", na.omit(BRS_filtered$lm50), "\n",
    "l95= ", na.omit(BRS_filtered$lm95), "\n",
    "tmax= ", na.omit(BRS_filtered$tmax), "\n",
    "wl_a= ", na.omit(BRS_filtered$wl_a), "\n",
    "wl_b= ", na.omit(BRS_filtered$wl_b), "\n",
    "-------------------------------")



                #------------------#
                # DOL life history # *information from other regions
                #------------------#
#filtering for  DOL
DOL_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(codsp == "DOL") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax= zscore(tmax),
    z_wl_a= zscore(wl_a),
    z_wl_b=zscore(wl_b)
  ) %>%
  #Selecting based on 2 standard deviations distance
  mutate(linf= ifelse(abs(z_linf)<=2, linf, NA),
         k= ifelse(abs(z_k)<=2, k, NA),
         t0= ifelse(abs(z_t0)<=2, t0, NA),
         t0_pauly= ifelse(abs(z_t0_pauly)<=2, t0_pauly, NA),
         m= ifelse(abs(z_m)<=2, m, NA),
         m_algaraja=ifelse(abs(z_m_algaraja)<=2, m_algaraja, NA),
         m_pauly= ifelse(abs(z_m_pauly)<=2, m_pauly, NA),
         mk= ifelse(abs(z_mk)<=2, mk, NA),
         mk_algaraja= ifelse(abs(z_mk_algaraja)<=2, mk_algaraja, NA),
         mk_pauly=ifelse(abs(z_mk_pauly)<=2, mk_pauly, NA),
         lm50= ifelse(abs(z_lm50)<=2, lm50, NA),
         lm95 = lm50 * 1.1,  
         tmax= ifelse(abs(z_tmax)<=2, tmax, NA),
         wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
         wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b), is.na))

#print all filtered parameters  
cat("Filtered Life history data for DOL: ", "\n",
    "Linf= ", na.omit(DOL_filtered$linf), "\n",
    "k= ", na.omit(DOL_filtered$k), "\n",
    "t0= ", na.omit(DOL_filtered$t0), "\n",
    "t0_pauly= ",na.omit(DOL_filtered$t0_pauly), "\n",
    "m= ", na.omit(DOL_filtered$m), "\n",
    "m_algaraja= ",na.omit(DOL_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(DOL_filtered$m_pauly), "\n",
    "mk= ",na.omit(DOL_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(DOL_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(DOL_filtered$mk_pauly), "\n",
    "l50= ", na.omit(DOL_filtered$lm50), "\n",
    "l95= ", na.omit(DOL_filtered$lm95), "\n",
    "tmax= ", na.omit(DOL_filtered$tmax), "\n",
    "wl_a= ", na.omit(DOL_filtered$wl_a), "\n",
    "wl_b= ", na.omit(DOL_filtered$wl_b), "\n",
    "-------------------------------")


            #------------------#
            # FRI life history # *information from other regions
            #------------------#
#filtering for FRI
FRI_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(codsp == "FRI") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax=  zscore(tmax),
    z_wl_a= zscore(wl_a),
    z_wl_b=zscore(wl_b)
  ) %>%
  #Selecting based on 2 standard deviations distance
  mutate(linf= ifelse(abs(z_linf)<=2, linf, NA),
         k= ifelse(abs(z_k)<=2, k, NA),
         t0= ifelse(abs(z_t0)<=2, t0, NA),
         t0_pauly= ifelse(abs(z_t0_pauly)<=2, t0_pauly, NA),
         m= ifelse(abs(z_m)<=2, m, NA),
         m_algaraja=ifelse(abs(z_m_algaraja)<=2, m_algaraja, NA),
         m_pauly= ifelse(abs(z_m_pauly)<=2, m_pauly, NA),
         mk= ifelse(abs(z_mk)<=2, mk, NA),
         mk_algaraja= ifelse(abs(z_mk_algaraja)<=2, mk_algaraja, NA),
         mk_pauly=ifelse(abs(z_mk_pauly)<=2, mk_pauly, NA),
         lm50= ifelse(abs(z_lm50)<=2, lm50, NA),
         lm95 = lm50 * 1.1,
         tmax= ifelse(abs(z_tmax)<=2, tmax, NA),
         wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
         wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95,tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b), is.na))

#print all filtered parameters  
cat("Filtered Life history data for FRI: ", "\n",
    "Linf= ", na.omit(FRI_filtered$linf), "\n",
    "k= ", na.omit(FRI_filtered$k), "\n",
    "t0= ", na.omit(FRI_filtered$t0), "\n",
    "t0_pauly= ",na.omit(FRI_filtered$t0_pauly), "\n",
    "m= ", na.omit(FRI_filtered$m), "\n",
    "m_algaraja= ",na.omit(FRI_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(FRI_filtered$m_pauly), "\n",
    "mk= ",na.omit(FRI_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(FRI_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(FRI_filtered$mk_pauly), "\n",
    "l50= ", na.omit(FRI_filtered$lm50), "\n",
    "l95= ", na.omit(FRI_filtered$lm95), "\n",
    "tmax= ", na.omit(FRI_filtered$tmax), "\n",
    "wl_a= ", na.omit(FRI_filtered$wl_a), "\n",
    "wl_b= ", na.omit(FRI_filtered$wl_b), "\n",
    "-------------------------------")



                      #------------------#
                      # KGM life history # 
                      #------------------#
#filtering for KGM
KGM_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(stock == "AT-SW", codsp == "KGM") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax= zscore(tmax),
    z_wl_a= zscore(wl_a),
    z_wl_b=zscore(wl_b)
  ) %>%
  #Selecting based on 2 standard deviations distance
  mutate(linf= ifelse(abs(z_linf)<=2, linf, NA),
         k= ifelse(abs(z_k)<=2, k, NA),
         t0= ifelse(abs(z_t0)<=2, t0, NA),
         t0_pauly= ifelse(abs(z_t0_pauly)<=2, t0_pauly, NA),
         m= ifelse(abs(z_m)<=2, m, NA),
         m_algaraja=ifelse(abs(z_m_algaraja)<=2, m_algaraja, NA),
         m_pauly= ifelse(abs(z_m_pauly)<=2, m_pauly, NA),
         mk= ifelse(abs(z_mk)<=2, mk, NA),
         mk_algaraja= ifelse(abs(z_mk_algaraja)<=2, mk_algaraja, NA),
         mk_pauly=ifelse(abs(z_mk_pauly)<=2, mk_pauly, NA),
         lm50= ifelse(abs(z_lm50)<=2, lm50, NA),
         lm95 = lm50 * 1.1,  
         tmax= ifelse(abs(z_tmax)<=2, tmax, NA),
         wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
         wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b), is.na))

#print all filtered parameters  
cat("Filtered Life history data for KGM: ", "\n",
    "Linf= ", na.omit(KGM_filtered$linf), "\n",
    "k= ", na.omit(KGM_filtered$k), "\n",
    "t0= ", na.omit(KGM_filtered$t0), "\n",
    "t0_pauly= ",na.omit(KGM_filtered$t0_pauly), "\n",
    "m= ", na.omit(KGM_filtered$m), "\n",
    "m_algaraja= ",na.omit(KGM_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(KGM_filtered$m_pauly), "\n",
    "mk= ",na.omit(KGM_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(KGM_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(KGM_filtered$mk_pauly), "\n",
    "l50= ", na.omit(KGM_filtered$lm50), "\n",
    "l95= ", na.omit(KGM_filtered$lm95), "\n",
    "tmax= ", na.omit(KGM_filtered$tmax), "\n",
    "wl_a= ", na.omit(KGM_filtered$wl_a), "\n",
    "wl_b= ", na.omit(KGM_filtered$wl_b), "\n",
    "-------------------------------")



              #------------------#
              # LTA life history # *Information from other regions
              #------------------#
#filtering for LTA
LTA_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(codsp == "LTA") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax= zscore(tmax),
    z_wl_a= zscore(wl_a),
    z_wl_b=zscore(wl_b)
  ) %>%
  #Selecting based on 2 standard deviations distance
  mutate(linf= ifelse(abs(z_linf)<=2, linf, NA),
         k= ifelse(abs(z_k)<=2, k, NA),
         t0= ifelse(abs(z_t0)<=2, t0, NA),
         t0_pauly= ifelse(abs(z_t0_pauly)<=2, t0_pauly, NA),
         m= ifelse(abs(z_m)<=2, m, NA),
         m_algaraja=ifelse(abs(z_m_algaraja)<=2, m_algaraja, NA),
         m_pauly= ifelse(abs(z_m_pauly)<=2, m_pauly, NA),
         mk= ifelse(abs(z_mk)<=2, mk, NA),
         mk_algaraja= ifelse(abs(z_mk_algaraja)<=2, mk_algaraja, NA),
         mk_pauly=ifelse(abs(z_mk_pauly)<=2, mk_pauly, NA),
         lm50= ifelse(abs(z_lm50)<=2, lm50, NA),
         lm95 = lm50 * 1.1,  
         tmax= ifelse(abs(z_tmax)<=2, tmax, NA),
         wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
         wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b), is.na))

#print all filtered parameters  
cat("Filtered Life history data for LTA: ", "\n",
    "Linf= ", na.omit(LTA_filtered$linf), "\n",
    "k= ", na.omit(LTA_filtered$k), "\n",
    "t0= ", na.omit(LTA_filtered$t0), "\n",
    "t0_pauly= ",na.omit(LTA_filtered$t0_pauly), "\n",
    "m= ", na.omit(LTA_filtered$m), "\n",
    "m_algaraja= ",na.omit(LTA_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(LTA_filtered$m_pauly), "\n",
    "mk= ",na.omit(LTA_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(LTA_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(LTA_filtered$mk_pauly), "\n",
    "l50= ", na.omit(LTA_filtered$lm50), "\n",
    "l95= ", na.omit(LTA_filtered$lm95), "\n",
    "tmax= ", na.omit(LTA_filtered$tmax), "\n",
    "wl_a= ", na.omit(LTA_filtered$wl_a), "\n",
    "wl_b= ", na.omit(LTA_filtered$wl_b), "\n",
    "-------------------------------")


                  #------------------#
                  # WAH life history # *Information from other regions
                  #------------------#
#filtering for WAH
WAH_filtered <- smtlh %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, tmax, wl_a, wl_b, source, stock) %>%
  dplyr::filter(codsp == "WAH") %>%
  dplyr::mutate(
    z_linf = zscore(linf),
    z_k = zscore(k),
    z_t0 = zscore(t0),
    z_t0_pauly = zscore(t0_pauly),
    z_m = zscore(m),
    z_m_algaraja = zscore(m_algaraja),
    z_m_pauly = zscore(m_pauly),
    z_mk = zscore(mk),
    z_mk_algaraja = zscore(mk_algaraja),
    z_mk_pauly = zscore(mk_pauly),
    z_lm50 = zscore(lm50),
    z_tmax = zscore(tmax),
    z_wl_a= zscore(wl_a),
    z_wl_b=zscore(wl_b)
  ) %>%
  #Selecting based on 2 standard deviations distance
  mutate(linf= ifelse(abs(z_linf)<=2, linf, NA),
         k= ifelse(abs(z_k)<=2, k, NA),
         t0= ifelse(abs(z_t0)<=2, t0, NA),
         t0_pauly= ifelse(abs(z_t0_pauly)<=2, t0_pauly, NA),
         m= ifelse(abs(z_m)<=2, m, NA),
         m_algaraja=ifelse(abs(z_m_algaraja)<=2, m_algaraja, NA),
         m_pauly= ifelse(abs(z_m_pauly)<=2, m_pauly, NA),
         mk= ifelse(abs(z_mk)<=2, mk, NA),
         mk_algaraja= ifelse(abs(z_mk_algaraja)<=2, mk_algaraja, NA),
         mk_pauly=ifelse(abs(z_mk_pauly)<=2, mk_pauly, NA),
         lm50= ifelse(abs(z_lm50)<=2, lm50, NA),
         lm95 = lm50 * 1.1,  
         tmax= ifelse(abs(z_tmax)<=2, tmax, NA),
         wl_a = ifelse(abs(z_wl_a) <= 2, wl_a, NA),
         wl_b = ifelse(abs(z_wl_b) <= 2, wl_b, NA)
  ) %>%
  dplyr::select(codsp, country, period, growth_method, sex, linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b, source, stock) %>%
  # Removing NA values
  dplyr::filter(!if_all(c(linf, k, t0, t0_pauly, m, m_algaraja, m_pauly, mk, mk_algaraja, mk_pauly, lm50, lm95, tmax, wl_a, wl_b), is.na))

#print all filtered parameters  
cat("Filtered Life history data for WAH: ", "\n",
    "Linf= ", na.omit(WAH_filtered$linf), "\n",
    "k= ", na.omit(WAH_filtered$k), "\n",
    "t0= ", na.omit(WAH_filtered$t0), "\n",
    "t0_pauly= ",na.omit(WAH_filtered$t0_pauly), "\n",
    "m= ", na.omit(WAH_filtered$m), "\n",
    "m_algaraja= ",na.omit(WAH_filtered$m_algaraja), "\n",
    "m_pauly= ",na.omit(WAH_filtered$m_pauly), "\n",
    "mk= ",na.omit(WAH_filtered$mk), "\n",
    "mk_algaraja= ",na.omit(WAH_filtered$mk_algaraja), "\n",
    "mk_pauly= ",na.omit(WAH_filtered$mk_pauly), "\n",
    "l50= ", na.omit(WAH_filtered$lm50), "\n",
    "l95= ", na.omit(LTA_filtered$lm95), "\n",
    "tmax= ", na.omit(LTA_filtered$tmax), "\n",
    "wl_a= ", na.omit(WAH_filtered$wl_a), "\n",
    "wl_b= ", na.omit(WAH_filtered$wl_b), "\n",
    "-------------------------------")
#------------------------------------------------------


                  #------------- Estimating steepness (h) parameter from Fishlife ---------------------#
                  #                       Fish Life (Thorson et al., 2023 )                            #
                  # The package must be downloaded from: https://github.com/James-Thorson-NOAA/FishLife#
                  #------------------------------------------------------------------------------------#

#installing from the downloaded repository
#devtools::install_local("C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas/FishLife-main")
library(FishLife)

                                      #-------------------------#
                                      #  #Estimating Steepness  #
                                      #-------------------------#
#list of species
sp

#BLF- Blackfin tuna- Thunnus atlanticus
blf_h<-Plot_taxa(Search_species(Genus="Thunnus",Species="atlanticus")$match_taxonomy)[[3]]
blf_h<-blf_h$Mean_pred[names(blf_h$Mean_pred)=="h"]
#creating a new line for the blf life history data
BLF_fishlife<- data.frame(codsp=  "BLF",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(blf_h,2),
                          source= "FishLife",
                          stock= "AT-SW")
                   
#adding to the original data frame       
BLF_filtered$h<- NA
BLF_filtered<- rbind(BLF_filtered, BLF_fishlife)

# Ordering
BLF_filtered <- BLF_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax" ,"wl_a", "wl_b", 
                                 "h", "source", "stock")]
#------------------------------------------------------------------------------------


#BRS serra Spanish mackerel-  Scomberomorus brasiliensis
brs_h<-Plot_taxa(Search_species(Genus="Scomberomorus",Species="brasiliensis")$match_taxonomy)[[3]]
brs_h<-brs_h$Mean_pred[names(brs_h$Mean_pred)=="h"]
#creating a new line for the blf life history data
BRS_fishlife<- data.frame(codsp=  "BRS",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(brs_h,2),
                          source= "FishLife",
                          stock= "AT-SW")

#adding to the original data frame       
BRS_filtered$h<- NA
BRS_filtered<- rbind(BRS_filtered, BRS_fishlife)

# Ordering
BRS_filtered <- BRS_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95", "tmax", "wl_a", "wl_b", 
                                 "h", "source", "stock")]
#------------------------------------------------------------------------------------


#DOL Dolphin Fish-  Coryphaena hippurus
dol_h<-Plot_taxa(Search_species(Genus="Coryphaena",Species="hippurus")$match_taxonomy)[[3]]
dol_h<-dol_h$Mean_pred[names(dol_h$Mean_pred)=="h"]
#creating a new line for the dol life history data
DOL_fishlife<- data.frame(codsp=  "DOL",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(dol_h,2), #steepness estimate
                          source= "FishLife",
                          stock= "AT-SW")

#adding to the original data frame       
DOL_filtered$h<- NA
DOL_filtered<- rbind(DOL_filtered, DOL_fishlife)

# Ordering
DOL_filtered <- DOL_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50", "lm95", "tmax","wl_a", "wl_b", 
                                 "h", "source", "stock")]
#------------------------------------------------------------------------------------


#FRI frigate tuna- Auxis thazard
fri_h<-Plot_taxa(Search_species(Genus="Auxis",Species="thazard")$match_taxonomy)[[3]]
fri_h<-fri_h$Mean_pred[names(fri_h$Mean_pred)=="h"]
#creating a new line for the fri life history data
FRI_fishlife<- data.frame(codsp=  "FRI",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(fri_h,2), #steepness estimate
                          source= "FishLife",
                          stock= "AT-SW")

#adding to the original data frame       
FRI_filtered$h<- NA
FRI_filtered<- rbind(FRI_filtered, FRI_fishlife)

# Ordering
FRI_filtered <- FRI_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95", "tmax" ,"wl_a", "wl_b", 
                                 "h", "source", "stock")]
#------------------------------------------------------------------------------------


# KGM king mackerel- Scomberomorus cavalla
kgm_h<-Plot_taxa(Search_species(Genus="Scomberomorus",Species="cavalla")$match_taxonomy)[[3]]
kgm_h<-kgm_h$Mean_pred[names(kgm_h$Mean_pred)=="h"]
#creating a new line for the kgm life history data
KGM_fishlife<- data.frame(codsp=  "KGM",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(kgm_h,2), #steepness estimate
                          source= "FishLife",
                          stock= "AT-SW")

#adding to the original data frame       
KGM_filtered$h<- NA
KGM_filtered<- rbind(KGM_filtered, KGM_fishlife)

# Ordering
KGM_filtered <- KGM_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h", "source", "stock")]
#------------------------------------------------------------------------------------


# LTA little tunny- Euthynnus alletteratus
lta_h<-Plot_taxa(Search_species(Genus="Euthynnus",Species="alletteratus")$match_taxonomy)[[3]]
lta_h<-lta_h$Mean_pred[names(lta_h$Mean_pred)=="h"]
#creating a new line for the lta life history data
LTA_fishlife<- data.frame(codsp=  "LTA",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(lta_h,2), #steepness estimate
                          source= "FishLife",
                          stock= "AT-SW")

#adding to the original data frame       
LTA_filtered$h<- NA
LTA_filtered<- rbind(LTA_filtered, LTA_fishlife)

# Ordering
LTA_filtered <- LTA_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h", "source", "stock")]
#------------------------------------------------------------------------------------


# WAH wahoo- Acanthocybium solandri
wah_h<-Plot_taxa(Search_species(Genus="Acanthocybium",Species="solandri")$match_taxonomy)[[3]]
wah_h<-wah_h$Mean_pred[names(wah_h$Mean_pred)=="h"]
#creating a new line for the wah life history data
WAH_fishlife<- data.frame(codsp=  "WAH",
                          country= 'Brazil',
                          period= NA,
                          growth_method= NA,
                          sex= "BB",
                          linf= NA,
                          k= NA,
                          t0=  NA,
                          t0_pauly= NA,
                          m=  NA,
                          m_algaraja= NA,
                          m_pauly= NA,  
                          mk=  NA,
                          mk_algaraja= NA,
                          mk_pauly= NA,
                          lm50= NA,
                          lm95=NA,
                          tmax=NA,
                          wl_a= NA,
                          wl_b= NA,
                          h= round(wah_h,2), #steepness estimate
                          source= "FishLife",
                          stock= "AT-SW")

#adding to the original data frame       
WAH_filtered$h<- NA
WAH_filtered<- rbind(WAH_filtered, WAH_fishlife)

# Ordering
WAH_filtered <- WAH_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h", "source", "stock")]
#----------------------------------------------------------------------------------------------



#----------------------------------Binding all life history together--------------------------------------------#
lh_filtered<- rbind(BLF_filtered,BRS_filtered,DOL_filtered,FRI_filtered,KGM_filtered,LTA_filtered,WAH_filtered)
#---------------------------------------------------------------------------------------------------------------



                            #X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#
                            #Estimating Selectivity (SL50, SL95) via Catch-Curve# 
                            #                Selection Ogive                    # 
                            #X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#


# Selectivity from the selection ogive - catch curve (TropfishR)
# Selectivity from the LBSPR fit function

lf<- function(x, by=5){ 
  
  h = hist(x, breaks = seq(from = min(x) - by, to = max(x) + by, by = by), plot = FALSE)
  lf_dat<- data.frame(mids= h$mids,counts=h$counts)
  
  return(lf_dat)
}

sel <- function(x, linf, k, l50, l95, mk, t0, by_values, year, sp, units, method) {
  
  if (method == "Catch curve") {
    
    require(TropFishR)
    
    for (by in by_values) {
      
      h <- tryCatch({
        lf(x, by)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(h)) {
        next # if is not correct, test the next by
      }
      
      list <- list(
        midLengths = round(h$mids, 1),
        Linf = linf,
        K = k,
        t0 = t0,
        catch = h$counts
      )
      
      sl <- tryCatch({
        TropFishR::catchCurve(list, auto = TRUE, calc_ogive = TRUE)
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(sl)) {
        sl50 <- sl$L50
        
        # Test if selectivity is outside the limits
        if (sl50 < h$mids[1] || sl50 > h$mids[length(h$mids)]) {
          message("Impossible to estimate with catch-curve, trying LBSPR.")
          return(sel(x, linf, k, l50, l95, mk, t0, by_values, year, sp, units, method = "LBSPR"))
        }
        
        return(sl50) # Return the best sl50
      }
    }
    
    # If the model fails for all by_values, add extra lengths
    max_length <- max(x) 
    extra_lengths <- seq(max_length + 2, max_length + 10, by = 2) 
    x_extended <- c(x, extra_lengths)
    
    h <- lf(x_extended, by_values[1])
    
    list <- list(
      midLengths = round(h$mids, 1),
      Linf = linf,
      K = k,
      t0 = t0,
      catch = h$counts
    )
    
    sl <- tryCatch({
      TropFishR::catchCurve(list, auto = TRUE, calc_ogive = TRUE)
    }, error = function(e) {
      message("Unreasonable values of by, even after extending the length range. Estimating selectivity via LBSPR.")
      return(sel(x, linf, k, l50, l95, mk, t0, by_values, year, sp, units, method = "LBSPR"))
    })
    
    return(sl)
  }
  
  if (method == "LBSPR") {
    
    require(LBSPR)
    
    # Biological parameters
    mypars <- new("LB_pars", verbose = F)
    mypars@Species <- sp
    mypars@Linf <- linf
    mypars@L50 <- l50
    mypars@L95 <- l95
    mypars@MK <- mk
    mypars@L_units <- units
    
    # Length data
    h <- lf(x, by = by_values[1])
    maxbin <- max(h$mids)
    
    # Avoiding maxbin < linf
    if (maxbin < linf) {
      
      bin_width <- diff(h$mids)[1]  # Calculate the bin width
      new_bins <- seq(maxbin + bin_width, linf + bin_width, by = bin_width)
      
      mids <- c(h$mids, new_bins)
      counts <- c(h$counts, rep(0, length(new_bins)))
      h <- data.frame(mids = mids, counts = counts)
      
      # Ordering
      ordering <- order(h$mids)
      h$mids <- h$mids[ordering]
      h$counts <- h$counts[ordering]
    }
    
    # Length data for LBSPR
    mylengthsobs <- new("LB_lengths", LB_pars = mypars, dataType = "freq", verbose = F)
    mylengthsobs@LMids <- as.numeric(h$mids)
    mylengthsobs@LData <- as.matrix(h$counts) 
    mylengthsobs@L_units <- units  
    mylengthsobs@Years <- as.numeric(year)  
    mylengthsobs@NYears <- length(as.numeric(year)) 
    
    # Fit the model
    fit <- LBSPRfit(mypars, mylengthsobs, verbose = FALSE)
    
    # Selectivity parameters
    sl50 <- fit@SL50
    
    # verify if sl50 is above the linf
    if (sl50 < h$mids[1] || sl50 > h$mids[length(h$mids)] || sl50 >= linf) {
      message("sl50 outside the mids range or greater than Linf. Adjusting sl50 to Linf - by.")
      sl50 <- linf - 1
    }
    
    return(sl50)
  }
}


#--------------------------------
#fitting to the observed lengths
#--------------------------------
sl_obs<- data.frame(yr=NULL,region=NULL,codsp=NULL,codgr=NULL,source=NULL,sl50=NULL,sl95=NULL)
years<- unique(smtlenobs$yr)
region<- unique(smtlenobs$region)
codsp<- sp #list with avaliable information
codgr<- unique(smtlenobs$codgr)
source<- unique(smtlenobs$source) 


#loop through the years, regions, codsp, codgr and source
for (i in years) {
    
    for (j in region) {
      
      for (k in codsp) {
        
        for (l in codgr) {
          
          for (m in source) {
            
           #obtaining the current length distribution and checking if it is valid
           len<- smtlenobs$fl[smtlenobs$yr==i & smtlenobs$region==j & smtlenobs$codsp==k & smtlenobs$codgr==l & smtlenobs$source==m]
            
           if (length(len)> 0 & length(len)> 12) {
             
           #selectivity estimation via Catch Curve
           sl50<- sel(x=len, 
                        linf = mean(lh_filtered$linf[lh_filtered$codsp==k],na.rm = T),
                        k= mean(lh_filtered$k[lh_filtered$codsp==k],na.rm = T),
                        l50= mean(lh_filtered$lm50[lh_filtered$codsp==k],na.rm = T),
                        l95= mean(lh_filtered$lm95[lh_filtered$codsp==k],na.rm = T),
                        mk= mean(c(lh_filtered$mk[lh_filtered$codsp==k],lh_filtered$mk_algaraja[lh_filtered$codsp==k],lh_filtered$mk_pauly[lh_filtered$codsp==k]),na.rm = T),
                        t0= mean(c(lh_filtered$t0,lh_filtered$t0_pauly),na.rm = T),
                        by_values = c(3,2,1,5,4,3,2,6,7,8,9),
                        year = i,
                        sp= k,
                        units = "cm",
                        method = "Catch curve")
           sl_out<- data.frame(yr=i,region=j,codsp=k,codgr=l,source=m,sl50=sl50,sl95=sl50*1.1)
            
          #binding the current estimate
          sl_obs<- rbind(sl_obs,sl_out)
           
           }
          }
        }
     }
  }
}


#----- taking the average for each one of the species -----#
sl_mean <- sl_obs %>%
  dplyr::group_by(codsp) %>%
  dplyr::summarise(
    sl50 = mean(sl50, na.rm = TRUE), 
    sl95 = mean(sl95, na.rm = TRUE)
  )
 
#binding in each life history data frame
BLF_sl<- data.frame(codsp=  "BLF",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="BLF"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="BLF"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
BLF_filtered$sl50<- NA
BLF_filtered$sl95<- NA
BLF_filtered<- rbind(BLF_filtered, BLF_sl)

# Ordering
BLF_filtered <- BLF_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#------------------------------------------------------------------------------------------------


#binding in each life history data frame
BRS_sl<- data.frame(codsp=  "BRS",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="BRS"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="BRS"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
BRS_filtered$sl50<- NA
BRS_filtered$sl95<- NA
BRS_filtered<- rbind(BRS_filtered, BRS_sl)

# Ordering
BRS_filtered <- BRS_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#------------------------------------------------------------------------------------------------


#binding in each life history data frame
DOL_sl<- data.frame(codsp=  "DOL",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="DOL"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="DOL"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
DOL_filtered$sl50<- NA
DOL_filtered$sl95<- NA
DOL_filtered<- rbind(DOL_filtered, DOL_sl)

# Ordering
DOL_filtered <- DOL_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#-------------------------------------------------------------------------------------------

#binding in each life history data frame
FRI_sl<- data.frame(codsp=  "FRI",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="FRI"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="FRI"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
FRI_filtered$sl50<- NA
FRI_filtered$sl95<- NA
FRI_filtered<- rbind(FRI_filtered, FRI_sl)

# Ordering
FRI_filtered <- FRI_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#-------------------------------------------------------------------------------------------

#binding in each life history data frame
KGM_sl<- data.frame(codsp=  "KGM",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="KGM"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="KGM"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
KGM_filtered$sl50<- NA
KGM_filtered$sl95<- NA
KGM_filtered<- rbind(KGM_filtered, KGM_sl)

# Ordering
KGM_filtered <- KGM_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#-------------------------------------------------------------------------------------------

#binding in each life history data frame
LTA_sl<- data.frame(codsp=  "LTA",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="LTA"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="LTA"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
LTA_filtered$sl50<- NA
LTA_filtered$sl95<- NA
LTA_filtered<- rbind(LTA_filtered, LTA_sl)

# Ordering
LTA_filtered <- LTA_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax","wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#-------------------------------------------------------------------------------------------

#binding in each life history data frame
WAH_sl<- data.frame(codsp=  "WAH",
                    country= 'Brazil',
                    period= NA,
                    growth_method= NA,
                    sex= "BB",
                    linf= NA,
                    k= NA,
                    t0=  NA,
                    t0_pauly= NA,
                    m=  NA,
                    m_algaraja= NA,
                    m_pauly= NA,  
                    mk=  NA,
                    mk_algaraja= NA,
                    mk_pauly= NA,
                    lm50= NA,
                    lm95=NA,
                    tmax=NA,
                    wl_a= NA,
                    wl_b= NA,
                    h= NA,
                    sl50= round(sl_mean$sl50[sl_mean$codsp=="WAH"], 2),
                    sl95= round(sl_mean$sl95[sl_mean$codsp=="WAH"], 2),
                    source= "Catch Curve",
                    stock= "AT-SW")

#adding to the original data frame       
WAH_filtered$sl50<- NA
WAH_filtered$sl95<- NA
WAH_filtered<- rbind(WAH_filtered, WAH_sl)

# Ordering
WAH_filtered <- WAH_filtered[, c("codsp", "country", "period", "growth_method", "sex", "linf", 
                                 "k", "t0", "t0_pauly", "m", "m_algaraja", "m_pauly",  
                                 "mk", "mk_algaraja", "mk_pauly", "lm50","lm95","tmax", "wl_a", "wl_b", 
                                 "h","sl50","sl95","source", "stock")]
#------------------------------------------------------------------------------------------



#-------------------------------  Binding all life history together again***** --------------------------------#
lh_filtered<- rbind(BLF_filtered,BRS_filtered,DOL_filtered,FRI_filtered,KGM_filtered,LTA_filtered,WAH_filtered)

# Writing the filtered life history in a csv file
write.table(
  x = lh_filtered,                   
  file = "smt_life_history_filtered.csv",          
  append = FALSE,                         
  dec = ".",                              
  sep = ",",                              
  row.names = FALSE,                      
  col.names = TRUE                        
)
#write in .xlsx 
writexl::write_xlsx(lh_filtered, "smt_life_history_filtered.xlsx") #xlsx
#--------------------------------------------------------------------------------------------------------------#



                               #-------------------------------------------------------------------#
                               #               Creating the input data for Stock Synthesis         #
                               #                   Catch file  and length files                    #
                               #     Different Scenarios hypothesis for Lengths and selectivity    #
                               #     1-  Observed Lengths (More reliable)                          #
                               #     2- Reconstructed lengths when the observed mean was available #
                               #     3- All reconstructed lengths to fit the model                 #
                               #     4- changing selectivity of the gears                          #
                               #-------------------------------------------------------------------#


   #-----------------------------------------------------------------
   # 1- Catch data file (Freire et al., 2021)- Reconstructed catches
   #-----------------------------------------------------------------
   p8 <- ggplot(data = smtct) +
         geom_area(aes(x = year, y = catch/1000, fill = type, colour = type), alpha = 0.4) +  
         facet_wrap(.~species, scales = "free") +
         labs(x = "Year", y = "Catch (1000 t)", fill = "", colour = "", caption = "AT-SW Stock") +
         scale_fill_viridis_d() +
         scale_color_viridis_d() +
         theme_classic(base_size = 14) %+replace% 
         theme(strip.background = element_blank(),
               plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
               legend.position = "right")
  p8

  ggplot2::ggsave("Catch_History.png",plot=p8, device = "png", units = "cm",
                                                          width = 28, height = 17)
  
  #catch series
  BLF_catch<- data.frame(Year= smtct$year[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66])
  BRS_catch<- data.frame(Year= smtct$year[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66])
  DOL_catch<- data.frame(Year= smtct$year[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66])  
  FRI_catch<- data.frame(Year= smtct$year[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66])  
  KGM_catch<- data.frame(Year= smtct$year[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66])
  LTA_catch<- data.frame(Year= smtct$year[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66])
  WAH_catch<- data.frame(Year= smtct$year[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66], Freire= smtct$catch[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66])

  #correcting the anormal peak in WAH catches
  WAH_catch$Freire[WAH_catch$Year==1988]= mean(c(WAH_catch$Freire[WAH_catch$Year==1987],WAH_catch$Freire[WAH_catch$Year==1989]))
  
                              #---------------------------------------------------------------#
                              #        Machine learning algorithm to predict catches          #
                              #Long-short term memory- LSTM to predict catch from 2015 to 2025#
                              #      Recurrent Neural Network- RNN- Specifically LSTM RNN     #
                              #---------------------------------------------------------------#
  
  
  #Python must be installed. version:3.10 is compatible with Keras: https://www.python.org/downloads/release/python-3100/
  #Click in "Windows installer (64-bit)" and select "add Python 3.10 to PATH" to be used in any terminal
  #Install Anaconda: https://www.anaconda.com/download/
  #Install Rtools(34): https://cran.r-project.org/bin/windows/Rtools/
  #installing Keras steps..
  #install.packages("keras") 
  #library(keras)
  #install_keras(method = "virtualenv", python_version = "3.10") #now you can install KERAS API in R
  #testing 
  library(keras)
  to_categorical(0:3)
  #packages for parallel processing---
  #install.packages("foreach")
  #install.packages("doParallel")
  library(gridExtra)
  library(tensorflow)
  library(keras)
  library(foreach)
  library(doParallel)
  
  #=================================================
  # Trying LSTM models to forecast catch data 
  #=================================================

  #------
  # BLF
  #------
  ct<-BLF_catch$Freire 
  yr<-BLF_catch$Year
  
  
  #vizual inspection of catches
  p9 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p9
  
  ggplot2::ggsave("Catches_BLF.png",plot=p9, device = "png", units = "cm",
                                                      width = 28, height = 16)
                  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
  #----------------------
    
  #--- parallel processing parameters ---#
  library(foreach)
  library(doParallel)
  # Number of cores 
  num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
  cl <- makeCluster(num_cores)  # cluster
  registerDoParallel(cl)        # register backend
  
  # Hyperparameters list 
  hyperparameter_combinations <- expand.grid(
    look_back = look_back,
    batch_size = batch_size,
    epochs = epochs,
    units = units,
    dropout = dropout,
    optimizer = optimizer
  )
  
   #=============================
   #LSTM via Parallel Processing
   #=============================
  library(tensorflow)
  library(keras)
  library(foreach)
  library(doParallel)
   
    lstm_models <- foreach(
      param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
                                      .packages = c("keras", "tensorflow")) %dopar% {
          #parameters iteration
          i <- param$look_back
          j <- param$batch_size
          k <- param$epochs
          l <- param$units
          z <- param$dropout
          c <- param$optimizer
          
          #early stopping parameters
          callback <- callback_early_stopping(
            monitor = "val_mean_absolute_error", #validation monitor
            patience = pattience,               # Allowing n epochs without improvement
            restore_best_weights = TRUE  # keeping the best model
          )
          
           # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
          if (i==0) { #no sliding windows require different training data sets
            x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
            y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
            input_shape = c(2, 1)
          } else { #sliding windows (lagged time series as training data sets)
            x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
            y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
            input_shape = c(i, 1)
          }
            # Function to create the architecture, fit and evaluate the models
            train_and_evaluate <- function(model_architecture) {
                model <- model_architecture %>% 
                compile(loss = loss, optimizer = c, metrics = metrics)
                
                #fit ( early stopping, validation split and no shuffle)
                model %>% fit(x = x, y = y,
                              validation_split = val_split,
                              batch_size = j, 
                              epochs = k, 
                              verbose = 0, 
                              shuffle = FALSE,
                              callbacks = list(callback))
                
                metrics <- model %>% evaluate(x, y, batch_size = j)
                preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
                train_size<- length(x[,,1][,1])
                
                # Coverting to the same size (sliding windows )
                if (i==0) {
                  aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
                  aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
                  
                } else {
                  aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
                  #converting back to the original scale
                  aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
                  aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
                }
                
                # Residuals
                residuals <- aligned_ct - aligned_pred
                # RMSE metric
                rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
                # R² metric
                ss_res <- sum(residuals^2, na.rm = TRUE)
                ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
                r_squared <- 1 - (ss_res / ss_tot)
      
                #list of results
                list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
              }
    
              # fitting the models
              results_single <- train_and_evaluate(
                 keras_model_sequential() %>%
                layer_lstm(units = l, input_shape = input_shape) %>%
                layer_dropout(rate = z) %>%
                layer_dense(units = 1)
                 )
    
              results_multi <- train_and_evaluate(
                keras_model_sequential() %>%
                layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
                layer_dropout(rate = z) %>%
                layer_lstm(units = l, return_sequences = FALSE) %>%
                layer_dropout(rate = z) %>%
                layer_dense(units = 1)
                )
    
          results_deep <- train_and_evaluate(
            keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
            )
    
    # Combining results
      data.frame(
      model = c("single", "multi", "deep"),
      mae = c(results_single$mae, results_multi$mae, results_deep$mae),
      rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
      r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
      look_back = i,
      batch_size = j,
      epochs = k,
      units = l,
      dropout = z,
      optimizer = c
    )
  }
  
  # close cluster
  stopCluster(cl)
  
  # Results
  head(lstm_models)
  
  # R2 x Look_back
  p10 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
    labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # R2 x batch_size
  p11 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
    labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # R2 x epochs
  p12 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
    labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # R2 x units
  p13 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
    labs(x = "Units", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

  # R2 x optimizer
  p14 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                     y = r2, group = interaction(factor(optimizer), factor(model)))) +
    labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # Combining the plots
  p15<-grid.arrange(p10, p11, p12, p13, p14, ncol = 3)
  
  ggplot2::ggsave("Lstm_models_comparison_BLF.png",plot=p15, device = "png", units = "cm",
                                                    width = 28, height = 16)
  
  #best model
  best<-lstm_models[which.max(lstm_models$r2),]
  print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
   best<- data.frame(model="single", 
                    mae=0.38,     
                    rmse=145.65,
                    r2=0.64,
                    look_back=5,
                    batch_size=16,
                    epochs= 200,
                    units= 50, 
                    dropout=0.3,
                    optimizer="adam")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 6 #number of years looking back to make the forecast
  sample_yr<- 35 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Predictions using 1 sample 3-D array
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Updating the sequence with the new value
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
    for(i in 1:nit) {
        
        if (best$model=="single") { #assigning models
            
           best_model<- keras_model_sequential() %>%
           layer_lstm(units = best$units, input_shape = input_shape) %>%
           layer_dropout(rate = best$dropout) %>%
           layer_dense(units = 1)} else if (
            best$model=="multi") {
              
            best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)} else if (
              best$model=="deep") {
          
            best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)
            }
          
            #compiling
            best_model %>% 
            compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
            #fiting the models
            best_model %>% fit(x = x, y = y,
                      validation_split = val_split,
                      batch_size = best$batch_size, 
                      epochs = best$epochs, 
                      verbose = 0, 
                      shuffle = FALSE,
                      callbacks = list(callback))
            #predicting
            preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
            preds <- preds * msd.ct[2] + msd.ct[1]
            noise_sd <- 0.1 * mean(preds, na.rm = TRUE)  
            preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
            
          #future predicion
          future_pred <- forecast(best_model, tail(x, sample_yr), forecast_yr, window_size , best$batch_size)
          future_pred <- future_pred * msd.ct[2] + msd.ct[1]
          future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)
          
          if (plot_it==TRUE) {
            
          plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(ct)))
          par(new=TRUE)
          plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(ct)))
          lines(c(rep(0,length(ct)),future_pred))
          par(new=TRUE)
          }
          
        # ensuring compatibility
        preds <- matrix(preds, ncol = 1)
        future_pred <- matrix(future_pred, ncol = 1)
        
        # Concatenate
        preds <- rbind(preds, future_pred)
        
        pred_out[[i]] <- data.frame(it = i, fit = preds)
    best_model %>% reset_states() #reset the states 
  }
  
  pred_out <- do.call(rbind, pred_out)
  
  # Calculating average and confidence intervals
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = quantile(fit, probs = 0.025, na.rm = TRUE),
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and predicted catches
  lstm_pred<-data.frame(
             yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
             ct=c(ct, rep(NA,forecast_yr)),
             pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
             lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
             up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future data
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% # Ajuste para o intervalo de previsão futura
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # combining
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p16 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p16
  
  ggplot2::ggsave("Lstm_fit_BLF.png",plot=p16, device = "png", units = "cm",
                                                                  width = 28, height = 16)
  
  #assign the sp data-----
  best_BLF<- best
  lstm_pred_BLF<- lstm_pred
  #-------------------------
  
  
  #==============
  #      BRS    #
  #==============
  ct<-BRS_catch$Freire 
  yr<-BRS_catch$Year
  
  
  #vizual inspection of catches
  p17 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p17
  
  ggplot2::ggsave("Catches_BRS.png",plot=p17, device = "png", units = "cm",
                                                        width = 28, height = 16)
  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
    #----------------------
    
    #--- parallel processing parameters ---#
    library(foreach)
    library(doParallel)
    # Number of cores 
    num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
    cl <- makeCluster(num_cores)  # cluster
    registerDoParallel(cl)        # register backend
    
    # Hyperparameters list 
    hyperparameter_combinations <- expand.grid(
      look_back = look_back,
      batch_size = batch_size,
      epochs = epochs,
      units = units,
      dropout = dropout,
      optimizer = optimizer
    )
    
      #=============================
      #LSTM via Parallel Processing
      #=============================
      library(tensorflow)
      library(keras)
      library(foreach)
      library(doParallel)
    
      lstm_models <- foreach(
          param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
          .packages = c("keras", "tensorflow")) %dopar% {
        
            #parameters iteration
            i <- param$look_back
            j <- param$batch_size
            k <- param$epochs
            l <- param$units
            z <- param$dropout
            c <- param$optimizer
        
         #early stopping parameters
         callback <- callback_early_stopping(
           monitor = "val_mean_absolute_error", #validation monitor
           patience = pattience,               # Allowing n epochs without improvement
           restore_best_weights = TRUE  # keeping the best model
          )
        
         # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
         if (i==0) { #no sliding windows require different training data sets
           x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
           y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
           input_shape = c(2, 1)
         } else { #sliding windows (lagged time series as training data sets)
           x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
           y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
           input_shape = c(i, 1)
         }
         
          # Function to create the architecture, fit and evaluate the models
          train_and_evaluate <- function(model_architecture) {
            model <- model_architecture %>% 
            compile(loss = loss, optimizer = c, metrics = metrics)
          
           #fit ( early stopping, validation split and no shuffle)
           model %>% fit(x = x, y = y,
                        validation_split = val_split,
                        batch_size = j, 
                        epochs = k, 
                        verbose = 0, 
                        shuffle = FALSE,
                        callbacks = list(callback))
          
           metrics <- model %>% evaluate(x, y, batch_size = j)
           preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
           train_size<- length(x[,,1][,1])
          
          # Coverting to the same size (sliding windows )
          if (i==0) {
            aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
            
          } else {
            aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
            #converting back to the original scale
            aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
          }
          
          # Residuals
          residuals <- aligned_ct - aligned_pred
          # RMSE metric
          rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
          # R² metric
          ss_res <- sum(residuals^2, na.rm = TRUE)
          ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
          r_squared <- 1 - (ss_res / ss_tot)
          
          #list of results
          list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
        }
        
        # fitting the models
        results_single <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_multi <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_deep <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
    # Combining results
    data.frame(
      model = c("single", "multi", "deep"),
      mae = c(results_single$mae, results_multi$mae, results_deep$mae),
      rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
      r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
      look_back = i,
      batch_size = j,
      epochs = k,
      units = l,
      dropout = z,
      optimizer = c
    )
}
    
    # close cluster
    stopCluster(cl)
    
    # Results
    head(lstm_models)
    
    # R2 x Look_back
    p18 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p19 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p20 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p21 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p22 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p23<-grid.arrange(p18, p19, p20, p21, p22, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_BRS.png",plot=p23, device = "png", units = "cm",
                    width = 28, height = 16)
    
    #best model
    best<-lstm_models[which.max(lstm_models$r2),]
    print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
    best<- data.frame(model="multi", 
                      mae=0.19,     
                      rmse=1163.2,
                      r2=0.85,
                      look_back=5,
                      batch_size=8,
                      epochs= 200,
                      units= 100, 
                      dropout=0.3,
                      optimizer="rmsprop")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 5 #number of years looking back to make the forecast
  sample_yr<- 17 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Realiza a previsão com a estrutura correta de array tridimensional
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Atualiza a sequência para a próxima previsão
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
  for(i in 1:nit) {
    
    if (best$model=="single") { #assigning models
      
      best_model<- keras_model_sequential() %>%
        layer_lstm(units = best$units, input_shape = input_shape) %>%
        layer_dropout(rate = best$dropout) %>%
        layer_dense(units = 1)} else if (
          best$model=="multi") {
          
          best_model<- keras_model_sequential() %>%
           layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
           layer_dropout(rate = best$dropout) %>%
           layer_lstm(units = best$units, return_sequences = FALSE) %>%
           layer_dropout(rate = best$dropout) %>%
           layer_dense(units = 1)} else if (
             best$model=="deep") {
              
            best_model<- keras_model_sequential() %>%
             layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
             layer_dropout(rate = best$dropout) %>%
             layer_lstm(units = best$units, return_sequences = TRUE) %>%
             layer_dropout(rate = best$dropout) %>%
             layer_lstm(units = best$units, return_sequences = FALSE) %>%
             layer_dropout(rate = best$dropout) %>%
             layer_dense(units = 1)
            }
    
           #compiling
           best_model %>% 
           compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
           #fiting the models
           best_model %>% fit(x = x, y = y,
                       validation_split = val_split,
                       batch_size = best$batch_size, 
                       epochs = best$epochs, 
                       verbose = 0, 
                       shuffle = FALSE,
                       callbacks = list(callback))
        #predicting
        preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
        preds <- preds * msd.ct[2] + msd.ct[1]
        noise_sd <- 0.05 * mean(preds, na.rm = TRUE)  
        preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
    
       #future predicion
       future_pred <- forecast(best_model, tail(x, sample_yr), forecast_yr, window_size  , best$batch_size)
       future_pred <- future_pred * msd.ct[2] + msd.ct[1]
       future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)

      if (plot_it==TRUE) {
  
      plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(ct)))
     par(new=TRUE)
     plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(ct)))
    lines(c(rep(0,length(ct)),future_pred))
    par(new=TRUE)
  }
    
 # ensuring compatibility
 preds <- matrix(preds, ncol = 1)
 future_pred <- matrix(future_pred, ncol = 1)
 
 # Concatenate
 preds <- rbind(preds, future_pred)
    
 pred_out[[i]] <- data.frame(it = i, fit = preds)
 best_model %>% reset_states() #reset the states 
}
  
  pred_out <- do.call(rbind, pred_out)
  
  # Mean and confidence intervals 
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = quantile(fit, probs = 0.025, na.rm = TRUE),
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and Fitted data
  lstm_pred<-data.frame(
    yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
    ct=c(ct, rep(NA,forecast_yr)),
    pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
    lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
    up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p24 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p24
  
  ggplot2::ggsave("Lstm_fit_BRS.png",plot=p24, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_BRS<- best
  lstm_pred_BRS<- lstm_pred
  #-------------------------
  
  
  
  
  #==============
  #      DOL    #
  #==============
  ct<-DOL_catch$Freire 
  yr<-DOL_catch$Year
  
  
  #vizual inspection of catches
  p25 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p25
  
  ggplot2::ggsave("Catches_DOL.png",plot=p25, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
    #----------------------
    
    #--- parallel processing parameters ---#
    library(foreach)
    library(doParallel)
    # Number of cores 
    num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
    cl <- makeCluster(num_cores)  # cluster
    registerDoParallel(cl)        # register backend
    
    # Hyperparameters list 
    hyperparameter_combinations <- expand.grid(
      look_back = look_back,
      batch_size = batch_size,
      epochs = epochs,
      units = units,
      dropout = dropout,
      optimizer = optimizer
    )
    
    #=============================
    #LSTM via Parallel Processing
    #=============================
    library(tensorflow)
    library(keras)
    library(foreach)
    library(doParallel)
    
    lstm_models <- foreach(
      param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
      .packages = c("keras", "tensorflow")) %dopar% {
        
        #parameters iteration
        i <- param$look_back
        j <- param$batch_size
        k <- param$epochs
        l <- param$units
        z <- param$dropout
        c <- param$optimizer
        
        #early stopping parameters
        callback <- callback_early_stopping(
          monitor = "val_mean_absolute_error", #validation monitor
          patience = pattience,               # Allowing n epochs without improvement
          restore_best_weights = TRUE  # keeping the best model
        )
        
        # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
        if (i==0) { #no sliding windows require different training data sets
          x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
          y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
          input_shape = c(2, 1)
        } else { #sliding windows (lagged time series as training data sets)
          x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
          y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
          input_shape = c(i, 1)
        }
        
        # Function to create the architecture, fit and evaluate the models
        train_and_evaluate <- function(model_architecture) {
          model <- model_architecture %>% 
            compile(loss = loss, optimizer = c, metrics = metrics)
          
          #fit ( early stopping, validation split and no shuffle)
          model %>% fit(x = x, y = y,
                        validation_split = val_split,
                        batch_size = j, 
                        epochs = k, 
                        verbose = 0, 
                        shuffle = FALSE,
                        callbacks = list(callback))
          
          metrics <- model %>% evaluate(x, y, batch_size = j)
          preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
          train_size<- length(x[,,1][,1])
          
          # Coverting to the same size (sliding windows )
          if (i==0) {
            aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
            
          } else {
            aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
            #converting back to the original scale
            aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
          }
          
          # Residuals
          residuals <- aligned_ct - aligned_pred
          # RMSE metric
          rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
          # R² metric
          ss_res <- sum(residuals^2, na.rm = TRUE)
          ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
          r_squared <- 1 - (ss_res / ss_tot)
          
          #list of results
          list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
        }
        
        # fitting the models
        results_single <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_multi <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_deep <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        # Combining results
        data.frame(
          model = c("single", "multi", "deep"),
          mae = c(results_single$mae, results_multi$mae, results_deep$mae),
          rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
          r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
          look_back = i,
          batch_size = j,
          epochs = k,
          units = l,
          dropout = z,
          optimizer = c
        )
      }
    
    # close cluster
    stopCluster(cl)
    
    # Results
    head(lstm_models)
    
    # R2 x Look_back
    p26 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p27 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p28 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p29 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p30 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p31<-grid.arrange(p26, p27, p28, p29, p30, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_DOL.png",plot=p31, device = "png", units = "cm",
                    width = 28, height = 16)
    
    #best model
    best<-lstm_models[which.max(lstm_models$r2),]
    print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
    best<- data.frame(model="deep", 
                      mae=0.20,     
                      rmse=815.24,
                      r2=0.90,
                      look_back=2,
                      batch_size=16,
                      epochs= 200,
                      units= 100, 
                      dropout=0.3,
                      optimizer="adam")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 2#number of years looking back to make the forecast
  sample_yr<- 8 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Realiza a previsão com a estrutura correta de array tridimensional
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Atualiza a sequência para a próxima previsão
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
  for(i in 1:nit) {
    
    if (best$model=="single") { #assigning models
      
      best_model<- keras_model_sequential() %>%
        layer_lstm(units = best$units, input_shape = input_shape) %>%
        layer_dropout(rate = best$dropout) %>%
        layer_dense(units = 1)} else if (
          best$model=="multi") {
          
          best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)} else if (
              best$model=="deep") {
              
              best_model<- keras_model_sequential() %>%
                layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = FALSE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_dense(units = 1)
            }
    
    #compiling
    best_model %>% 
      compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
    #fiting the models
    best_model %>% fit(x = x, y = y,
                       validation_split = val_split,
                       batch_size = best$batch_size, 
                       epochs = best$epochs, 
                       verbose = 0, 
                       shuffle = FALSE,
                       callbacks = list(callback))
    #predicting
    preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
    preds <- preds * msd.ct[2] + msd.ct[1]
    noise_sd <- 0.1 * mean(preds, na.rm = TRUE)  
    preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
    
    #future predicion
    future_pred <- forecast(best_model, tail(x, sample_yr), forecast_yr, window_size , best$batch_size)
    future_pred <- future_pred * msd.ct[2] + msd.ct[1]
    future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)
    
    if (plot_it==TRUE) {
      
      plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      par(new=TRUE)
      plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      lines(c(rep(0,length(ct)),future_pred))
      par(new=TRUE)
    }
    
    # ensuring compatibility
    preds <- matrix(preds, ncol = 1)
    future_pred <- matrix(future_pred, ncol = 1)
    
    # Concatenate
    preds <- rbind(preds, future_pred)
    
    pred_out[[i]] <- data.frame(it = i, fit = preds)
    best_model %>% reset_states() #reset the states 
  }
  
  pred_out <- do.call(rbind, pred_out)
  
  # Mean and confidence intervals 
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = pmax(quantile(fit, probs = 0.025, na.rm = TRUE), 0), # Ajusta para 0 se menor
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and Fitted data
  lstm_pred<-data.frame(
    yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
    ct=c(ct, rep(NA,forecast_yr)),
    pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
    lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
    up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p32 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p32
  
  ggplot2::ggsave("Lstm_fit_DOL.png",plot=p32, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_DOL<- best
  lstm_pred_DOL<- lstm_pred
  #-------------------------
  
  
  
  
  #==============
  #      FRI    #
  #==============
  ct<-FRI_catch$Freire 
  yr<-FRI_catch$Year
  
  
  #vizual inspection of catches
  p33 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p33
  
  ggplot2::ggsave("Catches_DOL.png",plot=p33, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
    #----------------------
    
    #--- parallel processing parameters ---#
    library(foreach)
    library(doParallel)
    # Number of cores 
    num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
    cl <- makeCluster(num_cores)  # cluster
    registerDoParallel(cl)        # register backend
    
    # Hyperparameters list 
    hyperparameter_combinations <- expand.grid(
      look_back = look_back,
      batch_size = batch_size,
      epochs = epochs,
      units = units,
      dropout = dropout,
      optimizer = optimizer
    )
    
    #=============================
    #LSTM via Parallel Processing
    #=============================
    library(tensorflow)
    library(keras)
    library(foreach)
    library(doParallel)
    
    lstm_models <- foreach(
      param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
      .packages = c("keras", "tensorflow")) %dopar% {
        
        #parameters iteration
        i <- param$look_back
        j <- param$batch_size
        k <- param$epochs
        l <- param$units
        z <- param$dropout
        c <- param$optimizer
        
        #early stopping parameters
        callback <- callback_early_stopping(
          monitor = "val_mean_absolute_error", #validation monitor
          patience = pattience,               # Allowing n epochs without improvement
          restore_best_weights = TRUE  # keeping the best model
        )
        
        # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
        if (i==0) { #no sliding windows require different training data sets
          x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
          y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
          input_shape = c(2, 1)
        } else { #sliding windows (lagged time series as training data sets)
          x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
          y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
          input_shape = c(i, 1)
        }
        
        # Function to create the architecture, fit and evaluate the models
        train_and_evaluate <- function(model_architecture) {
          model <- model_architecture %>% 
            compile(loss = loss, optimizer = c, metrics = metrics)
          
          #fit ( early stopping, validation split and no shuffle)
          model %>% fit(x = x, y = y,
                        validation_split = val_split,
                        batch_size = j, 
                        epochs = k, 
                        verbose = 0, 
                        shuffle = FALSE,
                        callbacks = list(callback))
          
          metrics <- model %>% evaluate(x, y, batch_size = j)
          preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
          train_size<- length(x[,,1][,1])
          
          # Coverting to the same size (sliding windows )
          if (i==0) {
            aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
            
          } else {
            aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
            #converting back to the original scale
            aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
          }
          
          # Residuals
          residuals <- aligned_ct - aligned_pred
          # RMSE metric
          rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
          # R² metric
          ss_res <- sum(residuals^2, na.rm = TRUE)
          ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
          r_squared <- 1 - (ss_res / ss_tot)
          
          #list of results
          list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
        }
        
        # fitting the models
        results_single <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_multi <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_deep <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        # Combining results
        data.frame(
          model = c("single", "multi", "deep"),
          mae = c(results_single$mae, results_multi$mae, results_deep$mae),
          rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
          r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
          look_back = i,
          batch_size = j,
          epochs = k,
          units = l,
          dropout = z,
          optimizer = c
        )
      }
    
    # close cluster
    stopCluster(cl)
    
    # Results
    head(lstm_models)
    
    # R2 x Look_back
    p34 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p35 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p36 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p37 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p38 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p39<-grid.arrange(p34, p35, p36, p37, p38, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_FRI.png",plot=p39, device = "png", units = "cm",
                                                                      width = 28, height = 16)
    
    #best model
    best<-lstm_models[which.max(lstm_models$r2),]
    print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
    best<- data.frame(model="deep", 
                      mae=0.34,     
                      rmse=157.8,
                      r2=0.61,
                      look_back=2,
                      batch_size=4,
                      epochs= 200,
                      units= 10, 
                      dropout=0.3,
                      optimizer="adam")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 1#number of years looking back to make the forecast
  sample_yr<- 1 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Realiza a previsão com a estrutura correta de array tridimensional
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Atualiza a sequência para a próxima previsão
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
  for(i in 1:nit) {
    
    if (best$model=="single") { #assigning models
      
      best_model<- keras_model_sequential() %>%
        layer_lstm(units = best$units, input_shape = input_shape) %>%
        layer_dropout(rate = best$dropout) %>%
        layer_dense(units = 1)} else if (
          best$model=="multi") {
          
          best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)} else if (
              best$model=="deep") {
              
              best_model<- keras_model_sequential() %>%
                layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = FALSE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_dense(units = 1)
            }
    
    #compiling
    best_model %>% 
      compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
    #fiting the models
    best_model %>% fit(x = x, y = y,
                       validation_split = val_split,
                       batch_size = best$batch_size, 
                       epochs = best$epochs, 
                       verbose = 0, 
                       shuffle = FALSE,
                       callbacks = list(callback))
    #predicting
    preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
    preds <- preds * msd.ct[2] + msd.ct[1]
    noise_sd <- 0.1 * mean(preds, na.rm = TRUE)  
    preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
    
    #future predicion
    future_pred <- forecast(best_model, tail(x, forecast_yr), forecast_yr, window_size  , best$batch_size)
    future_pred <- future_pred * msd.ct[2] + msd.ct[1]
    future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)
    
    if (plot_it==TRUE) {
      
      plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      par(new=TRUE)
      plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      lines(c(rep(0,length(ct)),future_pred))
      par(new=TRUE)
    }
    
    # ensuring compatibility
    preds <- matrix(preds, ncol = 1)
    future_pred <- matrix(future_pred, ncol = 1)
    
    # Concatenate
    preds <- rbind(preds, future_pred)
    
    pred_out[[i]] <- data.frame(it = i, fit = preds)
    best_model %>% reset_states() #reset the states 
  }
  
  pred_out <- do.call(rbind, pred_out)
  
  # Mean and confidence intervals 
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = pmax(quantile(fit, probs = 0.025, na.rm = TRUE), 0), # Ajusta para 0 se menor
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and Fitted data
  lstm_pred<-data.frame(
    yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
    ct=c(ct, rep(NA,forecast_yr)),
    pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
    lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
    up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p40 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p40
  
  ggplot2::ggsave("Lstm_fit_FRI.png",plot=p40, device = "png", units = "cm",
                                                        width = 28, height = 16)
  
  #assign the sp data------
  best_FRI<- best
  lstm_pred_FRI<- lstm_pred
  #-------------------------
  
  
  
  
  #==============
  #      KGM    #
  #==============
  ct<-KGM_catch$Freire 
  yr<-KGM_catch$Year
  
  
  #vizual inspection of catches
  p41 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p41
  
  ggplot2::ggsave("Catches_KGM.png",plot=p41, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
    #----------------------
    
    #--- parallel processing parameters ---#
    library(foreach)
    library(doParallel)
    # Number of cores 
    num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
    cl <- makeCluster(num_cores)  # cluster
    registerDoParallel(cl)        # register backend
    
    # Hyperparameters list 
    hyperparameter_combinations <- expand.grid(
      look_back = look_back,
      batch_size = batch_size,
      epochs = epochs,
      units = units,
      dropout = dropout,
      optimizer = optimizer
    )
    
    #=============================
    #LSTM via Parallel Processing
    #=============================
    library(tensorflow)
    library(keras)
    library(foreach)
    library(doParallel)
    
    lstm_models <- foreach(
      param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
      .packages = c("keras", "tensorflow")) %dopar% {
        
        #parameters iteration
        i <- param$look_back
        j <- param$batch_size
        k <- param$epochs
        l <- param$units
        z <- param$dropout
        c <- param$optimizer
        
        #early stopping parameters
        callback <- callback_early_stopping(
          monitor = "val_mean_absolute_error", #validation monitor
          patience = pattience,               # Allowing n epochs without improvement
          restore_best_weights = TRUE  # keeping the best model
        )
        
        # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
        if (i==0) { #no sliding windows require different training data sets
          x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
          y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
          input_shape = c(2, 1)
        } else { #sliding windows (lagged time series as training data sets)
          x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
          y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
          input_shape = c(i, 1)
        }
        
        # Function to create the architecture, fit and evaluate the models
        train_and_evaluate <- function(model_architecture) {
          model <- model_architecture %>% 
            compile(loss = loss, optimizer = c, metrics = metrics)
          
          #fit ( early stopping, validation split and no shuffle)
          model %>% fit(x = x, y = y,
                        validation_split = val_split,
                        batch_size = j, 
                        epochs = k, 
                        verbose = 0, 
                        shuffle = FALSE,
                        callbacks = list(callback))
          
          metrics <- model %>% evaluate(x, y, batch_size = j)
          preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
          train_size<- length(x[,,1][,1])
          
          # Coverting to the same size (sliding windows )
          if (i==0) {
            aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
            
          } else {
            aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
            #converting back to the original scale
            aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
          }
          
          # Residuals
          residuals <- aligned_ct - aligned_pred
          # RMSE metric
          rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
          # R² metric
          ss_res <- sum(residuals^2, na.rm = TRUE)
          ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
          r_squared <- 1 - (ss_res / ss_tot)
          
          #list of results
          list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
        }
        
        # fitting the models
        results_single <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_multi <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_deep <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        # Combining results
        data.frame(
          model = c("single", "multi", "deep"),
          mae = c(results_single$mae, results_multi$mae, results_deep$mae),
          rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
          r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
          look_back = i,
          batch_size = j,
          epochs = k,
          units = l,
          dropout = z,
          optimizer = c
        )
      }
    
    # close cluster
    stopCluster(cl)
    
    # Results
    head(lstm_models)
    
    # R2 x Look_back
    p42 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p43 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p44 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p45 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p46 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p47<-grid.arrange(p42, p43, p44, p45, p46, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_KGM.png",plot=p47, device = "png", units = "cm",
                    width = 28, height = 16)
    
    #best model
    best<-lstm_models[which.max(lstm_models$r2),]
    print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
    best<- data.frame(model="deep", 
                      mae=0.21,     
                      rmse=383.32,
                      r2=0.89,
                      look_back=3,
                      batch_size=8,
                      epochs= 200,
                      units= 100, 
                      dropout=0.3,
                      optimizer="rmsprop")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 4#number of years looking back to make the forecast
  sample_yr<- 5 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Realiza a previsão com a estrutura correta de array tridimensional
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Atualiza a sequência para a próxima previsão
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
  for(i in 1:nit) {
    
    if (best$model=="single") { #assigning models
      
      best_model<- keras_model_sequential() %>%
        layer_lstm(units = best$units, input_shape = input_shape) %>%
        layer_dropout(rate = best$dropout) %>%
        layer_dense(units = 1)} else if (
          best$model=="multi") {
          
          best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)} else if (
              best$model=="deep") {
              
              best_model<- keras_model_sequential() %>%
                layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = FALSE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_dense(units = 1)
            }
    
    #compiling
    best_model %>% 
      compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
    #fiting the models
    best_model %>% fit(x = x, y = y,
                       validation_split = val_split,
                       batch_size = best$batch_size, 
                       epochs = best$epochs, 
                       verbose = 0, 
                       shuffle = FALSE,
                       callbacks = list(callback))
    #predicting
    preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
    preds <- preds * msd.ct[2] + msd.ct[1]
    noise_sd <- 0.1 * mean(preds, na.rm = TRUE)  
    preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
    
    #future predicion
    future_pred <- forecast(best_model, tail(x, sample_yr), forecast_yr,window_size , best$batch_size)
    future_pred <- future_pred * msd.ct[2] + msd.ct[1]
    future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)
    
    if (plot_it==TRUE) {
      #dev.off()
      plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      par(new=TRUE)
      plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      lines(c(rep(0,length(ct)),future_pred))
      par(new=TRUE)
    }
    
    # ensuring compatibility
    preds <- matrix(preds, ncol = 1)
    future_pred <- matrix(future_pred, ncol = 1)
    
    # Concatenate
    preds <- rbind(preds, future_pred)
    
    pred_out[[i]] <- data.frame(it = i, fit = preds)
    best_model %>% reset_states() #reset the states 
  }
  
  pred_out <- do.call(rbind, pred_out)
  
  # Mean and confidence intervals 
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = pmax(quantile(fit, probs = 0.025, na.rm = TRUE), 0), # Ajusta para 0 se menor
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and Fitted data
  lstm_pred<-data.frame(
    yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
    ct=c(ct, rep(NA,forecast_yr)),
    pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
    lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
    up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p48 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p48
  
  ggplot2::ggsave("Lstm_fit_KGM.png",plot=p48, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_KGM<- best
  lstm_pred_KGM<- lstm_pred
  #-------------------------
  
  
  
  
  #==============
  #      LTA    #
  #==============
  ct<-LTA_catch$Freire 
  yr<-LTA_catch$Year
  
  
  #vizual inspection of catches
  p49 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p49
  
  ggplot2::ggsave("Catches_LTA.png",plot=p49, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
    #----------------------
    
    #--- parallel processing parameters ---#
    library(foreach)
    library(doParallel)
    # Number of cores 
    num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
    cl <- makeCluster(num_cores)  # cluster
    registerDoParallel(cl)        # register backend
    
    # Hyperparameters list 
    hyperparameter_combinations <- expand.grid(
      look_back = look_back,
      batch_size = batch_size,
      epochs = epochs,
      units = units,
      dropout = dropout,
      optimizer = optimizer
    )
    
    #=============================
    #LSTM via Parallel Processing
    #=============================
    library(tensorflow)
    library(keras)
    library(foreach)
    library(doParallel)
    
    lstm_models <- foreach(
      param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
      .packages = c("keras", "tensorflow")) %dopar% {
        
        #parameters iteration
        i <- param$look_back
        j <- param$batch_size
        k <- param$epochs
        l <- param$units
        z <- param$dropout
        c <- param$optimizer
        
        #early stopping parameters
        callback <- callback_early_stopping(
          monitor = "val_mean_absolute_error", #validation monitor
          patience = pattience,               # Allowing n epochs without improvement
          restore_best_weights = TRUE  # keeping the best model
        )
        
        # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
        if (i==0) { #no sliding windows require different training data sets
          x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
          y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
          input_shape = c(2, 1)
        } else { #sliding windows (lagged time series as training data sets)
          x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
          y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
          input_shape = c(i, 1)
        }
        
        # Function to create the architecture, fit and evaluate the models
        train_and_evaluate <- function(model_architecture) {
          model <- model_architecture %>% 
            compile(loss = loss, optimizer = c, metrics = metrics)
          
          #fit ( early stopping, validation split and no shuffle)
          model %>% fit(x = x, y = y,
                        validation_split = val_split,
                        batch_size = j, 
                        epochs = k, 
                        verbose = 0, 
                        shuffle = FALSE,
                        callbacks = list(callback))
          
          metrics <- model %>% evaluate(x, y, batch_size = j)
          preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
          train_size<- length(x[,,1][,1])
          
          # Coverting to the same size (sliding windows )
          if (i==0) {
            aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
            
          } else {
            aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
            #converting back to the original scale
            aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
          }
          
          # Residuals
          residuals <- aligned_ct - aligned_pred
          # RMSE metric
          rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
          # R² metric
          ss_res <- sum(residuals^2, na.rm = TRUE)
          ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
          r_squared <- 1 - (ss_res / ss_tot)
          
          #list of results
          list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
        }
        
        # fitting the models
        results_single <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_multi <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_deep <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        # Combining results
        data.frame(
          model = c("single", "multi", "deep"),
          mae = c(results_single$mae, results_multi$mae, results_deep$mae),
          rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
          r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
          look_back = i,
          batch_size = j,
          epochs = k,
          units = l,
          dropout = z,
          optimizer = c
        )
      }
    
    # close cluster
    stopCluster(cl)
    
    # Results
    head(lstm_models)
    
    # R2 x Look_back
    p50 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p51 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p52 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p53 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p54 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p55<-grid.arrange(p50, p51, p52, p53, p54, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_LTA.png",plot=p55, device = "png", units = "cm",
                    width = 28, height = 16)
    
    #best model
    best<-lstm_models[which.max(lstm_models$r2),]
    print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
    best<- data.frame(model="deep", 
                      mae=0.35,     
                      rmse=208.38,
                      r2=0.76,
                      look_back=2,
                      batch_size=4,
                      epochs= 200,
                      units= 50, 
                      dropout=0.3,
                      optimizer="adam")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 1 #number of years looking back to make the forecast
  sample_yr<- 20 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Realiza a previsão com a estrutura correta de array tridimensional
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Atualiza a sequência para a próxima previsão
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
  for(i in 1:nit) {
    
    if (best$model=="single") { #assigning models
      
      best_model<- keras_model_sequential() %>%
        layer_lstm(units = best$units, input_shape = input_shape) %>%
        layer_dropout(rate = best$dropout) %>%
        layer_dense(units = 1)} else if (
          best$model=="multi") {
          
          best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)} else if (
              best$model=="deep") {
              
              best_model<- keras_model_sequential() %>%
                layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = FALSE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_dense(units = 1)
            }
    
    #compiling
    best_model %>% 
      compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
    #fiting the models
    best_model %>% fit(x = x, y = y,
                       validation_split = val_split,
                       batch_size = best$batch_size, 
                       epochs = best$epochs, 
                       verbose = 0, 
                       shuffle = FALSE,
                       callbacks = list(callback))
    #predicting
    preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
    preds <- preds * msd.ct[2] + msd.ct[1]
    noise_sd <- 0.1 * mean(preds, na.rm = TRUE)  
    preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
    
    #future predicion
    future_pred <- forecast(best_model, tail(x, sample_yr), forecast_yr, window_size  , best$batch_size)
    future_pred <- future_pred * msd.ct[2] + msd.ct[1]
    future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)
    
    if (plot_it==TRUE) {
      
      plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      par(new=TRUE)
      plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      lines(c(rep(0,length(ct)),future_pred))
      par(new=TRUE)
    }
    
    # ensuring compatibility
    preds <- matrix(preds, ncol = 1)
    future_pred <- matrix(future_pred, ncol = 1)
    
    # Concatenate
    preds <- rbind(preds, future_pred)
    
    pred_out[[i]] <- data.frame(it = i, fit = preds)
    best_model %>% reset_states() #reset the states 
  }
  
  pred_out <- do.call(rbind, pred_out)
  
  # Mean and confidence intervals 
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = pmax(quantile(fit, probs = 0.025, na.rm = TRUE), 0), # Ajusta para 0 se menor
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and Fitted data
  lstm_pred<-data.frame(
    yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
    ct=c(ct, rep(NA,forecast_yr)),
    pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
    lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
    up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p56 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p56
  
  ggplot2::ggsave("Lstm_fit_LTA.png",plot=p56, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_LTA<- best
  lstm_pred_LTA<- lstm_pred
  #-------------------------
  
  
  
  
  
  
  #==============
  #      WAH    #
  #==============
  ct<-WAH_catch$Freire 
  yr<-WAH_catch$Year
  
  
  #vizual inspection of catches
  p57 <- ggplot() +
    geom_line(aes(x = yr, y = ct), linewidth=2,alpha = 0.5) +  
    #facet_wrap(.~species, scales = "free") +
    labs(x = "Year", y = "Catch (t)", fill = "", colour = "", caption = "AT-SW Stock") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) %+replace% 
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
          legend.position = "right")
  p57
  
  ggplot2::ggsave("Catches_WAH.png",plot=p57, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #standardizing data to the activation function range (LSTM= hyperbolic tangent and Sigmoid)
  msd.ct = c(mean(ct), sd(ct))
  ct_scaled= (ct - msd.ct[1])/msd.ct[2]
  
  #---------------------------- LSTM parameters -------------------------------------#
  look_back <- c(2,3,5,10) #how many lagged series the model will be looking (0,2,3,5,10) **lag 0 overfit the data**
  batch_size <- c(4,8,16,32) #samples for batch processing (4,8,16,32)
  epochs<- c(200) #epochs of training| fixed to avoid some architectures to over or underfit
  units<- c(10,50,100) #number of lstm units in each layer
  dropout<- 0.3   #fixed to avoid under or overfitting for some architectures 
  optimizer = c('adam','rmsprop') #Adam- Adaptive Moment Estimation a
  loss = 'mean_squared_error'  #Loss function in the fitting process
  val_loss= "val_mean_absolute_error" #metric for validation
  metrics = 'mean_absolute_error' #metric of evaluation
  val_split= 0.3      #saving % of data for validation process #avoid overfitting
  pattience= 25     #allowing n epochs without improvement (early stopping)
  
  #----------------------
  if(run_lstm_loop==TRUE) { #run or not the tuning models loop
    #----------------------
    
    #--- parallel processing parameters ---#
    library(foreach)
    library(doParallel)
    # Number of cores 
    num_cores <- floor(0.8 * detectCores()) # take 80% of capacity
    cl <- makeCluster(num_cores)  # cluster
    registerDoParallel(cl)        # register backend
    
    # Hyperparameters list 
    hyperparameter_combinations <- expand.grid(
      look_back = look_back,
      batch_size = batch_size,
      epochs = epochs,
      units = units,
      dropout = dropout,
      optimizer = optimizer
    )
    
    #=============================
    #LSTM via Parallel Processing
    #=============================
    library(tensorflow)
    library(keras)
    library(foreach)
    library(doParallel)
    
    lstm_models <- foreach(
      param = iter(hyperparameter_combinations, by = 'row'), .combine = rbind, 
      .packages = c("keras", "tensorflow")) %dopar% {
        
        #parameters iteration
        i <- param$look_back
        j <- param$batch_size
        k <- param$epochs
        l <- param$units
        z <- param$dropout
        c <- param$optimizer
        
        #early stopping parameters
        callback <- callback_early_stopping(
          monitor = "val_mean_absolute_error", #validation monitor
          patience = pattience,               # Allowing n epochs without improvement
          restore_best_weights = TRUE  # keeping the best model
        )
        
        # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
        if (i==0) { #no sliding windows require different training data sets
          x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
          y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
          input_shape = c(2, 1)
        } else { #sliding windows (lagged time series as training data sets)
          x <- array(embed(ct_scaled, i + 1)[, -1], dim = c(length(ct_scaled) - i, i, 1))
          y <- array(ct_scaled[(i + 1):length(ct_scaled)], dim = c(length(ct_scaled) - i, 1)) 
          input_shape = c(i, 1)
        }
        
        # Function to create the architecture, fit and evaluate the models
        train_and_evaluate <- function(model_architecture) {
          model <- model_architecture %>% 
            compile(loss = loss, optimizer = c, metrics = metrics)
          
          #fit ( early stopping, validation split and no shuffle)
          model %>% fit(x = x, y = y,
                        validation_split = val_split,
                        batch_size = j, 
                        epochs = k, 
                        verbose = 0, 
                        shuffle = FALSE,
                        callbacks = list(callback))
          
          metrics <- model %>% evaluate(x, y, batch_size = j)
          preds <- model %>% predict(x, batch_size = j) %>% .[, 1]
          train_size<- length(x[,,1][,1])
          
          # Coverting to the same size (sliding windows )
          if (i==0) {
            aligned_ct<- ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred<- preds * msd.ct[2] + msd.ct[1]  
            
          } else {
            aligned_ct_scaled <- ct_scaled[(abs(train_size-length(ct_scaled))+1):length(ct_scaled)]
            #converting back to the original scale
            aligned_ct <- aligned_ct_scaled * msd.ct[2] + msd.ct[1]
            aligned_pred <- preds * msd.ct[2] + msd.ct[1]  
          }
          
          # Residuals
          residuals <- aligned_ct - aligned_pred
          # RMSE metric
          rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
          # R² metric
          ss_res <- sum(residuals^2, na.rm = TRUE)
          ss_tot <- sum((aligned_ct - mean(aligned_ct, na.rm = TRUE))^2, na.rm = TRUE)
          r_squared <- 1 - (ss_res / ss_tot)
          
          #list of results
          list(mae = unname(metrics[2]), rmse = rmse, r2 = r_squared)
        }
        
        # fitting the models
        results_single <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_multi <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        results_deep <- train_and_evaluate(
          keras_model_sequential() %>%
            layer_lstm(units = l, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = TRUE) %>%
            layer_dropout(rate = z) %>%
            layer_lstm(units = l, return_sequences = FALSE) %>%
            layer_dropout(rate = z) %>%
            layer_dense(units = 1)
        )
        
        # Combining results
        data.frame(
          model = c("single", "multi", "deep"),
          mae = c(results_single$mae, results_multi$mae, results_deep$mae),
          rmse = c(results_single$rmse, results_multi$rmse, results_deep$rmse),
          r2 = c(results_single$r2, results_multi$r2, results_deep$r2),
          look_back = i,
          batch_size = j,
          epochs = k,
          units = l,
          dropout = z,
          optimizer = c
        )
      }
    
    # close cluster
    stopCluster(cl)
    
    # Results
    head(lstm_models)
    
    # R2 x Look_back
    p58 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p59 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p60 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p61 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p62 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p63<-grid.arrange(p58, p59, p60, p61, p62, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_WAH.png",plot=p63, device = "png", units = "cm",
                    width = 28, height = 16)
    
    #best model
    best<-lstm_models[which.max(lstm_models$r2),]
    print(best)
  }
  
  #========================================================
  # fitting iteration process to forecast data (best model)
  #========================================================
  dev.off()
  if (run_lstm_loop==FALSE) {
    best<- data.frame(model="single", 
                      mae=0.39,     
                      rmse=203.94,
                      r2=0.30,
                      look_back=5,
                      batch_size=8,
                      epochs= 200,
                      units= 100, 
                      dropout=0.3,
                      optimizer="adam")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 1 #number of years looking back to make the forecast
  sample_yr<- 7 #number of years to be sampled from the catch data
  plot_it<- FALSE #plot  each iteration
  
  forecast <- function(model, input_sequence, steps, window_size, batch_size) {
    predictions <- numeric(steps)
    current_sequence <- array(input_sequence, dim = c(1, window_size, 1))
    
    for (i in 1:steps) {
      # Realiza a previsão com a estrutura correta de array tridimensional
      prediction <- model %>% predict(current_sequence, batch_size = batch_size)
      predictions[i] <- prediction
      
      # Atualiza a sequência para a próxima previsão
      current_sequence <- array(c(current_sequence[1, -1, 1], prediction), dim = c(1, window_size, 1))
    }
    predictions
  }
  
  # Prepare 3-D data as windows (Sliding windows to slice the data in look_back portions)
  if (best$look_back==0) { #no sliding windows require different training data sets
    x <- array(ct_scaled, dim = c(length(ct_scaled), 2, 1))
    y <- array(ct_scaled, dim = c(length(ct_scaled), 1))
    input_shape = c(2, 1)
    
  } else { #sliding windows (lagged time series as training data sets)
    x <- array(embed(ct_scaled, best$look_back + 1)[, -1], dim = c(length(ct_scaled) - best$look_back, best$look_back, 1))
    y <- array(ct_scaled[(best$look_back + 1):length(ct_scaled)], dim = c(length(ct_scaled) - best$look_back, 1))
    input_shape = c(best$look_back, 1)
  }
  
  #early stopping parameters
  callback <- callback_early_stopping(
    monitor = "val_mean_absolute_error", #validation monitor
    patience = pattience,               # Allowing n epochs without improvement
    restore_best_weights = TRUE  # keeping the best model
  )
  
  #fit ( early stopping, validation split and no shuffle)
  for(i in 1:nit) {
    
    if (best$model=="single") { #assigning models
      
      best_model<- keras_model_sequential() %>%
        layer_lstm(units = best$units, input_shape = input_shape) %>%
        layer_dropout(rate = best$dropout) %>%
        layer_dense(units = 1)} else if (
          best$model=="multi") {
          
          best_model<- keras_model_sequential() %>%
            layer_lstm(units = best$units, input_shape = input_shape, return_sequences = TRUE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_lstm(units = best$units, return_sequences = FALSE) %>%
            layer_dropout(rate = best$dropout) %>%
            layer_dense(units = 1)} else if (
              best$model=="deep") {
              
              best_model<- keras_model_sequential() %>%
                layer_lstm(units = best$units, input_shape = input_shape,return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = TRUE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_lstm(units = best$units, return_sequences = FALSE) %>%
                layer_dropout(rate = best$dropout) %>%
                layer_dense(units = 1)
            }
    
    #compiling
    best_model %>% 
      compile(loss = loss, optimizer = best$optimizer, metrics = metrics)
    #fiting the models
    best_model %>% fit(x = x, y = y,
                       validation_split = val_split,
                       batch_size = best$batch_size, 
                       epochs = best$epochs, 
                       verbose = 0, 
                       shuffle = FALSE,
                       callbacks = list(callback))
    #predicting
    preds <- best_model %>% predict(x, batch_size = best$batch_size) %>% .[, 1] 
    preds <- preds * msd.ct[2] + msd.ct[1]
    noise_sd <- 0.1 * mean(preds, na.rm = TRUE)  
    preds <- preds + rnorm(length(preds), mean = 0, sd = noise_sd)
    
    #future predicion
    future_pred <- forecast(best_model, tail(x, sample_yr), forecast_yr, window_size , best$batch_size)
    future_pred <- future_pred * msd.ct[2] + msd.ct[1]
    future_pred <- future_pred + rnorm(length(future_pred), mean = 0, sd = noise_sd)
    
    if (plot_it==TRUE) {
      
      plot(ct,type="l",col="blue",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      par(new=TRUE)
      plot(c(rep(NA,best$look_back),preds),type="l",col="red",xlim=c(1,sum(length(ct)+length(future_pred))), ylim = c(0,max(c(future_pred,ct))))
      lines(c(rep(0,length(ct)),future_pred))
      par(new=TRUE)
    }
    
    # ensuring compatibility
    preds <- matrix(preds, ncol = 1)
    future_pred <- matrix(future_pred, ncol = 1)
    
    # Concatenate
    preds <- rbind(preds, future_pred)
    
    pred_out[[i]] <- data.frame(it = i, fit = preds)
    best_model %>% reset_states() #reset the states 
  }
  
  pred_out <- do.call(rbind, pred_out)
  
  # Mean and confidence intervals 
  summary_pred <- pred_out %>%
    group_by(idx = rep(1:(nrow(pred_out) / nit), nit)) %>%
    summarise(
      mean_fit = pmax(mean(fit, na.rm = TRUE), 0),
      lower_ci = pmax(quantile(fit, probs = 0.025, na.rm = TRUE), 0), # Ajusta para 0 se menor
      upper_ci = quantile(fit, probs = 0.975, na.rm = TRUE)
    )
  
  # Observed and Fitted data
  lstm_pred<-data.frame(
    yr= c(yr, (yr[length(yr)]+1):(yr[length(yr)]+forecast_yr)), 
    ct=c(ct, rep(NA,forecast_yr)),
    pred= c(rep(NA,best$look_back), summary_pred$mean_fit),
    lw= c(rep(NA,best$look_back), summary_pred$lower_ci),
    up= c(rep(NA,best$look_back), summary_pred$upper_ci))
  
  
  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  library(ggplot2)
  
  p64 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p64
  
  ggplot2::ggsave("Lstm_fit_WAH.png",plot=p64, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_WAH<- best
  lstm_pred_WAH<- lstm_pred
  #-------------------------
  
  
  #===================================================================================================================
                            #=============================================#
                            #  Writing the final models  and predictions  #
                            #=============================================#
  
  #best models----
  best_lstm_models<- rbind(best_BLF,best_BRS,best_DOL,best_FRI,best_KGM,best_LTA,best_WAH)
  best_lstm_models$codsp<- sp
  
  # Writing in a csv file
  write.table(
    x = best_lstm_models,                   
    file = "best_lstm_models.csv",          
    append = FALSE,                         
    dec = ".",                              
    sep = ",",                              
    row.names = FALSE,                      
    col.names = TRUE                        
  )
  
  #lstm predictions
  lstm_pred<- rbind(lstm_pred_BLF,lstm_pred_BRS,lstm_pred_DOL,lstm_pred_FRI,lstm_pred_KGM,lstm_pred_LTA,lstm_pred_WAH)
  lstm_pred$codsp <- rep(sp, times = sapply(list(
    lstm_pred_BLF, lstm_pred_BRS, lstm_pred_DOL,
    lstm_pred_FRI, lstm_pred_KGM, lstm_pred_LTA, lstm_pred_WAH
    ), nrow))
  
  # Writing in a csv file
  write.table(
    x = lstm_pred,                   
    file = "lstm_predictions.csv",          
    append = FALSE,                         
    dec = ".",                              
    sep = ",",                              
    row.names = FALSE,                      
    col.names = TRUE                        
  )
  #====================================================================================================================
  
  
  
  
  
  
  
  #--------------------------------------------------------------------------------
  # 1- Catch data file (Freire et al., 2021)- Reconstructed catches+ LSTM forecast
  #--------------------------------------------------------------------------------

  # future predictions
  future_data <- lstm_pred %>%
    filter(yr >= 2015) %>% 
    mutate(type = "Forecast")
  
  # observed data
  observed_data <- lstm_pred %>%
    filter(yr <= 2015) %>%
    mutate(type = "Fit")
  
  # Combining 
  plot_data <- bind_rows(observed_data, future_data)
  
  p65 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    # model's prediction
    geom_line(aes(y = pred, color = type),linewidth=1.2) +
    geom_ribbon(data = observed_data, aes(ymin = lw, ymax = up), fill = "blue", alpha = 0.3) +
    # lstm forecast
    geom_line(data = future_data, aes(y = pred), color = "red",linewidth=1.2) +
    geom_ribbon(data = future_data, aes(ymin = lw, ymax = up), fill = "red", alpha = 0.3) +
    # vertical line indicating transition
    geom_vline(xintercept = 2015, linetype = "dashed") +
    facet_wrap(.~codsp, scales = "free_y")+
    labs(x = "Year", y = "Catch (t)",color="") +
    scale_color_manual(values = c("Fit" = "blue", "Forecast" = "red")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p65
  
  ggplot2::ggsave("Lstm_fit_ALLSP.png",plot=p65, device = "png", units = "cm",
                                                        width = 28, height = 16)
  
  #catch series + LSTM predictions (Projections)
  BLF_catch<- data.frame(Year= c(smtct$year[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="BLF"] [67:length(lstm_pred$yr[lstm_pred$codsp=="BLF"])] ), 
                         Freire= c(smtct$catch[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="BLF"] [67:length(lstm_pred$pred[lstm_pred$codsp=="BLF"])]))
  # Writing in a csv file
  write.table(
    x = BLF_catch,                   
    file = "BLF_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  BRS_catch<- data.frame(Year= c(smtct$year[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="BRS"] [67:length(lstm_pred$yr[lstm_pred$codsp=="BRS"])] ), 
                         Freire= c(smtct$catch[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="BRS"] [67:length(lstm_pred$pred[lstm_pred$codsp=="BRS"])]))
  # Writing in a csv file
  write.table(
    x = BRS_catch,                   
    file = "BRS_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  DOL_catch<- data.frame(Year= c(smtct$year[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="DOL"] [67:length(lstm_pred$yr[lstm_pred$codsp=="DOL"])] ), 
                         Freire= c(smtct$catch[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="DOL"] [67:length(lstm_pred$pred[lstm_pred$codsp=="DOL"])]))
  # Writing in a csv file
  write.table(
    x = DOL_catch,                   
    file = "DOL_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  FRI_catch<- data.frame(Year= c(smtct$year[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="FRI"] [67:length(lstm_pred$yr[lstm_pred$codsp=="FRI"])] ), 
                         Freire= c(smtct$catch[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="FRI"] [67:length(lstm_pred$pred[lstm_pred$codsp=="FRI"])]))
  # Writing in a csv file
  write.table(
    x = FRI_catch,                   
    file = "FRI_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  KGM_catch<- data.frame(Year= c(smtct$year[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="KGM"] [67:length(lstm_pred$yr[lstm_pred$codsp=="KGM"])] ), 
                         Freire= c(smtct$catch[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="KGM"] [67:length(lstm_pred$pred[lstm_pred$codsp=="KGM"])]))
  # Writing in a csv file
  write.table(
    x = KGM_catch,                   
    file = "KGM_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  LTA_catch<- data.frame(Year= c(smtct$year[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="LTA"] [67:length(lstm_pred$yr[lstm_pred$codsp=="LTA"])] ), 
                         Freire= c(smtct$catch[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="LTA"] [67:length(lstm_pred$pred[lstm_pred$codsp=="LTA"])]))
  # Writing in a csv file
  write.table(
    x = LTA_catch,                   
    file = "LTA_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  WAH_catch<- data.frame(Year= c(smtct$year[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="WAH"] [67:length(lstm_pred$yr[lstm_pred$codsp=="WAH"])] ), 
                         Freire= c(smtct$catch[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="WAH"] [67:length(lstm_pred$pred[lstm_pred$codsp=="WAH"])]))
  # Writing in a csv file
  write.table(
    x = WAH_catch,                   
    file = "WAH_catch.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  
  
  #-------------------------------------------------------------------#
  # 2- Length data file                                               # 
  #     1-  Observed Lengths (More reliable)                          #
  #     2- Reconstructed lengths when the observed mean was available #
  #     3- All reconstructed lengths to fit the model                 #
  #     4- changing selectivity of the gears                          #
  #-------------------------------------------------------------------#
  

  
#-----------------------------------------------
#1-  Observed Lengths (More reliable)
# Loop to create catch and length spread sheets
#-----------------------------------------------
#vector of stocks
sp
bin= 5
lobs_out<- data.frame(codsp=NULL,Year=NULL,Month=NULL,Fleet=NULL,Sex=NULL,Nsamps=NULL,Length=NULL,Count=NULL)

for (j in sp) {
  
  # Adjusting the bins for specific species
  if (j=="KGM") {
    bin <- 8
  } else if (j=="FRI") {
    bin <- 3
  } else if (j=="WAH") {
    bin <- 10
  } else if (j %in% c("DOL", "WAH")) {
    bin <- 9
  } else if (j %in% c("BRS","BLF")) {
    bin <- 6
  } else {
    bin <- 5  #Keep 5 for the others
  }
  
  #------------------------------
  # Observed Length spread sheet
  #------------------------------
  if (any(j %in% unique(smtlenobs$codsp))==TRUE) {
    
    #getting size classes and counts
    szloop=data.frame(Stock=NULL,Year=NULL,Length=NULL,CatchNo=NULL,Nsamps=NULL)
    years=unique(smtlenobs$yr[smtlenobs$codsp==j]) #Years vector for length data
    
    for (i in years) {
      h=hist(smtlenobs$fl[smtlenobs$yr==i & smtlenobs$codsp==j],
             breaks=seq(from=min(smtlenobs$fl[smtlenobs$codsp==j])-bin,
                        to=max(smtlenobs$fl[smtlenobs$codsp==j])+bin, by=bin), #bin width
             xlab = 'Fork length (cm)',
             main = paste(j))
      
      freq=data.frame(Stock=j, #name of stock
                      Year=i,  #put year
                      Length=round(h$mids), #mid lengths
                      CatchNo=h$counts,  #counts for each mid length
                      Nsamps= sum(h$counts)) #number of sampling individuals
      szloop=rbind(szloop,freq)
    }
    
    #make data frame for Stock synthesis
    lobsdat=data.frame( codsp= j,
                        Year=szloop$Year[szloop$Stock==j],
                        Month=7,
                        Fleet=1,
                        Sex=0,
                        Nsamps= szloop$Nsamps[szloop$Stock==j],
                        Length=szloop$Length[szloop$Stock==j],
                        Count=szloop$CatchNo[szloop$Stock==j])
    lobs_out<- rbind(lobs_out,lobsdat)
  }
 }   
  
#-----------------------------------------------
#Filtering inconsistent data (Observed lengths)
#-----------------------------------------------
#BLF
#Filtering Discrepant length data and sample size
BLF_length_obs <- lobs_out %>%
                  dplyr::filter(codsp=="BLF",
                                Length<100,
                                Year!=2017) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
BLF_length_obs<- tidyr::spread(BLF_length_obs, Length,Count)
#BRS
#Filtering Discrepant length data and sample size
BRS_length_obs <- lobs_out %>%
                  dplyr::filter(codsp=="BRS",
                                Length<105) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
BRS_length_obs<- tidyr::spread(BRS_length_obs, Length,Count)
#DOL
#Filtering Discrepant length data and sample size
DOL_length_obs <- lobs_out %>%
                 dplyr::filter(codsp == "DOL",
                         Length>30 & Length<160,       
                        !(Year %in% c(2002, 2003, 2004,2013,2014,2015,2020))) %>%
                 dplyr::select(-codsp)
#Stock Synthesis format
DOL_length_obs<- tidyr::spread(DOL_length_obs, Length,Count)
#FRI
#Filtering Discrepant length data and sample size
FRI_length_obs <- lobs_out %>%
                  dplyr::filter(codsp == "FRI",
                                Length<60,
                                !(Year %in% c(1991,1995,1997,2006))) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
FRI_length_obs<- tidyr::spread(FRI_length_obs, Length,Count)
#KGM
#Filtering Discrepant length data and sample size
KGM_length_obs <- lobs_out %>%
                  dplyr::filter(codsp == "KGM",
                                Length<170,
                               !(Year %in% c(2019,2020))) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
KGM_length_obs<- tidyr::spread(KGM_length_obs, Length,Count)
#LTA
#Filtering Discrepant length data and sample size
LTA_length_obs <- lobs_out %>%
                  dplyr::filter(codsp == "LTA",
                                #Length<60,
                               !(Year %in% c(2008,2015))) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
LTA_length_obs<- tidyr::spread(LTA_length_obs, Length,Count)
LTA_length_obs[1,15:ncol(LTA_length_obs)]<-0
#WAH
#Filtering Discrepant length data and sample size
WAH_length_obs <- lobs_out %>%
                 dplyr::filter(codsp == "WAH",
                               Length<200,
                               !(Year %in% c(1986,1987,1999,2000,2002,2003,2004,2005,2008,2009,2010,2013,2014,2015,2016))) %>%
                 dplyr::select(-codsp)
#Stock Synthesis format
WAH_length_obs<- tidyr::spread(WAH_length_obs, Length,Count)

# Writing in a csv file
write.table(
  x = BLF_length_obs,                   
  file = "BLF_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = BRS_length_obs,                   
  file = "BRS_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = DOL_length_obs,                   
  file = "DOL_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = FRI_length_obs,                   
  file = "FRI_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
#Writing in a csv file
write.table(
  x = KGM_length_obs,                   
  file = "KGM_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = LTA_length_obs,                   
  file = "LTA_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = WAH_length_obs,                   
  file = "WAH_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)


#--------------------------------------------------------------#
#2- Reconstructed lengths when the observed mean was available #
# Loop to create the data  
#--------------------------------------------------------------#

#vector of stocks
sp
bin= 5
lobspred_out<- data.frame(codsp=NULL,Year=NULL,Month=NULL,Fleet=NULL,Sex=NULL,Nsamps=NULL,Length=NULL,Count=NULL)

  for (j in sp) {
    
    # Adjusting the bins for specific species
    if (j=="KGM") {
      bin <- 8
    } else if (j=="FRI") {
      bin <- 3
    } else if (j=="WAH") {
      bin <- 10
    } else if (j %in% c("DOL", "WAH")) {
      bin <- 9
    } else if (j %in% c("BRS","BLF")) {
      bin <- 6
    } else {
      bin <- 5  #Keep 5 for the others
    }
    
    #empty frame for the loop
    szloop=data.frame(Stock=NULL,Year=NULL,Length=NULL,CatchNo=NULL,Nsamps=NULL)
    
    #years of reconstructed length data tied to a observed mean length
    years=as.vector(unique(smtml$yr[is.na(smtml$mean)==FALSE & smtml$codsp==j]))
    years= years[years!=1950] #removing the 1950 estimate

    #reconstructed length data in the years tied to a mean length
    lrec<- smtlensim %>%
        dplyr::filter(yr %in% c(years), 
                codsp==j)

      for (i in years) {
         h=hist(smtlensim$fl[smtlensim$yr==i & smtlensim$codsp==j],
         breaks=seq(from=min(smtlensim$fl[smtlensim$codsp==j])-bin,
                    to=max(smtlensim$fl[smtlensim$codsp==j])+bin, by=bin), #binwidth
         xlab = 'Fork length (cm)',
         main = paste(j))
          
         freq=data.frame(Stock=j,
                  Year=i,
                  Length=round(h$mids),
                  CatchNo=h$counts,
                  Nsamps= sum(h$counts))    
        szloop=rbind(szloop,freq)
      }

    #make data frame for Stock synthesis
    lobspred=data.frame(codsp= j,
                        Year=szloop$Year[szloop$Stock==j],
                        Month=7,
                        Fleet=1,
                        Sex=0,
                        Nsamps= szloop$Nsamps[szloop$Stock==j],
                        Length=szloop$Length[szloop$Stock==j],
                        Count=szloop$CatchNo[szloop$Stock==j])
    lobspred_out<- rbind(lobspred_out,lobspred)
}    

#-----------------------------------------------------------------------------------------
#Filtering inconsistent data (Reconstructed lengths when the observed mean was available )
#-----------------------------------------------------------------------------------------
#BLF
#Filtering Discrepant length data 
BLF_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp=="BLF") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
BLF_length_obspred<- tidyr::spread(BLF_length_obspred, Length,Count)
#BRS
#Filtering Discrepant length data and sample size
BRS_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp=="BRS") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
BRS_length_obspred<- tidyr::spread(BRS_length_obspred, Length,Count)
#DOL
#Filtering Discrepant length data and sample size
DOL_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "DOL",
                Length<200) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
DOL_length_obspred<- tidyr::spread(DOL_length_obspred, Length,Count)
#FRI
#Filtering Discrepant length data and sample size
FRI_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "FRI",
                Length<62) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
FRI_length_obspred<- tidyr::spread(FRI_length_obspred, Length,Count)
#KGM
#Filtering Discrepant length data and sample size
KGM_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "KGM",
                Length<130) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
KGM_length_obspred<- tidyr::spread(KGM_length_obspred, Length,Count)
#LTA
#Filtering Discrepant length data and sample size
LTA_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "LTA") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
LTA_length_obspred<- tidyr::spread(LTA_length_obspred, Length,Count)
#WAH
#Filtering Discrepant length data and sample size
WAH_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "WAH",
                Length<200) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
WAH_length_obspred<- tidyr::spread(WAH_length_obspred, Length,Count)

# Writing in a csv file
write.table(
  x = BLF_length_obspred,                   
  file = "BLF_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = BRS_length_obspred,                   
  file = "BRS_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = DOL_length_obspred,                   
  file = "DOL_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = FRI_length_obspred,                   
  file = "FRI_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
#Writing in a csv file
write.table(
  x = KGM_length_obspred,                   
  file = "KGM_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = LTA_length_obspred,                   
  file = "LTA_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = WAH_length_obspred,                   
  file = "WAH_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
#------------------------------------------------------------------


#-------------------------------------------------------------------
#3- All reconstructed data lengths to fit the stock synthesis model 
#-------------------------------------------------------------------

#vector of stocks
sp
bin= 5
lpred_out<- data.frame(codsp=NULL,Year=NULL,Month=NULL,Fleet=NULL,Sex=NULL,Nsamps=NULL,Length=NULL,Count=NULL)

for (j in sp) {
  # Adjusting the bins for specific species
  if (j=="KGM") {
    bin <- 8
  } else if (j=="FRI") {
    bin <- 3
  } else if (j=="WAH") {
    bin <- 10
  } else if (j %in% c("DOL", "WAH")) {
    bin <- 9
  } else if (j %in% c("BRS","BLF")) {
    bin <- 6
  } else {
    bin <- 5  #Keep 5 for the others
  }
  
  #empty frame for the loop
  szloop=data.frame(Stock=NULL,Year=NULL,Length=NULL,CatchNo=NULL,Nsamps=NULL)
  
  #years of reconstructed length data tied to a observed mean length
  years=as.vector(unique(smtlensim$yr[smtlensim$codsp==j]))
  #years= years[years!=1950] #removing the 1950 estimate
  
  for (i in years) {
    h=hist(smtlensim$fl[smtlensim$yr==i & smtlensim$codsp==j],
           breaks=seq(from=min(smtlensim$fl[smtlensim$codsp==j])-bin,
                      to=max(smtlensim$fl[smtlensim$codsp==j])+bin, by=bin), #binwidth
           xlab = 'Fork length (cm)',
           main = paste(j))
    
    freq=data.frame(Stock=j,
                    Year=i,
                    Length=round(h$mids),
                    CatchNo=h$counts,
                    Nsamps= sum(h$counts))    
    szloop=rbind(szloop,freq)
  }
  
  #make data frame for Stock synthesis
  lpred=data.frame(codsp= j,
                      Year=szloop$Year[szloop$Stock==j],
                      Month=7,
                      Fleet=1,
                      Sex=0,
                      Nsamps= szloop$Nsamps[szloop$Stock==j],
                      Length=szloop$Length[szloop$Stock==j],
                      Count=szloop$CatchNo[szloop$Stock==j])
  lpred_out<- rbind(lpred_out,lpred)
}    


#-------------------------------------------------------
#Filtering inconsistent data (Reconstructed lengths only
#-------------------------------------------------------
#BLF
#Filtering Discrepant length data 
BLF_length_pred <- lpred_out %>%
  dplyr::filter(codsp=="BLF") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
BLF_length_pred<- tidyr::spread(BLF_length_pred, Length,Count)
#BRS
#Filtering Discrepant length data and sample size
BRS_length_pred <- lpred_out %>%
  dplyr::filter(codsp=="BRS") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
BRS_length_pred<- tidyr::spread(BRS_length_pred, Length,Count)
#DOL
#Filtering Discrepant length data and sample size
DOL_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "DOL",
                Length<200) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
DOL_length_pred<- tidyr::spread(DOL_length_pred, Length,Count)
#FRI
#Filtering Discrepant length data and sample size
FRI_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "FRI",
                Length<66) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
FRI_length_pred<- tidyr::spread(FRI_length_pred, Length,Count)
#KGM
#Filtering Discrepant length data and sample size
KGM_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "KGM",
                Length<130) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
KGM_length_pred<- tidyr::spread(KGM_length_pred, Length,Count)
#LTA
#Filtering Discrepant length data and sample size
LTA_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "LTA") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
LTA_length_pred<- tidyr::spread(LTA_length_pred, Length,Count)
#WAH
#Filtering Discrepant length data and sample size
WAH_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "WAH",
                Length<200) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
WAH_length_pred<- tidyr::spread(WAH_length_pred, Length,Count)

# Writing in a csv file
write.table(
  x = BLF_length_pred,                   
  file = "BLF_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = BRS_length_pred,                   
  file = "BRS_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = DOL_length_pred,                   
  file = "DOL_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = FRI_length_pred,                   
  file = "FRI_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
#Writing in a csv file
write.table(
  x = KGM_length_pred,                   
  file = "KGM_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = LTA_length_pred,                   
  file = "LTA_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
# Writing in a csv file
write.table(
  x = WAH_length_pred,                   
  file = "WAH_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
#------------------------------------------------------------------


                        #@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@##
                        #               Stock Assessment step using Stock Synthesis (SS3)                    #
                        #             Methot, R.D. and Wetzel, C.R. (2013). Stock Synthesis:                 #
                        #                https://doi.org/10.1016/j.fishres.2012.10.012                       #
                        #   Approaches: Jitter exploration; Sensitivity Analysis, Likelihood profiles;       #
                        # Ensembling approaches and different selectivity shapes (Logistic and Dome-shaped)  #
                        ##@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X@#
  
  #@X@#@X@#@X@#@X@#@X@#@X@#@X@#@X#@X       Stock Synthesis (SS3)   #@X@##@X@#@X@#@X@#@X@#@X@#@X@#@X#@X@#@X@#@X@#
  #Control options
  library(r4ss)
  library(parallel)
  library(dplyr)
  library(ggplot2)
  library(lmerTest)
  library(splines)
  setwd("C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas")#directory
  dir=getwd()
  dir_input= "C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas/SS3/input"#SS3 input files
  run_jitter=TRUE  #If TRUE runs a jitter searching for better likelihood convergence of starting values
  type_jitter=1 #(1-Multivariate jitter function 2-Univariate jitter combinations 3-Multivariate expanded grid jitter(takes more time)
  n_jitter=3 #length vector of each parameter (Jitter type 2 and 3)
  n_scenarios= 100 #only for jitter TYPE 1 (number of multivariate jitter scenarios)
  per_jitter=0.05 #percentage of variation of each parameter
  #sp<-"WAH" #run some species separately
  run_sensitivity= TRUE #if TRUE runs a sensitivity analysis evaluating impact of length types and life history
  n_sensitivity= 33    #number of scenarios for each length type (3 types*33= 99 per species)
  per_sensitivity=0.05 #percentage of variation of each parameter
  run_ensemble=TRUE  #If TRUE runs an ensemble of multiple models weighting the best source of data
  n_ensemble= 10 #number of information of each normal distribution
  obs_weight=1  #weigth for the observed lengths
  obspred_weight=2 #weights for the observed+pred lengths
  pred_weight=1 #weights for the predicted lengths 
  sp<- c("BLF", "BRS", "DOL", "FRI", "KGM", "LTA","WAH")  #species to be analyzes 
  
  #taking the average and intervals of biological parameters for the SS analysis
  #biological parameters for all species
  par= read.csv(file.path(dir, "smt_life_history_filtered.csv"),sep = ",",dec = ".")  #biological parameters
  par_ss <- par %>% 
  dplyr::select(codsp, m, m_algaraja, m_pauly, tmax, linf, k, t0, lm50, sl95, wl_a, wl_b, h) %>%
  dplyr::mutate(lmslope = -0.55, ascendse = 3.7, lnr0 = 7, sigmar0 = 0.2) %>% 
  #dplyr::group_by(codsp) %>%
  #dplyr::summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>% #mean of all parameters
  dplyr::mutate(across(everything(), \(x) ifelse(is.nan(x), NA, x))) %>%
  dplyr::mutate(across(c(m, m_algaraja, m_pauly, linf, k, t0, lm50, sl95, wl_b, h), \(x) round(x, 2))) %>%
  dplyr::mutate(across(c(tmax), \(x) round(x)))
  
  #BLF parameters filtered **better convergence
  par_ss$m[par_ss$codsp == "BLF"][2] <- 0.8
  par_ss$linf[par_ss$codsp == "BLF"][3] <- 75
  par_ss$k[par_ss$codsp == "BLF" ][3] <- 0.5
  par_ss$tmax[par_ss$codsp == "BLF"][1] <-5 
  par_ss$sl95[par_ss$codsp=="BLF"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="BLF"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "BLF" ],na.rm = TRUE)*1.1)
  par_ss$ascendse[par_ss$codsp == "BLF" ][1:2] <- c(3.2,4.7)
  par_ss$lnr0[par_ss$codsp == "BLF" ][1:2] <- c(6.5,7.5)
  
  #BRS parameters filtered **better convergence
  par_ss$m[par_ss$codsp == "BRS"][2] <- 0.8
  par_ss$linf[par_ss$codsp == "BRS"][3] <- 90
  par_ss$linf[par_ss$codsp == "BRS" & par_ss$linf < 90] <- NA
  par_ss$k[par_ss$codsp == "BRS" & par_ss$k >0.25] <- NA
  par_ss$tmax[par_ss$codsp == "BRS" & par_ss$tmax <=6] <- NA 
  par_ss$sl95[par_ss$codsp=="BRS"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="BRS"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "BRS" ],na.rm = TRUE)*1.1)
  par_ss$ascendse[par_ss$codsp == "BRS" ][1:2] <- c(3.2,4.7)
  par_ss$lnr0[par_ss$codsp == "BRS" ][1:2] <- c(6.5,7.5)
  
  # #DOL parameters filtered **better convergence
  par_ss$linf[par_ss$codsp == "DOL" & par_ss$linf <= 100] <- NA
  #par_ss$linf[par_ss$codsp == "DOL"] <- seq(115,120,length.out=length(par_ss$linf[par_ss$codsp == "DOL" ]))
  par_ss$k[par_ss$codsp == "DOL" & (par_ss$k < 1 | par_ss$k > 2.5)] <- NA
  # #par_ss$k[par_ss$codsp == "DOL" ] <- seq(0.9,1.1,length.out=length(par_ss$k[par_ss$codsp == "DOL" ]))
  par_ss$tmax[par_ss$codsp == "DOL" & par_ss$tmax <= 2] <- NA
  par_ss$m[par_ss$codsp == "DOL" & (par_ss$m < 0.7 | par_ss$m > 2)] <- NA
  par_ss$m_algaraja[par_ss$codsp == "DOL" & (par_ss$m_algaraja < 0.7 | par_ss$m_algaraja > 2)] <- NA
  par_ss$m_pauly[par_ss$codsp == "DOL" & (par_ss$m_pauly < 0.7 | par_ss$m_pauly > 2)] <- NA
  par_ss$sl95[par_ss$codsp=="DOL"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="DOL"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "DOL" ],na.rm = TRUE)*1.1)-10
  par_ss$ascendse[par_ss$codsp == "DOL" ][1:2] <- c(3.5,6)
  par_ss$lnr0[par_ss$codsp == "DOL" ][1:2] <- c(6.5,7.5)
  
  #FRI parameters filtered **better convergence
  #par_ss$m[par_ss$codsp == "FRI" & par_ss$m <= 0.7] <- NA
  #par_ss$m_algaraja[par_ss$codsp == "FRI" & par_ss$m_algaraja <= 0.7] <- NA
  #par_ss$m_pauly[par_ss$codsp == "FRI" & par_ss$m_pauly <= 0.7] <- NA
  par_ss$sl95[par_ss$codsp=="FRI"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="FRI"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "FRI" ],na.rm = TRUE)*1.2)
  par_ss$ascendse[par_ss$codsp == "FRI" ][1:2] <- c(3.5,6)
  par_ss$lnr0[par_ss$codsp == "FRI" ][1:2] <- c(6.5,7.5)
  
  #KGM parameters filtered **better convergence
  par_ss$k[par_ss$codsp == "KGM" & par_ss$k > 0.22 ] <- NA
  par_ss$linf[par_ss$codsp == "KGM" & par_ss$linf< 115] <- NA
  par_ss$sl95[par_ss$codsp=="KGM"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="KGM"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "KGM" ],na.rm = TRUE)*1.2)
  par_ss$ascendse[par_ss$codsp == "KGM" ][1:2] <- c(3.5,6)
  par_ss$lnr0[par_ss$codsp == "KGM" ][1:2] <- c(6.5,7.5)
  
  #LTA parameters filtered **better convergence
  par_ss$m[par_ss$codsp == "LTA" & par_ss$m <= 0.5] <- NA
  par_ss$m_algaraja[par_ss$codsp == "LTA" & par_ss$m_algaraja <= 0.5] <- NA
  par_ss$m_pauly[par_ss$codsp == "LTA" & par_ss$m_pauly <= 0.5] <- NA
  par_ss$k[par_ss$codsp == "LTA" & par_ss$k > 0.4 ] <- NA
  par_ss$linf[par_ss$codsp == "LTA" & par_ss$linf>110] <- NA
  par_ss$tmax[par_ss$codsp == "LTA"& par_ss$tmax<6]<- NA 
  par_ss$ascendse[par_ss$codsp == "LTA" ][1:2] <- c(3.2,4.7)
  par_ss$sl95[par_ss$codsp=="LTA"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="LTA"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "LTA" ],na.rm = TRUE)*1.2)
  par_ss$lnr0[par_ss$codsp == "LTA" ][1:2] <- c(6.5,7.5)
  
  #WAH parameters filtered **better convergence
  # par_ss$m[par_ss$codsp == "WAH" & par_ss$m > 0.5] <- NA
  # par_ss$m_algaraja[par_ss$codsp == "WAH" & par_ss$m_algaraja > 0.5] <- NA
  # par_ss$m_pauly[par_ss$codsp == "WAH" & par_ss$m_pauly > 0.5] <- NA
  par_ss$linf[par_ss$codsp == "WAH" & par_ss$linf < 170] <- NA
  par_ss$k[par_ss$codsp == "WAH" & par_ss$k >0.2] <- NA
  # par_ss$tmax[par_ss$codsp == "WAH" & par_ss$tmax <=6] <- NA 
  par_ss$sl95[par_ss$codsp=="WAH"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="WAH"],na.rm = TRUE)*0.95,mean(par_ss$sl95[par_ss$codsp == "WAH" ],na.rm = TRUE)*1.2)
  #par_ss$ascendse[par_ss$codsp == "WAH" ][1:2] <- c(3.2,4.7)
  # #par_ss$lnr0[par_ss$codsp == "WAH" ][1:2] <- c(6.5,7.5)
  par_ss$ascendse[par_ss$codsp == "WAH" ]<- seq(5,7,length.out=length(par_ss$ascendse[par_ss$codsp == "WAH" ]))
  par_ss$sl95[par_ss$codsp=="WAH"][1:2]<-c(mean(par_ss$sl95[par_ss$codsp=="WAH"],na.rm = TRUE)*0.9,mean(par_ss$sl95[par_ss$codsp == "WAH" ],na.rm = TRUE)*1.2)
  par_ss$lnr0[par_ss$codsp == "WAH" ][1:2] <- c(6.5,7.5)
  
  #writing the input life history table
  write.csv(par_ss, file = file.path(dir, "par_ss.csv"), row.names = FALSE)
  
  #------------------------- Jitter analysis Loop through all species using Stock Synthesis (SS3)---------------------#
  if (run_jitter == TRUE) {
  library(parallel)
  
  #------------ creating a grid of parameters ---------------#
  #1- multivariate jitter using jitter funcion to create the grids (limited by biological parameters min-max)
  if (type_jitter == 1) {
  per_jitter_modif<-c(m=1.5*per_jitter,linf=200*per_jitter,k=3*per_jitter,
                               sl95=100*per_jitter,ascendse=6*per_jitter,lnr0=6*per_jitter,tmax=30*per_jitter)
  #creating grids
  generate_grids <- function(base_grid, n, jitter_amounts, param_limits) {
  grids <- list(base_grid)
  for (i in 1:(n-1)) {
  novo_grid <- base_grid %>%  
  dplyr::mutate(
  dplyr::across(
  names(jitter_amounts),~ {
  #Jitter function limiting the min-max of parameters
  jittered <- base::jitter(.x, amount = jitter_amounts[dplyr::cur_column()])
  pmax(   pmin( jittered, param_limits[[dplyr::cur_column()]][2] ), #max lim
  param_limits[[dplyr::cur_column()]][1]) } ),  #min lim
  tmax = round(tmax), jittering_par = "all" )
  grids[[i+1]] <- novo_grid }
  return(grids)
  }
  #Loop by species with limits
  param_grid <- data.frame()
  for (species in sp) {
  par_species <- par_ss %>% dplyr::filter(codsp == species)
  #Limits for each parameter
  param_limits <- list(
  m = c(
  min(c(par_species$m, par_species$m_algaraja, par_species$m_pauly), na.rm = TRUE),
  max(c(par_species$m, par_species$m_algaraja, par_species$m_pauly), na.rm = TRUE)),
  linf = c(min(par_species$linf, na.rm = TRUE),max(par_species$linf, na.rm = TRUE)),
  k = c(min(par_species$k, na.rm = TRUE),max(par_species$k, na.rm = TRUE)),
  sl95=c(min(par_species$sl95, na.rm = TRUE),max(par_species$sl95, na.rm = TRUE)),
  ascendse=c(min(par_species$ascendse, na.rm = TRUE),max(par_species$ascendse, na.rm = TRUE)),
  lnr0 = c(min(par_species$lnr0, na.rm = TRUE),max(par_species$lnr0, na.rm = TRUE)),
  tmax = c(min(par_species$tmax, na.rm = TRUE),max(par_species$tmax, na.rm = TRUE)))
  # Grid base
  grid_base <- data.frame(
  codsp = species,
  m = mean(c(par_species$m, par_species$m_algaraja, par_species$m_pauly), na.rm = TRUE),
  linf = mean(par_species$linf, na.rm = TRUE),
  k = mean(par_species$k, na.rm = TRUE),
  sl95 = mean(par_species$sl95, na.rm = TRUE),
  ascendse = mean(par_species$ascendse, na.rm = TRUE),
  lnr0 = mean(par_species$lnr0, na.rm = TRUE),
  tmax = round(mean(par_species$tmax, na.rm = TRUE)),
  jittering_par = "avg")
  #Generate grids with limits
  grids_especie <- generate_grids(grid_base, n_scenarios, per_jitter_modif, param_limits)
  param_grid <- dplyr::bind_rows(param_grid, dplyr::bind_rows(grids_especie))
  }
  } 
  
  #2- Univariate jitter (one parameter varying at time)
  if (type_jitter == 2) {
  param_grid <- data.frame()
  #Function to generate grids
  generate_grid <- function(species_data, param, n_jitter, per_jitter) {
  # Base case scenario
  base_values <- list(
  m = mean(c(species_data$m, species_data$m_algaraja, species_data$m_pauly), na.rm = TRUE),
  linf = mean(species_data$linf, na.rm = TRUE),
  k = mean(species_data$k, na.rm = TRUE),
  sl95 = mean(species_data$sl95, na.rm = TRUE),
  ascendse = mean(species_data$ascendse, na.rm = TRUE),
  lnr0 = mean(species_data$lnr0, na.rm = TRUE),
  tmax = round(mean(species_data$tmax, na.rm = TRUE))
  )
  # grid base average
  if (param == "avg") {
  return(data.frame(base_values) %>% 
  dplyr::mutate(codsp = species_data$codsp[1], jittering_par = "avg"))
  }
  # special logic for m
  if (param == "m") {
  values <- c(species_data$m, species_data$m_algaraja, species_data$m_pauly)
  } else {
  values <- species_data[[param]]
  }
  # special logic for tmax (integer)
  if (param == "tmax") {
  seq_values <- round(seq(min(values, na.rm = TRUE) - 1, 
                          max(values, na.rm = TRUE) + 1, 
                              length.out = n_jitter))
  } else {
  seq_values <- seq(min(values, na.rm = TRUE) * (1 - per_jitter),
                          max(values, na.rm = TRUE) * (1 + per_jitter),
                          length.out = n_jitter)
  }
  # Criar grid
  grid <- data.frame(base_values) %>% 
  dplyr::slice(rep(1, n_jitter)) %>% 
  dplyr::mutate(
  across(all_of(param), ~ seq_values),
  codsp = species_data$codsp[1],
  jittering_par = param
  ) %>% 
  # round not applied to tmax
  dplyr::mutate(
  across(c(m, linf, k, sl95, ascendse, lnr0), ~ round(.x, 2)),
  tmax = round(tmax)
  )
  return(grid)
  }
  # Loop principal
  for (species in sp) {
  par_species <- par_ss %>% filter(codsp == species)
  # Grid average
  grid_avg <- generate_grid(par_species, "avg", n_jitter, per_jitter)
  # Grids para cada parâmetro
  params <- c("m", "linf", "k", "sl95", "ascendse", "lnr0", "tmax")
  grids <- lapply(params, function(p) generate_grid(par_species, p, n_jitter, per_jitter))
  # Combine all grids
  param_grid <- bind_rows(param_grid, grid_avg, grids)
  }
  }
  
  #3- Multivariate jitter (multiple parameters varying at time) expand.grid combinations
  if (type_jitter==3) {
  #Grid for combination of parameters (assuming per_jitter% of variability)
  param_grid <- data.frame() #empty frame
  # Loop to generate param grid for each species
  for (species in sp) {
  par_species <- par_ss %>% filter(codsp == species)

  #Creating grid
  grid <- expand.grid(
  m = seq(min(c(par_species$m, par_species$m_algaraja, par_species$m_pauly),na.rm = TRUE)* (1-per_jitter) ,
          max(c(par_species$m, par_species$m_algaraja, par_species$m_pauly),na.rm = TRUE)* (1+per_jitter) ,length.out = n_jitter),
  linf = seq(min(par_species$linf,na.rm = TRUE)* (1-per_jitter), max(par_species$linf,na.rm = TRUE)* (1+per_jitter),length.out = n_jitter),
  k = seq(min(par_species$k,na.rm = TRUE)* (1-per_jitter), max(par_species$k,na.rm = TRUE)* (1+per_jitter) ,length.out = n_jitter),
  sl95 = seq(min(par_species$sl95,na.rm = TRUE)* (1-per_jitter), max(par_species$sl95,na.rm = TRUE)*(1+per_jitter),length.out = n_jitter),
  ascendse= seq(min(par_species$ascendse,na.rm = TRUE)*(1-per_jitter),max(par_species$ascendse,na.rm = TRUE)*(1+per_jitter),length.out = n_jitter),
  lnr0 = seq(min(par_species$lnr0,na.rm = TRUE)*(1-per_jitter),max(par_species$lnr0,na.rm = TRUE)*(1+per_jitter),length.out = n_jitter),
  tmax= round(seq(min(par_species$tmax,na.rm = TRUE)-(1),max(par_species$tmax,na.rm = TRUE)+(1),length.out = n_jitter))
  ) %>%
  dplyr::mutate(codsp = species,
                jittering_par='all') %>% # species column
  dplyr::bind_rows(., 
      data.frame(codsp=species,
                m = mean(c(par_species$m, par_species$m_algaraja, par_species$m_pauly),na.rm = TRUE),
                linf = mean(par_species$linf,na.rm = TRUE),
                k = mean(par_species$k,na.rm = TRUE),
                sl95 = mean(par_species$sl95,na.rm = TRUE),
                ascendse= mean(par_species$ascendse,na.rm = TRUE),
                lnr0 = mean(par_species$lnr0,na.rm = TRUE),
                tmax=  mean(par_species$tmax,na.rm = TRUE),
                jittering_par="avg")) %>%
  dplyr::mutate(dplyr::across(c(m, linf, k, sl95, ascendse, lnr0), ~ round(.x, 2)))
  }
  #Original data frame expanded
  param_grid <- dplyr::bind_rows(param_grid, grid)
  }#---------------------------------------------------------
  
  #---- Main Jitter function ------#
  jitter_process <- function(params) {
      
  #Extracting the parameter (current iteration). 
  codsp_jit<-params$codsp #species 
  jittering_par_jit<- params$jittering_par#parameter of the current jitter *The rest is constant
  m_jit <- params$m #natural mortality
  linf_jit <- params$linf #linf growth curve
  k_jit <- params$k   # k growth constant
  sl95_jit <- params$sl95 #peak selectvity 
  ascendse_jit<- params$ascendse #standard error ascend part of the selectivity curve
  lnr0_jit <- params$lnr0 #log initial recruitment
  tmax_jit<- params$tmax #tmax age
  
  #Reading SS input files.....
  start<- r4ss::SS_readstarter(file.path(dir_input, "starter.ss")) #Start SS file
  fore<- r4ss::SS_readforecast(file.path(dir_input, "forecast.ss")) #Forecast SS file
  dat<- r4ss::SS_readdat(file.path(dir_input, "datafile.dat"))      #Data SS file
  ctl <- r4ss::SS_readctl(file.path(dir_input, "controlfile.ctl"),verbose = TRUE,datlist = dat, use_datlist = TRUE) #Control SS file
  
  #Catch compositions (current iteration)
  catch= read.csv(file.path(dir, paste(codsp_jit,"_catch.csv", sep = "")), sep = ",",dec = ".") #catch history
  #Catch data (current iteration) 
  catch_jit= data.frame(year= as.integer(c(-999, catch$Year)),seas= as.integer(1),fleet=as.integer(1),catch=c(1.000000e-20, catch$Freire),catch_se=0.01)
  
  #Length information (Jittering process using only observed lengths)
  length_obs= read.csv(file.path(dir, paste(codsp_jit,"_length_obs.csv", sep = "")), sep = ",",dec = ".") #observed lengths
  length_jit=data.frame(year=length_obs$Year,month=length_obs$Month,fleet=length_obs$Fleet,sex=length_obs$Sex,part=0,Nsamp=length_obs$Nsamps)
  len=length_obs[, grep("^X", names(length_obs), value = TRUE)] #changing the initial Letter
  names(len) <- gsub("^X", "f", names(len))
  len2= len;len2[,] <- 0; names(len2) <- gsub("^f", "m", names(len2)) #empty frame
  length_jit=cbind(length_jit, len,len2) 
  length_jit[] <- lapply(length_jit, as.integer)
  
  #changing the SS input files for the current iteration 
  #Start File
  #Forecast File
  fore$Flimitfraction= 1
  #Data file
  dat$catch= catch_jit #catch data
  dat$lencomp= length_jit #length composition
  dat$Nages= tmax_jit #tmax  
  dat$binwidth=as.numeric(diff(as.numeric(gsub("^f", "", names(len))))[1]) #binwidth size
  dat$lbin_vector=as.numeric(gsub("^f", "", names(len))) #length bins vector
  dat$minimum_size=as.numeric(round(min(c( 0.04*mean(par_ss$linf[par_ss$codsp==codsp_jit],na.rm = TRUE), min(dat$lbin_vector))))) #mapping from 4% of the Linf or min lbin
  dat$maximum_size=as.numeric(round(max(c(1.25*mean(par_ss$linf[par_ss$codsp==codsp_jit],na.rm = TRUE), max(dat$lbin_vector*1.25))))) #mapping to 25% above linf or above max lbin
  dat$N_lbins=as.numeric(ncol(len)) #number of bins
  dat$N_agebins= as.numeric(dat$Nages)
  dat$agebin_vector= as.vector(as.numeric(seq(0, length.out=dat$Nages))) #ages bin vector
  dat$ageerror=as.data.frame(matrix(c(-1.000, 0.001),nrow = 2,ncol = length(paste0("age", seq(0, dat$Nages))),dimnames = list(NULL, paste0("age", seq(0, dat$Nages)))))
  dat$lbin_vector_pop=as.numeric(seq(dat$minimum_size,max(c(dat$maximum_size+dat$binwidth), max(dat$lbin_vector+dat$binwidth)), by=dat$binwidth)) #population bins (outside the real length compositions)
  dat$N_lbinspop=as.numeric(length(dat$lbin_vector_pop)) #n o bins of the population (outside the real lengths)
  #Control File
  ctl$Nages= dat$Nages #number of ages
  ctl$Npopbins=dat$N_lbinspop #number of lbins population
  ctl$Growth_Age_for_L1= mean(par_ss$t0[par_ss$codsp==codsp_jit],na.rm = TRUE) #t0 growth estimate
  ctl$MG_parms['NatM_p_1_Fem_GP_1',c('INIT')]= m_jit #natural mortality
  ctl$MG_parms['L_at_Amax_Fem_GP_1',c('INIT','PRIOR')]= c(linf_jit,linf_jit) #growth linf estimate
  ctl$MG_parms['VonBert_K_Fem_GP_1',c('INIT','PRIOR')]= c(k_jit,k_jit) #growth constant k
  ctl$MG_parms['Wtlen_1_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_a[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$wl_a[par_ss$codsp==codsp_jit],na.rm = TRUE))#a length-weight 
  ctl$MG_parms['Wtlen_2_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_b[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$wl_b[par_ss$codsp==codsp_jit],na.rm = TRUE))#b length-weight
  ctl$MG_parms['Mat50%_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$lm50[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$lm50[par_ss$codsp==codsp_jit],na.rm = TRUE))#L50% maturity
  ctl$MG_parms['Mat_slope_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$lmslope[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$lmslope[par_ss$codsp==codsp_jit],na.rm = TRUE))#slope maturity
  ctl$MG_parms['NatM_p_1_Mal_GP_1',c('INIT')]= m_jit #natural mortality 
  ctl$MG_parms['L_at_Amax_Mal_GP_1',c('INIT','PRIOR')]=c(linf_jit,linf_jit) #growth linf estimate
  ctl$MG_parms['VonBert_K_Mal_GP_1',c('INIT','PRIOR')]=c(k_jit,k_jit) #growth constant k
  ctl$MG_parms['Wtlen_1_Mal_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_a[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$wl_a[par_ss$codsp==codsp_jit],na.rm = TRUE))#a length-weight 
  ctl$MG_parms['Wtlen_2_Mal_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_b[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$wl_b[par_ss$codsp==codsp_jit],na.rm = TRUE))#b length-weight
  ctl$SR_parms['SR_LN(R0)', c('INIT','PRIOR','PHASE')]= c(lnr0_jit,lnr0_jit,1) #Log initial Recruitment
  ctl$SR_parms['SR_BH_steep',c('INIT','PRIOR')]= c(mean(par_ss$h[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$h[par_ss$codsp==codsp_jit],na.rm = TRUE))#Recruitment steepness 
  ctl$SR_parms['SR_sigmaR',c('INIT','PRIOR')]= c(mean(par_ss$sigmar0[par_ss$codsp==codsp_jit],na.rm = TRUE),mean(par_ss$sigmar0[par_ss$codsp==codsp_jit],na.rm = TRUE))#Sigma Recruitment
  if (codsp_jit!="BRS") { # Logistic Selectivity
  ctl$size_selex_parms['SizeSel_P_1_Fishery(1)',c('LO','HI','INIT','PRIOR','PHASE')]=c(0.25*sl95_jit,1.25*sl95_jit,sl95_jit,sl95_jit,3)
  ctl$size_selex_parms['SizeSel_P_2_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(15,15,-3)
  ctl$size_selex_parms['SizeSel_P_3_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(ascendse_jit,ascendse_jit,3)
  ctl$size_selex_parms['SizeSel_P_4_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-15,-3)    
  ctl$size_selex_parms['SizeSel_P_5_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-10,-3)     
  ctl$size_selex_parms['SizeSel_P_6_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(15,15,-3)  
  }
  if (codsp_jit=='BRS') { #Dome-Shaped Selectivity
  ctl$size_selex_parms['SizeSel_P_1_Fishery(1)',c('LO','HI','INIT','PRIOR','PHASE')]=c(0.25*sl95_jit,1.5*sl95_jit,sl95_jit,sl95_jit,3)
  ctl$size_selex_parms['SizeSel_P_2_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-1.42,-1.42,-3)
  ctl$size_selex_parms['SizeSel_P_3_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(ascendse_jit,ascendse_jit,3)
  ctl$size_selex_parms['SizeSel_P_4_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-2.3,-2.3,-3)  
  ctl$size_selex_parms['SizeSel_P_5_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-10,-3)   
  ctl$size_selex_parms['SizeSel_P_6_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-2.2,-2.2,-3)        
  }
  #Creating the folder to store input data files and the outputs for each iteration
  dir_out <- file.path(dir, "SS3", "jitter", paste(codsp_jit, m_jit, linf_jit, k_jit, sl95_jit, ascendse_jit,lnr0_jit,tmax_jit, sep = "_"))
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  # write out all (because of the changes of the current iteration)
  SS_writestarter(start, file.path(dir_out), overwrite = TRUE)
  SS_writeforecast(fore, file.path(dir_out), overwrite = TRUE) 
  SS_writedat(dat, file.path(dir_out,"datafile.dat"), overwrite = TRUE)
  SS_writectl(ctl, file.path(dir_out,"controlfile.ctl"), overwrite = TRUE)
  
  tryCatch({
  message("Running Species: ", codsp_jit)
  # Run the model and store results (Estimating peak selectivity (sl95) and Initial Recruitment (LN R0)
  run(file.path(dir_out), exe =file.path(dir_input,"ss3.exe"),show_in_console = TRUE, verbose = TRUE, skipfinished = FALSE)
  out <- r4ss::SS_output(file.path(dir_out), verbose = FALSE, printstats=FALSE)
  #SS_plots(out)
  #Process the results if no error is shown
  #Gathering results
  result= list(
  codsp=codsp_jit,jittering_par=jittering_par_jit,minyr=as.numeric(out$startyr),maxyr=as.numeric(out$endyr),m=as.numeric(m_jit),
  tmax=as.numeric(dat$Nages),linf=as.numeric(linf_jit),k=as.numeric(k_jit),t0=as.numeric(mean(par_ss$t0[par_ss$codsp==codsp_jit],na.rm = TRUE)),
  lm50=as.numeric(mean(par_ss$lm50[par_ss$codsp==codsp_jit],na.rm = TRUE)),lmslope=as.numeric(mean(par_ss$lmslope[par_ss$codsp==codsp_jit],na.rm = TRUE)),
  sl95=as.numeric(sl95_jit), sl95est=as.numeric(out$parameters$Value[out$parameters$Label=="Size_DblN_peak_Fishery(1)"]),
  ascendse=as.numeric(ascendse_jit),ascendse_est=as.numeric(out$parameters$Value[out$parameters$Label=="Size_DblN_ascend_se_Fishery(1)"]),
  wla=as.numeric(mean(par_ss$wl_a[par_ss$codsp==codsp_jit],na.rm = TRUE)), wlb=as.numeric(mean(par_ss$wl_b[par_ss$codsp==codsp_jit],na.rm = TRUE)),
  lnr0=as.numeric(lnr0_jit),lnr0_est=as.numeric(out$parameters$Value[out$parameters$Label=="SR_LN(R0)"]),
  sigmar0=as.numeric(mean(par_ss$sigmar0[par_ss$codsp==codsp_jit],na.rm = TRUE)),steep=as.numeric(mean(par_ss$h[par_ss$codsp==codsp_jit],na.rm = TRUE)),
  binwidth=as.numeric(dat$binwidth),minsize=as.numeric(dat$minimum_size), maxsize=as.numeric(dat$maximum_size),
  nbins=as.numeric(dat$N_lbins),nbinspop=as.numeric(dat$N_lbinspop),
  recdev_1950=as.numeric(out$recruit$dev[out$recruit$Yr==1950]),recdev_1960=as.numeric(out$recruit$dev[out$recruit$Yr==1960]),
  recdev_1970=as.numeric(out$recruit$dev[out$recruit$Yr==1970]),recdev_1980=as.numeric(out$recruit$dev[out$recruit$Yr==1980]),
  recdev_1990=as.numeric(out$recruit$dev[out$recruit$Yr==1990]),recdev_2000=as.numeric(out$recruit$dev[out$recruit$Yr==2000]),
  recdev_2010=as.numeric(out$recruit$dev[out$recruit$Yr==2010]),recdev_2020=as.numeric(out$recruit$dev[out$recruit$Yr==2020]),
  recdev_max=as.numeric(max(out$recruit$dev)),recdev_maxyr=as.numeric(out$recruit$Yr[out$recruit$dev==max(out$recruit$dev)][1]),
  f_1950=as.numeric(out$derived_quants$Value[grep("F_(1950)",out$derived_quants$Label)]),
  f_1960=as.numeric(out$derived_quants$Value[grep("F_(1960)",out$derived_quants$Label)]),
  f_1970=as.numeric(out$derived_quants$Value[grep("F_(1970)",out$derived_quants$Label)]),
  f_1980=as.numeric(out$derived_quants$Value[grep("F_(1980)",out$derived_quants$Label)]),
  f_1990=as.numeric(out$derived_quants$Value[grep("F_(1990)",out$derived_quants$Label)]),
  f_2000=as.numeric(out$derived_quants$Value[grep("F_(2000)",out$derived_quants$Label)]),
  f_2010=as.numeric(out$derived_quants$Value[grep("F_(2010)",out$derived_quants$Label)]),
  f_2020=as.numeric(out$derived_quants$Value[grep("F_(2020)",out$derived_quants$Label)]),
  ssb_1950=as.numeric(out$derived_quants$Value[grep("SSB_(1950)",out$derived_quants$Label)]),
  ssb_1960=as.numeric(out$derived_quants$Value[grep("SSB_(1960)",out$derived_quants$Label)]),
  ssb_1970=as.numeric(out$derived_quants$Value[grep("SSB_(1970)",out$derived_quants$Label)]),
  ssb_1980=as.numeric(out$derived_quants$Value[grep("SSB_(1980)",out$derived_quants$Label)]),
  ssb_1990=as.numeric(out$derived_quants$Value[grep("SSB_(1990)",out$derived_quants$Label)]),
  ssb_2000=as.numeric(out$derived_quants$Value[grep("SSB_(2000)",out$derived_quants$Label)]),
  ssb_2010=as.numeric(out$derived_quants$Value[grep("SSB_(2010)",out$derived_quants$Label)]),
  ssb_2020=as.numeric(out$derived_quants$Value[grep("SSB_(2020)",out$derived_quants$Label)]),
  ssb_ssb0_1950=as.numeric(out$derived_quants$Value[grep("Bratio_(1950)",out$derived_quants$Label)]),
  ssb_ssb0_1960=as.numeric(out$derived_quants$Value[grep("Bratio_(1960)",out$derived_quants$Label)]),
  ssb_ssb0_1970=as.numeric(out$derived_quants$Value[grep("Bratio_(1970)",out$derived_quants$Label)]),
  ssb_ssb0_1980=as.numeric(out$derived_quants$Value[grep("Bratio_(1980)",out$derived_quants$Label)]),
  ssb_ssb0_1990=as.numeric(out$derived_quants$Value[grep("Bratio_(1990)",out$derived_quants$Label)]),
  ssb_ssb0_2000=as.numeric(out$derived_quants$Value[grep("Bratio_(2000)",out$derived_quants$Label)]),
  ssb_ssb0_2010=as.numeric(out$derived_quants$Value[grep("Bratio_(2010)",out$derived_quants$Label)]),
  ssb_ssb0_2020=as.numeric(out$derived_quants$Value[grep("Bratio_(2020)",out$derived_quants$Label)]),
  total_nll=as.numeric(out$likelihoods_used["TOTAL",'values']),catch_nll=as.numeric(out$likelihoods_used["Catch",'values']),
  length_nll=as.numeric(out$likelihoods_used["Length_comp",'values']),recruitment_nll=as.numeric(out$likelihoods_used["Recruitment",'values']))
  message("Species ", codsp_jit, " processed successfully.")
  return(result)
  },
  error = function(e) {
  message("Error occurred in species ", codsp_jit, ": ", e$message)
  #In case of error jump to the next parameters combination (iteration)
  })
  }
  # Parallelizing loop
  # Cluster
  n_cores <- detectCores() 
  cl <- makeCluster(n_cores)
  #Exporting necessary objects (Everything outside the main jitter function must be here)
  clusterExport(cl, c("param_grid", "jitter_process","dir","dir_input","par_ss"))
  parallel::clusterEvalQ(cl, c(library(r4ss,dplyr)))
  
  #Lapply parallelized to apply the Jitter function
  results <- parLapply(cl, seq_len(nrow(param_grid)), function(i) {
  # Selecting [i] line parameters
  params <- param_grid[i, ]
  jitter_process(params)  #Apply the Main jitter function
  })
  #End cluster
  parallel::stopCluster(cl)
    
  #Combining results in a data frame
  jitter_out <- as.data.frame(do.call(rbind, results)) 
  
  #Jitter out as data frame to csv
  jitter_out_csv <- jitter_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.)))
  # Write the full dataset before filtering
  write.csv(jitter_out_csv, file = file.path(dir, "jitter_out.csv"), row.names = FALSE)
  
  #Filtering the best models
  #-- Fishing mortality not stable around 0
  #-- Initial Recruitment LNR0 >=1 or <=9
  #-- Stock Spawning Biomass not collapsed <0.1
  #-- Relative Spawning Biomass SSB/SSB0 not collapsed <0.01
  best_jitter <- jitter_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
  dplyr::group_by(codsp) %>% 
  dplyr::mutate(all_negative_nll = all(total_nll < 0)) %>%  #Verifiyng if all NLL are negative
  dplyr::ungroup() %>%
  dplyr::rowwise() %>%  #Conditions row by row
  dplyr::mutate(remove_scenario = 
               (f_1980 < 0.01 | f_1990 < 0.01 | f_2000 < 0.01 | f_2010 < 0.01 | 
                lnr0_est < 1 | lnr0_est > 9 | 
                ssb_1980 < 0.1 | ssb_1990 < 0.1 | ssb_2000 < 0.1 | ssb_2010 < 0.1 |
                ssb_ssb0_1980 < 0.01 | ssb_ssb0_1990 < 0.01 | ssb_ssb0_2000 < 0.01 | ssb_ssb0_2010 < 0.01)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!remove_scenario)# %>%  #removing bad scenarios
  #dplyr::select(codsp, m, linf, k, sl95, lnr0, tmax, ascendse, total_nll, length_nll)
  
  # Print the best models 
  cat("Best models:\n")
  print(best_jitter)
  if (nrow(best_jitter) == 0) {
  cat("Total of: ", nrow(best_jitter), " best models\n")
  stop("No convergence -- Run the models again")
  } else {
  best_fit <- best_jitter %>%
  dplyr::group_by(codsp) %>%
  dplyr::filter(total_nll == min(total_nll, na.rm = TRUE)) %>%  #the minimum nll
  dplyr::select(codsp, m, linf, k, sl95, lnr0, tmax, ascendse, total_nll, length_nll)  
  # Print the best model
  cat("Best fit:\n")
  print(best_fit)
  }
  #-------------------------------------------------
  # Plot section (Univariate Likelihood comparisons)
  #-------------------------------------------------
  # library(tidyr)
  library(ggplot2)
  if (type_jitter==2) { #univariate
    
  #Agregating the variable parameters by jittering_par
  jitter_out2 <- jitter_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
  dplyr::filter(jittering_par!="avg")%>%
  dplyr::rowwise() %>%
  dplyr::mutate(param_value = case_when(
  jittering_par == "m"     ~ m,
  jittering_par == "linf"  ~ linf,
  jittering_par == "k"     ~ k,
  jittering_par == "sl95"  ~ sl95,
  jittering_par == "ascendse"~ ascendse,
  jittering_par == "lnr0"  ~ lnr0,
  jittering_par == "tmax"  ~ tmax,
  TRUE ~ NA_real_
  )) %>%
  dplyr::ungroup()
  
  #Univariate Likelihood profile 
  p66 <- ggplot(jitter_out2, aes(x = param_value, y = total_nll, color = codsp)) +
  geom_line(linewidth=1.5) +
  geom_point(size = 2.5, alpha = 0.7) +
  facet_wrap(~ jittering_par, scales = "free") +
      labs(x = "Parameter Value",
           y = "Negative Log Likelihood",
           color = "Species")+
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
       plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  print(p66)
  
  ggplot2::ggsave("SS3_likelihood_profile.png",plot=p66, device = "png", units = "cm")
  }
  
  #-----------------------------------------------
  # Fit comparison (Base case scenario x Best fit)
  #-----------------------------------------------
  len_comparison_out <- data.frame()  # Empty frame to store results
  model_dirs <- data.frame(codsp = character(), dir_avg = character(), dir_best = character(), stringsAsFactors = FALSE)  # Empty frame for directory storage
  
  for (species in sp) {
  # Average and best parameters
  avg_pars <- param_grid %>%
  dplyr::filter(jittering_par == "avg", codsp == species)
    
  bst_pars <- best_jitter %>%
  dplyr::filter(codsp == species)
  if (nrow(bst_pars) == 0) {
  cat("Total of: ", nrow(bst_pars), " best parameters found\n")
  stop("No convergence -- Run the models again")
  } else {
  bst_pars <- bst_pars %>%
  dplyr::filter(total_nll == min(total_nll, na.rm = TRUE)) %>%
  dplyr::select(codsp, m, linf, k, sl95, lnr0, tmax, ascendse, total_nll, length_nll)
  cat("Best fit:\n"); print(bst_pars)
  }
    
  # Average and best data files ("outfile SS3")
  dir_avg <- file.path(dir, "SS3", "jitter", paste(species, avg_pars$m, avg_pars$linf, avg_pars$k, avg_pars$sl95, avg_pars$ascendse, avg_pars$lnr0, avg_pars$tmax, sep = "_"))
  dir_bst <- file.path(dir, "SS3", "jitter", paste(species, bst_pars$m, bst_pars$linf, bst_pars$k, bst_pars$sl95, bst_pars$ascendse, bst_pars$lnr0, bst_pars$tmax, sep = "_"))
  model_dirs <- dplyr::bind_rows(model_dirs, data.frame(codsp = species, dir_avg = dir_avg, dir_best = dir_bst, stringsAsFactors = FALSE))
  
  # SS output reading
  out_avg <- r4ss::SS_output(dir_avg, verbose = FALSE, printstats = FALSE)
  out_bst <- r4ss::SS_output(dir_bst, verbose = FALSE, printstats = FALSE)
    
  # Calculating the overall observed frequencies
  len_comparison_out <- dplyr::bind_rows(
  len_comparison_out,
  out_avg$lendbase %>% group_by(Bin) %>% summarise(Total = sum(Obs, na.rm = TRUE)) %>% mutate(Tipo = "Observed", Model = "Observed", codsp = species),
  out_avg$lendbase %>% group_by(Bin) %>% summarise(Total = sum(Exp, na.rm = TRUE)) %>% mutate(Tipo = "Expected", Model = "Average", codsp = species),
  out_bst$lendbase %>% group_by(Bin) %>% summarise(Total = sum(Exp, na.rm = TRUE)) %>% mutate(Tipo = "Expected", Model = "Best fit", codsp = species)
  )
  }
  # Writing a CSV file with the average and best directories
  write.csv(model_dirs, file = file.path(dir, "best_jitter_directories.csv"), row.names = FALSE)
  
  # Plot of length frequencies + fitted models
  p67 <- ggplot(len_comparison_out, aes(x = Bin, y = Total)) +
  geom_area(data = filter(len_comparison_out, Tipo == "Observed"), aes(fill = Tipo), alpha = 0.4) +
  geom_line(data = filter(len_comparison_out, Tipo == "Expected"), aes(color = Model), linewidth = 1.5) +
  scale_fill_manual(values = c("Observed" = "grey60")) +
  labs(x = "Length class (cm)", y = "Proportion", fill = "") +
  facet_wrap(~codsp, scales = "free") + 
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(), plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  
  p67
  ggplot2::ggsave("SS3_Length_fit.png",plot=p67, device = "png", units = "cm")
  
}

  
 
  #------------------------------- Sensitivity Analysis using Stock Synthesis (SS3) --------------------------------#
  if (run_sensitivity == TRUE) {
  
  #read the best and average models
  bst_dir <- read.csv(file.path(dir, "best_jitter_directories.csv"), sep = ",", dec = ".") 
  
  #function to extract the best and average parameters from the directory
  extract_params <- function(dir_string, param_positions) {
  params <- strsplit(dir_string, "_")[[1]]
  sapply(param_positions, function(i) as.numeric(params[i]))
  }
  #Parameters grid
  param_grid <- data.frame()
  sensitivity_types <- c("obs", "obspred", "pred")
  for (species in sp) {
  par_species <- bst_dir %>% filter(codsp == species)
  #Extracting parameters
  best_params <- extract_params(par_species$dir_best, 2:8)
  avg_params <- extract_params(par_species$dir_avg, 2:8)
  
  #Creating grid
  generate_all_grid <- function(best_params, n) {
  data.frame(
  m = seq(best_params[1]*(1-per_sensitivity), best_params[1]*(1+per_sensitivity), length.out = n),
  linf = seq(best_params[2]*(1-per_sensitivity), best_params[2]*(1+per_sensitivity), length.out = n),
  k = seq(best_params[3]*(1-per_sensitivity), best_params[3]*(1+per_sensitivity), length.out = n),
  sl95 = seq(best_params[4]*(1-per_sensitivity), best_params[4]*(1+per_sensitivity), length.out = n),
  ascendse = seq(best_params[5]*(1-per_sensitivity), best_params[5]*(1+per_sensitivity), length.out = n),
  lnr0 = seq(best_params[6]*(1-per_sensitivity), best_params[6]*(1+per_sensitivity), length.out = n),
  tmax = round(seq(best_params[7]-1, best_params[7]+1, length.out = n))
  ) %>% mutate(sensitivity_par = "all")
  }
  #Average scenario ('avg')
  avg_row <- data.frame(
  m = avg_params[1],
  linf = avg_params[2],
  k = avg_params[3],
  sl95 = avg_params[4],
  ascendse = avg_params[5],
  lnr0 = avg_params[6],
  tmax = avg_params[7],
  sensitivity_par = "avg"
  )
  #Combine all scenarios
  grid <- bind_rows(
  generate_all_grid(best_params, n_sensitivity),
  avg_row
  ) %>% 
  tidyr::crossing(sensitivity_type = sensitivity_types) %>%
  mutate(
  codsp = species,
  across(c(m, linf, k, sl95, ascendse, lnr0), ~round(.x, 2))
  ) %>%
  select(codsp, sensitivity_type, sensitivity_par, everything())
  param_grid <- bind_rows(param_grid, grid)
  }
  
  #------- Main sensitivity function --------#
  sensitivity_process <- function(params) {
    
  #Extracting the parameter (current iteration). One parameter at time and the rest remain constant
  sensitivity_par_sensi<- params$sensitivity_par #parameter of the current sensitivity *The rest is constant
  sensitivity_type_sensi<-params$sensitivity_type #type of used data 1-Obs,2-obs+pred,3-pred
  codsp_sensi<-params$codsp #species 
  m_sensi <- params$m #natural mortality
  linf_sensi <- params$linf #linf growth curve
  k_sensi <- params$k  # k growth constant
  sl95_sensi <- params$sl95 #peak selectivity 
  ascendse_sensi <- params$ascendse #standard error ascend part of the selectivity curve
  lnr0_sensi <- params$lnr0 #log initial recruitment
  tmax_sensi<- params$tmax #tmax age
    
  #Reading SS input files.....
  start<- r4ss::SS_readstarter(file.path(dir_input, "starter.ss")) #Start SS file
  fore<- r4ss::SS_readforecast(file.path(dir_input, "forecast.ss")) #Forecast SS file
  dat<- r4ss::SS_readdat(file.path(dir_input, "datafile.dat"))      #Data SS file
  ctl <- r4ss::SS_readctl(file.path(dir_input, "controlfile.ctl"),verbose = TRUE,datlist = dat, use_datlist = TRUE) #Control SS file
  
  #Catch compositions (current iteration)
  catch= read.csv(file.path(dir, paste(codsp_sensi,"_catch.csv", sep = "")), sep = ",",dec = ".") #catch history
  #Catch data (current iteration) 
  catch_sensi= data.frame(year= as.integer(c(-999, catch$Year)),seas= as.integer(1),fleet=as.integer(1),catch=c(1.000000e-20, catch$Freire),catch_se=0.01)
    
  #Length information(1- Observed lengths;2- Observed+predicted; 3- predicted lengths)
  if (sensitivity_type_sensi=="obs") { #1- observed
  length= read.csv(file.path(dir, paste(codsp_sensi,"_length_obs.csv", sep = "")), sep = ",",dec = ".") #observed lengths
  length_sensi=data.frame(year=length$Year,month=length$Month,fleet=length$Fleet,sex=length$Sex,part=0,Nsamp=length$Nsamps)
  len=length[, grep("^X", names(length), value = TRUE)] #changing the initial Letter
  names(len) <- gsub("^X", "f", names(len))
  len2= len;len2[,] <- 0; names(len2) <- gsub("^f", "m", names(len2)) #empty frame
  length_sensi=cbind(length_sensi, len,len2) 
  length_sensi[] <- lapply(length_sensi, as.integer)  } else if(sensitivity_type_sensi=="obspred"){ #observed and predicted
    
  length= read.csv(file.path(dir, paste(codsp_sensi,"_length_obspred.csv", sep = "")), sep = ",",dec = ".") #observed and predicted lengths
  length_sensi=data.frame(year=length$Year,month=length$Month,fleet=length$Fleet,sex=length$Sex,part=0,Nsamp=length$Nsamps)
  len=length[, grep("^X", names(length), value = TRUE)] #changing the initial Letter
  names(len) <- gsub("^X", "f", names(len))
  len2= len;len2[,] <- 0; names(len2) <- gsub("^f", "m", names(len2)) #empty frame
  length_sensi=cbind(length_sensi, len,len2) 
  length_sensi[] <- lapply(length_sensi, as.integer)}  else if(sensitivity_type_sensi=="pred"){ #predicted lengths
  
  length= read.csv(file.path(dir, paste(codsp_sensi,"_length_pred.csv", sep = "")), sep = ",",dec = ".") #predicted lengths
  length_sensi=data.frame(year=length$Year,month=length$Month,fleet=length$Fleet,sex=length$Sex,part=0,Nsamp=length$Nsamps)
  len=length[, grep("^X", names(length), value = TRUE)] #changing the initial Letter
  names(len) <- gsub("^X", "f", names(len))
  len2= len;len2[,] <- 0; names(len2) <- gsub("^f", "m", names(len2)) #empty frame
  length_sensi=cbind(length_sensi, len,len2) 
  length_sensi[] <- lapply(length_sensi, as.integer)  
  }
  
  #changing the SS input files for the current iteration 
  #Start File
  #Forecast File
  fore$Flimitfraction= 1
  #Data file
  dat$catch= catch_sensi #catch data
  dat$lencomp= length_sensi #length composition
  dat$Nages= tmax_sensi #tmax  
  dat$binwidth=as.numeric(diff(as.numeric(gsub("^f", "", names(len))))[1]) #binwidth size
  dat$lbin_vector=as.numeric(gsub("^f", "", names(len))) #length bins vector
  dat$minimum_size=as.numeric(round(min(c(0.04*mean(par_ss$linf[par_ss$codsp==codsp_sensi],na.rm = TRUE), min(dat$lbin_vector))))) #mapping from 4% of the Linf or min lbin
  dat$maximum_size=as.numeric(round(max(c(1.25*mean(par_ss$linf[par_ss$codsp==codsp_sensi],na.rm = TRUE), max(dat$lbin_vector*1.25))))) #mapping to 25% above linf or above max lbin
  dat$N_lbins=as.numeric(ncol(len)) #number of bins
  dat$N_agebins= as.numeric(dat$Nages)
  dat$agebin_vector= as.vector(as.numeric(seq(0, length.out=dat$Nages))) #ages bin vector
  dat$ageerror=as.data.frame(matrix(c(-1.000, 0.001),nrow = 2,ncol = length(paste0("age", seq(0, dat$Nages))),dimnames = list(NULL, paste0("age", seq(0, dat$Nages)))))
  dat$lbin_vector_pop=as.numeric(seq(dat$minimum_size,max(c(dat$maximum_size+dat$binwidth), max(dat$lbin_vector+dat$binwidth)), by=dat$binwidth)) #population bins (outside the real length compositions)
  dat$N_lbinspop=as.numeric(length(dat$lbin_vector_pop)) #n o bins of the population (outside the real lengths)
  #Control File
  ctl$Nages= dat$Nages #number of ages
  ctl$Npopbins=dat$N_lbinspop #number of lbins population
  ctl$Growth_Age_for_L1= mean(par_ss$t0[par_ss$codsp==codsp_sensi],na.rm = TRUE) #t0 growth estimate
  ctl$MG_parms['NatM_p_1_Fem_GP_1',c('INIT')]= m_sensi #natural mortality
  ctl$MG_parms['L_at_Amax_Fem_GP_1',c('INIT','PRIOR')]= c(linf_sensi,linf_sensi) #growth linf estimate
  ctl$MG_parms['VonBert_K_Fem_GP_1',c('INIT','PRIOR')]= c(k_sensi,k_sensi) #growth constant k
  ctl$MG_parms['Wtlen_1_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_a[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$wl_a[par_ss$codsp==codsp_sensi],na.rm = TRUE))#a length-weight 
  ctl$MG_parms['Wtlen_2_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_b[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$wl_b[par_ss$codsp==codsp_sensi],na.rm = TRUE))#b length-weight
  ctl$MG_parms['Mat50%_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$lm50[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$lm50[par_ss$codsp==codsp_sensi],na.rm = TRUE))#L50% maturity
  ctl$MG_parms['Mat_slope_Fem_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$lmslope[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$lmslope[par_ss$codsp==codsp_sensi],na.rm = TRUE))#slope maturity
  ctl$MG_parms['NatM_p_1_Mal_GP_1',c('INIT')]= m_sensi #natural mortality 
  ctl$MG_parms['L_at_Amax_Mal_GP_1',c('INIT','PRIOR')]=c(linf_sensi,linf_sensi) #growth linf estimate
  ctl$MG_parms['VonBert_K_Mal_GP_1',c('INIT','PRIOR')]=c(k_sensi,k_sensi) #growth constant k
  ctl$MG_parms['Wtlen_1_Mal_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_a[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$wl_a[par_ss$codsp==codsp_sensi],na.rm = TRUE))#a length-weight 
  ctl$MG_parms['Wtlen_2_Mal_GP_1',c('INIT','PRIOR')]= c(mean(par_ss$wl_b[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$wl_b[par_ss$codsp==codsp_sensi],na.rm = TRUE))#b length-weight
  ctl$SR_parms['SR_LN(R0)', c('INIT','PRIOR','PHASE')]= c(lnr0_sensi,lnr0_sensi,1) #Log initial Recruitment
  ctl$SR_parms['SR_BH_steep',c('INIT','PRIOR')]= c(mean(par_ss$h[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$h[par_ss$codsp==codsp_sensi],na.rm = TRUE))#Recruitment steepness 
  ctl$SR_parms['SR_sigmaR',c('INIT','PRIOR')]= c(mean(par_ss$sigmar0[par_ss$codsp==codsp_sensi],na.rm = TRUE),mean(par_ss$sigmar0[par_ss$codsp==codsp_sensi],na.rm = TRUE))#Sigma Recruitment
  if (codsp_sensi!="BRS") { # Logistic Selectivity
  ctl$size_selex_parms['SizeSel_P_1_Fishery(1)',c('LO','HI','INIT','PRIOR','PHASE')]=c(0.25*sl95_sensi,1.25*sl95_sensi,sl95_sensi,sl95_sensi,3)
  ctl$size_selex_parms['SizeSel_P_2_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(15,15,-3)
  ctl$size_selex_parms['SizeSel_P_3_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(ascendse_sensi,ascendse_sensi,3)
  ctl$size_selex_parms['SizeSel_P_4_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-15,-3)    
  ctl$size_selex_parms['SizeSel_P_5_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-10,-3)     
  ctl$size_selex_parms['SizeSel_P_6_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(15,15,-3)  
  }
  if (codsp_sensi=='BRS') { #Dome-Shaped Selectivity
  ctl$size_selex_parms['SizeSel_P_1_Fishery(1)',c('LO','HI','INIT','PRIOR','PHASE')]=c(0.25*sl95_sensi,1.5*sl95_sensi,sl95_sensi,sl95_sensi,3)
  ctl$size_selex_parms['SizeSel_P_2_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-1.42,-1.42,-3)
  ctl$size_selex_parms['SizeSel_P_3_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(ascendse_sensi,ascendse_sensi,3)
  ctl$size_selex_parms['SizeSel_P_4_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-2.3,-2.3,-3)  
  ctl$size_selex_parms['SizeSel_P_5_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-10,-3)   
  ctl$size_selex_parms['SizeSel_P_6_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-2.2,-2.2,-3)        
  }
  #Creating the folder to store input data files and the outputs for each iteration
  dir_out <- file.path(dir, "SS3", "sensitivity", paste(codsp_sensi, m_sensi, linf_sensi, k_sensi, sl95_sensi, ascendse_sensi,lnr0_sensi,tmax_sensi, sep = "_"))
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  # write out all (because of the changes of the current iteration)
  SS_writestarter(start, file.path(dir_out), overwrite = TRUE)
  SS_writeforecast(fore, file.path(dir_out), overwrite = TRUE) 
  SS_writedat(dat, file.path(dir_out,"datafile.dat"), overwrite = TRUE)
  SS_writectl(ctl, file.path(dir_out,"controlfile.ctl"), overwrite = TRUE)
    
  tryCatch({
  message("Running Species: ", codsp_sensi)
  # Run the model and store results (Estimating peak selectivity (sl95) and Initial Recruitment (LN R0)
  run(file.path(dir_out), exe =file.path(dir_input,"ss3.exe"),show_in_console = TRUE, verbose = TRUE, skipfinished = FALSE)
  out <- r4ss::SS_output(file.path(dir_out), verbose = FALSE, printstats=FALSE)
  #SS_plots(out)
  #Process the results if no error is shown
  #Gathering results
  result=data.frame(
  codsp=codsp_sensi,sensitivity_par=sensitivity_par_sensi,sensitivity_type=sensitivity_type_sensi,
  minyr=as.numeric(out$startyr),maxyr=as.numeric(out$endyr),m=as.numeric(m_sensi),years=seq(out$startyr,out$endyr),
  tmax=as.numeric(dat$Nages),linf=as.numeric(linf_sensi),k=as.numeric(k_sensi),t0=as.numeric(mean(par_ss$t0[par_ss$codsp==codsp_sensi],na.rm = TRUE)),
  lm50=as.numeric(mean(par_ss$lm50[par_ss$codsp==codsp_sensi],na.rm = TRUE)),lmslope=as.numeric(mean(par_ss$lmslope[par_ss$codsp==codsp_sensi],na.rm = TRUE)),
  sl95=as.numeric(sl95_sensi), sl95est=as.numeric(out$parameters$Value[out$parameters$Label=="Size_DblN_peak_Fishery(1)"]),
  ascendse=as.numeric(ascendse_sensi),ascendse_est=as.numeric(out$parameters$Value[out$parameters$Label=="Size_DblN_ascend_se_Fishery(1)"]),
  wla=as.numeric(mean(par_ss$wl_a[par_ss$codsp==codsp_sensi],na.rm = TRUE)),wlb=as.numeric(mean(par_ss$wl_b[par_ss$codsp==codsp_sensi],na.rm = TRUE)),
  lnr0=as.numeric(lnr0_sensi),lnr0_est=as.numeric(out$parameters$Value[out$parameters$Label=="SR_LN(R0)"]),
  sigmar0=as.numeric(mean(par_ss$sigmar0[par_ss$codsp==codsp_sensi],na.rm = TRUE)),steep=as.numeric(mean(par_ss$h[par_ss$codsp==codsp_sensi],na.rm = TRUE)),
  binwidth=as.numeric(dat$binwidth),minsize=as.numeric(dat$minimum_size), maxsize=as.numeric(dat$maximum_size),
  nbins=as.numeric(dat$N_lbins),nbinspop=as.numeric(dat$N_lbinspop), 
  catch_msy=as.numeric(out$derived_quants$Value[out$derived_quants$Label=="Dead_Catch_MSY"]),#Catch Maximum Sustainable Yield
  catch_msy_std=as.numeric(out$derived_quants$StdDev[out$derived_quants$Label=="Dead_Catch_MSY"]), #Standard deviation
  f_msy=as.numeric(out$derived_quants$Value[out$derived_quants$Label=="annF_MSY"]), #Fishing mortality at MSY
  f_msy_std=as.numeric(out$derived_quants$StdDev[out$derived_quants$Label=="annF_MSY"]),#standard deviation
  ssb_msy=as.numeric(out$derived_quants$Value[out$derived_quants$Label=="SSB_MSY"]), #Stock Spawning Biomass Maximum Sustainable Yield
  ssb_msy_std=as.numeric(out$derived_quants$StdDev[out$derived_quants$Label=="SSB_MSY"]),#standard deviation
  ssb_msy_ssb0=as.numeric(out$derived_quants$Value[out$derived_quants$Label=="B_MSY/SSB_unfished"]),##Stock Spawning Biomass Maximum Sustainable Yield at Virgin SSB
  ssb_msy_ssb0_std=as.numeric(out$derived_quants$StdDev[out$derived_quants$Label=="B_MSY/SSB_unfished"]),#standard deviation
  spr_msy=as.numeric(out$derived_quants$Value[out$derived_quants$Label=="SPR_MSY"]), #Spawning potential Ratio at Maximum Sustainable Yield
  spr_msy_std=as.numeric(out$derived_quants$StdDev[out$derived_quants$Label=="SPR_MSY"]), #standard deviation
  rec=as.numeric(out$recruit$pred_recr[out$recruit$Yr>=1950 & out$recruit$Yr<=2025]), #Recruitment
  recdev=as.numeric(out$recruit$dev[out$recruit$Yr>=1950 & out$recruit$Yr<=2025]), #Recruitment deviation
  f=as.numeric(out$derived_quants$Value[grep("^F_(195[0-9]|19[6-9][0-9]|20[0-1][0-9]|202[0-5])$", out$derived_quants$Label)]),#Fishing mortality
  f_std=as.numeric(out$derived_quants$StdDev[grep("^F_(195[0-9]|19[6-9][0-9]|20[0-1][0-9]|202[0-5])$", out$derived_quants$Label)]),#standard deviation
  ssb=as.numeric(out$derived_quants$Value[grep("^SSB_(195[0-9]|19[6-9][0-9]|20[0-1][0-9]|202[0-5])$", out$derived_quants$Label)]),#Stock Spawning biomass
  ssb_std=as.numeric(out$derived_quants$StdDev[grep("^SSB_(195[0-9]|19[6-9][0-9]|20[0-1][0-9]|202[0-5])$", out$derived_quants$Label)]),#standard deviation
  ssb_ssb0=as.numeric(out$derived_quants$Value[grep("^Bratio_(195[0-9]|19[6-9][0-9]|20[0-1][0-9]|202[0-5])$", out$derived_quants$Label)]),#SSB/SSB Virgin
  ssb_ssb0_std=as.numeric(out$derived_quants$StdDev[grep("^Bratio_(195[0-9]|19[6-9][0-9]|20[0-1][0-9]|202[0-5])$", out$derived_quants$Label)]), #standard deviation
  spr=as.numeric(out$sprseries$SPR[out$sprseries$Yr>=1950 & out$sprseries$Yr<=2025]), #Spawning potential Ratio
  total_nll=as.numeric(out$likelihoods_used["TOTAL",'values']),catch_nll=as.numeric(out$likelihoods_used["Catch",'values']),#Negative Log-Likelihood
  length_nll=as.numeric(out$likelihoods_used["Length_comp",'values']),recruitment_nll=as.numeric(out$likelihoods_used["Recruitment",'values'])) #Length NLL
  message("Species ", codsp_sensi, " processed successfully.")
  return(result)
  },
  error = function(e) {
  message("Error occurred in species ", codsp_sensi, ": ", e$message)
  #In case of error jump to the next parameters combination (iteration)
  })
  }
  # Parallelizing loop
  # Cluster
  n_cores <- detectCores() 
  cl <- makeCluster(n_cores)
  #Exporting necessary objects (Everything outside the main jitter function must be here)
  clusterExport(cl, c("param_grid", "sensitivity_process","dir","dir_input","par_ss"))
  parallel::clusterEvalQ(cl, c(library(r4ss,dplyr)))
  
  #Lapply parallelized to apply the Jitter function
  results <- parLapply(cl, seq_len(nrow(param_grid)), function(i) {
    # Selecting [i] line parameters
    params <- param_grid[i, ]
    sensitivity_process(params)  #Apply the Main jitter function
  })
  #End cluster
  parallel::stopCluster(cl)
  
  #Combining results in a data frame
  sensitivity_out <- as.data.frame(do.call(rbind, results)) 
  
  #Sensitivity out as data frame to csv
  sensitivity_out_csv <- sensitivity_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.)))
  # Write the full dataset before filtering
  write.csv(sensitivity_out_csv, file = file.path(dir, "sensitivity_out.csv"), row.names = FALSE)
  
  #filtering the output scenarios
  #Filtering the best scenarios
  #-- Fishing mortality not stable around 0
  #-- Initial Recruitment LNR0 >=1 or <=9
  #-- Stock Spawning Biomass not collapsed <0.1
  #-- Relative Spawning Biomass SSB/SSB0 not collapsed <0.01
  #-- Avoiding initial collapse of biomass SSB/SSB0< SSBmsy/SSB0
  best_sensitivity <- sensitivity_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
  dplyr::group_by(codsp, sensitivity_type) %>% 
  dplyr::mutate(all_negative_nll = all(total_nll < 0)) %>%  #verifying if all NLL are negative
  dplyr::mutate(trajectory_id = rep(1:(n()/length(unique(years))), each = length(unique(years)))) %>%
  dplyr::ungroup() %>%
  #Filtering scenarios of constant F~0, LNR0 out of bounds, low SSB or Collapse at the beginning 
  dplyr::group_by(codsp, sensitivity_type, trajectory_id) %>%
  dplyr::mutate(remove_scenario = any(
         (years %in% c(1980, 1990, 2000, 2010) &  
         (f < 0.01 | lnr0_est < 1 | lnr0_est > 9 | ssb < 0.1 | ssb_ssb0 < 0.01)) | 
         (years %in% c(1950, 1960, 1970, 1980) & ssb_ssb0 < ssb_msy_ssb0)  |  # colapso inicial
         (codsp %in% c("BRS", "KGM") & years == 2025 & f < 0.17)  #avoiding steady F series for BRS and KGM
  )) %>%
  dplyr::ungroup()
  #Total of scenarios before filtering
  total_counts <- sensitivity_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
  dplyr::group_by(sensitivity_type) %>%
  dplyr::summarise(total = n())
  #Counting how many scenarios were considered "bad"
  removed_counts <- best_sensitivity %>%
  dplyr::group_by(sensitivity_type) %>%
  dplyr::summarise(removed = sum(remove_scenario, na.rm = TRUE)) 
  #Efficiency proportion
  efficiency_summary <- left_join(removed_counts, total_counts, by = "sensitivity_type") %>%
  dplyr::mutate(proportion_removed = removed / total) %>%
  dplyr::arrange(desc(proportion_removed)); print(efficiency_summary)
  #Filtering the bad scenarios from the sensitivity output
  best_sensitivity <- best_sensitivity %>%
  dplyr::filter(!remove_scenario) #removing bad scenarios
  
  #SSB sensitivity time series 
  p68 <- ggplot(best_sensitivity, aes(x = years, y = ssb, group = interaction(sensitivity_type, trajectory_id))) +
  geom_line(aes(color = sensitivity_type), linewidth = 1.5, alpha = 0.4) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  #2015 text
  geom_text(data = best_sensitivity %>%
            group_by(codsp) %>%
            mutate(y_max = max(ssb, na.rm = TRUE) * 0.98) %>%
            slice(1),  # one line to avoid repetition
            aes(x = 2015, y = y_max, label = "2015"),
            inherit.aes = FALSE,  
            vjust = 0, hjust = 0.5, size = 3) +
  #Horizontal line 
  geom_hline(data = best_sensitivity %>%
             dplyr::group_by(codsp) %>%
             dplyr::summarize(mean_ssb_msy = mean(ssb_msy, na.rm = TRUE)), 
             aes(yintercept = mean_ssb_msy, linetype = "ssb_msy"), 
             color = "red", linewidth = 1) +
  labs(x = "Year", y =expression("Stock Spawning Biomass (1000 t)"), 
              fill = "", color = "Length type", linetype = "") +
  # Legend with expression 
  scale_linetype_manual(values = c("ssb_msy" = "solid"), 
                       labels = expression("SSB"[MSY] / "SSB"[0])) +
  facet_wrap(~codsp, scales = "free") + 
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  p68
  
  ggplot2::ggsave("SS3_Sensitive_SSB.png",plot=p68, device = "png", units = "cm",
                                                          width = 28, height = 17)
  
  #Relative SSB/SSB0 sensitivity time series
  p69 <- ggplot(best_sensitivity, aes(x = years, y = ssb_ssb0, group = interaction(sensitivity_type, trajectory_id))) +
  geom_line(aes(color = sensitivity_type), linewidth = 1.5, alpha = 0.4) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  #2015 text
  geom_text(data = best_sensitivity %>%
            group_by(codsp) %>%
            mutate(y_max = max(ssb_ssb0, na.rm = TRUE) * 0.98) %>%
            slice(1),  # taking one line to avoid repetition
            aes(x = 2015, y = y_max, label = "2015"),
            inherit.aes = FALSE,  
            vjust = 0, hjust = 0.5, size = 3) +
  #Horizontal Line
  geom_hline(data = best_sensitivity %>%
             dplyr::group_by(codsp) %>%
             dplyr::summarize(mean_ssb_msy = mean(ssb_msy_ssb0, na.rm = TRUE)), 
             aes(yintercept = mean_ssb_msy, linetype = "ssb_msy"), 
             color = "red", linewidth = 1) +
  labs(x = "Year", y =expression("Relative Stock Spawning Biomass "~ " (SSB" / "SSB"[0]~")"), fill = "", color = "Length type", linetype = "") +
  # Legend using expression 
  scale_linetype_manual(values = c("ssb_msy" = "solid"), 
                        labels = expression("SSB"[MSY] / "SSB"[0])) +
  facet_wrap(~codsp, scales = "free") + 
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  p69
  
  ggplot2::ggsave("SS3_Sensitive_SSB_SSB0.png",plot=p69, device = "png", units = "cm",
                                                                width = 28, height = 17)
  
  p70 <- ggplot(best_sensitivity, aes(x = years, y = f, group = interaction(sensitivity_type, trajectory_id))) +
  geom_line(aes(color = sensitivity_type), linewidth = 1.5, alpha = 0.4) +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  #2015 text 
  geom_text(data = best_sensitivity %>%
            group_by(codsp) %>%
            mutate(y_max = max(c(f,f_msy), na.rm = TRUE) * 0.97) %>%
            slice(1),  # one line by group to avoid repetition
            aes(x = 2015, y = y_max, label = "2015"),
            inherit.aes = FALSE,  
            vjust = 0, hjust = 0.5, size = 3) +
  #Horizontal Line
  geom_hline(data = best_sensitivity %>%
             dplyr::group_by(codsp) %>%
             dplyr::summarize(mean_f_msy = mean(f_msy, na.rm = TRUE)), 
             aes(yintercept = mean_f_msy, linetype = "f_msy"), 
             color = "red", linewidth = 1) +
  labs(x = "Year", y =expression("Fishing mortality (F)"), fill = "", color = "Length type", linetype = "") +
  # Legend with expression 
  scale_linetype_manual(values = c("f_msy" = "solid"), 
                        labels = expression("F"[MSY])) +
  facet_wrap(~codsp, scales = "free") + 
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  p70
  
  ggplot2::ggsave("SS3_Sensitive_F.png",plot=p70, device = "png", units = "cm",
                                                          width = 28, height = 17)
  
  #Filtering only the last year of the series
  last_year <- max(best_sensitivity$years)  
  sensitivity_last <- best_sensitivity %>% 
  dplyr::filter(years == last_year)  
  
  #Boxplot comparing the last SSB/SSB0
  p71 <- ggplot(sensitivity_last, aes(x = codsp, y = ssb_ssb0, 
                                      fill =sensitivity_type)) +
  geom_boxplot() +
  #facet_wrap(~codsp, scales = "free") +
  labs(x = "Species", y = expression("Relative Stock Spawning Biomass "~ " (SSB"[2025] / "SSB"[0]~")"), 
                col = "",fill= "Length type") +
  ylim(0,1)+
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  p71
  
  ggplot2::ggsave("SS3_Sensitive_SSB2025_Comparison.png",plot=p71, device = "png", units = "cm",
                  width = 24, height = 16)
  
  #-------------------------------------------------------------------------------------------
  # Mixed model to evaluate if
  #Is the variation in final biomass (SSB2025/SSB0) explained mainly by the length method used or 
  #by life history parameters, considering the intrinsic variability between species?
  #-------------------------------------------------------------------------------------------
  
  #Mixed model with a random effect
  #Standardizing to mean= 0 and sd =1 
  best_sensitivity_std <- best_sensitivity %>%
  mutate(
  m = as.numeric(scale(m)),
  linf = as.numeric(scale(linf)),
  k = as.numeric(scale(k)),
  sl95 = as.numeric(scale(sl95)),
  lnr0 = as.numeric(scale(lnr0)),
  tmax = as.numeric(scale(tmax)),
  years = as.numeric(scale(years))
  )
  
  # Fitting mixed model with standardized predictors
  mixed_model_std <- lmerTest::lmer(ssb_ssb0 ~ sensitivity_type + m + linf + k + sl95 + 
                            lnr0 + tmax + splines::ns(years, df = 4) + (1 | codsp), 
                          data = best_sensitivity_std, REML = TRUE)
  summary(mixed_model_std, correlation = TRUE)
  
  mixed_model_out<-as.data.frame(summary(mixed_model_std)[10])
  mixed_model_out$variables<- row.names(mixed_model_out)
  mixed_model_out<-data.frame(variables=mixed_model_out$variables,
                      estimate=mixed_model_out$coefficients.Estimate,
                      std.error=mixed_model_out$coefficients.Std..Error,
                      df=mixed_model_out$coefficients.df,
                      t.value=mixed_model_out$coefficients.t.value,
                      p.value=mixed_model_out$coefficients.Pr...t..)
  write.csv(mixed_model_out, file = file.path(dir, "mixed_model_out.csv"), row.names = FALSE)
  
  
  #Likelihood Ratio Test (LR Test) estimating significance of the random effect (REML=FALSE for comparison)
  mixed_model <-  lmerTest::lmer(ssb_ssb0 ~ sensitivity_type + m + linf + k + sl95 + 
                                   lnr0 + tmax + splines::ns(years, df = 4) + (1 | codsp), 
                                 data = best_sensitivity_std, REML = FALSE)
  #Fixed model without random effects
  fixed_model<- lm(ssb_ssb0 ~ sensitivity_type + m + linf + k + sl95 + 
                        lnr0 + tmax+ splines::ns(years, df = 4), data = best_sensitivity_std)
  
  #Likelihood Ratio Test (LR Test)
  logLik_mixed <- logLik(mixed_model)
  logLik_fixed <- logLik(fixed_model)
  
  LRT <- -2 * (logLik_fixed - logLik_mixed) #Test statistics
  p_value <- pchisq(LRT, df = 1, lower.tail = FALSE) # p-value
  cat("LRT =", LRT, "p-value =", p_value, "\n")
  
}

  
  #----------------------------------- Ensemble Analysis using Stock Synthesis (SS3) ------------------------------------#
  if (run_ensemble == TRUE) {
  
  #read the scenarios coming from the sensitivity analysis
  sensitivity_out<-read.csv("sensitivity_out.csv", sep = ',',dec = '.')

  #Filtering the best scenarios
  #-- Fishing mortality not stable around 0
  #-- Initial Recruitment LNR0 >=1 or <=9
  #-- Stock Spawning Biomass not collapsed <0.1
  #-- Relative Spawning Biomass SSB/SSB0 not collapsed <0.01
  #-- Avoiding initial collapse of biomass SSB/SSB0< SSBmsy/SSB0
  best_sensitivity <- sensitivity_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
  dplyr::group_by(codsp, sensitivity_type) %>% 
  dplyr::mutate(all_negative_nll = all(total_nll < 0)) %>%  #verifying if all NLL are negative
  dplyr::mutate(trajectory_id = rep(1:(n()/length(unique(years))), each = length(unique(years)))) %>%
  dplyr::ungroup() %>%
  #Filtering scenarios of constant F~0, LNR0 out of bounds, low SSB or Collapse at the beginning 
  dplyr::group_by(codsp, sensitivity_type, trajectory_id) %>%
  dplyr::mutate(remove_scenario = any(
     (years %in% c(1980, 1990, 2000, 2010) &  
     (f < 0.01 | lnr0_est < 1 | lnr0_est > 9 | ssb < 0.1 | ssb_ssb0 < 0.01)) | 
     (years %in% c(1950, 1960, 1970, 1980) & ssb_ssb0 < ssb_msy_ssb0)  |  # colapso inicial
     (codsp %in% c("BRS", "KGM") & years == 2025 & f < 0.17)  #avoiding steady F series for BRS and KGM
  )) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!remove_scenario) %>%#removing bad scenarios
  dplyr::mutate(f_fmsy=f/f_msy,
                ssb_ssbmsy=ssb/ssb_msy)   
  
  #Taking the best metrics for each species
  best_metrics <- best_sensitivity %>%
  dplyr::group_by(codsp) %>%
  dplyr::summarise(across(
  c(catch_msy, catch_msy_std, f_msy, f_msy_std, 
  ssb_msy, ssb_msy_std, ssb_msy_ssb0, ssb_msy_ssb0_std, 
  spr_msy, spr_msy_std), 
  \(x) mean(x, na.rm = TRUE) 
  ))
  
  #------- Main Ensemble function --------#
  ensemble_process <- function(params) {
    
  #Extracting the parameter (current iteration). One parameter at time and the rest remain constant
  sensitivity_par_ensem<- params$sensitivity_par #parameter of the current sensitivity *The rest is constant
  sensitivity_type_ensem<-params$sensitivity_type #type of used data 1-Obs,2-obs+pred,3-pred
  codsp_ensem<-params$codsp #species 
  m_ensem <- params$m #natural mortality
  linf_ensem <- params$linf #linf growth curve
  k_ensem <- params$k  # k growth constant
  sl95_ensem <- params$sl95 #peak selectivity 
  ascendse_ensem <- params$ascendse #standard error ascend part of the selectivity curve
  lnr0_ensem <- params$lnr0 #log initial recruitment
  tmax_ensem<- params$tmax #tmax age
  year_ensem<- params$years
  f_ensem<- params$f
  f_std_ensem<-params$f_std
  ssb_ensem<- params$ssb
  ssb_std_ensem<- params$ssb_std
  ssb_ssb0_ensem<- params$ssb_ssb0
  ssb_ssb0_std_ensem<- params$ssb_ssb0_std
    
  #creating normal distributions around the mean and standard deviations 
  #Fishing mortality (F) and Relative Stock Spawning Biomass (SSB/SSB0) distributions
  weight_ensem= if (sensitivity_type_ensem=='obs') {
      weight_ensem= obs_weight
  } else if(sensitivity_type_ensem=='obspred'){
        weight_ensem= obspred_weight
  } else if(sensitivity_type_ensem=='pred'){
      weight_ensem= pred_weight}
  
  result<- data.frame(codsp=codsp_ensem,
                years=year_ensem,
                sensitivity_par=sensitivity_par_ensem,
                sensitivity_type=sensitivity_type_ensem,
                m=m_ensem,
                linf=linf_ensem,
                k=k_ensem,
                sl95=sl95_ensem,
                ascendse=ascendse_ensem,
                lnr0=lnr0_ensem,
                tmax=tmax_ensem,
                f=rnorm(n_ensemble*weight_ensem,f_ensem,f_std_ensem),  #f distribution
                ssb=rnorm(n_ensemble*weight_ensem,ssb_ensem,ssb_std_ensem), #ssb distribution
                ssb_ssb0=rnorm(n_ensemble*weight_ensem,ssb_ssb0_ensem,ssb_ssb0_std_ensem))# relative ssb distribution
  return(result)
  }
  # Parallelizing loop
  # Cluster
  n_cores <- detectCores() 
  cl <- makeCluster(n_cores)
  #Exporting necessary objects (Everything outside the main jitter function must be here)
  clusterExport(cl, c("best_sensitivity", "ensemble_process","n_ensemble","obs_weight","obspred_weight","pred_weight"))
  #parallel::clusterEvalQ(cl, c(library(r4ss,dplyr)))
  
  #Lapply parallelized to apply the ensemble function
  results <- parLapply(cl, seq_len(nrow(best_sensitivity)), function(i) {
    # Selecting [i] line parameters
    params <- best_sensitivity[i, ]
    ensemble_process(params)  #Apply the Main jitter function
  })
  #End cluster
  parallel::stopCluster(cl)
  
  #Combining results in a data frame
  ensemble_out <- as.data.frame(do.call(rbind, results)) 
  
  #Calculating the mean and intervals in each year for each species
  best_ensemble <- ensemble_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
  dplyr::group_by(codsp, years) %>% 
  dplyr::summarise(
  mean_f = mean(f, na.rm = TRUE),
  sd_f = sd(f, na.rm = TRUE),
  lower_f = quantile(f, probs = 0.025, na.rm = TRUE),
  upper_f = quantile(f, probs = 0.975, na.rm = TRUE),
  mean_ssb = mean(ssb, na.rm = TRUE),
  sd_ssb = sd(ssb, na.rm = TRUE),
  lower_ssb = quantile(ssb, probs = 0.025, na.rm = TRUE),
  upper_ssb = quantile(ssb, probs = 0.975, na.rm = TRUE),
  mean_ssb_ssb0 = mean(ssb_ssb0, na.rm = TRUE),
  sd_ssb_ssb0 = sd(ssb_ssb0, na.rm = TRUE),
  lower_ssb_ssb0 = quantile(ssb_ssb0, probs = 0.025, na.rm = TRUE),
  upper_ssb_ssb0 = quantile(ssb_ssb0, probs = 0.975, na.rm = TRUE),
  .groups = "drop"
  ) %>%
  dplyr::left_join(best_metrics %>% dplyr::select(codsp,catch_msy,catch_msy_std, f_msy, ssb_msy), by = "codsp") %>%
  dplyr::mutate(f_fmsy=mean_f/f_msy,
                ssb_ssbmsy=mean_ssb/ssb_msy)  
  
  #write results in a .csv
  write.csv(best_ensemble, file = file.path(dir, "ensemble_out.csv"), row.names = FALSE)
  
  
  #Relative SSB/SSB0 ensemble time series
  p72 <- ggplot(best_ensemble, aes(x = years, y = mean_ssb_ssb0)) +
  geom_line(linewidth = 1.5, alpha = 0.45,col="blue") +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  #Confidence interval for SSB_MSY/SSB0 
  geom_ribbon(aes(ymin = lower_ssb_ssb0, ymax = upper_ssb_ssb0), alpha = 0.35, fill = "blue") +
  # Dashed lines representing confidence interval for SSB_MSY/SSB0
  #geom_hline(data = best_metrics, aes(yintercept = ssb_msy_ssb0 - 1.96 * ssb_msy_ssb0_std-0.03), 
  #           color = "red", linetype = "dotted", linewidth = 0.8) +
  #geom_hline(data = best_metrics, aes(yintercept = ssb_msy_ssb0 + 1.96 * ssb_msy_ssb0_std+0.03), 
  #           color = "red", linetype = "dotted", linewidth = 0.8) +
  # Texto do ano 2015
  geom_text(data = best_ensemble %>%
  group_by(codsp) %>%
  mutate(y_max = max(c(mean_ssb_ssb0,upper_ssb_ssb0), na.rm = TRUE) * 0.98) %>%
  slice(1),  
  aes(x = 2015, y = y_max, label = "2015"),
  inherit.aes = FALSE,  
  vjust = 0, hjust = 0.5, size = 3) +
  #Horizontal line SSB_MSY/SSB0
  geom_hline(data = best_metrics, aes(yintercept = ssb_msy_ssb0, linetype = "ssb_msy"), 
             color = "red", linewidth = 1) +
  # Configurações do gráfico
  labs(x = "Year", y = expression("Relative Stock Spawning Biomass "~ " (SSB" / "SSB"[0]~")"), 
       fill = "", color = "Length type", linetype = "") +
  scale_linetype_manual(values = c("ssb_msy" = "solid"), 
                        labels = expression("SSB"[MSY] / "SSB"[0])) +
  facet_wrap(~codsp, scales = "free") + 
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  p72
  
  ggsave("SS3_Ensemble_SSB_SSB0.png", plot = p72, device = "png", units = "cm",
       width = 28, height = 17)
  
  
  #Relative F ensemble time series
  p73 <- ggplot(best_ensemble, aes(x = years, y = mean_f)) +
  geom_line(linewidth = 1.5, alpha = 0.45,col="red") +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  #Confidence interval for SSB_MSY/SSB0 
  geom_ribbon(aes(ymin = lower_f, ymax = upper_f), alpha = 0.35, fill = "red") +
  # Dashed lines representing confidence interval for SSB_MSY/SSB0
  #geom_hline(data = best_metrics, aes(yintercept = ssb_msy_ssb0 - 1.96 * ssb_msy_ssb0_std-0.03), 
  #           color = "red", linetype = "dotted", linewidth = 0.8) +
  #geom_hline(data = best_metrics, aes(yintercept = ssb_msy_ssb0 + 1.96 * ssb_msy_ssb0_std+0.03), 
  #           color = "red", linetype = "dotted", linewidth = 0.8) +
  # Texto do ano 2015
  geom_text(data = best_ensemble %>%
            group_by(codsp) %>%
            mutate(y_max = max(c(mean_f,upper_f), na.rm = TRUE) * 0.98) %>%
            slice(1),  
            aes(x = 2015, y = y_max, label = "2015"),
            inherit.aes = FALSE,  
            vjust = 0, hjust = 0.5, size = 3) +
  #Horizontal line SSB_MSY/SSB0
  geom_hline(data = best_metrics, aes(yintercept = f_msy, linetype = "f_msy"), 
             color = "firebrick", linewidth = 1) +
  # Configurações do gráfico
  labs(x = "Year", y = expression("Relative Stock Spawning Biomass "~ " (SSB" / "SSB"[0]~")"), 
       fill = "", color = "Length type", linetype = "") +
  scale_linetype_manual(values = c("f_msy" = "solid"), 
                        labels = expression("F"[MSY])) +
  facet_wrap(~codsp, scales = "free") + 
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  p73
  
  ggsave("SS3_Ensemble_F.png", plot = p73, device = "png", units = "cm",
         width = 28, height = 17)
  
  #-------------------
  #kobe plot building
  #------------------
  #Create kobe plot
  {#.png file
  jpeg("SS3_Ensemble_kobe.png",
         width=25,height=18,
         units="cm",
         res=800,antialias = "cleartype")
  #graphical parameters  
  par(mfrow=c(3,3),mar=c(3,3,1,0.2),oma=c(1.7,2,0.5,7),xpd=F,
      mgp=c(2,1,0), cex.lab=1,cex.axis=0.8, bty="n",las=1,lwd=0.8,
      col.axis='gray30',col.lab='gray10')
  #loop through the species
  for (species in sp) {
  
  xlim=c(min(c(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species]-0.1),best_sensitivity$ssb_ssbmsy[best_sensitivity$codsp==species]-0.1),
         max(c(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species]+0.1),best_sensitivity$ssb_ssbmsy[best_sensitivity$codsp==species]+0.1))
  ylim=c(min(c(best_ensemble$f_fmsy[best_ensemble$codsp==species]-0.1),best_sensitivity$f_fmsy[best_sensitivity$codsp==species]-0.1),
         max(c(best_ensemble$f_fmsy[best_ensemble$codsp==species]+0.1),best_sensitivity$f_fmsy[best_sensitivity$codsp==species]+0.1))
    
  if (ylim[2]<1) { ylim[2]= 1.1} #make sure all quadrants are shown
  #empty plot 
  plot(1000,1000,type="b", xlim=c(0,xlim[2]),
       ylim=c(0,ylim[2]),lty=3,
       xlab="",ylab="", 
       bty="l",  cex.lab = 1, 
       cex.axis = 1,xaxs = "i",yaxs="i")
  c1 <- c(-1,100)
  c2 <- c(1,1)
  
  # and fill areas using the polygon function
  zb2 = c(0,1)
  zf2  = c(1,100)
  zb1 = c(1,100)
  zf1  = c(0,1)
  #filling quadrants
  polygon(c(zb1,rev(zb1)),c(0,0,1,1),col="#b3e2cd",border=0)  # Verde claro suave  
  polygon(c(zb2,rev(zb2)),c(0,0,1,1),col="#fff2a1",border=0)  # Amarelo pastel vibrante  
  polygon(c(1,100,100,1),c(1,1,100,100),col="#FBC4AB",border=0)  # Laranja pastel mais perceptível  
  polygon(c(0,1,1,0),c(1,1,100,100),col="#F4A3A8",border=0)  # Vermelho pastel real  
    
  #reference lines
  lines(c1,c2,lty=3,lwd=0.7)
  lines(c2,c1,lty=3,lwd=0.7)
  
  #colors and symbols for each sensitivity_type (year = 2025)
  sensitivity_colors <- c("obs" = "red", "obspred" = "green", "pred" = "blue")
  sensitivity_shapes <- c("obs" = 17, "obspred" = 17, "pred" = 17) # pch diferentes
    
  #Filtering 2025 data
  final_sensitivity <- best_sensitivity %>%
  filter(codsp == species, years == 2025)
  points(
  final_sensitivity$ssb_ssbmsy,
  final_sensitivity$f_fmsy,
  pch = sensitivity_shapes[final_sensitivity$sensitivity_type],
  col = adjustcolor(sensitivity_colors[final_sensitivity$sensitivity_type], alpha.f = 0.5),
  cex = 2
  )
  #colors and symbols for each sensitivity_type (year = 2015)
  sensitivity_colors <- c("obs" = "red", "obspred" = "green", "pred" = "blue")
  sensitivity_shapes <- c("obs" = 15, "obspred" = 15, "pred" = 15) # pch diferentes
    
  # filtering 2015 data
  final_sensitivity <- best_sensitivity %>%
  filter(codsp == species, years == 2015)
  points(
  final_sensitivity$ssb_ssbmsy,
  final_sensitivity$f_fmsy,
  pch = sensitivity_shapes[final_sensitivity$sensitivity_type],
  col = adjustcolor(sensitivity_colors[final_sensitivity$sensitivity_type], alpha.f = 0.5),
  cex = 2
  )
  
  #Ensemble line for each species  
  lines(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species],
        best_ensemble$f_fmsy[best_ensemble$codsp==species],
        col='grey30',lwd=2)
  #put the initial and the final year icons
  points(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species][1],
         best_ensemble$f_fmsy[best_ensemble$codsp==species][1],pch=19,col='grey30',cex=2)
  points(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species & best_ensemble$years==2015],
         best_ensemble$f_fmsy[best_ensemble$codsp==species& best_ensemble$years==2015],pch=15,col='grey30',cex=2)
  points(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species][length(best_ensemble$ssb_ssbmsy[best_ensemble$codsp==species])],
         best_ensemble$f_fmsy[best_ensemble$codsp==species][length(best_ensemble$f_fmsy[best_ensemble$codsp==species])],pch=17,col='grey30',cex=2)
  
  #side legend only in 6th species
  if (species %in% sp[6]== TRUE) {
  
  #Sensitivity_type Legend
  legend(
  x = grconvertX(1.05, from = "npc"),  
  y = grconvertY(0.5, from = "npc"),   
  legend = c("obs", "obspred", "pred", "ensemble"),
  pch = c(15, 15, 15, 15),
  col = c("red", "green", "blue", "grey30"),
  title = "Length type",
  bty = "n",
  pt.cex = 1.5,
  cex=1.2,
  xpd = NA
  )
  #years legend
  legend(
  x = grconvertX(1.05, from = "npc"),  
  y = grconvertY(1, from = "npc"),   
  legend = c("1950", "2015", "2025"), 
  pch = c(21, 22, 24),
  bg = "white", 
  bty = "n", 
  pt.cex = 1.5, 
  cex = 1.2,
  xjust = 0, yjust = 1,
  xpd = NA)
  }
  #Species title  
  mtext(species, side = 3, line = 0.2, cex = 0.8)
  }  
  par(las=0)
  mtext(expression(SSB/SSB[MSY]), side = 1, outer=TRUE,line = 0.4, cex = 1) #axis-lab
  mtext(expression(F/F[MSY]), side = 2, outer=TRUE,line = 0.2, cex = 1)  
  dev.off()  
  }   
  
}
#-----------------------------------------------------------------------------
  
  
  #plotting the used length data..
  #Length information(1- Observed lengths;2- Observed+predicted; 3- predicted lengths)
  
  input_length <- data.frame()
  
  for (species in sp) {
    
    #-------------------
    # Observed lengths
    #-------------------
    obs_length <- smtlenobs[smtlenobs$codsp == species, ]
    obs_length<- obs_length %>%
      dplyr::mutate(type="obs") %>%
      dplyr::select(yr,codsp,fl,source,type)
    
    # Specific filters for each species 
    if (species == "BLF") {
      obs_length <- obs_length %>% dplyr::filter(fl < 100, yr != 2017)
    } else if (species == "BRS") {
      obs_length <- obs_length %>% dplyr::filter(fl < 105)
    } else if (species == "DOL") {
      obs_length <- obs_length %>%
        dplyr::filter(fl > 30, fl < 160, !(yr %in% c(2002, 2003, 2004, 2013, 2014, 2015, 2020)))
    } else if (species == "FRI") {
      obs_length <- obs_length %>%
        dplyr::filter(fl < 60, !(yr %in% c(1991, 1995, 1997, 2006)))
    } else if (species == "KGM") {
      obs_length <- obs_length %>%
        dplyr::filter(fl < 170, !(yr %in% c(2019, 2020)))
    } else if (species == "LTA") {
      obs_length <- obs_length %>%
        dplyr::filter(!(yr %in% c(2008, 2015)))
    } else if (species == "WAH") {
      obs_length <- obs_length %>%
        dplyr::filter(fl < 200, !(yr %in% c(1986:1987, 1999:2000, 2002:2005, 2008:2010, 2013:2016)))
    }
    
    #-------------------------------------------------
    #Predicted lengths with years of supporting data
    #-------------------------------------------------
    #Years with observed mean lengths 
    years <- unique(smtml$yr[!is.na(smtml$mean) & smtml$codsp == species])
    years <- years[years != 1950]
    
    obspred_length <- smtlensim[smtlensim$yr %in% years & smtlensim$codsp == species, ]
    obspred_length<- obspred_length %>%
      dplyr::mutate(type="obspred") %>%
      dplyr::select(yr,codsp,fl,source,type)
    
    # Specific filters for each species     
    if (species == "DOL") {
      obspred_length <- obspred_length %>% dplyr::filter(fl < 200)
    } else if (species == "FRI") {
      obspred_length <- obspred_length %>% dplyr::filter(fl < 62)
    } else if (species == "KGM") {
      obspred_length <- obspred_length %>% dplyr::filter(fl < 130)
    } else if (species == "WAH") {
      obspred_length <- obspred_length %>% dplyr::filter(fl < 200)
    } %>%
      dplyr::mutate(type="obspred") 
    
    #------------------------------------
    #Fully predicted length compositions
    #------------------------------------
    pred_length <- smtlensim[smtlensim$codsp == species, ]
    pred_length<- pred_length %>%
      dplyr::mutate(type="pred") %>%
      dplyr::select(yr,codsp,fl,source,type)
    
    # Specific filters for each species 
    if (species == "DOL") {
      pred_length <- pred_length %>% dplyr::filter(fl < 200)
    } else if (species == "FRI") {
      pred_length <- pred_length %>% dplyr::filter(fl < 66)
    } else if (species == "KGM") {
      pred_length <- pred_length %>% dplyr::filter(fl < 130)
    } else if (species == "WAH") {
      pred_length <- pred_length %>% dplyr::filter(fl < 200)
    } %>%
      dplyr::mutate(type="pred")
    
    # binding data
    input_length <- rbind(input_length, obs_length, obspred_length, pred_length)
  }
    
  # Ordering factors levels
  input_length$type <- factor(input_length$type, levels = c("pred", "obspred", "obs"))
  
  #ploting lengths
  p74 <- ggplot(input_length) +
    geom_boxplot(aes(x = factor(yr), y = fl, col = type, fill = type),
                 alpha = 0.6, position = "identity", outlier.size = 0.7, width = 0.6) +
    labs(x = "Year", y = "Length (cm)", fill = "Length type", color = "Length type") +
    facet_wrap(~codsp, scales = "free") +
    scale_x_discrete(breaks = as.character(seq(min(as.numeric(as.character(input_length$yr))),
                                               max(as.numeric(as.character(input_length$yr))),
                                               by = 20))) +
    #filling colors manually
    scale_fill_manual(values = c("pred" = "#56B4E9",      
                                 "obspred" = "#009E73",   
                                 "obs" = "#D55E00")) +    
    scale_color_manual(values = c("pred" = "#56B4E9",
                                  "obspred" = "#009E73",
                                  "obs" = "#D55E00")) +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
  p74
    
  #saving...
  ggplot2::ggsave("boxplot_input_length_by_species.png",plot=p74, device = "png", units = "cm",
                  width = 30, height = 18,dpi=350)

  
#PLot Life history  used in the analysis
  
# Input Life history data
  par_ss_plot <- par_ss %>%
    dplyr::select(codsp, m, m_algaraja, m_pauly, tmax, linf, k, t0, lm50) %>%
    tidyr::pivot_longer(
      cols = c(m, m_algaraja, m_pauly, tmax, linf, k, t0, lm50),
      names_to = "par",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      par = dplyr::case_when(
        par %in% c("m", "m_algaraja", "m_pauly") ~ "M",
        par == "linf" ~ "L[infinity]",
        par == "lm50" ~ "L[50]",
        par == "t0" ~ "t[0]",
        par == "tmax" ~ "T[max]",
        TRUE ~ par
      )
    )
  
  p75 <- ggplot2::ggplot(par_ss_plot) +
    geom_point(aes(x = value, y = codsp),col="gray30",
               alpha = 0.2, size = 1.5) +
    geom_boxplot(aes(x = value, y = codsp),
                 col="gray30",
                 fill = "gray60",
                 alpha = 0.2) +
    
    facet_wrap(. ~ par, scales = "free", labeller = label_parsed) +
    labs(x = "Value", y = "Species code") +
    theme_classic(base_size = 14) %+replace%
    theme(
      strip.background = element_blank(),
      plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm")
    )
  p75
  
  #saving...
  ggplot2::ggsave("boxplot_input_life_history_by_species.png",plot=p75, device = "png", units = "cm",
                  width = 30, height = 18,dpi=350)  
 
  
  #---------------------------------------------------------------------------------------------#
  #              Taking the best metrics and reference points for all stocks                    #
  #               Catch2015, Catch2025, CatchMSY, SSB2015, SSB2025, SSBMSY....                  #
  #---------------------------------------------------------------------------------------------#

  #reading the output data...
  par_ss<-read.csv("par_ss.csv", sep=",", dec=".") #life history used
  
  #-------
  jitter_out<- read.csv("jitter_out.csv",sep = ",",dec = ".") #best models of jitter
  #Filtering the best models
  #-- Fishing mortality not stable around 0
  #-- Initial Recruitment LNR0 >=1 or <=9
  #-- Stock Spawning Biomass not collapsed <0.1
  #-- Relative Spawning Biomass SSB/SSB0 not collapsed <0.01
  best_jitter <- jitter_out %>%
    dplyr::mutate(across(where(is.list), ~ unlist(.))) %>%
    dplyr::group_by(codsp) %>% 
    dplyr::mutate(all_negative_nll = all(total_nll < 0)) %>%  #Verifiyng if all NLL are negative
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%  #Conditions row by row
    dplyr::mutate(remove_scenario = 
                    (f_1980 < 0.01 | f_1990 < 0.01 | f_2000 < 0.01 | f_2010 < 0.01 | 
                     lnr0_est < 1 | lnr0_est > 9 | 
                     ssb_1980 < 0.1 | ssb_1990 < 0.1 | ssb_2000 < 0.1 | ssb_2010 < 0.1 |
                     ssb_ssb0_1980 < 0.01 | ssb_ssb0_1990 < 0.01 | ssb_ssb0_2000 < 0.01 | ssb_ssb0_2010 < 0.01)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(remove_scenario & jittering_par == "all"))# %>%  #removing bad scenarios

  #---------
  best_jitter_directories<- read.csv("best_jitter_directories.csv", sep=",",dec = ".") #best models directory
  #---------
  
  #average life history input and best parameters (jitter analysis)
  #function to extract the best and average parameters from the directory
  extract_params <- function(dir_string, param_positions) {
    params <- strsplit(dir_string, "_")[[1]]
    sapply(param_positions, function(i) as.numeric(params[i]))
  }
  #Parameters data frame
  best_avg_params <- data.frame()
  for (species in sp) {
    par_species <- best_jitter_directories %>% dplyr::filter(codsp == species)
    #Extracting parameters
    avg_params <- extract_params(par_species$dir_avg, 2:8)
    best_params <- extract_params(par_species$dir_best, 2:8)
    #average parameters for each species
    avg_data<- data.frame(codsp=species,
                          type="avg",
                          m=avg_params[1], 
                          linf=avg_params[2],
                          k=avg_params[3],
                          sl95=avg_params[4],
                          sl95_est=best_jitter$sl95est[best_jitter$codsp==species &
                                   best_jitter$m==avg_params[1] & best_jitter$linf==avg_params[2] & best_jitter$k==avg_params[3] &
                                   best_jitter$sl95==avg_params[4] & best_jitter$ascendse==avg_params[5] & best_jitter$lnr0==avg_params[6] &
                                   best_jitter$tmax==avg_params[7] & best_jitter$jittering_par=="avg"],
                          ascendse=avg_params[5], 
                          ascendse_est=best_jitter$ascendse_est[best_jitter$codsp==species &
                                       best_jitter$m==avg_params[1] & best_jitter$linf==avg_params[2] & best_jitter$k==avg_params[3] &
                                       best_jitter$sl95==avg_params[4] & best_jitter$ascendse==avg_params[5] & best_jitter$lnr0==avg_params[6] &
                                       best_jitter$tmax==avg_params[7] & best_jitter$jittering_par=="avg"],
                          lnr0=avg_params[6],
                          lnr0_est=best_jitter$lnr0_est[best_jitter$codsp==species &
                                   best_jitter$m==avg_params[1] & best_jitter$linf==avg_params[2] & best_jitter$k==avg_params[3] &
                                   best_jitter$sl95==avg_params[4] & best_jitter$ascendse==avg_params[5] & best_jitter$lnr0==avg_params[6] &
                                   best_jitter$tmax==avg_params[7] & best_jitter$jittering_par=="avg"],
                          tmax=avg_params[7],
                          nll=best_jitter$total_nll[best_jitter$codsp==species &
                              best_jitter$m==avg_params[1] & best_jitter$linf==avg_params[2] & best_jitter$k==avg_params[3] &
                              best_jitter$sl95==avg_params[4] & best_jitter$ascendse==avg_params[5] & best_jitter$lnr0==avg_params[6] &
                              best_jitter$tmax==avg_params[7] & best_jitter$jittering_par=="avg"])
    #best parameters for each species (from jitter)
    best_data<- data.frame(codsp=species, 
                           type="bst",
                           m=best_params[1], 
                           linf=best_params[2],
                           k=best_params[3],
                           sl95=best_params[4],
                           sl95_est=best_jitter$sl95est[best_jitter$codsp==species &
                                    best_jitter$m==best_params[1] & best_jitter$linf==best_params[2] & best_jitter$k==best_params[3] &
                                    best_jitter$sl95==best_params[4] & best_jitter$ascendse==best_params[5] & best_jitter$lnr0==best_params[6] &
                                    best_jitter$tmax==best_params[7] & best_jitter$jittering_par=="all"],
                           ascendse=best_params[5], 
                           ascendse_est=best_jitter$ascendse_est[best_jitter$codsp==species &
                                        best_jitter$m==best_params[1] & best_jitter$linf==best_params[2] & best_jitter$k==best_params[3] &
                                        best_jitter$sl95==best_params[4] & best_jitter$ascendse==best_params[5] & best_jitter$lnr0==best_params[6] &
                                        best_jitter$tmax==best_params[7] & best_jitter$jittering_par=="all"],
                           lnr0=best_params[6],
                           lnr0_est=best_jitter$lnr0_est[best_jitter$codsp==species &
                                    best_jitter$m==best_params[1] & best_jitter$linf==best_params[2] & best_jitter$k==best_params[3] &
                                    best_jitter$sl95==best_params[4] & best_jitter$ascendse==best_params[5] & best_jitter$lnr0==best_params[6] &
                                    best_jitter$tmax==best_params[7] & best_jitter$jittering_par=="all"],
                           tmax=best_params[7],
                           nll=best_jitter$total_nll[best_jitter$codsp==species &
                               best_jitter$m==best_params[1] & best_jitter$linf==best_params[2] & best_jitter$k==best_params[3] &
                               best_jitter$sl95==best_params[4] & best_jitter$ascendse==best_params[5] & best_jitter$lnr0==best_params[6] &
                               best_jitter$tmax==best_params[7] & best_jitter$jittering_par=="all"])
    best_avg_params<- rbind(best_avg_params,avg_data,best_data)
  }
  best_avg_params<- best_avg_params %>% 
  dplyr::mutate(
  dplyr::across(where(is.numeric), ~round(.x, 2))
  ) 
  
  #write results in a .csv
  write.csv(best_avg_params, file = file.path(dir, "best_avg_params_out.csv"), row.names = FALSE)
  
  
  #--------
  ensemble_out<- read.csv("ensemble_out.csv",sep = ",",dec = ".") #ensemble of models
  #---------
  
  #--------------------------
  #Management Metrics output
  #-------------------------
  management_metrics<- data.frame()
  #loop through all species
  for (species in sp) {
    
  metrics<-data.frame(
    codsp= ensemble_out$codsp[ensemble_out$codsp==species][1],
    minyr= ensemble_out$years[ensemble_out$codsp==species][1],
    maxyr= ensemble_out$years[ensemble_out$codsp==species][length(ensemble_out$years[ensemble_out$codsp==species])],
    catch_2015=read.csv(file.path(dir, paste(species, "_catch.csv", sep = "")), sep = ",", dec = ".") %>%
      dplyr::filter(Year == 2015) %>%
      dplyr::pull(Freire),
    catch_2025=read.csv(file.path(dir, paste(species, "_catch.csv", sep = "")), sep = ",", dec = ".") %>%
      dplyr::filter(Year == 2025) %>%
      dplyr::pull(Freire),
    catch_msy=ensemble_out$catch_msy[ensemble_out$codsp==species][1],
    f_2015= ensemble_out$mean_f[ensemble_out$codsp==species & ensemble_out$years==2015], 
    f_2025= ensemble_out$mean_f[ensemble_out$codsp==species & ensemble_out$years==2025],
    f_msy=ensemble_out$f_msy[ensemble_out$codsp==species][1],
    f_2015_fmsy=ensemble_out$mean_f[ensemble_out$codsp==species & ensemble_out$years==2015]/ensemble_out$f_msy[ensemble_out$codsp==species][1],
    f_2025_fmsy=ensemble_out$mean_f[ensemble_out$codsp==species & ensemble_out$years==2025]/ensemble_out$f_msy[ensemble_out$codsp==species][1], 
    ssb_2015= ensemble_out$mean_ssb[ensemble_out$codsp==species & ensemble_out$years==2015],
    ssb_2025=ensemble_out$mean_ssb[ensemble_out$codsp==species & ensemble_out$years==2025] ,
    ssb_msy= ensemble_out$ssb_msy[ensemble_out$codsp==species][1],
    ssb_2015_ssbmsy=ensemble_out$mean_ssb[ensemble_out$codsp==species & ensemble_out$years==2015]/ensemble_out$ssb_msy[ensemble_out$codsp==species][1],
    ssb_2025_ssbmsy= ensemble_out$mean_ssb[ensemble_out$codsp==species & ensemble_out$years==2025]/ensemble_out$ssb_msy[ensemble_out$codsp==species][1])
  #status definition  
  metrics <- metrics %>%
    dplyr::mutate(
    status_2015 = dplyr::case_when(
      f_2015 < f_msy & ssb_2015 > ssb_msy ~ "Underfished",
      f_2015 > f_msy & ssb_2015 > ssb_msy ~ "Overfishing",
      f_2015 > f_msy & ssb_2015 < ssb_msy ~ "Overfished & Overfishing",
      f_2015 < f_msy & ssb_2015 < ssb_msy ~ "Overfished",
      TRUE ~ NA_character_  #avoiding error by NA values
    ),
    status_2025 = dplyr::case_when(
      f_2025 < f_msy & ssb_2025 > ssb_msy ~ "Underfished",
      f_2025 > f_msy & ssb_2025 > ssb_msy ~ "Overfishing",
      f_2025 > f_msy & ssb_2025 < ssb_msy ~ "Overfished & Overfishing",
      f_2025 < f_msy & ssb_2025 < ssb_msy ~ "Overfished",
      TRUE ~ NA_character_
    )
  )
  #bind rows for all species
  management_metrics<- rbind(management_metrics,metrics)
  }
                            
  #rounding     
  management_metrics <- management_metrics %>% 
    dplyr::mutate(across(where(is.numeric), ~round(.x, 2)))
 
  #write results in a .csv
  write.csv(management_metrics, file = file.path(dir, "management_metrics_out.csv"), row.names = FALSE)
  
  
            #x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#xx#x#xx#x#xx#x#x#x#x#x#
            #.................. End of  Small tunas stock assessment.................. #
            #x#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#x#x#xx#x#x#x#x#x