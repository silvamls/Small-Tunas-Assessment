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
#install.packages("purrr")
library(purrr)
#install.packages('ggplot2')
library(ggplot2)
# the development version from github LBSPR
#install.packages("devtools") #you need the devtools to install the LBSPR
#devtools::install_github("AdrianHordyk/LBSPR")
library(LBSPR)
#install.packages("TropFishR")
library(TropFishR)
# The package must be downloaded from: https://github.com/James-Thorson-NOAA/FishLife#
#devtools::install_local("C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas/FishLife-main")
library(FishLife)
#---------------------------------------- Machine Learning packages --------------------------------------------------#
#Python must be installed. version:3.11 is compatible with tensorflow: https://www.python.org/downloads/release/python-3119/
#Click in "Windows installer (64-bit)" and select "add Python 3.11 to PATH" to be used in any terminal
#Now install Keras ...
#install.packages("keras") 
#library(keras)
#install_keras(method = "virtualenv", python_version = "3.11") #now you can install KERAS API in R
#testing 
library(keras)
to_categorical(0:3)
#tensorflow (neural networks back-end)
library(tensorflow)
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
#install.packages("lmerTest")
library(lmerTest)
#install.packages("splines")
library(splines)
#install.packages("devtools") #you need the devtools to install ss3diags
#devtools::install_github("jabbamodel/ss3diags",force = TRUE)
library(ss3diags)

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
  # DOL Dolhpin fish (Coryphaena hippurus)
  #--------------------------------------------------------#
  
  #species with avaliable length and catch data for AT-SW
  sp<- c("BLF", "BRS", "DOL", "FRI", "KGM", "LTA","WAH")
  
# ------------------- Life History Exploratory Analysis --------------------------#

unique(smtlh$specie) #list of available life history species data
table(smtlh$specie, smtlh$stock)  

#boxplot for the main life history parameters
#factored data set (pivot-longer)
smtlh_long <- smtlh %>%
  dplyr::filter(codsp %in% sp) %>%
  dplyr::select(specie, codsp, source, stock,
         linf, k, t0, t0_pauly,
         tmax, tmax_taylor,
         m, m_algaraja, m_pauly,
         lm50) %>%
  # pivot for long while maintaining all variants
  pivot_longer(
    cols = c(linf, k, t0, t0_pauly,
             tmax, tmax_taylor,
             m, m_algaraja, m_pauly,
             lm50),
    names_to = "par_variant",
    values_to = "value"
  ) %>%
  # now we aggregate variants under general parameters
  mutate(
    par = dplyr::case_when(
      par_variant %in% c("linf")                    ~ "linf",
      par_variant %in% c("k")                       ~ "k",
      par_variant %in% c("t0", "t0_pauly")          ~ "t0",
      par_variant %in% c("tmax", "tmax_taylor")     ~ "tmax",
      par_variant %in% c("m", "m_algaraja", "m_pauly") ~ "m",
      par_variant %in% c("lm50")                    ~ "lm50",
      TRUE ~ par_variant
    ),
    # transform codsp into a factor to sort on the y-axis
    codsp = factor(codsp)
  ) %>%
  mutate(
    value = case_when(
      par == "m"  & value > 3 ~ NA_real_,  # cut M > 3
      par == "k"  & value > 4 ~ NA_real_,  # cut k > 4
      TRUE ~ value
    )
  )  %>%
  mutate( #ordering factors
    par = factor(
      par,
      levels = c("linf", "k", "t0", "m", "tmax", "lm50")
    )
  )

# secure parsable labels (use infty)
named_labels <- c(
  linf = "L[infinity]",
  k = "k",
  t0 = "t[0]",
  tmax = "T[max]",
  m = "M",
  lm50 = "L[50]"
)

# plot: a panel per aggregated pair (pair); points still show all variants
p1 <- ggplot(smtlh_long, aes(x = value, y = codsp)) +
  geom_boxplot(aes(x = value, y = codsp),
               outlier.shape = NA,
               color = "gray30", fill = "gray85", alpha = 0.3, width = 0.6) +
  geom_point(
    aes(color = stock),
    position = position_jitter(height = 0.15),
    size = 1.5,
    alpha = 0.6
  )+
  facet_wrap(~ par, scales = "free_x",
             labeller = labeller(par = as_labeller(named_labels, label_parsed)),
             ncol = 2) +
  labs(x = "Value", y = "Species code") +
  theme_classic(base_size = 13) +
  scale_color_manual(values = c(
    "AT-NE" = "#D73027",  # strong red
    "AT-NW" = "#4575B4",  # middle blue
    "AT-SE" = "#1A9850",  # strong green
    "AT-SW" = "#762A83",  # dark purple
    "MD"    = "#E6AB02"   # dark yellow 
  ))+
  theme(
    # spacing between panels
    panel.spacing = unit(0.6, "lines"),
    strip.background = element_blank(),
    # TEXT SIZES -----------------------------
    axis.text.x = element_text(size = 11),   # X-axis text size
    axis.text.y = element_text(size = 11),   # Y-axis text size
    axis.title.x = element_text(size = 13),  # X-axis title
    axis.title.y = element_text(size = 13),  # Y-axis title
    
    strip.text = element_text(size = 14), #facet text
    legend.position = "bottom"
  )
p1

#saving...
ggplot2::ggsave("boxplot_input_life_history_by_species.png",plot=p1, device = "png", units = "cm",
                width = 17, height = 18,dpi=350)  


#-----------------------------------------
# Comparing growth curves for each species
# plausible values for M and L50
#----------------------------------------

# Calculate average tmax by species (only valid values)
tmax_means <- smtlh %>%
  dplyr::filter(codsp %in% sp, !is.na(tmax), tmax > 0) %>%
  dplyr::group_by(codsp) %>%
  dplyr::summarise(tmax_mean = ceiling(mean(tmax, na.rm = TRUE)), .groups = "drop")

# Data preparation now using average tmax per species
smtlh_for_curves <- smtlh %>%
  dplyr::filter(codsp %in% sp) %>%
  dplyr::select(specie, codsp, stock, source, source_label,
         year = startyr, linf, k, t0, tmax) %>%
  dplyr::filter(!is.na(linf) & !is.na(k) & !is.na(t0)) %>%
  left_join(tmax_means, by = "codsp") %>%
  mutate(
    row_id = row_number(),
    # Maximum age definition rule:
    # 1. Use the row's tmax if it exists and is > 0
    # 2. Otherwise, use the species' tmax_mean
    # 3. Otherwise, use a fallback (example: 15 years)
    max_age = case_when(
      !is.na(tmax) & tmax > 0 ~ as.numeric(tmax),
      !is.na(tmax_mean)       ~ as.numeric(tmax_mean),
      TRUE                    ~ 15  # fallback 
    )
  )

#For each row/combo, generate a sequence of ages and calculate L(t)
# Using purrr::pmap_dfr to generate a "long" data frame with all the curves
smtlh_growth_curves <- purrr::pmap_dfr(
  list(
    specie = smtlh_for_curves$specie,
    codsp  = smtlh_for_curves$codsp,
    stock  = smtlh_for_curves$stock,
    source = smtlh_for_curves$source,
    source_label = smtlh_for_curves$source_label,
    row_id = smtlh_for_curves$row_id,
    linf = smtlh_for_curves$linf,
    k = smtlh_for_curves$k,
    t0 = smtlh_for_curves$t0,
    max_age = smtlh_for_curves$max_age
  ),
  function(specie, codsp, stock, source, source_label, row_id, linf, k, t0, max_age) {
    ages <- seq(t0, max_age, by = 0.1)        # Resolution 0.1 years (adjust if desired)
    length_t <- linf * (1 - exp(-k * (ages - t0))) # von Bertalanffy L(t)
    # Optional: Avoid negative values for very young ages if t0 is positive.
    length_t <- pmax(length_t, 0)
    tibble(
      specie = specie,
      codsp  = codsp,
      stock  = stock,
      source = source,
      source_label = source_label,
      combo_id = paste0(codsp, "_", source, "_", row_id),
      linf = linf,
      k = k,
      t0 = t0,
      age = ages,
      length = length_t
    )
  }
)

# inspecionate results
dplyr::glimpse(smtlh_growth_curves)
dplyr::count(smtlh_growth_curves, codsp)  # lines per species (curves x ages)

#Plot: all curves by species (colors by source)
p2<-ggplot(smtlh_growth_curves, 
       aes(x = age, y = length, group = combo_id, color = stock)) +
  geom_line(alpha = 0.4, linewidth=1) +
  facet_wrap(~codsp, scale="free")+
  labs(y = "Length (cm)", x = "Age (years)") +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    # spacing between panels
    panel.spacing = unit(0.6, "lines"),
    strip.background = element_blank(),
    # TEXT SIZES -----------------------------
    axis.text.x = element_text(size = 11),   # X-axis text size
    axis.text.y = element_text(size = 11),   # Y-axis text size
    axis.title.x = element_text(size = 13),  # X-axis title
    axis.title.y = element_text(size = 13),  # Y-axis title
    strip.text = element_text(size = 11) #facet text
  )
p2

#saving...
ggplot2::ggsave("growth_curves_by_species.png",plot=p2, device = "png", units = "cm",
                width = 19, height = 18,dpi=350)  

#-----------------------------------------------
#Evaluating natural Mortality (M) input methods
#-----------------------------------------------
M_long <- smtlh %>%
  dplyr::filter(codsp %in% sp) %>%
  dplyr::select(specie, codsp, stock,
         m, m_algaraja, m_pauly) %>%
  pivot_longer(
    cols = c(m, m_algaraja, m_pauly),
    names_to = "M_method",
    values_to = "M_value"
  ) %>%
  dplyr::filter(!is.na(M_value))


#plot all M estimates (Combining m, m_algaraja, m_pauly)
p3 <- ggplot(M_long, aes(x = M_value, y = codsp, colour = stock)) +
  geom_jitter(height = 0.15, alpha = 0.6, size = 2) +
  labs(
    x = "Natural mortality (M, all methods combined)",
    y = "Species code",
    colour = "Stock" ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "bottom"
  )

p3

#saving...
ggplot2::ggsave("M_estimates_by_species.png",plot=p3, device = "png", units = "cm",
                width = 19, height = 18,dpi=350)  


#------------------------------------------
#Evaluating maturity length (L50) by stock
#------------------------------------------
L50_long <- smtlh %>%
  dplyr::filter(codsp %in% sp) %>%
  dplyr::select(specie, codsp, stock,lm50) %>%
  dplyr::filter(!is.na(lm50))


#plot all L50 estimates 
p4 <- ggplot(L50_long, aes(x = lm50, y = codsp, colour = stock)) +
  geom_jitter(height = 0.15, alpha = 0.6, size = 2) +
  labs(
    x = "Maturity Length (cm)",
    y = "Species code",
    colour = "Stock" ) +
  theme_classic(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.position = "bottom"
  )

p4

#saving...
ggplot2::ggsave("L50_estimates_by_species.png",plot=p4, device = "png", units = "cm",
                width = 19, height = 18,dpi=350)  


                  #------------------------------------------------------------------------------------#
                  #                 Estimating steepness (h) parameter from Fishlife                   #
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

#NUll column of steepness (h)
smtml$h<- NA

#BLF- Blackfin tuna- Thunnus atlanticus
blf_h<-Plot_taxa(Search_species(Genus="Thunnus",Species="atlanticus")$match_taxonomy)[[3]]
blf_h<-blf_h$Mean_pred[names(blf_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="BLF"]<- blf_h
#------------------------------------------------------------------------------------

#BRS serra Spanish mackerel-  Scomberomorus brasiliensis
brs_h<-Plot_taxa(Search_species(Genus="Scomberomorus",Species="brasiliensis")$match_taxonomy)[[3]]
brs_h<-brs_h$Mean_pred[names(brs_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="BRS"]<- brs_h
#------------------------------------------------------------------------------------

#DOL Dolphin Fish-  Coryphaena hippurus
dol_h<-Plot_taxa(Search_species(Genus="Coryphaena",Species="hippurus")$match_taxonomy)[[3]]
dol_h<-dol_h$Mean_pred[names(dol_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="DOL"]<- dol_h
#------------------------------------------------------------------------------------

#FRI frigate tuna- Auxis thazard
fri_h<-Plot_taxa(Search_species(Genus="Auxis",Species="thazard")$match_taxonomy)[[3]]
fri_h<-fri_h$Mean_pred[names(fri_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="FRI"]<- fri_h
#------------------------------------------------------------------------------------

# KGM king mackerel- Scomberomorus cavalla
kgm_h<-Plot_taxa(Search_species(Genus="Scomberomorus",Species="cavalla")$match_taxonomy)[[3]]
kgm_h<-kgm_h$Mean_pred[names(kgm_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="KGM"]<- kgm_h
#------------------------------------------------------------------------------------

# LTA little tunny- Euthynnus alletteratus
lta_h<-Plot_taxa(Search_species(Genus="Euthynnus",Species="alletteratus")$match_taxonomy)[[3]]
lta_h<-lta_h$Mean_pred[names(lta_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="LTA"]<- lta_h
#------------------------------------------------------------------------------------

# WAH wahoo- Acanthocybium solandri
wah_h<-Plot_taxa(Search_species(Genus="Acanthocybium",Species="solandri")$match_taxonomy)[[3]]
wah_h<-wah_h$Mean_pred[names(wah_h$Mean_pred)=="h"]
#adding to the original data frame       
smtlh$h[smtlh$codsp=="WAH"]<- wah_h
#----------------------------------------------------------------------------------------------


                            #X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#
                            #Estimating Selectivity (SL50, SL95) via Catch-Curve# 
                            #                Selection Ogive                    # 
                            #X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#X#


# Selectivity from the selection ogive - catch curve (TropfishR)
# Selectivity from the LBSPR fit function
lf <- function(x, by = 5){ 
  h <- hist(x,
            breaks = seq(from = min(x) - by,
                         to = max(x) + by,
                         by = by),
            plot = FALSE)
  data.frame(mids = h$mids, counts = h$counts)
}

sel_catchcurve <- function(x, linf, k, t0, by_values) {
  
  require(TropFishR)
  
  for (by in by_values) {
    
    h <- tryCatch(lf(x, by), error = function(e) NULL)
    if (is.null(h)) next
    
    cc_input <- list(
      midLengths = round(h$mids, 1),
      Linf = linf,
      K = k,
      t0 = t0,
      catch = h$counts
    )
    
    cc <- tryCatch(
      TropFishR::catchCurve(cc_input, auto = TRUE, calc_ogive = TRUE),
      error = function(e) NULL
    )
    
    if (is.null(cc)) next
    
    # Extrair SL50 e SL95 com segurança
    sl50 <- cc$L50
    sl95 <- cc$L95
    
    # Checagens biológicas mínimas
    if (is.na(sl50) || is.na(sl95)) next
    if (sl50 < min(h$mids) || sl95 > max(h$mids)) next
    if (sl95 <= sl50) next
    
    return(list(sl50 = sl50, sl95 = sl95))
  }
  
  # Se nenhum by funcionar
  return(list(sl50 = NA, sl95 = NA))
}


#--------------------------------
#fitting to the observed lengths
#--------------------------------
sl_obs <- data.frame(
  yr = integer(),
  region = character(),
  codsp = character(),
  codgr = character(),
  source = character(),
  sl50 = numeric(),
  sl95 = numeric()
)
years<- unique(smtlenobs$yr) 
region<- unique(smtlenobs$region) 
codsp<- sp #list with avaliable information 
codgr<- unique(smtlenobs$codgr) 
source<- unique(smtlenobs$source)


for (i in years) {
  for (j in region) {
    for (k in codsp) {
      for (l in codgr) {
        for (m in source) {
          
          len <- smtlenobs$fl[
            smtlenobs$yr == i &
              smtlenobs$region == j &
              smtlenobs$codsp == k &
              smtlenobs$codgr == l &
              smtlenobs$source == m
          ]
          
          if (length(len) > 12) {
            
            res <- sel_catchcurve(
              x = len,
              linf = mean(smtlh$linf[smtlh$codsp == k & smtlh$stock=="AT-SW"], na.rm = TRUE),
              k    = mean(smtlh$k[smtlh$codsp == k & smtlh$stock=="AT-SW"], na.rm = TRUE),
              t0   = mean(c(smtlh$t0[smtlh$codsp == k & smtlh$stock=="AT-SW"], smtlh$t0_pauly[smtlh$codsp == k & smtlh$stock=="AT-SW"]), na.rm = TRUE),
              by_values = c(1, 2, 3, 4, 5)
            )
            
            sl_obs <- rbind(
              sl_obs,
              data.frame(
                yr = i,
                region = j,
                codsp = k,
                codgr = l,
                source = m,
                sl50 = res$sl50,
                sl95 = res$sl95
              )
            )
          }
        }
      }
    }
  }
}

# mean SL50 and SL95
sl_mean <- sl_obs %>%
  dplyr::group_by(codsp) %>%
  dplyr::summarise(
    sl50 = mean(sl50, na.rm = TRUE),
    sl95 = mean(sl95, na.rm = TRUE)
  )
 
#binding in the life history data frame
smtlh$sl50<- NA
smtlh$sl95<- NA

smtlh$sl50[smtlh$codsp=="BLF" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="BLF"]
smtlh$sl50[smtlh$codsp=="BRS" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="BRS"]
smtlh$sl50[smtlh$codsp=="DOL" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="DOL"]
smtlh$sl50[smtlh$codsp=="FRI" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="FRI"]
smtlh$sl50[smtlh$codsp=="KGM" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="KGM"]
smtlh$sl50[smtlh$codsp=="LTA" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="LTA"]
smtlh$sl50[smtlh$codsp=="WAH" & smtlh$stock=="AT-SW"]<- sl_mean$sl50[sl_mean$codsp=="WAH"]

smtlh$sl95[smtlh$codsp=="BLF" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="BLF"]
smtlh$sl95[smtlh$codsp=="BRS" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="BRS"]
smtlh$sl95[smtlh$codsp=="DOL" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="DOL"]
smtlh$sl95[smtlh$codsp=="FRI" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="FRI"]
smtlh$sl95[smtlh$codsp=="KGM" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="KGM"]
smtlh$sl95[smtlh$codsp=="LTA" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="LTA"]
smtlh$sl95[smtlh$codsp=="WAH" & smtlh$stock=="AT-SW"]<- sl_mean$sl95[sl_mean$codsp=="WAH"]

#-----------------------------------------------------------
#writing the final csv life history of small tunas species
#-----------------------------------------------------------
smtlh_ss<-smtlh
write.table(
  x = smtlh_ss,                   
  file = "smtlh_ss.csv",          
  append = FALSE,                         
  dec = ".",                              
  sep = ",",                              
  row.names = FALSE,                      
  col.names = TRUE                        
)



#-------------------------------------------------------------------#
#                   Catch file  and length files                    #
#-------------------------------------------------------------------#

   #-----------------------------------------------------------------
   # 1- Catch data file (Freire et al., 2021)- Reconstructed catches
   #-----------------------------------------------------------------
#correcting the anormal peak in WAH catches (1988)
smtct$catch[smtct$type=="FREIRE" & smtct$year==1988 & smtct$species=="WAH"]= mean(
  smtct$catch[smtct$type=="FREIRE" & smtct$year==1987 & smtct$species=="WAH"],
  smtct$catch[smtct$type=="FREIRE" & smtct$year==1989 & smtct$species=="WAH"]
)  

 p5 <- ggplot(data = filter(smtct,type=="FREIRE", year<=2015) )+
         geom_line(aes(x=year,y=catch/1000),linewidth=1)+
         facet_wrap(.~species, scales = "free") +
         labs(x = "Year", y = "Catch (1000 t)", caption = "AT-SW Stock") +
         scale_fill_viridis_d() +
         scale_color_viridis_d() +
         theme_classic(base_size = 14) %+replace% 
         theme(strip.background = element_blank(),
               plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"),
               legend.position = "right")
  p5

ggplot2::ggsave("Catch_History.png",plot=p5, device = "png", units = "cm",
                                                    width = 28, height = 17)
  
  
  
                              #---------------------------------------------------------------#
                              #        Machine learning algorithm to predict catches          #
                              #Long-short term memory- LSTM to predict catch from 2015 to 2025#
                              #      Recurrent Neural Network- RNN- Specifically LSTM RNN     #
                              #---------------------------------------------------------------#
  
  #Python must be installed. version:3.11 is compatible with tensorflow: https://www.python.org/downloads/release/python-3119/
  #Click in "Windows installer (64-bit)" and select "add Python 3.11 to PATH" to be used in any terminal
  #Now install Keras ...
  #install.packages("keras") 
  #library(keras)
  #install_keras(method = "virtualenv", python_version = "3.11") #now you can install KERAS API in R
  #testing 
  library(keras)
  to_categorical(0:3)
  #tensorflow (neural networks back-end)
  library(tensorflow)
  #packages for parallel processing---
  #install.packages("foreach")
  #install.packages("doParallel")
  library(gridExtra)
  library(tensorflow)
  library(foreach)
  library(doParallel)
  
  #=================================================
  # Trying LSTM models to forecast catch data 
  #=================================================

  #------
  # BLF
  #------
  ct<-smtct$catch[smtct$species=="BLF" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="BLF" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
  p6 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
    labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # R2 x batch_size
  p7 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
    labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # R2 x epochs
  p8 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
    labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # R2 x units
  p9 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
    labs(x = "Units", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))

  # R2 x optimizer
  p10 <- ggplot(data = lstm_models) +
    geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                     y = r2, group = interaction(factor(optimizer), factor(model)))) +
    labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
    scale_fill_viridis_d() +
    scale_color_viridis_d() +
    theme_classic(base_size = 14) +
    theme(strip.background = element_blank(),
          plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
  
  # Combining the plots
  p11<-grid.arrange(p6, p7, p8, p9, p10, ncol = 3)
  
  ggplot2::ggsave("Lstm_models_comparison_BLF.png",plot=p11, device = "png", units = "cm",
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
  
  p12 <- ggplot(plot_data, aes(x = yr)) +
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
  p12
  
  ggplot2::ggsave("Lstm_fit_BLF.png",plot=p12, device = "png", units = "cm",
                                                                  width = 28, height = 16)
  
  #assign the sp data-----
  best_BLF<- best
  lstm_pred_BLF<- lstm_pred
  #-------------------------
  
  
  #==============
  #      BRS    #
  #==============
  ct<-smtct$catch[smtct$species=="BRS" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="BRS" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
    p13 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p14 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p15 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p16 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p17 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p18<-grid.arrange(p13, p14, p15, p16, p17, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_BRS.png",plot=p18, device = "png", units = "cm",
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
  
  p19 <- ggplot(plot_data, aes(x = yr)) +
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
  p19
  
  ggplot2::ggsave("Lstm_fit_BRS.png",plot=p19, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_BRS<- best
  lstm_pred_BRS<- lstm_pred
  #-------------------------
  
  
  
  
  #==============
  #      DOL    #
  #==============
  ct<-smtct$catch[smtct$species=="DOL" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="DOL" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
    p20 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p21 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p22 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p23 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p24 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p25<-grid.arrange(p20, p21, p22, p23, p24, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_DOL.png",plot=p25, device = "png", units = "cm",
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
  
  p26 <- ggplot(plot_data, aes(x = yr)) +
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
  p26
  
  ggplot2::ggsave("Lstm_fit_DOL.png",plot=p26, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_DOL<- best
  lstm_pred_DOL<- lstm_pred
  #-------------------------
  
  
  
  
  #==============
  #      FRI    #
  #==============
  ct<-smtct$catch[smtct$species=="FRI" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="FRI" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
    p27 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p28 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p29 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p30 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p31 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p32<-grid.arrange(p27, p28, p29, p30, p31, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_FRI.png",plot=p32, device = "png", units = "cm",
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
  
  p33 <- ggplot(plot_data, aes(x = yr)) +
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
  p33
  
  ggplot2::ggsave("Lstm_fit_FRI.png",plot=p33, device = "png", units = "cm",
                                                        width = 28, height = 16)
  
  #assign the sp data------
  best_FRI<- best
  lstm_pred_FRI<- lstm_pred
  #-------------------------
  
  
  
  #==============
  #      KGM    #
  #==============
  ct<-smtct$catch[smtct$species=="KGM" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="KGM" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
    
    ggplot2::ggsave("Lstm_models_comparison_KGM.png",plot=p39, device = "png", units = "cm",
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
                      mae=0.21,     
                      rmse=383.32,
                      r2=0.89,
                      look_back=4,
                      batch_size=32,
                      epochs= 50,
                      units= 100, 
                      dropout=0.3,
                      optimizer="rmsprop")
  }
  
  nit=10
  pred_out <- vector("list", nit)
  forecast_yr<- 10 #number of years to forecast 2015:2025
  window_size<- 5#number of years looking back to make the forecast
  sample_yr<- 4 #number of years to be sampled from the catch data
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
  
  ggplot2::ggsave("Lstm_fit_KGM.png",plot=p40, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_KGM<- best
  lstm_pred_KGM<- lstm_pred
  #-------------------------
  
  
  
    
  #==============
  #      LTA    #
  #==============
  ct<-smtct$catch[smtct$species=="LTA" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="LTA" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
    p41 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p42 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p43 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p44 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p45 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p46<-grid.arrange(p41, p42, p43, p44, p45, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_LTA.png",plot=p46, device = "png", units = "cm",
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
  
  p47 <- ggplot(plot_data, aes(x = yr)) +
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
  p47
  
  ggplot2::ggsave("Lstm_fit_LTA.png",plot=p47, device = "png", units = "cm",
                  width = 28, height = 16)
  
  #assign the sp data------
  best_LTA<- best
  lstm_pred_LTA<- lstm_pred
  #-------------------------
  
  
  
  #==============
  #      WAH    #
  #==============
  ct<-smtct$catch[smtct$species=="WAH" & smtct$type=="FREIRE" & smtct$year<=2015] 
  yr<-smtct$year[smtct$species=="WAH" & smtct$type=="FREIRE"& smtct$year<=2015]
  
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
    p48 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(look_back), fill = factor(model), y = r2, group = interaction(factor(look_back), factor(model)))) +
      labs(x = "Look_back", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x batch_size
    p49 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(batch_size), fill = factor(model), y = r2, group = interaction(factor(batch_size), factor(model)))) +
      labs(x = "Batch_size", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x epochs
    p50 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(epochs), fill = factor(model), y = r2, group = interaction(factor(epochs), factor(model)))) +
      labs(x = "Epochs", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x units
    p51 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(units), fill = factor(model), y = r2, group = interaction(factor(units), factor(model)))) +
      labs(x = "Units", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # R2 x optimizer
    p52 <- ggplot(data = lstm_models) +
      geom_boxplot(aes(x = factor(optimizer), fill = factor(model), 
                       y = r2, group = interaction(factor(optimizer), factor(model)))) +
      labs(x = "Optimizer", y = "R2", fill = "Model", colour = "") +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      theme_classic(base_size = 14) +
      theme(strip.background = element_blank(),
            plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "mm"))
    
    # Combining the plots
    p53<-grid.arrange(p48, p49, p50, p51, p52, ncol = 3)
    
    ggplot2::ggsave("Lstm_models_comparison_WAH.png",plot=p53, device = "png", units = "cm",
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
                      mae=0.39,     
                      rmse=203.94,
                      r2=0.86,
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
  window_size<- 2 #number of years looking back to make the forecast
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
  
  p54 <- ggplot(plot_data, aes(x = yr)) +
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
  p54
  
  ggplot2::ggsave("Lstm_fit_WAH.png",plot=p54, device = "png", units = "cm",
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
  
  #R2 labels
  r2_labels <- best_lstm_models %>%
    dplyr::select(codsp, r2) %>%
    dplyr::mutate(label = paste0("R² = ", round(r2, 2)))
  
  p55 <- ggplot(plot_data, aes(x = yr)) +
    # catch series
    geom_line(aes(y = ct), color = "grey15",linewidth=1.2) +
    geom_text(data = r2_labels,
              aes(x = -Inf, y = Inf, label = label),
              hjust = -0.1, vjust = 1.1,
              inherit.aes = FALSE,
              size = 3)+
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
  p55
  
  ggplot2::ggsave("Lstm_fit_ALLSP.png",plot=p55, device = "png", units = "cm",
                                                        width = 28, height = 16)
  
  
  #catch series (Base case scenario)
  BLF_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66]), 
                         Freire= c(smtct$catch[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = BLF_catch_base,                   
    file = "BLF_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  BRS_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66]), 
                         Freire= c(smtct$catch[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = BRS_catch_base,                   
    file = "BRS_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  DOL_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66]), 
                         Freire= c(smtct$catch[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = DOL_catch_base,                   
    file = "DOL_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  FRI_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66]), 
                         Freire= c(smtct$catch[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = FRI_catch_base,                   
    file = "FRI_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  KGM_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66]), 
                         Freire= c(smtct$catch[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = KGM_catch_base,                   
    file = "KGM_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  LTA_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66] ), 
                         Freire= c(smtct$catch[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = LTA_catch_base,                   
    file = "LTA_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  WAH_catch_base<- data.frame(Year= c(smtct$year[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66]), 
                         Freire= c(smtct$catch[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66]))
  # Writing in a csv file
  write.table(
    x = WAH_catch_base,                   
    file = "WAH_catch_base.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  #------------------------------------------------------------------------------
  
  
  #catch series + LSTM predictions (Data Forecasting)
  BLF_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="BLF"] [67:length(lstm_pred$yr[lstm_pred$codsp=="BLF"])] ), 
                         Freire= c(smtct$catch[smtct$species=="BLF" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="BLF"] [67:length(lstm_pred$pred[lstm_pred$codsp=="BLF"])]))
  # Writing in a csv file
  write.table(
    x = BLF_catch_for,                   
    file = "BLF_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  BRS_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="BRS"] [67:length(lstm_pred$yr[lstm_pred$codsp=="BRS"])] ), 
                         Freire= c(smtct$catch[smtct$species=="BRS" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="BRS"] [67:length(lstm_pred$pred[lstm_pred$codsp=="BRS"])]))
  # Writing in a csv file
  write.table(
    x = BRS_catch_for,                   
    file = "BRS_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  DOL_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="DOL"] [67:length(lstm_pred$yr[lstm_pred$codsp=="DOL"])] ), 
                         Freire= c(smtct$catch[smtct$species=="DOL" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="DOL"] [67:length(lstm_pred$pred[lstm_pred$codsp=="DOL"])]))
  # Writing in a csv file
  write.table(
    x = DOL_catch_for,                   
    file = "DOL_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  FRI_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="FRI"] [67:length(lstm_pred$yr[lstm_pred$codsp=="FRI"])] ), 
                         Freire= c(smtct$catch[smtct$species=="FRI" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="FRI"] [67:length(lstm_pred$pred[lstm_pred$codsp=="FRI"])]))
  # Writing in a csv file
  write.table(
    x = FRI_catch_for,                   
    file = "FRI_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  KGM_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="KGM"] [67:length(lstm_pred$yr[lstm_pred$codsp=="KGM"])] ), 
                         Freire= c(smtct$catch[smtct$species=="KGM" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="KGM"] [67:length(lstm_pred$pred[lstm_pred$codsp=="KGM"])]))
  # Writing in a csv file
  write.table(
    x = KGM_catch_for,                   
    file = "KGM_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  LTA_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="LTA"] [67:length(lstm_pred$yr[lstm_pred$codsp=="LTA"])] ), 
                         Freire= c(smtct$catch[smtct$species=="LTA" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="LTA"] [67:length(lstm_pred$pred[lstm_pred$codsp=="LTA"])]))
  # Writing in a csv file
  write.table(
    x = LTA_catch_for,                   
    file = "LTA_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  WAH_catch_for<- data.frame(Year= c(smtct$year[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66],lstm_pred$yr[lstm_pred$codsp=="WAH"] [67:length(lstm_pred$yr[lstm_pred$codsp=="WAH"])] ), 
                         Freire= c(smtct$catch[smtct$species=="WAH" & smtct$type=="FREIRE"][1:66],lstm_pred$pred[lstm_pred$codsp=="WAH"] [67:length(lstm_pred$pred[lstm_pred$codsp=="WAH"])]))
  # Writing in a csv file
  write.table(
    x = WAH_catch_for,                   
    file = "WAH_catch_for.csv",          
    append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
  )
  
  #writing the full Catch data frame..
  #auxiliary function
  make_catch_df <- function(catch_df, codsp_name) {
    
    n <- nrow(catch_df)
    
    data.frame(
      year  = catch_df$Year,
      codsp = rep(codsp_name, n),
      catch = catch_df$Freire,
      type  = c(
        rep("Freire", 66),
        rep("Aug", n - 66)
      )
    )
  }
  
  #binding all catch data frames
  catch_all <- dplyr::bind_rows(
    make_catch_df(BLF_catch_for, "BLF"),
    make_catch_df(BRS_catch_for, "BRS"),
    make_catch_df(DOL_catch_for, "DOL"),
    make_catch_df(FRI_catch_for, "FRI"),
    make_catch_df(KGM_catch_for, "KGM"),
    make_catch_df(LTA_catch_for, "LTA"),
    make_catch_df(WAH_catch_for, "WAH")
  )
  #write the full final catch data frame
  write.table(
    x = catch_all,
    file = "smtct_for.csv",
    append = FALSE,
    dec = ".",
    sep = ",",
    row.names = FALSE,
    col.names = TRUE
  )
  
  
  
  #-------------------------------------------------------------------#
  #     2- Length data file                                           # 
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
                                Length<110,
                                !(Year %in% c(2017))) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
BLF_length_obs<- tidyr::spread(BLF_length_obs, Length,Count)

# Writing in a csv file
write.table(
  x = BLF_length_obs,                   
  file = "BLF_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#BRS
#Filtering Discrepant length data and sample size
BRS_length_obs <- lobs_out %>%
                  dplyr::filter(codsp=="BRS",
                                Length<105) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
BRS_length_obs<- tidyr::spread(BRS_length_obs, Length,Count)
# Writing in a csv file
write.table(
  x = BRS_length_obs,                   
  file = "BRS_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#DOL
#Filtering Discrepant length data and sample size
DOL_length_obs <- lobs_out %>%
                 dplyr::filter(codsp == "DOL",
                         Length>30 & Length<160,       
                        !(Year %in% c(2002, 2003, 2004,2006,2013,2014,2015,2020))) %>%
                 dplyr::select(-codsp)
#Stock Synthesis format
DOL_length_obs<- tidyr::spread(DOL_length_obs, Length,Count)
# Writing in a csv file
write.table(
  x = DOL_length_obs,                   
  file = "DOL_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)


#FRI
#Filtering Discrepant length data and sample size
FRI_length_obs <- lobs_out %>%
                  dplyr::filter(codsp == "FRI",
                                Length<60,
                                !(Year %in% c(1991,1995,1997,2006))) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
FRI_length_obs<- tidyr::spread(FRI_length_obs, Length,Count)
# Writing in a csv file
write.table(
  x = FRI_length_obs,                   
  file = "FRI_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#KGM
#Filtering Discrepant length data and sample size
KGM_length_obs <- lobs_out %>%
                  dplyr::filter(codsp == "KGM",
                                Length<170,
                               !(Year %in% c(2019,2020))) %>%
                  dplyr::select(-codsp)
#Stock Synthesis format
KGM_length_obs<- tidyr::spread(KGM_length_obs, Length,Count)
#Writing in a csv file
write.table(
  x = KGM_length_obs,                   
  file = "KGM_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

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
# Writing in a csv file
write.table(
  x = LTA_length_obs,                   
  file = "LTA_length_obs.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

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
# Writing in a csv file
write.table(
  x = BLF_length_obspred,                   
  file = "BLF_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#BRS
#Filtering Discrepant length data and sample size
BRS_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp=="BRS") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
BRS_length_obspred<- tidyr::spread(BRS_length_obspred, Length,Count)
# Writing in a csv file
write.table(
  x = BRS_length_obspred,                   
  file = "BRS_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#DOL
#Filtering Discrepant length data and sample size
DOL_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "DOL",
                Length<200) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
DOL_length_obspred<- tidyr::spread(DOL_length_obspred, Length,Count)
# Writing in a csv file
write.table(
  x = DOL_length_obspred,                   
  file = "DOL_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#FRI
#Filtering Discrepant length data and sample size
FRI_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "FRI",
                Length<62) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
FRI_length_obspred<- tidyr::spread(FRI_length_obspred, Length,Count)
# Writing in a csv file
write.table(
  x = FRI_length_obspred,                   
  file = "FRI_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#KGM
#Filtering Discrepant length data and sample size
KGM_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "KGM",
                Length<130) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
KGM_length_obspred<- tidyr::spread(KGM_length_obspred, Length,Count)
#Writing in a csv file
write.table(
  x = KGM_length_obspred,                   
  file = "KGM_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#LTA
#Filtering Discrepant length data and sample size
LTA_length_obspred <- lobspred_out %>%
  dplyr::filter(codsp == "LTA") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
LTA_length_obspred<- tidyr::spread(LTA_length_obspred, Length,Count)
# Writing in a csv file
write.table(
  x = LTA_length_obspred,                   
  file = "LTA_length_obspred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

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
# Writing in a csv file
write.table(
  x = BLF_length_pred,                   
  file = "BLF_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#BRS
#Filtering Discrepant length data and sample size
BRS_length_pred <- lpred_out %>%
  dplyr::filter(codsp=="BRS") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
BRS_length_pred<- tidyr::spread(BRS_length_pred, Length,Count)
# Writing in a csv file
write.table(
  x = BRS_length_pred,                   
  file = "BRS_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#DOL
#Filtering Discrepant length data and sample size
DOL_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "DOL",
                Length<200) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
DOL_length_pred<- tidyr::spread(DOL_length_pred, Length,Count)
# Writing in a csv file
write.table(
  x = DOL_length_pred,                   
  file = "DOL_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)

#FRI
#Filtering Discrepant length data and sample size
FRI_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "FRI",
                Length<66) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
FRI_length_pred<- tidyr::spread(FRI_length_pred, Length,Count)
# Writing in a csv file
write.table(
  x = FRI_length_pred,                   
  file = "FRI_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)


#KGM
#Filtering Discrepant length data and sample size
KGM_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "KGM",
                Length<130) %>%
  dplyr::select(-codsp)
#Stock Synthesis format
KGM_length_pred<- tidyr::spread(KGM_length_pred, Length,Count)
#Writing in a csv file
write.table(
  x = KGM_length_pred,                   
  file = "KGM_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)


#LTA
#Filtering Discrepant length data and sample size
LTA_length_pred <- lpred_out %>%
  dplyr::filter(codsp == "LTA") %>%
  dplyr::select(-codsp)
#Stock Synthesis format
LTA_length_pred<- tidyr::spread(LTA_length_pred, Length,Count)
# Writing in a csv file
write.table(
  x = LTA_length_pred,                   
  file = "LTA_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)


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
  x = WAH_length_pred,                   
  file = "WAH_length_pred.csv",          
  append = FALSE, dec = ".", sep = ",", row.names = FALSE, col.names = TRUE                        
)
#------------------------------------------------------------------


# =======================================================================================
#                   STOCK ASSESSMENT MODULE – STOCK SYNTHESIS (SS3)                     #
#         Methot & Wetzel (2013) – https://doi.org/10.1016/j.fishres.2012.10.012        #
#                                                                                       #                            
         # This module performs integrated stock assessment using Stock Synthesis (SS3) #
#                                                                                       #                                            
#    Analytical approaches implemented:                                                 #
#   • Sensitivity analysis (M, selectivity, steepness)                                  #  
#   • Likelihood profiles                                                               #
#   • Alternative selectivity shapes (Logistic vs Dome-shaped)                          #
#                                                                                       #                                  
#     All scenarios are structured under a unified parameter grid framework.            #
# =======================================================================================

  
# ---------------------------------------------------------
# ENVIRONMENT SETUP
# ---------------------------------------------------------
# Load required libraries for:
#   - SS3 input/output handling (r4ss)
#   - Parallel computation
#   - Data manipulation (dplyr, tidyr)
#   - Visualization (ggplot2)
#   - Mixed models and spline utilities (lmerTest, splines)
# ---------------------------------------------------------
library(r4ss)
library(parallel)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmerTest)
library(splines)

# ---------------------------------------------------------
# DIRECTORY STRUCTURE
# ---------------------------------------------------------
# Define working directory and SS3 input folder.
# All model runs and outputs will be organized relative
# to this main directory.
# ---------------------------------------------------------
setwd("C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas")#directory
dir=getwd()
dir_input= "C:/Matheus/Universidade/Doutorado/Stock Assessment Small Tunas/SS3/input"#SS3 input files
smtlh_ss<- read.csv("smtlh_ss.csv",sep=",",dec = ".")

# ---------------------------------------------------------
# SPECIES INCLUDED IN THE ANALYSIS
# ---------------------------------------------------------
# BLF, BRS, DOL, FRI, KGM, LTA, WAH
# These represent the small tuna stocks evaluated
# under the integrated SS3 framework.
# ---------------------------------------------------------
sp<- c("BLF", "BRS", "DOL", "FRI", "KGM", "LTA","WAH")  
          
# ------------------------------------------------------------------------------
# BASE PARAMETER DATASET CONSTRUCTION
# ------------------------------------------------------------------------------
# Objective:
#   Build the biological parameter set for the Base Case scenario.
#
# Approach:
#   • Use literature-derived parameters.
#   • Select median (real observed) biological values.
#   • No interpolation applied for Southwest stocks.
#
# Initial conditions:
#   - Initial ln(R0) = 7
#   - Initial sigmaR = 0.2 (later adjusted)
#   - Initial maturation slope = -0.55
#
# Special treatments:
#   - DOL: broader growth filtering to avoid extreme values
#   - KGM: constrained Linf range
#   - WAH and DOL: not restricted to AT-SW stock (few parameters)
# ------------------------------------------------------------------------------
base_data <- smtlh_ss %>%
  filter(
    codsp %in% sp,
    case_when(
      codsp %in% c("DOL","WAH") ~ TRUE,
      TRUE          ~ stock == "AT-SW"
    )
  ) %>%
  filter(
    case_when(
      codsp != "DOL" ~ TRUE,
      codsp == "DOL" ~
        (is.na(linf) | (linf >= 80 & linf <= 200)) &
        (is.na(k)    | (k    >= 0.5 & k    <= 2))
    )
  ) %>%
   filter(
     case_when(
      codsp != "KGM" ~ TRUE,
     codsp == "KGM" ~
       (is.na(linf) | (linf >=130 & linf <= 165))
    )
   )
# ------------------------------------------------------------------------------
# GROWTH PARAMETER SELECTION
# ------------------------------------------------------------------------------
# Instead of directly using medians for von Bertalanffy parameters,
# this function:
#   1. Computes the multivariate median (Linf, k, t0).
#   2. Identifies the empirical study closest to that median
#      in Euclidean space.
#   3. Selects that study to preserve biological coherence
#      among growth parameters.
#
# This avoids mixing parameters from different sources
# that could produce unrealistic growth curves.
# ------------------------------------------------------------------------------
pick_growth_study <- function(df){
  
  df <- df %>%
    mutate(t0_use = coalesce(t0, t0_pauly)) %>%
    filter(!is.na(linf), !is.na(k), !is.na(t0_use))
  
  med_linf <- median(df$linf, na.rm = TRUE)
  med_k    <- median(df$k, na.rm = TRUE)
  med_t0   <- median(df$t0_use, na.rm = TRUE)
  
  df %>%
    mutate(
      d = sqrt((linf-med_linf)^2 +
                 (k-med_k)^2 +
                 (t0_use-med_t0)^2)
    ) %>%
    arrange(d) %>%
    slice(1) %>%
    transmute(
      linf = linf,
      k    = k,
      t0   = t0_use
    )
}

# ------------------------------------------------------------------------------
# MEDIAN BIOLOGICAL PARAMETERS
# ------------------------------------------------------------------------------
# For remaining life-history parameters (M, maturity, weight-length,
# steepness, selectivity), median values across literature sources
# are computed per species.
#
# Quantile type = 1 (empirical median without interpolation).
# ------------------------------------------------------------------------------
rest_medians <- base_data %>%
  group_by(codsp) %>%
  summarise(
    tmax = ceiling(quantile(c(tmax, tmax_taylor), 0.5, na.rm = TRUE, type = 1)),
    m    = quantile(c(m, m_algaraja, m_pauly), 0.5, na.rm = TRUE, type = 1),
    lm50 = quantile(lm50, 0.5, na.rm = TRUE, type = 1),
    wla  = quantile(wl_a, 0.5, na.rm = TRUE, type = 1),
    wlb  = quantile(wl_b, 0.5, na.rm = TRUE, type = 1),
    h    = quantile(h,    0.5, na.rm = TRUE, type = 1),
    sl50 = quantile(sl50, 0.5, na.rm = TRUE, type = 1),
    sl95 = quantile(sl95, 0.5, na.rm = TRUE, type = 1),
    .groups = "drop"
  )

growth_block <- base_data %>%
  group_by(codsp) %>%
  group_modify(~ pick_growth_study(.x)) %>%
  ungroup()

# ------------------------------------------------------------------------------
# BASE SCENARIO PARAMETER GRID
# ------------------------------------------------------------------------------
# Combines:
#   • Selected coherent growth study
#   • Median biological parameters
#
# Defines:
#   - Selectivity type (Logistic or Dome-shaped)
#   - Initial recruitment level (lnR0)
#   - Recruitment variability (sigmaR)
#   - Maturity slope
#
# This represents the biological baseline scenario.
# ------------------------------------------------------------------------------
param_grid_base <- growth_block %>%
  left_join(rest_medians, by = "codsp") %>%
  mutate(
    scenario  = "Base",
    init_yr   = 1950,
    end_yr    = 2015,
    length_type= "Obs",
    rec_type  = "Variable",
    stock     = "AT-SW",
    par_var   = NA_character_,
    lnr0      = 7,
    sel_type = case_when(
      codsp %in% c("BLF", "DOL", "FRI", "KGM", "LTA", "WAH") ~ "Logistic",
      codsp == "BRS"                                      ~ "Dome-Shaped",
      TRUE                                                ~ NA_character_
    ),
    ascend_se = case_when(
      codsp %in% c("BLF", "BRS", "LTA") ~ 3.9,
      codsp %in% c("DOL", "FRI", "KGM") ~ 4.7,
      codsp == "WAH"                   ~ 7,
      TRUE                             ~ NA_real_
    ),
    sigma_r = 0.1,
    mat_slope= -0.55,
    m = case_when(
      codsp %in% c("LTA") ~ 0.83,
      TRUE          ~ m)
    )


# ==============================================================================
# SENSITIVITY ANALYSIS SCENARIOS
# ==============================================================================
# Objective:
#   Evaluate model structural and biological uncertainty.
#
# Scenarios include:
#   • Natural mortality (M) – 25th and 75th percentiles
#   • Selectivity slope (SL95 ±10%)
#   • Recruitment productivity (steepness ±15%)
#
# These scenarios test robustness of stock status inference
# to key life-history assumptions.
# ==============================================================================
#Low selectivity (0.9)
sel_low <- param_grid_base %>%
  dplyr::mutate(
    sl95     = sl95 * c(0.9),
    scenario = "SelLower",
    par_var  = "Selectivity")
#High selectivity (1.1)
sel_high <- param_grid_base %>%
  dplyr::mutate(
    sl95     = sl95 * c(1.1),
    scenario = "SelHigher",
    par_var  = "Selectivity")
#Low productivity (steepness down)
rec_low <- param_grid_base %>%
  mutate(
    h        = pmax(0.55, h * 0.85),
    scenario = "SteepLow",
    par_var  = "Steepness"
  )
#High productivity (steepness up)
rec_high <- param_grid_base %>%
  mutate(
    h        = pmin(0.95, h * 1.15),
    scenario = "SteepHigh",
    par_var  = "Steepness"
  )# M sensitivity
m_sensitivity <- smtlh_ss %>%
  filter(codsp %in% sp) %>%
  
  pivot_longer(
    cols = c(m, m_algaraja, m_pauly),
    names_to  = "param_source",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  
  group_by(codsp) %>%
  summarise(
    m_p25 = quantile(value, 0.25, na.rm = TRUE, type = 1),
    m_p75 = quantile(value, 0.75, na.rm = TRUE, type = 1),
    .groups = "drop"
  ) %>%
  
  left_join(param_grid_base, by = "codsp") %>%
  
  pivot_longer(
    cols = c(m_p25, m_p75),
    names_to  = "m_scenario",
    values_to = "m_new"
  ) %>%
  
  mutate(
    m        = m_new,
    scenario = if_else(m_scenario == "m_p25", "MLower", "MHigher"),
    par_var  = "M"
  ) %>%
  
  dplyr::select(-m_scenario, -m_new)

#sensitivity data frame
param_grid_sensitivity <- dplyr::bind_rows(
  m_sensitivity,
  sel_low,
  sel_high,
  rec_low,
  rec_high
)
# ------------------------------------------------------------------------------
# OBSERVATION-MODEL EXPERIMENT: LENGTH DATA TREATMENT
# ------------------------------------------------------------------------------
# Evaluates the impact of alternative length data usage:
#   - Obs     : Observed lengths
#   - Obspred : Observed + predicted
#   - Pred    : Model-predicted only
#
# Anchored in biological observations.
# ------------------------------------------------------------------------------
param_grid_lentype<- param_grid_base %>%
  dplyr::group_by(codsp) %>%
  tidyr::crossing(rep = 1:3) %>%
  dplyr::mutate(
    length_type = c("Obs","Obspred","Pred")[rep],
    scenario= "Length_type") %>%
  dplyr::select(-rep)

# ------------------------------------------------------------------------------
# CATCH FORECAST EXTENSION (2015–2025)
# ------------------------------------------------------------------------------
# Extends the assessment period using externally predicted
# catches (LSTM-based forecasts).
#
# Characteristics:
#   • No observational anchoring after 2015
#   • Pure statistical augmentation
#   • Data augmentation experiment
#
# Designed to evaluate forward-projection sensitivity.
# ------------------------------------------------------------------------------
param_grid_projection<- param_grid_base %>%
  dplyr::mutate(
    length_type = c("Obs"),
    scenario    = "Catch_for" ,
    init_yr     = 1950,
    end_yr      = 2025) 

# ------------------------------------------------------------------------------
# FINAL PARAMETER GRID
# ------------------------------------------------------------------------------
# Combines:
#   • Base scenario
#   • Sensitivity scenarios
#   • Observation-model experiments
#   • Projection scenarios
#
# This unified grid is used to generate all of SS3 model runs.
# ------------------------------------------------------------------------------
param_grid <- dplyr::bind_rows(param_grid_base,param_grid_sensitivity,param_grid_lentype,param_grid_projection)
 
param_grid %>%
  dplyr::count(codsp, scenario) %>%
  print(n = 100)
  
 #params= param_grid[param_grid$scenario=="Length_type",]
 #param_grid=params
    
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@- Main run_ss function --#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
{run_ss <- function(params) {
  
  #Extracting the parameter (current iteration). 
  codsp_it =  params$codsp #species code
  linf_it  =  params$linf  #growth Linfinity
  k_it     =  params$k    #growth k constant
  t0_it    =  params$t0   #t0 growth intercept
  tmax_it  =  params$tmax #max estimated age
  m_it     =  params$m    #natural mortality
  lm50_it  =  params$lm50 #length at 50% maturity
  wla_it   =  params$wla  #length-weight (a)
  wlb_it   =  params$wlb  #length-weight (b)
  h_it     =  params$h    #steepness    
  sl50_it  =  params$sl50 #length at 50% selectivity
  sl95_it  =  params$sl95 #length at 95% selectivity    
  scenario_it = params$scenario #current scenario
  init_yr_it  = params$init_yr  #initial year (catch series) 
  end_yr_it   = params$end_yr   #end year (catch series) 
  length_type_it=params$length_type #length type (obs,obspred,pred)
  rec_type_it =  params$rec_type #recruitment type
  stock_it =  params$stock #stock name
  par_var_it  = params$par_var #varying parameter
  lnr0_it  =    params$lnr0 # log initial recruitment 
  sel_type_it = params$sel_type #selectivity type
  ascend_se_it= params$ascend_se #ascend standard deviation 
  sigma_r_it  = params$sigma_r  #sigma R 
  mat_slope_it= params$mat_slope #maturation slope
  
  #Reading SS input files.....
  start<- r4ss::SS_readstarter(file.path(dir_input, "starter.ss")) #Start SS file
  fore<- r4ss::SS_readforecast(file.path(dir_input, "forecast.ss")) #Forecast SS file
  dat<- r4ss::SS_readdat(file.path(dir_input, "datafile.dat"))      #Data SS file
  ctl <- r4ss::SS_readctl(file.path(dir_input, "controlfile.ctl"),verbose = TRUE,datlist = dat, use_datlist = TRUE) #Control SS file
  
  # --------------------------------------------------
  # Catch compositions (current iteration)
  # --------------------------------------------------
  catch_file <- ifelse(
    scenario_it == "Catch_for",
    paste0(codsp_it, "_catch_for.csv"),
    paste0(codsp_it, "_catch_base.csv"))
  catch <- read.csv(file.path(dir, catch_file), sep = ",", dec = ".")
  
  # Catch data (SS format)
  catch_it <- data.frame(
    year     = as.integer(c(-999, catch$Year)),
    seas     = 1L,
    fleet    = 1L,
    catch    = c(1.000000e-20, catch$Freire),
    catch_se = 0.01)
  # temporal filter – catch
  catch_it <- catch_it[catch_it$year <= end_yr_it, ]  
  
  # --------------------------------------------------
  # Length compositions (obs / obspred / pred)
  # --------------------------------------------------
  length_file <- switch(
    length_type_it,
    Obs     = paste0(codsp_it, "_length_obs.csv"),
    Obspred = paste0(codsp_it, "_length_obspred.csv"),
    Pred    = paste0(codsp_it, "_length_pred.csv"),
    stop("length_type_it invalid: ", length_type_it)
  )
  length <- read.csv(file.path(dir, length_file), sep = ",", dec = ".")
  
  # Length data (SS format – ALWAYS needed)
  length_it <- data.frame(
    year  = length$Year,
    month = length$Month,
    fleet = length$Fleet,
    sex   = length$Sex,
    part  = 0,
    Nsamp = length$Nsamps
  )
  # Female bins
  len_f <- length[, grep("^X", names(length), value = TRUE)]
  names(len_f) <- gsub("^X", "f", names(len_f))
  # Male bins (zeros)
  len_m <- len_f
  len_m[,] <- 0
  names(len_m) <- gsub("^f", "m", names(len_m))
  # Bind everything
  length_it <- cbind(length_it, len_f, len_m)
  # SS likes integers
  length_it[] <- lapply(length_it, as.integer)  
  # temporal filter – length
  length_it <- length_it[length_it$year <= end_yr_it, ]  
 
  #------------------------------------------------------
  #changing the SS input files for the current iteration 
  #------------------------------------------------------
  #Start File
  if (codsp_it == "DOL") {
    # ---- DOL: avoid non-estimable MSY ----
    start$depl_basis       <- 2   # relative to SSBMSY if exists, otherwise falls back to target
    start$depl_denom_frac <- 1
    start$F_std_basis     <- 0   # absolute F (no ratio)
  } else {
    # ---- Other species: classic Kobe MSY ----
    start$depl_basis       <- 2   # SSB/SSBMSY
    start$depl_denom_frac <- 1
    start$F_std_basis     <- 2   # F/FMSY
  }
  #start$F_std_units= 1
  #--------------------------------------------------------------------------------------------------------------------------------------------
  #Forecast File
  fore$Flimitfraction= 1
  #--------------------------------------------------------------------------------------------------------------------------------------------
  #Data file
  dat$styr= init_yr_it #initial year
  dat$endyr= end_yr_it #end year
  dat$catch= catch_it #catch data
  dat$lencomp= length_it #length composition
  dat$Nages= as.numeric(ceiling(tmax_it)) #tmax  
  dat$binwidth=as.numeric(diff(as.numeric(gsub("^f", "", names(len_f))))[1]) #binwidth size
  dat$lbin_vector=as.numeric(gsub("^f", "", names(len_f))) #length bins vector
  dat$minimum_size=as.numeric(round(min(c( 0.04*linf_it, min(dat$lbin_vector))))) #mapping from 4% of the Linf or min lbin
  dat$maximum_size=as.numeric(round(max(c(1.25*linf_it, max(dat$lbin_vector*1.25))))) #mapping to 25% above linf or above max lbin
  dat$N_lbins=as.numeric(ncol(len_f)) #number of bins
  dat$N_agebins= as.numeric(dat$Nages)
  dat$agebin_vector= 0:(dat$Nages - 1) #ages bin vector
  dat$ageerror=as.data.frame(matrix(c(-1.000, 0.001),nrow = 2,ncol = length(paste0("age", seq(0, dat$Nages))),dimnames = list(NULL, paste0("age", seq(0, dat$Nages)))))
  dat$lbin_vector_pop=as.numeric(seq(dat$minimum_size,max(c(dat$maximum_size+dat$binwidth), max(dat$lbin_vector+dat$binwidth)), by=dat$binwidth)) #population bins (outside the real length compositions)
  dat$N_lbinspop=as.numeric(length(dat$lbin_vector_pop)) #n o bins of the population (outside the real lengths)
  #--------------------------------------------------------------------------------------------------------------------------------------------
  #Control File
  ctl$Nages= dat$Nages #number of ages
  ctl$Npopbins=dat$N_lbinspop #number of lbins population
  #female parameters
  ctl$MG_parms['NatM_p_1_Fem_GP_1',c('INIT','PRIOR')] = c(m_it, m_it) #natural mortality
  ctl$MG_parms['L_at_Amax_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(linf_it),as.numeric(linf_it)) #growth linf estimate
  ctl$MG_parms['VonBert_K_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(k_it),as.numeric(k_it)) #growth constant k
  ctl$MG_parms['Wtlen_1_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(wla_it),as.numeric(wla_it)) #a length-weight 
  ctl$MG_parms['Wtlen_2_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(wlb_it),as.numeric(wlb_it)) #b length-weight
  ctl$MG_parms['Eggs_alpha_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(wla_it),as.numeric(wla_it)) #a weigth == fecundity
  ctl$MG_parms['Eggs_beta_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(wlb_it),as.numeric(wlb_it)) #b weigth == fecundity
  ctl$MG_parms['Mat50%_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(lm50_it),as.numeric(lm50_it))#L50% maturity
  ctl$MG_parms['Mat_slope_Fem_GP_1',c('INIT','PRIOR')]= c(as.numeric(mat_slope_it),as.numeric(mat_slope_it))#slope maturity
  #Male parameters
  ctl$MG_parms['NatM_p_1_Mal_GP_1',c('INIT','PRIOR')] = c(m_it, m_it) #natural mortality
  ctl$MG_parms['L_at_Amax_Mal_GP_1',c('INIT','PRIOR')]= c(as.numeric(linf_it),as.numeric(linf_it)) #growth linf estimate
  ctl$MG_parms['VonBert_K_Mal_GP_1',c('INIT','PRIOR')]= c(as.numeric(k_it),as.numeric(k_it)) #growth constant k
  ctl$MG_parms['Wtlen_1_Mal_GP_1',c('INIT','PRIOR')]= c(as.numeric(wla_it),as.numeric(wla_it)) #a length-weight
  ctl$MG_parms['Wtlen_2_Mal_GP_1',c('INIT','PRIOR')]= c(as.numeric(wlb_it),as.numeric(wlb_it)) #b length-weight
  #recruitment parameters
  ctl$MainRdevYrFirst=init_yr_it
  ctl$MainRdevYrLast=end_yr_it
  ctl$last_early_yr_nobias_adj=init_yr_it
  ctl$first_yr_fullbias_adj= init_yr_it
  ctl$last_yr_fullbias_adj= end_yr_it
  ctl$first_recent_yr_nobias_adj=end_yr_it
  ctl$SR_parms['SR_LN(R0)', c('INIT','PRIOR','PHASE')]= c(as.numeric(lnr0_it),as.numeric(lnr0_it),1) #Log initial Recruitment
  ctl$SR_parms['SR_BH_steep',c('INIT','PRIOR')]= c(as.numeric(h_it),as.numeric(h_it)) #Recruitment steepness 
  ctl$SR_parms['SR_sigmaR',c('INIT','PRIOR')]= c(as.numeric(sigma_r_it),as.numeric(sigma_r_it)) #Sigma Recruitment
  #selectivity
  if (sel_type_it=="Logistic") { # Logistic Selectivity
  
    ctl$size_selex_types$Pattern<- 1
    #ctl$size_selex_patterns[ctl$size_selex_patterns$Fleet==1,"Pattern"] <- 6
    ctl$size_selex_parms['SizeSel_P_1_Fishery(1)',c('LO','HI','INIT','PRIOR','PR_SD','PHASE')] <- c(0.6*sl50_it, 1.4*sl50_it, sl50_it, sl50_it, 1, 2)
    ctl$size_selex_parms['SizeSel_P_2_Fishery(1)',c('LO','HI','INIT','PRIOR','PR_SD','PHASE')] <- c(1,25, 5, 5, 1, 3)
    keep <- rownames(ctl$size_selex_parms) %in% c(
      "SizeSel_P_1_Fishery(1)",
      "SizeSel_P_2_Fishery(1)")
    ctl$size_selex_parms <- ctl$size_selex_parms[keep, ]
   
  }
  if (sel_type_it=="Dome-Shaped") { #Dome-Shaped Selectivity
    
    ctl$size_selex_types$Pattern<- 24 #double normal curve (dome-shaped)
    ctl$size_selex_parms['SizeSel_P_1_Fishery(1)',c('LO','HI','INIT','PRIOR','PHASE')]=c(0.6*sl95_it,1.4*sl95_it,sl95_it,sl95_it*1.1,2)
    ctl$size_selex_parms['SizeSel_P_2_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-1.42,-1.42,-3)
    ctl$size_selex_parms['SizeSel_P_3_Fishery(1)',c('LO','HI','INIT','PRIOR','PHASE')]=c(-5,15,ascend_se_it,ascend_se_it,2)
    ctl$size_selex_parms['SizeSel_P_4_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-2.3,-2.3,-3)  
    ctl$size_selex_parms['SizeSel_P_5_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-15,-10,-3)   
    ctl$size_selex_parms['SizeSel_P_6_Fishery(1)',c('INIT','PRIOR','PHASE')]=c(-2.2,-2.2,-3)        
  }
  #----------------------------------------------------------------------------------------------------------------------------------------
  #Creating the folder to store input data files and the outputs for each iteration
  dir_out <- file.path(dir, "SS3", "scenarios",paste(codsp_it,scenario_it,init_yr_it,end_yr_it,length_type_it,rec_type_it,stock_it,par_var_it,sel_type_it,sep="_"))
  
  # ---- create clean path ----#
  if (dir.exists(dir_out)) {
    unlink(dir_out, recursive = TRUE, force = TRUE)
  }
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  
  # write out all (because of the changes of the current iteration)
  SS_writestarter(start, file.path(dir_out), overwrite = TRUE)
  SS_writeforecast(fore, file.path(dir_out), overwrite = TRUE) 
  SS_writedat(dat, file.path(dir_out,"datafile.dat"), overwrite = TRUE)
  SS_writectl(ctl, file.path(dir_out,"controlfile.ctl"), overwrite = TRUE)
  
  tryCatch({
    message("Running Species: ", codsp_it)
    # Run the model and store results (Estimating peak selectivity (sl95) and Initial Recruitment (LN R0)
    run(file.path(dir_out), exe =file.path(dir_input,"ss3.exe"),show_in_console = TRUE, verbose = TRUE, skipfinished = FALSE)
    out <- r4ss::SS_output(file.path(dir_out), verbose = FALSE, printstats=FALSE)
    SS_plots(out)
    #Process the results if no error is shown
    #Gathering results
    result= list(
      codsp=codsp_it,linf=linf_it,k=k_it,t0=t0_it,tmax=tmax_it,m=m_it,lm50=lm50_it,wla=wla_it,wlb=wlb_it,h=h_it,
      sl50=sl50_it,sl50_est=ifelse(sel_type_it=="Logistic",as.numeric(out$parameters$Value[out$parameters$Label=="Size_inflection_Fishery(1)"]),
                                                           as.numeric(out$parameters$Value[out$parameters$Label=="Size_DblN_peak_Fishery(1)"])),
      scenario=scenario_it,init_yr=init_yr_it,
      end_yr=end_yr_it,length_type=length_type_it,rec_type=rec_type_it,stock=stock_it,par_var=par_var_it,lnr0=lnr0_it,      
      lnr0_est=as.numeric(out$parameters$Value[out$parameters$Label=="SR_LN(R0)"]),sel_type=sel_type_it,ascend_se=ascend_se_it,
      ascend_se_est=ifelse(sel_type_it=="Logistic",as.numeric(out$parameters$Value[out$parameters$Label=="Size_95%width_Fishery(1)"]),
                                                   as.numeric(out$parameters$Value[out$parameters$Label=="Size_DblN_ascend_se_Fishery(1)"])),
      sigma_r=sigma_r_it,
      mat_slope=mat_slope_it,
      total_nll=as.numeric(out$likelihoods_used["TOTAL",'values']),catch_nll=as.numeric(out$likelihoods_used["Catch",'values']),
      length_nll=as.numeric(out$likelihoods_used["Length_comp",'values']),recruitment_nll=as.numeric(out$likelihoods_used["Recruitment",'values']),
      path= dir_out)
    message("Species ", codsp_it, " processed successfully.")
    return(result)
  },
  error = function(e) {
    message("Error occurred in species ", codsp_it, ": ", e$message)
    #In case of error jump to the next parameters combination (iteration)
  })
}
#============================================== End of Main run_ss function =======================================================================#

# Parallelizing loop
# Cluster
n_cores <- detectCores() 
cl <- makeCluster(n_cores)
#Exporting necessary objects (Everything outside the main jitter function must be here)
clusterExport(cl, c("param_grid", "run_ss", "dir", "dir_input"))
parallel::clusterEvalQ(cl, c(library(r4ss,dplyr)))

#Lapply parallelized to apply the Jitter function
results <- parLapply(cl, seq_len(nrow(param_grid)), function(i) {
  # Selecting [i] line parameters
  params <- param_grid[i, ]
  run_ss(params)  #Apply the Main jitter function
})
#End cluster
parallel::stopCluster(cl)

#Combining results in a data frame
run_ss_out <- as.data.frame(do.call(rbind, results)) 

#Jitter out as data frame to csv
ss_out_csv <- run_ss_out %>%
  dplyr::mutate(across(where(is.list), ~ unlist(.)))
# Write the full dataset before filtering
write.csv(ss_out_csv, file = file.path(dir, "ss_out.csv"), row.names = FALSE)
#-----------------------------------------------------------------------------
}


#------------------------------------------------------#
#           reading Scenarios r4ss::SS_output()        #         
#               Assigning models objects               #
#  Scenarios of  Base, MLower, MHigher, SelLower       #
#SelHigher, SteepLow, SteepHigh, Length_type, Catch_for#
#------------------------------------------------------#
dir_out <- file.path(dir, "SS3", "scenarios")
models <- list.dirs(dir_out, recursive = FALSE, full.names = TRUE)


run_log <- data.frame(
  folder = character(),
  object_name = character(),
  status = character(),
  reason = character(),
  has_report = logical(),
  has_covar = logical(),
  stringsAsFactors = FALSE
)

for (i in models) {
  
  folder_name <- basename(i)
  name <- sub("(_(Obs|Pred|Obspred)).*", "\\1", folder_name)
  name <- make.names(name)
  
  cat("\nRunning:", name, "\n")
  
  report_file <- file.path(i, "Report.sso")
  covar_file  <- file.path(i, "covar.sso")
  
  obj <- tryCatch(
    r4ss::SS_output(i, verbose = FALSE, printstats = FALSE),
    error = function(e) e
  )
  
  has_report <- file.exists(report_file)
  has_covar  <- file.exists(covar_file)
  
  if (inherits(obj, "error")) {
    
    run_log <- rbind(run_log, data.frame(
      folder = folder_name,
      object_name = name,
      status = "failed",
      reason = conditionMessage(obj),
      has_report = has_report,
      has_covar = has_covar,
      stringsAsFactors = FALSE
    ))
    
    message("❌ Falied ", folder_name)
    next
  }
  
  assign(name, obj, envir = .GlobalEnv)
  
  run_log <- rbind(run_log, data.frame(
    folder = folder_name,
    object_name = name,
    status = "success",
    reason = ifelse(has_covar, "ok", "no_hessian"),
    has_report = has_report,
    has_covar = has_covar,
    stringsAsFactors = FALSE
  ))
  
  message("✅ success:", name)
}

subset(run_log, status == "failed")


#-----------------------------------------------------#
#      taking the INPUT Life history parameters       #
#input life history used in Stock Synthesis framework #
#           assign corresponding authors              #
#-----------------------------------------------------#

match_source <- function(df, value_cols, target_row, tol = 1e-3){
  
  idx <- rep(TRUE, nrow(df))
  
  for(col in value_cols){
    idx <- idx & abs(df[[col]] - target_row[[col]]) < tol
  }
  
  unique(df$source[idx])
}

ss_final_input_lh <- param_grid %>%
  filter(scenario == "Base") %>%
  rowwise() %>%
  
  mutate(
    # ---- Growth authors ----
    author_growth = paste(match_source(smtlh_ss,c("linf","k"),pick(everything())),collapse = "; "),
    # ---- M authors ----
    author_m = paste(unique(smtlh_ss$source[abs(smtlh_ss$m - m) < 1e-6]),collapse = "; "),
    # ---- Maturity authors ----
    author_mat = paste(unique(smtlh_ss$source[abs(smtlh_ss$lm50 - lm50) < 1e-6]),collapse = "; ")
  ) %>%
  rowwise() %>%
  mutate(
    authors = paste(
      unique(na.omit(c_across(c(author_growth,
                                author_m,
                                author_mat)))),
      collapse = "; "
    )
  ) %>%
  ungroup()

# export as csv file
write.csv(ss_final_input_lh, file = "ss_final_input_lh.csv",row.names = FALSE)


#------------------------------------------------------------------------------------#
#               #plotting the used length data in SS analysis                        #
#Length information(1- Observed lengths;2- Observed+predicted; 3- predicted lengths) #
#------------------------------------------------------------------------------------#

#-------------------------------------------------------
# Function to apply species-specific  length filters
#-------------------------------------------------------
filter_species_lengths <- function(df, species, type_length) {
  
  df <- df %>% dplyr::select(yr, codsp, fl, source) %>%
    mutate(type = type_length)
  
  if (species == "BLF") {
    df <- df %>% filter(fl < 110, yr != 2017)
  } else if (species == "BRS") {
    df <- df %>% filter(fl < 105)
  } else if (species == "DOL") {
    df <- df %>% filter(fl > 30, fl < 200,
                        !(yr %in% c(2002,2003,2004,2006,2013,2014,2015,2020)))
  } else if (species == "FRI") {
    df <- df %>% filter(fl < 66,
                        !(yr %in% c(1991,1995,1997,2006)))
  } else if (species == "KGM") {
    df <- df %>% filter(fl < 170,
                        !(yr %in% c(2019,2020)))
  } else if (species == "LTA") {
    df <- df %>% filter(!(yr %in% c(2008,2015)))
  } else if (species == "WAH") {
    df <- df %>% filter(fl < 200,
                        !(yr %in% c(1986,1987,1999,2000,2002,2003,2004,
                                    2005,2008,2009,2010,2013,2014,2015,2016)))
  }
  
  return(df)
}

#-------------------------
# Building final dataset
#-------------------------
input_length_list <- list()

for (species in sp) {
  
  # Observed
  obs_length <- smtlenobs %>%
    filter(codsp == species) %>%
    filter_species_lengths(species, "obs")
  
  # Years with observed mean
  years <- smtml %>%
    filter(codsp == species, !is.na(mean), yr != 1950) %>%
    pull(yr) %>%
    unique()
  
  # Observed + predicted
  obspred_length <- smtlensim %>%
    filter(codsp == species, yr %in% years) %>%
    filter_species_lengths(species, "obspred")
  
  # Fully predicted
  pred_length <- smtlensim %>%
    filter(codsp == species) %>%
    filter_species_lengths(species, "pred")
  
  input_length_list[[species]] <- bind_rows(obs_length,
                                            obspred_length,
                                            pred_length)
}

input_length <- bind_rows(input_length_list) %>%
  mutate(type = case_when(
    type == "obs"     ~ "Obs",
    type == "pred"    ~ "Pred",
    type == "obspred" ~ "Obspred"
  ))


input_length$type <- factor(input_length$type,
                            levels = c("Pred", "Obspred", "Obs"))

#------
# Plot
#------
p56 <- ggplot(input_length) +
  geom_boxplot(aes(x = factor(yr),
                   y = fl,
                   color = type,
                   fill = type),
               alpha = 0.6,
               position = "identity",
               outlier.size = 0.7,
               width = 0.6) +
  facet_wrap(~codsp, scales = "free") +
  scale_fill_manual(values = c("Pred" = "#56B4E9",
                               "Obspred" = "#009E73",
                               "Obs" = "#D55E00")) +
  scale_color_manual(values = c("Pred" = "#56B4E9",
                                "Obspred" = "#009E73",
                                "Obs" = "#D55E00")) +
  scale_x_discrete(breaks = as.character(seq(min(as.numeric(as.character(input_length$yr))),
                                             max(as.numeric(as.character(input_length$yr))),
                                             by = 20))) +
  labs(x = "Year",
       y = "Length (cm)",
       fill = "Length type",
       color = "Length type") +
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
p56

ggsave("boxplot_input_length_by_species.png", plot = p56, units = "cm", width = 30, height = 18, dpi = 350)


#----------------------------------------------------------------------------------#
# Taking the final estimated parameters for all scenarios                          #
# Final gradient, min, max bounds, Standard deviation     
#creating a final table and writing as a csv file
#-----------------------------------------------------------------------------------#
# Using only models with stats= Success
models <- run_log %>%
  filter(status == "success") %>%
  pull(object_name)

ss_params_out <- list()

for (i in seq_along(models)) {
  
  obj_name <- models[i]
  
  # check if the object exists in the environment
  if (!exists(obj_name, envir = .GlobalEnv)) {
    message("⚠️ Object not found: ", obj_name)
    next
  }
  
  obj <- get(obj_name, envir = .GlobalEnv)
  
  # check if there is a parameter table
  if (is.null(obj$parameters)) {
    message("⚠️ No parameters found: ", obj_name)
    next
  }
  
  tab_res <- obj$parameters %>%
    dplyr::select(Label, Value, Min, Max, Init, Parm_StDev, Gradient)
  
  # Identify which selectivity parameter exists
  sel_label <- NULL
  
  if ("Size_inflection_Fishery(1)" %in% tab_res$Label) {
    sel_label <- "Size_inflection_Fishery(1)"
  }
  
  if ("Size_DblN_peak_Fishery(1)" %in% tab_res$Label) {
    sel_label <- "Size_DblN_peak_Fishery(1)"
  }
  
  # parameters of interest
  keep_labels <- c("SR_LN(R0)", sel_label)
  
  tab_res <- tab_res %>%
    filter(Label %in% keep_labels) %>%
    mutate(
      Models = obj_name,
      Converged = ifelse(is.na(Parm_StDev), "No_Hessian", "OK")
    )
  
  ss_params_out[[i]] <- tab_res
}


# put it all together
ss_params_out <- bind_rows(ss_params_out) %>%
  mutate(
    Models = as.character(Models),
    
    # specie = first part
    Specie = sub("_.*", "", Models),
    
    # raw scenario (second part)
    Scenario_raw = sub("^[^_]+_", "", Models),
    Scenario_raw = sub("_.*", "", Scenario_raw),
    
    # last part  (Obs / Pred / Obspred)
    LengthType = sub(".*_", "", Models),
    
    # special case for Length_type
    Scenario = case_when(
      Scenario_raw == "Length" ~ paste0("Length_", LengthType),
      TRUE ~ Scenario_raw
    )
  ) %>%
  mutate(
    Parameter = Label,
    Initial   = Init,
    Estimate  = Value,
    Sd        = Parm_StDev,
    Model     = Models
  ) %>%
  dplyr::select(
    Specie, Scenario, Parameter,
    Initial, Min, Max,
    Estimate, Sd, Gradient,
    Model, Converged
  ) %>%
  arrange(Scenario)


# view
head(ss_params_out)

# export as csv file
write.csv(ss_params_out, file = "ss_params_out.csv",row.names = FALSE)



#-------------------------------------------------------------------------#
  # Sensitivity analysis through the scenarios

  #   Evaluate model structural and biological uncertainty.
  #
  # Scenarios include:
  #   • Natural mortality (M) – 25th and 75th percentiles
  #   • Selectivity slope (SL95 ±10%)
  #   • Recruitment productivity (steepness ±15%)
  #
  # These scenarios test robustness of stock status inference
  # to key life-history assumptions.
  # take F, FMSY, SSB, SSBMSY, F/FMSY, SSB/SSBMSY
  # Derive sd for F/FMSY and SSB/SSBMSY through delta method
#------------------------------------------------------------------------#
ss_sensi_out <- list()

for (i in seq_along(models)) {
  
  obj_name <- models[i]
  
  if (!exists(obj_name, envir = .GlobalEnv)) next
  
  #get the object in the environment
  obj <- get(obj_name, envir = .GlobalEnv)
  
  codsp   <- sub("_.*", "", obj_name)
  # raw scenario
  scenario_raw = sub("^[^_]+_", "", obj_name)
  scenario_raw = sub("_.*", "", scenario_raw)
  # last part (Obs / Pred / Obspred)
  LengthType = sub(".*_", "", obj_name)
  # special case for Length_type
  scenario <- case_when(
    scenario_raw == "Length" ~ paste0("Length_", LengthType),
    scenario_raw == "Catch"  ~ "Catch_Forecast",
    TRUE ~ scenario_raw
  )
  
  model   <- obj_name
  year    <- seq(obj$startyr, obj$endyr)
  
  # derived quantities from SS
  dq <- obj[["derived_quants"]]
  
  #F metrics
  fmsy      <- dq$Value[dq$Label=="annF_MSY"]
  fmsy_sd   <- dq$StdDev[dq$Label=="annF_MSY"]
  # SSB metrics
  ssbmsy    <- dq$Value[dq$Label=="SSB_MSY"]
  ssbmsy_sd <- dq$StdDev[dq$Label=="SSB_MSY"]
  
  f_lab   <- paste("F", year, sep = "_")
  ssb_lab <- paste("SSB", year, sep = "_")
  
  # F SD
  f <- dq$Value[match(f_lab, dq$Label)]
  f_sd <- dq$StdDev[match(f_lab, dq$Label)]
  # SSB SD
  ssb <- dq$Value[match(ssb_lab, dq$Label)]
  ssb_sd <- dq$StdDev[match(ssb_lab, dq$Label)]
  # Relative metrics
  f_fmsy       <- f / fmsy
  ssb_ssbmsy   <- ssb / ssbmsy
  
  # Delta method to calculate ratio sd
  f_fmsy_sd <- f_fmsy * sqrt(ifelse(f == 0, NA, (f_sd/f)^2) + (fmsy_sd/fmsy)^2)
  
  ssb_ssbmsy_sd <- ssb_ssbmsy * sqrt(ifelse(ssb == 0, NA, (ssb_sd/ssb)^2) + (ssbmsy_sd/ssbmsy)^2)
  
  tab_res <- data.frame(
    Species = codsp,
    Scenario = scenario,
    Model = model,
    Year = year,
    F = f,
    F_sd = f_sd,
    F_Fmsy = f_fmsy,
    F_Fmsy_sd = f_fmsy_sd,
    SSB = ssb,
    SSB_sd = ssb_sd,
    SSB_SSBmsy = ssb_ssbmsy,
    SSB_SSBmsy_sd = ssb_ssbmsy_sd
  )
  
  ss_sensi_out[[i]] <- tab_res
}

ss_sensi_out <- dplyr::bind_rows(ss_sensi_out) %>%
    mutate(
    bad_sd = SSB_SSBmsy_sd > 1,   #classifiy bad sceanrios to remove
  )

#-------------------------------------------------------------
# Identify unstable sensitivity models and remove them
#-------------------------------------------------------------
# Models are flagged as "bad" if:
#  - relative biomass exceeds biologically implausible values
#  - or uncertainty (SD) is excessively large.
bad_models <- ss_sensi_out %>%
  group_by(Model) %>%
  summarise(
    bad = any(
      SSB_SSBmsy > 10 |  #removing extreme SSB broken models
        bad_sd |
        (Species == "DOL" & SSB_SSBmsy > 1.8) #extreme SSB broken model
    ),
    .groups = "drop"
  ) %>%
  filter(bad) %>%
  pull(Model)

# Keep only acceptable models for plotting
ss_sensi_plot <- ss_sensi_out %>%
  filter(!Model %in% bad_models) %>%
  mutate(
    Scenario_plot = ifelse(Scenario == "Base", "Base", Scenario)
  ) %>%
  filter(Scenario_plot != "Length_Obs")
#-------------------------------------------------------------
# Define scenario ordering for legend and plotting
#-------------------------------------------------------------
scenario_levels <- c(
  "Base",
  "Catch_Forecast",
  "Length_Obspred",
  "Length_Pred",
  "MHigher",
  "MLower",
  "SelHigher",
  "SelLower",
  "SteepHigh",
  "SteepLow")

ss_sensi_plot$Scenario_plot <- factor(
  ss_sensi_plot$Scenario_plot,
  levels = scenario_levels)

# Separate base model from sensitivity scenarios
base_data <- filter(ss_sensi_plot, Scenario == "Base")
sens_data <- filter(ss_sensi_plot, Scenario != "Base")
unique(sens_data$Scenario)

# Define plotting symbols (pch) for each scenario
pch_vals <- c(
  "Base" = 16,
  "Catch_Forecast" = 16,
  "Length_Obspred" = 17,
  "Length_Pred" = 18,
  "MHigher" = 3,
  "MLower" = 4,
  "SelHigher" = 8,
  "SelLower" = 1,
  "SteepHigh" = 2,
  "SteepLow" = 0
)

pch_vals <- pch_vals[scenario_levels]

#must define manual colors
scenario_colors <- c(
  Base = "black",
  Catch_Forecast = "#F8766D",
  Length_Obspred = "#B79F00",
  Length_Pred = "#00BA38",
  MHigher = "#00BFC4",
  MLower = "#00A9CF",
  SelHigher = "#619CFF",
  SelLower = "#8B80FF",
  SteepHigh = "#F564E3",
  SteepLow = "#C77CFF"
)

# Reduce visual clutter by plotting points every 10 years
sens_points <- sens_data %>%
  filter(Year %% 10 == 0)   # pontos a cada 5 anos

sens_levels <- levels(ss_sensi_plot$Scenario_plot)[-1]



library(ggplot2)
#-------------------------------------------------------------
# Plot sensitivity trajectories relative to MSY reference points
#-------------------------------------------------------------
p57 <- ggplot() +
# Base model uncertainty envelope (95% CI ribbon)
  geom_ribbon(data = base_data,
    aes(x = Year, ymin = SSB_SSBmsy - 1.96*SSB_SSBmsy_sd,
      ymax = SSB_SSBmsy + 1.96*SSB_SSBmsy_sd,
      group = Scenario_plot,
      fill = "Base"),
    alpha = 0.3,
    color = NA)+
# Sensitivity scenario trajectories
  geom_line(data = sens_data,
    aes(x = Year, y = SSB_SSBmsy,
      group = Scenario_plot,
      color = Scenario_plot,
    group = interaction(Species, Scenario_plot)),
    linewidth = 1,
    alpha = 0.8)+
  # Add symbols periodically to improve scenario identification
  geom_point(data = sens_points,
    aes(x = Year, y = SSB_SSBmsy,
      color = Scenario_plot,
      shape = Scenario_plot,
      group = Scenario_plot),
    size = 2.5,
    stroke = 0.6)+
  # Highlight base model trajectory
  geom_line(data = base_data,
    aes(x = Year,y = SSB_SSBmsy,
      group = interaction(Species, Scenario_plot),
      color = Scenario_plot),
    linewidth = 1.2)+
  # Manual scales for colours and symbols
  scale_color_manual(
    values = scenario_colors,
    limits = scenario_levels
  )+
scale_shape_manual(
  values = pch_vals,
  limits = scenario_levels
)+
  scale_fill_manual(
    values = c("Base" = "grey70"),
    guide = "none")+
# Reference lines (Kobe thresholds and forecast start year)
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_vline(xintercept = 2015, linetype = "dashed") +
  # Legend adjustments
  # guides(fill = "none",
  #   shape = "none",
  #   color = guide_legend(
  #     override.aes = list(
  #       shape = unname(pch_vals[levels(ss_sensi_plot$Scenario_plot)]),
  #       size = 3.5,
  #       linewidth = 1.2)))+
  facet_wrap(~Species, scales = "free_y") +
  labs(
    x = "Year",
    y = expression("SSB/SSB"[MSY]),
    color = "Scenario",
    shape = "Scenario"
  )+
  theme_classic(base_size = 14) +
  theme(strip.background = element_blank(),
        plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm"))
p57

ggplot2::ggsave("Sensitive_SSB_SSBMSY.png",plot=p57, device = "png", units = "cm",
                width = 34, height = 20)


#--------------------------------------------------------------#
#         Final F/FMSY and SSB/SSBMSY table                    #
#  summarize F/FMSY2015, SSB/SSB2015,F/FMSY2025, SSB/SSB2025   #
#          Base case scenarios of each specie                  #
#ALL status coming from "Base" and 2025 status from "Catch_For"#
#--------------------------------------------------------------#
management_metrics <- list()

for(i in sp){
  
  base <- run_log %>%
    filter(grepl(paste0("^", i, "_Base"), object_name)) %>%
    pull(object_name)
  
  forecast <- run_log %>%
    filter(grepl(paste0("^", i, "_Catch_for"), object_name)) %>%
    pull(object_name)
  
  if(length(base)==0 | length(forecast)==0) next
  
  obj_base <- get(base)
  obj_for  <- get(forecast)
  
  getdq <- function(obj,label){
    x <- obj$derived_quants$Value[obj$derived_quants$Label==label]
    if(length(x)==0) NA else x
  }
  
  f_msy   <- getdq(obj_base,"annF_MSY")
  ssb_msy <- getdq(obj_base,"SSB_MSY")
  
  f_2015 <- getdq(obj_base,"F_2015")
  f_2025 <- getdq(obj_for,"F_2025")
  
  ssb_2015 <- getdq(obj_base,"SSB_2015")
  ssb_2025 <- getdq(obj_for,"SSB_2025")
  
  fr_2015 <- f_2015/f_msy
  fr_2025 <- f_2025/f_msy
  
  br_2015 <- ssb_2015/ssb_msy
  br_2025 <- ssb_2025/ssb_msy
  
  #classify status based on F/FMSY and B/BMSY
  classify <- function(FR, BR){
    case_when(
      FR < 1 & BR > 1 ~ "Not overfished / Not overfishing",
      FR > 1 & BR > 1 ~ "Not overfished / Overfishing",
      FR > 1 & BR < 1 ~ "Overfished / Overfishing",
      FR < 1 & BR < 1 ~ "Overfished / Not overfishing",
      TRUE ~ NA_character_
    )
  }
  
  management_metrics[[i]] <- data.frame(
    Species = i,
    F_2015 = f_2015,
    F_2025 = f_2025,
    F_MSY = f_msy,
    F_FMSY_2015 = fr_2015,
    F_FMSY_2025 = fr_2025,
    SSB_2015 = ssb_2015,
    SSB_2025 = ssb_2025,
    SSB_MSY = ssb_msy,
    SSB_SSBMSY_2015 = br_2015,
    SSB_SSBMSY_2025 = br_2025,
    Status_2015 = classify(fr_2015,br_2015),
    Status_2025 = classify(fr_2025,br_2025)
  )
}

management_metrics <- bind_rows(management_metrics)%>% 
  dplyr::mutate(across(where(is.numeric), ~round(.x, 2)))

#write results in a .csv
write.csv(management_metrics, file = "management_metrics_out.csv", row.names = FALSE)



#---------------------------------------------------------------------------------------------------#
#               Run Cookbook for SS base case scenarios (Carvalho et al., 2021)                     #
#Evaluate  Convergence,Goodness-of-fit, Model consistency, Retrospective analysis,likelihood profile#
#---------------------------------------------------------------------------------------------------#

#-----------------------
#Goodness-of-fit
#-----------------------
png(
  filename = "Goodness_of_fit.png",
  width = 14,
  height = 15,
  units = "cm",
  res = 500,
  type = "windows",
  antialias = "cleartype"
)

sspar(mfrow = c(3,2))
par(
  mfrow = c(3,2),
  mai = c(0.1, 0.1, 0.4, 0.1),   
  omi = c(0.4, 0.4, 0.06, 0.06), 
  mgp = c(5, 0.2, 0),
  bty = "l",cex=0.75
)
blf_gof=SSplotJABBAres(BLF_Base_1950_2015_Obs,add = T,verbose = F,
                  subplots = c("len"))
title(main = "BLF", cex.main = 0.9)

brs_gof=SSplotJABBAres(BRS_Base_1950_2015_Obs,add = T,verbose = F,
                       subplots = c("len"))
title(main = "BRS", cex.main = 0.9)

dol_gof=SSplotJABBAres(DOL_Base_1950_2015_Obs,add = T,verbose = F,
                       subplots = c("len"))
title(main = "DOL", cex.main = 0.9)

fri_gof=SSplotJABBAres(FRI_Base_1950_2015_Obs,add = T,verbose = F,
                       subplots = c("len"))
title(main = "FRI", cex.main = 0.9)

#kgm_gof=SSplotJABBAres(KGM_Base_1950_2015_Obs,add = T,verbose = F,
#                       subplots = c("len"))
#title(main = "KGM", cex.main = 0.9)

lta_gof=SSplotJABBAres(LTA_Base_1950_2015_Obs,add = T,verbose = F,
                       subplots = c("len"))
title(main = "LTA", cex.main = 0.9)

Wah_gof=SSplotJABBAres(WAH_Base_1950_2015_Obs,add = T,verbose = F,
                       subplots = c("len"))
title(main = "WAH", cex.main = 0.9)

mtext(side = 1, outer = TRUE, line = 1.2, "Year")
mtext(side = 2, outer = TRUE, line = 1.2, "Mean length residuals")

dev.off()
#------------------------------------------------------------------


#-----------------------
#likelihood profile
#------------------------
bases <- subset(run_log, grepl("_Base_", object_name) & status == "success")$object_name
bases
dir_out <- file.path(dir, "SS3", "scenarios")
#sp= c("BLF", "BRS", "DOL", "FRI", "KGM","LTA", "WAH")

#Creating the LN(R0) vector to be tested
make_lnr0_vec <- function(sp_out, mult = 3, n = 10, fallback = 0.4) {
  
  p <- sp_out$parameters
  row <- p[p$Label == "SR_LN(R0)", ]
  
  lnr0_base <- row$Value
  lnr0_sd   <- row$Parm_StDev
  
  if (is.na(lnr0_sd) || lnr0_sd <= 0) {
    delta <- fallback
  } else {
    delta <- mult * lnr0_sd
  }
  
  vec <- seq(
    lnr0_base - delta,
    lnr0_base + delta,
    length.out = n
  )
  return(vec)
}

lnr0_list <- list(
  BLF = make_lnr0_vec(BLF_Base_1950_2015_Obs),
  BRS = make_lnr0_vec(BRS_Base_1950_2015_Obs),
  DOL = make_lnr0_vec(DOL_Base_1950_2015_Obs),
  FRI = make_lnr0_vec(FRI_Base_1950_2015_Obs),
  KGM = make_lnr0_vec(KGM_Base_1950_2015_Obs),
  LTA = make_lnr0_vec(LTA_Base_1950_2015_Obs),
  WAH = make_lnr0_vec(WAH_Base_1950_2015_Obs)
)


profile_list <- list()

for (i in bases) {
  
  sp_dir <- file.path(dir_out, run_log$folder[run_log$object_name == i])
  
  # ---- create a clean path ----
  dir_prof <- file.path(sp_dir, "profile")
  
  if (dir.exists(dir_prof)) {
    unlink(dir_prof, recursive = TRUE, force = TRUE)
  }
  
  dir.create(dir_prof, recursive = TRUE, showWarnings = FALSE)
  
  # ---- SS input files ----
  files_to_copy <- c(
    "controlfile.ctl",
    "datafile.dat",
    "forecast.ss",
    "starter.ss"
  )
  src_files <- file.path(sp_dir, files_to_copy)
  print(src_files)
  print(file.exists(src_files))
  stopifnot(all(file.exists(src_files)))
  #copy SS input files to profile path
  file.copy(
    from = src_files,
    to   = dir_prof,
    overwrite = TRUE
  )
  #check
  print(list.files(dir_prof))
  
  # ---- adjust starter to assign the profile control file ----
  starter <- r4ss::SS_readstarter(file.path(dir_prof, "starter.ss"))
  starter$ctlfile <- 'control_profile.ss'
  starter$prior_like <- 1
  r4ss::SS_writestarter(starter, dir = dir_prof, overwrite = TRUE)
  
  #STOCK SYNTHESIS MUST HAVE AT LEAST ONE PARAMETER AT PHASE 1
  #if there's no parameter to estimate in phase 1 the likelihood profile will change the selectivity to phase 1, since profiling R0 means phase -1
  #changing phase to 2 or 1 in selectivity peak or inflection (SL50, SL95) and rewrite the controlfile
  dat<- r4ss::SS_readdat(file.path(dir_prof, "datafile.dat"))      #Data SS file
  ctl <- r4ss::SS_readctl(file.path(dir_prof, "controlfile.ctl"),verbose = TRUE,datlist = dat, use_datlist = TRUE) #Control SS file
  ctl$size_selex_parms$PHASE[1] <- ifelse( ctl$size_selex_parms$PHASE[1] > 0, pmin(ctl$size_selex_parms$PHASE[1], 2),ctl$size_selex_parms$PHASE[1])
  SS_writectl(ctl, file.path(dir_prof,"controlfile.ctl"), overwrite = TRUE)
  
  #taking lnr0 vector
  sp_i= strsplit(i, "_")[[1]][1]
  lnr0_vec <- lnr0_list[[sp_i]]
  stopifnot(!is.null(lnr0_vec))
  
  # ---- run likelihood profile ----
  r4ss::profile(
    dir = dir_prof,
    oldctlfile = "controlfile.ctl",
    newctlfile = "control_profile.ss",
    string = "SR_LN(R0)",   
    profilevec = lnr0_vec,
    exe = file.path(dir_input, "ss3.exe"),
    overwrite = TRUE,
    verbose = TRUE
  )
  
  # ---- read results ----
  prof_models <- try(
    r4ss::SSgetoutput(
      dirvec = dir_prof,
      keyvec = seq_along(lnr0_vec),
      verbose = FALSE
    ),
    silent = TRUE
  )
  
  if (inherits(prof_models, "try-error")) {
    warning(paste("Profile output failed for", i))
    next
  }
  #summarizing results
  profile_list[[i]] <- try(
    r4ss::SSsummarize(prof_models),
    silent = TRUE
  )
}
#--------------------------------
{ #Plot the likelihood profile
png(
  filename = "likelihood_profile.png",
  width = 14,
  height = 16,
  units = "cm",
  res = 500,
  type = "windows",
  antialias = "cleartype"
)

# ---- layout ----
  sspar(mfrow = c(4,2))
  par(
    mfrow = c(4,2),
    mai = c(0.1, 0.1, 0.4, 0.1),   
    omi = c(0.5,0.5, 0.06, 0.06),  
    mgp = c(0, 0.2, 0),
    bty = "l",cex=0.75
  )
  
# ---- plot ----
blf_prof=profile_list$BLF_Base_1950_2015_Obs
blf_prof= r4ss::SSplotProfile(
  summaryoutput = blf_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",          
  ylab = "",                   
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "BLF",  cex.main = 0.9)

brs_prof=profile_list$BRS_Base_1950_2015_Obs
brs_prof= r4ss::SSplotProfile(
  summaryoutput = brs_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",          
  ylab = "",                   
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "BRS",  cex.main = 0.9)

dol_prof=profile_list$DOL_Base_1950_2015_Obs
dol_prof= r4ss::SSplotProfile(
  summaryoutput = dol_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",          
  ylab = "",                   
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "DOL",  cex.main = 0.9)

fri_prof=profile_list$FRI_Base_1950_2015_Obs
fri_prof= r4ss::SSplotProfile(
  summaryoutput = fri_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",         
  ylab = "",                  
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "FRI",  cex.main = 0.9)

kgm_prof=profile_list$KGM_Base_1950_2015_Obs
kgm_prof= r4ss::SSplotProfile(
  summaryoutput = kgm_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",         
  ylab = "",                   
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "KGM",  cex.main = 0.9)

lta_prof=profile_list$LTA_Base_1950_2015_Obs
lta_prof= r4ss::SSplotProfile(
  summaryoutput = lta_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",          
  ylab = "",                   
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "LTA",  cex.main = 0.9)

wah_prof=profile_list$WAH_Base_1950_2015_Obs
wah_prof= r4ss::SSplotProfile(
  summaryoutput = wah_prof,
  profile.string = "SR_LN(R0)",
  components = c("TOTAL", "Catch","Length_comp","Recruitment"),
  component.labels =c("TOTAL", "Catch","Length_comp","Recruitment"),
  minfraction = 0,
  exact = TRUE,
  profile.label = "",          
  ylab = "",                   
  add_cutoff = FALSE,
  pwidth = 7,
  pheight = 5,
  lwd = 2,
  pch = ""
)
title(main = "WAH", cex.main = 0.9)

mtext(side = 1, outer = TRUE, line = 1.5, expression(paste("ln(", R[0], ")")))
mtext(side = 2, outer = TRUE, line = 1.5, "Change in -log-likelihood")

dev.off()
}

#------------------------------------------------------------------------------


# -----------------------
# Retrospective analysis
# -----------------------
library(r4ss)

# filtra bases
bases <- subset(run_log, grepl("_Base_", object_name) & status == "success")
exe_path <- file.path(dir_input, "ss3.exe")
n_peels=7

# ----------------------------
# 1) First run retrospectives
# ---------------------------
for (i in seq_len(nrow(bases))) {
  
  bd_folder <- bases$folder[i]
  bd_object <- bases$object_name[i]
  
  run_dir <- file.path(dir_out, bd_folder)
  
  # ---- create a clean retrospective folder------#
  retro_dir <- file.path(run_dir, "retrospectives")
  
  if (dir.exists(retro_dir)) {
    unlink(retro_dir, recursive = TRUE, force = TRUE)
  }
  
  cat("\nRunning retro for:", bd_object, "\n")
  cat("Directory:", run_dir, "\n")
  
  file.copy(exe_path, file.path(run_dir, "ss3.exe"), overwrite = TRUE)
  
  #run retrospectives (take last value out and refit the model)
  r4ss::retro(
    dir       = run_dir,
    years     = 0:n_peels,      
    exe       = exe_path,
    overwrite = TRUE,
    verbose   = FALSE
  )
}

# -------------------------------------
# 2) function to read retrospectives
# -------------------------------------
make_retro_object <- function(run_dir, n_peel = 7) {
  retro_root <- file.path(run_dir, "retrospectives")
  dirs <- file.path(retro_root, paste0("retro", 0:n_peel))
  r4ss::SSgetoutput(dirvec = dirs, verbose = FALSE)
}

# -----------------------
# 3) Read and summarize
# -----------------------
retro_models <- list()
retro_list   <- list()

for (i in seq_len(nrow(bases))) {

  bd_folder <- bases$folder[i]
  bd_object <- bases$object_name[i]
  
  run_dir <- file.path(dir_out, bd_folder)
  
  cat("Reading retro for:", bd_object, "\n")
  
  retro_models[[bd_object]] <- make_retro_object(run_dir, n_peel = n_peels)
  
  retro_list[[bd_object]] <- r4ss::SSsummarize(
    retro_models[[bd_object]],
    verbose = FALSE
  )
}

# -----------------------
# 4) Checking
# -----------------------
names(retro_list)
length(retro_list[[1]])
class(retro_list[[1]][[1]])

#------------------------
#ploting retrospectives
#------------------------
{png(
  filename = "Retrospectives.png",
  width = 12,
  height = 15,
  units = "cm",
  res = 500,
  type = "windows",
  antialias = "cleartype"
)

sspar(mfrow = c(4,2))
par(
  mfrow = c(4,2),
  mai = c(0.1, 0.1, 0.4, 0.1),   
  omi = c(0.4, 0.4, 0.06, 0.06), 
  mgp = c(5, 0.2, 0),
  bty = "l",cex=0.75
)

blf_retro = SSplotRetro(retro_list$BLF_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "BLF", cex.main = 0.9)

brs_retro = SSplotRetro(retro_list$BRS_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "BRS", cex.main = 0.9)

dol_retro = SSplotRetro(retro_list$DOL_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "DOL", cex.main = 0.9)

fri_retro = SSplotRetro(retro_list$FRI_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "FRI", cex.main = 0.9)

kgm_retro = SSplotRetro(retro_list$KGM_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "KGM", cex.main = 0.9)

lta_retro = SSplotRetro(retro_list$LTA_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "LTA", cex.main = 0.9)

wah_retro = SSplotRetro(retro_list$WAH_Base_1950_2015_Obs,subplots = "F", add = T, forecast = F, legend = T, verbose = F,legendloc = "topleft", legendcex = 0.8)
title(main = "WAH", cex.main = 0.9)

mtext(side = 1, outer = TRUE, line = 1.2, "Year")
mtext(side = 2, outer = TRUE, line = 1.2, "Fishing mortality (F)")

dev.off()
}
#------------------------------------------------------------------



#-----------------------------------------------
# Model uncertainty delta-Multivariate lognormal 
#approximation to generate joint error distributions 
# KOBE PLOT building
#-----------------------------------------------
{png(
  filename = "Kobe_plot.png",
  width = 22,
  height = 19,
  units = "cm",
  res = 500,
  type = "windows",
  antialias = "cleartype"
)
  
  sspar(mfrow = c(4,4))
  par(
    mfrow = c(3,3),
    mai = c(0.1, 0.1, 0.4, 0.1),   
    omi = c(0.4, 0.4, 0.06, 0.06), 
    mgp = c(5, 0.2, 0),
    bty = "l",cex=0.75
  )
blf_mvln=SSdeltaMVLN(BLF_Base_1950_2015_Obs, plot = TRUE)
title(main = "BLF", cex.main = 0.9)

brs_mvln=SSdeltaMVLN(BRS_Base_1950_2015_Obs, plot = TRUE)
title(main = "BRS", cex.main = 0.9)

#>>Non-convergence of MSY quantities- USING SSB at 40% (SSB40)<<
dol_mvln=SSdeltaMVLN(DOL_Base_1950_2015_Obs, plot = TRUE)
title(main = "DOL", cex.main = 0.9)

fri_mvln=SSdeltaMVLN(FRI_Base_1950_2015_Obs, plot = TRUE)
title(main = "FRI", cex.main = 0.9)

kgm_mvln=SSdeltaMVLN(KGM_Base_1950_2015_Obs, plot = TRUE)
title(main = "KGM", cex.main = 0.9)

lta_mvln=SSdeltaMVLN(LTA_Base_1950_2015_Obs, plot = TRUE)
title(main = "LTA", cex.main = 0.9)

wah_mvln=SSdeltaMVLN(WAH_Base_1950_2015_Obs, plot = TRUE)
title(main = "WAH", cex.main = 0.9)

mtext(side = 1, outer = TRUE, line = 1.2, expression("SSB/SSB"[MSY]))
mtext(side = 2, outer = TRUE, line = 1.2, expression("F/F"[MSY]))

dev.off()
}
    
    
  

            #x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#x#xx#x#xx#x#xx#x#x#x#x#x#
            #.................. End of  Small tunas stock assessment.................. #
            #x#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#xx#x#x#x#xx#x#x#x#x#x