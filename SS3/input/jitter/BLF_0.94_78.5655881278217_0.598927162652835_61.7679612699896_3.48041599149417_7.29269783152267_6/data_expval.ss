#V3.30.23.1;_safe;_compile_date:_Dec  5 2024;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-ost/ss3-source-code

#_Start_time: Mon Mar  3 20:27:53 2025
#_expected_values
#C should work with SS version:
#C file created using an r4ss function
#C file write time: 2025-03-03  20:24:53
#V3.30.23.1;_safe;_compile_date:_Dec  5 2024;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.2
1950 #_StartYr
2025 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
2 #_Nsexes: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
6 #_Nages=accumulator age, first age is always age 0
1 #_Nareas
1 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=predator(M2) 
#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type fishery_timing area catch_units need_catch_mult fleetname
 1 -1 1 1 0 Fishery  # 1
#Bycatch_fleet_input_goes_next
#a:  fleet index
#b:  1=include dead bycatch in total dead catch for F0.1 and MSY optimizations and forecast ABC; 2=omit from total catch for these purposes (but still include the mortality)
#c:  1=Fmult scales with other fleets; 2=bycatch F constant at input value; 3=bycatch F from range of years
#d:  F or first year of range
#e:  last year of range
#f:  not used
# a   b   c   d   e   f 
#_Catch data: year, seas, fleet, catch, catch_se
#_catch_se:  standard error of log(catch)
#_NOTE:  catch data is ignored for survey fleets
-999 1 1 1.62249e-17 0.01
1950 1 1 27.639 0.01
1951 1 1 26.407 0.01
1952 1 1 24.001 0.01
1953 1 1 26.52 0.01
1954 1 1 23.272 0.01
1955 1 1 22.911 0.01
1956 1 1 34.044 0.01
1957 1 1 40.835 0.01
1958 1 1 64.248 0.01
1959 1 1 59.223 0.01
1960 1 1 91.275 0.01
1961 1 1 53.773 0.01
1962 1 1 64.058 0.01
1963 1 1 80.593 0.01
1964 1 1 97.126 0.01
1965 1 1 113.652 0.01
1966 1 1 130.335 0.01
1967 1 1 197.048 0.01
1968 1 1 199.764 0.01
1969 1 1 29.047 0.01
1970 1 1 116.414 0.01
1971 1 1 40.718 0.01
1972 1 1 159.111 0.01
1973 1 1 161.433 0.01
1974 1 1 105.922 0.01
1975 1 1 73.44 0.01
1976 1 1 82.757 0.01
1977 1 1 91.721 0.01
1978 1 1 570.399 0.01
1979 1 1 252.045 0.01
1980 1 1 259.766 0.01
1981 1 1 479.236 0.01
1982 1 1 339.728 0.01
1983 1 1 241.802 0.01
1984 1 1 461.874 0.01
1985 1 1 733.952 0.01
1986 1 1 620.791 0.01
1987 1 1 706.647 0.01
1988 1 1 601.368 0.01
1989 1 1 558.542 0.01
1990 1 1 201.652 0.01
1991 1 1 288.618 0.01
1992 1 1 436.597 0.01
1993 1 1 534.713 0.01
1994 1 1 383.828 0.01
1995 1 1 293.211 0.01
1996 1 1 345.097 0.01
1997 1 1 173.813 0.01
1998 1 1 346.159 0.01
1999 1 1 433.18 0.01
2000 1 1 557.707 0.01
2001 1 1 562.754 0.01
2002 1 1 811.706 0.01
2003 1 1 524.054 0.01
2004 1 1 493.007 0.01
2005 1 1 475.221 0.01
2006 1 1 500.221 0.01
2007 1 1 408.845 0.01
2008 1 1 311.004 0.01
2009 1 1 229.773 0.01
2010 1 1 320.837 0.01
2011 1 1 540.03 0.01
2012 1 1 1059.37 0.01
2013 1 1 652.467 0.01
2014 1 1 914.922 0.01
2015 1 1 1115.72 0.01
2016 1 1 396.09 0.01
2017 1 1 474.737 0.01
2018 1 1 428.859 0.01
2019 1 1 502.32 0.01
2020 1 1 603.345 0.01
2021 1 1 577.154 0.01
2022 1 1 488.935 0.01
2023 1 1 516.253 0.01
2024 1 1 537.545 0.01
2025 1 1 562.547 0.01
-9999 0 0 0 0
#
#_CPUE_and_surveyabundance_and_index_observations
#_units: 0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=exp(recdev); 36=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)
#_errtype:  -1=normal; 0=lognormal; 1=lognormal with bias correction; >1=df for T-dist
#_SD_report: 0=not; 1=include survey expected value with se
#_note that link functions are specified in Q_setup section of control file
#_dataunits = 36 and 35 should use Q_type 5 to provide offset parameter
#_fleet units errtype SD_report
1 1 0 0 # Fishery
#_year month index obs err
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note: only enter units and errtype for fleets with discard 
# note: discard data is the total for an entire season, so input of month here must be to a month in that season
#_fleet units errtype
# -9999 0 0 0.0 0.0 # terminator for discard data 
#
0 #_use meanbodysize_data (0/1)
#_COND_0 #_DF_for_meanbodysize_T-distribution_like
# note:  type=1 for mean length; type=2 for mean body weight 
#_year month fleet part type obs stderr
#  -9999 0 0 0 0 0 0 # terminator for mean body size data 
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
6 # binwidth for population size comp 
4 # minimum size in the population (lower edge of first bin and size at age 0.00) 
118 # maximum size in the population (lower edge of last bin) 
1 # use length composition data (0/1/2) where 2 invokes new comp_comtrol format
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined sex below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  consecutive index for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_Using old format for composition controls
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.001 0 0 0 0 0.001 #_fleet:1_Fishery
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  (0=combined; 1=discard; 2=retained
15 #_N_LengthBins
 10 16 22 28 34 40 46 52 58 64 70 76 82 88 94
#_year month fleet sex part Nsamp datavector(female-male)
 1977 7 1 0 0 543  0.534975 0.534975 0.534975 0.535021 0.594013 5.81791 47.4188 74.7593 150.129 142.851 75.2498 31.7334 9.47524 2.12494 0.7065 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 1991 7 1 0 0 236  0.232512 0.232512 0.232512 0.232568 0.303344 6.53325 54.2122 54.6318 45.4269 42.5749 21.7293 7.41361 1.62204 0.381041 0.241571 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 1998 7 1 0 0 469  0.462069 0.462069 0.462069 0.4621 0.501721 4.00061 31.473 45.4383 99.6062 136.028 97.4453 40.6626 9.71331 1.71449 0.568619 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 1999 7 1 0 0 511  0.503448 0.503448 0.503448 0.503488 0.554536 5.05789 40.7569 65.8367 133.7 128.738 78.151 39.9023 12.9926 2.60902 0.6876 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 2007 7 1 0 0 219  0.215764 0.215764 0.215764 0.215785 0.242911 2.63298 21.2913 29.4536 54.1288 59.1511 34.6473 12.931 2.88766 0.532394 0.237848 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 2010 7 1 0 0 306  0.301478 0.301478 0.301478 0.301499 0.329034 2.76552 22.5863 45.0106 103.846 83.4431 32.0168 10.708 2.99803 0.744798 0.346195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 2019 7 1 0 0 204  0.200985 0.200985 0.200985 0.200995 0.213062 1.28284 10.555 31.857 84.6529 57.5028 13.934 2.28626 0.484781 0.225421 0.201996 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
-9999 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
#
6 #_N_age_bins
 0 1 2 3 4 5
1 #_N_ageerror_definitions
 -1 -1 -1 -1 -1 -1 -1
 0.001 0.001 0.001 0.001 0.001 0.001 0.001
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined sex below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  parm number for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
-1 0.001 0 0 0 0 0.001 #_fleet:1_Fishery
3 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex*length distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_year month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_Use_MeanSize-at-Age_obs (0/1)
#
0 #_N_environ_variables
# -2 in year will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#_year variable value
#
# Sizefreq data. Defined by method because a fleet can use multiple methods
0 # N sizefreq methods to read (or -1 for expanded options)
#
0 # do tags (0/1)
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#_year, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
#_year, seas, fleet, age/size, bin, selex_prior, prior_sd
# feature not yet implemented
#
999

