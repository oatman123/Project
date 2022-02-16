#=============================================================================================================#
#																																																							#
#	Model: Multi-unit Lifecycle Loss and Exposure Analyzer (mollea) - Volume Projection													#
#	Description: A. Data creation 																																							#
#  																																																						#
# Moody's Analytics (Ziv Rubin)																																								#
#	Created: 			09 June, 2020																																									#
#	Last Update: 	09 November, 2020																																							#
#	Version: 1.0																																																#
#																																																							#
#=============================================================================================================#

##TEST

#=============================================================================================================#
#                  												   Script setup																									 ####
#=============================================================================================================#

# # General 
# rm(list=ls())                                                                                                 #	remove all data from enviroment
# gc()                                                                                                          # collect all garbage to free memory
# options(warn=-1)                                                                                              # turn warnings off
# 
# #=======================================#
# #								Options							 		# 
# #=======================================#
# 
# # location of main model folder (all results will be placed in sub-folders here)
# dir_main <- "Z:/zrubin/volume_projection/"
# 
# # location of model data folder
# dir_data <- "Z:/zrubin/volume_projection/data/"
# 
# # location of mollea master data (raw loans data)
# dir_raw_data <- "N:/Inputs Data/pragma/20191231/Production/"
# 
# # Raw csv data file name (without file extension)
# raw_data_name <- "prod_multi_full_19q4_newLBA"
# 
# # DataBuffet downloaded basket file name (without file extension)
# db_data_name <- "Basket_2020-07-16_14h_13m"
# 
# # Create new aggregated data ("yes"/"no") 
# new_data <- "yes"                  	                                                                          # "yes" will run "A_data_creation.R" to create a fresh data file; "no" will load an old version of "data_aggr.Rdata" from data folder
# 
# # Load new raw MOLLEA data ("yes"/"no")
# new_raw_data <- "no"                                                                                          # "yes" will load new raw MOLLEA data in "A_data_creation.R"; "no" will use saved raw data from data folder
# 
# # Define bounds for historical data
# f_hist <- 2000                      	                                                                        # define first year of historical data
# l_hist <- 2019                        	                                                                      # define last year of historical data
# 
# # Define bounds for model development
# f_dev_Rental_New <- 2004                                                                                      # define first year of Rental New development
# f_dev_Rental_Existing <- 2004                                                                                 # define first year of Rental Existing development
# f_dev_Refinance <- 2004                                                                                       # define first year of Refinance development
# l_dev <- 2019                                                                                                 # define last year of development
# oos_start <- 2015                                                                                             # define first year of out-of-smaple analysis
# 
# # Define end of forecast
# l_frcst <- 2025
# 
# #=======================================#
# #								Folders							 		# 
# #=======================================#
# # folder locations
# setwd(dir_main)                                                                                              	# set main directory
# 
# #===========================================#
# # 				Install packages and tools		 		#
# #===========================================#
# 
# # Automatically install missing packages
# ID_package <- c(
# 	"plyr"              # for data management
# 	,"dplyr"            # for data management
# 	,"tidyr"            # for data management
# 	,"zoo"              # for time management
# 	,"janitor"          # for clean data frames
# 	#,"reshape2"         # for expanding dataframe
# 	,"ggplot2"          # for plots
# 	#,"sandwich"        # for robust standard error
# 	#,"fitdistrplus"    # distribution fitting
# 	#,"nlme"            # heteroscedasticity test 
# 	,"lmtest"          # heteroscedasticity test 
# 	,"openxlsx"         # excel files
# 	,"fastDummies"      # factor to dummies
# 	#,"rsq"             # rsq
# 	,"MLmetrics"        # MAPE and RMSE
# 	,"stats"           # BIC
# 	#,"rlist"           # bind list by column
# 	,"imputeTS"         # impute NAs in time series
#		#,"tseries"          # units root test
# 	,"plm"              # panel unit root tests
# 	,"strucchange"      # structural breaks
# 	#,"weights"         # weighted correlation
# 	,"outreg"          # tidy regression results
# )
# new_packages <- ID_package[!(ID_package %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# lapply(ID_package, require, character.only = TRUE)
# 
# # ggplot Moody’s template
# source(file = file.path("ecca_template.R"))
# invisible(eccaTheme(14)) # font size
# 
# rm(list = c("new_packages", "ID_package"))

#=============================================================================================================#
#                  											   			Load raw data 																						 ####
#=============================================================================================================#

#=====================#
# Load raw MOLLEA Data
#=====================#
# Load raw csv mollea master data from DPA output location or load RData file from modeling data folder
if(new_raw_data == "yes") {
	data_raw <- read.csv(file = file.path(dir_raw_data, paste0(raw_data_name, ".csv")),
																								header = TRUE, sep = ",", na.strings = c("", " ", "NA"))
	save(data_raw, file = file.path(dir_data, paste0(raw_data_name, ".RData")))
} else {
		load(file = file.path(dir_data, paste0(raw_data_name, ".RData")))
}

# transfom all variables to lowercase
data_raw <- clean_names(data_raw)

#=============================================================================================================#
#                  										Load and prepare econ variables 																		 ####
#=============================================================================================================#

#============================================#
# Load econ variables from DataBuffet
#============================================#

# load economic data file downloaded from DataBuffet basket
data_econ_raw <- read.xlsx(file.path(dir_data, paste0(db_data_name, ".xlsx")))

# Clean raw data
data_econ_raw <- data_econ_raw %>%
	rename(yvintage = "Mnemonic:") %>%                                                                          # fix first column name
	filter(!yvintage %in% c("Description:", "Source:", "Native Frequency:", "Geography:")) %>%                  # remove redundant rows
	mutate(yvintage = as.Date(as.numeric(yvintage), origin = "1899-12-30")) %>%                                 # fix date
	mutate(yvintage = format(yvintage, "%Y"))                                                                   # change date to year number

# wide to long
data_econ_raw <- pivot_longer(data_econ_raw, cols = names(data_econ_raw[-1]),
																names_to = c(".value", "geo"),
																names_pattern = "(.*)[.](.*)",
																values_to = c("FLBRQ", "FGDP", "FRMORT5YQ", "FSTOCKPQ", "FPOPQ", "FRPRIMEQ", 
																							"FCPWTI", "FRGT5YQ", "FRGT10YQ", "FLBEQ", "FCPIRENTQ", 
																							"FCPIQ", "FHSTQ"))

# Rename vars and make all data numeric
data_econ_raw <- data_econ_raw %>% 
	mutate(yvintage = as.numeric(yvintage)) %>%
	mutate(ump_rate = as.numeric(FLBRQ)) %>%
	mutate(emp = as.numeric(FLBEQ)) %>%
	rename(gdp = "FGDP$Q") %>%
	mutate(gdp = as.numeric(gsub("ND", "", gdp))) %>%
	mutate(hstarts = as.numeric(FHSTQ)) %>%
	mutate(rmort5y = as.numeric(FRMORT5YQ)) %>%
	mutate(stockp = as.numeric(FSTOCKPQ)) %>%
	mutate(rprime = as.numeric(FRPRIMEQ)) %>%
	mutate(cpwti = as.numeric(FCPWTI)) %>%
	mutate(rgt5y = as.numeric(FRGT5YQ)) %>%
	mutate(rgt10y = as.numeric(FRGT10YQ)) %>%
	mutate(cpi_rent = as.numeric(FCPIRENTQ)) %>%
	mutate(cpi = as.numeric(FCPIQ)) %>%
	mutate(pop = as.numeric(FPOPQ)) %>%
	select(geo, yvintage, ump_rate, emp, gdp, hstarts, rmort5y, stockp, rprime, 
				 cpwti, rgt5y, rgt10y, cpi_rent, cpi, pop)

# Subset to province
data_econ_province <- data_econ_raw %>%
	filter(!geo %in% c("ICAN", "IUSA") & yvintage <= l_frcst) %>%                                               # remove national observations and trim data to last forecast year
	mutate(province = substr(geo, 6, 7)) %>%                                                                    # change geo to province codes
	mutate(province = case_when(
		province == "NL" ~ "NF",
		province == "QC" ~ "QU",
		TRUE ~ province)) %>%                                                                                     # fix Newfoundland and Quebec codes
	select(province, yvintage, ump_rate, emp, gdp, hstarts, cpi_rent, cpi, pop)                                 # keep only province econs

# subset to national econs
data_econ_national <- data_econ_raw %>%
	filter(geo == "ICAN") %>%
	select(yvintage, rmort5y, stockp, rprime, rgt5y, rgt10y) %>%                                                # Canadian national econs and oil prices
	left_join(filter(data_econ_raw, geo == "IUSA")[, c("yvintage", "cpwti")], by = "yvintage") %>%
	filter(yvintage <= l_frcst)

#============================================#
# Load econ variables from CMHC
#============================================#

# Vacancy rates
data_vacancy <- read.xlsx(file.path(dir_data, "vacancy_rates_pr.xlsx"))                                       # load vacancy rates data (downloaded from CMHC data portal for each province, data is for Q3 of each year)
data_vacancy <- pivot_longer(data_vacancy, cols = names(data_vacancy[-1]), 
														 names_to = "province", values_to = "vac_rate")                                   # wide to long

# Condo RPI
data_rpi_condo <- read.xlsx(file.path(dir_data, "condo_rpi_pr.xlsx"))                                         # load cond RPI data (PSAD data sent from JS)
data_rpi_condo <- pivot_longer(data_rpi_condo, cols = names(data_rpi_condo[-1]), 
												 names_to = "province", values_to = "rpi_condo")                                      # wide to long
data_rpi_condo$yvintage <- as.numeric(substr(data_rpi_condo$mvintage, 1, 4))                                  # transform date to year
data_rpi_condo <- ddply(data_rpi_condo, ~ yvintage + province, summarise, rpi_condo = mean(rpi_condo))        # aggregate from months to years

# Singles RPI
data_rpi_singles <- read.xlsx(file.path(dir_data, "singles_rpi_pr.xlsx"))                                     # load cond RPI data (PSAD data sent from JS)
data_rpi_singles <- pivot_longer(data_rpi_singles, cols = names(data_rpi_singles[-1]), 
															 names_to = "province", values_to = "rpi_singles")                              # wide to long
data_rpi_singles$yvintage <- as.numeric(substr(data_rpi_singles$mvintage, 1, 4))                              # transform date to year
data_rpi_singles <- ddply(data_rpi_singles, ~ yvintage + province, summarise, rpi_singles = mean(rpi_singles))# aggregate from months to years

# Add national RPI and vacancy rates to national data
data_econ_national <- data_econ_national %>%
	left_join(filter(data_rpi_condo, province == "Canada")[, c("yvintage", "rpi_condo")], by = "yvintage") %>%
	left_join(filter(data_vacancy, province == "Canada")[, c("yvintage", "vac_rate")], by = "yvintage") %>%
	rename(rpi_national = rpi_condo, vac_rate_national = vac_rate)

#============================================#
# Join and Aggregate econ variables
#============================================#

# Join all econs together based on an empty frame and add region variable
data_econ <- data_econ_province %>% 
	left_join(data_vacancy, by = c("yvintage", "province")) %>%	
	left_join(data_rpi_condo, by = c("yvintage", "province")) %>%
	left_join(data_rpi_singles, by = c("yvintage", "province")) %>%
	left_join(data_econ_national, by = "yvintage") %>%
	mutate(regioncd_name = case_when(
		province %in% c("NF", "PE", "NS", "NB")             ~ "Atlantic",
		province == "QU"                                    ~ "Quebec",
		province == "BC"                                    ~ "BC",
		province %in% c("MB", "SK", "AB", "YK", "NW", "NU") ~ "Prairie and Territories",
		province == "ON"                                    ~ "Ontario"
	))                                                                                                          # region name var
	
# Aggregate econs into regions
data_econ_aggr <- ddply(data_econ, ~ yvintage + regioncd_name, summarise,
													vac_rate = weighted.mean(vac_rate, pop, na.rm = TRUE),
													vac_rate_national = mean(vac_rate_national, na.rm = TRUE),
													rpi_condo = weighted.mean(rpi_condo, pop, na.rm = TRUE),
													rpi_national = mean(rpi_national, na.rm = TRUE),
													rpi_singles = weighted.mean(rpi_singles, pop, na.rm = TRUE),
													ump_rate = weighted.mean(ump_rate, pop, na.rm = TRUE),
													emp = sum(emp, na.rm = TRUE),
													gdp = weighted.mean(gdp, pop, na.rm = TRUE),
													hstarts = sum(hstarts, na.rm = TRUE),
													cpi = weighted.mean(cpi, pop, na.rm = TRUE),
													cpi_rent = weighted.mean(cpi_rent, pop, na.rm = TRUE),
													pop = sum(pop, na.rm = TRUE),
													rmort5y = mean(rmort5y, na.rm = TRUE),
													stockp = mean(stockp, na.rm = TRUE),
													rprime = mean(rprime, na.rm = TRUE),
													cpwti = mean(cpwti, na.rm = TRUE),
													rgt5y = mean(rgt5y, na.rm = TRUE),
													rgt10y = mean(rgt10y, na.rm = TRUE),
													rmort_spread = mean(rmort5y, na.rm = TRUE) - mean(rprime, na.rm = TRUE))

# Consolidate RPIs (singles RPI for Atlantic region condo for the rest)
data_econ_rpi <- data_econ_aggr                                                                           # keep non-consolidated data for RPI test
data_econ_aggr <- data_econ_aggr %>% 
	mutate(rpi = ifelse(regioncd_name == "Atlantic", rpi_singles, rpi_condo)) %>%
	select(-c(rpi_condo, rpi_singles))

#============================================#
# Add transformations and lags
#============================================#

# Add transformations for candidate econs
rhs_econ_yd <- c("vac_rate", "vac_rate_national", "ump_rate", "emp", "hstarts", "rmort5y", 
									 "rprime", "rgt5y", "rgt10y", "rmort_spread", "pop")
data_trans_yd <- lapply(rhs_econ_yd, function(x) {
	data_econ_aggr %>% group_by(regioncd_name) %>%
		mutate( !!paste0("yd_", x) := get(x) - 
																		dplyr::lag(get(x), 1, na.pad = TRUE, order_by = yvintage)) %>%
		ungroup(regioncd_name) %>%
		select(paste0("yd_", x))
})                                                                                                            # yd_ variables transformation
data_trans_yd <- bind_cols(data_trans_yd)                                                                     # bind transformed lists to dataframe
rhs_econ_yp <- c("rpi", "rpi_national", "emp", "gdp", "hstarts", "stockp", "cpwti", "cpi_rent", "pop")
data_trans_yp <- lapply(rhs_econ_yp, function(x) {
	data_econ_aggr %>% group_by(regioncd_name) %>%
		mutate( !!paste0("yp_", x) := (get(x) / 
																	 	dplyr::lag(get(x), 1, na.pad = TRUE, order_by = yvintage)) - 1) %>%
		ungroup(regioncd_name) %>%
		select(paste0("yp_", x))
})                                                                                                            # yp_ variables transformation
data_trans_yp <- bind_cols(data_trans_yp)                                                                     # bind transformed lists to dataframe
data_econ_aggr <- cbind(data_econ_aggr, data_trans_yd, data_trans_yp)                                         # bind transformed to econ data

# Add lags to all econs and their transformations
rhs_for_lags_yd <- as.vector(outer("yd", rhs_econ_yd, paste, sep = "_"))
rhs_for_lags_yp <- as.vector(outer("yp", rhs_econ_yp, paste, sep = "_"))
rhs_for_lags <- c(unique(c(rhs_econ_yd, rhs_econ_yp)), rhs_for_lags_yd, rhs_for_lags_yp)                      # declare vars for lags (all econs with all transformations)
max_lag <- 2                                                                                                  # define maximum lag (in quarters)
data_lags <- lapply(rhs_for_lags, function(x) {
	lapply(seq(1:max_lag), function(y) {
		data_econ_aggr %>% group_by(regioncd_name) %>%
			mutate( !!paste0(x, "_", y) := dplyr::lag(get(x), y, na.pad = TRUE, order_by = yvintage)) %>%
			ungroup(regioncd_name) %>%
			select(paste0(x, "_", y))
	})
})
data_lags <- bind_cols(data_lags)                                                                             # bind lag lists to dataframe
data_econ_aggr <- cbind(data_econ_aggr, data_lags)                                                            # bind lags dataframe to econ data

rm(list = c("data_econ_raw", "data_econ_province", "data_vacancy", "data_rpi_condo", "data_rpi_singles",
						"data_econ_national", "data_econ", "rhs_econ_yd", "rhs_econ_yp", "data_trans_yd", 
						"data_trans_yp", "rhs_for_lags_yd", "rhs_for_lags_yp", "rhs_for_lags", "data_lags", "max_lag"))
gc()

#=============================================================================================================#
#                  									   Prepare raw data for aggregation 																	 ####
#=============================================================================================================#

# select and transform relevant variables
data_raw_aggr <- data_raw %>% 
	filter(age_to_orig == 1                                                                                     # keep only one observation per loan
				 ,product_n %in% c(30, 31, 38, 37, 32, 33)	                                                          # keep only relevant products
				 ,loan_id != 90403684
				 ,loan_id != 90157157                                                                                 # remove large outliers in Quebec in 2009Q1
				 ,loan_id != 90261033                                                                                 # remove low price outlier in Ontario in 2006Q2
				 ,loan_id != 90256124                                                                                 # remove low price outlier in Quebec in 2006Q2
				 ,loan_id != 90565540                                                                                 # remove low price outlier in Ontario in 2013Q4
				 ,loan_id != 90566043                                                                                 # remove low price outlier in Ontario in 2013Q4
				 ,loan_id != 90433434) %>%                                                                            # remove high price outlier in Praire in 2009Q1
	mutate(multi_value = ifelse(loan_id == 90495565, multi_value * (16554150 / 22037100), multi_value)) %>%     # fix value of high price outlier in BC in 2011
	select(loan_id, start_date, product_n, multi_value, units_n, province_n, province) %>%                      # select variables to include
	mutate(price = multi_value / units_n) %>%	 	                                                                # calculate new value per unit
	mutate(below_7_units = case_when(
																	product_n == 30 ~ 1,
																	product_n == 31 ~ 1,
																	product_n == 38 ~ 0,
																	product_n == 37 ~ 1,
																	product_n == 32 ~ 0,
																	product_n == 33 ~ 0
																)) %>%                                                                        # create below 7 units product indicator for share down
	mutate(product_n = case_when(
															product_n == 30 ~ 1,
															product_n == 31 ~ 2,
															product_n == 38 ~ 3,
															product_n == 37 ~ 3,
															product_n == 32 ~ 1,
															product_n == 33 ~ 2
														)) %>%                                                                            # create new product number based on new segmentation
	mutate(product_name = case_when(
																	product_n == 1 ~ "Rental_New",
																	product_n == 2 ~ "Rental_Existing",
																	product_n == 3 ~ "Refinance"
																	)) %>%                                                                      # create product name var
	mutate(regioncd_n = case_when(
																province_n >= 0 & province_n <= 3   ~ 27,
																province_n == 4                     ~ 37,
																province_n == 9                     ~ 64,
																province_n >= 6 & province_n <= 8   ~ 66,
																province_n >= 11 & province_n <= 13 ~ 66,
																province_n == 5                     ~ 70
																)) %>%                                                                        # create region var
	mutate(regioncd_name = case_when(
															regioncd_n == 27 ~ "Atlantic",
															regioncd_n == 37 ~ "Quebec",
															regioncd_n == 64 ~ "BC",
															regioncd_n == 66 ~ "Prairie and Territories",
															regioncd_n == 70 ~ "Ontario"
															)) %>%                                                                          # create region name var
	mutate(pid = group_indices(., product_n, regioncd_n)) %>%                                                   # add segment id var
	mutate(pid_name = paste0(product_name, " ", regioncd_name)) %>%                                             # add pid name
	mutate(qvintage = as.yearqtr(as.Date(start_date), na.rm = TRUE)) %>%                                        # create quarter of origination var
	mutate(yvintage = as.numeric(format(qvintage, "%Y")))                                                       # create year of origination var

#=============================================================================================================#
#                  														   Aggregate data 																					 ####
#=============================================================================================================#

# create empty dataframe with full time and segements
yvintage_vector <- seq(f_hist, l_frcst, by = 1)                                                                # create yvintage squance
segments <- ddply(data_raw_aggr, ~ pid + pid_name + product_name + regioncd_name, summarise,
									regioncd_n = mean(regioncd_n),
									product_n = mean(product_n))                                                                # create data for segment names and numbers
data_aggr <- expand.grid(pid = seq(1:15), yvintage = yvintage_vector)	 	                                      # create empty dataframe for data
data_aggr <- merge(data_aggr, segments, by = "pid", all.x = TRUE)                                             # add segment variables to data
data_aggr <- filter(data_aggr, !(product_name == "Refinance" & yvintage < f_dev_Refinance))

# Aggregate raw data by segments and yvintage
data_for_aggr <- ddply(data_raw_aggr, ~ yvintage + pid, summarise,
											 loans = sum(!is.na(loan_id)),
											 units = sum(units_n, na.rm = TRUE),
											 value = sum(multi_value, na.rm = TRUE),
											 price = sum(multi_value, na.rm = TRUE) / sum(units_n, na.rm = TRUE) / 1000,
											 below_7_units = mean(below_7_units, na.rm = TRUE) * 100)

# Merge loans data into empty dataframe
data_aggr <- merge(data_aggr, data_for_aggr, by = c("pid", "yvintage"), all.x = TRUE)
col_zero <- c("loans", "units")
data_aggr[,col_zero] <- lapply(data_aggr[,col_zero], function(x) ifelse(is.na(x), 0, x))

# create dummies from factors
data_aggr <- fastDummies::dummy_cols(data_aggr) %>%
	rename(regioncd_name_Prairie = 'regioncd_name_Prairie and Territories')

#=============================================================================================================#
#                  						   Add transformations and merge econ variables 														 ####
#=============================================================================================================#

# merge econs and create real prices
data_aggr <- data_aggr %>%
	left_join(data_econ_aggr, by = c("yvintage", "regioncd_name")) %>%
	mutate(price_real = price/(cpi/100))

# Add transformations for non-econs
vars_for_trans <- c("units", "price", "price_real")
data_trans <- lapply(vars_for_trans, function(x) {
	data_aggr %>% group_by(pid) %>%
		mutate( !!paste0("yd_", x) := get(x) - dplyr::lag(get(x), 1, na.pad = TRUE, order_by = yvintage)) %>%
		mutate( !!paste0("yp_", x) := (get(x) / dplyr::lag(get(x), 1, na.pad = TRUE, order_by = yvintage)) - 1) %>%
		ungroup(pid) %>%
		select(paste0("yd_", x), paste0("yp_", x))
})
data_trans <- bind_cols(data_trans)
data_aggr <- cbind(data_aggr, data_trans)

# Add lags to non-econs and their transformations
vars_for_lags <- c("units", "price", "yp_price", "yp_price_real")                                             # declare vars for lags
max_lag <- 4                                                                                                  # define maximum lag (in quarters)
data_lags <- lapply(vars_for_lags, function(x) {
	lapply(seq(1:max_lag), function(y) {
		data_aggr %>% group_by(pid) %>%
			mutate( !!paste0(x, "_", y) := dplyr::lag(get(x), y, na.pad = TRUE, order_by = yvintage)) %>%
			ungroup(pid) %>%
			select(paste0(x, "_", y))
	})
})
data_lags <- bind_cols(data_lags)                                                                             # bind lag lists to dataframe
data_aggr <- cbind(data_aggr, data_lags)                                                                      # bind lags dataframe to data

rm(list = c("data_raw", "yvintage_vector", "segments", "data_for_aggr", "col_zero", "vars_for_trans", 
						"data_trans", "vars_for_lags", "max_lag", "data_lags"))
gc()

#=============================================================================================================#
#                  											   Add approved and received data 																 ####
#=============================================================================================================#

# load approved and received data
data_approved <- read.xlsx(file.path(dir_data, 
																		 "Multi Unit 2000May to 2020July Approved and Rec Volumes.xlsx"),
													 sheet = "ApprovedVolumes")                                                         # load approved volume
data_received <- read.xlsx(file.path(dir_data, 
																		 "Multi Unit 2000May to 2020July Approved and Rec Volumes.xlsx"),
													 sheet = "ReceivedVolumes")                                                         # load received volume

# Remove "Grand Total" line
data_approved <- filter(data_approved, PROVINCE != "Grand Total")
data_received <- filter(data_received, PROVINCE != "Grand Total")

# pivot datasets
data_approved_long <- pivot_longer(data_approved, cols = names(data_approved[-c(1, 2, 3)]), 
														 names_to = "yvintage", values_to = "units_approved") %>%                             # wide to long
	rename(qtr = AprvQtr)
data_received_long <- pivot_longer(data_received, cols = names(data_received[-c(1, 2, 3)]), 
																	 names_to = "yvintage", values_to = "units_received") %>%                       # wide to long
	rename(qtr = RecQtr)

# merge datasets
data_approved_received <- merge(data_approved_long, data_received_long, 
																by = c("PROVINCE", "CUCODE", "qtr", "yvintage"),
																all = TRUE)

# Clean and arrange dataset for aggregation
data_approved_received <- data_approved_received %>%
	mutate(units_approved = ifelse(is.na(units_approved), 0, units_approved)) %>%                               # replace NAs with zeros
	mutate(units_received = ifelse(is.na(units_received), 0, units_received)) %>%                               # replace NAs with zeros
	filter(CUCODE %in% c("0020", "0021", "0033", "0034", "0040", "0041")) %>%
	mutate(qvintage = as.yearqtr(paste0(yvintage, qtr))) %>%                                                    # create qvintage var
	mutate(province = case_when(
		PROVINCE == "NL" ~ "NF",
		PROVINCE == "QC" ~ "QU",
		TRUE ~ PROVINCE)) %>%                                                                                     # fix province codes
	mutate(regioncd_n = case_when(
		province %in% c("NB", "NF", "NS", "PE")          ~ 27,
		province == "QU"                                 ~ 37,
		province == "BC"                                 ~ 64,
		province %in% c("AB", "MB", "NT-NU", "SK", "YK") ~ 66,
		province == "ON"                                 ~ 70
	)) %>%                                                                                                      # create region var
	mutate(regioncd_name = case_when(
		regioncd_n == 27 ~ "Atlantic",
		regioncd_n == 37 ~ "Quebec",
		regioncd_n == 64 ~ "BC",
		regioncd_n == 66 ~ "Prairie and Territories",
		regioncd_n == 70 ~ "Ontario"
	)) %>%                                                                                                      # create region name var
	mutate(product_n = case_when(
		CUCODE == "0020" ~ 1,
		CUCODE == "0021" ~ 2,
		CUCODE == "0033" ~ 3,
		CUCODE == "0034" ~ 3,
		CUCODE == "0040" ~ 1,
		CUCODE == "0041" ~ 2)) %>%                                                                                # create aggregated product codes
	mutate(product_name = case_when(
		product_n == 1 ~ "Rental_New",
		product_n == 2 ~ "Rental_Existing",
		product_n == 3 ~ "Refinance")) %>%                                                                        # create product name var
	group_by(product_name) %>%
	mutate(units_approved_1 = dplyr::lag(units_approved, 1, na.pad = TRUE, order_by = yvintage)) %>%
	mutate(units_approved_2 = dplyr::lag(units_approved, 2, na.pad = TRUE, order_by = yvintage)) %>%
	mutate(units_received_1 = dplyr::lag(units_received, 1, na.pad = TRUE, order_by = yvintage)) %>%
	mutate(units_received_2 = dplyr::lag(units_received, 2, na.pad = TRUE, order_by = yvintage)) %>%            # add lags
	ungroup()

# Aggreagte to region, year and aggregated products
data_approved_received <- ddply(data_approved_received, ~ yvintage + regioncd_name + product_name, summarise,
																units_approved = sum(units_approved),
																units_approved_1 = sum(units_approved_1),
																units_approved_2 = sum(units_approved_2),
																units_received = sum(units_received),
																units_received_1 = sum(units_received_1),
																units_received_2 = sum(units_received_2))

# Merge to full data
data_aggr <- merge(data_aggr, data_approved_received, 
									 by = c("yvintage", "regioncd_name", "product_name"), 
									 all.x = TRUE)

# Add approved and received ratios
data_aggr <- data_aggr %>%
	mutate(units_approved_ratio = units_approved / units) %>%
	mutate(units_received_ratio = units_received / units)

rm(list = c("data_approved", "data_received", "data_approved_long", "data_received_long", 
						"data_approved_received"))
gc()

#=============================================================================================================#
#                  																	Save data			 																				 ####
#=============================================================================================================#

save(data_aggr, file = file.path(dir_data, "data_aggr.RData"))

#=============================================================================================================#
