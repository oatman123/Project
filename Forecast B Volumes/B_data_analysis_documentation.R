#=============================================================================================================#
#																																																							#
#	Model: Multi-unit Lifecycle Loss and Exposure Analyzer (mollea) - Volume Projection													#
#	Description: B. Data analysis for documentation																															#
#  																																																						#
# Moody's Analytics (Ziv Rubin)																																								#
#	Created: 			09 June, 2020																																									#
#	Last Update: 	09 November, 2020		  																																				#
#	Version: 1.0																																																#
#																																																							#
#=============================================================================================================#


#=============================================================================================================#
#                  												   Script setup																									 ####
#=============================================================================================================#

# General 
rm(list=ls())                                                                                                 #	remove all data from enviroment
gc()                                                                                                          # collect all garbage to free memory
options(warn=-1)                                                                                              # turn warnings off

#=======================================#
#								Options							 		# 
#=======================================#

# location of main model folder (all results will be placed in sub-folders here)
dir_main <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/"

# location of codes folder
dir_codes <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/codes/"

# location of model data folder
dir_data <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/data/"

# location of mollea master data (raw loans data)
dir_raw_data <- "//Prdermfs01/erm/Insurance/Models/Commercial Multi/Inputs Data/pragma/20191231/Production/"

# Raw csv data file name (without file extension)
raw_data_name <- "prod_multi_full_19q4_newLBA"

# DataBuffet downloaded basket file name (without file extension)
db_data_name <- "Basket_2020-07-16_14h_13m"

# Create new aggregated data ("yes"/"no") - needs "yes" for some of the analysis, look below at different sections
new_data <- "no"                  	                                                                          # "yes" will run "A_data_creation.R" to create a fresh data file; "no" will load an old version of "data_aggr.Rdata" from data folder

# Load new raw MOLLEA data ("yes"/"no") *very long process*
new_raw_data <- "no"                                                                                          # "yes" will load new raw MOLLEA data in "A_data_creation.R"; "no" will use saved raw data from data folder

# Define bounds for historical data
f_hist <- 2000                      	                                                                        # define first year of historical data
f_hist_q <- "2000Q1"                     	                                                                    # define first quarter of historical data
l_hist <- 2019                        	                                                                      # define last year of historical data
l_hist_q <- "2019Q4"                     	                                                                    # define last quarter of historical data

# Define bounds for model development
f_dev_Rental_New <- 2004                                                                                      # define first year of Rental New development
f_dev_Rental_Existing <- 2004                                                                                 # define first year of Rental Existing development
f_dev_Refinance <- 2004                                                                                       # define first year of Refinance development
f_dev_Refinance_q <- "2004Q1"                                                                                 # define first quarter of Refinance development
l_dev <- 2018                                                                                                 # define last year of development
oos_start <- 2015                                                                                             # define first year of out-of-smaple analysis

# Define end of forecast
l_frcst <- 2025

#=======================================#
#								Folders							 		# 
#=======================================#

# folder locations
setwd(dir_main)                                                                                              	# set main directory
dir_tables <- file.path("results/data_analysis/tables")                                                       #	location of result tables folder
dir_charts <- file.path("results/data_analysis/charts")                                                       #	location of result charts folder

# create folders
dir.create(file.path("results"), showWarnings = FALSE)
dir.create(file.path("results/data_analysis"), showWarnings = FALSE)
dir.create(dir_tables, showWarnings = FALSE)
dir.create(dir_charts, showWarnings = FALSE)

#===========================================#
# 				Install packages and tools		 		#
#===========================================#

# Automatically install missing packages
ID_package <- c(
	"plyr"              # for data management
	,"dplyr"            # for data management
	,"tidyr"            # for data management
	,"zoo"              # for time management
	,"janitor"          # for clean data frames
	#,"reshape2"         # for expanding dataframe
	,"ggplot2"          # for plots
	#,"sandwich"        # for robust standard error
	#,"fitdistrplus"    # distribution fitting
	#,"nlme"            # heteroscedasticity test 
	,"lmtest"          # heteroscedasticity test 
	,"openxlsx"         # excel files
	,"fastDummies"      # factor to dummies
	#,"rsq"             # rsq
	,"MLmetrics"        # MAPE and RMSE
	,"stats"           # BIC
	#,"rlist"           # bind list by column
	,"imputeTS"         # impute NAs in time series
	,"tseries"          # units root tests
	,"plm"              # panel unit root tests
	,"strucchange"      # structural breaks
	#,"weights"         # weighted correlation
	,"outreg"          # tidy regression results
)
new_packages <- ID_package[!(ID_package %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(ID_package, require, character.only = TRUE)

# ggplot Moody’s template
#source(file = file.path("ecca_template.R"))
source("C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/ecca_template.R")
invisible(eccaTheme(14)) # font size

rm(list = c("new_packages", "ID_package"))

#=============================================================================================================#
#                  											 			 Load aggregate Data 																				 ####
#=============================================================================================================#

#=====================#
# Load data
#=====================#
# Run A_data_creation or load data_aggr saved in data folder
if(new_data == "yes") {
	source(file.path(dir_codes, "A_data_creation.R"), local = TRUE, echo = TRUE)
} else {
		load(file = file.path(dir_data, "data_aggr.RData"))
}

#=============================================================================================================#
#                  											   			Load raw data 																						 ####
#=============================================================================================================#

#=====================#
# Load raw MOLLEA Data
#=====================#
# Load raw csv data from DPA output location or load RData file from modeling data folder
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
#                  											 Full (old) segments analysis																			 ####
#=============================================================================================================#

#=====================#
# Aggregate data
#=====================#

# select and transform relevant variables
data_raw_aggr_old <- data_raw %>% 
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
	mutate(product_name = case_when(
																product_n == 30 ~ "Rental_New_7_less",
																product_n == 31 ~ "Rental_Existing_7_less",
																product_n == 38 ~ "Refinance_7_more",
																product_n == 37 ~ "Refinance_7_less",
																product_n == 32 ~ "Rental_New_7_more",
																product_n == 33 ~ "Rental_Existing_7_more"
															)) %>%                                                                          # create product name var
	mutate(regioncd_n = case_when(
															province_n >= 0 & province_n <= 3   ~ 27,
															province_n == 4                     ~ 37,
															province_n == 9                     ~ 64,
															province_n >= 6 & province_n <= 8   ~ 66,
															province_n >= 11 & province_n <= 13 ~ 66,
															province_n == 5                     ~ 70
														)) %>%                                                                            # create region var
	mutate(regioncd_name = case_when(
																	regioncd_n == 27 ~ "ATLANTIC",
																	regioncd_n == 37 ~ "QUEBEC",
																	regioncd_n == 64 ~ "BC",
																	regioncd_n == 66 ~ "PRAIRIE_TERRITORIES",
																	regioncd_n == 70 ~ "ONTARIO"
																)) %>%                                                                        # create region name var
	mutate(below_7_units = case_when(
																	product_n == 30 ~ 1,
																	product_n == 31 ~ 1,
																	product_n == 38 ~ 0,
																	product_n == 37 ~ 1,
																	product_n == 32 ~ 0,
																	product_n == 33 ~ 0)) %>%                                                   # create below 7 units product indicator
	mutate(pid = group_indices(., product_n, regioncd_n)) %>%                                                   # add segment id var
	mutate(pid_name = paste0(product_name, " ", regioncd_name)) %>%                                             # add pid name
	mutate(qvintage = as.yearqtr(as.Date(start_date), na.rm = TRUE)) %>%                                        # create quarter of origination var
	mutate(yvintage = as.numeric(format(qvintage, "%Y")))                                                       # create year of origination var

# create empty dataframe with full time and segements
qvintage_res <- seq(unclass(as.yearqtr(f_hist_q)), unclass(as.yearqtr(l_hist_q)), by = 0.25)                  # create qvintage squance
qvintage_vector <- structure(qvintage_res, class = "yearqtr")	                                                # put qvintage sequance into yearqtr class
segments <- ddply(data_raw_aggr_old, ~ pid + pid_name + product_name + regioncd_name, summarise,
									regioncd_n = mean(regioncd_n),
									product_n = mean(product_n))                                                                # create data for segment names and numbers
data_aggr_old <- expand.grid(pid = seq(1:30), qvintage = qvintage_vector)	 	                                  # create empty dataframe for data
data_aggr_old <- merge(data_aggr_old, segments, by = "pid", all.x = TRUE)                                     # add segment variables to data
data_aggr_old <- filter(data_aggr_old, !((product_name == "Refinance_7_more" | product_name == "Refinance_7_less") 
																				 & qvintage < as.yearqtr(f_dev_Refinance_q)))                         # trim data for refinance

# Aggregate raw data by segments and qvintage
data_for_aggr_old <- ddply(data_raw_aggr_old, ~ qvintage + pid, summarise,
											 loans = sum(!is.na(loan_id)),
											 units = sum(units_n, na.rm = TRUE),
											 value = sum(multi_value, na.rm = TRUE),
											 price = sum(multi_value, na.rm = TRUE) / sum(units_n, na.rm = TRUE) / 1000,
											 below_7_units = mean(below_7_units, na.rm = TRUE))

# Merge loans data into empty dataframe
data_aggr_old <- merge(data_aggr_old, data_for_aggr_old, by = c("pid", "qvintage"), all.x = TRUE)
col_zero <- c("loans", "units")
data_aggr_old[,col_zero] <- lapply(data_aggr_old[,col_zero], function(x) ifelse(is.na(x), 0, x))

# add aggregate products
data_aggr_old <- data_aggr_old %>%
	mutate(product_name_2 = case_when(
																	product_name == "Rental_New_7_less" ~ "Rental_New",
																	product_name == "Rental_Existing_7_less" ~ "Rental_Existing",
																	product_name == "Refinance_7_more" ~ "Refinance",
																	product_name == "Refinance_7_less" ~ "Refinance",
																	product_name == "Rental_New_7_more" ~ "Rental_New",
																	product_name == "Rental_Existing_7_more" ~ "Rental_Existing"))

#======================================#
# Analyse segment coverage
#======================================#
# Segment coverage analysis
segments_analysis <- ddply(data_aggr_old, ~ regioncd_name + product_name, summarise,
													 loans_p_0 = sum(loans == 0) / length(loans) * 100,
													 loans_p_3 = sum(loans <= 3) / length(loans) * 100,
													 loans_n_0 = sum(loans == 0),
													 loans_n_3 = sum(loans <= 3),
													 loans_n_avg = mean(loans),
													 obs = length(loans))
segments_analysis[,5:dim(segments_analysis)[2]] <- round(segments_analysis[,5:dim(segments_analysis)[2]], 2)
segments_analysis_list <- list(
	all = segments_analysis,
	loans_p_0 = spread(segments_analysis[, c("regioncd_name", "product_name", "loans_p_0")], product_name, loans_p_0),
	loans_p_3 = spread(segments_analysis[, c("regioncd_name", "product_name", "loans_p_3")], product_name, loans_p_3),
	loans_n_0 = spread(segments_analysis[, c("regioncd_name", "product_name", "loans_n_0")], product_name, loans_n_0),
	loans_n_3 = spread(segments_analysis[, c("regioncd_name", "product_name", "loans_n_3")], product_name, loans_n_3),
	obs = spread(segments_analysis[, c("regioncd_name", "product_name", "obs")], product_name, obs),
	loans_n_avg = spread(segments_analysis[, c("regioncd_name", "product_name", "loans_n_avg")], product_name, loans_n_avg))
write.xlsx(segments_analysis_list, file.path(dir_tables, "segments_analysis.xlsx"))

#======================================#
# Plot above/below 7 units over time
#======================================#

# create below 7 units product indicator
data_plot <- ddply(data_aggr_old, ~ product_name_2 + qvintage, summarise, 
									 below_7_units = weighted.mean(below_7_units, loans, na.rm = TRUE) * 100) %>%               # aggregate data with below 7 share
	group_by(product_name_2) %>%
	mutate(below_7_units_12avg = rollapply(below_7_units, 12, mean, align = "right", partial = TRUE)) %>%       # add 1 year rolling average
	ungroup()
plot <- ggplot(data_plot, aes(x = qvintage)) +
	geom_line(aes(y = below_7_units, color = product_name_2), size = 1, linetype = "solid") +
	geom_line(aes(y = below_7_units_12avg, color = product_name_2), size = 1, linetype = "dotted") +
	scale_colour_manual("", breaks = unique(data_plot$product_name_2), values = eccaChartColors[c(3, 11, 2)]) +
	theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Quarter of Origination") + ylab("Share of below 7 units, %") + scale_x_yearqtr(format = "%YQ%q", n = 10)
ggsave(file.path(dir_charts, "below_7_units.png"), plot = plot, width = 8, height = 5)	

rm(list = c("qvintage_res", "qvintage_vector", "segments", "data_for_aggr_old", "col_zero", "segments_analysis", "segments_analysis_list", "data_plot", "plot"))
gc()

#=============================================================================================================#
#            											 New (3 products) segments analysis quarterly														 ####
#=============================================================================================================#

#=====================#
# Aggregate data
#=====================#
data_aggr_new_qtr <- ddply(data_aggr_old, ~ product_name_2 + regioncd_name + qvintage, summarise,
													 below_7_units = weighted.mean(below_7_units, loans, na.rm = TRUE) * 100,
													 loans = sum(loans, na.rm = TRUE),
													 units = sum(units, na.rm = TRUE),
													 value = sum(value, na.rm = TRUE),
													 price = sum(value, na.rm = TRUE) / sum(units, na.rm = TRUE) / 1000)

#====================================#
# Plot prices and units
#====================================#

data_plot <- data_aggr_new_qtr %>%
	mutate(regioncd_name = ifelse(regioncd_name == "PRAIRIE_TERRITORIES", "PRAIRIE", regioncd_name))

plot <- ggplot(data_plot, aes(x = qvintage)) +
	geom_line(aes(y = price), size = 0.75, color = eccaChartColors[3]) +
	facet_grid(product_name_2 ~ regioncd_name, scales = "free_y") +
	theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Quarter of Origination") + ylab("Price, $000") + scale_x_yearqtr(format = "%YQ%q", n = 6)
ggsave(file.path(dir_charts, "price_regional_product.png"), plot = plot, width = 8, height = 5)
plot <- ggplot(data_plot, aes(x = qvintage)) +
	geom_line(aes(y = units), size = 0.75, color = eccaChartColors[3]) +
	facet_grid(product_name_2 ~ regioncd_name, scales = "free_y") +
	theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
	xlab("Quarter of Origination") + ylab("Units, #") + scale_x_yearqtr(format = "%YQ%q", n = 6)
ggsave(file.path(dir_charts, "units_regional_product.png"), plot = plot, width = 8, height = 5)

rm(list = c("data_plot", "plot"))
gc()

#=============================================================================================================#
#            															 Export excluded loans data																			 ####
#=============================================================================================================#

# generate data of excluded loans
data_exclud <- data_raw %>%
	filter(age_to_orig == 1                                                                                     # keep only one observation per loan
				 ,product_n %in% c(30, 31, 38, 37, 32, 33)	                                                          # keep only relevant products
				 ,loan_id %in% c(90403684, 90157157, 90261033, 90256124, 90565540, 90566043, 90433434, 90495565)) %>% # keep excluded loans
	mutate(loan_price = multi_value / units_n / 1000) %>%                                                       # calculate average price per unit
	mutate(product_name = case_when(
		product_n == 30 ~ "Rental_New",
		product_n == 31 ~ "Rental_Existing",
		product_n == 38 ~ "Refinance",
		product_n == 37 ~ "Refinance",
		product_n == 32 ~ "Rental_New",
		product_n == 33 ~ "Rental_Existing")) %>%                                                                 # create product name var
	mutate(regioncd_name = case_when(
		province_n >= 0 & province_n <= 3   ~ "Atlantic",
		province_n == 4                     ~ "Quebec",
		province_n == 9                     ~ "BC",
		province_n >= 6 & province_n <= 8   ~ "Prairie and Territories",
		province_n >= 11 & province_n <= 13 ~ "Prairie and Territories",
		province_n == 5                     ~ "Ontario")) %>%                                                     # create region var
	mutate(qvintage = as.yearqtr(as.Date(start_date), na.rm = TRUE)) %>%                                        # create quarter of origination var
	mutate(yvintage = as.numeric(format(qvintage, "%Y"))) %>%                                                   # create year of origination var
	left_join(select(data_aggr, product_name, regioncd_name, yvintage, loans, units, price), 
						by = c("yvintage", "product_name", "regioncd_name")) %>%                                          # add segment averages
	rename(segment_loans = loans, segement_units = units, segment_price = price) %>%                            # rename segment variables
	select(loan_id, yvintage, regioncd_name, product_name, loan_units = units_n, 
				 loan_price, segment_loans, segement_units, segment_price)                                            # select variables to include in final table

# export excluded loans table
write.xlsx(data_exclud, file.path(dir_tables, "exclusions_analysis.xlsx"))

#=============================================================================================================#
#                  										Analysis of Condo and singles RPI			 															 ####
#=============================================================================================================#

# This section requires new_data <- "yes" set in beginning of code

# correlations table
corr <- data_econ_rpi %>% 
	filter(!is.na(rpi_condo) & !is.na(rpi_singles)) %>% group_by(regioncd_name) %>% dplyr::summarize(cor(rpi_condo, rpi_singles))
write.xlsx(corr, file.path(dir_tables, "rpi_corr.xlsx"), row.names = TRUE)

# plot correlations
plot <- ggplot(data_econ_rpi, aes(x = yvintage)) + 
	geom_line(aes(y = rpi_condo, color = "Condo RPI"), size = 1.5) +
	geom_line(aes(y = rpi_singles, color = "Singles RPI"), size = 1.5) +
	theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) + 
	scale_colour_manual("", breaks = c("Condo RPI", "Singles RPI"), values = eccaChartColors[c(3, 11)]) + 
	facet_wrap(~ regioncd_name, ncol = 3) + 
	xlab("Year of Origination") + ylab("RPI")
ggsave(file.path(dir_charts, "rpi_condo_singles_facet_qvin.png"), plot = plot, width = 8, height = 5)

rm(list = c("corr", "plot", "data_econ_rpi"))
gc()

#=============================================================================================================#
#                  								 				Macros unit-root analysis 																			 ####
#=============================================================================================================#

# This section requires new_data <- "yes" set in beginning of code

# Define macros for analysis (panel and national)
panel_macros <- c("rpi", "yp_rpi", "vac_rate", "yd_vac_rate", "ump_rate", "yd_ump_rate", 
									"emp", "yp_emp", "pop", "yp_pop", "gdp", "yp_gdp", "hstarts", "yp_hstarts", 
									"cpi_rent", "yp_cpi_rent")
national_macros <- c("rpi", "yp_rpi", "vac_rate", "yd_vac_rate", "ump_rate", "yd_ump_rate", 
										 "emp", "yp_emp", "pop", "yp_pop", "gdp", "yp_gdp", "hstarts", "yp_hstarts", 
										 "cpi_rent", "yp_cpi_rent", 
										 "rmort5y", "yd_rmort5y", "rprime", "yd_rprime", "stockp", "yp_stockp", 
										 "cpwti", "yp_cpwti", "rgt5y", "yd_rgt5y", "rgt10y", "yd_rgt10y", 
										 "rmort_spread", "yd_rmort_spread")

# Define unit root tests to use
root_tests <- c("madwu", "Pm", "invnormal")
Test <- c("Maddala and Wu (1999)", "PM - Choi (2001)", "invnormal - Choi (2001)")
panel_h1 <- c("H1: stationarity", "H1: stationarity", "H1: stationarity")
national_h1 <- c("H1: stationarity", "H0: unit root")

#======================================#
# Panel unit root test
#======================================#

# Panel unit root test
data_econ_aggr_panel <- data_econ_aggr %>% filter(yvintage <= l_hist)                                         # Trim econ data to historical bounds
panel_ur_results <- lapply(panel_macros, function(m) {
	unit_root <- round(unlist(lapply(root_tests, function(r) {
		data <- filter(data_econ_aggr_panel, !is.na(get(m)))                                                      # data without NAs
		purtest(as.formula(paste0(m, "~ trend")), data = data, 
						index = "regioncd_name", test = r, lags = "SIC", 
						pmax = 8, na.rm = TRUE)[[1]][[6]]})), 3)                                                          # unit root tests
	setNames(unit_root, 1:3)
})
panel_ur_results <- bind_rows(panel_ur_results)                                                               # bind vars tests to dataframe
names(panel_ur_results) <- Test                                                                               # add names of tests
rownames(panel_ur_results) <- panel_macros                                                                    # add names of vars
panel_ur_results <- rbind("Hypothesis" = panel_h1, panel_ur_results)                                          # and hypotheses

#======================================#
# National unit root test
#======================================#

# Aggregate data
data_econ_national <- ddply(data_econ_aggr, ~ yvintage, summarise,
															vac_rate = weighted.mean(vac_rate, pop, na.rm = TRUE),
															rpi = weighted.mean(rpi, pop, na.rm = TRUE),
															ump_rate = weighted.mean(ump_rate, pop, na.rm = TRUE),
															emp = sum(emp, na.rm = TRUE),
															gdp = weighted.mean(gdp, pop, na.rm = TRUE),
															hstarts = sum(hstarts, na.rm = TRUE),
															cpi_rent = weighted.mean(cpi_rent, pop, na.rm = TRUE),
															rmort5y = mean(rmort5y, na.rm = TRUE),
															stockp = mean(stockp, na.rm = TRUE),
															rprime= mean(rprime, na.rm = TRUE),
															cpwti = mean(cpwti, na.rm = TRUE),
															rgt5y = mean(rgt5y, na.rm = TRUE),
															rgt10y = mean(rgt10y, na.rm = TRUE),
															rmort_spread = mean(rmort_spread, na.rm = TRUE),
															pop = sum(pop, na.rm = TRUE))                                                   # aggregate all macros to national numbers

# Add transformations for macros
rhs_macros <- c("rpi", "rpi_national", "vac_rate", "vac_rate_national", "ump_rate", "emp", "gdp", 
								"hstarts", "rmort5y", "stockp", "rprime", "cpwti", "rgt5y", "rgt10y", "rmort_spread", 
								"cpi_rent", "pop")
data_trans <- lapply(names(data_econ_national[-1]), function(x) {
	data_econ_national %>% 
		mutate( !!paste0("yd_", x) := get(x) - 
							dplyr::lag(get(x), 1, na.pad = TRUE, order_by = yvintage)) %>%
		mutate( !!paste0("yp_", x) := (get(x) / 
																	 	dplyr::lag(get(x), 1, na.pad = TRUE, order_by = yvintage)) - 1) %>%
		select(paste0("yd_", x), paste0("yp_", x))
})

# bind transformed lists to dataframe and trim econ data to historical bounds
data_trans <- bind_cols(data_trans)
data_econ_national <- cbind(data_econ_national, data_trans) %>% 
	filter(yvintage <= l_hist)

# National unit root test
adf <- round(unlist(lapply(national_macros, function(m) {
	data <- filter(data_econ_national, !is.na(get(m)))[, m]                                                     # data without NAs
	adf.test(data, k = trunc(4*((length(data)/100)^0.25)))$p.value})), 3)                                       # ADF test
pp <- round(unlist(lapply(national_macros, function(m) {
	data <- filter(data_econ_national, !is.na(get(m)))[, m]                                                     # data without NAs
	pp.test(data, lshort = TRUE)$p.value})), 3)                                                                 # PP test
national_ur_result <- as.data.frame(cbind(
	"ADF Test" = adf,
	"Phillips-Perron Test" = pp))                                                                               # bind all national tests to dataframe
rownames(national_ur_result) <- national_macros                                                               # add names of vars
national_ur_result <- rbind("Hypothesis" = national_h1, national_ur_result)                                   # bind all tests with hypothesis line

#======================================#
# Export results to excel
#======================================#

write.xlsx(list(panel_ur_results, national_ur_result), 
					 file.path(dir_tables, "macro_unit_root_test.xlsx"), row.names = TRUE)

#======================================#
# Plot macros over time
#======================================#
dir <- "unit_roots"
dir.create(file.path(dir_charts, dir), showWarnings = FALSE)

for(r in c(panel_macros)) {
	plot <- ggplot(data_econ_aggr_panel, aes(x = yvintage, y = get(r), color = regioncd_name)) + geom_line() + 
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) + 
		scale_colour_manual("", breaks = data_econ_aggr_panel$regioncd_name, values = eccaChartColors[1:5]) + 
		xlab("Year") + ylab(r)
	ggsave(file.path(dir_charts, dir, paste0(r, ".png")), plot = plot, width = 8, height = 5)
}

for(r in c(national_macros)) {
	plot <- ggplot(data_econ_national, aes(x = yvintage, y = get(r))) + geom_line() + 
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) + 
		xlab("Year") + ylab(r)
	ggsave(file.path(dir_charts, dir, paste0(r, "_national.png")), plot = plot, width = 8, height = 5)
}

rm(list = c("panel_macros", "national_macros", "root_tests", "Test", "panel_h1", "national_h1", 
						"panel_ur_results", "data_vac_rate", "data_econ_national", "adf", "kpss", 
						"pp", "national_ur_result", "plot", "r"))
gc()

#=============================================================================================================#
#                  					 				Units and prices unit-root analysis 																	 ####
#=============================================================================================================#

# Define transformations for analysis (panel and national)
test_trans <- c("units", "yd_units", "yp_units", "price", "yd_price", "yp_price")

# Define unit root tests to use
test_code <- c("madwu", "Pm", "invnormal", "logit")
test_name <- c("Maddala and Wu (1999)", "PM - Choi (2001)", "invnormal - Choi (2001)", "logit - Choi (2001)")
panel_h1 <- c("H1: stationarity", "H1: stationarity", "H1: stationarity", "H1: stationarity")

#==============================================#
# Test unit roots aggreagted by region
#==============================================#

# Aggregate by regions
data_unit_region <- ddply(data_aggr, ~ regioncd_name + yvintage, summarise,
											 price = weighted.mean(price, units, na.rm = TRUE),
											 units = sum(units, na.rm = TRUE)) %>%                                                  # aggregate variables to region numbers
	group_by(regioncd_name) %>%
	mutate(yd_units = units - dplyr::lag(units, 1, na.pad = TRUE, order_by = yvintage)) %>%
	mutate(yp_units = (units / dplyr::lag(units, 1, na.pad = TRUE, order_by = yvintage)) - 1) %>%
	mutate(yd_price = price - dplyr::lag(price, 1, na.pad = TRUE, order_by = yvintage)) %>%
	mutate(yp_price = (price / dplyr::lag(price, 1, na.pad = TRUE, order_by = yvintage)) - 1) %>%               # add transformations
	ungroup() %>%
	filter(yvintage >= f_dev_Rental_New)

# Panel unit root test
unit_root_test <- lapply(test_trans, function(m) {
	unit_root <- round(unlist(lapply(test_code, function(r) {
		data <- filter(data_unit_region, !is.na(get(m)))                                                                 # data without NAs
		purtest(as.formula(paste0(m, "~ trend")), data = data, 
						index = "regioncd_name", test = r, lags = "AIC", 
						pmax = 5, na.rm = TRUE)[[1]][[6]]})), 3)                                                          # unit root tests
	setNames(unit_root, 1:4)
})
unit_root_test <- bind_rows(unit_root_test)                                                               # bind vars tests to dataframe
names(unit_root_test) <- test_name                                                                               # add names of tests
rownames(unit_root_test) <- test_trans                                                                     # add names of vars
unit_root_test <- rbind("Hypothesis" = panel_h1, unit_root_test)                                          # and hypotheses

# export results
write.xlsx(unit_root_test, file.path(dir_tables, "unit_root_test.xlsx"), row.names = TRUE)

rm(list = c("test_trans", "test_code", "test_name", "panel_h1", "data_unit_region", "unit_root_test"))
gc()
#=============================================================================================================#

#=============================================================================================================#
#    																		Approved and received units analysis						 									 ####
#=============================================================================================================#

#===================================#
# Approved and received correlation
#===================================#

corr <- data_aggr %>%
	filter((product_name == "Rental_New" & yvintage >= f_dev_Rental_New & yvintage <= l_dev) | 
				 	(product_name == "Rental_Existing" & yvintage >= f_dev_Rental_Existing & yvintage <= l_dev) | 
				 	(product_name == "Refinance" & yvintage >= f_dev_Refinance & yvintage <= l_dev)) %>%
	ddply(., ~ product_name, summarise,
							units_approved = cor(units, units_approved, use = "complete.obs"),
							units_approved_1 = cor(units, units_approved_1, use = "complete.obs"),
							units_approved_2 = cor(units, units_approved_2, use = "complete.obs"),
							units_received = cor(units, units_received, use = "complete.obs"),
							units_received_1 = cor(units, units_received_1, use = "complete.obs"),
							units_received_2 = cor(units, units_received_2, use = "complete.obs"))
write.xlsx(corr, file.path(dir_tables, paste0("appr_rec_corr.xlsx")), row.names = TRUE)

#============================#
# Plot approved and received
#============================#

dir <- "appr_rec"
dir.create(file.path(dir_charts, dir), showWarnings = FALSE)

for(p in unique(data_aggr$product_name)) {
	data_plot <- data_aggr %>%
		filter((product_name == "Rental_New" & yvintage >= f_dev_Rental_New & yvintage <= l_dev) | 
					 	(product_name == "Rental_Existing" & yvintage >= f_dev_Rental_Existing & yvintage <= l_dev) | 
					 	(product_name == "Refinance" & yvintage >= f_dev_Refinance & yvintage <= l_dev)) %>% 
		filter(product_name == p)
	# Units regional
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = units_approved, color = "Units approved"), size = 1) +
		geom_line(aes(y = units_received, color = "Units received"), size = 1) +
		geom_line(aes(y = units, color = "Units"), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = c("Units approved", "Units received", "Units"), values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Units")
	ggsave(file.path(dir_charts, dir, paste0("units_appr_rec_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	# units ratio regional
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = units_approved_ratio, color = "Units approved ratio"), size = 1) + 
		geom_line(aes(y = units_received_ratio, color = "Units received ratio"), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = c("Units approved ratio", "Units received ratio"), values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Units ratio, %")
	ggsave(file.path(dir_charts, dir, paste0("units_appr_rec_ratio_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	# aggregate to national data
	data_plot_national <- ddply(data_plot, ~ yvintage, summarise, 
															units = sum(units),
															units_approved = sum(units_approved),
															units_received = sum(units_received))
	data_plot_national <- data_plot_national %>% 
		mutate(units_approved_ratio = units_approved / units) %>%
		mutate(units_received_ratio = units_received / units)
	# units national
	plot <- ggplot(data_plot_national, aes(x = yvintage)) +
		geom_line(aes(y = units_approved, color = "Units approved"), size = 1) +
		geom_line(aes(y = units_received, color = "Units received"), size = 1) +
		geom_line(aes(y = units, color = "Units"), size = 1) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = c("Units approved", "Units received", "Units"), values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Units")
	ggsave(file.path(dir_charts, dir, paste0("units_appr_rec_national_", p, ".png")), plot = plot, width = 8, height = 5)
	# units ratio national
	plot <- ggplot(data_plot_national, aes(x = yvintage)) +
		geom_line(aes(y = units_approved_ratio, color = "Units approved ratio"), size = 1) + 
		geom_line(aes(y = units_received_ratio, color = "Units received ratio"), size = 1) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = c("Units approved ratio", "Units received ratio"), values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Units ratio, %")
	ggsave(file.path(dir_charts, dir, paste0("units_appr_rec_ratio_national_", p, ".png")), plot = plot, width = 8, height = 5)
}

rm(list = c("corr", "data_plot", "plot", "data_plot_national", "dir", "p"))
gc()
	
