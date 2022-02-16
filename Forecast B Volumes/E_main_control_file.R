#=============================================================================================================#
#																																																							#
#	Model: Multi-unit Lifecycle Loss and Exposure Analyzer (mollea) - Volume Projection													#
#	Description: E. Model implementation    																																		#
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
#rm(list=ls())                                                                                                 #	remove all data from enviroment
gc()                                                                                                          # collect all garbage to free memory
options(warn=-1)                                                                                              # turn warnings off

#=======================================#
#								Options							 		# 
#=======================================#

# location of main model folder (all results will be placed in sub-folders here)
dir_main <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection"

# location of codes folder
dir_codes <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/codes/"

# location of model data folder
dir_data <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/data/"

# location of model development results folder (winner models)
dir_winner <- "C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/results/model_dev/"

# location of mollea master data (raw loans data)
dir_raw_data <- "//Prdermfs01/erm/Insurance/Models/Commercial Multi/Inputs Data/pragma/20191231/Production/"

# Raw csv data file name (without file extension)
raw_data_name <- "prod_multi_full_19q4_newLBA"

# DataBuffet downloaded basket file name (without file extension)
db_data_name <- "Basket_2020-07-16_14h_13m"

# Units model development results (winner models) file name (without file extension)
winner_units_name <- "winner_models_units"

# Price model development results (winner models) file name (without file extension)
winner_price_name <- "winner_models_price"

# Create new aggregated data ("yes"/"no") 
new_data <- "yes"                  	                                                                          # "yes" will run "A_data_creation.R" to create a fresh data file; "no" will load an old version of "data_aggr.Rdata" from data folder

# Load new raw MOLLEA data ("yes"/"no") *very long process*
new_raw_data <- "yes"                                                                                          # "yes" will load new raw MOLLEA data in "A_data_creation.R"; "no" will use saved raw data from data folder

# Define bounds for historical data
f_hist <- 2000   # define first year of historical data
l_hist <- 2019   # define last year of historical data

# Define bounds for model development (please don't change line 65 to line 69)
f_dev_Rental_New <- 2004                                                                                      # define first year of Rental New development
f_dev_Rental_Existing <- 2004                                                                                 # define first year of Rental Existing development
f_dev_Refinance <- 2004                                                                                       # define first year of Refinance development
l_dev <- 2019                                                                                                 # define last year of development
oos_start <- 2015                                                                                             # define first year of out-of-smaple analysis

# Define end of forecast
l_frcst <- 2025

#=======================================#
#								Folders							 		# 
#=======================================#

# folder locations
setwd(dir_main)                                                                                              	# set main directory
dir_tables <- file.path("results/implementation/tables")                                                      #	location of result tables folder
dir_charts <- file.path("results/implementation/charts")                                                      #	location of result charts folder

# create folders
dir.create(file.path("results"), showWarnings = FALSE)
dir.create(file.path("results/implementation"), showWarnings = FALSE)
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
	#,"tseries"          # units root tests
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
source("C:/Users/hzhou/OneDrive - CMHC-SCHL/MORA/2022/volume projection/codes/ecca_template.R")
invisible(eccaTheme(14)) # font size

rm(list = c("new_packages", "ID_package"))

#=============================================================================================================#
#                  											  				 Load Data 																							 ####
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

#==========================================#
# Load development results (winner models)
#==========================================#

load(file = file.path(dir_winner, paste0(winner_units_name, ".RData")))
load(file = file.path(dir_winner, paste0(winner_price_name, ".RData")))

#=============================================================================================================#
#                  									 Model development setup and data																			 ####
#=============================================================================================================#

#=====================#
# Setup
#=====================#

# Add additional variables for implementation
data_imp <- data_aggr %>%
	mutate(yd_vac_rate_national_bc = yd_vac_rate_national * regioncd_name_BC) %>%
	mutate(yp_stockp_qu = yp_stockp * regioncd_name_Quebec) %>%
	mutate(yd_rmort5y_2_on = yd_rmort5y_2 * regioncd_name_Ontario) %>%
	mutate(yd_rmort5y_2_at = yd_rmort5y_2 * regioncd_name_Atlantic) %>%
	mutate(yd_rmort_spread_at = yd_rmort_spread * regioncd_name_Atlantic) %>%
	mutate(yd_ump_rate_at = yd_ump_rate * regioncd_name_Atlantic) %>%
	mutate(yd_ump_rate_qu = yd_ump_rate * regioncd_name_Quebec) %>%
	mutate(ydum_07_10 = as.numeric(ifelse(yvintage >= 2007 & yvintage <= 2010, 1, 0))) %>%
	mutate(ydum_07_10_qu = as.numeric(ifelse(ydum_07_10 == 1 & regioncd_name == "Quebec", 1, 0))) %>%
	mutate(yd_rprime_qu = yd_rprime * regioncd_name_Quebec) %>%
	mutate(yd_rgt5y_qu = yd_rgt5y * regioncd_name_Quebec) %>%
	mutate(yp_cpwti_2_qu = yp_cpwti_2 * regioncd_name_Quebec) %>%
	mutate(yp_rpi_national_qu = yp_rpi_national * regioncd_name_Quebec) %>%
	mutate(yd_emp_qu = yd_emp * regioncd_name_Quebec) %>%
	mutate(yd_emp_1_on = yd_emp_1 * regioncd_name_Ontario) %>%
	mutate(yp_cpwti_on = yp_cpwti * regioncd_name_Ontario) %>%
	mutate(yd_ump_rate_1_on = yd_ump_rate_1 * regioncd_name_Ontario) %>%
	mutate(yd_rmort5y_on = yd_rmort5y * regioncd_name_Ontario) %>%
	group_by(pid) %>%
	mutate(yd_rprime_4avg = rollapply(yd_rprime, 4, mean, align = "right", partial = TRUE)) %>%
	ungroup() %>%
	mutate(yd_rprime_4avg_qu = yd_rprime_4avg * regioncd_name_Quebec) %>%
	mutate(yd_rprime_4avg_on = yd_rprime_4avg * regioncd_name_Ontario)


#=============================================================================================================#
#                  															Model implementation			 																 ####
#=============================================================================================================#

#=========================#
# Generate fitted values
#=========================#

# Run models, generate in-sample and out-of-sample fitted values and calculate fit stats
list_predicted <- lapply(unique(data_imp$product_name), function(p) {
	# Decalre yvintage for forecatsing
	yvintage_for_fit <- unique(filter(data_imp, yvintage > l_hist)$yvintage)                                    # vector of yvintage for forecast
	
	# created forecasts
	data_fit <- data_imp %>% filter(product_name == p) %>%
		select(yvintage, product_name, regioncd_name, pid, units, price, price_real, cpi,
					 yp_price_real, yp_price_real_1, pop, names(winner_models_price[[p]]$coefficients[-1]), 
					 names(winner_models_units[[p]]$coefficients[-1])) %>%                                              # trim fit data to only relevant vars
		mutate(f_units = as.numeric(ifelse(yvintage > l_hist, NA, units))) %>%                                    # empty fitted units with "anchor" units for out-of-sample fit
		mutate(f_yp_price_real = as.numeric(ifelse(yvintage > l_hist, NA, yp_price_real))) %>%                    # empty fitted yp_price_real values with "anchor" yp_price_real for out-of-sample fit
		mutate(f_price_real = as.numeric(ifelse(yvintage > l_hist, NA, price_real)))                              # empty fitted price_real values with "anchor" price_real for out-of-sample fit
	for(y in yvintage_for_fit) {
		data_fit <- data_fit %>% group_by(regioncd_name) %>%
			mutate(units_1 = dplyr::lag(f_units, 1, order_by = yvintage)) %>%                                       # create units AR(1)
			mutate(units_2 = dplyr::lag(f_units, 2, order_by = yvintage)) %>%                                       # create units AR(2)
			mutate(units_3 = dplyr::lag(f_units, 3, order_by = yvintage)) %>%                                       # create units AR(3)
			mutate(yp_price_real_1 = dplyr::lag(f_yp_price_real, 1, order_by = yvintage)) %>%                       # create price AR(1)
			mutate(yp_price_real_2 = dplyr::lag(f_yp_price_real, 2, order_by = yvintage)) %>%                       # create price AR(2)
			mutate(yp_price_real_3 = dplyr::lag(f_yp_price_real, 3, order_by = yvintage)) %>%                       # create price AR(3)
			ungroup()
		for(r in unique(data_fit$regioncd_name)) {
			data_fit[which(data_fit$yvintage >= y &
										 	data_fit$regioncd_name == r),
							 "f_units"]  <- predict(winner_models_units[[p]],
							 														 type = "response",
							 														 newdata = filter(data_fit, yvintage >= y & 
							 														 								 	regioncd_name == r),
							 														 na.rm = TRUE)                                                      # generate fitted units numbers for this loop
			data_fit$f_units <- ifelse(data_fit$f_units < 0, 0, data_fit$f_units)                                   # censor units at zero
			data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, 
							 "f_yp_price_real"] <- predict(winner_models_price[[p]], 
							 																		type = "response", 
							 																		newdata = filter(data_fit, yvintage == y & 
							 																										 	regioncd_name == r), 
							 																		na.rm = TRUE)                                               # generate yp_price_real fitted in-sample numbers for this loop
			data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, "f_price_real"] <- 
				(data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, "f_yp_price_real"] + 1) * 
				data_fit[data_fit$yvintage == y - 1 & data_fit$regioncd_name == r, "f_price_real"]                    # generate price_real fitted in-sample numbers for this loop
		}}
	data_fit %>% 
		mutate(f_price = f_price_real * cpi/100) %>%                                                              # un-deflate prices back to nominal values
		mutate(hist_pred = ifelse(yvintage <= l_hist, "History", "Prediction")) %>%                               # add history/prediction factor for charts
		select(yvintage, product_name, regioncd_name, pid, hist_pred, units, price, price_real, f_units, 
					 f_price, f_price_real, pop)
})

# binds results to one dataframe
data_predicted <- bind_rows(list_predicted)

# export results to excel
write.xlsx(data_predicted[order(data_predicted$pid),], file.path(dir_tables, "volume_projections.xlsx"))

#=====================#
# Plot predictions
#=====================#

for(p in unique(data_imp$product_name)) {

	# Combine actual and fitted values (in-sample and out-of-sample) from winner models to one dataframe
	data_plot <- data_predicted %>% filter(product_name == p) 
	
	# Aggreagte into national dataframe
	data_plot_national <- ddply(data_plot, ~ yvintage, summarise,
															price = weighted.mean(price, pop, na.rm = TRUE),
															f_price =weighted.mean(f_price, pop, na.rm = TRUE),
															units = sum(units, na.rm = TRUE),
															f_units = sum(f_units, na.rm = TRUE)) %>%
		mutate(hist_pred = ifelse(yvintage <= l_hist, "History", "Prediction"))
	
	# Plot predicted values - regional
	breaks <- c("History", "Prediction")
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = f_units, color = hist_pred, group = 1), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual(values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Units, #")
	ggsave(file.path(dir_charts, paste0("units_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = f_price, color = hist_pred, group = 1), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual(values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Price, $000")
	ggsave(file.path(dir_charts, paste0("price_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	
	# Plot fitted values - national
	plot <- ggplot(data_plot_national, aes(x = yvintage)) +
		geom_line(aes(y = f_units, color = hist_pred, group = 1), size = 1) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual(values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Units, #")
	ggsave(file.path(dir_charts, paste0("units_national_", p, ".png")), plot = plot, width = 8, height = 5)
	
	plot <- ggplot(data_plot_national, aes(x = yvintage)) +
		geom_line(aes(y = f_price, color = hist_pred, group = 1), size = 1) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual(values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Price, $000")
	ggsave(file.path(dir_charts, paste0("price_national_", p, ".png")), plot = plot, width = 8, height = 5)
}

#=============================================================================================================#
