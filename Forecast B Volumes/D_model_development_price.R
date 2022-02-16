#=============================================================================================================#
#																																																							#
#	Model: Multi-unit Lifecycle Loss and Exposure Analyzer (mollea) - Volume Projection													#
#	Description: D. Model Development - Price																																		#
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

# Create new aggregated data ("yes"/"no") 
new_data <- "no"                  	                                                                          # "yes" will run "A_data_creation.R" to create a fresh data file; "no" will load an old version of "data_aggr.Rdata" from data folder

# Load new raw MOLLEA data ("yes"/"no") *very long process*
new_raw_data <- "no"                                                                                          # "yes" will load new raw MOLLEA data in "A_data_creation.R"; "no" will use saved raw data from data folder

# Define bounds for historical data
f_hist <- 2000                      	                                                                        # define first year of historical data
l_hist <- 2019                        	                                                                      # define last year of historical data

# Define bounds for model development
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
dir_tables <- file.path("results/model_dev/price/tables")                                                     #	location of result tables folder
dir_charts <- file.path("results/model_dev/price/charts")                                                     #	location of result charts folder

# create folders
dir.create(file.path("results"), showWarnings = FALSE)
dir.create(file.path("results/model_dev"), showWarnings = FALSE)
dir.create(file.path("results/model_dev/price"), showWarnings = FALSE)
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

#=============================================================================================================#
#                  									 Model development setup and data																			 ####
#=============================================================================================================#

#=====================#
# Setup
#=====================#

# Filter data to development bounds and add additional variables for estimation
data_dev_p <- data_aggr %>%
	filter((product_name == "Rental_New" & yvintage >= f_dev_Rental_New & yvintage <= l_dev) | 
				 	(product_name == "Rental_Existing" & yvintage >= f_dev_Rental_Existing & yvintage <= l_dev) | 
				 	(product_name == "Refinance" & yvintage >= f_dev_Refinance & yvintage <= l_dev)) %>%
	mutate(yd_vac_rate_national_bc = yd_vac_rate_national * regioncd_name_BC) %>%
	mutate(yp_stockp_qu = yp_stockp * regioncd_name_Quebec) %>%
	mutate(yd_rmort5y_2_on = yd_rmort5y_2 * regioncd_name_Ontario) %>%
	mutate(yd_rmort5y_2_at = yd_rmort5y_2 * regioncd_name_Atlantic) %>%
	mutate(yd_rmort_spread_at = yd_rmort_spread * regioncd_name_Atlantic) %>%
	mutate(yd_ump_rate_at = yd_ump_rate * regioncd_name_Atlantic) %>%
	mutate(yd_ump_rate_qu = yd_ump_rate * regioncd_name_Quebec) 

#====================================#
# Plot actual prices and grwoth rates
#====================================#

for(p in unique(data_dev_p$product_name)) {
	data_plot <- data_dev_p %>% filter(product_name == p)
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = price, color = "Price"), size = 1) +
		geom_line(aes(y = price_real, color = "Real price"), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = c("Price", "Real price"), values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Price, $000")
	ggsave(file.path(dir_charts, paste0("price_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = yp_price, color = "Price growth rate"), size = 1) + 
		geom_line(aes(y = yp_price_real, color = "Real price growth rate"), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = c("Price growth rate", "Real price growth rate"), values = eccaChartColors[c(3, 11)]) +
		xlab("Year of Origination") + ylab("Price growth rate, %")
	ggsave(file.path(dir_charts, paste0("yp_price_real_regional_", p, ".png")), plot = plot, width = 8, height = 5)
}

rm(list = c("data_plot", "plot"))
gc()

#=============================================================================================================#
#        												Serial correlation (AR) analysis - Price			 														 ####
#=============================================================================================================#

#=====================#
# AR Models
#=====================#

# Define models
models_Rental_New <- list(
	"ar_4" = "yp_price_real_1 + yp_price_real_2 + yp_price_real_3 + yp_price_real_4",
	"ar_3" = "yp_price_real_1 + yp_price_real_2 + yp_price_real_3",
	"ar_2" = "yp_price_real_1 + yp_price_real_2",
	"ar_1" = "yp_price_real_1"
	)

models_Rental_Existing <- list(
	"ar_4" = "yp_price_real_1 + yp_price_real_2 + yp_price_real_3 + yp_price_real_4",
	"ar_3" = "yp_price_real_1 + yp_price_real_2 + yp_price_real_3",
	"ar_2" = "yp_price_real_1 + yp_price_real_2",
	"ar_1" = "yp_price_real_1"
	)

models_Refinance <- list(
	"ar_4" = "yp_price_real_1 + yp_price_real_2 + yp_price_real_3 + yp_price_real_4",
	"ar_3" = "yp_price_real_1 + yp_price_real_2 + yp_price_real_3",
	"ar_2" = "yp_price_real_1 + yp_price_real_2",
	"ar_1" = "yp_price_real_1"
	)

#=====================#
# Estimation
#=====================#

# Run models, generate in-sample and out-of-sample fitted values and calculate fit stats
ar_results_price <- lapply(unique(data_dev_p$product_name), function(p) {
	data_est <- data_dev_p %>% 
		filter(product_name == p & yvintage >= get(paste0("f_dev_", p)) + 5)
	# estimate model
	models_results_is <- lapply(get(paste0("models_", p)), function(l) {
		lm(as.formula(paste0("yp_price_real ~", l)),
			 data = data_est,
			 na.rm = TRUE)
	})                                                                                                          # in-sample estimate models
	models_results_is_plm <- lapply(get(paste0("models_", p)), function(l) {
		plm(as.formula(paste0("yp_price_real ~", l)),
				data = data_est,
				na.rm = TRUE, index = c("regioncd_name", "yvintage"), model = "pooling")
	}) 	                                                                                                        # panel (plm) estimation for tests	                                                                                                        # estimate out-of-sample models
	models_results_is_df <- outreg(models_results_is, digits = 3L, alpha = c(0.1, 0.05, 0.01),
																 bracket = c("pv"), starred = c("coef"), robust = TRUE, small = TRUE,
																 constlast = TRUE, norepeat = TRUE, se = FALSE)                               # output in-sample estimation

	# Calculate models fit measures
	fit_stats <- lapply(names(models_results_is), function(m) {
		c(
			"BIC" = round(BIC(models_results_is[[m]]), 4),                                                       # model BIC
			"pbg_test_1" = round(pbgtest(models_results_is_plm[[m]], order = 1)$p.value, 4),                     # in-sample residuals serial correlation test
			"pbg_test_2" = round(pbgtest(models_results_is_plm[[m]], order = 2)$p.value, 4),                     # in-sample residuals serial correlation test
			"pbg_test_3" = round(pbgtest(models_results_is_plm[[m]], order = 3)$p.value, 4),                     # in-sample residuals serial correlation test
			"pbg_test_4" = round(pbgtest(models_results_is_plm[[m]], order = 4)$p.value, 4)                      # in-sample residuals serial correlation test
		)
	})
	names(fit_stats) <- names(models_results_is)
	fit_stats <- as.data.frame(bind_cols(fit_stats))
	fit_stats <- cbind(".variable" = "", 
										 ".stat" = c("BIC", "Residuals Breusch-Godfrey AR Test 1", 
										 						"Residuals Breusch-Godfrey AR Test 2", 
										 						"Residuals Breusch-Godfrey AR Test 3", 
										 						"Residuals Breusch-Godfrey AR Test 4"),
										 fit_stats)                                                                            # add fit-stats names
	results_is_df <- rbind(models_results_is_df, fit_stats)
	# export models results and stats to excel
	write.xlsx(results_is_df, file.path(dir_tables, paste0("AR_", p, ".xlsx")), row.names = TRUE)
})

rm(list = c("models_Rental_New", "models_Rental_Existing", "models_Refinance", "p", "dir"))
gc()

#=============================================================================================================#
#                  										Analysis of candidate econ variables			 													 ####
#=============================================================================================================#

rhs_econ_yd <- c("vac_rate", "vac_rate_national", "ump_rate", "emp", "hstarts", "rmort5y", 
								 "rprime", "rgt5y", "rgt10y", "rmort_spread", "pop")
rhs_econ_yp <- c("rpi", "rpi_national", "emp", "gdp", "hstarts", "stockp", "cpwti", "cpi_rent", "pop")
rhs_econ_yd <- as.vector(outer("yd", rhs_econ_yd, paste, sep = "_"))
rhs_econ_yp <- as.vector(outer("yp", rhs_econ_yp, paste, sep = "_"))
rhs_econ <- c(rhs_econ_yd, rhs_econ_yp)

#======================================#
# Univariate regression analysis
#======================================#

# models
econs <- grep(paste(rhs_econ, collapse = "|"), names(data_dev_p), value = TRUE)
models <- list(
	"Rental_New" = c("Rental_New", "yp_price_real_1 + yp_price_real_2", 4),
	"Rental_Existing" = c("Rental_Existing", "yp_price_real_1 + yp_price_real_2", 4),
	"Refinance" = c("Refinance", "yp_price_real_1", 3)
)

# Generate coefficients, p-values and correlations
coef<- lapply(models, function(p) {
	coef <- lapply(econs, function(m) {
		lm <- summary(lm(as.formula(paste0("yp_price_real ~", p[2], "+", m)), 
										 data = filter(data_dev_p, product_name == p[1] & yvintage >= get(paste0("f_dev_", p[1]))),
										 na.rm = TRUE))
		result <- as.data.frame(cbind(
			"variable" = m,
			"coefficient" = round(lm$coefficients[as.numeric(p[3]),1], 3), 
			"p_value" = round(lm$coefficients[as.numeric(p[3]),4], 3)))
	})
	coef <- bind_rows(coef)
})
write.xlsx(coef, file.path(dir_tables, paste0("econs_coef.xlsx")), row.names = TRUE)

rm(list = c("corr", "plot", "econs", "models", "coef", "result"))
gc()

#=============================================================================================================#
#                  												Model development - Price			 																	 ####
#=============================================================================================================#

#=====================#
# Models
#=====================#

# Define models
base_Rental_New <- c("yp_price_real_1 + yp_price_real_2")
models_Rental_New <- list(
	"model_1" = c(" + yd_vac_rate_national")
	,"model_2" = c(" + yd_vac_rate_national + yd_vac_rate_national_bc")
	,"model_3" = c(" + yd_vac_rate_national + yp_rpi")
	,"model_4" = c(" + yd_vac_rate_national + yp_rpi + yp_stockp + yp_stockp_qu")
)

base_Rental_Existing <- c("yp_price_real_1 + yp_price_real_2")
models_Rental_Existing <- list(
	"model_1" = c(" + yd_vac_rate_2 + yp_stockp")
	,"model_2" = c(" + yd_vac_rate_2 + yp_stockp + yd_rmort5y_2_at + yd_rmort5y_2_on")
	,"model_3" = c(" + yd_vac_rate_2 + yp_stockp + yd_ump_rate_at")
	,"model_4" = c(" + yd_vac_rate_2 + yp_stockp + yd_rmort5y_2_on + yd_ump_rate_at")
	,"model_5" = c(" + yd_vac_rate_2 + yp_stockp + yd_rmort_spread_at")
)

base_Refinance <- c("yp_price_real_1")
models_Refinance <- list(
	"model_1" = c(" + yd_rmort_spread_2 + yd_vac_rate_2 + yp_rpi_national_1")
	,"model_2" = c(" + yd_rmort_spread_2 + yd_vac_rate_2 + yp_rpi_1")
	,"model_3" = c(" + yd_rmort_spread_2 + yd_vac_rate_2 + yp_rpi_1 + yd_ump_rate_at + yd_ump_rate_qu")
)

#=====================#
# Estimation
#=====================#

# Run models, generate in-sample and out-of-sample fitted values and calculate fit stats
models_results_price <- lapply(unique(data_dev_p$product_name), function(p) {
	# estimate model
	models_results_is <- lapply(get(paste0("models_", p)), function(l) {
		lm(as.formula(paste0("yp_price_real ~", get(paste0("base_", p)), l)),
			 data = filter(data_dev_p, product_name == p & yvintage >= get(paste0("f_dev_", p))),
			 na.rm = TRUE)
	})                                                                                                          # estimate in-sample models
	models_results_is_plm <- lapply(get(paste0("models_", p)), function(l) {
		plm(as.formula(paste0("yp_price_real ~", get(paste0("base_", p)), l)),
				data = filter(data_dev_p, product_name == p & yvintage >= get(paste0("f_dev_", p))),
				na.rm = TRUE, index = c("regioncd_name", "yvintage"), model = "pooling")
	}) 	                                                                                                        # panel (plm) estimation for tests
	models_results_oos <- lapply(get(paste0("models_", p)), function(l) {
		lm(as.formula(paste0("yp_price_real ~", get(paste0("base_", p)), l)),
			 data = filter(data_dev_p, yvintage <= 2014 & product_name == p),
			 na.rm = TRUE)
	})	                                                                                                        # estimate out-of-sample models
	
	models_results_is_df <- outreg(models_results_is, digits = 3L, alpha = c(0.1, 0.05, 0.01),
																 bracket = c("pv"), starred = c("coef"), robust = TRUE, small = TRUE,
																 constlast = TRUE, norepeat = TRUE, se = FALSE)                               # output in-sample estimation
	models_results_oos_df <- outreg(models_results_oos, digits = 3L, alpha = c(0.1, 0.05, 0.01),
																 bracket = c("pv"), starred = c("coef"), robust = TRUE, small = TRUE,
																 constlast = TRUE, norepeat = TRUE, se = FALSE)                               # output out-of-sample estimation
	
	# Generate fitted values (in-sample and out-of-sample)
	yvintage_for_fit_is <- unique(filter(data_dev_p, yvintage >= get(paste0("f_dev_", p)) + 4)$yvintage)        # vector of yvintage for in-sample
	yvintage_for_fit_oos <- unique(filter(data_dev_p, yvintage >= oos_start)$yvintage)                          # vector of yvintage for out-of-sample
	
	fitted_values <- lapply(names(models_results_is), function(m) {
		data_fit <- data_dev_p %>% filter(product_name == p & yvintage >= get(paste0("f_dev_", p))) %>%
			select(yvintage, regioncd_name, pop, price, price_real, cpi,
						 yp_price_real, yp_price_real_1,  names(models_results_is[[m]]$coefficients[-1])) %>%             # trim fit data to only relevant vars
			mutate(model_fit_is = predict(models_results_is[[m]], type = "response", newdata = ., na.rm = TRUE)) %>%# model in-sample fitted values
			mutate(model_resid_is = yp_price_real - model_fit_is) %>%                                               # model in-sample residuals
			mutate(yp_price_real_fit_is = as.numeric(ifelse(yvintage >= get(paste0("f_dev_", p)) + 4, 
																								 NA, yp_price_real))) %>%                                     # empty yp_price_real fitted values with "anchor" yp_price_real for in-sample fit
			mutate(yp_price_real_fit_oos = as.numeric(ifelse(yvintage >= oos_start, NA, yp_price_real))) %>%        # empty fitted yp_price_real values with "anchor" yp_price_real for out-of-sample fit
			mutate(price_real_fit_is = as.numeric(ifelse(yvintage >= get(paste0("f_dev_", p)) + 4, 
																									 NA, price_real))) %>%                                      # empty price_real fitted values with "anchor" price_real for in-sample fit
			mutate(price_real_fit_oos = as.numeric(ifelse(yvintage >= oos_start, NA, price_real)))                  # empty fitted price_real values with "anchor" price_real for out-of-sample fit
		for(y in yvintage_for_fit_is) {
			data_fit <- data_fit %>%
				group_by(regioncd_name) %>%
				mutate(yp_price_real_1 = dplyr::lag(yp_price_real_fit_is, 1, order_by = yvintage)) %>%
				mutate(yp_price_real_2 = dplyr::lag(yp_price_real_fit_is, 2, order_by = yvintage)) %>%
				mutate(yp_price_real_3 = dplyr::lag(yp_price_real_fit_is, 3, order_by = yvintage)) %>%
				ungroup()                                                                                             # set lagged yp_price_real (AR terms) to fitted values from previous loop
			for(r in unique(data_fit$regioncd_name)) {
				data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r,
							 "yp_price_real_fit_is"] <- predict(models_results_is[[m]],
							 													 type = "response",
							 													 newdata = filter(data_fit, yvintage == y & regioncd_name == r),
							 													 na.rm = TRUE)                                                        # generate yp_price_real fitted in-sample numbers for this loop
				data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, "price_real_fit_is"] <- 
					(data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, "yp_price_real_fit_is"] + 1) * 
					data_fit[data_fit$yvintage == y - 1 & data_fit$regioncd_name == r, "price_real_fit_is"]             # generate price_real fitted in-sample numbers for this loop
			}}
			for(y in yvintage_for_fit_oos) {
				data_fit <- data_fit %>% group_by(regioncd_name) %>%
					mutate(yp_price_real_1 = dplyr::lag(yp_price_real_fit_oos, 1, order_by = yvintage)) %>%
					mutate(yp_price_real_2 = dplyr::lag(yp_price_real_fit_oos, 2, order_by = yvintage)) %>%
					mutate(yp_price_real_3 = dplyr::lag(yp_price_real_fit_oos, 3, order_by = yvintage)) %>%
					ungroup()
				for(r in unique(data_fit$regioncd_name)) {
					data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, 
									 "yp_price_real_fit_oos"] <- predict(models_results_is[[m]], 
									 															type = "response", 
									 															newdata = filter(data_fit,yvintage == y & regioncd_name == r), 
									 															na.rm = TRUE)                                                 # generate yp_price_real fitted in-sample numbers for this loop
					data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, "price_real_fit_oos"] <- 
						(data_fit[data_fit$yvintage == y & data_fit$regioncd_name == r, "yp_price_real_fit_oos"] + 1) * 
						data_fit[data_fit$yvintage == y - 1 & data_fit$regioncd_name == r, "price_real_fit_oos"]          # generate price_real fitted in-sample numbers for this loop
				}}
		data_fit %>% 
			mutate(price_fit_is = price_real_fit_is * cpi/100) %>%
			mutate(price_fit_oos = price_real_fit_oos * cpi/100) %>%
			select(yvintage, regioncd_name, pop, price, price_real, yp_price_real, yp_price_real_1, 
						 price_fit_is, price_real_fit_is, yp_price_real_fit_is, price_fit_oos, price_real_fit_oos, 
						 yp_price_real_fit_oos, model_resid_is)
	})
	names(fitted_values) <- names(get(paste0("models_", p)))
	
	# Calculate models fit measures
	fit_stats <- lapply(names(models_results_is), function(m) {
		c(
			"residul_ur_ips" = round(purtest(model_resid_is ~ trend,
																			 data = filter(fitted_values[[m]], !is.na(model_resid_is)),
																			 index = "regioncd_name", test = "ips", lags = "SIC",
																			 pmax = 3, na.rm = TRUE)[[1]][[6]], 4),                                 # in-sample residuals stationarity
			"residul_ur_hadri" = round(purtest(model_resid_is ~ trend,
																				 data = filter(fitted_values[[m]], !is.na(model_resid_is)),
																				 index = "regioncd_name", test = "hadri", lags = "SIC",
																				 pmax = 3, na.rm = TRUE)[[1]][[6]], 3),                               # in-sample residuals stationarity
			"pbg_test" = round(pbgtest(models_results_is_plm[[m]], order = 1)$p.value, 3),                          # in-sample residuals serial correlation test AR(1)
			"pcd_test_lm" = round(pcdtest(models_results_is_plm[[m]], test = c("lm"))$p.value, 3),                  # in-sample residuals cross-sectional dependence
			"pcd_test_cd" = round(pcdtest(models_results_is_plm[[m]], test = c("cd"))$p.value, 3),                  # in-sample residuals cross-sectional dependence
			"bp_test" = round(bptest(as.formula(paste0("yp_price_real ~", 
																								 paste(names(models_results_is[[m]]$coefficients[-1]), 
																								 			collapse = " + "))), 
															 data = filter(data_dev_p, product_name == p))$p.value, 3),                     # in-sample Breusch-Pagan test for homoskedasticity
			"rsq_is" = with(fitted_values[[m]], round(cor(price, price_fit_is, 
																										use = "complete.obs")^2, 3)),                             # rsq for in-sample
			"rsq_oos_5" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) & 
																	yvintage >= oos_start), round(cor(price, price_fit_oos)^2, 3)),             # rsq for out-of-sample
			"rsq_oos_5_on" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) & 
																	 	yvintage >= oos_start & regioncd_name == "Ontario"),
														round(cor(price, price_fit_oos)^2, 3)),                                           # rsq for out-of-sample for ontario
			"rsq_oos_5_qu" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) & 
																	 	yvintage >= oos_start & regioncd_name == "Quebec"), 
														round(cor(price, price_fit_oos)^2, 3)),                                           # rsq for out-of-sample for Quebec
			"rmse_is" = with(filter(fitted_values[[m]], !is.na(price_fit_is)), 
											 round(RMSE(price, price_fit_is), 3)),                                                  # rmse for in-sample
			"rmse_oos_5" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) & 
																 	yvintage >= oos_start), round(RMSE(price, price_fit_oos), 3)),              # rmse for out-of-sample
			"rmse_oos_5_on" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) & 
																			yvintage >= oos_start & regioncd_name == "Ontario"), 
														 round(RMSE(price, price_fit_oos), 3)),                                           # rmse for out-of-sample Ontario
			"rmse_oos_5_qu" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) & 
																			yvintage >= oos_start & regioncd_name == "Quebec"), 
														 round(RMSE(price, price_fit_oos), 3)),                                           # rmse for out-of-sample Quebec
			"sum_resid_oos" = with(filter(fitted_values[[m]], !is.na(price_fit_oos) 
																		& yvintage >= oos_start), round(sum(price - price_fit_oos), 3))           # sum of residuals for out-of-sample
		)
	})
	names(fit_stats) <- names(models_results_is)
	fit_stats <- as.data.frame(bind_cols(fit_stats))
	fit_stats <- cbind(".variable" = "", 
										 ".stat" = c("Residuals Unit-Root IPS test",
										 						"Residuals Unit-Root Hadri test",
										 						"Residuals AR(1) Breusch-Godfrey Test", 
										 						"Residuals cross-sectional dependence Breusch-Pagan LM test",
										 						"Residuals cross-sectional dependence Pesaran CD",
										 						"Breusch-Pagan homoscedasticity test",
										 						"IS R-Sq", "OOS R-Sq 5yr", "OOS R-Sq 5yr Ontario", "OOS R-Sq 5yr Quebec", 
										 						"IS RMSE", "OOS RMSE 5yr", "OOS RMSE 5yr Ontario", "OOS RMSE 5yr Quebec", 
										 						"OOS Sum of residuals 5yr"),
										 fit_stats)                                                                               # add fit-stats names
	results_is_df <- rbind(models_results_is_df, fit_stats)
	# export models results and stats to excel
	write.xlsx(list(results_is_df, models_results_oos_df),
						 file.path(dir_tables, paste0("model_dev_", p, ".xlsx")), row.names = TRUE)                       # export results to excel
	# keep fitted values and fit stats
	list(models_results_is = models_results_is, fitted_values = fitted_values, fit_stats = fit_stats)
})
names(models_results_price) <- unique(data_dev_p$product_name)                                                # give results list names

#=====================#
# select winner models
#=====================#

winner_models <- list(
	"Rental_New" = c("model_3", "model_1"),
	"Rental_Existing" = c("model_1", "model_5"),
	"Refinance" = c("model_1", "model_3")
)

#===========================#
# export winner models
#===========================#

# output estimation results
winner_models_results <- outreg(list("Rental New" = models_results_price$Rental_New$models_results_is[[winner_models$Rental_New[1]]], 
																		 "Rental Existing" = models_results_price$Rental_Existing$models_results_is[[winner_models$Rental_Existing[1]]], 
																		 "Refinance" = models_results_price$Refinance$models_results_is[[winner_models$Refinance[1]]]), 
																digits = 3L, alpha = c(0.1, 0.05, 0.01),
																bracket = c("pv"), starred = c("coef"), robust = TRUE, small = TRUE,
																constlast = TRUE, norepeat = TRUE, se = FALSE, pv = TRUE)

# output estimation fit stats
winner_models_fit_stats <- cbind(
	".variable" = "",
	".stat" = c("Residuals Unit-Root IPS test", "Residuals Unit-Root Hadri test", 
							"Residuals AR(1) Breusch-Godfrey Test", 
							"Residuals cross-sectional dependence Breusch-Pagan LM test",
							"Residuals cross-sectional dependence Pesaran CD",
							"Breusch-Pagan homoscedasticity test",
							"IS R-Sq", "OOS R-Sq 5yr", "OOS R-Sq 5yr Ontario", 
							"OOS R-Sq 5yr Quebec", "IS RMSE", "OOS RMSE 5yr", "OOS RMSE 5yr Ontario", 
							"OOS RMSE 5yr Quebec", "OOS Sum of residuals 5yr"),
	"Rental New" = models_results_price$Rental_New$fit_stats[[winner_models$Rental_New[1]]],
	"Rental Existing" = models_results_price$Rental_Existing$fit_stats[[winner_models$Rental_Existing[1]]],
	"Refinance" = models_results_price$Refinance$fit_stats[[winner_models$Refinance[1]]])
winner_models_results <- rbind(winner_models_results, winner_models_fit_stats)

# export results to excel
write.xlsx(winner_models_results, file.path(dir_tables, "model_dev_winners.xlsx"), row.names = TRUE)

# export winner models to file
winner_models_price <- list("Rental_New" = models_results_price$Rental_New$models_results_is[[winner_models$Rental_New[1]]], 
														"Rental_Existing" = models_results_price$Rental_Existing$models_results_is[[winner_models$Rental_Existing[1]]], 
														"Refinance" = models_results_price$Refinance$models_results_is[[winner_models$Refinance[1]]])
save(winner_models_price, file = file.path("results/model_dev", "winner_models_price.RData"))

#=====================#
# Plot winner models
#=====================#

for(p in unique(data_dev_p$product_name)) {
	dir <- paste0(p)
	dir.create(file.path(dir_charts, dir), showWarnings = FALSE)
	
	# Combine actual and fitted values (in-sample and out-of-sample) from winner models to one dataframe
	data_plot_1 <- models_results_price[[p]]$fitted_values[[winner_models[[p]][1]]] %>%
		select(yvintage, regioncd_name, pop, price, price_real, yp_price_real, model_resid_is, 
					 price_real_fit_is_1 = price_real_fit_is, price_fit_is_1 = price_fit_is, 
					 price_real_fit_oos_1 = price_real_fit_oos, price_fit_oos_1 = price_fit_oos, 
					 yp_price_real_fit_is_1 = yp_price_real_fit_is, yp_price_real_fit_oos_1 = yp_price_real_fit_oos)    # get fitted values of 1nd winner model
	data_plot_2 <- models_results_price[[p]]$fitted_values[[winner_models[[p]][2]]] %>%
		select(yvintage, regioncd_name, price_fit_is_2 = price_fit_is, price_real_fit_is_2 = price_real_fit_is, 
					 price_real_fit_oos_2 = price_real_fit_oos, price_fit_oos_2 = price_fit_oos,
					 yp_price_real_fit_is_2 = yp_price_real_fit_is, yp_price_real_fit_oos_2 = yp_price_real_fit_oos)    # get fitted values of 2nd winner model
	data_plot <- data_plot_1 %>% left_join(data_plot_2, by = c("yvintage", "regioncd_name"))                    # merge fitted values of 1st and 2nd winner models
	data_econ_residual <- data_plot %>% select(yvintage, regioncd_name, model_resid_is)                         # get residuals of winner model for plotting against econs
	data_econ_residual <- data_dev_p %>% filter(product_name == p) %>%
		select(yvintage, regioncd_name, all_of(rhs_econ)) %>%
		left_join(data_econ_residual, by = c("yvintage", "regioncd_name"))                                        # merge residuals and winner model econs

	# Aggreagte into national dataframe
	data_plot_national <- ddply(data_plot, ~ yvintage, summarise,
															price = weighted.mean(price, pop, na.rm = TRUE),
															price_fit_is_1 =weighted.mean(price_fit_is_1, pop, na.rm = TRUE),
															price_fit_oos_1 = weighted.mean(price_fit_oos_1, pop, na.rm = TRUE),
															price_fit_is_2 = weighted.mean(price_fit_is_2, pop, na.rm = TRUE),
															price_fit_oos_2 = weighted.mean(price_fit_oos_2, pop, na.rm = TRUE),
															price_real = weighted.mean(price_real, pop, na.rm = TRUE),
															price_real_fit_is_1 =weighted.mean(price_real_fit_is_1, pop, na.rm = TRUE),
															price_real_fit_oos_1 = weighted.mean(price_real_fit_oos_1, pop, na.rm = TRUE),
															price_real_fit_is_2 = weighted.mean(price_real_fit_is_2, pop, na.rm = TRUE),
															price_real_fit_oos_2 = weighted.mean(price_real_fit_oos_2, pop, na.rm = TRUE),
															model_resid_is = sum(model_resid_is))

	# Plot fitted values - regional
	breaks <- c("Actual", "Fitted - winner model")
	#breaks <- c("Actual", paste0("Fitted - ", winner_models[[p]][1]), paste0("Fitted - ", winner_models[[p]][2]))
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = price, color = breaks[1]), size = 1) +
		geom_line(aes(y = price_fit_is_1, color = breaks[2]), size = 1) +
		#geom_line(aes(y = price_fit_is_2, color = breaks[3]), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = breaks, values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Price, $000")
	ggsave(file.path(dir_charts, dir, paste0("is_regional_", p, ".png")), plot = plot, width = 8, height = 5)

	plot <- ggplot(filter(data_plot, yvintage >= oos_start - 5), aes(x = yvintage)) +
		geom_line(aes(y = price_fit_oos_1, color = breaks[2]), size = 1) +
		#geom_line(aes(y = price_fit_oos_2, color = breaks[3]), size = 1) +
		geom_line(aes(y = price, color = breaks[1]), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = breaks, values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Price, $000") + scale_x_continuous(breaks = seq(oos_start - 5, oos_start + 4, by = 2))
	ggsave(file.path(dir_charts, dir, paste0("oos_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	
	plot <- ggplot(data_plot, aes(x = yvintage)) +
		geom_line(aes(y = yp_price_real, color = breaks[1]), size = 1) +
		geom_line(aes(y = yp_price_real_fit_is_1, color = breaks[2]), size = 1) +
		#geom_line(aes(y = yp_price_real_fit_is_2, color = breaks[3]), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = breaks, values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Real price Growth Rate, %")
	ggsave(file.path(dir_charts, dir, paste0("is_regional_yp_", p, ".png")), plot = plot, width = 8, height = 5)
	
	plot <- ggplot(filter(data_plot, yvintage >= oos_start - 5), aes(x = yvintage)) +
		geom_line(aes(y = yp_price_real_fit_oos_1, color = breaks[2]), size = 1) +
		#geom_line(aes(y = yp_price_real_fit_oos_2, color = breaks[3]), size = 1) +
		geom_line(aes(y = yp_price_real, color = breaks[1]), size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = breaks, values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Real price Growth Rate, %") + scale_x_continuous(breaks = seq(oos_start - 5, oos_start + 4, by = 2))
	ggsave(file.path(dir_charts, dir, paste0("oos_regional_yp_", p, ".png")), plot = plot, width = 8, height = 5)
	
	# Plot fitted values - national
	plot <- ggplot(data_plot_national, aes(x = yvintage)) +
		geom_line(aes(y = price, color = breaks[1]), size = 1) +
		geom_line(aes(y = price_fit_is_1, color = breaks[2]), size = 1) +
		#geom_line(aes(y = price_fit_is_2, color = breaks[3]), size = 1) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = breaks, values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Price, $000")
	ggsave(file.path(dir_charts, dir, paste0("is_national_", p, ".png")), plot = plot, width = 8, height = 5)

	plot <- ggplot(filter(data_plot_national, yvintage >= oos_start - 5), aes(x = yvintage)) +
		geom_line(aes(y = price_fit_oos_1, color = breaks[2]), size = 1) +
		#geom_line(aes(y = price_fit_oos_2, color = breaks[3]), size = 1) +
		geom_line(aes(y = price, color = breaks[1]), size = 1) +
		theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
		scale_colour_manual("", breaks = breaks, values = eccaChartColors[c(3, 11, 2)]) +
		xlab("Year of Origination") + ylab("Price, $000") + scale_x_continuous(breaks = seq(oos_start - 5, oos_start + 4, by = 2))
	ggsave(file.path(dir_charts, dir, paste0("oos_national_", p, ".png")), plot = plot, width = 8, height = 5)
	
	# Plot predicted residuals of 1st winner model - regional
	plot <- ggplot(data_plot, aes(x = yvintage)) + 
		geom_line(aes(y = model_resid_is), color = eccaChartColors[11], size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
		xlab("Year of Origination") + ylab("Residuals - yp_price")
	ggsave(file.path(dir_charts, dir, paste0("model_resid_is_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	
	# Plot predicted residuals of 1st winner model - national
	plot <- ggplot(data_plot_national, aes(x = yvintage)) + 
		geom_line(aes(y = model_resid_is), color = eccaChartColors[11], size = 1) +
		theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
		xlab("Year of Origination") + ylab("Residuals - yp_price")
	ggsave(file.path(dir_charts, dir, paste0("model_resid_is_national_", p, ".png")), plot = plot, width = 8, height = 5)

	# Plot econs vs. residuals of 1st winner model - regional scatter
	for(m in rhs_econ) {
	plot <- ggplot(data_econ_residual, aes(x = get(m), y = model_resid_is)) + 
		geom_point(color = eccaChartColors[11], size = 1) +
		facet_wrap( ~ regioncd_name, ncol = 3) +
		theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) + 
		xlab(m) + ylab("In-sample Residuals")
	ggsave(file.path(dir_charts, dir, paste0(m, "_model_resid_is_regional_", p, ".png")), plot = plot, width = 8, height = 5)
	}
}

#===================================================#
# Break points analysis of residuals from models
#===================================================#

# regional break points
break_points <- lapply(unique(data_dev_p$product_name), function(p) {
	# Combine actual and fitted values (in-sample and out-of-sample) from 1st winner models to one dataframe
	data_break <- models_results_price[[p]]$fitted_values[[winner_models[[p]][1]]] %>%
		select(yvintage, regioncd_name, model_resid_is) 
	break_points <- lapply(unique(data_dev_p$regioncd_name), function(r) {
		data_break_region <- data_break %>% filter(regioncd_name == r)
		break_points <- breakdates(breakpoints(model_resid_is ~ yvintage, data = data_break_region, h = 4))
		break_points <- (get(paste0("f_dev_",p)) + break_points * (l_hist - get(paste0("f_dev_",p)) + 1)) - 1
	})
	names(break_points) <- unique(data_dev_p$regioncd_name)
	break_points <- as.data.frame(bind_cols(break_points))
})
names(break_points) <- unique(data_dev_p$product_name)
break_points <- unlist(break_points)

# National break points
break_points_national <- lapply(unique(data_dev_p$product_name), function(p) {
	# Combine actual and fitted values (in-sample and out-of-sample) from 1st winner models to one dataframe
	data_break <- models_results_price[[p]]$fitted_values[[winner_models[[p]][1]]] %>%
		select(yvintage, regioncd_name, model_resid_is)
	data_break <- ddply(data_break, ~ yvintage, summarise, model_resid_is = sum(model_resid_is))
	break_points <- breakdates(breakpoints(model_resid_is ~ yvintage, data = data_break, h = 4))
	break_points <- (get(paste0("f_dev_",p)) + break_points * (f_hist - get(paste0("f_dev_",p)) + 1)) - 1
})
names(break_points_national) <- unique(data_dev_p$product_name)
break_points_national <- unlist(break_points_national)

rm(list = c("base_Rental_New", "models_Rental_New", "base_Rental_Existing", "models_Rental_Existing", 
						"base_Refinance", "models_Refinance", "results", "models_export", "is_fitted_values", 
						"is_fitted_values_national", "rsq", "breaks", "data_plot_1", "data_plot_2", "data_plot", 
						"data_plot_national", "econs", 'data_econ_residual', "plot", "p", "dir", "rhs_econs"))
gc()

#=============================================================================================================#
#                  											Price of other products analysis																	 ####
#=============================================================================================================#

# create price data for the 3 products
data_price_other <- models_results_price[["Rental_New"]]$fitted_values[[winner_models[["Rental_New"]][1]]] %>%
	select(yvintage, regioncd_name, yp_price_real_Rental_New = yp_price_real, 
				 yp_price_real_1_Rental_New = yp_price_real_1, model_resid_is_Rental_New = model_resid_is) %>%
	left_join(models_results_price[["Rental_Existing"]]$fitted_values[[winner_models[["Rental_Existing"]][1]]]
						[, c("yvintage", "regioncd_name", "yp_price_real", "yp_price_real_1", "model_resid_is")], 
						by = c("yvintage", "regioncd_name")) %>%
	rename(yp_price_real_Rental_Existing = yp_price_real, yp_price_real_1_Rental_Existing = yp_price_real_1, 
				 model_resid_is_Rental_Existing = model_resid_is) %>%
	left_join(models_results_price[["Refinance"]]$fitted_values[[winner_models[["Refinance"]][1]]]
						[, c("yvintage", "regioncd_name", "yp_price_real", "yp_price_real_1", "model_resid_is")], 
						by = c("yvintage", "regioncd_name")) %>%
	rename(yp_price_real_Refinance = yp_price_real, yp_price_real_1_Refinance = yp_price_real_1, 
				 model_resid_is_Refinance = model_resid_is)

# Correlation tables
corr_yp_price <- data_price_other[c("yp_price_real_Rental_New", "yp_price_real_Rental_Existing", "yp_price_real_Refinance", 
																 "yp_price_real_Rental_New", "yp_price_real_Rental_Existing", "yp_price_real_Refinance")] %>%
	as.matrix %>%
	cor(use = "complete.obs") %>%
	as.data.frame
corr_yp_price_1 <- data_price_other[c("yp_price_real_Rental_New", "yp_price_real_Rental_Existing", "yp_price_real_Refinance", 
																		"yp_price_real_1_Rental_New", "yp_price_real_1_Rental_Existing", "yp_price_real_1_Refinance")] %>%
	as.matrix %>%
	cor(use = "complete.obs") %>%
	as.data.frame
corr_resid <- data_price_other[c("model_resid_is_Rental_New", "model_resid_is_Rental_Existing", "model_resid_is_Refinance", 
																 "yp_price_real_Rental_New", "yp_price_real_Rental_Existing", "yp_price_real_Refinance")] %>%
	as.matrix %>%
	cor(use = "complete.obs") %>%
	as.data.frame
corr_resid_1 <- data_price_other[c("model_resid_is_Rental_New", "model_resid_is_Rental_Existing", "model_resid_is_Refinance", 
																	 "yp_price_real_1_Rental_New", "yp_price_real_1_Rental_Existing", "yp_price_real_1_Refinance")] %>%
	as.matrix %>%
	cor(use = "complete.obs") %>%
	as.data.frame
write.xlsx(list(corr_yp_price, corr_yp_price_1, corr_resid, corr_resid_1), file.path(dir_tables, paste0("other_price_corr.xlsx")), row.names = TRUE)

#=============================================================================================================#
