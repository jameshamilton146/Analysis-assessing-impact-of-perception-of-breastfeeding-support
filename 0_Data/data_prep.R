library(haven)
library(data.table)
library(magrittr)
library(caret)
library(stats)
library(nnet)
library(car)
library(lmtest)
data.directory <- "C:/Users/James/Documents/R/Katies_Thesis/0_Data/"

ifps_dt <- "ifps2.sas7bdat" %>%
           paste0(data.directory, .) %>%
           read_sas() %>%
           as.data.table()


modeling_dt <- ifps_dt[, .(SAMPMIQ)]

# Breastfeeding difficulties, question 36 --------------------------

N36_cols <- colnames(ifps_dt)[colnames(ifps_dt) %like% "N36"]

N36_dt <- melt(ifps_dt[, .SD, .SDcols = c("SAMPMIQ", N36_cols)], 
                id.vars = "SAMPMIQ", 
                value.name = "response", 
                variable.name = "question")

N36_dt %<>% .[, .(response = sum(response, na.rm = TRUE)), keyby = .(SAMPMIQ)]

N36_dt[, breast_feeding_difficulties := FALSE]
N36_dt[response > 0, breast_feeding_difficulties := TRUE]

modeling_dt[N36_dt, 
        breast_feeding_difficulties := i.breast_feeding_difficulties, 
        on = .(SAMPMIQ)]

rm(N36_dt, N36_cols)

# ----------------------------------------------------------------------

# Breast feeding support, question 38 -----------------------------------

N38_dt <- ifps_dt[, .(N38, SAMPMIQ)]
N38_dt[, breast_feeding_support := FALSE]
N38_dt[N38 == 1, breast_feeding_support := TRUE]

modeling_dt[N38_dt, 
            breast_feeding_support := i.breast_feeding_support, 
            on = .(SAMPMIQ)]

rm(N38_dt)

# ------------------------------------------------------------------------

# Perception of support, question 39

N39_dt <- ifps_dt[, .(N39, SAMPMIQ)]

N39_dt[N39 %in% c(1, 2), perception_of_support := "Unhelfpul"]
N39_dt[N39 %in% c(3), perception_of_support := "Inconclusive"]
N39_dt[N39 %in% c(4, 5), perception_of_support := "Helpful"]

modeling_dt[N39_dt, 
            perception_of_support := i.perception_of_support, 
            on = .(SAMPMIQ)]

rm(N39_dt)

# -----------------------------------------------------------------------

# Breast Feeding Intensity at 2 - 6 months ----------------------------

months <- 1:6

for(i in months){
  
  if (i == 1) {
    feeding_cols <- colnames(ifps_dt)[colnames(ifps_dt) %like% "N40"]
    
    bf_feeding_col <- "N40A"
  } else {
    feeding_cols <- paste0("M", i, "A1", LETTERS[1:10])
    
    bf_feeding_col <- paste0("M", i, "A1A")
  }

  
  temp_dt <- melt(ifps_dt[, .SD, .SDcols = c("SAMPMIQ", feeding_cols)],
                  id.vars = "SAMPMIQ",
                  value.name = "response",
                  variable.name = "question")
  
  temp_dt[is.na(response) & question == bf_feeding_col, response := 0]
  
  temp_dt %<>% .[, .(breast_feeding_intensity = response[question == bf_feeding_col]/sum(response, 
                                                                                     na.rm = TRUE)), 
                keyby = .(SAMPMIQ)]
  
  temp_dt[is.nan(breast_feeding_intensity), breast_feeding_intensity := NA]
  
  modeling_dt[temp_dt, breast_feeding_intensity := i.breast_feeding_intensity, on = .(SAMPMIQ)]
  
  setnames(modeling_dt, "breast_feeding_intensity", paste0("breast_feeding_intensity_", i, "_mo"))
  
  rm(feeding_cols, bf_feeding_col, temp_dt)
}

# ----------------------------------------------------------------------

# BFHI Exposure --------------------------------------------------------

BFHI_dt <- ifps_dt[, .SD, .SDcols = c("N20", 
                                      "N11", 
                                      "N25", 
                                      "N28", 
                                      paste0("N29", c("A", "B", "C")),
                                      "N32", 
                                      "SAMPMIQ")]

number_BFHI_criteria <- 6

# Time until mother breastfed for the first time
BFHI_dt[N20 %in% 1:2, BFHI_exp_1 := TRUE]
BFHI_dt[N20 %in% 3:9, BFHI_exp_1 := FALSE]

# No Pacifiers
BFHI_dt[N11 %in% c(1, 3), BFHI_exp_2 := FALSE]
BFHI_dt[N11 %in% c(2), BFHI_exp_2 := TRUE]

# Rooming in
BFHI_dt[N25 %in% c(1), BFHI_exp_3 := TRUE]
BFHI_DT[N25 %in% c(2, 3), BFHI_exp_3 := FALSE]

# BF on Demand
BFHI_dt[N28 %in% c(1), BFHI_exp_4 := TRUE]
BFHI_dt[N28 %in% c(2, 3), BFHI_exp_4 := FALSE]

# Only BM
BFHI_dt[N29A == 2 & N29B == 2 & N29C == 2, BFHI_exp_5 := TRUE]
BFHI_dt[N29A %in% c(1, 3) | N29B %in% c(1, 3) | N29C %in% c(1, 3), BFHI_exp_5 := FALSE]

# Fostering support groups
BFHI_dt[N32 == 1, BFHI_exp_6 := TRUE]
BFHI_dt[N32 == 2, BFHI_exp_6 := FALSE]

BFHI_dt %<>% melt(., measure.vars = paste0("BFHI_exp_", 1:number_BFHI_criteria),
                  variable.name = "question",
                  value.name = "response")

BFHI_dt %<>% .[, .(BFHI_score = sum(response, na.rm = TRUE),
                   BFHI_all_missing = all(is.na(response))), by = .(SAMPMIQ)]

BFHI_dt[BFHI_all_missing == TRUE, BFHI_score := NA]

modeling_dt[BFHI_dt, BFHI_score := i.BFHI_score, on = .(SAMPMIQ)]

rm(BFHI_dt, number_BFHI_criteria)
# ----------------------------------------------------------------------

# Maternal Age ---------------------------------------------------------
P9_dt <- ifps_dt[, .(P9, SAMPMIQ)]

P9_dt[, unique(P9)] %>% sort()

P9_dt[, maternal_age := cut(P9, 
                            breaks = c(18, 25, 30, 35, Inf), 
                            labels = c("18-24", "25-29", "30-34", "35+"),
                            include.lowest = TRUE,
                            right = FALSE)]

modeling_dt[P9_dt, maternal_age := i.maternal_age, on = .(SAMPMIQ)]

rm(P9_dt)
# ----------------------------------------------------------------------

# Race/Ethnicity -------------------------------------------------------

race_dt <- ifps_dt[, .(RACE_ETH, SAMPMIQ)]

race_dt[RACE_ETH == 1, race_eth := "White, Non-Hispanic"]
race_dt[RACE_ETH == 2, race_eth := "Black, Non-Hispanic"]
race_dt[RACE_ETH == 3, race_eth := "Hispanic"]
race_dt[RACE_ETH %in% c(4, 5), race_eth := "Other"]

modeling_dt[race_dt, race_eth := i.race_eth, on = .(SAMPMIQ)]

rm(race_dt)
# ----------------------------------------------------------------------

# Education ---------------------------------------------
edu_dt <- ifps_dt[, .(EDUC, SAMPMIQ)]

edu_dt[EDUC %in% c(1, 2, 3), education := "Less Than High School"]
edu_dt[EDUC %in% c(4), education := "High School"]
edu_dt[EDUC %in% c(5), education := "1-3 Years College"]
edu_dt[EDUC %in% c(6, 7), education := "College Graduate"]

modeling_dt[edu_dt, education := i.education, on = .(SAMPMIQ)]

rm(edu_dt)
# -------------------------------------------------------

# Parity
parity_dt <- ifps_dt[, .(P41_1, P41_2, SAMPMIQ)]

parity_dt[P41_1 == 0 & P41_2 == 0, parity := "nullipara"]
parity_dt[P41_1 %in% 1:12 | P41_2 %in% 1:12, parity := "primipara"]

modeling_dt[parity_dt, parity := i.parity, on = .(SAMPMIQ)]

rm(parity_dt)
# -------------------------------------------------------

# Income Level --------------------------------------------

income_dt <- ifps_dt[, .(INCOME, SAMPMIQ)]

income_dt[INCOME %in% 31:37, income := "<$20,000"]
income_dt[INCOME %in% 38:46, income := "$20,000-$49,999"]
income_dt[INCOME %in% 47:57, income := ">$50,000"]

modeling_dt[income_dt, income := i.income, on = .(SAMPMIQ)]

rm(income_dt)
# ---------------------------------------------------------

# BMI -----------------------------------------------------

# ------------------------------------------------------------

# WIC Participation -------------------------------------------

wic_dt <- ifps_dt[, .(P6_1, P6_2, P6_3, SAMPMIQ)]

wic_dt[P6_1 == 1 | P6_2 == 1, wic_participation := TRUE]
wic_dt[P6_3 == 1, wic_participation := FALSE]

modeling_dt[wic_dt, wic_participation := i.wic_participation, on = .(SAMPMIQ)]


rm(wic_dt)
# ----------------------------------------------------------------------

# Modeling -------------------------------------------------------------

modeling_cols <- paste0("bf_intens_", 1:6, "_mo")

# break_options <- c(0, .2, .4, .6, .8, 1)
break_options <- c(0, .25, .75, 1)

modeling_dt[, (modeling_cols) := lapply(.SD, 
                                        cut, 
                                        breaks = break_options,
                                        include.lowest = TRUE), 
            .SDcols = paste0("breast_feeding_intensity_", 1:6, "_mo")]

modeling_dt[, (paste0("breast_feeding_intensity_", 1:6, "_mo")) := NULL]

modeling_dt %<>% .[breast_feeding_difficulties == TRUE & 
                     breast_feeding_support == TRUE & 
                     !is.na(perception_of_support)]

setnames(modeling_dt, modeling_cols, paste0(1:6, "_month"))

modeling_dt %<>% melt(., measure.vars = paste0(1:6, "_month"), 
                      variable.name = "time_of_bf_intens_meas", 
                      value.name = "breast_feeding_intensity")

modeling_dt %<>% .[!is.na(breast_feeding_intensity)
                   ]
factor_cols <- modeling_dt[, .SD, .SDcols = -c("SAMPMIQ")] %>% colnames()

modeling_dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

# Relevel Factors --------------------------------------------------------------

# modeling_dt[, `:=` (perception_of_support = relevel(perception_of_support, ref = "Inconclusive"),
#                     breast_feeding_intensity = relevel(breast_feeding_intensity, ref = "[0,0.2]"))]
# ------------------------------------------------------------------------------

modeling_data_list <- split(modeling_dt, by = "time_of_bf_intens_meas")


test_fit <- lapply(modeling_data_list, function(x){
  multinom(breast_feeding_intensity ~ perception_of_support + BFHI_score + maternal_age + race_eth + education, 
                 data = x) %>% return()
})

fit1 <- multinom(breast_feeding_intensity ~ perception_of_support + BFHI_score + maternal_age + race_eth + education + parity + income + wic_participation, 
               data = modeling_data_list[[1]])
fit2 <- multinom(breast_feeding_intensity ~ BFHI_score + maternal_age + race_eth + education + parity + income + wic_participation, 
                 data = modeling_data_list[[1]])

fit3 <- multinom(breast_feeding_intensity ~ perception_of_support + BFHI_score + maternal_age + race_eth + education + parity + wic_participation, 
                 data = modeling_data_list[[1]])
fit4 <- multinom(breast_feeding_intensity ~ perception_of_support + maternal_age + race_eth + education, 
                 data = modeling_data_list[[1]])
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

lrtest(fit1, fit3)
test_fit2 <- lapply(modeling_data_list, function(x){
  multinom(breast_feeding_intensity ~ BFHI_score + maternal_age + race_eth + education, 
           data = x) %>% return()
})

test_lr <- lapply(seq_along(test_fit), function(i){
  lrtest(test_fit[[i]], test_fit2[[i]])
})
fit_summ_list <- lapply(test_fit, summary)

p_value_list <- lapply(test_fit, function(x){
  z <- summary(x)$coefficients/summary(x)$standard.errors
  
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  
  return(p)
})

control_vars <- modeling_dt[, .SD, .SDcols = -c("SAMPMIQ", 
                                                "breast_feeding_difficulties", 
                                                "breast_feeding_support", 
                                                "perception_of_support",
                                                "breast_feeding_intensity",
                                                "time_of_bf_intens_meas")] %>% colnames()

univariate_fits <- lapply(control_vars, function(x){
  lapply(modeling_data_list, function(y){
    multinom(breast_feeding_intensity ~ get(x), data = y) %>% return()
  }) %>% return()
})

names(univariate_fits) <- control_vars

univariate_p_values <- lapply(univariate_fits, function(x){
  lapply(x, function(y){
    z <- summary(y)$coefficients/summary(y)$standard.errors
    
    p <- (1 - pnorm(abs(z), 0, 1)) * 2
    
    return(p)
  }) %>% return()
})

univariate_summaries <- lapply(univariate_fits, function(x){
  lapply(x, summary) %>% return()
})

univariate_p_values
# Plots -----------------------------------------------------------

dist_plot <- ggplot(modeling_dt, aes(x = breast_feeding_intensity, fill = perception_of_support)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ time_of_bf_intens_meas, ncol = 2)
# ----------------------------------------------------------------------
