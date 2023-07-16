library(haven)
library(data.table)
library(magrittr)
library(stats)
library(nnet)
library(lmtest)
library(MASS)
library(ggplot2)
library(googledrive)
library(pedometrics)
library(car)
library(ggalt)
project.directory <- "C:/Users/James/Documents/R/Katies_Thesis"
data.directory <- "C:/Users/James/Documents/R/Katies_Thesis/0_Data/"

modeling_dt <- fread(file.path(data.directory, "modeling_dt.csv"))

demographic_variables <- c("race_eth", "maternal_age", "education", "income", "bmi")

observation_count_dt <- lapply(c(demographic_variables, "perception_of_support"), function(x){
  temp_dt <- modeling_dt[, .N, keyby = c("time_of_bf_intens_meas", x)]
  
  setnames(temp_dt, x, "variable_level")
  temp_dt[, variable := x]
  return(temp_dt)
}
) %>% rbindlist()

# Subset to 1, 3, and 6 months

observation_count_dt <- observation_count_dt[time_of_bf_intens_meas %in% c("1_month", "3_month", "6_month")] %>%
                        dcast(., ... ~ time_of_bf_intens_meas, value.var = "N")

setorder(observation_count_dt, variable, variable_level)

month_cols <- c("1_month", "3_month", "6_month")
observation_count_dt[, (paste0(month_cols, "_perc")) := lapply(.SD, 
                                                               function(x){
                                                                 x / sum(x)
                                                                 }), 
                     by = .(variable), 
                     .SDcols = month_cols]

observation_count_dt[, (paste0(month_cols, "_perc")) := lapply(.SD, scales::percent), 
                     .SDcols = (paste0(month_cols, "_perc"))]

fwrite(observation_count_dt, 
       file.path(project.directory, 
                 "2_Outputs/Distribution_Tables/by_month_demographic_table.csv"))

drive_rm("by_month_demographic_table.csv")
drive_upload(file.path(project.directory, "2_Outputs/Distribution_Tables/by_month_demographic_table.csv"))

drive_share("by_month_demographic_table.csv",
            role = "reader",
            type = "user",
            emailAddress = "katie.palomares@gmail.com")


# Multicollinearity Visual ------------------------------------------------

vif_dt <- fread(file.path(project.directory, "2_outputs/Coefficient_Tables/vif_dt.csv"))

vif_plot <- ggplot(vif_dt, aes_string(x = "Variable", y = "GVIF^(1/(2*Df))")) +
  geom_bar(stat = "identity", fill = "blue4", width = .3, alpha = .6) +
  geom_hline(yintercept = 5, linetype = "dashed") +
  coord_flip() +
  theme_minimal() +
  labs(y = "Scaled Generalized Variance Inflation Factor",
       x = "Independent/Control Variable",
       title = "Multicollinearity Investigation",
       subtitle = "Scaled Generalized Variance Inflaction Factor Relative to Multicollinearity Threshold for Independent and Control Variables") +
  geom_text(aes(y=5, label="Multicollinearity Threshhold", x=5.5), colour="black", vjust = -1, angle=90)

# Risk Ratio Visualization
model_fit <- readRDS(file.path(project.directory, "2_Outputs/R_Objects/model_fit.RDS"))

coefficient_dt <- lapply(paste0(1:6, "_month"), function(x){
  dt <- data.table(model_fit[[x]][["full_model_summary"]]$coefficients)
  dt[, bf_intensity := c("Medium Breastfeeding Intensity", "High Breastfeeding Intensity")]
  
  dt <- melt(dt, 
             id.vars = "bf_intensity", 
             variable.name = "variable", 
             value.name = "coefficient")
  
  dt[, time_of_bf_meas := x]
  
  std_err_dt <- data.table(model_fit[[x]][["full_model_summary"]]$standard.errors)
  std_err_dt[, bf_intensity := c("Medium Breastfeeding Intensity", "High Breastfeeding Intensity")]
  
  std_err_dt <- melt(std_err_dt, 
             id.vars = "bf_intensity", 
             variable.name = "variable", 
             value.name = "standard_error")
  
  dt[std_err_dt, standard_error := i.standard_error, on = .(bf_intensity, variable)]
  return(dt)
}) %>% rbindlist()

coefficient_dt[, `:=` (conf_low = exp(coefficient - standard_error),
                rel_risk = exp(coefficient),
                conf_high = exp(coefficient + standard_error))]

coefficient_dt[, time_of_bf_meas := gsub("_month", "", time_of_bf_meas)]
coefficient_dt[, trans_variable :=as.character(gsub("perception_of_support", "", variable))]

p <- ggplot(coefficient_dt[variable %like% "perception"], 
            aes(x = rel_risk, y = time_of_bf_meas, color = trans_variable)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = conf_high, xmin = conf_low, color = trans_variable), size = .5, height = 
                     .2) +
    geom_point(size = 3.5) +
    theme_minimal()+
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Risk Ratio",
         title = "Breastfeeding Support Risk Ratios",
         y = "Time of Measurement (Months)",
         color = "Perception of Support") +
    facet_wrap(~bf_intensity, ncol = 1) +
    theme(legend.position = "bottom")

png(file.path(project.directory, "2_Outputs/Distribution_Graphs/breast_feeding_support_risk_ratios.png"))
p
dev.off()

drive_rm("breast_feeding_support_risk_ratios.png")
drive_upload(file.path(project.directory, "2_Outputs/Distribution_Graphs/breast_feeding_support_risk_ratios.png"))

drive_share("breast_feeding_support_risk_ratios.png",
            role = "reader",
            type = "user",
            emailAddress = "katie.palomares@gmail.com")
