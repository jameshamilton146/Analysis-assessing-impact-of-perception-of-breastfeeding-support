library(survival)

test <- copy(modeling_dt)

test[, bf_time := as.numeric(substr(time_of_bf_intens_meas, 1, 1))]

test[, survival := FALSE]
test[breast_feeding_intensity %in% c("High", "Medium"), survival := TRUE]

test2 <- test[, .(any_surv = any(survival == TRUE),
                  all_surv = all(survival == TRUE)), by = .(SAMPMIQ)][
                    any_surv == TRUE & all_surv == FALSE
                  ]

test3 <- rbind(test[SAMPMIQ %in% test2[, SAMPMIQ], .(death_time = min(bf_time[survival == FALSE]),
                                               death = TRUE), by = .(SAMPMIQ)],
               test[, .(all_surv = all(survival == TRUE)), by = .(SAMPMIQ)][
                          all_surv == TRUE, .(death_time = 6, death = FALSE, SAMPMIQ)
                        ])

merge_cols <- test[, .SD, .SDcols = -c("SAMPMIQ", 
                                       "bf_time", 
                                       "breast_feeding_intensity", "time_of_bf_intens_meas", "survival",
                                       "breast_feeding_difficulties",
                                       "breast_feeding_support")] %>% colnames()

test3[test, (merge_cols) := mget(paste0("i.", merge_cols)), on = .(SAMPMIQ)]


cox_survival_model <- coxph(Surv(death_time, death) ~ perception_of_support + BFHI_score + maternal_age + race_eth + education + parity + income + bmi + wic_participation + breast_feeding_attitude,
                            data = test3)


test4 <- test3[, lapply(.SD, function(x){rep(mean(as.numeric(x)), 3)}), .SDcols = -c("death_time", "death", "perception_of_support", "SAMPMIQ")]

test4[, perception_of_support := test3[, unique(as.numeric(perception_of_support))]]
library(survminer)

ggforest(cox_survival_model, data = test3)


ggadjustedcurves(cox_survival_model, variable = "perception_of_support")
ggcoxdiagnostics(cox_survival_model)
ggcoxzph(cox_survival_model)

png("survival_model_forest_graph.png")
ggforest(cox_survival_model, data = test3)
dev.off()

png("cox_survival_curve.png")
ggadjustedcurves(cox_survival_model, variable = "perception_of_support")
dev.off()

png("cox_survival_diagnostics.png")
ggcoxdiagnostics(cox_survival_model)
dev.off()

drive_rm("categorizing_num_of_resp_into_each_cat.csv")
drive_upload("cox_survival_diagnostics.png")

drive_share("cox_survival_diagnostics.png",
            role = "reader",
            type = "user",
            emailAddress = "katie.palomares@gmail.com")
