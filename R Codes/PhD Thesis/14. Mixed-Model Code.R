# This code will run the mixed-model analysis for Aim 3
# Models will be random slopes and intercepts

# Load the necessary packages
library(lme4)
library(flexplot)
library(readr)
library(r2mlm)
library(sjPlot)
library(dplyr)

# install.packages("devtools")
# # install the stable version
# devtools::install_github("dustinfife/flexplot")
# 
# # install the development version
# devtools::install_github("dustinfife/flexplot", ref="development")

# Load "Aim 3 (mixed model) Variables" from Chapter 6 - Aim 3
file_path <- file.choose()

# Read the data in
df_mmv <- read_csv(file_path)

# Rename Selected Variables
colnames(df_mmv)[colnames(df_mmv) == "Trunk Flexion @ BR"] <- "Trunk_Flexion_BR"
colnames(df_mmv)[colnames(df_mmv) == "COM Velocity (X) (MAW to BR)"] <- "COM_VelX_MAWtoBR"

# Only include Trunk_Flexion_BR and COM_VelX_MAWtoBR
df_mmv_cleaned <- df_mmv %>% 
  # filter(!is.na(Trunk_Flexion_BR) & !is.na(COM_VelX_MAWtoBR))
  select(1:10) 

df_mmv_cleaned <- df_mmv_cleaned[complete.cases(df_mmv_cleaned), ]
 

# Generate the baseline model (no predictor variables) and test assumptions
# install.packages("DHARMa")
library(DHARMa)

baseline <- lmer(AIM ~ 1 + (1 | Player), data = df_mmv_cleaned) # build the model
summary(baseline) # summarise the model
plot(baseline)
hist(residuals(baseline), breaks = 30, main = "Residuals Histogram") # plot the residuals
simulationOutput <- simulateResiduals(fittedModel = baseline) # use DHARMa to test assumptions
plot(simulationOutput) # plot assumptions 
# plotQQunif(simulationOutput) # left plot in plot.DHARMa()
# plotResiduals(simulationOutput) # right plot in plot.DHARMa()
shapiro.test(residuals(baseline)) # normality statistics

residuals_baseline <- as.data.frame(residuals(baseline, type = "pearson"))
residuals_baseline <- cbind(residuals_baseline, df_mmv_cleaned$Player)
colnames(residuals_baseline) <- c("Residuals", "Player")
car::leveneTest(Residuals ~ as.factor(Player), data = residuals_baseline)

################################################################
# Be sure to export the histogram (600) and DHARMa (1200) plots!
################################################################

baseline1 <- glmer(AIM ~ 1 + (1 | Player), family = Gamma(link = "log"),
                   data = df_mmv_cleaned)
plot(baseline1)
hist(residuals(baseline1), breaks = 30, main = "Residuals Histogram")
simulationOutput <- simulateResiduals(fittedModel = baseline1) # use DHARMa to test assumptions
plot(simulationOutput) # plot assumptions
shapiro.test(residuals(baseline1)) # normality statistics

residuals_baseline1 <- as.data.frame(residuals(baseline1, type = "pearson"))
residuals_baseline1 <- cbind(residuals_baseline1, df_mmv_cleaned$Player)
colnames(residuals_baseline1) <- c("Residuals", "Player")
car::leveneTest(Residuals ~ as.factor(Player), data = residuals_baseline1)

########################################################################################
# Based on the model assumptions, a generalized linear model is deemed more appropriate!
########################################################################################

# Model 1 (1 predictor) with random intercepts
model_1_ri <- glmer(AIM ~ Trunk_Flexion_BR + # fixed effects
                      (1 | Player), # random effects
                    family = Gamma(link = "log"), # set the distribution to gamma
                    data = df_mmv_cleaned)

# Model 1 (1 predictor) with random intercepts & slopes
model_1_ris <- glmer(AIM ~ Trunk_Flexion_BR + # fixed effects
                       (1 + Trunk_Flexion_BR | Player), # random effects
                     family = Gamma(link = "log"), # set the distribution to gamma
                     data = df_mmv_cleaned)

# Compare models
model.comparison(model_1_ris, model_1_ri)
# summary(model_1_ri) # summarize preferred model

# Compare model_1_ri to baseline
model.comparison(baseline1, model_1_ri)


#####################################################################################################
# If model_1_ri is better than baseline (AIC/BIC, Bayes Factor, p value), add next predictor variable
#####################################################################################################

# Model 2 (2 predictors) with random intercepts
model_2_ri <- glmer(AIM ~ Trunk_Flexion_BR + COM_VelX_MAWtoBR + # fixed effects
                      (1 | Player), # random effects
                    family = Gamma(link = "log"), # set the distribution to gamma,
                    # Gabrielle change: adding control statement to address warning
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                    # end of Gabrielle change
                    data = df_mmv_cleaned)
summary(model_2_ri)
# Model 2 (2 predictors) with random intercepts & slopes
model_2_ris <- glmer(AIM ~ Trunk_Flexion_BR + COM_VelX_MAWtoBR + # fixed effects
                       (1 + Trunk_Flexion_BR + COM_VelX_MAWtoBR | Player), # random effects
                     family = Gamma(link = "log"), # set the distribution to gamma
                     data = df_mmv_cleaned)

# Compare models
model.comparison(model_2_ri, model_2_ris)

# Compare model_2_ri to model_1_ri
model.comparison(model_2_ri, model_1_ri)

# summarise progressing model
summary(model_1_ri)
performance::icc(model_1_ri) # compute model ICCs
flexplot::estimates(model_1_ri) # obtain ICCs and other model summaries
r2mlm(model_1_ri)


library(effects)
# Plot the effects of predictors
plot(allEffects(model_1_ri))

visualize(model_1_ri,
          plot = "model",
          formula = AIM ~ Trunk_Flexion_BR + Player,
          sample = 20)

# Generate output table for the reduced vs. final model
tab_model(baseline1,model_1_ri, show.df = TRUE,
          dv.labels = c("Reduced Model", "Final Model"),
          file = "Aim 3 - Reduced vs. Full Model Outputs.doc")


#################################################################################################
# If model_2 is better than model_1 (AIC/BIC, Bayes Factor, p value), add next predictor variable
#################################################################################################

# # Model 3 (3 predictors) with random intercepts
# model_3_ri <- glmer(AIM ~ `Trunk Flexion @ BR` + `COM Velocity (X) (MAW to BR)` + 
#                    `Trunk Flexion Velocity (MAW to BR)` + # fixed effects
#                    (1 | Player), # random effects
#                    family = Gamma(link = "log"), # set the distribution to gamma
#                    data = df_mmv_cleaned)
# 
# # Model 3 (3 predictors) with random intercepts & slopes
# model_3_ris <- glmer(AIM ~ `Trunk Flexion @ BR` + `COM Velocity (X) (MAW to BR)` + # fixed effects
#                     `Trunk Flexion Velocity (MAW to BR)` +
#                     (1 + `Trunk Flexion @ BR` + `COM Velocity (X) (MAW to BR)` + 
#                     `Trunk Flexion Velocity (MAW to BR)`| Player), # random effects
#                     family = Gamma(link = "log"), # set the distribution to gamma
#                     data = df_mmv_cleaned)
# 
# # Compare model_3 to the model_2
# model.comparison(model_3_ri, model_3_ris)
# 
# # summarise model
# summary(model_3_ri)
# 
# #################################################################################################
# # If model_3 is better than model_2 (AIC/BIC, Bayes Factor, p value), add next predictor variable
# #################################################################################################
# 
# # Re-scale variables to aid with convergence issues
# df_mmv_cleaned$`Trunk Flexion @ BR` <- scale(df_mmv_cleaned$`Trunk Flexion @ BR`)
# df_mmv_cleaned$`COM Velocity (X) (MAW to BR)` <- scale(df_mmv_cleaned$`COM Velocity (X) (MAW to BR)`)
# df_mmv_cleaned$`Trunk Flexion Velocity (MAW to BR)` <- scale(df_mmv_cleaned$`Trunk Flexion Velocity (MAW to BR)`)
# df_mmv_cleaned$`Trunk Flexion Velocity (max)` <- scale(df_mmv_cleaned$`Trunk Flexion Velocity (max)`)
# 
# # Model 4 (4 predictors) with random intercepts
# model_4_ri <- glmer(AIM ~ `Trunk Flexion @ BR` + `COM Velocity (X) (MAW to BR)` + 
#                    `Trunk Flexion Velocity (MAW to BR)` + `Trunk Flexion Velocity (max)` + # fixed effects
#                    (1 | Player), # random effects
#                    family = Gamma(link = "log"), # set the distribution to gamma
#                    data = df_mmv_cleaned)
# 
# # Model 4 (4 predictors) with random intercepts & slopes
# model_4_ris <- glmer(AIM ~ `Trunk Flexion @ BR` + `COM Velocity (X) (MAW to BR)` + # fixed effects
#                     `Trunk Flexion Velocity (MAW to BR)` + `Trunk Flexion Velocity (max)` +
#                     (1 + `Trunk Flexion @ BR` + `COM Velocity (X) (MAW to BR)` + 
#                     `Trunk Flexion Velocity (MAW to BR)` + `Trunk Flexion Velocity (max)` | Player), # random effects
#                      family = Gamma(link = "log"), # set the distribution to gamma
#                      data = df_mmv_cleaned)
# 
# model.comparison(model_4_ri, model_4_ris)
# 
# # Both models failed to converge!
# 
# 
# 
# model_full <- lmer(AIM ~ `Trunk Flexion @ BR` +  
#                      `COM Velocity (X) (MAW to BR)` + `Trunk Flexion (MAW to BR)` +
#                      `Trunk Flexion Velocity (max)` + `Trunk Flexion Velocity (BFC to FFC)` +
#                      `Trunk Rotation (Y) @ BR` + `Lateral Trunk Flexion (BFC to FFC)` +
#                      `Trunk Rotation (Y) (BR to EFT)` + `Front Knee Extension Velocity @ BR` +
#                      `Stride Length (% Height) @ FFC` + `Elbow to Thorax (X) (MAW to BR)` +
#                      `Shoulder H Abd Velocity (max)` + # fixed effects
#                      
#                      
#                      (1 + `Trunk Flexion @ BR` + `Elbow to Thorax (X) (BFC to FFC)` + 
#                         `COM Velocity (X) (MAW to BR)` + `Trunk Flexion (MAW to BR)` +
#                         `Trunk Flexion Velocity (max)` + `Trunk Flexion Velocity (BFC to FFC)` +
#                         `Trunk Rotation (Y) @ BR` + `Lateral Trunk Flexion (BFC to FFC)` +
#                         `Trunk Rotation (Y) (BR to EFT)` + `Front Knee Extension Velocity @ BR` +
#                         `Stride Length (% Height) @ FFC` + `Elbow to Thorax (X) (MAW to BR)` +
#                         `Shoulder H Abd Velocity (max)`| Player), # random effects
#                    data = df_mmv_cleaned)
# 
# 
# full_summary <- summary(model_full)
# 
# model.comparison(baseline, model_full)
