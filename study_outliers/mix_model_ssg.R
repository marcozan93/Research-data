#### MIXED MODEL SSG STUDY OUTLIERS ####
# Author: Dr. Marco Zanin

library(lme4)
library(lattice)
library(performance)
library(ggplot2)

#Load the data
df <- read.csv("./ssg.csv")
#convert date to <date>
df$date <- as.Date(df$date, "%d/%m/%Y")
#create a column to differentiate between forwards and backs
df$forward0_back1 <- ifelse(df$players <= 22, 0, 1)
#convert the column to integer
df$forward0_back1 <- as.integer(df$forward0_back1)
#check the structure of the data frame
str(df)

#Create a data frame including outliers
df_complete <- df
#Create a data frame excluding error outliers
df_no_outliers <- 
  df[-c(5,7,37,106,143,223,224,255,259,412,421,448,464,500,554,588,622,637,656),]

#The Increased SE column in the output indicates how much larger 
#the standard error is due to the correlation with other predictors.
#
#plot the output
#w <- check_collinearity(x)
#plot(w)
#
#variance inflation factor (VIF)
#car::vif(x)
#car::vif(y)

#Multicollinearity can have different reasons. Probably in many cases it will 
#help to center or standardize the predictors. Sometimes the only way to avoid 
#multicollinearity is to remove one of the predictors with a very high VIF value. 
#Collecting more data may also help, but this is of course not always possible.
#
#I could actually use AIC to identify which multicollinear variable should be
#maintained in the model and which should be excluded by comparing the two
#models with only one of the two correlated variables and check which one has a 
#lower AIC (Graham, 2003)
#
#VIF as low as 2 can have significant impacts (Graham, 2003)
#
#For a detailed description and presentation of Multicolllinearity
#check documentation for ?performance::check_collinearity
#
#Multicollinearity can be found also in "An introduction to statistical
#learning with applications in R"
#
#
#### MODEL BUILDING #### 

#### I. RANDOM EFFFECTS ####

##Start by specifying correctly the random component.
#Based on study design, date, ssg_bout, and players should all be the groups for variation
# 1) Include all predictors as fixed effects and add date as random effect
fit0 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date),
             data = df_no_outliers)
# 2) Add the player as a group for the intercept
fit1 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date) +
               (1|players),
             data = df_no_outliers)
# 3) Compare models
anova(fit0, fit1)

# 4) Is the random component of date necessary?
fit2 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|players),
             data = df_no_outliers)
anova(fit1, fit2)
# Apprently, yes! Fit1 is significantly better than fit2.

# 5) Should players be allowed to vary within each date?
fit3 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date) +
               (1|players) +
               (1|date:players),
             data = df_no_outliers)
anova(fit1, fit3)
# Yes! Fit3 is significantly better than fit1

# 6) Should the intercept vary for each player and for each player within date? 
fit4 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date/players),
             data = df_no_outliers)
anova(fit3, fit4)
#Yes! Fit3 is significantly better than fit4.

# 7) Should the intercept be allowed to vary based on the ssg_bout as well?
fit5 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout),
             data = df_no_outliers)
anova(fit3, fit5)
#Yes! Fit5 is significantly better than fit3.

# 8) Should the intercept be allowed to vary based on ssg_bout within date as well?
fit6 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout) +
               (1|date:ssg_bout),
             data = df_no_outliers)
anova(fit5, fit6)
#Yes! Fit6 is significantly better than fit5.

# 9) Should the intercept be allowed to vary for players within ssg_bout?
fit7 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout) +
               (1|date:ssg_bout) +
               (1|ssg_bout:players),
             data = df_no_outliers)
anova(fit6, fit7)
summary(rePCA(fit7))
#NO! Fit6 seems better based on AIC (7586.4 v 7588.4)
#Furthermore, fit7 is singular with (1|ssg_bout|players) being
#nearly zero, thus showing that this component is not necessary.

#It results that the intercept should vary across:
# I. date of data collection,
# II. ssg_bout performed,
# III. players involved,
# IV. players within each date of data collection,
# V. ssg_bout performed within each date of data collection,
#
#This would actually represent the study design where each player is 
#involved in one ssg_bout in a certain date, and repeated measures are available.


#### II. FIXED EFFECTS ####

#Check for collinearity of predictors
fit6 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               forward0_back1 +
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout) +
               (1|date:ssg_bout),
             data = df_no_outliers)
isSingular(fit6) #FALSE
performance::check_collinearity(fit6)
#Multicollinearity is quiete high for:
# total_distance_m = 6.06 VIF
# total_player_load = 6.37 VIF
#MULTICOLLINEARITY DETECTED!
#
#Create two models: one with only total_player_load and with total_distance_m
#and compare using AIC.
fit6.1 <- lmer(stagnos_trimp ~ total_distance_m + 
                 tot_hsr_distance + 
                 acceleration_density + 
                 #total_player_load + 
                 player_load_slow + 
                 get_up + 
                 bullet +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
isSingular(fit6.1) #FALSE
summary(fit6.1)
performance::check_collinearity(fit6.1)
#Multicollinearity is no longer an issue as all VIF are < 2.
anova(fit6, fit6.1)
#However, after removing total_player_load, 
#model fit6 seems more appropriate than fit6.1 based on AIC (AIC = 7586.4 v 7588,
# Chisq = 3.631, df = 1, Pr(>Chisq) = 0.05722)
#
#Hence it was decided to try to remove total_distance_m and compare the models
#
#Remove total_distance_m
fit6.2 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 player_load_slow + 
                 get_up + 
                 bullet +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
isSingular(fit6.2) #FALSE
anova(fit6, fit6.2)
#fit6.2 is slightly better than fit6 with AIC = 7585.4 v 7586.4, Chisq=1.0686, df=1, Pr(>Chisq)=0.3013
anova(fit6.1, fit6.2)
#fit6.2 is also a bit better than 6.1 (AIC = 7585.4 v 7588.0)
#
#Therefore, I remove total_distance_m from the model and keep fit6.2 as the
#fit that removes multicollinearity.
#Keep removing variable with t-statistic < 2.
fit6.3 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 #tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 player_load_slow + 
                 get_up + 
                 bullet +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
isSingular(fit6.3) #FALSE
anova(fit6.2, fit6.3)
#fit6.3 is better based on AIC (AIC = 7584.4 v 7585.4)
summary(fit6.3)

#Keep removing variables with t-statistic < 2.
fit6.4 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 #tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 player_load_slow + 
                 get_up + 
                 #bullet +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
isSingular(fit6.4) #FALSE
anova(fit6.3, fit6.4)
#fit6.4 is slightly better (AIC = 7582.4 v 7584.4)
summary(fit6.4)

#Keep removing variables with t-statistic < 2.
fit6.5 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 #tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 player_load_slow + 
                 #get_up + 
                 #bullet +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
anova(fit6.4, fit6.5) 
#AIC = 7582.4 v 7581.3, Chisq=0.9249, df=1, Pr(>Chisq)=0.3362
summary(fit6.5)

fit6.6 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 #tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 #player_load_slow + 
                 #get_up + 
                 #bullet +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
anova(fit6.5, fit6.6) 
#AIC = 7581.3 v 7580.4, Chisq=1.0374, df=1, Pr(>Chisq)=0.3084
summary(fit6.6)

####CONCLUSIONS ####
#
# model fit6.6 can be considered as my final model.
#heart rate is affected by:
# I. being a forward or a back,
# II. acceleration density,
# III. player load.
#
#In this data frame, multicollinearity was resolved practicality of measurements
final_fit <- lmer(stagnos_trimp ~ 
                 acceleration_density + 
                 total_player_load +
                 forward0_back1 +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
#profile likelihood confidence intervals at 95% level
confint.merMod(final_fit, level = 0.95, method = "profile", oldNames = FALSE)
#plot regularity of log-likelihood
pp <- profile(final_fit, signames = FALSE)
xyplot(pp, main = "Regularity log-likelihood SSG")

#Model including outliers
final_fit_out <- lmer(stagnos_trimp ~ 
                    acceleration_density + 
                    total_player_load +
                    forward0_back1 +
                    (1|date) +
                    (1|players) +
                    (1|date:players) +
                    (1|ssg_bout) +
                    (1|date:ssg_bout),
                  data = df_complete)
summary(final_fit_out)
confint.merMod(final_fit_out, level = 0.95, method = "profile", oldNames = FALSE)


##########################################################################################


library(ggplot2)
library(ggeffects)
library(patchwork)

# Extract the prediction data frame
pred_pl <- ggpredict(final_fit, terms = c("total_player_load"))  # this gives overall predictions for the model
# Plot the predictions 
plot_pl <- ggplot(pred_pl) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "PlayerLoad (AU)", y = "TRIMP (AU)") + 
  theme_minimal()


pred_acc <- ggpredict(final_fit, terms = c("acceleration_density"))  # this gives overall predictions for the model
plot_acc <- ggplot(pred_acc) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "Avg acc-dec (m*s^2)", y = "") + 
  theme_minimal()


pred_ps <- ggpredict(final_fit, terms = c("forward0_back1"))  # this gives overall predictions for the model
plot_ps <- ggplot(pred_ps) + 
  geom_point(aes(x = as.factor(x), y = predicted), size=3) +          # slope
  geom_errorbar(aes(x=as.factor(x), ymin=conf.low, ymax=conf.high), width=.05) +
  labs(x = "Forwards=0, Backs=1", y = "") + 
  theme_minimal()


#This is the final figure that I could export in .PNG

PLOT <- (plot_pl + plot_acc + plot_ps) + plot_annotation(title = "SSG-BF",
                                              tag_levels = "A",
                                              tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))
ggsave(filename='.study_ssg_bf.tiff', plot = PLOT, dpi=600, 
       device='tiff')#, width=700, height=300, units='px')
