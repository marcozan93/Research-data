#### MIXED MODEL vSSG BACKS STUDY OUTLIERS ####
# Author: Dr. Marco Zanin

library(lme4)
library(lattice)
library(performance)
library(ggplot2)

#Load the data
df <- read.csv("./backs_vssg.csv", sep=",")
#convert date to <date>
df$date <- as.Date(df$date, "%d/%m/%Y")
#check the structure of the data frame
str(df)

#Create a data frame including outliers
df_complete <- df
#Create a data frame excluding error outliers
df_no_outliers <- df[-c(98, 231, 249, 41, 265, 305, 431, 29, 179, 17, 
        5, 445, 158, 322, 356, 175, 373, 445),]


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
                     (1|date),
             data = df_no_outliers)
# 2) Add the player as a group for the intercept
fit1 <- lmer(stagnos_trimp ~ total_distance_m + 
                     tot_hsr_distance + 
                     acceleration_density + 
                     total_player_load + 
                     player_load_slow + 
                     get_up + 
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
                     (1|date) +
                     (1|players) +
                     (1|date:players) +
                     (1|ssg_bout) +
                     (1|date:ssg_bout) +
                     (1|ssg_bout:players),
             data = df_no_outliers)
anova(fit6, fit7)
#Yes! Fit7 is significantly better than fit6 (AIC = 4987.1 v 4995.6)

#It results that the intercept should vary across:
# I. date of data collection,
# II. ssg_bout performed,
# III. players involved,
# IV. players within each date of data collection,
# V. ssg_bout performed within each date of data collection,
# VI. players within each ssg_bout.
#
#This would actually represent the study design where each player is 
#involved in one ssg_bout in a certain date, and repeated measures are available.


#### II. FIXED EFFECTS ####

#Check for collinearity of predictors
fit7 <- lmer(stagnos_trimp ~ total_distance_m + 
                     tot_hsr_distance + 
                     acceleration_density + 
                     total_player_load + 
                     player_load_slow + 
                     get_up + 
                     (1|date) +
                     (1|players) +
                     (1|date:players) +
                     (1|ssg_bout) +
                     (1|date:ssg_bout) +
                     (1|ssg_bout:players),
             data = df_no_outliers)
isSingular(fit7) #FALSE
performance::check_collinearity(fit7)
#Multicollinearity is quiete high for:
# total_distance_m = 4.62 VIF
# total_player_load = 4.64 VIF

#Multicollinearity was handled by removing the variable
# "total_player_load". This because total_player_load and
#total_distance_m may share a lot of information and for
#practitioners, it would be easier to interpret the variable
#total_distance_m. Furthermore, this metric is more reliable 
#than total_player_load as the latter one is affected by the
#the total amount of running and by displacements in the vertical
#axis. Conversely, total distance has resukted to be valid and reliable
#and not affected by software updates in the global positioning systems.


fit7.1 <- lmer(stagnos_trimp ~ total_distance_m + 
                       tot_hsr_distance + 
                       acceleration_density + 
                       #total_player_load + 
                       player_load_slow + 
                       get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       (1|ssg_bout) +
                       (1|date:ssg_bout) +
                       (1|ssg_bout:players),
               data = df_no_outliers)
isSingular(fit7.1) #FALSE
summary(fit7.1)
performance::check_collinearity(fit7.1)
#Multicollinearity is no longer an issue as all VIF are < 2.
anova(fit7, fit7.1)
#fit7.1 seems more appropriate (AIC = 4986.6 v 4987.1)

#Compare AICs for models with total_distance_m only and total_player_load only
fit7.1.x <- lmer(stagnos_trimp ~ #total_distance_m + 
                       tot_hsr_distance + 
                       acceleration_density + 
                       total_player_load + 
                       player_load_slow + 
                       get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       (1|ssg_bout) +
                       (1|date:ssg_bout) +
                       (1|ssg_bout:players),
               data = df_no_outliers)
anova(fit7, fit7.1.x)
anova(fit7.1, fit7.1.x) #AIC: fit7.1 = 4986.6, fit7.1.x = 4986.6
#The results show that the models with only total_distance_m and with
#only total_player_load are equivalent.
#Hence, for semplicity of interpretation, the model with 
#total_distance_m was used and total_player_load was removed from the model.

#Start to remove variable with very low t-statistic
fit7.2 <- lmer(stagnos_trimp ~ total_distance_m + 
                       tot_hsr_distance + 
                       #acceleration_density + 
                       #total_player_load + 
                       player_load_slow + 
                       get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       (1|ssg_bout) +
                       (1|date:ssg_bout) +
                       (1|ssg_bout:players),
               data = df_no_outliers)
isSingular(fit7.2) #FALSE
anova(fit7.1, fit7.2)
#fit7.2 is a bit better than 7.1 (AIC = 4985.1 v 4986.6)
summary(fit7.2)

#Keep removing variable with t-statistic < 2.
fit7.3 <- lmer(stagnos_trimp ~ total_distance_m + 
                       #tot_hsr_distance + 
                       #acceleration_density + 
                       #total_player_load + 
                       player_load_slow + 
                       get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       (1|ssg_bout) +
                       (1|date:ssg_bout) +
                       (1|ssg_bout:players),
               data = df_no_outliers)
isSingular(fit7.3) #FALSE
anova(fit7.2, fit7.3)
#fit7.3 is better based on AIC (AIC = 4984.1 v 4985.1)
summary(fit7.3)

#Keep removing variables with t-statistic < 2.
fit7.4 <- lmer(stagnos_trimp ~ total_distance_m + 
                       #tot_hsr_distance + 
                       #acceleration_density + 
                       #total_player_load + 
                       #player_load_slow + 
                       get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       (1|ssg_bout) +
                       (1|date:ssg_bout) +
                       (1|ssg_bout:players),
               data = df_no_outliers)
isSingular(fit7.4) #FALSE
anova(fit7.3, fit7.4)
#fit6.4 is slightly better (AIC = 4983.9 v 4984.1)
summary(fit7.4)

#In the vSSG-forwards model, at this point I had to solve a singularity issue.
#To do so, I have removed the random component of the intercept which was
#allowed to vary based on the ssg_bout performed.
#
#I could try to to do this even on this model and see the result.
fit7.4.1 <- lmer(stagnos_trimp ~ total_distance_m + 
                       #tot_hsr_distance + 
                       #acceleration_density + 
                       #total_player_load + 
                       #player_load_slow + 
                       get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       #(1|ssg_bout) +
                       (1|date:ssg_bout),
               data = df_no_outliers)
anova(fit7.4, fit7.4.1)
#However, in this scenario, fit7.4 which includes (1|ssg_bout) is
#significantly better than fit7.4.1 without that component.
# AIC: 4983.9 v 4993.2, Chisq = 13.281, df = 2, p = 0.001306
#
#Therefore, the random component of (1|ssg_bout) was retained in the model
#for vSSG-backs.

####FINAL MODEL ####
final_fit <- lmer(stagnos_trimp ~ 
                          total_distance_m + 
                          get_up + 
                       (1|date) +
                       (1|players) +
                       (1|date:players) +
                       (1|ssg_bout) +
                       (1|date:ssg_bout) +
                       (1|ssg_bout:players),
               data = df_no_outliers)
#Profile likelihood confidence itnervals at 95% level
confint.merMod(final_fit, level = 0.95, method = "profile", oldNames = FALSE)
#Profile log-likelihood
pp <- profile(final_fit, signames = FALSE)
#Plot zeta-function to assess regularity of log-likelihood function
xyplot(pp, main = "Regularity log-likelihood function for vSSG backs")


#### CONCLUSIONS ####
#
#The final model for vSSG-backs shows that heart rate is affected by 
# I. total distance covered,
# II. number of get_up performed


#### Final model including outliers ####
final_fit_outliers <- lmer(stagnos_trimp ~ 
                          total_distance_m + 
                          get_up + 
                          (1|date) +
                          (1|players) +
                          (1|date:players) +
                          (1|ssg_bout) +
                          (1|date:ssg_bout) +
                          (1|ssg_bout:players),
                  data = df_complete)
isSingular(final_fit_outliers) #TRUE
summary(final_fit_outliers) 
confint.merMod(final_fit_outliers, level = 0.95, method = "profile", oldNames = FALSE)
#Including outliers leads to greater standard errors but similar results.
#Hence conclusions can be made over the significance of total distance and
#get_up for the vSSG-backs.

################################################################

library(ggplot2)
library(ggeffects)
library(patchwork)

# Extract the prediction data frame
pred_td <- ggpredict(final_fit, terms = c("total_distance_m"))  # this gives overall predictions for the model
# Plot the predictions 
plot_td <- ggplot(pred_td) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "Total distance (m)", y = "TRIMP (AU)") + 
  theme_minimal()


pred_gu <- ggpredict(final_fit, terms = c("get_up"))  # this gives overall predictions for the model
plot_gu <- ggplot(pred_gu) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "Get-up (count)", y = "") + 
  theme_minimal()


#This is the final figure that I could export in .PNG

PLOT <- (plot_td + plot_gu) + plot_annotation(title = "SSG-B",
                                                         tag_levels = "A",
                                                         tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))
ggsave(filename='.study_ssg_b.tiff', plot = PLOT, dpi=600, 
       device='tiff')#, width=700, height=300, units='px')
