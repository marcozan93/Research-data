#### MIXED MODEL vSSG BACKS STUDY2b ####

library(lme4)
library(lattice)
library(performance)
library(ggplot2)

#Load the data
df <- read.csv("./forwards_vssg.csv", sep=",")
#convert date to <date>
df$date <- as.Date(df$date, "%d/%m/%Y")
#check the structure of the data frame
str(df)

#Create a data frame including outliers
df_complete <- df
#Create a data frame excluding error outliers
df_no_outliers <- df[-c(180,239,266,300,320,340,503),]
#df_no_outliers <- df[-c(180,239,300,340,503),] #removing 266 and 320 leads to t>2 final model

#criteria for solving multicollinearity:
# 1) Specify full model, determine VIF, remove variable with highest VIF.
# 2) Specify two models, including and excluding the two variables with highest 
#    VIFs and compare the two models using AIC (Graham, 2003)
#
# Not sure which option I should use!


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
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout) +
               (1|date:ssg_bout) +
               (1|ssg_bout:players),
             data = df_no_outliers)
isSingular(fit7) #TRUE
rePCA(fit7)
anova(fit6, fit7)
#NO! Fit6 seems better based on AIC (6015.1 v 6017.0).
#Furthermore, fit7 is singular.

fit8 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               (1|date) +
               (1|players) +
               (1|date:players) +
               #(1|ssg_bout) +     #this is removed as fit7 showed ssg_bout=0
               (1|date:ssg_bout) +
               (1|ssg_bout:players),
             data = df_no_outliers)
anova(fit6, fit8)
#These are basically equivalent with AIC of fit6=6015.1 and fit8=6015.0
#
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
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout) +
               (1|date:ssg_bout),
             data = df_no_outliers)
isSingular(fit6) #FALSE
performance::check_collinearity(fit6)
#Multicollinearity is quiete high for:
# total_distance_m = 2.55 VIF
# total_player_load = 2.91 VIF

#Multicollinearity was handled by removing the variable
# "total_player_load". This because total_player_load and
#total_distance_m may share a lot of information and for
#practitioners, it would be easier to interpret the variable
#total_distance_m. Furthermore, this metric is more reliable 
#than total_player_load as the latter one is affected by the
#the total amount of running and by displacements in the vertical
#axis. Conversely, total distance has resukted to be valid and reliable
#and not affected by software updates in the global positioning systems.

fit6.1 <- lmer(stagnos_trimp ~ total_distance_m + 
               tot_hsr_distance + 
               acceleration_density + 
               #total_player_load + 
               player_load_slow + 
               get_up + 
               bullet +
               (1|date) +
               (1|players) +
               (1|date:players) +
               (1|ssg_bout) +
               (1|date:ssg_bout),
             data = df_no_outliers)
isSingular(fit6.1) #TRUE
summary(fit6.1)

#By removing one of the variables with high VIF, I get a singular fit.
##Singularity may indicate that the model is overfitted as some of 
#the random components may be close to or exactly zero (Bates et al., 2015).
#
#To further investigate where this issue occurs,
#I can then have a look at the random effects from the model with
ranef(fit6.1)
#and from the estimated standard deviations between the random effects terms
VarCorr(fit6.1)
#
#In addition, lme4 provides a function to performed a PCA on the 
#random effects covariance matrix (i.e., rePCA())
#which provides more detail about the singularity pattern, showing the
#standard deviations of orthogonal variance components and the mapping from
#variance terms in the model to orthogonal components 
#(i.e., eigenvector/rotation matrices) (Bates et al., 2015).
re_pca <- rePCA(fit6.1)
summary(re_pca)
#From this information, it is evident that the random effect values for 
#ssg_bout are extremely close to zero and substantially different from
#the other random components. Consequently, this highlights how the
#overfitting of the random effects is produced by the ssg_bout component 
#(Bates et al., 2015). Following recommendations from Bates et al. (2015), the
#random component of ssg_bout is removed from the model.
fit6.1.1 <- lmer(stagnos_trimp ~ total_distance_m + 
                 tot_hsr_distance + 
                 acceleration_density + 
                 #total_player_load + 
                 player_load_slow + 
                 get_up + 
                 bullet +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 #(1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
#The model is now NOT singular
isSingular(fit6.1.1) #FALSE
#
#In addition, when assessing collinearity issues
performance::check_collinearity(fit6.1)
#Multicollinearity is no longer an issue as all VIF are < 2.
#
#Successively a model comparison with the full model is performed
anova(fit6, fit6.1.1)
#which shows that fit6.1.1 is more appropriate (AIC = 6011.9 v 6015.1)
#
#
#Going back to the multicollinearity issue, I now remove total_distance_m
#from the model fit6
#removing total_distance_m instead of total_player_load
fit6.2 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 player_load_slow + 
                 get_up + 
                 bullet +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 (1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
isSingular(fit6.2) #TRUE
#However, as before, this results in a singularity issue.
#Assessment was again performed using random effects, and PCA of random effects
#correlation matrix.
ranef(fit6.2)
#which shows zero values for ssg_bout
#and
summary(rePCA(fit6.2))
#which again shows how the standard deviaiton for ssg_bout is zero.
#Consequently, this shows how the model is overfitted in the random component,
#and ssg_bout is removed (Bates et al., 2015).
fit6.2.1 <- lmer(stagnos_trimp ~ #total_distance_m + 
                 tot_hsr_distance + 
                 acceleration_density + 
                 total_player_load + 
                 player_load_slow + 
                 get_up + 
                 bullet +
                 (1|date) +
                 (1|players) +
                 (1|date:players) +
                 #(1|ssg_bout) +
                 (1|date:ssg_bout),
               data = df_no_outliers)
#which does not lead to singularity issues
isSingular(fit6.2.1) #FALSE
#
#Comparison of the full model with the reduced model which removes
#collinearity and singularity issues shows that
anova(fit6, fit6.2.1)
#fit6.2.1 is a better model (AIC = 6011.7 v 6015.1)
#
#
#Ultimately, to further investigate multicollinearity and decide which
#variable should be maintained and which should be removed from the
#model, the models without total_distance_m and the model without
#total_player_load were compared:
anova(fit6.1.1, fit6.2.1)
#This showed that the model with total_player_load was a slightly better model
#(AIC: fit6.2.1 = 6011.7 v fit6.1.1 = 6011.9)
#
#Due to simlarity in AIC (difference of 0.2), and the fact that
#total distance is a construct more easily interpretable by
#practitioners, player load was removed from the model.
#
#
#From this model I begin to deleat variables that do not contribute to the model
fit6.1.2 <- lmer(stagnos_trimp ~ total_distance_m + 
                   #tot_hsr_distance + 
                   acceleration_density + 
                   #total_player_load + 
                   player_load_slow + 
                   get_up + 
                   bullet +
                   (1|date) +
                   (1|players) +
                   (1|date:players) +
                   #(1|ssg_bout) +
                   (1|date:ssg_bout),
                 data = df_no_outliers)
anova(fit6.1.1, fit6.1.2)
#AIC   6011.9    6010.0
summary(fit6.1.2)
#
fit6.1.3 <- lmer(stagnos_trimp ~ total_distance_m + 
                   #tot_hsr_distance + 
                   acceleration_density + 
                   #total_player_load + 
                   player_load_slow + 
                   get_up + 
                   #bullet +
                   (1|date) +
                   (1|players) +
                   (1|date:players) +
                   #(1|ssg_bout) +
                   (1|date:ssg_bout),
                 data = df_no_outliers)
anova(fit6.1.2, fit6.1.3)
#AIC    6010.0    6008.9
#
summary(fit6.1.3)
#
fit6.1.4 <- lmer(stagnos_trimp ~ total_distance_m + 
                   #tot_hsr_distance + 
                   acceleration_density + 
                   #total_player_load + 
                   #player_load_slow + 
                   get_up + 
                   #bullet +
                   (1|date) +
                   (1|players) +
                   (1|date:players) +
                   #(1|ssg_bout) +
                   (1|date:ssg_bout),
                 data = df_no_outliers)
anova(fit6.1.3, fit6.1.4)
#AIC    6008.9    6008.4
#
summary(fit6.1.4)
#
#Trying to further remove variables with a t-value lower than 2
#total_distance_m = 1.882
#get_up = 1.834
fit6.1.5 <- lmer(stagnos_trimp ~ total_distance_m + 
                   #tot_hsr_distance + 
                   acceleration_density + 
                   #total_player_load + 
                   #player_load_slow + 
                   #get_up + 
                   #bullet +
                   (1|date) +
                   (1|players) +
                   (1|date:players) +
                   #(1|ssg_bout) +
                   (1|date:ssg_bout),
                 data = df_no_outliers)
anova(fit6.1.4, fit6.1.5)
#AIC    6008.4    6009.8  Chsq=3.39, df=1, Pr(>Chsq)=0.06571
#
fit6.1.6 <- lmer(stagnos_trimp ~ #total_distance_m + 
                   #tot_hsr_distance + 
                   acceleration_density + 
                   #total_player_load + 
                   #player_load_slow + 
                   get_up + 
                   #bullet +
                   (1|date) +
                   (1|players) +
                   (1|date:players) +
                   #(1|ssg_bout) +
                   (1|date:ssg_bout),
                 data = df_no_outliers)
anova(fit6.1.4, fit6.1.6)
#AIC    6008.4    6009.8  Chisq=3.41, df=1, Pr(>Chisq)=0.06459
fit6.1.7 <- lmer(stagnos_trimp ~ #total_distance_m + 
                   #tot_hsr_distance + 
                   acceleration_density + 
                   #total_player_load + 
                   #player_load_slow + 
                   #get_up + 
                   #bullet +
                   (1|date) +
                   (1|players) +
                   (1|date:players) +
                   #(1|ssg_bout) +
                   (1|date:ssg_bout),
                 data = df_no_outliers)
anova(fit6.1.4, fit6.1.7)
#AIC   6008.4     6014.1

#Consequently, by removing further variables the model does not improve.
#Model fit6.1.4 would be my final fit.
#
#
#Therefore, the final model seems to be fit6.1.4
final_fit <- lmer(stagnos_trimp ~ total_distance_m + 
                                acceleration_density + 
                                get_up + 
                                (1|date) +
                                (1|players) +
                                (1|date:players) +
                                (1|date:ssg_bout),
                              data = df_no_outliers)
summary(final_fit)
#Profile likelihood confidence intervals at 95% level
confint.merMod(final_fit, level = 0.95, method = "profile", oldNames = FALSE)
#Plot regularity of log-likelihood
pp <- profile(final_fit, signames = FALSE)
xyplot(pp, main = "Regularity of log-likelihood function vSSG forwards")


####CONCLUSIONS ####
#
#The internal load is affected by
# total distance covered
# acceleration density
# get up

### ANALYSIS INCLUDING OUTLIERS
final_fit_outliers <- lmer(stagnos_trimp ~ total_distance_m + 
                    acceleration_density + 
                    get_up + 
                    (1|date) +
                    (1|players) +
                    (1|date:players) +
                    (1|date:ssg_bout),
                  data = df_complete)
summary(final_fit_outliers)
#Profile likelihood confidence intervals at 95% level
confint.merMod(final_fit_outliers, level = 0.95, method = "profile", oldNames = FALSE)


#==================================================

#### visualisation ####

## Forest plot

library(sjPlot)

sjPlot::plot_model(final_fit, type='std',#type='est',
                   #axis.labels=c("Get-up", "Avg acc dec", "Total distance"),
                   show.values=TRUE, show.p=FALSE, shoe.intercept=TRUE,
                   #show.data=TRUE,jitter=0.2,
                   title="Fixed effects for the model of SSG-F")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(equatiomatic)
library(tidyverse)

extract_eq(final_fit)

#Manipulate the data to create a nice table with the info I need
#MLE, SE, t statistic, PLCI95%
summary_td <- summary(final_fit)
ci_td <- as.data.frame(confint.merMod(final_fit, level = 0.95, method = "profile", oldNames = FALSE))
ci_td["Term"] <- rownames(ci_td)
coef_td <- as.data.frame(summary_td$coefficients)
coef_td["Term"] <- rownames(coef_td)
td_results <- merge(coef_td, ci_td, by = "Term")
names(td_results)[names(td_results) == "Estimate"] <- "MLE"
names(td_results)[names(td_results) == "Std. Error"] <- "SE"
names(td_results)[names(td_results) == "t value"] <- "t"
#td_results <- td_results[,-5]
td_results[,-1] <- round(td_results[,-1], 2)

td_results$`95%PLCI` <- str_c("[", td_results$`2.5 %`, ", ", td_results$`97.5 %`, "]")
td_results <- subset(td_results, select = -c(`2.5 %`, `97.5 %`))
td_results

# Regression plot

library(ggeffects)

# Extract the prediction data frame
pred_td <- ggpredict(final_fit, terms = c("total_distance_m"))  # this gives overall predictions for the model
# Plot the predictions 
plot_td <- ggplot(pred_td) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    #geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error),
    geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_point(data = df_no_outliers,                      # adding the raw data (scaled values)
               aes(x = total_distance_m, y = stagnos_trimp))+#, colour = mountainRange)) + 
    geom_text(x=180, y=220, label=expression(paste(italic("y")," = 371.42 + 0.35 ", italic("X"))),
              size=3.5)+  
  labs(x = "Total distance (m)", y = "TRIMP (AU)") + 
    theme_minimal()


pred_acc <- ggpredict(final_fit, terms = c("acceleration_density"))  # this gives overall predictions for the model
plot_acc <- ggplot(pred_acc) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = df_no_outliers,                      # adding the raw data (scaled values)
             aes(x = acceleration_density, y = stagnos_trimp))+#, colour = mountainRange)) + 
  geom_text(x=0.55, y=220, label=expression(paste(italic("y")," = 371.42 + 163.17 ", italic("X"))),
            size=3.5)+  
  labs(x = "Avg acc-dec (m*s^2)", y = "") + 
  theme_minimal()


pred_gu <- ggpredict(final_fit, terms = c("get_up"))  # this gives overall predictions for the model
plot_gu <- ggplot(pred_gu) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  geom_point(data = df_no_outliers,                      # adding the raw data (scaled values)
             aes(x = get_up, y = stagnos_trimp))+#, colour = mountainRange)) + 
  geom_text(x=1.7, y=220, label=expression(paste(italic("y")," = 371.42 + 4.38 ", italic("X"))),
            size=3.5)+  
  labs(x = "Get-up (count)", y = "") + 
  theme_minimal()


library(patchwork)

#This is the final figure that I could export in .PNG
(plot_td + plot_acc + plot_gu) + plot_annotation(title = "Model summary",
                                         tag_levels = "A",
                                         tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))


#####################################################################################


# Extract the prediction data frame
pred_td <- ggpredict(final_fit, terms = c("total_distance_m"))  # this gives overall predictions for the model
# Plot the predictions 
plot_td <- ggplot(pred_td) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "Total distance (m)", y = "TRIMP (AU)") + 
  theme_minimal()


pred_acc <- ggpredict(final_fit, terms = c("acceleration_density"))  # this gives overall predictions for the model
plot_acc <- ggplot(pred_acc) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "Avg acc-dec (m*s^2)", y = "") + 
  theme_minimal()


pred_gu <- ggpredict(final_fit, terms = c("get_up"))  # this gives overall predictions for the model
plot_gu <- ggplot(pred_gu) + 
  geom_line(aes(x = x, y = predicted)) +          # slope
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "lightgrey", alpha = 0.5) +  # error band
  labs(x = "Get-up (count)", y = "") + 
  theme_minimal()


library(patchwork)

#This is the final figure that I could export in .PNG

PLOT <- (plot_td + plot_acc + plot_gu) + plot_annotation(title = "SSG-F",
                                                 tag_levels = "A",
                                                 tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))
ggsave(filename='./study_ssg_f.tiff', plot = PLOT, dpi=600, 
       device='tiff')#, width=700, height=300, units='px')
