#### Imports ####

# data manipulation
library(dplyr)

# modelling
library(lme4)
library(emmeans)
library(performance)
library(parameters)
library(olsrr)

# plotting
library(ggeffects)
library(ggplot2)
library(patchwork)
library(lattice)
library(ggrepel)
library(sjPlot)




#### Loading data ####
data <- read.csv("./data.csv") # insert path of your data




#### Data preparation ####

data$Time <- ifelse(data$time == 0, "Pre", "Post")
data$Time <- as.factor(data$Time)
data$Time <- factor(data$Time, levels = c("Pre", "Post"))
data$Group <- ifelse(data$group == 0, "CON", "OWL")
data$Group <- as.factor(data$Group)
## Mean centering height and weight
mean_height <- mean(data$height)
mean_weight <- mean(data$weight)
# Subtract the mean from each value in the height column to center the mean at 0
data$height_scaled <- data$height - mean_height
data$weight_scaled <- data$weight - mean_weight
data$time <- as.factor(data$time)
data$group <- as.factor(data$group)
data$id <- as.factor(data$id)


##################### Visualisations / Outliers ################################

# Colors for groups: CON = control group, OWL = Olympic weightlifting group
CON_COLOR = "#2596be"
OWL_COLOR = "#e28743"


#### Outliers: Univariate techniques ####

### (1) BOX PLOTS

# Calculate the IQR for each group and time
# (Tukey, 1977)
TUKEY_POTENTIAL_OUTLIERS <- 1.5
TUKEY_FAR_OUT_VALUES <- 3 

iqr_values <- data %>%
  group_by(Time, Group) %>%
  summarize(
    Lower_Whisker_cmj = quantile(cmj, 0.25) - TUKEY_FAR_OUT_VALUES * IQR(cmj),
    Upper_Whisker_cmj = quantile(cmj, 0.75) + TUKEY_FAR_OUT_VALUES * IQR(cmj),
    Lower_Whisker_10m = quantile(X10m, 0.25) - TUKEY_FAR_OUT_VALUES * IQR(X10m),
    Upper_Whisker_10m = quantile(X10m, 0.75) + TUKEY_FAR_OUT_VALUES * IQR(X10m),
    Lower_Whisker_30m = quantile(X30m, 0.25) - TUKEY_FAR_OUT_VALUES * IQR(X30m),
    Upper_Whisker_30m = quantile(X30m, 0.75) + TUKEY_FAR_OUT_VALUES * IQR(X30m),
    Lower_Whisker_5rm = quantile(X5rm, 0.25) - TUKEY_FAR_OUT_VALUES * IQR(X5rm),
    Upper_Whisker_5rm = quantile(X5rm, 0.75) + TUKEY_FAR_OUT_VALUES * IQR(X5rm)
  )
# Join the original data with the IQR values to identify outliers
data_with_outliers <- data %>%
  left_join(iqr_values, by = c("Time", "Group")) %>%
  mutate(Outlier_cmj = ifelse(cmj < Lower_Whisker_cmj | cmj > Upper_Whisker_cmj, as.character(row_number()), NA),
         Outlier_10m = ifelse(X10m < Lower_Whisker_10m | X10m > Upper_Whisker_10m, as.character(row_number()), NA),
         Outlier_30m = ifelse(X30m < Lower_Whisker_30m | X30m > Upper_Whisker_30m, as.character(row_number()), NA),
         Outlier_5rm = ifelse(X5rm < Lower_Whisker_5rm | X5rm > Upper_Whisker_5rm, as.character(row_number()), NA))


# box plot with far out values function
create_custom_boxplot <- function(data, column_name, y_axis_label, outlier_col,
                                  CON_COLOR = "#2596be", OWL_COLOR = "#e28743",
                                  fill_=FALSE, legend_position_=NULL) {
  # Ensure the column name is a character string and exists in the dataframe
  if (!is.character(column_name) || !column_name %in% names(data)) {
    stop("The column name must be a character string and present in the dataframe.")
  }
  
  # Filtering data
  y_var <- data[[column_name]]
  outliers_ <- data[[outlier_col]]
  
  # Creating the plot
  p <- ggplot(data, aes(x = Time, y = y_var, fill = Group)) +
    geom_boxplot(alpha = 0.5) +
    geom_point(position = position_dodge(width = 0.75), aes(color = Group), 
               size = 2, shape = 21, alpha = 0.5) + 
    scale_fill_manual(values = c(CON_COLOR, OWL_COLOR)) +
    scale_color_manual(values = c(CON_COLOR, OWL_COLOR)) +
    labs(y = y_axis_label, x = "Time", fill = "Group:") +
    theme_minimal() +
    theme(legend.position = legend_position_) +
    guides(fill = fill_, color=FALSE) +
    geom_text(aes(label = outliers_),color="red",position = position_dodge(width = 0.75), 
              hjust = -0.3, vjust = 0, na.rm = TRUE)
  
  return(p)
}

bxp_cmj <- create_custom_boxplot(data_with_outliers, "cmj", "CMJ height (cm)", 
                                 "Outlier_cmj")

bxp_10m <- create_custom_boxplot(data_with_outliers, "X10m", "10m sprint time (s)", 
                                 "Outlier_10m")

bxp_30m <- create_custom_boxplot(data_with_outliers, "X30m", "30m sprint time (s)", 
                                 "Outlier_30m", fill_=NULL, legend_position="bottom")

bxp_5rm <- create_custom_boxplot(data_with_outliers, "X5rm", "30m sprint time (s)", 
                                 "Outlier_5rm")

# Descriptive data figure
(bxp_cmj + bxp_5rm) / (bxp_10m + bxp_30m)  + 
  plot_annotation(title = "",
                  tag_levels = "A",
                  tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))


### (2) 3*SD

plot_outliers_sd <- function(data, column_name, y_axis_label = "Value") {
  # Ensure the column name is a character string and exists in the dataframe
  if (!is.character(column_name) || !column_name %in% names(data)) {
    stop("The column name must be a character string and present in the dataframe.")
  }
  
  # Calculate the mean and standard deviation
  column_data <- data[[column_name]]
  column_mean <- mean(column_data, na.rm = TRUE)
  column_sd <- sd(column_data, na.rm = TRUE)
  
  # Create a data frame for the ggplot
  plot_data <- data.frame(value = column_data, observation_id = seq_along(column_data))
  
  # Identify outliers
  outliers <- with(data, data[column_data > column_mean + 3 * column_sd | column_data < column_mean - 3 * column_sd, ])
  outliers_data <- data.frame(value = outliers[[column_name]], observation_id = as.numeric(row.names(outliers)))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = observation_id, y = value)) +
    geom_point() +
    geom_hline(yintercept = column_mean + 3 * column_sd, linetype = 2, linewidth = 0.7) +
    geom_hline(yintercept = column_mean - 3 * column_sd, linetype = 2, linewidth = 0.7) +
    geom_text_repel(data = outliers_data,
                    aes(label = observation_id), 
                    color = "red") +
    xlab("Observation id") + ylab(y_axis_label) +
    theme_minimal()
  
  # Return the plot
  return(p)
}


sd_cmj <- plot_outliers_sd(data, "cmj", "CMJ height (cm)")

sd_10m <- plot_outliers_sd(data, "X10m", "10m sprint (s)")

sd_30m <- plot_outliers_sd(data, "X30m", "30m sprint (s)")

sd_5rm <- plot_outliers_sd(data, "X5rm", "5RM (kg)")

# Outliers using 3*SD data figure
(sd_cmj + sd_5rm) / (sd_10m + sd_30m)  + 
  plot_annotation(title = "Outliers using 3 x SD",
                  tag_levels = "A",
                  tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))



### (3) MODIFIED Z-SCORES

plot_modified_z_scores <- function(data, column_name, y_axis_name = "Modified z-score (AU)") {
  
  # Ensure the column name is a character string and exists in the dataframe
  if (!is.character(column_name) || !column_name %in% names(data)) {
    stop("The column name must be a character string and present in the dataframe.")
  }
  
  
  # Extract the specified column
  data_ <- data[[column_name]]
  
  # Get the median of the data
  median_ <- median(data_, na.rm = TRUE)
  
  # Calculate the absolute difference between each value and the median
  data_minus_median_ <- abs(data_ - median_)
  
  # Calculate MAD (median of the absolute deviations about the median)
  MAD_ <- median(data_minus_median_, na.rm = TRUE)
  
  # Modified Z-scores
  mod_z_scores_ <- 0.6745 * (data_ - median_) / MAD_
  
  # Take the absolute value of the modified z-score
  abs_mod_z_scores_ <- abs(mod_z_scores_)
  
  # Create a data frame for ggplot
  plot_data <- data.frame(z_score = abs_mod_z_scores_, observation_id = seq_along(abs_mod_z_scores_))
  
  # Create the plot
  z_plot <- ggplot(plot_data, aes(x = observation_id, y = z_score)) + 
    geom_point() +
    geom_hline(yintercept = 3.5, linetype = 2, linewidth = 0.7) +
    geom_text_repel(data = subset(plot_data, z_score > 3.5),
                    aes(label = observation_id),
                    color = "red") +
    xlab("Observation id") + ylab(y_axis_name) +
    theme_minimal()
  
  # Return the plot
  return(z_plot)
}


z_plot_cmj <- plot_modified_z_scores(data, "cmj", "Modified z-score (AU) [CMJ]")

z_plot_10m <- plot_modified_z_scores(data, "X10m", "Modified z-score (AU) [10m]")

z_plot_30m <- plot_modified_z_scores(data, "X30m", "Modified z-score (AU) [30m]")

z_plot_5rm <- plot_modified_z_scores(data, "X5rm", "Modified z-score (AU) [5RM]")

(z_plot_cmj + z_plot_5rm) / (z_plot_10m + z_plot_30m)  + 
  plot_annotation(title = "Outliers using Modified Z-scores",
                  tag_levels = "A",
                  tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))



#### Outliers: Multiple constract techniques ####

#### (1) LEVERAGE ####
#create my reference regression model
fit0 <- lm(X10m ~ time + group + time*group + height_scaled + weight_scaled, data = data)

#determine leverage
leverage_fit0 <- as.data.frame(hatvalues(fit0)) #leverage for each value
colnames(leverage_fit0)[1] <- "leverage"        #change column name
leverage_fit0                                   #visualise data frame
k <- length(coef(fit0))                     #number of predictors
n <- nrow(data)                              #sample size

#cut-off 2(k+1)/n (Cohen et al., 2003)
lev_cutoff <- ((2*(k+1))/n)
subset(leverage_fit0, hatvalues(fit0) > lev_cutoff)

#create a new column if outlier or not
breaks <- c(0, lev_cutoff, 1)
labs <- c("ok", "outlier")
leverage_fit0$cutoff <- cut(leverage_fit0$leverage, breaks = breaks, labels = labs)

#plot leverage
lev <- ggplot() + 
  geom_point(data=leverage_fit0, aes(x = as.numeric(row.names(leverage_fit0)), 
                                     y = leverage)) +
  geom_hline(yintercept = lev_cutoff, linetype = 2, linewidth = 0.7) +
  geom_text_repel(data = subset(leverage_fit0, 
                                leverage > lev_cutoff),
                  aes(x = as.numeric(row.names(
                    subset(leverage_fit0, leverage > lev_cutoff))),
                    y = leverage,
                    label = as.numeric(row.names(
                      subset(leverage_fit0, leverage > lev_cutoff)))),
                  color = "red", max.overlaps = 20) + 
  ylab("Leverage") + xlab("Observation id") +
  theme_minimal() +
  labs(title = "Leverage value per observation")

#see the rows detected from main data frame
data[as.numeric(row.names(subset(leverage_fit0, leverage > lev_cutoff))),]
# player id = 0: OUTLIER

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### (2) STUDENTISED DELETED RESIDUALS ####

#get the data from studentised deleted residuals function
sdr <- olsrr::ols_plot_resid_stud(fit0)
sdr_data <- sdr$data
#get the cutoff proposed by Aguinis et al. (2013)
k <- length(coef(fit0)) #number of predictors
n <- nrow(data)
#find t critical value
sdr_cutoff <- qt(p=(0.05/n), df=(n-k-1), lower.tail=FALSE)

#Plot studentised deleated residuals
stu_del_res <- ggplot() + 
  geom_point(data=sdr_data, aes(x=obs, y=dsr)) +
  geom_hline(yintercept = sdr_cutoff, linetype = 2, linewidth = 0.7) +
  geom_hline(yintercept = -sdr_cutoff, linetype = 2, linewidth = 0.7) +
  geom_text_repel(data = subset(sdr_data, 
                                dsr > sdr_cutoff | dsr < -sdr_cutoff),
                  aes(x = obs,
                      y = dsr,
                      label = obs),
                  color = "red", max.overlaps = 20) + 
  ylab("Studentised deleated residuals") + xlab("Observation ID") +
  theme_minimal() +
  labs(title = "Error outliers: Studentised delated residuals")


#### (3) AIC MIXED MODEL ####

## (3.1) Investigate player ID first (random effect)

#create a list where to store the aic derived by iteratively removing each player id
my_aic_players_list <- list()
#use a for loop to iteratively each player id and get AIC
for(i in seq(1,length(unique(data$id)),1)) {
  my_aic_players_list[[i]] <- AIC(
    lmer(X10m ~ time + group + time*group +
           height_scaled + weight_scaled +
           (1|id),
         data = subset(data, id != sort(unique(data$id))[i]), REML = TRUE))
}
#convert list to dataframe and change column name
my_aic_players_df <- do.call(rbind.data.frame, my_aic_players_list)
colnames(my_aic_players_df)[1] <- "aic"

#visualise the effect of AIC by removing each player id
#if AIC is high means that the specific player is important for the model
#a low AIC may suggest that the removal of that player id actually improves
#the fit of the model.
aic_player <- ggplot(data = my_aic_players_df) + 
  geom_point(aes(x = as.numeric(row.names(my_aic_players_df))-1, 
                 y = aic)) +
  geom_text_repel(data = subset(my_aic_players_df, aic< -72), 
                  aes(x = as.numeric(
                    row.names(
                      subset(my_aic_players_df, aic< -72)))-1,
                    y = aic,
                    label = as.numeric(
                      row.names(
                        subset(my_aic_players_df, aic< -72))
                    )-1), color = "red") +
  xlab("Player id") +
  ylab("AIC") + theme_minimal() +
  labs(title = "Model fit outliers: AIC for player id")



## (3.2) Investigate each observation

my_aic_obs_list <- list()
for(i in seq(1,nrow(data),1)) {
  my_aic_obs_list[[i]] <- AIC(
    lmer(X10m ~ time + group + time*group +
           height_scaled + 
           weight_scaled + 
           (1|id),
         data = data[-i,], REML = TRUE))
}
#convert list to dataframe and change column name
my_aic_obs_df <- do.call(rbind.data.frame, my_aic_obs_list)
colnames(my_aic_obs_df)[1] <- "aic"

#visualise the AIC by removing each observation
#lower values show that if removed the AIC decreases
#suggesting that it is a better model
aic_obs <- ggplot(data = my_aic_obs_df) + 
  geom_point(aes(x = as.numeric(row.names(my_aic_obs_df)), 
                 y = aic)) +
  geom_text_repel(data = subset(my_aic_obs_df, aic< -73), 
                  aes(x = as.numeric(
                    row.names(
                      subset(my_aic_obs_df, aic< -73))),
                    y = aic,
                    label = as.numeric(
                      row.names(
                        subset(my_aic_obs_df, aic< -73))
                    )), color = "red") +
  xlab("Observation id") +
  ylab("AIC") + theme_minimal() +
  labs(title = "Model fit outliers: AIC for observation id")



#### (4) DFFITS ####

### (4.1) DFFITS REGRESSION

# get the dffits values for regression model
dffits0 <- as.data.frame(dffits(fit0)) 
colnames(dffits0)[1] <- "dffit"
# get hat values to get the mean leverage
hat <- as.data.frame(hatvalues(fit0))  
# using cutoff 2*sqrt(p/n) where p/n is the mean leverage (Li & Valliant, 2011)
#Leverage is the group of values in the diagonal of the hat matrix
dffits_cutoff <- 2*sqrt(mean(hat$`hatvalues(fit0)`))


#create a new column which identifies outliers
dffits0$outlier <- ifelse(dffits0$dffit > dffits_cutoff | dffits0$dffit <= -dffits_cutoff, 
                          "outlier", "ok")

#filter out outliers
dffits1 <- subset(dffits0, dffit > dffits_cutoff | dffit <= -dffits_cutoff)

#Plot dffits for general linear model with columns instead of points
dffits_plot_linear <- ggplot() + 
  geom_point(data = dffits0, 
             aes(x = as.numeric(row.names(dffits0)), y = dffit)) +
  geom_hline(yintercept = dffits_cutoff, 
             linetype = 2, linewidth = 0.7) +
  geom_hline(yintercept = -dffits_cutoff, 
             linetype = 2, linewidth = 0.7) +
  geom_text_repel(data = subset(dffits0, 
                                dffit > dffits_cutoff | dffit <= -dffits_cutoff), 
                  aes(x = as.numeric(
                    row.names(
                      subset(dffits0, dffit > dffits_cutoff | dffit <= -dffits_cutoff))), 
                    y = dffit, 
                    label = as.numeric(
                      row.names(
                        subset(dffits0, 
                               dffit > dffits_cutoff | dffit <= -dffits_cutoff)))), 
                  color = "red") +
  xlab("Observation id") + ylab("DFFITS") +
  theme_minimal() +
  labs(title = "DFFITS general linear model")


### (4.2) DFFITS MIXED MODEL

####DFFITS MIXED MODEL: PLAYER GROUP ####
#for each player
X10m_model1 <- lmer(X10m ~ time*group + height_scaled + weight_scaled +
                      (1 | id), data = data)
dffits_mixed_players <- as.data.frame(HLMdiag::mdffits(X10m_model1, level = "id")) 
names(dffits_mixed_players) <- "dffit"

#get hat values for the mixed model
# hat values to get the mean leverage
hat_mixed <- as.data.frame(hatvalues(X10m_model1))  
names(hat_mixed) <- "hat"

# using cutoff 2*sqrt(p/n) where p/n is the mean leverage (Li & Valliant, 2011) 
#Leverage is the group of values in the diagonal of the hat matrix
dffits_mixed_cutoff <- 2*sqrt(mean(hat_mixed$hat))

#Aguinis et al. (2013) cutoff for DFFITS
#mz_cutoff <- 2*sqrt((attributes(logLik(fit_mixed))$df+1)/nrow(dffits_mixed_players))
# However, I am gonna use the cutoff proposed by Li & Valliant (2011).
#Both Li & Valliant (2011) and Aguinis et al. (2013) use the same reference for cutoff
#which is Belsley et al. (1980).
#I have used the leverage to determine the cutoff, same as in the regression model.
#However,the leverage should be derived in a different way from the linear model.
# In mixed models, a leverage for the fixed effects and the leverage for 
# the random effects should be computed (Demidenko and Stukel, 2005).
#However, the leverage computation for mixed  models with crossed random
# effect is not supported by the HLMdiag package (Loy and Hofmann, 2014).
#In addition, the function to determine DFFITS is not available in the 
# package influence.ME.
#Demidenko, E., & Stukel, T. A. (2005) Influence analysis for linear mixed-effects models. 
#Statistics in Medicine, 24(6), 893-909.
#
#HLMdiag::leverage(fit_mixed) # does not work, not supported for crossed random effects
#
#Consequently, the leverage derived from the hat matrix of the mixed model was used
#as a cutoff due to the limitations of other packages and also the cutoff should represent
#a general guideline to focus on specific observations rather than a strict rule.

#plot dffits mixed model for each player id with columns
dffits_plot_mixed_players <- ggplot() + 
  geom_point(data = dffits_mixed_players, 
             aes(x = as.numeric(row.names(dffits_mixed_players))-1, y = dffit)) +
  geom_text_repel(data = subset(dffits_mixed_players, 
                                dffit>0.1), 
                  aes(x = as.numeric(
                    row.names(
                      subset(dffits_mixed_players, 
                             dffit>0.1)))-1, 
                    y = dffit, 
                    label = as.numeric(
                      row.names(
                        subset(dffits_mixed_players, 
                               dffit>0.1)))-1), 
                  color = "red") +
  #geom_hline(yintercept = dffits_mixed_cutoff,linetype = 2, size = 0.7) + #this would be the cutoff
  xlab("Player id") + ylab("DFFITS") +
  theme_minimal() +
  labs(title = "DFFITS mixed model for each player")


### (4.3) DFFITS MIXED MODEL: OBSERVATIONS

#get dffits values for observations 
#for each observation
dffits_mixed <- as.data.frame(HLMdiag::mdffits(X10m_model1)) 
names(dffits_mixed) <- "dffit"

#using 2*sqrt(p/n) I detect zero outliers. However, there is the issue 
#about how I determined the leverage. Consequently, I may simply look at 
#the most extreme values.
dffits1_mixed <- subset(dffits_mixed, dffit > 2*sqrt(mean(hat_mixed$hat)) 
                        | dffit <= -2*sqrt(mean(hat_mixed$hat)))
#return zero results as outleirs

#plot the dffits mixed model per observation without the cutoff
dffits_plot_mixed <- ggplot() + 
  geom_point(data = dffits_mixed, 
             aes(x = as.numeric(row.names(dffits_mixed)), y = dffit)) +
  geom_text_repel(data = subset(dffits_mixed, 
                                dffit>0.04), 
                  aes(x = as.numeric(
                    row.names(
                      subset(dffits_mixed, 
                             dffit>0.04))), 
                    y = dffit, 
                    label = as.numeric(
                      row.names(
                        subset(dffits_mixed, 
                               dffit>0.04)))), 
                  color = "red") +
  xlab("Observation id") + ylab("DFFITS") +
  theme_minimal() +
  labs(title = "DFFITS mixed model for each observation")

#Figure for DFFITS
figure_dffits <- dffits_plot_linear + dffits_plot_mixed_players + dffits_plot_mixed +
  # plot_layout(width = c(2,1)) +
  plot_annotation(title = "DFFITS 10m",
                  theme = theme(plot.title = element_text(size = 16,
                                                          hjust = 0.5)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### (5) COOK'S DISTANCE ####


#### (5.1) COOK'S DISTANCE REGRESSION

#get cook's distance for regression model
cooksd <- as.data.frame(cooks.distance(fit0))
names(cooksd) <- "d"

#cutoff for cook's distance as 4/n cutoff (Van der Meer et al., 2006)
cooks_cutoff <- 4/nrow(data)

#final observations that might be considered influential.
cook_infl <- subset(cooksd, d > cooks_cutoff) 

#plot cook's distance for regression model
cooks_plot_linear <- ggplot() + 
  geom_point(data = cooksd, aes(x = as.numeric(
    row.names(cooksd)), 
    y = d)) + 
  geom_hline(yintercept = cooks_cutoff, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(cooksd, d > cooks_cutoff), 
                  aes(x = as.numeric(
                    row.names(
                      subset(cooksd, d > cooks_cutoff))), 
                    y = d, 
                    label = as.numeric(
                      row.names(
                        subset(cooksd, d > cooks_cutoff)))), 
                  color = "red") +
  xlab("Observation id") + ylab("Cook's d") +
  theme_minimal() +
  labs(title = "Cook's distance general linear model")

#rows identified in main data frame
data[as.numeric(
  row.names(
    subset(cooksd, d > cooks_cutoff))),]


#### (5.2) COOK'S DISTANCE MIXED MODEL


### COOK'S DISTANCE MIXED MODEL: PLAYER GROUP

#Influential observations based on the group of "players" as random effect:
infl_obs_players <- influence.ME::influence(X10m_model1, group = "id")
#get cook's d mixed model per each player
cooks_d_mixed_players <- as.data.frame(influence.ME::cooks.distance.estex(infl_obs_players))
names(cooks_d_mixed_players) <- "d"

#cook's distance cutoff 4/n (Van der Meer et al., 2006)
4/nrow(cooks_d_mixed_players)

#plot cook's distance mixed model per player id
cooks_mixed_players_plot <- ggplot() + 
  geom_point(data = cooks_d_mixed_players, 
             aes(x = as.numeric(row.names(cooks_d_mixed_players)),
                 y = d)) + 
  geom_hline(yintercept = 4/nrow(cooks_d_mixed_players), 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(cooks_d_mixed_players, d>4/nrow(cooks_d_mixed_players)), 
                  aes(x = as.numeric(
                    row.names(
                      subset(cooks_d_mixed_players, d>4/nrow(cooks_d_mixed_players)))), 
                    y = d, 
                    label = as.numeric(
                      row.names(
                        subset(cooks_d_mixed_players, d>4/nrow(cooks_d_mixed_players))))), 
                  color = "red") +
  xlab("Player id") + ylab("Cook's d") +
  theme_minimal() +
  labs(title = "Cook's distance mixed model per player id")


### COOK'S DISTANCE MIXED MODEL: OBSERVATIONS

#Influential observations based on each single observation:
infl_obs <- influence.ME::influence(X10m_model1, obs = TRUE)
#get cook's d mixed model for each observation
cooks_d_mixed <- as.data.frame(influence.ME::cooks.distance.estex(infl_obs))
names(cooks_d_mixed) <- "d"

#plot cook's d mixed model for each observation
cooks_mixed_obs_plot <- ggplot() + 
  geom_point(data = cooks_d_mixed, 
             aes(x = as.numeric(row.names(cooks_d_mixed)),
                 y = d)) + 
  geom_hline(yintercept = 4/nrow(cooks_d_mixed), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(cooks_d_mixed, d>4/nrow(cooks_d_mixed)), 
                  aes(x = as.numeric(
                    row.names(
                      subset(cooks_d_mixed, d>4/nrow(cooks_d_mixed)))), 
                    y = d, 
                    label = as.numeric(
                      row.names(
                        subset(cooks_d_mixed, d>4/nrow(cooks_d_mixed))))), 
                  color = "red") +
  xlab("Observation id") + ylab("Cook's d") +
  theme_minimal() +
  labs(title = "Cook's distance mixed model per observation")





############################### Modelling ######################################


#### CMJ ####
cmj_model <- lmer(cmj ~ time*group + (1 | id), data = data)

cmj_model1 <- lmer(cmj ~ time*group + height_scaled + weight_scaled +
                     (1 | id), data = data)

anova(cmj_model, cmj_model1)
# COMMENT: There is evidence to support cmj_model1                                (1)

# Creating Estimated Marginal Means
emm_cmj <- emmeans(cmj_model1, ~ time*group)
# Pairwise comparisons for time within each group
pairs(emm_cmj, simple="group")
confint(pairs(emm_cmj, simple="group"))
# Comparisons between groups at each time point
pairs(emm_cmj, simple="time")
confint(pairs(emm_cmj, simple="time"))
# COMMENT: No statistically significant differences between and within            (2)
# groups for CMJ. 


### Checking model assumptions

# return a list of single plots
diagnostic_plots_cmj <- plot(check_model(cmj_model1, panel = FALSE))

# posterior predictive checks
diagnostic_plots_cmj[[1]] # This looks good and assumptions are met.

# linearity
diagnostic_plots_cmj[[2]] # It is mostly flat and linear, hence good.

# homoscedasticiy - homogeneity of variance
diagnostic_plots_cmj[[3]]
check_heteroscedasticity(cmj_model1)
# OK: Error variance appears to be homoscedastic (p = 0.298).

# influential observations - outliers
diagnostic_plots_cmj[[4]]
check_outliers(cmj_model1)
# OK: No outliers detected.
# - Based on the following method and threshold: cook (0.849).
# - For variable: (Whole model)

# normally distributed residuals
diagnostic_plots_cmj[[5]] # All good here.

# multicollinearity
check_collinearity(cmj_model1) # no multicollinearity, low VIF < 3

# normality of random effects
diagnostic_plots_cmj[[6]] # dots mostly normal.

# COMMENT: Assumptions of linearity, homogeity of variance, multicollinearity,    (3)
# normality of residuals, normality of random effects are met. No outliers
# have been detected for CMJ.

# COMMENT: cmj_model1 is my final model for inference !                           (4)
sjPlot::tab_model(cmj_model1)

# anova table
car::Anova(cmj_model1)

## Plot CMJ estimates
pred_cmj <- ggpredict(cmj_model1, terms = c("time", "group"))
plot_pred_cmj <- ggplot(pred_cmj) + 
  geom_point(aes(x = as.factor(x), y = predicted, color=as.factor(group)), size=3,position = position_dodge(0.3)) +
  geom_line(aes(x = as.factor(x), y = predicted, group=as.factor(group), color=as.factor(group)),position = position_dodge(0.3)) +
  geom_errorbar(aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, color= as.factor(group)), width = .1, position = position_dodge(0.3)) +
  scale_color_manual(values = c(CON_COLOR, OWL_COLOR), labels = c("CON", "WLG")) +
  labs(x = "Time", y = "CMJ height (cm)", color = "Group:") + 
  theme_minimal() +
  guides(color=FALSE) +
  theme(legend.position = "right") +
  scale_x_discrete(labels=c("0" = "Pre", "1" = "Post"))



############################## X10m ####################################
X10m_model <- lmer(X10m ~ time*group + (1 | id), data = data)


X10m_model1 <- lmer(X10m ~ time*group + height_scaled + weight_scaled +
                      (1 | id), data = data)

# model without the top outlier for weight (player id 0)
data_no_outliers_10m <- data[-c(18,1),]
X10m_model2 <- lmer(X10m ~ time*group + height_scaled + weight_scaled +
                      (1 | id), data = data_no_outliers_10m)

anova(X10m_model, X10m_model1)
# There is evidence to support model1

# Creating Estimated Marginal Means
X10m_emm <- emmeans(X10m_model2, ~ time*group)
# Pairwise comparisons for time within each group
pairs(X10m_emm, simple="group")
confint(pairs(X10m_emm, simple="group"))
# Comparisons between groups at each time point
pairs(X10m_emm, simple="time")
confint(pairs(X10m_emm, simple="time"))


### Checking model assumptions
# return a list of single plots
diagnostic_plots_10m <- plot(check_model(X10m_model2, panel = FALSE))

# posterior predicive checks
diagnostic_plots_10m[[1]] # This looks good and assumptions are met.

# linearity
diagnostic_plots_10m[[2]] # It is mostly flat and linear, hence good.

# homoscedasticiy - homogeneity of variance
diagnostic_plots_10m[[3]]
check_heteroscedasticity(X10m_model2)
# Warning: Heteroscedasticity (non-constant error variance) detected (p < .001).

# To solve this use robust covariance matrix estimation
# https://easystats.github.io/parameters/articles/model_parameters_robust.html
# model parameters with cluster robust estimation
model_parameters(
  X10m_model2,
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = data_no_outliers_10m$id)
)


# influential observations - outliers
check_outliers(X10m_model1, method="all")
# 2 outliers detected: cases 1, 18.
# - Based on the following methods and thresholds: zscore_robust (3.291), 
# iqr (2), ci (1), cook (0.849), mahalanobis (20), mahalanobis_robust (20), 
# mcd (20), ics (0.001), optics (8), lof (0.001).
# - For variable: (Whole model).
# 
# Note: Outliers were classified as such by at least half of the selected methods.
check_outliers(X10m_model2, method="all")
# OK: No outliers detected.
# - Based on the following methods and thresholds: zscore_robust (3.291),  
# iqr (2), ci (1), cook (0.85), mahalanobis (20), mahalanobis_robust (20), 
# mcd (20), ics (0.001), optics (8), lof (0.001).
# - For variable: (Whole model)

# COMMENT: After removing the top outlier for weight, no outliers are detected.   (5)

# multicollinearity
diagnostic_plots_10m[[4]]
check_collinearity(X10m_model2) # All good here.

# normally distributed residuals
diagnostic_plots_10m[[5]] # dots are similar to normal.
# It is also possible to calculate heteroscedasticity-consistent standard errors to help.

# normality of random effects
diagnostic_plots_10m[[6]] # all good here.

# This should be my final model parameters !!
model_parameters(
  X10m_model2,
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = data_no_outliers_10m$id)
)


# COMMENT: This model represents the one used for inference for 10m.              (6)
# Only one outlier was removed as it was the heaviest one.

# COMMENT: Assumptions of linearity, normality of random effects,                 (7)
# multicollinearity, normality of distributed residuals were met.
# However, homogeneity of variance was not met. Hence, the parameter estimation
# using robust estimators.


## Plot model 10m estimates
pred_10m <- ggpredict(X10m_model2, terms = c("time", "group"))
plot_pred_10m <- ggplot(pred_10m) + 
  geom_point(aes(x = as.factor(x), y = predicted, color=as.factor(group)), size=3,position = position_dodge(0.3)) +
  geom_line(aes(x = as.factor(x), y = predicted, group=as.factor(group), color=as.factor(group)),position = position_dodge(0.3)) +
  geom_errorbar(aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, color= as.factor(group)), width = .1, position = position_dodge(0.3)) +
  scale_color_manual(values = c(CON_COLOR, OWL_COLOR), labels = c("CON", "WLG")) +
  labs(x = "Time", y = "10m sprint (s)", color = "Group:") + 
  theme_minimal() +
  theme(legend.position = "right") +
  scale_x_discrete(labels=c("0" = "Pre", "1" = "Post"))



############################## X30m ####################################
X30m_model <- lmer(X30m ~ time*group + (1 | id), data = data)


X30m_model1 <- lmer(X30m ~ time*group + height_scaled + weight_scaled +
                      (1 | id), data = data)

anova(X30m_model, X30m_model1)
# There is evidence to support model1

# model without the outlier identified in 10m
X30m_model2 <- lmer(X30m ~ time*group + height_scaled + weight_scaled +
                      (1 | id), data = data_no_outliers_10m)

X30m_model3 <- lmer(X30m ~ time*group +
                      (1 | id), data = data_no_outliers_10m)
anova(X30m_model2, X30m_model3)

# Creating Estimated Marginal Means
X30m_emm <- emmeans(X30m_model2, ~ time*group)
# Pairwise comparisons for time within each group
pairs(X30m_emm, simple="group")
confint(pairs(X30m_emm, simple="group"))
# Comparisons between groups at each time point
pairs(X30m_emm, simple="time")
confint(pairs(X30m_emm, simple="time"))


# return a list of single plots
diagnostic_plots_30m <- plot(check_model(X30m_model2, panel = FALSE))

# posterior predicive checks
diagnostic_plots_30m[[1]] # This looks good and assumptions are met.

# linearity
diagnostic_plots_30m[[2]] # It is mostly flat and linear, hence good.

# homoscedasticiy - homogeneity of variance
diagnostic_plots_30m[[3]]
check_heteroscedasticity(X30m_model2)
# Warning: Heteroscedasticity (non-constant error variance) detected (p < .001).

# To solve this use robust covariance matrix estimation
# https://easystats.github.io/parameters/articles/model_parameters_robust.html
# model parameters with cluster robust estimation
model_parameters(
  X30m_model2,
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = data_no_outliers_10m$id)
)


# influential observations - outliers
diagnostic_plots_30m[[4]]
check_outliers(X30m_model1)
# 1 outlier detected: case 58.
# - Based on the following method and threshold: cook (0.903).
# - For variable: (Whole model).

check_outliers(X30m_model2)
# OK: No outliers detected.
# - Based on the following method and threshold: cook (0.85).
# - For variable: (Whole model)

# normality of residuals
diagnostic_plots_30m[[5]] # all okay

# multicollinearity
check_collinearity(X30m_model2) # All good here.

# normally distributed random effects
diagnostic_plots_30m[[6]] # dots are not too far from normal.

# This should be my final model parameters !!
model_parameters(
  X30m_model2,
  vcov = "vcovCR",
  vcov_args = list(type = "CR1", cluster = data_no_outliers_10m$id)
)


## 30m
pred_30m <- ggpredict(X30m_model2, terms = c("time", "group"))
plot_pred_30m <- ggplot(pred_30m) + 
  geom_point(aes(x = as.factor(x), y = predicted, color=as.factor(group)), size=3,position = position_dodge(0.3)) +
  geom_line(aes(x = as.factor(x), y = predicted, group=as.factor(group), color=as.factor(group)),position = position_dodge(0.3)) +
  geom_errorbar(aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, color= as.factor(group)), width = .1, position = position_dodge(0.3)) +
  scale_color_manual(values = c(CON_COLOR, OWL_COLOR), labels = c("CON", "WLG")) +
  labs(x = "Time", y = "30m sprint (s)", color = "Group") + 
  theme_minimal() +
  guides(color=FALSE) +
  theme(legend.position = "right") +
  scale_x_discrete(labels=c("0" = "Pre", "1" = "Post"))


############################## X5rm ####################################
X5rm_model <- lmer(X5rm ~ time + group + time*group + (1 | id), data = data)

X5rm_model1 <- lmer(X5rm ~ time*group + height_scaled + weight_scaled +
                      (1 | id), data = data)

anova(X5rm_model, X5rm_model1)
# There is no evidence to support model1

# Creating Estimated Marginal Means
X5rm_emm <- emmeans(X5rm_model, ~ group*time)
# Pairwise comparisons for time within each group
pairs(X5rm_emm, simple="group")
confint(pairs(X5rm_emm, simple="group"))
# Comparisons between groups at each time point
pairs(X5rm_emm, simple="time")
confint(pairs(X5rm_emm, simple="time"))
# The significant decrease observed when comparing group 0 at time 0 with 
# group 1 at time 1 (-21.71) suggests that the changes over time are not uniform 
# across groups, with group 1 experiencing a larger decrease.


# return a list of single plots
diagnostic_plots_5rm <- plot(check_model(X5rm_model, panel = FALSE))

# posterior predicive checks
diagnostic_plots_5rm[[1]] # This looks good and assumptions are met.

# linearity
diagnostic_plots_5rm[[2]] # It is mostly flat and linear, hence good.

# homoscedasticiy - homogeneity of variance
diagnostic_plots_5rm[[3]]
check_heteroscedasticity(X5rm_model)
# OK: Error variance appears to be homoscedastic (p = 0.097).

# influential observations - outliers
check_outliers(X5rm_model)
# OK: No outliers detected.
# - Based on the following method and threshold: cook (0.701).
# - For variable: (Whole model)

# multicollinearity
diagnostic_plots_5rm[[4]]
check_collinearity(X5rm_model) # All good here.

# normally distributed residuals
diagnostic_plots_5rm[[5]] # dots are not too far from normal.

# normality of random effects
diagnostic_plots_5rm[[6]] # All good here.

# COMMENT: All assumptions are satisfied.                                         (8)

# X5rm_model should be my final model parameters !!


## 5RM
pred_5rm <- ggpredict(X5rm_model, terms = c("time", "group"))
plot_pred_5rm <- ggplot(pred_5rm) + 
  geom_point(aes(x = as.factor(x), y = predicted, color=as.factor(group)), size=3,position = position_dodge(0.3)) +
  geom_line(aes(x = as.factor(x), y = predicted, group=as.factor(group), color=as.factor(group)),position = position_dodge(0.3)) +
  geom_errorbar(aes(x = as.factor(x), ymin = conf.low, ymax = conf.high, color= as.factor(group)), width = .1, position = position_dodge(0.3)) +
  scale_color_manual(values = c(CON_COLOR, OWL_COLOR), labels = c("CON", "WLG")) +
  labs(x = "Time", y = "5RM BS (kg)", color = "Group:") + 
  theme_minimal() +
  guides(color=FALSE) +
  theme(legend.position = "right") +
  scale_x_discrete(labels=c("0" = "Pre", "1" = "Post"))

################################################################################


#### Figure predictions ####

(plot_pred_cmj + plot_pred_5rm) / (plot_pred_10m + plot_pred_30m) +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "",#"Model estimated values",
                  tag_levels = "A",
                  tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0),
        plot.title = element_text(size = 16, hjust = 0.5))







