#### OUTLIERS DETECTION SSG DATA FRAME STUDY2b ####

library(lme4)
library(influence.ME)
library(olsrr)
library(ggplot2)
library(patchwork)
library(ggrepel)

#Loading data:
ssg <- read.csv("./ssg_study.csv", sep=",")
ssg$date <- as.Date(ssg$date, "%d/%m/%Y")
str(ssg)

#### ERROR OUTLIERS ####
#### 1) SINGLE CONSTRUCT TECHNIQUES ####

#### 1.1) BOX PLOTS ####
box_td <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = total_distance_m)) +
  ylab("Metre (m)") + xlab("Total distance") +
  labs(title = "(a) Total distance")
box_hsr <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = tot_hsr_distance)) +
  ylab("Metre (m)") + xlab("HSR distance") +
  labs(title = "(b) HSR (>61%)")
box_pl <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = total_player_load)) +
  ylab("Arbitrary units (AU)") + xlab("Player load") +
  labs(title = "(c) PL")
box_pls <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = player_load_slow)) +
  ylab("Arbitrary units (AU)") + xlab("Player load slow") +
  labs(title = "(d) PL slow (<2 m/s)")
box_acc <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = acceleration_density)) +
  ylab("Metre/second^2 (m/s^2)") + xlab("Avg accel decel") +
  labs(title = "(e) Avg acc dec")
box_gu <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = get_up)) +
  ylab("Count (n)") + xlab("Get up") +
  labs(title = "(f) Get up")
box_bu <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = bullet)) +
  ylab("Count (n)") + xlab("Bullet") +
  labs(title = "(g) Bullet")
box_tri <- ggplot() + 
  geom_boxplot(data = ssg, aes(x = "", y = stagnos_trimp)) +
  ylab("Arbitrary units (AU)") + xlab("TRIMP") +
  labs(title = "(h) Stagno's TRIMP")

figure_box_plot <- (box_td | box_hsr | box_pl | box_pls) / 
  (box_acc | box_gu | box_bu | box_tri) +
  plot_annotation(title = "Box plot SSG",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)))#Figure 1: box plot
#Boxplots represent a univariate approach to investigate
#the data for potential influential observations and outliers.
#Boxplot are shown for each dependent and independent variable.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 1.2) 3*SD ####
sd_td <- ggplot() +
  geom_point(data = as.data.frame(ssg$total_distance_m), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$total_distance_m))), 
                 y = ssg$total_distance_m)) +
  geom_hline(yintercept = mean(ssg$total_distance_m) + 3*sd(ssg$total_distance_m), 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = mean(ssg$total_distance_m) - 3*sd(ssg$total_distance_m), 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                total_distance_m > mean(ssg$total_distance_m) + 3*sd(ssg$total_distance_m) | 
                                  total_distance_m < mean(ssg$total_distance_m) - 3*sd(ssg$total_distance_m)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, total_distance_m > mean(ssg$total_distance_m) + 3*sd(ssg$total_distance_m) | 
                               total_distance_m < mean(ssg$total_distance_m) - 3*sd(ssg$total_distance_m)))),
                    y = total_distance_m,
                    label = as.numeric(
                      row.names(
                        subset(ssg, total_distance_m > mean(ssg$total_distance_m) + 3*sd(ssg$total_distance_m) 
                               | total_distance_m < mean(ssg$total_distance_m) - 3*sd(ssg$total_distance_m))))),
                  color = "red") +
  xlab("Observation id") + ylab("Total distance (m)") + 
  labs(title = "(a) 3*sd: Total distance")

sd_hs <- ggplot() +
  geom_point(data = as.data.frame(ssg$tot_hsr_distance), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$tot_hsr_distance))), 
                 y = ssg$tot_hsr_distance)) +
  geom_hline(yintercept = mean(ssg$tot_hsr_distance) + 3*sd(ssg$tot_hsr_distance), 
             linetype = 2, size = 0.7) +
  #geom_hline(yintercept = mean(ssg$tot_hsr_distance) - 3*sd(ssg$tot_hsr_distance), 
  #           linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                tot_hsr_distance > mean(ssg$tot_hsr_distance) + 3*sd(ssg$tot_hsr_distance)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, tot_hsr_distance > mean(ssg$tot_hsr_distance) + 3*sd(ssg$tot_hsr_distance)))),
                    y = tot_hsr_distance,
                    label = as.numeric(
                      row.names(
                        subset(ssg, tot_hsr_distance > mean(ssg$tot_hsr_distance) + 3*sd(ssg$tot_hsr_distance))))),
                  color = "red") +
  xlab("Observation id") + ylab("HSR distance (m)") + 
  labs(title = "(b) 3*sd: HSR (>61%)")

sd_acc <- ggplot() +
  geom_point(data = as.data.frame(ssg$acceleration_density), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$acceleration_density))), 
                 y = ssg$acceleration_density)) +
  geom_hline(yintercept = mean(ssg$acceleration_density) + 3*sd(ssg$acceleration_density), 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = mean(ssg$acceleration_density) - 3*sd(ssg$acceleration_density), 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                acceleration_density > mean(ssg$acceleration_density) + 3*sd(ssg$acceleration_density) | 
                                  acceleration_density < mean(ssg$acceleration_density) - 3*sd(ssg$acceleration_density)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, acceleration_density > mean(ssg$acceleration_density) + 3*sd(ssg$acceleration_density) | 
                               acceleration_density < mean(ssg$acceleration_density) - 3*sd(ssg$acceleration_density)))),
                    y = acceleration_density,
                    label = as.numeric(
                      row.names(
                        subset(ssg, acceleration_density > mean(ssg$acceleration_density) + 3*sd(ssg$acceleration_density) 
                               | acceleration_density < mean(ssg$acceleration_density) - 3*sd(ssg$acceleration_density))))),
                  color = "red") +
  xlab("Observation id") + ylab("Avg acc decel (m/s^2)") + 
  labs(title = "(e) 3*sd: Avg acc dec")

sd_pl <- ggplot() +
  geom_point(data = as.data.frame(ssg$total_player_load), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$total_player_load))), 
                 y = ssg$total_player_load)) +
  geom_hline(yintercept = mean(ssg$total_player_load) + 3*sd(ssg$total_player_load), 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = mean(ssg$total_player_load) - 3*sd(ssg$total_player_load), 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                total_player_load > mean(ssg$total_player_load) + 3*sd(ssg$total_player_load) | 
                                  total_player_load < mean(ssg$total_player_load) - 3*sd(ssg$total_player_load)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, total_player_load > mean(ssg$total_player_load) + 3*sd(ssg$total_player_load) | 
                               total_player_load < mean(ssg$total_player_load) - 3*sd(ssg$total_player_load)))),
                    y = total_player_load,
                    label = as.numeric(
                      row.names(
                        subset(ssg, total_player_load > mean(ssg$total_player_load) + 3*sd(ssg$total_player_load) 
                               | total_player_load < mean(ssg$total_player_load) - 3*sd(ssg$total_player_load))))),
                  color = "red") +
  xlab("Observation id") + ylab("Player load (AU)") + 
  labs(title = "(c) 3*sd: PL")

sd_pls <- ggplot() +
  geom_point(data = as.data.frame(ssg$player_load_slow), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$player_load_slow))), 
                 y = ssg$player_load_slow)) +
  geom_hline(yintercept = mean(ssg$player_load_slow) + 3*sd(ssg$player_load_slow), 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = mean(ssg$player_load_slow) - 3*sd(ssg$player_load_slow), 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                player_load_slow > mean(ssg$player_load_slow) + 3*sd(ssg$player_load_slow) | 
                                  player_load_slow < mean(ssg$player_load_slow) - 3*sd(ssg$player_load_slow)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, player_load_slow > mean(ssg$player_load_slow) + 3*sd(ssg$player_load_slow) | 
                               player_load_slow < mean(ssg$player_load_slow) - 3*sd(ssg$player_load_slow)))),
                    y = player_load_slow,
                    label = as.numeric(
                      row.names(
                        subset(ssg, player_load_slow > mean(ssg$player_load_slow) + 3*sd(ssg$player_load_slow) 
                               | player_load_slow < mean(ssg$player_load_slow) - 3*sd(ssg$player_load_slow))))),
                  color = "red") +
  xlab("Observation id") + ylab("Player load slow (AU)") + 
  labs(title = "(d) 3*sd: PL slow (<2m/s)")

sd_tri <- ggplot() +
  geom_point(data = as.data.frame(ssg$stagnos_trimp), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$stagnos_trimp))), 
                 y = ssg$stagnos_trimp)) +
  geom_hline(yintercept = mean(ssg$stagnos_trimp) + 3*sd(ssg$stagnos_trimp), 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = mean(ssg$stagnos_trimp) - 3*sd(ssg$stagnos_trimp), 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                stagnos_trimp > mean(ssg$stagnos_trimp) + 3*sd(ssg$stagnos_trimp) | 
                                  stagnos_trimp < mean(ssg$stagnos_trimp) - 3*sd(ssg$stagnos_trimp)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, stagnos_trimp > mean(ssg$stagnos_trimp) + 3*sd(ssg$stagnos_trimp) | 
                               stagnos_trimp < mean(ssg$stagnos_trimp) - 3*sd(ssg$stagnos_trimp)))),
                    y = stagnos_trimp,
                    label = as.numeric(
                      row.names(
                        subset(ssg, stagnos_trimp > mean(ssg$stagnos_trimp) + 3*sd(ssg$stagnos_trimp) 
                               | stagnos_trimp < mean(ssg$stagnos_trimp) - 3*sd(ssg$stagnos_trimp))))),
                  color = "red") +
  xlab("Observation id") + ylab("TRIMP (AU)") + 
  labs(title = "(h) 3*sd: Stagno's TRIMP")

sd_bu <- ggplot() +
  geom_point(data = as.data.frame(ssg$bullet), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$bullet))), 
                 y = ssg$bullet)) +
  geom_hline(yintercept = mean(ssg$bullet) + 3*sd(ssg$bullet), 
             linetype = 2, size = 0.7) +
  #geom_hline(yintercept = mean(ssg$bullet) - 3*sd(ssg$bullet), 
  #           linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                bullet > mean(ssg$bullet) + 3*sd(ssg$bullet)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, bullet > mean(ssg$bullet) + 3*sd(ssg$bullet)))),
                    y = bullet,
                    label = as.numeric(
                      row.names(
                        subset(ssg, bullet > mean(ssg$bullet) + 3*sd(ssg$bullet))))),
                  color = "red", max.overlaps = 20) +
  xlab("Observation id") + ylab("Bullet (n)") + 
  labs(title = "(g) 3*sd: Bullet")

sd_gu <- ggplot() +
  geom_point(data = as.data.frame(ssg$get_up), 
             aes(x = as.numeric(row.names(as.data.frame(ssg$get_up))), 
                 y = ssg$get_up)) +
  geom_hline(yintercept = mean(ssg$get_up) + 3*sd(ssg$get_up), 
             linetype = 2, size = 0.7) +
  #geom_hline(yintercept = mean(ssg$get_up) - 3*sd(ssg$get_up), 
  #           linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(ssg, 
                                get_up > mean(ssg$get_up) + 3*sd(ssg$get_up)),
                  aes(x = as.numeric(
                    row.names(
                      subset(ssg, get_up > mean(ssg$get_up) + 3*sd(ssg$get_up)))),
                    y = get_up,
                    label = as.numeric(
                      row.names(
                        subset(ssg, get_up > mean(ssg$get_up) + 3*sd(ssg$get_up))))),
                  color = "red") +
  xlab("Observation id") + ylab("Get up (n)") + 
  labs(title = "(f) 3*sd: Get up")

figure_sd <- (sd_td | sd_hs | sd_pl | sd_pls) / (sd_acc | sd_gu | sd_bu | sd_tri)
# Figure 3*SD for each variable collected.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 1.3) MODIFIED Z-SCORES ####

# I. TOTAL DISTANCE
data_td <- ssg$total_distance_m
#get the median of the data
median_td <- median(data_td)

#calculate the absolute difference between each value and the median
data_minus_median_td <- c()
for (i in seq_along(data_td)) {
  data_minus_median_td[i] <- abs(data_td[i] - median_td)
}
data_minus_median_td
#calculate MAD (median of the absoluite deviations about the median)
MAD_td <- median(data_minus_median_td)

#modified Z-scores
mod_z_scores_td <- c()
for (i in seq_along(data_td)) {
  mod_z_scores_td[i] <- 0.6745*(data_td[i] - median_td)/MAD_td
}
mod_z_scores_td
#Take the absolute value of the modified z-score
abs_mod_z_scores_td <- as.data.frame(abs(mod_z_scores_td))
colnames(abs_mod_z_scores_td) <- "z_score"

z_td <- ggplot() + 
  geom_point(data = abs_mod_z_scores_td, 
             aes(x = as.numeric(row.names(abs_mod_z_scores_td)), 
                 y = z_score)) +
  geom_hline(yintercept = 3.5, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(abs_mod_z_scores_td, z_score > 3.5),
                  aes(x = as.numeric(row.names(subset(abs_mod_z_scores_td, z_score > 3.5))),
                      y = z_score,
                      label = as.numeric(row.names(subset(abs_mod_z_scores_td, z_score > 3.5)))),
                  color = "red") +
  xlab("Observation id") + ylab("Modified z-score (AU)") + 
  labs(title = "(a) Mod z-score: total distance")


# II. HSR DISTANCE
data_hs <- ssg$tot_hsr_distance
#get the median of the data
median_hs <- median(data_hs)
#calculate the absolute difference between each value and the median
data_minus_median_hs <- c()
for (i in seq_along(data_hs)) {
  data_minus_median_hs[i] <- abs(data_hs[i] - median_hs)
}
data_minus_median_hs
#calculate MAD (median of the absoluite deviations about the median)
MAD_hs <- median(data_minus_median_hs)

#modified Z-scores
mod_z_scores_hs <- c()
for (i in seq_along(data_hs)) {
  mod_z_scores_hs[i] <- 0.6745*(data_hs[i] - median_hs)/MAD_hs
}
mod_z_scores_hs
#Take the absolute value of the modified z-score
abs_mod_z_scores_hs <- as.data.frame(abs(mod_z_scores_hs))
colnames(abs_mod_z_scores_hs) <- "z_score"
#
#As MAD is zero, this leads to x/0, thus leading to NaN/Inf values !
# Therefore, no modified z-scores available for HSR distance.

# III. AVERAGE ACCELERATION DECELERATION
data_acc <- ssg$acceleration_density
#get the median of the data
median_acc <- median(data_acc)

#calculate the absolute difference between each value and the median
data_minus_median_acc <- c()
for (i in seq_along(data_acc)) {
  data_minus_median_acc[i] <- abs(data_acc[i] - median_acc)
}
data_minus_median_acc
#calculate MAD (median of the absoluite deviations about the median)
MAD_acc <- median(data_minus_median_acc)

#modified Z-scores
mod_z_scores_acc <- c()
for (i in seq_along(data_acc)) {
  mod_z_scores_acc[i] <- 0.6745*(data_acc[i] - median_acc)/MAD_acc
}
mod_z_scores_acc
#Take the absolute value of the modified z-score
abs_mod_z_scores_acc <- as.data.frame(abs(mod_z_scores_acc))
colnames(abs_mod_z_scores_acc) <- "z_score"

z_acc <- ggplot() + 
  geom_point(data = abs_mod_z_scores_acc, 
             aes(x = as.numeric(row.names(abs_mod_z_scores_acc)), 
                 y = z_score)) +
  geom_hline(yintercept = 3.5, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(abs_mod_z_scores_acc, z_score > 3.5),
                  aes(x = as.numeric(row.names(subset(abs_mod_z_scores_acc, z_score > 3.5))),
                      y = z_score,
                      label = as.numeric(row.names(subset(abs_mod_z_scores_acc, z_score > 3.5)))),
                  color = "red") +
  xlab("Observation id") + ylab("Modified z-score (AU)") + 
  labs(title = "(d) Mod z-score: avg acc dec")

# IV. PLAYER LOAD
data_pl <- ssg$total_player_load
#get the median of the data
median_pl <- median(data_pl)

#calculate the absolute difference between each value and the median
data_minus_median_pl <- c()
for (i in seq_along(data_pl)) {
  data_minus_median_pl[i] <- abs(data_pl[i] - median_pl)
}
data_minus_median_pl
#calculate MAD (median of the absoluite deviations about the median)
MAD_pl <- median(data_minus_median_pl)

#modified Z-scores
mod_z_scores_pl <- c()
for (i in seq_along(data_pl)) {
  mod_z_scores_pl[i] <- 0.6745*(data_pl[i] - median_pl)/MAD_pl
}
mod_z_scores_pl
#Take the absolute value of the modified z-score
abs_mod_z_scores_pl <- as.data.frame(abs(mod_z_scores_pl))
colnames(abs_mod_z_scores_pl) <- "z_score"

z_pl <- ggplot() + 
  geom_point(data = abs_mod_z_scores_pl, 
             aes(x = as.numeric(row.names(abs_mod_z_scores_pl)), 
                 y = z_score)) +
  geom_hline(yintercept = 3.5, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(abs_mod_z_scores_pl, z_score > 3.5),
                  aes(x = as.numeric(row.names(subset(abs_mod_z_scores_pl, z_score > 3.5))),
                      y = z_score,
                      label = as.numeric(row.names(subset(abs_mod_z_scores_pl, z_score > 3.5)))),
                  color = "red") +
  xlab("Observation id") + ylab("Modified z-score (AU)") + 
  labs(title = "(b) Mod z-score: PL")


# V. PLAYER LOAD SLOW
data_pls <- ssg$player_load_slow
#get the median of the data
median_pls <- median(data_pls)

#calculate the absolute difference between each value and the median
data_minus_median_pls <- c()
for (i in seq_along(data_pls)) {
  data_minus_median_pls[i] <- abs(data_pls[i] - median_pls)
}
data_minus_median_pls
#calculate MAD (median of the absoluite deviations about the median)
MAD_pls <- median(data_minus_median_pls)

#modified Z-scores
mod_z_scores_pls <- c()
for (i in seq_along(data_pls)) {
  mod_z_scores_pls[i] <- 0.6745*(data_pls[i] - median_pls)/MAD_pls
}
mod_z_scores_pls
#Take the absolute value of the modified z-score
abs_mod_z_scores_pls <- as.data.frame(abs(mod_z_scores_pls))
colnames(abs_mod_z_scores_pls) <- "z_score"

z_pls <- ggplot() + 
  geom_point(data = abs_mod_z_scores_pls, 
             aes(x = as.numeric(row.names(abs_mod_z_scores_pls)), 
                 y = z_score)) +
  geom_hline(yintercept = 3.5, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(abs_mod_z_scores_pls, z_score > 3.5),
                  aes(x = as.numeric(row.names(subset(abs_mod_z_scores_pls, z_score > 3.5))),
                      y = z_score,
                      label = as.numeric(row.names(subset(abs_mod_z_scores_pls, z_score > 3.5)))),
                  color = "red") +
  xlab("Observation id") + ylab("Modified z-score (AU)") + 
  labs(title = "(c) Mod z-score: PL slow")


# VI. GET UP
data_gu <- ssg$get_up
#get the median of the data
median_gu <- median(data_gu)

#calculate the absolute difference between each value and the median
data_minus_median_gu <- c()
for (i in seq_along(data_gu)) {
  data_minus_median_gu[i] <- abs(data_gu[i] - median_gu)
}
data_minus_median_gu
#calculate MAD (median of the absoluite deviations about the median)
MAD_gu <- median(data_minus_median_gu)

#modified Z-scores
mod_z_scores_gu <- c()
for (i in seq_along(data_gu)) {
  mod_z_scores_gu[i] <- 0.6745*(data_gu[i] - median_gu)/MAD_gu
}
mod_z_scores_gu
#Take the absolute value of the modified z-score
abs_mod_z_scores_gu <- as.data.frame(abs(mod_z_scores_gu))
colnames(abs_mod_z_scores_gu) <- "z_score"
#
#As MAD is zero, this leads to x/0, thus leading to NaN/Inf values !
# Therefore, no modified z-scores available for HSR distance.

# VII. BULLET
data_bu <- ssg$bullet
#get the median of the data
median_bu <- median(data_bu)

#calculate the absolute difference between each value and the median
data_minus_median_bu <- c()
for (i in seq_along(data_bu)) {
  data_minus_median_bu[i] <- abs(data_bu[i] - median_bu)
}
data_minus_median_bu
#calculate MAD (median of the absoluite deviations about the median)
MAD_bu <- median(data_minus_median_bu)

#modified Z-scores
mod_z_scores_bu <- c()
for (i in seq_along(data_bu)) {
  mod_z_scores_bu[i] <- 0.6745*(data_bu[i] - median_bu)/MAD_bu
}
mod_z_scores_bu
#Take the absolute value of the modified z-score
abs_mod_z_scores_bu <- as.data.frame(abs(mod_z_scores_bu))
colnames(abs_mod_z_scores_bu) <- "z_score"
#
#As MAD is zero, this leads to x/0, thus leading to NaN/Inf values
# Therefore, no modified z-scores available for HSR distance.

# VIII. STAGNO'S TRIMP
data_tri <- ssg$stagnos_trimp
#get the median of the data
median_tri <- median(data_tri)

#calculate the absolute difference between each value and the median
data_minus_median_tri <- c()
for (i in seq_along(data_tri)) {
  data_minus_median_tri[i] <- abs(data_tri[i] - median_tri)
}
data_minus_median_tri
#calculate MAD (median of the absoluite deviations about the median)
MAD_tri <- median(data_minus_median_tri)

#modified Z-scores
mod_z_scores_tri <- c()
for (i in seq_along(data_tri)) {
  mod_z_scores_tri[i] <- 0.6745*(data_tri[i] - median_tri)/MAD_tri
}
mod_z_scores_tri
#Take the absolute value of the modified z-score
abs_mod_z_scores_tri <- as.data.frame(abs(mod_z_scores_tri))
colnames(abs_mod_z_scores_tri) <- "z_score"

z_tri <- ggplot() + 
  geom_point(data = abs_mod_z_scores_tri, 
             aes(x = as.numeric(row.names(abs_mod_z_scores_tri)), 
                 y = z_score)) +
  geom_hline(yintercept = 3.5, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(abs_mod_z_scores_tri, z_score > 3.5),
                  aes(x = as.numeric(row.names(subset(abs_mod_z_scores_tri, z_score > 3.5))),
                      y = z_score,
                      label = as.numeric(row.names(subset(abs_mod_z_scores_tri, z_score > 3.5)))),
                  color = "red") +
  xlab("Observation id") + ylab("Modified z-score (AU)") + 
  labs(title = "(d) Mod z-score: Stagno's TRIMP")

#observations will be labelled as outliers when 
# |modified Z-score| > 3.5
# 3.5 is based on 10,000 simulations
#( Iglewicz, B., & Hoaglin, D. C. (1993). How to detect and handle outliers (Vol. 16). Asq Press., p.12)

figure_mod_z_scores <- (z_td | z_pls | z_pl) /  (z_acc | z_tri)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### ERROR OUTLIERS ####
#### 2) MULTIPLE CONSTRUCT TECHNIQUES ####

#### 2.1) LEVERAGE ####
#create my reference regression model
#regression model
fit0 <- lm(stagnos_trimp ~ total_distance_m + 
             tot_hsr_distance + 
             acceleration_density + 
             total_player_load + 
             player_load_slow + 
             get_up + bullet, data = ssg)

#determine leverage
leverage_fit0 <- as.data.frame(hatvalues(fit0)) #leverage for each value
colnames(leverage_fit0)[1] <- "leverage"        #change column name
leverage_fit0                                   #visualise data frame
k <- length(coef(fit0))                     #number of predictors
n <- nrow(ssg)                              #sample size

#cut-off 2(k+1)/n (Cohen et al., 2003)
lev_cutoff <- ((2*(k+1))/n)
subset(leverage_fit0, hatvalues(fit0) > lev_cutoff)

#create a new column if outlier or not
breaks <- c(0, lev_cutoff, 1)
labs <- c("ok", "outlier")
leverage_fit0$cutoff <- cut(leverage_fit0$leverage, breaks = breaks, labels = labs)
leverage_fit0

#plot leverage
lev <- ggplot() + 
  geom_point(data=leverage_fit0, aes(x = as.numeric(row.names(leverage_fit0)), 
                                     y = leverage)) +
  geom_hline(yintercept = lev_cutoff, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(leverage_fit0, 
                                leverage > lev_cutoff),
                  aes(x = as.numeric(row.names(
                    subset(leverage_fit0, leverage > lev_cutoff))),
                    y = leverage,
                    label = as.numeric(row.names(
                      subset(leverage_fit0, leverage > lev_cutoff)))),
                  color = "red", max.overlaps = 20) + 
  ylab("Leverage") + xlab("Observation id") +
  labs(title = "Leverage plot")+ 
  theme(plot.title = element_text(size=16,hjust = 0.5))

#see the rows detected from main data frame
ssg[as.numeric(row.names(subset(leverage_fit0, leverage > lev_cutoff))),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 2.2) STUDENTISED DELETED RESIDUALS ####

#get the data from studentised deleted residuals function
sdr <- ols_plot_resid_stud(fit0)
sdr_data <- sdr$data
#get the cutoff proposed by Aguinis et al. (2013)
k <- length(coef(fit0)) #number of predictors
n <- nrow(ssg)
#find t critical value
sdr_cutoff <- qt(p=(0.05/n), df=(n-k-1), lower.tail=FALSE)

#Plot studentised deleated residuals
stu_del_res <- ggplot() + 
  geom_point(data=sdr_data, aes(x=obs, y=dsr)) +
  geom_hline(yintercept = sdr_cutoff, linetype = 2, size = 0.7) +
  geom_hline(yintercept = -sdr_cutoff, linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(sdr_data, 
                                dsr > sdr_cutoff | dsr < -sdr_cutoff),
                  aes(x = obs,
                      y = dsr,
                      label = obs),
                  color = "red", max.overlaps = 20) + 
  ylab("Studentised deleted residuals") + xlab("Observation ID") +
  labs(title = "Studentised deleted residuals plot")+ 
  theme(plot.title = element_text(size=16,hjust = 0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####INFLUENTIAL OUTLIERS ####

#### 1) MODEL FIT OUTLIERS ####

#### 1.1) R SQUARED ####
#get R_squared for each model where one observation is removed
#create a copy of the data frame to play with
data_train <- ssg
#create an empty list to store the multiple r squared values
my_r_squared <- list()
#use a for loop to iteratively remove one row from the data frame and derive r squared
for(i in seq(1,nrow(data_train),1)) {
  my_r_squared[[i]] <- summary(
    lm(stagnos_trimp ~ total_distance_m + 
         tot_hsr_distance + 
         acceleration_density + 
         total_player_load + 
         player_load_slow + 
         get_up + bullet, data = data_train[-i,])
  )$r.squared
}
#convert the dataframe to a list and rename the column
my_r_squared_df <- do.call(rbind.data.frame, my_r_squared)
colnames(my_r_squared_df)[1] <- "r_squared"

#plot the r squared values
r_squared <- ggplot(data = my_r_squared_df) + 
  geom_point(aes(x = as.numeric(row.names(my_r_squared_df)), 
                 y = r_squared)) +
  geom_text_repel(data = subset(my_r_squared_df, r_squared>0.1091), 
                  aes(x = as.numeric(
                    row.names(
                      subset(my_r_squared_df, r_squared>0.1091))),
                    y = r_squared,
                    label = as.numeric(
                      row.names(
                        subset(my_r_squared_df, r_squared>0.1091))
                    )), color = "red") +
  xlab("Observation id") +
  ylab("R squared") +
  labs(title = "Coefficient of determination plot")+ 
  theme(plot.title = element_text(size=16,hjust = 0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 1.2) AIC MIXED MODEL ####

## 1.2.1) Investigate player ID first (random effect)

#create a list where to store the aic derived by iteratively removing each player id
my_aic_players_list <- list()
#use a for loop to iteratively each player id and get AIC
for(i in seq(1,length(unique(data_train$players)),1)) {
  my_aic_players_list[[i]] <- AIC(
    lmer(stagnos_trimp ~ total_distance_m + 
           tot_hsr_distance + 
           acceleration_density + 
           total_player_load + 
           player_load_slow + 
           get_up + bullet + 
           (1|date) +
           (1|players) +
           (1|date:ssg_bout) +
           (1|date:players),
         data = subset(data_train, players != i), REML = TRUE))
}
#convert list to dataframe and change column name
my_aic_players_df <- do.call(rbind.data.frame, my_aic_players_list)
colnames(my_aic_players_df)[1] <- "aic"

#visualise the effect of AIC by removing each player id
#if AIC is high means that the specific player is important for the model
#a low AIC may suggest that the removal of that player id actually improves
#the fit of the model.
aic_player <- ggplot(data = my_aic_players_df) + 
  geom_point(aes(x = as.numeric(row.names(my_aic_players_df)), 
                 y = aic)) +
  geom_text_repel(data = subset(my_aic_players_df, aic<7850), 
                  aes(x = as.numeric(
                    row.names(
                      subset(my_aic_players_df, aic<7850))),
                    y = aic,
                    label = as.numeric(
                      row.names(
                        subset(my_aic_players_df, aic<7850))
                    )), color = "red") +
  xlab("Player id") +
  ylab("AIC") +
  labs(title = "Model fit outliers: AIC for player id")

player_14 <- subset(ssg, players == 14) #two rows are zeros for trimp
player_22 <- subset(ssg, players == 22) #three rows are zeros for trimp
player_29 <- subset(ssg, players == 29) #two rows are zeros for trimp
player_31 <- subset(ssg, players == 31) #one rows is very low, 159AU for trimp
player_34 <- subset(ssg, players == 34) #trimp seems legit
player_37 <- subset(ssg, players == 37) #one row is zero for trimp


## 1.2.2) Investigate each observation

my_aic_obs_list <- list()
for(i in seq(1,nrow(data_train),1)) {
  my_aic_obs_list[[i]] <- AIC(
    lmer(stagnos_trimp ~ total_distance_m + 
           tot_hsr_distance + 
           acceleration_density + 
           total_player_load + 
           player_load_slow + 
           get_up + bullet + 
           (1|date) +
           (1|players) +
           (1|date:ssg_bout) +
           (1|date:players),
         data = data_train[-i,], REML = TRUE))
}
#convert list to dataframe and change column name
my_aic_obs_df <- do.call(rbind.data.frame, my_aic_obs_list)
colnames(my_aic_obs_df)[1] <- "aic"
my_aic_obs_df

#visualise the AIC by removing each observation
#lower values show that if removed the AIC decreases
#suggesting that it is a better model
aic_obs <- ggplot(data = my_aic_obs_df) + 
  geom_point(aes(x = as.numeric(row.names(my_aic_obs_df)), 
                 y = aic)) +
  geom_text_repel(data = subset(my_aic_obs_df, aic<8050), 
                  aes(x = as.numeric(
                    row.names(
                      subset(my_aic_obs_df, aic<8050))),
                    y = aic,
                    label = as.numeric(
                      row.names(
                        subset(my_aic_obs_df, aic<8050))
                    )), color = "red") +
  xlab("Observation id") +
  ylab("AIC") +
  labs(title = "AIC plot") + 
  theme(plot.title = element_text(size=16,hjust = 0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### PREDICTOR OUTLIERS ####

#### 2) DFBETAS ####

#### 2.1) DFBETAS REGRESSION ####
#Get dfbetas values for each X variable in the model
dfbetas0 <- as.data.frame(dfbetas(fit0))
#Check whether every row contains values greater than the threshold of 2/sqrt(n)
dfb <- list()
for(i in seq(1, nrow(dfbetas0), 1)) {
  dfb[[i]] <- any(dfbetas0[i, ] > 2/sqrt(nrow(dfbetas0))) # create a list with TRUE and FALSE to use as condition
}
dfb
dfb <- do.call(rbind.data.frame, dfb) 
names(dfb) <- "true_false"
dfb
dfbetas1 <- cbind.data.frame(dfbetas0, dfb) # cbind the TRUE FALSE column with rest of data frame
infl_obs_dfbetas <- subset(dfbetas1, true_false == TRUE) # filter data that are greater than threshold

dfb_plot_obs_int <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = `(Intercept)`, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, `(Intercept)` > 0.2 | `(Intercept)` < -0.2),
                  aes(x = `(Intercept)`,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, `(Intercept)` > 0.2 | `(Intercept)` < -0.2))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, `(Intercept)` > 0.2 | `(Intercept)` < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "Intercept")

dfb_plot_obs_td <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = total_distance_m, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, total_distance_m > 0.18 | total_distance_m < -0.2),
                  aes(x = total_distance_m,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, total_distance_m > 0.18 | total_distance_m < -0.2))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, total_distance_m > 0.18 | total_distance_m < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "Total distance")

dfb_plot_obs_hs <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = tot_hsr_distance, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, tot_hsr_distance > 0.20 | tot_hsr_distance < -0.19),
                  aes(x = tot_hsr_distance,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, tot_hsr_distance > 0.20 | tot_hsr_distance < -0.19))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, tot_hsr_distance > 0.20 | tot_hsr_distance < -0.19)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "HSR distance")

dfb_plot_obs_acc <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = acceleration_density, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, acceleration_density > 0.19 | acceleration_density < -0.19),
                  aes(x = acceleration_density,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, acceleration_density > 0.19 | acceleration_density < -0.19))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, acceleration_density > 0.19 | acceleration_density < -0.19)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "Avg acc dec")

dfb_plot_obs_pl <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = total_player_load, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, total_player_load > 0.19 | total_player_load < -0.19),
                  aes(x = total_player_load,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, total_player_load > 0.19 | total_player_load < -0.19))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, total_player_load > 0.19 | total_player_load < -0.19)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "PL")

dfb_plot_obs_pls <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = player_load_slow, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, player_load_slow > 0.19 | player_load_slow < -0.19),
                  aes(x = player_load_slow,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, player_load_slow > 0.19 | player_load_slow < -0.19))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, player_load_slow > 0.19 | player_load_slow < -0.19)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "PL slow")

dfb_plot_obs_gu <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = get_up, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, get_up > 0.19 | get_up < -0.19),
                  aes(x = get_up,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, get_up > 0.19 | get_up < -0.19))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, get_up > 0.19 | get_up < -0.19)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "Get-up")

dfb_plot_obs_bu <- ggplot() +
  geom_point(data = dfbetas1, 
             aes(x = bullet, 
                 y = as.numeric(row.names(dfbetas1)))) +
  geom_vline(xintercept = 2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_vline(xintercept = -2/sqrt(nrow(dfbetas1)), linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1, bullet > 0.19 | bullet < -0.19),
                  aes(x = bullet,
                      y = as.numeric(
                        row.names(
                          subset(dfbetas1, bullet > 0.19 | bullet < -0.19))),
                      label = as.numeric(
                        row.names(
                          subset(dfbetas1, bullet > 0.19 | bullet < -0.19)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "First-man-to-ruck")

figure_dfbetas <- (dfb_plot_obs_int | dfb_plot_obs_td | dfb_plot_obs_hs) /
  (dfb_plot_obs_pl |dfb_plot_obs_pls | dfb_plot_obs_acc )   + 
  plot_annotation(title = "DFBETAS plots",
                  theme = theme(plot.title = element_text(size = 16, hjust = 0.5)),
                  tag_levels = "A",
                  tag_suffix = '.') & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 10, hjust = 0, vjust = 0))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 2.2) DFBETAS MIXED MODEL ####

#create the same model used previously for AIC
fit_mixed <- lmer(stagnos_trimp ~ total_distance_m +  tot_hsr_distance + 
                    acceleration_density + total_player_load +
                    player_load_slow + get_up + bullet +
                    (1|date) +
                    (1|players) +
                    (1|date:ssg_bout) +
                    (1|date:players),
                  data = ssg, REML = TRUE)

#### DFBETAS MIXED MODEL: PLAYER GROUP ####

#Influential observations based on the group of "players" as random effect:
infl_obs_players <- influence.ME::influence(fit_mixed, group = "players")
#for each player
dfbetas_mixed_players <- influence.ME::dfbetas.estex(infl_obs_players) 
#convert to a data frae
dfbetas_mixed_players <- as.data.frame(dfbetas_mixed_players)

#cutoff for dfb players mixed: 2/sqrt(n) 
dfb_mixed_pl_cutoff <- 2/sqrt(nrow(dfbetas_mixed_players))

# rows with values greater than cut off are related to players ID 13, 17, 22, 27, 30, 31, 37
subset(ssg, players == 22)
#Interestingly enough, player ID 22 had four rows with unlikely TRIMP values (i.e, rows 554, 588, 622 = 0 and row 656 = 92.88).

dfb_plot_mixed_players_int <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = `(Intercept)`, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                `(Intercept)` > dfb_mixed_pl_cutoff |
                                  `(Intercept)` < -dfb_mixed_pl_cutoff),
                  aes(x = `(Intercept)`,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         `(Intercept)` > dfb_mixed_pl_cutoff |
                                           `(Intercept)` < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         `(Intercept)` > dfb_mixed_pl_cutoff |
                                           `(Intercept)` < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(a) Intercept")

dfb_plot_mixed_players_td <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = total_distance_m, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                total_distance_m > dfb_mixed_pl_cutoff |
                                  total_distance_m < -dfb_mixed_pl_cutoff),
                  aes(x = total_distance_m,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         total_distance_m > dfb_mixed_pl_cutoff |
                                           total_distance_m < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         total_distance_m > dfb_mixed_pl_cutoff |
                                           total_distance_m < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(b) Total distance")

dfb_plot_mixed_players_hs <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = tot_hsr_distance, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                tot_hsr_distance > dfb_mixed_pl_cutoff |
                                  tot_hsr_distance < -dfb_mixed_pl_cutoff),
                  aes(x = tot_hsr_distance,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         tot_hsr_distance > dfb_mixed_pl_cutoff |
                                           tot_hsr_distance < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         tot_hsr_distance > dfb_mixed_pl_cutoff |
                                           tot_hsr_distance < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(c) HSR distance")

dfb_plot_mixed_players_acc <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = acceleration_density, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                acceleration_density > dfb_mixed_pl_cutoff |
                                  acceleration_density < -dfb_mixed_pl_cutoff),
                  aes(x = acceleration_density,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         acceleration_density > dfb_mixed_pl_cutoff |
                                           acceleration_density < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         acceleration_density > dfb_mixed_pl_cutoff |
                                           acceleration_density < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(f) Avg acc dec")

dfb_plot_mixed_players_pl <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = total_player_load, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                total_player_load > dfb_mixed_pl_cutoff |
                                  total_player_load < -dfb_mixed_pl_cutoff),
                  aes(x = total_player_load,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         total_player_load > dfb_mixed_pl_cutoff |
                                           total_player_load < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         total_player_load > dfb_mixed_pl_cutoff |
                                           total_player_load < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(d) PL")

dfb_plot_mixed_players_pls <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = player_load_slow , 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                player_load_slow > dfb_mixed_pl_cutoff |
                                  player_load_slow < -dfb_mixed_pl_cutoff),
                  aes(x = player_load_slow,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         player_load_slow > dfb_mixed_pl_cutoff |
                                           player_load_slow < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         player_load_slow > dfb_mixed_pl_cutoff |
                                           player_load_slow < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(e) PL slow")

dfb_plot_mixed_players_gu <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = get_up, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                get_up > dfb_mixed_pl_cutoff |
                                  get_up < -dfb_mixed_pl_cutoff),
                  aes(x = get_up,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         get_up > dfb_mixed_pl_cutoff |
                                           get_up < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         get_up > dfb_mixed_pl_cutoff |
                                           get_up < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(g) Get up")

dfb_plot_mixed_players_bu <- ggplot() +
  geom_point(data = dfbetas_mixed_players, 
             aes(x = bullet, 
                 y = as.numeric(row.names(dfbetas_mixed_players)))) +
  geom_vline(xintercept = dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_pl_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas_mixed_players, 
                                bullet > dfb_mixed_pl_cutoff |
                                  bullet < -dfb_mixed_pl_cutoff),
                  aes(x = bullet,
                      y = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         bullet > dfb_mixed_pl_cutoff |
                                           bullet < -dfb_mixed_pl_cutoff))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas_mixed_players, 
                                         bullet > dfb_mixed_pl_cutoff |
                                           bullet < -dfb_mixed_pl_cutoff)))),
                  color = "red") +
  ylab("Player id") + xlab("DFBETAS") +
  labs(title = "(h) Bullet")

#Figure dfbetas mixed per player
figure_dfbetas_mixed_players <- (dfb_plot_mixed_players_int | dfb_plot_mixed_players_td | 
                     dfb_plot_mixed_players_hs |
                     dfb_plot_mixed_players_pl ) / 
  (dfb_plot_mixed_players_pls | dfb_plot_mixed_players_acc | 
     dfb_plot_mixed_players_gu | dfb_plot_mixed_players_bu)


#### DFBETAS MIXED MODEL: OBSERVATIONS ####
#Influential observations based on each single observation:
infl_obs <- influence.ME::influence(fit_mixed, obs = TRUE)
# for each observation
dfbetas_mixed <- influence.ME::dfbetas.estex(infl_obs)  
## dfbetas for observations:
# create a list with TRUE and FALSE to use as condition
dfb_mixed <- list()
for(i in seq(1, nrow(dfbetas_mixed), 1)) {
  dfb_mixed[[i]] <- any(dfbetas_mixed[i, ] > 2/sqrt(nrow(dfbetas_mixed))) 
}
dfb_mixed
dfb_mixed <- do.call(rbind.data.frame, dfb_mixed) 
names(dfb_mixed) <- "true_false"
dfb_mixed
# cbind the TRUE FALSE column with rest of data frame
dfbetas1_mixed <- cbind.data.frame(dfbetas_mixed, dfb_mixed)
# filter data that are greater than threshold
infl_obs_dfbetas_mixed <- subset(dfbetas1_mixed, true_false == TRUE) 

#using a cutoff of 2/sqrt(n)
dfb_mixed_obs_cutoff <- 2/sqrt(nrow(dfbetas_mixed))

#creating the plots for dfbetas mixed models for each observation
dfb_plot_mixed_obs_int <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = `(Intercept)`, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                `(Intercept)` > 0.2 |
                                  `(Intercept)` < -0.2),
                  aes(x = `(Intercept)`,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         `(Intercept)` > 0.2 |
                                           `(Intercept)` < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         `(Intercept)` > 0.2 |
                                           `(Intercept)` < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(a) Intercept")

dfb_plot_mixed_obs_td <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = total_distance_m, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                total_distance_m > 0.2 |
                                  total_distance_m < -0.2),
                  aes(x = total_distance_m,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         total_distance_m > 0.2 |
                                           total_distance_m < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         total_distance_m > 0.2 |
                                           total_distance_m < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(b) Total distance")

dfb_plot_mixed_obs_hs <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = tot_hsr_distance, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                tot_hsr_distance > 0.2 |
                                  tot_hsr_distance < -0.2),
                  aes(x = tot_hsr_distance,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         tot_hsr_distance > 0.2 |
                                           tot_hsr_distance < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         tot_hsr_distance > 0.2 |
                                           tot_hsr_distance < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(c) HSR distance")

dfb_plot_mixed_obs_acc <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = acceleration_density, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                acceleration_density > 0.2 |
                                  acceleration_density < -0.2),
                  aes(x = acceleration_density,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         acceleration_density > 0.2 |
                                           acceleration_density < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         acceleration_density > 0.2 |
                                           acceleration_density < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(f) Avg acc dec")

dfb_plot_mixed_obs_pl <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = total_player_load, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                total_player_load > 0.2 |
                                  total_player_load < -0.2),
                  aes(x = total_player_load,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         total_player_load > 0.2 |
                                           total_player_load < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         total_player_load > 0.2 |
                                           total_player_load < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(d) PL")

dfb_plot_mixed_obs_pls <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = player_load_slow, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                player_load_slow > 0.2 |
                                  player_load_slow < -0.2),
                  aes(x = player_load_slow,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         player_load_slow > 0.2 |
                                           player_load_slow < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         player_load_slow > 0.2 |
                                           player_load_slow < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(e) PL slow")

dfb_plot_mixed_obs_gu <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = get_up, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                get_up > 0.2 |
                                  get_up < -0.2),
                  aes(x = get_up,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         get_up > 0.2 |
                                           get_up < -0.2))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         get_up > 0.2 |
                                           get_up < -0.2)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(g) Get up")

dfb_plot_mixed_obs_bu <- ggplot() +
  geom_point(data = dfbetas1_mixed, 
             aes(x = bullet, 
                 y = as.numeric(row.names(dfbetas1_mixed)))) +
  geom_vline(xintercept = dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_vline(xintercept = -dfb_mixed_obs_cutoff, 
             linetype = 2, size = 0.7) +
  geom_text_repel(data = subset(dfbetas1_mixed, 
                                bullet > 0.15 |
                                  bullet < -0.15),
                  aes(x = bullet,
                      y = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         bullet > 0.15 |
                                           bullet < -0.15))
                      ),
                      label = as.numeric(
                        row.names(subset(dfbetas1_mixed, 
                                         bullet > 0.15 |
                                           bullet < -0.15)))),
                  color = "red") +
  ylab("Observation id") + xlab("DFBETAS") +
  labs(title = "(h) Bullet") #+
  #theme(plot.title = element_text(size=12))

#assembling figures 
figure_dfbetas_mixed_obs <- (dfb_plot_mixed_obs_int | dfb_plot_mixed_obs_td | 
                           dfb_plot_mixed_obs_hs |
                           dfb_plot_mixed_obs_pl ) / 
  (dfb_plot_mixed_obs_pls | dfb_plot_mixed_obs_acc |
     dfb_plot_mixed_obs_gu | dfb_plot_mixed_obs_bu)

#creating final figure for DFBETAS mixed model observations
figure_dfbetas_mixed_obs + 
  plot_annotation(title = "DFBETAS per observation in general linear mixed model",
                  theme = theme(plot.title = element_text(size = 16))) 
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 3) DFFITS ####

#### 3.1) DFFITS REGRESSION ####

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
dffits0

#filter out outliers
dffits1 <- subset(dffits0, dffit > dffits_cutoff | dffit <= -dffits_cutoff)

#Plot dffits for general linear model with columns instead of points
ggplot() + 
  geom_col(data = dffits0, 
           aes(x = as.numeric(row.names(dffits0)), y = dffit)) +
  geom_hline(yintercept = dffits_cutoff, 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = -dffits_cutoff, 
             linetype = 2, size = 0.7) +
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
  labs(title = "DFFITS general linear model")
#same plot but with points intead of columns
dffits_plot_linear <- ggplot() + 
  geom_point(data = dffits0, 
           aes(x = as.numeric(row.names(dffits0)), y = dffit)) +
  geom_hline(yintercept = dffits_cutoff, 
             linetype = 2, size = 0.7) +
  geom_hline(yintercept = -dffits_cutoff, 
             linetype = 2, size = 0.7) +
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
  labs(title = "DFFITS plot")+
  theme(plot.title = element_text(size = 16, hjust = 0.5))


#### 3.2) DFFITS MIXED MODEL ####

####DFFITS MIXED MODEL: PLAYER GROUP ####
#for each player
dffits_mixed_players <- as.data.frame(HLMdiag::mdffits(fit_mixed, level = "players")) 
names(dffits_mixed_players) <- "dffit"

#get hat values for the mixed model
# hat values to get the mean leverage
hat_mixed <- as.data.frame(hatvalues(fit_mixed))  
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
ggplot() + 
  geom_col(data = dffits_mixed_players, 
           aes(x = as.numeric(row.names(dffits_mixed_players)), y = dffit)) +
  geom_text(data = subset(dffits_mixed_players, 
                          dffit>0.05), 
            aes(x = as.numeric(
              row.names(
                subset(dffits_mixed_players, 
                       dffit>0.05))), 
              y = dffit, 
              label = as.numeric(
                row.names(
                  subset(dffits_mixed_players, 
                         dffit>0.05)))), 
            color = "red", nudge_y = 0.01) +
  xlab("Player id") + ylab("DFFITS") +
  labs(title = "DFFITS mixed model for each player")

#same plot but with dots
dffits_plot_mixed_players <- ggplot() + 
  geom_point(data = dffits_mixed_players, 
           aes(x = as.numeric(row.names(dffits_mixed_players)), y = dffit)) +
  geom_text_repel(data = subset(dffits_mixed_players, 
                          dffit>0.05), 
            aes(x = as.numeric(
              row.names(
                subset(dffits_mixed_players, 
                       dffit>0.05))), 
              y = dffit, 
              label = as.numeric(
                row.names(
                  subset(dffits_mixed_players, 
                         dffit>0.05)))), 
            color = "red") +
  #geom_hline(yintercept = dffits_mixed_cutoff,linetype = 2, size = 0.7) + #this would be the cutoff
  xlab("Player id") + ylab("DFFITS") +
  labs(title = "DFFITS mixed model for each player")


####DFFITS MIXED MODEL: OBSERVATIONS ####

#get dffits values for observations 
#for each observation
dffits_mixed <- as.data.frame(HLMdiag::mdffits(fit_mixed)) 
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
                                dffit>0.02), 
                  aes(x = as.numeric(
                    row.names(
                      subset(dffits_mixed, 
                             dffit>0.02))), 
                    y = dffit, 
                    label = as.numeric(
                      row.names(
                        subset(dffits_mixed, 
                               dffit>0.02)))), 
                  color = "red") +
  xlab("Observation id") + ylab("DFFITS") +
  labs(title = "DFFITS mixed model for each observation")

#Figure for DFFITS
figure_dffits <- dffits_plot_linear + dffits_plot_mixed_players + dffits_plot_mixed +
  plot_layout(width = c(2,1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### 4) COOK'S DISTANCE ####


#### 4.1) COOK'S DISTANCE REGRESSION ####

#get cook's distance for regression model
cooksd <- as.data.frame(cooks.distance(fit0))
names(cooksd) <- "d"

#cutoff for cook's distance as 4/n cutoff (Van der Meer et al., 2006)
cooks_cutoff <- 4/nrow(ssg)

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
  xlab("Observation id") + ylab("d") +
  labs(title = "Cook's distance")+
  theme(plot.title = element_text(size = 16, hjust = 0.5))

#rows identified in main data frame
ssg[as.numeric(
  row.names(
    subset(cooksd, d > cooks_cutoff))),]


#### 4.2) COOK'S DISTANCE MIXED MODEL ####


#### COOK'S DISTANCE MIXED MODEL: PLAYER GROUP ####

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
  labs(title = "Cook's distance mixed model per player id")


#### COOK'S DISTANCE MIXED MODEL: OBSERVATIONS ####

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
  labs(title = "Cook's distance mixed model per observation")


#Cook's distance figure (regression + mixed model)
figure_cooks_d <- cooks_plot_linear + cooks_mixed_players_plot + cooks_mixed_obs_plot +
   plot_layout(widths = c(2, 1))




#===============================================================

#### ASSEMBLE ALL THE IDENTIFIED ROWS IN A DATA FRAME ####

three_sd_td <- as.numeric(
  row.names(
    subset(ssg, total_distance_m > mean(ssg$total_distance_m) + 3*sd(ssg$total_distance_m) | 
             total_distance_m < mean(ssg$total_distance_m) - 3*sd(ssg$total_distance_m))))

three_sd_hs <- as.numeric(
  row.names(
    subset(ssg, tot_hsr_distance > mean(ssg$tot_hsr_distance) + 3*sd(ssg$tot_hsr_distance))))

three_sd_acc <- as.numeric(
  row.names(
    subset(ssg, acceleration_density > mean(ssg$acceleration_density) + 3*sd(ssg$acceleration_density) | 
             acceleration_density < mean(ssg$acceleration_density) - 3*sd(ssg$acceleration_density))))

three_sd_pl <- as.numeric(
  row.names(
    subset(ssg, total_player_load > mean(ssg$total_player_load) + 3*sd(ssg$total_player_load) | 
             total_player_load < mean(ssg$total_player_load) - 3*sd(ssg$total_player_load))))

three_sd_pls <- as.numeric(
  row.names(
    subset(ssg, player_load_slow > mean(ssg$player_load_slow) + 3*sd(ssg$player_load_slow) | 
             player_load_slow < mean(ssg$player_load_slow) - 3*sd(ssg$player_load_slow))))

three_sd_tri <- as.numeric(
  row.names(
    subset(ssg, stagnos_trimp > mean(ssg$stagnos_trimp) + 3*sd(ssg$stagnos_trimp) | 
             stagnos_trimp < mean(ssg$stagnos_trimp) - 3*sd(ssg$stagnos_trimp))))

three_sd_bu <- as.numeric(
  row.names(
    subset(ssg, bullet > mean(ssg$bullet) + 3*sd(ssg$bullet))))

three_sd_gu <- as.numeric(
  row.names(
    subset(ssg, get_up > mean(ssg$get_up) + 3*sd(ssg$get_up))))

z_score_td <- as.numeric(row.names(subset(abs_mod_z_scores_td, z_score > 3.5)))

z_score_acc <- as.numeric(row.names(subset(abs_mod_z_scores_acc, z_score > 3.5)))

z_score_pl <- as.numeric(row.names(subset(abs_mod_z_scores_pl, z_score > 3.5)))

z_score_pls <- as.numeric(row.names(subset(abs_mod_z_scores_pls, z_score > 3.5)))

z_score_tri <- as.numeric(row.names(subset(abs_mod_z_scores_tri, z_score > 3.5)))

leverage_rows <- as.numeric(row.names(
  subset(leverage_fit0, leverage > lev_cutoff)))

sdr_rows <- as.numeric(row.names(subset(sdr_data, 
                                        dsr > sdr_cutoff | dsr < -sdr_cutoff)))

r_squared_rows <- as.numeric(
  row.names(
    subset(my_r_squared_df, r_squared>0.1091)))

aic_player_id <- as.numeric(
  row.names(
    subset(my_aic_players_df, aic<7850)))

aic_obs_rows <- as.numeric(
  row.names(
    subset(my_aic_obs_df, aic<8050)))

dfbetas_int_rows <- as.numeric(
  row.names(
    subset(dfbetas1, `(Intercept)` > 0.2 | `(Intercept)` < -0.2)))

dfbetas_td_rows <- as.numeric(
  row.names(
    subset(dfbetas1, total_distance_m > 0.18 | total_distance_m < -0.2)))

dfbetas_hs_rows <- as.numeric(
  row.names(
    subset(dfbetas1, tot_hsr_distance > 0.20 | tot_hsr_distance < -0.19)))

dfbetas_acc_rows <- as.numeric(
  row.names(
    subset(dfbetas1, acceleration_density > 0.19 | acceleration_density < -0.19)))

dfbetas_pl_rows <- as.numeric(
  row.names(
    subset(dfbetas1, total_player_load > 0.19 | total_player_load < -0.19)))

dfbetas_pls_rows <- as.numeric(
  row.names(
    subset(dfbetas1, player_load_slow > 0.19 | player_load_slow < -0.19)))

dfbetas_gu_rows <- as.numeric(
  row.names(
    subset(dfbetas1, get_up > 0.19 | get_up < -0.19)))

dfbetas_bu_rows <- as.numeric(
  row.names(
    subset(dfbetas1, bullet > 0.19 | bullet < -0.19)))

dfbetas_mix_players_int_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   `(Intercept)` > dfb_mixed_pl_cutoff |
                     `(Intercept)` < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_td_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   total_distance_m > dfb_mixed_pl_cutoff |
                     total_distance_m < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_hs_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   tot_hsr_distance > dfb_mixed_pl_cutoff |
                     tot_hsr_distance < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_acc_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   acceleration_density > dfb_mixed_pl_cutoff |
                     acceleration_density < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_pl_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   total_player_load > dfb_mixed_pl_cutoff |
                     total_player_load < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_pls_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   player_load_slow > dfb_mixed_pl_cutoff |
                     player_load_slow < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_gu_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   get_up > dfb_mixed_pl_cutoff |
                     get_up < -dfb_mixed_pl_cutoff)))

dfbetas_mix_players_bu_rows <- as.numeric(
  row.names(subset(dfbetas_mixed_players, 
                   bullet > dfb_mixed_pl_cutoff |
                     bullet < -dfb_mixed_pl_cutoff)))

dfbetas_mix_obs_int_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   `(Intercept)` > 0.2 |
                     `(Intercept)` < -0.2)))

dfbetas_mix_obs_td_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   total_distance_m > 0.2 |
                     total_distance_m < -0.2)))

dfbetas_mix_obs_hs_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   tot_hsr_distance > 0.2 |
                     tot_hsr_distance < -0.2)))

dfbetas_mix_obs_acc_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   acceleration_density > 0.2 |
                     acceleration_density < -0.2)))

dfbetas_mix_obs_pl_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   total_player_load > 0.2 |
                     total_player_load < -0.2)))

dfbetas_mix_obs_pls_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   player_load_slow > 0.2 |
                     player_load_slow < -0.2)))

dfbetas_mix_obs_gu_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   get_up > 0.2 |
                     get_up < -0.2)))

dfbetas_mix_obs_bu_rows <- as.numeric(
  row.names(subset(dfbetas1_mixed, 
                   bullet > 0.15 |
                     bullet < -0.15)))

dffits_rows <- as.numeric(
  row.names(
    subset(dffits0, dffit > dffits_cutoff | dffit <= -dffits_cutoff)))

dffits_mix_pl_rows <- as.numeric(
  row.names(
    subset(dffits_mixed_players, 
           dffit>0.05)))

dffits_mix_obs_rows <- as.numeric(
  row.names(
    subset(dffits_mixed, 
           dffit>0.02)))

cooks_rows <- as.numeric(
  row.names(
    subset(cooksd, d > cooks_cutoff)))

cooks_mix_pl_rows <- as.numeric(
  row.names(
    subset(cooks_d_mixed_players, d>4/nrow(cooks_d_mixed_players))))

cooks_mix_obs_rows <- as.numeric(
  row.names(
    subset(cooks_d_mixed, d>4/nrow(cooks_d_mixed))))

#### DATA MANIPULATION FOR FINAL PLOT WITH ALL POTENTIAL OUTLIERS ####

#create a list with all the number of rows identified by the methods
three_sd_td 
three_sd_hs
three_sd_acc
three_sd_pl
three_sd_pls
three_sd_tri
three_sd_bu
three_sd_gu
z_score_td
z_score_acc
z_score_pl
z_score_pls
z_score_tri
leverage_rows
sdr_rows
r_squared_rows
aic_player_id
aic_obs_rows
dfbetas_int_rows
dfbetas_td_rows
dfbetas_hs_rows
dfbetas_acc_rows
dfbetas_pl_rows
dfbetas_pls_rows
dfbetas_gu_rows
dfbetas_bu_rows
dfbetas_mix_players_int_rows
dfbetas_mix_players_td_rows
dfbetas_mix_players_hs_rows
dfbetas_mix_players_acc_rows
dfbetas_mix_players_pl_rows
dfbetas_mix_players_pls_rows
dfbetas_mix_players_gu_rows
dfbetas_mix_players_bu_rows
dfbetas_mix_obs_int_rows
dfbetas_mix_obs_td_rows
dfbetas_mix_obs_hs_rows
dfbetas_mix_obs_acc_rows
dfbetas_mix_obs_pl_rows
dfbetas_mix_obs_pls_rows
dfbetas_mix_obs_gu_rows
dfbetas_mix_obs_bu_rows
dffits_rows
dffits_mix_pl_rows
dffits_mix_obs_rows
cooks_rows
cooks_mix_pl_rows
cooks_mix_obs_rows


#create a list with all the rows identified as outliers
my_list <- list(
     three_sd_td = three_sd_td, 
     three_sd_hs = three_sd_hs,
     three_sd_acc = three_sd_acc,
     three_sd_pl = three_sd_pl,
     three_sd_pls = three_sd_pls,
     three_sd_tri = three_sd_tri,
     three_sd_bu = three_sd_bu,
     three_sd_gu = three_sd_gu,
     z_score_td = z_score_td,
     z_score_acc = z_score_acc,
     z_score_pl = z_score_pl,
     z_score_pls = z_score_pls,
     z_score_tri = z_score_tri,
     leverage_rows = leverage_rows,
     sdr_rows = sdr_rows,
     r_squared_rows = r_squared_rows,
     aic_obs_rows = aic_obs_rows,
     dfbetas_int_rows = dfbetas_int_rows,
     dfbetas_td_rows = dfbetas_td_rows,
     dfbetas_hs_rows = dfbetas_hs_rows,
     dfbetas_acc_rows = dfbetas_acc_rows,
     dfbetas_pl_rows = dfbetas_pl_rows,
     dfbetas_pls_rows = dfbetas_pls_rows,
     dfbetas_gu_rows = dfbetas_gu_rows,
     dfbetas_bu_rows = dfbetas_bu_rows,
     dfbetas_mix_obs_int_rows = dfbetas_mix_obs_int_rows,
     dfbetas_mix_obs_td_rows = dfbetas_mix_obs_td_rows,
     dfbetas_mix_obs_hs_rows = dfbetas_mix_obs_hs_rows,
     dfbetas_mix_obs_acc_rows = dfbetas_mix_obs_acc_rows,
     dfbetas_mix_obs_pl_rows = dfbetas_mix_obs_pl_rows,
     dfbetas_mix_obs_pls_rows = dfbetas_mix_obs_pls_rows,
     dfbetas_mix_obs_gu_rows = dfbetas_mix_obs_gu_rows,
     dfbetas_mix_obs_bu_rows = dfbetas_mix_obs_bu_rows,
     dffits_rows = dffits_rows,
     dffits_mix_obs_rows = dffits_mix_obs_rows,
     cooks_rows = cooks_rows,
     cooks_mix_obs_rows = cooks_mix_obs_rows)
#columns with the # represent outliers at the player level and hence are
#excluded from the analysis at the observation level

#use a for loop to make every element of the list of the same length using NAs
for (i in seq_along(my_list)) {
  my_vec <- rep(NA, nrow(ssg))
  my_vec[my_list[[i]]] <- my_list[[i]]
  my_list[[i]] <- my_vec
}
my_list

#convert the list to a data frame
my_outliers <- do.call(cbind, my_list)
#remove all rows that contain only NAs
my_outliers <- my_outliers[which(rowSums(my_outliers, na.rm=T) > 0),]
#make sure the data frame is actually a data frame
my_outliers_df <- as.data.frame(my_outliers)
#count the number of times each row was detected as outlier
my_outliers_df$total_count <- as.vector(rowSums(!is.na(my_outliers_df)))

#combine all the rows identified as outleirs in a single vector
all_outliers <- c(
  three_sd_td,
three_sd_hs,
three_sd_acc,
three_sd_pl,
three_sd_pls,
three_sd_tri,
three_sd_bu,
three_sd_gu,
z_score_td,
z_score_acc,
z_score_pl,
z_score_pls,
z_score_tri,
leverage_rows,
sdr_rows,
r_squared_rows,
aic_obs_rows,
dfbetas_int_rows,
dfbetas_td_rows,
dfbetas_hs_rows,
dfbetas_acc_rows,
dfbetas_pl_rows,
dfbetas_pls_rows,
dfbetas_gu_rows,
dfbetas_bu_rows,
dfbetas_mix_obs_int_rows,
dfbetas_mix_obs_td_rows,
dfbetas_mix_obs_hs_rows,
dfbetas_mix_obs_acc_rows,
dfbetas_mix_obs_pl_rows,
dfbetas_mix_obs_pls_rows,
dfbetas_mix_obs_gu_rows,
dfbetas_mix_obs_bu_rows,
dffits_rows,
dffits_mix_obs_rows,
cooks_rows,
cooks_mix_obs_rows)

#get the unique values and sort them in ascending order
unique_outliers <- sort(unique(all_outliers))

#add the unique rows to the main data frame to create a plot
my_outliers_df$row_outlier <- unique_outliers

#create a copy of the data frame for the final plot
outliers_df_copy <- data.frame(row_outlier = my_outliers_df$row_outlier, 
                               count = my_outliers_df$total_count)
#order count in decreasing order
outliers_df_copy <- outliers_df_copy[order(outliers_df_copy$count, decreasing = TRUE),]

#split the data frame into three groups (100/3=33) for the final plot
outliers_df_copy$group <- c(rep(1, 33), rep(2, 33), rep(3, 34))
#check the data frame for the final plot
outliers_df_copy

#create the final plot to show how many times each row has been detected as 
#potential outlier
final_outliers_plot <- ggplot() +
  geom_col(data = outliers_df_copy, aes(x = reorder(row_outlier, -count),
                                        y = count)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Observation id") +
  ylab("Count (n)") +
  facet_wrap(~group, ncol = 1, scales = "free_x") +
  labs(title = "Observations detected as potential outliers")


### Player id outliers
all_player_outliers <- c(
aic_player_id,
dfbetas_mix_players_int_rows,
dfbetas_mix_players_td_rows,
dfbetas_mix_players_hs_rows,
dfbetas_mix_players_acc_rows,
dfbetas_mix_players_pl_rows,
dfbetas_mix_players_pls_rows,
dfbetas_mix_players_gu_rows,
dfbetas_mix_players_bu_rows,
dffits_mix_pl_rows,
cooks_mix_pl_rows)

#create a data frame for ggplot 
all_player_outliers_df <- as.data.frame(all_player_outliers)

#create a graph showing the frequency of detection of player id
final_player_outliers_plot <- ggplot(all_player_outliers_df) + 
  geom_bar(aes(x = forcats::fct_infreq(as.factor(all_player_outliers)))) +
  xlab("Player id") + ylab("Count (n)") +
  labs(title = "Players detected as potential outliers")
