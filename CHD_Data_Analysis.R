library(tidyverse)
library(qwraps2)
library(FactoMineR)

dat <- read_csv("dat/chddata.csv")

dat$bmi[sample(1:nrow(dat), size = 4)] <- NA
dat$alcoh[sample(1:nrow(dat), size = 37)] <- NA
dat$choltot <- dat$choltot + rnorm(nrow(dat), 0, 2)
dat$choltot[dat$smoke == 1] <- round(dat$choltot[dat$smoke == 1] + rchisq(nrow(dat[dat$smoke == 1, ]), 10), 2)
dat$weight <- round(dat$weight + rnorm(nrow(dat), 0, 2))
dat$height <- round(dat$height + rnorm(nrow(dat), 0, 1))

dat$choltot <- round(0.05 * dat$hdl + dat$choltot, 2)

write_csv(dat, "dat/chddata_dsi.csv")

#########

dat <- read_csv("dat/chddata_dsi.csv")

# Check Data
colnames(dat)
dat
head(dat)
tail(dat)

object.size(dat)
View(dat)

######
dat <- dat %>% 
  mutate(female = factor(gender)) %>% 
  select(-gender, -X1)

######  Check NAs
sum(is.na(dat))
which(is.na(dat), arr.ind = TRUE)
colnames(dat)[5:6]
dat %>% filter(!complete.cases(.))

dat$alcoh[is.na(dat$alcoh)] <- mean(dat$alcoh, na.rm = TRUE)
dat$bmi[is.na(dat$bmi)] <- mean(dat$bmi, na.rm = TRUE)

sum(is.na(dat))

write_csv(dat, "dat/chddata_dsi_cleaned.csv")
dat <- read_csv("dat/chddata_dsi_cleaned.csv")
dat
nrow(dat)
# Univariate 
colnames(dat)
qplot(bmi, data = dat, fill = I("darkgreen"), color = I("black"))
qplot(choltot, data = dat, fill = I("darkred"), color = I("black"))
qplot(age, data = dat, fill = I("dodgerblue"), color = I("black"))
qplot(female, data = dat)
qplot(hdl, data = dat)
qplot(sysbp, data = dat)
qplot(factor(incchd), data = dat)

data_summary <- 
  list("Total Chol." = 
         list("min" = ~min(choltot),
              "max" = ~max(choltot),
              "mean (sd)" = ~qwraps2::mean_sd(choltot)),
       "Inc. CHD" = list("% (n)" = ~qwraps2::perc_n(incchd)),
       "HDL" = 
         list("min" = ~min(hdl),
              "max" = ~max(hdl),
              "mean (sd)" = ~qwraps2::mean_sd(hdl)),
       "BMI" = 
         list("min" = ~min(bmi),
              "max" = ~max(bmi),
              "mean (sd)" = ~qwraps2::mean_sd(bmi)),
       "Age" = 
         list("min" = ~min(age),
              "median" = ~median(age),
              "max" = ~max(age)),
       "sysbp" = 
         list("min" = ~min(sysbp),
              "max" = ~max(sysbp),
              "mean (sd)" = ~qwraps2::mean_sd(sysbp)),
       "diabp" = 
         list("min" = ~min(diabp),
              "max" = ~max(diabp),
              "mean (sd)" = ~qwraps2::mean_sd(diabp)),
       "height" = 
         list("min" = ~min(height),
              "max" = ~max(height),
              "mean (sd)" = ~qwraps2::mean_sd(height)),
       "weight" = 
         list("min" = ~min(weight),
              "max" = ~max(weight),
              "mean (sd)" = ~qwraps2::mean_sd(weight)))

orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")
summary_table(dat, data_summary)

# Find Average Total Cholesterol by gender

dat %>% group_by(female) %>% 
  summarize(mean_choltot = mean(choltot))

########### K Means
colnames(dat)
clust_cols <- c("choltot", "hdl", "alcoh", "bmi", "trig", "age", 
                "diabetes", "diabp", "sysbp", "height", 
                "weight", "smoke", "female")
fit <- kmeans(dat_clust %>% select(clust_cols), 
              centers = 5)

library(factoextra)
factoextra::fviz_cluster(fit, data = dat)

wss <- function(k) {
  kmeans(dat_clust, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fit <- kmeans(dat_clust, 
              centers = 2)
factoextra::fviz_cluster(fit, data = dat)

dat$cluster <- fit$cluster

dat %>% group_by(cluster) %>% 
  summarize(mean_incchd = mean(incchd))

dat %>% group_by(cluster) %>% 
  summarize_all(.funs = mean)

library(broom)
kclusts <- tibble(k = 1:6) %>%
  mutate(
    kclust = map(k, ~kmeans(dat_clust, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, dat_clust)
  )

kclusts

clusters <- kclusts %>%
  unnest(tidied)

assignments <- kclusts %>% 
  unnest(augmented)

clusterings <- kclusts %>%
  unnest(glanced, .drop = TRUE)

p1 <- ggplot(assignments, aes(hdl)) +
  geom_boxplot(aes(y = hdl, fill = .cluster)) + 
  facet_wrap(~ k)
p1

p2 <- ggplot(assignments, aes(hdl, choltot)) +
  geom_point(aes(fill = .cluster), alpha = 0.75, pch = 21, color = "gray19") + 
  facet_wrap(~ k)
p2


# Bootstrap Confidence Interval for Mean Difference in choltot between genders

dat_nest <- dat_clust %>% 
  dplyr::group_by(cluster) %>% 
  tidyr::nest()

dat_nest
purrr::map(dat_nest, head)

boot_mean <- function(d, i) {
  mean(d[i])
}

dat_nest <- dat_nest %>%  
  dplyr::mutate(booted = purrr::map(.x = data, 
                                    ~ boot::boot(data = .x$incchd,
                                                 statistic = boot_mean,
                                                 R = 5000,
                                                 stype = "i")))

plots <- purrr::map(.x = dat_nest$booted, 
                    ~ plot(.x))

dat_nest <- dat_nest %>% 
  dplyr::mutate(booted_ci = purrr::map(.x = booted, 
                                       ~ boot::boot.ci(.x,
                                                       conf = 0.95, 
                                                       type = "bca")))

purrr::map(.x = dat_nest$booted_ci, 
           ~ print(.x))

str(dat_nest$booted_ci[[1]])
dat_booted <- dat_nest %>%
  # Add columns
  dplyr::mutate(statistic = purrr::map(.x = booted_ci, # The list-column containing <S3 bootci> objects
                                       ~ .x$t0), # The point estimate
                lower_ci = purrr::map(.x = booted_ci,
                                      ~ .x$bca[[4]]), # The value of the lower 2.5% limit
                upper_ci = purrr::map(.x = booted_ci,
                                      ~ .x$bca[[5]])) %>% # The value of teh upper 97.5% limit
  # Drop the list-columns (no longer needed)
  dplyr::select(-data, -booted, -booted_ci) %>%
  # Unnest the dataframe
  tidyr::unnest()
dat_booted
knitr::kable(x = dat_booted, digits = 1, col.names = c("Cluster", "Mean Chol. Total", "Lower 2.5%", "Upper 97.5%"))

################
colnames(dat)
fit_choltot_glm <- glm(incchd ~ choltot, data = dat, family = binomial)
fit_selected_glm <- glm(incchd ~ choltot + trig + hdl + weight, data = dat, family = binomial)
fit_full_glm <- glm(incchd ~ choltot + trig + hdl + weight + smoke + age + height + sysbp + diabp + female + diabetes, data = dat, family = binomial)

tidy(fit_full_glm)
augment(fit)
glance(fit_choltot_glm)
glance(fit_selected_glm)
glance(fit_full_glm)

fit_clust_glm <- glm(incchd ~ cluster, 
                          data = dat, family = binomial)
fit_clust_full_glm <- glm(incchd ~ cluster + choltot + trig + hdl + weight + smoke + age + height + sysbp + diabp + female + diabetes, 
                     data = dat, family = binomial)
tidy(fit_clust_glm)
tidy(fit_clust_full_glm)
glance(fit_clust_glm)
glance(fit_clust_full_glm)

###########

pc_results <- PCA(dat_clust, ncp = 2)
fviz_screeplot(pc_results, addlabels = TRUE)
fviz_pca_var(pc_results, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_contrib(pc_results, choice = "var", axes = 1, top = 10)
fviz_pca_ind(pc_results, habillage = dat$incchd,  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE)







nrow(dat)
