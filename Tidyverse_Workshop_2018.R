## ----setup, include=FALSE------------------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", fig.height = 4, 
                      cache = TRUE, warning = FALSE)
library(tidyverse)

## ---- echo = FALSE, fig.align='center'-----------------------------------
knitr::include_graphics('Figures/tidy_pipeline.jpg')

## ---- echo = FALSE, fig.align='center'-----------------------------------
knitr::include_graphics('Figures/tidy_data.png')

## ---- echo = FALSE, fig.align='center', out.height="275px", out.width="175px", fig.asp=TRUE----
knitr::include_graphics('Figures/r4ds.png')

## ---- eval = FALSE-------------------------------------------------------
## install.packages("tidyverse")
## install.packages("rmarkdown")

## ------------------------------------------------------------------------
library(tidyverse)

## ------------------------------------------------------------------------
imdb <- read_csv("dat/movie_metadata.csv")
imdb <- unique(imdb)
nrow(imdb)

## ------------------------------------------------------------------------
imdb

## ------------------------------------------------------------------------
head(imdb)

## ------------------------------------------------------------------------
colnames(imdb)

## ---- echo = FALSE, fig.align='center'-----------------------------------
knitr::include_graphics('Figures/ggplot.png')

## ------------------------------------------------------------------------
library(ggplot2)

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_histogram(aes(x = imdb_score))

## ------------------------------------------------------------------------
?geom_histogram

## ------------------------------------------------------------------------
imdb$recent <- imdb$title_year > 2000
ggplot(imdb) + 
  geom_histogram(aes(x = imdb_score, fill = recent))

## ------------------------------------------------------------------------
imdb$recent <- imdb$title_year > 2000
ggplot(imdb) + 
  geom_histogram(aes(x = imdb_score, fill = recent), 
                 color = "black", bins = 30)

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_histogram(aes(x = imdb_score, fill = recent), 
                 color = "black", bins = 30) + 
  xlab("IMDB Score") + 
  ylab("Count") + 
  ggtitle("Comparing IMDB Scores for Recent vs Old Movies")

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_histogram(aes(x = imdb_score, fill = recent), 
                 color = "black", bins = 30, alpha = 0.4) + 
  xlab("IMDB Score") + 
  ylab("Count") + 
  ggtitle("Comparing IMDB Scores for Recent vs Old Movies")

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_density(aes(x = imdb_score, fill = recent), 
               color = "black", alpha = 0.4) + 
  xlab("IMDB Score") + 
  ylab("Density") + 
  ggtitle("Comparing IMDB Scores for Recent vs Old Movies")

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_point(aes(x = budget, y = imdb_score))

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_point(aes(x = log(budget), y = imdb_score))

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_point(aes(x = log(budget), y = imdb_score), color = "green4")

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_point(aes(x = log(budget), y = imdb_score), 
             pch = 21, fill = "green4", 
             color = "black", alpha = 0.8)

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_histogram(aes(x = budget))

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_histogram(aes(x = log(budget)))

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_histogram(aes(x = log(budget)), 
                 color = "orange", fill = "purple", bins = 30) + 
  ggtitle("Exercise 2") + 
  xlab("The Log of the Budget") + 
  ylab("Frequency Count")

## ------------------------------------------------------------------------
imdb$recent <- imdb$title_year > 2000
ggplot(imdb) + 
  geom_histogram(aes(x = log(budget), fill = recent), 
                 color = "black")

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_point(aes(x = log(budget), y = imdb_score, fill = recent), 
             pch = 21, color = "black")

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_boxplot(aes(y = imdb_score, x = recent, fill = recent)) + 
  coord_flip()

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_qq(aes(sample = imdb_score)) + 
  geom_qq_line(aes(sample = imdb_score))

## ------------------------------------------------------------------------
ggplot(imdb) + 
  geom_point(aes(x = log(budget), y = imdb_score, fill = recent), 
             pch = 21, color = "black") +
  geom_smooth(aes(x = log(budget), y = imdb_score, color = recent), 
              method = "lm")

## ------------------------------------------------------------------------
ggplot(imdb[imdb$recent, ]) +
  geom_boxplot(aes(y = imdb_score, fill = factor(title_year), 
                   x = factor(title_year)))

## ------------------------------------------------------------------------
# Print species means of Sepal Width
data(iris)
iris <-  filter(iris, Species!="setosa")
iris <- select(iris, c(Sepal.Width, Species)) 
iris <- group_by(iris, Species)
species_means <- summarise(iris, mean(Sepal.Width))
print(species_means)

## ------------------------------------------------------------------------
# Prints species means, does not save anything
# Original data.frame iris is unaffected
data(iris)
iris %>% filter(Species!="setosa") %>%
    select(c(Sepal.Width, Species)) %>% 
    group_by(Species) %>%
    summarise(mean(Sepal.Width))

## ------------------------------------------------------------------------
# To save the results instead
species_means <- iris %>% 
    filter(Species!="setosa") %>%
    select(c(Sepal.Width, Species)) %>% 
    group_by(Species) %>%
    summarise(mean(Sepal.Width))

## ------------------------------------------------------------------------
species_means

## ---- eval=FALSE---------------------------------------------------------
## data(iris)
## iris %>% summarise(mean_sepal_width = mean(Sepal.Width),
##                    min_sepal_width = min(Sepal.Width))

## ------------------------------------------------------------------------
data(iris)

iris %>% 
    filter(Species == "virginica") %>%
    summarise(mean_sepal_width = mean(Sepal.Width))

## ------------------------------------------------------------------------
data(iris)
iris %>% 
    filter(Species == "virginica") %>%
    summarise(min_sepal_width = min(Sepal.Width),
              med = median(Sepal.Width), maximum = max(Sepal.Width),
              stdev = sd(Sepal.Width))

## ------------------------------------------------------------------------
data(iris)
iris %>% 
    group_by(Species) %>%
    summarise(min_sepal_width = min(Sepal.Width),
              med = median(Sepal.Width), maximum = max(Sepal.Width),
              stdev = sd(Sepal.Width))

## ---- eval = FALSE-------------------------------------------------------
## imdb %>%
##   group_by(actor_1_name) %>%
##   summarize(n())

## ------------------------------------------------------------------------
imdb %>% 
  group_by(actor_1_name) %>% 
  summarize(n())

## ------------------------------------------------------------------------
imdb %>% 
  group_by(actor_1_name) %>% 
  summarize(n_movies = n()) %>% 
  arrange(desc(n_movies))

## ------------------------------------------------------------------------
imdb %>% 
  group_by(actor_1_name) %>% 
  summarize(mean_imdb_score = mean(imdb_score)) %>% 
  arrange(desc(mean_imdb_score))

## ------------------------------------------------------------------------
imdb %>% 
  group_by(actor_1_name) %>% 
  summarize(mean_imdb_score = mean(imdb_score), n_movies = n()) %>% 
  filter(n_movies > 5) %>% 
  top_n(10, mean_imdb_score) %>% 
  arrange(desc(mean_imdb_score))

## ------------------------------------------------------------------------
actor <- "Harrison Ford"
filter(imdb, actor_1_name == actor) %>% nrow

## ------------------------------------------------------------------------
imdb %>% 
  filter(actor_1_name == actor) %>% 
  select(imdb_score)

## ------------------------------------------------------------------------
ggplot(imdb %>% filter(actor_1_name == actor)) + 
  geom_histogram(aes(x = imdb_score), 
                 fill = "darkred", color = "black", bins = 15)

## ------------------------------------------------------------------------
imdb %>% 
  filter(actor_1_name %in% actor) %>% 
  group_by(genres) %>% 
  summarize(mean_score = mean(imdb_score), n_movies = n())

## ------------------------------------------------------------------------
imdb %>% 
  filter(actor_1_name %in% actor) %>% 
  transmute(action = str_detect(genres, "Action"))

## ------------------------------------------------------------------------
imdb %>%
  filter(actor_1_name %in% actor) %>% 
  mutate(action = str_detect(genres, "Action")) %>% 
  group_by(action) %>% 
  summarize(mean(imdb_score))

## ------------------------------------------------------------------------
dat <- read_csv("dat/chddata_dsi.csv")

## ------------------------------------------------------------------------
dat

## ------------------------------------------------------------------------
dat <- dat %>% 
  mutate(female = factor(gender)) %>% 
  select(-gender, -X1)

## ------------------------------------------------------------------------
######  Check NAs
sum(is.na(dat))
dat %>% filter(!complete.cases(.))

dat$alcoh[is.na(dat$alcoh)] <- mean(dat$alcoh, na.rm = TRUE)
dat$bmi[is.na(dat$bmi)] <- mean(dat$bmi, na.rm = TRUE)
sum(is.na(dat))

## ------------------------------------------------------------------------
write_csv(dat, "dat/chddata_dsi_cleaned.csv")
dat <- read_csv("dat/chddata_dsi_cleaned.csv")
dat

## ---- echo=FALSE, results='asis'-----------------------------------------
library(qwraps2)
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
              "mean (sd)" = ~qwraps2::mean_sd(bmi)))

orig_opt <- options()$qwraps2_markup
options(qwraps2_markup = "markdown")
summary_table(dat, data_summary)

## ------------------------------------------------------------------------
qplot(choltot, data = dat, fill = I("darkred"), color = I("black"), xlab = "Chol. Total", main = "Distribution of Total Cholesterol")

## ------------------------------------------------------------------------
qplot(x = factor(incchd), y = choltot, data = dat, geom = "boxplot", fill = factor(incchd))

## ------------------------------------------------------------------------
qplot(x = factor(incchd), y = log(trig), data = dat, geom = "boxplot", fill = factor(incchd))

## ------------------------------------------------------------------------
a <- 1:10
purrr::map(a, function(x) x^2)

## ------------------------------------------------------------------------
a <- 1:10
purrr::map_dbl(a, function(x) x^2)

## ------------------------------------------------------------------------
a <- 1:10
purrr::map_dbl(a, ~ .^2)

## ----bootstrap-----------------------------------------------------------
boot_mean <- function(d, i) {
  mean(d[i])
}

## ------------------------------------------------------------------------
dat_booted <- dat %>% 
  dplyr::group_by(female) %>% 
  tidyr::nest() 
dat_booted

## ------------------------------------------------------------------------
dat_booted <- dat_booted %>%
  dplyr::mutate(booted = purrr::map(.x = data, 
                                    ~ boot::boot(data = .x$incchd,
                                                 statistic = boot_mean,
                                                 R = 5000,
                                                 stype = "i")))
dat_booted

## ------------------------------------------------------------------------
plots <- purrr::map(.x = dat_booted$booted, 
                    ~ plot(.x))

## ------------------------------------------------------------------------
dat_booted <- dat_booted %>% 
  dplyr::mutate(booted_ci = purrr::map(.x = booted, 
                                       ~ boot::boot.ci(.x,
                                                       conf = 0.95, 
                                                       type = "bca")))
dat_booted

## ------------------------------------------------------------------------
dat_booted <- dat_booted %>% 
    dplyr::mutate(statistic = purrr::map(.x = booted_ci, 
                                       ~ .x$t0), 
                lower_ci = purrr::map(.x = booted_ci,
                                      ~ .x$bca[[4]]), 
                upper_ci = purrr::map(.x = booted_ci,
                                      ~ .x$bca[[5]])) %>% 
  dplyr::select(-data, -booted, -booted_ci) %>%
  tidyr::unnest()
dat_booted

## ------------------------------------------------------------------------
boot_mean <- function(d, i) {
  mean(d[i])
}

dat_booted <- dat %>% 
  dplyr::group_by(female) %>% 
  tidyr::nest() %>%  
  dplyr::mutate(booted = purrr::map(.x = data, 
                                    ~ boot::boot(data = .x$incchd,
                                                 statistic = boot_mean,
                                                 R = 5000,
                                                 stype = "i"))) %>%
  dplyr::mutate(booted_ci = purrr::map(.x = booted, 
                                       ~ boot::boot.ci(.x,
                                                       conf = 0.95, 
                                                       type = "bca"))) %>% 
  dplyr::mutate(statistic = purrr::map(.x = booted_ci, 
                                       ~ .x$t0), 
                lower_ci = purrr::map(.x = booted_ci,
                                      ~ .x$bca[[4]]), 
                upper_ci = purrr::map(.x = booted_ci,
                                      ~ .x$bca[[5]])) %>% 
  dplyr::select(-data, -booted, -booted_ci) %>%
  tidyr::unnest()

## ---- results='asis'-----------------------------------------------------
knitr::kable(x = dat_booted, digits = 3, col.names = c("Female", "Est. CHD Incidence", "Lower 2.5%", "Upper 97.5%"), format = "html", caption = "95% CI for CHD Incidence by Gener")

## ------------------------------------------------------------------------
library(broom)
fit_glm <- glm(incchd ~ ., data = dat, family = "binomial")
tidy(fit_glm)

## ---- results="asis"-----------------------------------------------------
knitr::kable(tidy(fit_glm) %>% 
               filter(p.value < 0.05, term != "(Intercept)") %>% 
               select(-statistic) %>% 
               transmute(Source = term, Estimate = estimate, SE = std.error, P = p.value), 
             format = "markdown",
             digits = 4)

## ----pressure------------------------------------------------------------
plot(pressure, main = "Plot Title", pch = 23, col = "blue", bg = "blue")

