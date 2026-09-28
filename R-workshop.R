# SRI Quantitative Methods Bootcamp: R workshop
# Adapted from Sonmez's "A Gentle Introduction to R" in SOCS0100.
# Open Bootcamp.Rproj, then run this script from top to bottom.
# Requires R 4.1 or later (for the native pipe |>).
# Install once in the console, if needed:
# install.packages(c("readr", "dplyr", "ggplot2"))
# No internet connection is needed after installation.

## ---- r-packages
library(readr)    # import and export data
library(dplyr)    # transform and summarise
library(ggplot2)  # visualise

## ---- r-objects
hours <- 3
minutes <- hours * 60
minutes

## ---- r-vectors
scores <- c(60, 70, 80, NA)
scores[1]           # first element
scores >= 70        # a logical test
length(scores)      # number of elements

## ---- r-functions
mean(scores)                # NA: a value is missing
mean(scores, na.rm = TRUE)   # mean of observed scores
sum(is.na(scores))          # how many are missing?

## ---- r-import
wdi <- read_csv("WDI_Data.csv", show_col_types = FALSE)
dim(wdi)  # rows, then columns

## ---- r-inspect
wdi |>
  select(countryname, continent, gdpcap, lifexp_tot) |>
  head(3)

## ---- r-column
mean(wdi$gdpcap, na.rm = TRUE)

## ---- r-filter
europe <- wdi |>
  filter(continent == "Europe") |>
  select(countryname, gdpcap, lifexp_tot)

## ---- r-mutate
plot_data <- wdi |>
  filter(!is.na(gdpcap), !is.na(lifexp_tot), gdpcap > 0) |>
  mutate(gdp_thousands = gdpcap / 1000)

## ---- r-summary
gdp_by_continent <- wdi |>
  group_by(continent) |>
  summarise(
    n_gdp = sum(!is.na(gdpcap)),
    mean_gdp = mean(gdpcap, na.rm = TRUE),
    .groups = "drop"
  )
gdp_by_continent

## ---- r-plot
gdp_plot <- ggplot(plot_data,
    aes(x = gdpcap, y = lifexp_tot,
        colour = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(x = "GDP per capita (2015 US$, log scale)",
       y = "Life expectancy (years)",
       colour = "Continent") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
print(gdp_plot)

## ---- r-model
life_model <- lm(lifexp_tot ~ log(gdpcap), data = plot_data)
coef(life_model)     # intercept and slope
confint(life_model)  # 95% confidence intervals

## ---- r-save
dir.create("outputs", showWarnings = FALSE)
write_csv(gdp_by_continent, "outputs/gdp_by_continent.csv")
ggsave("outputs/gdp_life_expectancy.png", gdp_plot,
       width = 8, height = 5, dpi = 150)

## ---- r-practice-solutions
# Exercise 1: replace mean() with median().
median(scores, na.rm = TRUE)  # 70

# Exercise 2: mean life expectancy and number observed in Europe.
europe_life <- wdi |>
  filter(continent == "Europe") |>
  summarise(
    n_life = sum(!is.na(lifexp_tot)),
    mean_life = mean(lifexp_tot, na.rm = TRUE)
  )
europe_life  # 46 observed; mean approximately 79.24 years

# Exercise 3: one mean per continent; count observations used.
life_by_continent <- wdi |>
  group_by(continent) |>
  summarise(
    n_life = sum(!is.na(lifexp_tot)),
    mean_life = mean(lifexp_tot, na.rm = TRUE),
    .groups = "drop"
  )
life_by_continent

# These are unweighted country/economy means: each observed row counts equally.
# Excluding missing observations does not fix possible missing-data bias.
