
#####################################
# Exploratory data analysis reports #
#               and                 #
#           Twirly text             #
#####################################



# Load required libraries
library(skimr)
library(psych)
library(corrplot)
library(PerformanceAnalytics)
library(GGally)
library(DataExplorer)
library(summarytools)
library(SmartEDA)
library(janitor)
library(inspectdf)
library(tidyverse)
library(tidyquant)
library(gt)
library(GWalkR)

# make a quick dataset
# mt cars is weird as has index name which I want to convert to a column varible
# I also want to create a make group, the word function is pretty cool for this
# in second version also made make a factor

mtcars

data <- as.tibble(mtcars, rownames = 'cars') |>
  mutate(make = word(cars, 1))

data <- mtcars |>
  rownames_to_column(var = 'cars') |>
  mutate(make = as.factor(word(cars, 1)))

# Skimr: Summary of the dataset ----
skim(data)

# also can fit into a gt table
skim(data) |> 
  gt()

# Psych: Descriptive statistics ----
describe(data)

# also gt table
describe(data) |> 
  gt()

# summary
summary(data)


# Corrplot: Correlation matrix visualization ----
corrplot(
  cor(data[, 2:12]),
  method = "circle",
  addCoef.col = 'grey',
  order = 'hclust',
  rect.col = 'blue',
  addrect = 2
)

#  PerformanceAnalytics: Correlation matrix with scatterplots and histograms ----
chart.Correlation(
  data[, 2:12],
  histogram = TRUE,
  pch = 19
)

# DataExplorer: Generate a full EDA report ----
create_report(
  data,
  output_file = "DataExplorer_Report.html"
)

# Summarytools: Summary table for the dataset ----
dfSummary(data) |>
  stview()

#  SmartEDA: Generate a detailed EDA report in HTML ----
ExpReport(
  data,
  op_file = "SmartEDA_Report.html"
)

#  Janitor: Frequency table for a categorical variable with Tabyl ----
data |>
  tabyl(make) |>
  adorn_totals("row") |>
  adorn_pct_formatting()

data |>
  tabyl(make) |>
  adorn_totals("row") |>
  adorn_pct_formatting() |>
  gt()

# 10. Inspectdf: Visualize missing values in the dataset ----

# inspect nas
inspect_na(data)

inspect_na(data) |>
  show_plot()

# inspect categories
inspect_cat(data) 

inspect_cat(data) |> 
  show_plot()

# inspect correlations
inspect_cor(data)

inspect_cor(data) |>
  show_plot() + theme_minimal()

# with a bit of love, I think that last chart could be a really good alternative
# to correlation matrix

# GWalkR - 

gwalkr(data)








######################################





# also a little play with geomtextpath

library(geomtextpath)
library(NHSRdatasets)

# get basic dataset
# add some prettier label categories for type
dat <- ae_attendances |>
  filter(org_code == 'RF4') |>
  mutate(lab = case_when (type == 1 ~ 'Emergency',
                          type == 2 ~ 'Walk in',
                          .default = 'Other'),
         cov_start = as.Date("2018-03-01"))

# basic chart facet by type
ggplot(dat, aes(
  x = period,
  y = attendances, color = lab
)) +
  geom_line() +
  facet_wrap(~lab,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# add in smoothing line
ggplot(dat, aes(x = period, 
                y = attendances, 
                color = lab)) +
  geom_line(
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_smooth() +
  facet_wrap(~lab,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# remove SE from smoothing line
ggplot(dat, aes(x = period, 
                y = attendances, 
                color = lab)) +
  geom_line(
    alpha = 0.5,
    linetype = "dashed"
  ) +
  geom_smooth(se = FALSE) +
  facet_wrap(~lab,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# add a prettier labeled smoothing line
ggplot(dat, aes(x = period, 
                y = attendances, 
                color = type)) +
  geom_line(
    alpha = 0.5,
    linetype = "dashed"
  ) +
  facet_wrap(~lab,
    scales = "free_y"
  ) +
  geom_labelsmooth(aes(label = lab),
                   text_smoothing = 50, 
                   fill = "#F6F6FF",
                   method = "loess", 
                   formula = y ~ x,
                   size = 4, 
                   linewidth = 1, 
                   boxlinewidth = 0.3)+
  theme_minimal() +
  theme(legend.position = "none")

# add another line annotated line for covid
ggplot(dat, aes(x = period, 
                y = attendances, 
                color = type)) +
  geom_line(
    alpha = 0.7,
    linetype = "dotted",
    size = 1
  ) +
  facet_wrap(~lab,
    scales = "free_y"
  ) +
  geom_labelsmooth(aes(label = lab),
    text_smoothing = 50, 
    fill = "#F6F6FF",
    method = "loess", formula = y ~ x,
    size = 4, linewidth = 1, boxlinewidth = 0.3
  ) +
  geom_textvline(
    xintercept = dat$cov_start, 
    label = "Start of covid", 
    hjust = 0.3,
    linetype = 2, 
    vjust = 0.3, 
    color = "blue4"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# a little tweak to the line label
ggplot(dat, aes(x = period, 
                y = attendances, 
                color = type)) +
  geom_line(
    alpha = 0.7,
    linetype = "dotted",
    size = 1
  ) +
  facet_wrap(~lab,
             scales = "free_y"
  ) +
  geom_labelsmooth(aes(label = lab),
                   text_smoothing = 50, 
                   fill = "#F6F6FF",
                   method = "loess", 
                   formula = y ~ x,
                   size = 4, 
                   linewidth = 1, 
                   boxlinewidth = 0.3, 
                   hjust = -0.01
  ) +
  geom_textvline(
    xintercept = dat$cov_start, 
    label = "Start of covid", 
    hjust = 0.3,
    linetype = 2, 
    vjust = 0.3, 
    color = "blue4"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# another example from the package

df <- data.frame(Activity = c("Work", "Play"), Happiness = c(0.5, 0.7))

ggplot(df, aes(Activity, Happiness)) + 
  geom_col(fill = "gold", color = "gray50") +
  theme_minimal()

# add a curvy line
ggplot(df, aes(Activity, Happiness)) + 
  geom_col(fill = "gold", color = "gray50") + 
  geom_textcurve(data = data.frame(x = 1, 
                                   xend = 2, 
                                   y = 0.72, 
                                   yend = 0.52), 
                 aes(x, y, 
                     xend = xend, 
                     yend = yend), 
                 hjust = 0.35, 
                 ncp = 20,
                 curvature = -0.8, 
                 label = "significant difference") +
  geom_point(aes(y = Happiness + 0.02)) +
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()

# instead of points we can have arrows
ggplot(df, aes(Activity, Happiness)) + 
  geom_col(fill = "gold", color = "gray50") + 
  geom_textcurve(data = data.frame(x = 1, 
                                   xend = 2, 
                                   y = 0.72, 
                                   yend = 0.52), 
                 aes(x, y, 
                     xend = xend, 
                     yend = yend), 
                 hjust = 0.35, 
                 ncp = 20,
                 curvature = -0.8, 
                 arrow = arrow(length = unit(2.5, 'mm'),  
                               ends = 'both', 
                               type = 'open'),
                 label = "significant difference") +
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()

# tweak to a one ended arrow
ggplot(df, aes(Activity, Happiness)) + 
  geom_col(fill = "gold", color = "gray50") + 
  geom_textcurve(data = data.frame(x = 1, 
                                   xend = 2, 
                                   y = 0.72, 
                                   yend = 0.52), 
                 aes(x, y, 
                     xend = xend, 
                     yend = yend), 
                 hjust = 0.35, 
                 ncp = 20,
                 curvature = -0.8, 
                 arrow = arrow(length = unit(2.5, 'mm'),  
                               ends = 'last', 
                               type = 'open'),
                 label = "significant difference") +
  scale_y_continuous(limits = c(0, 1)) 

# finally stolen from the git and adjusted slightly



df <- data.frame(x = c("A long axis label", "Another long label",
                       "The longest label of all", "Yet another label"),
                 y = c(4, 6, 8, 10))

# basic plot
ggplot(df, aes(x, y, fill = x)) + 
  geom_col(width = 0.5) +
  scale_fill_brewer(type = "qual") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 15),
        legend.position = "none") 

library(scales)

# fix those dodgy labels with the scales package
ggplot(df, aes(x, y, fill = x)) + 
  geom_col(width = 0.5) +
  theme_minimal() +
  scale_fill_brewer(type = "qual") +
  theme(axis.text.x = element_text(size = 15),
        legend.position = "none") +
  scale_x_discrete(labels = wrap_format(15))


# found this version to turn it into a polar style, quite nice if you 
# want pie chart style but not pie chart
ggplot(df, aes(x, y, fill = x)) + 
  geom_col(width = 0.5) +
  theme_minimal() +
  scale_fill_brewer(type = "qual") +
  scale_x_discrete(labels = wrap_format(10)) +
  coord_polar() +
  theme(axis.text.x = element_text(size = 15),
        legend.position = "none")


# geom text path also allows you to make curvy text
ggplot(df, aes(x, y, fill = x)) + 
  geom_col(width = 0.5) +
  theme_minimal() +
  scale_fill_brewer(type = "qual") +
  theme(axis.text.x = element_text(size = 15),
        legend.position = "none") +
  scale_x_discrete(labels = wrap_format(10)) +
  coord_curvedpolar()







