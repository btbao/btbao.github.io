## LSE Data Analytics Online Career Accelerator 
# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment 5 scenario
## Turtle Games’s sales department has historically preferred to use R when performing 
## sales analyses due to existing workflow systems. As you’re able to perform data analysis 
## in R, you will perform exploratory data analysis and present your findings by utilising 
## basic statistics and plots. You'll explore and prepare the data set to analyse sales per 
## product. The sales department is hoping to use the findings of this exploratory analysis 
## to inform changes and improvements in the team. (Note that you will use basic summary 
## statistics in Module 5 and will continue to go into more detail with descriptive 
## statistics in Module 6.)

################################################################################

## Assignment 5 objective
## Load and wrangle the data. Use summary statistics and groupings if required to sense-check
## and gain insights into the data. Make sure to use different visualisations such as scatterplots, 
## histograms, and boxplots to learn more about the data set. Explore the data and comment on the 
## insights gained from your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and distributions or 
## behaviour that may be of interest to the business.

################################################################################

# Module 5 assignment: Load, clean and wrangle data using R

## It is strongly advised that you use the cleaned version of the data set that you created and 
##  saved in the Python section of the course. Should you choose to redo the data cleaning in R, 
##  make sure to apply the same transformations as you will have to potentially compare the results.
##  (Note: Manual steps included dropping and renaming the columns as per the instructions in module 1.
##  Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 

## 1. Open your RStudio and start setting up your R environment. 
## 2. Open a new R script and import the turtle_review.csv data file, which you can download from 
##      Assignment: Predicting future outcomes. (Note: You can use the clean version of the data 
##      you saved as csv in module 1, or, can manually drop and rename the columns as per the instructions 
##      in module 1. Drop ‘language’ and ‘platform’ and rename ‘remuneration’ and ‘spending_score’) 
## 3. Import all the required libraries for the analysis and view the data. 
## 4. Load and explore the data.
##    - View the head the data.
##    - Create a summary of the new data frame.
## 5. Perform exploratory data analysis by creating tables and visualisations to better understand 
##      groupings and different perspectives into customer behaviour and specifically how loyalty 
##      points are accumulated. Example questions could include:
##    - Can you comment on distributions, patterns or outliers based on the visual exploration of the data?
##    - Are there any insights based on the basic observations that may require further investigation?
##    - Are there any groupings that may be useful in gaining deeper insights into customer behaviour?
##    - Are there any specific patterns that you want to investigate
## 6. Create
##    - Create scatterplots, histograms, and boxplots to visually explore the loyalty_points data.
##    - Select appropriate visualisations to communicate relevant findings and insights to the business.
## 7. Note your observations and recommendations to the technical and business users.

###############################################################################

# Your code here.

#Check your current working directory
getwd()

#Change your working directory
setwd("~/Documents/London School of Economics and Political Sciences/Course 3/LSE_DA301_assignment_files_new")setwd("~/Documents/London School of Economics and Political Sciences/Course 3/LSE_DA301_assignment_files_new")

#Verify it changed
getwd()

# Read the cleaned turtle reviews data
turtle_df <- read.csv("turtle_reviews_clean.csv",
                      check.names = FALSE, 
                      stringsAsFactors = FALSE, 
                      na.strings = c("", "NA"))

# Quick checks
str(turtle_df)   # structure & data types
head(turtle_df)  # first few rows
summary(turtle_df)  # summary statistics & NA counts
dim(turtle_df) #dimensions

# Import necessary libraries
# Tidyverse for data manipulation & plotting
library(tidyverse)   
# skimr for an extended summary
library(skimr)
# Automated EDA
library(DataExplorer)  

## 1. Can you comment on distributions, patterns, or outliers based on the visual exploration of the data?

# 1. Univariate: Histogram + Density of Loyalty Points
ggplot(turtle_df, aes(x = `Loyalty Points`)) +
  geom_histogram(binwidth = 250, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_density(aes(y = ..count.. * 250),  # scale density to histogram counts
               color = "darkblue", size = 1) +
  labs(title = "Distribution of Loyalty Points",
       x     = "Loyalty Points",
       y     = "Count") +
  theme_minimal()

# Observations:
# - Right‐skewed distribution: most customers have 500–2,500 points.
# - Long tail of high‐point earners (>6,000): potential outliers.
# - Possible secondary bump around 3,500–4,000: mid‐tier cohort.
# Business Insights:
# - Define loyalty tiers (e.g., Bronze 0–1,000; Silver 1,001–3,000; Gold 3,001+).
# - Audit top 1–2% of earners to confirm data validity.
# - Target mid‐tier customers with promotions to push them into Gold.

# 2. Bivariate: Loyalty Points vs. Spending Score
ggplot(turtle_df, aes(x = `Spending Score (1-100)`, y = `Loyalty Points`)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  labs(title = "Loyalty Points vs. Spending Score",
       x     = "Spending Score (1–100)",
       y     = "Loyalty Points") +
  theme_minimal()

# Observations:
# - Moderate positive trend: higher spending‐score generally → more points.
# - Some high‐scorers have low point balances and vice versa.
# Business Insights:
# - Flag customers whose points lag their spend for targeted reminders.
# - Promote enrollment/linking of loyalty accounts at checkout for high scorers with low points.
# - Check product categories for inconsistent point accrual.

# 3. Bivariate: Loyalty Points vs. Remuneration, colored by Gender
ggplot(turtle_df, aes(x = `Remuneration (K£)`, y = `Loyalty Points`, color = Gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Loyalty Points vs. Remuneration by Gender",
       x     = "Remuneration (K£)",
       y     = "Loyalty Points") +
  theme_minimal()

# Observations:
# - Moderate upward curve: higher‐paid customers tend to earn more points.
# - Females show greater variance at mid‐remuneration.
# Business Insights:
# - Introduce salary‐linked bonus points or tier thresholds.
# - Target female customers with high pay but low points via tailored outreach.
# - Review point structure to ensure no gender bias.

# 4. Boxplot + Violin: Loyalty Points by Gender
ggplot(turtle_df, aes(x = Gender, y = `Loyalty Points`, fill = Gender)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  coord_cartesian(ylim = c(0, quantile(turtle_df$`Loyalty Points`, 0.95))) +
  labs(
    title = "Loyalty Points by Gender (Violin + Boxplot)",
    x     = "Gender",
    y     = "Loyalty Points"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Observations:
# - Medians are similar across genders, but the distribution shape differs.
# - Females exhibit a wider spread and more pronounced tails, indicating greater variance.
# - A small cluster of extreme high‐point outliers is visible in both groups.
# Business Insights:
# - Consider gender‐neutral core rewards with optional add-ons tailored to each gender’s preferences.
# - Engage high-variance segments (especially female outliers) with VIP or insider perks.
# - Audit extreme outliers to ensure points were legitimately earned.


# 5. Boxplot + Violin: Loyalty Points by Education Level
ggplot(turtle_df, aes(x = Education, y = `Loyalty Points`, fill = Education)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(
    title = "Loyalty Points by Education Level (Violin + Boxplot)",
    x     = "Education",
    y     = "Loyalty Points"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Show as Barchart

# Compute average Loyalty Points by Education level
loyalty_means <- turtle_df %>%
  group_by(Education) %>%
  summarize(mean_points = mean(`Loyalty Points`, na.rm = TRUE)) %>%
  arrange(mean_points)

# Bar chart of average Loyalty Points with numeric labels
ggplot(loyalty_means, aes(x = reorder(Education, mean_points), y = mean_points, fill = Education)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(mean_points, 1)), 
            hjust = -0.1, 
            size = 3) +
  coord_flip(clip = "off") +
  labs(
    title = "Average Loyalty Points by Education Level",
    x     = "Education Level",
    y     = "Average Loyalty Points"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title  = element_text(size = 14, face = "bold"),
    plot.margin = margin(5.5, 40, 5.5, 5.5)  # extra right margin for labels
  )

# Observations:
# - The Basic-education cohort not only has the highest average (~2265 pts) 
#   but also the highest median and the widest spread of loyalty points.
# - Postgraduate and PhD groups sit close together with moderate averages (~1500 pts)
#   and tighter distributions, indicating consistent—but not extreme—engagement.
# - Graduate holders average ~1666 pts with a moderate spread; Diploma holders lag 
#   behind at ~1336 pts with the narrowest distribution and few outliers.
#
# Business Insights:
# - **Basic cohort**: flagship segment—leverage their high engagement for referrals, 
#   “ambassador” programs, and upsell premium products.
# - **Postgrad/PhD**: steady engagers—offer tailored experiential rewards (exclusive events, 
#   advanced access) to deepen loyalty.
# - **Graduate**: growth opportunity—promote targeted bonus-point campaigns to push more 
#   graduates into the Postgrad/PhD range.
# - **Diploma**: activation focus—run “double-points days” or educational “learn-and-earn” 
#   content to boost both average and spread of points.


# 6. Boxplot + Violin: Loyalty Points by Age Group
turtle_df %>%
  mutate(AgeGroup = cut(
    Age,
    breaks = c(17, 25, 35, 45, 55, 65, Inf),
    labels = c("18–25","26–35","36–45","46–55","56–65","66+"),
    right = FALSE
  )) %>%
  ggplot(aes(x = AgeGroup, y = `Loyalty Points`, fill = AgeGroup)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(
    title = "Loyalty Points by Age Group (Violin + Boxplot)",
    x     = "Age Group",
    y     = "Loyalty Points"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

## Corroborate with Bar Chart

age_means <- turtle_df %>%
  mutate(
    AgeGroup = cut(
      Age,
      breaks = c(17, 25, 35, 45, 55, 65, Inf),
      labels = c("18–25","26–35","36–45","46–55","56–65","66+"),
      right = FALSE
    )
  ) %>%
  group_by(AgeGroup) %>%
  summarize(avg_points = mean(`Loyalty Points`, na.rm = TRUE)) %>%
  arrange(avg_points)

ggplot(age_means, aes(x = reorder(AgeGroup, avg_points), y = avg_points, fill = AgeGroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_points, 1)),
            hjust = -0.1, size = 3) +
  coord_flip(clip = "off") +
  labs(
    title = "Average Loyalty Points by Age Group",
    x     = "Age Group",
    y     = "Average Loyalty Points"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title  = element_text(size = 14, face = "bold"),
    plot.margin = margin(5.5, 40, 5.5, 5.5)
  )

# Observations:
# - 26–35 leads on average (~1993 pts) and shows a broad distribution (many mid/high earners).
# - 36–45 is a strong second (~1782 pts) with a tight violin body, indicating consistent engagement.
# - 46–55 has a lower average (~1407 pts) but the longest upper tail (super-earners present).
# - 66+ (~1252 pts) outperforms 56–65 (~1097 pts) despite a smaller sample; seniors show notable high-end outliers.
# - 56–65 sits mid-low with moderate spread; 18–25 has the lowest average (~878 pts) and many zero/low balances.
# Business Insights:
# - Activate 18–25: starter bonuses & gamified onboarding to close activation gap.
# - Scale 26–35: referral & ambassador programs leveraging top performers.
# - Retain 36–45: exclusive tiered rewards & VIP events for the core cohort.
# - Elevate 46–55: targeted bonus-point offers & VIP upgrade prompts for high-tier members.
# - Support 56–65: seasonal spend-&-earn campaigns to boost accrual.
# - Reward 66+: non-points perks & VIP outreach for high-value outliers.


# 7. Multivariate: Spending vs. Loyalty, faceted by Education
ggplot(turtle_df, aes(x = `Spending Score (1-100)`, y = `Loyalty Points`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  facet_wrap(~ Education) +
  labs(
    title = "Spending Score vs. Loyalty Points, by Education",
    x     = "Spending Score (1–100)",
    y     = "Loyalty Points"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )

# Observations:
# - Many top‐spenders (Spending Score ≥ 80) fall below the fit line in several education panels, meaning they could be earning more points.
# - A handful of low‐spenders (Score < 40) sit well above the line, demonstrating strong loyalty despite modest spending.
# - High spenders above or on the fit line represent your most engaged, high‐value customers.
#
# Business Insights:
# - Alert high‐spenders below the line about bonus‐point opportunities (e.g., “You could earn X more points!”) to drive awareness.
# - Design exclusive retention incentives (VIP tiers, early product access) for the high‐spender/high‐points cohort.
# - Encourage low‐spend/high‐loyalty customers to spend more via targeted multipliers or “earn more when you spend” campaigns.
# - As this is segmented by Education Levels, we could target more precisely.

# 8. Multivariate: Spending Score vs. Loyalty Points, faceted by Age Group
# (Scatter + linear trend within each age bracket)
turtle_df %>%
  mutate(
    AgeGroup = cut(
      Age,
      breaks = c(17, 25, 35, 45, 55, 65, Inf),
      labels = c("18–25","26–35","36–45","46–55","56–65","66+"),
      right = FALSE
    )
  ) %>%
  ggplot(aes(x = `Spending Score (1-100)`, y = `Loyalty Points`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~ AgeGroup) +
  labs(
    title = "Spending Score vs. Loyalty Points, by Age Group",
    x     = "Spending Score (1–100)",
    y     = "Loyalty Points"
  ) +
  theme_minimal() +
  theme(
    strip.text   = element_text(face = "bold"),
    plot.title   = element_text(size = 14, face = "bold")
  )

# Observations:
# - 26–35 & 36–45 age groups have multiple top-spenders (Score ≥ 80) falling below the fit line → they’re missing out on points.
# - 18–25 & 56–65 cohorts include low-spenders above the line → these customers are highly loyal despite low spend.
# - 46–55 & 66+ show mixed patterns, with some high-spenders under-earning but also loyal outliers.
# Business Insights:
# - Send “You could earn X more points” alerts to 26–35 & 36–45 under-earners to drive awareness and upsell.
# - Offer VIP retention perks (exclusive tiers, early access) for the high-spender/high-points customers across all ages.
# - Design “earn more when you spend” incentives (double-points days, targeted multipliers) for 18–25 & 56–65 high-loyalty, low-spend segments.

# 9. Bubble Chart: Loyalty vs. Remuneration (size = Spending Score)
library(ggplot2)
library(viridis)    # for scale_color_viridis_c()
library(scales)     # for comma()

ggplot(turtle_df, aes(
  x     = `Remuneration (K£)`,
  y     = `Loyalty Points`,
  color = `Spending Score (1-100)`,
  size  = `Spending Score (1-100)`
)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c(option = "plasma") +
  scale_size_continuous(range = c(1, 6)) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Loyalty Points vs. Remuneration\n(size & color = Spending Score)",
    x     = "Remuneration (K£)",
    y     = "Loyalty Points (log scale)",
    color = "Spending Score",
    size  = "Spending Score"
  ) +
  theme_minimal() +
  theme(
    plot.title     = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

# Observations:
# - Clear upward trend: higher-paid customers generally accumulate more points.
# - High spending-score customers concentrate in mid-to-high pay bands (~40–80 K£).    # - Super-earners span a wide salary range (50–95 K£), not just top earners.
# - Core cluster around 20–40 K£ remuneration & 500–2 000 points highlights mid-paid, mid-loyalty users.
# - Low-paid (≤20 K£) customers seldom exceed 1 000 points, indicating limited engagement.
# Business Insights:
# - Define pay-band tiers (20–40, 40–60, 60+ K£) and tailor rewards to each segment.
# - Push the core cluster upward with time-limited bonus-point promotions.
# - Audit super-earners for VIP program inclusion or data-quality verification.
# - Incentivize low-paid customers via accessible rewards (e.g., double-points days).
# - Leverage spending-score coloring to identify under-earning high spenders for targeted outreach.
    
## 2. Insights that merit deeper investigation
# - Top‐earner outliers: identify customers >5 000 pts to verify legitimacy or bulk‐purchase behavior
# - Under‐earning high spenders: flag Spending Score ≥80 who fall below trend line to check for account/linkage issues
# - Basic‐education cohort: investigate what drives their unexpectedly high avg/median points despite “basic” label

## 3. Groupings for deeper segmentation
# - Demographic buckets: AgeGroup (18–25, 26–35, …), Education (Basic→Postgrad), Gender
# - Behavioral quadrants: 
#     1. High spend / high loyalty (VIP core)
#     2. High spend / low loyalty (activation targets)
#     3. Low spend / high loyalty (upsell targets)
#     4. Low spend / low loyalty (onboarding/education targets)
# - Remuneration bands: pay brackets (20–40, 40–60, 60+ K£) for tiered reward scaling

## 4. Specific patterns to drill into next
# - Review sentiment vs. loyalty: test correlation between customer review sentiment scores and loyalty‐point balances

### Summary of Findings

# Overall Distribution
# - Most customers hold 500–2,500 points.
# - A small “power-user” group exceeds 5,000 points.

# Spend vs. Points Gaps
# - High spenders (Score ≥ 80) in Ages 26–35 and 36–45, as well as Diploma and Graduate education levels,
#   often fall below the trend line—meaning they could be earning more points.
# - Some low-spenders (Score < 40) over-earn points, marking them as potential ambassadors.

# Key Insights
# 1. Core Segments (Ages 36–45, 26–35; Postgraduates)
#    • Convert spend to points most efficiently.
#    • Recommend VIP tiers, early-access events, and referral incentives.
#
# 2. Activation Gaps (Diploma & High-Spending Graduates)
#    • Under-earn relative to spend.
#    • Deploy “You could earn X more points” alerts, double-points days, and clear earning guides.
#
# 3. Under-Leveraged Cohorts (Ages 18–25, 56–65)
#    • High points but low spend.
#    • Introduce spend-based multipliers to encourage additional purchases.

# Further Exploration
# - Audit extreme outliers (> 5,000 points) for data integrity and VIP candidacy.
# - Analyze review sentiment vs. loyalty balances to identify true advocates.

# Next Steps
# 1. Launch targeted “gap” campaigns for under-earners.
# 2. Pilot segmented, tiered rewards and track changes in spend and engagement.

###############################################################################
###############################################################################

# Assignment 6 scenario

## In Module 5, you were requested to redo components of the analysis using Turtle Games’s preferred 
## language, R, in order to make it easier for them to implement your analysis internally. As a final 
## task the team asked you to perform a statistical analysis and create a multiple linear regression 
## model using R to predict loyalty points using the available features in a multiple linear model. 
## They did not prescribe which features to use and you can therefore use insights from previous modules 
## as well as your statistical analysis to make recommendations regarding suitability of this model type,
## the specifics of the model you created and alternative solutions. As a final task they also requested 
## your observations and recommendations regarding the current loyalty programme and how this could be 
## improved. 

################################################################################

## Assignment 6 objective
## You need to investigate customer behaviour and the effectiveness of the current loyalty program based 
## on the work completed in modules 1-5 as well as the statistical analysis and modelling efforts of module 6.
##  - Can we predict loyalty points given the existing features using a relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

## Assignment 6 assignment: Making recommendations to the business.

## 1. Continue with your R script in RStudio from Assignment Activity 5: Cleaning, manipulating, and 
##     visualising the data.
## 2. Load and explore the data, and continue to use the data frame you prepared in Module 5.
## 3. Perform a statistical analysis and comment on the descriptive statistics in the context of the 
##     review of how customers accumulate loyalty points.
##  - Comment on distributions and patterns observed in the data.
##  - Determine and justify the features to be used in a multiple linear regression model and potential
##.    concerns and corrective actions.
## 4. Create a Multiple linear regression model using your selected (numeric) features.
##  - Evaluate the goodness of fit and interpret the model summary statistics.
##  - Create a visual demonstration of the model
##  - Comment on the usefulness of the model, potential improvements and alternate suggestions that could 
##     be considered.
##  - Demonstrate how the model could be used to predict given specific scenarios. (You can create your own 
##     scenarios).
## 5. Perform exploratory data analysis by using statistical analysis methods and comment on the descriptive 
##     statistics in the context of the review of how customers accumulate loyalty points.
## 6. Document your observations, interpretations, and suggestions based on each of the models created in 
##     your notebook. (This will serve as input to your summary and final submission at the end of the course.)

################################################################################

# Your code here.

# Import necessary libraries
library(tidyverse)    # data manipulation & plotting
library(moments)      # skewness & kurtosis
library(car)          # VIF (for later)
library(lmtest)       # Breusch–Pagan test (for later)
library(tseries)      # Jarque–Bera test (for later)

# Load the Turtle Games data set
turtle <- read.csv('turtle_reviews_clean.csv', header = TRUE)

# Inspect the data
head(turtle)
str(turtle)

###############################################################################
# 2. Explore the data set (Descriptive statistics)

# Overall summary of all columns
summary(turtle)

# Summary of the target: Loyalty.Points
summary(turtle$`Loyalty.Points`)

# Central tendency and extremes for Loyalty.Points
mean(turtle$`Loyalty.Points`)
median(turtle$`Loyalty.Points`)
min(turtle$`Loyalty.Points`)
max(turtle$`Loyalty.Points`)

# Quartiles and IQR
quantile(turtle$`Loyalty.Points`, 0.25)
quantile(turtle$`Loyalty.Points`, 0.75)
IQR(turtle$`Loyalty.Points`)

# Variability measures
var(turtle$`Loyalty.Points`)
sd(turtle$`Loyalty.Points`)

# Descriptive stats for Spending.Score..1.100.
summary(turtle$`Spending.Score..1.100.`)
sd(turtle$`Spending.Score..1.100.`)

# Descriptive stats for Remuneration..K..
summary(turtle$`Remuneration..K..`)
sd(turtle$`Remuneration..K..`)

# Descriptive stats for Age
summary(turtle$Age)
sd(turtle$Age)

###############################################################################
# 3. Assess normality and distribution patterns

# Q–Q plot for Loyalty.Points
qqnorm(turtle$`Loyalty.Points`)
qqline(turtle$`Loyalty.Points`, col = 'red')

# Shapiro–Wilk test for Loyalty.Points
shapiro.test(turtle$`Loyalty.Points`)

# Skewness & kurtosis for Loyalty.Points
skewness(turtle$`Loyalty.Points`)
kurtosis(turtle$`Loyalty.Points`)

###############################################################################
# 4. Inferential exploration: correlations

# Normality tests for predictors
shapiro.test(turtle$`Spending.Score..1.100.`)
shapiro.test(turtle$`Remuneration..K..`)
shapiro.test(turtle$Age)

# Pearson correlations with Loyalty.Points
cor(turtle$`Loyalty.Points`, turtle$`Spending.Score..1.100.`, method = "pearson")
cor(turtle$`Loyalty.Points`, turtle$`Remuneration..K..`,   method = "pearson")
cor(turtle$`Loyalty.Points`, turtle$Age,                  method = "pearson")
###############################################################################

# Should we include Education?. Education analysis (categorical → ordinal → correlation + ANOVA)
###############################################################################

# Ordinal encode Education
turtle$EduOrd <- as.numeric(
  factor(turtle$Education,
         levels = c("Basic", "Diploma", "graduate", "postgraduate", "PhD"),
         ordered = TRUE)
)

# Spearman correlation (Loyalty.Points vs EduOrd)
spearman_res <- cor.test(
  turtle$`Loyalty.Points`,
  turtle$EduOrd,
  method = "spearman"
)
print(spearman_res)

# One-way ANOVA: Loyalty.Points by Education
educ_aov <- aov(`Loyalty.Points` ~ Education, data = turtle)
summary(educ_aov)

# Effect size η²
ss  <- summary(educ_aov)[[1]]$`Sum Sq`
eta2 <- ss["Education"] / sum(ss)
cat("Eta-squared for Education: ", round(eta2, 3), "\n")

# Group summary by Education
turtle %>%
  group_by(Education) %>%
  summarise(
    count       = n(),
    mean_points = mean(`Loyalty.Points`),
    sd_points   = sd(`Loyalty.Points`)
  )

## REVIEW OF DESCRIPTIVE STATISTICS
# - Loyalty.Points:
#     • Range:      25 … 6 847
#     • Mean:       1 578
#     • Median:     1 276
#     • IQR:        979
#     • SD:         1 283
#     → Very high dispersion; most customers cluster between ~772–1 751,
#       but a small number achieve extremely high point totals.
#
# - Spending.Score..1.100.:
#     • Range:      1 … 99
#     • Mean/Med:   50
#     • IQR:        32–73
#     • SD:         26
#     → Fairly uniform spending behaviour across customers.
#
# - Remuneration..K.. (annual income in K£):
#     • Range:      12.30 … 112.34
#     • Mean:       48.08
#     • Median:     47.15
#     • IQR:        30.34–63.96
#     • SD:         23.12
#     → Moderate spread in income levels.
#
# - Age:
#     • Range:      17 … 72
#     • Mean:       39.49
#     • Median:     38.00
#     • IQR:        29–49
#     • SD:         13.57
#     → Broad age distribution with modest variability.
#
###############################################################################
# DISTRIBUTIONS & PATTERNS
# - Normality tests (Shapiro–Wilk):
#     • Loyalty.Points: W = 0.84307, p < 2.2e-16    → strongly non-normal
#     • Spending.Score: W = 0.96835, p < 2.2e-16    → non-normal but ~uniform
#     • Remuneration..K..: W = 0.96768, p < 2.2e-16 → non-normal, moderate skew
#     • Age: W = 0.95242, p < 2.2e-16              → non-normal, mild skew
#
# - Skewness & Kurtosis (Loyalty.Points):
#     • Skewness ≈ 1.464   → pronounced right skew
#     • Kurtosis ≈ 4.709   → heavy tails
#
# - Pearson correlations:
#     • Loyalty.Points ↔ Spending.Score:  r ≈ 0.672   (strong +)
#     • Loyalty.Points ↔ Remuneration:    r ≈ 0.616   (strong +)
#     • Loyalty.Points ↔ Age:              r ≈ –0.042  (negligible)
#
###############################################################################
# EDUCATION ANALYSIS
# - Ordinal encode: Basic < Diploma < graduate < postgraduate < PhD
# - Spearman’s rho (Loyalty.Points vs EduOrd):
#     • ρ ≈ –0.0756, p ≈ 0.0013 → significant but minuscule effect
#
# - One-way ANOVA (by Education):
#     • F(4,1995) = 7.226, p ≈ 8.94e-06 → group means differ due to large N
# - Effect size η² ≈ 0.014 → Education explains only ~1.4% of variance
#
# - Group means & SDs:
#     • Basic       (n=50)  → mean=2 265, SD=1 510   [unstable, small n]
#     • graduate    (n=900) → mean=1 666, SD=1 341
#     • PhD         (n=460) → mean=1 500, SD=1 274
#     • postgraduate(n=400) → mean=1 499, SD=1 136
#     • diploma     (n=190) → mean=1 336, SD=1 163
#
# → Conclusion: Education is statistically significant but practically negligible—
#   omit from the MLR or include only if theoretically warranted.
#
###############################################################################
# FEATURE SELECTION & MODEL CONCERNS
# - Include in MLR:
#     • Spending.Score..1.100.  (continuous)
#     • Remuneration..K..       (continuous)
#     • Age                     (continuous)
#     • Gender                  (factor → dummy)
#
# - Exclude:
#     • Education               (negligible contribution)
#     • Product (high-cardinality ID)
#     • Review / Summary (text features with near-zero linear association)
#
# - Potential concerns & corrective actions:
#     1. Non-normal, heteroscedastic residuals:
#        → transform Loyalty.Points (log or √) or use robust regression;
#          apply White’s (HC) standard errors.
#     2. Outliers/heavy tails in Loyalty.Points:
#        → Winsorize top 1–2% or fit quantile regression.
#     3. Multicollinearity:
#        → check VIF; combine or drop collinear dummies if VIF > 5.
#     4. Generalization:
#        → use k-fold cross-validation or hold-out testing.

###############################################################################
# Turtle Games Multiple Linear Regression Analysis
# Full model, diagnostics, visualization, prediction scenarios,
# reduced models, and adjusted R² comparison.
###############################################################################

# 1. Load libraries
library(tidyverse)    # data manipulation & plotting
library(car)          # VIF
library(lmtest)       # Breusch–Pagan & coeftest
library(tseries)      # Jarque–Bera
install.packages("sandwich")
library(sandwich)   # for vcovHC()


# 2. Load & prepare data
turtle <- read.csv('turtle_reviews_clean.csv', header = TRUE)

# Convert Gender to factor with Female as baseline
turtle$Gender <- factor(turtle$Gender)
turtle$Gender <- relevel(turtle$Gender, ref = "Female")

# 3. Fit full model (Model1)
model1 <- lm(
  `Loyalty.Points` ~ `Spending.Score..1.100.` +
    `Remuneration..K..` +
    Age +
    Gender,
  data = turtle
)

# 4. Model summary & goodness‐of‐fit
summary(model1)
#   – R² ≈ 0.8408, Adj R² ≈ 0.8405
#   – F-statistic: p < 0.001
#   – Coefficients (all p < 0.01):
#       * Spending.Score ..:+34.16 points per unit
#       * Remuneration ..:+34.07 points per K£
#       * Age: +11.22 points per year
#       * GenderMale: –78.48 points vs Female

# 5. Diagnostics & corrective actions
par(mfrow = c(2,2))
plot(model1)
bptest(model1)  # heteroscedasticity test
coeftest(model1, vcov. = vcovHC(model1, type = "HC1"))  # robust SEs
jarque.bera.test(residuals(model1))  # normality of residuals
vif(model1)  # multicollinearity

# 6. Visual demonstration
turtle <- turtle %>%
  mutate(
    Predicted = predict(model1),
    Residual  = residuals(model1)
  )

# 6a. Actual vs Predicted
ggplot(turtle, aes(x = Predicted, y = `Loyalty.Points`)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Actual vs Predicted Loyalty Points",
    x = "Predicted",
    y = "Actual"
  )

# 6b. Residuals vs Fitted
ggplot(turtle, aes(x = Predicted, y = Residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    title = "Residuals vs Fitted Values",
    x = "Fitted (Predicted) Values",
    y = "Residuals"
  )

# 7. Prediction scenarios
new_customers <- data.frame(
  `Spending.Score..1.100.` = c(85, 20, 60),
  `Remuneration..K..`      = c(70, 30, 50),
  Age                      = c(30, 55, 40),
  Gender                   = factor(c("Female", "Male", "Female"),
                                    levels = levels(turtle$Gender))
)

predict(
  model1,
  newdata  = new_customers,
  interval = "confidence"
)

# 8. Reduced models to test variable importance
# Model2: drop Gender
model2 <- lm(
  `Loyalty.Points` ~ `Spending.Score..1.100.` +
    `Remuneration..K..` +
    Age,
  data = turtle
)
summary(model2)

# Model3: drop Age (only Spending.Score & Remuneration)
model3 <- lm(
  `Loyalty.Points` ~ `Spending.Score..1.100.` +
    `Remuneration..K..`,
  data = turtle
)
summary(model3)

# 9. Compare Adjusted R-squared
adj1 <- summary(model1)$adj.r.squared
adj2 <- summary(model2)$adj.r.squared
adj3 <- summary(model3)$adj.r.squared

cat("Adjusted R-squared:\n",
    " Model1 (all vars) : ", round(adj1, 4), "\n",
    " Model2 (-Gender)  : ", round(adj2, 4), "\n",
    " Model3 (-Age)     : ", round(adj3, 4), "\n")

###############################################################################
# MODEL USEFULNESS, IMPROVEMENTS & ALTERNATIVES
#
# - Model1 (Spending.Score, Remuneration, Age, Gender) explains ~84.05% of variance 
#   (Adj R² = 0.8405). Model2 (dropping Gender) explains ~83.97% (Adj R² = 0.8397), 
#   Model3 (dropping Age) explains ~82.67% (Adj R² = 0.8267). 
#   ⇒ Gender and Age each add small but real explanatory power.
#
# - Diagnostics:
#     • Breusch–Pagan test (BP = 44.43, p ≈ 5.23e-9) indicates heteroscedasticity.
#     • Jarque–Bera test (χ² = 24.55, p ≈ 4.68e-6) shows residuals non-normal.
#     • VIFs all ≈ 1 → no multicollinearity concerns.
#     • Robust SEs (HC1) change SEs slightly but all predictors remain highly significant.
#   → Use response transformation (e.g. log √Loyalty.Points) or robust regression 
#     (e.g. MASS::rlm) to guard against heteroscedasticity and non-normal errors.
#
#
# - Alternative modeling approaches:
#     • Tree-based/ensemble methods (random forest)
#
###############################################################################
# Demonstrate predictions for specific customer scenarios using model1
###############################################################################

# Define new customer scenarios
scenarios <- data.frame(
  `Spending.Score..1.100.` = c(85, 20, 60, 50),
  `Remuneration..K..`      = c(70, 30, 50, 48),
  Age                      = c(30, 55, 40, 38),
  Gender                   = c("Female", "Male", "Female", "Male")
)

# Make sure Gender is a factor with the same levels as in your training data
scenarios$Gender <- factor(scenarios$Gender,
                           levels = levels(turtle$Gender))

# Use predict() to generate point estimates and 95% confidence intervals
predictions <- predict(
  model1,
  newdata  = scenarios,
  interval = "confidence",
  level    = 0.95
)

# Combine scenarios and predictions into one table
results <- cbind(scenarios, as.data.frame(predictions))

# View the results
print(results)

###############################################################################
# INTERPRETATION OF PREDICTION SCENARIOS
#
# Scenario 1 (High spender & high income, 30-year-old Female):
#   • Spending.Score = 85, Remuneration = £70K, Age = 30
#   • Predicted Loyalty.Points ≈ 3,448 
#   • 95% CI: [3,400, 3,497]
#   → Very high point accumulation; a narrow interval indicates precise estimate.
#
# Scenario 2 (Low spender & low income, 55-year-old Male):
#   • Spending.Score = 20, Remuneration = £30K, Age = 55
#   • Predicted Loyalty.Points ≈   67 
#   • 95% CI: [  18,  117]
#   → Minimal accumulation; even upper bound remains low.
#
# Scenario 3 (Mid-range spender & income, 40-year-old Female):
#   • Spending.Score = 60, Remuneration = £50K, Age = 40
#   • Predicted Loyalty.Points ≈ 2,025 
#   • 95% CI: [1,994, 2,057]
#   → Moderate accumulation; precise prediction in mid-range.
#
# Scenario 4 (Below-average spender & income, 38-year-old Male):
#   • Spending.Score = 50, Remuneration = £48K, Age = 38
#   • Predicted Loyalty.Points ≈ 1,515 
#   • 95% CI: [1,480, 1,549]
#   → Reflects Gender effect (~75 fewer points vs Female) and moderate spending.
#
# KEY TAKEAWAYS:
# - Spending.Score and Remuneration are primary drivers: small changes yield large point differences.
# - Gender shifts (~–75 points for Male) and Age effects (~+11 points per year) are secondary.
# - Narrow confidence intervals in typical ranges imply high precision; use model to guide targeted offers.


###############################################################################

#### FINAL BUSINESS OBSERVATIONS AND RECOMMENDATIONS ###

###############################################################################
# MODEL PERFORMANCE & BUSINESS INSIGHTS
#
# - Model1 (Spending.Score, Remuneration, Age, Gender) explains 84.05% of variance
#   in loyalty points. Model2 (-Gender) explains 83.97%; Model3 (-Age) explains 82.67%.
#   Retaining Gender and Age measurably improves fit.
#
# KEY BUSINESS INSIGHTS
# - Spending.Score & Remuneration drive loyalty: +1 point in Spending.Score or £1K
#   in income → +34 loyalty points.
# - Gender & Age refine forecasts: males ≈ –75 points; +11 points per additional year.
# - Top 1% of customers accumulate exceptionally high points, skewing averages.
#
# PREDICTIVE SCENARIOS
# 1. High-spender/high-income  (Score=85, Income=70K, Age=30, Female) → ~3,448 pts
# 2. Low-spender/low-income    (Score=20, Income=30K, Age=55, Male)   → ~67 pts
# 3. Mid-range                 (Score=60, Income=50K, Age=40, Female) → ~2,025 pts
# 4. Below-average             (Score=50, Income=48K, Age=38, Male)   → ~1,515 pts
#
# ACTIONABLE BUSINESS INITIATIVES
# - Tiered Rewards: Launch Silver/Gold/Platinum tiers based on predicted points
#   to incentivize incremental spending.
# - Double-Points Campaigns: Deploy time-limited multipliers to low-spender/
#   low-income segments to boost engagement.
# - Premium Experiences: Offer exclusive events or early access for high-spender/
#   high-income customers.
# - Personalized Communications: Use model outputs to tailor messaging by
#   gender and age for maximum impact.
###############################################################################

