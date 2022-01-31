# import data for on-budget Federal spending in post-secondary education 
# from 1990 - 2019 (source: National Center for Education Statistics)
library(readxl)
library(pastecs)
library(dplyr)
tabn401_10 <- read_excel("C:/Users/tanur/OneDrive/Desktop/ECON 6374 Prob & Stats/Data science & research projects/tabn401.10.xls", 
                         sheet = "Post-secondary on-budget aid")
View(tabn401_10)

library(formattable)
tabn401_10 <- tabn401_10[-(1:5),] # to remove years before 1990
tabn401_10$`Post-secondary (in thousands $ - base year 2019)` <- currency(tabn401_10$`Post-secondary (in thousands $ - base year 2019)`)
View(tabn401_10)

tabn401_10 <- tabn401_10 %>%
  mutate(party_in_power = c("R", "R", "R", "D", "D", "D", "D", "D", "D", 
                            "D", "D", "R", "R", "R", "R", "R", "R", "R",
                            "R", "D", "D", "D", "D", "D", "D", "D", "D",
                            "R", "R", "R"))


# In order to visualize the data, we may plot the spending by both parties:
library(ggplot2)
library(scales)
plot_parties <- ggplot(tabn401_10, aes(x = `Fiscal year`, 
                                       y = `Post-secondary (in thousands $ - base year 2019)`)) +
  geom_point(stat = 'identity', position = "jitter", color = 'white', shape = 21, size = 4, aes(fill = party_in_power)) +
  scale_fill_manual(values=c("blue", "red")) +
  xlab("Fiscal Year") + 
  ylab("On-budget Federal spending in post-secondary education") + 
  ggtitle("Political party v/s On-budget Federal spending in post-secondary education") +
  labs(subtitle = "In thousands dollars. Base year: 2019") +
  scale_y_continuous(labels = comma) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot_parties

democrat <- tabn401_10 %>%
  filter(tabn401_10$party_in_power == "D")
View(democrat)
republican <- tabn401_10 %>%
  filter(tabn401_10$party_in_power == "R")
View(republican)

qqnorm(democrat$`Post-secondary (in thousands $ - base year 2019)`, 
       main = "QQ Plot of federal spending by Democrats") 
qqline(democrat$`Post-secondary (in thousands $ - base year 2019)`)

qqnorm(republican$`Post-secondary (in thousands $ - base year 2019)`, 
       main = "QQ Plot of federal spending by Republicans")

# Since the QQ plots do not particularly follow the Normal Distribution,
# I cannot use a parametric test like the t-test. I resort to a non-
# parametric test: the Wilcoxon Rank Sum test. The assumptions of the
# samples being drawn from the same continuous distribution are 
# fulfilled with the help of a continuity correction factor.

# To use Wilcoxon Rank Sum test, I check whether the two 
# samples have similar distributions:
library(cowplot)
options(scipen = 999)
one <- ggplot(democrat, aes(y = `Post-secondary (in thousands $ - base year 2019)`, x = `Fiscal year`, group = 1)) +
  geom_bar(stat = 'identity', fill = 'cyan') + 
  geom_line(data = democrat, aes(y = `Post-secondary (in thousands $ - base year 2019)`, x = `Fiscal year`), colour = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Democratic spending")
two <- ggplot(republican, aes(y= `Post-secondary (in thousands $ - base year 2019)`, x= `Fiscal year`, group = 1)) +
  geom_bar(stat = 'identity', fill = 'pink') + 
  geom_line(data = republican, aes(y = `Post-secondary (in thousands $ - base year 2019)`, x = `Fiscal year`), colour = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Distribution of Republican spending") 
two

one
two
plot_grid(one, two)

# Null Hypothesis (H0): the Median of on-budget Federal spending in 
# post-secondary education by Democrats = the Median of on-budget 
# Federal spending in post-secondary education by Republicans
# Alternate Hypothesis (H1): the Median of on-budget Federal spending 
# in post-secondary education by Democrats > the Median of on-budget 
# Federal spending in post-secondary education by Republicans

library(BSDA)
wilcox_test_parties <- wilcox.test(democrat$`Post-secondary (in thousands $ - base year 2019)`,
                                   republican$`Post-secondary (in thousands $ - base year 2019)`,
                                   alternative = "greater", correct = TRUE,
                                   exact = TRUE, conf.level = 0.95)
wilcox_test_parties

# The rank sum statistic 'W' is 113 with a p-value of 0.4918 (with 
# confidence level 95%). As the p-value > 0.05 (the smallest value 
# of alpha at which H0 can be rejected), I cannot reject H0 i.e. the
# Median of on-budget Federal spending in post-secondary education 
# by Democrats may be equal to the Median of on-budget Federal spending
# in post-secondary education by Republicans (1990 - 2019).

# The difference in medians between the two samples may be found
# using the bootstrap approach, i.e. to re-sample the data 
# (with replacement) 10,000 times, each time taking a difference in 
# medians. Then the median of those 10,000 differences is
# taken to estimate the difference in medians. Confidence intervals
# based on the 10,000 differences may also be found.

library(boot)

combined <- rbind(democrat, republican)
combined <- combined[,-c(1,2)]
View(combined)

# a function called med.diff to calculate the difference in medians:
med.diff <- function(d, i) {
  fund <- d[i,] 
  median(fund$`Post-secondary (in thousands $ - base year 2019)`[fund$party_in_power == "D"]) - 
    median(fund$`Post-secondary (in thousands $ - base year 2019)`[fund$party_in_power == "R"])
}

#  re-sample the data 10,000 times, taking a difference in medians each 
# time, and saving the results into an object called boot.out.

boot.out <- boot(data = combined, statistic = med.diff, R = 10000)

# boot.out object is a list object. The element named "t" contains the
# 10,000 differences in medians. Taking the median of those values gives
# a point estimate of the estimated difference in medians.

median(boot.out$t)

# The estimate is obtained i.e. the median of the differences in 
# spending between the two parties is $1,473,342 (in thousands $).

# Next the boot.ci function is used to calculate confidence intervals.
boot.ci(boot.out, type = "perc")

# The CI is -$28,635,739 to $34,631,404 (in thousands $).



# tried overlaying but didn't work:
ggplot(data = tabn401_10, aes(x=`Fiscal year`, y=`Post-secondary (in thousands $ - base year 2019)`, fill=`party_in_power`)) +
geom_bar(stat="identity", position ="identity") +
  scale_colour_manual(values=c("lightblue4", "red")) +
  scale_fill_manual(values=c("lightblue", "pink"))


