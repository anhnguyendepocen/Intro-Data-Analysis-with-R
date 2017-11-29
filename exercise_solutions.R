# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ R-Script to solve the exercises and homework for the workshop:
# ~ Introduction to Data Analysis with R
# ~ 23 & 24 November 2017
#
# ~ last updated: 2017-11-29
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Refers to Script-v.0.7-2017-11-28 from the Introduction to Data Analysis with
# R workshop, taught at the European University Institute on 23 & 24 November
# 2017.

# ==== Exercise 1 (3.1 RStudio) ====
# If you encounter any other difficulties, remember to either use Google with
# a precise problem description, or check out RStudio's online documentation:
# https://support.rstudio.com/hc/en-us/sections/200107586-Using-the-RStudio-IDE

# E.g., for R Projects:
# https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects

# ==== Exercise 2 (3.3 Calculator and Logical Comparison) ====
# You can either do everything inline:
32.85 / 3
ceiling(32.85 / 3)

# Pro Tipp: Or store steps of your process in objects in your R environment. 
# This way you can reuse the results later on in a different context.
bill <- 32.85
bill

price_per_person <- bill / 3
price_per_person

rounded_price <- ceiling(price_per_person)
rounded_price

# Use the help function to find out what the ceiling() function does
?ceiling
# or
?ceiling()
# or
help(ceiling)

# ==== Homework (3.3 Calculator and Logical Comparison) ====
# (1)
((3 + 4 - 5) - 9)^2
-99 / 33 + 42
log(1)
sqrt(2)^2

# (2)
5 == 7
5 * 5 >= 6 * 4
sqrt(3) != cos(17)

# (3)
?mean

# Agreed, this might be a very specific technique that you would rarely use.
# Basically, it is to see whether there are some outliers at either end of the
# spectrum that affect the calculation of the mean. 

# ==== Exercise 3 (3.4 Working Directory) ====
# Check the current working directory of your R session (your R Project folder
# by default):
getwd()

# Then use the dir() function to check whether the 'course_material' folder is
# in your current working directory:
dir()

# Pro Tipp: If you're writing a more complicated script, or even a function of your own,
# you might want to use logical control structure to do this for you (see 
# section 9 of the script):
if ("course_material" %in% dir()) {
  cat("Exercise 3 solved. Go ahead!")
} else {
  print("R couldn't find the 'course_material' folder in the current working
        directory. Please download the course material and save it in the right
        space, or change your working directory.")
}

# Pro Tipp: You wonder what is the difference between cat() and print()? In this case, 
# are functionally equivalent, but use the help function to find out more, or 
# just use both and see what different outputs they produce.

# ==== Exercise 4 (3.6 Object Classes) ====
# Use the help function to find out how to use the matrix() function:
?matrix

# Create matrix
neo <- matrix(data = 1:9, nrow = 3, ncol = 3)
neo 

# Convert to data.fame
df <- as.data.frame(neo)
df

# Save in .RData file
save(df, file = "A_matrix_as_df.RData")

# Remove the two objects from your R environment
rm(df, neo)

# Double check the objects in your environment (manually)
ls()

# Pro Tipp: Double check with logical control structures (see section 9 of the 
# script)
if (c("df", "neo") %in% ls()) {
  print("You didn't properly delete the objects from your R environment. Try 
        again!")
} else {
  print("Well done! Now load them again with the load() function!")
}

# Reload the matrix
load("A_matrix_as_df.RData")

# Double check
ls()

# Pro Tipp: You can delete files from your directory either manually via your
# operating system, or via R
?file 

file.remove("A_matrix_as_df.RData")

# ==== Exercise 5 (3.7 Packages) ====
# Even if you install only one package, you'll have to use the plural of 
# 'installpackages' rather than 'install.package'. Also, you always need to use
# quotation marks (either ' or ") with this function.
install.packages("ggplot2")

# The library can take the name of the package with and without the quotation
# marks.
library(ggplot2)

# The following needs quotation marks again. I know it's annoying.
ls("package:ggplot2")

?ggplot()

# ==== Exercise 6 (4 Vector/Factor Operations) ====
repetitive <- rep(1:5, 10)
repetitive

# If you use the length function, you can double check whether the vector 
# actually has 50 elements.
length(repetitive)

can_count <- seq(0, 20, 2)
can_count

subs <- can_count[(length(can_count)-2):length(can_count)]

must_eat <- c("Coccoli", "Bistecca", "Cacciucco alla Livornese")

# ==== Homework (4 Vector/Factor Operations) ====
# (1)
a <- c(10, 11, 9, 12, 11)
b <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
d <- seq(0, 8, 2)
e <- c("a", "b", "c", "a", "b")

another_matrix_1 <- rbind(a,b)
another_matrix_1
class(another_matrix_1)

another_matrix_2 <- cbind(d,e)
another_matrix_2
class(another_matrix_2)

# If you use rbind or cbind with simple homogenous vectors, it will 
# automatically create a matrix instead of a data.frame. Since matrices are 
# also meant to only store homogenous content (i.e., only elements of the same
# class), it forces vectors of different classes into the same. The least 
# common denominator is always the character class, as it is easier to convert
# a number into a character than a character into a number.

# Use the class() function to double check, and the conversion functions to fix
# any issue that might arise (as.numeric, as.character, as.integer, etc.)

# (2)
x <- 33:133
x 
length(x)

# (3)
y <- x[26:50]
y

# (4)
z <- x[1:25]

# (5)
col_yz <- cbind(y, z)
col_yz

line_yz <- rbind(y, z)
line_yz

# (6)
class(col_yz)
class(line_yz)

# (7)
subgroup <- x[x < 57 | x >= 83]

# ==== Exercise 7 (5.1 Opening/Saving) ====
# Use install.packages if necessary
library("haven")
fela <- read_dta("course_material/Fearon+Laitin-2003.dta")
class(fela)

# ==== Homework (5.1 Opening/Saving) ====
usel_dta <- read_dta("course_material/uspresidentialelections.dta")

# I usually don't need row names in my csv files. Instead of comma separation,
# I use ; because my French operating system tells Excel to look for ;
# Annoying, I know...
write.table(usel_dta, file = "uspresidentialelections.csv", sep = ";",
            row.names = FALSE)

# Small correction: If you use write.csv(), sep is automatically set to "," and
# you cannot change it. write.table is functionally equivalent, but with 
# different default options and more arguments.

usel_csv <- read.table("uspresidentialelections.csv", sep = ";",
                       header = TRUE)

# ==== Exercise 8 (5.2 Exploring Structure) ====
fela <- as.data.frame(fela)
summary(fela)

# Pro Tipp: Subset the data where variables have missing values. Use the help
# function to find out more about the colSums() and which() function.
NA_count_per_col <- colSums(is.na(fela))
variables_with_NA <- which(NA_count_per_col > 0)
summary(fela[, variables_with_NA])

# ==== Exercise 9 (5.3 Subsetting) ====
# Countries in the data.frame:
unique(fela$cname)

# How many 'democratic' country-year observations experienced war
table(fela[fela$polity2 > 7, 'war'])

# Use the unique function to arrive at the same conclusio
unique(fela[fela$polity2 > 7 & fela$war == 1, 'cname'])

# Missing polity while war
fela[fela$war == 1 & is.na(fela$polity2), c('cname', 'year', 'polity2', 'war')]

# ==== Homework (5.3 Subsetting) ====
# Polity2: Albania vs. Algeria, 1989 to 1999
fela[fela$cname %in% c("ALBANIA", "ALGERIA") & fela$year %in% c(1989:1999), 
     c("cname", "year", "polity2")]

# Oil exporters
oil_exporters <- unique(fela[fela$Oil == 1, 'cname'])
oil_exporters
length(oil_exporters)

# ==== Exercise 10 (5.4 Manipulating/Recoding) ====
# New logged population
fela$new_logged_pop <- log(fela$pop)

# New polity category variable
fela$polity_cat <- cut(fela$polity2,
                       breaks = c(-10, -1, 0, 7, 10),
                       labels = c("Autocracy", "Trumpocracy", "Anocracy", "Democracy"))
table(fela$polity2, fela$polity_cat)

# Recode polity2 missing to -20
fela$polity2_new <- ifelse(is.na(fela$polity2), -20, fela$polity2)

# or
fela$polity2_new <- fela$polity2
fela$polity2_new[is.na(fela$polity2_new)] <- -20

# ==== Homework (5.4 Manipulating/Recoding) ====
# Create european colony dummy
fela$european_colony <- ifelse(fela$colbrit == 1 | fela$colfra == 1, 1, 0)

# Double check whether the counts add up:
table(fela$colbrit) + table(fela$colfra)
table(fela$european_colony)

# Normalize/standardize/scale/mean-centre the polity2 variable
fela$polity2_std <- round(scale(fela$polity2), 2)

table(fela$polity2, fela$polity2_std)

# ==== Homework (5.5 Merging/Appending/Reshaping) ====
# This one is up to you. Follow the examples in the script. Use the help function
# and go back to google if you encounter a specific problem.

# ==== Homework (5.6 Missing Values) ====
# The zelig package was supposed to be the solution to all these problems as it
# quickly averaged model results over the multiply imputed datasets. But it 
# seems that the developers (a group related to the Harvard quants center) has
# semi-abondaned the package.

# I haven't dealt with this for a while, but I used loops to extract coeffients
# etc. from the regression object, and average them over all imputed datasets.
# This required some matrix math, but worked fine for me. This entry suggests
# to use the lapply function, which should be very similar:
# https://stats.stackexchange.com/questions/117605/lmer-with-multiply-imputed-data

# Shoot me a message if you found an awesome solution that is better than this.
# Again, google is your friend!

# ==== Homework (7.2 Generalized Linear Models) ====
# Replication of Fearon+Laitin-2003. 
# Before we start, let's load the original data again to prevent our previous 
# playing around with it to mess with our replication attempt. Also, let's clear
# our R environment.
library("haven")
rm(list = ls())
fela <- read_dta("course_material/Fearon+Laitin-2003.dta")

# Let's take: Table1. Logit Analysis of Determinants of Civil War Onset, 1945-1999 (p. 84)
# Model (1) Civil War:
# Prior war
# Per capita income
# log(population)
# log(% mountainous)
# Noncontiguous state
# Oil exporter
# New state
# Instability
# Democracy
# Ethnic fractionalization
# Religious fractionalization

# Here is the .do file replication command
# /* Model #1 */
# logit onset warl gdpenl lpopl lmtnest ncontig Oil nwstate instab polity2l ethfrac relfrac ,nolog
fmla <- onset ~ warl + gdpenl + lpopl + lmtnest + ncontig + Oil + nwstate + instab + polity2l + ethfrac + relfrac

mod1 <- glm(fmla, data = fela, family = binomial(link = "logit"))

# The variable lpopl is not in the data. We assume that Fearon and Laitin meant
# to specify lpopl1, so let's change this and try again.
fmla <- onset ~ warl + gdpenl + lpopl1 + lmtnest + ncontig + Oil + nwstate + instab + polity2l + ethfrac + relfrac

mod1 <- glm(fmla, data = fela, family = binomial(link = "logit"))

# Another error message suggests that our dependent variable is not binary at 
# all. Let's check:
table(fela$onset)
fela[!c(fela$onset %in% c(0, 1)), c('cname', 'year', 'onset')]

# I don't know what happened in Russia in 1946, but let's drop this observation
# for the sake of this replication attempt. If we assing a missing value, the
# glm function will automatically drop the entire row (listwise deletion).
fela[!c(fela$onset %in% c(0, 1)), c('onset')] <- NA

# Let's try again
mod1 <- glm(fmla, data = fela, family = binomial(link = "logit"))
summary(mod1)

# We did it! And the numbers match up in an acceptable way. What do you think
# caused the slight deviation? There might be some slight difference in the 
# maximum likelihood function that R uses to fit the model, vs. that of Stata.
# It also might come from lpopl not being entirely the same as lpopl1. Or did
# we forget to specify something? Shoot me a message if you think I overlooked
# something!

# Model Fit (my personal favourite to complain about the accuracy of social
# science modelling and sometimes ludicrous interpretations thereof).
install.packages("BaylorEdPsych")
library("BaylorEdPsych")
PseudoR2(mod1)

# What kind of conclusions can you draw from these Pseudo-R-squared model fit
# measures? Are they really useful? I think it depends way too much on which 
# specification you choose. And the publication craze really incentivises you
# to choose the one that is most striking. To put it bluntly: I think using them 
# is tantamount to misleading your readers about your data.

# Pro Question: Why can we not calculate Count and Adj.Count PseudoR2s? Why does
# it show NA? Understand that these two juxtapose predicted y with observed y. 
# Since logit regression only produces predicted probabilities of observing a
# civil war (1) in a specific case, this relies on setting a threshold for when
# to consider a predicted y to be 1 and when 0. The typical threshold for these
# PseudoR2 versions are 50% chance of observing 1. 
# If we look at the distribution of our y_hat, it becomes clear that none of the
# observations meets this threshold. So Fearon and Laitin's model would never
# properly predict the onset of a civil war, which doesn't align with the 
# observed data.
plot(density(y_hat))

# We can use separationplots to understand this a bit better.
install.packages("separationplot")
library("separationplot")

y_hat <- predict(mod1, type = "response")

separationplot(y_hat, as.numeric(mod1$y))

# How to read this graph? Understand it's three elements:
# This kind of graph attempts to overcome the intrinsically arbitrary nature of 
# the pseudo-R-squared approaches by giving a visual, rather than numerical, 
# impression of how well the models can ’separate’ observations with and without 
# civil war onset. 
# The first element of the graph is that it depicts the 6610 country-year 
# observations of the data on the horizontal axis, covering 161 countries from 
# 1945 to 1999 
# The second element is that it shows the predicted probability of the onset of 
# a civil war for each country-year on the vertical axis, indicated by the black 
# line. The country-year observations are ordered in a way that those with lower 
# probabilities are on the left, and those with higher probabilities are on 
# right side of the graph. 
# The third and final element of the graph is that it highlights the observations 
# with CSDP missions by vertical lines. 
# These three elements combined juxtapose the hypothetical predictions of a 
# mission based on the model, with the factual observation of the CSDP missions. 
# The more those two coincide, the better the model-fit.

# Once we understand the logic of these graphs, we can make it look nicer and
# publication ready with the predict() function and ggplot2. Let's also add an
# 95% uncertainty interval to the predictions while we're at it.
library("ggplot2")

pred <- predict(mod1, type = "response", se.fit = TRUE)
df_sep <- data.frame("y_hat" = pred$fit, "y" = mod1$y)
df_sep$low95 <- pred$fit - 1.959964 * pred$se.fit
df_sep$hig95 <- pred$fit + 1.959964 * pred$se.fit

# Let's order the dataframe according to the highest predicted probability:
df_sep <- df_sep[order(df_sep$y_hat),]
df_sep$x <- 1:nrow(df_sep)

sep_plot <- ggplot(df_sep, aes(x = x)) +
  theme(plot.title = element_text(size = rel(1.25), hjust = 0.5, face = 'bold'),
        panel.background = element_blank(),
        panel.border = element_rect(colour = 'grey50', fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Separation Plot for Model (1)",
       x = 'Observations Ordered from Lowest to Highest Predicted Probability',
       y = 'Predicted Probability of Civil War Onset') +
  geom_vline(data = df_sep[df_sep$y ==1, ],
             aes(xintercept = x), colour = "darkred", alpha = .7) +
  geom_ribbon(aes(ymax = hig95, ymin = low95), fill = 'grey', alpha = .7) +
  geom_line(aes(y = y_hat), size = 1.1)

# Yes, this might look a bit over the top for you, but once you understand that
# you just stack things that you want to add to the ggplot with +, it should 
# become more transparent. Let's look at it:
sep_plot

# And save it as PDF
pdf("fela_separationplot.pdf", width = 10, height = 5)
sep_plot
dev.off()

# ==== Homework (7.3 Multilevel Models) ====
# Let's add random intercept per country and year to account for local and 
# time specific ideosyncracies.
library("lme4")
fmla <- onset ~ warl + gdpenl + lpopl1 + lmtnest + ncontig + Oil + nwstate + instab + polity2l + ethfrac + relfrac + (1 | cname) + (1 | year)

mod1_multilevel <- glmer(fmla, data = fela, family = binomial(link = "logit"))

# Yes, this takes a while longer. Depending on your model structure, you might 
# want to consider looking at parallel processing to use all the CPU cores of 
# your computer. Use google as your friend and helper.
summary(mod1_multilevel)

# Interestingly, this does not change too much about the regression results, and
# variance of intercepts across these clusters is minimal. Take your time to 
# think about why that is, and whether you want to try out random slopes for 
# specific variables. Also: Does it really matter as the explanatory significance
# of these indicators is rather weak in the first place?
# Let's check what multilevel models do to our separation plot. First of all,
# as you will find out, it is difficult to calculate standard errors for 
# predictions with these models. There are some bootstrapping techniques 
# (see bootMer function), but let's leave that to a course on multilevel 
# modelling.

pred <- predict(mod1_multilevel, type = "response")
df_sep <- data.frame("y_hat" = pred, "y" = mod1$y)

# Let's order the dataframe according to the highest predicted probability:
df_sep <- df_sep[order(df_sep$y_hat),]
df_sep$x <- 1:nrow(df_sep)

sep_plot <- ggplot(df_sep, aes(x = x)) +
  theme(plot.title = element_text(size = rel(1.25), hjust = 0.5, face = 'bold'),
        panel.background = element_blank(),
        panel.border = element_rect(colour = 'grey50', fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Separation Plot for Model (1)",
       x = 'Observations Ordered from Lowest to Highest Predicted Probability',
       y = 'Predicted Probability of Civil War Onset') +
  geom_vline(data = df_sep[df_sep$y ==1, ],
             aes(xintercept = x), colour = "darkred", alpha = .7) +
  geom_line(aes(y = y_hat), size = 1.1)

# Yes, this might look a bit over the top for you, but once you understand that
# you just stack things that you want to add to the ggplot with +, it should 
# become more transparent. Let's look at it:
sep_plot

# And save it as PDF
pdf("fela_separationplot_multilevel.pdf", width = 10, height = 5)
sep_plot
dev.off()

# Comparing the two suggests that a typical time-series cross-sectional random
# intercept specification does not much. Neither in terms of unearthing 
# ommitted variable bias, nor in terms of model fit. 
# My personal interpretation of these separationplots is that this sets the 
# field of quantitative civil war studies to square one when it comes to 
# explaining civil war onset.

# ==== Homework (7.4 Margins Package) ====
# Let's check the margins package and see what it offers.
# You can generate average marginal effects quickly. Much more efficient than
# doing it by hand. I admit it.
margins(mod1, type = "response")

# And you can look at the general effect of a specific variable. Handy!
cplot(mod1, x = "nwstate", se.type = "shade")

# But, let's get more creative and think about the implications of our models
# in more tangible and realistic circumstances. For example, let's see what our 
# (bad) model tells us would have happened, if Afghanistan was a new state 
# according to Fearon and Laitin's definition.
fela_afgh_hypo <- fela[fela$cname == "AFGHANIS", ]
fela_afgh_hypo$nwstate <- 1
fela_afgh_hypo$scenario <- "Hypothetical"

fela_afgh_real <- fela[fela$cname == "AFGHANIS", ]
fela_afgh_real$scenario <- "Real"

fela_afgh <- rbind(fela_afgh_real, fela_afgh_hypo)

pred <- predict(mod1, newdata = fela_afgh, type = "response", se.fit = TRUE)

fela_afgh$y_hat <- pred$fit
fela_afgh$low95 <- pred$fit - 1.959964 * pred$se.fit
fela_afgh$hig95 <- pred$fit + 1.959964 * pred$se.fit

ggplot(fela_afgh, aes(x = year, group = scenario, 
                     fill = scenario, linetype = scenario)) +
  geom_point(aes(y = onset), colour = "black") +
  geom_line(aes(y = y_hat), colour = "black") +
  geom_ribbon(aes(ymin = low95, ymax = hig95), alpha = .7)

# First, notice that 18 observations went missing because some of the 
# explanatory variables have missing values. The other thing, this graph alerts
# us to is the operationalization of the dependent variable. It stressed that
# only the year a civil war started are coded as one (see the black dots), and
# all other observations are 0. Yet, all the post-onset observations might
# introduce endogeneity problems, don't they?

# But, how do I interpret this graph? It shows both the substantive significance
# of the new state variable, as well as the uncertainty in the model. Ignoring 
# the confidence interval, we might say that new states are significantly more
# likely to experience civil war. With the confidence interval, we might 
# conclude that this effect is potentially marginal in the case of Afghanistan,
# considering all the other things that are going on. 

# Moreover, it again illustrates on this specific case, that the model is rather
# unconcinving in its ability to actually predict/explain civil war onset. 
# According to the model predictions, Afghanistan became substantially less 
# likely to experience civil war exactly during the time when it experienced
# them.






