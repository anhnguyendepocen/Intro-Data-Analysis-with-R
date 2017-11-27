# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~ R-Script with snippets from the workshop:
# ~ Introduction to Data Analysis with R
# ~ 23 & 24 November 2017
#
# ~ last updated: 2017-11-27
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Refers to Script-v.0.6-2017-11-23 from the Introduction to Data Analysis with 
# R workshop, tought at the European University Institute on 23 & 24 November
# 2017.

if (!isTRUE(require("haven"))) {
  install.packages("haven")
}

if (!isTRUE(require("MASS"))) {
  install.packages("MASS")
}

if (!isTRUE(require("separationplot", quietly = TRUE))) {
  install.packages("separationplot")
  require("separationplot")
}



