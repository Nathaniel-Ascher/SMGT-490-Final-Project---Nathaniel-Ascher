install.packages("devtools")
devtools::install_github(repo = "saberpowers/sabRmetrics")
library(sabRmetrics)

setwd("~/Downloads")
savant2024 = read.csv("savant2024.csv")
savant2025 <- sabRmetrics::download_baseballsavant(
  start_date = "2025-03-27",
  end_date = "2025-09-28")
