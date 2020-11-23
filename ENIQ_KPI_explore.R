# explore the ENIQ KPI files
install.packages('mosaic')
library(mosaic)

# read in the file
# C:\Users\lmcstro\Documents\ENIQ CM viz data folder
setwd("C:/Users/lmcstro/Documents/ENIQ CM viz data folder")
list.files()
df.kpi.values <- read.csv("test_CM_cluster_KPI_values.csv",
               header = TRUE, stringsAsFactors = FALSE)
str(df.kpi.values)

# convert numeric parameters back to numeric
df.kpi.values[, c(5:13)] <- sapply(df.kpi.values[, c(5:13)], as.numeric)

# examine for NA

for(c in colnames(df.kpi.values)) {
  print(c)
  print(
    paste("percentage Available", 
          (length(df.kpi.values[,c]) - sum(is.na(df.kpi.values[,c]))) /
                                  length(df.kpi.values[,c]) ))
}

# Remove NAs
#head(!is.na(df.kpi.values[,c]))
for(c in colnames(df.kpi.values)) {
  df.kpi.values <- df.kpi.values[!is.na(df.kpi.values[,c]),]
}
str(df.kpi.values)


df.kpi.values[1:10,c(1:4)]
max(df.kpi.values$DATE)
min(df.kpi.values$DATE)

unique(df.kpi.values$EUtranCellFDD)

v.eutran <- unique(df.kpi.values$EUtranCellFDD)
colnames(df.kpi.values[c(5:13)])

plot(df.kpi.values[df.kpi.values$EUtranCellFDD =="AICC1R",c("KPI_AvgRrcUsers_h")],type = "l", lty = 1)
plot(df.kpi.values[df.kpi.values$EUtranCellFDD =="AICC2R",c("KPI_ErabSuccessRate_h")],type = "l", lty = 1)
plot(df.kpi.values[df.kpi.values$EUtranCellFDD =="AICE2M",c("KPI_ErabSuccessRate_h")],type = "l", lty = 1)


plot(df.kpi.values[df.kpi.values$EUtranCellFDD =="MIDE2O",
        c("KPI_ErabSuccessRate_h")],type = "l", lty = 1,
        ylab=" ",
        main="MIDE2O / KPI_ErabSuccessRate")
abline(v=(9*24),col=c("blue"))

par(mfrow=c(3,1))

plot(df.kpi.values[df.kpi.values$EUtranCellFDD =="MIDE2O",
                   c("KPI_ErabSuccessRate_h")],type = "l", lty = 1,
     ylab=" ",
     main="MIDE2O / KPI_ErabSuccessRate")
abline(v=(9*24),col=c("blue"))
plot(df.kpi.values[df.kpi.values$EUtranCellFDD =="MIDE2O",
                   c("KPI_ErabSuccessRate_h")],type = "l", lty = 1,
     ylab=" ",
     main="MIDE2O / KPI_ErabSuccessRate")
abline(v=(9*24),col=c("blue"))


# Sample some cells to normalize the seasonable data and see what he relative range is between hours
f.minmax <- function (x){
  #print("")
  #print(head(x))
  #print(min(x))
  #print(max(x))
  return ((x - min(x))/(max(x)-min(x))) 
}

set.seed(1234)
x.sample.size <- 100
v.sample.cells <- sample(unique(df.kpi.values$EUtranCellFDD),x.sample.size)

# KPIs that are seasonal
v.seasonal <- c("KPI_AvgRrcUsers_h", "KPI_UeThroughputDl_h", "KPI_CellThroughputDl_h")
v.col.select <- append(c("DATE", "ELEMENT", "EUtranCellFDD", "Time_n"),v.seasonal)
df.sample <- df.kpi.values[df.kpi.values$EUtranCellFDD %in% v.sample.cells,v.col.select]
v.cells <- unique(df.sample$EUtranCellFDD)
str(df.sample)


# normalize seasonal value max-min so they range from 0 to 1, per cell

for (c in v.cells) {
  df.sample[df.sample$EUtranCellFDD == c,c(5:7)] <- 
    sapply(df.sample[df.sample$EUtranCellFDD == c,c(5:7)], f.minmax)
}

str(df.sample)

max(df.sample[,c(5:7)])

# Now consider how it looks from hourly basis
par(mfrow=c(1,1))

boxplot(KPI_AvgRrcUsers_h~Time_n,
        data=df.sample,
        main="KPI_AvgRrcUsers per hour",
        xlab="hour of day",
        ylab="relative score",
        col="orange",
        border="brown")
boxplot(KPI_UeThroughputDl_h~Time_n,
        data=df.sample,
        main="KPI_UeThroughputDl_h per hour",
        xlab="hour of day",
        ylab="relative score",
        col="orange",
        border="brown")
boxplot(KPI_CellThroughputDl_h~Time_n,
        data=df.sample,
        main="KPI_CellThroughputDl_h per hour",
        xlab="hour of day",
        ylab="relative score",
        col="orange",
        border="brown")

# Now look at the non-seasonal
#  
# - KPI_RrcSuccessRate_h
# - KPI_S1SuccessRate_h
# - KPI_ErabSuccessRate_h
# - KPI_Retainability_h
# - KPI_Accessibility_h
# - KPI_Availability_h

v.col.select <- c("KPI_RrcSuccessRate_h", "KPI_S1SuccessRate_h", 
                  "KPI_ErabSuccessRate_h", "KPI_Retainability_h",
                  "KPI_Accessibility_h","KPI_Availability_h")
str(df.kpi.values)

summary(df.kpi.values[,v.col.select])
# data is already ranging from 0-1 across all the parameters

boxplot(KPI_RrcSuccessRate_h~Time_n,
        data=df.kpi.values,
        main="KPI_CellThroughputDl_h per hour",
        xlab="hour of day",
        ylab="relative score",
        col="orange",
        border="brown")

hist(df.kpi.values$KPI_RrcSuccessRate_h, breaks = 100)

df.sample.1 <- df.kpi.values[df.kpi.values$EUtranCellFDD %in% v.sample.cells,]

boxplot(KPI_RrcSuccessRate_h~Time_n,
        data=df.sample.1,
        main="KPI_RrcSuccessRate_h per hour",
        outline=FALSE,
        xlab="hour of day",
        ylab="relative score",
        col="orange",
        border="brown")

KPI_Retainability_h 

boxplot((1-KPI_Retainability_h)~Time_n,
        data=df.sample.1,
        main="KPI_Retainability_h per hour",
        outline=FALSE,
        xlab="hour of day",
        ylab="relative score",
        col="orange",
        border="brown")

hist(df.kpi.values[,v.col.select[1]], breaks = 100, main = c)


for (c in v.col.select){
  hist(df.kpi.values[,c], breaks = 100)
}

# mostly 1 or 0.  rare to find cases below

favstats(KPI_RrcSuccessRate_h ~Time_n, data=df.sample.1)


## EXPERIMENTAL CODE

x.a <- boxplot(KPI_CellThroughputDl_h~Time_n,
               data=df.sample,
               main="KPI_CellThroughputDl_h per hour",
               xlab="hour of day",
               ylab="relative score",
               col="orange",
               border="brown")
str(x.a)
x.a$stats

favstats(KPI_CellThroughputDl_h~Time_n, data=df.sample)

