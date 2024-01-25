setwd("D:/METOPEL UAS/ILA METOPEL")
library(readxl)
library(tidyverse)
library(kableExtra)
read_excel("industri.xlsx")
dat <- read_excel("industri.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# regresi
reg1<-lm(ekspor~sawit+bbm+kertas+besi,data=dat)
summary(reg1)

# Plot 
plot(dat$tahun,dat$ekspor,xlab="Tahun",ylab="Nilai FOB Ekspor Total Indonesia ")
plot(dat$tahun,dat$sawit,xlab="Tahun",ylab="Nilai FOB Sawit")
plot(dat$tahun,dat$bbm,xlab="Tahun",ylab="Nilai FOB Bidang Perminyakan")
plot(dat$tahun,dat$kertas,xlab="Tahun",ylab="Nilai FOB Ekspor Industri Kertas")
plot(dat$tahun,dat$besi,xlab="Tahun",ylab="Nilai FOB Ekspor Besi dasar")

# Plot Error

dat$m<-resid(reg1)
plot(dat$ekspor,dat$m,xlab="Nilai Ekspor Keseluruhan",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$sawit,dat$m,xlab="Nilai Ekspor Minyak Sawit",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$bbm,dat$m,xlab="Nilai Ekspor Perminyakan",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$kertas,dat$m,xlab="Nilai Ekspor Industri Kertas",ylab="error")
abline(h=0) # membuat garis horizontal di y=0

dat$m<-resid(reg1)
plot(dat$besi,dat$m,xlab="Nilai Ekspor Industri Besi Dasar",ylab="error")
abline(h=0) # membuat garis horizontal di y=0
