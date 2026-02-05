#//////////////////////////////////////////////////////----
# Mandatory Assignment ----
#//////////////////////////////////////////////////////----

# load packages ----
#.................................----

#install.packages("lmtest")
#install.packages("sandwich")
#install.packages("readxl")
#install.packages("kableExtra")
#install.packages("ggplot2")
library(ggplot2)
library(readxl)
library(lubridate)
library(scales)
library(lmtest)
library(sandwich)
library(dplyr)
library(moments)
library(kableExtra)

# load Data
rm(list = ls())
setwd("C:/Users/alasl/OneDrive/Desktop/Finance Assignment")
getwd()

FFfactors <- read.csv("FFFactors.csv",skip = 4, header = T, sep=",", quote = "\"", dec = ".", fill =T, comment.char="")
SP500 <- read_excel("C:/Users/alasl/OneDrive/Desktop/Finance Assignment/SP500.xlsx", sheet="Sheet1")

#.................................
# Data Cleaning----
#.................................----

# Change the Format 
  FFfactors$Date <- ymd(FFfactors$Date)
  SP500$Name <- ymd(SP500$Name)

#Create new Dataframe with the important Data
  SP500 <- data.frame(Date= SP500$Name,ColgatePalm = SP500$`COLGATE-PALM.`,Etsy=SP500$ETSY)

#let both start from the same Date (first observation of the Etsy stock)
  SP500 <- SP500[SP500$Date >= "2015-04-16" ,]
  FFfactors <- FFfactors[FFfactors$Date >= "2015-04-16" , ]


# delete NA´s
  SP500 <- na.omit(SP500)
  FFfactors <- na.omit(FFfactors)


# rename Mkt.RF to prevent further problems
  colnames(FFfactors)[which(colnames(FFfactors) == "Mkt.RF")] <- "Mkt_RF"

# create list to store the final results
  results <- list()
#.................................
# Data processing ----
#.................................----

# compute the net Returns for CAPM and Fama French
  returns <- na.omit(data.frame(Date = SP500$Date,
                              ColgatePalm =c(NA,(SP500$ColgatePalm[-1]/SP500$ColgatePalm[-length(SP500$ColgatePalm)] -1)),
                              Etsy = c(NA,(SP500$Etsy[-1]/SP500$Etsy[-length(SP500$Etsy)] -1) )) )
  
# compute net returns of the Portfolio (50/50)
  PF <- (returns$ColgatePalm + returns$Etsy)/2
  returns <- cbind(returns,PF)

# calculating decimal values in Percent values
  returns[,2:4] <- returns[,2:4] *100

# merge the stock and Portfolio Returns with the Market data
  CAPMdat <- merge(returns,FFfactors, by = "Date", all = F)

# create a new returns data-frame to compare each returns with each toher inclusing the market
  returnsCEPM <- CAPMdat[,1:4]
  returnsCEPM$Market <- CAPMdat$Mkt_RF+CAPMdat$RF 
  
#.................................
# Descriptive Analysis----
#.................................----
# 4.4


descr_returnsCAPM <- rbind(
  mean = returnsCEPM %>% summarise_if(is.numeric, list(~mean(., na.rm = TRUE))),
  median = returnsCEPM %>% summarise_if(is.numeric, list(~median(., na.rm = TRUE))),
  var = returnsCEPM %>% summarise_if(is.numeric, list(~var(., na.rm = TRUE))),
  sd = returnsCEPM %>% summarise_if(is.numeric, list(~sd(., na.rm = TRUE))),
  skew = returnsCEPM %>% summarise_if(is.numeric, list(~skewness(., na.rm = TRUE))),
  kurt = returnsCEPM %>% summarise_if(is.numeric, list(~kurtosis(., na.rm = TRUE))),
  min = returnsCEPM %>% summarise_if(is.numeric, list(~min(., na.rm = TRUE))),
  max = returnsCEPM %>% summarise_if(is.numeric, list(~max(., na.rm = TRUE)))
)
results$descrReturs <- descr_returnsCAPM

#.................................
# Plot----
#.................................----
pdf("cumulativeValueOf1$InvestmentPlot.pdf", width = 10, height = 4 )
cumulativeReturns <- CAPMdat %>%
  mutate( CumColgatePal = cumprod(1+ ColgatePalm/100) ,
          CumEtsy = cumprod(1+Etsy/100) ,
          CumPortfolio = cumprod(1+ PF/100) ,
          CumMarket = cumprod(1+ (Mkt_RF -RF)/100))
ggplot(cumulativeReturns, aes(x = Date)) +
  geom_line(aes(y = CumColgatePal, color = "Colgate-Palmolive"), size = 1) +
  geom_line(aes(y = CumEtsy, color = "Etsy"), size = 1) +
  geom_line(aes(y = CumPortfolio, color = "Portfolio"), size = 1) +
  geom_line(aes(y = CumMarket, color = "Market"), size = 1) +
  labs(y = "Cumulative value ($)", x = "Date", color="Legend") +
  scale_y_continuous(breaks = seq(0, 10, by = 1), minor_breaks = seq(-1, 9, by = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("blue", "red", "green", "black")) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = c(0.01, 0.99), # Position legend at the top left
    legend.justification = c(0, 1),
    legend.background = element_rect(colour = "black", linetype = "solid"),
    panel.grid.major = element_line(color = "gray90"), 
    panel.grid.minor = element_line(color = "gray90"),
    panel.background = element_rect(fill = "white", color = "black"),
    plot.background = element_rect(fill = "white", color = "white") 
  ) +
  coord_cartesian(xlim = c(min(cumulativeReturns$Date),as.Date("2024-06-01")),ylim = c(0,10.2), expand=F)
dev.off()

#.................................
# Estimation of the CAPM ----
#.................................----

# compute excess returns for each asset
exret <- CAPMdat
exret$ColgatePalm <- exret$ColgatePalm-exret$RF
exret$Etsy <- exret$Etsy-exret$RF
exret$PF <- exret$PF-exret$RF
exret$Mkt_RF <-exret$Mkt_RF 

# compute excess returns
CAPM_Etsy <- lm(exret$Etsy ~ exret$Mkt_RF)
CAPM_ColgatePalmolive <- lm(exret$ColgatePalm ~ exret$Mkt_RF, data = exret)
CAPM_PF <- lm(exret$PF ~ exret$Mkt_RF, data=exret)

CAPM_Etsy_Coef <- coeftest(CAPM_Etsy, vcov = NeweyWest(CAPM_Etsy, lag = 1))
CAPM_ColatePalmolive_Coef <- coeftest(CAPM_ColgatePalmolive, vcov = NeweyWest(CAPM_ColgatePalmolive, lag = 1))
CAPM_Portfolio_Coef <- coeftest(CAPM_PF,vcov = NeweyWest(CAPM_PF, lag = 1))


#store results
results$CAPM_Etsy <- CAPM_Etsy_Coef
results$CAPM_ColgatePalmolive <- CAPM_ColatePalmolive_Coef
results$CAPM_Portfolio <- CAPM_Portfolio_Coef
#.................................
# Estimation of the Fama-French Model----
#.................................----

# compute Fama-French Model
#HML represent the return of a portfolio of stocks with a high book-to-market
#ratio in excess of the return on a portfolio of stocks with a low book-to-market ratio,
#and SMB the return of a portfolio of small stocks in excess of the return on a
#portfolio of large stocks.

ff_model_colgate <- lm(exret$ColgatePalm ~ exret$Mkt_RF + exret$SMB + exret$HML)
ff_model_Etsy <- lm(exret$Etsy ~ exret$Mkt_RF + exret$SMB + exret$HML)
ff_model_PF <- lm(exret$PF ~ exret$Mkt_RF + exret$SMB + exret$HML )


FFM_ColgatePalmolive <- coeftest(ff_model_colgate, vcov = NeweyWest(ff_model_PF, lag = 1))
FFM_Etsy <- coeftest(ff_model_Etsy, vcov = NeweyWest(ff_model_PF, lag = 1))
FFM_Portfolio <- coeftest(ff_model_PF, vcov = NeweyWest(ff_model_PF, lag = 1))

#store result ff_Model
results$FFM_ColgatePalmolive <- FFM_ColgatePalmolive
results$FFM_Etsy <- FFM_Etsy
results$FFM_Portfolio <- FFM_Portfolio





