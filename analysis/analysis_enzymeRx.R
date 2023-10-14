### INFO
# project: Project #27: The effect of COVID-19 on pancreatic cancer diagnosis and care
# subproject: NICE Quality statement 4: Pancreatic enzyme replacement therapy
# author: Agz Leman
# 28 Feb 2022
# Plots monthly rates 
###

## library
library(tidyverse)
library(here)
library(MASS)


###
#download and prep the data
###

ERx_Rates <- read_csv(here::here("output", "measures", "measure_enzymeRx_rate.csv"))
####when using downloaded data 
#ERx_Rates <- read.csv("C:/Users/al0025/OneDrive - University of Surrey/OldHomeDrive/al0025/Documents/OpenSafely/Projects/ERx/Output release/Archive/measure_enzymeRx_rate.csv")
#ERx_Rates$date <- as.Date(ERx_Rates$date, format = "%Y-%m-%d")

ERx_Rates_rounded <- as.data.frame(ERx_Rates)
for (i in 1:2){
  ERx_Rates_rounded[,i] <- plyr::round_any(as.data.frame(ERx_Rates)[,i], 5, f = round)}
ERx_Rates_rounded$value <- ERx_Rates_rounded$enzyme_replace/ERx_Rates_rounded$population
# calc rate per 100
ERx_Rates_rounded$rate <- ERx_Rates_rounded$enzyme_replace / ERx_Rates_rounded$population * 100
### save the rounded file 
write.table(ERx_Rates_rounded, here::here("output", "ERx_Rates_rounded.csv"),sep = ",",row.names = FALSE)
###### cut date that is incomplete (thypically last month)
#cut_date2 <- "2023-01-01"
#a <- which(ERx_Rates_rounded$date > as.Date(cut_date2, format = "%Y-%m-%d"))
#ERx_Rates_rounded <- ERx_Rates_rounded[-a,]

###
# plot monthly number of Rxs
###
p <- ggplot(data = ERx_Rates_rounded,aes(date, enzyme_replace)) +
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "2 month",
               date_labels = "%Y-%m")+
  labs(title = "Patients receiving enzyme replacement", 
       x = "Time", y = "Number of patients")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
start <- "2020-03-01"
p <- p + geom_vline(xintercept=as.Date(start, format="%Y-%m-%d"), size=0.3, colour="red")
guidel <- "2018-02-01"
p <- p + geom_vline(xintercept=as.Date(guidel, format="%Y-%m-%d"), size=0.3, colour="blue")

# save
ggsave(
  plot= p, dpi=800,width = 20,height = 10, units = "cm",
  filename="ERx_number.png", path=here::here("output"),
)

###
# plot monthly rates per 100
###
p <- ggplot(data = ERx_Rates_rounded,
                          aes(date, rate)) +
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "2 month",
               date_labels = "%Y-%m")+
  labs(title = "Patients receiving enzyme replacement", 
       x = "Time", y = "Rate per 100 patients diagnosed")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
start <- "2020-03-01"
p <- p + geom_vline(xintercept=as.Date(start, format="%Y-%m-%d"), size=0.3, colour="red")
guideli <- "2018-02-01"
p <- p + geom_vline(xintercept=as.Date(guideli, format="%Y-%m-%d"), size=0.3, colour="blue")
QS <- "2018-12-01"
p <- p + geom_vline(xintercept=as.Date(QS, format="%Y-%m-%d"), size=0.3, colour="darkgreen")
p <- p +  geom_text(aes(x=as.Date(QS, format="%Y-%m-%d"), y=25), 
                    color = "darkgreen",label="Quality\nstandard", angle = 90, size = 3)
# save
ggsave(
  plot= p, dpi=800,width = 20,height = 10, units = "cm",
  filename="ERx_rates.png", path=here::here("output"),
)

########## model the data 

model_data <- subset(ERx_Rates_rounded, select=c("rate","date"))
model_data$lockdown <- 0 
model_data$guideline <- 0

# periods
start <- "2020-03-01"
guideli <- "2018-02-01"

 
model_data2 <- model_data[1:dim(model_data)[1],]
# censor the analysis - cut two months at the end
#model_data2 <- model_data[1:(dim(ERx_Rates_rounded)[1]-2),]
model_data2$time <- as.numeric(c(1:dim(model_data2)[1]))
model_data_no_covid <- model_data2
model_data2$guideline[model_data2$date>guideli & model_data2$date<=start]<-1
model_data2$lockdown[model_data2$date>start]<-1

model <- glm(rate ~ time 
                 + lockdown + lockdown*time
                 #+ guideline + guideline*time
             , data=model_data2)

model_data2$predicted <- predict(model,type="response",model_data2)
model_data2$predicted_no_covid <- predict(model,type="response",model_data_no_covid)

ilink <- family(model)$linkinv
model_data2 <- bind_cols(model_data2, setNames(as_tibble(predict(model, model_data2, se.fit = TRUE)[1:2]),
                                             c('fit_link','se_link')))

n <- 2
model_data2 <- mutate(model_data2,
                     pred  = ilink(fit_link),
                     upr = ilink(fit_link + (n * se_link)),
                     lwr = ilink(fit_link - (n * se_link)))
model_data2 <- bind_cols(model_data2, setNames(as_tibble(predict(model, model_data_no_covid, se.fit = TRUE)[1:2]),
                                             c('fit_link_noCov','se_link_noCov')))


model_data2 <- mutate(model_data2,
                     pred_noCov  = ilink(fit_link_noCov),
                     upr_noCov = ilink(fit_link_noCov + (n * se_link_noCov)),
                     lwr_noCov = ilink(fit_link_noCov - (n * se_link_noCov)))


p <- ggplot(data = model_data,aes(date, rate, color = "Recorded data", lty="Recorded data")) +
  geom_line()+
  geom_point()+
  scale_x_date(date_breaks = "3 month",
               date_labels = "%Y-%m")+
  labs(title = "Patients receiving enzyme replacement \n Region: whole England", 
       x = "", y = "Rate per 100 patients with \nunresectable pancreatic cancer")+
  theme_bw()+
  scale_y_continuous(limits = c(0, 70))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
start <- "2020-03-01"
p <- p + geom_vline(xintercept=as.Date(start, format="%Y-%m-%d"), size=0.3, colour="blue")
p <- p +  geom_text(aes(x=as.Date(start, format="%Y-%m-%d"), y=10), 
                    color = "blue",label="Beginning of\nCOVID-19 restrictions", angle = 90, size = 3)

guideli <- "2018-02-01"
p <- p + geom_vline(xintercept=as.Date(guideli, format="%Y-%m-%d"), size=0.3, colour="black")
p <- p +  geom_text(aes(x=as.Date(guideli, format="%Y-%m-%d"), y=10), 
                    color = "black",label="National\nguidelines", angle = 90, size = 3)

QS <- "2018-12-01"
p <- p + geom_vline(xintercept=as.Date(QS, format="%Y-%m-%d"), size=0.3, colour="darkgreen")
p <- p +  geom_text(aes(x=as.Date(QS, format="%Y-%m-%d"), y=10), 
                    color = "darkgreen",label="Quality\nstandard", angle = 90, size = 3)

p<-p+geom_line(data=model_data2, aes(y=predicted, color = "Model with COVID-19", lty="Model with COVID-19"), size=0.5)
#p<-p+geom_ribbon(data=model_data2, aes(ymin = lwr, ymax = upr), fill = "grey30", alpha = 0.1)
p<-p+geom_line(data=model_data2, aes(y=predicted_no_covid, color = "Model no COVID-19", lty="Model no COVID-19"), size=0.5)
p<-p+geom_ribbon(data=model_data2, aes(ymin = lwr_noCov, ymax = upr_noCov),color = "red",
                 lty=0, fill = "red", alpha = 0.1)
p <- p + labs(caption="OpenSafely-TPP October 2023")
p <- p + theme(plot.caption = element_text(size=8))
p <- p + theme(plot.title = element_text(size = 10))
p <- p + scale_color_manual(name = NULL, values = c("Model no COVID-19" = "red", "Recorded data" = "black", 
                                                 "Model with COVID-19" = "blue"),guide="none")
p <- p + scale_linetype_manual(name = NULL, values = c("Model no COVID-19" = "solid", "Recorded data" = "solid",
                                                   "Model with COVID-19" = "dashed"),guide="none")
p <- p + scale_fill_manual(name = NULL, values = c("Model no COVID-19" = "red", "Recorded data" = "white",
                                                    "Model with COVID-19" = "white"),guide="none")
p <- p + guides(colour = guide_legend(override.aes = list(linetype=c(1,2,1),fill=c("red",NA,NA), 
                                                          shape = c(NA, NA, 16))))

ggsave(
  plot= p, dpi=800,width = 20,height = 15, units = "cm",
  filename="model_rates.png", path=here::here("output"),
)

####
# plot by region 
####
###Download data 
Region <- read_csv(here::here("output", "measures", "measure_ExByRegion_rate.csv"))
# Round numerator and denominator 
Region_rounded <- as.data.frame(Region)
for (i in 2:3){
  Region_rounded[,i] <- plyr::round_any(Region_rounded[,i], 5, f = round)}
Region_rounded$value <- Region_rounded$enzyme_replace/Region_rounded$population
Region_rounded$rate <- Region_rounded$enzyme_replace / Region_rounded$population * 100
### save the rounded file 
write.table(Region_rounded, here::here("output", "Region_rounded.csv"),sep = ",",row.names = FALSE)
###### cut data that is after the cut off date 
#a <- which(Region_rounded$date > as.Date(cut_date2, format = "%Y-%m-%d"))
#Region_rounded <- Region_rounded[-a,]

p <- ggplot(data = Region_rounded,
            aes(date, rate, color = region, lty = region)) +
  geom_line()+
  #geom_point(color = "region")+
  scale_x_date(date_breaks = "3 month",
               date_labels = "%Y-%m")+
  labs(title = "Patients receiving enzyme replacement \n by Region in England", 
       x = "", y = "Rate per 100 patients with \nunresectable pancreatic cancer")+
  theme_bw()+
  scale_y_continuous(limits = c(0, 70))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
p <- p + labs(caption="OpenSafely-TPP October 2023")
p <- p + theme(plot.caption = element_text(size=8))
p <- p + theme(plot.title = element_text(size = 10))

start <- "2020-03-01"
p <- p + geom_vline(xintercept=as.Date(start, format="%Y-%m-%d"), size=0.3, colour="blue")
p <- p +  geom_text(aes(x=as.Date(start, format="%Y-%m-%d")+25, y=10), 
                    color = "blue",label="Beginning of\nCOVID-19 restrictions", angle = 90, size = 3)
guideli <- "2018-02-01"
p <- p + geom_vline(xintercept=as.Date(guideli, format="%Y-%m-%d"), size=0.3, colour="black")
p <- p +  geom_text(aes(x=as.Date(guideli, format="%Y-%m-%d"), y=10), 
                    color = "black",label="National\nguidelines", angle = 90, size = 3)

QS <- "2018-12-01"
p <- p + geom_vline(xintercept=as.Date(QS, format="%Y-%m-%d"), size=0.3, colour="darkgreen")
p <- p +  geom_text(aes(x=as.Date(QS, format="%Y-%m-%d"), y=10), 
                    color = "darkgreen",label="Quality\nstandard", angle = 90, size = 3)

# save
ggsave(
  plot= p, dpi=800,width = 20,height = 15, units = "cm",
  filename="Region.png", path=here::here("output"),
)
#
### regional 
#
name_vector <- names(table(Region_rounded$region))
fig_vector <- toupper(letters)[1: length(name_vector)]

########## 
###GLM per region model the data 
for (i in name_vector){
  Reg_r <- Region_rounded[which(Region_rounded$region==i),]
  
  model_data <- subset(Reg_r , select=c("rate","date"))
  model_data$lockdown <- 0 
  model_data$guideline <- 0
  # periods
  start <- "2020-03-01"
  guideli <- "2018-02-01"
  
  model_data2 <- model_data[1:dim(model_data)[1],]
  model_data2$time <- as.numeric(c(1:dim(model_data2)[1]))
  model_data_no_covid <- model_data2
  model_data2$guideline[model_data2$date>guideli & model_data2$date<=start]<-1
  model_data2$lockdown[model_data2$date>start]<-1
  
  model <- glm(rate ~ time + lockdown + lockdown*time, data=model_data2)
  
  model_data2$predicted <- predict(model,type="response",model_data2)
  model_data2$predicted_no_covid <- predict(model,type="response",model_data_no_covid)
  
  ilink <- family(model)$linkinv
  model_data2 <- bind_cols(model_data2, setNames(as_tibble(predict(model, model_data2, se.fit = TRUE)[1:2]),
                                                 c('fit_link','se_link')))
  n <-  2 # 2 for 95% CIs? 
  model_data2 <- mutate(model_data2,
                        pred  = ilink(fit_link),
                        upr = ilink(fit_link + (n * se_link)),
                        lwr = ilink(fit_link - (n * se_link)))
  model_data2 <- bind_cols(model_data2, setNames(as_tibble(predict(model, model_data_no_covid, se.fit = TRUE)[1:2]),
                                                 c('fit_link_noCov','se_link_noCov')))
  
  model_data2 <- mutate(model_data2,
                        pred_noCov  = ilink(fit_link_noCov),
                        upr_noCov = ilink(fit_link_noCov + (n * se_link_noCov)),
                        lwr_noCov = ilink(fit_link_noCov - (n * se_link_noCov)))
  
  ####plot 
  model_data$date <- as.Date(model_data$date, format = "%d/%m/%Y")
  model_data2$date <- as.Date(model_data2$date, format = "%d/%m/%Y")
  p <- ggplot(data = model_data,aes(date, rate, color = "Recorded data", lty="Recorded data")) +
    geom_line()+
    geom_point()+
    scale_x_date(date_breaks = "3 month",
                 date_labels = "%Y-%m")+
    labs(title = paste0("Figure 3",fig_vector[which(name_vector==i)],". Region: ",i), 
         x = "", y = "Rate per 100 patients with \nunresectable pancreatic cancer")+
    theme_bw()+
    scale_y_continuous(limits = c(0, 75))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
  start <- "2020-03-01"
  p <- p + geom_vline(xintercept=as.Date(start, format="%Y-%m-%d"), size=0.3, colour="red")
  p <- p +  geom_text(aes(x=as.Date(start, format="%Y-%m-%d")+25, y=10), 
                      color = "red",label="Lockdown", angle = 90, size = 3)
  
  guideli <- "2018-02-01"
  p <- p + geom_vline(xintercept=as.Date(guideli, format="%Y-%m-%d"), size=0.3, colour="black")
  p <- p +  geom_text(aes(x=as.Date(guideli, format="%Y-%m-%d"), y=10), 
                      color = "black",label="National\nguidelines", angle = 90, size = 3)
  
  QS <- "2018-12-01"
  p <- p + geom_vline(xintercept=as.Date(QS, format="%Y-%m-%d"), size=0.3, colour="darkgreen")
  p <- p +  geom_text(aes(x=as.Date(QS, format="%Y-%m-%d"), y=10), 
                      color = "darkgreen",label="Quality\nstandard", angle = 90, size = 3)
  
  p<-p+geom_line(data=model_data2, aes(y=predicted, color = "Model with COVID-19", lty="Model with COVID-19"), size=0.5)
  #p<-p+geom_ribbon(data=model_data2, aes(ymin = lwr, ymax = upr), fill = "grey30", alpha = 0.1)
  p<-p+geom_line(data=model_data2, aes(y=predicted_no_covid, color = "Model", lty="Model"), size=0.5)
  p<-p+geom_ribbon(data=model_data2, aes(ymin = lwr_noCov, ymax = upr_noCov),color = "red",
                   lty=0, fill = "red", alpha = 0.1)
  #p <- p + labs(caption="OpenSafely-TPP October 2023")
  p <- p + theme(plot.caption = element_text(size=8))
  p <- p + theme(plot.title = element_text(size = 10))
  p <- p + theme(legend.position = "none") 
  p <- p + scale_color_manual(name = NULL, values = c("Model" = "red", "Recorded data" = "black", 
                                                      "Model with COVID-19" = "blue"),guide="none")
  p <- p + scale_linetype_manual(name = NULL, values = c("Model" = "solid", "Recorded data" = "solid",
                                                         "Model with COVID-19" = "dashed"),guide="none")
  p <- p + scale_fill_manual(name = NULL, values = c("Model" = "red", "Recorded data" = "white",
                                                     "Model with COVID-19" = "white"),guide="none")
  p <- p + guides(colour = guide_legend(override.aes = list(linetype=c(1,2,1),fill=c("red",NA,NA), 
                                                            shape = c(NA, NA, 16))))
  
  ggsave(
    plot= p, dpi=800,width = 20,height = 8, units = "cm",
    filename = paste0("Figure_3",fig_vector[which(name_vector==i)],"_",i,".png"), path=here::here("output"))
}

########## 
###no GLM 
for (i in name_vector){
  Reg_r <- Region_rounded[which(Region_rounded$region==i),]
  
  model_data <- subset(Reg_r , select=c("rate","date"))

  ####plot 
  model_data$date <- as.Date(model_data$date, format = "%d/%m/%Y")
  
  p <- ggplot(data = model_data,aes(date, rate)) +
    geom_line()+
    geom_point()+
    scale_x_date(date_breaks = "3 month",
                 date_labels = "%Y-%m")+
    labs(title = paste0("Figure 3",fig_vector[which(name_vector==i)],". Region: ",i), 
         x = "", y = "Rate per 100 patients with \nunresectable pancreatic cancer")+
    theme_bw()+
    scale_y_continuous(limits = c(0, 75))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")
  start <- "2020-03-01"
  p <- p + geom_vline(xintercept=as.Date(start, format="%Y-%m-%d"), size=0.3, colour="red")
  p <- p +  geom_text(aes(x=as.Date(start, format="%Y-%m-%d")+25, y=10), 
                      color = "red",label="Lockdown", angle = 90, size = 3)
  
  guideli <- "2018-02-01"
  p <- p + geom_vline(xintercept=as.Date(guideli, format="%Y-%m-%d"), size=0.3, colour="black")
  p <- p +  geom_text(aes(x=as.Date(guideli, format="%Y-%m-%d"), y=10), 
                      color = "black",label="National\nguidelines", angle = 90, size = 3)
  
  QS <- "2018-12-01"
  p <- p + geom_vline(xintercept=as.Date(QS, format="%Y-%m-%d"), size=0.3, colour="darkgreen")
  p <- p +  geom_text(aes(x=as.Date(QS, format="%Y-%m-%d"), y=10), 
                      color = "darkgreen",label="Quality\nstandard", angle = 90, size = 3)
  
  #p <- p + labs(caption="OpenSafely-TPP October 2023")
  p <- p + theme(plot.caption = element_text(size=8))
  p <- p + theme(plot.title = element_text(size = 10))
  p <- p + theme(legend.position = "none") 

  ggsave(
    plot= p, dpi=800,width = 20,height = 8, units = "cm",
    filename = paste0("NoGLM_Figure_3",fig_vector[which(name_vector==i)],"_",i,".png"), path=here::here("output"))
}
