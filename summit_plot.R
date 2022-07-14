#RUN THIS TO GET THE PACKAGES YOU NEED (Only done once)
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggpubr")
# install.packages("gridExtra")
# install.packages("RColorBrewer")

#RUN TO LOAD THE PACKAGES (Every time R restarts)
library("dplyr")
library("ggplot2")
library("ggpubr")
library("gridExtra")
library("RColorBrewer")
theme_set(theme_pubr())

#Set this to your working directory
setwd("C:/Users/jlmad/Downloads/SMDC")

#Data formatting
{
d_h <- read.csv("weekends_holidays.csv")
summitraw<-read.csv("SummitLoginCSV.csv")
summitraw$time_to_create_1G[is.na(summitraw$time_to_create_1G)] = 0

summitraw$date = as.Date(summitraw$date, format = "%b%d_%Y")
d_h$date = as.Date(d_h$date, "%d-%m-%Y")

summitraw$hour = as.integer(summitraw$hour)-1

#Holidays are 1, weekends are 2
d_h$we_ho = as.integer(d_h$we_ho)

summitraw$wo_ho_we = 0

for(i in 1:length(d_h$date)){
  if(d_h$date[i] %in% summitraw$date){
    temp = which(d_h$date[i] == summitraw$date)
    for (j in temp)
    summitraw$wo_ho_we[j] = d_h$we_ho[i]
  }
}

summitraw$datetime = as.POSIXct(paste(summitraw$date, summitraw$hour), format="%Y-%m-%d %H")
}

#Data Reading/Subsetting
{
#WO_HO_WE
{#read in the summitv2 data
      colnames(summitraw)[1] = 'node'
      summitv2 <-summitraw[order(summitraw$datetime),]

      #Selecting for
      # summitv2 = subset(summitv2, datetime < as.POSIXct("2020-03-16", tz = "UTC"))
      # summitv2$covidstatus = as.numeric(summitv2$covidstatus)
        # summitv2$unaliased_ls_time = log10(summitv2$unaliased_ls_time)
        # summitv2$color_ls_time = log10(summitv2$color_ls_time)
        # summitv2$time_to_create_1G = log10(summitv2$time_to_create_1G)

        #Subsetting data into individual login nodes
        summitv2_log1 <-subset(summitv2, node == "login1")
        summitv2_log2 <-subset(summitv2, node == "login2")
        summitv2_log3 <-subset(summitv2, node == "login3")
        summitv2_log4 <-subset(summitv2, node == "login4")
        summitv2_log5 <-subset(summitv2, node == "login5")

        #Ordering the subsets by date time
        summitv2_log1 <-summitv2_log1[order(summitv2_log1$datetime),]
        summitv2_log2 <-summitv2_log2[order(summitv2_log2$datetime),]
        summitv2_log3 <-summitv2_log3[order(summitv2_log3$datetime),]
        summitv2_log4 <-summitv2_log4[order(summitv2_log4$datetime),]
        summitv2_log5 <-summitv2_log5[order(summitv2_log5$datetime),]

        #Subsetting workday, weekend, and holiday
        ln1_work<-subset(summitv2_log1, wo_ho_we == 0)
        ln1_holi<-subset(summitv2_log1, wo_ho_we == 1)
        ln1_wknd<-subset(summitv2_log1, wo_ho_we == 2)

        ln2_work<-subset(summitv2_log2, wo_ho_we == 0)
        ln2_holi<-subset(summitv2_log2, wo_ho_we == 1)
        ln2_wknd<-subset(summitv2_log2, wo_ho_we == 2)

        ln3_work<-subset(summitv2_log3, wo_ho_we == 0)
        ln3_holi<-subset(summitv2_log3, wo_ho_we == 1)
        ln3_wknd<-subset(summitv2_log3, wo_ho_we == 2)

        ln4_work<-subset(summitv2_log4, wo_ho_we == 0)
        ln4_holi<-subset(summitv2_log4, wo_ho_we == 1)
        ln4_wknd<-subset(summitv2_log4, wo_ho_we == 2)

        ln5_work<-subset(summitv2_log5, wo_ho_we == 0)
        ln5_holi<-subset(summitv2_log5, wo_ho_we == 1)
        ln5_wknd<-subset(summitv2_log5, wo_ho_we == 2)

        total_work<-subset(summitv2, wo_ho_we == 0)
        total_holi<-subset(summitv2, wo_ho_we == 1)
        total_wknd<-subset(summitv2, wo_ho_we == 2)

        #Work Hourly Mean
        {
          #Total hourly average
          {
            twm<- total_work %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            twm = as.data.frame(twm)
          }

          #login node 1 daily average
          {
            ln1wm <- ln1_work %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln1wm = as.data.frame(ln1wm)
          }

          #login node 2 daily average
          {
            ln2wm <- ln2_work %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln2wm = as.data.frame(ln2wm)
          }

          #login node 3 daily average
          {
            ln3wm <- ln3_work %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln3wm = as.data.frame(ln3wm)
          }

          #login node 4 daily average
          {
            ln4wm <- ln4_work %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln4wm = as.data.frame(ln4wm)
          }

          #login node 5 daily average
          {ln5wm <- ln5_work %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln5wm = as.data.frame(ln5wm)
          }
        }

        #Weekend Hourly mean
        {
          #Total hourly average
          {
            twkm <- total_wknd %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            twkm = as.data.frame(twkm)
          }

          #login node 1 daily average
          {
            ln1wkm <- ln1_wknd %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln1wkm = as.data.frame(ln1wkm)
          }

          #login node 2 daily average
          {
            ln2wkm <- ln2_wknd %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln2wkm = as.data.frame(ln2wkm)
          }

          #login node 3 daily average
          {
            ln3wkm <- ln3_wknd %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln3wkm = as.data.frame(ln3wkm)
          }

          #login node 4 daily average
          {
            ln4wkm <- ln4_wknd %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln4wkm = as.data.frame(ln4wkm)
          }

          #login node 5 daily average
          {
            ln5wkm <- ln5_wknd %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln5wkm = as.data.frame(ln5wkm)
          }}

        #Holiday Hourly mean
        {
          #Total hourly average
          {
            thm <- total_holi %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            thm = as.data.frame(thm)
          }

          #login node 1 daily average
          {
            ln1hm <- ln1_holi %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln1hm = as.data.frame(ln1hm)
          }

          #login node 2 daily average
          {
            ln2hm <- ln2_holi %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln2hm = as.data.frame(ln2hm)
          }

          #login node 3 daily average
          {
            ln3hm <- ln3_holi %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln3hm = as.data.frame(ln3hm)
          }

          #login node 4 daily average
          {
            ln4hm <- ln4_holi %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln4hm = as.data.frame(ln4hm)
          }

          #login node 5 daily average
          {
            ln5hm <- ln5_holi %>%
              group_by(hour) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
            ln5hm = as.data.frame(ln5hm)
          }
      }
    }
  
#Pre/Post
{#read in the summitv2 data
colnames(summitraw)[1] = 'node'
summitraw <- summitraw %>%
              mutate(covidstatus = ifelse(summitraw$datetime <= as.POSIXct("2020-03-16", tz = "UTC"), 0, 1))

summitv2 <-summitraw[order(summitraw$datetime),]

#Selecting for 
# summitv2 = subset(summitv2, datetime < as.POSIXct("2020-03-16", tz = "UTC"))
# summitv2$covidstatus = as.numeric(summitv2$covidstatus)

for (i in 0:1){
summitv2 <-summitraw[order(summitraw$datetime),]
summitv2 = subset(summitv2, covidstatus == i)

# summitv2$unaliased_ls_time = log10(summitv2$unaliased_ls_time)
# summitv2$color_ls_time = log10(summitv2$color_ls_time)
# summitv2$time_to_create_1G = log10(summitv2$time_to_create_1G)

#Subsetting data into individual login nodes
summitv2_log1 <-subset(summitv2, node == "login1")
summitv2_log2 <-subset(summitv2, node == "login2")
summitv2_log3 <-subset(summitv2, node == "login3")
summitv2_log4 <-subset(summitv2, node == "login4")
summitv2_log5 <-subset(summitv2, node == "login5")

#Ordering the subsets by date time
summitv2_log1 <-summitv2_log1[order(summitv2_log1$datetime),]
summitv2_log2 <-summitv2_log2[order(summitv2_log2$datetime),]
summitv2_log3 <-summitv2_log3[order(summitv2_log3$datetime),]
summitv2_log4 <-summitv2_log4[order(summitv2_log4$datetime),]
summitv2_log5 <-summitv2_log5[order(summitv2_log5$datetime),]

#Subsetting workday, weekend, and holiday
ln1<-summitv2_log1
ln2<-summitv2_log2
ln3<-summitv2_log3
ln4<-summitv2_log4
ln5<-summitv2_log5
total<-summitv2


#Work Hourly Mean
{
#Total hourly average
{
  tm<- total %>%
    group_by(hour) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  tm = as.data.frame(tm)
}

#login node 1 daily average
{
  ln1m <- ln1 %>%
    group_by(hour) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  ln1m = as.data.frame(ln1m)
}

#login node 2 daily average
{
  ln2m <- ln2 %>%
    group_by(hour) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  ln2m = as.data.frame(ln2m)
}

#login node 3 daily average
{
  ln3m <- ln3 %>%
    group_by(hour) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  ln3m = as.data.frame(ln3m)
}

#login node 4 daily average
{
  ln4m <- ln4 %>%
    group_by(hour) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  ln4m = as.data.frame(ln4m)
}

#login node 5 daily average
{ln5m <- ln5 %>%
    group_by(hour) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  ln5m = as.data.frame(ln5m)
}
}


if(i == 0){
#Subsetting workday, weekend, and holiday
ln1mean<-ln1m
ln2mean<-ln2m
ln3mean<-ln3m
ln4mean<-ln4m
ln5mean<-ln5m
totalmean<-tm

ln1mean$covidstatus = 0
ln2mean$covidstatus = 0
ln3mean$covidstatus = 0
ln4mean$covidstatus = 0
ln5mean$covidstatus = 0
totalmean$covidstatus = 0

}
else if(i == 1){
  ln1m$covidstatus = 1
  ln2m$covidstatus = 1
  ln3m$covidstatus = 1
  ln4m$covidstatus = 1
  ln5m$covidstatus = 1
  tm$covidstatus = 1
  
  ln1mean<-rbind(ln1mean, ln1m)
  ln2mean<-rbind(ln2mean, ln2m)
  ln3mean<-rbind(ln3mean, ln3m)
  ln4mean<-rbind(ln4mean, ln4m)
  ln5mean<-rbind(ln5mean, ln5m)
  totalmean<-rbind(totalmean, twm)
}
}
}

#Pre/Post and WO_HO_WE
{#read in the summitv2 data
  #   colnames(summitraw)[1] = 'node'
  #   summitraw <- summitraw %>%
  #     mutate(covidstatus = ifelse(summitraw$datetime <= as.POSIXct("2020-03-16", tz = "UTC"), 0, 1))
  #   
  #   summitv2 <-summitraw[order(summitraw$datetime),]
  #   
  #   #Selecting for 
  #   # summitv2 = subset(summitv2, datetime < as.POSIXct("2020-03-16", tz = "UTC"))
  #   # summitv2$covidstatus = as.numeric(summitv2$covidstatus)
  #   
  #   for (i in 0:1){
  #     summitv2 <-summitraw[order(summitraw$datetime),]
  #     summitv2 = subset(summitv2, covidstatus == i)
  #     
  #     # summitv2$unaliased_ls_time = log10(summitv2$unaliased_ls_time)
  #     # summitv2$color_ls_time = log10(summitv2$color_ls_time)
  #     # summitv2$time_to_create_1G = log10(summitv2$time_to_create_1G)
  #     
  #     #Subsetting data into individual login nodes
  #     summitv2_log1 <-subset(summitv2, node == "login1")
  #     summitv2_log2 <-subset(summitv2, node == "login2")
  #     summitv2_log3 <-subset(summitv2, node == "login3")
  #     summitv2_log4 <-subset(summitv2, node == "login4")
  #     summitv2_log5 <-subset(summitv2, node == "login5")
  #     
  #     #Ordering the subsets by date time
  #     summitv2_log1 <-summitv2_log1[order(summitv2_log1$datetime),]
  #     summitv2_log2 <-summitv2_log2[order(summitv2_log2$datetime),]
  #     summitv2_log3 <-summitv2_log3[order(summitv2_log3$datetime),]
  #     summitv2_log4 <-summitv2_log4[order(summitv2_log4$datetime),]
  #     summitv2_log5 <-summitv2_log5[order(summitv2_log5$datetime),]
  #     
  #     #Subsetting workday, weekend, and holiday
  #     ln1_work<-subset(summitv2_log1, wo_ho_we == 0)
  #     ln1_holi<-subset(summitv2_log1, wo_ho_we == 1)
  #     ln1_wknd<-subset(summitv2_log1, wo_ho_we == 2)
  #     
  #     ln2_work<-subset(summitv2_log2, wo_ho_we == 0)
  #     ln2_holi<-subset(summitv2_log2, wo_ho_we == 1)
  #     ln2_wknd<-subset(summitv2_log2, wo_ho_we == 2)
  #     
  #     ln3_work<-subset(summitv2_log3, wo_ho_we == 0)
  #     ln3_holi<-subset(summitv2_log3, wo_ho_we == 1)
  #     ln3_wknd<-subset(summitv2_log3, wo_ho_we == 2)
  #     
  #     ln4_work<-subset(summitv2_log4, wo_ho_we == 0)
  #     ln4_holi<-subset(summitv2_log4, wo_ho_we == 1)
  #     ln4_wknd<-subset(summitv2_log4, wo_ho_we == 2)
  #     
  #     ln5_work<-subset(summitv2_log5, wo_ho_we == 0)
  #     ln5_holi<-subset(summitv2_log5, wo_ho_we == 1)
  #     ln5_wknd<-subset(summitv2_log5, wo_ho_we == 2)
  #     
  #     total_work<-subset(summitv2, wo_ho_we == 0)
  #     total_holi<-subset(summitv2, wo_ho_we == 1)
  #     total_wknd<-subset(summitv2, wo_ho_we == 2)
  #     
  #     #Work Hourly Mean
  #     {
  #       #Total hourly average
  #       {
  #         twm<- total_work %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         twm = as.data.frame(twm)
  #       }
  #       
  #       #login node 1 daily average
  #       {
  #         ln1wm <- ln1_work %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln1wm = as.data.frame(ln1wm)
  #       }
  #       
  #       #login node 2 daily average
  #       {
  #         ln2wm <- ln2_work %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln2wm = as.data.frame(ln2wm)
  #       }
  #       
  #       #login node 3 daily average
  #       {
  #         ln3wm <- ln3_work %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln3wm = as.data.frame(ln3wm)
  #       }
  #       
  #       #login node 4 daily average
  #       {
  #         ln4wm <- ln4_work %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln4wm = as.data.frame(ln4wm)
  #       }
  #       
  #       #login node 5 daily average
  #       {ln5wm <- ln5_work %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln5wm = as.data.frame(ln5wm)
  #       }
  #     }
  #     
  #     #Weekend Hourly mean
  #     {
  #       #Total hourly average
  #       {
  #         twkm <- total_wknd %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         twkm = as.data.frame(twkm)
  #       }
  #       
  #       #login node 1 daily average
  #       {
  #         ln1wkm <- ln1_wknd %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln1wkm = as.data.frame(ln1wkm)
  #       }
  #       
  #       #login node 2 daily average
  #       {
  #         ln2wkm <- ln2_wknd %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln2wkm = as.data.frame(ln2wkm)
  #       }
  #       
  #       #login node 3 daily average
  #       {
  #         ln3wkm <- ln3_wknd %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln3wkm = as.data.frame(ln3wkm)
  #       }
  #       
  #       #login node 4 daily average
  #       {
  #         ln4wkm <- ln4_wknd %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln4wkm = as.data.frame(ln4wkm)
  #       }
  #       
  #       #login node 5 daily average
  #       {
  #         ln5wkm <- ln5_wknd %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln5wkm = as.data.frame(ln5wkm)
  #       }}
  #     
  #     #Holiday Hourly mean
  #     {
  #       #Total hourly average
  #       {
  #         thm <- total_holi %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         thm = as.data.frame(thm)
  #       }
  #       
  #       #login node 1 daily average
  #       {
  #         ln1hm <- ln1_holi %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln1hm = as.data.frame(ln1hm)
  #       }
  #       
  #       #login node 2 daily average
  #       {
  #         ln2hm <- ln2_holi %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln2hm = as.data.frame(ln2hm)
  #       }
  #       
  #       #login node 3 daily average
  #       {
  #         ln3hm <- ln3_holi %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln3hm = as.data.frame(ln3hm)
  #       }
  #       
  #       #login node 4 daily average
  #       {
  #         ln4hm <- ln4_holi %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln4hm = as.data.frame(ln4hm)
  #       }
  #       
  #       #login node 5 daily average
  #       {
  #         ln5hm <- ln5_holi %>%
  #           group_by(hour) %>%
  #           summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
  #                             mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
  #                             total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
  #         ln5hm = as.data.frame(ln5hm)
  #       }}
  #     
  #     if(i == 0){
  #       #Subsetting workday, weekend, and holiday
  #       ln1workmean<-ln1wm
  #       ln1holimean<-ln1hm
  #       ln1wkndmean<-ln1wkm
  #       
  #       ln2workmean<-ln2wm
  #       ln2holimean<-ln2hm
  #       ln2wkndmean<-ln2wkm
  #       
  #       ln3workmean<-ln3wm
  #       ln3holimean<-ln3hm
  #       ln3wkndmean<-ln3wkm
  #       
  #       ln4workmean<-ln4wm
  #       ln4holimean<-ln4hm
  #       ln4wkndmean<-ln4wkm
  #       
  #       ln5workmean<-ln5wm
  #       ln5holimean<-ln5hm
  #       ln5wkndmean<-ln5wkm
  #       
  #       total_work_mean<-twm
  #       total_holi_mean<-thm
  #       total_wknd_mean<-twkm
  #       
  #       ln1workmean$covidstatus = 0
  #       ln1holimean$covidstatus = 0
  #       ln1wkndmean$covidstatus = 0
  #       
  #       ln2workmean$covidstatus = 0
  #       ln2holimean$covidstatus = 0
  #       ln2wkndmean$covidstatus = 0
  #       
  #       ln3workmean$covidstatus = 0
  #       ln3holimean$covidstatus = 0
  #       ln3wkndmean$covidstatus = 0
  #       
  #       ln4workmean$covidstatus = 0
  #       ln4holimean$covidstatus = 0
  #       ln4wkndmean$covidstatus = 0
  #       
  #       ln5workmean$covidstatus = 0
  #       ln5holimean$covidstatus = 0
  #       ln5wkndmean$covidstatus = 0
  #       
  #       total_work_mean$covidstatus = 0
  #       total_holi_mean$covidstatus = 0
  #       total_wknd_mean$covidstatus = 0
  #     }
  #     else if(i == 1){
  #       ln1wm$covidstatus = 1
  #       ln1hm$covidstatus = 1
  #       ln1wkm$covidstatus = 1
  #       
  #       ln2wm$covidstatus = 1
  #       ln2hm$covidstatus = 1
  #       ln2wkm$covidstatus = 1
  #       
  #       ln3wm$covidstatus = 1
  #       ln3hm$covidstatus = 1
  #       ln3wkm$covidstatus = 1
  #       
  #       ln4wm$covidstatus = 1
  #       ln4hm$covidstatus = 1
  #       ln4wkm$covidstatus = 1
  #       
  #       ln5wm$covidstatus = 1
  #       ln5hm$covidstatus = 1
  #       ln5wkm$covidstatus = 1
  #       
  #       twm$covidstatus = 1
  #       thm$covidstatus = 1
  #       twkm$covidstatus = 1
  #       
  #       ln1workmean<-rbind(ln1workmean, ln1wm)
  #       ln1holimean<-rbind(ln1holimean, ln1hm)
  #       ln1wkndmean<-rbind(ln1wkndmean, ln1wkm)
  #       
  #       ln2workmean<-rbind(ln2workmean, ln2wm)
  #       ln2holimean<-rbind(ln2holimean, ln2hm)
  #       ln2wkndmean<-rbind(ln2wkndmean, ln2wkm)
  #       
  #       ln3workmean<-rbind(ln3workmean, ln3wm)
  #       ln3holimean<-rbind(ln3holimean, ln3hm)
  #       ln3wkndmean<-rbind(ln3wkndmean, ln3wkm)
  #       
  #       ln4workmean<-rbind(ln4workmean, ln4wm)
  #       ln4holimean<-rbind(ln4holimean, ln4hm)
  #       ln4wkndmean<-rbind(ln4wkndmean, ln4wkm)
  #       
  #       ln5workmean<-rbind(ln5workmean, ln5wm)
  #       ln5holimean<-rbind(ln5holimean, ln5hm)
  #       ln5wkndmean<-rbind(ln5wkndmean, ln5wkm)
  #       
  #       total_work_mean<-rbind(total_work_mean, twm)
  #       total_holi_mean<-rbind(total_holi_mean, thm)
  #       total_wknd_mean<-rbind(total_wknd_mean, twkm)}
  #   }
  # }

}
}


#Calculating the means by day
{
#Total daily average
{
  
  templog1 = summitv2
  templog1$unaliased_ls_time = log10(templog1$unaliased_ls_time)
  templog1$color_ls_time = log10(templog1$color_ls_time)
  templog1$time_to_create_1G = log10(templog1$time_to_create_1G)
  
  summitv2 = templog1
  
total_mean <- summitv2 %>%
    group_by(date) %>%
    summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                      mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                      total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
total_mean$date = as.POSIXlt(total_mean$date, format = "%Y-%m-%d")
total_mean = as.data.frame(total_mean)


# total_sum <- summitv2 %>%
#   group_by(date, hour) %>%
#   summarise_at(vars(users, running_procs, mem_used, running_jobs, total_jobs), list(sum))
# total_sum$date = as.POSIXlt(total_sum$date, format = "%Y-%m-%d")
# total_sum = as.data.frame(total_sum)
# total_sum = 
}

#login node 1 daily average
{
templog1 = summitv2_log1
templog1$unaliased_ls_time = log10(templog1$unaliased_ls_time)
templog1$color_ls_time = log10(templog1$color_ls_time)
templog1$time_to_create_1G = log10(templog1$time_to_create_1G)

summitv2_log1 = templog1
  
log1mean <- summitv2_log1 %>%
              group_by(date) %>%
              summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                                mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                                total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
log1mean$date = as.POSIXlt(log1mean$date)
log1mean = as.data.frame(log1mean)
}
  
#login node 2 daily average
{
  templog1 = summitv2_log2
  templog1$unaliased_ls_time = log10(templog1$unaliased_ls_time)
  templog1$color_ls_time = log10(templog1$color_ls_time)
  templog1$time_to_create_1G = log10(templog1$time_to_create_1G)
  
  summitv2_log2 = templog1
  
log2mean <- summitv2_log2 %>%
  group_by(date) %>%
  summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                    mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                    total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
log2mean$date = as.POSIXlt(log2mean$date)
log2mean = as.data.frame(log2mean)
}
  
#login node 3 daily average
{
  templog1 = summitv2_log3
  templog1$unaliased_ls_time = log10(templog1$unaliased_ls_time)
  templog1$color_ls_time = log10(templog1$color_ls_time)
  templog1$time_to_create_1G = log10(templog1$time_to_create_1G)
  
  summitv2_log3 = templog1
  
log3mean <- summitv2_log3 %>%
  group_by(date) %>%
  summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                    mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                    total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
log3mean$date = as.POSIXlt(log3mean$date)
log3mean = as.data.frame(log3mean)
}

#login node 4 daily average
{
  templog1 = summitv2_log4
  templog1$unaliased_ls_time = log10(templog1$unaliased_ls_time)
  templog1$color_ls_time = log10(templog1$color_ls_time)
  templog1$time_to_create_1G = log10(templog1$time_to_create_1G)
  
  summitv2_log4 = templog1
  
log4mean <- summitv2_log4 %>%
  group_by(date) %>%
  summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                    mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                    total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
log4mean$date = as.POSIXlt(log4mean$date)
log4mean = as.data.frame(log4mean)
}

#login node 5 daily average
{
  templog1 = summitv2_log5
  templog1$unaliased_ls_time = log10(templog1$unaliased_ls_time)
  templog1$color_ls_time = log10(templog1$color_ls_time)
  templog1$time_to_create_1G = log10(templog1$time_to_create_1G)
  
  summitv2_log5 = templog1
  
  log5mean <- summitv2_log5 %>%
  group_by(date) %>%
  summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                    mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                    total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
log5mean$date = as.POSIXlt(log5mean$date)
log5mean = as.data.frame(log5mean)
}
}

#Calculating the means by hour
{
  #Total hourly average
  {
    total_hmean <- summitv2 %>%
      group_by(hour) %>%
      summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                        mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                        total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
    total_hmean = as.data.frame(total_hmean)
  }
  
  #login node 1 daily average
  {
    log1hmean <- summitv2_log1 %>%
      group_by(hour) %>%
      summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                        mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                        total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
    log1hmean = as.data.frame(log1hmean)
  }
  
  #login node 2 daily average
  {
    log2hmean <- summitv2_log2 %>%
      group_by(hour) %>%
      summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                        mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                        total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
    log2hmean = as.data.frame(log2hmean)
  }
  
  #login node 3 daily average
  {
    log3hmean <- summitv2_log3 %>%
      group_by(hour) %>%
      summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                        mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                        total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
    log3hmean = as.data.frame(log3hmean)
  }
  
  #login node 4 daily average
  {
    log4hmean <- summitv2_log4 %>%
      group_by(hour) %>%
      summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                        mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                        total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
    log4hmean = as.data.frame(log4hmean)
  }
  
  #login node 5 daily average
  {log5hmean <- summitv2_log5 %>%
      group_by(hour) %>%
      summarise_at(vars(users, running_procs, cpu_load_1_min, cpu_load_5_min, cpu_load_15_min,
                        mem_total, mem_avail, mem_used, unaliased_ls_time, color_ls_time, running_jobs,
                        total_jobs, time_to_create_1G, disk_util, wo_ho_we), list(mean))
    log5hmean = as.data.frame(log5hmean)
  }
}

#LINE CHART
{
#Summitraw
#list <- c(4:8, 10:17)

#mean
list <- c(2:6, 9:15)
title_list <- c("Date", "Average Daily Users", "Average Daily Running Processes", "Average Daily CPU Load (1 min)",
                "Average Daily CPU Load (5 min)", "Average Daily CPU Load (15 min)", "Memory Total", "Memory Available",
                "Average Daily Memory Used", "Average Daily Unaliased ls Time", "Average Color ls Time", "Average Daily Running Jobs",
                "Average Daily Total Jobs", "Average Daily Time to Create 1G", "Average Daily Percent Disk Utilization")
y_cap_list <- c("Date", "Users", "Running Processes", "CPU Load (1 min)",
                "CPU Load (5 min)", "CPU Load (15 min)", "Memory Total", "Memory Available",
                "Memory Used (kb)", "Unaliased ls Time (sec)", "Color ls Time (sec)", "Running Jobs",
                "Total Jobs", "Time to Create 1G (sec)", "Percent Disk Utilization")

pdfcomplist <- c("date", "user_comp.pdf", "proc_comp.pdf", "load1_comp.pdf", "load5_comp.pdf", "load15_comp.pdf",
                 "memtotal", "memavailable", "memused_comp.pdf", "uals_comp.pdf", "colorls_comp.pdf", "runjob_comp.pdf",
                 "totjob_comp.pdf", "create1G_comp.pdf", "percdu_comp.pdf")
pdfindlist <- c("date", "user_ind.pdf", "proc_ind.pdf", "load1_ind.pdf", "load5_ind.pdf", "load15_ind.pdf",
                "memtotal", "memavailable", "memused_ind.pdf", "uals_ind.pdf", "colorls_ind.pdf", "runjob_ind.pdf",
                "totjob_ind.pdf", "create1G_ind.pdf", "percdu_ind.pdf")

pdfwidth<-19
pdfheight<-15
pdf("nodes_compiled.pdf", width = pdfwidth, height = pdfheight)
for (i in list){
print(i)
###############################################################################################
###############################################################################################
#SELECT HERE
#You can grab columns in R by using the $ in the following way: dataframe$Columnname
#Change the header after the $ to choose what you want to plot
#Use summitv2_log# to use all hours (more intensive and harder to read)
#y axis selections 
y_axis1 <- log1mean[,i]
y_axis2 <- log2mean[,i]
y_axis3 <- log3mean[,i]
y_axis4 <- log4mean[,i]
y_axis5 <- log5mean[,i]
y_axis6 <- total_mean[,i]


#X axis selections (Will probably always be date)
#Use summitv2_log# to use all hours (more intensive and harder to read)
x_axis1 <- log1mean$date
x_axis2 <- log2mean$date
x_axis3 <- log3mean$date
x_axis4 <- log4mean$date
x_axis5 <- log5mean$date
x_axis6 <- total_mean$date

#Captioning and Title
x_caption <- "Date-Time"
y_caption <- y_cap_list[i]
plot_title<- title_list[i]

#Y-axis dimensions and PDF dimensions
y_dim <- c(0, max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i])) + max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i]))*0.1)

if(i == 10 || i == 11) y_dim <- c(min(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i])) + min(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i]))*0.1, max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i])) + max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i]))*0.1)


#Adding vertical lines for certain events
#Change the dates to place the lines
# abline(v = as.POSIXct("2020-01-01 UTC"), lwd = 2)
# abline(v = as.POSIXct("2020-01-06 UTC"), lwd = 2)

#All 5 login nodes in same figure
pdfname_compiled <- pdfcomplist[i]
#6 figures with one for each login node
pdfname_individual <- pdfindlist[i]

#TRUE for lines to be on the graphs
#FALSE for only the points to be included
line <- TRUE

##############################################################################################
##############################################################################################

#Plotting
#Produces two pdfs in the current working directory
#compiled plot of all 5 login nodes
{
par(mfrow=c(1,1))
plot(x_axis1, y_axis1, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="#A1D99B", cex.lab = 1.3, cex.axis = 1.1, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3, mgp = c(1,1.5,0), at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                            as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
                                            labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                                                       "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if(line == TRUE){
lines(x_axis1, y_axis1, col = "#A1D99B",  lwd=1)
points(x_axis2, y_axis2, col = "#74C476", pch=20, lwd=1)
lines(x_axis2, y_axis2, col = "#74C476", lwd=1)
points(x_axis3, y_axis3, col = "#41AB5D", pch=20, lwd=1)
lines(x_axis3, y_axis3, col = "#41AB5D", lwd=1)
points(x_axis4, y_axis4, col = "#238B45", pch=20, lwd=1)
lines(x_axis4, y_axis4, col = "#238B45", lwd=1)
points(x_axis5, y_axis5, col = "#005A32", pch=20, lwd=1)
lines(x_axis5, y_axis5, col = "#005A32", lwd=1)
}
legend("topright", title = paste(plot_title), legend = c("Login Node 1", "Login Node 2", "Login Node 3", "Login Node 4", "Login Node 5"),
       col = c("#A1D99B", "#74C476", "#41AB5D", "#238B45","#005A32"), bty = "b",pch = c(20, 20, 20, 20, 20, NA, NA),
       lty = c(1,1,1,1, 1, 1), pt.cex = 1.2, cex = 1.2, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)
}
}
dev.off()

pdf("nodes_ind.pdf", width = pdfwidth, height = pdfheight)
for(i in list){
  #SELECT HERE
  #You can grab columns in R by using the $ in the following way: dataframe$Columnname
  #Change the header after the $ to choose what you want to plot
  #Use summitv2_log# to use all hours (more intensive and harder to read)
  #y axis selections 
    y_axis1 <- log1mean[,i]
    y_axis2 <- log2mean[,i]
    y_axis3 <- log3mean[,i]
    y_axis4 <- log4mean[,i]
    y_axis5 <- log5mean[,i]
    y_axis6 <- total_mean[,i]
    
  #Captioning and Title
  x_caption <- "Date-Time"
  y_caption <- y_cap_list[i]
  plot_title<- title_list[i]
  
  #Y-axis dimensions and PDF dimensions
  y_dim <- c(0, max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i])) + max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i]))*0.1)
  if(i == 10 || i == 11) y_dim <- c(min(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i])) + min(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i]))*0.1, max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i])) + max(na.omit(log1mean[,i]), na.omit(log2mean[,i]), na.omit(log3mean[,i]), na.omit(log4mean[,i]), na.omit(log5mean[,i]), na.omit(total_mean[,i]))*0.1)
  
  
  
#5 individual plots of login nodes
{
par(mfrow=c(3,2))
plot(x_axis1, y_axis1, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="#A1D99B", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3,  mgp = c(1,1.5,0), at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                                   as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
     labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if (line == TRUE) lines(x_axis1, y_axis1, col = "#A1D99B", lwd=2)
legend("topright", title = paste(plot_title), legend = c("Login Node 1"),
       col = c("#A1D99B"), bty = "b",pch = c(20, NA, NA, NA, NA, NA, NA),
       lty = c(1,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)

plot(x_axis2, y_axis2, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="#74C476", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3,  mgp = c(1,1.5,0), at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                                   as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
     labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if (line == TRUE) lines(x_axis2, y_axis2, col = "#74C476", lwd=2)
legend("topright", title = paste(plot_title), legend = c("Login Node 2"),
       col = c("#74C476"), bty = "b",pch = c(20, NA, NA, NA, NA, NA, NA),
       lty = c(1,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)

plot(x_axis3, y_axis3, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="#41AB5D", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3,  mgp = c(1,1.5,0), at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                                   as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
     labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if (line == TRUE) lines(x_axis3, y_axis3, col = "#41AB5D", lwd=2)
legend("topright", title = paste(plot_title), legend = c("Login Node 3"),
       col = c("#41AB5D"), bty = "b",pch = c(20, NA, NA, NA, NA, NA, NA),
       lty = c(1,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)

plot(x_axis4, y_axis4, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="#238B45", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3,  mgp = c(1,1.5,0), at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                                   as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
     labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if (line == TRUE) lines(x_axis4, y_axis4, col = "#238B45", lwd=2)
legend("topright", title = paste(plot_title), legend = c("Login Node 4"),
       col = c("#238B45"), bty = "b",pch = c(20, NA, NA, NA, NA, NA, NA),
       lty = c(1,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)

plot(x_axis5, y_axis5, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="#005A32", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3,  mgp = c(1,1.5,0),  at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                                   as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
     labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if (line == TRUE) lines(x_axis5, y_axis5, col = "#005A32", lwd=2)
legend("topright", title = paste(plot_title), legend = c("Login Node 5"),
       col = c("#005A32"), bty = "b",pch = c(20, NA, NA, NA, NA, NA, NA),
       lty = c(1,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)

plot(x_axis6, y_axis6, xlab=x_caption, ylab=y_caption, ylim=y_dim, 
     pch=20,col="red", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, xaxt = 'n', yaxt = ifelse(i == 10 || i == 11 || i == 14,'n', 's'))
axis(1, las = 1, cex.axis = 0.9, cex = 1.3,  mgp = c(1,1.5,0), at = c(as.POSIXct("2020-01-01 UTC"), as.POSIXct("2020-02-01 UTC"), as.POSIXct("2020-03-01 UTC"), as.POSIXct("2020-04-01 UTC"), as.POSIXct("2020-05-01 UTC"), as.POSIXct("2020-06-01 UTC"), as.POSIXct("2020-07-01 UTC"), as.POSIXct("2020-08-01 UTC"), as.POSIXct("2020-09-01 UTC"), as.POSIXct("2020-10-01 UTC"), as.POSIXct("2020-11-01 UTC"), as.POSIXct("2020-12-01 UTC"),
                                                   as.POSIXct("2021-01-01 UTC"), as.POSIXct("2021-02-01 UTC"), as.POSIXct("2021-03-01 UTC"), as.POSIXct("2021-04-01 UTC"), as.POSIXct("2021-05-01 UTC"), as.POSIXct("2021-06-01 UTC"), as.POSIXct("2021-07-01 UTC"), as.POSIXct("2021-08-01 UTC"), as.POSIXct("2021-09-01 UTC"), as.POSIXct("2021-10-01 UTC"), as.POSIXct("2021-11-01 UTC"), as.POSIXct("2021-12-01 UTC")),
     labels = c("Jan\n2020", "Feb\n2020", "Mar\n2020", "Apr\n2020", "May\n2020", "Jun\n2020", "Jul\n2020", "Aug\n2020", "Sep\n2020", "Oct\n2020", "Nov\n2020", "Dec\n2020",
                "Jan\n2021", "Feb\n2021", "Mar\n2021", "Apr\n2021", "May\n2021", "Jun\n2021", "Jul\n2021", "Aug\n2021", "Sep\n2021", "Oct\n2021", "Nov\n2021", "Dec\n2021"))
if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
if (line == TRUE) lines(x_axis6, y_axis6, col = "red", lwd=2)
legend("topright", title = paste(plot_title), legend = c("Login Node Average"),
       col = c("red"), bty = "b",pch = c(20, NA, NA, NA, NA, NA, NA),
       lty = c(1,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
       text.col = c("black"),horiz = F )
abline(h = 0, lwd = 2)
}

}
}
dev.off()

#Hour BAR CHART
{
# {
# list1 <- c(2:6, 9:15)
# day <- "Post-COVID Weekend "
# title1_list <- c("Date", paste(day, "Average Hourly Users"), paste(day, "Average Hourly Running Processes"), paste(day, "Average Hourly CPU Load (1 min)"),
#                 paste(day, "Average Hourly CPU Load (5 min)"), paste(day, "Average Hourly CPU Load (15 min)"), "Memory Total", "Memory Available",
#                 paste(day, "Average Hourly Memory Used"), paste(day, "Average Hourly Unaliased ls Time"), paste(day, "Average Color ls Time"), paste(day, "Average Hourly Running Jobs"),
#                 paste(day, "Average Hourly Total Jobs"), paste(day, "Average Hourly Time to Create 1G"), paste(day, "Average Hourly Percent Disk Utilization"))
# y_cap1_list <- c("Date", "Users", "Running Processes", "CPU Load (1 min)",
#                 "CPU Load (5 min)", "CPU Load (15 min)", "Memory Total", "Memory Available",
#                 "Memory Used (kb)", "Unaliased ls Time (sec)", "Color ls Time (sec)", "Running Jobs",
#                 "Total Jobs", "Time to Create 1G (sec)", "Percent Disk Utilization")
# 
# pdfind1list <- c("date", "user_bar.pdf", "proc_bar.pdf", "load1_bar.pdf", "load5_bar.pdf", "load15_bar.pdf",
#                 "memtotal", "memavailable", "memused_bar.pdf", "uals_bar.pdf", "colorls_bar.pdf", "runjob_bar.pdf",
#                 "totjob_bar.pdf", "create1G_bar.pdf", "percdu_bar.pdf")
# 
# pdf("barplot_compiled_weekend_POSTCOVID.pdf", width = pdfwidth, height = pdfheight)
# par(mfrow=c(3,2))
# for(i in list1){
# print(i)
# #Use summitv2_log# to use all hours (more intensive and harder to read)
# #y axis selections
# y_axis1 <- ln1wkndmean[,i]
# y_axis2 <- ln2wkndmean[,i]
# y_axis3 <- ln3wkndmean[,i]
# y_axis4 <- ln4wkndmean[,i]
# y_axis5 <- ln5wkndmean[,i]
# y_axis6 <- total_wknd_mean[,i]
# 
# #X axis selections (Will probably always be date)
# #Use summitv2_log# to use all hours (more intensive and harder to read)
# x_axis1 <- ln1wkndmean$hour
# x_axis2 <- ln2wkndmean$hour
# x_axis3 <- ln3wkndmean$hour
# x_axis4 <- ln4wkndmean$hour
# x_axis5 <- ln5wkndmean$hour
# x_axis6 <- total_wknd_mean$hour
# 
# #Captioning and Title
# x_caption <- "Date-Time"
# y_caption <- y_cap1_list[i]
# plot_title<- title1_list[i]
# 
# #Y-axis dimensions and PDF dimensions
# y_dim <- c(0, max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6)) + max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6))*0.2)
# pdfwidth<-16
# pdfheight<-12
# 
# pdfname_individual <- pdfind1list[i]
# 
# #5 individual plots of login nodes
# {
# 
#   barplot(y_axis1, names = x_axis1, col = "#A1D99B", xlab = "Hour of the Day", ylab = y_cap1_list[i],
#           space = 0, las=1, ylim = y_dim)
#   legend("topright", title = paste(plot_title), legend = c("Login Node 1"),
#          col = c("#A1D99B"), bty = "b",pch = c(15, NA, NA, NA, NA, NA, NA),
#          lty = c(NA,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
#          text.col = c("black"),horiz = F )
#   abline(h = 0)
# 
#   barplot(y_axis2, names = x_axis2, col = "#74C476", xlab = "Hour of the Day", ylab = y_cap1_list[i],
#           space = 0, las=1, ylim = y_dim)
#   legend("topright", title = paste(plot_title), legend = c("Login Node 2"),
#          col = c("#74C476"), bty = "b",pch = c(15, NA, NA, NA, NA, NA, NA),
#          lty = c(NA,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
#          text.col = c("black"),horiz = F )
#   abline(h = 0)
# 
#   barplot(y_axis3, names = x_axis3, col = "#41AB5D", xlab = "Hour of the Day", ylab = y_cap1_list[i],
#           space = 0, las=1, ylim = y_dim)
#   legend("topright", title = paste(plot_title), legend = c("Login Node 3"),
#          col = c("#41AB5D"), bty = "b",pch = c(15, NA, NA, NA, NA, NA, NA),
#          lty = c(NA,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
#          text.col = c("black"),horiz = F )
#   abline(h = 0)
# 
#   barplot(y_axis4, names = x_axis4, col = "#238B45", xlab = "Hour of the Day", ylab = y_cap1_list[i],
#           space = 0, las=1, ylim = y_dim)
#   legend("topright", title = paste(plot_title), legend = c("Login Node 4"),
#          col = c("#41AB5D"), bty = "b",pch = c(15, NA, NA, NA, NA, NA, NA),
#          lty = c(NA,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
#          text.col = c("black"),horiz = F )
#   abline(h = 0)
# 
#   barplot(y_axis5, names = x_axis5, col = "#005A32", xlab = "Hour of the Day", ylab = y_cap1_list[i],
#           space = 0, las=1, ylim = y_dim)
#   legend("topright", title = paste(plot_title), legend = c("Login Node 5"),
#          col = c("#005A32"), bty = "b",pch = c(15, NA, NA, NA, NA, NA, NA),
#          lty = c(NA,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
#          text.col = c("black"),horiz = F )
#   abline(h = 0)
# 
#   barplot(y_axis6, names = x_axis6, col = "red", xlab = "Hour of the Day", ylab = y_cap1_list[i],
#           space = 0, las=1, ylim = y_dim)
#   legend("topright", title = paste(plot_title), legend = c("All Nodes Total"),
#          col = c("red"), bty = "b",pch = c(15, NA, NA, NA, NA, NA, NA),
#          lty = c(NA,NA, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2),
#          text.col = c("black"),horiz = F )
#   abline(h = 0)
# }
# }
# 
# dev.off()
# }
}

#Covid Plots with wo_ho_we division
{
list1 <- c(2:6, 9:15)
day <- "Workday "
title1_list <- c("Date", paste(day, "Average Hourly Users"), paste(day, "Average Hourly Running Processes"), paste(day, "Average Hourly CPU Load (1 min)"),
                 paste(day, "Average Hourly CPU Load (5 min)"), paste(day, "Average Hourly CPU Load (15 min)"), "Memory Total", "Memory Available",
                 paste(day, "Average Hourly Memory Used"), paste(day, "Average Hourly Unaliased ls Time"), paste(day, "Average Color ls Time"), paste(day, "Average Hourly Running Jobs"),
                 paste(day, "Average Hourly Total Jobs"), paste(day, "Average Hourly Time to Create 1G"), paste(day, "Average Hourly Percent Disk Utilization"))
y_cap1_list <- c("Date", "Users", "Running Processes", "CPU Load (1 min)",
                 "CPU Load (5 min)", "CPU Load (15 min)", "Memory Total", "Memory Available",
                 "Memory Used (kb)", "Unaliased ls Time (sec)", "Color ls Time (sec)", "Running Jobs",
                 "Total Jobs", "Time to Create 1G (sec)", "Percent Disk Utilization")

pdfind1list <- c("date", "user_bar.pdf", "proc_bar.pdf", "load1_bar.pdf", "load5_bar.pdf", "load15_bar.pdf",
                 "memtotal", "memavailable", "memused_bar.pdf", "uals_bar.pdf", "colorls_bar.pdf", "runjob_bar.pdf",
                 "totjob_bar.pdf", "create1G_bar.pdf", "percdu_bar.pdf")

pdf("compiled_workday_COVID.pdf", width = pdfwidth, height = pdfheight)
par(mfrow=c(3,2))
for(i in list1){
  print(i)

  temp1 <- subset(ln1workmean, covidstatus == 0)
  temp2 <- subset(ln2workmean, covidstatus == 0)
  temp3 <- subset(ln3workmean, covidstatus == 0)
  temp4 <- subset(ln4workmean, covidstatus == 0)
  temp5 <- subset(ln5workmean, covidstatus == 0)
  temp6 <- subset(total_work_mean, covidstatus == 0)
  temp7 <- subset(ln1workmean, covidstatus == 1)
  temp8 <- subset(ln2workmean, covidstatus == 1)
  temp9 <- subset(ln3workmean, covidstatus == 1)
  temp10 <- subset(ln4workmean, covidstatus == 1)
  temp11 <- subset(ln5workmean, covidstatus == 1)
  temp12 <- subset(total_work_mean, covidstatus == 1)

  #Use summitv2_log# to use all hours (more intensive and harder to read)
  #y axis selections
  y_axis1 <- temp1[,i]
  y_axis2 <- temp2[,i]
  y_axis3 <- temp3[,i]
  y_axis4 <- temp4[,i]
  y_axis5 <- temp5[,i]
  y_axis6 <- temp6[,i]

  liney1 = temp7[,i]
  liney2 = temp8[,i]
  liney3 = temp9[,i]
  liney4 = temp10[,i]
  liney5 = temp11[,i]
  liney6 = temp12[,i]

  #X axis selections (Will probably always be date)
  #Use summitv2_log# to use all hours (more intensive and harder to read)
  x_axis1 <- temp1$hour
  x_axis2 <- temp2$hour
  x_axis3 <- temp3$hour
  x_axis4 <- temp4$hour
  x_axis5 <- temp5$hour
  x_axis6 <- temp6$hour

  linex1 = temp7$hour
  linex2 = temp8$hour
  linex3 = temp9$hour
  linex4 = temp10$hour
  linex5 = temp11$hour
  linex6 = temp12$hour

  #Captioning and Title
  x_caption <- "Hour"
  y_caption <- y_cap1_list[i]
  plot_title<- title1_list[i]

  #Y-axis dimensions and PDF dimensions
  y_dim <- c(0, max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))
             + max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.4)
  #if(i == 10 || i == 11) y_dim <- c(min(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6)) +
  #                                   min(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.1,
  #                                 max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6)) +
  #                                   max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.1)

  pdfwidth<-16
  pdfheight<-12


  {
    par(mfrow=c(3,2))
    plot(x_axis1, y_axis1, xlab=x_caption, ylab=y_caption, ylim=y_dim,
         pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
    # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
    # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
    if (line == TRUE) lines(x_axis1, y_axis1, col = "blue", lwd=2)
    points(linex1, liney1, col = "red", lwd=2)
    lines(linex1, liney1, col = "red", lwd=2)
    legend("topright", title = paste(plot_title), legend = c("Login Node 1 Pre-COVID", "Login Node 1 Post-COVID" ),
           col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
           lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
           text.col = c("black"),horiz = F )
    abline(h = 0, lwd = 2)

    plot(x_axis2, y_axis2, xlab=x_caption, ylab=y_caption, ylim=y_dim,
         pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
    # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
    # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
    if (line == TRUE) lines(x_axis2, y_axis2, col = "blue", lwd=2)
    points(linex2, liney2, col = "red", lwd=2)
    lines(linex2, liney2, col = "red", lwd=2)
    legend("topright", title = paste(plot_title), legend = c("Login Node 2 Pre-COVID", "Login Node 2 Post-COVID" ),
           col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
           lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
           text.col = c("black"),horiz = F )
    abline(h = 0, lwd = 2)

    plot(x_axis3, y_axis3, xlab=x_caption, ylab=y_caption, ylim=y_dim,
         pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3,  yaxt = ifelse(i == -1,'n', 's'))
    # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
    # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
    if (line == TRUE) lines(x_axis3, y_axis3, col = "blue", lwd=2)
    points(linex3, liney3, col = "red", lwd=2)
    lines(linex3, liney3, col = "red", lwd=2)
    legend("topright", title = paste(plot_title), legend = c("Login Node 3 Pre-COVID", "Login Node 3 Post-COVID" ),
           col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
           lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
           text.col = c("black"),horiz = F )
    abline(h = 0, lwd = 2)

    plot(x_axis4, y_axis4, xlab=x_caption, ylab=y_caption, ylim=y_dim,
         pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
    # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
    # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
    if (line == TRUE) lines(x_axis4, y_axis4, col = "blue", lwd=2)
    points(linex4, liney4, col = "red", lwd=2)
    lines(linex4, liney4, col = "red", lwd=2)
    legend("topright", title = paste(plot_title), legend = c("Login Node 4 Pre-COVID", "Login Node 4 Post-COVID" ),
           col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
           lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
           text.col = c("black"),horiz = F )
    abline(h = 0, lwd = 2)

    plot(x_axis5, y_axis5, xlab=x_caption, ylab=y_caption, ylim=y_dim,
         pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
    # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
    # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
    if (line == TRUE) lines(x_axis5, y_axis5, col = "blue", lwd=2)
    points(linex5, liney5, col = "red", lwd=2)
    lines(linex5, liney5, col = "red", lwd=2)
    legend("topright", title = paste(plot_title), legend = c("Login Node 5 Pre-COVID", "Login Node 5 Post-COVID" ),
           col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
           lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
           text.col = c("black"),horiz = F )
    abline(h = 0, lwd = 2)

    print(y_axis6)
    plot(x_axis6, y_axis6, xlab=x_caption, ylab=y_caption, ylim=y_dim,
         pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3,  yaxt = ifelse(i == -1,'n', 's'))
    # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
    # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
    if (line == TRUE) lines(x_axis6, y_axis6, col = "blue", lwd=2)
    points(linex6, liney6, col = "red", lwd=2)
    lines(linex6, liney6, col = "red", lwd=2)
    legend("topright", title = paste(plot_title), legend = c("Login Node Average Pre-COVID", "Login Node Average Post-COVID" ),
           col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
           lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
           text.col = c("black"),horiz = F )
    abline(h = 0, lwd = 2)
  }

}
dev.off()
}


{
  list1 <- c(2:6, 9:15)
  day <- ""
  title1_list <- c("Date", paste(day, "Average Hourly Users"), paste(day, "Average Hourly Running Processes"), paste(day, "Average Hourly CPU Load (1 min)"),
                   paste(day, "Average Hourly CPU Load (5 min)"), paste(day, "Average Hourly CPU Load (15 min)"), "Memory Total", "Memory Available",
                   paste(day, "Average Hourly Memory Used"), paste(day, "Average Hourly Unaliased ls Time"), paste(day, "Average Color ls Time"), paste(day, "Average Hourly Running Jobs"),
                   paste(day, "Average Hourly Total Jobs"), paste(day, "Average Hourly Time to Create 1G"), paste(day, "Average Hourly Percent Disk Utilization"))
  y_cap1_list <- c("Date", "Users", "Running Processes", "CPU Load (1 min)",
                   "CPU Load (5 min)", "CPU Load (15 min)", "Memory Total", "Memory Available",
                   "Memory Used (kb)", "Unaliased ls Time (sec)", "Color ls Time (sec)", "Running Jobs",
                   "Total Jobs", "Time to Create 1G (sec)", "Percent Disk Utilization")
  
  pdfind1list <- c("date", "user_bar.pdf", "proc_bar.pdf", "load1_bar.pdf", "load5_bar.pdf", "load15_bar.pdf",
                   "memtotal", "memavailable", "memused_bar.pdf", "uals_bar.pdf", "colorls_bar.pdf", "runjob_bar.pdf",
                   "totjob_bar.pdf", "create1G_bar.pdf", "percdu_bar.pdf")
  
  pdf("compiled_COVID.pdf", width = pdfwidth, height = pdfheight)
  par(mfrow=c(3,2))
  for(i in list1){
    print(i)
    
    temp1 <- subset(ln1mean, covidstatus == 0)
    temp2 <- subset(ln2mean, covidstatus == 0)
    temp3 <- subset(ln3mean, covidstatus == 0)
    temp4 <- subset(ln4mean, covidstatus == 0)
    temp5 <- subset(ln5mean, covidstatus == 0)
    temp6 <- subset(totalmean, covidstatus == 0)
    temp7 <- subset(ln1mean, covidstatus == 1)
    temp8 <- subset(ln2mean, covidstatus == 1)
    temp9 <- subset(ln3mean, covidstatus == 1)
    temp10 <- subset(ln4mean, covidstatus == 1)
    temp11 <- subset(ln5mean, covidstatus == 1)
    temp12 <- subset(totalmean, covidstatus == 1)
    
    #Use summitv2_log# to use all hours (more intensive and harder to read)
    #y axis selections
    y_axis1 <- temp1[,i]
    y_axis2 <- temp2[,i]
    y_axis3 <- temp3[,i]
    y_axis4 <- temp4[,i]
    y_axis5 <- temp5[,i]
    y_axis6 <- temp6[,i]
    
    liney1 = temp7[,i]
    liney2 = temp8[,i]
    liney3 = temp9[,i]
    liney4 = temp10[,i]
    liney5 = temp11[,i]
    liney6 = temp12[,i]
    
    #X axis selections (Will probably always be date)
    #Use summitv2_log# to use all hours (more intensive and harder to read)
    x_axis1 <- temp1$hour
    x_axis2 <- temp2$hour
    x_axis3 <- temp3$hour
    x_axis4 <- temp4$hour
    x_axis5 <- temp5$hour
    x_axis6 <- temp6$hour
    
    linex1 = temp7$hour
    linex2 = temp8$hour
    linex3 = temp9$hour
    linex4 = temp10$hour
    linex5 = temp11$hour
    linex6 = temp12$hour
    
    #Captioning and Title
    x_caption <- "Hour"
    y_caption <- y_cap1_list[i]
    plot_title<- title1_list[i]
    
    #Y-axis dimensions and PDF dimensions
    y_dim <- c(0, max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))
               + max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.4)
    #if(i == 10 || i == 11) y_dim <- c(min(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6)) +
    #                                   min(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.1,
    #                                 max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6)) +
    #                                   max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.1)
    
    pdfwidth<-16
    pdfheight<-12
    
    
    {
      par(mfrow=c(3,2))
      plot(x_axis1, y_axis1, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis1, y_axis1, col = "blue", lwd=2)
      points(linex1, liney1, col = "red", lwd=2)
      lines(linex1, liney1, col = "red", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 1 Pre-COVID", "Login Node 1 Post-COVID" ),
             col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
             lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis2, y_axis2, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis2, y_axis2, col = "blue", lwd=2)
      points(linex2, liney2, col = "red", lwd=2)
      lines(linex2, liney2, col = "red", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 2 Pre-COVID", "Login Node 2 Post-COVID" ),
             col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
             lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis3, y_axis3, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3,  yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis3, y_axis3, col = "blue", lwd=2)
      points(linex3, liney3, col = "red", lwd=2)
      lines(linex3, liney3, col = "red", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 3 Pre-COVID", "Login Node 3 Post-COVID" ),
             col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
             lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis4, y_axis4, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis4, y_axis4, col = "blue", lwd=2)
      points(linex4, liney4, col = "red", lwd=2)
      lines(linex4, liney4, col = "red", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 4 Pre-COVID", "Login Node 4 Post-COVID" ),
             col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
             lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis5, y_axis5, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis5, y_axis5, col = "blue", lwd=2)
      points(linex5, liney5, col = "red", lwd=2)
      lines(linex5, liney5, col = "red", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 5 Pre-COVID", "Login Node 5 Post-COVID" ),
             col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
             lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      print(y_axis6)
      plot(x_axis6, y_axis6, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="blue", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3,  yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis6, y_axis6, col = "blue", lwd=2)
      points(linex6, liney6, col = "red", lwd=2)
      lines(linex6, liney6, col = "red", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node Average Pre-COVID", "Login Node Average Post-COVID" ),
             col = c("blue", "red"), bty = "b",pch = c(20, 20, NA, NA, NA, NA, NA),
             lty = c(1,1, NA, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
    }
    
  }
  dev.off()
}


#wo_ho_we
{
  list1 <- c(2:6, 9:15)
  day <- "Workday "
  day1 <- "Weekend "
  day2 <- "Holiday "
  title1_list <- c("Date", paste(day, "Average Hourly Users"), paste(day, "Average Hourly Running Processes"), paste(day, "Average Hourly CPU Load (1 min)"),
                   paste(day, "Average Hourly CPU Load (5 min)"), paste(day, "Average Hourly CPU Load (15 min)"), "Memory Total", "Memory Available",
                   paste(day, "Average Hourly Memory Used"), paste(day, "Average Hourly Unaliased ls Time"), paste(day, "Average Color ls Time"), paste(day, "Average Hourly Running Jobs"),
                   paste(day, "Average Hourly Total Jobs"), paste(day, "Average Hourly Time to Create 1G"), paste(day, "Average Hourly Percent Disk Utilization"))
  title2_list <- c("Date", paste(day1, "Average Hourly Users"), paste(day1, "Average Hourly Running Processes"), paste(day1, "Average Hourly CPU Load (1 min)"),
                   paste(day1, "Average Hourly CPU Load (5 min)"), paste(day1, "Average Hourly CPU Load (15 min)"), "Memory Total", "Memory Available",
                   paste(day1, "Average Hourly Memory Used"), paste(day1, "Average Hourly Unaliased ls Time"), paste(day1, "Average Color ls Time"), paste(day1, "Average Hourly Running Jobs"),
                   paste(day1, "Average Hourly Total Jobs"), paste(day1, "Average Hourly Time to Create 1G"), paste(day1, "Average Hourly Percent Disk Utilization"))
  title3_list <- c("Date", paste(day2, "Average Hourly Users"), paste(day2, "Average Hourly Running Processes"), paste(day2, "Average Hourly CPU Load (1 min)"),
                   paste(day2, "Average Hourly CPU Load (5 min)"), paste(day2, "Average Hourly CPU Load (15 min)"), "Memory Total", "Memory Available",
                   paste(day2, "Average Hourly Memory Used"), paste(day2, "Average Hourly Unaliased ls Time"), paste(day2, "Average Color ls Time"), paste(day2, "Average Hourly Running Jobs"),
                   paste(day2, "Average Hourly Total Jobs"), paste(day2, "Average Hourly Time to Create 1G"), paste(day2, "Average Hourly Percent Disk Utilization"))
  
  y_cap1_list <- c("Date", "Users", "Running Processes", "CPU Load (1 min)",
                   "CPU Load (5 min)", "CPU Load (15 min)", "Memory Total", "Memory Available",
                   "Memory Used (kb)", "Unaliased ls Time (sec)", "Color ls Time (sec)", "Running Jobs",
                   "Total Jobs", "Time to Create 1G (sec)", "Percent Disk Utilization")
  
  pdfind1list <- c("date", "user_bar.pdf", "proc_bar.pdf", "load1_bar.pdf", "load5_bar.pdf", "load15_bar.pdf",
                   "memtotal", "memavailable", "memused_bar.pdf", "uals_bar.pdf", "colorls_bar.pdf", "runjob_bar.pdf",
                   "totjob_bar.pdf", "create1G_bar.pdf", "percdu_bar.pdf")
  
  pdf("compiled_wo_ho_we.pdf", width = pdfwidth, height = pdfheight)
  par(mfrow=c(3,2))
  for(i in list1){
    print(i)
    
    temp1 <- ln1wm
    temp2 <- ln2wm
    temp3 <- ln3wm
    temp4 <- ln4wm
    temp5 <- ln5wm
    temp6 <- twm
    temp7 <- ln1wkm
    temp8 <- ln2wkm
    temp9 <- ln3wkm
    temp10 <- ln4wkm
    temp11 <- ln5wkm
    temp12 <- twkm
    temp13 <- ln1hm
    temp14 <- ln2hm
    temp15 <- ln3hm
    temp16 <- ln4hm
    temp17 <- ln5hm
    temp18 <- thm
    
    #Use summitv2_log# to use all hours (more intensive and harder to read)
    #y axis selections
    y_axis1 <- temp1[,i]
    y_axis2 <- temp2[,i]
    y_axis3 <- temp3[,i]
    y_axis4 <- temp4[,i]
    y_axis5 <- temp5[,i]
    y_axis6 <- temp6[,i]
    
    liney1 = temp7[,i]
    liney2 = temp8[,i]
    liney3 = temp9[,i]
    liney4 = temp10[,i]
    liney5 = temp11[,i]
    liney6 = temp12[,i]
    
    lineyy1 = temp13[,i]
    lineyy2 = temp14[,i]
    lineyy3 = temp15[,i]
    lineyy4 = temp16[,i]
    lineyy5 = temp17[,i]
    lineyy6 = temp18[,i]
    
    #X axis selections (Will probably always be date)
    #Use summitv2_log# to use all hours (more intensive and harder to read)
    x_axis1 <- temp1$hour
    x_axis2 <- temp2$hour
    x_axis3 <- temp3$hour
    x_axis4 <- temp4$hour
    x_axis5 <- temp5$hour
    x_axis6 <- temp6$hour
    
    linex1 = temp7$hour
    linex2 = temp8$hour
    linex3 = temp9$hour
    linex4 = temp10$hour
    linex5 = temp11$hour
    linex6 = temp12$hour
    
    linexx1 = temp13$hour
    linexx2 = temp14$hour
    linexx3 = temp15$hour
    linexx4 = temp16$hour
    linexx5 = temp17$hour
    linexx6 = temp18$hour
    
    #Captioning and Title
    x_caption <- "Hour"
    y_caption <- y_cap1_list[i]
    plot_title<- title1_list[i]
    
    #Y-axis dimensions and PDF dimensions
    y_dim <- c(0, max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6), na.omit(lineyy1), na.omit(lineyy2), na.omit(lineyy3), na.omit(lineyy4), na.omit(lineyy5), na.omit(lineyy6))
               + max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6), na.omit(lineyy1), na.omit(lineyy2), na.omit(lineyy3), na.omit(lineyy4), na.omit(lineyy5), na.omit(lineyy6))*0.6)
    #if(i == 10 || i == 11) y_dim <- c(min(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6)) +
    #                                   min(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.1,
    #                                 max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6)) +
    #                                   max(na.omit(y_axis1), na.omit(y_axis2), na.omit(y_axis3), na.omit(y_axis4), na.omit(y_axis5), na.omit(y_axis6), na.omit(liney1), na.omit(liney2), na.omit(liney3), na.omit(liney4), na.omit(liney5), na.omit(liney6))*0.1)
    
    pdfwidth<-16
    pdfheight<-12
    
    
    {
      par(mfrow=c(3,2))
      plot(x_axis1, y_axis1, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="aquamarine1", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis1, y_axis1, col = "aquamarine1", lwd=2)
      points(linex1, liney1, col = "aquamarine3", lwd=2)
      lines(linex1, liney1, col = "aquamarine3", lwd=2)
      points(linexx1, lineyy1, col = "aquamarine4", lwd=2)
      lines(linexx1, lineyy1, col = "aquamarine4", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 1 Workday", "Login Node 1 Weekend", "Login Node 1 Holiday" ),
             col = c("aquamarine1", "aquamarine3", "aquamarine4"), bty = "b",pch = c(20, 20, 20, NA, NA, NA, NA),
             lty = c(1,1, 1, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis2, y_axis2, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="aquamarine1", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis2, y_axis2, col = "aquamarine1", lwd=2)
      points(linex2, liney2, col = "aquamarine3", lwd=2)
      lines(linex2, liney2, col = "aquamarine3", lwd=2)
      points(linexx2, lineyy2, col = "aquamarine4", lwd=2)
      lines(linexx2, lineyy2, col = "aquamarine4", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 2 Workday", "Login Node 2 Weekend", "Login Node 2 Holiday" ),
             col = c("aquamarine1", "aquamarine3", "aquamarine4"), bty = "b",pch = c(20, 20, 20, NA, NA, NA, NA),
             lty = c(1,1, 1, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis3, y_axis3, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="aquamarine1", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3,  yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis3, y_axis3, col = "aquamarine1", lwd=2)
      points(linex3, liney3, col = "aquamarine3", lwd=2)
      lines(linex3, liney3, col = "aquamarine3", lwd=2)
      points(linexx3, lineyy3, col = "aquamarine4", lwd=2)
      lines(linexx3, lineyy3, col = "aquamarine4", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 3 Workday", "Login Node 3 Weekend", "Login Node 3 Holiday" ),
             col = c("aquamarine1", "aquamarine3", "aquamarine4"), bty = "b",pch = c(20, 20, 20, NA, NA, NA, NA),
             lty = c(1,1, 1, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis4, y_axis4, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="aquamarine1", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      if (line == TRUE) lines(x_axis4, y_axis4, col = "aquamarine1", lwd=2)
      points(linex4, liney4, col = "aquamarine3", lwd=2)
      lines(linex4, liney4, col = "aquamarine3", lwd=2)
      points(linexx4, lineyy4, col = "aquamarine4", lwd=2)
      lines(linexx4, lineyy4, col = "aquamarine4", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 4 Workday", "Login Node 4 Weekend", "Login Node 4 Holiday" ),
             col = c("aquamarine1", "aquamarine3", "aquamarine4"), bty = "b",pch = c(20, 20, 20, NA, NA, NA, NA),
             lty = c(1,1, 1, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      plot(x_axis5, y_axis5, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="aquamarine1", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3, yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis5, y_axis5, col = "aquamarine1", lwd=2)
      points(linex5, liney5, col = "aquamarine3", lwd=2)
      lines(linex5, liney5, col = "aquamarine3", lwd=2)
      points(linexx5, lineyy5, col = "aquamarine4", lwd=2)
      lines(linexx5, lineyy5, col = "aquamarine4", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node 5 Workday", "Login Node 5 Weekend", "Login Node 5 Holiday" ),
             col = c("aquamarine1", "aquamarine3", "aquamarine4"), bty = "b",pch = c(20, 20, 20, NA, NA, NA, NA),
             lty = c(1,1, 1, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
      
      print(y_axis6)
      plot(x_axis6, y_axis6, xlab=x_caption, ylab=y_caption, ylim=y_dim,
           pch=20,col="aquamarine1", cex.lab = 1.3, cex.axis = 1.3, cex = 1.3,  yaxt = ifelse(i == -1,'n', 's'))
      # if(i == 10 || i == 11) axis(2, las = 1, cex.axis = 1.3, at = seq(-2.8, -1.8, 0.2), cex = 1.3, labels = c(expression(paste("10"^-2.8)), expression(paste("10"^-2.6)), expression(paste("10"^-2.4)), expression(paste("10"^-2.2)), expression(paste("10"^-2)), expression(paste("10"^-1.8))))
      # if(i == 14) axis(2, las = 1, cex.axis = 1.3, at = seq(0, 2, 0.5), cex = 1.3, labels = c(expression(paste("10"^0)), expression(paste("10"^0.5)), expression(paste("10"^1.0)), expression(paste("10"^1.5)), expression(paste("10"^2))))
      if (line == TRUE) lines(x_axis6, y_axis6, col = "aquamarine1", lwd=2)
      points(linex6, liney6, col = "aquamarine3", lwd=2)
      lines(linex6, liney6, col = "aquamarine3", lwd=2)
      points(linexx6, lineyy6, col = "aquamarine4", lwd=2)
      lines(linexx6, lineyy6, col = "aquamarine4", lwd=2)
      legend("topright", title = paste(plot_title), legend = c("Login Node Average Workday", "Login Node Average Weekend", "Login Node Average Holiday" ),
             col = c("aquamarine1", "aquamarine3", "aquamarine4"), bty = "b",pch = c(20, 20, 20, NA, NA, NA, NA),
             lty = c(1,1, 1, NA, NA, NA), pt.cex = 1.2, cex = 1.4, lwd = c(2, 2, 2),
             text.col = c("black"),horiz = F )
      abline(h = 0, lwd = 2)
    }
    
  }
  dev.off()
}
