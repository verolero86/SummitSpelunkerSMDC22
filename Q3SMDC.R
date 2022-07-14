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
colnames(summitraw)[1] = 'node'

for(i in (1:5)){
sr <- subset(summitraw, node == paste("login", i, sep = ""))
sr <- summitraw

df = data.frame(sr$node, sr$hour, sr$users, sr$running_procs, sr$running_jobs, sr$mem_used, sr$cpu_load_15_min)
temp = df[, c('sr.users', 'sr.running_procs', 'sr.running_jobs', 'sr.mem_used', 'sr.cpu_load_15_min')]
df$mah = mahalanobis(temp, colMeans(temp), cov(temp)) 

df = df[order(df$mah),]

df$usersub =(df$sr.users - colMeans(temp)[1])/colMeans(temp)[1] 
df$procsub = (df$sr.running_procs - colMeans(temp)[2])/colMeans(temp)[2] 
df$jobssub = (df$sr.running_jobs - colMeans(temp)[3])/colMeans(temp)[3] 
df$memusub = (df$sr.mem_used-colMeans(temp)[4])/colMeans(temp)[4] 
df$cpulsub = (df$sr.cpu_load_15_min-colMeans(temp)[5])/colMeans(temp)[5] 
df$res_sum = rowSums(df[,c("usersub", "procsub", "jobssub", "memusub", "cpulsub")])

dfleft = subset(df, res_sum <= 0)
dfright = subset(df, res_sum > 0)

# dftop = df[seq(61896, 82528, 1),]
# dfbottom = df[seq(0, 20632, 1),]
# dfmid = df[seq(20632, 61896, 1),]

dftopavg = colMeans(dfright[, c('sr.users', 'sr.running_procs', 'sr.running_jobs', 'sr.mem_used', 'sr.cpu_load_15_min')])
dfmidavg = colMeans(df[, c('sr.users', 'sr.running_procs', 'sr.running_jobs', 'sr.mem_used', 'sr.cpu_load_15_min')])
dfbottomavg = colMeans(dfleft[, c('sr.users', 'sr.running_procs', 'sr.running_jobs', 'sr.mem_used', 'sr.cpu_load_15_min')])

df$topdist = mahalanobis(temp, dftopavg, cov(temp)) 
df$middist = mahalanobis(temp, dfmidavg, cov(temp)) 
df$bottomdist = mahalanobis(temp, dfbottomavg, cov(temp)) 
df$mindist = apply(df[,c('topdist', 'middist', 'bottomdist')], 1, min)

df <- df %>%
  mutate(class = case_when(
    bottomdist==mindist ~ "Light",
    middist==mindist ~ "Average",
    topdist==mindist ~ "Intensive",
    ))

df1 <- subset(df, sr.node == "login1")
df2<- subset(df, sr.node == "login2")
df3<- subset(df, sr.node == "login3")
df4<- subset(df, sr.node == "login4")
df5<- subset(df, sr.node == "login5")

test1_2<-t.test(df1$mah, df2$mah)
test1_3<-t.test(df1$mah, df3$mah)
test1_4<-t.test(df1$mah, df4$mah)
test1_5<-t.test(df1$mah, df5$mah)
# dfwork = subset(df, sr.hour %in% (8:18))
# 
# itest = subset(dfwork, class == "Intensive")
# atest = subset(dfwork, class == "Average")
# ltest = subset(dfwork, class == "Light")
# 
# print(paste("login",i, sep = ""))
# print(paste("Intensive:", dim(itest)[1]))
# print(paste("Average:", dim(atest)[1]))
# print(paste("Light:", dim(ltest)[1]))
# print("")


itest = subset(df, class == "Intensive")
atest = subset(df, class == "Average")
ltest = subset(df, class == "Light")

print(paste("login",i, sep = ""))
print(paste("Intensive:", dim(itest)[1]))
print(paste("Average:", dim(atest)[1]))
print(paste("Light:", dim(ltest)[1]))
print("")

}

scat1 <- ggplot(df, aes(x=sr.node, y=usersub, color=class)) +
  geom_violin(position=position_jitter(0.1))+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color="red")+
  scale_x_discrete(limits=c("login1", "login2", "login3", "login4", "login5"), labels=c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"))+
  labs(x="Nodes", y = expression("Normalized Users Residual"),)

scat2 <- ggplot(df, aes(x=sr.node, y=procsub, color=class)) +
  geom_violin(position=position_jitter(0.1))+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color="red")+
  scale_x_discrete(limits=c("login1", "login2", "login3", "login4", "login5"), labels=c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"))+
  labs(x="Nodes", y = expression("Normalized Running Processes Residual"),)

scat3 <- ggplot(df, aes(x=sr.node, y=jobssub, color=class)) +
  geom_violin(position=position_jitter(0.1))+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color="red")+
  scale_x_discrete(limits=c("login1", "login2", "login3", "login4", "login5"), labels=c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"))+
  labs(x="Nodes", y = expression("Normalized Running Jobs Residual"),)

scat4 <- ggplot(df, aes(x=sr.node, y=memusub, color=class)) +
  geom_violin(position=position_jitter(0.1))+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color="red")+
  scale_x_discrete(limits=c("login1", "login2", "login3", "login4", "login5"), labels=c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"))+
  labs(x="Nodes", y = expression("Normalized Memory Utilization Residual"),)

scat5 <- ggplot(df, aes(x=sr.node, y=cpulsub, color=class)) +
  geom_violin(position=position_jitter(0.1))+
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, color="red")+
  scale_x_discrete(limits=c("login1", "login2", "login3", "login4", "login5"), labels=c("Node 1", "Node 2", "Node 3", "Node 4", "Node 5"))+
  labs(x="Nodes", y = expression("Normalized CPU Load Residual (15 min)"),)



multi.page <- ggarrange(scat1, scat2, scat3, 
                        scat4, scat5,
                        ncol = 2, nrow = 3, legend = "none")

pdf("compiled_violin.pdf", width = pdfwidth, height = pdfheight)
multi.page
dev.off()