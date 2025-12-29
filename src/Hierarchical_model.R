library(R2jags)
library(stringr)

setwd("/work/mrks/Exam")

#dat <- read.csv("logfiles/logfile_3_U13.csv")
#dat <- read.csv("logfiles/logfile_4_U13.csv")
#dat <- read.csv("logfiles/logfile_3_U14.csv")
#dat <- read.csv("logfiles/logfile_13_U14.csv")
#dat <- read.csv("logfiles/logfile_3_U17.csv")
#dat <- read.csv("logfiles/logfile_20_U17.csv")
#dat <- read.csv("logfiles/logfile_25_U17.csv")
#dat <- read.csv("logfiles/logfile_3_U19.csv")
dat <- read.csv("logfiles/logfile_5_U19.csv")
#dat <- read.csv("logfiles/anonymized_data.csv")

# categories videos in terms of best response
stim <- as.integer(str_sub(dat$Video,-5,-5))

# count stim numbers
nA <- sum(stim==1)
nB <- sum(stim==2)
nC <- sum(stim==3)
nD <- sum(stim==4)

# categorise responses in terms of stimulus categories
resp <- dat$Decision
resp[resp=="highest free man" | resp=="highest free man2"]<-1
resp[resp=="link out" | resp=="link out2"]<-2
resp[resp=="switch play" | resp=="switch play2" | resp=="switch play "]<-3
resp[resp=="pass it over" | resp=="pass it over2"]<-4
resp[resp=="false highest free man" | resp=="false highest free man2"]<-5
resp[resp=="false link out" | resp=="false link out2" | resp=="false link out "]<-6
resp[resp=="false switch play" | resp=="false switch play2" | resp=="false switch play2 "]<-7
resp[resp=="false pass it over" | resp=="false pass it over2" ]<-8
resp <- as.integer(resp)  

# count responses as H, M, FA, relative to response rank - see model outline on remarkable
HA <- sum(stim==1 & resp==1)
HB <- sum(stim==2 & resp==2)
HC <- sum(stim==3 & resp==3)
HD <- sum(stim==4 & resp==4)
GB <- sum(stim==1 & resp==2)
GC <- sum(stim<3 & resp==3)
GD <- sum(stim<4 & resp==4)
MA <- sum(stim==1 & resp>4) 
MB <- sum(stim==2 & resp>4) 
MC <- sum(stim==3 & resp>4)
MD <- sum(stim==4 & resp>4)

data <- list("nA","nB","nC","nD",
             "HA","HB","HC","HD",
             "GB","GC","GD",
             "MA","MB","MC","MD")

params <- c("kappa_A", "kappa_B", "kappa_C","kappa_D", 
            "gamma_A","gamma_B","gamma_C","gamma_D")

samples <- jags(data, inits=NULL, params,
                model.file ="Models/MPT_hierarchical.txt",
                n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)

#png("results/false_alarm_results2.png", width = 800, height = 800)   # open PNG device
par(mfrow=c(2,2))
plot(samples$BUGSoutput$sims.list$kappa_A,
     samples$BUGSoutput$sims.list$gamma_A,
     xlim=c(0,1),ylim=c(0,1),
     col=rgb(.1,.8,.1,.1),pch = 16,
     xlab = "Know",ylab = "Guess",main = "Highest free pass")
points(mean(samples$BUGSoutput$sims.list$kappa_A),
       mean(samples$BUGSoutput$sims.list$gamma_A),
       col="red",pch=13)

plot(samples$BUGSoutput$sims.list$kappa_B,
     samples$BUGSoutput$sims.list$gamma_B,
     xlim=c(0,1),ylim=c(0,1),
     col=rgb(.1,.8,.1,.1),pch = 16,
     xlab = "Know",ylab = "Guess",main = "Link out")
points(mean(samples$BUGSoutput$sims.list$kappa_B),
       mean(samples$BUGSoutput$sims.list$gamma_B),
       col="red",pch=13)

plot(samples$BUGSoutput$sims.list$kappa_C,
     samples$BUGSoutput$sims.list$gamma_C,
     xlim=c(0,1),ylim=c(0,1),
     col=rgb(.1,.8,.1,.1),pch = 16,
     xlab = "Know",ylab = "Guess",main = "Switch play")
points(mean(samples$BUGSoutput$sims.list$kappa_C),
       mean(samples$BUGSoutput$sims.list$gamma_C),
       col="red",pch=13)

plot(samples$BUGSoutput$sims.list$kappa_D,
     1-samples$BUGSoutput$sims.list$gamma_D,
     xlim=c(0,1),ylim=c(0,1),
     col=rgb(.1,.8,.1,.1),pch = 16,
     xlab = "Know",ylab = "Guess",main = "Over opponent")
points(mean(samples$BUGSoutput$sims.list$kappa_D),
       mean(1-samples$BUGSoutput$sims.list$gamma_D),
       col="red",pch=13)

mtext("Hierarchical model", outer=TRUE, cex=1.5)

#dev.off()

# par(mfrow=c(2,2))
# plot(density(samples$BUGSoutput$sims.list$kappa_A),
#      xlim=c(0,1),ylim=c(0,15),
#      col="blue",lwd=2,
#      main="Highest open",
#      xlab = "Probability")
# lines(density(samples$BUGSoutput$sims.list$gamma_A),
#       col="red",lwd=2)
# 
# plot(density(samples$BUGSoutput$sims.list$kappa_B),
#      xlim=c(0,1),ylim=c(0,15),
#      col="blue",lwd=2,
#      main="Link out",
#      xlab = "Probability")
# lines(density(samples$BUGSoutput$sims.list$gamma_B),
#       col="red",lwd=2)
# 
# plot(density(samples$BUGSoutput$sims.list$kappa_C),
#      xlim=c(0,1),ylim=c(0,15),
#      col="blue",lwd=2,
#      main="Switch play to open lines",
#      xlab = "Probability")
# lines(density(samples$BUGSoutput$sims.list$gamma_C),
#       col="red",lwd=2)
# 
# plot(density(samples$BUGSoutput$sims.list$kappa_D),
#      xlim=c(0,1),ylim=c(0,15),
#      col="blue",lwd=2,
#      main="Over opponent",
#      xlab = "Probability")
# lines(density(samples$BUGSoutput$sims.list$gamma_D),
#       col="red",lwd=2)


