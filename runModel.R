library(R2jags)
library(stringr)

setwd("C:/Users/au199986/OneDrive - Aarhus Universitet/Research/ViborgFF/GlorijaPeter")

#dat <- read.csv("logfile_3_U13.csv")
#dat <- read.csv("logfile_4_U13.csv")
#dat <- read.csv("logfile_3_U14.csv")
#dat <- read.csv("logfile_13_U14.csv")
#dat <- read.csv("logfile_3_U17.csv")
#dat <- read.csv("logfile_20_U17.csv")
#dat <- read.csv("logfile_25_U17.csv")
#dat <- read.csv("logfile_3_U19.csv")
dat <- read.csv("logfile_5_U19.csv")


# categories videos in terms of best response
stim <- as.integer(str_sub(dat$Video,-5,-5))

# count stim numbers
nA <- sum(stim==1)
nB <- sum(stim==2)
nC <- sum(stim==3)
nD <- sum(stim==4)

# categorise responses in terms of stimulus categories
resp <- dat$Decision
resp[resp=="highest free man" | resp=="false highest free man" |
      resp=="highest free man2" | resp=="false highest free man2" ]<-1
resp[resp=="link out" | resp=="false link out" | 
       resp=="link out2" | resp=="false link out2" ]<-2
resp[resp=="switch play" | resp=="false switch play" |
       resp=="switch play2" | resp=="false switch play2" ]<-3
resp[resp=="pass it over" | resp=="false pass it over" |
       resp=="pass it over2" | resp=="false pass it over2" ]<-4

resp[resp=="false link out "] <- 2
resp[resp=="switch play "]<-3
resp[resp=="false switch play2 "]<-3

resp <- as.integer(resp)  

# count responses as H, M, FA, relative to response rank - see model outline on remarkable
HA <- sum(stim==1 & resp==1)
MA <- sum(stim==1 & resp>1) # all errors where stim is mistaken for lower ranked stimuli are "misses"
FA <- sum(stim>1 & resp==1) # all errors where stim is mistaken for higher ranked stimuli are "false alarms"

HB <- sum(stim==2 & resp==2)
MB <- sum(stim==2 & resp>2) # all errors where stim is mistaken for lower ranked stimuli are "misses"
FB <- sum(stim>2 & resp==2) # all errors where stim is mistaken for higher ranked stimuli are "false alarms"
            
HC <- sum(stim==3 & resp==3)
MC <- sum(stim==3 & resp==4) # all errors where stim is mistaken for lower ranked stimuli are "misses"
FC <- sum(stim==4 & resp==3) # all errors where stim is mistaken for higher ranked stimuli are "false alarms"
            
HD <- sum(stim==4 & resp==4)          

data <- list("nA","nB","nC","nD",
             "HA","HB","HC","HD",
             "FA","FB","FC")

params <- c("kappa_A","kappa_B","kappa_C","kappa_D",
            "gamma_A","gamma_B","gamma_C","gamma_D")

samples <- jags(data, inits=NULL, params,
                model.file ="MPT_decision.txt",
                n.chains=3, n.iter=5000, n.burnin=1000, n.thin=1)

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
     samples$BUGSoutput$sims.list$gamma_D,
     xlim=c(0,1),ylim=c(0,1),
     col=rgb(.1,.8,.1,.1),pch = 16,
     xlab = "Know",ylab = "Guess",main = "Over opponent")
points(mean(samples$BUGSoutput$sims.list$kappa_D),
       mean(samples$BUGSoutput$sims.list$gamma_D),
       col="red",pch=13)




# 
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



#------ From Peter's email explaining how the videos are labelled -------------------------------- 
#Another important change that will help us with coding and streamlining the process is file naming.
#Use underscores in the folder names instead of space bar - folder names Klip_1, Klip_2, Klip_3…
#We have also thought about how the videos/options should be encoded, essentially having the last 
#index indicate what principle the video/option is related to. In addition, the options should be 
#further encoded to show which principle a bad solution belongs to.

#So something like this:
#1: Highest free man 
#2: Link out
#3: Switch play to open lines 
#4: Pass it over the opponent
#5: bad solution with Highest free man
#6: bad solution with Link out
#7: bad solution with Switch play to open lines
#8: bad solution with Pass it over the opponent
