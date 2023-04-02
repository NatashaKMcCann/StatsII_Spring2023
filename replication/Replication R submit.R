###########################################
# Code for Adoption of Merit Aid Programs #
# Will Doyle 7/24/06                      # 
# w.doyle@vanderbilt.edu                  #
###########################################

###########################################
# Required Libraries
###########################################
install.packages("maps")
install.packages("rms")
library(rms)
library(lattice)
library(maps)

###########################################
#Analysis and Tables
###########################################


merit1<- meritfinal
attach(merit1)
S2a<-Surv(startyear,endyear,meritstate.obs)

#Code it into non-time dependent dataset

mymerit<-rep(0,length(merit1))
mymerit[merit1$meritstate.obs==1]<-1              
mymerit[merit1$endyear==13]<-1              
newdata<-data.frame(merit1,mymerit)
newdata<-subset(newdata,subset=newdata$mymerit==1)

rep.1<-rep(0,dim(newdata)[1])
i<-2:50
  rep.1[newdata$state[i]==newdata$state[i-1]]<-1
rep.2<-rep(0,dim(newdata)[1])
i<-1:49
  rep.2[2:50]<-rep.1[1:49]

newdata<-subset(newdata,subset=(rep.2==0))
attach(newdata)
s2<-Surv(endyear,meritstate.obs,type='right')

#models

#Diffusion only: Models 1 and 1a

par.diffuse<-coxph(s2~diffuse+cluster(state),data=newdata,y=T) #Model 1
par.region<-coxph(s2~region.1+cluster(state),data=newdata,y=T) #Mdel 1a

#educational variables only: Model 2

par.educ<-coxph(s2~continue+outmig+attain+I(composite/100)
                +cluster(state),data=newdata,y=T) #model 2

#political variables only: Model 3
par.political<-coxph(s2~govideo+prc.updem+first.year
                     +cluster(state),data=newdata,y=T) #model 3

#all at once: Model 4

par.all<-coxph(s2~diffuse+ #diffusion
               continue+outmig+attain+I(composite/100)+ #educational block
               govideo+prc.updem+first.year+ #political 
               income.i+perc1824+ #Controls
               cluster(state),data=newdata,y=T,x=T)
par.all
###########################################
# Predicted probabilities
###########################################

par.all.1<-cph(s2~diffuse+ #diffusion
               continue+outmig+attain+composite+ #educational block
               govideo+prc.updem+first.year+ #political 
               income.i+perc1824 #Controls
               ,data=newdata,x=T,y=T)
#Diffusion

my.data.diffuse<- expand.grid(list(diffuse=c(0,1,2),
                                     continue=mean(newdata$continue),
                                     outmig=mean(newdata$outmig),
                                     attain=mean(newdata$attain),
                                     composite=mean(newdata$composite),
                                     govideo=mean(newdata$govideo),
                                     prc.updem=mean(na.omit(newdata$prc.updem)),
                                     first.year=mean(newdata$first.year),
                                     income.i=mean(newdata$income.i),
                                     perc1824=mean(perc1824)
                                     )
                              )

survtime.diffuse<-survest(par.all.1,my.data.diffuse,times=c(12))

#Continue

my.data.continue<- expand.grid(list(diffuse=mean(newdata$diffuse),
                                     continue=c(min(newdata$continue),
                                       max(newdata$continue)),
                                     outmig=mean(newdata$outmig),
                                     attain=mean(newdata$attain),
                                     composite=mean(newdata$composite),
                                     govideo=mean(newdata$govideo),
                                     prc.updem=mean(na.omit(newdata$prc.updem)),
                                     first.year=mean(newdata$first.year),
                                     income.i=mean(newdata$income.i),
                                     perc1824=mean(perc1824)
                                     )
                               )

survtime.continue<-survest(par.all.1,my.data.continue,times=(12))

#Outmig

my.data.outmig<- expand.grid(list(diffuse=mean(newdata$diffuse),
                                     continue=mean(newdata$continue),
                                     outmig=c(min(newdata$outmig),
                                       max(newdata$outmig)),
                                     attain=mean(newdata$attain),
                                     composite=mean(newdata$composite),
                                     govideo=mean(newdata$govideo),
                                     prc.updem=mean(na.omit(newdata$prc.updem)),
                                     first.year=mean(newdata$first.year),
                                     income.i=mean(newdata$income.i),
                                     perc1824=mean(perc1824)
                                  )
                             )

survtime.outmig<-survest(par.all.1,my.data.outmig,times=(12))

#attain

my.data.attain<- expand.grid(list(diffuse=mean(newdata$diffuse),
                                     continue=mean(newdata$continue),
                                     outmig=mean(newdata$outmig),
                                     attain=c(min(newdata$attain),
                                       max(newdata$attain)),
                                     composite=mean(newdata$composite),
                                     govideo=mean(newdata$govideo),
                                     prc.updem=mean(na.omit(newdata$prc.updem)),
                                     first.year=mean(newdata$first.year),
                                     income.i=mean(newdata$income.i),
                                     perc1824=mean(perc1824)
                                  )
                             )

survtime.attain<-survest(par.all.1,my.data.attain,times=(12))

#first year

my.data.first.year<-expand.grid(list(diffuse=mean(newdata$diffuse),
                                     continue=mean(newdata$continue),
                                     outmig=mean(newdata$outmig),
                                     attain=mean(newdata$attain),
                                     composite=mean(newdata$composite),
                                     govideo=mean(newdata$govideo),
                                     prc.updem=mean(na.omit(newdata$prc.updem)),
                                     first.year=c(0,1),
                                     income.i=mean(newdata$income.i),
                                     perc1824=mean(perc1824)
                                     )
                                )

survtime.first.year<-survest(par.all.1,my.data.first.year,times=(12))


#income

my.data.income<- expand.grid(list(diffuse=mean(newdata$diffuse),
                                     continue=mean(newdata$continue),
                                     outmig=mean(newdata$outmig),
                                     attain=mean(newdata$attain),
                                     composite=mean(newdata$composite),
                                     govideo=mean(newdata$govideo),
                                     prc.updem=mean(na.omit(newdata$prc.updem)),
                                     first.year=mean(first.year),
                                     income.i=c(min(newdata$income.i),
                                       max(newdata$income.i)),
                                     perc1824=mean(perc1824)
                                  )
                             )

survtime.income<-survest(par.all.1,my.data.income,times=(12))

###########################################
# Model 4.a
###########################################

par.all.reg<-coxph(s2~region.1+ #diffusion
               continue+outmig+attain+I(composite/100)+ #educational block
               govideo+prc.updem+first.year+ #political 
               income.i+perc1824+ #Controls
               cluster(state),data=newdata,y=T)

data.frame(par.all$coefficients,vif(par.all))

par.all.restrict<-coxph(s2~
               diffuse+ #diffusion
               continue+outmig+attain+composite+ #educational block
               govideo+prc.updem+first.year+ #political 
               perc1824+ #Controls
               cluster(state),data=newdata,y=T)

###########################################
# Plots
###########################################

merit1<-read.csv(file="meritfinal.csv",header=TRUE,sep=",")

#Code it into non-time dependent dataset
mymerit<-rep(0,length(merit1))
mymerit[merit1$meritstate.obs==1]<-1              
mymerit[merit1$endyear==13]<-1              
newdata<-data.frame(merit1,mymerit)
newdata<-subset(newdata,subset=newdata$mymerit==1)
rep.1<-rep(0,dim(newdata)[1])
i<-2:50
  rep.1[newdata$state[i]==newdata$state[i-1]]<-1
rep.2<-rep(0,dim(newdata)[1])
i<-1:49
  rep.2[2:50]<-rep.1[1:49]

newdata<-subset(newdata,subset=(rep.2==0))

s2<-Surv(endyear,meritstate.obs,type='right')


#Figure 4

dev.new()
par(mfrow=c(2,2))

survplot(survfit(s2~1),xlab="Years",title="All States")

diffuse.dummy<-rep(0,length(diffuse))
diffuse.dummy[diffuse>0]<-1

survplot(survfit(s2~diffuse.dummy),
         label.curves=list(
           method="arrow",
           arrow.factor=1.5,
           labels=c("No neighbors","At least one neighbor"),
           cex=.66),
         xlab="Years",
         time.inc=1
         )
         
hi.attain<-rep(0,length(attain))
hi.attain[attain>mean(attain)+sd(attain)]<-1

survplot(survfit(s2~hi.attain),
         label.curves=list(
           method="arrow",
           arrow.factor=1.5,
           labels=c("All Other","High Attainment"),
           cex=.66
           ),
           xlab="Years"
         )

hi.conserve<-rep(0,length(govideo))
hi.conserve[govideo<mean(govideo)-sd(govideo)]<-1

survplot(survfit(s2~hi.conserve),
         label.curves=list(
           method="arrow",
           arrow.factor=1.5,
           labels=c("All Other","Highly Conservative"),
           cex=.66
           ),
           xlab="Years"
         )
dev.copy2pdf(file = "kmcurves.pdf")

mod.all<-cph(s2~diffuse+
             continue+outmig+attain+composite+
             govideo+prc.updem+first.year+
             income.i+perc1824,data=newdata,
             surv=T,
             x=T,
             y=T)

hypo.data<-expand.grid(list(
                            diffuse=0,
                            continue=quantile(continue),
                            outmig=mean(outmig),
                            attain=mean(attain),
                            composite=mean(composite),
                            govideo=mean(govideo),
                            prc.updem=mean(na.omit(prc.updem)),
                            first.year=mean(na.omit(first.year)),
                            income.i=mean(income.i),
                            perc1824=mean(perc1824)
                            ))

predict.all<-predict(mod.all,newdata=hypo.data)

dev.new()
par(mfrow=c(2,2))

survplot(mod.all,
         continue=c( (mean(continue)+sd(continue)),
           (mean(continue)),
           (mean(continue)-sd(continue))),
         diffuse=0,
         outmig=mean(outmig),
         attain=mean(attain),
         composite=mean(composite),
         govideo=mean(govideo),
         prc.updem=mean(na.omit(prc.updem)),
         first.year=mean(na.omit(first.year)),
         income.i=mean(income.i),
         perc1824=mean(perc1824),
         label.curves=list(
           method="arrow",
           labels=c("High Continue","Mean Continue", "Low Continue")
           ),
         xlab="Years",
         time.inc=1
         )

survplot(mod.all,
         outmig=c( (mean(outmig)+sd(outmig)),
           (mean(outmig)),
           (mean(outmig)-sd(outmig))),
         continue=mean(continue),
         diffuse=0,
         attain=mean(attain),
         composite=mean(composite),
         govideo=mean(govideo),
         prc.updem=mean(na.omit(prc.updem)),
         first.year=mean(na.omit(first.year)),
         income.i=mean(income.i),
         perc1824=mean(perc1824),
         label.curves=list(
           method="arrow",
           labels=c("High Migration","Mean Migration", "Low Migration")
           ),
         xlab="Years",         
         time.inc=1         
         )         

survplot(mod.all,
         attain=c( (mean(attain)+sd(attain)),
           (mean(attain)),
           (mean(attain)-sd(attain))),
         continue=mean(continue),
         diffuse=0,
         outmig=mean(outmig),
         composite=mean(composite),
         govideo=mean(govideo),
         prc.updem=mean(na.omit(prc.updem)),
         first.year=mean(na.omit(first.year)),
         income.i=mean(income.i),
         perc1824=mean(perc1824),
         label.curves=list(
           method="arrow",
           labels=c("High Attainment","Mean Attainment", "Low Attainment")
           ),
         xlab="Years" ,        
         time.inc=1         
         )         

survplot(mod.all,
         income.i=c( (mean(income.i)+sd(income.i)),
         (mean(income.i)),
         (mean(income.i)-sd(income.i))),
         continue=mean(continue),
         diffuse=0,
         attain=mean(attain),
         composite=mean(composite),
         govideo=mean(govideo),
         prc.updem=mean(na.omit(prc.updem)),
         first.year=mean(na.omit(first.year)),
         outmig=mean(outmig),
         perc1824=mean(perc1824),
         label.curves=list(
           method="arrow",
           labels=c("High Income","Mean Income", "Low Income")
           ),
         xlab="Years"  ,       
         time.inc=1         
         )

dev.copy2pdf(file = "predicthaz.pdf")


#Figure 5

state.data<-subset(newdata,subset=(meritstate.obs<1&state!='NE'))

my.test<-survest(mod.all,newdata=state.data,times=c(1:13))

predict.13<-my.test$surv[,13]
predict.state<-as.character(state.data$state)
my.f<-data.frame(predict.13,predict.state)[order(predict.13),]
     
dev.new()
dotchart(rev(my.f$predict.13),
         labels=(rev(as.character(my.f$predict.state))),
                 cex=.8)

dev.copy2pdf(file = "predictions.pdf")   


##Lattice Plot: Figure 2##

dev.new()


merit1<-read.csv(file="meritaid_describe.csv",header=TRUE,sep=",")   

xyplot(merit.fte.i~year|state,
       data=merit1,
       ylab="Merit Aid per FTE",
       xlab="State",as.table=T,
       type='l',
       layout=c(10,5)
       )

dev.copy2pdf(file = "lattice_fte.pdf")


##Maps## Figure 
            
dev.new(12,12)
par(mfrow=c(2,3))

for (i in c(1990,1994,1996,1998,2000,2002)){

longname.y<-merit1$longstate[merit1$year==i]


###########################################
# Merit aid as defined
###########################################


meritstate<-as.character(longname.y[merit1$meritstate.obs[merit1$year==i]==1])
notmeritstate<-as.character(longname.y[merit1$meritstate.obs[merit1$year==i]==0])

map("state",interior=F)
title(paste("",i,sep=""))
map("state",region=(meritstate),exact=F,add=T, col=gray(.5),fill=T)
map("state",region=(notmeritstate),exact=F,add=T, col='white',fill=T)
}

dev.copy2pdf(file = "meritmaps.pdf")


# Checking residuals of the models

par.all<-coxph(s2~diffuse+ #diffusion
                 continue+outmig+attain+I(composite/100)+ #educational block
                 govideo+prc.updem+first.year+ #political 
                 income.i+perc1824+ #Controls
                 cluster(state),data=newdata,y=T,x=T)

res1 <- residuals(par.all)
resplot <- plot(res1)
resplot

qq <- qqnorm(res1)
qq
plotqq <- plot(qq)
plotqq
qq

qq1 <- qqline(res1)


res1 <- residuals(par.all)
qqline(res1)


my_model <- lm(startyear ~ endyear + meritstate.year, data = meritfinal)
summary(my_model)$r.squared
par(mfrow = c(2,2))
m <- plot(my_model)
m

res2 <- residuals(meritfinal)
resplot2 <- plot(res2)
resplot2


#linear regressionata = meritfinal)
model <- attain ~ continue + outmig
model
mod3 <- lm(model, d
mod3
summary(mod3)


#poisson 
pois <- glm(diffuse ~ continue+ outmig, data = meritfinal, family = poisson)
summary(pois)

resid <- residuals(pois, type = "deviance")

