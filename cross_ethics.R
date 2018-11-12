######################################################################
# Cross analysis script for User Involvement Survey - Ethics

######################################################################

######################################################################
# Imports and constants
######################################################################

library(foreign)
library(ggplot2)
library(GGally)
library(data.table)
library(gmodels)
library(Hmisc)
library(corrplot)
library(magrittr)
library(reshape2)
library(scales)
library(readr)
library(car)
library(rgl)
library(nFactors)
library(cluster)
library(pvclust)

POPULATION.SIZE = 397
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year
set.seed(2017)

######################################################################
# Read raw data files
######################################################################

### ERICSSON ###
data1 <- read.csv("raportti_ericsson.csv")
data1 <- data.frame(data1)
data1$jobfunction1 <- factor(data1$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                             levels = c(0:6),
                             labels = c("Developing software", "Testing software", "UX design", "Management", "System or network operations", "Software architecture", "Other"))
data1$jobfunction1.other <- data1$If.other..please.specify
data1$jobfunction1[4] <- "Developing software" #Fixing
data1$jobfunction1.other[4] <- ""
data1$jobfunction1[9] <- "Management" #Fixing custoemr manager to mng
data1$jobfunction1.other[9] <- ""
data1$jobfunction1[15] <- "Management" #Fixing product owner to mng
data1$jobfunction1.other[15] <- ""

# Recode job functions to roles, collapsing everything to four roles (Developer, UX designer, Manager, Other)
data1$role <- recode(data1$jobfunction1,
                     "c('Developing software','Testing software','System or network operations','Software architecture')='Developer'; c('UX design')='UX designer'; c('Management')='Manager'")

attach(data1)
# Job function
print("Primary job function")
summary(jobfunction1)
ggplot(data1, aes(x=jobfunction1)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 9, 21), labels = c("0", "2", "3", "9", "21"))
data1$jobfunction1.other

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data1$usernotif.S1 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data1$usernotif.S2 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data1$usernotif.S3 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data1$usernotif.S4 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data1$usernotif.S5 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data1$usernotif.S6 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data1$usernotif.S7 <- data1$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.intentionally.deceive.or.mislead.the.user.if.experiment.results.depend.on.it
usernotif1.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif1.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data1$expinv.S1 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.experiment.results.will.be.correct
data1$expinv.S2 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data1$expinv.S3 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data1$expinv.S4 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.experiments
data1$expinv.S5 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data1$expinv.S6 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data1$expinv.S7 <- data1$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv1.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv1.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif1 <- data.frame(Statement=factor(rep(usernotif1.statements, each=length(data1$usernotif.S1))),
                          Rating=c(
                            data1$usernotif.S1,
                            data1$usernotif.S2,
                            data1$usernotif.S3,
                            data1$usernotif.S4,
                            data1$usernotif.S5,
                            data1$usernotif.S6,
                            data1$usernotif.S7), 
                          Jobf = data1$role)
ggplot(data=undernotif1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Ericsson")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv1 <- data.frame(Statement=factor(rep(expinv1.statements, each=length(data1$expinv.S1))),
                      Rating=c(
                        data1$expinv.S1,
                        data1$expinv.S2,
                        data1$expinv.S3,
                        data1$expinv.S4,
                        data1$expinv.S5,
                        data1$expinv.S6,
                        data1$expinv.S7),
                      Jobf = data1$role)
ggplot(data=expinv1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))




### F-SECURE ###
data2 <- read.csv("raportti_fsecure.csv")
data2 <- data.frame(data2)

data2$jobfunction2 <- factor(data2$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                             levels = c(0:9),
                             labels = c("Developing software", "Product management or ownership", "Management, other", "Business development", "UX design", "Software architecture", "Providing consulting for customers", "Providing customer support", "Providing training services for customers", "Other"))
data2$jobfunction2.other <- data2$If.other..please.specify

# Recode job functions to roles, collapsing everything to four roles (Developer, UX designer, Manager, Other)
data2$role <- recode(data2$jobfunction2,
                     "c('Developing software','Software architecture')='Developer'; c('Product management or ownership','Management, other','Business development')='Manager'; c('UX design')='UX designer'; c('Providing consulting for customers','Providing customer support','Providing training services for customers','Other')='Other'")

attach(data2)
# Job function
print("Primary job function")
summary(jobfunction2)
ggplot(data2, aes(x=jobfunction2)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 1, 6), labels = c("0", "1", "6"))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data2$usernotif.S1 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data2$usernotif.S2 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data2$usernotif.S3 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data2$usernotif.S4 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data2$usernotif.S5 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data2$usernotif.S6 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data2$usernotif.S7 <- data2$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif2.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif2.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data2$expinv.S1 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data2$expinv.S2 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data2$expinv.S3 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data2$expinv.S4 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data2$expinv.S5 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data2$expinv.S6 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data2$expinv.S7 <- data2$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv2.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv2.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif2 <- data.frame(Statement=factor(rep(usernotif2.statements, each=length(data2$usernotif.S1))),
                          Rating=c(
                            data2$usernotif.S1,
                            data2$usernotif.S2,
                            data2$usernotif.S3,
                            data2$usernotif.S4,
                            data2$usernotif.S5,
                            data2$usernotif.S6,
                            data2$usernotif.S7)
                          , Jobf = data2$role)
ggplot(data=undernotif2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("F-secure")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv2 <- data.frame(Statement=factor(rep(expinv2.statements, each=length(data2$expinv.S1))),
                      Rating=c(
                        data2$expinv.S1,
                        data2$expinv.S2,
                        data2$expinv.S3,
                        data2$expinv.S4,
                        data2$expinv.S5,
                        data2$expinv.S6,
                        data2$expinv.S7),
                      Jobf = data2$role)
ggplot(data=expinv2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))





### VAADIN ###
data3 <- read.csv("raportti_vaadin.csv")
data3 <- data.frame(data3)
data3$jobfunction3 <- factor(data3$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                             levels = c(0:10),
                             labels = c("Developing the Vaadin Framework", "Developing the Vaadin Pro Tools", "Product management", "Management, other", "UX design", "Software architecture", "Advocating products and services" ,"Providing consulting", "Providing customer support", "Providing training services", "Other"))
#fixing: changed other to sales as both were sales anyways
data3$jobfunction3[11] <- "Management, other" #fixing: adding "account manager" to management, others 
data3$jobfunction3.other <- data3$If.other..please.specify
data3$jobfunction3.other[11] <- ""

# Recode job functions to roles, collapsing everything to four roles (Developer, UX designer, Manager, Other)
data3$role <- recode(data3$jobfunction3,
                     "c('Developing the Vaadin Framework','Developing the Vaadin Pro Tools', 'Software architecture')='Developer'; c('Product management','Management, other')='Manager'; c('UX design')='UX designer'; c('Advocating products and services','Providing consulting', 'Providing customer support', 'Providing training services', 'Other')='Other'")


attach(data3)
# Job function
print("Primary job function")
summary(jobfunction3)
ggplot(data3, aes(x=jobfunction3)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 1, 6), labels = c("0", "1", "6"))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data3$usernotif.S1 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data3$usernotif.S2 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data3$usernotif.S3 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data3$usernotif.S4 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data3$usernotif.S5 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data3$usernotif.S6 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data3$usernotif.S7 <- data3$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif3.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif3.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data3$expinv.S1 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data3$expinv.S2 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data3$expinv.S3 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Our.company.does.not.have.the.needed.technical.infrastructure
data3$expinv.S4 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data3$expinv.S5 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data3$expinv.S6 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data3$expinv.S7 <- data3$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.the.product.strategy
expinv3.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv3.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif3 <- data.frame(Statement=factor(rep(usernotif3.statements, each=length(data3$usernotif.S1))),
                          Rating=c(
                            data3$usernotif.S1,
                            data3$usernotif.S2,
                            data3$usernotif.S3,
                            data3$usernotif.S4,
                            data3$usernotif.S5,
                            data3$usernotif.S6,
                            data3$usernotif.S7)
                          ,Jobf = data3$role)
ggplot(data=undernotif3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Vaadin")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv3 <- data.frame(Statement=factor(rep(expinv3.statements, each=length(data3$expinv.S1))),
                      Rating=c(
                        data3$expinv.S1,
                        data3$expinv.S2,
                        data3$expinv.S3,
                        data3$expinv.S4,
                        data3$expinv.S5,
                        data3$expinv.S6,
                        data3$expinv.S7),
                      Jobf = data3$role)
ggplot(data=expinv3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))





### REAKTOR ###
data4 <- read.csv("raportti_reaktor.csv", encoding = "UTF-8")
data4 <- data.frame(data4)
data4$jobfunction4 <- factor(data4$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function..,
                             levels = c(0:6),
                             labels = c("Developing software", "Product management", "Business development", "UX design", "Graphic design", "Coaching", "Other"))
data4$jobfunction4.other <- data4$If.other..please.specify
data4$jobfunction4.other[45] <- "" #fixing: This person already marked UX design, but also put in other "UI design". no need for the second one
data4$jobfunction4.other[48] <- "" #fixing: UX, graphic design to UX design
data4$jobfunction4[48] <- "UX design"
data4$jobfunction4.other[4] <- "" #Changing data science to developing software 
data4$jobfunction4[4] <- "Developing software"

# Recode job functions to roles, collapsing everything to four roles (Developer, UX designer, Manager, Other)
data4$role <- recode(data4$jobfunction4,
                     "c('Developing software')='Developer'; c('Product management','Business development')='Manager'; c('UX design','Graphic design')='UX designer'; c('Coaching','Other')='Other'")

attach(data4)
print("Primary job function")
summary(jobfunction4)
ggplot(data4, aes(x=jobfunction4)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 12, 43), labels = c("0", "2", "3", "12", "43"))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
data4$usernotif.S1 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.do.not.need.to.know.they.are.involved
data4$usernotif.S2 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.we.collect.personal.information..users.need.to.be.notified
data4$usernotif.S3 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...If.no.laws.are.being.broken..users.do.not.need.to.be.notified
data4$usernotif.S4 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.can.be.involved.in.an.experiment.without.their.knowledge.if.we.let.them.know.afterwards
data4$usernotif.S5 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...Users.should.always.be.notified.when.they.are.being.involved.in.an.experiment
data4$usernotif.S6 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.not.to.disclose.all.the.experiment.details.to.users.involved
data4$usernotif.S7 <- data4$X4.1.How.much.do.you.agree.with.the.following.statements.regarding.notifying.users.about.experiments..Please.answer.according.to.your.personal.beliefs...It.is.ok.to.trick.the.user.if.the.validity.of.experiment.results.depend.on.it
usernotif4.statements <- c(
  "Users do not need to know they are involved",
  "If we collect personal information, users need to be notified",
  "If no laws are being broken, users do not need to be notified",
  "Users can be involved in an experiment without their knowledge if we let them know afterwards",
  "Users should always be notified when they are being involved in an experiment",
  "It is ok not to disclose all the experiment details to users involved",
  "It is ok to trick the user if the validty of experiment results depend on it")
usernotif4.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
data4$expinv.S1 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...I.cannot.trust.that.the.results.will.be.correct
data4$expinv.S2 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Involving.users.in.experiments.is.time.consuming
data4$expinv.S3 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...My.customer.does.not.have.the.needed.technical.infrastructure
data4$expinv.S4 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.would.not.like.to.be.part.of.software.experiments
data4$expinv.S5 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Users.have.to.be.convinced.of.the.benefit.before.taking.part
data4$expinv.S6 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.give.users.false.expectations
data4$expinv.S7 <- data4$X4.2.How.much.do.you.agree.with.the.following.statements.about.involving.users.in.experiments..Please.answer.according.to.your.personal.beliefs...Experiments.reveal.secrets.about.my.customer.s.product.strategy
expinv4.statements <- c(
  "I cannot trust that the experiment results will be correct",
  "Involving users in experiments is time-consuming",
  "My customer does not have the needed technical infrastructure",
  "Users would not like to be part of software experiments",
  "Users have to be convinced of the benefit before taking part",
  "Experiments give users false expectations",
  "Experiments reveal secrets about my customer's strategy")
expinv4.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif4 <- data.frame(Statement=factor(rep(usernotif4.statements, each=length(data4$usernotif.S1))),
                          Rating=c(
                            data4$usernotif.S1,
                            data4$usernotif.S2,
                            data4$usernotif.S3,
                            data4$usernotif.S4,
                            data4$usernotif.S5,
                            data4$usernotif.S6,
                            data4$usernotif.S7)
                          ,Jobf = data4$role)

ggplot(data=undernotif4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + ggtitle("Reaktor")

# 4.2 How much do you agree with the following statements about involving users in experiments? Please answer according to your personal beliefs.
expinv4 <- data.frame(Statement=factor(rep(expinv4.statements, each=length(data4$expinv.S1))),
                      Rating=c(
                        data4$expinv.S1,
                        data4$expinv.S2,
                        data4$expinv.S3,
                        data4$expinv.S4,
                        data4$expinv.S5,
                        data4$expinv.S6,
                        data4$expinv.S7),
                      Jobf = data4$role)
ggplot(data=expinv4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))




### ALL DATA ###
total_undernotif <- rbind(undernotif1, undernotif2, undernotif3, undernotif4)
total_expinv <- rbind(expinv1, expinv2, expinv3, expinv4)

xx <- rbind(undernotif1, undernotif2, undernotif3, undernotif4) 
yy  <- rbind(expinv1, expinv2, expinv3, expinv4)

#4.1
ggplot(data=total_undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text =element_text(size=12))  +  ggtitle("Total") + scale_x_discrete(limits=c("Users do not need to know they are involved", "If we collect personal information, users need to be notified", "If no laws are being broken, users do not need to be notified", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "Users should always be notified when they are being involved in an experiment", "It is ok not to disclose all the experiment details to users involved", "It is ok to trick the user if the validty of experiment results depend on it" )) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

#Total versus roles:
ggplot(xx, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement)) + ggtitle("Total") + guides(fill=FALSE) + coord_flip() + scale_size_continuous(range = c(0, 70)) + facet_wrap(~xx$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("It is ok to trick the user if the validty of experiment results depend on it", "It is ok not to disclose all the experiment details to users involved", "Users should always be notified when they are being involved in an experiment", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "If no laws are being broken, users do not need to be notified", "If we collect personal information, users need to be notified", "Users do not need to know they are involved"))
# , labeller = as_labeller(jb_names)

#4.2
ggplot(data=total_expinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text=element_text(size=11)) + ggtitle("Total") + scale_x_discrete(limits=c("I cannot trust that the experiment results will be correct", "Involving users in experiments is time-consuming", "My customer does not have the needed technical infrastructure", "Users would not like to be part of software experiments", "Users have to be convinced of the benefit before taking part", "Experiments give users false expectations", "Experiments reveal secrets about my customer's strategy"))  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
#Total versus roles:
ggplot(yy, aes(x=Statement,y=Rating, fill=Rating))+ geom_boxplot(aes(fill = Statement), fill="white", colour="black") + guides(fill=FALSE) + coord_flip() + ggtitle("Total") + scale_size_continuous(range = c(0, 70)) + facet_wrap(~yy$Jobf) +  labs(x = "", y = "") + scale_x_discrete(limits=c("Experiments reveal secrets about my customer's strategy", "Experiments give users false expectations", "Users have to be convinced of the benefit before taking part", "Users would not like to be part of software experiments", "My customer does not have the needed technical infrastructure", "Involving users in experiments is time-consuming", "I cannot trust that the experiment results will be correct"))



### CLUSTERING ###

# Question: How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
# Choices:
#  INVA: Users do not need to know they are involved
#  INVB: If we collect personal information, users need to be notified
#  INVC: If no laws are being broken, users do not need to be notified
#  INVD: Users can be involved in an experiment without their knowledge if we let them know afterwards
#  INVE: Users should always be notified when they are being involved in an experiment
#  INVF: It is ok not to disclose all the experiment details to users involved
#  INVG: It is ok to intentionally deceive or mislead the user if experiment results depend on it
#  INVH: I cannot trust that the experiment results will be correct
#  INVI: Involving users in experiments is time-consuming
#  INVJ: Our company does not have the needed technical infrastructure to run experiments
#  INVK: Users would not like to be part of experiments
#  INVL: Users have to be convinced of the benefit before taking part
#  INVM: Experiments give users false expectations
#  INVN: Experiments reveal secrets about the product strategy

## Merge data from all companies into a single data set
# First, make company-specific data frames that all have exactly the same columns
cols1 <- c(2:5, 7, 42:55, 58) # Ericsson
cols2 <- c(2:5, 6, 53:66, 81) # F-secure
cols3 <- c(2:5, 7, 56:69, 72) # Vaadin
cols4 <- c(2:5, 6, 50:63, 77) # Reaktor
clus_colnames  <- c("SUBTIME", "JOBFUNC", "JOBFUNCOTHER", "JOBTIME", "GENDER", "INVA", "INVB", "INVC", "INVD", "INVE", "INVF", "INVG", "INVH", "INVI", "INVJ", "INVK", "INVL", "INVM", "INVN", "ROLE")
clus_data1 <- data1[, cols1]
names(clus_data1) <- clus_colnames
clus_data2 <- data2[, cols2]
names(clus_data2) <- clus_colnames
clus_data3 <- data3[, cols3]
names(clus_data3) <- clus_colnames
clus_data4 <- data4[, cols4]
names(clus_data4) <- clus_colnames

# Second, concatenate the company-specific data frames into a single data frame
clus_data <- rbind(clus_data1, clus_data2, clus_data3, clus_data4)

# Also create a subset without demographic data
clus_data_sub <- clus_data[,c(6:19)]
# and a scaled version of that with NA's removed
clus_data_scaled <- scale(na.omit(clus_data_sub))

## Correlation matrix

cor_matrix <- cor(clus_data_sub, use = "pairwise.complete.obs")
print(cor_matrix %>% round(2))
corrplot.mixed(cor_matrix, lower = "number", upper = "circle")

## Principal components analysis

pc <- princomp(cor_matrix, cor=TRUE)
summary(pc)
loadings(pc)
plot(pc, type="lines") # indicates 3 main components
print(pc$scores)
biplot(pc)

# View PCA in 3d
plot3d(pc$scores[,1:3])
text3d(pc$scores, texts = rownames(clus_data_sub))
text3d(pc$loadings[,1:3], texts = rownames(pc$loadings), col = "red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0), pc$loadings[i,1:3]))
}
lines3d(coords, col = "red", lwd = 4)

## Factor analysis

# How many factors?
ev <- eigen(cor_matrix)
ap <- parallel(subject = nrow(na.omit(clus_data_sub)), var = ncol(na.omit(clus_data_sub)), rep = 100, cent = .05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS) # indicates 2 factors

# Do factor analysis with 2 factors
fa <- factanal(na.omit(clus_data_sub), 2, rotation="varimax")
print(fa, digits=2, cutoff=.3, sort=TRUE)
plot(fa$loadings[,1:2], type="n")
text(fa$loadings, labels=names(clus_data_sub), cex=.7)

# residual matrix
round(cor_matrix - (fa$loadings %*% t(fa$loadings) + diag(fa$uniquenesses)), 3)

# Factor analysis of only ethics statements
clus_ethics <- clus_data_sub[,c(1:7)]
cor_matrix_ethics <- cor(clus_ethics, use = "pairwise.complete.obs")
# How many factors?
ev <- eigen(cor_matrix_ethics)
ap <- parallel(subject = nrow(na.omit(clus_ethics)), var = ncol(na.omit(clus_ethics)), rep = 100, cent = .05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS) # indicates 1 factor
# Do factor analysis with 2 factors anyway :)
fa <- factanal(na.omit(clus_ethics), 2, rotation="varimax")
print(fa, digits=2, cutoff=.3, sort=TRUE)
plot(fa$loadings[,1:2], type="n")
text(fa$loadings, labels=names(clus_ethics), cex=.7)

## K-means clustering

# How many clusters?
wss <- (nrow(clus_data_scaled) - 1) * sum(apply(clus_data_scaled, 2, var))
for (i in 2:14) {
  wss[i] <- sum(kmeans(clus_data_scaled, centers=i)$withinss)
}
plot(1:14, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") # 3 clusters; also supported by PCA scree plot

cl <- kmeans(clus_data_scaled, 3)
aggregate(clus_data_scaled, by=list(cl$cluster), FUN=mean)
clusplot(clus_data_scaled, cl$cluster, color = TRUE, shade = FALSE, labels=2, lines=0)

# Verify the number of clusters using a silhouette plot
dissE <- daisy(clus_data_scaled)
dE2 <- dissE^2
sk2 <- silhouette(cl$cluster, dE2)
plot(sk2)

## Hierarchical agglomerative clustering

# Hierarchical clustering of observations
d <- dist(clus_data_scaled, method="euclidean")
hcl <- hclust(d, method="ward.D2")
plot(hcl, cex=.5)
groups <- cutree(hcl, k=3)
rect.hclust(hcl, k=3, border="red")
# See the roles distribution of the groups
sapply(unique(groups),function(g)clus_data$ROLE[groups== g])

#heatmap
dev.off()
library(gplots)
# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(groups)))
# create vector of colors for side bar
myClusterSideBar <- clusterCols[groups]
# choose a color palette for the heat map
myheatcol <- rev(redgreen(75))
# draw the heat map
heatmap.2(clus_data_scaled, main="Hierarchical Cluster", Rowv=as.dendrogram(hcl), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)


# Multiscale bootstrap sampling method, clustering variables rather than observations
pcl <- pvclust(clus_data_scaled, method.hclust="ward.D2", method.dist="euclidian")
plot(pcl)
pvrect(pcl, alpha=.85, border=2)
pvrect(pcl, alpha=.6, border=4)


###################################################
# Below: Adding company identifiers as a second approach, just to get the correlation matrix. Original dataset and clusters are above.
###################################################

clus_colnames2  <- c("SUBTIME", "JOBFUNC", "JOBFUNCOTHER", "JOBTIME", "GENDER", "INVA", "INVB", "INVC", "INVD", "INVE", "INVF", "INVG", "INVH", "INVI", "INVJ", "INVK", "INVL", "INVM", "INVN", "ROLE", "Eri", "Fsc", "Vaa", "Rea")
clus_data11 <- data1[, cols1]
clus_data11[,21] <- 1 #adding indicators for companies.. this might be the dumbest way
clus_data11[,22] <- 0
clus_data11[,23] <- 0
clus_data11[,24] <- 0
names(clus_data11) <- clus_colnames2

clus_data22 <- data2[, cols2]
clus_data22[,21] <- 0
clus_data22[,22] <- 1
clus_data22[,23] <- 0
clus_data22[,24] <- 0
names(clus_data22) <- clus_colnames2

clus_data33 <- data3[, cols3]
clus_data33[,21] <- 0
clus_data33[,22] <- 0
clus_data33[,23] <- 1
clus_data33[,24] <- 0
names(clus_data33) <- clus_colnames2

clus_data44 <- data4[, cols4]
clus_data44[,21] <- 0
clus_data44[,22] <- 0
clus_data44[,23] <- 0
clus_data44[,24] <- 1
names(clus_data44) <- clus_colnames2

# Second, concatenate the company-specific data frames into a single data frame
clus_data2 <- rbind(clus_data11, clus_data22, clus_data33, clus_data44)
# Also create a subset without demographic data
yeah <- c(6:19, 21:24)
clus_data_sub2 <- clus_data2[, yeah]
# and a scaled version of that with NA's removed
clus_data_scaled2 <- scale(na.omit(clus_data_sub2))

## Correlation matrix with company identifiers
cor_matrix2 <- cor(clus_data_sub2, use ="pairwise.complete.obs")
print(cor_matrix2 %>% round(2)) 
corrplot.mixed(cor_matrix2, lower = "number", upper = "circle")

# Hierarchical clustering of observations with company identifiers
d2 <- dist(clus_data_scaled2, method="euclidean")
hcl2 <- hclust(d2, method="ward.D2")
plot(hcl2, cex=.5)
groups2 <- cutree(hcl2, k=3)
# to look at the clusters a bit, e.g.: 
table(groups2)
summary(clus_data2$ROLE[groups2 == 1])
rect.hclust(hcl2, k=3, border="red")
# shows each clusters' roles
sapply(unique(groups2),function(g)clus_data2$ROLE[groups2== g])


#heatmap with company identifiers
library(gplots)
# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(groups2)))
# create vector of colors for side bar
myClusterSideBar <- clusterCols[groups2]
# choose a color palette for the heat map
myheatcol <- rev(redgreen(75))
# draw the heat map
heatmap.2(clus_data_scaled2, main="Hierarchical Cluster", Rowv=as.dendrogram(hcl2), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)