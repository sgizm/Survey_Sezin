######################################################################
# Analysis script for User Involvement Survey - Whole
# It works with Whote_data.R, has to be in the same directory

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
library(plyr)
#library(dplyr)

#POPULATION.SIZE = 397
CURRENTYEAR <- 1900 + as.POSIXlt(Sys.Date())$year
set.seed(2018)
##########################################################################
# Read raw csv files of Ericsson, Fsecure, Vaadin and Reaktor respectively
##########################################################################

#🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩

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
data1$role <- car::recode(data1$jobfunction1,
                     "c('Developing software','Testing software','System or network operations','Software architecture')='Developer'; c('UX design')='UX designer'; c('Management')='Manager'")
data1$worktime <- data1$X1.2.How.long.have.you.been.working.in.your.current.company.role.
data1$worktime[5] <- NA #fixing
#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data1$birthyear <- data1$X1.3.What.is.your.year.of.birth.
data1$birthyear[31] <- data1$birthyear[31] + 1900 # Fix data1 entry error
data1$age <- CURRENTYEAR - data1$birthyear
data1$age_range <- car::recode(data1$age, "1:20='20 or less'; 21:30='21-30'; 31:40='31-40'; 41:50='41-50'; 50:9999='50 or more'")


data1$gender <- factor(data1$X1.4.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

data1$teamsize <- factor(data1$X1.5.What.is.the.size.of.your.primary.work.team..,
                        levels = c(0:4,""),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20", "No team"))


# 2.1 In whch development activities are users involved in your company? (click all that apply)
data1$useractivities.specifying.requirements <- data1$Specifying.requirements
data1$useractivities.designing.software <- data1$Designing.software
data1$useractivities.implementing.software <- data1$Implementing.software
data1$useractivities.testing <- data1$Testing
data1$useractivities.after.release <- data1$The.activities.after.release
data1$useractivities.other <- data1$Other
data1$useractivities.other.open <- data1$If.other..please.specify..separate.with.commas.
useractivities1.options <- c("Specifying requirements", "Software design", "Implementation", "Testing", "The activities after release", "Other")

# 2.2. How much do you agree with the following statements? (User involvement statements)
data1$userinv.S1 <- data1$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data1$userinv.S2 <- data1$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data1$userinv.S3 <- data1$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data1$userinv.S4 <- data1$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data1$userinv.S5 <- data1$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work
data1$userinv.S6 <- data1$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
userinv1.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users' needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)
userinv1.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
# Note that statements are merged into 4 categories. 
data1$userinf.mgr <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Managers
data1$userinf.uxd <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...UX.designers
data1$userinf.dev <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.developers.
data1$userinf.tst <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.testers
data1$userinf.arc <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Software.architects.
data1$userinf.ops <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...System.or.network.operators
data1$userinf.slf <- data1$X2.3.In.your.experience..how.easy.is.it.for.the.following.to.get.information.from.users...Myself
userinf1.statements <- c("Managers", "UX designers", "Developers", "Myself")
userinf1.options <- c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy", "I don't know")

# 2.4 How often do you use the following ways to get information about users?
data1$infofreq.O1 <- data1$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.remotely.observe.users.when.they.are.using.the.software..e.g...screen.sharing.
data1$infofreq.O2 <- data1$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.am.physically.present.with.users.when.they.are.using.the.software..e.g...talk.aloud.study.
data1$infofreq.O3 <- data1$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.in.person.after.they.used.the.software..e.g...post.use.interview.
data1$infofreq.O4 <- data1$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.or.video.
infofreq1.statements <- c(
  "I remotely observe or interact with users when they are using the software (e.g., screen sharing)",
  "I interact with the users before they use the software",
  "I interact with users after they used the software (e.g., post-use interview, feedback)",
  "Through recorded usage data (e.g., log data)")
infofreq1.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
data1$userinf.open <- data1$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
data1$condexp <- factor(data1$X3.1.Does.your.company.conduct.experiments.involving.the.users..,
                       levels = c(1:4, "NA"),
                       labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data1$exp.open <- data1$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data1 for understanding user needs. How much do you agree with each statement?
data1$understanding.S1 <- data1$For.understanding.user.needs.better.........data.should.always.be.collected.because.it.might.be.needed.later
data1$understanding.S2 <- data1$For.understanding.user.needs.better.........data.should.only.be.collected.when.there.is.a.known.need.or.assumption.to.test
data1$understanding.S3 <- data1$For.understanding.user.needs.better.........all.data.about.user.behaviour.is.useful
data1$understanding.S4 <- data1$For.understanding.user.needs.better.........focused.data.on.a.specifically.chosen.user.action.is.useful
data1$understanding.S5 <- data1$For.understanding.user.needs.better.........users.themselves.must.be.actively.involved.in.development
data1$understanding.S6 <- data1$For.understanding.user.needs.better.........we.just.need.to.measure.user.behaviour
understanding1.statements <- c(
  "..data should always be collected because it might be needed later",
  "..data should only be collected when there is a known need or assumption",
  "..rich, detailed data about what users do is useful",
  "..focused data on a specific user action or behaviour is useful",
  "..users themselves must be actively involved in shaping the software",
  "..we need to measure user behaviour to decide what the software should be like"
)
understanding1.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

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


########################

attach(data1)
# Job function
print("Primary job function")
summary(jobfunction1)
ggplot(data1, aes(x=role)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Ericsson roles", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 10, 20), labels = c("0", "2", "3", "10", "20"))
data1$jobfunction1.other

# Work time
print("How long have you been working in your current role")
summary(worktime)
ggplot(data1, aes(x=data1$worktime)) +
  geom_histogram(binwidth=12, fill="#FF9999", colour="#FF9999") +
  labs(x="Ericsson work time", y="Frequency") + theme(axis.text=element_text(size=13)) +
  scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,11), labels = c("0", "1", "2","3", "4", "5", "6","7", "8","11"))

ggplot(data1, aes(x=worktime)) + 
  geom_histogram(aes(y=..density..),# Histogram with density instead of count on y-axis
                 binwidth=10,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF9999", colour="#FF9999")  # Overlay with transparent density plot

# Birth year
print("Year of birth")
summary(birthyear)
ggplot(data1, aes(x=birthyear)) +
  geom_histogram(binwidth=1, fill="#FF9999", colour="#FF9999") +
  labs(x="Year of birth", y="Frequency")

# Age
print("Age")
summary(age)
ggplot(data1, aes(x=age)) +
  geom_density(fill="#FF9999", colour="#FF9999") +
  labs(x="Age", y="Density")

ggplot(data1, aes(x=age)) + 
  geom_histogram(aes(y=..density..),# Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF9999", colour="#FF9999")  # Overlay with transparent density plot

ggplot(data1, aes(x=age_range)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Ericsson age range", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# Gender
print("Gender")
#summary(gender)
ggplot(data1, aes(x=gender)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Ericsson gender", y="Frequency")

# Team size
print("Team size")
summary(teamsize)
ggplot(data1, aes(x=teamsize)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Ericsson team size", y="Frequency")

#🚩🚩🚩🚩🚩🚩���
## Section 2 - Ericsson

# 2.1 In which development activities are users involved in your company? (click all that apply)
useractivities1.specifying.requirements.count <- sum(data1$useractivities.specifying.requirements, na.rm=TRUE)
useractivities1.designing.software.count <- sum(data1$useractivities.designing.software, na.rm=TRUE)
useractivities1.implementing.software <- sum(data1$useractivities.implementing.software, na.rm=TRUE)
useractivities1.testing <- sum(data1$useractivities.testing, na.rm=TRUE)
useractivities1.after.release <- sum(data1$useractivities.after.release, na.rm=TRUE)
useractivities1.other <- sum(data1$useractivities.other, na.rm=TRUE)
useractivities1 <- data.frame(Activity=useractivities1.options,
                             Frequency=c(
                               useractivities1.specifying.requirements.count,
                               useractivities1.designing.software.count,
                               useractivities1.implementing.software,
                               useractivities1.testing,
                               useractivities1.after.release,
                               useractivities1.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities1)
ggplot(useractivities1, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="#FF9999", colour="#FF9999") +
  labs(x="Ericsson user activities", y="Frequency")

# 2.2 How much do you agree with the following statements?
userinv1 <- data.frame(Statement=factor(rep(userinv1.statements, each=length(data1$userinv.S1))),
                      Rating=c(
                        data1$userinv.S1,
                        data1$userinv.S2,
                        data1$userinv.S3,
                        data1$userinv.S4,
                        data1$userinv.S5,
                        data1$userinv.S6),
                      Jobf = data1$role,
                      Comp = "Ericsson"
)

ggplot(userinv1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())
#+
  #facet_wrap(~userinv1$Jobf, ncol = 3)

# Removing other category from ROLES:
ggplot(subset(userinv1, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  facet_wrap(~Jobf) +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf1 <- data.frame(Statement=factor(rep(userinf1.statements, each=length(data1$userinf.mgr))),
                      Rating=c(
                        data1$userinf.mgr,
                        data1$userinf.uxd,
                        data1$userinf.dev,
                        data1$userinf.slf),
                      Jobf = data1$role,
                      Comp = "Ericsson")
ggplot(userinf1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("Myself","UX designers", "Managers", "Developers")) +
  theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# 2.4 How often do you use the following ways to get information about users?
infofreq1 <- data.frame(Statement=factor(rep(infofreq1.statements, each=length(data1$infofreq.O1))),
                       Rating=c(
                         data1$infofreq.O1,
                         data1$infofreq.O2,
                         data1$infofreq.O3,
                         data1$infofreq.O4),
                       Jobf = data1$role,
                       Comp = 'Ericsson')
ggplot(infofreq1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip()

# 3.1 Does your company conduct experiments involving the users?
ggplot(data1, aes(x=condexp)) +
  geom_bar(fill="#FF9999", colour="#FF9999") +
  labs(x="Conducting experiments", y="Frequency")

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
understanding1 <- data.frame(Statement=factor(rep(understanding1.statements, each=length(data1$understanding.S1))),
                            Rating=c(
                              data1$understanding.S1,
                              data1$understanding.S2,
                              data1$understanding.S3,
                              data1$understanding.S4,
                              data1$understanding.S5,
                              data1$understanding.S6),
                            Jobf = data1$role,
                            Comp = 'Ericsson')

ggplot(understanding1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank()
        ) +
  facet_wrap(~understanding1$Jobf, ncol = 3)

# Removing other category
ggplot(subset(understanding1, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  facet_wrap(~Jobf) +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

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
                          Jobf = data1$role,
                          Comp = 'Ericsson')
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
                      Jobf = data1$role,
                      Comp = 'Ericsson')
ggplot(data=expinv1, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

#🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩

### F-SECURE ###

######################################################################
# Preprocess
######################################################################

data2 <- read.csv("raportti_fsecure.csv")
data2 <- data.frame(data2)

data2$jobfunction2 <- factor(data2$X1.1.Which.of.the.following.most.closely.matches.your.primary.job.function....,
                             levels = c(0:9),
                             labels = c("Developing software", "Product management or ownership", "Management, other", "Business development", "UX design", "Software architecture", "Providing consulting for customers", "Providing customer support", "Providing training services for customers", "Other"))
data2$jobfunction2.other <- data2$If.other..please.specify

# Recode job functions to roles, collapsing everything to four roles (Developer, UX designer, Manager, Other)
data2$role <- car::recode(data2$jobfunction2,
                     "c('Developing software','Software architecture')='Developer'; c('Product management or ownership','Management, other','Business development')='Manager'; c('UX design')='UX designer'; c('Providing consulting for customers','Providing customer support','Providing training services for customers','Other')='Other'")

data2$worktime <- data2$X1.2.How.long.have.you.been.working.in.your.current.company.role.
#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data2$gender <- factor(data2$X1.3.Which.of.the.following.best.describes.you....,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))


data2$age_range <- factor(data2$X1.4.What.is.your.age.range....,
                         levels = c(0:4),
                         labels = c("20 or less", "21-30", "31-40", "41-50", "50 or more"))

data2$teamsize <- factor(data2$X1.5.How.many.people.do.you.work.with.on.a.regular.basis.in.the.company....,
                        levels = c(0:4),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20"))

data2$end_user <- factor(data2$X1.6.Who.do.you.consider.to.be.the.primary.user.in.your.job.function....,
                        levels = c(0:1),
                        labels = c("Business user within B2B domain", "End user using F-secure product and services"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
data2$useractivities.forming.ideas <- data2$Forming.product.or.service.ideas
data2$useractivities.gathering.requirements <- data2$Gathering.requirements
data2$useractivities.software.design <- data2$Software.design
data2$useractivities.implementation <- data2$Implementation
data2$useractivities.testing <- data2$Testing
data2$useractivities.after.release <- data2$The.activities.after.release
data2$useractivities.customer.support <- data2$Providing.customer.support
data2$useractivities.consulting <- data2$Providing.consulting
data2$useractivities.billing <- data2$Billing.services
data2$useractivities.other <- data2$Other
data2$useractivities.other.open <- data2$If.other..please.specify..separate.with.commas.
useractivities2.options <- c("Specifiying requirements", "Software design", "Implementation", "Testing", "The activities after release", "Other")

# 2.2. How much do you agree with the following statements? (User involvement statements)
data2$userinv.S1 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data2$userinv.S2 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data2$userinv.S3 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data2$userinv.S4 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data2$userinv.S5 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work.
data2$userinv.S6 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
data2$userinv.S7 <- data2$X2.2..How.much.do.you.agree.with.the.following.statements...I.would.like.to.get.more.feedback.from.users

userinv2.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users' needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)
userinv2.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
data2$userinf.dev <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Developers
data2$userinf.mgr <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Product.managers.or.owners
data2$userinf.mgr.other <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Managers..other
data2$userinf.bus.dev <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Business.developers
data2$userinf.ux.designer <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...UX.designers
data2$userinf.arc <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Software.architects.
data2$userinf.con <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Consultants
data2$userinf.cust.sup <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Customer.support
data2$userinf.tra <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Trainers
data2$userinf.slf <- data2$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Myself
userinf2.statements <- c("Managers", "UX designers", "Developers", "Myself")
userinf2.options <- c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy", "I don't know")

# 2.4 How often do you use the following ways to get information about users?
data2$infofreq.O1 <- data2$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.remotely.observe.users.when.they.are.using.the.software..e.g...screen.sharing.
data2$infofreq.O2 <- data2$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.am.physically.present.with.users.when.they.are.using.the.software..e.g...talk.aloud.study.
data2$infofreq.O3 <- data2$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.the.users.before.they.use.the.software
data2$infofreq.O4 <- data2$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.in.person.after.they.used.the.software..e.g...post.use.interview.
data2$infofreq.O5 <- data2$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.
data2$infofreq.O6 <- data2$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.online.discussion.channels..e.g..forums.

infofreq2.statements <- c(
  "I remotely observe or interact with users when they are using the software (e.g., screen sharing)",
  "I interact with the users before they use the software",
  "I interact with users after they used the software (e.g., post-use interview, feedback)",
  "Through recorded usage data (e.g., log data)")
infofreq2.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
data2$userinf.open <- data2$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
data2$condexp <- factor(data2$X3.1.Does.your.company.conduct.experiments.involving.the.users....,
                       levels = c(1:4, "NA"),
                       labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data2$exp.open <- data2$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
data2$understanding.S1 <- data2$To.understand.users..needs.better.......data.should.always.be.collected.because.it.might.be.needed.later
data2$understanding.S2 <- data2$To.understand.users..needs.better.......data.should.only.be.collected.when.there.is.a.known.need.or.assumption
data2$understanding.S3 <- data2$To.understand.users..needs.better.......rich..detailed.data.about.what.users.do.is.useful
data2$understanding.S4 <- data2$To.understand.users..needs.better.......focused.data.on.a.specific.user.action.or.behaviour.is.useful
data2$understanding.S5 <- data2$To.understand.users..needs.better.......users.themselves.must.be.actively.involved.in.shaping.the.software
data2$understanding.S6 <- data2$To.understand.users..needs.better.......we.need.to.measure.user.behaviour.to.decide.what.the.software.should.be.like
understanding2.statements <- c(
  "..data should always be collected because it might be needed later",
  "..data should only be collected when there is a known need or assumption",
  "..rich, detailed data about what users do is useful",
  "..focused data on a specific user action or behaviour is useful",
  "..users themselves must be actively involved in shaping the software",
  "..we need to measure user behaviour to decide what the software should be like"
)
understanding2.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

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

attach(data2)
#🚩🚩🚩🚩🚩🚩���
## Section 2 Fsecure 

# Job function
print("Primary job function")
summary(jobfunction2)
ggplot(data2, aes(x=role)) +
  geom_bar(fill="#FF9999", colour="white") + 
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 1, 6), labels = c("0", "1", "6"))

# Work time
print("How long have you been working in your current role")
summary(data2$worktime)
ggplot(data2, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + scale_x_continuous(breaks=c(3,12,24,36,84), labels=c("<3m", "1y","2y", "3y","7y")) + scale_y_continuous(breaks=c(0,1,2), labels = c("0", "1", "2")) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())


# Gender
print("Gender")
summary(data2$gender)
ggplot(data2, aes(x=gender)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Gender", y="Frequency")

# Age range
print("Age range")
summary(data2$age_range)
ggplot(data2, aes(x=age_range)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Age range", y="Frequency") +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Team size
print("Team size")
summary(data2$teamsize)
ggplot(data2, aes(x=teamsize)) +
  geom_bar(fill="lightgoldenrod2", colour="white") +
  labs(x="Team size", y="Frequency")

# End user
print("End user")
summary(data2$end_user)
ggplot(data2, aes(x=end_user)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="End user", y="Frequency")

# 2.1 In which development activities are users involved in your company? (click all that apply)
# Note not using some activities 
#useractivities2.forming.ideas <- sum(data2$useractivities.forming.ideas, na.rm=TRUE)
useractivities2.gathering.requirements <- sum(data2$Gathering.requirements, na.rm = TRUE)
useractivities2.software.design <- sum(data2$useractivities.software.design, na.rm=TRUE)
useractivities2.implementation <- sum(data2$useractivities.implementation, na.rm=TRUE)
useractivities2.testing <- sum(data2$useractivities.testing, na.rm=TRUE)
useractivities2.after.release <- sum(data2$useractivities.after.release, na.rm=TRUE)
useractivities2.customer.support <- sum(data2$useractivities.customer.support, na.rm=TRUE)
useractivities2.consulting<- sum(data2$useractivities.consulting, na.rm=TRUE)
useractivities2.billing <- sum(data2$useractivities.billing, na.rm=TRUE)
useractivities2.other <- sum(data2$useractivities.other, na.rm=TRUE)
useractivities2 <- data.frame(Activity=useractivities2.options,
                             Frequency=c(
                               useractivities2.gathering.requirements,
                               useractivities2.software.design,
                               useractivities2.implementation,
                               useractivities2.testing,
                               useractivities2.after.release,
                               useractivities2.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities2)
ggplot(useractivities2, aes(x=Activity, y=Frequency)) + labs(x="Development activities where users are involved") +
  geom_bar(stat="identity", fill="lightpink2", colour="black") + theme(axis.text=element_text(size=10))

# 2.2 How much do you agree with the following statements?
userinv2 <- data.frame(Statement=factor(rep(userinv2.statements, each=length(data2$userinv.S1))),
                      Rating=c(
                        data2$userinv.S1,
                        data2$userinv.S2,
                        data2$userinv.S3,
                        data2$userinv.S4,
                        data2$userinv.S5,
                        data2$userinv.S6),
                      Jobf = data2$role,
                      Comp = "F-secure"
)

ggplot(userinv2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=16)) +
  facet_wrap(~userinv2$Jobf)
# + scale_y_discrete(name="Ra$Jobf, limits=c("1","2", "3","4","5"))

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf2 <- data.frame(Statement=factor(rep(userinf2.statements, each=length(data2$userinf.mgr))),
                      Rating=c(
                        data2$userinf.mgr,
                        data2$userinf.ux.designer,
                        data2$userinf.dev,
                        data2$userinf.slf),
                      Jobf = data2$role,
                      Comp = "F-secure")
ggplot(userinf2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() 

# 2.4 How often do you use the following ways to get information about users?
infofreq2 <- data.frame(Statement=factor(rep(infofreq2.statements, each=length(data2$infofreq.O1))),
                       Rating=c(
                         data2$infofreq.O1,
                         data2$infofreq.O3,
                         data2$infofreq.O4,
                         data2$infofreq.O5),
                       Jobf = data2$role,
                       Comp = 'F-secure')
ggplot(infofreq2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=16))

# 3.1 Does your company conduct experiments involving the users?
ggplot(data2, aes(x=condexp)) +
  geom_bar(fill="burlywood1", colour="white") +
  labs(x="Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=16))

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
understanding2 <- data.frame(Statement=factor(rep(understanding2.statements, each=length(data2$understanding.S1))),
                            Rating=c(
                              data2$understanding.S1,
                              data2$understanding.S2,                              
                              data2$understanding.S3,
                              data2$understanding.S4,
                              data2$understanding.S5,
                              data2$understanding.S6),
                            Jobf = data2$role,
                            Comp = 'F-secure')
ggplot(data=understanding2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("..we need to measure user behaviour to decide what the software should be like","..users themselves must be actively involved in shaping the software", "..focused data on a specific user action or behaviour is useful", "..rich, detailed data about what users do is useful", "..data should only be collected when there is a known need or assumption", "..data should always be collected because it might be needed later" )) + theme(axis.text=element_text(size=11))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif2 <- data.frame(Statement=factor(rep(usernotif2.statements, each=length(data2$usernotif.S1))),
                          Rating=c(
                            data2$usernotif.S1,
                            data2$usernotif.S2,
                            data2$usernotif.S3,
                            data2$usernotif.S4,
                            data2$usernotif.S5,
                            data2$usernotif.S6,
                            data2$usernotif.S7),
                          Jobf = data2$role,
                          Comp = 'F-secure')
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
                      Jobf = data2$role,
                      Comp = 'F-secure')
ggplot(data=expinv2, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

#🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩

# VAADIN
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
data3$role <- car::recode(data3$jobfunction3,
                     "c('Developing the Vaadin Framework','Developing the Vaadin Pro Tools', 'Software architecture')='Developer'; c('Product management','Management, other')='Manager'; c('UX design')='UX designer'; c('Advocating products and services','Providing consulting', 'Providing customer support', 'Providing training services', 'Other')='Other'")
# Worktime
data3$worktime <- data3$X1.2.How.long.have.you.been.working.in.your.current.company.role.
data3$worktime[7] <- NA #fixing
#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data3$gender <- factor(data3$X1.4.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

# Age
data3$birthyear <- data3$X1.3.What.is.your.year.of.birth.
data3$birthyear[5] <- data3$birthyear[5] + 1900 # Fix data entry error
data3$age <- CURRENTYEAR - data3$birthyear
data3$age_range <- car::recode(data3$age, "1:20='20 or less'; 21:30='21-30'; 31:40='31-40'; 41:50='41-50'; 50:9999='50 or more'")


#Team size
data3$teamsize <- factor(data3$X1.5.How.many.people.do.you.work.with.on.a.regular.basis.in.the.company....,
                        levels = c(0:4),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20"))
# Work location
data3$location <- factor(data3$X1.6.Where.is.your.primary.work.location....,
                        levels = c(0:2),
                        labels = c("Finland", "Germany", "USA"))

# End user
data3$end_user <- factor(data3$X1.7.Who.do.you.consider.as.your.primary.user.in.your.job.function....,
                        levels = c(0:1),
                        labels = c("Software developer using Vaadin products", "End user using applications developed with Vaadin tools or frameworks"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
data3$useractivities.forming.ideas <- data3$Forming.product.or.service.ideas
data3$useractivities.gathering.requirements <- data3$Gathering.requirements
data3$useractivities.software.design <- data3$Software.design
data3$useractivities.implementation <- data3$Implementation
data3$useractivities.testing <- data3$Testing
data3$useractivities.after.release <- data3$The.activities.after.release
data3$useractivities.commiting.code <- data3$Committing.code
data3$useractivities.fixes <- data3$Providing.fixes
data3$useractivities.submitting.bugs <- data3$Submitting.bugs
data3$useractivities.online.discussion <- data3$Participating.in.online.discussion
data3$useractivities.other <- data3$Other
useractivities3.options <- c("Specifiying requirements", "Software design", "Implementation", "Testing", "The activities after release", "Other")

#Other activities
data3$useractivities.other.open <- data3$If.other..please.specify.1

# 2.2. How much do you agree with the following statements? (User involvement statements)
data3$userinv.S1 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data3$userinv.S2 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data3$userinv.S3 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data3$userinv.S4 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data3$userinv.S5 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work.
data3$userinv.S6 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
data3$userinv.S7 <- data3$X2.2..How.much.do.you.agree.with.the.following.statements...I.would.like.to.get.more.feedback.from.users

userinv3.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users' needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)
userinv3.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
data3$userinf.pro.mng <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Product.managers
data3$userinf.mng <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Managers..other
data3$userinf.ux.designer <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...UX.designers
data3$userinf.dev.frame <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Vaadin.Framework.developers
data3$userinf.dev.tool <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Vaadin.Pro.Tools.developers
data3$userinf.arc <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Software.architects.
data3$userinf.adv <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Product.and.service.advocates
data3$userinf.con <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Consultants
data3$userinf.cust.sup <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Customer.support
data3$userinf.tra <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Trainers
data3$userinf.slf <- data3$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users...Myself
userinf3.statements <- c("Managers", "UX designers","Developers", "Myself")
userinf3.options <- c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy", "I don't know")

# 2.4 How often do you use the following ways to get information about users?
data3$infofreq.O1 <- data3$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.remotely.observe.users.when.they.are.using.the.software..e.g...screen.sharing.
data3$infofreq.O2 <- data3$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.am.physically.present.with.users.when.they.are.using.the.software..e.g...talk.aloud.study.
data3$infofreq.O3 <- data3$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.the.users.before.they.use.the.software
data3$infofreq.O4 <- data3$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.in.person.after.they.used.the.software..e.g...post.use.interview.
data3$infofreq.O5 <- data3$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.
data3$infofreq.O6 <- data3$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.online.discussion.channels..e.g..forums.

infofreq3.statements <- c(
  "I remotely observe or interact with users when they are using the software (e.g., screen sharing)",
  "I interact with the users before they use the software",
  "I interact with users after they used the software (e.g., post-use interview, feedback)",
  "Through recorded usage data (e.g., log data)")
infofreq3.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
data3$userinf.open <- data3$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
data3$condexp <- factor(data3$X3.1.Does.your.company.conduct.experiments.involving.the.users.,
                       levels = c(1:4, "NA"),
                       labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data3$exp.open <- data3$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
data3$understanding.S1 <- data3$To.understand.users..needs.better.......data.should.always.be.collected.because.it.might.be.needed.later
data3$understanding.S2 <- data3$To.understand.users..needs.better.......data.should.only.be.collected.when.there.is.a.known.need.or.assumption
data3$understanding.S3 <- data3$To.understand.users..needs.better.......rich..detailed.data.about.what.users.do.is.useful
data3$understanding.S4 <- data3$To.understand.users..needs.better.......focused.data.on.a.specific.user.action.or.behaviour.is.useful
data3$understanding.S5 <- data3$To.understand.users..needs.better.......users.themselves.must.be.actively.involved.in.shaping.the.software
data3$understanding.S6 <- data3$To.understand.users..needs.better.......we.need.to.measure.user.behaviour.to.decide.what.the.software.should.be.like
understanding3.statements <- c(
  "..data should always be collected because it might be needed later",
  "..data should only be collected when there is a known need or assumption",
  "..rich, detailed data about what users do is useful",
  "..focused data on a specific user action or behaviour is useful",
  "..users themselves must be actively involved in shaping the software",
  "..we need to measure user behaviour to decide what the software should be like"
)
understanding3.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

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

#🚩🚩🚩🚩🚩🚩🚩���
# Section 2 Vaadin

attach(data3)
# Job function
print("Primary job function")
summary(jobfunction3)
ggplot(data3, aes(x=role)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 1, 6), labels = c("0", "1", "6"))

data3$jobfunction3.other

# Gender
print("Gender")
summary(data3$gender)
ggplot(data3, aes(x=gender)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=15)) + scale_y_continuous(breaks=c(0, 21), labels = c("0", "21"))

# Age
print("Age")
summary(data3$age)
ggplot(data3, aes(x=age)) +
  geom_density(fill="#FF9999", colour="#FF9999") +
  labs(x="Age", y="Density")

ggplot(data3, aes(x=age)) + 
  geom_histogram(aes(y=..density..),# Histogram with density instead of count on y-axis
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF9999", colour="#FF9999")  # Overlay with transparent density plot

print("Age range")
summary(data3$age_range)
ggplot(data3, aes(x=age_range)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Age range", y="Frequency") +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  scale_y_continuous(breaks=c(0, 1, 2, 4, 5, 9), labels = c("0", "1", "2", "4", "5", "9")) +
  theme(axis.text=element_text(size=13))


# Work time
print("How long have you been working in your current role")
summary(data3$worktime)
ggplot(data3, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency")  + theme(axis.text=element_text(size=13)) + 
  scale_x_continuous(breaks=c(0,12,24,60,120), labels=c("<6m","1y","2y","5y", "10y")) + 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6), labels = c("0", "1", "2", "3", "4", "5", "6")) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())


# Team size
print("Team size")
summary(data3$teamsize)
ggplot(data3, aes(x=teamsize)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13)) + 
  scale_y_continuous(breaks=c(0,2,3,5,8), labels = c("0", "2", "3", "5", "8")) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Work location
print("Work location")
summary(data3$location)
ggplot(data3, aes(x=location)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="Work location", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,2,3,10,16), labels = c("0","2", "3", "10", "16"))

# End user
print("End user")
summary(data3$end_user)
ggplot(data3, aes(x=end_user)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="End user", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,5,7,10,14), labels = c("0", "5", "7", "10", "14"))

# 2.1 In which development activities are users involved in your company? (click all that apply)
# Note the changes in the activity staetments 
#useractivities3.forming.ideas <- sum(data3$useractivities.forming.ideas, na.rm=TRUE)
useractivities3.gathering.requirements <- sum(data3$useractivities.gathering.requirements, na.rm=TRUE) 
useractivities3.software.design <- sum(data3$useractivities.software.design, na.rm=TRUE)
useractivities3.implementation <- sum(data3$useractivities.implementation, na.rm=TRUE)
useractivities3.testing <- sum(data3$useractivities.testing, na.rm=TRUE)
useractivities3.after.release <- sum(data3$useractivities.after.release, na.rm=TRUE)
#useractivities3.commiting.code <- sum(data3$useractivities.commiting.code, na.rm=TRUE)
#useractivities3.providing.fixes <- sum(data3$useractivities.providing.fixes, na.rm=TRUE)
#useractivities3.submitting.bugs <- sum(data3$useractivities.submitting.bugs, na.rm=TRUE)
#useractivities3.online.discussion<- sum(data3$useractivities.online.discussion, na.rm=TRUE)
useractivities3.other <- sum(data3$useractivities.other, na.rm=TRUE)
useractivities3 <- data.frame(Activity=useractivities3.options,
                             Frequency=c(
                               useractivities3.gathering.requirements,
                               useractivities3.software.design,
                               useractivities3.implementation,
                               useractivities3.testing,
                               useractivities3.after.release,
                               useractivities3.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities3)
ggplot(data=useractivities3, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="plum4", colour="black") 

# 2.2 How much do you agree with the following statements?
userinv3 <- data.frame(Statement=factor(rep(userinv3.statements, each=length(data3$userinv.S1))),
                      Rating=c(
                        data3$userinv.S1,
                        data3$userinv.S2,
                        data3$userinv.S3,
                        data3$userinv.S4,
                        data3$userinv.S5,
                        data3$userinv.S6),
                      Jobf = data3$role,
                      Comp = "Vaadin"
                      )
ggplot(data=userinv3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=16)) 
#+
#  facet_wrap(~userinv3$Jobf)
# + scale_y_discrete(name="Rating", limits=c("1","2", "3","4","5"))

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf3 <- data.frame(Statement=factor(rep(userinf3.statements, each=length(data3$userinf.pro.mng))),
                      Rating=c(
                        data3$userinf.mng,
                        data3$userinf.ux.designer,
                        data3$userinf.dev.frame,
                        data3$userinf.slf),
                      Jobf = data3$role,
                      Comp = "Vaadin")
ggplot(data=userinf3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() 

# 2.4 How often do you use the following ways to get information about users?
infofreq3 <- data.frame(Statement=factor(rep(infofreq3.statements, each=length(data3$infofreq.O1))),
                       Rating=c(
                         data3$infofreq.O1,
                         data3$infofreq.O3,
                         data3$infofreq.O4,
                         data3$infofreq.O5),
                       Jobf = data3$role,
                       Comp = 'Vaadin')
                       
ggplot(data=infofreq3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

#2.5 Try to remember a situation
data3$userinf.open

# 3.1 Does your company conduct experiments involving the users?
ggplot(data3, aes(x=condexp)) +
  geom_bar(fill="burlywood1", colour="white") +
  labs(x="Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=16)) + scale_y_continuous(breaks=c(0,2,6,11), labels = c("0", "2", "6", "11"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data3$exp.open

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
understanding3 <- data.frame(Statement=factor(rep(understanding3.statements, each=length(data3$understanding.S1))),
                            Rating=c(
                              data3$understanding.S1,
                              data3$understanding.S2,
                              data3$understanding.S3,
                              data3$understanding.S4,
                              data3$understanding.S5,
                              data3$understanding.S6),
                            Jobf = data3$role,
                            Comp = 'Vaadin')
ggplot(data=understanding3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("..we need to measure user behaviour to decide what the software should be like","..users themselves must be actively involved in shaping the software", "..focused data on a specific user action or behaviour is useful", "..rich, detailed data about what users do is useful", "..data should only be collected when there is a known need or assumption", "..data should always be collected because it might be needed later" )) + theme(axis.text=element_text(size=11))

# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif3 <- data.frame(Statement=factor(rep(usernotif3.statements, each=length(data3$usernotif.S1))),
                          Rating=c(
                            data3$usernotif.S1,
                            data3$usernotif.S2,
                            data3$usernotif.S3,
                            data3$usernotif.S4,
                            data3$usernotif.S5,
                            data3$usernotif.S6,
                            data3$usernotif.S7),
                          Jobf = data3$role,
                          Comp = 'Vaadin')
ggplot(data=undernotif3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11)) + 
  ggtitle("Vaadin")

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
                      Jobf = data3$role,
                      Comp = 'Vaadin')
ggplot(data=expinv3, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))

#🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩🚩

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
data4$role <- car::recode(data4$jobfunction4,
                     "c('Developing software')='Developer'; c('Product management','Business development')='Manager'; c('UX design','Graphic design')='UX designer'; c('Coaching','Other')='Other'")
# Worktime
data4$worktime <- data4$X1.2.How.long.have.you.been.working.in.your.current.company.role.
#fixings: 9999 to: 132 for a better histogram 
data4$worktime[c(9, 12, 24, 30, 40, 64)] <- 132 

#[15:58, 1/26/2017] Fabian Fagerholm: and the scale will draw better                        
#[15:59, 1/26/2017] Fabian Fagerholm: but we have to remember that when reporting

data4$gender <- factor(data4$X1.3.Which.of.the.following.best.describes.you..,
                      levels = c("F", "M", "NA"),
                      labels = c("Female", "Male", "Other / prefer not to say"))

# Age range
data4$age_range <- factor(data4$X1.4.What.is.your.age.range..,
                         levels = c(0:4),
                         labels = c("20 or less", "21-30", "31-40", "41-50", "50 or more"))

#Team size
data4$teamsize <- factor(data4$X1.5.How.many.people.do.you.work.with.on.a.regular.basis.in.the.company..,
                        levels = c(0:4),
                        labels = c("< 3", "3-5", "6-10", "11-20", ">20"))
# Work location
data4$location <- factor(data4$X1.6.Where.is.your.primary.office.located..,
                        levels = c(0:4),
                        labels = c("Helsinki", "Seinäjoki", "New York", "Tokyo", "Amsterdam"))
#### HUOM! UNSOLVED CHAR PROBLEM!


# End user
data4$end_user <- factor(data4$X1.7.Which.one.do.you.consider.to.be.the.current.primary.project.you.work.in..,
                        levels = c(0:1),
                        labels = c("Internal project", "Customer project"))

data4$end_user_open <- data4$If.customer.project..please.provide.the.name.of.the.customer.s....The.answers.are.confidential..

# 2.1 In which development activities are users involved in your company? (click all that apply)
# Note the changes in the activities: first two statements and design statements. 
data4$useractivities.forming.ideas <- data4$Forming.product.or.service.ideas
data4$useractivities.gathering.requirements <- data4$Gathering.requirements
data4$useractivities.ui.design <- data4$UI.design
data4$useractivities.tech.design <- data4$Technical.design
data4$useractivities.implementation <- data4$Implementation
data4$useractivities.testing <- data4$Testing
data4$useractivities.after.release <- data4$The.activities.after.release
data4$useractivities.other <- data4$Other
#data4$useractivities.other.open <- data4$If.other..please.specify
useractivities4.options <- c("Specifying requirements", "Software design", "Implementation", "Testing", "The activities after release", "Other")

#Other activities
data4$useractivities.other.open <- data4$If.other..please.specify.1

# 2.2. How much do you agree with the following statements? (User involvement statements)
data4$userinv.S1 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...I.know.who.uses.the.software.I.contribute.to.in.my.work
data4$userinv.S2 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...I.need.to.ask.for.permission.to.contact.users
data4$userinv.S3 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...I.frequently.have.direct.contact.with.users
data4$userinv.S4 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.sufficient.information.about.users..needs
data4$userinv.S5 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...I.have.information.about.users.that.is.relevant.for.my.work
data4$userinv.S6 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...The.information.I.have.about.users.is.up.to.date
data4$userinv.S7 <- data4$X2.2..How.much.do.you.agree.with.the.following.statements...I.would.like.to.get.more.feedback.from.users

userinv4.statements <- c(
  "I know who uses the software I contribute to in my work",
  "I need to ask for permission to contact users",
  "I frequently have direct contact with users",
  "I have sufficient information about users' needs",
  "I have information about users that is relevant for my work",
  "The information I have about users is up to date"
)
userinv4.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

# 2.3 In your experience, how easy is it for the following to get information from users?
data4$userinf.dev <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Developers
data4$userinf.mng <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Managers
data4$userinf.bis.dev <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Business.developers
data4$userinf.ux.designer <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...UX.designers
data4$userinf.graph.designer <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Graphic.designers
data4$userinf.coach <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Coaches
data4$userinf.slf <- data4$X2.3.In.your.experience..how.easy.is.it.for.the.following.roles.to.get.information.from.users..Please.consider.the.roles.in.your.company.context...Myself
userinf4.statements <- c("Managers", "UX designers", "Developers", "Myself")
userinf4.options <- c("Very difficult", "Difficult", "Neither easy nor difficult", "Easy", "Very easy", "I don't know")

# 2.4 How often do you use the following ways to get information about users?
data4$infofreq.O1 <- data4$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.the.users.after.the.initial.design..e.g...screen.sharing.
data4$infofreq.O2 <- data4$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.the.users.when.they.are.using.the.software..e.g...talk.aloud.study.
data4$infofreq.O3 <- data4$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.the.users.before.they.use.the.software
data4$infofreq.O4 <- data4$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...I.interact.with.users.after.they.used.the.software..e.g...post.use.interview.
data4$infofreq.O5 <- data4$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.recorded.usage.data..e.g...log.data.
data4$infofreq.O6 <- data4$X2.4.How.often.do.you.use.the.following.ways.to.get.information.about.users...Through.direct.feedback..e.g..via.email..forums..

infofreq4.statements <- c(
  "I remotely observe or interact with users when they are using the software (e.g., screen sharing)",
  "I interact with the users before they use the software",
  "I interact with users after they used the software (e.g., post-use interview, feedback)",
  "Through recorded usage data (e.g., log data)")
infofreq4.options <- c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2.5 Try to remember a situation where you knew that involving users in development would be useful, but you could not involve them. Please describe the situation and what challenges you faced.
data4$userinf.open <- data4$X2.5.Try.to.remember.a.situation.where.you.knew.that.involving.users.in.development.would.be.useful..but.you.could.not.involve.them..Please.describe.the.situation.and.what.challenges.you.faced.

# 3.1 Does your company conduct experiments involving the users?
data4$condexp <- factor(data4$X3.1.Does.your.company.conduct.experiments.involving.the.users.,
                       levels = c(1:4, "NA"),
                       labels = c("Never", "Rarely", "Occasionally", "Yes, actively", "I don't know"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data4$exp.open <- data4$X3.2.Please.describe.a.typical.experiment.you.have.seen.or.been.involved.in.in.your.company..including.the.roles...Skip.this.if.you.have.not.seen.or.been.involved.in.any.experiments..

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
data4$understanding.S1 <- data4$To.understand.users..needs.better.......data.should.always.be.collected.because.it.might.be.needed.later
data4$understanding.S2 <- data4$To.understand.users..needs.better.......data.should.only.be.collected.when.there.is.a.known.need.or.assumption
data4$understanding.S3 <- data4$To.understand.users..needs.better.......rich..detailed.data.about.what.users.do.is.useful
data4$understanding.S4 <- data4$To.understand.users..needs.better.......focused.data.on.a.specific.user.action.or.behaviour.is.useful
data4$understanding.S5 <- data4$To.understand.users..needs.better.......users.themselves.must.be.actively.involved.in.shaping.the.software
data4$understanding.S6 <- data4$To.understand.users..needs.better.......we.need.to.measure.user.behaviour.to.decide.what.the.software.should.be.like
understanding4.statements <- c(
  "..data should always be collected because it might be needed later",
  "..data should only be collected when there is a known need or assumption",
  "..rich, detailed data about what users do is useful",
  "..focused data on a specific user action or behaviour is useful",
  "..users themselves must be actively involved in shaping the software",
  "..we need to measure user behaviour to decide what the software should be like"
)
understanding4.options <- c("Completely disagree", "Disagree", "Neither disagree or agree", "Agree", "Completely agree", "I don't know")

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

#🚩🚩🚩🚩🚩🚩🚩���
# section 2 reaktor

attach(data4)
print("Primary job function")
summary(jobfunction4)
ggplot(data4, aes(x=role)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 2, 3, 12, 43), labels = c("0", "2", "3", "12", "43"))

data4$jobfunction4.other

# Gender
print("Gender")
summary(data4$gender)
ggplot(data4, aes(x=gender)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=15)) + scale_y_continuous(breaks=c(0, 3, 6, 57), labels = c("0", "3", "6", "57"))

# Age range
print("Age range")
summary(data4$age_range)
ggplot(data4, aes(x=age_range)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Age range", y="Frequency") + scale_y_continuous(breaks=c(0, 14, 16, 36), labels = c("0", "14", "16", "36")) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  theme(axis.text=element_text(size=13))

# Work time
print("How long have you been working in your current role")
summary(data4$worktime)
ggplot(data4, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency")  + theme(axis.text=element_text(size=13))+ 
  scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9")) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Team size
print("Team size")
summary(data4$teamsize)
ggplot(data4, aes(x=teamsize)) +
  geom_bar(fill="white", colour="black") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13)) + 
  scale_y_continuous(breaks=c(0,3,6,10,20,27), labels = c("0", "3", "6", "10", "20","27")) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# Work location
print("Work location")
summary(data4$location)
ggplot(data4, aes(x=location)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="Work location", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,2,3,10,16), labels = c("0","2", "3", "10", "16"))

# End user
print("End user")
summary(data4$end_user)
ggplot(data4, aes(x=end_user)) +
  geom_bar(fill="cadetblue2", colour="white") +
  labs(x="End user", y="Frequency") + theme(axis.text=element_text(size=13)) + scale_y_continuous(breaks=c(0,6,60), labels = c("0", "6", "60"))

summary(data4$end_user_open)
##BE BACK HERE! Na'a could be the problem

# 2.1 In which development activities are users involved in your company? (click all that apply)
#useractivities4.forming.ideas <- sum(data4$useractivities.forming.ideas, na.rm=TRUE) 
#merging forming ideas with gathering requirements below
useractivities4.gathering.requirements <- sum(data4$useractivities.gathering.requirements, na.rm=TRUE)
useractivities4.ui.design <- sum(data4$useractivities.ui.design, na.rm=TRUE)
#useractivities4.tech.design <- sum(data4$useractivities.tech.design, na.rm=TRUE) + sum(data4$useractivities.ui.design, na.rm=TRUE)
useractivities4.implementation <- sum(data4$useractivities.implementation, na.rm=TRUE)
useractivities4.testing <- sum(data4$useractivities.testing, na.rm=TRUE)
useractivities4.after.release <- sum(data4$useractivities.after.release, na.rm=TRUE)
useractivities4.other <- sum(data4$useractivities.other, na.rm=TRUE)
useractivities4 <- data.frame(Activity=useractivities4.options,
                             Frequency=c(
                               useractivities4.gathering.requirements,
                               useractivities4.ui.design,
                               useractivities4.implementation,
                               useractivities4.testing,
                               useractivities4.after.release,
                               useractivities4.other))

print("Frequencies of development activities that users are involved in")
summary(useractivities4)
ggplot(data=useractivities4, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="plum4", colour="black") + 
  theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ scale_x_discrete(limits=c("Specifying requirements", "Software design", "Implementation", "Testing", "The activities after release", "Other")) + scale_y_continuous(breaks=c(0,99,74,27,48,46,5), labels = c("0", "99", "74", "27", "48", "46", "5"))


# 2.2 How much do you agree with the following statements?
userinv4 <- data.frame(Statement=factor(rep(userinv4.statements, each=length(data4$userinv.S1))),
                      Rating=c(
                        data4$userinv.S1,
                        data4$userinv.S2,
                        data4$userinv.S3,
                        data4$userinv.S4,
                        data4$userinv.S5,
                        data4$userinv.S6),
                      Jobf = data4$role,
                      Comp = "Reaktor"
                      )
                        
ggplot(userinv4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=16))
#+ facet_wrap(~userinv4$Jobf)
# + scale_y_discrete(name="Rating", limits=c("1","2", "3","4","5"))

# 2.3 In your experience, how easy is it for the following to get information from users?
userinf4 <- data.frame(Statement=factor(rep(userinf4.statements, each=length(data4$userinf.mng))),
                      Rating=c(
                        data4$userinf.mng,
                        data4$userinf.ux.designer,
                        data4$userinf.dev,
                        data4$userinf.slf),
                      Jobf = data4$role,
                      Comp = "Reaktor")
ggplot(data=userinf4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() 

# 2.4 How often do you use the following ways to get information about users?
infofreq4 <- data.frame(Statement=factor(rep(infofreq4.statements, each=length(data4$infofreq.O1))),
                       Rating=c(
                         data4$infofreq.O2,
                         data4$infofreq.O3,
                         data4$infofreq.O4,
                         data4$infofreq.O5),
                       Jobf = data4$role,
                       Comp = 'Reaktor')
ggplot(data=infofreq4, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() 

#2.5 Try to remember a situation
data4$userinf.open

# 3.1 Does your company conduct experiments involving the users?
ggplot(data4, aes(x=condexp)) +
  geom_bar(fill="burlywood1", colour="white") +
  labs(x="Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=16)) + scale_y_continuous(breaks=c(0,2,6,11), labels = c("0", "2", "6", "11"))

# 3.2 Please describe a typical experiment you have seen or been involved in in your company, including the roles. (Skip this if you have not seen or been involved in any experiments.)
data4$exp.open

# 3.3 Below are three pairs of statements about collecting data for understanding user needs. How much do you agree with each statement?
understanding4 <- data.frame(Statement=factor(rep(understanding4.statements, each=length(data4$understanding.S1))),
                            Rating=c(
                              data4$understanding.S1,
                              data4$understanding.S2,
                              data4$understanding.S3,
                              data4$understanding.S4,
                              data4$understanding.S5,
                              data4$understanding.S6),
                            Jobf = data4$role,
                            Comp = 'Reaktor')
ggplot(data=understanding4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + scale_x_discrete(limits=c("..we need to measure user behaviour to decide what the software should be like","..users themselves must be actively involved in shaping the software", "..focused data on a specific user action or behaviour is useful", "..rich, detailed data about what users do is useful", "..data should only be collected when there is a known need or assumption", "..data should always be collected because it might be needed later" )) + theme(axis.text=element_text(size=11))
# 4.1 How much do you agree with the following statements regarding notifying users about experiments? Please answer according to your personal beliefs.
undernotif4 <- data.frame(Statement=factor(rep(usernotif4.statements, each=length(data4$usernotif.S1))),
                          Rating=c(
                            data4$usernotif.S1,
                            data4$usernotif.S2,
                            data4$usernotif.S3,
                            data4$usernotif.S4,
                            data4$usernotif.S5,
                            data4$usernotif.S6,
                            data4$usernotif.S7),
                          Jobf = data4$role,
                          Comp = 'Reaktor')
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
                      Jobf = data4$role,
                      Comp = 'Reaktor')
ggplot(data=expinv4, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + coord_flip() + theme(axis.text=element_text(size=11))




