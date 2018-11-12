######################################################################
# Cross analysis script for User Involvement Survey - Whole

######################################################################

######################################################################
# Imports and constants
######################################################################
######################################################################
# Cross analysis script for User Involvement Survey - Whole

######################################################################

######################################################################
# Imports and constants
######################################################################
source(file='Import.R')

# First, make company-specific data frames that all have exactly the same columns
# which( colnames(data4)=="condexp" ) to check column indexes
cols1 <- c(2:5, 7, 42:55, 58, 59, 62:64, 72:80, 84, 90, 92:97, 85:88) # Ericsson
cols2 <- c(2:5, 6, 53:66, 81, 82, 84, 83, 85, 97:102, 104, 105, 108, 113, 121, 123:128, 114, 116:118) # F-secure
cols3 <- c(2:5, 7, 56:69, 72, 73, 77, 74, 78, 93:98, 101:103, 110, 118, 120:125, 111, 113:115) # Vaadin
cols4 <- c(2:5, 6, 50:63, 77, 78, 80, 79, 81, 94:99, 102, 104, 101, 107, 115, 117:122, 109:112) # Reaktor

clus_colnames  <- c("SUBTIME", "JOBFUNC", "JOBFUNCOTHER", "JOBTIME", "GENDER", "INVA", "INVB", "INVC", "INVD", "INVE", "INVF", "INVG", "INVH", "INVI", "INVJ", "INVK", "INVL", "INVM", "INVN", "ROLE", "worktime", "age_range","gender", "team_size", "USERA","USERB","USERC","USERD","USERE","USERF", "INF.MNG","INF.UX", "INF.DEV", "INF.SLF", "condexp", "3.3A", "3.3B","3.3C","3.3D","3.3E", "3.3F", "M1", "M2", "M3", "M4")
clus_data1 <- data1[, cols1]
names(clus_data1) <- clus_colnames
clus_data2 <- data2[, cols2]
names(clus_data2) <- clus_colnames
clus_data3 <- data3[, cols3]
names(clus_data3) <- clus_colnames
clus_data4 <- data4[, cols4]
names(clus_data4) <- clus_colnames

# Second, concatenate the company names' column
clus_data <- rbind(clus_data1, clus_data2, clus_data3, clus_data4)
clus_data[1:35,46] <- "Ericsson"
clus_data[36:43,46] <- "F-secure"
clus_data[44:64,46] <- "Vaadin"
clus_data[65:130,46] <- "Reaktor"
names(clus_data)[46] <- "COMPANY"


## DEMOGRAPHICS ##
# 1.1 job functions total and over companies 
ggplot(clus_data, aes(x=ROLE)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_y_continuous(breaks=c(0, 14, 22, 23, 71), labels = c("0", "14", "22", "23", "71")) 

ggplot(clus_data, aes(x=ROLE)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Job functions", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~clus_data$COMPANY)

# 1.2 worktime total (remove the facet_wrap parameter) and worktime over companies 
ggplot(clus_data, aes(x=worktime)) +
  geom_histogram(binwidth=12, colour="black", fill="white") +
  labs(x="Work time", y="Frequency") + theme(axis.text=element_text(size=13)) + 
  facet_wrap(~clus_data$ROLE) + scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9"))
# example: looking into developers whose worktime is less than 1y
# View(clus_data[ which(clus_data$worktime<=12 & clus_data$ROLE == "Developer"), ], )

# one company, example dataset3
ggplot(data3, aes(x=data3$worktime)) +
     geom_histogram(binwidth=12, colour="black", fill="white") +
     labs(x="Vaadin_Work time", y="Frequency") + theme(axis.text=element_text(size=13)) + 
  scale_x_continuous(breaks=c(1, 12, 24, 36, 48, 60, 72, 96, 108, 120, 132), labels=c("<1y", "1y", "2y","3y", "4y", "5y","6y", "8y", "9y", "10y", ">10y")) + scale_y_continuous(breaks=c(0,1,4,5,6,7,8,9), labels = c("0", "1", "4", "5", "6","7", "8", "9"))

# 1.3 age range over companies
ggplot(clus_data, aes(x=age_range)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Age range", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~clus_data$ROLE)

# one company
ggplot(data2, aes(x=data2$age_range)) +
     geom_bar(fill="#FF9999", colour="white") +
     labs(x="Age range", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

# 1.4 gender over companies/roles
ggplot(clus_data, aes(x=gender)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~clus_data$ROLE)

# one company, example data2
ggplot(data2, aes(x=data2$gender)) +
     geom_bar(fill="#FF9999", colour="white") +
     labs(x="Gender", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# 1.5 team size
ggplot(clus_data, aes(x=team_size)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Team size", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~clus_data$ROLE) + scale_y_continuous(breaks=c(0,3,6,9,12,20,25), labels=c("0", "3","6", "9","3y","5y", "10y"))

# one company example data3
ggplot(data3, aes(x=data3$teamsize)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Vaadin_Team size", y="Frequency") + theme(axis.text=element_text(size=13))

## END OF DEMOGRAPHICS ##

# 2.1
# one data frame for the merged statements
useractivities = cbind(useractivities1[1], useractivities1[-1] + useractivities2[-1] + useractivities3[-1] + useractivities4[-1])
summary(useractivities)
ggplot(useractivities, aes(x=Activity, y=Frequency)) +
  geom_bar(stat="identity", fill="#FF9999", colour="#FF9999") +
  labs(x="Total Activities", y="Frequency") + theme(axis.text=element_text(size=13)) 

# one company
ggplot(useractivities2, aes(x=Activity, y=Frequency)) +
       geom_bar(stat="identity", fill="#FF9999", colour="#FF9999") +
       labs(x="F_secure_Total Activities", y="Frequency") + theme(axis.text=element_text(size=13)) 

# 2.2 
userinv <- rbind(userinv1, userinv2, userinv3, userinv4)
ggplot(userinv, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() +
  labs(x="Total involvement statements", y="Frequency") + theme(axis.text=element_text(size=13)) +
  facet_wrap(~userinv$Jobf)
data.frame(userinv)
# Example counting on specific statements:
# count(userinv[ which(userinv$Statement=='The information I have about users is up to date'), ], "Rating")

# to mask the company names 
jb_names = c("Ericsson"="Company A", "F-secure"="Company B","Vaadin"="Company C", "Reaktor"="Company D")

# facet_wraping over companies 
ggplot(userinv, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  labs(x="Total involvement statements", y="Frequency") + theme(axis.text=element_text(size=9)) + 
  facet_wrap(~userinv$Comp, ncol = 4, labeller = as_labeller(jb_names)) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# facet_wraping over roles 
ggplot(subset(userinv, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  labs(x="Total involvement statements", y="Frequency") + theme(axis.text=element_text(size=9)) + 
  facet_wrap(~Jobf, ncol = 3) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# 2.3
userinf <- rbind(userinf1, userinf2, userinf3, userinf4)
ggplot(userinf, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() +
  labs(x="Who is closest to user info?", y="Frequency") + theme(axis.text=element_text(size=13))+ 
  facet_wrap(~userinf$Jobf+Comp)

# 2.4
infofreq <- rbind(infofreq1, infofreq2, infofreq3, infofreq4)
ggplot(infofreq, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot() + guides(fill=FALSE) + coord_flip() + 
  labs(x="Total_Methods used", y="Frequency") + theme(axis.text=element_text(size=7)) +
  facet_wrap(~userinf$Jobf)
# 2.4 with ROLES dropped the other group and violin plot trials
ggplot(subset(infofreq1, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) + geom_violin(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  theme(axis.text=element_text(size=8)) + labs(x="Total_User understanding", y="Frequency") +
  facet_wrap(~Jobf) + 
  geom_jitter(shape=16, position=position_jitter(0.1)) + 
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) 
  

# 3.1 
ggplot(clus_data, aes(x=condexp)) +
  geom_bar(fill="#FF9999", colour="white") +
  labs(x="Total_Conducting experiments", y="Frequency") + theme(axis.text=element_text(size=13), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~clus_data$ROLE, ncol=4)

# 3.3
understanding <- rbind(understanding1, understanding2, understanding3, understanding4)
ggplot(understanding, aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  theme(axis.text=element_text(size=9)) + labs(x="Total_User understanding", y="Frequency") +
  facet_wrap(~understanding$Jobf, ncol = 4, labeller = as_labeller(jb_names)) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())
# 3.3 with the other group dropped on ROLES:
ggplot(subset(understanding, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) + geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + coord_flip() +
  theme(axis.text=element_text(size=9)) + labs(x="Total_User understanding", y="Frequency") +
  facet_wrap(~Jobf, ncol = 3) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# 4.1
undernotif <- rbind(undernotif1, undernotif2, undernotif3, undernotif4)
ggplot(subset(undernotif, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text =element_text(size=8))  + scale_x_discrete(limits=c("Users do not need to know they are involved", "If we collect personal information, users need to be notified", "If no laws are being broken, users do not need to be notified", "Users can be involved in an experiment without their knowledge if we let them know afterwards", "Users should always be notified when they are being involved in an experiment", "It is ok not to disclose all the experiment details to users involved", "It is ok to trick the user if the validty of experiment results depend on it" )) + 
  coord_flip() +
  facet_wrap(~Jobf, ncol = 3) +
  theme(axis.title.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

# with colors
ggplot(undernotif, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + theme(axis.text =element_text(size=7))  +  ggtitle("Total_ethics1") + coord_flip() + 
  theme(axis.text =element_text(size=5)) +
  facet_wrap(~undernotif$Jobf)

# example viewing on specific statements:
# View(undernotif2[ which(undernotif2$Statement=='It is ok not to disclose all the experiment details to users involved'), ], "Rating")

# 4.2
expinv <- rbind(expinv1, expinv2, expinv3, expinv4)
ggplot(subset(expinv, Jobf %in% c("Developer", "Manager", "UX designer")), aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot(fill="white", colour="black") + guides(fill=FALSE) + theme(axis.text=element_text(size=8)) +
  scale_x_discrete(limits=c("I cannot trust that the experiment results will be correct", "Involving users in experiments is time-consuming", "My customer does not have the needed technical infrastructure", "Users would not like to be part of software experiments", "Users have to be convinced of the benefit before taking part", "Experiments give users false expectations", "Experiments reveal secrets about my customer's strategy"))  + 
  theme(axis.title.x=element_blank(), 
        #axis.text.x=element_blank(), 
        #axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) + 
  coord_flip() +
  facet_wrap(~Jobf, ncol = 3)

# with colors
ggplot(expinv, aes(x=Statement, y=Rating, fill=Statement)) +
  geom_boxplot() + guides(fill=FALSE) + theme(axis.text =element_text(size=7))  +  ggtitle("Total_ethics2") + coord_flip() + 
  facet_wrap(~expinv$Jobf+Comp)

# Line plot for mean values
numericColumns <- sapply(clus_data, is.numeric)
qColNames <- colnames(clus_data)[ grepl("^INV",colnames(clus_data)) & numericColumns ]
colNamesToColIndsRules <- setNames( 1:length(qColNames), qColNames )
qDF <- 
  melt( data = clus_data, id.vars = c("gender") ) %>% 
  dplyr::filter( variable %in% qColNames ) %>%
  dplyr::filter(!is.na(value)) %>%
  dplyr::filter(!is.na(gender)) %>%
  group_by( gender, variable )

group_means <- qDF %>%
  group_by(gender, variable) %>%
  dplyr::summarise(mean = mean(as.numeric(value)))

ggplot(group_means, aes(x=as.numeric(variable), y=mean, color=gender)) + geom_line() + 
  scale_x_continuous(labels=as.character(group_means$variable), breaks=as.numeric(group_means$variable))
  
### CORRELATIONS ###

cols <- c(6:19, 25:34, 36:45) # Selecting the numerical columns
clus_data_selected <- clus_data[, cols]
# and a scaled version of that with NA's removed
clus_data_scaled <- scale(na.omit(clus_data_selected))

## Correlation matrix ##

cor_matrix <- cor(clus_data_selected, use = "pairwise.complete.obs")
print(cor_matrix %>% round(2))
# add order = "hclust" as a parameter below for clustering of correlation coefficients
corrplot.mixed(cor_matrix, lower = "number", upper = "circle", order = "hclust")
# simpler view:
corrplot(cor_matrix, order = "hclust", addrect = 5)

# get the  most significant correlations (p > 0.05): 
correlations <- rcorr(as.matrix(clus_data_scaled))
for (i in 1:30){
  for (j in 1:30){
    if ( !is.na(correlations$P[i,j])){
      if ( correlations$P[i,j] < 0.05 ) {
        print(paste(rownames(correlations$P)[i], "-" , colnames(correlations$P)[j], ": ", correlations$P[i,j]))
      }}}}

# highest correlations in each column:
library(data.table)
setDT(melt(cor_matrix))[Var1 != Var2, .SD[which.max(value)], keyby=Var1]
setDT(melt(cor_matrix))[Var1 != Var2, .SD[which.min(value)], keyby=Var1]

# heatmap
col <- colorRampPalette(c("darkblue", "white", "darkorange"))(20) # get some colors
heatmap(x = cor_matrix, col=col, symm = TRUE)

#alternative views
library("PerformanceAnalytics")
chart.Correlation(cor_matrix, histogram = TRUE, pch = 19)

# Hierarchical clustering of observations with company identifiers
d2 <- dist(clus_data_scaled, method="euclidean")
hcl2 <- hclust(d2, method="ward.D2")
plot(hcl2, cex=.5)
groups2 <- cutree(hcl2, k=3)

# to look at the clusters a bit, e.g.: 
table(groups2)
summary(clus_data$ROLE[groups2 == 1])
rect.hclust(hcl2, k=3, border="red")
# shows each clusters' roles
sapply(unique(groups2),function(g)clus_data$ROLE[groups2== g])


# another heatmap
#heatmap with company identifiers
library(gplots)
# get a color palette equal to the number of clusters
clusterCols <- rainbow(length(unique(groups2)))
# create vector of colors for side bar
myClusterSideBar <- clusterCols[groups2]
# choose a color palette for the heat map
myheatcol <- rev(redgreen(75))
# draw the heat map
heatmap.2(clus_data_scaled, main="Hierarchical Cluster", Rowv=as.dendrogram(hcl2), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)


## Principal components analysis
pc <- princomp(cor_matrix, cor=TRUE)
summary(pc)
loadings(pc)
plot(pc, type="lines") # indicates 3 main components
print(pc$scores)
biplot(pc)
# to look at the eigenvalues:
library("factoextra")
get_eigenvalue(pc)

## Factor analysis

# How many factors?
library(nFactors)
ev <- eigen(cor_matrix)
ap <- parallel(subject = nrow(na.omit(clus_data_selected)), var = ncol(na.omit(clus_data_selected)), rep = 100, cent = .05)
nS <- nScree(x = ev$values, aparallel = ap$eigen$qevpea)
plotnScree(nS) 