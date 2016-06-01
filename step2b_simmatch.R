library(ggplot2)
library(plyr)
library(Matching)

##### READ IN DATA #####
### CORRECTED VALUES OF SINGLES AND AGENTS
data <- read.csv2("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/data.csv", header=T)
singles <- read.csv("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/singles.csv", header=T)
partners <- read.csv("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/partners.csv", header=T)

### DETERMINE GENERAL PARAMETERS
N <- dim(partners)[1] # Number of Age Groups in partner dataset
X <- 500000 # Number of agents to be simulated
S <- round(X*sum(data$femratio*(1-data$relw_share)*data$ageshare + (1-data$femratio)*(1-data$relm_share)*data$ageshare)) # Calculate number of single agents
set.seed(5)

### MATRICES OF AGE DIFFERENCES
mat.mm <- read.csv2("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/mat.msm.csv", header=F)
mat.ww <- read.csv2("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/mat.wsw.csv", header=F)
mat.mw <- read.csv2("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/mat.msw.csv", header=F)
mat.wm <- read.csv2("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/mat.wsm.csv", header=F)

### TAKE TIME
tic <- Sys.time()

##### SECTION 3: SIMULATE POPULATION #####

### CREATE EMPTY DATAFRAME FOR AGENTS
pop <- matrix(ncol = 9, nrow = X, dimnames=list(1:X, c("id", "age", "sex", "sexor", "rel", "pid", "page", "psex", "psexor")))

i<-1;j<-1
### CREATE SINGLE AGENTS
repeat{  
  # assign id:
  pop[i,1] <- i
  # sample age:
  pop[i,2] <- sample(singles$age, 1, replace = T, prob = singles$ageshare)
  # sample sex dependent on age (0:=female, 1:=male):
  pop[i,3] <- sample(c(0,1), 1, replace = T, prob = c(singles$femratio[pop[i,2]+1],(1 - singles$femratio[pop[i,2]+1]))) 
  # sample sexor dependent on sex
  pop[i,4] <- if (pop[i,3] == 0) {sample(c(1,0), 1, replace = T, prob = c(singles$wsw_rate[pop[i,2]+1], (1 - singles$wsw_rate[pop[i,2]+1])))} else {sample(c(1,0), 1, replace=T, prob=c(singles$msm_rate[pop[i,2]+1], (1 - singles$msm_rate[pop[i,2]+1])))}
  # relationship status
  pop[i,5] <- 0 
  # values of non-existent partner
  pop[i,6] <- 0
  pop[i,7] <- 0
  pop[i,8] <- 0
  pop[i,9] <- 0
  i <- i + 1
  if (i > S) break
}
p.index <- i
### CREATE PARTNER AGENTS
repeat{
  # assign id:
  pop[i,1]<-i
  # sample age:
  pop[i,2] <- sample(partners$age, 1, replace=T, prob=partners$ageshare)
  # sample sex dependent on age (0:=female, 1:=male):
  pop[i,3] <- sample(c(0,1), 1, replace=T, prob=c(partners$femratio[pop[i,2]-11], (1 - partners$femratio[pop[i,2]-11]))) 
  # sample sexor dependent on sex (0:=heterosexual, 1:=homosexual)
  if (pop[i,3]==0) {pop[i,4] <- sample(c(1,0), 1, replace=T, prob=c(partners$wsw_rate[pop[i,2]-11], (1 - partners$wsw_rate[pop[i,2]-11])))} 
  if (pop[i,3]==1) {pop[i,4] <- sample(c(1,0), 1, replace=T, prob=c(partners$msm_rate[pop[i,2]-11], (1 - partners$msm_rate[pop[i,2]-11])))}
  # relationship status
  pop[i,5] <- 1 
  # Set no partner id
  pop[i, 6] <- NA
  # preferences in partnership
  # preference in age corresponding to the dependent age distribution dependent on sex and sexual orientation:
  if (pop[i,3] == 0 & pop[i,4] == 1){pop[i,7] <- sample(c(12:100), 1, replace = T, mat.ww[,pop[i,2]-11])}
  if (pop[i,3] == 0 & pop[i,4] == 0){pop[i,7] <- sample(c(12:100), 1, replace = T, mat.wm[,pop[i,2]-11])}
  if (pop[i,3] == 1 & pop[i,4] == 0){pop[i,7] <- sample(c(12:100), 1, replace = T, mat.mw[,pop[i,2]-11])}
  if (pop[i,3] == 1 & pop[i,4] == 1){pop[i,7] <- sample(c(12:100), 1, replace = T, mat.mm[,pop[i,2]-11])}
  # preference in sex corresponding to the agents sexual orientation
  if (pop[i,4] == 0) {pop[i,8] <- abs(1 - pop[i,3])}
  if (pop[i,4] == 1) {pop[i,8] <- pop[i,3]}
  # set partner sexor corresponding to the agents sexual orientation
  pop[i,9] <- pop[i,4]
  i <- i + 1
  if (i > X) break
}

toc1 <- Sys.time()

pop <- as.data.frame(pop)

### SELECT PARTNER AGENTS TO BE MATCHED
dtm <- pop[p.index:dim(pop)[1],]

# SELECT HETEROSEXUAL FEMALE AND MALE AGENTS
dtm.het <- dtm[which(dtm$sexor == 0),]
# CREATE MATCHING VARIABLE M(atching)AGE
dtm.het$mage <- NA
dtm.het[which(dtm.het$sex == 0),"mage"] <- dtm.het[which(dtm.het$sex == 0),"age"]
dtm.het[which(dtm.het$sex == 1),"mage"] <- dtm.het[which(dtm.het$sex == 1),"page"]
# MATCH HETEROSEXUAL COUPLES USING MATCHING PACKAGE
result.het <- Match(
  Tr = dtm.het[,"sex"],
  X = dtm.het[,"mage"],
  M = 1,
  caliper = 2,
  ties=F,
  replace=F,
  version="fast")

dtm.het[c(result.het$index.treated), c("pid", "page")] <- dtm.het[c(result.het$index.control), c("id", "age")]
dtm.het[c(result.het$index.control), c("pid", "page")] <- dtm.het[c(result.het$index.treated), c("id", "age")]
dtm.het$mage <- NULL


# SELECT HOMOSEXUAL FEMALE AGENTS
dtm.wsw <- dtm[which(dtm$sexor == 1 & dtm$sex == 0),]
# CREATE "TREATMENT" VARIABLE
dtm.wsw$TR <- sample(c(0,1), replace=T, dim(dtm.wsw)[1])
# CREATE MATCHING VARIABLE M(atching)AGE
dtm.wsw$mage <- dtm.wsw$page
# MATCH HOMOSEXUAL, FEMALE COUPLES USING MATCHING PACKAGE
result.wsw <- Match(
  Tr = dtm.wsw[,"TR"],
  X = dtm.wsw[,"mage"],
  M = 1,
  caliper = 2,
  ties=F,
  replace=F,
  version="fast")

dtm.wsw[c(result.wsw$index.treated), c("pid", "page")] <- dtm.wsw[c(result.wsw$index.control), c("id", "age")]
dtm.wsw[c(result.wsw$index.control), c("pid", "page")] <- dtm.wsw[c(result.wsw$index.treated), c("id", "age")]
dtm.wsw$mage <- NULL
dtm.wsw$TR <- NULL


# SELECT HOMOSEXUAL MALE AGENTS
dtm.msm <- dtm[which(dtm$sexor == 1 & dtm$sex == 1),]
# CREATE "TREATMENT" VARIABLE
dtm.msm$TR <- sample(c(0,1), replace=T, dim(dtm.msm)[1])
# CREATE MATCHING VARIABLE M(atching)AGE
dtm.msm$mage <- dtm.msm$page
# MATCH HOMOSEXUAL, FEMALE COUPLES USING MATCHING PACKAGE
result.msm <- Match(
  Tr = dtm.msm[,"TR"],
  X = dtm.msm[,"mage"],
  M = 1,
  caliper = 2,
  ties=F,
  replace=F,
  version="fast")

dtm.msm[c(result.msm$index.treated), c("pid", "page")] <- dtm.msm[c(result.msm$index.control), c("id", "age")]
dtm.msm[c(result.msm$index.control), c("pid", "page")] <- dtm.msm[c(result.msm$index.treated), c("id", "age")]
dtm.msm$mage <- NULL
dtm.msm$TR <- NULL

pop <- rbind(pop[1:(p.index-1),], dtm.het, dtm.msm, dtm.wsw)

toc2 <- Sys.time()
toc1 - tic
toc2 - tic  

write.csv(pop, "pop.match.csv")


summary(pop)

## age pyramid:
data1 <- read.csv("https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/GER_pop.csv")
data1$count <-  data1$count*5
data1$single <- abs(data1$partner - 1)
datafem <- subset(data1, data1[,'sex0'] == 0)
colnames(datafem) <- c("age", "count", "sex", "partner", "single")
datamal <- subset(data1, data1[,'sex0'] == 1)
colnames(datamal) <- c("age", "count", "sex", "partner", "single")

pop$partner <- abs(pop$rel-1)
pop$test <- apply(pop[,c("sex", "rel", "sexor")], 1, paste, collapse="")

ggplot(data=pop,aes(x = age, fill=interaction(as.factor(sex), as.factor(partner)))) +
  geom_bar(data=pop[pop$sex==0,], stat="count") + 
  geom_bar(data=pop[pop$sex==1,], aes(y=..count..*(-1))) +
  geom_line(data=datafem, aes(x=age, y=count, linetype=as.factor(partner)), colour="#0066CC", size=1) +
  geom_line(data=datamal, aes(x=age, y=count, linetype=as.factor(partner)), colour="#E30026", size=1) +  
  theme_bw() +
  scale_y_continuous("Number of agents", breaks=c(-4000, -2000, 0, 2000, 4000), labels=c(4000, 2000, 0, 2000, 4000)) + 
  scale_x_continuous("Age of agents", breaks=c(-0.5, 24.5, 49.5, 74.5, 99.5), label=c(0, 25, 50, 75, 100), expand=c(0.01, 0.01)) +
  scale_fill_manual(values=c("#CA2D78", "#F192B7", "#0784C8", "#7FA6B5"), labels=c("Female with partner", "Female single", "Male with partner", "Male single"), guide = guide_legend(reverse=TRUE)) + 
  scale_linetype_manual(values=c(1,2), labels=c("partnership","single")) +
  theme(legend.position=c(.15, .875), legend.box.just = "left") +
  labs(linetype="Real population", fill="Agent population") +
  coord_flip()

ggsave("AGEPYR_match.pdf", device="pdf", height=5.75, width=4, units='in', scale=2)
