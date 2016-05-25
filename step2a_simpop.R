library(ggplot2)
library(plyr)

if (Sys.info()['sysname'] == 'Linux'){path <- paste(getwd(), '/', sep='')}
if (Sys.info()['sysname'] == 'Windows'){path <- paste(as.vector(strsplit(getwd(), '/')[[1]][1:length(strsplit(getwd(), '/')[[1]])-1]), '/', sep="", collapse="")}

##### READ IN DATA #####
### CORRECTED VALUES OF SINGLES AND AGENTS
data <- read.csv2(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/data.csv", sep=""), header=T)
singles <- read.csv(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/singles.csv", sep=""), header=T)
partners <- read.csv(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/partners.csv", sep=""), header=T)

### DETERMINE GENERAL PARAMETERS
N <- dim(partners)[1] # Number of Age Groups in partner dataset
X <- 500000 # Number of agents to be simulated
S <- round(X*sum(data$femratio*(1-data$relw_share)*data$ageshare + (1-data$femratio)*(1-data$relm_share)*data$ageshare)) # Calculate number of single agents
set.seed(5)

### MATRICES OF AGE DIFFERENCES
mat.mm <- read.csv2(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/mat.msm.csv", sep=""), header=F)
mat.ww <- read.csv2(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/mat.wsw.csv", sep=""), header=F)
mat.mw <- read.csv2(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/mat.msw.csv", sep=""), header=F)
mat.wm <- read.csv2(paste(path, "/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/agent-creates-agent/mat.wsm.csv", sep=""), header=F)

### TAKE TIME
tic <- Sys.time()

##### SECTION 2.4: PROBABILITIES FOR PARTNER AGENTS #####

### CALCULATE PROBABILITIES OF AGENT As (equation 9)

# FEMALE AGENTS AS AGENT A
female_agent_A <- data.frame(csex = rep(0, 178), cage = rep(seq(1, 89, 1), 2), 
                        psex = c(rep(0, 89), rep(1, 89)), page = rep(seq(1, 89, 1), 2), 
                        value = append(partners$ageshare * partners$femratio * partners$wsw_rate, partners$ageshare * partners$femratio * (1 - partners$wsw_rate)), 
                        agent=rep("A", 178))

# MALE AGENTS AS AGENT A
male_agent_A <- data.frame(csex = rep(1, 178), cage = rep(seq(1, 89, 1), 2), 
                        psex = c(rep(1, 89), rep(0, 89)), page = rep(seq(1, 89, 1), 2), 
                        value = append(partners$ageshare * (1 - partners$femratio) * partners$msm_rate, partners$ageshare * (1 - partners$femratio) * (1 - partners$msm_rate)), 
                        agent=rep("A", 178))

### CALCULATE PROBABILITIES OF AGENT Bs (equation 10)

# FEMALE AGENTS AS AGENT B
csex <- NA; cage <- NA; psex <- NA; page <- NA; value <- NA

for (i in 1:N){   # For each age of women
  for (j in 1:N){ # For each age j of the female-creating male, sum probabilities of female created being age i
    csex[(i-1)*N+j] <- 1
    cage[(i-1)*N+j] <- j
    page[(i-1)*N+j] <- i
    psex[(i-1)*N+j] <- 0
    value[(i-1)*N+j] <- partners$ageshare[j] * (1 - partners$femratio[j]) * (1 - partners$msm_rate[j]) * mat.mw[i,j] # Hetero MEN creating WOMEN as partners
  }
}
for (i in (N+1):(2*N)){ # For each age of women 
  for (j in 1:N){       # For each age j of the female-creating female, sum probabilities of female created being age i
    csex[(i-1)*N+j] <- 0
    cage[(i-1)*N+j] <- j
    page[(i-1)*N+j] <- i - N
    psex[(i-1)*N+j] <- 0
    value[(i-1)*N+j] <- partners$ageshare[j] * partners$femratio[j] * partners$wsw_rate[j] * mat.ww[(i-N),j] # Homosexual WOMEN creating WOMEN as partners
  }
}

female_agent_B <- data.frame(csex = as.factor(csex), cage = cage, psex = as.factor(psex), page = page, value = value)
female_agent_B$agent <- "B"

# MALE AGENTS AS AGENT B
csex <- NA; cage <- NA; psex <- NA; page <- NA; value <- NA

for (i in 1:N){     # For each age of men
  for (j in 1:N){   # For each age j of the male-creating female, sum probabilities of male created being age i
    csex[(i-1)*N+j] <- 0
    cage[(i-1)*N+j] <- j
    page[(i-1)*N+j] <- i
    psex[(i-1)*N+j] <- 1
    value[(i-1)*N+j] <- partners$ageshare[j] * partners$femratio[j] * (1 - partners$wsw_rate[j]) * mat.wm[i,j] # Hetero WOMEN creating MEN as partners
  }
}
for (i in (N+1):(2*N)){
  for (j in 1:N){
    csex[(i-1)*N+j] <- 1
    cage[(i-1)*N+j] <- j
    page[(i-1)*N+j] <- i - N
    psex[(i-1)*N+j] <- 1
    value[(i-1)*N+j] <- partners$ageshare[j] * (1 - partners$femratio[j]) * partners$msm_rate[j] * mat.mm[(i-N),j] # Homosexual MEN creating MEN as partners
  }
}
male_agent_B <- data.frame(csex = as.factor(csex), cage = cage, psex = as.factor(psex), page = page, value = value)
male_agent_B$agent <- "B"

### COMBINE ALL PROBABILITIES OF AGENTS As AND Bs
data.c <- rbind(female_agent_A, female_agent_B, male_agent_A, male_agent_B)

summary(data.c)
head(data.c)
# ACTUAL CORRECTION CALCULATION
data.c$value <- data.c$value / sum(data.c$value)
#sum(data.c$value) # the sum over all terminal nodes of the probabilities need to sum to 1.

# Reshape dataset into long-format
data.c$sex <- NA; data.c$age <- NA
data.c$sex[which(data.c$agent == "B")] <- data.c$psex[which(data.c$agent == "B")]
data.c$age[which(data.c$agent == "B")] <- data.c$page[which(data.c$agent == "B")]
data.c$sex[which(data.c$agent == "A")] <- data.c$csex[which(data.c$agent == "A")]
data.c$age[which(data.c$agent == "A")] <- data.c$cage[which(data.c$agent == "A")]
data.c$sexor <- 0
data.c$sexor[which(data.c$csex == data.c$psex)] <- 1
data.c <- data.c[,c("age", "sex", "sexor", "value")]

### START REVERSE AGGREGATION OF VALUES ALONG THE DECISION TREE
step1 <- aggregate(data.c$value, list(age=data.c$age, sex=data.c$sex, sexor=data.c$sexor), sum)
step2 <- aggregate(step1$x, list(age=step1$age, sex=step1$sex), sum)

# CALUCALTE AGE-PROBABILITIES
P_a <- aggregate(step2$x, list(age=step2$age), sum)
P_a <- P_a$x

# CALCULATE GENDER PROBABILITIES
step2w <- step2[which(step2$sex == 0),]
P_w <- step2w$x / P_a

# CALCULATE SEXUAL ORIENTATION
step1ww <- step1[which(step1$sex == 0 & step1$sexor == 1),]
P_ww <- step1ww$x / P_a * P_w
step1mm <- step1[which(step1$sex == 1 & step1$sexor == 1),]
P_mm <- step1mm$x / P_a * (1-P_w)

partners.c <- data.frame(age = c(12:100), ageshare = P_a, femratio = P_w, msm_rate = P_mm, wsw_rate = P_ww)

toc1 <- Sys.time()

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
  # set relationship status
  pop[i,5] <- 0 
  # set value of non-existent partner to zero
  pop[i,6] <- 0
  pop[i,7] <- 0
  pop[i,8] <- 0
  pop[i,9] <- 0
  i <- i + 1
  if (i > S) break
}
### CREATE PARTNER AGENTS
repeat{
  # assign id:
  pop[i,1]<-i
  # sample age:
  pop[i,2] <- sample(partners.c$age, 1, replace = T, prob = partners.c$ageshare)
  # sample sex dependent on age (0:=female, 1:=male):
  pop[i,3] <- sample(c(0,1), 1, replace = T, prob = c(partners.c$femratio[pop[i,2]-11], (1 - partners.c$femratio[pop[i,2]-11]))) 
  # sample sexor dependent on sex
  if (pop[i,3]==0) {pop[i,4] <- sample(c(1,0), 1, replace = T, prob = c(partners.c$wsw_rate[pop[i,2]-11], (1 - partners.c$wsw_rate[pop[i,2]-11])))} 
  if (pop[i,3]==1) {pop[i,4] <- sample(c(1,0), 1, replace = T, prob = c(partners.c$msm_rate[pop[i,2]-11], (1 - partners.c$msm_rate[pop[i,2]-11])))}
  pop[i,5]<-1 
  # assign partner id of the partner to be created in the next steps
  pop[i,6]<-i+1
  # create partner
  # assign id:
  pop[i+1,1]<-i+1
  # sexural orientation is the same as the partners' sexual orientation
  pop[i+1,4] <- pop[i,4]
  # partners sex corresponding to the creators sexual orientation
  if (pop[i,4] == 1) {pop[i+1,3] <- pop[i,3]}
  if (pop[i,4] == 0) {pop[i+1,3] <- abs(1 - pop[i,3])}
  # partners age corresponding to the dependent age distribution dependent on sex and sexual orientation:
  if (pop[i+1,3] == 0 & pop[i+1,4] == 1){pop[i+1,2] <- sample(c(12:100), 1, replace = T, mat.ww[,pop[i,2]-11])}
  if (pop[i+1,3] == 0 & pop[i+1,4] == 0){pop[i+1,2] <- sample(c(12:100), 1, replace = T, mat.wm[,pop[i,2]-11])}
  if (pop[i+1,3] == 1 & pop[i+1,4] == 0){pop[i+1,2] <- sample(c(12:100), 1, replace = T, mat.mw[,pop[i,2]-11])}
  if (pop[i+1,3] == 1 & pop[i+1,4] == 1){pop[i+1,2] <- sample(c(12:100), 1, replace = T, mat.mm[,pop[i,2]-11])}
  # partner is always in a relationship
  pop[i+1,5]<-1 
  # partner information:
  pop[i+1,6]<-pop[i,1]
  # partner age
  pop[i+1,7] <- pop[i,2]
  pop[i,7] <- pop[i+1,2]
  # partner sex
  pop[i+1,8] <- pop[i,3]
  pop[i,8] <- pop[i+1,3]
  # partner sexor
  pop[i+1,9] <- pop[i,4]
  pop[i,9] <- pop[i+1,4]  
  i<-i+2
  if (i > (X-1)) break
}
toc2 <- Sys.time()

simtime1 <- toc1 - tic
simtime2 <- toc2 - tic
simtime1;simtime2

pop <- as.data.frame(pop)

##### PLOT RESULT #####
### GET REAL POPULATION
data1 <- read.csv(paste(path,'/Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/GER_pop.csv', sep=""))
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

ggsave(paste(path, "Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/AGEPYR_aca.pdf", sep=""), device="pdf", height=5.75, width=4, units='in', scale=2)


library(ggplot2)
data <- data.frame(Time = c(66.1776, 5348), Method= c("ACA", "Matching"))
p <- ggplot(data=data, aes(x=Method, y=Time)) + geom_bar(stat="identity") + coord_flip()
p <- p + scale_y_continuous("Time in seconds", breaks=c(0, 900, 1800, 2700, 3600, 4500))
p
ggsave(paste(path, "Dropbox/1_Projekt_Syphilis/2_ABM_model/6_start_pop/timecomp.pdf", sep=""), device="pdf", height=1, width=4, units='in', scale=2)

