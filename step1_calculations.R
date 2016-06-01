
###### READ IN DATA #####

### POPULATION DATA ###
data <- read.csv2("https://github.com/stefanscholz8/agent-creates-agent/raw/master/data.csv", header=T)
head(data)


##### SECTION 2.2: PROBABILITIES FOR SINGLE AGENTS #####

### CALCULATE AGE DISTRIBUTION OF SINGLES (equation 5)
age.s <- (data$femratio * (1 - data$relw_share) * data$ageshare + (1 - data$femratio) * (1 - data$relm_share) * data$ageshare) / sum(data$femratio * (1 - data$relw_share) * data$ageshare + (1 - data$femratio) * (1 - data$relm_share) * data$ageshare)
### CALCULATE GENDER GIVEN AGE OF SINGLES (equation 6)
sex.s.f <- data$femratio * (1 - data$relw_share) * data$ageshare / (data$femratio * (1 - data$relw_share) * data$ageshare + (1 - data$femratio) * (1 - data$relm_share) * data$ageshare)
### CALCULATE SEXUAL ORIENTATION GIVEN AGE AND GENDER OF SINGLES (equation 7)
wsw.s <- data$femratio * (1 - data$relw_share) * data$ageshare * data$wsw_rate / (sex.s.f * data$femratio * data$ageshare)
msm.s <- (1 - data$femratio) * (1 - data$relm_share) * data$ageshare * data$msm_rate / ((1 - sex.s.f) * (1 - data$femratio) * data$ageshare)

### CREATE DATAFRAME WITH VALUES FOR SINGLES
singles <- data.frame(age=c(0:100), ageshare = age.s, femratio = sex.s.f, msm_rate = msm.s, wsw_rate =wsw.s)

# SAVE VALUES FOR SINGLES (ALREADY IN THE GITHUB REPOSITORY)
# write.csv(singles, "https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/singles.csv", row.names=F)

##### SECTION 2.3: PROBABILITIES FOR PARTNER AGENTS #####

### CALCULATE AGE DISTRIBUTION OF PARTNER (adapted equation 5)
age.p <- (data$femratio * data$relw_share * data$ageshare + (1 - data$femratio) * data$relm_share * data$ageshare) / sum(data$femratio * data$relw_share * data$ageshare + (1 - data$femratio) * data$relm_share * data$ageshare)
### CALCULATE GENDER GIVEN AGE OF PARTNER (adapted equation 6)
sex.p.f <- data$femratio * data$relw_share * data$ageshare / (data$femratio * data$relw_share * data$ageshare + (1 - data$femratio) * data$relm_share * data$ageshare)
### CALCULATE SEXUAL ORIENTATION GIVEN AGE AND GENDER OF PARTNER (adapted equation 7)
wsw.p <- data$femratio * data$relw_share * data$ageshare * data$wsw_rate / (sex.s.f * data$femratio * data$ageshare)
msm.p <- (1 - data$femratio) * data$relm_share * data$ageshare * data$msm_rate / ((1 - sex.s.f) * (1 - data$femratio) * data$ageshare)

### CREATE DATAFRAME WITH VALUES FOR RELATIONSHIPS
partners <- data.frame(age=c(0:100), ageshare = age.p, femratio = sex.p.f, msm_rate = msm.p, wsw_rate =wsw.p)
# SELECT AGENTS ABOVE MINIMUM AGE FOR RELATIONSHIPS
partners <- partners[13:101, ]

# SAVE VALUES FOR RELATIONSHIPS (ALREADY IN THE GITHUB REPOSITORY)
# write.csv(partners, "https://raw.githubusercontent.com/stefanscholz8/agent-creates-agent/master/partners.csv", row.names=F)

