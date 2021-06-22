# install.packages("tidyr")
# install.packages("dplyr") 
#setwd("D:/Private/Studia/advanced-stat/project/developer_survey_2020")
raw_data <- read.csv("D:/Private/Studia/advanced-stat/project/developer_survey_2020/survey_results_public.csv")
raw_data = subset(raw_data, select=c(Hobbyist, Age, Age1stCode, ConvertedComp, Country, LanguageWorkedWith, EdLevel, Gender, JobSat, NEWEdImpt, NEWJobHunt, NEWJobHuntResearch, NEWLearn, NEWOvertime, OrgSize, WorkWeekHrs, YearsCode, YearsCodePro))

# Removing missing data rows
data <- raw_data[complete.cases(raw_data), ]


##########
######### Transform language column into multiple columns
##########

# Add ; at the end
data$LanguageWorkedWith <- paste0(data$LanguageWorkedWith,';')


# Create a vector with languages column names.
languages <- c("JavaScript", "Python", "Kotlin", "C++", "Swift", "Dart")

# Create columns for selected languages
for(language in languages) {
	language_sep = paste0(language, ";")
	data[[language]] <- ifelse(grepl(language_sep, data$LanguageWorkedWith), "1", "0")
}

########
######## Replace values
########

data$YearsCodePro[data$YearsCodePro=='Less than 1 year'] <- 0
data$YearsCodePro[data$YearsCodePro=='More than 50 years'] <- 51

data$YearsCode[data$YearsCode=='Less than 1 year'] <- 0
data$YearsCode[data$YearsCode=='More than 50 years'] <- 51

data$Age1stCode[data$Age1stCode=='Younger than 5 years'] <- 4
data$Age1stCode[data$Age1stCode=='Older than 85'] <- 51

# Remove extreme age
data = data[!(data$Age>120),]
data = data[!(data$Age<15),]


# Remove extremes working hours
data = data[!(data$WorkWeekHrs>130),]
data = data[!(data$WorkWeekHrs<20),]

data$Gender[!(data$Gender=='Man' | data$Gender=='Woman')] <- 'non-binary'


########
######## Transform categorical string columns into categorical numeric strings
########

categorical_columns <- c("Country", "EdLevel", "Gender", "JobSat", "NEWEdImpt", "NEWLearn", "NEWOvertime", "OrgSize")
for(cat_column in categorical_columns) {
	categories = unique(data[[cat_column]])
 	data[cat_column] = as.numeric(factor(data[[cat_column]], levels=categories))
}

########
########
########

require(ca)
##########
########## CA orgSize vs JobSat
##########

orgSize <- rep(1,nrow(data)) 
orgSize = data[,"OrgSize"]
orgSize.names = unique(raw_data[complete.cases(raw_data), ][["OrgSize"]])

jobSat = data[,"JobSat"]
jobSat.names = unique(raw_data[complete.cases(raw_data), ][["JobSat"]])

jobSat.orgSize <- table(jobSat, orgSize) 

colnames(jobSat.orgSize) <- orgSize.names
rownames(jobSat.orgSize) <- jobSat.names

my.ca <- ca(jobSat.orgSize)
plot(my.ca, main="CA of a job satisfaction and an organization size", mass=c(TRUE,TRUE))

##########
########## CA JobSat vs hobbist vs country
##########

country = data[,"Country"]
#country = country[!(country>30)]
hobby = data[, "Hobbyist"]

country_hobby <- paste(country,hobby, sep="")

jobSat = data[,"JobSat"]
jobSat.names = unique(raw_data[complete.cases(raw_data), ][["JobSat"]])

country_hobby.jobSat <- table(country_hobby, jobSat)

my.ca <- ca(country_hobby.jobSat)
plot(my.ca, main="CA of a job satisfaction and country&hobby", mass=c(TRUE,TRUE))



##########
##########
##########



# Create column 'Java' and write 1 if Java appears in the LanguageWorkedWith column, 0 otherwise.
raw_data$Java <- ifelse(grepl("Java;", raw_data$LanguageWorkedWith), "1", "0")

# Removing specific column
raw_data$Java <- NULL

# Remove last character in a specific column.
raw_data$LanguageWorkedWith = substr(raw_data$LanguageWorkedWith, 1, nchar(raw_data$LanguageWorkedWith)-1)