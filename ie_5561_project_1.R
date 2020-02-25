## IE 5561 Project 1 ##

# install required packages
install.packages('TH.data')
install.packages("tidyverse")
install.packages("caret")
install.packages("funModeling")
install.packages("corrplot")

# load required libraries
require(TH.data)      # for glaucoma data set
require(tidyverse)    # for data manipulation and graphics 
require(caret)        # for resampling and model training
require(funModeling)  # for useful data exploration tools
require(corrplot)     # for correlation matrices


                      ### EXPLORATORY DATA ANALYSIS ###
# load in dataset
higher_ed <- read_csv("/Volumes/GoogleDrive/My Drive/IE 5561 Projects/Project 1/Data/Higher_Ed.csv", 
                      col_types = cols(BIRYR = col_factor(),
                                       GENDER = col_factor(),
                                       MINRTY = col_factor(),
                                       RACETH = col_factor(), 
                                       DGRDG = col_factor(),
                                       OCEDRLP = col_factor(),
                                       NOCPRMG = col_factor()))


# rename the dataset
ed <- higher_ed

# attach the Glaucoma dataset to the environment
attach(ed)

# rename factor levels
ed$GENDER <- plyr::revalue(GENDER, c("1" = "Female", "2" = "Male"))
ed$MINRTY <- plyr::revalue(MINRTY, c("0" = "No", "1" = "Yes"))
ed$RACETH <- plyr::revalue(RACETH, c("1" = "Asian", "2" = "White", "3" = "Minorities", "4"  = "Other"))
ed$DGRDG <-  plyr::revalue(DGRDG, c("1" = "Bachelors", "2" = "Masters", "3" = "Doctorate", "4" = "Professional"))
ed$OCEDRLP <- plyr::revalue(OCEDRLP, c("1" = "Closely related", "2" = "Somewhat related", "3" = "Not related"))
ed$NOCPRMG <- plyr::revalue(NOCPRMG, c("1" = "Computer Scientists", "2" = "Life Scientists", "3" = "Physical scientists", "4" = "Social Scientists", "5" = "Engineers", "6" = "Science Related", "7" = "Non-Scientists"))


# check for missing values
sum(is.na(ed))

# create a validation set 70% for training set
set.seed(123) 
validation_index <- createDataPartition(ed$SALARY, p = 0.7, list = FALSE)

# select 20% of the data for validation
validation <- ed[-validation_index,]

# use the remaining 70% of data to training and testing the models
ed <- ed[validation_index,]

# display the structure of the data
str(ed)

# dimensions of dataset
dim(ed)

#check frequency of the factor variables
freq(ed)

# summarize all attribute distributions
summary(ed)

#visualize distributions of the continous variables
plot_num(ed)

# split input and output
x <- ed[,1:10]
y <- ed[,11]

# exploring categorical variables
# visualize salary wrt to gender, minority factor, race, job type
ggplot(ed, aes(SALARY)) +
  geom_histogram()

ed %>%
  group_by(GENDER) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
p1 <- ggplot(ed, aes(x = GENDER, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()

ed %>%
  group_by(MINRTY) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
p2 <- ggplot(ed, aes(x = MINRTY, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar)

ed %>%
  group_by(RACETH) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    n       = n()
  )

p3 <- ggplot(ed, aes(x = RACETH, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()

ed %>%
  group_by(NOCPRMG) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    n       = n()
  )
p4 <- ggplot(ed, aes(x = NOCPRMG, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

# assessing normality assumption of salary
par(mfrow = c(1, 2))
# non-log transformed
qqnorm(SALARY, main = "Untransformed\nNormal Q-Q Plot")
qqline(SALARY)
# log transformed
qqnorm(log(SALARY), main = "Log Transformed\nNormal Q-Q Plot")
qqline(log(SALARY))

fm <- lm(SALARY~., data = ed)
summary(fm)

engineers <- ed %>%
  filter(NOCPRMG == "Engineers")

ggplot(engineers, aes(GENDER, SALARY)) + 
  geom_boxplot()

engineers %>%
  group_by(GENDER) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )


                      ### DATA PREPROCESSING ###



                      ### MODEL BUILDING AND EVALUATION ###


                      ### MODEL TUNING AND SELECTION ###


