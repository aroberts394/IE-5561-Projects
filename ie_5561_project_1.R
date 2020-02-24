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
higher_ed <- read_csv("/Volumes/GoogleDrive/My Drive/IE 5561 Projects/Project 1/Data/Higher_Ed.csv", col_types = cols(BIRYR = col_factor(),                                                                     GENDER = col_factor(),
                                  MINRTY = col_factor(),                                                                    RACETH = col_factor(), 
                                  DGRDG = col_factor(),
                                  OCEDRLP = col_factor(),
                                  NOCPRMG = col_factor()))
# rename the dataset
ed <- higher_ed
# attach the Glaucoma dataset to the environment
attach(ed)
# check for missing values
sum(is.na(ed))

# create a validation set 70% for training set
set.seed(123) 
validation_index <- createDataPartition(ed$SALARY, p = 0.7, list = FALSE)

# select 20% of the data for validation
validation <- ed[-validation_index,]

# use the remaining 80% of data to training and testing the models
ed <- ed[validation_index,]

# display the structure of the data
str(ed)

# dimensions of dataset
dim(ed)

#check frequency of the factor variables
freq(ed)

# summarize all attribute distributions
summary(ed)

#check distributions of the variables
plot_num(ed)

# split input and output
x <- ed[,1:10]
y <- ed[,11]

# visualize salary wrt to gender, minority class and race
ed %>%
  group_by(GENDER) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    n       = n()
  )
ggplot(ed, aes(x = GENDER, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()

ed %>%
  group_by(MINRTY) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    n       = n()
  )
ggplot(ed, aes(x = MINRTY, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()

ed %>%
  group_by(RACETH) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    n       = n()
  )

ggplot(ed, aes(x = RACETH, y = SALARY)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) +
  coord_flip()


                      ### DATA PREPROCESSING ###


                    ### MODEL BUILDING AND EVALUATION ###


                    ### MODEL TUNING AND SELECTION ###


