## IE 5561 Project 1 ##

# install required packages
install.packages("tidyverse")
install.packages("caret")
install.packages("funModeling")
install.packages("corrplot")
install.packages("scales")
install.packages("forcats")

# load required libraries
require(tidyverse)    # for data manipulation and graphics 
require(caret)        # for resampling and model training
require(funModeling)  # for useful data exploration tools
require(corrplot)     # for correlation matrices
require(scales)       # for plot scaling functions
require(forcats)      # for modifying factor plots

                      ### EXPLORATORY DATA ANALYSIS ###
# load in dataset
higher_edu <- read_csv("/Users/aroberts394/Desktop/IE-5561-Projects/Project-1/Higher_Ed.csv")


# factorizing categorical variables
higher_ed <- higher_edu %>%
  mutate(GENDER = factor(GENDER),
         BIRYR = factor(BIRYR),
         MINRTY = factor(MINRTY), 
         RACETH = factor(RACETH),
         DGRDG = factor(DGRDG), 
         OCEDRLP = factor(OCEDRLP), 
         NOCPRMG = factor(NOCPRMG),
         NOCPR = factor(NOCPR))
  
# rename the dataset
ed <- higher_ed

# attach the higher ed dataset to the environment
attach(ed)

# check for missing values
sum(is.na(ed))

# display the structure of the data
str(ed)

# dimensions of dataset
dim(ed)

# summarize all attribute distributions
summary(ed)

##exploring all numeric variables
#correlations of all numeric variables
numericVars <- which(sapply(ed, is.numeric)) #index numeric variables
all_numVar <- ed[, numericVars]
(cor_numVar <- cor(all_numVar, method = "pearson", use = "complete.obs"))

#correlation plot
corrplot(cor_numVar, type = "upper", order = "hclust",
         method = "color",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", 
         insig = "blank")

#visualize distributions of the continous variables
plot_num(ed)

# denisty plots of Salaries, Age and Weight
p1 <- ggplot(ed, aes(SALARY)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 5000, color = "grey30", fill = "white") + 
  geom_density(alpha = .2, fill = "antiquewhite3")

p2 <- ggplot(ed, aes(AGE)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey30", fill = "white") + 
  geom_density(alpha = .2, fill = "antiquewhite3")

p3 <- ggplot(ed, aes(WEIGHT)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey30", fill = "white") + 
  geom_density(alpha = .2, fill = "antiquewhite3")

gridExtra::grid.arrange(p1, p2, p3, nrow = 2)


# salary as a function of age, weights boxplots
(p4 <- ggplot(ed[!is.na(ed$SALARY),], 
             aes(x=factor(AGE), y=SALARY))+
  geom_boxplot(col='blue') + labs(x='Age') +
  scale_y_continuous(breaks= seq(0, 200000, by=50000), 
                     labels = comma) +
  scale_x_discrete(breaks = seq(0, 80, by = 5)))

# boxplot of salary
p5 <- ggplot(ed, aes("var", SALARY)) +
  geom_boxplot(outlier.alpha = .25) +
  scale_y_continuous(
    labels = scales::dollar, 
    breaks = quantile(ed$SALARY)
  )

# boxplot of Age
p6 <- ggplot(ed, aes("var", AGE)) +
  geom_boxplot(outlier.alpha = 0.25 ) +
  scale_y_continuous(
    labels = scales::number,
    breaks = quantile(ed$AGE)
  )

# boxplot of weight
p7 <- ggplot(ed, aes("var", WEIGHT)) +
  geom_boxplot(outlier.alpha = 0.25) +
  scale_y_continuous(
    labels = scales::number,
    breaks = quantile(ed$WEIGHT)
  )

# combine and arrange overall boxplots
gridExtra::grid.arrange(p1, p2, p3, p5, p6, p7, nrow = 2)


# exploring categorical variables
ed2 <- ed
# rename factor levels for understanding
ed2$GENDER <- plyr::revalue(GENDER, c("1" = "Female", "2" = "Male"))
ed2$MINRTY <- plyr::revalue(MINRTY, c("0" = "No", "1" = "Yes"))
ed2$RACETH <- plyr::revalue(RACETH, c("1" = "Asian", "2" = "White", "3" = "Minorities", "4"  = "Other"))
ed2$DGRDG <-  plyr::revalue(DGRDG, c("1" = "Bachelors", "2" = "Masters", "3" = "Doctorate", "4" = "Professional"))
ed2$OCEDRLP <- plyr::revalue(OCEDRLP, c("1" = "Closely related", "2" = "Somewhat related", "3" = "Not related"))
ed2$NOCPRMG <- plyr::revalue(NOCPRMG, c("1" = "Computer Scientists", "2" = "Life Scientists", "3" = "Physical scientists", "4" = "Social Scientists", "5" = "Engineers", "6" = "Science Related", "7" = "Non-Scientists"))


#check frequency of the factor variables
freq(ed2)

# plot boxplot of salary vs. gender 
med_gender <- ed2 %>%
  group_by(GENDER) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
p8 <- ggplot(ed2, aes(x = GENDER, y = SALARY)) +
  geom_boxplot() +
  geom_text(data = med_gender, aes(GENDER, med_sal, 
                                   label = dollar(med_sal)),
            position = position_dodge(width = 0.8), 
            size = 3, vjust = -0.5) +
  scale_y_continuous(labels = scales::dollar) 

# plot boxplot of salary vs. minority class
med_minrty <- ed2 %>%
  group_by(MINRTY) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
p9 <- ggplot(ed2, aes(x = MINRTY, y = SALARY)) +
  geom_boxplot() +
  geom_text(data = med_minrty, aes(MINRTY, med_sal, 
                                   label = dollar(med_sal)),
            position = position_dodge(width = 0.8), 
            size = 3, vjust = -0.5) +
  scale_y_continuous(labels = scales::dollar)

# plot boxplot of salary vs. race
med_race <- ed2 %>%
  group_by(RACETH) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
p10 <- ggplot(ed2, aes(x = RACETH, y = SALARY)) +
  geom_boxplot() +
  geom_text(data = med_race, aes(RACETH, med_sal, 
                                   label = dollar(med_sal)),
            position = position_dodge(width = 0.8), 
            size = 3, vjust = -0.5) +
  scale_y_continuous(labels = scales::dollar)

# plot boxplot of salary vs. occupational group
med_group <- ed2 %>%
  group_by(NOCPRMG) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
(p11 <- ggplot(ed2, aes(x = NOCPRMG, y = SALARY)) +
  geom_boxplot() +
  geom_text(data = med_group, aes(NOCPRMG, med_sal, 
                                 label = dollar(med_sal)),
            position = position_dodge(width = 0.8), 
            size = 3, vjust = -0.5) +
  scale_y_continuous(labels = scales::dollar) +
    coord_flip())

# plot salary vs. degree type
med_degree <- ed2 %>%
  group_by(DGRDG) %>%
  summarise(
    avg_sal = mean(SALARY),
    sd_sal  = sd(SALARY),
    med_sal = median(SALARY),
    n       = n()
  )
p12 <- ggplot(ed2, aes(x = DGRDG, y = SALARY)) +
    geom_boxplot() +
    geom_text(data = med_degree, aes(DGRDG, med_sal, 
                                    label = dollar(med_sal)),
              position = position_dodge(width = 0.8), 
              size = 3, vjust = -0.5) +
    scale_y_continuous(labels = scales::dollar)

#combine the plots together
gridExtra::grid.arrange(p8, p9, p10, p12)

# plot of gender count by occupation group
p13 <- ggplot(ed2, aes(x=fct_infreq(NOCPRMG), 
                       fill = GENDER)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(0.9),
            size = 3, hjust = 1) +
  coord_flip()

# plot salary of gender per occupation group
p14 <- ggplot(ed2, aes(x=fct_infreq(NOCPRMG),
                       y=SALARY,
                       color = GENDER)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar,
                     breaks = seq(0, 150000, 25000))

# combine plots of gender occupation and salary
gridExtra::grid.arrange(p13, p14, nrow = 2)

# plot of gender count by degree type 
(p15 <- ggplot(ed2, aes(x=fct_infreq(DGRDG), fill = GENDER)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(0.9),
            size = 3, hjust = 1) +
  coord_flip())

# plot of most likely job by gender
p16 <- ggplot(ed2, aes(x=GENDER)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(0.9),
            size = 3, hjust = 1) +
  facet_wrap(~ OCEDRLP) +
  coord_flip()

# plot of most likely job by occupation
p17 <- ggplot(ed2, aes(x=NOCPRMG)) +
  geom_bar(position = "dodge") +
  geom_text(stat='count', aes(label=..count..), 
            position = position_dodge(0.9),
            size = 3, hjust = 1) +
  facet_wrap(~ OCEDRLP) +
  coord_flip()

gridExtra::grid.arrange(p16, p17, nrow = 2)



                      ### DATA PREPROCESSING ###
# split input and output
x <- ed[,1:10]
y <- ed[,11]

# create a validation set 70% for training set
set.seed(123) 
validation_index <- createDataPartition(ed$SALARY, p = 0.7, list = FALSE)

# select 20% of the data for validation
validation <- ed[-validation_index,]

# use the remaining 70% of data to training and testing the models
ed <- ed[validation_index,]



                      ### MODEL BUILDING AND EVALUATION ###


                      ### MODEL TUNING AND SELECTION ###


