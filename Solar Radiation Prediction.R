library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)  
library(corrplot)
library(ggfortify)

#### Load data ####
df <- read.csv("SolarPrediction.csv")

## no scientific notation
options(scipen = 999)

###################### step1 Inspection and data cleaning ######################

##### Inspection ####

##Summary
head(df,500)
summary(df)
# UNIXTime              Data               Time             Radiation        Temperature  
# Min.   :1472724008   Length:32686       Length:32686       Min.   :   1.11   Min.   :34.0  
# 1st Qu.:1475546498   Class :character   Class :character   1st Qu.:   1.23   1st Qu.:46.0  
# Median :1478026070   Mode  :character   Mode  :character   Median :   2.66   Median :50.0  
# Mean   :1478047265                                         Mean   : 207.12   Mean   :51.1  
# 3rd Qu.:1480480128                                         3rd Qu.: 354.24   3rd Qu.:55.0  
# Max.   :1483264501                                         Max.   :1601.26   Max.   :71.0  
# Pressure        Humidity      WindDirection.Degrees.     Speed        TimeSunRise       
# Min.   :30.19   Min.   :  8.00   Min.   :  0.09         Min.   : 0.000   Length:32686      
# 1st Qu.:30.40   1st Qu.: 56.00   1st Qu.: 82.23         1st Qu.: 3.370   Class :character  
# Median :30.43   Median : 85.00   Median :147.70         Median : 5.620   Mode  :character  
# Mean   :30.42   Mean   : 75.02   Mean   :143.49         Mean   : 6.244                     
# 3rd Qu.:30.46   3rd Qu.: 97.00   3rd Qu.:179.31         3rd Qu.: 7.870                     
# Max.   :30.56   Max.   :103.00   Max.   :359.95         Max.   :40.500                     
# TimeSunSet       
# Length:32686      
# Class :character  
# Mode  :character 

##correlation
cor(df[,4:9])
#                          Radiation Temperature    Pressure     Humidity WindDirection.Degrees.       Speed
# Radiation               1.00000000  0.73495476  0.11901566 -0.226170647           -0.230323549  0.07362687
# Temperature             0.73495476  1.00000000  0.31117348 -0.285054954           -0.259421188 -0.03145814
# Pressure                0.11901566  0.31117348  1.00000000 -0.223973259           -0.229009974 -0.08363929
# Humidity               -0.22617065 -0.28505495 -0.22397326  1.000000000           -0.001833315 -0.21162367
# WindDirection.Degrees. -0.23032355 -0.25942119 -0.22900997 -0.001833315            1.000000000  0.07309242
# Speed                   0.07362687 -0.03145814 -0.08363929 -0.211623673            0.073092422  1.00000000

corrplot(cor(df[,4:9]), method = "number")
corrplot(cor(df[,4:9]), method = "circle")

# There seems no multicollinearity


##### cleaning ####
## Change column names
df <- df %>% rename("WindDirection"="WindDirection.Degrees.")


#### Create a new column #### 
## Hour category 
df$Time <-  strptime(df$Time, format="%H:%M:%S")
df <- df %>% mutate(Hour =Time$hour)

## Wind_cat category 
df <- df %>% mutate(Wind_cat =cut(WindDirection, breaks=4))
summary(df)

## Sunshine (1= during sunshine)
df$TimeSunRise <-  strptime(df$TimeSunRise, format="%H:%M:%S")
df$TimeSunSet <-  strptime(df$TimeSunSet, format="%H:%M:%S")
df <- df %>% mutate(Sunshine = ifelse(Time > TimeSunRise & Time < TimeSunSet, 1, 0))
df$Sunshine <- as.factor(df$Sunshine)


## Check the relation between Radiation and Sunshine
summary(df[df$Sunshine==0,])
summary(df[df$Sunshine==1,])

p1 <- ggplot(df, aes(x=Sunshine,y=Radiation)) + geom_boxplot() + ggtitle("Frequency of Radiation") +
        theme(plot.title = element_text(hjust = 0.5)) 
p2 <- ggplot(df,aes(x=Hour, y=Radiation, colour=Sunshine)) + geom_point() + 
        scale_colour_brewer(palette = "Set1") + ggtitle("Radiation vs Hour") +
        theme(plot.title = element_text(hjust = 0.5)) 
grid.arrange(p1, p2, nrow = 1)
remove(p1,p2)

summary(df[df$Sunshine==0,c(4,14)])
summary(df[df$Sunshine==1,c(4,14)])
boxplot(df$Radiation[df$Sunshine==0])

## Remove all Sunshine==0 observations 
## because Radiation is completely related to sunshine==1
df <- df[df$Sunshine==1,]
df <- df[,1:13]
summary(df)

#### Wind Direction ####
ggplot(df, aes(x=Wind_cat,y=Radiation)) + geom_boxplot() + ggtitle("Radiation by four types of wind direction")+
        theme(plot.title = element_text(hjust = 0.5)) 

ggplot(df, aes(x=WindDirection)) + geom_histogram() + ggtitle("Frequency of WindDirection") +
        theme(plot.title = element_text(hjust = 0.5)) 

x <- df[,c(8,4)]
k_2 <- kmeans(x,2)
k_4 <- kmeans(x,4)

x_2 <- cbind(x,k_2$cluster)
x_2 <- x_2 %>%  rename("cluster" ="k_2$cluster")
ggplot(x_2, aes(x=WindDirection, y=Radiation, colour=as.factor(cluster))) +geom_point() + 
        ggtitle("Radiation vs WindDirection by two clusters") + theme(plot.title = element_text(hjust = 0.5)) 

x_4 <- cbind(x,k_4$cluster)
x_4 <- x_4 %>%  rename("cluster" ="k_4$cluster")
ggplot(x_4, aes(x=WindDirection, y=Radiation, colour=as.factor(cluster))) +geom_point()+ 
        ggtitle("Radiation vs WindDirection by four clusters") + theme(plot.title = element_text(hjust = 0.5)) 

#### scatter plots ####
p3 <- ggplot(df, aes(x=Temperature, y=Radiation)) + geom_point() + ggtitle("Radiation vs Temperature") +
        theme(plot.title = element_text(hjust = 0.5)) 

p4 <- ggplot(df,aes(x=Pressure, y=Radiation)) + geom_point() + ggtitle("Radiation vs Pressure") +
        theme(plot.title = element_text(hjust = 0.5)) 

p5 <- ggplot(df,aes(x=Humidity, y=Radiation)) + geom_point() + ggtitle("Radiation vs Humidity") +
        theme(plot.title = element_text(hjust = 0.5)) 

p6 <- ggplot(df,aes(x=Speed, y=Radiation)) + geom_point() + ggtitle("Radiation vs Speed") +
        theme(plot.title = element_text(hjust = 0.5)) 

grid.arrange(p3, p4, p5 ,p6, nrow = 2, ncol=2)

remove(p3, p4, p5 ,p6)

#### histograms ####
ggplot(df, aes(x=Radiation)) + geom_histogram() + ggtitle("Frequency of Radiation") +
        theme(plot.title = element_text(hjust = 0.5)) 
ggplot(df, aes(x=Pressure)) + geom_histogram() + ggtitle("Frequency of Pressure") +
        theme(plot.title = element_text(hjust = 0.5)) 
ggplot(df, aes(x=Humidity)) + geom_histogram() + ggtitle("Frequency of Humidity") +
        theme(plot.title = element_text(hjust = 0.5)) 
ggplot(df, aes(x=Speed)) + geom_histogram() + ggtitle("Frequency of Speed") +
        theme(plot.title = element_text(hjust = 0.5)) 




##### Multivariable regresssion model ####

# Split data set into training set and test set
n <- nrow(df)  
ntrain <- round(n*0.6)    
set.seed(314)             
tindex <- sample(n, ntrain) 

train_df <- df[tindex,]  
test_df <- df[-tindex,]  

# Multivariable regression by step by step function

lm_max <- lm(Radiation ~ Temperature+Pressure+Humidity+Speed, data=train_df)
lm_min <- lm(Radiation ~ 1, data=train_df)
step(lm_max,direction = "both", scope=list(upper=lm_max, lower=lm_min))

lm1 <- lm_max
summary(lm1)
# Call:
#         lm(formula = Radiation ~ Temperature + Pressure + Humidity + 
#                    Speed, data = train_df)
# 
# Residuals:
#         Min      1Q  Median      3Q     Max 
# -728.98 -176.75  -13.77  173.60  990.19 
# 
# Coefficients:
#               Estimate Std. Error t value             Pr(>|t|)    
# (Intercept) -5521.3410  1626.5639  -3.394              0.00069 ***
# Temperature    33.8812     0.5387  62.897 < 0.0000000000000002 ***
# Pressure      133.3872    53.6296   2.487              0.01289 *  
# Humidity       -0.5493     0.1231  -4.463           0.00000819 ***
# Speed          10.9950     0.7498  14.664 < 0.0000000000000002 ***
#         ---
# Signif. codes:  0 e***f 0.001 e**f 0.01 e*f 0.05 e.f 0.1 e f 1
# 
# Residual standard error: 253.4 on 9360 degrees of freedom
# Multiple R-squared:  0.4272,	Adjusted R-squared:  0.427 
# F-statistic:  1745 on 4 and 9360 DF,  p-value: < 0.00000000000000022


# Diagnostic plots
autoplot(lm1)

## Test the train model using test_df
predict1 <- predict(lm1, newdata=test_df)
cor(predict1, test_df$Radiation)

# [1] 0.6646391
# Not high correlation


###################### step2 see per day data ######################

#### Aggregate by day #### 
class(df$TimeSunSet - df$TimeSunRise)
head(df$TimeSunSet - df$TimeSunRise, 500)

df_avg <- group_by(df,Data) %>% summarise(Total_Radiation=sum(Radiation), Avg_Temp = mean(Temperature),
                                          Avg_Pressure =mean(Pressure), Avg_Humidity = mean(Humidity),
                                          Avg_Speed = mean(Speed), SunHours= TimeSunSet -TimeSunRise) %>% arrange(Total_Radiation) 

df_avg$SunHours <-  as.numeric(df_avg$SunHours)

##### Inspection ####
head(df_avg)
summary(df_avg)
# Data               Total_Radiation    Avg_Temp      Avg_Pressure  
# Length:15608       Min.   : 2145   Min.   :44.96   Min.   :30.23  
# Class :character   1st Qu.:43465   1st Qu.:51.77   1st Qu.:30.40  
# Mode  :character   Median :62183   Median :55.54   Median :30.43  
#                    Mean   :58975   Mean   :55.07   Mean   :30.42  
#                    3rd Qu.:75820   3rd Qu.:58.41   3rd Qu.:30.45  
#                    Max.   :92708   Max.   :64.45   Max.   :30.52  
# Avg_Humidity      Avg_Speed         SunHours    
# Min.   : 19.56   Min.   : 2.695   Min.   :10.93  
# 1st Qu.: 59.87   1st Qu.: 5.398   1st Qu.:11.05  
# Median : 80.50   Median : 6.076   Median :11.43  
# Mean   : 73.89   Mean   : 6.285   Mean   :11.52  
# 3rd Qu.: 93.08   3rd Qu.: 6.843   3rd Qu.:11.93  
# Max.   :101.97   Max.   :14.981   Max.   :12.52  


cor(df_avg[,2:7])
#                 Total_Radiation   Avg_Temp Avg_Pressure Avg_Humidity  Avg_Speed    SunHours
# Total_Radiation       1.0000000  0.7046691    0.3377711  -0.63886952  0.1827536  0.29813402
# Avg_Temp              0.7046691  1.0000000    0.5234518  -0.68092799 -0.1224500  0.54844194
# Avg_Pressure          0.3377711  0.5234518    1.0000000  -0.28296527 -0.3095100  0.33316110
# Avg_Humidity         -0.6388695 -0.6809280   -0.2829653   1.00000000 -0.1786026  0.03338728
# Avg_Speed             0.1827536 -0.1224500   -0.3095100  -0.17860263  1.0000000 -0.32214703
# SunHours              0.2981340  0.5484419    0.3331611   0.03338728 -0.3221470  1.00000000

hist(df_avg$Total_Radiation)


##### Moltivariable regresssion model ####
# Split data set into training set and test set
n2 <- nrow(df_avg)  
ntrain2 <- round(n2*0.6)    
set.seed(314)             
tindex2 <- sample(n2, ntrain2) 

train_df_avg <- df_avg[tindex2,]  
test_df_avg <- df_avg[-tindex2,]  


lm_av_max <- lm(Total_Radiation ~ Avg_Temp+Avg_Pressure+Avg_Humidity+Avg_Speed+SunHours, data=train_df_avg)
lm_av_min <- lm(Total_Radiation ~ 1, data=train_df_avg)
step(lm_av_max,direction = "both", scope=list(upper=lm_av_max, lower=lm_av_min))

lm2 <- lm_av_max
summary(lm2)
# Call:
#         lm(formula = Total_Radiation ~ Avg_Temp + Avg_Pressure + Avg_Humidity + 
#                    Avg_Speed + SunHours, data = train_df_avg)
# 
# Residuals:
#         Min     1Q Median     3Q    Max 
# -41547  -9539   1986  10441  27915 
# 
# Coefficients:
#                Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)  -933873.01  107761.29  -8.666 < 0.0000000000000002 ***
# Avg_Temp        2034.35      72.24  28.163 < 0.0000000000000002 ***
# Avg_Pressure   26848.61    3566.62   7.528   0.0000000000000564 ***
# Avg_Humidity    -274.49      11.92 -23.028 < 0.0000000000000002 ***
# Avg_Speed       2998.30      92.14  32.542 < 0.0000000000000002 ***
# SunHours        5661.17     461.58  12.265 < 0.0000000000000002 ***
#         ---
# Signif. codes:  0 e***f 0.001 e**f 0.01 e*f 0.05 e.f 0.1 e f 1
# 
# Residual standard error: 13870 on 9359 degrees of freedom
# Multiple R-squared:  0.5936,	Adjusted R-squared:  0.5933 
# F-statistic:  2734 on 5 and 9359 DF,  p-value: < 0.00000000000000022

## Test the train model using test_df
predict2 <- predict(lm2, newdata=test_df_avg)
cor(predict2, test_df_avg$Total_Radiation)
# [1] 0.7713145
# better correlation

# Diagnostic plots
autoplot(lm2)


