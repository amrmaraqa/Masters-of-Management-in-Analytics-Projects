IMDB_data=read.csv("D:/Study/McGill University - MMA/Courses/MGSC 661/Team Projects/Midterm Project/IMDB_data.csv")
attach(IMDB_data)


require(lmtest)
require(plm)
library(car)
library(ggplot2)
library(splines)


# action
reg_action_1 = lm(imdbScore~poly(action,1))
summary(reg_action_1)
residualPlot(reg_action_1, col= "grey")


# adventure
reg_adventure_1 = lm(imdbScore~poly(adventure,1))
summary(reg_adventure_1)


# scifi
reg_scifi_1 = lm(imdbScore~poly(scifi,1))
summary(reg_scifi_1)


# thriller
reg_thriller_1 = lm(imdbScore~poly(thriller,1))
summary(reg_thriller_1)


# musical
reg_musical_1 = lm(imdbScore~poly(musical,1))
summary(reg_musical_1)


# romance
reg_romance_1 = lm(imdbScore~poly(romance,1))
summary(reg_romance_1)


# western
reg_western_1 = lm(imdbScore~poly(western,1))
summary(reg_western_1)


# sport
reg_sport_1 = lm(imdbScore~poly(sport,1))
summary(reg_sport_1)


# horror
reg_horror_1 = lm(imdbScore~poly(horror,1))
summary(reg_horror_1)


# drama
reg_drama_1 = lm(imdbScore~poly(drama,1))
summary(reg_drama_1)
# 11 percent preditive power. I think we should include it.


# war
reg_war_1 = lm(imdbScore~poly(war,1))
summary(reg_war_1)


# animation
reg_animation_1 = lm(imdbScore~poly(animation,1))
summary(reg_animation_1)


# crime
reg_crime_1 = lm(imdbScore~poly(crime,1))
summary(reg_crime_1)

# The rest of the genres possess a very low predictive power
# So, we will only consider drama as a predictor for our model


### nbNewsArticles
require(ggplot2)

#STEP2
boxplot(nbNewsArticles)
hist(nbNewsArticles)

plot = ggplot(IMDB_data, aes(y = imdbScore, x = nbNewsArticles))
scatter = geom_point(color = 'black')
plot + scatter
# most less than 100K, several clear outliers

lm_nbNewsArticles1 = lm(imdbScore~nbNewsArticles)
summary(lm_nbNewsArticles1)
# p value = <2e-16 , adjusted r squared = 0.05034 
# seems like a good predictor


#STEP3
#Non linearity Tests
reg1=lm(imdbScore~nbNewsArticles)
summary(reg1)
plot(nbFaces,imdbScore)
residualPlots(reg1)
#P value = < 2.2e-16, non linear

#check which polydegree is best
reg2=lm(imdbScore~poly(nbNewsArticles,2))
reg3=lm(imdbScore~poly(nbNewsArticles,3))
reg4=lm(imdbScore~poly(nbNewsArticles,4))
reg5=lm(imdbScore~poly(nbNewsArticles,5))
reg6=lm(imdbScore~poly(nbNewsArticles,6))
reg7=lm(imdbScore~poly(nbNewsArticles,7))
reg8=lm(imdbScore~poly(nbNewsArticles,8))
reg9=lm(imdbScore~poly(nbNewsArticles,9))

anova(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9)
# Degree 5 is the optimal degree for nbNewsArticles
# it does not make sense because 2000 to the power of 5 would probably make the prediction out of the range


# heteroskedasticity
residualPlot(lm_nbNewsArticles1, quadratic = FALSE) 
ncvTest(lm_nbNewsArticles1) 
# p value = p = < 2.22e-16 so the variable is heteroskedastic

table(nbNewsArticles)

# testing divide it by 100 
IMDB_data$NumArticles = IMDB_data$nbNewsArticles/100
attach(IMDB_data)

reg_NumArticles = lm(imdbScore~nbNewsArticles)
summary(reg_NumArticles)
# It might work
reg_NumArticles5 = lm(imdbScore~poly(NumArticles, 5))
summary(reg_NumArticles5)
# might consider but not sure


# trying splines
library(splines)
k1 = quantile(nbNewsArticles, 0.25)
k2 = quantile(nbNewsArticles, 0.5)
k3 = quantile(nbNewsArticles, 0.75)

lm_NewsArticles_spline = lm(imdbScore~bs(nbNewsArticles, knots = c(k1,k2,k3), degree = 1))
summary(lm_NewsArticles_spline)
# adjusted r squared = 0.1196 


# Since the adjusted r squared is less than the polynomial, a fifth degree polynomial would be the optimal choice 
# Any polynomial of degree more than 3 would perform better than a spline


#1 Movie Budget does not need 
IMDB_data$Budget_Millions=(IMDB_data$movieBudget/1000000)
attach(IMDB_data)
boxplot(Budget_Millions)
plot(movieBudget,imdbScore) #very bad plot, not showing any speicifc rs


lm_budget=lm(imdbScore~Budget_Millions,data=IMDB_data)
summary(lm_budget)  #p value is very low=0.00054 so there is a rs between them, and adjusted r2 is 0.56%, very low

#Test for nonlinearity
residualPlots(lm_budget) #p=29%, shows lineairyt

#Hetero: 
ncvTest(lm_budget) #P is 2.94e-06 heteroskedasticity
plot(lm_budget)
abline(lm_budget,lty = 2, col = "red")
coeftest(lm_budget, vcov=vcovHC(lm_budget, type="HC1"))


#2 ReleaseDay does not need
attach(IMDB_data)
boxplot(releaseDay)
plot(releaseDay, imdbScore) #veyr bad, just points scattered


lm_releaseDay=lm(imdbScore~releaseDay,data=IMDB_data)
summary(lm_releaseDay)  #p value is very high=0.35, no rs between y and x and r2 is negative  -6.315e-05

#Test for nonlinearity
residualPlots(lm_releaseDay) #p=29%, shows lineairyt weird?


#3 ReleaseMonth needs:
IMDB_data$month1 = ifelse(IMDB_data$releaseMonth == 'Jan', 1, 0)
IMDB_data$month2 = ifelse(IMDB_data$releaseMonth == 'Feb', 1, 0)
IMDB_data$month3 = ifelse(IMDB_data$releaseMonth == 'Mar', 1, 0)
IMDB_data$month4 = ifelse(IMDB_data$releaseMonth == 'Apr', 1, 0)
IMDB_data$month5 = ifelse(IMDB_data$releaseMonth == 'May', 1, 0)
IMDB_data$month6 = ifelse(IMDB_data$releaseMonth == 'Jun', 1, 0)
IMDB_data$month7 = ifelse(IMDB_data$releaseMonth == 'Jul', 1, 0)
IMDB_data$month8 = ifelse(IMDB_data$releaseMonth == 'Aug', 1, 0)
IMDB_data$month9 = ifelse(IMDB_data$releaseMonth == 'Sep', 1, 0)
IMDB_data$month10 = ifelse(IMDB_data$releaseMonth == 'Oct', 1, 0)
IMDB_data$month11 = ifelse(IMDB_data$releaseMonth == 'Nov', 1, 0)
IMDB_data$month12 = ifelse(IMDB_data$releaseMonth == 'Dec', 1, 0)
attach(IMDB_data)

#we cant plot this i think

reg_12months = lm(imdbScore~
                    month1+month2+month3+month4+month5+month6+month7+month8+month9+month10+
                    month11+month12)
summary(reg_12months)   #p value is 4.56e-07 and adjusted r2 is 2%

#linearity:
residualPlots(reg_12months) #all of them are >10%, so linear

#hetero 
ncvTest(reg_12months) #P is 0.005 heteroskedasticity
plot(reg_12months)
abline(reg_12months,lty = 2, col = "blue")
coeftest(reg_12months, vcov=vcovHC(reg_12months, type="HC1"))


#4 Release Year
IMDB_data$ReleaseYear_2010 = ifelse(IMDB_data$releaseYear >= 2010, 1, 0)
IMDB_data$ReleaseYear_2000 = ifelse(IMDB_data$releaseYear >= 2000 & IMDB_data$releaseYear < 2010, 1, 0)
IMDB_data$ReleaseYear_1990 = ifelse(IMDB_data$releaseYear >= 1990 & IMDB_data$releaseYear < 2000, 1, 0)
IMDB_data$ReleaseYear_1980 = ifelse(IMDB_data$releaseYear >= 1980 & IMDB_data$releaseYear < 1990, 1, 0)
IMDB_data$ReleaseYear_1970 = ifelse(IMDB_data$releaseYear >= 1970 & IMDB_data$releaseYear < 1980, 1, 0)
IMDB_data$ReleaseYear_1960 = ifelse(IMDB_data$releaseYear >= 1960 & IMDB_data$releaseYear < 1970, 1, 0)
IMDB_data$ReleaseYear_1950 = ifelse(IMDB_data$releaseYear >= 1950 & IMDB_data$releaseYear < 1960, 1, 0)
IMDB_data$ReleaseYear_1940 = ifelse(IMDB_data$releaseYear >= 1940 & IMDB_data$releaseYear < 1950, 1, 0)
IMDB_data$ReleaseYear_1930 = ifelse(IMDB_data$releaseYear < 1940, 1, 0)

attach(IMDB_data)

reg_9years = lm(imdbScore~
                  ReleaseYear_2010+ReleaseYear_1930+
                  ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+
                  ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000)
summary(reg_9years)


#linearity:
residualPlots(reg_9years) #all of them are >10%, so linear in the tukey test

#hetero
ncvTest(reg_9years) #P is 0.02<0.05 heteroskedasticity
plot(reg_9years)

coeftest(reg_9years, vcov=vcovHC(reg_9years, type="HC1"))


#5 duration
attach(IMDB_data)
plot(duration,imdbScore)#looks like most movies are in the 100-150 min mark zone

lm_duration=lm(imdbScore~duration)
summary(lm_duration)  #adjusted r2 is 16.8% and p is 2.2e-16

#nonlinearity:
residualPlots(lm_duration) #not linear at allvery low P, 2e-11

#heteo:
ncvTest(lm_duration) #p very small, fix it
coeftest(lm_duration, vcov=vcovHC(lm_duration, type="HC1"))
summary(lm_duration)


#check degree of duration
lm_duration1=lm(imdbScore~duration)
lm_duration2=lm(imdbScore~poly(duration,2))
lm_duration3=lm(imdbScore~poly(duration,3))
lm_duration4=lm(imdbScore~poly(duration,4))
lm_duration5=lm(imdbScore~poly(duration,5))
anova(lm_duration1,lm_duration2,lm_duration3,lm_duration4,lm_duration5)
anova(lm_duration1,lm_duration2,lm_duration4)
# Second degree is the optimal degree


### nbFaces ###
attach(IMDB_data)

#Visualize nbFaces
plot = ggplot(IMDB_data, aes(y=imdbScore, x=nbFaces))
scatter = geom_point(color = 'black')
plot+scatter

lm_faces1 = lm(imdbScore~nbFaces)
summary(lm_faces1) #sig, p = 8.4e-5, and 0.75% r2


#Test for non-linearity
residualPlots(lm_faces1)
# p = 0.08. not linear. Run anova and find degree. Run spline.

#Test for heteroskedasticity
residualPlot(lm_faces1, quadratic = FALSE) #visually, hetero exists
ncvTest(lm_faces1) #shows no heteroskedasticity (p = 0.77 > 0.05)

#Anova Test
lm_faces2 = lm(imdbScore~poly(nbFaces,2))
lm_faces3 = lm(imdbScore~poly(nbFaces,3))
lm_faces4 = lm(imdbScore~poly(nbFaces,4))
lm_faces5 = lm(imdbScore~poly(nbFaces,5))
lm_faces6 = lm(imdbScore~poly(nbFaces,6))

anova(lm_faces1,lm_faces2,lm_faces3,lm_faces4,lm_faces5,lm_faces6) # p^2 = 0.063 and p^6 = 0.079
anova(lm_faces1,lm_faces2,lm_faces6) #the move from 2nd to 6th is not significant.
#Hence, nbFaces is poly 2nd degree. But move to 2nd degree doesn't seem that significant p^2 > 0.05
# So, try with splines

summary(lm_faces2) # p = 3.946e-5 and r2 = 0.945% (poly^2 without outliers)

#linear spline regressions
k1 = quantile(nbFaces, 0.25)
k2 = quantile(nbFaces, 0.5)
k3 = quantile(nbFaces, 0.75)

lm_faces_spline = lm(imdbScore~bs(nbFaces, knots = c(k1,k2,k3), degree = 1))
summary(lm_faces_spline) # spline has lower significance and lower r2. 

#Conclusion: best nbFaces seems to be poly^2. nbFaces dont seem to be a strong predictor to include'''


### Movie Meter ###

lm_moviemeter = lm(imdbScore~movieMeter_IMDBpro)
summary(lm_moviemeter)
# sig p = 7.897e-5 and 0.7537% r2

#test for non-linearity
residualPlots(lm_moviemeter) # p = 3.824e-15 therefore non-linear

#Anova Test
lm_movMeter2 = lm(imdbScore~poly(movieMeter_IMDBpro,2))
lm_movMeter3 = lm(imdbScore~poly(movieMeter_IMDBpro,3))
lm_movMeter4 = lm(imdbScore~poly(movieMeter_IMDBpro,4))
lm_movMeter5 = lm(imdbScore~poly(movieMeter_IMDBpro,5))
lm_movMeter6 = lm(imdbScore~poly(movieMeter_IMDBpro,6))

anova(lm_moviemeter,lm_movMeter2,lm_movMeter3,lm_movMeter4,lm_movMeter5,lm_movMeter6)
#degree keeps increasing. Maybe spline

#Test for heteroskedasticity
residualPlots(lm_moviemeter, quadratic = FALSE) #visually hetero
ncvTest(lm_moviemeter) #shows heteroskedasticity p = 0.00013 (p < 0.05)

coeftest(lm_moviemeter, vcov=vcovHC(lm_moviemeter, type = 'HC1'))
#After fixing hetero, movieMeter is WAY less important p = 0.01228


#Conclusion: movieMeter is non-linear but r2 is very low


require(psych)
quantvars=IMDB_data[, c(5,6,8,9,15)]
corr_matrix=cor(quantvars)
round(corr_matrix,3)


# Trying to add all numerical variables and find an optimal model.

mreg_1 = lm(imdbScore~poly(movieBudget,1)+poly(duration,2)+poly(nbNewsArticles,5)+poly(drama,1))
summary(mreg_1)

mreg_2 = lm(imdbScore~poly(movieBudget,1)+poly(duration,2)+poly(nbNewsArticles,5)+poly(drama,1)+poly(movieMeter_IMDBpro, 8))
summary(mreg_2)
# No increase in the predictive power; so, let's not include movieMeter

mreg_3 = lm(imdbScore~poly(movieBudget,1)+poly(duration,2)+poly(nbNewsArticles,5)+poly(drama,1)+poly(releaseYear,3))
summary(mreg_3)
# Best predictive performance


# Checking collinearity among numerical predictors
quantvars_new=IMDB_data[, c(5,8,9,15,31)]
corr_matrix_new=cor(quantvars_new)
round(corr_matrix_new,3)
# No collinearity but also no improvement in the predictive power!!


mreg_4 = lm(imdbScore~poly(movieBudget,1)+poly(duration,2)+poly(drama,1), poly(releaseYear,3))
summary(mreg_4)
# Not a choice for elimination


mreg_5 = lm(imdbScore~poly(duration,2)+poly(nbNewsArticles,5)+poly(drama,1), poly(releaseYear,3))
summary(mreg_5)
# Not a choice for elimination


mreg_6 = lm(imdbScore~poly(movieBudget,1)+poly(nbNewsArticles,5)+poly(drama,1), poly(releaseYear,3))
summary(mreg_6)
# Not a choice for elimination


mreg_7 = lm(imdbScore~poly(movieBudget,1)+poly(duration,2)+poly(nbNewsArticles,5)+poly(releaseYear,3))
summary(mreg_7)
# Not a choice for elimination


mreg_8 = lm(imdbScore~poly(movieBudget,1)+poly(duration,2)+poly(nbNewsArticles,5)+poly(drama,1)+poly(releaseYear,3)+poly(movieMeter_IMDBpro, 8))
summary(mreg_8)

anova(mreg_3, mreg_8)
# mreg_8 seems like a better model but overfitting might become a concern


### Ironically, we learned non-linearity would make our predictions exceed the range!
# So, we will keep predictors linear


reg_quant = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro)
summary(reg_quant)
# We will add categorical variables to this model

# Adding categorical variables

### maturityRating
table(maturityRating)

plot = ggplot(IMDB_data, aes(y = imdbScore, x = maturityRating))
scatter = geom_point(color = 'black')
plot + scatter
# PG, PG-13, and R represent almost all the dataset

# dummifying maturityRating
IMDB_data$MaturityRating_PG = ifelse(IMDB_data$maturityRating == "PG", 1, 0)
IMDB_data$MaturityRating_PG13 = ifelse(IMDB_data$maturityRating == "PG-13", 1, 0)
IMDB_data$MaturityRating_R = ifelse(IMDB_data$maturityRating == "R", 1, 0)
attach(IMDB_data)

lm_MaturityRating = lm(imdbScore~MaturityRating_PG+MaturityRating_PG13+MaturityRating_R)
summary(lm_MaturityRating)
# adjusted r squared = 0.03059 < 0.05
# not a good predictor



### country

table(country)
# USA and UK represent almost all the data


plot = ggplot(IMDB_data, aes(y = imdbScore, x = country))
scatter = geom_point(color = 'black')
plot + scatter

# dummifying country
IMDB_data$countryUSA = ifelse(IMDB_data$country == "USA", 1, 0)
IMDB_data$countryUK = ifelse(IMDB_data$country == "UK", 1, 0)
attach(IMDB_data)

lm_Country = lm(imdbScore~countryUSA+countryUK)
summary(lm_Country)
# adjusted r squared = 0.01581 < 0.05
# not a good predictor



### colourFilm

table(colourFilm)
# Most films are of categiry "Color"


plot = ggplot(IMDB_data, aes(y = imdbScore, x = colourFilm))
scatter = geom_point(color = 'black')
plot + scatter

# dummifying colourFilm
IMDB_data$color = ifelse(IMDB_data$colourFilm == "Color", 1, 0)
attach(IMDB_data)

lm_Color = lm(imdbScore~color)
summary(lm_Color)
# adjusted r squared = 0.02536 < 0.05
# not a good predictor



### cinematographer

table(cinematographer)
# since there are hundreds of distinct values, visualization does not seem helpful


# dummifying cinematographer
# The frequency of appearance in the data may not be a good basis for dummification
# So, we obtained a list of top 30 cinematographers from "Ranker"
# The upcoming dummification for this variable is based on the mentioned list of top cinematographers

top_30_cinematographers <- list("John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore",
                                "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
                                "Russell Carpenter","Philippe Rousselot","Amir Mokri","Tom Stern","Stephen F. Windon","John Mathieson","Seamus McGarvey","Caleb Deschanel","Guillermo Navarro",
                                "John Seale","Lawrence Sher")
IMDB_data$top30cinematographer = ifelse(IMDB_data$cinematographer %in% top_30_cinematographers, 1,0)
attach(IMDB_data)
reg_with30cinematographers = lm(imdbScore~top30cinematographer)
summary(reg_with30cinematographers)
# adjusted r squared = 0.01512
# It is too low but when we dummify the variable using the as.factor function
# the variable seems like a better predictor

# So, we try to modify our dummification
top_20_cinematographers <- list("John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore",
                                "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
                                "Russell Carpenter")
IMDB_data$top20cinematographer = ifelse(IMDB_data$cinematographer %in% top_20_cinematographers, 1,0)
attach(IMDB_data)
reg_with20cinematographers = lm(imdbScore~top20cinematographer)
summary(reg_with20cinematographers)
# adjusted r squared = 0.01079 which is less than the previous trial
# including less cinematographers might not be helpful
# We will try with 10 to make sure


top_10_cinematographers <- list(
  "John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore"
)
IMDB_data$top10cinematographer = ifelse(IMDB_data$cinematographer %in% top_10_cinematographers, 1,0)
attach(IMDB_data)
reg_with10cinematographers = lm(imdbScore~top10cinematographer)
summary(reg_with10cinematographers)
# adjusted r squared = 0.001762 which is significantly less than previous trials
# So, including less cinematographers may not be helpful
# We go back to 30 cinematographers and try to divide them into 3 groups


cinematographer11_20 <- list(
  "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
  "Russell Carpenter"
)
IMDB_data$cinematographers_11_20 = ifelse(IMDB_data$cinematographer %in% cinematographer11_20, 1,0)
cinematographer21_30 <- list(
  "Philippe Rousselot","Amir Mokri","Tom Stern","Stephen F. Windon","John Mathieson","Seamus McGarvey","Caleb Deschanel","Guillermo Navarro",
  "John Seale","Lawrence Sher"
)
IMDB_data$cinematographers_21_30 = ifelse(IMDB_data$cinematographer %in% cinematographer21_30, 1,0)
attach(IMDB_data)
reg_30cinematographers_3groups = lm(imdbScore~top10cinematographer+cinematographers_11_20+cinematographers_21_30)
summary(reg_30cinematographers_3groups)
# adjusted r squared = 0.01441 which is still less than our first trial
# So, we will include 30 top cinematographers in our model


### productionCompany

table(productionCompany)
# since there are hundreds of distinct values, visualization does not seem helpful


# dummifying productionCompany
# The frequency of appearance in the data may not be a good basis for dummification
# So, we obtained a list of top 50 production companies based on "box office" data
# The upcoming dummification for this variable is based on the mentioned list of top companies

top25_companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment', 'DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films', 'Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment')
IMDB_data$top25companies = ifelse(IMDB_data$productionCompany %in% top25_companies, 1, 0)
attach(IMDB_data)

reg_25companies = lm(imdbScore~IMDB_data$top25companies)
summary(reg_25companies)
# adjusted r squared = 0.0003849 which is very low
# So, we try to add more companies

top50_companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment', 'DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films', 'Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment', 'Touchstone Pictures', 'Illumination Entertainment', 'Working Title Films', 'di Bonaventura Pictures', 'Skydance Productions', 'TSG Entertainment', 'Jerry Bruckheimer', 'Wingnut Films', 'Bad Robot', 'Eon Productions', 'Twentieth Century Fox', 'Perfect World Pictures', '1492 Pictures', 'Fox 2000 Pictures', 'The Kennedy/Marshall Company', 'Imagine Entertainment', 'One Race Films', 'Temple Hill Entertainment', 'Atlas Entertainment', 'Ingenious Film Partners', 'Blumhouse', 'Blue Sky Studios', 'Sony Pictures Animation', 'Syncopy', 'Hasbro Studios')
IMDB_data$top50companies = ifelse(IMDB_data$productionCompany %in% top50_companies, 1, 0)
attach(IMDB_data)

reg_50companies = lm(imdbScore~IMDB_data$top50companies)
summary(reg_50companies)
# adjusted r squared = 4.828e-05 which is less than the previous trial
# So, we try including less companies


top10_companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment')
IMDB_data$top10companies = ifelse(IMDB_data$productionCompany %in% top10_companies, 1, 0)
attach(IMDB_data)
reg_10companies = lm(imdbScore~IMDB_data$top10companies)
summary(reg_10companies)
# adjusted r squared decreased again
# So, we are trying to add more companies

companies_11_20 <- list('DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films')
IMDB_data$companies11_20 = ifelse(IMDB_data$productionCompany %in% companies_11_20, 1, 0)
attach(IMDB_data)
reg_20companies_2group = lm(imdbScore~IMDB_data$top10companies+companies11_20)
summary(reg_20companies_2group)
# adjusted r squared increased so adding more companies and dividing them into different groups might be helpful

# We try to add companies 21-50 within an additional group
companies21_50 <- list('Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment', 'Touchstone Pictures', 'Illumination Entertainment', 'Working Title Films', 'di Bonaventura Pictures', 'Skydance Productions', 'TSG Entertainment', 'Jerry Bruckheimer', 'Wingnut Films', 'Bad Robot', 'Eon Productions', 'Twentieth Century Fox', 'Perfect World Pictures', '1492 Pictures', 'Fox 2000 Pictures', 'The Kennedy/Marshall Company', 'Imagine Entertainment', 'One Race Films', 'Temple Hill Entertainment', 'Atlas Entertainment', 'Ingenious Film Partners', 'Blumhouse', 'Blue Sky Studios', 'Sony Pictures Animation', 'Syncopy', 'Hasbro Studios')
IMDB_data$companies_21_50 = ifelse(IMDB_data$productionCompany %in% companies21_50, 1, 0)
attach(IMDB_data)
reg_50companies_3group = lm(imdbScore~IMDB_data$top10companies+companies11_20+IMDB_data$companies_21_50)
summary(reg_50companies_3group)
# The adjusted r squared decreased
# So, the optimal choice turns out to be including 20 top companies in 2 group (1-10 and 11-20)


#6 language
IMDB_data$language=as.factor(IMDB_data$language) 
attach(IMDB_data)
levels(language)
lm_lang1=lm(imdbScore~language)
summary(lm_lang1)    #p is 0.0228 and adjusted r2=0.7%, throughout all languages, P is very high, close to 1
#Doesnt seem to have a strong relationship with the imdb score.
#Lets dummify it, with english, and the rest 0

residualPlots(lm_lang1)

IMDB_data$lang_english = ifelse(language== "English", 1, 0)
attach(IMDB_data)
lm_lang_eng=lm(imdbScore~lang_english)
summary(lm_lang_eng)     #p is 2.29e-06, very low so it is related. And adjusted r2 is 1.1%, which is higher than before

#Confirm linearity
residualPlots(lm_lang_eng) #P is 72%, so it is linear

#Hetero: 
ncvTest(lm_lang_eng) #P is 0.06>0.05, no heteroskedasticity


### ACTORS ###
# categorize actors based on starMeter (top 25%)

IMDB_data$actor1_25 = ifelse(IMDB_data$actor1_starMeter >= 4665, 1, 0)
IMDB_data$actor2_25 = ifelse(IMDB_data$actor2_starMeter >= 4665, 1, 0)
IMDB_data$actor3_25 = ifelse(IMDB_data$actor3_starMeter >= 4665, 1, 0)

attach(IMDB_data)

lm1 = lm(imdbScore~actor1_25)
lm2 = lm(imdbScore~actor2_25)
lm3 = lm(imdbScore~actor3_25)

summary(lm1) #sig and 0.77% r2
summary(lm2) #a lot less sig than actor1 (p = 0.018) and 0.2367% r2 
summary(lm3) #not sig and negative r2. Check linearity

# Look at how R^2 reacts to adding predictors
lm4 = lm(imdbScore~actor1_25+actor2_25) 
#r2 increased but minimal 0.8% r2. significance of actor2 dropped drastically (drop var?)
lm5 = lm(imdbScore~actor1_25+actor2_25+actor3_25) #actor 3 decreased r2
lm6 = lm(imdbScore~actor1_25+actor3_25) #actor 3 decreased r2

summary(lm4)
summary(lm5)
summary(lm6)

#Test for non-linearity
library(car)
reg=lm(imdbScore~actor1_25+actor2_25+actor3_25)
residualPlots(reg)
residualPlots(lm3)
# They're all linear but together they're non-linear. Idk why the aggregate is non-linear.
# Since they're all linear. Only actor 1 and actor 2 should be kept (with reservation. very small r2)

#Test for heteroskedasticity
regh=lm(imdbScore~actor1_25+actor2_25)
summary(regh) 
residualPlot(regh, quadratic = FALSE)
ncvTest(regh) #shows heteroskedasticity (p < 0.05)
coeftest(regh, vcov=vcovHC(regh, type = 'HC1'))


# dummifying productionCompany
# trying different dummifications / different numbers / different groups
top25companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment', 'DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films', 'Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment')
IMDB_data$top25companies_dummy = ifelse(IMDB_data$productionCompany %in% top25companies, 1, 0)
attach(IMDB_data)

reg_dummy_companies = lm(imdbScore~movieBudget+duration+nbNewsArticles+movieMeter_IMDBpro+top25companies_dummy)
summary(reg_dummy_companies)

reg_test = lm(imdbScore~top25companies_dummy)
summary(reg_test)

top50companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment', 'DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films', 'Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment', 'Touchstone Pictures', 'Illumination Entertainment', 'Working Title Films', 'di Bonaventura Pictures', 'Skydance Productions', 'TSG Entertainment', 'Jerry Bruckheimer', 'Wingnut Films', 'Bad Robot', 'Eon Productions', 'Twentieth Century Fox', 'Perfect World Pictures', '1492 Pictures', 'Fox 2000 Pictures', 'The Kennedy/Marshall Company', 'Imagine Entertainment', 'One Race Films', 'Temple Hill Entertainment', 'Atlas Entertainment', 'Ingenious Film Partners', 'Blumhouse', 'Blue Sky Studios', 'Sony Pictures Animation', 'Syncopy', 'Hasbro Studios')
IMDB_data$top50companies_dummy = ifelse(IMDB_data$productionCompany %in% top50companies, 1, 0)
attach(IMDB_data)

reg_50_companies = lm(imdbScore~movieBudget+duration+nbNewsArticles+movieMeter_IMDBpro+top50companies_dummy)
summary(reg_50_companies)

top10companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment')
IMDB_data$top10companies_dummy = ifelse(IMDB_data$productionCompany %in% top10companies, 1, 0)
attach(IMDB_data)
reg_10_companies = lm(imdbScore~movieBudget+duration+nbNewsArticles+movieMeter_IMDBpro+top10companies_dummy)
summary(reg_10_companies)

companies_11_20 <- list('DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films')
IMDB_data$companies11_20dummy = ifelse(IMDB_data$productionCompany %in% companies_11_20, 1, 0)
attach(IMDB_data)
reg_20_companies = lm(imdbScore~movieBudget+duration+nbNewsArticles+movieMeter_IMDBpro+top10companies_dummy+companies11_20dummy)
summary(reg_20_companies)

reg_year_20_companies = lm(imdbScore~movieBudget+duration+nbNewsArticles+movieMeter_IMDBpro+top10companies_dummy+companies11_20dummy+ReleaseYear_2010)
summary(reg_year_20_companies)

companies_21_50 <- list('Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment', 'Touchstone Pictures', 'Illumination Entertainment', 'Working Title Films', 'di Bonaventura Pictures', 'Skydance Productions', 'TSG Entertainment', 'Jerry Bruckheimer', 'Wingnut Films', 'Bad Robot', 'Eon Productions', 'Twentieth Century Fox', 'Perfect World Pictures', '1492 Pictures', 'Fox 2000 Pictures', 'The Kennedy/Marshall Company', 'Imagine Entertainment', 'One Race Films', 'Temple Hill Entertainment', 'Atlas Entertainment', 'Ingenious Film Partners', 'Blumhouse', 'Blue Sky Studios', 'Sony Pictures Animation', 'Syncopy', 'Hasbro Studios')
IMDB_data$companies_21_50dummy = ifelse(IMDB_data$productionCompany %in% companies_21_50, 1, 0)
attach(IMDB_data)
reg_50_companies_3g = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+companies11_20dummy+companies_21_50dummy+ReleaseYear_2010)
summary(reg_50_companies_3g)


reg_year_companies11_20 = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+companies11_20dummy+ReleaseYear_2010)
summary(reg_year_companies11_20)


# adding directors to the model
# trying different dummifications for directors
top10directors <- list("Stanley Kubrick", "Martin Scorsese","Quentin Tarantino", "Christopher Nolan","David Fincher","Ethan Coen","Joel Coen","Steven Spielberg","Alfred Hitchcock","Francis Ford Coppola")
directors11_20 <- list("Clint Eastwood","John Carpenter","Paul Thomas Anderson","Ridley Scott","Milos Forman")
directors21_30 <- list("Sidney Lumet",
                       "Guillermo del Toro",
                       "Brian de Palma",
                       "Wes Anderson",
                       "Sergio Leone",
                       "Robert Zemeckis",
                       "Mel Brooks",
                       "Peter Jackson",
                       "James Cameron",
                       "George Cukor",
                       "Terry Gilliam",
                       "Elia Kazan",
                       "Billy Wilder",
                       "John Huston",
                       "Frank Capra")
IMDB_data$top10directors_dummy = ifelse(IMDB_data$director %in% top10directors, 1, 0)
IMDB_data$directors11_20_dummy = ifelse(IMDB_data$director %in% directors11_20, 1, 0)
IMDB_data$directors21_30_dummy = ifelse(IMDB_data$director %in% directors21_30, 1, 0)

attach(IMDB_data)
reg_30directors = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+companies11_20dummy+ReleaseYear_2010+top30directors_dummy)
summary(reg_30directors)


reg_top_companies_directors = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+companies11_20dummy+ReleaseYear_2010+top10directors_dummy+directors11_20_dummy+directors21_30_dummy)
summary(reg_top_companies_directors)

reg_quant = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro)
summary(reg_quant)

top20directors=list("Stanley Kubrick", "Martin Scorsese","Quentin Tarantino", "Christopher Nolan","David Fincher","Ethan Coen","Joel Coen","Steven Spielberg","Alfred Hitchcock","Francis Ford Coppola","Clint Eastwood","John Carpenter","Paul Thomas Anderson","Ridley Scott","Milos Forman",
                    "Sidney Lumet",
                    "Guillermo del Toro",
                    "Brian de Palma",
                    "Wes Anderson",
                    "Sergio Leone")
IMDB_data$top20directors_dummy = ifelse(IMDB_data$director %in% top20directors, 1, 0)
attach(IMDB_data)
reg_companies_directors20 = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+companies11_20dummy+ReleaseYear_2010+top20directors_dummy)
summary(reg_companies_directors20)


reg_companies20_directors20 = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                                   companies11_20dummy+ReleaseYear_2010+ReleaseYear_1930+ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000+top20directors_dummy)
summary(reg_companies20_directors20)
coefficients_companies_directors=coef(reg_companies20_directors20)
write.csv(coefficients_companies_directors, "D:/Study/McGill University - MMA/Courses/MGSC 661/Team Projects/Midterm Project/coefficients_comp_dir.csv")


top30directors <- list("Stanley Kubrick", "Martin Scorsese","Quentin Tarantino", "Christopher Nolan","David Fincher","Ethan Coen","Joel Coen","Steven Spielberg","Alfred Hitchcock","Francis Ford Coppola","Clint Eastwood","John Carpenter","Paul Thomas Anderson","Ridley Scott","Milos Forman",
                       "Sidney Lumet",
                       "Guillermo del Toro",
                       "Brian de Palma",
                       "Wes Anderson",
                       "Sergio Leone",
                       "Robert Zemeckis",
                       "Mel Brooks",
                       "Peter Jackson",
                       "James Cameron",
                       "George Cukor",
                       "Terry Gilliam",
                       "Elia Kazan",
                       "Billy Wilder",
                       "John Huston",
                       "Frank Capra"
)
IMDB_data$top30directors_dummy = ifelse(IMDB_data$director %in% top30directors, 1, 0)
attach(IMDB_data)
reg_30directors = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                       companies11_20dummy+ReleaseYear_2010+ReleaseYear_1930+ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000+top30directors_dummy)
summary(reg_30directors)
### THIS IS OUR CURRENT OPTIMAL!


# adding releaseMonth
# trying to dummify be season to reduce the number of additional predictors
winter1 <- list("Nov", "Dec", "Jan")
IMDB_data$winter = ifelse(IMDB_data$releaseMonth %in% winter1, 1, 0)
spring1 <- list("Feb", "Mar", "Apr")
IMDB_data$spring = ifelse(IMDB_data$releaseMonth %in% spring1, 1, 0)
summer1 <- list("May", "Jun", "Jul")
IMDB_data$summer = ifelse(IMDB_data$releaseMonth %in% summer1, 1, 0)
fall1 <- list("Aug", "Sep", "Oct")
IMDB_data$fall = ifelse(IMDB_data$releaseMonth %in% fall1, 1, 0)
attach(IMDB_data)

reg_with_season = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro
                     +ReleaseYear_2010+ReleaseYear_1930+ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000+top10companies_dummy+companies11_20dummy+top30directors_dummy+winter+spring+summer+fall)
summary(reg_with_season)


# trying duration in hours instead of minutes
IMDB_data$duration_hour = IMDB_data$duration/60
attach(IMDB_data)
reg_poly_dur = lm(imdbScore~movieBudget+poly(duration_hour,2)+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                    companies11_20dummy+ReleaseYear_2010+ReleaseYear_1930+ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000+top30directors_dummy)
summary(reg_poly_dur)
coefficients_poly_dur=coef(reg_poly_dur)
write.csv(coefficients_poly_dur, "D:/Study/McGill University - MMA/Courses/MGSC 661/Team Projects/Midterm Project/poly_dur.csv")


# adding cinematographers 
# trying different dummifications for directors
top_30_cinematographers <- list("John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore",
                                "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
                                "Russell Carpenter","Philippe Rousselot","Amir Mokri","Tom Stern","Stephen F. Windon","John Mathieson","Seamus McGarvey","Caleb Deschanel","Guillermo Navarro",
                                "John Seale","Lawrence Sher")

IMDB_data$top30cinematographers_dummy = ifelse(IMDB_data$cinematographer %in% top_30_cinematographers, 1,0)
attach(IMDB_data)
reg_with_30cinematographers = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                                   companies11_20dummy+ReleaseYear_2010+ReleaseYear_1930+ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000+top30directors_dummy+top30cinematographers_dummy)
summary(reg_with_30cinematographers)
# NEW BEST RESULT



reg_optimal = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                   companies11_20dummy+top30directors_dummy+
                   top30cinematographers_dummy)
summary(reg_optimal)


# Eliminating outliers

library("car")
outlierTest(reg_optimal)
IMDB_data_without_outliers_1 = IMDB_data[-c(492,1806,316,192,1581,989), ]
reg_without_outliers_1 = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                              companies11_20dummy+top30directors_dummy+
                              top30cinematographers_dummy, data = IMDB_data_without_outliers_1)
summary(reg_without_outliers_1)
# The improvement is significant; so, we continue eliminating outliers


outlierTest(reg_without_outliers_1)
IMDB_data_without_outliers_2 = IMDB_data_without_outliers_1[-c(12), ]
reg_without_outliers_2 = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                              companies11_20dummy+top30directors_dummy+
                              top30cinematographers_dummy, data = IMDB_data_without_outliers_2)
summary(reg_without_outliers_2)
# The improvement is signifant; so, we continue eliminating outliers


outlierTest(reg_without_outliers_2)
IMDB_data_without_outliers_3 = IMDB_data_without_outliers_2[-c(1123), ]
reg_without_outliers_3 = lm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
                              companies11_20dummy+top30directors_dummy+
                              top30cinematographers_dummy, data = IMDB_data_without_outliers_3)
summary(reg_without_outliers_3)
# The improvement is very minimal; so, we stop eliminating outliers
# The optimal model would be our previous trial 

# This is our best model
# We export the coefficients to make predictions
coefficients_optimal=coef(reg_without_outliers_2)
write.csv(coefficients_optimal, "D:/Study/McGill University - MMA/Courses/MGSC 661/Team Projects/Midterm Project/coefficients_optimal_OutliersRemoved.csv")

# MSE
library(boot)
MSEs = rep(NA, 5)
for (i in 1:5){
  fit=glm(imdbScore~movieBudget+duration+nbNewsArticles+drama+movieMeter_IMDBpro+top10companies_dummy+
            companies11_20dummy+top30directors_dummy+
            top30cinematographers_dummy, data = IMDB_data_without_outliers_2)
  MSEs[i]=cv.glm(IMDB_data_without_outliers_2,fit,K=10)$delta[1]
  
}            

which.min(MSEs)
min(MSEs)
MSEs


"""
The following lines of code display a 2nd attempt at building a prediction model based on the IMDB dataset. The first model performed better, howvever, 
and we stuck with it.

#Step 1: dummify if needed 
#Step 2: visualize quant variables (if dummify visualize)
#Step 3: run individual linear regression (comment in code p-val and adj r2. Also note variables doing well and those not)
#Step 4: Run non-linearity, hetero and outliers tests and get adjusted versions (all of these affect p-val so take note of how p-val changes)
#Step 5: If non-linear, run anova and note optimal degree
#Step 6: Run regression with optimal degree for non-linear variables and note p-val and adj r2. which good/bad
#Step 7: Run spline regression for non-linear of multiple degrees and note p-val and adj r2. good/bad?

imdb=read.csv('C:/Users/m.maraqa/Desktop/MMA courses/Fall 2022/Multivariate Statistical Analysis/Midterm Project/IMDB_data.csv')
attach(imdb)

install.packages('lmtest')
install.packages('plm')

require(lmtest)
require(plm)
library(car)
library(ggplot2)
library(splines)

View(imdb)
names(imdb)

#nbfaces
install.packages("car")
library(car)
library(plm)
library(lmtest)


#1 Movie Budget does not need dummifying 
attach(imdb)
imdb$Budget_Millions=(imdb$movieBudget/1000000) #mill didn't make diff 
attach(imdb)
boxplot(Budget_Millions)
plot(movieBudget,imdbScore) #very bad plot, not showing any speicifc rs


lm_budget=lm(imdbScore~Budget_Millions,data=imdb)
summary(lm_budget)  #p value is very low=0.00054 so there is a rs between them, and adjusted r2 is 0.56%, very low

#Test for nonlinearity
residualPlots(lm_budget) #p=29%, shows linearity

#Hetero: 
ncvTest(lm_budget) #P is 2.94e-06 heteroskedasticity
plot(lm_budget)
abline(lm_budget,lty = 2, col = "red")
coeftest(lm_budget, vcov=vcovHC(lm_budget, type="HC1")) 
#p = 0.0003018. it's lower than before, so stronger rel

'''#Outliers

qqPlot(lm_budget, envelope=list(style='none')) #outliers 316 and 989
imdb_budget_outliers=imdb[-c(989,316),]
attach(imdb_budget_outliers)
lm_lang_budget1=lm(imdbScore~movieBudget,data=imdb_budget_outliers)
summary(lm_lang_budget1) #p us bow 0.0003 and r2 is 0.6%
'''


#2 ReleaseDay does not need dummifying
attach(imdb)
boxplot(releaseDay)
plot(releaseDay, imdbScore) #veyr bad, just points scattered


lm_releaseDay=lm(imdbScore~releaseDay,data=imdb)
summary(lm_releaseDay)  #p value is very high=0.35, no rs between y and x and r2 is negative  -6.315e-05

#Test for nonlinearity
residualPlots(lm_releaseDay) #p=29%, shows linearity 

"""Conclusion: dont include"""

#3 ReleaseMonth needs dummifying:
imdb$month1 = ifelse(imdb$releaseMonth == 'Jan', 1, 0)
imdb$month2 = ifelse(imdb$releaseMonth == 'Feb', 1, 0)
imdb$month3 = ifelse(imdb$releaseMonth == 'Mar', 1, 0)
imdb$month4 = ifelse(imdb$releaseMonth == 'Apr', 1, 0)
imdb$month5 = ifelse(imdb$releaseMonth == 'May', 1, 0)
imdb$month6 = ifelse(imdb$releaseMonth == 'Jun', 1, 0)
imdb$month7 = ifelse(imdb$releaseMonth == 'Jul', 1, 0)
imdb$month8 = ifelse(imdb$releaseMonth == 'Aug', 1, 0)
imdb$month9 = ifelse(imdb$releaseMonth == 'Sep', 1, 0)
imdb$month10 = ifelse(imdb$releaseMonth == 'Oct', 1, 0)
imdb$month11 = ifelse(imdb$releaseMonth == 'Nov', 1, 0)
imdb$month12 = ifelse(imdb$releaseMonth == 'Dec', 1, 0)
attach(imdb)

reg_12months = lm(imdbScore~
                    month1+month2+month3+month4+month5+month6+month7+month8+month9+month10+
                    month11+month12)
summary(reg_12months)   #p value is 4.56e-07 and adjusted r2 is 2%

#linearity:
residualPlots(reg_12months) #all of them are >10%, so linear

#hetero
ncvTest(reg_12months) #P is 0.005 heteroskedasticity
plot(reg_12months)
abline(reg_12months,lty = 2, col = "blue")
coeftest(reg_12months, vcov=vcovHC(reg_12months, type="HC1"))

'''#outlier

qqPlot(reg_12months, envelope=list(style='none')) #outliers 316 and 989

attach(imdb_budget_outliers)
reg_12months_new=lm(imdbScore~ month1+month2+month3+month4+month5+month6+month7+month8+month9+month10+
                      month11+month12,data=imdb_budget_outliers)
summary(reg_12months_new)'''

#p is low 0.0003 and r2 is 0.6% so doesn't sig. Test on final model
'''Conclusion: test on final. doesnt seem sig'''


#4 Release Year

imdb$ReleaseYear_2020 = ifelse(imdb$releaseYear >= 2020, 1, 0)
imdb$ReleaseYear_2010 = ifelse(imdb$releaseYear >= 2010 & imdb$releaseYear < 2020, 1, 0)
imdb$ReleaseYear_2000 = ifelse(imdb$releaseYear >= 2000 & imdb$releaseYear < 2010, 1, 0)
imdb$ReleaseYear_1990 = ifelse(imdb$releaseYear >= 1990 & imdb$releaseYear < 2000, 1, 0)
imdb$ReleaseYear_1980 = ifelse(imdb$releaseYear >= 1980 & imdb$releaseYear < 1990, 1, 0)
imdb$ReleaseYear_1970 = ifelse(imdb$releaseYear >= 1970 & imdb$releaseYear < 1980, 1, 0)
imdb$ReleaseYear_1960 = ifelse(imdb$releaseYear >= 1960 & imdb$releaseYear < 1970, 1, 0)
imdb$ReleaseYear_1950 = ifelse(imdb$releaseYear >= 1950 & imdb$releaseYear < 1960, 1, 0)
imdb$ReleaseYear_1940 = ifelse(imdb$releaseYear >= 1940 & imdb$releaseYear < 1950, 1, 0)
imdb$ReleaseYear_1930 = ifelse(imdb$releaseYear < 1940, 1, 0)

attach(imdb)

reg_9years = lm(imdbScore~
                  ReleaseYear_2010+ReleaseYear_1930+
                  ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+
                  ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000)
summary(reg_9years)
#p = 2.2e-16 and r2 = 4.5%

#linearity:
residualPlots(reg_9years) #all of them are >10%, so linear in the tukey test

#hetero
ncvTest(reg_9years) #P is 0.02<0.05 heteroskedasticity
plot(reg_9years)

coeftest(reg_9years, vcov=vcovHC(reg_9years, type="HC1"))
#

'''#outlier

qqPlot(reg_9years, envelope=list(style='none')) #outliers 1581 and 989 like language so just attach language oytlier dataste
imdb_year_outliers=imdb[-c(1581,989),]
attach(imdb_year_outliers)
reg_9years1 = lm(imdbScore~
                   ReleaseYear_2010+ReleaseYear_1930+
                   ReleaseYear_1940+ReleaseYear_1950+ReleaseYear_1960+ReleaseYear_1970+
                   ReleaseYear_1980+ReleaseYear_1990+ReleaseYear_2000, data=imdb_year_outliers)
summary(reg_9years1)

#p and adjusted r2 same almost as before removing outliers'''

''' Conclusion: We found year2010 to have a p = 0.16. We found that odd and we decided to run each decade 
individually, which resulted in high p vals and negligible adj r2. So, we will test their
addition to the final model.'''

#5 duration
attach(imdb)
plot(duration,imdbScore)#looks like most movies are in the 100-150 min mark zone

lm_duration=lm(imdbScore~duration)
summary(lm_duration)  #adjusted r2 is 16.8% and p is 2.2e-16

#nonlinearity:
residualPlots(lm_duration) #not linear at all very low P, 2e-11

#heteo:
ncvTest(lm_duration) #p very small, fix it
coeftest(lm_duration, vcov=vcovHC(lm_duration, type="HC1"))
summary(lm_duration)

'''#outliertest:
qqPlot(lm_duration, envelope=list(style='none')) #395 and 191 outliers
imdb_duration_outliers=imdb[-c(191,395),]
attach(imdb_duration_outliers)
lm_duration1=lm(imdbScore~duration,data=imdb_duration_outliers)
summary(lm_duration1) #p us bow 2.2e-16 and and r2 is 17%'''

#check degree of duration
lm_duration1=lm(imdbScore~duration,data=imdb_duration_outliers)
lm_duration2=lm(imdbScore~poly(duration,2),data=imdb_duration_outliers)
lm_duration3=lm(imdbScore~poly(duration,3),data=imdb_duration_outliers)
lm_duration4=lm(imdbScore~poly(duration,4),data=imdb_duration_outliers)
lm_duration5=lm(imdbScore~poly(duration,5),data=imdb_duration_outliers)
anova(lm_duration1,lm_duration2,lm_duration3,lm_duration4,lm_duration5)#betwene 2 and 4  

anova(lm_duration1,lm_duration2,lm_duration4) 

summary(lm_duration4) #adjusted r2 is 19.9% and p is 2.2e-16

'''Conc: CHECK DEGREES WITH MSE. between 2 and 4. Best is 4 but maybe better 
to do 2 for less fitting'''

#6 language
imdb$language=as.factor(imdb$language) 
attach(imdb)
levels(language)
lm_lang1=lm(imdbScore~language)
summary(lm_lang1)    
#p is 0.0228 and adjusted r2=0.7%, throughout all languages, P is very high, close to 1
#Doesnt seem to have a strong relationship with the imdb score.
#Lets dummify it, with english, and the rest 0

residualPlots(lm_lang1)

imdb$lang_english = ifelse(language== "English", 1, 0)
attach(imdb)
lm_lang_eng=lm(imdbScore~lang_english)
summary(lm_lang_eng)     
#p is 2.29e-06, very low so it is related. And adjusted r2 is 1.1%, 
#which is higher than before

#Confirm linearity
residualPlots(lm_lang_eng) #P is 72%, so it is linear

#Hetero: 
ncvTest(lm_lang_eng) #P is 0.06>0.05, no heteroskedasticity

'''#Outliers

qqPlot(lm_lang_eng, envelope=list(style='none'))
outlierTest(lm_lang_eng)  #989 and 1581, both in english, remove them

imdb_lang_outliers=imdb[-c(989,1581),]
attach(imdb_lang_outliers)
lm_lang_eng1=lm(imdbScore~lang_english,data=imdb_lang_outliers)
summary(lm_lang_eng1) #p is very low, 2.13e-06, adjusted r2 is 1.11%'''

'''Conc: doesnt seem to affect score that much but include dummified lang for now'''


#country,maturiryrating, nbarticles, colorfilm




#-----------------------------------------------------------------------

### nbNewsArticles
require(ggplot2)

#STEP2
boxplot(nbNewsArticles)
hist(nbNewsArticles)

plot = ggplot(imdb, aes(y = imdbScore, x = nbNewsArticles))
scatter = geom_point(color = 'black')
plot + scatter
# most less than 100K, several clear outliers

imdb$nbNewsArticles = imdb$nbNewsArticles/1000000
attach(imdb)
summary(nbNewsArticles)
lm_nbNewsArticles1 = lm(imdbScore~nbNewsArticles)
summary(lm_nbNewsArticles1)
# p value = <2e-16 , adjusted r squared = 0.05034 = 5.034%, coef = 1.33e-4 
# seems like a good predictor


#STEP3
#Non linearity Tests
reg1=lm(imdbScore~nbNewsArticles)
summary(reg1)
plot(nbNewsArticles,imdbScore)
residualPlots(reg1)
#P value = < 2.2e-16, non linear

#check which polydegree is best
reg2=lm(imdbScore~poly(nbNewsArticles,2))
reg3=lm(imdbScore~poly(nbNewsArticles,3))
reg4=lm(imdbScore~poly(nbNewsArticles,4))
reg5=lm(imdbScore~poly(nbNewsArticles,5))
reg6=lm(imdbScore~poly(nbNewsArticles,6))
reg7=lm(imdbScore~poly(nbNewsArticles,7))
reg8=lm(imdbScore~poly(nbNewsArticles,8))
reg9=lm(imdbScore~poly(nbNewsArticles,9))

anova(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9)
# Degree 5 is the optimal degree for nbNewsArticles
# it does not make sense because 2000 to the power of 5 would probably make 
# the prediction out of the range. If dividing by 10^6, we get in results in range
# So, test with MSEs

#test nbnewarticles with 5th degree 
summary(reg5) #p val = 2.2e-16 and adj r2 increased to 13%

#test nbnewarticles with 3th degree
summary(reg3) #p val = 2.2e-16 and adj r2 increased to 12% 

# heteroskedasticity
residualPlot(lm_nbNewsArticles1, quadratic = FALSE) 
ncvTest(lm_nbNewsArticles1)
# p value = p = < 2.22e-16 so the variable is heteroskedastic
coeftest(lm_nbNewsArticles1, vcov=vcovHC(lm_nbNewsArticles1, type="HC1"))

#Outliers
qqPlot(lm_nbNewsArticles1, envelope=list(style='none'))
outlierTest(lm_nbNewsArticles1)  #989 and 1581, both in english, remove them

imdb_news_out=imdb[-c(492,989,1581),]
detach(imdb)
attach(imdb_news_out)
lm_news_out=lm(imdbScore~nbNewsArticles,data=imdb_news_out)
summary(lm_news_out) #p is very low, 2.2e-16, adjusted r2 increased from 5% to 8.03%

#test nbnewarticles with 5th degree
reg5=lm(imdbScore~poly(nbNewsArticles,5))
summary(reg5) #p val = 2.2e-16 and adj r2 increased to 13%

#test nbnewarticles with 3th degree
reg3=lm(imdbScore~poly(nbNewsArticles,3))
summary(reg3) #p val = 2.2e-16 and adj r2 increased to 12% 

#removing outliers using 1.5*IQR approach >1996.75 and <-1073.25
imdbIQR = imdb
for (i in 1:1930){
  if(imdb$nbNewsArticles[i] > 1996.75){
    imdbIQR=imdbIQR[-c(i),]
  }
}

#run outlier test on new data. no need for this test #####################################
attach(imdbIQR)
lm_nbNewsArticles1 = lm(imdbScore~nbNewsArticles)
qqPlot(lm_nbNewsArticles1, envelope=list(style='none'))
outlierTest(lm_nbNewsArticles1)

summary(lm_nbNewsArticles1)

#test nbnewarticles with 5th degree
reg5=lm(imdbScore~poly(nbNewsArticles,5))
summary(reg5) #p val = 2.2e-16 and adj r2 increased to 13%

#test nbnewarticles with 3th degree
reg3=lm(imdbScore~poly(nbNewsArticles,3))
summary(reg3) #p val = 2.2e-16 and adj r2 increased to 12% 

'''Conclusion: divide nbNews by 10^6 to scale it down. Check degrees in MSE test 
to see optimal degree'''

table(nbNewsArticles)

# testing divide it by 100 
IMDB_data$NumArticles = IMDB_data$nbNewsArticles/100
attach(IMDB_data)

reg_NumArticles = lm(imdbScore~nbNewsArticles)
summary(reg_NumArticles)
# It might work
reg_NumArticles5 = lm(imdbScore~poly(NumArticles, 5))
summary(reg_NumArticles5)
# might consider but not sure


# trying splines
library(splines)
k1 = quantile(nbNewsArticles, 0.25)
k2 = quantile(nbNewsArticles, 0.5)
k3 = quantile(nbNewsArticles, 0.75)

lm_NewsArticles_spline = lm(imdbScore~bs(nbNewsArticles, knots = c(k1,k2,k3), degree = 1))
spline= geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3), degree=1))
plot+scatter+lm_NewsArticles_spline
summary(lm_NewsArticles_spline)
# adjusted r squared = 0.1196 


# Since the adjusted r squared is less than the polynomial, a fifth degree polynomial would be the optimal choice 
# Any polynomial of degree more than 3 would perform better than a spline


### maturityRating
attach(imdb)
table(maturityRating)

boxplot(maturityRating)

plot = ggplot(IMDB_data, aes(y = imdbScore, x = maturityRating))
scatter = geom_point(color = 'black')
plot + scatter

# PG, PG-13, and R represent almost all the dataset

# dummifying maturityRating
imdb$MaturityRating_PG = ifelse(imdb$maturityRating == "PG", 1, 0)
imdb$MaturityRating_PG13 = ifelse(imdb$maturityRating == "PG-13", 1, 0)
imdb$MaturityRating_R = ifelse(imdb$maturityRating == "R", 1, 0)
attach(imdb)

lm_MaturityRating = lm(imdbScore~MaturityRating_PG+MaturityRating_PG13+MaturityRating_R)
summary(lm_MaturityRating)
# p = 1.449e-13 and adjusted r squared = 0.03059

# heteroskedasticity
residualPlot(lm_MaturityRating, quadratic = FALSE) 
ncvTest(lm_MaturityRating)
# p value = 3.23e-5 so the variable is heteroskedastic
coeftest(lm_MaturityRating, vcov=vcovHC(lm_MaturityRating, type="HC1"))
#from hetero test and reg, all except R rated are significant. 

lmPG = lm(imdbScore~MaturityRating_PG)
lmPG13 = lm(imdbScore~MaturityRating_PG13)
lmR = lm(imdbScore~MaturityRating_R)

summary(lmPG) #not sig p = 0.39
summary(lmPG13) # sig and decent r2=2%
summary(lmR) # sig and r2=2%

'''Conc: leave out now. test addition to final model'''

### country

table(country)
# USA and UK represent almost all the data

boxplot(country)

plot = ggplot(imdb, aes(y = imdbScore, x = country))
scatter = geom_point(color = 'black')
plot + scatter

# dummifying country
imdb$countryUSA = ifelse(imdb$country == "USA", 1, 0)
imdb$countryUK = ifelse(imdb$country == "UK", 1, 0)
attach(imdb)

lm_Country = lm(imdbScore~countryUSA+countryUK)
summary(lm_Country)
# p = 7.923e-8 and adjusted r squared = 1.58%


# heteroskedasticity
residualPlot(lm_Country, quadratic = FALSE) 
ncvTest(lm_Country)
# p value = 0.0016 so the variable is heteroskedastic
coeftest(lm_Country, vcov=vcovHC(lm_Country, type="HC1"))

USA = lm(imdbScore~countryUSA)
UK = lm(imdbScore~countryUK)

summary(USA) # sig and r2 = 0.8%
summary(UK) # sig and r2 = 1.6%

'Conc: keep for now. Test addition on final'

### colourFilm

table(colourFilm)
# Most films are of categiry "Color"

boxplot(colourFilm)

plot = ggplot(IMDB_data, aes(y = imdbScore, x = colourFilm))
scatter = geom_point(color = 'black')
plot + scatter

# dummifying colourFilm
imdb$color = ifelse(imdb$colourFilm == "Color", 1, 0)
attach(imdb)

lm_Color = lm(imdbScore~color)
summary(lm_Color)
# p = 1.18e-12 and adjusted r squared = 0.02536 

'Conc; keep for now. test addition'

### cinematographer

table(cinematographer)
# since there are hundreds of distinct values, visualization does not seem helpful


# dummifying cinematographer
# The frequency of appearance in the data may not be a good basis for dummification
# So, we obtained a list of top 30 cinematographers from "Ranker"
# The upcoming dummification for this variable is based on the mentioned list of top cinematographers

top_30_cinematographers <- list("John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore",
                                "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
                                "Russell Carpenter","Philippe Rousselot","Amir Mokri","Tom Stern","Stephen F. Windon","John Mathieson","Seamus McGarvey","Caleb Deschanel","Guillermo Navarro",
                                "John Seale","Lawrence Sher")
imdb$top30cinematographer = ifelse(imdb$cinematographer %in% top_30_cinematographers, 1,0)
attach(imdb)
reg_30cin = lm(imdbScore~top30cinematographer)
summary(reg_30cin)
# p = 3.56e-8 and adjusted r squared = 0.01512
# It is too low but when we dummify the variable using the as.factor function
# the variable seems like a better predictor

# So, we try to modify our dummification
top_20_cinematographers <- list("John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore",
                                "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
                                "Russell Carpenter")
IMDB_data$top20cinematographer = ifelse(IMDB_data$cinematographer %in% top_20_cinematographers, 1,0)
attach(IMDB_data)
reg_with20cinematographers = lm(imdbScore~top20cinematographer)
summary(reg_with20cinematographers)
# adjusted r squared = 0.01079 which is less than the previous trial
# including less cinematographers might not be helpful
# We will try with 10 to make sure


top_10_cinematographers <- list(
  "John Schwartzman","Dean Semler","Don Burgess","Jeremy Lasky","Dariusz Wolski","Dan Mindel","Andrew Lesnie", "Janusz Kaminski", "Trent Opaloch","Mauro Fiore"
)
IMDB_data$top10cinematographer = ifelse(IMDB_data$cinematographer %in% top_10_cinematographers, 1,0)
attach(IMDB_data)
reg_with10cinematographers = lm(imdbScore~top10cinematographer)
summary(reg_with10cinematographers)
# adjusted r squared = 0.001762 which is significantly less than previous trials
# So, including less cinematographers may not be helpful
# We go back to 30 cinematographers and try to divide them into 3 groups


cinematographer11_20 <- list(
  "Dean Cundey","Bill Pope","Robert Richardson","David Tattersall","Ben Davis","Matthew Libatique","Oliver Wood","Roger Deakins","Wally Pfister",
  "Russell Carpenter"
)
IMDB_data$cinematographers_11_20 = ifelse(IMDB_data$cinematographer %in% cinematographer11_20, 1,0)
cinematographer21_30 <- list(
  "Philippe Rousselot","Amir Mokri","Tom Stern","Stephen F. Windon","John Mathieson","Seamus McGarvey","Caleb Deschanel","Guillermo Navarro",
  "John Seale","Lawrence Sher"
)
IMDB_data$cinematographers_21_30 = ifelse(IMDB_data$cinematographer %in% cinematographer21_30, 1,0)
attach(IMDB_data)
reg_30cinematographers_3groups = lm(imdbScore~top10cinematographer+cinematographers_11_20+cinematographers_21_30)
summary(reg_30cinematographers_3groups)
# adjusted r squared = 0.01441 which is still less than our first trial
# So, 

'Conc: we will include 30 top cinematographers in our model'



### productionCompany

table(productionCompany)
# since there are hundreds of distinct values, visualization does not seem helpful


# dummifying productionCompany
# The frequency of appearance in the data may not be a good basis for dummification
# So, we obtained a list of top 50 production companies based on "box office" data
# The upcoming dummification for this variable is based on the mentioned list of top companies

top25_companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment', 'DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films', 'Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment')
IMDB_data$top25companies = ifelse(IMDB_data$productionCompany %in% top25_companies, 1, 0)
attach(IMDB_data)

reg_25companies = lm(imdbScore~IMDB_data$top25companies)
summary(reg_25companies)
# adjusted r squared = 0.0003849 which is very low
# So, we try to add more companies

top50_companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment', 'DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films', 'Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment', 'Touchstone Pictures', 'Illumination Entertainment', 'Working Title Films', 'di Bonaventura Pictures', 'Skydance Productions', 'TSG Entertainment', 'Jerry Bruckheimer', 'Wingnut Films', 'Bad Robot', 'Eon Productions', 'Twentieth Century Fox', 'Perfect World Pictures', '1492 Pictures', 'Fox 2000 Pictures', 'The Kennedy/Marshall Company', 'Imagine Entertainment', 'One Race Films', 'Temple Hill Entertainment', 'Atlas Entertainment', 'Ingenious Film Partners', 'Blumhouse', 'Blue Sky Studios', 'Sony Pictures Animation', 'Syncopy', 'Hasbro Studios')
IMDB_data$top50companies = ifelse(IMDB_data$productionCompany %in% top50_companies, 1, 0)
attach(IMDB_data)

reg_50companies = lm(imdbScore~IMDB_data$top50companies)
summary(reg_50companies)
# adjusted r squared = 4.828e-05 which is less than the previous trial
# So, we try including less companies


top10_companies <- list('Warner Bros.', 'Universal Pictures', 'Columbia Pictures', 'Walt Disney Pictures', 'Marvel Studios', 'Paramount Pictures', '20th Century Fox', 'New Line Cinema', 'Legendary Pictures', 'Dune Entertainment')
IMDB_data$top10companies = ifelse(IMDB_data$productionCompany %in% top10_companies, 1, 0)
attach(IMDB_data)
reg_10companies = lm(imdbScore~IMDB_data$top10companies)
summary(reg_10companies)
# adjusted r squared decreased again
# So, we are trying to add more companies

companies_11_20 <- list('DreamWorks Animation', 'Amblin Entertainment', 'Relativity Media', 'Disney-Pixar', 'Village Roadshow Productions', 'Metro-Goldwyn-Mayer Pictures', 'DreamWorks Pictures', 'Regency Enterprises', 'Lucasfilm', 'Heyday Films')
IMDB_data$companies11_20 = ifelse(IMDB_data$productionCompany %in% companies_11_20, 1, 0)
attach(IMDB_data)
reg_20companies_2group = lm(imdbScore~top10companies+companies11_20)
summary(reg_20companies_2group)
# adjusted r squared increased so adding more companies and dividing them into different groups might be helpful

# We try to add companies 21-50 within an additional group
companies21_50 <- list('Walt Disney Animation Studios', 'Lionsgate', 'RatPac Entertainment', 'Original Film', 'Summit Entertainment', 'Touchstone Pictures', 'Illumination Entertainment', 'Working Title Films', 'di Bonaventura Pictures', 'Skydance Productions', 'TSG Entertainment', 'Jerry Bruckheimer', 'Wingnut Films', 'Bad Robot', 'Eon Productions', 'Twentieth Century Fox', 'Perfect World Pictures', '1492 Pictures', 'Fox 2000 Pictures', 'The Kennedy/Marshall Company', 'Imagine Entertainment', 'One Race Films', 'Temple Hill Entertainment', 'Atlas Entertainment', 'Ingenious Film Partners', 'Blumhouse', 'Blue Sky Studios', 'Sony Pictures Animation', 'Syncopy', 'Hasbro Studios')
IMDB_data$companies_21_50 = ifelse(IMDB_data$productionCompany %in% companies21_50, 1, 0)
attach(IMDB_data)
reg_50companies_3group = lm(imdbScore~top10companies+companies11_20+IMDB_data$companies_21_50)
summary(reg_50companies_3group)
# The adjusted r squared decreased
# So, the optimal choice turns out to be including 20 top companies in 2 group (1-10 and 11-20)

'Conc: exclude production company'

### ACTORS ###
# categorize actors based on starMeter (top 25%)

imdb$actor1_25 = ifelse(imdb$actor1_starMeter >= 4665, 1, 0)
imdb$actor2_25 = ifelse(imdb$actor2_starMeter >= 4665, 1, 0)
imdb$actor3_25 = ifelse(imdb$actor3_starMeter >= 4665, 1, 0)

attach(imdb)

lm1 = lm(imdbScore~actor1_25)
lm2 = lm(imdbScore~actor2_25)
lm3 = lm(imdbScore~actor3_25)

summary(lm1) #sig and 0.77% r2
summary(lm2) #a lot less sig than actor1 (p = 0.018) and 0.2367% r2 
summary(lm3) #not sig and negative r2. Check linearity

# Look at how R^2 reacts to adding predictors
lm4 = lm(imdbScore~actor1_25+actor2_25) 
#r2 increased but minimal 0.8% r2. significance of actor2 dropped drastically (drop var?)
lm5 = lm(imdbScore~actor1_25+actor2_25+actor3_25) #actor 3 decreased r2
lm6 = lm(imdbScore~actor1_25+actor3_25) #actor 3 decreased r2

summary(lm4)
summary(lm5)
summary(lm6)

#Test for non-linearity
library(car)
reg=lm(imdbScore~actor1_25+actor2_25+actor3_25)
residualPlots(reg)
residualPlots(lm3)
# They're all linear but together they're non-linear. Idk why the aggregate is non-linear.
# Since they're all linear. Only actor 1 and actor 2 should be kept (with reservation. very small r2)

#Test for heteroskedasticity
regh=lm(imdbScore~actor1_25+actor2_25)
summary(regh) 
residualPlot(regh, quadratic = FALSE)
ncvTest(regh) #shows heteroskedasticity (p < 0.05)

coeftest(regh, vcov=vcovHC(regh, type = 'HC1'))
#After fixing hetero, I think actor2 should be dropped and only actor 1 kept

#Outlier test
rego=lm(imdbScore~actor1_25)
summary(rego)
qqPlot(rego, envelope = list(style='none')) #316 1581 outliers
outlierTest(rego) #Bonferroni p = 0.038 < 0.05 so outliers exist

#remove outliers
imdb_act_out = imdb[-c(316,1581),]
attach(imdb_act_out)
rego1 = lm(imdbScore~actor1_25)
summary(rego1) 
#sig of actor1 decreased a little (compared to lm1) after removing outliers. p = 3.55e-5
#adj r2 increased from 0.77%, r2 = 0.83% 

''' Conclusion: actors dont seem to be a strong predictor to include. We can try adding 
actor 1_25'''

rego1=lm(imdbScore~actor1_starMeter)
rego2=lm(imdbScore~actor2_starMeter)
rego3=lm(imdbScore~actor3_starMeter)

outlierTest(rego1)
outlierTest(rego2)
outlierTest(rego3)

############################################################################################

### nbFaces ###
attach(imdb)

#Visualize nbFaces
plot = ggplot(imdb, aes(y=imdbScore, x=nbFaces))
scatter = geom_point(color = 'black')
plot+scatter

lm_faces1 = lm(imdbScore~nbFaces)
summary(lm_faces1) #sig, p = 8.4e-5, and 0.75% r2


#Test for non-linearity
residualPlots(lm_faces1)
# p = 0.08. not linear. Run anova and find degree. Run spline.

#Test for heteroskedasticity
residualPlot(lm_faces1, quadratic = FALSE) #visually, hetero exists
ncvTest(lm_faces1) #shows no heteroskedasticity (p = 0.77 > 0.05)

#Outlier test
qqPlot(lm_faces1, envelope = list(style='none')) # 316 and 1581 outliers
outlierTest(lm_faces1) # Benferroni p = 0.036 < 0.05 so outliers exist

#remove outliers
imdb_act_out = imdb[-c(316,1581),] #same dataset for actors becuz same outliers
detach(imdb)
attach(imdb_act_out)
lm_faces1 = lm(imdbScore~nbFaces)
summary(lm_faces1) 
#sig of nbFaces increased a little (compared to lm_faces1) after removing outliers. p = 4.1e-5
#adj r2 increased from 0.75%, r2 = 0.82% 

#Anova Test
lm_faces2 = lm(imdbScore~poly(nbFaces,2))
lm_faces3 = lm(imdbScore~poly(nbFaces,3))
lm_faces4 = lm(imdbScore~poly(nbFaces,4))
lm_faces5 = lm(imdbScore~poly(nbFaces,5))
lm_faces6 = lm(imdbScore~poly(nbFaces,6))

anova(lm_faces1,lm_faces2,lm_faces3,lm_faces4,lm_faces5,lm_faces6) # p^2 = 0.063 and p^6 = 0.079
anova(lm_faces1,lm_faces2,lm_faces6) #the move from 2nd to 6th is not significant.
#Hence, nbFaces is poly 2nd degree. But move to 2nd degree doesn't seem that significant p^2 > 0.05
# So, try with splines

summary(lm_faces2) # p = 3.946e-5 and r2 = 0.945% (poly^2 without outliers)

#linear spline regressions
k1 = quantile(nbFaces, 0.25)
k2 = quantile(nbFaces, 0.5)
k3 = quantile(nbFaces, 0.75)

lm_faces_spline = lm(imdbScore~bs(nbFaces, knots = c(k1,k2,k3), degree = 1))

eq_spline= geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3), degree=1))
plot+scatter+lm_faces_spline

summary(lm_faces_spline) # spline has lower significance and lower r2. 

'''Conclusion: best nbFaces seems to be poly^2. nbFaces dont seem to be a strong predictor to include'''

############################################################################################
### Plot keywords: probs can be discarded because information is represented by genre ###

lm_plot1 = lm(imdbScore~plotKeywords)

summary(lm_plot1) 
############################################################################################

### Genres ###
# action
lm_action = lm(imdbScore~action)
summary(reg_action_1)
residualPlots(reg_action_1, col= "grey")
# sig p = 2.096e-12 and 2.5% r2

# adventure
lm_adventure = lm(imdbScore~adventure)
summary(lm_adventure)
#sig p = 0.003282 and 0.3958% r2

# scifi
lm_scifi = lm(imdbScore~scifi)
summary(lm_scifi)
#sig p = 3.669e-5 and 0.8285% r2

# thriller
lm_thriller = lm(imdbScore~thriller)
summary(lm_thriller)
#sig p = 0.0004324 and 0.589% r2

# musical
lm_musical = lm(imdbScore~musical)
summary(lm_musical)
#not sig. p = 0.32 and negative r2. test for linearity, hetero, and otuliers

# romance
lm_romance = lm(imdbScore~romance)
summary(lm_romance)
#not sig. p = 0.5135 and negative r2. test for linearity, hetero, and outliers

# western
lm_western = lm(imdbScore~western)
summary(lm_western)
#sig p = 0.003974 and 0.377% r2

# sport
lm_sport = lm(imdbScore~sport)
summary(lm_sport)
# sig p = 0.01567 but not too much. 0.25% r2 

# horror
lm_horror = lm(imdbScore~horror)
summary(lm_horror)
#sig p = 2.1e-13 and 2.7% r2

# drama
lm_drama = lm(imdbScore~drama)
summary(lm_drama)
# sig p = 2.2e-16 and 11.4% r2 preditive power. I think we should include it.


# war
lm_war = lm(imdbScore~war)
summary(lm_war)
#sig p =  1.757e-6 and 1.1% r2

# animation
lm_animation = lm(imdbScore~animation)
summary(lm_animation)
#not sig. p = 0.47 and r2 is negative test for lin, hetero and outliers

# crime
lm_crime = lm(imdbScore~crime)
summary(lm_crime)
#sig p = 0.0069 and 0.3259% r2

attach(imdb)

'''Conclusion: choose genres with low p vals. We can start with r2 > 1%, horror drama war
action'''

############################################################################################
### Movie Meter ###

lm_moviemeter = lm(imdbScore~movieMeter_IMDBpro)
summary(lm_moviemeter)
# sig p = 7.897e-5 and 0.7537% r2

#test for non-linearity
residualPlots(lm_moviemeter) # p = 3.824e-15 therefore non-linear

#Anova Test
lm_movMeter2 = lm(imdbScore~poly(movieMeter_IMDBpro,2))
lm_movMeter3 = lm(imdbScore~poly(movieMeter_IMDBpro,3))
lm_movMeter4 = lm(imdbScore~poly(movieMeter_IMDBpro,4))
lm_movMeter5 = lm(imdbScore~poly(movieMeter_IMDBpro,5))
lm_movMeter6 = lm(imdbScore~poly(movieMeter_IMDBpro,6))

anova(lm_moviemeter,lm_movMeter2,lm_movMeter3,lm_movMeter4,lm_movMeter5,lm_movMeter6)
#degree keeps increasing. Maybe spline

#Test for heteroskedasticity
residualPlots(lm_moviemeter, quadratic = FALSE) #visually hetero
ncvTest(lm_moviemeter) #shows heteroskedasticity p = 0.00013 (p < 0.05)

coeftest(lm_moviemeter, vcov=vcovHC(lm_moviemeter, type = 'HC1'))
#After fixing hetero, movieMeter is WAY less important p = 0.01228

#spline regression
attach(imdb)
plot = ggplot(imdb, aes(y=imdbScore, x=lm_moviemeter))
scatter = geom_point(color = 'black')
plot+scatter

'''Conclusion: movieMeter is non-linear but r2 is very low. prob not worth adding'''
############################################################################################
### director ###
top30dir <- list("Stanley Kubrick", "Martin Scorsese","Quentin Tarantino", "Christopher Nolan","David Fincher","Ethan Coen","Joel Coen","Steven Spielberg","Alfred Hitchcock","Francis Ford Coppola","Clint Eastwood","John Carpenter","Paul Thomas Anderson","Ridley Scott","Milos Forman",
                 "Sidney Lumet",
                 "Guillermo del Toro",
                 "Brian de Palma",
                 "Wes Anderson",
                 "Sergio Leone",
                 "Robert Zemeckis",
                 "Mel Brooks",
                 "Peter Jackson",
                 "James Cameron",
                 "George Cukor",
                 "Terry Gilliam",
                 "Elia Kazan",
                 "Billy Wilder",
                 "John Huston",
                 "Frank Capra"
)
imdb$top_30dir = ifelse(imdb$director %in% top30dir, 1, 0)
attach(imdb)

lm_dir = lm(imdbScore~top_30dir)
summary(lm_dir)
# sig p = 2.2e-16 and r2 is 6.5%

#Test for hetero
residualPlots(lm_dir)
ncvTest(lm_dir) # p = 0.000649 there's hetero
coeftest(lm_dir, vcov=vcovHC(lm_dir, type = 'HC1'))
#After fixing hetero, significance was not affected

'''Conclusion: top30 seems to be a very strong predcitor. should be included'''

############################################################################################
### DISTRIBUTOR ###
table(distributor)
summary(table(distributor))

imdb$distributor = as.factor(imdb$distributor)
attach(imdb)
lm_dis = lm(imdbScore~distributor)
summary(lm_dis)
# sig p = 1.979e-10 and r2 = 10.17%. Too many distributors with high p-val

#Hetero test
residualPlots(lm_dis)
ncvTest(lm_dis) #p = 9.27e-7 theres hetero
coeftest(lm_dis, vcov=vcovHC(lm_dir, type = 'HC1'))
#no p val from coeftest above. Dist must be dummified.

top100dist <- list('Walt Disney',
'Warner Bros.',
'Sony Pictures',
'Universal',
'20th Century Fox',
'Paramount Pictures',
'Lionsgate',
'New Line',
'Dreamworks SKG',
'Miramax',
'MGM',
'Fox Searchlight',
'Focus Features',
'Weinstein Co.',
'Summit Entertainment',
'Sony Pictures Classics',
'STX Entertainment',
'Miramax/Dimension',
'Relativity',
'Open Road',
'United Artists',
'A24',
'Roadside Attractions',
'Newmarket Films',
'20th Century Studios',
'IFC Films',
'CBS Films',
'FilmDistrict',
'Artisan',
'Paramount Vantage',
'Overture Films',
'IMAX Films',
'USA Films',
'Gramercy',
'Weinstein/Dimension',
'Bleecker Street',
'Pure Flix Entertainment',
'Neon',
'FUNimation',
'Magnolia Pictures',
'Picturehouse',
'Polygram',
'Warner Independent',
'MacGillivray Freeman Films',
'Focus/Rogue Pictures',
'Fine Line',
'Entertainment Studios Motion Pictures',
'Samuel Goldwyn Films',
'Eros Entertainment',
'Sony/TriStar',
'Freestyle Releasing',
'October Films',
'Annapurna Pictures',
'Broad Green Pictures',
'UTV Communications',
'Aviron Pictures',
'Yash Raj Films',
'Searchlight Pictures',
'Orion Pictures',
'Music Box Films',
'Savoy',
'Rocky Mountain Pictures',
'Filmways Pictures',
'ThinkFilm',
'Goldwyn Entertainment',
'National Geographic Entertainment',
'Yari Film Group Releasing',
'Trimark',
'Destination Films',
'Amazon Studios',
'Alliance Atlantis',
'Avco Embassy',
'Fathom Events',
'Well Go USA',
'Compass International',
'EuropaCorp',
'Giant Screen Films',
'nWave Pictures',
'Crunchyroll',
'First Look',
'Global Road',
'FIP',
'RADiUS-TWC',
'BH Tilt',
'High Top Releasing',
'Greenwich',
'Apparition',
'Clarius Entertainment',
'GKIDS',
'Zeitgeist',
'IDP/Goldwyn/Roadside',
'101 Studios',
'Focus / Gramercy',
'Great India Films',
'The Orchard',
'New Yorker',
'IDP Distribution',
'3D Entertainment',
'China Lion Film Distribution',
'LD Entertainment')

imdb$top100dis = ifelse(imdb$distributor %in% top100dist, 1, 0)
attach(imdb)

lmtopdist = lm(imdbScore~top100dis)
summary(lmtopdist)
# sig p = 0.00016 and r2 is 0.7%

#Test for hetero
residualPlots(lmtopdist)
ncvTest(lmtopdist) # p = 0.09 there's no hetero

#top 50 dist
top50dist <- list('Walt Disney',
                                     'Warner Bros.',
                                     'Sony Pictures',
                                     'Universal',
                                     '20th Century Fox',
                                     'Paramount Pictures',
                                     'Lionsgate',
                                     'New Line',
                                     'Dreamworks SKG',
                                     'Miramax',
                                     'MGM',
                                     'Fox Searchlight',
                                     'Focus Features',
                                     'Weinstein Co.',
                                     'Summit Entertainment',
                                     'Sony Pictures Classics',
                                     'STX Entertainment',
                                     'Miramax/Dimension',
                                     'Relativity',
                                     'Open Road',
                                     'United Artists',
                                     'A24',
                                     'Roadside Attractions',
                                     'Newmarket Films',
                                     '20th Century Studios',
                                     'IFC Films',
                                     'CBS Films',
                                     'FilmDistrict',
                                     'Artisan',
                                     'Paramount Vantage',
                                     'Overture Films',
                                     'IMAX Films',
                                     'USA Films',
                                     'Gramercy',
                                     'Weinstein/Dimension',
                                     'Bleecker Street',
                                     'Pure Flix Entertainment',
                                     'Neon',
                                     'FUNimation',
                                     'Magnolia Pictures',
                                     'Picturehouse',
                                     'Polygram',
                                     'Warner Independent',
                                     'MacGillivray Freeman Films',
                                     'Focus/Rogue Pictures',
                                     'Fine Line',
                                     'Entertainment Studios Motion Pictures',
                                     'Samuel Goldwyn Films',
                                     'Eros Entertainment',
                                     'Sony/TriStar',
                                     'Freestyle Releasing')

imdb$top50dis = ifelse(imdb$distributor %in% top50dist, 1, 0)
attach(imdb)

lm50dist = lm(imdbScore~top50dis)
summary(lm50dist)
# sig and r2 decreased

#Test for hetero
residualPlots(lm50dist)
ncvTest(lm50dist) # p = 0.11 there's no hetero

'Conc: after dummy, very low r2. so dont include '

#OUR FINAL MODEL
attach(imdb)

#first final model: using anova optimals for polynomials degree > 1
fmodel1 = lm(imdbScore~poly(duration,4)+poly(nbNewsArticles,5)+lang_english+color+top30cinematographer+action+horror+
             drama+war+top_30dir)

summary(fmodel1)
#p = 2.2e-16 and r2 = 0.37

#2nd final model: using duration^2 and news^5. news^3 also decreases r^2 so we decide optimal degree in MSE loop
fmodel2 = lm(imdbScore~poly(duration,2)+poly(nbNewsArticles,5)+lang_english+color+top30cinematographer+action+horror+
               drama+war+top_30dir)

summary(fmodel2)
#p = 2.2e-16 and r2 = 0.3604

# 3rd model: adding vars
fmodel3 = lm(imdbScore~duration+poly(nbNewsArticles,5)+lang_english+color+top30cinematographer+action+horror+
               drama+war+top_30dir+Budget_Millions+MaturityRating_PG13+MaturityRating_R)

summary(fmodel3)
#p = 2.2e-16 and r2 = 0.3604
# Adding budget: r2 = 0.3826 #keep
# Adding maturity (13 and R): r2 = 0.3913 #keep
# Adding country: r2 = 0.3954 #exclude 
# Adding act1_25: r2 = 0.3972 #exclude
# Adding top100dis: r2 = 0.4008 exclude it no big diff
# Adding releaseYear: r2 = 0.4238 exclude years 
# Adding releaseMonth: r2 = 0.4241 exclude month

############################################################################################################################
# K -fold test for optimal polynomial degrees
attach(imdb)
c = 1
MSEs = rep(NA, 25)
combos = rep(NA, 25)
for (i in 1:5){
  for(j in 1:5){
    fit=glm(imdbScore~poly(duration,i)+poly(nbNewsArticles,j)+lang_english+color+top30cinematographer+action+horror+
              drama+war+top_30dir+Budget_Millions+MaturityRating_PG13+MaturityRating_R, data=imdb)
    MSEs[c]=cv.glm(imdb,fit,K=10)$delta[1]
    combos[c]=paste(c(i,j), collapse='')
    c=c+1
  }
}            

combos[which.min(MSEs)]
min(MSEs)
MSEs

#From observations, lowest MSE happens at linear NewsArticles
"""