# Import Shark Tank Dataset using readr
st = read.csv('C:\\Users\\m.maraqa\\Desktop\\MMA courses\\Fall 2022\\Multivariate Statistical Analysis\\Final Project\\Dataset 3 — Shark tank pitches.csv')
attach(st)
########################################################################## DATA EXPLORATION ###############################################
# Getting number of deals
table(deal) #244 failed, 251 successful

# Getting number of categories
length(unique(category)) # 54 categories
table(category) #highest amount in specialty foods = 62

# Locations number
length(unique(location)) #255 locations

#Stats for askedfor
describe(askedFor)

#Stats for stake
describe(exchangeForStake)

#stats for valuation
describe(valuation)

# The sharks in 6 szns
df1 = as.vector(as.matrix(st[c(8:12)]))
unique(df1) #"Barbara Corcoran"  "Lori Greiner" "Robert Herjavec"   "Kevin O'Leary" "Steve Tisch"  "Daymond John"     
            #"Jeff Foxworthy"    "Mark Cuban"        "Kevin Harrington"  "John Paul DeJoria" "Nick Woodman"    
length(unique(df1)) # 11 sharks

# Multiple entrepreneurs count
table(Multiple.Entreprenuers) #161 multiple, 334 lone

########### Running PCA on numerical variables to examine relationships #####################
library(ggfortify)

labels(st)

numvars = df[,c(3,8,9,10,11)]
pca=prcomp(numvars,scale=TRUE)
pca
autoplot(pca, data = numvars, col=ifelse(st$deal==TRUE,"green","black"), loadings.label = TRUE )
autoplot(pca, data = numvars, col=ifelse(st$category=='Specialty Food',"green","black"), loadings.label = TRUE )
autoplot(pca, data = numvars, col=ifelse(st$Multiple.Entreprenuers=='TRUE',"green","black"), loadings.label = TRUE )
########################################################################## PRE-PROCESSING ###########################################################################################
#Drop futile column: description, entrepreneurs, website, title,episode-season
labels(st)
st = st[-c(2,5,7,17,18)]
labels(st)

#Dropping NA values
cbind(
  lapply(
    lapply(st, is.na)
    , sum)
) #No NA values found

############### OUTLIERS ###################
library(car)
# Checking for outliers with Benferroni test
st$outcome = ifelse(st$deal == 'TRUE', 1, 0)
st$deal = as.factor(st$deal)
st$mulent = ifelse(st$Multiple.Entreprenuers == 'TRUE', 1, 0)
st$Multiple.Entreprenuers = as.factor(st$Multiple.Entreprenuers)
attach(st)

logit=glm(outcome~episode+category+location+askedFor+exchangeForStake+valuation+season+shark1+shark2+
            shark3+shark4+shark5+Multiple.Entreprenuers, family = 'binomial')
logit
outlierTest(logit)

#Dropping outliers
st = st[-c(54,97,99,117,155,191),]

############# COLLINEARITY ###############
require(psych)
# Correlation Matrix
labels(st)
quantvars=st[, c(2, 5, 6, 7, 8,16)]
corr_matrix=cor(quantvars)
round(corr_matrix, 2)

'Valuation and askedFor are highly positively correlated (0.76). Remove valuation.'
#Dropping one of colinear variables
st = st[,-c(7)]
labels(st)

#################### RANDOM FOREST ##################################
library(randomForest)

########## First RF ############
myforest=randomForest(deal~episode+askedFor+exchangeForStake+season+shark1+shark2+
                        shark3+shark4+shark5+Multiple.Entreprenuers, ntree=500, 
                      data=st, importance=TRUE, na.action = na.omit)
myforest #49.9% error

# Feature importance and plot
importance(myforest)
varImpPlot(myforest)

'''Sharks seem to have very low importance. What if insteand of shark1-5, we studied the 
  presence of each shark and its effect on the prediction'''

labels(st)
df1 = as.vector(as.matrix(st[c(8:12)]))
unique(df1)

st$Barbara_Corcoran = ifelse(st$shark1 == 'Barbara Corcoran' |
                               st$shark2 == 'Barbara Corcoran' |
                               st$shark3 == 'Barbara Corcoran' |
                               st$shark4 == 'Barbara Corcoran' |
                               st$shark5 == 'Barbara Corcoran', 1, 0)
st$Lori_Greiner = ifelse(st$shark1 == 'Lori Greiner' |
                           st$shark2 == 'Lori Greiner' |
                           st$shark3 == 'Lori Greiner' |
                           st$shark4 == 'Lori Greiner' |
                           st$shark5 == 'Lori Greiner', 1, 0)
st$Robert_Herjavec = ifelse(st$shark1 == 'Robert Herjavec' |
                              st$shark2 == 'Robert Herjavec' |
                              st$shark3 == 'Robert Herjavec' |
                              st$shark4 == 'Robert Herjavec' |
                              st$shark5 == 'Robert Herjavec', 1, 0)
st$Kevin_OLeary = ifelse(st$shark1 == 'Kevin O\'Leary' |
                           st$shark2 == 'Kevin O\'Leary' |
                           st$shark3 == 'Kevin O\'Leary' |
                           st$shark4 == 'Kevin O\'Leary' |
                           st$shark5 == 'Kevin O\'Leary', 1, 0)
st$Steve_Tisch = ifelse(st$shark1 == 'Steve Tisch' |
                          st$shark2 == 'Steve Tisch' |
                          st$shark3 == 'Steve Tisch' |
                          st$shark4 == 'Steve Tisch' |
                          st$shark5 == 'Steve Tisch', 1, 0)
st$Daymond_John = ifelse(st$shark1 == 'Daymond John' |
                           st$shark2 == 'Daymond John' |
                           st$shark3 == 'Daymond John' |
                           st$shark4 == 'Daymond John' |
                           st$shark5 == 'Daymond John', 1, 0)
st$Jeff_Foxworthy = ifelse(st$shark1 == 'Jeff Foxworthy' |
                             st$shark2 == 'Jeff Foxworthy' |
                             st$shark3 == 'Jeff Foxworthy' |
                             st$shark4 == 'Jeff Foxworthy' |
                             st$shark5 == 'Jeff Foxworthy', 1, 0)
st$Mark_Cuban = ifelse(st$shark1 == 'Mark Cuban' |
                         st$shark2 == 'Mark Cuban' |
                         st$shark3 == 'Mark Cuban' |
                         st$shark4 == 'Mark Cuban' |
                         st$shark5 == 'Mark Cuban', 1, 0)
st$Kevin_Harrington = ifelse(st$shark1 == 'Kevin Harrington' |
                               st$shark2 == 'Kevin Harrington' |
                               st$shark3 == 'Kevin Harrington' |
                               st$shark4 == 'Kevin Harrington' |
                               st$shark5 == 'Kevin Harrington', 1, 0)
st$John_Paul_DeJoria = ifelse(st$shark1 == 'John Paul DeJoria' |
                                st$shark2 == 'John Paul DeJoria' |
                                st$shark3 == 'John Paul DeJoria' |
                                st$shark4 == 'John Paul DeJoria' |
                                st$shark5 == 'John Paul DeJoria', 1, 0)
st$Nick_Woodman = ifelse(st$shark1 == 'Nick Woodman' |
                           st$shark2 == 'Nick Woodman' |
                           st$shark3 == 'Nick Woodman' |
                           st$shark4 == 'Nick Woodman' |
                           st$shark5 == 'Nick Woodman', 1, 0)
#Dropping shark 1 to 5
st = st[,-c(8:12)]
attach(st)

########## Second RF ############
myforest2=randomForest(deal~episode+category+location+askedFor+exchangeForStake+season+Multiple.Entreprenuers+
                         Barbara_Corcoran+Lori_Greiner+Robert_Herjavec+Kevin_OLeary+Steve_Tisch+Daymond_John+
                         Jeff_Foxworthy+Mark_Cuban+Kevin_Harrington+John_Paul_DeJoria+Nick_Woodman, ntree=500, 
                      data=st, importance=TRUE, na.action = na.omit)
myforest2 #49.28% error

# Feature importance and plot
importance(myforest2)
varImpPlot(myforest2)

'This confirms that the sharks at the pitch have no effect on the status of the deal'
'''Comparing performance of vars across those two RF tests, it can be noted that
 that the consistently important features are exchnageforstake, location, category,
 episode (not so much), and askedfor. Thus, these should be kept and rest discarded'''

################### Categories ##############
unique(category) # Number of distinct categories = 54

#There are 54 categories, which can definetly be grouped into smaller groups
#the intuition is that sharks may prefer certain categories over others, and some are more "in"/"booming"

Occasions = list('Weddings','Holiday Cheer','Party Supplies')
Nutrition = list('Alcoholic Beverages','Non-Alcoholic Beverages','Water Bottles','Wine Accessories','Specialty Food')
Fitness = list('Fitness Programs','Golf Products','Fitness Apparel and Accesssories','Fitness Equipment','Cycling')
Wellness = list('Personal Care and Cosmetics','Outdoor Recreation','Health and Well-Being','Maternity','Homeopathic Remedies','Education')
Med_Ent = list('Mobile Apps','Music','Toys and Games','Automotive','Entertainment','Electronics','Pet Products')
Services = list('Pest Control','Consumer Services','Professional Services','Novelties','Online Services')
Home = list('Gardening','Productivity Tools','Furniture','Kitchen Tools','Storage and Cleaning Products','Home Improvement','Home Accessories','Home Security Solutions')
Fashion = list('Men and Women\'s Apparel','Fashion Accessories','Women\'s Apparel','Women\'s Shoes','Men\'s Accessories','Undergarments and Basics','Men and Women\'s Shoes','Men and Women\'s Accessories','Costumes','Women\'s Accessories')
Children = list('Baby and Children\'s Apparel and Accessories', 'Baby and Child Care','Baby and Children\'s Food','Baby and Children\'s Bedding','Baby and Children\'s Entertainment')

st$services = ifelse(st$category %in% Services, 1,0)
st$fashion = ifelse(st$category %in% Fashion, 1,0)
st$wellness = ifelse(st$category %in% Wellness, 1,0)
st$fitness = ifelse(st$category %in% Fitness, 1,0)
st$occasions = ifelse(st$category %in% Occasions, 1,0)
st$med_ent = ifelse(st$category %in% Med_Ent, 1,0)
st$nutrition = ifelse(st$category %in% Nutrition, 1,0)
st$home = ifelse(st$category %in% Home, 1,0)
st$kids = ifelse(st$category %in% Children, 1,0)

#drop category 
labels(st)
st = st[,-c(3)]
attach(st)

############## Location #############
length(unique(location)) #255 locations 

#Splitting locations into regions
st$state = substring(st$location, nchar(st$location)-1, nchar(st$location))
attach(st)

length(unique(state)) # 44 states

ne = list('CT','MA','ME','NH','NJ','NY','PA','RI','VT')
mw = list('IA','IL','IN','KS','MI','MN','MO','ND','NE','OH','SD','WI')
w = list('AK', 'AZ','CA','CO','HI','ID','MT','NM','NV','OR','UT','WA','WY')
s = list('AL','AR','DC','DE','FL','GA','KY','LA','MD','MS','NC','OK','SC','TN','TX','VA','WV') 

st$NE = ifelse(st$state %in% ne, 1,0)
st$MW = ifelse(st$state %in% mw, 1,0)
st$W = ifelse(st$state %in% w, 1,0)
st$S = ifelse(st$state %in% s, 1,0)
attach(st)

table(NE) #83 projects in the NE
table(MW) #58 projects in the MW
table(W) #213 projects in the W
table(S) #135 projects in the S

#Drop location
labels(st)
st = st[,-c(3)]
attach(st)

################# RF: checking if all locations and categories are important #########################
myforest3=randomForest(deal~episode+askedFor+exchangeForStake+season+Multiple.Entreprenuers+
                         services+fashion+wellness+fitness+occasions+med_ent+nutrition+home+kids+NE+MW+W+S, 
                       ntree=500, data=st, importance=TRUE, na.action = na.omit)
myforest3 #45.6% error

# Feature importance and plot
importance(myforest3)
varImpPlot(myforest3)

'Keep vars you see log reg below'

###################################### LOGISTICAL REG ########################################
# Split the data into train-test (70/30)
train_end = 0.7*nrow(st) # train_end = 342
test_start = as.integer(train_end) + 1 # test_start = 343
trainset = st[c(1:train_end),]
testset = st[c(test_start:nrow(st)),]

rownames(trainset) = c(1:342)
rownames(testset) = c(1:147)

# Training logistic regression model with important features from RF
detach(st)
attach(trainset)
reg1=glm(outcome~episode+askedFor+exchangeForStake+services+med_ent+S, data = trainset, family = 'binomial')

summary(reg1)

# Get stargazer output
library(stargazer)
stargazer(reg1,dep.var.labels = 'Shark Tank Deal Status',
          covariate.labels=c('Episode Number', 'Requested Funding', 'Company Stake Offered'
                             , 'Product Category: Services', 'Product Category: Media and Entertainment',
                             'Company Location: Southern U.S. Region'),
          type = 'html')

# Getting % variance
require(rms)
mlog1 = lrm(outcome~episode+askedFor+exchangeForStake+services+med_ent+S, data = trainset)
mlog1

# Getting predictions
preds = predict(reg1, testset, type="response")

  #Prob of success is a 50%. if prob better than chance, then predict success
testset$prediction = ifelse(preds > 0.5, 1, 0)
testset$predstatus = ifelse(testset$outcome == testset$prediction, 1, 0)
accuracy_logreg = sum(testset$predstatus)/nrow(testset) # accuracy = 0.55 = 55%

#####################################       GBT      ########################################
# Split the data into train-test (70/30)
train_end = 0.7*nrow(st) # train_end = 342
test_start = as.integer(train_end) + 1 # test_start = 343
trainset = st[c(1:train_end),]
testset = st[c(test_start:nrow(st)),]

rownames(trainset) = c(1:342)
rownames(testset) = c(1:147)
attach(trainset)

# Build GBM
library(gbm)
set.seed (1)
boost=gbm(outcome~episode+askedFor+exchangeForStake+services+med_ent+S,
            data=trainset,distribution='bernoulli',n.trees=10000, interaction.depth=4)
summary(boost)

preds=predict(boost, newdata=testset, n.trees=10000, type = 'response')
preds
testset$prediction = ifelse(preds > 0.5, 1, 0)
testset$predstatus = ifelse(testset$outcome == testset$prediction, 1, 0)
accuracy_gbm = sum(testset$predstatus)/nrow(st) # accuracy = 0.147