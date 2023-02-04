############################################### Load Libraries ############################################################################
'IMPORT LIBRARIES HERE BEFORE GRADING'

import pandas as pd
import numpy as np
from numpy import where
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import pyplot

from sklearn.model_selection import train_test_split
from sklearn.linear_model import Lasso
from sklearn.decomposition import PCA
from sklearn.model_selection import GridSearchCV
from sklearn.datasets import make_classification
from sklearn.metrics import accuracy_score
from sklearn.metrics import silhouette_samples
from sklearn.metrics import silhouette_score
from sklearn.metrics import calinski_harabasz_score
from scipy.stats import f

from sklearn.ensemble import RandomForestClassifier
from sklearn.inspection import permutation_importance
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import IsolationForest
from sklearn.cluster import KMeans

'CHECK BELOW CLUSTERING FOR GRADING'
#########################################################################################################################
#
################################################## CLASSIFICATION ###########################################################
#
##########################################################################################################################

################################################# Import data ############################################################################################
kickstarter_df = pd.read_excel("C:\\Users\\m.maraqa\\Desktop\\MMA courses\\Fall 2022\\Data Mining and Visualization\\Individual Project\\Kickstarter.xlsx")

################################################ Pre-Processing ############################################################################################3
# Drop NA values
kickstarter_df.isna().any()
kickstarter_df = kickstarter_df.dropna()

# Drop values of state other than 'successful' and 'failed'    
kickstarter_df = kickstarter_df.drop(kickstarter_df[(kickstarter_df['state'] != 'successful') & (kickstarter_df['state'] != 'failed')].index, axis = 'index')
kickstarter_df = kickstarter_df.reset_index(drop = True)

# Setup the variables
#Change all goals to usd
kickstarter_df['goal_usd'] = kickstarter_df['goal']*kickstarter_df['static_usd_rate']

#Only choose relevant predictors
X = kickstarter_df[['goal_usd','country','category','name_len','name_len_clean','blurb_len','blurb_len_clean', 
                    'created_at_weekday','created_at_day', 'created_at_month', 'created_at_yr', 'created_at_hr', 
                    'launched_at_weekday', 'launched_at_day', 'launched_at_month','launched_at_yr','launched_at_hr',
                    'deadline_weekday','deadline_day', 'deadline_month','deadline_yr','deadline_hr', 
                    'create_to_launch_days', 'launch_to_deadline_days']]
X=pd.get_dummies(X,columns=['country','category','created_at_weekday', 'launched_at_weekday', 'deadline_weekday'])

#Set target variable
y = kickstarter_df['state']

#Turn state to binary
kickstarter_df.loc[kickstarter_df['state'] =='successful', 'state'] = 1
kickstarter_df.loc[kickstarter_df['state'] =='failed', 'state'] = 0
kickstarter_df['state']=kickstarter_df['state'].astype('int')


# Detect anomalies
iforest = IsolationForest(n_estimators=100, contamination=0.02)

df_an = pd.concat([y,X], axis = 1)
pred = iforest.fit_predict(df_an)
score = iforest.decision_function(df_an)

# Extract anomalies
anomaly_index=where(pred==-1)
anomaly_values=df_an.iloc[anomaly_index]

#Remove anomalies
df_an = df_an.drop(anomaly_values.index)
df_an = df_an.reset_index(drop = True)

y = df_an['state']
y = y.astype('int')
X = df_an.drop(columns = 'state')

############################################## Feature selection ####################################################################
# Standardize data (MinMaxStd)
scaler = MinMaxScaler()
X_std = scaler.fit_transform(X)

################### LASSO feature selection ################
lasso=Lasso(alpha=0.01,random_state=5)
lasso.fit(X_std,y)
lasso.coef_

sel=pd.DataFrame(list(zip(X.columns,lasso.coef_)),columns=["predictor","coefficient"])
X_lasso = X[sel['predictor'][sel['coefficient'] != 0]] 

################# Random Forest Feature Selection #############
rf=RandomForestClassifier(random_state=5)
model=rf.fit(X_std,y)
model.feature_importances_

sel_rf = pd.DataFrame(list(zip(X.columns,model.feature_importances_)), columns = ['predictor','feature importance'])
sel_rf = sel_rf.sort_values(by = 'feature importance', ascending = False)
sel_rf = sel_rf.reset_index(drop = True)

# Selecting top 10 features according to RF feature importance
X_rf = X[sel_rf['predictor'][0:10]]

######################### PCA Feature Selection ###############
pca = PCA(n_components=80) #80 is number of predictors in X_std
pca.fit(X_std)
pca.explained_variance_

# select number of components
PC_values = np.arange(pca.n_components_) + 1
plt.plot(PC_values, pca.explained_variance_ratio_.cumsum(), 'o-', linewidth=2, color='blue')
plt.title('Scree Plot')
plt.xlabel('Principal Component')
plt.ylabel('Variance Explained')
plt.axhline(y=0.95, color='grey', linestyle='--')
plt.text(1.1, 1, '95% cut-off threshold', color = 'black', fontsize=16)
plt.show()

'65 components capture around 95% of the total variance in the data'

################################################# KNN model #################################################################################
# GridsearchCV for hyperparameter tuning
knn_ps = {'n_neighbors':list(range(50,351,25)),
              'weights': ['uniform', 'distance']}

knn = KNeighborsClassifier(p=2)
gcv_knn = GridSearchCV(estimator = knn, param_grid = knn_ps, cv = 5, n_jobs = -1, verbose = 1, scoring = 'accuracy')
gcv_knn.fit(X_std,y)
print('Best parameter combination for KNN model is\n', gcv_knn.best_params_)

'Best parameter combination for KNN model is {n_neighbors: 75, weights: distance}'

######### KNN with all predictors ##########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_std, y, test_size = 0.33, random_state = 5)

knn = KNeighborsClassifier(p = 2, n_neighbors = 75, weights = 'distance')
model = knn.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_knn=accuracy_score(y_test, y_test_pred) # 0.6903 accuracy score

######### KNN with LASSO predictors #########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_lasso, y, test_size = 0.33, random_state = 5)

knn_lasso = KNeighborsClassifier(p = 2, n_neighbors = 75, weights = 'distance')
model = knn_lasso.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_knn_lasso=accuracy_score(y_test, y_test_pred) # 0.6956 accuracy score


########## KNN with top 10 RF predictors ############
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_rf, y, test_size = 0.33, random_state = 5)

knn_rf = KNeighborsClassifier(p = 2, n_neighbors = 75, weights = 'distance')
model = knn_rf.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_knn_rf=accuracy_score(y_test, y_test_pred) # 0.6801 accuracy score

########## KNN with 65 principal components ###########
pca = PCA(n_components=65)
pca.fit(X_std)
X_new_pca = pca.transform(X_std)

X_train, X_test, y_train, y_test = train_test_split(X_new_pca, y, test_size = 0.3, random_state = 0)

knn_pca = KNeighborsClassifier(n_neighbors=75, p=2, weights='distance')
model = knn_pca.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
accuracy_knn_pca = accuracy_score(y_test, y_test_pred) #0.6948 accuracy score

############################################### Random Forest ######################################################################################
rf_ps = {'min_samples_split': list(range(2,20,1)),
         'min_samples_leaf': list(range(1,11,1)),
         'max_depth': list(range(1,32,4)),
         'n_estimators': list(range(50,501,50)),
         'max_features':['auto', 'log2' ,'sqrt']}

rf=RandomForestClassifier(random_state=0)
gcv_rf = GridSearchCV(estimator = rf, param_grid = rf_ps, cv = 5, n_jobs = -1, verbose = 1, scoring = 'accuracy')
gcv_rf.fit(X_std, y)
print('Best parameter combination for RF model is\n', gcv_rf.best_params_)

'''Best parameter combination for RF model is {min_samples_split: 17, min_samples_leaf: 6, 
                                                max_depth: 21, n_estimators: 500, max_features: log2}'''

######### RF with all predictors ##########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_std, y, test_size = 0.33, random_state = 5)

rf = RandomForestClassifier(min_samples_split = 17, min_samples_leaf = 6, max_depth = 21,  
                             n_estimators = 500, max_features = 'log2')
model = rf.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_rf=accuracy_score(y_test, y_test_pred) # 0.7444 accuracy score

######### RF with LASSO predictors #########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_lasso, y, test_size = 0.33, random_state = 5)

rf_lasso = RandomForestClassifier(min_samples_split = 17, min_samples_leaf = 6, max_depth = 21,  
                                  n_estimators = 500, max_features = 'log2')
model = rf_lasso.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_rf_lasso=accuracy_score(y_test, y_test_pred) # 0.6852 accuracy score


########## RF with top 10 RF predictors ############
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_rf, y, test_size = 0.33, random_state = 5)

rf_rf = RandomForestClassifier(min_samples_split = 17, min_samples_leaf = 6, max_depth = 21,  
                               n_estimators = 500, max_features = 'log2')
model = rf_rf.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_rf_rf=accuracy_score(y_test, y_test_pred) # 0.7103 accuracy score

########## RF with 65 principal components #########
pca = PCA(n_components=65)
pca.fit(X_std)
X_new_pca = pca.transform(X_std)

X_train, X_test, y_train, y_test = train_test_split(X_new_pca, y, test_size = 0.3, random_state = 0)

rf_pca = RandomForestClassifier(min_samples_split = 17, min_samples_leaf = 6, max_depth = 21,  
                                n_estimators = 500, max_features = 'log2')
model = rf_pca.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
accuracy_rf_pca = accuracy_score(y_test, y_test_pred) #0.7255 accuracy score

############################################# Gradient Boosting #################################################################################
gbt_ps = {'loss':['log_loss','deviance'],
          'learning_rate': list(range(0.025, 0.125, 0.025)),
          'min_samples_split': list(range(2,20,1)),
          'min_samples_leaf': list(range(1,11,1)),
          'max_depth': list(range(2,32,4)),
          'n_estimators': list(range(50,251,10)),
          'max_features':['auto', 'log2' ,'sqrt']}

'loss = 'deviance', learning_rate = 0.075, max_features = auto, max_depth = 6, min_samples_split = 9, min_samples_leaf = 3, n_estimators = 250'

gbt=GradientBoostingClassifier(random_state=0)
gcv_gbt = GridSearchCV(estimator = gbt, param_grid = gbt_ps, cv = 5, n_jobs = -1, verbose = 1, scoring = 'accuracy')
gcv_gbt.fit(X_std, y)
print('Best parameter combination for GBT model is\n', gcv_gbt.best_params_)

'''Best parameter combination for GBT model is {loss: deviance, learning_rate: 0.075, 
                                                min_samples_split: 9, min_samples_leaf: 3, 
                                                max_depth: 6, n_estimators: 250, max_features: auto}'''

######### GBT with all predictors ##########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_std, y, test_size = 0.33, random_state = 5)

gbt = GradientBoostingClassifier(loss = 'deviance', learning_rate = 0.075, min_samples_split = 9,
                                 min_samples_leaf = 3, max_depth = 6, n_estimators = 250, max_features = 'auto')
model = gbt.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_gbt=accuracy_score(y_test, y_test_pred) # 0.7593 accuracy score

######### GBT with LASSO predictors #########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_lasso, y, test_size = 0.33, random_state = 5)

gbt_lasso = GradientBoostingClassifier(loss = 'deviance', learning_rate = 0.075, min_samples_split = 9,
                                  min_samples_leaf = 3, max_depth = 6, n_estimators = 250, max_features = 'auto')
model = gbt_lasso.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_gbt_lasso=accuracy_score(y_test, y_test_pred) # 0.6847 accuracy score


########## GBT with top 10 RF predictors ############
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_rf, y, test_size = 0.33, random_state = 5)

gbt_rf = GradientBoostingClassifier(loss = 'deviance', learning_rate = 0.075, min_samples_split = 9,
                               min_samples_leaf = 3, max_depth = 6, n_estimators = 250, max_features = 'auto')
model = gbt_rf.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
acc_gbt_rf=accuracy_score(y_test, y_test_pred) # 0.6910 accuracy score

########## GBT with 65 principal components #########
pca = PCA(n_components=65)
pca.fit(X_std)
X_new_pca = pca.transform(X_std)

# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_new_pca, y, test_size = 0.3, random_state = 0)

gbt_pca = GradientBoostingClassifier(loss = 'deviance', learning_rate = 0.075, min_samples_split = 9,
                                    min_samples_leaf = 3, max_depth = 6, n_estimators = 250, max_features = 'auto')
model = gbt_pca.fit(X_train, y_train)
y_test_pred = model.predict(X_test)
accuracy_gbt_pca = accuracy_score(y_test, y_test_pred) #0.7305 accuracy score

#########################################################################################################################
#
################################################## CLUSTERING ###########################################################
#
##########################################################################################################################

kickstarter_df = pd.read_excel("C:\\Users\\m.maraqa\\Desktop\\MMA courses\\Fall 2022\\Data Mining and Visualization\\Individual Project\\Kickstarter.xlsx")

################################################ Pre-Processing ############################################################################################3
# Drop NA values
kickstarter_df.isna().any()
kickstarter_df = kickstarter_df.dropna()

# Drop values of state other than 'successful' and 'failed'    
kickstarter_df = kickstarter_df.drop(kickstarter_df[(kickstarter_df['state'] != 'successful') & (kickstarter_df['state'] != 'failed')].index, axis = 'index')
kickstarter_df = kickstarter_df.reset_index(drop = True)

#Turn state to binary
kickstarter_df.loc[kickstarter_df['state'] =='successful', 'state'] = 1
kickstarter_df.loc[kickstarter_df['state'] =='failed', 'state'] = 0
kickstarter_df['state']=kickstarter_df['state'].astype('int')

# Setup the variables
kickstarter_df['goal_usd'] = kickstarter_df['goal']*kickstarter_df['static_usd_rate']
kickstarter_df['delta_per'] = (kickstarter_df['usd_pledged']-kickstarter_df['goal_usd'])*100/kickstarter_df['goal_usd']
kickstarter_df.loc[kickstarter_df['staff_pick'] =='True', 'state'] = 1
kickstarter_df.loc[kickstarter_df['staff_pick'] =='False', 'state'] = 0
kickstarter_df['staff_pick']=kickstarter_df['staff_pick'].astype('int')

# I would like to observe the relationships between staff_pick, delta and launch_to_deadline_days
df = kickstarter_df.loc[:,['delta_per', 'staff_pick', 'launch_to_deadline_days']]

# Detect anomalies
iforest = IsolationForest(n_estimators=100, contamination=0.02)
pred = iforest.fit_predict(df)
score = iforest.decision_function(df)

# Extract anomalies
anomaly_index=where(pred==-1)
anomaly_values=df.iloc[anomaly_index]

#Remove anomalies
df = df.drop(anomaly_values.index)
df = df.reset_index(drop = True)

# Remove negative delta (failed projects)
df = df[df['delta_per'] >= 0]

################### KMeans Clustering ####################################
scaler = StandardScaler()
X_std = scaler.fit_transform(df)
# Finding optimal K (Silhouette)
for i in range (2,20):    
    kmeans = KMeans(n_clusters=i)
    model = kmeans.fit(X_std)
    labels = model.labels_
    print(i,':',silhouette_score(X_std,labels))
# optimal k = 10. However, 10 is too big to classify based on 3 predictors. 
# 4 is a low number with a similar silhouette score so choose 4.

#Finding optimal K (Elbow)
withinss = []
for i in range(2,20):
    kmeans = KMeans(n_clusters=i)
    kmeansmodel = kmeans.fit(X_std)
    withinss.append(kmeansmodel.inertia_)

pyplot.plot(list(range(2,20)), withinss)
plt.show()
#optimal k is 10 but 4 silhouette score is not much lower than 10. So choose k = 4

################### KMeans with 4 clusters ############################
kmeans = KMeans(n_clusters = 4, random_state = 5)
model = kmeans.fit(X_std)
labels = model.labels_
np.unique(model.labels_)

#Visualize clusters
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(df['delta_per'], df['staff_pick'], df['launch_to_deadline_days'],c=labels, cmap ='rainbow', marker = 'o')
ax.set_title("K-Means Clustering Result")
ax.set_xlabel('% Increase')
ax.set_ylabel('Staff Pick')
ax.set_zlabel('Launch to Deadlines Days')
plt.show()

print("Silhouette score is :")
print(silhouette_score(X_std, labels))
# 0.5844
print("calinski_harabasz_score is :")
score = calinski_harabasz_score(X_std, labels)
print(score)
# 3192.59

print("pvalue is :")
df1 = 3 # df1=k-1
df2 = 14071-3 # df2=n-k
pvalue = 1-f.cdf(score, df1, df2)
print(pvalue)
#1.1102230246251565e-16

#Cluster Stats
df_clusters = pd.concat([df.reset_index(drop=True),pd.DataFrame(labels, columns=["labels"])], axis=1)
cluster1 = df_clusters.loc[df_clusters["labels"]==0]
cluster2 = df_clusters.loc[df_clusters["labels"]==1]
cluster3 = df_clusters.loc[df_clusters["labels"]==2]
cluster4 = df_clusters.loc[df_clusters["labels"]==3]

c1 = pd.DataFrame(cluster1.describe()).drop(columns = ['labels'])
c2 = pd.DataFrame(cluster2.describe()).drop(columns = ['labels'])
c3 = pd.DataFrame(cluster3.describe()).drop(columns = ['labels'])
c4 = pd.DataFrame(cluster4.describe()).drop(columns = ['labels'])
########################################################################################################################

#################################################### GRADING ############################################################

################################################# CLASSIFICATION ################################################################
# Import Grading Data
kickstarter_df = pandas.read_excel("Kickstarter-Grading-Sample.xlsx")

# Pre-Process Grading Data
# Drop NA values
kickstarter_df.isna().any()
kickstarter_df = kickstarter_df.dropna()

# Drop values of state other than 'successful' and 'failed'    
kickstarter_df = kickstarter_df.drop(kickstarter_df[(kickstarter_df['state'] != 'successful') & (kickstarter_df['state'] != 'failed')].index, axis = 'index')
kickstarter_df = kickstarter_df.reset_index(drop = True)

# Setup the variables
#Change all goals to usd
kickstarter_df['goal_usd'] = kickstarter_df['goal']*kickstarter_df['static_usd_rate']

#Only choose relevant predictors
X = kickstarter_df[['goal_usd','country','category','name_len','name_len_clean','blurb_len','blurb_len_clean', 
                    'created_at_weekday','created_at_day', 'created_at_month', 'created_at_yr', 'created_at_hr', 
                    'launched_at_weekday', 'launched_at_day', 'launched_at_month','launched_at_yr','launched_at_hr',
                    'deadline_weekday','deadline_day', 'deadline_month','deadline_yr','deadline_hr', 
                    'create_to_launch_days', 'launch_to_deadline_days']]
X=pd.get_dummies(X,columns=['country','category','created_at_weekday', 'launched_at_weekday', 'deadline_weekday'])

#Set target variable
y = kickstarter_df['state']
#Turn state to binary
kickstarter_df.loc[kickstarter_df['state'] =='successful', 'state'] = 1
kickstarter_df.loc[kickstarter_df['state'] =='failed', 'state'] = 0
kickstarter_df['state']=kickstarter_df['state'].astype('int')

# Detect anomalies
iforest = IsolationForest(n_estimators=100, contamination=0.02)

df_an = pd.concat([y,X], axis = 1)
pred = iforest.fit_predict(df_an)
score = iforest.decision_function(df_an)

# Extract anomalies
anomaly_index=where(pred==-1)
anomaly_values=df_an.iloc[anomaly_index]

#Remove anomalies
df_an = df_an.drop(anomaly_values.index)
df_an = df_an.reset_index(drop = True)

y = df_an['state']
y = y.astype('int')
X = df_an.drop(columns = 'state')

######## Apply the model previously trained to the grading data ##########

# Standardize data (MinMaxStd)
scaler = MinMaxScaler()
X_std = scaler.fit_transform(X)
######### RF with all predictors ##########
# Split data to train-test
X_train, X_test, y_train, y_test = train_test_split(X_std, y, test_size = 0.33, random_state = 5)

rf = RandomForestClassifier(min_samples_split = 17, min_samples_leaf = 6, max_depth = 21,  
                             n_estimators = 500, max_features = 'log2')
model = rf.fit(X_train, y_train)
y_test_pred = model.predict(X_test)

# Calculate accuracy score
acc_rf=accuracy_score(y_test, y_test_pred) 


########################################################################################################################

#################################################### GRADING ############################################################

################################################## CLUSTERING ########################################################
# Import Grading Data
kickstarter_df = pandas.read_excel("Kickstarter-Grading-Sample.xlsx")

# Drop NA values
kickstarter_df.isna().any()
kickstarter_df = kickstarter_df.dropna()

# Drop values of state other than 'successful' and 'failed'    
kickstarter_df = kickstarter_df.drop(kickstarter_df[(kickstarter_df['state'] != 'successful') & (kickstarter_df['state'] != 'failed')].index, axis = 'index')
kickstarter_df = kickstarter_df.reset_index(drop = True)

#Turn state to binary
kickstarter_df.loc[kickstarter_df['state'] =='successful', 'state'] = 1
kickstarter_df.loc[kickstarter_df['state'] =='failed', 'state'] = 0
kickstarter_df['state']=kickstarter_df['state'].astype('int')

# Setup the variables
kickstarter_df['goal_usd'] = kickstarter_df['goal']*kickstarter_df['static_usd_rate']
kickstarter_df['delta_per'] = (kickstarter_df['usd_pledged']-kickstarter_df['goal_usd'])*100/kickstarter_df['goal_usd']
kickstarter_df.loc[kickstarter_df['staff_pick'] =='True', 'state'] = 1
kickstarter_df.loc[kickstarter_df['staff_pick'] =='False', 'state'] = 0
kickstarter_df['staff_pick']=kickstarter_df['staff_pick'].astype('int')

# I would like to observe the relationships between staff_pick, delta and launch_to_deadline_days
df = kickstarter_df.loc[:,['delta_per', 'staff_pick', 'launch_to_deadline_days']]

# Detect anomalies
iforest = IsolationForest(n_estimators=100, contamination=0.02)
pred = iforest.fit_predict(df)
score = iforest.decision_function(df)

# Extract anomalies
anomaly_index=where(pred==-1)
anomaly_values=df.iloc[anomaly_index]

#Remove anomalies
df = df.drop(anomaly_values.index)
df = df.reset_index(drop = True)

# Remove negative delta (failed projects)
df = df[df['delta_per'] >= 0]

#Standardize Data
scaler = StandardScaler()
X_std = scaler.fit_transform(df)

################### KMeans with 4 clusters ############################
kmeans = KMeans(n_clusters = 4, random_state = 5)
model = kmeans.fit(X_std)
labels = model.labels_
np.unique(model.labels_)

# Visualize clusters
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(df['delta_per'], df['staff_pick'], df['launch_to_deadline_days'],c=labels, cmap ='rainbow')
ax.set_xlabel('% Increase')
ax.set_ylabel('Staff Pick')
ax.set_zlabel('Launch to Deadlines Days')

# Cluster Stats
df_clusters = pd.concat([df.reset_index(drop=True),pd.DataFrame(labels, columns=["labels"])], axis=1)
cluster1 = df_clusters.loc[df_clusters["labels"]==0]
cluster2 = df_clusters.loc[df_clusters["labels"]==1]
cluster3 = df_clusters.loc[df_clusters["labels"]==2]
cluster4 = df_clusters.loc[df_clusters["labels"]==3]

c1 = pd.DataFrame(cluster1.describe()).drop(columns = ['labels'])
c2 = pd.DataFrame(cluster2.describe()).drop(columns = ['labels'])
c3 = pd.DataFrame(cluster3.describe()).drop(columns = ['labels'])
c4 = pd.DataFrame(cluster4.describe()).drop(columns = ['labels'])

print("Silhouette score is :")
print(silhouette_score(X_std, labels))
# 0.5844
print("calinski_harabasz_score is :")
score = calinski_harabasz_score(X_std, labels)
print(score)
# 3192.59

print("pvalue is :")
df1 = 3 # df1=k-1
df2 = 14071-3 # df2=n-k
pvalue = 1-f.cdf(score, df1, df2)
print(pvalue)
#1.1102230246251565e-16