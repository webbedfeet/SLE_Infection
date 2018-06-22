import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score, accuracy_score

dat = pd.read_csv('indiv_dat1.csv')
indiv = dat.drop(['hospid','lupus'], axis=1)
X, y = indiv.drop('dead',axis=1).values, indiv['dead'].values
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2,random_state=50)

dFull = xgb.DMatrix(X, y)
dTrain = xgb.DMatrix(X_train, y_train)
dTest = xgb.DMatrix(X_test, y_test)

params = {'eta':0.1,
        'max_depth':6,
        'subsample':1,
        'objective': 'binary:logistic'}
#params['eval_metric'] = roc_auc_score

model = xgb.train(
        params,
        dTrain,
        evals = [(dTest,"Test")],
        early_stopping_rounds = 5
        )

cvresult = xgb.cv(params, dFull, num_boost_round=500, nfold=5, early_stopping_rounds = 5, seed = 2049, metrics=['auc'])

params['n_estimators'] = 20
clf = xgb.XGBClassifier(max_depth = 6,
        learning_rate = 0.1,
        subsample = 1,
        seed = 2049,
        n_estimators = 20)
clf.fit(X,y)
risk = clf.predict_proba(X)[:,1]

dat['risk'] = risk

bl=(dat.
        groupby(['hospid','lupus'])[['risk','dead']].
        aggregate(np.sum))
bl['OE'] = bl['dead']/bl['risk']
bl = bl.reset_index()

results = bl.pivot(index= 'hospid', columns='lupus', 
        values = 'OE')
results['RR'] = results[1]/results[0]

results = results.loc[~pd.isna(results.RR),:]

######################################################################

## Not using race
dat1 = pd.read_csv('indivdat2.csv')
indiv1 = dat1.drop(['highvolume','hospid','lupus'], axis=1)
X1, y1 = indiv1.drop('dead',axis=1).values, indiv1['dead'].values
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2,random_state=50)

dFull1 = xgb.DMatrix(X1, y1)
dTrain = xgb.DMatrix(X_train, y_train)
dTest = xgb.DMatrix(X_test, y_test)

params = {'eta':0.1,
        'max_depth':6,
        'subsample':1,
        'objective': 'binary:logistic'}
cvresult1 = xgb.cv(params, dFull1, num_boost_round=500, nfold=5, early_stopping_rounds = 5, seed = 2049, metrics=['auc'])

clf1 = xgb.XGBClassifier(max_depth = 6,
        learning_rate = 0.1,
        subsample = 1,
        seed = 2049,
        n_estimators = 50)
clf1.fit(X1,y1)
risk1 = clf1.predict_proba(X1)[:,1]

dat1['risk'] = risk1

from sklearn.metrics import roc_auc_score

roc_auc_score(dat1.dead, dat1.risk)

bl=(dat1.
        groupby(['hospid','lupus'])[['risk','dead']].
        aggregate(np.sum))
bl['OE'] = bl['dead']/bl['risk']
bl = bl.reset_index()

results = bl.pivot(index= 'hospid', columns='lupus', 
        values = 'OE')
results['RR'] = results[1]/results[0]
results = results.reset_index()

## Looking at hospital characteristics

bl1 = dat1[['hospid','highvolume','northeast','midwest','south','west', 'rural','smallurban','largeurban','bed1','bed2','bed3']]
bl2 = results[['hospid','RR']]
blah = bl1.set_index('hospid').join(bl2.set_index('hospid')).reset_index().drop_duplicates()

from sklearn import tree
from sklearn.tree import DecisionTreeRegressor
import graphviz

dt = DecisionTreeRegressor(max_depth=3, min_samples_leaf=5)
dt.fit(blah.drop('RR', axis=1).values, blah.RR.values)

dot_data = tree.export_graphviz(dt, out_file=None,
        feature_names = blah.columns[:-1])
graph = graphviz.Source(dot_data)
graph

blah.to_csv('results.csv', index=False)
