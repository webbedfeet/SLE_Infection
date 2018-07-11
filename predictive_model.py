import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.model_selection import train_test_split, GridSearchCV
#from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns


pd.__version__
dat = pd.read_csv('indiv_dat1.csv') # Generated in DataMunging.R
indiv = dat.drop(['hospid','lupus'], axis=1)
X, y = indiv.drop('dead',axis=1).values, indiv['dead'].values
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2,random_state=50)

# Model training
dFull = xgb.DMatrix(X, y)
dTrain = xgb.DMatrix(X_train, y_train)
dTest = xgb.DMatrix(X_test, y_test)

params = {'eta':0.1,
        'max_depth':6,
        'subsample':1,
        'objective': 'binary:logistic',
        }
#params['eval_metric'] = roc_auc_score

#model = xgb.train(
#        params,
#        dTrain,
#        evals = [(dTest,"Test")],
#        early_stopping_rounds = 5,
#        num_boost_round= 20
#        )

cvresult = xgb.cv(params, dFull, num_boost_round=500, nfold=5, early_stopping_rounds = 5, seed = 2049, metrics=['auc'])

# 10 rounds gets to 99.7% of max test AUC
params['n_estimators'] = 10
clf = xgb.XGBRegressor(max_depth = 6,
        objective = 'binary:logistic',
        learning_rate = 0.1,
        subsample = 1,
        seed = 2049,
        n_estimators = 10)
clf.fit(X,y);
import joblib
joblib.dump(clf, 'PredictedModel.joblib.dat')
# clf = joblib.load('PredictedModel.dat')
import shutil
import os
shutil.copy2('PredictedModel.joblib.dat', os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data'))

# Predictions from model
risk = clf.predict(X)
dat['risk'] = risk
dat.to_csv('indiv_dat1_risk.csv', index = False)
shutil.copy2('indiv_dat1_risk.csv', os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data'))

# TODO: Make computation of RR into a function
# TODO: Run bootstrap to get RR for each hospital

def compute_RR(d):
    """
    USAGE:
    compute_RR(d)

    RESULT:
    A Series object with hospital ID as index and containing the RR

    Take the full data set, including hospid and risk scores, and
    compute the ratio of SMRs based on the risk scores as 'expecteds'.
    """
    bl = (d.
             groupby(['hospid', 'lupus'])[['risk','dead']].
             aggregate(np.sum))
    bl['OE'] = bl['dead']/bl['risk']
    bl = bl.reset_index()
    results = bl.pivot(index = 'hospid', columns = 'lupus', values = 'OE')
    results['RR'] = results[1]/results[0]
#    results.set_index('hospid')
    results = results['RR']
    return(results)

results = compute_RR(dat)

results = results.loc[~pd.isna(results.RR),:]
results.shape
bl2=dat[dat.lupus==0].groupby('hospid').size()
bl3 = dat[dat.lupus==0].groupby('hospid')['dead'].mean()
bl3.head()
blah = pd.concat([results, bl2, bl3], axis = 1)
blah.columns = ['0','1','RR','N','NonLupusMort']
blah.head()
roc_auc_score(dat.dead, dat.risk)

blah.plot(x = 'NonLupusMort', y = 'RR', kind='scatter')
plt.show()

plt.scatter(blah.N, blah.NonLupusMort); plt.show()
import seaborn as sns
sns.regplot('NonLupusMort', 'RR', data=blah[blah.RR>0], lowess=True)
plt.plot([0,0.35],[1,1])
plt.plot([0,0.35], [2,2], 'r:')
plt.show()

blah.to_csv('Hosp_indiv1_results.csv', index = False)
######################################################################

# Bootstrapping to capture uncertainty of estimates

def model_scoring(mod, d):
    d1 = d.copy() # Otherwise changes happen in place
    indiv = d1.drop(['hospid','lupus'], axis=1)
    X, y = indiv.drop('dead',axis=1).values, indiv['dead'].values
    risk = mod.predict(X)
    d1['risk'] = risk
    return(d1)

D = pd.read_csv('indiv_dat1.csv') # Generated in DataMunging.R
results = compute_RR(model_scoring(clf, D))
np.random.seed(2940)
for i in range(1000):
    print(i)
    # Stratified bootstrap
    D1 = D[D.lupus==1].sample(frac = 1, replace = True, axis = 0)
    D0 = D[D.lupus==0].sample(frac = 1, replace = True, axis = 0)
    Dnew = D1.append(D0)
    #D1 = D.sample(frac = 1, replace = True, axis = 0)
    r = compute_RR(model_scoring(clf, Dnew))
    results = pd.concat([results, r], axis = 1)
results.columns = range(1001)
results = results.replace(np.inf, np.NaN)
results1 = results.T
results1.to_csv(os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data/bootstrapRR1.csv'), index = False)




## Not using race

## dat1 = pd.read_csv('indiv_dat2.csv')
## indiv1 = dat1.drop(['hospid','lupus'], axis=1)
## X1, y1 = indiv1.drop('dead',axis=1).values, indiv1['dead'].values
## X_train, X_test, y_train, y_test = train_test_split(X1, y1, test_size=0.2,random_state=50)
##
## dFull1 = xgb.DMatrix(X1, y1)
## dTrain = xgb.DMatrix(X_train, y_train)
## dTest = xgb.DMatrix(X_test, y_test)
##
## params = {'eta':0.1,
##         'max_depth':6,
##         'subsample':1,
##         'objective': 'binary:logistic'}
## cvresult1 = xgb.cv(params, dFull1, num_boost_round=500, nfold=5, early_stopping_rounds = 5, seed = 2049, metrics=['auc'])
##
## clf1 = xgb.XGBRegressor(max_depth = 6,
##         learning_rate = 0.1,
##         subsample = 1,
##         seed = 2049,
##         n_estimators = 20)
## clf1.fit(X1,y1)
## risk1 = clf1.predict(X1)
##
## dat1['risk'] = risk1
## dat1.to_csv('indiv_dat2_risk.csv', index = False)
##
## from sklearn.metrics import roc_auc_score
##
## roc_auc_score(dat1.dead, dat1.risk)
##
## bl=(dat1.
##         groupby(['hospid','lupus'])[['risk','dead']].
##         aggregate(np.sum))
## bl['OE'] = bl['dead']/bl['risk']
## bl = bl.reset_index()
##
## bl.to_csv('Hosp_indiv2_results.csv', index=False)
##
## results = bl.pivot(index= 'hospid', columns='lupus',
##         values = 'OE')
## results['RR'] = results[1]/results[0]
## results = results.reset_index()
##
## Looking at hospital characteristics

# bl1 = dat1[['hospid','highvolume','northeast','midwest','south','west', 'rural','smallurban','largeurban','bed1','bed2','bed3']]
# bl2 = results[['hospid','RR']]
# blah = bl1.set_index('hospid').join(bl2.set_index('hospid')).reset_index().drop_duplicates()
#
#
#
# from sklearn import tree
# from sklearn.tree import DecisionTreeRegressor
# import graphviz
#
# dt = DecisionTreeRegressor(max_depth=3, min_samples_leaf=5)
# dt.fit(blah.drop('RR', axis=1).values, blah.RR.values)
#
# dot_data = tree.export_graphviz(dt, out_file=None,
#         feature_names = blah.columns[:-1])
# graph = graphviz.Source(dot_data)
# graph
