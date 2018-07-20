import numpy as np
import pandas as pd
import xgboost as xgb
from sklearn.model_selection import train_test_split, GridSearchCV
#from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import roc_auc_score, accuracy_score
import matplotlib.pyplot as plt
import seaborn as sns
import operator
import joblib
from dask_ml.wrappers import ParallelPostFit



pd.__version__
dat = pd.read_csv('indiv_dat2.csv') # Generated in DataMunging.R, no race
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

def create_feature_map(features):
	outfile = open('xgb.fmap', 'w')
	i = 0
	for feat in features:
		outfile.write('{0}\t{1}\tq\n'.format(i, feat))
		i = i + 1
	outfile.close()

ftrs = list(np.array(indiv.drop('dead', axis = 1).columns))

features = ['Age','SES_Q1','SES_Q2','SES_Q3','SES_Q4','Elixhauser', 'Female','Male', 'Ventilator','Ventilator',
    'Cardiac fail','Cardiac fail','Neuro fail','Neuro fail', 'Heme fail','Heme fail','Liver fail','Liver fail',
    'Renal fail','Renal fail']
create_feature_map(ftrs)
cvresult = xgb.cv(params, dFull, num_boost_round=500, nfold=5, early_stopping_rounds = 5, seed = 2049, metrics=['auc'])

# 10 rounds gets to 99.7% of max test AUC
params['n_estimators'] = 10
clf = xgb.XGBRegressor(max_depth = 6,
        objective = 'binary:logistic',
        learning_rate = 0.1,
        subsample = 1,
        seed = 2049,
        n_estimators = 10)
clf.fit(X,y)
import joblib
joblib.dump(clf, 'PredictedModel.joblib.dat')
# clf = joblib.load('PredictedModel.joblib.dat')
import shutil
import os
shutil.copy2('PredictedModel.joblib.dat', os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data'))

## An alternative random forest model with better characteristics
crf = RandomForestRegressor(n_estimators = 250, min_samples_leaf = 10)
crf.fit(X,y)
pr = crf.predict(X)
joblib.dump(crf, 'PredictedModelRF.joblib.dat')
shutil.copy2('PredictedModelRF.joblib.dat', os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data'))

# Calibration
from sklearn.metrics import brier_score_loss
p = clf.predict(X)
brier_score_loss(y, p)
from sklearn.calibration import calibration_curve
frac_pos, mean_pred = calibration_curve(y, p, n_bins=20)
frac_pos1, mean_pred1 = calibration_curve(y, pr, n_bins = 20)
plt.plot(mean_pred, frac_pos, 'sg-', mean_pred1, frac_pos1, 'sb-', [0,1],[0,1], 'k:')
plt.show()

# ROC curves
from sklearn.metrics import roc_curve
fpr = dict()
tpr = dict()
fpr[0],tpr[0],_ = roc_curve(y, p)
fpr[1],tpr[1],_ = roc_curve(y,pr)
plt.plot(fpr[0],tpr[0], color = 'blue', lw = 2, label = 'XGBoost (AUC = %0.2f)' % roc_auc_score(y,p))
plt.plot(fpr[1],tpr[1], color = 'green', lw = 2, label = 'Random Forest (AUC = %0.2f)' % roc_auc_score(y,pr))
plt.plot([0,1],[0,1], color = 'navy', linestyle = '--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver Operating Curve (ROC)')
plt.legend(loc = 'lower right')


# Feature importances
importance = clf.get_booster().get_score(fmap = 'xgb.fmap', importance_type = 'gain')
importance = sorted(importance.items(), key = operator.itemgetter(1))
df_importance = pd.DataFrame(importance, columns = ['feature','fscore'])
df_importance['feature'] = pd.Series(['SES_Q2','SES_Q4','Gender','SES_Q1','Neuro fail','SES_Q3','Age','Renal fail','Liver fail','Elixhauser','Bone marrow failure','Cardiac failure','Ventilator'])
df_importance = df_importance.sort_index(ascending=False)
df_importance['prop_fscore'] = df_importance.fscore/np.max(df_importance.fscore)
plt.figure()
df_importance[df_importance.fscore > 10].plot()
df_importance[df_importance.fscore > 10].plot(kind = 'barh', x = 'feature',y = 'fscore', legend = False)
plt.show()
plt.gcf().savefig('feature_importances.png')

# Predictions from model
risk = clf.predict(X)
dat['risk'] = risk
dat.to_csv('indiv_dat1_risk.csv', index = False)
shutil.copy2('indiv_dat1_risk.csv', os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data'))

dat2 = dat.copy()
dat2['risk'] = crf.predict(X)
dat2.to_csv('indiv_dat1_risk2.csv', index = False)
shutil.copy2('indiv_dat1_risk2.csv', os.path.expanduser('~/Dropbox/NIAMS/Ward/SLE_Infections/data'))


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


plt.plot(compute_RR(dat), compute_RR(dat2), 'k.')
plt.plot([0,8],[0,8], linestyle = '--', color = 'lightgrey')
plt.xlabel('RR from xgboost')
plt.ylabel('RR from randomforest')
plt.show()


results = compute_RR(dat2)

# results = results.loc[~pd.isna(results.RR),:]
results.shape
bl2=dat2[dat2.lupus==0].groupby('hospid').size()
bl3 = dat2.groupby('hospid')['dead'].mean()
bl3.head()
blah = pd.concat([results, bl2, bl3], axis = 1)
blah.columns = ['RR','N','Mortality']
blah.head()
roc_auc_score(dat2.dead, dat2.risk)

blah.plot(x = 'Mortality', y = 'RR', kind='scatter')
plt.show()

plt.scatter(blah.N, blah.Mortality); plt.show()
import seaborn as sns
sns.regplot('Mortality', 'RR', data=blah[blah.RR>0], lowess=True)
plt.plot([0,0.35],[1,1])
plt.plot([0,0.35], [2,2], 'r:')
plt.show()

blah.to_csv('Hosp_indiv1RF_results.csv', index = False)
######################################################################

# Bootstrapping to capture uncertainty of estimates

def model_scoring(mod, d):
    d1 = d.copy() # Otherwise changes happen in place
    indiv = d1.drop(['hospid','lupus'], axis=1)
    X, y = indiv.drop('dead',axis=1).values, indiv['dead'].values
    risk = mod.predict(X)
    d1['risk'] = risk
    return(d1)

D = pd.read_csv('indiv_dat2.csv') # Generated in DataMunging.R
results = compute_RR(model_scoring(crf, D))
np.random.seed(2940)
for i in range(1000):
    print(i)
    # Stratified bootstrap
    D1 = D[D.lupus==1].sample(frac = 1, replace = True, axis = 0)
    D0 = D[D.lupus==0].sample(frac = 1, replace = True, axis = 0)
    Dnew = D1.append(D0)
    #D1 = D.sample(frac = 1, replace = True, axis = 0)
    r = compute_RR(model_scoring(crf, Dnew))
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
