# More_Linear_Regression_Methods
This repository focuses on different linear regression methods which are uncommon.

# Advanced Regression Methods
- Principal Component Regression
In statistics, principal component regression (PCR) is a regression analysis technique that is based on principal component analysis (PCA). Typically, it considers regressing the outcome (also known as the response or the dependent variable) on a set of covariates (also known as predictors, or explanatory variables, or independent variables) based on a standard linear regression model, but uses PCA for estimating the unknown regression coefficients in the model.

In PCR, instead of regressing the dependent variable on the explanatory variables directly, the principal components of the explanatory variables are used as regressors. One typically uses only a subset of all the principal components for regression, thus making PCR some kind of a regularized procedure. Often the principal components with higher variances (the ones based on eigenvectors corresponding to the higher eigenvalues of the sample variance-covariance matrix of the explanatory variables) are selected as regressors. However, for the purpose of predicting the outcome, the principal components with low variances may also be important, in some cases even more important.

One major use of PCR lies in overcoming the multicollinearity problem which arises when two or more of the explanatory variables are close to being collinear.PCR can aptly deal with such situations by excluding some of the low-variance principal components in the regression step. In addition, by usually regressing on only a subset of all the principal components, PCR can result in dimension reduction through substantially lowering the effective number of parameters characterizing the underlying model. This can be particularly useful in settings with high-dimensional covariates. Also, through appropriate selection of the principal components to be used for regression, PCR can lead to efficient prediction of the outcome based on the assumed model.

- Best Subsets Regression
Best subsets regression assess all possible models and displays a subset along with their adjusted R-squared and Mallows' Cp values. Best subsets does not pick a final model for you but it does present you with multiple models and information to help you choose the final model.Best Subsets compares all possible models using a specified set of predictors, and displays the best-fitting models that contain one predictor, two predictors, and so on. The end result is a number of models and their summary statistics. It is up to you to compare and choose one. Sometimes the results do not point to one best model and your judgment is required.


