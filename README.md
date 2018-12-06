# ADS Project 5: 

Term: Fall 2018

+ Group 3
+ Kaggle Competition: Predicting Transactional Revenue for the google merchandise web store
+ Team members
	+ Atishay Sehgal
	+ Deepika S Namboothiri
	+ Sam Kolins
	+ Sarah Wu
	+ Wenting Yu
+ Project summary: We investigate different methods to accurately predict revenue for a Google Webstore client. The catch here is that the revenue is very sparse or in other words, we're faced with an Imbalanced regression problem. We attempt a two pronged approach: A one step model approach - where we use just one model to solve the problem; A two step model approach - where we first classify into users who pay and those who don't and run the regression based on what our classification model does. We implement a range of models based on a lot of discussions and thought.
	
**Contribution statement**: 
+ Team members
	+ Atishay Sehgal: Cleaned, Preprocessed and Structured the data. Did the Exploratory Analysis. Created the following models: One Step Xgboost, One Step Xgboost + Tweedie, Two step models: 6 classification models - 3 classification trees (unsampled, ROSE sampled and SMOTE sampled) and 3 boosted classification trees (unsampled, ROSE sampled and SMOTE sampled). Two step models: 2 regression models - xgboost and multivariate adaptive regression splines. Contributed in some way to all processes.
	+ Deepika S Namboothiri: Research on attribution model, crossvalidated xgboost and presentation.
	+ Sam Kolins: Worked on the following models: one-stage LASSO, LASSO/Ridge on all paid data, penalized LDA, two-stage penalized LDA+LASSO/Ridge, and wrote the main.Rmd introduction.
	+ Sarah Wu: Wrote the entire report for our project.
	+ Wenting Yu:  

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── data/
├── doc/
├── figs/
├── lib/
└── output/
```

Please see each subfolder for a README file.
