# Project: Can you recognize the emotion from an image of a face? 
<img src="figs/CE.jpg" alt="Compound Emotions" width="500"/>
(Image source: https://www.pnas.org/content/111/15/E1454)

### [Full Project Description](doc/project3_desc.md)

Term: Fall 2019

+ Team 5
+ Team members
	+ Qiwen Gao, qg2165
	+ Sixing Hao, sh3799
	+ Sagar Lal, sl3946
	+ Yakun Wang, yw3211
	+ Xiyi Yan, xy2380

+ Project summary: In this project, we created a classification engine for facial emotion recognition. We tried several different models and feature engineering. In terms of models, we tried gbm's, svm, xgboost, and random forest. For feature engineering, we tried several methods of feature reduction both manual and using a pca. We also generated our own features of accounting for area of several parts of the face, as well as angles between different points on the face. We discovered that an ensemble method using a weighted average between SVM and Xgboost yielded the best accuracy when considering training time and our modified dataset of hand selected points and created features.
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
