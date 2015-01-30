### AIC Number: Overview and its use in R

#### Overview

(from Wikipedia) Akaike information criterion (AIC) is a measure of the relative quality of a statistical model for a given set of data. That is, given a collection of models for the data, AIC estimates the quality of each model, relative to the other models. Hence, AIC provides a means for model selection. It is formally defined as:

![AIC = 2k - 2ln(L)](http://upload.wikimedia.org/math/7/5/a/75a00ba4d67592a77bb87db1c723ddfe.png)

where L be the maximized value of the likelihood function ( ![likelihood function = L(theta|x) = P(x|theta)](http://upload.wikimedia.org/math/5/e/3/5e3baf3649cc06f012be6cc5bfa7e86d.png) ) for the model and k is the number of parameters in the model.

#### So what?

AIC gives us an opportunity to compare a set of models, and evaluate a set of parameters, relative to each other. We learn nothing about the quality of the models themselves, but can at least identify which among a set of models warrant further investigation.

#### Example

The example below uses the Kaggle Titanic dataset. It creates 3 different linear models with different parameter sets to see how AIC changes. This is a very basic example.

	> trainDF <- read.csv('train.csv')
	> testLM <- lm(Survived ~ Age + SibSp + Pclass + Sex, 
					data=subset(trainDF, !is.na(Age) & !is.na(SibSp)))
	> AIC(testLM)
	[1] 875.0388
	> testLM_1 <- lm(Survived ~ Age + Pclass, 
					data=subset(trainDF, !is.na(Age)))
	> testLM_2 <- lm(Survived ~ Age + Pclass + Sex, 
					data=subset(trainDF, !is.na(Age)))
	> AIC(testLM, testLM_1, testLM_2)
         df      AIC
	testLM    6 658.4335
	testLM_1  4 876.7700
	testLM_2  5 667.707

The results of the AIC comparison suggest that the parameters in testLM_2 are the best set to move forward with.

There is also a stepwise AIC finctional capability contained within the MASS package that may be of some use, though I have not had an opportunity to explore its meaning

	> library(MASS)
	> step <- stepAIC(testLM, direction="both")
	Start:  AIC=-1369.81
	Survived ~ Age + SibSp + Pclass + Sex
	
	         Df Sum of Sq    RSS     AIC
	<none>                103.38 -1369.8
	- SibSp   1     1.645 105.02 -1360.5
	- Age     1     4.971 108.35 -1338.3
	- Pclass  1    17.439 120.82 -1260.5
	- Sex     1    37.031 140.41 -1153.2

#### References
[1] [R Documentation regarding AIC](https://stat.ethz.ch/R-manual/R-patched/library/stats/html/AIC.html)

[2] [Wikipedia Article](http://en.wikipedia.org/wiki/Akaike_information_criterion)

[3] [Quick-R Multiple Linear Regression Page, section re: Variable Selection](http://www.statmethods.net/stats/regression.html)
