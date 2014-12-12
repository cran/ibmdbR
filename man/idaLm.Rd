% 
% Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
%   	
% This program is free software: you can redistribute it and/or modify 
% it under the terms of the GNU General Public License as published by 
% the Free Software Foundation, either version 3 of the License, or 
% (at your option) any later version. 
%
% This program is distributed in the hope that it will be useful, 
% but WITHOUT ANY WARRANTY; without even the implied warranty of 
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
% GNU General Public License for more details. 
%
% You should have received a copy of the GNU General Public License 
% along with this program. If not, see <http://www.gnu.org/licenses/>. 
% 
% 
\name{idaLm} 
\alias{idaLm}
\alias{print.idaLm}
 
\title{Linear regression}
\description{ This function performs a linear regression on the contents of a IDA data frame (\code{\link{ida.data.frame}}). }
\usage{
idaLm(form, idadf,limit=25)

\method{print}{idaLm}(x)
}

\arguments{
    \item{form}{A \code{formula} object that specifies both the name of the column that contains the continuous  
	target variable and either a list of columns separated by plus symbols or a single period (to specify that all other columns in the IDA data frame are to be used as predictors).
	The specified columns can contain continuous or categorical values. 
	The specified formula cannot contain transformations.}
    \item{idadf}{A IDA data frame that contains the input data for the function.}
    \item{limit}{The maximum number of distinct values per categorical column. The default is 25.}
	\item{x}{An object of the class \code{idaLm}.}
    }
	
\value{The procedure returns a linear regression model in an object of class \code{idaLm}.}

\details{
The \code{idaLm} function computes a linear regression model by extracting a covariance matrix and 
computing its inverse. This implementation is optimized for problems that involve a large number of 
samples and a relatively small number of predictors. The maximum number of columns is 87.

Missing values in the input table are ignored when calculating the covariance matrix. If this 
leads to undefined entries in the covariance matrix, the function fails. If the inverse of the 
covariance matrix cannot be computed (for example, due to correlated predictors), the Moore-Penrose 
generalized inverse is used instead.
 
The output of the idaLm function has the following attributes:

$coefficients is a vector with two values. The first value is the slope of the line that best 
fits the input data; the second value is its y-intercept.

$RSS is the root sum square (that is, the square root of the sum of the squares).

$effects is not used and can be ignored.

$rank is the rank.

$df.residuals is the number of degrees of freedom associated with the residuals. 

$coefftab is a is a vector with four values:
- The slope and y-intercept of the line that best fits the input data
- The standard error
- The t-value
- The p-value

$Loglike is the log likelihood ratio. 

$AIC is the Akaike information criterion. This is a measure of the relative quality of the model.

$BIC is the Bayesian information criterion. This is used for model selection.
}

\examples{
\donttest{
#Create a pointer to table DB2INST1.SHOWCASE_SYSUSAGE
sysusage<-ida.data.frame('DB2INST1.SHOWCASE_SYSUSAGE')

#Calculate linear model in-db
lm1 <- idaLm(MEMUSED~USERS, sysusage)
}
}