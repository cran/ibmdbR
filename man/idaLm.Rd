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
\alias{predict.idaLm}
\alias{plot.idaLm}
 
\title{Linear regression}
\description{ This function performs linear regression on the contents of an \code{\link{ida.data.frame}}. }
\usage{

idaLm(form, idadf, modelname = NULL, dropModel = TRUE, limit = 25)

\method{print}{idaLm}(x, ...)
\method{predict}{idaLm}(object, newdata, id, outtable = NULL, ...)
\method{plot}{idaLm}(x, names = TRUE, max_forw = 50, max_plot = 15, order = NULL,
lmgON = FALSE, backwardON = FALSE, ...)
}

\arguments{
    \item{form}{A \code{formula} object that specifies both the name of the column that contains the continuous  
	target variable and either a list of columns separated by plus symbols or a single period (to specify that all other columns in the ida.data.frame are to be used as predictors).
	The specified columns can contain continuous or categorical values. 
	The specified formula cannot contain transformations.}
    \item{idadf}{An ida.data.frame that contains the input data for the function.}
    \item{modelname}{Name of the model that will be created in the database.}
 %   \item{storeCovMat}{logical: Indicates whether the covariance Matrix shall be stored in a Table and in the R object.}
    \item{dropModel}{logical: If TRUE the in database model will be dropped after the calculation.}
 %   \item{clearExisting}{logical: If TRUE an existing model with the name modelname will be dropped and a new one will be created.}
    \item{limit}{The maximum number of levels for a categorical column. Is set constant to 25.
                 Parameter only exists for consistency with older version of idaLm.}
  	\item{x}{An object of the class \code{idaLm}.}
  	\item{object}{object of the class \code{idaLm}}
  	\item{newdata}{an ida.data.frame that contains data that will be predicted.}
  	\item{id}{the name of the id column in newdata}
  	\item{outtable}{The name of the table the results will be written in.}
  	\item{names}{\code{logical}: If set TRUE then the plot will contain the names of the attributes instead of numbers.}
    \item{max_forw}{\code{integer}: The maximum number of iterations the heuristic forward/backward will be calculated.}
    \item{max_plot}{\code{integer}: The maximum number of attributes that will appear in the plot. It must be bigger than 0.}
	\item{order}{Vector of attribute names. Method will calculate the value of the models with the attributes in the order of the vector and plot the value for each of it.}
    \item{lmgON}{\code{logical}: If set TRUE the method will calculate the importance metric lmg. This method has exponential runningtime and is not supported for more than 15 attributes}
  	\item{backwardON}{\code{logical}: If set TRUE the method will calculate the backward heuristic. By default (FALSE) it will do the forward heuristic.}
  	\item{...}{Additional parameters.}
    }
	
\value{The procedure returns a linear regression model of class \code{idaLm}.}

\details{
The \code{idaLm} function computes a linear regression model by extracting a covariance matrix and 
computing its inverse. This implementation is optimized for problems that involve a large number of 
samples and a relatively small number of predictors. The maximum number of columns is 78.

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
\itemize{
\item The slope and y-intercept of the line that best fits the input data
\item The standard error
\item The t-value
\item The p-value
}
$Loglike is the log likelihood ratio. 

$AIC is the Akaike information criterion. This is a measure of the relative quality of the model.

$BIC is the Bayesian information criterion. This is used for model selection.

$CovMat the Matrix used in the calculation ("Covariance Matrix"). This matrix is necessary for the Calculation in plot.idaLm and the statistics.

$card the number of dummy variables created for categorical columns and 1 for numericals.

$model the in database modelname of the idaLm object.

$numrow the number of rows of the input table that do not contain NAs.

$sigma the residual standard error.

The \code{plot.idaLm} function uses \eqn{R^2} as a measure of quality of a linear model.
\eqn{R^2} compares the variance of the predicted values and the variance of the actual values
of the target variable.

$First:            Returns the \eqn{R^2} value of the linear model for each attribute alone.

$Usefulness:       Returns the \eqn{R^2} value reduction of the linear model with all
                   attributes to the linear model with one attribute taken away.
              
$Forward_Values:   Is only calculated if backwardON=FALSE. This is a heuristic that adds in         
                   each step the attribute which has the most \eqn{R^2} increase.
                 
$LMG:              Is only calculated if lmgON=TRUE. It returns the increase of \eqn{R^2} of
                   each attribute averaged over every possible permutation. By grouping some
                   of the permutations we only need to average over every possible subset. 
                   For n attributes there are \eqn{2^n} subsets. So LMG is an algorithm with
                   exponential runningtime and is not recommended for more than 15
                   attributes.

$Backward_Values:  Is only calculated if backwardON=TRUE. Similar to the forward heuristic.
                   This time we choose in each step of the algorithm that has minimal
                   \eqn{R^2} reduction when taking it out of the model, starting with all
                   attributes.
                   
$Model_Values:     Is only calculated if order is a vector of attributes. In this case the
                   function calculates the \eqn{R^2} value for the models that we get when
                   we add one attribute of order in each step.
                    
RelImpPlot.png:    If lmgON=FALSE. This plot shows a stackplot of the values
                   Usefulness,First and the Model_Value of the heuristic. Note that usually                     Usefulness<First<Model_Value and that the bars overlap each other.
                   If lmgON=TRUE. This plot shows the LMG values of the
                   attributes in the order of the heuristic forward, backward or order.
}

\examples{
\dontrun{
#Create a pointer to table IRIS
idf <- ida.data.frame("IRIS")

#Calculate linear model in-db
lm1 <- idaLm(SepalLength~., idf)

library(ggplot2)
plot(lm1)
}
}