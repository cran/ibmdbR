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
\name{idaNaiveBayes}
\alias{idaNaiveBayes}
\alias{predict.idaNaiveBayes}
\alias{print.idaNaiveBayes}

\title{Naive Bayes Classifier}

\description{
	This function generates a Naive Bayes classification model based on the contents of a IDA data frame (\code{\link{ida.data.frame}}).
}
\usage{
idaNaiveBayes(form,data,id="id",modelname=NULL)

\method{predict}{idaNaiveBayes}(object,newdata,id,...)
\method{print}{idaNaiveBayes}(x,...)  

}
\arguments{
	\item{form}{A \code{formula} object that describes the model to fit.}
	\item{data}{A \code{ida.data.frame} object.}
	\item{id}{The name of the column that contains unique IDs.}
	\item{modelname}{Name for the model. Will be created automatically unless specified otherwise.}
	\item{object}{An object of the class \code{idaNaiveBayes} to be predicted.}
	\item{newdata}{A IDA data frame that contains the data to which to apply the model.}
	\item{x}{An object of the class \code{idaNaiveBayes} to be printed.} 
	\item{...}{Additional parameters to pass to the predict method.} 
	
}
\details{
	\code{idaNaiveBayes} builds a Naive Bayes classification model, thus a model that assumes independence
	of input variables with respect to the target variable.
	
	Continuous input variables are discretized using
	equal width discretization. Missing values are ignored on a record and attribute level when calculating the
	conditional probabilities. 
}
\value{
	The function \code{idaNaiveBayes} returns an object of class \code{"idaNaiveBayes"} and \code{"naiveBayes"} compatible
	with Naive Bayes objects produced by the e1071 package.
	
	The \code{predict.idaNaiveBayes} method applies the model to the data in a table and returns a IDA data frame that contains
	a list of tuples, each of which comprises one row ID and one prediction.
}
\examples{
\dontrun{
#Create ida data frame
idf <- ida.data.frame("IRIS")

#Create a naive bayes model
nb <- idaNaiveBayes(Species~SepalLength,idf,"ID")

#Print the model
print(nb)

#Apply the model to data
idf2 <- predict(nb,idf,"ID")

#Inspect the results
head(idf2)

}
}
