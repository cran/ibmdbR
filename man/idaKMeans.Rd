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
\name{idaKMeans}
\alias{idaKMeans}
\alias{print.idaKMeans}
\alias{predict.idaKMeans}

\title{k-means clustering}
\description{This function generates a k-means clustering model based on the contents of a IDA data frame (\code{\link{ida.data.frame}}).	
}
\usage{
idaKMeans(
	data,
	id, 
	k=3,
	maxiter=5,
	distance="euclidean",
	outtable=NULL,
	randseed=12345,
	statistics=NULL,
	modelname=NULL
)

\method{print}{idaKMeans}(x,...)  
\method{predict}{idaKMeans}(object, newdata, id,...)  
}

\arguments{
	\item{data}{A IDA data frame that contains the input data for the function. The input IDA data frame 
	must include a column that contains a unique ID for each row.}
	\item{id}{The name of the column that contains a unique ID for each row of the input data.}
	\item{k}{The number of clusters to be calculated.} 
	\item{maxiter}{The maximum number of iterations to be used to calculate the k-means clusters. A larger 
	number of iterations increases both the precision of the results and the amount of time required to 
	calculate them. }
	\item{distance}{The distance function that is to be used. This must be set to \code{"euclidean"}, which 
	causes the squared Euclidean distance to be used.}
	\item{outtable}{The name of the output table that is to contain the results of the operation. When NULL 
	is specified, a table name is generated automatically.} 
	\item{randseed}{The seed for the random number generator.}
	\item{statistics}{Denotes which statistics to calculate. Allowed values are \code{"none"},\code{"columns"} and \code{"all"}. If NULL, the default of the database system will be used.}
	\item{modelname}{The name under which the model is stored in the database.
	This is the name that is specified when using functions such as \code{\link{idaRetrieveModel}} or \code{\link{idaDropModel}}.}
	\item{object}{An object of the class \code{idaKMeans} to be predicted.} 
	\item{x}{An object of the class \code{idaKMeans} to be printed.} 
	\item{newdata}{A IDA data frame that contains the data to which to apply the model.} 
	\item{...}{Additional parameters to pass to the print or predict method.} 
}

\details{
	The idaKMeans function calculates the squared Euclidean distance between rows, and groups them into clusters.  
	Initial clusters are chosen randomly using a random seed, and the results are adjusted iteratively until 
	either the maximum number of iterations is reached or until two iterations return identical results. 
	Variables with missing values are set zero for distance calculation.

	The parameter \code{modelname} can be used to decide under which name the model is stored in the database.
    This name can be used to retrieve or drop the model later (\code{\link{idaRetrieveModel}},\code{\link{idaDropModel}}).
	
    The output of the print function for a idaKMeans object is:

	- A vector containing a list of centers
	- A vector containing a list of cluster sizes
	- A vector containing a list of the number of elements in each cluster
	- A data frame or the name of the table containing the calculated cluster assignments
	- The within-cluster sum of squares (which indicates cluster density)
	- The names of the slots that are available in the idaKMeans object
}

\value{
	The idaKMeans function returns an object of class \code{idaKMeans} and \code{kmeans}.
}

\seealso{\code{\link{idaRetrieveModel}}, \code{\link{idaDropModel}}, \code{\link{idaListModels}} }

\examples{\dontrun{

#Create ida data frame
idf <- ida.data.frame("IRIS")

#Create a kmeans model stored in the database as KMEANSMODEL
km <- idaKMeans(idf, id="ID",modelname="KMEANSMODEL") 
	
#Print the model
print(km)

#Predict the model
pred <- predict(km,idf,id="ID")

#Inspect the predictions
head(pred)
	
}}