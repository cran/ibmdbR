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
\name{idaDivCluster}
\alias{idaDivCluster}
\alias{print.idaDivCluster}
\alias{predict.idaDivCluster}

\title{Hierarchical (divisive) clustering}
\description{This function generates a hierarchical (divisive) clustering model 
 based on the contents of an IDA data frame (\code{\link{ida.data.frame}}) by applying recursively the K-means algorithm.	
}
\usage{
idaDivCluster(
    data,
    id,
    distance="euclidean",
    maxiter=5, 
    minsplit=5,
    maxdepth=3,
    randseed=12345,
    outtable=NULL,
    modelname=NULL
)

\method{print}{idaDivCluster}(x,...)  
\method{predict}{idaDivCluster}(object, newdata, id,...)  
}

\arguments{
	\item{data}{An IDA data frame that contains the input data for the function. The input IDA data frame 
	must include a column that contains a unique ID for each row.}
	\item{id}{The name of the column that contains a unique ID for each row of the input data.}
	\item{distance}{The distance function that is to be used. This can be set to \code{"euclidean"}, which 
	causes the squared Euclidean distance to be used, or \code{"norm_euclidean"}, which causes normalized euclidean distance to be used.}
	\item{maxiter}{The maximum number of iterations to perform in the base K-means Clustering algorithm}
	\item{minsplit}{The minimum number of instances per cluster that can be split.}
	\item{maxdepth}{The maximum number of cluster levels (including leaves).}
	\item{randseed}{The seed for the random number generator.}
	\item{outtable}{The name of the output table that is to contain the results of the operation. When NULL 
	is specified, a table name is generated automatically.} 
	\item{modelname}{The name under which the model is stored in the database.
	This is the name that is specified when using functions such as \code{\link{idaRetrieveModel}} or \code{\link{idaDropModel}}.}
	\item{object}{An object of the class \code{idaDivCluster} to used for prediction, i.e. for applying it to new data.} 
	\item{x}{An object of the class \code{idaDivCluster} to be printed.} 
	\item{newdata}{An IDA data frame that contains the data to which to apply the model.} 
	\item{...}{Additional parameters to pass to the print or predict method.} 
}

\details{
	
	The idaDivCluster clustering function builds a hierarchical clustering model by applying the K-means algorithm 
	recursively in a top-down fashion. The hierarchy of clusters is represented in a binary tree structure (each
    parent node has exactly 2 child nodes). The leafs of the cluster tree are identified by negative numbers.
	
	Models are stored persistently in the database under the name \code{modelname}. Model names cannot have more than 64 characters and
    cannot contain white spaces. They need to be quoted like table names, otherwise they will be treated upper case by default. Only one
    model with a given name is allowed in the database at a time. If a model with \code{modelname} already exists, you need to drop it with \code{idaDropModel}
    first before you can create another one with the same name. The model name can be used to retrieve the model later (\code{\link{idaRetrieveModel}}).
	
    The output of the print function for a idaDivCluster object is:
    
    \itemize{

	\item A vector containing a list of centers
	\item A vector containing a list of cluster sizes
	\item A vector containing a list of the number of elements in each cluster
	\item A data frame or the name of the table containing the calculated cluster assignments
	\item The within-cluster sum of squares (which indicates cluster density)
	\item The names of the slots that are available in the idaDivCluster object.
	}
}

\value{
	The idaDivCluster function returns an object of class \code{idaDivCluster}.
}

\seealso{\code{\link{idaRetrieveModel}}, \code{\link{idaDropModel}}, \code{\link{idaListModels}} }

\examples{\dontrun{

#Create ida data frame
idf <- ida.data.frame("IRIS")

#Create a DivCluster model stored in the database as DivClusterMODEL
dcm <- idaDivCluster(idf, id="ID",modelname="DivClusterMODEL") 
	
#Print the model
print(dcm)

#Predict the model
pred <- predict(dcm,idf,id="ID")

#Inspect the predictions
head(pred)
	
}}