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
\name{idaTwoStep}
\alias{idaTwoStep}
\alias{print.idaTwoStep}
\alias{predict.idaTwoStep}

\title{two step clustering}
\description{This function generates a two step clustering model based on the contents of an IDA data frame (\code{\link{ida.data.frame}}).	
}
\usage{
idaTwoStep( data, id, k=3, maxleaves=1000, distance="euclidean", outtable=NULL, 
            randseed=12345, statistics=NULL, modelname=NULL)

\method{print}{idaTwoStep}(x,...)  
\method{predict}{idaTwoStep}(object, newdata, id,...)  
}

\arguments{
	\item{data}{A IDA data frame that contains the input data for the function. The input IDA data frame 
	must include a column that contains a unique ID for each row.}
	\item{id}{The name of the column that contains a unique ID for each row of the input data.}
	\item{k}{The number of clusters to be calculated.} 
	\item{maxleaves}{The maximum number of leaf nodes in the initial clustering tree. When the tree contains maxleaves 
	leaf nodes, the following data records are aggregated into clusters associated with the existing leaf nodes.}
	\item{distance}{The distance function that is to be used. This can be set to \code{"euclidean"}, which 
	causes the squared Euclidean distance to be used, or \code{"norm_euclidean"}, which causes normalized euclidean distance to be used.}
	\item{outtable}{The name of the output table that is to contain the results of the operation. When NULL 
	is specified, a table name is generated automatically.} 
	\item{randseed}{The seed for the random number generator.}
	\item{statistics}{Denotes which statistics to calculate. Allowed values are \code{"none"},\code{"columns"} and \code{"all"}. If NULL, the default of the database system will be used.}
	\item{modelname}{The name under which the model is stored in the database.
	This is the name that is specified when using functions such as \code{\link{idaRetrieveModel}} or \code{\link{idaDropModel}}.}
	\item{object}{An object of the class \code{idaTwoStep} to be used for prediction, i.e. for applying it to new data.} 
	\item{x}{An object of the class \code{idaTwoStep} to be printed.} 
	\item{newdata}{A IDA data frame that contains the data to which to apply the model.} 
	\item{...}{Additional parameters to pass to the print or predict method.} 
}

\details{
	
	The idaTwoStep clustering function distributes first the input data into a hierarchical tree structure 
	according to the distance between the data records where each leaf node corresponds to a (small) cluster.
	Then idaTwoStep reduces the tree by aggregating the leaf nodes according to the distance function until k clusters remain.
	 
	
	Models are stored persistently in database under the name \code{modelname}. Model names cannot have more than 64 characters and
    cannot contain white spaces. They need to be quoted like table names, otherwise they will be treated upper case by default. Only one
    model with a given name is allowed in the database at a time. If a model with \code{modelname} already exists, you need to drop it with \code{idaDropModel}
    first before you can create another one with the same name. The model name can be used to retrieve the model later (\code{\link{idaRetrieveModel}}).
	
    The output of the print function for a idaTwoStep object is:
    
    \itemize{

	\item A vector containing a list of centers
	\item A vector containing a list of cluster sizes
	\item A vector containing a list of the number of elements in each cluster
	\item A data frame or the name of the table containing the calculated cluster assignments
	\item The within-cluster sum of squares (which indicates cluster density)
	\item The names of the slots that are available in the idaTwoStep object
	}
}

\value{
	The idaTwoStep function returns an object of class \code{idaTwoStep} and \code{TwoStep}.
}

\seealso{\code{\link{idaRetrieveModel}}, \code{\link{idaDropModel}}, \code{\link{idaListModels}} }

\examples{\dontrun{

#Create ida data frame
idf <- ida.data.frame("IRIS")

#Create a TwoStep model stored in the database as TwoStepMODEL
tsm <- idaTwoStep(idf, id="ID",modelname="TwoStepMODEL") 
	
#Print the model
print(tsm)

#Predict the model
pred <- predict(tsm,idf,id="ID")

#Inspect the predictions
head(pred)
	
}}