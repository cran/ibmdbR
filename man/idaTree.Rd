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

\name{idaTree}
\alias{idaTree}
%\alias{print.idaTree}
\alias{plot.idaTree}
\alias{predict.idaTree}
\title{Decision and Regression tree}
\description{This function generates a tree model based on the contents of an IDA data frame (\code{\link{ida.data.frame}}).
}

\usage{
idaTree( form, data, id, minsplit=50, maxdepth=10, qmeasure=NULL,
         minimprove=0.01, eval=NULL, valtable=NULL, modelname=NULL)

%\method{print}{idaTree}(object) 
\method{plot}{idaTree}(x,...)  
\method{predict}{idaTree}(object, newdata, id, ...)  

}
\arguments{
	\item{form}{A \code{formula} object that specifies both the name of the column that contains the categorical 
	target variable and either a list of columns separated by plus symbols (each column corresponds to one predictor 
	variable) or a single period (to specify that all other columns in the IDA data frame are to be used as predictors.}
	\item{data}{An IDA data frame that contains the input data for the function. The input IDA data frame 
	must include a column that contains a unique ID for each row.}	
	\item{id}{The name of the column that contains a unique ID for each row of the input data.}
	\item{minsplit}{The minimum number of rows a node must contain to be split further.}
  	\item{maxdepth}{The maximum depth (that is, the number of hierarchical levels) of the generated tree.}
  	\item{qmeasure}{The measure that is to be used to prune the tree. 
	For a decision tree, allowed values are \code{"Acc"} (this is the default) and \code{"wAcc"}. 
	For a regression tree, allowed values are \code{"mse"} (this is the default), \code{"r2"}, \code{"pearson"}, and \code{"spearman"}.}  	
	\item{minimprove}{The minimum improvement. A node is not split further unless the split improves the 
	class impurity by at least the amount specified for this parameter.}
	\item{eval}{The criterion that is to be used to calculate each split.
	For a decision tree, allowed values are \code{"entropy"} (this is the default) and \code{"gini"}.
	For a regression tree, the only allowed value is \code{"variance"} (this is the default).}
	\item{valtable}{When the output tree is to be pruned using external data, use this parameter 
	to specify the fully-qualified name of the table that contains that data. Otherwise, specify NULL.}	
	\item{modelname}{The name under which the model is stored in the database.
	This is the name that is specified when using functions such as \code{\link{idaRetrieveModel}} or \code{\link{idaDropModel}}.}
	\item{object}{An object of the class \code{idaTree}.}
	\item{x}{An object of the class \code{idaTree}.}
	\item{newdata}{A IDA data frame that contains the data to which to apply the model.} 
	\item{...}{additional arguments to be passed to plot or predict.}
}
\details{
	
	The idaTree function uses a top-down, iterative procedure to generate a decision-tree or regression-tree model,
	depending on the type of the target variable. The resulting model comprises a network of nodes and connectors, 
	and each subnode is the endpoint of a binary split.
	
	A node is not split further when any of the following are true:
	\itemize{
	\item The node has a uniform class (and therefore cannot be split further).
	\item Additional splits do not improve the class impurity by at least the amount specified by \code{minimprove}.
	\item The number of rows contained by the node is less than the value specified by \code{minsplit}.
	\item The tree depth reaches the value specified by \code{maxdepth}.
	}
    If variable that is used to determine a split does not have a value, the corresponding row remains in the node that is being split.

	The output of the print function for a idaTree object is a textual description of the corresponding model.

	The output of the plot function for a idaTree object is a graphical representation of the corresponding model.

	Models are stored persistently in the database under the name \code{modelname}. Model names cannot have more than 64 characters and
    cannot contain white spaces. They need to be quoted like table names, otherwise they will be treated upper case by default. Only one
    model with a given name is allowed in the database at a time. If a model with \code{modelname} already exists, you need to drop it with \code{idaDropModel}
    first before you can create another one with the same name. The model name can be used to retrieve the model later (\code{\link{idaRetrieveModel}}).


	The \code{predict.idaTree} method applies the model to the data in a table and returns a IDA data frame that contains
	a list of tuples, each of which comprises one row ID and one prediction.
}

\value{
	The idaTree function returns an object of classes \code{idaTree} and \code{rpart}.
}

\seealso{\code{\link{idaRetrieveModel}}, \code{\link{idaDropModel}}, \code{\link{idaListModels}} }

\examples{\dontrun{

#Create a pointer to the table IRIS
idf <- ida.data.frame('IRIS')

#Create a tree model
tr <- idaTree(Species~.,idf,"ID",modelname="MYTREEMODEL")

#Print the model
print(tr)

#Plot the model
plot(tr)

#Apply the model to data
pred <- predict(tr,idf,id="ID")

#Inspect the predictions
head(pred)

}

}


