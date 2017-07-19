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
\name{idaArule} 
\alias{idaArule}
\alias{idaApplyRules}
 
\title{Association Rule Mining}
\description{
	This function calculates association rules on a database table.
}
\usage{
idaArule(
		data,
		tid,
		item,
		maxlen=5,
		maxheadlen=1,
		minsupport=NULL,
		minconf=0.5,
		nametable=NULL,
		namecol=NULL,
		modelname=NULL
)

idaApplyRules(modelname, newdata, tid, item, nametable=NULL, namecol=NULL, ...)

}

\arguments{
	\item{data}{An \code{ida.data.frame} object pointing to the data to be mined.}
	\item{tid}{Input table column that identifies the transactions by an id.}
	\item{item}{Input table column that identifies items in transactions.}
	\item{maxlen}{The maximum length of a rule. Must be two or larger.}
	\item{maxheadlen}{The maximum length of the rule head.}
	\item{minsupport}{The minimal support of a rule to be considered.}
	\item{minconf}{The minimal confidence of a rule to be considered.}
	\item{nametable}{A database table containing a mapping between the items in the input table and their name. 
	                 The table must contain at least two columns, the first column is named as the column indicated in the item parameter 
	                 and the second column is named as indicated in parameter namecol.}
	\item{namecol}{The name of the column containing the item name in case \code{nametable} was specified.}
	\item{modelname}{The name of the model in-database. If NULL, it is automatically generated.}
	\item{newdata}{A table to which to apply the rules.}
	\item{...}{Additional stored procedure parameters.}
}


\details{
\code{idaArule} finds association rules in transactional data. The input data must be in transactional format, thus each
row of the table contains exactly one item and an identifier of which transaction this item is assigned to. These two
columns need to be specified using the \code{tid} and \code{item} parameters. If the items are referred to with numeric IDs in the
transaction table, it is often useful to add a name mapping to produce rules that contain names instead of item IDs. This can be
achieved by setting the parameters \code{nametable} and \code{namecol}.

Models are stored persistently in database under the name \code{modelname}. Model names cannot have more than 64 characters and
cannot contain white spaces. They need to be quoted like table names, otherwise they will be treated upper case by default. Only one
model with a given name is allowed in the database at a time. If a model with \code{modelname} already exists, you need to drop it with \code{idaDropModel}
first before you can create another one with the same name. The model name can be used to retrieve the model later (\code{\link{idaRetrieveModel}}).
	
\code{idaApplyRules} applies a rule model stored in the database to a table with transactions. 

}

\value{\code{idaArule} returns an object of class \code{rules} compatible with the packages \code{arules} and \code{arulesViz}
\code{idaApplyRules} returns an object of class \code{ida.data.frame}, pointing to a table that contains a mapping between transaction IDs and matched 
rules.
}

\examples{\dontrun{

idf <- ida.data.frame("GOSALES.ORDER_DETAILS")

r <- idaArule(idf,tid="ORDER_NUMBER",item="PRODUCT_NUMBER",minsupport=0.01)

inspect(r)

applyResult <- idaApplyRules(idaGetModelname(r),idf,"ORDER_NUMBER","PRODUCT_NUMBER")
	
}}