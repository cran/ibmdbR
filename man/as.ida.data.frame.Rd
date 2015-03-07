% 
% Copyright (c) 2010, 2015, IBM Corp. All rights reserved. 
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
\name{as.ida.data.frame}
\alias{as.ida.data.frame}
\title{Convert an R object to a IDA data frame}
\description{
  This function creates a IDA data frame \code{\link{ida.data.frame}} from a local R \code{data.frame}
  by creating a table in the data base.
}

\usage{
as.ida.data.frame(x,table=NULL,clear.existing=FALSE,case.sensitive=TRUE)
}

\arguments{
  \item{x}{The name of the input object that is to be converted to a IDA data frame.}
  \item{table}{The name of the database table that is to be created to hold the 
  contents of the IDA data frame. The specified name is folded to uppercase.
  If this parameter is not specified, a name is generated automatically.}
  \item{clear.existing}{If the \code{table} parameter is specified and a table with the 
  specified name already exists, this parameter specifies whether the existing table is to be 
  dropped (TRUE) or whether the \code{as.ida.data.frame} statement is to be ignored and 
  a warning message issued (FALSE).} 
  \item{case.sensitive}{If the \code{table} parameter is specified for an existing table, 
  this parameter specifies whether the column names in that table name are to be treated
  as case-sensitive (TRUE) or not case-sensitive (FALSE).}
}

\value{
  A IDA data frame that points to the newly created table.
}

\seealso{ \code{\link{as.data.frame}} }

\examples{
\dontrun{

#Add an ID column to iris
iris2 <- iris
iris2$ID <- 1:150

#Upload it and overwrite if already exists
idf <- as.ida.data.frame(iris2,"IRIS",clear.existing=T)
	
}
}