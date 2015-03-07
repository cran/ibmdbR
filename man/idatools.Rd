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
\name{idaDeleteTable, idaExistTable, idaGetValidTableName}
\alias{idaDeleteTable}
\alias{idaExistTable}
\alias{idaGetValidTableName}
\alias{idaIsView}
\title{Miscellaneous tools}
\description{
  These functions simplify working with database tables.
}
\usage{

idaDeleteTable(table)

idaExistTable(tableName)

idaIsView(tableName)

idaGetValidTableName(prefix="DATA_FRAME_")
}
\arguments{
  \item{table}{The name of a database table or a \code{ida.data.frame}.}
  \item{tableName}{The name of a database table.}
  \item{prefix}{Keyword used to specify the prefix of a table name.} 
}

\details{
  Use the \code{idaDeleteTable} function to drop the specified table. 
  The specified table must exist in the current database. 
  
  Use the \code{idaExistTable} function to determine whether the specified table exists in the database.
  
  Use the \code{idaGetValidTableName} function to obtain a table name that is not yet in use. This name will be the specified or default prefix followed by a number, for example, data_frame_7.
}

\value{

  The \code{idaDeleteTable} function does not return a value.
  
  The \code{idaExistTable} function returns a logical value (TRUE or FALSE) that indicates
  whether the specified table exists in the database.
  
  The \code{idaGetValidTableName} function returns a string representing a table name.
}

\examples{
\dontrun{

#Check whether a table with a given name exists
idaExistTable('IRIS')

#Create a pointer to the table
idf <- ida.data.frame('IRIS')

#Obtain a unique table name for a copy
copyTableName <- idaGetValidTableName(prefix = "COPY_")

#Create a copy of the original table
idfCopy <- as.ida.data.frame(as.data.frame(idf),copyTableName)

#Delete the copy again
idaDeleteTable(copyTableName)

}
}

