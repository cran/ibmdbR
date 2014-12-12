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
\name{idaQuery, idaScalarQuery}
\alias{idaQuery}
\alias{idaScalarQuery}
\title{Run an SQL query on the database}
\description{
Use these functions to run any SQL query on the database and put the
results into a \code{data.frame}.
}
\usage{
idaQuery(..., as.is=TRUE)

idaScalarQuery(..., as.is=TRUE)
}
\arguments{
  \item{...}{Any number of query parts which are passed to \code{paste}.}
  \item{as.is}{Specifies whether the result columns are to be converted using RODBC type conversions (as.is=FALSE) or left unconverted (as.is=TRUE).
  For more information about RODBC type conversions, see the descriptions of the functions \code{sqlGetResults} and \code{type.convert}.}
}
\details{
All parts of the input query are concatenated with \code{paste(..., sep="")}
and the result is passed to the database.
}
\value{
  The \code{idaQuery} function returns a data frame that contains the
  result of the specified query.

  The \code{idaScalarQuery} function returns the result of the specified query coerced
  to a single scalar value.
}

\examples{
\donttest{
#idaScalarQuery returns a single value
v <- idaScalarQuery("SELECT COUNT(*) FROM DB2INST1.SHOWCASE_SYSTEMS")

#idaQuery returns a data.frame
df <- idaScalarQuery("SELECT * FROM DB2INST1.SHOWCASE_SYSTEMS")

#idaQuery and idaScalarQuery automatically paste all arguments into a single query
#This is convenient if you use variables

tableName <- "DB2INST1.SHOWCASE_SYSTEMS"
df <- idaQuery("SELECT COUNT(*) FROM ",tableName)
}
}
