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
\name{idaShowTables}
\alias{idaShowTables}
\title{Return a list of tables}
\description{
  Returns a data frame that contains the names of the tables contained in the
  current database.
}
\usage{
idaShowTables(showAll=FALSE, matchStr=NULL, schema=NULL, accelerated=FALSE)
}

\arguments{
  \item{showAll}{List all tables that are listed in the catalog of the current database (TRUE) or only those tables that are in the current schema (FALSE).}
  \item{matchStr}{If not NULL, only tables that contain the character string in this argument will be returned.}
  \item{schema}{If not NULL, only tables with this schema will be returned. This parameter is ignored if showAll=FALSE.}
  \item{accelerated}{Valid for DB2 for z/OS connections only. If TRUE, only accelerated tables will be returned.}
}

\value{
  A data frame with the columns Schema, Name, Owner, and Type. 
  For DB2 for z/OS connections the columns Acceleratorname and Enable are included as well.
}
\examples{
\dontrun{

#Get a list of all tables in the current schema
tabs <- idaShowTables()

}
}
