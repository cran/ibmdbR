% 
% Copyright (c) 2013, 2014, IBM Corp. All rights reserved. 
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
\name{idadf,idaSave,idaUpdate}
\alias{idadf}
\alias{idaSave}
\alias{idaUpdate}

\title{Query, store and update data in the database.}
\description{
These functions allows to query, store and update data in the database.
Usually, it is easier to use \code{\link{idaQuery}},\code{\link{ida.data.frame}}
and \code{\link{as.ida.data.frame}} instead of these methods. 

They can be useful, however, if an explicit connection object is
needed, e.g. if there are several connections to different
databases.

}
\usage{
idadf(idaConn, query)
idaSave(idaConn, dfrm, tblName = "", rowName = "", conType = "odbc")
idaUpdate(db2Conn, updf, dfrm, idaIndex = "", conType = "odbc")

}
\arguments{
  \item{idaConn}{The IDA connection object.}
  \item{db2Conn}{The IDA connection object.}
  \item{query}{A query.}
  \item{dfrm}{A data.frame to store.}
  \item{tblName}{Name of the table to which to store the data.}
  \item{rowName}{Name of the row name column.}
  \item{updf}{Name of the table to update.}
  \item{idaIndex}{Name of the index column.}
  \item{conType}{Type of the connection.}
}

\details{

\code{idadf},\code{idaSave} and \code{idaUpdate} are simple wrappers around the RODBC functions \code{\link[RODBC]{sqlQuery}},
\code{\link[RODBC]{sqlSave}} and \code{\link[RODBC]{sqlUpdate}}.

Usually, it is easier to use \code{\link{idaQuery}},\code{\link{ida.data.frame}}
and \code{\link{as.ida.data.frame}} instead of these methods. 

}

\seealso{\code{\link{idaQuery}},\code{\link{ida.data.frame}},\code{\link{as.ida.data.frame}},\code{\link[RODBC]{sqlQuery}},\code{\link[RODBC]{sqlSave}},\code{\link[RODBC]{sqlUpdate}}}

\examples{
\dontrun{
# create connection to DB
con <- idaConnect("BLUDB", "", "")

# create data.frame from table
df <- idadf(con, "SELECT * FROM IRIS")

# close the connection again
idaClose(con)
}
}