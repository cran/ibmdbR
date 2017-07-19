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
\name{idaConnect, idaClose}
\alias{idaClose}
\alias{idaConnect}

\title{
Open or closes a IDA database connection}
\description{
These functions are used to open or close an existing IDA database connection.}
\usage{
idaConnect(dsn, uid = "", pwd = "", conType = "odbc", 
  dsnLookup = c("auto", "default", "store"), ...)

idaClose(idaConn, conType = "odbc")
}
\arguments{
  \item{dsn}{The DSN of the data base.}
  \item{uid}{The user name.}
  \item{pwd}{The password.}
  \item{conType}{The connection type.}  
  \item{dsnLookup}{This parameter only is used when ibmdbR is loaded in an RStudio instance of IBM Data Science Experience. Per default ("auto") ibmdbR automatically detects if the provided dsn value is an usual DSN string or the name of a connection in the local connection store. The lookup method can also be manually set to "default", if the dsn parameter should be treated as an usual DSN string. If set to "store" the connection store of RStudio on DSX will be used.}
  \item{...}{Additional arguments for DSN lookup.}
  \item{idaConn}{The connection object.}
}

\details{
Opens or closes a connection to a database. Currently, RODBC is used as underlying library, this might
change, however, in the future. 
}

\examples{
\dontrun{
#Connect locally 
con <- idaConnect('BLUDB','','')

#Close the connection
idaClose(con)
}
}