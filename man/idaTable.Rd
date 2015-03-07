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
\name{idaTable}
\alias{idaTable}

\title{In-Database Cross Tabulation and Table Creation}
\description{Function used to build a contingency table of the counts at each combination of factor levels based on the contents of a IDA data frame (\code{\link{ida.data.frame}}).	
}
\usage{
idaTable(idadf,max.entries=1000)

}

\arguments{
	\item{idadf}{A IDA data frame that contains the input data for the function.}
	\item{max.entries}{The maximum number of entries. If the cross product of all columns exceeds this number, an error will  be thrown.}
}

\details{
	\code{idaTable} uses the cross-classifying factors to build a contingency table of the counts at each combination of categorical
	values in all categorical columns of the \code{ida.data.frame} passed as input. 
}

\value{
	The \code{idaTable} function returns a contingency table, an object of class "table".
}

\examples{
\dontrun{

#Create a pointer to the table IRIS
idf<-ida.data.frame('IRIS')

#Add a column
idf$SepalLengthg4 <- ifelse(idf$SepalLength>4,'t','f')

#Calculate the cross-table between Species and SepalLengthg4
idaTable(idf[,c('Species','SepalLengthg4')])

}
}