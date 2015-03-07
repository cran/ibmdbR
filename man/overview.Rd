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
\name{ibmdbR-package}
\alias{ibmdbR-package}
\alias{ibmdbR}
\docType{package}
\title{
IBM In-Database Analytics
}
\description{
In-database analytics functions operate directly on data in a database, 
rather than requiring that the data first be extracted to working memory.
This lets you analyze large amounts of data that would be impractical 
or impossible to extract. It also avoids security issues associated 
with extracting data, and ensures that the data being analyzed 
is as current as possible. Some functions additionally use lazy loading 
to load only those parts of the data that are actually required, 
to further increase efficiency.

This package also contains a data structure called a \code{\link{ida.list}}, which you 
can use to store R objects in the database. This simplifies the sharing 
of R objects among users. Each user is assigned two tables for R object 
storage: a private table, to which only that user has access, and a 
public table, which can be read by other users. Use a IDA list to generate 
a pointer to either of these tables, and use the pointer to list, store, 
or retrieve R objects.} 
