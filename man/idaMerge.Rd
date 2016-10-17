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
\name{idaMerge}
\alias{idaMerge}
\title{Merge IDA data frames}
\description{
  This function merges two IDA data frames(that is, two objects of the class \code{\link{ida.data.frame}}).
}
\usage{
idaMerge(x, y, by=intersect(x@cols, y@cols), by.x=by, by.y=by, 
    all=FALSE, all.x=all, all.y=all, sort=TRUE,   
    suffixes=c("_x", "_y"), table=NULL)
}
\arguments{
  \item{x}{The first \code{ida.data.frame} object to be merged.}
  \item{y}{The second \code{ida.data.frame} object to be merged.}
  
  \item{by}{Specification of the common columns; see the
    \emph{Details} section.}
  \item{by.x}{Specification of the common columns; see the
    \emph{Details} section.}
  \item{by.y}{Specification of the common columns; see the
    \emph{Details} section.}
      
  \item{all}{Whether non-matching columns of \code{x} and \code{y} are to be appended to the result. 
  If set to FALSE, only columns common to both \code{x} and \code{y} are included in the output.
  This parameter overrides the \code{all.x} and \code{all.y} parameters.
  In SQL database terminology, specifying \code{all=FALSE} results in 
  an inner join that is equivalent to a natural join, and specifying 
  \code{all=TRUE} results in a full outer join. 
  In a full outer join, the columns that 
  are common to both \code{x} and \code{y} are followed by the remaining 
  columns in \code{x}, which are followed by the remaining columns in \code{y}.
}
  \item{all.x}{If columns from only one of the IDA data frames being merged are to be included in the output, 
  set its corresponding parameter to TRUE and the other parameter to FALSE.
  In SQL database terminology, specifying \code{all.x=TRUE} and \code{all.y=FALSE} results in a left outer join, 
  and specifying \code{all.x=FALSE} and \code{all.y=TRUE} results in a right outer join. 

  If \code{TRUE}, then extra rows are added to the
    output, one for each row in \code{x} that has no matching row in
	\code{y}. These rows have a value of \code{NA} in those columns
	that are typically filled with values from \code{y}. The default is
	\code{FALSE}, so that only rows with data from both \code{x} and
	\code{y} are included in the output.
	
	If \code{all.x} is true, all the non matching cases of \code{x} are
also appended to the result, with a value of \code{NA} filled in the
corresponding columns of \code{y}}
	
  \item{all.y}{Analogous to \code{all.x}}	
  \item{sort}{This parameter is ignored. The output is never sorted regardless of the setting of this parameter.}
  \item{suffixes}{
    Two 2-character strings, each of which specifies a suffix that is
    used when generating column names.
	By specifying different suffixes, you can ensure that each column 
	can be uniquely attributed to either \code{x} or \code{y}. 
	Note that a dot (.) is not a valid character for a column name.}
  \item{table}{Name of the output IDA data frame. }

}
\details{

This function merges two IDA data frames on the columns that they have in common.
The rows in the two data frames that match on the specified columns
are extracted, and joined together. If there is more than one match,
all possible matches contribute one row each. For the precise meaning
of \sQuote{match}.

If \code{by} or both \code{by.x} and \code{by.y} are of length 0 (a
length zero vector or \code{NULL}), the result, \code{r}, is the
Cartesian product of \code{x} and \code{y}, that is, a \emph{cross join}. 

If non-merged columns of the data frames have identical names and are to be included in the output, 
suffixes are appended to the names of the corresponding columns in the output to make their names unique. 


Note that this function creates, in the current database, a view that corresponds to the output object. 
Within the current session, this view can be accessed using the same IDA data frame object. 
However, it is persistent and, after it is no longer needed, it must be dropped manually. 

}
\value{
  A \code{ida.data.frame} object.   
}

\seealso{\code{\link{ida.data.frame}}
}
\examples{
\dontrun{

idf <- ida.data.frame('IRIS')

#Perform a self-join
idf2 <- idaMerge(idf,idf,by="ID")

}

}
