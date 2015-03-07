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
\name{ida.data.frame, is.ida.data.frame}
\alias{ida.data.frame}
\alias{is.ida.data.frame}
\title{Create a IDA data frame}
\description{
  This function creates a IDA data frame (that is, an object of the class \code{\link{ida.data.frame}}).
  It does not store any data in local memory, but aggregates metadata used to determine the
  exact table subset (columns - \code{SELECT} clause; and/or rows - 
  \code{WHERE} clause) and creates a pointer to a table located in the database.
}

\usage{
ida.data.frame(table)
is.ida.data.frame(x)
}

\arguments{
  \item{table}{Name of a table or view in the current database.}
  \item{x}{An \code{ida.data.frame} object.}
 }
 
\details{   


  The argument table must be a valid table or view name and the table/view must exist.
  
  If schema or table are set in quotes, they will be treated case sensitive otherwise
  they are automatically converted to the default schema of the database. Columns
  are always treated case sensitive. 
   
  A subset of columns and/or rows may be specified
  using the indexing operator \code{[]} (which is translated to the
  \code{SELECT} clause for columns and/or the \code{WHERE} clause for
  rows). Note that columns are treated case sensitive.

  One limitation is that rows cannot be selected using their numbers.
  Instead, you must specify value-based conditions, for example \code{d[d$ID > 10,]}
  which means \dQuote{all rows where the value of the first column is
  greater than 10}. The \code{$} operator may be also used to select
  a \code{ida.data.frame} column.

  You can also add and alter columns in an ida.data.frame.Currently, a limited set of functions and operators
  is supported to define columns based on other columns. The following is supported:

\itemize{
   \item Arithmetic operators are +,-,/,*,^
   \item Mathematical functions are abs,sqrt,log,log10,exp,floor,round,ceiling
   \item Casting functions: as.numeric, as.integer, as.character
   \item Comparison and logical operators: <,<=,>,>=,!=,==,!,&,|
   \item Conditional functions: ifelse
   \item Special functions: is.db.null (checks whether column value is NULL in the table)
}
   There are several rules for adding columns:
\enumerate{
	\item You can not combine columns from different tables or from ida.data.frames that have different WHERE conditions.
	\item You cannot add a column to a ida.data.frame that was defined on columns from another ida.data.frame
	\item You can only add columns that evaluate to non-logical, atomar values
}

The package does basic type checking to enforce these rules, however, it is still possible that the database
will refuse a query that was not properly defined.

  \code{is.ida.data.frame} checks if the given object's class is \code{ida.data.frame}.

}
\value{
  \code{ida.data.frame} returns a IDA data frame.

  \code{is.ida.data.frame} returns a logical value that indicates
  whether the specified object is a IDA data frame.
  
}

\examples{
\dontrun{
idf <- ida.data.frame('IRIS')
is.ida.data.frame(idf)

#Select only certain rows or columns
#The following creates a ida.data.frame that only selects rows with
#Species=='setosa' and the first three columns of the table
idf2 <- idf[idf$Species=='setosa',1:3]

#Define new columns based on existing ones
idf$SepalLengthX <- idf$SepalLength+1
idf$SepalLengthY <- ifelse(idf$SepalLengthX>4.5,idf$ID,10)

#Take a look at the newly defined columns
head(idf)

 
}
}
