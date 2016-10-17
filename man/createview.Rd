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
\name{idaCreateView, idaDropView}
\alias{idaCreateView}
\alias{idaDropView}
\title{Create or drop a view}
\description{
  Use these functions to create or drop a view that is based on a \code{\link{ida.data.frame}}.
}
\usage{
idaCreateView(x, newColumn = NULL) 
idaDropView(v) 
}
\arguments{
  \item{x}{\code{ida.data.frame} for which a view is to be created.}
  \item{newColumn}{The expression specifying the column to be added.}
  \item{v}{Name of the view to be dropped.}
}
\details{
  The \code{idaCreateView} function creates a view from the specified IDA data frame.
  The \code{idaDropView} function drops the specified view.
}

\value{
  The \code{idaCreateView} function returns the view name.
  The \code{idaDropView} function does not return a value.
}

\examples{
\dontrun{
idf <- ida.data.frame('IRIS')

#Create a view based on the IDA data frame
vname <- idaCreateView(idf)

#Drop the view
idaDropView(vname)
}
}