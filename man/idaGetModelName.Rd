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
\name{idaGetModelName}
\alias{idaGetModelname}
\title{Get the name of a model}
\description{
Use this function to get the name under which a model is stored in-database. This function can be applied to objects returned by
functions like \code{\link{idaNaiveBayes}}, \code{\link{idaKMeans}} or \code{\link{idaArule}}.
}

\usage{
idaGetModelname(object)
}

\arguments{
  \item{object}{The object representing the model.}
}

\value{
	The fully qualified name of the model, as stored in-database. This name is used, e.g. in conjunction with the \code{idaRetrieveModel}
	or with the \code{idaDropModel} function.
}

\examples{\dontrun{

#Get the name of a model stored in variable km	
modelname <- idaGetModelname(km)

}}