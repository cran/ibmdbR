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
\name{idaRetrieveModel}
\alias{idaRetrieveModel}
\title{Retrieve a predictive model from the database}
\description{
Use this function to retrieve from the database a model that was created using the \code{\link{idaNaiveBayes}} 
or \code{\link{idaKMeans}} function.
}

\usage{
idaRetrieveModel(modelname)
}

\arguments{
  \item{modelname}{The name of the predictive model to be retrieved.}
}

\value{
This function returns an R object that contains a representation of the retrieved model.
The class of the returned object depends on the function that was used to create the 
model.
}

%\seealso{\code{\link{idaTree}}, \code{\link{idaKMeans}} }

\examples{
\dontrun{

#Retrieve the model with name "MYKMEANSMODEL" from the database
trCopy <- idaRetrieveModel("KMEANSMODEL");
}
}
