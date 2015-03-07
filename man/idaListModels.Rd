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
\name{idaListModels and idaModelExists}
\alias{idaListModels}
\alias{idaModelExists}
\title{List all predictive models in the database}
\description{
Use these function to list all models in the schema of the current user that were created using the \code{\link{idaNaiveBayes}} 
or \code{\link{idaKMeans}} functions or check whether a model with a specific name exists.
}

\usage{
idaListModels()
idaModelExists(modelname)
}

\arguments{
  \item{modelname}{The name of a predictive model.}
}

\value{
\code{idaListModels} returns a data frame that contains a list of the predictive models that are stored
in the current schema and information about each one.

\code{idaModelExists} returns a boolean value depending on whether the model exists or not.

}

%\seealso{\code{\link{idaTree}}, \code{\link{idaKMeans}} }

\examples{
\dontrun{
#Get a list of all models
q <- idaListModels();
}
}
