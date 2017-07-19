% 
% Copyright (c) 2010, 2016, IBM Corp. All rights reserved. 
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
\name{idaGlm} 
\alias{idaGlm}
\alias{print.idaGlm}
\alias{predict.idaGlm}
 
\title{Generalized Linear Models (GLM)}
\description{ This function computes generalized linear models on the contents of an \code{\link{ida.data.frame}}. }
\usage{

idaGlm(	form, data, id = "id", intercept = T, family = "bernoulli", family_param = -1, 
          link = "logit", link_param = 1, maxit = 20, eps = 1e-3, tol = 1e-7, 
          method = "irls", trials = NULL, incolumn = "", interaction = "", 
          modelname = NULL, format = "glm", raw.resid = F, dropAfter = F, ...)
          
\method{print}{idaGlm}(x, ...)
\method{predict}{idaGlm}(object, newdata, id, outtable = NULL, ...)
}

\arguments{
    \item{form}{A \code{formula} object that describes the GLM to build.}
    \item{data}{An \code{nz.data.frame} object that stores the data to be used for GLM building.}
    \item{id}{The ID column name.}
    \item{intercept}{The intercept.}
	\item{family}{The type of error distribution. It can have one of the follwing values: 
	      "bernoulli", "gaussian", "poisson", "binomial", "negativebinomial", "wald", "gamma"}
	\item{family_param}{A family-specific parameter.}
	\item{link}{Type of link function. It can have one of the follwoing values:}
	           "logit", "canbinom", "cangeom", "cannegbinom", "cauchit", "clog", "cloglog", "gaussit", "identity", "inverse", "invnegative", "invsquare", "log", "loglog", "oddspower", "power", "probit", "sqrt"
	\item{link_param}{Link parameter, 1 by default.}
	\item{maxit}{Maximum number of iterations. 20 by default.}
	\item{eps}{Maximum (relative) error used as a stopping criterion. This should be sufficiently small for the algorithm to work.}
	\item{tol}{The tolerance for the linear equation solver to consider a value equal to be zero. This should be sufficiently small for the algorithm to work.}
	\item{method}{Computing algorithm: either \code{"irls"} ("iteratively reweighted least square") or \code{"psgd"} ("parallel stochastic gradient descent").}
	\item{trials}{The input table column containing the number of trials for the binominal distribution. Ignored unless family is 'binomial'.}
	\item{incolumn}{Overwrite automatic creation of incolumn parameter and specify your own incolumn here.}
	\item{interaction}{Overwrite automatic creation of interaction parameter and specify your own incolumn here.}
    \item{modelname}{Name of the model that will be created in the database. Will be created automatically if not specified.}
	\item{format}{Specify output format. Either \code{"glm"} for output looking like \code{stats::glm} or \code{raw} for downloading all results as \code{data.frame}s.}
	\item{raw.resid}{If \code{format} equals \code{"raw"}, whether to download the residuals or return \code{NULL} instead.}
	\item{dropAfter}{Whether to drop the results after downloading them as specified in \code{format}.}
	\item{x}{An \code{idaGlm} object.}
	\item{object}{An \code{idaGlm} object.}
	\item{newdata}{New data used for prediction as \code{ida.data.frame}.}
	\item{outtable}{The name of the table the results will be written in.}
  	\item{...}{Additional parameters.}
    }
	
\value{
	\itemize{
		\item The function \code{idaGlm} returns the generalized linear regression model of classes 
			\code{glm} and \code{idaGlm} if \code{format} equals \code{"glm"} or a \code{list} of
			\code{data.frame}s if \code{format} equals \code{"raw"}.
		\item The functions \code{print} and \code{summary} have no return values.
		\item The function \code{predict} returns an \code{ida.data.frame} that contains the predicted values.
	}
}

\details{
	For more details on the GLM algorithm and requirements to the \code{data}, please refer to
	the documentation of the \code{nza..GLM} stored procedure in the Netezza
	In-Database Analytics Reference Guide or Netezza In-Database Analytics Developers Guide.
}

\examples{
\dontrun{
#Add isSetosa column to iris data frame
iris2 <- iris
iris2$isSetosa <- ifelse(iris2$Species=="setosa", 1, 0)

#Store the iris2 data frame in the IRIS2 table
idf <-as.ida.data.frame(iris2, table="IRIS2", clear.existing=T, rownames="ID")

#Calculate GLM model in-db
glm <- idaGlm(isSetosa~PetalLength+SepalLength*SepalWidth+PetalWidth, idf2, id="ID")

#Print the model
print(glm)

#Apply the model to data
idf2 <- predict(glm, idf, "ID")

#Inspect the results
head(idf2)

}}
