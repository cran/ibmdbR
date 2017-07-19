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
\name{idaSample}
\alias{idaSample}
\title{Taking a random sample from a IDA data frame}
\description{
  This function draws a random sample from a IDA data frame (that is, an object of the class \code{\link{ida.data.frame}}).  
}

\usage{
idaSample(bdf, n, stratCol=NULL,stratVals=NULL,stratProbs=NULL,
dbPreSamplePercentage=100,fetchFirst=F);
}

\arguments{
  \item{bdf}{The IDA data frame from which the sample is to be drawn.}
  \item{n}{The number of rows of sample data to be retrieved.}
  \item{stratCol}{For stratified sampling, the column that determines the strata.}
  \item{stratVals}{For stratified sampling, a vector of values that determine the subset of strata from which samples are to be drawn.}
  \item{stratProbs}{For stratified sampling, a vector of explicit sampling probabilities. 
	Each value corresponds to a value of the vector specified for \code{stratVals}.}
  \item{dbPreSamplePercentage}{The percentage of the IDA data frame from which the sample is to be drawn (see details).} 
  \item{fetchFirst}{Fetch first rows instead of using random sample.}
}

\details{
	If \code{stratCol} is specified, a stratified sample based on the contents of the specified column is taken. 
	Unless \code{stratVals} is also specified, each unique value in the column results in one stratum.
	If \code{stratVals} is also specified, only the values it specifies result in strata, and only rows that contain one of those values are included in the  
       sample; other rows are ignored. 

	Unless \code{stratProbs} is also specified, the number of rows retrieved for each stratum is proportional 
	to the size of that stratum relative to the overall sample. 

	To undersample or oversample data, use \code{stratProbs} to specify, for each 
	value of \code{stratVals}, the fraction of the rows of the corresponding stratum that are to be included in the sample. 

	For each stratum, the calculated number of rows is rounded up to the next highest integer. This ensures that there
	is at least one sample for each stratum. Consequently, the number of samples that is returned might
	be higher than the value specified for \code{n}.

	The value of \code{dbPreSamplePercentage} is a numeric value in the range 0-100 that represents the percentage of the 
	IDA data frame that is to serve as the source of the sample data. 
	When working with an especially large IDA data frame, specifying a value smaller than 100 improves performance, because less data must be processed.
	However, the proportionality of the pre-sampled data might vary from that of the complete data, and this would result in
       a biased sample. It can even happen that entire strata are excluded from the final sample.
	
	When \code{fetchFirst} is set to TRUE, the sample values of each stratum are taken in the order in which they are 
	returned from the database rather than randomly. This is usually much faster than random sampling, but can introduce bias.
}

\value{
  An object of class \code{data.frame} that contains the sample.
}

\examples{
\dontrun{
idf<-ida.data.frame('IRIS')

#Simple random sampling
df <- idaSample(idf,10)

#Stratified sample
df <- idaSample(idf,10,'Species')

}
}