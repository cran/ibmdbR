%
% Copyright (c) 2010, 2018, IBM Corp. All rights reserved.
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
\name{idaTApply}
\alias{idaTApply}

\title{Apply R-function to subsets of IDA data frame}
\description{This function applies a R function to each subset (group of rows) of
a given IDA data frame (\code{\link{ida.data.frame}}).
}

\usage{
idaTApply(X, INDEX, FUN = NULL, output.name=NULL, output.signature=NULL,
          clear.existing=FALSE, debugger.mode=FALSE,
          num.tasks = 0, working.dir=NULL, apply.function="default", ...)

}

\arguments{
	\item{X}{A IDA data frame that contains the input data for the function.}
	\item{INDEX}{The name or the position of the column of the input IDA data frame X used to partition
	the input data into subsets.}
	\item{FUN}{The R function to be applied to the subsets of the input data.}
	\item{output.name}{The name of the output table where the results are written to.}
	\item{output.signature}{The Db2 data types of the output table.
	  It is a named list with the column names as the names and the data types as the values.
	  Supported data types are CHAR, VARCHAR, SMALLINT, INTEGER, BIGINT, FLOAT, REAL, DOUBLE,
	  DECFLOAT, DECIMAL, NUMERIC, DATE}
	\item{clear.existing}{If TRUE the ouput table is dropped before recreating it.}
	\item{debugger.mode}{If TRUE intermediate results written into the working directory will not be removed.}
	\item{num.tasks}{The number of parallel tasks, i.e. R processes, which execute the R function on the subsets
	of the input data.
	If not specified or if the value is less than 1 it is calculated based on the number of available CPUs.}
	\item{working.dir}{The name of the directory where the directory is created into which intermediate results
	are written to. This directory is removed if debugger.mode is FALSE. The default value for
	working.directory is the value of the
	extbl_location Db2 database configuration variable or, if this variable has not been set, the home directory.}
	\item{apply.function}{The name of the R function to be used for parallelizing the execution of the
	calls of the function FUN.
	Possible values are "default", "spark.lapply" and "mclapply". If the value is "default" "spark-lapply" is used in a
	multi-node and "mclapply" in a single node environment.
	Please note that using the "spark.lapply" function requires Db2 Warehouse with integrated Spark.}
	\item{...}{Additional parameters that can be passed to the function FUN to be called by idaTApply.}
}

\details{
	\code{idaTApply} applies a user-provided R function to each subset (group of rows) of a given ida.data.frame.
	The subsets are determined by a specified index column. The results of applying the function are written into a Db2 table
	 which is referenced by the returned ida.data.frame.

}


\value{
	The \code{idaTApply} function returns a \code{ida.data.frame}     .
}


\examples{
\dontrun{
#Create an ida data frame from the iris data
idf <- as.ida.data.frame(iris)

#Define a function that computes the mean value for every column of a data frame x
#except the index column.
#It returns a data frame with the value of the index column and the mean values.
columnMeans<- function(x, index) {
       cbind(index=x[1,match(index, names(x))],
              as.data.frame(as.list(apply(x[,names(x) != index],2,mean))))}


#Apply the columnMeans function to the subsets of the iris data identified by the Species column
resSig <- list(Species="VARCHAR(12)", MSepalLength="DOUBLE", MSepalWidth="DOUBLE",
                                       MPetalLength="DOUBLE", MPetalWidth="DOUBLE")
resDf <-
  idaTApply(idf, "Species", FUN=columnMeans, output.name="IRIS_MEANS", output.signature=resSig)

#It is possible as well to apply an anonymous function.
#The value "5" of the second parameter designates the position of the "Species" column
#in the idf ida.data.frame.
#The output table of the previous call is recreated because of the "clear.existing=T" parameter.
resDf <- idaTApply(idf, 5,
                   FUN=function(x, index) {
                              cbind(index=x[1,match(index, names(x))],
                                     as.data.frame(as.list(apply(x[,names(x) != index],2,mean))))},
                   output.name="IRIS_MEANS", output.signature=resSig, clear.existing=T)

#Apply the columnMeans2 function which has an additional parameter "columns"
#to specify the columns for which the mean values are computed
columnMeans2 <- function(x, index, columns) {
       cbind(index=x[1,match(index, names(x))],
              as.data.frame(as.list(apply(x[,names(x) != index & names(x) \%in\% columns],2,mean))))}
petalColumns <- c("PetalLength", "PetalWidth")
resSig2 <- list(Species="VARCHAR(12)", MPetalLength="DOUBLE", MPetalWidth="DOUBLE")
resDf2 <- idaTApply(idf, "Species", FUN=columnMeans2, output.name="IRIS_MEANS2",
                    output.signature=resSig2, clear.existing=T, columns=petalColumns)
}
}
