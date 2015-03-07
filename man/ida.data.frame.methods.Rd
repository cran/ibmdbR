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

\name{ida.data.frame methods}
\docType{methods}
\alias{sd,ida.data.frame-method}
\alias{max,ida.data.frame-method}
\alias{mean,ida.data.frame-method}
\alias{min,ida.data.frame-method}
\alias{length,ida.data.frame-method}
\alias{print,ida.data.frame-method}
\alias{names,ida.data.frame-method}
\alias{colnames,ida.data.frame-method}
\alias{summary,ida.data.frame-method}
\alias{NROW,ida.data.frame-method}
\alias{NCOL,ida.data.frame-method}
\alias{dim,ida.data.frame-method}
\alias{var,ida.data.frame-method}
\alias{head,ida.data.frame-method}
\alias{hist,ida.data.frame-method}
\alias{$,ida.data.frame-method}
\alias{$<-,ida.data.frame-method}
\alias{[,ida.data.frame-method}
\alias{as.data.frame,ida.data.frame-method}
\alias{cor,ida.data.frame-method}
\alias{cov,ANY,ANY-method}
\alias{cov,ANY,ida.data.frame-method}
\alias{cov,ida.data.frame,ANY-method}
\alias{cov,ida.data.frame,ida.data.frame-method}
\alias{[,ida.data.frame,ANY,ANY,ANY-method}

\title{Available methods for ida.data.frame}
\description{
\code{ida.data.frame} objects provide many methods that will behave exactly like or very
similar to methods defined on a regular \code{data.frame}. The following is a list of
currently supported methods: as.data.frame, sd, max, mean, min, length, print, names, colnames,
summary, NROW, NCOL, dim, var, head, hist, cor, cov. Furthermore, the $ and [ operators allow you
to select columns and rows and the $<- operator will allow you to add columns. For details see
the documentation of \code{\link{ida.data.frame}}.

}
