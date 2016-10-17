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
\name{idaListAccelerators, idaSetAccelerator, idaGetAccelerator, idaGetAcceleratorDetails}
\alias{idaListAccelerators}
\alias{idaSetAccelerator}
\alias{idaGetAccelerator}
\alias{idaGetAcceleratorDetails}
% \alias{idaGetAcceleratorQueryAcceleration}

\title{Show and set accelerator settings}
\description{
Use these functions for DB2 for z/OS connections to retrieve the list of available accelerators and 
to set and get the current accelerator settings.   
}

\usage{
idaListAccelerators()
idaSetAccelerator(acceleratorName, queryAcceleration="ENABLE")
idaGetAccelerator()
idaGetAcceleratorDetails()
}

\arguments{
  \item{acceleratorName}{The name of the accelerator where the analytics functions 
  (like idaKMeans or idaTree)  are executed.}
  \item{queryAcceleration}{The value which the DB2 for z/OS register CURRENT QUERY ACCELERATION is set to. Possible values 
  are "NONE", ENABLE", "ENABLE WITH FALLBACK", "ELIGIBLE" and "ALL" }
}

\value{
\code{idaListAccelerators} returns a data frame that contains a list of the accelerators 
available for the current DB2 for z/OS connection.

\code{idaSetAccelerator} sets the accelerator to be used for the subsequent calls of the analytics functions.

\code{idaGetAccelerator} retrieves the name of the accelerator.

\code{idaGetAcceleratorDetails} retrieves the name of accelerator together with the encoding (like UNICODE or EBCDIC) of its data
and the value for query acceleration (like ENABLE or ELIGIBLE) and returns these values in a list object with elements "Accelerator", "Encoding" 
and "QueryAcceleration".

}

\examples{
\dontrun{
#Get a list of all accelerators
q <- idaListAccelerators();

#Set accelerator to "MYACCEL"
idaSetAccelerator("MYACCEL");

#Get name of current accelerator"
idaGetAccelerator();

#Get name of current accelerator together with its encoding and query acceleration"
idaGetAcceleratorDetails();

#Get encoding of current acccelerator
idaGetAcceleratorDetails()$Encoding
}
}
