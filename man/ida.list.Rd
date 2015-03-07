
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
\name{ida.list}
\alias{ida.list}
\alias{is.ida.list}
\title{Store and retrieve R objects in the database}
\description{
  A user can elect to store R objects in a database table rather than storing them in a workstation file system. 
  This makes it easier for users to share objects, and simplifies backup tasks.
  
  Each user has two R object storage tables:

  \itemize{  
   \item A private table, for objects that other users are not to be able to access
   \item A public table, for objects that other users are to be able to read
  }
  
  Use the \code{ida.list} function to create a pointer to either of your own R object storage tables, 
  or to the public R object storage table of another user. You can then use the pointer to store 
  objects in or retrieve objects from the corresponding table. (If the table belongs to another user 
  you can only retrieve objects from it, not store objects in it.)
  
  Please note that whether public tables might not have effect on databases that do not allow to set
  permissions accordingly, for instance, in multi-tenant environments. To enable the sharing of objects,
  an administrator needs to first create a role names R_USERS_PUBLIC and assign it to all users who
  should be allowed to share objects.
  
}

\usage{
ida.list(type='public',user=NULL) 
}

\arguments{
  \item{type}{The type (private or public) of the table. You can specify 'private' only if \code{user} is NULL  
  or is set explicitly to your own user ID.} 
  \item{user}{The user ID of the owner of the R object storage table. If set to NULL, the user ID is that of the current user.}
}

% \details{}

\value{
  A pointer to an R object storage table.
}

\examples{
\dontrun{
# Create a pointer to the private R object storage table of the current user.
myPrivateObjects <- ida.list(type='private')

# Use the pointer created in the previous example to store a series of numbers in an object with 
# the name 'series100' in the private R object storage table of the current user.
myPrivateObjects['series100'] <- 1:100

# Retrieve the object with the name 'series100' from the 
# private R object storage table of the current user.
x <- myPrivateObjects['series100']

# Delete the object with name 'series100' from the 
# private R object storage table of the current user.

myPrivateObjects['series100'] <- NULL

# List all objects in the private R object storage table of the current user.
names(myPrivateObjects)

# Return the number of objects in the private R object storage table of the current user.
length(myPrivateObjects)

# Create a pointer to the public R object storage table of the current user.
myPublicObjects <- ida.list(type="public")

}
}
