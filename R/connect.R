# 
# Copyright (c) 2013, 2014, IBM Corp. All rights reserved. 
# 		
# This program is free software: you can redistribute it and/or modify 
# it under the terms of the GNU General Public License as published by 
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version. 
#
# This program is distributed in the hope that it will be useful, 
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU General Public License for more details. 
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see <http://www.gnu.org/licenses/>. 
#

idaConnect <- function (dsn,
                        uid = "",
                        pwd = "",
                        conType = "odbc",
                        dsnLookup = c("auto", "default", "store"),
                        ...) {
  dsnLookup <- match.arg(dsnLookup)
  if (conType == "odbc")
  {
    if (pwd == "")
    {
      # call Db2 connection info mechanism if present
      dashWrapper <- "/opt/ibm/Rsupport/dshttpwrapper.R"
      if (file.exists(dashWrapper))
      {
        getCurrentBLUUserCredential <- function() {
          list(user = "", pass = "")
        }
        source(dashWrapper, local = TRUE)
        cred <- getCurrentBLUUserCredential()
        uid <- cred$user
        pwd <- cred$pass
      } else {
        # call dsx connection info mechanism if present
        dsxWrapper <- "/opt/ibm/dashdb/dsxwrapper.R"
        # if dsx script is available and type is store or dsn is not in dsn format
        if ((dsnLookup == "store" || (dsnLookup == "auto" && !isDSNString(dsn))) && file.exists(dsxWrapper)) {
          getDsnInfo <- function (dsn, ...) {
            NULL
          }
          source(dsxWrapper, local = TRUE)
          if (!nzchar(dsn)) {
            warning(paste0("DSN string for entry ", dsn, " in local connection store is empty."))
          }
          dsn <- getDsnInfo(dsn)
        }
      }
    }
    idaCon <- odbcConnect(dsn, uid, pwd, believeNRows = FALSE)
    invisible(idaCon)
    return(idaCon)
  }
}

#' isDSNString
#' 
#' Checks if string is a DSN connection string
#'
#' @param dsnString 
#'
#' @return true, if dsnString is DSN connection String
isDSNString <- function(dsnString) {
  splittedList <- lapply(strsplit(dsnString, ";"), function(x) { strsplit(x, "=")})
  sapply(splittedList, function(x) {
    equalSignOccurences <- sapply(x, length) - 1
    equalSignOccurences.length <- length(equalSignOccurences)
    if (equalSignOccurences.length > 1) {
      all(equalSignOccurences[2:equalSignOccurences.length] > 0)
    } else {
      equalSignOccurences[1] > 0
    }  
  })
}
