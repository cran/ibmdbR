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

idaSave <- function (idaConn, dfrm, tblName="", rowName="", conType = "odbc") {
  if (conType == "odbc") {
    if(idaIsDb2z()) {
      db2zSave(idaConn, dfrm, tblName=tblName, rowName=rowName)
    } else {
      if (rowName != ""){
        sqlSave(idaConn, dfrm, tablename = tblName, rownames = rowName, addPK=TRUE,fast=ifelse(idaIsOracleMode(),F,T))
      } else {
        sqlSave(idaConn, dfrm, tablename = tblName, rownames = FALSE,fast=ifelse(idaIsOracleMode(),F,T))
      }
    }
  }
}

db2zSave <- function (idaConn, dfrm, tblName="", rowName="", dbname="", asAOT=FALSE) {
  
  columnsDecl <- "("
  if (rowName != "") {
    columnsDecl <- paste( columnsDecl, rowName, " VARCHAR(40), ", sep="")
  }
  colNames <- names(dfrm)
  ncols <- ncol(dfrm) 
  delim<- ","
  quotes <- c()
  for (i in c(1:ncols)) {
    #		type <- typeof(dfrm [1,i])
    #		quoteStr <- ""
    #		if (type == "double") {
    #			typeString =  "DOUBLE"
    #		} else if (type == "integer") {
    #			typeString = "DOUBLE"
    #		} else if (type == "character") {
    #			typeString = paste("VARCHAR(", max(nchar(dfrm[,i])), ")", sep="")
    #			quoteStr <- "'"
    #		} else if (type == "logical") {
    #			typeString = "VARCHAR(6)"
    #			quoteStr <- "'"
    #		} else {
    #			typeString = paste("VARCHAR(", max(nchar(as.character(dfrm[,i]))), ")", sep="")
    #			quoteStr <- "'"
    #		}
    #		if (i == ncols)  {
    #			delim <- ")"
    #		}
    
    cl <- class(dfrm [1,i])
    quoteStr <- ""
    if (cl == "numeric") {
      typeString =  "DOUBLE"
    } else if (cl == "character") {
      typeString = paste("VARCHAR(", max(nchar(dfrm[,i]),  na.rm=TRUE), ")", sep="")
      quoteStr <- "'"
    } else if (cl == "logical") {
      typeString = "VARCHAR(6)"
      quoteStr <- "'"
    } else if (cl == "factor") {
      typeString = paste("VARCHAR(", max(nchar(as.character(dfrm[,i])), na.rm=TRUE), ")", sep="")
      quoteStr <- "'"
    } else {
      typeString = paste("VARCHAR(", max(nchar(as.character(dfrm[,i])),  na.rm=TRUE), ")", sep="")
      quoteStr <- "'"
    }
    if (i == ncols)  {
      delim <- ")"
    }
    
    quotes <- c(quotes, quoteStr)
    
    columnsDecl <- paste(columnsDecl, paste("\"", sub(".", "", colNames[i], fixed=T), "\"", sep=""), typeString, delim, sep = " ")
  }
  
  if (is.null(tblName) || tblName == "" || asAOT) {
    db2TblName <- idaGetValidTableName(prefix = "IDAR_DATA_FRAME_")
  } else {
    db2TblName <- tblName
  } 
  
  inDbClause <- ""
  tryCatch({	
    if(dbname != "") {
      if (!idaExistDatabase(dbname)) {
        idaQuery("CREATE DATABASE ", dbname, " CCSID ", idaGetAcceleratorEncoding())	
      }
      tbsp <- idaGetValidTablespaceName()
      idaQuery("CREATE TABLESPACE ", tbsp, " IN ", dbname, " CCSID ", idaGetAcceleratorEncoding())
      inDbClause <- paste( "IN ", dbname, ".", tbsp, sep = "")
    } 
    db2TblDdlStmt <- paste("CREATE TABLE ", db2TblName, columnsDecl, inDbClause, "CCSID", idaGetAcceleratorEncoding(), sep = " ")
    idaQuery(db2TblDdlStmt)
    
    x <- parseTableName(db2TblName);
    dbName <- idaScalarQuery("select DBNAME from SYSIBM.SYSTABLES where CREATOR ='", x$schema, "' and NAME = '", x$table, "'")
    
    if (rowName != ""){
      sqlSave(idaConn, dfrm, tablename = db2TblName, rownames = rowName, append=TRUE, addPK=FALSE,fast=T)
    } else {
      sqlSave(idaConn, dfrm, tablename = db2TblName, rownames = FALSE, append=TRUE, fast=T)
    }
    accelerateTable(db2TblName)
  }, error = function(e) {
    # in case of error, let user know what happend
    try({if (is.null(tblName) || tblName == "") idaDeleteTable(db2TblName); if(dbname == "") idaDropDatabase(dbName)})
    stop(e)
  })
  
  
  if (asAOT) {
    tryCatch({		
      if (is.null(tblName) || tblName == "") {
        aotTblName <- idaGetValidTableName(prefix = "IDAR_DATA_FRAME_")
      } else {
        aotTblName <- tblName
      }
      aotDdlStmt <- paste("create table ", aotTblName, columnsDecl, "IN ACCELERATOR" , idaGetAccelerator(), "CCSID", idaGetAcceleratorEncoding(), sep = " ")
      idaQuery(aotDdlStmt)
      
      idaQuery("insert into ", aotTblName, " select * from ", db2TblName)
    }, error = function(e) {
      try({if (is.null(tblName) || tblName == "") idaDeleteTable(aotTblName)})
      # in case of error, let user know what happend
      stop(e)
    }, finally = {
      try({idaDeleteTable(db2TblName); if(dbname == "") idaDropDatabase(dbName)})
    })
  }
}
