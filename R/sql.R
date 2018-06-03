# 
# Copyright (c) 2010, 2014, 2016, 2018 IBM Corp. All rights reserved.
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
# 

utils::globalVariables(c("idaRGlobal"))

################ show tables ############################
idaShowTables <- function(showAll=FALSE, matchStr=NULL, schema=NULL, accelerated=FALSE) {
  idaCheckConnection();
  
  if (!is.null(matchStr) && !is.character(matchStr))
    stop("This function can only be applied to character values.")
  
  if(idaIsDb2z()) {
  
    wherePart <- "WHERE (not s.CREATOR like 'SYS%') ";
    if(!showAll) {
      currSchema <- idaGetCurrentSchema();
      wherePart <- paste("WHERE (s.CREATOR = '",currSchema,"') ",sep='');
    } else if (!is.null(schema))  {
      wherePart <- paste("WHERE (s.CREATOR = '",schema,"') ",sep='');
    }
  
    if (accelerated) {
      accelerator <- idaGetAccelerator();
      if (accelerator == "") {
        stop('For the "accelerated=TRUE"" option the accelerator has to be set with the idaSetAccelerator function.')
      } else {
        wherePart <- paste( wherePart, "AND (a.ACCELERATORNAME = '", accelerator, "') ", sep="")
      }
    }
    if (!is.null(matchStr))
      wherePart <- paste(wherePart," AND s.NAME LIKE '%", paste(matchStr,collapse='%',sep=''), "%'", sep='')
    
    query <- 	paste("SELECT distinct s.CREATOR as \"Schema\", s.NAME as \"Name\", s.OWNER as \"Owner\", s.TYPE as \"Type\", ",
                    "coalesce(a.ACCELERATORNAME, '') as \"Acceleratorname\", coalesce(a.ENABLE, ' ') as \"Enable\" ",
                    "from SYSIBM.SYSTABLES s left outer join SYSACCEL.SYSACCELERATEDTABLES a ",
                    "on s.CREATOR = a.CREATOR and s.NAME=a.NAME ",
                    wherePart,
                    "ORDER BY \"Schema\",\"Name\"")
    # idaQuery('SELECT distinct CREATOR as "Schema", NAME as "Name", OWNER as "Owner", TYPE as "Type" from SYSIBM.SYSTABLES ', wherePart ,'ORDER BY "Schema","Name"')
    idaQuery(query)
  } else {
    wherePart <- "WHERE (OWNERTYPE = 'U')";
    if(!showAll) {
      currSchema <- idaGetCurrentSchema();
      wherePart <- paste(wherePart," AND (TABSCHEMA= '",currSchema,"') ",sep='');
    }
  
    if (!is.null(matchStr))
      wherePart <- paste(wherePart," AND TABNAME LIKE '%", paste(matchStr,collapse='%',sep=''), "%'", sep='')
  
    idaQuery('SELECT distinct TABSCHEMA as "Schema", TABNAME as "Name", OWNER as "Owner", TYPE as "Type" from SYSCAT.TABLES ', wherePart ,'ORDER BY "Schema","Name"')

  }
}


################ Internal schema and catalog utilities ############################
idaGetCurrentSchema <- function() {
  return(idaScalarQuery("select trim(current_schema) from sysibm.sysdummy1"));
}

prepareCatalogLookup <- function(tableName) {
  x <- parseTableName(tableName);
  if(idaIsDb2z()) {
    return(paste("NAME='",x$table,"'",ifelse(!is.null(x$schema),paste(" AND CREATOR = '",x$schema,"'",sep=''),''),sep=''));
  } else {
    return(paste("TABNAME='",x$table,"'",ifelse(!is.null(x$schema),paste(" AND TABSCHEMA = '",x$schema,"'",sep=''),''),sep=''));
  }
}

parseTableName <- function(tableName,removeQuotes=T,disallowQuotes=F) {
  
  inTabNameQuotes <- F;
  sepTableHits <- c();
  
  for(i in 1:nchar(tableName)) {
    
    c <- substr(tableName,i,i);
    if(c=="\"")
      inTabNameQuotes <- !inTabNameQuotes;
    
    if((c==".")&&(!inTabNameQuotes))
      sepTableHits <- c(sepTableHits,i);	
  }
  
  if(inTabNameQuotes) {
    stop("Table name not formatted correctly");
  }
  
  #Only the table name, if DB is schema enabled, we need to 
  # read the current schema, as this could change after the
  # nz.data.frame was created
  if(length(sepTableHits)==0) {
    table <- tableName;
    schema <- idaGetCurrentSchema();
    #Table and schema name provided, if db is not schema enabled, ignore schema	
  } else if(length(sepTableHits)==1) {
    schema <- substr(tableName, 1, sepTableHits[1]-1);		
    table <- substr(tableName, sepTableHits[1]+1, nchar(tableName));
  } else {
    stop("Table name ", table, " is not well-formed");
  }
  
  #Remove quotes or set case
  if (nchar(table) > 2 && substr(table, 1, 1) == '"' && substr(table, nchar(table), nchar(table)) == '"') {
    
    if(disallowQuotes)
      stop("Quoted schema and table names are currently not supported for this function.")
    
    if(removeQuotes)
      table <- substr(table,2,nchar(table)-1)
  } else {
    table <- toupper(table);
  }
  
  #Remove quotes or set case
  if(!is.null(schema)) {
    if (nchar(schema) > 2 && substr(schema, 1, 1) == '"' && substr(schema, nchar(schema), nchar(schema)) == '"') {
      if(disallowQuotes)
        stop("Quoted schema and table names are currently not supported for this function.")
      
      if(removeQuotes)	
        schema <- substr(schema,2,nchar(schema)-1)
    } else { 
      schema <- toupper(schema);
    }
  }
  
  return(list(table=table,schema=schema));
}


################ Basic check for databases (DB2 for z/OS only) and tablespaces ############################

idaExistDatabase <- function (databaseName) {
  if(idaIsDb2z()) {
    idaCheckConnection();
    databaseExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSIBM.SYSDATABASE WHERE NAME = '", databaseName, "'"))>0);
    return(databaseExists);
  } else {
    stop("The idaExistDatabase and idaDropDatabase functions are defined for connections to DB2 for z/OS subsystems only.");
  }
}

idaExistTablespace <- function (tbspName) {
  idaCheckConnection();
  if(idaIsDb2z()) {
    tableExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSIBM.SYSTABLESPACE WHERE NAME = '", tbspName, "'"))>0);
  } else {
    tableExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSCAT.TABLESPACES WHERE TBSPACE = '", tbspName, "'"))>0);
  }
  return(tableExists);
}

idaGetValidTablespaceName <- function () {
  idaCheckConnection();
  prefix <-toupper("TSP"); 
  while (TRUE) {
    # for DB2 for z/OS the names tablespaces consist of 8 or less characters
    name = paste(prefix, floor(runif(1,0,99999)), sep="")
    if (!idaExistTablespace(name))
      return(name)
  }

  return(name)
}


################ Basic operations on tables ############################
idaExistTable <- function (tableName) {
  idaCheckConnection();
  cmp <- prepareCatalogLookup(tableName)
  if(idaIsDb2z()) {
    tableExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSIBM.SYSTABLES WHERE ", cmp))>0);
  } else {
    tableExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSCAT.TABLES WHERE ", cmp))>0);
  }
  return(tableExists);
}


idaIsRegularTable <- function (tableName) {
  idaCheckConnection();
  cmp <- paste(prepareCatalogLookup(tableName), " AND TYPE = 'T'",sep='');
  if(idaIsDb2z()) {
    tableExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSIBM.SYSTABLES WHERE ", cmp))>0);
  } else {
    tableExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSCAT.TABLES WHERE ", cmp))>0);
  }
  return(tableExists);
}

idaIsView <- function (tableName) {
  idaCheckConnection();
  cmp <- paste(prepareCatalogLookup(tableName), " AND TYPE = 'V'",sep=''); 
  if(idaIsDb2z()) {
    viewExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSIBM.SYSTABLES WHERE ", cmp))>0);
  } else {
    viewExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSCAT.TABLES WHERE ", cmp))>0);
  }
  return(viewExists);
}

idaListTableColumns <- function (tableName) {
  idaCheckConnection();
  if(idaIsDb2z()) {
    x <- parseTableName(tableName);
    cmp <- paste("TBNAME='",x$table,"'",ifelse(!is.null(x$schema),paste(" AND TBCREATOR = '",x$schema,"'",sep=''),''),sep='')
    return(as.vector(idaQuery("SELECT NAME FROM SYSIBM.SYSCOLUMNS WHERE ",cmp," ORDER BY COLNO ASC")[[1]]))
  } else {
    cmp <- prepareCatalogLookup(tableName)
    return(as.vector(idaQuery("SELECT COLNAME FROM SYSCAT.COLUMNS WHERE ",cmp," ORDER BY COLNO ASC")[[1]]))
  }
}


idaExistColumnInTable <- function (columnName, tableName) {
  idaCheckConnection();
  if(idaIsDb2z()) {
    x <- parseTableName(tableName);
    cmp <- paste("TBNAME='",x$table,"'",ifelse(!is.null(x$schema),paste(" AND TBCREATOR = '",x$schema,"' AND NAME = '", columnName, "'", sep=''),''),sep="")
    columnExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSIBM.SYSCOLUMNS WHERE ",cmp ))>0)
  } else {
    cmp <- paste( prepareCatalogLookup(tableName), " AND COLNAME = '", columnName, "'", sep="")
    columnExists <- as.logical(as.integer(idaScalarQuery("SELECT CAST(COUNT(*) AS INTEGER) FROM SYSCAT.COLUMNS WHERE ",cmp ))>0)
  }
  return(columnExists)
}




idaDeleteViewOrTable <- function(tableName) {
  if(idaIsView(tableName)) {
    idaDropView(tableName)
  } else {
    idaDeleteTable(tableName);
  }
}

idaDeleteTable <- function(table) {
  if(inherits(table,"ida.data.frame")) {
    idaDeleteViewOrTable(table@table)
  } else {
    if (idaExistTable(table)) {
      if (idaIsDb2z() && isAccelerated(table)) {
        accelerateTable(table, undo=TRUE)
      }
      idaQuery("DROP TABLE ", table)
    }
    invisible()
  }
}

idaDropView <- function(v) {
  idaCheckConnection();
  try({idaQuery("DROP VIEW ", v)});
}

idaDropDatabase <- function(dbname) {
  if (idaExistDatabase(dbname) && idaScalarQuery("select count(*) from SYSIBM.SYSTABLES where DBNAME = '", dbname, "'") == 0) {
    idaQuery("DROP DATABASE ", dbname)
  }
}

idaDeleteTempTables <- function(pattern='DATA_FRAME_[0-9]+') {
  idaCheckConnection();
  tempTables <- idaShowTables()
  tempTables <- tempTables[grep(pattern, tempTables$Name),]
  if (nrow(tempTables)==0) {
    cat("No temporary tables or views are found to be deleted.")
    return(invisible(NULL))
  }
  row.names(tempTables) <- 1:nrow(tempTables)
  cat("The temporary tables and views to be deleted are following:\n")
  print(tempTables)
  cat("\nHow do you like to delete the temporary tables and views?\n")
  cat("1) Delete all temporary tables and views without further confirmation.\n")
  cat("2) Ask conformation for deleting each temporary table or view.\n")
  cat("3) Cancel deletion and return.\n\n")
  while(TRUE){
    cat("Type a key to select your choice:\n")
    cat(" 1) 'a'       2) 'b'       3) 'c'\n")
    choice <- readline()
    if (choice=='a' || choice=='b' || choice=='c' )
      break
  }
  if (choice=='a') {
    for (i in 1:nrow(tempTables)) {
      tempTab = tempTables[i,]
      idaDeleteViewOrTable(paste('"', sub('\\s+$', '', tempTab$Schema), '"."', tempTab$Name, '"', sep=''))
    } 
  } else if (choice=='b') {
    for (i in 1:nrow(tempTables)) {
      tempTab <- tempTables[i,]
      schema <- sub('\\s+$', '', tempTab$Schema)
      cat("Delete ", ifelse(tempTab$Type=='T', 'table', 'view'), ": ", 
          schema, '.', tempTab$Name, "? yes[y], no[n] or exit[e] :", sep='')
      choice <- readline()
      if (choice=='e') {
        break
      } else if (choice=='y') {
        idaDeleteViewOrTable(paste('"', schema, '"."', tempTab$Name, '"', sep=''))
      }
    }
  }
}


idaRenameTable <- function(from=NULL, to=NULL, drop.to=T) {

  if (is.null(from) || is.null(to)) {
    # there is nothing to rename
    stop("idaRenameTable: the from and to parameters have to be both non-null values.")
  }
  xTo <- parseTableName(to)
  tryCatch({

      odbcSetAutoCommit(get("p_idaConnection",envir=idaRGlobal), autoCommit = FALSE)
      if(isTRUE(drop.to)) {
        idaDeleteTable(to)
      }
      idaQuery("RENAME TABLE ", from , " TO ", xTo$table)
      odbcEndTran(get("p_idaConnection",envir=idaRGlobal), commit = TRUE);
    }, error=function(e){
      print(e);
      odbcEndTran(get("p_idaConnection",envir=idaRGlobal), commit = FALSE);
      stop(paste("idaRenameTable(from=", from, ", to=", to, " ,drop.to=", drop.to, ") failed", sep=""))
    }, finally={
      odbcSetAutoCommit(get("p_idaConnection",envir=idaRGlobal), autoCommit = TRUE)
    }
  )
}



idaQuery <- function (..., as.is = TRUE, na.strings = "NA")  {
  
  idaCheckConnection();
  
  # for a DB2/z connection id the connection has been lost
  if (idaIsDb2z() & get("p_connectString", envir=idaRGlobal) != "") {
    conWorking=FALSE
    try({
      conWorking <-odbcQuery(get("p_idaConnection", envir=idaRGlobal), "select count(*) from sysibm.sysdummy1")>0
      }, silent=TRUE
    )
    if (!conWorking) {
      reconnect()
    }
  }
  
  query <- paste(..., sep = "", collapse = "")
  
  if(exists("p_debug",envir=idaRGlobal)&&(get("p_debug",envir=idaRGlobal))) { 
    print(query)
  }
  
  result <- sqlQuery(get("p_idaConnection", envir=idaRGlobal), query, believeNRows = FALSE, 
                      stringsAsFactors = FALSE, as.is = as.is, na.strings = na.strings)
  
  #we got an error message
  if (is.character(result) && length(result) > 0) {
    stop(paste(result, collapse = "\n"))
  }
  
  return(result)
}

idaScalarQuery <- function (..., as.is=TRUE) {
  return(idaQuery(..., as.is=as.is)[[1]][1])
}

idaGetValidTableName <- function (prefix = "DATA_FRAME_") {
  idaCheckConnection();
  prefix <-toupper(prefix); 
  while (TRUE) {
    name = paste(prefix, floor(runif(1,0,100000)), sep="")
    if (!idaExistTable(name))
      return(name)
  }
}

callSP <- function (spname, retvalcolumn=NULL, ...) {
  idaCheckConnection()
  
  args = list(...)
  views <- c()
  tmp <- c()
  for (name in names(args)) {
    value <- args[[name]]
    if (is.null(value))   next
    if (is.ida.data.frame(value)) {
      view <- idaCreateView(value)
      tmp[length(tmp) + 1] = paste(name, "=", view, sep = "")
      views <- append(views, view)
    } else if (is.character(value) && length(grep(" ", value))) {
      tmp[length(tmp) + 1] = paste(name, "=\"", value, "\"", sep = "")
    } else if (length(value) > 1) {
      tmp[length(tmp) + 1] = paste(name, "=\"", paste(value, collapse = " "), "\"", sep = "")
    } else
      tmp[length(tmp) + 1] = paste(name, "=", value, sep = "")
  }
  
  if(idaIsDb2z()) {
    acceleratorName = idaGetAccelerator();
    try({
        idaQuery("DELETE FROM SESSION.INZA_MSG")
      }
     );
    retvalcolexpr <- "";
    if (!is.null(retvalcolumn) && retvalcolumn != "")  {
      retvalcolexpr <- paste(", ", retvalcolumn, sep="")
    }
    res <- try(
        idaQuery("CALL INZAR.", spname, "('", acceleratorName, "', '", paste(tmp, collapse = ","), "', 'SESSION.INZA_MSG(MESSAGE ",
                  retvalcolexpr, ")')"),
        silent=T);
  } else {
    res <- try(idaQuery("CALL IDAX.", spname, "('", paste(tmp, collapse = ","), "')"), silent=T)
  }
  for (view in views) idaDropView(view)
  
  if(idaIsDb2z()) {
    message <- sqlQuery(get("p_idaConnection",envir=idaRGlobal), "select MESSAGE from SESSION.INZA_MSG")$MESSAGE
    if (regexpr("The operation was completed successfully.", message, fixed=TRUE) > 0)  {
      if (!is.null(retvalcolumn) && retvalcolumn != "")  {
        res <- try(idaQuery("SELECT ", retvalcolumn, " FROM SESSION.INZA_MSG"))[1,1]
      } else if (!is.list(res) ) { # no result set has been returned
        res  <- matrix(c(0),1,1)
      }
    } else {
      startpos = regexpr("<message", message, fixed=TRUE) +9
      endpos = regexpr("</message>", message, fixed=TRUE) -1
      fullError <- gsub("&apos;", "'", gsub("&quot;", '"', substr(message, startpos, endpos)))
      #  fullError <- sprintf("The call of the stored procedure %s failed.", spname)
      stop(fullError)
    }
  } else if(inherits(res, "try-error")) {
    fullError <- idaScalarQuery("values idax.last_message")
    if(nchar(fullError)==0)
      fullError <- res;
    stop(fullError)
  }
  return(invisible(res))
}

# available for DB2 z/OS only
accelerateTable <-function(tableName, undo=FALSE) {
  if(idaIsDb2z()) {
    procName = "INZAR.ACCELERATE_TABLE"
    if (undo) {
      procName = "INZAR.ACCEL_REMOVE_TABLE"
    }
    acceleratorName = idaGetAccelerator();
    x <- parseTableName(tableName);
    try({idaQuery("DELETE FROM SESSION.INZA_MSG")})
    idaQuery(paste("call ", procName, "('", acceleratorName, "', '", x$schema, "', '", x$table, "', 'SESSION.INZA_MSG(MESSAGE)')", sep=""))
    message <- sqlQuery(get("p_idaConnection",envir=idaRGlobal), "select MESSAGE from SESSION.INZA_MSG")$MESSAGE
    if (regexpr("The operation was completed successfully.", message, fixed=TRUE) > 0)  {
      return( matrix(c(0),1,1) )
    } else {
      startpos <- regexpr("<message", message, fixed=TRUE) +9
      endpos <- regexpr("</message>", message, fixed=TRUE) -1
      fullError <- gsub("&apos;", "'", gsub("&quot;", '"', substr(message, startpos, endpos)))
      stop(fullError)
    }
  }
}	


isAccelerated <- function(tableName) {
  if(idaIsDb2z()) {
    x <- parseTableName(tableName)
    query <- paste("select count(*)from SYSIBM.SYSTABLES s left outer join SYSACCEL.SYSACCELERATEDTABLES a ",
            "on s.CREATOR = a.CREATOR and s.NAME=a.NAME ",
            "where s.CREATOR = '", x$schema, "'and s.NAME = '", x$table, "' and s.TYPE ='T'", sep="")
    return(idaScalarQuery(query) > 0)
  } else {
    return(FALSE)
  }
}	





################ Table def ############################
idaTableDef <- function(bdf,  collapse=TRUE) {
  if (!is.ida.data.frame(bdf))
    stop("this method can be applied only on ida.data.frame")
  
  viewName <- NULL;	
  if(length(idadf.defined.columns(bdf)>0)) {
    viewName <- idaCreateView(bdf);
    bdf <- ida.data.frame(viewName);
  }	
  if(idaIsDb2z()) {
    x <- parseTableName(bdf@table);
    cmp <- paste("TBNAME='",x$table,"'",ifelse(!is.null(x$schema),paste(" AND TBCREATOR = '",x$schema,"'",sep=''),''),sep='')
    attrs <- idaQuery("SELECT NAME AS COLNAME,trim(COLTYPE) AS TYPENAME FROM SYSIBM.SYSCOLUMNS WHERE ",cmp," ORDER BY COLNO ASC")
  } else {
    attrs <- idaQuery("SELECT COLNAME, TYPENAME FROM SYSCAT.COLUMNS WHERE ",prepareCatalogLookup(bdf@table), " ORDER BY COLNO")
  }
  
  if(!is.null(viewName)) {
    idaDropView(viewName);
  }
  
  idx <- tolower(attrs[,1]) %in% tolower(bdf@cols)
  
  if (as.logical(collapse)) {
    return(paste(attrs[idx,1], attrs[idx,2], collapse=","))
  } else {
    res <- data.frame(name=attrs[idx,1], type=attrs[idx,2])
    res$valType <- ifelse(res$type %in% c('VARCHAR','CHARACTER','VARGRAPHIC','GRAPHIC','CLOB'),'CATEGORICAL',ifelse(res$type %in% c('SMALLINT', 'INTEGER','BIGINT','REAL','DOUBLE','FLOAT','DECIMAL','NUMERIC'),'NUMERIC','NONE'))
    return(res)
  }
}



idaMaterialize <- function(idf,tableName, asAOT=TRUE, dbname="") {
  
  v <- idaCreateView(idf)
  
  if(!is.null(tableName)&&idaExistTable(tableName)) {
    stop("Table already exists, choose a different name.")	
  }	
  tryCatch({
        
      odbcSetAutoCommit(get("p_idaConnection",envir=idaRGlobal), autoCommit = FALSE)
      if(idaIsDb2z()) {
        createDB2zTableAs(tableName, v, dbname=dbname, asAOT=asAOT)
        odbcEndTran(get("p_idaConnection",envir=idaRGlobal), commit = TRUE);
      } else {
        idaQuery("CREATE TABLE ", tableName, " LIKE ", v," ORGANIZE BY ROW NOT LOGGED INITIALLY");
      }
        
      idaQuery("INSERT INTO ", tableName, " SELECT * FROM ", v);
      if(idaIsDb2z() && !asAOT) {
        accelerateTable(tableName)
      }

      odbcEndTran(get("p_idaConnection",envir=idaRGlobal), commit = TRUE);
        
    }, error=function(e){
        print(e);
        odbcEndTran(get("p_idaConnection",envir=idaRGlobal), commit = FALSE);
        if(idaIsDb2z()) idaDeleteTable(tableName)
    }, finally={
        odbcSetAutoCommit(get("p_idaConnection",envir=idaRGlobal), autoCommit = TRUE);idaDropView(v)
    }
  );
}

idaAppend <- function(df, table) {
  sqlSave(get("p_idaConnection",envir=idaRGlobal), dat=df, tablename = table, rownames=F,append=T);
}

##############################  Utilities ############################

removeQuotes <- function(x) {
  if (!is.character(x) || length(x)>1)
    stop("This function can only be applied on a character value")
  if (nchar(x) > 2 && substr(x, 1, 1) == '"' && substr(x, nchar(x), nchar(x)) == '"') {
    x <- substr(x, 2, nchar(x)-1)
    return(x)
  } else {
    return (x)
  }
}

quoteSQLIdentifier <- function(x) {
  # quote quotes
  x <- gsub('"', '""', x, fixed = TRUE)
  x <- gsub("'", "''", x, fixed = TRUE)
  if (nzchar(x)) {
    # quote statement
    return(paste0('"', x, '"'))
  } else {
    return("")
  }
}


colName <- function(colDef) {
  if (!inherits(colDef, 'ida.col.def'))
    stop("This function can only be applied on ida.col.def object")
  
  term = removeQuotes(colDef@term)
  tab <- colDef@table
  if (term %in% tab@cols)
    return(term)
    
  defCols <- tab@colDefs
  if (is.null(defCols) || length(defCols)==0)
    return(NULL)
  for (colName in names(defCols)) {
    if (term == defCols[[colName]])
      return(colName)
  }
}

# trim tariling and leading blanks
idaTrim <- function (x) gsub("^\\s+|\\s+$", "", x)

# trim trailing blanks
idaRTrim <- function (x) sub("\\s+$", "", x)

# trim leading blanks
idaLTrim <- function (x) sub("^\\s+", "", x)

# utility function for DB2/z:
# res is the result of an DB2/z INZA procedure call
# which is a data frame with columns SEQID and TABLE_DETAILS
idaDataFrameFromResultSet <- function(res)  {
   rows <- strsplit(res$TABLES_DETAILS, '\n') [[1]]
   nrows = length(rows)
   #first create an empty data frame
   colnames = strsplit(rows[2], '|', fixed=TRUE )[[1]]
   ncols = length(colnames)
   colname = idaTrim(colnames[1])
   trimmedColnames = c(colname)
   df <- data.frame("X"=1:(nrows-3))
   
   for(i in 2:(ncols)){
      colname = idaTrim(colnames[i])
      trimmedColnames = c(trimmedColnames, colname)
      df <- cbind(df, 1:(nrows-3))
   }
   colnames(df)<- trimmedColnames
   
   for (i in 4:(nrows)) {
    rowvals = strsplit(rows[i], '|', fixed=TRUE )[[1]]
    for (j in 1:ncols) {
      df[i-3, j]<- idaRTrim(substring(rowvals[j],2))
    }
   }
   return(df)
}


idaDataFrameFromResultStr.old <- function(resStr)  {
   rows <- strsplit(resStr, '\n') [[1]]
   nrows = length(rows)
   #first create an empty data frame
   colnames = strsplit(rows[2], '|', fixed=TRUE )[[1]]
   ncols = length(colnames)
   colname = idaTrim(colnames[1])
   trimmedColnames = c(colname)
   df <- data.frame("X"=1:(nrows-3))
   
   for(i in 2:(ncols)){
      colname = idaTrim(colnames[i])
      trimmedColnames = c(trimmedColnames, colname)
      df <- cbind(df, 1:(nrows-3))
   }
   colnames(df)<- trimmedColnames
   
   if (nrows >= 4) {
      for (i in 4:(nrows)) {
      rowvals = strsplit(rows[i], '|', fixed=TRUE )[[1]]
        for (j in 1:ncols) {
          df[i-3, j]<- idaRTrim(substring(rowvals[j],2))
        }
      }
   } else {
      # the result set is empty, but df has 2 rows, remove them
      df <- df[-c(1,2),]
   }
   return(df)
}


idaDataFrameFromResultStr <- function(resStr, rowSplitStr='\n', columnSplitStr = ' | ', headerRow=2, skippedDataRows=1, skippedRowChars=1)  {
   skippedRows <- skippedDataRows + headerRow-1
   rows <- strsplit(resStr, rowSplitStr, fixed=TRUE) [[1]]
   nrows = length(rows)
   #first create an empty data frame
   colnames = strsplit(rows[headerRow], columnSplitStr, fixed=TRUE )[[1]]
   ncols = length(colnames)
   colname = idaTrim(colnames[1])
   trimmedColnames = c(colname)
   df <- data.frame("X"=1:(nrows-1-skippedRows))
   
   for(i in 2:(ncols)){
      colname = idaTrim(colnames[i])
      trimmedColnames = c(trimmedColnames, colname)
      df <- cbind(df, 1:(nrows-1-skippedRows))
   }
   colnames(df)<- trimmedColnames
   
   if (nrows >= skippedRows+2) {
      for (i in (skippedRows+2):(nrows)) {
      rowvals = strsplit(substr(rows[i], skippedRowChars+1, nchar(rows[i])), columnSplitStr, fixed=TRUE )[[1]]
        for (j in 1:ncols) {
          df[i-1-skippedRows, j]<- idaRTrim(rowvals[j])
        }
      }
   } else if (skippedRows > 0){
     # the result set is empty, but df has the skipped rows, remove them
      df <- df[-c(1,skippedRows),]
   }
   
   return(df)
}


createDB2zTableAs <- function(tableName, asTableView, asAOT=TRUE, dbname="") {
  cols <- idaQuery("SELECT NAME,trim(COLTYPE) AS COLTYPE, LENGTH, SCALE FROM SYSIBM.SYSCOLUMNS WHERE TBNAME ='", asTableView,"' ORDER BY COLNO ASC")
  createTableStmt <- paste("CREATE TABLE ", tableName, " (", sep="")
  for(i in 1:nrow(cols)) {
    colType0 <- cols[i,"COLTYPE"];
    colType <- switch(colType0,
                      "VARBIN"="VARBINARY",
                      "VARG"="VARGRAPHIC",
                      "TIMESTMP"="TIMESTAMP",
                      "TIMESTZ"="TIMESTAMP WITH TIME ZONE",
                      colType0);

    colDecl <- paste("\"", cols[i,"NAME"], "\" ", colType, sep="")
    if ( colType %in% c("CHAR", "VARCHAR", "LONGVAR", "BINARY", "VARBINARY", "GRAPHIC", "VARGRAPHIC", "DECIMAL", "LONGVARG", "DECFLOAT") ) {
      colDecl <- paste(colDecl, "(", cols[i,"LENGTH"], sep="")
      if ( colType %in% c("DECIMAL") ) {
        colDecl <- paste(colDecl, ", ", cols[i,"SCALE"], sep="")
      }
      colDecl <- paste(colDecl, ")", sep="")
    }
    if (i < nrow(cols)) {
      colDecl <- paste(colDecl, ", ", sep="")
    }
    createTableStmt <- paste(createTableStmt, colDecl, sep="")
  }
  createTableStmt <- paste(createTableStmt, ")", sep="")
  if (asAOT) {
    createTableStmt <- paste(createTableStmt, " IN ACCELERATOR ", idaGetAccelerator(), sep="")
  } else if (dbname != "") {
    if (!idaExistDatabase(dbname)) {
      idaQuery("CREATE DATABASE ", dbname, " CCSID ", idaGetAcceleratorEncoding())
    }
    tbsp <- idaGetValidTablespaceName()
    idaQuery("CREATE TABLESPACE ", tbsp, " IN ", dbname, " CCSID ", idaGetAcceleratorEncoding())
    createTableStmt <- paste(createTableStmt, " IN ", dbname, ".", tbsp, sep = "")
  }
  createTableStmt <- paste(createTableStmt, " CCSID ", idaGetAcceleratorEncoding(), sep="")
  idaQuery(createTableStmt)
}
  