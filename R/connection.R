# 
# Copyright (c) 2010, 2014, IBM Corp. All rights reserved. 
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


idaInit <- function(con,jobDescription=NULL) {
  #Check if the connection is open?
  conOpen <- FALSE;
  try({idadf(con,'select trim(current_schema) from sysibm.sysdummy1');conOpen<-TRUE},silent=TRUE);
  if(!conOpen) {
    stop("con is not an open connection, please use idaConnect() to create an open connection to the data base.");
  }

  #Put the connection object into a new environment
  assign("idaRGlobal", new.env(parent = baseenv()), envir=baseenv())
  assign("p_idaConnection", con, envir = idaRGlobal) 

  # a DB2 database (i.e. subsystem) comprises multiple database in contrast to a DB2 LUW database 
  isDB2z <- odbcGetInfo(con)[1]=="DB2" & (odbcQuery(con, "select count(*) from SYSIBM.SYSTABLES where CREATOR = 'SYSIBM' and NAME='SYSDATABASE'") > 0)
  
  assign("p_db2z",isDB2z,envir=idaRGlobal)    
  cs <- sub('PWD=******;', '', attributes(con)$connection.string,  fixed=T)
  connectString <- substr(cs, start=5, stop=nchar(cs))
  
  assign("p_connectString", connectString, envir = idaRGlobal)
  assign("p_nbReconnects", 0,  envir = idaRGlobal)
  if (isDB2z) {
    # On DB2 z/OS create a DGTT to transfer error messages to the R client
    idaQuery("DECLARE GLOBAL TEMPORARY TABLE SESSION.INZA_MSG(MESSAGE VARCHAR(2000), RETVAL VARCHAR(8192)) CCSID UNICODE ON COMMIT PRESERVE ROWS")
	idaQuery("INSERT INTO SESSION.INZA_MSG(MESSAGE) values('Dummy message')")
  } else  {
     #Check Oracle compatibility
     regVars <- idaQuery("SELECT reg_var_name, reg_var_value, level FROM table(REG_LIST_VARIABLES()) as reg")
     compVec <- regVars[regVars$REG_VAR_NAME=='DB2_COMPATIBILITY_VECTOR',2] 
  
    if(length(compVec)) {
      if(compVec=='ORA') {
        assign("p_db2_comp", "ORA", envir = idaRGlobal) 
      }
    }
  }
  
  script <- jobDescription
  #Set the application parameters
  if(is.null(script)) {
	#Need to find out how the current script is called if possible
	script <- 'interactive' 
	scriptFiles <- lapply(sys.frames(), function(x) x$ofile)
	for(i in 1:length(scriptFiles)) {
		if(!is.null(scriptFiles[[i]])) {
			script <- scriptFiles[[i]];
			break;
		}
	}
  
    if (!isDB2z) {
		# Set the current connection parameters for job monitoring
		try({idaQuery(paste("CALL WLM_SET_CLIENT_INFO(NULL,NULL,'ibmdbR','",script,"',NULL)",sep=''))},silent=T)
	}
  }   
  #Check what functions are available in the database
  c1 <- idaCheckProcedure("KMEANS","idaKMeans",TRUE) 
  c2 <- idaCheckProcedure("NAIVEBAYES","idaNaiveBayes",TRUE)
  if (isDB2z) {
  	c3 <- idaCheckProcedure("ARULE","idaArule",TRUE)
  	c4 <- idaCheckProcedure("TWOSTEP","idaTwoStep",TRUE)
  } else  {
	c3 <- idaCheckProcedure("ASSOCRULES","idaArule",TRUE)
  	c4 <- idaCheckProcedure("LINEAR_REGRESSION","idaLm",TRUE)
  }	
  #c5 <- idaCheckProcedure("SEQRULES","idaSeqRules",TRUE)
  c5 <- T
  
  if(!(c1&&c2&&c3)) {
	message("Note that not all backend databases provide push-down capabilities for all analytical functions.")
  }
}

idaIsDb2z <- function() {
  return(get("p_db2z",envir=idaRGlobal))
}

idaListAccelerators <- function() {
  idaCheckConnection();
  con <- get("p_idaConnection", envir=idaRGlobal)
  if (!idaIsDb2z()) {
	stop(sprintf("%s is not a DB2/z database", odbcGetInfo(con)[4]))
  } else if (!idaExistTable("SYSACCEL.SYSACCELERATORS")) {
	stop(sprintf( "The DB2/z database %s has not been enabled for acceleration.", odbcGetInfo(con)[4]))
  } else {
    idaQuery("select ACCELERATORNAME from SYSACCEL.SYSACCELERATORS")
  } 
}

idaSetAccelerator <- function(acceleratorName, queryAcceleration="ENABLE") {
   if (!(toupper(queryAcceleration) %in% c("NONE", "ENABLE", "ENABLE WITH FALLBACK", "ELIGIBLE", "ALL"))) {
		stop(sprintf('The value %s for the queryAcceleration parameter is invalid. Valid values are ""NONE", "ENABLE", "ENABLE WITH FALLBACK", "ELIGIBLE" and "ALL".', queryAcceleration))
   }
   if(toupper(queryAcceleration) == "NONE") {
   		warning(sprintf('With the value "%s" for query acceleration no queries will be accelerated and no advanced analytics functions will work.', queryAcceleration))	
   }
   assign("p_accelerator", "", envir = idaRGlobal)
   assign("p_encoding", "", envir=idaRGlobal)
   
   accls <- idaListAccelerators()
   if (length(accls[accls$ACCELERATORNAME==acceleratorName,])> 0) {
		ntables <- idaScalarQuery(paste("select count(*) from SYSACCEL.SYSACCELERATEDTABLES where ACCELERATORNAME = '", acceleratorName, "'", sep = ""))
		if (ntables == 0) {
			stop(sprintf("No tables are enabled for acceleration for accelerator %s. ", acceleratorName))
		} 
		encoding <- idaScalarQuery(paste( "select distinct (ENCODING_SCHEME) from SYSIBM.SYSTABLES t, SYSACCEL.SYSACCELERATEDTABLES a ", 
										  "where a.ACCELERATORNAME = '", acceleratorName, 
										  "' and ENABLE = 'Y' and T.CREATOR = A.CREATOR and T.NAME = A.NAME", 
										  sep=""))
		assign("p_accelerator", acceleratorName, envir = idaRGlobal)
		assign("p_encoding", switch(EXPR = encoding, 'A'="ASCII", 'E'="EBCDIC", 'U'="UNICODE", "UNICODE"), envir=idaRGlobal)
		idaQuery("SET CURRENT QUERY ACCELERATION ", queryAcceleration)
		assign("p_query_acceleration", queryAcceleration,  envir = idaRGlobal)
   } else  {
		stop(sprintf("The accelerator with the name %s is not known.", acceleratorName))
   }
}
	



idaGetAccelerator <- function() {
	if (exists("p_accelerator",envir=idaRGlobal))  {
		return(get("p_accelerator",envir=idaRGlobal))
	} else  {
		stop(sprintf("The name of the accelerator hasn't been set with the idaSetAccelerator function."))
	}
}

idaGetAcceleratorEncoding <- function() {
	if (exists("p_accelerator",envir=idaRGlobal))  {
		return(get("p_encoding",envir=idaRGlobal))
	} else  {
		stop(sprintf("The name of the accelerator hasn't been set with the idaSetAccelerator function."))
	}
}


idaGetAcceleratorDetails <- function() {
	if (exists("p_accelerator",envir=idaRGlobal))  {
		return(list(Accelerator=get("p_accelerator",envir=idaRGlobal), 
		            Encoding=get("p_encoding",envir=idaRGlobal),
		            QueryAcceleration=get("p_query_acceleration",envir=idaRGlobal)))
	} else  {
		stop(sprintf("The name of the accelerator hasn't been set with the idaSetAccelerator function."))
	}	
}


idaIsOracleMode <- function() {
  return(exists("p_db2_comp",envir=idaRGlobal)&&(get("p_db2_comp",envir=idaRGlobal)=='ORA'))
}

idaCheckConnection <- function () {
  # make sure that the connection exists
  if ((!exists("p_idaConnection",envir=idaRGlobal)) || is.null(get("p_idaConnection",envir=idaRGlobal)))
    stop("The connection is not set, please use idaInit(con), where con is an open connection.", call.=FALSE)
   	
}

idaCheckProcedure <- function(procName, algName, verbose=FALSE) {
  
  if(idaIsDb2z()) {
	catQuery <- paste("SELECT COUNT(*) FROM SYSIBM.SYSROUTINES WHERE NAME = '",procName,"' AND SCHEMA = 'INZAR'",sep='') 
  } else {
	catQuery <- paste("SELECT COUNT(*) FROM SYSCAT.ROUTINES WHERE ROUTINENAME = '",procName,"' AND ROUTINEMODULENAME = 'IDAX'",sep='') 
  } 
  available <- as.numeric(idaScalarQuery(catQuery))>0;
  
  if(verbose&&!available) {
    message(paste("Function ",algName, " ",ifelse(available,"","not "),"available for this connection.",sep=''))
  } 
  
  invisible(available)
}

idaCheckRole <- function(roleName) {
  #catQuery <- paste("SELECT COUNT(*) FROM SYSCAT.ROLES WHERE ROLENAME = '",roleName,"'",sep='') 
  #available <- idaScalarQuery(catQuery)>0;
  
  #if(!available)
  #  return(FALSE)
  if(idaIsDb2z()) {
	# for DB2z/OS we assume the role "R_USERS__PUBLIC"
	granted <- roleName=="R_USERS_PUBLIC"
  } else {
	catQuery <- paste("SELECT COUNT(*) FROM SYSCAT.ROLEAUTH WHERE ROLENAME = '",roleName,"' AND GRANTEE=CURRENT USER",sep='') 
	granted <- as.numeric(idaScalarQuery(catQuery))>0;
  }
  return(granted)
}

idaCheckSharing <- function() {
  
  roledashDB <- "DASHDB_ENTERPRISE_USER"
  roleDB2 <- "R_USERS_PUBLIC"
  if(idaIsDb2z()) {
	# for DB2z/OS we assume the role "R_USERS__PUBLIC"
	return(roleDB2)
  } else {
	#First try dashDB
	if(idaCheckRole(roledashDB)) {
		return(roledashDB)
	} else if(idaCheckRole(roleDB2)) {
		return(roleDB2)
	} else {
		return(NULL)
	}
  }
}
  
reconnect <- function() {
	connectString <- get("p_connectString", envir=idaRGlobal)
	nbReconnects <- get("p_nbReconnects", envir=idaRGlobal)
	try({idaClose()}, silent=TRUE)
	con <- idaConnect(connectString)		
	assign("p_idaConnection", con, envir = idaRGlobal) 
	assign("p_nbReconnects", nbReconnects + 1, envir = idaRGlobal) 
	if (idaIsDb2z())  {
		sqlQuery(con, "DECLARE GLOBAL TEMPORARY TABLE SESSION.INZA_MSG(MESSAGE VARCHAR(2000), RETVAL VARCHAR(8192)) CCSID UNICODE ON COMMIT PRESERVE ROWS")
		sqlQuery(con, "INSERT INTO SESSION.INZA_MSG(MESSAGE) values('Dummy message')")
		sqlQuery(con, paste("SET CURRENT QUERY ACCELERATION ",  get("p_query_acceleration", envir=idaRGlobal), sep=""))
	}
}


  
