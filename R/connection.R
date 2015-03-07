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
  conOpen <- F;
  try({idadf(con,'select trim(current_schema) from sysibm.sysdummy1');conOpen<-T},silent=T);
  if(!conOpen) {
    stop("con is not an open connection, please use idaConnect() to create an open connection to the data base.");
  }
  
  #Put the connection object into a new environment
  assign("idaRGlobal", new.env(parent = baseenv()), envir=baseenv())
  assign("p_idaConnection", con, envir = idaRGlobal) 
  
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
  }
    
  #Set the current connection parameters for job monitoring
  try({idaQuery(paste("CALL WLM_SET_CLIENT_INFO(NULL,NULL,'ibmdbR','",script,"',NULL)",sep=''))},silent=T)
   
  #Check what functions are available in the database
  #c1 <- idaCheckProcedure("KMEANS","idaKMeans",T)
  #c2 <- idaCheckProcedure("NAIVEBAYES","idaNaiveBayes",T)
  #c3 <- T
  #  c3 <- idaCheckProcedure("ASSOCRULES","idaArule",T) 
  
  #if(!(c1&&c2&&c3)) {
  #  message("Note that not all backend databases provide push-down capabilities for all analytical functions.")
  #}
  
}

idaCheckConnection <- function () {
  # make sure that the connection exists
  if ((!exists("p_idaConnection",envir=idaRGlobal)) || is.null(get("p_idaConnection",envir=idaRGlobal)))
    stop("The connection is not set, please use idaAnalyticsInit(con), where con is an open connection.", call.=FALSE)
}

idaCheckProcedure <- function(procName, algName, verbose=F) {
  
  catQuery <- paste("SELECT COUNT(*) FROM SYSCAT.ROUTINES WHERE ROUTINENAME = '",procName,"' AND ROUTINEMODULENAME = 'IDAX'",sep='') 
  available <- idaScalarQuery(catQuery)>0;
  
  if(verbose&&!available) {
    message(paste("Function ",algName, " ",ifelse(available,"","not "),"available for this connection.",sep=''))
  } 
  
  invisible(available)
}

idaCheckRole <- function(roleName) {
  catQuery <- paste("SELECT COUNT(*) FROM SYSCAT.ROLES WHERE ROLENAME = '",roleName,"'",sep='') 
  available <- idaScalarQuery(catQuery)>0;
  return(available)
}
