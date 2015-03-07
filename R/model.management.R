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
# 

idaListModels <- function() {
  idaQuery("call idax.list_models()")
}

idaDropModel <- function(modelname) {
  xx <- parseTableName(modelname);
  model <- paste('"',xx$schema,'"."',xx$table,'"',sep=''); 	
  
  idaQuery("CALL IDAX.DROP_MODEL('model=",model,"')")
}

#idaGetModelname <- function(object) {
#  if(inherits(object,"rules")) {
#    return(object@info$model)
#  } else if(inherits(object,"idaKMeans")){
#    return(object$model)
#  } else if(inherits(object,"idaNaiveBayes")) {
#    return(object$model) 
#  } else {
#    stop("Model type not supported")    
#  }
#}

idaRetrieveModel <- function(modelname) {
  xx <- parseTableName(modelname);
  
  model <- xx$table
  modelSchema <- xx$schema
  
  if(idaExistTable(paste('"',modelSchema,'"."',model,'_CLUSTERS"',sep=""))) {
    idaRetrieveKMeansModel(modelname)
  } else {#if(idaExistTable(paste('"',modelSchema,'"."',model,'_DISC"',sep=""))) {
    idaRetrieveNBModel(modelname)
  } 
#    else 
#    {
#    idaRetrieveRulesModel(modelname) 
#  }
}

idaModelExists <- function(modelname) {
  xx <- parseTableName(modelname);
  models <- idaListModels()
  return(nrow(models[(models$MODELNAME==xx$table)&(models$MODELSCHEMA==xx$schema),])>0)
}

idaGetValidModelName <- function(prefix) {
  prefix <-toupper(prefix); 
  while (TRUE) {
    name <- paste(prefix, floor(runif(1,0,100000)),'_',as.integer(Sys.time()), sep="")
    if (!idaModelExists(name))
      return(name)
  }
}