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

idaParseRFormula <- function(form, idadf){
  if (!is.ida.data.frame(idadf)) 
    stop(simpleError("idadf must be an object of ida.data.frame class"))
  
  addIntercept = attr(terms(form, keep.order=T, data=data.frame(x=1)), "intercept")
  isResponse   = attr(terms(form, keep.order=T, data=data.frame(x=1)), "response")
  
  vars = all.vars(form)
  
  colu = idadf@cols
  where.is.dot = grep(".",vars,fixed=T)
  if (length(where.is.dot)>0) {
    if (!all(vars[-where.is.dot] %in% colu))
      stop(simpleError(paste("Some variables are not available in ida.data.frame:", paste(setdiff(vars[-where.is.dot], colu),collapse=", "))))    
    predictors <- c(vars[-c(where.is.dot,isResponse)], setdiff(colu, vars))
    
  } else {
    tab1 = attr(terms(form), "factors")
    predictors <- c(colnames(tab1))  	
  }
  
  addIntercept = attr(terms(form, keep.order=T, data=data.frame(x=1)), "intercept")
  isResponse   = attr(terms(form, keep.order=T, data=data.frame(x=1)), "response")
  
  cols <- c();
  colLevels <- c();
  allCols <- c();
  
  for(pred in predictors) {
    
    tok <- strsplit(as.character(pred),':');
    
    if(length(tok[[1]])==1) {
      cols <- c(cols,pred);
      allCols <- c(allCols,pred);
    }	
    
    if(length(tok[[1]])==2) {
      colLevels <- c(colLevels,tok);
      allCols <- c(allCols,tok[[1]][1]);
    }	
  }	 	
  if (!all(allCols %in% colu))
    stop(paste("Some variables are not available in ida.data.frame:", paste(setdiff(allCols, colu),collapse=", ")))
  
  list(cols=cols,colLevels=colLevels, intercept=addIntercept, response=vars[isResponse])
}


idaParseGlmRFormula <- function(form, idadf){
  if (!is.ida.data.frame(idadf)) 
    stop(simpleError("idadf must be an object of ida.data.frame class"))
  
  vars = all.vars(form)
  
  # is there a dot?  
  # special case - if there is . in the variable list
  colu = idadf@cols
  where.is.dot = grep(".",vars,fixed=T)
  if (length(where.is.dot)>0) {
    if (!all(vars[-where.is.dot] %in% colu))
      stop(simpleError(paste("Some variables are not avaliable in ida.data.frame:", paste(setdiff(vars[-where.is.dot], colu),collapse=", "))))    
    vars = c(vars[-where.is.dot], setdiff(colu, vars))
    fiordervars = seq_along(vars)[-1]
    hiordervars = NULL
  } else {
    tab1 = attr(terms(form), "factors")
    order1 = attr(terms(form), "order")
    
    #
    # only first order variables
    fordervars = sapply(which(order1==1), function(i) vars[tab1[,i]>0])
    fiordervars = sapply(which(order1==1), function(i) which(tab1[,i]>0))
    
    #
    # only second or higher order variables
    hordervars = sapply(which(order1>=2), function(i) vars[tab1[,i]>0])
    hiordervars = sapply(which(order1>=2), function(i) which(tab1[,i]>0))
    # test: are all variables in data frame?
    if (!all(vars %in% colu))
      stop(simpleError(paste("Some variables are not avaliable in ida.data.frame:", paste(setdiff(vars, colu),collapse=", "))))
  }
  
  
  # or +0 or -1
  addIntercept = attr(terms(form, keep.order=T, data=data.frame(x=1)), "intercept")
  isResponse   = attr(terms(form, keep.order=T, data=data.frame(x=1)), "response")
  
  areFactors = rep(F, length(vars))
  if (length(where.is.dot) == 0) 
    areFactors   = paste("factor(",vars,")",sep="") %in% rownames(tab1)
  
  # check agains transformations    
  if (length(where.is.dot)>0) {
    # left side    
    if (length(form)==3 & !(as.character(form)[2] %in% vars))
      stop(paste("Error: Variable transformations are not supported."))
  } else {
    are.not1 <- !(vars %in% rownames(tab1))
    are.not2 <- !(paste("factor(",vars,")",sep="") %in% rownames(tab1))
    are.not <- are.not1 & are.not2 
    if (any(are.not)) {
      stop(paste("Error: Variable transformations are not supported."))
    }
  }
  
  # create corresponding ida data frame
  indy = unlist(sapply(vars, function(x) which(colu==x, T)))
  list(data=idadf[,indy], intercept=addIntercept, response=isResponse, varlist=vars, areFactors = areFactors, fiordervars = fiordervars, hiordervars = hiordervars)
}


idaCreateInputDataView <- function(form,idadf) {
  
  parsedForm <- idaParseRFormula(form, idadf)
  cols <- c(parsedForm$response,parsedForm$cols);
  
  tableDef <- idaTableDef(idadf, F);
  colList <- c()
  for (i in 1:length(cols)) {
    if (tableDef[tableDef$name == cols[i], "valType"] == "NUMERIC")
      colList <- c(colList, paste("CAST(\"", cols[i], "\" AS DOUBLE) AS \"",cols[i],"\"",sep=''))
    else 
      colList <- c(colList, paste("\"", cols[i], "\"", sep=''));
  }
  
  whereNotNull <- paste(sapply(cols, function(x) paste("\"", x, "\" IS NOT NULL", sep='')),collapse=' AND ', sep='');
  
  #if(!is.null(parsedForm$colLevels)) {
  #colIndex <- 1;	
  #for(levCol in parsedForm$colLevels)	{
  #	queryCols <- c(queryCols,paste("CASE WHEN \"",levCol[1],"\" =  '",levCol[2],"' THEN 1 ELSE 0 END AS C",colIndex," ",sep='')); 
  #	colIndex <- colIndex+1;
  #}
  
  #for(levCol in parsedForm$colLevels)	{
  #	cols <- c(cols,paste(levCol[1],"_",levCol[2],sep=''));  
  #}
  #}
  
  viewName <- idaGetValidTableName();
  query <- paste("CREATE VIEW ",viewName," AS SELECT ", paste(colList,sep=',',collapse=',')," FROM ",idadf.from(idadf),sep='');
  if (nchar(idadf@where)) {
    query <- paste(query, " WHERE ", idadf@where, " AND ", whereNotNull)
  } else {
    query <- paste(query, " WHERE ", whereNotNull)
  }
  
  idaQuery(query);
  list(view=viewName,colNames=cols);
}
