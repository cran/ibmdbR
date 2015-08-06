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

################ Covariance matrix calculation ############################
idaDotProduct <- function(idadf,colNames,intercept,limit) {
  cols <- colnames(idadf);
  tableDef <- idaTableDef(idadf, F);
  numOfCatCols <- nrow(tableDef[tableDef$valType == "CATEGORICAL",]);
  numCols <- length(cols)
  if (intercept) 
  	 numCols <- numCols + 1

  if (numCols <= 42 && numOfCatCols == 0) {
    idaDotProductSQL(idadf,colNames,intercept);
  }else if (numCols <= 87) {
	  idaDotProductUDTF(idadf,colNames,intercept,limit)
  }else{
  	stop("Only up to 87 columns are currently supported.");
  }
}

idaDotProductSQL <- function(idadf,colNames,intercept) {
  
  cols <- colnames(idadf);
  
  if(length(cols)>42)
    stop("Only up to 40 predictors are currently supported.");
  
  queryList <- c();	
  if(intercept) {	
    for(i in 1:length(cols)) {
      for(j in i:length(cols)) {
        queryList <- c(queryList,paste("SUM(\"", cols[i] ,"\"*\"", cols[j],"\") ",sep=''));
      }
      queryList <- c(queryList,paste("SUM(\"", cols[i] ,"\")",sep=''));
    }
    queryList <- c(queryList,"COUNT(*)");	
    
    query <- paste("SELECT ", paste(queryList,sep=',',collapse=',')," FROM ",idadf.from(idadf),sep='');
    res <- idaQuery(query);
    res[[length(res)]] <- as.numeric(res[[length(res)]])
    
	if (nrow(res[complete.cases(res),]) == 0) {
		stop("There is no valid data in the input table for building the model. Note that the rows with missing values in your input data are ignored.");
	}
	
    mdat <- matrix(1:(length(cols)+1)*(length(cols)+1),nrow=length(cols)+1,ncol=length(cols)+1,dimnames = list(c(colNames, "Intercept"),c(colNames, "Intercept")),byrow=T)
    
    r <- 1;
    c <- 1;
    for(i in 1:ncol(res)) {
      mdat[r,c] <- res[[i]][1];
      mdat[c,r] <- mdat[r,c];
      c <- c + 1;
      if(c>length(cols)+1) {
        r <- r+1;
        c <- r;
      }
    }
    mdat
  } else {
    for(i in 1:length(cols)) {
      for(j in i:length(cols)) {
        queryList <- c(queryList,paste("SUM(\"", cols[i] ,"\"*\"", cols[j],"\") ",sep=''));
      }
    }
    
    query <- paste("SELECT ", paste(queryList,sep=',',collapse=',')," FROM ",idadf.from(idadf),sep='');
    res <- idaQuery(query);

	if (nrow(res[complete.cases(res),]) == 0) {
		stop("There is no valid data in the input table for building the model. Note that the rows with missing values in your input data are ignored.");
	}
	
    mdat <- matrix(1:(length(cols))*(length(cols)),nrow=length(cols),ncol=length(cols),dimnames = list(colNames,colNames),byrow=T)
    
    r <- 1;
    c <- 1;
    for(i in 1:ncol(res)) {
      mdat[r,c] <- res[[i]][1];
      mdat[c,r] <- mdat[r,c];
      c <- c + 1;
      if(c>length(cols)) {
        r <- r+1;
        c <- r;
      }
    }
    mdat
  }
  
}

idaDotProductUDTF <- function(idadf,colNames,intercept, limit) {
	
	# Verify that the UDTF is registered
	result <- idaQuery("select char(funcname,30), char(funcschema,30) from syscat.functions where funcname = 'DIST_LM_UDTF' AND funcschema = 'SYSIBMADM'");
	if (nrow(result) == 0) {
		stop("The required UDTF is not registered. 
              The R client that you use is not compatible  
              with the uderlying database version.");
	}
	
	# Verify that the limit parameter is a positive integer.
	limit <- as.integer(limit)
	if (limit <= 0) {
		stop("The parameter limit must be a positive integer value greater than 0.")
	}
	
	# Get column type information: continuous (NUMERIC) vs. nominal (CATEGORICAL) columns
	tableDef <- idaTableDef(idadf, F);
	
	# The target column must be continuous
	if (tableDef[1,"valType"] != "NUMERIC") 
		stop("The target column must be continuous.");
	targetColumn <- colNames[1];
	numOfContCols <- nrow(tableDef[tableDef$valType == "NUMERIC",]);
	
	# Retrieve column names ordered by type
	tableDef <- tableDef[tableDef$name %in% colNames[-1],];
	cols <- as.character(tableDef[(order(tableDef[,"valType"], decreasing=T)), "name"]);
	
	# workaround: can be removed when TABLE WITH FINAL is supported
	rowCount <- as.numeric(idaScalarQuery("SELECT COUNT(*) FROM ",idadf.from(idadf)));
	if (rowCount == 0) {
		stop("There is no valid data in the input table for building the model. Note that the rows with missing values in your input data are ignored.");
	}
	
	# Get cardinalities for nominal columns
	nomCols <- as.character(tableDef[tableDef[,3] == "CATEGORICAL", 1]);
	contCols <- as.character(tableDef[tableDef[,3] == "NUMERIC", 1]);

	case_list <- c();
	nomColNames <- c();
	if (length(nomCols) > 0) {
		card <- c();
		for(i in 1:length(nomCols)) {
			# Retrieve the different values that each categoric column can have 
			query <- paste("select \"", nomCols[i], "\" from ", idadf.from(idadf), 
					" group by \"", nomCols[i], "\" order by \"", nomCols[i], "\"", sep='');
			values <- idaQuery(query)[,1];
			
			# Concatenate the column name with each value. This is needed to map back positions in the
		    # RCV format returned by UDTF to unique column names. 
			nomColNames <- c(nomColNames, sapply(values, function(x) paste(nomCols[i],"_",x, sep='')));
			
			# Build CASE WHEN statement for each categoric column to pass to the UDTF only an
		    # index instead of the actual column value.
			case_stmt <- paste("CASE \"", nomCols[i], "\" ", sep=''); 
			for (j in 1: length(values))
				case_stmt <- paste(case_stmt, " WHEN '", values[j], "' THEN ", j, sep='');
			case_stmt <- paste(case_stmt, " END", sep='');
			case_list <- c(case_list, case_stmt);

			# Get the cardinality for each column.
			card <- c(card, length(values));
		}
		
		# Build a list of cardinalities to pass to the UDTF 
		if (length(nomCols[card > limit]) > 0) {
			stop(paste("The following categoric column's cardinality is greater than ", limit,
					   ". Remove these columns: ", paste(nomCols[card > limit], collapse=', '), 
					   " or increase the limit for the nominal column cardinality in your idaLm call.", sep=''));
		}
		cardinalities <- paste(card, collapse=':')
		cardinalities <- paste("'", cardinalities, ":'", sep='')
		
	} else cardinalities <- "''"
	
    #Fix for oracle compatibility mode
    if(cardinalities=="''")
      cardinalities <- "' '"
    
	# Put together the query that calls the UDTF to calculate the co-variance matrix
	# Expected order of columns: target, (intercept,) continuous columns, nominal columns 
	queryList <- c();
	queryList <- c(queryList, paste("\"", targetColumn ,"\"",sep=''));
	if (intercept) {
		queryList <- c(queryList, '1E0');
		numOfContCols <- numOfContCols + 1;
	} 
	queryList <- c(queryList, sapply(contCols, function(x) paste("\"", x ,"\"",sep='')));
	query <- paste("select row_name, col_name, val from ", idadf.from(idadf),  
			", TABLE(IDAX.DIST_LM_UDTF(");
	query <- paste(query, rowCount[[1]], ",", numOfContCols, ",", cardinalities, ",", 
			       paste(c(queryList,case_list),sep=',',collapse=','), "))");
	res <- idaQuery(query);
	#print(query);
	#print(res);
	
	# From the UDTF we get a table in RCV format where row and column names are the position 
	# of the column in the column list as passed to the UDTF. 
	# Now we have to map back the position to the actual column name:
	
	# Build column name dictionary
	contCols <- contCols[contCols != targetColumn];
	if (intercept)
		contCols <- c(targetColumn, "Intercept", contCols)
	else 
		contCols <- c(targetColumn, contCols);

	fullNames <- c(contCols, nomColNames)
	mapToFullName <- setNames(fullNames, 1:length(fullNames)); 
	
	# Replace row and column names with there full column name
	res$ROW_NAME <- mapToFullName[res$ROW_NAME]
	res$COL_NAME <- mapToFullName[res$COL_NAME]
	
	# fill matrix from RCV table
	fullNames <- unique(res$ROW_NAME);
	fullNames <- c(targetColumn, fullNames[! fullNames %in% c(targetColumn, "Intercept")]);
	if (intercept)
		fullNames <- c(fullNames, "Intercept");
	
	mdat <- matrix(0,nrow=length(fullNames),ncol=length(fullNames),dimnames = list(fullNames,fullNames),byrow=T)
	for(i in 1:nrow(res)) {
		mdat[res[[1]][i],res[[2]][i]] <- as.numeric(res[[3]][i]);
		mdat[res[[2]][i],res[[1]][i]] <- as.numeric(res[[3]][i]);
	}
	
	#print(mdat);
	mdat
}


################ Linear regression ############################
idaLm <- function(form,idadf, limit=25) {
  
  if(!is.ida.data.frame(idadf))
    stop("This function can only be applied to ida.data.frame objects.")
  
  idaCheckConnection();
  
  formOut <- idaParseRFormula(form,idadf);
  
  if(length(formOut$response)==0) {
    stop("No target variable specified.")
  }
  
  input <- idaCreateInputDataView(form,idadf);
  inputDf <- ida.data.frame(input$view);
  
  res <- NULL;
  tryCatch({
        
        mat <- idaDotProduct(inputDf,input$colNames,formOut$intercept, limit);
        
        p = ncol(mat)-1
        n = mat[p+1,p+1]
        YTY = mat[1,1]
        XTX = mat[-1,-1]
        XTY = mat[1,-1]
        
        res <- ida.lm(XTX, XTY, YTY, n, weights=weights)
      },error=function(e){stop(e)},
      finally={idaDropView(input$view);});
  res;
}

getBeta <- function(XTX, XTY, idaInvXTX) {
  beta = idaInvXTX %*% XTY
  rownames(beta) = colnames(XTX)
  beta
}

getLogLikelihood <- function(RSS, n, p) {
  -n/2*log(2*pi) - n/2*log(RSS/(n-p)) - (n-p)/2
}

getAIC <- function(L, n, p) {
  -2*L + 2*p
}

getBIC <- function(L, n, p) {
  -2*L + log(n)
}

getR2 <- function(XTX, YTY, beta) {
  (t(beta) %*% XTX %*% beta)/YTY
}

getTest <- function(idaInvXTX, beta,n, p, sigmasq=1) {
  sdbeta =  sqrt(diag(idaInvXTX)*sigmasq)
  tval  = beta/sdbeta
  pval   = 1 - abs(1 - 2*pt(tval, n-p))
  cbind(sdbeta, tval, pval)
}

ida.lm <- function(XTX, XTY, YTY, n, weights=-1) {
  #
  # in current implementation we use (X^T X)^-1 X^T Y equation
  # that's due to compability with ridge
  
  idaInvXTX = NULL;
  
  #Try solve first, only if it fails use ginv
  try({idaInvXTX = solve(XTX)},silent=T)
  if(is.null(idaInvXTX)) {
    idaInvXTX = ginv(XTX);
  }
  
  beta = getBeta(XTX, XTY, idaInvXTX)
  names(beta) = colnames(XTX)
  RSS  = (YTY - 2* t(beta) %*% XTY + t(beta) %*% XTX %*% beta)[1,1]
  ran = sum(abs(eigen(XTX, only.values=T)$values)>10^-8)
  p    = min(ran, n-1)
  likelihood = getLogLikelihood(RSS, n, p)
  AIC  = getAIC(likelihood, n, p)
  BIC  = getBIC(likelihood, n, p)
  tests      = getTest(idaInvXTX, beta, n, p, RSS/(n-p))
  coeftab = data.frame(Estimate = beta, Std.Error = tests[,1], t.value = tests[,2], p.value = tests[,3])
  rownames(coeftab) = names(beta)
  res = list(coefficients = beta, RSS=RSS, effects=NULL, rank = p, df.residuals = n-p, coefftab = coeftab, Loglike = likelihood, AIC=AIC, BIC=BIC)
  class(res) = c("idaLm")
  res
}

print.idaLm <- function(x) {
  cat("\nCoefficients:\n")
  print(x$coefftab)
  cat("\nResidual standard error: ", x$RSS, " on ",x$df.residuals," degrees of freedom\n\n")
  cat("Log-likelihood: ", x$Loglike, "\n")
  cat("AIC: ", x$AIC, "\n")
  cat("BIC: ", x$BIC, "\n")
}