# 
# Copyright (c) 2010, 2014, 2016 IBM Corp. All rights reserved. 
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

idaGlm <- function(form,
                   data,
                   id  = "id",
                   intercept = T,
                   family = "bernoulli",
                   family_param = -1,
                   link = "logit",
                   link_param = 1,
                   maxit = 20,
                   eps = 1e-3,
                   tol = 1e-7,
                   method = "irls",
                   trials = NULL,
                   incolumn = "",
                   interaction = "",
                   modelname = NULL, 
                   format = "glm",
                   raw.resid = F,
                   dropAfter = F,
                   ...) {
  
  if(!idaCheckProcedure("GLM","idaGlm",F)) {
    stop("Function not available.")
  }
  
  if(idaIsDb2z()) {
    encoding <- idaGetAcceleratorEncoding();
    if (encoding != "UNICODE"){
      stop(paste("For DB2 for z/OS connections only UNICODE data are supported. The encoding of the input data, however is ",
                  encoding, sep=""))
    }
  } else if (!idaIsRegularTable(data@table) && !is.null(data@where)) {
    stop(simpleError("Only IDA data frames on regular tables without where-conditions are supported for idaGlm."))
  }

  model <- modelname
  if (all(is.null(data), is.null(model))) {
    stop("Plase specify either data or model.")
  }
  
  # switch to retrieve mode, if necessary
  if (all(is.null(data), !is.null(model))) {
    if (format == "glm") {
      result <- idaGlm.format.glm(model, call=NULL, id, form, data=NULL)
    } else if (format == "raw") {
      result <- idaGlm.format.raw(model, raw.resid)
    }
  } else {	
    call <- match.call()
    ntab1 = idaParseGlmRFormula(form, data)
    # dataTmp preparation to run the SQL procedure
    varY  <- paste('\"',ntab1$varlist[1],'\"',sep="")
    dataTmp = data[, which(data@cols %in% c(ntab1$varlist, id, trials))]
    
    if(is.null(model)) {
      model <- idaGetValidModelName('GLM_')
    }  else {
      if(grepl(" ", model)) {
        stop("Space in model name not allowed.")
      }
      
      xx <- parseTableName(modelname);
      if (idaIsDb2z()) {
        model <- paste('"',xx$table,'"',sep=''); 	
      } else {
        model <- paste('"',xx$schema,'"."',xx$table,'"',sep=''); 	
      }
      
      if(idaModelExists(model)){
        stop('Model with the same name already exists.')
      }
    }
    
    
    colu = data@cols
    if (!(id %in% colu))
      stop(simpleError(paste("Id variable is not avaliable in ida.data.frame:", id)))
    id <- paste('\"',id,'\"',sep="")
    
    # check for factors, unless overwritten by user
    if (incolumn == "") {
      if (sum(ntab1$areFactors)!=0) {
        incolumn <- paste(dQuoteSimple(ntab1$varlist[ntab1$areFactors]),collapse=":nom;")
        incolumn <- paste(incolumn,":nom;",sep="")
      } else {
        incolumn <- NULL
      }
    }
    
    # check for interaction terms, unless overwritten by user
    if (interaction == "") {
      if (length(ntab1$hiordervars)!=0) {
        interaction <- paste(dQuoteSimple(lapply(ntab1$hiordervars,function(x) paste(ntab1$varlist[x],collapse='*'))),collapse=';')
      } else {
        interaction <- NULL
      }
    }
    
    # check for trials parameter
    if(!is.null(trials)) {
      trials <- dQuoteSimple(trials)
    }
    
    dataTmpv = idaCreateView(dataTmp)
    
    tryCatch({
      if (idaIsDb2z()) {
        callSP("GLM ",
                model=model,
                intable=dataTmpv,
                id=id,
                target=varY,
                family=family,
                family_param=family_param,
                link=link,
                link_param=link_param,
                maxit=maxit,
                eps=eps,
                tol=tol,
                method=method,
                trials=trials,
                intercept=intercept,
                incolumn=incolumn,
                interaction=interaction,
                ...)
       } else {
        if (is.null(incolumn)) {
          # if we use the input table as intable we have to specify the input columns explicitley
          incolumn <-
            paste("\"",
                  paste(dataTmp@cols[ !sapply(dataTmp@cols, FUN=function(x) {paste("\"", x, "\"",sep="")})
                                      %in%
                                      c(id,varY) ],
                        collapse="\";\""),
                  "\"",
                  sep="")
        }
        callSP("GLM ",
                model=model,
                intable=data@table,
                id=id,
                target=varY,
                family=family,
                link=link,
                link_param=link_param,
                maxit=maxit,
                tol=tol,
                trials=trials,
                intercept=intercept,
                incolumn=incolumn,
                effect=interaction,
                ...)
      }
    }, error = function(e) {
      # in case of error, let user know what happend
      stop(e)
    }, finally = {
      # drop view
      idaDropView(dataTmpv)
    }
    )
    
    if (format == "glm") {
      result <- idaGlm.format.glm(model, call, id, form, data)
    } else if (format == "raw") {
      result <- idaGlm.format.raw(model, raw.resid)
    }
  }
  
  if (dropAfter) {
    try(idaDropModel(model))
  }
  
  return(result)
}

#------------------------------------------------------------------------------

idaGlm.format.glm <- function(model, call, id, form, data) {
  
  xx <- parseTableName(model);
  modelName <- xx$table
  modelSchema <- xx$schema
  
  # results are converted
  
  dictCols <- "ATTNUM, ATTNAME, ATTTYPE, DICNAME, LEVELS, ATTAVG, ATTSTD"
  modelCols <- "CLASS_ID, FAC_ID, BETA, STD_ERROR, TEST, P_VALUE, DF"
  facdicCols <- "FAC_ID, FAC_EXPRESSION"
  ppmatCols <- "FAC_ID, GLM_ID, LEVEL, POWER"
  residsCols <- "ID, RAW, PEARSON, DEVIANCE"
  
  
  if(idaIsDb2z()) {
    
    exportModelTable <- idaGetValidTableName(prefix = "IDAR_MODEL_TABLE_")
    
    tryCatch({	
      res <- callSP("EXPORT_MODEL_TO_TABLE", model=model, outtable=exportModelTable)
      
      dictQuery <- paste('SELECT ', dictCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Dictionary\'',sep="")
      dict.out <- idaQuery(dictQuery)
      
      modelQuery <- paste('SELECT ', modelCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Model\'',sep="")
      model.out <- idaQuery(modelQuery)
      
      facdicQuery <- paste('SELECT ', facdicCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'FACDic\'',sep="")
      facdic.out <- idaQuery(facdicQuery)
      
      ppmatQuery <- paste('SELECT ', ppmatCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'PPMatrix\'',sep="")
      ppmat.out <- idaQuery(ppmatQuery)
      
      residsQuery <- paste('SELECT ', residsCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Residuals\'',sep="")
      resids.out <-  idaQuery(residsQuery)
    }, error = function(e) {
      # in case of error, let user know what happend
      stop(e)
    }, finally = {
      idaDeleteTable(exportModelTable)
    })
  } else {
    dictQuery <- paste('SELECT ', dictCols, ' FROM "', modelSchema,'"."',modelName,'_DICTIONARY"',sep="")
    dict.out <- idaQuery(dictQuery)
    
    modelQuery <- paste('SELECT ', modelCols, ' FROM "', modelSchema,'"."',modelName,'_MODEL"',sep="")
    model.out <- idaQuery(modelQuery)
    
    facdicQuery <- paste('SELECT ', facdicCols, ' FROM "', modelSchema,'"."',modelName,'_FACDIC"',sep="")
    facdic.out <- idaQuery(facdicQuery)
    
    ppmatQuery <- paste('SELECT ', ppmatCols, ' FROM "', modelSchema,'"."',modelName,'_PPMATRIX"',sep="")
    ppmat.out <- idaQuery(ppmatQuery)
    
    # residsQuery <-  paste('SELECT ', residsCols, ' FROM "', modelSchema,'"."',modelName,'_RESIDUALS"',sep="")
    # resids.out <-  idaQuery(residsQuery)
    resids.out <- NULL
  }
  
  # get coefficients (=beta)
  coefficients <- model.out$BETA
  
  # get IDs from FACDIC and match proper names to coefficients
  names(coefficients) <- facdic.out$FAC_EXPRESSION[match(model.out$FAC_ID,facdic.out$FAC_ID)] 
  
  # get residuals as ida.data.frame (select only ID and RAW)
  residuals <- resids.out[,c(1,2)]
  
  # prepare summarized output by stored procedure PRINT_GLM
  summary <- callSP("PRINT_GLM", retvalcolumn="RETVAL", model=model)
  #	summary <- "summary of the GLM model"
  ##return glmOut list
  glmOut <- list(	modelname=model,
                  call=call,
                  id=id,
                  form=form,
                  data=data,
                  coefficients=coefficients,		
                  dict=dict.out,
                  model=model.out,
                  facdic=facdic.out,
                  ppmat=ppmat.out,
                  residuals=residuals,
                  summary=summary)
  
  class(glmOut) <- c("idaGlm","glm")
  return(glmOut)
}

#------------------------------------------------------------------------------

idaGlm.format.raw <- function(model, raw.resid) {
  
  xx <- parseTableName(model);
  modelName <- xx$table
  modelSchema <- xx$schema
  
  # results are converted
  dictTable <- paste("NZA_META_", model, "_DICTIONARY",sep="")
  modelTable <- paste("NZA.NZA_META_",model,"_MODEL",sep="")
  facdicTable <- paste("NZA.NZA_META_", model, "_FACDIC",sep="")
  ppmatTable <- paste("NZA.NZA_META_",model,"_PPMATRIX",sep="")
  residsTable <- paste("NZA.NZA_META_",model,"_RESIDUALS",sep="")
  
  
  dictCols <- "ATTNUM, ATTNAME, ATTTYPE, DICNAME, LEVELS, ATTAVG, ATTSTD"
  modelCols <- "CLASS_ID, FAC_ID, BETA, STD_ERROR, TEST, P_VALUE, DF"
  facdicCols <- "FAC_ID, FAC_EXPRESSION"
  ppmatCols <- "FAC_ID, GLM_ID, LEVEL, POWER"
  residsCols <- "ID, RAW, PEARSON, DEVIANCE"
  
  
  if(idaIsDb2z()) {
    
    exportModelTable <- idaGetValidTableName(prefix = "IDAR_MODEL_TABLE_")
    
    tryCatch({	
      res <- callSP("EXPORT_MODEL_TO_TABLE", model=model, outtable=exportModelTable)
      
      dictQuery <- paste('SELECT ', dictCols, ' FROM ', exportModelTable,' where MODELUSAGE= \' Dictionary\'',sep="")
      dict.out <- idaQuery(dictQuery)
      
      modelQuery <- paste('SELECT ', modelCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Model\'',sep="")
      model.out <- idaQuery(modelQuery)
      
      facdicQuery <- paste('SELECT ', facdicCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'FACDic\'',sep="")
      facdic.out <- idaQuery(facdicQuery)
      
      ppmatQuery <- paste('SELECT ', ppmatCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'PPMatrix\'',sep="")
      ppmat.out <- idaQuery(ppmatQuery)
        
      if (raw.resid) {
          residsQuery <- paste('SELECT ', residsCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Residuals\'',sep="")
          resids.out <-  idaQuery(residsQuery)
      } else  {
          resids.out <- NULL
      }
    }, error = function(e) {
      # in case of error, let user know what happend
      stop(e)
    }, finally = {
      idaDeleteTable(exportModelTable)
    })
  } else {
    dictQuery <- paste('SELECT ', dictCols, ' FROM "', modelSchema,'"."',modelName,'_DICTIONARY"',sep="")
    dict.out <- idaQuery(dictQuery)
    
    modelQuery <- paste('SELECT ', modelCols, ' FROM "', modelSchema,'"."',modelName,'_MODEL"',sep="")
    model.out <- idaQuery(modelQuery)
    
    facdicQuery <- paste('SELECT ', facdicCols, ' FROM "', modelSchema,'"."',modelName,'_FACDIC"',sep="")
    facdic.out <- idaQuery(facdicQuery)
    
    ppmatQuery <- paste('SELECT ', ppmatCols, ' FROM "', modelSchema,'"."',modelName,'_PPMATRIX"',sep="")
    ppmat.out <- idaQuery(ppmatQuery)
    
    # if (raw.resid) {
    #   residsQuery <-  paste('SELECT ', residsCols, ' FROM "', modelSchema,'"."',modelName,'_RESIDUALS"',sep="")
    #   resids.out <-  idaQuery(residsQuery)
    # } else  {
    #   resids.out <- NULL
    # }
    resids.out <- NULL
  }
  
  
  
  # create raw-output as list
  raw <- list(dictionary=dict.out, model=model.out, facdic=facdic.out, ppmatrix=ppmat.out, residuals=resids.out, modelname=model)
  return(raw)
}

idaRetrieveGlmModel <- function(model)  {
  idaGlm.format.glm(model, NULL, NULL, NULL, NULL)
}

# -----------------------------------------------------------------------------

predict.idaGlm <- function(object, newdata, id, outtable = NULL, ...) {
  if (!idaIsDb2z() && !idaIsRegularTable(newdata@table) && !is.null(newdata@where)) {
    stop(simpleError("Only IDA data frames on regular tables without where-conditions are supported for predict.idaGlm."))
  }

  if (is.null(outtable) || outtable == "") {
    outtable <- idaGetValidTableName(paste("PREDICT_",sep=""))
  }
  outtable <- idaGetValidTableName(paste("PREDICT_",sep=""))
  
  colu = newdata@cols
  if (!(id %in% colu))
    stop(simpleError(paste("Id variable is not avaliable in ida.data.frame:", id)))
  
  id  <- dQuoteSimple(id)
  
  tmpView <- idaCreateView(newdata)
  if (idaIsDb2z()) {
    inTableView <- tmpView
  } else {
    inTableView <- newdata@table
  }
  tryCatch({
    callSP("PREDICT_GLM ",
           model=object$modelname,
           intable=inTableView,
           id=id,
           outtable=outtable, 
           ... )
  }, error = function(e) {
    # in case of error, drop view and let user know, what happend
    stop(e)
  }, finally = {
    # drop view
    idaDropView(tmpView)
  }
  )
  
  object.pred <- ida.data.frame(outtable)
  return(object.pred)
}

# -----------------------------------------------------------------------------

print.idaGlm <- function(x, ...) {
  cat("\nModel Name\n")
  cat(x$modelname,"\n")
  cat("\nCall:")
  print(x$call)
  cat("\n\nCoefficients:\n")
  print(x$coef)
  
  trim <- function(s) {
    return(gsub("(^[\n\t ]+|[\n\t ]+$)", "", s))
  }
  
  getGlmDataIndex <- function(l, type) {
    return(match(type, l[[1]]))
  }
  
  outputList <- strsplit(gsub("[\n\t]", "", x$summary[[1]]), "\\|")
  outputList <- lapply(outputList, trim)
  
  cat("\n\nResiduals Summary:\n")
  cat("Pearson:\t\t")
  pIndex <- getGlmDataIndex(outputList, "Pearson")
  cat("RSS:", outputList[[1]][pIndex+1], "\tdf:", outputList[[1]][pIndex+2], "\tp-value:", outputList[[1]][pIndex+3])
  cat("\nDeviance:\t\t")
  dIndex <- getGlmDataIndex(outputList, "Deviance")
  cat("RSS:", outputList[[1]][dIndex+1], "\tdf:", outputList[[1]][dIndex+2], "\tp-value:", outputList[[1]][dIndex+3], "\n")
  invisible(x)
}

idaGlm.format.summary <- function(model) {
  summary <- model$summary
  coeffStartPos <- regexpr('|', summary, fixed=TRUE)
  coeffEndPos <- (regexpr('Residuals Summary:', summary, fixed=TRUE) -1)
  coeffTitle <- idaTrim(substr(summary, 1, coeffStartPos -1 ))
  coeffStr <- substr(summary, coeffStartPos +1, coeffEndPos)
  coeffDf <- idaDataFrameFromResultStr(coeffStr, rowSplitStr='| |',headerRow=1, skippedDataRows=0)
  residSummary <- substr(summary, coeffEndPos+1, nchar(summary))
  residStartPos <- regexpr('|', residSummary, fixed=TRUE)
  residStr <- substr(residSummary, residStartPos +1, nchar(residSummary))
  residTitle <- idaTrim(substr(residSummary,1, residStartPos -1))
  residDf <- idaDataFrameFromResultStr(residStr, rowSplitStr='| |', headerRow=1, skippedDataRows=0)
  return(list(coeffTitle, coeffDf, residTitle, residDf))
}


# -----------------------------------------------------------------------------

summary.idaGlm <- function(x) {
  cat("\nCall:")
  print(x$call)
  cat("\n\n")
  cat(x$summary[[1]])
  invisible(x$summary)
}

# -----------------------------------------------------------------------------

residuals.idaGlm <- function(x) {
  residuals.frame <- as.data.frame(x$residuals)
  id <- gsub("\\\"", "", x$id)
  res <- residuals.frame[order(residuals.frame[[id]]),][,'RAW']
  names(res) <- 1:length(res)
  return(res)
}

# -----------------------------------------------------------------------------

fitted.idaGlm <- function(x) {
  residuals.frame <- as.data.frame(x$residuals)
  id <- gsub("\\\"", "", x$id)
  residuals <- residuals.frame[order(residuals.frame[[id]]),][,'RAW']
  ntab1 = idaParseRFormula(x$form, x$data)
  data <- as.data.frame(x$data)
  yActual <- data[order(data[[id]]),][,which(names(data) %in% c(x$id,ntab1$varlist[1]))]
  res <- (yActual - residuals)
  names(res) <- 1:length(res)
  res
}

dQuoteSimple <- function(str) {
  paste0('"',str,'"')
}
