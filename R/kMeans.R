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

idaKMeans <- function(
  data,
  id,
  k=3,
  maxiter=5,
  distance="euclidean",
  outtable=NULL,
  randseed=12345,
  statistics=NULL,
  modelname=NULL) {
  
  if(!idaCheckProcedure("KMEANS","idaKMeans",F)) {
    stop("Function not available.")
  }
  
  modelName <- modelname;
  
  colu = data@cols
  if (!(id %in% colu))
    stop(simpleError(paste("Id variable is not available in ida.data.frame:", id)))
  id.no.quotes <- id
  id  <- paste('"',id,'"',sep='');
  
  model <- modelName;
  
  if (is.null(model)) {
    model <- idaGetValidModelName('KMEANS_')
  } else {
    
    if(grepl(" ",model)) {
      stop("Space in model name not allowed.")
    }
    
    xx <- parseTableName(modelName);
    if (idaIsDb2z()) {
      model <- paste('"',xx$table,'"',sep=''); 	
    } else {
      model <- paste('"',xx$schema,'"."',xx$table,'"',sep=''); 	
    }
    
  }
  
  if (!is.null(outtable)&&idaExistTable(outtable)) {
    stop("Outtable name already exists.")
  }
  
  tmpView <- idaCreateView(data)
  
  tmpOuttable <- NULL
  if(idaIsDb2z() && is.null(outtable)) {
    tmpOuttable <- idaGetValidTableName(prefix = "IDAR_OUTTABLE_")
    outtable <- tmpOuttable
  }
  
  tryCatch({	
    res <- callSP("KMEANS",
                  model=model,
                  intable=tmpView,
                  k=k,
                  maxiter=maxiter,
                  outtable=outtable,
                  distance=distance,
                  id=id,
                  randseed=randseed,
                  statistics=statistics)
    actual.k <- as.numeric(res[1,1])
  }, error = function(e) {
    # in case of error, let user know what happend
    stop(e)
  }, finally = {
    # drop view
    idaDropView(tmpView)
    if (!is.null(tmpOuttable)) {
      idaDeleteTable(tmpOuttable)
    }
  }
  )
  
  result <- idaRetrieveKMeansModel(model);
  
  return(result)
}

#------------------------------------------------------------------------------

idaRetrieveKMeansModel <- function(modelName) {
  
  xx <- parseTableName(modelName);
  model <- xx$table
  modelSchema <- xx$schema
  columnsColList <- "COLUMNNAME, DATATYPE, OPTYPE, USAGETYPE, COLUMNWEIGHT, AUTOTRANSFORM, TRANSFORMEDCOLUMN, COMPAREFUNCTION, IMPORTANCE, OUTLIERTREATMENT, LOWERLIMIT, UPPERLIMIT, CLOSURE, STATISTICSTYPE"
  modelColList <- "MODELCLASS,COMPARISONTYPE, COMPARISONMEASURE, NUMCLUSTERS "
  clustersColList <- "CLUSTERID, NAME, DESCRIPTION, SIZE, RELSIZE, WITHINSS"
  columnStatsColList <- "CLUSTERID, COLUMNNAME, CARDINALITY, MODE, MINIMUM,  MAXIMUM, MEAN, VARIANCE, VALIDFREQ, MISSINGFREQ, INVALIDFREQ, IMPORTANCE"; 
  
  if(idaIsDb2z()) {
    
    exportModelTable <- idaGetValidTableName(prefix = "IDAR_MODEL_TABLE_")
    
    tryCatch({	
      res <- callSP("EXPORT_MODEL_TO_TABLE", model=modelName, outtable=exportModelTable)
      model1 <- paste('SELECT ', columnsColList, ' FROM ', exportModelTable,' where MODELUSAGE= \'Columns\'',sep="")
      columns <- idaQuery(model1)
      
      model2 <- paste('SELECT ', modelColList, ' FROM ', exportModelTable,' where MODELUSAGE= \'Model\'',sep="")
      modelMain <- idaQuery(model2)
      
      model3 <- paste('SELECT ', clustersColList, ' FROM ', exportModelTable,' where MODELUSAGE= \'Clusters\'',sep="")
      kmOutStat <- idaQuery(model3)
      
      model4 <- paste('SELECT ', columnStatsColList, ' FROM ', exportModelTable,' where MODELUSAGE= \'Column Statistics\'',sep="")
      kols <- idaQuery(model4)
    }, error = function(e) {
      # in case of error, let user know what happend
      stop(e)
    }, finally = {
      idaDeleteTable(exportModelTable)
    }
    )
    
  } else {
    model1 <- paste('SELECT * FROM "',modelSchema,'"."',model,'_COLUMNS"',sep="")
    columns <- idaQuery(model1)
    
    model2 <- paste('SELECT * FROM "',modelSchema,'"."',model,'_MODEL"',sep="")
    modelMain <- idaQuery(model2)
    
    model3 <- paste('SELECT * FROM "',modelSchema,'"."',model,'_CLUSTERS"',sep="")
    kmOutStat <- idaQuery(model3)
    
    model4 <- paste('SELECT * FROM "',modelSchema,'"."',model,'_COLUMN_STATISTICS"',sep="")
    kols <- idaQuery(model4)
  }
  
  
  contCols <- columns[columns$USAGETYPE=='active'&columns$OPTYPE=='continuous','COLUMNNAME']
  catCols <- columns[columns$USAGETYPE=='active'&columns$OPTYPE=='categorical','COLUMNNAME']
  
  k <- modelMain[1,4]
  distance <- modelMain[1,3]	
  
  kols$CARDINALITY <- as.numeric(kols$CARDINALITY)
  kols$VALIDFREQ <- as.numeric(kols$VALIDFREQ)
  kols$MISSINGFRQ <- as.numeric(kols$MISSINGFREQ)
  kols$INVALIDFREQ <- as.numeric(kols$INVALIDFREQ)
  
  kols <- kols[kols[,1]>0,]
  kols <- kols[order(kols[,1], kols[,2]),]
  
  cents  <- data.frame(CLUSTERID=1:k)
  
  if(length(contCols)>0)
    for(i in 1:length(contCols)) {
      cents[contCols[i]]<-kols[kols$COLUMNNAME==contCols[i],7]
    }
  
  if(length(catCols)>0)
    for(i in 1:length(catCols)) {
      cents[catCols[i]]<-kols[kols$COLUMNNAME==catCols[i],4]
    }
  
  kmOutStat <- kmOutStat[order(kmOutStat[,1]),]
  
  cluster <- NULL;
  
  tmp = list(
    cluster=cluster,
    centers=cents, 
    withinss=kmOutStat$WITHINSS,
    size=kmOutStat$SIZE,
    distance=distance,
    model=model
  )
  
  class(tmp) = c("idaKMeans", "kmeans")
  return(tmp)
}


# Taken from print.kmeans
print.idaKMeans <- function (x, ...) {
  cat("KMeans clustering with ", length(x$size), " clusters of sizes ", paste(x$size, collapse = ", "), "\n", sep = "")
  cat("\nCluster means:\n")
  print(x$centers, ...)
  
  if(!is.null(x$cluster)) {
    cat("\nClustering vector:\n")
    print(x$cluster, ...)
  }
  
  cat("\nWithin cluster sum of squares by cluster:\n")
  print(x$withinss, ...)
  # Sum of square statistics from original print.kmeans left out ....
  cat("\nAvailable components:\n")
  print(names(x))
  invisible(x)
}

predict.idaKMeans <- function(object, newdata, id,...) {
  
  newData <- newdata
  outtable <- idaGetValidTableName(paste("PREDICT_",sep=""))
  
  colu = newData@cols
  if (!(id %in% colu))
    stop(simpleError(paste("Id variable is not available in ida.data.frame:", id)))
  
  id  <- paste('\"',id,'\"',sep="")
  tmpView <- idaCreateView(newData)
  
  tryCatch({	
    callSP("PREDICT_KMEANS",
           model=object$model,
           intable=tmpView,
           id=id,
           outtable=outtable,
           ...)
  }, error = function(e) {
    # in case of error, let user know what happend
    stop(e)
  }, finally = {
    # drop view
    idaDropView(tmpView)
  }
  )
  
  object.pred <- ida.data.frame(outtable)
  return(object.pred)
}
