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

idaDivCluster <- function(
    data,
    id,
    distance="euclidean",
    maxiter=5,
    minsplit=5,
    maxdepth=3,
    randseed=12345,
    outtable=NULL,
    modelname=NULL) {
  
  if(!idaIsDb2z()) {
    stop("idaDivCluster is only available for DB2 for z/OS.")
  }
  if(!idaCheckProcedure("DIVCLUSTER","idaDivCluster",F)) {
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
    model <- idaGetValidModelName('DIVCLUSTER_')
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
  if(is.null(outtable)) {
    tmpOuttable <- idaGetValidTableName(prefix = "IDAR_OUTTABLE_")
    outtable <- tmpOuttable
  }
  
  tryCatch({
        res <- callSP("DIVCLUSTER",
                      model=model,
                      intable=tmpView,
                      id=id,
                      distance=distance,
                      maxiter=maxiter,
                      minsplit=minsplit,
                      maxdepth=maxdepth,
                      randseed=randseed,
                      outtable=outtable)
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
  
  result <- idaRetrieveDivClusterModel(model);
  
  return(result)
}

#------------------------------------------------------------------------------

idaRetrieveDivClusterModel <- function(modelName) {
  
  xx <- parseTableName(modelName);
  model <- xx$table
  modelSchema <- xx$schema
  # Column Properties
  colPropsCols <- "'\"'|| COLNAME || '\"' as COLNAME, COLDATATYPE, COLROLE, COLTYPE, COLWEIGHT, IDCOL"
  # Model
  modelCols <- "CLUSTER_ID, CLUSTER_SIZE, CLUSTER_WITHINSS" # + incolumns
  # Dictionary
  dictCols <- "ATTNUM, ATTNAME, ATTTYPE, DICNAME, LEVELS, ATTAVG, ATTSTD"
  # "COLUMNNAME, DATATYPE, OPTYPE, USAGETYPE, COLUMNWEIGHT, COMPAREFUNCTION, IMPORTANCE, OUTLIERTREATMENT, LOWERLIMIT, UPPERLIMIT, CLOSURE, STATISTICSTYPE"

  # modelColList <- "MODELCLASS,COMPARISONTYPE, COMPARISONMEASURE, NUMCLUSTERS "
  # clustersColList <- "CLUSTERID, NAME, DESCRIPTION, SIZE, RELSIZE, WITHINSS"
  # columnStatsColList <- "CLUSTERID, COLUMNNAME, CARDINALITY, MODE, MINIMUM,  MAXIMUM, MEAN, VARIANCE, VALIDFREQ, MISSINGFREQ, INVALIDFREQ, IMPORTANCE"; 
  
  if(idaIsDb2z()) {
     
    exportModelTable <- idaGetValidTableName(prefix = "IDAR_MODEL_TABLE_")
    
    tryCatch({
        res <- callSP("EXPORT_MODEL_TO_TABLE", model=modelName, outtable=exportModelTable)

        colPropsQuery <-  paste('SELECT ', colPropsCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Column Properties\'',sep="")
        colProps.out <- idaQuery(colPropsQuery)

        dictQuery <-  paste('SELECT ', dictCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Dictionary\'',sep="")
        dict.out <- idaQuery(dictQuery)

        contCols <- colProps.out[colProps.out$COLROLE=='input' & colProps.out$COLTYPE=='cont', 'COLNAME']
        catCols <- colProps.out[colProps.out$COLROLE=='input' & colProps.out$COLTYPE=='nom', 'COLNAME']

        modelCols <- paste(modelCols, ", ", paste(contCols, collapse=", "), ", ", paste(catCols, collapse=", "), sep = "")

        modelQuery <-  paste('SELECT ', modelCols, ' FROM ', exportModelTable,' where MODELUSAGE= \'Model\' order by abs(CLUSTER_ID)',sep="")
        model.out <- idaQuery(modelQuery)

      }, error = function(e) {
        # in case of error, let user know what happend
        stop(e)
      }, finally = {
        idaDeleteTable(exportModelTable)
      }
    )
  }

  distance <- NULL
  
  sizes <- model.out[,"CLUSTER_SIZE"]
  withinss <- model.out[,"CLUSTER_WITHINSS"]
  
  cluster <- NULL;
  
  tmp = list(
      cluster=cluster,
      centers=model.out, 
      withinss=withinss,
      size=sizes,
      distance=distance,
      model=modelName
  )
  
  class(tmp) = c("idaDivCluster", "divCluster")
  return(tmp)
}


# Taken from print.kmeans
print.idaDivCluster <- function (x, ...) {
  cat("Two step clustering with ", length(x$size), " clusters of sizes ", paste(x$size, collapse = ", "), "\n", sep = "")
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

predict.idaDivCluster <- function(object, newdata, id,...) {
  
  newData <- newdata
  outtable <- idaGetValidTableName(paste("PREDICT_",sep=""))
  
  colu = newData@cols
  if (!(id %in% colu))
    stop(simpleError(paste("Id variable is not available in ida.data.frame:", id)))
  
  id  <- paste('\"',id,'\"',sep="")
  tmpView <- idaCreateView(newData)
  
  tryCatch({	
      callSP("PREDICT_DIVCLUSTER",
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
