#
# Copyright (c) 2013, 2018, IBM Corp. All rights reserved.
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


idaTApply <- function (X, INDEX, FUN = NULL, output.name=NULL, output.signature=NULL,
                  clear.existing=FALSE, debugger.mode=FALSE,
                  num.tasks = 0, working.dir=NULL, apply.function="default", ...) {

  startTime <- Sys.time()

  if(idaIsDb2z()) {
    stop("This function is not supported for DB2 for z/OS connections.")
  }

  if (!is.ida.data.frame(X)) {
    stop("This function can only be run for ida.data.frame.")
  }

  # a function has to be supplied
  if (is.null(FUN) || !is.function(FUN)) {
    stop("FUN has to be a function.")
  }
  environment(FUN) <- globalenv();

  if (!is.logical(clear.existing)) {
    stop("The value of the clear.existing parameter has to be either T(RUE) or F(ALSE).")
  }

  outputTableName <- output.name
  renameTo <- NULL
  if (is.null(output.name)) {
    stop("The parameter output.name has to be specified with a non-NULL value.")
  } else if (idaExistTable(output.name)) {
    if (isTRUE(clear.existing)) {
      # if clear.existing == TRUE and the outputtable exists
      # we write the results first in a new table
      # that will be renamed to output.name at the end
      renameTo = outputTableName
      x <- parseTableName(output.name)
      outputTableName <- idaGetValidTableName(prefix = paste(x$schema, ".TAPPLY_", sep=""))
    } else {
      stop(paste( "The output table with the name ", output.name,
                  " exists already and the clear.existing flag is set to FALSE.", sep=""))
    }
  }

  # Db2 column definitions for the output table have to be supplied
  if (is.null(output.signature)) {
    stop("The parameter output.signature has to be specified.")
  } else if (!is.list(output.signature) || length(output.signature)==0) {
    stop("The parameter output.signature has to be a non-empty list of the form (field.name=Db2.DATA.TYPE, ...).")
  }

  # check if the data types in output.signature are supported by Db2
  db2DataTypes <- c("SMALLINT", "INTEGER", "BIGINT", "FLOAT", "DOUBLE", "REAL", "DECFLOAT", "DECIMAL", "NUMERIC",
                     "VARCHAR", "CHAR", "DATE")
  db2DataTypesString <- paste(db2DataTypes, collapse=", ")

  output.columns <-
    paste(  sapply( names(output.signature),
                    function(x){
                        dt <- toupper(eval(parse(text=paste("output.signature$",x))));
                        pos<-regexpr('(', dt, fixed=T)[1];
                        if (pos > 0) dt2 <- substr(dt, 1, (pos-1)) else  dt2 <- dt;
                        if(!dt2%in% db2DataTypes)
                          stop(paste( dt2, "in output.signature is not one of the supported Db2 data types: ",
                                      db2DataTypesString, ".",sep=" "));
                        paste("\"", x, "\" ", dt, sep="")}),
            collapse=", ")

  if (!is.null(working.dir) && !dir.exists(working.dir)) {
    stop(paste("The working directory ", working.dir, "does not exist.", sep=""))
  }

  # check apply.function parameter which specfies the apply function used for distributing the workload
  # the default is mclapply on a single node and spark.lapply on a multi node environment
  applyFunctions <- c("spark.lapply", "mclapply", "default")
  if (is.null(apply.function)) {
    apply.function <- "default"
  }
  if (!(apply.function %in% applyFunctions)) {
    stop(paste("The value ", apply.function, " of the apply.function parameter is not one of the supported values: ",
              paste(applyFunctions, collapse=", "),
              ".",
    sep=""))
  }

  # Determine the grouping column. At the end of the following block, the following conditions apply:
  # - INDEX contains the name of the index column.
  # - idx contains the column number of the index column.
  cols <- X@cols
  if (is.integer(INDEX) ||
     (is.double(INDEX) && floor(INDEX) == INDEX)) {
    idx = as.integer(INDEX)
    if (idx < 1) {
      stop("INDEX has to be greater than 0.")
    }
    if (idx > length(cols)) {
      stop("Input data frame has only ", length(cols), " columns.")
    }
    INDEX = cols[idx]
  } else if (!is.character(INDEX)) {
    stop("INDEX value cannot be determined with value of type ", typeof(INDEX), ".")
  } else if (!(INDEX %in% cols)) {
    stop("Input data frame does not have column named ", INDEX, ".")
  } else {
    idx <- which(cols %in% INDEX)
  }

  # check internal parameters

  save.mode = "exttable"
  saveModes <- c("exttable", "db2load", "idaxsource")
  output.schema=NULL
  # default for insert.group.size will be calculated based on num.tasks in idaTApply.lapply
  insert.group.size <- 0
  read.group.size <- 1

  #If functions are passed as arguments, make sure to remove the name space
  args <- list(...)
  args <- lapply(args,
    function(arg) {
      if (is.function(arg)) {
        environment(arg) <- globalenv()
        return(arg)
      } else {
        return(arg)
      }
   })

   # the first two arguments are reserved for the input data frame and the index
  functionArgs <- list(NULL, NULL)

  for (name in names(args)) {
    value <- args[[name]]
    if (name == "read.group.size") {
      # the number of INDEX values for which the data is read through one select-statement
      if (!is.null(value)) {
        if (is.numeric(value) && value > 0) {
          read.group.size=as.integer(value)
        } else {
          stop(paste("The value ", value, " of the internal read.group.size parameter is not a number greater than 0.",
                      sep=""))
        }
      }
    } else if (name == "insert.group.size") {
      # the number of tasks whose insert operations into the output table
      # are performed by one single task
      if (!is.null(value)) {
        if (is.numeric(value) && value >= 0) {
          insert.group.size=as.integer(value)
        } else {
          stop(paste("The value ", value,
                      " of the internal insert.group.size parameter is not a number greater or equal to 0.",
                      sep=""))
        }
      }
    } else if (name == "save.mode") {
      if (!is.null(value)) {
        if (value %in% saveModes) {
          save.mode <- value
        } else {
          stop(paste("The value ", value,
                      " of the internal save.mode parameter is not one of the supported methods for saving the results: ",
                      paste(saveModes, collapse=", "),
                      sep=""))
        }
      }
    } else {
      functionArgs[[name]]<-value
    }
  }

  idaTApply.lapply( X, INDEX, FUN, outputTableName, renameTo, output.signature, output.columns,
                    debugger.mode, num.tasks, read.group.size, insert.group.size, save.mode, working.dir,
                    apply.function, functionArgs)

  resDf <- ida.data.frame(output.name)
  endTime <- Sys.time()
  print(paste("Total execution time of idaTApply:",
              as.numeric(endTime-startTime, units = "secs", digits = 1), " secs", sep=""))
  return(resDf)
}


idaTApply.lapply <- function (X, INDEX, FUN, output.name, rename.to, output.signature, output.columns,
                              debugger.mode, num.tasks, read.group.size, insert.group.size, save.mode, working.dir,
                              apply.function, functionArgs) {
  startTimeFunction <- Sys.time()
  # determine working directory
  # extbl_location contains the allowed paths for external tables
  extblLocationValue <-
    idaQuery("select trim(VALUE) as VALUE from SYSIBMADM.DBCFG ",
              "where NAME = 'extbl_location' and DBPARTITIONNUM=0")$VALUE

  homeDir <- Sys.getenv("HOME")
  l <- nchar(homeDir)
  if (substr(homeDir,l,l) == "/") {
    homeDir<-substr(homeDir, 1,l-1)
  }

  print_prefix <- "idaTApply - "
  INDEXwithQuotes <- paste('\"',INDEX,'\"',sep="")
  numFunArgs <- length(formals(FUN))

  # the maximum number of parallel Db2 insert tasks/threads for writing the results into the output table;
  # it is used for save.mode=="exttable" to determine the value for insert.group.size
  # if it has not been specified or has a value less than 1
  max_parallel_insert_tasks <- 80

  if (identical(extblLocationValue, character(0))) {
    if (save.mode=="exttable") {
      stop("For save.mode exttable the Db2 configuration parameter EXTBL_LOCATION needs to be defined.")
    }
    if(is.null(working.dir)) {
      # default is home directory when results are saved through DB2 load or Spark data frames
      working.dir <- homeDir
    }
  } else {
    extblLocations <- strsplit(extblLocationValue, ";", fixed=TRUE)[[1]]
    exttblLocations <-
      sapply( extblLocations,
              function(path){l <- nchar(path); if (substr(path,l,l) == "/") substr(path, 1,l-1) else path;})

    if (is.null(working.dir)) {
      if ( TRUE %in% sapply(extblLocations, function(path) {substr(homeDir, 1, nchar(path)) == path}) ) {
        # use home directory as working directory if it is below an external table directory
        # to be sure to be able to write into that directory
        working.dir <- homeDir
      } else {
        working.dir <- extblLocations[1]
      }
    # check if working.dir contains a path in extblLocations
    } else if ( ! TRUE %in% sapply(extblLocations, function(path) {substr(working.dir, 1, nchar(path)) == path})) {
      stop(paste("The value of the save.mode parameter is exttable, but the working directory ", working.dir,
        " is not a subdirectory of one of the Db2 external table directories: ", extblLocationValue, ".", sep=""))
    } else {
      l <- nchar(working.dir)
      if (substr( working.dir, nchar( working.dir),nchar( working.dir)) =="/") {
        working.dir <- substr(working.dir, 1,l-1)
      }
    }
  }

  idaTApplyDir <- paste("/idatapply_dir_", format(Sys.time(),'%y%m%d_%H%M%S'), "/", sep="")
  dataDir <- paste(working.dir, idaTApplyDir, sep="")


  # determine the number of nodes
  db2instHome <- system(". ~/.bashrc ; echo ~${DB2INSTANCE}", intern=T)
  numNodes <- 1
  # mcapply does not support multi-nodes configurations
  if (apply.function != "mclapply") {
    numNodes <- as.integer(system(paste("cat ", db2instHome,
                          "/sqllib/db2nodes.cfg | awk '{print $2}' | sort -u | wc -l",
                          sep=""), intern=T))
  }
  numCores <- detectCores()
  if (is.null(numCores)) numCores <- 1

  if (is.null(num.tasks) || num.tasks <= 0) {
    # default for number of tasks is half of the total number of cores
    num.tasks  <- as.integer(max(numCores * numNodes/2,1))
  } else if (num.tasks > as.integer(numCores * numNodes)) {
    stop(paste("The value of the parameter num.tasks (", num.tasks, ") exceeds the number of available cores (",
                as.integer(numCores * numNodes),
                "). The execution is stopped to prevent the system to be overloaded.",
                sep=""))
  }
  if (insert.group.size < 1) {
    insert.group.size <- ceiling(num.tasks/max_parallel_insert_tasks)
  }
  print(paste(print_prefix, " execution context: db2inst-home: ", db2instHome, ", numNodes: ", numNodes,
              ", numCores: ", numCores, ", num.tasks: ", num.tasks ,", read.group.size: ", read.group.size,
              ", insert.group.size: ", insert.group.size, ", data directory: ", dataDir,
              ", save.mode: ", save.mode, ", apply.function: ", apply.function, sep=""))

  useSparkConditions <- c(apply.function =="spark.lapply", numNodes > 1 , save.mode == "idaxsource")
  if (any(useSparkConditions)) {
    if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
      sparkHome ="/opt/ibm/dashdb_spark/spark"
      if (dir.exists(sparkHome)) {
        Sys.setenv(SPARK_HOME = sparkHome)
      } else {
        stop(paste0("The directory of the default location for the Spark installation on Db2 Warehouse \"",
                    sparkHome, "\" does not exist. ",
                    "Please set the \"SPARK_HOME\" environment variable to the path of the installation location of Spark."))
      }
    }

    # check if SparkR package has been installed
    # SparkR can be installed under the standard R locations as well
    sparkLibPaths <- c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib"), base::.libPaths())
    if (!requireNamespace("SparkR",
                          lib.loc = sparkLibPaths,
                          quietly = TRUE)) {
      stop(paste0("The SparkR package is needed because ",
                  paste(c("the value of the parameter apply.function is \"spark.lapply\"",
                          "idaTApply is executed in a multi-node environment",
                          "the value of the parameter save.mode is \"idaxsource\"")[useSparkConditions],
                        collapse=" and "),
                  ". But it has not been installed in none of the following directories: ",
                  paste(sparkLibPaths, collapse=", "),
                  "."))
    }

    sparkCores <- as.integer(numCores*numNodes)
    if (num.tasks < sparkCores) {
      sparkCores <- as.integer(num.tasks)
    }
    sparkExecCores <- as.integer(ceiling(num.tasks/numNodes))

    print(paste(print_prefix, " Spark home directory: ", Sys.getenv("SPARK_HOME"), sep=""))
    print(paste(print_prefix, " Spark configuration: sparkCores: ", sparkCores, ", sparkExecCores: ", sparkExecCores, sep=""))

    c <- SparkR::sparkR.session(enableHiveSupport = FALSE,
                                spark.default.parallelism = sparkCores,
                                spark.cores.max = sparkCores,
                                spark.executor.cores = sparkExecCores)
  }

  # compute the list-argument of the Spark lapply function, it contains the list of the lists of INDEX values
  indexes <- idaQuery( "select distinct(\"", INDEX, "\") as INDEX from (", capture.output(X),
                       ") where \"", INDEX, "\" is not null")$INDEX
  numIndexes <- length(indexes)
  quoteChar = ""
  sqlQuoteChar = ""
  if (numIndexes == 0) {
    stop(paste( "The index column ", INDEX,
                " of the input data frame contains no non-null values. The execution is stopped.",
                sep=""))
  } else if (is.character(indexes[1]) ) {
    quoteChar = "\""
    sqlQuoteChar="'"
  }
  bucketSize <- ceiling(numIndexes/num.tasks)
  indexReadGroupsList <-list()
  id <- 1
  K <- 1
  L <- bucketSize

  while(K <= numIndexes) {
    bucket <- indexes[K:L]
    bl <- length(bucket)
    nrg <- ceiling(bl/read.group.size)
    readGroups <- lapply(1:nrg, function(rg) {bucket[(read.group.size*(rg-1)+1) : min(bl, read.group.size*rg)]})
    indexReadGroupsList[[id]] <- list(ID=id, READ_GROUPS=readGroups, FUN_ARGS=functionArgs)
    id <- id+1
    K <- K+bucketSize
    L <- min(L+bucketSize,numIndexes)
  }

  maxTask <- id -1
  fileStem <- paste( "tapply", "-", num.tasks, sep="")
  filePath <- paste(dataDir, fileStem, sep="")

  # fun.lapply is the function called by the lapply function that calls the functions FUN
  fun.lapply <- function(indexReadGroups) {
    library(ibmdbR)
    # - - trace helper - -
    timings <- vector()
    timing_ids <- vector()
    timing_start <- 1
    timing_no <- 1
    agg_timings <- vector()
    nb_timings <- vector()

    doneExtension <- ".done"

    traceTime <- function(tracepoint, taskBucket) {
      task_print_prefix <- paste( print_prefix, "task bucket ", taskBucket, " - ", sep="")
      if (tracepoint == "start") {
        timing_start <<- timing_no
      }
      timings[timing_no] <<- Sys.time()
      timing_ids[timing_no] <<- tracepoint
      if (timing_no > timing_start) {
        if (tracepoint == "sum") {
          print("")
          print(paste(task_print_prefix, "Summary:", sep=""))
          i <- timing_start + 1
          while (i < timing_no) {
            if (timing_ids[i] != "") {
              t_init <- if (is.na(agg_timings[ timing_ids[i] ])) 0 else agg_timings[ timing_ids[i] ]
              nb_init <- if (is.na(nb_timings[ timing_ids[i] ])) 0 else nb_timings[ timing_ids[i] ]
              agg_timings[ timing_ids[i] ] <<- t_init + timings[i] - timings[i-1]
              nb_timings[ timing_ids[i] ] <<- nb_init + 1
            }
            i <- i + 1
          }
          for ( i in 1:length(agg_timings)  ) {
            print(paste(task_print_prefix, "Total time spent for ", nb_timings[i], " time(s)", names(agg_timings[i]), ": ", as.numeric(agg_timings[i], units = "secs", digits = 1), " secs"))
          }
          print(paste(task_print_prefix, "Overall elapsed time: ", as.numeric(timings[timing_no] - timings[timing_start], units = "secs", digits = 1), " secs"))
          print("")
        }
      }
      timing_no <<- timing_no + 1
    }

    # - - /trace helper - -
    traceTime("start", indexReadGroups$ID)

    # the file where the results for IDs index read group I are written to
    resultDataFile <- paste (filePath, ".", indexReadGroups$ID,".csv", sep="")

    con <- idaConnect("BLUDB", "", "")
    idaInit(con)

    traceTime("", indexReadGroups$ID)
    # execute the function for each index in indexReadGroups$READ_GROUPS
    for (readGroup in indexReadGroups$READ_GROUPS) {
      # step 1: read records for id=ID into R data frame inDF
      # inDF <-  eval(parse(text=paste("as.data.frame(idf[idf$", INDEX, "==", quoteChar, index, quoteChar, ",])", sep="")))
      readGroupString <- paste("(", paste(readGroup, collapse=paste(sqlQuoteChar, ", ", sqlQuoteChar)), ")", sep=sqlQuoteChar)
      idf <- idaQuery( "select * from (", capture.output(X), ") where ", INDEXwithQuotes, " in ", readGroupString )

      for( index in readGroup) {
        # idf <- X
        # step 1: read records for id=ID into R data frame inDF
        inDF <-  eval(parse(text=paste("idf[idf$", INDEX, "==", quoteChar, index, quoteChar, ",]", sep="")))

        traceTime("readData", indexReadGroups$ID)
        # step 2: execute the function on inDF
        if (numFunArgs==1) {
          resDF <- FUN(inDF)
        } else if (numFunArgs == 2) {
          resDF <- FUN(inDF, INDEX)
        } else {
          args <- indexReadGroups$FUN_ARGS
          args[[1]] <- inDF
          args[[2]] <- INDEX
          resDF <- do.call(FUN, args)
        }
        traceTime("executeFunction", indexReadGroups$ID)

        # step 3: write result dataframe resDF in resultDataFile
        # write.table called with package name utils to avoid "object 'C_writetable' not found" errors
        # when called during the execution of spark.lapply
        utils::write.table( resDF, file=resultDataFile,   append=(index!=indexReadGroups$READ_GROUPS[[1]][1]), sep=",",
                            row.names=FALSE, col.names = FALSE)
        traceTime("writeResults", indexReadGroups$ID)
      }
    }
    system(paste("touch ", resultDataFile, doneExtension, sep=""))

    if (save.mode=="exttable") {
      insertStmt <- paste("insert into ", output.name,
                          " SELECT * FROM EXTERNAL '", resultDataFile,
                          "' ( ", output.columns, ")",
                          " USING (DELIMITER ',')", sep="")
      if (insert.group.size <= 1) {
        idaQuery(insertStmt)
        traceTime("insertIntoResultsTable", indexReadGroups$ID)
      }
      if (insert.group.size > 1 && indexReadGroups$ID%%insert.group.size == 1 && indexReadGroups$ID < maxTask) {
        # remainingResultFiles are the result files of other tasks
        # whose data is inserted into the result table by this task
        remainingResultFiles <-
          sapply((indexReadGroups$ID+1):min((indexReadGroups$ID+insert.group.size-1),maxTask),
                  function(x){paste (filePath, ".", x, ".csv", sep="")})
        sleepTime<-0.1
        while(length(remainingResultFiles) > 0) {
          notLoadedFiles <- c()
          nbNotLoadedFiles <-0
          for(i in 1:length(remainingResultFiles)) {
            resFile<- remainingResultFiles[i]
            if (file.exists(paste(resFile, doneExtension, sep=""))) {
                    sleepTime<-0.1
              system(paste("cat ", resFile, " >> ",  resultDataFile, sep=""))
              traceTime("copyResultFiles", indexReadGroups$ID)
            } else {
              nbNotLoadedFiles  <- nbNotLoadedFiles  +1
              notLoadedFiles[nbNotLoadedFiles] <- resFile
            }
          }
          if (nbNotLoadedFiles == length(remainingResultFiles)) {
            # no remaining result file could be loaded, sleep a bit
            system(paste("sleep ", sleepTime, sep=""))
            sleepTime <- sleepTime*1.1
            traceTime("sleep", indexReadGroups$ID)
          }
          remainingResultFiles <- notLoadedFiles
        }
        idaQuery(insertStmt)
        traceTime("insertIntoResultsTable", indexReadGroups$ID)
      }
    }

    idaClose(con)

    traceTime("sum", indexReadGroups$ID)
  }

  createOutputTable<- function(outputTable, outputColumns) {

    if( idaExistTable(outputTable) ) {
      idaDeleteTable(outputTable)
    }
    idaQuery("CREATE TABLE ", outputTable,
             " (",outputColumns, ")",
             " ORGANIZE BY COLUMN",
             " IN USERSPACE1")
  }


  # save results into result table by using DB2 load
  saveResultsLoad <- function(outputTable, filePath, numTasks) {
   loadFiles <- paste(filePath, ".", 1,".csv", sep="")

    for (I in 2:numTasks) {
      loadFiles <- paste(loadFiles, ", ", filePath, ".", I,".csv", sep="")
    }

    loadQuery <-
      paste("CALL SYSPROC.ADMIN_CMD('LOAD FROM ", loadFiles, " OF DEL METHOD P (1, 2, 3, 4) insert into ", outputTable, "')", sep="")
    idaQuery(loadQuery)

   }

  # save results using an Idax Spark datasource
  saveResultsIdax <- function(outputTable, output.signature, filePath, numTasks, numCores) {
    # concatenate the result data files to a single (output) file
    outputFile <- paste(filePath, ".csv", sep="")
    catStmt <- paste("cat ", filePath, ".", 1,".csv", sep="")

    for (I in 2:numTasks) {
      catStmt <- paste(catStmt, " ", filePath, ".", I,".csv", sep="")
    }
    catStmt <- paste(catStmt, " > ", outputFile, sep="")
    system(catStmt)

    # create a Spark dataframe that points to outputFile
    outputSchemaString <-
        paste("SparkR::structType(",
              paste(sapply(names(output.signature),
                    function(x){  dt <- tolower(eval(parse(text=paste("output.signature$",x))));
                                  pos<-regexpr('(', dt, fixed=T)[1];
                                  if (pos > 0) dt <- substr(dt, 1, (pos-1));
                                  if (dt%in% c("char",  "varchar", "date")) dt <- "string" else if (dt == "float") dt <- "double";
                                  paste("SparkR::structField('", x, "', '", dt, "')", sep="")}),
                    collapse=", "),
              ")",
              sep="")
    print(paste(print_prefix, "used Spark schema for saving results. ", outputSchemaString, sep=""))
    outputSchema <- eval(parse(text=outputSchemaString))
    result_sdf <- SparkR::read.df(outputFile, source="csv", header = "false", inferSchema = "false", schema = outputSchema)

    # write the result_sdf Spark dataframe to the result table outputTable
    SparkR::write.df( result_sdf,
                      outputTable,
                      source="com.ibm.idax.spark.idaxsource",
                      allowAppend=TRUE,
                      mode="append",
                      maxInsertThreadsPerNode=numCores,
                      dbtable=outputTable)
  }

  tryCatch({
      # now start the real processing
      dir.create(dataDir, mode="0777")
      system(paste( "chmod a+w ", dataDir, sep=""))

      createOutputTable(output.name, output.columns)

      startTimeLapply <- Sys.time()
      print(paste(print_prefix, "execution time of preparation for lapply: ",
        as.numeric(startTimeLapply-startTimeFunction, units = "secs", digits = 1), " secs", sep=""))

      # as a default for a single node system we do not need Spark for the parallelization
      # because of the mclapply function of the parallel R-package
      if(apply.function == "mclapply" || (apply.function == "default" && numNodes == 1)) {
        mclapply(indexReadGroupsList, fun.lapply, mc.cores=num.tasks)
      } else {
        SparkR::spark.lapply(indexReadGroupsList, fun.lapply)
      }

      startTimeSaveResults <- Sys.time()
      print(paste(print_prefix,
                  "execution time of lapply",
                  switch(save.mode, exttable=" with saving results through exttable", ""), ": ",
                  as.numeric(startTimeSaveResults-startTimeLapply, units = "secs", digits = 1), " secs", sep=""))

      if (save.mode=="db2load") {
          saveResultsLoad(output.name, filePath, maxTask)
      } else if (save.mode == "idaxsource"){
          saveResultsIdax(output.name, output.signature, filePath, maxTask, numCores)
      }
      endTimeSaveResults <- Sys.time()
      if (save.mode%in% c("db2load", "idaxsource")) {
        print(paste(print_prefix, "execution time for saving results through ", save.mode, ": ",
                    as.numeric(endTimeSaveResults-startTimeSaveResults, units = "secs", digits = 1), " secs", sep=""))
      }
      if (!is.null(rename.to)) {
        idaRenameTable(from=output.name, to=rename.to, drop.to=T)
        endTimeRenameTable <- Sys.time()
        print(paste(print_prefix, "execution time for RENAME_TABLE: ",
                    as.numeric(endTimeRenameTable-endTimeSaveResults, units = "secs", digits = 1), " secs", sep=""))
      }

    },error = function(e) {
      print(e)
      # in case of error, let user know what happened
      tryCatch({
        print(paste(print_prefix, "dropping table ", output.name, " if it exists.", sep=""))
        idaDeleteTable(output.name)
      }, error=function(e){print(e)}
      )
      stop(e)
    }, finally = {
      # remove data directory
      if (!debugger.mode) {
        print(paste(print_prefix, "removing data directory ", dataDir, sep=""))
        unlink(dataDir, recursive=T)
      }
    }
  )
}



