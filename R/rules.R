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

idaArule <- function(data,tid,item,maxlen=5,maxheadlen=1,minsupport=NULL,minconf=0.5,nametable=NULL,namecol=NULL, modelname=NULL) {
  
  isDB2z <- idaIsDb2z()
  newColumnExpr = NULL
  if (isDB2z) {
    spname <- "ARULE"
    # IDAA does not pass the alias of a column to Netezza
    # "?COLUMN?" is the name generated for unnamed columns by Netezza 
    newColumnExpr = '1 as "?COLUMN?"'
  } else {
    spname <-"ASSOCRULES"
  }
  if(!idaCheckProcedure(spname,"idaArule",F)) {
    stop("idaArule is not available for the current connection.")
  }
  
  model <- modelname
  
  if (is.null(model)) {
    model <- idaGetValidModelName('ARULE_')
  } else {
    if(grepl(" ",model)) {
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
  
  # create view from given data argument
  tmpView <- idaCreateView(data, newColumn=newColumnExpr)
  
  tryCatch({		
    # validate given id and item argument
    colu = data@cols
    if (!(tid %in% colu)) {
      stop(simpleError(paste("tid variable is not available in ida.data.frame:", tid)))
    }
    if (!(item %in% colu)) {
      stop(simpleError(paste("item variable is not available in ida.data.frame:", item)))
    }
    
    tid <- paste('\"', tid, '\"', sep="")
    item <- paste('\"', item, '\"', sep="")
    
    # call stored procedure
    if (isDB2z) {
      callSP(spname,
             model=model,
             intable=tmpView,
             by='"?COLUMN?"',
             tid=tid,
             item=item,
             maxsetsize=maxlen,
             support=minsupport*100.0,
             confidence=minconf)
    } else {
      callSP(spname,
             model=model,
             intable=tmpView,
             tid=tid,
             item=item,
             maxlen=maxlen,
             maxheadlen=maxheadlen,
             minsupport=minsupport,
             minconf=minconf,
             nametable=nametable,
             namecol=namecol)
    }
  }, error = function(e) {
    # in case of error, let user know what happened
    stop(e)
  }, finally = {
    # drop view
    idaDropView(tmpView)
  }
  )
  
  tryCatch({		
    result <- idaRetrieveRulesModel(model, nametable=nametable,item=item, namecol=namecol);
  }, error = function(e) {
    # in case of error, drop the model, 
    # suppress errors and warnings to avoid that they overwrite the error of the association rules procedure 
    tryCatch({idaDropModel(model)},warning=function(warn){}, error=function(err){})
    stop(e)
  })
  
  return(result)
}


idaRetrieveRulesModel <- function(modelname, nametable=NULL,item=NULL, namecol=NULL) {
  
  xx <- parseTableName(modelname);
  model <- xx$table
  modelSchema <- xx$schema
  
  
  groupColList <- "GID, GROUP_NAME, NUMBEROFTRANSACTIONS, MAXNUMBEROFITEMSPERTA, AVGNUMBEROFITEMSPERTA, NUMBEROFITEMS, NUMBEROFITEMSETS, NUMBEROFRULES, MINIMUMSUPPORT"  
  itemSetColList <- "SID, GID, ITEM1, ITEM2, ITEM3, ITEM4, ITEM5, SIZE, SUPPORT, LIFT" 
  
  if(idaIsDb2z()) {
    modelname2 <-  paste('"', modelname, '"', sep='')
    exportModelTable <- idaGetValidTableName(prefix = "IDAR_MODEL_TABLE_")
    
    tryCatch({			
      if (is.null(nametable) || is.null(item)|| is.null(namecol)) {
        res <- callSP("EXPORT_MODEL_TO_TABLE", model=modelname, outtable=exportModelTable)
        itemTable <-  paste('SELECT integer(ITEM) as ITEM, ITEM_NAME as ITEMID, ITEM_NAME as ITEMNAME FROM ', 
                            exportModelTable,' where MODELUSAGE= \'Item\' ORDER BY ITEM',sep="")
      } else {
        res <- callSP("EXPORT_MODEL_TO_TABLE", model=modelname, outtable=exportModelTable, nametable=nametable, item=item, itemname=namecol)
        nn <- parseTableName(nametable);
        mm <- parseTableName(exportModelTable);
        itemNameLength <- max(idaScalarQuery(paste("select max(length) from SYSIBM.SYSCOLUMNS where TBNAME = '", mm$table, "' and TBCREATOR = '", mm$schema, "' and NAME in ('ITEM_NAME', 'ITEM_NAME2')", sep="")),
                              32)								
        itemTable <-  paste('SELECT integer(ITEM) as ITEM, ITEM_NAME as ITEMID, nvl(cast(ITEM_NAME2 as VARCHAR(', itemNameLength, ')), cast(ITEM_NAME as VARCHAR(', itemNameLength,'))) as ITEMNAME FROM ' , 
                            exportModelTable,' where MODELUSAGE= \'Item\' ORDER BY ITEM',sep="")
      }
      modelCols <- idaListTableColumns(exportModelTable)
      itemCols <- modelCols[grep("ITEM", modelCols)]
      itemCols <- itemCols[grep("_NAME", itemCols, invert=TRUE)]		
      
      itemSetColList <- paste( "SID, GID, ", paste(itemCols, collapse=", "), ", SIZE, SUPPORT, LIFT" , sep="")
      itemsetTable2 <-  paste('SELECT ', itemSetColList, ' FROM ', exportModelTable,' where MODELUSAGE= \'ItemSet\'',sep="")
      
      itemsetTable  <- ""
      unionAll  <- ""
      for (itemCol in itemCols) { 
        itemsetTable  <- paste(	itemsetTable,  unionAll,
                                'SELECT integer(SID) as ITEMSETID, integer(', itemCol, ') as ITEMID FROM ', 
                                exportModelTable,
                                ' where MODELUSAGE= \'Itemset\' and ', itemCol,' is not NULL', 
                                sep="")
        unionAll <- " UNION ALL "						
      }
      
      itemsetTableOld <- paste( 'SELECT integer(SID) as ITEMSETID, integer(ITEM1) as ITEMID FROM ', exportModelTable,' where MODELUSAGE= \'Itemset\' UNION ALL ', 
                                'SELECT integer(SID) as ITEMSETID, integer(ITEM2) as ITEMID FROM ', exportModelTable,' where MODELUSAGE= \'Itemset\' and not ITEM2 is NULL UNION ALL ', 
                                'SELECT integer(SID) as ITEMSETID, integer(ITEM3) as ITEMID FROM ', exportModelTable,' where MODELUSAGE= \'Itemset\' and not ITEM3 is NULL UNION ALL ', 
                                'SELECT integer(SID) as ITEMSETID, integer(ITEM4) as ITEMID FROM ', exportModelTable,' where MODELUSAGE= \'Itemset\' and not ITEM4 is NULL UNION ALL ', 
                                'SELECT integer(SID) as ITEMSETID, integer(ITEM5) as ITEMID FROM ', exportModelTable,' where MODELUSAGE= \'Itemset\' and not ITEM5 is NULL', sep="")
      
      ruleColList <- "GID, integer(RHS_SID) as HEADID, integer(LHS_SID) as BODYID, RHS_SIZE, LHS_SIZE, SUPPORT, CONFIDENCE, LIFT, CONVICTION, AFFINITY, LEVERAGE"  
      ruleTable <-  paste('SELECT ', ruleColList, ' FROM ', exportModelTable,' where MODELUSAGE= \'Rule\'',sep="")
      
      headSupport <- paste('SELECT R.RHS_SID, R.LHS_SID, I.SUPPORT FROM ', exportModelTable,' R, ', exportModelTable,
                           ' I where R.MODELUSAGE= \'Rule\' and I.MODELUSAGE= \'ItemSet\' and R.LHS_SID =I.SID', "")
      
      item.out <- idaQuery(itemTable)
      itemset.out <- idaQuery(itemsetTable)
      rule.out <- idaQuery(ruleTable)
      head.out <- idaQuery(headSupport)
      
    }, error = function(e) {
      # in case of error, let user know what happend
      stop(e)
    }, finally = {
      idaDeleteTable(exportModelTable)
    }
    )
    
  } else {
    
    itemTable <- paste('SELECT * FROM "',modelSchema,'"."',model,'_ITEMS" ORDER BY ITEMID',sep="")
    itemsetTable <- paste('SELECT * FROM "',modelSchema,'"."',model,'_ASSOCPATTERNS"',sep="")
    
    ruleTable <- paste('SELECT A.*,B.SUPPORT FROM "',modelSchema,'"."',model,'_ASSOCRULES" A',',"',modelSchema,'"."',model,'_ASSOCPATTERNS_STATISTICS" B WHERE A.ITEMSETID=B.ITEMSETID ORDER BY RULEID',sep="")
    headSupport <- paste('SELECT A.RULEID,B.SUPPORT FROM "',modelSchema,'"."',model,'_ASSOCRULES" A',',"',modelSchema,'"."',model,'_ASSOCPATTERNS_STATISTICS" B WHERE A.HEADID=B.ITEMSETID ORDER BY RULEID',sep="")
    
    item.out <- idaQuery(itemTable)
    itemset.out <- idaQuery(itemsetTable)
    rule.out <- idaQuery(ruleTable)
    head.out <- idaQuery(headSupport)
    
    rule.out$LIFT <- rule.out$CONFIDENCE/head.out$SUPPORT
  } 
  # how many itemsets are there?
  n.sets <- nrow(itemset.out)
  
  # itemsets are saved in sparse matrices, i. e. for each itemset
  # there is one row, containing one column for each item, if this item
  # is inside the itemset by true or false
  # need to convert itemids into position ids in order to create the sparse matrices
  # position[ ,1] contains the itemset id
  # position[ ,2] contains the item id
  position <- itemset.out[,c('ITEMSETID','ITEMID')]
  
  # create sparse matrix with all itemsets
  sets <- sparseMatrix(position[,1],position[,2])
  # load itemnames (only required ones 1:ncol(sets))
  sets.labels <- item.out[1:ncol(sets),3]
  
  if(length(unique(sets.labels))!=length(sets.labels)) {
    warning("Duplicates in item names are not supported. Using item ids.")
    sets.labels <- item.out[1:ncol(sets),2]
  }
  
  # for arules compatibility, make sure labels are in 'AsIs' character format
  sets.labels <- I(as.character(sets.labels))
  
  # create quality table
  quality <- rule.out[ ,c("SUPPORT", "CONFIDENCE", "LIFT")]
  # for arules compatibility, columnnames must be lowercase
  names(quality) <- tolower(names(quality))
  
  info <- data.frame(data="RETREIVED FROM EXISTING MODEL", NA, support=0, confidence=0, model=I(model))
  
  # get left and right hand sides from sets
  # NOTE: calling as-Matrix again is necessary when there is only one rule
  rule.count <- nrow(rule.out)
  if (rule.count == 0) {
    lhs <- new("itemMatrix")
    rhs <- new("itemMatrix")
  } else {
    if (rule.count == 1) {
      lhs.data <- as(Matrix(sets[rule.out[ ,"BODYID"], ]),"nsparseMatrix")
      rhs.data <- as(Matrix(sets[rule.out[ ,"HEADID"], ]),"nsparseMatrix")
    } else {
      lhs.data <- Matrix::t(sets[rule.out[ ,"BODYID"], ])
      rhs.data <- Matrix::t(sets[rule.out[ ,"HEADID"], ])
    }
    # create lhs and rhs as "itemMatrix" objects, transpose lhs and rhs for compatibility
    lhs <- new("itemMatrix", data=lhs.data, itemInfo=data.frame(labels=sets.labels))
    rhs <- new("itemMatrix", data=rhs.data, itemInfo=data.frame(labels=sets.labels))
  }
  
  # create "rules" object
  rules <- new("rules", lhs=lhs, rhs=rhs, quality=quality, info=info)
  
  return(rules)
}

idaApplyRules <- function(modelname, newdata, tid, item, nametable=NULL,namecol=NULL, ...) {
  
  outtable <- idaGetValidTableName(paste("APPLYRULES_",sep=""))
  
  colu = newdata@cols
  if (!(item%in% colu))
    stop(simpleError(paste("Item variable is not available in ida.data.frame:", item)))
  
  if (!(tid %in% colu))
    stop(simpleError(paste("TID variable is not available in ida.data.frame:", tid)))
  
  
  item  <- paste('\"',item,'\"',sep="")
  tid  <- paste('\"',tid,'\"',sep="")
  
  tmpView <- idaCreateView(newdata)
  
  newColumnExpr = NULL
  if(idaIsDb2z()) {
    spname = "PREDICT_ARULE"
    newColumnExpr = '1 as "?COLUMN?"'
    if(is.null(outtable)) {
      tmpOuttable <- idaGetValidTableName(prefix = "IDAR_OUTTABLE_")
      outtable <- tmpOuttable
    }
  } else {
    spname = "PREDICT_ASSOCRULES"
  }
  tmpView <- idaCreateView(newdata, newColumn=newColumnExpr)
  
  tryCatch({	
    if(idaIsDb2z()) {
      if (is.null(nametable) || is.null(namecol)) {		
        callSP(spname,
               model=modelname,
               intable=tmpView,
               by='"?COLUMN?"',
               item=item,
               tid=tid,
               outtable=outtable,
               ...)
      } else {
        callSP(spname,
               model=modelname,
               intable=tmpView,
               by='"?COLUMN?"',
               item=item,
               tid=tid,
               outtable=outtable,
               namemap=nametable, 
               itemname=item, 
               itemnamemapped=namecol, 
               ...)            	
      }  		
    } else {
      callSP(spname,
             model=modelname,
             intable=tmpView,
             item=item,
             tid=tid,
             outtable=outtable)
    }        
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
