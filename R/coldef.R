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

############## General ######################

setMethod("print", signature(x="ida.col.def"),
    function (x) {
      cat(paste("Column definition: ", x@term, " base table: ",x@table@table ," \n Use this to define new columns on a ida.data.frame using the $ operator. To select a subset of a table, use bldf[] notation. "),"\n")
    }
)

setMethod("show", signature(object="ida.col.def"),
    function (object) {
      cat(paste("Column definition: ", object@term, " base table: ",object@table@table ," \n Use this to define new columns on a ida.data.frame using the $ operator. To select a subset of a table, use bldf[] notation. "),"\n")
    }
)


setMethod("as.vector", signature(x="ida.col.def"),
    function (x,mode="any") {
      res <- idaQuery("SELECT ", x@term , " FROM ",x@table@table,ifelse(nchar(x@table@where), paste(" WHERE ", x@table@where), ""))
      return(res[[1]])
    }
)


as.ida.col.def <- function(x) {
  if(inherits(x,"ida.data.frame")) {
    return(paste('"',x@cols[1],'"',sep=''))
  } else if(inherits(x,"ida.col.def")) {
    return(x@term)
  } else {
    if(is.character(x)) {
      return(paste("'",x,"'",sep=''))
    } else {
      return(as.character(x))
    }
  }	
}

################ Aggregate functions ############################

setIdaAggFunct <- function(aggFunct,sqlAggFunct=aggFunct) {
  setMethod(aggFunct,signature(x='ida.col.def'),function (x) {
        checkLogical(F,x);
        checkAggregation(x)
        return(new(Class="ida.col.def",term=paste(sqlAggFunct,'(',as.ida.col.def(x),')',sep=''),table=x@table,type="expr",aggType="simple"));
      });
}

setIdaAggFunct("mean","AVG")
setIdaAggFunct("min","MIN")
setIdaAggFunct("max","MAX")
setIdaAggFunct("var","VARIANCE")
setIdaAggFunct("sum","SUM")
setIdaAggFunct("var","VARIANCE")
setIdaAggFunct("sd","STDDEV")


################ Arithmetic operators ############################

setIdaGenericOp <- function(op,sqlFunct=NULL,requireLogical=F,outputLogical=F) {
  
  if(is.null(sqlFunct)) {
    setMethod(op,signature(e1="ida.col.def"),function (e1, e2) {
          checkSameTable(e1,e2);
          checkLogical(F,e1,e2);
          return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),op,as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="expr",aggType=aggType(e1,e2)));
        });
    
    setMethod(op,signature(e1="ida.col.def",e2="ida.col.def"),function (e1, e2) {
          checkSameTable(e1,e2);
          checkLogical(F,e1,e2);
          return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),op,as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="expr",aggType=aggType(e1,e2)));
        });
    
    setMethod(op,signature(e2="ida.col.def"),function (e1, e2) {
          checkSameTable(e1,e2);
          checkLogical(F,e1,e2);
          return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),op,as.ida.col.def(eval(e2)),')',sep=''),table=e2@table,type="expr",aggType=aggType(e1,e2)));
        });
  } else {
    setMethod(op,signature(e1="ida.col.def"),function (e1, e2) {
          checkSameTable(e1,e2);
          checkLogical(F,e1,e2);
          return(new(Class="ida.col.def",term=paste(sqlFunct,'(',as.ida.col.def(eval(e1)),',',as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="expr",aggType=aggType(e1,e2)));
        });
    
    setMethod(op,signature(e1="ida.col.def",e2="ida.col.def"),function (e1, e2) {
          checkSameTable(e1,e2);
          checkLogical(F,e1,e2);
          return(new(Class="ida.col.def",term=paste(sqlFunct,'(',as.ida.col.def(eval(e1)),',',as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="expr",aggType=aggType(e1,e2)));
        });
    
    setMethod(op,signature(e2="ida.col.def"),function (e1, e2) {
          checkSameTable(e1,e2);
          checkLogical(F,e1,e2);
          return(new(Class="ida.col.def",term=paste(sqlFunct,'(',as.ida.col.def(eval(e1)),',',as.ida.col.def(eval(e2)),')',sep=''),table=e2@table,type="expr",aggType=aggType(e1,e2)));
        });
  }
}


setIdaGenericOp('+');
setIdaGenericOp('-');
setIdaGenericOp('*');
setIdaGenericOp('/');
setIdaGenericOp('^','POWER');
setIdaGenericOp('%%','MOD');

################ Comparison operators ############################

setIdaGenericCompareOp <- function(op,sqlOp=op) {
  
  setMethod(op,signature(e1="ida.col.def"),function (e1, e2) {
        checkSameTable(e1,e2);
        checkLogical(F,e1,e2);
        return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),sqlOp,as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="logical",aggType="none"));
      });
  
  setMethod(op,signature(e1="ida.col.def",e2="ida.col.def"),function (e1, e2) {
        checkSameTable(e1,e2);
        checkLogical(F,e1,e2);
        return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),sqlOp,as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="logical",aggType="none"));
      });
  
  setMethod(op,signature(e2="ida.col.def"),function (e1, e2) {
        checkSameTable(e1,e2);
        checkLogical(F,e1,e2);
        return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),sqlOp,as.ida.col.def(eval(e2)),')',sep=''),table=e2@table,type="logical",aggType="none"));
      });
}

setIdaGenericCompareOp('==','=');
setIdaGenericCompareOp('!=','<>');
setIdaGenericCompareOp('<','<');
setIdaGenericCompareOp('>=','>=');
setIdaGenericCompareOp('<=','<=');
setIdaGenericCompareOp('>','>');


################ Logical operators ############################

setIdaGenericLogicalOp <- function(op,sqlOp=op) {
  
  setMethod(op,signature(e1="ida.col.def"),function (e1, e2) {
        checkSameTable(e1,e2);
        checkLogical(T,e1,e2);
        return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),sqlOp,as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="logical",aggType="none"));
      });
  
  setMethod(op,signature(e1="ida.col.def",e2="ida.col.def"),function (e1, e2) {
        checkSameTable(e1,e2);
        checkLogical(T,e1,e2);
        return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),sqlOp,as.ida.col.def(eval(e2)),')',sep=''),table=e1@table,type="logical",aggType="none"));
      });
  
  setMethod(op,signature(e2="ida.col.def"),function (e1, e2) {
        checkSameTable(e1,e2);
        checkLogical(T,e1,e2);
        return(new(Class="ida.col.def",term=paste('(',as.ida.col.def(eval(e1)),sqlOp,as.ida.col.def(eval(e2)),')',sep=''),table=e2@table,type="logical",aggType="none"));
      });
}

setIdaGenericLogicalOp('&',' AND ')
setIdaGenericLogicalOp('|',' OR ')

setMethod('!',signature(x="ida.col.def"),function (x) {
      checkLogical(T,x);
      return(new(Class="ida.col.def",term=paste('NOT ',as.ida.col.def(eval(x)),' ',sep=''),table=x@table,type="logical",aggType="none"));
    });

################ Casting operators ############################

setMethod('as.numeric',signature(x="ida.col.def"),function (x) {
      checkLogical(F,x);
      return(new(Class="ida.col.def",term=paste('CAST(',as.ida.col.def(eval(x)),' AS DOUBLE)',sep=''),table=x@table,type="expr",aggType=aggType(x)));
    });

setMethod('as.character',signature(x="ida.col.def"),function (x) {
      checkLogical(F,x);
      return(new(Class="ida.col.def",term=paste('CAST(',as.ida.col.def(x),' AS VARCHAR(100))',sep=''),table=x@table,type="expr",aggType=aggType(x)));
    });

setMethod('as.integer',signature(x="ida.col.def"),function (x) {
      
      checkLogical(F,x);
      return(new(Class="ida.col.def",term=paste('CAST(',as.ida.col.def(eval(x)),' AS INT)',sep=''),table=x@table,type="expr",aggType=aggType(x)));
    });

################ NULL check ############################

db.is.null <- function(x) {
  
  if(!inherits(x,"ida.col.def")) {
    stop("The db.is.null operator can only be applied to a column definition.")
  }
  
  checkLogical(F,x);
  return(new(Class="ida.col.def",term=paste(as.ida.col.def(eval(x)),' IS NULL ',sep=''),table=x@table,type="logical",aggType="none"));
}

################ IFELSE ############################

setMethod('ifelse',signature(test="ida.col.def"),function (test,yes,no) {
      
      checkSameTable(test,yes,no)
      
      if(test@type!='logical')
        stop("Operator can only be applied to logical values.")
      
      return(new(Class="ida.col.def",term=paste("CASE WHEN ", as.ida.col.def(test)," THEN ",as.ida.col.def(eval(yes))," ELSE ",as.ida.col.def(eval(no))," END ",sep=''),table=test@table,type="expr",aggType="none"));		
    });

################ Scalar functions ############################

setIdaScalarFunct <- function(scalFunct,scalFunctSQL) {
  setMethod(scalFunct,signature(x='ida.col.def'),function (x) {
        checkLogical(F,x);
        return(new(Class="ida.col.def",term=paste(scalFunctSQL,'(',as.ida.col.def(eval(x)),')',sep=''),table=x@table,type="expr",aggType=aggType(x)));
      });
}

setIdaScalarFunct('abs','ABS')
setIdaScalarFunct('sqrt','SQRT')
setIdaScalarFunct('log','LN')
setIdaScalarFunct('log10','LOG10')
setIdaScalarFunct('exp','EXP')
setIdaScalarFunct('floor','FLOOR')
setIdaScalarFunct('round','ROUND')
setIdaScalarFunct('ceiling','CEIL')

################ Utilities ############################

aggType <- function(...) {
  
  args = list(...);
  
  for(i in 1:length(args)) {
    if(inherits(args[[i]],'ida.col.def')) {
      if(args[[i]]@aggType=="simple") {
        return("simple")
      }
    }
  }
  return("none");	
}

checkSameTable <- function(...) {
  
  args = list(...);
  bdf <- NULL;	
  
  for(i in 1:length(args)) {
    if(inherits(args[[i]],'ida.col.def')) {
      if(!is.null(args[[i]]@table)) {
        if(is.null(bdf)) {
          bdf <- args[[i]]@table
        } else {
          if((args[[i]]@table@table!=bdf@table)||(args[[i]]@table@where!=bdf@where))
            stop("Cannot combine two columns from different ida.data.frames.")
        }
      }
    }	
  }	
}

checkLogical <- function(requireLogical,...) {
  
  args = list(...);
  bdf <- NULL;	
  
  for(i in 1:length(args)) {
    if(inherits(args[[i]],'ida.col.def')) {
      if((args[[i]]@type=='logical')) {
        if(!requireLogical)
          stop("This operator cannot be applied to logical values.")
      } else {
        if(requireLogical)
          stop("This operator can only be applied to logical values.")
      }
    }	
  }
}

checkAggregation <- function(...) {
  args = list(...);
  bdf <- NULL;	
  
  for(i in 1:length(args)) {
    if(inherits(args[[i]],'ida.col.def')) {
      if((args[[i]]@aggType!='none')) {
        stop("Operation cannot be applied to aggregate values.")
      }	
    }
  }
}

