ibmdbR 1.48.0
=============

New features
------------

* Added support for DB2 for z/OS (C) in conjunction with IBM DB2 Analytics Accelerator (C).
    * DB2 for z/OS with IBM DB2 Analytics Accelerator (C) support for:
      * `idaArule` 
      * `idaKMeans`
      * `idaLm`
      * `idaNaiveBayes`
      * `idaTree`
    * New DB2 for z/OS with IBM DB2 Analytics Accelerator (C) clustering algorithms:
      * `idaTwoStep` 
      * `idaDivCluster`

Bug fixes
---------

* Calculating `idaLm` model statistics failed in some cases when categorical variables were present