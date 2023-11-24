ibmdbR 1.51.0
=============

New features
------------

* `idaConnect` now validates the return value of `odbcConnect` and `stop`s if
  the call was not successful. This was previously done only by `idaInit`.

Bug fixes
---------

* `idaInit` failed because the base environment and its namespace are locked since R 4.1.0.

ibmdbR 1.50.0
=============

New features
------------

* New `idaTApply` function

ibmdbR 1.49.0
=============

New features
------------

* Exported `idaAppend` function, which appends the contents of a data.frame to an ida.data.frame.
* Usage of temporary ids in `idaLm`, if no valid id column was specified.

Documentation
---------

* Added instructions to setup ODBC on Mac.

Bug fixes
---------

* Fixed an error which could occur when no rules are returned from `idaArule`.

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
    * New DB2 for z/OS with IBM DB2 Analytics Accelerator (C) regression algorithm:
        * `idaGlm`

Bug fixes
---------

* Calculating `idaLm` model statistics failed in some cases when categorical variables were present.
