Getting Started
---------------
The ibmdbR package works with the following IBM database products and services:

- Products:
    -	IBM® Db2® Warehouse (formerly IBM dashDB® Local)
    - IBM Db2 for z/OS® in conjunction with IBM Db2 Analytics Accelerator for z/OS
- Managed services:
    -	IBM Db2 Warehouse on Cloud (formerly IBM dashDB for Analytics)

Before you start, you must install one of these products or provision an instance of one of these managed services. For more information, see:

-	[IBM Db2 Warehouse Offerings](https://www.ibm.com/analytics/us/en/technology/cloud-data-services/dashdb/)
- [IBM Db2 for z/OS ](https://www.ibm.com/analytics/us/en/db2/db2-for-zos/)
- [IBM Db2 Analytics Accelerator for z/OS](http://www-03.ibm.com/software/products/en/db2analacceforzos)

If you use a Db2 managed service directly, everything is pre-configured and ready to be used 
from your web browser. If you connect to a Db2 server or use a Db2 managed service remotely, you must first install the appropriate client driver packages. You can download the packages from the service’s web console. 
After you install the driver packages, you must configure an ODBC source. Refer to the documentation of 
your operating system for information on how to do this.

For an introduction to the package and first steps, see the package vignette "Use IBM In-Database Analytics with R".
