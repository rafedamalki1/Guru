/* 
rbfilt1.p

Running Report Engine from an application - TABLE INTERFACE.

Running a report with a simple filter override.  

You must be connected to the Runtable database in multi-user mode before 
running this procedure. 
*/

DO TRANSACTION:
 
  CREATE RBREPORT.

  ASSIGN 
    RBREPORT.RB-REPORT-LIBRARY = "c:\dlc\src\aderb\rbsample.prl"
    RBREPORT.RB-REPORT-NAME = "Customer Discount"
    RBREPORT.RB-PRINT-DESTINATION = "D"
    RBREPORT.RB-INCLUDE-RECORDS = "O"    
    RBREPORT.RB-FILTER = "IN-RANGE(Order-Line.Discount, 15, 30)"
    RBREPORT.RB-DISPLAY-STATUS = yes
    RBREPORT.RB-DISPLAY-ERRORS = yes.
    
  RELEASE RBREPORT.  

END.

RUN aderb\_prore(false,
          "-db Runtable -S servername -H hostname -N networktype -rbdel").

