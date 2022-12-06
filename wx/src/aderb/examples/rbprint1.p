/* 
rbprint1.p

Running Report Engine from an application - TABLE INTERFACE.

Prompting user for print destination and printer name
using Report Builder built-in prompting mechanism. 

You must be connected to the Runtable database in multi-user mode before 
running this procedure.
*/

DO TRANSACTION:
 
   CREATE RBREPORT.

   ASSIGN
      RBREPORT.RB-REPORT-LIBRARY = "c:\dlc\src\aderb\rbsample.prl"
      RBREPORT.RB-REPORT-NAME = "Accts Receivable"
      RBREPORT.RB-PRINT-DESTINATION = "?"
      RBREPORT.RB-PRINTER-NAME = "?"
      RBREPORT.RB-DISPLAY-STATUS = yes
      RBREPORT.RB-DISPLAY-ERRORS = yes. 
      
    RELEASE RBREPORT.     
  
END.
                                                         
/* Display report and delete record */
  
RUN aderb\_prore(false, 
          "-db Runtable -S servername -H hostname -N networktype -rbdel").  

