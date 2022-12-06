/* 
rbfilt2.p

Running Report engine from an application - TABLE INTERFACE. 

Prompting for a filter override with user-defined prompting
mechanism. 

You must be connected to the Runtable database in multi-user mode 
before running this procedure.
*/  

DEF VAR  high-value AS INTEGER INITIAL 0.
DEF VAR  low-value AS INTEGER INITIAL 0.
DEF VAR  rb-filter-value  AS CHARACTER INITIAL "".

FORM  "Enter Low  Value for CUSTOMER NUMBER: "  low-value at 20  SKIP
      "Enter High Value for CUSTOMER NUMBER: " high-value at 20 
         WITH FRAME TEST-FRAME CENTERED NO-LABELS.

/* prompt for filter information */
 
UPDATE low-value high-value WITH FRAME TEST-FRAME.
HIDE FRAME TEST-FRAME. 
   
/* Assign filter override to rb-filter-value */
   
rb-filter-value = "Customer.Cust-num >= " + STRING(low-value) + 
                     " AND Customer.Cust-num <= " + STRING(high-value).

/*  Add the Customer List report with the filter override to 
the Report Engine table */             
   
DO TRANSACTION:            
       
   CREATE RBREPORT.
                                      
   ASSIGN 
     RBREPORT.RB-REPORT-LIBRARY = "c:\dlc\src\aderb\rbsample.prl"
     RBREPORT.RB-REPORT-NAME = "Customer List" 
     RBREPORT.RB-PRINT-DESTINATION = "D"   
     RBREPORT.RB-INCLUDE-RECORDS  = "O"
     RBREPORT.RB-FILTER  = rb-filter-value
     RBREPORT.RB-DISPLAY-ERRORS = yes
     RBREPORT.RB-DISPLAY-STATUS = yes.
     
   RELEASE RBREPORT.  
     
END.  

/* Display report with filter override and delete record. */
      
RUN  aderb\_prore(false,
           "-db Runtable -S servername -H hostname -N networktype -rbdel"). 
  
