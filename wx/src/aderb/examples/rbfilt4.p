/*
rbfilt4.p

Running Report Engine from an application - PRINTRB INTERFACE.

Prompting the user for a filter override with user-defined
prompting mechanism. 
*/


DEF VAR high-value AS INTEGER INITIAL 0.
DEF VAR low-value AS INTEGER INITIAL 0.
DEF VAR rb-filter-value  AS CHARACTER INITIAL "".

FORM  "Enter Low  Value for CUSTOMER NUMBER: "  low-value at 20  SKIP
      "Enter High Value for CUSTOMER NUMBER: " high-value at 20 
         WITH FRAME TEST-FRAME CENTERED NO-LABELS.

/* prompt for filter override values */
 
UPDATE low-value high-value WITH FRAME TEST-FRAME.
HIDE FRAME TEST-FRAME.

   
/* Assign filter string to rb-filter-value */
   
rb-filter-value = "Customer.Cust-num >= " + STRING(low-value) + 
                    " AND Customer.Cust-num <= " + STRING(high-value).


/* Display Report with filter override */
        
RUN  aderb\_printrb(
       "c:\dlc\src\aderb\rbsample.prl",   /* RB-REPORT-LIBRARY */
       "Customer List",                   /* RB-REPORT-NAME */
       "",                                /* RB-DB-CONNECTION */
       "O",                               /* RB-INCLUDE-RECORDS - letter O*/
       rb-filter-value,                   /* RB-FILTER */
       "",                                /* RB-MEMO-FILE */
       "D",                               /* RB-PRINT-DESTINATION */
       "",                                /* RB-PRINTER-NAME */
       "",                                /* RB-PRINTER-PORT */
       "",                                /* RB-OUTPUT-FILE */
        0,                                /* RB-NUMBER-COPIES - zero */
        0,                                /* RB-BEGIN-PAGE -zero */
        0,                                /* RB-END-PAGE - zero */
       no,                                /* RB-TEST-PATTERN */
       "",                                /* RB-WINDOW-TITLE */
      yes,                                /* RB-DISPLAY-ERRORS */
      yes,                                /* RB-DISPLAY-STATUS */
       no,                                /* RB-NO-WAIT */
       "").                               /* RB-OTHER-PARAMETERS */

