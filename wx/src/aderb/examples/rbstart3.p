/*
rbstart3.p

Running Report Engine from an application - PRNTRB2 INTERFACE.

Displaying a report as saved by Report Builder. 
*/  
             
RUN  aderb\_prntrb2(
       "c:\dlc\src\aderb\rbsample.prl", /* RB-REPORT-LIBRARY */
       "Customer List",                 /* RB-REPORT-NAME */
       "",                              /* RB-DB-CONNECTION */
       "",                              /* RB-INCLUDE-RECORDS */
       "",                              /* RB-FILTER */
       "",                              /* RB-MEMO-FILE */
       "D",                             /* RB-PRINT-DESTINATION */
       "",                              /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        0,                              /* RB-NUMBER-COPIES  - zero */
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "",                              /* RB-WINDOW-TITLE */
      yes,                              /* RB-DISPLAY-ERRORS */
      yes,                              /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
       "",                              /* RB-OTHER-PARAMETERS */
       "status.txt").                   /* RB-STATUS-FILE */


