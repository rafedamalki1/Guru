/*
rbfilt3.p

Running Report Engine from an application - PRINTRB INTERFACE.

Displaying a report with a simple filter override. 
*/

             
RUN  aderb\_printrb(
       "c:\dlc\src\aderb\rbsample.prl",  /* RB-REPORT-LIBRARY */
       "Customer Discount",              /* RB-REPORT-NAME */
       "",                               /* RB-DB-CONNECTION */
       "O",                              /* RB-INCLUDE-RECORDS - letter O */
       "IN-LIST(Order-Line.Discount,
                      10,15,35) > 0",    /* RB-FILTER */
       "",                               /* RB-MEMO-FILE */
       "D",                              /* RB-PRINT-DESTINATION */
       "",                               /* RB-PRINTER-NAME */
       "",                               /* RB-PRINTER-PORT */
       "",                               /* RB-OUTPUT-FILE */
        0,                               /* RB-NUMBER-COPIES - zero */
        0,                               /* RB-BEGIN-PAGE - zero */
        0,                               /* RB-END-PAGE - zero */
       no,                               /* RB-TEST-PATTERN */
       "",                               /* RB-WINDOW-TITLE */
      yes,                               /* RB-DISPLAY-ERRORS */
      yes,                               /* RB-DISPLAY-STATUS */
       no,                               /* RB-NO-WAIT */
       "").                              /* RB-OTHER-PARAMETERS */
                                                             
                                                                                                 



