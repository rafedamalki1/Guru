/*
rbdbconn.p

Running Report Engine from an application - PRINTRB INTERFACE.

Overriding database connection information.   
*/

RUN aderb\_printrb(
	"c:\dlc\src\aderb\rbsample.prl",     /* RB-REPORT-LIBRARY */
	"Accts Receivable",                  /* RB-REPORT-NAME */
	"sports = -db sports -H newhost 
		-S newserver -N TCP",        /* RB-DB-CONNECTION */
	"",                                  /* RB-INCLUDE-RECORDS */
	"",                                  /* RB-FILTER */
	"",                                  /* RB-MEMO-FILE */
	"",                                  /* RB-PRINT-DESTINATION */
	"",                                  /* RB-PRINTER-NAME */
	"",                                  /* RB-PRINTER-PORT */
	"",                                  /* RB-OUTPUT-FILE */
	0,                                   /* RB-NUMBER-COPIES - zero */
	0,                                   /* RB-BEGIN-PAGE - zero */
	0,                                   /* RB-END-PAGE - zero */
	no,                                  /* RB-TEST-PATTERN */
	"",                                  /* RB-WINDOW-TITLE */
	yes,                                 /* RB-DISPLAY-ERRORS */
	yes,                                 /* RB-DISPLAY-STATUS */
	no,                                  /* RB-NO-WAIT */
	"").                                 /* RB-OTHER-PARAMETERS */

