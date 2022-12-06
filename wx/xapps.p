/*xapps.p*/
DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE NEW SHARED VARIABLE vartvar AS CHARACTER NO-UNDO.
 
DEFINE  VARIABLE appcon AS LOGICAL NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
{CONAPP.I}
vartvar = "\\GRANGURU\guru_ser\server\PRO9s\RR.TXT ".
RUN XLD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
(INPUT vartvar).

IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         DELETE OBJECT Guru.Konstanter:apphand.

/*/*1*/
OUTPUT TO D:\delad\server\PRO9S\RR.TXT APPEND. 
PUT "PC002 is a machine in the network" today skip.
OUTPUT CLOSE.

OUTPUT TO \\192.121.248.232\delad\server\PRO9S\RR.TXT APPEND. 
PUT "PC004 is a machine in the network" today skip.
OUTPUT CLOSE.

OUTPUT TO \\GRANGURU\delad\server\PRO9S\RR.TXT APPEND. 
PUT "PC003 is a machine in the network" today skip.
OUTPUT CLOSE.

OUTPUT TO f:\protemp8\RR.TXT APPEND.
PUT "Hello" SKIP.
PUT "F is a mapped network drive" today  skip.
OUTPUT CLOSE.



/*2*/
OUTPUT TO d:\delad\ELPAO\RR.TXT APPEND.
PUT "Hello 3" SKIP.
PUT "D is the hard drive on machine NTSERVER2" today  skip.
OUTPUT CLOSE.
 /*3*/
OUTPUT TO \\ntserver2\delad\ELPAO\RR.TXT APPEND.
PUT "Hello 4" SKIP.
PUT "NTSERVER2 is the machine where appserver is running on."  today skip.
*/
/*4*/
/*
OUTPUT TO F:\indata\RR.TXT APPEND.
PUT "Hello" SKIP.
PUT "PC004 is a machine in the network" today skip.
OUTPUT CLOSE. 
OUTPUT TO \\granhr\pak\indata\RR.TXT APPEND.
PUT "Hello" SKIP.
PUT "PC005 is a machine in the network" today skip.
OUTPUT CLOSE.
*/
/*4*/

 /*
OUTPUT TO d:\delad\server\PRO8S\RR.TXT APPEND.
PUT "Hello" SKIP.
PUT "d: is the machine where appserver is running on."  today skip.
OUTPUT CLOSE.
OUTPUT TO \\192.71.44.30\guru_ser\server\PRO8S\RR.TXT APPEND.
PUT "Hello" SKIP.
PUT "192.71.44.30 is the machine where appserver is running on."  today skip.
OUTPUT CLOSE.

OUTPUT TO \\GRANGURU\guru_ser\server\PRO8S\RR.TXT APPEND.
PUT "Hello" SKIP.
PUT "GRANGURU is the machine where appserver is running on."  today skip.
OUTPUT CLOSE.
 */
