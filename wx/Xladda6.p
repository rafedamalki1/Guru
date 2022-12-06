/*xladda6.p*/



DEFINE NEW SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO. 

globforetag = "gsol".
/*
RUN SEEKPATH.P. 
 */
guruvar = "C:\progress\PRO8\GURU\\import\mtrl.d". 
INPUT FROM D:\PRO8\GURU\\import\mtrl.d  convert target "iso8859-1" source "iso8859-1".
REPEAT:
   CREATE MTRL.
   ASSIGN.
   IMPORT MTRL.
END.
