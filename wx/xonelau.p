/*xonelau.p*/
/* läsa in materielkataloger onninen och elektroskandia*/
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE sokfil AS CHARACTER.

/*utfil = "C:\nyelekt.txt".*/
utfil = "C:\nyonninen.txt".
sokfil = SEARCH(utfil).
/* 1 0 Onninen , 1 1 Elektroskandia*/
IF sokfil = ? THEN sokfil = sokfil.
ELSE DO:            
   RUN GRANAHLS.P (INPUT 1, INPUT 0).   
END.            
/*utfil = "F:\elpool\elpnj\mtrlelpool\nyonninen.txt".
sokfil = SEARCH(utfil).
IF sokfil = ? THEN sokfil = sokfil.
ELSE DO:           
   RUN GRANAHLS.P (INPUT 1, INPUT 2).   
END.            */

