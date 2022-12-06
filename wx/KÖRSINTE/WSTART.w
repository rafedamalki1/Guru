/*WSTART.W*/
/*
WSTART.W input från  valdbtemp mm         
APPCLASSER.p   kollar om valdbtemp har satt classer         
Modules\Global\startclass.p   sätter varforetypchar[48]        Guru.Konstanter:varforetypchar
            
startclass.p spring.p  sätter classerkoll      varforetypchar[48] = classerkoll.   

körs bara ifrån dvs bara vi som kan fulstarta guru

*/ 
DEFINE INPUT  PARAMETER gforetag AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER classerkoll AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nyprog AS LOGICAL NO-UNDO.




DEFINE VARIABLE korfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE oshelp AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE Urlsite AS CHARACTER NO-UNDO.
DEFINE VARIABLE versionok AS LOGICAL NO-UNDO.
DEFINE VARIABLE foret AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.


IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:  
   vc = "".
   /*
   RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT foret,OUTPUT Urlsite).
   */
   RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT foret).
   
END.
   
/*kollar om man har gått över till classer*/
IF classerkoll = "" THEN DO:
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN APPCLASSER.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT gforetag, OUTPUT classerkoll). 
   END.
   ELSE DO:
      RUN APPCLASSER.p 
         (INPUT gforetag, OUTPUT classerkoll).                  
   END.   
END.      

IF classerkoll = "" THEN DO:
   RUN STARTMULTI.P (INPUT-OUTPUT nyprog, INPUT classerkoll).                        
   REPEAT:
      IF Guru.Konstanter:globanvbyt NE "" THEN DO:
         RUN STARTMULTI.P (INPUT-OUTPUT nyprog, INPUT classerkoll). 
      END.
      ELSE LEAVE.
   END.
END.
ELSE DO:
   RUN Modules\Global\GuruClasserStart.p (INPUT-OUTPUT nyprog, INPUT Guru.Konstanter:globanvbyt).

   RUN STARTMULTI.P (INPUT-OUTPUT nyprog, INPUT classerkoll). 
                     
   REPEAT:
    /*Anders Olsson Elpool i Umeå AB  27 nov 2017 11:04:04 
    LOP OM MAN BYTER USER 
    */
      IF Guru.Konstanter:globanvbyt NE "" THEN DO:
         RUN Modules\Global\GuruClasserStart.p (INPUT-OUTPUT nyprog,INPUT Guru.Konstanter:globanvbyt).
         RUN STARTMULTI.P (INPUT-OUTPUT nyprog, INPUT classerkoll).  
      END.
      ELSE LEAVE.
   END. 
END.      
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
   
END.


     



