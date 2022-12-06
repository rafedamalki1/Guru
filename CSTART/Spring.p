
/*------------------------------------------------------------------------
    File        : Spring.p
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : 
    Created     : Thu Oct 20 14:50:09 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/
 {AppSpringSetInfo.I}

DEFINE VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.
DEFINE VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.  
{VALDBTEMP.I} 

DEFINE INPUT PARAMETER appat AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER valrunvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE wcrunvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
DEFINE VARIABLE AppSpringFel AS CHARACTER NO-UNDO.
DEFINE VARIABLE FILL-IN-START AS CHARACTER NO-UNDO.

DEFINE VARIABLE finnsfil AS LOGICAL NO-UNDO.
DEFINE VARIABLE cWorkDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE intdir AS INTEGER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE singelhelp AS CHARACTER NO-UNDO.

valrunvar = TRIM(valrunvar).
RUN filkoll_UI.
RUN WC-CHECK.P (INPUT FILL-IN-START, OUTPUT wcrunvar, OUTPUT TABLE valdbtemp).

FIND FIRST valdbtemp WHERE valdbtemp.GFORETAG = valrunvar NO-LOCK NO-ERROR.

IF NOT AVAILABLE valdbtemp THEN DO:
   QUIT.
END. 

Guru.GlobalaVariabler:AppSpringFel = "".
Guru.Konstanter:apphand = ?.
Guru.Konstanter:appcon = appat.
Guru.Konstanter:conappvar = valdbtemp.APPCON.
singelhelp = "".
singelhelp = Guru.Konstanter:AppSpringSet[14].
Guru.Konstanter:AppSpringSet = "". 
RUN SpringStart.p (INPUT valdbtemp.GFORETAG).
{AppSprinSet.I}
Guru.Konstanter:AppSpringSet[14] = singelhelp.
Guru.Konstanter:AppSpringSet[15] = FILL-IN-START.

IF appat = TRUE THEN Guru.Konstanter:AppSpringSet[16] = CHR(65) + CHR(112) + CHR(112) + CHR(97) + CHR(116).
ELSE Guru.Konstanter:AppSpringSet[16] = CHR(69) + CHR(106) + CHR(97) + CHR(112) + CHR(112) + CHR(97) + CHR(116).

Guru.Konstanter:GForetag = valdbtemp.GFORETAG.
IF appat = TRUE THEN DO:
   RUN appcon_UI.
   IF  Guru.Konstanter:appcon = FALSE THEN RETURN. 
END.

IF Guru.Konstanter:appcon THEN DO:                           
   RUN STYREAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT valdbtemp.FORETAG, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT TRUE). 
END.
ELSE DO:
   RUN STYREAPP.P 
   (INPUT valdbtemp.FORETAG, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT TRUE).               
END.
{STYREAPPLADD.I}  
 
IF Guru.Konstanter:varforetypval[55] = 1 THEN  Guru.GlobalaVariabler:Kryptonit = TRUE.
ELSE Guru.GlobalaVariabler:Kryptonit = FALSE.   
IF FILL-IN-START = "FSUND" OR FILL-IN-START = "FMISV" OR FILL-IN-START = "FSNAT"  OR FILL-IN-START = "FELPA" THEN DO:
   ASSIGN 
   Guru.Konstanter:AppSpringSet[6]  = CHR(70) + CHR(76) + CHR(69) + CHR(88)  /*"FLEX"*/
   Guru.Konstanter:AppSpringSet[7]  =  CHR(70) + CHR(76) + CHR(69) + CHR(88) + CHR(65).   /*"FLEXA".*/  
   IF Guru.Konstanter:varforetypval[56] = 6 THEN Guru.Konstanter:AppSpringSet[7] =  CHR(70) + CHR(76) + CHR(69) + CHR(88) + CHR(65) + CHR(82).  /*"FLEXAR".*/  
  
END.
IF FILL-IN-START = "SELNDEPA" THEN DO:
   ASSIGN 
   Guru.Konstanter:AppSpringSet[6]  = CHR(83) + CHR(69) + CHR(76) + CHR(78) + CHR(68) + CHR(69) + CHR(80) + CHR(65) /* "SELNDEPA"*/
   Guru.Konstanter:AppSpringSet[7]  =  CHR(68) + CHR(69) + CHR(80) + CHR(65) + CHR(83) + CHR(69) + CHR(76) + CHR(78).  /* "DEPASELN".*/  
  
END.
 
IF SESSION:CLIENT-TYPE = "WEBCLIENT" AND Guru.Konstanter:AppSpringSet[5]  = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN DO:
   IF Guru.Konstanter:AppSpringSet[6] = "FLEX" OR  Guru.Konstanter:AppSpringSet[6]  = "DEPASELN" THEN DO:
      RUN AppSpringDbCon.P ON  Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:AppSpringSet,INPUT 0, OUTPUT AppSpringFel).
   END.    
   ELSE Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
END.

ELSE DO:
   IF Guru.Konstanter:AppSpringSet[6] = "" THEN Guru.Konstanter:AppSpringSet[6] = Guru.Konstanter:AppSpringSet[5].
   Guru.Konstanter:AppSpringSet[11] = TRIM(STRING(Guru.Konstanter:appcon,"true/false")).
   IF Guru.Konstanter:appcon = TRUE THEN RUN AppSpringDbCon.P ON  Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:AppSpringSet,INPUT 0, OUTPUT AppSpringFel).
   ELSE RUN AppSpringDbCon.P (INPUT Guru.Konstanter:AppSpringSet,INPUT 0, OUTPUT AppSpringFel).
   Guru.GlobalaVariabler:AppSpringFel = AppSpringFel.
   /*Anders Olsson Elpool i Umeå AB  12 okt 2022 10:02:56 
   LIGGER PÅ SERVER
  STOPPAR ELPAO = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) */
   IF Guru.Konstanter:AppSpringSet[5] = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) AND Guru.Konstanter:AppSpringSet[7] = "" AND    
   Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL9.I}   THEN Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
   
   
END.

spring:
REPEAT: 
   {GURUSTARTVILLKOR.I}
   
   IF Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL9.I} THEN DO:
      Guru.Konstanter:AppSpringSet[10] = {APPCON1.i} + {APPCON2.i} .
      
      IF Guru.Konstanter:appcon = TRUE THEN RUN FORVERSpring.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:AppSpringSet[1],INPUT Guru.Konstanter:conappvar).
      ELSE RUN FORVERSpring.p (INPUT Guru.Konstanter:AppSpringSet[1],INPUT Guru.Konstanter:conappvar).  
   END.   
   ELSE IF Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I} THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         IF Guru.Konstanter:apphand:CONNECTED()  THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT() NO-ERROR.
      END.   
      Guru.Konstanter:appcon = FALSE.
      IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
      Guru.Konstanter:apphand = ?.
      Guru.Konstanter:AppSpringSet[6]  = "".
      RUN GuruSpringMail.w (INPUT appat).
      
   END. 
   ELSE DO:
      
      IF Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL12.I} THEN  Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
      ELSE IF Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL7.I} THEN  Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
      ELSE DO:
         MESSAGE Guru.GlobalaVariabler:AppSpringFel
         VIEW-AS ALERT-BOX.
         IF Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL12.I} THEN  Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
         ELSE IF Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL7.I} THEN  Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
         ELSE Guru.GlobalaVariabler:AppSpringFel  = "".
      END.
      IF Guru.Konstanter:appcon THEN DO: 
         IF Guru.Konstanter:apphand:CONNECTED()  THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT() NO-ERROR.
      END.
      Guru.Konstanter:appcon = FALSE.
      IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
      Guru.Konstanter:apphand = ?.
      IF Guru.GlobalaVariabler:AppSpringFel  = "" THEN DO: 
         Guru.Konstanter:AppSpringSet  = "".
         QUIT.
      END.   
      ELSE NEXT spring.
   END.
   IF Guru.Konstanter:AppSpringSet[10] = {APPCON1.i} + {APPCON2.i} THEN DO:
      &Scoped-define NEW NEW  
    /*  {GLOBVAR2DEL2.I}*/
      
      
      DEFINE VARIABLE nyprog AS LOGICAL NO-UNDO.
     
      IF  Guru.Konstanter:AppSpringSet[1] = "dELPA" OR Guru.Konstanter:AppSpringSet[1] = "classELPA" THEN Guru.SharedVariable:demokvar = TRUE.
       
      Guru.Konstanter:appfel = FALSE.   
     
      RUN Modules\Global\GuruClasserStart.p (INPUT-OUTPUT nyprog, INPUT Guru.Konstanter:globanvbyt).
      RUN STARTMULTI.P (INPUT-OUTPUT nyprog). 
      DELETE OBJECT Guru.Konstanter:startGlobalFakeRoot NO-ERROR.   
      Guru.Konstanter:startGlobalFakeRoot = ?.  
   END. 
   IF Guru.Konstanter:globanvbyt = {LOSENKOLLFEL11.I} THEN DO:
      Guru.Konstanter:globanvbyt = "".
      Guru.GlobalaVariabler:AppSpringFel = {LOSENKOLLFEL8.I}.
      NEXT spring.
   END.   
   ELSE IF Guru.Konstanter:globanvbyt = {LOSENKOLLFEL10.I} THEN DO:  
      Guru.Konstanter:globanvbyt = "".
      RUN appcon_UI.
      IF Guru.Konstanter:appcon = TRUE THEN RUN AppSpringDbCon.P ON  Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:AppSpringSet,INPUT 0, OUTPUT AppSpringFel).
      ELSE RUN AppSpringDbCon.P (INPUT Guru.Konstanter:AppSpringSet,INPUT 0, OUTPUT AppSpringFel).
      Guru.GlobalaVariabler:AppSpringFel = AppSpringFel.
      NEXT spring.
   END.   
   ELSE Guru.Konstanter:AppSpringSet  = "".
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:apphand:CONNECTED()  THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT() NO-ERROR.
   END.   
   Guru.Konstanter:appcon = FALSE.
   IF Guru.Konstanter:apphand NE ? THEN DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
   Guru.Konstanter:apphand = ?.
   LEAVE.
END.   
PROCEDURE appcon_UI :
   CREATE SERVER Guru.Konstanter:apphand.
   Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(Guru.Konstanter:conappvar,{APPCON1.i},{APPCON2.i},Guru.Konstanter:AppSpringSet[1]) NO-ERROR.
   IF NOT Guru.Konstanter:appcon THEN DO:
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         MESSAGE 
         ERROR-STATUS:NUM-MESSAGES 
         " fel uppkom vid anslutningen." SKIP 
         "Det går ej att ansluta appserver och databasen i Guru." SKIP
         "Kontakta system ansvarig." SKIP
         "Kontakta Elpool tel 090/184540." SKIP
         SKIP
         "Vill du se felmeddelandena ?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel på anslutningen"
         UPDATE view-errs AS LOGICAL .       
         IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
            MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
            ERROR-STATUS:GET-MESSAGE(ivar)
            VIEW-AS ALERT-BOX.
         END.     
      END.
      ELSE DO:
         MESSAGE 
         "Det går ej att ansluta appserver och databasen i Guru." SKIP
         "Kontakta system ansvarig." SKIP
         "Kontakta Elpool tel 090/184540." SKIP
         SKIP
         VIEW-AS ALERT-BOX  TITLE "Fel på anslutningen".
      END.
      RETURN.
   END.
END PROCEDURE.
PROCEDURE filkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.
   DEFINE VARIABLE filnamnE AS CHARACTER NO-UNDO
   VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 3. 
   finnsfil = FALSE.
   FILE-INFO:FILE-NAME = "wc-start.r".
   IF FILE-INFO:FULL-PATHNAME = ? THEN  FILE-INFO:FILE-NAME = "wc-start.w".
   intdir = R-INDEX(FILE-INFO:FULL-PATHNAME,"\").   
   cWorkDirectory = SUBSTRING(FILE-INFO:FULL-PATHNAME, 1, intdir - 1).
   intdir = R-INDEX(cWorkDirectory,"\").   
   cWorkDirectory = SUBSTRING(cWorkDirectory, 1, intdir ).  
   INPUT FROM OS-DIR(cWorkDirectory) NO-ECHO.
   REPEAT:
      SET filnamnE ^ attrlist. 
      IF filnamnE MATCHES "foretagstart.*" THEN DO: 
         finnsfil = TRUE.         
         FILL-IN-START = ENTRY(2, filnamnE, ".").   
         LEAVE.              
      END.
   END.
   INPUT CLOSE. 
   
   
END PROCEDURE.