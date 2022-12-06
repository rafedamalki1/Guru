/*DYNHMT.I*/
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandoorgfalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommandonyfalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE orginaltab AS CHARACTER NO-UNDO.
DEFINE VARIABLE jointab AS CHARACTER NO-UNDO.
DEFINE VARIABLE nytab      AS CHARACTER NO-UNDO.
DEFINE VARIABLE utvaltab      AS CHARACTER NO-UNDO.

DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE uqh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
DEFINE VARIABLE jointabh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE nyfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE nytabh AS HANDLE NO-UNDO.
DEFINE VARIABLE utvalfalth AS HANDLE NO-UNDO.
DEFINE VARIABLE utvaltabh AS HANDLE NO-UNDO.
DEFINE VARIABLE extratemptabh AS HANDLE NO-UNDO.
DEFINE VARIABLE extratemptabh2 AS HANDLE NO-UNDO.
DEFINE VARIABLE extrajointemptabh AS HANDLE NO-UNDO.
DEFINE VARIABLE uservar AS CHARACTER NO-UNDO.
PROCEDURE objdelete_UI :
  
   IF VALID-HANDLE(nyfalth) THEN DELETE OBJECT nyfalth NO-ERROR.         
   IF VALID-HANDLE(utvalfalth) THEN DELETE OBJECT utvalfalth NO-ERROR.      
   IF VALID-HANDLE(utvaltabh) THEN DELETE OBJECT utvaltabh NO-ERROR.       
   IF VALID-HANDLE(extrajointemptabh) THEN DELETE OBJECT extrajointemptabh NO-ERROR.
   /*
   IF VALID-HANDLE(extratemptabh2) THEN DELETE OBJECT extratemptabh2 NO-ERROR.  
   */
   IF VALID-HANDLE(nytabh) THEN DELETE OBJECT nytabh NO-ERROR.
   IF VALID-HANDLE(orgtabh) THEN DELETE OBJECT orgtabh NO-ERROR.
   /*IF VALID-HANDLE(extratemptabh) THEN DELETE OBJECT extratemptabh NO-ERROR.*/
   IF VALID-HANDLE(jointabh) THEN DELETE OBJECT jointabh NO-ERROR.
   ASSIGN
   nyfalth = ?
   utvalfalth = ?
   utvaltabh = ?
 
   extrajointemptabh = ?
   extratemptabh2 = ?
 
   jointabh = ?
   nytabh = ?
   orgtabh = ?.
  /* extratemptabh = ?.*/
END PROCEDURE.

PROCEDURE dynquery_UI:
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE itabfalth AS HANDLE NO-UNDO.
   
   {DYNQUERY.I}                                                     
   IF orgtabh:NAME = "EXTRADATA" THEN tabfalth = orgtabh:BUFFER-FIELD("SOKCHAR").
   IF orgtabh:NAME = "EXTRADATA" THEN itabfalth = orgtabh:BUFFER-FIELD("SOKINT").
   DO WHILE orgtabh:AVAILABLE:        
      nytabh:BUFFER-CREATE().      
      nytabh:BUFFER-COPY(orgtabh).
      IF orgtabh:NAME = "PERSONALTAB" THEN DO:
         Guru.GlobalaVariabler:GDPRtyp = "PL".
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
      END.
      IF orgtabh:NAME = "ANVANDARE" THEN DO:
         Guru.GlobalaVariabler:GDPRtyp = "AL".
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE + "," + orgtabh:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.
      END.   
      IF orgtabh:NAME = "EXTRADATA" THEN DO: 
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "BESTEPOST2" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," +  tabfalth:BUFFER-VALUE(1) .
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "BESTEPOST" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tabfalth:BUFFER-VALUE(1) .
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "SUPPERS" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + tabfalth:BUFFER-VALUE(1) .
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "AOPROJ" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "SPFRISK" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "AVAFOR" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "OTBEORD" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE BEGINS "DISPENS" THEN   Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "MARKAG" THEN DO:
            Guru.GlobalaVariabler:GDPRtyp = "M".
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + STRING(orgtabh:BUFFER-FIELD("HUVUDINT"):BUFFER-VALUE).
         END. 
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "FASTLOPNR" OR  orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "MARKFASTANDEL"   THEN DO:
            Guru.GlobalaVariabler:GDPRtyp = "M".
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + orgtabh:BUFFER-FIELD("HUVUDCH"):BUFFER-VALUE.
         END.
         IF orgtabh:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = "VARDFAST" THEN DO:
            Guru.GlobalaVariabler:GDPRtyp = "M".
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + STRING(itabfalth:BUFFER-VALUE(1) ).
         END.  
          
         
         
      END.   
      IF dynajoin = TRUE THEN DO:
         nytabh:BUFFER-COPY(jointabh). 
      END.
      IF dynakollvar = TRUE THEN RUN dynstartkoll_UI.     
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   
   {GDPRLOGGCLIENT.I}
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.
   {UNSETUSE.I}
END PROCEDURE. 
PROCEDURE dynqueryom_UI:
   /*omvänd ordning på buffer-copy jmf med ovan*/ 
   {DYNQUERY.I}    
   DO WHILE orgtabh:AVAILABLE:        
      nytabh:BUFFER-CREATE().      
      IF dynajoin = TRUE THEN DO:
         nytabh:BUFFER-COPY(jointabh). 
      END.
      nytabh:BUFFER-COPY(orgtabh). 
      IF dynakollvar = TRUE THEN RUN dynstartkoll_UI.     
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.
   {UNSETUSE.I}
END PROCEDURE. 
PROCEDURE dynspar_UI:
   {SETUSE.I}
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.
   /*tabell i databasen som efterfrågas*/
   CREATE BUFFER orgtabh FOR TABLE orginaltab IN WIDGET-POOL "dynTemp". 
   /*resultattabell */
   CREATE BUFFER nytabh FOR TABLE extratemptabh IN WIDGET-POOL "dynTemp".
   
   
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   qh:SET-BUFFERS(nytabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   
   DO WHILE nytabh:AVAILABLE: 
      DO TRANSACTION:
         orgtabh:BUFFER-CREATE().      
         orgtabh:BUFFER-COPY(nytabh).         
      END.
      qh:GET-NEXT(NO-LOCK).        
   END.
   IF orgtabh:AVAILABLE THEN orgtabh:BUFFER-RELEASE() NO-ERROR.      
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.
   {UNSETUSE.I}
END PROCEDURE. 
PROCEDURE dynakoll_UI:
   DEFINE OUTPUT PARAMETER dynmusz AS LOGICAL NO-UNDO.

   nyfalth = nytabh:BUFFER-FIELD(kommandonyfalt).
   utvalfalth = utvaltabh:BUFFER-FIELD(kommandoorgfalt).
   IF nyfalth:BUFFER-VALUE NE utvalfalth:BUFFER-VALUE THEN DO:
      nytabh:BUFFER-DELETE(). 
      dynmusz = TRUE.      
   END.
END PROCEDURE. 
PROCEDURE and_UI:
   IF kommandoquery NE " " THEN kommandoquery = kommandoquery + " AND".
END PROCEDURE.
PROCEDURE dyndelete_UI:
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.
   DEFINE INPUT PARAMETER dynajoin AS LOGICAL NO-UNDO.
   {SETUSE.I}  
   /*tabell i databasen som efterfrågas*/
   CREATE BUFFER orgtabh FOR TABLE orginaltab IN WIDGET-POOL "dynTemp". 
   /*dina val*/  
   /*om du har en joinad utsökning*/
   IF dynajoin = TRUE THEN DO:
      CREATE BUFFER jointabh FOR TABLE extrajointemptabh IN WIDGET-POOL "dynTemp".
   END.
   /*query för tabellen som efterfrågas*/
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   IF dynajoin = TRUE THEN DO:
      qh:SET-BUFFERS(jointabh,orgtabh).
   END.
   ELSE qh:SET-BUFFERS(orgtabh).
   
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).  
   DO WHILE orgtabh:AVAILABLE:        
      DO TRANSACTION:
         qh:GET-CURRENT(EXCLUSIVE-LOCK).  
         orgtabh:BUFFER-DELETE().      
      END.
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.
   {UNSETUSE.I}
END PROCEDURE. 

