
 /*------------------------------------------------------------------------
    File        : MtrlJmfPrisDb.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 14:13:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.*.
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder.
USING OpenEdge.Net.URI.


CLASS Modules.Materiel.MtrlJmfPrisDb: 
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO. 
   /*
   DEFINE PUBLIC VARIABLE ControllMtrlJmfPris                  AS Modules.Materiel.MtrlJmfPrisSaldo NO-UNDO. 
   */
   DEFINE PUBLIC VARIABLE ControllMtrlJmfPris                  AS Modules.Materiel.MtrlJmfPris NO-UNDO.
   DEFINE PUBLIC VARIABLE SummaRadbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qH2       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE netth AS HANDLE NO-UNDO.
   DEFINE VARIABLE inkberh AS HANDLE NO-UNDO.
   DEFINE VARIABLE apphproxy AS HANDLE NO-UNDO.
   DEFINE VARIABLE approxcon AS LOGICAL NO-UNDO.
   {JMFPRISMTRL.I}
   {EXTRADATA.I}
   {ESLAGERSTATUSTT.I}
   {DEPMTR.I}
   DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
   &Scoped-define PUBLIC PUBLIC
   {JMFPRISTT.I}
   {ENRJMF.I}
   {JMFPRISTH.I}
   CONSTRUCTOR PUBLIC MtrlJmfPrisDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
   END CONSTRUCTOR.
  
    
  
   
   METHOD PUBLIC VOID ConnectForm():
      DEFINE VARIABLE jmfLevantorTThout AS HANDLE NO-UNDO.
      DEFINE VARIABLE enrjmftth AS HANDLE NO-UNDO.
      {JMFPRISTTH.I}
      IF Guru.Konstanter:appcon THEN DO:
         IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN NETTOMARK.P PERSISTENT SET netth ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:   
         IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN NETTOMARK.P PERSISTENT SET netth.
      END. 
      IF  Guru.Konstanter:appcon THEN DO:
         RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
      END.
      ELSE DO:
         RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
      END.
       IF Guru.Konstanter:appcon THEN DO:
         RUN INKBERAPP.P PERSISTENT SET inkberh ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.                             
      ELSE DO:
         RUN INKBERAPP.P PERSISTENT SET inkberh.   
      END.
      enrjmftth = Guru.GlobalaVariabler:jmfPrisEnrTTh:TABLE-HANDLE.
        
      RUN jmfpris_UI IN netth (INPUT-OUTPUT TABLE-HANDLE enrjmftth, OUTPUT TABLE rubrikTT, OUTPUT TABLE jmfenrTT, OUTPUT TABLE-HANDLE jmfLevantorTThout).
      Guru.GlobalaVariabler:jmfLevantorBuffh  =  jmfLevantorTThout:DEFAULT-BUFFER-HANDLE.
      THIS-OBJECT:ConnectAppproxy().
   END METHOD.
   
    METHOD PUBLIC VOID InitializeTTs():
      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FAVO"                   
      inextradatatemp.HUVUDCH =  Guru.Konstanter:globanv              
      inextradatatemp.HUVUDINT =  ?.   
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
         
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text = STRING(extradatatemp.SOKINT[7]).
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      CREATE BUFFER SummaRadbuffh FOR TABLE MtrlJmfPrisTTh.
      THIS-OBJECT:JmfMtrl(). 
      
   END METHOD.
   METHOD PUBLIC VOID SparaValdLev():
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE Valdlev AS CHARACTER NO-UNDO.
      DEFINE VARIABLE sokvar AS CHARACTER NO-UNDO.
      IF Guru.GlobalaVariabler:LevkodtabInkfalth = ? THEN RETURN.
      
      THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST("WHERE  ENR = ''",NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(1) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev1:VALUE).
      ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(2) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev2:VALUE).
      ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(3) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev3:VALUE).
      ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(4) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev4:VALUE).  
      
      /*ALLA FÅR SAMMA*/
      IF Valdlev NE "" THEN DO:
         queryvar = "FOR EACH " + Guru.GlobalaVariabler:jmfPrisEnrTTh:TABLE. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:jmfPrisEnrTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("OutputLEVKOD"):BUFFER-VALUE = Valdlev. 
            Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("RETUR"):BUFFER-VALUE = TRUE.
            qH:GET-NEXT().
         END.
      END.     
      IF Valdlev = "" THEN DO:
         queryvar = "FOR EACH " + THIS-OBJECT:MtrlJmfPrisTTh:TABLE. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:MtrlJmfPrisTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "" THEN.
            ELSE DO:
               Valdlev = "".
               IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(1) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev1:VALUE).
               ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(2) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev2:VALUE).
               ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(3) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev3:VALUE).
               ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("VALD"):BUFFER-VALUE(4) = TRUE THEN Valdlev = STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev4:VALUE).  
               /*BYTT PÅ NÅGRA ARTIKLAR*/
               IF Valdlev NE "" THEN DO:
                  Guru.GlobalaVariabler:jmfPrisEnrTTh:FIND-FIRST("WHERE  ENR = " + QUOTER(THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE),NO-LOCK) NO-ERROR.
                  Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("OutputLEVKOD"):BUFFER-VALUE = Valdlev.
                  Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("RETUR"):BUFFER-VALUE = TRUE.
               END.     
            END.  
            qH:GET-NEXT().
         END.
        
    /*byter på dem som inte blev bytta ovan*/
         queryvar = "FOR EACH " + Guru.GlobalaVariabler:jmfPrisEnrTTh:TABLE + " WHERE RETUR = FALSE ". 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:jmfPrisEnrTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            sokvar = " WHERE RETUR = TRUE AND ENR = " + QUOTER(Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE) + 
            " AND InputLEVKOD = " +   QUOTER( Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE).
            Guru.GlobalaVariabler:jmfPrisEnrBuffTTh:FIND-FIRST(sokvar,NO-LOCK) NO-ERROR.
            IF Guru.GlobalaVariabler:jmfPrisEnrBuffTTh:AVAILABLE THEN DO:
               Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("OutputLEVKOD"):BUFFER-VALUE =  Guru.GlobalaVariabler:jmfPrisEnrBuffTTh:BUFFER-FIELD("OutputLEVKOD"):BUFFER-VALUE. 
               Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("RETUR"):BUFFER-VALUE = TRUE.
            END.   
            qH:GET-NEXT().
         END.
      END.
      THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST("WHERE  ENR = ''",NO-LOCK) NO-ERROR.
     
   
   END METHOD. 
   METHOD PUBLIC VOID JmfMtrl():
      DEFINE VARIABLE sokvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE ialev AS INTEGER NO-UNDO.
      DEFINE VARIABLE SummaPris AS DECIMAL EXTENT 4 NO-UNDO.
    
      queryvar =  "FOR EACH " + THIS-OBJECT:rubrikTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:rubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         antallev =  antallev + 1.
         sokvar = " WHERE LEVKOD = " +  QUOTER(THIS-OBJECT:rubrikTTh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE).
         queryvar2 =  "FOR EACH " + THIS-OBJECT:jmfenrTTh:TABLE + sokvar.
         qH2 = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:jmfenrTTh,queryvar2).
         qH2:GET-FIRST().
        
         DO WHILE qH2:QUERY-OFF-END = FALSE:
            sokvar = "WHERE ENR =" + QUOTER(THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST(sokvar) NO-ERROR.
            IF THIS-OBJECT:MtrlJmfPrisTTh:AVAILABLE THEN .
            ELSE DO:
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-CREATE. 
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE =  THIS-OBJECT:MtrlJmfPrisTTh:RECID.
                
            END.
           /*
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
            */
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE.
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENRLEV"):BUFFER-VALUE(antallev) = THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("ENRLEV"):BUFFER-VALUE.
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE(antallev) = THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE.
             /*one vill inte ha med Draka i prisjämförelser Mats Johansson 20171020 Lena*/
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(antallev) = THIS-OBJECT:jmfenrTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE.
           
            qH2:GET-NEXT().
         END.  
         qH:GET-NEXT(). 
      END.
    
      /*KÖRS ALLTID*/ 
      IF Guru.GlobalaVariabler:LevkodtabInkfalth NE ? OR Guru.GlobalaVariabler:LevkodtabInkfalth = ? THEN DO:
        
         queryvar = "FOR EACH " + Guru.GlobalaVariabler:jmfPrisEnrTTh:TABLE  + " BY ENR BY InputLEVKOD". 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:jmfPrisEnrTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            sokvar = "WHERE  ENR = " + QUOTER(Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE) +  
            " AND InputLEVKOD = ''" .
            THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST(sokvar,NO-LOCK) NO-ERROR.
            
            IF THIS-OBJECT:MtrlJmfPrisTTh:AVAILABLE THEN DO:
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE.
               IF Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputBENAMNING"):BUFFER-VALUE NE "" THEN THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputBENAMNING"):BUFFER-VALUE.
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
            END.
            
            ELSE DO:
               sokvar = "WHERE  ENR = " + QUOTER(Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE) + " AND InputLEVKOD = " + 
               QUOTER(Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE).
               THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST(sokvar,NO-LOCK) NO-ERROR.
               IF THIS-OBJECT:MtrlJmfPrisTTh:AVAILABLE THEN DO:
                  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE + Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE. 
               END.   
               ELSE DO:
                  sokvar = "WHERE  ENR = " + QUOTER(Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
                  
                  THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST(sokvar,NO-LOCK) NO-ERROR.
                  /*NY SAMMA ENR NY LEV POST EJ SUMMARADEN */
                  IF THIS-OBJECT:MtrlJmfPrisTTh:AVAILABLE THEN DO:
                     THIS-OBJECT:SummaRadbuffh:BUFFER-CREATE. 
                     THIS-OBJECT:SummaRadbuffh:BUFFER-COPY(THIS-OBJECT:MtrlJmfPrisTTh).
                     THIS-OBJECT:SummaRadbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE =  THIS-OBJECT:SummaRadbuffh:RECID.
                     THIS-OBJECT:SummaRadbuffh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
                     THIS-OBJECT:SummaRadbuffh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE.
                     THIS-OBJECT:SummaRadbuffh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
                     IF Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputBENAMNING"):BUFFER-VALUE NE "" THEN  THIS-OBJECT:SummaRadbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputBENAMNING"):BUFFER-VALUE.
                  END.
               END.   
            END.   
     
            qH:GET-NEXT().
         END.
      
      END. 
        
      ELSE DO:
         queryvar = "FOR EACH " + Guru.GlobalaVariabler:jmfPrisEnrTTh:TABLE. 
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(Guru.GlobalaVariabler:jmfPrisEnrTTh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            sokvar = "WHERE  ENR = " + QUOTER(Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE).
            THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST(sokvar,NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:MtrlJmfPrisTTh:AVAILABLE THEN DO:
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE.
               THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE = Guru.GlobalaVariabler:jmfPrisEnrTTh:BUFFER-FIELD("ORDNING"):BUFFER-VALUE.
            END.
            qH:GET-NEXT().
         END.
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:MtrlJmfPrisTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:MtrlJmfPrisTTh,queryvar).
      qH:GET-FIRST().
      ialev = 1.       
      DO WHILE qH:QUERY-OFF-END = FALSE:
         DO WHILE ialev <= antallev:
            IF ialev = 1 THEN THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENHETJMF"):BUFFER-VALUE = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENHET"):BUFFER-VALUE(ialev).
            SummaPris[ialev] = SummaPris[ialev] +  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) * THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE.
           
            IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) = 0 THEN.
            ELSE DO:
               
               IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:  
                  /*obs! finns på två ställen*/
                  IF ialev = 1 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev1:VALUE)),NO-LOCK) NO-ERROR.
                  IF ialev = 2 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev2:VALUE)),NO-LOCK) NO-ERROR.
                  IF ialev = 3 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev3:VALUE)),NO-LOCK) NO-ERROR.
                  IF ialev = 4 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev4:VALUE)),NO-LOCK) NO-ERROR.
                  IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
                     /*Anders Olsson Elpool i Umeå AB  2 jun 2022 13:42:44 
                     rad korr endast Rexel 
                     */
                     /*
                     IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Onninen" THEN  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) = 0.8 *  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev).
                     IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Elektroskandia" THEN  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) = 0.8 *  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev).
                     IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Selga" THEN  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) = 0.8 *  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev).
                     IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Ahlsell" THEN  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) = 0.8 *  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev).
              */
                     IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Rexel" THEN  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) = 0.9375 *  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev).
                
                  END.
               END. 
               IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = 0 THEN THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev).
             
               ELSE IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev) < THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE THEN DO:
                  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(ialev). 
               END.   
            END.
            ialev = ialev + 1.   
         END.  
         ialev = 1. 
         qH:GET-NEXT(). 
      END.
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         CREATE enrjmf.
         enrjmf.ENR = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE.
         IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = 0 THEN THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = -1.
         qH:GET-NEXT(). 
      END.  
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:  
         ialev = 1.
         DO WHILE ialev <= antallev: 
            IF ialev = 1 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev1:VALUE)),NO-LOCK) NO-ERROR.
            IF ialev = 2 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev2:VALUE)),NO-LOCK) NO-ERROR.
            IF ialev = 3 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev3:VALUE)),NO-LOCK) NO-ERROR.
            IF ialev = 4 THEN Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD  = " + QUOTER(STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev4:VALUE)),NO-LOCK) NO-ERROR.
            IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
               /*obs! finns på två ställen*/
               IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Onninen" THEN SummaPris[ialev] = 0.8 * SummaPris[ialev].
               IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Elektroskandia" THEN SummaPris[ialev] = 0.8 * SummaPris[ialev].
               IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Selga" THEN SummaPris[ialev] = 0.8 * SummaPris[ialev].
               IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Ahlsell" THEN SummaPris[ialev] = 0.8 * SummaPris[ialev].
               IF Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE BEGINS "Rexel" THEN SummaPris[ialev] = 0.75 * SummaPris[ialev].
            END.
            ialev = ialev + 1.
         END.    
      END. 
    
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-CREATE. 
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE =  THIS-OBJECT:MtrlJmfPrisTTh:RECID.
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "Summa".
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(1) =  SummaPris[1].
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(2) =  SummaPris[2].
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(3) =  SummaPris[3].
      THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("PRIS"):BUFFER-VALUE(4) =  SummaPris[4].
      
      ialev = 1.
      DO WHILE ialev <= antallev:
         IF SummaPris[ialev] > 0 THEN DO: 
            IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = 0 THEN  THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = SummaPris[ialev].
            ELSE IF SummaPris[ialev] < THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE THEN DO:
                THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("MINPRIS"):BUFFER-VALUE = SummaPris[ialev]. 
            END.   
             
         END.
         ialev = ialev + 1.  
      END.       
   END METHOD.
   METHOD PUBLIC VOID NuQ(INPUT aa AS CHARACTER):
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:MtrlJmfPrisTTh,queryvar).
      qH:GET-FIRST().
             
      DO WHILE qH:QUERY-OFF-END = FALSE:
         MESSAGE aa
         THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("InputLEVKOD"):BUFFER-VALUE     THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
         
         VIEW-AS ALERT-BOX.
         qH:GET-NEXT().
      END.
   END METHOD.
   
   METHOD PUBLIC INTEGER Levkod(INPUT levid AS CHARACTER):
      IF STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev1:VALUE) = levid THEN RETURN 1.
         ELSE IF STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev2:VALUE) = levid THEN RETURN 2.
         ELSE IF STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev3:VALUE) = levid THEN RETURN 3.
         ELSE IF STRING(THIS-OBJECT:ControllMtrlJmfPris:comboLev4:VALUE) = levid THEN RETURN 4.
         ELSE RETURN 0.
   END METHOD.
   METHOD PUBLIC VOID ConnectAppproxy():
      DEFINE VARIABLE conappproxy AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
      IF Guru.Konstanter:AppSpringSet[1] = "XVSAB" OR Guru.Konstanter:AppSpringSet[1] = "XVSABPRO" THEN DO:
         conappproxy = REPLACE(Guru.Konstanter:conappvar,"appvsab","appvsabproxy").
         
         CREATE SERVER apphproxy.
         approxcon = apphproxy:CONNECT(conappproxy,{APPCON1.i},{APPCON2.i},Guru.Konstanter:AppSpringSet[1]) NO-ERROR.
         IF NOT approxcon THEN DO:
            MESSAGE 
            ERROR-STATUS:NUM-MESSAGES 
            " fel uppkom vid anslutningen." SKIP 
            "Det går ej köra Saldofunktionen i Guru." SKIP
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
            RETURN.
         END.
         
         
      END.   
   END METHOD.
   METHOD PUBLIC VOID SaldoES():
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text  = "Elektroskandia".
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVNAMN = " + QUOTER(THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text),NO-LOCK) NO-ERROR.
      
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         pos = THIS-OBJECT:Levkod(Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE).
         IF pos = 0 THEN RETURN.
         THIS-OBJECT:KontrollEnrMtrl(pos).
      END.
      ELSE RETURN.
      /*
      IF approxcon AND (Guru.Konstanter:AppSpringSet[1] = "XVSAB" OR Guru.Konstanter:AppSpringSet[1] = "XVSABPRO") THEN DO:
         RUN ESLAGERSTATUS.P ON apphproxy (INPUT-OUTPUT TABLE eslagersatatustt).
      END.   
      ELSE
      */ 
      IF Guru.Konstanter:AppSpringSet[1] = "VSAB" OR Guru.Konstanter:AppSpringSet[1] = "VSABPRO" THEN DO:
         IF Guru.Konstanter:appcon THEN RUN ESLAGERSTATUS.P ON Guru.Konstanter:apphand (INPUT-OUTPUT TABLE eslagersatatustt).
         ELSE RUN ESLAGERSTATUS.P (INPUT-OUTPUT TABLE eslagersatatustt).
      END.  
      ELSE RUN ESLAGERSTATUS.P (INPUT-OUTPUT TABLE eslagersatatustt).
      THIS-OBJECT:LoadSaldo(pos).
      
   END METHOD.
   METHOD PUBLIC VOID SaldoON():
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text  = "Onninen".
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVNAMN = " + QUOTER(THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         pos = THIS-OBJECT:Levkod(Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE).
         IF pos = 0 THEN RETURN.
         THIS-OBJECT:KontrollEnrMtrl(pos).
      END.   
      ELSE RETURN.
     
      IF Guru.Konstanter:AppSpringSet[1] = "VSAB" OR Guru.Konstanter:AppSpringSet[1] = "VSABPRO" THEN DO:
         IF Guru.Konstanter:appcon THEN RUN OnniLAGERSTATUS2.p ON Guru.Konstanter:apphand (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
         ELSE RUN  OnniLAGERSTATUS2.p (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
      END.     
      ELSE RUN  OnniLAGERSTATUS2.p (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
      THIS-OBJECT:LoadSaldo(pos).
     
   END METHOD.
   METHOD PUBLIC VOID SaldoRE():
      DEFINE VARIABLE levn AS CHARACTER NO-UNDO.
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text  = "Rexel".
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVNAMN = " + QUOTER(THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         pos = THIS-OBJECT:Levkod(Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE).
         IF pos = 0 THEN RETURN.
         THIS-OBJECT:KontrollEnrMtrl(pos).
      END.   
      ELSE DO:
         levn = "Selga".
         Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVNAMN BEGINS " + QUOTER(levn),NO-LOCK) NO-ERROR.
         IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
            pos = THIS-OBJECT:Levkod(Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE).
            IF pos = 0 THEN RETURN.
            THIS-OBJECT:KontrollEnrMtrl(pos).
         END.
         ELSE RETURN.
  
      END.
      
      IF Guru.Konstanter:AppSpringSet[1] = "VSAB" OR Guru.Konstanter:AppSpringSet[1] = "VSABPRO" THEN DO:
         IF Guru.Konstanter:appcon THEN RUN SELGALAGERSTATUS2.p ON Guru.Konstanter:apphand (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
         ELSE RUN SELGALAGERSTATUS2.p (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
      END.     
      ELSE RUN SELGALAGERSTATUS2.p (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
     
      THIS-OBJECT:LoadSaldo(pos).
   END METHOD.
   
   METHOD PUBLIC VOID SaldoAHL():
      DEFINE VARIABLE pos AS INTEGER NO-UNDO.
      THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text  = "Ahlsell".
      Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVNAMN = " + QUOTER(THIS-OBJECT:ControllMtrlJmfPris:Levnamn:GuruText:Text),NO-LOCK) NO-ERROR.
      IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
         pos = THIS-OBJECT:Levkod(Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE).
         IF pos = 0 THEN RETURN.
         THIS-OBJECT:KontrollEnrMtrl(pos).
      END.   
      ELSE RETURN.
     
      IF Guru.Konstanter:AppSpringSet[1] = "VSAB" OR Guru.Konstanter:AppSpringSet[1] = "VSABPRO" THEN DO:
         /*
         IF Guru.Konstanter:appcon THEN RUN AHLSELLAGERSTATUS.P ON Guru.Konstanter:apphand (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
         ELSE*/  RUN AHLSELLAGERSTATUS.P (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
      END.     
      ELSE DO:
/*         THIS-OBJECT:AhlsellSaldo().*/
         /*
         RUN AHLSELLAGERSTATUS.P  (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
         */
        /* RUN TestAhlsell.p.*/
        RUN AHLSELLAGERSTATUS.P  (INPUT THIS-OBJECT:ControllMtrlJmfPris:RexPostnr:GuruText:Text, INPUT-OUTPUT TABLE eslagersatatustt).
      END.
      THIS-OBJECT:LoadSaldo(pos).
     
   END METHOD.

   /*
   METHOD PUBLIC VOID AhlsellSaldo():
      DEFINE VARIABLE lRetOK AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE oRequest     AS IHttpRequest         NO-UNDO.
      DEFINE VARIABLE oResponse    AS IHttpResponse        NO-UNDO.
      DEFINE VARIABLE oURI         AS URI                  NO-UNDO.
      DEFINE VARIABLE oRequestBody AS OpenEdge.Core.String NO-UNDO.
      DEFINE VARIABLE hXMLHandle   AS HANDLE               NO-UNDO.
      DEFINE VARIABLE lcXML        AS LONGCHAR             NO-UNDO.
      DEFINE VARIABLE chelp AS CHARACTER NO-UNDO FORMAT "x(64)".
      DEFINE VARIABLE chelpi AS INTEGER NO-UNDO.
      DEFINE VARIABLE chelpislut AS INTEGER NO-UNDO.
      FOR EACH eslagersatatustt WHERE NO-LOCK:
       
         oRequestBody =  NEW OpenEdge.Core.String(
                         '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
                          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                          xmlns:xsd="http://www.w3.org/2001/XMLSchema">
                          <soap:Body>
                          <GetQuantity xmlns="http://tempuri.org/">
                          <articleNo>' + eslagersatatustt.UTANEENR + '</articleNo>
                          </GetQuantity>
                          </soap:Body>
                          </soap:Envelope>'  ).
         oURI = URI:Parse("http://perimeter2.ahlsell.com/inbound/soap/stockrequest").
         oRequest = RequestBuilder:Post(oUri, oRequestBody)
                               :ContentType('text/xml;charset=UTF-8')
                               :AcceptAll()
                               :AddHeader('SOAPAction', 'http://tempuri.org/Add';)
                               :Request.
         oResponse = ClientBuilder:Build()
         :Client:Execute(oRequest).
       
         IF oResponse:StatusCode <> 200 THEN DO:
            MESSAGE "http error: " oResponse:StatusCode VIEW-AS ALERT-BOX. 
            RETURN ERROR "Request Error: " + STRING(oResponse:StatusCode).
         END.
         ELSE DO:
            hXMLHandle = CAST(oResponse:Entity,WidgetHandle):Value.
            hXMLHandle:SAVE('LONGCHAR',lcXML).
            chelp = SUBSTRING(STRING(lcXML),15 + INDEX(STRING(lcXML),"EnoughQuantity"),1).
            IF chelp = "J" THEN DO:
               chelpi = INDEX(STRING(lcXML),"InStockQuantity").
               chelpislut = INDEX(STRING(lcXML),"</InStockQuantity",chelpi) - 1. 
               chelp = SUBSTRING(STRING(lcXML),16 + chelpi,chelpislut - (15 + chelpi)).
               eslagersatatustt.ANTALILAGER = DECIMAL(chelp).
               IF eslagersatatustt.ANTALILAGER >= eslagersatatustt.ANTAL THEN eslagersatatustt.SVAR = "OK".
               ELSE eslagersatatustt.SVAR = "För få i lager".
           
               /*
               EnoughQuantity 
               InStockQuantity
               */
                
            END.
            ELSE DO:
               eslagersatatustt.SVAR = "Lagerförs inte!".
            END.
         END.  
      END.
         
   END METHOD.
   */
   
   METHOD PUBLIC VOID LoadSaldo(pos AS INTEGER):
      FOR EACH eslagersatatustt WHERE NO-LOCK:
         THIS-OBJECT:MtrlJmfPrisTTh:FIND-FIRST("WHERE TTRECID = " + STRING(eslagersatatustt.TTRECID) ,NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:MtrlJmfPrisTTh:AVAILABLE THEN DO:
            
            ASSIGN 
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANMARK"):BUFFER-VALUE(pos)  = eslagersatatustt.SVAR
            THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("LAGERSALDO"):BUFFER-VALUE(pos) = eslagersatatustt.ANTALILAGER.
             
           
         END.
      END.
   END METHOD.
   METHOD PUBLIC VOID KontrollEnrMtrl(pos AS INTEGER):
      EMPTY TEMP-TABLE eslagersatatustt NO-ERROR. 
      queryvar =  "FOR EACH " + THIS-OBJECT:MtrlJmfPrisTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:MtrlJmfPrisTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENR"):BUFFER-VALUE = "" THEN.
         ELSE DO:
            CREATE eslagersatatustt.
            ASSIGN
            
            eslagersatatustt.ENR = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ENRLEV"):BUFFER-VALUE(pos)
            eslagersatatustt.BENAMNING = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE
            eslagersatatustt.ANTAL = THIS-OBJECT:MtrlJmfPrisTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE
            eslagersatatustt.TTRECID = THIS-OBJECT:MtrlJmfPrisTTh:RECID.
            eslagersatatustt.UTANEENR = eslagersatatustt.ENR. 
            IF eslagersatatustt.UTANEENR BEGINS "E" THEN eslagersatatustt.UTANEENR = SUBSTRING(eslagersatatustt.UTANEENR,2).
         END.     
         qH:GET-NEXT(). 
      END.  
      RUN KontrollEnrMtrl_UI IN inkberh (INPUT-OUTPUT TABLE eslagersatatustt).
   END METHOD.
  
   METHOD PUBLIC VOID SaldoPoster():
      
   END METHOD.
   METHOD PUBLIC VOID BytLev():
      DEFINE VARIABLE enrjmftth AS HANDLE NO-UNDO.
      EMPTY TEMP-TABLE rubriktt NO-ERROR.
      
      THIS-OBJECT:RubrikHmt(STRING(ControllMtrlJmfPris:comboLev1:VALUE),1).
      THIS-OBJECT:ControllMtrlJmfPris:GridJmfPris:SetGroupHeader(0,"Leverantor1",rubriktt.LEVNAMN).
      THIS-OBJECT:RubrikHmt(STRING(ControllMtrlJmfPris:comboLev2:VALUE),2).
      THIS-OBJECT:ControllMtrlJmfPris:GridJmfPris:SetGroupHeader(0,"Leverantor2",rubriktt.LEVNAMN).
      THIS-OBJECT:RubrikHmt(STRING(ControllMtrlJmfPris:comboLev3:VALUE),3).
      THIS-OBJECT:ControllMtrlJmfPris:GridJmfPris:SetGroupHeader(0,"Leverantor3",rubriktt.LEVNAMN).
      THIS-OBJECT:RubrikHmt(STRING(ControllMtrlJmfPris:comboLev4:VALUE),4).
      THIS-OBJECT:ControllMtrlJmfPris:GridJmfPris:SetGroupHeader(0,"Leverantor4",rubriktt.LEVNAMN).
      THIS-OBJECT:MtrlJmfPrisTTh:EMPTY-TEMP-TABLE ().
      enrjmftth = Guru.GlobalaVariabler:jmfPrisEnrTTh:TABLE-HANDLE.  
      RUN jmfprisIgen_UI IN  netth (INPUT-OUTPUT TABLE-HANDLE enrjmftth, INPUT TABLE rubriktt, OUTPUT TABLE jmfenrTT).
      THIS-OBJECT:MtrlJmfPrisTTh:EMPTY-TEMP-TABLE().
      THIS-OBJECT:ControllMtrlJmfPris:GridJmfPris:GuruReopen().
     
      THIS-OBJECT:JmfMtrl().
      
        
   END METHOD.
   METHOD PUBLIC VOID RubrikHmt(INPUT cmblev AS CHARACTER, INPUT max4 AS INTEGER ):
      IF cmblev  NE "" THEN DO:
         Guru.GlobalaVariabler:jmfLevantorBuffh:FIND-FIRST("WHERE  LEVKOD = " + QUOTER(cmblev),NO-LOCK) NO-ERROR.
         IF Guru.GlobalaVariabler:jmfLevantorBuffh:AVAILABLE THEN DO:
            CREATE rubriktt.
            ASSIGN
            rubrikTT.ORDNING = max4
            rubriktt.LEVKOD =  Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE
            rubriktt.LEVNAMN = Guru.GlobalaVariabler:jmfLevantorBuffh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE.
          
         END.
      END.   
      
   END METHOD.
   DESTRUCTOR MtrlJmfPrisDb():
     
      EMPTY TEMP-TABLE MtrlJmfPrisTT  NO-ERROR. 
      IF VALID-HANDLE(netth) THEN DELETE PROCEDURE netth NO-ERROR.
      netth = ?.
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.  
      edataapph = ?.
      IF VALID-HANDLE(inkberh) THEN DELETE PROCEDURE inkberh NO-ERROR.
      inkberh = ?.
     
      IF approxcon THEN DO:
         IF apphproxy:CONNECTED()  THEN approxcon = apphproxy:DISCONNECT() NO-ERROR.
      END.   
      approxcon = FALSE.
      IF apphproxy NE ? THEN DELETE OBJECT apphproxy NO-ERROR.
      apphproxy = ?.
      
      
      
      
   END DESTRUCTOR.
   
END CLASS.

