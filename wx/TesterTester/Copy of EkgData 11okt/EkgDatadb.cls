 
 /*------------------------------------------------------------------------
    File        : KalkylAdmDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 14:13:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.EkgData.EkgDatadb: 
   {EKGKAT.I}
   
   {EKGPRODATA.I}
   {TIDUTTT.I}
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE ControlShell               AS Modules.EkgData.EkgShell NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg               AS Modules.EkgData.EkgWinLg NO-UNDO.
   DEFINE PUBLIC VARIABLE Root                       AS Guru.Root                NO-UNDO.
   
   CONSTRUCTOR PUBLIC EkgDatadb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
   END CONSTRUCTOR.
   METHOD PUBLIC VOID Connect():
      IF Guru.Konstanter:appcon THEN DO:
         RUN EKGAPPDS.p PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN EKGAPPDS.p PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).
      END. 
   END METHOD.
   METHOD PUBLIC VOID InitializeTTs():
      {EKGTTH.i}      
   END METHOD.
   METHOD PUBLIC VOID hmt(): 
     MESSAGE "i db"
     VIEW-AS ALERT-BOX.
   END METHOD.
   METHOD PUBLIC VOID EkgDef():
     
      THIS-OBJECT:Root:DatabaseManager:Global:FetchDefaultValues("EkgShell",Guru.GlobalaVariabler:GuruDefaultAnv,0,"").
   END METHOD.
   
   /*Tracking för allt*/
   METHOD PUBLIC VOID EKGTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgsubkatTT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgresursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgresursprisTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgpaslagTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgnivaTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgresursnivaTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgresurspaslagTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgregelTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgmtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp5TT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp5resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp4TT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp4resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp4frekvensTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp3TT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp3arbkodTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp3resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp3frekvensTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2TT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2arbkodTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2frekvensTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2mtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp1TT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp1arbkodTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp1frekvensTT:TRACKING-CHANGES = onoff.     
   END METHOD.
  
   
   /*Tracking för EKGResursDS*/
   METHOD PUBLIC VOID EKGResursTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgresursTT:TRACKING-CHANGES = onoff.  
   END METHOD.
   
   /*Tracking för EKGSubKatDS*/
   METHOD PUBLIC VOID EKGSubKatTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgsubkatTT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgnivaTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgmtrlTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgpaslagTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgregelTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGKatalogResursDS*/
   METHOD PUBLIC VOID EKGKatalogResursTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgresursprisTT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgresursnivaTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgresurspaslagTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGP5DS*/
   METHOD PUBLIC VOID EKGP5Tracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgp5TT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgp5resursTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGP4DS*/
   METHOD PUBLIC VOID EKGP4Tracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgp4TT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgp4resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp4frekvensTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGP3DS*/
   METHOD PUBLIC VOID EKGP3Tracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgp3TT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgp3resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp3frekvensTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGP2DS*/
   METHOD PUBLIC VOID EKGP2Tracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgp2TT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgp2resursTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2frekvensTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp2mtrlTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGP1DS*/
   METHOD PUBLIC VOID EKGP1Tracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgp1TT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgp1frekvensTT:TRACKING-CHANGES = onoff.
   END METHOD.
   
   /*Tracking för EKGArbKoderDS*/
   METHOD PUBLIC VOID EKGArbKoderTracking(INPUT onoff AS LOGICAL):
      TEMP-TABLE ekgp1arbkodTT:TRACKING-CHANGES = onoff.  
      TEMP-TABLE ekgp2arbkodTT:TRACKING-CHANGES = onoff.
      TEMP-TABLE ekgp3arbkodTT:TRACKING-CHANGES = onoff.   
   END METHOD.
   
   METHOD PUBLIC VOID LaddaKataloger():  
      RUN LaddaKataloger IN AppServerHandle (OUTPUT TABLE ekgsubkatalogerTT).      
   END METHOD.
   
   METHOD PUBLIC VOID LaddaKatalog(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaKatalog IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGSubKatDS).
      THIS-OBJECT:EKGSubKatTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID KopKatalog(INPUT-OUTPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):      
      RUN KopKatalog IN AppServerHandle (INPUT-OUTPUT ekgsubidvar, INPUT ebrkatvar).            
   END METHOD.
   
   METHOD PUBLIC VOID BortKatalog(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER): 
      THIS-OBJECT:EKGTracking(FALSE).
      DATASET EKGSubKatDS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGKatalogResursDS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGP5DS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGP4DS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGP3DS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGP2DS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGP1DS:EMPTY-DATASET() NO-ERROR.
      DATASET EKGArbKoderDS:EMPTY-DATASET() NO-ERROR.
               
      RUN BortKatalog IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar).
                  
   END METHOD.
   
   METHOD PUBLIC VOID LaddaResurser():
      RUN LaddaResurser IN AppServerHandle (OUTPUT DATASET EKGResursDS).
      THIS-OBJECT:EKGResursTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaKatalogResurser(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaKatalogResurser IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGKatalogResursDS).
      THIS-OBJECT:EKGKatalogResursTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaArbKoder(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaArbKoder IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGArbKoderDS).
      THIS-OBJECT:EKGArbKoderTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP5(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaP5 IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGP5DS).
      THIS-OBJECT:EKGP5Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP4(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaP4 IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGP4DS).
      THIS-OBJECT:EKGP4Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP3(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaP3 IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGP3DS).
      THIS-OBJECT:EKGP3Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP2(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaP2 IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGP2DS).
      THIS-OBJECT:EKGP2Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP1(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      RUN LaddaP1 IN AppServerHandle (INPUT ekgsubidvar, INPUT ebrkatvar, OUTPUT DATASET EKGP1DS).
      THIS-OBJECT:EKGP1Tracking(TRUE).
   END METHOD.  
   
   /*VF = VISNINGSFÄLT METHOD används inte*/
   METHOD PUBLIC VOID LaddaAllaVF(INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
    THIS-OBJECT:LaddaResurserPriserVF().
    THIS-OBJECT:LaddaP5VFresurs().
  
    THIS-OBJECT:LaddaP4VFfrek().       
    THIS-OBJECT:LaddaP4VFresurs().   
    THIS-OBJECT:LaddaP3VFfrek().    
    THIS-OBJECT:LaddaP3VFresurs(ebrkatvar).    
    THIS-OBJECT:LaddaP2VFfrek().    
    THIS-OBJECT:LaddaP2VFresurs(ebrkatvar).
    THIS-OBJECT:LaddaP2VFmtrl().    
    THIS-OBJECT:LaddaP1VFfrek().       
   END METHOD.
   
   METHOD PUBLIC LOGICAL FinnsArbkodImport(INPUT nivvar AS CHARACTER, INPUT arbkodvar AS CHARACTER):
      IF nivvar = "P3" THEN DO:
         FIND FIRST importP3TTbuff WHERE importP3TTbuff.P3ARBKOD = arbkodvar AND importP3TTbuff.P3LOPNR = 0 NO-LOCK NO-ERROR.
         IF AVAILABLE importP3TTbuff THEN RETURN TRUE.
         ELSE RETURN FALSE.
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID ResurspaslagsUpd(): 
      FOR EACH ekgresurspaslagTT WHERE ekgresurspaslagTT.RESURSNR = 0 NO-LOCK:
         BUFFER-COPY ekgresursprisTT TO ekgresurspaslagTT.
         ASSIGN 
         ekgresurspaslagTT.RESURSBENAMNING = ekgresursprisTT.RESURSBENAMNING
         ekgresurspaslagTT.PASLAGSBENAMNING = ekgpaslagTT.BENAMNING.                                      
      END.
   END METHOD.
   METHOD PUBLIC VOID P2ResursUpd(): 
      FOR EACH ekgp2resursTT WHERE ekgp2resursTT.P2arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp2TT  EXCEPT EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp2resursTT.
         ASSIGN 
         ekgp2resursTT.P2BENAMNING = ekgp2TT.BENAMNING
         ekgp2resursTT.P2ENHET    = ekgp2TT.ENHET  
         ekgp2resursTT.RESURSBENAMNING = ekgresursForTT.BENAMNING 
         ekgp2resursTT.RESURSENHET = ekgresursForTT.ENHET
         ekgp2resursTT.RESURSPRIS = ekgresursForTT.PRIS 
         ekgp2resursTT.EA = ekgresursForTT.EA.    
         THIS-OBJECT:LaddaP2VFresursSingel(ControlWinLg:ebrkatvar).        
      END.
   END METHOD.
   METHOD PUBLIC VOID P2MtrlUpd(): 
      FOR EACH ekgp2mtrlTT WHERE ekgp2mtrlTT.P2arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp2TT  EXCEPT EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp2mtrlTT.
         ASSIGN 
         ekgp2mtrlTT.P2BENAMNING = ekgp2TT.BENAMNING.
         ekgp2mtrlTT.MTRLBENAMNING = ekgmtrlTT.BENAMNING.      
         THIS-OBJECT:LaddaP2VFmtrlsingel().                
      END.
      
   END METHOD. 
   
   METHOD PUBLIC VOID P3ResursUpd(): 
      FOR EACH ekgp3resursTT WHERE ekgp3resursTT.P3arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp3TT  EXCEPT EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp3resursTT.
         ASSIGN 
         ekgp3resursTT.P3BENAMNING = ekgp3TT.BENAMNING
         ekgp3resursTT.P3ENHET    = ekgp3TT.ENHET  
         ekgp3resursTT.RESURSBENAMNING = ekgresursForTT.BENAMNING 
         ekgp3resursTT.RESURSENHET = ekgresursForTT.ENHET
         ekgp3resursTT.RESURSPRIS = ekgresursForTT.PRIS 
         ekgp3resursTT.EA = ekgresursForTT.EA.    
         THIS-OBJECT:LaddaP3VFresursSingel(ControlWinLg:ebrkatvar).        
      END.
   END METHOD. 
   
   METHOD PUBLIC VOID P1FrekvensUpd():
      FOR EACH ekgp1frekvensTT WHERE ekgp1frekvensTT.P1arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp1TT  EXCEPT MONTTIM EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp1frekvensTT.
         ASSIGN 
         ekgp1frekvensTT.P1BENAMNING = ekgp1TT.BENAMNING
         ekgp1frekvensTT.P2BENAMNING = ekgp2TT.BENAMNING. 
         THIS-OBJECT:LaddaP1VFfrekSingel().                        
      END.
   END METHOD.
   
   METHOD PUBLIC VOID P2FrekvensUpd():
      FOR EACH ekgp2frekvensTT WHERE ekgp2frekvensTT.P2arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp2TT  EXCEPT MONTTIM EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp2frekvensTT.
         ASSIGN 
         ekgp2frekvensTT.P2BENAMNING = ekgp2TT.BENAMNING
         ekgp2frekvensTT.P3BENAMNING = ekgp3TT.BENAMNING. 
         THIS-OBJECT:LaddaP2VFfrekSingel().                        
      END.
   END METHOD.
   
   METHOD PUBLIC VOID P3FrekvensUpd():
      FOR EACH ekgp3frekvensTT WHERE ekgp3frekvensTT.P3arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp3TT  EXCEPT MONTTIM EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp3frekvensTT.
         ASSIGN 
         ekgp3frekvensTT.P3BENAMNING = ekgp3TT.BENAMNING
         ekgp3frekvensTT.P4BENAMNING = ekgp4TT.BENAMNING. 
         THIS-OBJECT:LaddaP3VFfrekSingel().                        
      END.
   END METHOD. 
   
   METHOD PUBLIC VOID P4FrekvensUpd(): 
      FOR EACH ekgp4frekvensTT WHERE ekgp4frekvensTT.P4arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp4TT  EXCEPT MONTTIM EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp4frekvensTT.
         ASSIGN 
         ekgp4frekvensTT.P4BENAMNING = ekgp4TT.BENAMNING
         ekgp4frekvensTT.P5BENAMNING = ekgp5TT.BENAMNING. 
         THIS-OBJECT:LaddaP4VFfrekSingel().                                
      END.
   END METHOD. 
   METHOD PUBLIC VOID P4ResursUpd(): 
      FOR EACH ekgp4resursTT WHERE ekgp4resursTT.P4arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp4TT  EXCEPT EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp4resursTT.
         ASSIGN 
         ekgp4resursTT.P4BENAMNING = ekgp4TT.BENAMNING
         ekgp4resursTT.P4ENHET    = ekgp4TT.ENHET  
         ekgp4resursTT.RESURSBENAMNING = ekgresursForTT.BENAMNING 
         ekgp4resursTT.RESURSENHET = ekgresursForTT.ENHET
         ekgp4resursTT.RESURSPRIS = ekgresursForTT.PRIS 
         ekgp4resursTT.EA = ekgresursForTT.EA.    
         THIS-OBJECT:LaddaP4VFresursSingel().        
      END.
   END METHOD. 
   
   
   METHOD PUBLIC VOID P5ResursUpd(): 
      FOR EACH ekgp5resursTT WHERE ekgp5resursTT.P5arbkod = " " NO-LOCK:
         BUFFER-COPY ekgp5TT  EXCEPT EA ARBETEKOST MASKINKOST OVRIGT EAMASK EAOVRIGT TO ekgp5resursTT.
         ASSIGN 
         ekgp5resursTT.P5BENAMNING = ekgp5TT.BENAMNING
         ekgp5resursTT.P5ENHET    = ekgp5TT.ENHET  
         ekgp5resursTT.RESURSBENAMNING = ekgresursForTT.BENAMNING 
         ekgp5resursTT.RESURSENHET = ekgresursForTT.ENHET
         ekgp5resursTT.RESURSPRIS = ekgresursForTT.PRIS 
         ekgp5resursTT.EA = ekgresursForTT.EA.
         THIS-OBJECT:LaddaP5VFresursSingel().        
      END.
   END METHOD. 
   
   METHOD PUBLIC VOID ResursprisUpd():          
      FOR EACH ekgresursprisTT WHERE ekgresursprisTT.REGELID = 0 NO-LOCK:
         BUFFER-COPY ekgresursTT  TO ekgresursprisTT.
         ASSIGN 
         ekgresursprisTT.REGELID = 3
         ekgresursprisTT.RESURSBENAMNING  = ekgresursTT.BENAMNING          
         ekgresursprisTT.RESURSENHET = ekgresursTT.ENHET
         ekgresursprisTT.RESURSAKTIV = ekgresursTT.AKTIV.
         FIND FIRST ekgregelTT WHERE ekgregelTT.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.
         ASSIGN
         ekgresursprisTT.REGELANMARKNING = ekgregelTT.ANMARKNING.                      
      END.        
   END METHOD.
   
   METHOD PUBLIC VOID LaddaResurserPriserVF():
      FOR EACH ekgresursprisTT NO-LOCK:
         FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgresursprisTT.RESURSNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgresursprisTT.RESURSBENAMNING = ekgresursTT.BENAMNING
         ekgresursprisTT.RESURSENHET = ekgresursTT.ENHET
         ekgresursprisTT.RESURSAKTIV = ekgresursTT.AKTIV.
         FIND FIRST ekgregelTT WHERE ekgregelTT.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.
         ASSIGN
         ekgresursprisTT.REGELANMARKNING = ekgregelTT.ANMARKNING.    
      END.      
      /*Koppla resursbenämning och påslagsbenämning till ekgresurpaslagTT*/
      FOR EACH ekgresurspaslagTT :
         FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgresurspaslagTT.RESURSNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgresurspaslagTT.RESURSBENAMNING = ekgresursTT.BENAMNING.
         FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = ekgresurspaslagTT.PASLAGNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgresurspaslagTT.PASLAGSBENAMNING = ekgpaslagTT.BENAMNING NO-ERROR.
      END.   
      FOR EACH ekgresursnivatt:    
         FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgresursnivatt.RESURSNR NO-LOCK NO-ERROR.
         IF AVAILABLE ekgresursTT THEN  ASSIGN  ekgresursnivatt.RESURSBENAMNING = ekgresursTT.BENAMNING.
      END.   
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP5VFresurs():
     THIS-OBJECT:EKGP5Tracking(FALSE).
     FOR EACH ekgp5resursTT NO-LOCK:
         FIND FIRST ekgP5TT WHERE ekgP5TT.P5ARBKOD = ekgp5resursTT.P5ARBKOD AND ekgP5TT.P5LOPNR = ekgp5resursTT.P5LOPNR NO-LOCK NO-ERROR.           
         THIS-OBJECT:LaddaP5VFresursSingel().  
      END.
      THIS-OBJECT:EKGP5Tracking(TRUE).
   END METHOD.  
   
   
   METHOD PUBLIC VOID LaddaP5VFresursSingel():
      FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgp5resursTT.RESURSNR NO-LOCK NO-ERROR.
      ASSIGN
      ekgp5resursTT.P5BENAMNING =  ekgP5TT.BENAMNING
      ekgp5resursTT.P5ENHET = ekgP5TT.ENHET
      ekgp5resursTT.RESURSBENAMNING =  ekgresursTT.BENAMNING.
      ekgp5resursTT.RESURSENHET = ekgresursTT.ENHET.
      FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp5resursTT.RESURSNR NO-LOCK NO-ERROR.
      FIND FIRST ekgresurspaslagTT WHERE ekgresurspaslagTT.RESURSNR = ekgresursprisTT.RESURSNR  NO-LOCK NO-ERROR.
      FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = ekgresurspaslagTT.PASLAGNR NO-LOCK NO-ERROR.           
      IF AVAILABLE ekgpaslagTT THEN DO:
         ASSIGN
         ekgp5resursTT.RESURSPRIS =  ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         ekgp5resursTT.EA = ekgresursprisTT.EA.
      END.
      ELSE DO:
        ASSIGN
        ekgp5resursTT.RESURSPRIS =  ekgresursprisTT.PRIS.
        ekgp5resursTT.EA = ekgresursprisTT.EA.
     END.
      IF ekgresursprisTT.REGELID = 1 THEN DO:
         /*MONTÖR EA*/
         ASSIGN.
         ekgp5resursTT.EAMONT = ekgp5resursTT.ANTAL.                     
         IF AVAILABLE ekgpaslagTT THEN DO:
            ASSIGN.                              
            ekgp5resursTT.ARBETEKOST =  ekgp5resursTT.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.                                                    
      END.
      IF ekgresursprisTT.REGELID = 2 THEN DO:
         /*MASKIN EA*/
         ASSIGN.                          
         ekgp5resursTT.EAMASK = ekgp5resursTT.ANTAL * ekgresursprisTT.EA.                           
         ekgp5resursTT.MASKINKOST = ekgp5resursTT.ANTAL * ekgresursprisTT.PRIS.                                   
      END.
      IF ekgresursprisTT.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
         /*ÖVRIGT EA*/
         ASSIGN.                                               
         ekgp5resursTT.EAOVRIGT = ekgp5resursTT.ANTAL * ekgresursprisTT.EA.                           
         ekgp5resursTT.OVRIGT = ekgp5resursTT.ANTAL * ekgresursprisTT.PRIS.                               
      END.                       
      IF ekgresursprisTT.REGELID = 8 THEN DO:
         /*ÖVRIG KOSTNAD*/
         ASSIGN.
         ekgp5resursTT.OVRIGT = ekgp5resursTT.ANTAL * ekgresursprisTT.PRIS.                                                          
      END.       
   END METHOD.   
   METHOD PUBLIC VOID LaddaP4VFfrek():
      /*FREKVENS*/    
      THIS-OBJECT:EKGP4Tracking(FALSE).
      FOR EACH ekgp4frekvensTT  NO-LOCK:                  
         FIND FIRST ekgp5TT WHERE ekgp5TT.P5ARBKOD = ekgp4frekvensTT.P5ARBKOD AND ekgp5TT.P5LOPNR = ekgp4frekvensTT.P5LOPNR NO-LOCK NO-ERROR.         
         THIS-OBJECT:LaddaP4VFfrekSingel().
      END.               
      THIS-OBJECT:EKGP4Tracking(TRUE).
   END METHOD.
   METHOD PUBLIC VOID LaddaP4VFfrekenarb():
      /*FREKVENS*/          
      THIS-OBJECT:EKGP4Tracking(FALSE).
      FOR EACH ekgp4frekvensbuff WHERE ekgp4frekvensbuff.P4ARBKOD = ekgp4frekvensTT.P4ARBKOD  NO-LOCK:         
         FIND FIRST ekgp5TT WHERE ekgp5TT.P5ARBKOD = ekgp4frekvensTT.P5ARBKOD AND ekgp5TT.P5LOPNR = ekgp4frekvensTT.P5LOPNR NO-LOCK NO-ERROR.         
         THIS-OBJECT:LaddaP4VFfrekSingel().
      END.               
      THIS-OBJECT:EKGP4Tracking(TRUE).
   END METHOD.
   METHOD PUBLIC VOID LaddaP4VFfrekSingel():
      /*FREKVENS*/    
      IF AVAILABLE ekgp5TT THEN DO:
         FIND FIRST ekgp4TT WHERE ekgp4TT.P4ARBKOD = ekgp4frekvensTT.P4ARBKOD AND ekgp4TT.P4LOPNR = ekgp4frekvensTT.P4LOPNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgp4frekvensTT.P4BENAMNING =  ekgp4TT.BENAMNING.         
         ekgp4frekvensTT.P5BENAMNING =  ekgp5TT.BENAMNING.              
         ekgp4frekvensTT.FMONTTIM = ekgp4frekvensTT.ANTAL * ekgp5TT.MONTTIM.
         ekgp4frekvensTT.FMASKTIM = ekgp4frekvensTT.ANTAL * ekgp5TT.MASKTIM.         
         ASSIGN
         ekgp4frekvensTT.FARBETEKOST =  ekgp4frekvensTT.ANTAL * ekgp5TT.ARBETEKOST.                                                                                         
         ekgp4frekvensTT.FMASKINKOST = ekgp4frekvensTT.ANTAL * ekgp5TT.MASKINKOST.
         ekgp4frekvensTT.FOVRIGT = ekgp4frekvensTT.ANTAL * ekgp5TT.OVRIGT.       
         ekgp4frekvensTT.FEAMASK = ekgp4frekvensTT.ANTAL * ekgp5TT.EAMASK.
         ekgp4frekvensTT.FEAOVRIGT = ekgp4frekvensTT.ANTAL * ekgp5TT.EAOVRIGT.
         ekgp4frekvensTT.FEA = ekgp4frekvensTT.ANTAL * ekgp5TT.EA.
         ekgp4frekvensTT.FSUMMA =  ekgp4frekvensTT.FARBETEKOST + ekgp4frekvensTT.FMASKINKOST + ekgp4frekvensTT.FOVRIGT.        
      END.               
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP4VFresurs():        
     /*RESURS*/
      THIS-OBJECT:EKGP4Tracking(FALSE).
      FOR EACH ekgp4resursTT  NO-LOCK:
         THIS-OBJECT:LaddaP4VFresursSingel().  
      END.      
      THIS-OBJECT:EKGP4Tracking(TRUE).
   END METHOD.  
   
   METHOD PUBLIC VOID LaddaP4VFresursSingel():
     FIND FIRST ekgp4TT WHERE ekgp4TT.P4ARBKOD = ekgp4resursTT.P4ARBKOD AND ekgp4TT.P4LOPNR = ekgp4resursTT.P4LOPNR NO-LOCK NO-ERROR.
     ASSIGN
     ekgp4resursTT.P4BENAMNING =  ekgp4TT.BENAMNING.
     ekgp4resursTT.P4ENHET = ekgP4TT.ENHET.  
     FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgp4resursTT.RESURSNR NO-LOCK NO-ERROR.
     ASSIGN
     ekgp4resursTT.RESURSBENAMNING =  ekgresursTT.BENAMNING.
     ekgp4resursTT.RESURSENHET = ekgresursTT.ENHET.
     FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp4resursTT.RESURSNR NO-LOCK NO-ERROR.
     FIND FIRST ekgresurspaslagTT WHERE ekgresurspaslagTT.RESURSNR = ekgresursprisTT.RESURSNR NO-LOCK NO-ERROR.
     FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = ekgresurspaslagTT.PASLAGNR NO-LOCK NO-ERROR.        
     IF AVAILABLE ekgpaslagTT THEN DO:
        ASSIGN
         ekgp4resursTT.RESURSPRIS =  ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         ekgp4resursTT.EA = ekgresursprisTT.EA.
     END.
     ELSE DO:
         ASSIGN
         ekgp4resursTT.RESURSPRIS =  ekgresursprisTT.PRIS.
         ekgp4resursTT.EA = ekgresursprisTT.EA.
     END.                      
        
     IF ekgresursprisTT.REGELID = 1 THEN DO:
        /*MONTÖR EA*/
        ASSIGN.
        ekgp4resursTT.EAMONT = ekgp4resursTT.ANTAL.                    
        IF AVAILABLE ekgpaslagTT THEN DO:
           ASSIGN.                              
           ekgp4resursTT.ARBETEKOST =  ekgp4resursTT.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
        END.                                                 
     END.
     IF ekgresursprisTT.REGELID = 2 THEN DO:
        /*MASKIN EA*/
        ASSIGN.                          
        ekgp4resursTT.EAMASK = ekgp4resursTT.ANTAL * ekgresursprisTT.EA.                           
        ekgp4resursTT.MASKINKOST = ekgp4resursTT.ANTAL * ekgresursprisTT.PRIS.                                   
     END.
     IF ekgresursprisTT.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
        /*ÖVRIGT EA*/
        ASSIGN.                                               
        ekgp4resursTT.EAOVRIGT = ekgp4resursTT.ANTAL * ekgresursprisTT.EA.                           
        ekgp4resursTT.OVRIGT = ekgp4resursTT.ANTAL * ekgresursprisTT.PRIS.                               
     END.                   
     IF ekgresursprisTT.REGELID = 8 THEN DO:
        /*ÖVRIG KOSTNAD*/
        ASSIGN.
        ekgp4resursTT.OVRIGT = ekgp4resursTT.ANTAL * ekgresursprisTT.PRIS.                                                          
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP3VFfrek():
      /*FREKVENS*/    
      THIS-OBJECT:EKGP3Tracking(FALSE).
      FOR EACH ekgp3frekvensTT  NO-LOCK:         
         FIND FIRST ekgp4TT WHERE ekgp4TT.P4ARBKOD = ekgp3frekvensTT.P4ARBKOD AND ekgp4TT.P4LOPNR = ekgp3frekvensTT.P4LOPNR NO-LOCK NO-ERROR.
         THIS-OBJECT:LaddaP3VFfrekSingel().                   
      END. 
      THIS-OBJECT:EKGP3Tracking(TRUE).
   END METHOD.    
   
   METHOD PUBLIC VOID LaddaP3VFfrekenarb():
      /*FREKVENS*/          
      THIS-OBJECT:EKGP3Tracking(FALSE).
      FOR EACH ekgp3frekvensbuff WHERE ekgp3frekvensbuff.P3ARBKOD = ekgp3frekvensTT.P3ARBKOD AND ekgp3frekvensbuff.P3LOPNR = ekgp3frekvensTT.P3LOPNR  NO-LOCK:         
         FIND FIRST ekgp4TT WHERE ekgp4TT.P4ARBKOD = ekgp3frekvensTT.P4ARBKOD AND ekgp4TT.P4LOPNR = ekgp3frekvensTT.P4LOPNR NO-LOCK NO-ERROR.
         THIS-OBJECT:LaddaP3VFfrekSingel().
      END.               
      THIS-OBJECT:EKGP3Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP3VFfrekSingel():
      /*FREKVENS*/    
      IF AVAILABLE ekgp4TT THEN DO:
         FIND FIRST ekgp3TT WHERE ekgp3TT.P3ARBKOD = ekgp3frekvensTT.P3ARBKOD AND ekgp3TT.P3LOPNR = ekgp3frekvensTT.P3LOPNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgp3frekvensTT.P3BENAMNING =  ekgp3TT.BENAMNING.         
         ASSIGN
         ekgp3frekvensTT.P4BENAMNING =  ekgp4TT.BENAMNING.     
         ekgp3frekvensTT.FMONTTIM = ekgp3frekvensTT.ANTAL * ekgp4TT.MONTTIM.
         ekgp3frekvensTT.FMASKTIM = ekgp3frekvensTT.ANTAL * ekgp4TT.MASKTIM.      
         ASSIGN
         ekgp3frekvensTT.FARBETEKOST =  ekgp3frekvensTT.ANTAL * ekgp4TT.ARBETEKOST.                                                                                         
         ekgp3frekvensTT.FMASKINKOST = ekgp3frekvensTT.ANTAL * ekgp4TT.MASKINKOST.
         ekgp3frekvensTT.FOVRIGT = ekgp3frekvensTT.ANTAL * ekgp4TT.OVRIGT.       
         ekgp3frekvensTT.FEAMASK = ekgp3frekvensTT.ANTAL * ekgp4TT.EAMASK.
         ekgp3frekvensTT.FEAOVRIGT = ekgp3frekvensTT.ANTAL * ekgp4TT.EAOVRIGT.
         ekgp3frekvensTT.FEA = ekgp3frekvensTT.ANTAL * ekgp4TT.EA.
         ekgp3frekvensTT.FSUMMA =  ekgp3frekvensTT.FARBETEKOST + ekgp3frekvensTT.FMASKINKOST + ekgp3frekvensTT.FOVRIGT.
      END.
       
   END METHOD.
   
   
   METHOD PUBLIC VOID LaddaP3VFresurs(INPUT ebrkatvar AS CHARACTER):        
     /*RESURS*/
     THIS-OBJECT:EKGP3Tracking(FALSE).
      FOR EACH ekgp3resursTT NO-LOCK:
         THIS-OBJECT:LaddaP3VFresursSingel(ebrkatvar). 
      END.      
      THIS-OBJECT:EKGP3Tracking(TRUE).
   END METHOD.  
   
   METHOD PUBLIC VOID LaddaP3VFresursSingel(INPUT ebrkatvar AS CHARACTER):
      FIND FIRST ekgp3TT WHERE ekgp3TT.P3ARBKOD = ekgp3resursTT.P3ARBKOD AND ekgp3TT.P3LOPNR = ekgp3resursTT.P3LOPNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp3TT THEN DO:
        ASSIGN
        ekgp3resursTT.P3BENAMNING =  ekgp3TT.BENAMNING.
        ekgp3resursTT.P3ENHET = ekgp3TT.ENHET.
      END.     
      FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgp3resursTT.RESURSNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgresursTT THEN DO:
        ASSIGN
        ekgp3resursTT.RESURSBENAMNING =  ekgresursTT.BENAMNING.
        ekgp3resursTT.RESURSENHET = ekgresursTT.ENHET.
      END.   
      FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp3resursTT.RESURSNR NO-LOCK NO-ERROR.
      FIND FIRST ekgresurspaslagTT WHERE ekgresurspaslagTT.RESURSNR = ekgresursprisTT.RESURSNR  NO-LOCK NO-ERROR.
      FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = ekgresurspaslagTT.PASLAGNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgresursprisTT THEN DO:
        IF AVAILABLE ekgpaslagTT THEN DO:                           
           ASSIGN
           ekgp3resursTT.RESURSPRIS =  ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
           ekgp3resursTT.EA = ekgresursprisTT.EA.
        END.
        ELSE DO:
           ASSIGN
           ekgp3resursTT.RESURSPRIS =  ekgresursprisTT.PRIS.
           ekgp3resursTT.EA = ekgresursprisTT.EA.
        END.
      END.        
   
      IF ekgresursprisTT.REGELID = 1 THEN DO:
         /*MONTÖR EA*/
         ASSIGN.
         ekgp3resursTT.EAMONT = ekgp3resursTT.ANTAL.                    
         IF AVAILABLE ekgpaslagTT THEN DO:
            ASSIGN.                              
            ekgp3resursTT.ARBETEKOST =  ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.
         ELSE DO:
           /*SKALL ALLTID VARA PÅSLAG*/
            ASSIGN.
            ekgp3resursTT.ARBETEKOST = ekgp3resursTT.ANTAL  * ekgresursprisTT.PRIS.
         END.                                      
      END.
      IF ekgresursprisTT.REGELID = 2 THEN DO:
        /*MASKIN EA*/
         ASSIGN.                          
         ekgp3resursTT.EAMASK = ekgp3resursTT.ANTAL * ekgresursprisTT.EA.                           
         ekgp3resursTT.MASKINKOST = ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS.                                   
      END.
      IF ekgresursprisTT.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
         /*ÖVRIGT EA*/
         ASSIGN.                                               
         ekgp3resursTT.EAOVRIGT = ekgp3resursTT.ANTAL * ekgresursprisTT.EA.                           
         ekgp3resursTT.OVRIGT = ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS.                               
      END.
      IF ekgresursprisTT.REGELID = 4 THEN DO:
          /*Utrust ÖVRIGT EA*/                                
         /*IF ebrkatvar = "KLG1" THEN DO:
            ASSIGN.                                       
            ekgp3resursTT.EAOVRIGT = ekgp3resursTT.ANTAL * ekgresursprisTT.EA.                           
            ekgp3resursTT.OVRIGT = ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS.
         END.*/
         IF ebrkatvar = "KLG2" THEN DO:      
            ekgp3resursTT.EAUTRUST = ekgp3resursTT.ANTAL * ekgresursprisTT.EA.                           
            ekgp3resursTT.UTRUSTKOST = ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS.
         END.                                                         
      END.           
      IF ekgresursprisTT.REGELID = 6 OR ekgresursprisTT.REGELID = 7 THEN DO:
        /*BEREDARE FINNS I P3, TILLK BEREDARE FINNS I P3*/                     
         IF AVAILABLE ekgresurspaslagTT  THEN DO: 
            ASSIGN.                            
            ekgp3resursTT.ARBETEKOST =  ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.
         ELSE DO:
            /*SKALL ALLTID VARA PÅSLAG*/
            ASSIGN.
            ekgp3resursTT.ARBETEKOST = ekgp3resursTT.ANTAL  * ekgresursprisTT.PRIS.
         END.                                  
      END.        
      IF ekgresursprisTT.REGELID = 8 THEN DO:
         /*ÖVRIG KOSTNAD*/
         ASSIGN.
         ekgp3resursTT.OVRIGT = ekgp3resursTT.ANTAL * ekgresursprisTT.PRIS.                                                          
      END. 
           
   END METHOD.
   
   
   METHOD PUBLIC VOID LaddaP2VFfrek():
      /*FREKVENS*/    
      THIS-OBJECT:EKGP2Tracking(FALSE).
      FOR EACH ekgp2frekvensTT NO-LOCK:        
         FIND FIRST ekgp3TT WHERE ekgp3TT.P3ARBKOD = ekgp2frekvensTT.P3ARBKOD AND ekgp3TT.P3LOPNR = ekgp2frekvensTT.P3LOPNR NO-LOCK NO-ERROR.
         THIS-OBJECT:LaddaP2VFfrekSingel().
      END. 
      THIS-OBJECT:EKGP2Tracking(TRUE).
   END METHOD.
   METHOD PUBLIC VOID LaddaP2VFfrekenarb():
      /*FREKVENS*/    
      THIS-OBJECT:EKGP2Tracking(FALSE).      
      FOR EACH ekgp2frekvensbuff WHERE ekgp2frekvensbuff.P2ARBKOD = ekgp2frekvensTT.P2ARBKOD AND ekgp2frekvensbuff.P2LOPNR = ekgp2frekvensTT.P2LOPNR  NO-LOCK:         
         FIND FIRST ekgp3TT WHERE ekgp3TT.P3ARBKOD = ekgp2frekvensTT.P3ARBKOD AND ekgp3TT.P3LOPNR = ekgp2frekvensTT.P3LOPNR NO-LOCK NO-ERROR.
         THIS-OBJECT:LaddaP2VFfrekSingel().
      END.               
      THIS-OBJECT:EKGP2Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP2VFfrekSingel():
      /*FREKVENS*/
       DEBUGGER:SET-BREAK().    
      IF AVAILABLE ekgp3TT THEN DO:
         FIND FIRST ekgp2TT WHERE  ekgp2TT.P2ARBKOD = ekgp2frekvensTT.P2ARBKOD AND ekgp2TT.P2LOPNR = ekgp2frekvensTT.P2LOPNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgp2frekvensTT.P2BENAMNING =  ekgp2TT.BENAMNING.         
         ASSIGN
         ekgp2frekvensTT.P3BENAMNING =  ekgp3TT.BENAMNING.   
         ekgp2frekvensTT.FBERTIM = ekgp2frekvensTT.ANTAL * ekgp3TT.BERTIM.
         ekgp2frekvensTT.FMONTTIM = ekgp2frekvensTT.ANTAL * ekgp3TT.MONTTIM.
         ekgp2frekvensTT.FMASKTIM = ekgp2frekvensTT.ANTAL * ekgp3TT.MASKTIM.
         ekgp2frekvensTT.FUTRUSTTIM = ekgp2frekvensTT.ANTAL * ekgp3TT.UTRUSTTIM.                     
         ASSIGN         
         ekgp2frekvensTT.FARBETEKOST =  ekgp2frekvensTT.ANTAL * ekgp3TT.ARBETEKOST.                                                                                         
         ekgp2frekvensTT.FMASKINKOST = ekgp2frekvensTT.ANTAL * ekgp3TT.MASKINKOST.
         ekgp2frekvensTT.FUTRUSTKOST = ekgp2frekvensTT.ANTAL * ekgp3TT.UTRUSTKOST.
         ekgp2frekvensTT.FOVRIGT = ekgp2frekvensTT.ANTAL * ekgp3TT.OVRIGT.       
         ekgp2frekvensTT.FEAMASK = ekgp2frekvensTT.ANTAL * ekgp3TT.EAMASK.
         ekgp2frekvensTT.FEAUTRUST = ekgp2frekvensTT.ANTAL * ekgp3TT.EAUTRUST.
         ekgp2frekvensTT.FEAOVRIGT = ekgp2frekvensTT.ANTAL * ekgp3TT.EAOVRIGT.
         ekgp2frekvensTT.FEA = ekgp2frekvensTT.ANTAL * ekgp3TT.EA.
         ekgp2frekvensTT.FSUMMA =  ekgp2frekvensTT.FARBETEKOST + ekgp2frekvensTT.FMASKINKOST + ekgp2frekvensTT.FOVRIGT + ekgp2frekvensTT.FUTRUSTKOST.
      END.    
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP2VFresurs(INPUT ebrkatvar AS CHARACTER):         
      /*RESURS*/
      THIS-OBJECT:EKGP2Tracking(FALSE).
      FOR EACH ekgp2resursTT  NO-LOCK:
         THIS-OBJECT:LaddaP2VFresursSingel(ebrkatvar).         
      END.           
      THIS-OBJECT:EKGP2Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID LaddaP2VFresursSingel(INPUT ebrkatvar AS CHARACTER):
      FIND FIRST ekgp2TT WHERE  ekgp2TT.P2ARBKOD = ekgp2resursTT.P2ARBKOD AND ekgp2TT.P2LOPNR = ekgp2resursTT.P2LOPNR NO-LOCK NO-ERROR.
      ASSIGN
      ekgp2resursTT.P2BENAMNING =  ekgp2TT.BENAMNING.
      ekgp2resursTT.P2ENHET = ekgp2TT.ENHET.        
      FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgp2resursTT.RESURSNR NO-LOCK NO-ERROR.
      ASSIGN
      ekgp2resursTT.RESURSBENAMNING =  ekgresursTT.BENAMNING.
      ekgp2resursTT.RESURSENHET = ekgresursTT.ENHET.
      FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp2resursTT.RESURSNR NO-LOCK NO-ERROR.
      FIND FIRST ekgresurspaslagTT WHERE ekgresurspaslagTT.RESURSNR = ekgresursprisTT.RESURSNR NO-LOCK NO-ERROR.
      FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = ekgresurspaslagTT.PASLAGNR NO-LOCK NO-ERROR.      
      IF AVAILABLE ekgpaslagTT THEN DO:
         ASSIGN
         ekgp2resursTT.RESURSPRIS =  ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         ekgp2resursTT.EA = ekgresursprisTT.EA.
      END.
      ELSE DO:
         ASSIGN
         ekgp2resursTT.RESURSPRIS =  ekgresursprisTT.PRIS.
         ekgp2resursTT.EA = ekgresursprisTT.EA.
      END.                                   
      IF ekgresursprisTT.REGELID = 1 THEN DO:
         /*MONTÖR EA*/
         ASSIGN.
         ekgp2resursTT.EAMONT = ekgp2resursTT.ANTAL.                    
         IF AVAILABLE ekgpaslagTT THEN DO:
            ASSIGN.                              
            ekgp2resursTT.ARBETEKOST =  ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.
         ELSE DO:
            /*SKALL ALLTID VARA PÅSLAG*/
            ASSIGN.
            ekgp2resursTT.ARBETEKOST = ekgp2resursTT.ANTAL  * ekgresursprisTT.PRIS.
         END.                                      
      END.
      IF ekgresursprisTT.REGELID = 2 THEN DO:
         /*MASKIN EA*/
         ASSIGN.                          
         ekgp2resursTT.EAMASK = ekgp2resursTT.ANTAL * ekgresursprisTT.EA.                           
         ekgp2resursTT.MASKINKOST = ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS.                                   
      END.
      IF ekgresursprisTT.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
         /*ÖVRIGT EA*/
         ASSIGN.                                               
         ekgp2resursTT.EAOVRIGT = ekgp2resursTT.ANTAL * ekgresursprisTT.EA.                           
         ekgp2resursTT.OVRIGT = ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS.                               
      END.
      IF ekgresursprisTT.REGELID = 4 THEN DO:
         /*Utrust ÖVRIGT EA*/                                
         /*IF ebrkatvar = "KLG1" THEN DO:
            ASSIGN.                                       
            ekgp2resursTT.EAOVRIGT = ekgp2resursTT.ANTAL * ekgresursprisTT.EA.                           
            ekgp2resursTT.OVRIGT = ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS.
         END.*/
         IF ebrkatvar = "KLG2" THEN DO:      
            ekgp2resursTT.EAUTRUST = ekgp2resursTT.ANTAL * ekgresursprisTT.EA.                           
            ekgp2resursTT.UTRUSTKOST = ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS.
         END.                                                          
      END.         
      IF ekgresursprisTT.REGELID = 6 OR ekgresursprisTT.REGELID = 7 THEN DO:                            
         IF AVAILABLE ekgresurspaslagTT  THEN DO: 
            ASSIGN.                            
            ekgp2resursTT.ARBETEKOST =  ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.
         ELSE DO:
            /*SKALL ALLTID VARA PÅSLAG*/
            ASSIGN.
            ekgp2resursTT.ARBETEKOST = ekgp2resursTT.ANTAL  * ekgresursprisTT.PRIS.
         END.                                  
      END.         
      IF ekgresursprisTT.REGELID = 8 THEN DO:
         /*ÖVRIG KOSTNAD*/
         ASSIGN.
         ekgp2resursTT.OVRIGT = ekgp2resursTT.ANTAL * ekgresursprisTT.PRIS.                                                          
      END.                
   END METHOD.
   METHOD PUBLIC VOID LaddaP2VFmtrl():
      /*materiel*/  
      THIS-OBJECT:EKGP2Tracking(FALSE).  
      FOR EACH ekgp2mtrlTT NO-LOCK:                 
         THIS-OBJECT:LaddaP2VFmtrlSingel().
      END. 
      THIS-OBJECT:EKGP2Tracking(TRUE).
   END METHOD.  
   METHOD PUBLIC VOID LaddaP2VFmtrlsingel():
      FIND FIRST ekgp2TT WHERE  ekgp2TT.P2ARBKOD = ekgp2mtrlTT.P2ARBKOD AND ekgp2TT.P2LOPNR = ekgp2mtrlTT.P2LOPNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp2TT THEN DO:
         ASSIGN
         ekgp2mtrlTT.P2BENAMNING =  ekgp2TT.BENAMNING.
      END.   
      FIND FIRST ekgmtrlTT WHERE ekgmtrlTT.ARTNR = ekgp2mtrlTT.ARTNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgmtrlTT THEN DO:
         ASSIGN
         ekgp2mtrlTT.MTRLBENAMNING =  ekgmtrlTT.BENAMNING.   
         ekgp2mtrlTT.KALKYLPRIS = ekgmtrlTT.KALKYLPRIS.
      END.
      FIND FIRST ekgmtrlTT WHERE ekgmtrlTT.ARTNR = ekgp2mtrlTT.ARTNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgmtrlTT THEN DO:
         ASSIGN           
         ekgp2mtrlTT.MTRLSUMMA = ekgmtrlTT.KALKYLPRIS * ekgp2mtrlTT.ANTAL.
      END.              
   END METHOD.

   METHOD PUBLIC VOID LaddaP1VFfrek():
      /*FREKVENS*/    
      THIS-OBJECT:EKGP1Tracking(FALSE).
      FOR EACH ekgp1frekvensTT  NO-LOCK:         
         FIND FIRST ekgp2TT WHERE ekgp2TT.P2ARBKOD = ekgp1frekvensTT.P2ARBKOD AND ekgp2TT.P2LOPNR = ekgp1frekvensTT.P2LOPNR NO-LOCK NO-ERROR.
         THIS-OBJECT:LaddaP1VFfrekSingel().
      END.  
      THIS-OBJECT:EKGP1Tracking(TRUE).
   END METHOD.
   METHOD PUBLIC VOID LaddaP1VFfrekenarb():
      /*FREKVENS*/    
      THIS-OBJECT:EKGP1Tracking(FALSE).      
      FOR EACH ekgp1frekvensbuff WHERE ekgp1frekvensbuff.P1ARBKOD = ekgp1frekvensTT.P1ARBKOD AND ekgp1frekvensbuff.P1LOPNR = ekgp1frekvensTT.P1LOPNR  NO-LOCK:         
         FIND FIRST ekgp2TT WHERE ekgp2TT.P2ARBKOD = ekgp1frekvensTT.P2ARBKOD AND ekgp2TT.P2LOPNR = ekgp1frekvensTT.P2LOPNR NO-LOCK NO-ERROR.
         THIS-OBJECT:LaddaP1VFfrekSingel().
      END.               
      THIS-OBJECT:EKGP1Tracking(TRUE).
   END METHOD.  
   
   METHOD PUBLIC VOID LaddaP1VFfrekSingel():
      /*FREKVENS*/    
     .
      IF AVAILABLE ekgp2TT THEN DO:
         FIND FIRST ekgp1TT WHERE  ekgp1TT.P1ARBKOD = ekgp1frekvensTT.P1ARBKOD AND ekgp1TT.P1LOPNR = ekgp1frekvensTT.P1LOPNR NO-LOCK NO-ERROR.
         ASSIGN
         ekgp1frekvensTT.P1BENAMNING =  ekgp1TT.BENAMNING.         
         ASSIGN
         ekgp1frekvensTT.P2BENAMNING =  ekgp2TT.BENAMNING.   
         ekgp1frekvensTT.FBERTIM = ekgp1frekvensTT.ANTAL * ekgp2TT.BERTIM.
         ekgp1frekvensTT.FMONTTIM = ekgp1frekvensTT.ANTAL * ekgp2TT.MONTTIM.
         ekgp1frekvensTT.FMASKTIM = ekgp1frekvensTT.ANTAL * ekgp2TT.MASKTIM.
         ekgp1frekvensTT.FUTRUSTTIM = ekgp1frekvensTT.ANTAL * ekgp2TT.UTRUSTTIM.                                 
         ASSIGN            
         ekgp1frekvensTT.FARBETEKOST =  ekgp1frekvensTT.ANTAL * ekgp2TT.ARBETEKOST.                                                                                         
         ekgp1frekvensTT.FMASKINKOST = ekgp1frekvensTT.ANTAL * ekgp2TT.MASKINKOST.
         ekgp1frekvensTT.FMATERIELKOST = ekgp1frekvensTT.ANTAL * ekgp2TT.MATERIELKOST.
         ekgp1frekvensTT.FUTRUSTKOST = ekgp1frekvensTT.ANTAL * ekgp2TT.UTRUSTKOST.
         ekgp1frekvensTT.FOVRIGT = ekgp1frekvensTT.ANTAL * ekgp2TT.OVRIGT.       
         ekgp1frekvensTT.FEAMASK = ekgp1frekvensTT.ANTAL * ekgp2TT.EAMASK.
         ekgp1frekvensTT.FEAUTRUST = ekgp1frekvensTT.ANTAL * ekgp2TT.EAUTRUST.
         ekgp1frekvensTT.FEAOVRIGT = ekgp1frekvensTT.ANTAL * ekgp2TT.EAOVRIGT.
         ekgp1frekvensTT.FEA = ekgp1frekvensTT.ANTAL * ekgp2TT.EA.
         ekgp1frekvensTT.FSUMMA =  ekgp1frekvensTT.FARBETEKOST + ekgp1frekvensTT.FMASKINKOST + ekgp1frekvensTT.FMATERIELKOST + ekgp1frekvensTT.FOVRIGT + ekgp1frekvensTT.FUTRUSTKOST.            
      END.        
   END METHOD.
   
   METHOD PUBLIC VOID Beraknapaslag():      
      IF AVAILABLE ekgpaslagTT THEN DO:
         IF ekgpaslagTT.PASLAGNR = 1 THEN DO:
            FOR EACH ekgmtrlTT WHERE ekgmtrlTT.AKTIV = TRUE :
               ASSIGN ekgmtrlTT.BASPRIS = ekgmtrlTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
               IF ekgmtrlTT.LASTA = FALSE THEN ekgmtrlTT.KALKYLPRIS = ekgmtrlTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
            END.                                   
         END.
         IF ekgpaslagTT.PASLAGNR = 5 THEN DO:
            IF ekgpaslagTT.PASLAG NE 0 THEN DO:
               FOR EACH ekgp2resursTT WHERE ekgp2resursTT.RESURSNR = 61:
                  ekgp2resursTT.ANTAL = ekgp2resursTT.ANTAL * (1 + ekgpaslagTT.PASLAG). 
               END.
            END.      
         END.               
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaRP():
      IF AVAILABLE ekgresursprisTT THEN DO:   
         FIND FIRST ekgregelTT WHERE ekgregelTT.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.
         ASSIGN
         ekgresursprisTT.REGELANMARKNING = ekgregelTT.ANMARKNING.                        
         IF ekgresursprisTT.REGELID = 5  THEN DO:
            FOR EACH ekgresursprisbuff :
               /*inte montör eafaktor beredare övrigt eller tillkommande beredare*/
               IF ekgresursprisbuff.REGELID = 1 OR ekgresursprisbuff.REGELID = 5 OR ekgresursprisbuff.REGELID = 6 OR ekgresursprisbuff.REGELID = 7 OR  ekgresursprisbuff.REGELID = 8 THEN.
               ELSE ASSIGN ekgresursprisbuff.EA = ekgresursprisbuff.PRIS / ekgresursprisTT.PRIS.
            END.
         END.
         ELSE DO:      
            IF ekgresursprisTT.REGELID = 1  THEN ekgresursprisTT.EA = 1.
            ELSE IF  ekgresursprisTT.REGELID = 6 OR ekgresursprisTT.REGELID = 7 OR  ekgresursprisTT.REGELID = 8 THEN ekgresursprisTT.EA = 0.
            ELSE DO: 
               FIND FIRST ekgresursprisbuff WHERE ekgresursprisbuff.REGELID = 5 NO-ERROR.
               IF AVAILABLE ekgresursprisbuff THEN DO:
                  ekgresursprisTT.EA = ekgresursprisTT.PRIS / ekgresursprisbuff.PRIS.
               END.
            END.   
         END.
         IF ekgresursprisTT.RESURSNR = 17 THEN DO:
            FIND FIRST ekgresursprisbuff WHERE ekgresursprisbuff.RESURSNR = 101 NO-ERROR.
            ASSIGN ekgresursprisbuff.PRIS = ekgresursprisTT.PRIS.
         END.   
      END.      
   END METHOD.
   METHOD PUBLIC VOID BeraknaMtrlkosten():
      /*PÅSLAG MATERIEL*/
      IF AVAILABLE ekgmtrlTT THEN DO:
         FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = 1 NO-LOCK NO-ERROR.
         IF ekgpaslagTT.PASLAG NE 0 THEN DO:         
            ASSIGN ekgmtrlTT.BASPRIS = ekgmtrlTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
            IF ekgmtrlTT.LASTA = FALSE THEN ekgmtrlTT.KALKYLPRIS = ekgmtrlTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.               
      END.      
   END METHOD.
   
   METHOD PUBLIC VOID Beraknamtrlkostalla():
      /*PÅSLAG MATERIEL*/
      FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = 1 NO-LOCK NO-ERROR.
      IF ekgpaslagTT.PASLAG NE 0 THEN DO:
         FOR EACH ekgmtrlTT WHERE ekgmtrlTT.AKTIV = TRUE :
            ASSIGN ekgmtrlTT.BASPRIS = ekgmtrlTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
            IF ekgmtrlTT.LASTA = FALSE THEN ekgmtrlTT.KALKYLPRIS = ekgmtrlTT.PRIS * ( 1 + ekgpaslagTT.PASLAG).
         END.               
      END.      
   END METHOD.
      
            
   METHOD PUBLIC VOID BeraknaP5alla():
      FOR EACH ekgp5TT:
         THIS-OBJECT:BeraknaP5en().
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP4allafrek():
      FOR EACH ekgp4TT WHERE ekgp4TT.FREKVENS = TRUE:
         THIS-OBJECT:BeraknaP4enfrek().
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP4allaresurs():
      FOR EACH ekgp4TT WHERE ekgp4TT.FREKVENS = FALSE:
         THIS-OBJECT:BeraknaP4enresurs().
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP3allafrek():
      FOR EACH ekgp3TT WHERE ekgp3TT.FREKVENS = TRUE:
         THIS-OBJECT:BeraknaP3enfrek().
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP3allaresurs( INPUT ebrkatvar AS CHARACTER):
      FOR EACH ekgp3TT WHERE ekgp3TT.FREKVENS = FALSE:
         THIS-OBJECT:BeraknaP3enresurs(ebrkatvar).
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP2allafrek():
      FOR EACH ekgp2TT WHERE ekgp2TT.FREKVENS = TRUE:
         THIS-OBJECT:BeraknaP2enfrek( ).
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP2allaresurs( INPUT ebrkatvar AS CHARACTER):
      FOR EACH ekgp2TT WHERE ekgp2TT.FREKVENS = FALSE:
         THIS-OBJECT:BeraknaP2enresurs(ebrkatvar).
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP2allamtrl():
      FOR EACH ekgp2TT :
         THIS-OBJECT:BeraknaP2mtrl().
      END.            
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP1allafrek():
      FOR EACH ekgp1TT WHERE ekgp1TT.FREKVENS = TRUE:
         THIS-OBJECT:BeraknaP1enfrek().
      END.            
   END METHOD.
  
   METHOD PUBLIC VOID BeraknaP5en():
      IF AVAILABLE ekgp5TT THEN DO:
         ASSIGN
         ekgp5TT.MONTTIM = 0 ekgp5TT.BERTIM = 0 ekgp5TT.MASKTIM = 0 ekgp5TT.EAMASK = 0 ekgp5TT.EAOVRIGT = 0 
         ekgp5TT.EAUTRUST = 0 ekgp5TT.ARBETEKOST = 0 ekgp5TT.MATERIELKOST = 0 ekgp5TT.MASKINKOST = 0 ekgp5TT.OVRIGT = 0  
         ekgp5TT.EA = 0 ekgp5TT.SUMMA = 0.      
         FOR EACH ekgp5resursbuff WHERE ekgp5resursbuff.P5ARBKOD = ekgp5TT.P5ARBKOD NO-LOCK:         
            FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp5resursbuff.RESURSNR NO-LOCK NO-ERROR.
            FIND FIRST ekgregelTT WHERE ekgregelTT.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.                                 
            IF AVAILABLE ekgresursprisTT THEN DO:
               FIND FIRST ekgresurspaslagTT WHERE ekgresurspaslagTT.RESURSNR = ekgp5resursbuff.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE ekgresurspaslagTT  THEN DO:
                  FIND FIRST ekgpaslagTT WHERE ekgpaslagTT.PASLAGNR = ekgresurspaslagTT.PASLAGNR NO-LOCK NO-ERROR.
               END.                                                         
               IF ekgresursprisTT.REGELID = 1 THEN DO:
                  /*MONTÖR EA*/
                  ASSIGN.
                  ekgp5TT.MONTTIM = ekgp5TT.MONTTIM + ekgp5resursbuff.ANTAL.                           
                  IF AVAILABLE ekgresurspaslagTT  THEN DO:
                     ASSIGN.                              
                     ekgp5TT.ARBETEKOST =  ekgp5TT.ARBETEKOST + (ekgp5resursbuff.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ASSIGN.
                     ekgp5TT.ARBETEKOST = ekgp5TT.ARBETEKOST +  (ekgp5resursbuff.ANTAL  * ekgresursprisTT.PRIS).
                  END.                                      
               END.
               IF ekgresursprisTT.REGELID = 2 THEN DO:
                  /*MASKIN EA*/
                  ASSIGN.
                  ekgp5TT.MASKTIM = ekgp5TT.MASKTIM + ekgp5resursbuff.ANTAL.                           
                  ekgp5TT.EAMASK = ekgp5TT.EAMASK + (ekgp5resursbuff.ANTAL * ekgresursprisTT.EA). 
                  IF AVAILABLE ekgresurspaslagTT THEN DO:                                               
                     ekgp5TT.MASKINKOST = ekgp5TT.MASKINKOST + (ekgp5resursbuff.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagTT.PASLAG)).
                  END.   
                  ELSE ekgp5TT.MASKINKOST = ekgp5TT.MASKINKOST + (ekgp5resursbuff.ANTAL * ekgresursprisTT.PRIS).                                                     
               END.
               IF ekgresursprisTT.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
                  /*ÖVRIGT EA*/
                  ASSIGN.                                               
                  ekgp5TT.EAOVRIGT = ekgp5TT.EAOVRIGT + (ekgp5resursbuff.ANTAL * ekgresursprisTT.EA).                           
                  ekgp5TT.OVRIGT = ekgp5TT.OVRIGT + (ekgp5resursbuff.ANTAL * ekgresursprisTT.PRIS).                               
               END.                        
               IF ekgresursprisTT.REGELID = 8 THEN DO:
                  /*ÖVRIG KOSTNAD*/
                  ASSIGN.
                  ekgp5TT.OVRIGT = ekgp5TT.OVRIGT + (ekgp5resursbuff.ANTAL * ekgresursprisTT.PRIS).                                                          
               END.     
            END.                                                  
         END.
         ASSIGN.
         ekgp5TT.EA = ekgp5TT.MONTTIM + ekgp5TT.EAMASK +  ekgp5TT.EAOVRIGT + ekgp5TT.EAUTRUST.
         ekgp5TT.SUMMA = ekgp5TT.ARBETEKOST + ekgp5TT.MASKINKOST + ekgp5TT.MATERIELKOST + ekgp5TT.OVRIGT + ekgp5TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP4enfrek():
      IF AVAILABLE ekgp4TT THEN DO:                           
         ASSIGN
         ekgp4TT.MONTTIM = 0 ekgp4TT.BERTIM = 0 ekgp4TT.MASKTIM = 0 ekgp4TT.EAMASK = 0 ekgp4TT.EAOVRIGT = 0 ekgp4TT.EAUTRUST = 0 ekgp4TT.ARBETEKOST = 0 ekgp4TT.MATERIELKOST = 0 ekgp4TT.MASKINKOST = 0 ekgp4TT.OVRIGT = 0  ekgp4TT.EA = 0 ekgp4TT.SUMMA = 0.
         /*på P4 nivå finns inga resurser om frekvens = true*/            
         FOR EACH ekgp4frekvensbuff WHERE ekgp4frekvensbuff.P4ARBKOD = ekgp4TT.P4ARBKOD NO-LOCK:
            FIND FIRST ekgp5TT WHERE  ekgp5TT.P5ARBKOD = ekgp4frekvensbuff.P5ARBKOD NO-LOCK NO-ERROR.
            IF AVAILABLE ekgp5TT  THEN DO:
               ekgp4TT.MONTTIM = ekgp4TT.MONTTIM + (ekgp4frekvensbuff.ANTAL * ekgp5TT.MONTTIM).
               ekgp4TT.MASKTIM = ekgp4TT.MASKTIM + (ekgp4frekvensbuff.ANTAL * ekgp5TT.MASKTIM). 
               ekgp4TT.ARBETEKOST =  ekgp4TT.ARBETEKOST + (ekgp4frekvensbuff.ANTAL * ekgp5TT.ARBETEKOST).                                                                                         
               ekgp4TT.MASKINKOST = ekgp4TT.MASKINKOST + (ekgp4frekvensbuff.ANTAL * ekgp5TT.MASKINKOST).
               ekgp4TT.OVRIGT = ekgp4TT.OVRIGT + (ekgp4frekvensbuff.ANTAL * ekgp5TT.OVRIGT).
               
               ekgp4TT.EAMASK = ekgp4TT.EAMASK + (ekgp4frekvensbuff.ANTAL * ekgp5TT.EAMASK).
               ekgp4TT.EAOVRIGT = ekgp4TT.EAOVRIGT + (ekgp4frekvensbuff.ANTAL * ekgp5TT.EAOVRIGT).
               ekgp4TT.EAUTRUST = ekgp4TT.EAUTRUST + (ekgp4frekvensbuff.ANTAL * ekgp5TT.EAUTRUST).
            END.   
         END.                                                
         ekgp4TT.EA = ekgp4TT.MONTTIM + ekgp4TT.EAMASK +  ekgp4TT.EAOVRIGT + ekgp4TT.EAUTRUST.
         ekgp4TT.SUMMA = ekgp4TT.ARBETEKOST + ekgp4TT.MASKINKOST + ekgp4TT.MATERIELKOST + ekgp4TT.OVRIGT + ekgp4TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP4enresurs():
      IF AVAILABLE ekgp4TT THEN DO:
         ASSIGN
         ekgp4TT.MONTTIM = 0 ekgp4TT.BERTIM = 0 ekgp4TT.MASKTIM = 0 ekgp4TT.EAMASK = 0 ekgp4TT.EAOVRIGT = 0 ekgp4TT.EAUTRUST = 0 ekgp4TT.ARBETEKOST = 0 ekgp4TT.MATERIELKOST = 0 ekgp4TT.MASKINKOST = 0 ekgp4TT.OVRIGT = 0  ekgp4TT.EA = 0 ekgp4TT.SUMMA = 0.      
         FOR EACH ekgp4resursbuff WHERE  ekgp4resursbuff.P4ARBKOD = ekgp4tt.P4ARBKOD NO-LOCK:                
            FIND FIRST ekgresurspristt WHERE  ekgresurspristt.RESURSNR = ekgp4resursbuff.RESURSNR NO-LOCK NO-ERROR.                                                  
            IF AVAILABLE ekgresurspristt THEN DO:
               FIND FIRST ekgresurspaslagtt WHERE  ekgresurspaslagtt.RESURSNR = ekgp4resursbuff.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE ekgresurspaslagtt  THEN DO:
                  FIND FIRST ekgpaslagtt WHERE  ekgpaslagtt.PASLAGNR = ekgresurspaslagtt.PASLAGNR NO-LOCK NO-ERROR.
               END.                                                         
               IF ekgresurspristt.REGELID = 1 THEN DO:
                  /*MONTÖR EA*/
                  ekgp4tt.MONTTIM = ekgp4tt.MONTTIM + ekgp4resursbuff.ANTAL.                           
                  IF AVAILABLE ekgresurspaslagtt  THEN DO:                              
                     ekgp4tt.ARBETEKOST =  ekgp4tt.ARBETEKOST + (ekgp4resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp4tt.ARBETEKOST = ekgp4tt.ARBETEKOST +  (ekgp4resursbuff.ANTAL  * ekgresurspristt.PRIS).
                  END.                                      
               END.
               IF ekgresurspristt.REGELID = 2 THEN DO:
                  /*MASKIN EA*/
                  ekgp4tt.MASKTIM = ekgp4tt.MASKTIM + ekgp4resursbuff.ANTAL.                           
                  ekgp4tt.EAMASK = ekgp4tt.EAMASK + (ekgp4resursbuff.ANTAL * ekgresurspristt.EA).      
                  IF AVAILABLE ekgresurspaslagtt THEN DO:                     
                     ekgp4tt.MASKINKOST = ekgp4tt.MASKINKOST + (ekgp4resursbuff.ANTAL * ekgresurspristt.PRIS  * ( 1 + ekgpaslagtt.PASLAG)).
                  END.     
                  ELSE ekgp4tt.MASKINKOST = ekgp4tt.MASKINKOST + (ekgp4resursbuff.ANTAL * ekgresurspristt.PRIS).                                   
               END.
               IF ekgresurspristt.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
                  /*ÖVRIGT EA*/                                               
                  ekgp4tt.EAOVRIGT = ekgp4tt.EAOVRIGT + (ekgp4resursbuff.ANTAL * ekgresurspristt.EA).                           
                  ekgp4tt.OVRIGT = ekgp4tt.OVRIGT + (ekgp4resursbuff.ANTAL * ekgresurspristt.PRIS).                               
               END.                           
               IF ekgresurspristt.REGELID = 8 THEN DO:
                  /*ÖVRIG KOSTNAD*/
                  ekgp4tt.OVRIGT = ekgp4tt.OVRIGT + (ekgp4resursbuff.ANTAL * ekgresurspristt.PRIS).                                                          
               END.     
            END.                        
         END.               
         ekgp4TT.EA = ekgp4TT.MONTTIM + ekgp4TT.EAMASK +  ekgp4TT.EAOVRIGT + ekgp4TT.EAUTRUST.
         ekgp4TT.SUMMA = ekgp4TT.ARBETEKOST + ekgp4TT.MASKINKOST + ekgp4TT.MATERIELKOST + ekgp4TT.OVRIGT + ekgp4TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP3enfrek():
      IF AVAILABLE ekgp3TT THEN DO:                           
         ASSIGN
         ekgp3TT.MONTTIM = 0 ekgp3TT.BERTIM = 0 ekgp3TT.MASKTIM = 0 ekgp3TT.UTRUSTTIM = 0 ekgp3TT.EAMASK = 0 ekgp3TT.EAOVRIGT = 0 ekgp3TT.EAUTRUST = 0 ekgp3TT.ARBETEKOST = 0 ekgp3TT.MATERIELKOST = 0 ekgp3TT.MASKINKOST = 0 ekgp3TT.UTRUSTKOST = 0 ekgp3TT.OVRIGT = 0  ekgp3TT.EA = 0 ekgp3TT.SUMMA = 0.
         /*på P4 nivå finns inga resurser om frekvens = true*/            
         FOR EACH ekgp3frekvensbuff WHERE ekgp3frekvensbuff.P3ARBKOD = ekgp3TT.P3ARBKOD AND ekgp3frekvensbuff.P3LOPNR = ekgp3TT.P3LOPNR NO-LOCK:
            FIND FIRST ekgp4TT WHERE ekgp4TT.P4ARBKOD = ekgp3frekvensbuff.P4ARBKOD NO-LOCK NO-ERROR.
            IF AVAILABLE ekgp4TT  THEN DO:
               ekgp3TT.MONTTIM = ekgp3TT.MONTTIM + (ekgp3frekvensbuff.ANTAL * ekgp4TT.MONTTIM).
               ekgp3TT.MASKTIM = ekgp3TT.MASKTIM + (ekgp3frekvensbuff.ANTAL * ekgp4TT.MASKTIM). 
               ekgp3TT.ARBETEKOST =  ekgp3TT.ARBETEKOST + (ekgp3frekvensbuff.ANTAL * ekgp4TT.ARBETEKOST).                                                                                         
               ekgp3TT.MASKINKOST = ekgp3TT.MASKINKOST + (ekgp3frekvensbuff.ANTAL * ekgp4TT.MASKINKOST).
               ekgp3TT.OVRIGT = ekgp3TT.OVRIGT + (ekgp3frekvensbuff.ANTAL * ekgp4TT.OVRIGT).
               
               ekgp3TT.EAMASK = ekgp3TT.EAMASK + (ekgp3frekvensbuff.ANTAL * ekgp4TT.EAMASK).
               ekgp3TT.EAOVRIGT = ekgp3TT.EAOVRIGT + (ekgp3frekvensbuff.ANTAL * ekgp4TT.EAOVRIGT).
               ekgp3TT.EAUTRUST = ekgp3TT.EAUTRUST + (ekgp3frekvensbuff.ANTAL * ekgp4TT.EAUTRUST).
            END.   
         END.
         /*För P3 kan det vara både frekvens och tillkommande resurs 101-tillkommande beredare*/
         FOR EACH ekgp3resursbuff WHERE ekgp3resursbuff.P3ARBKOD = ekgp3TT.P3ARBKOD AND ekgp3resursbuff.P3LOPNR = ekgp3TT.P3LOPNR NO-LOCK:               
            FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp3resursbuff.RESURSNR NO-LOCK NO-ERROR. 
            FIND FIRST ekgregeltt WHERE ekgregeltt.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.                                   
            IF AVAILABLE ekgresursprisTT THEN DO:
               FIND FIRST ekgresurspaslagtt WHERE  ekgresurspaslagtt.RESURSNR = ekgp3resursbuff.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE ekgresurspaslagtt  THEN DO:
                  FIND FIRST ekgpaslagtt WHERE ekgpaslagtt.PASLAGNR = ekgresurspaslagtt.PASLAGNR NO-LOCK NO-ERROR.
               END.                                                                           
               IF ekgresursprisTT.REGELID = 7 THEN DO:
                  /*TILLK BEREDARE FINNS i P3*/
                  ekgp3TT.BERTIM = ekgp3TT.BERTIM + ekgp3resursbuff.ANTAL.                           
                  IF AVAILABLE ekgresurspaslagtt  THEN DO:                              
                     ekgp3TT.ARBETEKOST =  ekgp3TT.ARBETEKOST + (ekgp3resursbuff.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp3TT.ARBETEKOST = ekgp3TT.ARBETEKOST +  (ekgp3resursbuff.ANTAL  * ekgresursprisTT.PRIS).
                  END.                                  
               END.                       
            END.                                                            
         END.                           
         ekgp3TT.EA = ekgp3TT.MONTTIM + ekgp3TT.EAMASK +  ekgp3TT.EAOVRIGT + ekgp3TT.EAUTRUST.
         ekgp3TT.SUMMA = ekgp3TT.ARBETEKOST + ekgp3TT.MASKINKOST + ekgp3TT.MATERIELKOST + ekgp3TT.OVRIGT + ekgp3TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP3enresurs(INPUT ebrkatvar AS CHARACTER):
      IF AVAILABLE ekgp3TT THEN DO:
         ASSIGN
         ekgp3TT.MONTTIM = 0 ekgp3TT.BERTIM = 0 ekgp3TT.MASKTIM = 0 ekgp3TT.UTRUSTTIM = 0 ekgp3TT.EAMASK = 0 ekgp3TT.EAOVRIGT = 0 ekgp3TT.EAUTRUST = 0 ekgp3TT.ARBETEKOST = 0 ekgp3TT.MATERIELKOST = 0 ekgp3TT.MASKINKOST = 0 ekgp3TT.UTRUSTKOST = 0 ekgp3TT.OVRIGT = 0  ekgp3TT.EA = 0 ekgp3TT.SUMMA = 0.      
         FOR EACH ekgp3resursbuff WHERE  ekgp3resursbuff.P3ARBKOD = ekgp3tt.P3ARBKOD AND ekgp3resursbuff.P3LOPNR = ekgp3tt.P3LOPNR  NO-LOCK:                
            FIND FIRST ekgresurspristt WHERE  ekgresurspristt.RESURSNR = ekgp3resursbuff.RESURSNR NO-LOCK NO-ERROR.                                                  
            IF AVAILABLE ekgresurspristt THEN DO:
               FIND FIRST ekgresurspaslagtt WHERE  ekgresurspaslagtt.RESURSNR = ekgp3resursbuff.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE ekgresurspaslagtt  THEN DO:
                  FIND FIRST ekgpaslagtt WHERE  ekgpaslagtt.PASLAGNR = ekgresurspaslagtt.PASLAGNR NO-LOCK NO-ERROR.
               END.                                                         
               IF ekgresurspristt.REGELID = 1 THEN DO:
                  /*MONTÖR EA*/
                  ekgp3tt.MONTTIM = ekgp3tt.MONTTIM + ekgp3resursbuff.ANTAL.                           
                  IF AVAILABLE ekgresurspaslagtt  THEN DO:                              
                     ekgp3tt.ARBETEKOST =  ekgp3tt.ARBETEKOST + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp3tt.ARBETEKOST = ekgp3tt.ARBETEKOST +  (ekgp3resursbuff.ANTAL  * ekgresurspristt.PRIS).
                  END.                                      
               END.
               IF ekgresurspristt.REGELID = 2 THEN DO:
                  /*MASKIN EA*/
                  ekgp3tt.MASKTIM = ekgp3tt.MASKTIM + ekgp3resursbuff.ANTAL.                           
                  ekgp3tt.EAMASK = ekgp3tt.EAMASK + (ekgp3resursbuff.ANTAL * ekgresurspristt.EA).    
                  IF AVAILABLE ekgresurspaslagtt THEN DO:                       
                     ekgp3tt.MASKINKOST = ekgp3tt.MASKINKOST + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.     
                  ELSE ekgp3tt.MASKINKOST = ekgp3tt.MASKINKOST + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS).                                   
               END.
               IF ekgresurspristt.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
                  /*ÖVRIGT EA*/                                               
                  ekgp3tt.EAOVRIGT = ekgp3tt.EAOVRIGT + (ekgp3resursbuff.ANTAL * ekgresurspristt.EA).                           
                  ekgp3tt.OVRIGT = ekgp3tt.OVRIGT + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS).                               
               END.
               IF ekgresurspristt.REGELID = 4 THEN DO:
                  /*Utrust ÖVRIGT EA*/         
                  /*IF ebrkatvar = "KLG1" THEN DO:                                       
                     ekgp3tt.EAOVRIGT = ekgp3tt.EAOVRIGT + (ekgp3resursbuff.ANTAL * ekgresurspristt.EA).                           
                     ekgp3tt.OVRIGT = ekgp3tt.OVRIGT + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS).
                  END.*/
                  IF ebrkatvar = "KLG2" THEN DO:      
                     /*P4 FINNS INTE I KLG2*/     
                     ekgp3tt.UTRUSTTIM = ekgp3tt.UTRUSTTIM + ekgp3resursbuff.ANTAL.                            
                     ekgp3tt.EAUTRUST = ekgp3tt.EAUTRUST + (ekgp3resursbuff.ANTAL * ekgresurspristt.EA).                           
                     ekgp3tt.UTRUSTKOST = ekgp3tt.UTRUSTKOST + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS).
                  END.                                              
               END.               
               IF ekgresurspristt.REGELID = 6 OR ekgresurspristt.REGELID = 7 THEN DO:
                  /*BEREDARE FINNS  I P3*/
                  ekgp3tt.BERTIM = ekgp3tt.BERTIM + ekgp3resursbuff.ANTAL.                           
                  IF AVAILABLE ekgpaslagtt  THEN DO:                              
                     ekgp3tt.ARBETEKOST =  ekgp3tt.ARBETEKOST + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp3tt.ARBETEKOST = ekgp3tt.ARBETEKOST +  (ekgp3resursbuff.ANTAL  * ekgresurspristt.PRIS).
                  END.                                  
               END.                                      
               IF ekgresurspristt.REGELID = 8 THEN DO:
                  /*ÖVRIG KOSTNAD*/
                  ekgp3tt.OVRIGT = ekgp3tt.OVRIGT + (ekgp3resursbuff.ANTAL * ekgresurspristt.PRIS).                                                          
               END.     
            END.                        
         END.               
         ekgp3TT.EA = ekgp3TT.MONTTIM + ekgp3TT.EAMASK +  ekgp3TT.EAOVRIGT + ekgp3TT.EAUTRUST.
         ekgp3TT.SUMMA = ekgp3TT.ARBETEKOST + ekgp3TT.MASKINKOST + ekgp3TT.MATERIELKOST + ekgp3TT.OVRIGT + ekgp3TT.UTRUSTKOST.
      END.         
   END METHOD.

   METHOD PUBLIC VOID BeraknaP2enfrek():
      IF AVAILABLE ekgp2TT THEN DO:                       
         ASSIGN
         ekgp2TT.MONTTIM = 0 ekgp2TT.BERTIM = 0 ekgp2TT.MASKTIM = 0 ekgp2TT.UTRUSTTIM = 0 ekgp2TT.EAMASK = 0 ekgp2TT.EAOVRIGT = 0 ekgp2TT.EAUTRUST = 0 ekgp2TT.ARBETEKOST = 0 /*ekgp2TT.MATERIELKOST = 0*/ ekgp2TT.MASKINKOST = 0 ekgp2TT.UTRUSTKOST = 0 ekgp2TT.OVRIGT = 0  ekgp2TT.EA = 0 ekgp2TT.SUMMA = 0.
         /*på P4 nivå finns inga resurser om frekvens = true*/            
         FOR EACH ekgp2frekvensbuff WHERE ekgp2frekvensbuff.P2ARBKOD = ekgp2TT.P2ARBKOD AND ekgp2frekvensbuff.P2LOPNR = ekgp2TT.P2LOPNR NO-LOCK:
            FIND FIRST ekgp3TT WHERE ekgp3TT.P3ARBKOD = ekgp2frekvensbuff.P3ARBKOD AND ekgp3TT.P3LOPNR = ekgp2frekvensbuff.P3LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE ekgp3TT  THEN DO:
               ekgp2TT.MONTTIM = ekgp2TT.MONTTIM + (ekgp2frekvensbuff.ANTAL * ekgp3TT.MONTTIM).
               ekgp2TT.BERTIM = ekgp2TT.BERTIM + (ekgp2frekvensbuff.ANTAL * ekgp3TT.BERTIM).
               ekgp2TT.MASKTIM = ekgp2TT.MASKTIM + (ekgp2frekvensbuff.ANTAL * ekgp3TT.MASKTIM).
               ekgp2TT.UTRUSTTIM = ekgp2TT.UTRUSTTIM + (ekgp2frekvensbuff.ANTAL * ekgp3TT.UTRUSTTIM). 
               ekgp2TT.ARBETEKOST =  ekgp2TT.ARBETEKOST + (ekgp2frekvensbuff.ANTAL * ekgp3TT.ARBETEKOST).                                                                                         
               ekgp2TT.MASKINKOST = ekgp2TT.MASKINKOST + (ekgp2frekvensbuff.ANTAL * ekgp3TT.MASKINKOST).
               ekgp2TT.UTRUSTKOST = ekgp2TT.UTRUSTKOST + (ekgp2frekvensbuff.ANTAL * ekgp3TT.UTRUSTKOST).
               ekgp2TT.OVRIGT = ekgp2TT.OVRIGT + (ekgp2frekvensbuff.ANTAL * ekgp3TT.OVRIGT).
               
               ekgp2TT.EAMASK = ekgp2TT.EAMASK + (ekgp2frekvensbuff.ANTAL * ekgp3TT.EAMASK).
               ekgp2TT.EAOVRIGT = ekgp2TT.EAOVRIGT + (ekgp2frekvensbuff.ANTAL * ekgp3TT.EAOVRIGT).
               ekgp2TT.EAUTRUST = ekgp2TT.EAUTRUST + (ekgp2frekvensbuff.ANTAL * ekgp3TT.EAUTRUST).
            END.   
         END.
         /*För P2 kan det vara både frekvens och tillkommande resurs 101-tillkommande beredare och övriga kostnader*/            
         FOR EACH ekgp2resursbuff WHERE ekgp2resursbuff.P2ARBKOD = ekgp2TT.P2ARBKOD AND ekgp2resursbuff.P2LOPNR = ekgp2TT.P2LOPNR NO-LOCK:               
            FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgp2resursbuff.RESURSNR NO-LOCK NO-ERROR. 
            FIND FIRST ekgregeltt WHERE ekgregeltt.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.                                   
            IF AVAILABLE ekgresursprisTT THEN DO:
               FIND FIRST ekgresurspaslagtt WHERE ekgresurspaslagtt.RESURSNR = ekgp2resursbuff.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE ekgresurspaslagtt  THEN DO:
                  FIND FIRST ekgpaslagtt WHERE ekgpaslagtt.PASLAGNR = ekgresurspaslagtt.PASLAGNR NO-LOCK NO-ERROR.
               END.                                                                           
               IF ekgresursprisTT.REGELID = 7 THEN DO:
                  /*TILLK BEREDARE FINNS */
                  ekgp2TT.BERTIM = ekgp2TT.BERTIM + ekgp2resursbuff.ANTAL.                           
                  IF AVAILABLE ekgresurspaslagtt  THEN DO:                              
                     ekgp2TT.ARBETEKOST =  ekgp2TT.ARBETEKOST + (ekgp2resursbuff.ANTAL * ekgresursprisTT.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp2TT.ARBETEKOST = ekgp2TT.ARBETEKOST +  (ekgp2resursbuff.ANTAL  * ekgresursprisTT.PRIS).
                  END.                                  
               END.
               IF ekgresursprisTT.REGELID = 8 THEN DO:
                  /*ÖVRIG KOSTNAD*/
                  ekgp2TT.OVRIGT = ekgp2TT.OVRIGT + (ekgp2resursbuff.ANTAL * ekgresursprisTT.PRIS).                                                          
               END.                        
            END.                                                            
         END.                                   
         ekgp2TT.EA = ekgp2TT.MONTTIM + ekgp2TT.EAMASK +  ekgp2TT.EAOVRIGT + ekgp2TT.EAUTRUST.
         ekgp2TT.SUMMA = ekgp2TT.ARBETEKOST + ekgp2TT.MASKINKOST + ekgp2TT.MATERIELKOST + ekgp2TT.OVRIGT + ekgp2TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP2enresurs(INPUT ebrkatvar AS CHARACTER):
      IF AVAILABLE ekgp2TT THEN DO:         
         ASSIGN
         ekgp2TT.MONTTIM = 0 ekgp2TT.BERTIM = 0 ekgp2TT.MASKTIM = 0 ekgp2TT.UTRUSTTIM = 0 
         ekgp2TT.EAMASK = 0 ekgp2TT.EAOVRIGT = 0 ekgp2TT.EAUTRUST = 0
         ekgp2TT.ARBETEKOST = 0 /*ekgp2TT.MATERIELKOST = 0*/ ekgp2TT.MASKINKOST = 0 ekgp2TT.UTRUSTKOST = 0 ekgp2TT.OVRIGT = 0  ekgp2TT.EA = 0 ekgp2TT.SUMMA = 0.      
         FOR EACH ekgp2resursbuff WHERE  ekgp2resursbuff.P2ARBKOD = ekgp2tt.P2ARBKOD AND ekgp2resursbuff.P2LOPNR = ekgp2tt.P2LOPNR  NO-LOCK:                
            FIND FIRST ekgresurspristt WHERE  ekgresurspristt.RESURSNR = ekgp2resursbuff.RESURSNR NO-LOCK NO-ERROR.                                                  
            IF AVAILABLE ekgresurspristt THEN DO:
               FIND FIRST ekgresurspaslagtt WHERE  ekgresurspaslagtt.RESURSNR = ekgp2resursbuff.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE ekgresurspaslagtt  THEN DO:
                  FIND FIRST ekgpaslagtt WHERE  ekgpaslagtt.PASLAGNR = ekgresurspaslagtt.PASLAGNR NO-LOCK NO-ERROR.
               END.                                                         
               IF ekgresurspristt.REGELID = 1 THEN DO:
                  /*MONTÖR EA*/
                  ekgp2tt.MONTTIM = ekgp2tt.MONTTIM + ekgp2resursbuff.ANTAL.                           
                  IF AVAILABLE ekgresurspaslagtt  THEN DO:                              
                     ekgp2tt.ARBETEKOST =  ekgp2tt.ARBETEKOST + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp2tt.ARBETEKOST = ekgp2tt.ARBETEKOST +  (ekgp2resursbuff.ANTAL  * ekgresurspristt.PRIS).
                  END.                                      
               END.
               IF ekgresurspristt.REGELID = 2 THEN DO:
                  /*MASKIN EA*/
                  ekgp2tt.MASKTIM = ekgp2tt.MASKTIM + ekgp2resursbuff.ANTAL.                           
                  ekgp2tt.EAMASK = ekgp2tt.EAMASK + (ekgp2resursbuff.ANTAL * ekgresurspristt.EA).  
                  IF AVAILABLE ekgresurspaslagtt THEN DO:                         
                     ekgp2tt.MASKINKOST = ekgp2tt.MASKINKOST + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).      
                  END.                                
                  ELSE ekgp2tt.MASKINKOST = ekgp2tt.MASKINKOST + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS).
               END.
               
               IF ekgresursprisTT.REGELID = 3 OR ekgresursprisTT.REGELID = 5 THEN DO:
                  /*ÖVRIGT EA*/
                  ekgp2tt.EAOVRIGT = ekgp2tt.EAOVRIGT + (ekgp2resursbuff.ANTAL * ekgresurspristt.EA).                           
                  ekgp2tt.OVRIGT = ekgp2tt.OVRIGT + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS).                                                 
               END.
               IF ekgresurspristt.REGELID = 4 THEN DO:
                  /*Utrust ÖVRIGT EA*/         
                  /*IF ebrkatvar = "KLG1" THEN DO:                                       
                     ekgp2tt.EAOVRIGT = ekgp2tt.EAOVRIGT + (ekgp2resursbuff.ANTAL * ekgresurspristt.EA).                           
                     ekgp2tt.OVRIGT = ekgp2tt.OVRIGT + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS).
                  END.*/
                  IF ebrkatvar = "KLG2" THEN DO:                          
                     ekgp2tt.UTRUSTTIM = ekgp2tt.UTRUSTTIM + ekgp2resursbuff.ANTAL.                               
                     ekgp2tt.EAUTRUST = ekgp2tt.EAUTRUST + (ekgp2resursbuff.ANTAL * ekgresurspristt.EA).                           
                     ekgp2tt.UTRUSTKOST = ekgp2tt.UTRUSTKOST + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS).
                  END.                                              
               END.               
               IF ekgresurspristt.REGELID = 6 OR ekgresurspristt.REGELID = 7 THEN DO:
                  /*BEREDARE FINNS  I P2*/
                  ekgp2tt.BERTIM = ekgp2tt.BERTIM + ekgp2resursbuff.ANTAL.                           
                  IF AVAILABLE ekgpaslagtt  THEN DO:                              
                     ekgp2tt.ARBETEKOST =  ekgp2tt.ARBETEKOST + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS * ( 1 + ekgpaslagtt.PASLAG)).
                  END.
                  ELSE DO:
                     /*SKALL ALLTID VARA PÅSLAG*/
                     ekgp2tt.ARBETEKOST = ekgp2tt.ARBETEKOST +  (ekgp2resursbuff.ANTAL  * ekgresurspristt.PRIS).
                  END.                                  
               END.                                      
               IF ekgresurspristt.REGELID = 8 THEN DO:
                  /*ÖVRIG KOSTNAD*/
                  ekgp2tt.OVRIGT = ekgp2tt.OVRIGT + (ekgp2resursbuff.ANTAL * ekgresurspristt.PRIS).                                                          
               END.     
            END.                        
         END.               
         ekgp2TT.EA = ekgp2TT.MONTTIM + ekgp2TT.EAMASK +  ekgp2TT.EAOVRIGT + ekgp2TT.EAUTRUST.
         ekgp2TT.SUMMA = ekgp2TT.ARBETEKOST + ekgp2TT.MASKINKOST + ekgp2TT.MATERIELKOST + ekgp2TT.OVRIGT + ekgp2TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   METHOD PUBLIC VOID BeraknaP2mtrl():
      IF AVAILABLE ekgp2TT THEN DO:  
         ASSIGN ekgp2TT.MATERIELKOST = 0.    
         FOR EACH ekgp2mtrlbuff WHERE  ekgp2mtrlbuff.P2ARBKOD = ekgp2tt.P2ARBKOD AND ekgp2mtrlbuff.P2LOPNR = ekgp2tt.P2LOPNR  NO-LOCK:                         
            FIND FIRST ekgmtrltt WHERE ekgmtrltt.ARTNR = ekgp2mtrlbuff.ARTNR NO-LOCK NO-ERROR.
            IF AVAILABLE ekgmtrltt THEN DO:
               ekgp2TT.MATERIELKOST =  ekgp2TT.MATERIELKOST + (ekgp2mtrlbuff.ANTAL * ekgmtrltt.KALKYLPRIS).
            END.                                                    
         END.                     
         ekgp2TT.SUMMA = ekgp2TT.ARBETEKOST + ekgp2TT.MASKINKOST + ekgp2TT.MATERIELKOST + ekgp2TT.OVRIGT + ekgp2TT.UTRUSTKOST.
      END.         
   END METHOD.
      
   METHOD PUBLIC VOID BeraknaP1enfrek():
      IF AVAILABLE ekgp1TT THEN DO:               
         ASSIGN
         ekgp1TT.MONTTIM = 0 ekgp1TT.BERTIM = 0 ekgp1TT.MASKTIM = 0 ekgp1TT.UTRUSTTIM = 0  ekgp1TT.EAMASK = 0 ekgp1TT.EAOVRIGT = 0 ekgp1TT.EAUTRUST = 0 ekgp1TT.ARBETEKOST = 0 ekgp1TT.MATERIELKOST = 0 ekgp1TT.MASKINKOST = 0 ekgp1TT.OVRIGT = 0 ekgp1TT.UTRUSTKOST = 0  ekgp1TT.EA = 0 ekgp1TT.SUMMA = 0.
         /*på P4 nivå finns inga resurser om frekvens = true*/            
         FOR EACH ekgp1frekvensbuff WHERE ekgp1frekvensbuff.P1ARBKOD = ekgp1TT.P1ARBKOD AND ekgp1frekvensbuff.P1LOPNR = ekgp1TT.P1LOPNR NO-LOCK:
            FIND FIRST ekgp2TT WHERE ekgp2TT.P2ARBKOD = ekgp1frekvensbuff.P2ARBKOD AND ekgp2TT.P2LOPNR = ekgp1frekvensbuff.P2LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE ekgp2TT  THEN DO:
               ekgp1TT.MONTTIM = ekgp1TT.MONTTIM + (ekgp1frekvensbuff.ANTAL * ekgp2TT.MONTTIM).
               ekgp1TT.BERTIM = ekgp1TT.BERTIM + (ekgp1frekvensbuff.ANTAL * ekgp2TT.BERTIM).
               ekgp1TT.MASKTIM = ekgp1TT.MASKTIM + (ekgp1frekvensbuff.ANTAL * ekgp2TT.MASKTIM).
               ekgp1TT.UTRUSTTIM = ekgp1TT.UTRUSTTIM + (ekgp1frekvensbuff.ANTAL * ekgp2TT.UTRUSTTIM). 
               ekgp1TT.ARBETEKOST =  ekgp1TT.ARBETEKOST + (ekgp1frekvensbuff.ANTAL * ekgp2TT.ARBETEKOST).                                                                                         
               ekgp1TT.MASKINKOST = ekgp1TT.MASKINKOST + (ekgp1frekvensbuff.ANTAL * ekgp2TT.MASKINKOST).
               ekgp1TT.MATERIELKOST = ekgp1TT.MATERIELKOST + (ekgp1frekvensbuff.ANTAL * ekgp2TT.MATERIELKOST).
               ekgp1TT.UTRUSTKOST = ekgp1TT.UTRUSTKOST + (ekgp1frekvensbuff.ANTAL * ekgp2TT.UTRUSTKOST).
               ekgp1TT.OVRIGT = ekgp1TT.OVRIGT + (ekgp1frekvensbuff.ANTAL * ekgp2TT.OVRIGT).
               
               ekgp1TT.EAMASK = ekgp1TT.EAMASK + (ekgp1frekvensbuff.ANTAL * ekgp2TT.EAMASK).
               ekgp1TT.EAOVRIGT = ekgp1TT.EAOVRIGT + (ekgp1frekvensbuff.ANTAL * ekgp2TT.EAOVRIGT).
               ekgp1TT.EAUTRUST = ekgp1TT.EAUTRUST + (ekgp1frekvensbuff.ANTAL * ekgp2TT.EAUTRUST).
            END.   
         END.                                               
         ekgp1TT.EA = ekgp1TT.MONTTIM + ekgp1TT.EAMASK +  ekgp1TT.EAOVRIGT + ekgp1TT.EAUTRUST.
         ekgp1TT.SUMMA = ekgp1TT.ARBETEKOST + ekgp1TT.MASKINKOST + ekgp1TT.MATERIELKOST + ekgp1TT.OVRIGT + ekgp1TT.UTRUSTKOST.
      END.         
   END METHOD.
   
   /*ändra katalog*/     
   METHOD PUBLIC VOID AndKatalog(INPUT intth AS HANDLE):
      eekgsubkatalogerTTh = intth. 
      
      RUN Katspar IN AppServerHandle (INPUT TABLE eekgsubkatalogerTT).          
   END METHOD.
   
   /*NyResurs*/  
   METHOD PUBLIC ROWID NyResurs(INPUT benamning AS CHARACTER, INPUT nyttResursNr AS INTEGER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      /*DEFINE VARIABLE nyttResursNr AS INTEGER.*/
      /*FIND LAST ekgresursTT USE-INDEX RESURSNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgresursTT THEN nyttResursNr = ekgresursTT.RESURSNR + 1.
      ELSE nyttResursNr = 1.*/
      CREATE ekgresursTT.
      ASSIGN
      ekgresursTT.RESURSNR  = nyttResursNr
      ekgresursTT.BENAMNING = benamning
      ekgresursTT.AKTIV = TRUE 
      ekgresursTT.REGELID = 1
      ekgresursTT.ANMARKNING = ""
      ekgresursTT.TTRECID = RECID(ekgresursTT).
      returner = ROWID(ekgresursTT). 
      ResursSpara().
      RETURN returner.
   END METHOD.
   
   /*NyMtrl*/
   METHOD PUBLIC ROWID NyMtrl(INPUT artnr AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
     DEFINE VARIABLE returner AS ROWID NO-UNDO.
     CREATE ekgmtrlTT.
     ASSIGN
     ekgmtrlTT.EKGSUBID  = ekgsubidvar 
     ekgmtrlTT.EBRKAT = ebrkatvar
     ekgmtrlTT.ARTNR  = artnr 
     ekgmtrlTT.BENAMNING = "<Ny Benämning>"
     ekgmtrlTT.AKTIV  = TRUE
     ekgmtrlTT.TTRECID = RECID(ekgmtrlTT).
     returner = ROWID(ekgmtrlTT). 
     RETURN returner. 
   END METHOD.

   /*NyPaslag*/
   METHOD PUBLIC ROWID NyPaslag(INPUT benamning AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
     DEFINE VARIABLE returner AS ROWID NO-UNDO.
     DEFINE VARIABLE nyttPaslagNr AS INTEGER.
     FIND LAST ekgpaslagTT USE-INDEX PASLAGNR NO-LOCK NO-ERROR.
     IF AVAILABLE ekgpaslagTT THEN nyttPaslagNr = ekgpaslagTT.PASLAGNR + 1.
     ELSE nyttPaslagNr = 1.
     CREATE ekgpaslagTT.
     ASSIGN
     ekgpaslagTT.EKGSUBID  = ekgsubidvar 
     ekgpaslagTT.EBRKAT = ebrkatvar
     ekgpaslagTT.PASLAGNR = nyttPaslagNr
     ekgpaslagTT.BENAMNING = benamning
     ekgpaslagTT.TTRECID = RECID(ekgpaslagTT).
     returner = ROWID(ekgpaslagTT). 
     RETURN returner. 
   END METHOD.
   
   
   /*NyP5*/ 
   METHOD PUBLIC ROWID NyP5LopNr(INPUT arbKod AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
     DEFINE VARIABLE returner AS ROWID NO-UNDO.
     IF AVAILABLE ekgp5TT THEN returner = ROWID(ekgp5TT).
     IF arbKod = "" THEN RETURN returner.
     FIND FIRST ekgp5buff WHERE ekgp5buff.P5ARBKOD = arbKod NO-LOCK NO-ERROR.
     IF NOT AVAILABLE ekgp5buff THEN DO:     
        CREATE ekgp5TT.
        ASSIGN
        ekgp5TT.EKGSUBID  = ekgsubidvar 
        ekgp5TT.EBRKAT = ebrkatvar
        ekgp5TT.P5ARBKOD  = arbKod 
        ekgp5TT.P5LOPNR  = 0
        ekgp5TT.BENAMNING = "<Ny Benämning>"
        ekgp5TT.TTRECID = RECID(ekgp5TT).
        returner = ROWID(ekgp5TT).
     END.   
     ELSE  returner = ROWID(ekgp5buff).   
     RETURN returner. 
   END METHOD.
   
   /*NyP4*/ 
   METHOD PUBLIC ROWID NyP4LopNr(INPUT arbKod AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp4TT THEN returner = ROWID(ekgp4TT).
      IF arbKod = "" THEN RETURN returner.
      FIND FIRST ekgp4buff WHERE ekgp4buff.P4ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp4buff THEN DO:
         CREATE ekgp4TT.
         ASSIGN
         ekgp4TT.EKGSUBID  = ekgsubidvar 
         ekgp4TT.EBRKAT = ebrkatvar
         ekgp4TT.P4ARBKOD  = arbKod 
         ekgp4TT.P4LOPNR  = 0
         ekgp4TT.BENAMNING = "<Ny Benämning>"         
         ekgp4TT.TTRECID = RECID(ekgp4TT).
         returner = ROWID(ekgp4TT). 
      END.
      ELSE  returner = ROWID(ekgp4buff).   
      RETURN returner. 
   END METHOD.
   
   /*NyP3ArbKod*/
   METHOD PUBLIC ROWID NyP3ArbKod(INPUT arbKod AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp3arbkodTT THEN returner = ROWID(ekgp3arbkodTT).
      IF arbKod = "" THEN RETURN returner.
      FIND FIRST ekgp3arbkodbuff WHERE ekgp3arbkodbuff.P3ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp3arbkodbuff THEN DO:
         CREATE ekgp3arbkodTT.
         ASSIGN
         ekgp3arbkodTT.EKGSUBID  = ekgsubidvar 
         ekgp3arbkodTT.EBRKAT = ebrkatvar
         ekgp3arbkodTT.P3ARBKOD  = arbKod 
         ekgp3arbkodTT.BENAMNING = "<Ny Benämning>"
         ekgp3arbkodTT.TTRECID = RECID(ekgp3arbkodTT).
         returner = ROWID(ekgp3arbkodTT).
      END.
      ELSE  returner = ROWID(ekgp3arbkodbuff).    
      RETURN returner. 
   END METHOD.
   
   /*NyP3*/ 
   METHOD PUBLIC ROWID NyP3LopNr(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp3TT THEN returner = ROWID(ekgp3TT).
      IF arbKod = "" OR lopnr = 0 THEN RETURN returner.
      FIND FIRST ekgp3buff WHERE ekgp3buff.P3ARBKOD = arbKod AND ekgp3buff.P3LOPNR = lopnr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp3buff THEN DO:
         CREATE ekgp3TT.
         ASSIGN
         ekgp3TT.EKGSUBID  = ekgsubidvar 
         ekgp3TT.EBRKAT = ebrkatvar
         ekgp3TT.P3ARBKOD  = arbKod 
         ekgp3TT.P3LOPNR  = lopnr
         ekgp3TT.BENAMNING = "<Ny Benämning>"
         ekgp3TT.TTRECID = RECID(ekgp3TT).
         returner = ROWID(ekgp3TT).
      END.
      ELSE  returner = ROWID(ekgp3buff).    
      RETURN returner. 
   END METHOD.
   
   /*NyP2ArbKod*/
   METHOD PUBLIC ROWID NyP2ArbKod(INPUT arbKod AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp2arbkodTT THEN returner = ROWID(ekgp2arbkodTT).
      IF arbKod = "" THEN RETURN returner.
      FIND FIRST ekgp2arbkodbuff WHERE ekgp2arbkodbuff.P2ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp2arbkodbuff THEN DO:
         CREATE ekgp2arbkodTT.
         ASSIGN
         ekgp2arbkodTT.EKGSUBID  = ekgsubidvar 
         ekgp2arbkodTT.EBRKAT = ebrkatvar
         ekgp2arbkodTT.P2ARBKOD  = arbKod 
         ekgp2arbkodTT.BENAMNING = "<Ny Benämning>"
         ekgp2arbkodTT.TTRECID = RECID(ekgp2arbkodTT).
         returner = ROWID(ekgp2arbkodTT).
      END.
      ELSE  returner = ROWID(ekgp2arbkodbuff).    
      RETURN returner. 
   END METHOD.
   
   /*NyP2*/
   METHOD PUBLIC ROWID NyP2LopNr(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp2TT THEN returner = ROWID(ekgp2TT).
      IF arbKod = "" OR lopnr = 0 THEN RETURN returner.
      FIND FIRST ekgp2buff WHERE ekgp2buff.P2ARBKOD = arbKod AND ekgp2buff.P2LOPNR = lopnr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp2buff THEN DO:
         CREATE ekgp2TT.
         ASSIGN
         ekgp2TT.EKGSUBID  = ekgsubidvar 
         ekgp2TT.EBRKAT = ebrkatvar
         ekgp2TT.P2ARBKOD  = arbKod 
         ekgp2TT.P2LOPNR  = lopnr
         ekgp2TT.BENAMNING = "<Ny Benämning>"
         ekgp2TT.TTRECID = RECID(ekgp2TT).
         returner = ROWID(ekgp2TT).
      END.    
      ELSE  returner = ROWID(ekgp2buff).
      RETURN returner. 
   END METHOD.
   
   /*NyP1ArbKod*/
   METHOD PUBLIC ROWID NyP1ArbKod(INPUT arbKod AS CHARACTER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp1arbkodTT THEN returner = ROWID(ekgp1arbkodTT).
      IF arbKod = "" THEN RETURN returner.
      FIND FIRST ekgp1arbkodbuff WHERE ekgp1arbkodbuff.P1ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp1arbkodbuff THEN DO:
         CREATE ekgp1arbkodTT.
         ASSIGN
         ekgp1arbkodTT.EKGSUBID  = ekgsubidvar 
         ekgp1arbkodTT.EBRKAT = ebrkatvar
         ekgp1arbkodTT.P1ARBKOD  = arbKod 
         ekgp1arbkodTT.BENAMNING = "<Ny Benämning>"
         ekgp1arbkodTT.TTRECID = RECID(ekgp1arbkodTT).
         returner = ROWID(ekgp1arbkodTT).
      END.    
      ELSE  returner = ROWID(ekgp1arbkodbuff).  
      RETURN returner. 
   END METHOD.
   
   /*NyP1*/
   METHOD PUBLIC ROWID NyP1LopNr(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER, INPUT ekgsubidvar AS INTEGER, INPUT ebrkatvar AS CHARACTER):
      DEFINE VARIABLE returner AS ROWID NO-UNDO.
      IF AVAILABLE ekgp1TT THEN returner = ROWID(ekgp1TT).
      IF arbKod = "" OR lopnr = 0 THEN RETURN returner.
      FIND FIRST ekgp1buff WHERE ekgp1buff.P1ARBKOD = arbKod AND ekgp1buff.P1LOPNR = lopnr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekgp1buff THEN DO:
         CREATE ekgp1TT.
         ASSIGN
         ekgp1TT.EKGSUBID  = ekgsubidvar 
         ekgp1TT.EBRKAT = ebrkatvar
         ekgp1TT.P1ARBKOD  = arbKod 
         ekgp1TT.P1LOPNR  = lopnr
         ekgp1TT.BENAMNING = "<Ny Benämning>"
         ekgp1TT.TTRECID = RECID(ekgp1TT).
         returner = ROWID(ekgp1TT).
      END.    
      ELSE  returner = ROWID(ekgp1buff).
      RETURN returner. 
   END METHOD.
   
   /*TaBortP5 + delkoder*/  
   METHOD PUBLIC LOGICAL TaBortP5(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      FIND FIRST ekgp4frekvensTT WHERE ekgp4frekvensTT.P5ARBKOD = ekgp5TT.P5ARBKOD NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp4frekvensTT THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(94) + " " + ekgp4frekvensTT.P4ARBKOD, "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.   
      FIND FIRST ekgp5TT WHERE ekgp5TT.P5ARBKOD = arbKod AND ekgp5TT.P5LOPNR = lopnr NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp5TT THEN DO:
         FOR EACH ekgp5resursTT WHERE ekgp5resursTT.P5ARBKOD = arbKod AND ekgp5resursTT.P5LOPNR = lopnr NO-LOCK.
            DELETE ekgp5resursTT.
         END.
         DELETE ekgp5TT.
      END.
      RETURN TRUE.
   END METHOD.
   
   /*TaBortP4 + delkoder*/  
   METHOD PUBLIC LOGICAL TaBortP4(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      FIND FIRST ekgp3frekvensTT WHERE ekgp3frekvensTT.P4ARBKOD = ekgp4TT.P4ARBKOD NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp3frekvensTT THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(94) + " " + ekgp3frekvensTT.P3ARBKOD  + " " + STRING(ekgp3frekvensTT.P3LOPNR), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      FIND FIRST ekgp4TT WHERE ekgp4TT.P4ARBKOD = arbKod AND ekgp4TT.P4LOPNR = lopnr NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp4TT THEN DO:
         FOR EACH ekgp4resursTT WHERE ekgp4resursTT.P4ARBKOD = arbKod AND ekgp4resursTT.P4LOPNR = lopnr NO-LOCK.
            DELETE ekgp4resursTT.
         END.
         FOR EACH ekgp4frekvensTT WHERE ekgp4frekvensTT.P4ARBKOD = arbKod AND ekgp4frekvensTT.P4LOPNR = lopnr NO-LOCK.
            DELETE ekgp4frekvensTT.
         END.
         DELETE ekgp4TT.
      END.
      RETURN TRUE.
   END METHOD.  
     
   /*TaBortP3ArbKod*/  
   METHOD PUBLIC LOGICAL TaBortP3ArbKod(INPUT arbKod AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      FIND FIRST ekgp2frekvensTT WHERE ekgp2frekvensTT.P3ARBKOD = ekgp3arbkodTT.P3ARBKOD NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp2frekvensTT THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(94) + " " + ekgp2frekvensTT.P2ARBKOD + " " + STRING(ekgp2frekvensTT.P2LOPNR), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      FIND FIRST ekgp3arbkodTT WHERE ekgp3arbkodTT.P3ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp3arbkodTT THEN DO:
         FOR EACH ekgp3TT WHERE ekgp3TT.P3ARBKOD = ekgp3arbkodTT.P3ARBKOD NO-LOCK.
            FOR EACH ekgp3resursTT WHERE ekgp3resursTT.P3ARBKOD = ekgp3TT.P3ARBKOD NO-LOCK.
               DELETE ekgp3resursTT.
            END.
            FOR EACH ekgp3frekvensTT WHERE ekgp3frekvensTT.P3ARBKOD = ekgp3TT.P3ARBKOD NO-LOCK.
               DELETE ekgp3frekvensTT.
            END.
            DELETE ekgp3TT.
         END.
         DELETE ekgp3arbkodTT.
      END.    
      RETURN TRUE.
   END METHOD.
   
   /*TaBortP3 + delkoder*/  
   METHOD PUBLIC LOGICAL TaBortP3(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      FIND FIRST ekgp2frekvensTT WHERE ekgp2frekvensTT.P3ARBKOD = ekgp3TT.P3ARBKOD AND ekgp2frekvensTT.P3LOPNR = ekgp3TT.P3LOPNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp2frekvensTT THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(94) + " " + ekgp2frekvensTT.P2ARBKOD + " " + STRING(ekgp2frekvensTT.P2LOPNR), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      FIND FIRST ekgp3TT WHERE ekgp3TT.P3ARBKOD = arbKod AND ekgp3TT.P3LOPNR = lopnr NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp3TT THEN DO:
         FOR EACH ekgp3resursTT WHERE ekgp3resursTT.P3ARBKOD = arbKod AND ekgp3resursTT.P3LOPNR = lopnr NO-LOCK.
            DELETE ekgp3resursTT.
         END.
         FOR EACH ekgp3frekvensTT WHERE ekgp3frekvensTT.P3ARBKOD = arbKod AND ekgp3frekvensTT.P3LOPNR = lopnr NO-LOCK.
            DELETE ekgp3frekvensTT.
         END.
         DELETE ekgp3TT.
      END.
      RETURN TRUE.
   END METHOD.
   
   /*TaBortP2ArbKod*/  
   METHOD PUBLIC LOGICAL TaBortP2ArbKod(INPUT arbKod AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      FIND FIRST ekgp1frekvensTT WHERE ekgp1frekvensTT.P2ARBKOD = ekgp2arbkodTT.P2ARBKOD NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp1frekvensTT THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(94) + " " + ekgp1frekvensTT.P1ARBKOD + " " + STRING(ekgp1frekvensTT.P1LOPNR), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      FIND FIRST ekgp2arbkodTT WHERE ekgp2arbkodTT.P2ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp2arbkodTT THEN DO:
         FOR EACH ekgp2TT WHERE ekgp2TT.P2ARBKOD = ekgp2arbkodTT.P2ARBKOD NO-LOCK.
            FOR EACH ekgp2resursTT WHERE ekgp2resursTT.P2ARBKOD = ekgp2TT.P2ARBKOD NO-LOCK.
               DELETE ekgp2resursTT.
            END.
            FOR EACH ekgp2frekvensTT WHERE ekgp2frekvensTT.P2ARBKOD = ekgp2TT.P2ARBKOD NO-LOCK.
               DELETE ekgp2frekvensTT.
            END.
            FOR EACH ekgp2mtrlTT WHERE ekgp2mtrlTT.P2ARBKOD = ekgp2TT.P2ARBKOD NO-LOCK.
               DELETE ekgp2mtrlTT.
            END.
            DELETE ekgp2TT.
         END.
         DELETE ekgp2arbkodTT.
      END.    
      RETURN TRUE.
   END METHOD.
   
   /*TaBortP2*/  
   METHOD PUBLIC LOGICAL TaBortP2(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      FIND FIRST ekgp1frekvensTT WHERE ekgp1frekvensTT.P2ARBKOD = ekgp2TT.P2ARBKOD AND ekgp1frekvensTT.P2LOPNR = ekgp2TT.P2LOPNR NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp1frekvensTT THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(94) + " " + ekgp1frekvensTT.P1ARBKOD  + " " + STRING(ekgp1frekvensTT.P1LOPNR), "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      FIND FIRST ekgp2TT WHERE ekgp2TT.P2ARBKOD = arbKod AND ekgp2TT.P2LOPNR = lopnr NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp2TT THEN DO:
         FOR EACH ekgp2resursTT WHERE ekgp2resursTT.P2ARBKOD = arbKod AND ekgp2resursTT.P2LOPNR = lopnr NO-LOCK.
            DELETE ekgp2resursTT.
         END.
         FOR EACH ekgp2frekvensTT WHERE ekgp2frekvensTT.P2ARBKOD = arbKod AND ekgp2frekvensTT.P2LOPNR = lopnr NO-LOCK.
            DELETE ekgp2frekvensTT.
         END.
         FOR EACH ekgp2mtrlTT WHERE ekgp2mtrlTT.P2ARBKOD = arbKod AND ekgp2mtrlTT.P2LOPNR = lopnr NO-LOCK.
            DELETE ekgp2mtrlTT.
         END.
         DELETE ekgp2TT.
      END.
      RETURN TRUE.
   END METHOD.
   
   /*TaBortP1ArbKod*/  
   METHOD PUBLIC VOID TaBortP1ArbKod(INPUT arbKod AS CHARACTER):
      FIND FIRST ekgp1arbkodTT WHERE ekgp1arbkodTT.P1ARBKOD = arbKod NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp1arbkodTT THEN DO:
         FOR EACH ekgp1TT WHERE ekgp1TT.P1ARBKOD = ekgp1arbkodTT.P1ARBKOD NO-LOCK.
           FOR EACH ekgp1frekvensTT WHERE ekgp1frekvensTT.P1ARBKOD = ekgp1TT.P1ARBKOD NO-LOCK.
               DELETE ekgp1frekvensTT.
           END.
           DELETE ekgp1TT.
         END.
         DELETE ekgp1arbkodTT.
      END.    
   END METHOD.
   
   /*TaBortP1 + delkoder*/  
   METHOD PUBLIC VOID TaBortP1(INPUT arbKod AS CHARACTER, INPUT lopnr AS INTEGER):
      FIND FIRST ekgp1TT WHERE ekgp1TT.P1ARBKOD = arbKod AND ekgp1TT.P1LOPNR = lopnr NO-LOCK NO-ERROR.
      IF AVAILABLE ekgp1TT THEN DO:
         FOR EACH ekgp1frekvensTT WHERE ekgp1frekvensTT.P1ARBKOD = arbKod AND ekgp1frekvensTT.P1LOPNR = lopnr NO-LOCK.
            DELETE ekgp1frekvensTT.
         END.
         DELETE ekgp1TT.
      END.
   END METHOD. 
   
   /*TaBortResurs*/  
   METHOD PUBLIC VOID TaBortResurs(INPUT resnr AS INTEGER):
   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
   DEFINE VARIABLE reskoll AS CHARACTER .    
   rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(119),"", System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
      IF rrr:ToString() = "Yes" THEN DO:    
         RUN kollresurs IN AppServerHandle (INPUT resnr , OUTPUT reskoll). 
         IF reskoll NE "" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(120) + " " + reskoll , "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
            RETURN.
         END.
         ELSE DO:
            FOR EACH ekgresursTT WHERE ekgresursTT.RESURSNR = resnr NO-LOCK:
               DELETE ekgresursTT.
            END.
            ResursSpara().           
         END.
      END.      
   END METHOD.
   
   
   
   METHOD PUBLIC LOGICAL TaBortResurspris(INPUT RESNR AS INTEGER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      DEFINE VARIABLE reskoll AS CHARACTER NO-UNDO.
      reskoll = "".
      FIND FIRST ekgp5resursTT WHERE  ekgp5resursTT.RESURSNR = RESNR NO-ERROR.
      FIND FIRST ekgp4resursTT WHERE  ekgp4resursTT.RESURSNR = RESNR NO-ERROR.
      FIND FIRST ekgp3resursTT WHERE  ekgp3resursTT.RESURSNR = RESNR NO-ERROR.
      FIND FIRST ekgp2resursTT WHERE  ekgp2resursTT.RESURSNR = RESNR NO-ERROR.
      IF AVAILABLE ekgp5resursTT THEN reskoll = "P5arbkod " + ekgp5resursTT.P5ARBKOD.
      IF AVAILABLE ekgp4resursTT THEN reskoll = "P4arbkod " + ekgp4resursTT.P4ARBKOD.
      IF AVAILABLE ekgp3resursTT THEN reskoll = "P3arbkod " + ekgp3resursTT.P3ARBKOD + " " + STRING(ekgp3resursTT.P3LOPNR).
      IF AVAILABLE ekgp2resursTT THEN reskoll = "P2arbkod " + ekgp2resursTT.P2ARBKOD + " " + STRING(ekgp2resursTT.P2LOPNR).
      IF reskoll NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(103) + " " + reskoll , "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      ELSE DO:   
         FOR EACH ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = RESNR NO-LOCK:
            DELETE ekgresursprisTT.
         END.   
         FOR EACH ekgresursnivaTT WHERE ekgresursnivaTT.RESURSNR = RESNR:
            DELETE ekgresursnivaTT.
         END.   
         RETURN TRUE.
      END.
   END METHOD.
   
   /*TaBortMtrl*/  
   METHOD PUBLIC LOGICAL TaBortMtrl(INPUT artnr AS CHARACTER):
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.      
      DEFINE VARIABLE reskoll AS CHARACTER NO-UNDO.
      reskoll = "".
      FIND FIRST ekgp2mtrlTT WHERE  ekgp2mtrlTT.ARTNR = artnr NO-ERROR.      
      IF AVAILABLE ekgp2mtrlTT THEN reskoll = "P2arbkod " + ekgp2mtrlTT.P2ARBKOD + " " + STRING(ekgp2mtrlTT.P2LOPNR).
      IF reskoll NE "" THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(124) + " " + reskoll , "",System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Information).
         RETURN FALSE.
      END.
      ELSE DO:   
         FOR EACH ekgmtrlTT WHERE ekgmtrlTT.ARTNR = artnr NO-LOCK:
            DELETE ekgmtrlTT.
         END.      
         RETURN TRUE.
      END.      
   END METHOD.
   
   /*TaBortPaslag*/  
   METHOD PUBLIC VOID TaBortPaslag(INPUT paslagnr AS INTEGER):
      FOR EACH ekgpaslagTT WHERE ekgpaslagTT.paslagNR = paslagnr NO-LOCK:
         DELETE ekgpaslagTT.
      END.
   END METHOD.
   
   
   METHOD PUBLIC VOID HamtaResursFor(INPUT niva AS INTEGER):  
      EMPTY TEMP-TABLE ekgresursForTT NO-ERROR.
      FOR EACH ekgresursnivaTT WHERE ekgresursnivaTT.NIVA = niva.
         FIND FIRST ekgresursTT WHERE ekgresursTT.RESURSNR = ekgresursnivaTT.RESURSNR AND ekgresursTT.AKTIV = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ekgresursTT THEN DO:
            FIND FIRST ekgresursprisTT WHERE ekgresursprisTT.RESURSNR = ekgresursnivaTT.RESURSNR  NO-LOCK NO-ERROR.
            IF AVAILABLE ekgresursprisTT THEN DO:
               CREATE ekgresursForTT.
               ASSIGN
               ekgresursForTT.RESURSNR  = ekgresursTT.RESURSNR
               ekgresursForTT.BENAMNING = ekgresursTT.BENAMNING
               ekgresursForTT.ENHET = ekgresursTT.ENHET
               ekgresursForTT.REGELID = ekgresursprisTT.REGELID
               ekgresursForTT.PRIS        = ekgresursprisTT.PRIS
               ekgresursForTT.EA         = ekgresursprisTT.EA
               ekgresursForTT.TTRECID = RECID(ekgresursForTT).
               FIND FIRST ekgregeltt WHERE ekgregeltt.REGELID = ekgresursprisTT.REGELID NO-LOCK NO-ERROR.
               IF AVAILABLE ekgregeltt  THEN DO:            
                  ekgresursForTT.UTOVERFREK = ekgregeltt.UTOVERFREK.
               END.
            END.
          END.            
       END.
   END METHOD.
   
   METHOD PUBLIC VOID LastUppGammalKat(INPUT ekgsubid AS INTEGER): 
        FOR EACH ekgsubkatalogerTTbuff WHERE ekgsubkatalogerTTbuff.ANVANDSAV = Guru.Konstanter:globanv NO-LOCK:
         IF ekgsubkatalogerTTbuff.EKGSUBID = ekgsubid THEN.
         ELSE DO:
            THIS-OBJECT:ControlWinLg:AnvandsAvOff(STRING(ekgsubkatalogerTTbuff.EKGSUBID)).
         END.   
      END.                       
   END METHOD.
   
   METHOD PUBLIC LOGICAL LastKat(INPUT ekgsubid AS INTEGER): 
      DEFINE VARIABLE lastkat AS LOGICAL.   
      RUN LastKat IN AppServerHandle (INPUT ekgsubid, OUTPUT lastkat).  
      RETURN lastkat.          
   END METHOD.
   
  METHOD PUBLIC LOGICAL AnvandsKat(INPUT ekgsubid AS INTEGER): 
      DEFINE VARIABLE anvandskat AS LOGICAL.    
      RUN AnvandsKat IN AppServerHandle (INPUT ekgsubid, OUTPUT anvandskat).
      RETURN anvandskat.       
   END METHOD.
   
  METHOD PUBLIC CHARACTER AnvandsAv(INPUT ekgsubid AS INTEGER): 
      DEFINE VARIABLE anvandsAv AS CHARACTER.    
      RUN AnvandsAv IN AppServerHandle (INPUT ekgsubid, OUTPUT anvandsAv).
      RETURN anvandsAv.       
   END METHOD.
   
   METHOD PUBLIC VOID TidutHmt(vad AS CHARACTER):
      CREATE tidut.
      tidut.UT = vad.  
      tidut.TTRECID = RECID(tidut).            
   END METHOD.
   
   METHOD PUBLIC VOID ArbKoderSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGArbKoderTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGArbKoderDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGArbKoderDS:HANDLE).
      RUN SparaProDataSetEKGArbKoderDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGArbKoderDS:HANDLE).
      THIS-OBJECT:EKGArbKoderTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID ResursSpara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGResursTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGResursDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGResursDS:HANDLE).
      RUN SparaProDataSetEKGResursDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGResursDS:HANDLE).
      THIS-OBJECT:EKGResursTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID Katalogresursspara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGKatalogResursTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGKatalogResursDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGKatalogResursDS:HANDLE).
      RUN SparaProDataSetEKGKatalogResursDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGKatalogResursDS:HANDLE).
      THIS-OBJECT:EKGKatalogResursTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID Subkatspara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGSubKatTracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGSubKatDS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGSubKatDS:HANDLE).
      RUN SparaProDataSetEKGSubKatDS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGSubKatDS:HANDLE).
      THIS-OBJECT:EKGSubKatTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID P5Spara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGP5Tracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGP5DS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGP5DS:HANDLE).
      RUN SparaProDataSetEKGP5DS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGP5DS:HANDLE).
      THIS-OBJECT:EKGP5Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID P4Spara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGP4Tracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGP4DS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGP4DS:HANDLE).
        
      RUN SparaProDataSetEKGP4DS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGP4DS:HANDLE).
      THIS-OBJECT:EKGP4Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID P3Spara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGP3Tracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGP3DS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGP3DS:HANDLE).
      RUN SparaProDataSetEKGP3DS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGP3DS:HANDLE).
      THIS-OBJECT:EKGP3Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID P2Spara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGP2Tracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGP2DS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGP2DS:HANDLE).
      RUN SparaProDataSetEKGP2DS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGP2DS:HANDLE).
      THIS-OBJECT:EKGP2Tracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID P1Spara():
      DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
      THIS-OBJECT:EKGP1Tracking(FALSE).
      CREATE DATASET hDSChanges.
      hDSChanges:CREATE-LIKE (DATASET EKGP1DS:HANDLE).
      hDSChanges:GET-CHANGES (DATASET EKGP1DS:HANDLE).
      RUN SparaProDataSetEKGP1DS IN AppServerHandle(INPUT DATASET-HANDLE hDSChanges).
      hDSChanges:MERGE-CHANGES(DATASET EKGP1DS:HANDLE).
      THIS-OBJECT:EKGP1Tracking(TRUE).
   END METHOD.
     
   DESTRUCTOR EkgDatadb():
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle.
   END DESTRUCTOR.
END CLASS.