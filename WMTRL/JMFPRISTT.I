
/*------------------------------------------------------------------------
    File        : JMFPRISTT.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 19 15:21:09 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/



   /*  START  TABELL enrjmf
    
    
    Guru.GlobalaVariabler:jmfPrisEnrTTh =  TEMP-TABLE enrjmf:HANDLE:DEFAULT-BUFFER-HANDLE.
    båda är buffers 
    jmfPrisEnrTTh     START TABBEL
    jmfPrisEnrBuffTTh
    
   jmfLevantorTThout DE LEVERANTÖRER SOM SKA VARA MED I VALLISTORNA
   jmfLevantorBuffh
   
    jmfenrTT SÖKTA ENR NU MED PRIS OCH LEV
    SKAPAS I NETTOMARK EN POST FÖR VARJE ENR OCH LEVERANTÖR AV DE FYRA SOM FINNS I rubrikTT.
    
      
    jmfenrTTh        = TEMP-TABLE jmfenrTT:HANDLE:DEFAULT-BUFFER-HANDLE. POSTER TILL VARJE COLUMN
    rubrikTTh        = TEMP-TABLE rubrikTT:HANDLE:DEFAULT-BUFFER-HANDLE. FÖR VARJE KOLUMN
     /*DE POSTER MAN NU KAN JMFÖRA*/
     MtrlJmfPrisTTh   = TEMP-TABLE MtrlJmfPrisTT:HANDLE:DEFAULT-BUFFER-HANDLE. GRIDDEN
      till kolumnerna i grideden
    rubrikTT  ORDNING OCH NAMN PÅ LEVERANTÖRER SOM ÄR VALDA I CMB
    MtrlJmfPrisTTh visas i grid
   */
DEFINE TEMP-TABLE jmfenrTT NO-UNDO
   FIELD ENR AS CHARACTER
   FIELD ENRLEV AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD LEVKOD AS CHARACTER
   FIELD PRIS AS DECIMAL
   INDEX  LEVENR LEVKOD  ENR.
   
DEFINE TEMP-TABLE rubrikTT NO-UNDO
   FIELD ORDNING AS INTEGER
   FIELD LEVKOD AS CHARACTER
   FIELD LEVNAMN AS CHARACTER
   INDEX ORDNING IS PRIMARY ORDNING
   INDEX LEVENR LEVKOD.
