
/*------------------------------------------------------------------------
    File        : InDarwinDS.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jun 19 16:20:51 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/
   IF VALID-HANDLE(InDarwinDS) THEN DO:
      InDarwinDS:EMPTY-DATASET().
      RUN GetDatasetDeftt_UI ("InDarwinDS").
      DatasetDeftt.pcKeyValue[1] = "AVDELNING.AVDELNINGNR = " + STRING(vad) + " AND AVDELNING.KOSTMASK = 1 ".
      DatasetDeftt.pcKeyValue[2] = "STORDISTRIKT.AVDELNINGNR = " + STRING(vad) + " AND STORDISTRIKT.ARTAL = " + STRING(vadi).
      RUN ReloadDs_UI IN dyndamicDSh 
      ({DataSetInput.I} OUTPUT DATASET-HANDLE InDarwinDS BIND).
   END.   
   ELSE DO:
      RUN DistriktCreate_UI.
      RUN GetDatasetDeftt_UI ("InDarwinDS").  
      DatasetDeftt.antaltab = 6.
      DatasetDeftt.pcBuffers[1] = STRING(ForetagDBbuffh).
      DatasetDeftt.pcBuffers[2] = STRING(DistriktDBbuffh). 
      DatasetDeftt.pcBuffers[3] = STRING(KundDataDBbuffh). 
      DatasetDeftt.pcBuffers[4] = STRING(LedninggsDataDBbuffh).
      DatasetDeftt.pcBuffers[5] = STRING(StorningsDataDBbuffh).
      DatasetDeftt.pcBuffers[6] = STRING(StorFelMtrlDBbuffh). 
      DatasetDeftt.pcRelTables[1] = "1,2".
      DatasetDeftt.pcRelTables[2] = "2,3".
      DatasetDeftt.pcRelTables[3] = "2,4".
      DatasetDeftt.pcRelTables[4] = "2,5".
      DatasetDeftt.pcRelTables[5] = "5,6".
      
      DatasetDeftt.pcRelFields[1] = "AVDELNINGNR,AVDELNINGNR".
      DatasetDeftt.pcRelFields[2] = "DISTRIKTID,DISTRIKTID".
      DatasetDeftt.pcRelFields[3] = "DISTRIKTID,DISTRIKTID".
      DatasetDeftt.pcRelFields[4] = "DISTRIKTID,DISTRIKTID".
      DatasetDeftt.pcRelFields[5] = "STORNUMMERID,STORNUMMERID".
      DatasetDeftt.pcSources[1] = "AVDELNING".
      DatasetDeftt.pcSources[2] = "STORDISTRIKT".
      DatasetDeftt.pcSources[3] = "KUNDSTOR".
      DatasetDeftt.pcSources[4] = "LEDNINGSDATA".
      DatasetDeftt.pcSources[5] = "STORNINGSTAB".
      DatasetDeftt.pcSources[6] = "STORFELMTRL".
      DatasetDeftt.pcSourceKeys[1] = "AVDELNINGNR".
      DatasetDeftt.pcSourceKeys[2] = "DISTRIKTID".
      DatasetDeftt.pcSourceKeys[3] = "DISTRIKTID,SPANDARWINID,ARTAL".
      DatasetDeftt.pcSourceKeys[4] = "DISTRIKTID,SPANDARWINID,ARTAL,NATUPPLAGGID1,NATUPPLAGGID2,NATUPPLAGGID3,NATUPPLAGGID4".
      DatasetDeftt.pcSourceKeys[5] = "DISTRIKTID,STORNUMMERID".
      DatasetDeftt.pcSourceKeys[6] = "STORNUMMERID,SFMID".
      DatasetDeftt.pcKeyValue[1] = "AVDELNING.AVDELNINGNR = " + STRING(vad) + " AND AVDELNING.KOSTMASK = 1 ".
      DatasetDeftt.pcKeyValue[2] = "STORDISTRIKT.AVDELNINGNR = " + STRING(vad) + " AND STORDISTRIKT.ARTAL = " + STRING(vadi).
      
      DatasetDeftt.repMode = FALSE.
      DatasetDeftt.nestMode = TRUE.
      /*
      RUN DefAndLoadDs_UI  IN dyndamicDSh
      ({DataSetInput.I} OUTPUT DATASET-HANDLE InDarwinDS BIND).
     */
     
      RUN DefAndLoadDsRe_UI  IN dyndamicDSh
      ({DataSetInput.I} OUTPUT DATASET-HANDLE InDarwinDS BIND).
     
   END.