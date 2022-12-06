 
 /*------------------------------------------------------------------------
    File        : DatabaseManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Feb 13 12:51:19 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.DatabaseManager:
    {BerKalktt.I}
   DEFINE PUBLIC VARIABLE Root AS Guru.Root.
   DEFINE PUBLIC VARIABLE Excelblad AS Modules.Global.Excelfunc.
   DEFINE PUBLIC VARIABLE Kalkyl AS Modules.Kalkyl.Kalkyldb.
/*
   DEFINE PUBLIC VARIABLE Arende AS Modules.Arende.Arendedb.
  */
   /*
   DEFINE PUBLIC VARIABLE KalkylAdm AS Modules.Kalkyl.KalkylAdmdb.
   */
   DEFINE PUBLIC VARIABLE KalkylAdm AS Modules.KalkylAdm.KalkylAdmdb.
   DEFINE PUBLIC VARIABLE EkgData AS Modules.EkgData.EkgDatadb.
   DEFINE PUBLIC VARIABLE KabelAnd AS Modules.Beredning.KabelAndDb.
   DEFINE PUBLIC VARIABLE UtbyLAdm AS  Modules.Beredning.UtbytListaAdmDb.
/*
   DEFINE PUBLIC VARIABLE KabelSkap AS Modules.Beredning.KabelSkapDb.
  */
   DEFINE PUBLIC VARIABLE MtrlJmfPris AS Modules.Materiel.MtrlJmfPrisDb.
   DEFINE PUBLIC VARIABLE GPL AS Modules.Projekt.GPLDb.
   DEFINE PUBLIC VARIABLE Storning AS Modules.Storning.StorningDb.
   DEFINE PUBLIC VARIABLE DarwinPlusUser AS Modules.Storning.DarwinPlusUserDb.
   DEFINE PUBLIC VARIABLE Global AS Modules.Global.Globaldb.
   DEFINE PUBLIC VARIABLE Sprak AS Modules.Register.Sprakdb.
   DEFINE PUBLIC VARIABLE AnvSparr AS Modules.Register.AnvSparrdb.
    
   CONSTRUCTOR PUBLIC DatabaseManager (INPUT r AS Guru.Root) :
     
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Global = NEW Modules.Global.Globaldb(THIS-OBJECT:Root).
     
   END CONSTRUCTOR.
   DESTRUCTOR PUBLIC DatabaseManager():
     DELETE OBJECT  THIS-OBJECT:Kalkyl NO-ERROR.     
     DELETE OBJECT  THIS-OBJECT:KalkylAdm NO-ERROR.
/*
     DELETE OBJECT  THIS-OBJECT:Arende NO-ERROR.
  */
     DELETE OBJECT  THIS-OBJECT:EkgData NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:KabelAnd NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:GPL NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:Storning NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:DarwinPlusUser NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:Global NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:AnvSparr NO-ERROR.
     DELETE OBJECT  THIS-OBJECT:Sprak NO-ERROR.
     THIS-OBJECT:Global = ?.
     THIS-OBJECT:AnvSparr = ?.
     THIS-OBJECT:Kalkyl = ?.     
     THIS-OBJECT:KalkylAdm = ?.
    /*
     THIS-OBJECT:Arende = ?.
     */
     THIS-OBJECT:EkgData = ?.
     THIS-OBJECT:KabelAnd = ?. 
     THIS-OBJECT:GPL = ?. 
     THIS-OBJECT:Storning = ?.
     THIS-OBJECT:DarwinPlusUser = ?. 
     THIS-OBJECT:Sprak = ?.
     
   END DESTRUCTOR.
   METHOD PUBLIC VOID Excelblad():
      THIS-OBJECT:Excelblad = NEW  Modules.Global.Excelfunc(THIS-OBJECT:Root). 
   END METHOD. 

   METHOD PUBLIC VOID KalkylDb():
      /*skall sättas vi varje ny dbmanager
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      alt Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      */
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      THIS-OBJECT:Kalkyl = NEW Modules.Kalkyl.Kalkyldb(THIS-OBJECT:Root). 
   END METHOD.
    METHOD PUBLIC VOID KalkylDb(INPUT startvar AS CHARACTER):
      /*skall sättas vi varje ny dbmanager
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      alt Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      */
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      THIS-OBJECT:Kalkyl = NEW Modules.Kalkyl.Kalkyldb(THIS-OBJECT:Root, INPUT startvar). 
   END METHOD.
   METHOD PUBLIC LOGICAL KalkylDb(INPUT TABLE berkalktt,bernumval AS INTEGER,sidnrval AS INTEGER,visadirekt AS LOGICAL):
      
      /*skall sättas vi varje ny dbmanager
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      alt Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      */
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      THIS-OBJECT:Kalkyl = NEW Modules.Kalkyl.Kalkyldb(THIS-OBJECT:Root,INPUT TABLE berkalktt,bernumval,sidnrval,visadirekt). 
      RETURN Guru.GlobalaVariabler:KalkylSomStartiBer.
   END METHOD.
/*
   METHOD PUBLIC VOID ArendeDb():
      /*skall sättas vi varje ny dbmanager
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      alt Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      */
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      THIS-OBJECT:Arende = NEW Modules.Arende.Arendedb(THIS-OBJECT:Root). 
   END METHOD. 
  */
   METHOD PUBLIC VOID KalkylAdmDb():
      /*skall sättas vi varje ny dbmanager
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      alt Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      /*
      THIS-OBJECT:KalkylAdm = NEW Modules.Kalkyl.KalkylAdmDb(THIS-OBJECT:Root).
      */ 
      THIS-OBJECT:KalkylAdm = NEW Modules.KalkylAdm.KalkylAdmDb(THIS-OBJECT:Root).
   END METHOD. 
   METHOD PUBLIC VOID UtblAdmDb():
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:UtbyLAdm = NEW  Modules.Beredning.UtbytListaAdmDb(THIS-OBJECT:Root).
   END METHOD.
   METHOD PUBLIC VOID EkgDataDb():
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:EkgData = NEW Modules.EkgData.EkgDatadb(THIS-OBJECT:Root).
       
   END METHOD.
   METHOD PUBLIC VOID KabelAndDb():
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:KabelAnd = NEW Modules.Beredning.KabelAndDb(THIS-OBJECT:Root).
       
   END METHOD.
/*
   METHOD PUBLIC VOID KabelSkapDb():
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:KabelSkap = NEW Modules.Beredning.KabelSkapDb(THIS-OBJECT:Root).
       
   END METHOD. 
  */
   METHOD PUBLIC VOID MtrlJmfPrisDb():
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:MtrlJmfPris = NEW Modules.Materiel.MtrlJmfPrisDb(THIS-OBJECT:Root).
       
   END METHOD.
   
   METHOD PUBLIC VOID AnvSparrDb():
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:AnvSparr = NEW Modules.Register.AnvSparrDb(THIS-OBJECT:Root).
       
   END METHOD.
   METHOD PUBLIC VOID GPLDb(aonrvar AS CHARACTER, delnrvar AS INTEGER, plidvar AS INTEGER):
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:GPL = NEW Modules.Projekt.GPLDb(THIS-OBJECT:Root, aonrvar, delnrvar, plidvar).
       
   END METHOD.
   
   METHOD PUBLIC VOID StorningDb():
      /*skall sättas vi varje ny dbmanager            */
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:Storning = NEW Modules.Storning.StorningDb(THIS-OBJECT:Root).
       
   END METHOD.
   METHOD PUBLIC VOID DarwinPlusUserDb():
      Guru.GlobalaVariabler:GuruDefaultAnv = Guru.Konstanter:globanv.
      THIS-OBJECT:DarwinPlusUser = NEW Modules.Storning.DarwinPlusUserDb(THIS-OBJECT:Root).
   END METHOD.
   METHOD PUBLIC VOID sprakdb():
      Guru.GlobalaVariabler:GuruDefaultAnv = "".
      THIS-OBJECT:Sprak = NEW Modules.Register.Sprakdb(THIS-OBJECT:Root).
   END METHOD.
    

END CLASS.