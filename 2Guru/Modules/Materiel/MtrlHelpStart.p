

/*------------------------------------------------------------------------
    File        : MtrlHelpStart.p
    Purpose     : 
                 METHOD PUBLIC STATIC  VOID MtrlHelpStart():
                     IF NOT VALID-HANDLE(MtrlClasserStart) THEN DO:
                        RUN Modules\Materiel\MtrlHelpStart.p PERSISTENT SET MtrlClasserStart.
                     END.  
      
                   END METHOD.

                    Guru.Konstanter:MtrlHelpStart().
                    RUN jmfmtrpris_UI IN Guru.Konstanter:MtrlClasserStart.
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/



PROCEDURE jmfmtrpris_UI :
   DEFINE VARIABLE startjmfprisroot AS Guru.Root NO-UNDO.
  

   RUN jmfmtrprisFORM_UI.
   /*
   IF startjmfprisroot = startjmfprisroot THEN RETURN.
    
   
   /*f�r windows,dbmanager mm*/
   startjmfprisroot = NEW Guru.Root().
   /*kopplar till r�tt db.cls via dbmanager*/
   startjmfprisroot:StartMtrlJmfPrisDb().
   /*k�r connect i db.cls*/
   startjmfprisroot:StartMtrlJmfPrisSaldo(INPUT THIS-PROCEDURE).
   /*k�r MP.cls som k�r INiTT f�r att h�mta tt*/
   
   /*
   DEFINE VARIABLE MtrlJmfPris   AS Modules.Materiel.MtrlJmfPrisSaldo NO-UNDO.
   MtrlJmfPris = NEW Modules.Materiel.MtrlJmfPrisSaldo(INPUT startjmfprisroot).
   WAIT-FOR MtrlJmfPris:ShowDialog().
   */
   EMPTY TEMP-TABLE rubrikTT NO-ERROR. 
   EMPTY TEMP-TABLE jmfenrTT NO-ERROR. 
   DELETE OBJECT startjmfprisroot NO-ERROR.
   startjmfprisroot = ?.
   
   */
/*
   DELETE OBJECT MtrlJmfPris NO-ERROR.
   MtrlJmfPris = ?.
   */
END PROCEDURE.

PROCEDURE jmfmtrprisFORM_UI :
   DEFINE VARIABLE MtrlJmfPris   AS Modules.Materiel.MtrlJmfPris NO-UNDO.
   DEFINE VARIABLE startjmfprisroot AS Guru.Root NO-UNDO. 
   
   /*f�r windows,dbmanager mm*/
   startjmfprisroot = NEW Guru.Root().
   /*kopplar till r�tt db.cls via dbmanager*/
   startjmfprisroot:StartMtrlJmfPrisDb().
   /*k�r connect i db.cls*/
   startjmfprisroot:StartMtrlJmfPris().
   /*k�r MP.cls som k�r INiTT f�r att h�mta tt*/
   MtrlJmfPris = NEW Modules.Materiel.MtrlJmfPris(INPUT startjmfprisroot).
   WAIT-FOR MtrlJmfPris:ShowDialog().
   DELETE OBJECT startjmfprisroot NO-ERROR.
   startjmfprisroot = ?.
   DELETE OBJECT MtrlJmfPris NO-ERROR.
   MtrlJmfPris = ?.
END PROCEDURE.

