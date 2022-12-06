
/*---------------------------SchacktHelpStart.p--------------------------------------------
    File        : SchacktHelpStart.p
    Purpose     : 

    Syntax      :run Modules\Beredning\SchacktHelpStart.p 

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/


PROCEDURE Start_UI :
   DEFINE INPUT  PARAMETER SchaktProtDSh AS HANDLE NO-UNDO.
   DEFINE INPUT  PARAMETER antalDStabeller AS INTEGER NO-UNDO.
   
   DEFINE VARIABLE schaktvis   AS Modules.Beredning.Schaktvisning NO-UNDO.
   DEFINE VARIABLE VisaSchroot AS Guru.Root NO-UNDO.
   VisaSchroot = NEW Guru.Root().
   VisaSchroot:VisaSchakt(THIS-PROCEDURE).
   schaktvis = NEW Modules.Beredning.Schaktvisning(INPUT VisaSchroot).
   schaktvis:ConnectDataset(INPUT DATASET-HANDLE SchaktProtDSh BIND, INPUT antalDSTabeller).
   schaktvis:InitiateSchaktVis().
   WAIT-FOR schaktvis:ShowDialog().
   DELETE OBJECT VisaSchroot NO-ERROR.
   DELETE OBJECT schaktvis NO-ERROR.
   schaktvis = ?.
   schaktvis = ?.
END PROCEDURE.

