 
 /*------------------------------------------------------------------------
    File        : InfraInstall
    Purpose     : Dummyklass för att testa om infragistics finns
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Tue Apr 24 15:01:28 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Helpers.InfraInstall INHERITS Infragistics.Win.UltraWinGrid.UltraCombo:
   
   CONSTRUCTOR PUBLIC InfraInstall (  ):
      SUPER ().
   END CONSTRUCTOR.   
   
   DESTRUCTOR PUBLIC InfraInstall ( ):

   END DESTRUCTOR.
END CLASS.