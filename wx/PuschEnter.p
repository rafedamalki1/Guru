
/*------------------------------------------------------------------------
    File        : PuschEnter.p
    Purpose     : 

    Syntax      :run Modules\Beredning\PuschEnter.p 

    Description : 

    Author(s)   : 
    Created     : Tue Sep 15 15:39:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE VARIABLE PuschEnter   AS Modules.Beredning.FillEnter NO-UNDO.
RUN PuschEnter_UI.
PROCEDURE PuschEnter_UI :
   PuschEnter = NEW Modules.Beredning.FillEnter().
   WAIT-FOR PuschEnter:ShowDialog().
END PROCEDURE.