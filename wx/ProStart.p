/*ProStart.p*/

RUN Bvisa_UI.
PROCEDURE Bvisa_UI :
  
   DEFINE VARIABLE byggvis   AS Modules.Beredning.Gridtest NO-UNDO.  
   
   byggvis = NEW Modules.Beredning.Gridtest().
  
/*   
   byggvis:InitiateByggPro().
  */
   WAIT-FOR byggvis:ShowDialog().
  
   DELETE OBJECT byggvis NO-ERROR.
   RETURN.

END PROCEDURE.


   

   