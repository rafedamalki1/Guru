&Scoped-define NEW NEW
   {GLOBVAR2DEL1.I}
  



   



{EXECLIN2.I}
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


DEFINE TEMP-TABLE TIDUT NO-UNDO
   FIELD UT AS CHARACTER.
CREATE TIDUT.
ASSIGN
   TIDUT.UT = "123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456789A123456b". 
CREATE TIDUT.
ASSIGN
   TIDUT.UT = "123456789A123456789b123456789c123456789A123456789A123456789A123456789v123456789s123456789x123456789Abbbb".

PROCEDURE t_UI :
   DEFINE INPUT PARAMETER tt AS CHARACTER NO-UNDO.
   CREATE TIDUT.
ASSIGN
TIDUT.UT = tt.
END PROCEDURE.
RUN t_ui (INPUT "430113-1035 13032 Kent Niklasson").                                                                                                                                                                                                                                                                           
RUN t_ui (INPUT "Sjuk                  20061208       20061208          0.250  1 13:30 15:30").
RUN t_ui (INPUT "Sjuk                  20061211       20061215          1.000  5").                                                                                                                                                                                                                                            
RUN t_ui (INPUT "Sembet                20061227       20061229          1.000  3").                                                                                                                                                                                                                                            
RUN t_ui (INPUT "440928-2938 13033 Jan Gustavsson").                                                                                                                                                                                                                                                                           
RUN t_ui (INPUT "Sembet                20061201       20061222          1.000  16").                                                                                                                                                                                                                                           
RUN t_ui (INPUT "Atidkont              20061227       20061229   024.00        3").                                                                                                                                                                                                                                            
RUN t_ui (INPUT "450416-1946 16101 Lillemor Sjögren").                                                                                                                                                                                                                                                                         
RUN t_ui (INPUT "Sembet                20061204       20061204          1.000  1").                                                                                                                                                                                                                                            
RUN t_ui (INPUT "Sembet                20061219       20061219          1.000  1").                                                                                                                                                                                                                                            
RUN t_ui (INPUT "450806-3031 13003 Åke Petersson").                                                                                                                                                                                                                                                                            
RUN t_ui (INPUT "Sembet                20061201       20061201          1.000  1").                                                                                                                                                                                                                                            
RUN t_ui (INPUT "Atidkont              20061213       20061213   01.00         1 15:15 16:15").                                                                                                                                                                                                                                
RUN t_ui (INPUT "Atidkont              20061214       20061214   03.00         1 13:15 16:15").  
RUN EXLONFIL3.P (INPUT 0 , INPUT "GKAL", INPUT TABLE tidut, INPUT FALSE).  
