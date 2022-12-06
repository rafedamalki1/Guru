/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: XACCESS.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.03.19 10:29 ELPAO   
     Modified: 
*/
{ODBCTEMP.I}
DEFINE TEMP-TABLE feltemp NO-UNDO
   FIELD FELTEXT AS CHARACTER.
DEFINE VARIABLE odbch AS HANDLE NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.


/*RUN ODBC.P PERSISTENT SET odbch (INPUT "X:\pro9\guru\Wextra\AvCad.mdb", INPUT "localhost", INPUT "", INPUT "", OUTPUT TABLE feltemp).*/

RUN ODBC.P PERSISTENT SET odbch (INPUT "SIB",
                                 INPUT "127.0.0.1",
                                 INPUT "test",
                                 INPUT "test",
                                 OUTPUT TABLE feltemp).

kommando = "matreg".
RUN hamtap_UI IN odbch (INPUT kommando,OUTPUT TABLE sqldat).


/*RUN ODBC.P PERSISTENT SET odbch (INPUT "julia.capitex.se",
                                 INPUT "195.67.71.108",       
                                 INPUT "eo_result",           
                                 INPUT "FRiED6",              
                                 OUTPUT TABLE feltemp).      
  */

FOR EACH feltemp NO-LOCK:
   DISP feltemp.
END.

/*"ODBC;DSN=ems2mov_local;HOST=127.0.0.1;PORT=15500;DATABASE=ems2mov;UID='admin';PWD='admin';;TABLE='pub.ped-venda'" */
                                
/*
                                
systemerror 

   bfposto positionstidsstämpel
   2013150 matchar INTE listtidsstämpeln
   ta bort i ändra i materiel

   konstruktion materiel ändra*/
