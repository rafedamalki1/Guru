
/* SparaDynDSstarMulti.I*/

/*HÄMTA SAMMA DS FLERA GÅNGER MEN INTE MED ATT SPARA KALKBERAPPDS.p FÖR ATT VISA FLERA BEREDNINGAR IHOP*/

DEFINE VARIABLE dyndamicDSMh AS HANDLE NO-UNDO EXTENT 10.

   
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[1].    
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[2].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[3].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[4].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[5].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[6].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[7].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[8].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[9].
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSMh[10].
