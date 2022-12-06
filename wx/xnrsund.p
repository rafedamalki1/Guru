RUN cc_UI (INPUT "39014").             /* De projektnummer som ska ändras är:                             */
RUN cc_UI (INPUT "39015").             /* Arbetsart=Pr-avtal 1 Sundsvall; 39014 - 39017, 39034, 39036     */
RUN cc_UI (INPUT "39016").             /* Arbetsart=Pr-avtal 2 Sundsvall; 39001 - 39013, 39035, 39037     */
RUN cc_UI (INPUT "39017").             /* Arbetsart=Pr-avtal 3 Sundsvall; 39061 - 39063                   */
RUN cc_UI (INPUT "39034").             /* Arbetsart=Pr-avtal 4 Sundsvall; 39046 - 39058                   */
RUN cc_UI (INPUT "39036").             /* Arbetsart=Pr-avtal 1 Timrå; 39018 - 39020, 39025, 39030, 39038  */
RUN cc_UI (INPUT "39001").             /* Arbetsart=Pr-avtal 2 Timrå; 39032, 39039 - 39045, 39060         */
RUN cc_UI (INPUT "39002").             /* Arbetsart=Pr-avtal 1 Ånge; 39021 - 39024                        */
RUN cc_UI (INPUT "39003").             /* Arbetsart=Pr-avtal 2 Ånge; 78001 - 78007                        */
RUN cc_UI (INPUT "39004").
RUN cc_UI (INPUT "39005").
RUN cc_UI (INPUT "39006").
RUN cc_UI (INPUT "39007").
RUN cc_UI (INPUT "39008").
RUN cc_UI (INPUT "39009").
RUN cc_UI (INPUT "39010").
RUN cc_UI (INPUT "39011").
RUN cc_UI (INPUT "39012").
RUN cc_UI (INPUT "39013").
RUN cc_UI (INPUT "39035").
RUN cc_UI (INPUT "39037").
RUN cc_UI (INPUT "39061").
RUN cc_UI (INPUT "39062").
RUN cc_UI (INPUT "39063").
RUN cc_UI (INPUT "39046").
RUN cc_UI (INPUT "39047").             
RUN cc_UI (INPUT "39048").
RUN cc_UI (INPUT "39049").
RUN cc_UI (INPUT "39050").
RUN cc_UI (INPUT "39051").
RUN cc_UI (INPUT "39052").
RUN cc_UI (INPUT "39053").
RUN cc_UI (INPUT "39054").
RUN cc_UI (INPUT "39055").
RUN cc_UI (INPUT "39056").
RUN cc_UI (INPUT "39057").
RUN cc_UI (INPUT "39058").
RUN cc_UI (INPUT "39018").
RUN cc_UI (INPUT "39019").
RUN cc_UI (INPUT "39020").
RUN cc_UI (INPUT "39025").
RUN cc_UI (INPUT "39030").
RUN cc_UI (INPUT "39038").
RUN cc_UI (INPUT "39032").
RUN cc_UI (INPUT "39039").
RUN cc_UI (INPUT "39040").
RUN cc_UI (INPUT "39041").
RUN cc_UI (INPUT "39042").
RUN cc_UI (INPUT "39043").
RUN cc_UI (INPUT "39044").
RUN cc_UI (INPUT "39045").
RUN cc_UI (INPUT "39060").
RUN cc_UI (INPUT "39021").
RUN cc_UI (INPUT "39022").
RUN cc_UI (INPUT "39023").
RUN cc_UI (INPUT "39024").
RUN cc_UI (INPUT "78001").
RUN cc_UI (INPUT "78002").
RUN cc_UI (INPUT "78003").
RUN cc_UI (INPUT "78004").
RUN cc_UI (INPUT "78005").
RUN cc_UI (INPUT "78006").
RUN cc_UI (INPUT "78007").


PROCEDURE cc_UI.
DEFINE INPUT PARAMETER cc AS CHARACTER NO-UNDO.
   FOR EACH aonrtab WHERE aonrtab.AONR = cc:
      IF aonrtab.DELNR = 01 THEN aonrtab.ARBUPPG[1] = "Projektering/beredning".
      IF aonrtab.DELNR = 02 THEN aonrtab.ARBUPPG[1] = "Marknad"      .         
      IF aonrtab.DELNR = 03 THEN aonrtab.ARBUPPG[1] = "Arbete"       .         
      IF aonrtab.DELNR = 04 THEN aonrtab.ARBUPPG[1] = "Dokumentation".         
      IF aonrtab.DELNR = 05 THEN aonrtab.ARBUPPG[1] = "Material"      .        
      IF aonrtab.DELNR = 06 THEN aonrtab.ARBUPPG[1] = "Förbindelsehyra".       
   END.
END.
 
 
