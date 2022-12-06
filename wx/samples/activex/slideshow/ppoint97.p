/*  
 * This sample demonstrates the use of Microsoft Office 97 Powerpoint as an 
 * ActiveX Automation Server.  It shows a couple of slides in edit mode and 
 * then runs a slide show.
 */

DEFINE VAR objApplication AS COM-HANDLE.
DEFINE VAR objPresentation AS COM-HANDLE.
DEFINE VAR objSlide AS COM-HANDLE.
DEFINE VAR filename AS CHARACTER.
DEFINE VAR curDir AS CHARACTER.
DEFINE VAR k AS INTEGER.
DEFINE VAR l AS INTEGER.
DEFINE VAR m AS INTEGER.
DEFINE VAR n AS INTEGER.

m = 24. /* constant ppShapeSixteenPointStar for ShapeType object */ 
n = 12. /* constant ppPresetTextureWovenMat for PresetTexture object */

CREATE "PowerPoint.Application.8" objApplication.
objApplication:Visible = YES.
objApplication:WindowState = 1. /* Displays Normal Window */
    
/* The Add() takes optional argument NO, but use default which is YES */
objPresentation = objApplication:Presentations:Add(1).

IF objApplication:ActiveWindow:ViewType <> 7 THEN
    objApplication:ActiveWindow:ViewType = 7. /* constant 7 = ppViewSlideSorter */

/* Add five slides to objPresentation */
DO k = 1 TO 5:
    objPresentation:Slides:Add(k, 2).
END.

l = objPresentation:Slides:Count.
DO k = 1 TO l:
    objPresentation:Slides:Item(k):Shapes:Item(1):TextFrame:TextRange:Text = "Testing PowerPoint Shapes".
    objPresentation:Slides:Item(k):Shapes:Item(1):TextFrame:TextRange:Font:Bold = YES. 
    objPresentation:Slides:Item(k):Shapes:Item(2):TextFrame:TextRange:Text = "Shape #" + String(k).
    objPresentation:Slides:Item(k):Shapes:AddShape(m,178,203,323,324).
    objPresentation:Slides:Item(k):Shapes:Item(3):Fill:PresetTextured(n).
    objPresentation:Slides:Item(k):SlideShowTransition:AdvanceOnTime = YES.
    objPresentation:Slides:Item(k):SlideShowTransition:AdvanceTime = 2.
    m = m - 1.
    n = n - 1.
END.
objPresentation:SlideShowSettings:ShowType = 2.
objPresentation:SlideShowSettings:StartingSlide = 1.
objPresentation:SlideShowSettings:EndingSlide = 5.
objPresentation:SlideShowSettings:AdvanceMode = 2.
objPresentation:SlideShowSettings:LoopUntilStopped = YES.
objPresentation:SlideShowSettings:Run().


MESSAGE "Sit back and watch the show!" SKIP
        "To end the show, press OK"
        VIEW-AS ALERT-BOX.
 
RELEASE OBJECT objPresentation.
objApplication:Quit().
RELEASE OBJECT objApplication. 

