/*elpfh-button-testing.p*/
DEFINE VARIABLE MyCase AS TesterTester.ButtonTooltipTester.
MyCase = NEW TesterTester.ButtonTooltipTester().
/*MyCase:button1:SetTooltip("MUFASA").*/
WAIT-FOR MyCase:ShowDialog().
DELETE OBJECT MyCase NO-ERROR.