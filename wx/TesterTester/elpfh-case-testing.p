/*elpfh-case-testing.p*/
DEFINE VARIABLE MyCase AS TesterTester.CaseForm.
MyCase = NEW TesterTester.CaseForm().
WAIT-FOR MyCase:ShowDialog().
DELETE OBJECT MyCase NO-ERROR.