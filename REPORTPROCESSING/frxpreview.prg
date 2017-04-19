*=======================================================
* Report Preview - main program
*
* In VFP9, a program or app may be assigned as the
* "Report Preview" application:
*
*    _REPORTPREVIEW = home()+"reportpreview.app"
*
* This program is the main program of the application
* that forms the default "preview factory" implementation.
*
* roRef is passed by reference. It must be assigned
* a reference to a Preview form before returning.
*
*=======================================================
#INCLUDE "INCLUDE\VFX.H"

LPARAMETERS roRef
#IF DEBUG_METHOD_LOGGING
	DEBUGOUT SPACE(PROGRAM(-1)) + "frxpreview.prg::(main)"
#ENDIF
*--------------------------------------------------------------
* Ensure some essential files are built in:
*--------------------------------------------------------------
EXTERNAL CLASS     frxPreview.vcx

*-----------------------------------------------------------
* We've been passed an object var to place the preview form
* reference into. Called from inside the REPORT FORM ... command.
* Return a reference to the preview container:
*-----------------------------------------------------------
IF SET("TALK") ="ON"
	SET TALK OFF
	roRef = NEWOBJECT("frxPreviewProxy","frxPreview.vcx")
	SET TALK ON
ELSE
	roRef = NEWOBJECT("frxPreviewProxy","frxPreview.vcx")
ENDIF
RETURN


*===========================================
* Class ErrorHandler
*
* A basic error handler.
*
* Useage:
* x = newobject("ErrorHandler","frxPreview.prg")
* x.Handle( iError, cMethod, iLine, THIS )
* if x.cancelled
*  :
* if x.suspened
*  :
*
*===========================================
DEFINE CLASS ErrorHandler AS CUSTOM

	suspended = .F.
	cancelled = .F.
	errorText = ""

*------------------------------------------
	PROCEDURE Handle
*------------------------------------------
	LPARAMETERS iError, cMethod, iLine, oRef

	STORE .F. TO THIS.cancelled, THIS.suspended

	LOCAL cErrorMsg, iRetval
	cErrorMsg = MESSAGE()

	cErrorMsg = m.cErrorMsg + CHR(13) + ;
		"Line " + TRANSFORM(m.iLine) + " in " + m.cMethod + "()"

	IF NOT EMPTY( MESSAGE(1) )
		cErrorMsg = m.cErrorMsg + ":" + CHR(13) + MESSAGE(1)
	ENDIF
*if not empty( sys(2018) )
*	cErrorMsg = m.cErrorMsg + chr(13) + sys(2018)
*endif
	IF PARAMETERS() > 3
		cErrorMsg = m.cErrorMsg + CHR(13) + oRef.NAME + ".Error()"
	ENDIF

*------------------------------------------------------
* Save the error message so that it can be retrieved
*------------------------------------------------------
	THIS.errorText = m.cErrorMsg

	IF DEBUG_SUSPEND_ON_ERROR
		cErrorMsg = m.cErrorMsg + CHR(13) + CHR(13) + "Do you want to suspend execution?"

		IF VARTYPE(goprogram) = "O"
			iRetval = goProgram.vfxmessagebox( m.cErrorMsg, 3 + 16 + 512, IIF(goProgram.lRuntimeLocalization, ;
				goLocalize.cDEFAULT_MBOX_TITLE_LOC, DEFAULT_MBOX_TITLE_LOC) + " Error" )
		ELSE
			iRetval = MESSAGEBOX( m.cErrorMsg, 3 + 16 + 512, DEFAULT_MBOX_TITLE_LOC + " Error" )
		ENDIF

		DO CASE
			CASE m.iRetVal = 6 && yes
				THIS.suspended = .T.

			CASE m.iRetVal = 2 && cancel
				THIS.cancelled = .T.

		ENDCASE
	ELSE
		IF VARTYPE(goprogram) = "O"
			 = goProgram.vfxmessagebox( m.cErrorMsg, 0 + 16, IIF(goProgram.lRuntimeLocalization, ;
				goLocalize.cDEFAULT_MBOX_TITLE_LOC, DEFAULT_MBOX_TITLE_LOC) + " Error" )
		ELSE
			 = MESSAGEBOX( m.cErrorMsg, 0 + 16, DEFAULT_MBOX_TITLE_LOC + " Error" )
		ENDIF
		THIS.cancelled = .T.
	ENDIF

	RETURN .F.
	ENDPROC

ENDDEFINE