******************************************************************************
* Project...........: Visual Extend 16.0
*                     Copyright (c) dFPUG c/o ISYS GmbH
*                     All Rights Reserved.
*
* Program...........: VFXMAIN.PRG
* Author ...........: ISYS GmbH
* Created...........: October 2009
* Version...........: 16.00.0000
*
* Description.......:
* Calling Samples...:
* Parameter List....:
* Major change list.:
*
LPARAMETERS tcDatabaseMaintenance, tcPath, tcDBC

#include "INCLUDE\VFX.H"

IF OS(3) >= "6" AND VAL(SUBSTR(VERSION(4), 12, 4)) < 5016
	DECLARE INTEGER GdiSetBatchLimit IN WIN32API INTEGER
	GdiSetBatchLimit(1)
ENDIF


* Check for Database maintenance start
IF EMPTY(tcDatabaseMaintenance)
	_PECODE =""
ELSE
	tcDatabaseMaintenance = TRANSFORM(tcDatabaseMaintenance)
	IF 	("$REPAIR$" $ tcDatabaseMaintenance) OR ;
			("$PACK$" $ tcDatabaseMaintenance) OR ;
			("$PACKMEMO$" $ tcDatabaseMaintenance) OR ;
			("$REINDEX$" $ tcDatabaseMaintenance) OR ;
			("$TABLE$" $ tcDatabaseMaintenance) OR ;
			("$VALIDATE$" $ tcDatabaseMaintenance) OR ;
			("$VALIDATE:" $ tcDatabaseMaintenance) OR ;
			("$CHECKSTRU$" $ tcDatabaseMaintenance) OR ;
			("$CHECKSTRU:" $ tcDatabaseMaintenance)

		IF ("$TABLE$" $ tcDatabaseMaintenance)
			tcDatabaseMaintenance = STRTRAN(tcDatabaseMaintenance, "$TABLE$", "")
		ENDIF
		_PECODE = tcDatabaseMaintenance + CHR(13) + CHR(10) + tcPath + CHR(13) + CHR(10) + tcDBC
	ELSE
		_PECODE =""
	ENDIF
ENDIF

CLEAR ALL
CLEAR DLLS
CLEAR PROGRAM
CLEAR
*{V&U MS 2014-02-19 6303
IF TYPE("_vfp.ActiveProject.Name") == "C"
	_vfp.ActiveProject.Close()
ENDIF 
CLOSE DATABASES ALL
*}V&U MS 2014-02-19

* Wait for locks in CDX and FPT-files like done with DBF files
 = SYS(3052, 1, .T.)
 = SYS(3052, 2, .T.)

LOCAL lcSys16, lnPos
IF VERSION(2) = 2
	SET ASSERT ON
ELSE
*{ V&U UH 2008-09-21
* May cause error in case of unsufficient user rights.
*{ V&U RI 2008-12-10
	lcSys16 = SYS(16)
	lnPos = AT(":\", lcSys16)
	lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
	TRY
		CD (JUSTPATH(lcSys16))
	CATCH
	ENDTRY
*} V&U UH 2008-09-21
ENDIF


PUBLIC goenvironment, glclientsupport, glError1429Occurred

glclientsupport = .F.
glError1429Occurred = .F.
goenvironment = CREATEOBJECT("CEnvironment")

IF VARTYPE(goenvironment) # "O"

	IF VARTYPE(_VFP) ="O"
		_VFP.VISIBLE = .T.
	ENDIF
	_SCREEN.WINDOWSTATE = 2

	RETURN .F.
ENDIF

goenvironment.SET()
goenvironment.SETDataEnvironment()

IF !EMPTY(_PECODE)
&&Parameters tcarg, tcAction, tcPath, tcDBC
	DO FORM vfxdbfun WITH "", MLINE(_PECODE, 1), MLINE(_PECODE, 2), MLINE(_PECODE, 3) NOSHOW
	RETURN .F.
ENDIF

PUBLIC govfptoolbar

govfptoolbar = CREATEOBJECT("CVFPToolBar")

LOCAL lcoldcaption, lcoldicon, loException AS EXCEPTION

lcoldcaption = _SCREEN.CAPTION
lcoldicon    = _SCREEN.ICON

PUBLIC __vfx_runtime

__vfx_runtime = .F.

*{ V&U RI 2008-12-10
lcSys16 = SYS(16)
lnPos = AT(":\", lcSys16)
lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10

IF ".EXE" $ UPPER(SYS(16)) OR ".APP" $ UPPER(lcSys16)
	__vfx_runtime = .T.

	SET SYSMENU TO

	_SCREEN.ICON    = mainicon_loc
ENDIF

PUBLIC goprogram

goprogram = CREATEOBJECT("CApplication")


IF VARTYPE(goprogram) = "O"
	TRY
		lnFLLVersion = GetFLLVersion()
	CATCH TO loException
		lnFLLVersion = 1
	ENDTRY
	IF lnFLLVersion < 1200
		 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, ;
			goLocalize.cMSG_WRONGVFXFLLVERSIONEXECUTIONCANCELLED, ;
			MSG_WRONGVFXFLLVERSIONEXECUTIONCANCELLED), 0 + 16, _SCREEN.CAPTION)
		QUIT
	ENDIF

	IF TYPE("goProgram.nForegroundBuffMemSize") = "N" AND ;
			goProgram.nForegroundBuffMemSize >= 0
*{V&U MS 2009-05-11
* Limit to 2GB
		SYS(3050, 1, MIN(goProgram.nForegroundBuffMemSize, 2147483647))
*}V&U MS 2009-05-11
	ENDIF
	IF TYPE("goProgram.nBackgroundBuffMemSize") = "N" AND ;
			goProgram.nBackgroundBuffMemSize >= 0
*{V&U MS 2009-05-11
* Limit to 2GB
		SYS(3050, 2, MIN(goProgram.nBackgroundBuffMemSize, 2147483647))
*}V&U MS 2009-05-11
	ENDIF

	IF goprogram.lUseActivation
		PUBLIC goActivation
		goActivation = CREATEOBJECT("cVfxActivation")
	ENDIF

*{JEI MS 04.02.2008
	IF goProgram.lAllowUpdates
		goProgram.ApplicationUpdate()
	ENDIF
*}JEI MS 04.02.2008

	RELEASE gcoldtalk, gcolddir, gcoldpath, gcoldclasslib, gcoldescape, gcoldresource
	IF !(goprogram.lUseActivation = .T.) OR ;
			(goprogram.lUseActivation = .T. AND TYPE("goActivation") = "O")

		IF VARTYPE(_VFP) ="O"
			_VFP.VISIBLE = .T.
		ENDIF
		IF goprogram.onuserlogin()
*--prepare connection manager, if used, replace "conXY" with your connection name
*goProgram.oConnMgr.setConnectionName("conXY")
*--
			goprogram.START()
			goprogram.onuserlogout()
		ELSE
			RELEASE goUser
		ENDIF
	ENDIF
ENDIF

RELEASE govfptoolbar
RELEASE goprogram
RELEASE goenvironment

*{V&U MS 2014-02-19 6303
CLOSE DATABASES ALL
*}V&U MS 2014-02-19

_SCREEN.CAPTION = lcoldcaption
_SCREEN.ICON    = lcoldicon

CLEAR ALL
CLEAR DLLS
CLEAR PROGRAM
RELEASE ALL EXTENDED
SET MESSAGE TO

* If started in VFP.exe reopen the project file.
IF ADIR(laproj,"*.pjx") = 1
	_SHELL ="MODIFY PROJECT [" + laproj[1,1] + "]"
ENDIF

RETURN .T.

*****************************************************************************
**
DEFINE CLASS capplication AS cfoxappl

	cmaindatabase = database_loc
	cdatadir = datapath_loc
	cmainicon = mainicon_loc
	cintrobitmap = introform_loc
ENDDEFINE

***********************************************************
**  Class CEnvironment

DEFINE CLASS cEnvironment AS cAppEnvironment OF LIB\APPL.vcx

ENDDEFINE && CEnvironment
***********************************************************