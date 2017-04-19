******************************************************************************
* Project...........: Visual Extend 16.0
*                     Copyright (c) dFPUG c/o ISYS GmbH
*                     All Rights Reserved.
*
* Program...........: VFXFUNC.PRG
* Author ...........: ISYS GmbH
* Created...........: October 2009
* Version...........: 16.00.0000
*
* Description.......:
* Calling Samples...:
* Parameter List....:
* Major change list.:
*

#include "INCLUDE\VFX.H"

*-------------------------------------------------------
* Function....: OnError
* Called by...: cFoxApp.Start
*
* Abstract....: ON ERROR Function Handler
*
* Returns.....:
*
* Parameters..: nError, cMethod, nLine, cMessage
*
* Notes.......: Global error handler.
*-------------------------------------------------------
FUNCTION onerror(nerror, cmethod, nline, cmessage, cmessage1)
LOCAL lnSelect, latables[1,1], j, k, lctext, lctables, lnoldreprocess, ;
	lcmessage, laerror[7], lcCallStack, llReadOnlyTableError

*{V&U MP 2014-04-22 6493, MS 2014-06-17 6573 Commented
*!*	LOCAL loActiveForm
*!*	loActiveForm = goProgram.GetActiveForm()
*}V&U MP 2014-04-22
IF TYPE("goprogram.class") <>"C"
	RETURN
ENDIF

* Record / File is in use by another ....
IF (nerror = 108) OR (nerror = 109)
	LOCAL lnsecsincemidnight
	lnsecsincemidnight = VAL(SYS(2))

	IF (goprogram.nlastinuseerror = 0) OR (lnsecsincemidnight - goprogram.nlastinuseerror > 20)
* first time the error occured
		goprogram.nlastinuseerror = lnsecsincemidnight
	ELSE
* test for still in 15 seconds timeout period otherwise do not perform a retry
		IF (lnsecsincemidnight - goprogram.nlastinuseerror) > 15
* clear InUse-Error
			goprogram.nlastinuseerror = 0
		ENDIF
	ENDIF

	IF goprogram.nlastinuseerror > 0
* wait and try again
		INKEY(0.03)
		RETRY
	ENDIF
ENDIF

* clear InUse-Error
goprogram.nlastinuseerror = 0

IF EMPTY(cmessage)
	lcmessage = MESSAGE()
ELSE
	lcmessage = cmessage
ENDIF

laerror[1]  = .F.

 = AERROR(laerror)

*{ V&U MS 2014-05-14 6549, Move back to Top
*{ V&U MS 2012-08-30 5894, Use Critical error behavior only if goprogram.nAppOnErrorBehavior = 0
*{ V&U MS 2012-10-18 5959, Check goprogram.nAppOnErrorBehavior in ErrorHandler
IF !(VERSION(2) = 2) AND nError <> 1466 
	IF !EMPTY(laError[1])
		*{V&U MS 2014-06-17 6573
*!*			IF TYPE("loActiveForm") = "O" AND PEMSTATUS(loActiveForm, 'ErrorHandler', 5)
*!*				IF loActiveForm.ErrorHandler(laError[1])
*!*					RETURN .F.
*!*				ENDIF
*!*			ENDIF
		IF TYPE("_Screen.ActiveForm.Class") == "C"
			IF PEMSTATUS(_Screen.ActiveForm, 'ErrorHandler', 5)
				IF _Screen.ActiveForm.ErrorHandler(laError[1])
					RETURN .F.
				ENDIF
			ENDIF
		ENDIF		
		*}V&U MS 2014-06-17
	ENDIF
ENDIF
*} V&U MS 2012-10-18
*} V&U MS 2012-08-30
*} V&U MS 2014-05-14

lnSelect = SELECT() && JEI RI 2008.06.04 changed from ALIAS() to SELECT()

IF EMPTY(nline)
	nline = -1
ENDIF

*{V&U MS 2013-03-19 6142
lcSys16 = SYS(16, 0)
lnPos = AT(":\", lcSys16)
lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
lcCurrentVersion = ''
IF AGETFILEVERSION(laVersion, lcSys16) > 0
	IF !EMPTY(laVersion[4])
		lcCurrentVersion = UPPER(ALLTRIM(laVersion[4]))
	ENDIF 
ENDIF 

lcText   = 	IIF(!EMPTY(lcCurrentVersion), IIF(goProgram.lRuntimeLocalization, ;
			IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cCAP_LBLAPPLICATIONVERSION, "Application version:"), ;
			CAP_LBLAPPLICATIONVERSION) + " " + lcCurrentVersion + CHR(13), '') + ;
	 		UPPER(cmethod) + CHR(13) + ;
			"line : " + TRANSFORM(nline)
*}V&U MS 2013-03-19
lcCode = "code : " + cmessage1

lctables = ""

IF goprogram.nerrordetaillevel > 0
	lcCallStack = "Call Stack:" + CHR(13)
	j = 1
	DO WHILE !EMPTY(PROGRAM(j))
		lcCallStack = lcCallStack + PROGRAM(j) + CHR(13)
		j = j + 1
	ENDDO
ENDIF
IF AUSED(latables) > 0
	lctables = "*" + ALIAS() + " [" + TAG() + "]" + " " + TRANSFORM(RECNO())

	FOR j = 1 TO ALEN(latables, 1)
		IF latables[j,1] # ALIAS()
			lctables = lctables + CHR(13) + latables[j,1] +"(" + TRANSFORM(RECNO(latables[j,1])) +")"
		ENDIF
	NEXT
ENDIF

?? CHR(7)

_SCREEN.LOCKSCREEN             = .F.

*{V&U MS 2014-06-17 6573 
*!*	IF TYPE("loActiveForm") = "O" AND TYPE("loActiveForm.lockscreen") # "U"
*!*		loActiveForm.LOCKSCREEN  = .F.
*!*	ENDIF
IF TYPE("_screen.ActiveForm.Class") == "C" AND TYPE("_screen.ActiveForm.LockScreen") # "U"
	_screen.ActiveForm.LockScreen = .F.
ENDIF
*}V&U MS 2014-06-17

LOCAL lnanswer, llUseIDE
*{ V&U MS 2011-11-10 5526
* Do not show message box. Act always as Ignore
lnAnswer = 5
*} V&U MS 2011-11-10

* Show error messagebox
*{JEI MS 15.08.2006
IF VERSION(2) = 2
	llUseIDE = .T.
	lcMessageText = IIF(goProgram.lRuntimeLocalization, ;
		IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_ERRORNUM,"Error:"), ;
		MSG_ERRORNUM) + ;
		TRANSFORM(nerror) + id_cr + ;
		IIF(goProgram.lRuntimeLocalization, ;
		IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_METHOD, "Method:"), ;
		MSG_METHOD) + cmethod + " : " + ;
		TRANSFORM(nline) + id_cr + ;
		lcmessage + id_cr + "'" + cmessage1 +"'" + ;
		id_cr + IIF(goProgram.lRuntimeLocalization, ;
		IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_DOYOUWANTTODEBUG, "Do you want to debug?"), ;
		MSG_DOYOUWANTTODEBUG)

	lnAnswer = goProgram.vfxmessagebox(lcMessageText, 3 + 16, IIF(goProgram.lRuntimeLocalization, ;
		IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_PROGRAM_ERROR, "Program Error"), ;
		MSG_PROGRAM_ERROR))
	DO CASE
		CASE lnAnswer = 2
			lnAnswer = 3 && Abort
		CASE lnAnswer = 7
			lnAnswer = 5 && Ignore
	ENDCASE
*}JEI MS 15.08.2006
ELSE
	IF goprogram.nAppOnErrorBehavior = 0
		lnanswer = 5 && Ignore
		lcMessagetext = ""
	ELSE
		IF goprogram.nAppOnErrorBehavior = 1
			lcMessageText = IIF(goProgram.lRuntimeLocalization, ;
				IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_ERRORNUM,"Error:"), ;
				MSG_ERRORNUM) + ;
				TRANSFORM(nerror) + id_cr + ;
				IIF(goProgram.lRuntimeLocalization, ;
				IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_METHOD, "Method:"), ;
				MSG_METHOD) + cmethod + " : " + ;
				TRANSFORM(nline) + id_cr + ;
				lcmessage + id_cr + "'" + cmessage1 +"'"
		ELSE
			lcMessageText = IIF(goProgram.lRuntimeLocalization, ;
				IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_FATAL_ERROR, "Fatal Error"), ;
				MSG_FATAL_ERROR)
		ENDIF
		*{ V&U MS 2011-11-10 5526
		*{ V&U MS 2012-08-30 5894, if nAppOnErrorBehavior <> 0, Show messagebox
		lnAnswer = goProgram.vfxmessagebox(lcMessageText, 2 + 16, IIF(goProgram.lRuntimeLocalization, ;
			IIF(TYPE("goLocalize") = "O" AND !ISNULL(goLocalize), goLocalize.cMSG_PROGRAM_ERROR, "Program Error"), ;
			MSG_PROGRAM_ERROR))
		*} V&U MS 2012-08-30
		*} V&U MS 2011-11-10
	ENDIF
ENDIF
IF nerror = 1466 && Connection handle is invalid. Stop program execution
	lnanswer =  3	&& Abort
ENDIF

* Log error
IF lnanswer # 4
	LOCAL lcloseit

	lnoldreprocess = SET('REPROCESS')

	SET REPROCESS TO AUTOMATIC

*{ V&U MS 2008-10-14
* VFX tables are on SQL along with Data tables. To prevent commit current transaction
	IF goProgram.nVfxSysTableLoc >= 1 AND goPath.VfxSystemTableLocation = 1
		llOpenLogError = .F.
		TRY
			lnLogConn = SQLSTRINGCONNECT(goPath.ConnectionString)
			SQLEXEC(lnLogConn, "Select * from vfxLog where Error < 0", "vfxlog")
		CATCH
* Do nothing
			llOpenLogError = .T.
		ENDTRY
		IF llOpenLogError
			RETURN .T.
		ENDIF
	ELSE
*} V&U MS 2008-10-14
		vOpenVfxLog = OpenVfxLog(@lcloseit)
		IF TYPE("vOpenVfxLog") <>"O" AND !vOpenVfxLog
			RETURN .T.
		ENDIF
	ENDIF

	SELECT vfxlog
	llReadOnlyTableError = .F.
	TRY
		INSERT INTO vfxlog (TYPE, DATE, TIME) ;
			VALUES ("ERROR", DATE(), TIME())

		IF TYPE("GoUser.user") ="C"
			REPLACE USER WITH GoUser.USER
		ENDIF

		REPLACE ERROR WITH nerror, ;
			MESSAGE WITH lcmessage, ;
			method  WITH IIF(goprogram.nerrordetaillevel = 0, "", lctext + CHR(13) + lcCallStack), ;
			TABLES  WITH IIF(goprogram.nerrordetaillevel = 0, "", lctables), ;
			CODE	WITH lcCode

		IF goProgram.nErrorDetailLevel = 2
			lctempfile = SYS(2023) + UPPER("\X" + SUBSTR(SYS(2015), 4, 7) +".TXT")

			REPLACE MEMORY WITH MEMORY + CHR(13) + "MEMORY CONFIGURATION:" + CHR(13)

			LIST MEMO TO FILE(lctempfile) NOCONSOLE

			APPEND MEMO MEMORY FROM (lctempfile)

			ERASE (lctempfile)

			REPLACE MEMORY WITH MEMORY + CHR(13) + "OBJECTS:" + CHR(13)

			LIST OBJECTS TO FILE(lctempfile) NOCONSOLE

			APPEND MEMO MEMORY FROM (lctempfile)

			ERASE (lctempfile)

			REPLACE STATUS WITH STATUS + CHR(13) + "STATUS:" + CHR(13)

			LIST STATUS TO FILE(lctempfile) NOCONSOLE

			APPEND MEMO STATUS FROM (lctempfile)

			ERASE (lctempfile)

			IF lCloseit
				USE IN vfxLog
			ENDIF
		ENDIF
	CATCH
		llReadOnlyTableError = .T.
	ENDTRY

	IF llReadOnlyTableError
		RETURN .F.
	ENDIF

	IF USED("vfxLog")
		SELECT vfxLog
		IF CURSORGETPROP("Buffering") > 1
*{ V&U MS 2008-10-14
* VFX tables are on SQL along with Data tables. To prevent commit current transaction
			IF goProgram.nVfxSysTableLoc >= 1 AND goPath.VfxSystemTableLocation = 1
				lcInsertCom = "INSERT INTO vfxLog([TYPE], [DATE], [TIME], [USER], [ERROR], " + ;
					"[MESSAGE], [METHOD], [TABLES], [CODE], [MEMORY], [STATUS])" + ;
					"VALUES ('ERROR', GETDATE(), convert(Char, getdate(), 108), " + ;
					"?vfxLog.User, ?vfxLog.Error, ?vfxLog.MESSAGE, ?vfxLog.METHOD, " + ;
					"?vfxLog.TABLES, ?vfxLog.CODE, ?vfxLog.MEMORY, ?vfxLog.STATUS)"
				TRY
					SQLEXEC(lnLogConn, lcInsertCom)
					SQLDISCONNECT(lnLogConn)
				CATCH
*Do Nothing
				ENDTRY
			ELSE
*} V&U MS 2008-10-14
				TABLEUPDATE()
			ENDIF
		ENDIF
		*{V&U MS 2011-11-10 5526
		IF lnAnswer <> 3 AND lnAnswer <> 5
			USE IN vfxLog
		ENDIF 
		*}V&U MS 2011-11-10 
	ENDIF

	SET REPROCESS TO lnoldreprocess

	IF !EMPTY(lnSelect)
		SELECT (lnSelect)
	ENDIF
ENDIF

DO CASE
	CASE lnanswer = 3	&& Abort

** Send error report
		LOCAL lcString, nrecno, lcloseit
		nrecno = 0
		IF TYPE("goprogram.nAppOnErrorBehavior") = "N"
			goprogram.nAppOnErrorBehavior = 0
		ENDIF

		lctempfile = ADDBS(SYS(2023)) +"X" + SUBSTR(SYS(2015), 4, 7) +".TXT"
		*{ V&U MS 2011-11-10 5526
		IF !USED("vfxLog")
			vOpenVfxLog = OpenVfxLog(@lcloseit)
			IF TYPE("vOpenVfxLog") <>"O" AND !vOpenVfxLog
				RETURN .T.
			ENDIF
		ENDIF 
		*} V&U MS 2011-11-10 
		SELECT vfxlog
		GO BOTTOM IN vfxlog
		nrecno = RECNO()
		cFieldList = "message,method,code,status"
		LIST FIELDS MESSAGE ALL FOR RECNO() = nrecno  NOCONSOLE TO FILE(lctempfile) ADDITIVE
		LIST FIELDS method ALL FOR RECNO() = nrecno NOCONSOLE TO FILE(lctempfile) ADDITIVE
		LIST FIELDS CODE ALL FOR RECNO() = nrecno NOCONSOLE TO FILE(lctempfile) ADDITIVE
		LIST FIELDS STATUS ALL FOR RECNO() = nrecno NOCONSOLE TO FILE(lctempfile) ADDITIVE

		lcString = lcMessagetext +";" + lctempfile

		*{ V&U MS 2011-11-10 5526
		IF !llUseIDE						&& Not in IDE
			PUBLIC gcUserMessageText
			gcUserMessageText = ""
			goProgram.RunForm("vfxErrorReport", lcString)
			IF !EMPTY(gcUserMessageText)
				SELECT vfxLog
				IF nRecno <= RECCOUNT()
					GO (nRecno)
				ENDIF
				REPLACE MESSAGE WITH MESSAGE + CHR(13) + CHR(10) + ;
					IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_FROMUSER, CAP_FROMUSER) + " " + ;
					ALLTRIM(gcUserMessageText)
			ENDIF
		ELSE
			goProgram.lAbortAfterCriticalError = .T.
		ENDIF
		USE IN vfxlog
		*} V&U MS 2011-11-10

		ERASE (lctempfile)
		*{ V&U MS 2011-11-10 5526
		RELEASE gcUserMessageText
		IF goProgram.lAbortAfterCriticalError
		*} V&U MS 2011-11-10
			DO WHILE TXNLEVEL() > 0
				ROLLBACK
			ENDDO

			IF AUSED(latables) > 0
				FOR j = 1 TO ALEN(latables, 1)
					IF INLIST(CURSORGETPROP('Buffering', latables[j,1]), 3, 5)
						 = TABLEREVERT(.T., latables[j,1])
					ENDIF
				NEXT
			ENDIF

			ON SHUTDOWN QUIT

			CLEAR EVENTS

			IF nerror = 1466 && Connection handle is invalid.
				goprogram.nAppOnErrorBehavior = 0		&& Avoid error messages
				LOCAL lnDataSessionCnt, lnOldDataSession, lnDataSessionNum, laSessions[1]
				lnDataSessionCnt = ASESSIONS(laSessions)	&& Disables display of the Open Dialog Box
				lnOldDataSession = SET("Datasession")
				FOR lnDataSessionNum = 1 TO lnDataSessionCnt
					SET DATASESSION TO laSessions[lnDataSessionNum]
					SET TABLEPROMPT OFF
				ENDFOR
				SET DATASESSION TO lnOldDataSession
				RELEASE goUser							&& Avoid execution of goProgram.onUserLogout
			ENDIF

			IF VERSION(2)#2
				QUIT
			ELSE
				IF SYS(2410) = "1" && Try Catch
					IF TYPE("goProgram.class") ="C"
						goProgram.onQuit()
					ELSE
						QUIT
					ENDIF
				ELSE
					RETURN TO MASTER
				ENDIF
			ENDIF
		ENDIF 

	CASE lnanswer = 4	&& Retry
		RETRY

	CASE lnanswer = 5	&& Ignore
		*{ V&U MS 2011-11-10 5526
		* In this case also call ErrorDialog, but not Quit the Application
		IF goProgram.lIsCriticalError = .T. AND !llUseIDE 
			** Send error report
			LOCAL lcString, nRecno, lCloseIt
			nRecno = 0
			IF TYPE("goprogram.nAppOnErrorBehavior") = "N"
				goProgram.nAppOnErrorBehavior = 0
			ENDIF

			lcTempFile = ADDBS(SYS(2023)) + "X" + SUBSTR(SYS(2015), 4, 7) + ".TXT"
			IF !USED("vfxLog")
				vOpenVfxLog = OpenVfxLog(@lCloseIt)
				IF TYPE("vOpenVfxLog") <> "O" AND !vOpenVfxLog
					RETURN .T.
				ENDIF
			ENDIF
			SELECT vfxLog
			GO BOTTOM IN vfxlog
			nRecno = RECNO()
			cFieldList = "message,method,code,status"
			LIST FIELDS MESSAGE ALL FOR RECNO() = nRecno NOCONSOLE TO FILE(lcTempFile) ADDITIVE
			LIST FIELDS method ALL FOR RECNO() = nRecno NOCONSOLE TO FILE(lcTempFile) ADDITIVE
			LIST FIELDS CODE ALL FOR RECNO() = nRecno NOCONSOLE TO FILE(lcTempFile) ADDITIVE
			LIST FIELDS STATUS ALL FOR RECNO() = nRecno NOCONSOLE TO FILE(lcTempFile) ADDITIVE

			lcString = lcMessageText + ";" + lcTempFile

			PUBLIC gcUserMessageText
			gcUserMessageText = ""
			goProgram.RunForm("vfxErrorReport", lcString)
			IF !EMPTY(gcUserMessageText)
				SELECT vfxLog
				IF nRecno <= RECCOUNT()
					GO (nRecno)
				ENDIF
				REPLACE MESSAGE WITH MESSAGE + CHR(13) + CHR(10) + "From user: " + ;
					ALLTRIM(gcUserMessageText)
			ENDIF
			ERASE (lcTempFile)
			RELEASE gcUserMessageText
			USE IN vfxLog
		ENDIF
		*} V&U MS 2011-11-10
		RETURN .T.

	CASE lnAnswer = 6	&& IDE, YES
		DEBUG
		SUSPEND

ENDCASE

RETURN .F.
ENDFUNC

*-------------------------------------------------------
*Function....: OpenVfxLog
*Called by...: OnError
*Returns.....: USED("vfxLog")
*-------------------------------------------------------
FUNCTION OpenVfxLog
LPARAMETERS lcloseit
LOCAL lRes
IF goProgram.nVfxSysTableLoc >= 1
	IF USED("VFXLOG")
		USE IN VFXLOG
	ENDIF
	lRes = goProgram.CheckAndCreateSQLTable("VFXLOG")
	IF lRes
		*{V&U MS 2010-05-13, 2014-04-28
		caVfxLog = OPENTABLE("select * from VfxLog where 1 = 2", "IDVFXLOG", .F., "", ;
			.T., ,"vfxLog","cAppVFXDataAccess", .F., .F., .F., .F., "DATE,METHOD,TABLES,TIME,TYPE,USER")
		*}V&U MS 2010-05-13, 2014-04-28
	ENDIF
ENDIF

IF !USED("VFXLOG")
	IF !EMPTY(goprogram.cvfxdir)
		lCloseIt = .T.
		IF ADIR(ladummy, ADDBS(goprogram.cvfxdir) + "vfxlog.dbf")#1
			goProgram.CreateTable(ADDBS(goprogram.cvfxdir) +"vfxlog")
		ENDIF
		USE (ADDBS(goProgram.cvfxdir) + "vfxlog") IN 0 ORDER TAG OBJECT SHARED AGAIN
	ELSE
		LOCAL loApplicationFileLoc, lcDirName
		loApplicationFileLoc = CREATEOBJECT("cApplicationFileLoc")
		lcDirName = loApplicationFileLoc.GetFileLocation("vfxlog.dbf", .T.)
		IF EMPTY(lcDirName)
			lcDirName = loApplicationFileLoc.GetFileCreationFolder(0)
			lcDirName = ADDBS(lcDirName) +"vfxlog"
			goProgram.CreateTable(lcDirName)
		ENDIF
		USE (lcDirName) IN 0 ORDER TAG OBJECT SHARED AGAIN
	ENDIF
ENDIF

IF TYPE("caVfxLog") ="O" AND !ISNULL(caVfxLog)
	RETURN caVfxLog
ELSE
	RETURN USED("VfxLog")
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: OnShutDown
* Called by...: cFoxapp.Start
*
* Abstract....: ON SHUTDOWN Function Handler
*
* Returns.....:
*
* Parameters..:
*
* Notes.......: Shut down the application
*-------------------------------------------------------
FUNCTION onshutdown()
goprogram.onquit()
ENDFUNC

*-------------------------------------------------------
* Function....: GetNewID
* Called by...:
*
* Abstract....: Generate a CHAR 10 unique key
*
* Returns.....: cUniqueKey
*
* Parameters..: tcAlias,tnLen,tcStart
*
* Notes.......: USE DATA\VFXSYSID ORDER TAG KEY
*-------------------------------------------------------
FUNCTION getnewid(tcalias, tnlen, tcstart)
LOCAL lcalias, lcid, lnoldreprocess, lnoldarea, lcexact, llFound

lnoldarea = SELECT()

*!* Set exact on to find the right record.
lcexact = SET("EXACT")
SET EXACT ON

IF EMPTY(tcalias)
	lcalias = ALIAS()
	IF CURSORGETPROP("SOURCETYPE") = 1    && LOCAL VIEW
		lcalias = UPPER(CURSORGETPROP("TABLES"))
		lcalias = SUBSTR(lcalias, AT("!", lcalias) + 1)
	ENDIF
ELSE
	lcalias = UPPER(tcalias)
ENDIF

lnoldreprocess = SET('REPROCESS')

SET REPROCESS TO AUTOMATIC

*!* This function should work also if the application object does not exist.
IF !USED("vfxsysid")
	IF TYPE("goprogram.class") ="C"
		IF goProgram.nvfxSysTableLoc >= 1
			goProgram.CheckAndCreateSQLTable("vfxsysid")
			*{V&U MS 2010-05-13, 2014-04-28
			oSysIdAdptr = OPENTABLE("vfxsysid", "IDVFXSYSID", .F., "INDEX ON UPPER(keyname) TAG KEY ADDITIVE", ;
									.T., ,"vfxsysid", "cAppVFXDataAccess", .F., .F., .F., .F., "VALUE")
			*}V&U MS 2010-05-13, 2014-04-28
			SET ORDER TO TAG KEY
		ELSE
			IF ADIR(ladummy, goprogram.cvfxdir +"vfxsysid.dbf")#1
* Table not found. Create new table.
				goprogram.createtable(goprogram.cvfxdir +"vfxsysid")
			ENDIF
			USE (goprogram.cvfxdir +"vfxsysid") IN 0 ORDER TAG KEY
		ENDIF
	ELSE
		IF FILE("vfxsysid.dbf")
			USE vfxsysid IN 0 ORDER TAG KEY
		ELSE
			IF FILE("data\vfxsysid.dbf")
				USE ("data\vfxsysid.dbf") IN 0 ORDER TAG KEY
			ENDIF
		ENDIF
	ENDIF
ENDIF

SELECT vfxsysid

* verify Startvalue
LOCAL lnstart

DO CASE
	CASE VARTYPE(tcstart) ="C"
		lnstart = VAL(tcstart)
	CASE VARTYPE(tcstart) ="N"
* do nothing
		lnstart = tcstart
	OTHERWISE
		lnstart = 1
ENDCASE

*{V&U MS 2013-01-25
llFound = SEEK(lcalias, "vfxsysid", "KEY")
IF !llFound
	GO TOP IN vfxsysid
ENDIF 	
IF EMPTY(tnlen)
	IF !EMPTY(vfxsysid.MAXLEN)
		tnlen = vfxsysid.MAXLEN
	ELSE
		tnlen = FSIZE('VALUE')
	ENDIF
ELSE 
	IF tnlen > FSIZE('VALUE')
		tnlen = FSIZE('VALUE')
	ENDIF	
ENDIF 	

IF llFound 
*}V&U MS 2013-01-25
	IF RLOCK()
* use current value in vfxsysid
		lcid = VAL(vfxsysid.VALUE)

** if the actual value in vfxsysid is smaller than the start value, take care of that!
		IF lcid < lnstart
			lcid = lnstart - 1
		ENDIF

		lcid = incbase10(IIF(lcid = 0,"", STR(lcid)), tnlen)

		REPLACE VALUE WITH lcid IN vfxsysid
	ENDIF

	UNLOCK
ELSE
* use start value minus 1 as it will be incremented by incbase10()
	lcid = lnstart - 1
	lcid = incbase10(IIF(lcid = 0,"", STR(lcid)), tnlen)

	INSERT INTO vfxsysid (keyname, VALUE, MAXLEN) ;
		VALUES(UPPER(lcalias), lcid, tnlen)
ENDIF

IF CURSORGETPROP("Buffering") > 1
	TABLEUPDATE()
ENDIF
USE IN vfxsysid
RELEASE oSysIdAdptr

SET REPROCESS TO lnoldreprocess

IF !EMPTY(lnoldarea)
	SELECT (lnoldarea)
ENDIF

*!* Restore the old setting for exact.
SET EXACT &lcexact

RETURN lcid
ENDFUNC

*-------------------------------------------------------
* Function....: XLock
* Called by...:
*
* Abstract....: Logical File/Record Locking
*
* Returns.....: .T. If record/table is locked
*
* Parameters..: tcAlias, tnRecord
*
* Notes.......: USE DATA\VFXLOCK ORDER TAG KEY
*               IF tnRecord is empty lock table
*-------------------------------------------------------
FUNCTION xlock(tcalias, tnrecord)
LOCAL lnSelect, lcloseit, lnoldreprocess

PUBLIC _goxlockuser, _goxlockdate, _goxlocktime

IF TYPE("goprogram.class") <>"C"
	RETURN .T.
ENDIF

_goxlockuser = ""
_goxlockdate = ""
_goxlocktime = ""

lnSelect  = SELECT() && JEI RI 2008.06.04 changed from ALIAS() to SELECT()

lnoldreprocess = SET('REPROCESS')

SET REPROCESS TO AUTOMATIC

lcloseit = .F.

IF EMPTY(tcalias)
	tcalias = ALIAS()
ENDIF

SELECT (tcalias)

IF EMPTY(tnrecord)			&& Lock Table!
	tnrecord = 0
ENDIF

IF !USED("VFXLOCK")
	IF goProgram.nvfxSysTableLoc >= 1
		goProgram.CheckAndCreateSQLTable("vfxlock")
		*{V&U MS 2014-04-28
		oLockAdptr = OPENTABLE("vfxlock", "IDVFXLOCK", .F., ;
			"INDEX ON UPPER(table)+STR(record,10) TAG KEY ADDITIVE", .T., ,"vfxlock", "cAppVFXDataAccess", ;
			.F., .F., .F., .F., "DATE, TABLE, TIME")
		*}V&U MS 2014-04-28	
		SET ORDER TO TAG KEY
	ELSE
		IF ADIR(ladummy, goprogram.cvfxdir +"vfxlock.dbf")#1
* Table not found. Create new table.
			goprogram.createtable(goprogram.cvfxdir +"vfxlock")
		ENDIF
		USE (goprogram.cvfxdir +"vfxlock") ORDER TAG KEY IN 0 AGAIN
	ENDIF
	lcloseit = .T.
ENDIF

SELECT vfxlock

IF SEEK(PADR(UPPER(tcalias), 32) + STR(tnrecord, 10), "VFXLOCK", "KEY")
	lok = .F.

	_goxlockuser = vfxlock.user_name
	_goxlockdate = DTOC(vfxlock.DATE)
	_goxlocktime = vfxlock.TIME

ELSE
	lok = .T.
	INSERT INTO vfxlock (table, record, date, time, user_name) VALUES(tcalias, tnrecord, DATE(), TIME(), GoUser.user_name)
ENDIF

IF lcloseit
	IF CURSORGETPROP("Buffering") > 1
		TABLEUPDATE()
	ENDIF
	USE IN vfxlock
	RELEASE oLockAdptr
ENDIF

SET REPROCESS TO lnoldreprocess

IF!EMPTY(lnSelect)
	SELECT (lnSelect)
ENDIF

RETURN lok
ENDFUNC

*-------------------------------------------------------
* Function....: XUnLock
* Called by...:
*
* Abstract....: Logical File/Record Unlocking
*
* Returns.....: .T. If record/table is unlocked
*
* Parameters..: tcAlias, tnRecord, tlAllLock
*
* Notes.......: USE DATA\VFXLOCK ORDER TAG KEY
*-------------------------------------------------------
FUNCTION xunlock(tcalias, tnrecord, tlalllock)
LOCAL lnSelect, lcloseit, lnoldreprocess

PUBLIC _goxlockuser, _goxlockdate, _goxlocktime

IF TYPE("goprogram.class") <>"C"
	RETURN .T.
ENDIF

_goxlockuser = ""
_goxlockdate = ""
_goxlocktime = ""

lnSelect  = SELECT() && JEI RI 2008.06.04 changed from ALIAS() to SELECT()

lnoldreprocess = SET('REPROCESS')

SET REPROCESS TO AUTOMATIC

lcloseit = .F.

IF EMPTY(tcalias)
	tcalias = ALIAS()
ENDIF

SELECT (tcalias)

IF EMPTY(tnrecord)			&& Lock Table!
	tnrecord = 0
ENDIF

IF !USED("VFXLOCK")
	IF goProgram.nvfxSysTableLoc >= 1
		goProgram.CheckAndCreateSQLTable("vfxlock")
		*{V&U MS 2014-04-28
		oLockAdptr =  OPENTABLE("vfxlock", "IDVFXLOCK", .F., ;
			"INDEX ON UPPER(table)+STR(record,10) TAG KEY ADDITIVE", .T., ,"vfxlock", "cAppVFXDataAccess", ;
			.F., .F., .F., .F., "DATE, TABLE, TIME")
		*}V&U MS 2014-04-28
		SET ORDER TO TAG KEY
	ELSE
		IF ADIR(ladummy, goprogram.cvfxdir +"vfxlock.dbf")#1
* Table not found. Create new table.
			THIS.createtable(goprogram.cvfxdir +"vfxlock")
		ENDIF
		USE (goprogram.cvfxdir +"vfxlock") ORDER TAG KEY IN 0 AGAIN
	ENDIF
	lcloseit = .T.
ENDIF

SELECT vfxlock

IF SEEK(PADR(UPPER(tcalias), 32) + STR(tnrecord, 10), "VFXLOCK", "KEY")
	IF FLOCK()
		IF tlalllock
			DELETE ALL FOR ALLTRIM(UPPER(TABLE)) == ALLTRIM(UPPER(tcalias))
		ELSE
			DELETE
		ENDIF
	ENDIF

	UNLOCK IN vfxlock

	lok = .T.
ELSE
	lok = .F.
ENDIF

IF lcloseit
	IF CURSORGETPROP("Buffering") > 1
		TABLEUPDATE()
	ENDIF
	USE IN vfxlock
	RELEASE oLockAdptr
ENDIF

SET REPROCESS TO lnoldreprocess

IF!EMPTY(lnSelect)
	SELECT (lnSelect)
ENDIF
RETURN lok
ENDFUNC

*-------------------------------------------------------
* Function....: XCrypt
* Called by...:
*
* Abstract....: Generate a crypted string
*
* Returns.....: cCryptedText
*
* Parameters..: tcText
*
* Notes.......: USE a XOR bitwise
*-------------------------------------------------------
FUNCTION xcrypt(tctext)
LOCAL lcbuffer, lnvalue, lnkey

lnkey    = 9
lcbuffer = ""

FOR j = 1 TO LEN(tctext)
	lnvalue  = BITXOR(ASC(SUBSTR(tctext, j, 1)), lnkey)
	lcbuffer = lcbuffer + CHR(lnvalue)
NEXT

RETURN lcbuffer
ENDFUNC

*-------------------------------------------------------
* Function....: ErrorMsg
* Called by...: vfx_doupdate()
*
* Abstract....: Display a Error MessageBox
*
* Returns.....: lnAnswer
*
* Parameters..: tcMessageText, tnDialogType, tcMessageTitle
*
* Notes.......: USE a XOR bitwise
*-------------------------------------------------------
FUNCTION errormsg(tcmessagetext, tndialogtype, tcmessagetitle)
LOCAL lnanswer

IF EMPTY(tcmessagetext)
	tcmessagetext = "Syntax Error: ErrorMsg(tcMessageText, tnDialogType, tcMessageTitle)"
ENDIF

IF EMPTY(tndialogtype)
	tndialogtype = MB_OK + mb_iconstop
ENDIF

IF EMPTY(tcmessagetitle) AND TYPE("goProgram.class") ="C"
	tcmessagetitle = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ATTENTION, MSG_ATTENTION)
ENDIF

IF TYPE("goProgram.class") ="C"
	lnanswer = goProgram.vfxmessagebox(tcmessagetext, tndialogtype, tcmessagetitle)
ELSE
	lnanswer = MESSAGEBOX(tcmessagetext, tndialogtype, tcmessagetitle)
ENDIF
RETURN lnanswer
ENDFUNC

*-------------------------------------------------------
* Function....: SetFirstFocus
* Called by...: CDataForm::OnInsert(), CDataForm::OnEdit()
*
* Abstract....: Set focus to control with lesser TabIndex
*
* Returns.....:
*
* Parameters..: oContainer
*
* Notes.......:
*-------------------------------------------------------
FUNCTION setfirstfocus(ocontainer)
LOCAL _array[1,1], cobjname, oobject, j, maxobj, x, ocontrol, ntabindex

maxobj = AMEMBERS(_array, ocontainer, 1)

ntabindex = 99999

ocontrol  = .F.

FOR j = 1 TO maxobj
	IF _array[j,2] = "Object"
		cobjname = _array[j,1]
		oobject = ocontainer.&cobjname

		IF UPPER(oobject.BASECLASS) $ UPPER("TextBox;EditBox;ComboBox;ListBox;CheckBox;Spinner;Container;Grid")
			IF oobject.ENABLED AND oobject.VISIBLE
				IF oobject.TABINDEX < ntabindex AND oobject.TABSTOP
					ocontrol  = oobject
					ntabindex = oobject.TABINDEX
				ENDIF
			ENDIF
		ENDIF

		IF UPPER(oobject.BASECLASS) == UPPER("Page")
			IF oobject.PARENT.ACTIVEPAGE = oobject.PAGEORDER
				IF setfirstfocus(oobject)
					RETURN .T.
				ENDIF
			ENDIF
		ENDIF
	ENDIF
NEXT

IF VARTYPE(ocontrol) = "O"
	IF UPPER(ocontrol.BASECLASS) = "CONTAINER"
		setfirstfocus(ocontrol)
	ELSE
		ocontrol.SETFOCUS()
	ENDIF
	RETURN .T.
ENDIF

RETURN .F.
ENDFUNC

*-------------------------------------------------------
* Function....: GetArgCount
* Called by...:
*
* Abstract....: Return number of argument in a string
*
* Returns.....: Number of argument
*
* Parameters..:  tcArgument, tcSeparator
*
* Notes.......: Default tcSeparator := ;
*-------------------------------------------------------
FUNCTION getargcount(tcargument, tcseparator)

LOCAL narg, i, nmaxlen, lcBaseSeparator

IF VARTYPE(tcargument) # "C"
	RETURN 0
ENDIF

nmaxlen = LEN(tcargument)

IF nmaxlen = 0
	RETURN 0
ENDIF

IF TYPE("goProgram.cListSeparatorChars") == "C" AND !EMPTY(goProgram.cListSeparatorChars)
	lcBaseSeparator = goProgram.cListSeparatorChars
ELSE
	lcBaseSeparator = ";"
ENDIF

IF EMPTY(tcSeparator)
*{ V&U MS 2008-12-05
* If more than one separator listed in goProgram.cListSeparatorChars
	tcSeparator = LEFT(ALLTRIM(lcBaseSeparator), 1)
	tcArgument  = CHRTRAN(tcArgument, ALLTRIM(lcBaseSeparator), REPLICATE(tcSeparator, LEN(lcBaseSeparator)))
*} V&U MS 2008-12-05
ENDIF

narg = OCCURS(tcseparator, tcargument)

narg = narg + 1

RETURN narg
ENDFUNC

*-------------------------------------------------------
* Function....: GetArg
* Called by...:
*
* Abstract....: Return a specific argument from a argument string list
*
* Returns.....: cArgument
*
* Parameters..: tcArgument, tcArgNo, tcSeparator
*
* Notes.......: Default tcSeparator := ;
*-------------------------------------------------------
FUNCTION getarg(tcargument, tcargno, tcseparator)

LOCAL lcBaseSeparator AS STRING

IF TYPE("goProgram.cListSeparatorChars") == "C" AND !EMPTY(goProgram.cListSeparatorChars)
	lcBaseSeparator = ALLTRIM(goProgram.cListSeparatorChars)
ELSE
	lcBaseSeparator = ";"
ENDIF

IF ISNULL(tcargument)
	tcargument = ""
ENDIF

IF VARTYPE(tcargument) # "C"
	RETURN ""
ENDIF

IF PCOUNT() < 3 OR tcseparator == ''
*{ V&U MS 2008-12-05
* If more than one separator listed in goProgram.cListSeparatorChars
	tcSeparator = LEFT(ALLTRIM(lcBaseSeparator), 1)
	tcArgument  = CHRTRAN(tcArgument, ALLTRIM(lcBaseSeparator), REPLICATE(tcSeparator, LEN(lcBaseSeparator)))
*} V&U MS 2008-12-05
ENDIF

tcargument = tcseparator + tcseparator + tcargument + ;
	REPLICATE(tcseparator, MAX(0, tcargno))
tcargument = SUBSTR(tcargument, AT(tcseparator, tcargument, MAX(0, tcargno) + 1) + LEN(tcseparator))
RETURN LEFT(tcargument, AT(tcseparator, tcargument) - 1)
ENDFUNC

*-------------------------------------------------------
* Function....: IncBase10
* Called by...: GetNewId
*
* Abstract....: Generate a incremented char value
*
* Returns.....: cNewValue
*
* Parameters..: tcValue, tnMaxLen
*
* Notes.......: Format string: "0000000000"
*-------------------------------------------------------
FUNCTION incbase10(tcvalue, tnmaxlen)
LOCAL lcnewvalue, lnstringlength

lnstringlength = LEN(tcvalue)

IF PCOUNT() = 2
	lnstringlength = tnmaxlen
ENDIF

IF EMPTY(tcvalue)
	tcvalue = '0'
ENDIF

lcnewvalue = PADL(RIGHT(ALLT(STR(VAL(tcvalue) + 1, lnstringlength)), lnstringlength), lnstringlength)
lcnewvalue = STRTRAN(lcnewvalue,' ', '0')

RETURN lcnewvalue
ENDFUNC

*-------------------------------------------------------
* Function....: Spy
* Called by...:
*
* Abstract....: Display last 4  programs stack calls
*
* Returns.....:
*
* Parameters..:
*
* Notes.......: Use for debugging purposes.
*-------------------------------------------------------
FUNCTION spy
LPARAMETERS tcvariable
LOCAL j,  k,  ctext, xvalue

ctext = "Called by: " + CHR(13)

j = 1

DO WHILE !EMPTY(PROGRAM(j))
	j = j + 1
ENDDO

k = j - 1

IF k > 4
	j = k - 4
ENDIF

DO WHILE j < k
	ctext = ctext + PROGRAM(j) + CHR(13)
	j = j + 1
ENDDO

IF !EMPTY(tcvariable)
	ctext  =  ctext + tcvariable + " = "

	xvalue = &tcvariable

	IF VARTYPE(xvalue) = "C"
		ctext = ctext + xvalue
	ENDIF

	IF VARTYPE(xvalue)  $  "NFI"
		ctext = ctext + TRANSFORM(xvalue)
	ENDIF

	IF VARTYPE(xvalue)  $  "D"
		ctext = ctext + DTOC(xvalue)
	ENDIF

	IF VARTYPE(xvalue)  $  "D"
		ctext = ctext + DTOC(xvalue)
	ENDIF

	IF VARTYPE(xvalue)  $  "L"
		ctext = ctext + IIF(xvalue, ".T.", ".F.")
	ENDIF

ENDIF
IF TYPE("goProgram.class") ="C"
	goProgram.vfxwaitwindow(ctext)
ELSE
	WAIT WINDOW ctext
ENDIF

RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: GetText()
* Called by...:
*
* Abstract....:
*
* Returns.....: String of particular language
*
* Parameters..: message id, language id
*
* Notes.......: Can be used for localization.
*-------------------------------------------------------
FUNCTION gettext(tcmessageid, tclangid)
LOCAL lnoldsession, lnSelect, lctext, lused

lctext = ""

IF EMPTY(tcmessageid)
	RETURN lctext
ENDIF

lnSelect     = SELECT() && JEI RI 2008.06.04 changed from ALIAS() to SELECT()
lnoldsession = SET('datasession')

SET DATASESSION TO 1

IF !USED("vfxmsg")
	LOCAL lctable
	IF VARTYPE(goprogram)#"O"
		*{V&U MS 2011-02-01, Use VfxGetFile instead of GETFILE()
		lctable = VfxGetFile('DBF','Message')
		*}V&U MS 2011-02-01
		IF EMPTY(lctable)
			RETURN '???'
		ENDIF
	ELSE
		lctable = goprogram.cvfxdir + "vfxmsg"
	ENDIF

	USE (lctable) ORDER TAG KEY AGAIN IN 0 ALIAS vfxmsg

ENDIF

IF EMPTY(tclangid)
	IF VARTYPE(goprogram) = "O"
		tclangid = goprogram.getlangid()
	ELSE
		tclangid = "TEXT"
	ENDIF
ELSE
	IF !INLIST(tclangid,"ENG","ESP","FRE","GER","ITA","USR")
		tclangid = "TEXT"
	ENDIF
ENDIF

IF SEEK(UPPER(ALLTRIM(tcmessageid)), "vfxmsg","key")
	lctext = EVAL("vfxmsg." + tclangid)
ELSE
	lctext = "?" + ALLTRIM(tcmessageid) +"?"
ENDIF

SET DATASESSION TO lnoldsession

IF !EMPTY(lnSelect)
	SELECT(lnSelect)
ENDIF

IF EMPTY(lctext)
	lctext = "?" + ALLTRIM(tcmessageid) +"? [" + tclangid +"]"
ENDIF

RELEASE oMsgAdptr
RETURN lctext
ENDFUNC

*-------------------------------------------------------
* Function....: ConvertToChar(tuParam)
* Called by...:
*
* Abstract....: Converts any type into a character type.
*
* Returns.....:
*
* Parameters..: any type
*
* Notes.......:
*-------------------------------------------------------
FUNCTION converttochar(tuparam)
LOCAL lcretval, lctype

lcretval = ""
lctype = VARTYPE(tuparam)

DO CASE
	CASE lctype = "C"
		*{V&U MS 2010-09-30, Replace "¦" with CHR(166)
		lcretval = IIF(ISNULL(tuparam), CHR(166), tuparam)
		*}V&U MS 2010-09-30
	CASE INLIST(lctype, "N", "B", "Y")
		lcretval = STR(tuparam)

	CASE lctype = "L"
		lcretval = IIF(tuparam, "T", "F")

	CASE lctype = "D"
		lcretval = DTOC(tuparam)

	CASE lctype = "T"
		lcretval = TTOC(tuparam)
ENDCASE

RETURN lcretval
ENDFUNC

*-------------------------------------------------------
* Function....: Helpme()
* Called by...:
*
* Abstract....: Allows to edit a context sensitive help entry
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE helpme
LOCAL ladummy[1], retval

*{V&U MP 2014-04-22 6493, MS 2014-06-17 6573 Commented
*!*	LOCAL loActiveForm, locActiveControl
*!*	loActiveForm = goProgram.GetActiveForm()
*!*	locActiveControl = goProgram.GetActiveControl()
PRIVATE pcHelptext, pcBook, pcBook2, pcChaptertcTitle, pcIndex, pcSbarText, pcTtipText, pcComment
IF ADIR(ladummy,"vfxhelp.dbf") = 1 AND TYPE("_Screen.ActiveForm.Class") == "C"
* Edit help
*!*		WITH loActiveForm
*!*			IF TYPE("locActiveControl") ="O" AND !ISNULL(locActiveControl)
	WITH _Screen.ActiveForm 
		IF TYPE("_Screen.ActiveForm.ActiveControl.Class") == "C" AND !ISNULL(_Screen.ActiveForm.ActiveControl)
* Search HelpcontextID
			lnHelpID = 0
*!*				IF LOWER(locActiveControl.BASECLASS) =="grid"
*!*					IF locActiveControl.ACTIVECOLUMN#0
*!*						loControl = locActiveControl
			IF LOWER(_Screen.ActiveForm.ActiveControl.BaseClass) == "grid"
				IF _Screen.ActiveForm.ActiveControl.ActiveColumn # 0
					loControl = _Screen.ActiveForm.ActiveControl
					lcCurrentcontrol = loControl.COLUMNS[loControl.activecolumn].CURRENTCONTROL
					lnHelpID = loControl.COLUMNS[loControl.activecolumn].&lcCurrentcontrol..HELPCONTEXTID
					loControl = .NULL.
					RELEASE loControl
				ENDIF
			ELSE
*!*					lnHelpID = locActiveControl.HELPCONTEXTID
				lnHelpID = _Screen.ActiveForm.ActiveControl.HelpContextID
			ENDIF
*}V&U MP 2014-04-22 

*{ V&U RI 2009-02-23
			IF TYPE("goProgram.Class") == "C" AND !EMPTY(goProgram.cLangID) AND ADIR(laDummy, FORCEEXT("vfxhelp_" + goProgram.cLangID, "DBF")) > 0
				USE ("vfxhelp_" + goProgram.cLangID) IN 0 SHARED AGAIN ALIAS vfxhelp
			ELSE
				USE vfxhelp IN 0 SHARED AGAIN ALIAS vfxhelp
			ENDIF
*} V&U RI 2009-02-23

			IF SEEK(.ACTIVECONTROL.HELPCONTEXTID,"vfxhelp","helpid")
				pcBook = vfxhelp.book
				pcBook2 = vfxhelp.book2
				pcChapter = vfxhelp.chapter
				pcIndex = vfxhelp.INDEX
				pcTitle = vfxhelp.TITLE
				pcHelptext = vfxhelp.helptext
				pcSbarText = vfxhelp.sbartext
				pcTtipText = vfxhelp.ttiptext
				pcComment = vfxhelp.objcomment
				IF EMPTY(m.pcBook)		&& Form Caption
					pcBook = .CAPTION
				ENDIF
				IF EMPTY(m.pcBook2)		&& Page Caption
*!*						IF TYPE("loActiveForm.pgfpageframe") ="O"
					IF TYPE("_Screen.ActiveForm.pgfPageFrame.Class") == "C"
						pcBook2 = .pgfpageframe.PAGES[.pgfpageframe.activepage].CAPTION
					ENDIF
				ENDIF
				IF EMPTY(m.pcChapter)
					IF PEMSTATUS(.ACTIVECONTROL,"controlsource", 5)
						IF "." $ .ACTIVECONTROL.CONTROLSOURCE
							pcChapter = SUBSTR(.ACTIVECONTROL.CONTROLSOURCE, AT(".", .ACTIVECONTROL.CONTROLSOURCE) + 1)
						ELSE
							pcChapter = .ACTIVECONTROL.CONTROLSOURCE
						ENDIF
						pcChapter = PROPER(m.pcChapter)
					ENDIF
				ENDIF
				IF EMPTY(m.pcIndex)
					pcIndex = m.pcChapter +" (" + ALLTRIM(m.pcBook) +", " + ALLTRIM(m.pcBook2) +")"
				ENDIF
				IF EMPTY(m.pcTitle)
					pcTitle = m.pcIndex
				ENDIF
				IF EMPTY(m.pcHelptext)
					IF !EMPTY(.ACTIVECONTROL.TOOLTIPTEXT)
						pcHelptext = .ACTIVECONTROL.TOOLTIPTEXT
					ENDIF
					IF !EMPTY(.ACTIVECONTROL.STATUSBARTEXT)
						pcHelptext = .ACTIVECONTROL.STATUSBARTEXT
					ENDIF
				ENDIF
				IF EMPTY(m.pcSbarText)
					pcSbarText = ALLTRIM(.ACTIVECONTROL.STATUSBARTEXT)
				ENDIF
				IF EMPTY(m.pcTtipText)
					pcTtipText = ALLTRIM(.ACTIVECONTROL.TOOLTIPTEXT)
				ENDIF
				IF EMPTY(m.pcComment)
					pcComment = ALLTRIM(.ACTIVECONTROL.COMMENT)
				ENDIF
				*{V&U VM 2014-02-07 6375
				DO FORM ("FORM\vfxhelp") WITH .F. TO retval
				*}V&U VM 2014-02-07
				IF VARTYPE(retval) ="N"
					IF retval = 1
						REPLACE helptext WITH m.pcHelptext, ;
							book WITH m.pcBook, ;
							book2 WITH m.pcBook2, ;
							chapter WITH m.pcChapter, ;
							TITLE WITH m.pcTitle, ;
							INDEX WITH m.pcIndex, ;
							sbartext WITH m.pcSbarText, ;
							ttiptext WITH m.pcTtipText, ;
							objcomment WITH m.pcComment IN vfxhelp
					ENDIF
				ENDIF
			ELSE
				IF TYPE("goProgram.class") ="C"
					goProgram.vfxwaitwindow("HelpContextID not found: " + ;
						JUSTFNAME(SYS(1271, .ACTIVECONTROL)) +"-" + SYS(1272, .ACTIVECONTROL) +"=" + TRANSFORM(.ACTIVECONTROL.HELPCONTEXTID))
				ELSE
					WAIT WINDOW "HelpContextID not found: " + ;
						JUSTFNAME(SYS(1271, .ACTIVECONTROL)) +"-" + SYS(1272, .ACTIVECONTROL) +"=" + TRANSFORM(.ACTIVECONTROL.HELPCONTEXTID)
				ENDIF
			ENDIF

			IF CURSORGETPROP("Buffering", "vfxhelp") > 1
				TABLEUPDATE(.T., .T., "vfxhelp")
			ENDIF

			USE IN vfxhelp
		ENDIF
	ENDWITH
ELSE
* Show CHM-Help
	contexthelp()
ENDIF
RETURN .T.

*-------------------------------------------------------
* Function....: ContextHelp()
* Called by...:
*
* Abstract....: Calls the context sensitive help system.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION contexthelp()

LOCAL lnHelpID, loCurrentControl

*{V&U MP 2014-04-22 6493, MS 2014-06-17 6573 Undo changes
*!*	LOCAL lcCurrentHelpFile, loActiveForm
*!*	loActiveForm = goProgram.GetActiveForm()
*!*	loActiveControl = goProgram.GetActiveControl()
*}V&U MP 2014-04-22 

lnHelpID = -1

lcCurrentHelpFile = "" 		&& JEI IG 2006.11.28 Add

*!*	IF TYPE("loActiveForm") = "O"
IF TYPE("_screen.ActiveForm.Class") == "C"
*{ JEI IG 2006.11.28 Add
*!*		IF TYPE("loActiveForm.cFormHelpFile") == "C"
*!*			lcCurrentHelpFile = loActiveForm.cFormHelpFile
*!*		ENDIF
	IF TYPE("_screen.ActiveForm.cFormHelpFile") == "C"
		lcCurrentHelpFile = _screen.ActiveForm.cFormHelpFile
	ENDIF 
	IF EMPTY(lcCurrentHelpFile)
*} JEI IG 2006.11.28
*!*			IF TYPE("loActiveControl") = "O"
*!*				loCurrentControl = loActiveControl 
		IF TYPE("_screen.ActiveForm.ActiveControl.Class") == "C"
			loCurrentControl = _screen.ActiveForm.ActiveControl
			DO WHILE .T.
				IF PEMSTATUS(loCurrentControl, "helpcontextid", 5) AND ;
						!EMPTY(loCurrentControl.HELPCONTEXTID)

					lnHelpID = loCurrentControl.HELPCONTEXTID

					EXIT
				ELSE
					IF TYPE("loCurrentControl.Parent") = "O"
						loCurrentControl = loCurrentControl.PARENT
					ELSE
						EXIT
					ENDIF
				ENDIF
			ENDDO
		ELSE
*!*				IF TYPE("loActiveForm.helpcontextid") != "U"
*!*					IF !EMPTY(loActiveForm.HELPCONTEXTID)
*!*						lnHelpID = loActiveForm.HELPCONTEXTID
*!*					ENDIF
*!*				ENDIF
			IF TYPE("_screen.ActiveForm.HelpContextID") != "U"
				IF !EMPTY(_screen.ActiveForm.HelpContextID)
					lnHelpID = _screen.ActiveForm.HelpContextID
				ENDIF
			ENDIF
		ENDIF
	ENDIF
ENDIF

registerhelp()

IF lnHelpID == -1
*{ JEI IG 2006.11.28 Changes
	IF EMPTY(lcCurrentHelpFile) AND TYPE("goprogram.class") =="C"
		lcCurrentHelpFile = goProgram.chelpfile
	ENDIF
	IF !EMPTY(lcCurrentHelpFile)
		lcCurrentHelpFile = ADDBS(SYS(5) + SYS(2003)) + lcCurrentHelpFile
		IF FILE(lcCurrentHelpFile)
			SET HELP TO (lcCurrentHelpFile)
			HELP
		ENDIF
	ENDIF
*} JEI IG 2006.11.28
ELSE
	HELP ID lnHelpID
ENDIF

RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: RegisterHelp()
* Called by...:
*
* Abstract....: Registers help system.
*
* Returns.....: .T. if Foxhelp is available or .F. if not
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION registerhelp()

LOCAL lcBuffer, lnRes, lnResSys

lnResSys = 0

DECLARE INTEGER CLSIDFromProgID IN Ole32.DLL STRING cProdID, STRING @vbuffer
lcBuffer = SPACE(16)
TRY
	lnRes = CLSIDFromProgID(STRCONV("Foxhhelp.Foxhtmlhelp9" + CHR(0), 5), @lcBuffer)
&& CLSIDFromProgID returns 0 if the item is found in registry
&& and non-0 if not found
CATCH
	lnRes = -1
ENDTRY

CLEAR DLLS "CLSIDFromProgID"

IF lnRes <> 0
	lnResSys = goProgram.registercomponent("foxhhelpps9.dll", 1)
	IF lnResSys < 0
		RETURN .F.
	ENDIF
	lnResSys = goProgram.registercomponent("foxhhelp9.EXE", 2)
	IF lnResSys < 0
		RETURN .F.
	ENDIF
ELSE
	RETURN .F.
ENDIF

ENDFUNC

*-------------------------------------------------------
* Function....: addpagedelay()
* Called by...:
*
* Abstract....: Load a container containing controls for a page. Used for delayed instantiation.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION addpagedelay(toform, topage, tcobjname, tcclassname, tlcentered)

IF EMPTY(tcobjname)
	tcobjname = "o"
ENDIF

IF EMPTY(tcclassname)
	RETURN ''
ENDIF

IF PCOUNT() < 5
	tlcentered = .T.
ENDIF

IF TYPE("toPage." + tcobjname) =="U"
	LOCAL loControl

	toform.LOCKSCREEN = .T.

	topage.ADDOBJECT(tcobjname, tcclassname)

	loControl = EVAL("toPage." + tcobjname)

	IF loControl.BASECLASS == "Container"
		loControl.BORDERWIDTH = 0
	ENDIF

	IF PEMSTATUS(loControl,'OnLoadPosition', 5)
		loControl.onloadposition()
	ENDIF

	IF VARTYPE(toform.oresizecontrol) ="O"
		IF tlcentered AND loControl.COMMENT != "<FIX_TOP_LEFT>"
			LOCAL lnfactorx, lnfactory

			lnfactorx = toform.oresizecontrol.nfactorx
			lnfactory = toform.oresizecontrol.nfactory

			loControl.LEFT = (topage.PARENT.PAGEWIDTH  - loControl.WIDTH * lnfactorx) / 2
			loControl.TOP  = (topage.PARENT.PAGEHEIGHT - loControl.HEIGHT * lnfactory) / 2

			IF loControl.LEFT > 2
				loControl.LEFT = loControl.LEFT - 2 * lnfactorx
			ENDIF

			IF loControl.TOP > 2
				loControl.TOP  = loControl.TOP - 2 * lnfactory
			ENDIF

		ENDIF
		toform.oresizecontrol.addcontrol(loControl)
	ENDIF

	loControl.VISIBLE = .T.

	toform.LOCKSCREEN = .F.
ENDIF
IF TYPE("toPage") = "O" AND !ISNULL(toPage) AND PEMSTATUS(toPage, "Refresh", 5)
	toPage.REFRESH()
ENDIF
ENDFUNC

#DEFINE dbf_backlink_len  263
#DEFINE dbf_data_offset   8

*-------------------------------------------------------
* Function....: readdbclink()
* Called by...:
*
* Abstract....: Internal used for client database update.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION readdbclink(tnfilehandle)
LOCAL lcdblink, lnheader

IF tnfilehandle < 0
	RETURN ''
ENDIF

 = FSEEK(tnfilehandle, 0, 0)

IF !INLIST(ASC(FREAD(tnfilehandle, 1)), 48, 49, 50)		&& JEI IG 2006.04.19 Change
** Not a Visual FoxPro Database
	RETURN ''
ENDIF

 = FSEEK(tnfilehandle, dbf_data_offset, 0) && Position of the First Data Record

lnheaderlen = ASC(FREAD(tnfilehandle, 1)) + ASC(FREAD(tnfilehandle, 1)) * 256

IF lnheaderlen < dbf_backlink_len
	RETURN ''
ENDIF

** Backlink to the database
 = FSEEK(tnfilehandle, lnheaderlen - dbf_backlink_len)

lcdblink = ALLTRIM(FREAD(tnfilehandle, dbf_backlink_len))

RETURN STRTRAN(lcdblink, CHR(0),'')
ENDFUNC

*-------------------------------------------------------
* Function....: writedbclink()
* Called by...:
*
* Abstract....: Internal used for client database update.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION writedbclink(tnfilehandle, tcdbc)
LOCAL lnheader

IF tnfilehandle < 0
	RETURN -1
ENDIF

 = FSEEK(tnfilehandle, 0, 0)

IF !INLIST(ASC(FREAD(tnfilehandle, 1)), 48, 49, 50)		&& JEI IG 2006.04.19 Change
** Not a Visual FoxPro Database
	RETURN ''
ENDIF

 = FSEEK(tnfilehandle, dbf_data_offset, 0) && Position of the First Data Record

lnheaderlen = ASC(FREAD(tnfilehandle, 1)) + ASC(FREAD(tnfilehandle, 1)) * 256

IF lnheaderlen < dbf_backlink_len
	RETURN -1
ENDIF

tcdbc = LOWER(ALLTRIM(tcdbc))

IF LEN(tcdbc) > dbf_backlink_len
	RETURN -1
ENDIF

 = FSEEK(tnfilehandle, lnheaderlen - dbf_backlink_len)

lcdblink = tcdbc + REPLICATE(CHR(0), dbf_backlink_len - LEN(tcdbc))

 = FWRITE(tnfilehandle, lcdblink)

RETURN 0
ENDFUNC

*-------------------------------------------------------
* Function....: vfx_checkupdate()
* Called by...:
*
* Abstract....: Check whether a client database update is required.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfx_checkupdate(tofoxapp, tlmute, tcfrom, tcto)
IF TYPE("tcto")#"C"
	tcto =""
ENDIF
IF TYPE("tcfrom")#"C"
	tcfrom =""
ENDIF
IF TYPE("glclientsupport") ="U"
	PUBLIC glclientsupport
	glclientsupport = .F.
ENDIF

IF EMPTY(tcto)  AND  EMPTY(tcfrom)
	IF !EMPTY(datapath_loc)
		tcto = datapath_loc +"\"
		tcfrom = datapath_loc +"\update\"
	ELSE
		RETURN .F.
	ENDIF
ENDIF

IF !EMPTY(tcto)  AND  EMPTY(tcfrom)
	tcfrom = IIF(RIGHT(tcto, 1) = "\", LEFT(tcto, LEN(TRIM(tcto)) -1), TRIM(tcto)) + "\update\"
ENDIF

* Datadir and updatedir must be different!
IF UPPER(ALLTRIM(tcto)) == UPPER(ALLTRIM(tcfrom))
	RETURN .F.
ENDIF

* Are there any files in the update directory?
LOCAL lnhowmany, lok, j, laFile[1], laVersion[1], lnUpdCtrlFile
LOCAL llError, llWorkWithConfig, llUseSTONEFIELD, llNewVersion, llEmptyDBCCreated, llUpdateNeeded AS Logical
LOCAL lnDatabase, lnErrorNo, lnOldWorkArea AS INTEGER
LOCAL lcGenDBCFunction, lcKRTDBFName, lcKRTFile, lcOldDir, lcCurrentVersion, lcLastExeVersion, lcOldMultiLocks, lcBuffer, ;
	lcDatadictTableName, lcOldSetPath AS STRING
LOCAL loError AS EXCEPTION
LOCAL lcDatadictTableName, lnSelected, lcFileName, loArchive, llUpdateOnlyFreeTables, llDoNotRunVfxUpdate, lcSys16, lnPos
DIMENSION aUpdatedFreeFolders[1]
LOCAL loFLocations, lcVfxPath

*{ V&U MS 2008-12-19
IF !EMPTY(goProgram.cUpdateLogFileName) AND ADIR(laDir, goProgram.cUpdateLogFileName) > 0
*{V&U MS 2009-02-25
	goProgram.cUpdateLogFileName = FULLPATH(goProgram.cUpdateLogFileName)
*}V&U MS 2009-02-25
	ADDPROPERTY(goProgram, "lUpdateLogFileNameExist", .T.)
	STRTOFILE("", goProgram.cUpdateLogFileName, 0)
	WriteLogInfo("Database Update started.", goProgram.cUpdateLogFileName)
ELSE
*{V&U MS 2009-02-25
	goProgram.cUpdateLogFileName = ""
*}V&U MS 2009-02-25
	ADDPROPERTY(goProgram, "lUpdateLogFileNameExist", .F.)
ENDIF
*} V&U MS 2008-12-19

lnUpdCtrlFile = -1
lok = .T.
lnhowmany = ADIR(laFile, tcfrom +"*.*", "A")
lcdbc  = LOWER(tofoxapp.cmaindatabase)
lnDatabase = ADIR(laDB, tcfrom + lcdbc, "A")
llUseSTONEFIELD = .F.
llUpdateNeeded = .T.
#IFDEF _STONEFIELD
	llUseSTONEFIELD = .T.
#ENDIF
llEmptyDBCCreated = .F.

*{ Check File Version
lcLastExeVersion = "0.0.0"
lcCurrentVersion = "0.0.0"
*{ V&U RI 2008-12-10
lcSys16 = SYS(16, 0)
lnPos = AT(":\", lcSys16)
lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
IF AGETFILEVERSION(laVersion, lcSys16) > 0
	IF !EMPTY(laVersion[4])
		lcCurrentVersion = UPPER(ALLTRIM(laVersion[4]))
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("Current EXE version: " + lcCurrentVersion, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2009-02-19
	ENDIF
ELSE
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("No EXE version available.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2009-02-19
	llNewVersion = .F.
ENDIF

*{ Get last version
lnOldWorkArea = SELECT()
goProgram  = tofoxapp
IF goProgram.nVfxSysTableLoc  >= 1
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("CheckAndCreateSQLTable(VFXSys)...", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2009-02-19
	goProgram.CheckAndCreateSQLTable("VFXSys")
	*{V&U MS 2010-05-18
	lnOldAppOnErrorBehavior = goProgram.nAppOnErrorBehavior
	goProgram.nAppOnErrorBehavior = 0
	caVfxSys = 	OPENTABLE("select * from VFXSys", "IDVFXSYS", .F., "", ;
				.T., , "VFXSys", "cAppVFXDataAccess")
	IF !USED("VFXSys")
		caVfxSys = OPENTABLE("select * from VFXSys", "magic_id", .T., "", ;
				.T., , "VFXSys", "cAppVFXDataAccess")		
	ENDIF 	
	goProgram.nAppOnErrorBehavior = lnOldAppOnErrorBehavior	
	*}V&U MS 2010-05-18	
ELSE
	llError = .F.
	TRY
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("Open VFXSys", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2009-02-19
		USE VFXSys IN 0 AGAIN SHARED
	CATCH
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** Error opening VFXSys. Error: " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2009-02-19
		llError = .T.
	ENDTRY
	IF llError
		 = goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ANERROROCCURREDWHENUPDATINGDATABASE, MSG_ANERROROCCURREDWHENUPDATINGDATABASE), ;
			0 + 16, _SCREEN.CAPTION)
		RETURN .F.
	ENDIF
ENDIF
lcOldMultiLocks = SET("Multilocks")
SET MULTILOCKS ON
SELECT VFXSys
CURSORSETPROP("Buffering", 3)
IF RECCOUNT("VFXSys") = 0
	APPEND BLANK
ENDIF

IF VERSION(2) = 0		&& update AppVersion only when run in EXE
	IF TYPE("VFXSys.AppVersion") = "C"
		lcBuffer = ALLTRIM(VFXSys.AppVersion)
		IF !EMPTY(lcBuffer)
			lcLastExeVersion = lcBuffer
		ENDIF
	ENDIF
ENDIF

RELEASE caVfxSys
IF USED("VFXSys")
	USE IN "VFXSys"
ENDIF

SELECT(lnOldWorkArea)
SET MULTILOCKS &lcOldMultiLocks
*} Get last version
*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Comparing file versions.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U MS 2009-02-19
IF VersNr(UPPER(ALLTRIM(lcLastExeVersion))) < VersNr(lcCurrentVersion)
	llNewVersion = .T.
ENDIF

LOCAL lcOldStatusFieldType, lnFromStatusPos  
lcOldStatusFieldType = ""
IF llNewVersion
	*{ V&U RI 2010-03-31
	TRY 
		IF TYPE("goProgram.Class") == "C"
			IF goProgram.nvfxSysTableLoc >= 1
				*{V&U MS 2010-05-18, OpenTable can call onError. Prevent showing error message in exe
				lnOldAppOnErrorBehavior = goProgram.nAppOnErrorBehavior
				goProgram.nAppOnErrorBehavior = 0
				*{V&U MS 2014-04-28
				loCAVFXTASKLIST = OPENTABLE("vfxTaskList","", .F.,"", 1, ,"tmp_VFXTASKLIST_from", "cAppVFXDataAccess", .F., .F., .F., .F., "USER")
				*}V&U MS 2014-04-28
				goProgram.nAppOnErrorBehavior = lnOldAppOnErrorBehavior 
				*}V&U MS 2010-05-18
			ELSE
				USE VFXTASKLIST SHARED AGAIN IN 0 ALIAS tmp_VFXTASKLIST_from
			ENDIF
		ELSE
			USE VFXTASKLIST SHARED AGAIN IN 0 ALIAS tmp_VFXTASKLIST_from
		ENDIF
	
		AFIELDS(laVfxTaskListFieldsFrom, "tmp_VFXTASKLIST_from")
		lnFromStatusPos = ASCAN(laVfxTaskListFieldsFrom, "status", 1, ALEN(laVfxTaskListFieldsFrom, 1), 1, 7)							
		IF lnFromStatusPos > 0
			lcOldStatusFieldType = laVfxTaskListFieldsFrom[lnFromStatusPos + 1]
		ENDIF 
	CATCH
	FINALLY
		IF USED("tmp_VFXTASKLIST_from")
			USE IN tmp_VFXTASKLIST_from
		ENDIF
	ENDTRY 
	*} V&U RI 2010-03-31
	
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("New exe version is found.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
*{V&U MS 2008-09-04
	IF goProgram.lIgnoreRevision
		lnPos = AT(".", lcLastExeVersion, 2)
		IF lnPos > 0
			lcLastExeVers = LEFT(lcLastExeVersion, lnPos) + "0"
		ENDIF
		lnPos = AT(".", lcCurrentVersion, 2)
		IF lnPos > 0
			lcCurrentVers = LEFT(lcCurrentVersion, lnPos) + "0"
		ENDIF
		IF !(VersNr(UPPER(ALLTRIM(lcLastExeVers))) < VersNr(lcCurrentVers))
			llDoNotRunVfxUpdate = .T.
*{V&U MS 2008-12-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("Client Database Update will not be run because only Revision number is changed.", goProgram.cUpdateLogFileName)
			ENDIF
*}V&U MS 2008-12-19
		ENDIF
	ENDIF
*}V&U MS 2008-09-04
	UpdateConfigVFXStructure()
ELSE
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("Exe version is not newer.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
ENDIF

IF lnhowmany = 0 OR lnDatabase = 0
	IF llNewVersion AND !llUseStoneField AND !llDoNotRunVfxUpdate			&&{V&U MS 2008-09-04
*{ Create empty database
*{V&U MS 2009-02-25
		IF !CreateEmptyDBC(lcdbc, tcfrom, "", goProgram.cUpdateLogFileName)  && Cannot create database
*}V&U MS 2009-02-25
			lnhowmany = 0
			lnDatabase = 0
*{V&U MS 2008-12-19
			lcLogMsg = "Create empty database has returned .F.. Database: " + TRANSFORM(lcDbc) + "; Path: " + TRANSFORM(tcFrom)
*}V&U MS 2008-12-19
		ELSE
			lnhowmany = ADIR(laFile, tcfrom +"*.*", "A")
			lnDatabase = ADIR(laDB, tcfrom + lcdbc, "A")
			llEmptyDBCCreated = .T.
*{V&U MS 2008-12-19
			lcLogMsg = "Empty database is successfully created.  Database: " + TRANSFORM(lcDbc) + "; Path: " + TRANSFORM(tcFrom)
*}V&U MS 2008-12-19
		ENDIF
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo(lcLogMsg, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
*} Create empty database
	ENDIF
*} Check File Version
ENDIF

*{V&U MS 2008-09-02
*Extract from Archive the zip with free table structure
IF llNewVersion AND !llDoNotRunVfxUpdate
	*{V&U MS 2010-03-31
	IF ADIR(laDummy, "UPD$CTRL.KEY") = 1
		= ErrorMsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING))
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** " + IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING), ;
				goProgram.cUpdateLogFileName)
		ENDIF
		RETURN .F.
	ELSE 
		lnUpdCtrlFile = FCREATE("UPD$CTRL.KEY", 0)
		IF lnUpdCtrlFile < 0
			= ErrorMsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_CONFLICT, MSG_UPDATE_CONFLICT))
			SET SAFETY &lcsafety
			goProgram.LockCurrentLoggedUse()
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_CONFLICT, MSG_UPDATE_CONFLICT), ;
					goProgram.cUpdateLogFileName)
			ENDIF
			RETURN .F.
		ENDIF
	ENDIF 
	*}V&U MS 2010-03-31	
	
	USE vfxInternFiles SHARED
	LOCATE FOR TYPE = "ZIP"
	IF FOUND()
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("ZIP for Free database update is found.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
		lcFileName = vfxInternFiles.FileName
		COPY MEMO Content TO (lcFileName)
		loArchive = NEWOBJECT("cArchive")
		IF TYPE("loArchive.Class") == "C"
			loArchive.lQuietMode = .T.
			IF !loArchive.ExtractFromArchive(lcFileName, "*.*", tcFrom, "")
				RELEASE loArchive
				goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_FREETABLESUPDATEFAILED, ;
					MSG_FREETABLESUPDATEFAILED), 0 + 16, _SCREEN.CAPTION)
			ENDIF
			RELEASE loArchive
		ENDIF
		DELETE FILE (lcFileName)
	ENDIF
	USE IN vfxInternFiles
	SELECT(lnOldWorkArea)
*"" - Empty string is used for exe folder
* V&U MS 2010-06-21, Respect lSaveDataBeforeUpdate
	IF ADIR(laDir, "*.dbf") > 0 AND toFoxApp.lSaveDataBeforeUpdate
		CreateBackUp("\", "", .T., "*.dbf;*.fpt;*.cdx", .T., .T.) 	&& V&U MS 2008-12-19 Add last parameter tlQuietMode
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("Backup of free tables from EXE folder is created.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
	ENDIF
*Run the update of free tables in exe dir
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("Run the update of free tables in exe dir.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
	vfx_doFreeUpdate(toFoxApp, ADDBS(tcFrom) + "EXEDIR", "", @aUpdatedFreeFolders)
ENDIF
*}V&U MS 2008-09-02

llWorkWithConfig = (!EMPTY(tofoxapp.cConfigPath) AND FILE(tofoxapp.cConfigPath))
IF llWorkWithConfig
	IF !tofoxapp.LoadConfig("_TempPath")
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("Config is not load correctly.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
		RETURN .F.
	ENDIF

*{ JEI IG 2006.04.19 Change
	SELECT clientname, datapath, updatepath, reportpath, ;
		vfxpath, exportpath, importpath, metapath, connectionstring ;
		FROM _TempPath ;
		WHERE ALLTRIM(ConnStrType) == "P" ;
		INTO CURSOR TempPath
	USE IN _TempPath
*} JEI IG 2006.04.19

	IF RECCOUNT("TempPath") = 0 && No databases for update
		lnhowmany = 0
		lnDatabase = 0
	ENDIF
ENDIF

IF (lnHowMany > 0 AND lnDatabase > 0) OR (llDoNotRunVfxUpdate)			&&{V&U MS 2008-09-04
	IF glClientSupport
* Update all clients using this update directory
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("Update all clients using this update directory.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
		IF !llWorkWithConfig
			*{HC MS 2016-05-11
			loFLocations = NEWOBJECT("cApplicationFileLoc")
			lcVfxPath = loFLocations.GetFileLocation("Vfxpath.dbf")
			IF EMPTY(lcVfxPath)
				lcVfxPath = "Vfxpath"
			ENDIF 
			USE (lcVfxPath) SHARED AGAIN IN 0 ALIAS temppath
			*}HC MS 2016-05-11
		ENDIF
		SELECT temppath
		LOCAL lnreccnt
		COUNT ALL FOR !DELETED() TO lnreccnt

		IF lnreccnt > 0
			COPY TO ARRAY aclients
*{ JEI SN 2006.09.08
			lnSelected = SELECT()
			IF TYPE("goProgram.lcheckfordbupdate") == "L" AND ;
					goProgram.lcheckfordbupdate AND !llUseStoneField AND !llDoNotRunVfxUpdate			&&{V&U MS 2008-09-04

				IF ! vfx_checkfordbupdate(tcfrom, tcto)
*{V&U MS 2008-09-02
					SET DATABASE TO (tcFrom + goProgram.cMainDatabase)
					CLOSE DATABASES
*}V&U MS 2008-09-02
					lcDatadictTableName = "DATADICT"
					IF TYPE("goProgram.cMetadataTableName") = "C" AND !EMPTY(goProgram.cMetadataTableName)
						lcDatadictTableName = UPPER(JUSTSTEM(goProgram.cMetadataTableName))
					ENDIF
					lcDatadictTableName = lcDatadictTableName + "."
					FOR j = 1 TO lnhowmany
						IF lcDatadictTableName $ laFile[j,1]
							LOOP
						ENDIF
						ERASE (tcfrom + laFile[j,1])
					NEXT
*{V&U MS 2008-09-02
					llUpdateOnlyFreeTables = .T.
*{V&U MS 2008-12-19
					IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
						WriteLogInfo("Only free tables will be updated.", goProgram.cUpdateLogFileName)
					ENDIF
*}V&U MS 2008-12-19
*}V&U MS 2008-09-02
				ENDIF
			ENDIF
			SELECT (lnSelected)
*{ JEI SN 2006.09.08
*{ JEI IG 2006.04.04 Add
			AFIELDS(aclientfieldtypes)
			FOR i = 1 TO ALEN(aclientfieldtypes, 1)
				IF aclientfieldtypes[i,2] == "M"	&& field in table is memo
					LOCATE
					FOR j = 1 TO ALEN(aclients, 1)
						LOCATE FOR UPPER(ALLTRIM(clientname)) == UPPER(ALLTRIM(aclients[j,1]))
						aclients[j,i] = UPPER(ALLTRIM(LEFT(&aclientfieldtypes[i,1],64)))
					ENDFOR
				ENDIF
			ENDFOR
			RELEASE aclientfieldtypes
*} JEI IG 2006.04.04
			USE IN temppath

*{ JEI IG 2006.08.17 Add
&& Check for running update
			FOR j = 1 TO ALEN(aclients, 1)
				lcToData = ADDBS(ALLTRIM(aclients[j,2]))
				IF ALEN(aclients, 2) > 8			&& work with config
					IF EMPTY(lcToData) AND TYPE("aclients[j,9]") == "C"
						lcToData = ADDBS(JUSTPATH(ALLTRIM(aclients[j,9])))
					ENDIF
				ELSE 							&& work with vfxpath.dbf
					IF EMPTY(lcToData)
						lcToData = "DATA\"
					ENDIF
				ENDIF
				IF ADIR(ladummy, lcToData + "UPD$CTRL.KEY") = 1
					 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING))
*{V&U MS 2008-12-19
					IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
						WriteLogInfo("*** " + IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING), ;
							goProgram.cUpdateLogFileName)
					ENDIF
*}V&U MS 2008-12-19
					lok = .F.
					EXIT
				ENDIF
			ENDFOR
*} JEI IG 2006.08.17 Add

			IF lok
				LOCAL lcaclients3
				lcaclients3 = ""
				FOR j = 1 TO ALEN(aclients, 1)
*{ JEI IG 2006.09.18 Add
					IF EMPTY(ALLTRIM(aclients[j,3]))
						IF EMPTY(ALLTRIM(aclients[j,2]))
							lcaclients3 = "DATA\UPDATE\"
						ELSE
							lcaclients3 = ADDBS(UPPER(ALLTRIM(aclients[j,2]))) +"UPDATE\"
						ENDIF
					ELSE
						lcaclients3 = UPPER(ADDBS(ALLTRIM(aclients[j,3])))
					ENDIF
*} JEI IG 2006.09.18 Add
					IF (UPPER(TRIM(tcfrom)) == lcaclients3) OR llEmptyDBCCreated
*{ JEI IG 2006.04.18 Add
						lcToData = ALLTRIM(aclients[j,2])
						lcToVFX = ALLTRIM(aclients[j,5])
						IF ALEN(aclients, 2) > 8			&& work with config
							IF EMPTY(lcToData) AND TYPE("aclients[j,9]") == "C"
								lcToData = JUSTPATH(ALLTRIM(aclients[j,9]))
							ENDIF
							IF EMPTY(lcToVFX) AND TYPE("aclients[j,9]") == "C"
								lcToVFX = JUSTPATH(ALLTRIM(aclients[j,9]))
							ENDIF
						ELSE 							&& work with vfxpath.dbf
							IF EMPTY(lcToData)
								lcToData = "DATA"
							ENDIF
							IF EMPTY(lcToVFX)
								lcToVFX = "DATA"
							ENDIF
						ENDIF
						lcToData = ADDBS(lcToData)
						lcToVFX = ADDBS(lcToVFX)
*{V&U MS 2008-09-04
*If lcToVFX is connection string
						IF ADIR(laDir, lcToVFX, "D") = 0
							lcToVFX = .F.
						ENDIF
*}V&U MS 2008-09-04
*} JEI IG 2006.04.18 Add
*{V&U MS 2008-09-04
						IF llDoNotRunVfxUpdate
							lOk = .T.
						ELSE
							lOk = vfx_doupdate(tofoxapp, tlmute, tcfrom, lcToData, lcToVFX, llUpdateOnlyFreeTables, @aUpdatedFreeFolders) 	&& JEI IG 2006.04.18 Change
*{V&U MS 2008-12-19
							IF lOk AND TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
								WriteLogInfo("vfx_doupdate has returned .T.", goProgram.cUpdateLogFileName)
							ENDIF
*}V&U MS 2008-12-19
						ENDIF
						IF lOk
							vfx_UpdateAppVersion(lcToVFX, lcCurrentVersion)
						ENDIF
*}V&U MS 2008-09-04
					ENDIF
				NEXT
			ENDIF
		ENDIF

		IF USED("TempPath")
			USE IN temppath
		ENDIF
	ELSE
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("Update only current client.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
*{V&U MS 2008-09-04
		IF llDoNotRunVfxUpdate
			lOk = .T.
		ELSE
*{ V&U MS 2008-12-02 Modified. In vfx_doupdate if type(tcvfxpath) <> "C", tcvfxpath = tcto
			lOk = vfx_doupdate(tofoxapp, tlmute, tcfrom, tcto, .F., .F., @aUpdatedFreeFolders)
*} V&U MS 2008-12-02
*{ V&U MS 2008-12-19
			IF lOk AND TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("vfx_doupdate has returned .T.", goProgram.cUpdateLogFileName)
			ENDIF
*} V&U MS 2008-12-19
		ENDIF
		IF lOk
			vfx_UpdateAppVersion(tcto, lcCurrentVersion)
		ENDIF
*}V&U MS 2008-09-04
	ENDIF
ENDIF

*{V&U MS 2010-03-31
IF lnUpdCtrlFile != -1
	= FCLOSE(lnUpdCtrlFile)
	ERASE("UPD$CTRL.KEY")
ENDIF 
*}V&U MS 2010-03-31

lOldDispLogin = SQLGETPROP(0,"DispLogin")
SQLSETPROP(0,"DispLogin", 3)
IF llNewVersion
	IF !SQLDatabaseUpdate(tofoxapp, llNewVersion, llDoNotRunVfxUpdate, @aUpdatedFreeFolders)
		IF !_VFP.VISIBLE
			_VFP.VISIBLE = .T.
			_SCREEN.WINDOWSTATE = goProgram.nWindowstate
		ENDIF
		goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DATABASEUPDATEFAILED, ;
			MSG_DATABASEUPDATEFAILED), 0 + 16, _SCREEN.CAPTION)
		lok = (lok AND .F.)

		IF TYPE("goExecCommError") == "O"
* log the error
			LOCAL nOldErrorBehavior, lcErrorName
			nOldErrorBehavior  = goprogram.nAppOnErrorBehavior
			goprogram.nAppOnErrorBehavior = 0
			IF goExecCommError.ErrorCount > 0
				FOR i = 1 TO goExecCommError.ErrorCount
					lcErrorName = "Error" + TRANSFORM(i)
					onerror(goExecCommError.&lcErrorName..ERRORNO, ;
						goExecCommError.&lcErrorName..Method, ;
						goExecCommError.&lcErrorName..LINENO, ;
						goExecCommError.&lcErrorName..MESSAGE, ;
						goExecCommError.&lcErrorName..LINECONTENTS + CHR(13) + CHR(10) + ;
						goExecCommError.&lcErrorName..tcCommand)
				ENDFOR
			ENDIF
*{ V&U MS 2008-12-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** Error in SQLDatabaseUpdate: " + TRANSFORM(goExecCommError.&lcErrorName..MESSAGE), ;
					goProgram.cUpdateLogFileName)
			ENDIF
*} V&U MS 2008-12-19
			goprogram.nAppOnErrorBehavior = nOldErrorBehavior
			RELEASE goExecCommError
		ENDIF
	ELSE
*{ V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("SQLDatabaseUpdate has returned .T.", goProgram.cUpdateLogFileName)
		ENDIF
*} V&U MS 2008-12-19
		lok = (lok AND .T.)
	ENDIF

*{V&U MS 2008-09-04
*Delete the subfolders of Update folder with free tables structure
	lnDirToDel = ADIR(laDirToDel, ADDBS(tcFrom) + "*.", "D")
	FOR lnDir = 1 TO lnDirToDel
		IF !(LEFT(laDirToDel[lnDir, 1], 1) == ".")
			DelDirectory(ADDBS(tcFrom) + laDirToDel[lnDir, 1])
		ENDIF
	ENDFOR
*}V&U MS 2008-09-04
ENDIF

*JEI VM 20080829{
IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ELSE
*JEI VM 20080829}
	WAIT CLEAR
ENDIF

SQLSETPROP(0,"DispLogin", lOldDispLogin)

IF lok OR llEmptyDBCCreated
* delete update directory if all clients succeeded
* or an empty DBC was created
	CLOSE DATABASES ALL
	lcDatadictTableName = "DATADICT"
	IF TYPE("tofoxapp.cMetadataTableName") = "C" AND !EMPTY(tofoxapp.cMetadataTableName)
		lcDatadictTableName = UPPER(JUSTSTEM(tofoxapp.cMetadataTableName))
	ENDIF
	lcDatadictTableName = lcDatadictTableName + "."
	FOR j = 1 TO lnhowmany
		IF lcDatadictTableName $ laFile[j,1]
			LOOP
		ENDIF
		ERASE (tcfrom + laFile[j,1])
	NEXT
ENDIF

*{ JEI IG 2006.10.24 Change
lnOldWorkArea = SELECT()

IF lok AND ;
		(goProgram.nVfxSysTableLoc  >= 1) AND ;
		(VERSION(2) = 0)		&& update AppVersion only when run in EXE

	goProgram.CheckAndCreateSQLTable("VFXSys")
	*{V&U MS 2010-05-13
	caVfxSys = OPENTABLE("select * from VFXSys","IDVFXSYS", .F.,"", ;
		.T., ,"VFXSys","cAppVFXDataAccess")
	*}V&U MS 2010-05-13	
	IF USED("VFXSys")
		CURSORSETPROP("Buffering", 3,"VFXSys")

		IF RECCOUNT("VFXSys") = 0
			APPEND BLANK
		ENDIF
		IF TYPE("VFXSys.AppVersion") == "C"
			REPLACE AppVersion WITH lcCurrentVersion IN VFXSys
			TABLEUPDATE(.F., .T., "VFXSys")
		ENDIF
	ENDIF
*{ V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("AppVersion is updated to " + TRANSFORM(lcCurrentVersion), goProgram.cUpdateLogFileName)
	ENDIF
*} V&U MS 2008-12-19
ENDIF

*{ V&U RI 2010-03-31
LOCAL lnToStatusPos, lcNewStatusFieldType 
lcNewStatusFieldType = ""
TRY 
	IF TYPE("goProgram.Class") == "C"
		IF goProgram.nvfxSysTableLoc >= 1
			*{V&U MS 2014-04-28
			loCAVFXTASKLIST = OPENTABLE("vfxTaskList","", .F.,"", 1, ,"tmp_VFXTASKLIST_to", "cAppVFXDataAccess", .F., .F., .F., .F., "USER")
			*}V&U MS 2014-04-28
		ELSE
			USE VFXTASKLIST SHARED AGAIN IN 0 ALIAS tmp_VFXTASKLIST_to
		ENDIF
	ELSE
		USE VFXTASKLIST SHARED AGAIN IN 0 ALIAS tmp_VFXTASKLIST_to
	ENDIF
	 
	AFIELDS(laVfxTaskListFieldsTo, "tmp_VFXTASKLIST_to")
	lnToStatusPos = ASCAN(laVfxTaskListFieldsTo, "status", 1, ALEN(laVfxTaskListFieldsTo, 1), 1, 7)							
	IF lnToStatusPos > 0
		lcNewStatusFieldType = laVfxTaskListFieldsTo[lnToStatusPos + 1]
	ENDIF 

	* The type of field status is changed from N(1) to I
	IF lcOldStatusFieldType == "N" AND lcNewStatusFieldType == "I"
		REPLACE status WITH status + 1 IN tmp_VFXTASKLIST_to ALL
	ENDIF 
CATCH
FINALLY
	IF USED("tmp_VFXTASKLIST_to")
		USE IN tmp_VFXTASKLIST_to
	ENDIF
ENDTRY 
*} V&U RI 2010-03-31
	
*{ V&U RI 2009-02-19
IF !goProgram.lNoUpdateVfxfopen AND llNewVersion
	LOCAL loCAVFXFOPEN, loVfxfopenFields
	USE vfxInternFiles SHARED
	LOCATE FOR TYPE = "VFXFOPEN"
	lcFileName = FileName
	COPY MEMO Content TO (lcFileName)

	loArchive = NEWOBJECT("cArchive")
	IF TYPE("loArchive.Class") == "C"
		loArchive.lQuietMode = .T.
		IF loArchive.ExtractFromArchive(lcFileName, "*.*", tcFrom, "")
			TRY
				USE vfxfopen_temp IN 0 SHARED
			CATCH
			ENDTRY

			IF USED("vfxfopen_temp")
				IF TYPE("goProgram.Class") == "C"
					IF goProgram.nvfxSysTableLoc >= 1
						*{V&U MS 2010-05-13
						loCAVFXFOPEN = OPENTABLE("vfxFOpen","IDVFXFOPEN", .F.,"INDEX ON UPPER(objectid)+STR(objectno) TAG OBJECT ADDITIVE", ;
							.T., ,"vfxFOpen_old", "cAppVFXDataAccess")
						*}V&U MS 2010-05-13	
					ELSE
						USE vfxfopen SHARED AGAIN IN 0 ALIAS vfxFOpen_old
					ENDIF
				ELSE
					USE vfxfopen SHARED AGAIN IN 0 ALIAS vfxFOpen_old
				ENDIF

				SELECT vfxfopen_temp
				SCAN
					SELECT vfxFOpen_old
					LOCATE FOR UPPER(ALLTRIM(objectid)) + ALLTRIM(TRANSFORM(objectno)) == UPPER(ALLTRIM(vfxfopen_temp.objectid)) + ALLTRIM(TRANSFORM(vfxfopen_temp.objectno))
					IF !FOUND()
*{ V&U RI 2009-03-17
						SELECT vfxfopen_temp
						SCATTER MEMO NAME loVfxfopenFields
						SELECT vfxFOpen_old
						APPEND BLANK IN vfxFOpen_old
						*{V&U MS 2010-10-15
						GATHER NAME loVfxfopenFields FIELDS EXCEPT "IdVfxFopen" MEMO
						*}V&U MS 2010-10-15
						REPLACE objectid WITH vfxfopen_temp.objectid, ;
							objectno WITH vfxfopen_temp.objectno ;
							IN vfxFOpen_old
					ELSE
*} V&U RI 2009-03-17
						REPLACE TITLE WITH vfxfopen_temp.TITLE, ;
							DESCR WITH vfxfopen_temp.DESCR, ;
							FORM WITH vfxfopen_temp.FORM, ;
							PARAMETER WITH vfxfopen_temp.PARAMETER ;
							IN vfxFOpen_old
					ENDIF

					SELECT vfxfopen_temp
				ENDSCAN

				SELECT vfxFOpen_old
				SCAN
					SELECT vfxfopen_temp
					LOCATE FOR UPPER(ALLTRIM(objectid)) + ALLTRIM(TRANSFORM(objectno)) == UPPER(ALLTRIM(vfxFOpen_old.objectid)) + ALLTRIM(TRANSFORM(vfxFOpen_old.objectno))
					IF !FOUND()
						DELETE IN vfxFOpen_old
					ENDIF
					SELECT vfxFOpen_old
				ENDSCAN

				IF TYPE("goProgram.Class") == "C" AND goProgram.nvfxSysTableLoc >= 1
					TABLEUPDATE(.T., .F., "vfxFOpen_old")
				ENDIF
				USE IN vfxFOpen_old
				RELEASE loCAVFXFOPEN
				USE IN vfxfopen_temp

			ENDIF

			TRY
*{V&U MS 2009-07-02
				DELETE FILE (ADDBS(tcFrom) + "vfxfopen_temp.*")
*}V&U MS 2009-07-02
			CATCH
			ENDTRY

		ENDIF
		RELEASE loArchive
	ENDIF

	TRY
		DELETE FILE (lcFileName)
	CATCH
	ENDTRY

	USE IN vfxInternFiles
	SELECT(lnOldWorkArea)
ENDIF
*} V&U RI 2009-02-19

IF TYPE("caVfxSys") # "U"
	RELEASE caVfxSys
ENDIF

IF USED("VFXSys")
	USE IN "VFXSys"
ENDIF

SELECT(lnOldWorkArea)
SET MULTILOCKS &lcOldMultiLocks
*} JEI IG 2006.10.24

RETURN lok

*-------------------------------------------------------
* Function....: vfx_doFreeUpdate()
* Called by...: vfx_doUpdate()
*
* Abstract....: Internal used for client database update for free tables.
*
* Returns.....: Logical (.T. or .F.)
*
* Parameters..:	toFoxApp, tcFrom, tcTo
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfx_doFreeUpdate(toFoxApp, tcFrom, tcTo, taUpdatedFreeFolders)
LOCAL 	llRes, lnClientFiles, lnInstallFiles, lnFile, lcTempDir, lnFilesToCopy, ;
	lnCopyFile, lnFromFiles, lnToFiles, lnUpdFile, lnNeedUpdate, ;
	lnTempFiles, loError AS EXCEPTION, loProgressBar, lcBackupDir

llRes = .T.
IF ADIR(laDir, tcFrom, "D") = 0
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("Directory " + TRANSFORM(tcFrom) + " is not found.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
	RETURN
ENDIF

*{V&U MS 2008-09-04
IF ASCAN(taUpdatedFreeFolders, ALLTRIM(LOWER(ADDBS(tcTo))), 1, ALEN(taUpdatedFreeFolders, 1), 1) > 0
	RETURN
ENDIF
IF TYPE("taUpdatedFreeFolders[1]") == "C"
	lnRowNum = ALEN(taUpdatedFreeFolders, 1) + 1
ELSE
	lnRowNum = 1
ENDIF
DIMENSION taUpdatedFreeFolders(lnRowNum)
taUpdatedFreeFolders[lnRowNum] = ALLTRIM(LOWER(ADDBS(tcTo)))
*}V&U MS 2008-09-04

IF toFoxApp.lShowProgressOnUpdate
	loProgressBar = CREATEOBJECT('cZipMtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
	loProgressBar.lAutoReleaseOnComplete = .F.
	loProgressBar.SHOW()
ENDIF

*Check if in client directory are any files
lnClientFiles = ADIR(laFile, ADDBS(tcTo) + "*.dbf", "A")
IF lnClientFiles = 0
*New Installation, copy data from Update directory
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("New Installation, copy data from Update directory.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
	lnInstallFiles = ADIR(laFile, ADDBS(tcFrom) + "*.*", "A")
	IF lnInstallFiles > 0
		FOR lnFile = 1 TO lnInstallFiles
			IF toFoxApp.lShowProgressOnUpdate
				IF TYPE("laFile[lnFile, 1]") # "C"
					loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), lnInstallFiles, 1)
				ELSE
					loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) + " " + ;
						UPPER(laFile[lnFile, 1]), lnInstallFiles, lnFile)
				ENDIF
				INKEY(.1, "H")
			ENDIF
			COPY FILE (ADDBS(tcFrom) + laFile[lnFile, 1]) TO (ADDBS(tcTo) + laFile[lnFile, 1])
		NEXT
	ENDIF
ELSE
	lnInstallFiles = ADIR(laFile, ADDBS(tcFrom) + "*.dbf", "A")
	IF lnInstallFiles > 0
		TRY
*Copy files from update directory in temporary directory under client directory
*{V&U MS 2008-12-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("Copy files from update directory in temporary directory under client directory.", goProgram.cUpdateLogFileName)
			ENDIF
*}V&U MS 2008-12-19
			lcTempDir = ADDBS(tcTo) + "T" + SUBSTR(SYS(2015), 4, 7)
			MD (lcTempDir)
			lnFilesToCopy = ADIR(laFile, ADDBS(tcFrom) + "*.*", "A")
			FOR lnCopyFile = 1 TO lnFilesToCopy
				IF toFoxApp.lShowProgressOnUpdate
					IF TYPE("laFile[lnCopyFile, 1]") # "C"
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), lnFilesToCopy, 1)
					ELSE
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) + " " + ;
							UPPER(laFile[lnCopyFile, 1]), lnFilesToCopy, lnCopyFile)
					ENDIF
					INKEY(.1, "H")
				ENDIF
				COPY FILE (ADDBS(tcFrom) + laFile[lnCopyFile, 1]) TO (ADDBS(lcTempDir) + laFile[lnCopyFile, 1])
			NEXT
			IF toFoxApp.lSaveDataBeforeUpdate
				lcBackupDir = ADDBS(tcTo) + "B" + SUBSTR(SYS(2015), 4, 7)
				MD (lcBackupDir)
				lnFilesToCopy = ADIR(laFile, ADDBS(tcTo) + "*.dbf", "A")
				FOR lnCopyFile = 1 TO lnFilesToCopy
					IF toFoxApp.lShowProgressOnUpdate
						IF TYPE("laFile[lnCopyFile, 1]") # "C"
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), lnFilesToCopy, 1)
						ELSE
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) + " " + ;
								UPPER(laFile[lnCopyFile, 1]), lnFilesToCopy, lnCopyFile)
						ENDIF
						INKEY(.1, "H")
					ENDIF
					COPY FILE (ADDBS(tcto) + laFile[lnCopyFile, 1]) TO (ADDBS(lcBackupDir) + laFile[lnCopyFile, 1])
					IF ADIR(laCheckFile, ADDBS(tcto) + FORCEEXT(laFile[lnCopyFile, 1], "cdx")) > 0
						COPY FILE (ADDBS(tcto) + FORCEEXT(laFile[lnCopyFile, 1], "cdx")) TO (ADDBS(lcBackupDir) + FORCEEXT(laFile[lnCopyFile, 1], "cdx"))
					ENDIF
					IF ADIR(laCheckFile, ADDBS(tcto) + FORCEEXT(laFile[lnCopyFile, 1], "fpt")) > 0
						COPY FILE (ADDBS(tcto) + FORCEEXT(laFile[lnCopyFile, 1], "fpt")) TO (ADDBS(lcBackupDir) + FORCEEXT(laFile[lnCopyFile, 1], "fpt"))
					ENDIF
				NEXT
			ENDIF

			lnFromFiles = ADIR(laFromFile, ADDBS(tcFrom) + "*.dbf", "A")
			lnToFiles   = ADIR(laToFile, ADDBS(tcTo) + "*.dbf", "A")
			lnTempFiles = ADIR(laFile, ADDBS(lcTempDir) + "*.dbf", "A")
			FOR lnUpdFile = 1 TO lnTempFiles
				IF toFoxApp.lShowProgressOnUpdate
					IF TYPE("laFile[lnUpdFile, 1]") # "C"
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), lnTempFiles, 1)
					ELSE
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) + " " + ;
							UPPER(laFile[lnUpdFile, 1]), lnTempFiles, lnUpdFile)
					ENDIF
					INKEY(.1, "H")
				ENDIF

				lnNeedUpdate = vfx_NeedUpdate(laFile[lnUpdFile, 1], lcTempDir, tcTo, @laToFile, @laFromFile)
				DO CASE
					CASE lnNeedUpdate = 0
*no update needed, delete in temporary directory
						lcFileToDelete = FULLPATH(ADDBS(lcTempDir) + laFile[lnUpdFile, 1])
						DELETE FILE (lcFileToDelete)
						DELETE FILE (STRTRAN(lcFileToDelete, ".DBF", ".CDX"))
						DELETE FILE (STRTRAN(lcFileToDelete, ".DBF", ".FPT"))
*{V&U MS 2008-12-19
						IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
							WriteLogInfo("No update needed, delete in temporary directory. " + TRANSFORM(lcFileToDelete), ;
								goProgram.cUpdateLogFileName)
						ENDIF
*}V&U MS 2008-12-19
					CASE lnNeedUpdate = 1
*update, append data from client directory into temporary directory
						vfx_TransferData(ADDBS(tcTo) + laFile[lnUpdFile, 1], ADDBS(lcTempDir) + laFile[lnUpdFile, 1], .T.)
*{V&U MS 2008-12-19
						IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
							WriteLogInfo("Update, append data from client directory into temporary directory. " + TRANSFORM(laFile[lnUpdFile, 1]), ;
								goProgram.cUpdateLogFileName)
						ENDIF
*}V&U MS 2008-12-19
				ENDCASE
			NEXT

			lnTempFiles = ADIR(laFile, ADDBS(lcTempDir) + "*.*", "A")
			FOR lnFilesToCopy = 1 TO lnTempFiles
				IF toFoxApp.lShowProgressOnUpdate
					IF TYPE("laFile[lnFilesToCopy, 1]") # "C"
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), lnTempFiles, 1)
					ELSE
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) + " " + ;
							UPPER(laFile[lnFilesToCopy, 1]), lnTempFiles, lnFilesToCopy)
					ENDIF
					INKEY(.1, "H")
				ENDIF
				COPY FILE (ADDBS(lcTempDir) + laFile[lnFilesToCopy, 1]) TO (ADDBS(tcTo) + laFile[lnFilesToCopy, 1])
			NEXT
		CATCH TO loError
			llRes = .F.
		ENDTRY
*{V&U MS 2008-12-19
		IF !llRes AND TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** Error in vfx_doFreeUpdate: " + TRANSFORM(loError.MESSAGE), ;
				goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
	ENDIF
ENDIF

IF !llRes AND toFoxApp.lSaveDataBeforeUpdate
	lnRuturnBackupFiles = ADIR(laBackupFile, ADDBS(lcBackupDir) + "*.*", "A")
	FOR lnBackup = 1 TO lnRuturnBackupFiles
		TRY
			COPY FILE (ADDBS(lcBackupDir) + laBackupFile[lnBackup, 1]) TO (ADDBS(tcTo) + laBackupFile[lnBackup, 1])
		CATCH
		ENDTRY
	NEXT
ENDIF

IF !EMPTY(lcBackupDir)
	DelDirectory(lcBackupDir)
ENDIF
IF !EMPTY(lcTempDir)
	DelDirectory(lcTempDir)
ENDIF

IF toFoxApp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF

RETURN llRes
ENDFUNC

*-------------------------------------------------------
* Function....: vfx_doupdate()
* Called by...:
*
* Abstract....: Internal used for client database update.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfx_doupdate(tofoxapp, tlmute, tcfrom, tcto, tcvfxpath, tlUpdateOnlyFreeTables, taUpdatedFreeFolders)
*!* Check whether an update is currently running with this client.

LOCAL lnUpdCtrlFile
IF TYPE("goprogram.class") <>"C" OR TYPE("tofoxapp") <>"O"
	RETURN
ENDIF

IF ADIR(ladummy, tcto + "UPD$CTRL.KEY") = 1
	 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING))
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING), ;
			goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
	RETURN .F.
ENDIF

IF !_VFP.VISIBLE
	_VFP.VISIBLE = .T.
	_SCREEN.WINDOWSTATE = goProgram.nWindowstate
ENDIF

IF TYPE("tcVfxPath")#"C"
	tcvfxpath = tcto
ENDIF

*{V&U MS 2010-03-31
lnUpdCtrlFile = FCREATE(tcTo + "UPD$CTRL.KEY", 0)
IF lnUpdCtrlFile < 0
	= ErrorMsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_CONFLICT, MSG_UPDATE_CONFLICT))
	SET SAFETY &lcsafety
	goProgram.LockCurrentLoggedUse()
	*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_CONFLICT, MSG_UPDATE_CONFLICT), ;
			goProgram.cUpdateLogFileName)
	ENDIF
	*}V&U MS 2008-12-19
	RETURN .F.
ENDIF
*}V&U MS 2010-03-31
* V&U MS 2008-12-19 Add last parameter

*{V&U MS 2009-09-04 move it after backup JEI VM 2008.07.11{
*{V&U MS 2010-05-20, pass tcTo, tcVfxPath as parameter to Before and After client database update
IF !GoProgram.BeforeClientDatabaseUpdate(tcTo, tcVfxPath)
*}V&U MS 2010-05-20
*{V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("BeforeClientDatabaseUpdate has returned .F.", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U MS 2008-12-19
	RETURN
ENDIF
*JEI VM 2008.07.11}
*{V&U MS 2010-06-21, Respect lSaveDataBeforeUpdate and move it after BeforeClientDatabaseUpdate
IF toFoxApp.lSaveDataBeforeUpdate
	CreateBackUp(tcto, tcvfxpath, .T., .F., .F., .T.)
	*{V&U MS 2008-08-29
	IF !(ALLTRIM(LOWER(tcVfxPath)) == ALLTRIM(LOWER(tcTo))) AND ;
			ADIR(laDir, ADDBS(tcVfxPath) + "*.dbf") > 0

		CreateBackUp(tcVfxPath, tcVfxPath, .T., "*.dbf;*.fpt;*.cdx", .T., .T.)
	ENDIF
	*}V&U MS 2008-08-29
ENDIF 
*}V&U MS 2010-06-21
*!* Initialize this function.
LOCAL lcpath, lcdbc, lnhowmany, laFile[1], laupdated[1], lcdbclink, lnneedupdate, ;
	j, lcsafety, lcTableStemName 
LOCAL lcDatadictTableName AS STRING

lcDatadictTableName = "DATADICT."
IF TYPE("tofoxapp.cMetadataTableName") = "C" AND !EMPTY(tofoxapp.cMetadataTableName)
	lcDatadictTableName = UPPER(JUSTSTEM(tofoxapp.cMetadataTableName)) + "."
ENDIF

lcdbc  = LOWER(tofoxapp.cmaindatabase)

LOCAL lavfxfiles[1], lcto
lavfxfiles[1] = ""
lcto          = ""

IF ALLTRIM(UPPER(tcTo)) <> ALLTRIM(UPPER(tcVfxPath))
	= ADIR(lavfxfiles, tcvfxpath +"*.*", "A")
ENDIF

IF !(".dbc" $ lcdbc)
	lcdbc = lcdbc + ".dbc"
ENDIF
lcsafety = SET('safety')
SET SAFETY OFF
CLOSE DATA ALL
CLOSE TABLES ALL

IF !tlUpdateOnlyFreeTables
*!* Are there any files in the client directory?
	lnhowmany = ADIR(laFile, tcto +"*.*", "A")
	IF lnhowmany = 0
** New Installation, copy data from Update directory
		lnhowmany = ADIR(laFile, tcfrom +"*.*", "A")
		IF lnhowmany > 0
			IF TYPE("toFoxApp.oIntroForm") ="O" AND !ISNULL(tofoxapp.ointroform)
				tofoxapp.ointroform.RELEASE()
***MS
				IF !_VFP.VISIBLE
					_VFP.VISIBLE = .T.
					_SCREEN.WINDOWSTATE = goProgram.nWindowState
				ENDIF
***MS
			ENDIF

*** Progress Bar for Update ***MS
			IF tofoxapp.lShowProgressOnUpdate
				loProgressBar = CREATEOBJECT('czipmtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
				loProgressBar.lautoreleaseoncomplete = .F.
				loProgressBar.SHOW()
			ENDIF
*** Progress Bar for Update ***MS

*!* Copy all files from update directory into client directory ...

*-- Pfad von VFX Tabellen wird berüchsichtigt wenn neue Instalation vorliegt
			LOCAL lctable, llfree, lctemp
			lctable     = SYS(2015)
			llfree      = .F.
			lctemp      = ""

			FOR j = 1 TO lnhowmany

				IF (INLIST(UPPER(JUSTEXT(laFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
						(lcDatadictTableName $ laFile[j,1]) OR ;
						"VFXAPPRIGHTS" $ UPPER(laFile[j,1]) OR ;
						"VFXINTERNFILES" $ UPPER(laFile[j,1]) OR ;
						"VFXPRINTPAGESIZE" $ UPPER(laFile[j,1]) OR ;
						"_REPORTOUTPUTCONFIG" $ UPPER(laFile[j,1]) OR ;
						UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laFile[j,1])
					LOOP
				ENDIF
*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate
					IF TYPE("lafile[j,1]")#"C"
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_COPYING, MSG_COPYING), lnhowmany, j)
					ELSE
						loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_COPYING, MSG_COPYING) + " " + ;
							UPPER(lafile[j,1]), lnhowmany, j)
					ENDIF
					INKEY(.1, "H")
				ENDIF
*** Progress Bar for Update ***MS

*goProgram.vfxwaitwindow(msg_copying+" " + laFile[j,1] + " ...",,,.T.,,,) && MS
				llfree = .F.
				lctemp = ""

				IF UPPER(LEFT(laFile[j,1], 3)) == "VFX"
					lctemp = LEFT(laFile[j,1], AT(".", laFile[j,1]) -1)

					USE (tcfrom + lctemp) IN 0 ALIAS (lctable) SHARED
					SELECT (lctable)
					llfree = EMPTY(CURSORGETPROP("database"))

					USE IN (lctable)

					IF !llfree
						CLOSE DATABASES ALL
					ENDIF
				ENDIF

				lcto = IIF(UPPER(LEFT(laFile[j,1], 3)) == "VFX"  AND  llfree, tcvfxpath, tcto)
				COPY FILE (tcfrom + laFile[j,1]) TO (lcto + laFile[j,1])
			NEXT
*** Progress Bar for Update ***MS
			IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
				loProgressBar.RELEASE()
			ENDIF
*** Progress Bar for Update ***MS
			RELEASE lctable, llfree, lctemp

		ENDIF
		CLOSE DATA ALL
		SET SAFETY &lcsafety
		GoProgram.LockCurrentLoggedUse()
*{V&U MS 2010-05-20, pass tcTo, tcVfxPath as parameter to Before and After client database update
		GoProgram.AfterClientDatabaseUpdate(tcTo, tcVfxPath)		&& JEI VM 2008.07.11
*}V&U MS 2010-05-20
*{V&U MS 2008-12-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("New Installation, Data is copied from Update directory.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U MS 2008-12-19
		RETURN .T.
	ELSE
		DIMENSION ladbcalttables(1), ladbcnewtables(1), lacurrusedtables(1)
		ladbcalttables(1)   = ""
		ladbcnewtables(1)   = ""
		lacurrusedtables(1) = ""

*!* Update Data
		lnhowmany = ADIR(laFile, tcfrom +"*.db?", "A")
		IF lnhowmany > 0
			IF TYPE("toFoxApp.oIntroForm") ="O" AND !ISNULL(tofoxapp.ointroform)
				tofoxapp.ointroform.RELEASE()
***MS
				IF !_VFP.VISIBLE
					_VFP.VISIBLE = .T.
					_SCREEN.WINDOWSTATE = goProgram.nWindowState
				ENDIF
***MS
			ENDIF
			IF tofoxapp.lInformUserForUpdate
				cText = IIF(goProgram.lRuntimeLocalization, ;
					goLocalize.cMSG_USERINFOSTRUCTURELCHANGES, ;
					MSG_USERINFOSTRUCTURELCHANGES)
				 = goProgram.vfxmessagebox(cText, 64, _SCREEN.CAPTION)
				 *{ HC MS 2015-06-26, Show message only once when more than one databases are updated
				 tofoxapp.lInformUserForUpdate = .F.
				 *}
			ENDIF

			LOCAL lcolddbcname, lcnewdbcname, lnfilehandle, lcommit, lcerror, lcsavedir, lcbackupdir, lcvfxbackupdir, ;
				loError AS EXCEPTION
** Some data into Update directory, take that ;)
			lcerror = ON('error')
			__vfx_error = .F.
			lcolddbcname = STRTRAN(lcdbc,".dbc","")
			lcnewdbcname = "X" + SUBSTR(SYS(2015), 4, 7)
			lcommit = .T.
			goProgram.vfxwaitwindow(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SAVINGDATA, MSG_SAVINGDATA), , , .T., , .T.,)
			lcvfxbackupdir = ""
			lcsavedir = ""
			lcbackupdir = ""
			TRY
*!* Copy files from update directory in temporary directory under client directory
				lcsavedir = tcto + "X" + SUBSTR(SYS(2015), 4, 7)
				MD (lcsavedir)
				lnhowmany = ADIR(laFile, tcfrom +"*.*", "A")

*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate
					loProgressBar = CREATEOBJECT('czipmtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
					loProgressBar.lautoreleaseoncomplete = .F.
					loProgressBar.SHOW()
				ENDIF
*** Progress Bar for Update ***MS

				FOR j = 1 TO lnhowmany

					IF (INLIST(UPPER(JUSTEXT(laFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
							(lcDatadictTableName $ laFile[j,1]) OR ;
							"VFXAPPRIGHTS" $ UPPER(laFile[j,1]) OR ;
							"VFXINTERNFILES" $ UPPER(laFile[j,1]) OR ;
							"VFXPRINTPAGESIZE" $ UPPER(laFile[j,1]) OR ;
							"_REPORTOUTPUTCONFIG" $ UPPER(laFile[j,1]) OR ;
							UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laFile[j,1])
						LOOP
					ENDIF
*** Progress Bar for Update ***MS
					IF tofoxapp.lShowProgressOnUpdate
						IF TYPE("lafile[j,1]")#"C"
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SAVINGDATA, MSG_SAVINGDATA), ;
								lnhowmany, j)
						ELSE
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SAVINGDATA, MSG_SAVINGDATA) +" " + ;
								UPPER(lafile[j,1]), lnhowmany, j)
						ENDIF
						INKEY(.1, "H")
					ENDIF
*** Progress Bar for Update ***MS

					COPY FILE (tcfrom + laFile[j,1]) TO (lcsavedir +"\" + laFile[j,1])
				NEXT

*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
					loProgressBar.RELEASE()
				ENDIF
*** Progress Bar for Update ***MS

				IF tofoxapp.lsavedatabeforeupdate
*!* Copy files from client-data directory in temporary directory under client directory (backup)
					lcbackupdir = tcto +"Z" + SUBSTR(SYS(2015), 4, 7)
					MD (lcbackupdir)
					lnhowmany = ADIR(laFile, tcto +"*.*", "A")

*** Progress Bar for Update ***MS
					IF tofoxapp.lShowProgressOnUpdate
						loProgressBar = CREATEOBJECT('cZipMtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
						loProgressBar.lautoreleaseoncomplete = .F.
						loProgressBar.SHOW()
					ENDIF
*** Progress Bar for Update ***MS

					FOR j = 1 TO lnhowmany

						IF (INLIST(UPPER(JUSTEXT(laFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
								(lcDatadictTableName $ laFile[j,1]) OR ;
								"VFXAPPRIGHTS" $ UPPER(laFile[j,1]) OR ;
								"VFXINTERNFILES" $ UPPER(laFile[j,1]) OR ;
								"VFXPRINTPAGESIZE" $ UPPER(laFile[j,1]) OR ;
								"_REPORTOUTPUTCONFIG" $ UPPER(laFile[j,1]) OR ;
								UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laFile[j,1])
							LOOP
						ENDIF
*** Progress Bar for Update ***MS
						IF tofoxapp.lShowProgressOnUpdate
							IF TYPE("lafile[j,1]")#"C"
								loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_BACKUPDATA, MSG_BACKUPDATA), ;
									lnhowmany, j)
							ELSE
								loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_BACKUPDATA, MSG_BACKUPDATA) + ;
									": " + UPPER(lafile[j,1]), lnhowmany, j)
							ENDIF
							INKEY(.1, "H")
						ENDIF
*** Progress Bar for Update ***MS
						COPY FILE (tcto + laFile[j,1]) TO (lcbackupdir +"\" + laFile[j,1])
					NEXT

*** Progress Bar for Update ***MS
					IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
						loProgressBar.RELEASE()
					ENDIF
*** Progress Bar for Update ***MS

*!* Copy VFX_files from client-data directory in temporary directory under client directory (backup)
					IF ALLTRIM(UPPER(tcTo)) <> ALLTRIM(UPPER(tcVfxPath))
						lcvfxbackupdir = tcto +"V" + SUBSTR(SYS(2015), 4, 7)
						MD (lcvfxbackupdir)
						lnhowmany = ADIR(laFile, tcvfxpath +"*.*", "A")

*** Progress Bar for Update ***MS
						IF tofoxapp.lShowProgressOnUpdate
							loProgressBar = CREATEOBJECT('cZipMtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
							loProgressBar.lautoreleaseoncomplete = .F.
							loProgressBar.SHOW()
						ENDIF
*** Progress Bar for Update ***MS

						FOR j = 1 TO lnhowmany

							IF (INLIST(UPPER(JUSTEXT(laFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
									(lcDatadictTableName $ laFile[j,1]) OR ;
									"VFXAPPRIGHTS" $ UPPER(laFile[j,1]) OR ;
									"VFXINTERNFILES" $ UPPER(laFile[j,1]) OR ;
									"VFXPRINTPAGESIZE" $ UPPER(laFile[j,1]) OR ;
									"_REPORTOUTPUTCONFIG" $ UPPER(laFile[j,1]) OR ;
									UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laFile[j,1])
								LOOP
							ENDIF
*** Progress Bar for Update ***MS
							IF tofoxapp.lShowProgressOnUpdate
								IF TYPE("lafile[j,1]")#"C"
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SAVINGDATA, MSG_SAVINGDATA), ;
										lnhowmany, j)
								ELSE
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SAVINGDATA, MSG_SAVINGDATA) + " " + ;
										UPPER(lafile[j,1]), lnhowmany, j)
								ENDIF
								INKEY(.1, "H")
							ENDIF
*** Progress Bar for Update ***MS

* Copy just VFX Tables, no other files.
							IF INLIST(JUSTEXT(laFile[j,1]), "DBF", "FPT", "CDX")
								COPY FILE (tcvfxpath + laFile[j,1]) TO (lcvfxbackupdir +"\" + laFile[j,1])
							ENDIF
						NEXT

*** Progress Bar for Update ***MS
						IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
							loProgressBar.RELEASE()
						ENDIF
*** Progress Bar for Update ***MS

					ENDIF
				ENDIF
				llCopySuccessfully = .T.
			CATCH TO loError
				IF !EMPTY(lcsavedir)
					DelDirectory(lcsavedir)
				ENDIF
				IF !EMPTY(lcbackupdir)
					DelDirectory(lcbackupdir)
				ENDIF
				IF !EMPTY(lcvfxbackupdir)
					DelDirectory(lcvfxbackupdir)
				ENDIF
				llCopySuccessfully = .F.
			ENDTRY
			goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
			IF !llCopySuccessfully
*{V&U MS 2008-12-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** Error while copying data in Backup folder: " + TRANSFORM(loError.MESSAGE), ;
						goProgram.cUpdateLogFileName)
				ENDIF
*}V&U MS 2008-12-19
* log the error
				nOldErrorBehavior  = goprogram.nAppOnErrorBehavior
				goprogram.nAppOnErrorBehavior = 0

				onerror(loError.ERRORNO, PROGRAM(), loError.LINENO, loError.MESSAGE, loError.LINECONTENTS)

				goprogram.nAppOnErrorBehavior = nOldErrorBehavior

				GoProgram.LockCurrentLoggedUse()

* Display a message to the user:
				 = goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ANERROROCCURREDWHENUPDATINGDATABASE, MSG_ANERROROCCURREDWHENUPDATINGDATABASE), ;
					0 + 16, _SCREEN.CAPTION)
				RETURN .F.
			ENDIF
*!* Create update log file.
			LOCAL lnascfile, loError AS EXCEPTION
			lnascfile = -1
			IF !tlmute
*{V&U MS 2008-12-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("------------------------------------------------", goProgram.cUpdateLogFileName)
					WriteLogInfo("Update started.", goProgram.cUpdateLogFileName)
				ELSE
*}V&U MS 2008-12-19
					IF FILE(tcto +"update.log")
						ERASE(tcto +"update.log")
					ENDIF
					lnascfile = FCREATE(tcto +"update.log")
					IF lnascfile != -1
						 = FWRITE(lnascfile,"------------------------------------------------" + CHR(13) + CHR(10) + ;
							"update started on " + TTOC(DATETIME())           + CHR(13) + CHR(10) + ;
							"------------------------------------------------" + CHR(13) + CHR(10) )
					ENDIF
				ENDIF
			ENDIF
			lnErrorNo = 0
			LOCAL lcProgram2, lcMessage2, lcLineContents2
			LOCAL lnError2No, lnLine2NO
			lnError2No = 0
			lnLine2NO = 0
			lcProgram2 = ""
			lcMessage2 = ""
			lcLineContents2 = ""
			TRY
*-- Find tables of current DBC
				OPEN DATAB (tcto + lcdbc)
				IF !EMPTY(DBC())
					 = ADBOBJECTS(ladbcalttables, "table")
				ENDIF
				CLOSE DATABASES

*-- Find table of new DBC
				OPEN DATAB (tcfrom + lcdbc)
				IF !EMPTY(DBC())
					 = ADBOBJECTS(ladbcnewtables, "table")
				ENDIF
				CLOSE DATABASES

*-- Check whether old tables are no more used in the new DBC
				OPEN DATAB (tcto + lcdbc)
				SET DATAB TO (tcto + lcdbc)

				lnFromFile = ADIR(lafromfile, tcfrom +"*.dbf", "A")
				lnToFile = ADIR(latofile, tcto  +"*.dbf", "A")

				LOCAL lcdeletefile
				IF lnToFile > 0 AND lnFromFile > 0
					*{V&U MS 2009-12-19
					DIMENSION laRenamedTables(1, 3)
					laRenamedTables[1, 1] = ""
					*}V&U MS 2009-12-19
					FOR i = 1  TO  ALEN(latofile, 1)
*-- Is table in new DBC
*{JEI MS 17.04.2007 Ascan modified
						IF ASCAN(lafromfile, latofile(i, 1), 1, ALEN(lafromfile), 0, 7) = 0
							DO CASE
								*{V&U MS 2009-12-19, Rename table, if in new dbc we'll find same table with prefix tbl. (DBF-CA wizard has renamed it)
								CASE ASCAN(lafromfile, "TBL" + laToFile(i, 1), 1, ALEN(lafromfile), 0, 7) > 0
									lnTableIndex = ASCAN(ladbcalttables, STRTRAN(latofile(i, 1), ".DBF", ""), 1, ALEN(ladbcalttables), 0, 7)
									IF ALEN(laRenamedTables, 1) = 1 AND EMPTY(laRenamedTables[1, 1])
										lnRow = 1
									ELSE 
										lnRow = ALEN(laRenamedTables, 1) + 1
									ENDIF 
									DIMENSION laRenamedTables(lnRow, 3)
									lcTableName = FORCEEXT(ADDBS(TcTo) + DBGETPROP(ladbcalttables(lnTableIndex), "TABLE", "Path"), 'dbf')
									RENAME TABLE laDbcAltTables[lnTableIndex] TO 'tbl' + laDbcAltTables[lnTableIndex]
									lcNewTableName = FORCEEXT(ADDBS(JUSTPATH(TcTo)) + 'tbl' + JUSTSTEM(lcTableName), 'dbf')						
									IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
										WriteLogInfo("Table is renamed From ==> " + lcTableName + ;
													" to " +  lcNewTableName, goProgram.cUpdateLogFileName)
									ELSE
										IF lnAscFile != -1
											 = FWRITE(lnAscFile, "Table is renamed From ==> " + lcTableName + ;
													" to " +  lcNewTableName)
										ENDIF
									ENDIF
									IF FILE(FORCEEXT(lcTableName, "dbf"))
										RENAME (FORCEEXT(lcTableName, "dbf")) TO (FORCEEXT(lcNewTableName, "dbf"))
									ENDIF
									IF FILE(FORCEEXT(lcTableName, "fpt"))
										RENAME (FORCEEXT(lcTableName, "fpt")) TO (FORCEEXT(lcNewTableName, "fpt"))
									ENDIF
									IF FILE(FORCEEXT(lcTableName, "cdx"))
										RENAME (FORCEEXT(lcTableName, "cdx")) TO (FORCEEXT(lcNewTableName, "cdx"))
									ENDIF 
									laRenamedTables[lnRow, 1] = JUSTSTEM(lcTableName)
									laRenamedTables[lnRow, 2] = JUSTSTEM(lcNewTableName)
									laRenamedTables[lnRow, 3] = 'tbl' + laDbcAltTables[lnTableIndex]
								*}V&U MS 2009-12-19
								
								CASE ASCAN(ladbcalttables, STRTRAN(latofile(i, 1), ".DBF", ""), 1, ALEN(ladbcalttables), 0, 7) > 0
									*{ HC VM 2016-04-01 edit
									lcsafety = SET('safety')								
									SET SAFETY OFF
									lcTableStemName = STRTRAN(latofile(i, 1), ".DBF", "")								
									IF INDBC(lcTableStemName, "table") 
											REMOVE TABLE (lcTableStemName)
									ENDIF 
									SET SAFETY &lcsafety	
									*{ HC VM 2016-04-01					
*{V&U MS 2008-12-19
									IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
										WriteLogInfo("Not used table ==> " + TcTo + UPPER(laToFile(i, 1)), goProgram.cUpdateLogFileName)
									ELSE
*}V&U MS 2008-12-19
										IF lnascfile != -1
											 = FWRITE(lnascfile, CHR(9) + "Not used table ==> " + tcto + UPPER(latofile(i, 1)) + CHR(13) + CHR(10))
										ENDIF
									ENDIF

								CASE ASCAN(ladbcalttables, STRTRAN(latofile(i, 1), ".DBF", ""), 1, ALEN(ladbcalttables), 0, 7) = 0
*{V&U MS 2008-12-19
									IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
										WriteLogInfo("Not used table ==> " + TcTo + UPPER(laToFile(i, 1)), goProgram.cUpdateLogFileName)
									ELSE
*}V&U MS 2008-12-19
										IF lnascfile != -1
											 = FWRITE(lnascfile, CHR(9) + "Not used table ==> " + tcto + UPPER(latofile(i, 1)) + CHR(13) + CHR(10))
										ENDIF
									ENDIF

							ENDCASE
						ENDIF
*}JEI MS 17.04.2007
					ENDFOR

					RELEASE lcdeletefile
					RELEASE ARRAY lafromfile, latofile
				ENDIF
				CLOSE DATABASES
				
				*{V&U MS 2009-12-18, Only if there is renamed tables in DBC
				IF !(ALEN(laRenamedTables, 1) = 1 AND EMPTY(laRenamedTables[1, 1]))
					SELECT 0
					USE (tcto + lcdbc)
					FOR lnIndex = 1 TO ALEN(laRenamedTables, 1)
						LOCATE FOR ALLTRIM(UPPER(ObjectType)) == 'TABLE' AND ALLTRIM(UPPER(ObjectName)) == ALLTRIM(UPPER(laRenamedTables[lnIndex, 3])) 
						IF FOUND()
							lcFirstPropPart = SUBSTR(Property, 1, 8)
							lnSizeTableName = LEN(laRenamedTables[lnIndex, 2] + '.dbf') + 7 + 1
							lcSecPropPart  = SUBSTR(Property, 11, 5)
							lcLastPropPart = SUBSTR(Property, 16 + LEN(laRenamedTables[lnIndex, 1] + '.dbf'))
							REPLACE Property WITH 	lcFirstPropPart + BINTOC(lnSizeTableName, "2RS") + lcSecPropPart + ;
													laRenamedTables[lnIndex, 2] + '.dbf' + lcLastPropPart
						ENDIF 
					ENDFOR 
					USE 
				ENDIF 
				*}V&U MS 2009-12-18

*!* Check and update all files
				lnhowmany = ADIR(laFile, lcsavedir +"\*.dbf", "A")
				IF lnhowmany > 0
					DIMENSION laupdated[lnHowMany,1]
				ENDIF
				LOCAL lcfiletodelete
				lcfiletodelete = ""

*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate
					loProgressBar = CREATEOBJECT('cZipMtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
					loProgressBar.lautoreleaseoncomplete = .F.
					loProgressBar.SHOW()
				ENDIF
*** Progress Bar for Update ***MS

*{ JEI IG 2006.04.18 Add
				LOCAL lcFromLoc, lcToLoc
				lcFromLoc = ADDBS(tcto)
				lcToLoc = ADDBS(ADDBS(SYS(2023)) + SYS(2015))
				TRY
					IF !DIRECTORY(lcToLoc)
						MD (lcToLoc)
					ENDIF
					COPY FILE (ADDBS(lcsavedir) + FORCEEXT(lcolddbcname,"dbc")) TO (lcToLoc + FORCEEXT(lcolddbcname,"dbc"))
					COPY FILE (ADDBS(lcsavedir) + FORCEEXT(lcolddbcname,"dct")) TO (lcToLoc + FORCEEXT(lcolddbcname,"dct"))
					COPY FILE (ADDBS(lcsavedir) + FORCEEXT(lcolddbcname,"dcx")) TO (lcToLoc + FORCEEXT(lcolddbcname,"dcx"))
				CATCH TO loError
					lnError2No = loError.ERRORNO
					lcProgram2 = PROGRAM()
					lnLine2NO = loError.LINENO
					lcMessage2 = loError.MESSAGE
					lcLineContents2 = loError.LINECONTENTS
				ENDTRY
*} JEI IG 2006.04.18 Add

				FOR j = 1 TO lnhowmany

					IF (INLIST(UPPER(JUSTEXT(laFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
							(lcDatadictTableName $ laFile[j,1]) OR ;
							"VFXAPPRIGHTS" $ UPPER(laFile[j,1]) OR ;
							"VFXINTERNFILES" $ UPPER(laFile[j,1]) OR ;
							"VFXPRINTPAGESIZE" $ UPPER(laFile[j,1]) OR ;
							"_REPORTOUTPUTCONFIG" $ UPPER(laFile[j,1]) OR ;
							UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laFile[j,1])
						LOOP
					ENDIF
*** Progress Bar for Update ***MS
					IF tofoxapp.lShowProgressOnUpdate
						IF TYPE("lafile[j,1]")#"C"
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), lnhowmany, 1)
						ELSE
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) +" " + ;
								UPPER(lafile[j,1]), lnhowmany, j)
						ENDIF
						INKEY(.1, "H")
					ENDIF
*** Progress Bar for Update ***MS

*{JEI MS 17.04.2007 Modified
					lcto = IIF(ASCAN(lavfxfiles, laFile(j, 1), 1, ALEN(lavfxfiles), 0, 7) > 0, tcvfxpath, tcto)
*}JEI MS 17.04.2007
					lnneedupdate = vfx_needupdate(laFile[j,1], lcsavedir, lcto, @ladbcalttables, @ladbcnewtables)

					DO CASE
						CASE lnneedupdate = 0
* no update needed, delete in temporary directory
*{V&U MS 2008-12-19
							IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
								WriteLogInfo("No update needed, delete in temporary directory. (" + TRANSFORM(laFile[j,1]) + ")", ;
									goProgram.cUpdateLogFileName)
							ENDIF
*}V&U MS 2008-12-19
							lcfiletodelete = FULLPATH(lcsavedir +"\" + laFile[j,1])
							DELETE FILE (lcfiletodelete)
							DELETE FILE (STRTRAN(lcfiletodelete, ".DBF", ".CDX"))
							DELETE FILE (STRTRAN(lcfiletodelete, ".DBF", ".FPT"))

						CASE lnneedupdate = 1
* update, append data from client directory into savedir directory
							lnfilehandle = FOPEN(lcto + laFile[j,1], 2)
							IF lnfilehandle < 0
*{V&U MS 2008-12-19
								IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
									WriteLogInfo("*** Error while opening file. (" + TRANSFORM(laFile[j,1]) + ")", ;
										goProgram.cUpdateLogFileName)
								ENDIF
*}V&U MS 2008-12-19
								lcommit = .F.
*** Progress Bar for Update ***MS
								IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
									loProgressBar.RELEASE()
								ENDIF
*** Progress Bar for Update ***MS
								EXIT
							ENDIF
							laupdated[j] = laFile[j,1]

*** Progress Bar for Update ***MS
							IF tofoxapp.lShowProgressOnUpdate
								IF TYPE("lafile[j,1]")#"C"
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), ;
										lnhowmany,  j)
								ELSE
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) +" " + ;
										UPPER(lafile[j,1]), lnhowmany, j)
								ENDIF
								INKEY(.1, "H")
							ENDIF
*** Progress Bar for Update ***MS
*{V&U MS 2008-12-19
							IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
								WriteLogInfo("Update needed, append data from client directory into save directory. (" + ;
									TRANSFORM(laFile[j,1]) + ")", goProgram.cUpdateLogFileName)
							ELSE
*}V&U MS 2008-12-19
								IF lnascfile != -1
									 = FWRITE(lnascfile, CHR(9) + UPPER(lcto + laFile[j,1]) + CHR(13) + CHR(10))
								ENDIF
							ENDIF

							LOCAL lcmagicid
*{ JEI IG 2006.04.18 Change
							lcmagicid = ;
								CHR(2)  +" " + ;		&& 0x02   FoxBASE
							CHR(3)  +" " + ;		&& 0x03   FoxBASE+/Dbase III plus, no memo
							CHR(48) +" " + ;		&& 0x30   Visual FoxPro
							CHR(49) +" " + ;		&& 0x31   Visual FoxPro, autoincrement enabled
							CHR(50) +" " + ;		&& 0x32   Visual FoxPro, Varchar, Varbinary, or Blob-enabled
							CHR(67) +" " + ;		&& 0x43   dBASE IV SQL table files, no memo
							CHR(99) +" " + ;		&& 0x63   dBASE IV SQL system files, no memo
							CHR(131) +" " + ;		&& 0x83   FoxBASE+/dBASE III PLUS, with memo
							CHR(203) +" " + ;		&& 0xCB   dBASE IV SQL table files, with memo
							CHR(245) +" " + ;		&& 0xF5   FoxPro 2.x (or earlie
							CHR(251)			&& 0xFB   FoxBASE
*} JEI IG 2006.04.18 Change
							 = FSEEK(lnfilehandle, 0, 0)
							IF (FREAD(lnfilehandle, 1) $ lcmagicid)
** It'a a xBase File
								CLOSE DATA ALL
** Rename BackLink to avoid database name conflict
								RENAME (tcto +"\" + lcolddbcname +".dbc") TO (tcto +"\" + lcnewdbcname +".dbc")
								RENAME (tcto +"\" + lcolddbcname +".dct") TO (tcto +"\" + lcnewdbcname +".dct")
								RENAME (tcto +"\" + lcolddbcname +".dcx") TO (tcto +"\" + lcnewdbcname +".dcx")
								IF FILE(lcto + laFile[j,1])
									IF !EMPTY(readdbclink(lnfilehandle))
										 = writedbclink(lnfilehandle, lcnewdbcname +".dbc")
									ENDIF
								ENDIF
								 = FCLOSE(lnfilehandle)

*!* Append data...
*** Progress Bar for Update ***MS
								IF tofoxapp.lShowProgressOnUpdate
									IF TYPE("lafile[j,1]")#"C"
										loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_APPEND, MSG_APPEND), ;
											lnhowmany, j)
									ELSE
										loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_APPEND, MSG_APPEND) +" " + ;
											UPPER(lafile[j,1]), lnhowmany, j)
									ENDIF
									INKEY(.1, "H")
								ENDIF
*** Progress Bar for Update ***MS
*{ JEI IG 2006.04.18 Change
								vfx_transferdata(lcto + laFile[j,1], lcsavedir +"\" + laFile[j,1], .T.)
*{ JEI IG 2006.04.18 Change

								lnfilehandle = FOPEN(lcto + laFile[j,1], 2)
								IF !EMPTY(readdbclink(lnfilehandle))
									 = writedbclink(lnfilehandle, lcolddbcname +".dbc")
								ENDIF

								CLOSE DATA ALL
								RENAME (tcto +"\" + lcnewdbcname +".dbc") TO (tcto +"\" + lcolddbcname +".dbc")
								RENAME (tcto +"\" + lcnewdbcname +".dct") TO (tcto +"\" + lcolddbcname +".dct")
								RENAME (tcto +"\" + lcnewdbcname +".dcx") TO (tcto +"\" + lcolddbcname +".dcx")
							ENDIF
							 = FCLOSE(lnfilehandle)

						CASE lnneedupdate = 2
*!*	new, nothing to do here, the file will be copied from the temporary directory into the client directory
*{V&U MS 2008-12-19
							IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
								WriteLogInfo("The file will be copied from temporary into client directory. (" + ;
									TRANSFORM(laFile[j,1]) + ")", goProgram.cUpdateLogFileName)
							ENDIF
*}V&U MS 2008-12-19

						CASE lnneedupdate = 3

							laupdated[j] = laFile[j,1]
*** Progress Bar for Update ***MS
							IF tofoxapp.lShowProgressOnUpdate
								IF TYPE("lafile[j,1]")#"C"
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), ;
										lnhowmany, j)
								ELSE
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) +" " + ;
										UPPER(lafile[j,1]), lnhowmany, j)
								ENDIF
								INKEY(.1, "H")
							ENDIF
*** Progress Bar for Update ***MS

*{V&U MS 2008-12-19
							IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
								WriteLogInfo("Append data. (" + ;
									TRANSFORM(laFile[j,1]) + ")", goProgram.cUpdateLogFileName)
							ELSE
*}V&U MS 2008-12-19
								IF lnascfile != -1
									 = FWRITE(lnascfile, CHR(9) + UPPER(lcto + laFile[j,1]) + CHR(13) + CHR(10))
								ENDIF
							ENDIF
*!* Append data...
*** Progress Bar for Update ***MS
							IF tofoxapp.lShowProgressOnUpdate
								IF TYPE("lafile[j,1]")#"C"
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING), ;
										lnhowmany, j)
								ELSE
									loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATING, MSG_UPDATING) +" " + ;
										UPPER(lafile[j,1]), lnhowmany, j)
								ENDIF
								INKEY(.1, "H")
							ENDIF
*** Progress Bar for Update ***MS

*{ JEI IG 2006.04.20 Change
							vfx_transferdata(lcto + laFile[j,1], lcsavedir +"\" + laFile[j,1], .T.)
*} JEI IG 2006.04.20 Change

							CLOSE DATA ALL
					ENDCASE
				NEXT
*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
					loProgressBar.RELEASE()
				ENDIF
*** Progress Bar for Update ***MS
*!* Update of all tables finished.
				CLOSE DATA ALL
				SET DATABASE TO
				SET MESSAGE TO ''
				SET MESSAGE TO
*!* Restore data from temporary directory
				lnhowmany = ADIR(laFile, lcsavedir +"\*.*", "A")

*{ JEI IG 2006.04.18 Add
				TRY
					IF DIRECTORY(lcToLoc) AND ;
							FILE(lcToLoc + FORCEEXT(lcolddbcname,"dbc")) AND ;
							FILE(lcToLoc + FORCEEXT(lcolddbcname,"dct")) AND ;
							FILE(lcToLoc + FORCEEXT(lcolddbcname,"dcx"))
						COPY FILE (lcToLoc + FORCEEXT(lcolddbcname,"dbc")) TO (ADDBS(lcsavedir) + FORCEEXT(lcolddbcname,"dbc"))
						COPY FILE (lcToLoc + FORCEEXT(lcolddbcname,"dct")) TO (ADDBS(lcsavedir) + FORCEEXT(lcolddbcname,"dct"))
						COPY FILE (lcToLoc + FORCEEXT(lcolddbcname,"dcx")) TO (ADDBS(lcsavedir) + FORCEEXT(lcolddbcname,"dcx"))
					ENDIF
				CATCH TO loError
					IF lnError2No # 0
						lnError2No = loError.ERRORNO
						lcProgram2 = PROGRAM()
						lnLine2NO = loError.LINENO
						lcMessage2 = loError.MESSAGE
						lcLineContents2 = loError.LINECONTENTS
					ENDIF
				ENDTRY
*} JEI IG 2006.04.18 Add

*{V&U MS 2008-12-19
				IF !EMPTY(lcMessage2) AND TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** Error in update data: " + TRANSFORM(lcMessage2), goProgram.cUpdateLogFileName)
				ENDIF
*}V&U MS 2008-12-19

*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate
					loProgressBar = CREATEOBJECT('cZipMtr', IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING))
					loProgressBar.lautoreleaseoncomplete = .F.
					loProgressBar.SHOW()
				ENDIF
*** Progress Bar for Update ***MS

				FOR j = 1 TO lnhowmany
					IF (INLIST(UPPER(JUSTEXT(laFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
							(lcDatadictTableName $ laFile[j,1]) OR ;
							"VFXAPPRIGHTS" $ UPPER(laFile[j,1]) OR ;
							"VFXINTERNFILES" $ UPPER(laFile[j,1]) OR ;
							"VFXPRINTPAGESIZE" $ UPPER(laFile[j,1]) OR ;
							"_REPORTOUTPUTCONFIG" $ UPPER(laFile[j,1]) OR ;
							UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laFile[j,1])
						LOOP
					ENDIF
*** Progress Bar for Update ***MS
					IF tofoxapp.lShowProgressOnUpdate
						IF TYPE("lafile[j,1]")#"C"
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RESTORE, MSG_RESTORE), ;
								lnhowmany, j)
						ELSE
							loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RESTORE, MSG_RESTORE) +" " + ;
								UPPER(lafile[j,1]), lnhowmany, j)
						ENDIF
						INKEY(.1, "H")
					ENDIF
*** Progress Bar for Update ***MS

*{JEI MS 17.04.2007 Modified
					lcto = IIF(ASCAN(lavfxfiles, laFile(j, 1), 1, ALEN(lavfxfiles), 0, 7) > 0, tcvfxpath, tcto)
*}JEI MS 17.04.2007
					COPY FILE (lcsavedir +"\" + laFile[j,1]) TO (lcto + laFile[j,1])
				NEXT

*** Progress Bar for Update ***MS
				IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
					loProgressBar.RELEASE()
				ENDIF
*** Progress Bar for Update ***MS
				IF !lcommit OR __vfx_error
*{V&U MS 2008-12-19
					IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
						WriteLogInfo("*** Error in update data", goProgram.cUpdateLogFileName)
					ELSE
*}V&U MS 2008-12-19
						IF lnascfile != -1
							 = FWRITE(lnascfile,"------------------------------------------------" + CHR(13) + CHR(10) + ;
								"update error on " + TTOC(DATETIME())             + CHR(13) + CHR(10) + ;
								"------------------------------------------------" + CHR(13) + CHR(10) )
						ENDIF
					ENDIF
					lcommit = .F.
				ENDIF
			CATCH TO loError
				lnErrorNo = loError.ERRORNO

				IF lnascfile != -1
					 = FWRITE(lnascfile,"------------------------------------------------" + CHR(13) + CHR(10) + ;
						"update error on " + TTOC(DATETIME())             + CHR(13) + CHR(10) + ;
						"error: " + loError.Message + CHR(13) + CHR(10) + ;	&& HC MS 2016-03-23
						"------------------------------------------------" + CHR(13) + CHR(10) )
				ENDIF

				CLOSE DATABASES ALL
				CLOSE TABLES ALL
				IF ADIR(laTmp, tcto +"\" + lcnewdbcname +".dbc") > 0
					TRY
						RENAME (tcto +"\" + lcnewdbcname +".dbc") TO (tcto +"\" + lcolddbcname +".dbc")
						RENAME (tcto +"\" + lcnewdbcname +".dct") TO (tcto +"\" + lcolddbcname +".dct")
						RENAME (tcto +"\" + lcnewdbcname +".dcx") TO (tcto +"\" + lcolddbcname +".dcx")
					CATCH
					ENDTRY
				ENDIF
				lnRuturnBackupFile = ADIR(laBackupFile, ADDBS(lcbackupdir) +"*.*", "A")
				FOR j = 1 TO lnRuturnBackupFile

					IF (INLIST(UPPER(JUSTEXT(laBackupFile[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
							(lcDatadictTableName $ laBackupFile[j,1]) OR ;
							"VFXAPPRIGHTS" $ UPPER(laBackupFile[j,1]) OR ;
							"VFXINTERNFILES" $ UPPER(laBackupFile[j,1]) OR ;
							"VFXPRINTPAGESIZE" $ UPPER(laBackupFile[j,1]) OR ;
							"_REPORTOUTPUTCONFIG" $ UPPER(laBackupFile[j,1]) OR ;
							UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laBackupFile[j,1])
						LOOP
					ENDIF
					TRY
						COPY FILE (ADDBS(lcbackupdir) + laBackupFile[j,1]) TO (tcto + laBackupFile[j,1])
					CATCH
					ENDTRY
				NEXT
				lnReturnUpdateFile = ADIR(laUpdateFiles, ADDBS(lcsavedir) + "*.*", "A")
				FOR j = 1 TO lnReturnUpdateFile

					IF (INLIST(UPPER(JUSTEXT(laUpdateFiles[j,1])), "LOG", "KEY", "FXP", "PRG")) OR ;
							(lcDatadictTableName $ laUpdateFiles[j,1]) OR ;
							"VFXAPPRIGHTS" $ UPPER(laUpdateFiles[j,1]) OR ;
							"VFXINTERNFILES" $ UPPER(laUpdateFiles[j,1]) OR ;
							"VFXPRINTPAGESIZE" $ UPPER(laUpdateFiles[j,1]) OR ;
							"_REPORTOUTPUTCONFIG" $ UPPER(laUpdateFiles[j,1]) OR ;
							UPPER(JUSTSTEM(lcdbc) +"krt") $ UPPER(laUpdateFiles[j,1])
						LOOP
					ENDIF
					TRY
						COPY FILE (ADDBS(lcsavedir) + laUpdateFiles[j,1]) TO (tcfrom + laUpdateFiles[j,1])
					CATCH
					ENDTRY
				NEXT
			FINALLY
*!* Delete saved data
				goProgram.vfxwaitwindow(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETE, MSG_DELETE), , , .T., , .T.,)
				DelDirectory(lcsavedir)
				IF tofoxapp.lsavedatabeforeupdate
*!* Delete Backup directory
					DelDirectory(lcbackupdir)
					IF ALLTRIM(UPPER(tcTo)) <> ALLTRIM(UPPER(tcVfxPath))
*!* Delete VFX_Backup directory
						DelDirectory(lcvfxbackupdir)
					ENDIF
				ENDIF

*JEI VM 20080829{
				IF TYPE("goProgram.class") == "C"
					goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
				ELSE
*JEI VM 20080829}
					WAIT CLEAR
				ENDIF
			ENDTRY

*{V&U MS 2008-12-19
			IF lnErrorNo > 0 AND TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** Error in update data: " + TRANSFORM(loError.MESSAGE), goProgram.cUpdateLogFileName)
			ENDIF

			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("------------------------------------------------", goProgram.cUpdateLogFileName)
				WriteLogInfo("Update finished", goProgram.cUpdateLogFileName)
			ELSE
*}V&U MS 2008-12-19
				IF lnascfile != -1
					 = FWRITE(lnascfile,"------------------------------------------------" + CHR(13) + CHR(10) + ;
						"update finished on " + TTOC(DATETIME())          + CHR(13) + CHR(10) + ;
						"------------------------------------------------" + CHR(13) + CHR(10) + CHR(13) + CHR(10))
				ENDIF
			ENDIF

*{JEI MS 27.06.2007
			IF lnascfile != -1
				 = FCLOSE(lnascfile)
			ENDIF
*}JEI MS 27.06.2007

			__vfx_error = .F.
			RELEASE __vfx_error
			ON ERROR &lcerror

			IF lnErrorNo > 0
				goprogram = tofoxapp
				lnOldAppOnErrorBehavior = goprogram.nAppOnErrorBehavior
				goprogram.nAppOnErrorBehavior = 0
				onerror(loError.ERRORNO, PROGRAM(), loError.LINENO, loError.MESSAGE, loError.LINECONTENTS)
				goprogram.nAppOnErrorBehavior = lnOldAppOnErrorBehavior
				IF !_VFP.VISIBLE
					_VFP.VISIBLE = .T.
					_SCREEN.WINDOWSTATE = goProgram.nWindowState
				ENDIF
			ENDIF

			IF lnError2No > 0
				goprogram = tofoxapp
				lnOldAppOnErrorBehavior = goprogram.nAppOnErrorBehavior
				goprogram.nAppOnErrorBehavior = 0
				onerror(lnError2No, lcProgram2, lnLine2NO, lcMessage2, lcLineContents2)
				goprogram.nAppOnErrorBehavior = lnOldAppOnErrorBehavior
				IF !_VFP.VISIBLE
					_VFP.VISIBLE = .T.
					_SCREEN.WINDOWSTATE = GoProgram.nWindowState
				ENDIF
			ENDIF

			IF (lnErrorNo > 0) OR (lnError2No > 0)
				goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DATABASEUPDATEFAILED, ;
					MSG_DATABASEUPDATEFAILED), 0 + 16, _SCREEN.CAPTION)
				RETURN .F.
			ENDIF
		ENDIF

		SET MESSAGE TO ''
		SET MESSAGE TO

*JEI VM 20080829{
		IF TYPE("goProgram.class") == "C"
			goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
		ELSE
*JEI VM 20080829}
			WAIT CLEAR
		ENDIF

		SET SAFETY &lcsafety
		CLOSE DATA ALL
	ENDIF
ENDIF
*{V&U MS 2010-03-31
IF lnUpdCtrlFile != -1
	= FCLOSE(lnUpdCtrlFile)
	ERASE(tcto + "UPD$CTRL.KEY")
ENDIF 
*}V&U MS 2010-03-31
*{V&U MS 2008-09-01
vfx_doFreeUpdate(toFoxApp, ADDBS(tcFrom) + "VFXDIR", tcVfxPath, @taUpdatedFreeFolders)
vfx_doFreeUpdate(toFoxApp, ADDBS(tcFrom) + "DATADIR", tcTo, @taUpdatedFreeFolders)
*}V&U MS 2008-09-01

GoProgram.LockCurrentLoggedUse()
*{V&U MS 2010-05-20, pass tcTo, tcVfxPath as parameter to Before and After client database update
GoProgram.AfterClientDatabaseUpdate(tcTo, tcVfxPath)		&& JEI VM 2008.07.11
*}V&U MS 2010-05-20
RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: vfx_needupdate()
* Called by...:
*
* Abstract....: Internal used for client database update.
*
* Returns.....:	0	--> no Update needed
*				1	--> Need Update
*				2	--> new Table
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfx_needupdate
PARAMETERS tctablename, tcfrom, tcto, taalttables, tanewtables

*-- Parameter kompatibilität
IF TYPE("taAltTables") = "L"  OR  TYPE("taAltTables") = "U"
	DIMENSION taalttables(1)
ENDIF

IF TYPE("taNewTables") = "L"  OR  TYPE("taNewTables") = "U"
	DIMENSION tanewtables(1)
ENDIF

LOCAL lnneedupdate
lnneedupdate = 0
IF FILE(tcto + tctablename)
*{JEI MS 18.04.2007 Modified Ascan
	DO CASE
*-- AltDBC(Table) = FREE  and  New(Table) = NOT FREE
		CASE ASCAN(taalttables, STRTRAN(tctablename,".DBF",""), 1, ALEN(taalttables), 0, 7) = 0  AND ;
				ASCAN(tanewtables, STRTRAN(tctablename,".DBF",""), 1, ALEN(tanewtables), 0, 7) <> 0

			lnneedupdate = 3
			RETURN (lnneedupdate)

*-- AltDBC(Table) = NOT FREE  and  New(Table) = FREE
		CASE ASCAN(taalttables, STRTRAN(tctablename,".DBF",""), 1, ALEN(taalttables), 0, 7) <> 0  AND ;
				ASCAN(tanewtables, STRTRAN(tctablename,".DBF",""), 1, ALEN(tanewtables), 0, 7) = 0
			lnneedupdate = 3
			RETURN (lnneedupdate)

		OTHERWISE
*-- do nothing
	ENDCASE
*{JEI MS 18.04.2007

	USE (tcfrom +"\" + tctablename) IN 0 ALIAS _tempnew SHARED AGAIN
	SELECT _tempnew
	 = AFIELDS(__new)
	USE (tcto + tctablename) IN 0 ALIAS _tempold SHARED AGAIN
	IF !USED("_tempold")
		USE (tcto + tctablename) IN 0 ALIAS _tempold SHARED AGAIN
	ENDIF
	IF !USED("_tempold")
		USE (tcto + tctablename) IN 0 ALIAS _tempold SHARED AGAIN
	ENDIF
	SELECT _tempold
	 = AFIELDS(__old)
	FOR ii = 1 TO 999

		IF (EMPTY(KEY(ii,"_tempnew")) AND !EMPTY(KEY(ii,"_tempold"))) OR ;
				(!EMPTY(KEY(ii,"_tempnew")) AND EMPTY(KEY(ii,"_tempold"))) OR ;
				(KEY(ii,"_tempnew") <> KEY(ii,"_tempold")) OR ;
				(TAG(ii,"_tempnew") <> TAG(ii,"_tempold")) OR ;
				(IDXCOLLATE(ii,"_tempnew") <> IDXCOLLATE(ii,"_tempold")) OR ;
				(DESCENDING(ii,"_tempnew") # DESCENDING(ii,"_tempold")) OR ;
				(!(FOR(ii,"_tempold") == FOR(ii,"_tempnew"))) OR ;
				(PRIMARY(ii,"_tempnew") # PRIMARY(ii,"_tempold")) OR ;
				(CANDIDATE(ii,"_tempnew") # CANDIDATE(ii,"_tempold"))

			lnneedupdate = 1
			EXIT
		ENDIF

		IF EMPTY(KEY(ii,"_tempnew")) OR EMPTY(KEY(ii,"_tempold"))
			EXIT
		ENDIF
	ENDFOR
	IF lnneedupdate = 0
		FOR ii = 1 TO MAX(ALEN(__new), ALEN(__old))
			*{ V&U MS 2012-01-21 5624, Different value on 17 param (NextValue for autoincrementing) should not be considered as structure differance
			IF ASUBSCRIPT(__new, ii, 2) = 17
				LOOP 
			ENDIF 	
			*} V&U MS 2012-01-21
			IF ALEN(__new) <> ALEN(__old) OR ;
					__new[asubscript(__new, ii, 1), asubscript(__new, ii, 2)] <> ;
					__old[asubscript(__old, ii, 1), asubscript(__old, ii, 2)]

				lnneedupdate = 1
				EXIT
			ENDIF
		ENDFOR
	ENDIF
	USE IN _tempnew
	USE IN _tempold
ELSE
	lnneedupdate = 2
ENDIF
RETURN (lnneedupdate)
ENDFUNC

*-------------------------------------------------------
* Function....: _vfx_updateerror()
* Called by...:
*
* Abstract....: Internal used for client database update.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION _vfx_updateerror(tnerror)
DO CASE
	CASE tnerror = 0
* Delete index marker.
		RETURN .T.
	CASE tnerror = 114
* Delete destroyed index files.
		DELETE FILE (tcto + STRTRAN(tctablename,".DBF",".CDX"))
		RETURN .T.
	CASE tnerror = 1558
* Ignore error message caused by tables that are no more part of the new DBC.
		RETURN .T.
	CASE tnerror = 1707
* Delete index marker.
		RETURN .T.
	CASE tnerror = 1884
		IF TYPE("goProgram.class") ="C"
			goProgram.vfxwaitwindow(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UNIQUEKEY, MSG_UNIQUEKEY), , , , , , 1)
		ENDIF
		__vfx_error = .T.
	OTHERWISE
		IF TYPE("goProgram.class") ="C"
			 = goProgram.vfxmessagebox(MESSAGE(), 48, IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ATTENTION, MSG_ATTENTION))
		ENDIF
		__vfx_error = .T.
ENDCASE
ENDFUNC

*-------------------------------------------------------
* Function....: getformparent()
* Called by...:
*
* Abstract....: Returns a reference to the parent for a child form.
*
* Returns.....: Object reference to parent form.
*
* Parameters..: Object reference of child form.
*
* Notes.......:
*-------------------------------------------------------
FUNCTION getformparent(tochildform)
LOCAL loform, loformx, lcformname

loform  = .NULL.

lcformname = tochildform.NAME

FOR j = 1 TO _SCREEN.FORMCOUNT
	IF PEMSTATUS(_SCREEN.FORMS[j],"_VFXClassName", 5)
		IF PEMSTATUS(_SCREEN.FORMS[j],"oFormList", 5)
			loformx = _SCREEN.FORMS[j].oformlist.getchild(lcformname)
			IF !ISNULL(loformx)
				IF COMPOBJ(loformx, tochildform)
					loform = _SCREEN.FORMS[j]
					EXIT
				ENDIF
			ENDIF
		ENDIF
	ENDIF
NEXT

RETURN loform
ENDFUNC

*-------------------------------------------------------
* Function....: vfx_getrelation()
* Called by...:
*
* Abstract....: Return a string containing all realtions of the current alias.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfx_getrelation
LPARAMETERS taChildAlias, tnJ
*{ V&U RI 2009-02-06, parameter tnJ is passed only when recursively called
* recursion is used so that the taChildAlias is filled also with the children of children and so on
LOCAL lcbuffer, lnI, lnJ, lnSelect

lcbuffer = ''
*{ V&U RI 2009-02-06
lnI = 1
IF EMPTY(tnJ)
	lnJ = 1
ELSE
	lnJ = tnJ
ENDIF
*} V&U RI 2009-02-06

IF !EMPTY(ALIAS())
	DO WHILE !EMPTY(RELATION(lnI))
*{ V&U RI 2009-02-06
		lnSelect = SELECT()
		SELECT(TARGET(lnI))
		vfx_getrelation(@taChildAlias, @lnJ)
		SELECT(lnSelect)
*} V&U RI 2009-02-06
		DIMENSION taChildAlias[lnJ,2]
		lcbuffer = lcbuffer + RELATION(lnI) +" into " + TARGET(lnI) +','
		taChildAlias[lnJ,1] = TARGET(lnI)
		taChildAlias[lnJ,2] = RECNO(TARGET(lnI))
		lnJ = lnJ + 1
*{ V&U RI 2009-02-06
		lnI = lnI + 1
*} V&U RI 2009-02-06
	ENDDO

	IF !EMPTY(lcbuffer)
		lcbuffer = LOWER(LEFT(lcbuffer, LEN(lcbuffer) -1))
	ENDIF

ENDIF
*{ V&U RI 2009-02-06
tnJ = lnJ
*} V&U RI 2009-02-06

RETURN lcbuffer
ENDFUNC

*-------------------------------------------------------
* Function....: CopytoExcel()
* Called by...:
*
* Abstract....: Copies data into an excelsheet via OLE.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......: MK
*-------------------------------------------------------
FUNCTION copytoexcel
lcxlsfile	 = ADDBS(SYS(2023)) + "x" + SUBSTR(SYS(2015), 4, 7)
lcxlsfile	 = FULLPATH(lcxlsfile)
COPY TO (lcxlsfile) XL5
LOCAL lo
lo = CREATEOBJECT("excel")
IF VARTYPE(lo) == "O"
	lo.openexcel(.F., .T.)
	IF VARTYPE(lo.oexcel) == "O"
		lo.addworkbook(lcxlsfile)
		lo.activateworkbook(1)
		lo.autoformat()
		lo.setwindowstate(-4137)
	ENDIF
ENDIF
DELETE FILE (lcxlsfile + ".xls")
RELEASE lo
ENDFUNC	&& CopytoExcel

*-------------------------------------------------------
* Function....: askform()
* Called by...:
*
* Abstract....: Shows a form like a messagebox.
*               The captions of the buttons can be passes as parameters and a timeout can be defined.
*
* Returns.....: Numeric, value like messagebox
*
* Parameters..: Messagetext, numeric value like for messagebox, caption of the dialog,
*               caption of button 1, caption of button 2, caption of button 3,
*               use the timeout, timeout value in seconds
*
* Notes.......: MK
*-------------------------------------------------------
FUNCTION askform

LPARAMETERS tctext, tnvalue, tctitle, tcbuttontext1, tcbuttontext2, tcbuttontext3, tltimer, tntimeout

LOCAL lnreturn

IF VARTYPE(goprogram) ="O"
	DO FORM ("vfxaskfm") WITH tctext, tnvalue, tctitle, tcbuttontext1, tcbuttontext2, ;
		tcbuttontext3, tltimer, tntimeout TO lnreturn
ELSE
	DO FORM ("FORM\vfxaskfm") WITH tctext, tnvalue, tctitle, tcbuttontext1, tcbuttontext2, ;
		tcbuttontext3, tltimer, tntimeout TO lnreturn
ENDIF

RETURN lnreturn

*-------------------------------------------------------
* Function....: getaudit()
* Called by...:
*
* Abstract....: Gets information from the audit trail.
*
* Returns.....: Textstring.
*
* Parameters..: Tablename, Recordid
*
* Notes.......:
*-------------------------------------------------------
FUNCTION getaudit
LPARAMETERS tctablename, tnid
LOCAL lcalias, lcretval, lcscanstr, lnlastcatid, ltlastdatetime, lcdeletestring, lnFieldsCount, laFieldsInfo[1,1], lnRow, lnWdth  
lcalias			 = ALIAS()
lcretval		 = ""
lnlastcatid		 = 0
ltlastdatetime	 = {^9999.01.01}
lcdeletestring	 = ""

IF TYPE("goProgram.class") ="C" AND goProgram.nvfxSysTableLoc >= 1
	*{V&U MS 2010-05-13, 2014-04-28
	oAuditAdptr = OPENTABLE("vfxaudit", "IDVFXAUDIT", .F., ;
		"INDEX ON IDVFXAUDIT TAG IDVFXAUDIT ADDITIVE" + CHR(13) + CHR(10) + ;
		"INDEX ON UPPER(table) TAG AUDIT ADDITIVE", .T., ,"tempaudit", "cAppVFXDataAccess", ;
		.F., .F., .F., .F., "TABLE, USER")
	*}V&U MS 2010-05-13, 2014-04-28
ELSE
	USE vfxaudit SHARED AGAIN IN 0 ALIAS tempaudit
ENDIF

*{ V&U VM 2010-06-18
lnFieldsCount = AFIELDS(laFieldsInfo, "tempaudit")
lnRow = ASCAN(laFieldsInfo, 'TABLE', 1, lnFieldsCount, 1, 9)
lnWdth = laFieldsInfo(lnRow,3)

IF TYPE("tnid") ="C"
	SELECT * FROM tempaudit ;
		WHERE UPPER(TABLE) + recordid = PADR(UPPER(tctablename), lnWdth) + tnid ;
		ORDER BY DATETIME ;
		INTO CURSOR temp
ELSE
	SELECT * FROM tempaudit ;
		WHERE UPPER(TABLE) + recordid = ;
		PADR(UPPER(tctablename), lnWdth) + PADR(TRANSFORM(tnid), 30) ;
		ORDER BY DATETIME ;
		INTO CURSOR temp

ENDIF
*} V&U VM 2010-06-18
SCAN
	lcretval = CHANGES + CHR(13) + "______________________" + CHR(13) + lcretval
ENDSCAN
USE IN tempaudit
RELEASE oAuditAdptr
RETURN lcretval

*-------------------------------------------------------
* Function....: _audit_insert()
* Called by...:
*
* Abstract....: Insert audit trigger function. Call this function in the insert trigger of a table to audit all insertions.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION _audit_insert()
_audit("I")
ENDFUNC

*-------------------------------------------------------
* Function....: _audit_update()
* Called by...:
*
* Abstract....: Update audit trigger function. Call this function in the update trigger of a table to audit all updates.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION _audit_update()
_audit("U")
ENDFUNC

*-------------------------------------------------------
* Function....: _audit_delete()
* Called by...:
*
* Abstract....: Delete audit trigger function. Call this function in the delete trigger of a table to audit all deletetions.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION _audit_delete()
_audit("D")
ENDFUNC

*-------------------------------------------------------
* Function....: _audit()
* Called by...:
*
* Abstract....: Internally used of the audit functions.
*
* Returns.....:
*
* Parameters..: tcType
*
* Notes.......:
*-------------------------------------------------------
FUNCTION _audit
LPARAMETERS tcType
LOCAL lGoUser

IF TYPE("pldoaudit") <>"U" AND !pldoaudit
	RETURN .T.
ENDIF

IF TYPE("GoUser.user") ="U"
	lGoUser = getwinuser()
ELSE
	lGoUser = GoUser.USER
ENDIF
LOCAL lcalias, lcdbf, lcrecordid, lcchange, lddatetime, lnpknum, lcinsert, ;
	lcfieldstate, lcfield, lcmemo, j, llchangepoolingcode, llchangeaddressname, llchangeparent, llchangemaster
lcalias		 = ALIAS()
lcdbf		 = DBF(lcalias)
lcdbf		 = STRTRAN(SUBSTR(lcdbf, RAT("\", lcdbf) + 1),".DBF","")
lnpknum		 = getpknum()
IF lnpknum	 = 0
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_CANNOTFINDTHEPRIMARYKEYFORTABLE, MSG_CANNOTFINDTHEPRIMARYKEYFORTABLE) + " " + TRIM(lcdbf) + "." + CHR(13) + ;
			IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_PLEASECONTACTTHESYSTEMADMINISTRATOR, MSG_PLEASECONTACTTHESYSTEMADMINISTRATOR), 16, _SCREEN.CAPTION)
	ENDIF
	RETURN .F.
ENDIF
lcrecordid	 = _tochar(EVAL(KEY(lnpknum)))
lddatetime	 = DATETIME()
lcfieldstate	 = SUBSTR(GETFLDSTATE(-1), 2)

DO CASE
	CASE tcType ="I"
		*{V&U MS 2010-08-09
		PUBLIC __VFX_LastAutoincValue
		__VFX_LastAutoincValue = GETAUTOINCVALUE()
		*}V&U MS 2010-08-09
		IF TYPE("goProgram.class") ="C"
			lcinsert = IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_RECORDHASBEENINSERTEDBY, MSG_RECORDHASBEENINSERTEDBY) + ;
				TRIM(lGoUser) + " " + IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_AT, MSG_AT) + " " + TTOC(lddatetime) + CHR(13) + CHR(10)
		ELSE
			lcinsert = ""
		ENDIF
		FOR j = 1 TO LEN(lcfieldstate)
			lcfield = FIELD(j)
			IF INLIST(SUBSTR(lcfieldstate, j, 1),'2','4')
				lcinsert = lcinsert + CHR(13) + CHR(10) + lcfield + ": " + ;
					STRTRAN(TRIM(_tochar(IIF(TYPE(lcField) == "G", "object", EVALUATE(lcField)))), CHR(10),"")
			ENDIF
		NEXT
		INSERT INTO vfxaudit (TABLE, recordid, USER, DATETIME, CHANGES) ;
			VALUES (lcdbf, lcrecordid, lGoUser, lddatetime, lcinsert)
	CASE tcType ="U"
		lcchange		 = ""
		FOR j = 1 TO LEN(lcfieldstate)
			lcfield = FIELD(j)
			IF INLIST(SUBSTR(lcfieldstate, j, 1),'2','4')
				IF TYPE(lcfield)#"G"
					IF OLDVAL(lcfield) <> EVAL(lcfield)
						lcchange = lcchange + CHR(13) + CHR(10) + lcfield + ": " + ;
							STRTRAN(TRIM(_tochar(OLDVAL(lcfield))), CHR(10),"") + " >>> " + ;
							STRTRAN(TRIM(_tochar(EVAL(lcfield))), CHR(10),"")
					ENDIF
				ENDIF
			ENDIF
		NEXT
		IF !EMPTY(lcchange) AND TYPE("goProgram.class") ="C"
			lcchange = IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_RECORDHASBEENUPDATEDBY, MSG_RECORDHASBEENUPDATEDBY) + " " + ;
				TRIM(lGoUser) + " " + IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_AT, MSG_AT) + " " + TTOC(lddatetime) + CHR(13) + CHR(10) + ;
				lcchange
		ENDIF
		IF !EMPTY(lcchange)
			INSERT INTO vfxaudit (TABLE, recordid, USER, DATETIME, CHANGES) ;
				VALUES (lcdbf, lcrecordid, lGoUser, lddatetime, lcchange)
		ENDIF
	CASE tcType ="D"
		IF TYPE("goProgram.class") ="C"
			INSERT INTO vfxaudit (TABLE, recordid, USER, DATETIME, CHANGES) ;
				VALUES (lcdbf, lcrecordid, lGoUser, lddatetime, ;
				IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_RECORDHASBEENDELETEDBY, MSG_RECORDHASBEENDELETEDBY) + " " + ;
				TRIM(lGoUser) + " " + IIF(goProgram.lRuntimeLocalization, GoLocalize.cMSG_AT, MSG_AT) + " " + TTOC(lddatetime) + CHR(13) + CHR(10))
		ENDIF
ENDCASE
ENDFUNC

*-------------------------------------------------------
* Function....: _tochar()
* Called by...:
*
* Abstract....: Converts any parameter into a string. Transforms , in currencies into .
*
* Returns.....:
*
* Parameters..:	tuparam
*
* Notes.......:
*-------------------------------------------------------
FUNCTION _tochar(tuparam)
LOCAL lcretval, lctype
lcretval = ""
lctype = TYPE("tuParam")
DO CASE
	CASE lctype = "Y"
		lcretval = STRTRAN(ALLTRIM(STR(tuparam, 20, 4)), ",", ".")
	OTHERWISE
		lcretval = TRANSFORM(tuparam)
ENDCASE
RETURN NVL(lcretval, ".NULL.")
ENDFUNC

*-------------------------------------------------------
* Function....: getwinuser()
* Called by...:
*
* Abstract....:
*
* Returns.....: Windows login name of the current user.
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION getwinuser
RETURN SUBSTR(SYS(0), AT("#", SYS(0)) + 2)

*-------------------------------------------------------
* Function....: getpknum()
* Called by...:
*
* Abstract....:
*
* Returns.....: Number of the primaary key index.
*
* Parameters..: tcKey
*
* Notes.......:
*-------------------------------------------------------
FUNCTION getpknum(tcKey)
LOCAL i, lnretval
lnretval = 0
FOR i = 1 TO 254
	IF IIF(EMPTY(tcKey), PRIMARY(i), UPPER(KEY(i)) == UPPER(tcKey))
		lnretval = i
		EXIT
	ENDIF
	IF EMPTY(KEY(i))
		EXIT
	ENDIF
NEXT
RETURN lnretval

*-------------------------------------------------------
* Function....: getcknum()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tnstart
*
* Notes.......:
*-------------------------------------------------------
FUNCTION getcknum()
LPARAMETERS tnstart
LOCAL i, lnretval
IF TYPE("tnstart")#"N"
	tnstart = 1
ENDIF
lnretval = 0
FOR i = tnstart TO 254
	IF CANDIDATE(i)
		lnretval = i
		EXIT
	ENDIF
	IF EMPTY(KEY(i))
		EXIT
	ENDIF
NEXT
RETURN lnretval

*-------------------------------------------------------
* Procedure...: addtofavorite()
* Called by...:
*
* Abstract....: Adds a record to the favorite menu.
*
* Returns.....:
*
* Parameters..:tlAddToTaskList - .T. if called from "Add to Task List" menu item
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE addtofavorite
LPARAMETERS tlAddToTaskList
LOCAL loActiveControl

*{V&U MP 2014-04-22 6493, MS 2014-06-17 6573 Undo changes
*!*	LOCAL loActiveControl, loActiveForm
*!*	loActiveControl = goProgram.GetActiveControl()
*!*	loActiveForm = goProgram.GetActiveForm()

*!*	IF EMPTY(loActiveForm.cfavoritescx)
*!*		RETURN .F.
*!*	ENDIF
IF EMPTY(_Screen.ActiveForm.cFavoriteScx)
	RETURN .F.
ENDIF

LOCAL lnolddatasession, lcpopup, lxid, lcdescr, lnelement, lcmenu, lcfieldname, m.lctemppopup, ;
	m.lcParentBarID, m.lcBuffer, llAlreadyInFav, llChildFavoriteRecord, loChildGrid, lcFavoriteChildId, ;
	lcGridHierarchy, llFavError, lnOldSelect, lcSCXName, loCursorAdapter, lcBitmapTaskList

*{ JEI MS 14.07.2006 cChildGrid favorites
lcGridHierarchy = ""
lcFavoriteChildId = 0
*!*	IF TYPE("loActiveControl") =="O" AND ;
*!*			ALLTRIM(LOWER(loActiveControl.CLASS)) = "cchildgrid" AND ;
*!*			!EMPTY(loActiveControl.cFavoriteID)

*!*		llChildFavoriteRecord = .T.
*!*		loChildGrid = loActiveControl
*!*	ENDIF
IF TYPE("_Screen.ActiveForm.ActiveControl.Class") == "C" AND ;
		ALLTRIM(LOWER(_Screen.ActiveForm.ActiveControl.Class)) = "cchildgrid" AND ;
		!EMPTY(_Screen.ActiveForm.ActiveControl.cFavoriteID)

	llChildFavoriteRecord = .T.
	loChildGrid = _Screen.ActiveForm.ActiveControl
ENDIF
*} JEI MS 14.07.2006

llAlreadyInFav = .F.
llFavError = .F.
lnolddatasession	 = SET("datasession")
*{JEI MS 20.09.2006
lnOldSelect = SELECT()
*}JEI MS 20.09.2006
*!*	SET DATASESSION TO loActiveForm.DATASESSIONID
SET DATASESSION TO _Screen.ActiveForm.DataSessionID

IF EOF() OR BOF()
	SET DATASESSION TO lnolddatasession
	RETURN
ENDIF
*!*	WITH loActiveForm
WITH _Screen.ActiveForm
	lcpopup		= JUSTSTEM(.cfavoritescx)
	lcSCXName	= .cfavoritescx
	lcbitmap	= JUSTFNAME(.ICON)
	lcBitmapTaskList = SYS(2014, .ICON)
	IF EMPTY(.cfavoritemenu)
*!*			lcmenu	= loActiveForm.CAPTION
		lcmenu	= _Screen.ActiveForm.Caption
	ELSE
		lcmenu	= .cfavoritemenu
	ENDIF
*}V&U MP 2014-04-22 
	*{ V&U VM 2011-06-30
	IF LOWER(._VFXClassName) == "cforminternet" 
		lxid = .cURL
		IF EMPTY(.cfavoritedescr)
			lcdescr = .cURL
		ENDIF 
	ELSE 
	*} V&U VM 2011-06-30
		IF EMPTY(.cfavoriteid)
	*{JEI MS 20.09.2006
			SELECT (.cworkalias)
	*}JEI MS 20.09.2006
	*{ V&U RI 2008.08.21
			IF BETWEEN(CURSORGETPROP("SourceType", .cworkalias), 101, 204) && workalias is CA
				llRes = .T.
				TRY
					loCursorAdapter = GETCURSORADAPTER(.cworkalias)
					lcfieldname = loCursorAdapter.KEYFIELDLIST
				CATCH
					llFavError = .T.
				ENDTRY
				IF llFavError OR EMPTY(lcfieldname)
					SET DATASESSION TO lnolddatasession && V&U RI 2008.08.21
					RETURN .F.
				ENDIF
			ELSE
	*} V&U RI 2008.08.21
				lnpknum		 = getpknum()
				IF lnpknum	 = 0
					SET DATASESSION TO lnolddatasession && V&U RI 2008.08.21
					RETURN .F.
				ENDIF
				lcfieldname = KEY(lnpknum)
			ENDIF
			lcfieldname = STRTRAN(lcfieldname,"UPPER(","")
			lcfieldname = STRTRAN(lcfieldname,"LOWER(","")
			lcfieldname = STRTRAN(lcfieldname,"STR(","")
			lcfieldname = STRTRAN(lcfieldname,")","")
			IF TYPE(ALLTRIM(.cworkalias) +"." + lcfieldname)#"U"
				lxid = EVAL(ALLTRIM(.cworkalias) +"." + lcfieldname)
			ELSE
				SET DATASESSION TO lnolddatasession && V&U RI 2008.08.21
				RETURN .F.
			ENDIF
		ELSE
			lxid = EVAL(.cfavoriteid)
		ENDIF
	
		IF EMPTY(.cfavoritedescr)
			lcdescr		 = converttochar(EVAL(FIELD(1)))
		ELSE
	ENDIF 
*{ JEI IG 04.09.2006 Change
		TRY
			lcdescr		 = EVAL(.cfavoritedescr)
		CATCH
			llFavError = .T.
		ENDTRY
		IF llFavError
			IF TYPE("goprogram.class") == "C"
				goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_NOFAVSETTINGS, MSG_NOFAVSETTINGS), 48, ;
					IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ATTENTION, MSG_ATTENTION))
			ELSE
				MESSAGEBOX("No favorites can be initialized, because the necessary settings are missing. Please contact your software developer.", 0 + 48,"Attention")
			ENDIF
			lcdescr		 = converttochar(EVAL(FIELD(1)))
		ENDIF
*} JEI IG 04.09.2006 Change
	ENDIF
ENDWITH

*{ JEI MS 14.07.2006 cChildGrid favorites
IF llChildFavoriteRecord
	WITH loChildGrid
		lcFavoriteChildId = EVALUATE(.cFavoriteId)
		IF EMPTY(.cfavoritedescr)
			lcdescr		 = ALLTRIM(lcdescr) + " - " + converttochar(EVAL(.cFavoriteId))
		ELSE
			lcdescr		 = ALLTRIM(lcdescr) + " - " + ALLTRIM(EVAL(.cfavoritedescr))
		ENDIF
		lcGridHierarchy = SYS(1272, loChildGrid)
		lcGridHierarchy = "ThisForm" + SUBSTR(lcGridHierarchy, AT(".", lcGridHierarchy))
	ENDWITH
ENDIF
*} JEI MS 14.07.2006

IF EMPTY(lcpopup) OR EMPTY(lxid) OR EMPTY(lcbitmap)
	SET DATASESSION TO lnolddatasession
	RETURN .F.
ENDIF

*{ V&U RI 2009-10-18
IF tlAddToTaskList
	goProgram.runForm("vfxTaskEdit")
	goProgram.oLastFormCreated.onInsert(lcSCXName, converttochar(lxid), lcdescr, lcBitmapTaskList)

	SET DATASESSION TO lnolddatasession
	IF !EMPTY(lnOldSelect) AND SELECT() <> lnOldSelect
		SELECT (lnOldSelect)
	ENDIF
	RETURN .T.
ENDIF
*} V&U RI 2009-10-18

* Create random number
barid = INT(SECONDS() * 1000)
lcParentBarID = ""
IF goProgram.nMenuAndToolbarStyle = 0	&& V&U MS 2008-10-14
	IF !POPUP(lcpopup)
		DEFINE BAR barid OF favorites PROMPT lcmenu AFTER 3
		ON BAR barid OF favorites ACTIVATE POPUP (lcpopup)
		DEFINE POPUP (lcpopup) MARGIN RELATIVE COLOR SCHEME 4
		m.lcParentBarID = TRANSFORM(barid)
	ELSE
		m.lctemppopup = LOWER(lcpopup)
		FOR m.kk = 1 TO ALEN(goprogram.afavorites)
			m.lcBuffer = JUSTSTEM(LOWER(getarg(goprogram.afavorites[m.kk], 1)))
			IF lcBuffer == lctemppopup
				m.lcParentBarID = getarg(goprogram.afavorites[m.kk], 9)
				EXIT
			ENDIF
		ENDFOR
	ENDIF

*{ check if menu entry already exists
	FOR m.kk = 1 TO ALEN(goprogram.afavorites)
		m.lcBuffer = ALLTRIM(LOWER(getarg(goprogram.afavorites[m.kk], 3)))
		IF lcBuffer == ALLTRIM(LOWER(lcdescr))
			barid = VAL(getarg(goprogram.afavorites[m.kk], 6))
			llAlreadyInFav = .T.
			EXIT
		ENDIF
	ENDFOR

*} check if menu entry already exists
	RELEASE BAR barid OF (lcpopup)
	DEFINE  BAR barid OF (lcpopup) PROMPT TRIM(lcdescr) BEFORE _MFIRST
	ON SELECTION BAR barid OF (lcpopup) gotofavorite(POPUP(), BAR(), .F., .T.)
	DO CASE
		CASE CNTBAR(lcpopup) = 10
* Create random number
			x = INT(SECONDS() * 1000)
			RELEASE BAR GETBAR(lcpopup, 10) OF (lcpopup)

			DEFINE BAR x OF (lcpopup) PROMPT IIF(goProgram.lRunTimeLocalization, goLocalize.cMEN_MOREFAVORITES, MEN_MOREFAVORITES)

			ON SELECTION BAR x OF (lcpopup) runmorefavorites(POPUP())
		CASE CNTBAR(lcpopup) = 11
			RELEASE BAR GETBAR(lcpopup, 10) OF (lcpopup)
	ENDCASE
ENDIF

lnelement = ASCAN(goprogram.afavorites, lcpopup +";" + converttochar(lxid) +";" + lcdescr, 1, ALEN(goprogram.afavorites), 0, 7)
IF ((lnelement = 0) AND !EMPTY(goprogram.afavorites[1]))
	DIMENSION goprogram.afavorites[alen(goProgram.aFavorites,1)+1]
ELSE
	ADEL(goprogram.afavorites, MAX(1, lnelement))
ENDIF

* SCX-Name(=Popupname);ID;Text;Icon;Menuname;Bar#

lcFavEntry = lcSCXName +";" + converttochar(lxid) +";" + lcdescr +";" + lcbitmap +";" + lcmenu +";" + ;
	converttochar(barid)  + ";" + lcGridHierarchy + ";" + ;
	converttochar(m.lcFavoriteChildId) + ";" + m.lcParentBarID
*{ JEI IG 30.01.2007 Change
* V&U MS 2010-04-27 Add !llAlreadyInFav
IF TYPE("lcFavEntry") == "C" AND !EMPTY(lcFavEntry) AND !llAlreadyInFav
	 = AINS(goprogram.afavorites, 1)
	goprogram.afavorites[1] = lcFavEntry
	IF !llAlreadyInFav AND TYPE("goProgram.oXPopen") == "O" AND !ISNULL(goProgram.oXPopen) AND ;
			IIF(TYPE("goUser.AddFavToXPOpen") == "N", goUser.AddFavToXPOpen = 1, .T.)
		goprogram.oxpopen.addfgroup(lcFavEntry)
	ENDIF
	savefavorites()
ENDIF
*} JEI IG 30.01.2007 Change
SET DATASESSION TO lnolddatasession
*{JEI MS 20.09.2006
IF !EMPTY(lnOldSelect)
	SELECT (lnOldSelect)
ENDIF
*}JEI MS 20.09.2006

*{ V&U MS 2008-10-14
IF goProgram.nMenuAndToolbarStyle > 0
	goProgram.oMenuBar.ReloadFavorites()
ENDIF
*} V&U MS 2008-10-14

ENDPROC

*-------------------------------------------------------
* Procedure...: gotofavorite()
* Called by...:
*
* Abstract....: (If required) run a form and go to the favorite record.
*
* Returns.....:
*
* Parameters..: tcform, barid, tlnotopenform, tlnewinstanceischildform
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE gotofavorite(tcform, barid, tlnotopenform, tlnewinstanceischildform)

*{V&U MP 2014-04-22 6493, MS 2014-06-17 6573 Undo changes
*!*	LOCAL loActiveForm
*!*	loActiveForm = goProgram.GetActiveForm()
IF EMPTY(tcform) OR EMPTY(barid)
	RETURN
ENDIF
LOCAL llformopen, lni, lcname, lcid, lcGridObjectHierarchy, lcDocID

lcid =""
FOR z = 1 TO ALEN(goprogram.afavorites)
	IF VAL(getarg(goprogram.afavorites[z], 6)) = barid
		lcid = getarg(goprogram.afavorites[z], 2)
*{ JEI MS 14.07.2006
		lcGridObjectHierarchy = getarg(goprogram.afavorites[z], 7)
		lcDocID = getarg(goprogram.afavorites[z], 8)
*) JEI MS 14.07.2006
		EXIT
	ENDIF
NEXT

IF EMPTY(lcid)
	RETURN .F.
ENDIF
llformopen = .F.
FOR lni = 1 TO _SCREEN.FORMCOUNT
	IF UPPER(_SCREEN.FORMS(lni).NAME) = "FRM" + UPPER(tcform) AND ;
			_SCREEN.FORMS(lni).nformstatus = 0 AND ;
			(!tlnewinstanceischildform OR EMPTY(_SCREEN.FORMS(lni).ccalledby))
		llformopen = .T.
		lcname = _SCREEN.FORMS(lni).NAME
		IF _SCREEN.FORMS(lni).viafavorite(lcid, lcGridObjectHierarchy, lcDocID)
			ACTIVATE WINDOW (lcname)
		ENDIF
		EXIT
	ENDIF
ENDFOR
IF !llformopen AND !tlnotopenform
	goprogram.runform(tcform)
*!*		loActiveForm.viafavorite(lcid, lcGridObjectHierarchy, lcDocID)
	_Screen.ActiveForm.ViaFavorite(lcid, lcGridObjectHierarchy, lcDocID)
ENDIF
*}V&U MP 2014-04-22 
RETURN (llformopen AND tlnotopenform)
ENDPROC

*-------------------------------------------------------
* Procedure...: savefavorites()
* Called by...:
*
* Abstract....: Save favorites data in the user variable GoUser.favorites.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE savefavorites()
IF TYPE("GoUser.favorites") == "U"
	RETURN
ENDIF
IF TYPE("goprogram.class") <>"C"
	RETURN
ENDIF

LOCAL m.lcBuffer, m.lnPos

WITH goprogram
	GoUser.favorites = ""
	FOR lni = 1 TO ALEN(.afavorites, 1)
		IF !EMPTY(.afavorites[lni])
			m.lnPos = AT(";", .afavorites[lni], 8)
			IF m.lnPos > 0
				lcBuffer = LEFT(.afavorites[lni], m.lnPos - 1)
			ELSE
				lcBuffer = .afavorites[lni]
			ENDIF

			IF lni = 1
				GoUser.favorites = lcBuffer
			ELSE
				GoUser.favorites = GoUser.favorites + CHR(13) + CHR(10) + lcBuffer
			ENDIF
		ENDIF
	ENDFOR
ENDWITH
ENDPROC

*-------------------------------------------------------
* Procedure...: deletefavorite()
* Called by...:
*
* Abstract....: Deletes favorite by a specific NO in
*				goProgram.aFavorites/goUser.Favorites
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE deletefavorite()
LPARAMETERS tvItemNo

IF TYPE("GoUser.favorites") == "U"
	RETURN
ENDIF
IF TYPE("goprogram.class") <>"C"
	RETURN
ENDIF

LOCAL m.lcBuffer, m.lnPos

lnPos = -2

DO CASE
	CASE TYPE("tvItemNo") == "L"
		IF tvItemNo
			lnPos = -1					&& delete all favorites
		ENDIF
	CASE TYPE("tvItemNo") == "N"
		IF tvItemNo > 0
			lnPos = tvItemNo
		ENDIF
	OTHERWISE
ENDCASE

IF lnPos = -2
	RETURN
ENDIF

IF lnPos = -1
	DIMENSION goprogram.afavorites[1]
	goprogram.afavorites[1] = .F.
	goUser.Favorites = ""
ELSE
	lcBuffer = goprogram.afavorites[lnPos]
	ADEL(goprogram.afavorites, lnPos)
	IF ALEN(goprogram.afavorites, 1) = 1
		goprogram.afavorites[1] = .F.
		goUser.Favorites = ""
	ELSE
		DIMENSION goprogram.afavorites[alen(goProgram.aFavorites,1) -1]
		savefavorites()
	ENDIF
	IF TYPE("goProgram.oXPopen") == "O" AND !ISNULL(goProgram.oXPopen)
		goProgram.oXPopen.deleteitem(lcBuffer)
	ENDIF
ENDIF

*{ V&U MS 2008-10-14
IF goProgram.nMenuAndToolbarStyle > 0
	goProgram.oMenuBar.ReloadFavorites()
ENDIF
*} V&U MS 2008-10-14

ENDPROC
*-------------------------------------------------------
* Procedure...: restorefavorites()
* Called by...:
*
* Abstract....: Restore favorites from user variable GoUser.favorites.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE restorefavorites()
LOCAL lni, lcname, lxid, lcmenu, m.lcParentBarID, m.lctemppopup, m.lcbuffer, m.kk
IF TYPE("GoUser.favorites") == "U"
	RETURN
ENDIF
IF TYPE("goprogram.class") <>"C"
	RETURN
ENDIF
*{ V&U MS 2008-10-03
IF goProgram.nMenuAndToolbarStyle > 0
	RETURN
ENDIF
*} V&U MS 2008-10-03
*{ V&U RI 2008-12-10
IF !POPUP("favorites")
	RETURN .F.
ENDIF
*} V&U RI 2008-12-10
FOR lni = 1 TO ALEN(goprogram.afavorites)
	IF TYPE("goProgram.aFavorites[lni]") ="C"
		barid = VAL(getarg(goprogram.afavorites[lni], 6))
		RELEASE BAR barid OF favorites
		lcname = getarg(goprogram.afavorites[lni], 1)
		RELEASE POPUP (lcname)
	ENDIF
ENDFOR

DIMENSION goprogram.afavorites[1]
goprogram.afavorites[1] = .F.

IF !EMPTY(GoUser.favorites)
	FOR lni = 1 TO MEMLINES(GoUser.favorites)
		DIMENSION goprogram.afavorites[lni]
		goprogram.afavorites[lni] = MLINE(GoUser.favorites, lni)
	ENDFOR
	FOR lni = 1 TO MEMLINES(GoUser.favorites)
		lcname = JUSTSTEM(getarg(goprogram.afavorites[lni], 1))
		IF !POPUP(lcname)
			lxid = VAL(getarg(goprogram.afavorites[lni], 2))
			lcmenu = getarg(goprogram.afavorites[lni], 5)
			barid = VAL(getarg(goprogram.afavorites[lni], 6))
			DEFINE BAR barid OF favorites PROMPT (lcmenu) AFTER 3
			ON BAR barid OF favorites ACTIVATE POPUP (lcname)
			DEFINE POPUP (lcname) MARGIN RELATIVE COLOR SCHEME 4
			m.lcParentBarID = TRANSFORM(barid)
		ELSE
			m.lctemppopup = LOWER(lcname)
			FOR m.kk = 1 TO ALEN(goprogram.afavorites)
				m.lcBuffer = JUSTSTEM(LOWER(getarg(goprogram.afavorites[m.kk], 1)))
				IF lcBuffer == lctemppopup
					m.lcParentBarID = getarg(goprogram.afavorites[m.kk], 9)
					EXIT
				ENDIF
			ENDFOR
		ENDIF

		IF GetArgCount(goprogram.afavorites[lni]) = 6
			goprogram.afavorites[lni] = goprogram.afavorites[lni] + ";;;" + m.lcParentBarID
		ELSE
			goprogram.afavorites[lni] = goprogram.afavorites[lni] + ";" + m.lcParentBarID
		ENDIF

		DO CASE
			CASE CNTBAR(JUSTSTEM(getarg(goprogram.afavorites[lni], 1))) < 9
				lnbar    = VAL(getarg(goprogram.afavorites[lni], 6))
				lcbar   = JUSTSTEM(getarg(goprogram.afavorites[lni], 1))
				lcprompt = TRIM(getarg(goprogram.afavorites[lni], 3))
				DEFINE BAR lnbar OF (lcbar) PROMPT lcprompt
				ON SELECTION BAR lnbar OF (lcbar) gotofavorite(POPUP(), BAR(), .F., .T.)
			CASE CNTBAR(JUSTSTEM(getarg(goprogram.afavorites[lni], 1))) = 9
				lnbar    = INT(SECONDS() * 1000)
				lcbar   = JUSTSTEM(getarg(goprogram.afavorites[lni], 1))

*			Define "More Favorites ..." bar
				DEFINE BAR lnbar OF (lcbar) PROMPT IIF(goProgram.lRunTimeLocalization, goLocalize.cMEN_MOREFAVORITES, MEN_MOREFAVORITES)
				ON SELECTION BAR lnbar OF (lcbar) runmorefavorites(POPUP())
		ENDCASE
	ENDFOR
ENDIF

ENDPROC

*-------------------------------------------------------
* Procedure...: runmanagefavorites()
* Called by...:
*
* Abstract....: Run the manage favorites dialog.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE runmanagefavorites()
LOCAL loForm

*{HC MS 2016-02-27, 
IF SYS(17) = "X64" OR ;
	(TYPE("goProgram.lUseNativeVfpFavorites") == "L" AND goProgram.lUseNativeVfpFavorites)
	
	loForm	 = CREATEOBJECT("cVfpManageFavorites")
ELSE
	loForm	 = CREATEOBJECT("cManageFavorites")
ENDIF 
*}HC MS 2016-02-27	
loForm.SHOW()
RELEASE loForm
*{ V&U MS 2008-10-14
IF goProgram.nMenuAndToolbarStyle > 0
	goProgram.oMenuBar.ReloadFavorites()
ENDIF
*} V&U MS 2008-10-14
ENDPROC

*-------------------------------------------------------
* Procedure...: runmorefavorites()
* Called by...:
*
* Abstract....: Run the more favorites dialog.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE runmorefavorites()
LPARAMETERS lcpopup
LOCAL loform
loform	 = CREATEOBJECT("cMoreFavorites", lcpopup)
loform.SHOW()
RELEASE loform
ENDPROC

*-------------------------------------------------------
* Function....: ChangeDEDbc()
* Called by...:
*
* Abstract....: Used to change the database path in the dataenvironment of a form. Required, if you want to use the multi client database feature and there is a data folder present at the client side.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION changededbc(toform, tctodbc, tcfromdbc)
IF ISNULL(toform)  .OR.  TYPE("toForm") <> "O"
	RETURN .F.
ENDIF

IF VARTYPE(toform.DATAENVIRONMENT) = "X" .OR. TYPE("toForm.dataenvironment") <> "O"
	IF toform.DECLASS =""
		RETURN .F.
	ENDIF
ENDIF

IF ISNULL(tctodbc)  .OR.  EMPTY(tctodbc)  .OR.  TYPE("tcToDbc") <> "C"
	LOCAL lcdbc, llretval
	lcdbc    = ""

	IF !EMPTY(DBC())
		lcdbc = ALLTRIM(DBC())
	ELSE
		IF TYPE("goProgram.class") ="C" .AND. VARTYPE(goprogram) <> "X"
			lcdbc = ALLTRIM(goprogram.cdatadir) + "\" + ALLTRIM(goprogram.cmaindatabase)
		ENDIF
	ENDIF

	lcdbc = IIF(".DBC" $ UPPER(lcdbc), UPPER(lcdbc), UPPER(lcdbc) +".DBC")

	IF EMPTY(lcdbc)
		RETURN .F.
	ENDIF

	tctodbc = lcdbc
ENDIF

IF ISNULL(tcfromdbc)  .OR.  EMPTY(tcfromdbc)  .OR.  TYPE("tcFromDbc") <> "C"
	tcfromdbc = ""
ENDIF

IF EMPTY(JUSTPATH(tcfromdbc)) .AND. PCOUNT() = 3
	RETURN .F.
ENDIF

IF EMPTY(JUSTPATH(tctodbc))
	RETURN .F.
ENDIF

LOCAL lcfrompath, lctopath, lctmppath, lcvfxpath
lcfrompath = JUSTPATH(TRIM(tcfromdbc)) + "\"
lctopath   = JUSTPATH(TRIM(tctodbc))   + "\"
lcvfxpath  = ""
lctmppath  = ""

IF 	TYPE("goProgram.class") ="C" AND !ISNULL(goprogram.cvfxdir) AND !EMPTY(goprogram.cvfxdir)
	lcvfxpath = JUSTPATH(TRIM(goprogram.cvfxdir))  + "\"
	lcvfxpath = lcvfxpath + IIF(RIGHT(TRIM(goprogram.cvfxdir), 1) = "\", "", "\")
	IF LEFT(lcvfxpath, 2) = ".."
		lctmppath = FULLPATH("")
		CD..
		lcvfxpath = IIF(LEFT(lcvfxpath, 1) = "..\", SUBSTR(lcvfxpath, 4), SUBSTR(lcvfxpath, 3))
		lcvfxpath = FULLPATH(lcvfxpath)
		CD (lctmppath)
	ELSE
		lcvfxpath = FULLPATH(lcvfxpath)
	ENDIF
ENDIF

IF !EMPTY(lcfrompath)
	IF LEFT(lcfrompath, 2) = ".."
		lctmppath = FULLPATH("")
		CD..
		lcfrompath = IIF(LEFT(lcfrompath, 1) = "..\", SUBSTR(lcfrompath, 4), SUBSTR(lcfrompath, 3))
		lcfrompath = FULLPATH(lcfrompath)
		CD (lctmppath)

		IF DIRECTORY(lcfrompath)
			tcfromdbc = lcfrompath + JUSTFNAME(tcfromdbc)
		ELSE
			RETURN .F.
		ENDIF
	ELSE
		lcfrompath = FULLPATH(lcfrompath)
	ENDIF
ENDIF

IF !EMPTY(lctopath)
	IF LEFT(lctopath, 2) = ".."
		lctmppath = FULLPATH("")
		CD..
		lctopath = IIF(LEFT(lctopath, 1) = "..\", SUBSTR(lctopath, 4), SUBSTR(lctopath, 3))
		lctopath = FULLPATH(lctopath)
		CD (lctmppath)

		IF DIRECTORY(lctopath)
			tctodbc = lctopath + JUSTFNAME(tctodbc)
		ELSE
			RETURN .F.
		ENDIF
	ELSE
		lctopath = FULLPATH(lctopath)
	ENDIF
ENDIF

IF !EMPTY(tcfromdbc)
	tcfromdbc = UPPER(ALLTRIM(tcfromdbc))
	tcfromdbc = IIF(".DBC" $ tcfromdbc, tcfromdbc, tcfromdbc +".DBC")
ENDIF

tctodbc   = UPPER(ALLTRIM(tctodbc))
tctodbc   = IIF(".DBC" $ tctodbc, tctodbc, tctodbc +".DBC")

IF tcfromdbc == tctodbc
	RETURN .T.
ENDIF

LOCAL ARRAY lacursorobj(1)
lacursorobj = .NULL.

IF AMEMBERS(lacursorobj, IIF(toform.DECLASS ="", toform.DATAENVIRONMENT, toform.DECLASS), 2) > 0
	LOCAL i, locursorobj

	FOR i = 1  TO  ALEN(lacursorobj, 1)
		locursorobj = EVALUATE(IIF(toform.DECLASS ="","toform.DataEnvironment.", ;
			toform.DECLASS) + lacursorobj(i))
		IF TYPE("loCursorObj") == "O"
			IF UPPER(locursorobj.BASECLASS) == "CURSOR"
				IF !EMPTY(locursorobj.DATABASE)  && it's free table?
					IF EMPTY(tcfromdbc)
						locursorobj.DATABASE = lctopath + JUSTFNAME(tctodbc)
					ELSE
						IF UPPER(ALLTRIM(locursorobj.DATABASE)) == lcfrompath + JUSTFNAME(tcfromdbc)
							locursorobj.DATABASE = lctopath + JUSTFNAME(tctodbc)
						ENDIF
					ENDIF
				ELSE
					IF TYPE("goProgram.class") ="C" AND goprogram.isfreevfxtable(locursorobj.CURSORSOURCE)
						lctmppath = lcvfxpath
					ELSE
						lctmppath = lctopath
					ENDIF

					IF UPPER(TRIM(JUSTPATH(locursorobj.CURSORSOURCE))) + "\" <> UPPER(TRIM(lctmppath))
						LOCAL lccursorsource
						lccursorsource = ""
						lccursorsource = lccursorsource + TRIM(lctmppath)
						lccursorsource = lccursorsource + IIF(RIGHT(lccursorsource, 1) = "\", "", "\")
						lccursorsource = lccursorsource + TRIM(JUSTFNAME(locursorobj.CURSORSOURCE))

						locursorobj.CURSORSOURCE = lccursorsource
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDFOR

ENDIF

RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: CheckIfChgDEDbc()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: toform,tcdbc
*
* Notes.......:
*-------------------------------------------------------
FUNCTION checkifchgdedbc(toform, tcdbc)

IF ISNULL(toform)  .OR.  TYPE("toForm") <> "O"
	RETURN .F.
ENDIF

IF VARTYPE(toform.DATAENVIRONMENT) = "X" .OR.  TYPE("toForm.dataenvironment") <> "O"
	IF toform.DECLASS =""
		RETURN .F.
	ENDIF
ENDIF

IF ISNULL(tcdbc)  .OR.  EMPTY(tcdbc)  .OR.  TYPE("tcToDbc") <> "C"
	LOCAL lcdbc, llretval
	lcdbc    = ""

	IF !EMPTY(DBC())
		lcdbc = ALLTRIM(DBC())
	ELSE
		IF TYPE("goProgram.class") ="C" AND !(VARTYPE(goprogram) = "X")
			LOCAL lcdatapath, lctmppath
			lcdatapath = TRIM(goprogram.cdatadir) + "\"
			lctmppath = ""

			IF LEFT(lcdatapath, 2) = ".."
				lctmppath = FULLPATH("")
				CD..
				lcdatapath = IIF(LEFT(lcdatapath, 1) = "..\", SUBSTR(lcdatapath, 4), SUBSTR(lcdatapath, 3))
				lcdatapath = FULLPATH(lcdatapath)
				CD (lctmppath)

				IF !DIRECTORY(lcdatapath)
					lcdatapath = ""
				ENDIF
			ELSE
				lcdatapath = FULLPATH(lcdatapath)
			ENDIF

			IF !EMPTY(lcdatapath)
				lcdbc = lcdatapath + ALLTRIM(goprogram.cmaindatabase)
			ELSE
				lcdbc = ""
			ENDIF
		ENDIF
	ENDIF

	IF EMPTY(lcdbc)
		RETURN .F.
	ENDIF

	lcdbc = IIF(".DBC" $ UPPER(lcdbc), UPPER(lcdbc), UPPER(lcdbc) +".DBC")
	tcdbc = lcdbc
ENDIF

LOCAL ARRAY lacursorobj(1)
lacursorobj = .NULL.

IF AMEMBERS(lacursorobj, IIF(toform.DECLASS ="",  toform.DATAENVIRONMENT, ;
		toform.DECLASS), 2) > 0
	LOCAL i, locursorobj

	FOR i = 1  TO  ALEN(lacursorobj, 1)
		locursorobj = EVALUATE(IIF(toform.DECLASS ="","toForm.DataEnvironment.", ;
			toform.DECLASS) + lacursorobj(i))
		IF TYPE("loCursorObj") == "O"
			IF UPPER(locursorobj.BASECLASS) == "CURSOR"
				IF !EMPTY(locursorobj.DATABASE)
					IF UPPER(ALLTRIM(locursorobj.DATABASE)) <> tcdbc
						llretval = .T.
						EXIT
					ENDIF
				ELSE
					LOCAL lctmppath
					IF TYPE("goProgram.class") ="C" AND goprogram.isfreevfxtable(locursorobj.CURSORSOURCE)
						lctmppath = goprogram.cvfxdir
					ELSE
						lctmppath = FULLPATH(JUSTPATH(tcdbc))  + "\"
					ENDIF

					IF UPPER(FULLPATH(TRIM(JUSTPATH(locursorobj.CURSORSOURCE)))) + "\" <> UPPER(TRIM(lctmppath))
						llretval = .T.
						EXIT
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDFOR
ENDIF

RETURN llretval
ENDFUNC

*-------------------------------------------------------
* Function....: ResourceOn()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION RESOURCEON
LOCAL lcwindir
lcwindir = ""

lcwindir = STRTRAN(GETENV("windir") + "\", "\\", "\")
IF EMPTY(lcwindir)
	RETURN .F.
ENDIF

IF !FILE((lcwindir) + "foxuser.dbf")
	COPY FILE "_foxuser.dbf" TO (lcwindir) + "foxuser.dbf"
	COPY FILE "_foxuser.fpt" TO (lcwindir) + "foxuser.fpt"
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: clearsqlparameters()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcsql
*
* Notes.......:
*-------------------------------------------------------
FUNCTION clearsqlparameters
LPARAMETERS tcsql

LOCAL lcretvalue
lcretvalue = tcsql

LOCAL lnargcount, lcsymbol, lcvalue, lctext, k

lnargcount = OCCURS('?', tcsql)

IF lnargcount > 0
	LOCAL lnpos

	FOR lnpos = 1  TO  lnargcount
		lcsymbol = SUBSTR(tcsql, AT('?', tcsql, lnpos) + 1)

		lctext = ''
		FOR k = 1 TO LEN(lcsymbol)
			IF LOWER(SUBSTR(lcsymbol, k, 1)) $ "abcdefghijklmnopqrstuvwxyz0123456789_"
				lctext = lctext + SUBSTR(lcsymbol, k, 1)
			ELSE
				EXIT
			ENDIF
		NEXT

		IF !EMPTY(lctext)
			lcretvalue = STRTRAN(lcretvalue, "?" + ALLTRIM(lctext), "null")
		ENDIF
	NEXT
ENDIF

RETURN lcretvalue

*-------------------------------------------------------
* Function....: vfxsqlconnect()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcdsn
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfxsqlconnect
LPARAMETERS tcdsn
IF EMPTY(NVL(tcdsn,""))
	tcdsn = .NULL.
ENDIF
RETURN IIF(EMPTY(NVL(tcdsn,"")), -1, SQLCONNECT(tcdsn, "", ""))

*-------------------------------------------------------
* Function....: vfxsqlexec()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tnconnection, tcsql, tccursor, tlnodisperror, tlRetryOnDeadlock
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfxsqlexec
LPARAMETERS tnconnection, tcsql, tccursor, tlnodisperror, tlRetryOnDeadlock
SET MESSAGE TO TRIM(LEFT(tcsql, 255))
LOCAL lnok
IF !EMPTY(tccursor)
	lnok = SQLEXEC(tnconnection, tcsql, tccursor)
ELSE
	lnok = SQLEXEC(tnconnection, tcsql)
ENDIF
IF VARTYPE(lnok) <> "N" OR lnok <= 0
	LOCAL laerror[1,7]
	AERROR(laerror)

	IF !tlnodisperror
		DO CASE
			CASE tlRetryOnDeadlock AND laerror[1,5] = 1205 AND lnloopcount < 10
				LOOP

			CASE laerror[1,5] = 8115
				IF TYPE("goProgram.class") ="C"
					 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SERVERERROR8115, MSG_SERVERERROR8115), ;
						16, IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ERRORWRITINGTOSERVER, MSG_ERRORWRITINGTOSERVER))
				ENDIF
			OTHERWISE
				IF TYPE("goProgram.class") ="C"
					 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_SERVERERROR, MSG_SERVERERROR) + ;
						CHR(13) + CHR(13) + ;
						laerror[1,3] + CHR(13) + CHR(13) + ;
						IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CONTACTADMIN, MSG_CONTACTADMIN), 16, ;
						IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ERRORWRITINGTOSERVER, MSG_ERRORWRITINGTOSERVER))
				ENDIF
		ENDCASE
	ENDIF

	PUBLIC glastvfxsqlexecerror
	glastvfxsqlexecerror = laerror[1,3]

ENDIF
SET MESSAGE TO
RETURN IIF(VARTYPE(lnok) ="N", lnok, -1)
ENDFUNC

DEFINE CLASS cconnectionmgr AS CUSTOM
	HIDDEN aconnections[1]
	HIDDEN cdsnname, cuserid, cpassword, cconnectionname, cConnectionstring, cDBDriver
	HIDDEN nDataSourcePlatformType, nDataSourceType
	HIDDEN lNoShowConnectionDialog

	PROCEDURE INIT(tcconnectionname, tcdsnname, tcuserid, tcpassword, tnDataSourceType)

	WITH THIS
		.aconnections[1] = -1

		.cconnectionname = IIF(EMPTY(NVL(tcconnectionname,"")), .NULL., tcconnectionname)

		.cdsnname	 = IIF(EMPTY(NVL(tcdsnname,"")), .NULL., tcdsnname)
		.cuserid	 = IIF(EMPTY(NVL(tcuserid,"")), .NULL., tcuserid)
		.cpassword	 = IIF(EMPTY(NVL(tcpassword,"")), .NULL., tcpassword)
		.nDataSourceType = IIF(EMPTY(NVL(tnDataSourceType,"")), 0, tnDataSourceType)

		.nDataSourcePlatformType = 0
		.lNoShowConnectionDialog = .T.
		.cDBDriver = ""
	ENDWITH
	ENDPROC

	PROTECTED FUNCTION createnewconnection()
		LOCAL lnsqlconnection, lnOldDispLogin AS INTEGER
		LOCAL loConnDataSource AS OBJECT
		LOCAL loException AS EXCEPTION
		LOCAL lcConnString, lcUID, lcPWD, lcDatabase AS STRING

*JEI VM 14.05.07(moved from FUNCTION getconnection()
		IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
			lcMessageText = IIF(goProgram.lRuntimeLocalization, ;
				IIF(TYPE("goLocalize") = "O", goLocalize.cMSG_WAITINGCONNECTION, "Waiting Connection ..."), ;
				MSG_WAITINGCONNECTION)
			goProgram.vfxwaitwindow(lcMessageText, .F., .F., .T., .F., .T.)
		ELSE
			WAIT WINDOW "Waiting Connection ..." NOWAIT NOCLEAR
		ENDIF
*JEI VM 14.05.07)

		lnsqlconnection = -1

		IF THIS.nDataSourceType = 1 && Use ADO
			loConnDataSource = .NULL.
			TRY
				loConnDataSource = CREATEOBJECT('ADODB.Connection')
			CATCH TO loException
				loConnDataSource = .NULL.
			ENDTRY

			IF !ISNULL(loConnDataSource)
				DO CASE
					CASE !EMPTY(NVL(THIS.cConnectionstring,""))
						loConnDataSource.ConnectionString = THIS.cConnectionstring

					CASE !EMPTY(NVL(THIS.cconnectionname,""))
						lcConnString = ""
						TRY
							lcConnString = DBGETPROP(THIS.cconnectionname,"CONNECTION","ConnectString")
							IF EMPTY(lcConnString)
								lcConnString = DBGETPROP(THIS.cconnectionname,"CONNECTION","DataSource")
								IF !EMPTY(lcConnString)
									lcUID = DBGETPROP(THIS.cconnectionname,"CONNECTION","UserId")
									lcPWD = DBGETPROP(THIS.cconnectionname,"CONNECTION","PassWord")
									lcDatabase = DBGETPROP(THIS.cconnectionname,"CONNECTION","Database")
									lcConnString = "DSN=" + lcConnString + ;
										IIF(!EMPTY(lcDatabase), ";DATABASE=" + lcDatabase, "") + ;
										IIF(!EMPTY(lcUID), ";UID=" + lcUID,"") + ;
										IIF(!EMPTY(lcPWD),";PWD=" + lcPWD,"")
								ENDIF
							ENDIF
						CATCH TO loException
							lcConnString = ""
						ENDTRY
						loConnDataSource.ConnectionString = lcConnString
				ENDCASE

				TRY
					loConnDataSource.OPEN()
					IF TYPE("glError1429Occurred") == "L"
						glError1429Occurred = .F.
					ENDIF
				CATCH TO loException
					IF loException.ERRORNO = 1429 AND ;
							TYPE("glError1429Occurred") == "L"		&& connection settings wrong
						glError1429Occurred = .T.
					ENDIF
					loConnDataSource = .NULL.
				ENDTRY


				IF !ISNULL(loConnDataSource) AND loConnDataSource.State = 1
					THIS.ADDADOCONNECTION(loConnDataSource)
				ELSE
					loConnDataSource = .NULL.
				ENDIF
			ENDIF

*JEI VM 14.05.07(
			IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
				goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
			ELSE
				WAIT CLEAR
			ENDIF
*JEI VM 14.05.07)

			RETURN loConnDataSource
		ELSE
			IF THIS.lNoShowConnectionDialog
				lnOldDispLogin = SQLGETPROP(0,"DispLogin")
				SQLSETPROP(0,"DispLogin", 3) && 3 ODBC Login dialog box isn't displayed and Visual FoxPro generates an
&& error if the required login information isn't available.
			ENDIF

			DO CASE
				CASE !EMPTY(NVL(THIS.cConnectionstring,""))
					lnsqlconnection = SQLSTRINGCONNECT(THIS.cConnectionstring)
				CASE !EMPTY(NVL(THIS.cconnectionname,""))
					lnsqlconnection = SQLCONNECT(THIS.cconnectionname)
				CASE !EMPTY(NVL(THIS.cdsnname,"")) AND TYPE("this.cUserId") = "C" AND TYPE("this.cPassword") = "C"
					lnsqlconnection = SQLCONNECT(THIS.cdsnname, THIS.cuserid, THIS.cpassword)
			ENDCASE

			IF THIS.lNoShowConnectionDialog
				SQLSETPROP(0,"DispLogin", lnOldDispLogin)
			ENDIF

			IF lnsqlconnection > 0
				THIS.ADD(lnsqlconnection)
				IF TYPE("glError1429Occurred") == "L"
					glError1429Occurred = .F.
				ENDIF
			ELSE
				IF TYPE("glError1429Occurred") == "L"
					glError1429Occurred = .T.
				ENDIF
			ENDIF

*JEI VM 14.05.07(
			IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
				goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
			ELSE
				WAIT CLEAR
			ENDIF
*JEI VM 14.05.07)

			RETURN lnsqlconnection
		ENDIF

		ENDFUNC

*---------------------
* Get a free SQL Connection
	FUNCTION getconnection()
	LOCAL lnconnectioncount, lnconn
	lnconnectioncount = ALEN(THIS.aconnections, 1)

	LOCAL lnsqlconnectionl
	LOCAL lcMessageText AS STRING
	lnsqlconnection = -1

	IF THIS.nDataSourceType = 0
* Look for a free SQL Connection
		FOR lnconn = 1 TO lnconnectioncount
			IF NVL(THIS.aconnections[lnConn], 0) > 0 AND !SQLGETPROP(THIS.aconnections[lnConn], "ConnectBusy")
				lnsqlconnection = THIS.aconnections[lnConn]
				EXIT
			ENDIF
		NEXT

		IF (lnsqlconnection = -1)
			lnsqlconnection = THIS.createnewconnection()
		ENDIF

		RETURN lnsqlconnection
	ELSE
		LOCAL loADODBConnection
		loADODBConnection = .NULL.
		FOR lnconn = 1 TO lnconnectioncount
			IF TYPE("NVL(THIS.aconnections[lnConn],'')") = "O" AND THIS.aconnections[lnConn].State = 1
				loADODBConnection = THIS.aconnections[lnConn]
				EXIT
			ENDIF
		ENDFOR

		IF ISNULL(loADODBConnection)
			loADODBConnection = THIS.CreateNewConnection()
		ENDIF

*JEI VM 20080829{
		IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
			goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
		ELSE
*JEI VM 20080829}
			WAIT CLEAR
		ENDIF

		RETURN loADODBConnection
	ENDIF
	ENDFUNC

	FUNCTION ADD(tnsqlconnection)
	LOCAL lnconnectioncount
	lnconnectioncount = 0

	IF (tnsqlconnection > 0)
* Connection is valid

		IF (THIS.aconnections[1] <> -1)
			lnconnectioncount = ALEN(THIS.aconnections, 1)
		ENDIF

		lnconnectioncount = lnconnectioncount + 1
		DIMENSION THIS.aconnections[lnConnectionCount]
		THIS.aconnections[lnConnectionCount] = tnsqlconnection
		IF lnconnectioncount = 1
			THIS.nDataSourcePlatformType = GetDataSourcePlatform(tnsqlconnection)
		ENDIF
	ENDIF

	RETURN lnconnectioncount
	ENDFUNC

	FUNCTION ADDADOCONNECTION(toConnDataSource)
	LOCAL lnconnectioncount AS INTEGER

	lnconnectioncount = 0

	IF TYPE("toConnDataSource") = "O" AND !ISNULL(toConnDataSource)
		IF TYPE("THIS.aconnections[1]") = "O" AND !ISNULL(THIS.aconnections[1])
			lnconnectioncount = ALEN(THIS.aconnections, 1)
		ENDIF

		lnconnectioncount = lnconnectioncount + 1
		DIMENSION THIS.aconnections[lnConnectionCount]
		THIS.aconnections[lnConnectionCount] = toConnDataSource
		IF lnconnectioncount = 1
			THIS.nDataSourcePlatformType = GetDataSourcePlatform(-1, toConnDataSource.ConnectionString)
		ENDIF
	ENDIF

	RETURN lnconnectioncount
	ENDFUNC

	PROCEDURE FREE()
	LOCAL lnconnectioncount, lnconn
	lnconnectioncount = ALEN(THIS.aconnections, 1)

	LOCAL lcOldOnError, llErr
	lcOldOnError = ON("error")
	llErr = .F.
	ON ERROR llErr = .T.

	IF THIS.nDataSourceType = 1 && Use ADO
		FOR lnconn = 1 TO lnconnectioncount
			IF TYPE("NVL(THIS.aconnections[lnConn],'')") = "O"
				THIS.aconnections[lnConn].CLOSE()
				THIS.aconnections[lnConn] = .NULL.
			ENDIF
		ENDFOR
	ELSE
		FOR lnconn = 1 TO lnconnectioncount
			IF NVL(THIS.aconnections[lnConn], 0) >= 1
				SQLDISCONNECT(THIS.aconnections[lnConn])
				THIS.aconnections[lnConn] = -1
			ENDIF
		NEXT
	ENDIF

	DIMENSION THIS.aconnections[1]
	THIS.aconnections[1] = .NULL.
	
	*{V&U MS 2011-09-16, Set nDataSourcePlatformType to 0
	This.nDataSourcePlatformType = 0
	*}V&U MS 2011-09-16
	
	ON ERROR &lcOldOnError
	ENDPROC


*---------------------
* Set ConnectionName
	PROCEDURE setconnectionname(tcconnectionname)
	THIS.cconnectionname = tcconnectionname
	ENDPROC

*---------------------
* Set DSN Name
	PROCEDURE setdsnname(tcdsnname)
	THIS.cdsnname	 = tcdsnname
*{ JEI IG 2006.10.17 Add
	IF TYPE("THIS.cdsnname") == "C" AND !EMPTY(THIS.cdsnname)
		LOCAL lnRetVal, lnODBCEnv, dsn, dsndesc, mdsn, mdesc, lcCurrentDSN
		*{ HC BB 2016-02-29, 6924
		DECLARE short SQLDataSources IN odbc32 ;
			LONG    henv, INTEGER fDir, STRING @ DSN, ;
			INTEGER DSNMax, INTEGER @pcbDSN, STRING @DESCRIPTION, ;
			INTEGER DescMax, INTEGER @desclen
		*} HC BB 2015-02-29
		
		lnRetVal = 0
		lnODBCEnv = VAL(SYS(3053))
		DO WHILE lnRetVal = 0 && SUCCESS
			dsn = SPACE(100)
			dsndesc = SPACE(100)
			mdsn = 0
			mdesc = 0
			lnRetVal = sqldatasources(lnODBCEnv, 1, ; &&SQL_FETCH_NEXT
			@dsn, 100, @mdsn, @dsndesc, 100, @mdesc)

			IF lnRetVal = 0 &&if no error occurred
				lcCurrentDSN = LEFT(dsn, AT(CHR(0), dsn) -1)
				IF UPPER(ALLTRIM(THIS.cdsnname)) == UPPER(ALLTRIM(lcCurrentDSN))
					THIS.cDBDriver = UPPER(ALLTRIM(LEFT(dsndesc, AT(CHR(0), dsndesc) -1)))
					EXIT
				ENDIF
			ENDIF
		ENDDO
	ENDIF
*} JEI IG 2006.10.17 Add
	ENDPROC

*---------------------
* Set Connectionstring
	PROCEDURE setConnectionstring(tcConnectionstring)
	THIS.cConnectionstring	 = tcConnectionstring
*{ JEI IG 2006.10.17 Add
	LOCAL lcDBDriver, lnPos1, lnPos2
	lcDBDriver = tcConnectionstring
	IF TYPE("lcDBDriver") != "C"
&& probably working with local DBC
		lcDBDriver = ""
	ENDIF
	lnPos1 = AT("=", lcDBDriver, 1)
	lnPos2 = AT(";", lcDBDriver, 1)
	IF lnPos2 = 0
		lnPos2 = LEN(lcDBDriver)
	ENDIF
	IF lnPos1 > 0
		lcDBDriver = SUBSTR(lcDBDriver, lnPos1 + 1, lnPos2 -1 - lnPos1)
	ELSE
		lcDBDriver = ""
	ENDIF
	THIS.cDBDriver = lcDBDriver
*} JEI IG 2006.10.17
	ENDPROC

*---------------------
* Set UserId
	PROCEDURE setuserid(tcuserid)
	THIS.cuserid	 = tcuserid
	ENDPROC

*---------------------
* Set Password
	PROCEDURE setpassword(tcpassword)
	THIS.cpassword	 = tcpassword
	ENDPROC

*---------------------
* Return ConnectionName
	FUNCTION getconnectionname()
	RETURN THIS.cconnectionname
	ENDPROC

*---------------------
* Return DSN Name
	FUNCTION getdsnname()
	RETURN THIS.cdsnname
	ENDFUNC

*---------------------
* Return Connectionstring
	FUNCTION getConnectionstring()
	RETURN THIS.cConnectionstring
	ENDFUNC

*---------------------
* Return Connectionstring
	FUNCTION getConnectionplatform()
	RETURN THIS.cDBDriver
	ENDFUNC

*---------------------
* Return UserId
	FUNCTION getuserid()
	RETURN THIS.cuserid
	ENDFUNC

*---------------------
* Return Password
	FUNCTION getpassword()
	RETURN THIS.cpassword
	ENDFUNC

*---------------------
* Get DataSource Platform Type
	FUNCTION GetDataSourcePlatformType()
	RETURN THIS.nDataSourcePlatformType
	ENDFUNC

*---------------------
* Set DataSourceType
	FUNCTION SetDataSourceType(tnDataSourceType)
	THIS.nDataSourceType = tnDataSourceType
	ENDFUNC

*---------------------
* Get DataSourceType
	FUNCTION GetDataSourceType()
	RETURN THIS.nDataSourceType
	ENDFUNC
*--------------------
* Set NoShowConnectionDialog
	FUNCTION SetNoShowConnectionDialog(tlNoShowConnectionDialog)
	THIS.lNoShowConnectionDialog = tlNoShowConnectionDialog
	ENDFUNC
*--------------------
* Get NoShowConnectionDialog
	FUNCTION GetNoShowConnectionDialog(tlNoShowConnectionDialog)
	RETURN THIS.lNoShowConnectionDialog
	ENDFUNC
*--------------------
* Release this
	PROCEDURE RELEASE()
	RELEASE THIS
	ENDPROC

*---------------------
* Clean up
	PROCEDURE DESTROY()
* Release all SQL Connections
	THIS.FREE()
	ENDPROC
ENDDEFINE

*-------------------------------------------------------
* Procedure...: idsynch()
* Called by...:
*
* Abstract....: synchronize vfxsysid in dbf environment. C/S Version see below
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE idsynch
LOCAL lcvfxsysidalias, lctablealias, lctablename, lnmaxid
LOCAL lcOldOnError, llerror, lnprimarykey
lcOldOnError = ON("error")
ON ERROR llerror = .T.

lcvfxsysidalias = "VFXSYSID" + SYS(2015)

IF TYPE("goProgram.class") ="C" AND goProgram.nvfxSysTableLoc >= 1
	*{V&U MS 2010-05-13, 2014-04-28
	oSysIdAdptr = OPENTABLE("vfxsysid", "IDVFXSYSID", .F., ;
							"INDEX ON UPPER(keyname) TAG KEY ADDITIVE", .T., , ;
							lcvfxsysidalias, "cAppVFXDataAccess", .F., .F., .F., .F., "VALUE")
	*}V&U MS 2010-05-13, 2014-04-28		
ELSE
	USE vfxsysid IN 0 ALIAS (lcvfxsysidalias) EXCLUSIVE
ENDIF

SELECT (lcvfxsysidalias)

ON ERROR &lcOldOnError

IF (llerror)
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_VFXSYSIDNOTEXCL, MSG_VFXSYSIDNOTEXCL), 64, ;
			IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_REFRESHID, MSG_REFRESHID))
	ENDIF
ELSE
	LOCAL lcolddeleted
	lcolddeleted = SET("DELETED")
	SET DELETED OFF

	SCAN
		lctablealias = ALLTRIM(keyname) + SYS(2015)
		lctablename = ALLTRIM(keyname)
		SET MESSAGE TO lctablename

		IF FILE(lctablename +".dbf")
			USE (lctablename) IN 0 ALIAS (lctablealias) AGAIN SHARED

			SELECT (lctablealias)

			lnprimarykey = getpknum()

			IF !EMPTY(lnprimarykey)
				SET ORDER TO lnprimarykey DESCENDING
				LOCATE
				lnmaxid = EVALUATE(FIELD(lnprimarykey))

				SELECT (lcvfxsysidalias)
				IF VAL(VALUE) < lnmaxid
					REPLACE VALUE WITH PADL(TRANSFORM(lnmaxid), MAXLEN,"0")
				ENDIF
			ENDIF

			USE IN (lctablealias)

		ENDIF
	ENDSCAN

	SET DELETED &lcolddeleted
	SET MESSAGE TO
ENDIF

IF USED(lcvfxsysidalias)
	IF CURSORGETPROP("Buffering") > 1
		TABLEUPDATE()
	ENDIF
	USE IN (lcvfxsysidalias)
	RELEASE oSysIdAdptr
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_IDSYNCHCOMPLETED, MSG_IDSYNCHCOMPLETED), 64, ;
			_SCREEN.CAPTION)
	ENDIF
ENDIF

RETURN

*-------------------------------------------------------
* Function....: idsynchcs()
* Called by...:
*
* Abstract....: synchronize vfxsysid in c/s environment. dbf Version see above
*
* Returns.....:
*
* Parameters..: tcdsn
*
* Notes.......:
*-------------------------------------------------------
FUNCTION idsynchcs

IF TYPE("goProgram.class") ="C" AND goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ASKSYNCH, MSG_ASKSYNCH), 36, _SCREEN.CAPTION) <> idyes
	RETURN
ENDIF

LOCAL lnselect, lcdecimals, lnconnection, lcsql, lnok, llok, lcOldSetPoint

lcOldSetPoint = SET("Point")
SET POINT TO "."

llok = .T.
lnconnection = vfxsqlconnect()
IF lnconnection < 0
	IF TYPE("goProgram.class") ="C"
		 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_NOCONNECTION, MSG_NOCONNECTION), 16, _SCREEN.CAPTION)
	ENDIF
	SET POINT TO &lcOldSetPoint
	RETURN .F.
ELSE
* open vfxsysid
	lnselect = SELECT()
	SELECT 0
	USE vfxsysid ALIAS _vfxsysid EXCL AGAIN

	SQLDISCONNECT(lnconnection)
	USE IN _vfxsysid
	SELECT (lnselect)
	IF TYPE("goProgram.class") ="C"
		 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_IDSYNCHCOMPLETED, MSG_IDSYNCHCOMPLETED), 64, ;
			_SCREEN.CAPTION)
	ENDIF
ENDIF
SET POINT TO &lcOldSetPoint
RETURN .T.

*-------------------------------------------------------
* Function....: idsynchwork()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tnconnection, tckeyname, tctablename, tckeyfieldname
*
* Notes.......:
*-------------------------------------------------------
FUNCTION idsynchwork
LPARAMETERS tnconnection, tckeyname, tctablename, tckeyfieldname
lcsql = "select max(" + tckeyfieldname + ") _max from " + tctablename
lnok = vfxsqlexec(tnconnection, lcsql, "__Cursor")
IF lnok <= 0
	llok = .F.
ELSE
	SELECT _vfxsysid
	LOCATE FOR keyname = PADR(tckeyname, LEN(keyname))
	IF !FOUND()
		APPEND BLANK
		REPLACE keyname WITH tckeyname
	ENDIF
	IF INT(VAL(_vfxsysid.VALUE)) <> NVL(__cursor._max, 0)
		IF !EMPTY(NVL(__cursor._max, 0))
			REPLACE VALUE WITH RIGHT("0000000000" + ALLTRIM(STR(__cursor._max, 10, 0)), 10)
		ELSE
			REPLACE VALUE WITH "0000000000"
		ENDIF
	ENDIF
	USE IN "__Cursor"
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: getparentform()
* Called by...:
*
* Abstract....:	Get the parent form
*
* Returns.....:
*
* Parameters..: tocontrol, tcObjectHierarchy
*
* Notes.......: tcObjectHierarchy - passed by ref
*-------------------------------------------------------
FUNCTION getparentform(tocontrol, tcObjectHierarchy)
LOCAL loparentform, lcobjname, lcformname
loparentform = .NULL.
lcobjname = tocontrol.NAME

DO WHILE TYPE("toControl.parent") # "U"

	IF LOWER(tocontrol.PARENT.BASECLASS) = "form"
		loparentform = tocontrol.PARENT
		EXIT
	ENDIF

* walk up
	tocontrol = tocontrol.PARENT
	lcobjname = tocontrol.NAME + "." + lcobjname
ENDDO

* Get object hierarchy
IF LOWER(loparentform.BASECLASS) = 'form'
	IF VARTYPE(loparentform.cformname) ="C"
		lcformname	 = UPPER(loparentform.cformname)
	ELSE
		lcformname	 = UPPER(loparentform.NAME)
	ENDIF
ELSE
	lcformname	 = UPPER(loparentform.NAME)
ENDIF
lcobjname	 = UPPER(lcformname + "." + lcobjname)
tcObjectHierarchy = lcobjname

RETURN loparentform
ENDFUNC

*-------------------------------------------------------
* Function....: collectoledata()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcObj
*
* Notes.......:
*-------------------------------------------------------
FUNCTION collectoledata(tcObj)
LOCAL lnZ, lnY, lcZ
lcZ =""
lnZ = 0
IF LOWER(tcObj.BASECLASS) ="pageframe"	&& activepage!
	lnZ = m.lnZ + 1
	LOCAL laValues[lnZ,2]
	laValues[lnZ,1] = tcObj.TABINDEX
	laValues[lnZ,2] = collectoledata(tcObj.PAGES[tcObj.activepage])
ELSE
	FOR EACH oObj IN tcObj.CONTROLS
		IF PEMSTATUS(oObj,"value", 5)
			IF PEMSTATUS(oObj,"tabindex", 5)
				lnZ = m.lnZ + 1
				LOCAL laValues[lnZ,2]
				laValues[lnZ,1] = oObj.TABINDEX
				laValues[lnZ,2] = TRANSFORM(oObj.VALUE)
			ENDIF
		ENDIF
		IF LOWER(oObj.BASECLASS) ="container"
			lnZ = m.lnZ + 1
			LOCAL laValues[lnZ,2]
			laValues[lnZ,1] = oObj.TABINDEX
			laValues[lnZ,2] = collectoledata(oObj)
		ENDIF
	ENDFOR
ENDIF
IF m.lnZ > 0
	 = ASORT(laValues, 1)
	FOR lnY = 1 TO m.lnZ
		lcZ = m.lcZ + laValues[m.lnY,2] + CHR(13)
	NEXT
ENDIF
RETURN m.lcZ

*-------------------------------------------------------
* Function....: getactivepage()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: toPageframe
*
* Notes.......:
*-------------------------------------------------------
FUNCTION getactivepage
LPARAMETERS toPageframe
LOCAL loPage
FOR EACH oPage IN toPageframe.PAGES
	IF oPage.PAGEORDER = toPageframe.ACTIVEPAGE
		loPage = oPage
		EXIT
	ENDIF
ENDFOR
RETURN loPage

*-------------------------------------------------------
* Function....: previewon
* Called by...:
*
* Abstract....: Store the position of the preview toolbar
*
* Notes.......:
*-------------------------------------------------------
FUNCTION previewon
IF VERSION(2) = 2 OR DRIVETYPE(SYS(5)) = 5
	RETURN .T.
ENDIF
LOCAL lcTempresource, llUse, lnselect
IF TYPE("goprogram.class") <>"C"
	RETURN
ENDIF
lnselect = SELECT()
lcTempresource = "X" + SUBSTR(SYS(2015), 4, 7)
CREATE TABLE (ADDBS(SYS(2023)) + lcTempresource) FREE;
	(TYPE c(12), ID c(12), NAME m, READONLY l, ckval N(6, 0), DATA m, UPDATED d)
IF USED("vfxres")
	llUse = .F.
ELSE
	llUse = .T.
	IF goProgram.nvfxSysTableLoc >= 1
		*{V&U MS 2010-05-13, 2014-04-28
		oResAdptr = OPENTABLE("vfxRes", "IDVFXRES", .F., ;
			"INDEX ON UPPER(user)+UPPER(objname) TAG USER ADDITIVE COLLATE [MACHINE]", ;
			.T., , "vfxRes", "cAppVFXDataAccess", .F., .F., .F., .F., "INDEX,USER")
		*}V&U MS 2010-05-13	, 2014-04-28
	ELSE
		USE (ADDBS(goprogram.cvfxdir) + "vfxres") SHARED AGAIN IN 0
	ENDIF
ENDIF
IF SEEK(UPPER(GoUser.USER) +"VFX_PREVIEW_TOOLBAR","vfxres","user")
	INSERT INTO (lcTempresource) VALUES (;
		SUBSTR(vfxres.objname, 20, 12), ;
		SUBSTR(vfxres.objname, 32, 12), ;
		vfxres.INDEX, ;
		vfxres.DESCENDING, ;
		VAL(SUBSTR(vfxres.objname, 44, 6)), ;
		vfxres.layout, ;
		DATE())
ENDIF
IF llUse
	IF CURSORGETPROP("Buffering") > 1
		TABLEUPDATE()
	ENDIF
	USE IN vfxres
	RELEASE oResAdptr
ENDIF
USE IN (lcTempresource)
SET RESOURCE TO (ADDBS(SYS(2023)) + lcTempresource)
SELECT(lnselect)
RETURN .T.

*-------------------------------------------------------
* Function....: previewoff
* Called by...:
*
* Abstract....: Store the position of the preview toolbar
*
* Notes.......:
*-------------------------------------------------------
FUNCTION previewoff
IF VERSION(2) = 2 OR DRIVETYPE(SYS(5)) = 5
	RETURN .T.
ENDIF
LOCAL lcTempresource, lnselect

IF TYPE("goProgram.class") <>"C"
	RETURN
ENDIF

lnselect = SELECT()
lcTempresource = SYS(2005)
SET RESOURCE OFF
USE (lcTempresource) IN 0 ALIAS tempresource
SELECT tempresource
LOCATE FOR ID ="TTOOLBAR    "
IF FOUND()
	IF USED("vfxres")
		llUse = .F.
	ELSE
		llUse = .T.
		IF goProgram.nvfxSysTableLoc >= 1
			*{V&U MS 2010-05-13, 2014-04-28
			oResAdptr = OPENTABLE("vfxRes", "IDVFXRES", .F., ;
				"INDEX ON UPPER(user)+UPPER(objname) TAG USER ADDITIVE COLLATE [MACHINE]", ;
				.T., ,"vfxRes","cAppVFXDataAccess", .F., .F., .F., .F., "INDEX,USER")
			*}V&U MS 2010-05-13, 2014-04-28
		ELSE
			USE (ADDBS(goprogram.cvfxdir) + "vfxres") SHARED AGAIN IN 0
		ENDIF
	ENDIF
	IF !SEEK(UPPER(GoUser.USER) +"VFX_PREVIEW_TOOLBAR","vfxres","user")
		SELECT vfxres
		APPEND BLANK
	ENDIF
	REPLACE USER WITH GoUser.USER, ;
		objname WITH "VFX_PREVIEW_TOOLBAR" + tempresource.TYPE + tempresource.ID + STR(tempresource.ckval, 6), ;
		INDEX WITH tempresource.NAME, ;
		DESCENDING WITH tempresource.READONLY, ;
		layout WITH tempresource.DATA IN vfxres
	IF llUse
		IF CURSORGETPROP("Buffering") > 1
			TABLEUPDATE()
		ENDIF
		USE IN vfxres
		RELEASE oResAdptr
	ENDIF
ENDIF
USE IN tempresource
lcTempresource = LEFT(lcTempresource, LEN(lcTempresource) -4)
DELETE FILE (lcTempresource +".dbf")
DELETE FILE (lcTempresource +".fpt")
SELECT(lnselect)
RETURN .T.

*-------------------------------------------------------
* Function....: DelDirectory
* Called by...:
*
* Abstract....: Delete Directory
*
* Notes.......:
*-------------------------------------------------------
FUNCTION DelDirectory(tcDIR)

LOCAL llResult, loError AS EXCEPTION
LOCAL lnAttemptNum AS INTEGER, lnNumDelFiles, lnJ
LOCAL ARRAY laDelFilesName[1]

TRY
	llResult = .F.
* 1
	IF DIRECTORY(tcDIR, 1)
* 2
		*{HC MS 2015-06-30, Make DelDirectory recursive
		lnNumDelFiles = ADIR(laDelFilesName, ADDBS(tcDIR) + "*.*", "D")
* 3
		FOR lnJ = 3 TO lnNumDelFiles	&& Skip "\." and "\.."
			IF DIRECTORY(ADDBS(tcDIR) + laDelFilesName[lnJ, 1], 1)
				DelDirectory(ADDBS(tcDIR) + laDelFilesName[lnJ, 1])
			ELSE 	
				ERASE (ADDBS(tcDIR) + laDelFilesName[lnJ, 1])
			ENDIF
		NEXT
		*}HC MS 2015-06-30	
		
		lnAttemptNum = 10
		DO WHILE lnAttemptNum > 0
&& try several times, because of timing problems (The directory is not empty.)
			TRY
* 4
				RD (tcDIR)
				lnAttemptNum = 0
			CATCH TO loError
				lnAttemptNum = lnAttemptNum - 1
			ENDTRY
		ENDDO
		RELEASE laDelFilesName
		llResult = .T.
	ENDIF
CATCH TO loError
	llResult = .F.
	onerror(loError.ERRORNO, PROGRAM(), loError.LINENO, loError.MESSAGE, loError.LINECONTENTS)
ENDTRY

RETURN llResult

ENDFUNC

*-------------------------------------------------------
* Function....: SQLDatabaseUpdate
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION SQLDatabaseUpdate(ofoxapp, tlNewVersion, tlDoNotRunVfxUpdate, taUpdatedFreeFolders)

** Update database structure
LOCAL 	oError AS EXCEPTION
LOCAL 	lcvfxpath, lcDataDir, lnOldDispLogin, lcMetadataTableName, lcDBConnType, lcConnectionString, lcFieldName AS STRING
LOCAL 	llWorkWithConfig, llNoDataDictTables, llErrorOccur AS Logical
LOCAL 	lnClientCount, lnCurrentClient, lnFieldsCount, lnVFXSysTablesLocation, lnFieldCount, ;
	lnCurrentField AS INTEGER, lnreccnt AS INTEGER

lcvfxpath = ALLTRIM(ofoxapp.cvfxpath)
lcDataDir = ADDBS(ALLTRIM(ofoxapp.cdatadir))
llNoDataDictTables = .F.

*{V&U MS 2009-01-12

* If no work with config - no SQL is used
llWorkWithConfig = !EMPTY(oFoxapp.cConfigPath) AND FILE(oFoxApp.cConfigPath)
IF !llWorkWithConfig
	RETURN .T.
ENDIF
*}V&U MS 2009-01-12

TRY
	llRes = .T.
	IF EMPTY(ofoxapp.cMetaPathDir)
		IF !EMPTY(ofoxapp.cdatadir)
			lcMetaDataPath = ADDBS(ALLTRIM(ofoxapp.cdatadir)) +'Update\'
		ELSE
			lcMetaDataPath = ADDBS(datapath_loc) + 'Update\'
		ENDIF
	ELSE
		lcMetaDataPath = ADDBS(ALLTRIM(ofoxapp.cMetaPathDir))
	ENDIF
	lcMetadataTableNamePath = lcMetaDataPath + FORCEEXT(ALLTRIM(ofoxapp.cMetadataTableName),"DBF")
	IF ADIR(laExistMetaData, lcMetadataTableNamePath) = 0
		llNoDataDictTables = .T.
		IF tlNewVersion AND FILE(FORCEEXT(ofoxapp.cMetadataTableName, "dbf"))
			llNoDataDictTables = .F.
			USE (FORCEEXT(ofoxapp.cMetadataTableName, "dbf")) IN 0 ALIAS Datadict
			SELECT Datadict
			IF RECCOUNT() = 0
				llNoDataDictTables = .T.
				USE IN Datadict
			ENDIF
		ENDIF
	ELSE
		USE (lcMetadataTableNamePath) IN 0 ALIAS Datadict
		SELECT Datadict
		IF RECCOUNT() = 0
			llNoDataDictTables = .T.
			USE IN Datadict
		ENDIF
	ENDIF
CATCH TO oError
	llRes = .F.
	IF USED("Datadict")
		USE IN Datadict
	ENDIF
	onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
ENDTRY
IF !llRes
	RETURN .F.
ENDIF
IF llNoDataDictTables
	RETURN .T.
ENDIF

LOCAL lcVersionToStore, lcSys16, lnPos
DIMENSION laVersion[1]
lcVersionToStore = "0.0.0"
*{ V&U RI 2008-12-10
lcSys16 = SYS(16, 0)
lnPos = AT(":\", lcSys16)
lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
IF AGETFILEVERSION(laVersion, lcSys16) > 0
	IF !EMPTY(laVersion[4])
		lcVersionToStore = UPPER(ALLTRIM(laVersion[4]))
	ENDIF
ENDIF

*{ V&U MS 2009-01-12 Modified. VfxPath not used for SQL.
*  If load config is not successful .F. must be returned. If no SQL rows in config - .T. must be returned
IF glclientsupport
	IF !oFoxApp.LoadConfig("_TempPath")
		RETURN .F.
	ENDIF
	SELECT * FROM _temppath ;
		WHERE !(ALLTRIM(ConnStrType) == "P") OR VFXSystemTableLocation > 1 ; && Get row only for SQL
	INTO CURSOR temppath
	USE IN _temppath

	IF USED("TempPath")
		SELECT temppath

		COUNT ALL FOR !DELETED() TO lnreccnt
		IF lnreccnt > 0
			lnFieldCount = FCOUNT("temppath")
			DIMENSION aclients[lnreccnt,lnFieldCount]
			COPY TO ARRAY aclients FOR !DELETED()
			FOR lnCurrentField = 1 TO lnFieldCount
				lcFieldName = FIELD(lnCurrentField)
				IF TYPE(lcFieldName) = "M"
					SCAN FOR !DELETED()
						aclients[RECNO(), lnCurrentField] = &lcFieldName.
					ENDSCAN
				ENDIF
			ENDFOR
			USE IN TempPath
		ELSE
			USE IN TempPath
			RETURN .T.
		ENDIF
	ELSE
		USE IN TempPath
		RETURN .F.
	ENDIF
ELSE
	DIMENSION aclients[1,2]
	aclients[1,2] = datapath_loc
ENDIF
*} V&U MS 2009-01-12

* DataDict Exist
IF llWorkWithConfig
	lnClientCount = ALEN(aclients, 1)
	lnFieldsCount = ALEN(aclients, 2)

	DIMENSION aclientslist[lnClientCount, lnFieldsCount + 4]
*	the command ACOPY(aclients[], aclientslist) does not work (Reorder Array)
	VFXACOPY(@aclients, @aclientslist)

	LOCAL loADOConnection AS ADODB.CONNECTION
	LOCAL loException AS EXCEPTION
	LOCAL lcDBCPathName, lcDBCConnName, lcOnlyBDCName, lcUID, lcPWD, lcDatabase, lcDSN AS STRING
	LOCAL lnPathLen, lnSysTablesLocationCorrect, lnOldDispLogin AS INTEGER
	LOCAL llCloseMain, llCloseSystem

	llCloseMain = .F.
	llCloseSystem = .F.

*{ JEI DL 2008-08-21
	IF TYPE("ofoxapp.nConfigVFXFormat") = "N"
		lnSysTablesLocationCorrect = IIF(ofoxapp.nConfigVFXFormat = 0, 0, 1)
	ENDIF
*} JEI DL 2008-08-21

	FOR lnCurrentClient = 1 TO lnClientCount
		lcDBConnType = ALLTRIM(aclientslist[lnCurrentClient,2])
		lnVFXSysTablesLocation = aclientslist[lnCurrentClient,5 + lnSysTablesLocationCorrect] &&JEI DL 2008-08-21 modify
*{ Database connection
		IF !(lcDBConnType == "P") && Not Native
			lcConnectionType = STRTRAN(lcDBConnType, "A", "")
			TRY
				DO CASE
					CASE lcConnectionType == "D" &&  Connection kept in DBC
						lnPathLen = AT("!", aclientslist[lnCurrentClient, 3])
						lcDBCPathName = IIF(lnPathLen > 0, LEFT(aclientslist[lnCurrentClient, 3], lnPathLen -1), ;
							aclientslist[lnCurrentClient, 3])
						IF AT(":", lcDBCPathName) = 0
							lcDBCPathName = ADDBS(ALLTRIM(aclientslist[lnCurrentClient,7])) + lcDBCPathName
						ENDIF

						lcDBCConnName = IIF(lnPathLen > 0, SUBSTR(aclientslist[lnCurrentClient, 3], lnPathLen + 1),"")

						lcOnlyBDCName = JUSTSTEM(lcDBCPathName)
						IF !DBUSED(lcOnlyBDCName)
							OPEN DATABASE (lcDBCPathName) SHARED
						ENDIF
						SET DATABASE TO (lcOnlyBDCName)
						lcConnectionString = DBGETPROP(lcDBCConnName,"CONNECTION","ConnectString")
						IF EMPTY(lcConnectionString)
							lcDSN = DBGETPROP(lcDBCConnName,"CONNECTION","DataSource")
							lcUID = DBGETPROP(lcDBCConnName,"CONNECTION","UserId")
							lcPWD = DBGETPROP(lcDBCConnName,"CONNECTION","PassWord")
							lcDatabase = DBGETPROP(lcDBCConnName,"CONNECTION","Database")
							lcConnectionString = "DSN=" + lcDSN + ;
								IIF(!EMPTY(lcUID), ";UID=" + lcUID, "") + ;
								IIF(!EMPTY(lcPWD), ";PWD=" + lcPWD, "") + ;
								IIF(!EMPTY(lcDatabase),";DATABASE=" + lcDatabase,"")
						ENDIF
						CLOSE DATABASES

					CASE lcConnectionType == "C" && Connection string
						lcConnectionString = aclientslist[lnCurrentClient, 3]

					CASE lcConnectionType == "N" && DSN
						lcConnectionString = "DSN=" + ALLTRIM(aclientslist[lnCurrentClient, 3]) + ;
							IIF(!EMPTY(aclientslist[lnCurrentClient, 4]), ";UID=" + ALLTRIM(aclientslist[lnCurrentClient, 4]),"") + ;
							IIF(!EMPTY(aclientslist[lnCurrentClient, 5]),";PWD=" + ALLTRIM(aclientslist[lnCurrentClient, 5]),"")

				ENDCASE

				IF LEFT(lcDBConnType, 1) == "A"
					loADOConnection = NEWOBJECT("ADODB.Connection")
					loADOConnection.ConnectionString = lcConnectionString
					loADOConnection.OPEN()
					aclientslist[lnCurrentClient, lnFieldsCount + 1] = loADOConnection
					aclientslist[lnCurrentClient, lnFieldsCount + 2] = (loADOConnection.State = 1)
				ELSE
					lnOldDispLogin = SQLGETPROP(0,"DispLogin")
					SQLSETPROP(0,"DispLogin", 3)
					aclientslist[lnCurrentClient, lnFieldsCount + 1] = SQLSTRINGCONNECT(lcConnectionString)
					aclientslist[lnCurrentClient, lnFieldsCount + 2] = (aclientslist[lnCurrentClient, lnFieldsCount + 1] > 0)
					SQLSETPROP(0,"DispLogin", lnOldDispLogin)
				ENDIF
			CATCH TO loException

			ENDTRY
		ENDIF
*} Database connection

*{V&U MS 2009-09-03, Move BeforeClientDatabaseUpdate after connection to main database is done.
		IF aClientsList[lnCurrentClient, lnFieldsCount + 2]
			IF !goProgram.BeforeClientDatabaseUpdate(IIF(!(LEFT(lcDBConnType, 1) == "A"), ;
					aClientsList[lnCurrentClient, lnFieldsCount + 1], 0), ;
					IIF((LEFT(lcDBConnType, 1) == "A"), ;
					aClientsList[lnCurrentClient, lnFieldsCount + 1], .NULL.))
				TRY
					IF LEFT(lcDBConnType, 1) == "A"
						aclientslist[lnCurrentClient, lnFieldsCount + 1].CLOSE()
					ELSE
						 = SQLDISCONNECT(aclientslist[lnCurrentClient, lnFieldsCount + 1])
					ENDIF
				CATCH
* Do Nothing
				ENDTRY

				LOOP
			ENDIF
		ENDIF
*}V&U MS 2009-09-03

*{VFX system tables
		DO CASE
			CASE lnVFXSysTablesLocation > 1
				TRY
					lcConnectionString = ""
					DO CASE
						CASE INLIST(lnVFXSysTablesLocation, 2, 4) && 2 - Connecton string, 4 - ADO Connection strind
							lcConnectionString = aclientslist[lnCurrentClient, 10]

						CASE lnVFXSysTablesLocation = 3 && 3- DSN
							lcConnectionString = "DSN=" + ALLTRIM(aclientslist[lnCurrentClient, 10]) + ;
								IIF(!EMPTY(aclientslist[lnCurrentClient, 11]), ";UID=" + ALLTRIM(aclientslist[lnCurrentClient, 11]),"") + ;
								IIF(!EMPTY(aclientslist[lnCurrentClient, 12]), ";PWD=" + ALLTRIM(aclientslist[lnCurrentClient, 12]),"")
					ENDCASE

					IF lnVFXSysTablesLocation = 4 && ADO
						loADOConnection = NEWOBJECT("ADODB.Connection")
						loADOConnection.ConnectionString = lcConnectionString
						loADOConnection.OPEN()
						aclientslist[lnCurrentClient, lnFieldsCount + 3] = loADOConnection
						aclientslist[lnCurrentClient, lnFieldsCount + 4] = (loADOConnection.State = 1)
					ELSE
						lnOldDispLogin = SQLGETPROP(0,"DispLogin")
						SQLSETPROP(0,"DispLogin", 3)
						aclientslist[lnCurrentClient, lnFieldsCount + 3] = SQLSTRINGCONNECT(lcConnectionString)
						aclientslist[lnCurrentClient, lnFieldsCount + 4] = (aclientslist[lnCurrentClient, lnFieldsCount + 3] > 0)
						SQLSETPROP(0,"DispLogin", lnOldDispLogin)
					ENDIF
				CATCH TO loException

				ENDTRY
			CASE lnVFXSysTablesLocation = 1		&& system tables with main DB
				IF TYPE("aclientslist[lnCurrentClient, lnFieldsCount + 1]") == "N"
					aclientslist[lnCurrentClient, lnFieldsCount + 3] = aclientslist[lnCurrentClient, lnFieldsCount + 1]		&& connection handle
					aclientslist[lnCurrentClient, lnFieldsCount + 4] = aclientslist[lnCurrentClient, lnFieldsCount + 2]		&& is connection handle usable
				ELSE 		&& if main DB is local
					aclientslist[lnCurrentClient, lnFieldsCount + 3] = 0
					aclientslist[lnCurrentClient, lnFieldsCount + 4] = .F.
				ENDIF
			CASE lnVFXSysTablesLocation = 0		&& system tables local
				IF !(lcDBConnType == "P")
					aclientslist[lnCurrentClient, lnFieldsCount + 3] = IIF(EMPTY(aclientslist[lnCurrentClient, 10]),"DATA\", aclientslist[lnCurrentClient, 10])
					aclientslist[lnCurrentClient, lnFieldsCount + 4] = .T.
				ELSE
					aclientslist[lnCurrentClient, lnFieldsCount + 3] = ""
					aclientslist[lnCurrentClient, lnFieldsCount + 4] = .F.
				ENDIF
*{V&U MS 2008-09-04
				IF !tlDoNotRunVfxUpdate
					*{V&U MS 2011-11-24 5552
					vfx_doFreeUpdate(oFoxApp, ADDBS(ALLTRIM(oFoxApp.cUpdateDir)) + "VFXDIR", aClientsList[lnCurrentClient, lnFieldsCount + 3], @taUpdatedFreeFolders)
					*}V&U MS 2011-11-24
				ENDIF
*}V&U MS 2008-09-04
			OTHERWISE
		ENDCASE
*}VFX system tables

*} Update Database
		IF !(lcDBConnType == "P") AND !tlDoNotRunVfxUpdate 		&& {V&U MS 2008-09-04
			IF lnVFXSysTablesLocation = 1
				SELECT * FROM Datadict WHERE UPPER(ALLTRIM(ModuleDscr)) == "*__VFX_MAINDBANDSYSTB" ;
					INTO CURSOR crsOneDB
			ELSE
				SELECT * FROM Datadict WHERE UPPER(ALLTRIM(ModuleDscr)) == "*__VFX_MAINDATABASE" ;
					INTO CURSOR crsOneDB
			ENDIF
			IF USED("crsOneDB") AND RECCOUNT("crsOneDB") > 0 AND aclientslist[lnCurrentClient, lnFieldsCount + 2]
				IF 	ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 1], "Begin transaction", "", .T.)
					IF !SQLUpdate(IIF(!(LEFT(lcDBConnType, 1) == "A"), aclientslist[lnCurrentClient, lnFieldsCount + 1], 0), ;
							IIF((LEFT(lcDBConnType, 1) == "A"), aclientslist[lnCurrentClient, lnFieldsCount + 1], .NULL.), ;
							aclientslist[lnCurrentClient, 1])
						 = ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 1], "Rollback transaction", "", .T.)
						llErrorOccur = .T.
					ELSE
						IF !ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 1], "Commit transaction", "", .T.)
							 = ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 1], "Rollback transaction", "", .T.)
						ENDIF
					ENDIF
				ENDIF
				llCloseMain = .T.
			ENDIF
			IF USED("crsOneDB")
				USE IN crsOneDB
			ENDIF
		ENDIF

* VFX System tables in diferent location from base database
		IF lnVFXSysTablesLocation > 1  AND !tlDoNotRunVfxUpdate 		&& {V&U MS 2008-09-04
			SELECT * FROM Datadict WHERE UPPER(ALLTRIM(ModuleDscr)) == "*__VFX_SYSTABLES" ;
				INTO CURSOR crsOneDB
			IF RECCOUNT("crsOneDB")	 > 0 AND aclientslist[lnCurrentClient, lnFieldsCount + 4]
				IF ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 3], "Begin transaction", "", .T.)
					IF !SQLUpdate(IIF(INLIST(lnVFXSysTablesLocation, 1, 2, 3), aclientslist[lnCurrentClient, lnFieldsCount + 3], 0), ;
							IIF(INLIST(lnVFXSysTablesLocation, 4, 5), aclientslist[lnCurrentClient, lnFieldsCount + 3], .NULL.), ;
							aclientslist[lnCurrentClient, 1])

						 = ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 3], "Rollback transaction", "", .T.)
						llErrorOccur = .T.
					ELSE
						IF !ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 3], "Commit transaction", "", .T.)
							 = ExecuteCommand(aclientslist[lnCurrentClient, lnFieldsCount + 3], "Rollback transaction", "", .T.)
						ENDIF
					ENDIF
				ENDIF
				llCloseSystem = .T.
			ENDIF
			USE IN crsOneDB
		ENDIF
*} Update Database
*{ JEI IG 2006.10.25 Add
*{ Update version number
		IF !llErrorOccur AND aclientslist[lnCurrentClient, lnFieldsCount + 4]
			vfx_updateappversion(aclientslist[lnCurrentClient, lnFieldsCount + 3], lcVersionToStore)
		ENDIF
*} Update version number
*} JEI IG 2006.10.25

*{V&U MS 2009-09-03, Move AfterClientDatabaseUpdate before connection to main database is closed.
		IF aClientsList[lnCurrentClient, lnFieldsCount + 2]
			goProgram.AfterClientDatabaseUpdate(IIF(!(LEFT(lcDBConnType, 1) == "A"), ;
				aClientsList[lnCurrentClient, lnFieldsCount + 1], 0), ;
				IIF((LEFT(lcDBConnType, 1) == "A"), ;
				aClientsList[lnCurrentClient, lnFieldsCount + 1], .NULL.))
		ENDIF
*}V&U MS 2009-09-03

		TRY
			IF llCloseMain
				IF LEFT(lcDBConnType, 1) == "A"
					aclientslist[lnCurrentClient, lnFieldsCount + 1].CLOSE()
				ELSE
					 = SQLDISCONNECT(aclientslist[lnCurrentClient, lnFieldsCount + 1])
				ENDIF
			ENDIF
		CATCH
* Do Nothing
		ENDTRY
		TRY
			IF llCloseSystem
				IF lnVFXSysTablesLocation = 4 && ADO
					aclientslist[lnCurrentClient, lnFieldsCount + 3].CLOSE()
				ELSE
					 = SQLDISCONNECT(aclientslist[lnCurrentClient, lnFieldsCount + 3])
				ENDIF
			ENDIF
		CATCH
* Do Nothing
		ENDTRY
	ENDFOR
	IF llErrorOccur
		RETURN .F.
	ENDIF
ELSE
	FOR lnCurentDBC = 1 TO ALEN(aclients, 1)

		lcDataDir = ADDBS(aclients[lnCurentDBC ,2])
		lnDatabaseNumber = ADIR(laDataBase, lcDataDir + "*.dbc")

		IF lnDatabaseNumber > 0
			lcDBName = lcDataDir + laDataBase(1, 1)
			lcOpenDB = DBC()
			llCloseDatabase = .F.
			IF (UPPER(ALLTRIM(lcDBName)) == UPPER(ALLTRIM(lcOpenDB)))
				CLOSE DATABASES
				llCloseDatabase = .T.
			ENDIF
			USE &lcDBName IN 0 AGAIN ALIAS crsDatabase

			SELECT ObjectName FROM crsDatabase WHERE UPPER(ALLTRIM(ObjectType)) == "CONNECTION" ;
				INTO CURSOR csrAllUserConnection READWRITE
			USE IN crsDatabase

			SELECT DISTINCT auc.ObjectName, d.ModuleDscr, d.DBaseName, 0000 AS ConnHandle ;
				FROM Datadict d LEFT OUTER JOIN csrAllUserConnection auc ;
				ON UPPER(ALLTRIM(auc.ObjectName)) == UPPER(ALLTRIM(d.ModuleDscr)) ;
				WHERE !((ALLTRIM(UPPER(ModuleDscr)) == "*__VFX_SYSTABLES") OR ;
				(ALLTRIM(UPPER(ModuleDscr)) == "*__VFX_MAINDBANDSYSTB") OR  ;
				(ALLTRIM(UPPER(ModuleDscr)) == "*__VFX_MAINDATABASE")) ;
				INTO CURSOR crsConnectins READWRITE

			IF RECCOUNT() > 0
				IF TYPE("ofoxapp.ointroform") ="O"
					IF !ISNULL(ofoxapp.ointroform)
						ofoxapp.ointroform.HIDE()
					ENDIF
				ENDIF
				IF !_VFP.VISIBLE
					_VFP.VISIBLE = .T.
					_SCREEN.WINDOWSTATE = GoProgram.nWindowState
				ENDIF
				IF TYPE("goProgram.class") ="C"
					goProgram.vfxwaitwindow(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING), , , .T., , .T.,)
				ENDIF
				OPEN DATABASE &lcDBName
				SELECT crsConnectins
				LOCAL lcModuleDescr, lcDataBaseName, lnConnHandle
				SCAN
					lcModuleDescr = ALLTRIM(crsConnectins.ModuleDscr)
					lcDataBaseName = ALLTRIM(crsConnectins.DBaseName)
					lcObjectName = ALLTRIM(crsConnectins.ObjectName)
					IF ISNULL(crsConnectins.ObjectName)
						lcConnStr = TryConnecting(lcDataBaseName, "", lcModuleDescr, ofoxapp)
						IF EMPTY(lcConnStr)
							RollbackTran()
							RETURN .F.
						ENDIF
						lnConnHandle = SQLSTRINGCONNECT(lcConnStr)
						IF lnConnHandle < 0
							RollbackTran()
							RETURN .F.
						ENDIF
* Create connection
						lcCreateConn = "CREATE CONNECTION [" + lcModuleDescr + "] CONNSTRING [" + lcConnStr + "]"
						&lcCreateConn
						SQLDISCONNECT(lnConnHandle)

						lnOldDispLogin = DBGETPROP(lcModuleDescr,"CONNECTION","DispLogin")
						DBSETPROP(lcModuleDescr, "Connection", "DispLogin", 3)
						lnConnHandle = SQLCONNECT(lcModuleDescr) && For test new connection
						DBSETPROP(lcModuleDescr, "Connection", "DispLogin", lnOldDispLogin)

						IF lnConnHandle < 0
							RollbackTran()
							RETURN .F.
						ENDIF
						SELECT crsConnectins
						REPLACE ConnHandle WITH lnConnHandle
					ELSE
						lcObjectName = ALLTRIM(csrAllUserConnection.ObjectName)
						lcDataSource = ALLTRIM(DBGETPROP(lcObjectName,"CONNECTION","DATASOURCE"))
						lcConnStr = ALLTRIM(DBGETPROP(lcObjectName,"CONNECTION","CONNECTSTRING"))
						IF EMPTY(lcDataSource) AND EMPTY(lcConnStr )
							RollbackTran()
							RETURN .F.
						ENDIF
						IF !EMPTY(lcDataSource)
							lnOldDispLogin = DBGETPROP(lcObjectName,"CONNECTION","DispLogin")
							DBSETPROP(lcObjectName, "Connection", "DispLogin", 3)
							lnConnHandle = SQLCONNECT(lcObjectName)
							DBSETPROP(lcObjectName, "Connection", "DispLogin", lnOldDispLogin)
						ELSE
							lnConnHandle = SQLSTRINGCONNECT(lcConnStr)
						ENDIF
						IF lnConnHandle  > 0
							IF SQLEXEC(lnConnHandle,"sp_tables", "crsCheckDB") < 0
								RollbackTran()
								RETURN .F.
							ENDIF
							SELECT crsCheckDB
							GO TOP
							IF !(UPPER(ALLTRIM(Table_Qualifier)) == UPPER(lcDataBaseName))
								SQLDISCONNECT(lnConnHandle)
								lcServerName = ""
								IF EMPTY(lcDataSource)
									lcServerName = GetServerNameFromConnStr(lcConnStr)
								ENDIF
								lcConnStr = TryConnecting(lcDataBaseName, lcServerName, lcModuleDescr, ofoxapp)
								IF EMPTY(lcConnStr)
									RollbackTran()
									RETURN .F.
								ENDIF
								lnConnHandle = SQLSTRINGCONNECT(lcConnStr)
							ENDIF
							USE IN crsCheckDB
							SELECT crsConnectins
							REPLACE ConnHandle WITH lnConnHandle
						ELSE
							IF EMPTY(lcDataSource) && Connection with connection string
								lcServerName = GetServerNameFromConnStr(lcConnStr)
								lcConnStr = TryConnecting(lcDataBaseName, lcServerName, lcModuleDescr, ofoxapp)
								IF EMPTY(lcConnStr)
									RollbackTran()
									RETURN .F.
								ELSE
									lnConnHandle = SQLSTRINGCONNECT(lcConnStr)
									IF lnConnHandle  < 0
										RollbackTran()
										RETURN .F.
									ENDIF
								ENDIF
								DELETE CONNECTION &lcObjectName
								lcCreateConn = "CREATE CONNECTION [" + lcObjectName + "] CONNSTRING [" + lcConnStr + "]"
								&lcCreateConn
								SELECT crsConnectins
								REPLACE ConnHandle WITH lnConnHandle
							ELSE && Use DSN
								lcConnStr = TryConnecting(lcDataBaseName, "", lcModuleDescr, ofoxapp)
								IF EMPTY(lcConnStr)
									RollbackTran()
									RETURN .F.
								ENDIF
								lnConnHandle = SQLSTRINGCONNECT(lcConnStr)
								IF lnConnHandle  < 0
									RollbackTran()
									RETURN .F.
								ENDIF
								*{ HC BB 2016-02-29, 6924
								DECLARE Short SQLDataSources IN ODBC32.DLL ;
									LONG    henv, INTEGER fDirection, ;
									STRING @ szDSN, INTEGER cbDSNMax, INTEGER @ pcbDSN, ;
									STRING @ szDescription, INTEGER cbDescriptionMax, INTEGER pcbDescription
								*} HC BB 2015-02-29
								
								nODBCEnv = VAL(SYS(3053))
								llDNSExist = .T.
								DO WHILE .T.
									dsn = SPACE(1024)
									dsndesc = SPACE(1024)
									mdsn = 0
									mdesc = 0
									nRetVal = SQLDataSources(nODBCEnv, 1, @dsn, 1024, @mdsn, @dsndesc, 255, @mdesc)
									DO CASE
										CASE nRetVal = 100
											EXIT
										CASE m.nRetVal # 0 AND m.nRetVal # 1
											EXIT
										OTHERWISE
											IF UPPER(ALLTRIM(lcDataSource)) == UPPER(ALLTRIM(dsn))
												llDNSExist = .T.
												EXIT
											ENDIF
									ENDCASE
								ENDDO
								IF llDNSExist = .T.
									fRequest = 2				&& Configure (edit) data source
								ELSE
									fRequest = 1              	&& Add data source
								ENDIF
								DECLARE INTEGER SQLConfigDataSource IN odbccp32.DLL ;
									INTEGER hwndParent, INTEGER fRequest, ;
									STRING lsDriver, STRING lsAttributes

								lcServerName = GetServerNameFromConnStr(lcConnStr)
								lnDB = AT("DATABASE=", UPPER(lcConnStr))
								IF lnDB > 0
									lcDataBaseName = SUBSTR(lcConnStr, lnDB + 9)
									lnpos = AT(";", lcDataBaseName)
									IF lnpos > 0
										lcDataBaseName = SUBSTR(lcDataBaseName, lnpos -1)
									ENDIF
								ELSE
									lcDataBaseName = ""
								ENDIF
								lnTrsConnPos = AT("TRUSTED_CONNECTION=", UPPER(lcConnStr))
								IF lnTrsConnPos > 0 && Used trusted connection
									lsAttributes = "DSN=" + lcDataSource + CHR(0) + ;
										"SERVER=" + lcServerName + CHR(0) + ;
										"TRUSTED_CONNECTION=1" + CHR(0) + ;
										"Database=" + lcDataBaseName + CHR(0)
								ELSE
									lsAttributes = "DSN=" + lcDataSource + CHR(0) + ;
										"SERVER=" + lcServerName + CHR(0) + ;
										"Database=" + lcDataBaseName + CHR(0)
								ENDIF

								lRetValue = SQLConfigDataSource(0, fRequest,"SQL Server" + CHR(0), lsAttributes)
								IF lRetValue <> 1
									RollbackTran()
									RETURN .F.
								ENDIF
								lnError = 0
								TRY
									lcUSerIDPos = AT("UID=", lcConnStr)
									IF lcUSerIDPos > 0
										lcUserName = SUBSTR(lcConnStr, lcUSerIDPos + 4)
										lnEndPoint = AT(";", lcUserName)
										IF lnEndPoint > 0
											lcUserName = SUBSTR(lcUserName, 1, lnEndPoint -1)
										ENDIF
										lnPassPos = AT("PWD=", lcConnStr)
										lcPass = SUBSTR(lcConnStr, lnPassPos + 4)
										lcEndPoint = AT(";", lcPass)
										IF lcEndPoint > 0
											lcPass = SUBSTR(lcPass, 1, lcEndPoint - 1)
										ENDIF
										lcDElConn = "DELETE CONNECTION [" + lcObjectName + "]"
										&lcDElConn
										lcCreateConn = "CREATE CONNECTION [" + lcObjectName + "] DATASOURCE [" + ;
											lcDataSource + "] USERID [" + lcUserName +"] PASSWORD [" + ;
											lcPass + "]"
										&lcCreateConn
									ELSE
										lcCreateConn = "CREATE CONNECTION [" + lcObjectName + "] " + ;
											"DATASOURCE [" + lcDataSource + "]"
										&lcCreateConn
									ENDIF
								CATCH TO oError
									lnError = oError.ERRORNO
								ENDTRY
								IF lnError > 0
									RollbackTran()
									RETURN .F.
								ENDIF

								lnOldDispLogin = DBGETPROP(lcObjectName,"CONNECTION","DispLogin")
								DBSETPROP(lcObjectName, "Connection", "DispLogin", 3)
								lnConnHandle = SQLCONNECT(lcObjectName)
								DBSETPROP(lcObjectName, "Connection", "DispLogin", lnOldDispLogin)

								IF lnConnHandle < 0
									RollbackTran()
									RETURN .F.
								ENDIF
								SELECT crsConnectins
								REPLACE ConnHandle WITH lnConnHandle
							ENDIF
						ENDIF
					ENDIF
					lnRes = SQLEXEC(crsConnectins.ConnHandle,"BEGIN TRANSACTION")
					IF lnRes < 0
						RollbackTran()
						RETURN .F.
					ENDIF
				ENDSCAN
				SCAN FOR ConnHandle > 0 &&Real Connection
					SELECT * FROM Datadict WHERE UPPER(ALLTRIM(ModuleDscr)) == ;
						UPPER(ALLTRIM(crsConnectins.ModuleDscr)) ;
						INTO CURSOR crsOneDB
					IF RECCOUNT("crsOneDB") > 0 AND !tlDoNotRunVfxUpdate 		&& {V&U MS 2008-09-04
						IF !SQLUpdate(crsConnectins.ConnHandle)
							RollbackTran()
							RETURN .F.
						ENDIF
					ENDIF
				ENDSCAN
				llerror = .F.
				SCAN FOR ConnHandle > 0 &&Real Connection
					IF SQLEXEC(crsConnectins.ConnHandle, "COMMIT TRANSACTION") < 0
						RollbackTran()
						llerror = .T.
					ENDIF
					IF llerror = .F.
						SQLDISCONNECT(crsConnectins.ConnHandle)
					ENDIF
				ENDSCAN
				IF llerror
					RETURN .F.
				ENDIF
			ENDIF

			IF llCloseDatabase
				OPEN DATABASE &lcOpenDB
			ENDIF

		ENDIF
	ENDFOR
ENDIF
IF USED("Datadict")
	USE IN Datadict
ENDIF
IF ADIR(laDummy, lcMetadataTableNamePath) = 1
	DELETE FILE(lcMetadataTableNamePath)
	DELETE FILE(FORCEEXT(lcMetadataTableNamePath,"fpt"))
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: UpdateApp
* Called by...:
*
* Abstract....: Update the running exe file at the client side.
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION UpdateApp()
LPARAMETERS tnUpdateType, tmUpdateApp
LOCAL lcNewName, lcVFXSysPath, lcDownloadFilePath, lcExeDir, lcFileNameAlt, ;
	oError AS EXCEPTION, oDownload AS OBJECT, laVersion[1], lcMessageText AS STRING, ;
	lcSys16, lnPos
LOCAL loXML AS XMLADAPTER
LOCAL lnUpdateType, lmUpdateApp, llDownload, lcProgramFile

llDownload = .T.
lcProgramFile = ""

IF TYPE("tnUpdateType") = "N" AND tnUpdateType >= 1 AND tnUpdateType <= 4
	lnUpdateType = tnUpdateType
ELSE
	lnUpdateType = 1
ENDIF

IF TYPE("tmUpdateApp") == "C" AND !EMPTY(tmUpdateApp)
	lmUpdateApp = tmUpdateApp
ELSE
	IF TYPE("GoSystem.UpdateApp") # "U"
		lmUpdateApp = GoSystem.UpdateApp
	ENDIF
ENDIF

IF EMPTY(lmUpdateApp)
	RETURN
ENDIF

* Delete old cache file
lcUrl = ""
DECLARE INTEGER DeleteUrlCacheEntry IN Wininet STRING lcUrl
lnMemLinesUpdateApp = MEMLINES(lmUpdateApp)
FOR k = 1 TO lnMemLinesUpdateApp
	lcLine = MLINE(lmUpdateApp, k)
	IF UPPER(LEFT(lcLine, 2)) == "D:"
		lcUrl = ALLTRIM(SUBSTR(lcLine, AT(":", lcLine) + 1))
		lnres = DeleteUrlCacheEntry(lcUrl)
	ENDIF
ENDFOR
CLEAR DLLS DeleteUrlCacheEntry

IF VERSION(2) = 0
*{ V&U RI 2008-12-10
	lcSys16 = SYS(16, 0)
	lnPos = AT(":\", lcSys16)
	lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
	lcProgramFile = FORCEEXT(lcSys16,"vfx")
	IF FILE(lcProgramFile)
		llDownload = .F.
	ENDIF
ENDIF

IF llDownload
	*{V&U MS 2012-06-28 
	* Exe update without CusMan
	IF AT('updatecustomers.vfx', LOWER(lmUpdateApp)) = 0 AND AT("updateversions.vfx", LOWER(lmUpdateApp)) = 0 AND tnUpdateType = 3
		IF TYPE("goProgram.class") ="C"
			lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DOWNLOADANDINSTALLUPD, MSG_DOWNLOADANDINSTALLUPD)
		ELSE
			lcMessageText = "Download and install updates?"
		ENDIF
		IF goProgram.vfxmessagebox(lcMessageText, 4 + 32, _SCREEN.CAPTION) = 7
			RETURN .F.
		ENDIF
	ENDIF
	*}V&U MS 2012-06-28 
	oDownload = CREATEOBJECT("CDownload")
	IF !oDownload.execmacro(lmUpdateApp, .T.)
*{JEI MS 11.12.2007
		IF !EMPTY(oDownload.LastErrorText)
			lcMessageText = ALLTRIM(oDownload.LastErrorText)
			IF TYPE("goProgram.class") = "C"
				goProgram.vfxmessagebox(lcMessageText, 0 + 16, _SCREEN.CAPTION)
			ELSE
				MESSAGEBOX(lcMessageText, 0 + 16, _SCREEN.CAPTION)
			ENDIF
		ENDIF
*}JEI MS 11.12.2007
		RETURN
	ENDIF
* UpdateCustomers.vfx
	lcUpdateCustomersFile = ADDBS(SYS(2023)) + "UpdateCustomers.vfx"
	IF FILE(lcUpdateCustomersFile)
		lnErrorNo = 0
		TRY
			lcXML = FILETOSTR(lcUpdateCustomersFile)
		CATCH TO oError
			lnErrorNo = oError.ERRORNO
		ENDTRY
		IF lnErrorNo > 0
			RELEASE oDownload
			onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
			RETURN .F.
		ENDIF
		lcPass = ""
		IF TYPE("goProgram.cConfigPassword") = "C"
			lcPass = goProgram.cConfigPassword
		ENDIF
		ERASE (lcUpdateCustomersFile)
		lcXMLDecrypt = Decrypt(lcXml, lcPass)
		lnErrorNo = 0
		lcCursorName = SYS(2015)
		IF TYPE("lcXMLDecrypt") = "C"
			TRY
				loXML = CREATEOBJECT("XMLAdapter")
				loXML.LOADXML(lcXMLDecrypt, .F.)
				loXML.TABLES.ITEM(1).TOCURSOR(.F., lcCursorName)
				loXML.RELEASEXML(.T.)
			CATCH TO oError
				lnErrorNo = oError.ERRORNO
			ENDTRY
			IF lnErrorNo > 0
				RELEASE oDownload
				onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
				RETURN .F.
			ENDIF

			lcRegKey = ""
			vcINIFileName = ""
			IF TYPE("goActivation") = "O"
				goActivation.GenerateHardwareKey(@lcRegKey, @vcINIFileName, .T.)
			ENDIF

			SELECT(lcCursorName)
			LOCATE FOR UPPER(ALLTRIM(CustomerID)) == lcRegKey
			IF !FOUND()
				IF tnUpdateType = 4
					IF TYPE("goProgram.class") ="C"
						lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_NOUPDATEVERSIONAVAILABLE, MSG_NOUPDATEVERSIONAVAILABLE)
					ELSE
						lcMessageText = "No update version available."
					ENDIF
					 = goProgram.vfxmessagebox(lcMessageText, 0 + 64, _SCREEN.CAPTION)
				ENDIF 
				RETURN .F.
			ENDIF

			lcUpdateVersionsFile = ADDBS(SYS(2023)) + "UpdateVersions.vfx"
			IF FILE(lcUpdateVersionsFile)
				lnErrorNo = 0
				TRY
					lcXML = FILETOSTR(lcUpdateVersionsFile)
				CATCH TO oError
					lnErrorNo = oError.ERRORNO
				ENDTRY
				IF lnErrorNo > 0
					RELEASE oDownload
					onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
					RETURN .F.
				ENDIF
				ERASE (lcUpdateVersionsFile)
				lcXMLDecrypt = Decrypt(lcXml, lcPass)
				lnErrorNo = 0
				lcCursorName = SYS(2015)
				IF TYPE("lcXMLDecrypt") = "C"
					TRY
						loXML.LOADXML(lcXMLDecrypt, .F.)
						loXML.TABLES.ITEM(1).TOCURSOR(.F., lcCursorName)
					CATCH TO oError
						lnErrorNo = oError.ERRORNO
					ENDTRY
					IF lnErrorNo > 0
						RELEASE oDownload
						onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
						RETURN .F.
					ENDIF
				ELSE
					RETURN .F.
				ENDIF
			ELSE
				RETURN .F.
			ENDIF

*{ V&U RI 2008-12-10
			lcSys16 = SYS(16, 0)
			lnPos = AT(":\", lcSys16)
			lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
			IF AGETFILEVERSION(laVersion, lcSys16) = 0
				RELEASE oDownload
				RETURN .F.
			ENDIF

			IF EMPTY(laVersion[4])
				RETURN .F.
			ENDIF
			lcCurrentVersion = UPPER(ALLTRIM(laVersion[4]))

* Get URL
			lcApplicationURL = ""
			SELECT(lcCursorName)
			SCAN
				IF versnr(UPPER(ALLTRIM(AppVersion))) <= versnr(lcCurrentVersion)
					lcApplicationURL = ALLTRIM(DownlURL)
					EXIT
				ENDIF
			ENDSCAN
			USE IN (lcCursorName)
			IF !EMPTY(lcApplicationURL)
				IF !(UPPER(LEFT(lcApplicationURL, 6)) = "FTP://") AND !(UPPER(LEFT(lcApplicationURL, 7)) = "HTTP://")
					lcApplicationURL = "http://" + lcApplicationURL
				ENDIF

				IF tnUpdateType = 3
					IF TYPE("goProgram.class") ="C"
						lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DOWNLOADANDINSTALLUPD, MSG_DOWNLOADANDINSTALLUPD)
					ELSE
						lcMessageText = "Download and install updates?"
					ENDIF
					IF goProgram.vfxmessagebox(lcMessageText, 4 + 32, _SCREEN.CAPTION) = 7
						RELEASE oDownload
						RETURN .F.
					ENDIF
				ENDIF
				IF TYPE("goProgram.class") ="C"
					lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DOWNLOADINGUPDATES, MSG_DOWNLOADINGUPDATES)
*{JEI MS 04.02.2008 Modified
					 = goProgram.vfxWaitWindow(lcMessageText, , , .T.,, .T.)
*}JEI MS 04.02.2008
				ELSE
					lcMessageText = "Downloading updates."
*{JEI MS 04.02.2008 Modified
					WAIT WINDOW lcMessageText NOWAIT NOCLEAR
*}JEI MS 04.02.2008
				ENDIF
				IF !oDownload.execmacro("D:" + lcApplicationURL, .T.)
					RELEASE oDownload
					IF TYPE("goProgram.class") == "C"
						goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
					ELSE
						WAIT CLEAR
					ENDIF
					RETURN .F.
				ENDIF
			ELSE
				IF tnUpdateType = 4
					IF TYPE("goProgram.class") == "C"
						lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RUNNINGLATESTVERSION, MSG_RUNNINGLATESTVERSION)
						 = goProgram.vfxmessagebox(lcMessageText, 0 + 64, _SCREEN.CAPTION)
					ELSE
						lcMessageText = "You are running the latest application version."
						WAIT WINDOW lcMessageText
					ENDIF
				ENDIF 
				RETURN .F.
			ENDIF
		ENDIF
	ENDIF
	
	*{V&U MS 2011-04-05
	IF TYPE("goProgram.class") == "C"
		lcWaitText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_WARNINGYOURAPPLICATIONWILLBERESTARTEDINAFEWSECONDS, ;
						MSG_WARNINGYOURAPPLICATIONWILLBERESTARTEDINAFEWSECONDS)
		= goProgram.vfxWaitWindow(lcWaitText, , , , , , 3)	
		= goProgram.vfxWaitWindow(lcWaitText, , , .T., , .T.)	
	ELSE 
		lcWaitText = "Warning: Your application will be restarted in a few seconds."
		WAIT WINDOW lcWaitText TIMEOUT 3
		WAIT WINDOW lcWaitText NOWAIT NOCLEAR
	ENDIF 	
	*}V&U MS 2011-04-05

*{JEI MS 26.09.2006 Moved from cFoxApp.Init
*{V&U MS 2009-08-03 Added !EMPTY(goProgram.cAddFilesDownloadURL)
* Download additional files
	IF TYPE("goProgram.Class") == "C" AND !EMPTY(goProgram.cAddFilesDownloadURL)
		goProgram.AddFilesUpdate()
	ENDIF
*}V&U MS 2009-08-03
*}JEI MS 26.09.2006

*JEI VM 20080829{
	IF TYPE("goProgram.class") == "C"
		goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
	ELSE
*JEI VM 20080829}
		WAIT CLEAR
	ENDIF

	RELEASE oDownload
	
	lcFileName = SYS(16, 0)
*{ V&U RI 2008-12-10
	lnPos = AT(":\", lcFileName)
	lcFileName = SUBSTR(lcFileName, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
	lcDownloadFilePath = ADDBS(SYS(2023)) + FORCEEXT(JUSTFNAME(lcFileName), "vfx")
	DIMENSION ladummy[1,1]
	IF ADIR(ladummy, lcDownloadFilePath) = 0
		RETURN
	ENDIF
	lcDownloadFile = FORCEEXT(lcFileName,"vfx")

	IF ADIR(ladummy, lcDownloadFile) = 1
		ERASE (lcDownloadFile)
	ENDIF
	COPY FILE (lcDownloadFilePath) TO (lcDownloadFile)
	ERASE (lcDownloadFilePath)
	lcFileDir = JUSTPATH(lcFileName)
	lcLoaderPath = ADDBS(lcFileDir) + "Loader.exe"
ELSE
	lcFileName = SYS(16, 0)
*{ V&U RI 2008-12-10
	lnPos = AT(":\", lcFileName)
	lcFileName = SUBSTR(lcFileName, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
	lcDownloadFile = FORCEEXT(lcFileName,"vfx")
	lcLoaderPath = ADDBS(JUSTPATH(lcFileName)) + "Loader.exe"
ENDIF

IF tnUpdateType = 2 OR (tnUpdateType = 4 AND !llDownload)
	IF TYPE("goProgram.class") ="C"
		lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATESREADYTOINSTALL, MSG_UPDATESREADYTOINSTALL)
		IF goProgram.vfxmessagebox(lcMessageText, 4 + 32, _SCREEN.CAPTION) = 7
			RETURN .F.
		ENDIF
	ELSE
		lcMessageText = "Updates are ready to install! Proceed?"
		IF MESSAGEBOX(lcMessageText, 4 + 32, _SCREEN.CAPTION) = 7
			RETURN .F.
		ENDIF
	ENDIF
ENDIF
 = ShellExecute(0,"Open" + CHR(0), lcLoaderPath + CHR(0), '"' + lcFileName + '"' + CHR(0),"", 1)

QUIT
ENDFUNC

*-------------------------------------------------------
* Function....: ExecuteCommand
* Called by...: SQLUpdate
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......: If a cursor/CA with alias = tcResultAlias
*				is open it is closed first
*-------------------------------------------------------
FUNCTION ExecuteCommand
LPARAMETERS tvDataSource AS Variant, tcCommand AS STRING, tcResultAlias AS STRING, tlCANotStartTransaction AS Logical

LOCAL llUseADO, llRes, llBindSuccessful AS Logical
LOCAL laCAExecSelectCmd AS CURSORADAPTER
LOCAL loADOCommand AS ADODB.COMMAND
LOCAL loRecordSet AS ADODB.Recordset
LOCAL lnRes, lnRecordSetNum AS INTEGER
LOCAL loException AS EXCEPTION
LOCAL lcTempFilePathName, lcResultAlias AS STRING
LOCAL loXMLAdapter AS XMLADAPTER
LOCAL loWaitExecution AS OBJECT

*{Check input parameters
IF VARTYPE(tvDataSource) = "N" AND tvDataSource > 0
	llUseADO = .F.
ELSE
	IF VARTYPE(tvDataSource) = "O"
		llUseADO = .T.
	ELSE
		RETURN .F.
	ENDIF
ENDIF
*}Check input parameters

*{Close Cursor/CA with this alias
IF !EMPTY(tcResultAlias) AND USED(tcResultAlias)
	IF BETWEEN(CURSORGETPROP("SourceType", tcResultAlias), 100, 299) OR CURSORGETPROP("SourceType", tcResultAlias) = 4	&& CursorAdapter
		loCARef = GETCURSORADAPTER(tcResultAlias)
		loCARef.CURSORDETACH()
		RELEASE loCARef
	ENDIF
	USE IN (tcResultAlias)
ENDIF
*}Close Cursor/CA with this alias

IF llUseADO
* Connection is ADO
	IF !EMPTY(tcResultAlias) AND VARTYPE(tcResultAlias) = "C" AND LEFT(UPPER(tcCommand), 6) ==  "SELECT"
* Command is SQL Select and result alias is specified.
		loADOCommand = CREATEOBJECT('ADODB.Command')
		loADOCommand.ActiveConnection = tvDataSource
		laCAExecSelectCmd = NEWOBJECT("CursorAdapter")

*{ Setup CursorAdapter properties
		laCAExecSelectCmd.DATASOURCETYPE = "ADO"
		laCAExecSelectCmd.USETRANSACTIONS = !tlCANotStartTransaction
		laCAExecSelectCmd.DATASOURCE = CREATEOBJECT("ADODB.Recordset") &&tvDataSource
		laCAExecSelectCmd.ALIAS = tcResultAlias
		laCAExecSelectCmd.SELECTCMD = tcCommand
*} Setup CursorAdapter properties

* Call CausorFill with object from class ADODB.Command for creating parameters if it necessity.
		TRY
			llCFillRes = laCAExecSelectCmd.CURSORFILL(laCAExecSelectCmd.USECURSORSCHEMA, laCAExecSelectCmd.NODATA, 0, loADOCommand)
		CATCH
			llCFillRes = .F.

			IF TYPE("goExecCommError") # "O"
				PUBLIC goExecCommError
				goExecCommError = CREATEOBJECT("empty")
				ADDPROPERTY(goExecCommError,"ErrorCount", 0)
			ENDIF
			goExecCommError.ErrorCount = goExecCommError.ErrorCount + 1
			lcErrorName = "Error" + TRANSFORM(goExecCommError.ErrorCount)
			&lcErrorName. = CREATEOBJECT("empty")

			ADDPROPERTY(goExecCommError, lcErrorName,&lcErrorName.)
			ADDPROPERTY(goExecCommError.&lcErrorName.,"Errorno",ERROR())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"Method",PROGRAM())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"LineNo",LINENO())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"Message",MESSAGE())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"LineContents",MESSAGE(1))
			ADDPROPERTY(goExecCommError.&lcErrorName.,"tcCommand",tcCommand)
		ENDTRY
		IF llCFillRes
* CursorAdapter fill successfully
			laCAExecSelectCmd.CURSORDETACH()
			RETURN .T.
		ELSE
			RETURN .F.
		ENDIF
	ELSE
* Command is not SQL Select or result alias is not specified
		loRecordSet  = .NULL.
		lcTempFilePathName = ""
		TRY
			loADOCommand = NEWOBJECT("ADODB.Command")
*{ Setup ADO Command settings
			loADOCommand.ActiveConnection = tvDataSource
			loADOCommand.CommandText = tcCommand
*{ Setup ADO Command settings
			CreateADOParameters(loADOCommand) && Create ADO parameters
			loRecordSet = loADOCommand.Execute() && Execute command
			lnRecordSetNum = 0
			IF !EMPTY(tcResultAlias)
&& Result alias is specified
				DO WHILE !ISNULL(loRecordSet) AND loRecordSet.State > 0  && For each recordset it resulted from command
					lcTempFilePathName = ADDBS(SYS(2023)) + FORCEEXT(SYS(2015),"XML")
					loRecordSet.SAVE(lcTempFilePathName, 1) && Save data from RecordSet like XML file.

*} Load XML file and create VFP cursor form  XML file.
					loXMLAdapter = NEWOBJECT("XMLAdapter")
					loXMLAdapter.LOADXML(lcTempFilePathName, .T.)
					lcResultAlias = tcResultAlias + IIF(lnRecordSetNum > 0, TRANSFORM(lnRecordSetNum), "")
					loXMLAdapter.TABLES.ITEM(1).TOCURSOR(.F., lcResultAlias)
					RELEASE loXMLAdapter
*} Load XML file and create VFP cursor form  XML file.

					IF !EMPTY(lcTempFilePathName) AND FILE(lcTempFilePathName)
						ERASE (lcTempFilePathName)
					ENDIF

					lnRecordSetNum = lnRecordSetNum + 1
					TRY
* Try to get next recodset if it exist.
						loRecordSet = loRecordSet.NextRecordset()
					CATCH TO loException
						TRY
							IF VARTYPE(loRecordSet) = "O" AND !ISNULL(loRecordSet) AND loRecordSet.State <> 0
								loRecordSet.CLOSE()
							ENDIF
						CATCH TO loException
						ENDTRY
						loRecordSet = .NULL.
					ENDTRY
				ENDDO
			ENDIF
			llRes = .T.
		CATCH TO loException
			llRes = .F.
			IF TYPE("goExecCommError") # "O"
				PUBLIC goExecCommError
				goExecCommError = CREATEOBJECT("empty")
				ADDPROPERTY(goExecCommError,"ErrorCount", 0)
			ENDIF
			goExecCommError.ErrorCount = goExecCommError.ErrorCount + 1
			lcErrorName = "Error" + TRANSFORM(goExecCommError.ErrorCount)
			&lcErrorName. = CREATEOBJECT("empty")

			ADDPROPERTY(goExecCommError, lcErrorName,&lcErrorName.)

			ADDPROPERTY(goExecCommError.&lcErrorName.,"Errorno",ERROR())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"Method",PROGRAM())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"LineNo",LINENO())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"Message",MESSAGE())
			ADDPROPERTY(goExecCommError.&lcErrorName.,"LineContents",MESSAGE(1))
			ADDPROPERTY(goExecCommError.&lcErrorName.,"tcCommand",tcCommand)
		FINALLY
			RELEASE loRecordSet
		ENDTRY
		RETURN llRes
	ENDIF
ELSE
*Connection is ODBC
	lnRes = -2
	IF !EMPTY(tcResultAlias) AND VARTYPE(tcResultAlias) = "C"
* Result alias is specified
		lnRes = SQLEXEC(tvDataSource, tcCommand, tcResultAlias)
	ELSE
* Result alias is not specified
		lnRes = SQLEXEC(tvDataSource, tcCommand)
	ENDIF
	IF lnRes <= 0
		IF TYPE("goExecCommError") # "O"
			PUBLIC goExecCommError
			goExecCommError = CREATEOBJECT("empty")
			ADDPROPERTY(goExecCommError,"ErrorCount", 0)
		ENDIF
		goExecCommError.ErrorCount = goExecCommError.ErrorCount + 1
		lcErrorName = "Error" + TRANSFORM(goExecCommError.ErrorCount)
		&lcErrorName. = CREATEOBJECT("empty")

		ADDPROPERTY(goExecCommError, lcErrorName,&lcErrorName.)
		ADDPROPERTY(goExecCommError.&lcErrorName.,"Errorno",ERROR())
		ADDPROPERTY(goExecCommError.&lcErrorName.,"Method",PROGRAM())
		ADDPROPERTY(goExecCommError.&lcErrorName.,"LineNo",LINENO())
		ADDPROPERTY(goExecCommError.&lcErrorName.,"Message",MESSAGE())
		ADDPROPERTY(goExecCommError.&lcErrorName.,"LineContents",MESSAGE(1))
		ADDPROPERTY(goExecCommError.&lcErrorName.,"tcCommand",tcCommand)
	ENDIF
	RETURN (lnRes > 0)
ENDIF

ENDFUNC

*-------------------------------------------------------
* Function....: CreateADOParameters
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CreateADOParameters
LPARAMETERS toADOCommnd AS ADODB.COMMAND

LOCAL llRes AS Logical
LOCAL lcBuffer, laCommndPar[1], lcParameterName, lcParameterType AS STRING
LOCAL lnParametrsCount, lnCurrentParameter, lnPos, lnADOType AS INTEGER
LOCAL loParameter AS ADODB.PARAMETER
LOCAL loException AS EXCEPTION

IF !("?" $ toADOCommnd.CommandText)
	RETURN .T.
ENDIF
lcBuffer = toADOCommnd.CommandText
* 1
lnParametrsCount = ALINES(laCommndPar, lcBuffer, 1 + 4 + 8, "?")
* 2
FOR lnCurrentParameter = 2 TO lnParametrsCount
	lcParameterName = ALLTRIM(laCommndPar[lnCurrentParameter])
	lnPos = ATC(" ", lcParameterName)
	IF lnPos > 0
		lcParameterName = LEFT(lcParameterName, lnPos)
	ENDIF
	IF !EMPTY(lcParameterName)
		lcParameterType = TYPE(lcParameterName)
		IF !INLIST(lcParameterType,"U", "O", "S")
* 3
			DO CASE
				CASE lcParameterType = "A"
					lnADOType = 8192
				CASE lcParameterType = "C"
					lnADOType = 129
				CASE lcParameterType = "D" && Date
					lnADOType = 133
				CASE lcParameterType = "G" && General
					lnADOType = 128
				CASE lcParameterType = "L" && Logical
					lnADOType = 11
				CASE lcParameterType = "M" && Memo
					lnADOType = 200
				CASE lcParameterType = "N" && Numeric, Float, Double, or Integer
					IF (&lcParameterName. - INT(&lcParameterName.)) = 0
						lnADOType = 3 && Integer
					ELSE
						lnADOType = 131
					ENDIF
				CASE lcParameterType = "Q" && Varbinary
					lnADOType = 204
				CASE lcParameterType = "T" &&DateTime
					lnADOType = 133
				CASE lcParameterType = "W" && Blob
					lnADOType = 128
				CASE lcParameterType = "Y" && Currency
					lnADOType = 6
			ENDCASE
			loParameter = .NULL.
			TRY
* 4
				loParameter = toADOCommnd.CreateParameter(lcParameterName, ;
					lnADOType, ;
					1, LEN(TRANSFORM(&lcParameterName.)), &lcParameterName.)
				toADOCommnd.PARAMETERS.APPEND(loParameter)
				toADOCommnd.PARAMETERS(lcParameterName) = &lcParameterName.
				toADOCommnd.CommandText = STRTRAN(toADOCommnd.CommandText, "?" + lcParameterName,"?", 1, 1, 1)
			CATCH TO loException
* Do Nothing
			ENDTRY
		ENDIF
	ENDIF
ENDFOR
ENDFUNC

*-------------------------------------------------------
* Function....: SQLUpdate
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION SQLUpdate(lnConnHandle AS INTEGER, ltADOConnection AS ADODB.CONNECTION, tcClientName AS STRING)

LOCAL lnRez
LOCAL lcViewStr, lcDropViewStr, lcDefValue
LOCAL llRes AS Logical
LOCAL loADOCommand AS ADODB.COMMAND
LOCAL lvDataSource AS Variant
LOCAL lnHowMany, j, lnCurrRecno
LOCAL loProgressBar AS OBJECT
LOCAL lcLabelUpdating

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("SQLUpdate started.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

IF VARTYPE(lnConnHandle) = "N" AND lnConnHandle > 0
	lvDataSource = lnConnHandle
ELSE
	IF VARTYPE(ltADOConnection) = "O" AND ltADOConnection.State > 0
		lvDataSource = ltADOConnection
	ELSE
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** ADO Connection is not open. Update stopped.", goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDIF

lcLabelUpdating = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_LBLUPDATING, CAP_LBLUPDATING)
IF TYPE("tcClientName") == "C"
	lcLabelUpdating = lcLabelUpdating + " " + tcClientName
ENDIF
tofoxapp = goProgram

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting Views.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

* Delete All Views
lcViewStr = "Select * from sysobjects Where xType = 'V' and Status > -1"

llRes = ExecuteCommand(lvDataSource, lcViewStr, "crsViewForDelete", .T.)
IF !llRes
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Views information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT crsViewForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGVIEWS, MSG_DELETINGVIEWS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update

*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("Dropping View [" + ALLTRIM(crsViewForDelete.NAME) + "]", goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	lcDropViewStr = "Drop View [" + ALLTRIM(crsViewForDelete.NAME) + "]"
	llRes = ExecuteCommand(lvDataSource, lcDropViewStr, "", .T.)
	IF !llRes
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by dropping View [" + ALLTRIM(crsViewForDelete.NAME) + "] " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsViewForDelete

*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
*End Delete Views

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Delete Stored Procedures", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

*Delete Procedures
LOCAL lcProcStr, lcDropProcStr

lcProcStr = "Select * from sysobjects Where xType = 'P' and Status > -1"
*lnRez = sqlexec(lnConnHandle,lcProcStr , "crsProcForDelete")
llRes = ExecuteCommand(lvDataSource, lcProcStr, "crsProcForDelete", .T.)
IF !llRes
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Stored Procedures information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lcProcStr, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT crsProcForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
	lcDropProcStr = "DROP PROCEDURE [" + ALLTRIM(crsProcForDelete.NAME) + "]"
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGPROCEDURES, MSG_DELETINGPROCEDURES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	llRes = ExecuteCommand(lvDataSource, lcDropProcStr, "", .T.)
	IF !llRes
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by dropping the Stored Procedure [" + ALLTRIM(crsProcForDelete.NAME) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcDropProcStr, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsProcForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
*End Delete Procedures

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting User Defined Functions", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

* Delete Functions
LOCAL lcFuncStr, lcFuncProcStr

lcFuncStr = "Select * from sysobjects Where xType = 'FN' and Status > -1"
llRes = ExecuteCommand(lvDataSource, lcFuncStr, "crsFuncForDelete", .T.)
IF !llRes
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving User Defined Functions information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lcFuncStr, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT crsFuncForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
	lcFuncProcStr = "DROP FUNCTION [" + ALLTRIM(crsFuncForDelete.NAME) + "]"
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGFUNCTIONS, MSG_DELETINGFUNCTIONS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	llRes = ExecuteCommand(lvDataSource, lcFuncProcStr, "", .T.)
	IF !llRes
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by dropping the User Defined Function [" + ALLTRIM(crsFuncForDelete.NAME) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcFuncProcStr, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsFuncForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End delete Functions

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting Constraints, Triggers & Foreign keys", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

* Delete Constraint & Triggers & Foreign key
LOCAL lcConstraint, lctablename, lcTriggerSTR, lcDropTrigger, lcForeignKey, lcDropFK, ;
	lctable
* Delete CHECK constraints
lcCheckConstraint = "select s.Name, c.Name as TblName " + ;
	"from SysObjects s Inner Join SysObjects c on s.Parent_Obj = c.ID " + ;
	"Where s.xType IN ('C', 'UQ')"

IF !ExecuteCommand(lvDataSource, lcCheckConstraint, "crsChechCon", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Constraints information (type C and UQ). Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lcCheckConstraint, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT crsChechCon
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
	SCAN
		lcDropConst = "Alter table [" + ALLTRIM(crsChechCon.TblName) + ;
			"] DROP CONSTRAINT [" + ALLTRIM(crsChechCon.NAME) + "]"
*** Progress Bar for Update
		IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
			j = j + 1
			loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGCONSTRIGFK, MSG_DELETINGCONSTRIGFK), ;
				lnhowmany, j)
			INKEY(.1, "H")
		ENDIF
*** Progress Bar for Update
		IF !ExecuteCommand(lvDataSource, lcDropConst, "", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by dropping the Constraint, Trigger OR Foreign key [" + ALLTRIM(crsChechCon.NAME) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				WriteLogInfo("*** The executed command was: " + lcDropConst, goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
	ENDSCAN
ENDSCAN
USE IN crsChechCon
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Delete CHECK constraint

lctable = "SELECT * From sysobjects Where xType = 'U' and Status > -1"
IF !ExecuteCommand(lvDataSource, lctable, "crsOnlyTable", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Constraints, Triggers OR Foreign keys information (type U). Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lctable, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT("crsOnlyTable")
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGCONSTRIGFK, MSG_DELETINGCONSTRIGFK), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update

&& Delete Constraint and Foreign keys
	lctablename = ALLTRIM(crsOnlyTable.NAME)
	lcConstraint = "sp_fkeys @fktable_name= [" + lctablename + "]"
	IF !ExecuteCommand(lvDataSource, lcConstraint, "crsAllConstr1", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by retrieving Foreign keys information. Update stopped. Table " + lctablename + " Error: " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcConstraint, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	SELECT DISTINCT FK_Name FROM crsAllConstr1 INTO CURSOR crsAllConstr
	USE IN crsAllConstr1
	SELECT crsAllConstr
	SCAN
		lcDropConst = "Alter table [" + lctablename + ;
			"] DROP CONSTRAINT [" + ALLTRIM(crsAllConstr.fk_name) + "]"
		IF !ExecuteCommand(lvDataSource, lcDropConst, "", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by dropping the Foreign key [" + ALLTRIM(crsAllConstr.fk_name) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				WriteLogInfo("*** The executed command was: " + lcDropConst, goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
	ENDSCAN
	USE IN crsAllConstr
* End delete Constraint

* Delete Triggers
	lcTriggerSTR = "sp_helptrigger [" + lctablename +"]"
	IF !ExecuteCommand(lvDataSource, lcTriggerSTR, "crsAllTriggers", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by retrieving Triggers information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcTriggerSTR, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	SELECT crsAllTriggers
	SCAN
		lcDropTrigger = "DROP TRIGGER [" + ALLTRIM(crsAllTriggers.trigger_name) + "]"
		IF !ExecuteCommand(lvDataSource, lcDropTrigger, "", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by dropping the Trigger [" + ALLTRIM(crsAllTriggers.trigger_name) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				WriteLogInfo("*** The executed command was: " + lcDropTrigger, goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
	ENDSCAN
	USE IN crsAllTriggers
* End delete Triggers
ENDSCAN
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update

*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGCONSTRIGFK, MSG_DELETINGCONSTRIGFK), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update

	lctablename = ALLTRIM(crsOnlyTable.NAME)
	lcConstraint = "sp_pkeys @table_name= [" + lctablename + "]"
	IF !ExecuteCommand(lvDataSource, lcConstraint, "tmp_crsAllPK", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by retrieving Primary Key information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcConstraint, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	SELECT DISTINCT pk_name FROM tmp_crsAllPK INTO CURSOR crsAllPK
	USE IN tmp_crsAllPK
	SELECT crsAllPK
	SCAN
		lcDropConst = "Alter table [" + lctablename + ;
			"] DROP CONSTRAINT [" + ALLTRIM(crsAllPK.pk_name) + "]"
		IF !ExecuteCommand(lvDataSource, lcDropConst, "", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by dropping the PK Constraint [" + ALLTRIM(crsAllPK.pk_name) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				WriteLogInfo("*** The executed command was: " + lcDropConst, goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
	ENDSCAN
	USE IN crsAllPK
ENDSCAN
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting Indexes", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Drop Index
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGINDEXES, MSG_DELETINGINDEXES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update

	lctablename = ALLTRIM(crsOnlyTable.NAME)
	lcIndex = "sp_helpindex @objname = [" + lctablename + "]"
	IF USED("tmp_crsAllIndex")
		USE IN tmp_crsAllIndex
	ENDIF
	IF !ExecuteCommand(lvDataSource, lcIndex, "tmp_crsAllIndex", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by retrieving Indexes information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcIndex, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	IF USED("tmp_crsAllIndex")
		SELECT tmp_crsAllIndex
		SCAN
			lcDropIndex = "DROP INDEX [" + lctablename + "].[" + ALLTRIM(tmp_crsAllIndex.index_name) + "]"
			IF !ExecuteCommand(lvDataSource, lcDropIndex, "", .T.)
*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by dropping the Index [" + ALLTRIM(tmp_crsAllIndex.index_name) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
					WriteLogInfo("*** The executed command was: " + lcDropIndex, goProgram.cUpdateLogFileName)
				ENDIF
*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF
		ENDSCAN
		USE IN tmp_crsAllIndex
	ENDIF
ENDSCAN
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Drop Index

*{V&U DL 2009-09-03
*Move this code to execute early. Can't change size of column which have default constraint
*DROP Constraint
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Drop default constraint.", goProgram.cUpdateLogFileName)
ENDIF
LOCAL lcDefValue
lcDelDef = "Select s.Name, c.Name as TblName " + ;
	"from SysObjects s Inner Join SysObjects c on s.Parent_Obj = c.ID " + ;
	"Where s.xType = 'D' and c.XType = 'U'"
IF !ExecuteCommand(lvDataSource, lcDelDef, "crsDelDef", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Column Defaults information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lcDelDef, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT crsDelDef
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
	lcDromDef = "ALTER TABLE [" + ALLTRIM(crsDelDef.TblName) + ;
		"] DROP CONSTRAINT [" + ALLTRIM(crsDelDef.NAME) + "]"

	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CHANGECOLDEFVALUES, MSG_CHANGECOLDEFVALUES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF

	IF !ExecuteCommand(lvDataSource, lcDromDef, "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + lcDromDef + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsDelDef
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* END DROP Constraint
*}V&U DL 2009-09-03

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting Tables and fields", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Delete Table and Fields
LOCAL lcDropColumn, lcDropTable

*{ DL 2008-08-22 move to execute first because
*when drop table column is mark for delete but it is imosible to delete all column from table
*First drop table and then when try to drop column sp_column return 0 row and continue
SELECT DISTINCT ot.NAME FROM crsOnlyTable ot LEFT OUTER JOIN ;
	crsOneDB odb ON UPPER(ALLTRIM(ot.NAME)) == UPPER(ALLTRIM(odb.Tbl_Name));
	WHERE ISNULL(odb.Tbl_Name) INTO CURSOR crsTableForDel
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGTABLESFIELDS, MSG_DELETINGTABLESFIELDS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	lcDropTable = "drop table [" + ALLTRIM(crsTableForDel.NAME) + "]"
	IF !ExecuteCommand(lvDataSource, lcDropTable, "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by dropping the table [" + ALLTRIM(crsTableForDel.NAME) + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcDropTable, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsTableForDel
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
*} DL 2008-08-22 move

SELECT crsOnlyTable
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGTABLESFIELDS, MSG_DELETINGTABLESFIELDS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	lctablename = ALLTRIM(crsOnlyTable.NAME)
	lcColumns =  "sp_columns [" + lctablename + "]"
	IF !ExecuteCommand(lvDataSource, lcColumns, "crsAllColumns", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by retrieving columns for table [" + lctablename + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcColumns, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	SELECT ac.COLUMN_NAME FROM crsAllColumns ac LEFT OUTER JOIN ;
		crsOneDB odb ON UPPER(ALLTRIM(ac.COLUMN_NAME)) == UPPER(ALLTRIM(odb.Field_Name)) ;
		AND UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(lctablename) ;
		WHERE ISNULL(odb.Field_Name) ;
		INTO CURSOR crsColumnForDel
	USE IN CRSALLCOLUMNS
	SCAN
*{ JEI DL 2008-08-21
*If column have default value sql add constraint and when try to drop the column throw exception
*Check if column have constraint first delete them and them drop column
		lnCurrRecno = RECNO()
		*V&U DL 2011-03-21 add parent_obj field
		lcConstraint = "select sc.name as ColumnName, c.parent_obj, c.Name as ConstraintName, c.xType as ConstraintXType, sc.id as TableID, "+;
						"so.Name as TableName " + ;
			"from syscolumns sc " + ;
			"inner join sysobjects so on so.id = sc.id " + ;
			"inner join sysobjects c on c.id = sc.cdefault " + ;
			"where sc.name = '" + ALLTRIM(COLUMN_NAME) + "' " + ;
			" and so.Name = '" + lctablename +"' and so.xType = 'U' and c.xType ='D' "

		IF !ExecuteCommand(lvDataSource, lcConstraint, "crsConstraintForDropColumn", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error retrieving Constraints information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				WriteLogInfo("*** The executed command was: " + lcConstraint, goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
		SELECT crsConstraintForDropColumn
		IF RECCOUNT("crsConstraintForDropColumn") > 0
			*{ V&U DL 2011-03-21
			*It is possible to has default value that is not a constraint.
			*Default constraint has parent_obj = tableid.
			IF NVL(parent_obj, 0) > 0
				lcDropConst = "alter table [" + lctablename + "] drop constraint [" + ALLTRIM(ConstraintName) + "]"
			ELSE
				lcDropConst = "sp_unbindefault '" + lctablename + "." + ALLTRIM(ColumnName) + "'"
			ENDIF
			*} V&U DL 2011-03-21
			IF !ExecuteCommand(lvDataSource, lcDropConst, "", .T.)
	*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by executing statement " + lcDropConst + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
					WriteLogInfo("*** The executed command was: " + lcDropConst, goProgram.cUpdateLogFileName)
				ENDIF
	*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF
			lcDropConst = ""
		ENDIF
		
		IF USED("crsConstraintForDropColumn")
			USE IN "crsConstraintForDropColumn"
		ENDIF
		SELECT crsColumnForDel
		GO lnCurrRecno
*} JEI DL 2008-08-21
		lcDropColumn = "alter table [" + lctablename + "] drop column [" + ALLTRIM(COLUMN_NAME) + "]"
		IF !ExecuteCommand(lvDataSource, lcDropColumn, "", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing statement " + lcDropColumn + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				WriteLogInfo("*** The executed command was: " + lcDropColumn, goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
	ENDSCAN
	USE IN CRSCOLUMNFORDEL
ENDSCAN
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End delete Table and Fields

LOCAL lcUDT, lcComma
SELECT Tbl_Name FROM crsOneDB WHERE Spec_Type = 5 INTO CURSOR crsUDT
lcComma = ""
lcUDT = ""
SCAN
	lcUDT = lcUDT + lcComma + "'" + UPPER(ALLTRIM(crsUDT.Tbl_Name)) + "'"
	lcComma = ","
ENDSCAN
USE IN crsUDT

IF !SQLXMLSchemaCollectionUpdate(lvDataSource, "crsOneDB")
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by executing SQLXMLSchemaCollectionUpdate. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF

* Modify Table stru
*{V&U MS 2013-04-24 6163
lcNoWidth = "'BIGINT','BIT','DATETIME','DATE','FLOAT','IMAGE','INT','MONEY','NTEXT','REAL'" + ;
	",'SMALLDATETIME','SMALLINT','SMALLMONEY','SQL_VARIANT','TEXT','TIMESTAMP'," + ;
	"'TINYINT','UNIQUEIDENTIFIER','XML'" +  IIF(!EMPTY(lcUDT), "," + lcUDT,"")
*}V&U MS 2013-04-24

SELECT DISTINCT  odb.Tbl_Name FROM crsOneDB odb ;
	inner JOIN crsOnlyTable ot ON UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(ALLTRIM(ot.NAME)) ;
	INTO CURSOR crsExistTable

USE IN crsOnlyTable
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
LOCAL llChangeToIdentity
SCAN && For every Table
	lctablename = ALLTRIM(crsExistTable.Tbl_Name)
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_MODIFYINGTABLESSTRUCTURE, MSG_MODIFYINGTABLESSTRUCTURE) + ;
			" " + lctablename, lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update

	lcGetColumns = "sp_columns [" + lctablename + "]"
	IF !ExecuteCommand(lvDataSource, lcGetColumns, "crsSQLColumn1", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing " + lcGetColumns + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	IF !ExecuteCommand(lvDataSource, "sp_help '" + lctablename + "'", "crsSQLColumnInfo", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing sp_help '" + lctablename + "' Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF

	USE IN crsSQLColumnInfo
	USE IN crsSQLColumnInfo2
	USE IN crsSQLColumnInfo3
	USE IN crsSQLColumnInfo4
	IF USED("crsSQLColumnInfo5")
		USE IN crsSQLColumnInfo5
	ENDIF

*{V&U MS 2009-09-04, Compare Type instead Type_name (Avoid differences coming from IDENTITY)
	SELECT sc.Column_Name, sc.PRECISION, sc.SCALE, sc.Column_Def, sc.Table_Name, sci.TYPE, sci.LENGTH, ;
		sc.Type_Name ;
		FROM crsSQLColumn1 sc ;
		INNER JOIN crsSQLColumnInfo1 sci ON LOWER(ALLTRIM(sc.Column_Name)) == LOWER(ALLTRIM(sci.Column_Name)) ;
		INTO CURSOR crsSQLColumn READWRITE

	USE IN crsSQLColumn1
	USE IN crsSQLColumnInfo1

	SELECT crsSQLColumn
	REPLACE Column_Def WITH STRTRAN(NVL(Column_Def,""), CHR(0),"") ALL
	REPLACE	PRECISION WITH -1 FOR LENGTH = -1

*{ V&U MS 2010-05-19, Compare Type_name instead of Type. Do not avoid IDENTITY change	
	SELECT odb. * FROM crsOneDB odb ;
		INNER JOIN crsSQLColumn sc ON UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(sc.Table_Name) AND ;
		UPPER(ALLTRIM(odb.Field_Name)) == UPPER(sc.COLUMN_NAME) AND ;
		odb.Field_Num < 900 ;
		WHERE ;
		(UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(lctablename) AND ;
		(!(UPPER(ALLTRIM(odb.Field_Type)) == UPPER(ALLTRIM(sc.Type_Name))) OR ;
		odb.Field_len <> sc.PRECISION OR ;
		odb.Field_Dec <> NVL(sc.SCALE, 0)) ;
		AND odb.Spec_Type = 0) ;
		OR (UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(lctablename) AND UPPER(ALLTRIM(odb.Field_Type)) == "XML") ;
		INTO CURSOR crsColumnsForChange
*}V&U MS 2009-09-04

	SCAN
*{V&U MS 2009-09-04, Use Type instead Field_Type (can include IDENTITY which give errors with ALTER)
* MS 2010-05-19, turned back to Field_Type 
		lcType = ALLTRIM(crsColumnsForChange.Field_Type)
*}V&U MS 2009-09-04
		llChangeToIdentity = .F.
		DO CASE 
			CASE UPPER(lcType) == "INT IDENTITY"
				llChangeToIdentity = .T.
			CASE crsColumnsForChange.Field_len = -1 AND INLIST(UPPER(lcType), "XML", "NVARCHAR", "VARBINARY", "VARCHAR", "SYSNAME")
				lcType = lcType + ALLTRIM(crsColumnsForChange.specdescr)
			OTHERWISE 
				IF !INLIST(UPPER(lctype), &lcNoWidth.)
					lctype = lctype + "(" + TRANSFORM(Field_len) + ;
						IIF(Field_Dec > 0, "," + TRANSFORM(Field_Dec),"") + ")"
				ENDIF
		ENDCASE
		IF !llChangeToIdentity 
			lcAlterTable = "Alter Table [" + lctablename + ;
				"] Alter column [" + ALLTRIM(crsColumnsForChange.Field_Name) + "] " + ;
				lctype + " " + IIF(AlNull > 0, "NULL", "NOT NULL")
			IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
	*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by executing " + lcAlterTable + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
				ENDIF
	*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF
		ENDIF 
	ENDSCAN
	
	* Create table string
	IF llChangeToIdentity 
		LOCAL lcCreateTable, lcInsertTable, lcFields 

		* Create table string
		lcCreateTable = [CREATE TABLE dbo.Tmp_] + lcTableName + [ ( ]
		lcInsertTable = [IF EXISTS(SELECT * FROM dbo.] + lcTableName + [) ] + ;
						[EXEC('INSERT INTO dbo.Tmp_] + lcTableName + [(]
		lcFields = []
		llChangeToIdentity = .F.

		SELECT odb. * FROM crsOneDB odb ;
			WHERE ;
			UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(lcTableName) AND ;
			odb.Spec_Type = 0 AND odb.Field_Num < 900 ;
			INTO CURSOR crsColumnsToCreate
		SCAN 
			lcType = ALLTRIM(crsColumnsToCreate.Field_Type)
			DO CASE 
				CASE UPPER(lcType) == "INT IDENTITY"
					lcType = lcType 
				CASE crsColumnsToCreate.Field_len = -1 AND INLIST(UPPER(lcType), "XML", "NVARCHAR", "VARBINARY", "VARCHAR", "SYSNAME")
					lcType = lcType + ALLTRIM(crsColumnsToCreate.SpecDescr)
				OTHERWISE 
					IF !INLIST(UPPER(lcType), &lcNoWidth.)
						lcType = lcType + "(" + TRANSFORM(Field_len) + ;
							IIF(Field_Dec > 0, "," + TRANSFORM(Field_Dec), "") + ")"
					ENDIF
			ENDCASE 
			lcCreateTable = lcCreateTable + "[" + ALLTRIM(crsColumnsToCreate.Field_Name) + "] " + ;
							lcType + " " + IIF(AlNull > 0, "NULL", "NOT NULL") + ", "
			lcFields = lcFields + "[" + ALLTRIM(crsColumnsToCreate.Field_Name) + "], "
		ENDSCAN
		lcCreateTable = SUBSTR(lcCreateTable, 1, LEN(lcCreateTable) - 2) + ") ON [PRIMARY]"
		lcInsertTable = lcInsertTable + SUBSTR(lcFields, 1, LEN(lcFields) - 2) + ;
						") " + ;
						[ SELECT ] + SUBSTR(lcFields, 1, LEN(lcFields) - 2) + ;
						[ FROM dbo.] + lcTableName + [ WITH (HOLDLOCK TABLOCKX)')]
		USE IN crsColumnsToCreate
		IF !ExecuteCommand(lvDataSource, lcCreateTable, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing " + lcCreateTable + " Update stopped. " + ;
				MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
		ENDIF 
		lcSetIdentity = [SET IDENTITY_INSERT dbo.Tmp_] + lctablename + [ ON]
		IF !ExecuteCommand(lvDataSource, lcSetIdentity, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing " + lcSetIdentity + " Update stopped. " + ;
				MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
		ENDIF		
		IF !ExecuteCommand(lvDataSource, lcInsertTable, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing " + lcInsertTable + " Update stopped. " + ;
				MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
		ENDIF 
		lcSetIdentity = [SET IDENTITY_INSERT dbo.Tmp_] + lcTableName + [ OFF]
		IF !ExecuteCommand(lvDataSource, lcSetIdentity, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing " + lcSetIdentity + " Update stopped. " + ;
				MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
		ENDIF
		lcDropTable = [DROP TABLE dbo.] + lcTableName
		IF !ExecuteCommand(lvDataSource, lcDropTable, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing " + lcDropTable + " Update stopped. " + ;
				MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
		ENDIF
		lcRenameTable = [EXECUTE sp_rename N'dbo.Tmp_] + lcTableName + ;
						[', N'] + lcTableName + [', 'OBJECT'] 					
		IF !ExecuteCommand(lvDataSource, lcRenameTable, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing " + lcRenameTable + " Update stopped. " + ;
				MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
		ENDIF
	ENDIF 	
*} V&U MS 2010-05-19	
	USE IN crsColumnsForChange
ENDSCAN
USE IN CRSEXISTTABLE
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End modify Table stru

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting User Defined Data Types.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Delete User Defined Data Types
lcUDF = "Select Name From SysTypes Where xType <> xUserType and xUserType > 256"
IF !ExecuteCommand(lvDataSource, lcUDF, "crsAllUDT", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving User Defined Data Types. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lcUDF, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT * FROM crsAllUDT audt LEFT OUTER JOIN crsOneDB odb ;
	ON UPPER(ALLTRIM(audt.NAME)) == UPPER(ALLTRIM(odb.Field_Name)) ;
	WHERE odb.Spec_Type = 5 AND ISNULL(odb.Field_Name) ;
	INTO CURSOR crsUDTForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGUSERTYPES, MSG_DELETINGUSERTYPES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	lcDropDef = "sp_droptype [" + ALLTRIM(crsUDTForDelete.NAME) + "]"
	IF !ExecuteCommand(lvDataSource, lcDropDef, "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing " + lcDropDef + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsUDTForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Delete User Defined Data Types

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting Defaults.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Delete Default
sql_str =' select name,id from sysobjects o ' + ;
	" where o.xtype='D' and o.parent_obj = 0 "
IF !ExecuteCommand(lvDataSource, sql_str, "crsAllDef", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Defaults information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + sql_str, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT ad. * FROM crsAllDef ad LEFT OUTER JOIN crsOneDB odb ;
	ON UPPER(ALLTRIM(ad.NAME)) == UPPER(ALLTRIM(odb.Tbl_Name)) ;
	WHERE odb.Spec_Type = 3 AND ISNULL(odb.Tbl_Name) ;
	INTO CURSOR crsDefForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGDEFAULTS, MSG_DELETINGDEFAULTS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, "DROP DEFAULT [" + ALLTRIM(crsDefForDelete.NAME) +"]", "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by dropping default [" + crsDefForDelete.NAME + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsDefForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End delete Default

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Deleting Rules.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Delete Rule
sql_str =' select name,id from sysobjects o ' + ;
	" where o.xtype='R' and o.status > - 1 "
IF !ExecuteCommand(lvDataSource, sql_str, "crsAllRul", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Rules information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + sql_str, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF
SELECT ad. * FROM crsAllRul ad LEFT OUTER JOIN crsOneDB odb ;
	ON UPPER(ALLTRIM(ad.NAME)) == UPPER(ALLTRIM(odb.Tbl_Name)) ;
	WHERE odb.Spec_Type = 4 AND ISNULL(odb.Tbl_Name) ;
	INTO CURSOR crsRulForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DELETINGRULES, MSG_DELETINGRULES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, "DROP RULE [" + ALLTRIM(crsRulForDelete.NAME) + "]", "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by dropping the rule [" + crsRulForDelete.NAME + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsRulForDelete
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Delete Rule

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating Rules.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* CREATE Rule
SELECT odb. * FROM crsAllRul ad RIGHT OUTER JOIN crsOneDB odb ;
	ON UPPER(ALLTRIM(ad.NAME)) == UPPER(ALLTRIM(odb.Tbl_Name)) ;
	WHERE odb.Spec_Type = 4 AND ISNULL(ad.NAME) ;
	INTO CURSOR crsRulForCreat
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CREATINGRULES, MSG_CREATINGRULES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, ALLTRIM(crsRulForCreat.specdescr), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by creating rule [" + crsRulForCreat.specdescr + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsRulForCreat
USE IN crsAllRul
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End create rule

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating Defaults.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Create Default
SELECT odb. * FROM crsAllDef ad RIGHT OUTER JOIN crsOneDB odb ;
	ON UPPER(ALLTRIM(ad.NAME)) == UPPER(ALLTRIM(odb.Field_Name)) ;
	WHERE odb.Spec_Type = 3 AND ISNULL(ad.NAME) ;
	INTO CURSOR crsDefCreate
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CREATINGDEFAULTS, MSG_CREATINGDEFAULTS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource,  ALLTRIM(crsDefCreate.specdescr), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by creating default [" + crsDefCreate.specdescr + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsDefCreate
USE IN crsAllDef
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Create Default

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating User Defined Data Types.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Create User Defined Data Types

SELECT odb. * FROM crsAllUDT ad RIGHT OUTER JOIN crsOneDB odb ;
	ON UPPER(ALLTRIM(ad.NAME)) == UPPER(ALLTRIM(odb.Tbl_Name)) ;
	WHERE odb.Spec_Type = 5 AND ISNULL(ad.NAME) ;
	INTO CURSOR crsDefCreate
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CREATINGUSERTYPES, MSG_CREATINGUSERTYPES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, ALLTRIM(crsDefCreate.specdescr), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by creating User Defined Data Type [" + crsDefCreate.specdescr + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsDefCreate
USE IN crsAllUDT
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Create User Defined Data Types

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating New Fields.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Add Fields
lcCilumn = "select sc.Name as Column_Name, so.Name as Table_Name from SysColumns sc " + ;
	"Inner Join SysObjects so on sc.ID = so.ID " + ;
	"Where so.xType = 'U' and so.Status > -1 "

IF !ExecuteCommand(lvDataSource, lcCilumn, "crsSQLColumn", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Fields information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lcCilumn, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF

SELECT DISTINCT UPPER(sc.Table_Name) AS Table_Name FROM crsOneDB odb inner JOIN crsSQLColumn sc ON ;
	UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(ALLTRIM(sc.Table_Name)) ;
	INTO CURSOR crsExistTables

SELECT odb. * FROM crsOneDB odb FULL OUTER JOIN crsSQLColumn sc ON ;
	UPPER(ALLTRIM(odb.Field_Name)) == UPPER(ALLTRIM(sc.COLUMN_NAME)) AND ;
	UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(ALLTRIM(sc.Table_Name)) ;
	inner JOIN crsExistTables et ON UPPER(ALLTRIM(odb.Tbl_Name)) == ALLTRIM(et.Table_Name) OR ;
	UPPER(ALLTRIM(sc.Table_Name)) == ALLTRIM(et.Table_Name) ;
	WHERE odb.Field_Num < 900 AND ISNULL(sc.COLUMN_NAME) INTO CURSOR crsNewColumns

USE IN crsExistTables
USE IN CRSSQLCOLUMN
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGNEWCOLUMNS, MSG_ADDINGNEWCOLUMNS) + ;
			" " + ALLTRIM(crsNewColumns.Tbl_Name), lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update

	lcTableName = ALLTRIM(crsNewColumns.Tbl_Name)
	lcFieldName = ALLTRIM(crsNewColumns.Field_Name)
	lcDataType =  UPPER(ALLTRIM(crsNewColumns.Field_Type))
	IF crsNewColumns.Field_Len = -1 AND INLIST(UPPER(lcDataType), "XML", "NVARCHAR", "VARBINARY", "VARCHAR", "SYSNAME")
		lcDataType = lcDataType + ALLTRIM(specdescr)
	ELSE
		IF !INLIST(lcDataType, &lcNoWidth )
			lcDataType = lcDataType + "(" + TRANSFORM(crsNewColumns.Field_len) + ;
				IIF(crsNewColumns.Field_Dec > 0,", " + TRANSFORM(crsNewColumns.Field_Dec),"") + ")"
		ENDIF
	ENDIF

	lcAlterTable = "ALTER TABLE [" + lcTableName + "] ADD [" + lcFieldName + "] " + lcDataType
	IF EMPTY(ALLTRIM(DEFVALUE))
*{V&U VJ 2009-02-21
		IF ATC("IDENTITY", UPPER(lcDataType)) > 0
			*{V&U MS 2010-05-18, Identity column can be added without null (even if table has data)
			IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
	*{V&U DL 2008-08-22 Type cannot not be identity. "identity null" is incorect syntax in sql
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error is raised adding IDENTITY Field " + ;
								"[" + lcFieldName + "] in table [" +  lcTableName + "]. Update stopped. " + ;
								MESSAGE(), goProgram.cUpdateLogFileName)
				ENDIF
				RETURN .F.
	*}V&U VJ 2009-02-19
			ENDIF 	
			*}V&U MS 2010-05-18
		ELSE
			lcAlterTable = lcAlterTable + " NULL"		&& ALTER TABLE cannot add columns that are NOT NULL and don't have DEFAULT value!!!
			WriteLogInfo("Executing " + lcAlterTable, goProgram.cUpdateLogFileName)
			IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by defining the field [" + lcFieldName + "] in table [" + lcTableName + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
					WriteLogInfo("*** The executed command was: " + lcAlterTable, goProgram.cUpdateLogFileName)
				ENDIF
*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF
		ENDIF
	ELSE
		IF AT("SP_BINDEFAULT", UPPER(DEFVALUE)) > 0
&& Case 1: sp_bindefault <defualt value>, '[<table name>].[field name]'

*{V&U VJ 2009-02-21
* the code is changes, bceause the syntax
* ALTER TABLE [myTable] ADD [NewField] NOT NULL DEFAULT 'UW_ZeroDefault'
*requires default value and not default name
* The code ic changed to create NULL column, then if necessary, to apply default value and to change the field to NOT NULL

			lcAlterTable = lcAlterTable  + " NULL"

			WriteLogInfo("Executing " + lcAlterTable, goProgram.cUpdateLogFileName)
			IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by defining the field [" + lcFieldName + "] in table [" + lcTableName + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
					WriteLogInfo("*** The executed command was: " + lcAlterTable, goProgram.cUpdateLogFileName)
				ENDIF
*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF
			WriteLogInfo("Executing " + ALLTRIM(DEFVALUE), goProgram.cUpdateLogFileName)
			IF !ExecuteCommand(lvDataSource, ALLTRIM(DEFVALUE), "", .T.)
*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by defining the field [" + lcFieldName + "] in table [" + lcTableName + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
					WriteLogInfo("*** The executed command was: " + lcAlterTable, goProgram.cUpdateLogFileName)
				ENDIF
*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF

			IF AlNull = 0
*	If field must be NOT NULL, first apply the default on existing data
				lcAlterTable = "UPDATE [" +  lcTableName + "] SET [" + lcFieldName + "] = DEFAULT"
				WriteLogInfo("Executing " + lcAlterTable, goProgram.cUpdateLogFileName)
				IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
*{V&U VJ 2009-02-19
					IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
						WriteLogInfo("*** An error occured by defining the field [" + lcFieldName + "] in table [" + lcTableName + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
						WriteLogInfo("*** The executed command was: " + lcAlterTable, goProgram.cUpdateLogFileName)
					ENDIF
*}V&U VJ 2009-02-19
					RETURN .F.
				ENDIF

				lcAlterTable = "ALTER TABLE [" +  lcTableName + "] ALTER COLUMN [" + lcFieldName + "] " + lcDataType + " NOT NULL"
				WriteLogInfo("Executing " + lcAlterTable, goProgram.cUpdateLogFileName)
				IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
*{V&U VJ 2009-02-19
					IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
						WriteLogInfo("*** An error occured by defining the field [" + lcFieldName + "] in table [" + lcTableName + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
						WriteLogInfo("*** The executed command was: " + lcAlterTable, goProgram.cUpdateLogFileName)
					ENDIF
*}V&U VJ 2009-02-19
					RETURN .F.
				ENDIF
			ENDIF
*}V&U VJ 2009-02-21

		ELSE
&& Case 2: ALTER TABLE [<table name>] ADD CONSTRAINT [<constraint name>] DEFAULT (<default value>) FOR <field name>
			LOCAL lnPosDefault, lnPosFor
			lcDefaultValue = ALLTRIM(DEFVALUE)
			lnPosDefault 	 = AT(" DEFAULT ", lcDefaultValue, 1)
			lnPosFor 		 = AT(" FOR ", lcDefaultValue, 1)
			IF lnPosFor = 0
				lnPosFor = LEN(lcDefaultValue)
			ENDIF
			lcDefaultValue = ALLTRIM(IIF(lnPosDefault > 0, ;
				SUBSTR(lcDefaultValue, lnPosDefault + 9, lnPosFor - (lnPosDefault + 9)),""))
			lcAlterTable = lcAlterTable  + IIF(AlNull > 0, " NULL ", " NOT NULL") + ;
				IIF(!EMPTY(lcDefaultValue)," DEFAULT " + lcDefaultValue,"")
			WriteLogInfo("Executing " + lcAlterTable, goProgram.cUpdateLogFileName)
			IF !ExecuteCommand(lvDataSource, lcAlterTable, "", .T.)
*{V&U VJ 2009-02-19
				IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
					WriteLogInfo("*** An error occured by defining the field [" + lcFieldName + "] in table [" + lcTableName + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
					WriteLogInfo("*** The executed command was: " + lcAlterTable, goProgram.cUpdateLogFileName)
				ENDIF
*}V&U VJ 2009-02-19
				RETURN .F.
			ENDIF
		ENDIF
	ENDIF

ENDSCAN
USE IN crsNewColumns
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End add Fields

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating New Tables.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Add new tables
lctables = "Select Name from SysObjects Where xType = 'U' and Status > -1"
IF !ExecuteCommand(lvDataSource, lctables, "crsAllSQLTable", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by retrieving Tables information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		WriteLogInfo("*** The executed command was: " + lctables, goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF

SELECT DISTINCT Tbl_Name FROM crsOneDB odb LEFT OUTER JOIN crsAllSQLTable ast ;
	ON UPPER(ALLTRIM(odb.Tbl_Name)) == UPPER(ALLTRIM(ast.NAME)) ;
	WHERE ISNULL(ast.NAME) AND odb.Spec_Type = 0 AND odb.Field_Num < 900 ;
	INTO CURSOR crsNewTables

USE IN CRSALLSQLTABLE

LOCAL lctablename
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGNEWTABLES, MSG_ADDINGNEWTABLES) + ;
			" " + ALLTRIM(crsNewTables.Tbl_Name), lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	lctablename = ALLTRIM(crsNewTables.Tbl_Name)
	SELECT * FROM crsOneDB WHERE UPPER(ALLTRIM(Tbl_Name)) == UPPER(lctablename) ;
		INTO CURSOR crsOneTable
	IF !Mk_Table(lctablename, "crsOneTable", lvDataSource, lcUDT)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by creating the table [" + lctablename + "] Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	USE IN crsOneTable
ENDSCAN
USE IN crsNewTables
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End add new tables

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Change Default Column Values.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

* Change Default Column Value
*{ V&U DL 2009-09-03
*Take only necessary constraint. someone constraint may be create from alter table when add new column.
lctables = "SELECT soT.name as tblName,sc.Name as ColName, sc.cDefault, so.Name as NameDF, so.Parent_Obj,  " + ;
	"sm.Text as DFDefinition " + ;
	"From SysColumns sc  " + ;
	"Left Outer Join SysObjects so on sc.cDefault = so.ID and so.xType = 'D'  " + ;
	"LEFT OUTER JOIN syscomments sm on sc.cdefault = sm.id  " + ;
	"AND sm.colid = 1  " + ;
	"left join sysobjects soT on soT.id = sc.ID and soT.xtype='U' and soT.Status>-1 " + ;
	"order by soT.name "
IF !ExecuteCommand(lvDataSource, lctables, "crsAllDefaultInDB", .T.)
*{V&U VJ 2009-02-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("*** An error occured by executing the statement " + lctables + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
	ENDIF
*}V&U VJ 2009-02-19
	RETURN .F.
ENDIF

&&SELECT * FROM crsOneDB WHERE !EMPTY(DEFVALUE) INTO CURSOR crsDefValue
SELECT def. * FROM crsOneDB def ;
	LEFT JOIN crsAllDefaultInDB alld ON UPPER(ALLTRIM(alld.tblName)) == UPPER(ALLTRIM(def.tbl_Name)) AND UPPER(ALLTRIM(alld.ColName)) == UPPER(ALLTRIM(def.Field_name));
	WHERE !EMPTY(DEFVALUE) AND ISNULL(alld.NAmeDF) ;
	INTO CURSOR crsDefValue
*} V&U DL 2009-09-03
j = 0
lnhowmany = RECCOUNT()
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CHANGECOLDEFVALUES, MSG_CHANGECOLDEFVALUES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	
	*{ V&U DL 2009-12-17
	lcTables = "SELECT sc.Name AS ColumnName, so.Name AS TableName, sc.isnullable AS AllowNull " + ;
				"FROM sysColumns sc " + ;
				"INNER JOIN SysObjects so ON so.id = sc.id " + ;
				"WHERE so.xType = 'U' AND so.Name = '" + ALLTRIM(Tbl_name) + "' AND sc.Name = '" + ALLTRIM(Field_name) + "'"
	lnCurrRecno = RECNO()
	IF !ExecuteCommand(lvDataSource, lcTables, "caColumnInformation", .T.)
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + lcTables + " Update stopped. " + ;
						MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF 
		RETURN .F.
	ENDIF
	SELECT "caColumnInformation"
	*If column is null, but in new database structure is not null update all rows where field is null with default value
	IF RECCOUNT() > 0 AND AllowNull = 1 AND crsDefValue.AlNull = 0
		lcTableName = UPPER(ALLTRIM(crsDefValue.Tbl_Name))
		lcFieldName = UPPER(ALLTRIM(crsDefValue.Field_Name))
		lcDefValue = UPPER(ALLTRIM(crsDefValue.DefValue))
		lcDefValue = STRTRAN(lcDefValue, "ALTER TABLE [" + lcTableName + "] ADD CONSTRAINT", "")
		lcDefValue = SUBSTR(lcDefValue, 1, LEN(lcDefValue) - (LEN(lcFieldName) + LEN("FOR ")))
		lcDefValue = SUBSTR(lcDefValue, ATC("]", lcDefValue) + 1)
		lcDefValue = ALLTRIM(STRTRAN(lcDefValue, "DEFAULT", ""))
		
		lcTables = "UPDATE " + lcTableName + " SET " + lcFieldName + "=" + lcDefValue + " WHERE " + lcFieldName + " IS NULL"
		IF !ExecuteCommand(lvDataSource, lctables, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing the statement " + lcTables + " Update stopped. " + ;
							MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF 
			RETURN .F.
		ENDIF
		
		lcDataType = UPPER(ALLTRIM(crsDefValue.Field_Type))
		IF crsDefValue.Field_Len = -1 AND INLIST(UPPER(lcDataType), "XML", "NVARCHAR", "VARBINARY", "VARCHAR", "SYSNAME")
			lcDataType = lcDataType + ALLTRIM(specdescr)
		ELSE
			IF !INLIST(lcDataType, &lcNoWidth)
				lcDataType = lcDataType + "(" + TRANSFORM(crsDefValue.Field_len) + ;
					IIF(crsDefValue.Field_Dec > 0, ", " + TRANSFORM(crsDefValue.Field_Dec), "") + ")"
			ENDIF
		ENDIF
		lctables = "ALTER TABLE " + lcTableName + ;
				   " ALTER COLUMN " + lcFieldName + " " + ;
				   lcDataType + " NOT NULL "
		IF !ExecuteCommand(lvDataSource, lctables, "", .T.)
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing the statement " + lcTables + " Update stopped. " + ;
							MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF 
			RETURN .F.
		ENDIF				   
	ENDIF 
	IF USED("caColumnInformation")
		USE IN caColumnInformation
	ENDIF 
	
	SELECT crsDefValue
	IF lnCurrRecno <> 0 AND lnCurrRecno <= RECCOUNT() AND RECNO() <> lnCurrRecno
		GO lnCurrRecno
	ENDIF
	*} V&U DL 2009-12-17

	IF !ExecuteCommand(lvDataSource, ALLTRIM(crsDefValue.DEFVALUE), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + crsDefValue.DEFVALUE + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsDefValue
*{ V&U DL 2009-09-03
IF USED("crsAllDefaultInDB")
	USE IN "crsAllDefaultInDB"
ENDIF
*} V&U DL 2009-09-03
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Change Default Column Value

* ADD PK
SELECT * FROM crsOneDB WHERE Field_Num = 990 INTO CURSOR crsAllPK
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGPKS, MSG_ADDINGPKS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	lcPK = "Alter Table [" + ALLTRIM(crsAllPK.Tbl_Name) + "] Add " + ;
		ALLTRIM(crsAllPK.specdescr)
	IF !ExecuteCommand(lvDataSource, lcPK, "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + lcPK + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsAllPK
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
*END ADD PK

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating Indexes.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Add Index
SELECT * FROM crsOneDB WHERE is_db = 1 AND Spec_Type = 0 AND Field_Num = 996 ;
	INTO CURSOR crsIndex READWRITE
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGINDEXES, MSG_ADDINGINDEXES) + ;
			" " + ALLTRIM(crsIndex.Tbl_Name), lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	llCreate = .F.
	lctablename = ALLTRIM(crsIndex.Tbl_Name)
	lcIndex = "sp_helpindex @objname = [" + lctablename + "]"
	IF USED("tmp_crsAllIndex")
		USE IN tmp_crsAllIndex
	ENDIF
	IF !ExecuteCommand(lvDataSource, lcIndex, "tmp_crsAllIndex", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by retrieving Index information. Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			WriteLogInfo("*** The executed command was: " + lcIndex, goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
	IF USED("tmp_crsAllIndex")
		SELECT tmp_crsAllIndex
		LOCATE FOR ALLTRIM(UPPER(tmp_crsAllIndex.index_name)) == UPPER(ALLTRIM(crsIndex.Field_Name))
		IF !FOUND()
			llCreate = .T.
		ENDIF
		USE IN tmp_crsAllIndex
	ELSE
		llCreate = .T.
	ENDIF
	IF llCreate = .T.
		IF !ExecuteCommand(lvDataSource, ALLTRIM(crsIndex.specdescr), "", .T.)
*{V&U VJ 2009-02-19
			IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
				WriteLogInfo("*** An error occured by executing the statement " + ALLTRIM(crsIndex.specdescr) + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
			ENDIF
*}V&U VJ 2009-02-19
			RETURN .F.
		ENDIF
	ENDIF
ENDSCAN
USE IN crsIndex
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End Index

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating FK.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Add FK
SELECT * FROM crsOneDB WHERE Field_Num = 994 INTO CURSOR crsAllFK
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGFKS, MSG_ADDINGFKS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	lcFK = "Alter Table [" + ALLTRIM(crsAllFK.Tbl_Name) + "] Add " + ;
		ALLTRIM(crsAllFK.specdescr)
	IF !ExecuteCommand(lvDataSource, lcFK, "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + lcFK + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsAllFK
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* end FK

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating Triggers.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19

* Add Triggers
SELECT * FROM crsOneDB ;
	WHERE is_db = 1 AND Spec_Type = 0 AND INLIST(Field_Num, 913, 914, 915) ;
	INTO CURSOR crsTriggerFromOneConn READWRITE
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGTRIGGERS, MSG_ADDINGTRIGGERS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, ALLTRIM(crsTriggerFromOneConn.specdescr), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + ALLTRIM(crsTriggerFromOneConn.specdescr) + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsTriggerFromOneConn
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End add Trigger

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating Procedures and Function.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Add Procedures and Function
SELECT * FROM crsOneDB ;
	WHERE Spec_Type = 2 INTO CURSOR crsProcFromOneConn READWRITE &&Only Proc
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGPROCEDURES, MSG_ADDINGPROCEDURES), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, ALLTRIM(crsProcFromOneConn.specdescr), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + ALLTRIM(crsProcFromOneConn.specdescr) + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsProcFromOneConn
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End add Procedures

*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("Creating Views.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
* Add View
SELECT * FROM crsOneDB ;
	WHERE Spec_Type = 1 INTO CURSOR crsViewFromOneConn READWRITE &&Only View
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate
	lnHowMany = RECCOUNT()
	j = 0
	IF lnHowMany > 0
		loProgressBar = CREATEOBJECT('czipmtr', lcLabelUpdating)
		loProgressBar.lautoreleaseoncomplete = .F.
		loProgressBar.SHOW()
	ENDIF
ENDIF
*** Progress Bar for Update
SCAN
*** Progress Bar for Update
	IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
		j = j + 1
		loProgressBar.FormUpdate(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ADDINGVIEWS, MSG_ADDINGVIEWS), ;
			lnhowmany, j)
		INKEY(.1, "H")
	ENDIF
*** Progress Bar for Update
	IF !ExecuteCommand(lvDataSource, ALLTRIM(crsViewFromOneConn.specdescr), "", .T.)
*{V&U VJ 2009-02-19
		IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
			WriteLogInfo("*** An error occured by executing the statement " + ALLTRIM(crsViewFromOneConn.specdescr) + " Update stopped. " + MESSAGE(), goProgram.cUpdateLogFileName)
		ENDIF
*}V&U VJ 2009-02-19
		RETURN .F.
	ENDIF
ENDSCAN
USE IN crsViewFromOneConn
*** Progress Bar for Update
IF tofoxapp.lShowProgressOnUpdate AND TYPE("loProgressBar") = "O" AND !ISNULL(loProgressBar)
	loProgressBar.RELEASE()
ENDIF
*** Progress Bar for Update
* End add View
*{V&U VJ 2009-02-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
	WriteLogInfo("SQL Update completed.", goProgram.cUpdateLogFileName)
ENDIF
*}V&U VJ 2009-02-19
RETURN .T.

ENDFUNC

*-------------------------------------------------------
* Function....: SQLXMLSchemaCollectionUpdate
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tvDataSource, tcAliasName
*
* Notes.......:
*-------------------------------------------------------
FUNCTION SQLXMLSchemaCollectionUpdate()
LPARAMETERS tvDataSource AS Variant, tcAliasName AS STRING

LOCAL lcSQL_Str, lcServerVersion AS STRING
LOCAL lnOldWorkArea AS INTEGER

IF !USED(tcAliasName)
	RETURN .F.
ENDIF

lnOldWorkArea = SELECT()
IF !ExecuteCommand(tvDataSource, [sp_server_info @attribute_id = '500'], "crsServerVersion", .T.)
	SELECT(lnOldWorkArea)
	RETURN .F.
ENDIF
SELECT crsServerVersion
LOCATE
lcServerVersion = ALLTRIM(crsServerVersion.Attribute_Value)
USE IN crsServerVersion
IF VAL(LEFT(lcServerVersion, AT(".", lcServerVersion) -1)) <= 8
	SELECT (lnOldWorkArea)
	RETURN .T.
ENDIF
* Get all schemas in database
lcSQL_Str = 	"Select xml_s.Name as SchemaName, col.Name as ColumnName, tbl.Name as TableName " + ;
	"From sys.xml_schema_collections xml_s " + ;
	"Left Join sys.Columns col on xml_s.xml_collection_id = col.xml_collection_id " + ;
	"Left Join sys.Tables tbl on col.object_id = tbl.object_id " + ;
	"Where xml_s.Name <> 'sys'"

IF !ExecuteCommand(tvDataSource, lcSQL_Str,"crsAllXMLSchemas", .T.)
	SELECT (lnOldWorkArea)
	RETURN .F.
ENDIF
* Remove schemas from tables
SELECT crsAllXMLSchemas
SCAN FOR !ISNULL(TableName)
	lcSQl_Str = "Alter Table [" + ALLTRIM(crsAllXMLSchemas.TableName) + "] Alter Column [" + ;
		ALLTRIM(crsAllXMLSchemas.ColumnName) + "] XML"

	IF !ExecuteCommand(tvDataSource, lcSQL_Str,"crsAllXMLSchemas", .T.)
		USE IN crsAllXMLSchemas
		SELECT (lnOldWorkArea)
		RETURN .F.
	ENDIF
ENDSCAN

* Drop Schemas
SELECT DISTINCT SchemaName FROM crsAllXMLSchemas INTO CURSOR crsXMLSchemasForRemove
USE IN crsAllXMLSchemas
SELECT crsXMLSchemasForRemove
SCAN
	lcSQL_Str = "DROP XML SCHEMA COLLECTION [" + ALLTRIM(crsXMLSchemasForRemove.SchemaName) + "]"
	IF !ExecuteCommand(tvDataSource, lcSQL_Str,"", .T.)
		USE IN crsXMLSchemasForRemove
		SELECT (lnOldWorkArea)
		RETURN .F.
	ENDIF
ENDSCAN

* Get XML Schema Collection
SELECT * FROM (tcAliasName) WHERE spec_type = 6 INTO CURSOR crsNewXMLSchema
SELECT crsNewXMLSchema
SCAN
* Create XML Schema Collection
	IF !ExecuteCommand(tvDataSource, ALLTRIM(crsNewXMLSchema.specdescr),"", .T.)
		USE IN crsNewXMLSchema
		SELECT (lnOldWorkArea)
		RETURN .F.
	ENDIF
ENDSCAN

USE IN crsNewXMLSchema
SELECT (lnOldWorkArea)
ENDFUNC

*-------------------------------------------------------
* Function....: Mk_Table
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: pDbfName, pCursor_Ddict, lnConnHandle, lcUDT
*
* Notes.......:
*-------------------------------------------------------
FUNCTION Mk_Table(pDbfName, pCursor_Ddict, lnConnHandle, lcUDT)
*  creates table using  cursor containing  the definition

LOCAL lc_Tbnm, ; && full table name
lc_Crea_Str, ;  && full command create table
lcComma

LOCAL lnTagNumber, lnCurrentTagNum, lcCurrentTagName
LOCAL lcFieldType AS STRING

pDbfName = ALLTRIM(pDbfName)

lc_Crea_Str ='create table [' + pDbfName + "]"

lc_Crea_Str = lc_Crea_Str +' ( '
SELECT(pCursor_Ddict)
lcNoWidth = "'BIGINT','BIT','DATETIME','DATE','FLOAT','IMAGE','INT','MONEY','NTEXT','REAL'" + ;
	",'SMALLDATETIME','SMALLINT','SMALLMONEY','SQL_VARIANT','TEXT','TIMESTAMP'," + ;
	"'TINYINT','UNIQUEIDENTIFIER','XML', 'SYSNAME'" + IIF(!EMPTY(lcUDT),"," + lcUDT,"")
lcComma = " "
SCAN FOR Field_Num < 900 AND !EMPTY(Field_Name)
** for every field
	lc_Crea_Str = lc_Crea_Str + lcComma + "[" + RTRIM(Field_Name) +'] '
	lcFieldType = ALLTRIM(Field_Type)
	IF Field_Len = -1
		lcFieldType = lcFieldType + ALLTRIM(specdescr)
	ENDIF
	lc_Crea_Str = lc_Crea_Str + lcFieldType
	connn ="!Inlist('" + UPPER(ALLTRIM(Field_Type)) +"'," + lcNoWidth  +")"
	IF &connn
		IF Field_len > 0 OR ISNULL(Field_len)
			lc_Crea_Str = lc_Crea_Str +'(' + TRANSFORM(Field_len)
			IF Field_Dec#0
				lc_Crea_Str = lc_Crea_Str +',' + TRANSFORM(Field_Dec)
			ENDIF
			lc_Crea_Str = lc_Crea_Str +')'
		ENDIF
	ENDIF

	IF AlNull = 1
		lc_Crea_Str = lc_Crea_Str +' NULL '
	ELSE
		lc_Crea_Str = lc_Crea_Str +' NOT NULL '
	ENDIF

	IF prim_key = 1
		lc_Crea_Str = lc_Crea_Str +', PRIMARY KEY [' + ALLTRIM(Field_Name) + "] tag " + ;
			LEFT(ALLTRIM(Field_Name), 10)
	ELSE
		IF uniq_key = 1
			lc_Crea_Str = lc_Crea_Str +' UNIQUE '
		ENDIF
	ENDIF
	lcComma = ", "
	SELECT(pCursor_Ddict)
ENDSCAN

lc_Crea_Str = lc_Crea_Str + ')'
IF !ExecuteCommand(lnConnHandle, lc_Crea_Str, "", .T.)
	RETURN .F.
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: RollbackTran
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION RollbackTran

LOCAL oError AS EXCEPTION

SELECT crsConnectins
SCAN FOR ConnHandle > 0
	TRY
		SQLEXEC(crsConnectins.ConnHandle, "ROLLBACK TRANSACTION")
		SQLDISCONNECT(crsConnectins.ConnHandle)
	CATCH TO oError

	ENDTRY
ENDSCAN
USE IN crsConnectins
ENDFUNC

*-------------------------------------------------------
* Function....: CreateBackUp
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:	tcFolderArchive, tcVfxPath, tlFromDBUpdate, tcFileSkeleton, tlNotRecurseSubfolders, tlQuietMode,
*				*{ V&U VM 2011-07-02
*				tlShowGetDirDialog - Defines if should be shown a dialog to select the folder where the zip archive will be created.
*				*} V&U VM 2011-07-02
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CreateBackUp(tcFolderArchive, tcVfxPath, tlFromDBUpdate, tcFileSkeleton, tlNotRecurseSubfolders, tlQuietMode, tlShowGetDirDialog)

LOCAL loCreateArchive, lcDirForArchiv, lcZipName, lcZipNameWithPath, oError AS EXCEPTION, ;
	lcExePath, laDummy[1,1], lcDBCName, laDBC[1], lcArchPath
LOCAL lnPos AS INTEGER

IF goprogram.nformcount <> 0
	 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_WINDOWS_OPENED, MSG_WINDOWS_OPENED), 0 + 48, ;
		_SCREEN.CAPTION)
	RETURN .F.
ENDIF

IF EMPTY(tcFolderArchive)
	tcFolderArchive = goProgram.cdatadir
ENDIF
*{V&U MS 2008-09-01
IF EMPTY(tcFileSkeleton)
	tcFileSkeleton = "*.*"
ENDIF
*}V&U MS 2008-09-01

lcArchPath = ""
IF TYPE("goSystem.ArchPath") ="C"
	lcArchPath = goSystem.ArchPath
ELSE
	llError = .F.
*{V&U MS 2009-10-01
	IF TYPE("goProgram.nVfxSysTableLoc") == "N" AND goProgram.nVfxSysTableLoc >= 1
		*{V&U MS 2010-05-18
		lnOldAppOnErrorBehavior = goProgram.nAppOnErrorBehavior
		goProgram.nAppOnErrorBehavior = 0
		caVfxSys = 	OPENTABLE("select * from VFXSys", "IDVFXSYS", .F., "", ;
					.T., , "VFXSys", "cAppVFXDataAccess")
		IF !USED("VFXSys")
			caVfxSys = OPENTABLE("select * from VFXSys", "magic_id", .T., "", ;
					.T., , "VFXSys", "cAppVFXDataAccess")		
		ENDIF 	
		goProgram.nAppOnErrorBehavior = lnOldAppOnErrorBehavior	
		IF USED("VFXSys")
			lcArchPath = VFXSys.ArchPath
		ENDIF 
		*}V&U MS 2010-05-18
	ELSE
		TRY
			USE tcvfxpath + "VFXSys" IN 0 AGAIN SHARED	&& we are working with local tables
			lcArchPath = VFXSys.ArchPath
		CATCH
			llError = .T.
		ENDTRY
	ENDIF
*}V&U MS 2009-10-01
	IF llError
		 = goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, ;
			goLocalize.cMSG_CANNOTREADaRCHIVEfOLDER, MSG_CANNOTREADaRCHIVEfOLDER), ;
			0 + 16, _SCREEN.CAPTION)
		RETURN .F.
	ENDIF
ENDIF

CLOSE DATABASE ALL
CLOSE TABLES ALL

lcExePath = ADDBS(SYS(5) + SYS(2003))

IF LEFT(goprogram.cdatadir, 2) =="\\" OR SUBSTR(goprogram.cdatadir, 2, 1) ==":"
	lcDirForArchiv = tcFolderArchive
ELSE
	lcDirForArchiv = lcExePath + tcFolderArchive
ENDIF

ldToDay = DATETIME()

lcNameOfLastFolder = ADDBS(lcDirForArchiv)
lcNameOfLastFolder = LEFT(lcNameOfLastFolder, LEN(lcNameOfLastFolder) - 1)
lnPos = RAT("\", lcNameOfLastFolder)
IF lnPos > 0
	lcNameOfLastFolder = SUBSTR(lcNameOfLastFolder, lnPos + 1)
ENDIF
lcDBCName = ""
IF TYPE("goProgram.cMainDatabase") = "C" AND !EMPTY(goProgram.cMainDatabase)
	lcDBCName = JUSTSTEM(goProgram.cMainDatabase)
ENDIF
IF EMPTY(lcDBCName)
	IF ADIR(laDBC, ADDBS(lcDirForArchiv) + "*.dbc") > 0
		lcDBCName = JUSTSTEM(laDBC[1])
	ENDIF
ENDIF

*{JEI MS 07.09.2006 Modified
lcZipName = FORCEEXT(lcNameOfLastFolder + IIF(!EMPTY(lcDBCName), "_" + lcDBCName, "") + "_" + ;
	TRANSFORM(YEAR(ldToDay)) + PADL(TRANSFORM(MONTH(ldToDay)), 2,"0") + PADL(TRANSFORM(DAY(ldToDay)), 2,"0") + ;
	PADL(TRANSFORM(HOUR(ldToDay)), 2,"0") + PADL(TRANSFORM(MINUTE(ldToDay)), 2,"0"),"Zip")
*}JEI MS 07.09.2006

*{ V&U VM 2011-07-02
IF tlShowGetDirDialog
	lcArchPath = GETDIR(lcArchPath)
	*{V&U MS 2012-04-25 5797
	IF EMPTY(lcArchPath)
		RETURN .F.
	ENDIF 
	*}V&U MS 2012-04-25
ENDIF 
*} V&U VM 2011-07-02

IF LEFT(lcArchPath, 2) =="\\" OR SUBSTR(lcArchPath, 2, 1) ==":"
	lcZipNameWithPath = ADDBS(lcArchPath) + lcZipName
ELSE
	lcZipNameWithPath = ADDBS(lcExePath + lcArchPath) + lcZipName
ENDIF

TRY
	loCreateArchive = CREATEOBJECT("cArchive")
*{ V&U MS 2008-12-18 Add tlQuietMode
	IF tlQuietMode
		loCreateArchive.lQuietMode = .T.
	ENDIF
*} V&U MS 2008-12-18
	*{V&U MS 2010-03-31, exclude "Upd$Ctrl.key" from backup }
	IF loCreateArchive.CreateArchive(lcDirForArchiv, tcFileSkeleton, lcZipNameWithPath, -1, !tlNotRecurseSubfolders, "", ;
				"Upd$Ctrl.key")
		IF !tlFromDBUpdate
			 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_BACKUPSUCCESSFULCOMPLETED, ;
				MSG_BACKUPSUCCESSFULCOMPLETED), 0 + 64, ;
				IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ARCHIVE, MSG_ARCHIVE))
		ENDIF
	ELSE
		IF ADIR(laDummy, lcZipNameWithPath) = 1
			ERASE (lcZipNameWithPath)
		ENDIF
	ENDIF
	RELEASE loCreateArchive

CATCH TO oError
	IF ADIR(laDummy, lcZipNameWithPath) = 1
		ERASE (lcZipNameWithPath)
	ENDIF
ENDTRY
GoProgram.LockCurrentLoggedUse()
ENDFUNC

*-------------------------------------------------------
* Function....: RestoreBackUp
* Called by...: BackUp
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcDirForUnZip - name of folder to extract
*				from archive
* Notes.......:
*-------------------------------------------------------
FUNCTION RestoreBackUp(tcDirForUnZip)

LOCAL loCreateArchive, lcDirForUnZip, lcArchiveName, lcOldDir, lcArchDir, lcTmpDir, lnCurrentFile, ;
	oError AS EXCEPTION, lcExePath, llRez

CLOSE DATABASE ALL
CLOSE TABLES ALL
lnErrorNum = 0
lcOldDir = ""
TRY
	*{V&U MS 2012-07-17 5861
	lcExePath = ADDBS(GetDefaultFolder())
	*}V&U MS 2012-07-17
*{V&U MS 2009-05-21
	IF EMPTY(tcDirForUnZip)
		tcDirForUnZip = goProgram.cDataDir
	ENDIF
	IF LEFT(tcDirForUnZip, 2) == "\\" OR SUBSTR(tcDirForUnZip, 2, 1) == ":"
		lcDirForUnZip = tcDirForUnZip
	ELSE
		lcDirForUnZip = lcExePath + tcDirForUnZip
	ENDIF
*}V&U MS 2009-05-21

	*{V&U MS 2012-07-17 5861
	lcOldDir = GetDefaultFolder()
	*}V&U MS 2012-07-17

	IF LEFT(GoSystem.ArchPath, 2) =="\\" OR SUBSTR(GoSystem.ArchPath, 2, 1) ==":"
		lcArchDir = GoSystem.ArchPath
	ELSE
		lcArchDir = ADDBS(lcExePath + GoSystem.ArchPath)
	ENDIF

	IF DIRECTORY(lcArchDir, 1)
		CD (lcArchDir)
	ENDIF
	*{V&U MS 2011-02-01, Use VfxGetFile instead of GETFILE()
	lcArchiveName = VfxGetFile("Zip")
	*}V&U MS 2011-02-01
	CD (lcOldDir)
	IF EMPTY(lcArchiveName)
		lnErrorNum = 1
	ELSE
		IF ADIR(laExistFile, lcArchiveName) = 0
			lnErrorNum = 1
		ENDIF
		RELEASE laExistFile
	ENDIF
CATCH TO oError
	 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RESTOREOPERATIONFAILED, ;
		MSG_RESTOREOPERATIONFAILED), 16, IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ARCHIVE, MSG_ARCHIVE))
	lnErrorNum = oError.ERRORNO
ENDTRY

IF lnErrorNum > 0
	IF !EMPTY(lcOldDir) AND DIRECTORY(lcOldDir, 1)
		CD (lcOldDir)
	ENDIF
	GoProgram.LockCurrentLoggedUse()
	RETURN .F.
ENDIF

lcTmpDir = ""
TRY
	lcTmpDir = ADDBS(lcExePath + SYS(2015))
	MD (lcTmpDir)
	lnNumFile = ADIR(laCopyFiles, ADDBS(lcDirForUnZip) + "*.*")
	lcDirUnZipCopy = ADDBS(lcDirForUnZip)
	FOR lnCurrentFile = 1 TO lnNumFile
		lcSourceFileName = ["] + lcDirUnZipCopy + laCopyFiles(lnCurrentFile, 1) + ["]
		lcDestinationFilrName = ["] + lcTmpDir + laCopyFiles(lnCurrentFile, 1) + ["]
		COPY FILE (lcSourceFileName) TO (lcDestinationFilrName)
	ENDFOR
CATCH TO oError
	lnErrorNum = oError.ERRORNO
ENDTRY
IF lnErrorNum > 0
	DelDirectory(lcTmpDir)
	GoProgram.LockCurrentLoggedUse()
	 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RESTOREOPERATIONFAILED, ;
		MSG_RESTOREOPERATIONFAILED), 16, IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ARCHIVE, MSG_ARCHIVE))
	RETURN .F.
ENDIF

llRez = .F.
TRY
	loCreateArchive = CREATEOBJECT("cArchive")
	llRez = loCreateArchive.ExtractFromArchive(lcArchiveName, "*.*", lcDirForUnZip, "")
	RELEASE loCreateArchive
CATCH TO oError
	lnErrorNum = oError.ERRORNO
ENDTRY

IF !llRez OR lnErrorNum > 0
	FOR lnCurrentFile = 1 TO lnNumFile
		lcSourceFileName = ["] + lcTmpDir + laCopyFiles(lnCurrentFile, 1) + ["]
		lcDestinationFilrName = ["] + lcDirUnZipCopy + laCopyFiles(lnCurrentFile, 1) + ["]
		COPY FILE (lcSourceFileName) TO (lcDestinationFilrName)
	ENDFOR
	RELEASE laCopyFiles
	DelDirectory(lcTmpDir)
	GoProgram.LockCurrentLoggedUse()
	 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RESTOREOPERATIONFAILED, ;
		MSG_RESTOREOPERATIONFAILED), 16, IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ARCHIVE, MSG_ARCHIVE))
	RETURN .F.
ENDIF
RELEASE laCopyFiles
DelDirectory(lcTmpDir)
 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_RESTORESUCCESSFULCOMPLETED, ;
	MSG_RESTORESUCCESSFULCOMPLETED), 64, IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ARCHIVE, MSG_ARCHIVE))
GoProgram.LockCurrentLoggedUse()
ENDFUNC

*-------------------------------------------------------
* Function....: BackUP
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tnProcess 1 - crete archive, 2 - restore from archive
*				*{ V&U VM 2011-07-02
*				tlShowGetDirDialog - Defines if should be shown a dialog to select the folder where the zip archive will be created.
*				*} V&U VM 2011-07-02
*
* Notes.......:
*-------------------------------------------------------
FUNCTION BackUP(tnProcess, tlShowGetDirDialog)

LOCAL llFormIsOpen, lcDataToArchive
IF goprogram.nformcount <> 0
	 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_WINDOWS_OPENED, MSG_WINDOWS_OPENED), 0 + 48, ;
		_SCREEN.CAPTION)
	RETURN .F.
ENDIF

llFormIsOpen  = .F.

*{V&U MS 2009-05-21
lcDataToArchive = ""
IF !(ALLTRIM(UPPER(goProgram.cDataSourceType)) == "NATIVE") AND goProgram.nVfxSysTableLoc = 0
	lcDataToArchive = goProgram.cVfxDir
	IF tnProcess = 1
		lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ONLYSYSTEMTABLESWILLBEINCLUDEDINTOARCHIVE, ;
			MSG_ONLYSYSTEMTABLESWILLBEINCLUDEDINTOARCHIVE)
	ELSE
		lcMessageText = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ONLYSYSTEMTABLESWILLBERESTOREDFROMARCHIVE, ;
			MSG_ONLYSYSTEMTABLESWILLBERESTOREDFROMARCHIVE)
	ENDIF
	IF goProgram.vfxMessageBox(lcMessageText, 4 + 32, _SCREEN.CAPTION) = 7
		RETURN .F.
	ENDIF
ENDIF
*}V&U MS 2009-05-21

FOR j = 1 TO _SCREEN.FORMCOUNT
	IF TYPE("_screen.Forms[j]._VFXClassName") ="C" AND TYPE("_screen.Forms[j].cSCXName") ="C"
		IF UPPER(ALLTRIM(_SCREEN.FORMS[j].cscxname)) == "VFXXPOPEN.SCX"
			_SCREEN.FORMS[j].RELEASE()
			llFormIsOpen  = .T.
		ENDIF
	ENDIF
NEXT

*{V&U MS 2009-05-21
IF tnProcess = 1
	*{ V&U VM 2011-07-02 - add tlShowGetDirDialog
	CreateBackUp(lcDataToArchive,,,,,, tlShowGetDirDialog)
	*} V&U VM 2011-07-02
ELSE
	RestoreBackUp(lcDataToArchive)
ENDIF
*}V&U MS 2009-05-21

IF llFormIsOpen
	goprogram.runform("vfxxpopen")
ENDIF

ENDFUNC

*-------------------------------------------------------
* Function....: TryConnecting
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: cDataBaseName, cServerName, cModuleDescr
*
* Notes.......:
*-------------------------------------------------------
FUNCTION TryConnecting(cDataBaseName, cServerName, cModuleDescr, ofoxapp)

LOCAL lServersString, lerrorString, lcReturnConnStr, lcServerName, lnSelect
IF VARTYPE(cServerName) # "C"
	cServerName = ""
ENDIF
lServersString = ""
lerrorString = ""
get_ser = GetSQLServers(@lServersString, @lerrorString)

IF get_ser < 0
	RETURN ""
ENDIF
Names_Data = STRTRAN(lServersString, ',', CHR(13) + CHR(10))
IF ATLINE(cServerName, Names_Data) = 0
	IF MEMLINE(Names_Data) = 1 && Only one Server
		lcServerName = lServersString
	ELSE
		DO FORM ("vfxServers") WITH cDataBaseName, Names_Data, cModuleDescr TO lcServerName
		IF EMPTY(lcServerName)
			RETURN ""
		ENDIF
	ENDIF
ELSE
	lcServerName = cServerName
ENDIF
lcConnStr = "DRIVER={SQL Server};SERVER=" + lcServerName

lnConnHandle = SQLSTRINGCONNECT(lcConnStr + ";Trusted_Connection=Yes")
IF lnConnHandle > 0
	lcReturnConnStr = lcConnStr + ";TRusted_Connection=Yes"
ELSE
	lnConnHandle = SQLSTRINGCONNECT(lcConnStr + ";UID=sa;PWD=")
	IF lnConnHandle > 0
		lcReturnConnStr = lcConnStr + ";UID=sa;PWD="
	ELSE
		IF TYPE("ofoxapp.ointroform") ="O"
			IF !ISNULL(ofoxapp.ointroform)
				ofoxapp.ointroform.HIDE()
			ENDIF
		ENDIF
		DO FORM ("vfxsqlstr") WITH cModuleDescr, lcServerName, cDataBaseName TO lcReturnConnStr
		IF EMPTY(lcReturnConnStr)
			RETURN ""
		ELSE
			lnConnHandle = SQLSTRINGCONNECT(lcReturnConnStr)
		ENDIF
		IF lnConnHandle < 0
			lcReturnConnStr = ""
		ENDIF
	ENDIF
ENDIF
IF lnConnHandle > 0
	lnSelect = SELECT() && JEI RI 2008.06.04 changed from ALIAS() to SELECT()
	IF SQLEXEC(lnConnHandle, "sp_databases", "crsAllDataBases") > 0
		SELECT crsAllDataBases
		LOCATE FOR UPPER(ALLTRIM(DataBase_Name)) == UPPER(ALLTRIM(cDataBaseName))
		IF !FOUND()
			IF SQLEXEC(lnConnHandle, "create database [" + cDataBaseName + "]") > 0
				lcReturnConnStr = lcReturnConnStr +  ";DATABASE=" + cDataBaseName
			ELSE
				IF TYPE("goProgram.class") ="C"
					goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_CANNOTCREATEDATABASE, ;
						MSG_CANNOTCREATEDATABASE) + " " + cDataBaseName, 0 + 16, ;
						IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_FRMERROROCCURED, CAP_FRMERROROCCURED))
				ENDIF
				lcReturnConnStr = ""
			ENDIF
		ELSE
			lnConnHandle2 = SQLSTRINGCONNECT(lcReturnConnStr + ";DATABASE=" + cDataBaseName)
			IF lnConnHandle2 > 0
				lcReturnConnStr = lcReturnConnStr +  ";DATABASE=" + cDataBaseName
				SQLDISCONNECT(lnConnHandle2)
			ENDIF
		ENDIF
		USE IN crsAllDataBases
	ELSE
		lcReturnConnStr = ""
	ENDIF
	IF !EMPTY(lnSelect)
		SELECT(lnSelect)
	ENDIF
	SQLDISCONNECT(lnConnHandle)
ENDIF
RETURN lcReturnConnStr
ENDFUNC

*-------------------------------------------------------
* Function....: GetServerNameFromConnStr
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: connStr
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetServerNameFromConnStr(cConnStr)
lcConnStrTmp = UPPER(cConnStr)
lcServerName = ALLTRIM(SUBSTR(cConnStr, AT("SERVER=", lcConnStrTmp) + 7))
lnpos = AT(";", lcServerName)
IF lnpos  > 0
	lcServerName = SUBSTR(lcServerName, 1, lnpos - 1)
ENDIF

RETURN lcServerName

ENDFUNC

*-------------------------------------------------------
* Function....: ValidFileName()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: cFileName
*
* Notes.......:
*-------------------------------------------------------
FUNCTION ValidFileName
LPARAMETERS cFileName
LOCAL cFS, cFE, cFP
cFS = CHRTRAN(cFileName,[,"?|<>*=%],"_________")
cFE = JUSTEXT(cFS)
cFP = ADDBS(JUSTPATH(cFS))
cFS = CHRTRAN(JUSTSTEM(cFS),[:/\],"___")
RETURN cFP + FORCEEXT(cFS, cFE)
ENDFUNC

*-------------------------------------------------------
* Function....: OpenTable()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcTableName, tcKeyFieldList, llUpdateKeyField, tcExecuteAfterCursorFill, ;
*				tnExecuteCursorFill, toForm , tcAlias, tcCAClassName, tlDontSendUpdates, ; 
*				tnBufferModeOverride, tcNotUpdatableFieldList, tlDontReplaceEmptyDatesWithNull, ;
*				tcReservedWordList, tcOptimizedForDBType
*				JEI MS 16.06.2006 There's places where SendUpdates is not needed. In caBaseDataAccess.SendUpdates = .T.
*				JEI VM 31.07.07 tlExecuteCursorFill --> tnExecuteCursorFill
*				0 - Do not execute CursorFill; 1 - execute CursorFill; 2 - execute CursorFill with NoData
* Notes.......:	V&U MS 16.12.2013 
*				tcReservedWordList - Contains a list of reserved words used in Select command (table names, field names)
*				tcOptimizedForDBType - Contains the DBType name, ;
*				for which the initial SelectCmd and cWhereClause are generated.
*				Possible values: "VFP", "SQL Server", "MySQL", "DB2", "Oracle"
*-------------------------------------------------------
FUNCTION OPENTABLE
LPARAMETERS tcTableName, tcKeyFieldList, llUpdateKeyField, tcExecuteAfterCursorFill, ;
	tnExecuteCursorFill, toForm AS FORM, tcAlias, tcCAClassName, tlDontSendUpdates, ;
	tnBufferModeOverride, tcNotUpdatableFieldList, tlDontReplaceEmptyDatesWithNull, ;
	tcReservedWordList, tcOptimizedForDBType

LOCAL loDE AS DATAENVIRONMENT, lcDEName, loCursorAdapter AS CURSORADAPTER, lcAlias, lcCAName, ;
	lcCAClassName, llDetachCursor, oError AS EXCEPTION, lnErrorNo, laError[1], ;
	loAliasIsUseObject AS CURSORADAPTER, loCurrentObject AS CURSORADAPTER, ;
	lnPos, lcTableName, lnExecuteCursorFill, lcLeftDelimiter, lcRightDelimiter, ;
	lcLeftIdentifier, lcRightIdentifier, loTempCA, lcNotUpdatableFieldList

*JEI VM 31.07.07( because old code is with logical parameter
IF TYPE("tnExecuteCursorFill") = "L"
	IF !tnExecuteCursorFill
		lnExecuteCursorFill = 0
	ELSE
		lnExecuteCursorFill = 1
	ENDIF
ELSE
	lnExecuteCursorFill = tnExecuteCursorFill
ENDIF
*JEI VM 31.07.07) because old code is with logical parameter

*{ Check input parameters
IF EMPTY(tcTableName)
	RETURN .NULL.
ENDIF

IF EMPTY(tcAlias)
	IF LOWER(LEFT(ALLTRIM(tcTableName), 7)) == "select "
		RETURN .NULL.
	ELSE
		lcAlias = tcTableName
	ENDIF
ELSE
	lcAlias = tcAlias
ENDIF

loCursorAdapter = .NULL.
lcCAName = lcAlias

IF VARTYPE(tcCAClassName) # "C" OR EMPTY(tcCAClassName)
	lcCAClassName = "cappdataaccess"
ELSE
	lcCAClassName = tcCAClassName
ENDIF

*{ V&U MS 2008-10-15
IF VARTYPE(tcNotUpdatableFieldList) # "C" OR EMPTY(tcNotUpdatableFieldList)
	lcNotUpdatableFieldList = ""
ELSE
	lcNotUpdatableFieldList = tcNotUpdatableFieldList
ENDIF
*} V&U MS 2008-10-15

*} Check input parameters
*{V&U VM 2014-02-18
*{HC VM 2015-09-28 (WI6876 - 1)
IF VARTYPE(toForm) = "O" AND LOWER(toForm.BASECLASS) = "form" AND ;
			((EMPTY(toForm.DEClass) AND TYPE("toForm.DATAENVIRONMENT.Class") == "C") OR ;
			(!EMPTY(toForm.DEClass) AND TYPE("toForm." + toForm.DEClass + ".Class") == "C"))
*}HC VM 2015-09-28
*}V&U VM 2014-02-18
* Form is specified
	lnErrorNo = 0
	loDE = .NULL.
	TRY
* Get reference to DataEnvironment object
		lcDEName = toForm.DECLASS
		IF !EMPTY(lcDEName)
			loDE = toForm.&lcDEName.
		ELSE
			loDE = toForm.DATAENVIRONMENT
		ENDIF
	CATCH TO oError
		lnErrorNo = oError.ERRORNO
		onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
	ENDTRY
	IF lnErrorNo > 0 OR ISNULL(loDE)
		RETURN .NULL.
	ENDIF

	llAliasIsUsed = .F.
	llObjectExist = PEMSTATUS(loDE, lcCAName, 5)
	IF llObjectExist
* Check for existing object in DataEnvironment with same name
		loObject = loDE.&lcCAName.
		IF LOWER(ALLTRIM(loObject.BASECLASS)) == "cursor" && Object is cursor
			ERROR 1771 && Generate Error: Object Exist
			RETURN .NULL.
		ENDIF
	ENDIF

	FOR m.kk = 1 TO loDE.OBJECTS.COUNT && For all objects in DataEnvironment
		loCurrentObject = loDE.OBJECTS(m.kk)
		lcBaseClass = LOWER(ALLTRIM(loCurrentObject.BASECLASS))
		IF INLIST(lcBaseClass, "cursor", "cursoradapter") && Object is Cursor or CursorAdapter
			* V&U MS 2009-11-24, "=="
			IF LOWER(ALLTRIM(loCurrentObject.ALIAS)) == LOWER(ALLTRIM(lcAlias))
&&Alias name is already in use.
				loAliasIsUseObject = loCurrentObject
				llAliasIsUsed = .T.
				IF lcBaseClass == "cursor" && Object is cursor
					ERROR 24 && Generate Error: Alias name is already in use.
					RETURN .NULL.
				ELSE
					IF LOWER(ALLTRIM(loAliasIsUseObject.TABLES)) == LOWER(ALLTRIM(tcTableName))
* CursorAdapter is from same Table
						IF lnExecuteCursorFill = 1						&& JEI VM 31.07.07 : IF tlExecuteCursorFill --> IF lnExecuteCursorFill = 1 (Because of change of variable type)
							TRY
* Execute CursorFill
								loAliasIsUseObject.CURSORFILL()
							CATCH
							ENDTRY
						ENDIF
						RETURN loAliasIsUseObject
					ELSE
						ERROR 24 && Generate Error: Alias name is already in use.
						RETURN .NULL.
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	ENDFOR

*
	IF llObjectExist
* Object Exist in DataEnvironment with same name
		IF !(LOWER(ALLTRIM(loObject.TABLES)) == LOWER(ALLTRIM(tcTableName))) && CursorAdapter is from same Table
			ERROR 24 && Generate Error: Alias name is already in use.
			RETURN .NULL.
		ENDIF
		lcBuffer = lcCAName
		lnObjectNum = 0
* Create new object name
		DO WHILE PEMSTATUS(loDE, lcCAName, 5)
			lnObjectNum = lnObjectNum + 1
			lcCAName = lcBuffer + TRANSFORM(lnObjectNum)
		ENDDO
	ENDIF

	TRY
* Add new object (CursorAdapter) in DataEnvironment
		loDE.ADDOBJECT(lcCAName, lcCAClassName)
		loCursorAdapter = loDE.&lcCAName.
	CATCH TO oError
		lnErrorNo = oError.ERRORNO
	ENDTRY
	IF lnErrorNo > 0
		onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
		RETURN .NULL.
	ENDIF
ELSE
	lnErrorNo = 0
	TRY
*Crete new CursorAdapter
		loCursorAdapter = NEWOBJECT(lcCAClassName)
	CATCH TO oError
		lnErrorNo = oError.ERRORNO
	ENDTRY
	IF lnErrorNo > 0 OR ISNULL(loCursorAdapter) OR VARTYPE(loCursorAdapter) # "O"
		IF lnErrorNo > 0
			onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
		ELSE
			onerror(ERROR(), PROGRAM(), LINENO(), MESSAGE(), "loCursorAdapter = NEWOBJECT(lcCAClassName)")
		ENDIF
		RETURN .NULL.
	ENDIF
ENDIF

*{ JEI IG 2006.11.20 Add
IF TYPE("tnBufferModeOverride") == "N"
	loCursorAdapter.BUFFERMODEOVERRIDE = tnBufferModeOverride
ENDIF
*{ JEI IG 2006.11.20

IF LOWER(LEFT(ALLTRIM(tcTableName), 7)) == "select "
* Developer specified SQL Select command
	loCursorAdapter.SELECTCMD = tcTableName
*{V&U MS 2009-07-28
	lcLeftDelimiter = IIF(TYPE("loCursorAdapter.cLeftDelimiter") == "C", ALLTRIM(loCursorAdapter.cLeftDelimiter), "")
	lcRightDelimiter = IIF(TYPE("loCursorAdapter.cRightDelimiter") == "C", ALLTRIM(loCursorAdapter.cRightDelimiter), "")
	m.lcTableName = GetTableFromSelect(tcTableName, lcLeftDelimiter, lcRightDelimiter) && Get table name from SQL Select command
*}V&U MS 2009-07-28
	IF !EMPTY(m.lcTableName)
		loCursorAdapter.TABLES = m.lcTableName
	ENDIF
ELSE
* Create SQL Select command
*{ JEI IG 2006.10.12 Change
	IF TYPE("loCursorAdapter.cLeftBracket") == "C" AND !EMPTY(loCursorAdapter.cLeftBracket) AND ;
			TYPE("loCursorAdapter.cRightBracket") == "C" AND !EMPTY(loCursorAdapter.cRightBracket)
		loCursorAdapter.SELECTCMD = 'Select * From ' + loCursorAdapter.cLeftBracket + tcTableName + loCursorAdapter.cRightBracket
	ELSE
		loCursorAdapter.SELECTCMD = "Select * From [" + tcTableName + "]"
	ENDIF
*{ JEI IG 2006.10.12
	loCursorAdapter.TABLES = tcTableName
ENDIF

loCursorAdapter.ALIAS = lcAlias
IF TYPE("loCursorAdapter.cExecuteAfterCursorFill") # "U"
	loCursorAdapter.cExecuteAfterCursorFill = ALLTRIM(tcExecuteAfterCursorFill)
ENDIF
loCursorAdapter.KEYFIELDLIST = ALLTRIM(tcKeyFieldList)

*{ JEI RKR 2005.09.20
* If property values read from class differ from properties set in form's DE,
* it is possible an error to be raised while executing CursorFill
* In any case these properties are updated later,
* based on retrieved data
*{ HC MS 2015-03-24
loCursorAdapter.UseCursorSchema = .F. 
*} HC MS 2015-03-24
loCursorAdapter.CURSORSCHEMA = ""
loCursorAdapter.UPDATENAMELIST =  ""
loCursorAdapter.UPDATABLEFIELDLIST = ""
loCursorAdapter.INSERTCMDREFRESHFIELDLIST = ""
loCursorAdapter.INSERTCMDREFRESHKEYFIELDLIST = ""
*{ JEI - MS 16.06.2006
IF tlDontSendUpdates
	loCursorAdapter.SENDUPDATES = .F.
ENDIF
*} JEI - MS
*} JEI RKR 2005.09.20

*{V&U MS 2013-12-17
loCursorAdapter.cReservedWordList = IIF(EMPTY(tcReservedWordList), "", tcReservedWordList)
IF !EMPTY(tcOptimizedForDBType)
	loTempCA = NEWOBJECT("cBaseDataAccess")

	STORE "" TO lcLeftIdentifier, lcRightIdentifier
	loTempCA.GetQuotedIdentifiers(tcOptimizedForDBType, @lcLeftIdentifier, @lcRightIdentifier)
	loTempCA.cLeftBracket = lcLeftIdentifier
	loTempCA.cRightBracket = lcRightIdentifier
	loCursorAdapter.SelectCmd = loTempCA.ProcessReservedWords(loCursorAdapter.SelectCmd, loCursorAdapter.cReservedWordList)
	loCursorAdapter.cOptimizedForDBType = tcOptimizedForDBType
	RELEASE loTempCA 
ENDIF 
loCursorAdapter.CheckAndOptimizeForDBType()
*}V&U MS 2013-12-17

*{V&U VM 2010-06-22	Modified: fill Cursor Adapter without data to set CursorSchema before fill data. Reason SQL 2008 field type VARCHAR(MAX) comes as C(0). 
IF loCursorAdapter.CURSORFILL(.F., .T.)
	*Create CursorSchema, UpdateNameList, UpdatableFieldList
	*{JEI MS 08.09.2006 Modified
	CreateCAFieldsList(loCursorAdapter.ALIAS, @loCursorAdapter, tcKeyFieldList, ;
			llUpdateKeyField, tlDontSendUpdates, lcNotUpdatableFieldList, tlDontReplaceEmptyDatesWithNull)
	*}JEI MS 08.09.2006
	loCursorAdapter.UseCursorSchema = .T.
	IF USED(loCursorAdapter.Alias)	
		USE IN (loCursorAdapter.Alias)
	ENDIF
ENDIF

IF lnExecuteCursorFill = 1 && Need execute CursorFill		&& JEI VM 31.07.07 : IF tlExecuteCursorFill --> IF lnExecuteCursorFill = 1 (Because of change of variable type)
	IF !loCursorAdapter.CURSORFILL(.T.)	&& always use the schema in the CursorSchema property, because of SQL 2008 field type VARCHAR(MAX) 
		AERROR(laError)
		onerror(laError[1], PROGRAM(), 0, laError[2], "")
		RELEASE loCursorAdapter
		RETURN .NULL.
	ENDIF
ELSE
	IF lnExecuteCursorFill = 2				&& JEI VM 31.07.07: In case lnExecuteCursorFill = 2	CA have to stay open wtih NO DATA
		loCursorAdapter.CURSORFILL(.T., .T.) && always use the schema in the CursorSchema property, because of SQL 2008 field type VARCHAR(MAX) 
	ENDIF
ENDIF
*}V&U VM 2010-06-22

*{JEI MS 22.08.2006
lnDataSourcePlatformType = loCursorAdapter.GetDataSourcePlatformType()
IF TYPE("lnDataSourcePlatformType") = "N"
	loCursorAdapter.GetInsertRefreshCmd(lnDataSourcePlatformType)
ENDIF
*}JEI MS 22.08.2006

RETURN loCursorAdapter
ENDFUNC

*-------------------------------------------------------
* Function....: GetTableFromSelect
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcSelectCmd, tcLeftDelimiter, tcRightDelimiter
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetTableFromSelect
LPARAMETERS tcSelectCmd, tcLeftDelimiter, tcRightDelimiter

LOCAL lcSelectCmd, lcTableName, lnStartPos, lnEndPos, lcSep, lcBuffer, llContinue

lcTableName = ""
lcSelectCmd = LOWER(ALLTRIM(tcSelectCmd))
lnStartPos = 0
lnEndPos = 0
lcSep = ""
lcBuffer = ""

*{V&U MS 2009-07-28
IF EMPTY(tcLeftDelimiter)
	tcLeftDelimiter = "["
ENDIF
IF EMPTY(tcRightDelimiter)
	tcRightDelimiter = "]"
ENDIF
*}V&U MS 2009-07-28

IF !(LEFT(m.lcSelectCmd, 6) == "select")
	RETURN m.lcTableName
ENDIF
m.lcSelectCmd = CHRTRAN(m.lcSelectCmd, CHR(9), " ")	&& replace unprintable symbols with spaces

*{V&U MS 2009-07-28, If used [Database].dbo.[Table]
lcBuffer = STRTRAN(m.lcSelectCmd, "." + tcLeftDelimiter, ".")
lcBuffer = STRTRAN(lcBuffer, tcRightDelimiter + ".", ".")
lcBuffer = CHRTRAN(lcBuffer, tcLeftDelimiter + tcRightDelimiter, "  ")

lnStartPos = AT(" from ", m.lcBuffer) + 5 + (LEN(m.lcSelectCmd) - LEN(m.lcBuffer))
lcBuffer = ALLTRIM(SUBSTR(tcSelectCmd, m.lnStartPos))
lcTableName = ""
llContinue = .T.
DO WHILE llContinue
	lnEndPos = 	IIF(AT(tcRightDelimiter, m.lcBuffer) > 0, ;
		IIF(AT(" ", SUBSTR(m.lcBuffer, AT(tcRightDelimiter, m.lcBuffer))) > 0, ;
		AT(" ", SUBSTR(m.lcBuffer, AT(tcRightDelimiter, m.lcBuffer))) + AT(tcRightDelimiter, m.lcBuffer), 0), ;
		AT(" ", m.lcBuffer))
	IF m.lnEndPos > 0
		lcTableName = lcTableName + LEFT(m.lcBuffer, m.lnEndPos - 1)
		m.lcBuffer = ALLTRIM(SUBSTR(m.lcBuffer, m.lnEndPos))
		IF (tcLeftDelimiter == tcRightDelimiter AND MOD(OCCURS(tcLeftDelimiter, m.lcBuffer), 2) == 0) OR ;
				(tcLeftDelimiter <> tcRightDelimiter AND OCCURS(tcLeftDelimiter, m.lcBuffer) == OCCURS(tcRightDelimiter, m.lcBuffer))
			llContinue = .F.
		ENDIF
	ELSE
		lcTableName = lcTableName + m.lcBuffer
		m.lcBuffer = ""
		llContinue = .F.
	ENDIF
ENDDO
*}V&U MS 2009-07-28

RETURN m.lcTableName
ENDFUNC

*-------------------------------------------------------
* Function....: CreateCAFieldsList()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:  tcAlias, tcCursorAdapter, tcKeyFieldList
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CreateCAFieldsList
LPARAMETERS tcAlias, toCursorAdapter AS CURSORADAPTER, tcKeyFieldList, ;
	tlUpdateKeyField, tlDontSendUpdates, tcNotUpdatableFieldList, tlDontReplaceEmptyDatesWithNull

LOCAL lnOldAlilas, lcSelectCmd, lcUpdatablefieldlist, lcUpdateNameList, lcCursorSchema, ;
	lnFieldCount, lcKeyFieldList, lcInsertCmdRefreshFieldList, llGUIDKeyField, lcBufferUpd, ;
	lcFieldsToWriteNullWhenEmpty, lnFieldCount, lnCurrentField, lcReplaceStr

lnOldAlilas = SELECT()
IF !EMPTY(tcAlias) AND VARTYPE(tcAlias) = "" AND USED(tcAlias)
	SELECT(tcAlias)
ENDIF

*{V&U MS 2011-12-17 5595
IF !(ATC("SQL Server", toCursorAdapter.cDBdriver) <> 0 OR ATC("SQL Native Client", toCursorAdapter.cDBdriver) <> 0 OR ;
	ATC("Oracle", toCursorAdapter.cDBdriver) <> 0 OR ATC("IBM", toCursorAdapter.cDBdriver) <> 0 OR ATC("DB2", toCursorAdapter.cDBdriver) <> 0 OR ;
	ATC("MySQL", toCursorAdapter.cDBdriver) <> 0)

	tlDontReplaceEmptyDatesWithNull = .T.
ENDIF 	
*}V&U MS 2011-12-17

STORE "" TO lcFieldsToWriteNullWhenEmpty, lcUpdatablefieldlist, lcUpdatenamelist, lcCursorSchema, ;
			lcSign, lcKeyFieldList, lcInsertCmdRefreshFieldList, lcBufferUpd, lcReplaceStr

lcTableName = ALLTRIM(toCursorAdapter.TABLES)
llGUIDKeyField = .F.

*{JEI MS 27.06.2007
lnFieldCount = AFIELDS(laFieldList)
IF AT(",", tcKeyFieldList) = 0
	lnKeyRow = ASCAN(laFieldList, ALLTRIM(tcKeyFieldList), 1, ALEN(laFieldList, 1), 1, 15)
	IF lnKeyRow > 0 AND ALLTRIM(laFieldList[lnKeyRow,2]) = 'C'
		llGUIDKeyField = .T.
	ENDIF
ENDIF
*}JEI MS 27.06.2007

*{JEI MS 08.09.2006 Modified
IF VARTYPE(tcKeyFieldList) == "C"
*{JEI MS 27.06.2007 Modified
	IF !tlUpdateKeyField AND !llGUIDKeyField
		lcKeyFieldList = lcKeyFieldList + "," + tcKeyFieldList
	ENDIF
*}JEI MS 27.06.2007
ENDIF
*}JEI MS 08.09.2006

lcBuffer = "," + LOWER(STRTRAN(lcKeyFieldList," ","")) + ",timestamp_column," && exclude timestamp column
*{ V&U MS 2008-10-15
IF !EMPTY(tcNotUpdatableFieldList)
	lcBuffer = lcBuffer + LOWER(STRTRAN(tcNotUpdatableFieldList, " ", "")) + ","
*{ V&U MS 2009-09-16, Not include in InsertCmdRefreshFieldList
	lcBufferUpd = "," + LOWER(STRTRAN(tcNotUpdatableFieldList, " ", "")) + ","
*} V&U MS 2009-09-16
ENDIF
*} V&U MS 2008-10-15

FOR m.kk = 1 TO lnFieldCount

	lcFieldName  = ALLTRIM(laFieldList[m.kk,1])

* Create CursorSchema
	lcType     = ALLTRIM(laFieldList[m.kk,2])
	lcLen      = TRANSFORM(laFieldList[m.kk,3])
	lcDecimals = TRANSFORM(laFieldList[m.kk,4])
	
	*{ VM 2010-06-22
	IF lcType = "C" AND lcLen = "0" 
		* SQL 2008 VARCHAR(MAX) Field 
		lcType = "M" 
	ENDIF 
	*} VM 2010-06-22

	lcCursorSchema = lcCursorSchema + lcSign + lcFieldName + ' ' + lcType

	DO CASE
		CASE lcType = 'C'
			lcCursorSchema = lcCursorSchema + '(' + lcLen + ')'
		CASE lcType $ 'NF'
			lcCursorSchema = lcCursorSchema + '(' + lcLen + ', ' + lcDecimals + ')'
		CASE lcType = 'B'
			lcCursorSchema = lcCursorSchema + '(' + lcDecimals + ')'
	ENDCASE

*{JEI MS 22.08.2006
* Create InsertCmdRefreshFieldList
* V&U MS 2009-09-16, Not include NotUpdatableFieldList in InsertCmdRefreshFieldList
	IF lcType <> 'M' AND !("," + LOWER(lcFieldName) + "," $ lcBufferUpd)
*{JEI MS 08.09.2006 Modified
		lcInsertCmdRefreshFieldList = lcInsertCmdRefreshFieldList + ;
			IIF(EMPTY(lcInsertCmdRefreshFieldList), "", ",") + lcFieldName
*}JEI MS 08.09.2006
	ENDIF
*}JEI MS 22.08.2006

* Create UpdateNameList
*{V&U MS 2010-09-13
	IF !("," + LOWER(lcFieldName) + "," $ lcBufferUpd)
		lcUpdateNameList = lcUpdateNameList + lcSign + lcFieldName + " " + lcTableName + ;
			"." + lcFieldName
	ENDIF 
*}V&U MS 2010-09-13

* Create UpdatableFieldList
	IF !("," + LOWER(lcFieldName) + "," $ lcBuffer)
		lcUpdatableFieldList = lcUpdatableFieldList + ;
			IIF(EMPTY(lcUpdatableFieldList),"",", ") + ;
			lcFieldName
	ENDIF
	*{V&U MS 2011-12-17 5595
	IF !tlDontReplaceEmptyDatesWithNull AND INLIST(lcType, 'D', 'T') AND laFieldList[m.kk, 5]
		lcFieldsToWriteNullWhenEmpty = lcFieldsToWriteNullWhenEmpty + ;
				IIF(!EMPTY(lcFieldsToWriteNullWhenEmpty), ", ", "") + lcFieldName
	ENDIF 
	*}V&U MS 2011-12-17
	lcSign = ", "
ENDFOR
toCursorAdapter.CURSORSCHEMA = lcCursorSchema

*{V&U MS 2011-12-17 5595
IF !EMPTY(lcFieldsToWriteNullWhenEmpty)
	toCursorAdapter.cFieldsToWriteNullWhenEmpty = lcFieldsToWriteNullWhenEmpty
	lnFieldCount = ALINES(laDummy, lcFieldsToWriteNullWhenEmpty, 5, ",")
	IF lnFieldCount > 0
		DIMENSION toCursorAdapter.aReplaceEmptyWithNull[lnFieldCount]
		FOR lnCurrentField = 1 TO lnFieldCount 
			lcReplaceStr = 	"IF !ISNULL(" + laDummy[lnCurrentField] + ") AND EMPTY(" + laDummy[lnCurrentField] + ")" + CHR(13) + CHR(10) + ;
							"	REPLACE " + laDummy[lnCurrentField] + " WITH .Null. " + ;
							IIF(!EMPTY(toCursorAdapter.Alias), " IN " + toCursorAdapter.Alias, "") + CHR(13) + CHR(10) + ;
							"ENDIF"
			toCursorAdapter.aReplaceEmptyWithNull[lnCurrentField] = lcReplaceStr		
		ENDFOR
	ENDIF
ENDIF
*}V&U MS 2011-12-17

*{JEI MS 08.09.2006 Modified
IF !tlDontSendUpdates
	toCursorAdapter.UPDATENAMELIST =  lcUpdateNameList
	toCursorAdapter.UPDATABLEFIELDLIST = lcUpdatableFieldList
*{JEI RI 22.01.2007
	IF llGUIDKeyField
		toCursorAdapter.INSERTCMDREFRESHFIELDLIST = ""
		toCursorAdapter.INSERTCMDREFRESHKEYFIELDLIST = ""
	ELSE
*{JEI MS 22.08.2006
		IF !EMPTY(tcKeyFieldList)
			toCursorAdapter.INSERTCMDREFRESHFIELDLIST = lcInsertCmdRefreshFieldList
			toCursorAdapter.INSERTCMDREFRESHKEYFIELDLIST = tcKeyFieldList
		ENDIF
*}JEI MS 22.08.2006
	ENDIF
*}JEI RI 22.01.2007
ENDIF
*}JEI MS 08.09.2006
SELECT(lnOldAlilas)
ENDFUNC

*-------------------------------------------------------
* Function....: ToolboxEngine
* Called by...: VFX ToolBox
*
* Abstract....: This is the engine for Toolbox functionality.
*
* Returns.....: .T. - Skip It / .F.
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
DEFINE CLASS ToolboxEngine AS CUSTOM

	NAME = "ToolboxEngine"

	DefaultClassLib  = ''

	RedrawToolbox = .T.

	lDeferLoad = .F.

	lCustomizeMode = .T.  && true if instantiated for customizing the toolbox

	CurrentCategory = .NULL.
	oCategoryCollection = .NULL.

	FilterID = '' && current category filter ID
	FilterName = ''

	Category = '' && current category

	ScrollSpeed      = SCROLLSPEED_DEFAULT  && scroll speed within a category (in milliseconds)
	FontString       = FONT_DEFAULT
	BUILDERLOCK      = .F.  && run builder on drop
	ShowHelpText     = .T.  && show help text
	ShowAlwaysOnTop  = .T.  && show toolbox as Always On Top
	DblClickToOpen   = .F.  && require double-click to open hyperlink items
	ShowToolTips     = .T.  && show toolbox tooltips
	NamingConvention = 1    && how to name class items added to toolbox
	DropText         = ''   && template to use when drop a class on a code window
	CtrlDropText     = ''   && template to use when a ctrl+drop is done on a code window
	ColumnSetCurrentControl = .F. && set CurrentControl property when dropping control on a column
	ColumnRemoveText1       = .T. && prompt to remove Text1 property when new control dropped on column
	AllowMinimize    = .F.   && allow toolbox to be minimized

	LastCategoryID  = ''   && last displayed CategoryID

	lInitError = .F.

	nNewFilterNum = 0

	cFontName  = "Tahoma"
	nFontSize  = 8
	cFontStyle = 'N'

	cTopToolString = ''


	ADD OBJECT PROTECTED tmrBuilder AS BuilderDelay

	PROCEDURE INIT(lCustomizeMode, lNoOpen, loTBForm)
	LOCAL ARRAY aFileList[1]

	THIS.RestorePrefs()
	ADDPROPERTY(THIS,"oTBForm", loTBForm)
	THIS.oCategoryCollection = CREATEOBJECT("Collection")

	THIS.DefaultClassLib = FULLPATH(DEFAULT_CLASSLIB)
	IF !FILE(THIS.DefaultClassLib)
		THIS.DefaultClassLib = DEFAULT_CLASSLIB
		IF !FILE(THIS.DefaultClassLib)
* we didn't find the version on disk at HOME() + "Toolbox",
* so use version that's bound into the APP
			THIS.DefaultClassLib = INTERNAL_CLASSLIB
		ENDIF
	ENDIF

	THIS.lCustomizeMode = lCustomizeMode

	IF !m.lNoOpen
		THIS.lInitError = !THIS.LoadToolbox(.T.)
	ENDIF

	ENDPROC

	PROCEDURE DESTROY()
	IF USED("ToolboxCursor")
		USE IN ToolboxCursor
	ENDIF
	IF USED("Toolbox")
		USE IN Toolbox
	ENDIF
	IF USED("ToolType")
		USE IN ToolType
	ENDIF
	IF USED("VirtualCursor")
		USE IN VirtualCursor
	ENDIF
	ENDPROC

* we use BINDEVENT() in Toolbox form to bind to this in order to
* refresh toolbox as necessary -- DO NOT REMOVE (even though it's empty)!!!
	PROCEDURE RefreshUI()
*** do not remove!!!
	ENDPROC

	PROCEDURE ParseFontString()
	LOCAL cFontString
	LOCAL nFontSize

	IF EMPTY(THIS.FontString)
		.cFontString = FONT_DEFAULT
	ELSE
		cFontString = THIS.FontString
	ENDIF

	THIS.cFontName  = LEFT(m.cFontString, AT(",", m.cFontString) - 1)
	nFontSize  = SUBSTR(m.cFontString, AT(",", m.cFontString) + 1)
	THIS.nFontSize  = VAL(LEFT(m.nFontSize, AT(",", m.nFontSize) - 1))
	THIS.cFontStyle = SUBSTR(m.cFontString, AT(",", m.cFontString, 2) + 1)
	ENDPROC

	FUNCTION SavePrefs()
	LOCAL oResource
	LOCAL cTopTool
	LOCAL i

	oResource = NEWOBJECT("FoxResource")

	oResource.LOAD("TOOLBOX")
	oResource.SET("FilterID", THIS.FilterID)
	oResource.SET("ScrollSpeed", THIS.ScrollSpeed)
	oResource.SET("FontString", THIS.FontString)
	oResource.SET("BuilderLock", THIS.BUILDERLOCK)
	oResource.SET("ShowHelpText", THIS.ShowHelpText)
	oResource.SET("ShowToolTips", THIS.ShowToolTips)
	oResource.SET("ShowAlwaysOnTop", THIS.ShowAlwaysOnTop)
	oResource.SET("DblClickToOpen", THIS.DblClickToOpen)
	oResource.SET("LastCategoryID", THIS.LastCategoryID)
	oResource.SET("NamingConvention", THIS.NamingConvention)
	oResource.SET("DropText", THIS.DropText)
	oResource.SET("CtrlDropText", THIS.CtrlDropText)
	oResource.SET("ColumnSetCurrentControl", THIS.ColumnSetCurrentControl)
	oResource.SET("ColumnRemoveText1", THIS.ColumnRemoveText1)
	oResource.SET("AllowMinimize", THIS.AllowMinimize)

* save the first displayed tool button in each category as a comma-delimited list
	cTopTool = ''
	FOR i = 1 TO THIS.oCategoryCollection.COUNT
		cTopTool = m.cTopTool + IIF(EMPTY(m.cTopTool), '', ',') + THIS.oCategoryCollection(m.i).UniqueID + '=' + THIS.oCategoryCollection(m.i).TopToolID
	ENDFOR
	oResource.SET("TopTool", m.cTopTool)
	oResource.SAVE("TOOLBOX")
	oResource = .NULL.
	ENDFUNC

	FUNCTION RestorePrefs()
	LOCAL oResource
	LOCAL i
	LOCAL nCnt
	LOCAL cUniqueID
	LOCAL cTopToolID
	LOCAL ARRAY aTopTool[1]

	oResource = NEWOBJECT("FoxResource")
	oResource.LOAD("TOOLBOX")

	THIS.FilterID         = NVL(m.oResource.GET("FilterID"), '')
	THIS.ScrollSpeed      = NVL(m.oResource.GET("ScrollSpeed"), THIS.ScrollSpeed)
	THIS.FontString       = NVL(m.oResource.GET("FontString"), THIS.FontString)
	THIS.BUILDERLOCK      = NVL(m.oResource.GET("BuilderLock"), THIS.BUILDERLOCK)
	THIS.ShowHelpText     = NVL(m.oResource.GET("ShowHelpText"), THIS.ShowHelpText)
	THIS.ShowToolTips     = NVL(m.oResource.GET("ShowToolTips"), THIS.ShowToolTips)
	THIS.ShowAlwaysOnTop  = NVL(m.oResource.GET("ShowAlwaysOnTop"), THIS.ShowAlwaysOnTop)
	THIS.DblClickToOpen   = NVL(m.oResource.GET("DblClickToOpen"), THIS.DblClickToOpen)
	THIS.LastCategoryID   = NVL(m.oResource.GET("LastCategoryID"), '')

	THIS.NamingConvention = NVL(m.oResource.GET("NamingConvention"), THIS.NamingConvention)
	THIS.DropText         = NVL(m.oResource.GET("DropText"), THIS.DropText)
	THIS.CtrlDropText     = NVL(m.oResource.GET("CtrlDropText"), THIS.CtrlDropText)
	THIS.ColumnSetCurrentControl = NVL(m.oResource.GET("ColumnSetCurrentControl"), THIS.ColumnSetCurrentControl)
	THIS.ColumnRemoveText1       = NVL(m.oResource.GET("ColumnRemoveText1"), THIS.ColumnRemoveText1)

	THIS.AllowMinimize = NVL(m.oResource.GET("AllowMinimize"), THIS.AllowMinimize)

	IF VARTYPE(THIS.NamingConvention) <> 'N'
		THIS.NamingConvention = 1
	ENDIF
	IF VARTYPE(THIS.DropText) <> 'C'
		THIS.DropText = ''
	ENDIF
	IF VARTYPE(THIS.CtrlDropText) <> 'C'
		THIS.CtrlDropText = ''
	ENDIF

	THIS.ParseFontString()

* retrieve the first tool to display in each category
	THIS.cTopToolString = NVL(m.oResource.GET("TopTool"), '')

	oResource = .NULL.
	ENDFUNC

* open the tables associated with the toolbox
	FUNCTION OpenToolbox(lCustomizeMode, cAlias, loTBForm)
*PUBLIC oTBAdptr
	IF PCOUNT() == 0
		lCustomizeMode = THIS.lCustomizeMode
	ENDIF

	IF VARTYPE(m.cAlias) <> 'C' OR EMPTY(m.cAlias)
		cAlias = "ToolboxCursor"
	ENDIF

	IF !USED(m.cAlias)
		IF TYPE("goProgram.class") ="C" AND goProgram.nVFXSysTableLoc >= 1
			*{V&U MS 2010-05-13, 2014-04-28
			oTBAdptr = OPENTABLE("vfxToolBox", "IDVFXTBOX", .F., ;
				"INDEX ON displayord TAG DISPLAYORD ADDITIVE" + CHR(10) + CHR(13) + ;
				"INDEX ON uniqueid TAG UNIQUEID ADDITIVE" + CHR(10) + CHR(13) + ;
				"INDEX ON showtype TAG SHOWTYPE ADDITIVE" + CHR(10) + CHR(13) + ;
				"INDEX ON tooltypeid TAG TOOLTYPEID ADDITIVE" + CHR(10) + CHR(13), ;
				.T., loTBForm, m.cAlias, "cAppVFXDataAccess", .F., .F., .F., .F., "USER")
			*}V&U MS 2010-05-13, 2014-04-28
			SET ORDER TO TAG DISPLAYORD
		ELSE
			USE vfxToolBox IN 0 ORDER TAG DISPLAYORD AGAIN ALIAS (m.cAlias)
		ENDIF
		IF m.lCustomizeMode
			SET ORDER TO
		ENDIF
	ENDIF

	RETURN USED(m.cAlias)
*RETURN oResAdptr
	ENDFUNC

* open the ToolType table
	FUNCTION OpenToolType(cAlias, loTBForm)
	LOCAL nSelect

	nSelect = SELECT()

	IF VARTYPE(m.cAlias) <> 'C' OR EMPTY(m.cAlias)
		cAlias = "ToolType"
	ENDIF

	IF USED(m.cAlias)
		USE IN (m.cAlias)
	ENDIF

	IF TYPE("goProgram.class") ="C" AND goProgram.nVFXSysTableLoc >= 1
		*{V&U MS 2010-05-13, 2014-04-28
		oResAdptr = OPENTABLE("vfxToolType", "IDVFXTTYPE", .F., "", .T., loTBForm, m.cAlias, ;
					"cAppVFXDataAccess", .F., .F., .F., .F., "USER")
		*}V&U MS 2010-05-13, 2014-04-28
	ELSE
		USE vfxToolType IN 0 AGAIN ALIAS (m.cAlias)
	ENDIF

	SELECT (m.nSelect)

	RETURN USED(m.cAlias)
	ENDFUNC

	FUNCTION GenerateUniqueID()
	RETURN "user." + SYS(2015)
	ENDFUNC

* --- START OF TOOLBOX METHODS ---
	FUNCTION LoadToolbox(lFirstLoad)
	LOCAL nSelect
	LOCAL lSuccess
	LOCAL oCategory
	LOCAL oRec
	LOCAL nFilterCnt
	LOCAL ARRAY aTopTool[1]
	LOCAL nCnt
	LOCAL i
	LOCAL cTopToolID
	LOCAL ARRAY aFilter[1]

	IF THIS.lDeferLoad
		RETURN
	ENDIF

	IF !m.lFirstLoad AND THIS.lCustomizeMode
		RETURN
	ENDIF

	nSelect = SELECT()

	IF USED("ToolboxCursor")
		USE IN ToolboxCursor
	ENDIF

	THIS.oCategoryCollection.REMOVE(-1)

	IF m.lFirstLoad
		THIS.CurrentCategory = .NULL.
	ENDIF

	m.lSuccess = THIS.OpenToolbox(, , THIS.oTBForm)
	IF m.lSuccess
*oRes = THIS.OpenToolbox()
*IF USED("ToolboxCursor")
		nFilterCnt = -1
		IF !THIS.lCustomizeMode AND !EMPTY(THIS.FilterID)
			IF SEEK(THIS.FilterID, "ToolboxCursor", "UniqueID")
				SELECT ToolName ;
					FROM ToolboxCursor ;
					WHERE ;
					ParentID == THIS.FilterID AND !Inactive ;
					INTO ARRAY aFilter
				m.nFilterCnt = _TALLY
			ELSE
				THIS.FilterID = ''
			ENDIF
		ENDIF

* convert the string of first tool to display within categories to an array
		nCnt = ALINES(aTopTool, THIS.cTopToolString, ',')

* add in normal category tabs
		SELECT ToolboxCursor
		SCAN ALL FOR (ShowType == SHOWTYPE_CATEGORY OR ShowType == SHOWTYPE_FAVORITES)
			IF !Inactive OR THIS.lCustomizeMode
* if we have a filter set, then make sure this category
* is in that filter
				IF m.nFilterCnt < 0 OR ASCAN(aFilter, PADR(ToolboxCursor.UniqueID, LEN(ToolboxCursor.ToolName))) > 0
					oCategory = CREATEOBJECT("ToolboxCategory")
					oCategory.UniqueID = RTRIM(ToolboxCursor.UniqueID)
					oCategory.ToolTypeID = ToolboxCursor.ToolTypeID
					*{V&U MS 2010-05-16
					oCategory.IDVFXTTYPE  = ToolboxCursor.IDVFXTTYPE
					*}V&U MS 2010-05-16					
					oCategory.ToolType   = ToolboxCursor.ToolType
					oCategory.ToolName   = RTRIM(ToolboxCursor.ToolName)
					oCategory.ParentID   = ToolboxCursor.ParentID
					oCategory.ClassType  = ToolboxCursor.ClassType
					oCategory.SetID      = ToolboxCursor.SetID
					oCategory.ClassName  = ToolboxCursor.ClassName
					oCategory.CLASSLIB   = ToolboxCursor.CLASSLIB
					oCategory.TOOLTIP    = ToolboxCursor.TOOLTIP
					oCategory.HelpFile   = ToolboxCursor.HelpFile
					oCategory.HelpID     = ToolboxCursor.HelpID
					oCategory.USER       = ToolboxCursor.USER

* determine the topmost tool to display in this category
					FOR m.i = 1 TO m.nCnt
						IF oCategory.UniqueID == GETWORDNUM(aTopTool[m.i], 1, '=')
							m.cTopToolID = GETWORDNUM(aTopTool[m.i], 2, '=')

							IF !EMPTY(m.cTopToolID)
								oCategory.TopToolID = m.cTopToolID
							ENDIF
							EXIT
						ENDIF
					ENDFOR


					THIS.oCategoryCollection.ADD(oCategory, oCategory.UniqueID)

				ENDIF
			ENDIF
		ENDSCAN

		IF m.lFirstLoad AND THIS.oCategoryCollection.COUNT > 0
			THIS.CurrentCategory = THIS.oCategoryCollection.ITEM(1)
		ENDIF

		IF TYPE("goProgram.class") ="C" AND goProgram.nVFXSysTableLoc >= 1
*Do nothing
		ELSE
			IF USED("ToolboxItemCursor")
				USE IN ToolboxItemCursor
			ENDIF
		ENDIF

		THIS.RefreshUI()
	ELSE
* To-Do: error message goes here
	ENDIF

	THIS.RedrawToolbox = .T.

	SELECT (m.nSelect)

	RETURN m.lSuccess
	ENDFUNC

* return a collection of UniqueID's for all tools in a given category
	FUNCTION GetToolsInCategory(m.cCategoryID)
	LOCAL nSelect
	LOCAL oToolCollection
	LOCAL i
	LOCAL nCnt
	LOCAL ARRAY aToolItems[1]

	nSelect = SELECT()

	m.oToolCollection = CREATEOBJECT("Collection")
	SELECT UniqueID ;
		FROM ToolboxCursor ;
		WHERE ParentID == m.cCategoryID AND ShowType == SHOWTYPE_TOOL AND !Inactive ;
		ORDER BY DisplayOrd ;
		INTO ARRAY aToolItems
	nCnt = _TALLY
	FOR i = 1 TO m.nCnt
		oToolCollection.ADD(aToolItems[m.i, 1])
	ENDFOR

	SELECT (m.nSelect)

	RETURN m.oToolCollection
	ENDFUNC

	FUNCTION GetRecord(cUniqueID)
	LOCAL oRec
	LOCAL nSelect

	nSelect = SELECT()

	oRec = .NULL.
	cUniqueID = PADR(m.cUniqueID, LEN(ToolboxCursor.UniqueID))

	IF SEEK(m.cUniqueID, "ToolboxCursor", "UniqueID")
		SELECT ToolboxCursor
		SCATTER MEMO NAME oRec
	ENDIF

	SELECT (m.nSelect)

	RETURN m.oRec
	ENDFUNC

* set the current toolbox category
	FUNCTION SetCategory(cUniqueID)
	LOCAL oCurrentCategory
	LOCAL oCategoryRec

	oCategoryRec = .NULL.
	oCurrentCategory = THIS.GetCategory(m.cUniqueID)
	IF !ISNULL(oCurrentCategory)
		THIS.LastCategoryID = m.cUniqueID
		oCategoryRec = THIS.GetRecord(m.cUniqueID)
		IF !ISNULL(oCategoryRec)
			THIS.CurrentCategory = oCurrentCategory

			THIS.RunAddIns(oCategoryRec.UniqueID, "ONLOAD")
		ENDIF
	ELSE
		THIS.LastCategoryID = ''
	ENDIF

	RETURN oCategoryRec
	ENDFUNC

* return a toolbox category by it's UniqueID
	FUNCTION GetCategory(cUniqueID)
	LOCAL oCategory

	TRY
		oCategory = THIS.oCategoryCollection.ITEM(RTRIM(m.cUniqueID))
	CATCH
		oCategory = .NULL.
	ENDTRY

	RETURN oCategory
	ENDFUNC

	FUNCTION GetCategoryByName(cCategory)
	LOCAL nSelect
	LOCAL oRec

	nSelect = SELECT()

	oRec = .NULL.

	SELECT ToolboxCursor
	LOCATE FOR UPPER(ALLTRIM(ToolName)) == UPPER(ALLTRIM(m.cCategory)) AND (ShowType == SHOWTYPE_CATEGORY OR ShowType == SHOWTYPE_FAVORITES)
	IF FOUND()
		SELECT ToolboxCursor
		SCATTER MEMO NAME oRec
	ENDIF

	SELECT (m.nSelect)

	RETURN m.oRec
	ENDFUNC

* Customize the toolbox
* [cUniqueID] = initial category to position on
	FUNCTION Customize(cCategoryID)
	LOCAL lSuccess

	IF VARTYPE(m.cCategoryID) <> 'C'
		cCategoryID = THIS.CurrentCategory.UniqueID
	ENDIF

	THIS.SavePrefs()

	IF TYPE("goProgram.class") ="C" AND goProgram.nVFXSysTableLoc >= 1
* Do nothing
	ELSE
		IF USED("vfxToolbox")
			USE IN vfxToolbox
		ENDIF
		IF USED("ToolboxCursor")
			USE IN ToolboxCursor
		ENDIF
	ENDIF

	lnDatasession = SET("Datasession")
	DO FORM ("vfxToolboxCustomize") WITH m.cCategoryID, THIS, lnDatasession TO lSuccess

	IF VARTYPE(m.lSuccess) <> 'L'
		lSuccess = .F.
	ENDIF

	IF THIS.OpenToolbox(, , THIS.oTBForm)
*		oRes = THIS.OpenToolbox()
*		IF USED("ToolboxCursor")
		IF m.lSuccess
			THIS.RestorePrefs()
			THIS.LoadToolbox()
		ENDIF
	ELSE
		RETURN TO MASTER
	ENDIF


	RETURN m.lSuccess
	ENDFUNC

* Show the tool properties
* [cUniqueID] = tool to show properties for
	FUNCTION ShowProperties(oToolItem, lNoAutoSave)
	LOCAL lSuccess

	lSuccess = .F.
	IF VARTYPE(m.oToolItem) == 'O'
		lSuccess = m.oToolItem.OnShowProperties()

		IF VARTYPE(m.lSuccess) <> 'L'
			lSuccess = .F.
		ENDIF

		IF m.lSuccess AND !m.lNoAutoSave
			THIS.SaveToolItem(m.oToolItem, .F.)
			THIS.LoadToolbox()
		ENDIF
	ENDIF

	RETURN m.lSuccess
	ENDFUNC

	FUNCTION ShowPropertiesForm(oToolItem)
	LOCAL lSuccess

	lnDatasession = SET("Datasession")
	DO FORM ("vfxToolboxProperties") WITH m.oToolItem, lnDatasession TO m.lSuccess

	IF THIS.OpenToolbox(, , THIS.oTBForm)
		THIS.LoadToolbox()
	ENDIF

	IF VARTYPE(m.lSuccess) <> 'L'
		lSuccess = .F.
	ENDIF

	RETURN m.lSuccess
	ENDFUNC

	FUNCTION ShowCategoryPropertiesForm(oToolItem)
	LOCAL lSuccess

	lnDatasession = SET("Datasession")
	DO FORM ("vfxToolboxCategoryProperties") WITH m.oToolItem, lnDatasession TO m.lSuccess

	IF THIS.OpenToolbox(, , THIS.oTBForm)
		THIS.LoadToolbox()
	ENDIF

	IF VARTYPE(m.lSuccess) <> 'L'
		lSuccess = .F.
	ENDIF

	RETURN m.lSuccess
	ENDFUNC

* add toolbox item to favorites
	FUNCTION AddToFavorites(cToolID)
	LOCAL nSelect

	nSelect = SELECT()

* add in favorites
	SELECT ToolboxCursor
	LOCATE FOR ShowType == SHOWTYPE_FAVORITES AND !Inactive
	IF FOUND()
		THIS.CopyTool(m.cToolID, ToolboxCursor.UniqueID)
	ENDIF

	SELECT (m.nSelect)
	ENDFUNC

* <cToolID> = UniqueID of tool to move/copy
* <oRec>    = target to move to -- can be category or tool
* [lCopy]   = make a copy of the tool rather than just moving it
	FUNCTION MoveTool(cToolID, cTargetID, lCopy)
	LOCAL oToolRec
	LOCAL i
	LOCAL nCnt
	LOCAL nDisplayOrd
	LOCAL oTargetRec
	LOCAL ARRAY aDisplayOrd[1]

	oToolRec   = THIS.GetToolObject(m.cToolID)
	oTargetRec = THIS.GetToolObject(m.cTargetID)

	IF !ISNULL(m.oToolRec)
		IF INLIST(m.oTargetRec.ShowType, SHOWTYPE_CATEGORY, SHOWTYPE_FAVORITES) && we're moving to a new category
			oToolRec.ParentID = m.oTargetRec.UniqueID
		ELSE
			oToolRec.ParentID = m.oTargetRec.ParentID  && get the category of the item we dropped on
		ENDIF

		SELECT UniqueID ;
			FROM ToolboxCursor ;
			WHERE ;
			ParentID == m.oToolRec.ParentID AND ;
			ShowType == SHOWTYPE_TOOL ;
			ORDER BY DisplayOrd ;
			INTO ARRAY aDisplayOrd
		nCnt = _TALLY
		nDisplayOrd = 0
		FOR i = 1 TO m.nCnt
			IF SEEK(aDisplayOrd[m.i], "ToolboxCursor", "UniqueID")
				nDisplayOrd = m.nDisplayOrd + 1
				IF aDisplayOrd[m.i] == m.oTargetRec.UniqueID
					nDisplayOrd = m.nDisplayOrd + 1
				ENDIF
				REPLACE ;
					DisplayOrd WITH m.nDisplayOrd ;
					IN ToolboxCursor
			ENDIF
		ENDFOR

		IF m.oTargetRec.ShowType == SHOWTYPE_CATEGORY && we're moving to a new category
			oToolRec.DisplayOrd = m.nDisplayOrd + 1 && always add to end when moving between categories
		ELSE
			oToolRec.DisplayOrd = m.oTargetRec.DisplayOrd
		ENDIF

		IF m.lCopy
			oToolRec.LockAdd    = .F.
			oToolRec.LockDelete = .F.
			oToolRec.LockRename = .F.

			THIS.NEWITEM(m.oToolRec)
		ELSE
			THIS.SaveItem(m.oToolRec)
			THIS.LoadToolbox()
		ENDIF
	ENDIF
	ENDFUNC

	FUNCTION CopyTool(cToolID, cTargetID)
	RETURN THIS.MoveTool(cToolID, cTargetID, .T.)
	ENDFUNC

* return ToolType record object given its uniqueid
	FUNCTION GetToolTypeRec(cUniqueID)
	LOCAL nSelect
	LOCAL oToolType

	nSelect = SELECT()

	oToolType = .NULL.
	IF THIS.OpenToolType(, THIS.oTBForm)
		cUniqueID = PADR(m.cUniqueID, LEN(ToolType.UniqueID))
		SELECT ToolType
		LOCATE FOR UniqueID == m.cUniqueID
		IF FOUND()
			SCATTER MEMO NAME m.oToolType
		ENDIF
	ENDIF

	SELECT (m.nSelect)

	RETURN m.oToolType
	ENDFUNC

* Find the tool in Toolbox.dbf and create
* the object specified in ClassLib/ClassName field.
* Pass this new object back
	FUNCTION GetToolObject(cUniqueID)
	LOCAL oToolObject
	LOCAL cClassName
	LOCAL cClassLib
	LOCAL oException
	LOCAL cAlias
	LOCAL nSelect
	LOCAL lVirtual
	LOCAL ARRAY aFileList[1]

	IF VARTYPE(m.cUniqueID) <> 'C' OR EMPTY(m.cUniqueID)
		RETURN .NULL.
	ENDIF

	oToolObject = .NULL.

	lVirtual = (LEFT(m.cUniqueID, 8) == "virtual.")
	IF m.lVirtual AND USED("VirtualCursor")
		cAlias = "VirtualCursor"
	ELSE
		cAlias = "ToolboxCursor"
	ENDIF

	IF SEEK(m.cUniqueID, m.cAlias, "UniqueID")
		nSelect = SELECT()
		SELECT (m.cAlias)

		cClassLib  = ALLTRIM(CLASSLIB)
		cClassName = ALLTRIM(ClassName)

		IF EMPTY(CLASSLIB) OR !FILE(CLASSLIB)
			cClassLib = THIS.DefaultClassLib
		ENDIF

		IF EMPTY(ClassName)
			DO CASE
				CASE ShowType == SHOWTYPE_CATEGORY
					cClassName = CATEGORYCLASS_GENERAL

				CASE ShowType == SHOWTYPE_FAVORITES
					cClassName = CATEGORYCLASS_FAVORITES

				CASE ShowType == SHOWTYPE_TOOL
					cClassName = ITEMCLASS_TOOL

				OTHERWISE
					cClassName = ITEMCLASS_ROOT
			ENDCASE
		ENDIF

		TRY
			oToolObject = NEWOBJECT(m.cClassName, m.cClassLib)
		CATCH TO oException
			goProgram.vfxmessagebox(oException.MESSAGE + "(" + oException.PROCEDURE + ")", MB_ICONSTOP, TOOLBOX_LOC)
		ENDTRY

* we couldn't find the specified class, so revert to the _root class
		IF ISNULL(m.oToolObject)
			TRY
				oToolObject = NEWOBJECT(ITEMCLASS_ROOT, THIS.DefaultClassLib)
			CATCH
			ENDTRY
		ENDIF

		IF !ISNULL(m.oToolObject)
			oToolObject.oEngine    = THIS
			oToolObject.UniqueID   = UniqueID
			oToolObject.ShowType   = ShowType
			oToolObject.ToolTypeID = ToolTypeID
			*{V&U MS 2010-05-16
			oToolObject.IDVFXTTYPE  = IDVFXTTYPE
			*}V&U MS 2010-05-16
			oToolObject.ToolType   = RTRIM(ToolType)
			oToolObject.ParentID   = ParentID
			oToolObject.ToolName   = RTRIM(ToolName)
			oToolObject.ClassName  = ClassName
			oToolObject.CLASSLIB   = CLASSLIB
			oToolObject.ToolData   = ToolData
			oToolObject.TOOLTIP    = TOOLTIP
			oToolObject.ImageFile  = THIS.EvalText(IIF(EMPTY(ImageFile), m.oToolObject.ImageFile, ImageFile))
			oToolObject.SetID      = SetID
			oToolObject.LockAdd    = LockAdd && TRUE indicates it can't be added to (for categories)
			oToolObject.LockDelete = LockDelete && TRUE indicates it can't be deleted
			oToolObject.LockRename = LockRename && TRUE indicates it can't be renamed

			oToolObject.USER       = USER
			oToolObject.DisplayOrd = DisplayOrd
			oToolObject.HelpFile   = HelpFile
			oToolObject.HelpID     = HelpID

			oToolObject.Inactive   = Inactive
			oToolObject.IsVirtual  = m.lVirtual
		ENDIF

		SELECT (m.nSelect)
	ENDIF

	RETURN m.oToolObject
	ENDFUNC

* -- Standard Events
	FUNCTION OnRightClick(cUniqueID)
	LOCAL oToolObject
	LOCAL oContextMenu

	oToolObject  = .NULL.
	oContextMenu = .NULL.

	IF VARTYPE(m.cUniqueID) == 'C' AND !EMPTY(m.cUniqueID)
		oToolObject = THIS.GetToolObject(m.cUniqueID)
	ENDIF
	IF VARTYPE(m.oToolObject) == 'O'
		oContextMenu = m.oToolObject.OnRightClick()
	ELSE
		oContextMenu = NEWOBJECT("ContextMenu")
		IF TYPE("_oToolbox") == 'O' AND !ISNULL(_oToolbox)
			oContextMenu.ShowInScreen = (_oToolbox.DOCKABLE == 0)
		ENDIF
	ENDIF

	RETURN m.oContextMenu
	ENDFUNC

	FUNCTION OnClick(cUniqueID)
	LOCAL oToolObject

	oToolObject = THIS.GetToolObject(m.cUniqueID)
	IF !ISNULL(m.oToolObject)
		m.oToolObject.OnClick()
	ENDIF

	RETURN
	ENDFUNC

* Create a new category
	FUNCTION AddCategory(cCategoryName, cToolTip, cToolTypeID)
	LOCAL nSelect
	LOCAL oCategory
	LOCAL nDisplayOrd
	LOCAL oFilterItem
	LOCAL oException
	LOCAL oToolType

	m.nSelect = SELECT()
	m.oCategory = .NULL.

	IF VARTYPE(m.cCategoryName) <> 'C' OR EMPTY(m.cCategoryName)
		lnDatasession = SET("Datasession")
		DO FORM ("vfxToolboxNewCategory") WITH THIS, lnDatasession TO m.oCategory
	ELSE
		IF VARTYPE(m.cToolTypeID) <> 'C' OR EMPTY(m.cToolTypeID)
			m.cToolTypeID = "CATEGORY.GENERAL"
		ENDIF

		oToolType = THIS.GetToolTypeRec(m.cToolTypeID)

		IF !ISNULL(m.oToolType)
			cClassName = ALLTRIM(m.oToolType.ClassName)
			cClassLib  = ALLTRIM(m.oToolType.CLASSLIB)

			IF VARTYPE(m.cToolTip) <> 'C'
				cToolTip = m.oToolType.TOOLTIP
			ENDIF


			IF VARTYPE(m.cClassName) <> 'C'
				cClassName = CATEGORYCLASS_GENERAL
			ENDIF
			IF VARTYPE(m.cClassLib) <> 'C' OR EMPTY(m.cClassLib)
				cClassLib = THIS.DefaultClassLib
			ENDIF

			TRY
				oCategory = NEWOBJECT(m.cClassName, m.cClassLib)
			CATCH TO oException
				oCategory = .NULL.
				goProgram.vfxmessagebox(oException.MESSAGE + CHR(10) + CHR(10) + m.cClassName + "(" + m.cClassLib + ")", MB_ICONEXCLAMATION, TOOLBOX_LOC)
			ENDTRY

			IF VARTYPE(m.oCategory) == 'O'
				WITH m.oCategory
					.ToolName   = m.cCategoryName
					.TOOLTIP    = m.cToolTip
					.ClassName  = m.cClassName
					.CLASSLIB   = IIF(m.cClassLib == THIS.DefaultClassLib, '', m.cClassLib)
					.ToolTypeID = m.oToolType.UniqueID
					*{V&U MS 2010-05-16
					.IDVFXTTYPE  = m.oToolType.IDVFXTTYPE
					*}V&U MS 2010-05-16
					.ToolType   = RTRIM(m.oToolType.ToolType)
				ENDWITH
			ENDIF
		ENDIF
	ENDIF

	IF VARTYPE(m.oCategory) == 'O'
* find the max
		SELECT MAX(DisplayOrd) ;
			FROM ToolboxCursor ;
			WHERE EMPTY(ParentID) ;
			INTO ARRAY aDisplayOrd
		IF _TALLY > 0 AND !ISNULL(aDisplayOrd[1])
			m.nDisplayOrd = aDisplayOrd[1] + 1
		ELSE
			m.nDisplayOrd = 0
		ENDIF

		WITH m.oCategory
			.UniqueID       = THIS.GenerateUniqueID()
			.ParentID       = ''
			.ImageFile      = ''
			.DisplayOrd     = m.nDisplayOrd
			.SetID          = ''
			.USER           = ''
		ENDWITH

		IF THIS.NEWITEM(m.oCategory)
			IF !EMPTY(THIS.FilterID)
* if we're currently filtered, then
* add this category to the current filter
				oFilterItem = NEWOBJECT(FILTERCLASS_ITEM, THIS.DefaultClassLib)
				WITH m.oFilterItem
					.UniqueID       = THIS.GenerateUniqueID()
					.ParentID       = THIS.FilterID
					.ToolTypeID     = ''
					*{V&U MS 2010-05-16
					.IDVFXTTYPE     = 0  
					*}V&U MS 2010-05-16
					.ToolType       = ''
					.ToolName       = m.oCategory.UniqueID
					.TOOLTIP        = ''
					.ImageFile      = ''
					.USER           = ''
				ENDWITH

				THIS.NEWITEM(m.oFilterItem)
			ENDIF
		ELSE
			oCategory = .NULL.
		ENDIF
	ENDIF

	IF THIS.OpenToolbox(, , THIS.oTBForm)
		THIS.LoadToolbox()
	ENDIF

	SELECT (m.nSelect)

	RETURN m.oCategory
	ENDFUNC

	FUNCTION SaveToolItem(oToolItem, lCheckForDuplicate)
	LOCAL lCheckForDuplicate
	LOCAL nSelect
	LOCAL lDoUpdate
	LOCAL cToolData

	IF VARTYPE(m.oToolItem) <> 'O'
		RETURN .F.
	ENDIF

	nSelect = SELECT()

	lDoUpdate = .F.
	IF !EMPTY(m.oToolItem.UniqueID) AND SEEK(m.oToolItem.UniqueID, "ToolboxCursor", "UniqueID")
		lDoUpdate = .T.
	ELSE
		IF lCheckForDuplicate
			SELECT ToolboxCursor
			LOCATE FOR ParentID == m.oToolItem.ParentID AND SetID == m.oToolItem.SetID AND ToolData == m.oToolItem.ToolData
			lDoUpdate = FOUND()
		ENDIF
	ENDIF

	IF m.lDoUpdate
		THIS.SaveItem(m.oToolItem)
	ELSE
		THIS.NEWITEM(m.oToolItem)
	ENDIF

	IF CURSORGETPROP("Buffering","ToolboxCursor") = 3 OR CURSORGETPROP("Buffering","ToolboxCursor") = 5
		SELECT ToolboxCursor
		TABLEUPDATE(.T.)
	ENDIF

	SELECT (nSelect)

	RETURN .T.
	ENDFUNC

* add an item to the Toolbox cursor
	FUNCTION CreateToolItem(cCategoryID, cToolName, cToolTip, cToolTypeID, cImageFile, cSetID, cUser)
	LOCAL oRec
	LOCAL nSelect
	LOCAL nDisplayOrd
	LOCAL cHomeDir
	LOCAL lDoUpdate
	LOCAL oToolItem
	LOCAL oDataValue
	LOCAL cToolData
	LOCAL cClassLib
	LOCAL cClassName
	LOCAL oToolType
	LOCAL ARRAY aDisplayOrd[1]

	IF VARTYPE(cCategoryID) <> 'C'
		RETURN .F.
	ENDIF

	IF VARTYPE(cToolTip) <> 'C'
		cToolTip = ''
	ENDIF
	IF VARTYPE(cImageFile) <> 'C'
		cImageFile = ''
	ENDIF
	IF VARTYPE(cToolTypeID) <> 'C'
		cToolTypeID = ''
	ENDIF
	IF VARTYPE(cSetID) <> 'C'
		cSetID = ''
	ENDIF
	IF VARTYPE(cUser) <> 'C'
		cUser = ''
	ENDIF


	nSelect = SELECT()

	oToolItem = .NULL.

* make sure the category exists
	m.oRec = THIS.GetRecord(RTRIM(m.cCategoryID))
	IF !ISNULL(m.oRec)
* Find the type in ToolType table and get the class library and name from that
		oToolType = THIS.GetToolTypeRec(m.cToolTypeID)
		IF !ISNULL(m.oToolType)
			cClassLib = m.oToolType.CLASSLIB
			cClassName = m.oToolType.ClassName

			IF EMPTY(m.cClassName)
				cClassName = ITEMCLASS_ROOT
				cClassLib  = THIS.DefaultClassLib
			ENDIF
			IF EMPTY(m.cClassLib)
				cClassLib = THIS.DefaultClassLib
			ENDIF

* create an instance of the class
			TRY
				oToolItem = NEWOBJECT(m.cClassName, m.cClassLib)
			CATCH TO oException
				oToolItem = .NULL.
				IF TYPE("goProgram.class") ="C"
					goProgram.vfxmessagebox(oException.MESSAGE, MB_ICONEXCLAMATION, TOOLBOX_LOC)
				ELSE
					MESSAGEBOX(oException.MESSAGE, MB_ICONEXCLAMATION, TOOLBOX_LOC)
				ENDIF
			ENDTRY

			IF VARTYPE(m.oToolItem) == 'O'
				cCategoryID = PADR(m.cCategoryID, LEN(ToolboxCursor.ParentID))

* find the max
				SELECT MAX(DisplayOrd) ;
					FROM ToolboxCursor ;
					WHERE ParentID == m.cCategoryID ;
					INTO ARRAY aDisplayOrd
				IF _TALLY > 0 AND !ISNULL(aDisplayOrd[1])
					nDisplayOrd = aDisplayOrd[1] + 1
				ELSE
					nDisplayOrd = 0
				ENDIF

				WITH m.oToolItem
					.UniqueID       = THIS.GenerateUniqueID()
					.ParentID       = m.cCategoryID
					.ToolTypeID     = m.oToolType.UniqueID
					*{V&U MS 2010-05-16
					.IDVFXTTYPE     = m.oToolType.IDVFXTTYPE
					*}V&U MS 2010-05-16					
					.ToolType       = RTRIM(m.oToolType.ToolType)
					.ToolName       = m.cToolName
					.TOOLTIP        = m.cToolTip
					.ImageFile      = IIF(EMPTY(m.cImageFile), .ImageFile, THIS.RelativeToHome(m.cImageFile))
					.ClassName      = m.cClassName
					.CLASSLIB       = IIF(m.cClassLib == THIS.DefaultClassLib, '', m.cClassLib)
					.DisplayOrd     = m.nDisplayOrd
					.SetID          = RTRIM(m.cSetID)
					.USER           = m.cUser
				ENDWITH
			ENDIF
		ENDIF
	ENDIF

	SELECT (m.nSelect)

	RETURN m.oToolItem
	ENDFUNC

* create a toolbox item of scrap text
	FUNCTION CreateToolItemScrap(cCategoryID, cText)
	LOCAL i
	LOCAL nCnt
	LOCAL cCaption
	LOCAL oToolItem
	LOCAL cImageFile
	LOCAL aText[1]

	m.cUniqueID = ''
	IF VARTYPE(cText) == 'C' AND !EMPTY(m.cText)
		cCaption = m.cText
		nCnt = ALINES(aText, m.cText)
		FOR i = 1 TO m.nCnt
			IF !EMPTY(aText[i])
				m.cCaption = aText[i]
				EXIT
			ENDIF
		ENDFOR
		cImageFile = ''
		IF TYPE("goProgram.class") ="C"
			cToolTextPrefix = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_TOOL_TEXTPREFIX, ;
				CAP_TOOL_TEXTPREFIX)
		ELSE
			cToolTextPrefix = ""
		ENDIF
		oToolItem = THIS.CreateToolItem(m.cCategoryID, cToolTextPrefix + m.cCaption, m.cText, "TEXTSCRAP", m.cImageFile, '', '')
		IF VARTYPE(m.oToolItem) == 'O'
			oToolItem.SetDataValue("textscrap", m.cText)
			THIS.SaveToolItem(m.oToolItem)
		ENDIF

	ENDIF
	ENDFUNC

* generate a toolname for a class based upon the selected naming convention
	FUNCTION GenerateToolName(cClassLib, cClassName)
	DO CASE
		CASE THIS.NamingConvention == 2  && ClassName
			RETURN m.cClassName

		CASE THIS.NamingConvention == 3  && Library.ClassName
			RETURN IIF(EMPTY(m.cClassLib), '', m.cClassLib + '.') + m.cClassName

		OTHERWISE  && ClassName (Library)
			RETURN m.cClassName + IIF(EMPTY(m.cClassLib), '', " (" + m.cClassLib + ")")

	ENDCASE

	ENDFUNC

* delete a toolbox category, item, or filter
	FUNCTION DeleteItem(cUniqueID, lPrompt)
	LOCAL oCategory
	LOCAL oRec
	LOCAL cMsg
	LOCAL nIndex

	cMsg = ""
	oRec = THIS.GetRecord(m.cUniqueID)
	IF VARTYPE(m.oRec) <> 'O'
		RETURN .F.
	ENDIF

	IF m.lPrompt
		IF TYPE("goProgram.class") ="C"
			DO CASE
				CASE m.oRec.ShowType == SHOWTYPE_CATEGORY OR m.oRec.ShowType == SHOWTYPE_FAVORITES
					cMsg = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_TOOL_DELETECATEGORY, MSG_TOOL_DELETECATEGORY)
				CASE m.oRec.ShowType == SHOWTYPE_FILTER
					cMsg = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_TOOL_DELETEFILTER, MSG_TOOL_DELETEFILTER)
				OTHERWISE
					cMsg = IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_TOOL_DELETE, MSG_TOOL_DELETE)
			ENDCASE
			llAnswer = goProgram.vfxmessagebox(m.cMsg + CHR(10) + CHR(10) + RTRIM(m.oRec.ToolName), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2, TOOLBOX_LOC)
		ELSE
			llAnswer = MESSAGEBOX(m.cMsg + CHR(10) + CHR(10) + RTRIM(m.oRec.ToolName), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2, TOOLBOX_LOC)
		ENDIF
		IF ISNULL(m.oRec) OR  llAnswer == IDNO
			RETURN .F.
		ENDIF
	ENDIF


	IF SEEK(m.cUniqueID, "ToolboxCursor", "UniqueID")
* if this is a category, then we'll need to delete all of those as well
		DELETE FROM ToolboxCursor WHERE UniqueID == m.cUniqueID OR ParentID == m.cUniqueID
	ENDIF

* find the toolbox in our categories
	DO CASE
		CASE m.oRec.ShowType == SHOWTYPE_CATEGORY
* remove the category and all of it's tools
			DELETE FROM ToolboxCursor WHERE ParentID == m.cUniqueID AND ShowType == SHOWTYPE_TOOL

* remove category from existing filters
			cUniqueID = PADR(m.cUniqueID, LEN(ToolboxCursor.ToolName))
			DELETE FROM ToolboxCursor WHERE ToolName == m.cUniqueID AND ShowType == SHOWTYPE_FILTERITEM


			IF !ISNULL(THIS.CurrentCategory) AND RTRIM(m.cUniqueID) == THIS.CurrentCategory.UniqueID
				THIS.CurrentCategory = .NULL.
			ENDIF


* from from current collection of categories
			nIndex = THIS.oCategoryCollection.GETKEY(RTRIM(m.cUniqueID))
			TRY
				THIS.oCategoryCollection.REMOVE(RTRIM(m.cUniqueID))
			CATCH
			ENDTRY

			m.nIndex = MIN(m.nIndex, THIS.oCategoryCollection.COUNT)
			IF BETWEEN(m.nIndex, 1, THIS.oCategoryCollection.COUNT)
				THIS.SetCategory(THIS.oCategoryCollection.ITEM(m.nIndex).UniqueID)
			ENDIF

		CASE m.oRec.ShowType == SHOWTYPE_FILTER
* remove filter and its subitems
			DELETE FROM ToolboxCursor WHERE ParentID == m.cUniqueID AND ShowType == SHOWTYPE_FILTERITEM

		CASE m.oRec.ShowType == SHOWTYPE_TOOL
* nothing extra to do for a tool item
	ENDCASE

	THIS.RefreshUI()

	RETURN .T.
	ENDFUNC

	FUNCTION RenameItem(cUniqueID, cNewName)
	LOCAL lSuccess
	LOCAL oToolItem
	LOCAL cToolName

	lSuccess = .F.

	oToolItem = THIS.GetToolObject(m.cUniqueID)
	IF VARTYPE(m.oToolItem) == 'O'
		cToolName = RTRIM(m.oToolItem.ToolName)

		IF VARTYPE(m.cNewName) <> 'C' OR EMPTY(m.cNewName)
			lnDatasession = SET("Datasession")
			DO FORM ("vfxToolboxRename") WITH m.cToolName, lnDatasession TO m.cNewName
		ENDIF
		IF !(m.cNewName == m.cToolName) AND !EMPTY(m.cNewName)
			oToolItem.ToolName = m.cNewName
			IF THIS.SaveItem(m.oToolItem)
				lSuccess = .T.

				THIS.RefreshUI()
			ENDIF
		ENDIF
	ENDIF

	RETURN m.lSuccess
	ENDFUNC

* create a new record from the toolitem object
* in ToolboxCursor
	FUNCTION NEWITEM(oToolItem)
	LOCAL lSuccess
	LOCAL nSelect
	LOCAL ARRAY aDisplayOrd[1]

	IF VARTYPE(m.oToolItem) <> 'O'
		RETURN .F.
	ENDIF

	nSelect = SELECT()

	lSuccess = .F.

	oToolItem.UniqueID = THIS.GenerateUniqueID()
	DO CASE
		CASE m.oToolItem.ShowType == SHOWTYPE_CATEGORY
* add new category
* oToolItem.ShowType = SHOWTYPE_CATEGORY

			SELECT MAX(DisplayOrd) ;
				FROM ToolboxCursor ;
				WHERE ShowType == SHOWTYPE_CATEGORY ;
				INTO ARRAY aDisplayOrd
			IF _TALLY > 0 AND !ISNULL(aDisplayOrd[1])
				oToolItem.DisplayOrd = aDisplayOrd[1] + 1
			ELSE
				oToolItem.DisplayOrd = 1
			ENDIF

		CASE m.oToolItem.ShowType == SHOWTYPE_TOOL
* add new tool
			IF m.oToolItem.DisplayOrd == 0
				SELECT MAX(DisplayOrd) ;
					FROM ToolboxCursor ;
					WHERE ;
					ParentID == oToolItem.ParentID AND ;
					ShowType == SHOWTYPE_TOOL ;
					INTO ARRAY aDisplayOrd
				IF _TALLY > 0 AND !ISNULL(aDisplayOrd[1])
					oToolItem.DisplayOrd = aDisplayOrd[1] + 1
				ELSE
					oToolItem.DisplayOrd = 1
				ENDIF
			ENDIF
	ENDCASE


	SELECT ToolboxCursor
	INSERT INTO ToolboxCursor FROM NAME m.oToolItem

	oToolItem.ParseToolData()
	REPLACE ;
		ToolData WITH m.oToolItem.EncodeToolData(), ;
		Inactive WITH .F., ;
		Modified WITH DATETIME() ;
		IN ToolboxCursor

	IF TYPE("goProgram.class") ="C" AND goProgram.nVFXSysTableLoc >= 1
		SELECT ToolboxCursor
		TABLEUPDATE(.T.)
	ELSE
		IF !THIS.lCustomizeMode
			THIS.LoadToolbox()
		ENDIF
	ENDIF

	lSuccess = .T.

	SELECT (m.nSelect)

	RETURN m.lSuccess
	ENDFUNC


	FUNCTION SaveItem(oToolItem)
	LOCAL lSuccess
	LOCAL nSelect

	IF VARTYPE(m.oToolItem) <> 'O'
		RETURN .F.
	ENDIF

	nSelect = SELECT()

	lSuccess = .F.
	IF SEEK(m.oToolItem.UniqueID, "ToolboxCursor", "UniqueID")
		oToolItem.ParseToolData()

		SELECT ToolboxCursor
		GATHER MEMO NAME m.oToolItem
		REPLACE ;
			ToolData WITH m.oToolItem.EncodeToolData(), ;
			Modified WITH DATETIME() ;
			IN ToolboxCursor

		IF CURSORGETPROP("Buffering","ToolboxCursor") = 3 OR CURSORGETPROP("Buffering","ToolboxCursor") = 5
			SELECT ToolboxCursor
			TABLEUPDATE(.T.)
		ENDIF

		lSuccess = .T.
	ENDIF

	SELECT (m.nSelect)

	RETURN m.lSuccess
	ENDFUNC


* Evaluate passed string.
* If it is surrounded by parens, then
* evaluate it, otherwise return the original text.
	FUNCTION EvalText(cText)
	IF LEFT(m.cText, 1) == '(' AND RIGHT(m.cText, 1) == ')'
		TRY
			m.cText = EVALUATE(m.cText)
		CATCH
		ENDTRY
	ENDIF

	RETURN m.cText
	ENDFUNC

* Save customization
* This must have been opened in customized mode to work
* (pass .T. to Init)
	FUNCTION SaveCustomization()
	LOCAL nSelect
	LOCAL cDeleted
	LOCAL oRec

	nSelect = SELECT()

	IF THIS.lCustomizeMode
		IF THIS.OpenToolbox(.F., "Toolbox", THIS.oTBForm)
*oRes = THIS.OpenToolbox(.F., "Toolbox")
*IF USED("ToolboxCursor")
			cDeleted = SET("DELETED")
			SET DELETED OFF

			SELECT ToolboxCursor
			SCAN ALL
				IF SEEK(ToolboxCursor.UniqueID, "Toolbox", "UniqueID")
					IF DELETED("ToolboxCursor")
						DELETE IN Toolbox
					ELSE
						SELECT ToolboxCursor
						SCATTER MEMO NAME oRec

						SELECT Toolbox
						*{V&U MS 2010-05-16
						GATHER MEMO NAME oRec FIELDS EXCEPT IDVFXTBOX
						*}V&U MS 2010-05-16
					ENDIF
				ELSE
					IF !DELETED("ToolboxCursor")
						SELECT ToolboxCursor
						SCATTER MEMO NAME oRec
* oRec.Inactive = !oRec.Checked

						SELECT Toolbox
						INSERT INTO Toolbox FROM NAME m.oRec
					ENDIF
				ENDIF
			ENDSCAN

			SET DELETED &cDeleted

			IF CURSORGETPROP("Buffering","ToolboxCursor") = 3 OR CURSORGETPROP("Buffering","ToolboxCursor") = 5
				SELECT ToolboxCursor
				TABLEUPDATE(.T.)
			ENDIF

		ENDIF
	ENDIF
	THIS.SavePrefs()

	SELECT (m.nSelect)
	ENDFUNC

* Customization Mode Only
* allows us to remove tool given its UniqueID
	FUNCTION RemoveTool(cUniqueID)
	LOCAL nSelect

	nSelect = SELECT()

	IF SEEK(m.cUniqueID, "ToolboxCursor", "UniqueID")
		DELETE IN ToolboxCursor
	ENDIF

	SELECT (m.nSelect)
	ENDFUNC

* return a collection of the add-ins for specified ToolTypeID
* <cToolTypeID> = Tool type to return addins for
* [cShowType]   = add-in type to retrieve, default to SHOWTYPE_ADDIN
	FUNCTION GetAddIns(cToolTypeID, cParentID, cClassType)
	LOCAL oAddInCollection
	LOCAL oAddIn
	LOCAL nSelect

	nSelect = SELECT()
	oAddInCollection = CREATEOBJECT("Collection")

	IF VARTYPE(m.cToolTypeID) <> 'C'
		cToolTypeID = ''
	ENDIF

	IF VARTYPE(m.cParentID) <> 'C'
		cParentID = ''
	ENDIF

	IF VARTYPE(m.cClassType) <> 'C'
		cClassType = ''
	ENDIF


	cToolTypeID = PADR(m.cToolTypeID, LEN(ToolboxCursor.ToolTypeID))
	cParentID   = PADR(m.cParentID, LEN(ToolboxCursor.ParentID))
	cClassType  = PADR(m.cClassType, LEN(ToolboxCursor.ClassType))

	SELECT ToolboxCursor
	SCAN ALL FOR ;
			(ShowType == SHOWTYPE_ADDIN OR ShowType == SHOWTYPE_ADDINMENU) AND ;
			ToolTypeID == m.cToolTypeID AND ;
			ParentID == m.cParentID AND ;
			ClassType == m.cClassType AND ;
			!Inactive
		oAddIn = CREATEOBJECT("ToolboxAddIn")
		oAddIn.UniqueID   = ToolboxCursor.UniqueID
		oAddIn.AddInName  = RTRIM(ToolboxCursor.ToolName)
		IF ToolboxCursor.ShowType == SHOWTYPE_ADDINMENU
			oAddIn.IsMenu   = .T.
			oAddIn.MenuCode = ToolboxCursor.ToolData
		ELSE
			oAddIn.IsMenu = EMPTY(ToolboxCursor.ToolData) AND EMPTY(ToolboxCursor.ClassName)
		ENDIF
		oAddInCollection.ADD(oAddIn)
	ENDSCAN

	SELECT (m.nSelect)

	RETURN m.oAddInCollection
	ENDFUNC


* run all Add-ins with matching ParentID and Classtype
* Mainly for the Classtype = "ONLOAD" to run an addin when
* the toolbox is loaded or a category is opened
	FUNCTION RunAddIns(cParentID, cClassType)
	LOCAL nSelect
	LOCAL lSuccess

	nSelect = SELECT()

	IF VARTYPE(m.cParentID) <> 'C'
		cParentID = ''
	ENDIF
	IF VARTYPE(m.cClassType) <> 'C' OR EMPTY(m.cClassType)
		cClassType = "ONLOAD"
	ENDIF

	cParentID = PADR(m.cParentID, LEN(ToolboxCursor.ParentID))
	cClassType  = PADR(m.cClassType, LEN(ToolboxCursor.ClassType))

	lSuccess = .F.
	SELECT ToolboxCursor
	SCAN ALL FOR ShowType == SHOWTYPE_ADDIN AND ParentID == m.cParentID AND ClassType == m.cClassType AND !Inactive
		IF THIS.InvokeAddIn(ToolboxCursor.UniqueID, THIS)
			lSuccess = .T.
		ENDIF
	ENDSCAN

	SELECT (m.nSelect)

	RETURN m.lSuccess
	ENDFUNC


	FUNCTION InvokeAddIn(cUniqueID, oToolItem, p1, p2, p3, p4)
	LOCAL oAddInRec
	LOCAL oException
	LOCAL oAddIn
	LOCAL lSuccess

	IF VARTYPE(m.oToolItem) <> 'O'
		oToolItem = .NULL.
	ENDIF

	lSuccess = .F.
	oAddInRec = THIS.GetRecord(m.cUniqueID)
	IF VARTYPE(oAddInRec) == 'O'
		IF EMPTY(m.oAddInRec.ClassName)
* no classname specified, so assume ToolData contains script to run
			IF !EMPTY(m.oAddInRec.ToolData)
				TRY
					DO CASE
						CASE PCOUNT() == 6
							EXECSCRIPT(m.oAddInRec.ToolData, m.oToolItem, m.p1, m.p2, m.p3, m.p4)
						CASE PCOUNT() == 5
							EXECSCRIPT(m.oAddInRec.ToolData, m.oToolItem, m.p1, m.p2, m.p3)
						CASE PCOUNT() == 4
							EXECSCRIPT(m.oAddInRec.ToolData, m.oToolItem, m.p1, m.p2)
						CASE PCOUNT() == 3
							EXECSCRIPT(m.oAddInRec.ToolData, m.oToolItem, m.p1)
						OTHERWISE
							EXECSCRIPT(m.oAddInRec.ToolData, m.oToolItem)
					ENDCASE
					m.lSuccess = .T.
				CATCH TO oException
					IF TYPE("goProgram.class") ="C"
						goProgram.vfxmessagebox(m.oException.MESSAGE, MB_ICONEXCLAMATION, TOOLBOX_LOC)
					ELSE
						MESSAGEBOX(m.oException.MESSAGE, MB_ICONEXCLAMATION, TOOLBOX_LOC)
					ENDIF
				ENDTRY
			ENDIF
		ELSE
* a class is specified, so create an instance of it and
* invoke the Execute() method with a reference to
* the tool item.
			TRY
				oAddIn = NEWOBJECT(m.oAddInRec.ClassName, m.oAddInRec.CLASSLIB)
				oAddIn.Execute(m.oToolItem)
				m.lSuccess = .T.

			CATCH TO oException
				IF TYPE("goProgram.class") ="C"
					goProgram.vfxmessagebox(m.oException.MESSAGE, MB_ICONEXCLAMATION, TOOLBOX_LOC)
				ELSE
					MESSAGEBOX(m.oException.MESSAGE, MB_ICONEXCLAMATION, TOOLBOX_LOC)
				ENDIF
			ENDTRY
		ENDIF
	ENDIF

	RETURN m.lSuccess
	ENDFUNC


* If cFilename is in a subfolder of the HOME() directory,
* then return it as an expression.  For example:
*	(HOME() + "ffc\_agent.vcx")
	FUNCTION RelativeToHome(m.cFilename)
	LOCAL cHomeDir

	IF !EMPTY(m.cFilename)
		cHomeDir = UPPER(HOME())
		IF m.cHomeDir == LEFT(UPPER(ADDBS(JUSTPATH(m.cFilename))), LEN(m.cHomeDir))
			cFilename = [(HOME() + "] + SUBSTR(m.cFilename, LEN(m.cHomeDir) + 1) + [")]
		ENDIF
	ENDIF

	RETURN m.cFilename
	ENDFUNC

ENDDEFINE

* This class is used to delay running of the builder
* when class is drag/dropped.  The problem is that
* some ActiveX controls and such don't cooperate very
* well when called from OleCompleteDrag() event ...
* so we have to setup a timer to call the builder
DEFINE CLASS BuilderDelay AS TIMER
*#include "INCLUDE\VFX.H"
	ENABLED = .F.
	INTERVAL = 200

	PROCEDURE TIMER()
	THIS.ENABLED = .F.
	THIS.RESET()

	LOCAL nDataSession
	LOCAL ARRAY aCtrlObj[1]

	 = ASELOBJ(aCtrlObj)
	IF TYPE("aCtrlObj[1]") == 'O'
		nDataSession = SET("DATASESSION")
		SET DATASESSION TO 1
		DO (_BUILDER) WITH aCtrlObj[1], "TOOLBOX"
		SET DATASESSION TO (m.nDataSession)
	ENDIF
	ENDPROC
ENDDEFINE

DEFINE CLASS ToolboxCategory AS CUSTOM
*	#include "INCLUDE\VFX.H"
	NAME       = "ToolboxCategory"
	UniqueID   = ''
	TopToolID  = ''
	ToolTypeID = ''
	*{V&U MS 2010-05-16
	IDVFXTTYPE = 0        
	*}V&U MS 2010-05-16
	ToolType   = ''
	ToolName   = ''
	ParentID   = ''
	ClassType  = ''
	SetID      = ''
	ClassName  = ''
	CLASSLIB   = ''
	TOOLTIP    = ''
	HelpFile   = ''
	HelpID     = 0
	USER       = ''

	oToolCollection = .NULL.

	PROCEDURE INIT()
	THIS.oToolCollection = CREATEOBJECT("Collection")
	ENDPROC

* -- Add content record to the pane
	FUNCTION AddTool(cUniqueID)
	THIS.oToolCollection.ADD(RTRIM(m.cUniqueID))

	RETURN .T.
	ENDFUNC
ENDDEFINE

DEFINE CLASS ToolboxFilter AS CUSTOM
	NAME = "ToolboxFilter"

	UniqueID   = ''
	FilterName = ''
ENDDEFINE

DEFINE CLASS ToolboxAddIn AS CUSTOM
	NAME = "AddInFilter"

	UniqueID    = ''
	AddInName   = ''
	IsMenu      = .F. && true if this is simply a parent menu for add-ins (no associated code)
	MenuCode    = ''
ENDDEFINE

DEFINE CLASS PropertyCollection AS COLLECTION
	FUNCTION PropertyExists(cPropName)
	RETURN !ISNULL(THIS.GetProperty(m.cPropName))
	ENDFUNC

	FUNCTION GetProperty(cPropName)
	LOCAL i
	LOCAL oPropObject

	oPropObject = .NULL.
	cPropName = UPPER(m.cPropName)
	FOR i = 1 TO THIS.COUNT
		IF UPPER(THIS.ITEM(m.i).NAME) == m.cPropName
			oPropObject = THIS.ITEM(m.i)
			EXIT
		ENDIF
	ENDFOR

	RETURN m.oPropObject
	ENDFUNC

	PROCEDURE AddPropertyValue(cName, cValue)
	LOCAL oPropObject

	oPropObject = CREATEOBJECT("Empty")
	ADDPROPERTY(m.oPropObject, "Name", m.cName)
	ADDPROPERTY(m.oPropObject, "Value", m.cValue)

	THIS.ADD(m.oPropObject)
	ENDPROC

ENDDEFINE


*-------------------------------------------------------
* Function....: ContextMenu - FoxMenu
* Called by...: VFX ToolBox
*
* Abstract....: Wraps context-sensitive (right-click) menu functionality in an object.
*
* Returns.....: .T. - Skip It / .F.
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------

DEFINE CLASS ContextMenu AS CUSTOM
	MenuBarCount = 0  && defined so we can hook an _Access method to this
	ShowInScreen = .T.
	oObject = .F.

	ADD OBJECT PopupNames AS COLLECTION
	ADD OBJECT MenuItems AS COLLECTION

	PROCEDURE INIT(oObject)
		This.oObject = oObject
	ENDPROC

	PROCEDURE DESTROY()
	LOCAL cRelName
	FOR EACH cRelName IN THIS.PopupNames
		RELEASE POPUP &cRelName
	ENDFOR
	ENDPROC

	FUNCTION MenuBarCount_Access()
	RETURN THIS.MenuItems.COUNT
	ENDFUNC

	FUNCTION AddMenu(cCaption, cActionCode, cPicture, lChecked, lEnabled, lBold, cKey)
	LOCAL oMenuItem

	IF PCOUNT() < 5
		lEnabled = .T.
	ENDIF

* we could pass a menu object rather than a caption
* (this is our technique for overloading a function!)
	IF VARTYPE(cCaption) == 'O'
		oMenuItem = cCaption
	ELSE
* don't add 2 menu separators in a row
		IF m.cCaption == "\-" AND THIS.MenuItems.COUNT > 0 AND THIS.MenuItems.ITEM(THIS.MenuItems.COUNT).CAPTION == "\-"
			RETURN .NULL.
		ENDIF

		oMenuItem = CREATEOBJECT("MenuItem")
		WITH oMenuItem
			oMenuItem.CAPTION = cCaption
			IF VARTYPE(cPicture) == 'C'
				.PICTURE = cPicture
			ENDIF
			IF VARTYPE(lChecked) == 'L'
				.Checked = lChecked
			ENDIF
			IF VARTYPE(cActionCode) == 'C'
				.ActionCode = cActionCode
			ENDIF
			IF VARTYPE(lEnabled) == 'L'
				.IsEnabled = lEnabled
			ENDIF
			IF VARTYPE(lBold) == 'L'
				.Bold = lBold
			ENDIF
			*{V&U MS 2010-09-01
			IF VARTYPE(cKey) == 'C'
				.Key = cKey
			ENDIF 
			*}V&U MS 2010-09-01
		ENDWITH
	ENDIF
	THIS.MenuItems.ADD(oMenuItem)

	RETURN oMenuItem
	ENDFUNC


	PROCEDURE SHOW(nRow, nCol)
	THIS.PopupNames.REMOVE(-1)

	DO CASE
		CASE goProgram.nMenuandToolbarStyle = 1
			loCtDropMenu = CREATEOBJECT("cDBiDropMenu", THIS)
			THIS.BuildMenu(@loCtDropMenu, m.nRow, m.nCol)
			loCtDropMenu.onDropMenu()
		CASE INLIST(goProgram.nMenuandToolbarStyle, 2, 3, 5)	&& V&U MS 2013-01-22 6086
			loShortcutMenu = CREATEOBJECT("cContextMenuDialog", This.oObject)
			This.BuildMenu(@loShortcutMenu, m.nRow, m.nCol)
			loShortcutMenu.Show()
		OTHERWISE
			IF VARTYPE(m.nRow) <> 'N'
				nRow = MROW("")
			ENDIF
			IF VARTYPE(m.nCol) <> 'N'
				nCol = MCOL("")
			ENDIF
			THIS.BuildMenu("shortcut", m.nRow, m.nCol)

			ACTIVATE POPUP shortcut
	ENDCASE
	ENDPROC

* Render the menu
	FUNCTION BuildMenu(cMenuName, nRow, nCol)
	LOCAL nBar
	LOCAL oMenuItem
	LOCAL cActionCode
	LOCAL cSubMenu
	LOCAL cSkipFor
	LOCAL cStyle
	LOCAL nLevel

	DO CASE
		CASE goProgram.nMenuandToolbarStyle = 1
			nBar = 1
			FOR EACH oMenuItem IN THIS.MenuItems
				nLevel = oMenuItem.SubMenu.MenuItems.COUNT + 1
				IF oMenuItem.IsEnabled
					cSkipFor = ''
				ELSE
					cSkipFor = "SKIP FOR .T."
				ENDIF

				IF oMenuItem.Bold
					cStyle = [STYLE "B"]
				ELSE
					cStyle = ''
				ENDIF

				oMenuItem.DBiIndex = m.nBar

				IF oMenuItem.CAPTION = "\-"
					cMenuName.octDropMenu.OBJECT.ADDITEM("", 1, 1)
				ELSE
					cMenuName.octDropMenu.OBJECT.ADDITEM(ALLTRIM(STRTRAN(oMenuItem.CAPTION, "\<", "&")), 0, nLevel)
					lcPictureName = oMenuItem.PICTURE
					IF FILE(lcPictureName)
						cMenuName.octDropMenu.ItemPicture(m.nBar) = LOADPICTURE(lcPictureName)
					ENDIF
					cMenuName.octDropMenu.ItemChecked(m.nBar) = oMenuItem.Checked
					IF oMenuItem.SubMenu.MenuItems.COUNT > 0
						cSubMenu = SYS(2015)
						oMenuItem.SubMenu.BuildMenu(m.cSubMenu)
					ENDIF
				ENDIF
				nBar = m.nBar + 1
			ENDFOR
		CASE INLIST(goProgram.nMenuandToolbarStyle, 2, 3, 5)	&& V&U MS 2013-01-22 6086
			nBar = 1
			FOR EACH oMenuItem IN THIS.MenuItems
				IF oMenuItem.CAPTION = "\-"
					lcPupupName = TRANSFORM(nBar)
					loItem = cMenuName.cntPopupMenu.AddPopupItem(lcPupupName, 'SEP', '')
				ELSE 
					lcPupupName = ALLTRIM(STRTRAN(oMenuItem.CAPTION, "\<", ""))
					loItem = cMenuName.cntPopupMenu.AddPopupItem(lcPupupName, 'NORM', '')
					loItem.cItemKey = ["] + oMenuItem.Key + ["] 
					lcPictureName = oMenuItem.PICTURE
					IF TYPE("goProgram.Class") = "C" AND goProgram.nColorDepth <= 8 AND ;
						FILE(FULLPATH(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + ;
						"_8bit." + JUSTEXT(lcPictureName)))
						
						lcPictureName = FULLPATH(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + ;
										"_8bit." + JUSTEXT(lcPictureName))
					ELSE 
						lcPictureName = FULLPATH(lcPictureName)
					ENDIF 
					IF TYPE("goProgram.Class") = "C" AND goProgram.nColorDepth <= 8 AND ;
						FILE(FULLPATH(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + ;
						"dis_8bit." + JUSTEXT(lcPictureName)))
						
						lcDisPictureName = FULLPATH(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + ;
										"dis_8bit." + JUSTEXT(lcPictureName))			
					ELSE 
						lcDisPictureName = FULLPATH(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + ;
										"dis." + JUSTEXT(lcPictureName))
					ENDIF 

					IF FILE(lcPictureName)
						loItem.cPicture = lcPictureName
						IF FILE(lcDisPictureName)
							loItem.cDisabledPicture = lcDisPictureName
						ELSE 
							loItem.cDisabledPicture = lcPictureName
						ENDIF 
					ENDIF 	
					loItem.Alignment = 0	
					IF oMenuItem.IsEnabled
						loItem.cSkipForExp = ''
					ELSE
						loItem.cSkipForExp = '.T.'
					ENDIF	
					IF oMenuItem.Checked	
						loItem.cMarkExp = ".T."	
					ELSE 
						loItem.cMarkExp = ".F."
					ENDIF 
					BINDEVENT(loItem, 'Execute', cMenuName, 'Execute')	
				ENDIF 	
				nBar = m.nBar + 1
			ENDFOR 
		OTHERWISE
			IF VARTYPE(cMenuName) <> 'C'
				cMenuName = SYS(2015)
			ENDIF

			IF THIS.ShowInScreen
				IF PCOUNT() < 3
					DEFINE POPUP (m.cMenuName) SHORTCUT RELATIVE IN WINDOW "Screen"
				ELSE
					DEFINE POPUP (m.cMenuName) SHORTCUT RELATIVE FROM m.nRow, m.nCol IN WINDOW "Screen"
				ENDIF
			ELSE
				IF PCOUNT() < 3
					DEFINE POPUP (m.cMenuName) SHORTCUT RELATIVE
				ELSE
					DEFINE POPUP (m.cMenuName) SHORTCUT RELATIVE FROM m.nRow, m.nCol
				ENDIF
			ENDIF
			THIS.PopupNames.ADD(m.cMenuName)

			nBar = 0
			FOR EACH oMenuItem IN THIS.MenuItems
				cActionCode = oMenuItem.ActionCode

				IF oMenuItem.IsEnabled
					cSkipFor = ''
				ELSE
					cSkipFor = "SKIP FOR .T."
				ENDIF

				IF oMenuItem.Bold
					cStyle = [STYLE "B"]
				ELSE
					cStyle = ''
				ENDIF

				nBar = m.nBar + 1
				DEFINE BAR (m.nBar) OF (m.cMenuName) PROMPT (oMenuItem.CAPTION) PICTURE (oMenuItem.PICTURE) &cStyle &cSkipFor

				IF VARTYPE(m.cActionCode) == 'C' AND !EMPTY(m.cActionCode)
					ON SELECTION BAR (m.nBar) OF (m.cMenuName) &cActionCode
				ENDIF

				IF oMenuItem.Checked
					SET MARK OF BAR (m.nBar) OF (m.cMenuName) TO .T.
				ENDIF

				IF oMenuItem.SubMenu.MenuItems.COUNT > 0
					cSubMenu = SYS(2015)

					ON BAR (m.nBar) OF (m.cMenuName) ACTIVATE POPUP &cSubMenu

					oMenuItem.SubMenu.BuildMenu(m.cSubMenu)
				ENDIF
			ENDFOR
	ENDCASE
	ENDFUNC
ENDDEFINE


DEFINE CLASS MenuItem AS CUSTOM
	NAME        = "MenuItem"
	CAPTION     = ''
	PICTURE     = ''
	Checked     = .F.
	ActionCode  = ''
	IsEnabled   = .T.
	Bold        = .F.
	DBiIndex 	 = .F.
	SubMenu 	 = .NULL.
	Key			= ''

	PROCEDURE INIT(cCaption, cActionCode, cPicture, lChecked, lEnabled)
	THIS.SubMenu = CREATEOBJECT("ContextMenu")

	IF VARTYPE(cCaption) == 'C'
		THIS.CAPTION = cCaption
	ENDIF
	IF VARTYPE(cPicture) == 'C'
		THIS.PICTURE = cPicture
	ENDIF
	IF VARTYPE(lChecked) == 'L'
		THIS.Checked = lChecked
	ENDIF
	IF VARTYPE(cActionCode) == 'C'
		THIS.ActionCode = cActionCode
	ENDIF
	IF VARTYPE(lEnabled) == 'L' AND PCOUNT() >= 5
		THIS.IsEnabled = lEnabled
	ENDIF
	ENDPROC

ENDDEFINE

* Abstract:
*   Class for add/retrieving values
*	from FoxUser resource file.
*

DEFINE CLASS FoxResource AS CUSTOM
	PROTECTED oCollection

	oCollection  = .NULL.

	ResourceType = "PREFW"
	ResourceFile = ''

	PROCEDURE INIT()
	THIS.oCollection = CREATEOBJECT("Collection")
	THIS.ResourceFile = SYS(2005)
	ENDPROC

	PROCEDURE DESTROY()
	THIS.CloseResource()
	ENDPROC

* Clear out all options
	FUNCTION CLEAR()
	THIS.oCollection.REMOVE(-1)
	ENDFUNC

	FUNCTION SET(cOption, xValue)
* remove if already exists
	IF THIS.OptionExists(m.cOption)
		THIS.oCollection.REMOVE(UPPER(m.cOption))
	ENDIF

* Add back in
	RETURN THIS.oCollection.ADD(m.xValue, UPPER(m.cOption))
	ENDFUNC

	FUNCTION GET(cOption)
	LOCAL xValue
	LOCAL i

	xValue = .NULL.
	cOption = UPPER(m.cOption)
	FOR i = 1 TO THIS.oCollection.COUNT
		IF UPPER(THIS.oCollection.GETKEY(m.i)) == m.cOption
			xValue = THIS.oCollection.ITEM(m.i)
			EXIT
		ENDIF
	ENDFOR

	RETURN m.xValue
	ENDFUNC

	FUNCTION OptionExists(cOption)
	LOCAL i
	LOCAL lExists

	lExists = .F.
	cOption = UPPER(m.cOption)
	FOR i = 1 TO THIS.oCollection.COUNT
		IF UPPER(THIS.oCollection.GETKEY(m.i)) == m.cOption
			lExists = .T.
			EXIT
		ENDIF
	ENDFOR

	RETURN m.lExists
	ENDFUNC

	FUNCTION OpenResource()
	IF !(SET("RESOURCE") == "ON")
		RETURN .F.
	ENDIF

	IF !USED("FoxResource")
		IF FILE(THIS.ResourceFile)
			TRY
				USE (THIS.ResourceFile) ALIAS FoxResource IN 0 SHARED AGAIN
			CATCH
			ENDTRY
		ENDIF
	ENDIF

	RETURN USED("FoxResource")
	ENDFUNC

	FUNCTION CloseResource()
	IF USED("FoxResource")
		USE IN FoxResource
	ENDIF
	ENDFUNC

	PROCEDURE SAVE(cID, cName)
	LOCAL nSelect
	LOCAL cType
	LOCAL i
	LOCAL ARRAY aOptions[1]

	IF VARTYPE(m.cName) <> 'C'
		cName = ''
	ENDIF
	IF THIS.OpenResource()
		nSelect = SELECT()

		cType = PADR(THIS.ResourceType, LEN(FoxResource.TYPE))
		cID   = PADR(m.cID, LEN(FoxResource.ID))

		SELECT FoxResource
		LOCATE FOR TYPE == m.cType AND ID == m.cID AND NAME == m.cName
		IF !FOUND()
			APPEND BLANK IN FoxResource
			REPLACE ;
				TYPE WITH m.cType, ;
				NAME WITH m.cName, ;
				ID WITH m.cID, ;
				READONLY WITH .F. ;
				IN FoxResource
		ENDIF

		IF !FoxResource.READONLY
			IF THIS.oCollection.COUNT > 0
				DIMENSION aOptions[THIS.oCollection.Count, 2]
				FOR i = 1 TO THIS.oCollection.COUNT
					aOptions[m.i, 1] = THIS.oCollection.GETKEY(m.i)
					aOptions[m.i, 2] = THIS.oCollection.ITEM(m.i)
				ENDFOR
				SAVE TO MEMO DATA ALL LIKE aOptions
			ELSE
				BLANK FIELDS DATA IN FoxResource
			ENDIF

			REPLACE ;
				UPDATED WITH DATE(), ;
				ckval WITH VAL(SYS(2007, FoxResource.DATA)) ;
				IN FoxResource
		ENDIF

		THIS.CloseResource()

		SELECT (m.nSelect)
	ENDIF
	ENDPROC

	PROCEDURE LOAD(cID, cName)
	LOCAL nSelect
	LOCAL cType
	LOCAL nCnt
	LOCAL i
	LOCAL ARRAY aOptions[1]

	IF VARTYPE(m.cName) <> 'C'
		cName = ''
	ENDIF

* THIS.Clear()
	IF THIS.OpenResource()
		nSelect = SELECT()

		cType = PADR(THIS.ResourceType, LEN(FoxResource.TYPE))
		cID   = PADR(m.cID, LEN(FoxResource.ID))

		SELECT FoxResource
		LOCATE FOR TYPE == m.cType AND ID == m.cID AND NAME == m.cName
		IF FOUND() AND !EMPTY(DATA) AND ckval == VAL(SYS(2007, DATA))
			RESTORE FROM MEMO DATA ADDITIVE
			IF VARTYPE(aOptions[1,1]) == 'C'
				nCnt = ALEN(aOptions, 1)
				FOR i = 1 TO m.nCnt
					THIS.SET(aOptions[m.i, 1], aOptions[m.i, 2])
				ENDFOR
			ENDIF
		ENDIF

		THIS.CloseResource()

		SELECT (m.nSelect)
	ENDIF
	ENDPROC

	FUNCTION GETDATA(cID, cName)
	LOCAL cData
	LOCAL nSelect
	LOCAL cType

	IF VARTYPE(m.cName) <> 'C'
		cName = ''
	ENDIF

	cData = .NULL.
	IF THIS.OpenResource()
		nSelect = SELECT()

		cType = PADR(THIS.ResourceType, LEN(FoxResource.TYPE))
		cID   = PADR(m.cID, LEN(FoxResource.ID))

		SELECT FoxResource
		LOCATE FOR TYPE == m.cType AND ID == m.cID AND NAME == m.cName
		IF FOUND() AND !EMPTY(DATA) && AND ckval == VAL(SYS(2007, Data))
			cData = FoxResource.DATA
		ENDIF

		THIS.CloseResource()

		SELECT (m.nSelect)
	ENDIF

	RETURN m.cData
	ENDFUNC

* save to a specific fieldname
	FUNCTION SaveTo(cField, cAlias)
	LOCAL i
	LOCAL nSelect
	LOCAL lSuccess
	LOCAL ARRAY aOptions[1]

	IF VARTYPE(m.cAlias) <> 'C'
		cAlias = ALIAS()
	ENDIF

	IF USED(m.cAlias)
		nSelect = SELECT()
		SELECT (m.cAlias)

		IF THIS.oCollection.COUNT > 0
			DIMENSION aOptions[THIS.oCollection.Count, 2]
			FOR i = 1 TO THIS.oCollection.COUNT
				aOptions[m.i, 1] = THIS.oCollection.GETKEY(m.i)
				aOptions[m.i, 2] = THIS.oCollection.ITEM(m.i)
			ENDFOR
			SAVE TO MEMO &cField ALL LIKE aOptions
		ELSE
			BLANK FIELDS &cField IN FoxResource
		ENDIF
		SELECT (m.nSelect)
		lSuccess = .T.
	ELSE
		lSuccess = .F.
	ENDIF

	RETURN m.lSuccess
	ENDFUNC


	FUNCTION RestoreFrom(cField, cAlias)
	LOCAL i
	LOCAL nSelect
	LOCAL lSuccess
	LOCAL ARRAY aOptions[1]

	IF VARTYPE(m.cAlias) <> 'C'
		cAlias = ALIAS()
	ENDIF

	IF USED(m.cAlias)
		nSelect = SELECT()
		SELECT (m.cAlias)

		RESTORE FROM MEMO &cField ADDITIVE
		IF VARTYPE(aOptions[1,1]) == 'C'
			nCnt = ALEN(aOptions, 1)
			FOR m.i = 1 TO m.nCnt
				THIS.SET(aOptions[m.i, 1], aOptions[m.i, 2])
			ENDFOR
		ENDIF

		SELECT (m.nSelect)
		lSuccess = .T.
	ELSE
		lSuccess = .F.
	ENDIF

	RETURN m.lSuccess
	ENDFUNC
ENDDEFINE

*-------------------------------------------------------
* Function....: GetExportFileName
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcFileType
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetExportFileName
LPARAMETERS tcFileType AS STRING, tcFileName AS STRING, tcExportFolderType AS STRING, tlReturnOnlyPath

LOCAL lcCaption, lcBuffer AS STRING
LOCAL lnOldWorkArea, lnStoreFolderConstant AS INTEGER


tcFileType = LOWER(ALLTRIM(tcFileType))
lcFileName = ""
tcExportFolderType = IIF(EMPTY(tcExportFolderType), "UserExportPath", tcExportFolderType)
IF EMPTY(tlReturnOnlyPath)
	tlReturnOnlyPath = .F.
ENDIF
lnStoreFolderConstant = 0x0005

IF TYPE("goProgram.class") ="C"
	DO CASE
		CASE tcFileType == "pdf"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cTTT_PDF, TTT_PDF)

		CASE tcFileType == "html"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_HTML, CAP_HTML)

		CASE tcFileType == "xml"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_XML, CAP_XML)

		CASE tcFileType == "tiff"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_TIFF, CAP_TIFF)

		CASE tcFileType == "bmp"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_BMP, CAP_BMP)

		CASE tcFileType == "xls"
			*{HC MS 2015-06-30, Create XLSX files without installed Excel
			IF TYPE("goProgram.lCreateXlsxDirectly") = "L" AND goProgram.lCreateXlsxDirectly
				lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_XLSX, CAP_XLSX)
			ELSE 
				lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_XLS, CAP_XLS)
			ENDIF 	
			*}HC MS 2015-06-30	

		CASE tcFileType == "csv"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_CSV, CAP_CSV)

		CASE tcFileType == "dbf"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_DBF, CAP_DBF)
		CASE LEFT(tcFileType, 3) == "bmp"
			lcCaption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_PICTURE, CAP_PICTURE)
			IF "WINDOWS 5" $ UPPER(OS()) OR "2000" $ UPPER(OS()) &&OR "WINDOWS NT 5" $ UPPER(OS())
				lnStoreFolderConstant = 0x0027
			ENDIF

		OTHERWISE
			RETURN ""
	ENDCASE
ENDIF

lnOldWorkArea = SELECT()

*** MS 21.04.2004
IF TYPE("goProgram.class") ="C" AND goprogram.lSaveExportPathPerUser
	lcloseit = !USED("resource")
	IF lcloseit
		IF goProgram.nVFXSysTableLoc >= 1
			*{V&U MS 2010-05-13, 2014-04-28
			oResAdptr = OPENTABLE("vfxRes", "IDVFXRES", .F., ;
				"INDEX ON UPPER(user)+UPPER(objname) TAG USER ADDITIVE COLLATE [MACHINE]", ;
				.T., ,"RESOURCE", "cAppVFXDataAccess", .F., .F., .F., .F., "INDEX,USER")
			*}V&U MS 2010-05-13, 2014-04-28
			SET ORDER TO TAG USER
		ELSE
			USE vfxres IN 0 ORDER TAG USER AGAIN ALIAS RESOURCE
		ENDIF
	ENDIF

	SELECT RESOURCE

	lckey = UPPER(PADR(GoUser.USER, 32,' ') + PADR(tcExportFolderType, LEN(RESOURCE.objname)))
	cFullPdfPath = ""
	lcOldSetDefault = ""
	lFindRow = SEEK(lckey,"resource","user")
	IF lFindRow
		cFullPdfPath = ALLTRIM(RESOURCE.layout)
	ENDIF
	IF EMPTY(cFullPdfPath)
		lcBuffer = ""
* 5 - 0x0005 CSIDL_PERSONAL A typical path is C:\Documents and Settings\username\My Documents.
		lnRes = GetFolderPath(lnStoreFolderConstant, @lcBuffer)
		IF lnRes = 0
			cFullPdfPath = lcBuffer
		ENDIF
	ENDIF

	IF tlReturnOnlyPath
		USE IN RESOURCE
		SELECT(lnOldWorkArea)
		RETURN cFullPdfPath
	ENDIF

	IF !EMPTY(cFullPdfPath)
		*{V&U MS 2012-07-17 5861
		lcOldSetDefault = GetDefaultFolder()
		*}V&U MS 2012-07-17
		IF DIRECTORY(cFullPdfPath)
			CD (cFullPdfPath)
		ENDIF
	ENDIF

	*{HC MS 2015-06-30, Create XLSX files without installed Excel
	IF tcFileType == "xls" AND TYPE("goProgram.lCreateXlsxDirectly") = "L" AND goProgram.lCreateXlsxDirectly
		lcFileName = ValidFileName(PUTFILE(lcCaption, ValidFileName(tcFileName), "xlsx"))
	ELSE 	
		lcFileName = ValidFileName(PUTFILE(lcCaption, ValidFileName(tcFileName), tcFileType))
	ENDIF 	
	*}HC MS 2015-06-30
	lcDirName = ADDBS(ALLTRIM(JUSTPATH(lcFileName)))
	IF !EMPTY(lcOldSetDefault)
		*{V&U MS 2012-07-17 5861
		CD (lcOldSetDefault)
		*}V&U MS 2012-07-17
	ENDIF
	SELECT RESOURCE
	IF lFindRow
		REPLACE layout WITH lcDirName
	ELSE
		APPEND BLANK
		REPLACE objname WITH tcExportFolderType, USER WITH GoUser.USER, layout WITH lcDirName
	ENDIF
	IF lcloseit
		IF CURSORGETPROP("Buffering") > 1
			TABLEUPDATE()
		ENDIF
		USE IN RESOURCE
		RELEASE oResAdptr
	ENDIF
ELSE
	cFullPdfPath = ""
	lcBuffer = ""
* 5 - 0x0005 CSIDL_PERSONAL A typical path is C:\Documents and Settings\username\My Documents.
	lnRes = GetFolderPath(lnStoreFolderConstant, @lcBuffer)
	IF lnRes = 0
		cFullPdfPath = lcBuffer
	ENDIF
	lcOldSetDefault = ""
	IF !EMPTY(cFullPdfPath)
		*{V&U MS 2012-07-17 5861
		lcOldSetDefault = GetDefaultFolder()
		*}V&U MS 2012-07-17
		IF DIRECTORY(cFullPdfPath)
			CD (cFullPdfPath)
		ENDIF
	ENDIF
	lcFileName = ValidFileName(PUTFILE(lcCaption, ValidFileName(tcFileName), tcFileType))

	IF !EMPTY(lcOldSetDefault) AND DIRECTORY(lcOldSetDefault, 1)
		*{V&U MS 2012-07-17 5861
		CD (lcOldSetDefault)
		*}V&U MS 2012-07-17
	ENDIF
ENDIF
*** MS 21.04.2004

SELECT(lnOldWorkArea)

RETURN lcFileName
ENDFUNC

*-------------------------------------------------------
* Function....: CheckVFXPublicObject()
* Called by...:
*
* Abstract....: Check if exist Object and Property. And if not - Creates them with DefaultValue
*
* Returns.....:
*
* Parameters..:	ObjectName, PorpertyName, DefaultValue
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CheckVFXPublicObject(ObjectName, PorpertyName, DEFAULTVALUE)

IF TYPE(ObjectName) <> "O"
	PUBLIC (ObjectName)
	&ObjectName = CREATEOBJECT("Empty")
ELSE
	IF !PEMSTATUS(&ObjectName,PorpertyName,5)
		ADDPROPERTY(&ObjectName, PorpertyName, DEFAULTVALUE)
	ENDIF
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: CheckUnicodeCompliance()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcLCID
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CheckUnicodeCompliance
LPARAMETERS tcLCID AS STRING

LOCAL m.lcWinUserLCID AS STRING
LOCAL m.lnLCID AS INTEGER

tcLCID = UPPER(RIGHT("00" + tcLCID, 2))
m.lcWinUserLCID = ""

DECLARE SHORT GetSystemDefaultLCID IN kernel32

m.lnLCID = GetSystemDefaultLCID()
CLEAR DLLS "GetSystemDefaultLCID"

m.lcWinUserLCID = UPPER(RIGHT("00" + TRANSFORM(m.lnLCID, "@0"), 2))

**MS
LOCAL lcAlias, lcRes
lcAlias = SELECT()
IF TYPE("goProgram.class") ="C"
	IF FILE("vfxLanguage.dbf")
		USE vfxLanguage SHARED AGAIN IN 0
	ELSE
		IF FILE("DATA\vfxLanguage.dbf")
			USE DATA\vfxLanguage SHARED  AGAIN IN 0
		ENDIF
	ENDIF
ELSE
	IF FILE("DATA\vfxLanguage.dbf")
		USE DATA\vfxLanguage SHARED  AGAIN IN 0
	ENDIF
ENDIF

IF !USED("vfxLanguage")
	RETURN .T.
ENDIF
SELECT vfxLanguage
LOCATE FOR ALLTRIM(UPPER(LangLCID)) == tcLCID
IF FOUND()
	IF ATC([,] + m.lcWinUserLCID +[,],[,] + STRTRAN(vfxLanguage.CompList,[ ],[]) +[,]) > 0	&&INLIST(m.lcWinUserLCID,ALLTRIM(vfxLanguage.CompList))
		lcRes = .T.
	ELSE
		lcRes = .F.
	ENDIF
ELSE
	lcRes = .T.
ENDIF
USE IN vfxLanguage
IF !EMPTY(lcAlias)
	SELECT (lcAlias)
ENDIF
RETURN lcRes
**MS
ENDFUNC


*-------------------------------------------------------
* Function....: GetDefaultFolder()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetDefaultFolder
*{V&U MS 2012-07-17 5861 
IF VERSION(2) = 2
     RETURN SYS(5) + SYS(2003)
ELSE
     RETURN JUSTPATH(SYS(16, 0))
ENDIF
*}V&U MS 2012-07-17
ENDFUNC

*-------------------------------------------------------
* Function....: ReplaceMemo()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcMemoContents, tcNewRow, tnNewRowPosition
*
* Notes.......:
*-------------------------------------------------------
FUNCTION ReplaceMemo
LPARAMETERS tcMemoContents, tcNewRow, tnNewRowPosition

LOCAL lnCurrentRow, lnRowNumber AS INTEGER
LOCAL lcNewMemo, lcCR AS STRING

lcNewMemo = ""
lcCR = ""
lnRowNumber = MAX(MEMLINES(tcMemoContents), tnNewRowPosition)

FOR lnCurrentRow = 1 TO lnRowNumber
	lcNewMemo = lcNewMemo + lcCR + ;
		IIF((lnCurrentRow = tnNewRowPosition), tcNewRow, ;
		MLINE(tcMemoContents, lnCurrentRow))

	lcCR = CHR(13) + CHR(10)
ENDFOR

RETURN lcNewMemo
ENDFUNC


*-------------------------------------------------------
* Function....: GetDefaultEMailClient
* Called by...:
*
* Abstract....:
*
* Returns.....: Default e-mail client: -1 Error, 0 - Unknown, 1 - Outlook Express, 2 - Microsoft Outlook
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetDefaultEMailClient

LOCAL loReg AS OBJECT
LOCAL llResult AS Logical
LOCAL lcBuffer AS STRING

* create registry object
TRY
	loReg = NEWOBJECT("cregistry")
	llResult = .T.
CATCH TO oError
	llResult = .F.
ENDTRY
IF !llResult
	RETURN -1
ENDIF

lcBuffer = ""
loReg.GetRegKey("", @lcBuffer,"mailto\shell\open\command", ;
	HKEY_CLASSES_ROOT)

IF EMPTY(lcBuffer)
	RETURN -1
ENDIF

lcBuffer = LOWER(lcBuffer)
DO CASE
	CASE "msimn.exe" $ lcBuffer
* Outlook Express
		RETURN 1

	CASE "outlook.exe" $ lcBuffer
* Microsoft Outlook
		RETURN 2

	OTHERWISE
		RETURN 0
ENDCASE
ENDFUNC

*-------------------------------------------------------
* Function....: versnr
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION versnr
LPARAMETER tcNummer
LOCAL lcZ, lnPos
lcZ = ""
lnPos = AT(".", m.tcNummer)
IF m.lnPos > 1
	lcZ = PADL(LEFT(m.tcNummer, m.lnPos - 1), 4, "0")
	tcNummer = SUBSTR(m.tcNummer, m.lnPos + 1)
	lnPos = AT(".", m.tcNummer)
	IF m.lnPos > 1
		lcZ = m.lcZ + PADL(LEFT(m.tcNummer, m.lnPos - 1), 4, "0")
		tcNummer = SUBSTR(m.tcNummer, m.lnPos + 1)
		lcZ = m.lcZ + PADL(m.tcNummer, 4, "0")
	ENDIF
ELSE
	lcZ = m.tcNummer
ENDIF
RETURN m.lcZ
ENDFUNC

*-------------------------------------------------------
* Function....: LoadCustomizationSettings
* Called by...:	goProgram.relogon, cLoginDialog
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......: Loads curtomization settings
*-------------------------------------------------------
FUNCTION LoadCustomizationSettings
LOCAL llAllUsrCust
SELECT vfxUsr
nRecNo = RECNO()
LOCATE FOR userlevel = 1

IF !AllUsrCust
	ckey = UPPER(PADR(vfxUsr.USER, 32,' ') + "VFX_UserOptions")
ELSE
	ckey = UPPER(PADR(GoUser.USER, 32,' ') + "VFX_UserOptions")
ENDIF

llResourceIsOpen = USED("RESOURCE")
lnErrorNo = 0
IF !llResourceIsOpen
	TRY
		IF TYPE("goProgram.class") ="C" AND goProgram.nvfxSysTableLoc >= 1
			IF goProgram.CheckAndCreateSQLTable("vfxRes")
				*{V&U MS 2010-05-13, 2014-04-28
				oResAdptr = OPENTABLE("vfxRes", "IDVFXRES", .F., ;
					"INDEX ON UPPER(user)+UPPER(objname) TAG USER ADDITIVE COLLATE [MACHINE]", ;
					.T., ,"RESOURCE", "cbasevfxdataaccess", .F., .F., .F., .F., "INDEX,USER")
				*}V&U MS 2010-05-13	, 2014-04-28
				SET ORDER TO TAG USER
			ELSE
				RETURN .F.
			ENDIF
		ELSE
			USE vfxres IN 0 ORDER TAG USER AGAIN ALIAS RESOURCE
		ENDIF
	CATCH TO oError
		lnErrorNo = oError.ERRORNO
	ENDTRY
	IF !USED("RESOURCE")
		RETURN .F.
	ENDIF
ENDIF

IF lnErrorNo > 0
	onerror(oError.ERRORNO, PROGRAM(), oError.LINENO, oError.MESSAGE, oError.LINECONTENTS)
	RETURN .F.
ENDIF

IF SEEK(ckey,"RESOURCE","USER")
	lcLayout = ALLTRIM(RESOURCE.Layout)
ELSE
	IF !EMPTY(NVL(goSystem.Customize,""))
		lcLayout = ALLTRIM(goSystem.Customize)
	ELSE
		*{V&U MS 2010-01-19, Modified
		*{V&U VM 2013-01-15, Modified
		lcLayout = "0" + CHR(13) + CHR(10) + ;	&& LargeIcons
			"1" + CHR(13) + CHR(10) + ;			&& ShowScreenTips
			"1" + CHR(13) + CHR(10) + ;			&& ShowShortcutKeys
			"1"	 + CHR(13) + CHR(10) + ; 		&& CloseOnESC
			IIF(goProgram.nasktosave = 1,"1","0") + CHR(13) + CHR(10) + ; 		&& AskToSave
			IIF(goProgram.nautoeditmode = 1,"1","0") + CHR(13) + CHR(10) + ; 	&& AutoEdit
			IIF(goProgram.nenteriseditingrid = 1,"1","0") + CHR(13) + CHR(10) + ; && EnterEdit
			"1"	 + CHR(13) + CHR(10) + ; 			&& UseTab/AutoSkip
			"1"	 + CHR(13) + CHR(10) + ; 			&& AutoPick
			"1"	 + CHR(13) + CHR(10) + ;			&& SavePStn
			"0"	 + CHR(13) + CHR(10) + ;			&& AddFvrtsToXpOpen
			"1"	 + CHR(13) + CHR(10) + ;			&& ActivateThemes
			"0"	 + CHR(13) + CHR(10) + ;			&& AutohideXPOpenDialog
			TRANSFORM(goProgram.nMenuAndToolbarStyle + 1) + CHR(13) + CHR(10) + ; 	&& OpenDialogMenuStyle				
			""	 + CHR(13) + CHR(10) + ;			&& ColorScheme
			"0"  + CHR(13) + CHR(10) + ;			&& ShowTaskList
			"1"	 + CHR(13) + CHR(10) + ;			&& ShowTips
			IIF(goProgram.nActivateFormPadInRibbonBar = 1, "1", "0") + CHR(13) + CHR(10) + ; && ActivateFormPad
			"0"	+ CHR(13) + CHR(10)	+ ;				&& Add Recent Forms To XPOpen
							"8944385" + CHR(13) + CHR(10)	+ ;		&& Main backcolor
							"15263693" + CHR(13) + CHR(10)	+ ;		&& Text backcolor
							"16777170" + CHR(13) + CHR(10)	+ ;		&& Forecolor
							"8421504" + CHR(13) + CHR(10)			&& Item backcolor
		*}V&U VM 2013-01-15
		*}V&U MS 2010-01-19	
	ENDIF
	*{V&U MS 2013-02-04
	IF goUser.AllUsrCust
		SELECT RESOURCE
		INSERT INTO RESOURCE (USER, objname) VALUES (UPPER(PADR(GoUser.USER, 32,' ')), ;
			"VFX_UserOptions")
		REPLACE Layout WITH lcLayout
	ENDIF
	*}V&U MS 2013-02-04
ENDIF
ADDPROPERTY(goUser,"CloseOnEsc", VAL(MLINE(lcLayout, 4)))
ADDPROPERTY(goUser,"AskToSave", VAL(MLINE(lcLayout, 5)))
ADDPROPERTY(goUser,"AutoEdit", VAL(MLINE(lcLayout, 6)))
ADDPROPERTY(goUser,"EnterEdit", VAL(MLINE(lcLayout, 7)))
ADDPROPERTY(goUser,"UseTab", VAL(MLINE(lcLayout, 8)))
ADDPROPERTY(goUser,"AutoPick", VAL(MLINE(lcLayout, 9)))
ADDPROPERTY(goUser,"SavePstn", VAL(MLINE(lcLayout, 10)))
ADDPROPERTY(goUser,"AddFavToXPOpen", VAL(MLINE(lcLayout, 11)))
IF !EMPTY(MLINE(lcLayout, 12))
	ADDPROPERTY(goUser,"ActivateThemes", VAL(MLINE(lcLayout, 12)))
ELSE
	ADDPROPERTY(goUser,"ActivateThemes", 1)
ENDIF
ADDPROPERTY(goUser,"AutoHideXPOpen", VAL(MLINE(lcLayout, 13)))
*{V&U MS 2010-01-19
ADDPROPERTY(goUser,"ActivateFormPadInRibbonBar", VAL(EVL(MLINE(lcLayout,18), "1")))
*}V&U MS 2010-01-19
*{V&U MS 2010-06-30
ADDPROPERTY(goUser,"AddRecentFormsToXPOpen", VAL(EVL(MLINE(lcLayout,19), "0")))
*}V&U MS 2010-06-30
SELECT vfxUsr
IF nRecNo > 0 AND nRecNo <= RECCOUNT()
	GOTO nRecNo
ENDIF

IF USED("RESOURCE")
	IF CURSORGETPROP("Buffering") > 1
		TABLEUPDATE()
	ENDIF
	USE IN RESOURCE
	RELEASE oResAdptr
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: CreateDefaultClientName
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......: Create Default Client Name
*-------------------------------------------------------
FUNCTION CreateDefaultClientName
LPARAMETERS tcConnStr AS STRING, tcConnStrType AS STRING

LOCAL lcClientName, lnPos, lcBuffer
* 1
lcClientName = ""
IF EMPTY(tcConnStr) OR VARTYPE(tcConnStr) <> "C"
	RETURN lcClientName
ENDIF
IF EMPTY(tcConnStrType) OR VARTYPE(tcConnStrType) <> "C"
	RETURN tcConnStr
ENDIF

tcConnStrType = STRTRAN(UPPER(ALLTRIM(tcConnStrType)),"A","") && Remove character "A" if used ADO connection
tcConnStr = ALLTRIM(tcConnStr)

DO CASE
* 2
	CASE tcConnStrType == "D" && Datebase Connection
		lnPos = AT("!", tcConnStr)
		IF lnPos > 0
			lcClientName = SUBSTR(tcConnStr, lnPos + 1)
		ELSE
			lcClientName = tcConnStr
		ENDIF
* 3
	CASE tcConnStrType == "C" && Connection String
		lcBuffer = STRTRAN(tcConnStr, " =","=")
		lcBuffer = STRTRAN(lcBuffer, "= ","=")
		lnPos = ATC("DATABASE=", lcBuffer)
		IF lnPos > 0
			lcBuffer = SUBSTR(lcBuffer, lnPos + 9)
			lnPos = ATC(";", lcBuffer)
			IF lnPos > 0
				lcClientName = LEFT(lcBuffer, lnPos - 1)
			ELSE
				lcClientName = lcBuffer
			ENDIF
		ELSE
			lcClientName = tcConnStr
		ENDIF
* 4
	CASE tcConnStrType == "N" && DSN
		lnPos = ATC("DSN=", tcConnStr)
		IF lnPos > 0
			lcBuffer = SUBSTR(tcConnStr, lnPos + 4)
			lnPos = ATC(";", lcBuffer)
			IF lnPos > 0
				lcClientName = LEFT(lcBuffer, lnPos - 1)
			ELSE
				lcClientName = lcBuffer
			ENDIF
		ELSE
			lcClientName = tcConnStr
		ENDIF
* 5
	CASE tcConnStrType == "P" &&Database

		lcClientName = JUSTSTEM(tcConnStr)
ENDCASE

RETURN PROPER(lcClientName)
ENDFUNC

*-------------------------------------------------------
* Function....: GetWinUserDefaultLangID()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetWinUserDefaultLangID

LOCAL lnLCID, lcLangID
DECLARE SHORT GetUserDefaultLCID IN kernel32

lnLCID = GetUserDefaultLCID()
CLEAR DLLS "GetUserDefaultLCID"

m.lcWinUserLCID = UPPER(RIGHT("00" + TRANSFORM(m.lnLCID, "@0"), 2))

**MS
LOCAL lcAlias, llUseInVfxLang
lcAlias = SELECT()
IF !USED("vfxLanguage")
	llUseInVfxLang = .T.
	IF TYPE("goProgram.class") ="C"
		IF FILE("vfxLanguage.dbf")
			USE vfxLanguage SHARED AGAIN IN 0
		ELSE
			IF FILE("DATA\vfxLanguage.dbf")
				USE DATA\vfxLanguage SHARED  AGAIN IN 0
			ENDIF
		ENDIF
	ELSE
		IF FILE("DATA\vfxLanguage.dbf")
			USE DATA\vfxLanguage SHARED  AGAIN IN 0
		ENDIF
	ENDIF
	IF !USED("vfxLanguage")
		#IFDEF id_language
			RETURN id_language
		#ELSE
			RETURN "ENG"
		#ENDIF
	ENDIF
ENDIF
SELECT vfxLanguage
LOCATE FOR ALLTRIM(UPPER(LangLCID)) == m.lcWinUserLCID AND isActive = .T.
IF FOUND()
	lcLangID = ALLTRIM(vfxLanguage.LangAbbrev)
ELSE
	#IFDEF id_language
		lcLangID = id_language
	#ELSE
		lcLangID = "ENG"
	#ENDIF
ENDIF
IF llUseInVfxLang
	USE IN vfxLanguage
ENDIF
IF !EMPTY(lcAlias)
	SELECT (lcAlias)
ENDIF
**MS

RETURN lcLangID
ENDFUNC

*-------------------------------------------------------
* Function....: GetDataSourcePlatform()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:tnConnectionHandle
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetDataSourcePlatform
LPARAMETERS tnConnectionHandle, tcConnectionString

LOCAL lcConnStr, lcDriver, lnSeparatorPos, lnDSNNamePos, lcDSNName, lnDataBaseServerType, ;
	laDSN[1,1], lnDataSourcesCount

lcConnStr = ""

IF VARTYPE(tnConnectionHandle) = "N" AND tnConnectionHandle > 0
	lcConnStr = SQLGETPROP(tnConnectionHandle,"ConnectString")
ELSE
	IF VARTYPE(tcConnectionString) = "C"
		lcConnStr = tcConnectionString
	ENDIF
ENDIF

IF EMPTY(lcConnStr)
	RETURN ""
ENDIF

lnDataSourcesCount = AODBCDataSources(@laDSN)

lcDriver = ""
lnDriverStrPos = ATC("DRIVER=", UPPER(lcConnStr))
IF lnDriverStrPos > 0
	lcDriver = SUBSTR(lcConnStr, lnDriverStrPos + 7)
	lnSeparatorPos = AT(";", lcDriver)
	IF lnSeparatorPos > 0
		lcDriver = LEFT(lcDriver, lnSeparatorPos - 1)
	ENDIF
ELSE
	lnDSNNamePos = AT("DSN=", UPPER(lcConnStr))
	IF lnDSNNamePos > 0
		lcDSNName = ALLTRIM(SUBSTR(lcConnStr, lnDSNNamePos + 4))
		lnSeparatorPos = AT(";", UPPER(lcDSNName))
		IF lnSeparatorPos > 0
			lcDSNName = LEFT(lcDSNName, lnSeparatorPos - 1)
		ENDIF
		FOR k = 1 TO lnDataSourcesCount
			IF UPPER(lcDSNName) == ALLTRIM(UPPER(laDSN[k,1]))
				lcDriver = ALLTRIM(laDSN[k,2])
				EXIT
			ENDIF
		ENDFOR
	ENDIF
ENDIF

DO CASE
	CASE ATC("FoxPro", lcDriver) <> 0 AND ATC("Microsoft", lcDriver) <> 0
		lnDataBaseServerType = 1
	CASE ATC("SQL Server", lcDriver) <> 0 OR ATC("SQL Native Client", lcDriver) <> 0
		lnDataBaseServerType = 2
	CASE ATC("Oracle", lcDriver) <> 0
		lnDataBaseServerType = 3
	CASE ATC("IBM", lcDriver) <> 0 OR ATC("DB2", lcDriver) <> 0
		lnDataBaseServerType = 4
*{V&U MS 2011-12-13 5595		 
	CASE ATC("MySQL", lcDriver) <> 0 
		lnDataBaseServerType = 5
*}V&U MS 2011-12-13				
	OTHERWISE
		lnDataBaseServerType = 0
ENDCASE

RETURN lnDataBaseServerType
ENDFUNC

*-------------------------------------------------------
* Function....: AODBCDataSources()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION AODBCDataSources
LPARAMETERS taDSN

LOCAL lnDataSourcesNum, lnRetVal, lnODBCEnv, dsn, dsndesc, mdsn, mdesc, lcServerType

*{ HC BB 2016-02-29, 6924
DECLARE short SQLDataSources IN odbc32 ;
	LONG    henv, INTEGER fDir, STRING @ DSN, ;
	INTEGER DSNMax, INTEGER @pcbDSN, STRING @DESCRIPTION, ;
	INTEGER DescMax, INTEGER @desclen
*} HC BB 2015-02-29

*Do until all the data source names have been retrieved
lnRetVal = 0
lnODBCEnv = VAL(SYS(3053))
lnDataSourcesNum = 0
DIMENSION taDSN[1,2]

taDSN[1,2] = .NULL.

DO WHILE lnRetVal = 0 && SUCCESS
	dsn = SPACE(100)
	dsndesc = SPACE(100)
	mdsn = 0
	mdesc = 0
	lcServerType = ""

	lnRetVal = sqldatasources(lnODBCEnv, 1, ; &&SQL_FETCH_NEXT
	@dsn, 100, @mdsn, @dsndesc, 100, @mdesc)

	IF lnRetVal = 0 THEN &&if no error occurred
		lnDataSourcesNum = lnDataSourcesNum + 1
		DIMENSION taDSN[lnDataSourcesNum ,2]
		taDSN[lnDataSourcesNum ,1] = LEFT(dsn, AT(CHR(0), dsn) -1)
		taDSN[lnDataSourcesNum ,2] = LEFT(dsndesc, AT(CHR(0), dsndesc) -1)
	ENDIF
ENDDO

RETURN lnDataSourcesNum
ENDFUNC


*-------------------------------------------------------
* Function....: LangSetupRecursive()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION LangSetupRecursive
LPARAMETERS toObject AS OBJECT

LOCAL loObject AS OBJECT
LOCAL lnCurrentObjectID AS INTEGER

* Use Object.Objects because PageFrame object not content property Controls
* Check if Objects is not from base class control because Objects and Controls Properties is not valid
IF TYPE("toObject.Objects.Count") = "N" AND !(LOWER(toObject.BASECLASS) == "control")
	FOR lnCurrentObjectID = 1 TO toObject.OBJECTS.COUNT
		loObject = toObject.OBJECTS(lnCurrentObjectID)
		IF PEMSTATUS(loObject,"langsetup", 5)
			loObject.langsetup()
		ENDIF
		langsetuprecursive(loObject)
	ENDFOR
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: VFXACOPY()
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION VFXACOPY
LPARAMETERS taSourceArray, taDestinationArray

EXTERNAL ARRAY taSourceArray, taDestinationArray

LOCAL lnSourceArrayRowCount, lnSourceArrayColumnCount, lnDestinationArrayRowCount, lnDestinationArrayColumnCount, ;
	lnRowCount, lnColumnCount, lnCurrentRow, lnCurrentColumn, lnElementCopied AS INTEGER

lnSourceArrayRowCount = ALEN(taSourceArray, 1)
lnSourceArrayColumnCount = ALEN(taSourceArray, 2)
lnDestinationArrayRowCount = ALEN(taDestinationArray, 1)
lnDestinationArrayColumnCount = ALEN(taDestinationArray, 2)

lnRowCount = MIN(lnSourceArrayRowCount, lnDestinationArrayRowCount)
lnColumnCount = MIN(lnSourceArrayColumnCount, lnDestinationArrayColumnCount)

lnElementCopied = 0
FOR lnCurrentRow = 1 TO lnRowCount
	FOR lnCurrentColumn = 1 TO lnColumnCount
		taDestinationArray[lnCurrentRow , lnCurrentColumn] = taSourceArray[lnCurrentRow , lnCurrentColumn]
		lnElementCopied = lnElementCopied + 1
	ENDFOR
ENDFOR

RETURN lnElementCopied
ENDFUNC



***************************************************************
*--------------------------PDF LISTENER
DEFINE CLASS PDFListener AS utilityReportListener &&&OF PDFLISTENER_SUPERCLASSLIB

	LISTENERTYPE = 0
	targetFileExt = "PDF"

	PROTECTED FRXPrintInfoAlias, reportTempFiles[1], IsOle

	generateNewStyleOutput = .F.  && default to .T. if you subclass from FX and have FXs
&& but you can still generate newstyle if
&& AddReport includes a specified listener reference;
&& an explicit reference in the collection
&& OR an OBJECT clause in the Clauses element
&& overrides tlOmitListenerReferences

	GSLocation = ""	&&ADDBS(JUSTPATH(THIS.ClassLibrary))
	GSExecFile = ""	&&BASE_GS_EXEC_FILE
	PSDriverSetupName = "" &&PSETUP_DEFAULTNAME_LOC
	FRXPrintInfoAlias = "P" + SYS(2015)
	retainTempFiles = .F.
	oWinAPI = NULL
	ThrowOleError = .F.
	IsOLE = ( INLIST(_VFP.STARTMODE, VFPSTARTMODE_COMEXE, VFPSTARTMODE_INPROCDLL, VFPSTARTMODE_MTDLL ))
	WaitAttempts = 20
	WaitInterval = 200

	PROCEDURE INIT()
	IF DODEFAULT()
		IF VARTYPE(goprogram) = "O"
			THIS.appName = IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTPDF_APPNAME_LOC, OUTPUTPDF_APPNAME_LOC)
		ELSE
			THIS.appName = OUTPUTPDF_APPNAME_LOC
		ENDIF
		THIS.oWinAPI = CREATEOBJECT("VFPWinAPILib")
		THIS.oWinAPI.ThrowOleError = THIS.ThrowOleError
*THIS.GSLocation = THIS.GetDefaultGSLocation()
		RETURN NOT (THIS.HadError OR ISNULL(THIS.oWinAPI))
	ELSE
		RETURN .F.
	ENDIF
	ENDPROC

	PROCEDURE ERROR(nError, cMethod, nLine)
	DODEFAULT(nError, cMethod, nLine)
	IF (NOT THIS.lIgnoreErrors) AND ;
			THIS.IsOLE AND THIS.ThrowOleError
		IF EMPTY(THIS.LastErrorMessage)
			THIS.LastErrorMessage = MESSAGE() + ", line " + TRANSFORM(nLine)
		ENDIF
		COMRETURNERROR( _VFP.SERVERNAME, THIS.LastErrorMessage)
	ENDIF
	ENDPROC

	PROCEDURE ThrowOleError_Assign(tVal)
	IF VARTYPE(tVal) = "L"
		THIS.ThrowOleError = tVal
		IF NOT ISNULL(THIS.oWinAPI)
			THIS.oWinAPI.ThrowOLEError = .T.
		ENDIF
	ENDIF
	ENDPROC

	PROCEDURE RunReports(tlRemoveReportsAfterRun, tlOmitListenerReferences)
* NB: we could force NOPAGEEJECT
* on all reports in the collection
* and then remove from the last one in
* the run, rather than processing multiple
* temporary PS files as done in this class.
* However, the extra work provides extra
* flexibility; one print job in Fox still
* requires the page size to be the same for
* each of the reports in the NOPAGEEJECT
* sequence.  By running the separate reports
* to separate PS files and then using
* the appropriate GS syntax to bind them
* together we can include multiple report page
* sizes, or different page sizes in multiple
* document sections, as needed.
	IF (NOT ISNULL(THIS.ReportFileNames)) AND ;
			(NOT EMPTY(THIS.ReportFileNames[1])) AND ;
			THIS.LoadPrinterInfo()
		THIS.verifyTargetFile()
		IF PCOUNT() = 2 OR VARTYPE(THIS.generateNewStyleOutput) # "L"
*L DODEFAULT(tlRemoveReportsAfterRun, tlOmitListenerReferences)
			DODEFAULT(.F., tlOmitListenerReferences)
		ELSE
*L DODEFAULT(tlRemoveReportsAfterRun,NOT THIS.generateNewStyleOutput)
			DODEFAULT(.F., NOT THIS.generateNewStyleOutput)
		ENDIF
		THIS.UnloadPrinterInfo()
*L RETURN THIS.ProcessPDF()
		RETURN THIS.ProcessPDF(tlRemoveReportsAfterRun)
	ELSE
		RETURN .F.
	ENDIF
	ENDPROC

	PROTECTED PROCEDURE ProcessPDF(tlRemoveReportsAfterRun)	&&L ProcessPDF()
		IF EMPTY(THIS.reportTempFiles[1])
			RETURN .F.
		ELSE
* move the collection of reportTempFiles into a GS batch file
* for processing
			LOCAL llReturn, lcCmdFile, lcCmd, lcDir
			lcCmdFile = THIS.MakeGSCommandFile()
* shell out to gs
*lcCmd = FORCEPATH(THIS.GSExecFile,THIS.GSLocation)+ " @"+JUSTFNAME(lcCmdFile)
			lcCmd = FORCEPATH(THIS.GSExecFile, THIS.GSLocation) + " @" + lcCmdFile
			lcCmd = STRTRAN(lcCmd,"\","/")
			*{V&U MS 2012-07-17 5861
			lcDir = GetDefaultFolder()
			*}V&U MS 2012-07-17
			CD (THIS.GSLocation)
* llReturn = THIS.oWinAPI.ProgExecute(lcCmd," NOT FILE(["+THIS.TargetFileName+"]) ",THIS.WaitAttempts)
* or:
			llReturn = THIS.oWinAPI.ProgExecuteX(lcCmd, 1)
			CD (lcDir)
			IF (NOT llReturn) AND EMPTY(THIS.LastErrorMessage)
				IF VARTYPE(goprogram) = "O"
					THIS.LastErrorMessage = ;
						THIS.PrepareErrorMessage( ;
						VFP_USER_DEFINED_ERROR, ;
						PROGRAM(), ;
						0, ;
						THIS.AppName, ;
						IIF(goProgram.lRuntimeLocalization, goLocalize.cWIN_API_ERROR_LOC, WIN_API_ERROR_LOC) + ;
						TRANSFORM(THIS.oWinAPI.GetLastWindowsError()), ;
						"")
				ELSE
					THIS.LastErrorMessage = ;
						THIS.PrepareErrorMessage( ;
						VFP_USER_DEFINED_ERROR, ;
						PROGRAM(), ;
						0, ;
						THIS.AppName, ;
						WIN_API_ERROR_LOC + TRANSFORM(THIS.oWinAPI.GetLastWindowsError()), ;
						"")
				ENDIF
			ENDIF
			IF EMPTY(THIS.LastErrorMessage) AND NOT FILE(THIS.TargetFileName)
				IF VARTYPE(goprogram) = "O"
					THIS.LastErrorMessage = ;
						THIS.PrepareErrorMessage( ;
						VFP_USER_DEFINED_ERROR, ;
						PROGRAM(), ;
						0, ;
						THIS.AppName, ;
						IIF(goProgram.lRuntimeLocalization, goLocalize.cGENERAL_FAILURE_LOC, GENERAL_FAILURE_LOC) + ;
						" " + THIS.TargetFileName, ;
						"")
				ELSE
					THIS.LastErrorMessage = ;
						THIS.PrepareErrorMessage( ;
						VFP_USER_DEFINED_ERROR, ;
						PROGRAM(), ;
						0, ;
						THIS.AppName, ;
						GENERAL_FAILURE_LOC +" " + THIS.TargetFileName, ;
						"")
				ENDIF
			ENDIF
			IF (NOT EMPTY(THIS.LastErrorMessage)) AND THIS.IsOle AND THIS.ThrowOleError
				COMRETURNERROR( _VFP.SERVERNAME, THIS.LastErrorMessage)
			ENDIF
			THIS.CleanupTempFiles(lcCmdFile)
			IF tlRemoveReportsAfterRun
				THIS.RemoveReports()
			ENDIF
			RETURN llReturn
		ENDIF
		ENDPROC

	PROTECTED PROCEDURE CleanupTempFiles(tcFile)
		LOCAL liIndex, lvFile, lcRetain
		IF THIS.retainTempFiles
			lcRetain = " RECYCLE"
		ELSE
			lcRetain = ""
		ENDIF
		FOR liIndex = 1 TO ALEN(THIS.reportTempFiles)
			lvFile = THIS.reportTempFiles[liIndex]
			IF (NOT EMPTY(lvFile)) AND FILE(lvFile)
				ERASE (lvFile) &lcRetain
			ENDIF
		ENDFOR
		IF (NOT EMPTY(tcFile)) AND FILE(tcFile)
			ERASE (tcFile) &lcRetain
		ENDIF
		ENDPROC

	PROCEDURE AddReport(tcFRXName, tcClauses, toListener)
	LOCAL liReports
	IF ISNULL(THIS.reportClauses)
		liReports = 0
	ELSE
		liReports = THIS.reportClauses.COUNT
	ENDIF
	DODEFAULT(tcFRXName, tcClauses, toListener)
	IF (NOT ISNULL(THIS.reportClauses)) AND ;
			THIS.reportClauses.COUNT > liReports
		THIS.AdjustReportClauses(liReports + 1)
	ENDIF
	ENDPROC

	PROTECTED PROCEDURE RemoveReports()
		DODEFAULT()
		DIMENSION THIS.reportTempFiles[1]
		THIS.reportTempFiles[1] = .F.
		ENDPROC

	PROTECTED PROCEDURE AdjustReportClauses(tiWhich)
		LOCAL lcClauses, lcTempFile, laWords[1], liIndex, lcWord
		lcClauses = NVL(THIS.reportClauses,"")
		lcClauses = EVL(THIS.reportClauses[tiWhich],"")
*lcTempFile = FORCEEXT(FORCEPATH("PS"+SYS(2015),THIS.GSLocation),".PS")
		lcTempFile = FORCEEXT(FORCEPATH("PS" + SYS(2015), ADDBS(SYS(2023))), ".PS")
* parse out the possible PREVIEW/TO PRINT
		lcClauses = " " + UPPER(lcClauses)
		IF " TO " $ lcClauses
* TO PRINT or TO FILE <name>, where FILE is optional
			lcClauses = STRTRAN(lcClauses,"FILE","")
		ENDIF
* now get rid of TO and the following word and get rid of PREVIEW and PROMPT.
* we have to do this because we can't assume that a
* listener will be attached to the REPORT FORM command (could be
* old style) or that, if it is, it is one of the appropriate
* listener types. It's probably a user error
* if these clauses occur, and there are other possible
* problem clauses (for example IN SCREEN) that shouldn't
* be used in a COM object, but these should take care of most cases.
		ALINES(laWords, lcClauses, 4," ")
		lcClauses = ""
		FOR liIndex = 1 TO ALEN(laWords)
			IF EMPTY(laWords[liIndex])
				lcWord = " "
			ELSE
				lcWord = " " + laWords[liIndex] +" "
			ENDIF
			DO CASE
				CASE " TO " $ lcWord
* don't include, and
* need to get rid of the following word
					laWords[liIndex+ 1] = " "
				CASE " PROM " $ lcWord OR ;
						" PROMP " $ lcWord OR ;
						" PROM " $ STRTRAN(lcWord,"PT", " ")
* don't include
				CASE " PREV " $ lcWord OR ;
						" PREVI " $ lcWord OR ;
						" PREVIE " $ lcWord OR ;
						" PREV " $ STRTRAN(lcWord,"IEW", " ")
* don't include
				OTHERWISE
					lcClauses = lcClauses + " " + lcWord
			ENDCASE
		ENDFOR
* adjust the clauses to go TO FILE
* with a temporary name
* and, because it can be old-style
* make sure to add NODIALOG for auto-quietmode behavior
		lcClauses = " " + lcClauses + " "

		IF ATC(" NODI ", lcClauses) = 0 AND ;
				ATC(" NODIA ", lcClauses) = 0 AND ;
				ATC(" NODIAL ", lcClauses) = 0 AND ;
				ATC(" NODIALO ", lcClauses) = 0 AND ;
				ATC(" NODIALOG ", lcClauses) = 0
			lcClauses = lcClauses + " NODIALOG "
		ENDIF

		IF ATC(" NOCO ", lcClauses) = 0 AND ;
				ATC(" NOCON ", lcClauses) = 0 AND ;
				ATC(" NOCONS ", lcClauses) = 0 AND ;
				ATC(" NOCONSO ", lcClauses) = 0 AND ;
				ATC(" NOCONSOL ", lcClauses) = 0 AND ;
				ATC(" NOCONSOLE ", lcClauses) = 0
			lcClauses = lcClauses + " NOCONSOLE "
		ENDIF

		THIS.reportClauses.REMOVE[tiWhich]
*L THIS.reportClauses.Add(lcClauses + " TO FILE "+ UPPER(lcTempFile))
		THIS.reportClauses.ADD(lcClauses + " TO FILE ([" + UPPER(lcTempFile) + "]) " )
* add to the collection of temporary Filenames
		IF tiWhich > 1
			DIMENSION THIS.reportTempFiles[tiWhich]
		ENDIF
		THIS.reportTempFiles[tiWhich] = lcTempFile
		ENDPROC

	PROCEDURE SUPPORTSLISTENERTYPE(ti)
	RETURN INLIST(ti, LISTENER_TYPE_PRN) AND ;
		(NOT THIS.IsSuccessor)
* NB/TBD: you could do this as a successor
* and support LISTENER_TYPE_PAGED as well
* as LISTENER_TYPE_PRN,
* if you opened your own print job and
* fed that handle to the engine in OutputPage.
* Not being done in this version,
* by default this class will pass references to
* itself as the listener in the OBJECT clause
* to all REPORT FORM commands anyway unless
* we're in an "old style" reporting situation.
	ENDPROC

	PROTECTED PROCEDURE MakeGSCommandFile()
		LOCAL lcFile, lcContents, lcFileString, liH
		lcFile = FORCEPATH("C" + SYS(2015) +".TXT", SYS(2023))
		lcFileString = THIS.GetQuotedFileString()
		lcContents = " -I..\lib;..\..\fonts -q -dNOPAUSE " + ;
			"-sDEVICE=pdfwrite -sOUTPUTFILE=" + ;
			["] + THIS.TargetFileName +["] + ;
			" -dBATCH " + ;
			 + lcFileString
* forward slashes or doublebackslashes
		lcContents = STRTRAN(lcContents,"\","/")
		IF FILE(lcFile)
			ERASE (lcFile)
		ENDIF
		liH = FCREATE(lcFile)
		FPUTS(liH, lcContents)
		FFLUSH(liH, .T.)
		FCLOSE(liH)
*      STRTOFILE(lcContents, lcFile) seemed to cause some problems
		RETURN lcFile

	PROTECTED PROCEDURE GetQuotedFileString()
		LOCAL liIndex, lcReturn, lcFile, lcFileContent
		lcReturn = ""
		FOR liIndex = 1 TO ALEN(THIS.reportTempFiles)
			IF NOT EMPTY(THIS.reportTempFiles[liIndex])
				lcFile = ALLTRIM(THIS.reportTempFiles[liIndex])
*{JEI MS 15.09.2006
				lcFileContent = FILETOSTR(lcFile)
				lcFileContent = STRTRAN(lcFileContent,"%%EOF" + CHR(13) + CHR(10) + CHR(4),"%%EOF" + CHR(13) + CHR(10))
				STRTOFILE(lcFileContent, lcFile)
*}JEI MS 15.09.2006
				IF LEFT(lcFile, 1) # ["]
					lcFile = ["] + lcFile
				ENDIF
				IF RIGHT(lcFile, 1) # ["]
					lcFile = lcFile + ["]
				ENDIF
				lcFile = " " + lcFile + " "
				lcReturn = lcReturn + lcFile
			ENDIF
		ENDFOR
		RETURN lcReturn

	PROCEDURE LoadPrinterInfo()
	IF NOT THIS.VerifyGSLibrary()
		RETURN .F.
	ENDIF
	IF NOT THIS.VerifyPrinterSetup()
		RETURN .F.
	ENDIF
	IF NOT THIS.AdjustVFPPrinterSetups()
		RETURN .F.
	ENDIF
	ENDPROC

*L PROCEDURE UnloadPrinterInfo()
	PROTECTED PROCEDURE UnloadPrinterInfo()
		IF USED(THIS.FRXPrintInfoAlias)
			LOCAL liSelect
			liSelect = SELECT()
			SELECT (THIS.FRXPrintInfoAlias)
* restore current printer setup from special FRX in
* FRXDataSession
			SYS(1037, 3)
			USE IN (THIS.FRXPrintInfoAlias) && we're finished with it
			SELECT (liSelect)
		ENDIF
		ENDPROC

	PROTECTED PROCEDURE PrepareFRXPrintInfo()
		LOCAL lcFile, liSelect, llCreated
		liSelect = SELECT()
		IF TYPE("THIS.CommandClauses.File") # "C" OR ;
				EMPTY(SYS(2000, THIS.COMMANDCLAUSES.FILE)) OR  ;
				NOT USED("FRX")
* could be built into the app, could be used
* at a different time than FRX is available
* or simply could be used in a different
* session than FRXDataSession
			lcFile = FORCEEXT(FORCEPATH("F" + SYS(2015), JUSTPATH(THIS.targetFileName)),"FRX")
			SELECT 0
			CREATE CURSOR x (onefield l)
			CREATE REPORT (lcFile) FROM (DBF("x"))
			USE IN x
			SELECT 0
			USE (lcFile) ALIAS TheFRX
			llCreated = .T.
		ELSE
			lcFile = DBF("FRX")
		ENDIF
		SELECT * FROM (lcFile) ;
			INTO CURSOR (THIS.FRXPrintInfoAlias) READWRITE ;
			WHERE RECNO() = 1
		SELECT (THIS.FRXPrintInfoAlias)
		REPLACE EXPR WITH "", TAG WITH "", Tag2 WITH ""
		IF llCreated
			USE IN TheFRX
			ERASE (lcFile)
			ERASE (FORCEEXT(lcFile,"FRT"))
		ELSE
* TBD:
* also attempt to remove printer setup info from
* The FRX? If readonly, do not attempt
		ENDIF
		SELECT (liSelect)
		RETURN llCreated
		ENDPROC

	PROTECTED PROCEDURE AdjustVFPPrinterSetups()
		LOCAL llFailure, oError AS EXCEPTION, liSelect
*THIS.setFRXDataSession()
		liSelect = SELECT()
		TRY
			THIS.prepareFRXPrintInfo()
			SELECT (THIS.FRXPrintInfoAlias)
			SYS(1037, 2)
			SET PRINTER TO NAME (THIS.PSDriverSetupName)
		CATCH TO oError
			THIS.DOMESSAGE(oError.MESSAGE, MB_ICONEXCLAMATION)
			THIS.LastErrorMessage = ;
				THIS.PrepareErrorMessage( ;
				VFP_USER_DEFINED_ERROR, ;
				PROGRAM(), ;
				oError.LINENO, ;
				THIS.AppName, ;
				oError.MESSAGE, ;
				oError.LINECONTENTS)
			llFailure = .T.
		ENDTRY
		SELECT (liSelect)
*THIS.setCurrentDataSession()
		RETURN NOT llFailure
		ENDPROC

	PROCEDURE VerifyPrinterSetup()
	RETURN .T.
	ENDPROC

	PROCEDURE GetDefaultGSLocation()
	LOCAL aDummy[1]

	IF EMPTY(ASTACKINFO(aDummy))
		RETURN UPPER(ADDBS(JUSTPATH(THIS.CLASSLIBRARY)))
	ELSE
		RETURN UPPER(ADDBS(JUSTPATH(aDummy[1,4])))
	ENDIF
	ENDPROC

	PROCEDURE VerifyGSLibrary()
	RETURN NOT EMPTY(SYS(2000, FORCEPATH(THIS.GSExecFile, THIS.GSLocation)))
	ENDPROC


ENDDEFINE

DEFINE CLASS VFPWinAPILib AS CUSTOM

	PROTECTED IsOle

	ThrowOleError = .F.
	IsOLE = ( INLIST(_VFP.STARTMODE, VFPSTARTMODE_COMEXE, VFPSTARTMODE_INPROCDLL, VFPSTARTMODE_MTDLL ))
	LastErrorMessage = ""

	PROCEDURE ERROR(nError, cMethod, nLine)
	THIS.LastErrorMessage = THIS.CLASS +" error #" + ;
		TRANSFORM(nError) +", " + cMethod +"." + ;
		TRANSFORM(nLine) +" [" + MESSAGE() +"]"
	IF THIS.IsOLE AND THIS.ThrowOleError
		COMRETURNERROR( _VFP.SERVERNAME, THIS.LastErrorMessage)
	ENDIF
	ENDPROC


	PROCEDURE GetLastWindowsError()
	DECLARE INTEGER GetLastError IN kernel32.DLL
	RETURN GetLastError()

	PROCEDURE ProgExecute(zCmdLine, cFailureConditions, nWait, cStatus)
	LOCAL liTrials, lcConditions
	IF NOT EMPTY(cFailureConditions)
		lcConditions = "(" + cFailureConditions + ") AND "
	ELSE
		lcConditions = ""
	ENDIF
	IF NOT (EMPTY(cStatus) OR THIS.IsOle)
		IF TYPE("goProgram.class") ="C"
			goProgram.vfxwaitwindow(cStatus)
		ELSE
			WAIT WINDOW cStatus
		ENDIF
	ENDIF
	TRY
		oShell = CREATEOBJECT("WScript.Shell")
		oShell.EXEC(zCmdLine)
	CATCH WHEN .T.
* don't want to use RUN command unless necessary
		RUN / N2 &zCmdLine.
	FINALLY
		IF VARTYPE(nWait) = "N"
			DECLARE Sleep IN Win32API INTEGER
			liTrials = 0
			DO WHILE &lcConditions. liTrials < 100
				SLEEP(1000)
				liTrials = liTrials + 1
			ENDDO
		ENDIF
		IF NOT (EMPTY(cStatus) OR THIS.IsOle)

*JEI VM 20080829{
			IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
				goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
			ELSE
*JEI VM 20080829}
				WAIT CLEAR
			ENDIF

		ENDIF
		oShell = NULL
	ENDTRY

	IF EMPTY(cFailureConditions)
		RETURN .T.
	ELSE
		RETURN (NOT &cFailureConditions.)
	ENDIF

	ENDPROC



* from this point to the end of the class is a
* slightly edited version of code from:
* http://www.experts-exchange.com/Databases/FoxPro/Q_10315608.html

	#DEFINE NORMAL_PRIORITY_CLASS 32
	#DEFINE IDLE_PRIORITY_CLASS 64
	#DEFINE HIGH_PRIORITY_CLASS 128
	#DEFINE REALTIME_PRIORITY_CLASS 1600

* Return code from WaitForSingleObject() if
* it timed out.
	#DEFINE WAIT_TIMEOUT 0x00000102
	#DEFINE TIMEOUT_INTERVAL 200
	#DEFINE TIMEOUT_ATTEMPTS 20

	#DEFINE PROCESSTIMEOUT 4294967295


	PROCEDURE ProgExecuteX(zCmdLine, nWait)
* zCmdLine ... Commandline
* nWait    ... 1 = FoxPro waits until the called program is finished

	DECLARE INTEGER CreateProcess IN kernel32.DLL ;
		INTEGER lpApplicationName, ;
		STRING lpCommandLine, ;
		INTEGER lpProcessAttributes, ;
		INTEGER lpThreadAttributes, ;
		INTEGER bInheritHandles, ;
		INTEGER dwCreationFlags, ;
		INTEGER lpEnvironment, ;
		INTEGER lpCurrentDirectory, ;
		STRING @lpStartupInfo, ;
		STRING @lpProcessInformation
	
	*{ HC BB 2016-02-29, 6924
	DECLARE INTEGER WaitForSingleObject IN WIN32API ;
		LONG    hHandle, INTEGER dwMilliseconds
	*} HC BB 2015-02-29

	DECLARE INTEGER GetLastError IN WIN32API

	LOCAL zStartUp, zProcess, nHandle, nReturn
	IF PCOUNT() < 2
		nWait = 0
	ENDIF

	zStartUp =    THIS.getStartupInfo() && REPLICATE(CHR(0), 68) &&
	zProcess = REPLICATE(CHR(0), 16)
	nReturn = CreateProcess(0, zCmdLine, 0, 0, 1, HIGH_PRIORITY_CLASS, 0, 0, @zStartUp, @zProcess)
	IF nWait = 1
		nHandle = ASC(LEFT(zProcess, 1)) + ;
			BITLSHIFT(ASC(SUBSTR(zProcess, 2, 1)), 8) + ;
			BITLSHIFT(ASC(SUBSTR(zProcess, 3, 1)), 16) + ;
			BITLSHIFT(ASC(SUBSTR(zProcess, 4, 1)), 24)
		WaitForSingleObject(nHandle, PROCESSTIMEOUT )
	ENDIF

	RETURN (nReturn # 0)

	PROCEDURE getStartupInfo
* creates the STARTUP structure to specify main window
* properties if a new window is created for a new process

*| typedef struct _STARTUPINFO {
*|     DWORD   cb;                4
*|     LPTSTR  lpReserved;        4
*|     LPTSTR  lpDesktop;         4
*|     LPTSTR  lpTitle;           4
*|     DWORD   dwX;               4
*|     DWORD   dwY;               4
*|     DWORD   dwXSize;           4
*|     DWORD   dwYSize;           4
*|     DWORD   dwXCountChars;     4
*|     DWORD   dwYCountChars;     4
*|     DWORD   dwFillAttribute;   4
*|     DWORD   dwFlags;           4
*|     WORD    wShowWindow;       2
*|     WORD    cbReserved2;       2
*|     LPBYTE  lpReserved2;       4
*|     HANDLE  hStdInput;         4
*|     HANDLE  hStdOutput;        4
*|     HANDLE  hStdError;         4
*| } STARTUPINFO, *LPSTARTUPINFO; total: 68 bytes
	#DEFINE STARTF_USESHOWWINDOW   1
	#DEFINE SW_SHOWMAXIMIZED       3
	#DEFINE SW_SHOWMINIMIZED       2
	RETURN  THIS.num2dword(68) + ;
		THIS.num2dword(0) + THIS.num2dword(0) + THIS.num2dword(0) + ;
		THIS.num2dword(0) + THIS.num2dword(0) + THIS.num2dword(0) + THIS.num2dword(0) + ;
		THIS.num2dword(0) + THIS.num2dword(0) + THIS.num2dword(0) + ;
		THIS.num2dword(STARTF_USESHOWWINDOW) + ;
		THIS.num2word(SW_SHOWMINIMIZED) + ;
		THIS.num2word(0) + THIS.num2dword(0) + ;
		THIS.num2dword(0) + THIS.num2dword(0) + THIS.num2dword(0)

* * *
* dword is compatible with LONG
	PROCEDURE  num2dword (lnValue)
	#DEFINE m0       256
	#DEFINE m1     65536
	#DEFINE m2  16777216
	LOCAL b0, b1, b2, b3
	b3 = INT(lnValue / m2)
	b2 = INT((lnValue - b3 * m2) / m1)
	b1 = INT((lnValue - b3 * m2 - b2 * m1) / m0)
	b0 = MOD(lnValue, m0)
	RETURN CHR(b0) + CHR(b1) + CHR(b2) + CHR(b3)
* * *
* dword is compatible with LONG
	FUNCTION  num2word (lnValue)
	RETURN CHR(MOD(m.lnValue, 256)) + CHR(INT(m.lnValue / 256))
* * *
	FUNCTION  buf2word (lcBuffer)
	RETURN ASC(SUBSTR(lcBuffer, 1, 1)) + ;
		ASC(SUBSTR(lcBuffer, 2, 1)) * 256
* * *
	FUNCTION  buf2dword (lcBuffer)
	RETURN;
		ASC(SUBSTR(lcBuffer, 1, 1)) + ;
		ASC(SUBSTR(lcBuffer, 2, 1)) * 256 + ;
		ASC(SUBSTR(lcBuffer, 3, 1)) * 65536 + ;
		ASC(SUBSTR(lcBuffer, 4, 1)) * 16777216
	ENDFUNC

	FUNCTION num2dword (lnValue)
* dword is compatible with LONG
	#DEFINE m0       256
	#DEFINE m1     65536
	#DEFINE m2  16777216
	LOCAL b0, b1, b2, b3
	b3 = INT(lnValue / m2)
	b2 = INT((lnValue - b3 * m2) / m1)
	b1 = INT((lnValue - b3 * m2 - b2 * m1) / m0)
	b0 = MOD(lnValue, m0)
	RETURN CHR(b0) + CHR(b1) + CHR(b2) + CHR(b3)
ENDDEFINE

*-------------------------------------------------------
* Function....: ContactUs
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION ContactUs()
LOCAL lcTempFilePathName AS STRING
LOCAL lnOldWorkArea AS INTEGER

IF !FILE("Vfxinternfiles.dbf")
	RETURN .F.
ENDIF

lnOldWorkArea = SELECT()

USE Vfxinternfiles.DBF AGAIN ALIAS Vfxinternfiles IN 0 SHARED
SELECT Vfxinternfiles
LOCATE FOR LOWER(ALLTRIM(TYPE)) = "contactus"
IF FOUND()
	lcTempFilePathName = ADDBS(SYS(2023)) + ALLTRIM(FileName)
	STRTOFILE(Content, lcTempFilePathName)
	DO FORM ("VFXContactUs") WITH lcTempFilePathName, .T. && .T. delete file after close form
ENDIF
USE IN Vfxinternfiles
SELECT(lnOldWorkArea)
ENDFUNC

*-------------------------------------------------------
* Function....: CopyNativeDBC
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcSourceFolder, tcDestinationFolder
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CopyNativeDBC
LPARAMETERS tcSourceFolder, tcDestinationFolder, tcFileSkeleton AS STRING

LOCAL loException AS EXCEPTION
LOCAL lnFileCount, lnCurentFileNum, lnFieldCount, lnCurrentField, lnErrorNo, lnOldWorkArea, lnLastFileNum, lnDBCCount, ;
	lnCurrentDBC, liDBCCount, liCurrentDBC AS INTEGER
LOCAL laFiles[1], laDummy[1], lcFileName, lcAutoincFieldName, laTables[1], laDatabases[1], lcErrorMessage, lcCR, ;
	lcFileSkeleton, lcSourceFolder, lcDBCName, laDBC[1] AS STRING
LOCAL llZapTableSuccessful, llErrorZapTable, llCreateFolder, llCheckForFPTAndCDX AS Logical


lnOldWorkArea = SELECT()

lcErrorMessage = ""
lcCR = ""
lcFileSkeleton = "*.dbf"
* 1
IF VARTYPE(tcFileSkeleton) = "C" AND !EMPTY(tcFileSkeleton)
	lcFileSkeleton = tcFileSkeleton
ENDIF

CREATE CURSOR crsSourceTables (TableName c(200), DeleteAllRows L, ResetIdentity L, ErrorMsg M)

* 2
IF ADIR(laDummy, tcSourceFolder) = 1 && DataBase Exist
	lcSourceFolder = ADDBS(JUSTPATH(tcSourceFolder))
	IF !DBUSED(tcSourceFolder)
		TRY
* 4
			OPEN DATABASE (tcSourceFolder) SHARED
		CATCH TO loException
			lcErrorMessage = loException.MESSAGE
		ENDTRY
	ENDIF
	IF DBUSED(tcSourceFolder) && 5
		SET DATABASE TO (tcSourceFolder)
* 6
		lnFieldCount = ADBOBJECTS(laTables,"TABLE")
		CLOSE DATABASES
	ELSE
* 7
		SELECT(lnOldWorkArea)
		RETURN lcErrorMessage
	ENDIF

* 8
	FOR lnCurentFileNum = 1 TO lnFieldCount
		APPEND BLANK
		REPLACE TableName WITH laTables[lnCurentFileNum] IN crsSourceTables
	ENDFOR
ELSE
	lcSourceFolder = ADDBS(tcSourceFolder)
* 9
	lnFieldCount = ADIR(laTables, tcSourceFolder + FORCEEXT(lcFileSkeleton, "dbf"))
	SELECT crsSourceTables
* 10
	FOR lnCurentFileNum = 1 TO lnFieldCount
		APPEND BLANK
		REPLACE TableName WITH laTables[lnCurentFileNum, 1] IN crsSourceTables
	ENDFOR
ENDIF

IF lnFieldCount = 0
	SELECT(lnOldWorkArea)
	RETURN [CANCEL]
ENDIF

SELECT crsSourceTables
LOCATE
* 11
*{V&U VM 2014-02-07 6375
DO FORM ("vfxtablezap") WITH .F. TO llRes
*}V&U VM 2014-02-07
IF !llRes
	USE IN crsSourceTables
	RETURN [CANCEL]
ENDIF

* 12 { Try close database
liDBCCount = ADATABASES(laDBC)
IF liDBCCount > 0 AND ADIR(laDummy, tcSourceFolder) = 1
	FOR liCurrentDBC = 1 TO liDBCCount
		IF FULLPATH(laDBC[liCurrentDBC,2]) == FULLPATH(tcSourceFolder)
			SET DATABASE TO (tcSourceFolder)
			CLOSE DATABASES
			EXIT
		ENDIF
	ENDFOR
ENDIF
*} Try close database

IF !DIRECTORY(tcDestinationFolder, 1)
* 13
	TRY
* 14
		MD(tcDestinationFolder)
		llCreateFolder = .T.
	CATCH TO loException
		lcErrorMessage = loException.MESSAGE
		lcCR = CHR(13) + CHR(10)
	ENDTRY
	IF !DIRECTORY(tcDestinationFolder, 1)
* 15
		SELECT(lnOldWorkArea)
* 16
		RETURN lcErrorMessage
	ENDIF
ENDIF

IF ADIR(laDummy, tcSourceFolder) = 1 && DBC exist
* 17
	lcDBCName = JUSTFNAME(tcSourceFolder)
	lnErrorNo = 0
	TRY
* 18
		COPY FILE (tcSourceFolder) TO (tcDestinationFolder + lcDBCName)
		COPY FILE (FORCEEXT(tcSourceFolder, "dct")) TO (tcDestinationFolder + FORCEEXT(lcDBCName, "dct"))
		COPY FILE (FORCEEXT(tcSourceFolder, "dcx")) TO (tcDestinationFolder + FORCEEXT(lcDBCName, "dcx"))
	CATCH TO loException
		lnErrorNo = loException.ERRORNO
		lcErrorMessage = lcErrorMessage + lcCR + loException.MESSAGE
		lcCR = CHR(13) + CHR(10)
	ENDTRY
ENDIF

lnLastFileNum = 0
SELECT crsSourceTables
* 19
SCAN
	lcFileName = FORCEEXT(ALLTRIM(crsSourceTables.TableName), "dbf")
	lnErrorNo = 0
* 20
	TRY
		COPY FILE (lcSourceFolder + lcFileName) TO (tcDestinationFolder + lcFileName)
		IF ADIR(laDummy, lcSourceFolder + FORCEEXT(lcFileName, "fpt")) = 1
			COPY FILE (lcSourceFolder + FORCEEXT(lcFileName, "fpt")) TO (tcDestinationFolder + FORCEEXT(lcFileName, "fpt"))
		ENDIF
		IF ADIR(laDummy, lcSourceFolder + FORCEEXT(lcFileName, "cdx")) = 1
			COPY FILE (lcSourceFolder + FORCEEXT(lcFileName, "cdx")) TO (tcDestinationFolder + FORCEEXT(lcFileName, "cdx"))
		ENDIF
	CATCH TO loException
		lnErrorNo = loException.ERRORNO
		lcErrorMessage = lcErrorMessage + lcCR + loException.MESSAGE
		lcCR = CHR(13) + CHR(10)
	ENDTRY
	IF lnErrorNo > 0
* 21
		lnLastFileNum = RECNO("crsSourceTables")
* 22
		EXIT
	ENDIF
ENDSCAN

*{23 Error occur while copy files
IF lnLastFileNum > 0
* 24
	SCAN FOR RECNO("crsSourceTables") <= lnLastFileNum
		TRY
			ERASE (tcDestinationFolder + FORCEEXT(ALLTRIM(crsSourceTables.TableName), "dbf"))
			ERASE (tcDestinationFolder + FORCEEXT(ALLTRIM(crsSourceTables.TableName), "fpt"))
			ERASE (tcDestinationFolder + FORCEEXT(ALLTRIM(crsSourceTables.TableName), "cdx"))
		CATCH
		ENDTRY
	ENDSCAN
	TRY
		IF llCreateFolder
			RD (tcDestinationFolder)
		ENDIF
	CATCH
	ENDTRY

	USE IN crsSourceTables
	SELECT(lnOldWorkArea)
* 25
	RETURN lcErrorMessage
ENDIF
*} Error occur while copy files

*{ Zap and reset Integer (Autoinc)
llZapTableSuccessful = .T.
llErrorZapTable = .T.
* 26
DO WHILE llZapTableSuccessful AND llErrorZapTable

	llZapTableSuccessful = .F.
	llErrorZapTable = .F.
* 27
	SELECT crsSourceTables
	SCAN FOR DeleteAllRows = .T.
		TRY
* 28
			SELECT 0
			USE (tcDestinationFolder + ALLTRIM(crsSourceTables.TableName)) EXCLUSIVE
			ZAP

			llZapTableSuccessful = .T.

			REPLACE DeleteAllRows WITH .F., ;
				ErrorMsg WITH "" IN crsSourceTables

* 29
			IF crsSourceTables.ResetIdentity
				lnFieldCount = AFIELDS(laDummy)
				lcAutoincFieldName = ""
				FOR lnCurrentField = 1 TO lnFieldCount
					IF laDummy[lnCurrentField, 17] <> 0 && Current field is Integer (Autoinc)
						lcAutoincFieldName = laDummy[lnCurrentField, 1]
						EXIT
					ENDIF
				ENDFOR
* 30
				IF !EMPTY(lcAutoincFieldName)
					ALTER TABLE (crsSourceTables.TableName) ALTER COLUMN &lcAutoincFieldName. INTEGER AUTOINC NEXTVALUE 1 STEP 1
				ENDIF
			ENDIF
		CATCH TO loException
			IF UPPER(loException.LINECONTENTS) = "ZAP"
				llErrorZapTable = .T.
				REPLACE ErrorMsg WITH loException.MESSAGE IN crsSourceTables
			ENDIF
		FINALLY
			USE
			SELECT crsSourceTables
		ENDTRY
	ENDSCAN
ENDDO
*{ 31 Close All Databases open after Zap Tables
lnDBCCount = ADATABASES(laDatabases)
FOR lnCurrentDBC = 1 TO lnDBCCount
	SET DATABASE TO laDatabases[lnCurrentDBC,1]
	CLOSE DATABASES
ENDFOR
*} Close All Databases open after Zap Tables
*} Zap and reset Integer (Autoinc)

*{ 32 Get all errors occur when zap tables
SELECT crsSourceTables
SCAN FOR !EMPTY(ErrorMsg)
	lcErrorMessage = lcErrorMessage + lcCR + ALLTRIM(crsSourceTables.ErrorMsg)
	lcCR = CHR(13) + CHR(10)
ENDSCAN
*} Get all errors occur when zap tables

USE IN crsSourceTables
SELECT(lnOldWorkArea)

RETURN lcErrorMessage
ENDFUNC

*-------------------------------------------------------
* Function....: GetODBCErrorText
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tnSourceDBConnHandle, tnDestinationDBConnHandle
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetODBCErrorText
LOCAL lcErrorMessage, laDummy[1] AS STRING
LOCAL lnRowsCount AS INTEGER

lcErrorMessage = ""

lnRowsCount = AERROR(laDummy)

IF lnRowsCount > 0
	IF INLIST(laDummy[1], 1427, 1429, ; && OLE Error if using ADO
		1526) && ODBC error
		lcErrorMessage = laDummy[3]
	ENDIF
ENDIF
IF EMPTY(lcErrorMessage)
	lcErrorMessage = MESSAGE()
ENDIF
RETURN lcErrorMessage
ENDFUNC

*-------------------------------------------------------
* Function....: CopySQLServerDB
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..: tnSourceDBConnHandle, tnDestinationDBConnHandle
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CopySQLServerDB
LPARAMETERS tvSourceDBConnHandle, tcDestinationDataBase, tlVFXTables

LOCAL llRes, llDeleteTableSuccessful, llErrorDeleteTable AS Logical
LOCAL lcSourceDataBase,  lcSQLCommand, lcDeviceName, laDatabases[1], lcDestinationDBPhysicalFName, ;
	lcSourceDBLogicalFName, lcSourceDBLogicalLDFFNam, lcDestinationDBPhysicalLDFFName, ;
	lcErrorMessage, lcCR, lcBuffer AS STRING

llUseADO = !(VARTYPE(tvSourceDBConnHandle) = "N")
lcErrorMessage = []
lcCR = ""

* 1 { Get name Of Source DataBase
lcSQLCommand = [sp_tables @table_type =  "'TABLE'"]
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "_crsSourceTables", .T.)
	lcErrorMessage = GetODBCErrorText()
	RETURN lcErrorMessage
ENDIF
CREATE CURSOR crsSourceTables (TableName c(200), DeleteAllRows L, ResetIdentity L, ErrorMsg M)
SELECT TABLE_Name AS TableName FROM _crsSourceTables INTO CURSOR __crsSourceTables
SELECT crsSourceTables
APPEND FROM DBF("__crsSourceTables")
LOCATE
lcSourceDataBase = ALLTRIM(_crsSourceTables.TABLE_QUALIFIER)
USE IN _crsSourceTables
USE IN __crsSourceTables
*} Get name Of Source DataBase

*{ Crete new Database
*2 - { Get name of all databases
lcSQLCommand = "sp_helpdb"
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "crsAllDatabase", .T.)
	lcErrorMessage = GetODBCErrorText()
	RETURN lcErrorMessage
ENDIF
SELECT crsAllDatabase
SELECT NAME FROM crsAllDatabase INTO ARRAY laDatabases
*} Get name of all databases

IF EMPTY(tcDestinationDataBase)
* 3
	DO FORM ("vfxgetdatabasename") WITH laDatabases, tlVFXTables TO lcRes && 4
	IF EMPTY(lcRes)
		USE IN crsAllDatabase
		RETURN [CANCEL]
	ENDIF
	tcDestinationDataBase = lcRes
ENDIF

* 5
*{V&U VM 2014-02-07 6375
DO FORM ("vfxtablezap") WITH .F. TO llRes
*}V&U VM 2014-02-07
IF !llRes
	USE IN crsAllDatabase
	RETURN [CANCEL]
ENDIF

* 6
SELECT crsAllDatabase
LOCATE FOR UPPER(ALLTRIM(NAME)) == UPPER(tcDestinationDataBase)
IF !FOUND()
* 7
	lcSQLCommand = "CREATE DATABASE [" + tcDestinationDataBase + "]"
	IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
		lcErrorMessage = GetODBCErrorText()
		RETURN lcErrorMessage
	ENDIF
ENDIF
USE IN crsAllDatabase
*} Crete new Database

* 8 { Create device
lcDeviceName = SYS(2015)
lcSQLCommand = [sp_addumpdevice 'disk', '] + lcDeviceName + [', '] + lcDeviceName + [.BAK' ]
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
	lcErrorMessage = GetODBCErrorText()
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF
*} Create device

*{ 9 Greate Backup
lcSQLCommand = "BACKUP DATABASE [" + lcSourceDataBase + "] to " + lcDeviceName
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
	lcErrorMessage = GetODBCErrorText()
	USE IN crsSourceTables
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF
*} Greate Backup

* 10 { Restore DataBase to New DB
lcSQLCommand = "use [" + lcSourceDataBase  + "]"
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
	lcErrorMessage = GetODBCErrorText()
	USE IN crsSourceTables
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF

* 11
lcSQLCommand = "sp_Helpdb @dbname=[" + tcDestinationDataBase + "]"
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "crsGetPhysicalFName", .T.)
	lcErrorMessage = GetODBCErrorText()
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF
SELECT crsGetPhysicalFName1
LOCATE FOR UPPER(ALLTRIM(FileGroup)) == "PRIMARY"
IF FOUND()
	lcDestinationDBPhysicalFName = ALLTRIM(crsGetPhysicalFName1.FileName)
ENDIF
LOCATE FOR ISNULL(FileGroup)
IF FOUND()
	lcDestinationDBPhysicalLDFFName = ALLTRIM(crsGetPhysicalFName1.FileName)
ENDIF

IF USED("crsGetPhysicalFName")
	USE IN crsGetPhysicalFName
ENDIF
IF USED("crsGetPhysicalFName1")
	USE IN crsGetPhysicalFName1
ENDIF

* 12
lcSQLCommand = "sp_Helpdb @dbname=[" + lcSourceDataBase + "]"
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "crsGetLogicalFName", .T.)
	lcErrorMessage = GetODBCErrorText()
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF
SELECT crsGetLogicalFName1
LOCATE FOR UPPER(ALLTRIM(FileGroup)) == "PRIMARY"
IF FOUND()
	lcSourceDBLogicalFName = ALLTRIM(crsGetLogicalFName1.NAME)
ENDIF
LOCATE FOR ISNULL(FileGroup)
IF FOUND()
	lcSourceDBLogicalLDFFName = ALLTRIM(crsGetLogicalFName1.NAME)
ENDIF
IF USED("crsGetLogicalFName")
	USE IN crsGetLogicalFName
ENDIF
IF USED("crsGetLogicalFName1")
	USE IN crsGetLogicalFName1
ENDIF

* 13
lcSQLCommand = "RESTORE FILELISTONLY FROM " + lcDeviceName + CHR(13) + CHR(10) + ;
	"RESTORE DATABASE [" + tcDestinationDataBase + "] FROM " + lcDeviceName + " WITH MOVE " + ;
	"'" + lcSourceDBLogicalFName + "' TO '"	 + lcDestinationDBPhysicalFName + "' " + ;
	", MOVE '" + lcSourceDBLogicalLDFFName + "' TO '" + lcDestinationDBPhysicalLDFFName + "', REPLACE"

IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
	lcErrorMessage = GetODBCErrorText()
	USE IN crsSourceTables
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF

* 14
lcSQLCommand = "use [" + tcDestinationDataBase + "]"
IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
	lcErrorMessage = GetODBCErrorText()
	USE IN crsSourceTables
	RoolbackNewDatabase(tvSourceDBConnHandle, tcDestinationDataBase, lcSourceDataBase)
	RETURN lcErrorMessage
ENDIF
* 15
lcSQLCommand = "sp_dropdevice @logicalname = [" + lcDeviceName +"], @delfile='DELFILE'"
llRes = ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
*} Restore DataBase to New DB

*{ Delete tables
llDeleteTableSuccessful = .T.
llErrorDeleteTable = .T.
* 16
DO WHILE llDeleteTableSuccessful AND llErrorDeleteTable
	llDeleteTableSuccessful = .F.
	llErrorDeleteTable = .F.
* 17
	SELECT crsSourceTables
	SCAN FOR DeleteAllRows
* 18
		lcSQLCommand = "DELETE FROM [" + ALLTRIM(TableName) + "]"
		IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
			lcBuffer = GetODBCErrorText()
			llErrorDeleteTable = .T.
		ELSE
			lcBuffer = ""
			llDeleteTableSuccessful = .T.
			REPLACE DeleteAllRows WITH .F. IN crsSourceTables
		ENDIF
		REPLACE ErrorMsg WITH lcBuffer IN crsSourceTables
* 19
		IF crsSourceTables.ResetIdentity
* 20
			lcSQLCommand = "DBCC CHECKIDENT ([" + ALLTRIM(crsSourceTables.TableName) + "], RESEED, 1)"
			IF !ExecuteCommand(tvSourceDBConnHandle, lcSQLCommand, "", .T.)
				lcErrorMessage = lcErrorMessage + lcCR + GetODBCErrorText()
				lcCR = CHR(13) + CHR(10)
			ENDIF
		ENDIF
	ENDSCAN
ENDDO
*} Delete tables

* 21 { Get Error Message
SELECT crsSourceTables
SCAN FOR !EMPTY(ErrorMsg)
	lcErrorMessage = lcErrorMessage + lcCR + ALLTRIM(crsSourceTables.ErrorMsg)
	lcCR = ""
ENDSCAN
*} Get Error Message

USE IN crsSourceTables
RETURN lcErrorMessage
ENDFUNC

*-------------------------------------------------------
* Function....: RoolbackNewDatabase
* Called by...:
*
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION RoolbackNewDatabase
LPARAMETERS tvSourceDBConnHandle AS Variant, tcDatabaseName AS STRING, tcBaseDB AS STRING

IF !EMPTY(tcDatabaseName)
	IF !EMPTY(tcBaseDB)
		ExecuteCommand(tvSourceDBConnHandle, "USE [" + tcBaseDB + "]", "", .T.)
	ENDIF
	ExecuteCommand(tvSourceDBConnHandle,"DROP DATABASE [" + tcDatabaseName + "]", "", .T.)
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: GetNewGUID
* Called by...:
*
* Abstract....:
*
* Returns.....: New GUID
*
* Parameters..:
*
* Notes.......: Uses CoCreateGuid() IN OLE32.DLL
*-------------------------------------------------------

FUNCTION GetNewGUID()
LOCAL lcResultGuid, lcGuid

lcResultGuid = ""
lcGuid = SPACE(16)

DECLARE LONG CoCreateGuid IN "OLE32.DLL" STRING@
IF CoCreateGuid(@lcGuid) = 0
	FOR i = 1 TO 16
		lcResultGuid = lcResultGuid + PADL(SUBSTR(TRANSFORM(ASC(SUBSTR(lcGuid, i, 1)), "@0"), 9), 2, "0")
		IF i = 4 OR i = 6 OR i = 8 OR i = 10
			lcResultGuid = lcResultGuid + "-"
		ENDIF
	ENDFOR
ELSE
	lcResultGuid = "0"
ENDIF
RETURN lcResultGuid
ENDFUNC

*-------------------------------------------------------
* Function....: AlreadyStarted
* Called by...: vfxmain.prg
*
* Abstract....: Checks if another instance of the application
*				is running
* Returns.....: .T. - another instance of application
*				is already running, .F. only one instance
* Parameters..: tcAppName (ApplicationName)
*
* Notes.......: If another instance of application is
*				already running bring it on top
*-------------------------------------------------------

FUNCTION alreadystarted()
LPARAMETERS tcAppName
LOCAL lcMsg, lcAppName, lnhWnd, llRetVal, nMutexHndl

DECLARE INTEGER CreateMutex IN WIN32API INTEGER lnAttributes, INTEGER lnOwner, STRING @lcAppName
DECLARE INTEGER GetProp IN WIN32API INTEGER lnhWnd, STRING @lcAppName
DECLARE INTEGER SetProp IN WIN32API INTEGER lnhWnd, STRING @lcAppName, INTEGER lnValue
DECLARE INTEGER CloseHandle IN WIN32API INTEGER lnMutexHandle
DECLARE INTEGER GetLastError IN WIN32API
DECLARE INTEGER GetWindow IN USER32 INTEGER lnhWnd, INTEGER lnRelationship
DECLARE INTEGER GetDesktopWindow IN WIN32API
DECLARE BringWindowToTop IN Win32APi INTEGER lnhWnd
DECLARE ShowWindow IN WIN32API INTEGER lnhWnd, INTEGER lnStyle

lcAppName = UPPER( ALLTRIM( tcAppName ) ) + CHR( 0 )
llRetVal = .F.

* Create a new MUTEX with the name of the application
nMutexHndl = CreateMutex( 0, 0, @lcAppName )

IF GetLastError() = 183
* Get hWnd of the first top level window on the Windows Desktop.
	lnhWnd = GetWindow( GetDesktopWindow(), 5 )

	DO WHILE lnhWnd > 0
		IF GetProp( lnhWnd, @lcAppName ) = 1
* Activate the app and stop current copy from starting
			IF TYPE("goProgram.class") ="C"
				 = goProgram.vfxmessagebox(IIF(goProgram.lRuntimeLocalization, ;
					goLocalize.cMSG_APPLICATIONISALREADYRUNNING, ;
					MSG_APPLICATIONISALREADYRUNNING), 0 + 64, .cMainTitle)
			ELSE
				MESSAGEBOX("Another instance of the application is already running", 0 + 64, _SCREEN.CAPTION)
			ENDIF
			BringWindowToTop( lnhWnd )
			ShowWindow( lnhWnd, 3 )
			llRetVal = .T.
			EXIT
		ENDIF
		lnhWnd = GetWindow( lnhWnd, 2  )
	ENDDO
ELSE
* Add a property to the FoxPro App for identification later
	SetProp( _VFP.HWND, @lcAppName, 1)
	IF TYPE("goProgram.class") ="C"
		goProgram.nMutexHandle = nMutexHndl
	ENDIF
	llRetVal = .F.
ENDIF

RETURN llRetVal
ENDFUNC

*-------------------------------------------------------
* Function....: GetDefaultFaxName
* Called by...: cFax, cMailMerge
*
* Abstract....: Checks the type of fax and gets the default file name
* Returns.....:
* Parameters..: lcFritzFax, lcWinFax, lcFaxName
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetDefaultFaxName
PARAMETERS lcFritzFax, lcWinFax, lcFaxName

LOCAL laPrinters[1], lnPrintersNum, lnCurrentPrinter
lnPrintersNum = APRINTERS(laPrinters, 1)

FOR lnCurrentPrinter = 1 TO lnPrintersNum
	lcCurrentPrinterName = LOWER(ALLTRIM(laPrinters[lnCurrentPrinter,1]))
	lcCurrentPrinterDriver = LOWER(ALLTRIM(laPrinters[lnCurrentPrinter,3]))
	DO CASE
		CASE LEFT(lcCurrentPrinterDriver, 9) == "fritz!fax" OR LEFT(lcCurrentPrinterDriver, 8) == "fritzfax" && Fritz!Fax
			lcFritzFax = lcCurrentPrinterName
			EXIT

		CASE LEFT(lcCurrentPrinterDriver, 6) == "winfax" && Winfax
			lcWinFax = lcCurrentPrinterName

		CASE "fax" $ lcCurrentPrinterName AND EMPTY(lcFaxName)
			lcFaxName = lcCurrentPrinterName
	ENDCASE
ENDFOR
ENDFUNC

*-------------------------------------------------------
* Function....: GetProducerAndAppName
* Called by...:
* Abstract....: Returns Producer and Application
*				Name as stored in goProgram object
* Returns.....:  0 - OK,
*				-1 - Producer or App name missing
* Parameters..: tcProducer, tcApplication (by Ref)
*
* Notes.......:
*-------------------------------------------------------

FUNCTION GetProducerAndAppName
PARAMETERS tcProducer, tcApplication
LOCAL lnResult AS INTEGER

lnResult = -1

IF TYPE("goProgram.cCompanyName") == "C" AND !EMPTY(goProgram.cCompanyName)
	tcProducer = ALLTRIM(UPPER(goProgram.cCompanyName))
ELSE
	tcProducer = ""
ENDIF

IF TYPE("goProgram.cAppName") == "C" AND !EMPTY(goProgram.cAppName)
	tcApplication = ALLTRIM(UPPER(goProgram.cAppName))
ELSE
	tcApplication = ""
ENDIF

IF !EMPTY(tcProducer) AND !EMPTY(tcApplication)
	lnResult = 0
ENDIF

RETURN lnResult
ENDFUNC

*-------------------------------------------------------
* Function....: CreateEmptyDBC
* Called by...:
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcDBCName, tcTargetFolder (by Ref)
*
* Notes.......: Creates an empty DBC using files generated by GenDBC
*-------------------------------------------------------
FUNCTION CreateEmptyDBC
LPARAMETERS tcDBCName AS STRING, tcTargetFolder AS STRING, tcRelativePathToKRTFile AS STRING, tcLogFileName AS STRING

LOCAL llRes AS Logical
LOCAL loError AS EXCEPTION
LOCAL lcGenDBCFunction, lcKRTDBFName, lcKRTFile, lcOldSetPath AS STRING
LOCAL lnErrorNo AS INTEGER, lcErrorMsg

lcErrorMsg = ""
IF EMPTY(tcDBCName) OR VARTYPE(tcDBCName) <> "C"
*{ V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("Database name is empty.", goProgram.cUpdateLogFileName)
	ENDIF
*} V&U MS 2008-12-19
	RETURN .F.
ENDIF
tcDBCName = FORCEEXT(tcDBCName, "dbc")
IF EMPTY(tcTargetFolder) OR VARTYPE(tcTargetFolder) <> "C"
	tcTargetFolder = ADDBS(SYS(2023)) + SYS(2015)
ENDIF
tcTargetFolder = ADDBS(tcTargetFolder)
IF !DIRECTORY(tcTargetFolder, 1)
	TRY
		MD (tcTargetFolder)
	CATCH TO loError
		lcErrorMsg = loError.MESSAGE
	ENDTRY
*{ V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist AND !EMPTY(lcErrorMsg)
		WriteLogInfo("*** Directory " + TRANSFORM(tcTargetFolder) + " cannot be created. Error: " + lcErrorMsg, ;
			goProgram.cUpdateLogFileName)
	ENDIF
*} V&U MS 2008-12-19
ENDIF
IF !DIRECTORY(tcTargetFolder, 1)
*{ V&U MS 2008-12-19
	IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist
		WriteLogInfo("Directory " + TRANSFORM(tcTargetFolder) + " is not found.", goProgram.cUpdateLogFileName)
	ENDIF
*} V&U MS 2008-12-19
	RETURN .F.
ENDIF

IF VERSION(2) = 2
	lcGenDBCFunction = 'VFX_' + JUSTSTEM(tcDBCName)
ELSE
	lcGenDBCFunction = SYS(2015)
ENDIF

lcKRTFile = ADDBS(SYS(2023)) + 'VFX_' + FORCEEXT(tcDBCName, "krt")
lcKRTDBFName = 'VFX_' + JUSTSTEM(tcDBCName) + "krt.dbf"
lcOldDir = GetDefaultFolder()
lnErrorNo = 0
lcOldSetPath = SET("Path")
TRY
	IF !FILE(lcKRTDBFName) AND VERSION(2) = 2
*{ V&U RI 2008-09-08
		IF EMPTY(tcRelativePathToKRTFile)
			IF FILE(ADDBS(lcOldDir) + "Data\" + lcKRTDBFName)
				lcKRTDBFName = ADDBS(lcOldDir) + "Data\" + lcKRTDBFName
			ENDIF
		ELSE
			IF FILE(ADDBS(ADDBS(lcOldDir) + tcRelativePathToKRTFile) + lcKRTDBFName)
				lcKRTDBFName = ADDBS(ADDBS(lcOldDir) + tcRelativePathToKRTFile) + lcKRTDBFName
			ENDIF
		ENDIF
*} V&U RI 2008-09-08
	ENDIF
	IF FILE(lcKRTDBFName)
		USE (lcKRTDBFName) AGAIN SHARED ALIAS _krtDbf
		SELECT _krtDbf
		LOCATE
		COPY MEMO _krtDbf.PROGRAM TO (lcKRTFile)
		USE IN _krtDbf
	ENDIF
	CD (tcTargetFolder)
	SET PATH TO (JUSTPATH(lcKRTFile)) ADDITIVE
	IF VERSION(2) = 2 AND !FILE(FORCEEXT(lcGenDBCFunction, "prg"))
*{ V&U RI 2008-09-08
		IF EMPTY(tcRelativePathToKRTFile)
			IF FILE(ADDBS(lcOldDir) + "Data\" + FORCEEXT(lcGenDBCFunction, "prg"))
				lcGenDBCFunction = ADDBS(lcOldDir) + "Data\" + FORCEEXT(lcGenDBCFunction, "prg")
			ENDIF
		ELSE
			IF FILE(ADDBS(ADDBS(lcOldDir) + tcRelativePathToKRTFile) + FORCEEXT(lcGenDBCFunction, "prg"))
				lcGenDBCFunction = ADDBS(ADDBS(lcOldDir) + tcRelativePathToKRTFile) + FORCEEXT(lcGenDBCFunction, "prg")
			ENDIF
		ENDIF
*} V&U RI 2008-09-08
	ELSE
		lcGenDBCFunction = ADDBS(SYS(2023)) + FORCEEXT(lcGenDBCFunction, "prg")
		lcOrigDBCFunc = FORCEEXT('VFX_' + JUSTFNAME(tcDBCName),"PRG")
		STRTOFILE(FILETOSTR(lcOrigDBCFunc), lcGenDBCFunction)
	ENDIF
	COMPILE (lcGenDBCFunction)
*{V&U MS 2009-02-25
	IF EMPTY(tcLogFileName)
		tcLogFileName = "CreateEmptyDbc.log"
	ENDIF
	DO (lcGenDBCFunction) WITH tcLogFileName
	IF TYPE("_GenDBC_Error") == "L" AND _GenDBC_Error
		llRes = .F.
	ELSE
		llRes = .T.
	ENDIF
*}V&U MS 2009-02-25
CATCH TO loError
	lcErrorMsg = loError.MESSAGE
FINALLY
	CD (lcOldDir)
ENDTRY
*{ V&U MS 2008-12-19
IF TYPE("goProgram.lUpdateLogFileNameExist") == "L" AND goProgram.lUpdateLogFileNameExist AND !EMPTY(lcErrorMsg)
	WriteLogInfo("*** Error in CreateEmptyDBC: " + lcErrorMsg, goProgram.cUpdateLogFileName)
ENDIF
*} V&U MS 2008-12-19

IF DBUSED(tcTargetFolder + tcDBCName)
	SET DATABASE TO (tcTargetFolder + tcDBCName)
	CLOSE DATABASES
ENDIF
TRY
	ERASE (lcKRTFile)
	IF VERSION(2) != 2
		lcGenDBCFunction = FORCEEXT(lcGenDBCFunction,"*")
		ERASE (lcGenDBCFunction)
	ENDIF
CATCH
ENDTRY

TRY
	SET PATH TO (lcOldSetPath)
CATCH
ENDTRY

RETURN llRes
ENDFUNC

*-------------------------------------------------------
* Function....: ReplaceDBCFiles
* Called by...:
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcDBCName, tcTargetFolder
*
* Notes.......: Replaces all three DBC files,based on new empty DBC
*-------------------------------------------------------
FUNCTION ReplaceDBCFiles
LPARAMETERS tcDBCName AS STRING, tcTargetFolder AS STRING

LOCAL lcTempFolder, lcDBCName, lcTargetFolder AS STRING
LOCAL llRes AS Logical
LOCAL loException AS EXCEPTION

IF EMPTY(tcDBCName) OR TYPE("tcDBCName") <> "C"
	RETURN .F.
ENDIF
IF !EMPTY(tcTargetFolder) AND TYPE("tcTargetFolder") = "C"
	lcTargetFolder = ADDBS(tcTargetFolder)
ELSE
	lcTargetFolder = ADDBS(JUSTPATH(tcDBCName))
ENDIF

lcDBCName = JUSTFNAME(tcDBCName)
lcTempFolder = ADDBS(ADDBS(SYS(2023)) + SYS(2015))

IF CreateEmptyDBC(lcDBCName, lcTempFolder)
	TRY
		COPY FILE (lcTempFolder + FORCEEXT(lcDBCName, "dbc")) TO (lcTargetFolder + FORCEEXT(lcDBCName, "dbc"))
		COPY FILE (lcTempFolder + FORCEEXT(lcDBCName, "dct")) TO (lcTargetFolder + FORCEEXT(lcDBCName, "dct"))
		COPY FILE (lcTempFolder + FORCEEXT(lcDBCName, "dcx")) TO (lcTargetFolder + FORCEEXT(lcDBCName, "dcx"))
		llRes = .T.
	CATCH TO loException

	ENDTRY
	DelDirectory(lcTempFolder)
ENDIF

RETURN llRes
ENDFUNC

*-------------------------------------------------------
* Function....: GetTableName
* Called by...:
* Abstract....: Returns a table name by given allias name
*
* Returns.....:	TableName
*
* Parameters..:	tcAliasName
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetTableName
LPARAMETERS tcAliasName

LOCAL lcTables, lnCommaPos, loCAObject, lnSourceType

lnSourceType = CURSORGETPROP("SOURCETYPE", tcAliasName)
DO CASE
	CASE lnSourceType = 1    && LOCAL VIEW
		lcTables = UPPER(CURSORGETPROP("TABLES", tcAliasName))
		lnCommaPos = AT(",", lcTables)
		*{V&U MS 2013-04-10
		IF lnCommaPos > 0
			lcTables = SUBSTR(lcTables, AT("!", lcTables) + 1, lnCommaPos - (AT("!", lcTables) + 1))
		ELSE
			lcTables = SUBSTR(lcTables, AT("!", lcTables) + 1)
		ENDIF
		*}V&U MS 2013-04-10
	CASE lnSourceType = 3	&& LOCAL TABLE
		lcTables = JUSTSTEM(DBF(tcAliasName))
	CASE lnSourceType > 100 OR lnSourceType = 4   && Cursor adapter
		loCAObject = GETCURSORADAPTER(tcAliasName)
		lcTables = loCAObject.TABLES
		lnCommaPos = AT(",", lcTables)
		IF lnCommaPos > 0
			lcTables = SUBSTR(lcTables, 1, lnCommaPos - 1)
		ENDIF
	OTHERWISE
		lcTables = UPPER(CURSORGETPROP("TABLES", tcAliasName))
		lnCommaPos = AT(",", lcTables)
		IF lnCommaPos > 0
			lcTables = SUBSTR(lcTables, 1, lnCommaPos - 1)
		ENDIF
ENDCASE

RETURN lcTables
ENDFUNC

*-------------------------------------------------------
* Function....: IsTerminalServer
* Called by...:
* Abstract....: Checks if current machine is terminal server
*
* Returns.....:	True/False
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------

FUNCTION IsTerminalServer
LOCAL lnRes

DECLARE INTEGER GetSystemMetrics IN WIN32API ;
	INTEGER nIndex
&& Return values:
&& non 0 - Terminal Services client session
&&     0 - Terminal Server console session
lnRes = 0
lnRes = GetSystemMetrics(4096) &&	4096 = 0x1000 = SM_REMOTESESSION

RETURN lnRes <> 0
ENDFUNC

*-------------------------------------------------------
* Function....: GetColorDepth
* Called by...:
* Abstract....: Returns the color depth of current display
*				settings
* Returns.....:	Power of 2, representing the color depth
*				(32 for 32-bit color)
* Parameters..:
*
* Notes.......: Returns as default 32
*-------------------------------------------------------

FUNCTION GetColorDepth
LOCAL lnRes

DECLARE INTEGER GetDC IN WIN32API INTEGER
DECLARE INTEGER GetDeviceCaps IN WIN32API INTEGER, INTEGER
lnRes = GetDeviceCaps(GetDC(_SCREEN.HWND), 12)

RETURN lnres
ENDFUNC

*-------------------------------------------------------
* Function....: UpdateConfigVFXStructure
* Called by...:
* Abstract....: Updates the structure of config.vfx
*				file
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------

FUNCTION UpdateConfigVFXStructure
LOCAL lcOldSelect, lcNewConfigVFXStructure
LOCAL lcXMLFileStr, lcXML
LOCAL lnerror, lnConfigVfxFormat, llReplaceAllActive, lcConfigName 

lnConfigVfxFormat = 0
lcOldSelect = SELECT()
TRY
	lcNewConfigVFXStructure = FILETOSTR("vfxconfigstructure.txt")
	&lcNewConfigVFXStructure
&& Creates cursor "_NewStructure" with the structure of the new config.vfx
CATCH TO loError
	lnerror = loError.ERRORNO
ENDTRY
IF USED("_NewStructure") AND TYPE("goProgram") == "O"
	IF goProgram.LoadConfig("_CurrentConfig")
		*{ V&U MS 2012-01-25 
		AFIELDS(laFields, "_CurrentConfig")
		IF ASCAN(laFields, "Active", 1, ALEN(laFields, 1), 1, 15) = 0
			llReplaceAllActive = .T.
		ENDIF 
		*} V&U MS 2012-01-25 
		TRY
			SELECT "_NewStructure"
			APPEND FROM DBF("_CurrentConfig")
			*{ V&U MS 2012-01-25 
			IF llReplaceAllActive 
				REPLACE Active WITH .T. ALL IN "_NewStructure"
			ENDIF 
			*} V&U MS 2012-01-25 
*{ JEI IG 2006.11.15 Change
			IF TYPE("goProgram.nConfigVfxFormat") == "N"
				lnConfigVfxFormat = goProgram.nConfigVfxFormat
			ENDIF
			*{HC MS 2016-05-05, Allow Config.vfx to be in ProgramData
			IF type("goProgram.cConfigPath") == "C"
				lcConfigName = addbs(goProgram.cConfigPath) + "Config.vfx"
			ELSE 
				lcConfigName = "Config.vfx"
			ENDIF 	
			CursorToConfigVFX(lnConfigVfxFormat, "_NewStructure", lcConfigName, goProgram.cConfigPassword)
			*}HC MS 2016-05-05
*} JEI IG 2006.11.15
			USE IN _NewStructure
		CATCH TO loError
			lnerror = loError.ERRORNO
		ENDTRY
	ENDIF
ENDIF
SELECT (lcOldSelect)
ENDFUNC

*-------------------------------------------------------
* Function....: vfx_transferdata
* Called by...: cDataBaseToolsvfxbase.DoIt();
*				vfx_doupdate()
* Abstract....: Transfers data between tables with the
*				same structure
* Returns.....:	.T. - Success, .F. - Errors occurred
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------

FUNCTION vfx_transferdata
LPARAMETERS tcOldTable, tcNewTable, tlUseAppend

LOCAL llSuccess, llUpdateError, llDeleted
LOCAL laAutoIncFields[1,4]
LOCAL lcDatabaseFileName, lcCurrentFieldName
LOCAL lnFieldCount, lnCurrentField, lnAutoIncFieldsCount
LOCAL lnAutoIncNewFieldsCount, lnLastAutoIncValue, lnSetDel, lnNewFieldPos
llSuccess = .T.

lnSetDel = SET("Deleted")
SET DELETED OFF

SELECT 0
USE (tcNewTable) EXCLUSIVE ALIAS _NewTable
lcDatabaseFileName = CURSORGETPROP("Database")
IF !EMPTY(lcDatabaseFileName)
	SET DATABASE TO (lcDatabaseFileName)
	DELETE TRIGGER ON (tcNewTable) FOR DELETE
	DELETE TRIGGER ON (tcNewTable) FOR INSERT
	DELETE TRIGGER ON (tcNewTable) FOR UPDATE
	IF !EMPTY(DBGETPROP(JUSTSTEM(tcNewTable),"TABLE","RuleExpression"))
		ALTER TABLE (tcNewTable) DROP CHECK
	ENDIF
	lnFieldCount = AFIELDS(laNewFieldList, "_NewTable")
	FOR lnCurrentField = 1 TO lnFieldCount
		IF !EMPTY(laNewFieldList[lnCurrentField, 7]) && Field validation expression
			ALTER TABLE (tcNewTable) ALTER COLUMN (laNewFieldList[lnCurrentField, 1]) DROP CHECK
		ENDIF
		IF !EMPTY(laNewFieldList[lnCurrentField, 9]) && Field default value
			ALTER TABLE (tcNewTable) ALTER COLUMN (laNewFieldList[lnCurrentField, 1]) DROP DEFAULT
		ENDIF
	ENDFOR
ENDIF
CURSORSETPROP("Buffering", 3, "_NewTable")
* Check for Integer Autoinc
lnAutoIncFieldsCount = 0
lnAutoIncNewFieldsCount = 0
TRY
	USE (tcOldTable) IN 0 ALIAS _OldTable
	lnFieldCount = AFIELDS(laFieldList, "_OldTable")
	lnNewFieldCount = AFIELDS(laNewFieldList, "_NewTable")
	lcCurrentFieldName = ""
	FOR lnCurrentField = 1 TO lnFieldCount
		IF laFieldList[lnCurrentField, 2] == "I"
			lcCurrentFieldName = laFieldList[lnCurrentField,1]
*{V&U MS 2009-02-12 Fields in two arrays may be in different positions
			lnNewFieldPos = ASCAN(laNewFieldList, lcCurrentFieldName, 1, lnNewFieldCount, 1, 15)
			IF lnNewFieldPos > 0		&& field is present in both old & new table
				DO CASE
					CASE laFieldList[lnCurrentField, 18] = ;
							laNewFieldList[lnNewFieldPos, 18]
&& no change
						IF laFieldList[lnCurrentField, 18] <> 0
							lnAutoIncFieldsCount = lnAutoIncFieldsCount + 1
							DIMENSION laAutoIncFields[lnAutoIncFieldsCount,4]
							ALTER TABLE (tcNewTable) ALTER COLUMN (laFieldList[lnCurrentField, 1]) INTEGER
							laAutoIncFields[lnAutoIncFieldsCount,1] = "ALTER TABLE [" + tcNewTable + "] " + ;
								"ALTER COLUMN " + laFieldList[lnCurrentField, 1] + " Integer "
							laAutoIncFields[lnAutoIncFieldsCount,2] = NVL(laFieldList[lnCurrentField, 17], 0)	&& next value
							laAutoIncFields[lnAutoIncFieldsCount,3] = NVL(laFieldList[lnCurrentField, 18], 0)	&& step
							laAutoIncFields[lnAutoIncFieldsCount,4] = lnCurrentField							&& field pos in table		&& JEI IG 2006.10.13 add
						ENDIF
					CASE laFieldList[lnCurrentField, 18] = 0 AND ;
							laNewFieldList[lnNewFieldPos, 18] <> 0
&& field in old table is Int, in new table is Int(AutoInc)
						lnAutoIncFieldsCount = lnAutoIncFieldsCount + 1
						DIMENSION laAutoIncFields[lnAutoIncFieldsCount,4]
						ALTER TABLE (tcNewTable) ALTER COLUMN (laFieldList[lnCurrentField, 1]) INTEGER
						laAutoIncFields[lnAutoIncFieldsCount,1] = "ALTER TABLE [" + tcNewTable + "] " + ;
							"ALTER COLUMN " + laFieldList[lnCurrentField, 1] + " Integer "
						laAutoIncFields[lnAutoIncFieldsCount,2] = NVL(laNewFieldList[lnNewFieldPos, 17], 0)	&& next value
						laAutoIncFields[lnAutoIncFieldsCount,3] = NVL(laNewFieldList[lnNewFieldPos, 18], 0)	&& step
						laAutoIncFields[lnAutoIncFieldsCount,4] = lnCurrentField							&& field pos in table		&& JEI IG 2006.10.13 add
					CASE laFieldList[lnCurrentField, 18] <> 0 AND ;
							laNewFieldList[lnNewFieldPos, 18] = 0
&& field in old table is Int(AutoInc), in new table is Int
&& Do nothing

					OTHERWISE
&& both fields AutoInc, different increment step - use increment step from new table
						lnAutoIncFieldsCount = lnAutoIncFieldsCount + 1
						DIMENSION laAutoIncFields[lnAutoIncFieldsCount,4]
						ALTER TABLE (tcNewTable) ALTER COLUMN (laFieldList[lnCurrentField, 1]) INTEGER
						laAutoIncFields[lnAutoIncFieldsCount,1] = "ALTER TABLE [" + tcNewTable + "] " + ;
							"ALTER COLUMN " + laFieldList[lnCurrentField, 1] + " Integer "
						laAutoIncFields[lnAutoIncFieldsCount,2] = NVL(laFieldList[lnCurrentField, 17], 0)	&& next value
						laAutoIncFields[lnAutoIncFieldsCount,3] = NVL(laNewFieldList[lnNewFieldPos, 18], 0)	&& step
						laAutoIncFields[lnAutoIncFieldsCount,4] = lnCurrentField							&& field pos in table		&& JEI IG 2006.10.13 add
				ENDCASE
			ENDIF
*}V&U MS 2009-02-12
		ENDIF
	ENDFOR

CATCH TO loError
	lnAutoIncFieldsCount  = 0
FINALLY
	IF USED("_OldTable")
		USE IN _OldTable
	ENDIF
ENDTRY

IF tlUseAppend
&& append data at once
&& used by database update
	SELECT _NewTable
	APPEND FROM (tcOldTable)
ELSE
&& append data row-by-row
&& used by database repair
	lnErrorNo = 0
	TRY
		USE (tcOldTable) AGAIN ALIAS _OldTable IN 0 SHARED
	CATCH TO loError
		llSuccess = .F.
		lnErrorNo = loError.ERRORNO
	ENDTRY

	IF USED("_OldTable")
		SELECT _OldTable
		SCAN
			llUpdateError = .F.
			loRowContent = .NULL.
			lnErrorNo = 0
*{V&U MS 2009-10-01
			llDeleted = DELETED()
*}V&U MS 2009-10-01
			TRY
				SCATTER MEMO NAME loRowContent
			CATCH TO loError
				lnErrorNo = loError.ERRORNO
				loRowContent = .NULL.
			ENDTRY
			IF lnErrorNo > 0
				TRY
					SCATTER NAME loRowContent
				CATCH TO loError
					lnErrorNo = loError.ERRORNO
					loRowContent = .NULL.
				ENDTRY
			ENDIF
			IF !ISNULL(loRowContent)
				TRY
					SELECT _NewTable
					APPEND BLANK
					GATHER NAME loRowContent MEMO
*{V&U MS 2009-10-01
					IF llDeleted
						DELETE
					ENDIF
*}V&U MS 2009-10-01
*{ JEI MS 13.07.2006 Flag llUpdateError added, because when TableUpdate returns .F., the Catch is not raised
					IF !TABLEUPDATE(.F.)
						llUpdateError = .T.
					ENDIF
				CATCH
					llUpdateError = .T.
				ENDTRY
				IF llUpdateError
					 = TABLEREVERT(.F.)
				ENDIF
*} JEI MS 13.07.2006
			ENDIF
		ENDSCAN
	ENDIF
ENDIF && tlUseAppend
IF !USED("_OldTable")
	USE (tcOldTable) IN 0 ALIAS _OldTable
ENDIF
IF lnAutoIncFieldsCount > 0
	FOR lnCurrentField = 1 TO lnAutoIncFieldsCount
		lnLastAutoIncValue = 0
		SELECT _NewTable
		CALCULATE MAX(&laFieldList[laAutoIncFields[lnCurrentField,4], 1].) TO lnLastAutoIncValue			&& JEI IG 2006.10.13 change
		lnLastAutoIncValue = NVL(lnLastAutoIncValue, 0) + laAutoIncFields[lnCurrentField,3]
		lnLastAutoIncValue = MAX(lnLastAutoIncValue, laAutoIncFields[lnCurrentField,2])
		lcBuffer = laAutoIncFields[lnCurrentField,1]
		IF laAutoIncFields[lnCurrentField,2] <> 0
			lcBuffer = lcBuffer + " AUTOINC NEXTVALUE " + ;
				TRANSFORM(lnLastAutoIncValue)
			lcBuffer = lcBuffer + " STEP " + ;
				TRANSFORM(laAutoIncFields[lnCurrentField,3])
		ENDIF
		TRY
			&lcBuffer
		CATCH
			llSuccess = .F.
		ENDTRY
	ENDFOR
ENDIF

USE IN _NewTable
IF USED("_OldTable")
	USE IN _OldTable
ENDIF
SET DELETED &lnSetDel
RETURN llSuccess
ENDFUNC

*-------------------------------------------------------
* Function....: RunBackdoorProgram
* Called by...: goProgram.OnPreStart()
* Abstract....: Runs a given program on start of application
*				After success the file is renamed
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------

FUNCTION RunBackdoorProgram
LOCAL lcProgramName, lcNewProgramName, lcDisplayText
LOCAL llExecuteOK, ldToday

lcProgramName = ""
lcDisplayText = ""
llExecuteOK = .T.

IF TYPE("goProgram.cBackdoorProgramName") == "C"
	lcProgramName = JUSTFNAME(ALLTRIM(goProgram.cBackdoorProgramName))
	IF ADIR(laDummy, FORCEEXT(lcProgramName,"*")) = 0
		lcProgramName = ""
	ENDIF
ELSE
	RETURN .F.
ENDIF

IF goProgram.lRunBackdoorProgram AND !EMPTY(lcProgramName)
* Run backdoor program
	lcDisplayMsg = IIF(goProgram.lRunTimeLocalization, ;
		goLocalize.cMSG_RUNNINGBACKDOORPROGRAM, ;
		MSG_RUNNINGBACKDOORPROGRAM)
	DO CASE
		CASE ADIR(laDummy, FORCEEXT(lcProgramName,"app")) = 1
			goProgram.vfxwaitwindow(lcDisplayMsg, .F., .F., .T., .F., .T.)
			lcProgramName = FORCEEXT(lcProgramName,"app")
			TRY
				DO (lcProgramName)
			CATCH
				llExecuteOK = .F.
			ENDTRY
		CASE ADIR(laDummy, FORCEEXT(lcProgramName,"prg")) = 1
			goProgram.vfxwaitwindow(lcDisplayMsg, .F., .F., .T., .F., .T.)
			lcProgramName = FORCEEXT(lcProgramName,"prg")
			TRY
				DO (lcProgramName)
			CATCH
				llExecuteOK = .F.
			ENDTRY
		CASE ADIR(laDummy, FORCEEXT(lcProgramName,"fxp")) = 1
			goProgram.vfxwaitwindow(lcDisplayMsg, .F., .F., .T., .F., .T.)
			lcProgramName = FORCEEXT(lcProgramName,"fxp")
			TRY
				DO (lcProgramName)
			CATCH
				llExecuteOK = .F.
			ENDTRY
		OTHERWISE
			llExecuteOK = .F.
	ENDCASE

	IF llExecuteOK
		TRY
			ldToday = DATE()
			lcNewProgramName = JUSTSTEM(lcProgramName) + "_" + ;
				TRANSFORM(YEAR(ldToday)) + ;
				PADL(TRANSFORM(MONTH(ldToday)), 2,"0") + ;
				PADL(TRANSFORM(DAY(ldToday)), 2,"0")
			lcNewProgramName = FORCEEXT(lcNewProgramName, JUSTEXT(lcProgramName))
			RENAME (lcProgramName) TO (lcNewProgramName)

			IF ADIR(laDummy, FORCEEXT(lcProgramName, "prg")) > 0
				DELETE FILE (FORCEEXT(lcProgramName, "prg"))
			ENDIF
			IF ADIR(laDummy, FORCEEXT(lcProgramName, "fxp")) > 0
				DELETE FILE (FORCEEXT(lcProgramName, "fxp"))
			ENDIF
		CATCH
		ENDTRY
	ENDIF
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ENDIF
RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: vfx_checkfordbupdate
* Called by...: vfx_checkupdate
* Abstract....: Checks whether a client database update
*				is needed
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfx_checkfordbupdate(tcfrom, tcto)

LOCAL llNeedUpdate, lnRes, laFile[1], lcdbc, lnhowmany, ladbcalttables[1], ladbcnewtables[1], lcDisplayMsg
llNeedUpdate = .F.

*{V&U MS 2010-03-25
lcDisplayMsg = 	IIF(goProgram.lRunTimeLocalization, ;
				goLocalize.cMSG_CHECKINGTABLESTRUCTURE, ;
				MSG_CHECKINGTABLESTRUCTURE)
goProgram.vfxwaitwindow(lcDisplayMsg, .F., .F., .T., .F., .T.)
*}V&U MS 2010-03-25

lcdbc  = LOWER(goProgram.cmaindatabase)
IF !(".dbc" $ lcdbc)
	lcdbc = lcdbc + ".dbc"
ENDIF

* Get tables of existing DBC
OPEN DATAB (tcto + lcdbc)
IF !EMPTY(DBC())
	 = ADBOBJECTS(ladbcalttables, "table")
ENDIF
CLOSE DATABASES

* Get tables of new DBC
OPEN DATAB (tcfrom + lcdbc)
IF !EMPTY(DBC())
	 = ADBOBJECTS(ladbcnewtables, "table")
ENDIF
CLOSE DATABASES

* Compare all tables
lnhowmany = ADIR(laFile, tcfrom +"\*.dbf", "A")
FOR j = 1 TO lnHowMany
* JEI VM 01.06.07(
	goProgram.vfxwaitwindow(lcDisplayMsg + " " + laFile[j,1], .F., .F., .T., .F., .T.)
* JEI VM 01.06.07)
	lRes = vfx_needupdate(laFile[j,1], tcfrom, tcto, @ladbcalttables, @ladbcnewtables)
	IF lRes <> 0
		llNeedUpdate = .T.
		EXIT &&for
	ENDIF
ENDFOR
goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
RETURN llNeedUpdate

ENDFUNC

*-------------------------------------------------------
* Function....: AddAlias
* Called by...:
* Abstract....:
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION AddAlias
LPARAMETERS lcExpression, lcAlias

LOCAL lcResult, lnLength, lnCurrentPosition, lcCurrentChar, lcCurrentWord, lcNewWord, lvValue

lcExpression = CHRTRAN(lcExpression, " ", "")
lcAlias = CHRTRAN(lcAlias, " ", "") + "."
lnLength = LEN(lcExpression)
lcResult = ""
lnCurrentPosition = 1
lcCurrentChar = SUBSTR(lcExpression, lnCurrentPosition, 1)
DO WHILE lnCurrentPosition <= lnLength
	lcCurrentWord = ""
	DO WHILE !INLIST(lcCurrentChar, "*", "\", "+", "-", ",", "(", ")")
		lcCurrentWord = lcCurrentWord + lcCurrentChar
		lnCurrentPosition = lnCurrentPosition + 1
		IF lnCurrentPosition <= lnLength
			lcCurrentChar = SUBSTR(lcExpression, lnCurrentPosition, 1)
		ELSE
			EXIT
		ENDIF
	ENDDO
	IF lcCurrentChar <> "(" THEN
		lcNewWord = lcAlias + lcCurrentWord
		TRY
			lvValue = EVALUATE(lcNewWord)
			lcCurrentWord = lcNewWord
		CATCH
*			do nothing - lcCurrentWord should not be prefixed
		ENDTRY
	ENDIF
	lcResult = lcResult + lcCurrentWord
	DO WHILE !ISALPHA(lcCurrentChar)
		lcResult = lcResult + lcCurrentChar
		lnCurrentPosition = lnCurrentPosition + 1
		IF lnCurrentPosition <= lnLength
			lcCurrentChar = SUBSTR(lcExpression, lnCurrentPosition, 1)
		ELSE
			EXIT
		ENDIF
	ENDDO
ENDDO

RETURN lcResult

*-------------------------------------------------------
* Function....: GetFolderPath
* Called by...:
* Abstract....: Takes the CSIDL of a folder and returns the path
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetFolderPath
LPARAMETERS vFolder, lcPathString
LOCAL lcBuffer, hwndOwner, nFolder, hToken, dwFlags, lcPathString, lpBuffer, lpString

*{ V&U MS 2008-10-15
vFolder = GetFolderPathParameter(vFolder)
*} V&U MS 2008-10-15
lcBuffer = SPACE(1024)
*{ V&U RI 2008-09-25 SHGetFolderPath function is deprecated in Windows Vista and later
IF VAL(OS(3)) < 6 OR TYPE("vFolder") == "N"
	*{ HC BB 2016-02-29, 6924
	DECLARE INTEGER SHGetFolderPath IN  Shfolder.DLL ;
		LONG    hwndOwner, ;
		INTEGER nFolder, ;
		LONG    hToken, ;
		LONG	dwFlags, ;
		STRING 	@lcBuffer
	*} HC BB 2015-02-29
	
	lnRes = SHGetFolderPath(0, vFolder, 0, 0, @lcBuffer)
ELSE
	DECLARE INTEGER SHGetKnownFolderPath IN  shell32.DLL ;
		STRING 	rfid, ;
		LONG	dwFlags, ;
		INTEGER hToken, ;
		INTEGER	@lpBuffer

	DECLARE RtlMoveMemory IN kernel32.DLL AS CopyMemory ;
		STRING @lpDest, ;
		INTEGER lpSource, ;
		INTEGER cbCopy

	DECLARE CoTaskMemFree IN ole32.DLL ;
		INTEGER lpMem

	DECLARE INTEGER lstrlenW IN kernel32.DLL AS lstrlen ;
		INTEGER lpString

	lpBuffer = 0
	lnRes = SHGetKnownFolderPath(vFolder, 0, 0, @lpBuffer)
	IF lnRes = 0
		lpString = SPACE(lstrlen(lpBuffer) * 2)
		CopyMemory(@lpString, lpBuffer, LEN(lpString))
		CoTaskMemFree(lpBuffer)
		lcBuffer = STRCONV(lpString, 6)
	ENDIF

	CLEAR DLLS "SHGetKnownFolderPath", "CopyMemory", "CoTaskMemFree", "lstrlenW"
ENDIF
*} V&U RI 2008-09-25
IF lnRes = 0
*{ V&U RI 2008-09-25
	IF AT(CHR(0), lcBuffer) > 0
		lcPathString = LEFT(lcBuffer, AT(CHR(0), lcBuffer) -1)
	ELSE
		lcPathString = ALLTRIM(lcBuffer)
	ENDIF
*} V&U RI 2008-09-25
ELSE
	lcPathString = ""
ENDIF

RETURN lnRes
ENDFUNC

*-------------------------------------------------------
* Function....: GetClipboardFormat
* Called by...:
*
* Abstract....: Returns the ID of clipboard format
* Returns.....:
*
* Parameters..: FormatName
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetClipboardFormat

LPARAMETERS tcFormatName
LOCAL lnFormat, lcFormatName, lnLength

DECLARE INTEGER GetClipboardFormatName IN WIN32API INTEGER nFormat, STRING @lpszFormatName, INTEGER cchMaxCount

FOR lnFormat = 0xC000 TO 0xFFFF
	LOCAL lcFormatName
	LOCAL lnLength
	lcFormatName = SPACE(LEN(tcFormatName) + 1)
	lnLength = GetClipboardFormatName(lnFormat, @lcFormatName, LEN(lcFormatName))
	IF lnLength > 0
		IF LEFT(UPPER(lcFormatName), lnLength) = ALLTRIM(UPPER(tcFormatName))
			RETURN lnFormat
		ENDIF
	ENDIF
ENDFOR

RETURN 0

ENDFUNC

*-------------------------------------------------------
* Function....: vfx_updateappversion
* Called by...: vfx_checkupdate
*
* Abstract....: Updates AppVersion in VFXSYS table in a
*				specific location
* Returns.....:
*
* Parameters..: Path, VersionNumber string
*
* Notes.......:
*-------------------------------------------------------

FUNCTION vfx_updateappversion(tvPath, tcVersionNO)
LOCAL lcVFXSYSPath, lcOldMultiLocksIII
LOCAL lnOldWorkAreaIII
LOCAL lcConnectString, lcCommStr, lnPlatform, lcLeftBracket, lcRightBracket

IF !(TYPE("tcVersionNO") == "C") OR !(TYPE("tvPath") $"CN")
	RETURN .F.
ENDIF

lnOldWorkAreaIII = SELECT()
llError = .F.

IF EMPTY(tcVersionNO)
	tcVersionNO = "0.0.0"
ENDIF
IF TYPE("caVfxSys") # "U"
	RELEASE caVfxSys
ENDIF
IF USED("vfxsys")
	USE IN "vfxsys"
ENDIF

lcOldMultiLocksIII = SET("Multilocks")
SET MULTILOCKS ON

DO CASE
	CASE TYPE("tvPath") == "C"		&& local path to DBF
		TRY
			lcVFXSYSPath = ADDBS(tvPath) + "vfxsys.dbf"
			IF FILE(lcVFXSYSPath)
				USE (lcVFXSYSPath) IN 0 ALIAS vfxsys AGAIN SHARED
			ENDIF
			IF USED("vfxsys") AND TYPE("VFXSys.AppVersion") == "C" AND ;
					VERSION(2) = 0		&& update AppVersion only when run in EXE
				SELECT vfxsys
				IF RECCOUNT("vfxsys") = 0
					APPEND BLANK
				ENDIF
				REPLACE AppVersion WITH tcVersionNO IN vfxsys
			ENDIF
		CATCH
			llError = .T.
		ENDTRY
	CASE TYPE("tvPath") == "N"		&& connection handle to remote server storing vfxsys
		TRY
			lnResult = SQLEXEC(tvPath,"Select * from VFXSYS","vfxsys")
			IF lnResult > 0
				IF USED("vfxsys") AND TYPE("VFXSys.AppVersion") == "C" AND ;
						VERSION(2) = 0		&& update AppVersion only when run in EXE
					lnPlatform = 0
					lnPlatform = GetDataSourcePlatform(tvPath)
					DO CASE
						CASE lnPlatform = 2		&& SQL Server
							lcLeftBracket = '['
							lcRightBracket = ']'
						CASE lnPlatform = 4		&& IBM DB2
							lcLeftBracket = '"'
							lcRightBracket = '"'
						OTHERWISE
							lcLeftBracket = ''
							lcRightBracket = ''
					ENDCASE
					lcConnectString = SQLGETPROP(tvPath,"ConnectString")
					SELECT vfxsys
					IF RECCOUNT("vfxsys") = 0
						lcCommStr = "insert into " + lcLeftBracket +"VFXSYS" + lcRightBracket +" (" + lcLeftBracket + "AppVersion" + lcRightBracket + ;
							") values ('" + ALLTRIM(TRANSFORM(tcVersionNO)) +"')"
						lnResult = ExecuteCommand(tvPath, lcCommStr)
						llError = !lnResult
					ELSE
						lcCommStr = "update " + lcLeftBracket +"vfxsys" + lcRightBracket + ;
							" set " + lcLeftBracket +"AppVersion" + lcRightBracket +"='" + ALLTRIM(TRANSFORM(tcVersionNO)) +"'"
						lnResult = ExecuteCommand(tvPath, lcCommStr)
						llError = !lnResult
					ENDIF
				ENDIF
			ELSE
				llError = .T.
			ENDIF
		CATCH
			llError = .T.
		ENDTRY
	OTHERWISE
		llError = .T.
ENDCASE

IF llError OR !USED("vfxsys")
	 = goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_ANERROROCCURREDWHENUPDATINGDATABASE, MSG_ANERROROCCURREDWHENUPDATINGDATABASE), ;
		0 + 16, _SCREEN.CAPTION)
	IF USED("vfxsys")
		USE IN "vfxsys"
	ENDIF
	SELECT(lnOldWorkAreaIII)
	RETURN .F.
ENDIF

IF USED("vfxsys")
	USE IN "vfxsys"
ENDIF
SELECT(lnOldWorkAreaIII)
SET MULTILOCKS &lcOldMultiLocksIII
RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: LoadVFXFOpen
* Called by...: cOpenFormVfxBase.LoadItemGroups(),
*				cXPopenCombo.EnableCombo()
* Abstract....: Gets all the forms from vfxfopen tables
*				allowed to be viewed by the current user
* Returns.....:
*
* Parameters..: nCalledFrom
*				0 - cOpenFormVfxBase.LoadItemGroups()
*				1 - cXPopenCombo.EnableCombo()
* Notes.......:
*-------------------------------------------------------

FUNCTION LoadVFXFOpen
LPARAMETERS nCalledFrom
LOCAL lnCalledFrom
LOCAL laTemp[1]
LOCAL loCAVFXFOPEN

IF TYPE("nCalledFrom") == "N"
	lnCalledFrom = nCalledFrom
ELSE
	lnCalledFrom = 0
ENDIF

IF TYPE("goprogram.class") ="C"
	IF goProgram.nvfxSysTableLoc >= 1
		loCAVFXFOPEN = OPENTABLE("vfxFOpen","IDVFXFOPEN", .F.,"INDEX ON UPPER(objectid)+STR(objectno) TAG OBJECT ADDITIVE", ;
			.T., ,"vfxFOpen", "cAppVFXDataAccess")
	ELSE
		USE vfxfopen SHARED AGAIN IN 0
	ENDIF
ELSE
	USE vfxfopen SHARED AGAIN IN 0
ENDIF

IF USED("vfxfopen")
	SELECT vfxfopen

	AFIELDS(laTemp)

	IF ASCAN(laTemp,"tbrcbosort", 1, ALEN(laTemp, 1), 1, 15) = 0 AND ;
			lnCalledFrom = 1
		RETURN .F.
	ENDIF

	IF USED("openform")
		USE IN openform
	ENDIF

	IF TYPE("goUserRights") = "O"
		IF lnCalledFrom = 1
			SELECT vfxfopen. * , SPACE(100) AS runcommand, 00 AS ListNumber, 00 AS ItemNumber FROM vfxfopen ;
				WHERE NVL(vfxfopen.tbrCboSort, 0) > 0 ;
				ORDER BY tbrCboSort INTO CURSOR openform READWRITE
		ELSE
			SELECT vfxfopen. * , SPACE(100) AS runcommand, 00 AS ListNumber, 00 AS ItemNumber FROM vfxfopen ;
				ORDER BY objectid, objectno INTO CURSOR openform READWRITE
		ENDIF
		SCAN
			LOCAL cFormName, cFormObjName
*{JEI MS 28.09.2006
			IF EMPTY(openform.FORM)
				cFormName = TRIM(objectid) + PADL(objectno, 3,"0")
			ELSE
				cFormName = ALLTRIM(UPPER(openform.FORM))
			ENDIF
*}JEI MS 28.09.2006
			cFormObjName = "goUserRights." + cFormName
			IF TYPE(cFormObjName) = "O"
				IF !(&cFormObjName..ViewPermit)
					DELETE
				ENDIF
			ENDIF
		ENDSCAN
	ELSE
		IF lnCalledFrom = 1
			SELECT vfxfopen. * , SPACE(100) AS runcommand, 00 AS ListNumber, 00 AS ItemNumber FROM vfxfopen ;
				WHERE (vfxfopen.viewlevel >= GoUser.userlevel OR vfxfopen.viewlevel = 0) AND NVL(vfxfopen.tbrCboSort, 0) > 0 ;
				ORDER BY tbrCboSort INTO CURSOR openform READWRITE
		ELSE
			SELECT vfxfopen. * , SPACE(100) AS runcommand, 00 AS ListNumber, 00 AS ItemNumber FROM vfxfopen ;
				WHERE vfxfopen.viewlevel >= GoUser.userlevel OR vfxfopen.viewlevel = 0 ;
				ORDER BY objectid, objectno INTO CURSOR openform READWRITE
		ENDIF
	ENDIF
	USE IN vfxfopen

	REPLACE runcommand WITH "goprogram.runform('" + ALLTRIM(openform.FORM) +"')" FOR EMPTY(openform.PARAMETER) IN openform

	REPLACE runcommand WITH "goprogram.runform('" + ALLTRIM(openform.FORM) +"','" + ALLTRIM(openform.PARAMETER) +"')" FOR !EMPTY(openform.PARAMETER) IN openform

	REPLACE runcommand WITH ALLTRIM(openform.PARAMETER) FOR EMPTY(openform.FORM) AND !EMPTY(openform.PARAMETER) IN openform

	REPLACE groupcap WITH openform.objectid FOR EMPTY(openform.groupcap) IN openform

	SELECT openform
ELSE
	RETURN .F.
ENDIF

RETURN .T.
ENDFUNC

*-------------------------------------------------------
* Function....: ReadConfigVFXtoCursor
* Called by...: goProgram.LoadConfig,
*				cClientDataAccessVfxBase.Load()
* Abstract....: Reads the passed string (XML or CSV) and
*				fills a cursor with it's contents
* Returns.....:
*
* Parameters..: tcFileContents, tcCursorName
*
* Notes.......:
*-------------------------------------------------------

FUNCTION ReadConfigVFXtoCursor
LPARAMETERS tcFileContents, tcCursorName

LOCAL lnRes, llXML

lnRes = 0
llXML = (AT("<?xml version", MLINE(tcFileContents, 1)) > 0)

IF llXML
	TRY
		loXML = CREATEOBJECT("XMLAdapter")
		loXML.LOADXML(tcFileContents, .F.)
		loXML.TABLES.ITEM(1).TOCURSOR(.F., tcCursorName)

	CATCH TO oError
		lnRes = oError.ERRORNO

	ENDTRY
ELSE
	lnRes = CSVStringToCursor(tcFileContents, tcCursorName)
ENDIF

RETURN lnRes
ENDFUNC

*-------------------------------------------------------
* Function....: CSVStringToCursor
* Called by...: ReadConfigVFXtoCursor
*
* Abstract....: Converts a string in CSV format with additional structure info line
*				into a cursor
* Returns.....:
*
* Parameters..: tcDataString, tcCursorName
*
* Notes.......:
*-------------------------------------------------------

FUNCTION CSVStringToCursor
LPARAMETERS tcDataString, tcCursorName

LOCAL lcFirstLine, lcCreateCursorStr, lcTempFile
LOCAL lnOldMemowidth, lnErrorNo

lnOldMemowidth = SET("Memowidth")
SET MEMOWIDTH TO 8000
lnErrorNo = 0
lcFirstLine = MLINE(tcDataString, 1)

&& Expected format of first row in CSV file: FirstColumn c(1),SecondColumn N(5,2),...
lcTempFile = ADDBS(SYS(2023)) + SYS(2015) +".csv"
STRTOFILE(tcDataString, lcTempFile)

lcCreateCursorStr = "CREATE CURSOR " + tcCursorName + " (" + lcFirstLine + ")"
TRY
	&lcCreateCursorStr.
*		After CREATE CURSOR, the newly created cursor is current work alias
	APPEND FROM (lcTempFile) TYPE CSV

CATCH TO loError
	lnErrorNo = loError.ERRORNO

FINALLY
	SET MEMOWIDTH TO lnOldMemowidth
*{JEI MS 19.05.2007
	IF FILE(lcTempFile)
		DELETE FILE (lcTempFile)
	ENDIF
*}JEI MS 19.05.2007
ENDTRY

*{JEI MS 19.04.2007, a numeric result is expected
RETURN lnErrorNo

ENDFUNC

*-------------------------------------------------------
* Function....: CursorToConfigVFX
* Called by...: cClientDataAccessVfxBase.SaveConfig()
*				UpdateConfigVFXStructure()
* Abstract....: Stores the specified cursor in config.vfx
*				in a specified format
* Returns.....:
*
* Parameters..: tnConfigVfxFormat, tcCursorName, tcFilePath,
*				tcPass
* Notes.......:
*-------------------------------------------------------

FUNCTION CursorToConfigVFX
LPARAMETERS tnConfigVfxFormat, tcCursorName, tcFilePath, tcPass
LOCAL lcXML, lcXMLFileStr, llConfigNotCreated 

IF !(TYPE("tnConfigVfxFormat") == "N") OR ;
		EMPTY(tcCursorName)
	RETURN .F.
ENDIF
IF EMPTY(tcFilePath)
	*{HC MS 2016-05-09
	IF type("goProgram.cConfigPath") = "C"
		tcFilePath = goProgram.cConfigPath 
	ENDIF 	

	IF EMPTY(tcFilePath)
		loFLocations = NEWOBJECT("cApplicationFileLoc")
		tcFilePath = ADDBS(ALLTRIM(loFLocations.GetFileCreationFolder(1))) + "Config.vfx"
		RELEASE loFLocations
	ENDIF
	*}HC MS 2016-05-09
ENDIF

lcXML = ""
lcXMLFileStr = ""
DO CASE

	CASE tnConfigVfxFormat = 0 		&& XML
		IF CURSORTOXML(tcCursorName, "lcXML", 1, 0, 0,"1") > 0
			lcXMLFileStr = ENCRYPT(lcXML, tcPass)
			IF VARTYPE(lcXMLFileStr) = "C"
				TRY
					STRTOFILE(lcXMLFileStr, tcFilePath, 0)
					IF EMPTY(goProgram.cConfigPath)
						goProgram.cConfigPath = tcFilePath
					ENDIF
				CATCH
					*{HC MS 2016-05-06
					llConfigNotCreated = .T.
					*}HC MS 2016-05-06				
				ENDTRY
			ENDIF
		ENDIF

	CASE tnConfigVfxFormat = 1		&& CSV
		lcFileContents = CursorToCSVString(tcCursorName)
		TRY
			lcFileContents = ENCRYPT(lcFileContents, tcPass)
			STRTOFILE(lcFileContents, tcFilePath, 0)
		CATCH TO loError
			lnError = loError.ERRORNO
			*{HC MS 2016-05-06
			llConfigNotCreated = .T.			
			*}HC MS 2016-05-06
		ENDTRY

	OTHERWISE

ENDCASE

*{HC MS 2016-05-06
IF llConfigNotCreated = .T.
	lcFilePath = putfile("Select where to create Config.vfx", "Config.vfx", "vfx")
	IF !empty(lcFilePath)
		CursorToConfigVFX(tnConfigVfxFormat, tcCursorName, lcFilePath, tcPass)
	ENDIF 
ENDIF 
*}HC MS 2016-05-06

ENDFUNC

*-------------------------------------------------------
* Function....: CursorToCSVString
* Called by...: CursorToConfigVFX
*
* Abstract....: Converts the specified cursor in string data
*				using CSV format with additional structure info line
*				Converts Memo data type to C(254)
* Returns.....: String
*
* Parameters..: tcAlias
*
* Notes.......:
*-------------------------------------------------------

FUNCTION CursorToCSVString
LPARAMETERS tcAlias	&& Optional - works with current alias if tcAlias is not pased

LOCAL lcTempFile, lcTempCursorStr, lcFirstLine, lcField, lcFieldDef, lcFileContents
DIMENSION laClientFieldTypes[1], laClientData[1]
LOCAL loError, lnError, lnOldMemowidth, lnCurrentRecno, lnSelect
LOCAL lcSafety, lcReplaceLine, lcAliasToConvert, lcFieldType

lnSelect = SELECT()
IF !EMPTY(tcAlias)
	IF USED(tcAlias)
		SELECT (tcAlias)
	ELSE
		RETURN ''	&& alias is not in use
	ENDIF
ENDIF
lcAliasToConvert = ALIAS()
lnCurrentRecno = RECNO()
lnOldMemowidth = SET("Memowidth")
SET MEMOWIDTH TO 8000
lcSafety = SET("Safety")
SET SAFETY OFF
lcTempFile = ADDBS(SYS(2023)) + SYS(2015) +".csv"
lcFirstLine = ""
lcField = ""
lcFieldDef = ""

AFIELDS(laClientFieldTypes, lcAliasToConvert)
lnFieldCount = ALEN(laClientFieldTypes, 1)

lcTempCursorStr = "CREATE CURSOR crsCSVcontents ("

*{JEI MS 19.04.2007 It may happen at this moment to have goProgram as Object, but goLocalize not.
lcLongStringMessageFrom = IIF(TYPE("goProgram") = "O" AND !ISNULL(goProgram), IIF(goProgram.lRuntimeLocalization, ;
	IIF(TYPE("goLocalize") = "O", goLocalize.cTHELONGSTRING, "The long string "), ;
	THELONGSTRING), "The long string ")

lcLongStringMessageTo = IIF(TYPE("goProgram") = "O" AND !ISNULL(goProgram), IIF(goProgram.lRuntimeLocalization, ;
	IIF(TYPE("goLocalize") = "O", goLocalize.cISTRIMMEDTO, "Is trimmed to"), ;
	ISTRIMMEDTO), "Is trimmed to")

lcLongStringMessageTitle = IIF(TYPE("goProgram") = "O" AND !ISNULL(goProgram), IIF(goProgram.lRuntimeLocalization, ;
	IIF(TYPE("goLocalize") = "O", goLocalize.cALONGSTRING, "A long string is trimmed"), ;
	ALONGSTRING), "A long string is trimmed")
*}JEI MS 19.04.2007

FOR i = 1 TO lnFieldCount
	lcFieldType = laClientFieldTypes[i,2]
	DO CASE
		CASE lcFieldType == "M"	&& field in table is memo
			lcFieldDef = "C(254)"

			lcFieldName = laClientFieldTypes[i,1]
			SCAN FOR LEN(ALLTRIM(EVALUATE(lcFieldName))) > 254
				 = MESSAGEBOX(lcLongStringMessageFrom + CHR(13) + CHR(10) + ;
					ALLTRIM(EVALUATE(lcFieldName)) + CHR(13) + CHR(10) + ;
					lcLongStringMessageTo + CHR(13) + CHR(10) + ;
					LEFT(ALLTRIM(EVALUATE(lcFieldName)), 254), 0, ;
					lcLongStringMessageTitle)
				REPLACE &lcFieldName WITH LEFT(EVALUATE(lcFieldName),254)
			ENDSCAN
*{JEI MS 11.05.2007
			REPLACE &lcFieldName WITH CHRTRAN(EVALUATE(lcFieldName),["],[']) ALL
*}JEI MS 11.05.2007

		CASE (lcFieldType $ "CQV")
			lcFieldDef = lcFieldType + "(" + TRANSFORM(laClientFieldTypes[i,3]) + ")"
*{JEI MS 11.05.2007
			lcFieldName = laClientFieldTypes[i,1]
			REPLACE &lcFieldName WITH CHRTRAN(EVALUATE(lcFieldName),["],[']) ALL
*}JEI MS 11.05.2007

		CASE (lcFieldType $ "FN")
			lcFieldDef = lcFieldType + "(" + TRANSFORM(laClientFieldTypes[i,3]) + "," + TRANSFORM(laClientFieldTypes[i,4]) + ")"

		CASE lcFieldType == "B"
			lcFieldDef = lcFieldType + "(" + TRANSFORM(laClientFieldTypes[i,4]) + ")"

		CASE lcFieldType == "I"
			lcFieldDef = lcFieldType

		OTHERWISE
			lcFieldDef = lcFieldType

	ENDCASE

*		Construct a structure description line for CSV file
	lcFirstLine = lcFirstLine + ", " + laClientFieldTypes[i,1] +" " + lcFieldDef

*		Construct a CREATE CURSOR String
	lcField = laClientFieldTypes[i,1] +" " + lcFieldDef + IIF(laClientFieldTypes[i,5]," NULL ","")
	lcTempCursorStr = lcTempCursorStr + lcField + IIF(i < lnFieldCount,",",")")

ENDFOR

lcFirstLine = SUBSTR(lcFirstLine, 3)	&& remove 1st comma
RELEASE laClientFieldTypes

TRY
*		Create a cursor with no Memo fields in it
	&lcTempCursorStr.
	SELECT crsCSVcontents
	APPEND FROM DBF(lcAliasToConvert)

*		Copy to a temporary file
	COPY TO (lcTempFile) TYPE CSV

*		Load file content and store structure description line
	lcFileContents = FILETOSTR(lcTempFile)
	lcReplaceLine = MLINE(lcFileContents, 1)
	lcFileContents = STRTRAN(lcFileContents, lcReplaceLine, lcFirstLine)

CATCH TO loError
	lnError = loError.ERRORNO
FINALLY
	IF FILE(lcTempFile)
		DELETE FILE (lcTempFile)
	ENDIF
	IF USED("crsCSVcontents")
		USE IN crsCSVcontents
	ENDIF
ENDTRY

SET MEMOWIDTH TO lnOldMemowidth
SET SAFETY &lcSafety.

*{JEI MS 11.05.2007 Added
SELECT (lcAliasToConvert)
IF lnCurrentRecno <= RECCOUNT()
	GO (lnCurrentRecno)
ENDIF

SELECT (lnSelect)
*}JEI MS 11.05.2007

RETURN lcFileContents

ENDFUNC


*-------------------------------------------------------
* Function....: GetEmptyValue
* Called by...: cPickAlternateBase.GetEmptyValue()
*
* Abstract....: Returns a variable with type equal to
*				the type of a specific field from a table
* Returns.....:
*
* Parameters..: lcTableName, lcFieldName
*
* Notes.......:
*-------------------------------------------------------

FUNCTION GetEmptyValue
LPARAMETERS  lcTableName, lcFieldName

LOCAL lResultValue, lnSelect

tmpAlias = SYS(2015)
lnSelect = SELECT() && JEI RI 2008.06.04 changed from ALIAS() to SELECT()
lResultValue = .NULL.
lcTableName = FORCEEXT(lcTableName, "")

IF TYPE(ALLTRIM(lcTableName) + "." + ALLTRIM(lcFieldName)) # "U"
	SQL_str = "Select " + lcFieldName + " From " + lcTableName + ;
		" Into Cursor " + tmpAlias + " Where .F. ReadWrite"
	&SQL_str

	APPEND BLANK

	lResultValue = EVALUATE(tmpAlias + [.] + lcFieldName)

	USE IN (tmpAlias)

	IF !EMPTY(lnSelect)
		SELECT(lnSelect)
	ENDIF
ENDIF

RETURN lResultValue
ENDFUNC


*-------------------------------------------------------
* Function....: SendDataOverHTTP
* Called by...: cConnectWebService.RegisterCustomerViaHTTP()
*
* Abstract....: Returns 0 when the registration is OK.
*				The error text is written in tcLastErrorText
* Returns.....:
*
* Parameters..: tcUrlServerName, tcUrlObjectName, tcDataString, tcMethodToCall, tcLastErrorText
*
* Notes.......:
*-------------------------------------------------------
FUNCTION SendDataOverHTTP
LPARAMETERS tcUrlServerName, tcUrlObjectName, tcDataString, tcMethodToCall, tcLastErrorText

#DEFINE INTERNET_OPEN_TYPE_PRECONFIG	0
#DEFINE INTERNET_DEFAULT_HTTP_PORT		80
#DEFINE INTERNET_SERVICE_HTTP			3
#DEFINE INTERNET_FLAG_RELOAD			0x80000000
#DEFINE INTERNET_FLAG_KEEP_CONNECTION   0x00400000
#DEFINE HTTP_QUERY_FLAG_NUMBER			0x20000000
#DEFINE HTTP_QUERY_STATUS_CODE			19
#DEFINE HTTP_STATUS_OK                  200

DECLARE LONG InternetOpen IN "wininet.dll" ;
	STRING lpszAgent, ;
	LONG dwAccessType, ;
	STRING lpszProxyName, ;
	STRING lpszProxyBypass, ;
	LONG dwFlags

DECLARE LONG InternetConnect IN "wininet.dll" ;
	LONG hInternetSession, ;
	STRING lpszServerName, ;
	SHORT nServerPort, ;
	STRING lpszUsername, ;
	STRING lpszPassword, ;
	LONG dwService, ;
	LONG dwFlags, ;
	LONG dwContext

DECLARE LONG HttpOpenRequest IN "wininet.dll" ;
	LONG hHttpSession, ;
	STRING lpszVerb, ;
	STRING lpszObjectName, ;
	STRING lpszVersion, ;
	STRING lpszReferer, ;
	STRING lpszAcceptTypes, ;
	LONG dwFlags, ;
	LONG dwContext

DECLARE LONG HttpSendRequest IN "wininet.dll" ;
	LONG hHttpRequest, ;
	STRING lpszHeaders, ;
	LONG dwHeadersLength, ;
	STRING lpOptional, ;
	LONG dwOptionalLength

DECLARE LONG HttpQueryInfo IN "wininet.dll" ;
	LONG hHttpRequest, ;
	LONG dwInfoLevel, ;
	LONG @ lpvBuffer, ;
	LONG @ lpdwBufferLength, ;
	LONG @ lpdwIndex

DECLARE LONG InternetReadFile IN "wininet.dll" ;
	LONG hFile, ;
	STRING @ lpBuffer, ;
	LONG dwNumberOfBytesToRead, ;
	LONG @ lpNumberOfBytesRead

DECLARE INTEGER InternetCloseHandle IN "wininet.dll" ;
	LONG hInet

DECLARE INTEGER GetLastError IN "win32api"

LOCAL lnReturnRes, lcurlServerName, lcurlObjectName, lcSrtingtoSend, lcResponseBuffer, lcHeader
LOCAL lnInternetSession, lnInternetConnect, lnHttpOpenRequest, llDoLoop, lcReadBuffer, lnTotalBytesRead, lnStatus, lnStatusSize, lnIndex

lnReturnRes = 0

lcurlServerName = tcUrlServerName
lcurlObjectName = tcUrlObjectName
lcSrtingtoSend = tcDataString
lcResponseBuffer = ""

IF TYPE("lcSrtingtoSend") == "C" AND !EMPTY(lcSrtingtoSend) AND !EMPTY(lcurlServerName) AND !EMPTY(lcurlObjectName)
	lcPostData = "tcMethodName=" + tcMethodToCall + "&tcData=" + lcSrtingtoSend

	lnInternetSession = 0
	lnInternetConnect = 0
	lnHttpOpenRequest = 0
	TRY
		lnInternetSession = InternetOpen("VFPHTTP", INTERNET_OPEN_TYPE_PRECONFIG, "", "", 0)

		IF lnInternetSession != 0
			lnInternetConnect = InternetConnect(lnInternetSession, lcurlServerName, INTERNET_DEFAULT_HTTP_PORT, "", "", INTERNET_SERVICE_HTTP, 0, 0)

			IF lnInternetConnect != 0
				lnHttpOpenRequest = HttpOpenRequest(lnInternetConnect, "POST", lcurlObjectName, "HTTP/1.1", "", "", INTERNET_FLAG_RELOAD + INTERNET_FLAG_KEEP_CONNECTION, 0)

				IF lnHttpOpenRequest != 0
					lcHeader = ""
					lcHeader = lcHeader + "Accept: text/html" + CHR(13) + CHR(10)
					lcHeader = lcHeader + "Cache-Control: no-cache" + CHR(13) + CHR(10)
					lcHeader = lcHeader + "Connection: Keep-Alive" + CHR(13) + CHR(10)
					lcHeader = lcHeader + "Content-Length: " + TRANSFORM(LEN(lcpostdata)) + CHR(13) + CHR(10)
					lcHeader = lcHeader + "Content-Type: application/x-www-form-urlencoded" + CHR(13) + CHR(10)
					lcHeader = lcHeader + "Host: " + lcurlServerName + CHR(13) + CHR(10)

					IF 0 != HttpSendRequest(lnHttpOpenRequest, lcHeader, LEN(lcHeader), lcPostData, LEN(lcPostData))
						lnStatus = 0
						lnStatusSize = 4
						lnIndex = 0

						IF 0 != HttpQueryInfo(lnHttpOpenRequest, HTTP_QUERY_FLAG_NUMBER + HTTP_QUERY_STATUS_CODE, @lnStatus, @lnStatusSize, @lnIndex)
							DO CASE
								CASE lnStatus = HTTP_STATUS_OK
									llDoLoop = .T.

									DO WHILE llDoLoop
										lcReadBuffer = SPACE(2048)
										lnTotalBytesRead = 0

										IF 0 != InternetReadFile(lnHttpOpenRequest, @lcReadBuffer, LEN(lcReadBuffer), @lnTotalBytesRead)
											lcResponseBuffer = lcResponseBuffer + LEFT(lcReadBuffer, lnTotalBytesRead)
											IF lnTotalBytesRead = 0
												llDoLoop = .F.
												lnReturnRes = 0
											ENDIF
										ELSE
											llDoLoop = .F.
											HandleInternetApiError(@lnReturnRes, @tcLastErrorText)
										ENDIF
									ENDDO
								OTHERWISE
									lnReturnRes = -1
									tcLastErrorText = "Server error. HTTP Status code: " + TRANSFORM(lnStatus)
							ENDCASE
						ELSE	&& 0 != HttpQueryInfo
							HandleInternetApiError(@lnReturnRes, @tcLastErrorText)
						ENDIF	&& 0 != HttpQueryInfo
					ELSE	&& 0 != HttpSendRequest
						HandleInternetApiError(@lnReturnRes, @tcLastErrorText)
					ENDIF 	&& 0 != HttpSendRequest
				ELSE	&& lnHttpOpenRequest != 0
					HandleInternetApiError(@lnReturnRes, @tcLastErrorText)
				ENDIF 	&& lnHttpOpenRequest != 0
			ELSE		&& lnInternetConnect != 0
				HandleInternetApiError(@lnReturnRes, @tcLastErrorText)
			ENDIF 		&& lnInternetConnect != 0
		ELSE		&& lnInternetSession != 0
			HandleInternetApiError(@lnReturnRes, @tcLastErrorText)
		ENDIF 		&& lnInternetSession != 0

		tcDataString = lcResponseBuffer

	CATCH TO loError
		tcLastErrorText = loError.MESSAGE
		lnReturnRes = -1
	FINALLY
		IF lnHttpOpenRequest != 0
			InternetCloseHandle(lnHttpOpenRequest)
		ENDIF
		IF lnInternetConnect != 0
			InternetCloseHandle(lnInternetConnect)
		ENDIF
		IF lnInternetSession != 0
			InternetCloseHandle(lnInternetSession)
		ENDIF
	ENDTRY
ENDIF

&& clear dlls because of error "Declare DLL call caused an exception"
CLEAR DLLS "InternetOpen","InternetConnect","HttpOpenRequest","HttpSendRequest", ;
	"HttpQueryInfo","InternetReadFile","InternetCloseHandle","GetLastError", ;
	"GetModuleHandle","FormatMessage","LocalFree","RtlMoveMemory"

RETURN lnReturnRes

ENDFUNC

PROCEDURE HandleInternetApiError
LPARAMETERS nErrorno, cMessage

DECLARE INTEGER GetLastError IN "win32api"

LOCAL nLen

nErrorno = GetLastError()
*0x0409 English (United States)
GetMessage("wininet", nErrorno, IIF(TYPE("goProgram.nLangLcid") == "N", goProgram.nLangLcid, 1033), @nLen, @cMessage)
IF nLen = 0
	GetMessage("wininet", nErrorno, LANG_NEUTRAL + (SUBLANG_DEFAULT * 1024), @nLen, @cMessage)
ENDIF
IF nLen = 0
	GetMessage("system", nErrorno, IIF(TYPE("goProgram.nLangLcid") == "N", goProgram.nLangLcid, 1033), @nLen, @cMessage)
ENDIF
IF nLen = 0
	GetMessage("system", nErrorno, LANG_NEUTRAL + (SUBLANG_DEFAULT * 1024), @nLen, @cMessage)
ENDIF

ENDPROC

PROCEDURE GetMessage
LPARAMETERS cSource, nErrorno, nLangid, nLen, cMessage

#DEFINE FORMAT_MESSAGE_ALLOCATE_BUFFER	0x100
#DEFINE FORMAT_MESSAGE_FROM_HMODULE		0x800
#DEFINE FORMAT_MESSAGE_FROM_SYSTEM		0x1000
#DEFINE FORMAT_MESSAGE_IGNORE_INSERTS	0x200

DECLARE INTEGER GetModuleHandle IN kernel32 STRING lpModuleName

DECLARE INTEGER FormatMessage IN kernel32;
	INTEGER dwFlags, INTEGER lpSource, INTEGER dwMsgId, ;
	INTEGER dwLangId, INTEGER @lpBuffer, ;
	INTEGER nSize, INTEGER Arguments

DECLARE INTEGER LocalFree IN kernel32 INTEGER HMEM

DECLARE RtlMoveMemory IN kernel32 AS CopyMemory;
	STRING @dst, INTEGER src, INTEGER nLen

LOCAL nFlags, lpSource, hBuffer

cMessage = ""

DO CASE
	CASE cSource == "wininet"
		nFlags = FORMAT_MESSAGE_ALLOCATE_BUFFER + ;
			FORMAT_MESSAGE_FROM_HMODULE + ;
			FORMAT_MESSAGE_IGNORE_INSERTS
		lpSource = GetModuleHandle("wininet.dll")
	CASE cSource == "system"
		nFlags = FORMAT_MESSAGE_ALLOCATE_BUFFER + ;
			FORMAT_MESSAGE_FROM_SYSTEM + ;
			FORMAT_MESSAGE_IGNORE_INSERTS
		lpSource = 0
ENDCASE

hBuffer = 0

nLen = FormatMessage(nFlags, lpSource, ;
	nErrorno, nLangid, @hBuffer, 0, 0)

IF nLen <> 0
	cMessage = REPLICATE(" ", nLen)
	CopyMemory(@cMessage, hBuffer, nLen)
	 = LocalFree(hBuffer)
ENDIF

ENDPROC

*-------------------------------------------------------
* Function....: IsField
* Called by...:
*
* Abstract....:
* Returns.....: .T. if tcalias contains alias and field name and stores the alias in
*				tcalias and the field name in tcfield
*
* Parameters..: tcalias, tcfield
*
* Notes.......:
*-------------------------------------------------------

FUNCTION IsField
LPARAMETERS tcalias, tcfield

LOCAL llx, lcerror
lcerror = ON("error")
llx         = .F.

IF EMPTY(tcalias)
	RETURN .F.
ENDIF

tcfield = IIF(AT(".", tcalias) = 0, tcalias, ;
	RIGHT(tcalias, LEN(tcalias) - AT(".", tcalias)))
tcalias = IIF(AT(".", tcalias) = 0, ALIAS(), ;
	LEFT(tcalias, AT(".", tcalias) -1))

IF USED(tcalias) AND TYPE(ALLTRIM(tcalias) + "." + ALLTRIM(tcfield)) # "U"
	llx = .T.
ENDIF
RETURN llx

ENDFUNC


*-------------------------------------------------------
* Function....: GetEmptyVariableValue
* Called by...:
*
* Abstract....:
* Returns.....: variable with type equal to
*				the type of a specific variable passed as parameter
*
* Parameters..: tvValue
*
* Notes.......:
*-------------------------------------------------------

FUNCTION GetEmptyVariableValue
LPARAMETERS tvValue

LOCAL tvResult
DO CASE
	CASE VARTYPE(tvValue) = "C"
		tvResult = ''

	CASE VARTYPE(tvValue) = "D"
		tvResult = CTOD('')

	CASE VARTYPE(tvValue) = "T"
		tvResult = CTOT('')

	CASE VARTYPE(tvValue) = "L"
		tvResult = .F.

	CASE VARTYPE(tvValue) $ "NY"
		tvResult = 0

	OTHERWISE
		tvResult = .NULL.
ENDCASE

RETURN tvResult
ENDFUNC

*-------------------------------------------------------
* Function....: IsAdmin
* Called by...: cUserDialogFormbase in vfxformbase.vcx
*
* Abstract....:
* Returns.....: Returns True when cuurent user has Admin access code
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------

FUNCTION IsAdmin

RETURN ( TYPE("goUser.user") ="C" AND goUser.USER ="ADMIN" ) OR;
	(TYPE("goUser.useraccess") ="C" AND ",ADMIN," $ UPPER(',' + STRTRAN(goUser.useraccess, " ", "") + ',' ))
ENDFUNC

*-------------------------------------------------------
* Function....: GetHKEYConstantValue
* Called by...: cvfxActivate in vfxAppl.vcx
*
* Abstract....:
* Returns.....: Returns the constant value
*
* Parameters..: tcConstantName
*
* Notes.......:
*-------------------------------------------------------

FUNCTION GetHKEYConstantValue
LPARAMETERS tcConstantName

LOCAL lnResult
lnResult = 0
DO CASE
	CASE UPPER(tcConstantName) == "HKEY_LOCAL_MACHINE"
		lnResult = HKEY_LOCAL_MACHINE
	CASE UPPER(tcConstantName) == "HKEY_CLASSES_ROOT"
		lnResult = HKEY_CLASSES_ROOT
	CASE UPPER(tcConstantName) == "HKEY_CURRENT_USER"
		lnResult = HKEY_CURRENT_USER
	CASE UPPER(tcConstantName) == "HKEY_USERS"
		lnResult = HKEY_USERS
	CASE UPPER(tcConstantName) == "HKEY_PERFORMANCE_DATA"
		lnResult = HKEY_PERFORMANCE_DATA
	CASE UPPER(tcConstantName) == "HKEY_CURRENT_CONFIG"
		lnResult = HKEY_CURRENT_CONFIG
	CASE UPPER(tcConstantName) == "HKEY_DYN_DATA"
		lnResult = HKEY_DYN_DATA
ENDCASE
RETURN lnResult

*-------------------------------------------------------
* Function....: UTCTime()
* Called by...:
*
* Abstract....:
* Returns.....: Return .T. if the time was changed successfully.
*				Return .F. if the time cannot be changed for any reason.
*
* Parameters..: tdDate, tcTime
*
* Notes.......: It`s possible to use only tdDatetime
*-------------------------------------------------------

PROCEDURE UTCTime
LPARAMETERS tdDate, tcTime

LOCAL lcUTCTime, lcFileTime, lcSysTime, ldDate, llRes, lnYear, lnMonth, lnDay, lnHour, lnMinute, lnSeconds

llRes = .F.

&&Declare API functions
DECLARE INTEGER LocalFileTimeToFileTime IN kernel32;
	STRING LOCALFILETIME, ;
	STRING @FILETIME

DECLARE INTEGER SystemTimeToFileTime IN kernel32;
	STRING lpSystemTime, ;
	STRING @lpFileTime

DECLARE INTEGER FileTimeToSystemTime IN kernel32;
	STRING  lpFileTime, ;
	STRING @lpSystemTime

&&Declare variables used in API functions
lcUTCTime = SPACE(10)
lcFileTime = SPACE(10)
lcSysTime = SPACE(10)

&&Check the data type of the parameters
DO CASE
	CASE VARTYPE(tdDate) = "T" &&UTC(ttDatatime)
&&Create string used in SystemTimeToFileTime and FileTimeToSystemTime
		lcSysTime = num2buf(YEAR(tdDate)) + num2buf(MONTH(tdDate)) + ;
			num2buf(DOW(tdDate, 0)) + num2buf(DAY(tdDate)) + ;
			num2buf(HOUR(tdDate)) + num2buf(MINUTE(tdDate)) + num2buf(SEC(tdDate)) + REPLI(CHR(0), 2)

	CASE VARTYPE(tdDate) = "D" AND VARTYPE(tcTime) = "C" &&UTCTime(tdDate,tcTime)
&&Create string used in SystemTimeToFileTime and FileTimeToSystemTime
		lcSysTime = Num2Buf(YEAR(tdDate)) + Num2Buf(MONTH(tdDate)) + ;
			Num2Buf(DOW(tdDate, 0)) + Num2Buf(DAY(tdDate)) + ;
			Num2Buf(HOUR(CTOT(tcTime))) + Num2Buf(MINUTE(CTOT(tcTime))) + Num2Buf(SEC(CTOT(tcTime))) + REPLI(CHR(0), 2)

	OTHERWISE
		RETURN .F. &&If the date type of the parameters is different return .F.

ENDCASE


IF SystemTimeToFileTime(lcSysTime, @lcFileTime) = 1 &&Convert the System time to Local time
	IF LocalFileTimeToFileTime(lcFileTime, @lcUTCTime) = 1
		IF FileTimeToSystemTime(lcUTCTime, @lcSysTime) = 1
			llRes = .T.
		ENDIF
	ENDIF
ENDIF

lnYear = ASC(SUBSTR(lcSysTime, 2)) * 256 + ASC(SUBSTR(lcSysTime, 1))
lnMonth = ASC(SUBSTR(lcSysTime, 4)) * 256 + ASC(SUBSTR(lcSysTime, 3))
lnDay = ASC(SUBSTR(lcSysTime, 8)) * 256 + ASC(SUBSTR(lcSysTime, 7))
lnHour = ASC(SUBSTR(lcSysTime, 10)) * 256 + ASC(SUBSTR(lcSysTime, 9))
lnMinute = ASC(SUBSTR(lcSysTime, 12)) * 256 + ASC(SUBSTR(lcSysTime, 11))
lnSeconds = ASC(SUBSTR(lcSysTime, 14)) * 256 + ASC(SUBSTR(lcSysTime, 13))



DO CASE
	CASE VARTYPE(tdDate) = "T"
		tdDate = DATETIME(lnYear, lnMonth, lnDay, lnHour, lnMinute, lnSeconds ) &&Specifies the Datetime


	CASE VARTYPE(tdDate) = "D" AND VARTYPE(tcTime) = "C"
		tdDate = DATE(lnYear, lnMonth, lnDay) &&Specifies the Date
&&Specifies the time in charecter variable
		tcTime = PADL(TRANSF(lnHour), 2,'0') +":" + ;
			PADL(TRANSF(lnMinute), 2,'0') +":" + ;
			PADL(TRANSF(lnSeconds), 2,'0')


ENDCASE


RETURN llRes
ENDPROC

*-------------------------------------------------------
* Function....: Num2Buf
* Called by...: UTCTime
*
* Abstract....:
* Returns.....: String
*
* Parameters..: tnValue
*
* Notes.......:
*-------------------------------------------------------

FUNCTION Num2Buf
LPARAMETERS tnValue
RETURN CHR(MOD(m.tnValue, 256)) + CHR(INT(m.tnValue / 256))
ENDFUNC

*-------------------------------------------------------
* Function....: OpenVfxUsr
* Called by...: when goProgram.nVfxSysTableLoc >= 1
* Returns.....: USED("vfxUsr")
*
* Notes.......: Opens vfxUsr with different keys depending on
*				idvfxusr field (exist or no)
*-------------------------------------------------------
FUNCTION OpenVfxUsr
LPARAMETERS tcAliasName, toObject, toUsrAdptr
LOCAL lnRow

IF EMPTY(tcAliasName)
	tcAliasName = "vfxUsr"
ENDIF

oUsrAdptr = OPENTABLE("vfxUsr", "", .F., "", .T.,, "vfxUsr", "cAppVFXDataAccess")
AFIELDS(laUsrFields, "vfxUsr")
lnRow = ASCAN(laUsrFields, "IDVFXUSR", 1, ALEN(laUsrFields, 1), 1, 15)
USE IN vfxUsr
RELEASE oUsrAdptr
*{V&U MS 2014-10-24
IF lnRow > 0
	toUsrAdptr = OPENTABLE("vfxUsr", "IDvfxUsr", .F., "INDEX ON UPPER(user_name) TAG NAME ADDITIVE" + CHR(13) + CHR(10) + ;
		"INDEX ON UPPER(user) TAG USER ADDITIVE" + CHR(13) + CHR(10) + ;
		"SET ORDER TO TAG USER", .T., toObject, tcAliasName, "cAppVFXDataAccess", .F., .F., .F., .F., "USER")
ELSE
	toUsrAdptr = OPENTABLE("vfxUsr", "User", .F., "INDEX ON UPPER(user_name) TAG NAME ADDITIVE" + CHR(13) + CHR(10) + ;
		"INDEX ON UPPER(user) TAG USER ADDITIVE" + CHR(13) + CHR(10) + ;
		"SET ORDER TO TAG USER", .T., toObject, tcAliasName, "cAppVFXDataAccess", .F., .F., .F., .F., "USER")
ENDIF
*}V&U MS 2014-10-24

RETURN USED(tcAliasName)
ENDFUNC

*-------------------------------------------------------
* Function....: LoadUserRights
* Called by...: LogiOk; Relogon
* Returns.....:
*
* Notes.......: Make goUserRights object
*				vfxUserGroups, vfxGroupRights, vfxFopen must be open
*-------------------------------------------------------
FUNCTION LoadUserRights

*{V&U MS 2011-04-22, Use ==
SELECT ug.USER, gr.formname, SUM(gr.ViewPermit) AS ViewPermit, ;
	SUM(gr.InsPermit) AS InsPermit, SUM(CopyPermit) AS CopyPermit, SUM(gr.EditPermit) AS EditPermit, ;
	SUM(gr.DelPermit) AS DelPermit, SUM(gr.PrtPermit) AS PrtPermit, SUM(gr.ExpPermit) AS ExpPermit,  ;
	vo.FORM AS FORM ;
	FROM vfxUserGroups ug ;
	JOIN vfxGroupRights gr ON ug.groupID = gr.groupID AND ALLTRIM(UPPER(ug.USER)) == ALLTRIM(UPPER(goUser.USER)) ;
	RIGHT JOIN vfxFopen vo ON gr.IDVFXFOPEN = vo.IDVFXFOPEN ;
	GROUP BY ug.USER, gr.formname, vo.FORM ;
	INTO CURSOR UsrGrps READWRITE
*}V&U MS 2011-04-22

REPLACE FormName WITH NVL(FormName, FORM), ViewPermit WITH NVL(ViewPermit, 1), ;
	EditPermit WITH NVL(EditPermit, 1), InsPermit WITH NVL(InsPermit, 1), ;
	DelPermit WITH NVL(DelPermit, 1), PrtPermit WITH NVL(PrtPermit, 1), ;
	CopyPermit WITH NVL(CopyPermit, 1), ExpPermit WITH NVL(ExpPermit, 1) ALL

GO TOP
IF RECCOUNT() > 0
	LOCATE FOR !(EMPTY(USER) OR ISNULL(USER))
	IF FOUND()
		PUBLIC goUserRights
		goUserRights = CREATEOBJECT("Empty")
		SCAN
			cChObj = JUSTSTEM(ALLTRIM(UsrGrps.FormName))
			IF !EMPTY(cChObj)
				oChObj = CREATEOBJECT("Empty")
				TRY
					ADDPROPERTY(goUserRights, cChObj, oChObj)
					ADDPROPERTY(goUserRights.&cChObj, "ViewPermit",IIF(UsrGrps.ViewPermit>0, .T., .F.))
					ADDPROPERTY(goUserRights.&cChObj, "NewPermit",IIF(UsrGrps.InsPermit>0, .T., .F.))
					ADDPROPERTY(goUserRights.&cChObj, "EditPermit",IIF(UsrGrps.EditPermit>0, .T., .F.))
					ADDPROPERTY(goUserRights.&cChObj, "DeletePermit",IIF(UsrGrps.DelPermit>0, .T., .F.))
					ADDPROPERTY(goUserRights.&cChObj, "PrintPermit",IIF(UsrGrps.PrtPermit>0, .T., .F.))
					ADDPROPERTY(goUserRights.&cChObj, "CopyPermit",IIF(UsrGrps.CopyPermit>0, .T., .F.))
					ADDPROPERTY(goUserRights.&cChObj, "ExportPermit",IIF(UsrGrps.ExpPermit>0, .T., .F.))
				CATCH TO loError
				ENDTRY
			ENDIF
		ENDSCAN
	ENDIF
ENDIF
USE IN UsrGrps
ENDFUNC

*-------------------------------------------------------
* Function....: CreateEmptyDBCSync
* Called by...:
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcDBCName, tcTargetFolder (by Ref)
*
* Notes.......: Creates an empty DBC using files generated by GenDBC
*-------------------------------------------------------
FUNCTION CreateEmptyDBCSync
LPARAMETERS tcDBCName AS STRING, tcTargetFolder AS STRING

LOCAL 	llRes AS LOGICAL, loError AS EXCEPTION, lcGenDBCFunction, lcKRTDBFName, lcKRTFile, ;
	lcOldSetPath, lnErrorNo AS INTEGER

IF TYPE("ThisForm.oFtpSyncClient.cLogFile") == "C"
	THISFORM.oFtpSyncClient.WriteLog("CreateEmptyDBCSync is started. Parameters: tcDBCName: " + ;
		ALLTRIM(tcDBCName) + "; tcTargetFolder: " + ALLTRIM(tcTargetFolder))
ENDIF

IF EMPTY(tcDBCName) OR VARTYPE(tcDBCName) <> "C"
	RETURN .F.
ENDIF
tcDBCName = FORCEEXT(tcDBCName, "dbc")
IF EMPTY(tcTargetFolder) OR VARTYPE(tcTargetFolder) <> "C"
	tcTargetFolder = ADDBS(SYS(2023)) + SYS(2015)
ENDIF
tcTargetFolder = ADDBS(tcTargetFolder)
IF !DIRECTORY(tcTargetFolder, 1)
	TRY
		MD (tcTargetFolder)
	CATCH TO loError
		IF TYPE("ThisForm.oFtpSyncClient.cLogFile") = "C"
			THISFORM.oFtpSyncClient.WriteLog("*** Error in MD (tcTargetFolder): " + ALLTRIM(loError.MESSAGE))
		ENDIF
	ENDTRY
ENDIF
IF !DIRECTORY(tcTargetFolder, 1)
	RETURN .F.
ENDIF
lcGenDBCFunction = 'VFXSync_' + JUSTSTEM(tcDBCName)
lcKRTFile 		 = ADDBS(SYS(2023)) + 'VFXSync_' + FORCEEXT(tcDBCName, "krt")
lcKRTDBFName 	 = 'VFXSync_' + JUSTSTEM(tcDBCName) + "krt.dbf"
lcOldDir 		 = GetDefaultFolder()
lnErrorNo 		 = 0
lcOldSetPath 	 = SET("Path")
llRes 			 = .T.

IF ADIR(laDummy, ADDBS(lcOldDir) + "Data\" + lcKRTDBFName) > 0
	lcKRTDBFName = ADDBS(lcOldDir) + "Data\" + lcKRTDBFName
ENDIF

IF ADIR(laDir, lcKRTDBFName) > 0
	TRY
		USE (lcKRTDBFName) AGAIN SHARED ALIAS _krtDbf
	CATCH TO loError
		THIS.WriteLog("*** Table " + TRANSFORM(lcKRTDBFName) + " cannot be opened. Error: " + TRANSFORM(loError.MESSAGE))
		llRes = .F.
	ENDTRY

	IF llRes
		SELECT _krtDbf
		LOCATE
		IF ADIR(laDummy, lcKRTFile) > 0
			TRY
				DELETE FILE (lcKRTFile)
			CATCH TO loError
				IF TYPE("ThisForm.oFtpSyncClient.cLogFile") == "C"
					THISFORM.oFtpSyncClient.WriteLog("*** File " + TRANSFORM(lcKRTFile) + " cannot be deleted. Error: " + ;
						ALLTRIM(loError.MESSAGE))
				ENDIF
			ENDTRY
		ENDIF
		COPY MEMO _krtDbf.PROGRAM TO (lcKRTFile)
		USE IN _krtDbf
	ENDIF
ENDIF

CD (tcTargetFolder)
SET PATH TO (JUSTPATH(lcKRTFile)) ADDITIVE

IF !FILE(FORCEEXT(lcGenDBCFunction, "prg"))
	IF ADIR(laDir, ADDBS(lcOldDir) + "Data\" + FORCEEXT(lcGenDBCFunction, "prg")) > 0
		lcGenDBCFunction = ADDBS(lcOldDir) + "Data\" + FORCEEXT(lcGenDBCFunction, "prg")
	ENDIF
ENDIF
TRY
	COMPILE (lcGenDBCFunction)
	DO (lcGenDBCFunction)
CATCH TO loError
	IF TYPE("ThisForm.oFtpSyncClient.cLogFile") = "C"
		THISFORM.oFtpSyncClient.WriteLog("*** Error in CreateEmptyDBCSync: " + ALLTRIM(loError.MESSAGE))
	ENDIF
	llRes = .F.
FINALLY
	CD (lcOldDir)
ENDTRY

IF DBUSED(tcTargetFolder + tcDBCName)
	SET DATABASE TO (tcTargetFolder + tcDBCName)
	CLOSE DATABASES
ENDIF
TRY
	ERASE (lcKRTFile)
CATCH TO loError
	IF TYPE("ThisForm.oFtpSyncClient.cLogFile") = "C"
		THISFORM.oFtpSyncClient.WriteLog("*** Error in CreateEmptyDBCSync: " + ALLTRIM(loError.MESSAGE))
	ENDIF
ENDTRY

SET PATH TO (lcOldSetPath)

IF TYPE("ThisForm.oFtpSyncClient.cLogFile") = "C"
	THISFORM.oFtpSyncClient.WriteLog(IIF(llRes, "", "*** ") + "CreateEmptyDBCSync is finished with result = " + TRANSFORM(llRes))
ENDIF

RETURN llRes
ENDFUNC

*-------------------------------------------------------
* Function....: Emit
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: 
*
* Notes.......: Creates the select command.
*-------------------------------------------------------
FUNCTION Emit
LOCAL noldmaxrecords

EXTERNAL ARRAY wzabqtxt
EXTERNAL ARRAY tafields

PRIVATE m.i, m.j, m.wzsTemp, m.wzsTag, m.wzsFldLst, m.wzsDBFs, m.wzsFilt
EXTERNAL ARRAY wzaQSort, wzaQFlds
PRIVATE wzdatestamp, wztimestamp, wz_timestamp, m.error
m.error = 0
LOCAL llUseForCursorAdapterQuery
*{ V&U DL 2010-11-01
IF TYPE("plCursorAdapterQuery") == "L" AND !ISNULL(plCursorAdapterQuery)
	llUseForCursorAdapterQuery = plCursorAdapterQuery
ENDIF 
LOCAL lnRowCount, lnColumnCount
IF llUseForCursorAdapterQuery
	IF TYPE("wzaQSort", 1) == "A"
		lnRowCount = ALEN(wzaQSort, 1)
		lnColumnCount = ALEN(wzaQSort, 2)
		lnColumnCount = IIF(lnColumnCount == 0, 1, lnColumnCount)
		
		DIMENSION tmp_wzaQSort[lnRowCount, lnColumnCount]
		VFXACOPY(@wzaQSort, @tmp_wzaQSort)
	ENDIF
	
	IF TYPE("wzaQFlds", 1) == "A"
		lnRowCount = ALEN(wzaQFlds, 1)
		lnColumnCount = ALEN(wzaQFlds, 2)
		lnColumnCount = IIF(lnColumnCount == 0, 1, lnColumnCount)
		
		DIMENSION tmp_wzaQFlds[lnRowCount, lnColumnCount]
		VFXACOPY(@wzaQFlds, @tmp_wzaQFlds)
	ENDIF
	
	IF TYPE("wzaQDD", 1) == "A"
		lnRowCount = ALEN(wzaQDD, 1)
		lnColumnCount = ALEN(wzaQDD, 2)
		lnColumnCount = IIF(lnColumnCount == 0, 1, lnColumnCount)
		
		DIMENSION tmp_wzaQDD[lnRowCount, lnColumnCount]
		VFXACOPY(@wzaQDD, @tmp_wzaQDD)
	ENDIF	
ENDIF
*} V&U DL 2010-11-01
	
CleanNames(@wzaQSort)
CleanNames(@wzaQFlds)
CleanTableName(@wzaQDD) &&V&U DL 2010-11-02

CLEAR PROGRAM	&&clear out program cache
SET TEXTMERGE TO (m.wzsFileName)
SET TEXTMERGE ON NOSHOW

m.wzsDBFs =""
m.wzsFldLst =""
FOR m.i = 1 TO ALEN(wzaQFlds, 1)
	m.wzsFldLst = m.wzsFldLst + wzaQFlds[m.i] +','
ENDFOR
m.wzsFldLst = LEFT(m.wzsFldLst, LEN(m.wzsFldLst) -1)

m.wzsFilt = CleanFilter(oEngine.cWizFiltExpr)

EmitSql("*")
SET TEXTMERGE TO

FOR m.i = 1 TO ALEN(wzaQDD, 1)
	IF !EMPTY(wzaQDD[m.i,1]) AND USED(wzaQDD[m.i,1]) AND CURSORGETPROP("sourcetype", wzaQDD[m.i,1]) # 3 &&V&U DL 2010-11-02 modify
		oEngine.SetErrorOff = .T.
		 = REQUERY(wzaQDD[m.i,1])
		oEngine.SetErrorOff = .F.
	ENDIF
ENDFOR
*{ V&U DL 2010-11-02
IF llUseForCursorAdapterQuery
	IF TYPE("tmp_wzaQDD", 1) == "A"
		VFXACOPY(@tmp_wzaQDD, @wzaQDD)
	ENDIF 
	IF TYPE("tmp_wzaQFlds", 1) == "A"
		VFXACOPY(@tmp_wzaQFlds, @wzaQFlds)
	ENDIF
	IF TYPE("tmp_wzaQSort", 1) == "A"
		VFXACOPY(@tmp_wzaQSort, @wzaQSort)
	ENDIF
ENDIF
*} V&U DL 2010-11-02

IF oEngine.lIsPreview
	owizard.form1.VISIBLE = .F.
	*{ V&U DL 2010-11-02 modify 
	IF !llUseForCursorAdapterQuery
	*{ V&U MS 2009-01-09
		lcPrgName = ["] + m.wzsFileName + [""]
		COMPILE (lcPrgName)
		DO (lcPrgName)
	*} V&U MS 2009-01-09
	ELSE
		LOCAL lcQueryToExecute, lcCAAliasName, lcKeyFieldList, lcExecuteAfterCursorFill 
		LOCAL llUpdateKeyField, lnExecuteCursorFill, lcCAClassName, llDontSendUpdates, lnBufferModeOverride
		LOCAL lcNotUpdatableFieldList, loForm, loCursorAdapter 
		
		lcQueryToExecute = FILETOSTR(m.wzsFileName)
		lcQueryToExecute = STRTRAN(lcQueryToExecute, CHR(13), " ")
		lcQueryToExecute = STRTRAN(lcQueryToExecute, CHR(10), " ")
		lcQueryToExecute = STRTRAN(lcQueryToExecute, ";", " ")
		
		lcCAAliasName = "Preview"
		lcKeyFieldList = ""
		lcExecuteAfterCursorFill = ""
		llUpdateKeyField = .F.
		lnExecuteCursorFill = .T.
		lcCAClassName = "cappdataaccess"
		llDontSendUpdates = .T.
		lnBufferModeOverride = 5
		lcNotUpdatableFieldList = ""
		loForm = null
		loCursorAdapter = null
		
		loCursorAdapter = OPENTABLE(lcQueryToExecute, lcKeyFieldList, llUpdateKeyField, lcExecuteAfterCursorFill, lnExecuteCursorFill,;
										loForm, lcCAAliasName, lcCAClassName, llDontSendUpdates, lnBufferModeOverride, lcNotUpdatableFieldList)			
		IF TYPE("loCursorAdapter") == "O" AND TYPE("loCursorAdapter.Class") == "C" ;
			AND USED(lcCAAliasName)	AND ALIAS() != lcCAAliasName
			
			SELECT(lcCAAliasName)
		ENDIF 
	ENDIF
	*} V&U DL 2010-11-02 modify 

	IF m.error > 0
		oEngine.Alert(MESSAGE())
	ENDIF
	ERASE (m.wzsFilename)
	ERASE (LEFT(m.wzsFileName, RAT('.', m.wzsFilename)) +"fxp")
	IF m.error = 0
		ACTI SCREEN
		SET SKIP OF MENU _MSYSMENU .T.
		BROW LAST NOMENU
*USE
		SET SKIP OF MENU _MSYSMENU .F.
	ENDIF
	owizard.form1.VISIBLE = .T.

	#IF .F.
		LOCAL ox
		ox = CREATE("formprev")
		ox.WINDOWTYPE = 1	&&modal
		ox.AUTOCENTER = .T.
		ox.grid1.RECORDSOURCE = ALIAS()
		ox.SHOW
		RELEASE ox
	#ENDIF
	oWizard.form1.REFRESH	&&force a repaint
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: CleanFilter
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcWizFiltExpr
*
* Notes.......: Cleans the filter.
*-------------------------------------------------------
FUNCTION CleanFilter
LPARAMETERS tcWizFiltExpr

LOCAL lcFilt, lnPos, lcStr, lnPos2, lcStr2

IF EMPTY(ALLTRIM(tcWizFiltExpr))
	RETURN ""
ENDIF
lnPos = ATCC(".", tcWizFiltExpr)
IF lnPos#0
	lcStr = LEFTC(tcWizFiltExpr, lnPos -1)
	IF RAT("(", lcStr) > 0
		lcStr = SUBSTR(lcStr, RAT("(", lcStr) + 1)
	ENDIF
ENDIF
lcFilt = tcWizFiltExpr
IF ATCC(" ", lcStr)#0
	lcStr = CHRTRAN(EVAL(lcStr)," ","_")
	lcFilt = lcStr + SUBSTR(tcWizFiltExpr, lnPos)
ENDIF

lnPos2 = ATCC(" AND ", lcFilt)
IF lnPos2 = 0
	lnPos2 = ATCC(" OR ", lcFilt)
	IF lnPos2 = 0
		RETURN lcFilt
	ELSE
		lcStr = ALLTRIM(SUBSTR(lcFilt, lnPos2 + 4))
	ENDIF
ELSE
	lcStr = ALLTRIM(SUBSTR(lcFilt, lnPos2 + 5))
ENDIF
lnPos = ATCC(".", lcStr)
lcStr2 = LEFTC(lcStr, lnPos -1)
IF ATCC(" ", lcStr2)#0
	lcStr2 = CHRTRANC(EVAL(lcStr2)," ","_")
ENDIF
lcStr2 = lcStr2 + SUBSTR(lcStr, lnPos)
lcFilt = LEFTC(lcFilt, lnPos2 + 3) +" " + lcStr2
RETURN lcFilt
ENDFUNC

*-------------------------------------------------------
* Function....: CleanNames
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: taFields
*
* Notes.......: Cleans names.
*-------------------------------------------------------
FUNCTION CleanNames
LPARAMETER taFields

LOCAL i, lcStr, lnPos
LOCAL llUseForCursorAdapterQuery
LOCAL lcTableName
*{ V&U DL 2010-11-01
IF TYPE("plCursorAdapterQuery") == "L" AND !ISNULL(plCursorAdapterQuery)
	llUseForCursorAdapterQuery = plCursorAdapterQuery
ENDIF 
*} V&U DL 2010-11-01

FOR i = 1 TO ALEN(taFields, 1)
	lnPos = ATCC(".", taFields[m.i])
	IF lnPos = 0
		LOOP
	ENDIF
	
	lcStr = LEFTC(taFields[m.i], lnPos -1)
	lnEmptyStringPos = ATCC(" ", lcStr)
	*{ V&U DL 2010-11-01 modify
	IF !llUseForCursorAdapterQuery 
		IF lnEmptyStringPos = 0
			LOOP
		ENDIF
		lcStr = CHRTRANC(EVAL(lcStr)," ","_")
		taFields[m.i] = lcStr + SUBSTR(taFields[m.i], lnPos)
	ELSE
		lcTableName = GetCursorAdapterTable(lcStr)
		IF !EMPTY(lcTableName)
			taFields[m.i] = lcTableName + SUBSTR(taFields[m.i], lnPos)
		ENDIF 
	ENDIF
	*} V&U DL 2010-11-01 modify
ENDFOR
ENDFUNC

*-------------------------------------------------------
* Function....: CleanTableName
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: taArray
*
* Notes.......: Cleans table names from array with structure of wzaQDD 
*				only when create select command for cursoradapter.
*-------------------------------------------------------
FUNCTION CleanTableName
LPARAMETER taArray

EXTERNAL ARRAY taArray
	LOCAL i
	LOCAL llUseForCursorAdapterQuery
	LOCAL lcTableName
	
	IF TYPE("plCursorAdapterQuery") == "L" AND !ISNULL(plCursorAdapterQuery)
		llUseForCursorAdapterQuery = plCursorAdapterQuery
	ENDIF 
	
	IF llUseForCursorAdapterQuery 
		FOR i = 1 TO ALEN(taArray, 1)
			lcTableName = taArray[i, 1]&&Alias
			IF TYPE("lcTableName") == "C"
				lcTableName = GetCursorAdapterTable(lcTableName)
				IF !EMPTY(lcTableName)
					taArray[i, 1] = lcTableName
				ENDIF
			ENDIF
			
			lcTableName = taArray[i, 3]&&Child alias
			IF TYPE("lcTableName") == "C"
				lcTableName = GetCursorAdapterTable(lcTableName)
				IF !EMPTY(lcTableName)
					taArray[i, 3] = lcTableName
				ENDIF
			ENDIF
			
			lcTableName = taArray[i, 6]
			IF TYPE("lcTableName") == "C"
				lcTableName = GetCursorAdapterTable(lcTableName)
				IF !EMPTY(lcTableName)
					taArray[i, 6] = lcTableName
				ENDIF
			ENDIF			
		ENDFOR
	ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: EmitSql
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: prefix
*
* Notes.......: Creates the select command.
*-------------------------------------------------------
FUNCTION EmitSql
PARAMETERS prefix	&&'*' for parm block, '!' for CKSQL, '' for normal FPSQL

PRIVATE m.i, m.wzsSort, m.wzsJoin, m.wzsGrp, m.prefix, m.wzsTemp
*- added var for cursor name, take from filename
PRIVATE m.wzlOuter, m.wzscursnam
LOCAL mtemp, mdbf, aTables[1]
LOCAL llUseForCursorAdapterQuery

*{ V&U DL 2010-10-13
IF TYPE("plCursorAdapterQuery") == "L" AND !ISNULL(plCursorAdapterQuery)
	llUseForCursorAdapterQuery = plCursorAdapterQuery
ENDIF 
*} V&U DL 2010-10-13

m.wzscursnam = "Query"
m.wzsJoin =""
IF !EMPTY(wzaQGrp[1,1])
	m.wzsGrp =""
	FOR m.i = 1 TO ALEN(wzaQGrp, 1)
		IF !EMPTY(wzaQGrp[m.i,1])
			IF VAL(wzaQGrp[m.i,2]) > 1
				DO CASE
					CASE SUBSTR(wzaQGrp[m.i,2], 2) ='D'
						DO CASE
							CASE VAL(wzaQGrp[m.i,2]) = 2	&&Year
								m.wzsTemp ="YEAR(" + wzaQGrp[m.i,1] +")"
							CASE VAL(wzaQGrp[m.i,2]) = 3	&&Month
								m.wzsTemp ="MONTH(" + wzaQGrp[m.i,1] +")"
							*{V&U DL 2010-10-15 modify
							CASE VAL(wzaQGrp[m.i,2]) = 4 AND !llUseForCursorAdapterQuery &&Dow FOX
								m.wzsTemp ="DOW(" + wzaQGrp[m.i,1] + ")"
							CASE VAL(wzaQGrp[m.i,2]) = 4 AND llUseForCursorAdapterQuery &&Dow SQL
								m.wzsTemp = "DATEPART(dw, " + wzaQGrp[m.i,1] + ")"
							CASE VAL(wzaQGrp[m.i,2]) = 5 AND !llUseForCursorAdapterQuery &&Yr/mn
								m.wzsTemp ="STR(YEAR(" + wzaQGrp[m.i,1] +"),4)+'/'+" + ;
									"STR(MONTH(" + wzaQGrp[m.i,1] +"),2)"
							CASE VAL(wzaQGrp[m.i,2]) = 5 AND llUseForCursorAdapterQuery &&Yr/mn SQL
								m.wzsTemp ="CAST(DATEPART(YYYY, " + wzaQGrp[m.i,1] +") AS CHAR)+'/'+" + ;
									"CAST(DATEPART(MM, " + wzaQGrp[m.i,1] +") AS CHAR)"
							CASE VAL(wzaQGrp[m.i,2]) = 6 AND !llUseForCursorAdapterQuery &&Quarter
								m.wzsTemp ="'Q'+STR(INT((MONTH(" + wzaQGrp[m.i,1] +")-1)/3)+1,1)"
							CASE VAL(wzaQGrp[m.i,2]) = 6 AND llUseForCursorAdapterQuery &&Quarter SQL
								m.wzsTemp ="'Q' + CAST( ((DATEPART(MM, " + wzaQGrp[m.i,1] + ") - 1) / 3) + 1 AS CHAR(1))"
							CASE VAL(wzaQGrp[m.i,2]) = 7 AND !llUseForCursorAdapterQuery &&Year/Quarter
								m.wzsTemp ="STR(YEAR(" + wzaQGrp[m.i,1] +"),4)" + ;
									"+'-Q'+STR(INT((MONTH(" + wzaQGrp[m.i,1] +")-1)/3)+1,1)"
							CASE VAL(wzaQGrp[m.i,2]) = 7 AND llUseForCursorAdapterQuery &&Year/Quarter SQL
								m.wzsTemp ="STR(YEAR(" + wzaQGrp[m.i,1] +"),4)" + ;
									"+'-Q'+STR(INT((MONTH(" + wzaQGrp[m.i,1] +")-1)/3)+1,1)"				
								m.wzsTemp ="CAST(DATEPART(YYYY, " + wzaQGrp[m.i,1] + ") AS CHAR)" +;
											"+'-Q' + CAST( ((DATEPART(MM, " + wzaQGrp[m.i,1] + ") - 1) / 3) + 1 AS CHAR(1))"
							*}V&U DL 2010-10-15 modify
						ENDCASE
					CASE SUBSTR(wzaQGrp[m.i,2], 2) ='N'
						m.wzsTemp = VAL(wzaQGrp[m.i,2]) -1
						m.wzsTemp = ALLTRIM(STR(10^m.wzsTemp))
						*{ V&U DL 2010-10-15 modify
						IF !llUseForCursorAdapterQuery
							m.wzsTemp ="INT(" + wzaQGrp[m.i,1] +"/" + m.wzsTemp + ;
								") * " + m.wzsTemp
						ELSE
							m.wzsTemp ="CAST(" + wzaQGrp[m.i,1] +"/" + m.wzsTemp + ;
								" AS INT) * " + m.wzsTemp							
						ENDIF 
						*} V&U DL 2010-10-15 modify
					OTHERWISE	&&must be Char
						m.wzsTemp ="LEFT(" + wzaQGrp[m.i,1] +"," + ;
							STR(VAL(wzaQGrp[m.i,2]) -1, 1) +')'
				ENDCASE
*- put CR & LF together
				m.wzsFldLst = m.wzsFldLst +",;" + ;
					CHR(13) + CHR(10) + m.prefix + CHR(9) + CHR(9) + ;
					m.wzsTemp +" AS GRP" + CHR(m.i + ASC('0'))
				m.wzsGrp = m.wzsGrp +",GRP" + CHR(m.i + ASC('0'))
			ELSE
				m.wzsGrp = m.wzsGrp +"," + wzaQGrp[m.i,1]
			ENDIF && wzaQGrp[m.i,2]>1
		ENDIF && !EMPTY(wzaQGrp[m.i,1])
	ENDFOR &&* m.i=1 TO ALEN(wzaQGrp,1)
	m.wzsGrp = SUBSTR(m.wzsGrp, 2)
ENDIF && !EMPTY(wzaQGrp[1,1])

* create FROM clause, no joins
DIMENSION aTables[ALEN(wzaQDD,1)]
m.wzsDBFs = ""
FOR m.i = 1 TO ALEN(wzaQDD, 1)
	aTables[m.i] = ""
	DO CASE
		CASE m.prefix ='!'
			m.wzsDBFs = m.wzsDBFs + wzaQDD[m.i,6]	&&Just CS name
		CASE m.prefix ='*' AND !llUseForCursorAdapterQuery && V&U DL 2010-11-02 modify
			mtemp = CURSORGETPROP("database", wzaQDD[m.i,1])
			m.mdbf =""
			IF !EMPTY(m.mtemp)  &&free table
				m.mdbf = m.mdbf + oEngine.JUSTSTEM(m.mtemp) +'!'
			ENDIF
			m.mdbf = m.mdbf + CURSORGETPROP("sourcename", wzaQDD[m.i,1])
			IF AT(' ', m.mdbf) > 0
				m.mdbf ='"' + m.mdbf +'"'
			ENDIF
			m.wzsDBFs = m.wzsDBFs + m.mdbf

* save table name for JOIN condition
			aTables[m.i] = m.mdbf
		CASE m.prefix ='*' AND llUseForCursorAdapterQuery && V&U DL 2010-11-02 modify
			m.mdbf = wzaQDD[m.i,1]
			aTables[m.i] = m.mdbf			
		OTHERWISE
			m.wzsDBFs = m.wzsDBFs + wzaQDD[m.i,1]
	ENDCASE
	m.wzsDBFs = m.wzsDBFs +' ' + wzaQDD[m.i,1]	&&local alias
	m.wzsDBFs = m.wzsDBFs +','
ENDFOR
m.wzsDBFs = LEFT(m.wzsDBFs, LEN(m.wzsDBFs) -1)

* create FROM clause with join clause
m.wzlOuter = ALEN(wzaQDD, 1) = 2 AND ;
	(VAL(wzaQDD[2,5]) > 0 OR OEngine.nJoinOption > 1)		&&Outer Join for 2 tables

IF m.wzlOuter
* Join is part of the FROM clause
	m.joinOper = IIF(OEngine.nJoinOption = 2, "LEFT OUTER JOIN", ;
		IIF(OEngine.nJoinOption = 3, "RIGHT OUTER JOIN", "FULL OUTER JOIN"))

	m.wzsJoin = aTables[1] + " " + wzaQDD[1,1] + ;
		IIF(ATCC(" ", wzaQDD[1,1])#0," " + CHRTRANC(EVAL(wzaQDD[1,1])," ","_"),"") + ;
		" " + m.joinOper + " " + aTables[2] + " " + wzaQDD[2,1] + ;
		IIF(ATCC(" ", wzaQDD[2,1])#0," " + CHRTRANC(EVAL(wzaQDD[2,1])," ","_"),"") + ;
		" ON " + JoinCond(wzaQDD[2,3], wzaQDD[2,4]) + " = " + JoinCond(wzaQDD[2,1], wzaQDD[2,2])

ELSE
	IF ALEN(wzaQDD, 1) < 2
		m.wzajoin = ''
	ELSE
		m.wzsJoin = BuildInnerJoin(@wzaQDD, @aTables, 2)
	ENDIF
ENDIF

* create the Sql query
	\SELECT

IF !EMPTY(wzaQSort[1])
	IF OEngine.nAmount != -1
			\\ TOP << TRIM(STR(OEngine.nAmount, 3)) >>
		IF OEngine.nPortion = 1
				\\ PERCENT 
		ENDIF
	ENDIF
ENDIF

*DO OutStr WITH m.Prefix+CHR(9)+CHR(9),m.wzsFldLst,0,1,','
OutStr(m.Prefix + CHR(9) + CHR(9), m.wzsFldLst, 0, 1,',')

IF !EMPTY(m.wzsJoin)
		\    FROM << m.wzsJoin >>
	m.wzsJoin = ""
ELSE
		\    FROM << m.wzsDBFs >>
ENDIF

IF !EMPTY(m.wzsJoin)
	*{ V&U DL 2010-10-13 modify
	IF !llUseForCursorAdapterQuery
		\\;
	ENDIF
	*} V&U DL 2010-10-13 modify
		\    WHERE << m.wzsJoin >>
ENDIF && !EMPTY(m.wzsJoin)
IF !EMPTY(m.wzsFilt)
	*{ V&U DL 2010-10-13 modify
	IF !llUseForCursorAdapterQuery
		\\;
	ENDIF
	*} V&U DL 2010-10-13 modify
		\    << IIF(EMPTY(m.wzsJoin),"WHERE "," AND ") >>
		\\(<< m.wzsFilt >>)
ENDIF && !EMPTY(wzaQFilt[1,1])

IF !EMPTY(wzaQSort[1])
	m.wzsSort =""

	FOR m.i = 1 TO ALEN(wzaQSort, 1)
		m.wzsSort = IIF(m.i > 1, m.wzsSort +',',"") + wzaQSort[m.i]
		IF m.i = 1 AND m.wziQSortA = 2
			m.wzsSort = m.wzsSort + " DESC "
		ENDIF
	ENDFOR &&* m.i=1 TO ALEN(wzaQSort,1)
	*{ V&U DL 2010-10-13 modify
	IF !llUseForCursorAdapterQuery
		\\;
	ENDIF
	*} V&U DL 2010-10-13 modify
		\    ORDER BY << m.wzsSort >>

ENDIF && !EMPTY(wzaQSort[1])
IF !EMPTY(wzaQGrp[1,1])
	*{ V&U DL 2010-10-13 modify
	IF !llUseForCursorAdapterQuery
		\\;
	ENDIF
	*} V&U DL 2010-10-13 modify
		\    GROUP BY
		\\ << m.wzsGrp >>
ENDIF
IF oEngine.lIsPreview AND !llUseForCursorAdapterQuery&&V&U DL 2010-10-13 modify && AND m.QWizType#'R'
		\\;
		\    INTO CURSOR "Preview" && IK 20081003
ENDIF
RETURN
ENDFUNC

*-------------------------------------------------------
* Function....: JoinCond
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: wzsArea, wzsFld
*
* Notes.......: Creates a join condition.
*-------------------------------------------------------
FUNCTION JoinCond
PARAMETERS m.wzsArea, m.wzsFld

IF ATCC(" ", m.wzsArea)#0
	m.wzsArea = CHRTRAN(EVAL(m.wzsArea)," ","_")
ENDIF
IF m.wzsFld ='('
	RETURN SUBSTR(m.wzsFld, 2, LEN(m.wzsFld) -2)
ELSE
	RETURN m.wzsArea +"." + m.wzsFld
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: BuildInnerJoin
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: aJoinInfo, aTables, index
*
* Notes.......: Builds Inner Joins.
*-------------------------------------------------------
FUNCTION BuildInnerJoin
PARAMETERS aJoinInfo, aTables, INDEX
DIMENSION aJoinInfo[ALEN(aJoinInfo,1),ALEN(aJoinInfo,2)]

IF ALEN(aJoinInfo, 1) == INDEX
* end of recursion, build join string for <index> level
	m.lcJoinStr = aTables[index-1] + " " + aJoinInfo[index-1,1] + " INNER JOIN " + ;
		aTables[index] + " " + aJoinInfo[index,1]
ELSE
* build join string from <index> level down
	m.lcJoinStr = aTables[index-1] + " " + aJoinInfo[index-1,1] + " INNER JOIN " + ;
		"(" + BuildInnerJoin(@aJoinInfo, @aTables, INDEX + 1) + ")"
ENDIF

* build join condition
m.lcJoinStr = m.lcJoinStr + " ON " + ;
	JoinCond(aJoinInfo[index,3], aJoinInfo[index,4]) + " = " + ;
	JoinCond(aJoinInfo[index,1], aJoinInfo[index,2])

RETURN m.lcJoinStr
ENDFUNC

*-------------------------------------------------------
* Function....: OutStr
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: wzsPref, wzsStr, wzii, wzsMode, wzsSep
*
* Notes.......: Watching for Quotes
*-------------------------------------------------------
FUNCTION OutStr
*OutStr watching for Quotes
PARAMETERS m.wzsPref, m.wzsStr, m.wzii, m.wzsMode, m.wzsSep
PRIVATE m.wzii, m.wziLen

LOCAL llUseForCursorAdapter
*{ V&U DL 2010-10-13
IF TYPE("plCursorAdapterQuery") == "L" AND !ISNULL(plCursorAdapterQuery)
	llUseForCursorAdapter = plCursorAdapterQuery
ENDIF 
*} V&U DL 2010-10-13

DO WHILE LENC(m.wzsStr) > 0
	m.wziLen = IIF(m.wzlTesting, 70, 70)
	IF TYPE("m.wzsMode") ='N'
		DO CASE
			CASE m.wzsMode = 0	&& sequential "* FIELDS =" or "SET FIELDS TO "
				\ << m.wzsPref >>
			CASE m.wzsMode = 1	&& line continuation, with ';' at the end
				IF m.wzii > 0
* The following line and others have 4 spaces on it... For 3.0a they had a Tab char
* which causes some back ends to break
					\
				ENDIF
		ENDCASE
		IF LENC(m.wzsStr) > m.wziLen AND AT_C(m.wzsSep, m.wzsStr) > 0
			m.wziLen = RATC(m.wzsSep, LEFTC(m.wzsStr, m.wziLen))
			IF m.wziLen = 0
				m.wziLen = ATC(m.wzsSep, m.wzsStr)
			ENDIF
				\\ << LEFTC(m.wzsStr, m.wziLen - (1 - m.wzsMode)) >>
		ELSE
				\\ << m.wzsStr >>
		ENDIF
		IF m.wzsMode = 1 AND !llUseForCursorAdapter	&&trailing ; for line continuation
				\\;
		ENDIF
	ELSE
			\ << m.wzsPref >>=
		IF m.wzii > 0
				\\m. << m.wzsPref >> +
		ENDIF
		IF "'" $ LEFTC(m.wzsStr, m.wziLen)
			IF '"' $ LEFTC(m.wzsStr, AT_C("'", m.wzsStr) -1)
				m.wziLen = AT_C('"', m.wzsStr)
					\\'<<LEFTC(m.wzsStr,m.wziLen)>>'
			ELSE
				m.wziLen = ATC("'", m.wzsStr)
					\\"<<LEFTC(m.wzsStr,m.wziLen)>>"
			ENDIF
		ELSE
				\\'<<LEFTC(m.wzsStr,m.wziLen)>>'
		ENDIF
	ENDIF
	m.wzsStr = SUBSTRC(m.wzsStr, m.wziLen + 1)
	m.wzii = m.wzii + 1
ENDDO
ENDFUNC

*-------------------------------------------------------
* Function....: SpreadIt
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: mObj
*
* Notes.......:
*-------------------------------------------------------
FUNCTION SpreadIt
PARAMETERS mObj
LOCAL i, cTable, j, k
IF TYPE("mObj") == "O"
	DIMENSION wzaQFlds[ALEN(mObj.aSelections, 1)]
	FOR i = 1 TO mObj.lstRight.LISTCOUNT
		k = mObj.lstRight.INDEXTOITEMID(m.i)
		FOR j = 1 TO ALEN(mObj.aSelections, 1)
			IF mObj.aSelections[m.j, 2] = m.k
				wzaQFlds[m.i] = ALLTRIM(mObj.aSelections[m.j, 1])
				EXIT
			ENDIF
		ENDFOR
	ENDFOR
ENDIF
IF !EMPTY(wzaQFlds[1])
	FOR i = 1 TO ALEN(wzaQflds)
		wzaQFlds[m.i] = TRIM(wzaQFlds[m.i])
		cTable = wzaQFlds[m.i]
		cTable = LEFTC(m.cTable, AT_C('.', m.cTable) - 1)
		IF ASCANNER(@wzaQDD, m.cTable, 1) = 0
			IF !EMPTY(wzaQDD[1, 1])
				DIMENSION wzaQDD[ALEN(wzaQDD, 1) + 1, 6]
			ENDIF
			wzaQDD[ALEN(wzaQDD, 1), 1] = ALLTRIM(m.cTable)
			wzaQDD[ALEN(wzaQDD, 1), 2] = ""		&&Child expr for relation
			wzaQDD[ALEN(wzaQDD, 1), 3] = ""		&& parent alias
			wzaQDD[ALEN(wzaQDD, 1), 4] = ""		&& parent expr for relation
			wzaQDD[ALEN(wzaQDD, 1), 5] = IIF(oEngine.cOuterJoin = 0, '0', '1')		&& outer join
			wzaQDD[ALEN(wzaQDD, 1), 6] = IIF(oEngine.nConnectHandle # 0, m.cTable, DBF(ALIAS(ALLTRIM(m.cTable))))
		ENDIF
	ENDFOR
ENDIF
ENDFUNC

*-------------------------------------------------------
* Function....: AScanner
* Called by...: Query wizard
* Abstract....:
*
* Returns.....:
*
* Parameters..: aArray, cSearch, nCol
*
* Notes.......:
*-------------------------------------------------------
FUNCTION AScanner
PARAMETERS aArray, cSearch, nCol
EXTERNAL ARRAY aArray
LOCAL i
FOR i = 1 TO ALEN(aArray, 1)
	IF TYPE("aArray[m.i, 1]") == "C" AND UPPER(aArray[m.i, 1]) == UPPER(cSearch)
		RETURN m.i
	ENDIF
ENDFOR
RETURN 0
ENDFUNC

*-------------------------------------------------------
* Function....: MoveFile
* Called by...:
* Abstract....:
*
* Returns.....:
*
* Parameters..: tcSourceFilePath, tcDestinationFilePath
*
* Notes.......:
*-------------------------------------------------------
FUNCTION vfxMoveFile
LPARAMETERS tcSourceFilePath, tcDestinationFilePath
DECLARE INTEGER MoveFile IN Kernel32.DLL ;
	STRING lpExistingFileName, ;
	STRING  lpNewFileName
LOCAL lnRes
lnRes = MoveFile(tcSourceFilePath, tcDestinationFilePath)
CLEAR DLLS "MoveFile"
RETURN (lnRes <> 0)
ENDFUNC


*-------------------------------------------------------
* Function....: GetFolderPathParameter
* Called by...: GetFolderPath
* Abstract....: Transform GetFolderPath parameter
*				depending on OS
* Returns.....:	Transformed Parameter Value
*
* Parameters..: tvFolder
*
* Notes.......:
*-------------------------------------------------------

FUNCTION GetFolderPathParameter
LPARAMETERS tvFolder

IF !(VAL(OS(3)) < 6)
	DO CASE
		CASE tvFolder = CSIDL_WINDOWS
			tvFolder = FOLDERID_WINDOWS
		CASE tvFolder = CSIDL_COMMON_APPDATA
			tvFolder = 	FOLDERID_PROGRAMDATA
		CASE tvFolder = CSIDL_APPDATA
			tvFolder = 	FOLDERID_ROAMINGAPPDATA
		CASE tvFolder = CSIDL_PERSONAL
			tvFolder = FOLDERID_DOCUMENTS
		CASE tvFolder = CSIDL_MYPICTURES
			tvFolder = FOLDERID_PICTURES
		CASE tvFolder = CSIDL_PROGRAM_FILES_COMMON
			tvFolder = 	FOLDERID_PROGRAMFILESCOMMONX86
		OTHERWISE
* Parameter will be returned without change
	ENDCASE
ENDIF

RETURN tvFolder
ENDFUNC

*-------------------------------------------------------
* Function....: GetRibbonPicture
* Called by...: Ribbon bar
* Abstract....: Returns the picture according ColorScheme
*				and ColorDepth
* Returns.....:	Picture name
*
* Parameters..: tcPictureName
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetRibbonPicture
LPARAMETERS tcPictureName, tcColorSchemePrefix, tnColorDepth

LOCAL lcPicture
lcPicture = ""
IF TYPE("goProgram.Class") == "C" AND !EMPTY(tcPictureName)
	lcPicture = FULLPATH(tcPictureName)

	*{V&U MS 2013-07-25 6225
	IF !EMPTY(goProgram.cColorSchemePrefix)
		IF FILE(ADDBS(JUSTPATH(lcPicture)) + ADDBS(goProgram.cColorSchemePrefix) + ;
				ALLTRIM(goProgram.cColorSchemePrefix) + "_" + JUSTSTEM(lcPicture) + "." + JUSTEXT(lcPicture))
			lcPicture = ADDBS(JUSTPATH(lcPicture)) + ADDBS(goProgram.cColorSchemePrefix) + ;
				ALLTRIM(goProgram.cColorSchemePrefix) + "_" + JUSTSTEM(lcPicture) + "." + JUSTEXT(lcPicture)
		ENDIF 
	ENDIF
	*}V&U MS 2013-07-25
	
	*{V&U MS 2014-12-10 6733
	IF TYPE("goUser.oCustomizeSettings.nMouseTouchBehavior") == "N" AND goUser.oCustomizeSettings.nMouseTouchBehavior = 1
		IF FILE(ADDBS(JUSTPATH(lcPicture)) + JUSTSTEM(lcPicture) + "_touch" + "." + JUSTEXT(lcPicture))
			lcPicture = ADDBS(JUSTPATH(lcPicture)) + JUSTSTEM(lcPicture) + "_touch" + "." + JUSTEXT(lcPicture)
		ENDIF 	
	ENDIF 
	*}V&U MS 2014-12-10
	
	IF goProgram.nColorDepth <= 8 AND FILE(ADDBS(JUSTPATH(lcPicture)) + JUSTSTEM(lcPicture) + "_8bit." + JUSTEXT(lcPicture))
		lcPicture = ADDBS(JUSTPATH(lcPicture)) + JUSTSTEM(lcPicture) + "_8bit." + JUSTEXT(lcPicture)
	ENDIF
ENDIF
RETURN lcPicture
ENDFUNC

*-------------------------------------------------------
* Function....: GetRibbonColor
* Called by...: Ribbon bar
* Abstract....: Return the color according ColorScheme,
*				ColorDepth and ControlProperty
* Returns.....:	Color
*
* Parameters..: tcCommentID, tcPropertyName, tnColor
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetRibbonColor
LPARAMETERS tcCommentID, tcPropertyName, tnColor, loError AS EXCEPTION

LOCAL lnColor, lnAlias
lnColor = -1
*{V&U MS 2013-02-04 6086
IF TYPE("goProgram.Class") == "C"
*}V&U MS 2013-02-04
	lnAlias = SELECT()
	lnColor = tnColor

*{ V&U MS 27.10.2008
* Moved to DATA folder
	IF !USED("vfxThemes")
		IF FILE("VfxThemes.dbf")
			USE VfxThemes IN 0 SHARED
		ELSE
			IF FILE("DATA\VfxThemes.dbf")
				USE DATA\VfxThemes IN 0 SHARED
			ENDIF
		ENDIF
	ENDIF
*} V&U MS 27.10.2008
	SELECT vfxThemes
	LOCATE FOR CommentID = tcCommentID AND ALLTRIM(LOWER(ThemeName)) == ALLTRIM(LOWER(goProgram.cColorSchemePrefix))
	IF FOUND()
		IF goProgram.nColorDepth <= 8
			tcPropertyName = ALLTRIM(tcPropertyName) + "8"
		ENDIF
*{ V&U MS 2008-11-10
		TRY
			lnColor = EVALUATE("RGB(" + &tcPropertyName. + ")")
		CATCH TO loError
* Do nothing
		ENDTRY
*} V&U MS 2008-11-10
	ENDIF
	SELECT (lnAlias)
ENDIF
RETURN lnColor
ENDFUNC

*{ V&U MS 2008-11-27
DEFINE CLASS GridHeader AS HEADER
	NAME = "Header"
	PROCEDURE DBLCLICK
	THIS.PARENT.PARENT.onSetOrder(THIS.PARENT)
	ENDPROC
	PROCEDURE MOUSEDOWN
	LPARAMETERS nButton, nShift, nXCoord, nYCoord
	IF TYPE("ThisForm.Sys2MouseDown") # "U"
		THISFORM.Sys2MouseDown = SYS(2)
	ENDIF

	IF nShift = 2
		NODEFAULT
		THIS.PARENT.PARENT.onSetOrder(THIS.PARENT, .F., .T.)
	ENDIF
	ENDPROC
	PROCEDURE MOUSEUP
	LPARAMETERS nButton, nShift, nXCoord, nYCoord

	IF TYPE("ThisForm.Sys2MouseUp") # "U" AND TYPE("ThisForm.Sys2MouseDown") # "U" AND nButton = 1 AND nShift = 0
		THISFORM.Sys2MouseUp = SYS(2)
		IF VAL(THISFORM.Sys2MouseUp) - VAL(THISFORM.Sys2MouseDown) < 0.5
			THIS.PARENT.PARENT.onSetOrder(THIS.PARENT)
		ENDIF
	ENDIF
	ENDPROC
ENDDEFINE

DEFINE CLASS GridTextBox AS TEXTBOX
	MARGIN = 0
	BORDERSTYLE = 0
	READONLY = .T.
	PROCEDURE DBLCLICK
	IF !THISFORM.lEmpty
		IF !(TYPE("This.ControlSource") $ "MG")
			THIS.PARENT.PARENT.onKeyEnter()
			NODEFAULT
		ENDIF
	ENDIF
	ENDPROC
	PROCEDURE KEYPRESS
	LPARAMETERS nKeyCode, nShiftAltCtrl
	WITH THIS.PARENT.PARENT
		.oControl = THIS
		IF .onKeyPress(nKeyCode, nShiftAltCtrl)
			NODEFAULT
		ENDIF
		.oControl = .NULL.
	ENDWITH
	ENDPROC
	PROCEDURE MOUSEDOWN
	LPARAMETERS nButton, nShift, nXCoord, nYCoord
	THIS.PARENT.PARENT.OLEDRAG(.T.)
	ENDPROC
ENDDEFINE

DEFINE CLASS GridColumn AS COLUMN
	HEADERCLASSLIBRARY = "vfxFunc.prg"
	HEADERCLASS = "GridHeader"
	READONLY = .T.
	ADD OBJECT Text1 AS GridTextBox
ENDDEFINE
*} V&U MS 2008-11-27

*-------------------------------------------------------
* Function....: WriteLogInfo
* Called by...: Client database update
* Abstract....: Writes info in log file
* Returns.....:
*
* Parameters..: tcLogInfo, tcFileName, tlUseUTCTime
*
* Notes.......:
*-------------------------------------------------------
PROCEDURE WriteLogInfo

LPARAMETERS tcLogInfo, tcFileName, tlUseUTCTime

LOCAL ltDateTime
ltDateTime = DATETIME()
IF tlUseUTCTime
	 = UTCTime(@ltDateTime)
ENDIF
TRY
	STRTOFILE(TRANSFORM(ltDateTime) + " - " + ALLTRIM(tcLogInfo) + CHR(13) + CHR(10), tcFileName, 1)
CATCH
* Do nothing. Log File cannot be opened.
ENDTRY
ENDPROC

*-------------------------------------------------------
* Function....: CopyDbfToDbc
* Called by...:
* Abstract....: Copy dbf to dbc with data. Triggers and RI not copied
* Returns.....:
*
* Parameters..: tcSourceDbf, tcDestinationDbc
*
* Notes.......:
*-------------------------------------------------------
FUNCTION CopyDbfToDbc
LPARAMETERS tcSourceDbf, tcDestinationDbc

LOCAL 	llRes, llCloseDatabase, lcSetDatabase, lnFieldCount, lcCreateTableStr, ;
	lcAlterString, lnRow, lnFieldNo, lcFieldName, lcFieldType, lcFieldDef, ;
	lcField, lnIndexes, lcIndexExpr, lnIndex, lnSelect

llRes = .T.
lnSelect = SELECT()
IF EMPTY(tcSourceDbf) OR EMPTY(tcDestinationDbc)
	RETURN .F.
ENDIF

IF ADIR(laDir, tcSourceDbf) = 0 OR ADIR(laDir, tcDestinationDbc) = 0
	RETURN .F.
ENDIF

TRY
	USE (tcSourceDbf) IN 0 SHARED ALIAS SOURCE
CATCH
	llRes = .F.
ENDTRY

IF llRes
	lcSetDatabase = SET("Database")
	ADATABASES(laDatabases)
	lnRow = ASCAN(laDatabases, ALLTRIM(UPPER(FULLPATH(tcDestinationDbc))), 1, ALEN(laDatabases, 1), 2, 15)
	IF lnRow = 0
		TRY
			OPEN DATABASE (tcDestinationDbc) EXCLUSIVE
			llCloseDatabase = .T.
		CATCH
			llRes = .F.
		ENDTRY
	ENDIF
ENDIF

IF !llRes
	IF USED("Source")
		USE IN SOURCE
	ENDIF
	RETURN .F.
ENDIF

* Creates a Create table string
IF USED("Source")
	lnFieldCount = AFIELDS(laFields, "Source")
	lcCreateTableStr = [CREATE Table ] + JUSTSTEM(tcSourceDbf) + [ (]
	lcAlterString = []
	FOR lnFieldNo = 1 TO lnFieldCount
		lcFieldName = laFields[lnFieldNo, 1]
		lcFieldType = laFields[lnFieldNo, 2]
		DO CASE
			CASE (lcFieldType $ "CQV")
				lcFieldDef = lcFieldType + [(] + TRANSFORM(laFields[lnFieldNo, 3]) + [)]
			CASE (lcFieldType $ "FN")
				lcFieldDef = lcFieldType + [(] + TRANSFORM(laFields[lnFieldNo, 3]) + [, ] + TRANSFORM(laFields[lnFieldNo, 4]) + [)]
			CASE lcFieldType == "B"
				lcFieldDef = lcFieldType + [(] + TRANSFORM(laFields[lnFieldNo, 4]) + [)]
			CASE lcFieldType == "I"
				IF !EMPTY(laFields[lnFieldNo, 18])
* Creates an Alter table string. Execute after append data
					lcAlterString = [ALTER TABLE ] + JUSTSTEM(tcSourceDbf) + ;
						[ ALTER COLUMN ] + lcFieldName + [ ] + lcFieldType + ;
						[ AUTOINC NEXTVALUE ] + TRANSFORM(laFields[lnFieldNo, 17]) + ;
						[ STEP ] + TRANSFORM(laFields[lnFieldNo, 18])
				ENDIF
				lcFieldDef = lcFieldType
			OTHERWISE
				lcFieldDef = lcFieldType
		ENDCASE

		lcField = lcFieldName + " " + lcFieldDef + IIF(laFields[lnFieldNo, 5], " NULL ", " ")
		lcCreateTableStr = lcCreateTableStr + lcField + IIF(lnFieldNo < lnFieldCount, ",", ")")
	ENDFOR
ENDIF

* Creates Indexes.
SELECT SOURCE
lnIndexes = ATAGINFO(laIndexes)
lcIndexExpr = []
FOR lnIndex = 1 TO lnIndexes
	lcIndexExpr = lcIndexExpr + [INDEX ON ] + ALLTRIM(laIndexes[lnIndex, 3]) + [ TAG ] + ALLTRIM(laIndexes[lnIndex, 1]) + ;
		[ COLLATE "] + ALLTRIM(laIndexes[lnIndex, 6]) + ["] + ;
		IIF(!EMPTY(laIndexes[lnIndex, 4]), [ FOR ] + ALLTRIM(laIndexes[lnIndex, 4]), []) + [ ] + ;
		IIF(DESCENDING(lnIndex, "Source"), [DESCENDING], [ASCENDING]) + [ ] + ;
		ICASE(PRIMARY(lnIndex, "Source"), [UNIQUE], CANDIDATE(lnIndex, "Source"), [CANDIDATE], []) + ;
		[ ADDITIVE ] + CHR(13) + CHR(10)
ENDFOR

ADATABASES(laDatabases)
lnRow = ASCAN(laDatabases, ALLTRIM(UPPER(FULLPATH(tcDestinationDbc))), 1, ALEN(laDatabases, 1), 2, 15)
IF lnRow > 0
	SET DATABASE TO (laDatabases[lnRow, 1])
ENDIF

TRY
	EXECSCRIPT(lcCreateTableStr)
CATCH
	llRes = .F.
ENDTRY

IF llRes
	APPEND FROM DBF("Source")
	USE IN SOURCE
	IF !EMPTY(lcAlterString)
		TRY
			EXECSCRIPT(lcAlterString)
		CATCH
			llRes = .F.
		ENDTRY
	ENDIF
	IF llRes AND !EMPTY(lcIndexExpr)
		TRY
			EXECSCRIPT(lcIndexExpr)
		CATCH
			llRes = .F.
		ENDTRY
	ENDIF
ENDIF
IF !EMPTY(llCloseDatabase)
	CLOSE DATABASES
ENDIF
IF !EMPTY(lcSetDatabase)
	SET DATABASE TO (lcSetDatabase)
ENDIF
IF !EMPTY(lnSelect)
	SELECT (lnSelect)
ENDIF

RETURN llRes
ENDFUNC

*-------------------------------------------------------
* Class.......: WindowsIniFile
* Called by...: cRegisterProcessing.CheckAndCreateIniEntry()
* Abstract....: Read/Write in Ini file
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
DEFINE CLASS WindowsIniFile AS CUSTOM
	lLoadedDLLs = .F.
	lHadError = .F.

	PROCEDURE GetINISection
	PARAMETERS taSections, tcSection, tcINIFile
	LOCAL lcINIValue, lnTotEntries, lnI, lnLastPos, lnTmpPos
	lcINIValue = ""
	IF TYPE("tcINIFile") # "C"
		tcINIFile = ""
	ENDIF

	IF THIS.GetINIEntry(@lcINIValue, tcSection, 0, tcINIFile) # ERROR_SUCCESS
		RETURN ERROR_FAILINI
	ENDIF

	lnTotEntries = OCCURS(CHR(0), lcINIValue)
	DIMENSION taSections[lnTotEntries]
	lnLastPos = 1
	FOR lnI = 1 TO lnTotEntries
		lnTmpPos = AT(CHR(0), lcINIValue, lnI)
		taSections[lnI] = SUBSTR(lcINIValue, lnLastPos, lnTmpPos - lnLastPos)
		lnLastPos = lnTmpPos + 1
	ENDFOR

	RETURN ERROR_SUCCESS
	ENDPROC

	PROCEDURE GetINIEntry
	LPARAMETER tcValue, tcSection, tcEntry, tcINIFile

* Get entry from INI file
	LOCAL lcBuffer, lnBufSize, lnErrNum, lnTotParms

	lnTotParms = PARAMETERS()

* Load API functions
	lnErrNum = THIS.LoadINIFuncs()

	IF lnErrNum # ERROR_SUCCESS
		RETURN lnErrNum
	ENDIF

* Parameter checks here
	IF lnTotParms < 3
		tcEntry = 0
	ENDIF

	lcBuffer = SPACE(2000)

	IF EMPTY(tcINIFile)
* WIN.INI file
		lnBufSize = GetWinINI(tcSection, tcEntry, "", @lcBuffer, LEN(lcBuffer))
	ELSE
* Private INI file
		lnBufSize = GetPrivateINI(tcSection, tcEntry, "", @lcBuffer, LEN(lcBuffer), tcINIFile)
	ENDIF

	IF lnBufSize = 0 &&could not find entry in INI file
		RETURN ERROR_NOINIENTRY
	ENDIF

	tcValue = LEFT(lcBuffer, lnBufSize)

** All is well
	RETURN ERROR_SUCCESS
	ENDPROC

	PROCEDURE WriteINIEntry
	LPARAMETER tcValue, tcSection, tcEntry, tcINIFile

* Get entry from INI file
	LOCAL lnErrNum

* Load API functions
	lnErrNum = THIS.LoadINIFuncs()
	IF lnErrNum # ERROR_SUCCESS
		RETURN lnErrNum
	ENDIF

	IF EMPTY(tcINIFile)
* WIN.INI file
		lnErrNum = WriteWinINI(tcSection, tcEntry, tcValue)
	ELSE
* Private INI file
		lnErrNum = WritePrivateINI(tcSection, tcEntry, tcValue, tcINIFile)
	ENDIF

** All is well
	RETURN IIF(lnErrNum = 1, ERROR_SUCCESS, lnErrNum)
	ENDPROC

	PROCEDURE LoadINIFuncs
* Loads funtions needed for reading INI files
	IF THIS.lLoadedDLLs
		RETURN ERROR_SUCCESS
	ENDIF

	DECLARE INTEGER GetPrivateProfileString IN Win32API ;
		AS GetPrivateINI STRING, STRING, STRING, STRING, INTEGER, STRING

	IF THIS.lHadError && error loading library
		RETURN -1
	ENDIF

	DECLARE INTEGER GetProfileString IN Win32API ;
		AS GetWinINI STRING, STRING, STRING, STRING, INTEGER

	DECLARE INTEGER WriteProfileString IN Win32API ;
		AS WriteWinINI STRING, STRING, STRING

	DECLARE INTEGER WritePrivateProfileString IN Win32API ;
		AS WritePrivateINI STRING, STRING, STRING, STRING

	THIS.lLoadedDLLs = .T.

* Need error check here
	RETURN ERROR_SUCCESS
	ENDPROC
ENDDEFINE

*-------------------------------------------------------
* Function....: UNZIPProgressOnStartup
* Called by...: ExtractFromArchiveOnStartup
*
* Abstract....: callback function for ExtractZipArchive
*
* Returns.....:
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
FUNCTION UNZIPProgressOnStartup
LPARAMETERS tcCurrentOperatedFile, nState, nArchiveFilesSize, nUnZIPedFilesSize

* check state
DO CASE
CASE nState = 1
* File already exist -> Overwrite it
	RETURN 2	
		
CASE nState = 5 OR nState = 6
* End extraction
	plOperationSucceeded  = (nState = 5)

CASE nState = 4
* File not extracted from archive
	plOperationSucceeded  = .F.
	RETURN 0

CASE nState = 3 
* File extracting finished
	IF LOWER(JUSTEXT(tcCurrentOperatedFile)) == 'ba_'
		RENAME (tcCurrentOperatedFile) TO FORCEEXT(tcCurrentOperatedFile, 'bat')
	ENDIF

ENDCASE
RETURN 2 && CONTINUE
ENDFUNC

*-------------------------------------------------------
* Function....: GetSQLDrivers
* Called by...: SQLDrivers combobox
*
* Abstract....: enumerates installed ODBC drivers
*
* Returns.....: semicolon separated list of installed drivers
*
* Parameters..: tcData by ref - contains list of drivers
*
* Notes.......:
*-------------------------------------------------------
FUNCTION GetSQLDrivers
LPARAMETERS tcData as String


		LOCAL henv, fDirection, szDriverDesc, cbDriverDescMax
		LOCAL pcbDriverDesc, szDriverAttributes, cbDrvrAttrMax, pcbDrvrAttr
		LOCAL szDSN, cbDSNMax, pcbDSN, szDescription, cbDescriptionMax, pcbDescription
		LOCAL lcODBCDLLFile AS Character 
		STORE "ODBC32.DLL" TO lcODBCDLLFile
		
		*{ HC BB 2016-02-29, 6924
		DECLARE Short SQLDrivers IN (lcODBCDLLFile) ;
			Long    henv, Integer fDirection, ;
			String @ szDriverDesc, Integer cbDriverDescMax, Integer pcbDriverDesc, ;
			String @ szDriverAttributes, Integer cbDrvrAttrMax, Integer pcbDrvrAttr
		*} HC BB 2015-02-29


		LOCAL nODBCEnv,nRetVal,dsn,dsndesc,mdsn,mdesc

		* Get ODBC environment handle
		nODBCEnv=VAL(SYS(3053))
		tcData = ""
		* -- Possible error messages
		* 527 "cannot load odbc library"
		* 528 "odbc entry point missing"
		* 182 "not enough memory"

		IF INLIST(nODBCEnv, 527, 528, 182)
			* Failed
			RETURN -113	&& failed to get ODBC environment
		ENDIF


		DO WHILE .T.
			dsn = space(200)
			dsndesc = space(200)
			mdsn = 0
			mdesc = 0

			* Return drivers or data sources
			nRetVal = SQLDrivers(m.nODBCEnv, 1, @dsn, 200, @mdsn, @dsndesc, 200, @mdesc)

			DO CASE
			CASE m.nRetVal = 100
				nRetVal = 0
				EXIT
			CASE m.nRetVal <> 0 AND m.nRetVal # 1 
				EXIT
			OTHERWISE
				IF ("SQLLEVEL=1" $ UPPER(m.dsndesc))
					tcData = tcData + LEFT(m.dsn, LEN(ALLTRIM(m.dsn)) - 1) + ";"
				ENDIF
			ENDCASE
		ENDDO
		IF	LEN(tcData) > 0
			tcData = LEFT(tcData, LEN(tcData) - 1)
		ENDIF

		RETURN nRetVal
ENDFUNC 
*-------------------------------------------------------
* Function....: ValidatePassword
* Author......: HC www.hceood.eu
* Created.....: July 2010
* Called by...: cuserdialogvfxbase, clogindialogvfxbase
* Description.:	Validates user password
* Parameter...:	tcPassword && decrypted password, tcAlias
* Return Value: logical
*(Change list):
*-------------------------------------------------------
FUNCTION ValidatePassword
LPARAMETERS tcPassword, tcAlias, lnSelect 

LOCAL lnI, lcHistryPassDecrypted, lnPasswordStrengthLevel, llValid, lcMessageText 

llValid = .T.
lcMessageText = ""
tcPassword = ALLTRIM(tcPassword)
lnSelect = SELECT()

&&Unique password validation
IF TYPE("goProgram.nEnforcePasswordHistoryCount")="N" AND goProgram.nEnforcePasswordHistoryCount > 0 AND UniquePass
	lcHistryPassDecrypted = xcrypt(HistryPass)
	FOR lnI = 1 TO MIN(goProgram.nEnforcePasswordHistoryCount, MEMLINES(lcHistryPassDecrypted))
		IF UPPER(tcPassword) == UPPER(ALLTRIM(MLINE(lcHistryPassDecrypted, lnI)))	 
			llValid = .F.
			*{ V&U MS 2012-01-20 5633
			lcMessageText = STRTRAN(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_PASSWORDCANNOTREPEAT, MSG_PASSWORDCANNOTREPEAT), ;
							"*", TRANSFORM(goProgram.nEnforcePasswordHistoryCount)) + CHR(13) + CHR(10)
			*} V&U MS 2012-01-20
		ENDIF 
	ENDFOR 
ENDIF 

&&Strong password validation.
IF llValid AND StrongPass AND TYPE("goProgram.nPasswordLength") = "N" AND goProgram.nPasswordLength > 0 
	IF LEN(tcPassword) < goProgram.nPasswordLength
		llValid = .F.
		*{ V&U MS 2012-01-20 5633
		lcMessageText = STRTRAN(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_THEPASSWORDMUSTBEATLEAST, MSG_THEPASSWORDMUSTBEATLEAST), ;
						"*", TRANSFORM(goProgram.nPasswordLength)) + CHR(13) + CHR(10)
		*} V&U MS 2012-01-20
	ENDIF 
ENDIF 

IF llValid AND StrongPass AND TYPE("goProgram.nPasswordStrengthLevel") = "N" AND goProgram.nPasswordStrengthLevel > 0 
	lnPasswordStrengthLevel = 0
	IF LEN(CHRTRAN(tcPassword, "abcdefghijklmnopqrstuvwxyz", "")) <> LEN(tcPassword)
		lnPasswordStrengthLevel = lnPasswordStrengthLevel  + 1
	ENDIF 
	IF LEN(CHRTRAN(tcPassword, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")) <> LEN(tcPassword)
		lnPasswordStrengthLevel = lnPasswordStrengthLevel  + 1
	ENDIF 
	IF LEN(CHRTRAN(tcPassword, "1234567890", "")) <> LEN(tcPassword)
		lnPasswordStrengthLevel = lnPasswordStrengthLevel  + 1
	ENDIF 
	IF LEN(CHRTRAN(tcPassword, "#%&*!@$^()+=?/'<>|[]{};:_-", "")) <> LEN(tcPassword);
		OR LEN(CHRTRAN(tcPassword, '"', '')) <> LEN(tcPassword)
		lnPasswordStrengthLevel = lnPasswordStrengthLevel  + 1
	ENDIF 
	
	IF goProgram.nPasswordStrengthLevel + 1 > lnPasswordStrengthLevel 
		llValid = .F.
		lcMessageText = CHRTRAN(IIF(goProgram.lRuntimeLocalization,goLocalize.cMSG_THEPASSWORDMUSTCONTAINCHARACTERSFROM,;
								MSG_THEPASSWORDMUSTCONTAINCHARACTERSFROM), "*", TRANSFORM(goProgram.nPasswordStrengthLevel + 1))
	ENDIF 
ENDIF 
SELECT (lnSelect)
IF !llValid 
	goProgram.vfxMessageBox(lcMessageText, 0+64, _SCREEN.CAPTION)
	RETURN .F.
ELSE 
	RETURN .T.
ENDIF 
	
ENDFUNC 
*-------------------------------------------------------
* Function....: OnPreSaveVfxUsr
* Author......: HC www.hceood.eu
* Created.....: July 2010
* Called by...: cuserdialogvfxbase, clogindialogvfxbase
* Description.:	Perform programatical changes before VfxUser save
* Parameter...:	tlEditPassExpiry, tlEditHistryPass, tcOldPass && decrypted old password, tcAlias 
* Return Value: logical
*(Change list):
*-------------------------------------------------------
FUNCTION OnPreSaveVfxUsr

LPARAMETERS tlEditPassExpiry, tlEditHistryPass, tcPassword, tcAlias

LOCAL lcHistryPassDecrypted, lnSelect  

lnSelect = SELECT()
SELECT (tcAlias)
&& Edit PassExpiry
IF tlEditPassExpiry AND TYPE("goProgram.nPasswordValidityDays ")="N" AND goProgram.nPasswordValidityDays > 0
		REPLACE PassExpiry WITH DATE() + goProgram.nPasswordValidityDays IN (tcAlias)
			
ENDIF

&& Edit HistryPass
IF tlEditHistryPass AND TYPE("goProgram.nEnforcePasswordHistoryCount") = "N" AND goProgram.nEnforcePasswordHistoryCount > 0 AND UniquePass;
	AND TYPE("tcPassword") == "C" AND !EMPTY(tcPassword)
	lcHistryPassDecrypted = xcrypt(HistryPass)
	IF MEMLINES(lcHistryPassDecrypted ) > goProgram.nEnforcePasswordHistoryCount
		lcHistryPassDecrypted = SUBSTR(lcHistryPassDecrypted , 1, RATLINE(lcHistryPassDecrypted , CHR(13) + CHR(10)))
	ENDIF 
	lcHistryPassDecrypted = ALLTRIM(tcPassword) + CHR(13) + CHR(10) + lcHistryPassDecrypted
	REPLACE HistryPass WITH  xcrypt(lcHistryPassDecrypted) IN (tcAlias)	 
ENDIF 
SELECT (lnSelect)

RETURN .T.	
ENDFUNC

*-------------------------------------------------------
* Function....: VFXGetAutoincValue
* Author......: HC www.hceood.eu
* Created.....: August 2010
* Used by.....: Data Access classes, included in InsertRefreshFieldsCmd
* Description.:	Returns GETAUTOINCVALUE() regarding Audit triggers
* Parameter...:	 
* Return Value: Integer
*(Change list):
*-------------------------------------------------------
FUNCTION VFXGetAutoincValue()

IF TYPE("__VFX_LastAutoincValue") == "N"
	lnAutoincValue = __VFX_LastAutoincValue
ELSE 
	lnAutoincValue = GETAUTOINCVALUE()
ENDIF 

RETURN lnAutoincValue

*-------------------------------------------------------
* Function....: GetCursorAdapterTable
* Author......: HC www.hceood.eu
* Created.....: November 2010
* Used by.....: QW
* Description.:	
* Parameter...:	 
* Return Value: String
*(Change list):
*-------------------------------------------------------
FUNCTION GetCursorAdapterTable
LPARAMETERS tcAlias
	LOCAL lcResult 
	lcResult = ""
	IF TYPE("tcAlias") == "C" AND !EMPTY(tcAlias) AND USED(tcAlias)
		TRY 
			loCursorAdapter = GETCURSORADAPTER(tcAlias)
		CATCH TO leException
		ENDTRY
		
		IF TYPE("loCursorAdapter") == "O" AND !ISNULL(loCursorAdapter)
			LOCAL lcTable
			LOCAL lnCommaPosition
			lcTable = ""
			TRY
				lcTable = CURSORGETPROP("Tables", tcAlias)
			CATCH TO leException
			ENDTRY
			lnCommaPosition = ATC(lcTable, ",")
			IF lnCommaPosition > 0
				lcTable = SUBSTR(lcTable, 0, lnCommaPosition - 1)
			ENDIF 
			lcTable = ALLTRIM(lcTable)
			IF EMPTY(lcTable)
				lcTable = loCursorAdapter.Tables
				lnCommaPosition = ATC(lcTable, ",")
				IF lnCommaPosition > 0
					lcTable = SUBSTR(lcTable, 0, lnCommaPosition - 1)
				ENDIF
			ENDIF 
			lcResult = lcTable
		ELSE
			lcSQL = ""
			TRY 
				lcSQL = CURSORGETPROP("SQL", tcAlias)
			CATCH TO leException
			ENDTRY 
			lcResult = GetTableFromSelect(lcSQL)
		ENDIF
	ENDIF
	lcResult = UPPER(lcResult)
	RETURN lcResult
ENDFUNC
*-------------------------------------------------------
* Function....: GetTabPicture
* Author......: HC www.hceood.eu
* Created.....: January 2011
* Used by.....: cPageFrameTab, cTab
* Description.:	Returns the picture according ColorScheme
* Parameter...:	tcPictureName, tcColorSchemePrefix
* Return Value: String
*(Change list):
*-------------------------------------------------------
*{ V&U VM 2011-01-10
FUNCTION GetTabPicture
LPARAMETERS tcPictureName, tcColorSchemePrefix

LOCAL lcPicture, lcPath 
lcPicture = ""
IF !EMPTY(tcPictureName) AND !EMPTY(tcColorSchemePrefix)
	lcPath = FULLPATH("bitmap\tabcolors")

	IF !EMPTY(tcColorSchemePrefix)
		IF FILE(ADDBS(lcPath) + ADDBS(tcColorSchemePrefix) + ;
				ALLTRIM(tcColorSchemePrefix) + JUSTSTEM(tcPictureName) + "." + JUSTEXT(tcPictureName))
			lcPicture = ADDBS(lcPath) + ADDBS(tcColorSchemePrefix) + ;
				ALLTRIM(tcColorSchemePrefix) +  JUSTSTEM(tcPictureName) + "." + JUSTEXT(tcPictureName)
		ENDIF
	ENDIF
ENDIF
RETURN lcPicture
ENDFUNC
*} V&U VM 2011-01-10
*-------------------------------------------------------
* Function....: VfxGetFile
* Author......: HC www.hceood.eu
* Created.....: January 2011
* Used by.....: cPageFrameTab, cTab
* Description.:	Displays the Open dialog box like GetFile()
* Parameter...:	tcPictureName, tcColorSchemePrefix
* Return Value: String
*(Change list):
*-------------------------------------------------------
*{ V&U MS 2011-01-31
FUNCTION VfxGetFile
LPARAMETERS tcFileExtensions, tcText, tcOpenButtonCaption, tnButtonType, tcTitleBarCaption

* Parameters are similar as GetFile() but are not supported if mscomdlg.commondialog is used
LOCAL 	loComDialog, lcFileName, lcSetPath, lcSetDefault, lcCurrentDir, lcFileExtensions, ;
		lcExtension, lcJustExt, lcDescription, lnExt, lcExt, lcNewExt, llUseGetFile 
		
lcSetPath 	 = SET("PATH") 
lcSetDefault = SET("DEFAULT")
*{V&U MS 2012-07-17 5861
lcCurrentDir = GetDefaultFolder()
*}V&U MS 2012-07-17

*{V&U MS 2011-05-09, Use GETFILE if NEWOBJECT("mscomdlg.commondialog") raises an error
llUseGetFile = .F.
TRY 
	loComDialog  = NEWOBJECT("mscomdlg.commondialog")
CATCH
	llUseGetFile = .T.
ENDTRY 	

IF llUseGetFile OR !(TYPE("loComDialog") == "O")
	*{V&U MS 2011-12-07 5588
	RETURN GETFILE(EVL(tcFileExtensions, ''), EVL(tcText, ''), EVL(tcOpenButtonCaption, ''), EVL(tnButtonType, 0), EVL(tcTitleBarCaption, ''))
	*}V&U MS 2011-12-07	
ENDIF 
*}V&U MS 2011-05-09

IF EMPTY(tcFileExtensions)
	lcFileExtensions = IIF(TYPE("goProgram.Class") == "C", ;
					IIF(goProgram.lRuntimelocalization, goLocalize.cCAP_FILTER_ALLFILES, CAP_FILTER_ALLFILES), ;
					"All files|*.*")
ELSE 
	lcFileExtensions = ""
	tcFileExtensions = STRTRAN(tcFileExtensions, ";", CHR(13) + CHR(10))
	FOR lnLine = 1 TO MEMLINES(tcFileExtensions)
		lcExtension  = MLINE(tcFileExtensions, lnLine)
		lcExtension  = CHRTRAN(lcExtension, " ", "")
		lcExtension  = CHRTRAN(lcExtension, ":", "|")
		lcExtension  = CHRTRAN(lcExtension, ",", ";")
		
		IF AT("|", lcExtension) = 0
			lcJustExt = lcExtension
			lcDescription = UPPER(lcExtension)
		ELSE 	
			lcDescription = SUBSTR(lcExtension, 1, AT("|", lcExtension) - 1)
			lcJustExt = SUBSTR(lcExtension, AT("|", lcExtension) + 1)
		ENDIF
		lcJustExt = STRTRAN(lcJustExt, ";", CHR(13) + CHR(10))
		lcNewExt  = ""
		FOR lnExt = 1 TO MEMLINES(lcJustExt)
			lcExt = MLINE(lcJustExt, lnExt)
			IF AT("*.", lcExt) = 0
				lcExt = "*." + lcExt
			ENDIF 
			lcNewExt = IIF(!EMPTY(lcNewExt), lcNewExt + [;], []) + lcExt	
		ENDFOR 
		
		lcFileExtensions = IIF(!EMPTY(lcFileExtensions), lcFileExtensions + "|", []) + lcDescription + "|" + lcNewExt
	ENDFOR 
ENDIF 

loComDialog.Filter = lcFileExtensions
loComDialog.FilterIndex = 1

* Specify size in octets to load the filter string(otherwise this returns error: 
* OLE IDispatch exception code 0 from CommonDialog)
*{V&U MS 2014-12-08 Increase MaxFileSize 
loComDialog.MaxFileSize = 280
*}V&U MS 2014-12-08
IF !EMPTY(tcTitleBarCaption)
	loComDialog.DialogTitle	= tcTitleBarCaption
ENDIF 
*{V&U MS 2012-07-23 5866
loComDialog.InitDir = ADDBS(SYS(5) + CURDIR())
*}V&U MS 2012-07-23
loComDialog.ShowOpen()
lcFileName = loComDialog.FileName

SET PATH TO (lcSetPath)
SET DEFAULT TO (lcSetDefault)
IF !EMPTY(lcCurrentDir)
	CD (lcCurrentDir)
ENDIF
RETURN lcFileName
*} V&U MS 2011-01-31

*-- PH 07.12.2009
*-- EXECUTEAFP
*-- Führt einen HTML-Code aus, welcher Scriptletzeichen beinhaltet
*-- die Scriptletzeichen sind <% %>
*-- Alles dazwischen wird als FoxPro Code ausgewertet
*-- Zusätzlich kann mit << >> ein Textmerge ausgeführt werden.
*----------------------------------------------------------------------
*-- Beispiele:
*-- <%?datetime()%> gibt das Datetime aus
*-- <<datetime()>> gibt ebenfalls Datetime aus
*--
*-- bei den << >> darf es nur ein evaluierbarer Ausdruck sein
*-- bei <% %> dürfen mehrere Zeilen geschrieben werden
*----------------------------------------------------------------------
FUNCTION executeafpx
LPARAMETERS tcHTMLString
LOCAL lcHTMLString, lcprocedurePRG, lcprocedureFXP,lcprocedureERR, lcsys2015, lcreturn

IF TYPE("gctmp") <> "C"
	LOCAL gctmp
	gctmp = GETENV("temp")
ENDIF
SET MEMOWIDTH TO 8092
lcsys2015 = SYS(2015)
lcprocedurePRG = ADDBS(gctmp) + lcsys2015 + ".PRG"
lcprocedureFXP = ADDBS(gctmp) + lcsys2015 + ".FXP"
lcprocedureERR = ADDBS(gctmp) + lcsys2015 + ".ERR"
lx1 = ADDBS(gctmp) + "Status" + SYS(2015) + ".txt"
lx2 = ADDBS(gctmp) + "Memo" + SYS(2015) + ".txt"

lcHTMLString = makehtmlsimple(tcHTMLString, lcsys2015)
STRTOFILE(lcHTMLString, lcprocedurePRG)
COMPILE (lcprocedurePRG)
IF FILE(lcprocedureERR)
	lcreturn = FILETOSTR(lcprocedureERR)
	DISPLAY STATUS TO FILE (lx1) NOCONSOLE
	lcdispstat = FILETOSTR(lx1)
	DISPLAY MEMO TO FILE (lx2) NOCONSOLE
	lcdispmemo = FILETOSTR(lx2)
	DELETE FILE (lx1)
	DELETE FILE (lx2)
	lcstring = ""
	lcstring=lcstring+"<table border=1>"
	lcs=FILETOSTR(lcprocedurePRG)
	lnmemlines = ALINES(laalines, lcs, 1)
	FOR i = 1 TO lnmemlines
		lcstring=lcstring+"<tr><td>"
		lcstring=lcstring+TRANSFORM(i,"99999")+"</td><td>"+STRTRAN(STRTRAN(laalines(i),"<","&lt;"),">","&gt;")+"</td></tr>"
	ENDFOR
	lcstring=lcstring+"</table>"
	lcreturn = lcreturn + lcstring + "<hr>" + lcdispstat + "<hr>"+ lcdispmemo
ELSE
	TRY
		___afp=""
		DO (lcprocedureFXP)
		lcreturn = ___afp
	CATCH TO oerr
		lcreturn = [  Fehler: ] + STR(oerr.ERRORNO) +;
			[  LineNo: ] + STR(oerr.LINENO)  +;
			[  Message: ] + oerr.MESSAGE  +;
			[  Procedure: ] + oerr.PROCEDURE  +;
			[  Details: ] + oerr.DETAILS  +;
			[  StackLevel: ] + STR(oerr.STACKLEVEL)  +;
			[  LineContents: ] + oerr.LINECONTENTS +;
			[  UserValue: ] + oerr.USERVALUE

		DISPLAY STATUS TO FILE (lx1) NOCONSOLE
		lcdispstat = FILETOSTR(lx1)
		DISPLAY MEMO TO FILE (lx2) NOCONSOLE
		lcdispmemo = FILETOSTR(lx2)
		DELETE FILE (lx1)
		DELETE FILE (lx2)
		lcstring=""
		lcstring=lcstring+"<table border=1>"
		lcs=FILETOSTR(lcprocedurePRG)
		lnmemlines = ALINES(laalines, lcs,1)
		FOR i=1 TO lnmemlines
			lcstring=lcstring+"<tr><td>"
			lcstring=lcstring+TRANSFORM(i,"99999")+"</td><td>"+STRTRAN(STRTRAN(laalines(i),"<","&lt;"),">","&gt;")+"</td></tr>"
		ENDFOR
		lcstring=lcstring+"</table>"
		lcreturn = lcreturn + lcstring + "<hr>" + lcdispstat + "<hr>"+ lcdispmemo
	ENDTRY
ENDIF
IF FILE(lcprocedurePRG)
	DELETE FILE (lcprocedurePRG)
ENDIF
IF FILE(lcprocedureFXP)
	DELETE FILE (lcprocedureFXP)
ENDIF
IF FILE(lcprocedureERR)
	DELETE FILE (lcprocedureERR)
ENDIF
RETURN lcreturn
ENDFUNC

PROCEDURE makehtmlsimple
LPARAMETER tcHTMLString, tcprocname
LOCAL lcstring,lcstring1, lcOut,i, lci, lni, lcstringsave,lcfile , lcprocname, lcmline,lnmemlines
LOCAL ARRAY laalines(1)
LOCAL arraycount
LOCAL lcs1
LOCAL ARRAY __temparray(1)
SET MEMOWIDTH TO 8192
*-- wenn alles mit <% anfängt eine Leerzeile einfügen
tcHTMLString = STRTRAN(tcHTMLString, CHR(10), CHR(13))
tcHTMLString = STRTRAN(tcHTMLString, CHR(13) + CHR(13), CHR(13) + CHR(10))
tcHTMLString = STRTRAN(tcHTMLString, CHR(9), " ")

arraycount=1
lcstring = ""
lcstring = lcstring + "procedure " + tcprocname+ CHR(13)+CHR(10)
lcstring = lcstring + "___afp = ''" + CHR(13)+CHR(10)

DO WHILE "<%" $ tcHTMLString
	DEBUGOUT tcHTMLString
	lcOut = SUBSTR(tcHTMLString, 1, AT("<%", tcHTMLString, 1) - 1)
	__temparray(arraycount) = lcOut

	lcstring = lcstring + [text to ___afp TEXTMERGE noshow addi] + CHR(13) + CHR(10)
	lcstring = lcstring + lcOut + CHR(13) + CHR(10)
	lcstring = lcstring + [endtext] + CHR(13) + CHR(10)

	arraycount = arraycount + 1
	DIME __temparray(arraycount)
	tcHTMLString = STRTRAN(tcHTMLString, lcOut, "", 1, 1)
	tcHTMLString = SUBSTR(tcHTMLString, 3)
	IF "%>" $ tcHTMLString
		lcOut = SUBSTR(tcHTMLString, 1, AT("%>", tcHTMLString, 1) - 1)
		lnmemlines = ALINES(laalines, lcOut, 1)
		FOR lnI = 1 TO lnmemlines
			lcmline = laalines(lnI)
			IF NOT EMPTY(lcmline)
				IF LEFT(ALLTRIM(lcmline), 1) <> "*"
					lcs1 = strausgabe(lcmline)+ CHR(13) + CHR(10)
					lcstring = lcstring + lcs1
				ENDIF
			ENDIF
		ENDFOR
		tcHTMLString = STRTRAN(tcHTMLString,lcOut, "", 1, 1)
		tcHTMLString = SUBSTR(tcHTMLString, 3)
	ENDIF
ENDDO
__temparray(arraycount)=tcHTMLString
lcstring = lcstring + [text to ___afp TEXTMERGE noshow addi] + CHR(13) + CHR(10)
lcstring = lcstring + tcHTMLString + CHR(13) + CHR(10)
lcstring = lcstring + [endtext] + CHR(13) + CHR(10)
lcstring = lcstring + CHR(13) + CHR(10)
lcstring = lcstring + "return ___afp"  + CHR(13) + CHR(10)
lcstring = lcstring + CHR(13) + CHR(10)
RETURN lcstring

PROCEDURE strausgabe
LPARAMETER tcout
LOCAL lcstring, lcs
tcout = ALLTRIM(CHRTRAN(tcout, CHR(9) + CHR(13) + CHR(10), "   "))
IF LEFT(tcout, 1) = "?" OR LEFT(tcout, 1) = "="
	lcstring = ALLTRIM(SUBSTR(tcout, 2))
	tcout = "___afp=___afp+transform("+lcstring+")"
ELSE
	IF PADR( UPPER(tcout), 4) == "RETU"
		lcstring = PADR( UPPER(tcout), 10)
		lcstring = STRTRAN(lcstring, "RETURN")
		lcstring = STRTRAN(lcstring, "RETUR")
		lcstring = STRTRAN(lcstring, "RETU")
		lcstring = ALLTRIM(lcstring)
		IF TYPE("eval(lcstring)") <> "C"
			tcout = "RETURN ___afp"
		ENDIF
	ENDIF
ENDIF
RETURN tcout

*-------------------------------------------------------
* Function....: ModernUIEventHookHandler
* Author......: HC www.hceood.eu
* Created.....: October 2012
* Used by.....: ModernUI Layout
* Description.:	Sets background theme of ModernUI Layout
* Parameter...:	tcEvent, toObject, toForm
* Return Value: Logical
*(Change list):
*-------------------------------------------------------
*{V&U MS 2012-10-01 5645
FUNCTION ModernUIEventHookHandler
LPARAMETERS tcEvent, toObject, toForm

IF LOWER(ALLTRIM(tcEvent)) = "init"
	IF TYPE("toObject.FontName") == "C"
		toObject.FontName = "Segoe UI"
	ENDIF 	
	DO CASE
		CASE LOWER(ALLTRIM(toObject.BaseClass)) = "pageframe"
			toObject.Themes = .F.
			FOR lnPage = 1 TO toObject.PageCount
				toObject.Pages[lnPage].BackColor = IIF(TYPE("goUser") = "O", goUser.oCustomizeSettings.nMainBackColor, RGB(1,123,136))
			ENDFOR 
		CASE INLIST(LOWER(ALLTRIM(toObject.BaseClass)), "form", "container")
			toObject.BackColor = IIF(TYPE("goUser") = "O", goUser.oCustomizeSettings.nMainBackColor, RGB(1,123,136))
		CASE INLIST(LOWER(ALLTRIM(toObject.BaseClass)), "textbox", "editbox", "combobox")
			toObject.BackColor = IIF(TYPE("goUser") = "O", goUser.oCustomizeSettings.nTextBackColor, RGB(205,231,232))
		*{V&U MS 2013-01-07 5645
		CASE INLIST(LOWER(ALLTRIM(toObject.BaseClass)), "label", "checkbox")
			toObject.ForeColor = IIF(TYPE("goUser") = "O", goUser.oCustomizeSettings.nForeColor, RGB(210,255,255))
		*}V&U MS 2013-01-07	
		*{V&U MS 2013-01-10 5645
		CASE LOWER(ALLTRIM(toObject.BaseClass)) = "optiongroup"
			FOR EACH loObject IN toObject.Objects
				loObject.ForeColor = IIF(TYPE("goUser") = "O", goUser.oCustomizeSettings.nForeColor, RGB(210,255,255))
			ENDFOR 
		*}V&U MS 2013-01-10
		*}V&U LD 2013-05-25 5645
		CASE LOWER(ALLTRIM(toObject.BaseClass)) = "grid"
       		toObject.SetAll("BackColor", RGB(100,180,180), "Header")
       		toObject.SetAll("BackColor", RGB(205,231,232), "Column")
		*}V&U LD 2013-05-25
	ENDCASE
ENDIF
*}V&U MS 2012-10-01

*-------------------------------------------------------
* Class.......: ExtensionHandler
* Author......: HC www.hceood.eu
* Created.....: July 2013
* Used by.....: Print/Preview
* Description.:	An extension hadler where Search in preview
*				functionality is implemented
* Parameter...:	
* Return Value: 
*(Change list):
*-------------------------------------------------------
*{V&U MS 2013-07-18 4777
DEFINE CLASS ExtensionHandler AS CUSTOM

	PreviewForm    = NULL
	lHighlightText = .F.
	*{V&U MP 12-08-2013
	_oProofSheet = ""
	*}V&U MP 12-08-2013	
	PROCEDURE SHOW(iStyle)
		BINDEVENT(This.PreviewForm, "RenderPage", THIS, "RenderPage", 1)
		DODEFAULT(m.iStyle)
	ENDPROC

	PROCEDURE cmdSearchVisibility
		LPARAMETERS tlVisible

		LOCAL loToolbar as Toolbar, lnWidth, lcText, lnSize
		loToolBar = This.PreviewForm.Toolbar

		IF NOT ISNULL(loToolBar) AND VARTYPE(loToolbar) = "O"

			WITH loToolBar.cntSearch1

				lnWidth = loToolBar.cntSearch1.cmdSearch1.Width
				.cmdSearch1.Width = lnWidth
				.cmdSearchBack1.Width = lnWidth
				.cmdSearchAgain1.Width = lnWidth

				.cmdSearchAgain1.Visible = tlVisible
				.cmdSearchBack1.Visible  = tlVisible

				IF tlVisible
					lcText = ": '" + ALLTRIM(This.PreviewForm.oReport.cTextToFind) + "'"
					lnSize = loToolBar.cntSearch1.cmdSearch1.Width
					.cmdSearchBack1.Left = lnSize + 1
					.cmdSearchBack1.ToolTipText = IIF(TYPE("goProgram.lRuntimeLocalization") = "L" AND goProgram.lRuntimeLocalization, goLocalize.cCAP_FINDBACK, CAP_FINDBACK) + lcText
					.cmdSearchAgain1.Left = (m.lnSize * 2) + 2
					.cmdSearchAgain1.ToolTipText = IIF(TYPE("goProgram.lRuntimeLocalization") = "L" AND goProgram.lRuntimeLocalization, goLocalize.cCAP_FINDNEXT, CAP_FINDNEXT) + lcText
					.Width = .cmdSearch1.Width + .cmdSearchBack1.Width + .cmdSearchAgain1.Width + 2
				ELSE
					.Width = .cmdSearch1.Width				
				ENDIF 
			ENDWITH 
		ENDIF 
	ENDPROC 

	PROCEDURE DoSearch
		* Prompt the user for some text to find, find the first instance of it, and
		* highlight it.

		This.ClearBox()

		LOCAL lcText, lcReportAlias, lcAlias
		lcReportAlias = ALIAS()
		lcAlias = This.PreviewForm.oReport.cOutputAlias

		IF EMPTY(m.lcAlias)
			IF TYPE("goProgram.Class") = "C" 
				goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, ;
									goLocalize.cMSG_SEARCHFEATUREISCURRENTLYUNAVAILABLEFORTHISREPORT, MSG_SEARCHFEATUREISCURRENTLYUNAVAILABLEFORTHISREPORT), ;
									48, _Screen.Caption)
			ELSE 	
				MESSAGEBOX("Search feature is currently unavailable for this report.", 48, _Screen.Caption)
			ENDIF 
			RETURN 
		ENDIF

		WITH This
			IF VARTYPE(This.PreviewForm.TOOLBAR) = "O"
				This.PreviewForm.ShowToolbar(.F.)
				DO FORM vfxRepSearch WITH '', This.PreviewForm TO lcText	
				IF VARTYPE(This.PreviewForm.TOOLBAR) = "O"
					This.PreviewForm.ShowToolbar(.T.)
				ENDIF 
			ELSE
				DO FORM vfxRepSearch WITH '', This.PreviewForm TO lcText	
			ENDIF

			IF _Screen.Visible 
				ACTIVATE WINDOW (This.PreviewForm.NAME)
				*{V&U MS 2013-11-13 6292
				IF TYPE("This._oProofSheet.NAME") == "C"
					ACTIVATE WINDOW (This._oProofSheet.NAME)
				ENDIF 	
				*}V&U MS 2013-11-13
			ENDIF 

			INKEY(.2)
			DOEVENTS 

			IF NOT EMPTY(lcText)
				LOCAL llError
				TRY
					SELECT (lcAlias)
				CATCH
					llError = .T.
				ENDTRY 
				IF llError
					IF TYPE("goProgram.Class") = "C" 
						goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, ;
											goLocalize.cMSG_SEARCHFEATUREISCURRENTLYUNAVAILABLEFORTHISREPORT, MSG_SEARCHFEATUREISCURRENTLYUNAVAILABLEFORTHISREPORT), ;
											48, _Screen.Caption)
					ELSE 	
						MESSAGEBOX("Search feature is currently unavailable for this report.", 48, _Screen.Caption)
					ENDIF  
					RETURN
				ENDIF 
						
				This.PreviewForm.oReport.cTextToFind = lcText
				
				LOCATE FOR UPPER(lcText) $ UPPER(CONTENTS)
				.HandleFind(FOUND())
			ELSE 
				This.cmdSearchVisibility(.F.)
			ENDIF 
		ENDWITH
		
		* Restore original alias
		TRY 
			SELECT (lcReportAlias)
		CATCH
		ENDTRY 
	ENDPROC 

	PROCEDURE DoSearchAgain
		This.ClearBox()
		
		LOCAL lcText, lcAlias, lcReportAlias
		lcReportAlias = ALIAS()
		lcAlias = This.PreviewForm.oReport.cOutputAlias
		lcText  = This.PreviewForm.oReport.cTextToFind

		IF NOT EMPTY(lcText)
			SELECT (lcAlias)
			IF NOT EOF()
				SKIP +1 
			ENDIF 
			LOCATE REST FOR UPPER(lcText) $ UPPER(CONTENTS)
			IF EOF()
				* Search from beginning
				LOCATE FOR UPPER(lcText) $ UPPER(CONTENTS)
			ENDIF 
			This.HandleFind(FOUND(), .T.) && The 2nd parameter means that we are calling
										&& SearchAgain
		ENDIF
		
		* Restore original alias
		TRY 
			SELECT (lcReportAlias)
		CATCH
		ENDTRY 
	ENDPROC 

	PROCEDURE DoSearchBack
		This.ClearBox()
		
		LOCAL lcText, lcAlias, lcReportAlias
		lcReportAlias = ALIAS()
		lcAlias = This.PreviewForm.oReport.cOutputAlias
		lcText  = UPPER(This.PreviewForm.oReport.cTextToFind)

		IF NOT EMPTY(lcText)
			SELECT (lcAlias)

			DO WHILE .T.
				SKIP -1
				IF BOF()
					GO BOTTOM 
				ENDIF 

				IF lcText $ UPPER(CONTENTS)
					EXIT
				ENDIF 
			ENDDO 
			This.HandleFind(.T., .T.) && The 2nd parameter means that we are calling
										&& SearchAgain
		ENDIF
		SELECT (lcReportAlias)
	ENDPROC 

	* Handle whether the object was found or not.
	PROCEDURE HandleFind
		LPARAMETERS tlFound, tlAgain

		IF tlFound
			This.lHighlighttext = .T.
			This.CmdSearchVisibility(.T.)
		ELSE

			IF NOT tlAgain
				This.CmdSearchVisibility(.F.)
			ENDIF 

			*MESSAGEBOX(_goFP.GetLoc("NOTFOUND"), ;
			*48, CHRTRAN(_goFP.GetLoc("FINDTEXT"),":",""))
			GO TOP 
			This.lHighlighttext = .F.
			RETURN 
		ENDIF

		IF PAGE <> This.PreviewForm.CurrentPage
			This.lHighlighttext = .T.
			This.PreviewForm.TempStopRepaint = .T.
			This.PreviewForm.setCurrentPage( PAGE )
		ENDIF 

		This.ScrollToObject(Left/10, Top/10, Width/10, Height/10)
		=INKEY(.2)
		DOEVENTS  
		This.lHighlighttext = .T.
		This.PreviewForm.RenderPages()
	ENDPROC 

	PROCEDURE ClearBox
		TRY 
			LOCAL loForm as Form
			loForm = This.PreviewForm
			IF VARTYPE(loForm.LineTop) = "O" 
				loForm.RemoveObject("lineTop")
			ENDIF 
			IF VARTYPE(loForm.LineBott) = "O" 
				loForm.RemoveObject("lineBott")
			ENDIF 
			IF VARTYPE(loForm.LineLeft) = "O" 
				loForm.RemoveObject("lineLeft")
			ENDIF 
			IF VARTYPE(loForm.LineRight) = "O" 
				loForm.RemoveObject("lineRight")
			ENDIF 
		CATCH
		ENDTRY 
	ENDPROC 

	* By Doug Hennig
	* We may need to scroll the form if the specified object isn't currently visible.
	PROCEDURE ScrollToObject
		LPARAMETERS tnLeft, tnTop, tnWidth, tnHeight
		LOCAL lnNewTop, lnNewLeft, lnVPos, lnHPos, ;
				lnVpTop, lnVpLeft, lnVpWidth, lnVpHeight

		WITH This.PreviewForm as Form
			lnVpTop    = .ViewPortTop 
			lnVpLeft   = .ViewPortLeft 
			lnVpWidth  = .ViewPortWidth 
			lnVpHeight = .ViewPortHeight 

			lnNewTop   = lnVpTop
			lnNewLeft  = lnVpLeft
			lnVPos     = tnTop  + tnHeight + This.PreviewForm.Canvas1.Top
			lnHPos     = tnLeft + tnWidth  + This.PreviewForm.Canvas1.Left

			IF NOT BETWEEN(lnVPos - 20, lnVpTop, lnVpTop + lnVpHeight)
				lnNewTop = lnVPos - lnVpHeight /2
			ENDIF
			IF NOT BETWEEN(lnHPos, lnVpLeft, lnVpLeft + lnVpWidth)
				lnNewLeft = lnHPos - lnVpWidth /2
			ENDIF
		
			IF lnNewTop <> lnVpTop OR lnNewLeft <> lnVpLeft
				.SetViewPort(lnNewLeft, lnNewTop)
			ENDIF
		ENDWITH
	ENDPROC

	PROCEDURE HighLightObject
		LPARAMETERS tnL, tnT, tnW, tnH

		LOCAL lnPixelsPerDPI960, lnHWnd, lnX, lnY, lnWidth, lnHeight

		WITH This.PreviewForm

			* Adjust coordinates
			lnPixelsPerDPI960 = .GetPixelsPerDPI960()
			lnX = .canvas1.Left + INT(tnL * lnPixelsPerDPI960) - 2
			lnY = .canvas1.Top  + INT(tnT * lnPixelsPerDPI960) - 2
			lnWidth  = INT(tnW * lnPixelsPerDPI960) + 8
			lnHeight = INT(tnH * lnPixelsPerDPI960) + 8

			* Draw the box
			LOCAL loForm as Form
			loForm = This.PreviewForm

			loForm.AddObject("lineTop"  , "Line")
			loForm.AddObject("lineBott" , "Line")
			loForm.AddObject("lineLeft" , "Line")
			loForm.AddObject("lineRight", "Line")
		
			LOCAL lnColor
			lnColor = RGB(255, 64, 64)

			.TempStopRepaint = .T.
			WITH loForm.LineTop as Line 
				.Width       = lnWidth
				.BorderColor = lnColor
				.BorderWidth = 0
				.Top         = lnY
				.Left        = lnX
				.Height      = 0
				.Visible     = .T.
			ENDWITH 

			.TempStopRepaint = .T.
			WITH loForm.LineBott as Line
				.Width       = lnWidth
				.BorderColor = lnColor
				.BorderWidth = 0
				.Top         = (lnY + lnHeight)
				.Left        = lnX
				.Height      = 0
				.Visible     = .T.
			ENDWITH 

			.TempStopRepaint = .T.
			WITH loForm.LineLeft as Line 
				.Width       = 0
				.BorderColor = lnColor
				.BorderWidth = 0
				.Top         = lnY
				.Left        = lnX
				.Height      = lnHeight
				.Visible     = .T.
			ENDWITH 

			.TempStopRepaint = .T.
			WITH loForm.LineRight as Line 
				.Width = 0
				.BorderColor = lnColor
				.BorderWidth = 0
				.Top         = lnY
				.Left        = lnX + lnWidth
				.Height      = lnHeight
				.Visible     = .T.
			ENDWITH 

		ENDWITH 
		This.lHighlightText = .F.
	ENDPROC

	PROCEDURE RenderPage
		LPARAMETERS tiPage, toCanvas
		This.ClearBox()
		IF This.lHighlightText
			SELECT (This.PreviewForm.oReport.cOutputAlias)
			This.lHighlightText = .F.
			This.HighlightObject(left, top, width, height)
		ENDIF 
	ENDPROC
	
	*{V&U MP 2013-08-12
	PROCEDURE DoProof
		LOCAL llShowToolBar, lnPosition
		
		*{V&U MS 2014-03-27 6452
		IF TYPE("This.PreviewForm.Class") <> "C"
			RETURN 
		ENDIF 
		*}V&U MS 2014-03-27
		*{V&U MS 2013-10-02
		llShowToolBar = TYPE("This.PreviewForm.TOOLBAR") = "O" ;
			AND This.PreviewForm.Toolbar.Visible = .T.
		*}V&U MS 2013-10-02
		IF m.llShowToolBar
			This.PreviewForm.ShowToolbar(.T.)
		ENDIF
		*{V&U MS 2014-03-26 6452
		WITH This
			IF !(VARTYPE(._oProofSheet) == 'O')
				This.PreviewForm.Parent.AddObject('ProofSheet', "ProofSheet")
				._oProofSheet = This.PreviewForm.Parent.ProofSheet
				._oProofSheet.SetReport(This.PreviewForm.oReport)
				*{V&U MP 2013-08-12
				._oProofSheet.Caption = IIF(goProgram.lRuntimeLocalization, goLocalize.cCAP_THUMBNAILS, CAP_THUMBNAILS)
				*}V&U MS 2013-08-12
				IF _Screen.Visible 
					ACTIVATE WINDOW (This.PreviewForm.NAME)
				ENDIF 
				*{V&U MS 2013-07-31
				* At right site
				IF (vartype(goProgram) = "O" AND goProgram.nThumbNailsPosition = 1) && OR This.nFormPosition = .F.
					._oProofSheet.Top = 0
					._oProofSheet.Left = _Screen.Width - ._oProofSheet.Width - SYSMETRIC(10)*5
					*{V&U MS 2013-11-01 6292
					._oProofSheet.Height = MAX(_Screen.Height - SYSMETRIC(9) - SYSMETRIC(10)*5 - ;
									IIF(._oProofSheet.shpRed.Width > ._oProofSheet.Width, SYSMETRIC(8), 0), 0)
					*}V&U MS 2013-11-01				
					This.PreviewForm.WindowState = 0
					This.PreviewForm.BorderStyle = 2
					This.PreviewForm.ControlBox = .F.
					This.PreviewForm.Movable = .F.
					This.PreviewForm.Top = 0
					This.PreviewForm.Left = 0
					*{V&U MS 2013-11-01 6292
					This.PreviewForm.Height = MAX(_Screen.Height - SYSMETRIC(9) - SYSMETRIC(10)*5 - ;
											IIF(This.PreviewForm.Width < This.PreviewForm.Canvas1.Width, SYSMETRIC(8), 0), 0)
					This.PreviewForm.Width = MAX(._oProofSheet.Left - SYSMETRIC(10)*5 - ;
											IIF(This.PreviewForm.Height < This.PreviewForm.Canvas1.Height, SYSMETRIC(5), 0), 0)
					*}V&U MS 2013-11-01	
				ENDIF
				
				* At left site
				IF (vartype(goProgram) = "O" AND goProgram.nThumbNailsPosition = 2)
					._oProofSheet.Top = 0
					._oProofSheet.Left = 0
					*{V&U MS 2013-11-01 6292
					._oProofSheet.Height = MAX(_Screen.Height - SYSMETRIC(9) - SYSMETRIC(10)*5 - ;
									IIF(._oProofSheet.shpRed.Width > ._oProofSheet.Width, SYSMETRIC(8), 0), 0)
					*}V&U MS 2013-11-01	 
					
					This.PreviewForm.WindowState = 0
					This.PreviewForm.BorderStyle = 2
					This.PreviewForm.ControlBox = .F.
					This.PreviewForm.Movable = .F.
					This.PreviewForm.Top = 0
					This.PreviewForm.Left = ._oProofSheet.Width + SYSMETRIC(10)*5 
					*{V&U MS 2013-11-01 6292
					This.PreviewForm.Height = MAX(_Screen.Height - SYSMETRIC(9) - SYSMETRIC(10)*5 - ;
											IIF(This.PreviewForm.Width < This.PreviewForm.Canvas1.Width, SYSMETRIC(8), 0), 0)
					This.PreviewForm.Width = MAX(_Screen.Width - This.PreviewForm.Left - SYSMETRIC(10)*5 - ;
											IIF(This.PreviewForm.Height < This.PreviewForm.Canvas1.Height, SYSMETRIC(5), 0), 0)
					*}V&U MS 2013-11-01
				ENDIF
				*}V&U MS 2013-07-31	
				._oProofSheet.SetProofCaption()
				._oProofSheet.AddProperty('oParent', 'ProofSheet')
				._oProofSheet.oParent = This
				IF This.PreviewForm.Visible
					._oProofSheet.Visible = .T.
					._oProofSheet.onShow()
				ENDIF 	
			ELSE 
				._oProofSheet.onShow()
				* read the selected page and move to it,
				* using the .SetCurrentPage() method of
				* the default preview container:
				IF VARTYPE(NVL(._oProofSheet, "")) = "O"
					LOCAL loExc as Exception 
					TRY 
						LOCAL lnPage
						m.lnPage = ._oProofSheet.CurrentPage
						This.PreviewForm.setCurrentPage(m.lnPage)
						This.PreviewForm.refresh()
					CATCH TO m.loExc
						IF ISDEBUGGING
							SET STEP ON 
						ENDIF 
					ENDTRY
				ENDIF 
				IF m.llShowToolBar AND VARTYPE(This.PreviewForm.TOOLBAR) = "O"
					This.PreviewForm.ShowToolbar(.T.)
				ENDIF
			ENDIF 
		ENDWITH 
		*}V&U MS 2014-03-26	
	ENDPROC
	*}V&U MP 2013-08-12
ENDDEFINE
*}V&U MS 2013-07-18

*{V&U MP 2013-08-12
* Class.......: PageSetBtn 
* Author......: HC www.hceood.eu
* Created.....: August 2013
* Used by.....: Print/Preview
* Description.:	
* Parameter...:	
* Return Value: 
*(Change list):
*-------------------------------------------------------
DEFINE CLASS PageSetBtn AS COMMANDBUTTON
	HEIGHT 	=  30
	WIDTH 	=  30
	CAPTION	= ""
	cType	= "NEXT"

	PROCEDURE CLICK
		DO CASE
		CASE This.cType == "FIRST"
			This.PARENT.nPageSet = 1

		CASE This.cType == "PREV"
			This.PARENT.nPageSet = This.PARENT.nPageSet - 1

		CASE This.cType == "NEXT"
			This.PARENT.nPageSet = This.PARENT.nPageSet + 1

		CASE This.cType == "LAST"
			This.PARENT.nPageSet = CEILING(This.PARENT.nPages / This.PARENT.nMaxMiniatureItem)
		ENDCASE
		This.PARENT.RefreshPageBtn()
	ENDPROC

	PROCEDURE REFRESH
		DO CASE
		CASE This.cType == "FIRST"
			This.ENABLED = NOT (This.PARENT.nPageSet == 1)

		CASE This.cType == "PREV"
			This.ENABLED = NOT (This.PARENT.nPageSet == 1)

		CASE This.cType == "NEXT"
			This.ENABLED = NOT (This.PARENT.nPageSet == CEILING(This.PARENT.nPages / This.PARENT.nMaxMiniatureItem))
		CASE This.cType == "LAST"
			This.ENABLED = NOT (This.PARENT.nPageSet == CEILING(This.PARENT.nPages / This.PARENT.nMaxMiniatureItem))
		ENDCASE
	ENDPROC
ENDDEFINE
*}V&U MP 2013-08-12

*{V&U MP 2013-08-12
* Class.......: frmReport
* Author......: HC www.hceood.eu
* Created.....: August 2013
* Used by.....: Print/Preview
* Description.: 
* Parameter...:	
* Return Value: 
*(Change list):
DEFINE CLASS frmReport AS FORM
	SHOWTIPS = .T.
	nNum = 0
	PROCEDURE Init
		This.Icon = "reportprocessing\images\wwrite.ico"
	ENDPROC
	
	PROCEDURE Sleep
		LPARAMETERS nSec
		
		DECLARE Sleep IN Win32API INTEGER nMilliseconds
		
		Sleep(nSec)
	ENDPROC
ENDDEFINE
*}V&U MP 2013-08-12

*{V&U MP 2013-08-12
* Class.......: proofshape
* Author......: HC www.hceood.eu
* Created.....: August 2013
* Used by.....: Print/Preview
* Description.: 
* Parameter...:	
* Return Value: 
*(Change list):
DEFINE CLASS proofshape AS SHAPE
	HEIGHT = 110
	WIDTH = 85
	nYC = 0
	nSec = 0
	*Provides the current page number for report output.
	PAGENO = 0
	NAME = "proofshape"

	PROCEDURE MOUSELEAVE
		LPARAMETERS nButton, nShift, nXCoord, nYCoord
		This.MOUSEPOINTER = 0 && Default
		* This.ResetToDefault("BorderColor")
	ENDPROC

	PROCEDURE MOUSEENTER
		LPARAMETERS nButton, nShift, nXCoord, nYCoord
		This.MOUSEPOINTER = 15 && Hand
		* This.BorderColor = RGB(255,0,0) && Red
		This.PARENT.nCurrShape = This.PAGENO
	ENDPROC

	PROCEDURE CLICK
		THISFORM.currentPage = This.PAGENO
		This.Parent.oParent.doProof()
	ENDPROC
		
	PROCEDURE MOUSEDOWN
		LPARAMETERS nButton, nShift, nXCoord, nYCoord
		IF nButton = 1
			This.nYC = nYCoord
			This.nSec = SECONDS()
		ENDIF
	ENDPROC

	PROCEDURE MOUSEUP
		LPARAMETERS nButton, nShift, nXCoord, nYCoord
		LOCAL lnI, lnDist, lnTime, lnVel, lnRange
		
		IF nButton = 1 AND This.nYC != nYCoord AND This.nYC != 0	
			lnDist = ABS(This.nYC - nYCoord) / 10
			lnTime = SECONDS() - This.nSec	
			lnVel = CEILING(lnDist / lnTime)
			
			lnRange = CEILING(lnVel / 5)
			IF lnRange > 20
				lnRange = 20
			ENDIF

			IF This.nYC > nYCoord
				* Scroll Up
				FOR lnI = 1 TO lnRange
					Thisform.nNum = Thisform.nNum + 20
					Thisform.setviewport(0,Thisform.nNum)
					Thisform.Sleep(lnI^3 / 50)
				ENDFOR
			ELSE
				* Scroll Down
				FOR lnI = 1 TO lnRange
					IF Thisform.nNum <= 0
						EXIT
					ENDIF
					Thisform.nNum = Thisform.nNum - 20
					Thisform.setviewport(0,Thisform.nNum)
					Thisform.Sleep(lnI^3 / 50)
				ENDFOR
			ENDIF
		ENDIF
	ENDPROC
ENDDEFINE
*}V&U MP 2013-08-12

*{V&U MP 2013-08-12
* Class.......: proofsheet
* Author......: HC www.hceood.eu
* Created.....: August 2013
* Used by.....: Print/Preview
* Description.: Thumbnails functionality 
* Parameter...:	
* Return Value: 
*(Change list):
#DEFINE SPACE_PIXELS 10
#DEFINE PR_IMGBTN_TOP   "reportprocessing\images\prefirst.bmp" 
#DEFINE PR_IMGBTN_PREV   "reportprocessing\images\preprev.bmp"
#DEFINE PR_IMGBTN_NEXT   "reportprocessing\images\prenext.bmp"
#DEFINE PR_IMGBTN_BOTT   "reportprocessing\images\prelast.bmp"

DEFINE CLASS proofsheet AS frmReport
	HEIGHT 				= 274
	WIDTH 				= 622
	SCROLLBARS 			= 3
	DOCREATE 			= .T.
	AUTOCENTER 			= .T.
	*TITLEBAR			= 0
	*ALWAYSONTOP		= .T.
	*{V&U MS 2013-07-31
	SHOWWINDOW 			= 1 && In Top-Level Form
	*DESKTOP			= .T.
	BorderStyle 		= 2
	ControlBox 			= .F.
	Movable 			= .F.
	*}V&U MS 2013-07-31
	closable 			= .F.
	currentPage 		= 0
	REPORTLISTENER 		= .NULL.
	lstarted 			= .F.
	nPages 				= 1
	lpainted 			= .F.
	nCurrShape 			= 0
	NAME 				= "proofsheet"
	nPageSet			= 1
	lShowDone			= .F.
	linactive           = .F.
	nOtherThenProofObj	= 0
	nMaxMiniatureItem	= IIF(VARTYPE(goProgram) = "O", goProgram.nMaxThumbNailsItems, 32)
	OldEscFUNCTION		= ""
	
	PROCEDURE INIT
	
		IF NOT DODEFAULT()
			RETURN .F.
		ENDIF
		
		*{V&U MS 2013-08-02
		This.AddObject("shpRed", "Shape")
		This.shpRed.BorderWidth = 2
		This.shpRed.BorderColor = RGB(255, 0, 0)
		This.shpRed.Visible = .F.
		This.shpRed.BackStyle = 0
		This.nOtherThenProofObj = This.nOtherThenProofObj + 1
		*}V&U MS 2013-08-02
		
		This.ADDOBJECT("PageSetFirst","PageSetBtn")
		This.nOtherThenProofObj = This.nOtherThenProofObj + 1

		WITH This.PageSetFirst
			.TOP  		= 1
			.LEFT 		= SPACE_PIXELS
			.CAPTION 	= ""
			.PICTURE 	=  PR_IMGBTN_TOP
			.TOOLTIPTEXT=   IIF(goProgram.lRuntimeLocalization,goLocalize.cCONTEXT_MENU_PROMPT_FIRST_PAGE_LOC,CONTEXT_MENU_PROMPT_FIRST_PAGE_LOC)
			.cType		= "FIRST"
			.VISIBLE 	= .F.
		ENDWITH

		This.ADDOBJECT("PageSetPrev","PageSetBtn")
		This.nOtherThenProofObj = This.nOtherThenProofObj + 1

		WITH This.PageSetPrev
			.TOP  		= 1
			.LEFT 		= This.PageSetFirst.LEFT + This.PageSetFirst.WIDTH + 2
			.CAPTION 	= ""
			.PICTURE 	= PR_IMGBTN_PREV
			.TOOLTIPTEXT=  IIF(goProgram.lRuntimeLocalization,goLocalize.cUI_TOOLBAR_TT_BACK_LOC,UI_TOOLBAR_TT_BACK_LOC)
			.cType		= "PREV"
			.VISIBLE 	= .F.
		ENDWITH

		This.ADDOBJECT("PageSetNext","PageSetBtn")
		This.nOtherThenProofObj = This.nOtherThenProofObj + 1

		WITH This.PageSetNext
			.TOP  		= 1
			.LEFT 		= This.PageSetPrev.LEFT + This.PageSetPrev.WIDTH + 2
			.CAPTION 	= ""
			.PICTURE 	= PR_IMGBTN_NEXT
			.TOOLTIPTEXT=  IIF(goProgram.lRuntimeLocalization,goLocalize.cUI_TOOLBAR_TT_NEXT_LOC,UI_TOOLBAR_TT_NEXT_LOC)
			.cType		= "NEXT"
			.VISIBLE 	= .F.
		ENDWITH

		This.ADDOBJECT("PageSetLast","PageSetBtn")
		This.nOtherThenProofObj = This.nOtherThenProofObj + 1

		WITH This.PageSetLast
			.TOP  		= 1
			.LEFT 		= This.PageSetNext.LEFT + This.PageSetNext.WIDTH + 2
			.CAPTION 	= ""
			.PICTURE 	= PR_IMGBTN_BOTT
			.TOOLTIPTEXT= IIF(goProgram.lRuntimeLocalization,goLocalize.cCONTEXT_MENU_PROMPT_LAST_PAGE_LOC,CONTEXT_MENU_PROMPT_LAST_PAGE_LOC)
			.cType		= "LAST"
			.VISIBLE 	= .F.
		ENDWITH


		This.ADDOBJECT("PageSetCaption","Label")
		This.nOtherThenProofObj = This.nOtherThenProofObj + 1

		WITH This.PageSetCaption
			.LEFT 		= This.PageSetFirst.LEFT && + This.PageSetLast.WIDTH + 10
			.AUTOSIZE 	= .T.
			.CAPTION 	= ""
			.FONTNAME	= "Arial"
			.FONTSIZE	= 8
			.FONTBOLD 	= .T.
			.TOP  		= This.PageSetFirst.TOP + (This.PageSetFirst.HEIGHT)
			.VISIBLE 	= .F.
		ENDWITH
	ENDPROC

	*{V&U MS 2014-03-11
	PROCEDURE KeyPress
		PARAMETERS nkeycode, nshiftaltctrl
		
		IF nkeycode = key_escape
			IF TYPE("This.oParent.PreviewForm.Class") == "C"
				This.oParent.PreviewForm.Release()
				This.Release()
				NODEFAULT
			ENDIF 
		ENDIF
	ENDPROC 
	*}V&U MS 2014-03-11

	PROCEDURE RefreshPageBtn
		This.PageSetNext.REFRESH()
		This.PageSetPrev.REFRESH()
	ENDPROC

	PROCEDURE SetReport
		LPARAMETERS oReport
		This.REPORTLISTENER = m.oReport
		This.nPages = m.oReport.OUTPUTPAGECOUNT
	ENDPROC

	PROCEDURE RESIZE
		
		IF This.WINDOWSTATE = 1 && Minimized
			This.linactive = .T.
		ELSE
			IF This.linactive = .T.
				This.lShowDone = .F.
				This.PAINT()
				This.linactive = .F.
			ENDIF
		ENDIF

		*{V&U MS 2014-03-27 6452
		This.onShow()
		*}V&U MS 2014-03-27
	ENDPROC

	PROCEDURE ACTIVATE
		This.lShowDone = .F.
	ENDPROC

	PROCEDURE QUERYUNLOAD
		This.HIDE()
		ACTIVATE SCREEN
	ENDPROC

	PROCEDURE nPageSet_assign
		LPARAMETERS vNewValue
		DO CASE
			CASE (This.nPageSet == CEILING(This.nPages / This.nMaxMiniatureItem)) ;
					AND (m.vNewValue <> CEILING(This.nPages / This.nMaxMiniatureItem))
				* We have to display ALL miniatures	
				FOR m.i = This.nOtherThenProofObj+1 TO This.OBJECTS.COUNT
					IF NOT This.OBJECTS[m.i].VISIBLE
						This.OBJECTS[m.i].VISIBLE = .T.
					ENDIF
				ENDFOR
				
			CASE (This.nPageSet <> CEILING(This.nPages / This.nMaxMiniatureItem)) ;
					AND (m.vNewValue == CEILING(This.nPages / This.nMaxMiniatureItem))
				* We have to display only some miniatures
				FOR m.i = This.nOtherThenProofObj+1 TO This.OBJECTS.COUNT
					IF m.i > This.nPages - ((CEILING(This.nPages / This.nMaxMiniatureItem)-1) * ;
						This.nMaxMiniatureItem) + This.nOtherThenProofObj
						This.OBJECTS[m.i].VISIBLE = .F.
					ENDIF
				ENDFOR
		ENDCASE
		
		This.nPageSet = m.vNewValue	
		This.SetProofCaption()
		*{V&U MS 2014-03-27 6452
		This.onShow()
		*}V&U MS 2014-03-27
	ENDPROC

	PROCEDURE SetProofCaption
	* ---------------------- *
	* Calculate the caption! *
	* ---------------------- *
		LOCAL cMessage, nFirstPage, nLastPage

		m.nFirstPage 	= ((This.nPageSet-1) * This.nMaxMiniatureItem) + 1     
		m.nLastPage  	= MIN(This.nPageSet * This.nMaxMiniatureItem, This.nPages)                
		m.cMessage	= IIF(goProgram.lRuntimeLocalization,goLocalize.cCAP_PAGESFROMTO, CAP_PAGESFROMTO) 
		This.PageSetCaption.CAPTION = STRTRAN(STRTRAN(m.cMessage, "%FP%", TRANSFORM(m.nFirstPage)), "%LP%", TRANSFORM(m.nLastPage))
	ENDPROC

	PROCEDURE ReportListener_Assign
		LPARAMETERS oNewValue
		This.REPORTLISTENER = m.oNewValue
		This.DoResizeProofSheet()
	ENDPROC

	PROCEDURE nMaxMiniatureItem_Assign
		LPARAMETERS nNewItem
		This.nMaxMiniatureItem = m.nNewItem
		This.DoResizeProofSheet()
	ENDPROC

	PROCEDURE DoResizeProofSheet
		IF NOT ISNULL(This.REPORTLISTENER)
			*Recalculating the Proof Page size to display the max miniature at one time
			nProofWidth  = This.REPORTLISTENER.GETPAGEWIDTH() / 96
			nProofHeight = This.REPORTLISTENER.GETPAGEHEIGHT() / 96
			
			*Calculating the max col
			nMaxScreenWToConsidere = (_SCREEN.WIDTH /5) * 4		&&  Just for "estetics"
			nMaxScreenHToConsidere = (_SCREEN.HEIGHT/5) * 4		&&  Just for "estetics"
			nDiv = nProofWidth+SPACE_PIXELS
			nNbCol = INT((nMaxScreenWToConsidere - SPACE_PIXELS) / nDiv)

			IF nNbCol >= This.nPages
				* The width does need to be so large...
				* We will keep only the space needed do display the pages!
				nNbCol = This.nPages
			ENDIF

			* Now, calculating the width to set the form
			* This.WIDTH = SPACE_PIXELS + (nNbCol * (nProofWidth+SPACE_PIXELS))
			This.WIDTH = 200   &&SPACE_PIXELS + (nNbCol * (nProofWidth+SPACE_PIXELS))
			* -------------------
			* Now for the hight
			* -------------------
			* Calculating number of row...
			* We will keep in mind that if there is less item to display than the miximum possible
			* then we dont need to reserve the whole place
			nNbRow = CEILING(MIN(This.nMaxMiniatureItem, This.nPages)/ nNbCol)
			* Checking if we must display nav btn or not
			IF CEILING(This.nPages / This.nMaxMiniatureItem) > 1
				nBaseHeight =  36 + SPACE_PIXELS &&_goFP._nBtSize + SPACE_PIXELS
			ELSE
				nBaseHeight = SPACE_PIXELS
			ENDIF
			* Now, calculating the height to set the form
			This.HEIGHT = MIN(nMaxScreenHToConsidere, nBaseHeight + (nNbRow * (nProofHeight+SPACE_PIXELS)))
			* Is the Height sufficient to display all of the miniatures?
			* If not, there will be a vertical scrool bar and we must reserve space for that.
			IF This.HEIGHT < nBaseHeight + (nNbRow * (nProofHeight+SPACE_PIXELS))
			* Add some space for the scrool bar to the WIDTH
				This.WIDTH = This.WIDTH + 20
			ENDIF
			*This.AUTOCENTER = .t.	&& Auto center the proof sheet
		ENDIF
	ENDPROC

	PROCEDURE PAINT
		IF (NOT ISNULL(This.REPORTLISTENER))
			IF NOT This.lShowDone
				FOR m.i = ((This.nPageSet - 1) * This.nMaxMiniatureItem) + 1 TO This.nPageSet * This.nMaxMiniatureItem
					IF TYPE("This.Objects[m.i - ((This.nPageSet - 1) * This.nMaxMiniatureItem) + This.nOtherThenProofObj]") == "O"
						IF m.i > This.nPages
							EXIT
						ELSE
							This.OBJECTS[m.i - ((This.nPageSet - 1) * This.nMaxMiniatureItem) + This.nOtherThenProofObj].TOOLTIPTEXT = ;
							IIF(goProgram.lRuntimeLocalization,goLocalize.cCAP_PAGE, CAP_PAGE) + " #" + " " + TRANSFORM(m.i)					
							This.REPORTLISTENER.OUTPUTPAGE( m.i, This.OBJECTS[m.i - ((This.nPageSet - 1) * This.nMaxMiniatureItem) + This.nOtherThenProofObj],2)
						ENDIF
					ELSE
						EXIT
					ENDIF
				ENDFOR
				
				This.lShowDone = .T.
			ENDIF
		ENDIF
	ENDPROC

	PROCEDURE SHOW
		LPARAMETERS nStyle

		*{V&U MS 2014-03-27 6452
		This.onShow()
		*}V&U MS 2014-03-27
		DODEFAULT(m.nStyle)
	ENDPROC
	
	*{V&U MS 2014-03-27 6452
	PROCEDURE onShow()
		IF CEILING(This.nPages / This.nMaxMiniatureItem) > 1
		* Since we have multi pages set, then we must display NAV buttons.
			IF NOT This.lstarted
				This.PageSetFirst.VISIBLE 	= .T.
				This.PageSetPrev.VISIBLE 	= .T.
				This.PageSetNext.VISIBLE 	= .T.
				This.PageSetLast.VISIBLE 	= .T.
				This.PageSetCaption.VISIBLE = .T.
			ENDIF
			iRowOffset = SPACE_PIXELS * 3 + 36 - 20 
		ELSE
		* There is only one page set, so there is no need to display navigation button!
		* They are alerady
			iRowOffset = SPACE_PIXELS	&& Set the start to the top!
		ENDIF
		
		iColOffset = SPACE_PIXELS
		nProofWidth  = This.REPORTLISTENER.GETPAGEWIDTH() / 43 &&96
		nProofHeight = This.REPORTLISTENER.GETPAGEHEIGHT() / 43 &&96
		iColCount  = INT((THISFORM.WIDTH - iColOffset)/ (nProofWidth + SPACE_PIXELS))	&& Number of column that can fit in the space allowed
		nCurCol = 1
		This.lShowDone = .F.
		
		FOR m.i = ((This.nPageSet - 1) * This.nMaxMiniatureItem) + 1 TO MIN(This.nPageSet * This.nMaxMiniatureItem, This.nPages)
			* Calculate the objectID here to facilitate reading the code
			nObjectID = m.i - ((This.nPageSet - 1) * This.nMaxMiniatureItem) + This.nOtherThenProofObj
			IF NOT This.lstarted
				This.ADDOBJECT(SYS(2015),"ProofShape")
				This.OBJECTS[nObjectID].VISIBLE = .T.

				This.OBJECTS[nObjectID].WIDTH  = nProofWidth
				This.OBJECTS[nObjectID].HEIGHT = nProofHeight
			ENDIF
			* Arrange shapes on form:
			TRY
				This.OBJECTS[nObjectID].TOP  	= iRowOffset
				This.OBJECTS[nObjectID].LEFT 	= SPACE_PIXELS + ((nCurCol-1) * (nProofWidth+SPACE_PIXELS))		&& iColOffset
				This.OBJECTS[nObjectID].PAGENO = m.i
				nCurCol = nCurCol + 1
				IF nCurCol > iColCount
					nCurCol = 1
					iRowOffset = iRowOffset + SPACE_PIXELS + This.OBJECTS[nObjectID].HEIGHT
				ENDIF
			CATCH
			ENDTRY
		ENDFOR
		
		*{V&U MS 2013-08-02
		WITH This
			FOR lnI = .nOtherThenProofObj + 1 TO .OBJECTS.COUNT
				IF .OBJECTS[lnI].pageno = .oParent.PREVIEWFORM.CurrentPage
					.shpRed.Top 	= .OBJECTS[lnI].Top - 5
					.shpRed.Left 	= .OBJECTS[lnI].Left - 5
					.shpRed.Width 	= .OBJECTS[lnI].Width + 10
					.shpRed.Height 	= .OBJECTS[lnI].Height + 10
					.shpRed.Visible = .T.
					EXIT 
				ENDIF 
			ENDFOR 
		ENDWITH 
		*}V&U MS 2013-08-02
		This.lstarted = .T.
	ENDPROC 
	*}V&U MS 2014-03-27

	PROCEDURE DESTROY
		This.REPORTLISTENER = NULL
	ENDPROC
ENDDEFINE
*}V&U MP 2013-08-13

*-------------------------------------------------------
* Function....: DoVfxDataExplorer
* Called by...: Menu Tools -> Data Explorer
* Abstract....: 
* Returns.....:	
*
* Parameters..: 
*
* Notes.......: Checks for VfxDataExplorer. If not available Downwload it.
*				Starts the app
*-------------------------------------------------------
FUNCTION DoVfxDataExplorer()

	LOCAL 	lcVfxDataExplorerAppName, lcDEDownloadLink, lcZIPFileName, lcSys16, lcExePath, ;
			lcDEAPPPathFileName, loDownload, llResult, lnErrorNo, loArchive, loException AS Exception
	
	lcVfxDataExplorerAppName = "VfxDataExplorer.app"
	IF FILE(lcVfxDataExplorerAppName)
		DO (lcVfxDataExplorerAppName) WITH "", goProgram.cLangID
	ELSE
		IF TYPE("goSystem.install_de") == "M"
			lcDEDownloadLink = MLINE(goSystem.install_de, 1)
		ELSE 
			IF TYPE("goSystem.install_de") == "C"
				lcDEDownloadLink = MLINE(goSystem.install_de, 1)
			ENDIF 
		ENDIF 
		IF LEFT(ALLTRIM(lcDEDownloadLink), 2) == "D:"
			lcDEDownloadLink = ALLTRIM(SUBSTR(lcDEDownloadLink, AT(":", lcDEDownloadLink) + 1))  
		ENDIF 
		IF EMPTY(lcDEDownloadLink) OR !("vfxdataexplorer.zip" $ LOWER(lcDEDownloadLink))
			goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, ;
									goLocalize.cMSG_INVALIDDOWNLOADLINKFORVFXDATAEXPLORERAPP, ;
									MSG_INVALIDDOWNLOADLINKFORVFXDATAEXPLORERAPP), 0+16, _Screen.Caption)
			RETURN .F.
		ENDIF

		lcZIPFileName = SUBSTR(lcDEDownloadLink, RAT("/", lcDEDownloadLink) + 1)	&& Get FileName from link

		IF VERSION(2) <> 2		
			lcSys16 = SYS(16, 0)
			IF AT(":\", lcSys16) > 0
				lcSys16 = SUBSTR(lcSys16, AT(":\", lcSys16) - 1)
			ENDIF 
			lcExePath = JUSTPATH(lcSys16)
		ELSE 
			lcExePath = SYS(5) + CURDIR()
		ENDIF 
		lcDEAPPPathFileName = ADDBS(lcExePath) + FORCEEXT(lcZIPFileName, "app") 
		IF ADIR(laDummy, lcDEAPPPathFileName) = 0
			loDownload = CREATEOBJECT("CDownload")
			DECLARE INTEGER DeleteUrlCacheEntry IN Wininet string lcUrl
			DeleteUrlCacheEntry(lcDEAPPPathFileName)
			TRY 
				IF FILE(ADDBS(SYS(2023)) + lcZIPFileName)
					ERASE (ADDBS(SYS(2023)) + lcZIPFileName)
				ENDIF
			CATCH
			ENDTRY
			llResult = loDownload.ExecMacro("D: " + lcDEDownloadLink, .T.)
			CLEAR DLLS DeleteUrlCacheEntry
			lnErrorNo = 0
			IF llResult
				TRY
					IF ADIR(laDummy, ADDBS(SYS(2023)) + lcZIPFileName) = 1
						loArchive = CREATEOBJECT("cArchive")
						loArchive.lQuietMode = .T.
						llResult = loArchive.ExtractFromArchive(ADDBS(SYS(2023)) + lcZIPFileName, "*.*", lcExePath, "")
						RELEASE loArchive
						IF !llResult
							lnErrorNo = -1
						ELSE
							ERASE (ADDBS(SYS(2023)) + lcZIPFileName)
						ENDIF
					ELSE
						lnErrorNo = 1 && File "name" does not exist. 
					ENDIF
				CATCH TO loException
					lnErrorNo = loException.ErrorNo
				* Do Nothing
				ENDTRY 
				
				IF lnErrorNo = 0
					goProgram.vfxWaitWindow(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DOWNLOADSUCCESSFUL, MSG_DOWNLOADSUCCESSFUL),,,.T.,,,)
				ENDIF
			ELSE
				IF !EMPTY(loDownload.LastErrorText)
					goProgram.vfxWaitWindow(loDownload.LastErrorText)
				ENDIF
			ENDIF
		ENDIF
		IF ADIR(laDummy, lcDEAPPPathFileName) = 1
			lnErrorNo = 0
			TRY
				DO (lcDEAPPPathFileName) WITH "", goProgram.cLangID
			CATCH TO loException
				lnErrorNo = loException.ErrorNo
			ENDTRY
			RETURN (lnErrorNo = 0)
		ELSE
			RETURN .F.
		ENDIF	
	ENDIF 	
ENDFUNC 

*{ HC VM 2015-05-25
*-------------------------------------------------------
* Function....: CheckSMB
* Called by...: 
*
* Abstract....: Check if SMB CACHING are OFF
*
* Returns.....: Logical
*
* Parameters..: 
*
* Notes.......: 
*-------------------------------------------------------
PROCEDURE CheckSMB
	LOCAL lnMajorVersion, llRes, loReg, lnErrorCode, lcOffValue, lcRegPath, lcRegName, lnErr

	* drivetype # 4
	IF DRIVETYPE(SYS(5)) # 4
		RETURN .T.
	ENDIF 
	
	* Windows < Vista
	lnMajorVersion = VAL(OS(3))
	IF lnMajorVersion < 6 OR (lnMajorVersion = 6 AND VAL(OS(4)) = 0)
		RETURN .T.
	ENDIF 
	
	* create registry object
	lnErr = 0
	TRY
		loReg = NEWOBJECT("cregistry")		
	CATCH TO oError
		lnErr = oError.ErrorNo
	ENDTRY
	IF lnErr <> 0
		RETURN .T.
	ENDIF
	
	* Check SMB
	llRes = .T.
	lcOffValue = "0"
	lcRegPath = "SYSTEM\CurrentControlSet\services\LanmanWorkstation\Parameters"
	
	lcRegName = "FileInfoCacheLifetime"
	lcBuffer = ""	
	lnErrorCode = loReg.GetRegKey(lcRegName, @lcBuffer, lcRegPath, HKEY_LOCAL_MACHINE)
	
	IF lnErrorCode # ERROR_SUCCESS OR lcBuffer # lcOffValue && missing or has a different value
		llRes = .F.
	ENDIF 
	
	IF llRes 
		lcRegName = "FileNotFoundCacheLifetime"
		lcBuffer = ""	
		lnErrorCode = loReg.GetRegKey(lcRegName, @lcBuffer, lcRegPath, HKEY_LOCAL_MACHINE)
		
		IF lnErrorCode # ERROR_SUCCESS OR lcBuffer # lcOffValue && missing or has a different value
			llRes = .F.
		ENDIF	
	ENDIF 
	
	IF llRes 
		lcRegName = "DirectoryCacheLifetime"
		lcBuffer = ""	
		lnErrorCode = loReg.GetRegKey(lcRegName, @lcBuffer, lcRegPath, HKEY_LOCAL_MACHINE)
		
		IF lnErrorCode # ERROR_SUCCESS OR lcBuffer # lcOffValue && missing or has a different value
			llRes = .F.
		ENDIF	
	ENDIF 
	
	RETURN llRes
ENDPROC 
*} HC VM 2015-05-25

*{ HC BB 2015-08-16, 6812
*---------------------------------------------------------
* Function....: QRBarcodeImage()
* Author......: HCEOOD http://www.hceood.eu/
* Created.....: August 2015
* Abstract....: Generates QR Barcode image with BarCodeLibrary.DLL
*
* Returns.....:	String - Generated QR Barcode Image file name
*
* Parameters..:	tcText: Text to encode
*   						tcFileName: Imagen File Name (optional)
*  							tnSize: Imagen Size [2..12] (default = 4)
*     									2 = 66 x 66 (in pixels)
*     									3 = 99 x 99
*     									4 = 132 x 132
*     									5 = 165 x 165
*     									6 = 198 x 198
*     									7 = 231 x 231
*     									8 = 264 x 264
*     									9 = 297 x 297
*     									10 = 330 x 330
*     									11 = 363 x 363
*     									12 = 396 x 396
*   						tnType: Imagen Type [BMP, JPG or PNG] (default = 0)
*     									0 = BMP
*     									1 = JPG
*     									2 = PNG
* Notes.......: Downloads BarCodeLibrary.dll if not found
*---------------------------------------------------------
PROCEDURE QRBarcodeImage(tcText, tcFileName, tnSize, tnType)

	LOCAL lcType, lcFolder, lcFileName, lcBuffer, lcQRDownloadLink, lcQRPath, lcQRDllPathFileName, ;
				lcZIPFileName, loDownload, lcUrl, llResult, lnErrorNo, loArchive, loException, lnSelect 
	
	llResult = .T.
	lnSelect = SELECT()
	* Default size:  132 x 132 pixels
	IF VARTYPE(tnSize) <> "N"
		tnSize = 4 
	ENDIF

	* Defaul type: PNG
	IF VARTYPE(tnType) <> "N"
		tnType = 2  
	ENDIF

	tnSize = MIN(MAX(tnSize, 2), 12)
	tnType = MIN(MAX(tnType, 0), 2)
	lcType = IIF(tnType = 1, "JPG", IIF(tnType = 2, "PNG", "BMP"))

	*{HC MS 2015-11-17
	lcFolder = ADDBS(SYS(2023) + '\QR' )
	IF EMPTY(tcFileName)
		lcFileName = FORCEEXT(lcFolder + SYS(2015), lcType)
	ELSE
		IF !EMPTY(JUSTPATH(tcFileName))
			lcFolder = JUSTPATH(tcFileName)
			lcFileName = FORCEEXT(tcFileName, lcType)
		ELSE 
			lcFileName = FORCEEXT(lcFolder + tcFileName, lcType)
		ENDIF 
	ENDIF
	IF NOT DIRECTORY(lcFolder)
		MD (lcFolder)
	ENDIF
	*}HC MS 2015-11-17
	
	lcBuffer = ""
	lcQRDownloadLink = ""	
	lcQRPath = ADDBS(SYS(2003))
	lcQRDllPathFileName = lcQRPath + "BarCodeLibrary.dll" 

	* Download BarCodeLibrary.dll if not found
	IF ADIR(laDummy, lcQRDllPathFileName) = 0 
		IF TYPE("goSystem.install_qr") == "M"
			lcQRDownloadLink = MLINE(goSystem.install_qr, 1)
		ELSE 
			IF TYPE("goSystem.install_qr") == "C"
				lcQRDownloadLink = MLINE(goSystem.install_qr, 1)
			ENDIF 
		ENDIF 
		IF LEFT(ALLTRIM(lcQRDownloadLink), 2) == "D:"
			lcQRDownloadLink = ALLTRIM(SUBSTR(lcQRDownloadLink, AT(":", lcQRDownloadLink) + 1))  
		ENDIF 
		IF EMPTY(lcQRDownloadLink) OR !("QR_Code.Zip" $ lcQRDownloadLink)
			goProgram.vfxMessageBox(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_INVALIDDOWNLOADLINKFORQRCODES, ;
									MSG_INVALIDDOWNLOADLINKFORQRCODES), ;
									0 + 16, _Screen.Caption)
			RETURN .F.
		ENDIF
		
		* Get FileName from link
		lcZIPFileName = SUBSTR(lcQRDownloadLink, RAT("/", lcQRDownloadLink) + 1)
		
		loDownload = CreateObject("CDownload")
		DECLARE INTEGER DeleteUrlCacheEntry IN Wininet string lcUrl
		DeleteUrlCacheEntry(lcQRDownloadLink)
		TRY 
			IF FILE(ADDBS(SYS(2023)) + lcZIPFileName) 
				ERASE (ADDBS(SYS(2023)) + lcZIPFileName) 
			ENDIF
		CATCH
		ENDTRY
		llResult = loDownload.ExecMacro("D: " + lcQRDownloadLink, .T.)
		CLEAR DLLS DeleteUrlCacheEntry
		
		lnErrorNo = 0
		IF llResult
			TRY
				IF ADIR(laDummy, ADDBS(SYS(2023)) + lcZIPFileName) = 1
					loArchive = CREATEOBJECT("cArchive")
					loArchive.lQuietMode = .T.
					llResult = loArchive.ExtractFromArchive(ADDBS(SYS(2023)) + lcZIPFileName, "*.*", lcQRPath, "")
					RELEASE loArchive
					IF !llResult
						lnErrorNo = -1
					ELSE
						ERASE (ADDBS(SYS(2023)) + lcZIPFileName)
					ENDIF
				ELSE
					* File "name" does not exist.
					lnErrorNo = 1  
				ENDIF
			CATCH TO loException
				lnErrorNo = loException.ErrorNo
			ENDTRY 
			
			IF lnErrorNo = 0
				goProgram.vfxWaitWindow(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_DOWNLOADSUCCESSFUL, MSG_DOWNLOADSUCCESSFUL), , , .T., , ,)
			ENDIF
		ELSE
			IF !EMPTY(loDownload.LastErrorText)
				goProgram.vfxWaitWindow(loDownload.LastErrorText)
			ENDIF
		ENDIF
	ENDIF 

	*{HC MS 2015-11-17, Check result before use. Return an "" if not successful
	IF llResult
		* Declare the functions of BarCodeLibrary.dll
		DECLARE INTEGER GenerateFile IN BarCodeLibrary.DLL ;
		  STRING cData, STRING cFileName

		DECLARE INTEGER SetConfiguration IN BarCodeLibrary.DLL ;
		  INTEGER nSize, INTEGER nImageType

		* Create QR Barcode Image
		SetConfiguration(tnSize, tnType)
		GenerateFile(tcText, lcFileName)

		CLEAR DLLS SetConfiguration, GenerateFile
	ENDIF 	
	
	IF !EMPTY(lnSelect) AND USED(lnSelect)
		SELECT (lnSelect)
	ENDIF
	
	IF !llResult
		RETURN ""
	ENDIF 
	*}HC MS 2015-11-17
	RETURN lcFileName
ENDPROC
*} HC BB 2015-08-16

*-------------------------------------------------------
* Class.......: cDay
* Called by...: cCalendarAlternateVfxBase
*
* Abstract....: Definition of the class of Form Calendar
*
* Parameters..:
*
* Notes.......:
*-------------------------------------------------------
*{ HC MS 2016-02-25, 
DEFINE CLASS cDay AS TextBox
	Alignment = 3
  BackStyle = 1
  BorderStyle = 0
  Height = 24
  InputMask = "99"
  ReadOnly = .T.
  SelectOnEntry = .T.
  SpecialEffect = 1
  Top = 83
  Width = 26
  Style = 0

  PROCEDURE KEYPRESS
    LPARAMETERS nKeyCode, nShiftAltCtrl

    IF nKeycode = 13
			Thisform.cmdOK.Click()
    ENDIF
    IF nKeyCode = 4 .AND. nShiftAltCtrl = 0
      KEYBOARD "{TAB}"
      NODEFAULT
      RETURN(.F.)
    ENDIF
    IF nShiftAltCtrl = 1
      NODEFAULT
      RETURN(.F.)
    ENDIF

    IF (nKeyCode = 6 .OR. nKeyCode = 1) .AND. nShiftAltCtrl = 0
      NODEFAULT
      RETURN(.F.)
    ENDIF

  ENDPROC

  PROCEDURE GOTFOCUS
    Thisform.nDay = This.Value
    Thisform.txtDate.Refresh()
    NODEFAULT
    =DODEFAULT()
    This.SelStart = 0
    This.SelLength = 999
  ENDPROC

  PROCEDURE CLICK
    This.SelStart = 0
    This.SelLength = 999
    lnCol = MCOL()
    lnRow = MROW()
    FOR lnX = 1 TO 5
      lnKey = INKEY("MH") 
      IF lnKey = 151 .AND. MCOL() = lnCol .AND. MROW() = lnRow
        Thisform.cmdOK.Click()
        EXIT
      ENDIF
    ENDFOR 
  ENDPROC

  PROCEDURE DBLCLICK
    Thisform.cmdOK.Click()
  ENDPROC

ENDDEFINE
*} HC MS 2016-02-25