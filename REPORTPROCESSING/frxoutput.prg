* REPORTOUTPUT WRAPPER PRG

#INCLUDE "INCLUDE\VFX.H"

LPARAMETERS tvType, tvReference, tvUnload

EXTERNAL TABLE OUTPUTAPP_INTERNALDBF

LOCAL oTemp, iType, iIndex, cType, cConfigTable, ;
	lSuccess, lSetTalkBackOn, lSafety, cFilter, cClass, cLib, cModule, ;
	oConfig, oError, lStringVar, lObjectMember, iParams, ;
	iUnload, iSelect, iSession, lSetTalkBackOnDefaultSession, vReturn, ;
	oSH

IF (SET("TALK") = "ON")
	SET TALK OFF
	lSetTalkBackOn = .T.
ENDIF

iParams = PARAMETERS()
iSession = SET("DATASESSION")

oSH = CREATEOBJECT("SH")

oSH.Execute(VFP_DEFAULT_DATASESSION)

iSelect = SELECT()

IF (SET("TALK") = "ON")
	SET TALK OFF
	lSetTalkBackOnDefaultSession = .T.
ENDIF


* if it is not integer, convert
* if it is lower than -1,
* this is a value private to REPORTOUTPUT.APP,
* potentially not even a ListenerType
* if it is not numeric, just set up the
* reference collection

DO CASE
	CASE VARTYPE(m.tvType) # "N"
		vReturn = ReportOutputConfig(OUTPUTAPP_CONFIGTOKEN_SETTABLE, .F., .F., m.oSH)
		DO ReportOutputCleanup WITH ;
			m.iSelect, m.lSetTalkBackOnDefaultSession, ;
			m.iSession, m.lSetTalkBackOn, m.oSH
		RETURN m.vReturn
	CASE ABS(m.tvType) # m.tvType AND m.tvType < LISTENER_TYPE_DEF
		vReturn = ReportOutputConfig(m.tvType, @m.tvReference, m.tvUnload, m.oSH)
		DO ReportOutputCleanup WITH ;
			m.iSelect, m.lSetTalkBackOnDefaultSession, ;
			m.iSession, m.lSetTalkBackOn, m.oSH
		RETURN m.vReturn
	OTHERWISE
		iType = INT(m.tvType)
ENDCASE

IF m.iParams = 3
	iUnload = VAL(TRANSFORM(m.tvUnload))
	IF VARTYPE(m.tvUnload) = "L" AND m.tvUnload
		vReturn = UnloadListener(m.iType)
		DO ReportOutputCleanup WITH ;
			m.iSelect, m.lSetTalkBackOnDefaultSession, ;
			m.iSession, m.lSetTalkBackOn, m.oSH
		RETURN m.vReturn
	ELSE
		IF m.iUnload > 0
			IF m.iUnload = OUTPUTAPP_LOADTYPE_UNLOAD
				vReturn = UnloadListener(m.iType)
				DO ReportOutputCleanup WITH ;
					m.iSelect, m.lSetTalkBackOnDefaultSession, ;
					m.iSession, m.lSetTalkBackOn, m.oSH
				RETURN m.vReturn
			ELSE
				DO UnloadListener WITH m.iType
			ENDIF
		ENDIF
	ENDIF
ENDIF

DO ReportOutputDeclareReference  WITH ;
	m.iParams, m.tvReference, m.lObjectMember, m.lStringVar


IF m.iType = LISTENER_TYPE_DEF
* always provide the reference fresh,
* do not use the collection
	oTemp = CREATEOBJECT("ReportListener")

ELSE

* check for public reference var (collection)
* if it is not available create

	cType = TRANSFORM(m.iType)

	iIndex = -1

	DO CheckPublicListenerCollection WITH m.cType, m.iIndex

	IF m.iIndex > -1
		oTemp = OUTPUTAPP_REFVAR.ITEM[m.iIndex]
	ELSE
* if they've passed in an existing object and
* it's not in the collection yet, add
* (SP1 change)
		IF TestListenerReference(m.tvReference)
			OUTPUTAPP_REFVAR.ADD(m.tvReference, m.cType)
* synch this up, JIC:
			DO CheckPublicListenerCollection WITH m.cType, m.iIndex
			IF m.iIndex > -1
				oTemp = m.tvReference
			ENDIF
		ENDIF
	ENDIF

	IF NOT TestListenerReference(m.oTemp)

* if it is not available,
* look for config file, choosing between built-in and
* on-disk

		oError = NULL
		STORE "" TO cClass, cLib, cModule

* try to open, error handle for
* unavailability

		DO GetConfigObject WITH m.oConfig

		TRY
			SELECT 0

			iIndex = -1
			DO CheckPublicListenerCollection WITH ;
				TRANSFORM(OUTPUTAPP_CONFIGTOKEN_SETTABLE), m.iIndex

			IF m.iIndex > -1
				cConfigTable = OUTPUTAPP_REFVAR.ITEM[m.iIndex]
			ELSE
				cConfigTable = oConfig.GetConfigTable()
* the collection will have been created by
* CheckPublicListenerCollection
				OUTPUTAPP_REFVAR.ADD(m.cConfigTable, TRANSFORM(OUTPUTAPP_CONFIGTOKEN_SETTABLE))
			ENDIF

			USE (m.cConfigTable ) ALIAS OutputConfig SHARED

			IF m.oConfig.VerifyConfigTable("OutputConfig")

* look for filter records first:

* OBJTYPE   110   identifies a configuration record
* OBJCODE   1    Configuration item type. 1= registry filter
* OBJNAME   not used
* OBJVALUE   not used
* OBJINFO   Filter expression

				SELECT OutputConfig
				SET ORDER TO 0
				LOCATE && GO TOP
				LOCATE FOR ObjType = OUTPUTAPP_OBJTYPE_CONFIG AND ;
					ObjCode = OUTPUTAPP_OBJCODE_FILTER AND ;
					NOT (EMPTY(ObjInfo) OR DELETED())
				IF FOUND()
					cFilter = " AND (" + ALLTR(ObjInfo) + ")"
				ELSE
					cFilter = ""
				ENDIF

* check for type record for the passed type and
* not deleted and in the filter

* OBJTYPE   100   identifies a Listener registry record
* OBJCODE   Listener Type   values -1, 0, 1, and 2 supported by default
* OBJNAME   Class to instantiate   may be ReportListener (base class)
* OBJVALUE   Class library or procedure file   may be blank
* OBJINFO   Module/Application containing library   may be blank

				LOCATE && GO TOP

				LOCATE FOR ObjType = OUTPUTAPP_OBJTYPE_LISTENER AND ;
					(ObjCode = m.iType)  ;
					&cFilter. AND (NOT DELETED())
				IF FOUND()
* get values
					cClass = ALLTRIM(ObjName)
					cLib = ALLTRIM(ObjValue)
					cModule = ALLTR(ObjInfo)

				ELSE

					DO GetSupportedListenerInfo WITH ;
						m.iType, m.cClass, m.cLib, m.cModule
				ENDIF

			ELSE

				IF ISNULL(m.oError) && should be
					oError = CREATEOBJECT("Exception")
*oError.Message = OUTPUTAPP_CONFIGTABLEWRONG_LOC
					IF VARTYPE(goprogram) = "O"
						oError.MESSAGE = IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_CONFIGTABLEWRONG_LOCO, OUTPUTAPP_CONFIGTABLEWRONG_LOCO) + ;
							IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_APPNAME_LOC, OUTPUTAPP_APPNAME_LOC) + CHR(13) + ;
							IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_CONFIGTABLEWRONG_LOCT, OUTPUTAPP_CONFIGTABLEWRONG_LOCT)
					ELSE
						oError.MESSAGE = OUTPUTAPP_CONFIGTABLEWRONG_LOCO + ;
							OUTPUTAPP_APPNAME_LOC + CHR(13) + ;
							OUTPUTAPP_CONFIGTABLEWRONG_LOCT
					ENDIF
				ENDIF

				IF OUTPUTAPP_DEFAULTCONFIG_AFTER_CONFIGTABLEFAILURE
					DO GetSupportedListenerInfo WITH ;
						m.iType, m.cClass, m.cLib, m.cModule
				ENDIF

			ENDIF

			IF USED("OutputConfig")
				USE IN OutputConfig
			ENDIF

			IF NOT EMPTY(m.cClass)
				IF NOT INLIST(UPPER(JUSTEXT(m.cModule)),"APP","EXE", "DLL")
* frxoutput can be built into the current app or exe
					cModule = ""
				ENDIF
				oTemp = NEWOBJECT(m.cClass, m.cLib, m.cModule)
			ENDIF
		CATCH TO oError
			EXIT
		FINALLY
* oSH.Execute(iSession)
* SET DATASESSION TO (iSession)
		ENDTRY

		IF NOT ISNULL(m.oError)
			DO ReportOutputCleanup WITH ;
				m.iSelect, m.lSetTalkBackOnDefaultSession, ;
				m.iSession, m.lSetTalkBackOn, m.oSH
			HandleError(m.oError)
		ELSE

			IF TestListenerReference(m.oTemp) AND ;
					PEMSTATUS(m.oTemp,"ListenerType", 5)
* see notes below, we don't
* prevent the assignment if not
* a listener but we do not want it
* in the collection nonetheless

				#IF OUTPUTAPP_ASSIGN_TYPE
					IF UPPER(m.oTemp.BASECLASS) == UPPER(m.oTemp.CLASS)
						oTemp.LISTENERTYPE = m.iType
					ENDIF
				#ENDIF
				OUTPUTAPP_REFVAR.ADD(m.oTemp, m.cType)
			ENDIF

		ENDIF

		STORE NULL TO oConfig, oError

	ENDIF

ENDIF


lSuccess =  TestListenerReference(m.oTemp)

* we don't test for listener baseclass --
* they could hide the property --
* also we get a more consistent
* error message letting the product
* handle things if the object does
* not descend from ReportListener
* however, we have to assign type as needed,
* and that will require a test.


IF m.lSuccess

	#IF OUTPUTAPP_ASSIGN_OUTPUTTYPE
		TRY
			oTemp.OUTPUTTYPE = m.iType
		CATCH WHEN .T.
* in case they
* hid or protected it,
* or have an assign method that errored
		ENDTRY
	#ENDIF

	DO CASE
		CASE m.iParams = 1
* nothing to assign, just store in the collection
		CASE m.lStringVar OR m.lObjectMember
			IF m.lStringVar AND TYPE(m.tvReference) = "U"
				PUBLIC &tvReference.
			ENDIF
			STORE m.oTemp TO (tvReference)
			#IF OUTPUTAPP_ASSIGN_TYPE
				IF PEMSTATUS(&tvReference.,"ListenerType",5) AND ;
						UPPER(m.oTemp.BASECLASS) == UPPER(m.oTemp.CLASS)
					&tvReference..LISTENERTYPE = m.iType
				ENDIF
			#ENDIF
		OTHERWISE
			tvReference = m.oTemp
			#IF OUTPUTAPP_ASSIGN_TYPE
				IF PEMSTATUS(m.tvReference,"ListenerType", 5) AND ;
						UPPER(m.oTemp.BASECLASS) == UPPER(m.oTemp.CLASS)
					m.tvReference.LISTENERTYPE = m.iType
				ENDIF
			#ENDIF
	ENDCASE
ELSE
	DO CASE
		CASE m.iParams = 1
* nothing to assign
		CASE m.lStringVar OR m.lObjectMember
			STORE NULL TO (tvReference)
		OTHERWISE
			tvReference = NULL
	ENDCASE
ENDIF

DO ReportOutputCleanup WITH ;
	m.iSelect, m.lSetTalkBackOnDefaultSession, ;
	m.iSession, m.lSetTalkBackOn, m.oSH

RETURN m.lSuccess  && not used by the product but might be used by somebody

PROC ReportOutputCleanup( ;
	tiSelect, tlResetTalkDefaultSession, tiSession, tlResetTalk, toSH )
toSH.Execute(VFP_DEFAULT_DATASESSION)  && JIC
SELECT (m.tiSelect)
IF m.tlResetTalkDefaultSession
	SET TALK ON
ENDIF
toSH.Execute(m.tiSession)
IF m.tlResetTalk
	SET TALK ON
ENDIF
toSH = NULL
ENDPROC

PROC TestListenerReference(toRef)

RETURN (VARTYPE(m.toRef) = "O") && AND ;
&& (UPPER(toRef.BASECLASS) == "REPORTLISTENER")

PROC GetSupportedListenerInfo(tiType, tcClass, tcLib, tcModule)
DO CASE
	CASE OUTPUTAPP_XBASELISTENERS_FOR_BASETYPES AND ;
			m.tiType = LISTENER_TYPE_PRN
		m.tcClass = OUTPUTAPP_CLASS_PRINTLISTENER
		m.tcLib = OUTPUTAPP_BASELISTENER_CLASSLIB

	CASE OUTPUTAPP_XBASELISTENERS_FOR_BASETYPES AND ;
			m.tiType = LISTENER_TYPE_PRV
		m.tcClass = OUTPUTAPP_CLASS_PREVIEWLISTENER
		m.tcLib = OUTPUTAPP_BASELISTENER_CLASSLIB

	CASE INLIST(m.tiType, LISTENER_TYPE_PRN, ;
			LISTENER_TYPE_PRV, ;
			LISTENER_TYPE_PAGED, ;
			LISTENER_TYPE_ALLPGS)
		tcClass = "ReportListener"
	CASE m.tiType = LISTENER_TYPE_HTML
		tcClass = OUTPUTAPP_CLASS_HTMLLISTENER
		tcLib = OUTPUTAPP_BASELISTENER_CLASSLIB
	CASE m.tiType = LISTENER_TYPE_XML
		tcClass = OUTPUTAPP_CLASS_XMLLISTENER
		tcLib = OUTPUTAPP_BASELISTENER_CLASSLIB
	CASE m.tiType = LISTENER_TYPE_DEBUG
		tcClass = OUTPUTAPP_CLASS_DEBUGLISTENER
		tcLib = OUTPUTAPP_BASELISTENER_CLASSLIB
	OTHERWISE
* ERROR here?
* No, let product handle it consistently.
ENDCASE

ENDPROC

PROC ReportOutputConfig(tnType, tvReference, tvUnload, toSH)
* NB: early quit in case somebody
* calls the thing improperly,
* even from the command line with a SET PROC
IF VARTYPE(m.tnType) # "N"
	RETURN .F.
ENDIF
* can support other things besides writing the
* table here
LOCAL iSession, oSession, oError, oConfig, cDBF, lSuccess, cType, iIndex, lcSys16, lnPos
oError = NULL
oConfig = NULL
iSession = SET("DATASESSION")
lSuccess = .F.
IF VARTYPE(goprogram) = "O"
	cTitle = IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_CONFIGTABLEBROWSE_LOC, OUTPUTAPP_CONFIGTABLEBROWSE_LOC)
ELSE
	cTitle = OUTPUTAPP_CONFIGTABLEBROWSE_LOC
ENDIF
TRY
	DO CASE
		CASE m.tnType = OUTPUTAPP_CONFIGTOKEN_SETTABLE AND ;
				VARTYPE(m.tvReference) = "C" AND ;
				FILE(FULLPATH(FORCEEXT(TRANSFORM(m.tvReference),"DBF")))
* use FILE() because it can be in the app

			cDBF = FULLPATH(FORCEEXT(TRANSFORM(m.tvReference),"DBF"))
			iIndex = -1
			cType = TRANSFORM(OUTPUTAPP_CONFIGTOKEN_SETTABLE)
			DO CheckPublicListenerCollection WITH m.cType, m.iIndex
			IF m.iIndex # -1
				OUTPUTAPP_REFVAR.REMOVE[m.iIndex]
			ENDIF
			OUTPUTAPP_REFVAR.ADD(m.cDBF, m.cType)
			lSuccess = .T.
		CASE m.tnType = OUTPUTAPP_CONFIGTOKEN_WRITETABLE
			oSession = CREATEOBJECT("session")
			lSafety = SET("SAFETY") = "ON"
			toSH.Execute(oSession.DATASESSIONID)
			IF m.lSafety
				SET SAFETY ON
			ENDIF
			DO GetConfigObject WITH m.oConfig, .T.
* use XML class, not config superclass,
* to write both sets of records, base config outline
* and base listener's nodenames
*{ V&U RI 2008-12-10
			lcSys16 = JUSTPATH(SYS(16, 0))
			lnPos = AT(":\", lcSys16)
			lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
			cDBF = FORCEEXT(FORCEPATH(OUTPUTAPP_EXTERNALDBF, lcSys16),"DBF")
			oConfig.CreateConfigTable(m.cDBF)
			IF NOT EMPTY(SYS(2000, m.cDBF))
				iIndex = -1
				cType = TRANSFORM(OUTPUTAPP_CONFIGTOKEN_SETTABLE)
				DO CheckPublicListenerCollection WITH m.cType, m.iIndex
				IF m.iIndex # -1
					OUTPUTAPP_REFVAR.REMOVE[m.iIndex]
				ENDIF
				OUTPUTAPP_REFVAR.ADD(m.cDBF, m.cType)
				USE (m.cDBF)
				LOCATE FOR ObjType = OUTPUTAPP_OBJTYPE_LISTENER AND ;
					ObjCode = LISTENER_TYPE_DEBUG AND ;
					UPPER(ALLTRIM(ObjName)) == 'DEBUGLISTENER' AND ;
					ObjValue = OUTPUTAPP_BASELISTENER_CLASSLIB AND ;
					DELETED()
				IF EOF()
*{ V&U RI 2008-12-10
					lcSys16 = SYS(16, 0)
					lnPos = AT(":\", lcSys16)
					lcSys16 = SUBSTR(lcSys16, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
					INSERT INTO (ALIAS()) VALUES ;
						(OUTPUTAPP_OBJTYPE_LISTENER, LISTENER_TYPE_DEBUG,'DebugListener', OUTPUTAPP_BASELISTENER_CLASSLIB, lcSys16)
					DELETE NEXT 1
				ENDIF
*!*	            SELECT  ObjType, ObjCode, ObjName, ObjValue , ;
*!*	                    LEFT(ObjInfo,30) AS Info FROM (m.cDBF) ;
*!*	                    INTO CURSOR STRTRAN(OUTPUTAPP_CONFIGTABLEBROWSE_LOC," ","")
*!*	            SELECT (STRTRAN(OUTPUTAPP_CONFIGTABLEBROWSE_LOC," ",""))
*!*	            BROWSE TITLE OUTPUTAPP_CONFIGTABLEBROWSE_LOC  FIELDS ;
*!*	              ObjType, ObjCode, ObjName, ObjValue , Info = LEFT(ObjInfo,30), ObjInfo
				IF VARTYPE(goprogram) = "O"
					BROWSE TITLE IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_CONFIGTABLEBROWSE_LOC, OUTPUTAPP_CONFIGTABLEBROWSE_LOC)
				ELSE
					BROWSE TITLE OUTPUTAPP_CONFIGTABLEBROWSE_LOC
				ENDIF
				USE
				lSuccess = .T.
			ELSE
				lSuccess = .F.
			ENDIF
		OTHERWISE
			iIndex = -1
			cType = TRANSFORM(OUTPUTAPP_CONFIGTOKEN_SETTABLE)
			DO CheckPublicListenerCollection WITH m.cType, m.iIndex
			IF m.iIndex = -1
* don't disturb it if it's there
				DO GetConfigObject WITH m.oConfig
				cDBF = m.oConfig.GetConfigTable()
				OUTPUTAPP_REFVAR.ADD(m.cDBF, m.cType)
				tvReference = m.cDBF
			ELSE
				tvReference = OUTPUTAPP_REFVAR.ITEM[m.iIndex]
			ENDIF
			lSuccess = .T.
	ENDCASE
CATCH WHEN WTITLE() =  cTitle
* goProgram.vfxmessagebox("here")
* error 57 on the browse -- no table open ad nauseum
CATCH TO oError
	lSuccess = .F.
FINALLY
	toSH.Execute(m.iSession)
ENDTRY

IF NOT ISNULL(m.oError)
	HandleError(m.oError)
ENDIF

RETURN m.lSuccess

ENDPROC

PROCEDURE GetConfigObject(toCfg, tXML)
LOCAL lcModule, lnPos
lcModule = _REPORTOUTPUT
IF NOT INLIST(UPPER(JUSTEXT(m.lcModule)),"EXE","APP","DLL")
	lcModule = SYS(16, 0)
*{ V&U RI 2008-12-10
	lnPos = AT(":\", lcModule)
	lcModule = SUBSTR(lcModule, IIF(lnPos > 1, lnPos - 1, 1))
*} V&U RI 2008-12-10
ENDIF
IF NOT INLIST(UPPER(JUSTEXT(m.lcModule)), "EXE","APP","DLL")
	lcModule = ""
ENDIF
IF m.tXML
	toCfg = NEWOBJECT(OUTPUTAPP_CLASS_XMLLISTENER, OUTPUTAPP_BASELISTENER_CLASSLIB, m.lcModule)
ELSE
	toCfg = NEWOBJECT(OUTPUTAPP_CLASS_UTILITYLISTENER, OUTPUTAPP_BASELISTENER_CLASSLIB, m.lcModule)
ENDIF
IF VARTYPE(toCfg) = "O"
	toCfg.QUIETMODE = .T.
	IF VARTYPE(goProgram) = "O"
		toCfg.AppName = IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_APPNAME_LOC, OUTPUTAPP_APPNAME_LOC)
	ELSE
		toCfg.AppName = OUTPUTAPP_APPNAME_LOC
	ENDIF
ENDIF
ENDPROC

PROCEDURE ReportOutputDeclareReference( ;
	tiParams, tvReference, tlObjectMember, tlStringVar)
LOCAL iDotPos
IF m.tiParams > 1 AND ;
		TYPE("m.tvReference") = "C"
	iDotPos = RAT(".", m.tvReference)
	IF iDotPos > 1 AND ;
			iDotPos < LEN(m.tvReference)
		IF TYPE(m.tvReference) = "U"
			IF TYPE(LEFT(m.tvReference, m.iDotPos -1)) = "O"
				ADDPROPERTY(EVAL(LEFT(m.tvReference, m.iDotPos -1)), SUBSTR(m.tvReference, m.iDotPos + 1))
				tlObjectMember = .T.
			ENDIF
		ELSE
			tlObjectMember = .T.
		ENDIF
	ELSE
		tlStringVar = .T.
	ENDIF
ENDIF
ENDPROC

PROCEDURE UnloadListener(tiType)
LOCAL lUnload, cType

IF VARTYPE(OUTPUTAPP_REFVAR) # "O" OR ;
		NOT (UPPER(OUTPUTAPP_REFVAR.CLASS) == ;
		UPPER(OUTPUTAPP_REFVARCLASS))

* nothing to do

ELSE
	cType = TRANSFORM(m.tiType)
* look for reference to a listener of the appropriate type
	FOR iIndex = 1 TO OUTPUTAPP_REFVAR.COUNT
		IF OUTPUTAPP_REFVAR.GETKEY(m.iIndex) == m.cType
			OUTPUTAPP_REFVAR.REMOVE(m.iIndex)
			lUnload = .T.
			EXIT
		ENDIF
	NEXT

ENDIF

RETURN m.lUnload
ENDPROC

PROCEDURE HandleError(toE)
DO CASE
	CASE NOT ISNULL(m.toE)
		IF EMPTY(toE.ERRORNO)
			ERROR toE.MESSAGE
		ELSE
			ERROR toE.ERRORNO, toE.DETAILS
		ENDIF
	CASE NOT EMPTY(MESSAGE())
		ERROR MESSAGE()
	OTHERWISE
*ERROR OUTPUTAPP_UNKNOWN_ERROR_LOC
		IF VARTYPE(goprogram) = "O"
			ERROR IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_UNKNOWN_ERROR_LOC, OUTPUTAPP_UNKNOWN_ERROR_LOC) + ;
				IIF(goProgram.lRuntimeLocalization, goLocalize.cOUTPUTAPP_APPNAME_LOC, OUTPUTAPP_APPNAME_LOC)
		ELSE
			ERROR OUTPUTAPP_UNKNOWN_ERROR_LOC + OUTPUTAPP_APPNAME_LOC
		ENDIF
ENDCASE
ENDPROC

PROCEDURE CheckPublicListenerCollection(tcType, tiIndex)

LOCAL iIndex

IF VARTYPE(OUTPUTAPP_REFVAR) # "O" OR ;
		NOT (UPPER(OUTPUTAPP_REFVAR.CLASS) == ;
		UPPER(OUTPUTAPP_REFVARCLASS))
* could be a collection subclass
* in which case look for
* AINSTANCE(aTemp, <classname>)

	PUBLIC OUTPUTAPP_REFVAR
	STORE CREATEOBJECT(OUTPUTAPP_REFVARCLASS) TO ([OUTPUTAPP_REFVAR])

ENDIF

IF NOT EMPTY(m.tcType)

	FOR iIndex = 1 TO OUTPUTAPP_REFVAR.COUNT
		IF OUTPUTAPP_REFVAR.GETKEY(m.iIndex) == m.tcType
			tiIndex = m.iIndex
			EXIT
		ENDIF
	NEXT

ENDIF

ENDPROC

DEFINE CLASS SH AS CUSTOM
	PROCEDURE Execute(tiSession)
	SET DATASESSION TO (m.tiSession)
	ENDPROC
ENDDEFINE