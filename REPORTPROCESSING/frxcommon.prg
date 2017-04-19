* Contains:
*
*	NameValuePairManager
*   ResourceManager


*=================================================
* Class: NameValuePairManager
*
* This Name-Value pair manager class is used
* by frxEvent to store session-level data. It is
* also very useful when you need to take a memo
* of name-value pairs (say, the header EXPR field)
* and get at the individual data items easily.
*=================================================

DEFINE CLASS NameValuePairManager AS CUSTOM

*-------------------------------------------------
* Properties:
*-------------------------------------------------
	DIMENSION KEYS[1]
	DIMENSION VALUES[1]

	stripDelimiters = .T.

*------------------------------------------
	FUNCTION GET
*------------------------------------------
	LPARAMETER cToken

	LOCAL retVal
	retVal = ""
	IF NOT EMPTY( m.cToken )
*iIndex = ascan(this.keys, "|"+alltrim(upper(m.cToken))+"|" )
		iIndex = ASCAN(THIS.KEYS, "|" + ALLTRIM(m.cToken) +"|", 1, ALEN(THIS.KEYS), 1, 7)
		IF m.iIndex > 0
			RETURN THIS.VALUES[m.iIndex]
		ENDIF
	ENDIF
	RETURN m.retVal
	ENDFUNC

*------------------------------------------
	FUNCTION SET
*------------------------------------------
	LPARAMETERS cKey, vValue

	LOCAL iIndex, iKeyCount
	IF NOT EMPTY( m.cKey )
*iIndex = ascan(this.keys, "|"+alltrim(upper(m.cKey))+"|" )
		iIndex = ASCAN(THIS.KEYS, "|" + ALLTRIM(m.cKey) +"|", 1, ALEN(THIS.KEYS), 1, 7)
		IF m.iIndex > 0
			THIS.VALUES[m.iIndex] = m.vValue
		ELSE
			iKeyCount = ALEN(THIS.KEYS, 1) + 1

			DIMENSION THIS.KEYS[m.iKeyCount]
			DIMENSION THIS.VALUES[m.iKeyCount]
			THIS.KEYS[m.iKeyCount]   = "|" + m.cKey +"|"
			THIS.VALUES[m.iKeyCount] = m.vValue
		ENDIF
	ENDIF

	ENDFUNC

*------------------------------------------
	FUNCTION getMemo
* returns a CRLF delimited string
*------------------------------------------
	LOCAL iPair, cText
	cText = ""
	FOR iPair = 2 TO ALEN( THIS.KEYS )
		cText = m.cText + STREXTRACT(THIS.KEYS[m.iPair],"|","|") + " = " + TRANSFORM(THIS.VALUES[m.iPair]) + CHR(13) + CHR(10)
	ENDFOR
	RETURN m.cText
	ENDFUNC

*------------------------------------------
	PROCEDURE RESET
*------------------------------------------
	DIMENSION THIS.KEYS[1]
	DIMENSION THIS.VALUES[1]
	THIS.KEYS   = .F.
	THIS.VALUES = .F.
	ENDPROC

*------------------------------------------
	FUNCTION loadMemo
* expects a CRLF delimited string
* Values will be stored as strings
*------------------------------------------
	LPARAMETER cText

	IF EMPTY( m.cText )
		RETURN .F.
	ENDIF

* populate arrays:
*
	LOCAL i, iLineCount, iKeyCount, cBuff, q, cKey, cValue
	PRIVATE aTemp

	iLineCount = ALINES( aTemp, m.cText )
	iKeyCount  = 0
	FOR i = 1 TO iLineCount
		IF EMPTY( aTemp[m.i] ) ;
				OR INLIST( LEFT( aTemp[m.i], 1 ), "[", ";", "*" )
* do nothing
		ELSE
			iKeyCount  = m.iKeyCount + 1

* extract the key:
			cBuff = ALLTRIM(aTemp[m.i])
			q = MIN( AT(" ", m.cBuff +" "), AT("=", m.cBuff +"="), AT(CHR(9), m.cBuff + CHR(9)) )
			cKey = ALLTRIM(LEFT(m.cBuff, q -1))

* extract the value:
			cBuff  = ALLTRIM( aTemp[m.i])
			cValue = ALLTRIM( SUBSTR( m.cBuff, AT("=", m.cBuff) + 1 ) )

			IF THIS.stripDelimiters
* quote removal:
				IF LEFT( m.cValue, 1) = ["] AND RIGHT(m.cValue, 1) = ["]
					cValue  = SUBSTR( m.cValue, 2, LEN(m.cValue ) -2)
				ENDIF
				IF LEFT( m.cValue, 1) = ['] AND RIGHT(m.cValue, 1) = [']
					cValue  = SUBSTR( m.cValue, 2, LEN(m.cValue ) -2)
				ENDIF
			ENDIF
			cValue = ALLTRIM(m.cValue )

* load them into the property:
*
			THIS.SET( m.cKey, m.cValue )
		ENDIF
	ENDFOR
	RETURN .T.
	ENDFUNC

ENDDEFINE

*=================================================
* Class: ResourceManager
*
* This class is derived from NameValuePairManager
* and contains additional logic for saving and
* restoring name-value pairs to the resource file
*=================================================
DEFINE CLASS ResourceManager AS NameValuePairManager

	currentWorkArea  = 0
	resourceWorkArea = 0

*-----------------------------------------------
* OpenResourceFile()
*   returns .T. if successfully opened the resource file
*-----------------------------------------------
	PROCEDURE OpenResourceFile

	IF FILE( SET("RESOURCE", 1) ) AND SET("RESOURCE") ="ON"

		THIS.currentWorkArea = SELECT()

		SELECT 0
		USE (SET("RESOURCE", 1)) AGAIN SHARED
	  	*-----------------------------------------------
        * Fix for SP2: If the resource file is built-in
    	* to the Application/Exe, it may not have an
        * index available!
        *-----------------------------------------------
        if upper(key(1))="TYPE+ID+PADR(NAME,24)"
              set order to 1
        endif
		THIS.resourceWorkArea = SELECT(0)

		RETURN .T.
	ENDIF
	RETURN .F.
	ENDPROC

*-----------------------------------------------
* LoadResource( ID, NAME )
* locates and loads a specified resource record
*    into the name-value pairs.
* Currently leaves resource file open.
*-----------------------------------------------
	PROCEDURE LoadResource
	LPARAMETER cID, cNAME

	IF THIS.OpenResourceFile()
		*-----------------------------------------------
		* Fix for SP2: If the resource file is built-in
		* to the Application/Exe, it may not have an 
		* index available! So use LOCATE instead of SEEK:
		*-----------------------------------------------
		locate for TYPE+ID+PADR(NAME,24) = padr("PREFW",12)+padr(m.cID,12) + m.cNAME
		if found()

			THIS.loadMemo( DATA )	

		endif
		
		SELECT (THIS.currentWorkArea)
		RETURN
	ENDIF
	RETURN .F.
	ENDPROC

*-----------------------------------------------
* SaveResource( ID, NAME )
* locates and saves the current name-value pairs
*    to a specified resource record.
* Opens the resource file if necessary.
* Closes the resource file when finished.
*-----------------------------------------------
	PROCEDURE SaveResource
	LPARAMETER cID, cNAME

	IF EMPTY( THIS.ResourceWorkArea )
		IF NOT THIS.OpenResourceFile()
			RETURN .F.
		ENDIF
	ELSE
		SELECT (THIS.ResourceWorkArea )
	ENDIF

	LOCAL lRetVal
	IF NOT ISREADONLY()

		LOCAL cData
		cData = THIS.getMemo()
	 	*-----------------------------------------------
        * Fix for SP2: If the resource file is built-in
        * to the Application/Exe, it may not have an
        * index available! So use LOCATE instead of SEEK:
        *-----------------------------------------------
        * if not seek( padr("PREFW",12) + padr(m.cID,12) + m.cNAME )
        locate for TYPE+ID+PADR(NAME,24) = padr("PREFW",12) + padr(m.cID,12) + m.cNAME
	    if not found()

			APPEND BLANK
			REPLACE ;
				TYPE		WITH "PREFW", ;
				ID			WITH m.cID, ;
				NAME		WITH m.cNAME, ;
				READONLY 	WITH .F.
		ENDIF

		IF NOT READONLY
			REPLACE	DATA 	WITH m.cData, ;
				CKVAL 	WITH VAL(SYS(2007, m.cData )), ;
				UPDATED WITH DATE()
		ENDIF

		m.lRetVal = .T.
	ELSE
		m.lRetVal = .F.
	ENDIF

*-------------------------------- Close the resource file:
	USE IN (THIS.ResourceWorkArea)
	THIS.ResourceWorkArea = 0

*-------------------------------- Restore the current workarea:
	SELECT (THIS.currentWorkArea)
	RETURN (m.lRetVal)
	ENDPROC

*-----------------------------------------------
* Destroy()
* Close the resource file if open
*-----------------------------------------------
	PROCEDURE DESTROY

	IF NOT EMPTY( THIS.ResourceWorkArea )
		USE IN (THIS.ResourceWorkArea)
		THIS.ResourceWorkArea = 0
	ENDIF
	IF NOT EMPTY( THIS.CurrentWorkArea )
		SELECT (THIS.CurrentWorkArea)
	ENDIF
	ENDPROC

*-----------------------------------------------
* SaveFontState( THIS )
* Saves Font Style properties into the specified
* resource file record
*   THIS  - object reference that has FontXxxx properties
*-----------------------------------------------
	PROCEDURE SaveFontState
	LPARAMETERS oRef

	THIS.SET(oRef.NAME + ".FontName",   oRef.FONTNAME )
	THIS.SET(oRef.NAME + ".FontSize",   oRef.FONTSIZE )
	THIS.SET(oRef.NAME + ".FontBold",   oRef.FONTBOLD )
	THIS.SET(oRef.NAME + ".FontItalic", oRef.FONTITALIC )

	ENDPROC


*-----------------------------------------------
* RestoreFontState( THIS )
* Restores form properties from the specified
* resource file record
*   THIS  - object reference that has FontXxxx properties
*-----------------------------------------------
	PROCEDURE RestoreFontState
	LPARAMETERS oRef

	LOCAL cValue
	cValue = THIS.GET(oRef.NAME + ".FontName")
	IF NOT EMPTY( m.cValue )
		oRef.FONTNAME = m.cValue
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".FontSize")
	IF NOT EMPTY( m.cValue )
		oRef.FONTSIZE = INT(VAL(m.cValue ))
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".FontBold")
	IF NOT EMPTY( m.cValue )
		oRef.FONTBOLD = (UPPER(m.cValue) =".T.")
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".FontItalic")
	IF NOT EMPTY( m.cValue )
		oRef.FONTITALIC = (UPPER(m.cValue) =".T.")
	ENDIF

	ENDPROC

*-----------------------------------------------
* SaveWindowState( THIS )
* Saves form properties into the specified
* resource file record
*   THIS  - object reference to form
*-----------------------------------------------
	PROCEDURE SaveWindowState
	LPARAMETERS oRef

	LOCAL iCurrentState
	iCurrentState = oRef.WINDOWSTATE
	IF oRef.WINDOWSTATE <> 0
*----------------------------------
* Fixed for SP1: was THIS.WindowState
*----------------------------------
		oRef.WINDOWSTATE = 0
	ENDIF

	THIS.SET(oRef.NAME + ".Top", oRef.TOP )
	THIS.SET(oRef.NAME + ".Left", oRef.LEFT )
	THIS.SET(oRef.NAME + ".Width", oRef.WIDTH )
	THIS.SET(oRef.NAME + ".Height", oRef.HEIGHT )
	THIS.SET(oRef.NAME + ".WindowState", m.iCurrentState )

	ENDPROC

*-----------------------------------------------
* RestoreWindowState( THIS )
* Restores form properties from the specified
* resource file record
*   THIS  - object reference to form
*-----------------------------------------------
	PROCEDURE RestoreWindowState
	LPARAMETERS oRef

	LOCAL cValue

	cValue = THIS.GET(oRef.NAME + ".Top")
	IF NOT EMPTY( m.cValue )
		oRef.TOP = INT(VAL( m.cValue ))
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".Left")
	IF NOT EMPTY( m.cValue )
		oRef.LEFT = INT(VAL( m.cValue ))
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".Width")
	IF NOT EMPTY( m.cValue )
		oRef.WIDTH = INT(VAL( m.cValue ))
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".Height")
	IF NOT EMPTY( m.cValue )
		oRef.HEIGHT = INT(VAL( m.cValue ))
	ENDIF
	cValue = THIS.GET(oRef.NAME + ".WindowState")
	IF NOT EMPTY( m.cValue )
		oRef.WINDOWSTATE = INT(VAL( m.cValue ))
	ENDIF
	ENDPROC

ENDDEFINE