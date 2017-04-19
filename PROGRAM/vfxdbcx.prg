******************************************************************************
* Project...........: Visual Extend 16.0
*                     Copyright (c) dFPUG c/o ISYS GmbH
*                     All Rights Reserved.
*
* Program...........: VFXDBCX.PRG
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

*******************************************************************
**
FUNCTION dbcx_init(tcdbc)
LOCAL lcpath, lcdbc, lok

#ifndef _dbcx
RETURN .T.
#ENDIF

IF FILE("UPD$CTRL.KEY")
	IF TYPE("goProgram.class") ="C"
		 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING))
	ENDIF
	RETURN .F.
ENDIF

tcdbc = UPPER(tcdbc)

lcpath = ADDBS(JUSTPATH (tcdbc))
lcdbc  = JUSTFNAME(tcdbc)

IF !(".DBC" $ lcdbc)
	lcdbc = lcdbc + ".DBC"
ENDIF

lok     = .T.

********************************************
** It's a new installation ?

LOCAL lnhowmany, lafile[1,1], lnewinstall, lnfile

lnfile      = -1
lnewinstall = .F.

*!*	Hardcoded path removed. UH, 11.02.98
*!*	lnHowMany = adir(laFile,"DATA\*.*", "A")
lnhowmany = ADIR(lafile, lcpath +"*.*", "A")

IF lnhowmany = 0
	LOCAL  j, lcfrom, lcto, lcverify

	lnewinstall = .T.

	lcfrom = lcpath +"UPDATE\"
	lcto   = lcpath

	lnhowmany = ADIR(lafile, lcfrom +"*.*", "A")

	CLOSE DATA ALL
	CLOSE TABLES ALL

	IF lnhowmany > 0
		lcverify = SET('safety')

		SET SAFETY OFF

		IF FILE("UPD$CTRL.KEY")
			IF TYPE("goProgram.class") ="C"
				 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING))
			ENDIF
			RETURN .F.
		ENDIF

		lnfile = FCREATE("UPD$CTRL.KEY", 0)

		IF lnfile < 0
			IF TYPE("goProgram.class") ="C"
				 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_CONFLICT, MSG_UPDATE_CONFLICT))
			ENDIF
			RETURN .F.
		ENDIF

		FOR j = 1 TO lnhowmany
			IF TYPE("goProgram.class") ="C"
				goProgram.vfxwaitwindow("Copying : " + lafile[j,1] + " ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
			ELSE
				WAIT WINDOW "Copying : " + lafile[j,1] + " ..." NOWAIT NOCLEAR 				&& JEI VM 20080828 edit {added NOCLEAR}
			ENDIF
			COPY FILE (lcfrom + lafile[j,1]) TO (lcto + lafile[j,1])
		NEXT

*JEI VM 20080828{
		IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
			goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
		ELSE
			WAIT CLEAR
		ENDIF
*JEI VM 20080828}

	ENDIF
ENDIF

IF !FILE(lcpath +"DBCXREG.DBF")

*!*	SDT: DBCX Metadata will be created from the DBCX compliant product.
*!*      VFX does not create the Metadata. UH 18.07.99
*!*	lok = dbcx_create(lcpath, lcdbc)
	lok = .F.

*!*		IF lok
*!*			= dbcx_cdbkreg(lcpath,"LIB\DBCX\")

*!*			#ifdef _stonefield
*!*				= dbcx_sdtreg(lcpath ,"LIB\DBCX\")
*!*			#ENDIF

*!*			CLOSE DATA ALL
*!*			CLOSE TABLES ALL

*!*			OPEN DATA (tcdbc) EXCLUSIVE


*!*	*!* SDT: Changed from MetaMgr to DBCXMgr. Global reference oMeta. UH 18.07.99
*!*	*!*		PUBLIC gometa
*!*	*!*		gometa=CREATEOBJECT("MetaMgr",.F.)
*!*	*!*		gometa.ldebugmode = .F.
*!*	*!*		=dbc_extend(gometa)
*!*	*!*		RELEASE gometa
*!*			PUBLIC oMeta
*!*			oMeta=CREATEOBJECT("DBCXMgr",.F.)
*!*			oMeta.SetDatabase(DBC())
*!*			oMeta.ldebugmode = .F.
*!*			=dbc_extend(oMeta)
*!*			RELEASE oMeta

*!*		ELSE
	 = errormsg("Error Creating DBC Extensions")
	lok = .F.
*!*		ENDIF
ENDIF

IF lok
	LOCAL j, lcfrom, lcto, lcverify

	CLOSE DATA ALL
	CLOSE TABLES ALL

	lcfrom = lcpath +"UPDATE\"
	lcto   = lcpath

	lnhowmany = ADIR(lafile, lcfrom +"*.*", "A")

	IF lnhowmany > 0 OR lnewinstall
*********************************
** File Not Opened ?

		IF lnfile < 0
			lcverify = SET('safety')

			SET SAFETY OFF

			IF FILE("UPD$CTRL.KEY") AND !lnewinstall
				IF TYPE("goProgram.class") ="C"
					 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_RUNNING, MSG_UPDATE_RUNNING))
				ENDIF
				RETURN .F.
			ENDIF

			lnfile = FCREATE("UPD$CTRL.KEY", 0)

			IF lnfile < 0
				IF TYPE("goProgram.class") ="C"
					 = errormsg(IIF(goProgram.lRuntimeLocalization, goLocalize.cMSG_UPDATE_CONFLICT, MSG_UPDATE_CONFLICT))
				ENDIF
				RETURN .F.
			ENDIF
		ENDIF

		FOR j = 1 TO lnhowmany
			IF TYPE("goProgram.class") ="C"
				goProgram.vfxwaitwindow("Copying : " + lafile[j,1] + " ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
			ELSE
				WAIT WINDOW "Copying : " + lafile[j,1] + " ..." NOWAIT NOCLEAR 				&& JEI VM 20080828 edit {added NOCLEAR}
			ENDIF
			COPY FILE (lcfrom + lafile[j,1]) TO (lcto + lafile[j,1])
		NEXT

		OPEN DATA (tcdbc) EXCLUSIVE

*!* SDT: Changed from MetaMgr to DBCXMgr. Global reference oMeta. UH 18.07.99
*!*			PUBLIC gometa
*!*			gometa=CREATEOBJECT("MetaMgr",.F.)
*!*			gometa.ldebugmode = .F.
*!*			#ifdef _stonefield
*!*				IF lnhowmany > 0 OR lnewinstall
*!*					IF gometa.osdtmgr.needupdate()
*!*						goProgram.vfxwaitwindow("Update ... ",,,.T.,,,)
*!*						gometa.osdtmgr.UPDATE()
*!*					ENDIF
*!*				ENDIF
*!*			#ENDIF
		PUBLIC ometa
		ometa = CREATEOBJECT("DBCXMgr", .F.)
		ometa.setdatabase(DBC())
		ometa.ldebugmode = .F.
		#IFDEF _stonefield
			IF lnhowmany > 0 OR lnewinstall
				IF ometa.osdtmgr.needupdate()
					IF TYPE("goProgram.class") ="C"
						goProgram.vfxwaitwindow("Update ... ", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
					ELSE
						WAIT WINDOW "Update..." NOWAIT NOCLEAR 				&& JEI VM 20080828 edit {added NOCLEAR}
					ENDIF
					ometa.osdtmgr.UPDATE("ALL")
				ENDIF
			ENDIF
		#ENDIF

*!* Moved here because files have to be closed before they can be deleted. UH 18.07.99
*!* SDT: Global reference oMeta. UH 18.07.99
*!*			IF VARTYPE(goMeta)=="O"
*!*				RELEASE gometa
*!*			ENDIF
		IF VARTYPE(ometa) =="O"
			RELEASE ometa
		ENDIF
		CLOSE DATA ALL
		CLOSE TABLES ALL

		FOR j = 1 TO lnhowmany
			IF TYPE("goProgram.class") ="C"
				goProgram.vfxwaitwindow("Deleting : " + lafile[j,1] + " ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
			ELSE
				WAIT WINDOW "Deleting..." NOWAIT NOCLEAR 									&& JEI VM 20080828 edit {added NOCLEAR}
			ENDIF
			ERASE (lcfrom + lafile[j,1])
		NEXT

		IF lcverify == "ON"
			SET SAFETY ON
		ENDIF

		 = FCLOSE(lnfile)

		ERASE ("UPD$CTRL.KEY")

*!* Moved some lines up. UH 18.07.99
*!*			CLOSE DATA ALL
*!*			CLOSE TABLES ALL

	ENDIF	&& lnHowMany > 0

	CLOSE DATA ALL
	CLOSE TABLES ALL

	IF TYPE("goProgram.class") == "C"
		goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
	ELSE
		WAIT CLEAR
	ENDIF

	lok = .T.
ENDIF	&& lOk
IF TYPE("goProgram.class") ="C"
	GoProgram.LockCurrentLoggedUse()
ENDIF
RETURN lok
ENDFUNC


*******************************************************************
**
FUNCTION dbcx_create(tcpath, tcdbc)
LOCAL lccurrcollate, ;
	lcalias, ;
	lncurrmemo, ;
	lnline, ;
	lcline

lccurrcollate = SET('COLLATE')
SET COLLATE TO 'MACHINE'

IF NOT FILE(tcpath + 'DBCXREG.DBF')
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxwaitwindow("Creating DBCX Registry Table ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
	ELSE
		WAIT WINDOW "Creating DBCX Registry Table ..." NOWAIT NOCLEAR 			&& JEI VM 20080828 edit {added NOCLEAR}
	ENDIF

	SELECT 0
	lcalias = createuniquealias()
	USE (tcpath + tcdbc) ALIAS lcalias AGAIN
	lncurrmemo = SET('MEMOWIDTH')
	SET MEMOWIDTH TO 2048

	SCAN FOR ATCLINE('[DBCXID: ', USER) > 0
		lnline = ATCLINE('[DBCXID: ', USER)
		lcline = MLINE(USER, lnline)
		REPLACE USER WITH STRTRAN(USER, lcline)
	ENDSCAN

	SET MEMOWIDTH TO lncurrmemo
	USE

	CREATE TABLE (tcpath + 'DBCXREG.DBF') FREE ;
		(cprodname  c(40), ;
		mlibpath   m   , ;
		clibname   c(12), ;
		cclassname c(30), ;
		ilastid    i   , ;
		tlastupdt  T   , ;
		cobjname   c(30))

	INDEX ON UPPER(cprodname) TAG cprodname FOR NOT DELETED() CANDIDATE
	INDEX ON DELETED()        TAG DELETED

	INSERT INTO dbcxreg (cprodname, tlastupdt) ;
		VALUES ('SYSTEM RECORD', DATETIME())

	USE
ENDIF

#IFDEF _stonefield
	IF NOT FILE(tcpath + 'SDTMETA.DBF')
		IF TYPE("goProgram.class") ="C"
			goProgram.vfxwaitwindow("Creating Stonefield Meta Data ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
		ELSE
			WAIT WINDOW "Creating Stonefield Meta Data ..." NOWAIT NOCLEAR 				&& JEI VM 20080828 edit {added NOCLEAR}
		ENDIF

		CREATE TABLE (tcpath + 'SDTMETA.DBF') FREE ;
			(iid i, ;
			AUTOOPEN l, ;
			canupdate l, ;
			CAPTION m, ;
			CODEPAGE N(5), ;
			COLLATE c(10), ;
			EXTENDED l, ;
			FILTER l, ;
			labels l, ;
			merge l, ;
			NOUPDATE l, ;
			relreports l, ;
			reports l, ;
			SELECT l, ;
			SORT l, ;
			COMMENT m, ;
			objectname c(128), ;
			autofilt m, ;
			autosql m, ;
			prevname c(10), ;
			autoclass c(128), ;
			autolib c(128), ;
			autoprefix c(5), ;
			autolabel l, ;
			dbcpath m, ;
			realname c(10))

		INDEX ON iid               TAG iid CANDIDATE
		INDEX ON UPPER(objectname) TAG objectname

		USE
	ENDIF

	IF NOT FILE(tcpath + 'SDTUSER.DBF')
		IF TYPE("goProgram.class") ="C"
			goProgram.vfxwaitwindow("Stonefield User's Meta Data ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
		ELSE
			WAIT WINDOW "Stonefield User's Meta Data ..." NOWAIT NOCLEAR  			&& JEI VM 20080828 edit {added NOCLEAR}
		ENDIF

		CREATE TABLE (tcpath + 'SDTUSER.DBF') FREE ;
			(property c(128), ;
			objecttype c(5), ;
			field_name c(10), ;
			field_type c(1), ;
			field_len N(3), ;
			field_dec N(3))

		INDEX ON field_name TAG FIELD    CANDIDATE
		INDEX ON property   TAG property

		USE

	ENDIF
#ENDIF

IF NOT FILE(tcpath + 'CDBKMETA.DBF')
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxwaitwindow("Stonefield CodeBook Meta Data ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
	ELSE
		WAIT WINDOW "Stonefield CodeBook Meta Data ..." NOWAIT NOCLEAR  			&& JEI VM 20080828 edit {added NOCLEAR}
	ENDIF

	CREATE TABLE (tcpath + 'CDBKMETA.DBF') FREE ;
		(mpath m, ;
		cdbcname c(128), ;
		iid i, ;
		crectype c(1), ;
		ccursor c(128), ;
		ccursor2 c(128), ;
		ctagname c(25), ;
		ncdxorder N(3), ;
		mtagfilter m, ;
		mtagexpr m, ;
		ctagtype c(1), ;
		lascending l, ;
		cchildtbl c(128), ;
		mparntexpr m, ;
		mchildexpr m, ;
		cfield c(128), ;
		mexpr m, ;
		ctype c(1), ;
		lbinary l, ;
		nsize N(3), ;
		ndecimals N(3), ;
		lnull l, ;
		cscxprompt c(30), ;
		cdlgprompt c(30), ;
		clstprompt c(30), ;
		cfrxcol1 c(30), ;
		cfrxcol2 c(30), ;
		minformat m, ;
		moutformat m, ;
		mmessage m, ;
		mtooltip m, ;
		mrangelo m, ;
		mrangehi m, ;
		mhelptext m, ;
		mnotes m, ;
		tlastmod T)

	INDEX ON iid              TAG iid       FOR NOT DELETED() CANDIDATE
	INDEX ON DELETED()        TAG DELETED
	INDEX ON UPPER(crectype)  TAG crectype  FOR NOT DELETED()
	INDEX ON UPPER(cdbcname)  TAG cdbcname  FOR NOT DELETED()
	INDEX ON UPPER(ccursor)   TAG ccursor   FOR NOT DELETED()
	INDEX ON UPPER(cfield)    TAG cfield    FOR NOT DELETED()
	INDEX ON UPPER(cchildtbl) TAG cchildtbl FOR NOT DELETED()
	INDEX ON UPPER(ctagname)  TAG ctagname  FOR NOT DELETED()

	USE

ENDIF

SET COLLATE TO lccurrcollate
*JEI VM 20080828{
IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ELSE
*JEI VM 20080828}
	WAIT CLEAR
ENDIF

RETURN .T.

*******************************************************************
**
FUNCTION dbc_extend(ometa, tcalias)

LOCAL latables[1], ;
	lntables, ;
	lcdbc, ;
	lcpath, ;
	llreturn, ;
	lcalias, ;
	lcfile, ;
	lni, ;
	liid

lntables = ADBOBJECTS(latables, 'Table')
lcdbc    = DBC()
lcpath   = ADDBS(JUSTPATH(lcdbc))
llreturn = .T.

***************************************************
*** Update each Table

IF VARTYPE(tcalias) = 'C' AND NOT EMPTY(tcalias)
	lcalias = UPPER(tcalias)
	lcfile  = FULLPATH(DBGETPROP(lcalias, 'Table', 'Path'), lcpath)

	IF arrayscan(@latables, lcalias) > 0
		llreturn = NOT FILE(lcfile) OR ;
			extendonetable(lcalias, ometa, lcdbc, lcfile)
	ELSE
		IF TYPE("goProgram.class") ="C"
			 = goProgram.vfxmessagebox(lcalias +": File Not Found", 32,"DBCX Extesion")
		ELSE
			MESSAGEBOX(lcalias +": File Not Found", 32,"DBCX Extesion")
		ENDIF
		RETURN .F.
	ENDIF
ELSE
***************************************************
*** Update each Table in the Database

	FOR lni = 1 TO lntables
		lcalias  = latables[lnI]
		lcfile   = FULLPATH(DBGETPROP(lcalias, 'Table', 'Path'), lcpath)
		llreturn = NOT FILE(lcfile) OR ;
			extendonetable(lcalias, ometa, lcdbc, lcfile)
		IF NOT llreturn
			EXIT
		ENDIF
	NEXT

***************************************************
*** Update each View

	lnviews = ADBOBJECTS(laviews, 'View')
	FOR lni = 1 TO lnviews
		llreturn = extendoneview(lcdbc, ometa, laviews[lnI])
		IF NOT llreturn
			EXIT
		ENDIF
	NEXT

***************************************************
*** Update each Relation

	lnrelations = ADBOBJECTS(larelations, 'Relation')
	FOR lni = 1 TO lnrelations
		llreturn = extendonerelation(lcdbc, ometa, larelations[lnI, 1], ;
			larelations[lnI, 2], larelations[lnI, 3], larelations[lnI, 4])
		IF NOT llreturn
			EXIT
		ENDIF
	NEXT
ENDIF

*********************************************************
* Set some properties for the database record, then exit.

#IFDEF _stonefield
	IF llreturn

		lcmetapath = DBF(ometa.osdtmgr.cdbcxalias)
		lcmetapath = LEFT(lcmetapath, RAT('\', lcmetapath))
		lcdbcpath  = SYS(2014, lcdbc, lcmetapath)
		liid       = ometa.dbgetdbckey(lcdbc, 'Database', 'Database')

		WITH ometa
			.dbcxsetprop('SDTExtended', .T.,                 liid)
			.dbcxsetprop('SDTDBCPath',  lcdbcpath,           liid)
			.dbcxsetprop('CBMPath',     JUSTPATH(lcdbcpath), liid)
			.dbcxsetprop('CBcDBCName',  JUSTSTEM(lcdbc),     liid)
			.dbcxsetprop('CBtLastMod',  DATETIME(),          liid)
		ENDWITH
	ELSE
		IF TYPE("goProgram.class") ="C"
			 = goProgram.vfxmessagebox(lcalias +": File not Extended", 32,"DBCX Extesion")
		ELSE
			MESSAGEBOX(lcalias +": File not Extended", 32,"DBCX Extesion")
		ENDIF
	ENDIF
#ENDIF

*JEI VM 20080829{
IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ELSE
*JEI VM 20080829}
	WAIT CLEAR
ENDIF

RETURN llreturn
ENDFUNC

*******************************************************************
**
FUNCTION dbcx_cdbkreg(tcpath, tcvcxlocn)
LOCAL lncurrselect, ;
	lcalias, ;
	lclibpath

lncurrselect = SELECT()
lcalias      = "X" + SUBSTR(SYS(2015), 4, 7)

lclibpath = tcvcxlocn

SELECT 0

USE (tcpath + 'DBCXREG') ORDER cprodname AGAIN ALIAS (lcalias)

SEEK UPPER("CodeBook")

IF NOT FOUND()
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxwaitwindow("Registering CodeBook Extensions ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
	ELSE
		WAIT WINDOW "Registering CodeBook Extensions ..." NOWAIT NOCLEAR 			&& JEI VM 20080828 edit {added NOCLEAR}
	ENDIF F

	INSERT INTO (lcalias) ;
		(cprodname, ;
		cversion, ;
		cdbcxname, ;
		cdbcxalias, ;
		mlibpath, ;
		clibname, ;
		cclassname, ;
		ilastid, ;
		tlastupdt) ;
		VALUES ;
		("CodeBook", ;
		"v3.2", ;
		'cdbkmeta.dbf', ;
		'codebook', ;
		lclibpath, ;
		'cdbkmgr.vcx', ;
		'cdbkmgr', ;
		0, ;
		DATETIME())
ENDIF

USE

SELECT (lncurrselect)
*JEI VM 20080828{
IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ELSE
*JEI VM 20080828}
	WAIT CLEAR
ENDIF

RETURN
ENDFUNC

*******************************************************************
**
FUNCTION dbcx_sdtreg(tcpath, tcvcxlocn)
LOCAL lncurrselect, ;
	lcalias, ;
	lclibpath

lncurrselect = SELECT()
lcalias      = "X" + SUBSTR(SYS(2015), 4, 7)

lclibpath = tcvcxlocn

SELECT 0

USE (tcpath + 'DBCXREG') ORDER cprodname AGAIN ALIAS (lcalias)

SEEK UPPER('Stonefield DBC Toolkit')

IF NOT FOUND()
	IF TYPE("goProgram.class") ="C"
		goProgram.vfxwaitwindow("Registering 'Stonefield DBC Toolkit' ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
	ELSE
		WAIT WINDOW "Registering 'Stonefield DBC Toolkit' ..." NOWAIT NOCLEAR 			&& JEI VM 20080828 edit {added NOCLEAR}
	ENDIF

	INSERT INTO (lcalias) ;
		(cprodname, ;
		cversion, ;
		cdbcxname, ;
		cdbcxalias, ;
		mlibpath, ;
		clibname, ;
		cclassname, ;
		ilastid, ;
		tlastupdt) ;
		VALUES ;
		('Stonefield DBC Toolkit', ;
		'v3.0a', ;
		'sdtmeta.dbf', ;
		'sdtmeta', ;
		lclibpath, ;
		'sdt.vcx', ;
		'sdtmgr', ;
		0, ;
		DATETIME())
ENDIF

USE

SELECT (lncurrselect)
*JEI VM 20080828{
IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ELSE
*JEI VM 20080828}
	WAIT CLEAR
ENDIF

RETURN
ENDFUNC

*******************************************************************
**
FUNCTION extendonetable(tcalias, ometa, 	tcdatabase, tcfile)
LOCAL lafields[1], ;
	lncodepage, ;
	lacodepages[1], ;
	lnpos, ;
	llused, ;
	lcmetapath, ;
	lccbmpath, ;
	lccbmname, ;
	lcsdtpath, ;
	liid, ;
	lnfields, ;
	lni, ;
	lcfield, ;
	laprops[1], ;
	lcfile, ;
	lntags, ;
	lcindex

IF TYPE("goProgram.class") ="C"
	goProgram.vfxwaitwindow("Processing " + tcalias + " ...", , , .T., , .T.,)	&& JEI VM 20080828 edit {added NOCLEAR}
ELSE
	WAIT WINDOW "Processing " + tcalias + " ..." NOWAIT NOCLEAR 			&& JEI VM 20080828 edit {added NOCLEAR}
ENDIF

#IFDEF _stonefield
	IF NOT ometa.osdtmgr.getdbfstructure(tcfile, @lafields, @lncodepage)
*JEI VM 20080828{
		IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
			goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
		ELSE
*JEI VM 20080828}
			WAIT CLEAR
		ENDIF

		RETURN .F.
	ENDIF

	IF lncodepage = 0
		lncodepage = CPCURRENT()

		ometa.osdtmgr.getcodepagearray(@lacodepages)

		lnpos      = arrayscan(@lacodepages, lncodepage, 2)

		IF lnpos > 0
			ometa.osdtmgr.writecodepage(lacodepages[lnPos, 3], , tcfile)
		ENDIF
	ENDIF

	llused = USED(tcalias)

	IF NOT llused
		USE (tcalias) AGAIN NODATA IN 0
	ENDIF

#ELSE
	llused = USED(tcalias)

	IF NOT llused
		USE (tcalias) AGAIN NODATA IN 0
	ENDIF

	 = AFIELDS(lafields, tcalias)
#ENDIF

lcmetapath = DBF(ometa.osdtmgr.cdbcxalias)
lcmetapath = LEFT(lcmetapath, RAT('\', lcmetapath))
lccbmpath  = JUSTPATH(SYS(2014, tcdatabase, lcmetapath))
lccbmname  = JUSTSTEM(tcdatabase)
lcsdtpath  = SYS(2014, tcdatabase, lcmetapath)
liid       = ometa.dbgetdbckey(tcdatabase, 'Table', tcalias)

#IFDEF _stonefield
	WITH ometa
		.dbcxsetprop('SDTCodePage', CPDBF(tcalias), liid)
		.dbcxsetprop('SDTDBCPath' , lcsdtpath,      liid)
		.dbcxsetprop('CBMPath',     lccbmpath,      liid)
		.dbcxsetprop('CBcDBCName',  lccbmname,      liid)
		.dbcxsetprop('CBtLastMod',  DATETIME(),     liid)
		.dbcxsetprop('SDTCaption' , PROPER(tcalias), liid)
		.dbcxsetprop('SDTCodePage', 1252, liid)
		.dbcxsetprop('SDTAutoOpen', .T., liid)
		.dbcxsetprop('SDTCanUpdate', .T., liid)
		.dbcxsetprop('SDTReports' , .T., liid)
	ENDWITH
#ELSE
	WITH ometa
		.dbcxsetprop('CBMPath',     lccbmpath,      liid)
		.dbcxsetprop('CBcDBCName',  lccbmname,      liid)
		.dbcxsetprop('CBtLastMod',  DATETIME(),     liid)
	ENDWITH
#ENDIF


#IFDEF _stonefield
	DIMENSION laprops[15, 3]

	laprops[ 1, 1] = 'oSDTMgr'
	laprops[ 1, 2] = 'SDTRealName'
	laprops[ 2, 1] = 'oCdbkMgr'
	laprops[ 2, 2] = 'CBcField'
	laprops[ 3, 1] = 'oCdbkMgr'
	laprops[ 3, 2] = 'CBcType'
	laprops[ 4, 1] = 'oCdbkMgr'
	laprops[ 4, 2] = 'CBnSize'
	laprops[ 5, 1] = 'oCdbkMgr'
	laprops[ 5, 2] = 'CBnDecimals'
	laprops[ 6, 1] = 'oCdbkMgr'
	laprops[ 6, 2] = 'CBlNull'
	laprops[ 7, 1] = 'oCdbkMgr'
	laprops[ 7, 2] = 'CBlBinary'
	laprops[ 8, 1] = 'oSDTMgr'
	laprops[ 8, 2] = 'SDTObjectName'
	laprops[ 9, 1] = 'oSDTMgr'
	laprops[ 9, 2] = 'SDTDBCPath'
	laprops[10, 1] = 'oCdbkMgr'
	laprops[10, 2] = 'CBMPath'
	laprops[11, 1] = 'oCdbkMgr'
	laprops[11, 2] = 'CBcDBCName'
	laprops[12, 1] = 'oCdbkMgr'
	laprops[12, 2] = 'CBtLastMod'

***********************************
	laprops[13, 1] = 'oSDTMgr'
	laprops[13, 2] = 'SDTAutoClass'
	laprops[14, 1] = 'oSDTMgr'
	laprops[14, 2] = 'SDTAutoPrefix'
	laprops[15, 1] = 'oSDTMgr'
	laprops[15, 2] = 'SDTAutoLabel'
***************************************

*********************************************
* Add the extended properties for each field.

	lnfields = FCOUNT(tcalias)

	FOR lni = 1 TO lnfields
		lafields[lnI, 2] = UPPER(lafields[lnI, 2])

		lcfield = tcalias + '.' + FIELD(lni, tcalias)

		liid    = ometa.dbgetdbckey(tcdatabase, 'Field', lcfield)

		laprops[ 1, 3] = lafields[lnI, 1]
		laprops[ 2, 3] = FIELD(lni, tcalias)
		laprops[ 3, 3] = lafields[lnI, 2]
		laprops[ 4, 3] = lafields[lnI, 3]
		laprops[ 5, 3] = lafields[lnI, 4]
		laprops[ 6, 3] = lafields[lnI, 5]
		laprops[ 7, 3] = lafields[lnI, 6]
		laprops[ 8, 3] = lcfield
		laprops[ 9, 3] = lcsdtpath
		laprops[10, 3] = lccbmpath
		laprops[11, 3] = lccbmname
		laprops[12, 3] = DATETIME()

*----------------------
		DO CASE
			CASE lafields[lnI, 2] = 'M'
				laprops[13, 3] = 'Editbox'
				laprops[14, 3] = 'edt'
				laprops[15, 3] = .T.

			CASE lafields[lnI, 2] = 'L'
				laprops[13, 3] = 'CheckBox'
				laprops[14, 3] = 'chk'
				laprops[15, 3] = .F.

			CASE lafields[lnI, 2] $ 'NFIBY'
				laprops[13, 3] = 'Spinner'
				laprops[14, 3] = 'spn'
				laprops[15, 3] = .T.

			OTHERWISE
				laprops[13, 3] = 'TextBox'
				laprops[14, 3] = 'txt'
				laprops[15, 3] = .T.
		ENDCASE

		ometa.dbcxsetrowprop(@laprops, liid)
	NEXT

#ELSE
	DIMENSION laprops[15, 3]

	laprops[ 1, 1] = 'oCdbkMgr'
	laprops[ 1, 2] = 'CBcField'

	laprops[ 2, 1] = 'oCdbkMgr'
	laprops[ 2, 2] = 'CBcType'

	laprops[ 3, 1] = 'oCdbkMgr'
	laprops[ 3, 2] = 'CBnSize'

	laprops[ 4, 1] = 'oCdbkMgr'
	laprops[ 4, 2] = 'CBnDecimals'

	laprops[ 5, 1] = 'oCdbkMgr'
	laprops[ 5, 2] = 'CBlNull'

	laprops[ 6, 1] = 'oCdbkMgr'
	laprops[ 6, 2] = 'CBlBinary'

	laprops[ 7, 1] = 'oCdbkMgr'
	laprops[ 7, 2] = 'CBMPath'

	laprops[ 8, 1] = 'oCdbkMgr'
	laprops[ 8, 2] = 'CBcDBCName'

	laprops[ 9, 1] = 'oCdbkMgr'
	laprops[ 9, 2] = 'CBtLastMod'

*********************************************
* Add the extended properties for each field.

	lnfields = FCOUNT(tcalias)

	FOR lni = 1 TO lnfields
		lafields[lnI, 2] = UPPER(lafields[lnI, 2])

		lcfield = tcalias + '.' + FIELD(lni, tcalias)

		liid    = ometa.dbgetdbckey(tcdatabase, 'Field', lcfield)

		laprops[ 1, 3] = FIELD(lni, tcalias)
		laprops[ 2, 3] = lafields[lnI, 2]
		laprops[ 3, 3] = lafields[lnI, 3]
		laprops[ 4, 3] = lafields[lnI, 4]
		laprops[ 5, 3] = lafields[lnI, 5]
		laprops[ 6, 3] = lafields[lnI, 6]
		laprops[ 7, 3] = lccbmpath
		laprops[ 8, 3] = lccbmname
		laprops[ 9, 3] = DATETIME()

		ometa.dbcxsetrowprop(@laprops, liid)
	NEXT
#ENDIF

#IFDEF _stonefield
	DIMENSION laprops[11, 3]
	laprops[ 1, 1] = 'oCdbkMgr'
	laprops[ 1, 2] = 'CBnCDXOrder'
	laprops[ 2, 1] = 'oCdbkMgr'
	laprops[ 2, 2] = 'CBmTagFilter'
	laprops[ 3, 1] = 'oCdbkMgr'
	laprops[ 3, 2] = 'CBmTagExpr'
	laprops[ 4, 1] = 'oCdbkMgr'
	laprops[ 4, 2] = 'CBcTagType'
	laprops[ 5, 1] = 'oCdbkMgr'
	laprops[ 5, 2] = 'CBlAscending'
	laprops[ 6, 1] = 'oSDTMgr'
	laprops[ 6, 2] = 'SDTCollate'
	laprops[ 7, 1] = 'oSDTMgr'
	laprops[ 7, 2] = 'SDTObjectName'
	laprops[ 8, 1] = 'oSDTMgr'
	laprops[ 8, 2] = 'SDTDBCPath'
	laprops[ 9, 1] = 'oCdbkMgr'
	laprops[ 9, 2] = 'CBMPath'
	laprops[10, 1] = 'oCdbkMgr'
	laprops[10, 2] = 'CBcDBCName'
	laprops[11, 1] = 'oCdbkMgr'
	laprops[11, 2] = 'CBtLastMod'

	lcfile = JUSTSTEM(DBF(tcalias))
	lntags = TAGCOUNT(lcfile, tcalias)

*******************************************************************
** Create an array to hold the extended properties for each index.

	FOR lni = 1 TO lntags
		lcindex = tcalias + '.' + TAG(lni, tcalias)
		liid    = ometa.dbgetdbckey(tcdatabase, 'Index', lcindex)
		laprops[ 1, 3] = lni
		laprops[ 2, 3] = FOR(lni, tcalias)
		laprops[ 3, 3] = KEY(lni, tcalias)
		DO CASE
			CASE UNIQUE(lni, tcalias)
				laprops[ 4, 3] = 'U'
			CASE PRIMARY(lni, tcalias)
				laprops[ 4, 3] = 'P'
			CASE CANDIDATE(lni, tcalias)
				laprops[ 4, 3] = 'C'
			OTHERWISE
				laprops[ 4, 3] = 'R'
		ENDCASE
		laprops[ 5, 3] = NOT DESCENDING(lni, tcalias)
		laprops[ 6, 3] = IDXCOLLATE(lni, tcalias)
		laprops[ 7, 3] = lcindex
		laprops[ 8, 3] = lcsdtpath
		laprops[ 9, 3] = lccbmpath
		laprops[10, 3] = lccbmname
		laprops[11, 3] = DATETIME()

		ometa.dbcxsetrowprop(@laprops, liid)
	NEXT
#ELSE
	DIMENSION laprops[8, 3]
	laprops[ 1, 1] = 'oCdbkMgr'
	laprops[ 1, 2] = 'CBnCDXOrder'
	laprops[ 2, 1] = 'oCdbkMgr'
	laprops[ 2, 2] = 'CBmTagFilter'
	laprops[ 3, 1] = 'oCdbkMgr'
	laprops[ 3, 2] = 'CBmTagExpr'
	laprops[ 4, 1] = 'oCdbkMgr'
	laprops[ 4, 2] = 'CBcTagType'
	laprops[ 5, 1] = 'oCdbkMgr'
	laprops[ 5, 2] = 'CBlAscending'
	laprops[ 6, 1] = 'oCdbkMgr'
	laprops[ 6, 2] = 'CBMPath'
	laprops[ 7, 1] = 'oCdbkMgr'
	laprops[ 7, 2] = 'CBcDBCName'
	laprops[ 8, 1] = 'oCdbkMgr'
	laprops[ 8, 2] = 'CBtLastMod'

	lcfile = JUSTSTEM(DBF(tcalias))
	lntags = TAGCOUNT(lcfile, tcalias)

*******************************************************************
** Create an array to hold the extended properties for each index.

	FOR lni = 1 TO lntags
		lcindex = tcalias + '.' + TAG(lni, tcalias)
		liid    = ometa.dbgetdbckey(tcdatabase, 'Index', lcindex)
		laprops[ 1, 3] = lni
		laprops[ 2, 3] = FOR(lni, tcalias)
		laprops[ 3, 3] = KEY(lni, tcalias)

		DO CASE
			CASE UNIQUE(lni, tcalias)
				laprops[ 4, 3] = 'U'
			CASE PRIMARY(lni, tcalias)
				laprops[ 4, 3] = 'P'
			CASE CANDIDATE(lni, tcalias)
				laprops[ 4, 3] = 'C'
			OTHERWISE
				laprops[ 4, 3] = 'R'
		ENDCASE

		laprops[ 5, 3] = NOT DESCENDING(lni, tcalias)
		laprops[ 6, 3] = lccbmpath
		laprops[ 7, 3] = lccbmname
		laprops[ 8, 3] = DATETIME()

		ometa.dbcxsetrowprop(@laprops, liid)
	NEXT
#ENDIF

IF NOT llused
	USE IN (tcalias)
ENDIF

*JEI VM 20080828{
IF TYPE("goProgram.class") ="C" AND !ISNULL(goProgram)
	goProgram.vfxwaitwindow("", .F., .F., .T., .T., .T.)
ELSE
	WAIT CLEAR
ENDIF
*JEI VM 20080828}

RETURN .T.
ENDFUNC

*******************************************************************************************
**
FUNCTION extendonerelation(tcdatabase, ometa, tcchild, tcparent, tcchildtag, tcparenttag)
LOCAL llpused, ;
	lcfile, ;
	lcparent, ;
	llcused, ;
	lcchild, ;
	lcmetapath, ;
	lccbmpath, ;
	lccbmname, ;
	lcparentexpr, ;
	lcchildexpr, ;
	liid

* Open the tables if necessary.
IF TYPE("goProgram.class") ="C"
	goProgram.vfxwaitwindow("Processing Relations ...", , , .T., , ,)
ELSE
	WAIT WINDOW "Processing Relations ..." NOWAIT
ENDIF

llpused = USED(tcparent)

IF NOT llpused
	lcfile = FULLPATH(DBGETPROP(tcparent, 'Table', 'Path'), tcdatabase)

	IF NOT FILE(lcfile)
		RETURN .T.
	ENDIF

	USE (lcfile) ALIAS (tcparent) AGAIN NODATA IN 0
ENDIF

lcparent = JUSTSTEM(DBF(tcparent))
llcused = USED(tcchild)

IF NOT llcused
	lcfile = FULLPATH(DBGETPROP(tcchild, 'Table', 'Path'), tcdatabase)

	IF NOT FILE(lcfile)
		IF NOT llpused
			USE IN (tcparent)
		ENDIF
		RETURN .T.
	ENDIF

	USE (lcfile) ALIAS (tcchild) AGAIN NODATA IN 0
ENDIF

lcchild = JUSTSTEM(DBF(tcchild))

* Get the values we'll need.

lcmetapath   = DBF(ometa.osdtmgr.cdbcxalias)
lcmetapath   = LEFT(lcmetapath, RAT('\', lcmetapath))
lccbmpath    = JUSTPATH(SYS(2014, tcdatabase, lcmetapath))
lccbmname    = JUSTSTEM(tcdatabase)
lcsdtpath    = SYS(2014, tcdatabase, lcmetapath)
lcparentexpr = KEY(TAGNO(tcparenttag, lcparent, tcparent), tcparent)
lcchildexpr  = KEY(TAGNO(tcchildtag,  lcchild,  tcchild),  tcchild)
liid         = ometa.dbgetdbckey(tcdatabase, 'Relation', tcparent + ',' + tcchild)

* Store the values in the meta data.

WITH ometa

	#IFDEF _stonefield
		.dbcxsetprop('SDTDBCPath',   lcsdtpath,       liid)
	#ENDIF

	.dbcxsetprop('CBMPath',      lccbmpath,       liid)
	.dbcxsetprop('CBcDBCName',   lccbmname,       liid)
	.dbcxsetprop('CBcChildTbl',  LOWER(tcchild),  liid)
	.dbcxsetprop('CBcCursor',    LOWER(tcparent), liid)
	.dbcxsetprop('CBmParntExpr', lcparentexpr,    liid)
	.dbcxsetprop('CBmChildExpr', lcchildexpr,     liid)
	.dbcxsetprop('CBtLastMod',   DATETIME(),      liid)
ENDWITH

* Close the tables if necessary.

IF NOT llpused
	USE IN (tcparent)
ENDIF

IF NOT llcused
	USE IN (tcchild)
ENDIF
RETURN .T.

************************************************************************
**
FUNCTION extendoneview(tcdatabase, ometa, tcview)
LOCAL lcsql, ;
	lnpos1, ;
	lnpos2, ;
	lctables, ;
	lnstart, ;
	lni, ;
	lnend, ;
	lctable, ;
	lacurrtables[1], ;
	llused, ;
	lcmetapath, ;
	lccbmpath, ;
	lccbmname, ;
	lcsdtpath, ;
	liid, ;
	lafields[1], ;
	lnfields, ;
	lcfield, ;
	laprops[1], ;
	latablesopen[1]

* Ensure each of the tables in the viw exists.
IF TYPE("goProgram.class") ="C"
	goProgram.vfxwaitwindow("Processing View " + tcview + " ...", , , .T., , ,)
ELSE
	WAIT WINDOW "Processing View " + tcview + " ..." NOWAIT
ENDIF

lcsql = DBGETPROP(tcview, 'View', 'SQL')

IF EMPTY(lcsql)
	RETURN .T.
ELSE
	lnpos1   = ATC(' FROM ',  lcsql)
	lnpos2   = ATC(' WHERE ', lcsql)
	lnpos2   = IIF(lnpos2 = 0, ATC(' ORDER ',  lcsql), lnpos2)
	lnpos2   = IIF(lnpos2 = 0, ATC(' GROUP ',  lcsql), lnpos2)
	lnpos2   = IIF(lnpos2 = 0, ATC(' HAVING ', lcsql), lnpos2)
	lnpos2   = IIF(lnpos2 = 0, LEN(lcsql) + 6,         lnpos2)

	lctables = SUBSTR(lcsql, lnpos1 + 6, lnpos2 - lnpos1 - 6)
	lnstart  = 1

	FOR lni = 1 TO OCCURS(',', lctables) + 1
		IF lni = OCCURS(',', lctables) + 1
			lnend = LEN(lctables)
		ELSE
			lnend = AT(',', lctables, lni) - 1
		ENDIF

		lctable = ALLTRIM(SUBSTR(lctables, lnstart, lnend - lnstart + 1))
		lctable = SUBSTR(lctable, AT('!', lctable) + 1)

		IF NOT FILE(FULLPATH(DBGETPROP(lctable, 'Table', 'Path'), tcdatabase))
			RETURN .T.
		ENDIF

		lnstart = lnend + 2
	NEXT
ENDIF

* Open the view if necessary.

 = AUSED(lacurrtables)

llused = USED(tcview)

IF NOT llused
	USE (tcview) AGAIN NODATA IN 0
ENDIF

* Add the extended properties for the current view.

lcmetapath = DBF(ometa.osdtmgr.cdbcxalias)
lcmetapath = LEFT(lcmetapath, RAT('\', lcmetapath))
lccbmpath  = JUSTPATH(SYS(2014, tcdatabase, lcmetapath))
lccbmname  = JUSTSTEM(tcdatabase)
lcsdtpath  = SYS(2014, tcdatabase, lcmetapath)
liid       = ometa.dbgetdbckey(tcdatabase, 'View', tcview)

WITH ometa
	#IFDEF _stonefield
		.dbcxsetprop('SDTDBCPath', lcsdtpath,  liid)
	#ENDIF
	.dbcxsetprop('CBMPath',    lccbmpath,  liid)
	.dbcxsetprop('CBcDBCName', lccbmname,  liid)
	.dbcxsetprop('CBtLastMod', DATETIME(), liid)
ENDWITH

* Create an array to hold the extended properties for each field.

#IFDEF _stonefield
	DIMENSION laprops[11, 3]

	laprops[ 1, 1] = 'oCdbkMgr'
	laprops[ 1, 2] = 'CBcDBCName'
	laprops[ 2, 1] = 'oCdbkMgr'
	laprops[ 2, 2] = 'CBcField'
	laprops[ 3, 1] = 'oCdbkMgr'
	laprops[ 3, 2] = 'CBcType'
	laprops[ 4, 1] = 'oCdbkMgr'
	laprops[ 4, 2] = 'CBnSize'
	laprops[ 5, 1] = 'oCdbkMgr'
	laprops[ 5, 2] = 'CBnDecimals'
	laprops[ 6, 1] = 'oCdbkMgr'
	laprops[ 6, 2] = 'CBlNull'
	laprops[ 7, 1] = 'oCdbkMgr'
	laprops[ 7, 2] = 'CBlBinary'
	laprops[ 8, 1] = 'oSDTMgr'
	laprops[ 8, 2] = 'SDTObjectName'
	laprops[ 9, 1] = 'oSDTMgr'
	laprops[ 9, 2] = 'SDTDBCPath'
	laprops[10, 1] = 'oCdbkMgr'
	laprops[10, 2] = 'CBMPath'
	laprops[11, 1] = 'oCdbkMgr'
	laprops[11, 2] = 'CBtLastMod'

* Add the extended properties for each field.

	lnfields = AFIELDS(lafields, tcview)

	FOR lni = 1 TO lnfields
		lcfield = tcview + '.' + lafields[lnI, 1]
		liid    = ometa.dbgetdbckey(tcdatabase, 'Field', lcfield)

		laprops[ 1, 3] = lccbmname
		laprops[ 2, 3] = lafields[lnI, 1]
		laprops[ 3, 3] = lafields[lnI, 2]
		laprops[ 4, 3] = lafields[lnI, 3]
		laprops[ 5, 3] = lafields[lnI, 4]
		laprops[ 6, 3] = lafields[lnI, 5]
		laprops[ 7, 3] = lafields[lnI, 6]
		laprops[ 8, 3] = lcfield
		laprops[ 9, 3] = lcsdtpath
		laprops[10, 3] = lccbmpath
		laprops[11, 3] = DATETIME()

		ometa.dbcxsetrowprop(@laprops, liid)
	NEXT

* Close any tables if necessary.

	 = AUSED(latablesopen)

	FOR lni = 1 TO ALEN(latablesopen, 1)
		lctable = latablesopen[lnI, 1]

		IF NOT EMPTY(lctable) AND arrayscan(@lacurrtables, lctable) = 0
			USE IN (lctable)
		ENDIF
	NEXT
#ELSE
	DIMENSION laprops[11, 3]

	laprops[ 1, 1] = 'oCdbkMgr'
	laprops[ 1, 2] = 'CBcDBCName'
	laprops[ 2, 1] = 'oCdbkMgr'
	laprops[ 2, 2] = 'CBcField'
	laprops[ 3, 1] = 'oCdbkMgr'
	laprops[ 3, 2] = 'CBcType'
	laprops[ 4, 1] = 'oCdbkMgr'
	laprops[ 4, 2] = 'CBnSize'
	laprops[ 5, 1] = 'oCdbkMgr'
	laprops[ 5, 2] = 'CBnDecimals'
	laprops[ 6, 1] = 'oCdbkMgr'
	laprops[ 6, 2] = 'CBlNull'
	laprops[ 7, 1] = 'oCdbkMgr'
	laprops[ 7, 2] = 'CBlBinary'
	laprops[ 8, 1] = 'oCdbkMgr'
	laprops[ 8, 2] = 'CBMPath'
	laprops[ 9, 1] = 'oCdbkMgr'
	laprops[ 9, 2] = 'CBtLastMod'

* Add the extended properties for each field.

	lnfields = AFIELDS(lafields, tcview)

	FOR lni = 1 TO lnfields
		lcfield = tcview + '.' + lafields[lnI, 1]
		liid    = ometa.dbgetdbckey(tcdatabase, 'Field', lcfield)

		laprops[ 1, 3] = lccbmname
		laprops[ 2, 3] = lafields[lnI, 1]
		laprops[ 3, 3] = lafields[lnI, 2]
		laprops[ 4, 3] = lafields[lnI, 3]
		laprops[ 5, 3] = lafields[lnI, 4]
		laprops[ 6, 3] = lafields[lnI, 5]
		laprops[ 7, 3] = lafields[lnI, 6]
		laprops[ 8, 3] = lccbmpath
		laprops[ 9, 3] = DATETIME()

		ometa.dbcxsetrowprop(@laprops, liid)
	NEXT

* Close any tables if necessary.

	 = AUSED(latablesopen)

	FOR lni = 1 TO ALEN(latablesopen, 1)
		lctable = latablesopen[lnI, 1]

		IF NOT EMPTY(lctable) AND arrayscan(@lacurrtables, lctable) = 0
			USE IN (lctable)
		ENDIF
	NEXT
#ENDIF

RETURN .T.

*!* SDT: Functions not provided with SDT 5.1. UH 18.07.99
*#ifndef _stonefield
*==============================================================================
* Function:			ArrayScan
* Purpose:			Find a particular value in a certain column in an array
* Author:			Doug Hennig
* Copyright:		(c) 1995 Stonefield Systems Group Inc.
* Last Revision:	12/10/95
* Parameters:		taArray  - the array (passed by reference using @) to
*						search
*					tuValue  - the value to search for
*					tnColumn - the column to search (optional: if it isn't
*						specified, column 1 is searched)
*					tnOccur  - the occurrance to search for (optional: if it
*						isn't specified, the first occurrance is located)
* Returns:			the row the value was found in if it was found, or 0 if
*						not
* Environment in:	none
* Environment out:	none
*==============================================================================

FUNCTION arrayscan
LPARAMETERS taarray, ;
	tuvalue, ;
	tncolumn, ;
	tnoccur
EXTERNAL ARRAY taarray
LOCAL lncolumn, ;
	lnoccur, ;
	lnrow, ;
	lnstartelement, ;
	lnfound, ;
	lncolumns, ;
	lnelement, ;
	lncol
lncolumn       = IIF(VARTYPE(tncolumn) = 'N', tncolumn, 1)
lnoccur        = IIF(VARTYPE(tnoccur)  = 'N', tnoccur,  1)
lnrow          = 0
lnstartelement = 1
lnfound        = 0
lncolumns      = ALEN(taarray, 2)

DO WHILE .T.
	lnelement = ASCAN(taarray, tuvalue, lnstartelement, ALEN(taarray), 0, 7)
	IF lnelement <> 0
		lncol = IIF(lncolumns > 1, ASUBSCRIPT(taarray, lnelement, 2), 1)
		IF lncol = lncolumn AND (VARTYPE(tuvalue) <> 'C' OR ;
				taarray[lnElement] == tuvalue)
			lnfound = lnfound + 1
			IF lnfound = lnoccur
				lnrow = IIF(lncolumns > 1, ASUBSCRIPT(taarray, lnelement, 1), ;
					lnelement)
				EXIT
			ENDIF
		ENDIF
		lnstartelement = lnelement + 1
	ELSE
		EXIT
	ENDIF
ENDDO
RETURN lnrow
ENDFUNC

*==============================================================================
* Function:			CreateUniqueAlias
* Purpose:			Create a unique alias for a table
* Author:			Doug Hennig
* Copyright:		(c) 1995 Stonefield Systems Group Inc.
* Last Revision:	12/10/95
* Parameters:		none
* Returns:			a unique alias
* Environment in:	none
* Environment out:	none
*==============================================================================

FUNCTION createuniquealias
LOCAL lcalias
lcalias = "X" + SUBSTR(SYS(2015), 4, 7)
DO WHILE USED(lcalias)
	lcalias = "X" + SUBSTR(SYS(2015), 4, 7)
ENDDO
RETURN lcalias
ENDFUNC

*#ENDIF