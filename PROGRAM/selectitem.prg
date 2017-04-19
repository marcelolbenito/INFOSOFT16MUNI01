FUNCTION onSelectItem
LPARAMETERS tcMenuName, tlFromClick
DO CASE
CASE ALLTRIM(UPPER(tcMenuName)) == 'OPEN'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_OPEN")))
RETURN .F.
ENDIF
goProgram.RunForm("VFXOPEN")

CASE ALLTRIM(UPPER(tcMenuName)) == 'CLOSE'
IF TYPE("_screen.ActiveForm")=="O" 
	IF PEMSTATUS(_screen.ActiveForm,"QueryUnload",5)
		IF _screen.ActiveForm.QueryUnload()
		    _screen.ActiveForm.Release()
		ENDIF		    
	ELSE
	    release _screen.ActiveForm
	ENDIF
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'PAGE_SETUP'
IF OS(5) > "7000"
	TRY
		SET PRINTER TO GETPRINTER()
	CATCH
	ENDTRY
ENDIF
TRY
	=SYS(1037)
CATCH
ENDTRY

CASE ALLTRIM(UPPER(tcMenuName)) == 'PRINT'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT")))
RETURN .F.
ENDIF
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint()
ENDIF	

CASE ALLTRIM(UPPER(tcMenuName)) == 'PREVIEW'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(.T.)
ENDIF	

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHPDF'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(1) && e-mail attach PDF
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHHTML'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(11) && e-mail attach HTML
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHXML'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(12) && e-mail attach XML
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHTIFF'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(13) && e-mail attach TIFF
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHBMP'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(14) && e-mail attach BMP
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHCSV'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(19) && e-mail attach CSV
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHEXCEL'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(20) && e-mail attach Excel
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ATTACHDBF'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(21) && e-mail attach DBF
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_FAX'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(3) && Fax
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_PDF'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(2) && PDF
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_HTML'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(6) && HTML
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_XML'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(7) && XML
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_TIFF'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(8) && TIFF
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_BMP'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(9) && BMP
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_CSV'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(15) && CSV
ENDIF	

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_EXCEL'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(16) && Excel
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_XML1'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(17) && XML
ENDIF	

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_DBF'
IF TYPE("_screen.ActiveForm.lEmpty") <> "U"
	_screen.ActiveForm.OnPrint(18) && DBF
ENDIF	

CASE ALLTRIM(UPPER(tcMenuName)) == 'EXIT'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_EXIT")))
RETURN .F.
ENDIF
IF goProgram.nMenuAndToolbarStyle >= 2 
	KEYBOARD '{ALT+F4}'
ELSE
	goProgram.OnQuit()
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'UNDO'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_RESTORE")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnUndo()

CASE ALLTRIM(UPPER(tcMenuName)) == 'SAVE'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_SAVE")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnSave()

CASE ALLTRIM(UPPER(tcMenuName)) == 'EDIT_10S0O890D'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_EDIT")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnEdit()

CASE ALLTRIM(UPPER(tcMenuName)) == 'NEW'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_NEW")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnInsert()

CASE ALLTRIM(UPPER(tcMenuName)) == 'COPY_RECORD'
_screen.ActiveForm.OnCopy()

CASE ALLTRIM(UPPER(tcMenuName)) == 'DELETE'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_DELETE")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnDelete()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_REQUERY'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_REQUERY")))
RETURN .F.
ENDIF
IF PEMSTATUS(_Screen.ActiveForm, "OnRefresh" ,5)
	_Screen.ActiveForm.OnRefresh(.F.,.T.)
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'FIND'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_FIND")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnSearch()

CASE ALLTRIM(UPPER(tcMenuName)) == 'OTHER'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_MORE")))
RETURN .F.
ENDIF
_screen.ActiveForm.OnMore()

CASE ALLTRIM(UPPER(tcMenuName)) == 'TOOLBARS'
goProgram.RunForm("VFXTBAR")

CASE ALLTRIM(UPPER(tcMenuName)) == 'NEXT_PAGE'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("VIEW_NEXTPAGE")))
RETURN .F.
ENDIF
_screen.activeForm.OnNextPage()

CASE ALLTRIM(UPPER(tcMenuName)) == 'PREVIOUS_PAGE'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("VIEW_PREVPAGE")))
RETURN .F.
ENDIF
_screen.activeForm.OnPrevPage()

CASE ALLTRIM(UPPER(tcMenuName)) == 'TOP_REC'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_TOP")))
RETURN .F.
ENDIF
_screen.activeForm.OnTop()

CASE ALLTRIM(UPPER(tcMenuName)) == 'PREVIOUS'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_PREV")))
RETURN .F.
ENDIF
_screen.activeForm.OnPrev()

CASE ALLTRIM(UPPER(tcMenuName)) == 'NEXT'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_NEXT")))
RETURN .F.
ENDIF
_screen.activeForm.OnNext()

CASE ALLTRIM(UPPER(tcMenuName)) == 'BOTTOM'
IF !tlFromClick AND (TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("EDIT_BOTTOM")))
RETURN .F.
ENDIF
_screen.activeForm.OnBottom()

CASE ALLTRIM(UPPER(tcMenuName)) == 'ADD_TO_FAVORITES'
addtofavorite()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MANAGE_FAVORITES'
runmanagefavorites()

CASE ALLTRIM(UPPER(tcMenuName)) == 'USER_LIST'
goProgram.RunForm("VFXUSER")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_USERGROUPS1'
goProgram.RunForm("VFXUSERGROUPS")

CASE ALLTRIM(UPPER(tcMenuName)) == 'USER_RIGHTS'
goProgram.RunForm("VFXRIGHT")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_USERSSTATUS'
goProgram.RunForm("vfxUsersStatus")

CASE ALLTRIM(UPPER(tcMenuName)) == 'LOGIN'
goProgram.ReLogon()

CASE ALLTRIM(UPPER(tcMenuName)) == 'DATABASE'
goProgram.RunForm("VFXDBFUN")

CASE ALLTRIM(UPPER(tcMenuName)) == 'AUDITTRAIL'
_screen.activeform.onaudit()

CASE ALLTRIM(UPPER(tcMenuName)) == 'AUDITINFORMATION'
goProgram.runform("VFXAUDITINFO")

CASE ALLTRIM(UPPER(tcMenuName)) == 'SYSTEM_ERRORS'
goProgram.RunForm("VFXLOG")

CASE ALLTRIM(UPPER(tcMenuName)) == 'SYSTEM_LOCKS'
goProgram.RunForm("VFXLOCK")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_FULLSCREENSHOT'
goProgram.printscreen()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ACTIVEFORMSCREENSHOT'
goProgram.printscreen(.T.)

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_MANAGECONFIGVFX'
goProgram.RunForm("vfxclientdataaccess")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_MANAGEPICKLISTS'
goProgram.RunForm("vfxplist")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_ADDTOTASKLIST'
addtofavorite(.T.)

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_TASKLIST'
goProgram.runForm("vfxTaskList")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_TOOLBOX'
goProgram.runForm("vfxToolbox")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_INTELLISENSETABLEEDITOR'
goProgram.runForm("vfxIntellisenseTableEditor")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_CRATEBACKUPARCHIVE1'
BackUp(1, IIF(TYPE("goProgram.Class") == "C", GoProgram.lShowGetDirForZip, .F.))

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_RESTOREFROMBACKUP1'
BackUp(2)

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_MODIFYREPORT1'
DECLARE INTEGER ShellExecute IN SHELL32.DLL  ;
		INTEGER nWinHandle, ;
		STRING cOperation, ;
		STRING cFileName, ;
		STRING cParameters, ;
		STRING cDirectory, ;
		INTEGER nShowWindow
lcExeFile = FULLPATH("vfxmodifyreport.exe")
IF goProgram.lRuntimeLocalization
	lcAppCaption = ALLTRIM(goLocalize.cCAP_APPLICATION_TITLE) 
ELSE
	lcAppCaption = ALLTRIM(CAP_APPLICATION_TITLE)
ENDIF 
shellexecute(0,"open",lcExeFile,goProgram.cLangID+[ "]+lcAppCaption+["],"",1)

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_DATAEXPLORER'
DoVfxDataExplorer()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_LOGINIPADDRESSES'
goProgram.RunForm("vfxipaddresses")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_COMMANDCONSOLE'
goProgram.RunForm("vfxcommandconsole")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_CUSTOMIZE'
goProgram.RunForm("vfxcustomize")

CASE ALLTRIM(UPPER(tcMenuName)) == 'OPTIONS'
goProgram.RunForm("VFXSYS")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_COUNTRYMANAGEMENT'
goProgram.RunForm("vfxcountry")

CASE ALLTRIM(UPPER(tcMenuName)) == '_MST_HPSCH'
=ContextHelp()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_VISITOURWEBSITE'
LOCAL loHyperLink as HyperLink

IF TYPE("goProgram.cCompanyWebSiteURL") = "C" AND !EMPTY(goProgram.cCompanyWebSiteURL)
	loHyperLink = NEWOBJECT("HyperLink")
	loHyperLink.NavigateTo(goProgram.cCompanyWebSiteURL)
	loHyperLink = .Null.
	RELEASE loHyperLink
ENDIF

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_CONTACTUS'
ContactUS()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_USERFEEDBACK'
goprogram.runform("vfxuserfeedback")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_REMOTECONTROL'
goProgram.RemoteAdministration()

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_APPLICATIONUPDATE'
loUpdate = NEWOBJECT("cAppUpdateEngine")
loUpdate.startupdate(1)
RELEASE loUpdate

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_UPDATESETTINGS'
goProgram.RunForm("VFXUPDATE")

CASE ALLTRIM(UPPER(tcMenuName)) == 'MEN_REGISTER'
goprogram.runform("vfxregister")

CASE ALLTRIM(UPPER(tcMenuName)) == 'ABOUT'
goProgram.RunForm("VFXABOUT")

CASE ALLTRIM(UPPER(tcMenuName)) == 'BARRIOS1'
goProgram.RunForm('barrios')

CASE ALLTRIM(UPPER(tcMenuName)) == 'COLORES2'
goProgram.RunForm('colores')

CASE ALLTRIM(UPPER(tcMenuName)) == 'COMERCIOS3'
goProgram.RunForm('comercios')

CASE ALLTRIM(UPPER(tcMenuName)) == 'PARTNAC4'
goProgram.RunForm('partnac')

CASE ALLTRIM(UPPER(tcMenuName)) == 'DPTOS5'
goProgram.RunForm('dptos')

CASE ALLTRIM(UPPER(tcMenuName)) == 'LOCALIDADES6'
goProgram.RunForm('localidades')

CASE ALLTRIM(UPPER(tcMenuName)) == 'PROVINCIAS7'
goProgram.RunForm('provincias')

CASE ALLTRIM(UPPER(tcMenuName)) == 'TIPODOC8'
goProgram.RunForm('tipodoc')

CASE ALLTRIM(UPPER(tcMenuName)) == 'CONTRIBUYENTES9'
goProgram.RunForm('contribuyentes')

CASE ALLTRIM(UPPER(tcMenuName)) == 'INMUEBLES10'
goProgram.RunForm('inmuebles')

CASE ALLTRIM(UPPER(tcMenuName)) == 'MARCAS11'
goProgram.RunForm('marcas')

CASE ALLTRIM(UPPER(tcMenuName)) == 'MODELOS12'
goProgram.RunForm('modelos')

CASE ALLTRIM(UPPER(tcMenuName)) == 'VEHICULOS13'
goProgram.RunForm('vehiculos')

CASE ALLTRIM(UPPER(tcMenuName)) == 'ESTADOS14'
goProgram.RunForm('estados')

CASE ALLTRIM(UPPER(tcMenuName)) == '_MWI_DEBUG'
	DEBUG
CASE ALLTRIM(UPPER(tcMenuName)) == '_MPR_CANCL'
	CANCEL
CASE ALLTRIM(UPPER(tcMenuName)) == '_MPR_RESUM'
	RESUME
CASE ALLTRIM(UPPER(tcMenuName)) == '_MPR_SUSPEND'
	SUSPEND
CASE ALLTRIM(UPPER(tcMenuName)) == '_MWI_VIEW'
	SET
CASE ALLTRIM(UPPER(tcMenuName)) == '_MWI_CASCADE'
	SYS(1500, '_MWI_CASCADE', '_MWINDOW')
CASE ALLTRIM(UPPER(tcMenuName)) == '_MWI_ARRAN'
	SYS(1500, '_MWI_ARRAN', '_MWINDOW')
ENDCASE
ENDFUNC

DEFINE CLASS CustomMenuEventHandler AS Custom
nDefaultItemWidth = 150
nDefaultAlignment = 2

FUNCTION onSelectMenu()
   AEVENTS(paSource, 0)
   onSelectItem(paSource[1].cItemKey, .T.)
ENDFUNC

FUNCTION AUDIT_onClick
LOCAL loItem, loSubMenu
AEVENTS(paSource, 0)
paSource[1].lSelected = .T.
loSubMenu = CREATEOBJECT('cPopupDialog', paSource[1], goProgram.oMenuBar.cntTabMenu)
WITH losubMenu
   .Width  = goProgram.otabMenuHandler.nDefaultItemWidth
   .Height = 144
   .nPopupStyle = 1
   .Resize()
loItem = .cntPopupItems.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMAUDITTRAIL, MEN_PRMAUDITTRAIL)), "NORM","")
loItem.cItemKey = 'AUDITTRAIL'
lcPictureName = JUSTFNAME('..\BITMAP\TOOLBAR\AUDIT.BMP')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.Alignment = 0
loItem.cSkipForExp = ''
BINDEVENT(loItem, 'Execute', goProgram.otabMenuHandler, 'onSelectMenu')
loItem = .cntPopupItems.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMAUDITINFORMATION, MEN_PRMAUDITINFORMATION)), "NORM","")
loItem.cItemKey = 'AUDITINFORMATION'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.Alignment = 0
loItem.cSkipForExp = ''
BINDEVENT(loItem, 'Execute', goProgram.otabMenuHandler, 'onSelectMenu')
goProgram.oMenuBar.cntTabMenu.oSubMenu = loSubMenu
   .Show()
ENDWITH
ENDFUNC

FUNCTION PrintScreen_onClick
LOCAL loItem, loSubMenu
AEVENTS(paSource, 0)
paSource[1].lSelected = .T.
loSubMenu = CREATEOBJECT('cPopupDialog', paSource[1], goProgram.oMenuBar.cntTabMenu)
WITH losubMenu
   .Width  = goProgram.otabMenuHandler.nDefaultItemWidth
   .Height = 144
   .nPopupStyle = 1
   .Resize()
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("TOOLS_FULLSCREENSHOT"))
loItem = .cntPopupItems.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMFULLSCREENSHOT, MEN_PRMFULLSCREENSHOT)), "NORM","")
loItem.cItemKey = 'MEN_FULLSCREENSHOT'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.Alignment = 0
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("TOOLS_FULLSCREENSHOT"))'
BINDEVENT(loItem, 'Execute', goProgram.otabMenuHandler, 'onSelectMenu')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("TOOLS_ACTIVEFORMSCREENSHOT"))
loItem = .cntPopupItems.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMACTIVEFORMSCREENSHOT, MEN_PRMACTIVEFORMSCREENSHOT)), "NORM","")
loItem.cItemKey = 'MEN_ACTIVEFORMSCREENSHOT'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.Alignment = 0
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("TOOLS_ACTIVEFORMSCREENSHOT"))'
BINDEVENT(loItem, 'Execute', goProgram.otabMenuHandler, 'onSelectMenu')
ENDIF
goProgram.oMenuBar.cntTabMenu.oSubMenu = loSubMenu
   .Show()
ENDWITH
ENDFUNC

FUNCTION MEN_BACKUP_onClick
LOCAL loItem, loSubMenu
AEVENTS(paSource, 0)
paSource[1].lSelected = .T.
loSubMenu = CREATEOBJECT('cPopupDialog', paSource[1], goProgram.oMenuBar.cntTabMenu)
WITH losubMenu
   .Width  = goProgram.otabMenuHandler.nDefaultItemWidth
   .Height = 144
   .nPopupStyle = 1
   .Resize()
IF TYPE("goProgram.Class") == "C" AND  goProgram.IsMenuItemAllowed("TOOLS_BACKUP")
loItem = .cntPopupItems.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMCRATEBACKUPARCHIVE, MEN_PRMCRATEBACKUPARCHIVE)), "NORM","")
loItem.cItemKey = 'MEN_CRATEBACKUPARCHIVE1'
lcPictureName = JUSTFNAME('..\BITMAP\TOOLBAR\ZIP.BMP')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.Alignment = 0
loItem.cSkipForExp = ''
BINDEVENT(loItem, 'Execute', goProgram.otabMenuHandler, 'onSelectMenu')
ENDIF
IF TYPE("goProgram.Class") == "C" AND  goProgram.IsMenuItemAllowed("TOOLS_BACKUP")
loItem = .cntPopupItems.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMRESTOREFROMBACKUP, MEN_PRMRESTOREFROMBACKUP)), "NORM","")
loItem.cItemKey = 'MEN_RESTOREFROMBACKUP1'
lcPictureName = JUSTFNAME('..\BITMAP\TOOLBAR\UNZIP.BMP')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.Alignment = 0
loItem.cSkipForExp = ''
BINDEVENT(loItem, 'Execute', goProgram.otabMenuHandler, 'onSelectMenu')
ENDIF
goProgram.oMenuBar.cntTabMenu.oSubMenu = loSubMenu
   .Show()
ENDWITH
ENDFUNC

ENDDEFINE


FUNCTION onSelectStartSubMenu
LPARAMETERS tcItemKey, toObject
DO CASE 
CASE tcItemKey = 'EMAIL'
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHPDF, MEN_PRMATTACHPDF)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHPDF'
lcPictureName = JUSTFNAME('..\BITMAP\TOOLBAR\PDF.BMP')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHHTML, MEN_PRMATTACHHTML)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHHTML'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHXML, MEN_PRMATTACHXML)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHXML'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHTIFF, MEN_PRMATTACHTIFF)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHTIFF'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHBMP, MEN_PRMATTACHBMP)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHBMP'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHCSV, MEN_PRMATTACHCSV)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHCSV'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHEXCEL, MEN_PRMATTACHEXCEL)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHEXCEL'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("FILE_EMAIL") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMATTACHDBF, MEN_PRMATTACHDBF)), "NORM","")
loItem.cItemKey = 'MEN_ATTACHDBF'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
CASE tcItemKey = 'MEN_SAVEAS'
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMPDF, MEN_PRMPDF)), "NORM","")
loItem.cItemKey = 'MEN_PDF'
lcPictureName = JUSTFNAME('..\BITMAP\TOOLBAR\PDF.BMP')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMHTML, MEN_PRMHTML)), "NORM","")
loItem.cItemKey = 'MEN_HTML'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMXML, MEN_PRMXML)), "NORM","")
loItem.cItemKey = 'MEN_XML'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMTIFF, MEN_PRMTIFF)), "NORM","")
loItem.cItemKey = 'MEN_TIFF'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMBMP, MEN_PRMBMP)), "NORM","")
loItem.cItemKey = 'MEN_BMP'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_PRINT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
CASE tcItemKey = 'MEN_EXPORTTO'
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMCSV, MEN_PRMCSV)), "NORM","")
loItem.cItemKey = 'MEN_CSV'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_EXPORT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMEXCEL, MEN_PRMEXCEL)), "NORM","")
loItem.cItemKey = 'MEN_EXCEL'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_EXPORT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE") AND goProgram.IsMenuItemAllowed("FILE_PRINT"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMXML, MEN_PRMXML)), "NORM","")
loItem.cItemKey = 'MEN_XML1'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_EXPORT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
IF TYPE("goProgram.Class") == "C" AND (goProgram.IsMenuItemAllowed("EDIT_SAVE"))
loItem = toObject.AddPopupItem(ALLTRIM(IIF(goProgram.lRuntimeLocalization, goLocalize.cMEN_PRMDBF, MEN_PRMDBF)), "NORM","")
loItem.cItemKey = 'MEN_DBF'
lcPictureName = JUSTFNAME('')
IF TYPE('goProgram.Class') == 'C' AND goProgram.nColorDepth <= 8
	IF !EMPTY(lcPictureName) AND FILE(ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName))
		lcPictureName = ADDBS(JUSTPATH(lcPictureName)) + JUSTSTEM(lcPictureName) + '_8bit.' + JUSTEXT(lcPictureName)
	ENDIF
ENDIF
IF FILE(lcPictureName)
   loItem.cPicture = lcPictureName
ENDIF 
loItem.cSkipForExp = 'TYPE("goProgram.Class") == "C" AND (goProgram.OnSkipMenu("FILE_EXPORT"))'
BINDEVENT(loItem, 'Execute', goProgram.oMenuBar, 'onItemExecute')
ENDIF
ENDCASE
ENDFUNC
