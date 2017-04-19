******************************************************************************
* Project...........: Visual Extend 16.0
*                     Copyright (c) dFPUG c/o ISYS GmbH.
*                     All Rights Reserved.
*
* Program...........: VFX.H
* Author ...........: ISYS GmbH
* Created...........: October 2013
* Version...........: 16.00.0000
*
* Description.......:
* Calling Samples...:
* Parameter List....:
* Major change list.:
*

#INCLUDE "INCLUDE\FOXPRO.H"

#DEFINE _VFX_VERSION    "16.00.1808"

******************************************************
* REMOVE THE REMARKS FOR THE OPTIONS YOU LIKE

* #DEFINE _DEBUG_MODE		.T.
#DEFINE _LANG_SETUP		.T.

* #DEFINE _DBCX			.T.
* #DEFINE _STONEFIELD		.T.
* #DEFINE _USE_STDLIB		.T.
******************************************************

#IFDEF _STONEFIELD
#IFNDEF _DBCX
#DEFINE _DBCX				.T.
#ENDIF
#ENDIF

#INCLUDE "INCLUDE\VFXDEF.H"
#INCLUDE "INCLUDE\VFXTXT.H"
#INCLUDE "INCLUDE\VFXMSG.H"
#INCLUDE "INCLUDE\VFXOFFCE.H"
#INCLUDE "INCLUDE\VFXTOOLBOX.H"
#INCLUDE "INCLUDE\REPORTLISTENERS.H"
#INCLUDE "ReportProcessing\REPORTOUTPUT.H"
#INCLUDE "ReportProcessing\FRXPREVIEW.H"
#INCLUDE "INCLUDE\FOXPRO_REPORTING.H"
#INCLUDE "INCLUDE\_FRXCURSOR.H"
#INCLUDE "INCLUDE\VFXGLOBAL.H"
#INCLUDE "INCLUDE\PDFLISTENER.H"
#INCLUDE "INCLUDE\VFXFTPUPLOAD.H"
#INCLUDE "INCLUDE\HPDF_CONSTS.H"

#INCLUDE "INCLUDE\USERTXT.H"
#INCLUDE "INCLUDE\USERDEF.H"
#INCLUDE "INCLUDE\USERMSG.H"