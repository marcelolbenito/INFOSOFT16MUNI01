******************************************************************************
* Project...........: Visual Extend 16.0
*                     Copyright (c) dFPUG c/o ISYS GmbH
*                     All Rights Reserved.
*
* Program...........: VFXDEF.H
* Author ...........: ISYS GmbH
* Created...........: October 2009
* Version...........: 16.00.0000
*
* Description.......:
* Calling Samples...:
* Parameter List....:
* Major change list.:
*

#DEFINE _VFX_HOMEDIR                 ""

#define ID_LANGUAGE "Esp"

* #DEFINE ID_LANGUAGE                "ENG"
* #DEFINE ID_LANGUAGE                "FRE"
* #DEFINE ID_LANGUAGE                "GER"
* #DEFINE ID_LANGUAGE                "ITA"
* #DEFINE ID_LANGUAGE                "ESP"
* #DEFINE ID_LANGUAGE                "USR"


#DEFINE ID_WINOFFTOP                23
#DEFINE ID_WINOFFLEFT               23

#DEFINE ID_CR                       CHR(13)
#DEFINE ID_TAB                      CHR(9)
#DEFINE ID_CRLF                     CHR(13) + CHR(10)

#DEFINE ID_NORMAL_MODE              0
#DEFINE ID_EDIT_MODE                1
#DEFINE ID_INSERT_MODE              2

#DEFINE ID_ON                       "ON"
#DEFINE ID_OFF                      "OFF"

*-- Constants used to read the system registry
#DEFINE HKEY_LOCAL_MACHINE      -2147483646
#DEFINE KEY_SHARED_TOOLS_LOCATION  "Software\Microsoft\Shared Tools\MSInfo"
#DEFINE KEY_NTCURRENTVERSION       "Software\Microsoft\Windows NT\CurrentVersion"
#DEFINE KEY_WIN4CURRENTVERSION     "Software\Microsoft\Windows\CurrentVersion"
#DEFINE KEY_QUERY_VALUE        1
#DEFINE ERROR_SUCCESS        0

#DEFINE ERROR_USER_DEFINED           1098
#DEFINE ERROR_RECORD_IN_USE          1502
#DEFINE ERROR_CANT_LOCK_FILE         1503
#DEFINE ERROR_CONNECTIVITY_ERROR     1526
#DEFINE ERROR_TRIGGER_FAILED         1539
#DEFINE ERROR_CONNECTION_BUSY        1541
#DEFINE ERROR_FIELD_CANT_BE_NULL     1581
#DEFINE ERROR_FIELD_RULE_VIOLATED    1582
#DEFINE ERROR_RECORD_RULE_VIOLATED   1583
#DEFINE ERROR_UPDATE_CONFLICT        1585
#DEFINE ERROR_MULTI_UPDATE_CONFLICT  1595
#DEFINE ERROR_UNIQUE_INDEX_VIOLATED  1884
#DEFINE ERROR_DE_ALREADY_UNLOADED    1967

#DEFINE KEY_SHIFT                       1
#DEFINE KEY_HOME                        1
#DEFINE KEY_CONTROL                     2
#DEFINE KEY_PAGEDOWN                    3
#DEFINE KEY_PGDOWN                      3
#DEFINE KEY_RIGHT                       4
#DEFINE KEY_UP                          5
#DEFINE KEY_END                         6
#DEFINE KEY_DELETE                      7
#DEFINE KEY_TAB                         9
#DEFINE KEY_F9                         -8
#DEFINE KEY_RETURN                     13
#DEFINE KEY_CARRIAGERETURN             13
#DEFINE KEY_CR                         13
#DEFINE KEY_ENTER                      13
#DEFINE KEY_SHIFTTAB                   15
#DEFINE KEY_PGUP                       18
#DEFINE KEY_PAGEUP                     18
#DEFINE KEY_LEFT                       19
#DEFINE KEY_INSERT                     22
#DEFINE KEY_CTRLEND                    23
#DEFINE KEY_DOWN                       24
#DEFINE KEY_ESCAPE                     27
#DEFINE KEY_CTRLHOME                   29
#DEFINE KEY_SPACE                      32
#DEFINE KEY_EXCLAMATION                33
#DEFINE KEY_DOUBLEQUOTE                34
#DEFINE KEY_POUND                      35
#DEFINE KEY_DOLLAR                     36
#DEFINE KEY_PERCENT                    37
#DEFINE KEY_AMPERSAND                  38
#DEFINE KEY_SINGLEQUOTE                39
#DEFINE KEY_LEFTPAREN                  40
#DEFINE KEY_RIGHTPAREN                 41
#DEFINE KEY_STAR                       42
#DEFINE KEY_ASTERISK                   42
#DEFINE KEY_PLUS                       43
#DEFINE KEY_COMMA                      44
#DEFINE KEY_DASH                       45
#DEFINE KEY_MINUS                      45
#DEFINE KEY_PERIOD                     46
#DEFINE KEY_FORWARDSLASH               47
#DEFINE KEY_SLASH                      47
#DEFINE KEY_0                          48
#DEFINE KEY_ZERO                       48
#DEFINE KEY_SHIFTEND                   49
#DEFINE KEY_ONE                        49
#DEFINE KEY_1                          49
#DEFINE KEY_2                          50
#DEFINE KEY_TWO                        50
#DEFINE KEY_3                          51
#DEFINE KEY_SHIFTPGDOWN                51
#DEFINE KEY_SHIFTPAGEDOWN              51
#DEFINE KEY_THREE                      51
#DEFINE KEY_FOUR                       52
#DEFINE KEY_4                          52
#DEFINE KEY_5                          53
#DEFINE KEY_FIVE                       53
#DEFINE KEY_SIX                        54
#DEFINE KEY_6                          54
#DEFINE KEY_SHIFTHOME                  55
#DEFINE KEY_7                          55
#DEFINE KEY_SEVEN                      55
#DEFINE KEY_8                          56
#DEFINE KEY_EIGHT                      56
#DEFINE KEY_SHIFTPAGEUP                57
#DEFINE KEY_9                          57
#DEFINE KEY_SHIFTPGUP                  57
#DEFINE KEY_NINE                       57
#DEFINE KEY_COLON                      58
#DEFINE KEY_SEMICOLON                  59
#DEFINE KEY_LESSTHAN                   60
#DEFINE KEY_EQUAL                      61
#DEFINE KEY_GREATERTHAN                62
#DEFINE KEY_QUESTION                   63
#DEFINE KEY_AT                         64
#DEFINE KEY_LEFTSQUARE                 91
#DEFINE KEY_BACKSLASH                  92
#DEFINE KEY_RIGHTSQUARE                93
#DEFINE KEY_CARET                      94
#DEFINE KEY_UNDERSCORE                 95
#DEFINE KEY_TICK                       96
#DEFINE KEY_LEFTCURL                  123
#DEFINE KEY_BAR                       124
#DEFINE KEY_RIGHTCURL                 125
#DEFINE KEY_TILDE                     126
#DEFINE KEY_BACKSPACE                 127
#DEFINE KEY_ALTHOME                   151
#DEFINE KEY_ALTEND                    159


* Operating System codes
#DEFINE OS_NT                           2
#DEFINE OS_DOS							5
#DEFINE OS_UNIX							6	

* DLL Paths for various operating systems
#DEFINE DLLPATH_NT					"\SYSTEM32\"

* DLL files used to read INI files
#DEFINE	DLL_KERNEL_NT				"KERNEL32.DLL"

* DLL files used to read registry
#DEFINE	DLL_ADVAPI_NT				"ADVAPI32.DLL"

* DLL files used to read ODBC info
#DEFINE DLL_ODBC_NT					"ODBC32.DLL"

* Registry roots
#DEFINE HKEY_CLASSES_ROOT           -2147483648  && BITSET(0,31)
#DEFINE HKEY_CURRENT_USER           -2147483647  && BITSET(0,31)+1
*#DEFINE HKEY_LOCAL_MACHINE         -2147483646  && BITSET(0,31)+2
#DEFINE HKEY_USERS                  -2147483645  && BITSET(0,31)+3
#DEFINE HKEY_PERFORMANCE_DATA       -2147483644  && BITSET(0,31)+4
#DEFINE HKEY_CURRENT_CONFIG         -2147483643  && BITSET(0,31)+5
#DEFINE HKEY_DYN_DATA               -2147483642  && BITSET(0,31)+6



* Misc
#DEFINE APP_PATH_KEY				"\Shell\Open\Command"
#DEFINE OLE_PATH_KEY				"\Protocol\StdFileEditing\Server"
#DEFINE VFP_OPTIONS_KEY1			"Software\Microsoft\VisualFoxPro\"
#DEFINE VFP_OPTIONS_KEY2			"\Options"
#DEFINE CURVER_KEY					"\CurVer"
#DEFINE ODBC_DATA_KEY				"Software\ODBC\ODBC.INI\"
#DEFINE ODBC_DRVRS_KEY				"Software\ODBC\ODBCINST.INI\"
#DEFINE SQL_FETCH_NEXT				1
#DEFINE SQL_NO_DATA					100

* Error Codes
*#DEFINE ERROR_SUCCESS				0	&& OK
#DEFINE ERROR_EOF 					259 && no more entries in key

* Note these next error codes are specific to this Class, not DLL
#DEFINE ERROR_NOAPIFILE				-101	&& DLL file to check registry not found
#DEFINE ERROR_KEYNOREG				-102	&& key not registered
#DEFINE ERROR_BADPARM				-103	&& bad parameter passed
#DEFINE ERROR_NOENTRY				-104	&& entry not found
#DEFINE	ERROR_BADKEY				-105	&& bad key passed
#DEFINE	ERROR_NONSTR_DATA			-106	&& data type for value is not a data string
#DEFINE ERROR_BADPLAT				-107	&& platform not supported
#DEFINE ERROR_NOINIFILE				-108	&& DLL file to check INI not found
#DEFINE ERROR_NOINIENTRY			-109	&& No entry in INI file
#DEFINE ERROR_FAILINI				-110	&& failed to get INI entry
#DEFINE ERROR_NOPLAT				-111	&& call not supported on this platform
#DEFINE ERROR_NOODBCFILE			-112	&& DLL file to check ODBC not found
#DEFINE ERROR_ODBCFAIL				-113	&& failed to get ODBC environment

* Data types for keys
#DEFINE REG_SZ 							1	&& Data string
#DEFINE REG_EXPAND_SZ 					2	&& Unicode string
#DEFINE REG_BINARY 						3	&& Binary data in any form.
#DEFINE REG_DWORD 						4	&& A 32-bit number.

* Data types labels
#DEFINE REG_BINARY_LOC				"*Binary*"			&& Binary data in any form.
#DEFINE REG_DWORD_LOC 				"*Dword*"			&& A 32-bit number.
#DEFINE REG_UNKNOWN_LOC				"*Unknown type*"	&& unknown type

* FoxPro ODBC drivers
#DEFINE FOXODBC_25					"FoxPro Files (*.dbf)"
#DEFINE FOXODBC_26					"Microsoft FoxPro Driver (*.dbf)"
#DEFINE FOXODBC_30					"Microsoft Visual FoxPro Driver"

* Define path to save data files or dll files
#DEFINE CSIDL_COMMON_APPDATA		0x0023		&&Version 5.0. A typical path is C:\Documents and Settings\All Users\Application Data.
#DEFINE CSIDL_APPDATA				0x001a		&&A typical path is C:\Documents and Settings\username\Application Data
#DEFINE CSIDL_PERSONAL				0x0005		&&A typical path is C:\Documents and Settings\username\My Documents.
#DEFINE CSIDL_PROGRAM_FILES_COMMON	0x002b		&&A typical path is C:\Program Files\Common. Valid only for Windows NT, Windows 2000, and Windows XP systems.
#DEFINE CSIDL_WINDOWS  				0x0024
#DEFINE CSIDL_MYPICTURES  			0x0027

#DEFINE FOLDERID_WINDOWS				CHR(0x04) + CHR(0xf4) + CHR(0x8B) + CHR(0xf3) + CHR(0x43) + CHR(0x1d) + CHR(0xf2) + CHR(0x42) + CHR(0x93) + CHR(0x05) + CHR(0x67) + CHR(0xDE) + CHR(0x0B) + CHR(0x28) + CHR(0xFC) + CHR(0x23)
#DEFINE FOLDERID_PROGRAMDATA			CHR(0x82) + CHR(0x5D) + CHR(0xAB) + CHR(0x62) + CHR(0xC1) + CHR(0xFD) + CHR(0xC3) + CHR(0x4D) + CHR(0xA9) + CHR(0xDD) + CHR(0x07) + CHR(0x0D) + CHR(0x1D) + CHR(0x49) + CHR(0x5D) + CHR(0x97) 
#DEFINE FOLDERID_ROAMINGAPPDATA			CHR(0xDB) + CHR(0x85) + CHR(0xB6) + CHR(0x3E) + CHR(0xF9) + CHR(0x65) + CHR(0xF6) + CHR(0x4C) + CHR(0xA0) + CHR(0x3A) + CHR(0xE3) + CHR(0xEF) + CHR(0x65) + CHR(0x72) + CHR(0x9F) + CHR(0x3D)
#DEFINE FOLDERID_DOCUMENTS				CHR(0xd0) + CHR(0x9a) + CHR(0xd3) + CHR(0xfd) + CHR(0x8f) + CHR(0x23) + CHR(0xaf) + CHR(0x46) + CHR(0xad) + CHR(0xb4) + CHR(0x6c) + CHR(0x85) + CHR(0x48) + CHR(0x03) + CHR(0x69) + CHR(0xc7)
#DEFINE FOLDERID_PICTURES				CHR(0x30) + CHR(0x81) + CHR(0xE2) + CHR(0x33) + CHR(0x1E) + CHR(0x4E) + CHR(0x76) + CHR(0x46) + CHR(0x83) + CHR(0x5A) + CHR(0x98) + CHR(0x39) + CHR(0x5C) + CHR(0x3B) + CHR(0xC3) + CHR(0xBB)
#DEFINE FOLDERID_PROGRAMFILESCOMMONX86	CHR(0x24) + CHR(0x4D) + CHR(0x97) + CHR(0xDE) + CHR(0xC6) + CHR(0xD9) + CHR(0x3E) + CHR(0x4D) + CHR(0xBF) + CHR(0x91) + CHR(0xF4) + CHR(0x45) + CHR(0x51) + CHR(0x20) + CHR(0xB9) + CHR(0x17)

* Virtual-Key Codes (used in RibbonBar)
#DEFINE GWL_WNDPROC					(-4)
#DEFINE WM_GETMINMAXINFO    		0x0024
#DEFINE WM_ACTIVATEAPP      		0x001C
#DEFINE WM_KEYDOWN         			0x0100
#DEFINE WM_KEYUP           			0x0101
#DEFINE WM_SYSKEYDOWN       		0x0104
#DEFINE WM_SYSKEYUP         		0x0105
#DEFINE WM_COMMAND          		0x0111
#DEFINE WM_SYSCOMMAND       		0x0112
#DEFINE WS_EX_LAYERED     			0x00080000
#DEFINE WS_EX_TOOLWINDOW  			0x00000080
#DEFINE WM_MOUSEHOVER     			0x02A1
#DEFINE WM_CHAR                     0x0102
#DEFINE WM_PAINT                    0x000F
#DEFINE VK_TAB 						0x09		&&TAB key
#DEFINE VK_SHIFT 					0x10		&&SHIFT key
#DEFINE VK_CONTROL 					0x11		&&CTRL key
#DEFINE VK_MENU 					0x12		&&ALT key
#DEFINE VK_ESCAPE 					0x1B		&&ESC key
#DEFINE VK_PRIOR 					0x21		&&PAGE UP key
#DEFINE VK_NEXT 					0x22		&&PAGE DOWN key
#DEFINE VK_END 						0x23		&&END key
#DEFINE VK_HOME 					0x24		&&HOME key
#DEFINE VK_LEFT 					0x25		&&LEFT ARROW key
#DEFINE VK_UP 						0x26		&&UP ARROW key
#DEFINE VK_RIGHT 					0x27		&&RIGHT ARROW key
#DEFINE VK_DOWN 					0x28		&&DOWN ARROW key
#DEFINE VK_SNAPSHOT 				0x2C		&&PRINT SCREEN key
#DEFINE VK_INSERT 					0x2D		&&INS key
#DEFINE VK_DELETE 					0x2E		&&DEL key
#DEFINE VK_F1 						0x70		&&F1 key
#DEFINE VK_F2 						0x71		&&F2 key
#DEFINE VK_F3 						0x72		&&F3 key
#DEFINE VK_F4 						0x73		&&F4 key
#DEFINE VK_F5 						0x74		&&F5 key
#DEFINE VK_F6 						0x75		&&F6 key
#DEFINE VK_F7 						0x76		&&F7 key
#DEFINE VK_F8 						0x77		&&F8 key
#DEFINE VK_F9 						0x78		&&F9 key
#DEFINE VK_F10 						0x79		&&F10 key
#DEFINE VK_F11 						0x7A		&&F11 key
#DEFINE VK_F12 						0x7B		&&F12 key

* Query Wizard
#DEFINE C_DEBUG 				.F.
#DEFINE DT_MEMO  				"M"
#DEFINE DT_GENERAL  			"G"
#DEFINE DT_BLOB					"W"
#DEFINE BMP_LOCAL				"dblview.bmp"
#DEFINE BMP_REMOTE				"dbrview.bmp"
#DEFINE BMP_TABLE				"dbtable.bmp"
#DEFINE TAGDELIM	 			" *"

* These are the countries and regions to enable DBCS:  Japan, Korea, PRC, Taiwan
#DEFINE DBCS_LOC "81 82 86 88"

* SetWindowPos
#DEFINE SWP_FRAMECHANGED    0x0020
#DEFINE SWP_NOSIZE          0x0001
#DEFINE SWP_NOMOVE          0x0002
#DEFINE SWP_NOZORDER        0x0004 

* SysTray
* These are the events that are receive from the Taskbar icon.
#DEFINE WM_MOUSEMOVE		0x0200
#DEFINE WM_LBUTTONDOWN		0x0201
#DEFINE WM_LBUTTONUP		0x0202
#DEFINE WM_LBUTTONDBLCLK	0x0203
#DEFINE WM_RBUTTONDOWN		0x0204
#DEFINE WM_RBUTTONUP		0x0205
#DEFINE WM_RBUTTONDBLCLK	0x0206
#DEFINE WM_MBUTTONDOWN		0x0207
#DEFINE WM_MBUTTONUP		0x0208
#DEFINE WM_MBUTTONDBLCLK	0x0209

* Mousewheel events also get passed, but are difficult to decipher.
#DEFINE WM_CONTEXTMENU      0x007B	&& Same as RightClick, but used when 
									&& Version 5 events have been specified.
									&& (See the DisplayBalloonTip method.)

#DEFINE WM_USER             0x0400
#DEFINE NIN_SELECT          WM_USER + 0
#DEFINE NINF_KEY            0x1
#DEFINE NIN_KEYSELECT       BITOR(NIN_SELECT , NINF_KEY)

* Balloon events supported on Windows ME and Windows XP, and later. Not supported on Win2k.
#DEFINE NIN_BALLOONSHOW     (WM_USER + 2)
#DEFINE NIN_BALLOONHIDE     (WM_USER + 3)
#DEFINE NIN_BALLOONTIMEOUT  (WM_USER + 4)
#DEFINE NIN_BALLOONUSERCLICK (WM_USER + 5)

#DEFINE NIM_ADD 			0
#DEFINE NIM_MODIFY 			1
#DEFINE NIM_DELETE 			2
#DEFINE NIF_MESSAGE 		1
#DEFINE NIF_ICON 			2
#DEFINE NIF_TIP 			4
#DEFINE NOTIFYICON_VERSION 	3
#DEFINE NIM_SETVERSION 		4
#DEFINE MIN_SYSTRAY_ICON_ID	0x4000
#DEFINE IMAGE_ICON          1
#DEFINE LR_LOADFROMFILE     0x0010
#DEFINE ABM_GETTASKBARPOS 	0x00000005
#DEFINE GWL_EXSTYLE         -20
#DEFINE WS_EX_TRANSPARENT   0x00000020

*	GDI+ Class library for Visual Foxpro
#DEFINE GDIPLUS_CHECK_PARAMS			.T.	&& Check parameter types
#DEFINE GDIPLUS_CHECK_OBJECT			.T.	&& Check GDI+ object handle
#DEFINE GDIPLUS_CHECK_GDIPLUSNOTINIT	.T.	&& Throw error if GDI+ not initialised

* Classes instantiated from gdiplus.vcx
#define GDIPLUS_CLASS_LIBRARY			This.ClassLibrary
#DEFINE GDIPLUS_CLASS_RECT				'GpRectangle'
#DEFINE GDIPLUS_CLASS_POINT				'GpPoint'
#DEFINE GDIPLUS_CLASS_SIZE				'GpSize'
#DEFINE GDIPLUS_CLASS_FONTFAMILY		'GpFontFamily'
#DEFINE GDIPLUS_CLASS_IMAGE				'GpImage'
#DEFINE GDIPLUS_CLASS_BITMAP			'GpBitmap'
#DEFINE GDIPLUS_CLASS_GRAPHICS			'GpGraphics'

* Control error handler behavior 
*#DEFINE GDIPLUS_ERRHANDLER_ALLOWMODAL		(inlist(_VFP.StartMode,0,4))
*#DEFINE GDIPLUS_ERRHANDLER_QUIET			(not inlist(_VFP.StartMode,0,4))
*#DEFINE GDIPLUS_ERRHANDLER_IGNOREERRORS	.F.
*#DEFINE GDIPLUS_ERRHANDLER_APPNAME			"GDI+ FFC Library"

* Set to .T. to rethrow errors inside error handler (eg when debugging)
#DEFINE GDIPLUS_ERRHANDLER_RETHROW			.F.

* Status enumeration
#DEFINE GDIPLUS_STATUS_OK						0
#DEFINE GDIPLUS_STATUS_GenericError  			1
#DEFINE GDIPLUS_STATUS_InvalidParameter  		2
#DEFINE GDIPLUS_STATUS_OutOfMemory  			3	
#DEFINE GDIPLUS_STATUS_ObjectBusy  				4
#DEFINE GDIPLUS_STATUS_InsufficientBuffer  		5
#DEFINE GDIPLUS_STATUS_NotImplemented  			6
#DEFINE GDIPLUS_STATUS_Win32Error  				7
#DEFINE GDIPLUS_STATUS_WrongState  				8
#DEFINE GDIPLUS_STATUS_Aborted  				9
#DEFINE GDIPLUS_STATUS_FileNotFound  			10
#DEFINE GDIPLUS_STATUS_ValueOverflow  			11
#DEFINE GDIPLUS_STATUS_AccessDenied  			12
#DEFINE GDIPLUS_STATUS_UnknownImageFormat  		13
#DEFINE GDIPLUS_STATUS_FontFamilyNotFound  		14
#DEFINE GDIPLUS_STATUS_FontStyleNotFound  		15
#DEFINE GDIPLUS_STATUS_NotTrueTypeFont  		16
#DEFINE GDIPLUS_STATUS_UnsupportedGdiplusVersion  	17
#DEFINE GDIPLUS_STATUS_GdiplusNotInitialized  		18
#DEFINE GDIPLUS_STATUS_PropertyNotFound  		19
#DEFINE GDIPLUS_STATUS_PropertyNotSupported		20

* Fill mode (how a closed path is filled)
#DEFINE GDIPLUS_FillMode_Alternate				0
#DEFINE GDIPLUS_FillMode_Winding				1

* Quality mode constants
#DEFINE GDIPLUS_QualityMode_Invalid   			-1
#DEFINE GDIPLUS_QualityMode_Default   			0
#DEFINE GDIPLUS_QualityMode_Low       			1	&& Best performance
#DEFINE GDIPLUS_QualityMode_High      			2  && Best rendering quality

* Alpha Compositing mode constants
#DEFINE GDIPLUS_CompositingMode_SourceOver		0
#DEFINE GDIPLUS_CompositingMode_SourceCopy		1

* Alpha Compositing quality constants
#DEFINE GDIPLUS_CompositingQuality_Invalid          GDIPLUS_QualityMode_Invalid
#DEFINE GDIPLUS_CompositingQuality_Default          GDIPLUS_QualityMode_Default
#DEFINE GDIPLUS_CompositingQuality_HighSpeed        GDIPLUS_QualityMode_Low
#DEFINE GDIPLUS_CompositingQuality_HighQuality      GDIPLUS_QualityMode_High
#DEFINE GDIPLUS_CompositingQuality_GammaCorrected	3
#DEFINE GDIPLUS_CompositingQuality_AssumeLinear		4

* Units
#DEFINE GDIPLUS_Unit_World      0 && World coordinate (non-physical unit)
#DEFINE GDIPLUS_Unit_Display    1 && Variable -- for PageTransform only
#DEFINE GDIPLUS_Unit_Pixel      2 && one device pixel.
#DEFINE GDIPLUS_Unit_Point      3 && 1/72 inch.
#DEFINE GDIPLUS_Unit_Inch       4 && 1 inch.
#DEFINE GDIPLUS_Unit_Document   5 && 1/300 inch.
#DEFINE GDIPLUS_Unit_Millimeter 6 && 1 millimeter.

#DEFINE	GDIPLUS_MetafileFrameUnit_Pixel      GDIPLUS_Unit_Pixel
#DEFINE	GDIPLUS_MetafileFrameUnit_Point      GDIPLUS_Unit_Point
#DEFINE	GDIPLUS_MetafileFrameUnit_Inch       GDIPLUS_Unit_Inch
#DEFINE	GDIPLUS_MetafileFrameUnit_Document   GDIPLUS_Unit_Document
#DEFINE	GDIPLUS_MetafileFrameUnit_Millimeter GDIPLUS_Unit_Millimeter
#DEFINE	GDIPLUS_MetafileFrameUnit_Gdi        7	&& GDI compatible .01 MM units

* Coordinate Space
#DEFINE	GDIPLUS_CoordinateSpace_World     	 0
#DEFINE	GDIPLUS_CoordinateSpace_Page      	 1
#DEFINE	GDIPLUS_CoordinateSpace_Device    	 2

* Wrap mode for brushes
#DEFINE	GDIPLUS_WrapMode_Tile				0
#DEFINE	GDIPLUS_WrapMode_TileFlipX			1
#DEFINE	GDIPLUS_WrapMode_TileFlipY			2
#DEFINE	GDIPLUS_WrapMode_TileFlipXY			3
#DEFINE	GDIPLUS_WrapMode_Clamp				4

* HatchBrush styles
#DEFINE	GDIPLUS_HatchStyle_Horizontal		0
#DEFINE	GDIPLUS_HatchStyle_Vertical			1
#DEFINE	GDIPLUS_HatchStyle_ForwardDiagonal	2
#DEFINE	GDIPLUS_HatchStyle_BackwardDiagonal	3
#DEFINE	GDIPLUS_HatchStyle_Cross			4
#DEFINE	GDIPLUS_HatchStyle_DiagonalCross	5
#DEFINE	GDIPLUS_HatchStyle_05Percent		6
#DEFINE	GDIPLUS_HatchStyle_10Percent		7
#DEFINE	GDIPLUS_HatchStyle_20Percent		8
#DEFINE	GDIPLUS_HatchStyle_25Percent		9
#DEFINE	GDIPLUS_HatchStyle_30Percent		10
#DEFINE	GDIPLUS_HatchStyle_40Percent		11
#DEFINE	GDIPLUS_HatchStyle_50Percent		12
#DEFINE GDIPLUS_HatchStyle_60Percent		13
#DEFINE GDIPLUS_HatchStyle_70Percent		14
#DEFINE GDIPLUS_HatchStyle_75Percent		15
#DEFINE GDIPLUS_HatchStyle_80Percent		16
#DEFINE GDIPLUS_HatchStyle_90Percent		17
#DEFINE GDIPLUS_HatchStyle_LightDownwardDiagonal	18
#DEFINE GDIPLUS_HatchStyle_LightUpwardDiagonal		19
#DEFINE GDIPLUS_HatchStyle_DarkDownwardDiagonal		20
#DEFINE GDIPLUS_HatchStyle_DarkUpwardDiagonal		21
#DEFINE GDIPLUS_HatchStyle_WideDownwardDiagonal		22
#DEFINE GDIPLUS_HatchStyle_WideUpwardDiagonal		23
#DEFINE GDIPLUS_HatchStyle_LightVertical			24
#DEFINE GDIPLUS_HatchStyle_LightHorizontal			25
#DEFINE GDIPLUS_HatchStyle_NarrowVertical			26
#DEFINE GDIPLUS_HatchStyle_NarrowHorizontal			27
#DEFINE GDIPLUS_HatchStyle_DarkVertical				28
#DEFINE GDIPLUS_HatchStyle_DarkHorizontal			29
#DEFINE GDIPLUS_HatchStyle_DashedDownwardDiagonal	30
#DEFINE GDIPLUS_HatchStyle_DashedUpwardDiagonal		31
#DEFINE GDIPLUS_HatchStyle_DashedHorizontal			32
#DEFINE GDIPLUS_HatchStyle_DashedVertical			33
#DEFINE GDIPLUS_HatchStyle_SmallConfetti			34
#DEFINE GDIPLUS_HatchStyle_LargeConfetti			35
#DEFINE GDIPLUS_HatchStyle_ZigZag					36
#DEFINE GDIPLUS_HatchStyle_Wave					37
#DEFINE GDIPLUS_HatchStyle_DiagonalBrick		38
#DEFINE GDIPLUS_HatchStyle_HorizontalBrick		39
#DEFINE GDIPLUS_HatchStyle_Weave				40
#DEFINE GDIPLUS_HatchStyle_Plaid				41
#DEFINE GDIPLUS_HatchStyle_Divot				42
#DEFINE GDIPLUS_HatchStyle_DottedGrid			43
#DEFINE GDIPLUS_HatchStyle_DottedDiamond		44
#DEFINE GDIPLUS_HatchStyle_Shingle				45
#DEFINE GDIPLUS_HatchStyle_Trellis				46
#DEFINE GDIPLUS_HatchStyle_Sphere				47
#DEFINE GDIPLUS_HatchStyle_SmallGrid			48
#DEFINE GDIPLUS_HatchStyle_SmallCheckerBoard	49
#DEFINE GDIPLUS_HatchStyle_LargeCheckerBoard	50
#DEFINE GDIPLUS_HatchStyle_OutlinedDiamond		51
#DEFINE GDIPLUS_HatchStyle_SolidDiamond			52

* Dash style constants
#DEFINE GDIPLUS_DashStyle_Solid				0
#DEFINE GDIPLUS_DashStyle_Dash				1
#DEFINE GDIPLUS_DashStyle_Dot				2
#DEFINE GDIPLUS_DashStyle_DashDot			3
#DEFINE GDIPLUS_DashStyle_DashDotDot		4
#DEFINE GDIPLUS_DashStyle_Custom          	5

* Dash cap constants
#DEFINE GDIPLUS_DashCap_Flat             	0
#DEFINE GDIPLUS_DashCap_Round            	2
#DEFINE GDIPLUS_DashCap_Triangle         	3

* LineCap
#DEFINE GDIPLUS_LineCap_Flat            	0
#DEFINE GDIPLUS_LineCap_Square           	1
#DEFINE GDIPLUS_LineCap_Round           	2
#DEFINE GDIPLUS_LineCap_Triangle        	3
#DEFINE GDIPLUS_LineCap_NoAnchor         	0x10 && corresponds to flat cap
#DEFINE GDIPLUS_LineCap_SquareAnchor    	0x11 && corresponds to square cap
#DEFINE GDIPLUS_LineCap_RoundAnchor      	0x12 && corresponds to round cap
#DEFINE GDIPLUS_LineCap_DiamondAnchor    	0x13 && corresponds to triangle cap
#DEFINE GDIPLUS_LineCap_ArrowAnchor      	0x14 && no correspondence
#DEFINE GDIPLUS_LineCap_Custom           	0xff && custom cap
#DEFINE GDIPLUS_LineCap_AnchorMask       	0xf0 && mask to check for anchor or not.

* Custom Line cap type constants
#DEFINE GDIPLUS_CustomLineCapType_Default         	0
#DEFINE GDIPLUS_CustomLineCapType_AdjustableArrow 	1

* Line join constants
#DEFINE GDIPLUS_LineJoin_Miter        		0
#DEFINE GDIPLUS_LineJoin_Bevel        		1
#DEFINE GDIPLUS_LineJoin_Round        		2
#DEFINE GDIPLUS_LineJoin_MiterClipped 		3

* Path point types (only the lowest 8 bits are used.)
* The lowest 3 bits are interpreted as point type
* The higher 5 bits are reserved for flags.
#DEFINE GDIPLUS_PathPointType_Start         0    && move
#DEFINE GDIPLUS_PathPointType_Line          1    && line
#DEFINE GDIPLUS_PathPointType_Bezier        3    && default Bezier (= cubic Bezier)
#DEFINE GDIPLUS_PathPointType_PathTypeMask  0x07 && type mask (lowest 3 bits).
#DEFINE GDIPLUS_PathPointType_DashMode      0x10 && currently in dash mode.
#DEFINE GDIPLUS_PathPointType_PathMarker    0x20 && a marker for the path.
#DEFINE GDIPLUS_PathPointType_CloseSubpath  0x80 && closed flag
#DEFINE GDIPLUS_PathPointType_Bezier3    	3    && cubic Bezier

* WarpMode constants
#DEFINE GDIPLUS_WarpMode_Perspective		0
#DEFINE GDIPLUS_WarpMode_Bilinear    		1

* LinearGradient Mode
#DEFINE GDIPLUS_LinearGradientMode_Horizontal	0
#DEFINE GDIPLUS_LinearGradientMode_Vertical		1
#DEFINE GDIPLUS_LinearGradientMode_ForwardDiagonal	2
#DEFINE GDIPLUS_LinearGradientMode_BackwardDiagonal 3

* CombineMode (for regions)
#DEFINE GDIPLUS_CombineMode_Replace		0
#DEFINE GDIPLUS_CombineMode_Intersect	1
#DEFINE GDIPLUS_CombineMode_Union		2
#DEFINE GDIPLUS_CombineMode_Xor			3
#DEFINE GDIPLUS_CombineMode_Exclude		4
#DEFINE GDIPLUS_CombineMode_Complement  5

* Image types
#DEFINE GDIPLUS_ImageType_Unknown		0
#DEFINE GDIPLUS_ImageType_Bitmap		1
#DEFINE GDIPLUS_ImageType_Metafile  	2

* StringAlignment enumeration
* Applies to GpStringFormat::Alignment, GpStringFormat::LineAlignment
#DEFINE GDIPLUS_STRINGALIGNMENT_Near		0	&& in Left-To-Right locale, this is Left
#DEFINE GDIPLUS_STRINGALIGNMENT_Center		1
#DEFINE GDIPLUS_STRINGALIGNMENT_Far			2	&& in Left-To-Right locale, this is Right

* StringFormatFlags enumeration
#DEFINE GDIPLUS_STRINGFORMATFLAGS_DirectionRightToLeft	1 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_DirectionVertical 	2 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_NoFitBlackBox 		4 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_DisplayFormatControl 	32 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_NoFontFallback 		1024 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_MeasureTrailingSpaces 2048 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_NoWrap 				4096 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_LineLimit 			8192 
#DEFINE GDIPLUS_STRINGFORMATFLAGS_NoClip 				16384 

* StringTrimming enumeration
#DEFINE GDIPLUS_STRINGTRIMMING_None 				0	&& no trimming. 
#DEFINE GDIPLUS_STRINGTRIMMING_Character 			1	&& nearest character. 
#DEFINE GDIPLUS_STRINGTRIMMING_Word					2	&& nearest wor 
#DEFINE GDIPLUS_STRINGTRIMMING_EllipsisCharacter 	3	&& nearest character, ellipsis at end
#DEFINE GDIPLUS_STRINGTRIMMING_EllipsisWord 		4	&& nearest word, ellipsis at end
#DEFINE GDIPLUS_STRINGTRIMMING_EllipsisPath 		5	&& ellipsis in center, favouring last slash-delimited segment
* StringDigitSubstitute
#DEFINE GDIPLUS_STRINGDIGITSUBSTITUTE_User 			0
#DEFINE GDIPLUS_STRINGDIGITSUBSTITUTE_None 			1
#DEFINE GDIPLUS_STRINGDIGITSUBSTITUTE_National		2
#DEFINE GDIPLUS_STRINGDIGITSUBSTITUTE_Traditional 	3

* HotkeyPrefix enumeration
#DEFINE GDIPLUS_HOTKEYPREFIX_None 		0	&& No hot-key prefix. 
#DEFINE GDIPLUS_HOTKEYPREFIX_Show 		1	&& display hot-key prefix
#DEFINE GDIPLUS_HOTKEYPREFIX_Hide 		2	&& Do not display the hot-key prefix. 

* FontStyle: face types and common styles
#DEFINE GDIPLUS_FontStyle_Regular     	0
#DEFINE GDIPLUS_FontStyle_Bold        	1
#DEFINE GDIPLUS_FontStyle_Italic      	2
#DEFINE GDIPLUS_FontStyle_BoldItalic  	3
#DEFINE GDIPLUS_FontStyle_Underline   	4
#DEFINE GDIPLUS_FontStyle_Strikeout   	8

#DEFINE GDIPLUS_InterpolationMode_Invalid          		GDIPLUS_QualityMode_Invalid
#DEFINE GDIPLUS_InterpolationMode_Default          		GDIPLUS_QualityMode_Default
#DEFINE GDIPLUS_InterpolationMode_LowQuality       		GDIPLUS_QualityMode_Low
#DEFINE GDIPLUS_InterpolationMode_HighQuality      		GDIPLUS_QualityMode_High
#DEFINE GDIPLUS_InterpolationMode_Bilinear				3
#DEFINE GDIPLUS_InterpolationMode_Bicubic				4
#DEFINE GDIPLUS_InterpolationMode_NearestNeighbor		5
#DEFINE GDIPLUS_InterpolationMode_HighQualityBilinear	6
#DEFINE GDIPLUS_InterpolationMode_HighQualityBicubic	7

#DEFINE GDIPLUS_PenAlignment_Center       	0
#DEFINE GDIPLUS_PenAlignment_Inset        	1

* Brush types
#DEFINE GDIPLUS_BrushType_SolidColor       	0
#DEFINE GDIPLUS_BrushType_HatchFill        	1
#DEFINE GDIPLUS_BrushType_TextureFill      	2
#DEFINE GDIPLUS_BrushType_PathGradient     	3
#DEFINE GDIPLUS_BrushType_LinearGradient   	4

* Pen's Fill types
#DEFINE GDIPLUS_PenType_SolidColor       	GDIPLUS_BrushType_SolidColor
#DEFINE GDIPLUS_PenType_HatchFill        	GDIPLUS_BrushType_HatchFill
#DEFINE GDIPLUS_PenType_TextureFill      	GDIPLUS_BrushType_TextureFill
#DEFINE GDIPLUS_PenType_PathGradient    	GDIPLUS_BrushType_PathGradient
#DEFINE GDIPLUS_PenType_LinearGradient   	GDIPLUS_BrushType_LinearGradient
#DEFINE GDIPLUS_PenType_Unknown         	-1

* Matrix Order
#DEFINE GDIPLUS_MatrixOrder_Prepend    		0
#DEFINE GDIPLUS_MatrixOrder_Append     		1

* SmoothingMode
#DEFINE GDIPLUS_SmoothingMode_Invalid     	GDIPLUS_QualityMode_Invalid
#DEFINE GDIPLUS_SmoothingMode_Default     	GDIPLUS_QualityMode_Default
#DEFINE GDIPLUS_SmoothingMode_HighSpeed   	GDIPLUS_QualityMode_Low,
#DEFINE GDIPLUS_SmoothingMode_HighQuality 	GDIPLUS_QualityMode_High
#DEFINE GDIPLUS_SmoothingMode_None			3
#DEFINE GDIPLUS_SmoothingMode_AntiAlias		4

* PixelOffsetMode
#DEFINE GDIPLUS_PixelOffsetMode_Invalid		GDIPLUS_QualityMode_Invalid
#DEFINE GDIPLUS_PixelOffsetMode_Default		GDIPLUS_QualityMode_Default
#DEFINE GDIPLUS_PixelOffsetMode_HighSpeed	GDIPLUS_QualityMode_Low
#DEFINE GDIPLUS_PixelOffsetMode_HighQuality	GDIPLUS_QualityMode_High
#DEFINE GDIPLUS_PixelOffsetMode_None		3
#DEFINE GDIPLUS_PixelOffsetMode_Half		4

* GpGraphics::Flush() modes
#DEFINE GDIPLUS_FlushIntention_Flush		0
#DEFINE GDIPLUS_FlushIntention_Sync			1

* Image file format identifiers (GUIDs)
#DEFINE GDIPLUS_IMAGEFORMAT_Undefined		0hA93C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_MemoryBMP		0hAA3C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_BMP				0hAB3C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_EMF				0hAC3C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_WMF				0hAD3C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_JPEG			0hAE3C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_PNG				0hAF3C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_GIF				0hB03C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_TIFF			0hB13C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_EXIF			0hB23C6BB92807D3119D7B0000F81EF32E
#DEFINE GDIPLUS_IMAGEFORMAT_Icon			0hB53C6BB92807D3119D7B0000F81EF32E

* Pixel formats
#DEFINE GDIPLUS_PIXELFORMAT_Indexed     	0x00010000 && Indexes into a palette
#DEFINE GDIPLUS_PIXELFORMAT_GDI         	0x00020000 && Is a GDI-supported format
#DEFINE GDIPLUS_PIXELFORMAT_Alpha       	0x00040000 && Has an alpha component
#DEFINE GDIPLUS_PIXELFORMAT_PAlpha       	0x00080000 && Pre-multiplied alpha
#DEFINE GDIPLUS_PIXELFORMAT_Extended     	0x00100000 && Extended color 16 bits/channel
#DEFINE GDIPLUS_PIXELFORMAT_Canonical    	0x00200000 
#DEFINE GDIPLUS_PIXELFORMAT_Undefined    	0
#DEFINE GDIPLUS_PIXELFORMAT_DontCare     	0

#DEFINE	GDIPLUS_PIXELFORMAT_1bppIndexed     	0x00030101
#DEFINE	GDIPLUS_PIXELFORMAT_4bppIndexed     	0x00030402
#DEFINE	GDIPLUS_PIXELFORMAT_8bppIndexed     	0x00030803
#DEFINE	GDIPLUS_PIXELFORMAT_16bppGrayScale  	0x00101004
#DEFINE	GDIPLUS_PIXELFORMAT_16bppRGB555     	0x00021005
#DEFINE	GDIPLUS_PIXELFORMAT_16bppRGB565     	0x00021006
#DEFINE	GDIPLUS_PIXELFORMAT_16bppARGB1555   	0x00061007
#DEFINE	GDIPLUS_PIXELFORMAT_24bppRGB        	0x00021808
#DEFINE	GDIPLUS_PIXELFORMAT_32bppRGB        	0x00022009
#DEFINE	GDIPLUS_PIXELFORMAT_32bppARGB       	0x0026200A
#DEFINE	GDIPLUS_PIXELFORMAT_32bppPARGB      	0x000E200B
#DEFINE	GDIPLUS_PIXELFORMAT_48bppRGB        	0x0010300C
#DEFINE	GDIPLUS_PIXELFORMAT_64bppPARGB      	0x001C400E

* Image flags (see GpImage::Flags property)
#DEFINE GDIPLUS_ImageFlags_None					0
#DEFINE GDIPLUS_ImageFlags_Scalable				0x0001
#DEFINE GDIPLUS_ImageFlags_HasAlpha				0x0002
#DEFINE GDIPLUS_ImageFlags_HasTranslucent		0x0004
#DEFINE GDIPLUS_ImageFlags_PartiallyScalable	0x0008
#DEFINE GDIPLUS_ImageFlags_ColorSpaceRGB		0x0010
#DEFINE GDIPLUS_ImageFlags_ColorSpaceCMYK		0x0020
#DEFINE GDIPLUS_ImageFlags_ColorSpaceGRAY		0x0040
#DEFINE GDIPLUS_ImageFlags_ColorSpaceYCBCR		0x0080
#DEFINE GDIPLUS_ImageFlags_ColorSpaceYCCK		0x0100
#DEFINE GDIPLUS_ImageFlags_HasRealDPI			0x1000
#DEFINE GDIPLUS_ImageFlags_HasRealPixelSize		0x2000
#DEFINE GDIPLUS_ImageFlags_ReadOnly				0x00010000
#DEFINE GDIPLUS_ImageFlags_Caching				0x00020000

* Encoder parameter type
#define GDIPLUS_ValueDataType_Byte				1	&& 8-bit unsigned
#define GDIPLUS_ValueDataType_ASCII				2	&& character string
#define GDIPLUS_ValueDataType_Short				3	&& 16-bit unsigned
#define GDIPLUS_ValueDataType_Long				4	&& 32-bit unsigned
#define GDIPLUS_ValueDataType_Rational			5	&& fraction ulong/ulong
#define GDIPLUS_ValueDataType_LongRange			6	&& Two ulongs (min,max)
#define GDIPLUS_ValueDataType_Undefined			7	&& array of bytes
#define GDIPLUS_ValueDataType_RationalRange		8	&& four ulongs
#define GDIPLUS_ValueDataType_Pointer			9	&& pointer

#define GDIPLUS_ENCODER_Compression				0h9D739DE0D4CCEE448EBA3FBF8BE4FC58
#define GDIPLUS_ENCODER_ColorDepth				0h5570086666AD7C4C9A1838A2310B8337
#define GDIPLUS_ENCODER_ScanMethod				0h61264E3A0931564E853642C156E7DCFA
#define GDIPLUS_ENCODER_Version					0h768CD1244A81A441BF531C219CCCF797
#define GDIPLUS_ENCODER_RenderMethod			0h3AC5426D9A2225488BB75C99E2B9A8B8
#define GDIPLUS_ENCODER_Quality					0hB5E45B1D4AFA2D459CDD5DB35105E7EB
#define GDIPLUS_ENCODER_Transformation			0hD1B20E8D8EA5A84EAA14108074B7B6F9
#define GDIPLUS_ENCODER_LuminanceTable			0hCE3BB3ED6602774AB90427216099E717
#define GDIPLUS_ENCODER_ChrominanceTable		0hDC55E4F2B30916438260676ADA32481C
#define GDIPLUS_ENCODER_SaveFlag				0hFC66222940ACBF478CFCA85B89A655DE

* GpImage::RotateFlip() parameter
#define GDIPLUS_ROTATEFLIPTYPE_RotateNoneFlipNone 0
#define GDIPLUS_ROTATEFLIPTYPE_Rotate90FlipNone   1
#define GDIPLUS_ROTATEFLIPTYPE_Rotate180FlipNone  2
#define GDIPLUS_ROTATEFLIPTYPE_Rotate270FlipNone  3

#define GDIPLUS_ROTATEFLIPTYPE_RotateNoneFlipX    4
#define GDIPLUS_ROTATEFLIPTYPE_Rotate90FlipX      5
#define GDIPLUS_ROTATEFLIPTYPE_Rotate180FlipX     6
#define GDIPLUS_ROTATEFLIPTYPE_Rotate270FlipX     7

#define GDIPLUS_ROTATEFLIPTYPE_RotateNoneFlipY    GDIPLUS_ROTATEFLIPTYPE_Rotate180FlipX
#define GDIPLUS_ROTATEFLIPTYPE_Rotate90FlipY      GDIPLUS_ROTATEFLIPTYPE_Rotate270FlipX
#define GDIPLUS_ROTATEFLIPTYPE_Rotate180FlipY     GDIPLUS_ROTATEFLIPTYPE_RotateNoneFlipX
#define GDIPLUS_ROTATEFLIPTYPE_Rotate270FlipY     GDIPLUS_ROTATEFLIPTYPE_Rotate90FlipX

#define GDIPLUS_ROTATEFLIPTYPE_RotateNoneFlipXY   GDIPLUS_ROTATEFLIPTYPE_Rotate180FlipNone
#define GDIPLUS_ROTATEFLIPTYPE_Rotate90FlipXY     GDIPLUS_ROTATEFLIPTYPE_Rotate270FlipNone
#define GDIPLUS_ROTATEFLIPTYPE_Rotate180FlipXY    GDIPLUS_ROTATEFLIPTYPE_RotateNoneFlipNone
#define GDIPLUS_ROTATEFLIPTYPE_Rotate270FlipXY    GDIPLUS_ROTATEFLIPTYPE_Rotate90FlipNone

* cEmail.SendViaExMapi()
#DEFINE MAPI_ORIG 	0
#DEFINE MAPI_TO		1
#DEFINE MAPI_CC		2
#DEFINE MAPI_BCC	3

#DEFINE IMPORTANCE_LOW 		0
#DEFINE IMPORTANCE_NORMAL	1
#DEFINE IMPORTANCE_HIGH 	2