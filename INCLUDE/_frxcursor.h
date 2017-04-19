*=======================================================
* frxCursor.vcx compile constants
*=======================================================

*-------------------------------------------------------
* Magic numbers
*-------------------------------------------------------

*-- Report Layout object dimensions
*--
#define BAND_SEPARATOR_HEIGHT_FRUS	 	2083.333
#define BAND_SEPARATOR_HEIGHT_PIXELS	20

*-- Object Cursor filter modes:
*--
#define OBJCSR_ALL_OBJECTS_IGNORE_GROUPS	0
#define OBJCSR_FILTER_ON_SELECTED			1
#define OBJCSR_SHOW_ALL_OBJECTS				2
#define OBJCSR_FILTER_GROUP					3

#define OBJCSR_SORTORDER_TYPE		        1
#define OBJCSR_SORTORDER_BAND		        2

#define FRX_OBJTYPE_MULTISELECT             99

#define HORZSIZE          4  && Horizontal size in millimeters
#define VERTSIZE          6  && Vertical size in millimeters

#define HORZRES           8  && Printable page width  / Horizontal width in pixels
#define VERTRES          10  && Printable page length / Vertical height in pixels

#define LOGPIXELSX       88  && DPI / Logical pixels/inch in X dimension
#define LOGPIXELSY       90  && DPI / Logical pixels/inch in Y dimension

#define PHYSICALWIDTH   110  && Actual page width  / Physical Width in device units
#define PHYSICALHEIGHT  111  && Actual page length / Physical Height in device units

#define PHYSICALOFFSETX 112  && Printable page left margin / Physical Printable Area x margin
#define PHYSICALOFFSETY 113  && Printable page top margin  / Physical Printable Area y margin

*=======================================================
* SP2 Constants & localization strings:
*=======================================================

*-- FRX object targets:
*--
#define TARGET_LAYOUT_ELEMENTS_LOC  "Any Report control or layout element"

*-----------------------------------------------------
* Moved from frxBuilder.h
*-----------------------------------------------------
#define INT_REGISTRY_TABLE		     "frxbuilder.dbf"
#define EXT_REGISTRY_TABLE		     "reportbuilder.dbf"
#define CONFIG_FILE_REGISTRY_TOKEN   "reportbuilder_registry"

#define MAX_ATTRIBS_IN_MEMBER_XML	254

*#define DEFAULT_MEMBERDATA_XML		[<?xml version = "1.0" encoding="Windows-1252" standalone="yes"?><VFPData><reportdata name="" type="R" script="" execute="" execwhen="" class="" classlib="" declass="" declasslib=""/></VFPData>]

#define DEFAULT_MEMBERDATA_XML		[<VFPData><reportdata name="" type="R" script="" execute="" execwhen="" class="" classlib="" declass="" declasslib=""/></VFPData>]

#define CURSOR_COLUMN_LIMIT			255
