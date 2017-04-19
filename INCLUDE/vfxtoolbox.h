#define TOOLBOX_LOC						"Toolbox"


* used by ToolBox.ShowType field
#define SHOWTYPE_CATEGORY	'C'
#define SHOWTYPE_TOOL		'T'
#define SHOWTYPE_FAVORITES	'F'
#define SHOWTYPE_FILTER		'S'
#define SHOWTYPE_FILTERITEM	'I'

* add-ins that shows on the menu
#define SHOWTYPE_ADDIN		'A'
#define SHOWTYPE_ADDINMENU	'M'  && menu option only

* -- Default classes and class library

* this is the vcx that should be found in HOME() + "Toolbox"
#define DEFAULT_CLASSLIB		"vfxtoolbox.vcx"

* if we don't find the above, we maintain a copy internal to the APP
#define INTERNAL_CLASSLIB		"vfxtoolbox.vcx"

#define FILTERCLASS_ITEM		"_filteritem"

#define CATEGORYCLASS_GENERAL	"_generalcategory"
#define CATEGORYCLASS_FAVORITES	"_favoritescategory"

#define ITEMCLASS_ROOT			"_root"
#define ITEMCLASS_TOOL			"_tool"

#define SCROLLSPEED_DEFAULT		30
#define FONT_DEFAULT			"Tahoma,8,N"

#define tvwFirst	0
#define tvwLast		1
#define tvwNext		2
#define tvwPrevious	3
#define tvwChild	4

#define WIN_SCX_DESIGN_LOC			"FORM DESIGNER -"
#define WIN_VCX_DESIGN_LOC			"CLASS DESIGNER -"

#define HKEY_CLASSES_ROOT			-2147483648  && BITSET(0,31)
#define HKEY_CURRENT_USER			-2147483647  && BITSET(0,31)+1

#define TOOLBOX_HELPID				1231116

* The following are invalid in object name so we strip them out if we find them in a filename
#define INVALID_OBJNAME_CHARS	" -!@#$%^&*()+={}[]:;?/<>,\|~`'" + ["]