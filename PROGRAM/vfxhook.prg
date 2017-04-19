******************************************************************************
* Project...........: Visual Extend 16.0
*                     Copyright (c) dFPUG c/o ISYS GmbH
*                     All Rights Reserved.
*
* Program...........: VFXHOOK.PRG
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

FUNCTION eventhookhandler(tcevent, toobject, toform)
LOCAL lcontinue

lcontinue = .T.

RETURN lcontinue
ENDFUNC