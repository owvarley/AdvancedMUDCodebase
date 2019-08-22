//
// MudCore
//
// MudCore is copyright (c) 2000, 2001 by Gary McNickle
// <gary#mcnickle.org>
//
// MudCore is free software; you can redistribute it and/or modify
// it under the terms of the MudCore license contained in the
// included file "license.txt".
//
// You should have received a copy of the MudCore license along with
// MudCore, if not, write to the author and request a copy.
//
// Gary McNickle
// <gary#mcnickle.org>
// 5408 E. 10th St
// Indianapolis, IN 46219 USA
//


#ifndef __MUDCORE_H__
#define __MUDCORE_H__

#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include "gString.h"

class CCommand;
class CEmotions;

/////////////////////////////////////////////////////////////////////////////////////
// Windows Specific Defines /////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
#if defined(WIN32) || defined(_WINDOWS)

	#ifndef _WINDOWS
		#define _WINDOWS
	#endif

	#ifndef WINSOCK_VERSION
		#include <winsock2.h>
		#include <windows.h>
		#include <fstream.h>
	#endif

	#if defined(_MSC_VER)
		#pragma warning(disable: 4786)
		#pragma warning(disable: 4273)	// '' : inconsistent dll linkage.  dllexport assumed.
		#pragma warning(disable: 4251)  // '' : class '' needs to have dll-interface to be used by clients of class ''
		#pragma warning(disable: 4049)  // LINK locally defined symbol '' imported
	#endif

	#ifndef _export
		#ifdef MUDCORE_EXPORTS
			#define _export __declspec(dllexport)
		#else
			#define _export __declspec(dllimport)
		#endif
	#endif

#endif


/////////////////////////////////////////////////////////////////////////////////////
// Other OS Defines /////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
#if defined(linux)
	#ifndef _export
		#define _export extern
	#endif
#endif


/////////////////////////////////////////////////////////////////////////////////////
// Commonly Used, OS Independent Defines ////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
#define	APPNAME					"AMC"
#define	VERSION					00631
#define	MIN						0
#define	MAX						1
#define	CURRENT					0
#define	PORT					4000

#define HAVE_PYTHON				0
#define HAVE_TCL				0

#ifdef MIL
	#undef MIL
#endif

#ifdef MSL
	#undef MSL
#endif

#define MAX_INPUT_LENGTH		1024
#define MIL						MAX_INPUT_LENGTH

#define MAX_STRING_LENGTH		MIL * 4
#define MSL						MAX_STRING_LENGTH

#define MAX_WIDTH				79

#define QUERRY_CODE				"\x01B[c"
#define RESPONSE_CODE			"\x01B["
#define ANSI_NOTICE				"\n\r#701#100A#200N#300S#401I#700 Color detected!\n\r\n\r"


/////////////////////////////////////////////////////////////////////////////////////
// Forward Class Definitions ////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
class CUser;

/////////////////////////////////////////////////////////////////////////////////////
// Portable Types ///////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
typedef unsigned int			UINT;
typedef unsigned char			UCHAR;
typedef unsigned char			BYTE;
typedef short int				SHORT;

/////////////////////////////////////////////////////////////////////////////////////
// Update Deltas ////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////
// This number, gives us the amount of seconds between each update cycle
// for this atomic type.
///////////////////////////////////////////////////////////////////////////
extern float	fActorUpdateDelta;
extern float	fItemUpdateDelta;
extern float	fPlayerUpdateDelta;
extern float	fNpcUpdateDelta;
extern float	fRoomUpdateDelta;
extern float	fAreaUpdateDelta;
extern float	fSpaceUpdateDelta;
extern float	fCrewUpdateDelta;

/////////////////////////////////////////////////////////////////////////////////////
// Function Declarations ////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
void TRACE(char* fmt, ...);
void AppCallback(void);

/////////////////////////////////////////////////////////////////////////////////////
// Function Type Definitions ////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
typedef int  t_MenuFn(CUser *ch, const gString& gsCommandLine);
typedef bool t_ExitFn(CUser *ch, int nExitCode);
typedef CCommand* t_CmdFn(void);
typedef CEmotions* t_EmoFn(void);

/////////////////////////////////////////////////////////////////////////////////////
// Helper Macros ////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////
#define __HASH(h, l)           ( ((h) << 16) | ((l) << 8) )
#define GET_HI(hash)           ( ((hash) >> 16) & 0xff )
#define GET_LO(hash)           ( ((hash) >> 8) & 0xff )



/////////////////////////////////////////////////////////////////////////////////////
// Globally Available Types /////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////

// Event Ranges
enum e_EventRanges
{
	EVRANGE_FIRST_BASE			= 0,
	EVRANGE_LAST_BASE			= 99,
	EVRANGE_FIRST_ATOMIC		= 100,
	EVRANGE_LAST_ATOMIC			= 999,
	EVRANGE_FIRST_ACTOR			= 1000,
	EVRANGE_LAST_ACTOR			= 1999,
	EVRANGE_FIRST_PLAYER		= 2000,
	EVRANGE_LAST_PLAYER			= 2999,
	EVRANGE_FIRST_NPC			= 3000,
	EVRANGE_LAST_NPC			= 3999,
	EVRANGE_FIRST_ITEM			= 4000,
	EVRANGE_LAST_ITEM			= 4999,
	EVRANGE_FIRST_AREA			= 5000,
	EVRANGE_LAST_AREA			= 5999,
	EVRANGE_FIRST_ROOM			= 6000,
	EVRANGE_LAST_ROOM			= 6999,
	EVRANGE_FIRST_SHIP			= 7000,
	EVRANGE_LAST_SHIP			= 7999,
	EVRANGE_FIRST_MOBILE		= 8000,
	EVRANGE_LAST_MOBILE			= 8999
};

// IMPLEMENT_COMMAND() kept for backwards compatibility. Safe
// to replace it with IMPLEMENT_CLASS
#define IMPLEMENT_COMMAND(command) \
	extern "C" __declspec(dllexport) command* Create_##command() \
{ return new command; }\

#define IMPLEMENT_CLASS(NewClass) \
	extern "C" __declspec(dllexport) NewClass* Create_##NewClass() \
{ return new NewClass; }\

#endif


