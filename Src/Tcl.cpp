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

//
// TCL Script Handler
//
// This script handler was written as an example of how to embed a scripting language
// into Mudcore.  For it to compile and work properly, you must have TCL 8.4 or better
// (previous versions untested) installed on your system. You must set the TCL 
// include and lib directories in your make paths (in VC, tools|options|directories)
// and you must set the value of HAVE_TCL to 1.  You should also set the tcl\bin 
// directory in your path.
//


#include "MudCore.h"
#include "Tools.h"
#include "Parser.h"

#if (HAVE_TCL)	

#include <float.h>
#include <limits.h>


#include "Actor.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "GameWorld.h"

#include <tcl.h>



/////////////////////////////////////////////////////////////////////////////////////////
// CTcl Script Handler ///////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
CTcl::CTcl()
{
	m_bInitialized = false;
	m_pInterpreter = NULL;
}

CTcl::~CTcl()
{
	if (m_bInitialized)
		Shutdown();
}

void CTcl::Initialize()
{
	if ( m_bInitialized )
		return;

	// Initialize Tcl
	try
	{
		Tcl_FindExecutable ( "mudcore.exe" );
		m_pInterpreter = Tcl_CreateInterp();

		if ( m_pInterpreter )
		{
		    if (Tcl_Init(m_pInterpreter) == TCL_ERROR)
				throw "Unable to initialize TCL";
		}
	}
	catch (...)
	{
		Tcl_DeleteInterp(m_pInterpreter);
		m_pInterpreter = NULL;

		g_Log.Log(LOG_ERROR, "[CTcl::Initialize] Failed initialization.");
		return;
	}

	m_bInitialized = true;
	g_Log.Log(LOG_INFO, "[CTcl::Initialize] TCL Successfully embedded.");
}

void CTcl::Shutdown()
{
	if ( !m_bInitialized )
		return;

	Tcl_Finalize();

	Tcl_DeleteInterp(m_pInterpreter);
	m_pInterpreter = NULL;

	m_bInitialized = false;
}


bool CTcl::IsInitialized()
{
	return m_bInitialized;
}

bool CTcl::ExecuteScript(const gString& szScript)
{
	if ( !IsInitialized() )
		return false;

	try
	{
		return ( Tcl_Eval(m_pInterpreter, szScript) == TCL_OK );
	}
	catch (...)
	{
		return false;
	}

	return true;
}

bool CTcl::ExecuteScript(const gFileName& fnScript)
{
	if ( !IsInitialized() )
		return false;

	try
	{
		return ( Tcl_EvalFile(m_pInterpreter, fnScript) == TCL_OK );
	}
	catch (...)
	{
		return false;
	}

	return true;
}

#else
#pragma message(Reminder "TCL not enabled. CTcl class will not be available.")

#endif // (HAVE_TCL)
