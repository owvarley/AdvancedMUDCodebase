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
// Python Script Handler
//
// This script handler was written as an example of how to embed a scripting language
// into Mudcore.  For it to compile and work properly, you must have Python 2.2 or better
// (previous versions untested) installed on your system. You must set the python
// include and lib directories in your make paths (in VC, tools|options|directories)
// and you must set the value of HAVE_PYTHON to 1.
//
// I've also included several functions that extend python specifically for MudCore.
// They should provide suffecient examples for how to write your own extension functions.
// For more information on the Python language, see www.python.org
//


#include "MudCore.h"
#include "Tools.h"
#include "Parser.h"

#if (HAVE_PYTHON)	

#include <float.h>
#include <limits.h>

#include "Actor.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "GameWorld.h"

//
// Python 2.2 will try to link to a debug buld of the python libraries if _DEBUG is
// set, but it does not ship with those libraries. So, to avoid having to compile
// Python, we remove the _DEBUG define temporarily.
//
#ifdef _DEBUG
	#undef _DEBUG
		#include <Python.h>
	#define _DEBUG
#else
	#include <Python.h>
#endif


PyObject* mcore_GetString(PyObject* self, PyObject* args);
PyObject* mcore_SetString(PyObject* self, PyObject* args);
PyObject* mcore_GetInt(PyObject* self, PyObject* args);
PyObject* mcore_SetInt(PyObject* self, PyObject* args);
PyObject* mcore_GetFloat(PyObject* self, PyObject* args);
PyObject* mcore_SetFloat(PyObject* self, PyObject* args);
PyObject* mcore_ExecuteActorCommand(PyObject* self, PyObject* args);
PyObject* mcore_LocateActor(PyObject* self, PyObject* args);
PyObject* mcore_SendToPlayer(PyObject* self, PyObject* args);


//
// Each of our extension functions must be registered with Python before it
// can call into them.
//
static PyMethodDef mcore_Methods[] = 
{
    {"LocateActor", mcore_LocateActor,			METH_VARARGS, "Locate a MudCore actor."},
	{"GetString",   mcore_GetString,			METH_VARARGS, "Get a string property from an actor."},
	{"SetString",	mcore_SetString,			METH_VARARGS, "Set a string property on an actor."},
	{"GetInt",		mcore_GetInt,				METH_VARARGS, "Get an integer property from an actor."},
	{"SetInt",      mcore_SetInt,				METH_VARARGS, "Set an integer property on an actor."},
	{"GetFloat",	mcore_GetFloat,				METH_VARARGS, "Get a float property from an actor."},
	{"SetFloat",	mcore_SetFloat,				METH_VARARGS, "Set a float property on an actor."},
	{"WriteTo",		mcore_SendToPlayer,			METH_VARARGS, "Send text to a player."},
	{"DoCmd",		mcore_ExecuteActorCommand,	METH_VARARGS, "Force an actor to execute a command."},
    {NULL, NULL, 0, NULL}
};


/////////////////////////////////////////////////////////////////////////////////////////
// CPython Script Handler ///////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
CPython::CPython()
{
	m_bInitialized = false;

	try
	{
		if ( Py_IsInitialized() )
			m_bInitialized = true;
	}
	catch (...)
	{
	}
}

CPython::~CPython()
{
	if (m_bInitialized)
		Shutdown();
}

void CPython::Initialize()
{
	if ( m_bInitialized )
		return;

	// Initialize Python
	try
	{
		Py_SetProgramName(APPNAME);

		Py_Initialize();
		Py_InitModule("mcore", mcore_Methods);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CPython::Initialize] Failed initialization.");
		return;
	}

	m_bInitialized = true;
	g_Log.Log(LOG_INFO, "[CPyton::Initialize] Python Successfully embedded.");
}

void CPython::Shutdown()
{
	if ( !m_bInitialized )
		return;

	// Shutdown Python
	Py_Finalize();
}


bool CPython::IsInitialized()
{
	return m_bInitialized;
}

bool CPython::ExecuteScript(const gString& szScript)
{
	if ( !IsInitialized() )
		return false;

	try
	{
		PyRun_SimpleString(szScript);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CPython::ExecuteScript] An unhandled exception occured while running a Python script.");
		return false;
	}

	return true;
}

bool CPython::ExecuteScript(const gFileName& fnScript)
{
	gString gsScript;
	std::fstream fp;

	fp.open(fnScript, ios::in|ios::nocreate);

	// Ok, the Python function PyRun_SimpleFile() does not seem to work, or 
	// work properly. I cant get it to not throw an exception. I've read some
	// information on this where mention is made of having to actually compile Python.
	// That seems ... excessive. Anyway, since I dont want to answer the inevitable
	// slew of "Why cant I get Python to compile?" questions, this works just as well.
	if ( fp.is_open() )
	{
		while ( !fp.eof() )
		{
			gsScript += CTools::Get().ReadLn(fp);
			gsScript += "\n";
		}

		fp.close();

		return ExecuteScript(gsScript);
	}
	else
		g_Log.Log(LOG_ERROR, "[CPython::ExecuteScript] Unable to open file \'%s\'.", fnScript);

	return false;
}



/////////////////////////////////////////////////////////////////////////////////////////
// Python Extension Functions ///////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////

// Given an actor object (passed in as a tuple from a python script),
// locate the requested property and return it's string value
//
// Syntax: mcore.GetString(Actor, Property Name)
//
// Note: For this to work, the property must be "Registered" with the
// PropertyMgr for this actor.
PyObject* mcore_GetString(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szName = NULL;
	gString gsValue;
	long nID = -1;

	try
	{
		if(!PyArg_ParseTuple(args, "lz", &nID, &szName))
			return Py_BuildValue("");

		if ( nID == -1 || !szName || strlen(szName) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);

		if ( pActor )
		{
			pActor->Get(szName, gsValue);
			return Py_BuildValue("z", gsValue);
		}
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.GetString] Failed.");
	}

	return Py_BuildValue("");
}


// Syntax: mcore.SetString(Actor, Property Name, New Property Value)
//
// Note: For this to work, the property must be "Registered" with the
// PropertyMgr for this actor.
PyObject* mcore_SetString(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szName = NULL;
	char* szValue = NULL;
	long nID = -1;

	try
	{
		if(!PyArg_ParseTuple(args, "lzz", &nID, &szName, &szValue))
			return Py_BuildValue("");

		if ( nID == -1 || !szName || strlen(szName) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);

		if ( !pActor || !pActor->Set(szName, szValue) )
			szValue = NULL;
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.SetString] Failed.");
	}

	return Py_BuildValue("z", szValue);
}


// Given an actor object (passed in as a tuple from a python script),
// locate the requested property and return it's integer value 
//
// Syntax: mcore.GetInt(Actor, Property Name)
//
// Note: For this to work, the property must be "Registered" with the
// PropertyMgr for this actor.
PyObject* mcore_GetInt(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szName = NULL;
	long nValue = LONG_MIN;
	long nID = -1;

	try
	{
		if(!PyArg_ParseTuple(args, "lz", &nID, &szName))
			return Py_BuildValue("l", LONG_MIN);

		if ( nID == -1 || !szName || strlen(szName) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);

		if ( pActor && pActor->Get(szName, nValue) )
			return Py_BuildValue("l", nValue);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.GetInt] Failed.");
	}

	return Py_BuildValue("");
}

//
// Syntax: mcore.SetInt(Actor, Property Name, New Property Value)
//
// Note: For this to work, the property must be "Registered" with the
// PropertyMgr for this actor.
PyObject* mcore_SetInt(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szName = NULL;
	long nValue = 0;
	long nID = -1;

	try
	{
		if(!PyArg_ParseTuple(args, "lzl", &nID, &szName, &nValue))
			return Py_BuildValue("");

		if ( nID == -1 || !szName || strlen(szName) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);

		if ( pActor && pActor->Set(szName, nValue) )
			return Py_BuildValue("l", nValue);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.SetInt] Failed.");
	}

	return Py_BuildValue("");
}


// Given an actor object (passed in as a tuple from a python script),
// locate the requested property and return it's float value
//
// Syntax: GetFloat(Actor, Property Name)
//
// Note: For this to work, the property must be "Registered" with the
// PropertyMgr for this actor.
PyObject* mcore_GetFloat(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szName = NULL;
	float fValue = FLT_MIN;
	long nID = -1;

	try
	{
		if(!PyArg_ParseTuple(args, "lz", &nID, &szName))
			return Py_BuildValue("");

		if ( nID == -1 || !szName || strlen(szName) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);
		
		if ( pActor && pActor->Get(szName, fValue) )
			return Py_BuildValue("f", fValue);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.GetFloat] Failed.");
	}

	return Py_BuildValue("");
}

//
// Syntax: mcore.SetFloat(Actor, Property Name, New Property Value)
//
// Note: For this to work, the property must be "Registered" with the
// PropertyMgr for this actor.
PyObject* mcore_SetFloat(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szName = NULL;
	float fValue = FLT_MIN;
	long nID = -1;
	

	try
	{
		if(!PyArg_ParseTuple(args, "lzf", &nID, &szName, &fValue))
			return Py_BuildValue("");

		if ( nID == -1 || !szName || strlen(szName) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);

		if ( pActor && pActor->Set(szName, fValue) )
			return Py_BuildValue("f", fValue);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.SetFloat] Failed.");
	}

	return Py_BuildValue("");
}


//
// Syntax: mcore.DoCmd(Actor, Command Name, Arguments)
//
PyObject* mcore_ExecuteActorCommand(PyObject* self, PyObject* args)
{
	CActor* pActor = NULL;
	char* szCmd = NULL;
	char* szArgs = NULL;
	long nID = -1;

	try
	{
		if(!PyArg_ParseTuple(args, "lzz", &nID, &szCmd, &szArgs))
			return Py_BuildValue("");

		if ( nID == -1 || !szCmd || strlen(szCmd) < 1 )
			return Py_BuildValue("");

		pActor = CGameObjects::Get().GetActor(nID);

		//
		// NOTE: Should restrict commands here. Advise against allowing scripts to 
		// force an actor to 'delete', or 'quit', etc...
		//
		if ( pActor )
			return Py_BuildValue("b", pActor->ExecuteCommand(szCmd, szArgs));
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.ExecuteActorCommand] Failed.");
	}

	return Py_BuildValue("");
}


// Given an actor name, and optionally, a world name,
// attempt to locate the requested actor. Return it's GUID if found.
PyObject* mcore_LocateActor(PyObject* self, PyObject* args)
{
	char* szWorld = NULL;
	char* szActor = NULL;

	CGameWorld* pWorld = NULL;
	CActor* pActor = NULL;

	try
	{
		if(!PyArg_ParseTuple(args, "z|z", &szActor, &szWorld))
			return Py_BuildValue("");

		if ( !szActor || strlen(szActor) == 0 )
			return Py_BuildValue("");

		if ( !szWorld )
			szWorld = "Default World";

		pWorld = CGameObjects::Get().GetWorld(szWorld);

		if ( !pWorld )
			return Py_BuildValue("");

		pActor = pWorld->GetActor(szActor);

		if ( pActor )
			return Py_BuildValue("l", pActor->GUID());
		else
			return Py_BuildValue("");
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.LocateActor] Failed.");
	}

	return Py_BuildValue("");
}

//
// Syntax: mcore.WriteTo(Actor, Message)
//
PyObject* mcore_SendToPlayer(PyObject* self, PyObject* args)
{
	PyObject* pObject = NULL;
	CActor* pActor = NULL;
	char* szText = NULL;

	try
	{
		if(!PyArg_ParseTuple(args, "O|z", &pObject, &szText))
			return Py_BuildValue("b", false);

		if ( !pObject || !szText || strlen(szText) < 1 )
			return Py_BuildValue("b", false);

		pActor = (CActor*)pObject;

		pActor->Write("%s", szText);

		return Py_BuildValue("b", true);
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[mcore.SendToPlayer] Failed.");
	}

	return Py_BuildValue("b", false);
}


#else
#pragma message(Reminder "Python not enabled. CPython class will not be available.")

#endif // (HAVE_PYTHON)
