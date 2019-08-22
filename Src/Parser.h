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
// The CParser class provides a simple API to base script interpreters off of. It is an
// abstract base class and as such cannot be directly instantiated.

#ifndef __SCRIPT_H__
#define __SCRIPT_H__

#include "Tools.h"
#include "gString.h"


#if (HAVE_PYTHON)
	#ifdef _DEBUG
		#undef _DEBUG
			#include <Python.h>
		#define _DEBUG
	#else
		#include <Python.h>
	#endif
#endif     

#if (HAVE_TCL)
#include <Tcl.h>
#endif

//
// A simple Script Parser api
//
class CParser	
{
// Methods
public:
	CParser() {m_bInitialized = false;}
	virtual ~CParser() {};

	virtual void		Initialize()=0;
	virtual void		Shutdown()=0;
	virtual bool		IsInitialized()=0;
	virtual bool		ExecuteScript(const gString& szScript)=0;
	virtual bool		ExecuteScript(const gFileName& fnScript)=0;

// Data
protected:
	bool		m_bInitialized;
};

#if (HAVE_PYTHON)
class CPython : public CParser
{
// Methods
public:
	CPython();
	~CPython();

	void		Initialize();
	void		Shutdown();
	bool		IsInitialized();
	bool		ExecuteScript(const gString& szScript);
	bool		ExecuteScript(const gFileName& fnScript);
// Data
protected:
};
#endif

#if (HAVE_TCL)

#pragma comment(lib, "tcl84.lib")

class CTcl : public CParser
{
// Methods
public:
	CTcl();
	~CTcl();

	void		Initialize();
	void		Shutdown();
	bool		IsInitialized();
	bool		ExecuteScript(const gString& szScript);
	bool		ExecuteScript(const gFileName& fnScript);
// Data
protected:
	Tcl_Interp* m_pInterpreter;
};
#endif


class CParserFactory
{
public:
	static CParser* Create(const gString& gsName);
};

#endif

