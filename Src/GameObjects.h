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
// CGameObjects Class
//
// GameObjects serves as a global repository for 'world independent'
// classes such as GameServer and Tools.  GameObjects also handles
// loading and configuring the base server options.
//


#ifndef __GAMEOBJECTS_H__
#define __GAMEOBJECTS_H__

#include "Mudcore.h"
#include "gString.h"
#include "Profile.h"
#include "Tools.h"
#include "Random.h"

#include "GameWorld.h"
#include "PathFinding.h"
#include "DataFile.h"
#include "Actor.h"
#include "User.h"
#include "Menu.h"
#include "Parser.h"
#include "HelpSystem.h"

class CSet;
class CTools;
class CGameServer;

class CGameObjects
{
public:
	///////////////////////////////////////////////////////////////////////////////////////
	// GameObjects is a singleton class. This method returns the one and only instance of
	// CGameObjects.
	///////////////////////////////////////////////////////////////////////////////////////
	static CGameObjects&	Get();

// Members
public:

	///////////////////////////////////////////////////////////////////////////////////////
	// Our constructors/destructors. Note that CGameObject is not designed to be a base
	// class for any other objects, therefore it's destructor is not virtual.
	///////////////////////////////////////////////////////////////////////////////////////
	CGameObjects();
	~CGameObjects();

	///////////////////////////////////////////////////////////////////////////////////////
	// Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////
	enum e_GameSettings
	{
		_ALLOW_CONNECTIONS	= 1,
		_ALLOW_NEW_ACCOUNTS,
		_STAFFLOCK,

		// Log Settings
		_LOG_DEBUG,
		_LOG_INFO,
		_LOG_WARN,
		_LOG_ERROR,
		_LOG_FATAL,
		_LOG_CRITICAL
	};

	enum e_Directories
	{
		_BASE				= 0,
		_DATA,
		_LOG,
		_PLAYER,
		_USER,
		_MENU,
		_WORLD,
		_SPACEDIR,
		_MODULES,
		_HULLCUBES,
		_NUMDIRS
	};


	enum e_Messages
	{
		MSG_CONNECTIONS		= 0,
		MSG_NEWPLAYERS,
		MSG_STAFFLOCK,
		MSG_GREETING
	};

	///////////////////////////////////////////////////////////////////////////////////////
	// Our basic configuration structure. Should hold esssential server configuration data
	///////////////////////////////////////////////////////////////////////////////////////
	typedef struct _BaseConfig
	{
		gString szDir[_NUMDIRS];
		gString gsDefaultMenu;
		bool	bReportProfile;
	};

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline Data Access Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline CSet*			GameFlags()		{ return m_pGameFlags; }
	inline CGameServer*		GameServer()	{ return m_GameServer; }
	inline CTools*			Tools()			{ return m_Tools; }
	
	inline	time_t			DateTime()		{ return m_Time; }
	inline  float			Clock()			{ return m_Clock.Elapsed(); }
	inline  UserMap&		Users()			{ return m_Users; }
	inline  ActorMap&		Players()		{ return m_Players; }
	inline  CDataFile&		ConfigData()	{ return m_ConfigFile; }
	inline  CParser*		ScriptHandler() { return m_pScriptHandler; }
	inline  void*			Handle()		{ return m_Handle; }
	inline  MenuMgr&		MenuManager()   { return m_MenuMgr; }
	inline  CHelpSystem&    HelpSystem()    { return m_HelpSystem; }
	inline  CTextMgr		MotdMgr()		{ return m_pMotdMgr; }
	
	///////////////////////////////////////////////////////////////////////////////////////
	// Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	bool					Initialize();
	void					Shutdown();
	void					Configure();
	void					Pulse();

	///////////////////////////////////////////////////////////////////////////////////////
	// GameWorld Functions
	///////////////////////////////////////////////////////////////////////////////////////
	inline  WorldList&      Worlds()		{ return m_Worlds; }
	CGameWorld*				GameWorld();
	bool					LoadGameWorlds();
	void					ShutdownWorlds();
	void					WriteMotd(CUser* pUser, int nEntries, bool bViewIndexes);
	CActor*					GetActor(long nID);
	CArea*					GetArea(CPlacement vPos);
	CGameWorld*				GetWorld(int iWorld);
	CGameWorld*				GetWorld(const gString& gsWorld);
	CRandom*				Rand() { return m_pRandomHandler; }

protected:
	bool					_Construct();
	bool					_Destruct();
	void					_MakeDirectory(gString& gsDir);

// Data

public:
	_BaseConfig				m_Config;
	CProfileMgr				m_ProfileMgr;


protected:

	
	// Config Setup
	CSet*					m_pGameFlags;
	CGameServer*			m_GameServer;
	CTools*					m_Tools;
	CRandom*				m_pRandomHandler;
	CDataFile				m_ConfigFile;
    CDataFile				m_WorldFile;
	CHelpSystem				m_HelpSystem;
	CTextMgr				m_pMotdMgr;

    WorldList				m_Worlds;

	time_t					m_Time;		// The actual time
	CTimer					m_Clock;	// The elapsed time, in seconds

	MenuMgr					m_MenuMgr;
	
	UserMap					m_Users;
	ActorMap				m_Players;
	CParser*				m_pScriptHandler;
	void*					m_Handle;
};

extern CGameObjects* _pgGameObjects;

#endif