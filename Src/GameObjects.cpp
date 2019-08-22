//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.
//
// The Advanced MUD Codebase Project
// AMC, copyright (c) 2005, 2006 by Owen Varley <owen@sw-erp.org>

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: CGameObjects
// Header   :: GameObjects.h
// Function :: Things that are global (like the game server, any global settings, etc) should go here...

#pragma warning(disable: 4786)

#include <memory.h>
#include <fstream>
#include <iostream>
#include <time.h>
#include <io.h>
#include <float.h>

#include "MudCore.h"
#include "Tools.h"
#include "DataFile.h"
#include "Maths.h"
#include "Set.h"
#include "RandomMT.h"
#include "RandomSys.h"


#include "GameObjects.h"
#include "GameServer.h"
#include "../gTools/Log.h"
#include "PathFinding.h"
#include "Actor.h"
#include "Area.h"
#include "Room.h"
#include "Parser.h"
#include "Emotions.h"

CGameObjects* _pgGameObjects = NULL;
static int _iReferenceCount = 0;

float	fActorUpdateDelta	=  3.0f;
float	fItemUpdateDelta	=  3.0f;
float	fPlayerUpdateDelta	=  3.0f;
float	fNpcUpdateDelta		=  3.0f;
float   fCrewUpdateDelta	=  3.0f;
float	fRoomUpdateDelta	= 30.0f;
float	fAreaUpdateDelta	= 60.0f;
float   fSpaceUpdateDelta   = 6.0f;


CGameObjects::CGameObjects()
{
	_Construct();
}

CGameObjects::~CGameObjects()
{
	_Destruct();
}

void CGameObjects::Shutdown()
{
	_Destruct();
}

//
// _Construct
// This function is here to allow us to re-initialize the game objects class without
// deleting it.
//
bool CGameObjects::_Construct()
{
	assert((++_iReferenceCount == 1) && "Use CGameObjects::Get() instead!");

#ifdef _WINDOWS
	m_Handle = (HMODULE)GetModuleHandle(NULL);
#else
	m_Handle = dlopen(APPNAME);
#endif

	if ( !m_Handle )
		g_Log.Log(LOG_ERROR, "[CGameObjects::_Construct] NULL Process Handle");

	// _pgGameObjects should always be NULL here.
	_pgGameObjects	= this;
	
	m_Clock.Start();
	m_Tools			= &CTools::Get();
	m_pGameFlags	= new CSet();
	m_GameServer	= new CGameServer();
	m_pScriptHandler= NULL;
	m_pRandomHandler= NULL;

	Initialize();
	Configure();

	//LoadGameWorlds();

	return true;
}

//
// _Destruct
// This function allows us to clean up the game objects class, without necessarily
// deleting it.
//
bool CGameObjects::_Destruct()
{
	delete m_pGameFlags;
	m_pGameFlags = NULL;

	delete m_GameServer;
	m_GameServer = NULL;

	m_Players.clear();
	m_Worlds.clear();

	m_ProfileMgr.Report();
	m_ProfileMgr.Delete("all");

	// Shutdown Python last
	if ( m_pScriptHandler != NULL )
	{
		m_pScriptHandler->Shutdown();
		delete m_pScriptHandler;
		m_pScriptHandler = NULL;
	}

	if ( m_pRandomHandler != NULL )
	{
		delete m_pRandomHandler;
		m_pRandomHandler = NULL;
	}

	_pgGameObjects = NULL;
	--_iReferenceCount;

	return true;
}


CGameWorld* CGameObjects::GameWorld()
{
	return (CGameWorld*)(*m_Worlds.begin());
}

//
// Load and initialize all of the game worlds.
//
bool CGameObjects::LoadGameWorlds()
{
	bool bRet = false;
	CGameWorld* pWorld = NULL;
	CDataFile WorldDF;	
	gString dfName;

	dfName.Format("%sGameWorlds.dat", m_Config.szDir[_WORLD]);

	if ( !WorldDF.Load(dfName) )
	{
		g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadGameWorlds] Unable to load game worlds. Aborting.");
		CGameObjects::Get().GameServer()->State() = CGameServer::_ERROR;
		return false;
	}

	gStringArray gsArray = WorldDF.GetSectionList();
	gStringArray::iterator pos;

	if ( gsArray.size() == 0 )
	{
		g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadGameWorlds] No game worlds defined. Aborting.");
		CGameObjects::Get().GameServer()->State() = CGameServer::_ERROR;
		return false;
	}

	for (pos = gsArray.begin(); pos != gsArray.end(); pos++)
	{
		gString gsKey, gsName;

		gsKey = (*pos);
		gsName = WorldDF.GetString(gsKey);

		if ( gsKey.Length() > 0 )
		{
			CGameWorld* pWorld = new CGameWorld;

			pWorld->SetName(gsName);
			pWorld->SetKey(gsKey);

			pWorld->m_gsAreaListFile.Format("%s%s\\arealist.dat", m_Config.szDir[_WORLD], gsKey);

			if (pWorld->Initialize())
			{
				m_Worlds.push_back(pWorld);
				
				// MCB 1.3 :: Added to fix problem with Areas being unable to load their HomeWorlds - Nekekami (5/12/2005)
				if (!pWorld->LoadAreas())
					g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadArea] Error Loading Areas.");

				if (!pWorld->LoadRaces())
					g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadRaces] Error Loading Races.");

				if (!pWorld->LoadEpacs())
					g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadEpacs] Error Loading Epacs.");

				if (!pWorld->LoadTemplates())
					g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadTemplates] Error Loading Templates.");

				if (!pWorld->LoadSpace())
					g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadSpace] Error Loading Space.");

				if (!pWorld->LoadCrews())
					g_Log.Log(LOG_CRITICAL, "[CGameObjects::LoadCrews] Error Loading Crews.");

				// add each worlds help file to our main help file
				gString gsHelp("%s%s\\help.dat",(const char*)m_Config.szDir[_WORLD], (const char*)gsKey);
				m_HelpSystem.Initialize(gsHelp);

				// Load in the motd entries
				gString gsMotd("%s\\motd.xml", (const char*)m_Config.szDir[_BASE]);
				m_pMotdMgr.Initialize(gsMotd);

			}
			else 
				delete pWorld;
		}
	}

	WorldList::iterator w_itor;

	for (w_itor = m_Worlds.begin(); w_itor != m_Worlds.end(); w_itor++)
		(*w_itor)->Update(true);

	return true;
}

void CGameObjects::ShutdownWorlds()
{
	WorldList::iterator pos;

	// Instruct each world to save each of it's areas
	try
	{
		for ( pos = m_Worlds.begin(); pos != m_Worlds.end(); pos++ )
			(*pos)->SaveAll();
	}
	catch(...) {}
}

void CGameObjects::WriteMotd(CUser* pUser, int nNumEntries, bool bShowIndexes)
{
	/*
	////////////////////[SW-ERP MOTD]\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	// 25/03/2006 // The name of our entry:                         \\
	//            // The text for this entry goes here and will be  \\
	//            // wrapped around to fit in this little box here  \\
	//////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	*/

	pUser->Socket()->Write("#NL#CR#400/////////////////////////#401[#701SW-ERP MOTD#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	int nCounter = 1;
	for (t_mTexts::const_reverse_iterator text = m_pMotdMgr.GetEntries().rbegin(); text != m_pMotdMgr.GetEntries().rend(); text++)
	{
		// We may only want to view so many entries
		if (nCounter >= nNumEntries)
			break;

		bool bInitial = true;
		gString gsCurrent;

		gString gsTemp = ((*text).second)->GetData();
		// For this formatting we will use a temporary string and take chunks 
		// of it bit by bit till we run out of string
		while (gsTemp.Length() > 0)
		{
			for (int i = MAX_WIDTH-25; i > 0; i--)
			{
				// Find the nearest space
				if (gsTemp.IsSpace(gsTemp.At(i)))
				{
					gsCurrent = gsTemp.Left(i);
					gsTemp = gsTemp.Right(gsTemp.Length() - i);
					break;
				}
			}

			// Remove leading empty space
			if (gsCurrent.IsSpace(gsCurrent.At(0)))
				gsCurrent.DeleteChar(0);
			
			//gString gsCurrent = gsTemp.Length() < 60 ? gsTemp : gsTemp.Left(60);
			//gsTemp = gsTemp.Right(gsTemp.Length() - 60);
			
			if (bInitial)
			{
				// Write the date as well
				if (bShowIndexes)
					pUser->Socket()->Write("#400//#600 %10s #400// #400[#401%2d#400] #601%-49s #400\\\\\n\r", (*text).second->GetDate(), nCounter, (*text).second->GetName() + ":");
				else
					pUser->Socket()->Write("#400//#600 %10s #400// #601%-54s #400\\\\\n\r", (*text).second->GetDate(), (*text).second->GetName() + ":");
				pUser->Socket()->Write("#400//            #400//#701 %-54s #400\\\\\n\r", gsCurrent);
				bInitial = false;
			}
			else
				pUser->Socket()->Write("#400//            #400//#701 %-54s #400\\\\\n\r", gsCurrent);
		}		

		nCounter++;

	}

	pUser->Socket()->Write("#400/////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
}

// Locate a specific actor by it's GUID
CActor* CGameObjects::GetActor(long nID)
{
	ActorMap::iterator a_pos;
	WorldList::iterator w_pos;
	CGameWorld* pWorld = NULL;

	for (w_pos = m_Worlds.begin(); w_pos != m_Worlds.end(); w_pos++)
	{
		pWorld = (*w_pos);

		a_pos = pWorld->Players().find(nID);
		if ( a_pos != pWorld->Players().end() )
			return ( (*a_pos).second );

		a_pos = pWorld->Mobiles().find(nID);
		if ( a_pos != pWorld->Mobiles().end() )
			return ( (*a_pos).second );

		a_pos = pWorld->Items().find(nID);
		if ( a_pos != pWorld->Items().end() )
			return ( (*a_pos).second );
	}

	return NULL;
}

CArea* CGameObjects::GetArea(CPlacement vPos)
{
	WorldList::iterator pos;
	AreaList::iterator pos2;
	CGameWorld* pWorld = NULL;
	CArea* pArea = NULL;

	for (pos = m_Worlds.begin(); pos != m_Worlds.end(); pos++ )
	{
		pWorld = (CGameWorld*)(*pos);

		if ( pWorld->GUID() == vPos.World() )
		{
			for (pos2 = pWorld->Areas().begin(); pos2 != pWorld->Areas().end(); pos2++)
			{
				pArea = (CArea*)(*pos2);

				if ( pArea->Area() == vPos.Area() )
					return pArea;
			}
		}
	}

	return NULL;
}

CGameWorld* CGameObjects::GetWorld(int iWorld)
{
	WorldList::iterator pos;	
        CGameWorld* pWorld = NULL;

	for (pos = m_Worlds.begin(); pos != m_Worlds.end(); pos++ )
	{
		pWorld = (CGameWorld*)(*pos);

		if ( pWorld && pWorld->GUID() == iWorld )
			return pWorld;
	}

	return NULL;
}

CGameWorld* CGameObjects::GetWorld(const gString& gsWorld)
{
	WorldList::iterator pos;
	CGameWorld* pWorld = NULL;

	for (pos = m_Worlds.begin(); pos != m_Worlds.end(); pos++ )
	{
		pWorld = (CGameWorld*)(*pos);

		if ( pWorld->Name().GetHash() == gsWorld.GetHash() )
			return pWorld;
	}

	return NULL;
}

void CGameObjects::_MakeDirectory(gString& gsDir)
{
	if ( gsDir.Length() == 0 )
		return;

	if ( !gFileName::DirectoryExists(gsDir) )
	{
		if (!gFileName::MakeDirectory(gsDir))
			g_Log.Log(LOG_ERROR, "Unable to create directory '%s'.", gsDir);
	}
}


void CGameObjects::Configure()
{
	gString gsKey = "none", gsData;
	bool bDone = false;
	CProfile* p = NULL;

	//////////////////////////////////////////////////////////////////////////////////////
	// Setup some default Profiles ///////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	p = m_ProfileMgr.Add("game_server");				// no parent
		p = m_ProfileMgr.Add("world_updates", p);		// child of game server
				m_ProfileMgr.Add("player_updates", p);	// child of world updates
				m_ProfileMgr.Add("area_updates", p);	// child of world updates
				m_ProfileMgr.Add("npc_updates", p);		// child of world updates
				m_ProfileMgr.Add("item_updates", p);	// child of world updates
				m_ProfileMgr.Add("space_updates", p);	// child of world updates
				m_ProfileMgr.Add("crew_updates", p);	// child of world updates

	p = m_ProfileMgr.Add("path_finding");


	//////////////////////////////////////////////////////////////////////////////////////
	// Setup some default directories ////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	m_Config.szDir[_BASE]		= ".\\";
	m_Config.szDir[_DATA]		= m_Config.szDir[_BASE] + "Data\\";
	m_Config.szDir[_LOG]		= m_Config.szDir[_DATA] + "Logs\\";
	m_Config.szDir[_PLAYER]		= m_Config.szDir[_DATA] + "Players\\";
	m_Config.szDir[_USER]		= m_Config.szDir[_DATA] + "Accounts\\";
	m_Config.szDir[_WORLD]		= m_Config.szDir[_DATA] + "Worlds\\";
	m_Config.szDir[_SPACEDIR]   = m_Config.szDir[_DATA] + "Space\\";
	m_Config.szDir[_MODULES]    = m_Config.szDir[_DATA] + "Space\\Ships\\Modules\\";
	m_Config.szDir[_HULLCUBES]  = m_Config.szDir[_DATA] + "Space\\Ships\\HullCubes\\";


	//////////////////////////////////////////////////////////////////////////////////////
	// Load the configuration file ///////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	if ( !m_ConfigFile.Load("config.dat") )
		Tools()->Report(E_ERROR, "[CGameObjects::Configure] Unable to load configuration file 'config.dat'.");


	//////////////////////////////////////////////////////////////////////////////////////
	// Setup the script interpreter //////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	gString gsScriptHandler = m_ConfigFile.GetString("script_interpreter", "Options");

	if ( gsScriptHandler.Length() > 0 )
	{
		// Initialize the script sublayer
		if ( (m_pScriptHandler = CParserFactory::Create(gsScriptHandler)) != NULL )
			m_pScriptHandler->Initialize();
	}

	gString gsRandomHandler = m_ConfigFile.GetString("random_handler", "Options");

	if ( gsRandomHandler.Length() > 0 )
	{
		if ( gsRandomHandler.CompareNoCase("basic") == 0 )
		{
			m_pRandomHandler = new CRandomSys;
		}
		else
		if ( gsRandomHandler.CompareNoCase("Mersenne Twister") == 0 )
		{
			m_pRandomHandler = new CRandomMT;

			CRandomMT* pHandler = (CRandomMT*)(m_pRandomHandler);

			pHandler->MimicRand( m_ConfigFile.GetBool("mimic_system_rand", "Options") );
		}
	}


	//////////////////////////////////////////////////////////////////////////////////////
	// Set our default menu //////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	m_Config.gsDefaultMenu = m_ConfigFile.GetString("default", "Menus");

	//////////////////////////////////////////////////////////////////////////////////////
	// Setup our directory structure /////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	gFileName gsBase = m_ConfigFile.GetString("base","Directories");
	gString gsDir;

	if ( gsBase.Length() > 0 )
	{
		_MakeDirectory(gsBase);
		gsBase = gsBase.AbsolutePath();
		m_Config.szDir[_BASE] = gsBase;
	}

	gsDir = gsBase + m_ConfigFile.GetString("data","Directories");
	if ( gsDir.Length() > 0 )
	{
		gsBase = gsDir;
		_MakeDirectory(gsDir);
		m_Config.szDir[_DATA] = gsDir;
	}

	gsDir = gsBase + m_ConfigFile.GetString("log","Directories");
	if ( gsDir.Length() > 0 )
	{
		_MakeDirectory(gsDir);
		m_Config.szDir[_LOG] = gsDir;
	}

	gsDir = gsBase + m_ConfigFile.GetString("player","Directories");
	if ( gsDir.Length() > 0 )
	{
		_MakeDirectory(gsDir);
		m_Config.szDir[_PLAYER] = gsDir;
	}

	gsDir = gsBase + m_ConfigFile.GetString("account","Directories");
	if ( gsDir.Length() > 0 )
	{
		_MakeDirectory(gsDir);
		m_Config.szDir[_USER] = gsDir;
	}

	gsDir = gsBase + m_ConfigFile.GetString("world","Directories");
	if ( gsDir.Length() > 0 )
	{
		_MakeDirectory(gsDir);
		m_Config.szDir[_WORLD] = gsDir;
	}

	gsDir = gsBase + m_ConfigFile.GetString("spacedir", "Directories");
	if ( gsDir.Length() > 0 )
	{
		_MakeDirectory(gsDir);
		m_Config.szDir[_SPACEDIR] = gsDir;
	}

	gsDir = gsBase + m_ConfigFile.GetString("modules", "Directories");
	if ( gsDir.Length() > 0 )
	{
		_MakeDirectory(gsDir);
		m_Config.szDir[_MODULES] = gsDir;
	}

	//////////////////////////////////////////////////////////////////////////////////////
	// Setup the log file ////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	gString gslfName;

	gslfName.Format("%s%d", m_Config.szDir[_LOG], time(0));
	Tools()->SetLogFile(gslfName);

	//////////////////////////////////////////////////////////////////////////////////////
	// Setup our Update deltas ///////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////
	fActorUpdateDelta	= Max(0.5f, m_ConfigFile.GetFloat("Actor",	"Update Deltas"));
	fItemUpdateDelta	= Max(0.5f, m_ConfigFile.GetFloat("Item",	"Update Deltas"));
	fPlayerUpdateDelta	= Max(0.5f, m_ConfigFile.GetFloat("Player",	"Update Deltas"));
	fNpcUpdateDelta		= Max(0.5f, m_ConfigFile.GetFloat("Npc",	"Update Deltas"));
	fRoomUpdateDelta	= Max(0.5f, m_ConfigFile.GetFloat("Room",	"Update Deltas"));
	fAreaUpdateDelta	= Max(0.5f, m_ConfigFile.GetFloat("Area",	"Update Deltas"));
	fSpaceUpdateDelta   = Max(0.5f, m_ConfigFile.GetFloat("Space",	"Update Deltas"));
}

//
// GameObjects is a "Singleton" class. In otherwords, only one instance of it ever
// exists at any given time.  Use 'Get()' to return a reference to this single object.
//
CGameObjects& CGameObjects::Get()
{
	if ( !_pgGameObjects )
		new CGameObjects;

	return *_pgGameObjects;
}

bool CGameObjects::Initialize()
{
/*	gString gsCredits("MudCore Server Version %s\n"
		             "Author: Gary McNickle\n"
					 "Publicly released under the GNU General Public License\n"
					 "on December 9th, 2000\n\n", Tools()->VersionToString(VERSION)); */

	// Updated to AMC
	gString gsCredits("AMC Server Version %s\n"
		             "Author: Owen Varley\n"
					 "Based on MUDCore 2.3 by Gary McNickle\n"
					 "Publicly released under the GNU General Public License "
					 "on December 9th, 2000\n\n", Tools()->VersionToString(VERSION));

	g_Log.Log(LOG_INFO, gsCredits);

	m_pGameFlags->SetBit(_ALLOW_CONNECTIONS);
	m_pGameFlags->SetBit(_ALLOW_NEW_ACCOUNTS);

	return true;
}

void CGameObjects::Pulse()
{
	WorldList::iterator pos;

	m_Time = time(0);

	// Process each worlds updates
	try
	{
		for ( pos = m_Worlds.begin(); pos != m_Worlds.end(); pos++ )
			(*pos)->Update();
	}
	catch(...) {}
}

