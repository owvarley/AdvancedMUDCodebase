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

// Class    :: CActor
// Header   :: Actor.h
// Function :: The CActor class is the base class of all actors.  Actors are loosely defined as 'things'
//			:: that interact with the environment. At the time of this writing, these include Items, NPCs,
//			:: and Players.

// Get rid of that pesky stl warning about std::containers...
#pragma warning(disable:4786)

#include <time.h>
#include <fstream>

#include "MudCore.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "Socket.h"
#include "Placement.h"
#include "PathFinding.h"
#include "../gTools/Log.h"


// This variable is used to determine a unique ID for each actor created.
UINT uiUniqueActorID = 0;

//
// Static definitions of member variables used to hold human readable versions of
// various enumerated types.
//
char* CActor::szActorFlags[] = {"Actor", "NPC", "Player", "Item", "Staff", "Assistant", "Administrator", NULL};
char* CActor::szActorStates[] = {"updates", "thinks", "pass unhandled events", "ignore events", NULL};
char* CActor::szActorPositions[] = {"prone", "crouched", "sitting", "standing", "hovering", "flying", NULL};
char* CActor::szActorMental[] = {"resting", "sleeping", "unconscious", "incapacitated", "shell shocked", "dead", NULL};


//
// The core actor constructor.
//
CActor::CActor()
{
	m_uiUniqueID    = ++uiUniqueActorID;
	m_iLevel		= 0;
	m_gsDescription	= "";
	m_gsName		= "";
	m_gsTitle		= "";
	m_CreationDate	= time(0);
	m_fLastUpdate	= CGameObjects::Get().Clock();
	m_pHomeWorld	= NULL;
	m_iVnum			= -1;
	m_gsShortDesc	= "";
	m_Health[0]		= 100;
	m_Health[1]		= 100;
	m_Position.Set(1, 0, 0);

	m_pWatchActor	= NULL;

	m_Manned		= NULL;
	m_MannedPos		= NULL;

	m_Command		= NULL;

	m_Leader		= NULL;
	m_Crew			= NULL;

	// Note that the default actor flags are zero. Fully initialized actors should
	// never have a zero ActorFlags setting, and this condition is used to indicate
	// an improperly designed, or not fully initialized actor.
	m_ActorFlags	 = new CSet;

	// Note that the default actor states are zero. By default, actors neither
	// update, nor think.
	m_ActorStates    = new CSet;

	m_ActorPositions = new CSet;
	m_ActorPositions->SetBit(_STANDING);
	m_gsShortDesc	 = "is standing here.";

	m_ActorMental    = new CSet;


	// Add the Attributes
	// CAttribute& Strength = m_Attributes['strength'];

	// Physical
	CAttribute& Strength = m_Attributes["strength"];
	CAttribute& Dexterity = m_Attributes["dexterity"];
	CAttribute& Constitution = m_Attributes["constitution"];

	// Mental
	CAttribute& Intelligence = m_Attributes["intelligence"];
	CAttribute& Reaction = m_Attributes["reaction"];
	CAttribute& Wisdom = m_Attributes["wisdom"];

	// Social
	CAttribute& Charisma = m_Attributes["charisma"];
	CAttribute& Intuition = m_Attributes["status"];
	CAttribute& Willpower = m_Attributes["willpower"];

	// Identifier is used primarily for debugging purposes.
	m_gsIdentifier.Format("Actor_%d", m_uiUniqueID);

	m_CurrentRoom	= NULL;

	m_ActiveEvents.clear();

	// Register the member variables that we want to expose to scripting
	Register("Name", &m_gsName);
	Register("GUID", (long*)&m_uiUniqueID);
	Register("Title", &m_gsTitle);
	Register("Description", &m_gsDescription);
	Register("LastUpdate", &m_fLastUpdate);
	Register("VNUM", (long*)&m_iVnum);
	Register("Position", &m_Position);
}


CActor::~CActor()
{
	// Actors should have allready been removed from their homeworld, but just in
	// case someone forgot...
	if ( m_pHomeWorld )
	{
		m_pHomeWorld->Remove(this);
		m_pHomeWorld = NULL;
	}

	delete m_ActorFlags;
	m_ActorFlags = NULL;

	delete m_ActorStates;
	m_ActorStates = NULL;

	delete m_ActorPositions;
	m_ActorPositions = NULL;

	delete m_ActorMental;
	m_ActorMental = NULL;

	m_Attributes.Clear();

	m_gsIdentifier	+= "_Deleted";
	m_CurrentRoom	= NULL;

	if (m_MannedPos)
		m_MannedPos->m_Manned = NULL;

	m_MannedPos		= NULL;
	m_Manned		= NULL;

	if (m_Command)
		m_Command->m_Commander = NULL;

	m_Command		= NULL;
	m_Leader		= NULL;
	m_Crew			= NULL;
}

// Copy constructor...
CActor::CActor(const CActor& a)
{
	//
	// Note: When copying one actor to another, DONT copy the unique ID.
	// Or the identifier.
	//
	m_gsName		= a.m_gsName;
	m_gsDescription	= a.m_gsDescription;
	m_gsTitle		= a.m_gsTitle;
	m_gsShortDesc	= a.m_gsShortDesc;
	m_Position		= a.m_Position;
	m_fLastUpdate	= a.m_fLastUpdate;
	m_iLevel		= a.m_iLevel;
	m_iVnum			= a.m_iVnum;
	m_CreationDate	= a.m_CreationDate;
	m_pHomeWorld	= a.m_pHomeWorld;
	m_CurrentRoom	= a.m_CurrentRoom;
	*m_ActorFlags	= *a.m_ActorFlags;
	*m_ActorStates	= *a.m_ActorStates;
	*m_ActorPositions = *a.m_ActorPositions;
	*m_ActorMental  = *a.m_ActorMental;
	*m_pWatchActor	= *a.m_pWatchActor;
	memcpy(m_Health, a.m_Health, sizeof(m_Health));

}

CActor& CActor::operator = (CActor& a)
{
	//
	// Note: When copying one actor to another, DONT copy the unique ID.
	// Or the identifier.
	//
	m_gsName		= a.m_gsName;
	m_gsDescription	= a.m_gsDescription;
	m_gsTitle		= a.m_gsTitle;
	m_gsShortDesc	= a.m_gsShortDesc;
	m_Position		= a.m_Position;
	m_fLastUpdate	= a.m_fLastUpdate;
	m_iLevel		= a.m_iLevel;
	m_iVnum			= a.m_iVnum;
	m_CreationDate	= a.m_CreationDate;
	m_pHomeWorld	= a.m_pHomeWorld;
	m_CurrentRoom	= a.m_CurrentRoom;
	*m_ActorFlags	= *a.m_ActorFlags;
	*m_ActorStates	= *a.m_ActorStates;
	*m_ActorPositions = *a.m_ActorPositions;
	*m_ActorMental  = *a.m_ActorMental;
	*m_pWatchActor	= *m_pWatchActor;
	memcpy(m_Health, a.m_Health, sizeof(m_Health));


	return *this;
}

// Write this actor to an output stream
std::ostream& operator << ( std::ostream& stream, const CActor& actor )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	assert(actor.m_ActorFlags && "actor is not fully initialized.");
	assert(actor.m_ActorStates && "actor is not fully initialized.");

	Tools.WriteLn(stream, "[Actor]");
	Tools.WriteLn(stream, " Version           : %d",		VERSION);

	if ( actor.m_gsName.Length() > 0 )
		Tools.WriteLn(stream, " Name              : \"%s\"",	actor.m_gsName);

	if ( actor.m_gsTitle.Length() > 0 )
		Tools.WriteLn(stream, " Title             : \"%s\"",	actor.m_gsTitle);

	if ( actor.m_gsShortDesc.Length() > 0 )
		Tools.WriteLn(stream, " Short Description : \"%s\"",	actor.m_gsShortDesc);

	if ( actor.m_gsDescription.Length() > 0 )
		Tools.WriteLn(stream, " Description       : \"%s\"",	actor.m_gsDescription);

	Tools.WriteLn(stream, " Unique ID         : %d",		actor.m_uiUniqueID);
	Tools.WriteLn(stream, " Level             : %d",		actor.m_iLevel);
	Tools.WriteLn(stream, " VNum              : %d",		actor.m_iVnum);
	Tools.WriteLn(stream, " Max Health        : %d",		actor.m_Health[0]);
	Tools.WriteLn(stream, " Current Health    : %d",		actor.m_Health[1]);
	Tools.WriteLn(stream, " Creation Date     : %d",		actor.m_CreationDate);
	Tools.WriteLn(stream, " Homeworld ID      : %d",		actor.m_pHomeWorld ? actor.m_pHomeWorld->GUID() : 0);

	// Vectors & sets have to be treated differently...
	stream <<			  " Position          : " <<		actor.m_Position;
	stream <<             " Actor Flags       : " <<		*actor.m_ActorFlags;
	stream <<             " Actor Mental      : " <<		*actor.m_ActorMental;
	stream <<             " Actor Pos         : " <<		*actor.m_ActorPositions;
	stream <<             " Actor States      : " <<		*actor.m_ActorStates;
	Tools.WriteLn(stream, "[/Actor]");

	return stream;
}

// Read this actor from an input stream
std::istream& operator >> ( std::istream& stream, CActor& actor )
{
	LOG_SCOPE("CActor:>>");
	CTools& Tools = *CGameObjects::Get().Tools();

	int nVersion, nWorld;
	gString gsKey, gsString;
	bool bDone = false;

	try
	{
		assert(actor.m_ActorFlags && "actor is not fully initialized.");
		assert(actor.m_ActorStates && "actor is not fully initialized.");

		if ( Tools.ReadKey(stream) == "[Actor]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Actor]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'A':
						if ( gsKey == "Actor Flags" )
							Tools.ReadData(stream, *actor.m_ActorFlags);
						else
						if ( gsKey == "Actor Mental" )
							Tools.ReadData(stream, *actor.m_ActorMental);
						else
						if ( gsKey == "Actor Pos" )
							Tools.ReadData(stream, *actor.m_ActorPositions);
						else
						if ( gsKey == "Actor States" )
							Tools.ReadData(stream, *actor.m_ActorStates);
						break;
					case 'C':
						if ( gsKey == "Creation Date" )
							Tools.ReadData(stream, actor.m_CreationDate);
						else
						if ( gsKey == "Current Health" )
							Tools.ReadData(stream, actor.m_Health[1]);
						break;
					case 'D':
						if ( gsKey == "Description" )
							Tools.ReadData(stream, actor.m_gsDescription);
						break;
					case 'H':
						if ( gsKey == "Homeworld ID" )
							Tools.ReadData(stream, nWorld);
						break;
					case 'L':
						if ( gsKey == "Level" )
							Tools.ReadData(stream, actor.m_iLevel);
						break;
					case 'M':
						if ( gsKey == "Max Health" )
							Tools.ReadData(stream,actor.m_Health[0]);
						break;
					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, actor.m_gsName);
						break;
					case 'P':
						if ( gsKey == "Position" )
							Tools.ReadData(stream, actor.m_Position);
						break;
					case 'S':
						if ( gsKey == "Short Description" )
							Tools.ReadData(stream, actor.m_gsShortDesc);
						break;
					case 'T':
						if ( gsKey == "Title" )
							Tools.ReadData(stream, actor.m_gsTitle);
						break;
					case 'U':
						if ( gsKey == "Unique ID" )
						{
							int nID;
//							Tools.ReadData(stream, actor.m_uiUniqueID);
							Tools.ReadData(stream, nID);
						}
						break;
					case 'V':
						if ( gsKey == "VNum" )
							Tools.ReadData(stream, actor.m_iVnum);
						else
						if ( gsKey == "Version" )
							Tools.ReadData(stream, nVersion);
						break;
					default:
						g_Log.Log(LOG_ERROR, "Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Actor]");
			}

			actor.m_pHomeWorld = CGameObjects::Get().GetWorld(nWorld);
		}
		else
			g_Log.Log(LOG_ERROR, "Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "Error encountered while reading %s\'s file..", actor.Name());
	}

	return stream;
}

void CActor::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CAtomic::WriteXml(pParent);

	Tools.WriteXml(pParent, "actor_flags",	*m_ActorFlags);
	Tools.WriteXml(pParent, "actor_states",	*m_ActorStates);
	Tools.WriteXml(pParent, "actor_pos",	*m_ActorPositions);
	Tools.WriteXml(pParent, "actor_mental",	*m_ActorMental);

	if ( m_pHomeWorld )
	{
		unsigned int nID = m_pHomeWorld->GUID();
		Tools.WriteXml(pParent, "homeworld_id", nID);
	}

	m_Attributes.WriteXml(pParent);

}

void CActor::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CAtomic::ReadXml(pParent);

	Tools.ReadXml(pParent, "actor_flags",	*m_ActorFlags);
	Tools.ReadXml(pParent, "actor_states",	*m_ActorStates);
	Tools.ReadXml(pParent, "actor_pos",		*m_ActorPositions);
	Tools.ReadXml(pParent, "actor_mental",	*m_ActorMental);

	unsigned int nHomeWorld;
	Tools.ReadXml(pParent, "homeworld_id", nHomeWorld);

	m_Attributes.ReadXml(pParent);

	// post init...
	m_pHomeWorld = CGameObjects::Get().GetWorld(nHomeWorld);
}

// Calculate the actors position based on the room he inhabits.
CPlacement CActor::Position()
{
//	static CPlacement _Nowhere(0,0,0);

	if ( m_CurrentRoom )
		m_Position = m_CurrentRoom->Position();
//	else
//		m_Position = _Nowhere;

	return m_Position;
}

// Describe this actor to another actor.
void CActor::DescribeTo(CActor* pA, bool bFull) const
{
	gString sVnum;

	sVnum.Format("[%5d] ", Vnum());
	pA->Write("%s%s\n\r%s\n\r",
		( pA->IsAdministrator() || pA->IsAssistant() || pA->IsStaff()) ? "" : sVnum,
		Name(),
		bFull ? Description() : ShortDesc());
}

// Can Mobile access the Communications equipment of the ship
bool CActor::CanComm()
{
	// Have to be manning a terminal
	if (this->Manned())
	{
		CModule* pMod = this->MannedPos();

		if (!pMod)
			return false;

		if (pMod->m_ncDurability <= 0)
			return false;

		if (pMod->m_nType == CModule::MT_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_HELM ||
			pMod->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_GUNNERY_CONSOLE)
			return true;
	}

	return false;
}

// Is mobile manning the piloting controls of the ship
bool CActor::CanPilot()
{
	// Have to be manning a terminal
	if (this->Manned())
	{
		CModule* pMod = this->MannedPos();

		if (!pMod)
			return false;

		if (pMod->m_ncDurability <= 0)
			return false;

		if (pMod->m_nType == CModule::MT_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_HELM ||
			pMod->m_nType == CModule::MT_PILOT_CONSOLE ||
			pMod->m_nType == CModule::MT_COPILOT_CONSOLE)
			return true;
	}

	return false;
}

bool CActor::CanFire()
{
	// Have to be manning a terminal
	if (this->Manned())
	{
		CModule* pMod = this->MannedPos();

		if (!pMod)
			return false;

		if (pMod->m_ncDurability <= 0)
			return false;

		if (pMod->m_nType == CModule::MT_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_COPILOT_CONSOLE ||
			pMod->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_GUNNERY_CONSOLE ||
			pMod->m_nType == CModule::MT_WEAPONS_CONSOLE)
			return true;
	}

	return false;
}

bool CActor::CanRadar()
{
	// Have to be manning a terminal
	if (this->Manned())
	{
		CModule* pMod = this->MannedPos();

		if (!pMod)
			return false;

		if (pMod->m_ncDurability <= 0)
			return false;

		if (pMod->m_nType == CModule::MT_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_PILOT_CONSOLE ||
			pMod->m_nType == CModule::MT_HELM ||
			pMod->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_GUNNERY_CONSOLE ||
			pMod->m_nType == CModule::MT_COPILOT_CONSOLE)
			return true;
	}

	return false;
}

bool CActor::CanNav()
{
	// Have to be manning a terminal
	if (this->Manned())
	{
		CModule* pMod = this->MannedPos();

		if (!pMod)
			return false;

		if (pMod->m_ncDurability <= 0)
			return false;

		if (pMod->m_nType == CModule::MT_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_HELM ||
			pMod->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_PILOT_CONSOLE ||
			pMod->m_nType == CModule::MT_NAV_CONSOLE)
			return true;
	}

	return false;
}

bool CActor::CanSystem()
{
	// Have to be manning a terminal
	if (this->Manned())
	{
		CModule* pMod = this->MannedPos();

		if (!pMod)
			return false;

		if (pMod->m_ncDurability <= 0)
			return false;

		if (pMod->m_nType == CModule::MT_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_COPILOT_CONSOLE ||
			pMod->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
			pMod->m_nType == CModule::MT_GUNNERY_CONSOLE ||
			pMod->m_nType == CModule::MT_ENGINEERING_CONSOLE ||
			pMod->m_nType == CModule::MT_PILOT_CONSOLE)
			return true;
	}

	return false;
}

//
// Make sure we dont have a vnum conflict with another actor in the world...
//
bool CActor::SetVnum(int iVnum)
{
	ActorMap::iterator pos;
	bool bFound = false;

	// If we are not currently occupying a world, then their can be no conflict.
	if ( !HomeWorld() )
	{
		m_iVnum = iVnum;
		return true;
	}

	pos = HomeWorld()->Mobiles().find( iVnum );
	bFound = ( pos != HomeWorld()->Mobiles().end() );

	if ( !bFound )
	  m_iVnum = iVnum;

	return !bFound;
}

// A 'WatchActor' is an actor who is currently observing this actor. Much like
// a spirit that has possessed the actor, they see and hear everything this watched
// actor sees or hears.
bool CActor::SetWatchActor(CActor* pWatcher)
{
	// Note: No validation done here!
	m_pWatchActor = pWatcher;

	return (m_pWatchActor != NULL);
}

// Apply a value to an actors current health. This value can be a negative or
// positive value.  The function returns true if actor health value is above
// 0 after the adjustment.
bool CActor::AdjCurrentHealth(int nValue)
{
	m_Health[0] += nValue;

	return (m_Health[0] > 0);
}

bool CActor::SetMaxHealth(int nValue)
{
	m_Health[1] = nValue;
	return true;
}

bool CActor::SetHomeWorld(CGameWorld* pWorld)
{
	if ( m_pHomeWorld && m_pHomeWorld != pWorld )
	{
		m_pHomeWorld->Remove(this);
	}

	m_pHomeWorld = pWorld;

	return (m_pHomeWorld != NULL);
}

// Moves an actor into a room, if necessary, removing him from any room it may
// already be occupying.
bool CActor::SetCurrentRoom(CRoom* pRoom)
{
	CRoom* pLastRoom = m_CurrentRoom;
	std::pair<ActorMap::iterator, bool> result;

	if ( !pRoom )
		return false;

	if ( m_CurrentRoom )
	{
		ActorMap::iterator pos = m_CurrentRoom->Actors().find(GUID());

		if ( pos != m_CurrentRoom->Actors().end() )
			m_CurrentRoom->Actors().erase(pos);
	}

	result = pRoom->Actors().insert( ActorMap::value_type(GUID(), this) );

	// For some reason, the actor is being properly inserted into the map, but
	// result.second is being shown as 'false'.  Figure this out...#TODO#

	/*
	if ( result.second )
		m_CurrentRoom = pRoom;
	else
	if ( pLastRoom )
	{
		m_CurrentRoom = 0; // so we dont recurse forever
		return SetCurrentRoom(pLastRoom);
	}
	*/
	m_CurrentRoom = pRoom;

	Position(); // Update our position.

//	return (result.second);
	return true;
}

// Display a string of formatted text to the actor.
int CActor::Write(char *fmt, ...)
{
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	if ( m_pWatchActor && m_pWatchActor->IsPlayer() )
	    (CPlayer*)m_pWatchActor->Write("(%s) %s", Name(), buf);

	return length;
}

// Report a Command from the Crew
int CActor::Report(CActor *pA, char *fmt, ...)
{
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	// If for some Reason there is no longer a Commander we will just report the order over
	// the ShipNet
	if (!pA)
	{
		this->ExecuteCommand("ShipNet", "\"%s\" %s", "Control Point", buf);
		return length;
	}


	if (this->m_Position.Area() == pA->Position().Area() &&
		this->m_Position.Room() == pA->Position().Room() &&
		this->m_Position.World() == pA->Position().World())
	{
		this->ExecuteCommand("say", "%s", buf);
	}
	else
	{
		this->ExecuteCommand("ShipNet", "\"%s\" %s", "Control Point", buf);
	}

	return length;
}

//
// This function should be used to process any logical reasoning the actor
// may need to make.
//
void CActor::Think(bool bForce)
{
	if ( !ActorStates()->IsSet(_THINKS) && !bForce )
		return;
}

//
// This function should be used to process any mechanical updates the actor
// may need to make
//
void CActor::Update(bool bForce)
{
	float fThisUpdate = CGameObjects::Get().Clock();

	if ( EventCount() > 0 && !ActorStates()->IsSet(_IGNORE_EVENTS) )
		HandleEvents();

	if ( !ActorStates()->IsSet(_UPDATES) && !bForce )
		return;

	// Only update every 3 seconds or if forced
	if ( bForce || (fThisUpdate - m_fLastUpdate >= fActorUpdateDelta) )
	{
		m_fLastUpdate = fThisUpdate;
	}
}

bool CActor::SetName(const gString& sNewName)
{
	if ( !sNewName.IsEmpty() )
	{
		m_gsName = sNewName;
		return true;
	}

	return false;
}

bool CActor::SetDescription(const gString& sNewDescription)
{
	m_gsDescription = sNewDescription;
	return true;
}

bool CActor::SetTitle(const gString& sNewTitle)
{
	m_gsTitle = sNewTitle;
	return true;
}

bool CActor::SetShortDesc(const gString& sNewShortDesc)
{
	m_gsShortDesc = sNewShortDesc;
	return true;
}


bool CActor::SetLevel(int iLevel)
{
	if ( iLevel > m_iLevel )
		Write("You are being advanced to level %d!\n\r", iLevel);
	else
		Write("You are being lowered to level %d.\n\r", iLevel);

	m_iLevel = iLevel;

	return true;
}

bool CActor::SetManned(CShip* pShip)
{
	m_Manned = pShip;
	return true;
}

bool CActor::SetCommand(CShip* pShip)
{
	m_Command = pShip;
	return true;
}

bool CActor::SetLeader(CCrew* pCrew)
{
	m_Leader = pCrew;
	return true;
}

bool CActor::SetMember(CCrew* pCrew)
{
	m_Crew = pCrew;
	return true;
}


bool CActor::SetMannedPos(CModule* pMod)
{
	m_MannedPos = pMod;
	return true;
}

//
// Note that the default actor has no knowledge of how to persist in the world.
// This and Load must be overridden to accomplish serialization.
//
bool CActor::Save()
{
	return false;
}

//
// Note that the default actor has no knowledge of how to persist in the world.
// This and Save must be overridden to accomplish serialization.
//
bool CActor::Load()
{
	return false;
}

bool CActor::ExecuteCommand(const gString& gsCmd, char* szArgs,...)
{
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, szArgs);
	length = _vsnprintf(buf, MSL, szArgs, args);
	va_end (args);

	if ( !m_pHomeWorld )
		return false;

	gString gsCommandLine;

	gsCommandLine.Format("%s %s", gsCmd, buf);

	return m_pHomeWorld->Interpret(this, gsCommandLine);
}


//
// Note that the default actor comparision is an EXACT comparision. It will return
// false if two actors are identical in every way, but have different uniqueIDs.
//
// This, and it's partner operator != will very likely need to be overridden for
// subclasses of actor.
//
bool CActor::operator == (CActor a2)
{
	return ( m_uiUniqueID == a2.m_uiUniqueID );
}

bool CActor::operator != (CActor a2)
{
	return !( *this == a2 );
}

// Derived Attributes
int CActor::Perception()
{
	int nPerception = 0;

	nPerception += m_Attributes["wisdom"].Cur();
	//nPerception += m_Attributes["focus"].Cur();

	return nPerception;
}

// Defense
int CActor::Defense()
{
	int nDefense = 0;

	if (m_Attributes["dexterity"].Cur() > m_Attributes["reaction"].Cur())
		nDefense += m_Attributes["reaction"].Cur();
	else
		nDefense += m_Attributes["dexterity"].Cur();

#pragma message (Reminder "[CActor::Defense] Needs racial bonus added")

	return nDefense;
}

// Peripheral
int CActor::Peripheral()
{
	return m_Attributes["wisdom"].Cur() + m_Attributes["constitution"].Cur() + m_Attributes["willpower"].Cur();
}

// Integrity
int CActor::Integrity()
{
	if (m_Attributes["wisdom"].Cur() > m_Attributes["willpower"].Cur())
		return m_Attributes["willpower"].Cur();
	else
		return m_Attributes["wisdom"].Cur();
}

// Health
int CActor::Health()
{
	return m_Attributes["constitution"].Cur() + 4;
}



////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool CActor::HandleEvent(CEvent& Event)
{
	bool bHandled = false;

	if ( ActorStates()->IsSet(_IGNORE_EVENTS) )
		return false;

	switch ( Event.m_EventCode )
	{
		case EV_DAMAGED:
			bHandled = OnDamage(Event);
			break;
	}

	if ( !bHandled && ActorStates()->IsSet(_PASSES_EVENTS) )
		return CAtomic::HandleEvent(Event);
	else
		return bHandled;

}

/////////////////////////////////////////////////////////////////////////////////
// Event Handlers ///////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
bool CActor::OnDamage(CEvent& Event)
{
	EDamage* pEvent = (EDamage*)(&Event);

	if ( pEvent )
		return AdjCurrentHealth(pEvent->fAmount);

	return false;
}

bool CActor::OnNotice(CEvent& Event)
{
	ENotice* pNotice = (ENotice*)(&Event);

	return Write(pNotice->gsNotice) > 0;
}

/////////////////////////////////////////////////////////////////////////////////
// Actor Events /////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
ENotice::ENotice() : CEvent(CActor::EV_NOTICE)
{
}

EDied::EDied() : CEvent(CActor::EV_DIED)
{
}

EDamage::EDamage() : CEvent(CActor::EV_DAMAGED), pInflictor(0), fAmount(0.0f)
{
}

EDie::EDie() : CEvent(CActor::EV_DIE)
{
}

EAttack::EAttack() : CEvent(CActor::EV_ATTACKED), m_pByWho(0)
{
}
