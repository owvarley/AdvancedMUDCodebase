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

// Class    :: CNpc
// Header   :: Npc.h
// Function :: Should provide a basic framework for AI


#include <fstream>

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "Npc.h"
#include "Room.h"
#include "Area.h"
#include "GameWorld.h"
#include "../gTools/Log.h"

char* CNpc::szNPCTypes[] =  {"Invalid", "Human"};
char* CNpc::szNPCStates[]=  {"None", "Idle", "Fighting", "Fleeing", "Hungry", "Thirsty", "Wandering", "Guarding", ""};
char* CNpc::szNPCPersonas[]={"Meek", "Nice", "Normal", "Aggressive", "Mean", "Cruel", ""};


CNpc::CNpc()
{
	m_State = _IDLE;
	m_fLastUpdate = CGameObjects::Get().Clock();
	m_fMaxIdleTime = 30.0f; // 30 seconds
	m_TargetPos = m_Position;
	m_Persona = (e_Persona)((rand()%_NUMPERSONAS));
	m_pTarget = NULL;
	m_gsIdentifier.Format("NPC_%d", uiUniqueActorID);
	m_ActorFlags->SetBit(CActor::_NPC);
	m_ActorStates->SetBit(CActor::_UPDATES);
	m_ActorStates->SetBit(CActor::_THINKS);

	Register("State", (long*)&m_State);
	Register("MaxIdleTime", &m_fMaxIdleTime);
}


CNpc::~CNpc()
{
}

CNpc& CNpc::operator = (CNpc& a)
{
	// dont copy creation date and unique id.
	CActor::operator=(a);

	memcpy(m_Health, a.m_Health, sizeof(m_Health));
	m_pHomeWorld	= a.m_pHomeWorld;
	m_fMaxIdleTime  = a.m_fMaxIdleTime;
	m_IdlePeriod    = a.m_IdlePeriod;
	m_State			= a.m_State;
	m_Persona		= a.m_Persona;
	m_TargetPos		= a.m_TargetPos;
	m_pTarget		= a.m_pTarget;

	return *this;
}

CNpc::e_Persona CNpc::GetPersona(gString gsPersona)
{
	if ( gsPersona.IsEmpty() )
		return CNpc::_NORMAL;

	gsPersona.MakeUpper();

	for (int n=0; n<CNpc::_NUMPERSONAS-1; n++)
		if ( gsPersona[0] == CNpc::szNPCPersonas[n][0] )
			return (CNpc::e_Persona)n;


	return CNpc::_NORMAL;
}

std::ostream& operator << ( std::ostream& stream, const CNpc& npc )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	stream << (const CActor&)(npc);

	Tools.WriteLn(stream, "[Npc]");
	Tools.WriteLn(stream, " Max Idle Time     : %2.3f", npc.m_fMaxIdleTime);
	Tools.WriteLn(stream, " Current State     : %d", (int)npc.m_State);
	Tools.WriteLn(stream, " Persona           : \"%s\"", CNpc::szNPCPersonas[npc.m_Persona]);

	if ( npc.m_pTarget )
	{
		stream <<		      " Target Position   : " << npc.m_TargetPos;
		Tools.WriteLn(stream, " Target GUID		  : %d", npc.m_pTarget->GUID());
	}

	Tools.WriteLn(stream, "[/Npc]");

	return stream;
}


std::istream& operator >> ( std::istream& stream, CNpc& npc )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	bool bDone = false;

    stream >> (CActor&)(npc);

	if (!stream.fail() && !stream.eof())

	try
	{
		if ( Tools.ReadKey(stream) == "[NPC]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/NPC]"  )
				switch ( gsKey[0] )
				{
					case 'C':
						if ( gsKey == "Current State" )
						{
							int nEnum;
							Tools.ReadData(stream, nEnum);
							npc.m_State = (CNpc::e_State)nEnum;
						}
						break;
					case 'M':
						if ( gsKey == "Max Idle Time" )
							Tools.ReadData(stream, npc.m_fMaxIdleTime);
						break;
					case 'P':
						if ( gsKey == "Persona" )
						{
							gString gsPers;
							Tools.ReadData(stream, gsPers);
							npc.m_Persona = CNpc::GetPersona(gsPers);
						}
						break;
					default:
						g_Log.Log(LOG_ERROR, "[Npc::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/NPC]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CNpc::>>] Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CNpc::>>] Error encountered while reading %s\'s file..", npc.Name());
	}

	return stream;
}

bool CNpc::Load(gFileName gsRootDir)
{
	std::fstream fp;
	gFileName gsFile;

	gsFile.Format("%sNPCs\\%d.npc", gsRootDir, Vnum());

	fp.open(gsFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp >> *this;
	//fp.unlock();
	fp.close();

	return true;
}


bool CNpc::Save(gFileName gsRootDir)
{
	std::fstream fp;
	gFileName gsFile;

	gsFile.Format("%sNPCs\\%d.npc", gsRootDir, Vnum());

	fp.open(gsFile, ios::in|ios::out);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp << *this;
	//fp.unlock();
	fp.flush();
	fp.close();

	return true;
}

void CNpc::Attack()
{

}

void CNpc::FindSustenance()
{

}

void CNpc::Wander()
{
	CRoom* pRoom;
	CExit* pExit;
	int i;

	pRoom = CurrentRoom();

	if ( !pRoom )
		return;

	if ( !pRoom->Exits().empty() )
	{
		ExitList::iterator pos;
		int nCount = 0;
		for (pos = pRoom->Exits().begin(); pos != pRoom->Exits().end(); pos++,nCount++ )
			; // count the exits.

		i = rand()%nCount;
		pExit = pRoom->Exits()[i];
	}

	if ( pExit )
		HomeWorld()->MoveTo(this, pExit->m_Destination );
}

void CNpc::MoveTowardDest()
{
	// One of these days, I'll get off my ass and finish the A* code that
	// this needs....
}

bool CNpc::ChangeState(e_State eState)
{
	if ( m_pTarget )
	{
		m_State = _FIGHT;
		return ( eState == _FIGHT );
	}

	m_State = eState;
	return true;
	// return false if for some reason, you cant change the state...
}

void CNpc::Think(bool bForce)
{
	if ( !ActorStates()->IsSet(_THINKS) && !bForce )
		return;

	g_Log.Log(LOG_INFO, "[CNpc::Think] %s is thinking...\n", m_gsIdentifier);
}

void TestNavTo(CActor* pActor, CPlacement pl)
{
	CDirectionList List;
	CTimer tm;

	tm.Start();
	if (pActor->HomeWorld()->Navigator()->BuildPathTo(List, pActor, pl))
	{
		float fTimeTaken = tm.Stop();

		gString gsPath;
		gString gsDir;
		gString gsExit;
		int nSize = List.Count();

		while ( !List.AtEnd() )
		{
			gsExit = CExit::szExitNames[List.Next()];

			gsDir.Format("%s ", gsExit.Left(1));

			gsPath += gsDir;
		}
		gsPath.TrimSpacesRight();

		//g_Log.Log(LOG_INFO,
		//	"[PathFinding Test] Calc. Path to: #%2d in %d steps. List: %s Took %2.3fs\n",
		//	pl.Room(), nSize, gsPath, fTimeTaken);
	}
}

void CNpc::Update(bool bForce)
{
	static bool bTestPathfinding = false;
	static bool bTestProperties = false;

	float fThisUpdate = CGameObjects::Get().Clock();

	// Pathfinding test & example code. See the function "TestNavTo" to see how
	// to call the navigation functions and retreive the paths returned.
	if ( bTestPathfinding && rand() % 3 == 0 )
	{
		TestNavTo( this, CPlacement(1,15, 0) );
		TestNavTo( this, CPlacement(1,16, 0) );
		TestNavTo( this, CPlacement(1, 7, 0) );
		TestNavTo( this, CPlacement(1,11, 0) );
		TestNavTo( this, CPlacement(1,18, 0) );
		TestNavTo( this, CPlacement(1,20, 0) );
		TestNavTo( this, CPlacement(1,22, 0) );
		TestNavTo( this, CPlacement(1,21, 0) );
	}

	if ( bTestProperties )
	{
/*		CPlacement p;
		long ID;
		gString gsDesc, gsName;
		float fIdle;
		
		Get("Position", p);
		Get("ID", ID);
		Get("Name", gsName);
		Get("MaxIdleTime", fIdle);

		SetName("TestMe");
		m_fMaxIdleTime = 1000.0f;

		Get("Name", gsName);
		Get("MaxIdleTime", fIdle);*/
	}

	if ( EventCount() > 0 && !ActorStates()->IsSet(_IGNORE_EVENTS) )
		HandleEvents();

	if ( !ActorStates()->IsSet(_UPDATES) && !bForce )
		return;

	if ( fThisUpdate - m_fLastUpdate < fNpcUpdateDelta)
		return;

	//g_Log.Log(LOG_INFO, "[CNpc::Update] Updating %s\n", m_gsIdentifier);

	switch ( m_State )
	{
		case _IDLE:
			{
				if ( m_IdlePeriod.Elapsed() >= m_fMaxIdleTime || (rand()%12 ==0))
				{
					m_IdlePeriod.Stop();
					ChangeState(_WANDER);
				}
			}
			break;
		case _WANDER:
			{
				Wander();
				m_IdlePeriod.Start();
				ChangeState(_IDLE);
			}
			break;
		case _FLEE:
			{
				Wander();
			}
			break;
		case _FIGHT:
			{
				Attack();
			}
			break;
		case _HUNGRY:
		case _THIRSTY:
			{
				FindSustenance();
			}
			break;
		case _GUARD:
			{
			}
			break;
	}

	m_fLastUpdate = fThisUpdate;
}

////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool CNpc::HandleEvent(CEvent& Event)
{
	switch ( Event.m_EventCode )
	{
		case EV_DAMAGED:
			return OnDamage(Event);
		default:
			return CActor::HandleEvent(Event);
	}

	return false;
}

/////////////////////////////////////////////////////////////////////////////////
// Event Handlers ///////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
bool CNpc::OnDamage(CEvent& Event)
{
	EDamage* pEvent = (EDamage*)(&Event);

	assert(pEvent);

	AdjCurrentHealth(pEvent->fAmount);

	// How's our health doing?
	if ( m_Health[CURRENT] < (m_Health[MAX]*.33) )
		ChangeState(_FLEE);

	else
	if ( pEvent->pInflictor ) // We've been injured by another actor
	{
		if ( Persona() == _MEEK )
			ChangeState(_FLEE);
		else
		{
			if ( !m_pTarget || m_TargetPos != Position() )
			{
				m_pTarget = pEvent->pInflictor;
				m_TargetPos = pEvent->pInflictor->Position();
			}

			ChangeState(_FIGHT);
		}
	}

	return false;
}

void CNpc::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CMobile::WriteXml(pParent);

	if ( m_pTarget )
	{
		unsigned int uiTarget = m_pTarget->GUID();
		Tools.WriteXml(pParent, "target_id", uiTarget);
	}
}

void CNpc::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CMobile::ReadXml(pParent);

	unsigned int uiTarget = 0;
	if (Tools.ReadXml(pParent, "target_id", uiTarget))
		m_pTarget = CGameObjects::Get().GetActor(uiTarget);
}