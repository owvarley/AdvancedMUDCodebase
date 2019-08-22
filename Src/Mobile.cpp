//
// MudCore
//
// MudCore is copyright (c) 2000, 2006 by Gary McNickle
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
// CMobile Class Implementation
//
// CMobile is an intermediate class for actors that can move and think.
//


// Get rid of that pesky stl warning about std::containers...
#pragma warning(disable:4786)

#include <ctime>
#include <fstream>

#include "MudCore.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "Actor.h"
#include "Mobile.h"
#include "Socket.h"
#include "PathFinding.h"
#include "Attributes.h"

#include <../gTools/Tools.h>
#include <../gTools/Placement.h>

using namespace std;


char* CMobile::szMobileGender[] = {"Male", "Female", "Neutral", NULL};


//
// The core actor constructor.
//
CMobile::CMobile()
{
	m_uiAntagonist	= 0;
	m_iLevel		= 0;
	m_nGender		= 0;

	/*
	CAttribute& Health = m_Attributes["health"];
	CAttribute& Constitution = m_Attributes["con"];

	Health.Set(0, 100, 100);
	Constitution.Set(0, 30, 20);
	*/

	m_gsTitle		= "";
	m_fWeight		= 1.0f;
	m_pWatchActor	= NULL;
	m_gsIdentifier.Format("Mobile_%d", m_uiUniqueID);
	m_CurrentRoom	= NULL;

	Register("title", &m_gsTitle);
	Register("race", &m_gsRace);
	Register("gender", &m_nGender);

}


CMobile::~CMobile()
{
	if ( m_pHomeWorld )
	{
		m_pHomeWorld->Remove(this);
		m_pHomeWorld = NULL;
	}

	m_Attributes.Clear();
	m_CurrentRoom	= NULL;
}

// Copy constructor...
CMobile::CMobile(const CMobile& a)
{
	CActor::CActor(a);

	m_gsTitle		= a.m_gsTitle;
	*m_pWatchActor	= *a.m_pWatchActor;
	m_uiAntagonist	= a.m_uiAntagonist;
	m_gsRace		= a.m_gsRace;
	m_nGender		= a.m_nGender;
	// attributes
	// inventory
}

CMobile& CMobile::operator = (const CMobile& a)
{
	CActor::operator = ((CActor)a);

	m_gsTitle		= a.m_gsTitle;
	m_uiAntagonist  = a.m_uiAntagonist;
	m_pWatchActor	= new CActor(*a.m_pWatchActor);
	m_gsRace		= a.m_gsRace;
	m_nGender		= a.m_nGender;
	// attributes
	// inventory

	return *this;
}

void CMobile::WriteXml(TiXmlNode* pParent)
{
	CActor::WriteXml(pParent);
}

void CMobile::ReadXml(TiXmlNode* pParent)
{
	CActor::ReadXml(pParent);
}

bool CMobile::SetRace(const gString& gsRace)
{
	m_gsRace = gsRace;
	return true;
}

bool CMobile::SetGender(const gString& gsGender)
{
	bool bFound = false;

	for (int i = 0; i < CMobile::_MAXGENDER; i++)
	{
		if (gsGender == (const char*)CMobile::szMobileGender[i])
		{
			m_nGender = i;
			bFound = true;
		}
	}

	return bFound;
}


bool CMobile::SetGender(const int nGender)
{
	m_nGender = nGender;
	return true;
}




// A 'WatchActor' is an actor who is currently observing this actor. Much like
// a spirit that has possessed the actor, they see and hear everything this watched
// actor sees or hears.
bool CMobile::SetWatchActor(CActor* pWatcher)
{
	// Note: No validation done here!
	m_pWatchActor = pWatcher;

	return (m_pWatchActor != NULL);
}

// Describe this actor to another actor.
void CMobile::DescribeTo(CActor* pA, bool bFull) const
{
	CActor::DescribeTo(pA, bFull);
}

// Display a string of formatted text to the actor.
int CMobile::Write(char *fmt, ...)
{
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	if ( m_pWatchActor && m_pWatchActor->IsPlayer() )
	    (CPlayer*)m_pWatchActor->Write("(%s) %s", (const char*)Name(), buf);

	return length;
}


//
// This function should be used to process any logical reasoning the actor
// may need to make.
//
void CMobile::Think(bool bForce)
{
	if ( !ActorStates()->IsSet(_THINKS) && !bForce )
		return;
}

void CMobile::Update(bool bForce)
{
	float fThisUpdate = CGameObjects::Get().Clock();

	if ( EventCount() > 0 && !ActorStates()->IsSet(_IGNORE_EVENTS) )
		HandleEvents();

	if ( !ActorStates()->IsSet(_UPDATES) && !bForce )
		return;

	// Only update every 3 seconds or if forced
	if ( bForce || (fThisUpdate - m_fLastUpdate >= fActorUpdateDelta) )
	{
		//CTools::Get().Report(E_INFO, "[CActor::Update] Performing Actor Update.\n");
		m_fLastUpdate = fThisUpdate;
	}
}

bool CMobile::Save()
{
	return CActor::Save();
}

bool CMobile::Load()
{
	return CActor::Load();
}

bool CMobile::ExecuteCommand(const gString& gsCmd, char* szArgs,...)
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

	gsCommandLine.Format("%s %s", (const char*)gsCmd, buf);

	return m_pHomeWorld->Interpret(this, gsCommandLine);
}


bool CMobile::FollowPath(CDirectionList& List, float fTime)
{
	bool bDone = false;

	if ( List.Count() == 0 || List.AtEnd() )
		return true;

	while ( !List.AtEnd() && !bDone )
	{
		CExit::_Directions Dir = (CExit::_Directions)List.Next();
		gString gsDir = CExit::szExitNames[Dir];

		if ( !ExecuteCommand("move", gsDir) )
			return false;
		else
		{
			time_t FutureTime = clock() + (clock_t)(0.5 * CLOCKS_PER_SEC);
			EMoveToNextRoom* pEvent = new EMoveToNextRoom();

			pEvent->m_pList = &List;
			pEvent->SetExecutionTime((clock_t)FutureTime);
			ReceiveEvent(*pEvent);
		}

	}

	return false;
}

// Returns the skills rating
int CMobile::GetSkill(int nSkill)
{
	SkillMap::iterator find = m_Skills.find(nSkill);

	if (find != m_Skills.end())
	{
		return (*find).second;
	}
	else
	{
		return 0;
	}
}

int CMobile::GetSkill(gString gsSkill)
{
	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	SkillIndex::iterator find = pEpacs->SkillsIndex().find(gsSkill);

	if (find != pEpacs->SkillsIndex().end())
	{
		return GetSkill((*find).second);
	}
	else
	{
		return 0;
	}
}

// Complicated function, has to handle skills modify ability values and
// also sub-skills modifying abilitie value
// For example:
// * Combat [1]
// Adding the Skill, Blaster Weapons will make this:
// * Combat [2]
// Having all three Skills will make it
// * Combat [4]
// Having the second Subskill for each skill (e.g. Improved Blaster Weapons, Improved Close Combat, etc)
// * Combat [5]
// Having the third Subskill for each skill makes this:
// * Combat [6]
// Having the fourth Subskill for each skill makes this:
// * Combat [7]
bool CMobile::AddSkill(int nSkill)
{
	SkillMap::iterator find = m_Skills.find(nSkill);
	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	// If they know this skill already, we return false
	if (find != m_Skills.end())
	{
		if ((*find).second > 0)
		{
			return false;
		}
		else
		{
			// For some reason they have the skill at a rating of zero or less, so we delete it
			m_Skills.erase(find);
		}
	}

	// Add the skill in and modify the Ability
	CSkill* pSkill = pEpacs->GetSkill(nSkill);

	if ( pSkill )
	{
		m_Skills[ nSkill ] = 1;	// Add the skill

		CSkill* pAbility = pEpacs->GetSkill(pSkill->Prereq());

		if (!pAbility)
			return false;

		// Locate the ability for this skill
		while ( pAbility->Type() != CSkill::_ABILITY )
		{
			// Move up the list of prequisites until we reach our ability
			pAbility = pEpacs->GetSkill(pAbility->Prereq());
		}

		// Got the ability now, increase it by one and end if the skill added is NOT 
		// a subskill
		if ( pSkill->Type() != CSkill::_SUBSKILL )
		{
			// Increase the ability by one
			m_Skills[ pAbility->Id() ] += 1;
		}
		else
		{
			// If this subskill's prerequisite is an Ability, we need to increase
			// our ability by one
			if ( pSkill->Prereq() == pAbility->Name() )
			{
				m_Skills[ pAbility->Id() ] += 1;
			}

			SkillMap smSubskills;	// Will hold the level for each subskill

			// Iterate through each Child of the Ability and check which subskills have been
			// already added to the player
			for (IntegerList::iterator it = pAbility->Children().begin(); it != pAbility->Children().end(); it++)
			{
				CSkill* pSubSkill = pEpacs->GetSkill(*it);

				if (pSubSkill->Type() != CSkill::_SUBSKILL)
					continue;

				int nSubSkillMain = (*it);	// Save the primary subskill so we can store this value in our map

				while( pSubSkill != NULL )
				{
					// Check they know the subskill
					if (GetSkill(pSubSkill->Id()) <= 0)
						break;
					else
						smSubskills[ nSubSkillMain ] = smSubskills[ nSubSkillMain ] + 1;

					if (pSubSkill->Children().size() <= 0)
						break;

					// Iterate through its children
					// ASSUMPTION: Each subskill will only ever have a single subskill below it
					for (IntegerList::iterator child = pSubSkill->Children().begin(); child != pSubSkill->Children().end(); child++)
					{
						if ((pEpacs->GetSkill(*child))->Type() != CSkill::_SUBSKILL)
						{
							continue;
						}
						else
						{
							pSubSkill = pEpacs->GetSkill((*child));
							break;
						}
					}
				}
			}

			// Now our SkillMap is complete, we can check if adding this skill gives a bonus to the ability
			int nBonus   = 1;
						
			for (SkillMap::iterator sk = smSubskills.begin(); sk != smSubskills.end(); sk++)
			{
				// Set our lowest value
				if (sk == smSubskills.begin())
					nBonus = (*sk).second;

				if ((*sk).second < nBonus)
					nBonus = (*sk).second;
			}

			// Check the current ability rating
			int nCurrRating = m_Skills[ pAbility->Id() ];

			// If our increased ability is less than what it should be
			// we increase it by one
			if ((nCurrRating - 4) < (nBonus-1) && (nCurrRating - 4) >= 0)
			{
				m_Skills[ pAbility->Id() ] += 1;
			}
		}		


	}
}

bool CMobile::AddSkill(gString gsSkill)
{
	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	SkillIndex::iterator find = pEpacs->SkillsIndex().find(gsSkill);

	if (find != pEpacs->SkillsIndex().end())
	{
		return AddSkill((*find).second);
	}
	else
	{
		return false;
	}
}


// Reversed AddSkill function - I feel dirty
bool CMobile::RemoveSkill(int nSkill)
{
	SkillMap::iterator find = m_Skills.find(nSkill);
	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	// If they don't know this skill already, we return false
	if (find != m_Skills.end())
	{
		if ((*find).second <= 0)
		{
			return false;
		}
	}
	else
	{
		return false;
	}

	// Remove the skill in and modify the Ability
	CSkill* pSkill = pEpacs->GetSkill(nSkill);

	if ( pSkill )
	{
		(*find).second = (*find).second - 1;

		CSkill* pAbility = pEpacs->GetSkill(pSkill->Prereq());

		if (!pAbility)
			return false;

		// Locate the ability for this skill
		while ( pAbility->Type() != CSkill::_ABILITY )
		{
			// Move up the list of prequisites until we reach our ability
			pAbility = pEpacs->GetSkill(pAbility->Prereq());
		}

		// Got the ability now, decrease it by one and end if the skill added is NOT 
		// a subskill
		if ( pSkill->Type() != CSkill::_SUBSKILL )
		{
			// Decrease the ability by one
			m_Skills[ pAbility->Id() ] -= 1;
		}
		else
		{
			// If this subskill's prerequisite is an Ability, we need to decrease
			// our ability by one
			if ( pSkill->Prereq() == pAbility->Name() )
			{
				m_Skills[ pAbility->Id() ] -= 1;
			}

			SkillMap smSubskills;	// Will hold the level for each subskill

			// Remove any children that this skill unlocked
			for (IntegerList::iterator it = pSkill->Children().begin(); it != pSkill->Children().end(); it++)
			{
				SkillMap::iterator erase = m_Skills.find(*it);

				// They have this subskill
				if (erase != m_Skills.end())
				{
					// Remove the subskill
					(*erase).second = 0;
				}
			}


			// Iterate through each Child of the Ability and check which subskills have been
			// already added to the player
			for (IntegerList::iterator it = pAbility->Children().begin(); it != pAbility->Children().end(); it++)
			{
				CSkill* pSubSkill = pEpacs->GetSkill(*it);

				if (pSubSkill->Type() != CSkill::_SUBSKILL)
					continue;

				int nSubSkillMain = (*it);	// Save the primary subskill so we can store this value in our map

				while( pSubSkill != NULL )
				{
					// Check they know the subskill
					if (GetSkill(pSubSkill->Id()) <= 0)
						break;
					else
						smSubskills[ nSubSkillMain ] = smSubskills[ nSubSkillMain ] + 1;

					if (pSubSkill->Children().size() <= 0)
						break;

					// Iterate through its children
					// ASSUMPTION: Each subskill will only ever have a single subskill below it
					for (IntegerList::iterator child = pSubSkill->Children().begin(); child != pSubSkill->Children().end(); child++)
					{
						if ((pEpacs->GetSkill(*child))->Type() != CSkill::_SUBSKILL)
						{
							continue;
						}
						else
						{
							pSubSkill = pEpacs->GetSkill((*child));
							break;
						}
					}
				}
			}

			// Now our SkillMap is complete, we can check if adding this skill gives a bonus to the ability
			int nBonus   = 1;
						
			for (SkillMap::iterator sk = smSubskills.begin(); sk != smSubskills.end(); sk++)
			{
				// Set our lowest value
				if (sk == smSubskills.begin())
					nBonus = (*sk).second;

				if ((*sk).second < nBonus)
					nBonus = (*sk).second;
			}

			// Check the current ability rating
			int nCurrRating = m_Skills[ pAbility->Id() ];

			// If our increased ability is greater than what it should be
			// we decrease it by one
			if ((nCurrRating - 4) > (nBonus-1) && (nCurrRating - 4) >= 0)
			{
				m_Skills[ pAbility->Id() ] -= 1;
			}
		}		


	}
	return false;
}

bool CMobile::RemoveSkill(gString gsSkill)
{
	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	SkillIndex::iterator find = pEpacs->SkillsIndex().find(gsSkill);

	if (find != pEpacs->SkillsIndex().end())
	{
		return RemoveSkill((*find).second);
	}
	else
	{
		return false;
	}
	return false;
}


//
// Note that the default actor comparision is an EXACT comparision. It will return
// false if two actors are identical in every way, but have different uniqueIDs.
//
// This, and it's partner operator != will very likely need to be overridden for
// subclasses of actor.
//
bool CMobile::operator == (const CMobile& a2)
{
	return ( m_uiUniqueID == a2.m_uiUniqueID );
}

bool CMobile::operator != (const CMobile& a2)
{
	return !( *this == a2 );
}


////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool	CMobile::HandleEvent(CEvent& Event)
{
	bool bHandled = false;

	if ( ActorStates()->IsSet(_IGNORE_EVENTS) )
		return false;

	switch ( Event.m_EventCode )
	{
		case EV_DAMAGED:
			bHandled = OnDamage(Event);
			break;
		case EV_MOVE_TO_NEXT_ROOM:
			bHandled = OnMoveToNextRoom(Event);
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
bool CMobile::OnDamage(CEvent& Event)
{
	EDamage* pEvent = static_cast<EDamage*>(&Event);

	if ( pEvent )
		return AdjCurrentHealth((int)(pEvent->fAmount));

	return false;
}

bool CMobile::OnMoveToNextRoom(CEvent& Event)
{
	EMoveToNextRoom* pMove = static_cast<EMoveToNextRoom*>(&Event);

	return FollowPath(*pMove->m_pList, pMove->m_fTime);
}


/////////////////////////////////////////////////////////////////////////////////
// Mobile Events /////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
EMoveToNextRoom::EMoveToNextRoom() :
  CEvent(CActor::EV_MOVE_TO_NEXT_ROOM), m_pList(new CDirectionList), m_fTime(0.5f)
{
}

EMoveToNextRoom::~EMoveToNextRoom()
{
	delete m_pList;
}

