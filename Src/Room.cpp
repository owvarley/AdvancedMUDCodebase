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

// Class    :: CRoom, CExit, CReset
// Header   :: Room.h
// Function :: The entire world of a MudCore system is really broken down into Rooms.
//			:: Each step a Player or Npc takes moves them from one room to another. 
//			:: Rooms then are really the building blocks of your worlds. This is common
//			:: on many mud systems. Rooms keep track of the Actors that they contain 
//			:: (In separate lists for Items and other Actors), they commonly have a series
//			:: of Exits leading off to other rooms and they can receive Events.

#pragma warning(disable: 4786)

#include "MudCore.h"
#include "GameObjects.h"
#include "GameWorld.h"
#include "GameServer.h"
#include "Tools.h"
#include "Npc.h"
#include "Reset.h"
#include "Room.h"
#include "../gTools/Log.h"

UINT uiUniqueExitID   = 0;
UINT uiUniqueRoomID   = 0;
UINT uiUniqueParentID = 0;

char* CExit::szDoorFlags[CExit::_NUMDOORFLAGS] = {"Open", "Closed", "Locked", "Swim", "Climb"};
char* CExit::szExitNames[CExit::NUMEXITS] = {"", "North", "South", "East", "West", "Up", "Down", "Northwest", "Northeast", "Southwest", "Southeast"};
char* CReset::szResetTypes[CReset::_NUMRESETTYPES] = {"Object", "Npc"};
char* CRoom::szRoomFlags[] = {"Obscured", "Hidden", "Nomob", "Turbolift", "Indoors", NULL};
char* CRoom::szBartle[] = {"Travel", "Lethal", "Social", "Adventure", NULL };
char* CRoom::szCoverTypes[] = {"Sparse", "Light", "Medium", "Dense", "Heavy", "Very Heavy", NULL };
char* CRoom::szTerrainTypes[] = {"Urban", "Desert", "Jungle", "Ice", "Space", "Swamp", "Mountain", "Water", "Volcanic", NULL };

CExit::CExit()
{
	uiUniqueExitID++;
	m_Direction = NORTH;
	m_nComplexity = 0;
	m_nStrength = 0;
	m_Flags = new CSet();
	m_Flags->SetBit(_DoorFlags(_OPEN));
	m_gsIdentifier.Format("Exit_%d", uiUniqueExitID);
}

CExit::~CExit()
{
	uiUniqueExitID--;
	delete m_Flags;
	m_Flags = NULL;
	m_gsIdentifier += "_Deleted";
}

CExit& CExit::operator = (CExit& exit)
{
	m_Destination = exit.m_Destination;
	*m_Flags = *exit.m_Flags;
	m_Direction = exit.m_Direction;
	m_fLastUpdate = exit.m_fLastUpdate;

	return *this;
}

void CExit::Update(bool bForce)
{
}

CExit::_Directions CExit::GetDirection(gString gsDir)
{
	if ( gsDir.IsEmpty() )
		return CExit::NORTH;

	gsDir.MakeUpper();

	for (int n=1; n<=CExit::NUMEXITS-1; n++)
	{
		if ( gsDir == (gString)CExit::szExitNames[n] )
			return (_Directions)n;
	}

	return CExit::NORTH;
}

void CExit::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "direction",	gString(CExit::szExitNames[m_Direction]));
	if (m_nComplexity > 0)
		Tools.WriteXml(pParent, "Complexity",	m_nComplexity);
	if (m_nStrength > 0)
		Tools.WriteXml(pParent, "Strength",		m_nStrength);
	Tools.WriteXml(pParent, "destination",	m_Destination);
	Tools.WriteXml(pParent, "flags",		*m_Flags);
}

void CExit::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsDir;
	Tools.ReadXml(pParent, "Complexity",	m_nComplexity);
	Tools.ReadXml(pParent, "Strength",		m_nStrength);
	Tools.ReadXml(pParent, "direction",		gsDir);
	Tools.ReadXml(pParent, "destination",	m_Destination);
	Tools.ReadXml(pParent, "flags",			*m_Flags);

	m_Direction = CExit::GetDirection(gsDir);
}

//////////////////////////
// Resets
//////////////////////////


void CReset::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlElement* pReset = static_cast<TiXmlElement*>(pParent);
	
	pReset->SetAttribute("type",	(int)m_Type);
	pReset->SetAttribute("vnum",	m_iVnum);
	pReset->SetAttribute("lrange",	m_iRange[0]);
	pReset->SetAttribute("urange",	m_iRange[1]);

	return;
}

void CReset::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlElement* pReset = static_cast<TiXmlElement*>(pParent);

	pReset->Attribute("type",	(int*)&m_Type);
	pReset->Attribute("vnum",	&m_iVnum);
	pReset->Attribute("lrange",	&m_iRange[0]);
	pReset->Attribute("urange",	&m_iRange[1]);

	return;
}

CRoom::CRoom()
{
   _Reset();
	uiUniqueRoomID++;
	m_gsIdentifier.Format("Room_%d", uiUniqueRoomID);
	m_RoomFlags = new CSet;

	Register("name",			&m_gsName);
	Register("description",		&m_gsDescription);
	Register("illumination",	&m_gsIllumination);
	Register("identifier",		&m_gsIdentifier);
	Register("position",		&m_Position);
	Register("parent",			&m_nParent);
	Register("size",			&m_nSize);
	Register("cover",			&m_nCover);
	Register("bartle",			&m_nBartle);
	Register("terrain",			&m_nTerrain);
	
}


CRoom::~CRoom()
{
	m_Exits.clear();
	m_Actors.clear();
	m_Resets.clear();
	m_Components.clear();
	m_Ships.clear();
	
	delete m_RoomFlags;
	m_RoomFlags = NULL;

	m_gsIdentifier += "_Deleted";
}

CRoom& CRoom::operator = (CRoom& room)
{
	m_gsName = room.m_gsName;
	m_gsDescription = room.m_gsDescription;
	m_gsIllumination = room.m_gsIllumination;
	m_Position = room.m_Position;
	m_nSize = room.m_nSize;
	m_nCover = room.m_nCover;
	m_nBartle = room.m_nBartle;
	m_nTerrain = room.m_nTerrain;

	// Roomflags

	if (!m_RoomFlags)
		m_RoomFlags = new CSet;

	*m_RoomFlags = *room.m_RoomFlags;

	// Lore
	for (gStringImap::iterator gstr = room.m_Lore.begin(); gstr != room.m_Lore.end(); gstr++)
	{
		m_Lore.insert(gStringImap::value_type((*gstr).first, (*gstr).second));
	}

	// Force
	for (gStringImap::iterator gstr = room.m_Force.begin(); gstr != room.m_Force.end(); gstr++)
	{
		m_Force.insert(gStringImap::value_type((*gstr).first, (*gstr).second));
	}

	// Perception
	for (gStringImap::iterator gstr = room.m_Perception.begin(); gstr != room.m_Perception.end(); gstr++)
	{
		m_Perception.insert(gStringImap::value_type((*gstr).first, (*gstr).second));
	}


	// Handle components
	for (gStringList::iterator it = room.m_Components.begin(); it != room.m_Components.end(); it++)
	{
		gString gsNew = *it;
		m_Components.push_back(gsNew);
	}

	// Handle exits
	for (ExitList::iterator Exit = room.m_Exits.begin(); Exit != room.m_Exits.end(); Exit++)
	{
		CExit* pExit = new CExit;
		*pExit = *(*Exit);
		m_Exits.push_back(pExit);
	}
	
	// Handle resets
	for (ResetList::iterator reset = room.m_Resets.begin(); reset != room.m_Resets.end(); reset++)
	{
		CReset* pReset = new CReset;
		*pReset = *(*reset);
		m_Resets.push_back(pReset);
	}

	return *this;
}

void CRoom::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CAtomic::WriteXml(pParent);
	
	// Save the additional desc components
	
	// Lore
	for (gStringImap::iterator gstr = m_Lore.begin(); gstr != m_Lore.end(); gstr++)
	{
		TiXmlNode* pLoreNode = Tools.InsertXmlChild(pParent, "Lore");
		Tools.WriteXml(pLoreNode, "difficulty",	(int)(*gstr).first);
		Tools.WriteXml(pLoreNode, "description", (*gstr).second);
	}

	// Force
	for (gStringImap::iterator gstr = m_Force.begin(); gstr != m_Force.end(); gstr++)
	{
		TiXmlNode* pLoreNode = Tools.InsertXmlChild(pParent, "Force");
		Tools.WriteXml(pLoreNode, "difficulty",	(int)(*gstr).first);
		Tools.WriteXml(pLoreNode, "description", (*gstr).second);
	}

	// Perception
	for (gStringImap::iterator gstr = m_Perception.begin(); gstr != m_Perception.end(); gstr++)
	{
		TiXmlNode* pLoreNode = Tools.InsertXmlChild(pParent, "Perception");
		Tools.WriteXml(pLoreNode, "difficulty",	(int)(*gstr).first);
		Tools.WriteXml(pLoreNode, "description", (*gstr).second);
	}

	Tools.WriteXml(pParent, "RoomFlags",		*m_RoomFlags);

	int nCount = 0;

	if ( m_Exits.size() > 0 )
	{
		for (nCount=0; nCount<m_Exits.size(); nCount++)
		{
			TiXmlNode* pExit = Tools.InsertXmlChild(pParent, "Exit");
			m_Exits[nCount]->WriteXml(pExit);
		}
	}

	for (nCount=0; nCount<m_Resets.size(); nCount++)
	{
		TiXmlNode* pReset = Tools.InsertXmlChild(pParent, "reset");
		m_Resets[nCount]->WriteXml(pReset);
	}

	// If the Room has components installed we save them
	for (gStringList::iterator it = m_Components.begin(); it != m_Components.end(); it++)
	{
		Tools.WriteXml(pParent, "component", (*it));
	}
}

void CRoom::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CAtomic::ReadXml(pParent);

	// Read in DESC components

	// LORE
	TiXmlNode* pLoreNode = pParent->FirstChild("Lore");
	while ( pLoreNode != NULL )
	{
		int nDifficulty;
		gString gsDesc;

		Tools.ReadXml(pLoreNode, "difficulty",		nDifficulty);
		Tools.ReadXml(pLoreNode, "description",		gsDesc);

		m_Lore[ nDifficulty ] = gsDesc;
		pLoreNode = pLoreNode->NextSibling("Lore");
	}

	// FORCE
	TiXmlNode* pForceNode = pParent->FirstChild("Force");
	while ( pForceNode != NULL )
	{
		int nDifficulty;
		gString gsDesc;

		Tools.ReadXml(pForceNode, "difficulty",		nDifficulty);
		Tools.ReadXml(pForceNode, "description",	gsDesc);

		m_Force[ nDifficulty ] = gsDesc;
		pForceNode = pForceNode->NextSibling("Force");
	}

	// PERCEPTION
	TiXmlNode* pPerceptionNode = pParent->FirstChild("Perception");
	while ( pPerceptionNode != NULL )
	{
		int nDifficulty;
		gString gsDesc;

		Tools.ReadXml(pPerceptionNode, "difficulty",	nDifficulty);
		Tools.ReadXml(pPerceptionNode, "description",	gsDesc);

		m_Perception[ nDifficulty ] = gsDesc;
		pPerceptionNode = pPerceptionNode->NextSibling("Perception");
	}


	Tools.ReadXml(pParent, "RoomFlags",		*m_RoomFlags);

	TiXmlNode* pNode = pParent->FirstChild("Exit");
	while ( pNode != NULL )
	{
		CExit* pExit = new CExit;
		pExit->ReadXml(pNode);
		m_Exits.push_back(pExit);

		pNode = pNode->NextSibling("Exit");
	}

	pNode = pParent->FirstChild("reset");
	while ( pNode != NULL )
	{
		CReset* pReset = new CReset;
		pReset->ReadXml(pNode);
		m_Resets.push_back(pReset);

		pNode = pNode->NextSibling("reset");
	}

	// Read in components
	pNode = pParent->FirstChild("component");
	while ( pNode != NULL )
	{
		gString gsName = pNode->FirstChild()->Value();
		AddComponent(gsName);
		pNode = pNode->NextSibling("component");
	}
	
}

void CRoom::_Reset()
{
	m_gsName = "";
	m_gsDescription = "";
	m_Position.Set(0,0,0);
	m_fLastUpdate = CGameObjects::Get().Clock();
	m_Actors.clear();
	m_Items.clear();
}


void CRoom::Remove(CActor *pA)
{
	ActorMap::iterator pos = m_Actors.find(pA->GUID());

	if ( pos != m_Actors.end() )
		m_Actors.erase(pos);
}

void CRoom::Add(CActor* pA)
{
	m_Actors.insert(ActorMap::value_type(pA->GUID(), pA));
}

void CRoom::AddShip(CShip* pShip)
{
	this->m_Ships.push_back(pShip->m_gsName);
}

bool CRoom::RemShip(gString gsShip)
{
	int nCount = 0;
	gStringList::iterator shi;

	m_Ships.remove(gsShip);

	return true;
}

CExit*	CRoom::GetExit(CExit::_Directions Dir)
{
	ExitList::iterator pos;
	CExit *pE = NULL;

	for (pos = Exits().begin(); pos != Exits().end(); pos++)
	{
		pE = (CExit*)(*pos);
		if ( pE && pE->m_Direction == Dir )
			return pE;
	}

	return NULL;
}

//
// Return the first direction that leads to the proper room or NONE
//
gString	CRoom::FindDirection(CPlacement v)
{
	ExitList::iterator pos;
	CExit *pE = NULL;
	CExit::_Directions Dir = CExit::NONE;

	for (pos = Exits().begin(); pos != Exits().end(); pos++)
	{
		pE = (CExit*)(*pos);
		if ( pE && pE->m_Destination == v )
		{
			Dir = pE->m_Direction;
			break;
		}
	}


	switch ( Dir )
	{
		case CExit::NORTH:
			return ("north");
			break;
		case CExit::SOUTH:
			return ("south");
			break;
		case CExit::EAST:
			return ("east");
			break;
		case CExit::WEST:
			return ("west");
			break;
		case CExit::UP:
			return ("above");
			break;
		case CExit::DOWN:
			return ("below");
			break;
		case CExit::NORTHWEST:
			return ("northwest");
			break;
		case CExit::NORTHEAST:
			return ("northeast");
			break;
		case CExit::SOUTHWEST:
			return ("southwest");
			break;
		case CExit::SOUTHEAST:
			return ("southeast");
			break;

		default:
			return "";
			break;
	}
}


//
// Return the first actor we find that matches given name
//
CActor*	CRoom::FindFirstActor(gString sWhat)
{
	ActorMap::iterator pos;
	CActor* pA = NULL;

	if ( !this )
		return NULL;

	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		pA = (CActor*)((*pos).second);

		if ( pA )
		{
			gString sName = pA->Name();

			sWhat.MakeUpper();
			sName.MakeUpper();

			if ( sName.Find(sWhat) != -1 )
				return pA;
		}
	}

	return NULL;
}

//
// Return the first item we find that matches given name
//
CItem*	CRoom::FindFirstItem(gString sWhat)
{
	ItemList::iterator pos;
	CItem* pI = NULL;

	for (pos = m_Items.begin(); pos != m_Items.end(); pos++)
	{
		pI = (CItem*)(*pos);
		if ( pI )
		{
			gString sName = pI->Name();
			sName.MakeUpper();
			sWhat.MakeUpper();
			if ( sName.HasPrefix(sWhat))
				return pI;
		}
	}

	return NULL;
}

//
// Return the first Module in the room that matches the given name
CModule* CRoom::FindFirstModule(gString gsModule)
{
	bool bFound = false;
	gString gsName;

	// Work through each Component to find a matching module
	for (gStringList::iterator it = this->m_Components.begin(); it != this->m_Components.end(); it++)
	{
		CComponent* pComp = this->GetComp(*it);

		if (pComp)
		{
			CModule* pMod = pComp->Get(gsModule);

			if (pMod)
				return pMod;
		}
	}

	return NULL;
}




//
// Return the number of NPCs that match this vnum in this room...
//
int CRoom::NPCCount(int iVnum)
{
	ActorMap::iterator pos;
	CNpc* pNpc = NULL;
	int iCount = 0;

	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		pNpc = (CNpc*)((*pos).second);

		if ( pNpc  && pNpc->Vnum() == iVnum )
			iCount++;
	}
	return iCount;
}

int CRoom::ObjectCount(int iVnum)
{
	ItemList::iterator pos;
	CItem* pItem = NULL;
	int iCount = 0;

	for (pos = m_Items.begin(); pos != m_Items.end(); pos++)
	{
		pItem = (CItem*)(*pos);

		if ( pItem && pItem->Vnum() == iVnum )
			iCount++;
	}
	return iCount;
}

ModuleList* CRoom::GetMod(int type)
{
	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->Area());
	ModuleList* mlMods = new ModuleList;

	if (!pArea)
		return mlMods;

	CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(pArea->Ship());

	if (!pShip)
		return mlMods;

	// We need to check each component to see if it contains a module of this given type
	for (gStringList::iterator it = this->m_Components.begin(); it != this->m_Components.end(); it++)
	{
		CComponent* pComp = pShip->GetComponent(*it);

		if (pComp)
		{
			CModule* pMod = pComp->Get(type);

			if (pMod)
				mlMods->push_back(pMod);
		}
	}

	return mlMods;
}

// Returns a list of all modules that match this name
ModuleList* CRoom::GetMod(gString gsName)
{
	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->Area());
	ModuleList* mlMods = new ModuleList;

	if (!pArea)
		return mlMods;

	CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(pArea->Ship());

	if (!pShip)
		return mlMods;

	// We need to check each component to see if it contains a module with this given name
	for (gStringList::iterator it = this->m_Components.begin(); it != this->m_Components.end(); it++)
	{
		CComponent* pComp = pShip->GetComponent(*it);

		if (pComp)
		{
			CModule* pMod = pComp->Get(gsName);

			if (pMod)
				mlMods->push_back(pMod);
		}
	}

	return mlMods;
}

// Returns a list of components installed in this room that match this type
ComponentList* CRoom::GetComp(int nType)
{
	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->Area());
	ComponentList* clComponents = new ComponentList;

	if (!pArea)
		return clComponents;

	CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(pArea->Ship());

	if (!pShip)
		return clComponents;

	// We need to check each component to see if it contains a module with this given name
	for (gStringList::iterator it = this->m_Components.begin(); it != this->m_Components.end(); it++)
	{
		CComponent* pComp = pShip->GetComponent(*it);

		if (pComp && pComp->m_Type == nType)
		{
			clComponents->push_back(pComp);
		}
	}

	return clComponents;

}

// Returns a component matching the name supplied
CComponent* CRoom::GetComp(gString gsName)
{
	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->Area());

	if (!pArea)
		return NULL;

	CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(pArea->Ship());

	if (!pShip)
		return NULL;

	// We need to check each component to see if it contains a module with this given name
	for (gStringList::iterator it = this->m_Components.begin(); it != this->m_Components.end(); it++)
	{
		if ((*it) == gsName)
			return pShip->GetComponent(*it);
	}

	return NULL;

}

// Returns the Area of thie Room
CArea* CRoom::GetArea()
{
	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->Area());

	return pArea;
}

// Returns the Ship this Room is contained in, if any
CShip* CRoom::GetShip()
{
	CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(this->GetArea()->Ship());

	return pShip;
}

// Returns the Parent room of this Room
CParentRoom* CRoom::GetParent()
{
	CArea * pArea = GetArea();

	if (pArea)
	{
		return pArea->GetParentRoom(m_nParent);
	}
	else
	{
		return NULL;
	}
}

bool CRoom::ShipHere(gString gsShip)
{
	bool bFound = false;

	for (gStringList::iterator it = this->m_Ships.begin(); it != this->m_Ships.end(); it ++)
	{
		if ((*it).HasPrefix(gsShip))
		{
			bFound = true;
			break;
		}
	}

	return bFound;

}

void CRoom::Update(bool bForce)
{
	CGameObjects& Game = CGameObjects::Get();

	float fThisUpdate = Game.Clock();
	CGameWorld* pWorld= Game.GetWorld( Position().World() );

	if ( EventCount() > 0 )
		HandleEvents();

	// Only update rooms every 1/2 minute or if forced
	if ( bForce || (fThisUpdate - m_fLastUpdate >= fRoomUpdateDelta) )
	{
		ResetList::iterator ResetItor;
		ExitList::iterator ExitItor;

		CExit *pExit = NULL;
		CReset *pReset = NULL;

		int iCount=0;

		//g_Log.Log(LOG_INFO, "[CRoom::Update] Updating %s.\n", Identifier());
		m_fLastUpdate = fThisUpdate;

		// Update all exits...
		for (ExitItor = m_Exits.begin(); ExitItor != m_Exits.end(); ExitItor++)
		{
			pExit = (CExit*)(*ExitItor);
			if ( pExit )
				pExit->Update(bForce);
		}

		// Update all resets...
		for (ResetItor = m_Resets.begin(); ResetItor != m_Resets.end(); ResetItor++)
		{
			// Perform room resets
			pReset = (CReset*)(*ResetItor);

			if ( pReset )
			switch ( pReset->Type() )
			{
				case CReset::_OBJECT:
					{
						int i = 0;
						iCount = ObjectCount(pReset->Vnum());

						if ( (pReset->m_iRange[MAX] > 0 && pReset->m_iRange[MAX] <= iCount)
							|| pReset->m_iRange[MIN] < iCount )
							break;

						for ( i = 0; i < pReset->m_iRange[MIN]-iCount; i++ )
						{
							CItem* pItem = new CItem;
							pItem->SetVnum( pReset->Vnum() );

							if ( pItem->Load( pWorld->m_gsAreaListFile.AbsolutePath() ) )
							{
								g_Log.Log(LOG_INFO, "[CRoom::Update] Resetting %s into %s. Current count %d.\n",
									  pItem->Identifier(), Identifier(), iCount);

  							    pWorld->Add(pItem, Position());
							}
							else
							{
								g_Log.Log(LOG_WARNING, "[CRoom::Update] Failed to reset Item #%d into %s Unable to load Item from data file.\n", pReset->Vnum(), Identifier());
								delete pItem;
							}
						}
					}
					break;
				case CReset::_NPC:
					{
						int i = 0;
						iCount = NPCCount(pReset->Vnum());

						if ( (pReset->m_iRange[MAX] > 0 && pReset->m_iRange[MAX] <= iCount)
							|| pReset->m_iRange[MIN] < iCount )
							break;

						for ( i = 0; i < pReset->m_iRange[MIN]-iCount; i++ )
						{
							CNpc* pNpc = new CNpc();
							pNpc->SetVnum( pReset->Vnum() );

							if ( pNpc->Load( pWorld->m_gsAreaListFile.AbsolutePath() ) )
							{
								g_Log.Log(LOG_INFO, "[CRoom::Update] Resetting %s into %s. Current count %d.\n",
									  pNpc->Identifier(), Identifier(), iCount);

								pWorld->Add(pNpc, Position());
							}
							else
							{
								g_Log.Log(LOG_WARNING, "[CRoom::Update] Failed to reset NPC #%d into %s Unable to load NPC from data file.\n", pReset->Vnum(), Identifier());
								delete pNpc;
							}
						}
					}
					break;
			}
		}
	}
}


// Writes to everyone in the room but the actor
void CRoom::Write(CActor* pA, char *fmt, ...)
{
	ActorMap::iterator pos;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	// Give message to all children
	if (GetParent())
		GetParent()->Write(pA, "%s", buf);

	// Give message to the room, minus our actor
	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		if (pA == (*pos).second)
			continue;

		((*pos).second)->Write(buf);
	}
}


void CRoom::Write(char *fmt, ...)
{
	ActorMap::iterator pos;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	// Send message to all other children
	if (GetParent())
		GetParent()->Write(this, "%s", buf);

	// Display message to all actors within this room
	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		((*pos).second)->Write(buf);
	}
}

void CRoom::Write(int nScope, char *fmt, ...)
{
	ActorMap::iterator pos;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	if (nScope == _GLOBAL)
		Write(buf);

	// Display message to all actors within this room
	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		((*pos).second)->Write(buf);
	}
}

// Create our description, the description depends on:
// [1] - Lighting within the room
// [2] - Player force level
// [3] - Player's galactic lore skill
// [4] - Player's perception
gString CRoom::Description(CActor *pA)
{
	gString gsDescription;

	// [1] First of all we have to check the lighting levels
	// if there is no light we use the infrared description 
	// if they have infrared
	// #TODO# if (IsLit())
	// else
	gsDescription += m_gsDescription;

	// [2] Second we check their force level and see whether there are
	// any force messages they can receive
	// #TODO# After force stats are added

	// [3] Third we check if they have the galactic lore skill, their level
	// in this determines what they can and can't see
	// #TODO# After skills are coded add this

	// [4] Finally we look at the player's perception, comparing this to the
	// DC of the perception messages. We start at their perception and work
	// backwards to locate the highest flagged message that they can see.
	for (int i = pA->Perception(); i > 0; i--)
	{
		gStringImap::iterator find = m_Perception.find(i);

		if (find != m_Perception.end())
		{
			gsDescription += " " + (*find).second;	
			break;
		}

	}

	return gsDescription;
}

// /-----------\ The room desc will go in this bit here
// | NW N NE U | isn't it cool 
// |  W * E  | | it wraps around the compass!
// | SW S SE D | Miguel is therefore the worlds best
// \-----------/ Closet living in coding midget
gString CRoom::FormatDesc(gString gsDescription)
{
	// We will format room descriptions to 80 columns as per standard
	// should we want to change this or have it specified by a player,
	// this number here can be changed
	int nWidth = 80;

	// The compass takes up 12 units of width, and 4 lines, so for the 
	// first four lines our width is nWidth - 12
	int nCompassWidth = nWidth - 12;
	int nLineCount = 1;

	gString gsTemp = gsDescription;
	gString gsCurrent = "";
	gString gsFinal = "";

	// Array will store the exit details for easy reference using the CExit::szExitNames order
	// 000  == No exit
	// 700  == Exit
	// 201  == Locked
	// 200  == Closed
	// 001  == Hidden
	// 200  == Other parent room

	gString gsExit[CExit::NUMEXITS];

	char* szShortExit[] = {"", "N", "S", "E", "W", "U", "D", "NW", "NE", "SW", "SE"};

	for (int i = 0; i < CExit::NUMEXITS; i++)
	{
		gString gsEx;
		gsEx.Format("#001%s#700", szShortExit[i]);	
		gsExit[i] = gsEx;		
	}




	// Compile the compass
	for (ExitList::iterator exit = m_Exits.begin(); exit != m_Exits.end(); exit++)
	{
		int nExit = (*exit)->Direction();
		gString gsExitShort = szShortExit[nExit];

		CArea* pArea = CGameObjects::Get().GameWorld()->GetArea((*exit)->Destination().Area());
		CRoom* pRoom = pArea->GetRoom((*exit)->Destination());

		if (!pArea || !pRoom)
			continue;

		gsExit[nExit] = "#600" + gsExitShort;


		// Check the number
		if ((*exit)->ExitFlags()->IsSet(CExit::_LOCKED))
			gsExit[nExit] = "#201" + gsExitShort + "#700";
		else if ((*exit)->ExitFlags()->IsSet(CExit::_CLOSED))
			gsExit[nExit] = "#200" + gsExitShort + "#700";
		/*else if ((*exit)->ExitFlags()->IsSet(CExit::_HIDDEN))
			gsExit[nExit] = 4;*/
		else if (this->Parent() != pRoom->Parent())
			gsExit[nExit] = "#200" + gsExitShort + "#700";				
	}

	// For this formatting we will use a temporary string and take chunks 
	// of it bit by bit till we run out of string
	while (gsTemp.Length() > 0 || nLineCount < 6)
	{
		// Compass lines
		if (nLineCount < 6)
		{
			for (int i = nCompassWidth-1; i > 0; i--)
			{
				// Find the nearest space
				if (gsTemp.IsSpace(gsTemp.At(i)))
				{
					gsCurrent = gsTemp.Left(i);
					gsTemp = gsTemp.Right(gsTemp.Length() - i);
					//gsCurrent = gsTemp.Length() < nCompassWidth ? gsTemp : gsTemp.Left(nCompassWidth);
					//gsTemp = gsTemp.Right(gsTemp.Length() - nCompassWidth);
					break;
				}
			}

			// Remove leading empty space
			if (gsCurrent.IsSpace(gsCurrent.At(0)))
				gsCurrent.DeleteChar(0);

			if (nLineCount == 1)
				gsCurrent.Format("#601/-----------\\#700 %s", gsCurrent);
			else if (nLineCount == 2)
				gsCurrent.Format("#601|#700 %s %s %s %s #601|#700 %s", gsExit[CExit::NORTHWEST], gsExit[CExit::NORTH], gsExit[CExit::NORTHEAST], gsExit[CExit::UP], gsCurrent);
			else if (nLineCount == 3)
				gsCurrent.Format("#601|#700  %s #601* %s  #601| |#700 %s", gsExit[CExit::WEST], gsExit[CExit::EAST], gsCurrent);
			else if (nLineCount == 4)
				gsCurrent.Format("#601|#700 %s %s %s %s #601|#700 %s", gsExit[CExit::SOUTHWEST], gsExit[CExit::SOUTH], gsExit[CExit::SOUTHEAST], gsExit[CExit::DOWN], gsCurrent);
			else if (nLineCount == 5)
				gsCurrent.Format("#601\\-----------/#700 %s", gsCurrent);
		}
		else
		{
			// Standard 80 line
			for (int i = MAX_WIDTH; i > 0; i--)
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
		}

		gsFinal += gsCurrent + "\n\r";
		nLineCount++;
		
	}

	return gsFinal;

}


////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool CRoom::HandleEvent(CEvent& Event)
{
	bool bHandled = false;

	if ( !bHandled )
		bHandled = CAtomic::HandleEvent(Event);

	return bHandled;

}


//////////////////////////
// Parent Rooms
//////////////////////////

CParentRoom::CParentRoom()
{
	m_nParentID = uiUniqueParentID;
	uiUniqueParentID++;

	m_gsName = "";
	m_gsDescription = "";
}

CParentRoom::~CParentRoom()
{
	m_gsName = "";
	m_nArea = 0;
	m_nParentID = 0;
	m_gsDescription = "";
	m_Rooms.clear();
}

// Returns the Area of the Room
CArea* CParentRoom::GetArea()
{
	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->m_nArea);

	return pArea;
}

// Return a list of all the rooms that are part of this Parent
RoomMap& CParentRoom::Rooms()
{
	CArea* pArea = GetArea();

	// Define our new RoomMap
	RoomMap rmRooms;

	if (pArea)
	{
		// Iterate through all the Room Placements
		for (PlacementList::iterator it = m_Rooms.begin(); it != m_Rooms.end(); it++)
		{
			// See if this room is still valid by fetching it using its Placement
			CRoom* pRoom = pArea->GetRoom(*it);

			// Its valid, add it to our list
			if (pRoom)
				rmRooms.insert(RoomMap::value_type((*it).Room(), pRoom));
		}
	}

	return rmRooms;
}

// Returns a list of Actors
// The function also updates the list of Actors by iterating through each room
ActorMap& CParentRoom::Actors()
{
	CArea* pArea = GetArea();

	// Refresh the list
	m_Actors.clear();

	if (pArea)
	{
		// Iterate through all the Room Placements
		for (PlacementList::iterator it = m_Rooms.begin(); it != m_Rooms.end(); it++)
		{
			// See if this room is still valid by fetching it using its Placement
			CRoom* pRoom = pArea->GetRoom(*it);

			// Its valid, add its Actors to our Map
			if (pRoom)
			{
				// Add these Actors to our map
				for (ActorMap::iterator act = pRoom->Actors().begin(); act != pRoom->Actors().end(); act++)							
					m_Actors.insert(*act);
			}
		}
	}

	return m_Actors;
}

void CParentRoom::WriteXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",				m_gsName);
	Tools.WriteXml(pParent, "description",		m_gsDescription);
	Tools.WriteXml(pParent, "id",				m_nParentID);
	Tools.WriteXml(pParent,	"area",				m_nArea);

	for (gStringImap::iterator gstr = m_Lore.begin(); gstr != m_Lore.end(); gstr++)
	{
		TiXmlNode* pLoreNode = Tools.InsertXmlChild(pParent, "Lore");
		Tools.WriteXml(pLoreNode, "difficulty",	(int)(*gstr).first);
		Tools.WriteXml(pLoreNode, "description", (*gstr).second);
	}
	
	for (PlacementList::iterator it = m_Rooms.begin(); it != m_Rooms.end(); it++)
	{
		TiXmlNode* pRoomNode = Tools.InsertXmlChild(pParent, "Room");

		Tools.WriteXml(pRoomNode, "placement",	(*it));	
	}

}

void CParentRoom::ReadXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "description",	m_gsDescription);
	Tools.ReadXml(pParent, "id",			m_nParentID);
	Tools.ReadXml(pParent, "area",			m_nArea);

	TiXmlNode* pLoreNode = pParent->FirstChild("Lore");
	while ( pLoreNode != NULL )
	{
		int nDifficulty;
		gString gsDesc;

		Tools.ReadXml(pLoreNode, "difficulty",	nDifficulty);
		Tools.ReadXml(pLoreNode, "description", gsDesc);

		m_Lore[ nDifficulty ] = gsDesc;
		pLoreNode = pLoreNode->NextSibling("Lore");
	}

	TiXmlNode* pRoomNode = pParent->FirstChild("Room");

	while (pRoomNode != NULL)
	{
		CPlacement pRoom;

		Tools.ReadXml(pRoomNode, "placement", pRoom);

		m_Rooms.push_back(pRoom);

		pRoomNode = pRoomNode->NextSibling("Room");
	}
}

bool CParentRoom::HandleEvent(CEvent &Event)
{
	return true;
}

// Create our description, the description depends on:
// [1] - Player's galactic lore skill
gString CParentRoom::Description(CActor *pA)
{
	gString gsDescription = m_gsDescription;

	// [1] A Parent Room description usually contains a lot of information about
	// the history of a room, more of this history can be unlocked by having more
	// skill within galactic lore.
	// #TODO# After galactic lore is coded

	return gsDescription;
}


// Writes information about the actor pA to the parent room
// will perform a PER check to ensure all players can see the
// player
void CParentRoom::Write(CActor *pA, char *fmt, ...)
{
	ActorMap::iterator pos;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		if ((*pos).second == pA)
			continue;

		// Check how far away each actor is by building a pathfinding route to them
		CDirectionList dlList;

		// Build route
		CGameObjects::Get().GameWorld()->Navigator()->BuildPathTo(dlList, pA, (*pos).second);

		// Check how far they are, based on the number of rooms to travel
		int nDistance = dlList.Remaining();

		// pA is the actor performing an action, can this (*pos) Actor see this?
		if (nDistance <= (*pos).second->Perception()+1)
		{
			// Don't display the message if they are in the same room as the player, normal message
			// writing will handle this
			if (nDistance != 0)
				((*pos).second)->Write("In the distance, %s", buf);
		}
	}

}


// Writes a message to all players within the parent room
void CParentRoom::Write(char *fmt, ...)
{
	ActorMap::iterator pos;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		((*pos).second)->Write("In the distance, %s", buf);
	}
}

// Writes a message to all players within the parent room
// excluding those within the specified room
void CParentRoom::Write(CRoom * pRoom, char *fmt, ...)
{
	ActorMap::iterator pos;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	for (pos = m_Actors.begin(); pos != m_Actors.end(); pos++)
	{
		if ((*pos).second->CurrentRoom() == pRoom)
			continue;

		((*pos).second)->Write("In the distance, %s", buf);
	}
}