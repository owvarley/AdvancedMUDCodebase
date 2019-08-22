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

// Class    :: CArea
// Header   :: Area.h
// Function :: An area represents a geographic region. It contains a list of Rooms
//			:: and is contained within a CGameWorld.


#pragma warning(disable:4786)

#include <map>
#include <vector>
#include <fstream>

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "GameServer.h"
#include "Area.h"
#include "Room.h"
#include "Npc.h"
#include "Actor.h"
#include "../gTools/Log.h"
#include "Placement.h"

UINT uiUniqueAreaID = 0;


CArea::CArea()
{
	uiUniqueAreaID++;
	m_pFlags = new CSet();
	m_bDelete = false;
	m_Position.Set(0,0,0);
	m_gsIdentifier.Format("Area_%d", uiUniqueAreaID);
	m_gsFileName = m_gsIdentifier;
	m_pHomeWorld = NULL;
	m_Location = new CCart();

	Register("name",		&m_gsName);
	Register("file_name",	&m_gsFileName);
	Register("identifier",	&m_gsIdentifier);
	Register("description",	&m_gsDescription);
	Register("position",	&m_Position);
	Register("last_update",	&m_fLastUpdate);
}

CArea::~CArea()
{
	delete m_pFlags;
	m_pFlags = NULL;

	m_gsName = "";
	m_gsAuthor = "";
	m_gsShip = "";
	m_bDelete = false;
	m_Position.Set(0,0,0);
	m_fLastUpdate	= CGameObjects::Get().Clock();

	m_Rooms.clear();

	m_gsIdentifier += "_Deleted";
	m_gsFileName = "";
	m_Location = NULL;
}

CArea& CArea::operator = (CArea& area)
{
	m_gsName = area.m_gsName;
	m_gsFileName = area.m_gsFileName;
	m_gsShip = area.m_gsShip;
	m_gsDescription = area.m_gsDescription;
	*m_pFlags = *area.m_pFlags;
	m_Position = area.m_Position;
	m_pHomeWorld = area.m_pHomeWorld;

	// Copy the Rooms over
	for (RoomMap::iterator room = area.m_Rooms.begin(); room != area.m_Rooms.end(); room++)
	{
		CRoom* pRoom = new CRoom;
		*pRoom = *((*room).second);
		m_Rooms.insert(RoomMap::value_type(pRoom->Vnum(), pRoom));
	}


	// #TODO#
	// Copy the Objects over

	// Copy the NPCs over
	
	return *this;
}

void CArea::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CAtomic::ReadXml(pParent);

	int nWorld;

	Tools.ReadXml(pParent, "name",				m_gsName);
	Tools.ReadXml(pParent, "author",			m_gsAuthor);
	Tools.ReadXml(pParent, "ship",				m_gsShip);
	Tools.ReadXml(pParent, "homeworldid",		nWorld);
	Tools.ReadXml(pParent, "flags",				*m_pFlags);
	m_Location->ReadXml(pParent);

	// Set the GameWorld
	m_pHomeWorld = CGameObjects::Get().GetWorld(nWorld);

	TiXmlNode* pRoomNode = pParent->FirstChild("Room");

	while ( pRoomNode != NULL )
	{
		CRoom* pRoom = new CRoom;

		pRoom->ReadXml(pRoomNode);

		m_Rooms[ pRoom->Vnum() ] = pRoom;

		pRoomNode = pRoomNode->NextSibling("Room");
	}

	TiXmlNode* pParentNode = pParent->FirstChild("Parent");

	while ( pParentNode != NULL )
	{
		CParentRoom* pParent = new CParentRoom();

		pParent->ReadXml(pParentNode);

		m_Parents[ pParent->ID() ] = pParent;

		pParentNode = pParentNode->NextSibling("Parent");
	}

	return;
}

void CArea::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CAtomic::WriteXml(pParent);

	RoomMap::iterator pos;
	int nRooms = m_Rooms.size();
	int nWorld = m_pHomeWorld == NULL ? -1 : (int)m_pHomeWorld->GUID();

	Tools.WriteXml(pParent, "author",			m_gsAuthor);
	Tools.WriteXml(pParent, "ship",				m_gsShip);
	Tools.WriteXml(pParent, "homeworldid",		nWorld);
	Tools.WriteXml(pParent, "flags",			*m_pFlags);
	m_Location->WriteXml(pParent);

	if ( nRooms > 0 )
	{
		for (pos = m_Rooms.begin(); pos != m_Rooms.end(); pos++)
		{
			TiXmlNode* pRoomNode = Tools.InsertXmlChild(pParent, "Room");

			CRoom* pRoom = ((*pos).second);
			pRoom->WriteXml(pRoomNode);
		}
	}

	if ( m_Parents.size() > 0)
	{
		for (ParentMap::iterator par = m_Parents.begin(); par != m_Parents.end(); par++)
		{
			TiXmlNode* pParentNode = Tools.InsertXmlChild(pParent, "Parent");

			(*par).second->WriteXml(pParentNode);
		}
	}

	return;
}

CRoom* CArea::GetRoom(CPlacement v)
{
	RoomMap::iterator pos;
	CRoom *pR = NULL;

	pos = m_Rooms.find(v.Room());

	if ( pos != m_Rooms.end() )
		pR = (CRoom*)((*pos).second);

	return pR;
}

CRoom* CArea::GetRoom(int iRoomNum)
{
	RoomMap::iterator pos;
	CRoom *pR = NULL;

	pos = m_Rooms.find(iRoomNum);

	if ( pos != m_Rooms.end() )
		pR = (CRoom*)((*pos).second);

	return pR;
}


// Returns a Parent Room by its ID
CParentRoom* CArea::GetParentRoom(int nParentID)
{
	ParentMap::iterator par = m_Parents.find(nParentID);

	if (par != m_Parents.end())
	{
		return (*par).second;
	}
	else
	{
		return NULL;
	}

}


void CArea::Update(bool bForce)
{
	LOG_SCOPE("CArea::Update");
	float fThisUpdate = CGameObjects::Get().Clock();
	RoomMap::iterator pos;
	CRoom* pR = NULL;

	if ( EventCount() > 0 )
		HandleEvents();

	// Only update every minute or if forced
	if ( bForce || (fThisUpdate - m_fLastUpdate >= fAreaUpdateDelta) )
	{
		//g_Log.Log(LOG_INFO, "[CArea::Update] Performing Area Update.\n");
		m_fLastUpdate = fThisUpdate;
	}

	// Maintenance Step
	// If this area belongs to a ship and that ship has been deleted we need to remove the
	// area. This should not happen but this is just in case.
	/*
	if (this->Ship() != "" && !this->m_pFlags->IsSet(_TEMPLATE))
	{
		CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(this->Ship());
		
		// It doesnt exist so remove it
		if (!pShip)
		{
			g_Log.Log(LOG_INFO, "Maintenance: %s deleted as %s missing.\n", this->Name(), this->Ship());
			this->Delete();
			return;
		}
		
		// Else we continue the update
	} */
	
	// Update each room.
	for (pos = m_Rooms.begin(); pos != m_Rooms.end(); pos++)
	{
		try
		{
			((*pos).second)->Update(bForce);
		}
		catch(...) {break;}
	}

}

bool CArea::Save()
{
	LOG_SCOPE("CArea::Save");
	// Format the File correctly so it now saves it in the Area Directory
	gString gsFile;
	gsFile.Format("%s%s\\Areas\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_WORLD], HomeWorld()->Key(), m_gsFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Area");

	WriteXml(pXmlNode);

	g_Log.Log(LOG_INFO, "\"%s\" saved.", m_gsName);

	return doc.SaveFile((const char*)gsFile);
}

bool CArea::Load()
{
	CGameObjects& globals = CGameObjects::Get();
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Area");

	ReadXml(pNode);
	return true;
}

////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool	CArea::HandleEvent(CEvent& Event)
{
	bool bHandled = false;

	if ( !bHandled )
		bHandled = CAtomic::HandleEvent(Event);

	return bHandled;

}
