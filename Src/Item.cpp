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

// Class    :: CItem
// Header   :: Item.h
// Function :: Implements the Item type of Actor, provides methods for creating, deleting,
//			:: saving, loading and event handling

#include <fstream>

#include "MudCore.h"
#include "Item.h"
#include "Actor.h"
#include "Placement.h"
#include "GameObjects.h"
#include "Tools.h"
#include "../gTools/Log.h"

CItem::CItem()
{
	m_uiValue = 0;
	m_iCarriedBy = -1;
	m_iLocatedIn = -1;
	m_gsIdentifier.Format("Item_%d", uiUniqueActorID);
	m_ActorFlags->SetBit(CActor::_ITEM);
	m_ActorStates->SetBit(CActor::_UPDATES);

	Register("Value", (long*)&m_uiValue);
	Register("CarriedBy", (long*)&m_iCarriedBy);
	Register("LocatedIn", (long*)&m_iLocatedIn);
}

CItem::~CItem()
{
	m_gsName = "";
	m_iVnum = -1;
	m_gsDescription = "";
	m_gsShortDesc = "";
	m_uiValue = 0;
	m_Position.Reset();
	m_iCarriedBy = -1;
	m_iLocatedIn = -1;
	m_gsIdentifier += "_Deleted";
}

CItem& CItem::operator = (CItem& i)
{
	CActor::operator=(i);

	m_iCarriedBy	= i.m_iCarriedBy;
	m_iLocatedIn	= i.m_iLocatedIn;
	m_uiValue		= i.m_uiValue;

	return *this;
}

bool CItem::operator == (CItem i)
{

	if ( m_iVnum != i.Vnum() )
		return false;
	if ( m_gsName.Compare( i.m_gsName ) != 0 )
		return false;
	if ( m_gsShortDesc.Compare( i.m_gsShortDesc ) != 0 )
		return false;

	return true;
}

std::ostream& operator << ( std::ostream& stream, const CItem& item )
{
	CTools& Tools = *CGameObjects::Get().Tools();

    stream << (const CActor&)(item);

	Tools.WriteLn(stream, "[Item]");
	Tools.WriteLn(stream, " Value             : %d", item.m_uiValue);

	if ( item.m_iCarriedBy != -1 )
		Tools.WriteLn(stream, " Carried By        : %d", item.m_iCarriedBy );

	if ( item.m_iLocatedIn != -1 )
		Tools.WriteLn(stream, " Located Inside    : %d", item.m_iLocatedIn);

	Tools.WriteLn(stream, "[/Item]");

	return stream;
}

std::istream& operator >> ( std::istream& stream, CItem& item )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	bool bDone = false;

    stream >> (CActor&)(item);

	if (!stream.fail() && !stream.eof())

	try
	{
		if ( Tools.ReadKey(stream) == "[Item]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Item]"  )
				switch ( gsKey[0] )
				{
					case 'C':
						if ( gsKey == "Carried By" )
							Tools.ReadData(stream, item.m_iCarriedBy);
						break;
					case 'L':
						if ( gsKey == "Located Inside" )
							Tools.ReadData(stream, item.m_iLocatedIn);
						break;
					case 'V':
						if ( gsKey == "Value" )
							Tools.ReadData(stream, item.m_uiValue);
						break;
					default:
						g_Log.Log(LOG_ERROR, "[Item::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Item]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CItem::>>] Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CItem::>>] Error encountered while reading %s\'s data..", item.Name());
	}

	return stream;
}

void CItem::Think(bool bForce)
{
	if ( !ActorStates()->IsSet(_THINKS) && !bForce )
		return;

}

void CItem::Update(bool bForce)
{
	float fThisUpdate = CGameObjects::Get().Clock();

	if ( EventCount() > 0 && !ActorStates()->IsSet(_IGNORE_EVENTS) )
		HandleEvents();

	if ( !ActorStates()->IsSet(_UPDATES) && !bForce )
		return;

	if ( fThisUpdate - m_fLastUpdate < fItemUpdateDelta )
		return;

	m_fLastUpdate = fThisUpdate;

	return;
}

void CItem::DescribeTo(CActor* pA, bool bFull)
{
	gString sVnum;

	sVnum.Format("[%5d] ", Vnum());

	pA->Write("%s%s\n\r%s\n\r",
		( pA->IsAdministrator() || pA->IsAssistant() ) ? "" : sVnum,
		Name(),
		bFull ? Description() : ShortDesc());
}


bool CItem::Load(gFileName gsRootDir)
{
	std::fstream fp;
	gFileName gsFile;

	gsFile.Format("%sItems\\%d.itm", gsRootDir, Vnum());

	fp.open(gsFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp >> *this;
	//fp.unlock();
	fp.close();

	return true;
}


bool CItem::Save(gFileName gsRootDir)
{
	std::fstream fp;
	gFileName gsFile;

	gsFile.Format("%sItems\\%d.itm", gsRootDir, Vnum());

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


////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool CItem::HandleEvent(CEvent& Event)
{
	bool bHandled = false;

	if ( ActorStates()->IsSet(_PASSES_EVENTS) )
		bHandled = CActor::HandleEvent(Event);

	return bHandled;
}
