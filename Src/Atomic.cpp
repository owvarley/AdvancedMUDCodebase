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

// Class    :: CAtomic
// Header   :: Atomic.h
// Function :: The Atomic Class is meant to provide a container to store Events and Affects in.
//			:: It is the base class of CActor, CArea, and CRoom, providing a single source of
//			:: events and affects for these base object types.

#pragma warning(disable:4786)

#include <vector>

#include "Atomic.h"
#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"



CAtomic::CAtomic()
{
	m_ActiveEvents.clear();
	m_ActiveAffects.clear();
}


CAtomic::~CAtomic()
{
	m_ActiveEvents.clear();
	m_ActiveAffects.clear();
}

void CAtomic::WriteXml(TiXmlNode* pParent)
{
	PropertyMgr::WriteXml(pParent);

	if ( m_ActiveAffects.size() > 0 )
	{
		Affectlist::const_iterator _a;
		for (_a = m_ActiveAffects.begin(); _a != m_ActiveAffects.end(); _a++)
		{
			TiXmlNode* pNode = CGameObjects::Get().Tools()->InsertXmlChild(pParent, "Affect");
			CAffect* pAffect = (*_a);
			pAffect->WriteXml(pNode);
		}
	}
}

void CAtomic::ReadXml(TiXmlNode* pParent)
{ 
	CTools& Tools = *CGameObjects::Get().Tools();

	PropertyMgr::ReadXml(pParent);

	TiXmlNode* pNode = pParent->FirstChild("Affect");
	while ( pNode != NULL )
	{
		CAffect* pAffect = new CAffect;
		pAffect->ReadXml(pNode);

		AddAffect(*pAffect);
		pNode = pNode->NextSibling("Affect");
	}
}

/////////////////////////////////////////////////////////////////////////////////
// Affect Methods ///////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

bool CAtomic::AddAffect(CAffect& Affect)
{
	m_ActiveAffects.push_back(&Affect);

	return true;
}

bool CAtomic::DelAffect(int nCount)
{
	Affectlist::iterator pos;

	if ( nCount > m_ActiveAffects.size() )
		return false;

	int nAt = 0;
	for ( pos = m_ActiveAffects.begin(); pos != m_ActiveAffects.end(); pos++, nAt++ )
	{
		if ( nAt == nCount )
		{
			m_ActiveAffects.erase(pos);
			return true;
		}
	}

	return false;
}

bool CAtomic::DelAffect(CAffect& Affect)
{
	Affectlist::iterator pos;
	CAffect af;

	for (pos = m_ActiveAffects.begin(); pos != m_ActiveAffects.end(); pos++)
	{
		af = (CAffect&)(*pos);

		if ( af == Affect )
		{
			m_ActiveAffects.erase(pos);
			return true;
		}
	}

	return false;
}

bool CAtomic::IsAffected(CAffect& Affect)
{
	Affectlist::iterator pos;
	CAffect af;

	for (pos = m_ActiveAffects.begin(); pos != m_ActiveAffects.end(); pos++)
	{
		af = (*pos);

		if ( af == Affect )
			return true;
	}
	return false;
}

void CAtomic::UpdateAffects()
{
	Affectlist::iterator pos;
	CAffect af;
	std::vector<Affectlist::iterator> DeleteList;

	try
	{
		for (pos = m_ActiveAffects.begin(); pos != m_ActiveAffects.end(); pos++)
		{
			af = (CAffect&)(*pos);

			af.Update();

			if ( af.IsExpired() )
				DeleteList.push_back(pos);
		}

		// Ok, now that we've safely marked all of our affects for deletion... delete them!
		if ( DeleteList.size() > 0 )
		{
			std::vector<Affectlist::iterator>::iterator mark;

			for (mark = DeleteList.begin(); mark != DeleteList.end(); mark++)
				m_ActiveAffects.erase( (*mark) );
		}
	}
	catch (...) {}
}


/////////////////////////////////////////////////////////////////////////////////
// Event Methods ////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////

void CAtomic::HandleEvents()
{
	std::vector<UINT> DeleteList;

	EventMap::iterator pos;
	CEvent* pEvent = NULL;
	clock_t CurrentTime = clock();

	for(pos=m_ActiveEvents.begin(); pos!=m_ActiveEvents.end(); pos++)
	{
		pEvent = (CEvent*)(*pos).second;

		if ( pEvent->m_TimeToExecute <= 0 || pEvent->m_TimeToExecute <= clock() )
		{
			// When an event has been handled (successfully or not), add the event
			// to the deletion list for later deletion from the event map.
			DeleteList.push_back(pEvent->GUID());
			HandleEvent( *pEvent );
		}
	}

	std::vector<UINT>::iterator l_itor;

	for (l_itor = DeleteList.begin(); l_itor != DeleteList.end(); l_itor++)
		m_ActiveEvents.erase( (*l_itor) );
}

bool CAtomic::HandleEvent(CEvent& Event)
{
	return DefaultHandler(Event);
}

bool CAtomic::ReceiveEvent(CEvent& Event)
{
	std::pair<EventMap::iterator,bool> result;

	result = m_ActiveEvents.insert( EventMap::value_type(Event.GUID(), &Event) );

	return (result.second);
}

