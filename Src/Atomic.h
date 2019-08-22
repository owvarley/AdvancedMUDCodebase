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
// The Atomic class is used to represent objects that can receive both
// Events, and Affects. Currently, this is CActor, CRoom, and CArea
// (And anything derived off of these classes
//

#ifndef __ATOMIC_H__
#define __ATOMIC_H__

#include "MudCore.h"
#include "Events.h"
#include "Affect.h"
#include "../gTools/TinyXml.h"
#include <../gTools/Property.h>

class CAtomic : public PropertyMgr
{
// Methods
public:
	CAtomic();
	virtual ~CAtomic();

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_AtomicEvents
	{
		EV_FIRST_ATOMIC_EVENT	= EVRANGE_FIRST_ATOMIC,
		EV_LAST_ATOMIC_EVENT	= EVRANGE_LAST_ATOMIC
	};

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline	const int				EventCount() const  { return m_ActiveEvents.size(); }
	inline  const int				AffectCount() const { return m_ActiveAffects.size(); }

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void					HandleEvents();
	virtual bool					HandleEvent(CEvent& Event);
	virtual bool					ReceiveEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Affect Methods
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					AddAffect(CAffect& Affect);
	virtual bool					DelAffect(int nCount);
	virtual bool					DelAffect(CAffect& Affect);
	virtual bool					IsAffected(CAffect& Affect);
	virtual void					UpdateAffects();

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Handlers
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					DefaultHandler(CEvent& Event) { return false; }

	virtual void					WriteXml(TiXmlNode* pParent);
	virtual void					ReadXml(TiXmlNode* pParent);

// Data
protected:

	// List of currently active events for this object.
	EventMap	m_ActiveEvents;

	// List of currently active affects for this object.
	Affectlist	m_ActiveAffects;

};


#endif