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

#ifndef __EVENTS_H__
#define __EVENTS_H__

#include <map>
#include <time.h>

#include "Mudcore.h"

extern UINT uiUniqueEventID;

class CActor;

typedef enum e_EventCodes
{
	EV_VOID			= EVRANGE_FIRST_BASE,	// Default event
	EV_TOUCH,								// An example event, marks the time 'touched'.
	// EV_LAST		= EVRANGE_LAST_BASE
};

class CEvent
{
// Methods
public:

	CEvent(int Code) : m_EventCode(Code),m_uiID(++uiUniqueEventID), m_TimeToExecute(0) {}

	virtual ~CEvent() {};
	inline long GUID() { return m_uiID; }
	inline int Code() { return m_EventCode; }
	inline void SetExecutionTime(clock_t Time) { if ( Time >= clock() ) m_TimeToExecute = Time; }

// Data
public:
	int					m_EventCode;	// Event Code
	long				m_uiID;			// Unique Event ID
	clock_t				m_TimeOfEvent;	// Exact Time Event Occured
	clock_t				m_TimeToExecute;// Time this event should be executed
};

typedef std::map<long, CEvent*> EventMap;
typedef CEvent event;


///////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Global Event Types /////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////

const event EVoid(EV_VOID);

class ETouch : public CEvent
{
public:
	ETouch();

	time_t m_TimeStamp;
};


#endif