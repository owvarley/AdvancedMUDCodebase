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

// Class    :: ETouch
// Header   :: Events.h
// Function :: Used to define an implementation for the ETouch class. Events are a means to notify an Atomic object
//			:: that something has happened, or should happen. Some examples might be to notify an NPC that it has
//			:: been attacked, or to notify a room that something has just occured inside it. Event handling is an 
//			:: overridable occurence, so each atomic class can potentially deal with a given event differently than
//			:: it's base class might have dealt with it. Events, while they are handled from within the update cycle,
//			:: take place immediately. An update may or may not happen, depending on several flags and timing, but 
//			:: events (if flagged appropriately) are 'handled' right away.

#include <time.h>
#include "Events.h"


UINT uiUniqueEventID = 0;


ETouch::ETouch() : CEvent(EV_TOUCH)
{
	m_TimeStamp = time(0);
}

