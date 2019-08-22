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
// NPC Class Definition
//
// NPC, or "Non-Player Character" are otherwise known in the gaming
// community as "AI". This class is meant to provide a base to build
// your muds AI from.
//
#ifndef __NPC_H__
#define __NPC_H__

#include "MudCore.h"
#include "Actor.h"
#include "Mobile.h"

class CActor;
class CPlacement;

class CNpc : public CMobile
{
public:
	CNpc();
	virtual ~CNpc();

	// Some useful operator overrides
	CNpc&	operator =		(CNpc& clone);

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	//  NPC Events
	// These enumerations define event IDs. These numbers need to be unique, and so
	// are defined by the range given in EVRANGE_FIRST through EVRANGE_LAST
	typedef enum e_NpcEvents
	{
		EV_FIRST_NPC_EVENT = EVRANGE_FIRST_NPC,
		EV_LAST_NPC_EVENT  = EVRANGE_LAST_NPC
	};

	//  eState
	//  These flags help to determine what an NPC does or may be doing.
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_State
	{
		_IDLE		= 0,
		_FIGHT		= 1,
		_FLEE		= 2,
		_HUNGRY		= 3,
		_THIRSTY	= 4,
		_WANDER		= 5,
		_GUARD		= 6,
		_NUMSTATES
	};

	//  ePersona
	//  These flags determine the personality of an NPC
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_Persona
	{
		_MEEK		= 0,
		_NICE		= 1,
		_NORMAL		= 2,
		_AGGRESSIVE	= 3,
		_MEAN		= 4,
		_CRUEL		= 5,
		_NUMPERSONAS
	};


	///////////////////////////////////////////////////////////////////////////////////////
	// Static character arrays that hold human-readable verions of the above types
	///////////////////////////////////////////////////////////////////////////////////////
	static char* szNPCTypes[];
	static char* szNPCStates[];
	static char* szNPCPersonas[];


// Members
public:

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline CPlacement		TargetPos()		{ return m_TargetPos; }
	inline e_Persona		Persona()		{ return m_Persona; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		Load(gFileName gsRootDir = "");
	virtual bool		Save(gFileName gsRootDir = "");

	virtual void		Think(bool bForce = false);
	virtual void		Update(bool bForce = false);
	virtual bool		ChangeState(e_State eState);
	virtual bool		HasReachedDest() { return m_TargetPos == m_Position; }
	virtual void		SetPersona(e_Persona P) { m_Persona = P; }
	virtual void		SetTargetPos(CPlacement V) { m_TargetPos = V; }


	///////////////////////////////////////////////////////////////////////////////////////
	// General lookup/conversion operations
	///////////////////////////////////////////////////////////////////////////////////////
	static e_Persona	GetPersona(gString gsPersona);

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Behaviour Related Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void		Attack();
	virtual void		FindSustenance();
	virtual void		Wander();
	virtual void		MoveTowardDest();

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Handlers
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		HandleEvent(CEvent& Event);
	virtual bool		OnDamage(CEvent& Event);


	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream&		operator << ( std::ostream& stream, const CNpc& npc );
	friend std::istream&		operator >> ( std::istream& stream, CNpc& npc );
	
	virtual void		WriteXml(TiXmlNode* pParent);
	virtual void		ReadXml(TiXmlNode* pParent);



// Data
public:
	float				m_fLastUpdate;
	float				m_fMaxIdleTime;
	CTimer				m_IdlePeriod;

	e_State				m_State;
	e_Persona			m_Persona;
	CPlacement			m_TargetPos;
	CActor*				m_pTarget;
};

#endif