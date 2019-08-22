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
// CActor Class header
//
// All characters, Item, Player and NPC are derived from CActor.
//

#if !defined __ACTOR_H__
#define __ACTOR_H__
#include <map>

#include <time.h>

#include "MudCore.h"
#include "Placement.h"
#include "Set.h"
#include "Atomic.h"
#include "Property.h"
#include "Spatial.h"
#include "Attributes.h"
#include "Crew.h"

class CRoom;
class CModule;
class CShip;
class CGameServer;
class CGameWorld;
class CDirectionList;

extern UINT	uiUniqueActorID;



class CActor: public CAtomic
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CGameServer;
	friend class CGameWorld;

// Methods
public:
							CActor();
	virtual					~CActor();

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	//  ActorFlags help determine the various 'types' of actor this may be, as well as
	//  access flags that may be set.
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ActorFlags
	{
		_NONE			= 0,	// Default Type. Indicates an unitialized actor
		_ITEM			= 1,	// An Item
		_NPC			= 2,	// Non-Player-Character (Mobile/AI/Bot)
		_PLAYER			= 3,	// A player character
		_STAFF			= 4,    // A staff member 
		_ASSISTANT		= 5,	// An administrative assistant
		_ADMINISTRATOR	= 6,	// An administrator
		_NUMACTORFLAGS			// Actor flags should go in order to keep this field valid.
	};

	//  ActorState
	//  These flags help to determine what an actor does or may be doing.
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ActorStates
	{
		_UPDATES		= 0,	// This actor is handling it's update cycle
		_THINKS			= 1,	// This actor is handling it's think cycle
		_PASSES_EVENTS  = 2,	// This actor passes unhandled events on to it's base class
		_IGNORE_EVENTS	= 3,	// This actor ignores events
		_FROZEN			= 4,
		_NUMACTORSTATES
	};

	//  ActorPositions
	//  These flags help to determine the position an Actor is in
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ActorPositions
	{
		_PRONE			= 0,
		_CROUCHED		= 1,
		_SITTING		= 2,
		_STANDING		= 3,
		_HOVERING		= 4,
		_FLYING			= 5,
		_NUMACTORPOS
	};

	//  ActorMental
	//  These flags help to determine the Mental State of the Actor
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ActorMental
	{
		_SLEEPING		= 0,
		_RESTING		= 1,
		_UNCONSCIOUS    = 2,
		_INCAPACITATED  = 3,
		_SHELLSHOCK		= 4,
		_DEAD			= 5,
		_NUMACTORMENTAL
	};

	//  ActorEvents
	//  These are events that all actors should know of, and possibly catch.
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ActorEvents
	{
		EV_NOTICE				=  EVRANGE_FIRST_ACTOR,
		EV_DAMAGED,
		EV_DIED,
		EV_DIE,
		EV_ATTACKED,
		EV_MOVE_TO_NEXT_ROOM,
		EV_LAST_ACTOR_EVENT		=  EVRANGE_LAST_ACTOR
	};

	///////////////////////////////////////////////////////////////////////////////////////
	// Static character arrays that hold human-readable verions of the above types
	///////////////////////////////////////////////////////////////////////////////////////
	static char *szActorFlags[];
	static char *szActorStates[];
	static char *szActorPositions[];
	static char *szActorMental[];

	// Our copy constructors
	CActor(const CActor& a);

	// Some useful operator overrides
	CActor&	operator =		(CActor& clone);
	bool	operator ==		( CActor a2 );
	bool	operator !=		( CActor a2 );

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline helper functions that return commonly requested booleans
	///////////////////////////////////////////////////////////////////////////////////////
	inline	bool 					IsStaff()			const { return m_ActorFlags->IsSet(_STAFF); }
	inline	bool 					IsAssistant()		const { return m_ActorFlags->IsSet(_ASSISTANT); }
	inline	bool 					IsAdministrator()	const { return m_ActorFlags->IsSet(_ADMINISTRATOR); }
	inline  bool					IsPlayer()			const { return m_ActorFlags->IsSet(_PLAYER); }
	inline  bool					IsNPC()				const { return m_ActorFlags->IsSet(_NPC); }
	inline  bool					IsItem()			const { return m_ActorFlags->IsSet(_ITEM); }
	
	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline	const gString			Name() 				const { return m_gsName; }
	inline	const gString			Description()		const { return m_gsDescription; }
	inline	const gString			Title()				const { return m_gsTitle; }
	inline	const gString			ShortDesc()			const { return m_gsShortDesc; }
	inline	const gString			Identifier()		const { return m_gsIdentifier; }
	inline	const int				Level()				const { return m_iLevel; }
	inline	const int				Vnum()				const { return m_iVnum; }
	inline  const int				CurrentHealth()		const { return m_Health[1]; }
	inline  const int				MaxHealth()			const { return m_Health[0]; }
	inline  const int				GUID()				const { return IsPlayer() ? Vnum() : m_uiUniqueID; }
	inline	const time_t			Created()			const { return m_CreationDate; }
	inline	CGameWorld*				HomeWorld()			{ return m_pHomeWorld; }
	inline  CCrew*					GetLeader()			{ return m_Leader; }
	inline  CCrew*					GetMember()			{ return m_Crew; }
	inline	CRoom*					CurrentRoom()		{ return m_CurrentRoom; }
	inline  CSet*					ActorFlags()		{ return m_ActorFlags; }
	inline  CShip*					Manned()			{ return m_Manned; }
	inline  CShip*					Command()			{ return m_Command; }
	inline  CModule*				MannedPos()			{ return m_MannedPos; }
	inline  CSet*					ActorPositions()	{ return m_ActorPositions; }
	inline  CSet*					ActorMental()		{ return m_ActorMental; }
	inline  CSet*					ActorStates()		{ return m_ActorStates; }
	inline  CActor*					WatchActor()		{ return m_pWatchActor; }
	inline  CAttributeMgr&			Attributes()		{ return m_Attributes; }
	virtual CPlacement				Position();
	
	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual functions that can be called to set the values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					SetName(const gString& sNewName);
	virtual bool					SetDescription(const gString& sNewDescription);
	virtual bool					SetTitle(const gString& sNewTitle);
	virtual bool					SetShortDesc(const gString& sNewShortDesc);
	virtual bool					SetLevel(int iLevel);
	virtual bool					SetVnum(int iVnum);
	virtual bool					SetManned(CShip* pShip);
	virtual bool					SetCommand(CShip* pShip);
	virtual	bool					SetMannedPos(CModule* pPos);
	virtual bool					SetLeader(CCrew* pCrew);
	virtual bool					SetMember(CCrew* pCrew);
	virtual bool					AdjCurrentHealth(int nValue);
	virtual bool					SetMaxHealth(int nValue);
	virtual bool					SetHomeWorld(CGameWorld* pWorld);
	virtual bool					SetCurrentRoom(CRoom* pRoom);
	virtual bool					SetWatchActor(CActor* pWatcher);

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					Save();
	virtual bool					Load();
	virtual int						Write(char *fmt,...);
	virtual int						Report(CActor* pA, char *fmt,...);
	virtual void					Update(bool bForce = false);
	virtual void					Think(bool bForce = false);
	virtual void					DescribeTo(CActor* pA, bool bFull = false) const;
	virtual bool					ExecuteCommand(const gString& gsCmd, char* szArgs,...);

	///////////////////////////////////////////////////////////////////////////////////////
	// Functions for determining if mobile can carry out space actions
	///////////////////////////////////////////////////////////////////////////////////////
	bool							CanPilot();
	bool							CanComm();
	bool							CanFire();
	bool							CanRadar();
	bool							CanNav();
	bool							CanSystem();

	///////////////////////////////////////////////////////////////////////////////////////
	// Derived attributes
	///////////////////////////////////////////////////////////////////////////////////////
	int								Perception();
	int								Defense();
	int								Health();
	int								Integrity();
	int								Peripheral();


	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					HandleEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Handlers
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					OnDamage(CEvent& Event);
	virtual bool					OnNotice(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream&			operator << ( std::ostream& stream, const CActor& actor );
	friend std::istream&			operator >> ( std::istream& stream, CActor& actor );

	virtual void					WriteXml(TiXmlNode* pParent);
	virtual void					ReadXml(TiXmlNode* pParent);


// Data
protected:
	// stores the common name of the actor
	gString							m_gsName;

	// A string used internally that stores a globally unique name of the actor.
	// Used primarily for debugging purposes
	gString							m_gsIdentifier;

	// This is the (potentially) multi-line description of the actor as seen when
	// another actor 'looks' at it.
	gString							m_gsDescription;

	// This is typically shown in 'who' output.
	gString							m_gsTitle;

	// This is typically a one line description shown when an actor is seen in a location.
	gString							m_gsShortDesc;

	// The actors exact position in the server.
	CPlacement						m_Position;

	// The time this actor last updated (in clock ticks)
	float							m_fLastUpdate;

	// The globally unique ID of this actor.
	UINT							m_uiUniqueID;

	// Ship player is manning a console for
	CShip*							m_Manned;

	// Ship player is in command of
	CShip*							m_Command;

	// Crew actor is leader for
	CCrew*							m_Leader;

	// Crew actor is team member of
	CCrew*							m_Crew;

	// Module they are manning
	CModule*						m_MannedPos;

	// This actors level, if any.
	int								m_iLevel;

	// This actors virtual number.
	int								m_iVnum;

	// This actors health (CURRENT, and MAX)
	int								m_Health[2];

	// The creation date/time of this actor.
	time_t							m_CreationDate;

	// The actors homeworld (if any)
	CGameWorld*						m_pHomeWorld;

	// The room the actor is currently in
	CRoom*							m_CurrentRoom;

	// Flags that may apply to this actor
	CSet*							m_ActorFlags;

	// Attribute Manager
	CAttributeMgr					m_Attributes;

	// Flags representing the actors current states
	CSet*							m_ActorStates;

	// Flags representing the actors positions
	CSet*							m_ActorPositions;

	// Flags representing the actors mental state
	CSet*							m_ActorMental;

	// The actor that is currently seeing everything this actor sees
	CActor*							m_pWatchActor;
};

typedef std::map<int, CActor*> ActorMap;

//
// Actor Events
//

class ENotice : public CEvent
{
public:
	ENotice();

	gString gsNotice;
};

// Sent to an entity to signify it has just taken damage
class EDamage : public CEvent
{
public:
	EDamage();

public:
	CActor* pInflictor;
	float	fAmount;
};

// Sent by an entity when it has just died
class EDied : public CEvent
{
public:
	EDied();

	EDamage evLastDamageEvent;
};

// Sent to an entity to tell it to die
class EDie : public CEvent
{
public:
	EDie();
};

// Sent to an entity to inform it that it is under attack
class EAttack : public CEvent
{
public:
	EAttack();

	CActor* m_pByWho;
};

/*
class EMoveToNextRoom : public CEvent
{
public:
	EMoveToNextRoom();

	CDirectionList& m_List;
	float m_fTime;
}; */

#endif