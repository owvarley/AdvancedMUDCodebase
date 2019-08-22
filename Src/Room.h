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

#ifndef __ROOM_H__
#define __ROOM_H__

#include <map>
#include <vector>

#include "MudCore.h"
#include "Set.h"
#include "Placement.h"
#include "Reset.h"
#include "Item.h"
#include "Module.h"
#include "Spatial.h"

extern UINT uiUniqueExitID;
extern UINT	uiUniqueRoomID;
extern UINT uiUniqueParentID;

class CParentRoom;

class CExit
{

public:
	CExit();
	~CExit();

	CExit& operator = (CExit& clone);

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	// _DoorFlags help us to determine the state and attributes of a door
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum _DoorFlags
	{
		_OPEN		= 0,	// Door is open
		_CLOSED		= 1,	// Door is closed
		_LOCKED		= 2,	// Door is locked
		_SWIM		= 3,	// Exit must be swum through
		_CLIMB		= 4,	// Exit must be climbed
		_NUMDOORFLAGS
	};

	// _Directions should be self explanatory.
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum  _Directions
	{
		NONE		= 0,
		NORTH		= 1,
		SOUTH		= 2,
		EAST		= 3,
		WEST		= 4,
		UP			= 5,
		DOWN		= 6,
		NORTHWEST   = 7,
		NORTHEAST   = 8,
		SOUTHWEST   = 9,
		SOUTHEAST   = 10,
		NUMEXITS
	};


	///////////////////////////////////////////////////////////////////////////////////////
	// Static character arrays that hold human-readable verions of the above types
	///////////////////////////////////////////////////////////////////////////////////////
	static char* szDoorFlags [_NUMDOORFLAGS];
	static char* szExitNames [NUMEXITS];

	///////////////////////////////////////////////////////////////////////////////////////
	// Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	inline gString		DirectionStr() 		const { return szExitNames[ m_Direction ]; }
	inline gString		Identifier()		const { return m_gsIdentifier; }
	inline int			Complexity()		const { return m_nComplexity; }
	inline int			Strength()			const { return m_nStrength; }
	virtual void		Update(bool bForce = false);
	CPlacement			Destination()		const { return m_Destination; }
	_Directions			Direction()			const { return m_Direction; }
	CSet*				ExitFlags()			const { return m_Flags; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	void				WriteXml(TiXmlNode* pParent);
	void				ReadXml(TiXmlNode* pParent);

	///////////////////////////////////////////////////////////////////////////////////////
	// General lookup/conversion operations
	///////////////////////////////////////////////////////////////////////////////////////
	static _Directions GetDirection(gString gsDir);

// data
public:

	CPlacement	m_Destination;
	gString		m_gsIdentifier;
	// Lock modifiers
	int			m_nComplexity;		// How complex the lock is (1-6)
	int			m_nStrength;		// How strong the door is (1-6)
	CSet*		m_Flags;
	_Directions	m_Direction;
	float		m_fLastUpdate;

};

typedef std::vector<CExit*> ExitList;


class CRoom : public CAtomic
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CGameWorld;
	friend class CArea;
	friend class CReset;

// Members
public:

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	//  e_RoomEvents
	//  These enumerate the events that this class has defined
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_RoomEvents
	{
		EV_FIRST_ROOM_EVENT	=	EVRANGE_FIRST_ROOM,
		EV_LAST_ROOM_EVENT	=	EVRANGE_LAST_ROOM
	};

	// _RoomFlags help us to determine the state and attributes of a door
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_RoomFlags
	{
		_OBSCURED		=	0,	// Prevents the occupants in the room being seen from parent
		_HIDDEN			=	1,	// Prevents room + exits being seen
		_NOMOB			=	2,  // Prevents non-retainer npcs from entering
		_TURBOLIFT		=	3,	// Special flag, room is a turbolift
		_INDOORS		=	4,	// Indoors flag, prevent weather messages
		_BALANCE		=	5,	// Balance requires a balance check to prevent falling
		_ROAD			=	6,	// Room acts as a road for vehicles
		_MAXROOMFLAGS
	};

	// _Barle defines the type of the area in terms of Richard Bartle's paper
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_Bartle
	{
		_ACHEIVER		=	0,	// Acheiver or Travel zone
		_KILLER			=	1,	// Killer or Deadly zone
		_SOCALISER		=	2,  // Socialiser or Social zone
		_EXPLORER		=	3,	// Explorer or Adventure zone
		_MAXBARTLE
	};


	// _TerrainTypes defines the type of terrain in the room
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_TerrainTypes
	{
		_URBAN			=	0,
		_DESERT			=	1,
		_JUNGLE			=	2,
		_ICE			=	3,
		_VOID			=	4,	// Space
		_SWAMP			=	5,
		_MOUNTAIN		=	6,
		_WATER			=	7,
		_VOLCANIC		=	8,
		_MAXSECTORTYPES
	};


	typedef enum e_RoomMessage
	{
		_LOCAL			= 0,
		_GLOBAL			= 1
	};

	static char* szRoomFlags [];
	static char* szCoverTypes [];
	static char* szBartle [];
	static char* szTerrainTypes [];

	CRoom();
	~CRoom();

	CRoom& operator = (CRoom& clone);

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline	gString		Name()			const { return m_gsName; }
	inline  gString		Identifier()	const { return m_gsIdentifier; }
	inline  int			Area()				  { return m_Position.Area(); }
	inline  int			Vnum()				  { return m_Position.Room(); }
	inline	int			World()			      { return m_Position.World(); }
	inline  int			Parent()		const { return m_nParent;	}
	inline  int			Size()			const { return m_nSize; }
	inline  int			Cover()			const { return m_nCover; }
	inline  int			Bartle()		const { return m_nBartle; }
	inline	int			Terrain()		const { return m_nTerrain; }	

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return non-constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline  ResetList&  Resets()		{ return m_Resets; }
	inline	ExitList&	Exits()			{ return m_Exits; }
    inline  ActorMap&	Actors()		{ return m_Actors; }
	inline	CPlacement&	Position()		{ return m_Position; }
	inline  gStringList& Components()   { return m_Components; }
	inline  gStringList& Ships()		{ return m_Ships; }
	
	///////////////////////////////////////////////////////////////////////////////////////
	// Functions that can be called to set the values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	void				SetName(const gString& s)			{ m_gsName = s; }
	void				SetDescription(const gString& s)	{ m_gsDescription = s; }
	void				SetArea(int i)						{ m_Position.Area() = i; }
	void				SetVnum(int i)						{ m_Position.Room() = i; }
	void				SetWorld(int i)						{ m_Position.World() = i; }
	void				SetSize(int i)						{ m_nSize = i; }
	void				SetCover(int i)						{ m_nCover = i; }
	void				SetBartle(int i)					{ m_nBartle = i; }
	void				SetTerrain(int i)					{ m_nTerrain = i; }
	void				SetParent(int i);
	void				SetPosition(int iA, int iR, int iW) { m_Position.Set(iA,iR,iW); }
	void				AddComponent(const gString& s)		{ m_Components.push_back(s); }

	///////////////////////////////////////////////////////////////////////////////////////
	// Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void		Add(CActor* pA);
	virtual void		Remove(CActor* pA);
	virtual void		Update(bool bForce);
	virtual void		Write(char *fmt,...);
	virtual void		Write(int nScope, char *fmt,...);
	virtual void		Write(CActor* pA, char *fmt,...);
	
	gString				Description(CActor * pA);		// Builds the description using the DESC system
	gString				FormatDesc(gString gsD);		// Formats the desc of the room for output
	bool				RemShip(gString gsShip);		//
	bool				ShipHere(gString gsShip);		// Is there a Ship landed here with this name
	CArea*				GetArea();						// Get the area this room is part of
	CShip*				GetShip();						// Get the Ship this room is part of
	CParentRoom*		GetParent();					// Get the parent room of this room
	ModuleList*			GetMod(int type);				// Get a module installed in this room
	ModuleList*			GetMod(gString gsName);			// Get a module
	ComponentList*		GetComp(int type);				// Get a component by type
	CComponent*			GetComp(gString gsName);		// Get a component by name
	gStringList*		Modules();

	int					NPCCount(int iVnum);
	int					ObjectCount(int iVnum);
	void				_Reset();
	void				AddShip(CShip* pShip);

	CExit*				GetExit(CExit::_Directions Dir);
	gString				FindDirection(CPlacement v);
	CActor*				FindFirstActor(gString sWhat);
	CItem*				FindFirstItem(gString sWhat);
	CModule*			FindFirstModule(gString sWhat);

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		HandleEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream& operator << ( std::ostream& stream, const CRoom& room );
	friend std::istream& operator >> ( std::istream& stream, CRoom& room );

	void				WriteXml(TiXmlNode* pParent);
	void				ReadXml(TiXmlNode* pParent);

// Data
public:
	float				m_fLastUpdate;

private:

	gString				m_gsName;		// Room name
	gString				m_gsIdentifier; // Unique identifier
	gString				m_gsComponent;	// Components

	// D.E.S.C System
	gString				m_gsDescription;
	gString				m_gsIllumination;	
	gStringImap			m_Lore;
	gStringImap			m_Force;
	gStringImap			m_Perception;

	int					m_nParent;		// Parent ID
	int					m_nTerrain;		// Sector type
	int					m_nCover;		// Cover type
	int					m_nSize;		// Max number of players in room
	int					m_nBartle;		// Bartle type
	
	CPlacement			m_Position;		// Vnum
	ExitList			m_Exits;		// Standard player exits
	gStringList			m_Components;	// Components in room
	gStringList			m_Ships;		// For Landed ships
	ResetList			m_Resets;		// Resets
	ActorMap			m_Actors;		// Actor list
	ItemList			m_Items;		// Item list
	CSet*				m_RoomFlags;	// Roomflags

};

typedef std::map<int, CRoom*> RoomMap;
typedef std::vector<CPlacement> PlacementList;

class CParentRoom : CAtomic
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CGameWorld;
	friend class CArea;
	friend class CRoom;

// Members
public:

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	//  e_ParentRoomEvents
	//  These enumerate the events that this class has defined
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ParentRoomEvents
	{
		EV_FIRST_PARENT_ROOM_EVENT	=	EVRANGE_FIRST_ROOM,
		EV_LAST_PARENT_ROOM_EVENT	=	EVRANGE_LAST_ROOM
	};

	CParentRoom();
	~CParentRoom();

	CParentRoom& operator = (CParentRoom& clone);

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline	gString		Name()			const { return m_gsName; }
	inline  int			ID()			const { return m_nParentID; }
		
	///////////////////////////////////////////////////////////////////////////////////////
	// Functions that can be called to set the values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	void				SetName(const gString& s) { m_gsName = s; }
	void				SetDescription(const gString& s) { m_gsDescription = s; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	ActorMap&			Actors();
	RoomMap&			Rooms();
	virtual void		Write(char *fmt,...);
	virtual void		Write(CActor* pA, char *fmt,...);
	virtual void		Write(CRoom* pR, char *fmt,...);
	gString				Description(CActor * pA);
	
	CArea*				GetArea();						// Get the area this room is part of
	bool				AddRoom(CPlacement v);			// Add a room to this parent
	bool				RemoveRoom(CPlacement v);		// Remove a room from this parent

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		HandleEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	void				WriteXml(TiXmlNode* pParent);
	void				ReadXml(TiXmlNode* pParent);

// Data
private:

	gString				m_gsName;

	// DESC System 
	gString				m_gsDescription;
	gStringImap			m_Lore;

	int					m_nParentID;
	int					m_nArea;
 
	PlacementList		m_Rooms;
	ActorMap			m_Actors;

};

// Maps ParentRoomID to Parent Room object
typedef std::map<int, CParentRoom*> ParentMap;

#endif