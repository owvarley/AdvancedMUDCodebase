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

#if !defined __AREA_H__
#define __AREA_H__

#include <vector>

#include "MudCore.h"
#include "Room.h"
#include "Actor.h"
#include "Placement.h"
#include "Set.h"
#include "Spatial.h"
#include "OTools.h"

extern UINT uiUniqueAreaID;

class CCart;

class CArea: public CAtomic
{
	friend class CGameWorld;

// Members
public:
	typedef enum e_AreaEvents
	{
		EV_FIRST_AREA_EVENT	=	EVRANGE_FIRST_AREA,
		EV_LAST_AREA_EVENT	=	EVRANGE_LAST_AREA
	};

	typedef enum e_AreaFlags
	{
		_UNDERCONSTRUCTION	= 0,
		_TEMPLATE			= 1,
		_NUMAREAFLAG
	};

	CArea();
	~CArea();

	CArea& operator = (CArea& clone);

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline const gString	Name() const 					{ return m_gsName; }
	inline const gString	FileName() const 				{ return m_gsFileName; }
	inline const gString	Identifier() const				{ return m_gsIdentifier;	}
	inline const gString	Author() const					{ return m_gsAuthor;	}
	inline const gString	Ship()	const					{ return m_gsShip;	}
	inline CGameWorld*		HomeWorld()	const 				{ return m_pHomeWorld; }
	inline RoomMap*			Rooms()		 					{ return &m_Rooms; }
	inline ParentMap*		Parents()						{ return &m_Parents; }
	inline CSet*			Flags()							{ return m_pFlags; }
	inline int				Area()  						{ return m_Position.Area();  }
	inline int				World()	 						{ return m_Position.World(); }

	///////////////////////////////////////////////////////////////////////////////////////
	// Methods that can be called to set the values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	void				SetName(const gString& s)			{ m_gsName = s; m_gsFileName = s; }
	void				SetFileName(const gString& s)		{ m_gsFileName = s; }
	void				SetArea(int i)						{ m_Position.Area() = i; }
	void				SetWorld(CGameWorld* pWorld)		{ m_pHomeWorld = pWorld;		}
	void				SetShip(const gString& s)			{ m_gsShip = s; }
	void				Delete()							{ m_bDelete = true; }
	CRoom*				GetRoom(CPlacement v);
	CRoom*				GetRoom(int iRoomNum);
	CParentRoom*		GetParentRoom(int nParentID);

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		Load();
	virtual bool		Save();
	virtual void		Update(bool bForce = false);

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		HandleEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream& operator << ( std::ostream& stream, const CArea& area );
	friend std::istream& operator >> ( std::istream& stream, CArea& area );

	void				WriteXml(TiXmlNode* pParent);
	void				ReadXml(TiXmlNode* pParent);

public:
	CCart*				m_Location;

// Data 
protected:

	gString				m_gsName;
	gString				m_gsFileName;
	gString				m_gsAuthor;
	gString				m_gsShip;
	gString				m_gsIdentifier;
	gString				m_gsDescription;
	RoomMap				m_Rooms;
	ParentMap			m_Parents;
	CPlacement			m_Position;
	CSet*				m_pFlags;
	CGameWorld*			m_pHomeWorld;
	float				m_fLastUpdate;
	bool				m_bDelete;
};

typedef std::vector<CArea*> AreaList;

#endif