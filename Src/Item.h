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

#ifndef __CITEM_H__
#define __CITEM_H__

#include <vector>

#include "MudCore.h"
#include "Actor.h"
#include "Placement.h"


class CItem : public CActor
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CRoom;

// Members
public:

	CItem();
	virtual ~CItem();

	// Item Events
	// These enumerations define event IDs. These numbers need to be unique, and so
	// are defined by the range given in EVRANGE_FIRST through EVRANGE_LAST
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ItemEvents
	{
		EV_FIRST_ITEM_EVENT	=	EVRANGE_FIRST_ITEM,
		// EV_ ...
		EV_LAST_ITEM_EVENT	=	EVRANGE_LAST_ITEM
	};

	// Some useful operator overrides
	CItem&	operator =	(CItem& i);
	bool	operator ==	(CItem i );

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline Data Access Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline	UINT&		Value()			{ return m_uiValue; }
	inline	int&		CarriedBy()		{ return m_iCarriedBy; }
	inline	int&		LocatedIn()		{ return m_iLocatedIn; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void		DescribeTo(CActor* pA, bool bFull = false);
	virtual void		Update(bool bForce = false);
	virtual void		Think(bool bForce = false);

	virtual bool		Load(gFileName gsRootDir = "");
	virtual bool		Save(gFileName gsRootDir = "");

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream& operator << ( std::ostream& stream, const CItem& item );
	friend std::istream& operator >> ( std::istream& stream, CItem& item );

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool		HandleEvent(CEvent& Event);


// Data
protected:
	// The dollar value of the item
	UINT				m_uiValue;

	// The GUID of the actor carrying the item
	int					m_iCarriedBy;

	// The GUID of the actor this item is located in
	int					m_iLocatedIn;
};

typedef std::vector<CItem*> ItemList;

#endif

