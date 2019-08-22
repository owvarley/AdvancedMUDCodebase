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
// Pathfinding
//

#ifndef __PATHFINDING_H__
#define __PATHFINDING_H__

#include "MudCore.h"
#include "GameWorld.h"
#include "Placement.h"
#include "Room.h"

// This structure holds an exit direction, and the room# it leads to
struct NavigationExit
{
	CPlacement vDestination;// Destination this direction leads to (world/area/room) 
	SHORT nDir;				// Direction
};

typedef std::vector<NavigationExit> NavExitList;

// This structure holds the current room location, and an array of NavigationExits
// leading out of it.
struct NavigationPoint
{
	CPlacement	vLocation;	// Current Location (world/area/room)
	NavExitList	Exits;		// Array of exits from this room
};

// This structure holds all the information we need for our A* pathfinding
struct NavigationNode
{
	NavigationPoint	nvPoint;
	NavigationNode*	pParent;
	int				nCostFromStart;
	int				nCostToGoal;

	bool			bOnOpen;
	bool			bOnClose;

	NavigationNode()
	{
		pParent			= NULL;
		nCostFromStart  = 0;
		nCostToGoal		= 0;
		bOnOpen			= false;
		bOnClose		= false;
	}
};

struct NavigationCosts
{
	int nDefaultCost;
	int nCostToOpenDoor;
	int nCostToOpenLockedDoor;
};

// NavigationMap
// This holds the exits and room#s of every room in the world
typedef std::map<gString, NavigationPoint*> NavigationMap;

// NodeList
// A commonly used list of NavigationNodes.  Used for both m_Open and m_Closed
// lists. (Note that these lists are treated very differently however)
typedef std::vector<NavigationNode*> NodeList;


// Note: Since we are treating the stl::vector more like a priority queue than
// a standard vector, we have to supply this class in order for the STL to sort
// the vector
class WhichNodeIsGreater
{
public:

	bool operator() (NavigationNode* pFirst, NavigationNode* pSecond) const 
		{ return ( (pFirst->nCostFromStart+pFirst->nCostToGoal) > (pSecond->nCostFromStart+pSecond->nCostToGoal)); }
};


// One of the things we want to be able to do, is store the results of our
// pathfindings.  This list is used for that purose. It's meant to provide
// basic, easy to use methods for traversal.
class CDirectionList
{
// Methods
public:
	CDirectionList() { m_nPos = 0; m_List.clear(); }
	~CDirectionList() { m_nPos = 0; m_List.clear(); }

	// Return the next direction.
	SHORT	Next() { if (AtEnd()) return 0; return m_List[m_nPos++]; }
	// Return the previous direction
	SHORT	Previous() { if (m_nPos==0) return 0; m_nPos--; return m_List[m_nPos]; }
	// Return the total # of directions
	SHORT	Count() { return m_List.size(); }	
	// Return the # of directions remaining
	SHORT	Remaining() { return m_List.size() - m_nPos; }
	// Return the # of directions travelled
	SHORT	Covered() { return m_nPos; }	
	// Have we reached the destination?
	bool	AtEnd() { return (m_nPos>=m_List.size()) ? true : false; }	
	// Reset the current pos to the beginning
	void	Reset() { m_nPos = 0; }
	void	Clear() { Reset(); m_List.clear(); }
	void	AddRoute(SHORT nDirection) { if (nDirection<1) return; m_List.insert(m_List.begin(), nDirection); }

	// Serialization
	friend std::ostream&		operator << ( std::ostream& stream, const CDirectionList& List );
	friend std::istream&		operator >> ( std::istream& stream, CDirectionList& List );


// Data
private:
	std::vector<SHORT> m_List;	
	int				   m_nPos;
};

// CNavigator
// Finally, our pathfinding class.  This is an implementation of the A* algorithm.
class CNavigator 
{
// Methods
public:
	CNavigator(CGameWorld* pWorld);
	~CNavigator();

	void Init();	
	void Clear();

	// Build a path to a given room
	bool BuildPathTo(CDirectionList& List, CActor* pActor, CRoom* pRoom);
	// Build a path to a given actor 
	bool BuildPathTo(CDirectionList& List, CActor* pFrom, CActor* pTo);
	// Build a path to an explicit location
	bool BuildPathTo(CDirectionList& List, CActor* pActor, CPlacement vTo);

private:

	// The actual implementation of A*
	bool BuildPath(CDirectionList& List, CActor* pFrom, CPlacement vTo);

	// Returns the direction in the list of exits to the requested location
	int  LocateExit(CPlacement vTo, NavExitList& List);

	// Cost methods
	int  TraversalCost(NavigationNode* pFrom, NavigationNode* pTo, CActor* pActor);
	int  EstimatePathCost(NavigationNode* pFrom, NavigationNode* pTo, CActor* pActor);

	// Priority Queue operations
	NavigationNode* PopNodeFromList(NodeList& List);
	void PushNodeOntoList(NodeList& List, NavigationNode* pNode);
	void UpdateNodeInList(NodeList& List, NavigationNode* pNode);
	void RemoveNodeFromList(NodeList& List, NavigationNode* pNode);


	// Compare one navigation node to another
	friend bool operator == (NavigationNode& a, NavigationNode& b);

// Data
private:
	NavigationMap	m_NavMap;
	NodeList		m_Open;
	NodeList		m_Closed;

	CGameWorld*		m_pWorld;
	bool			m_bReady;
	NavigationCosts m_Costs;
};



#endif