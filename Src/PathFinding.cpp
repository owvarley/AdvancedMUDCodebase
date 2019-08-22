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

// Class    :: CNpc
// Header   :: Npc.h
// Function :: This is Gary's navigation class that uses my own implementation of the A* algorithm.
//			:: My thanks to Bryan Stout and Amit Patel for their excellent references on the
//			:: subject. For the curious, the actual implementation of A* takes place inside
//			:: CNavigation::BuildPath(CDirectionList& List, CActor* pActor, CPlacement vTo)

#pragma warning(disable: 4786)

#include <algorithm>
#include <vector>
#include <map>

#include "Room.h"
#include "MudCore.h"
#include "Maths.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Profile.h"
#include "PathFinding.h"


CNavigator::CNavigator(CGameWorld* pWorld)
{
	m_pWorld = pWorld;

	Clear();
}

CNavigator::~CNavigator()
{
	Clear();
}

//
// Init
// This function goes through every world and adds references to every exit found
// in each room.  As the list should never change during game play (and if it does,
// calling Clear and Init() again should catch things up), then we can build the
// list once, and forget about it.  Pathfinding then will use this toned down list
// instead of iterating through everything again.
void CNavigator::Init()
{
	CRoom*		pRoom  = NULL;
	CExit*		pExit  = NULL;
	CArea*		pArea  = NULL;

	RoomMap::iterator RoomItor;
	AreaList::iterator AreaItor;
	ExitList::iterator ExitItor;

	if ( !m_pWorld || !m_bReady )
		return;

	// Setup our navigation costs.
	// Note that I've placed these into their own structure for readability purposes,
	// but also so that we dont have to make a hundred queries to ConfigData() every
	// time we calculate a traversal cost.  ConfigData() is fast, but not that fast...
	m_Costs.nDefaultCost			= Max(0, CGameObjects::Get().ConfigData().GetInt("cost_minimum", "PathFinding"));
	m_Costs.nCostToOpenDoor			= Max(0, CGameObjects::Get().ConfigData().GetInt("cost_to_open_door", "PathFinding"));
	m_Costs.nCostToOpenLockedDoor	= Max(0, CGameObjects::Get().ConfigData().GetInt("cost_to_open_locked_door", "PathFinding"));

	// For each area in this world
	for ( AreaItor = m_pWorld->Areas().begin();
		  AreaItor != m_pWorld->Areas().end();
		  AreaItor++ )
	{
		if ( (pArea = (*AreaItor)) != NULL )
		{
			// For each room in this area
			for ( RoomItor = pArea->Rooms()->begin();
				  RoomItor != pArea->Rooms()->end();
				  RoomItor++ )
			{
				if ( (pRoom = (*RoomItor).second) != NULL )
				{
					NavigationPoint* pPoint = new NavigationPoint;

					pPoint->vLocation = pRoom->Position();

					// For each exit in this room....
					for ( ExitItor = pRoom->Exits().begin();
						  ExitItor != pRoom->Exits().end();
						  ExitItor++ )
					{
						if ( (pExit = (*ExitItor)) != NULL )
						{
							NavigationExit* pNav = new NavigationExit;

							pNav->vDestination = pExit->Destination();
							pNav->nDir = pExit->Direction();

							pPoint->Exits.push_back(*pNav);
						}
					}

					gString gsPosHash;
					gsPosHash.Format("%d#%d#%d", pRoom->Position().Area(), pRoom->Position().Room(), pRoom->Position().World());
					m_NavMap[ gsPosHash ] = pPoint;
				}
			}
		}
	}

	if  ( m_NavMap.size() > 0 )
		m_bReady = true;
}

void CNavigator::Clear()
{
	NavigationMap::iterator itor;

	for (itor = m_NavMap.begin(); itor!=m_NavMap.end(); itor++)
		delete (*itor).second;

	m_NavMap.clear();
}

bool CNavigator::BuildPathTo(CDirectionList& List, CActor* pActor, CPlacement vTo)
{
	return BuildPath(List, pActor, vTo);
}

bool CNavigator::BuildPathTo(CDirectionList& List, CActor* pActor, CRoom* pRoom)
{
	return BuildPath(List, pActor, pRoom->Position());
}

bool CNavigator::BuildPathTo(CDirectionList& List, CActor* pFrom, CActor* pTo)
{
	return BuildPath(List, pFrom, pTo->Position());
}

bool CNavigator::BuildPath(CDirectionList& List, CActor* pActor, CPlacement vTo)
{
	NavigationMap::iterator pos;
	NavigationNode* pNode		= NULL;
	NavigationNode* pTestNode	= NULL;
	bool bFindStraightestRoute	= CGameObjects::Get().ConfigData().GetBool("find_straight_route", "PathFinding");
	bool bRunProfiler			= CGameObjects::Get().ConfigData().GetBool("profile_navigation", "PathFinding");
	static CProfile* pProfile	= CGameObjects::Get().m_ProfileMgr.Find("path_finding");

	if ( bRunProfiler )
		assert(pProfile && "Hey, someone took my profiler!");

	// We get the starting location from the actors position, so we need an actor.
	// and if the navigator hasn't already been initialized, then of course we cant
	// continue.
	if ( !pActor || !m_bReady )
		return false;

	// Ok, if the desired location happens to be the location the actor is in, then
	// sure, we found the path... return TRUE but leave an empty list.
	if ( pActor->Position() == vTo )
		return true;

	if ( bRunProfiler )
		pProfile->Start();

	// If the location cannot be found in our navigation map, then some odd error
	// has occured, we cannot continue.
	gString gsPosHash;
	gsPosHash.Format("%d#%d#%d", pActor->Position().Area(), pActor->Position().Room(), pActor->Position().World());

	if ( (pos = m_NavMap.find(gsPosHash)) == m_NavMap.end() )
	{
		if ( bRunProfiler )
			pProfile->Stop();

		return false;
	}

	// Empty out our open and closed lists
	m_Open.clear();
	m_Closed.clear();

	// Create a starting node from our current position
	pNode = new NavigationNode;
	pNode->nvPoint = (NavigationPoint)*(*pos).second;

	m_Open.push_back(pNode);

	// A* implementation
	while ( !m_Open.empty() )
	{
		int nDir = CExit::NONE;

		pTestNode = PopNodeFromList(m_Open);

		// if at a goal, build our list of directions and return
		if ( pTestNode->nvPoint.vLocation == vTo )
		{
			// Ok, we have a node with or without a parent.  What we need to do
			// now is build a list of directions from each parent to this node
			List.Clear();
			NavigationNode* pLast   = pTestNode;
			NavigationNode* pParent = pTestNode->pParent;

			while ( pParent != NULL )
			{
				// Ok, given a destination, and a list of exits, one of which leads
				// to that destination, locate the specific destination and return
				// it as a direction
				nDir = LocateExit(pLast->nvPoint.vLocation, pParent->nvPoint.Exits);
				assert(nDir != CExit::NONE);

				// Add the direction to the route
				List.AddRoute(nDir);

				pLast = pParent;
				pParent = pParent->pParent;
			}

			delete pNode;

			if ( bRunProfiler )
				pProfile->Stop();

			return true;
		}
		else
		{
			// At this point, we have a Node which contains a list of exits
			// to other rooms.  We need to examine each exit, and add the room
			// it leads to to the open list
			NavExitList::iterator ExItor;

			for (ExItor = pTestNode->nvPoint.Exits.begin();
				 ExItor != pTestNode->nvPoint.Exits.end();
				 ExItor++)
			{
				NavigationNode* pNewNode = new NavigationNode;
				pNewNode->pParent = pTestNode;

				// Setup the new nodes nvPoint information, based on our
				// map of rooms.

				gString gsPosHash;
				gsPosHash.Format("%d#%d#%d", (*ExItor).vDestination.Area(), (*ExItor).vDestination.Room(), (*ExItor).vDestination.World());

				pos = m_NavMap.find( gsPosHash );

				assert(pos != m_NavMap.end());
				pNewNode->nvPoint = (NavigationPoint)*(*pos).second;

				// Determine the cost to move from our current location, to the new
				// location. See the comments for the TraversalCost function
				int nNewCost = pTestNode->nCostFromStart + TraversalCost(pTestNode, pNewNode, pActor);

				// If this node has been visited allready, and the route was cheaper then,
				// then use those figures instead.
				if ( (pNewNode->bOnOpen || pNewNode->bOnClose) && pNewNode->nCostFromStart <= nNewCost )
					continue;

				// Ok, we either have a new node, or a node we've visited before but
				// at a more expensive cost, so... update the movement cost
				pNewNode->nCostFromStart = nNewCost;
				pNewNode->nCostToGoal = EstimatePathCost(pNode, pNewNode, pActor);

				if ( bFindStraightestRoute )
				{
					// Essentially, what we want to do here is pick the straightest route
					// to the target. The method I use to do that simply checks to see if
					// the new direction differs from the last direction, and if so, it
					// adds 1 to the traversal cost.
					if (pTestNode->pParent && pNewNode->pParent)
					{
						int nThis, nLast;

						nLast = LocateExit(pTestNode->nvPoint.vLocation, pTestNode->pParent->nvPoint.Exits);
						nThis = LocateExit(pNewNode->nvPoint.vLocation, pNewNode->pParent->nvPoint.Exits);

						if ( nLast != nThis )
							pNewNode->nCostToGoal++;
					}
				}

				if ( pNewNode->bOnClose )
					RemoveNodeFromList(m_Closed, pNewNode);

				if ( pNewNode->bOnOpen )
					UpdateNodeInList(m_Open, pNewNode);
				else
					PushNodeOntoList(m_Open, pNewNode);
			}
		}

		PushNodeOntoList(m_Closed, pTestNode);
	}

	delete pNode;


	if ( bRunProfiler )
		pProfile->Stop();

	return false;
}

// LocateExit
// Given a destination and a list of exits, one of which leads to that
// destination, locate the specific destination and return it as a direction
int CNavigator::LocateExit(CPlacement vTo, NavExitList& List)
{
	NavExitList::iterator itor;

	for (itor = List.begin(); itor != List.end(); itor++)
		if ( (*itor).vDestination == vTo )
			return (*itor).nDir;

	return CExit::NONE;
}


// TraversalCost
// This function should take terrain types and actor movement types
// into account. For example, a man who has a skill in swimming moving
// into a river might have a traversal cost of say, 2, where a man
// with no skill in swimming might have a cost of 8.
// A man walking up a mountain might have a cost of 6 where a goat walking
// up the same trail might have a cost of 2, etc.
// It might be 2x as hard to walk down the street if it's in the middle of
// a thunderstorm, etc...  You get the idea.
int  CNavigator::TraversalCost(NavigationNode* pFrom, NavigationNode* pTo, CActor* pActor)
{
	CArea* pArea	= NULL;
	CRoom* pRoomTo  = NULL;
	CRoom* pRoomFrom= NULL;
	CExit* pExit	= NULL;
	int nCost		= m_Costs.nDefaultCost;

	// We will need the area,room & exit information for this particular
	// spot to more accurately determine the costs

	// Locate the area we are travelling TO
	pArea = m_pWorld->GetArea(pTo->nvPoint.vLocation);
	assert(pArea && "Navigator could not locate the area. This is a critical error that should never occur.");

	// Locate the room we are travelling FROM
	pRoomFrom = pArea->GetRoom(pFrom->nvPoint.vLocation);
	assert(pRoomFrom && "Navigator could not locate the room. This is a critical error that should never occur.");

	// Locate the room we are travelling TO
	pRoomTo = pArea->GetRoom(pTo->nvPoint.vLocation);
	assert(pRoomTo && "Navigator could not locate the room. This is a critical error that should never occur.");

	// Given our current room, locate the exit that leads to the room we are travelling to
	CExit::_Directions Dir = (CExit::_Directions)LocateExit(pTo->nvPoint.vLocation, pFrom->nvPoint.Exits);
	assert(Dir != CExit::NONE && "Navigator could not locate the exit direction. This is a critical error that should never occur.");

	// Grab that exit from the room
	pExit = pRoomFrom->GetExit(Dir);
	assert(pExit && "Navigator could not locate the exit. This is a critical error that should never occur.");

	// Since this is going to be very implementation specific, I've considered wether
	// or not the exit is closed and locked, and left the rest up to the implementor.
	// At this point though, you should have all the information you need to determine
	// basic and complex movement costs.
	//
	// Actor doing the moving		== pActor
	// Direction of travel			== Dir
	// Area moving to				== pArea
	// Room moving from				== pRoomFrom
	// Room moving to				== pRoomTo
	// Exit moving through			== pExit

	// You may need the area moving from, which would be in
	// m_pWorld->GetArea( pActor->Position() );

	if ( pExit->ExitFlags()->IsSet(CExit::_CLOSED) )
		nCost += m_Costs.nCostToOpenDoor;

	if ( pExit->ExitFlags()->IsSet(CExit::_LOCKED) )
		nCost += m_Costs.nCostToOpenLockedDoor;

	return nCost;
}

// ExtimatePathCost
// Return an estimate of the cost to move from the current location to the goal
int  CNavigator::EstimatePathCost(NavigationNode* pFrom, NavigationNode* pTo, CActor* pActor)
{
	// This really should return an estimate of the distance between the two points,
	// but I dont know how to do this in a non-contiguous space such as our worlds.
	// Anyway, a value of 1 seems to work fine.
	return 1;
}

// PopNodeFromList
// Pull a node from the front of the list.  Since we are using a std::vector,
// we have to resort the list to give it the properties of a priority queue
NavigationNode* CNavigator::PopNodeFromList(NodeList& List)
{
	NavigationNode* pNode = List.front();

	std::pop_heap(List.begin(), List.end(), WhichNodeIsGreater());

	List.pop_back();

	if ( List == m_Open )
		pNode->bOnOpen = false;
	else
		pNode->bOnClose = false;

	return pNode;
}

// PushNodeFromList
// Push a node onto the back of the list.  Since we are using a std::vector,
// we have to resort the list to give it the properties of a priority queue
void CNavigator::PushNodeOntoList(NodeList& List, NavigationNode* pNode)
{
	if ( List == m_Open )
		pNode->bOnOpen = true;
	else
		pNode->bOnClose = true;

	// Put the node on the end of the vector, then re-sort it.
	List.push_back(pNode);

	std::push_heap(List.begin(), List.end(), WhichNodeIsGreater());
}

// UpdateNodeInlist
// Update the given node's information in the list. This will require
// a resort of the list.
void CNavigator::UpdateNodeInList(NodeList& List, NavigationNode* pNode)
{
	NodeList::iterator pos;

	for (pos=List.begin(); pos!=List.end(); pos++)
	{
		if ( pNode == *(pos) )
		{
			std::push_heap(List.begin(), pos+1, WhichNodeIsGreater());
			return;
		}
	}
}

// RemoveNodeFromList
// Delete the node from the list. Since only m_Closed uses this, and m_Closed
// does not have to act like a priority queue, there is no need to sort the list
// afterwards.
void CNavigator::RemoveNodeFromList(NodeList& List, NavigationNode* pNode)
{
	NavigationNode* pNavNode = NULL;

	if ( List == m_Open )
		pNode->bOnOpen = false;
	else
		pNode->bOnClose = false;

	NodeList::iterator pos;

	for (pos=List.begin(); pos != List.end(); pos++)
	{
		pNavNode = *(pos);

		if ( pNavNode == pNode )
		{
			List.erase(pos);
			delete pNavNode;
			return;
		}
	}
}

// Operator ==
// Compare one Navigation node to another
bool operator == (NavigationNode& a, NavigationNode& b)
{
	return (a.nvPoint.vLocation == b.nvPoint.vLocation);
}

//
// Some serialization helpers so we can easily store our Direction lists
//
std::ostream& operator << ( std::ostream& stream, const CDirectionList& List )
{
	int n;
	int nSize = List.m_List.size();

	stream << "<" << nSize << ",";

	for (n=0; n<nSize; n++)
	{
		stream << (long)List.m_List[n];
		if ( (n+1)<nSize )
		  stream << ",";
	}
	stream << ">\n";

	return stream;
}


std::istream& operator >> ( std::istream& stream, CDirectionList& List )
{
	int n=0, nSize;
	char c;

	stream >> c >> nSize >> c;

	List.Clear();

	for (n=0; n<nSize; n++)
	{
		int nEntry;
		stream >> nEntry >> c;

		List.AddRoute(nEntry);
	}

	return stream;
}


