//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// File     :: CmdsMove.cpp
// Header   :: CmdsMove.h
// Function :: Holds the implementations for commands that belong in the Movement category


#include "MudCore.h"
#include "CmdsMove.h"
#include "GameWorld.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "Room.h"
#include "../gTools/Log.h"


IMPLEMENT_CLASS(CmdMove);	// Allows a player to move using the Cardinal directions
IMPLEMENT_CLASS(CmdBoard);	// Allows a player to board a ship
IMPLEMENT_CLASS(CmdLeave);	// Allows a player to leave a ship

// Method     :: CmdMove
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <west|east|north|south|up|down>
// Return     :: Bool
// Function   :: Used to move about the GameWorld
// Written    :: Original {GMN}, Last Updated 01/02/2006 {OWV}
// Updated    :: [01/02/06] To prevent movement depependant upon actor position {OWV}

bool CmdMove::Perform(CActor* Ch, gStringList& CommandLine)
{
	ExitList::iterator pos;
	CRoom *pR;
	CExit *pE;
	gString sDirection = (gString)(*CommandLine.begin());

	sDirection.MakeUpper();
	pR = Ch->CurrentRoom();

	if ( !pR )
		return false;

	// Added to prevent movement dependant upon position
	// 01/02/2006 {OWV}
	if (Ch->ActorPositions()->IsSet(CActor::_SITTING))
	{
		Ch->Write("You cant move while sitting!\n\r");
		Ch->Write("Try standing up first\n\r");
		return true;
	}
	else if (Ch->ActorPositions()->IsSet(CActor::_CROUCHED))
	{
		Ch->Write("You cant move while crouched!\n\r");
		Ch->Write("Try standing up first\n\r");
		return true;
	}

	// Alias shortcut conversions
	if (sDirection == "SE")
		sDirection = "SOUTHEAST";
	if (sDirection == "NE")
		sDirection = "NORTHEAST";
	if (sDirection == "SW")
		sDirection = "SOUTHWEST";
	if (sDirection == "NW")
		sDirection = "NORTHWEST";


	for (pos=pR->Exits().begin(); pos != pR->Exits().end(); pos++)
	{
		pE = (CExit*)(*pos);

		gString gsExit = CExit::szExitNames[ pE->m_Direction ];

		if ( gsExit.HasPrefix(sDirection) )
		{
			Ch->HomeWorld()->MoveTo(Ch, pE->m_Destination );
			return true;
		}
	}

	sDirection.MakeLower();
	Ch->Write("There is no %s exit from this Room!\n\r", sDirection);
	return true;
}



// Method     :: CmdBoard
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <ship> to Board
// Return     :: Bool
// Function   :: Allows a Player to Enter a ship
// Written    :: 09/01/2006 {OWV}

bool CmdBoard::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	if (gsValue == "")
	{
		Ch->Write("[Invalid Input] You must enter the name of the Ship you wish to board.\n\r");
		Ch->Write("Syntax: Board <Shipname>\n\r");
		Ch->Write("To view the ships here type: Ships here\n\r");
		return true;
	}

	if (!Ch->CurrentRoom()->ShipHere(gsValue))
	{
		Ch->Write("You do not see '%s' here.\n\r", gsValue);
		return true;
	}

	CShip* pShip = pGalaxy->GetShi(gsValue);

	// We should always get a ship, but if we dont then the room contains a ship
	// that no longer exists
	if (!pShip)
	{ 
		Ch->Write("[Invalid Ship] Room contains invalid Ship.\n\r");
		Ch->CurrentRoom()->RemShip(gsValue);
		return true;
	}
	else
	{
		if (!pShip->m_ShipState->IsSet(CShip::_LANDED))
		{
			Ch->Write("The Ship must be on the ground before you can board it!\n\r");
			return true;
		}

		// Got a ship lets try and board it
		if (pShip->m_nExitState == CShip::ES_CLOSED)
		{
			Ch->Write("The %s on %s:%s is closed.\n\r", pShip->m_gsExit, pShip->m_gsType, pShip->m_gsName);
			return true;
		}
		else
		{
			// Is it open?
			if (pShip->m_nExitState != CShip::ES_OPEN)
			{
				Ch->Write("The %s is closed.\n\r", pShip->m_gsExit);
				return true;
			}

			// Board them
			CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Area->Area());


			// Is the area loaded and valid
			// TODO LAZY LOADING 
			if (!pArea)
			{
				Ch->Write("[Invalid Area] Invalid Error, Administration notified.\n\r");
				g_Log.Log(LOG_ERROR, "[CmdBoard::>>] Ship %s has invalid area!", pShip->m_gsName);							
			}
			CRoom* pTo = pArea->GetRoom(pShip->m_nExit);
			CRoom* pRoom = Ch->CurrentRoom();

			if (!pTo)
			{
				Ch->Write("[Incomplete Design] This ship has no entrance.\n\r");
				return true;
			}

			// Remove them from this Room
			pRoom->Remove(Ch);
			pRoom->Write("%s boards %s:%s\n\r", Ch->Name(), pShip->m_gsType, pShip->m_gsName);
			// Add them to the new one
			pTo->Write("%s boards the ship.\n\r", Ch->Name());
			pTo->Add(Ch);

			Ch->SetCurrentRoom(pTo);
			CGameObjects::Get().GameWorld()->Describe(Ch, Ch->Position(), true );
			Ch->Write("You board %s.\n\r", pShip->m_gsName);
		}
	}




	return true;

}

// Method     :: CmdLeave
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to leave a Ship
// Written    :: 09/01/2006 {OWV}

bool CmdLeave::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	CShip* pShip = Ch->CurrentRoom()->GetShip();
	
	if (!pShip)
	{
		Ch->Write("You are not onboard a ship.\n\r");
		return true;
	}

	if (!pShip->m_ShipState->IsSet(CShip::_LANDED))
	{
		Ch->Write("The Ship must be on the ground before you can disembark\n\r");
		return true;
	}

	CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());
	CRoom* pRoom = pArea->GetRoom(pShip->m_Land.Room());

	if (!pRoom)
	{
		g_Log.Log(LOG_ERROR, "[CmdLeave::>>] Ship %s has invalid area!", pShip->m_gsName);
		Ch->Write("[Invalid Area] Ship has invalid area.\n\r");
		return true;
	}
	else
	{	
		// Is it open?
		if (pShip->m_nExitState != CShip::ES_OPEN)
		{
			Ch->Write("The %s is closed.\n\r", pShip->m_gsExit);
			return true;
		}

		// Disembark them
		CRoom* pFrom = Ch->CurrentRoom();

		// Remove them from this Room
		pFrom->Remove(Ch);
		pFrom->Write("%s disembarks the ship.\n\r", Ch->Name(), pShip->m_gsType, pShip->m_gsName);
		// Add them to the new one
		pRoom->Write("%s disembarks from %s:%s\n\r", Ch->Name(), pShip->m_gsType, pShip->m_gsName);
		pRoom->Add(Ch);

		Ch->SetCurrentRoom(pRoom);
		CGameObjects::Get().GameWorld()->Describe(Ch, Ch->Position(), true );
		Ch->Write("You disembark from %s.\n\r", pShip->m_gsName);
	}

	return true;

}