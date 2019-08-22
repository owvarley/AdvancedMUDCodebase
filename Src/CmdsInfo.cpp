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

// File     :: CmdsInfo.cpp
// Header   :: CmdsInfo.h
// Function :: Holds the implementations for commands that belong in the Information category

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "CmdsInfo.h"
#include "Space.h"
#include "GameWorld.h"
#include "Room.h"
#include "../gTools/Log.h"

// Macro to define implementations
IMPLEMENT_CLASS(CmdGalaxy);		// Displays an overview of the galaxy
IMPLEMENT_CLASS(CmdEmotions);	// Displays a list of all available socials
IMPLEMENT_CLASS(CmdShips);		// Displays a list of all ships and allows the manipulation of them
IMPLEMENT_CLASS(CmdTeam);		// Displays the Staff List


// Method     :: CmdGalaxy
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to view all currently loaded Sectors of the Galaxy
// Written    :: 01/06/2005 {OWV}

bool CmdGalaxy::Perform(CActor* Ch, gStringList& CommandLine)
{
	SectorList::iterator sec;
	CSector* pSector = NULL;
	CSpatial* pSpatial = NULL;

		
	// Get the galaxy
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	Ch->Write("#202-#201[#702Galactic Sectors#201]#700#202-#700\n\r");
	int nSpa = 0;
	int nSec = 1;
	int nNo = 0;
	
	for (sec = pGalaxy->m_SectorList.begin(); sec != pGalaxy->m_SectorList.end(); sec++)
	{
		pSector = (*sec);
		if (nSec > 1)
			Ch->Write("\n\r");

		Ch->Write("#202 %d) %s [Size %d] #700\n\r", nSec, pSector->m_gsName, pSector->m_Location->Area()); //, pSector->m_Location->TRF()->Mark(), pSector->m_Location->TRF()->Bear(), pSector->m_Location->TRF()- );		
		Ch->Write("     #102-#101[#702Spatial Entities#101]#700#102-#700\n\r");

		for (gSpatialMap::iterator pos = pGalaxy->m_gSpatialMap.begin(); pos != pGalaxy->m_gSpatialMap.end(); pos++)	
		{
			CSpatial* pSpatial = (*pos).second;

			if (pSpatial->m_gsSector == pSector->m_gsName)
			{
				switch (pSpatial->m_nType)
				{ 
				case CSpatial::SO_PLANET:{
							CPlanet *pPlanet = (CPlanet*)(pSpatial);
							Ch->Write("     #102{PL} %s %s @ [%d %d %d]#700\n\r", pPlanet->m_gsName, (pPlanet->m_gsSector == "") ? "has no current sector" : "located in The " + pPlanet->m_gsSector, pPlanet->m_Location->x, pPlanet->m_Location->y, pPlanet->m_Location->z);
						}
						break;
				case CSpatial::SO_STAR: {
							CStar* pSat = (CStar*)(pSpatial);
							Ch->Write("     #602{ST} %s %s @ [%d %d %d] Energy: %d Dying: %s#700\n\r", pSat->m_gsName, (pSat->m_gsSector == "") ? "has no current sector" : "located in The " + pSat->m_gsSector, pSat->m_Location->x, pSat->m_Location->y, pSat->m_Location->z,  pSat->m_nEnergy, pSat->m_bDying ? "Yes" : "No");
						}
						break;
				case CSpatial::SO_ASTEROIDF: {
							CAsteroidField* pAsteroidField = (CAsteroidField*)(pSpatial);
							Ch->Write("     #401{AF} %s %s @ [%d %d %d] Size: %d Speed: %d#700\n\r", pAsteroidField->m_gsName, (pAsteroidField->m_gsSector == "") ? "has no current sector" : "located in The " + pAsteroidField->m_gsSector, pAsteroidField->m_Location->x, pAsteroidField->m_Location->y, pAsteroidField->m_Location->z,  pAsteroidField->m_AsteroidList.size(), pAsteroidField->m_nSpeed);
						}
						break;
				case CSpatial::SO_MOON: {
							CMoon* pMoon = (CMoon*)(pSpatial);
							Ch->Write("     #402{MO} %s %s @ [%d %d %d] Orbit: %d Planet: %s#700\n\r", pMoon->m_gsName, (pMoon->m_gsSector == "") ? "has no current sector" : "located in The " + pMoon->m_gsSector, pMoon->m_Location->x, pMoon->m_Location->y, pMoon->m_Location->z, pMoon->m_nOrbit, (pMoon->m_gsPlanet == "" ? "None" : pMoon->m_gsPlanet));
						}
						break;
				case CSpatial::SO_SAT: {
							CSat* pSat = (CSat*)(pSpatial);
							Ch->Write("     #502{SA} %s %s @ [%d %d %d] Owner: %s Power: %d Integrity: %d Orbit: %d Planet: %s#700\n\r", pSat->m_gsName, (pSat->m_gsSector == "") ? "has no current sector" : "located in The " + pSat->m_gsSector, pSat->m_Location->x, pSat->m_Location->y, pSat->m_Location->z,  (pSat->m_gsOwner == "" ? "None" : pSat->m_gsOwner), pSat->m_nPower, pSat->m_nIntegrity, pSat->m_nOrbit, (pSat->m_gsPlanet == "" ? "None" : pSat->m_gsPlanet));
						}
						break;
				case CSpatial::SO_BLACKHOLE: {
							CBlackhole* pBlack = (CBlackhole*)(pSpatial);
							Ch->Write("     #302{BH} %s %s @ [%d %d %d]#700\n\r", pBlack->m_gsName, (pBlack->m_gsSector == "") ? "has no current sector" : "located in The " + pBlack->m_gsSector, pBlack->m_Location->x, pBlack->m_Location->y, pBlack->m_Location->z);
						}
						break;
				case CSpatial::SO_SHIP: {
							CShip* pShip = (CShip*)(pSpatial);
							Ch->Write("     #602{SH} %s %s @ [%d %d %d]#700\n\r", pShip->m_gsName, (pShip->m_gsSector == "") ? "has no current sector" : "located in The " + pShip->m_gsSector, pShip->m_Location->x, pShip->m_Location->y, pShip->m_Location->z);
						}
						break;
				case CSpatial::SO_ASTEROID: {
							CAsteroid* pAsteroid = (CAsteroid*)(pSpatial);
							// We do not want to display any asteroids that belong to a field! too spammy!
							if (pAsteroid->m_gsField != "")
								continue;

							Ch->Write("     #301{AS} %s %s @ [%d %d %d] Size: %d Mineral: %d#700\n\r", pAsteroid->m_gsName, (pAsteroid->m_gsSector == "") ? "has no current sector" : "located in The " + pAsteroid->m_gsSector, pAsteroid->m_Location->x, pAsteroid->m_Location->y, pAsteroid->m_Location->z, pAsteroid->m_nSize, pAsteroid->m_nMineral);
						}
						break;
				default:{
							
							Ch->Write("     #102{SO} %s %s @ [%d %d %d] Mass: %d Gravity: %d#700\n\r", pSpatial->m_gsName, (pSpatial->m_gsSector == "") ? "has no current sector" : "located in The " + pSpatial->m_gsSector, pSpatial->m_Location->x, pSpatial->m_Location->y, pSpatial->m_Location->z, pSpatial->m_Signature[CSpatial::SI_MASS], pSpatial->m_nGravity);
						}
						break;
				}
				nSpa++;
			}

			if (pSpatial->m_gsSector == "")
				nNo++;
		}
		
		nSpa = 0;
		nSec++;

	}
	
	// Spatial Objects without Sectors
	if (nNo != 0)
	{  
		Ch->Write("\n\r#202 Unassigned Sector #700\n\r");
		Ch->Write("     #102-#101[#702Spatial Entities#101]#700#102-#700\n\r");

		for (gSpatialMap::iterator pos = pGalaxy->m_gSpatialMap.begin(); pos != pGalaxy->m_gSpatialMap.end(); pos++)	
		{
			CSpatial* pSpatial = (*pos).second;
			
			if (pSpatial->m_gsSector == "")
			{
				switch (pSpatial->m_nType)
				{ 
				case CSpatial::SO_PLANET:{
							CPlanet *pPlanet = (CPlanet*)pSpatial;
							Ch->Write("     #102{PL} %s %s @ [%0.0f %0.0f %0.0f]#700\n\r", pPlanet->m_gsName, (pPlanet->m_gsSector == "") ? "has no current sector" : "located in The " + pPlanet->m_gsSector, pPlanet->m_Location->Bear(), pPlanet->m_Location->Mark(), pPlanet->m_Location->Dist());
						}
						break;
				case CSpatial::SO_STAR: {
							CStar* pSat = (CStar*)pSpatial;
							Ch->Write("     #602{ST} %s %s @ [%0.0f %0.0f %0.0f] Energy: %d Dying: %s#700\n\r", pSat->m_gsName, (pSat->m_gsSector == "") ? "has no current sector" : "located in The " + pSat->m_gsSector, pSat->m_Location->Bear(), pSat->m_Location->Mark(), pSat->m_Location->Dist(),  pSat->m_nEnergy, pSat->m_bDying ? "Yes" : "No");
						}
						break;
				case CSpatial::SO_ASTEROIDF: {
							CAsteroidField* pAsteroidField = (CAsteroidField*)pSpatial;
							Ch->Write("     #401{AF} %s %s @ [%0.0f %0.0f %0.0f] Size: %d Speed: %d#700\n\r", pAsteroidField->m_gsName, (pAsteroidField->m_gsSector == "") ? "has no current sector" : "located in The " + pAsteroidField->m_gsSector, pAsteroidField->m_Location->Bear(), pAsteroidField->m_Location->Mark(), pAsteroidField->m_Location->Dist(),  pAsteroidField->m_AsteroidList.size(), pAsteroidField->m_nSpeed);
						}
						break;
				case CSpatial::SO_MOON: {
							CMoon* pMoon = (CMoon*)pSpatial;
							Ch->Write("     #402{MO} %s %s @ [%0.0f %0.0f %0.0f] Orbit: %d Planet: %s#700\n\r", pMoon->m_gsName, (pMoon->m_gsSector == "") ? "has no current sector" : "located in The " + pMoon->m_gsSector, pMoon->m_Location->Bear(), pMoon->m_Location->Mark(), pMoon->m_Location->Dist(), pMoon->m_nOrbit, (pMoon->m_gsPlanet == "" ? "None" : pMoon->m_gsPlanet));
						}
						break;
				case CSpatial::SO_SAT: {
							CSat* pSat = (CSat*)pSpatial;
							Ch->Write("     #502{SA} %s %s @ [%0.0f %0.0f %0.0f] Owner: %s Power: %d Integrity: %d Orbit: %d Planet: %s#700\n\r", pSat->m_gsName, (pSat->m_gsSector == "") ? "has no current sector" : "located in The " + pSat->m_gsSector, pSat->m_Location->Bear(), pSat->m_Location->Mark(), pSat->m_Location->Dist(),  (pSat->m_gsOwner == "" ? "None" : pSat->m_gsOwner), pSat->m_nPower, pSat->m_nIntegrity, pSat->m_nOrbit, (pSat->m_gsPlanet == "" ? "None" : pSat->m_gsPlanet));
						}
						break;
				case CSpatial::SO_BLACKHOLE: {
							CBlackhole* pBlack = (CBlackhole*)pSpatial;
							Ch->Write("     #302{BH} %s %s @ [%0.0f %0.0f %0.0f]#700\n\r", pBlack->m_gsName, (pBlack->m_gsSector == "") ? "has no current sector" : "located in The " + pBlack->m_gsSector, pBlack->m_Location->Bear(), pBlack->m_Location->Mark(), pBlack->m_Location->Dist());
						}
						break;
				case CSpatial::SO_SHIP: {
							CShip* pShip = (CShip*)pSpatial;
							Ch->Write("     #602{SH} %s %s @ [%0.0f %0.0f %0.0f]#700\n\r", pShip->m_gsName, (pShip->m_gsSector == "") ? "has no current sector" : "located in The " + pShip->m_gsSector, pShip->m_Location->Bear(), pShip->m_Location->Mark(), pShip->m_Location->Dist());
						}
						break;
				case CSpatial::SO_ASTEROID: {
							CAsteroid* pAsteroid = (CAsteroid*)pSpatial;
							// We do not want to display any asteroids that belong to a field! too spammy!
							if (pAsteroid->m_gsField != "")
								continue;

							Ch->Write("     #301{AS} %s %s @ [%0.0f %0.0f %0.0f] Size: %d Mineral: %d#700\n\r", pAsteroid->m_gsName, (pAsteroid->m_gsSector == "") ? "has no current sector" : "located in The " + pAsteroid->m_gsSector, pAsteroid->m_Location->Bear(), pAsteroid->m_Location->Mark(), pAsteroid->m_Location->Dist(), pAsteroid->m_nSize, pAsteroid->m_nMineral);
						}
						break;
				default:{
							
							Ch->Write("     #102{SO} %s %s @ [%0.0f %0.0f %0.0f] Mass: %d Gravity: %d#700\n\r", pSpatial->m_gsName, (pSpatial->m_gsSector == "") ? "has no current sector" : "located in The " + pSpatial->m_gsSector, pSpatial->m_Location->Bear(), pSpatial->m_Location->Mark(), pSpatial->m_Location->Dist(), pSpatial->m_Signature[CSpatial::SI_MASS], pSpatial->m_nGravity);
						}
						break;
				}
			}
		}
	}


	
	return true;

}

// Method     :: CmdEmotions
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to view all currently loaded Emotions
// Written    :: 01/06/2005 {OWV}

bool CmdEmotions::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGameWorld* pGame = CGameObjects::Get().GameWorld();
	int nCount = 1;
	
	for (EmoParsers::iterator emo = pGame->Emotions().begin(); emo != pGame->Emotions().end(); emo++)
		(*emo)->DescribeTo(Ch);

	if (nCount == 0)
	{
		Ch->Write("#702No Emotions created.#700\n\r");
	}

	return true;
}

// Method     :: CmdShips
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
//			  :: <view> <Ship #>
//			  :: <create> <template #> <name>
//			  :: <delete> <Ship #>
// Return     :: Bool
// Function   :: Used to view, create and delete Ships
// Written    :: 24/12/2005 {OWV}

bool CmdShips::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Get the galaxy
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CTLoader* pLoader = CGameObjects::Get().GameWorld()->TLoader();
	int nCount = 0;
	
	gString gsFunction = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Function to perform
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue3 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 3
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue4 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 4

	// Listing all Ships
	if (gsFunction == "" || gsFunction == "list")
	{
		if (pGalaxy->m_ShipList.size() == 0)
		{
			Ch->Write("No Ships exist. Use 'ship create <template Index no.>' to create Ship from a template.\n\rA list of valid templates can be found by typing: Designs\n\r");
			
		}
		else
		{
			Ch->Write("#500:#501:#701 Ship List #501:#500:#700\n\r");
			int nCount = 0;
			for (ShipList::iterator spa = pGalaxy->m_ShipList.begin(); spa != pGalaxy->m_ShipList.end(); spa++)
			{
				CShip* pShip = (CShip*)(*(*(*spa)));

				if (!pShip)
				{
					pGalaxy->m_ShipList.erase(spa);
					continue;
				}

				if (pShip->m_nType == CSpatial::SO_SHIP)
				{
					nCount++;
					Ch->Write("#500[#700 %3d#500]#701 %s #500<#501%s#500>#700\n\r", nCount, pShip->m_gsType, pShip->m_gsName);

				}

			}
			Ch->Write("#500[#701 %3d#500]#700 Ship%s in total\n\r", nCount, nCount > 1 ? "s" : "");
		}
		// End List of all Ships
	}
	// Create a ship from a Template
	if (gsFunction == "create")
	{
		if (
			gsValue1 == "" || atoi(gsValue1) > pLoader->m_Templates.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("<%d> is not a valid Template index, Choices: <1 - %d>.\n\r", atoi(gsValue1), pLoader->m_Templates.size());
			return true;
		}
		else
		{
			// Get the Template
			CTemplate* pTemplate = pLoader->m_Templates.at(atoi(gsValue1)-1);

			// We have selected a valid Template, now we need to check they provided a name for the ship
			if (gsValue2 == "")
			{
				Ch->Write("You must provide a name for this ship.\n\r");
				return true;
			}
			for (ShipList::iterator spa = pGalaxy->m_ShipList.begin(); spa != pGalaxy->m_ShipList.end(); spa++)
			{
				CShip* pShip = (CShip*)(*(*(*spa)));

				if (!pShip)
					pGalaxy->m_ShipList.erase(spa);

				if (pShip->m_nType == CSpatial::SO_SHIP)
				{
					if (pShip->m_gsName == gsValue2)
					{
						Ch->Write("There is already a ship called \"%s\" try another.\n\r", gsValue2);
						return true;
					}

				}

			}

			// We now have a unique name
			if (!pTemplate->m_Ship)
			{
				Ch->Write("The <%s> template has not yet been finished.\n\r", pTemplate->m_gsName);
				return true;
			}

			if (pTemplate->m_nStatus != CTemplate::_COMPLETED)
			{
				Ch->Write("The <%s> template has not yet been finished.\n\r", pTemplate->m_gsName);
				return true;
			}

			// #TODO# Some sort of checking algorithm is required in order to ensure the ship is complete


			// Copy this chosen ship to a new object in memory
			CShip* pShip = new CShip();
			*pShip = *(pTemplate->m_Ship);
			
			// Set the name fields
			pShip->m_gsType = pTemplate->m_Ship->m_gsType;			// Set the type e.g. Imperator Star Destroyer
			pShip->m_gsName = "PCS " + gsValue2;					// Set the name of the ship e.g. INS Tyrant
			pShip->m_gsDescription = pTemplate->m_Ship->m_gsType;
			
			pShip->m_gfFileName = pShip->m_gsName + ".shi";

			for (ShipList::iterator ship = pGalaxy->m_ShipList.begin(); ship != pGalaxy->m_ShipList.end(); ship++)
			{
				CShip* pOld = (CShip*)***ship; // * overloaded for CSpatialInt

				if (pShip->m_gsName == pOld->m_gsName)
				{
					Ch->Write("#100[#101Ship Exists#100]#700 A ship with that name already exists.\n\r");
					return true;
				}
			}

			// Need to set the area for the ship as the Template area

			// Get the Current GameWorld
			CGameWorld* pGame = CGameObjects::Get().GameWorld();

			// Get the Area
			CArea* pArea = new CArea;

			if (pTemplate->m_Area)
				*pArea = *pTemplate->m_Area;
			else
			{
				Ch->Write("[Critical Error] <%s> has no area.\n\r", pTemplate->m_gsName);
				g_Log.Log(LOG_ERROR, "[CCmdShips] <%s> has no area.\n\r", pTemplate->m_gsName);
				delete pShip;	// We want to delete the Memory assigned to the Ship as well
				return true;
			}

			pArea->SetName(pShip->m_gsName);
			pArea->SetFileName(pShip->m_gsName + ".are");
			pArea->SetShip(pShip->m_gsName);
			pArea->Flags()->RemoveBit(CArea::_TEMPLATE);	// Remove the Template flag for the area

			// We are creating a new area so we need a Unique ID for this area
			int nID = uiUniqueAreaID;
			pArea->SetArea(nID);
			
			// All our Rooms, Objects, Exits, Resets need to be updated with this Area ID
			// This is a crude hack, needs something more dynamic and solid #TODO#
			for (RoomMap::iterator it = pArea->Rooms()->begin(); it != pArea->Rooms()->end(); it++)
			{
				((*it).second)->SetArea(nID);
				for (ExitList::iterator ex = ((*it).second)->Exits().begin(); ex != ((*it).second)->Exits().end(); ex++)
				{
					// Problem is here, its not setting the exit placement right
					(*ex)->m_Destination = CPlacement(nID, (*ex)->Destination().Room(), (*ex)->Destination().World());

				}
			}
			
			pGame->Areas().push_back(pArea);
			pGame->SaveAll();



			// Need to assign each Component to the Ship
			for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
			{
				for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)

						(*comp)->m_gsShip = pShip->m_gsName;
			}

			// Generate the Shape
			pShip->m_Shape->SetLength(pShip);
			pShip->m_Shape->SetWidth(pShip);
			pShip->m_Shape->SetHeight(pShip);
			pShip->m_Shape->SetSize(pShip);

					
			// Temp for testing
			pShip->m_gsSector = "Corellian Sector";

			// Add it to our Ship List
			pGalaxy->AddShip(pShip->m_Vnum);
			// And to our Spatial list
			pGalaxy->AddSpatial(pShip);

			// For the moment lets add the ship to our Corellian Shipyard
			pShip->m_Land = CPlacement(2, 0, 0);
			pShip->m_ShipState->SetBit(CShip::_LANDED);
			CRoom* pRoom = CGameObjects:: Get().GameWorld()->GetArea(2)->GetRoom(0);
			pRoom->AddShip(pShip);

			// Save the Ship
			pShip->m_Area = pArea;
			pShip->m_nArea = pArea->Area();
			pShip->Save();

			// Save the Galaxy
			pGalaxy->Save();

			Ch->Write("%s <%s> created.\n\r", pShip->m_gsType, pShip->m_gsName);
			return true;
		}

	}
	// Remove a ship
	if (gsFunction == "delete")
	{
		CShip* pShip = NULL;

		if (gsValue1 == "")
		{
			Ch->Write("You must provide a name for the ship to delete.\n\r");
			return true;
		}

		if ( (pShip = pGalaxy->GetShi(gsValue1)) == NULL)
		{
			Ch->Write("No such ship exists.\n\r");
			return true;
		}

		if (pShip->m_ShipState->IsSet(CShip::_LANDED))
		{
			CArea *pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());

			if (pArea)
			{
				CRoom* pRoom = pArea->GetRoom(pShip->m_Land.Room());
				pRoom->RemShip(pShip->m_gsName);
			}
		}


		// Flag the Ship for deletion;
		pShip->m_bDelete = true;

		Ch->Write("%s deleted.\n\r", pShip->m_gsName);
		
		return true;

	}
	// Show a Ship
	if (gsFunction == "show")
	{
		CShip* pShip;

		if (gsValue1 == "")
		{
			Ch->Write("You must provide a name for the ship to view.\n\r");
			return true;
		}

		if ( (pShip = pGalaxy->GetShi(gsValue1)) == NULL)
		{
			Ch->Write("No such ship exists.\n\r");
			return true;
		}

		// Got the ship, lets show it
		Ch->Write(":: %s <%s> ::\n\r", pShip->m_gsType, pShip->m_gsName);
		Ch->Write(" Energy Load:\t%d\n\r", pShip->CurrPower());
		Ch->Write(" Power  Load:\t%d\n\r", pShip->MaxPower());
		Ch->Write(" Coolant Lvl:\t%d\n\r", pShip->m_nCoolant);

		Ch->Write(" >> Ship Batteries [%d]\n\r", pShip->Battery(true));
		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
			{
				if ((*comp)->m_Type == CComponent::CT_ENGINEERING)
				{
					CModule* pMod = ((CEngspace*)(*comp))->Get(CModule::MT_BATTERY_BANK);

					if (pMod)
					{
						nCount++;
						Ch->Write("  [%2d] - %s <%6d|%6d>\n\r", nCount, pMod->m_gsName, pMod->Plus("CurrCapacity", true), pMod->Plus("MaxCapacity", true));						
					}
				}
			}
		}

		Ch->Write(" Shield Status:\t<%d|%d>\n\r", pShip->Shield(true), pShip->Shield(false)); 

	}
	else if (gsFunction == "here")
	{
		nCount = 1;
		for (gStringList::iterator it = Ch->CurrentRoom()->Ships().begin(); it != Ch->CurrentRoom()->Ships().end(); it ++)
		{
			CShip* pShip = pGalaxy->GetShi(*it);
			Ch->Write("#600[#700 %2d#600]#701 %s #601:#700 %s\n\r", nCount, pShip->m_gsType, pShip->m_gsName);
			nCount++;
		}
	}
		

	return true;
}

// Method     :: CmdTeam
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to view the Diku style Wizlist
// Written    ::

bool CmdTeam::Perform(CActor* Ch, gStringList& CommandLine)
{

	return true;

}
