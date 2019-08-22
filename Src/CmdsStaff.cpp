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

// File     :: CmdsStaff.cpp
// Header   :: CmdsStaff.h
// Function :: Holds the implementations for commands that belong in the Staff category

#include "MudCore.h"
#include "Actor.h"
#include "Player.h"
#include "CmdsStaff.h"
#include "GameObjects.h"
#include "GameWorld.h"
#include "GameServer.h"
#include "Room.h"
#include "OTools.h"
#include "../gTools/Log.h"
#include <Math.h>
#include "Race.h"
#include <vector>


IMPLEMENT_CLASS(CmdGoto);			// Allows a staff member to goto an area/player
IMPLEMENT_CLASS(CmdShutdown);		// Allows an admin to shutdown the mud
IMPLEMENT_CLASS(CmdCreateSector);	// Allows a staff member to create a sector
IMPLEMENT_CLASS(CmdSector);			// Allows a staff member to edit a sector
IMPLEMENT_CLASS(CmdSpatial)			// Allows a staff member to create, edit and delete spatial objects
IMPLEMENT_CLASS(CmdPromote);		// Allows a staff member to promote another player/staff member
IMPLEMENT_CLASS(CmdDemote);			// Allows a staff member to demote another player/staff member
IMPLEMENT_CLASS(CmdTest);			// Admin function used to test various parts of the game
IMPLEMENT_CLASS(CmdConfig);			// Admin function that allows areas of the MUD to be modified
IMPLEMENT_CLASS(CmdModule);			// Staff command for viewing, editing and creating modules
IMPLEMENT_CLASS(CmdCrews);			// Allows the creation of ship crews
IMPLEMENT_CLASS(CmdMotd);			// Used to view and edit the motd
IMPLEMENT_CLASS(CmdRace);			// Used to view races
IMPLEMENT_CLASS(CmdHullcube);		// Staff command for creation and editing of hullcubes
IMPLEMENT_CLASS(CmdFreeze);			// Ability to freeze staff members

// Method     :: CmdGoto
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Area Id> <Room #>
//			  :: <Player Name>
// Return     :: Bool
// Function   :: Allows a Staff member to goto an area/player
// Written    :: 6/12/05 {OWV}, Last Updated 19/3/06 {OWV}
// Updated    :: [19/3/06] Added ability to goto Players {OWV}

bool CmdGoto::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();

	bool bPlayer = false;
	CActor* pTarget = NULL;
	CArea* pArea = NULL;
	CRoom* pRoom = NULL;

	// [1] - Is the argument a player?
	for (ActorMap::iterator pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
	{
		try 
		{
			if ( Ch->HomeWorld()->Players().size() == 0 )
				break;

			if ((*pos).second->Name().HasPrefix(gsValue))
			{
				pTarget = (*pos).second;
				bPlayer = true;								
			}

		}
		catch (...) {break;}
	}

	// Get the game World so we can check if the Area is valid
	CGameWorld* pWorld = CGameObjects::Get().GameWorld();

	// [2] If its not a player is it an area?
	if (!bPlayer)
	{

		// Try and get the Area
		pArea = pWorld->GetArea(atoi(gsValue));

		// Not a valid Area
		if (!pArea)
		{
			Ch->Write("<%d> Is not a valid Area Index. Valid values <1 - %d>\n\r", atoi(gsValue), uiUniqueAreaID);
			Ch->Write("Syntax: Goto <Area Index> <Room Index>\n\r");
			return true;
		}

		pRoom = pArea->GetRoom(atoi(gsValue2));

		// Not a valid room
		if (!pRoom)
		{
			Ch->Write("<%d> Is not a valid Room Index. Valid values <0 - %d>\n\r", atoi(gsValue2), pArea->Rooms()->size());
			return true;
		}
	}
	else
	{
		pArea = pTarget->CurrentRoom()->GetArea();
		pRoom = pTarget->CurrentRoom();

		if (!pArea || !pRoom)
		{
			Ch->Write("#100[#101Target Error#100]#700 Goto target is in an invalid area.\n\r");
			g_Log.Log("Goto target <%s> is within an invalid area/room", pTarget->Name());
			return true;
		}
	}

	Ch->CurrentRoom()->Write(Ch, "%s disappears from the room!\n\r", Ch->Name());

	// Move the player
	Ch->SetCurrentRoom(pRoom);

	// Show them the Description
	pWorld->Describe(Ch, Ch->Position(), true);

	Ch->CurrentRoom()->Write(Ch, "%s appears in the room!\n\r", Ch->Name());


	return true;
}

// Method     :: CmdShutdown
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows an administrator to shutdown the MUD
// Written    :: Original {GNM}

bool CmdShutdown::Perform(CActor* Ch, gStringList& CommandLine)
{
	LOG_SCOPE("CCommand::Shutdown");
	gString gsMessage = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);
	CGameObjects& globals = CGameObjects::Get();


	Ch->Write("Shutting Down now...");

	// Did they supply a message
	if (gsMessage != "")
	{
		ENotice* event = new ENotice;
		event->gsNotice.Format("#101[#100Shutdown by %s#101]#701 %s #700\n\r", Ch->Name(), gsMessage);

		for (ActorMap::iterator pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
		{
			try 
			{
				if ((*pos).second != NULL )
					(*pos).second->ReceiveEvent(*event);		
				
			}
			catch (...) {break;}
		}
	}

	g_Log.Log(LOG_INFO, "Shutting down the system!\n\r");
	globals.GameServer()->State() = CGameServer::_SHUTDOWN;
	return true;
}

// Method     :: CmdCreateSector
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Name> <FileName>
// Return     :: Bool
// Function   :: Used to create a new Sector and save it
// Written    :: 01/06/05 {OWV}

bool CmdCreateSector::Perform(CActor* Ch, gStringList& CommandLine)
{
	SectorList::iterator sec;
	CGameObjects& globals = CGameObjects::Get();

	// Validate arguments
	gString gsSecName = (CommandLine.empty()) ? "" : *CommandLine.begin();
	gString gsFilename = (CommandLine.empty()) ? "" : CommandLine.back();

	// Make sure neither are blank
	if ( gsSecName == "" || gsFilename == "")
	{
		Ch->Write("You must input both a Name and a Filename for the Sector!\n\r");
		Ch->Write("Syntax: CreateSector <name> <filename>\n\r");
		return true;
	}
	

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	// Add .sec extension
	gsFilename.Format("%s.sec", gsFilename);

	// Check for already existing Sectors
	for (sec = pGalaxy->m_SectorList.begin(); sec != pGalaxy->m_SectorList.end(); sec++)
	{
		CSector* pSector = (*sec);
		
		// Makesure Sector doesn't exist
		if (pSector->m_gsName == gsSecName)
		{
			Ch->Write("A Sector with that name already exists!\n\r");
			return true;
		}

		// Makesure Filename doesn't exist
		if (pSector->m_gsFileName == gsFilename)
		{
			Ch->Write("A Sector with that Filename already exists!\n\r");
			return true;
		}
	}

	// Create Object
	CSector* pNewSector = new CSector(gsSecName, gsFilename);

	// Save
	if (!pNewSector->Save(pNewSector->m_gsFileName))
	{
		g_Log.Log(LOG_ERROR, "[CmdsStaff::CreateSector] Unable to save new Sector.");
		Ch->Write("Unable to save new Sector.\n\r");
		return true;
	}
	
	// Add to SectorList
	pGalaxy->m_SectorList.push_back(pNewSector);

	// Save Galaxy
	if (!pGalaxy->Save())
	{
		g_Log.Log(LOG_ERROR, "[CmdsStaff::CreateSector] Unable to save Galaxy.");
		Ch->Write("Unable to save Galaxy.\n\r");
		return true;
	}
	

	Ch->Write("Sector created.\n\r");
	return true;
}

// Method     :: CmdSector
// Class	  :: CSector
// Parameters :: <actor, arguments>
// Arguments  :: <name> <field> <value>
// Return     :: Bool
// Function   :: Used for editing Sectors
// Written    :: 09/06/05 {OWV}

bool CmdSector::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGameObjects& globals = CGameObjects::Get();
	CSector* pSector;
	CGalaxy* pGalaxy = globals.GameWorld()->Galaxy();
	gString gsValue;
	std::vector<gString> CartValues;
	
	// Validate arguments
	gString gsSecName = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Sector Name
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsField = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Field
	if (!CommandLine.empty())
		CommandLine.pop_front();
	if (gsField != "location")
	{
		gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Value
		if (!CommandLine.empty())
			CommandLine.pop_front();
	}
	else
	{
		while (!CommandLine.empty())
		{
			CartValues.push_back(*CommandLine.begin());
			CommandLine.pop_front();
		}

		if (CartValues.size() < 24)
		{
			Ch->Write("To define the bounds of a Sector you must provide 24 values:\n\r");
			Ch->Write("tRb tRf tLb tLf bRb bRf bLb bLf\n\r");
			return true;
		}

	}

	
	if (gsField == "" ||  gsSecName == "" || (gsValue == "" && gsField != "area" && gsField != "delete" && gsField != "location"))
	{
		Ch->Write("Incorrect Syntax:\n\r");
		Ch->Write("Sector <sectorname> <field> <value>\n\r");
		return true;
	}

	// Valid Sector
	if ( (pSector = pGalaxy->GetSec(gsSecName)) == NULL)
	{
		Ch->Write("That Sector doesn't exist!\n\r");
		return true;
	}

	// Changes [Name, Explored, Location, Area] or Delete
	if (gsField == "Name")
	{
		pSector->m_gsName = gsValue;		
	}
	else if (gsField == "Explored")
	{
		if (gsValue == "0" || gsValue == "no" || gsValue == "false")
			pSector->m_Explored = false;
		else if (gsValue == "1" || gsValue == "yes" || gsValue == "true")
			pSector->m_Explored = true;
		else
		{
			Ch->Write("Invalid Value.\n\r");
			Ch->Write("Valid Choices are: yes, no");
			return true;
		}
			
	}
	else if (gsField == "Location")
	{
		// Updated to make it easy
		CCartBound* pCartBound = new CCartBound(atoi(*CartValues.begin()), atoi(*(CartValues.begin()+1)), atoi(*(CartValues.begin()+2)), 0, 0, 0);

		pSector->m_Location = pCartBound;


		Ch->Write("Sector Location set.\n\r");
		return true;
	}
	else if (gsField == "Delete")
	{
		if (pGalaxy->RemoveSector(pSector))
		{
			Ch->Write("Sector Deleted.\n\r");
			return true;
		}
		else
		{
			Ch->Write("Unable to delete Sector.\n\r");
			return true;
		} 
		Ch->Write("Disabled.\n\r");
		return true;
	}
	else
	{
		Ch->Write("Invalid Field.\n\r");
		Ch->Write("Valid options are: Name, Explored, Location, Area\n\r");
		return true;
	}
	
	Ch->Write("Changes Made.\n\r");
	return true;

}

// Method     :: CmdSpatial
// Class	  :: CSector
// Parameters :: <actor, arguments>
// Arguments  :: <name> <type:planet,star,etc> <function:create,edit,delete> <value>
// Return     :: Bool
// Function   :: Used for editing/creation of Spatial objects
// Written    :: 09/06/05 {OWV}

bool CmdSpatial::Perform(CActor* Ch, gStringList& CommandLine)
{
	SpatialList::iterator spa;
	AsteroidList::iterator ast;
	std::vector<gString> LocationValues;
	gString gsField;
	gString gsValue;


	// Validate arguments
	// Spatial Type Field Value
	// If not creating then the type argument becomes the Field
	gString gsSpatial = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Object name
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsType = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Type
	if (!CommandLine.empty())
		CommandLine.pop_front();	
	if (gsType != "location")
	{
		gsField = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Function
		if (!CommandLine.empty())
			CommandLine.pop_front();
		gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Value for create/delete
	}
	else
	{
		gsField = "a";
		gsValue = "a";
		while (!CommandLine.empty())
		{
			LocationValues.push_back(*CommandLine.begin());
			CommandLine.pop_front();
		}

		if (LocationValues.size() != 3)
		{
			Ch->Write("Invalid Syntax:\n\r");
			Ch->Write("spatial <obj> location <theta> <rho> <distance>\n\r");
			return true;
		}

	}
	
	// Syntax arguments:
	// <name> <type> <create>
	// <name> <delete>
	// <name> <field> <value>
	if (gsSpatial == "" || gsType == "" || (gsField == "" && gsType != "delete"))
	{
		Ch->Write("Incorrect Syntax:\n\r");
		Ch->Write("Spatial <objname> {type} <field>|<create>|<delete> <value>|<filename>\n\r");
		return true;
	}

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CSpatial* pSpatial = NULL;
	bool bDone = false;


	if (gsField == "create")
	{

			gString gsFile;
			

			gsFile.Format("%s.spa", gsSpatial);

			for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
			for (spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
			{
				CSpatial* pSpatial = (*(*(*spa)));

				if (!pSpatial)
				{
					(*pos).second.erase(spa);
					continue;
				}

				if (pSpatial->m_gsName == gsSpatial)
				{
					Ch->Write("A Spatial Object with that name already exists!\n\r");
					return true;
				}

				if (pSpatial->m_gfFileName == gsFile)
				{
					Ch->Write("A Spatial Object with that filename already exists!\n\r");
					return true;
				}
			}

			if (gsType == "planet")
			{

				// Create Object
				CSpatial* pNewPlanet = new CPlanet(gsSpatial, gsFile);
				// Save
				pNewPlanet->Save();		
				Ch->Write("Planet created.\n\r");
				pGalaxy->AddSpatial(pNewPlanet);
				pGalaxy->AddSpatialToSpace(pNewPlanet->m_Vnum);
				return true;
			}
			else if (gsType == "star")
			{
				CSpatial* pNewStar = new CStar(gsSpatial, gsFile);
				pNewStar->Save();
				Ch->Write("Star created.\n\r");
				pGalaxy->AddSpatial(pNewStar);
				pGalaxy->AddSpatialToSpace(pNewStar->m_Vnum);
				return true;
			}
			else if( gsType == "asteroidfield")
			{
				if (gsValue == "")
				{
					Ch->Write("Invalid syntax:\n\r");
					Ch->Write("spatial <name> asteroidfield <size>");
					return true;
				}

				if (atoi(gsValue) > 100)
				{
					Ch->Write("Too large!\n\r");
					return true;
				}

				CSpatial* pAsteroidField = new CAsteroidField(gsSpatial, gsFile, atoi(gsValue));
				pAsteroidField->Save();
				Ch->Write("Asteroid Field Created.\n\r");
				pGalaxy->AddSpatial(pAsteroidField);
				pGalaxy->AddSpatialToSpace(pAsteroidField->m_Vnum);
				return true;
			}
			else if (gsType == "moon")
			{
				CSpatial* pNewMoon = new CMoon(gsSpatial, gsFile);
				pNewMoon->Save();
				Ch->Write("Moon created.\n\r");
				pGalaxy->AddSpatial(pNewMoon);
				pGalaxy->AddSpatialToSpace(pNewMoon->m_Vnum);
				return true;
			}
			else if (gsType == "sat")
			{
				CSpatial* pNewSat = new CSat(gsSpatial, gsFile);
				pNewSat->Save();
				Ch->Write("Satellite created.\n\r");
				pGalaxy->AddSpatial(pNewSat);
				pGalaxy->AddSpatialToSpace(pNewSat->m_Vnum);
				return true;
			}
			else if( gsType == "blackhole")
			{
				CSpatial* pBlack = new CBlackhole(gsSpatial, gsFile);
				pBlack->Save();
				Ch->Write("Blackhole Created.\n\r");
				pGalaxy->AddSpatial(pBlack);
				pGalaxy->AddSpatialToSpace(pBlack->m_Vnum);
				return true;
			}
			else if( gsType == "asteroid")
			{
				CSpatial* pAsteroid = new CAsteroid(gsSpatial, gsFile);
				pAsteroid->Save();
				Ch->Write("Asteroid Created.\n\r");
				pGalaxy->AddSpatial(pAsteroid);
				pGalaxy->AddSpatialToSpace(pAsteroid->m_Vnum);
				return true;
			}

			return false;



	}
	

	// Check Spatial Object is valid
	pSpatial = pGalaxy->GetSpa(gsSpatial);
	if (pSpatial == NULL)
	{
		Ch->Write("That Spatial Object does not exist!\n\r");
		return true;
	}
	else
	{
		if (gsType == "delete")
		{
			if (pSpatial->m_nType == 2)
			{
				// Asteroid Fields we need to remove all the sub fields
				CAsteroidField* pAster = (CAsteroidField*)pSpatial;
				pAster->Destroy();
				Ch->Write("Spatial Object Deleted.\n\r");
				return true;
			}

			pSpatial->m_bDelete = true;

			Ch->Write("Spatial Object Deleted.\n\r");
			return true;
		}
		// Setting the description of the object
		if (gsType == "description")
		{
			pSpatial->m_gsDescription = gsField;
			bDone = true;
		}
		// Setting the name of the object
		else if (gsType == "name")
		{
			pSpatial->m_gsName = gsField;
			bDone = true;
		}
		// Setting the mass of the object
		else if (gsType == "mass")
		{
			pSpatial->m_Signature[CSpatial::SI_MASS] = atoi(gsField);
			bDone = true;
		}
		// Setting the ion of the object
		else if (gsType == "ion")
		{
			pSpatial->m_Signature[CSpatial::SI_ION] = atoi(gsField);
			bDone = true;
		}
		// Setting the heat of the object
		else if (gsType == "heat")
		{
			pSpatial->m_Signature[CSpatial::SI_HEAT] = atoi(gsField);
			bDone = true;
		}		
		// Setting the mass of the object
		else if (gsType == "em")
		{
			pSpatial->m_Signature[CSpatial::SI_EM] = atoi(gsField);
			bDone = true;
		}

		// Setting the gravity of the object
		else if (gsType == "gravity")
		{
			pSpatial->m_nGravity = atoi(gsField);
			bDone = true;
		}
		// Location of the object
		else if (gsType == "location") 
		{
			CCart* pCart = new CCart(atoi(*LocationValues.begin()), atoi(*(LocationValues.begin()+1)), atoi(*(LocationValues.begin()+2)));
			// We have to update the Asteroids contained within the field as well
			if (pSpatial->m_nType == 2)
			{
				CAsteroidField* pAster = (CAsteroidField*)(pSpatial);
				for (ast = pAster->m_AsteroidList.begin(); ast != pAster->m_AsteroidList.end(); ast++)
				{
					CAsteroid* pAsteroid = (CAsteroid*)(*ast);
					pAsteroid->m_Location = pCart;
				}
			}

			pSpatial->m_Location = pCart;
			bDone = true;

		}
		// Setting the Sector of the object
		else if (gsType == "sector")
		{
			if (gsField == "None")
			{
				Ch->Write("Spatial object's Sector removed.\n\r");
				pSpatial->m_gsSector = "";
				return true;
			}

			CSector* pSector = pGalaxy->GetSec(gsField);
			if (pSector == NULL)
			{
				Ch->Write("No \"%s\" sector exists!\n\r", gsField);
				return true;
			}
			else
			{
				// We have to update the Asteroids contained within the field as well
				if (pSpatial->m_nType == 2)
				{
					CAsteroidField* pAster = (CAsteroidField*)(pSpatial);
					for (ast = pAster->m_AsteroidList.begin(); ast != pAster->m_AsteroidList.end(); ast++)
					{
						CAsteroid* pAsteroid = (CAsteroid*)(*ast);
						pAsteroid->m_gsSector = pSector->m_gsName;
					}
				}

				pGalaxy->AddSpatialToSpace(pSpatial->m_Vnum);
				pSpatial->m_gsSector = pSector->m_gsName;
				bDone = true;
			}

		}
		else if (gsType == "dying") // Star dying
		{
			if (pSpatial->m_nType != 1)
			{
				Ch->Write("That is not a Star!\n\r");
				return true;
			}

			CStar* pStar = (CStar*)pSpatial;
			if (gsField == "yes" || gsField == "1" || gsField == "true")
			{
				pStar->m_bDying = true;
			}
			else
			{
				pStar->m_bDying = false;
			}

			bDone = true;

		}
		else if (gsType == "energy") // Star Energy
		{
			if (pSpatial->m_nType != 1)
			{
				Ch->Write("That is not a Star!\n\r");
				return true;
			}

			CStar* pStar = (CStar*)pSpatial;
			pStar->m_nEnergy = atoi(gsField);


			bDone = true;

		}
		else if (gsType == "planet") // Satellite's planet
		{
			if (pSpatial->m_nType != 5 && pSpatial->m_nType != 6)
			{
				Ch->Write("That is not a Satellite!\n\r");
				return true;
			}

			CSatellite* pSat = (CSatellite*)pSpatial;
			CPlanet* pPlan;
			if ( (pPlan = (CPlanet*)pGalaxy->GetSpa(gsField)) == NULL)
			{
				Ch->Write("That Planet does not exist!\n\r");
				return true;
			}

			pSat->m_gsPlanet = pPlan->m_gsName;
			bDone = true;

		}
		else if (gsType == "orbit")
		{
			if (pSpatial->m_nType != 5 && pSpatial->m_nType != 6)
			{
				Ch->Write("That is not a Satellite!\n\r");
				return true;
			}

			CSatellite* pSat = (CSatellite*)pSpatial;
			pSat->m_nOrbit = atoi(gsField);
			bDone = true;
		}
		else if (gsType == "owner") // Sat's owner
		{
			if (pSpatial->m_nType != 6)
			{
				Ch->Write("That is not a Satellite!\n\r");
				return true;
			}

			CSat* pSat = (CSat*)pSpatial;
			pSat->m_gsOwner = gsField;
			bDone = true;
		}
		else if (gsType == "power") // Sat's power
		{
			if (pSpatial->m_nType != 6)
			{
				Ch->Write("That is not a Satellite!\n\r");
				return true;
			}

			CSat* pSat = (CSat*)pSpatial;
			pSat->m_nPower = atoi(gsField);
			bDone = true;

		}
		else if (gsType == "mineral" || gsType == "size") // Asteroid's size
		{
			if (pSpatial->m_nType != 9)
			{
				Ch->Write("That is not an Asteroid!\n\r");
				return true;
			}

			CAsteroid* pAsteroid = (CAsteroid*)pSpatial;

			if (gsType == "mineral")
			{
				// Check its a valid mineral
				int nMineral = atoi(gsField);

				if (nMineral > 10) // Temp max minerals
				{
					Ch->Write("That is not a valid mineral!\n\r");
					Ch->Write("Choices are:\n\r");
					Ch->Write("1) Titanium\n\r");
					return true;
				}

				pAsteroid->m_nMineral = atoi(gsField);
			}

			if (gsType == "size")
				pAsteroid->m_nSize = atoi(gsField);
			bDone = true;

		}
		else if (gsType == "speed") // Asteroid Field's speed
		{
			if (pSpatial->m_nType != 2)
			{
				Ch->Write("That is not an Asteroid Field!\n\r");
				return true;
			}

			CAsteroidField* pField = (CAsteroidField*)pSpatial;
			pField->m_nSpeed = atoi(gsField);
			bDone = true;
		}

	}

	if (bDone)
	{
		Ch->Write("Changes made.\n\r");
		return true;
	}
			
	return true;

}

// Method     :: CmdPromote
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <Player to raise>
// Return     :: Bool
// Function   :: Used to raise a player up a staff level
// Written    :: 09/06/05 {OWV}

bool CmdPromote::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	ActorMap::iterator pos;
	CPlayer* pP = NULL;

	if (gsValue == "")
	{
		Ch->Write("[Invalid Input] You must enter the name of the Player you wish to promote.\n\r");
		return true;
	}

	bool bFound = false;

	for (pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
	{
		if ( (pP = (CPlayer*)((*pos).second)) != NULL )
		{
			if (pP->Name() == gsValue)
			{
				bFound = true;
				break;
			}
		}
	}

	if (!bFound)
	{
		Ch->Write("[Invalid Player] No such Player exists.\n\r");
		return true;
	}
	else
	{
		// We know pP isn't null now

		if (pP->ActorFlags()->IsSet(CActor::_ADMINISTRATOR))
		{
			Ch->Write("That player is already an Administrator, you cannot promote them further.\n\r");
			return true;
		}

		// Promotion to Administrator
		if (pP->ActorFlags()->IsSet(CActor::_ASSISTANT))
		{
			Ch->Write("%s promoted to Administrator.\n\r", pP->Name());
			pP->Write("%s has promoted you to an Administrator.\n\r", Ch->Name());
			pP->ActorFlags()->RemoveBit(CActor::_ASSISTANT);
			pP->ActorFlags()->SetBit(CActor::_ADMINISTRATOR);
			pP->Save();
			return true;
		}

		// Promotion to Assistant
		if (pP->ActorFlags()->IsSet(CActor::_STAFF))
		{
			Ch->Write("%s promoted to Assistant Administrator.\n\r", pP->Name());
			pP->Write("%s has promoted you to an Assistant Administrator.\n\r", Ch->Name());
			pP->ActorFlags()->RemoveBit(CActor::_STAFF);
			pP->ActorFlags()->SetBit(CActor::_ASSISTANT);
			pP->Save();
			return true;
		}		

		// Promotion to Assistant
		if (pP->ActorFlags()->IsSet(CActor::_PLAYER))
		{
			Ch->Write("%s promoted to Staff member.\n\r", pP->Name());
			pP->Write("%s has promoted you to a member of Staff.\n\r", Ch->Name());
			pP->ActorFlags()->SetBit(CActor::_STAFF);
			pP->Save();
			return true;
		}		

	}


	return true;
}

// Method     :: CmdDemote
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <Player>
// Return     :: Bool
// Function   :: Used to lower a player's staff level
// Written    :: 09/06/05 {OWV}

bool CmdDemote::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	ActorMap::iterator pos;
	CPlayer* pP = NULL;

	if (gsValue == "")
	{
		Ch->Write("[Invalid Input] You must enter the name of the Player you wish to demote.\n\r");
		return true;
	}

	bool bFound = false;

	for (pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
	{
		if ( (pP = (CPlayer*)((*pos).second)) != NULL )
		{
			if (pP->Name() == gsValue)
			{
				bFound = true;
				break;
			}
		}
	}

	if (!bFound)
	{
		Ch->Write("[Invalid Player] No such Player exists.\n\r");
		return true;
	}
	else
	{
		// We know pP isn't null now

		// Demotion to Assistant
		if (pP->ActorFlags()->IsSet(CActor::_ADMINISTRATOR))
		{
			Ch->Write("%s demoted.\n\r", pP->Name());
			pP->Write("%s has demoted you to Assistant Administrator.\n\r", Ch->Name());
			pP->ActorFlags()->RemoveBit(CActor::_ADMINISTRATOR);
			pP->ActorFlags()->SetBit(CActor::_ASSISTANT);
			pP->Save();
			return true;
		}

		// Demotion to Staff
		if (pP->ActorFlags()->IsSet(CActor::_ASSISTANT))
		{
			Ch->Write("%s demoted.\n\r", pP->Name());
			pP->Write("%s has demoted you to the Staff.\n\r", Ch->Name());
			pP->ActorFlags()->RemoveBit(CActor::_ASSISTANT);
			pP->ActorFlags()->SetBit(CActor::_STAFF);
			pP->Save();
			return true;
			return true;
		}		

		// Demotion to Player, removed from Staff
		if (pP->ActorFlags()->IsSet(CActor::_STAFF))
		{
			Ch->Write("%s demoted.\n\r", pP->Name());
			pP->Write("%s has removed you from the Staff.\n\r", Ch->Name());
			pP->ActorFlags()->RemoveBit(CActor::_STAFF);
			pP->Save();
			return true;
			return true;
		}		

		Ch->Write("That player is not a member of Staff\n\r");
		return true;

	}


	return true;
}


// Method     :: CmdTest
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <Lots>
// Return     :: Bool
// Function   :: Used to output some test data to a player
// Written    :: 09/06/05 {OWV}

bool CmdTest::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Validate arguments
	
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();	
	gString gsValue3 = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();	
	gString gsValue4 = (CommandLine.empty()) ? "" : *CommandLine.begin();
		if (!CommandLine.empty())
		CommandLine.pop_front();	
	gString gsValue5 = (CommandLine.empty()) ? "" : *CommandLine.begin();
		if (!CommandLine.empty())
		CommandLine.pop_front();	
	gString gsValue6 = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();	
	gString gsValue7 = (CommandLine.empty()) ? "" : *CommandLine.begin();
		if (!CommandLine.empty())
		CommandLine.pop_front();	
	gString gsValue8 = (CommandLine.empty()) ? "" : *CommandLine.begin();
	
	if (gsValue == "perception")
	{
		if (gsValue2.IsEmpty())
		{
			Ch->Write("You must insert a value to set your perception to.\n\r");
			return true;
		}
		else
		{
			Ch->Attributes()["wisdom"].SetCur(atoi(gsValue2));
			Ch->Write("Perception set to %d\n\r", atoi(gsValue2));
			return true;
		}
	}

	if (gsValue == "Wordwrap")
	{
		gString gsLongWord = "This is a really long word, im only typing this to see whether my new word wrapper actually works. Im going to make this long so that we can wrap more than one line, im aiming for something like 3 to 4 lines of wrap here, well, lets see how well i did.";

		gString gsNew;
		gsNew = gsLongWord.WordWrap(30);
		Ch->Write("%s\n\r", gsNew);
		return true;
	}

	if (gsValue == "newradar")
	{
		// Create a list of contacts
		std::map<gString, CCart*>NewContact;
		std::map<gString, CCart*>::iterator it;

		// Our psuedo 'range'
		const int nRange = 7; // So we can see 7 grids in each direction (Thats 7000 metres)
	 	const int nArraySize = nRange * 2 + 1;

		// Create a 3D array for contacts
		int Galaxy[nArraySize][nArraySize][nArraySize];

		// Initialize values
		for (int i = 0; i < nArraySize; i++)
			for (int j = 0; j < nArraySize; j++)
				for (int k = 0; k < nArraySize; k++)
					Galaxy[i][j][k] = 0;

		int nContactNum = 1;

		
		// Indexed Contacts map
		gStringMapList gmlContacts;

		NewContact.insert(std::map<gString, CCart*>::value_type("ISD Intrepid", new CCart(6000, 4000, 0)));
		NewContact.insert(std::map<gString, CCart*>::value_type("Rogue Squadron", new CCart(6600, 4200, 0)));
		NewContact.insert(std::map<gString, CCart*>::value_type("MC90 Home One", new CCart(-4000, 4000, 0)));
		NewContact.insert(std::map<gString, CCart*>::value_type("Rendili StarDreadnaught Katana 1", new CCart(2000, 2000, 0)));
		NewContact.insert(std::map<gString, CCart*>::value_type("ISD Chimaera", new CCart(-4000, 3000, 0)));
		NewContact.insert(std::map<gString, CCart*>::value_type("YT-1300 Millienium Falcon", new CCart(9000, -3000, 0)));

		// Define our position on the grid
		CCart *pUs = new CCart(2000, 0, 0);

		// We now need to translate all our contacts to make them relative to our position
		// We also divide their position by 1000 to work out their grid position then
		// insert them into this grid
		for (it = NewContact.begin(); it != NewContact.end(); it++)
		{
			int x = (*it).second->x - pUs->x; x /= 1000;
			int y = (*it).second->y - pUs->y; y /= 1000;
			int z = (*it).second->z - pUs->z; z /= 1000;

			// We have to now fix our positions to handle negative numbers
			x += nRange;
			y += nRange;
			z += nRange;

			// Need to adjust negative numbers
			// Our ship is in the centre of the range

			// Now we insert the contact into our grid using a unique number
			if (Galaxy[x][y][z] == 0)
			{
				// There is no key in this square, hence we add one
				Galaxy[x][y][0] = nContactNum;
				
				// Insert into index contact list
				gStringList gsList;
				gsList.push_back((*it).first);
				gmlContacts.insert(gStringMapList::value_type(nContactNum, gsList));

				nContactNum++;
			}
			else
			{
				// There is a key, we dont change the array but add our contact to the indexed contacts list

				gStringMapList::iterator gml = gmlContacts.find(Galaxy[x][y][z]);

				// We found one!
				if (gml != gmlContacts.end())
				{
					((*gml).second).push_back((*it).first);
				}
			}
			// Add we add the contact to our 'key'
		}

		// We are now the centre of the grid and all our contacts have been adjusted
		pUs->x = 0; pUs->y = 0; pUs->z = 0;

		


		// Draw the grid now
		for (int j = 0; j < 15; j++)
		{
			Ch->Write(" ");
			// Drawing the top border of the grid
			for (int i = 0; i < 60; i++)
				Ch->Write("#600-"); 

			Ch->Write("#700\n\r");


			Ch->Write("#600|");
			for (int i = 0; i < 15; i++)
			{
				if (j == 7 && i == 7)
					Ch->Write(" #201^ #600|");
				else 
				{
					if (Galaxy[i][j][0] != 0)
						Ch->Write(" #601%d #600|", Galaxy[i][j][0]);
					else
						Ch->Write("   #600|");
				}
			}

			Ch->Write("\n\r");
		}

		// Drawing the bottom border of the grid
		Ch->Write(" ");

		for (int i = 0; i < 60; i++)
			Ch->Write("#600-"); 

		Ch->Write("#700\n\r");

		// Draw our contact list
		for (gStringMapList::iterator gml = gmlContacts.begin(); gml != gmlContacts.end(); gml ++)
		{
			Ch->Write("[%2d] ", (*gml).first);

			for (gStringList::iterator gsl = (*gml).second.begin(); gsl != (*gml).second.end(); gsl++)
			{
				if (gsl == (*gml).second.begin())
					Ch->Write("%s\n\r", (*gsl));
				else
					Ch->Write("     %s\n\r", (*gsl));
			}
		}
		

		return true;

	}
	// Testing new skill storage
	if (gsValue == "Skills")
	{
		CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

		if (Ch->ActorFlags()->IsSet(CActor::_PLAYER))
		{
			Ch->Write(">> Current\n\r");
			pEpacs->DisplaySkills((CMobile*)Ch, true);
			Ch->Write(">> All\n\r");
			pEpacs->DisplaySkills((CMobile*)Ch, false);
		}

		return true;

	}
	// TESTING WEAPON RELOADING
	if (gsValue == "Weapon")
	{
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
		CShip* pShip = pGalaxy->GetShi("INS Chimaera");

		if (!pShip)
			return false;

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
			{
				if ((*comp)->m_Type == CComponent::CT_WEAPONMOUNT)
				{
					CModule* pMod = (*comp)->Get(CModule::MT_MUNITION_ELEVATOR);

					if (pMod)
					{
						pShip->Write(CShip::MT_FCS, "Missile launched.\n\r");
						pMod->SetPlus("Loading", true);
					}
				}
			}
		}

		pGalaxy = NULL;
		pShip = NULL;

		return true;
	}

	if (gsValue == "colour")
	{
		Ch->Write("\x01b[s\n\n");
		Ch->Write("This is a test!\n\n");
		Ch->Write("\x01b[u");
		Ch->Write("Overwrite");
	}

	if (gsValue == "leak1")
	{
		Ch->Write("Leaking\n\r");

		for (int i = 0; i < atoi(gsValue2); i++)
			gString	gsTest = "Test";

		return true;
	}

	if (gsValue == "hash")
	{
		gString gsInt = "intelligence";
		gString gsWis = "wisdom";

		Ch->Write("Intelligence: %d\n\r", gsInt.GetHash());
		Ch->Write("Wisdom: %d\n\r", gsWis.GetHash());
		return true;
	}

	if (gsValue == "leak2")
	{
		Ch->Write("Leaking\n\r");

		for (int i = 0; i < atoi(gsValue2); i++)
			std::string gsTest = "Test";

		return true;
	}

	// e-pac(s) testing
	if (gsValue == "epacs")
	{
		CGameObjects::Get().GameWorld()->Write("Rolling...\n\r> Skill     [%d]\n\r> Attribute [%d]\n\r> Modifiers [%d]\n\r> Threshold [%d]\n\r\n\r",
			atoi(gsValue2), atoi(gsValue3), atoi(gsValue4), atoi(gsValue5));
		
		int nTotal = 0;
		int nPasses = 0;
		int nMax = 1;
		int nResult = 0;
		
		for (int i = 0; i < nMax; i++)
		{
			nResult = CGameObjects::Get().GameWorld()->Epacs()->Roll(atof(gsValue2), atoi(gsValue3), atoi(gsValue4), atoi(gsValue5));
			nTotal += nResult;
			
			if (nResult > 0)
				nPasses++;
		}	
		
		CGameObjects::Get().GameWorld()->Write("\n\rAverage Result for [%d] runs: %0.2f\n\r", nMax, (float)nTotal/nMax);
		CGameObjects::Get().GameWorld()->Write("Successful %0.2f of the time.\n\r", (float)nPasses/nMax*100.0);
		CGameObjects::Get().GameWorld()->Epacs()->OutputTest();
		CGameObjects::Get().GameWorld()->Epacs()->ResetTest();
		return true;
	}

	if (gsValue =="epacsO")
	{
		CGameObjects::Get().GameWorld()->Write("Rolling...\n\r>Player 1:\n\r> Skill     [%d]\n\r> Attribute [%d]\n\r> Modifiers [%d]\n\r\n\r",
			atoi(gsValue2), atoi(gsValue3), atoi(gsValue4));
		CGameObjects::Get().GameWorld()->Write("Player 2:\n\r> Skill     [%d]\n\r> Attribute [%d]\n\r> Modifiers [%d]\n\r\n\r",
			atoi(gsValue5), atoi(gsValue6), atoi(gsValue7));
		CGameObjects::Get().GameWorld()->Write("Threshold for roll is [%d]\n\r", atoi(gsValue8));
		
		int nTotal = 0;
		int nP1 = 0;
		int nP2 = 0;
		int nMax = 1;
		int nResult = 0;

		for (int i = 0; i < nMax; i++)
		{
			nResult = CGameObjects::Get().GameWorld()->Epacs()->RollO(atof(gsValue2), atoi(gsValue3), atoi(gsValue4), atof(gsValue5), atoi(gsValue6), atoi(gsValue7), atoi(gsValue8));
			nTotal += nResult;

			if (nResult > 0)
			{
				//Ch->Write("Player 1!\n\r");
				nP1++;
			}
			else
			{
				//Ch->Write("Player 2!\n\r");
				nP2++;
			}
		}	

		nTotal /= nMax;
		
		CGameObjects::Get().GameWorld()->Write("\n\rAverage Result for [%d] runs: %s\n\r", nMax, nP1 >= nP2 ? "Player 1" : "Player 2");
		CGameObjects::Get().GameWorld()->Write("Player 1 won [%0.2f] of the time\n\r", (float)nP1/nMax*100.0);
		CGameObjects::Get().GameWorld()->Write("Player 2 won [%0.2f] of the time\n\r", (float)nP2/nMax*100.0);
	}

	// Lower, upper, Number
	if (gsValue == "rand")
	{
		CRandom* pRand = CGameObjects::Get().Rand();
		int nLower = atoi(gsValue2);
		int nUpper = atoi(gsValue3);
		int nRange = nUpper - nLower + 1;
		IntegerList ilRange;
		int nNumber= atoi(gsValue4);

		if (nNumber == 0)
			nNumber = 1;

		for (int j = 0; j < nRange; j++)
			ilRange.push_back(0);

		int nMax = nNumber;

		for (int i = 0; i < nMax; i++)
		{
			int nNum = pRand->NumberRange(nLower, nUpper);

			ilRange[(nNum-nLower)]++;
		}

		int nCount = nLower;
		for (IntegerList::iterator it = ilRange.begin(); it != ilRange.end(); it++)
		{
			Ch->Write("Number of [%d]: %d (%0.2f)\n\r", nCount, (*it), (float)ilRange[(*it)]/nMax*100.0);
			nCount++;
		}
		
	}

	// TESTING BULK DOORS
	if (gsValue == "Bulkdoors")
	{
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
		CShip* pShip = pGalaxy->GetShi("INS Chimaera");
		
		if (!pShip)
			return false;

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
			{
				if ((*comp)->m_Type == CComponent::CT_LANDING)
				{
					CModule* pMod = (*comp)->Get(CModule::MT_BLAST_DOORS);

					if (pMod)
					{
						pMod->SetPlus("Opening", 1);
						gString gsMsg;

						if (pMod->Plus("Open", false))
							pShip->Write(CShip::MT_SYSTEMS, "[%s] Closing %s.\n\r", (*comp)->m_gsName, pMod->m_gsName);
						else
							pShip->Write(CShip::MT_SYSTEMS, "[%s] Opening %s.\n\r", (*comp)->m_gsName, pMod->m_gsName);

					}
				}
			}
		}

		pGalaxy = NULL;
		pShip = NULL;

		return true;
	}

	// TESTING BEARING COMMAND
	if (gsValue == "Bearing")
	{
		CCart* pCart = new CCart(0, 0, 0);
		CCart* pHead = new CCart(0, 0, 180);
		CCart* pCart2= new CCart(5, 5, 5);
		CCart* pHead2 = new CCart();
		CCart* pHead3 = new CCart();
		pHead2->SetXYZ(pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ));
		pHead3->x = pCart->GetX(pCart2->z, pCart2->y, 10000);
		pHead3->y = pCart->GetY(pCart2->y, 10000);
		pHead3->z = pCart->GetZ(pCart2->z, pCart2->y, 10000);
		Ch->Write("\n\rTarget @ %d %d %d\n\rBearing: %3.0f Mark: %3.0f Distance: %0.0f\n\rConversion New: %d %d %d  Conversion Old: %d %d %d\n\r", pCart2->x, pCart2->y, pCart2->z, pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ), pCart->Distance(pCart2), pHead2->x, pHead2->y, pHead2->z, pHead3->x, pHead3->y, pHead3->z); 

		pCart2 = new CCart(5, 5, -5);
		pHead3->x = pCart->GetX(pCart2->z, pCart2->y, 10000);
		pHead3->y = pCart->GetY(pCart2->y, 10000);
		pHead3->z = pCart->GetZ(pCart2->z, pCart2->y, 10000);
		pHead2->SetXYZ(pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ));
		Ch->Write("\n\rTarget @ %d %d %d\n\rBearing: %3.0f Mark: %3.0f Distance: %0.0f\n\rConversion: %d %d %d  Conversion Old: %d %d %d\n\r", pCart2->x, pCart2->y, pCart2->z, pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ), pCart->Distance(pCart2), pHead2->x, pHead2->y, pHead2->z, pHead3->x, pHead3->y, pHead3->z); 

		pCart2 = new CCart(-5, 5, -5);
		pHead3->x = pCart->GetX(pCart2->z, pCart2->y, 10000);
		pHead3->y = pCart->GetY(pCart2->y, 10000);
		pHead3->z = pCart->GetZ(pCart2->z, pCart2->y, 10000);
		pHead2->SetXYZ(pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ));
		Ch->Write("\n\rTarget @ %d %d %d\n\rBearing: %3.0f Mark: %3.0f Distance: %0.0f\n\rConversion: %d %d %d  Conversion Old: %d %d %d\n\r", pCart2->x, pCart2->y, pCart2->z, pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ), pCart->Distance(pCart2), pHead2->x, pHead2->y, pHead2->z, pHead3->x, pHead3->y, pHead3->z); 

		pCart2 = new CCart(-5, 5, 5);
		pHead3->x = pCart->GetX(pCart2->z, pCart2->y, 10000);
		pHead3->y = pCart->GetY(pCart2->y, 10000);
		pHead3->z = pCart->GetZ(pCart2->z, pCart2->y, 10000);
		pHead2->SetXYZ(pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ));
		Ch->Write("\n\rTarget @ %d %d %d\n\rBearing: %3.0f Mark: %3.0f Distance: %0.0f\n\rConversion: %d %d %d  Conversion Old: %d %d %d\n\r", pCart2->x, pCart2->y, pCart2->z, pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ), pCart->Distance(pCart2), pHead2->x, pHead2->y, pHead2->z, pHead3->x, pHead3->y, pHead3->z); 

		pCart2 = new CCart(-5, 5, 5);
		pHead3->x = pCart->GetX(pCart2->z, pCart2->y, 10000);
		pHead3->y = pCart->GetY(pCart2->y, 10000);
		pHead3->z = pCart->GetZ(pCart2->z, pCart2->y, 10000);
		pHead2->SetXYZ(pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ));
		Ch->Write("\n\rTarget @ %d %d %d\n\rBearing: %3.0f Mark: %3.0f Distance: %0.0f\n\rConversion: %d %d %d  Conversion Old: %d %d %d\n\r", pCart2->x, pCart2->y, pCart2->z, pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ), pCart->Distance(pCart2), pHead2->x, pHead2->y, pHead2->z, pHead3->x, pHead3->y, pHead3->z); 

		pCart2 = new CCart(0, 1, 0);
		pHead3->x = pCart->GetX(pCart2->z, pCart2->y, 10000);
		pHead3->y = pCart->GetY(pCart2->y, 10000);
		pHead3->z = pCart->GetZ(pCart2->z, pCart2->y, 10000);
		pHead2->SetXYZ(pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ));
		Ch->Write("\n\rTarget @ %d %d %d\n\rBearing: %3.0f Mark: %3.0f Distance: %0.0f\n\rConversion: %d %d %d  Conversion Old: %d %d %d\n\r", pCart2->x, pCart2->y, pCart2->z, pCart->Bearing(pCart2, pHead, CCart::_XZ), pCart->Bearing(pCart2, pHead, CCart::_YZ), pCart->Distance(pCart2), pHead2->x, pHead2->y, pHead2->z, pHead3->x, pHead3->y, pHead3->z); 

	}


	// TESTING DAMAGE SYSTEM
	if (gsValue == "Report")
	{
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
		CShip* pShip = Ch->CurrentRoom()->GetShip();

		if (!pShip)
			return false;

		int nFrame = 1;
		int nHull = 1;
		int nComp = 1;
		int nMod = 1;

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			Ch->Write("[%d] %s\n\r", nFrame, (*frame)->m_gsName);
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			{
				Ch->Write(" [%d] %s\n\r", nHull, (*hull)->m_gsName);
				if ((*hull)->m_Armour)
					Ch->Write(" Armour: %s Diss: %d [%d/%d]\n\r", (*hull)->m_Armour->m_gsName, (*hull)->m_Armour->Plus("Dissipation", false), (*hull)->m_Armour->m_ncDurability, (*hull)->m_Armour->m_nmDurability);
				Ch->Write(" Keel: %d/%d\n\r", (*hull)->m_nCKeel, (*hull)->m_nMKeel);
				for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
				{
					Ch->Write("  [%d] %s\n\r", nComp, (*comp)->m_gsName);
					for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
					{	
						Ch->Write("   [%d] %30s\t [%4d|%4d] {%d|%d} %s\n\r", nMod, ((*mod).second)->m_gsName, ((*mod).second)->m_ncDurability, ((*mod).second)->m_nmDurability, (*mod).second->Plus("PowerLoad", false), (*mod).second->Plus("PowerUsage", false), ((*mod).second)->Powered() ? "{ON}" : "{OFF}");
						nMod++;
					}
					nComp ++;
					
				}
				nHull++;
			}
			nFrame++;
			
		}

		pGalaxy = NULL;
		pShip = NULL;

		return true;
	}

	if (gsValue == "crash")
	{
		if (Ch->Name() == "Owen")
		{
			CShip* pShip;

			pShip->m_gsName = "CRASH MUAHAHAHA";
		}
		else
		{
			Ch->Write("Only Owen is allowed to crash us!\n\r");
			return true;
		}
		return true;
	}

	if (gsValue == "states")
	{
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

		for (ShipList::iterator shi = pGalaxy->m_ShipList.begin(); shi != pGalaxy->m_ShipList.end(); shi++)
		{
			CShip* pShip = (CShip*)***shi;	

			Ch->Write("%s >> ", pShip->m_gsName);
			for (int i = 0; i < CShip::_NUMSTATES; i++)
				if (pShip->m_ShipState->IsSet(i))
					Ch->Write("%s ", CShip::szStates[i]);

			Ch->Write("\n\r");
				
		}
	}
		

	if (gsValue == "ShowSpa")
	{
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

		Ch->Write("Global Map >>\n\r");
		for (gSpatialMap::iterator it = pGalaxy->m_gSpatialMap.begin(); it != pGalaxy->m_gSpatialMap.end(); it++)
		{
			Ch->Write("[%d] %s\n\r", (*it).first, (*it).second->m_gsName);
		}

		Ch->Write("Space Map >>\n\r");
		for (SpatialMap::iterator spa = pGalaxy->m_SpatialMap.begin(); spa != pGalaxy->m_SpatialMap.end(); spa++)
		for (SpatialList::iterator sl = (*spa).second.begin(); sl != (*spa).second.end(); sl++)
		{
			CSpatial* pSpatial = ***sl;

			if (!pSpatial)
				continue;

			Ch->Write("[%d] %s\n\r", (*sl)->m_nVnum, pSpatial->m_gsName);
		}

		Ch->Write("Ships >>\n\r");
		for (ShipList::iterator shi = pGalaxy->m_ShipList.begin(); shi != pGalaxy->m_ShipList.end(); shi++)
		{
			Ch->Write("[%d] %s\n\r", (*shi)->m_nVnum, (*(*(*shi)))->m_gsName);
		}
		
		return true;
	}

	// TESTING DAMAGE
	if (gsValue == "Damage")
	{

		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
		CShip* pShip = Ch->CurrentRoom()->GetShip();

		if (!pShip)
			return false;

		if (atoi(gsValue2) < 0)
		{
			Ch->Write("No negative numbers dickwad.\n\r");
			return false;
		}

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			{
				(*hull)->Damage(atoi(gsValue2), false, pShip);
				Ch->Write("[%s] Dealt %d damage.\n\r", (*hull)->m_gsName, atoi(gsValue2));
			}
			
		}		

	}

	// RESTORING DAMAGE
	if (gsValue == "restore")
	{
		Ch->ExecuteCommand("ooc", "I am a dirty cheater");
		//return true;

		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
		CShip* pShip = Ch->CurrentRoom()->GetShip();

		if (!pShip)
			return false;

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			{
				if ((*hull)->m_Armour)
					(*hull)->m_Armour->m_ncDurability = (*hull)->m_Armour->m_nmDurability;

				(*hull)->m_nCKeel = (*hull)->m_nMKeel;

				for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
				{
					for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
					{	
						((*mod).second)->m_ncDurability = ((*mod).second)->m_nmDurability;
					}
										
				}
			}
						
		}

		pGalaxy = NULL;
		pShip = NULL;

		Ch->Write("All Integrities reset to max.\n\r");

		return true;
	}

	// Testing rotation of Cartesians

	if (gsValue == "rotation1")
	{
		CCart* pCart  = new CCart(3000, 2000, 1000);
		CCart* pCart2 = new CCart(3000, 2500, 1000);
		CCart* pCart3 = new CCart(3500, 2000, 1000);
		CCart* pCart4 = new CCart(3500, 2500, 1000);
		CCart* pLocal = new CCart(3250, 2250, 1250);
		static double fPi = 3.14159;
		pCart->Rotate(pLocal, 0, 0, fPi/2);
		pCart2->Rotate(pLocal, 0, 0, fPi/2);
		pCart3->Rotate(pLocal, 0, 0, fPi/2);
		pCart4->Rotate(pLocal, 0, 0, fPi/2);
		Ch->Write("[3000, 2000, 1000] Rotated by 90 gives us [%d, %d, %d]\n\r", pCart->x, pCart->y, pCart->z);
		Ch->Write("[3000, 2500, 1000] Rotated by 90 gives us [%d, %d, %d]\n\r", pCart2->x, pCart2->y, pCart2->z);
		Ch->Write("[3500, 2000, 1000] Rotated by 90 gives us [%d, %d, %d]\n\r", pCart3->x, pCart3->y, pCart3->z);
		Ch->Write("[3500, 2500, 1000] Rotated by 90 gives us [%d, %d, %d]\n\r", pCart4->x, pCart4->y, pCart4->z);
	}

	if (gsValue == "rotation2")
	{
		CCart* pCart  = new CCart(0, 5, 0);
		CCart* pLocal = new CCart(0,0,0);
		static double fPi = 3.14159;
		pCart->Rotate(pLocal, fPi/2, 0, 0);
		Ch->Write("[0, 5, 0] Rotated by 90 on X gives us [%2d, %2d, %2d]\n\r", pCart->x, pCart->y, pCart->z);
		pCart  = new CCart(0, 5, 0);
		pCart->Rotate(pLocal, 0, fPi/2, 0);
		Ch->Write("[0, 5, 0] Rotated by 90 on Y gives us [%2d, %2d, %2d]\n\r", pCart->x, pCart->y, pCart->z);
		pCart  = new CCart(0, 5, 0);
		pCart->Rotate(pLocal, 0, 0, fPi/2);
		Ch->Write("[0, 5, 0] Rotated by 90 on Z gives us [%2d, %2d, %2d]\n\r", pCart->x, pCart->y, pCart->z);
	}

	if (gsValue == "bearing1")
	{
		CCart* pFrom = new CCart(0, 0, 0);
		CCart* pTo = new CCart(0, 5, 0);
		CCart* pHead = new CCart(0, 180, 180);

		float fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		Ch->Write("Bearing  == %0.2f\n\r", fBear);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		Ch->Write("Mark == %0.2f\n\r", fBear);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Roll == %0.2f\n\r", fBear);
		return true;
	}

	if (gsValue == "gString")
	{
		gString gsTemp = "Left and $N Right";

		Ch->Write("%s\n\r", gsTemp.Left(1));
		Ch->Write("%s\n\r", gsTemp.Right(6));
		Ch->Write("%d\n\r", gsTemp.Find("$N"));
	}

	if (gsValue == "bearing2")
	{
		CCart* pFrom = new CCart(0, 0, 0);
		CCart* pTo = new CCart(0, 0, 1);
		CCart* pHead = new CCart(0, 335, 280);

		Ch->Write("#701Test Data for#600:#700 -25 #600m#700 -80\n\r");
		float fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		float fMark = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		float fRoll = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Enemy vessel A[F] in #100%s#700 arc %0.2f m %0.2f r %0.2f\n\r", CShip::szArc[pFrom->Arc(pTo, pHead)], fBear, fMark, fRoll);
		pTo = new CCart(1, 0, 0);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		fMark = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		fRoll = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Enemy vessel B[S] in #100%s#700 arc %0.2f m %0.2f r %0.2f\n\r", CShip::szArc[pFrom->Arc(pTo, pHead)], fBear, fMark, fRoll);
		pTo = new CCart(0, 0, -1);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		fMark = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		fRoll = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Enemy vessel C[A] in #100%s#700 arc %0.2f m %0.2f r %0.2f\n\r", CShip::szArc[pFrom->Arc(pTo, pHead)], fBear, fMark, fRoll);
		pTo = new CCart(-1, 0, 0);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		fMark = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		fRoll = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Enemy vessel D[P] in #100%s#700 arc %0.2f m %0.2f r %0.2f\n\r", CShip::szArc[pFrom->Arc(pTo, pHead)], fBear, fMark, fRoll);
		pTo = new CCart(0, 1, 0);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		fMark = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		fRoll = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Enemy vessel E[D] in #100%s#700 arc %0.2f m %0.2f r %0.2f\n\r", CShip::szArc[pFrom->Arc(pTo, pHead)], fBear, fMark, fRoll);
		pTo = new CCart(0, -1, 0);
		fBear = pFrom->Bearing(pTo, pHead, CCart::_XZ);
		fMark = pFrom->Bearing(pTo, pHead, CCart::_YZ);
		fRoll = pFrom->Bearing(pTo, pHead, CCart::_XY);
		Ch->Write("Enemy vessel F[V] in #100%s#700 arc %0.2f m %0.2f r %0.2f\n\r", CShip::szArc[pFrom->Arc(pTo, pHead)], fBear, fMark, fRoll);
		
		return true;
	}

	if (gsValue == "bearing3")
	{
		CCart* pFrom = new CCart(65, 145, 65);
		CCart* pHead = new CCart(0, 0, 0);
		CCart* pTo = new CCart(30, 50, 1000);
		Ch->Write("Enemy vessel A in %s arc\n\r", pFrom->Arc(pTo, pHead));
		pHead = new CCart(180, 0 , 0);
		pFrom = new CCart(55, 17, 9);
		pTo = new CCart(63, 27, 9);


	}

	if (gsValue == "bearing4")
	{
		CCart* pFrom = new CCart(0, 0, 0);
		CCart* pHead = new CCart(90, 0, 180);
		// X = Pitching Y = Yawing Z = Rolling
		// not: X = Roll, Y = Pitch, Z = Yaw
		CCart* pTo = new CCart(8, 10, 2); // Behind us

		Ch->Write("Enemy vessel A in %s arc\n\r", pFrom->Arc(pTo, pHead));

	}
	
	if (gsValue == "theta")
	{
		CCart* pFrom = new CCart(0, 0, 0);
		CCart* pHead = new CCart(225, 0, 0);
		CCart* pTo = new CCart(2, 2, 0);
		Ch->Write("%0.0f\n\r", pFrom->Arc(pTo, pHead));
	}

	if (gsValue == "conversion")
	{
		CCart* pNew = new CCart(500, 300, 300);
		pNew = new CCart(35000, 1200, 30000);
		Ch->Write("%d, %d, %d Is equal to %0.05f %0.5f @ %0.0f\n\r", pNew->x, pNew->y, pNew->z, pNew->Bear(), pNew->Mark(), pNew->Dist());
		Ch->Write("Re-conversion: %0.0f %0.0f %0.0f\n\r", pNew->GetX(pNew->Bear(), pNew->Mark(), pNew->Dist()), pNew->GetY(pNew->Mark(), pNew->Dist()), pNew->GetZ(pNew->Bear(), pNew->Mark(), pNew->Dist()));
	}

	if (gsValue == "plus")
	{
		
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
		CShip* pShip = pGalaxy->GetShi("PCS Katana");

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
			if (((*map).second)->Plus("Power", false) == 1)
				Ch->Write("%s", ((*map).second)->m_gsName);

	}

	return true;

}

// Method     :: CmdConfig
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <none>
//			  :: <field> <value>
// Return     :: Bool
// Function   :: Allows an administrator to set the Mud Config settings
// Written    :: 18/12/05 {OWV}

bool CmdConfig::Perform(CActor* Ch, gStringList& CommandLine)
{

	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();
	
	
	CDataFile pData = CGameObjects::Get().ConfigData();


	gStringArray pNames;
	pNames.push_back("Options");
	pNames.push_back("Menus");
	pNames.push_back("PathFinding");
	pNames.push_back("Update Deltas");

	if (gsValue == "" || gsValue == "show" || gsValue == "display")
	{

		Ch->Write(":: Game Config ::\n\r\n\r");

		for (int i = 0; i < pNames.size(); i++)
		{
			if (i % 2 == 0)
				Ch->Write("#601-----------------[#700%-15s#601]-----------------#600\n\r", pNames.at(i));
			else
				Ch->Write("#701-----------------[#700%-15s#701]-----------------#700\n\r", pNames.at(i));
			gStringArray pArray = pData.GetSectionList(pNames.at(i));
			gStringArray::iterator it;

			for (it = pArray.begin(); it != pArray.end(); it++)
			{
				if (i % 2 == 0)
					Ch->Write("#600 %-25s #601:#700 %s\n\r", (*it), pData.GetString(*it, pNames.at(i)));
				else
					Ch->Write("#700 %-25s #701:#700 %s\n\r", (*it), pData.GetString(*it, pNames.at(i)));
			}
		}
	}
	else
	{
		bool bValid = false;

		if (gsValue2 == "" )
		{
			Ch->Write("Invalid value:\n\rSyntax: config <field> <value>\n\r");
			return true;
		}

		for (int i = 0; i < pNames.size(); i++)
		{
			gStringArray pArray = pData.GetSectionList(pNames.at(i));
			gStringArray::iterator it;

			for (it = pArray.begin(); it != pArray.end(); it++)
				if (gsValue == (*it))
				{
					if (pData.SetValue((*it), gsValue2, "", pNames.at(i), 0))
						Ch->Write("[GameConfig] %s value changed to %s\n\r", (*it), gsValue2);
					else
						Ch->Write("[GameConfig] Unable to change %s\n\r", (*it));

					bValid = true;
				}			
		}

		if (!bValid)
		{
			Ch->Write("%s is not a valid key. See config for a list.\n\r", gsValue2);
			return true;
		}


	}


	return true;

}


// Method     :: CmdModule
// Class	  :: CModule
// Parameters :: <actor, arguments>
// Arguments  :: <create | list | delete | edit>
//			  :: <list> <component name>
//			  :: <list> <module name>
//			  :: <sort>
//			  :: <create> <type> <name> 
//			  :: <delete> <Module Index #>
//			  :: <edit> <Module Index #> <field> <value>
// Return     :: Bool
// Function   :: Used to create and edit Modules
// Written    :: 23/11/2005 {OWV}

bool CmdModule::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CMLoader* pLoader = CGameObjects::Get().GameWorld()->MLoader();
	
	// Process arguments
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
	if (!CommandLine.empty())
		CommandLine.pop_front();

	// Creating a new Module
	if (gsFunction == "create")
	{
		CModule* pModule = NULL;
		int nIndex = -1;

		if (gsValue1 == "")
		{
			Ch->Write("You must specify a type for this Module. Type 'module types' for a list.\n\r");
			return true;
		}

		// Accept the String or Index value
		if (pModule->Valid(gsValue1) || (nIndex = atoi(gsValue1) < CModule::MT_LAST))
		{
			// Check they Supplied a name
			if (gsValue2 == "")
			{
				Ch->Write("You must supply a name for the module.\n\r");
				return true;
			}
			else
			{
				// Create the object
				pModule = new CModule();
				pModule->m_gsName = gsValue2;
				pModule->m_gsFileName = gsValue2 + ".mod";
				pModule->m_nType = atoi(gsValue1);
				
				
				pLoader->m_Modules.push_back(pModule);

				// Extra fields for map go here
				switch (pModule->m_nType)
				{
					case CModule::MT_ARMOUR_PLATING:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Dissipation", 0));
					}
					break;

					case CModule::MT_POWER_LINK:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("PowerRate", 0));
					}
					break;

					case CModule::MT_SHIELD_GENERATOR:
					{
						// NOTE: 
						// There are four fields for modules that require power:
						// PowerLoad:  The current amount of power being used
						// PowerUsage: The total power required by this module
						// Powered:    Whether the module is powered on or not
						// Power:      Is the module attempting to power up
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));						
						pModule->m_Plus.insert(IntegerMap::value_type("CurrCharge", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("MaxCharge", 0));				
					}
					break;

					case CModule::MT_FABRICATION_MATERIAL:
					case CModule::MT_FUEL_CELLS:
					case CModule::MT_MISSILES:
					case CModule::MT_TORPEDOES:
					case CModule::MT_ROCKETS:
					case CModule::MT_CHAFF:
					case CModule::MT_FLARES:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Quantity", 0));				
					}
					break;

					
					case CModule::MT_REACTOR_PLANT:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerCapacity", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Fuel", 0));
					}
					break;

					case CModule::MT_COOLANT_PLANT:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantRate", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_BATTERY_BANK:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("CurrCapacity", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("MaxCapacity", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_ION_ENGINE:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Propulsion", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Acceleration", 1));
						pModule->m_Plus.insert(IntegerMap::value_type("Maneuver", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_DRIVE_BAFFLES:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Mask", 0));
					}
					break;

					case CModule::MT_HYPERDRIVE_MAIN:
					case CModule::MT_HYPERDRIVE_BACKUP:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Hyper", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("ShipType", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;
					
					case CModule::MT_COLLECTOR_ARRAY:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("RechargeRate", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_RADOME:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Type", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Active", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Range", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_MANUEVERING_THRUSTERS:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Maneuver", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_HEATSINK:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantRate", 0));
					}
					break;

					case CModule::MT_GRAVITY_WELL_PROJECTORS:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("FieldSize", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;
					
					case CModule::MT_COMMUNICATIONS:
					{
						CMComms* pComms = new CMComms();
						pModule->m_Comms = pComms;

						pModule->m_Plus.insert(IntegerMap::value_type("Range", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Memory", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_ASTROGATION_COMPUTER:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Memory", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_REPULSOR_COILS:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Strength", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;
					
					case CModule::MT_HOLONET_TRANSCEIVER:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Range", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Memory", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_BLAST_DOORS:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Delay", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Opening", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Open", 0));
					}
					break;

					case CModule::MT_TRACTOR_BEAM_PROJECTOR:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Range", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Memory", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;
					
					case CModule::MT_TURRET_MOUNT:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("WeaponType", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("TurretType", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("CoolantUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

					case CModule::MT_MUNITION_ELEVATOR:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("LoadTime", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Loading", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Loaded", 0));
					}
					break;
					
					case CModule::MT_ESCAPE_POD:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("LaunchTime", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Type", 0));
					}
					break;

					case CModule::MT_SFOIL_AND_ACTUATORS:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("Position", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("SpeedBonus", 0));
					}
					break;		

					case CModule::MT_LIFE_SUPPORT_UNIT:
					case CModule::MT_FLARE_AND_CHAFF_DISPENSOR:
					case CModule::MT_JAMMING_POD:
					case CModule::MT_SNOOPING_POD:
					case CModule::MT_CONTROL_CONSOLE:
					case CModule::MT_HELM:
					case CModule::MT_SECONDARY_CONTROL_CONSOLE:
					case CModule::MT_PILOT_CONSOLE:
					case CModule::MT_COPILOT_CONSOLE:
					case CModule::MT_GUNNERY_CONSOLE:
					case CModule::MT_WEAPONS_CONSOLE:
					case CModule::MT_NAV_CONSOLE:
					case CModule::MT_ENGINEERING_CONSOLE:
					case CModule::MT_COMMANDER_CONSOLE:
					{
						pModule->m_Plus.insert(IntegerMap::value_type("PowerLoad", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("PowerUsage", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Powered", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Power", 0));
						pModule->m_Plus.insert(IntegerMap::value_type("Timer", 0));
					}
					break;

				}
				
				Ch->Write("Module created.\n\r");

				pModule->Save();
				pLoader->Save(); 
				return true;
			}
		}
		else
		{
			Ch->Write("That is not a valid type. Type 'module types' for a complete list.\n\r");
			return true;
		}


	}

	// Deleting a Module
	if (gsFunction == "delete")
	{
		// They entered an index
		if (gsValue1 == "")
		{
			Ch->Write("You must enter an Index to delete.\n\r");
			return true;
		}
		
		// Its a valid index
		if (atoi(gsValue1) > pLoader->m_Modules.size() || atoi(gsValue1) == 0)
		{
			Ch->Write("That is not a valid index. Type module to view current modules.\n\r");
			return true;
		}
		else
		{
			CModule* pModule = pLoader->m_Modules.at(atoi(gsValue1) -1);

			pLoader->m_Modules.erase(pLoader->m_Modules.begin()+atoi(gsValue1)-1);

			// Delete object

			gString szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Ships\\Modules\\" + pModule->m_gsFileName;
			unlink(szFile);

			pLoader->Save();

			pModule = NULL;

			Ch->Write("Module deleted.\n\r");
			return true;
		}
	}

	// Showing a module
	if (gsFunction == "show" || gsFunction == "view")
	{
		// Make sure its a valid Index
		if (atoi(gsValue1) > pLoader->m_Modules.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("That is not a valid index. Type module to view current modules.\n\r");
			return true;
		}
		else
		{
			CModule* pModule;

			pModule = pLoader->m_Modules.at(atoi(gsValue1)-1);
			Ch->Write("#600:#601:#701 %s #600[#601%s#600]#601:#600:#700\n\r", pModule->m_gsName, pModule->m_gsFileName);
			Ch->Write("Type#600:#701 %s(%d)\t#700Size#600:#701 %4d\t#700Mass#600:#701 %6d #600kg#700\n\r", CModule::szModules[pModule->m_nType], pModule->m_nType, pModule->m_nSize, pModule->m_nMass);
			Ch->Write("Resilience#600:#701 %d\t#700Durability#600: [%d/%d]#700\n\r", pModule->m_nResilience, pModule->m_ncDurability, pModule->m_nmDurability);

			if (pModule->m_Plus.size() > 0)
			{ 
				for (IntegerMap::iterator imap = pModule->m_Plus.begin(); imap != pModule->m_Plus.end(); imap++)
				{
					// Don't show the Power field
					if ((*imap).first == "Power")
						continue;

					// Don't show the Powered field
					if ((*imap).first == "Powered")
						continue;

					if ((*imap).first == "CoolantLoad")
						continue;

					if ((*imap).first == "PowerLoad")
						continue;

					// Don't show the Timer field
					if ((*imap).first == "Timer")
						continue;

					// Special cases
					// [1] Turret Types
					if ((*imap).first == "TurretType")
						Ch->Write("#700%-12s#600:#701 %s [%d]\n\r", (*imap).first, CShip::szTurretTypes[(*imap).second], (*imap).second);
					else if ((*imap).first == "WeaponType")
						Ch->Write("#700%-12s#600:#701 %s [%d]\n\r", (*imap).first, CShip::szWeaponTypes[(*imap).second], (*imap).second);
					else if ((*imap).first == "Type")
						Ch->Write("#700%-12s#600:#701 %s [%d]\n\r", (*imap).first, CShip::szSensors[(*imap).second], (*imap).second);
					else
						Ch->Write("#700%-12s#600:#701 %d\n\r", (*imap).first, (*imap).second);

				}
			}

		}
	}

	// Showing all possible types
	if (gsFunction == "types")
	{
		Ch->Write("#600:#601:#701 Module Types #601:#600:#700\n\r\n\r");
		for (int i = 0; i < CModule::MT_LAST; i++)
		{
			Ch->Write(" #600[#601 %2d#600]#700 %-30s", i, CModule::szModules[i]);

			if ((i+1) % 2 == 0)
				Ch->Write("\n\r");
		}

		Ch->Write("\n\r");

	}

	// Displaying a list of all modules 
	if (gsFunction == "" || gsFunction == "list" || gsFunction == "sort")
	{
		if ((gsFunction == "list" || gsFunction == "")&& gsValue1 == "")
		{
			// We want an easy way to list all the modules that can be installed 
			// in this component if the player is in design mode
			if (((CPlayer*)Ch)->m_Component)
			{
				Ch->ExecuteCommand("module", "list \"%s\"", CComponent::szComponents[((CPlayer*)Ch)->m_Component->m_Type]);
				return true;
			}
					
			Ch->Write("#600:#601:#700 Modules #601:#600:#700\n\r");

			int i = 1;

			// The Module List will be sorted by types to make it easier to view
			if (pLoader->m_Modules.size() <= 0)
				Ch->Write("#600>#601>#700 No modules created.\n\r");
			else
				for (ModuleList::iterator mod = pLoader->m_Modules.begin(); mod != pLoader->m_Modules.end(); mod++)
				{
					Ch->Write("#600[#601 %2d#600]#701 %s#700\n\r", i, (*mod)->m_gsName);
					i++;
				}

			Ch->Write("\n\r#601Alternative Syntax#600:#700\n\r\t#600>#601>#701 module sort #600- Sorted by Module Type#700\n\r\t#600>#601>#701 module list <component name> #600- View all modules that can be installed in this component type#700\n\r\t#600>#601>#701 module list <Module Type> #600- Shows all modules of a specified Type#700\n\r");
			return true;
		}
		if (gsFunction == "sort")
		{
			std::map<int, ModuleMap>ModuleSort;
			std::map<int, ModuleMap>::iterator mList;

			Ch->Write("#600:#601:#700 Sorted Modules #601:#600:#700\n\r");

			int nCount = 1;
			// The Module List will be sorted by types to make it easier to view
			if (pLoader->m_Modules.size() <= 0)
				Ch->Write("#600>#601>#700 No modules created.\n\r");
			else
				for (ModuleList::iterator mod = pLoader->m_Modules.begin(); mod != pLoader->m_Modules.end(); mod++)
				{
					// We add a ModuleList for each type					
					if ( (mList = ModuleSort.find((*mod)->m_nType)) != ModuleSort.end())
					{
						// We already have a list to use
						(*mList).second.insert(ModuleMap::value_type(nCount, (*mod)));
					}
					else
					{
						// We don't have a list for this type yet
						ModuleMap mlNew;
						mlNew.insert(ModuleMap::value_type(nCount, (*mod)));
						ModuleSort.insert(std::map<int, ModuleMap>::value_type((*mod)->m_nType, mlNew));

					}

					nCount++;					
				}

				// We've sorted the modules now lets output them all
				int i = 1;
				int nColour = 1;
				for (mList = ModuleSort.begin(); mList != ModuleSort.end(); mList++)
				{
					if (nColour > 6)
						nColour = 1;

					Ch->Write("#600>#601>#%d00 %s#700\n\r", nColour, CModule::szModules[(*mList).first]);
					for (ModuleMap::iterator it = (*mList).second.begin(); it != (*mList).second.end(); it++)
					{										
						Ch->Write("#600[#601 %2d#600]#700 %s#700\n\r", ((*it).first), ((*it).second)->m_gsName);
						i++;
					}
					
					nColour++;
				}
		}
		else
		{
			CModule* pMod = NULL;
			CComponent* pComponent = NULL;

			// If their entry is a valid module type then lets list all modules of this type
			if (pMod->Valid(gsValue1))
			{
				Ch->Write(" #600:#601:#701 Modules#700:#700 %s #601:#600:#700\n\r", gsValue1);
				int i = 1;
				for (ModuleList::iterator mod = pLoader->m_Modules.begin(); mod != pLoader->m_Modules.end(); mod++)
				{
					if ((*mod)->m_nType == pMod->GetIndex(gsValue1))
						Ch->Write("#600[#601 %2d#600]#701 %s#700\n\r", i, (*mod)->m_gsName);
					i++;
				}
			}
			else if (pComponent->Valid(gsValue1))
			{
				Ch->Write(" #600:#601:#701 Modules#700:#701 %s #601:#600:#700\n\r", gsValue1);
				int i = 1;
				
				switch(pComponent->GetIndex(gsValue1))
				{
					case CComponent::CT_SHIELD:
						pComponent = new CShield();
						break;
					case CComponent::CT_ENGINEERING:
						pComponent = new CEngspace();
						break;
					case CComponent::CT_SUBLIGHT:
						pComponent = new CSublight();
						break;
					case CComponent::CT_HYPERDRIVE:
						pComponent = new CHyperdrive();
						break;
					case CComponent::CT_MAGAZINE:
						pComponent = new CMagazine();
						break;
					case CComponent::CT_EXTERNAL:
						pComponent = new CExternal();
						break;
					case CComponent::CT_INTERNAL:
						pComponent = new CInternal();
						break;
					case CComponent::CT_LANDING:
						pComponent = new CLanding();
						break;
					case CComponent::CT_BULKSTORAGE:
						pComponent = new CBulk();
						break;
					case CComponent::CT_CONTROLPOINT:
						pComponent = new CControl();
						break;
					case CComponent::CT_WEAPONMOUNT:
						pComponent = new CWeapon();
						break;
					case CComponent::CT_ESCAPEPOD:
						pComponent = new CEscape();
						break;
					case CComponent::CT_SFOIL:
						pComponent = new CSFoil();
						break;
				}
				for (ModuleList::iterator mod = pLoader->m_Modules.begin(); mod != pLoader->m_Modules.end(); mod++)
				{
					if (pComponent->CanInstall((*mod)->m_nType))
						Ch->Write("#600[#601 %2d#600]#701 %s#700\n\r", i, (*mod)->m_gsName);
					i++;
				}
			}
			else 
			{
				int i = 1;
				for (ModuleList::iterator mod = pLoader->m_Modules.begin(); mod != pLoader->m_Modules.end(); mod++)
				{
					if ((*mod)->m_gsName == gsValue1)
						Ch->Write("#600[#601 %2d#600]#701 %s#700\n\r", i, (*mod)->m_gsName);
					i++;
				}
			}

		}

	}

	// Editing a module
	if (gsFunction == "edit")
	{
		// module edit <#> <field> <value>

		// Make sure its a valid Index
		if (atoi(gsValue1) > pLoader->m_Modules.size() || atoi(gsValue1) == 0)
		{
			Ch->Write("That is not a valid index. Type module to view current modules.\n\r");
			return true;
		}
		else
		{
			bool bEdit = false;
			CModule* pModule;
			pModule = pLoader->m_Modules.at(atoi(gsValue1)-1);

			// Check for a valid field
			// Size
			if (gsValue2 == "size")
			{
				if (atoi(gsValue3) > 0)
				{
					pModule->m_nSize = atoi(gsValue3);
					Ch->Write("Size set.\n\r");
					pModule->Save();
					return true;
				}
				else
				{
					Ch->Write("Sizes must be positive values.\n\r");
					return true;
				}
			}
			// Name
			if (gsValue2 == "name")
			{
				pModule->m_gsName = gsValue3;
				pModule->Save();
				Ch->Write("Name set.\n\r");
				return true;
			}
			// Mass
			if (gsValue2 == "mass")
			{
				if (atoi(gsValue3) > 0)
				{
					pModule->m_nMass = atoi(gsValue3);
					Ch->Write("Mass set.\n\r");
					pModule->Save();
					return true;
				}
				else
				{
					Ch->Write("Mass must be positive values.\n\r");
					return true;
				}
			}
			// Resilience
			if (gsValue2 == "Resilience")
			{
				if (atoi(gsValue3) > 0)
				{
					pModule->m_nResilience = atoi(gsValue3);
					Ch->Write("Resilience set to %d\n\r", atoi(gsValue3));
					pModule->Save();
					return true;
				}
				else
				{
					Ch->Write("Resilience must be greater than 0.\n\r");
					return true;
				}
			}
			// Current Durability
			if (gsValue2 == "Durability")
			{
				if (atoi(gsValue3) > 0)
				{
					pModule->m_nmDurability = atoi(gsValue3);
					pModule->m_ncDurability = atoi(gsValue3);
					Ch->Write("Maxmium Durability set\n\r");
					pModule->Save();
					return true;
				}
				else
				{
					Ch->Write("The Durability must be greater 0.\n\r");
					return true;
				}
			}
			// Check the IntegerMap for values now
			if (pModule->m_Plus.find(gsValue2) != pModule->m_Plus.end())
			{
				gString gsField;

				// #TODO# Heavy range checking here to ensure that the value they entered is valid

				(*pModule->m_Plus.find(gsValue2)).second = atoi(gsValue3);
								
				Ch->Write("%s set.\n\r", (*pModule->m_Plus.find(gsValue2)).first);
				pModule->Save();
				return true;
			}
		}
	}

	return true;

}


// Method     :: CmdCrews
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <none>
//			  :: <create> <name> <type>
//			  :: <edit> <Crew Index #> <field> <value>
//			  :: <assign> <Crew Index #> <Ship> <Home> <Duty>
// Return     :: Bool
// Function   :: Allows the creation of Ship Crews
// Written    :: 14/02/2006 {OWV}

bool CmdCrews::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CCLoader* pLoader = CGameObjects::Get().GameWorld()->CLoader();

	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue3 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 3
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue4 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 4
		if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue5 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 5
		if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue6 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 6

	// [1] No argument display all Crews
	if (gsValue == "")
	{
		Ch->Write("#300:#301: #700Unassigned Ship Crews #301:#300:#700\n\r");
		
		if (pLoader->m_Crews.size() <= 0)
		{
			Ch->Write(" No Ship crews created.\n\r");
			return true;
		}
		else
		{
			int nCount = 0;
			int nTotal = 0;

			for (CrewList::iterator it = pLoader->m_Crews.begin(); it != pLoader->m_Crews.end(); it++)
			{
				nCount++;
				if (!(*it)->m_Ship)
				{
					Ch->Write("#300[#301 %3d#300]#700 %-20s #300>#301>#701 %-16s #300[#301 %d#300/#301 %d#300]#700 CO#300:#701 %s#700\n\r", nCount, (*it)->m_gsName, CCrew::szTypes[(*it)->m_nType], (*it)->m_ncCompliment, (*it)->m_nmCompliment, (*it)->m_Leader ? ("Ensign " + (*it)->m_Leader->Name()) : "No Leader");
					nTotal++;
				}

			}
			Ch->Write("#300[#301 %3d#300]#700 Unassigned Crew%s.\n\r", nTotal, nTotal > 1 ? "s" : " ");
			Ch->Write("#301[#300 %3d#301]#700 Crew%s in Total.\n\r", nCount, nCount > 1 ? "s" : " ");
		}
	}
	// [2] They want to create a new Ship Crew
	else if (gsValue == "create")
	{
		// Value2 == Name 
		// Value3 == Type
		if (gsValue2 == "")
		{
			Ch->Write("#100[#101Invalid Name#100]#700 You must enter a name for the Ship crew.\n\r");
			Ch->Write("Syntax#300:#701 Crew create <Crew Name> <Crew Type>#700\n\r");
			return true;
		}
		else if (gsValue3 == "" || atoi(gsValue3) >= CCrew::CT_MAX)
		{
			Ch->Write("#100[#101Invalid Type#100]#700 You must enter a Type for the Ship crew.\n\r");
			Ch->Write("Syntax#300:#701 Crew create <Crew Name> <Crew Type>#700\n\r");
			Ch->Write("Valid types#300:#701\n\r");
			for (int i = 0; i < CCrew::CT_MAX; i++)
				Ch->Write("[%d] %s\n\r", i, CCrew::szTypes[i]);
			return true;
		}

		// Check the name is unique
		for (CrewList::iterator it = pLoader->m_Crews.begin(); it != pLoader->m_Crews.end(); it++)
		{
			if ((*it)->m_gsName == gsValue2)
			{
				Ch->Write("#100[#101Name Exists#100]#700 A Crew named, '%s' already exists.\n\r", gsValue2);
				return true;
			}
		}

		// Clear validation, lets create the Crew
		CCrew* pCrew = new CCrew();

		// Set the fields
		pCrew->m_gsName = gsValue2;
		pCrew->m_gsFileName = gsValue2 + ".cre";
		pCrew->m_nType = atoi(gsValue3);
		pCrew->Save();

		pLoader->m_Crews.push_back(pCrew);
		pLoader->Save();
		Ch->Write("Crew added.\n\r");
		return true;
	// End Create	
	}
	// [3] They want to edit a field in the Crew
	else if (gsValue == "Edit")
	{
		if (gsValue2 == "")
		{
			Ch->Write("#100[#101No Entry#100]#700 You must enter a field to edit.\n\r");
			Ch->Write("Syntax#300:#701 Crew #300<#301edit#300> <#301Crew Index####300> <#301Name#300|#301Compliment#300|#301Type#300|#301Leader#300> <#301Value#300>#700\n\r");
			return true;
		}

		if (atoi(gsValue2) < 0 || atoi(gsValue2) > pLoader->m_Crews.size())
		{
			Ch->Write("#100[#101Invalid Index#100]#700 %d is not a valid Crew Index.\n\r", atoi(gsValue2));
			Ch->Write("Type#300:#701 Crews#700 To view full list.\n\r");
			return true;
		}

		CCrew* pCrew = pLoader->m_Crews.at(atoi(gsValue2)-1);

		if (!pCrew)
			return false;

		// Value2 == Crew to edit
		// Value3 == Field
		// Value4 == Value to Replace
		if (gsValue3 == "Name")
		{
			if (gsValue4 == "")
			{
				Ch->Write("#100[#101Invalid Input#100]#700 You must enter a new name for this Crew\n\r");
				return true;
			}

			pCrew->m_gsName = gsValue4;
			Ch->Write("Named changed.\n\r");
			pCrew->Save();
			return true;
		}
		else if (gsValue3 == "Compliment")
		{
			if (gsValue4 == "")
			{
				Ch->Write("#100[#101Invalid Input#100]#700 You must enter a maximum Compliment for this Crew\n\r");
				return true;
			}
			else if (atoi(gsValue4) <= 0)
			{
				Ch->Write("#100[#101Invalid Input#100]#700 The number must be bigger than zero.\n\r");
				return true;
			}


			pCrew->m_nmCompliment = atoi(gsValue4);
			pCrew->m_ncCompliment = atoi(gsValue4);
			pCrew->Save();
			Ch->Write("Crew Compliment set.\n\r");
			return true;

		}
		else if (gsValue3 == "Type")
		{
			if (gsValue4 == "")
			{
				Ch->Write("#100[#101Invalid Input#100]#700 You must enter a new Type for this Crew\n\r");
				Ch->Write("Valid types#300:#701\n\r");
				for (int i = 0; i < CCrew::CT_MAX; i++)
					Ch->Write("[%d] %s\n\r", i, CCrew::szTypes[i]);
			
				return true;
			}
			else if (atoi(gsValue4) >= CCrew::CT_MAX)
			{
				Ch->Write("#100[#101Invalid Type#100]#700 %d is not a valid type.\n\r", atoi(gsValue4));
				Ch->Write("Valid types#300:#701\n\r");
				for (int i = 0; i < CCrew::CT_MAX; i++)
					Ch->Write("[%d] %s\n\r", i, CCrew::szTypes[i]);
			
				return true;
			}

			pCrew->m_nType = atoi(gsValue4);
			pCrew->Save();
			Ch->Write("Crew Type set to %s.\n\r", CCrew::szTypes[atoi(gsValue4)]);
			return true;

		}
		else if (gsValue3 == "Leader")
		{
			if (gsValue4 == "")
			{
				Ch->Write("#100[#101Invalid Input#100]#700 You must enter a name for the Leader\n\r");
				return true;
			}

			if (pCrew->m_Leader)
			{
				Ch->Write("Ensign %s replaced with Ensign %s.\n\r", pCrew->m_Leader->Name(), gsValue4);
				delete pCrew->m_Leader;
				pCrew->m_Leader = NULL;
			}
			else
			{
				Ch->Write("Ensign %s set as Crew Leader.\n\r", gsValue4);
			}

			CNpc* pNpc = new CNpc();
			pNpc->SetName(gsValue4);
			pNpc->SetDescription("Ensign " + gsValue4 + " stands here, in charge of the " + CCrew::szTypes[pCrew->m_nType]);
			pNpc->SetHomeWorld(CGameObjects::Get().GameWorld());
			pNpc->SetShortDesc("Ensign " + gsValue4);
			pCrew->m_Leader = pNpc;
			pCrew->Save();
			return true;

		}

		Ch->Write("#100[#101Invalid Syntax#100]#700 Syntax#300:#701 Crew <edit> <Crew Index ###> <Field> <Value>\n\r");
		Ch->Write("Valid Fields #300:#700 Name, Type, Compliment, Leader\n\r");
		return true;
	}
	// [4] Assigning a Crew to a Ship
	if (gsValue == "Assign")
	{
		if (gsValue2 == "")
		{
			Ch->Write("#100[#101No Entry#100]#700 You must enter a ship to assign this crew to.\n\r");
			Ch->Write("Syntax#300:#701 Crew #300<#301assign#300> <#301Crew Index####300> <#301Ship name#300> <#301Home Station#300> <#301Duty Station#300> #700\n\r");
			return true;
		}

		// gsValue2 == Crew
		// gsValue3 == Ship
		// gsValue4 == Home station
		// gsValue5 == Duty station

		// When we assign a crew to a ship we remove it from our Crew Loader
		// and add it to the Crew Map of the Ship in question
		if (atoi(gsValue2) < 0 || atoi(gsValue2) > pLoader->m_Crews.size())
		{
			Ch->Write("#100[#101Invalid Index#100]#700 %d is not a valid Crew Index.\n\r", atoi(gsValue2));
			Ch->Write("Type#300:#701 Crews#700 To view full list.\n\r");
			return true;
		}

		CCrew* pCrew = pLoader->m_Crews.at(atoi(gsValue2)-1);

		if (!pCrew)
			return false;

		if (!pCrew->m_Leader)
		{
			Ch->Write("#100[#101Invalid Leader#100]#700 The Crew must have a leader set to it before it is assigned a ship.\n\r");
			return true;
		}

		CShip* pShip = pGalaxy->GetShi(gsValue3);

		if (!pShip)
		{
			Ch->Write("#100[#101Invalid Ship#100]#700 %s is not a Ship. Type ships to view valid choices.\n\r", gsValue3);
			return true;
		}

		if (pCrew->m_Ship)
			return true;

		if (atoi(gsValue4) < 0 && atoi(gsValue5) < 0)
		{
			Ch->Write("#100[#101Invalid Home#100]#700 You must designate a Home and Duty station for the Crew.\n\r");
			Ch->Write("Syntax#300:#701 Crew Assign #300<#301Crew ####300> <#301Ship Name#300> <#301Home Vnum#300> <#301Duty Vnum>#700\n\r");
			return true;
		}
		
		// Room messages
		gString gsHome;
		gString gsDuty;

		// Validate their entries
		if (!pShip->m_Area)
		{
			Ch->Write("#100[#101Invalid Area#100]#700 Ship has invalid area, unable to add crew.\n\r");
			return true;
		}
		else
		{
			if (!pShip->m_Area->GetRoom(atoi(gsValue4)))
			{
				Ch->Write("#100[#101Invalid Home Vnum#100]#700 %d is not a valid vnum in %s\n\r", atoi(gsValue4), pShip->m_gsName);
				return true;
			}
			if (!pShip->m_Area->GetRoom(atoi(gsValue5)))
			{
				Ch->Write("#100[#101Invalid Duty Vnum#100]#700 %d is not a valid vnum in %s\n\r", atoi(gsValue5), pShip->m_gsName);
				return true;
			}

			// Set their locations
			gsHome = pShip->m_Area->GetRoom(atoi(gsValue4))->Name();
			gsDuty = pShip->m_Area->GetRoom(atoi(gsValue5))->Name();
			pCrew->m_Home = new CPlacement(pShip->m_Area->Area(), atoi(gsValue4), pShip->m_Area->World());
			pCrew->m_Location = new CPlacement(pShip->m_Area->Area(), atoi(gsValue5), pShip->m_Area->World());

		}

		// Got our Crew and the Ship so move the Crew to the Ship
		CrewMap::iterator it = pShip->m_Crew.find(pCrew->m_nType);

		if (it == pShip->m_Crew.end())
		{
			// This is the first crew of this type to be added
			CrewList pList;
			pList.push_back(pCrew);
			pShip->m_Crew.insert(CrewMap::value_type(pCrew->m_nType, pList));
		}
		else
		{
			((*it).second).push_back(pCrew);
		}

		if (pCrew->m_Leader)
		{
			// Need to add the Leader to the Off Duty room
			Ch->Write("%s's Team assigned to the %s#300:#700 Duty #300<#301%s#300>#700 Home #300<#301%s#300>#700.\n\r", pCrew->m_Leader->Name(), pShip->m_gsName, gsDuty, gsHome);

		}
		else
			Ch->Write("%s Team assigned to the %s#300:#700 Duty #300<#301%s#300>#700 Home #300<#301%s#300>#700.\n\r", pCrew->m_gsName, pShip->m_gsName, gsDuty, gsHome);

		

		pCrew->m_Ship = pShip;
		pCrew->Save();
		pShip->Save();
		pLoader->Save();

		return true;
	}


	return true;
}

// Method     :: CmdMotd
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <list|delete <number>|add <name|text>>
// Return     :: Bool
// Function   :: Allows the motd to be added to and modified in game
// Written    :: 30/03/2006

bool CmdMotd::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2

	// PC only command
	if (Ch->IsNPC())
		return false;

	CPlayer* pPlayer = (CPlayer*)Ch;

	if (gsValue.IsEmpty())
	{
		CGameObjects::Get().WriteMotd(pPlayer->User(), 30, true);
		Ch->Write("\n\rExtra syntax#400:#701 Motd #400<#401Delete#400> <#401Entry ####400>\n\r              #701Motd #400<#401Add#400> <#401Name#400> <#401Description#400>#700\n\r");
		return true;
	}
	return true;
}

// Method     :: CmdRace
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none> | <list>
//			  :: <view #>
// Return     :: Bool
// Function   :: Allows a Staff member to view all the races in game
// Written    :: 23/05/06 {OWV}

bool CmdRace::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2


	// List all races
	if (gsValue == "" || gsValue == "list")
	{
		Ch->Write("#600:#601:#701 SWU Races #601:#600:#700\n\r");

		int nCount = 0;
		for (RaceList::iterator race = CGameObjects::Get().GameWorld()->RaceMgr()->Races().begin(); race != CGameObjects::Get().GameWorld()->RaceMgr()->Races().end(); race++)
		{
			CRace* pRace = (*race);
			nCount++;
			Ch->Write("#600[#601%2d#600]#700 %s\n\r", nCount, pRace->Name());			
		}
		Ch->Write("\n\r#601%d#701 Race%s in total.#700\n\r", nCount, nCount > 1 ? "s" : "");
	}

	if (gsValue == "view" || gsValue == "show")
	{
		if (gsValue2.IsEmpty())
		{
			Ch->Write("#101[#100Error#101]#700 No race number entered.\n\r");
			Ch->Write("You must enter the index number of the race to view.\n\r");
			return true;
		}

		if ((atoi(gsValue2)-1) > CGameObjects::Get().GameWorld()->RaceMgr()->Races().size())
		{
			Ch->Write("#101[#100Invalid Race Numberr#101]#701 %d#700 is not a valid choice.\n\r", atoi(gsValue2));
			Ch->Write("Valid range #600[#6011 - %d#600]#700\n\r", CGameObjects::Get().GameWorld()->RaceMgr()->Races().size());
			return true;
		}

		// Get the race
		CRace* pRace = CGameObjects::Get().GameWorld()->RaceMgr()->Races().at(atoi(gsValue2)-1);

		if (!pRace)
		{
			Ch->Write("#101[#100Invalid Object#101]#700 Contact an administrator.\n\r");
			return true;
		}

		Ch->Write("#600////////////////////////#601[#701 %-15s#700 #601]#600\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\r", pRace->Name());
		// Display attributes
		Ch->Write("#600// ATTRIBUTES#601:#600                                                 \\\\#700\n\r");
		AttributeMap::iterator att;

		if (pRace->Attributes().m_Attributes.size() <= 0)
		{
			Ch->Write("#600//#700                         #701No Attributes                       #600\\\\#700\n\r");
		}
		else
		{
			int nColumn = 1;
			for (att=pRace->Attributes().m_Attributes.begin(); att != pRace->Attributes().m_Attributes.end(); att++)
			{
				CAttribute* pA = (*att).second;
				gString gsName = pA->Name();

				if (nColumn % 2 == 1)
				{
					Ch->Write("#600//#701  %12s #600: #601[#600%2d#601,#600%2d#601,#600%2d#601]#701       ", 
					(const char*)gsName, pA->Min(), pA->Max(), pA->Cur());	
				}
				else
				{
					Ch->Write(" %12s #600: #601[#600%2d#601,#600%2d#601,#600%2d#601] #600\\\\#700\n\r", 
					(const char*)gsName, pA->Min(), pA->Max(), pA->Cur());
				}
				nColumn++;
			 }
		}
		

		// Display bodyparts
		int	nColumn = 1;
		Ch->Write("#600//--------------------------#601[#701BODYPARTS#601]#600------------------------\\\\#700\n\r");
		for (BodyPartMap::iterator bp = pRace->BodyParts().begin(); bp != pRace->BodyParts().end(); bp++)
		{
			CBodyPart* pPart = (*bp).second;

			if (nColumn % 2 == 1)
			{
				Ch->Write("#600//#701   %15s #501[#500%3.0f#501|#500%2d#501]#701        ", CRace::szBodyParts[pPart->Type()], pPart->Percentage(), pPart->Hitpoints());
			}
			else					
			{
				Ch->Write(" %15s #501[#500%3.0f#501|#500%2d#501] #600\\\\#700\n\r", CRace::szBodyParts[pPart->Type()], pPart->Percentage(), pPart->Hitpoints());				
			}

			nColumn++;			
		}
		// Handle odd count
		if (pRace->BodyParts().size() % 2)
		{
			Ch->Write("                          #600\\\\#700\n\r");
		}

		// Display Languages
		Ch->Write("#600//--------------------------#601[#701LANGUAGES#601]#600------------------------\\\\#700\n\r");
		nColumn = 0;
		for (gStringList::iterator it = pRace->Languages().begin(); it != pRace->Languages().end(); it++)
		{
			if (nColumn == 0)
			{
				Ch->Write("#600//#701 %-14s ", (*it));
				nColumn++;
			}
			else if (nColumn == 3)
			{
				Ch->Write("%-14s #600\\\\#700\n\r", (*it));
				nColumn = 0;
			}
			else
			{
				Ch->Write("%-14s ", (*it));
				nColumn++;
			}
		}
		// Formatting fix
		if (pRace->Languages().size() % 4 != 0)
		{
			int nRemainder = 4-(pRace->Languages().size() % 4);

			for (int i = 0; i < nRemainder; i++)
				Ch->Write("%-15s", " ");

			Ch->Write("#600\\\\#700\n\r");			
		}

		// Display Perks
		Ch->Write("#600//----------------------------#601[#701PERKS#601]#600--------------------------\\\\#700\n\r");
		nColumn = 0;
		for (gStringList::iterator it = pRace->Perks().begin(); it != pRace->Perks().end(); it++)
		{
			if (nColumn == 0)
			{
				Ch->Write("#600//#701 %-19s ", (*it));
				nColumn++;
			}
			else if (nColumn == 2)
			{
				Ch->Write("%-19s  #600\\\\#700\n\r", (*it));
				nColumn = 0;
			}
			else
			{
				Ch->Write("%-19s ", (*it));
				nColumn++;
			}
		}
		// Formatting fix
		if (pRace->Perks().size() % 3 != 0)
		{
			int nRemainder = 3-(pRace->Perks().size() % 3);

			for (int i = 0; i < nRemainder; i++)
				Ch->Write("%-20s", " ");

			Ch->Write("#600\\\\#700\n\r");
			
		}

		// Finished
		Ch->Write("#600//////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");





	}

	return true;
}

// Method     :: CmdHullcube
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none> | <list>
//			  :: <view #>
// Return     :: Bool
// Function   :: Allows a Staff member to create and modify hullcubes
// Written    :: 01/06/06 {OWV}

bool CmdHullcube::Perform(CActor* Ch, gStringList& CommandLine)
{
	CHLoader* pLoader = CGameObjects::Get().GameWorld()->HLoader();

	// Process arguments
	gString gsFunction = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Function to perform
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
	if (!CommandLine.empty())
		CommandLine.pop_front();
 
	// List all HullCubes
	if (gsFunction == "list" || gsFunction.IsEmpty())
	{
		Ch->Write("#600:#601:#700 HullCubes #601:#600:#700\n\r");

		if (pLoader->m_HullCubes.size() <= 0)
		{
			Ch->Write(" No HullCubes created.\n\r");
			return true;
		}

		int nCount = 1;

		for (int i = 0; i < CHull::HT_MAX; i++)
		{
			bool bFound = false;
			Ch->Write("#701%s#700\n\r", CHull::szHullTypes[i]);
			for (HullList::iterator hull = pLoader->m_HullCubes.begin(); hull != pLoader->m_HullCubes.end(); hull++)
			{
				if ((*hull)->m_nType == i)
				{
					bFound = true;
					Ch->Write("   #600[#601%3d#600]#701 %s #700\n\r", nCount, (*hull)->m_gsName);
					nCount++;
				}
			}
			if (!bFound)
				Ch->Write("   #600No HullCubes#700\n\r");
		}

		return true;
	}
	// Create a new HullCube
	if (gsFunction == "create")
	{
		if (!Ch->IsAdministrator())
		{
			Ch->Write("#100[#101Invalid Access#100]#700 Only Administrators can create HullCubes.#700\n\r");
			return true;
		}

		if (gsValue1.IsEmpty())
		{
			Ch->Write("#100[#101No Input#100]#700 You must enter a name for the HullCube\n\r");
			Ch->Write("Syntax#600:#701 hullcube create #600<#601HullCube Name#601>#700\n\r");
			return true;
		}

		// Does the name already exist?
		CHull* pTemp = new CHull;
		gString gsFile = gsValue1 + ".hul";
		if (pTemp->Load(gsFile))
		{
			Ch->Write("#100[#101Filename Exists#100]#700 A HullCube called #701%s#700 already exists.\n\r", gsValue1);
			Ch->Write("Select a different name for the HullCube.#700\n\r");
			delete pTemp;
			return true;
		}

		// Free the memory
		delete pTemp;

		CHull* pHull = new CHull;
		pHull->m_gsName = gsValue1;
		pHull->m_gsFileName = gsValue1 + ".hul";

		pHull->Save();
		pLoader->m_HullCubes.push_back(pHull);
		pLoader->Save();

		Ch->Write("#601HullCube created and saved.#700\n\r");
		return true;
		
	}
	// Edit a HullCube
	if (gsFunction == "edit")
	{
		if (!Ch->IsAdministrator())
		{
			Ch->Write("#100[#101Invalid Access#100]#700 Only Administrators can edit HullCubes.#700\n\r");
			return true;
		}

		if (gsValue1.IsEmpty())
		{
			Ch->Write("#100[#101Invalid Entry#100]#700 You must enter a HullCube number to edit.\n\r");
			Ch->Write("Type#600:#701 hullcube#700 to view a list of valid HullCubes.\n\r");
			return true;
		}

		// Make sure its a valid Index
		if (atoi(gsValue1) > pLoader->m_HullCubes.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("#100[#101Invalid Entry#100]#701 %d #700 is not a valid HullCube Index\n\r", atoi(gsValue1));
			Ch->Write("Valid Indexes#600:#701 1 - %d#700\n\r", pLoader->m_HullCubes.size());
			return true;
		}
		else
		{
			// Ok, valid index so lets get the hullcube they asked for
			CHull* pHull = pLoader->m_HullCubes.at(atoi(gsValue1)-1);

			if (gsValue3.IsEmpty())
			{
				Ch->Write("#100[#101No Input#100]#700 You must input a replacement value.\n\r");
				Ch->Write("Syntax#600:#701 Hullcube edit #600<#601Index Num#600> <#601Field to Edit#600> <#601Value#600>#700\n\r");
				return true;
			}
			// Now check what they want to edit

			// [1] Mass
			if (gsValue2 == "Mass")
			{
				pHull->m_nMass = atoi(gsValue3);
				Ch->Write("#601Mass set to %d#700\n\r", atoi(gsValue3));
				pHull->Save();
				return true;
			}
			// [2] Resilience
			if (gsValue2 == "Resilience")
			{
				pHull->m_nResilience = atoi(gsValue3);
				Ch->Write("#601SOAK value set to %d#700\n\r", atoi(gsValue3));
				pHull->Save();
				return true;
			}
			// [3] Size 
			if (gsValue2 == "size")
			{
				pHull->m_nSize = atoi(gsValue3);
				Ch->Write("#601Size value set to %d#700\n\r", atoi(gsValue3));
				pHull->Save();
				return true;
			}
			// [4] Keel
			if (gsValue2 == "keel")
			{
				pHull->m_nMKeel = atoi(gsValue3);
				Ch->Write("#601Keel value set to %d#700\n\r", atoi(gsValue3));
				pHull->Save();
				return true;
			}
			// [5] Type
			if (gsValue2 == "type")
			{
				if (atoi(gsValue3) >= CHull::HT_MAX)
				{
					Ch->Write("#100[#101Invalid Type#100]#701 %d#700 is not a valid HullCube type#700\n\r");
					Ch->Write("Valid Types#600:#700\n\r");
					for (int i = 0; i < CHull::HT_MAX; i++)
						Ch->Write("#600[#601%d#600]#701 %s#700\n\r", i, CHull::szHullTypes[i]);
					return true;
				}

				pHull->m_nType = atoi(gsValue3);
				pHull->Save();
				Ch->Write("#601HullCube type set to %s#700\n\r", CHull::szHullTypes[pHull->m_nType]);
				return true;
			}
			
			Ch->Write("#100[#101Invalid Field#100]#701 %s #700 is not a valid field to edit.\n\r");
			Ch->Write("Syntax#600:#701 Hullcube edit #600<#601Index Num#600> <#601Mass#601|#600Resilience#601|#600Keel#601|#600Size#601|#600Type#600> <#601Value#600>#700\n\r");
			return true;

		}

	}

	// Deleting a hull cube
	if (gsFunction == "delete")
	{
		// They entered an index
		if (gsValue1 == "")
		{
			Ch->Write("You must enter an Index to delete.\n\r");
			return true;
		}
		
		// Its a valid index
		if (atoi(gsValue1) > pLoader->m_HullCubes.size() || atoi(gsValue1) == 0)
		{
			Ch->Write("#100[#101Invalid Entry#100]#701 %d #700 is not a valid HullCube Index\n\r", atoi(gsValue1));
			Ch->Write("Valid Indexes#600:#701 1 - %d#700\n\r", pLoader->m_HullCubes.size());
			return true;
		}
		else
		{
			CHull* pHull = pLoader->m_HullCubes.at(atoi(gsValue1) -1);

			pLoader->m_HullCubes.erase(pLoader->m_HullCubes.begin()+atoi(gsValue1)-1);

			// Delete object

			gString szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Ships\\HullCubes\\" + pHull->m_gsFileName;
			unlink(szFile);

			pLoader->Save();

			delete pHull;
			pHull = NULL;

			Ch->Write("#601HullCube deleted#700.\n\r");
			return true;
		}
	}

	// Showing a module
	if (gsFunction == "show" || gsFunction == "view")
	{
		// Make sure its a valid Index
		if (atoi(gsValue1) > pLoader->m_HullCubes.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("#100[#101Invalid Entry#100]#701 %d #700 is not a valid HullCube Index\n\r", atoi(gsValue1));
			Ch->Write("Valid Indexes#600:#701 1 - %d#700\n\r", pLoader->m_HullCubes.size());
			return true;
		}
		else
		{
			CHull* pHull = pLoader->m_HullCubes.at(atoi(gsValue1)-1);
			Ch->Write("#600:#601:#700 %s #600[#601%s#600] #601:#600:#700\n\r", pHull->m_gsName, CHull::szHullTypes[pHull->m_nType]);
			Ch->Write("Size#600:#701 %d \t\t#700 Mass#600:#701 %d#700\n\r", pHull->m_nSize, pHull->m_nMass);
			Ch->Write("Resilience#600:#701 %d \t\t#700 Keel#600:#701 %d#700\n\r", pHull->m_nResilience, pHull->m_nMKeel); 
			return true;
		}
	}




	return true;
}


// Method     :: CmdFreeze
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Player Name>
// Return     :: Bool
// Function   :: Allows a Staff member to freeze a player
// Written    :: 29/07/06 {OWV}

bool CmdFreeze::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	bool bFreeze = false;

	for (ActorMap::iterator pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
	{
		try 
		{
			if ((*pos).second == Ch)
				continue;

			if ( ((*pos).second->Name().HasPrefix(gsValue)) || (gsValue == "all") )
			{
				if ((*pos).second->ActorStates()->IsSet(CActor::_FROZEN))
				{
					(*pos).second->Write("Movement slowly returns to your body as you thaw out.\n\r");
					(*pos).second->ActorStates()->RemoveBit(CActor::_FROZEN);

					if (gsValue != "all")
						Ch->Write("%s is now unfrozen!\n\r", (*pos).second->Name());
				}
				else
				{
					(*pos).second->Write("STOP! Hammer time..... Looks like you've been frozen solid!\n\r");
					(*pos).second->ActorStates()->SetBit(CActor::_FROZEN);
					bFreeze = true;

					if (gsValue != "all")
						Ch->Write("%s is now frozen!\n\r", (*pos).second->Name());
				}
			}
			else
			{
				continue;
			}

		}
		catch (...) {break;}
	}

	if (gsValue == "all")
	{
		if (bFreeze)
		{
			Ch->ExecuteCommand("OOC", "IM ON A POWER TRIP!");
			Ch->Write("All players frozen.\n\r");
		}
		else
		{
			Ch->ExecuteCommand("OOC", "POWER TRIP OVER!");
			Ch->Write("All players have been thawed out!\n\r");
		}

	}
	return true;

}

/*
	// [1] - Check if they are targetting a player
	if (gsValue == "all")
	{
		for (ActorMap::iterator pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
		{
			// Don't freeze yourself!
			if ((*pos).second == Ch)
				continue;

			if ((*pos).second->ActorFlags()->IsSet(CActor::_FROZEN))
			{
				(*pos).second->Write("Movement slowly returns to your body as you thaw out.\n\r");
				(*pos).second->ActorFlags()->RemoveBit(CActor::_FROZEN);
			}
			else
			{
				(*pos).second->Write("STOP! Hammer time..... Looks like you've been frozen solid!\n\r");
				(*pos).second->ActorFlags()->SetBit(CActor::_FROZEN);
			}

		}
	}
	else
	{*/