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

// File     :: CmdsSpace.cpp
// Header   :: CmdsSpace.h
// Function :: Holds the implementations for commands that belong in the Space category

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "CmdsSpace.h"
#include "CmdsPlayer.h"
#include "Space.h"
#include "Player.h"
#include "GameWorld.h"
#include "Room.h"
#include "../gTools/Log.h"
#include <Math.h>


IMPLEMENT_CLASS(CmdWarp);		// Allows staff to enter space without a ship
IMPLEMENT_CLASS(CmdSystems);	// Displays currently loaded systems
IMPLEMENT_CLASS(CmdPower);		// Handles power configuration
IMPLEMENT_CLASS(CmdSpeed);		// Handles speed changes
IMPLEMENT_CLASS(CmdHyper);		// Enters hyperspace
IMPLEMENT_CLASS(CmdManeuver);	// Allows a ship to maneuver
IMPLEMENT_CLASS(CmdRadar);		// Displays the contacts on a ship's radar
IMPLEMENT_CLASS(CmdStatus);		// Display a status/damage report for the ship
IMPLEMENT_CLASS(CmdSweep);		// Sweeps the system using radomes to update the radar
IMPLEMENT_CLASS(CmdComm);		// Handles the comm system
IMPLEMENT_CLASS(CmdShield);		// Displays a GUI of the shield status
IMPLEMENT_CLASS(CmdLaunch);		// Launches the ship into space
IMPLEMENT_CLASS(CmdScm);		// Displays and modifies the SCM state
IMPLEMENT_CLASS(CmdRoll);		// Initiates a roll of the ship
IMPLEMENT_CLASS(CmdLand);		// Lands a ship
IMPLEMENT_CLASS(CmdPilot);		// Allows a player to begin piloting a starfighter
IMPLEMENT_CLASS(CmdMan);		// Allows a player to man a console
IMPLEMENT_CLASS(CmdFire);		// Opens fire with weapons
IMPLEMENT_CLASS(CmdFcs);		// Displays Weapon Grouping
IMPLEMENT_CLASS(CmdTarget);		// Targets a Spatial Object

// Method     :: CmdWarp
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <sector>
// Return     :: Bool
// Function   :: Used for temporarily entering a sector
// Written    :: 13/6/05 {OWV}

bool CmdWarp::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsSector = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CSector* pSector;

	CPlayer* pPlayer = (CPlayer*)(Ch);

	if ( (pSector = pGalaxy->GetSec(gsSector)) == NULL)
	{
		if (pPlayer->m_Sector != "")
		{
			Ch->Write("You plant your feet firmly back on the ground!\n\r");
			pPlayer->m_Sector = "";
			return true;
		}

		
		Ch->Write("That Sector does not exist!\n\r");
		Ch->Write("Syntax: Warp <Sector to Enter>\n\r");
		return true;
	}
	else
	{
		pPlayer->m_Sector = pSector->m_gsName;
		Ch->Write("You click your heels together twice and leap into the sky!\n\r");
		return true;
	}

	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////
// Sublight Commands
///////////////////////////////////////////////////////////////////////////////////////////

// Method     :: CmdPower
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Subsystem>
//			  :: <Module Id>
//			  :: <Module Id> <passive/active>
// Return     :: Bool
// Function   :: Either powers up a shutdown system or shuts down a powered up one
// Written    :: 21/6/05 {OWV}

bool CmdPower::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	// Get the system
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Systemname
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsSensorMode = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Active/Passive setting

	int	nDrives[3];		nDrives[0] = 0;		nDrives[1] = 0;		nDrives[2] = 0;
	int	nShields[3];	nShields[0] = 0;	nShields[1] = 0;	nShields[2] = 0;
	int	nCapacitors[3]; nCapacitors[0] = 0; nCapacitors[1] = 0; nCapacitors[2] = 0;
	int	nSystems[3];	nSystems[0] = 0;	nSystems[1] = 0;	nSystems[2] = 0;
	int	nRepulsors[3];	nRepulsors[0] = 0;	nRepulsors[1] = 0;	nRepulsors[2] = 0;
	int	nGravwells[3];	nGravwells[0] = 0;	nGravwells[1] = 0;	nGravwells[2] = 0;
	int	nEcm[3];		nEcm[0] = 0;		nEcm[1] = 0;		nEcm[2] = 0;
	int	nWeapons[3];	nWeapons[0] = 0;	nWeapons[1] = 0;	nWeapons[2] = 0;
	int	nBattery[2];	nBattery[0] = 0;	nBattery[1] = 0;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to view its Power Configuration.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		g_Log.Log(LOG_ERROR, "[CmdPower::>>]  Ship has invalid area.\n\r");
		return false;
	}

	// Now the fun stuff, counting the number of each modules we have powered up
	// Using arrays the first digit will be the offline number, the second will be
	// the online one, the third is its energy load
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
	{
		CModule* pMod = ((*map).second);
		
		// Work out the Number of Online and Offline
		if (pShip->IsType(CShip::PT_DRIVE, pMod))
		{
			nDrives[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nDrives[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_SHIELD, pMod))
		{
			nShields[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nShields[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_CAPACITOR, pMod))
		{
			nCapacitors[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nCapacitors[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_SYSTEMS, pMod))
		{
			nSystems[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nSystems[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_REPULSOR, pMod))
		{
			nRepulsors[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nRepulsors[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_GRAVWELL, pMod))
		{
			nGravwells[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nGravwells[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_ECM, pMod))
		{
			nEcm[pMod->Plus("Powered", false)]++;
			if (pMod->Powered())
				nEcm[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_WEAPON, pMod))
		{
			nWeapons[pMod->Plus("Powered", false)]++;

			if (pMod->Powered())
				nWeapons[2] += pMod->Plus("PowerLoad", false);
		}
		else if (pShip->IsType(CShip::PT_BATTERY, pMod))
		{
			nBattery[pMod->Plus("Powered", false)]++;
		}

		pMod = NULL;
	}


	/* //////////////////////[    Ship Name     ]\\\\\\\\\\\\\\\\\\\\\\
	   // Energy Load: Minimum/Idle/Nominal/Combat/Max @ Energy Load \\
	   ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	   // DRIVES     >>   Online [#]   Offline [#]  Energy Load      \\
	   // SHIELDS    >>   Online [#]   Offline [#]  Energy Load      \\
	   // CAPACITORS >>   Online [#]   Offline [#]  Energy Load      \\ Line removed
	   // SYSTEMS    >>   Online [#]   Offline [#]  Energy Load      \\ 
	   // REPULSORS  >>   Online [#]   Offline [#]  Energy Load      \\
	   // GRAVWELLS  >>   Online [#]   Offline [#]  Energy Load      \\
	   // ECM        >>   Online [#]   Offline [#]  Energy Load      \\
	   // WEAPONS    >>   Online [#]   Offline [#]  Energy Load      \\
	   //////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	   // Batteries: [#] [Curr/Max]           IDLE/CHARGING/DRAINING \\
	   //////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ */	   

	// [1] - Display the current Power settings for the ship
	if (gsValue == "")
	{
		Ch->Write("\n\r");
		Ch->Write("#400////////////////////////#401[#700 %-17s#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r", pShip->m_gsName);
		// Work out the Energy Load
		gString gsLevel;
		int nLoad = pShip->Energy(CShip::ET_CURRENT);
		if (nLoad >= pShip->Energy(CShip::ET_MAX))
			gsLevel = "Max";
		else if (nLoad < pShip->Energy(CShip::ET_MAX) && nLoad >= pShip->Energy(CShip::ET_COMBAT))
			gsLevel = "Combat";
		else if (nLoad < pShip->Energy(CShip::ET_COMBAT) && nLoad >= pShip->Energy(CShip::ET_NORMAL))
			gsLevel = "Nominal";
		else if (nLoad < pShip->Energy(CShip::ET_NORMAL) && nLoad >= pShip->Energy(CShip::ET_IDLE))
			gsLevel = "Idle";
		else
			gsLevel = "Minimum";

		Ch->Write("#400//#700 Energy Level#401:#700 %-23s      Energy Load#401:#700 %6d #400\\\\#700\n\r", gsLevel, nLoad);
		Ch->Write("#400//#700                                          Coolant Level#401:#700 %6d #400\\\\#700\n\r", pShip->m_nCoolant);
		Ch->Write("#400//////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");

		// Drives
		Ch->Write("#400//#700 DRIVES     #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nDrives[1], nDrives[0], nDrives[2]);
		Ch->Write("#400//#700 SHIELDS    #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nShields[1], nShields[0], nShields[2]);
		//Ch->Write("#400//#700 CAPACITORS #400>#401>   #201Charge #401[#700 %2d #401]   #301Idle    #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nCapacitors[1], nCapacitors[0], nCapacitors[2]);
		Ch->Write("#400//#700 SYSTEMS    #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nSystems[1], nSystems[0], nSystems[2]);
		Ch->Write("#400//#700 REPULSORS  #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nRepulsors[1], nRepulsors[0], nRepulsors[2]);
		Ch->Write("#400//#700 GRAVWELLS  #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nGravwells[1], nGravwells[0], nGravwells[2]);
		Ch->Write("#400//#700 ECM        #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nEcm[1], nEcm[0], nEcm[2]);
		Ch->Write("#400//#700 WEAPONS    #400>#401>   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]#700  Eng Load %5d #400\\\\\n\r", nWeapons[1], nWeapons[0], nWeapons[2]);
		Ch->Write("#400////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		Ch->Write("#400//#700 BATTERIES  #400>#401>#700   #201Online #401[#700 %2d #401]   #101Offline #401[#700 %2d #401]                 #400\\\\#700\n\r", nBattery[1], nBattery[0]);
		gString gsBatt = "Idle";
		if (pShip->MaxPower() <= 0)
			gsBatt = "Draining";
		else if (pShip->Battery(true) < pShip->Battery(false))
			gsBatt = "Charging";

		Ch->Write("#400//            #400>#401>#700   Total Charge#401: [#700 %6d#401/#700 %6d#401]     {%-8s} #400\\\\#700\n\r", pShip->Battery(true), pShip->Battery(false), gsBatt);
		Ch->Write("#400////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		return true;
		// End displaying Power status
	}

	// Powering up by System groups
	if (gsValue == "Drives" || gsValue == "Shields" || gsValue == "Capacitors" || gsValue == "Systems" ||
		gsValue == "Repulsors" || gsValue == "Gravwells" || gsValue == "Ecm" || gsValue == "Weapons")
	{
		int nPower = 0;

		if (gsValue == "Drives")
			nPower = CShip::PT_DRIVE;
		else if (gsValue == "Shields")
			nPower = CShip::PT_SHIELD;
		else if (gsValue == "Capacitors")
			nPower = CShip::PT_CAPACITOR;
		else if (gsValue == "Systems")
			nPower = CShip::PT_SYSTEMS;
		else if (gsValue == "Repulsors")
			nPower = CShip::PT_REPULSOR;
		else if (gsValue == "Gravwells")
			nPower = CShip::PT_GRAVWELL;
		else if (gsValue == "Ecm")
			nPower = CShip::PT_ECM;
		else if (gsValue == "Weapons")
			nPower = CShip::PT_WEAPON;

		// Need a check here to see if there are any systems to power up

		switch (nPower)
		{
			case CShip::PT_DRIVE:
				if (nDrives[0] == 0 && nDrives[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Drives to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Drives to powerup.");


					return true;
				}
				break;

			case CShip::PT_SHIELD:
				if (nShields[0] == 0 && nShields[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Shield Emitters to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Shield Emitters to powerup.");
					return true;
				}
				break;

			case CShip::PT_CAPACITOR:
				if (nCapacitors[0] == 0 && nCapacitors[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Shield Capacitors to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Shield Capacitors to powerup.");
					return true;
				}
				break;

			case CShip::PT_SYSTEMS:
				if (nSystems[0] == 0 && nSystems[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Systems to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Systems to powerup.");

					return true;
				}
				break;

			case CShip::PT_REPULSOR:
				if (nRepulsors[0] == 0 && nRepulsors[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Repulsor Coils to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Repulsor Coils to powerup.");
					
					return true;
				}
				break;

			case CShip::PT_GRAVWELL:
				if (nGravwells[0] == 0 && nGravwells[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Gravity Well Projectors to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Gravity Well Projectors to powerup.");

					return true;
				}
				break;

			case CShip::PT_ECM:
				if (nEcm[0] == 0 && nEcm[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Electronic Countermeasures to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Electronic Countermeasures to powerup.");

					return true;
				}
				break;

			case CShip::PT_WEAPON:
				if (nWeapons[0] == 0 && nWeapons[1] == 0)
				{
					if (!Ch->IsNPC())
						Ch->Write("This ship has no Weapons to powerup.\n\r");
					else
						Ch->Report(pShip->m_Commander, "Commander, This ship has no Weapons to powerup.");

					return true;
				}
				break;
		}

		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
		{
			CModule* pMod = ((*map).second);

			// Special Case for the Batteries
			if (nPower == CShip::PT_SYSTEMS && pMod->m_nType == CModule::MT_BATTERY_BANK)
			{

			}
			else
			{
				if (!pShip->IsType(nPower, pMod))
					continue;
			}

			// First check if its powered and that we want to shut it down
			// m_nPower contains a list of the state of the Ship's power systems
			// a value of 0 indicates the systems should be shut down and 1 indicates
			// that the systems are powered up

			// We only want to power it down if our PowerState is powered up,
			// this prevents any systems we have shutdown independantly being
			// powered back up
			if (pMod->Powered() && pShip->m_nPower[nPower] == 1)
			{
				// Disable Repulsor lift effect
				if (pMod->m_nType == CModule::MT_REPULSOR_COILS)
				{
					if (pShip->m_ShipState->IsSet(CShip::_REPULSOR))
					{
						if (pShip->m_ShipState->IsSet(CShip::_FLYING))
						{
							pShip->m_ShipState->RemoveBit(CShip::_REPULSOR);
						}
						else
						{
							pShip->m_ShipState->SetBit(CShip::_LANDED);
							pShip->Write(CShip::MT_CRITICAL, "The Ship slowly settles down on the ground\n\r");

							// Let the room know
							CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());

							if (pArea)
							{
								CRoom * pRoom = pArea->GetRoom(pShip->m_Land.Room());
								if (pRoom)
									pRoom->Write("%s:%s slowly settles down on the ground.\n\r", pShip->m_gsType, pShip->m_gsName);

								pShip->m_ShipState->RemoveBit(CShip::_REPULSOR);
							}
						}
					}
				}

				
				pMod->SetPlus("Powered", 0);
				// Set its PowerLoad to 0
				pMod->SetPlus("PowerLoad", 0);
				//Ch->Write("[%s] %s powered down.\n\r", (*comp)->szClassnames[(*comp)->m_Type], pMod->m_gsName);
				continue;
			}
			// Again we only power up if our PowerState tells us to;
			else if (pShip->m_nPower[nPower] == 0)
			{	
				// You cant power up a destroyed Module
				if (pMod->m_ncDurability == -1)
				{
					if (!Ch->IsNPC())
						Ch->Write("You cannot power up %s as it has been destroyed.\n\r", ((*map).second)->m_gsName);						
					else
						Ch->Report(pShip->m_Commander, "Commander, %s is destroyed we are unable to Power it up.", ((*map).second)->m_gsName);					
				}
				else
				{
					pMod->SetPlus("Power", 1);	// Power it up
				}
			}
		}

		if (!Ch->IsNPC())
			Ch->Write("Powering %s ship's %s\n\r", pShip->m_nPower[nPower] == 1 ? "down" : "up", gsValue);
		else
			Ch->Report(pShip->m_Commander, "Powering %s ship's %s, Commander!", pShip->m_nPower[nPower] == 1 ? "down" : "up", gsValue);

		if (pShip->m_nPower[nPower] == 1)
			pShip->m_nPower[nPower] = 0;
		else
			pShip->m_nPower[nPower] = 1;
	
		return true;
	}

	// Power up a Specific Module
	// Their value will be the index for that Module from the Systems list
	// so we need to iterate through all the modules again and find the system
	int nIndex = 1;
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
			{
				IntegerMap::iterator it = ((*map).second)->m_Plus.find("Power");
				IntegerMap::iterator im = ((*map).second)->m_Plus.find("Powered");

				if (nIndex != atoi(gsValue))
				{
					nIndex++;
					continue;
				}

				// We can power radomes active or passive
				if (((*map).second)->m_nType == CModule::MT_RADOME && gsSensorMode != "")
				{
					if (((*map).second)->Plus("Active", false))
					{
						Ch->Write("%s switched to #101passive#700 mode.\n\r", ((*map).second)->m_gsName);
						((*map).second)->SetPlus("Active", 0);
					}
					else
					{
						Ch->Write("%s switched to #201active#700 mode.\n\r", ((*map).second)->m_gsName);
						((*map).second)->SetPlus("Active", 1);
					}
					return true;
				}

				// First check if its powered
				if (im != ((*map).second)->m_Plus.end())
				{
					// Equal to 1 means its powered up
					if ((*im).second == 1)
					{
						// Disable Repulsor lift effect
						if (((*map).second)->m_nType == CModule::MT_REPULSOR_COILS)
						{
							if (pShip->m_ShipState->IsSet(CShip::_FLYING))
							{
								pShip->m_ShipState->RemoveBit(CShip::_REPULSOR);
							}
							else
							{
								pShip->m_ShipState->SetBit(CShip::_LANDED);
								pShip->Write(CShip::MT_CRITICAL, "The Ship slowly settles down on the ground\n\r");

								// Let the room know
								CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());

								if (pArea)
								{
									CRoom * pRoom = pArea->GetRoom(pShip->m_Land.Room());
									if (pRoom)
										pRoom->Write("%s:%s slowly settles down on the ground.\n\r", pShip->m_gsType, pShip->m_gsName);

									pShip->m_ShipState->RemoveBit(CShip::_REPULSOR);
								}
							}
						}

						// Power it down
						(*im).second = 0;

						// Set its PowerLoad to 0
						((*map).second)->SetPlus("PowerLoad", 0);
						
						if (!Ch->IsNPC())
							Ch->Write("#100[#101%s#100]#700 %s powered #101down#700.\n\r", (*comp)->szClassnames[(*comp)->m_Type], ((*map).second)->m_gsName);
						else
							Ch->Report(pShip->m_Commander, "Powered down %s's %s, Commander", (*comp)->szClassnames[(*comp)->m_Type], ((*map).second)->m_gsName);
						return true;
					}
				}
				
				if (it != ((*map).second)->m_Plus.end())
				{				
					// You cant power up a destroyed Module
					if (((*map).second)->m_ncDurability == -1)
					{
						if (!Ch->IsNPC())
							Ch->Write("You cannot power up %s as it has been destroyed.\n\r", ((*map).second)->m_gsName);						
						else
							Ch->Report(pShip->m_Commander, "Commander, %s is destroyed we are unable to Power it up.");
					}
					else
					{
						(*it).second = 1;	// Power it up

						if (!Ch->IsNPC())
							Ch->Write("Powering up %s\n\r", ((*map).second)->m_gsName); 
						else
							Ch->Report(pShip->m_Commander, "Powering up %s, Commander!", ((*map).second)->m_gsName); 
						return true;
					}
				}
			}
		}
	}


	Ch->Write("Usage#400:#701 Power #400<#401SubSystem #400|#401 Specific Module#400>#700 Powers up or down a System\n\r#701Power #400<#401None#400>#700 Shows current Power readout.\n\r");
	return true;

}

// Method     :: CmdSystems
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: Systems, Drives, Weapons, Gravwells, Ecm
// Return     :: Bool
// Function   :: Shows information on the selected group of systems
// Written    :: 21/6/05 {OWV}

bool CmdSystems::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsSystem = (gString)(*CommandLine.begin());
	gsSystem.MakeUpper();

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to view its Power Configuration.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		g_Log.Log(LOG_ERROR, "[CmdSystems::>>]  Ship has invalid area.\n\r");
		return false;
	}

	int nCount = 1;

	// We want a list of ammunition for displaying WEAPONS
	int nAmmo[CShip::OR_MAX];

	for (int i = 0; i < CShip::OR_MAX; i++)
		nAmmo[i] = 0;

	// We need to check for prefixes now in order to determine
	// the full name of the system they are looking for
	gString gsSys;
	gsSys = "DRIVES";
	if (gsSys.HasPrefix(gsSystem))
		gsSystem = "DRIVES";
	gsSys = "SYSTEMS";
	if (gsSys.HasPrefix(gsSystem))
		gsSystem = "SYSTEMS";
	gsSys = "REPULSORS";
	if (gsSys.HasPrefix(gsSystem))
		gsSystem = "REPULSORS";
	gsSys = "GRAVWELLS";
	if (gsSys.HasPrefix(gsSystem))
		gsSystem = "GRAVWELLS";
	gsSys = "WEAPONS";
	if (gsSys.HasPrefix(gsSystem))
		gsSystem = "WEAPONS";
	gsSys = "CAPACITORS";
	if (gsSys.HasPrefix(gsSystem))
	{
		Ch->ExecuteCommand("Shield", "");
		return true;
	}
		
	
	Ch->Write("#400/////////////////////////////#401[#700 %-17s#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r", gsSystem);
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		// Check now if we display the Components Name
		// When we display a components name we also display the power status of the component
		// this is the amount of the powerlink's capacity that is currently being used.

		// DRIVES
		if (((*comp)->m_Type == CComponent::CT_SUBLIGHT || (*comp)->m_Type == CComponent::CT_HYPERDRIVE) && gsSystem == "DRIVES")
			Ch->Write("#400//#701 %-72s #400\\\\#700\n\r", (*comp)->m_gsName);

		// SYSTEMS
		if (((*comp)->m_Type == CComponent::CT_ENGINEERING ||
			(*comp)->m_Type == CComponent::CT_EXTERNAL ||
			(*comp)->m_Type == CComponent::CT_INTERNAL ||
			(*comp)->m_Type == CComponent::CT_CONTROLPOINT) && gsSystem == "SYSTEMS")
		{
			Ch->Write("#400//#701 %-59s [", (*comp)->m_gsName);
			int nPowerUsage = (((float)(*comp)->MaxPower()) - ((float)(*comp)->FreePower()));

			for (int i = 0; i < 10; i++)
			{
				if (nPowerUsage > 0) 
					Ch->Write("%s>", CGameWorld::nProgress[i]);
				else
					Ch->Write(" ");

				nPowerUsage--;
			}			
			Ch->Write("#701] #400\\\\#700\n\r");
		}

		
		// REPULSORS
		if ((*comp)->m_Type == CComponent::CT_INTERNAL && gsSystem == "REPULSORS")
			Ch->Write("#400//#701 %-72s #400\\\\#700\n\r", (*comp)->m_gsName);

		// GRAVWELLS
		if ((*comp)->m_Type == CComponent::CT_EXTERNAL && gsSystem == "GRAVWELLS")
			Ch->Write("#400//#701 %-72s #400\\\\#700\n\r", (*comp)->m_gsName);

		// ECM
		if ((*comp)->m_Type == CComponent::CT_EXTERNAL && gsSystem == "ECM")
			Ch->Write("#400//#701 %-72s #400\\\\#700\n\r", (*comp)->m_gsName);

		// WEAPONS
		if (gsSystem == "WEAPONS")
		{			
			if ((*comp)->m_Type == CComponent::CT_MAGAZINE)
			{				
				CModule* pMod;

				pMod = ((CMagazine*)(*comp))->Get(CModule::MT_MISSILES);
				if (pMod)
					nAmmo[CShip::OR_MISSILE] += pMod->Plus("Quantity", false);
				pMod = ((CMagazine*)(*comp))->Get(CModule::MT_TORPEDOES);
				if (pMod)
					nAmmo[CShip::OR_TORPEDO] += pMod->Plus("Quantity", false);
				pMod = ((CMagazine*)(*comp))->Get(CModule::MT_ROCKETS);
				if (pMod)
					nAmmo[CShip::OR_ROCKET] += pMod->Plus("Quantity", false);
				pMod = ((CMagazine*)(*comp))->Get(CModule::MT_FLARES);
				if (pMod)
					nAmmo[CShip::OR_CHAFF] += pMod->Plus("Quantity", false);
				pMod = ((CMagazine*)(*comp))->Get(CModule::MT_CHAFF);
				if (pMod)
					nAmmo[CShip::OR_FLARE] += pMod->Plus("Quantity", false);

			}
		}
		
		for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
		{
			CModule* pMod = ((*map).second);

			float fIntegrity = ((float)pMod->m_ncDurability / (float)pMod->m_nmDurability) * 100.0f;
			gString gsIntegrity;

			if (fIntegrity > 90.0)
				gsIntegrity.Format("#201 %3.0f#700", fIntegrity);
			if (fIntegrity <= 90.0 && fIntegrity > 75.0)
				gsIntegrity.Format("#200 %3.0f#700", fIntegrity);
			if (fIntegrity <= 75.0 && fIntegrity > 50.0)
				gsIntegrity.Format("#301 %3.0f#700", fIntegrity);
			if (fIntegrity <= 50.0 && fIntegrity > 25.0)
				gsIntegrity.Format("#101 %3.0f#700", fIntegrity);
			else if (fIntegrity <= 25.0)
				gsIntegrity.Format("#100 %3.0f#700", fIntegrity);

			if (pShip->IsType(CShip::PT_DRIVE, pMod) && gsSystem == "DRIVES")
			{
				Ch->Write("#400// [#401 %3d#400] #700%-50s %8s %s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->Powered() ? "#200<#201Online#200> " : "#100<#101Offline#100>", gsIntegrity);
			}
			else if (pShip->IsType(CShip::PT_SYSTEMS, pMod) && gsSystem == "SYSTEMS")
			{
				Ch->Write("#400// [#401 %3d#400] #700%-50s %8s %s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->Powered() ? "#200<#201Online#200> " : "#100<#101Offline#100>", gsIntegrity);
			}
			else if (pShip->IsType(CShip::PT_REPULSOR, pMod) && gsSystem == "REPULSORS")
			{
				Ch->Write("#400// [#401 %3d#400] #700%-50s %8s %s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->Powered() ? "#200<#201Online#200> " : "#100<#101Offline#100>", gsIntegrity);
			}
			else if (pShip->IsType(CShip::PT_GRAVWELL, pMod) && gsSystem == "GRAVWELLS")
			{
				Ch->Write("#400// [#401 %3d#400] #700%-50s %8s %s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->Powered() ? "#200<#201Online#200> " : "#100<#101Offline#100>", gsIntegrity);
			}
			else if (pShip->IsType(CShip::PT_ECM, pMod) && gsSystem == "ECM")
			{
				Ch->Write("#400// [#401 %3d#400] #700%-50s %8s %s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->Powered() ? "#200<#201Online#200> " : "#100<#101Offline#100>", gsIntegrity);
			}
			else if (pShip->IsType(CShip::PT_WEAPON, pMod) && gsSystem == "WEAPONS")
			{ 
				gString gsArcs = "#400[#701";
				CWeapon* pComp = (CWeapon*)(*comp);
				bool bNone = true;

				for (int i = 0; i < CShip::A_MAX; i++)
				{
					if (pComp->m_Orientation->IsSet(i))
					{
						bNone = false;
						gString gsArc = CShip::szArc[i];
						gsArc.TrimRight(1);	// Only want the first letter of the arc
						gsArcs += (gsArc + " ");
					}
						
				}

				if (bNone)
					gsArcs += "No Arc";

				gsArcs += "#400]";

				Ch->Write("#400// [#401 %3d#400] #700%-40s %-26s %s%s #400\\\\#700\n\r", nCount, pMod->m_gsName, gsArcs, pMod->Powered() ? "#200<#201On#200> " : "#100<#101Off#100>", gsIntegrity);
			}


			nCount++;
			pMod = NULL;

		}
	}
	Ch->Write("#400///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");

	// Show the ammunition levels
	if (gsSystem == "WEAPONS")
	{
		Ch->Write("#400// #700AMMUNITION STORES#400:                                                       \\\\#700\n\r");
		for (int i = 0; i < (CShip::OR_MAX - 1); i+=2)
			Ch->Write("#400// >#401>#700 %-15s #400[#701 %8d#400]  >#401>#700 %-15s #400[#701 %8d#400]           \\\\#700\n\r", CShip::szOrdinance[i], nAmmo[i], CShip::szOrdinance[i+1], nAmmo[i+1]);


		Ch->Write("#400///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	}

	return true;
}
// Method     :: CmdSpeed
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <speed>
// Return     :: Bool
// Function   :: Slows down/Speeds up the Ship
// Written    :: 21/6/05 {OWV}

bool CmdSpeed::Perform(CActor* Ch, gStringList& CommandLine)
{

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can pilot the ship
	if (!Ch->CanPilot())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to control this ship!\n\r");
			Ch->Write("Locate a piloting console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	if (pShip->TopSpeed(false) <= 0)
	{
		Ch->Write("Your vessel has no engines installed in it.\n\r");
		return true;
	}

	// Get the speed
	gString gsSpeed = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Speed

	// Validate for negative values
	int nSpeed = atoi(gsSpeed);
	
	if (nSpeed < 0)
	{
		Ch->Write("Deceleration is acheived by setting a smaller positive speed.\n\r");
		return true;
	}

	if (pShip->TopSpeed(true) <= 0)
	{
		Ch->Write("You must power up some Ion Engines first!\n\r");
		return true;
	}

	if (!pShip->m_ShipState->IsSet(CShip::_LAUNCHING) && !pShip->m_ShipState->IsSet(CShip::_FLYING))
	{
		Ch->Write("You must first launch the Ship before you can engage the Sublight Engines.\n\r");
		return true;
	}

	if (nSpeed > pShip->m_Speed)
	{
		Ch->Write("Increasing Speed.\n\r");
		pShip->NotifySpace("%s sublight engines brighten as it begins to increase its speed.\n\r", pShip->m_gsName);
		pShip->m_dSpeed = nSpeed;
		return true;
	}
	else if (nSpeed < pShip->m_Speed)
	{
		Ch->Write("Decreasing Speed.\n\r");
		pShip->NotifySpace("%s sublight engines darken as it begins to decrease its speed.\n\r", pShip->m_gsName);
		pShip->m_dSpeed = nSpeed;
		return true;
	}		
	else
	{
		Ch->Write("Maintaining Speed.\n\r");
		return true;
	}

	return true;

}

// Method     :: CmdHyper
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <speed|on|off>
// Return     :: Bool
// Function   :: Puts the Ship into Hyperspace/Removes it and sets the hyperspeed
// Written    :: 21/6/05 {OWV}

bool CmdHyper::Perform(CActor* Ch, gStringList& CommandLine)
{
	/*
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();

	// Check here they are in a ship
	// Temp for testing we will use the 'Test' Ship
	CShip* pShip = pGalaxy->GetShi("Test Ship");

	// Get the argument
	gString gsSpeed = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Speed

	// Condition 1: In Bridge/Cockpit

	// Condition 2: Has Piloting Access

	// Condition 3: No Gravity Shadow

	// Condition 4: Not in Tractor Beam

	// Condition 5: Course plotted

	CHyper* pHyper = (CHyper*)pShip->Fetch(CComponent::CT_HYPERDRIVE);

	if (!pHyper)
	{
		Ch->Write("This vessel is not equipped with a Hyperdrive.\n\r");
		return true;
	}

	if (gsSpeed == "On") // Turn on the Hyperdrive
	{
		if (pHyper->m_bEngaged)
		{
			Ch->Write("The Hyperdrive is already engaged!\n\r");
			return true;
		}
		else
		{
			// Reliability Check!
			int nChk = pRandom->NumberRange(1, 10);
			if (pHyper->m_nReliability < nChk)	// Failed!
			{
				pHyper->Damage(100); // Completely disable a component
				Ch->Write("You push the levers forward, Engaging the Hyperdrive.\n\r");
				Ch->Write("A Shrill Alarm sounds and numerous warning lights start to flash!\n\r");
				return true;
			}
			else
			{
				Ch->Write("You push the levers forward, Engaging the Hyperdrive.\n\r");
				Ch->Write("The stars disappear in a blur as you enter Hyperspace.\n\r");
				pHyper->m_bEngaged = true;
				pHyper->m_nSpeed = pHyper->m_nValue;
				pHyper->m_cHeat = (pHyper->m_Heat * pHyper->m_nSpeed);
				// Need to remove ship from Sector here, inform the sector
				return true;
			}
		}

	}
	else if (gsSpeed == "Off") // Turn off the Hyperdrive
	{
		if (pHyper->m_bEngaged)
			{
			// Reliability Check!
			int nChk = pRandom->NumberRange(1, 10);
			if (pHyper->m_nReliability < nChk)	// Failed!
			{
				pHyper->Damage(100); // Completely disable a component
				Ch->Write("You pull the levers backward, Disengaging the Hyperdrive.\n\r");
				Ch->Write("A Shrill Alarm sounds and numerous warning lights start to flash!\n\r");
				pHyper->m_nSpeed = 0;
				pHyper->m_bEngaged = false;
				return true;
			}
			else
			{
				Ch->Write("You pull the levers backward, Disengaging the Hyperdrive.\n\r");
				Ch->Write("The stars slow and reappear as you exit Hyperspace.\n\r");
				pHyper->m_bEngaged = false;
				pHyper->m_nSpeed = 0;
				// Need to remove ship from Sector here, inform the sector
				return true;
			}
		}
		else
		{
			Ch->Write("Your Hyperdrive is not engaged!\n\r");
			return true;
		}

	}
	else // Assuming they want to enter at a certain speed
	{
		if (atoi(gsSpeed) < 0)
		{
			Ch->Write("Such a move would defy the laws of both Space and Time.\n\r");
			return true;
		}

		if (pHyper->m_bEngaged)
		{
			Ch->Write("The Hyperdrive is already engaged!\n\r");
			return true;
		}
		else
		{
			// Reliability Check!
			int nChk = pRandom->NumberRange(1, 10);
			if (pHyper->m_nReliability < nChk)	// Failed!
			{
				pHyper->Damage(100); // Completely disable a component
				Ch->Write("You push the levers forward, Engaging the Hyperdrive.\n\r");
				Ch->Write("A Shrill Alarm sounds and numerous warning lights start to flash!\n\r");
				return true;
			}
			else
			{
				Ch->Write("You push the levers forward, Engaging the Hyperdrive.\n\r");
				Ch->Write("The stars disappear in a blur as you enter Hyperspace.\n\r");
				pHyper->m_bEngaged = true;
				pHyper->m_nSpeed = atoi(gsSpeed);
				pHyper->m_cHeat = (pHyper->m_Heat * pHyper->m_nSpeed);
				// Need to remove ship from Sector here, inform the sector
				return true;
			}
		}

	}
*/
	return true;

}

// Method     :: CmdManeuver
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <bearing> <mark>
// Return     :: Bool
// Function   :: Allows the player to change the heading of a ship
// Written    :: 04/7/05 {OWV}

bool CmdManeuver::Perform(CActor* Ch, gStringList& CommandLine)
{
    CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CModule* pMod = NULL;

	gString gsPhi = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Phi
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsTheta = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Theta
	if (!CommandLine.empty())
		CommandLine.pop_front();
	
	float fTheta = atof(gsTheta);
	float fPhi = atof(gsPhi);

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can pilot the ship
	if (!Ch->CanPilot())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to control this ship!\n\r");
			Ch->Write("Locate a piloting console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	if (pShip->Maneuver(false) <= 0)
	{
		Ch->Write("This vessel does not the ability to Maneuver.\n\r");
		return true;
	}

	if (pShip->Maneuver(true) <= 0)
	{
		Ch->Write("You must power up your Steering Engine and Maneuvering Thrusters first.\n\r");
		return true;
	}

	// Are they trying to roll while landed?
	if (pShip->m_ShipState->IsSet(CShip::_LANDED) || pShip->m_ShipState->IsSet(CShip::_REPULSOR))
	{
		Ch->Write("You cannot change your course while landed!\n\r");
		return true;
	}

	if (!pShip->m_ShipState->IsSet(CShip::_FLYING))
	{
		// #TODO
		Ch->Write("Atmospheric maneuvering is not implemented. Stick to Space.\n\r");
		return true;
	}

	// A negative number will cause a ship to move left and down
	// A positive number will cause a ship to move right and up

	if ((fTheta > 180 || fTheta < -180) || (fPhi > 180 || fPhi < -180))
	{
		Ch->Write("Valid Course changes are from -180 to +180\n\r");
		return true;
	}

	// We have set our Deltas now, we need to set the Destination Heading


	pShip->m_dHeading->z += fPhi;
	pShip->m_dHeading->y += fTheta;


	// Has this taken our heading over 360?
	if (pShip->m_dHeading->y >= 360)
		pShip->m_dHeading->y = abs(360 - pShip->m_dHeading->y);

	if (pShip->m_dHeading->z >= 360)
		pShip->m_dHeading->z = abs(360 - pShip->m_dHeading->z);

	// Has this taken our heading negative?
	if (pShip->m_dHeading->y < 0)
		pShip->m_dHeading->y = 360 - abs(pShip->m_dHeading->y);

	if (pShip->m_dHeading->z < 0)
		pShip->m_dHeading->z = 360 - abs(pShip->m_dHeading->z);


	// If a player is manning the Helm they will actually receive two messages in this instance
	Ch->Write("You manipulate the controls, turning towards a new Course.\n\r");
	pShip->Write(CShip::MT_HELM, "[Helm] Turning to new Course.\n\r");
	Ch->CurrentRoom()->Write(Ch, "%s manipulates the Helm, turning towards a new course.\n\r", Ch->Name());

	pShip->NotifySpace("%s begins to maneuver, turning towards a new course.\n\r", pShip->m_gsName);

	return true;

}

// Method     :: CmdRadar
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <range>
// Return     :: Bool
// Function   :: Allows a ship to use its radars to actively scan a system
// Written    :: 04/7/05 {OWV}

bool CmdRadar::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	ContactMap::iterator con;
	CModule* pMod = NULL;
	int nDistance;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanRadar())
	{
		// They can't access the radar, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the radar.\n\r");
			Ch->Write("Locate a ship console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	// Got the Ship's controls now display the information

	// The Radar command will list the data gathered from the Radar Contacts List
	Ch->CurrentRoom()->Write(Ch, "%s studies the Radar screen intently.\n\r", Ch->Name());

	Ch->Write("#400[#401RADAR#400]#700\n\r");

	if (pShip->m_Contacts.size() == 0)
	{
		Ch->Write(" #400>#401>#700 No Sensor Contacts registered.\n\r");	
	}
	else
	{
		std::vector<ContactMap::iterator>ContactDelete;

		for (con = pShip->m_Contacts.begin(); con != pShip->m_Contacts.end(); con++)
		{
			CSpatial *pSpatial = **(((*con).second)->m_Spatial);

			if (!pSpatial)
			{
				// Contact no longer exists, we add it to the delete list
				ContactDelete.push_back(con);
				continue;
			}

			CCart *pCart = ((*con).second)->m_Location;
			nDistance = pShip->Distance(pSpatial);
			gString gsArc = CShip::szArc[pShip->m_Location->Arc(pCart, pShip->m_Heading)];
			float fBear = pShip->m_Location->Bearing(pCart, pShip->m_Heading, CCart::_XZ);
			float fMark = pShip->m_Location->Bearing(pCart, pShip->m_Heading, CCart::_YZ);

			if (fBear > 180)
				fBear = -(360.0 - fBear);
			if (fMark > 180)
				fMark = -(360.0 - fMark);

			Ch->Write("#400[#401 %2d#400]#700 %-40s #400[#701 %5d#400]#701 Bearing#400:#700 %4.0f#701 Mark#400:#700 %4.0f#400 [%12s] #400@#700 %6.0d#400 nm#700 \n\r", (*con).first, pSpatial->m_gsName, ((*con).second)->m_Signature[CSpatial::SI_MASS], fBear, fMark, gsArc == "Fore" ? "#100Fore     #400" : (gsArc == "Aft" ? "#101Aft      #400" : (gsArc == "Port" ? "#200Port     #400" : (gsArc == "Starboard" ? "#201Starboard#400" : (gsArc == "Ventral" ? "#300Ventral  #400" : (gsArc == "Dorsal" ? "#301Dorsal   #400" : "None"))))) , nDistance);
			// DEBUG 
			CCart* pHead = new CCart();
			pHead->SetXYZ(fBear, fMark);
			//Ch->Write("Target location: %d %d %d\n\r", pSpatial->m_Location->x, pSpatial->m_Location->y, pSpatial->m_Location->z);
		}

		//Ch->Write("Our location: %d %d %d\n\r", pShip->m_Location->x, pShip->m_Location->y, pShip->m_Location->z);

		// Delete any Contacts in this List that we needed to remove
		for (std::vector<ContactMap::iterator>::iterator delc = ContactDelete.begin(); delc != ContactDelete.end(); delc++)
			pShip->m_Contacts.erase(*delc);
	}

	Ch->Write("\n\r");
	int nCount = 1;
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		if ((*comp)->m_Type == CComponent::CT_EXTERNAL)
		{
			for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
			{
				CModule* pMod = ((*map).second);

				if (pMod->m_nType == CModule::MT_RADOME)
					Ch->Write("#400[#401%2d#400]#701 %-25s #400[#401%-20s#400]#700 Range#400:#701 %6d %s\n\r", nCount, pMod->m_gsName, CShip::szSensors[pMod->Plus("Type", false)], pMod->Plus("Range", false), pMod->Plus("Active", false) ? "#200<#201Active#200>#700" : "#100<#101Passive#100>#700");

				nCount++;
			}
		}
		else
		{
			// We want to keep a counter that equals to a modules position in the list
			nCount += (*comp)->m_Modules.size();
		}



	}

	return true;
}

// Method     :: CmdStatus
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Views a ships Status report
// Written    :: 30/1/06 {OWV}

bool CmdStatus::Perform(CActor* Ch, gStringList& CommandLine)
{
	CModule* pMod;
	bool bDamaged = false;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the ship's systems
	if (!Ch->CanSystem())
	{
		// They can't access the systems, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the ship's status.\n\r");
			Ch->Write("Locate a systems console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	//////////////////////////[SHIP NAME GOES HERE]\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	//         [SUBLIGHT]        //                 [COURSE]                 \\
	//  DRIVES >> [ONLINE]       //      Bearing: 150       Mark: 140        \\ 
	//     Speed: 100/150 MGLTs  //               Roll: Upright              \\
	//                           //      Maneuver: Barrel-Roll [4/5]         \\
	///////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	//                          [STATUS REPORT]                              \\
	//  [1] Ventral Hull Cube >> Integrity [100%]                            \\
	//      Armour: [3500/3500]  Keel: [30000/30000]                         \\
	//                                                                       \\
	//  [1] Ventral Hull Cube >> Integrity [100%]                            \\
	//      Armour: [3500/3500]  Keel: [30000/30000]                         \\
	//                                                                       \\
	//  [1] Ventral Hull Cube >> Integrity [100%]                            \\
	//      Armour: [3500/3500]  Keel: [30000/30000]                         \\
	//                                                                       \\
	///////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ */

	Ch->Write("#400////////////////////////#401[#700 %-17s#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r", pShip->m_gsName);
	Ch->Write("#400//         [#401SUBLIGHT#400]       //               #400[#401COURSE#400]              \\\\#700\n\r");
	Ch->Write("#400//#700  DRIVES #400>#401> %8s     #400//   #700Bearing#400:#701 %3d        #700Mark#400:#701 %3d     #400\\\\#700\n\r", pShip->m_nPower[CShip::PT_DRIVE] == 1 ? "#200<#201Online#200> " : "#100<#101Offline#100>", pShip->m_Heading->z, pShip->m_Heading->y);
	Ch->Write("#400//#700     Speed#400:#701 %3d/%3d MGLTs #400//             #700Roll#400:#701 %-8s [%3d]    #400\\\\#700\n\r", pShip->m_Speed, pShip->TopSpeed(true), pShip->m_Heading->x == 0.0 ? "Upright" : (pShip->m_Heading->x == 90.0 ? "Left" : (pShip->m_Heading->x == 180.0 ? "Inverted" : (pShip->m_Heading->x == 270.0 ? "Right" : "Upright"))), pShip->m_Heading->x);
	Ch->Write("#400/////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\r");
	Ch->Write("#400//#700 DAMAGE REPORT                                                   #400\\\\\n\r");
	Ch->Write("#400//                                                                 #400\\\\\n\r");


	int nCount = 1;
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	{
		if (((*hull)->m_nCKeel != (*hull)->m_nMKeel) || (
			((*hull)->m_Armour && (*hull)->m_Armour->m_ncDurability != (*hull)->m_Armour->m_nmDurability)))
		{
			// If the HullCube is damaged we display the details
			float fIntegrity = ((float)(*hull)->m_nCKeel/(float)(*hull)->m_nMKeel)*100.0f;
			gString gsIntegrity;
			if (fIntegrity > 90.0)
				gsIntegrity.Format("#201 %3.0f#700", fIntegrity);
			if (fIntegrity <= 90.0 && fIntegrity > 75.0)
				gsIntegrity.Format("#200 %3.0f#700", fIntegrity);
			if (fIntegrity <= 75.0 && fIntegrity > 50.0)
				gsIntegrity.Format("#301 %3.0f#700", fIntegrity);
			if (fIntegrity <= 50.0 && fIntegrity > 25.0)
				gsIntegrity.Format("#101 %3.0f#700", fIntegrity);
			else if (fIntegrity <= 25.0)
				gsIntegrity.Format("#100 %3.0f#700", fIntegrity);

			Ch->Write("#400//  [#401 %3d#400]#700 %-25s >> Integrity#400: [%3s#400]          \\\\#700\n\r", nCount, (*hull)->m_gsName, gsIntegrity );
			Ch->Write("#400//      #700Keel#400: [#401 %8d#400/#401 %8d#400] ", (*hull)->m_nCKeel, (*hull)->m_nMKeel);
			if ((*hull)->m_Armour)
				Ch->Write("#700Armour#400: [#401 %6d#400/#401 %6d#400]#400      \\\\\n\r", (*hull)->m_Armour->m_ncDurability, (*hull)->m_Armour->m_nmDurability);
			else
				Ch->Write("                                           \\\\\n\r");

			for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
			for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
			{
			}

			bDamaged = true;
			nCount++;
		}

		
	}
	if (!bDamaged)
		Ch->Write("#400//#700                  #201[#700ALL SYSTEMS NOMINAL#201]                          #400\\\\\n\r");

	Ch->Write("#400/////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");

	Ch->CurrentRoom()->Write(Ch, "%s glances across to the Systems status readout.\n\r", Ch->Name());

	return true;
}

// Method     :: CmdSweep
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <heat|em|mass|ion>
// Return     :: Bool
// Function   :: Allows a ship to use its sensors to actively scan a system
// Written    :: 19/7/05 {OWV}

bool CmdSweep::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	ContactMap::iterator con;
	CModule* pMod = NULL;

	gString gsType = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Type

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanRadar())
	{
		// They can't access the radar, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the radar.\n\r");
			Ch->Write("Locate a ship console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	// Now we need to check to see what types of Sensors the ship has
	ModuleList* mlMods = pShip->Get(CModule::MT_RADOME);

	int nSensors[CShip::S_MAX];

	for (int i = 0; i < CShip::S_MAX; i++)
		nSensors[i] = 0;

	for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
	{
		if ((*mod)->m_ncDurability > 0)
			nSensors[((*mod)->Plus("Type", false))] = 1;
		else
			nSensors[((*mod)->Plus("Type", false))] = 2;
	}

	if (gsType == "")
		gsType = "all";

	bool bSensor = false;
	for (int i = 0; i < CShip::S_MAX; i++)
	{
		if (nSensors[i] == 1)
			bSensor = true;
	}

	if (!bSensor)
	{
		if (!Ch->IsNPC())
			Ch->Write("This vessel does not have any functioning Sensor Radomes.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, we do not have an functioning Sensor Radomes.");
		return true;
	}

	if (gsType != "Em" && gsType != "Heat"  && gsType != "Tactical" && gsType != "Ion" && gsType != "all" && gsType != "Mass")
	{
		if (!Ch->IsNPC())
			Ch->Write("That is not a valid Scanning System!\n\rSystems are: %s%s%s%s\n\r",
				nSensors[CShip::S_EM] == 1 ? "Em " : "", nSensors[CShip::S_HEAT] == 1 ? "Heat " : "",
				nSensors[CShip::S_ION] == 1 ? "Ion " : "", nSensors[CShip::S_MASS] == 1 ? "Mass " : "");
		else
			Ch->Report(pShip->m_Commander, "Commander, Thats not a valid Scanning System. This Vessel is equipped with %s%s%s%s Radomes.",
				nSensors[CShip::S_EM] == 1 ? "Em " : "", nSensors[CShip::S_HEAT] == 1 ? "Heat " : "",
				nSensors[CShip::S_ION] == 1 ? "Ion " : "", nSensors[CShip::S_MASS] == 1 ? "Mass " : "");

		return true;
	}

	gsType.MakeUpper();

	switch (gsType[0])
	{
		case 'E':
			if (nSensors[CShip::S_EM] == 2)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your EM Sensor Radome has been destroyed.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, Our EM Radome is unserviceable.");
				
				return true;
			}
			else if (nSensors[CShip::S_EM] == 0)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your ship does not possess an EM Radome to facilitate this sweep.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, We do not posses an EM Radome");

				return true;
			}
			else
			{
				pShip->m_Sweep[CShip::S_EM] = 1;
			}
			break;
		case 'H':
			if (nSensors[CShip::S_HEAT] == 2)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your Heat Sensor Radome has been destroyed.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, Our Heat Radome is unserviceable.");
				
				return true;
			}
			else if (nSensors[CShip::S_HEAT] == 0)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your ship does not possess a Heat Radome to facilitate this sweep.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, We do not posses a Heat Radome");

				return true;
			}
			else
			{
				pShip->m_Sweep[CShip::S_HEAT] = 1;
			}
			break;
		case 'I':
			if (nSensors[CShip::S_ION] == 2)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your Ion Sensor Radome has been destroyed.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, Our Ion Radome is unserviceable.");
				
				return true;
			}
			else if (nSensors[CShip::S_ION] == 0)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your ship does not possess an Ion Radome to facilitate this sweep.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, We do not posses an Ion Radome");
				
				return true;
			}
			else
			{
				pShip->m_Sweep[CShip::S_ION] = 1;
			}
		case 'M':
			if (nSensors[CShip::S_MASS] == 2)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your Mass Sensor Radome has been destroyed.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, Our Mass Radome is unserviceable.");
				return true;
			}
			else if (nSensors[CShip::S_MASS] == 0)
			{
				if (!Ch->IsNPC())
					Ch->Write("Your ship does not possess a Mass Radome to facilitate this sweep.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, We do not possess a Mass Radome!");

				return true;
			}
			else
			{
				pShip->m_Sweep[CShip::S_MASS] = 1;
			}
			break;
	}
			
	// Clear checks

	// Sweep simply scans each Radar contact and attempts to find out further detail about it

	if (Ch->IsNPC())
		Ch->Report(pShip->m_Commander, "Sweep commencing, Commander!");

	Ch->Write("You manipulate the Sensor controls, intiating a sensor sweep with the Vessel's radomes.#700\n\r");

	pShip->Write(CShip::MT_SENSORS, "#400[#401SWEEP commencing#400]#700\n\r");

	Ch->CurrentRoom()->Write(Ch, "%s manipulates the Sensor controls intiating a Sweep.\n\r", Ch->Name());
	

		
	// We need to set the timer for this Sweep, this depends on:
	// [1] Range of Scan
	// [2] Number of Radomes used
	int nCount = 0;
	for (int i = 0; i < CShip::S_MAX; i++)
		if (pShip->m_Sweep[i] == 1)
			nCount++;

	pShip->m_nSweep = nCount++;

	return true;
}


// Method     :: CmdComm
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
//			  :: <open>  <###.##>
//			  :: <close> <###.##>
//			  :: <jamm> <###.##> <###.##>
//			  :: <unjamm> <###.##> <###.##>
//			  :: <snoop> <###.##> <###.##>
//			  :: <unsnoop>  <###.##> <###.##>
//			  :: <load> <$$$##>
//			  :: <unload> <$$$##>
//			  :: <encrypt> <###.##> <$$$##>
//			  :: <record> <###.##>
//			  :: <recordings>
//			  :: <stop>
// Return     :: Bool
// Function   :: Allows a player to modify the configuration of their comms
//			  :: array. Allows channel opening, jamming, snooping and encryption.
// Written    :: 14/8/05 {OWV}

bool CmdComm::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	FreqList::iterator freq;
	MsgList::iterator msg;
	gStringList::const_iterator comms;

	gString gsFunction = (CommandLine.empty()) ? "" : *CommandLine.begin(); // Function
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Value
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();	// 2nd Value

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the comms
	if (!Ch->CanComm())
	{
		// They can't access the comm array, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the communications array.\n\r");
			Ch->Write("Locate a ship console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	// We need to get the Comms Array
	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	if (mlMods->size() == 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("This Ship has no Communications Array\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, This ship is not equipped with a Communications Array");

		return false;
	}

	CMComms* pComms = NULL;

	for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
	{
		// A ship can have as many Comm arrays as it wants for 
		// backup purposes, however, it can only ever have one
		// turned on at a time
		if ((*mod)->Powered())
		{
			pComms = (*mod)->m_Comms;
			// Found our powered one
			pMod = (*mod);
		}
	}
	
	if (!pMod)
	{
		if (!Ch->IsNPC())
			Ch->Write("You must power up your Communications Array first.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, Our Communications Array is not online!");

		return true;
	}

	////////////////////[Comtech Standard Comms Array]\\\\\\\\\\\\\\\\\\
	//                        STATUS :: ONLINE                        \\
	//              Memory Usage: 5%  [Using 1 of 50 Slots]           \\
	///////////////////////[ Active Frequencies ]\\\\\\\\\\\\\\\\\\\\\\\
	//  [1] - 105.55 Mhz Encrypt REB45                                \\
	//                                                    0 Open FRQ  \\
	//////////////////////[ Jammed Frequencies  ]\\\\\\\\\\\\\\\\\\\\\\\
	//  [1] - 104.45 Mhz                                              \\
	//                                                  0 Jammed FRQ  \\
	//////////////////////[ Snooped Frequencies ]\\\\\\\\\\\\\\\\\\\\\\\
	//  There are no Snooped Frequencies                              \\
	//                                                 0 Snooped FRQ  \\
	//////////////////////[ Encryption Protocols ]\\\\\\\\\\\\\\\\\\\\\\
	//  There are no Protocols currently loaded                       \\
	//                                                   0 Protocols  \\
	////////////////////[ Transmission Recordings ]\\\\\\\\\\\\\\\\\\\\\
	//  There are no saved Recordings                                 \\
	//                                                   0 Recordings \\
	//////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ 

	if (gsFunction == "status" || gsFunction == "")
	{
		Ch->Write("#600////////////////////#601[#700 %-27s#601]#600\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r", pMod->m_gsName);
		float fMem = pMod->Plus("Memory", false);
		

		float fUsed = ((float)(pComms->m_Protocols.size() + pComms->m_Recordings.size()) / fMem) * 100.0;
		Ch->Write("#600//#700 STATUS #600>#601>#600 %7s                            #700Memory Usage#601:#700 %3.0f#600 \\\\#700\n\r", pMod->Plus("Powered", false) ? "#201ONLINE #700#602" : "#101OFFLINE#700#602", fUsed);
		Ch->Write("#600//#700                                        #601[#700Using %2d of %2d Slots#601]#600 \\\\#700\n\r", (pComms->m_Protocols.size() + pComms->m_Recordings.size()), pMod->Plus("Memory", false)); 
		Ch->Write("#600///////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\r");
		Ch->Write("#600//#700 ACTIVE FREQUENCIES#600:                                            \\\\\n\r");
		if (pComms->m_Open.size() > 0)
		{ // Listing of all Open Frequencies
			int i = 1;
			for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
			{
				CFrequency* pFreq = *freq;
				if (pFreq->m_gsEncryption == "")
					Ch->Write("#600//#700  #601[#700%2d#601]#600 -#700 %3.2f #602Mhz %s                                          \\\\#700\n\r", i, pFreq->m_nFrequency, pComms->m_nOpen == (i-1) ? "#201<<#700#602" : "  ");
				else
					Ch->Write("#600//#700  #601[#700%2d#601]#600 -#700 %3.2f #602Mhz Encrypt #700%5s#602 %s                            \\\\#700\n\r", i,  pFreq->m_nFrequency, pFreq->m_gsEncryption, pComms->m_nOpen == (i-1) ? "#201<<#700#602" : "  ");

				i++;
			}
			
			Ch->Write("#600//#700                                                 #601 %3d#600 Open FRQ  \\\\#700\n\r", pComms->m_Open.size());
		}
		else
		{
			Ch->Write("#600//#700                                                    0 #600Open FRQ  \\\\#700\n\r");
		}

		Ch->Write("#600///////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\r");
		Ch->Write("#600//#700 JAMMED FREQUENCIES#600:                                            \\\\\n\r");
		mlMods = pShip->Get(CModule::MT_JAMMING_POD);
		if (mlMods->size() > 0)
		{
			CModule* pJamm = *mlMods->begin();
			
			// Check the Jamming Pod is not destroyed

			if (pJamm->m_ncDurability > 0)
			{
				if (pComms->m_Jamming.size() > 0)
				{ 
					CFrequency* pFrom;
					CFrequency* pTo;
					// Snooping Range
					freq = pComms->m_Jamming.begin();
				
					pFrom = *freq;
					pTo = *(freq+1);
					Ch->Write("#600//#700                #601[#700Jamming Band#601]#700 %3.2f#600 -#700 %3.2f #600Mhz              \\\\#700\n\r",  pFrom->m_nFrequency, pTo->m_nFrequency);
						
					Ch->Write("#600//#700                                       #601 %10d#600 Jammed Chans \\\\#700\n\r", (int)((pTo->m_nFrequency - pFrom->m_nFrequency)*100));
				}
				else
				{
					Ch->Write("#600//#700                                                 0 #600Jammed Chans \\\\#700\n\r");
				}
			}

		}
		else
		{
			Ch->Write("#600//#700                    No Jamming Pod Installed                    #600\\\\\n\r");		}


		mlMods = pShip->Get(CModule::MT_SNOOPING_POD);
		if (mlMods->size() > 0)
		{
			CModule* pSnoop = *mlMods->begin();
			
			// Check the Jamming Pod is not destroyed

			if (pSnoop->m_ncDurability > 0)
			{

				Ch->Write("#600//#700 SNOOPED FREQUENCIES#600:                                            \\\\\n\r");
				if (pComms->m_Snooping.size() > 0)
				{ 
					CFrequency* pFrom;
					CFrequency* pTo;
					// Snooping Range
					freq = pComms->m_Snooping.begin();
				
					pFrom = *freq;
					pTo = *(freq+1);
					Ch->Write("#600//#700                #601[#700Snooping Band#601]#700 %3.2f#600 -#700 %3.2f #600Mhz             \\\\#700\n\r",  pFrom->m_nFrequency, pTo->m_nFrequency);
						
					Ch->Write("#600//#700                                      #601 %10d#600 Snooped Chans \\\\#700\n\r", (int)((pTo->m_nFrequency - pFrom->m_nFrequency)*100));
				}
				else
				{
					Ch->Write("#600//#700                                                0 #600Snooped Chans \\\\#700\n\r");
				}
			}
			
		}

		Ch->Write("#600///////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\r");
		Ch->Write("#600//#700 ENCRYPTION FREQUENCIES#600:                                        \\\\\n\r");
		if (pComms->m_Protocols.size() > 0)
		{ // Installed Encryption protocols
			int i = 0;
			for (comms = pComms->m_Protocols.begin(); comms != pComms->m_Protocols.end(); comms++)
			{
				i++;
				gString gsEncr = *comms;
				Ch->Write("#602//#700  #601[#700%2d#601]#700#602 -#700 %s #602                                                 \\\\#700\n\r", i, gsEncr);
			}
			Ch->Write("#602//#700                           #601 %3d#700#602 Installed Encryption Protocols  \\\\#700\n\r", pComms->m_Protocols.size());
		}
		else
		{
			Ch->Write("#602//#700                                          0 #602Installed Protocols \\\\#700\n\r");
		}

		Ch->Write("#600///////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n\r");
		Ch->Write("#600//#700 TRANSMISSION RECORDINGS#600:                                       \\\\\n\r");
		if (pComms->m_Recording != NULL)
		{
			Ch->Write("#602//#700                           #601[#700Recording#601]#700#602 -#700 %3.2f Mhz #602            \\\\#700\n\r", pComms->m_Recording->m_nFrequency);
		}
		else
		{
			Ch->Write("#602//#700                           #601[#700Not Recording#601]#700#602                      \\\\#700\n\r");
		}
		if (pComms->m_Recordings.size() > 0)
		{
			Ch->Write("#602//#700                                           #601 %3d#700#602 Recordings \\\\#700\n\r", pComms->m_Recordings.size());
		}
		else
		{
			Ch->Write("#602//#700                                                  #700 0 #600Recordings \\\\#700\n\r");
		}

		Ch->Write("#602//////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");


	}
	else if (gsFunction == "Open")
	{
		// Open a new Frequency
		if (atoi(gsValue) < 0)
		{
			if (!Ch->IsNPC())
				Ch->Write("Negative frequencies are not allowed.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, We are unable to set Negative frequencies.");

			return true;
		}

		// Does it already exist?
		for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
		{
			CFrequency* pFreq = *freq;
			gString gsTemp;
			gString gsComp;
			gsTemp.Format("%0.3f", pFreq->m_nFrequency);
			gsComp.Format("%0.3f", atof(gsValue));
			if (gsTemp == gsComp)
			{
				if (!Ch->IsNPC())
					Ch->Write("That Frequency is already open.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander, That frequency is already open.");
				return true;
			}
		}

		// Valid range check goes here
		CFrequency* pNew = new CFrequency();
		pNew->m_nFrequency = atof(gsValue);

		pComms->m_Open.push_back(pNew);

		if (!Ch->IsNPC())
			Ch->Write("Frequency open.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Frequency open, Commander");


	}
	// Close is used to close an already Open Frequency
	else if (gsFunction == "Close")
	{
		// Close a Frequency
		bool bFound = false;
		FreqList pList;

		// Is it already Open?
		for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
		{
			CFrequency* pFreq = *freq;
			gString gsTemp;
			gString gsComp;
			gsTemp.Format("%0.3f", pFreq->m_nFrequency);
			gsComp.Format("%0.3f", atof(gsValue));

			if (gsTemp == gsComp)
			{
				if (!Ch->IsNPC())
					Ch->Write("Broadcast Frequency closed.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Broadcast Frequency closed, Commander");

				bFound = true;
			}
			else
			{
				pList.push_back(pFreq);
			}
		}

		if (!bFound)
		{
			if (!Ch->IsNPC())
				Ch->Write("No channels are open on that Frequency.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, We have no open channels on that Frequency");

			return true;
		}
		else
		{
			pComms->m_Open = pList;
		}

		return true;
	}
	// Load is used to load an Encryption protocol into the Comms computer
	else if (gsFunction == "load")
	{
		// First need to check for objects
		// #TODO#: Add after OLC objects are created

		// Check if it exists
		for (comms = pComms->m_Protocols.begin(); comms != pComms->m_Protocols.end(); comms++)
		{
			gString gsProt = *comms;
			
			if (gsValue == gsProt)
			{
				if (!Ch->IsNPC())
					Ch->Write("Encryption Protocol already loaded.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Commander that Encryption Protocol is already loaded.");

				return true;
			}
			
		}

		// Doesn't exist already lets do a memory check
		int nMem = 0;
		nMem = pMod->Plus("Memory", false);

		if ((pComms->m_Protocols.size() + pComms->m_Recordings.size()) >= nMem)
		{
			if (!Ch->IsNPC())
				Ch->Write("Communications Array memory is full, remove some data prior to loading another Protocol.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, Our Comms Array's memory is full.");

			return true;
		}
		else
		{
			pComms->m_Protocols.push_back(gsValue);
			if (!Ch->IsNPC())
				Ch->Write("Encryption Protocol loaded into array.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Loaded Encryption protocol, Commander");

			return true;
		}

	}
	// Unloading a Protocol from the Array
	else if (gsFunction == "delete")
	{
		// First need to check for objects
		// #TODO#: Add after OLC objects are created
		gStringList pNew;
		int i = 1;
		bool bFound = false;

		// Check if it exists
		for (comms = pComms->m_Protocols.begin(); comms != pComms->m_Protocols.end(); comms++)
		{
			gString gsProt = *comms;

			if (i == atoi(gsValue))
			{
				if (!Ch->IsNPC())
					Ch->Write("Encryption Protocol deleted.\n\r");
				else
					Ch->Report(pShip->m_Commander, "Deleted encryption protocol, Commander");

				bFound = true;
			}
			else
			{
				pNew.push_back(gsProt);
			}
			
			i++;
		}

		if (!bFound)
		{
			if (!Ch->IsNPC())
				Ch->Write("That Encryption Protocol does not exist.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, that Encryption protocol does not exit.");

			return true;
		}
		else
		{
			pComms->m_Protocols = pNew;
			return true;
		}

		
	}
	else if (gsFunction == "encrypt")
	{
		bool bFound = false;
		bool bOpen = false;

		// Check its a valid Protocol
		if (!pShip->CanDecrypt(gsValue2))
		{
			if (!Ch->IsNPC())
				Ch->Write("That Encryption protocol has not been loaded into the Comm Array.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander that Protocol has not been loaded into our Array");

			return true;
		}

		CFrequency* pFreq = new CFrequency();
		pFreq->m_nFrequency = atof(gsValue);

		// Check Frequency selected is Open
		if (!pShip->IsOpen(pFreq))
		{
			if (!Ch->IsNPC())
				Ch->Write("That Frequency Channel is not open. Open it first.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, Frequency channel is not open.");

			return true;
		}

		// Its a valid code and frequency
		for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
		{
			CFrequency* pFreq = *freq;
			gString gsTemp;
			gString gsComp;
			gsTemp.Format("%0.3f", pFreq->m_nFrequency);
			gsComp.Format("%0.3f", atof(gsValue));

			if (gsTemp == gsComp)
			{
				pFreq->m_gsEncryption = gsValue2;
					
				if (!Ch->IsNPC())
					Ch->Write("All transmissions using %0.2f will be Encrypted using %s", atof(gsValue), gsValue2);
				else
					Ch->Report(pShip->m_Commander, "All transmissions using %0.2f will be Encrypted using %s, Commander", atof(gsValue), gsValue2);

				return true;
			}
		}
			

	}
	// Disabling encryption for a Frequency
	else if (gsFunction == "decrypt")
	{
		// Stop using Encryption on a Frequency
		bool bFound = false;
		
		for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
		{
			CFrequency* pFreq = *freq;
			gString gsTemp;
			gString gsComp;
			gsTemp.Format("%0.3f", pFreq->m_nFrequency);
			gsComp.Format("%0.3f", atof(gsValue));

			if (gsTemp == gsComp)
			{
				if (!Ch->IsNPC())
					Ch->Write("Removing Encryption Protocol %s from transmissions on %3.2f\n\r", pFreq->m_gsEncryption, atof(gsValue));
				else
					Ch->Report(pShip->m_Commander, "Removing Encryption Protocol %s from transmissions on %3.2f, Commander\n\r", pFreq->m_gsEncryption, atof(gsValue));

				pFreq->m_gsEncryption = "";
				bFound = true;
			}
		}

		if (!bFound)
		{
			if (!Ch->IsNPC())
				Ch->Write("No channels are open on that Frequency.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, we have no Channels open on that Frequency");

			return true;
		}
		
		return true;
	}
	// Setting a channel to transmit on
	else if (gsFunction == "transmit")
	{
		bool bOpen = false;
		int i = 0;

		CFrequency* pFreq = new CFrequency();
		pFreq->m_nFrequency = atof(gsValue);

		// Check Frequency selected is Open
		if (!pShip->IsOpen(pFreq))
		{
			if (!Ch->IsNPC())
				Ch->Write("That channel is not currently open.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, That channel is not open.");
			return true;
		}
		else
		{
			for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
			{
				if ((*freq)->m_nFrequency == pFreq->m_nFrequency)
					pComms->m_nOpen = i;

				i++;
			}

			if (!Ch->IsNPC())
				Ch->Write("Active broadcast channel set to %3.2f.\n\r", (pComms->m_Open.at(pComms->m_nOpen))->m_nFrequency);
			else
				Ch->Report(pShip->m_Commander, "Active broadcast channel set to %3.2f, Commander.\n\r", (pComms->m_Open.at(pComms->m_nOpen))->m_nFrequency);
			

			return true;
		}


	}
	// Jamming
	else if (gsFunction == "jamm")
	{
		// Do they have a Jamming Pod
		ModuleList* mlMods = pShip->Get(CModule::MT_JAMMING_POD);

		if (mlMods->size() == 0)
		{
			if (!Ch->IsNPC())
				Ch->Write("Your vessel is not equipped with a Jamming Pod.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, Vessel is not equipped with a Jamming Pod.\n\r");
			
			return true;
		}

		// Jamm a new Frequency Band
		if (atoi(gsValue) < 0 || atoi(gsValue2) < 0)
		{
			if (!Ch->IsNPC())
				Ch->Write("Negative frequencies are not allowed.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, Comm Array is unable to handle Negative frequencies.\n\r");
			return true;
		}

		bool bEmpty = (pComms->m_Jamming.size() == 0);

		pComms->m_Jamming.clear();

		// Valid range check goes here
		CFrequency* pNew = new CFrequency();
		pNew->m_nFrequency = atof(gsValue);
		CFrequency* pNew2 = new CFrequency();
		pNew2->m_nFrequency = atof(gsValue2);

		pComms->m_Jamming.push_back(pNew);
		pComms->m_Jamming.push_back(pNew2);

		if (bEmpty)
		{
			if (!Ch->IsNPC())
				Ch->Write("Commencing jamming of band %3.2f - %3.2f Mhz.\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);
			else
				Ch->Report(pShip->m_Commander, "Commencing jamming of band %3.2f - %3.2f Mhz, Commander\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);
		}		
		else
		{
			if (!Ch->IsNPC())
				Ch->Write("Jamming band switched to %3.2f - %3.2f Mhz.\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);
			else
				Ch->Report(pShip->m_Commander, "Commander, Jamming band switched to %3.2f - %3.2f Mhz.\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);
		}

		return true;

	}
	// Snooping
	else if (gsFunction == "snoop")
	{

		// Do they have a Snooping Pod
		ModuleList* mlMods = pShip->Get(CModule::MT_SNOOPING_POD);

		if (mlMods->size() == 0)
		{
			if (!Ch->IsNPC())
				Ch->Write("Your vessel is not equipped with a Snooping Pod.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, This vessel is not equipped with a Snooping Pod.\n\r");
			
			return true;
		}

		// Snoop a new Frequency Band
		if (atoi(gsValue) < 0 || atoi(gsValue2) < 0)
		{
			if (!Ch->IsNPC())
				Ch->Write("Negative frequencies are not allowed.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, Comm Array is unable to handle Negative frequencies.\n\r");
			return true;
		}

		bool bEmpty = (pComms->m_Snooping.size() == 0);

		pComms->m_Snooping.clear();

		// Valid range check goes here
		CFrequency* pNew = new CFrequency();
		pNew->m_nFrequency = atof(gsValue);
		CFrequency* pNew2 = new CFrequency();
		pNew2->m_nFrequency = atof(gsValue2);

		pComms->m_Snooping.push_back(pNew);
		pComms->m_Snooping.push_back(pNew2);

		if (bEmpty)
			Ch->Write("New snooping band opened.\n\r");
		else
			Ch->Write("Snooping band switched to %3.2f - %3.2f Mhz.\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);

		if (bEmpty)
		{
			if (!Ch->IsNPC())
				Ch->Write("New snooping band opened.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, new Snooping band opened.\n\r");
		}		
		else
		{
			if (!Ch->IsNPC())
				Ch->Write("Snooping band switched to %3.2f - %3.2f Mhz.\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);
			else
				Ch->Report(pShip->m_Commander, "Commander, Snooping band switched to %3.2f - %3.2f Mhz.\n\r", pNew->m_nFrequency, pNew2->m_nFrequency);
		}

		return true;

	}
	else if (gsFunction == "unjamm")
	{
		pComms->m_Jamming.clear();

		if (!Ch->IsNPC())
			Ch->Write("Active jamming band cancelled.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, Active jamming band cancelled.");
		
		return true;
	}
	else if (gsFunction == "unsnoop")
	{
		pComms->m_Snooping.clear();
		
		if (!Ch->IsNPC())
			Ch->Write("Active Snooping band cancelled.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, Active Snooping band cancelled.");

		return true;
	}
	// Recording
	else if (gsFunction == "record")
	{
		// Record a new Frequency
		if (atoi(gsValue) < 0)
		{
			if (!Ch->IsNPC())
				Ch->Write("Negative frequencies are not allowed.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, Comm Array is unable to handle Negative frequencies.\n\r");
			return true;
		}

		// Valid range check goes here
		CFrequency* pNew = new CFrequency();
		pNew->m_nFrequency = atof(gsValue);

		pComms->m_Recording = pNew;

		if (!Ch->IsNPC())
				Ch->Write("Recording started.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Recording started, Commander");
		return true;
	}
	// Cancelling recording
	else if (gsFunction == "stop")
	{
		// Stop Recording a Frequency
		pComms->m_Recording = NULL;
		if (!Ch->IsNPC())
				Ch->Write("Recording stopped.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Recording stopped, Commander");

		return true;
	}
	// Showing Recordings
	else if (gsFunction == "recordings")
	{
		if (pComms->m_Recordings.size() <= 0)
		{
			Ch->Write("There are no recorded Transmissions.\n\r");
			return true;
		}

		Ch->Write("#602////////////////////#601[#700Transmission Recordings#601]#700#602\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		int i = 1;

		for (msg = pComms->m_Recordings.begin(); msg != pComms->m_Recordings.end(); msg++)
		{
			CMessage* pMsg = *msg;
			Ch->Write("#601[#700%3d#601]#700#602 %s on#700 %s#602 Mhz Recorded At#700 %s\r", i, pMsg->m_gsShip, pMsg->m_gsFrequency, ctime(&pMsg->m_tTime));
			Ch->Write("#601\"#700 %s #601\"#700\n\r", pMsg->m_gsMessage);
			i++;

		}
	}

	return true;
}

// Method     :: CmdShield
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a ship to manipulate its shields, also displays a GUI for the shield stats
// Written    :: 8/9/05 {OWV}

bool CmdShield::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can access ship systems
	if (!Ch->CanSystem())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to control the shields!\n\r");
			Ch->Write("Locate a systems console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}


	int nCap = 0;
	bool bDouble = false;

	// Center piece
	gString cR = "#100|#700";
	gString cA = "#300|#700";
	gString cG = "#200|#700";
	gString cGG= "#201|#700";
	gString cN = "|";

	// Top Corner piece
	gString tR = "#100/#700";
	gString tA = "#300/#700";
	gString tG = "#200/#700";
	gString tGG= "#201/#700";
	gString tN = "/";

	// Bottom Corner piece
	gString bR = "#100\\#700";
	gString bA = "#300\\#700";
	gString bG = "#200\\#700";
	gString bGG= "#201\\#700";
	gString bN = "\\";

	// Top Line piece
	gString mR = "#100_#700";
	gString mA = "#300_#700";
	gString mG = "#200_#700";
	gString mGG= "#201_#700";
	gString mN = "_";

	// Top Line double piece
	gString m2R = "#100-#700";
	gString m2A = "#300-#700";
	gString m2G = "#200-#700";
	gString m2GG= "#201-#700";
	gString m2N = "-";

	ModuleList* mlEmitters = pShip->Get(CModule::MT_SHIELD_GENERATOR);
	ModuleList* mlCapacitors = pShip->Get(CModule::MT_SHIELD_GENERATOR);
	
	int nShare = nCap / 24; // Dividing by six to split into sections and by four again to get segment
	int nMax[CShip::A_MAX] = {0, 0, 0, 0, 0, 0};

	for (int nM = 0; nM < CShip::A_MAX; nM++)
		nMax[nM] = pShip->Shield(nM, false);
		

	int nCur[CShip::A_MAX] = {0, 0, 0, 0, 0, 0};

	for (int nC = 0; nC < CShip::A_MAX; nC++)
		nCur[nC] = pShip->Shield(nC, true);
	

	// Port bank
	gString pC = " ";
	gString pT = " ";
	gString pB = " ";
	// Starboard bank
	gString sC = " ";
	gString sT = " ";
	gString sB = " ";
	// Fore bank
	gString tS = " ";
	gString tD = " ";
	// Aft bank
	gString bS = " ";
	gString bD = " ";
	// Dorsal bank
	gString dS = " ";
	gString dD = " ";
	// Vental bank
	gString vS = " ";
	gString vD = " ";

	// Port Shields::
	if (nCur[CShip::A_PORT] > (nMax[CShip::A_PORT]/4))	// 1/4 of Full
	{	
		if (nCur[CShip::A_PORT] > (nMax[CShip::A_PORT]/2)) // 1/2 of Full
		{
			if (nCur[CShip::A_PORT] > ((3 * nMax[CShip::A_PORT]) /4)) 
			{
				sC = cGG;
				sT = tGG;
				sB = bGG;
			}
			else
			{
				sC = cG;
				sT = tG;
				sB = bG;
			}
		}
		else
		{
			sC = cA;
			sT = tA;
			sB = bA;
		}

	}
	else
	{
		if (nCur[CShip::A_PORT] != 0)
		{
			sC = cR;
			sT = tR;
			sB = bR;
		}
		else
		{
			sC = cN;
			sT = tN;
			sB = bN;
		}
	}

	// Starboard Shield::
	if (nCur[CShip::A_STARBOARD] > (nMax[CShip::A_STARBOARD]/4))	// 1/4 of Full
	{	
		if (nCur[CShip::A_STARBOARD] > (nMax[CShip::A_STARBOARD]/2)) // 1/2 of Full
		{
			if (nCur[CShip::A_STARBOARD] >= ((3 * nMax[CShip::A_STARBOARD]) /4)) 
			{
				pC = cGG;
				pT = tGG;
				pB = bGG;
			}
			else
			{
				pC = cG;
				pT = tG;
				pB = bG;
			}
		}
		else
		{
			pC = cA;
			pT = tA;
			pB = bA;
		}

	}
	else
	{
		if (nCur[CShip::A_STARBOARD] != 0)
		{
			pC = cR;
			pT = tR;
			pB = bR;
		}
		else
		{
			pC = cN;
			pT = tN;
			pB = bN;
		}
	}

	// Fore Shield::
	if (nCur[CShip::A_FORE] > (nMax[CShip::A_FORE]/4))	// 1/4 of Full
	{	
		if (nCur[CShip::A_FORE] > (nMax[CShip::A_FORE]/2)) // 1/2 of Full
		{
			if (nCur[CShip::A_FORE] >= ((3 * nMax[CShip::A_FORE]) /4)) 
			{
				tS = mGG;
				tD = m2GG;

			}
			else
			{
				tS = mG;
			}
		}
		else
		{
			tS = mA;
		}

	}
	else
	{
		// We don't draw anything if its less than zero
		if (nCur[CShip::A_FORE] != 0)
		{
			tS = mR;
		}
		else
		{
			tS = mN;
		}
	}

	// Aft Shield::
	if (nCur[CShip::A_AFT] > (nMax[CShip::A_AFT]/4))	// 1/4 of Full
	{	
		if (nCur[CShip::A_AFT] > (nMax[CShip::A_AFT]/2)) // 1/2 of Full
		{
			if (nCur[CShip::A_AFT] >= ((3 * nMax[CShip::A_AFT]) /4)) 
			{
				bS = mGG;
				bD = m2GG;

			}
			else
			{
				bS = mG;
			}
		}
		else
		{
			bS = mA;
		}

	}
	else
	{
		if (nCur[CShip::A_STARBOARD] != 0)
		{
			bS = mR;
		}
		else
		{
			bS = mN;
		}
	}

	// Dorsal Shield::
	if (nCur[CShip::A_DORSAL] > (nMax[CShip::A_DORSAL]/4))	// 1/4 of Full
	{	
		if (nCur[CShip::A_DORSAL] > (nMax[CShip::A_DORSAL]/2)) // 1/2 of Full
		{
			if (nCur[CShip::A_DORSAL] >= ((3 * nMax[CShip::A_DORSAL]) /4)) 
			{
				dS = mGG;
				dD = m2GG;
			}
			else
			{
				dS = mG;
			}
		}
		else
		{
			dS = mA;
		}

	}
	else
	{
		if (nCur[CShip::A_VENTRAL] != 0)
		{
			dS = mR;
		}
		else
		{
			dS = mN;
		}
	}

	// Vental Shield::
	if (nCur[CShip::A_VENTRAL] > (nMax[CShip::A_VENTRAL]/4))	// 1/4 of Full
	{	
		if (nCur[CShip::A_VENTRAL] > (nMax[CShip::A_VENTRAL]/2)) // 1/2 of Full
		{
			if (nCur[CShip::A_VENTRAL] >= ((3 * nMax[CShip::A_VENTRAL]) /4)) 
			{
				vS = mGG;
				vD = m2GG;

			}
			else
			{
				vS = mG;
			}
		}
		else
		{
			vS = mA;
		}

	}
	else
	{
		// We don't draw anything if its less than zero
		if (nCur[CShip::A_VENTRAL] != 0)
		{
			vS = mR;
		}
		else
		{
			vS = mN;
		}
	}

	bool bCharging = false;

	 
	if (pShip->m_nPower[CShip::PT_SHIELD] == 1)
		bCharging = true;

	// Draw the Picture
	int nCount = 1;
	
	Ch->Write("#400///////////////////////////#401[#700Shield Banks#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	Ch->Write("#400//#700  Main Shields                            Dorsal/Ventral Shields  #400\\\\#700\n\r");
    Ch->Write("#400//#700     %s%s%s%s%s%s%s              Emitters               %s%s%s%s%s%s%s          #400\\\\#700\n\r", tS, tS, tS, tS, tS, tS, tS, dS, dS, dS, dS, dS, dS, dS);
	Ch->Write("#400//#700    %s       %s            %8s                                #400\\\\#700\n\r", pT, sB, bCharging ? "#700#202[#700#201Online#700#202] #700" : "#700#102[#700#101Offline#700#102]#700");
	Ch->Write("#400//#700   %s    #101O#700    %s                                     OO             #400\\\\#700 \n\r", pT, sB);
	Ch->Write("#400//#700  %s    OOO    %s          Capacitors                OO#101O#700            #400\\\\#700\n\r", pC, sC);
	Ch->Write("#400//#700   %s   OOO   %s       #400[#401%7d#400/#401%7d#400]#700             OO             #400\\\\#700\n\r", pB, sT, pShip->Capacitors(true), pShip->Capacitors(false));
	Ch->Write("#400//#700    %s%s%s%s%s%s%s%s%s                                    %s%s%s%s%s%s%s          #400\\\\#700\n\r", pB, bS, bS, bS, bS, bS, bS, bS, sT, vS, vS, vS, vS, vS, vS, vS);
	Ch->Write("#400//                                                                  \\\\#700\n\r");
	Ch->Write("#400/////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		if ((*comp)->m_Type == CComponent::CT_SHIELD)
		{
			Ch->Write("#400//#701 %-64s #400\\\\#700\n\r", (*comp)->m_gsName);
			// Show any Shield Capacitors or Emitters
			for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
			{
				CModule* pMod = ((*map).second);

				float fIntegrity = ((float)pMod->m_ncDurability / (float)pMod->m_nmDurability) * 100.0f;
				gString gsIntegrity;

				if (fIntegrity > 90.0)
					gsIntegrity.Format("#201 %3.0f#700", fIntegrity);
				if (fIntegrity <= 90.0 && fIntegrity > 75.0)
					gsIntegrity.Format("#200 %3.0f#700", fIntegrity);
				if (fIntegrity <= 75.0 && fIntegrity > 50.0)
					gsIntegrity.Format("#301 %3.0f#700", fIntegrity);
				if (fIntegrity <= 50.0 && fIntegrity > 25.0)
					gsIntegrity.Format("#101 %3.0f#700", fIntegrity);
				else if (fIntegrity <= 25.0)
					gsIntegrity.Format("#100 %3.0f#700", fIntegrity);

				if (pMod->m_nType == CModule::MT_SHIELD_GENERATOR)
				Ch->Write("#400// [#401 %3d#400] #700%-42s %8s %s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->Powered() ? "#200<#201Online#200> " : "#100<#101Offline#100>", gsIntegrity);

			}
			nCount++;
		}
		else
		{
			nCount += (*comp)->m_Modules.size();
		}


	}

	Ch->Write("#400/////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");


	Ch->CurrentRoom()->Write(Ch, "%s studies the Shield configuration.\n\r", Ch->Name());
	return true;
}

// Method     :: CmdLaunch
// Class	  :: <none>
// Parameters :: <actor, arguments> 
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a ship to launch
// Written    :: 17/12/2005 {OWV}

bool CmdLaunch::Perform(CActor* Ch, gStringList& CommandLine)
{
	LOG_SCOPE("CmdLaunch::Perform");
	// Testing purposes only
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();


	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can pilot the ship
	if (!Ch->CanPilot())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to control this ship!\n\r");
			Ch->Write("Locate a piloting console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	// Repulsor Coil check
	if (pShip->m_ShipState->IsSet(CShip::_FLYING))
	{
		if (!Ch->IsNPC())
			Ch->Write("Your ship is already launched!\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, We are already launched!");

		return true;
	}
	else if (!pShip->m_ShipState->IsSet(CShip::_REPULSOR))
	{
		if (!Ch->IsNPC())
			Ch->Write("You must engage your Repulsor Coils first of all.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, Our Repulsor Coils are offline!");

		return true;
	}
	else
	{
		bool bFound = false;
		// We need to power up the Ion Engines here as well, all of them!
		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
				for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
				{	
					if ((*comp)->m_Type == CComponent::CT_SUBLIGHT)
					{
						for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
						{
							if (((*mod).second)->m_nType == CModule::MT_ION_ENGINE)
							{
								bFound = true;
								((*mod).second)->SetPlus("Power", 1);
							}
						}
					}
				}
		}

		if (!bFound)
		{
			pShip->m_ShipState->RemoveBit(CShip::_LAUNCHING);
			pShip->m_ShipState->SetBit(CShip::_REPULSOR);

			if (!Ch->IsNPC())
				pShip->Write(CShip::MT_HELM, "Your ship is not equipped with Ion Engines! Launch Sequence aborted.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Commander, We do not have any Ion Engines, aborting launch sequence");
		}
		else
		{
			pShip->m_ShipState->SetBit(CShip::_LAUNCHING);
			pShip->m_ShipState->RemoveBit(CShip::_REPULSOR);
			pShip->m_ShipState->RemoveBit(CShip::_LANDED);
			// Need to close the doors!
			pShip->m_nExitState = CShip::ES_CLOSED;
			// Need an internal message about the door
			CArea* pArea = Ch->CurrentRoom()->GetArea();
			CRoom* pRoom = pArea->GetRoom(pShip->m_nExit);
			if (pShip->m_nExitState = CShip::ES_OPEN)
			{
				if (pRoom)
					pRoom->Write("The %s closes in preparation for takeoff.\n\r", pShip->m_gsExit);
				else
					g_Log.Log("%s has no room set as its Exit", pShip->m_gsName);
			}

			// Give an external message about the ship
			pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());
			pRoom = pArea->GetRoom(pShip->m_Land.Room());
			pRoom->Write("%s rotates skywards using its Repulsor coils. A Sharp whine indiciates its Sublight drive coming online.\n\r", pShip->m_gsName, pShip->m_gsCMsg);

			Ch->CurrentRoom()->Write(Ch, "%s manipulates the Helm and repulsors, bringing the ship off the ground.\n\r", Ch->Name());

			// Give the player a message
			if (!Ch->IsNPC())
				Ch->Write("You manipulate the repulsors and start to powerup the Sublight engines.\n\r");
			else
				Ch->Report(pShip->m_Commander, "Launch procedures initiated, Commander.");

			

			pShip->m_nTimer = CComponent::T_SUBLIGHT;
		}
		
	}


	return true;
}


// Method     :: CmdScm
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
//			  :: <Reactor|Coolant|Ion|Loadshed|Overheat|Bulkhead|Self-destruct>
// Return     :: Bool
// Function   :: Allows a ship to manipulate its Safety Control Mechanism
// Written    :: 17/12/2005 {OWV}

bool CmdScm::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();
	if (!CommandLine.empty())
		CommandLine.pop_front();

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdScm::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can access the ship's systems
	if (!Ch->CanSystem())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to access the ship's systems!\n\r");
			Ch->Write("Locate a systems console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	if (!pShip)
		return false;

	if (gsValue == "status" || gsValue == "")
	{

		///////////////////////////////[ SCM Status ]\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
		//                                                                          \\
		// Shutdown Procedures:                                                     \\
		// >> Reactor: Engaged [Shutdown if Reactor Integrity down to 50]           \\
		// >> Coolant: Engaged [Shutdown if Coolant Plant Integrity down to 50]     \\
		// >> Ion: Engaged [Shutdown if Ion Engine Integrity down to 50]            \\
		// Energy/Heat Management:                                                  \\
		// >> Loadshed: Engaged [Load Shedd if Energy Load exceeds Generation]      \\
		// >> Overheat: Engaged [Shutdown systems if they begin to Overheat]        \\
		// Emergency Overrides:                                                     \\
		// >> Bulkhead: Engaged [Close bulkheads in the event of a Hullbreech]      \\
		// >> Self-destruct: Engaged [Remove Reactor failsafes to cause meltdown]   \\
		//                                                                          \\
		///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ */

		Ch->Write("#100/////////////////////////////////#101[#700 SCM Status#101 ]#100\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		Ch->Write("#100//                                                                              \\\\#700\n\r");
		Ch->Write("#100// #701Shutdown Procedures#100:                                                         \\\\#700\n\r");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Shutdown if Reactor Integrity down to 50#100]       \\\\#700\n\r", CShip::szScm[CShip::SCM_REACTOR-1], pShip->m_Scm->IsSet(CShip::SCM_REACTOR) ? "#201Engaged   #100" : "#100Disengaged#100");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Shutdown if Coolant Plant Integrity down to 50#100] \\\\#700\n\r", CShip::szScm[CShip::SCM_COOLANT-1], pShip->m_Scm->IsSet(CShip::SCM_COOLANT) ? "#201Engaged   #100" : "#100Disengaged#100");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Shutdown if Ion Engine Integrity down to 50#100]    \\\\#700\n\r", CShip::szScm[CShip::SCM_ION-1], pShip->m_Scm->IsSet(CShip::SCM_ION) ? "#201Engaged   #100" : "#100Disengaged#100");
		Ch->Write("#100// #701Energy/Heat Management#100:                                                      \\\\#700\n\r");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Load Shedd if Energy Load exceeds Generation#100]   \\\\#700\n\r", CShip::szScm[CShip::SCM_LOADSHED-1], pShip->m_Scm->IsSet(CShip::SCM_LOADSHED) ? "#201Engaged   #100" : "#100Disengaged#100");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Shutdown systems if they begin to Overheat#100]     \\\\#700\n\r", CShip::szScm[CShip::SCM_OVERHEAT-1], pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT) ? "#201Engaged   #100" : "#100Disengaged#100");
		Ch->Write("#100// #701Emergency Overrides#100:                                                         \\\\#700\n\r");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Close bulkheads in the event of a Hullbreech#100]   \\\\#700\n\r", CShip::szScm[CShip::SCM_BULKHEAD-1], pShip->m_Scm->IsSet(CShip::SCM_BULKHEAD) ? "#201Engaged   #100" : "#100Disengaged#100");
		Ch->Write("#100// #700%-13s #100>#101> %-10s #100[#700Remove Reactor failsafes to cause meltdown#100]     \\\\#700\n\r", CShip::szScm[CShip::SCM_SELFDESTRUCT-1], pShip->m_Scm->IsSet(CShip::SCM_SELFDESTRUCT) ? "#100Engaged   #100" : "#201Disengaged#100");
		Ch->Write("#100//                                                                              \\\\#700\n\r");
		Ch->Write("#100/////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");

		Ch->CurrentRoom()->Write(Ch, "%s glances to the Safety Control Mechanism status screen.\n\r", Ch->Name());
		return true;

	}
	else
	{
		bool bValid = false;
		int nSys = 0;
		int i = 0;

		for (i = 0; i < CShip::SCM_MAX; i++)
		{
			gString gsScm = CShip::szScm[i]; 
			if (gsValue == gsScm)
			{
				bValid = true;
				nSys = i;
				break;
			}
		}

		if (bValid)
		{
			bool bRemove = false;

			switch (i+1)
			{
			case CShip::SCM_REACTOR:
			if (pShip->m_Scm->IsSet(CShip::SCM_REACTOR))
			{
				pShip->m_Scm->RemoveBit(CShip::SCM_REACTOR); bRemove = true; break;
			}
			else
				pShip->m_Scm->SetBit(CShip::SCM_REACTOR); break;

			case CShip::SCM_COOLANT:
			if (pShip->m_Scm->IsSet(CShip::SCM_COOLANT))
			{
				pShip->m_Scm->RemoveBit(CShip::SCM_COOLANT); bRemove = true; break;
			}
			else
				pShip->m_Scm->SetBit(CShip::SCM_COOLANT); break;

			case CShip::SCM_OVERHEAT:
			if (pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT))
			{
				pShip->m_Scm->RemoveBit(CShip::SCM_OVERHEAT); bRemove = true; break;
			}
			else
				pShip->m_Scm->SetBit(CShip::SCM_OVERHEAT); break;

			case CShip::SCM_BULKHEAD:
			if (pShip->m_Scm->IsSet(CShip::SCM_BULKHEAD))
			{
				pShip->m_Scm->RemoveBit(CShip::SCM_BULKHEAD); bRemove = true; break;
			}
			else
				pShip->m_Scm->SetBit(CShip::SCM_BULKHEAD); break;

			case CShip::SCM_ION:
			if (pShip->m_Scm->IsSet(CShip::SCM_ION))
			{
				pShip->m_Scm->RemoveBit(CShip::SCM_ION); bRemove = true; break;
			}
			else
				pShip->m_Scm->SetBit(CShip::SCM_ION); break;

			case CShip::SCM_LOADSHED:
			if (pShip->m_Scm->IsSet(CShip::SCM_LOADSHED))
			{
				pShip->m_Scm->RemoveBit(CShip::SCM_LOADSHED); bRemove = true; break;
			}
			else
				pShip->m_Scm->SetBit(CShip::SCM_LOADSHED); break;

			case CShip::SCM_SELFDESTRUCT:
			if (pShip->m_Scm->IsSet(CShip::SCM_SELFDESTRUCT))
			{
				pShip->m_nSelfdestruct = -1;
				pShip->m_Scm->RemoveBit(CShip::SCM_SELFDESTRUCT); bRemove = true; break;
			}
			else
			{
				pShip->m_nSelfdestruct = 4;
				pShip->m_Scm->SetBit(CShip::SCM_SELFDESTRUCT); break;
			}
			}
				


			if (bRemove)
				Ch->Write("[SCM] %s disabled.\n\r", gsValue);
			else
				Ch->Write("[SCM] %s mechanism enabled.\n\r", gsValue);

							
			return true;

		}
	}
		




	Ch->Write("Invalid Syntax:\n\rScm <Status> :: Display current SCM Setup.\n\rScm <System> :: Modify Scm settings\n\r");
	return true;
}

// Method     :: CmdRoll
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <left|right|inverted|upright>
// Return     :: Bool
// Function   :: Allows the player to change the roll rate of the Ship
// Written    :: 04/7/05 {OWV}

bool CmdRoll::Perform(CActor* Ch, gStringList& CommandLine)
{
    CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Degree of roll
	if (!CommandLine.empty())
		CommandLine.pop_front();

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdRoll::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can pilot the ship
	if (!Ch->CanPilot())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to control this ship!\n\r");
			Ch->Write("Locate a piloting console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	float fRoll;

	if (gsValue == "")
	{
		Ch->Write("[Invalid Input] You must select a position to roll to.\n\r");
		Ch->Write("Valid Positions: Upright, Left, Inverted, Right.\n\r");
		return true;
	}

	if (gsValue != "Left" && gsValue != "Right" && gsValue != "Inverted" && gsValue != "Upright")
	{
		Ch->Write("[Invalid Position] You must select a  valid position to roll to.\n\r");
		Ch->Write("Valid Positions: Upright, Left, Inverted, Right.\n\r");
		return true;
	}

	// Did we get a valid ship?
	if (!pShip)
		return false;

	// Do they have maneuvering thrusters?
	if (pShip->Maneuver(false) <= 0)
	{
		Ch->Write("This vessel does not the ability to Maneuver.\n\r");
		return true;
	}

	// Do they have these powered on?
	if (pShip->Maneuver(true) <= 0)
	{
		Ch->Write("You must power up your Steering Engine and Maneuvering Thrusters first.\n\r");
		return true;
	}

	// Are they trying to roll while landed?
	if (pShip->m_ShipState->IsSet(CShip::_LANDED) || pShip->m_ShipState->IsSet(CShip::_REPULSOR))
	{
		Ch->Write("You cannot roll on the ground!\n\r");
		return true;
	}

	// Are they trying to roll in the Atmosphere
	if (!pShip->m_ShipState->IsSet(CShip::_FLYING))
	{
		// #TODO
		Ch->Write("Atmospheric maneuvering is not implemented. Stick to Space.\n\r");
		return true;
	}


	// Determine our roll position
	if (gsValue == "Left")
		fRoll = 270.0;
	else if (gsValue == "Inverted")
		fRoll = 180.0;
	else if (gsValue == "Right")
		fRoll = 90.0;
	else if (gsValue == "Upright")
		fRoll = 0.0;
	else	// Sanity check
	{
		Ch->Write("[Invalid Position] You must select a  valid position to roll to.\n\r");
		Ch->Write("Valid Positions: Upright, Left, Inverted, Right.\n\r");
		return true;
	}


	// We have set our Deltas now, we need to set the Destination Heading
	pShip->m_dHeading->x = fRoll;

	Ch->CurrentRoom()->Write(Ch, "%s manipulates the Helm, starting to roll the vessel.\n\r", Ch->Name());

	Ch->Write("You move the flight yokes, intiating a roll.\n\r");
	pShip->Write(CShip::MT_HELM, "[Helm] Initiating roll.\n\r");
	pShip->NotifySpace("%s begins to roll %s\n\r", pShip->m_gsName, gsValue);

	return true;

}


// Method     :: CmdLand
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <target> {hover}
// Return     :: Bool
// Function   :: Allows the Ship to Land on a Planet
// Written    :: 04/01/2006 {OWV}

bool CmdLand::Perform(CActor* Ch, gStringList& CommandLine)
{
    CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Target
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsHover = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Hovering?
	bool bCrash = false;

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdLand::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal from which they can pilot the ship
	if (!Ch->CanPilot())
	{
		// If they aren't manning a terminal, let them know
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this in order to control this ship!\n\r");
			Ch->Write("Locate a piloting console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	if (gsValue == "")
	{
		if (!Ch->IsNPC())
			Ch->Write("[Invalid Input] You must select a valid landing site.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, Exactly where do you want me to Land?");

		return true;
	}

	if (!pShip)
		return false;

	// Are they trying to land while already landed?
	if (pShip->m_ShipState->IsSet(CShip::_LANDED))
	{
		Ch->Write("But! You are already landed!\n\r");
		return true;
	}

	// Are they Hovering on Repulsors?
	if (pShip->m_ShipState->IsSet(CShip::_REPULSOR) && !pShip->m_ShipState->IsSet(CShip::_FLYING))
	{
		if (!Ch->IsNPC())
			Ch->Write("To land simply disengage your Repulsor Coils.\n\r");
		else
			Ch->Report(pShip->m_Commander, "All we need to do is disengage Repulsor Coils, Commander.");

		return true;
	}

	// Did we get a valid ship?
	CSpatial* pSpatial = pGalaxy->GetSpa(gsValue);

	if (!pSpatial)
	{
		if (!Ch->IsNPC())
			Ch->Write("[Invalid Target] No such Target exists.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Unable to locate %s on Sensors, Commander.", gsValue);

		return true;
	}

	// Do they have maneuvering thrusters?
	// If not they are going to attempt a Crash landing
	if (pShip->Maneuver(false) <= 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("Attempting Crash landing...\n\r");
		else
			Ch->Report(pShip->m_Commander, "All hands! Brace for crash landing!");

		bCrash = true;
		return true;
	}

	// Do they have these powered on?
	if (pShip->m_Speed <= 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("You need to throttle up if you want to Land on %s!\n\r", gsValue);
		else
			Ch->Report(pShip->m_Commander, "Permission to throttle up in order to land, Commander?");

		return true;
	}

	// TEST #TODO# Extend this
	// We are just going to stick them on the 'Corellian Landing Pad'
	CRoom* pRoom = CGameObjects:: Get().GameWorld()->GetArea(2)->GetRoom(0);

	if (!pRoom)
	{
		Ch->Write("Unable to locate Landing pad. Landing aborted.\n\r");
		return true;
	}
	else
	{
		// Display some Messages
		ModuleList* mlMods = pShip->Get(CModule::MT_REPULSOR_COILS);
		CModule* pRepulsor = NULL;

		for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
		{
			if ((*mod)->m_ncDurability > 0)
				pRepulsor = (*mod);
		}

		if (!pRepulsor && gsHover != "")
		{
			Ch->Write("Your ship is not equipped with Repulsor Coils, attempting standard landing.\n\r");
			gsHover = "";
		}

		if (bCrash)
		{
			pShip->Write(CShip::MT_CRITICAL, "Without the ability to maneuver the ship slams into the ground out of control! As the ship slowly comes to a halt, you breathe a sigh of relief.\n\r");
			for (ActorMap::iterator act = pRoom->Actors().begin(); act != pRoom->Actors().end(); act++)
			{
				// Display a message to the Actor
				(*act).second->Write("%s falls out of the sky, crashing into the ground. Ops!\n\r", pShip->m_gsType);
			}

			pShip->m_Land = CPlacement(2, 0, 0);
			pShip->m_ShipState->SetBit(CShip::_LANDED);
			pGalaxy->RemoveSpatialFromSpace(pShip->m_Vnum);
		}
		else
		{
			if (pRepulsor && pRepulsor->m_ncDurability > 0 && gsHover != "")
			{
				pRepulsor->SetPlus("Powered", 1);
				pShip->Write(CShip::MT_SYSTEMS, "[%s] Online.\n\r", pRepulsor->m_gsName);
				pShip->m_ShipState->SetBit(CShip::_REPULSOR);
				pGalaxy->RemoveSpatialFromSpace(pShip->m_Vnum);
				
				Ch->CurrentRoom()->Write(Ch, "%s tightens their grip on the Flight controls as they prepare to enter the Hover.\n\r", Ch->Name());

				pShip->Write(CShip::MT_CRITICAL, "The ship enters the Hover just above the ground.\n\r");

				if (Ch->IsNPC())
					Ch->Report(pShip->m_Commander, "Hover established, Commander");

				for (ActorMap::iterator act = pRoom->Actors().begin(); act != pRoom->Actors().end(); act++)
				{
					// Display a message to the Actor
					(*act).second->Write("%s gently lowers down, establishing itself in the hover above the ground.\n\r", pShip->m_gsType);
				}
			}
			else
			{
				// Different message for using Repulsors to Land
				if (pRepulsor && pRepulsor->m_ncDurability > 0)
				{
					Ch->CurrentRoom()->Write(Ch, "%s engages repulsors manipulating the helm to bring the vessel into land.\n\r", Ch->Name());
					Ch->Write("You engage your %s and decelerate gently onto the ground.\n\r", pRepulsor->m_gsName);
					pShip->Write(CShip::MT_CRITICAL, "As the %s engage the ship decelerates gently onto the ground.\n\r", pRepulsor->m_gsName);

					if (Ch->IsNPC())
					{
						// Give a Completed Event so the Crew can update
						if (pShip->m_Crew.size() > 0)
						{
							EComplete * pEvent = new EComplete();
							pEvent->m_nType = COrder::_LAND;

							pShip->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
						}
					}

					
					for (ActorMap::iterator act = pRoom->Actors().begin(); act != pRoom->Actors().end(); act++)
					{
						// Display a message to the Actor
						(*act).second->Write("%s appears rapidly from above! It decelerates using its Repulsor Coils and settles slowly onto the ground.\n\r", pShip->m_gsType);
					}

				}
				else
				{

					if (Ch->IsNPC())
					{
						// Give a Completed Event so the Crew can update
						if (pShip->m_Crew.size() > 0)
						{
							EComplete * pEvent = new EComplete();
							pEvent->m_nType = COrder::_LAND;

							pShip->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
						}
					}

					Ch->CurrentRoom()->Write(Ch, "%s manipulates the flight yokes to bring the vessel down carefully into land.\n\r", Ch->Name());
					pShip->Write(CShip::MT_CRITICAL, "The ship maneuvers carefully as it settles down on the ground.\n\r");
					for (ActorMap::iterator act = pRoom->Actors().begin(); act != pRoom->Actors().end(); act++)
					{
						// Display a message to the Actor
						(*act).second->Write("%s appears from above and maneuvers in, gently settling down on the ground.\n\r", pShip->m_gsType);
					}
				}


				pShip->m_Land = CPlacement(2, 0, 0);
				pShip->m_ShipState->SetBit(CShip::_LANDED);
				pGalaxy->RemoveSpatialFromSpace(pShip->m_Vnum);

			}
		}
		
		// Remove their speed
		pShip->m_Speed = 0;
		pShip->m_dSpeed = 0;
		// Reset their Heading
		pShip->m_Heading->x = 0;
		pShip->m_Heading->y = 0;
		pShip->m_Heading->z = 0;
		pShip->m_dHeading->x = 0;
		pShip->m_dHeading->y = 0;
		pShip->m_dHeading->z = 0;
		// Add them to the Landing Pad
		pRoom->AddShip(pShip);
		// Remove them from Space
		pShip->m_gsSector = "";
		pShip->m_ShipState->RemoveBit(CShip::_FLYING);
		

	}
	return true;

}

// Method     :: CmdPilot
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a Pilot to assume control of a ship
// Written    :: 01/02/2006 {OWV}

bool CmdPilot::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they have the necessary Module installed and servicable
	ModuleList* mlList = Ch->CurrentRoom()->GetMod(CModule::MT_HELM);
	for (ModuleList::iterator mod = mlList->begin(); mod !=  mlList->end(); mod++)
	{
		if ((*mod)->m_ncDurability > 0)
			pMod = (*mod);
	}

	if (!pMod)
	{
		if (Ch->CurrentRoom()->GetMod(CModule::MT_HELM)->size() == 0)
		{
			Ch->Write("There are no controls here to maneuver the Ship here!\n\r");
			Ch->Write("You must locate the vessel's Helm.\n\r");
			return true;
		}
		else
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	// Is this helm already manned?
	if (pMod->Plus("Manned", false))
	{
		Ch->Write("The %s is already manned.\n\r", pMod->m_gsName);
		return true;
	}

	// Are they already sitting somewhere else?
	if (Ch->Manned() != 0) 
	{
		Ch->Write("You are already sitting at a console, try standing first.\n\r");
		return true;
	}

	// Are they just sitting
	if (Ch->ActorPositions()->IsSet(CActor::_SITTING))
	{
		Ch->Write("You are already sitting down, try standing first.\n\r");
		return true;
	}

	// Clear checks lets sit them down

	Ch->Write("You assume the piloting controls, and sit down at the %s\n\r", pMod->m_gsName);

	// Inform the room
	Ch->CurrentRoom()->Write(Ch, "%s assumes piloting control, sitting down at the %s\n\r", Ch->Name(), pMod->m_gsName);

	// Set them to the sitting position
	for (int i = 0; i < CActor::_NUMACTORPOS; i++)
	{
		if (Ch->ActorPositions()->IsSet(i))
			Ch->ActorPositions()->RemoveBit(i);
	}
	Ch->ActorPositions()->SetBit(CActor::_SITTING);

	// Change their Short Desc
	gString gsShort;
	gsShort.Format("is sitting at the %s", pMod->m_gsName);
	Ch->SetShortDesc(gsShort);

	// Set the Helm to occupied
	pMod->SetPlus("Manned", Ch->Vnum());

	// Set the player to Manning this ship
	Ch->SetManned(Ch->CurrentRoom()->GetShip());

	return true;
}

// Method     :: CmdMan
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <terminal>
// Return     :: Bool
// Function   :: Allows a Played to man a Terminal
// Written    :: 13/02/2006 {OWV}

bool CmdMan::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdMan::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// [1] Provided an argument
	if (gsValue != "")
	{
		
		// Already manning a Terminal
		if (Ch->MannedPos())
		{
			Ch->Write("You are already manning the %s\n\r", Ch->MannedPos()->m_gsName);
			return true;
		}

		// Check that the Module they supplied to man is a valid module
		ModuleList* mlList = Ch->CurrentRoom()->GetMod(gsValue);
		for (ModuleList::iterator mod = mlList->begin(); mod !=  mlList->end(); mod++)
		{
			if ((*mod)->m_ncDurability > 0)
				pMod = (*mod);
		}

		if (!pMod)
		{
			Ch->Write("There is no %s here to Man!\n\r", gsValue);
		}
		else
		{
			// Is it a Console?
			switch (pMod->m_nType)
			{
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
					break;

				default: 
					{
						Ch->Write("%s is not a Console!\n\r", pMod->m_gsName);
						return true;
					}
					break;
			}
			// We know its a console now
			// Is the Console already Manned?
			if (pMod->m_Manned && pMod->m_Manned->Name() != Ch->Name())
			{
				Ch->Write("%s is already manning that console.\n\r", pMod->m_Manned->Name());
				return true;
			}
			else
			{
				// Console is free lets get them to man it!
				pMod->m_Manned = Ch;							// Set our Actor to Manning the Console
				Ch->SetManned(Ch->CurrentRoom()->GetShip());	// Set the Player to Manning the Console
				Ch->SetMannedPos(pMod);							// Set the pointer to this Module

				// Set them to the sitting position
				for (int i = 0; i < CActor::_NUMACTORPOS; i++)
				{
					if (Ch->ActorPositions()->IsSet(i))			// Remove all other Positions and set
						Ch->ActorPositions()->RemoveBit(i);		// them to sitting
				}
				Ch->ActorPositions()->SetBit(CActor::_SITTING);	// Set to sitting

				// Change their Short Desc
				gString gsShort;
				gsShort.Format("is sitting at the %s", pMod->m_gsName);
				Ch->SetShortDesc(gsShort);						// Set their Short description to sitting
																// at this terminal

				// Message to Player and Room
				Ch->Write("You sit down at the %s, assuming control.\n\r", pMod->m_gsName);

				// Inform the room
				Ch->CurrentRoom()->Write(Ch, "%s mans the %s, sitting down and assuming control.\n\r", Ch->Name(), pMod->m_gsName);

				return true;				

			}
		}
	}
	// [2] Display all Mannable stations and their owner if Manned
	else
	{
		ComponentList* clList = Ch->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT);

		if (clList->size() <= 0)
		{
			Ch->Write("This room is not part of a Control point!\n\r");
			return true;
		}

		int nCount = 0;
		Ch->Write("#400///////////////////////////#401[#700Ship Consoles#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		Ch->Write("#400//                                                                   \\\\#700\n\r");
		for (ComponentList::iterator comp = clList->begin(); comp != clList->end(); comp++)
		for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
		{
			CModule* pMod = (*mod).second;

			switch(pMod->m_nType)
			{
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
						nCount++;
						Ch->Write("#400// [#401 %3d#400]#700 %-35s #400>#401> #700 %-18s #400\\\\#700\n\r", nCount, pMod->m_gsName, pMod->m_Manned ? pMod->m_Manned->Name() : "Unmanned");						
					}
					break;

				
			}

		}
		Ch->Write("#400//                                                  #701 %3d Console%s    #400\\\\#700\n\r", nCount, nCount > 1 ? "s" : " ");
		Ch->Write("#400/////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r\n\r");

		Ch->CurrentRoom()->Write(Ch, "%s glances at the control points' consoles, checking who is manning what.\n\r", Ch->Name());
	}
		
	return true;

}
// Method     :: CmdFire
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
//			  :: <weapon group>
// Return     :: Bool
// Function   :: Allows Ordinance to be launched
// Written    :: 01/02/2006 {OWV}

bool CmdFire::Perform(CActor* Ch, gStringList& CommandLine)
{
    CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);
	ModuleList WeaponList;
	int nNumHits;

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdFire::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanFire())
	{
		// They can't access the radar, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the radar.\n\r");
			Ch->Write("Locate a ship console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}
	// They are in the same room as the Master FCS

	if (!pShip->m_Target)
	{
		Ch->Write("You must specify a Target first.\n\r");
		Ch->Write("Syntax: Target <Target Name>\n\r");
		return true;
	}
	else
	{
		if (pShip->m_nTarget != -1)
		{
			Ch->Write("You have not finished locking onto that target yet!\n\r");
			return true;
		}

		CSpatial* pTarget = **pShip->m_Target;

		if (!pTarget)
			return true;

/*
		// Code for launching a Missile
		COrdinance * pMissile = new COrdinance();
		pMissile->m_gsName.Format("A %s", CShip::szOrdinance[0]);
		pMissile->m_gsDescription = "A Concussion missile";
		CCart* pCart = new CCart(pShip->m_Location->x, pShip->m_Location->y, pShip->m_Location->z);
		pMissile->m_Location = pCart;
		pMissile->m_Target = pShip->m_Target;
		pMissile->m_nSpeed = 400;
		pMissile->m_nDamage = 300;
		pMissile->m_nFuel = 15;
		pMissile->m_gsSector = pShip->m_gsSector;
		pGalaxy->AddSpatial(pMissile);
		pGalaxy->AddSpatialToSpace(pMissile->m_Vnum);

		// Work out the heading of the missile based on the location of the Target
		pMissile->m_dHeading->x = 0;
		pMissile->m_dHeading->y = 0;
		//pMissile->m_Location->Bearing(pTarget->m_Location, CCart::_YZ);
		pMissile->m_dHeading->z = pMissile->m_Location->Bearing(pShip->m_Target->m_Location, CCart::_XZ);
		pMissile->m_Heading->x = pMissile->m_dHeading->x;
		pMissile->m_Heading->y = pMissile->m_dHeading->y;
		pMissile->m_Heading->z = pMissile->m_dHeading->z;

		Ch->Write("[Target Locked] Missile launched at %s", pShip->m_Target->m_gsName);

		gString gsTemp;
		gsTemp.Format("%s has launched a Concussion Missile!\n\r", pShip->m_gsName);
		pShip->NotifySpace(gsTemp);
		return true; */

		bool bRecharged = false;
		bool bInArc = false;
		bool bPowered = false;
		int nNumWeps = 0;

		int nOurArc = pShip->m_Location->Arc(pTarget->m_Location, pShip->m_Heading);
		int nTarArc = 0;
		if (pTarget->m_nType == CSpatial::SO_SHIP)
			nTarArc = pTarget->m_Location->Arc(pShip->m_Location, ((CShip*)pTarget)->m_Heading);

		// They have a Target
		if (gsValue == "")
		{
			// They didnt specify a Weapon Group to fire so we fire anything that can hit
			for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
			{
				// We are only interested in weapons
				if ((*comp)->m_Type == CComponent::CT_WEAPONMOUNT)
				{
					CWeapon* pWep = (CWeapon*)*comp;

					if (pWep->m_Orientation->IsSet(nOurArc))
					{
						
						CModule* pMod = (*comp)->Get(CModule::MT_TURRET_MOUNT);
						CModule* pEle = (*comp)->Get(CModule::MT_MUNITION_ELEVATOR);

						if (!pMod || !pEle)
							continue;

						// Only lasers
						if (pMod->Plus("WeaponType", false) >= CShip::WT_MISSILE)
							continue;
						
						bInArc = true;

						// Must be powered
						if (!pMod->Powered())
							continue;
						
						if (pEle->Plus("Loading", false) == false)
						{
							bRecharged = true;
							pEle->SetPlus("Loading", true);
							nNumWeps++;
						}
								
						WeaponList.push_back(pMod);
						bPowered = true;
					}
				}
			}

			// Arc must come first as the if check above will only loop through weapons that are
			// orientated towards our target. If we have no weapons then we don't actualy even check
			// if weapons are powered up.
			if (!bInArc)
			{
				Ch->Write("You have no lasers orientated towards %s!\n\r", CShip::szArc[nOurArc]);
				return true;
			}

			if (!bPowered)
			{
				Ch->Write("You have no powered up lasers!\n\r");
				return true;
			}

			if (!bRecharged)
			{
				Ch->Write("Your weapon systems are still recharging!\n\r");
				return true;
			}

			Ch->CurrentRoom()->Write(Ch, "%s works the Fire Control system, opening fire!\n\r", Ch->Name());

			nNumHits = pShip->Fire(WeaponList, pTarget, nTarArc);

			Ch->Write("You open fire on %s with %d lasers.\n\r", pTarget->m_gsName, nNumWeps);

		}
		else
		{
			// They want to fire a Specific Weapon group
			if (atoi(gsValue) > pShip->m_Weapons.size() || atoi(gsValue) <= 0)
			{
				Ch->Write("#400<#401 %d#400>#700 is not a valid Weapon Group Index.\n\r", atoi(gsValue));
				Ch->Write("Type#400:#701 Fcs to view all Weapon Groups.#700\n\r");
				return true;
			}
			else
			{
				CWeaponGroup* pWep = pShip->m_Weapons.at(atoi(gsValue)-1);
				
				if (pWep)
				{
					for (IntegerList::iterator it = pWep->m_Weapons.begin(); it != pWep->m_Weapons.end(); it++)
					{
						CModule* pMod = pShip->GetModule(*it);						// Get the Module
						CWeapon* pCom = (CWeapon*)pShip->GetModComp(pMod);			// Get the Component
						CModule* pMun = pCom->Get(CModule::MT_MUNITION_ELEVATOR);	// Get the Munition Elevator

						if (pWep->m_Orientation->IsSet(nOurArc))
						{
							
							// Make sure we have valid modules
							if (!pMod || !pMun)
								continue;

							if (!pMod->Powered() || pMun->Powered())
								continue;
							
							bInArc = true;

							if (pMun->Plus("Loading", false) == false)
							{
								bRecharged = true;
								pMun->SetPlus("Loading", true);
							}

							WeaponList.push_back(pMod);
							bPowered = true;
							nNumWeps++;

						}
					}
					
					// As above: Arc must come first as the if check above will only loop through weapons that are
					// orientated towards our target. If we have no weapons then we don't actualy even check
					// if weapons are powered up.
					if (!bInArc)
					{
						Ch->Write("The Weapon Group has no lasers orientated towards %s.\n\r", CShip::szArc[nOurArc]);
						return true;
					}

					if (!bPowered)
					{
						Ch->Write("You have no powered up lasers in that Group!\n\r");
						return true;
					}

					if (!bRecharged)
					{
						Ch->Write("Your weapon group is still recharging!\n\r");
						return true;
					}

					Ch->CurrentRoom()->Write(Ch, "%s works the Fire Control system, opening fire!\n\r", Ch->Name());			

					nNumHits = pShip->Fire(WeaponList, pTarget, nTarArc);

					Ch->Write("You open fire on %s with your %s", pTarget->m_gsName, pWep->m_gsName);

				}
			}



		}
		


	}

	return true;

}


// Method     :: CmdFcs
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Shows a display of the Ships Fire Control System for Weapon Groups
// Written    :: 06/02/2006 {OWV}

bool CmdFcs::Perform(CActor* Ch, gStringList& CommandLine)
{
    CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Function
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value2

	bool bPowered = false;
	gString gsName;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdFcs::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanFire())
	{
		// They can't access the radar, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the radar.\n\r");
			Ch->Write("Locate a ship console and man it.\n\r");
			return true;
		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}

		bPowered = Ch->MannedPos()->Powered();
		gsName = Ch->MannedPos()->m_gsName;
	}

	// [1] - Showing the Weapon Groups
	if (gsValue == "" || gsValue == "show")
	{
		// They are in the same room as the Master FCS
		Ch->Write("#400/////////////////////////////////#401[#700 Master FCS#401 ]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		Ch->Write("#400// #700%20s#400:#700 %8s#400                                              \\\\#700\n\r", gsName, bPowered ? "#200<#201Online#200> " : "#100<#101Offline#100>");
		Ch->Write("#400//        #700Weapon Groups#400:#700 %3d#400                                                    \\\\#700\n\r", pShip->m_Weapons.size());
		Ch->Write("#400/////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		if (pShip->m_Weapons.size() > 0)
		{
			int nCount = 1;
			for (WeaponGroupList::iterator wep = pShip->m_Weapons.begin(); wep != pShip->m_Weapons.end(); wep++)
			{
				
				gString gsArcs = "#400[#701";
				bool bNone = true;

				for (int i = 0; i < CShip::A_MAX; i++)
				{
					if ((*wep)->m_Orientation->IsSet(i))
					{
						bNone = false;
						gString gsArc = CShip::szArc[i];
						gsArc.TrimRight(1);
						gsArcs += (gsArc + " ");
					}
						
				}

				if (bNone)
					gsArcs += "No Arc";

				gsArcs += "#400]";

				Ch->Write("#400// [#401 %2d#400]#700 %-35s %-26s                     \\\\\n\r", nCount, (*wep)->m_gsName, gsArcs);
				
				if ((*wep)->m_Weapons.size() <= 0)
					Ch->Write("#400//#700 No Weapons in group                                                          #400\\\\#700\n\r");


				// Group weapons up
				gStringSmap* gsGroups = new gStringSmap();

				for (IntegerList::iterator it = (*wep)->m_Weapons.begin(); it != (*wep)->m_Weapons.end(); it++)
				{
					if (gsGroups->find(pShip->GetModule(*it)->m_gsName) == gsGroups->end())
					{
						// This is a new weapon so we add it in
						gsGroups->insert(gStringSmap::value_type(pShip->GetModule(*it)->m_gsName, 1));			
					}
					else
					{
						// We already have one of these weapons so we increment the number
						gStringSmap::iterator gs = gsGroups->find(pShip->GetModule(*it)->m_gsName);
						(*gs).second++;
					}
				}

				for (gStringSmap::iterator gs = gsGroups->begin(); gs != gsGroups->end(); gs++)
				{
					Ch->Write("#400//#700 %3d #400x#700 %-40s                               #400\\\\#700\n\r", (*gs).second, (*gs).first);
				}
				Ch->Write("#400/////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
				nCount++;
			}

		}
		else
		{
			Ch->Write("#400//#700 No weapon groups defined#400                                                     \\\\#700\n\r");
			Ch->Write("#400/////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		}
	}

	Ch->CurrentRoom()->Write(Ch, "%s studies the Weapons Grouping readout on the Master FCS!\n\r", Ch->Name());
	return true;
}

// Method     :: CmdTarget
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Ship to Target>
//			  :: <None>
// Return     :: Bool
// Function   :: Targets an enemy vessel to facilitate firing upon them
// Written    :: 08/02/2006 {OWV}

bool CmdTarget::Perform(CActor* Ch, gStringList& CommandLine)
{
    CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	gString gsTarget = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);

	CModule* pMod = NULL;

	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdTarget::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanFire())
	{
		// They can't access the radar, now we give a message why
		if (!Ch->MannedPos())
		{
			Ch->Write("You are not manning a console, you must do this before accessing the radar.\n\r");
			Ch->Write("Locate a ship console and man it.\n\r");
			return true;

		}
		else if (Ch->MannedPos()->m_ncDurability <= 0)
		{
			Ch->Write("These controls are too badly damaged to be used!\n\r");
			Ch->Write("You must either repair the controls here or locate an alternative.\n\r");
			return true;
		}
	}

	// They are in the same room as the Master FCS, now we try to locate the Target

	if (gsTarget == "")
	{
		if (!Ch->IsNPC())
		{
			Ch->Write("You must enter a Target for the Targetting Computer to calculate.\n\r");
			Ch->Write("Syntax: Target <Name of Target>\n\r");
		}
		else
		{
			Ch->Report(pShip->m_Commander, "Commander, which vessel do you wish to target?");
		}
		return true;
	}
	else if (gsTarget == "none")
	{
		if (!Ch->IsNPC())
			Ch->Write("Target set to none.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Target lock broken, Commander.");

		CSpatial* pTarget = **pShip->m_Target;
		if (!pTarget)
			return true;

		pTarget->Write(CShip::MT_SENSORS, "#101%s has stopped targetting you.#700", pShip->m_gsName);
		pShip->m_Target = NULL;
		return true;
	}

	CSpatial* pTarget = pGalaxy->GetSpa(gsTarget);
	
	if (!pTarget || !pShip->Contains(pTarget))
	{
		if (!Ch->IsNPC())
			Ch->Write("The Targetting computer is unable to locate the specified target.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Commander, that target is not registering on our sensors.");
		return true;
	}
	else
	{
		if (!Ch->IsNPC())
			Ch->Write("Targetting %s....\n\r", pTarget->m_gsName);
		else
			Ch->Report(pShip->m_Commander, "Affirmative Commander, Target %s registered. Locking on now.", pTarget->m_gsName);

		pShip->m_nTarget = 3;
		pShip->m_Target = pTarget->m_Vnum;

		Ch->CurrentRoom()->Write(Ch, "%s manipulates the Targetting controls of the FCS, attempting to lock onto a target!\n\r", Ch->Name());
		return true;
	}

	return true;
}
