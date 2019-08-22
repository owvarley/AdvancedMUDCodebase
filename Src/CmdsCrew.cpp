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

// File     :: CmdsCrew.cpp
// Header   :: CmdsCrew.h
// Function :: Holds the implementations for commands that belong in the Crew category

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "CmdsCrew.h"
#include "Space.h"
#include "GameWorld.h"
#include "Room.h"
#include "../gTools/Log.h"

// Macro to define implementations
IMPLEMENT_CLASS(CmdRoster);		// Displays crew of the entire ship
IMPLEMENT_CLASS(CmdCCommand);	// Allows a player to assume command of a ship or crew team
IMPLEMENT_CLASS(CmdCBOrder);	// Passes orders to a bridge crew team
IMPLEMENT_CLASS(CmdCOrder);		// Passes orders to all crew teams
IMPLEMENT_CLASS(CmdCGq);		// Calls the crew of a ship to General Quarters
IMPLEMENT_CLASS(CmdCReport);	// Allows a PC crew team leader to report for duty

// Method     :: CmdRoster
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Displays Crew of this Ship
// Written    :: 14/02/2006 {OWV}

bool CmdRoster::Perform(CActor* Ch, gStringList& CommandLine)
{
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
		g_Log.Log(LOG_ERROR, "[CmdRoster::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// They must be within a Control Point to access this information
	if (Ch->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT)->size() <= 0)
	{
		Ch->Write("#100[#101Unable to Access#100]#700 You must locate the vessels Control point to view the ship's roster.\n\r");
		return true;
	}

	// Display the Roster
	gString gsLine;
	gsLine.Format("#701%s #400>#401>#700 %s", pShip->m_gsDesignation, pShip->m_gsName);
	Ch->Write("#400/////////////////////////////#401[#700Crew Roster#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	Ch->Write("#400// %-81s #400\\\\#700\n\r", gsLine);
	Ch->Write("#400// #700Crew/Berth#400: [#401 %7d#400/#401 %7d#400]   #700Ship status#400: %-16s #400\\\\#700\n\r", pShip->Crew(true, -1), pShip->Crew(false, -1), pShip->m_ShipState->IsSet(CShip::_GQ) ? "#100[#101General Quarters#100]" : "#200[#201Stand Down#200]      ");
	Ch->Write("#400////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	CrewMap::iterator crew = pShip->m_Crew.find(CCrew::CT_WEAPONRY);
	Ch->Write("#400// [#401 1#400]#700 GUNNERY        #400>#401>#700 Num Teams#400:#701 %4d  #700Total Personnel#400:#701 %8d #400\\\\#700\n\r", crew == pShip->m_Crew.end() ? 0 : ((*crew).second).size(), pShip->Crew(true, CCrew::CT_WEAPONRY));
	crew = pShip->m_Crew.find(CCrew::CT_BRIDGECREW);
	Ch->Write("#400// [#401 2#400]#700 BRIDGE CREW    #400>#401>#700 Num Teams#400:#701 %4d  #700Total Personnel#400:#701 %8d #400\\\\#700\n\r", crew == pShip->m_Crew.end() ? 0 : ((*crew).second).size(), pShip->Crew(true, CCrew::CT_BRIDGECREW));
	crew = pShip->m_Crew.find(CCrew::CT_ENGINEERING);
	Ch->Write("#400// [#401 3#400]#700 ENGINEERS      #400>#401>#700 Num Teams#400:#701 %4d  #700Total Personnel#400:#701 %8d #400\\\\#700\n\r", crew == pShip->m_Crew.end() ? 0 : ((*crew).second).size(), pShip->Crew(true, CCrew::CT_ENGINEERING));
	crew = pShip->m_Crew.find(CCrew::CT_PILOTS);
	Ch->Write("#400// [#401 4#400]#700 FIGHTER PILOTS #400>#401>#700 Num Teams#400:#701 %4d  #700Total Personnel#400:#701 %8d #400\\\\#700\n\r", crew == pShip->m_Crew.end() ? 0 : ((*crew).second).size(), pShip->Crew(true, CCrew::CT_PILOTS));
	crew = pShip->m_Crew.find(CCrew::CT_TROOPS);
	Ch->Write("#400// [#401 5#400]#700 TROOPS         #400>#401>#700 Num Teams#400:#701 %4d  #700Total Personnel#400:#701 %8d #400\\\\#700\n\r", crew == pShip->m_Crew.end() ? 0 : ((*crew).second).size(), pShip->Crew(true, CCrew::CT_TROOPS));

	if (pShip->m_Crew.size() <= 0)
		Ch->Write("#400////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	else
	{
		for (crew = pShip->m_Crew.begin(); crew != pShip->m_Crew.end(); crew++)
		{
			Ch->Write("#400///////////////////////////#401[#700%-15s#401]#400\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r", CCrew::szTypes[(*crew).first]);
			int nCount = 1;
			for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
			{
				gString gsSkill;
				if ((*it)->m_fSkill <= 5.0)
					gsSkill = "Novice";
				else if ((*it)->m_fSkill <= 7.5 && gsSkill == "")
					gsSkill = "Regular";
				else if ((*it)->m_fSkill <= 10.0 && gsSkill == "")
					gsSkill = "Veteran";
				else
					gsSkill = "Elite";

				CArea* pArea = CGameObjects::Get().GetArea(*(*it)->m_Location);
				CRoom* pRoom = NULL;

				if (pArea)
					pRoom = pArea->GetRoom(*(*it)->m_Location);

				Ch->Write("#400// [#401 %2d#400] #700 %-18s #400>#401>#700 %-18s #400:#700 %7s #400[#401 %4d#400] \\\\#700\n\r", nCount, (*it)->m_gsName, (*it)->m_gsOrder == "" ? CCrew::szStates[(*it)->m_nCrewState] : (*it)->m_gsOrder, gsSkill, (*it)->m_ncCompliment);
				Ch->Write("#400//      #700CO#400:#701 %-18s #700Station#400:#701 %-26s   #400\\\\#700\n\r", (*it)->m_Leader ? (*it)->m_Leader->Name() : "No Leader", pRoom ? pRoom->Name() : "No Station");
				nCount++;
			}
			
		}
		Ch->Write("#400////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");


	}

	return true;
}

// Method     :: CmdCCommand
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Crew Team name>
// Return     :: Bool
// Function   :: Allows a player to assume Command of the Vessel or Crew Team
// Written    :: 16/02/2006 {OWV}

bool CmdCCommand::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Crew Team

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
		g_Log.Log(LOG_ERROR, "[CmdRoster::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// They must be within a Control Point to assume Command
	if (Ch->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT)->size() <= 0)
	{
		Ch->Write("#100[#101Unable to Access#100]#700 You must locate the vessels Control point to Assume Command.\n\r");
		return true;
	}

	// Are they assuming command of the vessel or of a specific Crew Team
	if (gsValue.IsEmpty())
	{

		// The ship already has a commander
		if (pShip->m_Commander && pShip->m_Commander->Name() != Ch->Name())
		{
			Ch->Write("%s is in Command of this vessel!\n\r", pShip->m_Commander->Name());
			return true;
		}
		else if (pShip->m_Commander && pShip->m_Commander->Name() == Ch->Name())
		{
			Ch->Write("You stand down from Command of the Vessel.\n\r");
			Ch->CurrentRoom()->Write(Ch, "%s has stood down from command of the Vessel!\n\r", Ch->Name());
			Ch->Report(NULL, "This is your Commander speaking, I am standing down from Command.");
			pShip->m_Commander = NULL;
			Ch->SetCommand(NULL);		
			return true;
		}
		else 
		{
			// They want to assume command
			pShip->m_Commander = Ch;
			Ch->Write("You assume command of the Vessel.\n\r");
			Ch->CurrentRoom()->Write(Ch, "%s has assumed command of the Vessel!\n\r", Ch->Name());
			Ch->Report(NULL, "This is your Commander speaking, I have the con.");
			Ch->SetCommand(pShip);
			return true;
		}
	}
	else
	{
		// Find the team
		for (CrewMap::iterator crew = pShip->m_Crew.begin(); crew != pShip->m_Crew.end(); crew++)
		{
			for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
			{
				// Did we find a team?
				if ((*it)->m_gsName.HasPrefix(gsValue))
				{
					CCrew* pCrew = (*it);

					if (pCrew->m_Leader)
					{

						if (pCrew->m_Leader->Vnum() == Ch->Vnum())
						{
							Ch->Report(pShip->m_Commander, "I am standing down from crew command.");
							Ch->SetLeader(NULL);
							pCrew->m_Leader = NULL;
							return true;
						}


						if (!pCrew->m_Leader->IsNPC())
						{
							Ch->Write("%s commands that crew, they must step down first.\n\r", pCrew->m_Leader->Name());
							return true;
						}

						// We need to relieve the current team leader of command
						Ch->Report(pCrew->m_Leader, "%s, you are relieved of crew command!\n\r", pCrew->m_Leader->Name());
						pCrew->m_Leader->Report(Ch, "Aye sir, standing down.\n\r");
						CGameObjects::Get().GameWorld()->Remove(pCrew->m_Leader);
						delete pCrew->m_Leader;
						pCrew->m_Leader = Ch;
						Ch->SetLeader(pCrew);
						return true;
					}
					else
					{
						Ch->Report(pShip->m_Commander, "I am assuming command of %s.\n\r", pCrew->m_gsName);
						Ch->SetLeader(pCrew);
						pCrew->m_Leader = Ch;
						return true;
					}

				}

			}
		}
	}

	return true;
}

// Method     :: CmdCBOrder
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Gives Orders to the Bridge Crew
// Written    :: 17/02/2006 {OWV}

bool CmdCBOrder::Perform(CActor* Ch, gStringList& CommandLine)
{
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

	// They cannot reach this command unless they are in Command of a Vessel
	// but for insanity reasons
	if (!Ch->Command())
		return false;

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	
	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to Command its crew.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdCSpeed::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// They are in a ship, now we check if they are within a Control Point
	if (Ch->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT)->size() <= 0)
	{
		Ch->Write("You must locate the Ship's Bridge in order to issue commands.\n\r");
		return true;		
	}
	
	// They are within a Control Point so they can issue the command

	// [1] Is there actually a Bridge Crew onboard the vessel
	CrewMap::iterator crew = pShip->m_Crew.find(CCrew::CT_BRIDGECREW);

	if ((*crew).second.size() <= 0)
	{
		Ch->Write("Your vessel has no Bridge Crew to issue orders to!\n\r");
		return true;
	}

	// Now we need to check that there is a Bridge Crew that is on Duty
	bool bDuty = false;
	CCrew* pBridge = NULL;
	for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
	{
		if ((*it)->m_nCrewState >= CCrew::_ONDUTY)
		{
			pBridge = (*it);
			bDuty = true;
		}
	}

	if (!bDuty || !pBridge)
	{
		Ch->Write("None of your Bridge Crews are currently on Duty.\n\r");
		return true;
	}

	if (!pBridge->m_Leader)
	{
		Ch->Write("You must first elect a CO for this Crew, before you can issue orders.\n\r");
		return true;
	}

	if (!pBridge->m_Ship)
	{
		LOG_SCOPE("CmdsCrew::CmdCBOrder");
		g_Log.Log(LOG_ERROR, "%s within Ship Crew map but not assigned to ship!", pBridge->m_gsName);
		return true;
	}

	// Clear checks now we need to check what the command to be issued is and
	// create the corresponding Event and give it to the Bridge crew. We must also
	// validate the input of the Command. Notice that we do not validate the State
	// of the Ship, we will let this be handled by the Crew.

	// [1] Speed >> Setting the Speed of the Vessel
	if (gsValue == "Speed")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			if (atoi(gsValue2) < 0)
			{
				pBridge->m_Leader->Report(Ch, "Commander, the Sublight engines are unable to cope with Negative Thrust!");
				return true;
			}
			if (atoi(gsValue2) > pShip->TopSpeed(true))
			{
				pBridge->m_Leader->Report(Ch, "Commander, Our Drives can only maintain %d MGLTs maximum!", pShip->TopSpeed(true));
				return true;
			}
			else
			{
				ESpeed* pEvent = new ESpeed();
				pEvent->m_nSpeed = atoi(gsValue2);
				pBridge->ReceiveEvent(*pEvent);
			}
		}

		// Verbalise Command
		Ch->Report(pBridge->m_Leader, "Helm, make your speed %d MGLTs.", atoi(gsValue2));

	}
	// [2] Course >> Setting a Course for the Ship
	if (gsValue == "Course")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			if ((atoi(gsValue2) > 180 || atoi(gsValue2) < -180) || (atoi(gsValue3) > 180 || atoi(gsValue3) < -180))
			{
				pBridge->m_Leader->Report(pShip->m_Commander, "Commander, We are only able to change course from -180 to 180.");
				return true;
			}
			else
			{
				ECourse* pEvent = new ECourse();
				pEvent->m_nBearing = atoi(gsValue2);
				pEvent->m_nMark = atoi(gsValue3);
				pBridge->ReceiveEvent(*pEvent);
			}
		}

		// Verbalise Command
		Ch->Report(pBridge->m_Leader, "Helm, set course %d %d.", atoi(gsValue2), atoi(gsValue3));
	}
	// [3] Roll >> Making the Ship Roll
	if (gsValue == "Roll")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			if (gsValue2 != "Left" && gsValue2 != "Right" && gsValue2 != "Inverted" && gsValue2 != "Upright")
			{
				pBridge->m_Leader->Report(Ch, "Commander, the Ship can only maintain Upright, Left, Inverted or Right roll positions.");
				return true;
			}

			ERoll* pEvent = new ERoll();
			pEvent->m_gsRoll = gsValue2;
			pBridge->ReceiveEvent(*pEvent);
		}

		// Verbalise Command
		Ch->Report(pBridge->m_Leader, "Helm, Roll to %s!", gsValue2);
	}
	// [4] Power >> Modify power setting of ship
	if (gsValue == "Power")
	{
		bool bFound = false;

		// Specified a Sub system
		if (gsValue2 == "Drives" || gsValue2 == "Shields" || gsValue2 == "Capacitors" || gsValue2 == "Systems" ||
		gsValue2 == "Repulsors" || gsValue2 == "Gravwells" || gsValue2 == "Ecm" || gsValue2 == "Weapons")
		{
			bFound = true;
		}

		if (!bFound && atoi(gsValue2) <= 0 && pBridge->m_Leader->IsNPC())
		{
			pBridge->m_Leader->Report(Ch, "Commander, We can Power Drives, Shields, Capacitors, Systems, Repulsors, Gravwells, Ecm or Weapons. %s isn't one of those!", gsValue2);
			return true;
		}
		else if (atoi(gsValue2) <= 0)
		{
			int nPower;

			if (gsValue2 == "Drives")
				nPower = CShip::PT_DRIVE;
			else if (gsValue2 == "Shields")
				nPower = CShip::PT_SHIELD;
			else if (gsValue2 == "Capacitors")
				nPower = CShip::PT_CAPACITOR;
			else if (gsValue2 == "Systems")
				nPower = CShip::PT_SYSTEMS;
			else if (gsValue2 == "Repulsors")
				nPower = CShip::PT_REPULSOR;
			else if (gsValue2 == "Gravwells")
				nPower = CShip::PT_GRAVWELL;
			else if (gsValue2 == "Ecm")
				nPower = CShip::PT_ECM;
			else if (gsValue2 == "Weapons")
				nPower = CShip::PT_WEAPON;

			Ch->Report(pBridge->m_Leader, "Systems, power %s %s", pShip->m_nPower[nPower] == 1 ? "down" : "up", gsValue2);
		}

		// Specified a Module Index
		int nIndex = 1;
		bool bPowered = false;
		gString gsName = "";
		for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			for (ModuleMap::iterator map = (*comp)->m_Modules.begin(); map != (*comp)->m_Modules.end(); map++)
			{
				if (nIndex != atoi(gsValue2))
				{
					nIndex++;
					continue;
				}
				else
				{
					bFound = true;
					bPowered = (*map).second->Powered();
					gsName = (*map).second->m_gsName;
					nIndex++;
					break;
				}
			}
		}

		if (!bFound && pBridge->m_Leader->IsNPC())
		{
			pBridge->m_Leader->Report(Ch, "Commander, %s does not correspond to a valid Module!", gsValue2);
			return true;
		}
		else if (atoi(gsValue2) > 0)
		{
			Ch->Report(pBridge->m_Leader, "Systems, Power %s %s", bPowered ? "Down" : "Up", gsName);
		}

		if (pBridge->m_Leader->IsNPC())
		{
			EPower* pEvent = new EPower();
			pEvent->m_gsSystem = gsValue2;
			pBridge->ReceiveEvent(*pEvent);
		}
	}
	// [5] Comm >> Modify the Communications settings
	if (gsValue == "Comm")
	{
		// All validation for this Command is carried out by CmdComm
		// we just need to create the correct message to report and handle
		// function validation

		EComm* pEvent = new EComm();
		
		if (gsValue2 == "open")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Open Channel %3.2f", atof(gsValue3));
			pEvent->m_gsFunction = "open";
			pEvent->m_gsValue    = gsValue3;
		}
		else if (gsValue2 == "close")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Close Channel %3.2f", atof(gsValue3));
			pEvent->m_gsFunction = "close";
			pEvent->m_gsValue    = gsValue3;
		}
		else if (gsValue2 == "encrypt")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Encrypt %3.2f using Protocol %s", atof(gsValue3), gsValue4);
			pEvent->m_gsFunction = "encrypt";
			pEvent->m_gsValue    = gsValue3;
			pEvent->m_gsValue2   = gsValue4;

		}
		else if (gsValue2 == "decrypt")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Remove encryption of Channel %3.2f", atof(gsValue3));
			pEvent->m_gsFunction = "decrypt";
			pEvent->m_gsValue    = gsValue3;		
		}
		else if (gsValue2 == "transmit")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Send all transmissions using Channel %3.2f", atof(gsValue3));
			pEvent->m_gsFunction = "transmit";
			pEvent->m_gsValue    = gsValue3;
		}
		else if (gsValue2 == "jamm")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Jamm all communications between %3.2f and %3.2fmhz", atof(gsValue3), atof(gsValue4));
			pEvent->m_gsFunction = "jamm";
			pEvent->m_gsValue    = gsValue3;
			pEvent->m_gsValue2   = gsValue4;
		}
		else if (gsValue2 == "unjamm")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Cease jamming");
			pEvent->m_gsFunction = "unjamm";
		}
		else if (gsValue2 == "snoop")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Engage snooping of Channels %3.2f through %3.2fmhz", atof(gsValue3), atof(gsValue4));
			pEvent->m_gsFunction = "snoop";
			pEvent->m_gsValue    = gsValue3;
			pEvent->m_gsValue2   = gsValue4;

		}
		else if (gsValue2 == "unsnoop")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Cease snooping");
			pEvent->m_gsFunction = "unsnoop";
		}
		else if (gsValue2 == "load")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Load Encryption Protocol %s into Memory", gsValue3);
			pEvent->m_gsFunction = "load";
			pEvent->m_gsValue    = gsValue3;
		}
		else if (gsValue2 == "unload")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Unload Encryption Protocol %s", gsValue3);
			pEvent->m_gsFunction = "unload";
			pEvent->m_gsValue    = gsValue3;
		}
		else if (gsValue2 == "record")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Begin recording Channel %3.2f", atof(gsValue3));
			pEvent->m_gsFunction = "record";
			pEvent->m_gsValue    = gsValue3;
		}
		else if (gsValue2 == "stop")
		{
			Ch->Report(pBridge->m_Leader, "Communications, Cease recording");
			pEvent->m_gsFunction = "stop";
		}
		else
		{
			if (pBridge->m_Leader->IsNPC())
			{
				pBridge->m_Leader->Report(Ch, "Commander, I don't understand your order!");
				delete pEvent;
				return true;
			}
		}

		if (pBridge->m_Leader->IsNPC())
			pBridge->ReceiveEvent(*pEvent);
		else
			delete pEvent;

	}
	// [6] Sweep >> Order a Sweep of the system to commence
	if (gsValue == "Sweep")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			ESweep* pEvent = new ESweep();

			pEvent->m_gsRadome = gsValue2;

			pBridge->ReceiveEvent(*pEvent);
		}

		Ch->Report(pBridge->m_Leader, "Sensors, Commence full systems sweep.");

	}
	// [7] Transmit >> Order a Message to be Transmitted
	if (gsValue == "Transmit")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			ETransmit* pEvent = new ETransmit();

			pEvent->m_gsMessage = gsValue2;

			pBridge->ReceiveEvent(*pEvent);
		}

		Ch->Report(pBridge->m_Leader, "Comms, Transmit %s", gsValue2);


	}
	// [8] Broadcast >> Order a Message to be Broadcast
	if (gsValue == "Broadcast")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			EBroadcast* pEvent = new EBroadcast();

			pEvent->m_gsMessage = gsValue2;

			pBridge->ReceiveEvent(*pEvent);

		}

		Ch->Report(pBridge->m_Leader, "Comms, Broadcast all channels: %s", gsValue2);
	}
	// [9] Launch >> Order the Crew to Launch
	if (gsValue == "Launch")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			ELaunch* pEvent = new ELaunch();
			pBridge->ReceiveEvent(*pEvent);
		}

		Ch->Report(pBridge->m_Leader, "Helm, Commence launch procedures.");

	}
	// [10] Land >> Order the crew to Land
	if (gsValue == "Land")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			ELand* pEvent = new ELand();

			pEvent->m_gsLocation = gsValue2;

			pBridge->ReceiveEvent(*pEvent);
		}
		
		Ch->Report(pBridge->m_Leader, "Helm, Take us into land on %s", gsValue2);
	}
	// [11] Target >> Order the Crew to Target a ship
	if (gsValue == "Target")
	{
		if (pBridge->m_Leader->IsNPC())
		{
			ETarget* pEvent = new ETarget();

			pEvent->m_gsTarget = gsValue2;

			pBridge->ReceiveEvent(*pEvent);
		}

		if (gsValue2 == "none")
			Ch->Report(pBridge->m_Leader, "Gunnery, Disengage target lock.", gsValue2);
		else
			Ch->Report(pBridge->m_Leader, "Gunnery, Target %s!", gsValue2);

	}



	return true;
}

// Method     :: CmdCOrder
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Squad|All> <Order>
// Return     :: Bool
// Function   :: Gives Orders to all Crew/Specific Squad
// Written    :: 17/02/2006 {OWV}

bool CmdCOrder::Perform(CActor* Ch, gStringList& CommandLine)
{
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

	// They cannot reach this command unless they are in Command of a Vessel
	// but for insanity reasons
	if (!Ch->Command())
		return false;

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	
	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to Command its crew.\n\r");
		return true;
	}

	// Get the ship they are in
	CShip* pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdCOrder::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// They are in a ship, now we check if they are within a Control Point
	if (Ch->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT)->size() <= 0)
	{
		Ch->Write("You must locate the Ship's Bridge in order to issue commands.\n\r");
		return true;		
	}
	
	// Is there actually any Crew onboard?	
	if (pShip->m_Crew.size() <= 0)
	{
		Ch->Write("Your vessel has no Crew to issue orders to!\n\r");
		return true;
	}

	CEvent* pEvent = NULL;

	// [1] - Status >> Forces a status report from the Crew
	if (gsValue2 == "Status")
	{
		pEvent = new EStatus();
	}
	// [2] - Report >> Forces a crew to report to a location within the ship


	if (!pEvent)
		return false;

	// Generic Reporting and Event passing
	// [1] Order <all> <Command>
	// [2] Order <crew> <Command>
	if (gsValue == "all")
	{
		for (CrewMap::iterator crew = pShip->m_Crew.begin(); crew != pShip->m_Crew.end(); crew++)
		for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
		{
			CCrew* pCrew = (*it);

			if ((*it)->m_Leader && (*it)->m_Leader->IsNPC())
				pCrew->ReceiveEvent(*pEvent);			
		}

		if (gsValue2 == "Status")
			Ch->Report(NULL, "All Crews, Report in.");
	}
	else
	{
		bool bFound = false;
		for (CrewMap::iterator crew = pShip->m_Crew.begin(); crew != pShip->m_Crew.end(); crew++)
		for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
		{
			CCrew* pCrew = (*it);

			if (pCrew->m_gsName == gsValue)
			{
				bFound = true;

				if ((*it)->m_Leader && (*it)->m_Leader->IsNPC())
					pCrew->ReceiveEvent(*pEvent);
			}
		}

		if (bFound)
		{
			if (gsValue2 == "Status")
				Ch->Report(NULL, "%s, Report!", gsValue);
		}
		else
		{
			Ch->Write("#100[#101Invalid Crew#100]#700 %s is not a valid crew.\n\r", gsValue);
			return true;
		}

	}


	return true;
}

// Method     :: CmdCGq
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Gives the General Quarters order
// Written    :: 17/02/2006 {OWV}

bool CmdCGq::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsMsg = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);

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

	// Are they in Command?
	if (pShip->m_Commander && pShip->m_Commander == Ch)
	{
		// Is the ship on GQ?
		if (pShip->m_ShipState->IsSet(CShip::_GQ))
		{
			if (gsMsg == "")
			{
				pShip->Write(CShip::MT_ENTIRESHIP, "#100Another alarm sounds, followed by an announcement on the ShipNet#700\n\r");
				Ch->ExecuteCommand("ShipNet", "Announcement Stand Down, All Crew Stand down from General Quarters");				
			}
			else
			{
				pShip->Write(CShip::MT_ENTIRESHIP, "#100Another alarm sounds, followed by an announcement on the ShipNet#700\n\r");
				Ch->ExecuteCommand("ShipNet", "Announcement %s", gsMsg);

			}


			for (CrewMap::iterator crew = pShip->m_Crew.begin(); crew != pShip->m_Crew.end(); crew++)
			for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
			{
				if ((*it)->m_nCrewState > CCrew::_ONQRA)
				{
					if ((*it)->m_Leader && (*it)->m_Leader->IsNPC())
					{
						EReport* pEvent = new EReport;
						pEvent->m_Location = (*it)->m_Home;
						(*it)->ReceiveEvent(*pEvent);
						(*it)->m_nCrewState = CCrew::_REPORTING;
					}
				}

			}
			pShip->m_ShipState->RemoveBit(CShip::_GQ);
			return true;

		}
		else
		{
			if (gsMsg == "")
			{
				pShip->Write(CShip::MT_ENTIRESHIP, "#100An alarm sounds, quickly followed by a booming Announcement.#700\n\r");
				Ch->ExecuteCommand("ShipNet", "Announcement General Quarters, General Quarters, this is not a drill");
			}
			else
			{
				pShip->Write(CShip::MT_ENTIRESHIP, "#100An alarm sounds, quickly followed by a booming Announcement.#700\n\r");
				Ch->ExecuteCommand("ShipNet", "Announcement %s", gsMsg);
			}

			// We need to make all off duty Crew Report for duty now
			for (CrewMap::iterator crew = pShip->m_Crew.begin(); crew != pShip->m_Crew.end(); crew++)
				for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
				{
					CCrew* pCrew = (*it);

					if ((*it)->m_Leader && (*it)->m_Leader->IsNPC())
					{
						if (pCrew->m_nCrewState <= CCrew::_ONQRA)
						{
							EReport* pEvent = new EReport;
							pEvent->m_Location = pCrew->m_Location;
							pCrew->ReceiveEvent(*pEvent);
							pCrew->m_nCrewState = CCrew::_REPORTING;
						}
					}
				}

			pShip->m_ShipState->SetBit(CShip::_GQ);
			return true;
		}

	}
	else
	{
		// Again, should never happen...
		Ch->Write("You do not have the authority to order hands to General Quarters.\n\r");
		return true;
	}

	return true;
}

// Method     :: CmdCReport
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Duty, Orders>
// Return     :: Bool
// Function   :: Allows a Crew leader to report for duty, or request orders
// Written    :: 27/03/2006 {OWV}

bool CmdCReport::Perform(CActor* Ch, gStringList& CommandLine)
{
	LOG_SCOPE("CmdCReport::Perform");
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);

	if (!Ch->GetLeader())
	{
		Ch->Write("You do not command a Crew Team.\n\r");
		return true;
	}

	CShip* pShip = Ch->GetLeader()->m_Ship;
	CCrew* pCrew = Ch->GetLeader();

	if (!pShip)
	{
		g_Log.Log(LOG_ERROR, "%s has been assigned to an invalid ship.", pCrew->m_gsName);
		Ch->Write("Your team has not been assigned to a ship.\n\r");
		return true;
	}

	// If there is no value they are just trying to report status
	if (gsValue.IsEmpty())
	{
		Ch->Report(pShip->m_Commander, "%s, %s reporting. Crew %d/%d, %s", pCrew->m_gsName, pCrew->m_Leader->Name(), pCrew->m_ncCompliment, pCrew->m_nmCompliment, CCrew::szStates[pCrew->m_nCrewState]);
		return true;
	}

	// Reporting for Duty
	if (gsValue == "Duty")
	{
		// They are reporting for duty
		if (pCrew->m_nCrewState < CCrew::_ONDUTY)
		{
			Ch->Report(pShip->m_Commander, "%s reporting for duty, Commander!\n\r", pCrew->m_gsName);
			pCrew->m_nCrewState = CCrew::_ONDUTY;
			return true;
		}
		else
		{
			Ch->Report(pShip->m_Commander, "%s going off duty, Commander.\n\r", pCrew->m_gsName);
			pCrew->m_nCrewState = CCrew::_OFFDUTY;
			return true;
		}
	}

	return true;

}