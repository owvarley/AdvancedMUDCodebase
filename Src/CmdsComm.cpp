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

// File     :: CmdsComm.cpp
// Header   :: CmdsComm.h
// Function :: Holds the implementations for commands that belong in the Communication category

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "CmdsComm.h"
#include "Player.h"
#include "GameWorld.h"
#include "Room.h"
#include "Emotions.h"
#include "../gTools/Log.h"

// Macro to define implementations
IMPLEMENT_CLASS(CmdSay);		// In room messaging
IMPLEMENT_CLASS(CmdEmote);		// In room emote passing
IMPLEMENT_CLASS(CmdOOC);		// Global message channel
IMPLEMENT_CLASS(CmdTransmit);	// Ship single channel communication
IMPLEMENT_CLASS(CmdBroadcast);	// Ship multi channel communication
IMPLEMENT_CLASS(CmdTightbeam);	// Ship to ship, private hail communication
IMPLEMENT_CLASS(CmdShipNet);	// Internal ship communication

// Method     :: CmdSay
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Message to send>
// Return     :: Bool
// Function   :: Used for local communication, within the same room.
// Written    :: Original {GMN}, Updated 21/5/05 {OWV}
// Update     :: Changed the functionality of say so that it iterates through players now
//			  :: instead of using the event system.

bool CmdSay::Perform(CActor* Ch, gStringList& CommandLine)
{
  ActorMap::iterator pos;

  CActor* pA;
  gString csSay = "";

  CTools* Tools = CGameObjects::Get().Tools();

  if ( CommandLine.empty() )
  {
	  Ch->Write("Say what?\n\r");
	  return true;
  }

  if (Ch->CurrentRoom())
	Ch->CurrentRoom()->Write(Ch, "%s says #601\"#700#600%s#700#601\"#700\n\r",
						Ch->Name(), Tools->ParseStringList(CommandLine, 0, false, true));

  Ch->Write("You say #601\"#700#600%s#700#601\"#700\n\r", Tools->ParseStringList(CommandLine, 0, false, true));

  return true;
}

// Method     :: CmdEmote
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Emotion to display>
// Return     :: Bool
// Function   :: Used for carry out emotes
// Written    :: 1/6/05 {OWV}

bool CmdEmote::Perform(CActor* Ch, gStringList& CommandLine)
{
  ActorMap::iterator pos;

  CActor* pA;
  gString emote = "";

  CTools* Tools = CGameObjects::Get().Tools();

  if ( CommandLine.empty() )
  {
	  Ch->Write("Emote what?\n\rSyntax: Emote <pose of character>\n\r");
	  return true;
  }

  if (Ch->CurrentRoom())
	  Ch->CurrentRoom()->Write("%s %s\n\r",
						Ch->Name(),
						Tools->ParseStringList(CommandLine, 0, false, true)); 

  return true;
}

// Method     :: CmdOOC
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Global message>
// Return     :: Bool
// Function   :: Used for global communication
// Written    :: 1/6/05 {OWV}

bool CmdOOC::Perform(CActor* Ch, gStringList& CommandLine)
{
	ActorMap::iterator pos;

	CPlayer* pP = NULL;

	CTools* Tools = CGameObjects::Get().Tools();

	if (Ch)
	{

		if ( CommandLine.empty() )
		{
			Ch->Write("OOC what?\n\rSyntax: OOC <global message to broadcast>\n\r");
			return true;
		}

		for (pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
		{
			try 
			{
				if ( (pP = (CPlayer*)((*pos).second)) != NULL )
				{
					CPlayer* Ph = (CPlayer*)Ch;
					pP->Write("[#301OOC#700] %s: #301%s#700\n\r", Ph->User()->Name(), Tools->ParseStringList(CommandLine, 0, false, true));
				}

				if ( Ch->HomeWorld()->Players().size() == 0 )
					break;
			}
			catch (...) {break;}
		}

	}

	return true;

}

// Method     :: CmdTransmit
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Message to pass>
// Return     :: Bool
// Function   :: Sends a message using a ship's communication system
//			  :: Tranmissions are sent on open channels and are received
//			  :: by any other ships that have the frequency open.
// Written    :: 15/8/05 {OWV}

bool CmdTransmit::Perform(CActor* Ch, gStringList& CommandLine)
{

	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CTools* Tools = CGameObjects::Get().Tools();
	gString gsMessage;
	gString gsOriginal;

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

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanComm())
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

	// We need to get the Comms Array
	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	if (mlMods->size() == 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("This Ship has no Communications Array\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, We have no Comms Array.");

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
			pMod = (*mod);
			pComms = pMod->m_Comms;
		}
	}

	if (!pMod)
	{
		if (!Ch->IsNPC())
			Ch->Write("You must powerup your Communications Array first.\n\r");
		else 
			Ch->Report(pShip->m_Commander, "Sir, Our Comms Array is not powered up.");
		return true;
	}

	// No Communications Array
	if (!pComms)
	{
		if (!Ch->IsNPC())
			Ch->Write("You do not have a Communications Array installed.\n\r");
		else 
			Ch->Report(pShip->m_Commander, "Sir, We have no Comms Array installed.");
		return true;
	}


	// Communications Array is too badly damaged
	if (pMod->m_ncDurability <= 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("Your Communications Array is too badly damaged to transmit messages!\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, Our Comms Array is unserviceable.");
		return true;
	}

	// No Channel is open
	if (pComms->m_Open.size() <= 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("You must first open a frequency to transmit your message on.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, We have no open frequencies to transmit on.");
		return true;
	}

	if (CommandLine.empty())
	{
		if (!Ch->IsNPC())
			Ch->Write("You must enter a message to transmit.\n\r");
		else
			Ch->Report(pShip->m_Commander, "What would you like me to transmit, Sir?");

		return true;
	}
	else
	{
		gsMessage = Tools->ParseStringList(CommandLine, 0, false, true);
		gsOriginal = gsMessage;
		for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
		for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
		{
			CSpatial* pSpatial = ***spa;

			if (!pSpatial)
			{
				(*pos).second.erase(spa);
				continue;
			}

			// Is this our Ship?
			if (pSpatial == pShip)
				continue;

			if (pSpatial->m_nType == CSpatial::SO_SHIP)
			{
				CShip* pTShip = (CShip*)pSpatial;
				CModule* pMod2 = NULL;

				mlMods = pTShip->Get(CModule::MT_COMMUNICATIONS);
				for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
				{
					pMod2 = (*mod);
					// Found our powered one
					if (pMod2->Powered())
						break;
				}

				if (!pMod2 || (pMod2 && !pMod2->m_Comms))
					continue;

				
				CMComms* pTComms = pMod2->m_Comms;

				// (1) :: Distance and range check
				// Check if they are within range
				if (pShip->m_Location->Distance(pTShip->m_Location) > (pMod->Plus("Range", true)+pMod2->Plus("Range", true)))
					continue;

				// (2) :: Is their Comms Array online
				if (!pMod2->Powered())
					continue;

				// (3) :: Frequency check
				bool bFound = pTShip->IsOpen(pComms->m_Open.at(pComms->m_nOpen));

				// (4) :: Jamming check
				bool bJammed = pTShip->IsJammed(pComms->m_Open.at(pComms->m_nOpen));

				// (5) :: Encryption check
				bool bEncrypted = pTComms->IsEncrypted(pComms->m_Open.at(pComms->m_nOpen));

				bool bDecrypt = false;
				
				// Skip the ship if its the transmitting ship

				// They do not have the channel open they wont receive the message
				// but they may be snooping the channel and hence will be informed 
				// of the message being sent

				// (6) :: Snooping check
				if (!bFound)
				{
					if (pTShip->IsSnooped(pComms->m_Open.at(pComms->m_nOpen)))
					{
						pTShip->Write(CShip::MT_COMMS, "The Communications Array indicates it has detected a Transmission being broadcast on %3.2fMhz!\n\r", pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency);
						//return true;
					}
				}
				
				// Encryption check
				if (bEncrypted)
				{
					// Does the other ship have this protocol?
					if (pTShip->CanDecrypt(pComms->m_Open.at(pComms->m_nOpen)->m_gsEncryption))
						bDecrypt = true;
				}				

				// Recording check
				if (pTComms->m_Recording != NULL)
				{
					if (pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency == pTComms->m_Recording->m_nFrequency)
					{
						if (!bEncrypted)
							pComms->Record(pComms->m_Open.at(pComms->m_nOpen), gsMessage, pShip->m_gsName);
						else
						{
							gsMessage.Encrypt(10);
							pComms->Record(pComms->m_Open.at(pComms->m_nOpen), gsMessage, pShip->m_gsName);
						}
					}
				}
			
				if (bJammed && !bEncrypted)
					gsMessage.Jamm(10);

				if (bEncrypted && !bJammed && !bDecrypt)
					gsMessage.Encrypt(10);

				if (bEncrypted && bJammed && !bDecrypt)
				{
					gsMessage.Encrypt(10);
					gsMessage.Jamm(10);				
				}
						
				if (((CShip*)pSpatial)->m_nClass == CShip::ST_FIGHTER)
					((CShip*)pSpatial)->Write(CShip::MT_CRITICAL, "#601[#700#602INC MSG #601|#700#602 %3.2f Mhz FRM %s#601] ::#700 %s\n\r", pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency, Ch->Name(), gsMessage);
				else
					((CShip*)pSpatial)->Write(CShip::MT_COMMS, "#601[#700#602INC MSG #601|#700#602 %3.2f Mhz FRM %s#601] ::#700 %s\n\r", pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency, Ch->Name(), gsMessage);

				// Made a Transmission so need to boost their Electromagnetic Signature
				pShip->m_Signature[CSpatial::SI_EM] += (gsMessage.Length() * 100);
			}
		} 
		gString gsMsg;
		gsMsg.Format("#601[#700#602OUT MSG #601|#700#602 %3.2f Mhz#601] ::#700", pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency);

		if (!Ch->IsNPC())
			Ch->Write("%s %s\n\r", gsMsg, gsOriginal);
		else
			pShip->Write(CShip::MT_CRITICAL, "%s %s\n\r", gsMsg, gsOriginal);
	}


	return true;

}

// Method     :: CmdBroadcast
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Message to broadcast>
// Return     :: Bool
// Function   :: Sends a message using a ship's communication system
//			  :: Broadcast messages are sent on all open frequencies
//			  :: of that ship's comms array.
// Written    :: 25/8/05 {OWV}

bool CmdBroadcast::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CTools* Tools = CGameObjects::Get().Tools();
	FreqList::iterator freq;
	FreqList::iterator open;

	gString gsMessage;
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

	// Check they are manning a terminal that has access to the sensors
	if (!Ch->CanComm())
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

	// We need to get the Comms Array
	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	if (mlMods->size() == 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("This Ship has no Communications Array\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, We have no Comms Array.");

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
			pMod = (*mod);
			pComms = pMod->m_Comms;
		}
	}

	if (!pMod)
	{
		if (!Ch->IsNPC())
			Ch->Write("You must powerup your Communications Array first.\n\r");
		else 
			Ch->Report(pShip->m_Commander, "Sir, Our Comms Array is not powered up.");
		return true;
	}

	// No Communications Array
	if (!pComms)
	{
		if (!Ch->IsNPC())
			Ch->Write("You do not have a Communications Array installed.\n\r");
		else 
			Ch->Report(pShip->m_Commander, "Sir, We have no Comms Array installed.");
		return true;
	}


	// Communications Array is too badly damaged
	if (pMod->m_ncDurability <= 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("Your Communications Array is too badly damaged to transmit messages!\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, Our Comms Array is unserviceable.");
		return true;
	}

	// No Channel is open
	if (pComms->m_Open.size() <= 0)
	{
		if (!Ch->IsNPC())
			Ch->Write("You must first open a frequency to transmit your message on.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, We have no open frequencies to transmit on.");
		return true;
	}

	// Only one Channel
	if (pComms->m_Open.size() == 1)
	{
		if (!Ch->IsNPC())
			Ch->Write("A standard transmission would be more be useful when communicating on a single frequency.\n\r");
		else
			Ch->Report(pShip->m_Commander, "Sir, It would be more energy efficient to use a standard transmission for communicating on a single frequency.");

		return true;
	}

	if (CommandLine.empty())
	{
		if (!Ch->IsNPC())
			Ch->Write("You must enter a message to broadcast.\n\r");
		else
			Ch->Report(pShip->m_Commander, "What would you like me to broadcast, Sir?");

		return true;
	}
	else
	{
		// Need to compile a list of all Jammed frequencies
		FreqList fJammed;

		// We will do this by iterating through all ships close enough to be jamming the broadcast
		// we will create a list of all jammed frequencies
		for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
		for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
		{
			CSpatial* pSpatial = ***spa;

			if (!pSpatial)
			{
				(*pos).second.erase(spa);
				continue;
			}

			// Is this our Ship?
			if (pSpatial == pShip)
				continue;

			if (pSpatial->m_nType == CSpatial::SO_SHIP)
			{
				CShip* pTShip = (CShip*)pSpatial;
				CModule* pMod2 = NULL;

				ModuleList* mlMods = pTShip->Get(CModule::MT_COMMUNICATIONS);
				for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
				{
					pMod2 = (*mod);
					// Found our powered one
					if (pMod2->Powered())
						break;
				}

				if (!pMod2 || (pMod2 && !pMod2->m_Comms))
					continue;

				CMComms* pTComms = pMod2->m_Comms;

				// (1) :: Distance and range check
				// Check if they are within range
				if (pShip->m_Location->Distance(pTShip->m_Location) > (pMod->Plus("Range", true)+pMod2->Plus("Range", true)))
					continue;

				// (2) :: Is their Comms Array online
				if (!pMod2->Powered())
					continue;

				// (3) :: Add what they are Jamming to our fJammed list
				if (pTComms->m_Jamming.size() <= 0)
					continue;

				freq = pTComms->m_Jamming.begin();
				CFrequency* pFrom = *freq;
				CFrequency* pTo = *(freq + 1);

				for (float i = pFrom->m_nFrequency; i <= pTo->m_nFrequency; i += 0.01f)
				{
					CFrequency* pFreq = new CFrequency();
					pFreq->m_nFrequency = i;
					fJammed.push_back(pFreq);
				}
			}
		}

		// Send out the Message
		for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
		for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
		{
			CSpatial* pSpatial = ***spa; // * Operator overrided for CSpatialID

			if (!pSpatial)
			{
				(*pos).second.erase(spa);
				continue;
			}

			if (pSpatial->m_nType == CSpatial::SO_SHIP)
			{
				CShip* pTShip = (CShip*)pSpatial;
				CModule* pMod2 = NULL;

				ModuleList* mlMods = pTShip->Get(CModule::MT_COMMUNICATIONS);
				for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
				{
					pMod2 = (*mod);
					// Found our powered one
					if (pMod2->Powered())
						break;
				}

				if (!pMod2 || (pMod2 && !pMod2->m_Comms))
					continue;

				CMComms* pTComms = pMod->m_Comms;

				// Check if they are within range
				if (pShip->m_Location->Distance(pTShip->m_Location) > (pMod->Plus("Range", true)+pMod2->Plus("Range", true)))
					continue;

				// (2) :: Is their Comms Array online
				if (pMod2->Plus("Powered", false))
					continue;

				// (3) :: Frequency check
				bool bFound = false;
				for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
				{
					CFrequency* pFreq = *freq;

					if (pTShip->IsOpen(pFreq))
						bFound = true;
				}

				// (4) :: Jamming check
				bool bUnjammed = true;
				for (open = pTComms->m_Open.begin(); open != pTComms->m_Open.end(); open++)
				{
					// Need to check if there are any unjammed channels open that this
					// broadcast can reach the ship through
					for (freq = fJammed.begin(); freq != fJammed.end(); freq++)
					{
						if ((*open)->m_nFrequency == (*freq)->m_nFrequency)
							bUnjammed = false;

					}

					// We only need one unjammed channel for the message to reach the ship
					// without being jammed
					if (bUnjammed)
						break;

				}

				bool bJammed = !bUnjammed;

				// (5) :: Encryption check
				// PoD :: Should you be able to Encrypt a mass broadcast?
				
				// Skip the ship if its the transmitting ship

				// They do not have the channel open they wont receive the message
				// but they may be snooping the channel and hence will be informed 
				// of the message being sent

				// (6) :: Snooping check
				// We need to see if any of the open channels are within the snooping range

				if (!bFound)
				{
					for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
					{
						CFrequency* pFreq = *freq;
						
						if (pTShip->IsSnooped(pFreq))
						{
							pTShip->Write(CShip::MT_COMMS, "The Communications Array indicates it has detected a wide-band Broadcast!\n\r");
							continue;	// Only want the message once! #BUG# Might not work....
						}
					}
				}
				

				gsMessage = Tools->ParseStringList(CommandLine, 0, false, true);

				// Recording check
				bool bRecorded = false;
				
				for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
				{
					if (pTComms->m_Recording != NULL)
					{
						if (pComms->m_Open.at((*freq)->m_nFrequency == pTComms->m_Recording->m_nFrequency))
						{
							pComms->Record((*freq), gsMessage, pShip->m_gsName);
							bRecorded = true;
						}
					}

					// We only want to record the message once
					if (bRecorded)
						break;
				}
			
				if (bJammed)
					gsMessage.Jamm(10);

				// Fighters receive messages as Critical
				if (((CShip*)pSpatial)->m_nClass == CShip::ST_FIGHTER)
					((CShip*)pSpatial)->Write(CShip::MT_CRITICAL, "#601[#700#602INC BRDCAST #601|#700#602 %3.2f Mhz FRM %s#601] ::#700 %s\n\r ", pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency, Ch->Name(), gsMessage);
				else
					((CShip*)pSpatial)->Write(CShip::MT_COMMS, "#601[#700#602INC BRDCAST #601|#700#602 %3.2f Mhz FRM %s#601] ::#700 %s\n\r ", pComms->m_Open.at(pComms->m_nOpen)->m_nFrequency, Ch->Name(), gsMessage);

				// Made a Transmission so need to boost their Electromagnetic Signature
				// For a broadcast message you can produce a HUGE Em signal if you are using
				// multiple channels
				pShip->m_Signature[CSpatial::SI_EM] += (gsMessage.Length() * (100 * pComms->m_Open.size()));
			}
		} 
	}

	gString gsMsg;
	gsMsg.Format("#601[#700#602OUT BRDCAST #601|#700#602 All Chan#601] ::#700 %s\n\r ", gsMessage);

	if (!Ch->IsNPC())
		Ch->Write("%s\n\r", gsMsg);
	else
		pShip->Write(CShip::MT_CRITICAL, "%s\n\r", gsMsg);

	return true;

}

// Method     :: CmdTightbeam
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Target> <Message>
// Return     :: Bool
// Function   :: Sends a message directly to a ship using laser comms
//			  :: Tightbeam provides a secure method of communication
// Written    :: 

bool CmdTightbeam::Perform(CActor* Ch, gStringList& CommandLine)
{
	return true;
}


// Method     :: CmdShipNet
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Area> <Msg>
// Return     :: Bool
// Function   :: Sends a message using the Internal Ship Comms
//			  :: Area can either be a type of Component within the ship
//			  :: or a a specific component.
// Written    :: 18/02/06 {OWV}

bool CmdShipNet::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsType    = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();	// Remove this from the CommandLine so we can parse the rest
	gString gsMessage = CTools::Get().ParseStringList(CommandLine, 0, false, true);

	ActorMap list = CGameObjects::Get().GameWorld()->Players();
	ActorMap::iterator pla;

	CShip* pShip = NULL;


	// Check they are in a ship
	if (Ch->CurrentRoom()->GetArea()->Ship() == "")
	{
		Ch->Write("You must be in a ship to perform this Command.\n\r");
		return true;
	}

	// Get the ship they are in
	pShip = Ch->CurrentRoom()->GetShip();

	if (!pShip)
	{
		Ch->Write("[Invalid Ship] The Ship assigned to this area is invalid.\n\r");
		g_Log.Log(LOG_ERROR, "[CmdShipNet::>>]  %s has an Invalid area.\n\r", pShip->m_gsName);
		return false;
	}

	// They must be within a Control Point to send Message
	if (!Ch->IsNPC())
	{
		if (Ch->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT)->size() <= 0)
		{
			Ch->Write("#100[#101Unable to Access#100]#700 You must locate a Control point to Transmit on ShipNet.\n\r");
			return true;
		}
	}
	
	// Format the Message String
	gString gsMsg;
	gsMsg.Format("#500[#501ShipNet#500]-[#501%s#500@#501%s#500] - #501\"#500%s#501\"#700\n\r", Ch->Name(), Ch->CurrentRoom()->Name(), gsMessage);

	// Check where they want to send the message 
	if (gsType != "Announcement")
	{	
		// First assume it is a Component Name
		CComponent *pMsg = pShip->GetComponent(gsType);

		if (!pMsg)
		{
			// Its not the name of a Component, is it a Type?
			if (pMsg->Valid(gsType))
			{
				for (pla = list.begin(); pla != list.end(); pla ++)
				{
					CActor* pPlayer = (CPlayer*)((*pla).second);

					if (pPlayer == Ch)
						continue;

					if (pPlayer->CurrentRoom()->GetArea()->Ship() == pShip->m_gsName)
					{
						// Is this player within a component of this type
						if (pPlayer->CurrentRoom()->GetComp(pMsg->GetIndex(gsType))->size() > 0)
							pPlayer->Write(gsMsg);
						
					}
				}

			}
			else
			{
				Ch->Write("#100[#101Invalid Band#100]#700 %s is not a valid Band to send this Message to.\n\r", gsMessage);
				Ch->Write("Valid choices#400:#701 Announcement, <Component Type>, <Component Name>#700\n\r");
				return true;
			}
		}
		else
		{
			// Send it to all Players within this specific Component
			for (pla = list.begin(); pla != list.end(); pla ++)
			{
				CActor* pPlayer = (CPlayer*)((*pla).second);

				if (pPlayer == Ch)
					continue;

				if (pPlayer->CurrentRoom()->GetArea()->Ship() == pShip->m_gsName)
				{
					if (pPlayer->CurrentRoom()->GetComp(gsType))
						pPlayer->Write(gsMsg);
				}
			}
		}
	}
	else
	{
		// Send it to all Players on Ship
		for (pla = list.begin(); pla != list.end(); pla ++)
		{
			CActor* pPlayer = (CPlayer*)((*pla).second);

			if (pPlayer == Ch)
				continue;

			if (pPlayer->CurrentRoom()->GetArea()->Ship() == pShip->m_gsName)
			{
				pPlayer->Write(gsMsg);
			}
		}

	}

	gsType.MakeProper();
	Ch->Write("#500[#501ShipNet#500]-[#501%s#500] - #501\"#500%s#501\"#700\n\r", gsType, gsMessage);

	return true;
}