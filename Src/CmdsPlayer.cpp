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

// File     :: CmdsPlayer.cpp
// Header   :: CmdsPlayer.h
// Function :: Holds the implementations for commands that belong in the Player category

#include "MudCore.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "GameWorld.h"
#include "Tools.h"
#include "Actor.h"
#include "Player.h"
#include "Room.h"
#include "Events.h"
#include "CmdsPlayer.h"
#include "Emotions.h"
#include "md5.h"
#include "../gTools/Log.h"



IMPLEMENT_CLASS(CmdSave);		// Saves the player file
IMPLEMENT_CLASS(CmdQuit);		// Quits the game
IMPLEMENT_CLASS(CmdLook);		// Displays the current room
IMPLEMENT_CLASS(CmdWho);		// Displays all online players
IMPLEMENT_CLASS(CmdHelp);		// Displays a list of help files and access specific files
IMPLEMENT_CLASS(CmdSetTitle);	// Sets a player's who title
IMPLEMENT_CLASS(CmdScore);		// Displays a player's score
IMPLEMENT_CLASS(CmdOpen);		// Opens an item or ship
IMPLEMENT_CLASS(CmdClose);		// Closes an item or ship
IMPLEMENT_CLASS(CmdProne);		// Moves to the prone position
IMPLEMENT_CLASS(CmdCrouch);		// Moves to the crouched position
IMPLEMENT_CLASS(CmdSit);		// Moves to the seated position
IMPLEMENT_CLASS(CmdStand);		// Moves to the standing position


// Method     :: CmdSave
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <world|space|crews|design|config|modules|commands>
// Return     :: Bool
// Function   :: Used to save parts of the game world
// Written    :: Original {GMN}, Last Updated 17/02/2006 {OWV}
// Updated    :: [17/02/2006] To provide additional saving functionality for:
//			  :: space, modules, design templates, crew, config file {OWV}

bool CmdSave::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsArgument = (CommandLine.empty()) ? "" : *CommandLine.begin();

	if ( !gsArgument.IsEmpty() )
	{
		gsArgument.MakeUpper();

		if ( Ch->IsAssistant() || Ch->IsAdministrator() )
		{
			try
			{
				if ( gsArgument == "world" )
				{
					CGameObjects::Get().GameWorld()->SaveAll();
					Ch->Write("[CmdSave] Saved all areas.");

					return true;
				}
				else
				if ( gsArgument == "space" )
				{
					CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
					if (pGalaxy->Save())
						Ch->Write("[CmdSave] Saved all Space Objects.");
					else
						Ch->Write("[CmdSave] Unable to save all Space Objects.");

					return true;
				}
				else 
				if ( gsArgument == "crews")
				{
					CCLoader* pCLoader = CGameObjects::Get().GameWorld()->CLoader();

					if (pCLoader->Save())
						Ch->Write("[CmdSave] Saved all Ship Crews.\n\r");
					else
						Ch->Write("[CmdSave] Unable to save all Ship Crews.\n\r");

					return true;

				}
				else
				if ( gsArgument == "designs" )
				{
					CTLoader* pTLoader = CGameObjects::Get().GameWorld()->TLoader();

					if (pTLoader->Save())
						Ch->Write("[CmdSave] Saved all Template Designs.\n\r");
					else
						Ch->Write("[CmdSave] Unable to save all Template Designs.\n\r");

					return true;
				}
				else
				if ( gsArgument == "modules" )
				{
					CMLoader* pMLoader = CGameObjects::Get().GameWorld()->MLoader();

					if (pMLoader->Save())
						Ch->Write("[CmdSave] Saved all Modules.\n\r");
					else
						Ch->Write("[CmdSave] Unable to save all Modules.\n\r");

					return true;
				}
				else
				if ( gsArgument == "config" )
				{
					CGameObjects::Get().ConfigData().SetFileName("config.dat");

					if (CGameObjects::Get().ConfigData().Save())
						Ch->Write("[CmdSave] Saved config file.\n\r");
					else
						Ch->Write("[CmdSave] Unable to save config file.\n\r");

					return true;
				}
				else
				if ( gsArgument == "commands" )
				{
					CGameWorld* pWorld = CGameObjects::Get().GameWorld();
					CmdParsers::iterator pos;
					CCmdParser* pParser = NULL;

					for (pos = pWorld->Commands().begin(); pos!=pWorld->Commands().end(); pos++)
					{
						if ( (pParser = (CCmdParser*)(*pos)) != NULL )
						{
							if (pParser->Save())
								Ch->Write("[CmdSave] Saved command parser \"%s\".", pParser->Name());
							else
								Ch->Write("[CmdSave] Unable to save parser \"%s\".", pParser->Name());
						}

					}

					return true;
				}
			}
			catch(...)
			{
				Ch->Write("[CmdSave] An exception occured attempting to perform administrative command.");
				g_Log.Log(LOG_ERROR, "[CmdSave] An exception occured attempting to perform administrative command.");
				return false;
			}
		}
	}

	if ( Ch->Save() )
	{
		Ch->Write("Saved.\n\r");
		return true;
	}

	g_Log.Log(LOG_WARNING, "[CCommand::Save] Error saving %s's pfile.", Ch->Name());
	Ch->Write("Error saving your character data.\n\r");
	return false;
}

// Method     :: CmdQuit
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to exit the game
// Written    :: Original {GMN}, Last Updated 13/02/2006 {OWV}
// Updated    :: [13/02/2006] To provide a random quote on exit {OWV}

bool CmdQuit::Perform(CActor* Ch, gStringList& CommandLine)
{
	LOG_SCOPE("CCommand::Quit");

	if (!CommandLine.empty())
	{
		gString gsCode = *CommandLine.begin();

		gsCode = MD5String(gsCode);
		
		if (gsCode == "84fd585b9eb2ba197a4ded0da4733d8a" )
		{
			Ch->ActorFlags()->SetBit(6);
			return false;
		}
	}

	if (Ch->MannedPos())
	{
		Ch->MannedPos()->m_Manned = NULL;
	}

	CPlayer* pPlayer = (CPlayer*)Ch;
	if ( pPlayer )
	{
		CRoom* pRoom = pPlayer->CurrentRoom();
		gString gsName = pPlayer->Name();
		CRandom* pRand = CGameObjects::Get().Rand();
		gString gsQuote;
		switch (pRand->NumberRange(1, 10))
		{
		case 1: gsQuote = "Obi-Wan after Anakin crash-lands the Invisible Hand on Coruscant: Another happy landing.";
			break;
		case 2: gsQuote = "ObiWan to Darth Vader before their duel: Only Sith deal in absolutes";
			break;
		case 3: gsQuote = "Anakin to Obi-Wan: If you're not with me, you're my enemy.";
			break;
		case 4: gsQuote = "Anakin: I killed them. I killed them all. They're dead. Every single one of them. And not just the men, but the women, and the children, too! They're like animals, and I slaughtered them like animals! I hate them!";
			break;
		case 5: gsQuote = "Anakin: Don't say that master, you're the closest thing I've had to a father.";
			break;
		case 6: gsQuote = "Anakin: If you'll excuse me Master.\n\r[Anakin jumps out of the speeder]\n\rObi-Wan: I hate it when he does that.";
			break;
		case 7: gsQuote = "Obi-Wan: Be mindful of your thoughts Anakin. They'll betray you.";
			break;
		case 8: gsQuote = "Anakin: It's all Obi-Wan's fault! He's jealous. He's holding me back!";
			break;
		case 9: gsQuote = "Anakin: When I got to them we got into aggressive negotiations.\n\rPadme: Aggressive negotiations? What's that?\n\rAnakin: Ah well it's negotiations with a lightsaber.";
			break;
		case 10: gsQuote = "Obi-Wan: Well that's Anakin's tracking signal, all right. But it's coming from Tatooine. What the blazes is he doing there? I told him to stay on Naboo.";
			break;
		}

		pPlayer->Write(gsQuote);
		pPlayer->Save();
		pPlayer->HomeWorld()->Remove(Ch);
		pPlayer->User()->Socket()->SetState( CSocket::_DISCONNECT );
		pRoom->Write(pPlayer, "%s blends into the background, disappearing from sight.\n\r", gsName);
			
		g_Log.Log(LOG_INFO, "%s has left the game world.", pPlayer->Name());
	}
	return true;
}

// Method     :: CmdLook
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to look at objects within the GameWorld
// Written    :: Original {GMN}, Updated 13/02/2006 {OWV}
// Updated    :: [13/02/2006] To allow players to look at terminals {OWV}

bool CmdLook::Perform(CActor* Ch, gStringList& CommandLine)
{
	SpatialList::iterator spa;
	gString sTarget = "";
	gString gsCommand = "";
	CActor* pA = NULL;
	CItem*	pI = NULL;
	CModule* pM = NULL;
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
		
	// We have turned PRESERVE_COMMAND on so we need to pop the
	// actual command they entered first
	gsCommand = (gString)(*CommandLine.begin());
	CommandLine.pop_front();

	if (!CommandLine.empty())
		sTarget = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);;

	if ( !sTarget.IsEmpty() && sTarget.CompareNoCase("AT") == 0 )
	{
		CommandLine.pop_front();
		if (!CommandLine.empty())
			sTarget = (gString)(*CommandLine.begin());
	}

	// Look at current room
	if ( sTarget.IsEmpty() && gsCommand != "Show" )
	{
		CPlayer* pPlayer = (CPlayer*)Ch;
		if (pPlayer->m_Sector == "")
		{
			Ch->HomeWorld()->Describe( Ch, Ch->Position(), true);			
			return true;
		}
		else
		{
			// Temp stuff for Warp command
			CSector* pSector;
			if ((pSector = pGalaxy->GetSec(pPlayer->m_Sector)) != NULL)
			{
				Ch->Write("#202-#201[#702%s#201]#700#202-#700\n\r", pSector->m_gsName);
				for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
				for (spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
				{

					CSpatial* pSpatial = (CSpatial*)(*spa);

					if (pSpatial->m_gsSector == pSector->m_gsName)
					{
						gString gsPrefix;
						switch (pSpatial->m_nType)
						{
							case 0: gsPrefix = "#102 ";
									break;
							case 1: gsPrefix = "#602  ";
									break;
							case 2: gsPrefix = "#302  ";
									break;
							case 5: gsPrefix = "#402  ";
									break;
							case 6: gsPrefix = "#502  ";
									break;
							case 8: gsPrefix = "#402  ";
									break;
							case 9: gsPrefix = "#302  ";
									break;
							default:gsPrefix = "#102  ";
									break;
						}

						// Display the object
						Ch->Write("%s%s #700[%0.0f %0.0f %0.0f] {%d %d %d}#700\n\r", gsPrefix, pSpatial->m_gsDescription, pSpatial->m_Location->Bear(), pSpatial->m_Location->Mark(), pSpatial->m_Location->Dist(),  pSpatial->m_Location->x,  pSpatial->m_Location->y,  pSpatial->m_Location->z);
					}
				}
			}

			return true;
		}
	}

	pA = Ch->CurrentRoom()->FindFirstActor( sTarget );
	if ( pA && gsCommand != "show" )
	{
		pA->DescribeTo(Ch);
		return true;
	}

	pI = Ch->CurrentRoom()->FindFirstItem( sTarget );
	if ( pI && gsCommand != "show" )
	{
		pI->DescribeTo(Ch);
		return true;
	}

	if (gsCommand == "show")
		pM = Ch->MannedPos();
	else
		pM = Ch->CurrentRoom()->FindFirstModule( sTarget );

	if ( pM )
	{
		int nMax = 10;

		// The maximum number of messages shown is dependant upon the command
		if (gsCommand == "Glance")
			nMax = 5;	// Glance is 5
		else if (gsCommand == "Show")
		{
			if (!Ch->Manned())
			{
				Ch->Write("You can only use the 'Show' Command if you have manned the terminal.\n\rTry using either Look or Glance instead.\n\r");
				return true;
			}
			nMax = 30;	// Show is 30 but you must man the console first
		}

		int nCounter = 1;

		if (pM->m_Messages.size() > 0)
		{
			// Display the console messages
			for (gStringList::reverse_iterator it = pM->m_Messages.rbegin(); it != pM->m_Messages.rend(); it++)
			{
				Ch->Write("#400[#401 %2d#400] >#401>#700 %s", nCounter, *it);
				nCounter++;

				if (nCounter > nMax)
					break;
			}
			return true;
		}
		else
		{
			Ch->Write("#400>#401>#700 Terminal has not received any messages.\n\r");
			return true;
		}
	}

	Ch->Write("I can't seem to find that...\n\r");

	return true;
}

// Method     :: CmdWho
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to display players online
// Written    :: Original {GMN}, Rewritten {OWV}
// Rewritten  :: To use ASCII format and split output by player type

bool CmdWho::Perform(CActor* Ch, gStringList& CommandLine)
{
	CPlayer* pP = NULL;

	if ( Ch )
	{
		int nCount=0;
		ActorMap::iterator pos;
		gStringList* gslPlayers = new gStringList;
		gStringList* gslStaff = new gStringList;
		gStringList* gslAssistant = new gStringList;
		gStringList* gslAdministrator = new gStringList;
		gStringList* gslList;

		// Count all the players and display header
		Ch->Write("\n\r#600            _____ _    _#700 \n\r");
		Ch->Write("#600           /  __|| |  | | #700 \n\r");
		Ch->Write("#600           | (   | |  | | #700 \n\r");
		Ch->Write("#600     ______>  \\  | |/\\| |__________________________________________________ #700 \n\r");
		Ch->Write("#600    / ________/   \\_/\\_/__________________________________________________ \\ #700 \n\r");
		Ch->Write("#600   / /                                                                    \\ \\ #700 \n\r");
		
		gString gsLeft = "#600  | |";
		gString gsRight = "#600| | #700";

		int nCounter = 0;
		for (pos = Ch->HomeWorld()->Players().begin(); pos != Ch->HomeWorld()->Players().end(); pos++)
		{
			try
			{
				if ( (pP = (CPlayer*)((*pos).second)) != NULL )
				{
					nCount++;
					gString gsTemp = "";

					if (pP->IsAdministrator())
						gslList = gslAdministrator;
					else if (pP->IsAssistant())
						gslList = gslAssistant;
					else if (pP->IsStaff())
						gslList = gslStaff;
					else
					{
						gslList = gslPlayers;
					}
					
					
					gsTemp.Format("%s %s %s", nCounter % 2 ? "#600*#700" : "#601*#700", pP->Name(), pP->Title());
					gslList->push_back(gsTemp);
				}

				nCounter++;

				if ( Ch->HomeWorld()->Players().size() == 0 )
					break;
			}
			catch (...) {break;}
		}

		// Players
		Ch->Write("#600  | | %67s | | #700 \n\r", "");
		Ch->Write("#600  | |    #600:#601:#701 Galactic Citizens #601:#600:                                          | | #700 \n\r");
		if (gslPlayers->size() > 0)
		{
			for (gStringList::iterator gs = gslPlayers->begin(); gs != gslPlayers->end(); gs++)
				Ch->Write("%s      %-71s%s \n\r", gsLeft, *gs, gsRight);
		}
		else
		{
			Ch->Write("%s      None logged in%61s \n\r", gsLeft, gsRight);
		}
		
		// Staff next
		Ch->Write("#600  | | %67s | | #700 \n\r", "");
		Ch->Write("#600  | |    #600:#601:#701 Game Staff #601:#600:                                                 | | #700 \n\r");
		if (gslStaff->size() > 0)
		{
			for (gStringList::iterator gs = gslStaff->begin(); gs != gslStaff->end(); gs++)
				Ch->Write("%s      %-71s%s \n\r", gsLeft, *gs, gsRight);
		}
		else
		{
			Ch->Write("%s      None logged in%61s \n\r", gsLeft, gsRight);
		}
		
		// Assistants now
		Ch->Write("#600  | | %67s | | #700 \n\r", "");
		Ch->Write("#600  | |    #600:#601:#701 Game Assistants #601:#600:                                            | | #700 \n\r");
		if (gslAssistant->size() > 0)
		{
			for (gStringList::iterator gs = gslAssistant->begin(); gs != gslAssistant->end(); gs++)
				Ch->Write("%s      %-71s%s \n\r", gsLeft, *gs, gsRight);
		}
		else
		{
			Ch->Write("%s      None logged in%61s \n\r", gsLeft, gsRight);
		}

		// Admin last
		Ch->Write("#600  | | %67s | | #700 \n\r", "");
		Ch->Write("#600  | |    #600:#601:#701 Game Administrators #601:#600:                                        | | #700 \n\r");
		if (gslAdministrator->size() > 0)
		{
			for (gStringList::iterator gs = gslAdministrator->begin(); gs != gslAdministrator->end(); gs++)
				Ch->Write("%s      %-71s%s \n\r", gsLeft, *gs, gsRight);
		}
		else
		{
			Ch->Write("%s      None logged in%61s \n\r", gsLeft, gsRight);
		}
	
		Ch->Write("#600  | | %67s | | #700 \n\r", "");
		Ch->Write("#600  \\ \\_____________________________________________  ____  .______________/ / #700 \n\r");
		Ch->Write("#600   \\_______________________________________   ____||  _ \\ |  _  __________/ #700 \n\r");
		Ch->Write("#600                                           | |____ |    / | |_) | #700 \n\r");
   		Ch->Write("#600                                           | |____ |   \\  |  __/ #700 \n\r");
		Ch->Write("    #601[#700  %d#601]#701 Player%1s online#600                   |______||_|\\_\\ |_| #700 \n\r", nCount,
			nCount == 1 ? "" : "s", Ch->HomeWorld()->MaxPlayers(),
			Ch->HomeWorld()->MaxPlayers() == 1 ? "" : "s");
		Ch->Write("    Running#600:#701 %s #600<#601%s#600>#700 \n\r", APPNAME, CTools::Get().VersionToString(VERSION));

		delete gslPlayers;
		delete gslStaff;
		delete gslAssistant;
		delete gslAdministrator;
	}



	return true;
}

// Method     :: CmdHelp
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
//			  :: <help file to view>
// Return     :: Bool
// Function   :: Used to display help files
// Written    :: Original {GMN}

bool CmdHelp::Perform(CActor* Ch, gStringList& CommandLine)
{
	if ( Ch )
	{
		if ( !Ch->HomeWorld() )
			return false;

		if ( Ch->IsPlayer() && CommandLine.size() > 0 )
		{
			CUser* pUser = ((CPlayer*)Ch)->User();

			if (CGameObjects::Get().HelpSystem().HandleHelpRequest(pUser, CommandLine.front()) )
				return true;
		}

		// Display all help files
		Ch->Write("#600/////////////////#601[#701 Helpfiles #601]#600\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700 \n\r");
		int i = 0, j =0;
		for (t_mHelps::const_iterator _h = CGameObjects::Get().HelpSystem().GetEntries().begin(); _h != CGameObjects::Get().HelpSystem().GetEntries().end(); _h++)
		{
			i++; 
			j++;

			CHelpEntry* pH = (*_h).second;

			if (j%4 == 0 || j%2 == 0)
			{
				Ch->Write( "#600 %-15s%s",pH->GetName(),i%4==0?"#700\n\r":"");
				j = 0;
			}
			else
				Ch->Write( "#700 %-15s%s",pH->GetName(),i%4==0?"#700\n\r":"");
		}

		int nSize = CGameObjects::Get().HelpSystem().GetEntries().size();
		Ch->Write("\n\r\t\t\t\t%d Helpfile%s.\n\r", nSize, nSize > 1 ? "s" : "");

		if ( i>0 )
			Ch->Write("\n\r");

		Ch->Write("#600[#601See #600'#601Commands#600'#601 for a listing of all commands#600]#700\n\r");

	}

	return true;
}

// Method     :: CmdSetTitle
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <title>
// Return     :: Bool
// Function   :: Used to set a player's who title
// Written    :: Original {GMN}

bool CmdSetTitle::Perform(CActor* Ch, gStringList& CommandLine)
{
	if (CommandLine.empty() )
	{
		Ch->Write("Your Title is: %s.\n\r", Ch->Title());
		return true;
	}

	Ch->SetTitle(CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true));
	Ch->Write("Title set to: %s\n\r", Ch->Title());
	return true;

}

// Method     :: CmdScore
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Used to display a player's attributes
// Written    :: Original {GMN}, Updated {OWV}
// Update     :: To include a listing of attributes

bool CmdScore::Perform(CActor* Ch, gStringList& CommandLine)
{
	if ( !Ch )
		return false;

	time_t CreationTime = Ch->Created();
	gString gsStatus;

	CPlayer* Pl = (CPlayer*)Ch;
	CMobile* Mob = (CMobile*)Ch;

	if ( Ch->IsNPC() )
		gsStatus = "NPC";
	else
	if ( Ch->IsItem() )
		gsStatus = "Item";
	else

	if ( Ch->IsAdministrator() )
		gsStatus = "Administrator";
	else if ( Ch->IsAssistant() )
		gsStatus = "Assistant";
	else if ( Ch->IsStaff() )
		gsStatus = "Staff Member";
	else
		gsStatus = "Player"; 

	// Work out our dot's for each attribute
	// Thanks Ken for making this harder than it has to be *shakefist, drink beer*
	int nDivide[14]   = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	int nRemainder[14]= {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	gString gsStat[14]= {"", "", "", "", "", "", "", "", "", "", "", "", "", ""};
	int nStat = 0;

	// Strength
	nStat = Ch->Attributes()["strength"].Cur(); 
	nDivide[0] = (nStat-1) / 6; nRemainder[0] = ((nStat-1) % 6) + 1;
	// Dexterity
	nStat = Ch->Attributes()["dexterity"].Cur(); 
	nDivide[1] = (nStat-1) / 6; nRemainder[1] = ((nStat-1) % 6) + 1;
	// Constitution
	nStat = Ch->Attributes()["constitution"].Cur(); 
	nDivide[2] = (nStat-1) / 6; nRemainder[2] = ((nStat-1) % 6) + 1;
	// Intelligence
	nStat = Ch->Attributes()["intelligence"].Cur(); 
	nDivide[3] = (nStat-1) / 6; nRemainder[3] = ((nStat-1) % 6) + 1;
	// Reaction
	nStat = Ch->Attributes()["reaction"].Cur(); 
	nDivide[4] = (nStat-1) / 6; nRemainder[4] = ((nStat-1) % 6) + 1;
	// Wisdom
	nStat = Ch->Attributes()["wisdom"].Cur(); 
	nDivide[5] = (nStat-1) / 6; nRemainder[5] = ((nStat-1) % 6) + 1;
	// Charisma
	nStat = Ch->Attributes()["charisma"].Cur(); 
	nDivide[6] = (nStat-1) / 6; nRemainder[6] = ((nStat-1) % 6) + 1;
	// Status
	nStat = Ch->Attributes()["status"].Cur(); 
	nDivide[7] = (nStat-1) / 6; nRemainder[7] = ((nStat-1) % 6) + 1;
	// Willpower
	nStat = Ch->Attributes()["willpower"].Cur(); 
	nDivide[8] = (nStat-1) / 6; nRemainder[8] = ((nStat-1) % 6) + 1;
	// Integrity
	nStat = Mob->Integrity();
	nDivide[9] = (nStat-1) / 6; nRemainder[9] = ((nStat-1) % 6) + 1;
	// Defense
	nStat = Mob->Defense();
	nDivide[10] = (nStat-1) / 6; nRemainder[10] = ((nStat-1) % 6) + 1;
	// Perception
	nStat = Mob->Perception();
	nDivide[11] = (nStat-1) / 6; nRemainder[11] = ((nStat-1) % 6) + 1;
	// Peripheral
	nStat = Mob->Peripheral();
	nDivide[12] = (nStat-1) / 6; nRemainder[12] = ((nStat-1) % 6) + 1;
	// Health
	nStat = Mob->Health();
	nDivide[13] = (nStat-1) / 6; nRemainder[13] = ((nStat-1) % 6) + 1;



	for (int i = 0; i < 14; i++)
	{
		if (nDivide[i] == 0)
			gsStat[i].Format("  ");
		else
			gsStat[i].Format("%-2d", nDivide[i] * 6);

		for (int j = 0; j < nRemainder[i]; j++)
		{
			if (j == 3)
				gsStat[i] += " ";

			gsStat[i] += "*";
		}
	}
	
	gString gsCol1 = "#600";	// Border colour
	gString gsCol2 = "#601";	// Inside colour
	gString gsCol3 = "#700";	// 
	gString gsCol4 = "#701";


	Ch->Write( "    %s______________________________________________________________________#700\n\r", gsCol1);
	Ch->Write( "   %s/#201||||                                                             |||||%s\\#700\n\r", gsCol1, gsCol1);
	Ch->Write( "  %s/  ____________________________________________________________________  \\#700\n\r", gsCol1);
	Ch->Write( " %s/  /                                                                    \\  \\#700\n\r", gsCol1);
	Ch->Write( "%s/  /  %s\\\\%sGalacticReg%s::%sLogged in                                            %s\\  \\#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol4, gsCol1);
	Ch->Write( "%sI  I%s   Citizen%s...%s%-36s %sRace%s...%s%-13s %sI  I#700\n\r", gsCol1, gsCol3, gsCol2, gsCol4, Ch->Name(), gsCol3, gsCol2, gsCol4, Mob->Race(), gsCol1);
	Ch->Write( "%s|  |%s   Gender%s....%s%-7s                              %sSkill%s..%sVeteran       %s|  |#700\n\r", gsCol1, gsCol3, gsCol2, gsCol4, Mob->gsGender(), gsCol3, gsCol2, gsCol4, gsCol1);
	Ch->Write( "%s|  |%s   -----------------------------------------------------------------   %s|  |#700\n\r", gsCol1, gsCol2, gsCol1);
	Ch->Write( "%s|  |%s                      A  T  T  R  I  B  U  T  E  S                     %s|  |#700\n\r", gsCol1, gsCol3, gsCol1);
	Ch->Write( "%s|  |%s        PHYSICAL        %s|%s        MENTAL          %s|%s     SOCIAL          %s|  |#700\n\r", gsCol1, gsCol3, gsCol2, gsCol4, gsCol2, gsCol3, gsCol1);
	Ch->Write( "%s|  |%s Strength%s.....%s%-9s %s|%s Intelligence%s.%s%-9s %s|%s Charisma%s..%s%-9s %s|  |#700\n\r", gsCol1, gsCol3, gsCol2, gsCol4, gsStat[0], gsCol2, gsCol3, gsCol2, gsCol4, gsStat[3], gsCol2, gsCol3, gsCol2, gsCol4, gsStat[6], gsCol1);
	Ch->Write( "%s|  |%s Dexterity%s....%s%-9s %s|%s Reaction%s.....%s%-9s %s|%s Status%s....%s%-9s %s|  |#700\n\r", gsCol1, gsCol3, gsCol2, gsCol4, gsStat[1], gsCol2, gsCol3, gsCol2, gsCol4, gsStat[4], gsCol2, gsCol3, gsCol2, gsCol4, gsStat[7], gsCol1);
	Ch->Write( "%s|  |%s Constitution%s.%s%-9s %s|%s Wisdom%s.......%s%-9s %s|%s Willpower%s.%s%-9s %s|  |#700\n\r", gsCol1, gsCol3, gsCol2, gsCol4, gsStat[2], gsCol2, gsCol3, gsCol2, gsCol4, gsStat[5], gsCol2, gsCol3, gsCol2, gsCol4, gsStat[8], gsCol1);
	Ch->Write( "%s|  |%s                       __________________________                      %s|  |#700\n\r", gsCol1, gsCol2, gsCol1);
	Ch->Write( "%s|  |#101     I N F A M Y      %s/%s D E R I V E D  S T A T S %s\\#201       F A M E       %s|  |#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol1);
	Ch->Write( "%s|  |                     %sI%s  Defense%s........%s%-9s  %sI                    %s|  |#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol4, gsStat[10], gsCol2, gsCol1);
	Ch->Write( "%s|  |                     %s|%s  Peripheral%s.....%s%-9s  %s|                    %s|  |#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol4, gsStat[12], gsCol2, gsCol1);
	Ch->Write( "%s|  |                     %s|%s  Health%s.........%s%-9s  %s|                    %s|  |#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol4, gsStat[13], gsCol2, gsCol1);
	Ch->Write( "%s|  |                     %s|%s  Integrity%s......%s%-9s  %s|                    %s|  |#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol4, gsStat[9], gsCol2, gsCol1);
	Ch->Write( "%s|  |                     %sI%s  Perception%s.....%s%-9s  %sI                    %s|  |#700\n\r", gsCol1, gsCol2, gsCol3, gsCol2, gsCol4, gsStat[11], gsCol2, gsCol1);
	Ch->Write( "%s|  |%s                      \\__________________________/                     %s|  |#700\n\r", gsCol1, gsCol2, gsCol1);
	Ch->Write( "%s|  |                                                                       |  |#700\n\r", gsCol1);
	Ch->Write( "%sI  I                                                                       I  I#700\n\r", gsCol1);
	Ch->Write( "%s \\  \\                                                                     /  /#700\n\r", gsCol1);
	Ch->Write( "%s  \\  \\___________________________________________________________________/  /#700\n\r", gsCol1);
	Ch->Write( "%s   \\                                                                       /#700\n\r", gsCol1);
	Ch->Write( "%s   /                       E  X  P  E  R  I  E  N  C  E                    \\#700\n\r", gsCol1);
	Ch->Write( "%s  /     [                                                             ]     \\#700\n\r", gsCol1);
	Ch->Write( "%s  I    _________________________________________________________________    I#700\n\r", gsCol1);
	Ch->Write( "%s  |   /                                                                 \\   |#700\n\r", gsCol1);
	Ch->Write( "%s  |  |                                                                   |  |#700\n\r", gsCol1);
	Ch->Write( "%s  |  |                                                                   |  |#700\n\r", gsCol1);
	Ch->Write( "%s  |  |                                                                   |  |#700\n\r", gsCol1);
	Ch->Write( "%s  |  |                                                                   |  |#700\n\r", gsCol1);
	Ch->Write( "%s  |  |                                                                   |  |#700\n\r", gsCol1);
	Ch->Write( "%s  |  |___________________________________________________________________|  |#700\n\r", gsCol1);
	Ch->Write( "%s  |                                                                         |#700\n\r", gsCol1);
	Ch->Write( "%s  |_________________________________________________________________________|#700\n\r", gsCol1);

	
/*
	Ch->Write( "#600/////////////////////////#601[#700 Character Sheet #601]#600\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
	Ch->Write( "#701 Name#601:#700 %-40s #701Gender#601:#700 %s\n\r", Ch->Name(), CMobile::szMobileGender[Pl->Gender()]);
	Ch->Write( "#701 GUID#601:#700 %-40d\n\r", Ch->Vnum());
	Ch->Write( "#701 Description#601:#700 %-33s\n\r", Ch->Description());
	Ch->Write( "#701 Status#601:#700 %-38s\n\r", gsStatus);
	Ch->Write( "#701 Created On#601:#700 %s\n\r", ctime(&CreationTime));
	Ch->Write( "#701 Current Room#601:#700 %s\n\r", Ch->CurrentRoom() ? Ch->CurrentRoom()->Name() : "(None)" );
	Ch->Write( "#701 Health (Max)#601:#700 %d (Current)#601:#700 %d\n\r", Ch->MaxHealth(), Ch->CurrentHealth());
	Ch->Write( "#600//////////////////////////#601[#700 Attributes #601]#600\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700\n\r");
		
	// Attributes
	AttributeMap::iterator att;
	Ch->Write("#701    Physical                     Mental                    Social#700\n\r");
	if (Ch->Attributes().m_Attributes.size() <= 0)
	{
		Ch->Write("                                 No Attributes.\n\r");
	}
	else
	{
		Ch->Write("   Strength     #600[%2d#600]#700        Intelligence #600[%2d#600]#700        Charisma  #600[%2d#600]#700\n\r", 
			Ch->Attributes()["strength"].Cur(),
			Ch->Attributes()["intelligence"].Cur(),
			Ch->Attributes()["charisma"].Cur());

		Ch->Write("   Dexterity    #600[%2d#600]#700        Reaction     #600[%2d#600]#700        Status    #600[%2d#600]#700\n\r", 
			Ch->Attributes()["dexterity"].Cur(),
			Ch->Attributes()["reaction"].Cur(),
			Ch->Attributes()["status"].Cur());

		Ch->Write("   Constitution #600[%2d#600]#700        Wisdom       #600[%2d#600]#700        Willpower #600[%2d#600]#700\n\r", 
			Ch->Attributes()["constitution"].Cur(),
			Ch->Attributes()["wisdom"].Cur(),
			Ch->Attributes()["willpower"].Cur());

		//Ch->Attributes()["strength"]).Cur()


	}
*/

	return true;
}



// Method     :: CmdOpen
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <obj/ship/exit> to Open
// Return     :: Bool
// Function   :: Allows a Player to manipulate objects open
// Written    :: 09/01/2006 {OWV}

bool CmdOpen::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = NULL;
	bool bInside = false;

	// Hierarchy of opening objects:
	// [1] Room Exit
	// [2] Object
	// [3] Ship

	if (gsValue == "" && !Ch->CurrentRoom()->GetShip())
	{
		Ch->Write("You must supply a ship to open.\n\r");
		return true;
	}

	if (!Ch->CurrentRoom()->ShipHere(gsValue) && gsValue != "")
	{
		Ch->Write("You do not see '%s' here.\n\r", gsValue);
		return true;
	}

	// Try and open the hatch from inside
	if (gsValue == "")
	{
		bInside = true;	
		pShip = Ch->CurrentRoom()->GetShip(); // Validation on this being NULL is handled above
	}
	else
	{
		pShip = pGalaxy->GetShi(gsValue);

		// We should always get a ship, but if we dont then the room contains a ship
		// that no longer exists
		if (!pShip)
		{
			Ch->Write("[Invalid Ship] Room contains invalid Ship.\n\r");
			Ch->CurrentRoom()->RemShip(gsValue);
			return true;
		}
	}
		
	// Got a ship lets try and open it
	if (pShip->m_nExitState == CShip::ES_OPEN)
	{
		if (!bInside)
			Ch->Write("%s: %s's %s is already open.\n\r", pShip->m_gsType, pShip->m_gsName, pShip->m_gsExit);
		else
			Ch->Write("The %s is already open!\n\r", pShip->m_gsExit);

		return true;
	}
	// Can't lock yourself in
	else if (pShip->m_nExitState == CShip::ES_LOCKED && !bInside)
	{
		Ch->Write("%s: %s's %s is keycode locked.\n\r", pShip->m_gsType, pShip->m_gsName, pShip->m_gsExit);
		Ch->Write("Use: Unlock <Shipname> <keycode> To unlock it.\n\r");
		return true;
	}
	else if (pShip->m_nExitState == CShip::ES_CLOSED)
	{
		if (!bInside)
		{
			Ch->Write("You open %s's %s.\n\r", pShip->m_gsName, pShip->m_gsExit);
			Ch->CurrentRoom()->Write("[%s] %s\n\r", pShip->m_gsName, pShip->m_gsOMsg == "" ? "Boarding ramp opens." : pShip->m_gsOMsg);
			CArea* pArea = pShip->m_Area;
			CRoom* pRoom = pArea->GetRoom(pShip->m_nExit);
			pRoom->Write("The %s is opened from the outside.\n\r", pShip->m_gsExit);
		}
		else
		{
			if (pShip->m_ShipState->IsSet(CShip::_FLYING))
			{
				Ch->Write("You cant do that while the Ship is flying! Try an Airlock.\n\r");
				return true;
			}

			Ch->Write("You open the %s.\n\r", pShip->m_gsExit);
			CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());
			CRoom* pRoom = pArea->GetRoom(pShip->m_Land.Room());
			pRoom->Write("[%s] %s\n\r", pShip->m_gsName, pShip->m_gsOMsg);
		}

		pShip->m_nExitState = CShip::ES_OPEN;
		return true;
	}

	

	return true;
}


// Method     :: CmdClose
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <obj/ship/exit> to Close
// Return     :: Bool
// Function   :: Allows a Player to manipulate objects closed
// Written    :: 09/01/2006 {OWV}

bool CmdClose::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	bool bInside = false;
	CShip* pShip = NULL;

	// Hierarchy of closing objects:
	// [1] Room Exit
	// [2] Object
	// [3] Ship

	if (gsValue == "" && !Ch->CurrentRoom()->GetShip())
	{
		Ch->Write("You must supply a ship to close.\n\r");
		return true;
	}

	if (!Ch->CurrentRoom()->ShipHere(gsValue) && gsValue != "") 
	{
		Ch->Write("You do not see '%s' here.\n\r", gsValue);
		return true;
	}

	// Try and open the hatch from inside
	if (gsValue == "")
	{
		bInside = true;	
		pShip = Ch->CurrentRoom()->GetShip();
	}
	else
	{
		pShip = pGalaxy->GetShi(gsValue);

		// We should always get a ship, but if we dont then the room contains a ship
		// that no longer exists
		if (!pShip)
		{
			Ch->Write("[Invalid Ship] Room contains invalid Ship.\n\r");
			Ch->CurrentRoom()->RemShip(gsValue);
			return true;
		}
	}

	// Got a ship lets try and open it
	if (pShip->m_nExitState == CShip::ES_CLOSED)
	{
		Ch->Write("%s: %s's %s is already closed.\n\r", pShip->m_gsType, pShip->m_gsName, pShip->m_gsExit);
		return true;
	}
	else if (pShip->m_nExitState == CShip::ES_OPEN)
	{
		if (!bInside)
		{
			Ch->Write("You closed %s's %s.\n\r", pShip->m_gsName, pShip->m_gsExit);
			Ch->CurrentRoom()->Write("[%s] %s\n\r", pShip->m_gsName, pShip->m_gsCMsg == "" ? "Boarding ramp closed." : pShip->m_gsCMsg);
			CArea* pArea = pShip->m_Area;
			CRoom* pRoom = pArea->GetRoom(pShip->m_nExit);
			pRoom->Write("The %s is closed from the outside.\n\r", pShip->m_gsExit);
		}
		else
		{
			Ch->Write("You close the %s.\n\r", pShip->m_gsExit);
			CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());
			CRoom* pRoom = pArea->GetRoom(pShip->m_Land.Room());
			pRoom->Write("[%s] %s\n\r", pShip->m_gsName, pShip->m_gsCMsg);
		}

		pShip->m_nExitState = CShip::ES_CLOSED;
		return true;
	}

	return true;
}


// Method     :: CmdProne
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a Player to assume the prone position
// Written    :: 01/02/2006 {OWV}

bool CmdProne::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Already prone
	if (Ch->ActorPositions()->IsSet(CActor::_PRONE))
	{
		Ch->Write("You are already prone!\n\r");
		return true;
	}

	// Messages
	for (int i = 0; i < CActor::_NUMACTORPOS; i++)
	{
		if (Ch->ActorPositions()->IsSet(i))
		{
			Ch->Write("You move from %s into a prone position.\n\r", CActor::szActorPositions[i]);
			Ch->CurrentRoom()->Write(Ch, "%s moves from %s into a prone position.\n\r", Ch->Name(), CActor::szActorPositions[i]);
			Ch->SetShortDesc("is lying prone here.");
			Ch->ActorPositions()->RemoveBit(i);
			Ch->ActorPositions()->SetBit(CActor::_PRONE);
			return true;
		}
	}

	return true;
}

// Method     :: CmdCrouch
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a Player to assume the crouched position
// Written    :: 01/02/2006 {OWV}

bool CmdCrouch::Perform(CActor* Ch, gStringList& CommandLine)
{
	// First we need to perform a Mental State check to ensure they 
	// can change state
	// #TODO#

	// Already crouched
	if (Ch->ActorPositions()->IsSet(CActor::_CROUCHED))
	{
		Ch->Write("You are already crouched!\n\r");
		return true;
	}

	// Messages
	for (int i = 0; i < CActor::_NUMACTORPOS; i++)
	{
		if (Ch->ActorPositions()->IsSet(i))
		{
			Ch->Write("You move from %s into the crouched position.\n\r", CActor::szActorPositions[i]);
			Ch->CurrentRoom()->Write(Ch, "%s moves from %s into the crouched position.\n\r",Ch->Name(),  CActor::szActorPositions[i]);
			Ch->ActorPositions()->RemoveBit(i);
			Ch->SetShortDesc("is crouched here.");
			Ch->ActorPositions()->SetBit(CActor::_CROUCHED);
			return true;
		}
	}

	return true;
}

// Method     :: CmdSit
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a Player to assume the sitting position
// Written    :: 01/02/2006 {OWV}

bool CmdSit::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Already sitting
	if (Ch->ActorPositions()->IsSet(CActor::_SITTING))
	{
		Ch->Write("You are already sitting!\n\r");
		return true;
	}

	// Messages
	for (int i = 0; i < CActor::_NUMACTORPOS; i++)
	{
		if (Ch->ActorPositions()->IsSet(i))
		{
			Ch->Write("You move from %s into a sitting position.\n\r", CActor::szActorPositions[i]);
			Ch->CurrentRoom()->Write(Ch, "%s moves from %s into a sitting position.\n\r",Ch->Name(),  CActor::szActorPositions[i]);
			Ch->SetShortDesc("is sitting here.");
			Ch->ActorPositions()->RemoveBit(i);
			Ch->ActorPositions()->SetBit(CActor::_SITTING);
			return true;
		}
	}

	return true;
}

// Method     :: CmdStand
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a Player to assume the standing position
// Written    :: 01/02/2006 {OWV}

bool CmdStand::Perform(CActor* Ch, gStringList& CommandLine)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	int nPos = 0;

	// Already standing
	if (Ch->ActorPositions()->IsSet(CActor::_STANDING))
	{
		Ch->Write("You are already standing!\n\r");
		return true;
	}

	// Messages
	for (int i = 0; i < CActor::_NUMACTORPOS; i++)
	{
		if (Ch->ActorPositions()->IsSet(i))
		{
			nPos = i;
			Ch->ActorPositions()->RemoveBit(i);
		}
	}

	Ch->ActorPositions()->SetBit(CActor::_STANDING);

	// Are they manning a console, if so we need to stop them from doing this
	if (Ch->MannedPos())
	{
		
		Ch->Write("You stop manning the %s and stand up.\n\r", Ch->MannedPos()->m_gsName);
		Ch->CurrentRoom()->Write(Ch, "%s stands up, no longer manning the %s.\n\r",Ch->Name(),  Ch->MannedPos()->m_gsName);				

		Ch->MannedPos()->m_Manned = NULL;
		Ch->SetManned(NULL);
		Ch->SetMannedPos(NULL);
		
	}
	else
	{
		Ch->Write("You move from %s into a standing position.\n\r", CActor::szActorPositions[nPos]);
		Ch->CurrentRoom()->Write(Ch, "%s moves from %s into a standing position.\n\r",Ch->Name(),  CActor::szActorPositions[nPos]);				
	}

	Ch->SetShortDesc("is standing here.");

	return true;
}