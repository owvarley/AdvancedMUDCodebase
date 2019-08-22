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

// File     :: CmdsDesign.cpp
// Header   :: CmdsDesign.h
// Function :: Holds the implementations for commands that belong in the Design category

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Actor.h"
#include "CmdsDesign.h"
#include "Player.h"
#include "GameWorld.h"
#include "Room.h"
#include "Emotions.h"
#include "../gTools/Log.h"

// Macro to define implementations
IMPLEMENT_CLASS(CmdDesigns);		// Shows a list of all the designs
IMPLEMENT_CLASS(CmdDStart);			// Starts a new template design
IMPLEMENT_CLASS(CmdDStop);			// Stops a template design, saving it
IMPLEMENT_CLASS(CmdDEnter);			// Zooms into a level of detail
IMPLEMENT_CLASS(CmdDExit);			// Zooms out a level
IMPLEMENT_CLASS(CmdDEdit);			// Edits a field at this level of detail
IMPLEMENT_CLASS(CmdDInstall);		// Installs something into this level of detail
IMPLEMENT_CLASS(CmdDUninstall);		// Uninstalls something from this level
IMPLEMENT_CLASS(CmdDSave);			// Saves the template
IMPLEMENT_CLASS(CmdDFinish);		// Cycles the template onto the next stage
IMPLEMENT_CLASS(CmdDList);			// Lists the detail for the current level
IMPLEMENT_CLASS(CmdDShow);			// Shows a summary of the template
IMPLEMENT_CLASS(CmdDLink);			// Links two rooms together
IMPLEMENT_CLASS(CmdDUnlink);		// Unlinks two rooms
IMPLEMENT_CLASS(CmdDGoto);			// Goto the area for the ship
IMPLEMENT_CLASS(CmdDReview);		// Go back a stage in the creation cycle
IMPLEMENT_CLASS(CmdDClone);			// Clone a module any number of times

// Method     :: CmdDesigns
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: [1] Designs
//            :: [2] Designs list
//            :: [3] Designs view #
//            :: [4] Designs auth #
//            :: [5] Designs delete #
//			  :: [6] Designs open #
// Return     :: Bool
// Function   :: Allows a player to view the current design templates
// Written    :: 08/09/2005 {OWV}

bool CmdDesigns::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsFunction = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Function to perform
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2

	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);
	// We need an iterator to work our way through the list
	TemplateList::iterator tem;
	// We need the Template Loader here so we can list all Templates
	CTLoader* pLoader = CGameObjects::Get().GameWorld()->TLoader();

	// [1] & [2] :: Designs & Designs list
	// Function  :: Listing all the Templates
	if ((gsFunction == "list") || ( (!pPlayer->m_Template) && gsFunction == "" ))		
	{
		Ch->Write("#200:#201:#700 Ship Templates#201 :#200:#700\n\r");
		int i = 1; 

		for (tem = pLoader->m_Templates.begin(); tem != pLoader->m_Templates.end(); tem++)
		{
			Ch->Write("#200[#701 %2d#200]#700 %s #200<#201By:#701 %s#200> [#201%s#200]#700\n\r", i, (*tem)->m_gsName, (*tem)->m_gsAuthor, CTemplate::szStatus[(*tem)->m_nStatus]);
			i++;
		}
		return true;
	}
	// [3] :: Design view 
	// Functions :: Viewing the specfics of a certain Template
	else if (gsFunction == "view")
	{
		// Now Value 1 becomes the index number of the Template they wish to view
		if (atoi(gsValue1) > pLoader->m_Templates.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("#200<#701 %d#200>#700 is not a valid Template index, Choices: #200<#201 1 - %d#200>#700.\n\r", atoi(gsValue1), pLoader->m_Templates.size());
			return true;
		}

		// Get the Template
		CTemplate* pTemplate = pLoader->m_Templates.at(atoi(gsValue1)-1);

		Ch->Write("#200:#201:#700 %s by %s #201 :#200:#700\n\r", pTemplate->m_gsName, pTemplate->m_gsAuthor);
		Ch->Write("#200<#201%s#200>#700\n\r", pTemplate->m_gfFileName);

		if (pTemplate->m_Ship)
		{
			if (pTemplate->m_Ship->m_Shape)
			{
				Ch->Write("#200>#201>#701 Shape#700\n\r");
				Ch->Write(" Length#200:#701 %4dm \n\r #700Width#200:#701  %4dm \n\r #700Height#200:#701 %4dm#700\n\r", pTemplate->m_Ship->m_Shape->Length(), pTemplate->m_Ship->m_Shape->Width(), pTemplate->m_Ship->m_Shape->Height(), pTemplate->m_Ship->m_Shape->Size());
				if (pTemplate->m_Ship->m_Shape->m_Center)
					Ch->Write(" Center#200:#701 <%d %d %d>#700\n\r", pTemplate->m_Ship->m_Shape->m_Center->x, pTemplate->m_Ship->m_Shape->m_Center->y, pTemplate->m_Ship->m_Shape->m_Center->y);
				else
					Ch->Write(" Center#200:#701 Not defined.#700\n\r");			
			}
			Ch->Write("#200>#201>#701 Ship#700\n\r");
			Ch->Write(" Class#200:#701 %s#700\n\r", CShip::szTypes[pTemplate->m_Ship->m_nClass]);
			Ch->Write(" Designation#200:#701 %s#700\n\r", pTemplate->m_Ship->m_gsDesignation);
			Ch->Write(" Type#200:#701 %s#700\n\r", pTemplate->m_Ship->m_gsType);
			if (pTemplate->m_Ship->m_gsExit != "")
			{
				Ch->Write("#200>#201>#701 Exit#700\n\r");
				Ch->Write(" Name#200:#701     %s#700\n\r", pTemplate->m_Ship->m_gsExit);
				Ch->Write(" OpenMsg#200:#701  %s#700\n\r", pTemplate->m_Ship->m_gsOMsg);
				Ch->Write(" CloseMsg#200:#701 %s#700\n\r", pTemplate->m_Ship->m_gsCMsg);
			}
			Ch->Write("#200>#201>#701 Stats#700\n\r");
			Ch->Write(" Total Mass#200:#701 %d kg#700\n\r", pTemplate->m_Ship->Mass());
			Ch->Write(" Speed#200:#701   %d MGLT#700\n\r", pTemplate->m_Ship->TopSpeed(false));
			Ch->Write(" Shield#200:#701  %d#700\n\r", pTemplate->m_Ship->Shield(false));
			Ch->Write("#200>#201>#701 Energy Load#700\n\r");
			Ch->Write(" Minimum#200:#701 %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_MIN));
			Ch->Write(" Idle#200:#701    %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_IDLE));
			Ch->Write(" Normal#200:#701  %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_NORMAL));
			Ch->Write(" Combat#200:#701  %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_COMBAT));
			Ch->Write(" Maximum#200:#701 %5d\n\r", pTemplate->m_Ship->Energy(CShip::ET_MAX));
			Ch->Write("#200>#201>#701 Generation#700\n\r");
			Ch->Write(" Coolant#200:#701 %5d#700\n\r", pTemplate->m_Ship->Coolant(true));
			Ch->Write(" Power#200:#701   %5d#700\n\r", pTemplate->m_Ship->MaxPower());

			Ch->Write("\n\r");

			if (pTemplate->m_Ship->m_Frames.size() > 0)
			{
				int nCount = 1;

				for (FrameList::iterator frame = pTemplate->m_Ship->m_Frames.begin(); frame != pTemplate->m_Ship->m_Frames.end(); frame++)
				{
					Ch->Write("#200[#701 %2d#200]#700 Hull Frame, #201%-15s #700Total Mass#200:#701 %8d kg #700 ### HullCubes#200:#701 %2d#700\n\r", nCount, ("#200\"" +(*frame)->m_gsName + "#200\""), (*frame)->Mass(), (*frame)->m_HullCubes.size());
					nCount++;
				}
			}
				
			
		}
		else
			Ch->Write("#701Ship structure#200:#700\n\r No Ship structure created.\n\r");

		return true;
	}
	// [4] :: Design auth #
	// Function :: Allows an Administrator to approve a pending Design allowing it to be used in the game
	else if (gsFunction == "auth")
	{
		// Now Value 1 becomes the index number of the Template they wish to delete
		if (atoi(gsValue1) > pLoader->m_Templates.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("#100[#101Invalid Input#100] <#701%d#200>#700 is not a valid Template index, Choices#200: <#201 1 - %d#200>#700.\n\r", atoi(gsValue1), pLoader->m_Templates.size());
			return true;
		}

		// Get the Template
		CTemplate* pTemplate = pLoader->m_Templates.at(atoi(gsValue1)-1);

		if (!pTemplate)
			return false;

		// We have a valid template, check permissions
		if (pPlayer->IsAdministrator())
		{
			if (pTemplate->m_nStatus == CTemplate::_AUTHORISATION || pTemplate->m_nStatus == CTemplate::_COMPLETED)
			{
				if (pTemplate->m_gsAuthorisor == "")
				{
					Ch->Write("#201Template authorised.#700\n\r");
					pTemplate->m_gsAuthorisor = Ch->Name();
					pTemplate->m_nStatus++;
					pTemplate->Save();
					return true;
				}
				else
				{
					// Remove the authorisation
					Ch->Write("#201Template authorisation revoked.#700\n\r");
					pTemplate->m_gsAuthorisor = "";
					pTemplate->m_nStatus--;
					pTemplate->Save();
					return true;
				}
			}
			else
			{
				Ch->Write("#100[#101Invalid State#100]#700 That Template is not finished yet.\n\r");
				return true;
			}

		}
		else
		{
			Ch->Write("#100[#101Access Denied#100]#700 You do not have the correct permissions to authorise this Template.\n\r");
			return true;
		}

		return true;
	}
	// [5] :: Design delete #
	// Function :: Allows an Administrator to delete a template. Can only be performed by an Administrator
	else if (gsFunction == "delete")
	{
		// Now Value 1 becomes the index number of the Template they wish to delete
		if (atoi(gsValue1) > pLoader->m_Templates.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("#200[#201Invalid Input#200] <#701 %d#200>#700 is not a valid Template index, Choices: #200<#201 1 - %d#200>#700\n\r", atoi(gsValue1), pLoader->m_Templates.size());
			return true;
		}

		// Get the Template
		CTemplate* pTemplate = pLoader->m_Templates.at(atoi(gsValue1)-1);

		if (!pTemplate)
			return false;

		// We have a valid template now check Permissions
		if (pPlayer->Name() == pTemplate->m_gsAuthor || pPlayer->IsAdministrator())
		{
			pLoader->m_Templates.erase(pLoader->m_Templates.begin() + atoi(gsValue1)-1);

			gString	szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Ships\\Templates\\" + pTemplate->m_gfFileName;
			unlink(szFile);

			pLoader->Save();

			Ch->Write("#201%s deleted.#700\n\r", pTemplate->m_gsName);
			pTemplate = NULL;
			return true;
		}
		else
		{
			Ch->Write("#100[#101Access Denied#100]#700 You do not have the correct permissions to delete this Template.\n\r");
			return true;
		}

	}
	// [6] :: Design open #
	// Function :: Allows an Administrator to reopen a template for editing
	else if (gsFunction == "open")
	{
		// Now Value 1 becomes the index number of the Template they wish to delete
		if (atoi(gsValue1) > pLoader->m_Templates.size() || atoi(gsValue1) <= 0)
		{
			Ch->Write("#200[#201Invalid Input#200] <#701 %d#200>#700 is not a valid Template index, Choices: #200<#201 1 - %d#200>#700\n\r", atoi(gsValue1), pLoader->m_Templates.size());
			return true;
		}

		// Get the Template
		CTemplate* pTemplate = pLoader->m_Templates.at(atoi(gsValue1)-1);

		if (!pTemplate)
			return false;

		// Is it finished?
		if (pTemplate->m_nStatus != CTemplate::_COMPLETED)
		{
			Ch->Write("#200[#201Invalid State#200]#700 That template is not yet complete.\n\r");
			return true;
		}

		// Ok its finished, lets check they have the authorisation
		if (pPlayer->Name() == pTemplate->m_gsAuthor || pPlayer->IsAdministrator())
		{
			// They do, lets reopen it
			pTemplate->m_gsAuthorisor = "";
			pTemplate->m_nStatus = CTemplate::_TEMPLATE;
			Ch->Write("#201Template re-opened.#700\n\r");
			return true;
		}
		else
		{
			Ch->Write("#100[#101Access Denied#100]#700 You do not have the correct permissions to reopen this Template.\n\r");
			return true;
		}
	}


	Ch->Write("Syntax is#200:#701 \n\rDesigns #200<#201list#200>#701\n\rDesigns #200<#201view#200>#701 #200<#201####200>#701\n\rDesigns #200<#201auth#200>#701 #200<#201####200>#701\n\rDesigns #200<#201delete#200>#701 #200<#201####200>#700\n\r");
		
	return true;
}

// Method     :: CmdDStart
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <name of template>
// Return     :: Bool
// Function   :: Allows a player to start Designing/Editing a Template
// Written    :: 22/12/2005 {OWV}

bool CmdDStart::Perform(CActor* Ch, gStringList& CommandLine)
{

	// Process arguments
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);		// Value

	// We need the Template Loader here so we can list all Templates
	CTLoader* pLoader = CGameObjects::Get().GameWorld()->TLoader();
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (gsValue == "")
	{
		Ch->Write("#200[#201Invalid Input#200]#700 You must input a name for the Ship\n\r");
		Ch->Write("Syntax#200:#701 Start #200<#201Name of Template to Start/Edit#200>#700\n\r");
		return true;
	}
	else
	{
		bool bNew = true;

		if (pPlayer->m_Template)
		{
			Ch->Write("#100[#101Invalid Input#100]#700 You are already working on #200<#201%s#200>#700, finish this Template first.\n\r", pPlayer->m_Template->m_gsName);
			return true;
		}		

		// Check if the entered name corresponds to an already existing template or 
		// whether it is a new template which they wish to start work on.
		
		CTemplate* pEdit = NULL;

		for (TemplateList::iterator tem = pLoader->m_Templates.begin(); tem != pLoader->m_Templates.end(); tem++)
		{
			CTemplate* pFound = *tem;

			if (pFound->m_gsName == gsValue)
			{
				bNew = false;
				pEdit = pFound;
			}
		}

		if (bNew)
		{
			Ch->Write("Starting work on #200<#201%s#200>#700 Template.\n\r", gsValue);

			// Create the new Objects
			CTemplate* pTemp = new CTemplate();
			CShip* pShip = new CShip();
			CArea* pArea = new CArea();
			CShape* pShape = new CShape();
			//pShape->m_Ship = pShip;

			// Set the objects to this Template
			pTemp->m_Ship = pShip;
			pTemp->m_Area = pArea;
			pTemp->m_Shape = pShape;
			

			// Set its name
			pTemp->m_gsName = gsValue;		

			// Format and set the Filename of the Template
			pTemp->m_gfFileName = gsValue + ".tem";

			// Set this as the Template they are working on
			pPlayer->m_Template = pTemp;	

			// Set this Player as our Author
			pTemp->m_gsAuthor = pPlayer->Name();

			// Set the Status of the Template to Template, i.e. the Starting structure
			pTemp->m_nStatus = CTemplate::_TEMPLATE;
			
			// Save the Template
			pTemp->Save();

			// Add the Template to our Loader's list
			pLoader->m_Templates.push_back(pTemp);

			// Save the Loader
			pLoader->Save();

			Ch->Write("Entering Design Buffer.\n\rValid Commands#200:#701 ");
			// Dynamic list of available commands
			for (CmdParsers::iterator pos = Ch->HomeWorld()->Commands().begin(); pos != Ch->HomeWorld()->Commands().end(); pos++ )
			{
				if ((*pos)->m_gsName == "Design Commands")
				{
					for ( CommandList::iterator cmd = (*pos)->m_Commands.begin(); cmd != (*pos)->m_Commands.end(); cmd++ )
						Ch->Write("%s ", (*cmd)->Name());
				}
				Ch->Write("#700\n\r");
			}
			
			return true;
		}
		else
		{
			Ch->Write("Editing #200<#201%s#200>#700 Template.\n\r", gsValue);
			Ch->Write("Entering Design Buffer.\n\rValid Commands#200:#701 ");
			// Dynamic list of available commands
			for (CmdParsers::iterator pos = Ch->HomeWorld()->Commands().begin(); pos != Ch->HomeWorld()->Commands().end(); pos++ )
			{
				if ((*pos)->m_gsName == "Design Commands")
				{
					for ( CommandList::iterator cmd = (*pos)->m_Commands.begin(); cmd != (*pos)->m_Commands.end(); cmd++ )
						Ch->Write("%s ", (*cmd)->Name());
				}
				Ch->Write("#700\n\r");
			}

			pPlayer->m_Template = pEdit;

			// We need to load the Area file if there is one
			if (pPlayer->m_Template->m_Area)
			{
				// Get the Current GameWorld
				CGameWorld* pGame = CGameObjects::Get().GameWorld();

				// The area needs to be added to the Arealist so we can work with it
				pGame->Areas().push_back(pPlayer->m_Template->m_Area);
			}

			return true;
		}

		
	}

}

// Method     :: CmdDStop
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to stop Designing/Editing a Template
// Written    :: 22/12/2005 {OWV}

bool CmdDStop::Perform(CActor* Ch, gStringList& CommandLine)
{	
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);


	// Make sure they have a template already
	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101Invalid Input#100]#700 You are not currently working on a Template.\n\r");
		return true;
	}
	else
	{
		Ch->Write("Stopped work on Template #200<#201%s#200>#700, Template saved.\n\r", pPlayer->m_Template->m_gsName);

		// Are they in a room specific to this Template?
		if (pPlayer->m_Template->m_Area)
		{
			if (Ch->CurrentRoom()->Area() == pPlayer->m_Template->m_Area->Area())
			{
				CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(CGameObjects::Get().ConfigData().GetInt("default_area", "Options"));
				Ch->SetCurrentRoom((*pArea->Rooms()->begin()).second);
			}
		}

		// We need to remove the Area from the AreaList
		if (pPlayer->m_Template->m_Area)
		{
			// Get the Current GameWorld
			CGameWorld* pGame = CGameObjects::Get().GameWorld();
			bool bFound = false;

			int nCount = 0;
			for (AreaList::iterator area = pGame->Areas().begin(); area != pGame->Areas().end(); area++)
			{
				// We need to find the area to delete, if there isnt one then this is a new
				// template.
				if ((*area)->Identifier() == pPlayer->m_Template->m_Area->Identifier())
				{
					bFound = true;
					break;
				}

				nCount++;	
			}

			// Remove the Area from the list
			if (bFound)
				pGame->Areas().erase(pGame->Areas().begin()+nCount);

		}

		pPlayer->m_Template->Save();
		pPlayer->m_Template = NULL;
		pPlayer->m_Frame = NULL;
		pPlayer->m_HullCube = NULL;
		pPlayer->m_Component = NULL;

		
		return true;
	}

	return true;

}

// Method     :: CmdDEnter
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Index # of Frame/HullCube/Component>
// Return     :: Bool
// Function   :: Allows a player to Zoom into a Frame, HullCube or Component
// Written    :: 22/12/2005 {OWV}

bool CmdDEnter::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value
	
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#700 %s has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// Check for input first of all
	if (gsValue == "")
	{
		Ch->Write("#100[#101Invalid Input#100]#700 You must input an Index Number to Enter\n\r");
		if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
			Ch->Write("Type Show to see valid Index Numbers.\n\r");
		else
			Ch->Write("Type List to see valid Index Numbers.\n\r");
		return true;
	}
	else
	{
		if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
		{
			// [1] - How we handle the input depends on their 'level' of zoom

			// Frame Check: If they have one zoom in, if not check if the input is a valid frame
			if (pPlayer->m_Frame)
			{
				// HullCube Check: If they have one zoom in, if not check if the input is a valid hullcube
				if (pPlayer->m_HullCube)
				{
					// Component Check: If they have one zoom in, if not check if the input is a valid Component
					if (pPlayer->m_Component)
					{
						Ch->Write("#100[#101Invalid Command#100]#700 You cannot zoom in any further. Use exit to step back a level.\n\r");
						return true;
					}
					else
					{
						// They are trying to zoom into a Component
						// Check they entered a valid Component (Not negative)
						if (atoi(gsValue) > pPlayer->m_HullCube->m_Components.size() || atoi(gsValue)-1 < 0)
						{
							Ch->Write("#100[#101Invalid Input#100] <#701 %d#200>#700 is not a Valid Component Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), pPlayer->m_HullCube->m_Components);
							return true;
						}
						else
						{
							// We know this Frame is valid so we need to get a reference to it
							CComponent* pComp = pPlayer->m_HullCube->m_Components.at(atoi(gsValue) -1);

							// Sanity check
							if (!pComp)
								return false;

							pPlayer->m_Component = pComp;
							Ch->Write("Entering <%s>\n\r", pComp->m_gsName);
							
							// Display all its Modules
							pPlayer->ExecuteCommand("list", "");
		
						}
					}

				}
				else
				{
					// They are trying to zoom into a HullCube
					// Check they entered a valid Cube (Not negative)
					if (atoi(gsValue) > pPlayer->m_Frame->m_HullCubes.size() || atoi(gsValue)-1 < 0)
					{
						Ch->Write("#100[#101Invalid Input#100] <#701 %d#200>#700 is not a Valid HullCube Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), pPlayer->m_Frame->m_HullCubes.size());
						return true;
					}
					else
					{
						// We know this Frame is valid so we need to get a reference to it
						CHull* pHull = pPlayer->m_Frame->m_HullCubes.at(atoi(gsValue) -1);

						// Sanity check
						if (!pHull)
							return false;

						pPlayer->m_HullCube = pHull;
						Ch->Write("Entering #200<#201%s#200>#700\n\r", pHull->m_gsName);
						// Display its Components
						pPlayer->ExecuteCommand("list", "");
						
					}
				}

			}
			else
			{
				// They are trying to zoom into a Frame
				// Check they entered a valid Frame (Not negative)
				if (atoi(gsValue) > pTemplate->m_Ship->m_Frames.size() || atoi(gsValue)-1 < 0)
				{
					Ch->Write("#100[#101Invalid Input#100] <#702 %d#200>#700 is not a Valid Frame Index. Choices #200<#201 1 - %d#201>#700\n\r", atoi(gsValue), pTemplate->m_Ship->m_Frames.size());
					return true;
				}
				else
				{
					// We know this Frame is valid so we need to get a reference to it
					CFrame* pFrame = pTemplate->m_Ship->m_Frames.at(atoi(gsValue) -1);

					// Sanity check
					if (!pFrame)
						return false;

					pPlayer->m_Frame = pFrame;
					Ch->Write("Entering #200<#201%s#200>#700\n\r", pFrame->m_gsName);
					// Display all its HullCubes
					pPlayer->ExecuteCommand("list", "");
					
				}

			}
		}
		else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
		{
			// Check they entered a valid Weapon Group
			if (atoi(gsValue) > pTemplate->m_Ship->m_Weapons.size() || atoi(gsValue)-1 < 0)
			{
				Ch->Write("#100[#101Invalid Input#100] <#702 %d#200>#700 is not a Valid Weapon Group Index. Choices #200<#201 1 - %d#201>#700\n\r", atoi(gsValue), pTemplate->m_Ship->m_Weapons.size());
				return true;
			}
			else
			{
				// We now have a valid Weapon Group
				CWeaponGroup* pWeapon = pTemplate->m_Ship->m_Weapons.at(atoi(gsValue) -1);
	
				// Sanity check
				if (!pWeapon)
					return false;

				pPlayer->m_Weapon = pWeapon;
				Ch->Write("Entering #200<#201%s#200>#700\n\r", pWeapon->m_gsName);

				// Display the Weapon Groups details
				pPlayer->ExecuteCommand("show", "");
			
			}

		}
	}

	
	return true;
}

// Method     :: CmdDExit
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to Zoom out of a Frame, HullCube or Component
// Written    :: 22/12/2005 {OWV}

bool CmdDExit::Perform(CActor* Ch, gStringList& CommandLine)
{
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#700 %s has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;


	if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
		{
		// We need to zoom out depending on our level of zoom already
		if (pPlayer->m_Frame)
		{
			// Check if we have zoomed into a HullCube
			if (pPlayer->m_HullCube)
			{
				// Check if we have zoomed into a Component
				if (pPlayer->m_Component)
				{
					// Exit it
					Ch->Write("#201Exiting %s#700\n\r", pPlayer->m_Component->m_gsName);
					pPlayer->m_Component = NULL;
					
					// Display their current contents
					pPlayer->ExecuteCommand("list", "");

				}
				else
				{
					Ch->Write("#201Exiting %s#700\n\r", pPlayer->m_HullCube->m_gsName);
					pPlayer->m_HullCube = NULL;

					pPlayer->ExecuteCommand("list", "");

				}

			}
			else
			{
				Ch->Write("#201Exiting %s#700\n\r", pPlayer->m_Frame->m_gsName);
				pPlayer->m_Frame = NULL;

				pPlayer->ExecuteCommand("list", "");
			}

		}
		else
		{
			Ch->Write("#100[#101Invalid State#100]#700 You must first enter a Frame.\n\r");
			return true;
		}
	}
	else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
	{
		if (pPlayer->m_Weapon)
		{
			Ch->Write("#201Exiting %s#700\n\r", pPlayer->m_Weapon->m_gsName);
			pPlayer->m_Weapon = NULL;
		}
		else
		{
			Ch->Write("#100[#101Invalid State#100]#700 You must first enter a Weapon Group.\n\r");
			return true;
		}
	}



	return true;	
	
}

// Method     :: CmdDEdit
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Field> <New value>
// Return     :: Bool
// Function   :: Allows a player to exit a Ship, Frame, Hullcube, Component or Weapon group
// Written    :: 22/12/2005 {OWV}

bool CmdDEdit::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value
		if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#700 %s has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// Check for input first of all
	if (gsValue == "")
	{
		Ch->Write("[Invalid Input] You must input a field to Edit\n\r");

		if (pPlayer->m_Template->m_nStatus == CTemplate::_AREA)
		{
			if (Ch->CurrentRoom()->Area() == pPlayer->m_Template->m_Area->Area())
			{
				Ch->Write("Valid fields: Name, Description.\n\r");
				return true;
			}
			else
			{
				Ch->Write("#100[#101Invalid Area#100]#700 You can only edit the area belonging to your Design.\n\r");
				Ch->Write("Use the goto command to enter your Design Area.\n\r");
				return true;
			}
		}

		if (pPlayer->m_Weapon)
		{
			Ch->Write("Valid fields#200:#701 Name, Type#700\n\r");
			return true;
		}

		// Our error message will depend on our level of Zoom
		if (pPlayer->m_Frame)
		{			
			if (pPlayer->m_HullCube)
			{
				if (pPlayer->m_Component)
				{
					Ch->Write("Valid fields#200:#701 Name, Size, Type, Arc#700\n\r");
					return true;
				}
				else
				{
					Ch->Write("Valid fields#200:#700 Name, X, Y, Z, IntLength, IntWidth, IntHeight, ExtLength, ExtWidth, ExtHeight, Arc#700\n\r");
					return true;
				}
			}
			else
			{
				Ch->Write("Valid fields#200:#701 Name#700\n\r");
				return true;
			}
		}
		else
		{
			Ch->Write("Valid fields#200:#701 Class, Type, Designation, ExitName, OpenMsg, CloseMsg#700\n\r");
			return true;
		}
		
	}

	if (gsValue1 == "")
	{
		Ch->Write("#100[#101Invalid Entry#100]#700 You must enter a new value for the field.\n\r");
		return true;
	}
	else
	{
		if (pPlayer->m_Template->m_nStatus == CTemplate::_AREA)
		{
			if (gsValue == "Name")
			{
				pPlayer->CurrentRoom()->SetName(gsValue1);
				Ch->Write("Name set to #201%s#700\n\r", gsValue1);
				return true;
			}
			else if (gsValue == "Description")
			{
				pPlayer->CurrentRoom()->SetDescription(gsValue1);
				Ch->Write("#201Description set.#700\n\r");
				return true;
			}
			else 
			{
				Ch->Write("#100[#100Invalid Field#100]#700 %s is not a valid field to edit\n\r", gsValue);
				Ch->Write("Valid Fields#200:#701 Name, Description#700\n\r");
				return true;
			}
		}
		else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
		{
			// They can only edit if they have Zoomed into a Weapon Group
			if (pPlayer->m_Weapon)
			{
				if (gsValue == "Name")
				{
					pPlayer->m_Weapon->m_gsName = gsValue1;
					Ch->Write("Name set to #201%s#700\n\r", gsValue1);
					return true;
				}
				else if (gsValue == "Type")
				{
					if (atoi(gsValue1) < 0 || atoi(gsValue1) >= CWeaponGroup::_NUMWEAPONGROUP)
					{
						Ch->Write("#100[#101Invalid Type#100]#700 %d is not a valid type\n\r", atoi(gsValue1));
						Ch->Write("Valid Types#200:#700\n\r");
						for (int i = 0; i < CWeaponGroup::_NUMWEAPONGROUP; i++)
							Ch->Write("[%d] %s\n\r", i, CWeaponGroup::szTypes[i]);
						return true;
					}
					else
					{
						pPlayer->m_Weapon->m_nType = atoi(gsValue1);
						Ch->Write("Type set to #201%s#700\n\r", CWeaponGroup::szTypes[atoi(gsValue1)]);
						return true;
					}

				}

			}
			else
			{
				Ch->Write("#100[#100Invalid State#100]#700 You must first #701enter#700 a Weapon Group before you can edit it.\n\r");
				return true;
			}

		}
		// They entered a value, lets edit the field, again this depends on our level of zoom
		if (pPlayer->m_Frame)
		{
			if (pPlayer->m_HullCube)
			{
				if (pPlayer->m_Component)
				{
						// Change the Name of the Component
						if (gsValue == "Name")
						{
							Ch->Write("#201%s renamed to %s#700\n\r", pPlayer->m_Component->m_gsName, gsValue1);
							pPlayer->m_Component->m_gsName = gsValue1;
							return true;
						}
						// Change the Size of the Component
						else if (gsValue == "Size")
						{
							if (atoi(gsValue1) < 0)
							{
								Ch->Write("#100[#101Invalid Input#100]#700 Negative sizes are not permitted.\n\r");
								return true;
							}

							Ch->Write("#201%s size set to %d#700\n\r", pPlayer->m_Component->m_gsName, atoi(gsValue1));
							pPlayer->m_Component->m_nSize = atoi(gsValue1);
							return true;
						}
						// Change the Type of the Component
						else if (gsValue == "Type")
						{
							if (atoi(gsValue1) > CComponent::CT_LAST || atoi(gsValue1) < 0) 
							{
								Ch->Write("#200[#201Invalid Value#200]#700 That is not a valid value:\n\r");

								for (int i = 0; i < CComponent::CT_LAST; i++)
									Ch->Write("#200[#701 %d#200]#700 %s\n\r", i+1, CComponent::szComponents[i]);

								return true;
							}

							Ch->Write("%s type set to #201%s#700\n\r", pPlayer->m_Component->m_gsName, CComponent::szComponents[atoi(gsValue1)]);
							pPlayer->m_Component->m_nSize = atoi(gsValue1);
							return true;
						}
						// Change the Arc of the Component (Weapons and Shield Mounts only)
						else if (gsValue == "Arc")
						{
							CComponent* pComp = pPlayer->m_Component;

							if (pPlayer->m_Component->m_Type != CComponent::CT_SHIELD && pPlayer->m_Component->m_Type != CComponent::CT_WEAPONMOUNT)
							{
								Ch->Write("#100[#101Invalid Component#100]#700 Only Weapon or Shield mounts can be set to Arcs\n\r");
								return true;
							}

							bool bFound = false;
							int nArc = 0;
							int i = 0;

							for (i = 0; i < CShip::A_MAX; i++)
							{
								gString gsArc = CShip::szArc[i];
								if (gsArc == gsValue1)
								{
									nArc = i;
									bFound = true;
									break;
								}
							}
							
							if (!bFound && gsValue1 != "all")
							{
								Ch->Write("#100[#101Invalid Arc#100]#701 %s #700is not a valid arc.\n\r", gsValue1);
								Ch->Write("Valid arcs are:\n\r");
								for (int j = 0; j < CShip::A_MAX; j++)
									Ch->Write("%s ", CShip::szArc[j]);
								Ch->Write("All.\n\r");
								return true;
							}
							else
							{
								// All will be simple, it will add ALL arcs, or remove ALL arcs
								if (gsValue1 == "all")
								{
									// First we handle weapon mounts
									if (pComp->m_Type == CComponent::CT_WEAPONMOUNT)
									{
										if (((CWeapon*)pComp)->m_Orientation->IsSet(0))
										{
											for (i = 0; i < CShip::A_MAX; i++)
												((CWeapon*)pComp)->m_Orientation->RemoveBit(i);
											
											Ch->Write("%s no longer orientated in #201any#700 arcs.\n\r", pComp->m_gsName);
											return true;
										}
										else
										{
											for (i = 0; i < CShip::A_MAX; i++)
												((CWeapon*)pComp)->m_Orientation->SetBit(i);

											Ch->Write("%s orientated with #201all#700 arcs.\n\r", pComp->m_gsName);
											return true;
										}
									}
									// Now we handle Shield mounts
									else if (pComp->m_Type == CComponent::CT_SHIELD)
									{
										if (((CShield*)pComp)->m_Orientation->IsSet(0))
										{
											for (i = 0; i < CShip::A_MAX; i++)
												((CShield*)pComp)->m_Orientation->RemoveBit(i);
											
											Ch->Write("%s no longer orientated in #201any#700 arcs.\n\r", pComp->m_gsName);
											return true;
										}
										else
										{
											for (i = 0; i < CShip::A_MAX; i++)
												((CShield*)pComp)->m_Orientation->SetBit(i);

											Ch->Write("%s orientated with #201all#700 arcs.\n\r", pComp->m_gsName);
											return true;
										}
									}
								}
								else
								{								
									if (pComp->m_Type == CComponent::CT_WEAPONMOUNT)
									{
										if (((CWeapon*)pComp)->m_Orientation->IsSet(nArc))
										{
											((CWeapon*)pComp)->m_Orientation->RemoveBit(nArc);
											Ch->Write("%s no longer orientated in the #201%s#700 arc.\n\r", pComp->m_gsName, CShip::szArc[nArc]);
											return true;
										}
										else
										{
											((CWeapon*)pComp)->m_Orientation->SetBit(nArc);
											Ch->Write("%s orientated with the #201%s#700 arc.\n\r", pComp->m_gsName, CShip::szArc[nArc]);
											return true;
										}
									}
									
									if (pComp->m_Type == CComponent::CT_SHIELD)
									{
										if (((CShield*)pComp)->m_Orientation->IsSet(nArc))
										{
											((CShield*)pComp)->m_Orientation->RemoveBit(i);
											Ch->Write("%s no longer orientated in the #201%s#700 arc.\n\r", pComp->m_gsName, CShip::szArc[nArc]);
											return true;
										}
										else
										{
											((CShield*)pComp)->m_Orientation->SetBit(nArc);
											Ch->Write("%s orientated with the #201%s#700 arc.\n\r", pComp->m_gsName, CShip::szArc[nArc]);
											return true;
										}
									}
								}

							}

						}						
				
				}
				else
				{
					if (gsValue == "Name")
					{
						pPlayer->m_HullCube->m_gsName = gsValue1;
						Ch->Write("HullCube renamed to #201%s#700\n\r", gsValue1);
						return true;
					}
					// Editing Keel Value removed in ETS
					/*
					if (gsValue == "Keel")
					{
						
						pPlayer->m_HullCube->m_nCKeel = atoi(gsValue1);
						pPlayer->m_HullCube->m_nMKeel = atoi(gsValue1);
						Ch->Write("HullCube Keel integrity set to#201 %s#700\n\r", gsValue1);
						return true;
					} */
					if (gsValue == "IntWidth")
					{
						pPlayer->m_HullCube->m_nWidth = atoi(gsValue1);
						Ch->Write("Width set to #201 %d#200\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "IntLength")
					{
						pPlayer->m_HullCube->m_nLength = atoi(gsValue1);
						Ch->Write("Length set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "IntHeight")
					{
						pPlayer->m_HullCube->m_nHeight = atoi(gsValue1);
						Ch->Write("Height set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "ExtWidth")
					{
						pPlayer->m_HullCube->m_EDimension->m_nWidth = atoi(gsValue1);
						Ch->Write("Width set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "ExtLength")
					{
						pPlayer->m_HullCube->m_EDimension->m_nLength = atoi(gsValue1);
						Ch->Write("Length set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "ExtHeight")
					{
						pPlayer->m_HullCube->m_EDimension->m_nHeight = atoi(gsValue1);
						Ch->Write("Height set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "x")
					{
						pPlayer->m_HullCube->m_EDimension->m_Location->x = atoi(gsValue1);
						Ch->Write("X Location set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "y")
					{
						pPlayer->m_HullCube->m_EDimension->m_Location->y = atoi(gsValue1);
						Ch->Write("Y Location set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					}
					if (gsValue == "z")
					{
						pPlayer->m_HullCube->m_EDimension->m_Location->z = atoi(gsValue1);
						Ch->Write("Z Location set to #201 %d#700\n\r", atoi(gsValue1));
						return true;
					} 
					if (gsValue == "Arc")
					{
						bool bFound = false;
						int nArc = 0;
						int i = 0;

						for (i = 0; i < CShip::A_MAX; i++)
						{
							gString gsArc = CShip::szArc[i];
							if (gsArc == gsValue1)
							{
								nArc = i;
								bFound = true;
								break;
							}
						}
						
						if (!bFound && gsValue1 != "all")
						{
							Ch->Write("#100[#101Invalid Arc#100]#701 %s #700is not a valid arc.\n\r", gsValue1);
							Ch->Write("Valid arcs are:\n\r");
							for (int j = 0; j < CShip::A_MAX; j++)
								Ch->Write("%s ", CShip::szArc[j]);
							Ch->Write("All.\n\r");
							return true;
						}
						else
						{
							// All will be simple, it will add ALL arcs, or remove ALL arcs
							if (gsValue1 == "all")
							{
								if (pPlayer->m_HullCube->m_Cover->IsSet(0))
								{
									Ch->Write("%s no long able to be hit from #201any#700 arc.\n\r", pPlayer->m_HullCube->m_gsName);
									for (i = 0; i < CShip::A_MAX; i++)
										pPlayer->m_HullCube->m_Cover->RemoveBit(i);

									return true;

								}
								else
								{
									Ch->Write("%s can now be hit from #201all#700 arcs.\n\r", pPlayer->m_HullCube->m_gsName);
									for (i = 0; i < CShip::A_MAX; i++)
										pPlayer->m_HullCube->m_Cover->SetBit(i);

									return true;
								}


							}
							else
							{
								if (pPlayer->m_HullCube->m_Cover->IsSet(i))
								{
									pPlayer->m_HullCube->m_Cover->RemoveBit(i);
									Ch->Write("%s no long able to be hit from the #201%s#700 arc.\n\r", pPlayer->m_HullCube->m_gsName, CShip::szArc[i]);
									return true;
								}
								else
								{
									pPlayer->m_HullCube->m_Cover->SetBit(i);
									Ch->Write("%s can now be hit from the #201%s#700 arc.\n\r", pPlayer->m_HullCube->m_gsName, CShip::szArc[i]);
									return true;
								}
							}

						}

					}

					// If we reach here its not a valid field
					Ch->Write("#100[#101Invalid Field#100]#700 That is not a valid field\n\r");
					Ch->Write("Valid fields#200:#701 Name, X, Y, Z, IntLength, IntWidth, IntHeight, ExtLength, ExtWidth, ExtHeight, Arc, Keel#700\n\r");
					return true;

				}


			}
			else
			{
				// Renaming the Frame
				if (gsValue == "Name")
				{
					pPlayer->m_Frame->m_gsName = gsValue1;
					Ch->Write("Frame renamed to #201%s#700\n\r", gsValue1);
					return true;
				}

				Ch->Write("#100[#101Invalid Field#100]#700 That is not a valid field\n\r");
				Ch->Write("Valid fields#200:#701 Name#700\n\r");
				return true;

			}
		}
		else
		{
			// Editing the Ship
			if (gsValue == "Class")
			{
				// Check its a valid class
				if (atoi(gsValue1) > CShip::ST_MAX)
				{
					Ch->Write("#100[#101Invalid Class#100]#700 That is not a valid Class index.\n\r");
					Ch->Write("Valid selections are:\n\r");
					for (int i = 0; i < CShip::ST_MAX; i++)
						Ch->Write("#200[#701 %d#200]#700 %s\n\r", i, CShip::szTypes[i]);
					return true;

				}
				else
				{
					pTemplate->m_Ship->m_nClass = atoi(gsValue1);
					Ch->Write("#201Class set.#700\n\r");
					return true;
				}

			}

			if (gsValue == "Type")
			{
				pTemplate->m_Ship->m_gsType = gsValue1;
				Ch->Write("#201Type set.#700\n\r");
				return true;

			}

			if (gsValue == "Designation")
			{
				pTemplate->m_Ship->m_gsDesignation = gsValue1;
				Ch->Write("#201Designation set.#700\n\r");
				return true;

			}

			if (gsValue == "ExitName")
			{
				pTemplate->m_Ship->m_gsExit = gsValue1;
				Ch->Write("#201Exit Name set.#700\n\r");
				return true;
			}

			if (gsValue == "OpenMsg")
			{
				pTemplate->m_Ship->m_gsOMsg = gsValue1;
				Ch->Write("#201Exit Open Message set.#700\n\r");
				return true;
			}

			if (gsValue == "CloseMsg")
			{
				pTemplate->m_Ship->m_gsCMsg = gsValue1;
				Ch->Write("#201Exit Close Message set.#700\n\r");
				return true;
			}


			
			Ch->Write("#100[#101Invalid Field#100]#700 That is not a valid field to edit.\n\r");
			Ch->Write("Valid fields#200:#701 Class, Type, Designation, ExitName, OpenMsg, CloseMsg#700\n\r");
			return true;

		}

	}


	return true;

}

// Method     :: CmdDInstall
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: Frame    : <name>
//			  :: HullCube : <name>
//			  :: Component: <# comp type> <name> <max size>
//			  :: Module   : <# module index>
//			  :: In Area  : <# comp index>
//			  :: Add Exit : <exit>
//			  :: List comp: <list>
//			  :: Wep Group: <name>
// Return     :: Bool
// Function   :: Allows a player to install a Frame/HullCube/Component/Module in Template Mode
//			  :: Allows a player to assign Components, view components and assign an exit in Area Design Mode
//			  :: Allows a player to install weapon groups in Weapon Group config mode
// Written    :: 22/12/2005 {OWV}

bool CmdDInstall::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2
	
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100] #701%s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;
	CMLoader* pMLoader = CGameObjects::Get().GameWorld()->MLoader();
	CHLoader* pHLoader = CGameObjects::Get().GameWorld()->HLoader();

	// Check for input first of all
	if (gsValue == "")
	{
		if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
		{
			// They are trying to install a Frame, Cube, Component or Module in Template mode
			// Our error message will depend on our level of Zoom
			if (pPlayer->m_Frame)
			{
				if (pPlayer->m_HullCube)
				{
					if (pPlayer->m_Component)
					{
						// Installing a Module
						Ch->Write("#100[#101Invalid Input#100]#700 You must first enter a Module Index.\n\r");
						Ch->Write("Valid Choices #200<#201 1 - %d#200>#700\n\r", pMLoader->m_Modules.size());
						Ch->Write("%s Accepts#200:\n\r", pPlayer->m_Component->szClassnames[pPlayer->m_Component->m_Type]);
						int nCount = 1;
						for (InstallMap::iterator mod = pPlayer->m_Component->m_Install.begin(); mod != pPlayer->m_Component->m_Install.end(); mod++)
						{
							Ch->Write("[#701 %2d#200]#700 %s\n\r", nCount, CModule::szModules[((*mod).first)]);
							nCount++;
						} 
						return true;
					}
					else
					{
						// Installing a Component
						Ch->Write("#100[#101Invalid Input#100]#700 You must first enter a type for your Component.\n\r");
						Ch->Write("Syntax: Install #200<#201### of comp type#200> <#201name#200> <#201size#200>#700\n\r");
						for (int i = 0; i < CComponent::CT_LAST; i++)
							Ch->Write("#200[#701 %2d#200]#700 %s\n\r", i+1, CComponent::szComponents[i]);
						Ch->Write("Armour Install, Syntax#200:#701 Install armour #200<#201Armour Mod Index ####200> <#201Durability#200>#700\n\r");

						return true;
					}
				}
				else
				{
					// Installing a HullCube
					Ch->Write("#100[#101Invalid Input#100]#700 You must first enter a HullCube Index.\n\r");
					Ch->Write("Valid Choices #200<#201 1 - %d#200>#700\n\r", pHLoader->m_HullCubes.size());
					return true;
				}
			}
			else
			{
				// Installing a Frame
				Ch->Write("#100[#101Invalid Input#100]#700 You must first enter a name for your Frame.\n\r");
				return true;
			}
		}
		else if (pTemplate->m_nStatus == CTemplate::_AREA)
		{
			// They are trying to assign a Component or Module to the room
			Ch->Write("#100[#101Invalid Input#100]#700 You must enter the type of item you are assigning.\n\r");
			Ch->Write("Syntax#200:#700 Install #200<#201component index ####200>#201/#200<#201list#200>#201/#200<#201exit#200>#700\n\r");
			return true;
		}
		else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
		{
			// They are trying to create a Weapon group
			if (pPlayer->m_Weapon == NULL)
			{
				Ch->Write("#100[#101Invalid Input#100]#700 You must enter a name for the Weapon Group.\n\r");
				Ch->Write("Syntax#200:#700 Install #200<#201Name of Weapon Group#200>#700\n\r");
				return true;
			}
			else
			{
				Ch->Write("#100[#101Invalid Input#100]#700 You must enter a Module Index to add the Weapon Group.\n\r");
				Ch->Write("Syntax#200:#700 Install #200<#201Module Index ####200>#700\n\r");
				return true;
			}

		}
		
	}
	else
	{
		if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
		{
			if (pPlayer->m_Frame)
			{
				// They have a Frame
				if (pPlayer->m_HullCube)
				{
					// They have a Hullcube so we could be installing armour or a frame
					if (gsValue == "armour")
					{
						// Check they entered a valid module
						if (atoi(gsValue1)-1 > pMLoader->m_Modules.size() || atoi(gsValue1)-1 < 0)
						{
							Ch->Write("#200[#201Invalid Input#200] <#701 %s#200>#700 is not a valid Module Index.Choices #200<#201 1 - %d#200>#700\n\r", gsValue1, pMLoader->m_Modules.size());
							return true;
						}

						CModule* pMod = pMLoader->m_Modules.at(atoi(gsValue1)-1);

						// Check its armour
						if (pMod->m_nType != CModule::MT_ARMOUR_PLATING)
						{
							Ch->Write("#100[#101Invalid Type#100]#701 %s #700is not Armour Plating.\n\r", pMod->m_gsName);
							return true;
						}

						if (gsValue2 == "")
						{
							Ch->Write("#100[#101Invalid Entry#100]#701 You didn't enter a durability value.\n\rSyntax#200:#700 Install armour #200<#201Armour Mod Index ####200> <#201Durability#200>#700\n\r");
							return true;
						}

						if (atoi(gsValue2) <= 0)
						{
							Ch->Write("#100[#101Invalid Durability#100]#700 Armour durability must be positive.\n\r");
							return true;
						}
						
						// Add it
						pPlayer->m_HullCube->m_Armour = pMod;
						pMod->m_nmDurability = atoi(gsValue2);
						pMod->m_ncDurability = atoi(gsValue2);

						Ch->Write("%s installed as armour on #201%s#700\n\r", pMod->m_gsName, pPlayer->m_HullCube->m_gsName);
						return true;

					}

					if (pPlayer->m_Component)
					{
						// They have a component so are installing a Module
						if (atoi(gsValue)-1 > pMLoader->m_Modules.size() || atoi(gsValue)-1 < 0)
						{
							Ch->Write("#100[#101Invalid Input#100] <#701 %s#202>#700 is not a valid Module Index.Choices #200<#201 1 - %d#200>#700\n\r", gsValue, pMLoader->m_Modules.size());
							return true;
						}
						else
						{
							CModule* pMod = pMLoader->m_Modules.at(atoi(gsValue)-1);

							// We need to check that their is enough space left in the Component now
							if (pPlayer->m_Component->Size() < pMod->m_nSize)
							{
								Ch->Write("%s only has #201 %d#700 space remaining. #201%s#700 of size #201 %d#700 will not fit.\n\r", pPlayer->m_Component->m_gsName, pPlayer->m_Component->Size(), pMod->m_gsName, pMod->m_nSize);
								return true;
							}
							if (!pPlayer->m_Component->CanInstall(pMod->m_nType))
							{
								int nCount = 0;
								Ch->Write("%s is not engineered to mount #200'#201%s#200'#700 Modules\n\r", pPlayer->m_Component->szClassnames[pPlayer->m_Component->m_Type], pMod->szModules[pMod->m_nType]);
								Ch->Write("%s Accepts#200:\n\r", pPlayer->m_Component->szClassnames[pPlayer->m_Component->m_Type]);
								for (InstallMap::iterator mod = pPlayer->m_Component->m_Install.begin(); mod != pPlayer->m_Component->m_Install.end(); mod++)
								{
									Ch->Write("[#701 %2d#200]#700 %s\n\r", nCount, pMod->szModules[((*mod).first)]);
									nCount++;
								}
								return true;

							}
							else
							{
								// We have the space to install it							
								pPlayer->m_Component->m_Modules.insert(ModuleMap::value_type(pMod->m_nType, pMod));
								Ch->Write("%s installed into #201%s#700\n\r", pMod->m_gsName, pPlayer->m_Component->m_gsName);
								return true;
							}

						}

					}
					else
					{
						// No component so are installing one
						// They didnt enter a valid name
						if (gsValue1 == "")
						{
							Ch->Write("#100[#101Invalid Input#100]#700 You must enter a name for the Component.\n\r");
							Ch->Write("Syntax#200:#701 Install #200<#201type ### of comp#200> <#201name#200> <#201size#200>#700\n\r");
							return true;
						}

						// They didn't enter a valid size
						if (gsValue2 == "" || atoi(gsValue2) <= 0)
						{
							Ch->Write("#100[#101Invalid Input#100]#700 You must enter a positive number for size.\n\r");
							Ch->Write("Syntax#200:#701 Install #200<#201type ### of comp#200> <#201name#200> <#201size#200>#700\n\r");
							return true;
						}

						
						CComponent* pComponent = NULL;

						switch(atoi(gsValue)-1)
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
						
						// Check its valid
						if (!pComponent)
						{
							Ch->Write("#100[#101Invalid Type#100]#201 %d#700 is not a valid Type Index.\n\r", atoi(gsValue));
							Ch->Write("Syntax#200:#701 Install #200<#201### of comp type#200> <#201name#200> <#201size#200>#700\n\r");
							for (int i = 0; i < CComponent::CT_LAST; i++)
								Ch->Write("#200[#701%2d#200]#700 %s\n\r", i+1, CComponent::szComponents[i]);

							return true;
						}

						pComponent->m_gsName = gsValue1;
						pComponent->m_gsShip = pTemplate->m_Ship->m_gsName;
						pComponent->m_nSize = atoi(gsValue2);
						pComponent->m_Type = atoi(gsValue)-1;
						pPlayer->m_HullCube->m_Components.push_back(pComponent);
						Ch->Write("%s added to #201%s#700.\n\r", CComponent::szComponents[atoi(gsValue)-1], pPlayer->m_HullCube->m_gsName);

						return true;						


					}
				}
				else
				{
					// No HullCube so installing one
					if (atoi(gsValue)-1 > pHLoader->m_HullCubes.size() || atoi(gsValue)-1 < 0)
					{
						Ch->Write("#200[#201Invalid Input#200] <#701%s#200>#700 is not a valid HullCube Index.#700\n\r", gsValue);
						Ch->Write("Valid choices: #200<#2011 - %d#200>#700\n\r", pHLoader->m_HullCubes.size());
						Ch->Write("Type#200:#700 hullcube #701For an updated listing#700\n\r");
						return true;
					}

					CHull* pHull = new CHull;
					*pHull = *pHLoader->m_HullCubes.at(atoi(gsValue)-1);
					pPlayer->m_Frame->m_HullCubes.push_back(pHull);
					pHull->m_nCKeel = pHull->m_nMKeel;
					Ch->Write("HullCube#200, '#201%s#200'#700 installed.\n\r", pHull->m_gsName);
					return true;
				}
			}
			else
			{
				// No Frame so installing one
				CFrame* pFrame = new CFrame();
				pFrame->m_gsName = gsValue;
				pTemplate->m_Ship->m_Frames.push_back(pFrame);
				Ch->Write("New Frame#200, '#201%s#200'#700 created.\n\r", gsValue);
				return true;

			}
		}
		else if (pTemplate->m_nStatus == CTemplate::_AREA)
		{

			// They want to view a list of possible options
			if (gsValue == "list")
			{
				// View a List of Components for this HullCube
				int nCount = 1;
				for (FrameList::iterator frame = pTemplate->m_Ship->m_Frames.begin(); frame != pTemplate->m_Ship->m_Frames.end(); frame++)
				for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
				for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
				{
					if ((*hull)->m_nloV <= Ch->CurrentRoom()->Vnum() &&  Ch->CurrentRoom()->Vnum() <= (*hull)->m_nhiV)
					{
						gString gsTemp;
						gsTemp.Format("#200<#700Installed#200:#701 %d#200>#700", (*comp)->m_Installed.Room());
						Ch->Write("#200[#701 %2d#200] [#201%-22s#200]#700 %-25s %s\n\r", nCount, CComponent::szComponents[(*comp)->m_Type], (*comp)->m_gsName, (*comp)->m_Installed.World() != -1 ? gsTemp : "");					
					}
					
					nCount++;
				}
				Ch->Write("#200[#701 %d#200]#700 Components.\n\r", nCount);

			}
			else if (gsValue == "Exit")
			{
				// They want to make this room the Exit room for the Ship
				if (pTemplate->m_Ship)
				{
					pTemplate->m_Ship->m_nExit = Ch->CurrentRoom()->Vnum();
					Ch->Write("Ship Exit installed in room.\n\r");
					return true;
				}
			}
			else
			{
				// They want to install a Component in the room
				// Check its a valid component contained within the ship
				CComponent* pComp = pTemplate->m_Ship->GetComponent(atoi(gsValue)-1);

				// Let them know if they chose wrong
				if (!pComp)
				{
					Ch->Write("#100[#101Invalid Input#100] <#701%s#200>#700 is not a valid Component Index. Type List to view Valid Components.\n\r", gsValue);
					return true;
				}

				// Make sure this Component is part of the HullCube they are in
				CHull* pHull = pComp->GetHull(pTemplate->m_Ship);

				if (pHull->m_nloV > Ch->CurrentRoom()->Vnum() && Ch->CurrentRoom()->Vnum() > pHull->m_nhiV)
				{
					Ch->Write("#100[#101Invalid Cube#100]#701 %s #700does not belong to %s.\n\rYou can only install Components into the HullCube they were assigned.\n\r", pComp->m_gsName, pHull->m_gsName);
					return true;
				}

				// Clear checks now we check for it already being installed
				if (pComp)
				{
					// Its already been installed
					if (pComp->m_Installed.World() != -1)
					{
						Ch->Write("#100[#101Already Installed#100]#701 %s#700 has already been assigned to #201%d#700.\n\r", pComp->m_gsName, pComp->m_Installed.Room());
						Ch->Write("To change this use the #200'#201uninstall#200'#700 command.\n\r");
						return true;
					}
					else
					{
						Ch->CurrentRoom()->AddComponent(pComp->m_gsName);
						pComp->m_Installed.Set(Ch->CurrentRoom()->Area(), Ch->CurrentRoom()->Vnum(), Ch->CurrentRoom()->World());
						Ch->Write("#201%s installed into room.#700.\n\r", pComp->m_gsName);
						#pragma message (Reminder "[CmdsDesign::CmdInstall] Need to create objects for Modules")
					}	
				}
				else
				{
					Ch->Write("#100[#101Invalid Input#100]#701 %s#700 is not a valid Component within %s.\n\r", gsValue1, pTemplate->m_gsName);
					Ch->Write("Type#400: '#201install list#200'#700 to view valid components.\n\r");
					return true;
				}
			}

		}
		else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
		{
			if (pPlayer->m_Weapon)
			{
				if (!pTemplate->m_Ship->GetModule(atoi(gsValue)))
				{
					Ch->Write("#100[#101Invalid Index#100]#701 %d#700 is not a valid Module index.\n\r", atoi(gsValue));
					return true;
				}
				else
				{
					CModule* pMod = pTemplate->m_Ship->GetModule(atoi(gsValue));
					if (pMod->m_nType != CModule::MT_TURRET_MOUNT)
					{
						Ch->Write("#100[#101Invalid Module#100]#701 %s is not a Weapon Module.\n\r", pMod->m_gsName);
						return true;
					}
					else
					{
						// Check if they have already installed this weapon in this Group
						for (IntegerList::iterator it = pPlayer->m_Weapon->m_Weapons.begin(); it != pPlayer->m_Weapon->m_Weapons.end(); it++)
						{
							if (*it == atoi(gsValue))
							{
								Ch->Write("#100[#101Already Installed#100]#701 That weapon is already in this Weapon Group.\n\r");
								return true;
							}
						}


						pPlayer->m_Weapon->m_Weapons.push_back(atoi(gsValue));
						Ch->Write("%s added to Weapon Group\n\r", pMod->m_gsName);

						// Now we update the Weapon Group's orientation
						CWeapon* pWep = (CWeapon*)pTemplate->m_Ship->GetModComp(pMod);

						for (int i = 0; i < CShip::A_MAX; i++)
						{
							if (pWep->m_Orientation->IsSet(i))
								pPlayer->m_Weapon->m_Orientation->SetBit(i);
						}
						return true;
					}
				}

			}
			else
			{
				// We know that gsValue isn't empty now
				CWeaponGroup* pWeapon = new CWeaponGroup();
				pWeapon->m_gsName = gsValue;
				pTemplate->m_Ship->m_Weapons.push_back(pWeapon);

				Ch->Write("Weapon Group installed.\n\r");
				return true;
			}

		}

	}

	return true;

}

// Method     :: CmdDUninstall
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <index # to uninstall>
// Return     :: Bool
// Function   :: Allows a player to uninstall a Frame/HullCube/Component/Module
//			  :: Allows a player to remove a weapon group
// Written    :: 22/12/2005 {OWV}

bool CmdDUninstall::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 1
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Value 2
	
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#201Error#100]#701 %s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;
	CMLoader* pMLoader = CGameObjects::Get().GameWorld()->MLoader();

	// Check for input first of all
	if (gsValue == "" && pTemplate->m_nStatus != CTemplate::_AREA)
	{
		// Response depends on zoom level
		if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
		{
			Ch->Write("#100[#101Invalid Input#100]#701 <%d>#700 is not a valid Index.\n\r", atoi(gsValue));

			if (pPlayer->m_Frame)
			{
				if (pPlayer->m_HullCube)
				{
					if (pPlayer->m_Component)
					{
						// Removing a Module
						Ch->Write("Valid values are#201 1 - %d#700\n\r", pMLoader->m_Modules.size());
						return true;
					}
					else
					{
						// Removing a Component
						Ch->Write("Valid values are#201 1 - %d#700\n\r", pPlayer->m_HullCube->m_Components.size());
						return true;
					}
				}
				else
				{
					// Removing a HullCube
					Ch->Write("Valid values are#201 1 - %d#700\n\r", pPlayer->m_Frame->m_HullCubes.size());
					return true;
				}
			}
			else
			{
				// Removing a Frame
				Ch->Write("Valid values are#201 1 - %d#700\n\r", pTemplate->m_Ship->m_Frames.size());
				return true;
			}
		}
		else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
		{
			Ch->Write("#100[#101Invalid Input#100]#701 <%d>#700 is not a valid Index.\n\r", atoi(gsValue));
			Ch->Write("Valid values are#201 1 - %d#700\n\r", pTemplate->m_Ship->m_Weapons.size());
			return true;
		}

		// We dont have a value to be input for uninstalling a component from the room
	}
	else
	{
		if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
		{
			if (pPlayer->m_Frame)
			{
				if (pPlayer->m_HullCube)
				{
					if (pPlayer->m_Component)
					{
						// Removing a module (Got Valid Frame and Component)
						if (atoi(gsValue) > pPlayer->m_Component->m_Modules.size() || atoi(gsValue)-1 < 0)
						{
							Ch->Write("#200<#201 %d#200>#700 is not a Valid Module Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), pPlayer->m_Component->m_Modules.size());
							return true;
						}
						else
						{
							// Remove the Module
							int nCount = 0;
							ModuleMap::iterator mod;
							for (mod = pPlayer->m_Component->m_Modules.begin(); mod != pPlayer->m_Component->m_Modules.end(); mod++)
							{
								if (nCount == (atoi(gsValue)-1))
									break;

								nCount++;
							}
								
							if (mod != pPlayer->m_Component->m_Modules.end())
							{
								Ch->Write("#201%s#700 erased from #201%s#700\n\r", ((*mod).second)->m_gsName, pPlayer->m_Component->m_gsName);
								pPlayer->m_Component->m_Modules.erase(mod);
							}
							else							
								Ch->Write("#200[#201Error#200]#700 Invalid Module!\n\r");
							
							return true;
						}	
						
					}
					else
					{	
						// Remove the Component
						if (atoi(gsValue) > pPlayer->m_HullCube->m_Components.size() || atoi(gsValue)-1 < 0)
						{
							Ch->Write("#200<#201%d#200>#700 is not a Valid Component Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), pPlayer->m_HullCube->m_Components.size());
							return true;

						}
						else
						{
							pPlayer->m_HullCube->m_Components.erase(pPlayer->m_HullCube->m_Components.begin()+(atoi(gsValue)-1));
							Ch->Write("Component removed from #201%s#200\n\r", pPlayer->m_HullCube->m_gsName);
							return true;
						}

					}

				}
				else
				{
					// Remove the HullCube
					if (atoi(gsValue) > pPlayer->m_Frame->m_HullCubes.size() || atoi(gsValue)-1 < 0)
					{
						Ch->Write("#200<#201 %d#200>#700 is not a Valid HullCube Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), pPlayer->m_Frame->m_HullCubes.size());
						return true;
					}
					else
					{					
						pPlayer->m_Frame->m_HullCubes.erase(pPlayer->m_Frame->m_HullCubes.begin()+(atoi(gsValue)-1));
						Ch->Write("Hullcube removed from #201%s#700\n\r", pPlayer->m_Frame->m_gsName);
						return true;
					}

				}
			}
			else
			{
				// Deleting a Frame
				if (atoi(gsValue) > pTemplate->m_Ship->m_Frames.size() || atoi(gsValue)-1 < 0)
				{
					Ch->Write("#100[#101Invalid Input#100] <#701 %d#200>#700 is not a Valid Frame Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue2), pTemplate->m_Ship->m_Frames.size());
					return true;
				}

				pTemplate->m_Ship->m_Frames.erase(pTemplate->m_Ship->m_Frames.begin()+(atoi(gsValue)-1));
				Ch->Write("Frame removed from #201%s#700\n\r", pTemplate->m_gsName);
				return true;

			}
		}
		else if (pTemplate->m_nStatus == CTemplate::_AREA)
		{
			// Make sure there is a Component in this room
			if (Ch->CurrentRoom()->Components().size() > 0)
			{
				Ch->Write("#201No Components have been installed in this room.#700\n\r");
				return true;
			}
			else
			{
				// Did they enter a valid index to remove?
				if (atoi(gsValue) > Ch->CurrentRoom()->Components().size() || atoi(gsValue)-1 < 0)
				{
					Ch->Write("#100[#101Invalid Input#100] <#701 %d#200>#700 is not a Valid Frame Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), Ch->CurrentRoom()->Components().size());
					return true;
				}

				// Set it and its modules to being uninstalled
				int nCount = 0;
				gStringList::iterator str;
				for (str = Ch->CurrentRoom()->Components().begin(); str != Ch->CurrentRoom()->Components().end(); str++)
				{
					if (nCount == atoi(gsValue) - 1)
						break;				
				}
				
				CComponent* pComp = pTemplate->m_Ship->GetComponent(*str);

				if (!pComp)
				{
					Ch->Write("#100[#101Error#100]#700 Invalid Component. Error Logged.\n\r");
					g_Log.Log(LOG_ERROR, "[CmdDesign::CmdDUninstall] Invalid Component installed in Room %d.\n", Ch->CurrentRoom()->Vnum());
					return true;
				}
				else
				{
					pComp->m_Installed.Set(0,0,-1);
					for (ModuleMap::iterator mod = pComp->m_Modules.begin();mod != pComp->m_Modules.end(); mod++)
					{
						((*mod).second)->m_Installed.Set(0,0,-1);
					}
					Ch->Write("#201Component uninstalled.#700\n\r");
					// Remove the Component
					Ch->CurrentRoom()->Components().erase(str);
					return true;
				}


								

				

			

			}


		}
		else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
		{
			if (pPlayer->m_Weapon)
			{
				// Deleting a Weapon
				if (atoi(gsValue) > pPlayer->m_Weapon->m_Weapons.size() || atoi(gsValue)-1 < 0)
				{
					Ch->Write("#100[#101Invalid Input#100]#200 <#701 %d#200>#700 is not a Valid Weapon Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue2), pPlayer->m_Weapon->m_Weapons.size());
					return true;
				}
				else
				{
					pPlayer->m_Weapon->m_Weapons.erase(pPlayer->m_Weapon->m_Weapons.begin()+(atoi(gsValue)-1));
					Ch->Write("Weapon removed from #201%s#700\n\r", pPlayer->m_Weapon->m_gsName);

					// Now we must update the Arcs of this Weapon Group
					if (pPlayer->m_Weapon->m_Weapons.size() > 0)
					{		
						for (IntegerList::iterator it = pPlayer->m_Weapon->m_Weapons.begin(); it != pPlayer->m_Weapon->m_Weapons.end(); it++)
						{
							CWeapon* pWep = (CWeapon*)pPlayer->m_Template->m_Ship->GetModComp(pPlayer->m_Template->m_Ship->GetModule(*it));
							for (int i = 0; i < CShip::A_MAX; i++)
								if (pPlayer->m_Weapon->m_Orientation->IsSet(i))
									pPlayer->m_Weapon->m_Orientation->RemoveBit(i);
							
							for (int i = 0; i < CShip::A_MAX; i++)
								if (pWep->m_Orientation->IsSet(i))
									pPlayer->m_Weapon->m_Orientation->SetBit(i);

						}

					}
					else
					{
						for (int i = 0; i < CShip::A_MAX; i++)
								if (pPlayer->m_Weapon->m_Orientation->IsSet(i))
									pPlayer->m_Weapon->m_Orientation->RemoveBit(i);
					}


					return true;
				}
			}
			else
			{
				// Deleting a Weapon Group
				if (atoi(gsValue) > pTemplate->m_Ship->m_Weapons.size() || atoi(gsValue)-1 < 0)
				{
					Ch->Write("#100[#101Invalid Input#100]#200 <#701 %d#200>#700 is not a Valid Weapon Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue2), pTemplate->m_Ship->m_Weapons.size());
					return true;
				}
				else
				{
					pTemplate->m_Ship->m_Weapons.erase(pTemplate->m_Ship->m_Weapons.begin()+(atoi(gsValue)-1));
					Ch->Write("Weapon Group removed.#700\n\r");
					return true;
				}

			}

		}

	}


	return true;

}

// Method     :: CmdDSave
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to save his progress on a Template
// Written    :: 22/12/2005 {OWV}

bool CmdDSave::Perform(CActor* Ch, gStringList& CommandLine)
{
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#701 %s #700has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	if (pTemplate->Save())
	{
		Ch->Write("#201Template saved.#700\n\r");
		return true;
	}
	else
	{
		Ch->Write("#101Unable to save Template.#700\n\r");
		return true;
	}

	return true;

}

// Method     :: CmdDFinish
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to declare a section of the Template finished
//			  :: Template -> Area -> Weapon Groups -> Review -> Authorisation -> Finished
// Written    :: 23/12/2005 {OWV}

bool CmdDFinish::Perform(CActor* Ch, gStringList& CommandLine)
{

	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Vnum from


	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#702 %s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// [1] Template -> Area
	
	// TESTING TEMP
	if (gsValue == "reset")
	{
		pPlayer->m_Template->m_nStatus = CTemplate::_TEMPLATE;
		return true;
	}
	

	if (pPlayer->m_Template->m_nStatus == CTemplate::_TEMPLATE)
	{
		// We will need to check here they that they have a valid Template

		// Generate their shape
		pPlayer->m_Template->m_Ship->m_Shape->SetHeight(pPlayer->m_Template->m_Ship);
		pPlayer->m_Template->m_Ship->m_Shape->SetWidth(pPlayer->m_Template->m_Ship);
		pPlayer->m_Template->m_Ship->m_Shape->SetLength(pPlayer->m_Template->m_Ship);
		pPlayer->m_Template->m_Ship->m_Shape->SetSize(pPlayer->m_Template->m_Ship);
	
		// Make sure they have actually defined some size using HullCubes
		if (pPlayer->m_Template->m_Ship->m_Shape->Size() <= 0)
		{
			Ch->Write("#100[#101Incomplete Design#100]#700 Your design must include some HullCubes with defined sizes.\n\r");
			return true;
		}

		// Advance to the next step
		CArea* pArea = NULL;

		// Only generate an area if it doesn't already exist

		if (pPlayer->m_Template->m_Area->Rooms()->size() == 0 || gsValue == "generate")
		{
			// If we are generating the area we need to Uninstall any Components/Modules that were
			// previously installed.
			for (FrameList::iterator frame = pPlayer->m_Template->m_Ship->m_Frames.begin(); frame != pPlayer->m_Template->m_Ship->m_Frames.end(); frame++)
			{
				for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
					{
						(*comp)->m_Installed.Set(0, 0, -1);
						for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
							((*mod).second)->m_Installed.Set(0, 0, -1);

					}
			}
			pArea = pPlayer->m_Template->m_Ship->m_Shape->Generate(pPlayer->m_Template->m_Ship);
		}
		else
			pArea = pPlayer->m_Template->m_Area;

		if (!pArea)
		{
			Ch->Write("#100[#101Critical Error#100]#700 Null area file.\n\r");
			g_Log.Log(LOG_ERROR, "[CmdDesign::CmdDFinish] Shape Generate for %s failed.\n", pPlayer->m_Template->m_gsName);
			return true;
		}

		pArea->Flags()->SetBit(CArea::_TEMPLATE);	// Set the Area flags

		// Assign area file to ship
		pTemplate->m_Area = pArea;

		// Send the player to the first room
		pPlayer->SetCurrentRoom(pArea->GetRoom(0));

		// Make them look
		pPlayer->ExecuteCommand("look", "");

		Ch->Write("Entering Ship Area Design mode.\n\r");

		// Increment our Status
		pTemplate->m_nStatus++;

	}
	else if (pPlayer->m_Template->m_nStatus == CTemplate::_AREA)
	{
		Ch->Write("Design Template Area Design completed.\n\r");
		
		// Are they in a room specific to this Template? #TODO# Crude hack, fix it
		if (Ch->CurrentRoom()->Area() == pPlayer->m_Template->m_Area->Area())
		{
			CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(CGameObjects::Get().ConfigData().GetInt("default_area", "Options"));
			Ch->SetCurrentRoom((*pArea->Rooms()->begin()).second);
		}

		Ch->Write("You now have the ability to define #201Weapon Groups#700 for your ship.\n\rAvailable Commands#200:#701 Show, List, Install, Uninstall, Edit.\n\r");
		pTemplate->m_nStatus++;
	}
	else if (pPlayer->m_Template->m_nStatus == CTemplate::_WEAPONS)
	{
		if (pPlayer->m_Template->m_Ship->m_Weapons.size() <= 0)
		{
			Ch->Write("You have not defined any Weapon Configurations, this will be left to the Ship Captain now.\n\r");
			Ch->Write("Design Template Weapon Config completed.\n\r");
		}
		else
		{
			Ch->Write("Design Template Weapon Config completed.\n\r");
		}
		Ch->Write("You now have the ability to #201review#700 your Design.\n\rType#200:#701 review #200<#201template#200|#201area#200>#700 to return to a specific area of the Design.\n\r");

		pTemplate->m_nStatus++;
	}
	else if (pPlayer->m_Template->m_nStatus == CTemplate::_REVIEW)
	{
		Ch->Write("Review completed, Template submitted for #201Administrator authorisation.#700\n\r");
		pTemplate->m_gsAuthorisor = "";
		pTemplate->m_nStatus++;
	}


	return true;

}


// Method     :: CmdDList
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <ungrouped> Lists ungrouped weapons for Weapon grouping
// Return     :: Bool
// Function   :: Allows a player to view the contents of the current level of detail
//			  :: Frame    : Lists the hullcubes contained within the frame
//			  :: HullCube : Lists the Components contained within the cube
//			  :: Component: Lists the modules contained within the component
//			  :: Wep Group: Lists available weapons
// Written    :: 22/12/2005 {OWV}

bool CmdDList::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();	// Function to perform

	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#701 %s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
	{
		if (pPlayer->m_Frame)
		{
			if (pPlayer->m_HullCube)
			{
				if (pPlayer->m_Component)
				{
					// Display all its Modules
					Ch->Write("Hull Frame, \"%s\"\n\r", pPlayer->m_Frame->m_gsName);
					Ch->Write(" #200>#201>#700 Hull Cube#200, %-35s#700 ExtDim#200-(#201 %d#200,#201 %d#200,#201 %d#200)#700 Pos#200-(#201 %d#200,#201 %d#200,#201 %d#200)#700 IntDim#200-(#201 %d#200,#201 %d#200,#201 %d#200)\n\r", "#200\"#701" + pPlayer->m_HullCube->m_gsName + "#200\"", pPlayer->m_HullCube->m_EDimension->m_nWidth, pPlayer->m_HullCube->m_EDimension->m_nHeight, pPlayer->m_HullCube->m_EDimension->m_nLength, pPlayer->m_HullCube->m_EDimension->m_Location->x, pPlayer->m_HullCube->m_EDimension->m_Location->y, pPlayer->m_HullCube->m_EDimension->m_Location->z, pPlayer->m_HullCube->m_nWidth, pPlayer->m_HullCube->m_nHeight, pPlayer->m_HullCube->m_nLength);
					Ch->Write("    #700Cover Arcs#200:#701 ");
					bool bFound = false;
					for (int i = 0; i < CShip::A_MAX; i++)
					{
						if (pPlayer->m_HullCube->m_Cover->IsSet(i))
						{
							Ch->Write("%s ", CShip::szArc[i]);
							bFound = true;
						}
					}
					if (!bFound)
							Ch->Write("#101None#700 ");

					Ch->Write("#700\n\r\n\r");
					// Display the Component details
					Ch->Write("   #200>#201>#700 %s#200, \"#700%s#200\"#700 Size#200: <#201 %d#200|#201 %d#200>#700\n\r", CComponent::szComponents[pPlayer->m_Component->m_Type], pPlayer->m_Component->m_gsName, pPlayer->m_Component->Size(), pPlayer->m_Component->m_nSize);
					// If the component is a Shield or Weapon it has other details we need to show
					if (pPlayer->m_Component->m_Type == CComponent::CT_SHIELD || pPlayer->m_Component->m_Type == CComponent::CT_WEAPONMOUNT)
					{
						bool bFound = false;
						// Display the Orientation
						Ch->Write("      Orientation#200:#701 "); 
						for (int i = 0;i < CShip::A_MAX; i++)
						{
							if (pPlayer->m_Component->m_Type == CComponent::CT_WEAPONMOUNT) 
							if (((CWeapon*)pPlayer->m_Component)->m_Orientation->IsSet(i))
							{
								Ch->Write("%s ", CShip::szArc[i]);
								bFound = true;
							}

							if (pPlayer->m_Component->m_Type == CComponent::CT_SHIELD)
							if (((CShield*)pPlayer->m_Component)->m_Orientation->IsSet(i))
							{
								Ch->Write("%s ", CShip::szArc[i]);
								bFound = true;
							}
						}

						if (!bFound)
							Ch->Write("#101None#700 ");

						Ch->Write("#700\n\r");
					}
					if (pPlayer->m_Component->m_Modules.size() > 0)
					{
						Ch->Write("\n\r");
						int nCount = 0;
						for (ModuleMap::iterator mod = pPlayer->m_Component->m_Modules.begin(); mod != pPlayer->m_Component->m_Modules.end(); mod++)
						{
							nCount++;
							Ch->Write("    #200>#201> #200[#201 %2d#200]#700 %-35s Size#200-(#201 %4d#200) #700Mass#200-(#201 %d#200)#700 \n\r", nCount, ((*mod).second)->m_gsName, ((*mod).second)->m_nSize, ((*mod).second)->m_nMass );
						}
					}
					else
					{
						Ch->Write("    #200<#201<#700 No installed Modules\n\r");
					}

				}
				else
				{
					// Display its Components
					Ch->Write("Hull Frame, \"%s\"\n\r", pPlayer->m_Frame->m_gsName);
					Ch->Write(" #200>#201>#700 Hull Cube#200, %-35s#700 ExtDim#200-(#201 %d#200,#201 %d#200,#201 %d#200)#700 Pos#200-(#201 %d#200,#201 %d#200,#201 %d#200)#700 IntDim#200-(#201 %d#200,#201 %d#200,#201 %d#200)\n\r", "#200\"#701" + pPlayer->m_HullCube->m_gsName + "#200\"", pPlayer->m_HullCube->m_EDimension->m_nWidth, pPlayer->m_HullCube->m_EDimension->m_nHeight, pPlayer->m_HullCube->m_EDimension->m_nLength, pPlayer->m_HullCube->m_EDimension->m_Location->x, pPlayer->m_HullCube->m_EDimension->m_Location->y, pPlayer->m_HullCube->m_EDimension->m_Location->z, pPlayer->m_HullCube->m_nWidth, pPlayer->m_HullCube->m_nHeight, pPlayer->m_HullCube->m_nLength);
					
					Ch->Write(" #700 Cover Arcs#200:#701 ");
					bool bFound = false;
					for (int i = 0; i < CShip::A_MAX; i++)
					{
						if (pPlayer->m_HullCube->m_Cover->IsSet(i))
						{ 
							Ch->Write("%s ", CShip::szArc[i]);
							bFound = true;
						}
					}
					if (!bFound)
							Ch->Write("#101None#700 ");
					if (pPlayer->m_HullCube->m_Armour)
							Ch->Write("    Armour#200:#701 %s #200[#700Dissipation#200:#201 %d #700Durability#200:#201 %d#200]#700\n\r", pPlayer->m_HullCube->m_Armour->m_gsName, pPlayer->m_HullCube->m_Armour->Plus("Dissipation", false), pPlayer->m_HullCube->m_Armour->m_ncDurability);

					Ch->Write("#700\n\r\n\r");
					if (pPlayer->m_HullCube->m_Components.size() > 0)
					{
						int nCount = 1;
						for (ComponentList::iterator comp = pPlayer->m_HullCube->m_Components.begin(); comp != pPlayer->m_HullCube->m_Components.end(); comp++)
						{
							Ch->Write("   #200>#201> #200[#201 %2d#200]#700 %s#200, \"#701%s#200\"#700 Size#200: <#201 %d#200|#201 %d#200>#700\n\r", nCount, CComponent::szComponents[(*comp)->m_Type], (*comp)->m_gsName, (*comp)->Size(), (*comp)->m_nSize);
							// If the component is a Shield or Weapon it has other details we need to show
							if ((*comp)->m_Type == CComponent::CT_SHIELD || (*comp)->m_Type == CComponent::CT_WEAPONMOUNT)
							{
								bool bFound = false;
								// Display the Orientation
								Ch->Write("      #700Orientation#200:#701 "); 
								for (int i = 0;i < CShip::A_MAX; i++)
								{
									if ((*comp)->m_Type == CComponent::CT_WEAPONMOUNT) 
									if (((CWeapon*)(*comp))->m_Orientation->IsSet(i))
									{
										Ch->Write("%s ", CShip::szArc[i]);
										bFound = true;
									}

									if ((*comp)->m_Type == CComponent::CT_SHIELD)
									if (((CShield*)(*comp))->m_Orientation->IsSet(i))
									{
										Ch->Write("%s ", CShip::szArc[i]);
										bFound = true;
									}
								}

								if (!bFound)
									Ch->Write("#101None#700 ");

								Ch->Write("#700\n\r");
							}
							nCount++;
						}
					}
					else
					{
						Ch->Write("   #200<#201<#700 No installed Components\n\r");
					}

				}
			}
			else
			{
				// Display all its HullCubes
				if (pPlayer->m_Frame->m_HullCubes.size() > 0)
				{

					int nCount = 1;
					Ch->Write("Hull Frame#200, \"#701%s#200\"#700\n\r", pPlayer->m_Frame->m_gsName);
					for (HullList::iterator hull = pPlayer->m_Frame->m_HullCubes.begin(); hull != pPlayer->m_Frame->m_HullCubes.end(); hull++)
					{
						Ch->Write(" #200[#201 %2d#200]#700 Hull Cube#200, %-35s#700 ExtDim#200-(#201 %d#200,#201 %d#200,#201 %d#200)#700 Pos#200-(#201 %d#200,#201 %d#200,#201 %d#200)#700 IntDim#200-(#201 %d#200,#201 %d#200,#201 %d#200)\n\r", nCount, "#200\"#701" + (*hull)->m_gsName + "#200\"", (*hull)->m_EDimension->m_nWidth, (*hull)->m_EDimension->m_nHeight, (*hull)->m_EDimension->m_nLength, (*hull)->m_EDimension->m_Location->x, (*hull)->m_EDimension->m_Location->y, (*hull)->m_EDimension->m_Location->z, (*hull)->m_nWidth, (*hull)->m_nHeight, (*hull)->m_nLength);
						Ch->Write("    #700Cover Arcs#200:#701 ");
						bool bFound = false;
						for (int i = 0; i < CShip::A_MAX; i++)
						{
							if ((*hull)->m_Cover->IsSet(i))
							{
								Ch->Write("%s ", CShip::szArc[i]);
								bFound = true;
							}
						}
						if (!bFound)
								Ch->Write("#101None#700 ");
						Ch->Write("#700\n\r");
						if ((*hull)->m_Armour)
							Ch->Write("    Armour#200:#701 %s #200[#700Dissipation#200:#201 %d #700Durability#200:#201 %d#200]#700\n\r", (*hull)->m_Armour->m_gsName, (*hull)->m_Armour->Plus("Dissipation", false), (*hull)->m_Armour->m_ncDurability);
						
						Ch->Write("    Keel integrity#200:#701 %d#700\n\r", (*hull)->m_nMKeel);

						Ch->Write("\n\r");						
						nCount++;
					}
				}
				else
				{
					Ch->Write(" #200<#201<#700 No installed Hull Cubes.\n\r");
					return true;
				}

			}
		}
		else
		{
			// Display all the Frames
			int nCount = 1;
			for (FrameList::iterator fra = pTemplate->m_Ship->m_Frames.begin(); fra != pTemplate->m_Ship->m_Frames.end(); fra++)
			{
				Ch->Write("#200[#201 %2d#200]#700 Hull Frame#200, %-30s >#201>#700 Total Frame Mass#200:#701 %10d #700kg\n\r", nCount, "#200\"#701"+(*fra)->m_gsName+"#200\"", (*fra)->Mass());
				nCount++;
			}
		}
	}
	else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
	{
		// List all Weapons not in a Group
		gStringImap* giWeapons = new gStringImap();

		int nCounter = 1;
		// First of all we create a Map containing all the Weapon Modules indexed by their Module Index
		for (FrameList::iterator frame = pTemplate->m_Ship->m_Frames.begin(); frame != pTemplate->m_Ship->m_Frames.end(); frame++)
		{
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			{
				for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
				{	
					for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
					{
						if ((*comp)->m_Type == CComponent::CT_WEAPONMOUNT)
						{
							if ((*mod).second->m_nType == CModule::MT_TURRET_MOUNT)
							{
								// Add weapon to Map
								giWeapons->insert(gStringImap::value_type(nCounter, (*mod).second->m_gsName));
							}

						}

						nCounter++;
					}
				}
			}
		}

		if (gsValue == "Ungrouped")
		{
			// Now we remove any Weapons from the Map that are already in a Weapon Group
			for (WeaponGroupList::iterator it = pTemplate->m_Ship->m_Weapons.begin(); it != pTemplate->m_Ship->m_Weapons.end(); it++)
			{
				for (IntegerList::iterator i = (*it)->m_Weapons.begin(); i != (*it)->m_Weapons.end(); i++)
				{
					// Remove the Module from our Weapon List
					if (giWeapons->find(*i) != giWeapons->end())
						giWeapons->erase(*i);
				}
			}

			// Now we display the unassigned weapons
			Ch->Write("Ungrouped Weapons\n\r");
			for (gStringImap::iterator imap = giWeapons->begin(); imap != giWeapons->end(); imap++)
			{
				Ch->Write("#200[#201 %3d#200]#700 %-30s #200>#201>#701 ", (*imap).first, (*imap).second);

				CModule* pMod = pTemplate->m_Ship->GetModule((*imap).first);
				CWeapon* pWep = (CWeapon*)pTemplate->m_Ship->GetModComp(pMod);
				for (int i = 0; i < CShip::A_MAX; i++)
				{
					if (pWep->m_Orientation->IsSet(i))
						Ch->Write("%s ", CShip::szArc[i]);
				}
				Ch->Write("#700\n\r");
			}
		}
		else
		{
			// Display all Weapons
			Ch->Write("Weapons\n\r");
			for (gStringImap::iterator imap = giWeapons->begin(); imap != giWeapons->end(); imap++)
			{
				Ch->Write("#200[#201 %3d#200]#700 %-30s #200>#201>#701 ", (*imap).first, (*imap).second);
				
				CModule* pMod = pTemplate->m_Ship->GetModule((*imap).first);
				CWeapon* pWep = (CWeapon*)pTemplate->m_Ship->GetModComp(pMod);
				for (int i = 0; i < CShip::A_MAX; i++)
				{
					if (pWep->m_Orientation->IsSet(i))
						Ch->Write("%s ", CShip::szArc[i]);
				}
				Ch->Write("#700\n\r");
			}
			Ch->Write("To view Ungrouped weapons type#200:#701 list ungrouped.#700\n\r");
			return true;
		}

	}

	return true;

}

// Method     :: CmdDShow
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to view the current status of the Template/Area
//			  :: Template: Provides overview of ship
//			  :: Area    : Provides GUI of rooms
// Written    :: 22/12/2005 {OWV}

bool CmdDShow::Perform(CActor* Ch, gStringList& CommandLine)
{
	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("[No Template] You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("[Error] %s has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// Are we showing the Template status
	if (pTemplate->m_nStatus == CTemplate::_TEMPLATE)
	{

		Ch->Write("#200:#201:#700 %s by %s #201 :#200:#700\n\r", pTemplate->m_gsName, pTemplate->m_gsAuthor);
		Ch->Write("#200<#201%s#200>#700\n\r", pTemplate->m_gfFileName);
		
		if (pTemplate->m_Ship)
		{
			int nCounter = 0;
			if (pTemplate->m_Ship->m_Shape)
			{
				Ch->Write("#200>#201>#701 Shape#700\n\r");
				Ch->Write(" Length#200:#701 %4dm \n\r #700Width#200:#701  %4dm \n\r #700Height#200:#701 %4dm#700\n\r", pTemplate->m_Ship->m_Shape->Length(), pTemplate->m_Ship->m_Shape->Width(), pTemplate->m_Ship->m_Shape->Height(), pTemplate->m_Ship->m_Shape->Size());
				/*
				if (pTemplate->m_Ship->m_Shape->m_Center)
					Ch->Write(" Center#200:#701 <%d %d %d>#700\n\r", pTemplate->m_Ship->m_Shape->m_Center->x, pTemplate->m_Ship->m_Shape->m_Center->y, pTemplate->m_Ship->m_Shape->m_Center->y);
				else
					Ch->Write(" Center#200:#701 Not defined.#700\n\r");			 */
			}
			Ch->Write("#200>#201>#701 Ship#700\n\r");
			Ch->Write(" Class#200:#701 %s#700\n\r", CShip::szTypes[pTemplate->m_Ship->m_nClass]);
			Ch->Write(" Designation#200:#701 %s#700\n\r", pTemplate->m_Ship->m_gsDesignation);
			Ch->Write(" Type#200:#701 %s#700\n\r", pTemplate->m_Ship->m_gsType);
			if (pTemplate->m_Ship->m_gsExit != "")
			{
				Ch->Write("#200>#201>#701 Exit#700\n\r");
				Ch->Write(" Name#200:#701     %s#700\n\r", pTemplate->m_Ship->m_gsExit);
				Ch->Write(" OpenMsg#200:#701  %s#700\n\r", pTemplate->m_Ship->m_gsOMsg);
				Ch->Write(" CloseMsg#200:#701 %s#700\n\r", pTemplate->m_Ship->m_gsCMsg);
			}
			Ch->Write("#200>#201>#701 Stats#700\n\r");
			Ch->Write(" Total Mass#200:#701 %d kg#700\n\r", pTemplate->m_Ship->Mass());
			Ch->Write(" Speed#200:#701   %d MGLT#700\n\r", pTemplate->m_Ship->TopSpeed(false));
			Ch->Write(" Shield#200:#701  %d#700\n\r", pTemplate->m_Ship->Shield(false));
			Ch->Write("#200>#201>#701 Energy Load#700\n\r");
			Ch->Write(" Minimum#200:#701 %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_MIN));
			Ch->Write(" Idle#200:#701    %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_IDLE));
			Ch->Write(" Normal#200:#701  %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_NORMAL));
			Ch->Write(" Combat#200:#701  %5d#700\n\r", pTemplate->m_Ship->Energy(CShip::ET_COMBAT));
			Ch->Write(" Maximum#200:#701 %5d\n\r", pTemplate->m_Ship->Energy(CShip::ET_MAX));
			Ch->Write("#200>#201>#701 Generation#700\n\r");
			Ch->Write(" Coolant#200:#701 %5d#700\n\r", pTemplate->m_Ship->Coolant(true));
			Ch->Write(" Power#200:#701   %5d#700\n\r", pTemplate->m_Ship->MaxPower());

			Ch->Write("\n\r");

			if (pTemplate->m_Ship->m_Frames.size() > 0)
			{
				int i = 0;
				for (FrameList::iterator frame = pTemplate->m_Ship->m_Frames.begin(); frame != pTemplate->m_Ship->m_Frames.end(); frame++)
				{
					i++;
					Ch->Write("#200[#701 %2d#200]#700 Hull Frame, #201%-25s #700Total Mass#200:#701 %8d kg #700 ### HullCubes#200:#701 %2d#700\n\r", i, ("#200\"" +(*frame)->m_gsName + "#200\""), (*frame)->Mass(), (*frame)->m_HullCubes.size());
				}
			}				

		}
		else
			Ch->Write("Ship structure:\n\r No Ship structure created.\n\r");
	}

	// Showing the Area
	else if (pTemplate->m_nStatus == CTemplate::_AREA)
	{
		// Display all Hull Cubes
		int nCount = 1;
		for (FrameList::iterator fra = pTemplate->m_Ship->m_Frames.begin(); fra != pTemplate->m_Ship->m_Frames.end(); fra++)
		{
			Ch->Write("Hull Frame, \"%s\"\n\r", (*fra)->m_gsName);
			for (HullList::iterator hull = (*fra)->m_HullCubes.begin(); hull != (*fra)->m_HullCubes.end(); hull++)
			{
				Ch->Write(" (%2d) Hull Cube, \"%s\" ExtDim-(%d,%d,%d) @ Pos-(%d, %d, %d) IntDim-(%d,%d,%d)\n\r", nCount, (*hull)->m_gsName, (*hull)->m_EDimension->m_nWidth, (*hull)->m_EDimension->m_nHeight, (*hull)->m_EDimension->m_nLength, (*hull)->m_EDimension->m_Location->x, (*hull)->m_EDimension->m_Location->y, (*hull)->m_EDimension->m_Location->z, (*hull)->m_nWidth, (*hull)->m_nHeight, (*hull)->m_nLength);
				Ch->Write("    Cover Arcs:");
				for (int i = 0;i < CShip::A_MAX; i++)
				{
					if ((*hull)->m_Cover->IsSet(i))
						Ch->Write("%s ", CShip::szArc[i]);
				}
				Ch->Write("\n\r");
				if ((*hull)->m_Armour)
					Ch->Write("  Armour: %s {Dissipation: %d Durability %d}\n\r", (*hull)->m_Armour->m_gsName, (*hull)->m_Armour->Plus("Dissipation", false), (*hull)->m_Armour->m_ncDurability);
				nCount++;

				Ch->Write((*hull)->Gui());
			}
		}

	}
	// The Weapon Configuration screen
	else if (pTemplate->m_nStatus == CTemplate::_WEAPONS)
	{
		if (pPlayer->m_Weapon == NULL)
		{
			if (pTemplate->m_Ship->m_Weapons.size() <= 0)
			{
				Ch->Write("No Weapon Groups defined yet.\n\r");
				return true;
			}
			else
			{
				// Show the Weapon Groups
				int nCount = 1;
				for (WeaponGroupList::iterator it = pTemplate->m_Ship->m_Weapons.begin(); it != pTemplate->m_Ship->m_Weapons.end(); it++)
				{
					Ch->Write("#200[#201 %3d#200]#700 %-30s #200>#201>#700 Weapons #200[#201 %3d#200] :#700 %s\n\r", nCount, (*it)->m_gsName, (*it)->m_Weapons.size(), CWeaponGroup::szTypes[(*it)->m_nType]);
					nCount++;
				}

			}
		}
		else
		{
			Ch->Write("Weapon Group#200, \"#701%s#200\"#700\n\r", pPlayer->m_Weapon->m_gsName);
			Ch->Write("  Type#200:#701 %s#700\n\r", CWeaponGroup::szTypes[pPlayer->m_Weapon->m_nType]);
			Ch->Write("  Arcs#200:#701 ");
			bool bFound = false;
			for (int i = 0; i < CShip::A_MAX; i++)
			{
				if (pPlayer->m_Weapon->m_Orientation->IsSet(i))
				{
					Ch->Write("%s ", CShip::szArc[i]);
					bFound = true;
				}
			}

			if (bFound)
				Ch->Write("\n\r\n\r");
			else
				Ch->Write("None\n\r\n\r");

			int nCount = 1;

			if (pPlayer->m_Weapon->m_Weapons.size() <= 0)
				Ch->Write("   #200>#201>#700 No Weapons in group.\n\r");

			for (IntegerList::iterator it = pPlayer->m_Weapon->m_Weapons.begin(); it != pPlayer->m_Weapon->m_Weapons.end(); it++)
			{
				Ch->Write("   #200[#201 %3d#200]#700 %-30s #200[#700MI####200: #201 %3d#200]#700\n\r", nCount, pTemplate->m_Ship->GetModule(*it)->m_gsName, *it);
				nCount++;
			}
		}

	}

	// Show a Message stating the status of the Template
	if (pTemplate->m_nStatus == CTemplate::_AUTHORISATION || pTemplate->m_nStatus == CTemplate::_COMPLETED)
	{

		if (Ch->IsAdministrator())
		{
			Ch->Write(" #200:#201:#701 This Template has already been completed. To open it for editing type#200:#701 designs open #200<#201####200> #201:#200:#700  \n\r");
		}
		else
		{
			Ch->Write(" #200:#201:#701 This Template has already been completed. Only an Administrator can open it for editing again. #201:#200:#700  \n\r");
		}
	}

	return true;
}

// Method     :: CmdDLink
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <vnum> <direction> <vnum>
// Return     :: Bool
// Function   :: Allows a player to link together two rooms
// Written    :: 24/12/2005 {OWV}

bool CmdDLink::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Vnum from
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Direction
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue2 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Vnum to

	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	//.Make sure they have a ship structure before we attempt to link hullcubes
	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#701 %s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// Link can only be used when editing the Area of the Template
	if (pTemplate->m_nStatus == CTemplate::_AREA)
	{
		// [1] :: Did they enter values
		if (gsValue == "" || gsValue1 == "" || gsValue2 == "")
		{
			Ch->Write("#100[#101Invalid Input#100]#700 You must supply values.\n\r");
			Ch->Write("Syntax#200:#701 Link #200<#201Vnum from#200> <#201Direction#200> <#201Vnum to#200>\n\r");
			return true;
		}

		// [2] :: Did they enter a valid Vnum from
		if (pTemplate->m_Area->Rooms()->size() < atoi(gsValue) || atoi(gsValue) < 0)
		{
			Ch->Write("#100[#101Invalid Vnum#100]#700 No such room has vnum #200<#201%d#200>#700.\n\r", atoi(gsValue));
			Ch->Write("Valid range#200:#201 0 - %d#700\n\r", pTemplate->m_Area->Rooms()->size());	// We can use zero because
																						// areas are generated by
																						// the code
			return true;
		}

		// [3] :: Did they enter a valid Direction
		// Runs from 1 to ignore None

		bool bFound = false;
		for (int i = 1; i < CExit::NUMEXITS; i++)
		{
			gString gsExit = CExit::szExitNames[i];
			if (gsExit == gsValue1)
				bFound = true;			
		}

		if (!bFound)
		{
			Ch->Write("#100[#101Invalid Direction#100]#701 %s#700 is not a valid Direction, valid directions are#200:\n\r", gsValue1);
			for (int j = 1; j < CExit::NUMEXITS; j++)
			{
				Ch->Write("[#701 %d#200]#700 %s\n\r", j, CExit::szExitNames[j]);
					
			}
		}

		// [4] :: Did they enter a valid Vnum to
		if (pTemplate->m_Area->Rooms()->size() < atoi(gsValue2) || atoi(gsValue2) < 0)
		{
			Ch->Write("#100[#101Invalid Vnum#100]#700 No such room has vnum #200<#201 %d#200>#700\n\r", atoi(gsValue2));
			Ch->Write("Valid range#200:#201 0 - %d#700\n\r", pTemplate->m_Area->Rooms()->size());	
			return true;
		}

		// [5] :: Did they enter the same Vnum from as To
		if (atoi(gsValue) == atoi(gsValue2))
		{
			Ch->Write("#100[#101Invalid Input#100]#700 Cannot link to the same room!\n\r");
			return true;
		}

		// Its safe to get the Area now... I guess...
		CArea* pArea = pTemplate->m_Area;

		// Sanity check
		if (!pArea)
		{
			g_Log.Log(LOG_ERROR, "[CmdDesigns::CmdDLink] Invalid Area for Design Template: <%s>", pTemplate->m_gsName);
			return false;
		}

		// Get the supplied rooms
		CRoom* pFrom = pArea->GetRoom(atoi(gsValue));
		CRoom* pTo = pArea->GetRoom(atoi(gsValue2));

		// Extra Sanity check
		if (!pFrom || !pTo)
		{
			g_Log.Log(LOG_ERROR, "[CmdDesigns::CmdDLink] Invalid Room for Linking Template: <%s>", pTemplate->m_gsName);
			return false;
		}

		// [6] :: Does this exit already exist?
		for (ExitList::iterator exi = pFrom->Exits().begin(); exi != pFrom->Exits().end(); exi++)
		{
			if ((*exi)->Direction() == CExit::GetDirection(gsValue1))
			{
				Ch->Write("#100[#101Existing Exit#100]#700 Exit already exists. Use unlink to remove it.\n\r"
					);
				return true;
			}
		}

		// [7] :: Clear validation and ok to add the exit
		CExit* pEFrom = new CExit();
		CExit* pETo = new CExit();

		// Set the Exit from
		pEFrom->m_Direction = CExit::GetDirection(gsValue1);
		pEFrom->m_Destination = CPlacement(pArea->Area(), pTo->Vnum(), pArea->World());

		gString gsExit;

		// Set the Exit to
		switch(CExit::GetDirection(gsValue1))
		{
			case CExit::NORTH:	gsExit = "South"; break;
			case CExit::SOUTH:	gsExit = "North"; break;
			case CExit::WEST:	gsExit = "East"; break;
			case CExit::EAST:	gsExit = "West"; break;
			case CExit::UP:		gsExit = "Down"; break;
			case CExit::DOWN:	gsExit = "Up"; break;
		}

		pETo->m_Direction = CExit::GetDirection(gsExit);
		pETo->m_Destination = CPlacement(pArea->Area(), pFrom->Vnum(), pArea->World());


		// Add the exits in
		pFrom->Exits().push_back(pEFrom);
		pTo->Exits().push_back(pETo);

		Ch->Write("HullCubes #201linked#700.\n\r");
		return true;

	}

	Ch->Write("#100[#101Invalid Command#100]#701 Link can only be used during Area Design.#700\n\r");
	Ch->Write("Syntax#200:#701 Link #200<#201Vnum from#200> <#201Direction#200> <#201Vnum to#200>\n\r");
	return true;

}

// Method     :: CmdDUnlink
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <direction>
// Return     :: Bool
// Function   :: Allows a player to unlink two rooms (Forgot to write this, ops!)
// Written    :: 15/03/2006 {OWV}

bool CmdDUnlink::Perform(CActor* Ch, gStringList& CommandLine)
{
	LOG_SCOPE("CmdDesigns::CmdDUnlink");
	// Process argument
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Direction

	// Current room iterator
	ExitList::iterator pos;
	std::vector<ExitList::iterator>DeleteList;

	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	//.Make sure they have a ship structure before we attempt to link hullcubes
	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#701 %s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// Link can only be used when editing the Area of the Template
	if (pTemplate->m_nStatus == CTemplate::_AREA)
	{
		// [1] :: Did they enter values
		if (gsValue == "")
		{
			Ch->Write("#100[#101Invalid Input#100]#700 You must supply a direction.\n\r");
			Ch->Write("Syntax#200:#701 Unlink#200<#201Direction#200>#700\n\r");
			return true;
		}

		// [2] :: Did they enter a valid Direction
		// Runs from 1 to ignore None

		bool bFound = false;
		for (int i = 1; i < CExit::NUMEXITS; i++)
		{
			gString gsExit = CExit::szExitNames[i];
			if (gsExit == gsValue)
				bFound = true;			
		}

		if (!bFound)
		{
			Ch->Write("#100[#101Invalid Direction#100]#701 %s#700 is not a valid Direction, valid directions are#200:\n\r", gsValue);
			for (int j = 1; j < CExit::NUMEXITS; j++)
			{
				Ch->Write("[#701 %d#200]#700 %s\n\r", j, CExit::szExitNames[j]);
					
			}
		}

		// [3] Does this room have an exit in that direction
		bFound = false;
		int nRoomToVnum = 0;
		for (pos=Ch->CurrentRoom()->Exits().begin(); pos != Ch->CurrentRoom()->Exits().end(); pos++)
		{
			CExit* pExit = (CExit*)(*pos);

			gString gsExit = CExit::szExitNames[ pExit->m_Direction ];

			if ( gsExit.HasPrefix(gsValue) )
			{
				// We can assume that this room we have linked to is part of this ships area
				// as we restrict it to this in the link command
				nRoomToVnum = pExit->m_Destination.Room();
				bFound = true;
			}
		}

		if (!bFound)
		{
			Ch->Write("#100[#101Invalid Direction#100]#701 %s#700 is not a valid Exit direction from this room.\n\r", gsValue);
			return true;
		}


		// Its safe to get the Area now... I guess...
		CArea* pArea = pTemplate->m_Area;

		// Sanity check
		if (!pArea)
		{
			g_Log.Log(LOG_ERROR, "Invalid Area for Design Template: <%s>", pTemplate->m_gsName);
			return false;
		}

		// Get the room we are in and the room we are removing the exit from
		CRoom* pFrom = Ch->CurrentRoom();
		CRoom* pTo = pArea->GetRoom(nRoomToVnum);

		// Extra Sanity check
		if (!pFrom || !pTo)
		{
			g_Log.Log(LOG_ERROR, "Invalid Room for Unlinking Template: <%s>", pTemplate->m_gsName);
			return false;
		}

		// [4] :: Make this quite advanced, we are going to assume that there are multiple links
		//        from this room to our target room so we will remove them all
		for (pos = pFrom->Exits().begin(); pos != pFrom->Exits().end(); pos++)
		{
			CExit* pExit = (CExit*)(*pos);

			// This links to our target room so add it to our delete list
			if (pExit->Destination() == pTo->Position())
				DeleteList.push_back(pos);				
		}

		// Remove all the exits from this room to our target room
		for (std::vector<ExitList::iterator>::iterator exit = DeleteList.begin(); exit != DeleteList.end(); exit++)
			pFrom->Exits().erase(*exit);

		DeleteList.clear();

		// [5] :: Now we have to do the same for our target room
		for (pos = pTo->Exits().begin(); pos != pTo->Exits().end(); pos++)
		{
			CExit* pExit = (CExit*)(*pos);

			// This links to our target room so add it to our delete list
			if (pExit->Destination() == pFrom->Position())
				DeleteList.push_back(pos);				
		}

		// Remove all the exits from this room to our target room
		for (std::vector<ExitList::iterator>::iterator exit = DeleteList.begin(); exit != DeleteList.end(); exit++)
			pTo->Exits().erase(*exit);

		Ch->Write("Rooms #201unlinked#700.\n\r");
		return true;

	}

	Ch->Write("#100[#101Invalid Command#100]#701 Unlink can only be used during Area Design.#700\n\r");
	Ch->Write("Syntax#200:#701 Unlink <#201Direction#200>\n\r");
	return true;
}

// Method     :: CmdDGoto
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Allows a player to be placed within the template's area
// Written    :: 05/01/2005 {OWV}

bool CmdDGoto::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Vnum from
	// Get the game World so we can check if the Area is valid
	CPlayer* pPlayer = (CPlayer*) Ch;

	// Check they have assigned a template
	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101Invalid Template#100]#700 You must start work on a Design first.\n\r");
		return true;		
	}

	if (!pPlayer->m_Template->m_Area)
	{
		Ch->Write("#100[#101Invalid Area#100]#700 This template has not yet had its area generated.\n\r");
		return true;
	}

	// Try and get the Area
	CArea* pArea = pPlayer->m_Template->m_Area;

	// If they didn't supply a value then they just want to be placed in the
	// Area
	if (gsValue == "")
	{
		CRoom* pRoom = (*pArea->Rooms()->begin()).second;

		// Not a valid room
		if (!pRoom)
		{
			Ch->Write("#100[#101Invalid Room#100]#700 The template area has no rooms, contact an Administrator.\n\r");
			return true;
		}
		
		// Move the player
		Ch->SetCurrentRoom(pRoom);

		// Make them look
		pPlayer->ExecuteCommand("look", "");

		Ch->Write("Entering #201Template Area.#700\n\r");
	}
	else
	{
		// They want to be placed in a specific room
		CRoom* pRoom = pArea->GetRoom(atoi(gsValue));

		if (!pRoom)
		{
			Ch->Write("#100[#101Invalid Room#100]#701 %d#700 is not a Valid Vnum for this area.\n\r", atoi(gsValue));
			return true;
		}

		// Move the player
		Ch->SetCurrentRoom(pRoom);

		// Make them look
		pPlayer->ExecuteCommand("look", "");

	}



	return true;
}

// Method     :: CmdDReview
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <area|template>
// Return     :: Bool
// Function   :: Allows a player to go back and edit part of the template
// Written    :: 05/01/2005 {OWV}

bool CmdDReview::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Area to review
	CPlayer* pPlayer = (CPlayer*)Ch;

	// Check they have assigned a template
	if (!pPlayer->m_Template && gsValue == "Template")
	{
		Ch->Write("#100[#101Invalid Template#100]#700 You must start work on a Design first.\n\r");
		return true;		
	}

	if (!pPlayer->m_Template->m_Area && gsValue == "Area")
	{
		Ch->Write("#100[#101Invalid Area#100]#700 This template has not yet had its area generated.\n\r");
		Ch->Write("Return to the Template Design and type#200:#701 finish #700to generate it.\n\r");
		return true;
	}
	
	if (gsValue != "Template" && gsValue != "Area")
	{
		Ch->Write("#100[#101Invalid Input#100]#701 %s#700 is not a valid area to review.\n\r", gsValue);
		Ch->Write("Choices#200:#701 Template, Area#700\n\r");
		return true;
	}

	if (gsValue == "Template")
	{
		pPlayer->m_Template->m_nStatus = CTemplate::_TEMPLATE;
		Ch->Write("Reviewing #201Design Template.#700\n\r");
		return true;
	}
	else if (gsValue == "Area")
	{
		pPlayer->m_Template->m_nStatus = CTemplate::_AREA;
		Ch->Write("Entering #201Design area.#700\n\r");
		pPlayer->ExecuteCommand("goto", "");
		return true;

	}
	return true;
}

// Method     :: CmdDClone
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Comp Index #> <# of copies>
// Return     :: Bool
// Function   :: Allows a player to create a copy of a Component and all
//            :: the modules it contains.
// Written    :: 06/02/2006 {OWV}

bool CmdDClone::Perform(CActor* Ch, gStringList& CommandLine)
{
	// Process arguments
	gString gsValue = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Component to Clone
	if (!CommandLine.empty())
		CommandLine.pop_front();
	gString gsValue1 = (CommandLine.empty()) ? "" : *CommandLine.begin();		// Number to Clone

	// We need to cast the Actor to a Player so we can access the Design specific values
	CPlayer* pPlayer = (CPlayer*)(Ch);

	if (!pPlayer->m_Template)
	{
		Ch->Write("#100[#101No Template#100]#700 You must start a Template first. \n\r");
		return true;
	}

	if (!pPlayer->m_Template->m_Ship)
	{
		Ch->Write("#100[#101Error#100]#702 %s#700 has no ship structure\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	if (atoi(gsValue1) <= 0)
	{
		Ch->Write("#100[#101Error#100]#700 The number of clones to create must exceed 0.\n\r", pPlayer->m_Template->m_gsName);
		return true;
	}

	CTemplate* pTemplate = pPlayer->m_Template;

	// We can only clone Components so they must be zoomed into a HullCube first
	if (pPlayer->m_HullCube != NULL)
	{
		if (pPlayer->m_Component != NULL)
		{
			Ch->Write("#100[#101Error#100]#700 You can only clone Components. Exit back to the HullCube level.\n\r");
			return true;
		}
		else
		{
			// They are only zoomed into the HullCube here so we check its a valid 
			// component index.
			if (atoi(gsValue) > pPlayer->m_HullCube->m_Components.size() || atoi(gsValue)-1 < 0)
			{
				Ch->Write("#100[#101Invalid Input#100] <#701 %d#200>#700 is not a Valid Component Index. Choices #200<#201 1 - %d#200>#700\n\r", atoi(gsValue), pPlayer->m_HullCube->m_Components);
				return true;
			}
			else
			{
				// We know this Component is valid so we need to get a reference to it
				CComponent* pComp = pPlayer->m_HullCube->m_Components.at(atoi(gsValue) -1);

				// We have already established that the number of clones is a valid number
				for (int i = 1; i <= atoi(gsValue1); i++)
				{
					CComponent* pNew; // Create a new instance of the component in memory
					
					// Copy the data across
					switch(pComp->m_Type)
					{
						case CComponent::CT_SHIELD:
							pNew = new CShield();
							*(CShield*)pNew = *(CShield*)pComp;
							break;
						case CComponent::CT_ENGINEERING:
							pNew = new CEngspace();
							*pNew = *pComp;
							break;
						case CComponent::CT_SUBLIGHT:
							pNew = new CSublight();
							*pNew = *pComp;
							break;
						case CComponent::CT_HYPERDRIVE:
							pNew = new CHyperdrive();
							*pNew = *pComp;
							break;
						case CComponent::CT_MAGAZINE:
							pNew = new CMagazine();
							*pNew = *pComp;
							break;
						case CComponent::CT_EXTERNAL:
							pNew = new CExternal();
							*pNew = *pComp;
							break;
						case CComponent::CT_INTERNAL:
							pNew = new CInternal();
							*pNew = *pComp;
							break;
						case CComponent::CT_LANDING:
							pNew = new CLanding();
							*pNew = *pComp;
							break;
						case CComponent::CT_BULKSTORAGE:
							pNew = new CBulk();
							*pNew = *pComp;
							break;
						case CComponent::CT_CONTROLPOINT:
							pNew = new CControl();
							*pNew = *pComp;
							break;
						case CComponent::CT_WEAPONMOUNT:
							pNew = new CWeapon();
							*(CWeapon*)pNew = *(CWeapon*)pComp;
							break;
						case CComponent::CT_ESCAPEPOD:
							pNew = new CEscape();
							*pNew = *pComp;
							break;
						case CComponent::CT_SFOIL:
							pNew = new CSFoil();
							*pNew = *pComp;
							break;

						default:
							*pNew = *pComp;
							break;
					}

					// We need to give the Component a Unique name
					pNew->m_gsName.Format("%s %d", pNew->m_gsName, i+1);
					pPlayer->m_HullCube->m_Components.push_back(pNew);
				}

				Ch->Write("#201Clones created.#700\n\r");

			}

		}
	}
	else
	{
		Ch->Write("#100[#101Error#100]#700 You can only clone Components. Enter a HullCube first.\n\r");
		return true;
	}


	return true;
}