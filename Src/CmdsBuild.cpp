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

// File     :: CmdsBuild.cpp
// Header   :: CmdsBuild.h
// Function :: Holds the implementations for AMC's OLC engine

#include "MudCore.h"
#include "CmdsBuild.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "GameWorld.h"
#include "Tools.h"

// Macro to define implementations
IMPLEMENT_CLASS(CmdRedit);			// Used for room editing/creation
IMPLEMENT_CLASS(CmdRCreate);		// Used to create a room
IMPLEMENT_CLASS(CmdRShow);			// Used to show a room to the player
IMPLEMENT_CLASS(CmdRSize);			// Used to set the size of a room
IMPLEMENT_CLASS(CmdRBartle);		// Used to set the bartle field
IMPLEMENT_CLASS(CmdRName);			// Used to set the name of the room
IMPLEMENT_CLASS(CmdRDesc);			// Used to set the DESC components
IMPLEMENT_CLASS(CmdRFlags);			// Used to set room flags
IMPLEMENT_CLASS(CmdRTerrain);		// Used to set the terrain type of the room
IMPLEMENT_CLASS(CmdRExit);			// Used to set the exit properties
IMPLEMENT_CLASS(CmdRExflags);		// Used to set the exit flags
IMPLEMENT_CLASS(CmdRVexit);			// Used to set vehicle exits
IMPLEMENT_CLASS(CmdRInstaroom);		// Used to install resets for all objects/mobiles in room
IMPLEMENT_CLASS(CmdRTurbolift);		// Used to manipulate turbolift properties
IMPLEMENT_CLASS(CmdRCover);			// Used to set cover values
IMPLEMENT_CLASS(CmdRDig);			// Used to create exits and rooms
IMPLEMENT_CLASS(CmdPedit);			// Used for parent room editing/creation

//
// NOTICE: OLC PROGRAMMING IS ON HOLD WAITING THE RELEASE OF mcEDITOR BY GARY MCNICKLE
//

// Method     :: CmdRedit
// Class	  :: CmdRedit
// Parameters :: <actor, arguments>
// Arguments  :: <on>
//			  :: <create> {vnum}
//			  :: <name> {new name}
//			  :: <size> {new size}
//			  :: <bartle> {new bartle field}
//			  :: <desc>
//			  :: <flags>
//			  :: <terrain>
//			  :: <exit>
//			  :: <exflags>
//			  :: <vexit>
//			  :: <instaroom>
//			  :: <turbolift>
//			  :: <cover>
//			  :: <dig>
// Return     :: Bool
// Function   :: Handles all commands required for editting a room
// Written    :: 11/08/2005 {OWV}

bool CmdRedit::Perform(CActor* Ch, gStringList &CommandLine)
{
	gString gsPrefix;

	// First check if they entered anything, if not we just display a summary of their current room
	if (CommandLine.size() <= 0)
	{
		// Display the room details to them
		CmdRShow* pShow = new CmdRShow;
		pShow->Perform(Ch, CommandLine);
		delete pShow;
		return true;
	}
	else
	{
		gsPrefix = (CommandLine.front());
	}

	// Determine the command to perform based upon gsPrefix
	if (gsPrefix == "Create")
	{

	}
		
	
	return false;
}

// Method     :: CmdRCreate
// Class	  :: CmdRCreate
// Parameters :: <actor, arguments>
// Arguments  :: <create> {vnum}
// Return     :: Bool
// Function   :: Creates a new room from a free vnum
// Written    :: 11/08/2005 {OWV}

bool CmdRCreate::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRShow
// Class	  :: CmdRShow
// Parameters :: <actor, arguments>
// Arguments  :: <show>
// Return     :: Bool
// Function   :: Shows all details about the current room
// Written    :: 11/08/2005 {OWV}

bool CmdRShow::Perform(CActor* Ch, gStringList &CommandLine)
{
	Ch->Write("Show text.\n\r");
	return false;
}

// Method     :: CmdRName
// Class	  :: CmdRName
// Parameters :: <actor, arguments>
// Arguments  :: <name> {new name}
// Return     :: Bool
// Function   :: Sets the name of a room
// Written    :: 11/08/2005 {OWV}

bool CmdRName::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRDesc
// Class	  :: CmdRDesc
// Parameters :: <actor, arguments>
// Arguments  :: <Desc>
//			  :: <Desc> <Add> <type> <text>
//			  :: <Desc> <Rem> <num>
// Return     :: Bool
// Function   :: Modifies the DESC component of the room
// Written    :: 11/08/2005 {OWV}

bool CmdRDesc::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRSize
// Class	  :: CmdRSize
// Parameters :: <actor, arguments>
// Arguments  :: <Size> {new size}
// Return     :: Bool
// Function   :: Modifies the size of the room
// Written    :: 11/08/2005 {OWV}

bool CmdRSize::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRBartle
// Class	  :: CmdRBartle
// Parameters :: <actor, arguments>
// Arguments  :: <Bartle> {new type}
// Return     :: Bool
// Function   :: Modifies the bartle field
// Written    :: 11/08/2005 {OWV}

bool CmdRBartle::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRFlags
// Class	  :: CmdRFlags
// Parameters :: <actor, arguments>
// Arguments  :: <Flags> {flag to toggle}
// Return     :: Bool
// Function   :: Modifies the room's flags
// Written    :: 11/08/2005 {OWV}

bool CmdRFlags::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRTerrain
// Class	  :: CmdRTerrain
// Parameters :: <actor, arguments>
// Arguments  :: <Flags> {flag to toggle}
// Return     :: Bool
// Function   :: Modifies the room's terrain types
// Written    :: 11/08/2005 {OWV}

bool CmdRTerrain::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRExit
// Class	  :: CmdRExit
// Parameters :: <actor, arguments>
// Arguments  :: <Exit> {Direction}
//			  :: <Exit> {Direction} {Placement}
//			  :: <Exit> {Direction} {Strength/Complexity} {Value}
// Return     :: Bool
// Function   :: Modifies the room's exits and exit properties
// Written    :: 11/08/2005 {OWV}

bool CmdRExit::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRExflags
// Class	  :: CmdRExflags
// Parameters :: <actor, arguments>
// Arguments  :: <Exflags> <Direction> {flag to toggle}
// Return     :: Bool
// Function   :: Modifies the room's exit flags
// Written    :: 11/08/2005 {OWV}

bool CmdRExflags::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRVexit
// Class	  :: CmdRVexit
// Parameters :: <actor, arguments>
// Arguments  :: <Vexit> {Direction}
//			  :: <Vexit> {Direction} {Placement}
// Return     :: Bool
// Function   :: Creates and removes vehicle exits
// Written    :: 11/08/2005 {OWV}

bool CmdRVexit::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRInstaroom
// Class	  :: CmdRInstaroom
// Parameters :: <actor, arguments>
// Arguments  :: <Instraoom>
// Return     :: Bool
// Function   :: Used to install resets for all objects/mobiles in room
// Written    :: 11/08/2005 {OWV}

bool CmdRInstaroom::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRTurbolift
// Class	  :: CmdRTurbolift
// Parameters :: <actor, arguments>
// Arguments  :: <Turbolift>
//			  :: <Turbolift> Add {Floor number}	
//			  :: <Turbolift> Rem {Floor number}	
//			  :: <Turbolift> Callbutton {Placement}
//			  :: <Turbolift> Move {Message}
//			  :: <Turbolift> Stop {Message}
// Return     :: Bool
// Function   :: Used to install resets for all objects/mobiles in room
// Written    :: 11/08/2005 {OWV}

bool CmdRTurbolift::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRCover
// Class	  :: CmdRCover
// Parameters :: <actor, arguments>
// Arguments  :: <Cover> {new cover value}
// Return     :: Bool
// Function   :: Used to set the cover of the room
// Written    :: 11/08/2005 {OWV}

bool CmdRCover::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdRDig
// Class	  :: CmdRDig
// Parameters :: <actor, arguments>
// Arguments  :: <Dig> {Direction} {Vnum}
// Return     :: Bool
// Function   :: Used to create and link a new room
// Written    :: 11/08/2005 {OWV}

bool CmdRDig::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}

// Method     :: CmdPedit
// Class	  :: CmdPedit
// Parameters :: <actor, arguments>
// Arguments  :: <Pedit> {on}
// Return     :: Bool
// Function   :: Used to modify the parent room's values
// Written    :: 11/08/2005 {OWV}

bool CmdPedit::Perform(CActor* Ch, gStringList &CommandLine)
{
	return false;
}
