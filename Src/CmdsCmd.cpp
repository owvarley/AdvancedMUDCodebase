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

// File     :: CmdsCmd.cpp
// Header   :: CmdsCmd.h
// Function :: Holds the implementations for commands that belong in the Command category

#include "MudCore.h"
#include "CmdsCmd.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "GameWorld.h"
#include "Tools.h"

// Macro to define implementations
IMPLEMENT_CLASS(CmdCommands);	// Displays all commands in the game

// Method     :: CmdCommands
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <none>
// Return     :: Bool
// Function   :: Iterates through each Command Parser and displays the commands contained
//			  :: within it.
// Written    :: 14/12/2005 {OWV}

bool CmdCommands::Perform(CActor* Ch, gStringList& CommandLine)
{
	if ( Ch )
	{
		CmdParsers::iterator pos;

		if ( !Ch->HomeWorld() )
			return false;

		Ch->Write("#600:#601:#702 Commands#601 :#600:#700 \n\r\n\r");
		for ( pos = Ch->HomeWorld()->Commands().begin(); pos != Ch->HomeWorld()->Commands().end(); pos++ )
		{
			(*pos)->DescribeTo(Ch);
		}
	}

	return true;
}