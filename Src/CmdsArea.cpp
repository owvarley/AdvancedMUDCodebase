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

// File     :: CmdsArea.cpp
// Header   :: CmdsArea.h
// Function :: Holds the implementations for commands that belong in the Area category

#include "MudCore.h"
#include "CmdsArea.h"
#include "CmdsCmd.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "GameWorld.h"
#include "Tools.h"
#include "../gTools/Log.h"

// Macro to define implementations, also a good list of what this source file contains!
IMPLEMENT_CLASS(CmdAreas);	// Displays all areas in the game

// Method     :: CmdAreas
// Class	  :: <none>
// Parameters :: <actor, arguments>
// Arguments  :: <Maintenance>
// Return     :: Bool
// Function   :: Displays all areas in the Arealist, Maintenance argument performs maintenance
//			  :: on all areas, removing those linked to ships that no longer exist.
// Written    :: 14/02/2006 {OWV}

bool CmdAreas::Perform(CActor* Ch, gStringList& CommandLine)
{
	gString gsValue = CGameObjects::Get().Tools()->ParseStringList(CommandLine, 0, false, true);

	LOG_SCOPE("CmdAreas::Perform");

	if (gsValue == "Maintenance")
	{
		Ch->Write("Performing Area Maintenance.\n\r");
		
		int nCount = 0;
		for (AreaList::iterator area = CGameObjects::Get().GameWorld()->Areas().begin(); area != CGameObjects::Get().GameWorld()->Areas().end(); area++)
		{
			CArea* pArea = (*area);

			if (pArea->Ship() != "" && !pArea->Flags()->IsSet(CArea::_TEMPLATE))
			{
				CShip* pShip = CGameObjects::Get().GameWorld()->Galaxy()->GetShi(pArea->Ship());
				
				// It doesnt exist so remove it
				if (!pShip)
				{
					g_Log.Log(LOG_INFO, "Maintenance: %s deleted as %s missing.\n", pArea->Name(), pArea->Ship());
					Ch->Write(" #600>#601>#700 %s deleted.\n\r", pArea->Name());
					pArea->Delete();
					nCount++;
				}				
			}
		}

		Ch->Write("#600[#601 %d#600]#700 Area%s deleted.\n\r", nCount, nCount > 1 ? "s" : "");
	}
	else
	{
		Ch->Write("#600:#601:#701 Areas #601:#600:#700\n\r");
		
		int nCount = 0;
		for (AreaList::iterator area = CGameObjects::Get().GameWorld()->Areas().begin(); area != CGameObjects::Get().GameWorld()->Areas().end(); area++)
		{
			nCount++;
			Ch->Write("#600[#601 %3d#600] - {#601 0 #600-#601 %4d#600} ::#700 %s #600by#700 %s\n\r", (*area)->Area(), (*area)->Rooms()->size() - 1, (*area)->Name(), (*area)->Author() == "" ? "World Craft Team" : (*area)->Author());
		}

		Ch->Write("#600[#601 %3d#600]#700 Area%s total.\n\r", nCount, nCount > 1 ? "s" : "");
	}
	
	return true;
}

