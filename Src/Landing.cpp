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

// Class    :: CLanding
// Header   :: Spatial.h
// Function :: Implements the Landing Bay Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Landing Class 
///////////////////////////////////////////////////////////////////////////////////////////

CLanding::CLanding()
{
	m_Type = (CComponent::CT_LANDING);
	m_Install.insert(InstallMap::value_type(CModule::MT_BLAST_DOORS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_TRACTOR_BEAM_PROJECTOR, true));
}


CLanding::~CLanding()
{
	m_Type = 0;
}

void CLanding::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pTractor = Get(CModule::MT_TRACTOR_BEAM_PROJECTOR);	// Tractor Beam
	CModule* pDoors = Get(CModule::MT_BLAST_DOORS);					// Bay Doors

	// Update any Timers:
	// First of all Update any Power timers
	for (ModuleMap::iterator map = this->m_Modules.begin(); map != this->m_Modules.end(); map++)
	{
		IntegerMap::iterator it;
		it = ((*map).second)->m_Plus.find("Power");
	
		if (it == ((*map).second)->m_Plus.end())
			continue;

		CModule* pMod = ((*map).second);

		if (pMod)
		{
			// Is it trying to power up?
			if ((*pMod->m_Plus.find("Power")).second)
			{

				// Power up the Module
				if ((*pMod->m_Plus.find("Timer")).second == 0)
				{
					(*pMod->m_Plus.find("Powered")).second = true;
					(*pMod->m_Plus.find("Power")).second = false;

					pShip->Write(CShip::MT_SYSTEMS, "[%s] %s online\n\r", this->m_gsName, pMod->m_gsName);
					
					// The power up rate is the same for all modules
					(*pMod->m_Plus.find("Timer")).second = CComponent::T_TRACTOR;
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers
	
	// [1] - Tractor Beam
	// [2] - Blast Doors

	// [1] - Tractor Beam
	// [Target] - Current tractored Target
	// [Strength] - Strength of Tractor beam
	// #TODO#

	// As in the Black Book.

	// [2] - Blast Doors
	// [Delay] - Time it takes to open the doors
	// [Open] - Are the doors open yet?
	// [Opening] - Are the doors trying to open
	if (pDoors && pDoors->Plus("Opening", false))
	{
		if (pDoors->Plus("Delay", false) == 0)
		{
			gString gsMsg;
			// Check their last known position
			if (pDoors->Plus("Open", false) == 0)
			{
				pDoors->SetPlus("Open", 1);
				pShip->Write(CShip::MT_SYSTEMS, "[%s] %s open.\n\r", this->m_gsName, pDoors->m_gsName);
			}
			else
			{
				pDoors->SetPlus("Open", 0);
				pShip->Write(CShip::MT_SYSTEMS, "[%s] %s closed.\n\r", this->m_gsName, pDoors->m_gsName);
			}

			pDoors->SetPlus("Opening", 0);
			pDoors->SetPlus("Delay", CComponent::T_BLASTDOOR);
				
		}
		else
		{
			pDoors->SetPlus("Delay", pDoors->Plus("Delay", false) - 1);
		}
	}

	pShip = NULL;

}

void CLanding::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	return;
}


void CLanding::ReadExtra(TiXmlNode* pParent)
{
	return;
}