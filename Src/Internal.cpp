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

// Class    :: CInternal
// Header   :: Spatial.h
// Function :: Implements the Internal Systems Mount Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Internal Class 
///////////////////////////////////////////////////////////////////////////////////////////

CInternal::CInternal()
{
	m_Type = (CComponent::CT_INTERNAL);
	m_Install.insert(InstallMap::value_type(CModule::MT_ASTROGATION_COMPUTER, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_REPULSOR_COILS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_LIFE_SUPPORT_UNIT, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_HOLONET_TRANSCEIVER, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));

}


CInternal::~CInternal()
{
	m_Type = 0;
}

void CInternal::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pAstrogation = Get(CModule::MT_COLLECTOR_ARRAY);		// Astrogation Computer
	CModule* pRepulsor = Get(CModule::MT_RADOME);					// Repulsor Coils
	CModule* pLife = Get(CModule::MT_MANUEVERING_THRUSTERS);		// Life Support
	CModule* pHolo = Get(CModule::MT_HEATSINK);						// Holonet 
	CModule* pPower = Get(CModule::MT_POWER_LINK);					// Power Link

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

					pShip->Write(CShip::MT_SYSTEMS, "#200[#201%s#200]#700 %s #201online#700.\n\r", this->m_gsName, pMod->m_gsName);


					// Engage Repulsor Coils
					if (pMod->m_nType == CModule::MT_REPULSOR_COILS)
					{
						if (pShip->m_ShipState->IsSet(CShip::_LANDED))
						{
							pShip->m_ShipState->RemoveBit(CShip::_LANDED);
							pShip->m_ShipState->SetBit(CShip::_REPULSOR);
							pShip->Write(CShip::MT_CRITICAL, "The ship slowly rises from the ground using its Repulsor Coils.\n\r");
							// Let the room know
							CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(pShip->m_Land.Area());

							if (pArea)
							{
								CRoom * pRoom = pArea->GetRoom(pShip->m_Land.Room());
								if (pRoom)
									pRoom->Write("%s:%s slowly rises from the ground using its Repulsor Coils.\n\r", pShip->m_gsType, pShip->m_gsName);
							}
						}
						else
						{
							// Messages will be specific to the application in space
							// and hence will not be handled here
							pShip->m_ShipState->SetBit(CShip::_REPULSOR);
						}
						
					}

					// Set the powerup rate
					if (pMod->m_nType == CModule::MT_GRAVITY_WELL_PROJECTORS)
						(*pMod->m_Plus.find("Timer")).second = CComponent::T_GRAVWELLS;
					else if (pMod->m_nType == CModule::MT_ASTROGATION_COMPUTER)
						(*pMod->m_Plus.find("Timer")).second = CComponent::T_ASTROGRATIONCOMP;
					else
						(*pMod->m_Plus.find("Timer")).second = CComponent::T_INTERNAL;
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers 

	// #TODO# Timers for calculating Hyperspace jumps
	
	// [1] - Astrogation Computer
	// [2] - Repulsor Coils
	// [3] - Life Support
	// [4] - Holonet
	// [5] - Power Link

	// [3] - Life Support
	// If the Life Support isn't powered up any Players within that area of the Ship will start
	// to die
	if (pLife && !pLife->Powered()) // && Launched()
	{
		// #TODO# Add config to allow alarms to be disabled
		pShip->Write(CShip::MT_CRITICAL, "#100[#101Alarm#100]#700 %s disabled.\n\r", pLife->m_gsName);
	}
}

void CInternal::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CInternal::ReadExtra(TiXmlNode* pParent)
{
	return;
}