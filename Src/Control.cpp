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

// Class    :: CControl
// Header   :: Spatial.h
// Function :: Implements the Control Point Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Control Class 
///////////////////////////////////////////////////////////////////////////////////////////

CControl::CControl()
{
	m_Type = (CComponent::CT_CONTROLPOINT);
	m_Install.insert(InstallMap::value_type(CModule::MT_CONTROL_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_HELM, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_SECONDARY_CONTROL_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_PILOT_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COPILOT_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_GUNNERY_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_WEAPONS_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_NAV_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_ENGINEERING_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COMMANDER_CONSOLE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));

}


CControl::~CControl()
{
	m_Type = 0;
}

void CControl::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

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
			// If this Module is Powered up and:
			// [1] We have no Reactor and No Battery Charge
			// Then we must shut it down
			if (pMod->Powered() && pShip->Battery(true) <= 0 && pShip->MaxPower() <= 0)
			{
				pMod->SetPlus("Powered", 0);
				pShip->Write(CShip::MT_SYSTEMS, "#100[#101%s#100]#700 %s #201online#700.\n\r", this->m_gsName, pMod->m_gsName);
			}
			
			// Is it trying to power up?
			if ((*pMod->m_Plus.find("Power")).second)
			{

				// Power up the Module
				if ((*pMod->m_Plus.find("Timer")).second == 0)
				{
					(*pMod->m_Plus.find("Powered")).second = true;
					(*pMod->m_Plus.find("Power")).second = false;

					pShip->Write(CShip::MT_SYSTEMS, "#200[#201%s#200]#700 %s #201online#700.\n\r", this->m_gsName, pMod->m_gsName);
					
					// The power up rate is the same for all modules
					(*pMod->m_Plus.find("Timer")).second = CComponent::T_CONTROL;
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers

	pShip = NULL;

}

void CControl::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CControl::ReadExtra(TiXmlNode* pParent)
{
	
}
