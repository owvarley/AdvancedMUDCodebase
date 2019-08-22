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

// Class    :: CPowerplant
// Header   :: Spatial.h
// Function :: Implements the Powerplant Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 3. Powerplant Class & PowerState
///////////////////////////////////////////////////////////////////////////////////////////

CPowerState::CPowerState()
{
	m_nSublight = 25.0;
	m_nManeuver = 25.0;
	m_nWeapons = 25.0;
	m_nShields = 25.0;
	m_nFree = 0.0;

}

CPowerState::~CPowerState()
{
	m_nSublight = 0.0;
	m_nManeuver = 0.0;
	m_nWeapons = 0.0;
	m_nShields = 0.0;
	m_nFree = 0.0;
}

CEngspace::CEngspace()
{
	m_Type = (CComponent::CT_ENGINEERING);
	m_Install.insert(InstallMap::value_type(CModule::MT_REACTOR_PLANT, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COOLANT_PLANT, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_FUEL_LINE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_BATTERY_BANK, true));
	
}


CEngspace::~CEngspace()
{
	m_Type = 0;
}

void CEngspace::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pReactor = Get(CModule::MT_REACTOR_PLANT);	// Reactor plant
	CModule* pCoolant = Get(CModule::MT_COOLANT_PLANT);	// Coolant plant
	CModule* pBattery = Get(CModule::MT_BATTERY_BANK);	// Battery bank
	CModule* pFuel = Get(CModule::MT_FUEL_LINE);		// Fuel line
	CModule* pPower = Get(CModule::MT_POWER_LINK);		// Power link
	
	
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
					
					switch (pMod->m_nType)
					{
					case CModule::MT_REACTOR_PLANT:
						(*pMod->m_Plus.find("Timer")).second = CComponent::T_REACTOR;
						break;

					case CModule::MT_COOLANT_PLANT:
						(*pMod->m_Plus.find("Timer")).second = CComponent::T_COOLANT;
						break;

					default:
						(*pMod->m_Plus.find("Timer")).second = CComponent::T_REACTOR;
						break;
					}
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers
	// [1] - Reactor Plant
	// [2] - Coolant Plant
	// [3] - Battery Plant
	// [4] - Fuel Line
	// [5] - Power link

	// [1] - Reactor Plant
	// When online uses fuel to charge the battery. Will charge the battery regardless of whether
	// the battery is online or not. Requires coolant in order to maintain a safe core temperature
	// [Value 1] - Coolant Load
	// [Value 2] - Recharge Rate
	// [Fuel] - Fuel Usage
	if (pReactor && pReactor->Powered())
	{
		// Is the Reactor's Integrity now zero?
		float fInt = ((float)pReactor->m_ncDurability / (float)pReactor->m_nmDurability) * 100.0;

		if (fInt <= 50.0)
		{
			if (pShip->m_Scm->IsSet(CShip::SCM_REACTOR))
			{
				// Automatic shutdown due to damage
				pShip->Write(CShip::MT_CRITICAL, "[SCM Override] Integrity Critical, %s shutdown to prevent meltdown.\n\r", pReactor->m_gsName);
				pReactor->SetPlus("Powered", 0);
			}
		}
		else
		{
			// If Fuel line is still operational
			if (pFuel && pFuel->m_ncDurability > 0)
			{
				// Try and Consume some fuel
				if (pShip->ConsumeFuel(pReactor->Plus("Fuel", false)))
				{				
					// Consume some Coolant
					if (pShip->m_nCoolant > 0)
						pShip->m_nCoolant -= pReactor->Plus("CoolantLoad", false);
					else
					{
						if (pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT))
						{
							pShip->Write(CShip::MT_CRITICAL, "[SCM Override] Overheating detected. %s shutdown to prevent reactor damage.\n\r", pReactor->m_gsName);
							pReactor->SetPlus("Powered", 0);	// Turn it off
						}
						else
						{
							// There is no Coolant but our Reactor is running, so damage it
							CRandom* pRandom = CGameObjects::Get().Rand();
							pReactor->Damage(pRandom->NumberRange(1, 100)); // #TODO# Determine realisitic damage

							pShip->Write(CShip::MT_SYSTEMS, "[%s] %s is overheating.\n\r", this->m_gsName, pReactor->m_gsName);
						}

					}
				}
				
				
			}
		}

	} // End Reactor

	// [2] - Coolant Plant
	// When online uses power from the Ship's battery bank in order to create coolant.
	// [Value 1] - Coolant Rate 
	if (pCoolant && pCoolant->Powered())
	{
		float fInt = ((float)pCoolant->m_ncDurability / (float)pCoolant->m_nmDurability) * 100.0;

		if (fInt <= 50.0)
		{
			if (pShip->m_Scm->IsSet(CShip::SCM_COOLANT))
			{
				// Automatic shutdown due to damage
				pShip->Write(CShip::MT_SYSTEMS, "[SCM Override] Integrity Critical, %s shutdown due to prevent overheat.\n\r", pCoolant->m_gsName);
				pCoolant->SetPlus("Powered", 0);
			}
		}
		else
		{
			// Need to Generate some Coolant, todo this we need power
			if (pShip->m_nCoolant <= 50000)
				pShip->m_nCoolant += pCoolant->Plus("CoolantRate", true);
		}

		// #TODO# Heat signature
		// #TODO# Max Coolant for Ship
	}

	// [3] - Battery Bank
	// Stores energy for the Ship
	// [Value 1] - Current Charge
	// [Value 2] - Maximum Charge
	if (pBattery && pBattery->m_ncDurability > 0)
	{
		if (pBattery->Plus("CurrCapacity", true) < pBattery->Plus("MaxCapacity", true))
		{
			// Charge the battery
			// #POD# Should this be global?
			if (pReactor)
			{
				int nCharge = (pBattery->Plus("CurrCapacity", false) + pReactor->Plus("PowerCapacity", true));
				pBattery->SetPlus("CurrCapacity", nCharge);

				// Did we over charge?
				if (pBattery->Plus("CurrCapacity", true) > pBattery->Plus("MaxCapacity", true))
					pBattery->SetPlus("CurrCapacity", pBattery->Plus("MaxCapacity", false));
				
				if (pBattery->Plus("CurrCapacity", true) == pBattery->Plus("MaxCapacity", true))
				{
					pShip->Write(CShip::MT_SYSTEMS, "[Engspace] %s full charged.\n\r", pBattery->m_gsName);
				}
				
			}
		}
		else if (pBattery->Plus("CurrCapacity", true) > pBattery->Plus("MaxCapacity", true))
		{
			pShip->Write(CShip::MT_SYSTEMS, "[Engspace] %s full charged.\n\r", pBattery->m_gsName);
			
			// Fully charge the battery
			pBattery->SetPlus("CurrCapacity", pBattery->Plus("Max Capacity", false));
		}

	}

	pShip = NULL;
	
}

void CEngspace::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CEngspace::ReadExtra(TiXmlNode* pParent)
{
	return;
}
