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

// Class    :: CImpulse
// Header   :: Spatial.h
// Function :: Implements the Impulse Drive Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include <direct.h>
#include "Spatial.h"


///////////////////////////////////////////////////////////////////////////////////////////
// 2. Sublight Class
///////////////////////////////////////////////////////////////////////////////////////////

CSublight::CSublight()
{
	// Type
	m_Type = (CComponent::CT_SUBLIGHT);
	m_Install.insert(InstallMap::value_type(CModule::MT_ION_ENGINE, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_DRIVE_BAFFLES, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COOLANT_LINE, true));
}

CSublight::~CSublight()
{
	// Fields
	m_Type = 0;
}


void CSublight::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules	
	CModule* pIon = Get(CModule::MT_ION_ENGINE);						// Ion Engine
	CModule* pCoolant = Get(CModule::MT_COOLANT_LINE);					// Coolant line
	CModule* pPower = Get(CModule::MT_POWER_LINK);						// Power link
	CModule* pBaffles = Get(CModule::MT_DRIVE_BAFFLES);					// Driving Baffles
	
	
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
					
					(*pMod->m_Plus.find("Timer")).second = CComponent::T_SUBLIGHT;
	
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers
	// [1] - Ion Engine
	// [2] - Coolant Line
	// [3] - Power link
	// [4] - Driving Baffles

	// [1] - Ion Engine
	// [Speed       ] - Maximum attainable speed
	// [Energy Load ] - Energy usage
	// [Coolant Load] - Coolant usage
	if (pIon && pIon->Powered())
	{
		// Is the Power Link still useable?
		if (pPower && pPower->m_ncDurability <= 0 || !pPower)
		{
			pShip->Write(CShip::MT_SYSTEMS, "[%s] %s knocked offline.\n\r", this->m_gsName, pIon->m_gsName);
			pIon->SetPlus("Powered", 0);

		}
		else
		{
			float fInt = ((float)pIon->m_ncDurability / (float)pIon->m_nmDurability) * 100.0;

			if (fInt <= 50.0)
			{
				if (pShip->m_Scm->IsSet(CShip::SCM_ION))
				{
					// Automatic shutdown due to damage
					pShip->Write(CShip::MT_CRITICAL, "[SCM Override] Integrity Critical, %s shutdown to prevent Ion Drive collapse.\n\r", pIon->m_gsName);
					pIon->SetPlus("Powered", 0);
				}
			}
			else
				{
				if (pShip->m_nCoolant > 0 && (pCoolant && pCoolant->m_ncDurability > 0))
					pShip->m_nCoolant -= pIon->Plus("CoolantLoad", false);
				else
				{
					if (pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT))
					{
						pShip->Write(CShip::MT_SYSTEMS, "[SCM Override] Overheating detected. %s shutdown to prevent drive damage.\n\r", pIon->m_gsName);
						pIon->SetPlus("Powered", 0);	// Turn it off
					}
					else
					{
						// No coolant so we need to reduce the Integrity of the Drive
						CRandom* pRandom = CGameObjects::Get().Rand();
						pIon->Damage(pRandom->NumberRange(1, 100)); // #TODO# Determine realisitic damage

						// Inform player of this
						pShip->Write(CShip::MT_SYSTEMS, "[%s] %s is overheating.\n\r", this->m_gsName, pIon->m_gsName);
					}
				}
			}
		}
	}

	pShip = NULL;

}

void CSublight::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CSublight::ReadExtra(TiXmlNode* pParent)
{
	return;
}

