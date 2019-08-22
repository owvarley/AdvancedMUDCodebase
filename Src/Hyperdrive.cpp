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

// Class    :: CHyperdrive
// Header   :: Spatial.h
// Function :: Implements the Hyperdrive Mount Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Hyperdrive Class 
///////////////////////////////////////////////////////////////////////////////////////////

CHyperdrive::CHyperdrive()
{
	m_Type = (CComponent::CT_HYPERDRIVE);
	m_Install.insert(InstallMap::value_type(CModule::MT_HYPERDRIVE_MAIN, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_HYPERDRIVE_BACKUP, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COOLANT_LINE, true));

}


CHyperdrive::~CHyperdrive()
{
	m_Type = 0;
}

void CHyperdrive::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pPrimary = Get(CModule::MT_HYPERDRIVE_MAIN);	// Primary Drive
	CModule* pBackup  = Get(CModule::MT_HYPERDRIVE_BACKUP);	// Backup Drive
	CModule* pCoolant = Get(CModule::MT_COOLANT_LINE);		// Coolant Line
	CModule* pPower = Get(CModule::MT_POWER_LINK);			// Power link
	
	
	// [1] - Hyperdrive Main
	// [2] - Hyperdrive Backup
	// [3] - Coolant Line
	// [4] - Power link

	// [1] - Hyperdrive Main
	// [ShipType   ] - Type of ship it moves
	// [PowerLoad ] - Energy load
	// [CoolantRate] - Rate of Coolant consumption
	// [HyperRating] - Class of Hyperdrive
	if (pPrimary && pPrimary->Powered())
	{
		// Is the Power Link still useable?
		if (pPower && pPower->m_ncDurability <= 0 || !pPower)
		{
			pShip->Write(CShip::MT_SYSTEMS, "[%s] %s knocked offline.\n\r", this->m_gsName, pPrimary->m_gsName);
			pPrimary->SetPlus("Powered", 0);

		}
		else
		{

			// Is there any coolant
			if (pShip->m_nCoolant > 0 && pCoolant)
				pShip->m_nCoolant -= pPrimary->Plus("CoolantLoad", false);
			else
			{
				if (pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT))
				{
					pShip->Write(CShip::MT_CRITICAL, "[SCM Override] Overheating detected. %s shutdown to prevent drive damage.\n\r", pPrimary->m_gsName);
					pPrimary->SetPlus("Powered", 0);	// Turn it off
				}
				else
				{
					// No coolant so we need to reduce the Integrity of the Drive
					CRandom* pRandom = CGameObjects::Get().Rand();
					pPrimary->Damage(pRandom->NumberRange(1, 100)); // #TODO# Determine realisitic damage

					// Inform player of this
					pShip->Write(CShip::MT_SYSTEMS, "[%s] %s is overheating.\n\r", this->m_gsName, pPrimary->m_gsName);
				}
			}

			// #TODO# Move the Ship through Hyperspace
		}
	}

	// [2] - Hyperdrive Backup
	// [ShipType   ] - Type of ship it moves
	// [PowerLoad ] - Energy load
	// [CoolantRate] - Rate of Coolant consumption
	// [HyperRating] - Class of Hyperdrive
	if (pBackup && pBackup->Powered())
	{
		// Is the Power Link still useable?
		if (pPower && pPower->m_ncDurability <= 0 || !pPower)
		{
			pShip->Write(CShip::MT_SYSTEMS, "[%s] %s knocked offline.\n\r", this->m_gsName, pBackup->m_gsName);
			pBackup->SetPlus("Powered", 0);

		}
		else
		{

			// Is there any coolant
			if (pShip->m_nCoolant > 0 && pCoolant)
				pShip->m_nCoolant -= pBackup->Plus("CoolantLoad", false);
			else
			{
				if (pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT))
				{
					pShip->Write(CShip::MT_CRITICAL, "[SCM Override] Overheating detected. %s shutdown to prevent drive damage.\n\r", pBackup->m_gsName);
					pBackup->SetPlus("Powered", 0);	// Turn it off
				}
				else
				{
					// No coolant so we need to reduce the Integrity of the Drive
					CRandom* pRandom = CGameObjects::Get().Rand();
					pBackup->Damage(pRandom->NumberRange(1, 100)); // #TODO# Determine realisitic damage

					// Inform player of this
					pShip->Write(CShip::MT_SYSTEMS, "[%s] %s is overheating.\n\r", this->m_gsName, pBackup->m_gsName);
				}
			}

			// #TODO# Move the Ship through Hyperspace
		}
	}

	pShip = NULL;
}

void CHyperdrive::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CHyperdrive::ReadExtra(TiXmlNode* pParent)
{
	return;
}