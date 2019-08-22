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

// Class    :: CWeapon
// Header   :: Spatial.h
// Function :: Handles the functions and methods the Weapon Mount component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Weapon Class 
///////////////////////////////////////////////////////////////////////////////////////////

CWeapon::CWeapon()
{
	m_Type = (CComponent::CT_WEAPONMOUNT);
	m_Install.insert(InstallMap::value_type(CModule::MT_ARMOUR_PLATING, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_TURRET_MOUNT, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_MUNITION_ELEVATOR, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COOLANT_LINE, true));
	m_Orientation = new CSet;
}


CWeapon::~CWeapon()
{
	m_Type = 0;
	delete m_Orientation;
	m_Orientation = NULL;
}

// Copy a components modules to another component
CWeapon& CWeapon::operator = (CWeapon& comp)
{
	m_gsName = comp.m_gsName;
	m_Type = comp.m_Type;
	m_nSize = comp.m_nSize;
	m_gsShip = comp.m_gsShip;
	*m_Orientation = *comp.m_Orientation;
	
	for (ModuleMap::iterator mod = comp.m_Modules.begin(); mod != comp.m_Modules.end(); mod++)
	{
		CModule* pMod = new CModule;
		*pMod = *((*mod).second);
		m_Modules.insert(ModuleMap::value_type(pMod->m_nType, pMod));
	}

	return *this;
}

void CWeapon::Update()
{
		CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pTurret = Get(CModule::MT_TURRET_MOUNT);			// Weapon Mount
	CModule* pElevator = Get(CModule::MT_MUNITION_ELEVATOR);	// Munition Elevator
	CModule* pArmour = Get(CModule::MT_ARMOUR_PLATING);			// Armour Plating
	CModule* pCoolant = Get(CModule::MT_COOLANT_LINE);			// Coolant Line
	CModule* pPower = Get(CModule::MT_POWER_LINK);				// Power link

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
					
					// The power up rate is the same for all modules
					(*pMod->m_Plus.find("Timer")).second = CComponent::T_WEAPON;
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers

	// [1] - Turret mount
	// [2] - Munition Elevator

	// [1] - Turret mount
	// [Weapon Type ] - Launcher, Laser, Ion, Slug
	// [Arc         ] - Port, Starboard, Fore, Aft, Ventral, Dorsal
	// [Energy Load ] - Amount of Energy is uses
	// [Coolant Load] - Amount of Coolant it 
	// We need to consume some coolant from the Turret if it has an operational Coolant line
	// if it doesnt then we need to damage it.
	if (pTurret && pTurret->Powered())
	{
		// Check for a Coolant line now
		if (pCoolant && pCoolant->m_ncDurability > 0)
		{
			if (pShip->m_nCoolant > 0)
				pShip->m_nCoolant-= pTurret->Plus("CoolantLoad", false);
			else
			{
				if (pShip->m_Scm->IsSet(CShip::SCM_OVERHEAT))
				{
					pShip->Write(CShip::MT_SYSTEMS, "[SCM Override] Overheating detected. %s shutdown to prevent Turret damage.\n\r", pTurret->m_gsName);
					pTurret->SetPlus("Powered", 0);	// Turn it off
				}
				else
				{
					// No coolant so we need to reduce the Integrity of the Drive
					CRandom* pRandom = CGameObjects::Get().Rand();
					pTurret->Damage(pRandom->NumberRange(1, 100)); // #TODO# Determine realisitic damage

					// Inform player of this
					pShip->Write(CShip::MT_CRITICAL, "[%s] %s is overheating.\n\r", this->m_gsName, pTurret->m_gsName);
				}
			}

		}
		else // No coolant the turret needs to take damage
		{
			pTurret->Damage(0);
		}

		// If the turret is powered up and the Power link is destroyed, shut it down
		if (pPower && pPower->m_ncDurability <= 0 || !pPower)
		{
			pShip->Write(CShip::MT_SYSTEMS, "[%s] %s knocked offline.\n\r", this->m_gsName, pTurret->m_gsName);
			pTurret->SetPlus("Powered", 0);
		}
	}

	// [2] - Munition Elevator 
	// [LoadTime] - Time it takes to fetch ammunition from the Magazine 
	// [Loaded  ] - Is it currently loaded?
	// [Loading ] - Is it currently loading?
	// The Munition Elevator will work automatically to reload a Weapon Mount when it discharges.
	if (pElevator && pElevator->m_ncDurability > 0 && pPower && pPower->m_ncDurability > 0)
	{
		// First of all is the Turret online? If not we don't need to load it anyway
		if (pTurret && pTurret->Powered())
		{
			// Its Online reload it if its not already loaded
			if (!pElevator->Plus("Loaded", false) && !pElevator->Plus("Loading", false))
			{
				pElevator->SetPlus("Loading", true);
				pElevator->SetPlus("Timer", pElevator->Plus("LoadTime", false));
			}
			
		}

		// Now we need to check our Timer
		if (pElevator->Plus("Loading", false) && pTurret)
		{
			// Is it loaded now?
			if (pElevator->Plus("Timer", false) <= 0)
			{
				// Did we successfully reload?
				if (pShip->Reload(CShip::nAmmoType[pTurret->Plus("WeaponType", false)]))
				{
					if (pTurret->Plus("WeaponType", false) != CShip::OR_TIBANA)
						pShip->Write(CShip::MT_FCS, "#200[#201%s#200]#700 %s #201reloaded#700.\n\r", this->m_gsName, pTurret->m_gsName);


					pElevator->SetPlus("Loading", 0);
					pElevator->SetPlus("Loaded", 1);
					pElevator->SetPlus("Timer", pElevator->Plus("LoadTime", false));
				}
			}
			else
			{
				pElevator->SetPlus("Timer", pElevator->Plus("Timer", false) - 1);
			}
		}


	}

	
	pShip = NULL;
	
}

void CWeapon::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pExtra = Tools.InsertXmlChild(pParent, "Extra");

	Tools.WriteXml(pExtra, "orientation",	*m_Orientation);

	return;
}


void CWeapon::ReadExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pExtra = pParent->FirstChild("Extra");

	if (pExtra != NULL)
	{
		Tools.ReadXml(pExtra, "orientation", *m_Orientation);
	}
	
	return;
}