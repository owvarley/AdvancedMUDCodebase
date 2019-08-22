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

// Class    :: CExternal
// Header   :: Spatial.h
// Function :: Implements the External Systems Mount Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. External Class 
///////////////////////////////////////////////////////////////////////////////////////////

CExternal::CExternal()
{
	m_Type = (CComponent::CT_EXTERNAL);
	m_Install.insert(InstallMap::value_type(CModule::MT_COLLECTOR_ARRAY, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_RADOME, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_MANUEVERING_THRUSTERS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_HEATSINK, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_GRAVITY_WELL_PROJECTORS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_FLARE_AND_CHAFF_DISPENSOR, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_COMMUNICATIONS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_JAMMING_POD, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_SNOOPING_POD, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_ASTROGATION_COMPUTER, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));

}


CExternal::~CExternal()
{
	m_Type = 0;
}

void CExternal::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	bool bPowered = false;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pCollector = Get(CModule::MT_COLLECTOR_ARRAY);				// Collector Array
	CModule* pRadome = Get(CModule::MT_RADOME);							// Sensor Radomes
	CModule* pThrusters = Get(CModule::MT_MANUEVERING_THRUSTERS);		// Maneuvering Thrusters
	CModule* pHeatsink = Get(CModule::MT_HEATSINK);						// Heatsinks
	CModule* pGravwells = Get(CModule::MT_GRAVITY_WELL_PROJECTORS);		// Gravity Well Projectors
	CModule* pFlareChaff = Get(CModule::MT_FLARE_AND_CHAFF_DISPENSOR);	// Flare and Chaff Projectors
	CModule* pComms = Get(CModule::MT_COMMUNICATIONS);					// Communications
	CModule* pJamming = Get(CModule::MT_JAMMING_POD);					// Jamming Pod
	CModule* pSnooping = Get(CModule::MT_SNOOPING_POD);					// Snooping Pod
	CModule* pPower = Get(CModule::MT_POWER_LINK);						// Power link
	 
	// Update the powerstate of modules
	// This iterates through all modules and checks the amount of power they are receiving
	// if they are trying to power up and have not received their nominal amount to operate
	// and their is free power capacity, then we give the module more power.
	for (ModuleMap::iterator map = this->m_Modules.begin(); map != this->m_Modules.end(); map++)
	{
		CModule* pMod = ((*map).second);

		if (!pMod || pMod->Plus("Power", true) == 0)
			continue;

		// Is it trying to power up?
		if ((*pMod->m_Plus.find("Power")).second)
		{
			// Is there a powerlink that is functional
			if (pPower && pPower->m_ncDurability > 0)
			{
				// Now we transfer energy from the reactor to our module

				// Free power is the amount of capacity the ship's reactors have that isn't being used
				int nFreePower = pShip->FreePower();
				// Power rate is the rate at which power can be transfered by the power link
				int nPowerRate = pPower->Plus("PowerRate", true);
				// Required power is the remaining power that the module needs in order power up
				int nRequiredPower = pMod->Plus("PowerUsage", false) - pMod->Plus("PowerLoad", false);

				if (nFreePower > 0)
				{
					// Check to see if we have enough free power
					if (nFreePower < pPower->Plus("PowerRate", true))
					{
						// There is some free power but not a the full power rate
						// Now we check if this extra power will fully power the module
						// We have enough power to fully power this module
						if (nRequiredPower <= nFreePower)
						{
							pMod->SetPlus("PowerLoad", pMod->Plus("PowerUsage", false));						
							pShip->Write(CShip::MT_SYSTEMS, "#200[#201%s#200]#700 %s #201online#700, power nominal.\n\r", this->m_gsName, pMod->m_gsName);
						}
						else
						{
							pMod->SetPlus("PowerLoad", pMod->Plus("PowerLoad", false) + nFreePower);
							pShip->Write(CShip::MT_SYSTEMS, "#200[#201%s#200]#700 %s #200online#700.\n\r#100[#101Warning#100]#700 %s running at %0.0f nominal power level.", this->m_gsName, pMod->m_gsName, pMod->m_gsName, (float)(pMod->Plus("PowerLoad", false)/pMod->Plus("PowerCapacity", false))*100.0);
						}

						pMod->SetPlus("Power", false);
						pMod->SetPlus("Powered", true);
					}
					else
					{
						// Full power link's rate
						if (nPowerRate > nRequiredPower)
						{
							// Power the Module up
							pMod->SetPlus("Power", false);
							pMod->SetPlus("Powered", true);
							pMod->SetPlus("PowerLoad", pMod->Plus("PowerUsage", false));						
							pShip->Write(CShip::MT_SYSTEMS, "#200[#201%s#200]#700 %s #201online#700, power nominal.\n\r", this->m_gsName, pMod->m_gsName);
						}
						else
						{
							// Increase the power it has
							pMod->SetPlus("PowerLoad", pMod->Plus("PowerLoad", false) + nPowerRate);
						}
					}
				}
			}

			if (pMod->m_nType == CModule::MT_COMMUNICATIONS && pMod->Plus("Powered", false))
			{
				if (pMod->m_Comms)
				{
					// Default Galaxy wide communications channel
					CFrequency* pFreq = new CFrequency();
					pFreq->m_nFrequency = 150.15f;
					pMod->m_Comms->m_Open.push_back(pFreq);
				}
			}

		}			
	}
	
	// [1] - Collector Array
	// [2] - Radome
	// [3] - Maneuvering Thrusters
	// [4] - Heatsink
	// [5] - Gravwells
	// [6] - Flare + Chaff
	// [7] - Comms Array
	// [8] - Jamming Pod
	// [9] - Snooping Signal
	// [10] - Power link

	// [1] - Collector Array
	// Collector Arrays replenish a small amount of Fuel Cells every tick
	// [Value 1] - Rate of Fuel Replenishment
	if (pCollector && pCollector->Powered() && (pPower && pPower->m_ncDurability > 0))
	{
		// Its on, its powered lets recharge some fuel cells
		pShip->ConsumeFuel(-pCollector->Plus("RechargeRate", true));
	}

	// [2] - Radome
	// Radome allows the use of sensors onboard the vessel
	// [Value 1] - Type of Sensor
	// [Value 2] - Range
	// [Active ] - Active/Passive
	if (pRadome && (pPower && pPower->m_ncDurability > 0))
	{
		if (pRadome->Powered())
		{
			int nNumNew = 0;
			
			// Is it active?
			if (pRadome->Plus("Active", false) && pShip->m_gsSector != "")
			{
				int nRange = pRadome->Plus("Range", true);

				// If so then lets poll the system for new contacts
				// We can shorten our search by only getting the list of contacts in this Sector
				SpatialMap::iterator find = pGalaxy->m_SpatialMap.find(pShip->m_gsSector);

				for (SpatialList::iterator spa = (*find).second.begin(); spa != (*find).second.end(); spa++)
				{
					CSpatial* pSpatial = ***spa;

					if (!pSpatial)
					{
						(*find).second.erase(spa);
						continue;
					}

					if (pSpatial->m_gsSector == pShip->m_gsSector)
					{
						long double nDistance = pSpatial->m_Location->Distance(pShip->m_Location);

						// Simple Distance check to make sure the contact 
						// is within range of this radome
						if (nDistance > nRange)
							continue;

						// Don't confuse your own ship's signal
						if (pSpatial == pShip)
							continue;

						// If we don't already have the contact add it
						if (!pShip->Contains(pSpatial))
						{
							// Found a new contact
							nNumNew++;

							CContact* pContact = new CContact;

							// Only record the reading for the type of the Radome installed here
							if (pRadome->Plus("Type", false) == CSpatial::SI_EM)
								pContact->m_Signature[CSpatial::SI_EM] = pSpatial->m_Signature[CSpatial::SI_EM];
							else if (pRadome->Plus("Type", false) == CSpatial::SI_HEAT)
								pContact->m_Signature[CSpatial::SI_HEAT] = pSpatial->m_Signature[CSpatial::SI_HEAT];
							else if (pRadome->Plus("Type", false) == CSpatial::SI_ION)
								pContact->m_Signature[CSpatial::SI_ION] = pSpatial->m_Signature[CSpatial::SI_ION];
							else if (pRadome->Plus("Type", false) == CSpatial::SI_MASS)
								pContact->m_Signature[CSpatial::SI_MASS] = pSpatial->m_Signature[CSpatial::SI_MASS];
							
							pContact->m_Spatial = pSpatial->m_Vnum;
							pContact->m_Location = pSpatial->m_Location;
							pShip->m_Contacts.insert(ContactMap::value_type(pShip->m_Contacts.size()+1, pContact));
						}
						else
						{
							// If it already has the contact we need to update its details
							CContact* pContact = NULL;

							std::vector<ContactMap::iterator>ContactDelete;
							for (ContactMap::iterator con = pShip->m_Contacts.begin(); con != pShip->m_Contacts.end(); con++)
							{
								CSpatial* pFound = **(((*con).second)->m_Spatial);

								if (!pSpatial)
								{
									// Contact no longer exists, we add it to the delete list
									ContactDelete.push_back(con);
									continue;
								}
							
								// Found it!
								if (pFound->m_gfFileName == pSpatial->m_gfFileName)
									pContact = ((*con).second);
							}

							if (pContact)
							{
								// Update Readings
								if (pRadome->Plus("Type", false) == CSpatial::SI_EM)
									pContact->m_Signature[CSpatial::SI_EM] = pContact->m_Signature[CSpatial::SI_EM];
								else if (pRadome->Plus("Type", false) == CSpatial::SI_HEAT)
									pContact->m_Signature[CSpatial::SI_HEAT] = pContact->m_Signature[CSpatial::SI_HEAT];
								else if (pRadome->Plus("Type", false) == CSpatial::SI_ION)
									pContact->m_Signature[CSpatial::SI_ION] = pContact->m_Signature[CSpatial::SI_ION];
								else if (pRadome->Plus("Type", false) == CSpatial::SI_MASS)
									pContact->m_Signature[CSpatial::SI_MASS] = pContact->m_Signature[CSpatial::SI_MASS];
							}
						}
						
					}
				}
			}

			// Did we detect a new contact?
			if (nNumNew > 0)
				pShip->Write(CShip::MT_SENSORS, "#100[#101Sensor Alert#100] Sensors have detected %d new contact%s\n\r", nNumNew, nNumNew > 1 ? "s" : "");
		}
	}
	// [4] - Heat sink
	// The Heatsink helps us to cool down by producing coolant
	// [CoolantRate] - Rate of coolant production
	if (pHeatsink)
	{
		if (pShip->m_nCoolant <= 50000)
			pShip->m_nCoolant += pHeatsink->Plus("CoolantRate", true);
	}

	
}

void CExternal::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CExternal::ReadExtra(TiXmlNode* pParent)
{
	return;
}


CContact::CContact()
{
	this->m_Location = new CCart;
	this->m_Spatial = NULL;
	this->m_Signature[CSpatial::SI_EM] = 0;
	this->m_Signature[CSpatial::SI_HEAT] = 0;
	this->m_Signature[CSpatial::SI_ION] = 0;
	this->m_Signature[CSpatial::SI_MASS] = 0;
}

CContact::~CContact()
{
	this->m_Location = NULL;
	this->m_Spatial = NULL;
	this->m_Signature[0];
}


bool CContact::operator == (CContact a2)
{
	CSpatial* pA = **this->m_Spatial;
	CSpatial* pB = **a2.m_Spatial;

	if (!pA || !pB)
		return false;

	return ( pA->m_gfFileName == pB->m_gfFileName);
}