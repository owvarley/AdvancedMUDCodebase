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

// Class    :: CShield
// Header   :: Spatial.h
// Function :: Handles the functions and methods for the Ship board shield system

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>

CShield::CShield()
{
	m_Type = (CComponent::CT_SHIELD);
	m_bPowered = false;	
	m_bEmitter = false;
	
	m_Shieldstate = new CShieldState();
	m_Install.insert(InstallMap::value_type(CModule::MT_SHIELD_GENERATOR, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_POWER_LINK, true));
	m_Orientation = new CSet;
		
}

CShield::~CShield()
{
	m_bPowered = false;	
	m_bEmitter = false;

	delete m_Orientation;
	m_Orientation = NULL;
		
	m_Shieldstate = NULL;
}

// Copy a components modules to another component
CShield& CShield::operator = (CShield& comp)
{
	m_gsName = comp.m_gsName;
	m_nSize = comp.m_nSize;
	m_gsShip = comp.m_gsShip;
	m_Type = comp.m_Type;
	*m_Orientation = *comp.m_Orientation;
	
	for (ModuleMap::iterator mod = comp.m_Modules.begin(); mod != comp.m_Modules.end(); mod++)
	{
		CModule* pMod = new CModule;
		*pMod = *((*mod).second);
		m_Modules.insert(ModuleMap::value_type(pMod->m_nType, pMod));
	}

	return *this;
}

void CShield::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip;
	
	// We need to get the ship
	if ( (pShip = pGalaxy->GetShi(this->m_gsShip)) == NULL)
		return;

	// Get all the sub modules
	CModule* pGenerator = Get(CModule::MT_SHIELD_GENERATOR);		// Shield Emitter
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

					if (pMod->m_nType == CModule::MT_SHIELD_GENERATOR)
					{
						// When we power up a Shield Emitter we need to add in a value for each of 
						// the banks it can cover in order to track their values
						int nCount = 0;
						for (int i = 0;i < CShip::A_MAX; i++)
						{
							if (this->m_Orientation->IsSet(i))
								nCount ++;
						}

						for (int i = 0;i < CShip::A_MAX; i++)
						{
							if (this->m_Orientation->IsSet(i))
							{						
								// These will be stored the first time the Capacitor is powered up
								gString gsArc;

								gsArc.Format("Cur%s", CShip::szArc[i]);
								if (pMod->Plus(gsArc, false) == 0)
								{
									pMod->m_Plus.insert(IntegerMap::value_type(gsArc, 0));
									gsArc.Format("Max%s", CShip::szArc[i]);	// E.g. MaxFore
									pMod->m_Plus.insert(IntegerMap::value_type(gsArc, (pMod->Plus("MaxCharge", false)/nCount)));
								}

							}							
						}				

					}

					// The power up rate is the same for all modules
					(*pMod->m_Plus.find("Timer")).second = CComponent::T_SHIELD;
				}
				else
				{
					(*pMod->m_Plus.find("Timer")).second--;
				}
			}
		}
	} // End Timers

	// [1] - Shield Generator
	// [4] - Power link


	
	// [2] - Shield Generator
	// The Shield Generator holds the charge for the shield. These capacitors can be fully charged but not
	// online. When Charging the Capacitors use a high amount of Energy.
	// [Value 1] - Current Charge
	// [Value 2] - Maximum Charge
	if (pGenerator && pGenerator->Powered() && (pPower && pPower->m_ncDurability > 0))
	{
		if (pShip->Battery(true) > 0)
		{
			int nCurrent = 0;
			
			int nCount = 0;
			for (int i = 0;i < CShip::A_MAX; i++)
			{
				if (this->m_Orientation->IsSet(i))
					nCount ++;
			}

			// We also need to charge the Arc specific Capacitor values
			for (int i = 0; i < CShip::A_MAX; i++)
			{
				// Is this mount Orientated in this arc?
				if (this->m_Orientation->IsSet(i))
				{
					gString gsCurr;
					gString gsMax;
					gsCurr.Format("Cur%s", CShip::szArc[i]);
					gsMax.Format("Max%s", CShip::szArc[i]);

					// Charge the Capacitor
					pGenerator->SetPlus(gsCurr, (pGenerator->Plus(gsCurr,false)+ (400/nCount)));

					// Did we over charge?
					if (pGenerator->Plus(gsCurr, false) > pGenerator->Plus(gsMax, false))
						pGenerator->SetPlus(gsCurr, pGenerator->Plus(gsMax, false));

					nCurrent += pGenerator->Plus(gsCurr, false);
				}
	
			}

			pGenerator->SetPlus("CurrCharge", nCurrent);
		}		
	}

	// [3] - Power link
	// If the Power link is destroyed we need to shut down all shield systems
	if (pPower && pPower->m_ncDurability <= 0)
	{
		if (pGenerator->Powered())
		{
			pGenerator->SetPlus("Powered", 0);
			pShip->Write(CShip::MT_SYSTEMS, "[%s] %s knocked offline.\n\r", this->m_gsName, pGenerator->m_gsName);
		}

	}

	pShip = NULL;

}

void CShield::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pExtra = Tools.InsertXmlChild(pParent, "Extra");

	Tools.WriteXml(pExtra, "orientation",	*m_Orientation);

	return;
}


void CShield::ReadExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pExtra = pParent->FirstChild("Extra");

	if (pExtra != NULL)
	{
		Tools.ReadXml(pExtra, "orientation", *m_Orientation);
	}
	
	return;
}


CShieldState::CShieldState()
{
	for (int i = 0; i < CShip::A_MAX; i++)
		m_Shields[i] = 0;
}

CShieldState::~CShieldState()
{
	for (int i = 0; i < CShip::A_MAX; i++)
		m_Shields[i] = 0;
}

bool CShieldState::Set(int nAmount, int nBank)
{
	if (nBank >= CShip::A_MAX)
		return false;

	m_Shields[nBank] = nAmount;

	return true;
}

bool CShieldState::Charge(int nAmount, int nBank)
{
	if (nBank >= CShip::A_MAX)
		return false;

	m_Shields[nBank] += nAmount;

	return true;
}

bool CShieldState::Deplete(int nAmount, int nBank)
{
	if (nBank >= CShip::A_MAX)
		return false;

	m_Shields[nBank] -= nAmount;
	
	return true;
}

int CShieldState::Value()
{
	int nTotal;
	for (int i = 0; i < CShip::A_MAX; i++)
		nTotal += m_Shields[i];

	return nTotal;
}
