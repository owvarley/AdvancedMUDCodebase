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

// Class    :: CComponent
// Header   :: Components.h
// Function :: Handles the functions and methods for all Ship Components

#pragma warning(disable:4786)

#include "GameObjects.h"
#include <direct.h>
#include "Spatial.h"

// Human Readable Versions
char* CComponent::szComponents[] = { "Shield Mount", "Engineering Space", "Sublight Drive", "Hyperdrive Mount", "Magazine", "External System Mount",
									"Internal System Mount", "Landing Bay", "Bulk Storage", "Control Point", "Weapon Mount",
									"Escape Pod Housing", "S-Foil System", NULL };



char* CComponent::szClassnames[] = { "Shield", "Engspace", "Sublight", "Hyperdrive", "Magazine", "External", "Internal", "Landing",
									"Bulk", "Control", "Weapon", "Escape", "SFoil", NULL};


// Mapping for Impulse tags
// Method     :: ValidH
// Arguments  :: <header>
// Return     :: Bool
// Function   :: Used to compact the load component section of loading a ship. This check will determine whether the
//               current gsKey is a valid component. 
// Written    :: 18/11/05
bool CComponent::ValidH(gString header)
{
	for (int i = 0; i < CComponent::CT_LAST; i++)
	{
		// We just need to add the []'s to our classnames
		gString gsFormatted;
		gsFormatted.Format("[%s]", szClassnames[i]);

		// Is there a classname with this header?
		if (gsFormatted == header)
			return true;

		gsFormatted.Format("[/%s]", szClassnames[i]);
		
		// Check if its the end bracket for this class as well
		if (gsFormatted == header)
			return true;

	}

	// Nothing found
	return false;

}

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Component Class
///////////////////////////////////////////////////////////////////////////////////////////

CComponent::CComponent()
{
	m_gsName = "";
	m_gsShip = "";
	m_Type = 0;
	m_Installed.Set(0, 0, -1);
}

CComponent::~CComponent()
{
	m_Type = 0;
	m_gsName = "";
	m_gsShip = "";
	m_Modules.clear();
}

// Copy a components modules to another component
CComponent& CComponent::operator = (CComponent& comp)
{
	m_gsName = comp.m_gsName;
	m_nSize = comp.m_nSize;
	m_Type = comp.m_Type;
	m_gsShip = comp.m_gsShip;
	
	for (ModuleMap::iterator mod = comp.m_Modules.begin(); mod != comp.m_Modules.end(); mod++)
	{
		CModule* pMod = new CModule;
		*pMod = *((*mod).second);
		m_Modules.insert(ModuleMap::value_type(pMod->m_nType, pMod));
	}

	return *this;
}

bool CComponent::Install(gString gsShip)
{
	return true;
}

int CComponent::FreePower()
{
	int nMaxPower = 0;
	int nCurrPower = 0;

	// Get the power link
	CModule* pMod = this->Get(CModule::MT_POWER_LINK);

	// Is there one
	if (pMod)
	{
		nMaxPower = pMod->Plus("PowerRate", true);

		for (ModuleMap::iterator mod = m_Modules.begin(); mod != m_Modules.end(); mod++)
		{
			// If this component is using more than 0 load, use it
			nCurrPower = (*mod).second->Plus("PowerLoad", false);
			if (nCurrPower > 0)
			{
				nMaxPower -= nCurrPower;
			}
		}
	}
	else
	{
		// No power link, no free power, in fact no power at all
		return 0;
	}
	
	if (nMaxPower < 0)
		return 0;
	else
		return nMaxPower;
}

int CComponent::MaxPower()
{
	// Get the power link
	CModule* pMod = this->Get(CModule::MT_POWER_LINK);

	// Is there one
	if (pMod)
	{
		// The max power of this component is the power rate of the power link
		return pMod->Plus("PowerRate", true);		
	}

	return 0;
}


// Damage at the Component level simply translates down to the modules
// if a module receives more damage than it can take it is destroyed and 
// the rest of the damage is passed onto any other modules there are 
// within the Component
int CComponent::Damage(int nAmount)
{
	// To damage a component we simply damage each of its modules
	int nRem = nAmount;

	CModule* pArmour = this->Get(CModule::MT_ARMOUR_PLATING);

	// This component is armoured so we need to see if the armour will absorb this damage first
	if (pArmour)
	{
		// If we blast through the armour we need to damage other modules in the location
		nRem = pArmour->Damage(nAmount);
	}
	
	// We still have damage to deal
	if (nRem > 0)
	{
		// Here we need to iterate through the module list and damage each module
		// we need to make sure that if the damage is enough to destroy the module
		// that we carry over the remaining damage and apply it to other modules
		for (ModuleMap::iterator mod = this->m_Modules.begin(); mod != this->m_Modules.end(); mod++)
		{
			// The damage of Armour is handled above
			if ((*mod).second->m_nType == CModule::MT_ARMOUR_PLATING)
				continue;

			int nAmount = ((*mod).second)->Damage(nRem);

			// We've applied all Damage so we are finished
			if (nAmount == 0)
				return 0;

			// We still have more damage to apply
			nRem = nAmount;
		}
	}

	return nRem;
}


// Allows us to obtain the Hullcube this Component has been installed into
// will either return the Hullcube if there is one, or NULL otherwise.
CHull* CComponent::GetHull(CShip* pShip)
{
	CHull* pHull = NULL;

	// Iterate through the list of hull cubes and locate this Component
	for (FrameList::iterator frame = pShip->m_Frames.begin(); frame != pShip->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		// This component
		if ((*comp)->m_gsName == this->m_gsName)
			pHull = (*hull);
	}

	return pHull;
}

bool CComponent::Tamper()
{
	return true;
}


// Virtual method that is implemented by all classes which inherit from CCComponent
void CComponent::Update()
{
}

// Virtual method
void CComponent::ReadExtra(TiXmlNode* pParent)
{ 
}

// Virtual method
void CComponent::WriteExtra(TiXmlNode* pParent)
{ 
	return;
}

// Returns the integrity of the Component based on the integrity of its Modules
int CComponent::Integrity()
{
	int nMax = 0;
	int nCur = 0;

	nMax = (100 * this->m_Modules.size());
	nCur = 0;

	for (ModuleMap::iterator it = this->m_Modules.begin(); it != this->m_Modules.end(); it++)
	{
		nCur += ((*it).second)->m_ncDurability;
	}

	return ((float)nCur / (float)nMax) * 100;
}

bool CComponent::Uninstall()
{
	return true;
}

// Returns the remaining Space (Size) of the Component
int CComponent::Size()
{
	int nCount = 0;

	for (ModuleMap::iterator mod = this->m_Modules.begin(); mod != this->m_Modules.end(); mod++)
	{
		nCount += ((*mod).second)->m_nSize;
	}

	return (this->m_nSize - nCount);

}

// Returns the total Mass of the Component
int CComponent::Mass()
{
	int nCount = 0;

	for (ModuleMap::iterator mod = this->m_Modules.begin(); mod != this->m_Modules.end(); mod++)
	{
		nCount += ((*mod).second)->m_nMass;
	}

	return nCount;

}

// Returns a Module of a specific type
CModule* CComponent::Get(int nType)
{
	ModuleMap::iterator map;

	map = this->m_Modules.find(nType);

	if (map == this->m_Modules.end())
		return NULL;

	return (*map).second;
}

// Returns a Module with a specific name
CModule* CComponent::Get(gString gsName)
{
	ModuleMap::iterator map;

	for (map = this->m_Modules.begin(); map != this->m_Modules.end(); map++)
	{
		if (((*map).second)->m_gsName.HasPrefix(gsName))
			return (*map).second;
	}
	
	return NULL;
}

// Determines whether a Module can be installed in this Component
bool CComponent::CanInstall(int nType)
{
	InstallMap::iterator map;

	map = this->m_Install.find(nType);

	// Updates for MSVC++ 8.0+
	if (map != this->m_Install.end())
		return true;

	return false;
}

// Remove all sub-modules and reflag Component
bool CComponent::Destroy()
{

	this->m_Modules.clear();
	this->m_gsName.Format("[Destroyed] %s", this->m_gsName);

	return true;
}


// Returns true if the string is a valid Component type
bool CComponent::Valid(gString type)
{
	for (int i = 0; i < CComponent::CT_LAST; i++)
	{
		gString gsComponent = CComponent::szComponents[i];
		if (gsComponent == type)
			return true;
	}

	return false;
}

// Returns the Index value of a Component type
int CComponent::GetIndex(gString type)
{
	for (int i = 0; i < CComponent::CT_LAST; i++)
	{
		gString gsComponent = CComponent::szComponents[i];
		if (gsComponent == type)
			return i;
	}

	return -1;
}

void CComponent::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",				m_gsName);
	Tools.WriteXml(pParent, "ship",				m_gsShip);
	Tools.WriteXml(pParent, "installed",		m_Installed);
	Tools.WriteXml(pParent, "size",				m_nSize);
	Tools.WriteXml(pParent, "type",				m_Type);

	for (ModuleMap::iterator mod = m_Modules.begin(); mod != m_Modules.end(); mod++)
	{
		TiXmlNode* pModule = Tools.InsertXmlChild(pParent, "Module");
		Tools.WriteXml(pModule, "filename",		(*mod).second->m_gsFileName);
		Tools.WriteXml(pModule, "durability",	(*mod).second->m_ncDurability);
		Tools.WriteXml(pModule, "installed",	((*mod).second)->m_Installed);
	}

	// Write any Extra details
	//WriteExtra(pParent);

	return;
}

void CComponent::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",				m_gsName);
	Tools.ReadXml(pParent, "ship",				m_gsShip);
	Tools.ReadXml(pParent, "installed",			m_Installed);
	Tools.ReadXml(pParent, "size",				m_nSize);
	Tools.ReadXml(pParent, "type",				m_Type);

	TiXmlNode* pModuleNode = pParent->FirstChild("Module");

	// Read in each module
	while (pModuleNode != NULL)
	{
		gString gsFile;
		Tools.ReadXml(pModuleNode, "filename", gsFile);
	
		gsFile.Format("%s%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_MODULES], gsFile);						

		CModule* pMod = new CModule();					
		pMod->Load(gsFile);

		// Now we need to set the values we saved
		Tools.ReadXml(pModuleNode, "durability",	pMod->m_ncDurability);	// Damage done
		Tools.ReadXml(pModuleNode, "installed",		pMod->m_Installed);		// Where module is installed

		m_Modules.insert(ModuleMap::value_type(pMod->m_nType, pMod));

		pModuleNode = pModuleNode->NextSibling("Module");
	}

	// Read in the extra stuff
	//ReadExtra(pParent);


	return;

}

