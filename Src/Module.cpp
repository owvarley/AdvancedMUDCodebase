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

// Class    :: CModule
// Header   :: Module.h
// Function :: Handles the functions and methods for all Modules

#include "Module.h"
#include "GameObjects.h"
#include "Tools.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Spatial.h"
#include "OTools.h"

// This variable is used to determine a unique ID for each actor created.
UINT uiUniqueModuleID = 0;

char* CModule::szModules[] = {	"None",									"Armour Plating",
								"Shield Generator",						"Power Link",
								"Reactor Plant",						"Coolant Plant",
								"Fuel Line",							"Battery Bank",
								"Ion Engine",							"Drive Baffles",
								"Hyperdrive Main",						"Hyperdrive Backup",
								"Coolant Line",							"Missiles",
								"Torpedos",								"Rockets",
								"Chaff",								"Flares",
								"Collector Array",						"Radome",
								"Manuevering Thrusters",				"Heatsink",
								"Gravity Well Projectors",				"Flare and Chaff Dispensor",
								"Communications",						"Jamming Pod",
								"Snooping Pod",							"Astrogation computer",
								"Repulsor Coils",						"Life Support Unit",
								"Holo-Net Transceiver",					"Blast Doors",
								"Tractor Beam Projector",				"Fuel Cells",
								"Fabrication Material",					"Control Console",
								"Piloting Console",						"Gunnery Console",
								"Secondary Console",					"Copilot Console",
								"Navigation Console",					"Command Console",
								"Engineering Console",					"Weapon Console",
								"Helm",									"Turret Mount",
								"Munition Elevator",					"Escape Pod",
								"S-foil and Actuators", NULL };

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Module Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
CMLoader::CMLoader()
{

}

CMLoader::~CMLoader()
{
	this->m_Modules.clear();
}

void CMLoader::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pModNode = pParent->FirstChild("Module");

	while (pModNode != NULL)
	{
		gString gsFilename, gsFile;

		Tools.ReadXml(pModNode, "filename",	gsFile);

		gsFilename.Format("%sShips\\Modules\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						

		CModule* pMod = new CModule;

		pMod->Load(gsFilename);

		// Add it
		m_Modules.push_back(pMod);
		
		pModNode = pModNode->NextSibling("Module");
	}

	return;
}

void CMLoader::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	for (ModuleList::iterator mod = m_Modules.begin(); mod != m_Modules.end(); mod++)
	{
		TiXmlNode* pModNode = Tools.InsertXmlChild(pParent, "Module");

		Tools.WriteXml(pModNode, "filename",	(*mod)->m_gsFileName);
	}

	return;
}

bool CMLoader::Load(gFileName gfFile)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gfFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("ModuleLoader");

	ReadXml(pNode);
	return true;

	return true;
}

bool CMLoader::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "ModuleLoader");

	gString gsFile;

	gsFile.Format("%sShips\\Modules\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], "Modules.dat");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsFile);

	return true;
}


///////////////////////////////////////////////////////////////////////////////////////////
// 1. Module  Class
///////////////////////////////////////////////////////////////////////////////////////////
CModule::CModule()
{
	m_uiUniqueID = ++uiUniqueModuleID;
	m_gsName = "";
	m_gsFileName = "";
	m_nType = 0;
	m_nSize = 0;
	m_nMass = 0;
	m_nResilience = 0;
	m_nmDurability = 100;
	m_ncDurability = 100;
	m_Installed.Set(0, 0, -1);

	m_Comms = NULL;
	m_Manned = NULL;
}

CModule::~CModule()
{
	m_gsName = "";
	m_gsFileName = "";
	m_nType = 0;
	m_nSize = 0;
	m_nMass = 0;
	m_nResilience = 0;
	m_nmDurability = 0;
	m_ncDurability = 0;

	m_Comms = NULL;
	m_Manned = NULL;
	m_Messages.clear();
}

// Copy a module
CModule& CModule::operator = (CModule& mod)
{
	m_gsName = mod.m_gsName;
	m_gsFileName = mod.m_gsFileName;
	m_ncDurability = mod.m_ncDurability;
	m_nMass = mod.m_nMass;
	m_nResilience = mod.m_nResilience;
	m_nmDurability = mod.m_nmDurability;
	m_nSize = mod.m_nSize;
	m_nType = mod.m_nType;
	
	if (mod.m_Comms)
		m_Comms = new CMComms;

	for (IntegerMap::iterator plus = mod.m_Plus.begin(); plus != mod.m_Plus.end(); plus++)
	{
		m_Plus.insert(IntegerMap::value_type(((*plus).first), ((*plus).second)));
	}

	return *this;
} 

// Determines if a terminal can receive this type of message
bool CModule::CanReceive(int nType)
{
	// [1] MT_ENTIRESHIP	= Message that is given to the entire ship
	// [2] MT_HELM			= Message to the Helm only
	// [3] MT_LEEHELM		= Discontinued to be replaced. Replaced {OWV} @ 1/6/06
	// [4] MT_SENSORS		= Messages to the Sensor Terminal
	// [5] MT_ASTROGATION	= Messages to the Astrogation Terminal
	// [6] MT_COMMS			= Messages to the Comms Terminal
	// [7] MT_SYSTEMS		= Messages to the Systems Terminal
	// [8] MT_FCS			= Messages to the Fire Control System
	// [9] MT_CRITICAL		= Messages to all Control Points
	// [0] MT_HULLCUBE		= Messages to a specific HullCube

	// Starfighters / Smallship
	// [1] Single-seat
	//     >> Control console: ALL TERMINALS
	//	 
	// [2] Two-seater
	//     >> Piloting console: Helm, Sensors, Comms, Systems
	//     >> Gunnery console: Master FCS, Nav, Sensors, Systems
	//     
	// [3] Dual-control (TRAINING)
	//     >> Control console: ALL TERMINALS
	//     >> Secondary console: ALL TERMINALS
	//     
	// Midship
	// [1] Standard
	//     >> Pilot console: Helm, Sensors, Comms, Systems
	//     >> Co-pilot console: Helm, Sensors, Master FCS, Systems 
	//     >> Navigation console: Nav
	//     
	// Largeship+
	// [1] Standard
	//     >> Commander console: Overview of ALL TERMINALS
	//     >> Engineering console: Systems
	//     >> Weapons console: Master FCS
	//     >> Helm console: Nav, Comm, Sensors, Helm

	switch (nType)
	{
	case CShip::MT_HELM:
		{
			if (this->m_nType == CModule::MT_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_PILOT_CONSOLE ||
				this->m_nType == CModule::MT_COPILOT_CONSOLE || 
				this->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_HELM)
				return true;
		}
		break;

	case CShip::MT_SENSORS:
		{
			if (this->m_nType == CModule::MT_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_PILOT_CONSOLE ||
				this->m_nType == CModule::MT_HELM ||
				this->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_GUNNERY_CONSOLE ||
				this->m_nType == CModule::MT_COPILOT_CONSOLE)
				return true;
		}
		break;

	case CShip::MT_ASTROGATION:
		{
			if (this->m_nType == CModule::MT_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_HELM ||
				this->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_PILOT_CONSOLE ||
				this->m_nType == CModule::MT_NAV_CONSOLE)
				return true;
		}
		break;

	case CShip::MT_COMMS:
		{
			if (this->m_nType == CModule::MT_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_HELM ||
				this->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_GUNNERY_CONSOLE)
				return true;
		}
		break;

	case CShip::MT_SYSTEMS:
		{
			if (this->m_nType == CModule::MT_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_COPILOT_CONSOLE ||
				this->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_GUNNERY_CONSOLE ||
				this->m_nType == CModule::MT_ENGINEERING_CONSOLE ||
				this->m_nType == CModule::MT_PILOT_CONSOLE)
				return true;
		}
		break;

	case CShip::MT_FCS:
		{
			if (this->m_nType == CModule::MT_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_COPILOT_CONSOLE ||
				this->m_nType == CModule::MT_SECONDARY_CONTROL_CONSOLE ||
				this->m_nType == CModule::MT_GUNNERY_CONSOLE ||
				this->m_nType == CModule::MT_WEAPONS_CONSOLE)
				return true;
		}
		break;


	}

	return false;
}

// Determines if a Module is Powered up or not
bool CModule::Powered()
{
	IntegerMap::iterator it = this->m_Plus.find("Powered");

	// If there isn't a powered flag do nothing
	if (it == this->m_Plus.end())
		return false;

	// Is it Powered?
	if ((*it).second)
		return true;

	return false;
}

// Returns an Extra value stored in the Integer Map
int CModule::Plus(gString gsType, bool bActual)
{
	IntegerMap::iterator it = this->m_Plus.find(gsType);
	int nValue;

	// Did we find it?
	if (it == this->m_Plus.end())
		return 0;

	if (bActual)
	{
		nValue = (*it).second;

		float nEfficiency = ((float)this->m_ncDurability / (float)this->m_nmDurability); // Curr/Max
		int nMod = nValue * nEfficiency;

		return nMod;
	}

	nValue = (*it).second;

	return nValue;
}

bool CModule::SetPlus(gString gsType, int nValue)
{
	IntegerMap::iterator it = this->m_Plus.find(gsType);

	// Does this type exist?
	if (it == this->m_Plus.end())
		return false;

	(*it).second = nValue;
	return true;
}

// Damages a module
int CModule::Damage(int nAmount)
{
	// Armour works differently to normal Modules
	if (this->m_nType == MT_ARMOUR_PLATING)
	{
		// If the amount of damage is greater than the dissipation rate 
		// then the damage is inflicted upon the armour's durability
		if (this->Plus("Dissipation", false) >= nAmount)
			return 0;
		else
		{
			int nDurability = this->m_ncDurability;
			
			nDurability -= nAmount;

			if (nDurability <= 0)
			{
				this->m_ncDurability = 0;
				return abs(nDurability); // We destroyed the armour completely so we need to carry the damage over
			}
			else
			{
				this->m_ncDurability = nDurability;
				return 0;
			}

		}
	}
	else
	{
		// If a Module is destroyed it has Integrity -1
		if (this->m_ncDurability < 0)
			return nAmount;

		// Deduct the Damage
		this->m_ncDurability -= nAmount;

		// We only destroy the module in an Update function so we can inform the player
		if (this->m_ncDurability < 0)
		{
			int nOver = this->m_ncDurability;
			this->m_ncDurability = 0;
			return abs(nOver);	// We completely destroyed this module, so lets carry this over
		}
	}

	return 0;
}

// Returns true if the string is a valid Module type
bool CModule::Valid(gString type)
{
	for (int i = 0; i < CModule::MT_LAST; i++)
	{
		gString gsModule = CModule::szModules[i];
		if (gsModule == type)
			return true;
	}

	return false;
}

// Returns the Index value of a module type
int CModule::GetIndex(gString type)
{
	for (int i = 0; i < CModule::MT_LAST; i++)
	{
		gString gsModule = CModule::szModules[i];
		if (gsModule == type)
			return i;
	}

	return -1;
}

void CModule::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "filename",		m_gsFileName);
	Tools.ReadXml(pParent, "size",			m_nSize);
	Tools.ReadXml(pParent, "mass",			m_nMass);
	Tools.ReadXml(pParent, "type",			m_nType);
	Tools.ReadXml(pParent, "resilience",	m_nResilience);
	Tools.ReadXml(pParent, "durabilityc",	m_ncDurability);
	Tools.ReadXml(pParent, "durabilitym",	m_nmDurability);

	TiXmlNode* pPlusNode = pParent->FirstChild("Plus");

	// Read in all the plus fields
	while (pPlusNode != NULL)
	{
		gString gsName;
		int nValue;

		Tools.ReadXml(pPlusNode, "name",	gsName);
		Tools.ReadXml(pPlusNode, "value",	nValue);

		m_Plus.insert(IntegerMap::value_type(gsName, nValue));

		pPlusNode = pPlusNode->NextSibling("Plus");
	}

	TiXmlNode* pCommsNode = pParent->FirstChild("Comms");

	if (pCommsNode != NULL)
	{
		// Create the Comms object
		m_Comms = new CMComms;

		TiXmlNode* pMessageNode = pCommsNode->FirstChild("Message");

		while (pMessageNode != NULL)
		{
			CMessage* pMsg = new CMessage;
			pMsg->ReadXml(pMessageNode);
			m_Comms->m_Recordings.push_back(pMsg);

			pMessageNode = pMessageNode->NextSibling("Message");
		}

		TiXmlNode* pProtocolNode = pCommsNode->FirstChild("Protocol");

		while (pProtocolNode != NULL)
		{
			gString gsCode;

			Tools.ReadXml(pProtocolNode, "code", gsCode);

			this->m_Comms->m_Protocols.push_back(gsCode);
		}
	}

	return;

}

void CModule::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",			m_gsName);
	Tools.WriteXml(pParent, "filename",		m_gsFileName);
	Tools.WriteXml(pParent, "size",			m_nSize);
	Tools.WriteXml(pParent, "mass",			m_nMass);
	Tools.WriteXml(pParent, "type",			m_nType);
	Tools.WriteXml(pParent, "resilience",	m_nResilience);
	Tools.WriteXml(pParent, "durabilityc",	m_ncDurability);
	Tools.WriteXml(pParent, "durabilitym",	m_nmDurability);
	
	for (IntegerMap::iterator imap = m_Plus.begin(); imap != m_Plus.end(); imap++)
	{
		TiXmlNode* pPlusNode = Tools.InsertXmlChild(pParent, "Plus");
		gString gsName = (*imap).first;
		Tools.WriteXml(pPlusNode, "name",  gsName);
		Tools.WriteXml(pPlusNode, "value", (*imap).second);
	}

	if (m_Comms)
	{
		TiXmlNode* pCommsNode = Tools.InsertXmlChild(pParent, "Comms");

		MsgList mMessages = m_Comms->m_Recordings;
		// Save Recorded Messages
		for (MsgList::iterator msgs = mMessages.begin(); msgs != mMessages.end(); msgs++)
		{
			(*msgs)->WriteXml(pCommsNode);
		}
		// Save Encryption Protocols
		for (gStringList::iterator gs = m_Comms->m_Protocols.begin(); gs != m_Comms->m_Protocols.end(); gs++)
		{
			TiXmlNode* pProtocol = Tools.InsertXmlChild(pCommsNode, "Protocol");

			Tools.WriteXml(pProtocol, "code", (*gs));
		}
	}

	return;
}


bool CModule::Load(gString filename)
{	
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)filename) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Module");

	ReadXml(pNode);
	return true;

}

bool CModule::Load()
{	
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Module");

	ReadXml(pNode);
	return true;
}

bool CModule::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Module");

	gString gMod;

	gMod.Format("%s%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_MODULES], this->m_gsFileName);

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gMod);

	return true;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 2. Module Comms Class
///////////////////////////////////////////////////////////////////////////////////////////
CMComms::CMComms()
{
	this->m_nOpen = 0;
	this->m_Recording = NULL;
}

CMComms::~CMComms()
{
	this->m_Jamming.clear();
	this->m_nOpen = 0;
	this->m_Open.clear();
	this->m_Protocols.clear();
	this->m_Recording = NULL;
	this->m_Recordings.clear();
	this->m_Snooping.clear();
}

bool CMComms::Record(CFrequency *pFreq, gString gsMsg, gString gsShip)
{
	// Create the new object and add it to the list of messages
	CMessage* pMsg = new CMessage();

	pMsg->m_gsEncryption = pFreq->m_gsEncryption;
	pMsg->m_gsFrequency.Format("%3.2f", pFreq->m_nFrequency);
	pMsg->m_gsMessage = gsMsg;
	pMsg->m_gsShip = gsShip;
	pMsg->m_tTime = time(0);

	this->m_Recordings.push_back(pMsg);

	return true;
}



// Check if this Channel is Encrypted
bool CMComms::IsEncrypted(CFrequency* pFreq)
{
	if (pFreq->m_gsEncryption == "")
		return false;

	return true;
}

CFrequency::CFrequency()
{
	this->m_nFrequency = 0.0;
	this->m_gsEncryption = "";
}

CFrequency::~CFrequency()
{
	this->m_gsEncryption = "";
	this->m_nFrequency = 0.0;
}

CMessage::CMessage()
{
	this->m_gsEncryption = "";
	this->m_gsMessage = "";
	this->m_gsShip = "";
	this->m_gsFrequency = "";
	this->m_tTime = time(0);
}

CMessage::~CMessage()
{
	this->m_gsEncryption = "";
	this->m_gsMessage = "";
	this->m_gsFrequency = "";
	this->m_gsShip = "";
}

void CMessage::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "encryption",	m_gsEncryption);
	Tools.ReadXml(pParent, "frequency",		m_gsFrequency);
	Tools.ReadXml(pParent, "message",		m_gsMessage);
	Tools.ReadXml(pParent, "recorded",		m_tTime);
	Tools.ReadXml(pParent, "ship",			m_gsShip);

	return;

}

void CMessage::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pMsg = Tools.InsertXmlChild(pParent, "Message");

	Tools.WriteXml(pMsg, "encryption",	m_gsEncryption);
	Tools.WriteXml(pMsg, "frequency",	m_gsFrequency);
	Tools.WriteXml(pMsg, "message",		m_gsMessage);
	Tools.WriteXml(pMsg, "recorded",	m_tTime);
	Tools.WriteXml(pMsg, "ship",		m_gsShip);

	return;

}