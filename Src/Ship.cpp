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

// Class    :: CShip, CWeaponGroup
// Header   :: Spatial.h
// Function :: Handles the functions and methods for all Ships which inherit from CSpatial

#pragma warning(disable: 4786)

#include "Spatial.h"
#include "GameObjects.h"
#include "Player.h"
#include <Math.h>
#include "../gTools/Log.h"

char* CShip::szTypes[] = { "Starfighter", "Small Ship", "Midship", "Large Ship", "Capital Ship", "Space Station", NULL };

char* CShip::szSensors[] = { "Heat", "Electro-Magnetic", "Ion", "Mass", "Tactical", NULL };

char* CShip::szOrdinance[] = { "Missile", "Proton Torpedo", "Heavy Rocket", "Tibana Gas", "Chaff", "Flare", "Magnetic Slug", NULL };

char* CShip::szStates[] = { "Landed", "Hovering", "Atmosphere", "Landing", "Launching", "Flying", "General Quarters", NULL };

char* CShip::szScm[] = { "Reactor", "Coolant", "Overheat", "Bulkhead", "Ion", "Loadshed", "Self-destruct", "Max", NULL };

char* CShip::szArc[] = { "Fore", "Aft", "Starboard", "Port", "Ventral", "Dorsal", NULL };

char *CShip::szTurretTypes[] = {"Single", "Dual", "Quad", NULL };

char* CWeaponGroup::szTypes[] = {"Anti-starfighter", "Anti-Capital Ship", "Anti-Ordinance", "Electronic Counter Measures", NULL };

char *CShip::szWeaponTypes[] = {"Heavy Repeating Blaster", "Blaster Cannon", "Auto Cannon",
                                "Compact Laser Cannon", "Laser Cannon",
                                "Turbolaser", "Heavy Turbolaser",
								"Ion Cannon", "Heavy Ion Cannon",
								"Missile Launcher", "Torpedo Launcher", "Rocket Launcher", NULL };
// Space Weapons Damage Table
int CShip::nMaxDamage[] = {  50,  50,  50,	  // Blasters
                             80, 100,         // Laser Cannons
							500, 500,         // TurboLasers
							 50, 150,         // Ion Cannons
							500, 1500, 3000,  // Ordinance
							 0 };             // NULL

int CShip::nMinDamage[] = {  15,  20,  25,	  // Blasters
                             45,  35,         // Laser Cannons
							150, 200,         // TurboLasers
							  5,  20,         // Ion Cannons
							300, 1250, 2500,  // Ordinance
							 0 };             // NULL


int CShip::nAmmoType[]  = {  OR_TIBANA,  OR_TIBANA,  OR_TIBANA,	// Blasters
                             OR_TIBANA,  OR_TIBANA,				// Laser Cannons
							 OR_TIBANA,  OR_TIBANA,				// TurboLasers
							 OR_TIBANA,  OR_TIBANA,				// Ion Cannons
							 OR_MISSILE, OR_TORPEDO, OR_ROCKET, // Ordinance
							 0 };             // NULL

CWeaponGroup::CWeaponGroup()
{
	m_gsName = "";
	m_Orientation = new CSet;
	m_nType = _ANTISTARFIGHTER;
}

CWeaponGroup::~CWeaponGroup()
{
	m_gsName = "";
	delete m_Orientation;
	m_Orientation = NULL;
	m_nType = 0;
	m_Weapons.clear();
}

// Copy constructor...
CWeaponGroup& CWeaponGroup::operator = (CWeaponGroup& wep)
{
	m_gsName = wep.m_gsName;
	m_nType = wep.m_nType;
	*m_Orientation = *wep.m_Orientation;

	for (IntegerList::iterator it = wep.m_Weapons.begin(); it != wep.m_Weapons.end(); it++)
		m_Weapons.push_back(*it);
	
	return *this;
}

void CWeaponGroup::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pWeapon = Tools.InsertXmlChild(pParent, "WeaponGroup");

	Tools.WriteXml(pWeapon, "name",			m_gsName);
	Tools.WriteXml(pWeapon, "type",			m_nType);
	Tools.WriteXml(pWeapon, "orientation", *m_Orientation);
	for (IntegerList::iterator it = m_Weapons.begin(); it != m_Weapons.end(); it++)
		Tools.WriteXml(pWeapon, "weapon",	*it);

	return;

}

void CWeaponGroup::ReadXml(TiXmlNode* pParent)
{
	// ReadXML will be passed a WeaponGroup node
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "type",			m_nType);
	Tools.ReadXml(pParent, "orientation", *m_Orientation);
	
	TiXmlNode* pWeapon = pParent->FirstChild("weapon");

	while (pWeapon != NULL)
	{
		int nI = 0;
		Tools.ReadXml(pWeapon, "weapon",			nI);	
		m_Weapons.push_back(nI);
		pWeapon = pWeapon->NextSibling("weapon");
	}
	
}

CShip::CShip()
{
	m_gsName = "";
	m_gfFileName = "";
	m_nType = CSpatial::SO_SHIP;
	m_Signature[CSpatial::SI_MASS] = 0;
	m_nClass = 0;
	m_gsType = "";
	m_gsSector = "";
	m_gsDesignation = "";

	for (int i = 0; i < 8; i++)
		m_nPower[i] = 0;
	
	m_Speed = 0;
	m_nCoolant = 0;

	m_Heading = new CCart(0, 0, 0);
	m_dHeading = new CCart(0, 0, 0);

	m_ShipState = new CSet;
	m_Scm = new CSet;

	m_gsExit = "";
	m_gsOMsg = "";
	m_gsCMsg = "";
	m_nKeycode = 0;
	m_nExit = -1;
	m_nExitState = ES_CLOSED;

	// Set all bits for the SCM
	m_Scm->SetBit(SCM_REACTOR);		
	m_Scm->SetBit(SCM_COOLANT);
	m_Scm->SetBit(SCM_OVERHEAT);
	m_Scm->SetBit(SCM_BULKHEAD);
	m_Scm->SetBit(SCM_ION);
	m_Scm->SetBit(SCM_LOADSHED);

//	m_Shape = new CShape;
	m_Shape->m_nType = CShape::ST_RECTANGLE;

	m_Shields = new CShieldState;

	m_Signature[CSpatial::SI_EM] = 0;
	m_Signature[CSpatial::SI_HEAT] = 0;
	m_Signature[CSpatial::SI_ION] = 0;

	m_nSweep = -1;
	m_nTarget = -1;
	m_Target = NULL;
	m_Commander = NULL;

	m_bDelete = false;
}

CShip::CShip(gString name, gString filename)
{
	m_gsName = name;
	m_gfFileName = filename;
	m_nType = CSpatial::SO_SHIP;
	m_Signature[CSpatial::SI_MASS] = 0;

	for (int i = 0; i < 8; i++)
		m_nPower[i] = 0;

	m_nClass = 0;
	m_gsType = "";
	m_gsSector = "";
	m_gsDesignation = "";
	
	m_Speed = 0;
	m_nCoolant = 0;

	m_Heading = new CCart(0, 0, 0);
	m_dHeading = new CCart(0, 0, 0);

	m_ShipState = new CSet;
	m_Scm = new CSet;

	m_gsExit = "";
	m_gsOMsg = "";
	m_gsCMsg = "";
	m_nKeycode = 0;
	m_nExit = -1;
	m_nExitState = ES_CLOSED;

	// Set all bits for the SCM
	m_Scm->SetBit(SCM_REACTOR);		
	m_Scm->SetBit(SCM_COOLANT);
	m_Scm->SetBit(SCM_OVERHEAT);
	m_Scm->SetBit(SCM_BULKHEAD);
	m_Scm->SetBit(SCM_ION);
	m_Scm->SetBit(SCM_LOADSHED);

	m_ShipState->SetBit(_LANDED);

	m_Shields = new CShieldState;

	m_Signature[CSpatial::SI_EM] = 0;
	m_Signature[CSpatial::SI_HEAT] = 0;
	m_Signature[CSpatial::SI_ION] = 0;

	m_nSweep = -1;
	m_nTarget = -1;
	m_Target = NULL;
	m_Commander = NULL;

	m_bDelete = false;
}

// Copy constructor...
CShip& CShip::operator = (CShip& ship)
{
	m_gsName = ship.m_gsType;
	m_gfFileName = ship.m_gfFileName;
	m_gsDesignation = ship.m_gsDesignation;
	m_gsDescription = ship.m_gsDescription;
	m_nType = CSpatial::SO_SHIP;
	m_Signature[CSpatial::SI_MASS] = 0;
	m_nClass = ship.m_nClass;
	m_gsType = ship.m_gsType;
	m_gsSector = ship.m_gsSector;
	m_gsExit = ship.m_gsExit;
	m_gsOMsg = ship.m_gsOMsg;
	m_gsCMsg = ship.m_gsCMsg;
	m_nExit = ship.m_nExit;
		
	m_Speed = ship.m_Speed;
	m_nCoolant = 0;

	m_Heading = new CCart(0, 0, 0);
	m_dHeading = new CCart(0, 0, 0);

	if (ship.m_Shape)
		*m_Shape = *ship.m_Shape;
	else
		m_Shape = new CShape();

	if (!m_ShipState)
		m_ShipState = new CSet;

	if (!m_Scm)
		m_Scm = new CSet;

	*m_ShipState = *ship.m_ShipState;
	*m_Scm = *ship.m_Scm;

	m_Shields = new CShieldState;

	m_Signature[CSpatial::SI_EM] = 0;
	m_Signature[CSpatial::SI_HEAT] = 0;
	m_Signature[CSpatial::SI_ION] = 0;

	// Frames
	for (FrameList::iterator fra = ship.m_Frames.begin(); fra != ship.m_Frames.end(); fra++)
	{
		CFrame* pFrame = new CFrame;
		*pFrame = *(*fra);
		m_Frames.push_back(pFrame);
	}

	// Weapon States
	for (WeaponGroupList::iterator it = ship.m_Weapons.begin(); it != ship.m_Weapons.end(); it++)
	{
		CWeaponGroup* pWeapon = new CWeaponGroup();
		*pWeapon = *(*it);
		m_Weapons.push_back(pWeapon);
	}			

	m_bDelete = false;

	return *this;

}


CShip::~CShip()
{
	m_gsName = "";
	m_gfFileName = "";
	m_nClass = 0;
	m_gsType = "";
	m_gsSector = "";
	m_gsExit = "";
	m_gsOMsg = "";
	m_gsCMsg = "";

	for (int i = 0; i < 8; i++)
		m_nPower[i] = 0;

	for (int i = 0; i < m_Weapons.size(); i++)
		delete m_Weapons.at(i);

	// Free the crew
	for (CrewMap::iterator crew = m_Crew.begin(); crew != m_Crew.end(); crew++)
		for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
		{
			(*it)->m_Ship = NULL;				// Set their ship to none so they return to the
			((*it)->m_Location)->Set(0, 0, 0);	// crew list. We also need to reset their home
			((*it)->m_Home)->Set(0, 0, 0);		// and duty stations to nothing.
		}
			

	for (CrewMap::iterator crew = m_Crew.begin(); crew != m_Crew.end(); crew++)
		(*crew).second.clear();

	m_Crew.clear();


	m_Weapons.clear();

	delete m_ShipState;
	m_ShipState = NULL;

	delete m_Scm;
	m_Scm = NULL;

	m_Area = NULL;

	delete m_Heading;
	m_Heading = NULL;

	delete m_dHeading;
	m_dHeading = NULL;

	m_nCoolant = 0;
	m_nSweep = 0;
	m_nTarget = 0;
	m_nTimer = 0;
	m_nArea = 0;
	m_nMeltdown = 0;
	m_nCollapse = 0;
	m_nSelfdestruct = 0;

	m_Target = NULL;
	m_Commander = NULL;

	delete m_Shields;
	m_Shields = NULL;

}

void CShip::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CSpatial::WriteXml(pParent);

	Tools.WriteXml(pParent, "class",			m_nClass);
	Tools.WriteXml(pParent, "type",				m_gsType);
	Tools.WriteXml(pParent, "designation",		m_gsDesignation);
	Tools.WriteXml(pParent, "area",				m_nArea);

	if (m_gsExit != "")
	{
		Tools.WriteXml(pParent, "exit",			m_gsExit);
		Tools.WriteXml(pParent, "exitomsg",		m_gsOMsg);
		Tools.WriteXml(pParent, "exitcmsg",		m_gsCMsg);
		Tools.WriteXml(pParent, "exitcode",		m_nKeycode);
		Tools.WriteXml(pParent, "exitloc",		m_nExit);
	}

	Tools.WriteXml(pParent, "landed",			m_Land);
	Tools.WriteXml(pParent, "scm",				*m_Scm);
	Tools.WriteXml(pParent, "shipstate",		*m_ShipState);

	m_Location->WriteXml(pParent);

	for (WeaponGroupList::iterator wep = m_Weapons.begin(); wep != m_Weapons.end(); wep++)
		(*wep)->WriteXml(pParent);
	// Output the Frames of the Ship
	for (FrameList::iterator frame = m_Frames.begin(); frame != m_Frames.end(); frame++)
		(*frame)->WriteXml(pParent);

	return;

}

void CShip::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	CSpatial::ReadXml(pParent);

	Tools.ReadXml(pParent, "class",				m_nClass);
	Tools.ReadXml(pParent, "type",				m_gsType);
	Tools.ReadXml(pParent, "designation",		m_gsDesignation);
	Tools.ReadXml(pParent, "area",				m_nArea);

	// We need to assign this Ship to our Area now
	if (m_nArea != 0)
		m_Area = CGameObjects:: Get().GameWorld()->GetArea(m_nArea);

	Tools.ReadXml(pParent, "exit",				m_gsExit);
	if (m_gsExit != "")
	{		
		Tools.ReadXml(pParent, "exitomsg",		m_gsOMsg);
		Tools.ReadXml(pParent, "exitcmsg",		m_gsCMsg);
		Tools.ReadXml(pParent, "exitcode",		m_nKeycode);
		Tools.ReadXml(pParent, "exitloc",		m_nExit);
	}

	Tools.ReadXml(pParent, "landed",			m_Land);

	// We need to add this Ship to the room its currently occupying
	if (m_Land.Area() != 0 || m_Land.Room() != 0 || m_Land.World() != 0)
	{
		CRoom* pRoom = CGameObjects:: Get().GameWorld()->GetArea(m_Land.Area())->GetRoom(m_Land.Room());
		pRoom->AddShip(this);
		m_ShipState->RemoveBit(CShip::_FLYING);
		m_ShipState->SetBit(CShip::_LANDED);
	}
	

	Tools.ReadXml(pParent, "scm",				*m_Scm);
	Tools.ReadXml(pParent, "shipstate",			*m_ShipState);

	m_Location->ReadXml(pParent);

	TiXmlNode* pWeapon = pParent->FirstChild("WeaponGroup");

	while (pWeapon != NULL)
	{
		CWeaponGroup* pWep = new CWeaponGroup;
		pWep->ReadXml(pWeapon);
		m_Weapons.push_back(pWep);
		pWeapon = pWeapon->NextSibling("WeaponGroup");
	}

	// Read in all the Frames, HullCubes, Components and Modules
	TiXmlNode* pFrameNode  = pParent->FirstChild("Frame");

	while (pFrameNode != NULL)
	{
		CFrame* pFrame = new CFrame;
		pFrame->ReadXml(pFrameNode);
		m_Frames.push_back(pFrame);
		pFrameNode = pFrameNode->NextSibling("Frame");
	}

	return;

}


bool CShip::Load(gString gsFilename)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFilename) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Ship");

	ReadXml(pNode);
	return true;
}

bool CShip::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)this->m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Ship");

	ReadXml(pNode);
	return true;
}

bool CShip::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Ship");

	gString gShip;

	gShip.Format("%sShips\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], this->m_gfFileName);

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gShip);

	return true;
}


// Check if a the ship has this frequency open
bool CShip::IsOpen(CFrequency* pOpen)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = this;
	CModule* pMod = NULL;
	bool bFound = false;

	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	// We want to check if this frequency is open on any of Communications Arrays
	for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
	{
		pMod = (*mod);

		// If its not powered up its no use to us
		if (!pMod->Powered())
			continue;

		if (!pMod || (pMod && !pMod->m_Comms))
			continue;
		
		CMComms* pComms = pMod->m_Comms;
		FreqList::iterator freq;

		for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
		{
			CFrequency* pFreq = *freq;
			
			// Is it open on this ship?
			if (pFreq->m_nFrequency == pOpen->m_nFrequency)
				bFound = true;
		}
	}

	pMod = NULL;
	delete (mlMods);
	mlMods = NULL;

	return bFound;

}



// Check if the ship can Decrypt this Channel
bool CShip::CanDecrypt(gString gsProtocol)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = this;
	CModule* pMod = NULL;
	bool bFound = false;

	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	// We want to check if this frequency is open on any of Communications Arrays
	for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
	{
		pMod = (*mod);

		// If its not powered up its no use to us
		if (!pMod->Powered())
			continue;

		if (!pMod || (pMod && !pMod->m_Comms))
			continue;
		
		CMComms* pComms = pMod->m_Comms;
		gStringList::iterator comms;
		
		// Check Encryption protocol selected is valid
		for (comms = pComms->m_Protocols.begin(); comms != pComms->m_Protocols.end(); comms++)
		{
			gString gsProt = *comms;

			if (gsProt == gsProtocol)
			{
				bFound = true;
			}
		}
	}

	// Free memory
	pMod = NULL;
	delete (mlMods);
	mlMods = NULL;
	pGalaxy = NULL;

	return bFound;
}
	
// Is this Ship Jamming this Frequency
bool CShip::IsJammed(CFrequency* pFreq)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = this;
	CModule* pMod = NULL;

	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	// We want to check if this frequency is open on any of Communications Arrays
	for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
	{
		pMod = (*mod);

		// If its not powered up its no use to us
		if (!pMod->Powered())
			continue;

		if (!pMod || (pMod && !pMod->m_Comms))
			continue;
		
		CMComms* pComms = pMod->m_Comms;
		FreqList::iterator freq;
		gStringList::iterator comms;

		if (pComms->m_Jamming.size() > 0)
		{
			freq = pComms->m_Jamming.begin();
			CFrequency* pFrom = *freq;
			CFrequency* pTo = *(freq+1);

			if (pFreq->m_nFrequency >= pFrom->m_nFrequency && pFreq->m_nFrequency <= pTo->m_nFrequency)
				return true;
		}
	}

	// Free memory
	delete (mlMods);
	mlMods = NULL;
	pGalaxy = NULL;
	pMod = NULL;

	return false;
}
	
// Is this Ship Snooping this Frequency
bool CShip::IsSnooped(CFrequency* pFreq)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = this;
	CModule* pMod = NULL;

	ModuleList* mlMods = pShip->Get(CModule::MT_COMMUNICATIONS);

	// We want to check if this frequency is open on any of Communications Arrays
	for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
	{
		pMod = (*mod);

		// If its not powered up its no use to us
		if (!pMod->Powered())
				continue;

		if (!pMod || (pMod && !pMod->m_Comms))
				continue;
		
		CMComms* pComms = pMod->m_Comms;
		FreqList::iterator freq;
		gStringList::iterator comms;

		if (pComms->m_Snooping.size() > 0)
		{
			freq = pComms->m_Snooping.begin();
			CFrequency* pFrom = *freq;
			CFrequency* pTo = *(freq+1);

			if (pFreq->m_nFrequency >= pFrom->m_nFrequency && pFreq->m_nFrequency <= pTo->m_nFrequency)
				return true;
		}
	}

	delete (mlMods);
	mlMods = NULL;
	pGalaxy = NULL;
	pMod = NULL;
	pShip = NULL;

	return false;
}





// Method   : Mass
// Function : Returns the total Mass value for the Ship
//			: The mass of the ship is the combined total
//			: of the mass of each frame of the ship, this
//			: in turn is the mass of each hullcube that is
//			: installed into that frame.
// Arguments: <none>
int CShip::Mass()
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
		nCount += (*frame)->Mass();

	return nCount;
}




// Method   : TopSpeed
// Function : Returns the Top attainable speed of the ship
// Arguments: <bCurr>
//			:  True  = Returns the current top speed, based on number of powered engines
//			:  False = Returns the max top speed
int CShip::TopSpeed(bool bCurr)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_SUBLIGHT)
			{
				CModule* pMod = ((CSublight*)(*comp))->Get(CModule::MT_ION_ENGINE);

				if (bCurr)
				{
					if (pMod && pMod->Powered())
						nCount += pMod->Plus("Propulsion", true);
				}
				else
				{
					if (pMod)
						nCount += pMod->Plus("Propulsion", true);
				}
			}
		}
	}
						

	return nCount;
}




// Method   : Acceleration
// Function : Returns the acceleration of the ship
// Arguments: <bCurr>
//			:  True  = Returns the current acceleration, based on number of powered engines
//			:  False = Returns the max acceleration
int CShip::Acceleration(bool bCurr)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_SUBLIGHT)
			{
				CModule* pMod = ((CSublight*)(*comp))->Get(CModule::MT_ION_ENGINE);

				if (bCurr)
				{
					if (pMod && pMod->Powered())
						nCount += pMod->Plus("Acceleration", true);
				}
				else
				{
					if (pMod)
						nCount += pMod->Plus("Acceleration", true);
				}
			}
		}
	}

	return nCount;
}




// Method   : Maneuver
// Function : Returns the maneuver of the ship, based upon Ion Engines and Maneuvering thrusters
// Arguments: <bCurr>
//			:  True  = Returns the current maneuver
//			:  False = Returns the max maneuver
int CShip::Maneuver(bool bCurr)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_SUBLIGHT)
			{
				CModule* pMod = ((CSublight*)(*comp))->Get(CModule::MT_ION_ENGINE);

				if (bCurr)
				{
					if (pMod && pMod->Powered())
						nCount += pMod->Plus("Maneuver", true);
				}
				else
				{
					if (pMod)
						nCount += pMod->Plus("Maneuver", true);
				}
			}
			else if ((*comp)->m_Type == CComponent::CT_EXTERNAL)
			{
				CModule* pMod = ((CSublight*)(*comp))->Get(CModule::MT_MANUEVERING_THRUSTERS);

				if (bCurr)
				{
					if (pMod && pMod->Powered())
						nCount += pMod->Plus("Maneuver", true);
				}
				else
				{
					if (pMod)
						nCount += pMod->Plus("Maneuver", true);
				}
			}
		}
	}
						

	return nCount;
}






// Method   : Battery
// Function : Returns the battery capacity of the ship
// Arguments: <bCurr>
//			:  True  = Returns the current capacity
//			:  False = Returns the maximum capacity
int CShip::Battery(bool bCurr)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_ENGINEERING)
			{
				CModule* pMod = ((CEngspace*)(*comp))->Get(CModule::MT_BATTERY_BANK);

				if (pMod)
				{
					if (bCurr)
						nCount += pMod->Plus("CurrCapacity", true);
					else
						nCount += pMod->Plus("MaxCapacity", true);
				}
			}
		}
	}
						

	return nCount;
}



// Method   : Reload
// Function : Reload a Weapon Mount, take an ordinance piece from the Magazine
// Arguments: <nType>
//			:  The Ordinance type, taken from CShip::e_Ordinance
bool CShip::Reload(int nType)
{
	bool bReloaded = false;

	if (nType >= CShip::OR_MAX)
		return false;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_MAGAZINE)
			{
				CModule* pMod = NULL;

				switch (nType)
				{
					case CShip::OR_MISSILE: pMod = ((CMagazine*)(*comp))->Get(CModule::MT_MISSILES); break;
					case CShip::OR_TORPEDO: pMod = ((CMagazine*)(*comp))->Get(CModule::MT_TORPEDOES); break;
					case CShip::OR_ROCKET: pMod = ((CMagazine*)(*comp))->Get(CModule::MT_ROCKETS); break;
					case CShip::OR_FLARE: pMod = ((CMagazine*)(*comp))->Get(CModule::MT_FLARES); break;
					case CShip::OR_CHAFF: pMod = ((CMagazine*)(*comp))->Get(CModule::MT_CHAFF); break;
				}

				// See if this Magazine has any stores of what we need
				if (!pMod || pMod->Plus("Quantity", false) <= 0)
					continue;

				// Here we have a Magazine that has at least one more reload for us to use
				pMod->SetPlus("Quantity", pMod->Plus("Quantity", false) - 1);

				// Did we just deplete all stores?
				if (pMod->Plus("Quantity", false) == 0)
				{
					this->Write(CShip::MT_FCS, "[%s] %s stores #101depleted#700.\n\r", (*comp)->m_gsName, szOrdinance[nType]);
				}

				return true;
			}
		}
	}
						

	return bReloaded;
}



// Method   : Capacitors
// Function : Returns the shield capacitation value
// Arguments: <bType>
//			: True  = Current charge
//			: False = Maximum charge
int CShip::Capacitors(bool bType)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_SHIELD)
			{
				CModule* pMod = ((CShield*)(*comp))->Get(CModule::MT_SHIELD_GENERATOR);
				if (pMod)
				{
					// Current or Max is determined by bType
					if (bType)
						nCount += pMod->Plus("CurrCharge", true);
					else
						nCount += pMod->Plus("MaxCharge", true);
				}
			}
				
		}
	}
						
	return nCount;
}


// Returns the Total Shield Capactitation value
int CShip::Shield(bool bType)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_SHIELD)
			{
				CModule* pMod = ((CShield*)(*comp))->Get(CModule::MT_SHIELD_GENERATOR);

				if (pMod)
				{
					// Current or Max is determined by bType
					if (bType)
					{
						// The Shield mount must have an Emitter to be counted in CurrCharge
						if (pMod && pMod->Powered())
							nCount += pMod->Plus("CurrCharge", true);
					}
					else
						nCount += pMod->Plus("MaxCharge", true);
				}
			}
				
		}
	}
						
	return nCount;
}

// Returns the Total Shield Capactitation value for a specific arc
int CShip::Shield(int nArc, bool bType)
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_SHIELD)
			{
				CModule* pMod = ((CShield*)(*comp))->Get(CModule::MT_SHIELD_GENERATOR);

				if (pMod && ((CShield*)(*comp))->m_Orientation->IsSet(nArc))
				{
					// Current or Max is determined by bType
					if (bType)
					{
						// The Shield mount must have an Emitter to be counted in CurrCharge
						if (pMod && pMod->Powered())
						{
							gString gsArc;
							gsArc.Format("Cur%s", CShip::szArc[nArc]);
							nCount += pMod->Plus(gsArc, false);
						}
					}
					else
					{
						gString gsArc;
						gsArc.Format("Max%s", CShip::szArc[nArc]);
						nCount += pMod->Plus(gsArc, false);
					}
				}
			}
				
		}
	}
						
	return nCount;
}



// Returns the Total Crew compliment of a specific type
// Can return either the current number of crew of that type
// or the maximum number
int CShip::Crew(bool bCurr, int nType)
{
	// If nType == -1 then we want all types
	int nCount = 0;

	for (CrewMap::iterator it = this->m_Crew.begin(); it != this->m_Crew.end(); it++)
	for (CrewList::iterator crew = (*it).second.begin(); crew != (*it).second.end(); crew++)
	{
		if ((*crew)->m_nType == nType || nType == -1)
		{
			if (bCurr)
				nCount += (*crew)->m_ncCompliment;
			else
				nCount += (*crew)->m_nmCompliment;	
		}
	}

	return nCount;
}

// Consumes fuel cells from the Bulk Storage
bool CShip::ConsumeFuel(int nAmount)
{
	bool bEmpty = false;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_BULKSTORAGE)
			{
				CModule* pMod = ((CBulk*)(*comp))->Get(CModule::MT_FUEL_CELLS);

				// Did we get one?
				if (pMod)
				{
					// Is this Store empty? If so try another
					if (pMod->Plus("Quantity", false) <= 0)
						continue;

					// Can we consume all the fuel from this Bulk Store?
					if (pMod->Plus("Quantity", false) > nAmount)
					{
						pMod->SetPlus("Quantity", (pMod->Plus("Quantity", false) - nAmount));
						return true;
					}

					// Consume the remaining fuel
					pMod->SetPlus("Quantity", 0);
					this->Write(CShip::MT_HELM, "[%s] Fuel Cells depleted\n\r", (*comp)->m_gsName);
				}
			}
				
		}
	}

	return false;

}

// If the ship is running on batteries then we need to drain some
// Charge from the Batteries
bool CShip::DrainBattery(int nAmount)
{
	bool bEmpty = false;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			if ((*comp)->m_Type == CComponent::CT_ENGINEERING)
			{
				CModule* pMod = ((CEngspace*)(*comp))->Get(CModule::MT_BATTERY_BANK);

				// Did we get one?
				if (pMod)
				{
					// Is this Battery Drained? If so try another
					if (pMod->Plus("CurrCapacity", false) <= 0)
						continue;

					// Are we going to fully deplete the battery?
					if (pMod->Plus("CurrCapacity", false) > nAmount)
					{
						pMod->SetPlus("CurrCapacity", (pMod->Plus("CurrCapacity", false) - nAmount));
						return true;
					}

					// Drain the Battery
					pMod->SetPlus("CurrCapacity", 0);
					this->Write(MT_SYSTEMS, "[%s] Battery drained.\n\r", (*comp)->m_gsName);
				}
			}
				
		}
	}

	return false;

}


// Determines the level of power consumption of the ship.
// There are 6 different types of energy/power usage:
// MIN: The minimum usage
//		>> Life Support
// IDLE: Systems at idle
//		>> Life Support + Comms + Engines
// NORMAL: Normal level of usage
//		>> Life Support + Comms + Engines + Non-Charging shields + Coolant
// COMBAT: Ready to go
//		>> Everything + Shields non-charging
// MAX: All systems
//		>> Everything + Shields charging

int CShip::Energy(int nType)
{
	int nCount = 0;

	switch (nType)
	{
		// Min: Only running Life Support
		case CShip::ET_MIN:
			{
				for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
				{
					for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
					{
						if ((*comp)->m_Type == CComponent::CT_INTERNAL)
						{
							CModule* pMod = ((CInternal*)(*comp))->Get(CModule::MT_LIFE_SUPPORT_UNIT);

							if (pMod)
								nCount += (*pMod->m_Plus.find("PowerLoad")).second;
						}
					}
				}

			}
			break;

		// Idle: Backup Engines only, Life Support, Comms
		case CShip::ET_IDLE:
			{
				nCount += this->Energy(CShip::ET_MIN);

				for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
				{
					for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
					{
						// Comms
						if ((*comp)->m_Type == CComponent::CT_EXTERNAL)
						{
							CModule* pMod = ((CExternal*)(*comp))->Get(CModule::MT_COMMUNICATIONS);

							if (pMod)
								nCount += (*pMod->m_Plus.find("PowerLoad")).second;
						}
							

						// Ion Engine
						if ((*comp)->m_Type == CComponent::CT_SUBLIGHT)
						{
							CModule* pMod = ((CExternal*)(*comp))->Get(CModule::MT_ION_ENGINE);

							if (pMod)
								nCount += (*pMod->m_Plus.find("PowerLoad")).second;
						}
													
					}
				}

			}
			break;

		// Normal: Engines, Comms, Life Support, Idle Shields, Coolant Plant
		case CShip::ET_NORMAL:
			{
				// We want all from Min and then some
				nCount += this->Energy(CShip::ET_IDLE);

				for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
				{
					for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
					{
						// Coolant Plant
						if ((*comp)->m_Type == CComponent::CT_ENGINEERING)
						{
							CModule* pMod = ((CEngspace*)(*comp))->Get(CModule::MT_COOLANT_PLANT);

							if (pMod)
								nCount += (*pMod->m_Plus.find("PowerLoad")).second;
						}

						// Shields
						if ((*comp)->m_Type == CComponent::CT_SHIELD)
						{
							CModule* pMod = ((CShield*)(*comp))->Get(CModule::MT_SHIELD_GENERATOR);

							if (pMod)
								nCount += (*pMod->m_Plus.find("PowerLoad")).second;
						}
					
					}
				}
			}
			break;

		// Combat: Everything online, Shield Capacitors idle
		case CShip::ET_COMBAT:
			{
				nCount += this->Energy(CShip::ET_NORMAL);

				for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
				{
					for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
					{
						// Turrets
						if ((*comp)->m_Type == CComponent::CT_WEAPONMOUNT)
						{
							CModule* pMod = ((CWeapon*)(*comp))->Get(CModule::MT_TURRET_MOUNT);

							if (pMod)
								nCount += (*pMod->m_Plus.find("PowerLoad")).second;
						}
					
					}
				}
			}
			break;

		case CShip::ET_MAX:
			{
				nCount += this->Energy(CShip::ET_COMBAT);
				for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
				{
					for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
					for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
					{
	
						// Shields
						if ((*comp)->m_Type == CComponent::CT_SHIELD)
						{
							CModule* pMod = ((CShield*)(*comp))->Get(CModule::MT_SHIELD_GENERATOR);

							if (pMod)
								nCount += (9 * (*pMod->m_Plus.find("PowerLoad")).second);
						}
							
						

					}
				}
			}
			break;

	}

	return nCount;

}

// Returns the amount of coolant being used or the total coolant capacity
int CShip::Coolant(bool bCurrent)
{
	int nCount = 0;

	// If bCurrent is true then we want the current coolant load
	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		// Working out our Coolant Capacity
		if (!bCurrent)
		{
			if ((*comp)->m_Type == CComponent::CT_ENGINEERING)
			{
				CModule* pMod = ((CEngspace*)(*comp))->Get(CModule::MT_COOLANT_PLANT);

				if (!pMod)
					continue;

				// If we want the Current generation amount we dont want non-powered plants
				if (!pMod->Powered())
					continue;
				
				if (pMod)
					nCount += pMod->Plus("CoolantRate", true);
			}

			if ((*comp)->m_Type == CComponent::CT_EXTERNAL)
			{
				CModule* pMod = ((CExternal*)(*comp))->Get(CModule::MT_HEATSINK);

				if (pMod)
					nCount += pMod->Plus("CoolantRate", true);
			}
		}
		else
		{
			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				CModule* pMod = ((*mod).second);
				
				nCount += pMod->Plus("CoolantLoad", false);
			}
		}
	}

	return nCount;		

}

// Used to determine the current power usage
int CShip::CurrPower()
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
		{
			// We want all Modules that have an Energy Load and are Powered
			if (((*mod).second)->Plus("Powered", false))
				nCount += ((*mod).second)->Plus("PowerLoad", false);
		}
	}
	
	return nCount;
}

// Used to determine the maximum power capacity
int CShip::MaxPower()
{
	int nCount = 0;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		if ((*comp)->m_Type == CComponent::CT_ENGINEERING)
		{
			CModule* pMod = ((CEngspace*)(*comp))->Get(CModule::MT_REACTOR_PLANT);

			if (pMod)
			{
				nCount += pMod->Plus("PowerCapacity", true);
			}

		}
	}

	return nCount;
}

// Free power capacity
int CShip::FreePower()
{
	return MaxPower()-CurrPower();
}


void CShip::Destroy()
{
//	this->Write(MT_CRITICAL, "The ship is like dead man.\n\r");
}

// Method     :: Destroyed
// Class	  :: Ship
// Parameters :: <none>
// Arguments  :: <none>
// Return     :: Yes if destroyed, No otherwise
// Function   :: Works out if the entire ship has been destroyed
//            :: or not by looking for undamaged HullCubes
// Written    :: 12/02/2006 {OWV}
bool CShip::Destroyed()
{
	bool bDestroyed = true;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	{
		if ((*hull)->m_nCKeel > 0)
			bDestroyed = false;
	}

	return bDestroyed;
	
}

// Method     :: DamageShield
// Class	  :: Ship
// Parameters :: <Damage Amount, Arc Damage received in>
// Arguments  :: <none>
// Return     :: Void
// Function   :: Gives damage to Capacitors and trips Matrix Ciruits if required
// Written    :: 23/02/2006 {OWV}
bool CShip::DamageShield(int nAmount, int nArc)
{
	// We record the remainder so we can pass on shield damage if one capacitor
	// can't consume it all.
	int nRemainder = nAmount;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		// Did we consume all the damage last round?
		if (nRemainder <= 0)
			return true;

		// Work through each Shield mount
		if ((*comp)->m_Type == CComponent::CT_SHIELD)
		{
			CShield* pShield = (CShield*)(*comp);

			// This one is orientated in the arc we have to damage
			if (pShield->m_Orientation && pShield->m_Orientation->IsSet(nArc))
			{
				// Has it got any charge?
				CModule* pMod = pShield->Get(CModule::MT_SHIELD_GENERATOR);

				// We need a Capacitor that has charge to damage in the Arc we want
				gString gsArc;
				gsArc.Format("Cur%s", CShip::szArc[nArc]);

				if (pMod && pMod->Plus(gsArc, false) > 0)
				{
					// Ok so it has charge, will this consume all the damage?
					int nCharge = pMod->Plus(gsArc, false);
					
					// [1] It consumes all damage
					if (nCharge > nRemainder)
					{
						pMod->SetPlus(gsArc, (nCharge - nRemainder));									// Arc specific
						pMod->SetPlus("CurrCharge", (pMod->Plus("CurrCharge", false) - nRemainder) );	// From Current also

						nRemainder = 0;
						return true;
					}
					// [2] It takes damage and runs out of charge
					else if (nCharge <= nRemainder)
					{
						nRemainder -= nCharge;	// Subtract the damage this did consume

						// Adjust the CurrCharge
						pMod->SetPlus("CurrCharge", (pMod->Plus("CurrCharge", false) - nCharge) );
						// Remove all Charge from specific Arc
						pMod->SetPlus(gsArc, 0);

						// Was this the last arc in the Emitter to be knocked out? If so we 
						// have to shutdown the emitter
						if (pMod->Plus("CurrCharge", false) <= 0)
						{
							// Trip the Emitter
							if (pMod)
							{
								pMod->SetPlus("Powered", false);
								this->Write(MT_SYSTEMS, "[Defense] %s's Emitter overloaded due to damage.\n\r", (*comp)->m_gsName, pMod->m_gsName);
							}
						}
					}

				}

			}

		}

	}


	return true;
}

// Method     :: Damage
// Class	  :: Ship
// Parameters :: <Damage Amount, Arc Damage received in>
// Arguments  :: <none>
// Return     :: Void
// Function   :: Determines the damage done to the ship,
//			  :: shields absorb damage, then armour before
//			  :: the keel does
// Written    :: 03/02/2006 {OWV}
void CShip::Damage(int nAmount, int nArc)
{
	// -1 Means hit in all Arcs
	int nRemainder = 0; // Damage is carried over
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();


	// [1] >> First can the Shields absorb the damage
	if (this->Shield(nArc, true) <= nAmount)
	{
		// Shields cant absorb all the damage
		if (this->Shield(nArc, true) > 0)
		{
			// Knock the shield bank out
			DamageShield(nAmount, nArc);	
			// Let the Ship know the damage
			this->Write(MT_SYSTEMS, "[Defense] %s Shield bank down!\n\r", CShip::szArc[nArc]);
		}

		// Is there any damage to continue?		
		nRemainder = nAmount - this->Shield(nArc, true);
	}
	else
	{
		// Shields absorbed the damage reduce the Shield strength
		DamageShield(nAmount, nArc);
		// Inform the ship
		this->Write(MT_SYSTEMS, "[Defense] %s Shield absorbed %d damage!\n\r", CShip::szArc[nArc], nAmount);
		return;
	}

	// [2] >> Now we have to pick a HullCube to damage, we need to compile a list of all HullCubes
	//		  the can be hit from this arc
	HullList* hlTargets=  new HullList();

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	{
		if ((*hull)->m_Cover->IsSet(nArc) && nArc != -1)
			hlTargets->push_back(*hull);
		else
			hlTargets->push_back(*hull);
	}

	if (hlTargets->size() <= 0)
		return;

	// The damage system works by damaging a single, random HullCube which can be hit in the defined arc
	// if the hullcube gets completely destroyed by the attack then we apply any remaining damage to another
	// random HullCube
	while(nRemainder > 0)
	{
		CRandom* pRandom = CGameObjects::Get().Rand();

		// Random index for a HullCube
		int nIndex = pRandom->NumberRange(0, (hlTargets->size() - 1));

		int nDamage = hlTargets->at(nIndex)->Damage(nRemainder, false, this);

		if (nDamage >= 0)
		{
			this->Write(MT_SYSTEMS, "#100[#101Damage Report#100]#700 %s has taken %d damage!\n\r", hlTargets->at(nIndex)->m_gsName, (nAmount-nDamage));
		}

		// We dont want to damage the same HullCube twice so we now remove it from the list
		hlTargets->erase(hlTargets->begin()+nIndex);

		// It absorbed all the damage
		if (nDamage == 0)
			break;

		// We need a check here to makesure there are still hullcubes left to damage
		// if not then we need to break the loop
		if (hlTargets->size() <= 0)
			break;

		// We still have more damage to deal out to the next HullCube
		nRemainder = nDamage;
	}

	delete (hlTargets);
	hlTargets = NULL;
}

// Method     :: Fire
// Class	  :: Ship
// Parameters :: <Weapon List, Target>
// Arguments  :: <none>
// Return     :: Void
// Function   :: Works out the weapons that hit the target
//			  :: and deals out damage to the target
// Written    :: 25/02/2006 {OWV}
int CShip::Fire(ModuleList mlWeapons, CSpatial * pTarget, int nArc)
{
	int nAttack;
	int nDefenseDC;
	int nNumHit = 0;
	int nNumDodged = 0;
	int nDamage = 0;
	int nWeaponType = 0;
	int nCount = 0;
	CRandom* pRandom = CGameObjects::Get().Rand();

	// Insanity check
	if (mlWeapons.size() <= 0 || !pTarget)
		return 0;

	/////////////////////////////////////////////////////////////////////////
	// The Fire function determines hits:                                  //
	// E-Pac(s) Roll (Can the player hit the target)                       //
	//   >> ATTACK vs DC DEFENSE (+DODGE)                                  //
	/////////////////////////////////////////////////////////////////////////

	// E-Pac(s) System 
	//  >> Attack v.s Defence (Dodge)

	// [1] >> Attack
	// The Attack value is worked out using: 
	//    (a) For Crewed vessels:
	//       Gunnery Captain - Dice[Charisma + Gunnery Command] + Morale (Every success past 4 gives +1)
	//       Gunnery Team - Dice[Intelligence + Ship gunnery] + FCS + Above roll bonuses

	// TEMP
	// For sunday all Players have Attr(8) + Skill(8) = 16
	// All gun crews have Attr(6) + Skill(5) = 11

	// We determine this for each Weapon in the group
	for (ModuleList::iterator mod = mlWeapons.begin(); mod!= mlWeapons.end(); mod++)
	{
		CModule* pWeapon = (*mod);

		// We want to work out the type of weapons being used to give an
		// accurate message to the player
		if (nCount == 0)
			nWeaponType = pWeapon->Plus("WeaponType", false);
		else
			if (nWeaponType != pWeapon->Plus("WeaponType", false))
				nWeaponType = -1;

		nCount++;
		
		// Working out the Attack depends on the Ship type
		if (this->m_nClass > ST_FIGHTER)
		{
			// Commanders roll
			int nCommander = pRandom->NumberRange(1, 16);

			// D(16) == 0 is an automatic failure
			if (nCommander == 0)
				continue;

			int nBonus = 0;

			// Add bonus in
			if (nCommander >= 10)
				nBonus = nCommander - 9;

			// Attack roll is now Gunnery crew + Command bonus
			nAttack = pRandom->NumberRange(1, 15) + nBonus;
		}
		else
		{
			// Starfighters are the pilots of their own destiny
			// hence they roll everything themselves

			// Dual/Quad Weapons roll 2/3 times and take the highest

			if (pWeapon->Plus("TurretType", false) <= TT_SINGLE)
				nAttack = pRandom->NumberRange(1, 16);
			else if (pWeapon->Plus("TurretType", false) == TT_DUAL)
			{ // Dual rolls twice
				int n1 = pRandom->NumberRange(1, 16);
				int n2 = pRandom->NumberRange(1, 16);
				nAttack = n1 > n2 ? n2 : n1;
			}
			else if (pWeapon->Plus("TurretType", false) == TT_DUAL)
			{ // Quad rolls three times
				int n1 = pRandom->NumberRange(1, 16);
				int n2 = pRandom->NumberRange(1, 16);
				int n3 = pRandom->NumberRange(1, 16);
				
				if (n1 > n2 && n1 > n3)
					nAttack = n1;
				if (n2 > n1 && n2 > n3)
					nAttack = n2;
				if (n3 > n1 && n3 > n2)
					nAttack = n3;
			}

			// Automatic failure
			if (nAttack == 0)
				continue;
		} // We have the Attack roll now work out the Defense
				
		// [2] >> Defence
		// The Defence value is: DC == D(6) + DISTANCE + SPEED + SIZE

		//////////////////////////[SIZE TABLE]///////////////////////////////
		//[Mod]       [SIZE]      | [Example:]                             //
		// 15     10 000+  Meters | Super Star Destroyer                   // 
		// 14      5 000   Meters |                                        //
		// 13      1 000   Meters | Imperial Star Destroyer                //
		// 12        500   Meters | Assault frigate                        //
		// 11        250   Meters | Nebulon-B escort frigate               //
		// 10        100   Meters | Corellian corvette, Bulk freighter     //
		//  9         51   Meters | Medium-size freighter                  //
		//  8         21   Meters | Light freighter                        //
		//  7         11   Meters | X-Wing Starfighter                     //
		//  6          6   Meters | TIE fighter, rancor                    // 
		//  5          3   Meters | Missile, escape pod, airspeeder, wampa //
		//  4          1.2 Meters | Human                                  //
		//  3          0.6 Meters | Ewok, R2 Series astromech droid        //
		//  2          0.3 Meters | Ysalamiri, Cat                         //
		//  1          0.1 Meters | Marksman-H training remote             //
		//  0      Sub-0.1 Meters | Stingfly                               //
		/////////////////////////////////////////////////////////////////////

		// To calculate size we work out our size and then their size and subtract
		// our size - target size
		int nSize = 0;
		int nTempSize = 0;
		int nTSize = 0;
		int nLength = this->m_Shape->Length() >= this->m_Shape->Width() ? this->m_Shape->Length() : this->m_Shape->Width();
		int nTLength = 0;
				
		if (pTarget->m_nType == CSpatial::SO_SHIP)
			nTLength = this->m_Shape->Length() >= pTarget->m_Shape->Width() ? pTarget->m_Shape->Length() : pTarget->m_Shape->Width();
		else 
			nTLength = pTarget->m_Shape->m_nRadius;

				
		// We need the size for both our ship and the target
		for (int i = 0; i < 1; i++)
		{
			// Second round we use the Target's size
			if (i == 1)
				nLength = nTLength;

			if (nLength >= 1000)
				nTempSize = 15;
			else if (nLength < 10000 && nLength >= 5000)
				nTempSize = 14;
			else if (nLength < 5000 && nLength >= 1000)
				nTempSize = 13;
			else if (nLength < 1000 && nLength >= 500)
				nTempSize = 12;
			else if (nLength < 500 && nLength >= 250)
				nTempSize = 11;
			else if (nLength < 250 && nLength >= 100)
				nTempSize = 10;
			else if (nLength < 100 && nLength >= 51)
				nTempSize = 9;
			else if (nLength < 51 && nLength >= 21)
				nTempSize = 8;
			else if (nLength < 21 && nLength >= 11)
				nTempSize = 7;
			else if (nLength < 11 && nLength >= 6)
				nTempSize = 6;
			else if (nLength < 6 && nLength >= 3)
				nTempSize = 5;
			else if (nLength < 3 && nLength >= 1)
				nTempSize = 4;

			// The second time round we set the Target size
			if (i == 1)
				nTSize = nTempSize;
			else // First time we set our size
				nSize = nTempSize;
		}

		// Point defense weapons on large ships (>10) use the standard size of 10
		if (pWeapon->Plus("WeaponType", false) < CShip::WT_TURBOLASER && nSize > 10)
			nSize = 10;
				
		// Size = Firer Size - Target Size
		nSize = nSize - nTSize;

		//////[SPEED TABLE]//////
		//	0-10      -1       //
		//	11-20      0       //
		//	21-50      1       //
		//	51-75      2       //
		//	76-100     3       //
		//	101-125    4       //
		//	126-150    5       //
		//	151-200    6       //
		//	201-250    7       //
		//	251-300    8       //
		//	300+       9       //
		/////////////////////////
		int nSpeed = 0;

		if (pTarget->m_nType == CSpatial::SO_SHIP)
		{
			int nSpd = ((CShip*)pTarget)->m_Speed;
			
			if (nSpd <= 10)
				nSpeed = -1;
			else if (nSpd >= 11 && nSpd < 20)
				nSpeed = 0;
			else if (nSpd >= 21 && nSpd < 50)
				nSpeed = 1;
			else if (nSpd >= 51 && nSpd < 75)
				nSpeed = 2;
			else if (nSpd >= 76 && nSpd < 100)
				nSpeed = 3;
			else if (nSpd >= 101 && nSpd < 125)
				nSpeed = 4;
			else if (nSpd >= 126 && nSpd < 150)
				nSpeed = 5;
			else if (nSpd >= 151 && nSpd < 200)
				nSpeed = 6;
			else if (nSpd >= 201 && nSpd < 250)
				nSpeed = 7;
			else if (nSpd >= 251 && nSpd < 300)
				nSpeed = 8;
			else if (nSpd >= 300)
				nSpeed = 9;
		}
		else
		{
			nSpeed = -1;
		}		

		//////////[DISTANCE TABLE]////////
		//	6000 >             12       //
		//	5501 - 6000        10       //
		//	5001 - 5500         9       //
		//	4501 - 5000         8       //
		//  4000 - 4500         7       //
		//	3500 - 4000         6       //
		//	3000 - 3500         5       //
		//	2500 - 3000         4       //
		//	2000 - 2500         3       //
		//	1500 - 2000         2       //
		//	1000 - 1500         1       //
		//	900 - 1000          0       //
		//	800 - 900          -1       //
		//	700 - 800          -2       //
		//	600 - 700          -3       //
		//	500 - 600          -4       //
		//	0 - 500            -5       //
		//////////////////////////////////

		int nDistance;
		int nDist = this->Distance(pTarget);

		if (nDist >= 6000)
			nDistance = 12;
		else if (nDist >= 5501 && nDist < 6000)
			nDistance = 9;
		else if (nDist >= 5001 && nDist < 5500)
			nDistance = 8;
		else if (nDist >= 4501 && nDist < 5000)
			nDistance = 7;
		else if (nDist >= 4001 && nDist < 4500)
			nDistance = 6;
		else if (nDist >= 3501 && nDist < 4000)
			nDistance = 5;
		else if (nDist >= 3001 && nDist < 3500)
			nDistance = 4;
		else if (nDist >= 2501 && nDist < 3000)
			nDistance = 3;
		else if (nDist >= 2001 && nDist < 2500)
			nDistance = 2;
		else if (nDist >= 1501 && nDist < 2000)
			nDistance = 1;
		else if (nDist >= 1001 && nDist < 1500)
			nDistance = 0;
		else if (nDist >= 901 && nDist < 1000)
			nDistance = -1;
		else if (nDist >= 801 && nDist < 900)
			nDistance = -2;
		else if (nDist >= 701 && nDist < 800)
			nDistance = -3;
		else if (nDist >= 601 && nDist < 700)
			nDistance = -4;
		else if (nDist >= 501 && nDist < 600)
			nDistance = -5;
		else if (nDist < 500)
			nDistance = -6;

		// We have now got all three of our values so we can compute:
		// >> D(6) + SIZE + SPEED + DISTANCE

		nDefenseDC = pRandom->D6() + nSize + nSpeed + nDistance;

		// Our static Defense should never go below DC 3
		if (nDefenseDC < 3)
			nDefenseDC = 3;

		// We should Hit
		if (nAttack >= nDefenseDC)
		{ // We need to give Starfighters and Light freighters their dodge roll
			bool bDodge = false;

			if (pTarget->m_nType == CSpatial::SO_SHIP && ((CShip*)pTarget)->m_nClass < CShip::ST_SMALLSHIP)
			{
				// The Dodge roll is: DC == 20 + D6 - (DISTANCE + Self_SPEED)
				int nDodge = 10 + pRandom->D6() - (nDistance + nSpeed);
				int nPilot = pRandom->NumberRange(1, 16);

				if (nPilot >= nDodge)
					bDodge = true;
			}
			
			if (!bDodge)
			{
				nNumHit++;
				int nDam = pRandom->NumberRange(CShip::nMinDamage[pWeapon->Plus("WeaponType", false)], CShip::nMaxDamage[pWeapon->Plus("WeaponType", false)]);
				nDamage+= nDam;
			}
			else
			{
				nNumDodged++;
			}

		}

	}

	// Need to pass the messages
	gString gsMsg;
	this->Write(MT_CRITICAL, "Number Hits [%d] Number Dodges [%d] Damaged caused [%d]\n\r", nNumHit, nNumDodged, nDamage);

	gString gsHitVerb = "a volley of";

	// We want a unique verb depending on the number of weapons fired
	if (nNumHit == 1)
		gsHitVerb = "a single burst of";
	if (nNumHit == 2)
		gsHitVerb = "a dual shot of";
	if (nNumHit == 3)
		gsHitVerb = "a trio of";
	if (nNumHit == 4)
		gsHitVerb = "a quad shot of";
	if (nNumHit >= 10 && nNumHit < 20)
		gsHitVerb = "a volley of";
	if (nNumHit >= 20 && nNumHit < 50)
		gsHitVerb = "a barrade of";
	if (nNumHit >= 50)
		gsHitVerb = "a fusilade of";

	// Target gets a hit/miss message
	if (nNumHit > 0)
	{
		pTarget->Write(MT_CRITICAL, "#300You are hit by %s fire from %s! [%d]#700\n\r", nWeaponType == -1 ? "mixed laser" : CShip::szWeaponTypes[nWeaponType], m_gsName, nNumHit);
		pTarget->NotifySpace("%s opens fire on %s, hitting it with %s %s fire! [%d]\n\r", m_gsName, pTarget->m_gsName, gsHitVerb, nWeaponType == -1 ? "mixed laser" : CShip::szWeaponTypes[nWeaponType], nNumHit);

		// We have given them the messages, now we need to apply damage
		if (pTarget->m_nType == CSpatial::SO_SHIP)
			pTarget->Damage(nDamage, nArc);
		
		//else
		//	pTarget->Destroy();
	}
	else
	{
		pTarget->Write(MT_CRITICAL, "#300%s opens fire with %s %s fire and misses!#700\n\r", m_gsName, gsHitVerb, nWeaponType == -1 ? "Mixed laser" : CShip::szWeaponTypes[nWeaponType]);
		pTarget->NotifySpace("%s opens fire on %s, narrowly missing it with %s %s fire!\n\r", m_gsName, pTarget->m_gsName, gsHitVerb, nWeaponType == -1 ? "mixed laser" : CShip::szWeaponTypes[nWeaponType]);
	}

	if (nNumDodged > 0)
	{
		gString gsMsg;
		switch (pRandom->NumberRange(1, 6))
		{
			// Snap roll
			case 1: pTarget->Write(MT_CRITICAL, "#600You pitch up rapidly and apply full right aileron and rudder as you snap roll to avoid laser fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s performs a rapid, snap, roll in an attempt to dodge fire from %s.", pTarget->m_gsName, m_gsName);
				break;
			// Juke
			case 2: pTarget->Write(MT_CRITICAL, "#600You begin to vary your elevator inputs randomly as you juke manically to avoid fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s jukes about wildly trying to dodge fire from %s.", pTarget->m_gsName, m_gsName);
				break;
			// Jink
			case 3: pTarget->Write(MT_CRITICAL, "#600You weave the nose about in a random jinking sequence to throw off laser fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s jinks randomly, attempting to throw off fire from %s.", pTarget->m_gsName, m_gsName);
				break;
			// Wotan weave
			case 4: pTarget->Write(MT_CRITICAL, "#600You pitch up and begin to roll about a varying centre, corkscrewing in the trademark Wotan Weave style, to avoid fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s corkscrews in the trade mark Wotan Weave style, attempting to avoid fire from %s.", pTarget->m_gsName, m_gsName);
				break;
			// Break
			case 5: pTarget->Write(MT_CRITICAL, "#600You immediately apply full aileron followed by pulling hard back on the stick breaking quickly to the left, to avoid fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s rapidly breaks off into a max rate turn trying to avoid fire from %s.", pTarget->m_gsName, m_gsName);
				break;
			// Immelmann
			case 6: pTarget->Write(MT_CRITICAL, "#600You pitch up into the first half of a loop and quickly roll of the top into an Immelmann, attempting to throw of fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s begins a loop but quickly rolls off the top into an Immelmann, trying to avoid fire from %s.", pTarget->m_gsName, m_gsName);
				break;
			// Snap roll
			default: pTarget->Write(MT_CRITICAL, "#600You pitch up rapidly and apply full right aileron and rudder as you snap roll to avoid laser fire from %s.#700\n\r", m_gsName);
					pTarget->NotifySpace("%s performs a rapid, snap, roll in an attempt to dodge fire from %s.", pTarget->m_gsName, m_gsName);
				break;
		}
	}





	return 0;
}


ModuleList* CShip::Get(int nModType)
{
	ModuleList* mlModules = new ModuleList;
	CModule* pMod = NULL;


	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{	
			pMod = (*comp)->Get(nModType);

			if (pMod)
				mlModules->push_back(pMod);
		}
	}

	return mlModules;
}

CModule* CShip::GetModule(gString gsName, gString gsComp)
{
	CModule* pMod = NULL;

	gsName.MakeUpper();

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{	
			if ((*comp)->m_gsName != gsComp)
				continue;

			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				(*mod).second->m_gsName.MakeUpper();
				if (((*mod).second)->m_gsName == gsName)
				{
					pMod = ((*mod).second);
					break;
				}
				(*mod).second->m_gsName.MakeProper();
			}

		}
	}

	return pMod;
}

CModule* CShip::GetModule(int nIndex)
{
	CModule* pMod = NULL;

	int nCount = 0;
	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{
			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				nCount++;
				if (nCount == nIndex)
				{
					pMod = (*mod).second;
					return pMod;
				}
				
			}
		}
	}

	return pMod;
}

CComponent* CShip::GetComponent(gString gsName)
{
	CComponent* pComp = NULL;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{	
			if ((*comp)->m_gsName == gsName)
			{
				pComp = (*comp);
				break;
			}
		}
	}

	return pComp;
}


CComponent* CShip::GetComponent(int nIndex)
{
	CComponent* pComp = NULL;

	int nCount = 0;
	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{	
			if (nCount == nIndex)
			{
				pComp = (*comp);
				return pComp;
			}
			nCount++;
		}
	}

	return pComp;
}

CComponent* CShip::GetModComp(gString gsName)
{
	CComponent* pComp = NULL;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{	
			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				if (((*mod).second)->m_gsName == gsName)
				{
					pComp = (*comp);
					break;
				}
			}

		}
	}

	return pComp;
}

CComponent* CShip::GetModComp(CModule* pMod)
{
	CComponent* pComp = NULL;

	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{	
			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				if (((*mod).second) == pMod)
				{
					pComp = (*comp);
					break;
				}
			}

		}
	}

	return pComp;
}

bool CShip::IsType(int nType, CModule *pMod)
{
	switch (pMod->m_nType)
	{
		// Drives: Ion Engines, Steering Engines, Hyperdrives, Maneuvering Thrusters
		case CModule::MT_ION_ENGINE:
		case CModule::MT_HYPERDRIVE_MAIN:
		case CModule::MT_HYPERDRIVE_BACKUP:
		case CModule::MT_MANUEVERING_THRUSTERS:
			{
				if (nType == PT_DRIVE)
					return true;
				else
					return false;
			}
			break;

		// Shield Emitters
		case CModule::MT_SHIELD_GENERATOR:
			{
				if (nType == PT_SHIELD || nType == PT_CAPACITOR)
					return true;
				else
					return false;
			}
			break;

		// Systems: Control Points, Life Support, Comms, Astrogation, Nav, Sensors, Tractor Beams
	    //          Cooling, Reactors, Holonet
		case CModule::MT_REACTOR_PLANT:
		case CModule::MT_COOLANT_PLANT:
		case CModule::MT_RADOME:
		case CModule::MT_COMMUNICATIONS:
		case CModule::MT_ASTROGATION_COMPUTER:
		case CModule::MT_LIFE_SUPPORT_UNIT:
		case CModule::MT_HOLONET_TRANSCEIVER:
		case CModule::MT_TRACTOR_BEAM_PROJECTOR:
		case CModule::MT_HELM:
		case CModule::MT_PILOT_CONSOLE:
		case CModule::MT_SECONDARY_CONTROL_CONSOLE:
		case CModule::MT_COPILOT_CONSOLE:
		case CModule::MT_NAV_CONSOLE:
		case CModule::MT_GUNNERY_CONSOLE:
		case CModule::MT_WEAPONS_CONSOLE:
			{
				if (nType == PT_SYSTEMS)
					return true;
				else
					return false;
			}
			break;

		case CModule::MT_REPULSOR_COILS:
			{
				if (nType == PT_REPULSOR)
					return true;
				else
					return false;
			}
			break;

		case CModule::MT_GRAVITY_WELL_PROJECTORS:
			{
				if (nType == PT_GRAVWELL)
					return true;
				else
					return false;
			}
			break;

		case CModule::MT_FLARE_AND_CHAFF_DISPENSOR:
			{
				if (nType == PT_ECM)
					return true;
				else
					return false;
			}
			break;

		case CModule::MT_TURRET_MOUNT:
			{
				if (nType == PT_WEAPON)
					return true;
				else
					return false;
			}
			break;

		case CModule::MT_BATTERY_BANK:
			{
				if (nType == PT_BATTERY)
					return true;
				else
					return false;
			}
			break;

		default: return false;
			break;
	}

	return false;
}


void CShip::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	// [ 1] - Brown out: If the ship is not producing as much Power as its using, drain its batteries
	/* POD for Ken
	if (this->Generation(CShip::GT_POWER, true) < this->Energy(CShip::ET_CURRENT))
	{
		// [ a] - We need to carry out Load Shedding here
		if (this->m_Scm->IsSet(SCM_LOADSHED))
		{
			// Carry out load shedding
		}
		
		this->DrainBattery(abs(this->Generation(CShip::GT_POWER, true) - this->Energy(CShip::ET_CURRENT)));
	} */
	
	// [ 2] - Iterate through ALL Components and Update them
	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	{
		for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
		for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
		{		
			// Update Each component
			(*comp)->Update();
			
			//  [ 3a] - Now Iterator through Modules and Destroy any at Integrity 0
			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				// Is there anything waiting to be blown up?
				if (((*mod).second)->m_ncDurability == 0 )
				{
					gString gsMsg;

					// Special cases first:
					switch (((*mod).second)->m_nType)
					{
						// Reactor Plants cause extra damage to their Hull Cube
						case CModule::MT_REACTOR_PLANT:
							{
								if (((*mod).second)->Powered())
								{	
									CRandom* pRandom = CGameObjects::Get().Rand();
									this->m_nMeltdown = pRandom->NumberRange(4, 10); // Set the Timer
									this->Write(MT_CRITICAL, "#100[#101ALARM#100]#700 %s has entered meltdown! Evacuate! Ship destruction imminent!\n\r", ((*mod).second)->m_gsName);									
								}
								else
								{
									this->Write(MT_CRITICAL, "[%s] %s has been destroyed, Shutdown prevented meltdown\n\r", (*comp)->m_gsName, ((*mod).second)->m_gsName);
								}

								// Destroy the Module and power it down
								((*mod).second)->m_ncDurability = -1;
								((*mod).second)->SetPlus("Powered", 0);
							}
							break;

						// Coolant Plants like Reactors cause extra damage to their Hull Cube
						case CModule::MT_COOLANT_PLANT:
							{
								if (((*mod).second)->Powered())
								{
									CRandom* pRandom = CGameObjects::Get().Rand();
									(*hull)->Damage(pRandom->NumberRange(20, 100), true, this); // #TODO# Determine realisitic damage
									pRandom = NULL; // Free the memory

									this->Write(MT_CRITICAL, "#100[#101ALARM#100]#700 %s has been destroyed! Environment in %s lethal.\n\r", ((*mod).second)->m_gsName, (*comp)->m_gsName);
								}
								else
								{
									this->Write(MT_SYSTEMS, "[%s] %s has been destroyed, Shutdown prevented Coolant leak.\n\r", (*comp)->m_gsName, ((*mod).second)->m_gsName);														
								}

								((*mod).second)->m_ncDurability = -1;
								((*mod).second)->SetPlus("Powered", 0);	// Some Modules need to be shutdown
						
							}
							break;
						
						case CModule::MT_REPULSOR_COILS:
							{
								gsMsg.Format("[%s] %s has been destroyed!\n\r", (*comp)->m_gsName, ((*mod).second)->m_gsName);
								if (this->m_ShipState->IsSet(_REPULSOR))
								{
									if (this->m_ShipState->IsSet(_FLYING))
									{
										this->m_ShipState->RemoveBit(_REPULSOR);
									}
									else
									{
										this->m_ShipState->RemoveBit(_REPULSOR);
										this->m_ShipState->SetBit(_LANDED);
										gsMsg += "The Ship crashes back down to the ground!";
									}
								
								}

								this->Write(MT_SYSTEMS, "%s\n\r", gsMsg);
								((*mod).second)->m_ncDurability = -1;
								((*mod).second)->SetPlus("Powered", 0);	// Some Modules need to be shutdown
							}
							break;

						case CModule::MT_MUNITION_ELEVATOR:
							{
								CRandom* pRandom = CGameObjects::Get().Rand();
								gsMsg.Format("[%s] %s has been destroyed\n\r", (*comp)->m_gsName, ((*mod).second)->m_gsName);
								if (pRandom->NumberRange(1, 100) <= 5) // 5% Chance of blowing the magazine
								{
									gString gsMag;

									for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
										for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
											for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
											{
												// Detonate the ammunition!
												if ((*comp)->m_Type == CComponent::CT_MAGAZINE)
												{
													gsMag = (*comp)->m_gsName;
													for (ModuleMap::iterator mod2 = (*comp)->m_Modules.begin(); mod2 != (*comp)->m_Modules.end(); mod2++)
													{
														((*mod2).second)->SetPlus("Quantity", 0);
														((*mod2).second)->m_ncDurability = 0;
													}
												}
											}
									
									
									gsMsg += "The resulting explosion has detonated " + gsMag + "!";


								}							
								
								pRandom = NULL;
								this->Write(MT_SYSTEMS, "%s\n\r", gsMsg);
								((*mod).second)->m_ncDurability = -1;
								
							}
							break;

						// Dangerous Ordinance!
						case CModule::MT_MISSILES:
						case CModule::MT_TORPEDOES:
						case CModule::MT_ROCKETS:
							{								
								CRandom* pRandom = CGameObjects::Get().Rand();
								this->Write(MT_SYSTEMS, "#100[#101ALARM#100]#700 [%s] %s ammunition detonated!\n\r", (*comp)->m_gsName, ((*mod).second)->m_gsName);
								((*mod).second)->SetPlus("Quantity", 0); // Remove all ammunition

								this->Damage(pRandom->NumberRange(1000,30000), -1); // #TODO# Make this realistic

								((*mod).second)->m_ncDurability = -1;			
								
								this->NotifySpace("Brilliantly bright light explodes from %s's %s. The light is quickly intensified by a massive explosion as the %s's ammunition detonates!", this->m_gsName, (*hull)->m_gsName, (*comp)->m_gsName);
							}
							break;

						// All other modules just take themselves out
						// Sublight Bearings do extra damage, this is handled by its Update function.
						default:
							{					
								this->Write(MT_SYSTEMS, "[%s] %s has been destroyed!\n\r", (*comp)->m_gsName, ((*mod).second)->m_gsName);
								((*mod).second)->m_ncDurability = -1;
								((*mod).second)->SetPlus("Powered", 0);	// Some Modules need to be shutdown
							}
							break;
					}

					
				}
			}

		}
	}

	// [ 3] - Speed vs TopSpeed: Needed incase we have shutdown an engine or one has been destroyed
	if (this->m_Speed > this->TopSpeed(true))
	{
		// Reset the Ship's TopSpeed
		this->m_dSpeed = this->TopSpeed(true);

	}

	// [ 4] - Update the Ship's Position
	if (this->m_ShipState->IsSet(_LAUNCHING))
	{
		if (this->m_nTimer > 0)
			this->m_nTimer--;
		else
		{
			this->m_nTimer = 0;
			CRoom* pRoom = CGameObjects:: Get().GameWorld()->GetArea(2)->GetRoom(0);

			if (pRoom)
				pRoom->RemShip(this->m_gsName);

			// We are no longer launching, we have launched
			this->m_ShipState->RemoveBit(_LAUNCHING);
			this->m_ShipState->SetBit(_FLYING);

			// We need to remove their Land room
			this->m_Land.Set(0,0,0);

			// Add the ship to Space
			pGalaxy->AddSpatialToSpace(this->m_Vnum);

			// Notify the Ground
			CArea* pArea = CGameObjects::Get().GameWorld()->GetArea(this->m_Land.Area());

			if (pArea)
			{
				CRoom * pRoom = pArea->GetRoom(this->m_Land.Room());
				if (pRoom)
					pRoom->Write("With a blast of heat, %s:%s disappears into the Planet's atmosphere.\n\r", this->m_gsType, this->m_gsName);
			}

			pArea = NULL; // Free the memory
			pRoom = NULL;

			this->Write(MT_CRITICAL, "You leave the Planet's atmosphere far behind and enter the void of space.\n\r");

			// Give a Completed Event so the Crew can update
			if (m_Crew.size() > 0)
			{
				EComplete * pEvent = new EComplete();
				pEvent->m_nType = COrder::_LAUNCH;

				this->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
			}

			// Defaults to heading straight down the positive Z
			this->m_Heading->x = 0; this->m_Heading->y = 0; this->m_Heading->z = 0;
			this->m_dHeading->x = 0; this->m_dHeading->y = 0; this->m_dHeading->z = 0;

			this->NotifySpace("%s appears from Corellia's atmosphere.\n\r", this->m_gsName);


			// We need to power down our Repulsor coils
			this->m_nPower[CShip::PT_REPULSOR] = 0;
			for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)			
			for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
			for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
			{	
				if ((*comp)->m_Type == CComponent::CT_INTERNAL)
				{
					CModule* pMod = (*comp)->Get(CModule::MT_REPULSOR_COILS);

					if (pMod)
					{
						pMod->SetPlus("Powered", 0);
						this->Write(MT_SYSTEMS, "[%s] %s offline.\n\r", (*comp)->m_gsName, pMod->m_gsName);					
					}
				}
			}
			
				
			// Set 10% speed if they haven't set one already
			if (this->m_Speed <= 0)
			{
				this->m_Speed = (int)((float)this->TopSpeed(true) * 0.1);
				this->m_dSpeed = this->m_Speed;
			}

			// This is just temporary #TODO#
			this->m_gsSector = "Corellian Sector";	
			// Save the changes
			this->Save();
		}
	}

	// [ 5] - Update the Ship's Speed 
	if (this->m_dSpeed != this->m_Speed)
	{
		// They are different so we need to change our speed
		// [ 5a] They are accelerating
		if (this->m_dSpeed > this->m_Speed)
		{
			// Check here that we aren't going to push them over 
			// their destination speed

			if ((this->m_Speed + this->Acceleration(true)) > this->m_dSpeed)
			{
				this->m_Speed = this->m_dSpeed;
			}
			else
			{
				this->m_Speed += this->Acceleration(true);
			}

			// Did we over do it?
			if (this->m_Speed > this->TopSpeed(true))
				this->m_Speed = this->TopSpeed(true);
		}
		// [ 5b] They are decelerating
		// This could because they have just shut all their engines down or had
		// them destroyed
		else if (this->m_dSpeed < this->m_Speed)
		{
			// Is our acceleration too good? I.e. will it slow us below
			// the speed we want?
			if ((this->m_Speed - this->Acceleration(true)) < this->m_dSpeed)
			{
				this->m_Speed = this->m_dSpeed;
			}
			else
			{
				// Check here for destroyed engines
				if (this->TopSpeed(true) <= 0)
				{
					this->m_Speed -= 8;
				}

				this->m_Speed -= this->Acceleration(true);
			}
			
			// Did we over do it?
			if (this->m_Speed < 0)
				this->m_Speed = 0;
		}
		// [ 5c] They have reached their destination speed
		if (this->m_dSpeed == this->m_Speed)
		{
			gString gsTmp;
			
			if (this->m_Speed >= this->TopSpeed(true) && this->TopSpeed(true) != 0)
			{
				this->Write(MT_HELM, "The ship has attained flank speed\n\r");
				gsTmp.Format("%s's sublight drives glow white-hot as it acheived flank speed.\n\r", this->m_gsName);
			}
			else if (this->m_Speed == 0)
			{
				this->Write(MT_HELM, "The ship has come to a halt\n\r");
				gsTmp.Format("Darkness consumes %s's sublight drives as it comes to a halt.\n\r", this->m_gsName);
			}
			else
			{
				this->Write(MT_HELM, "The ship has attained its new speed\n\r");
				gsTmp.Format("%s begins to cruise at its new speed.\n\r", this->m_gsName);
			}
			

			// Give a Completed Event so the Crew can update
			if (m_Crew.size() > 0)
			{
				EComplete * pEvent = new EComplete();
				pEvent->m_nType = COrder::_SPEED;

				this->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
			}			
			this->NotifySpace(gsTmp);
		}
	}

	// [ 6] - Give us a Space Prompt and move the Ship
	if (m_Speed > 0 && m_ShipState->IsSet(_FLYING))
	{
		this->Write(MT_HELM, "#400<#701 %d %d %d#400> :#401: #701Bearing#400 [#700 %d#400]#701 Mark#400 [#700 %d#400]#701 Roll#400 [#700%s#400] #400@#700 %d#400 MGLTs#700\n\r", m_Location->x, m_Location->y, m_Location->z, m_Heading->z, m_Heading->y, m_Heading->x == 0.0 ? "Upright" : (m_Heading->x == 90.0 ? "Left" : (m_Heading->x == 180.0 ? "Inverted" : (m_Heading->x == 270.0 ? "Right" : "Upright"))), m_Speed);
		
		// Move the Ship
		// We need to convert its Polar headings
		CCart* pCart = new CCart();

		// We dont want players to have a heading and not to move 
		// so we set 0, 0, 0 equal to 1, 1, 1
		if (this->m_Heading->z == 0 && this->m_Heading->y == 0)
		{
			pCart->x = 0;
			pCart->y = 0;
			pCart->z = 1;
		}
		else
		{
			// The ship is assumed to always move forward along the positive Z axis
			// hence when we have changed course a system for determining
			// the location of our Z axis is required.
			/*
			pCart->x = pCart->GetX(360-this->m_Heading->z, this->m_Heading->y, 10000);
			pCart->y = pCart->GetY(this->m_Heading->y, 10000);
			pCart->z = pCart->GetZ(360-this->m_Heading->z, this->m_Heading->y, 10000);*/


			pCart->SetXYZ(this->m_Heading->z, this->m_Heading->y);
		}
		this->m_Location->Move(pCart, m_Speed);

		delete pCart; // Free memory
		pCart = NULL;
	}  

	// [ 7] - Update the Ship if its is turning
	// Destination heading and current heading are not the same
	if (this->m_Heading->z != this->m_dHeading->z || this->m_Heading->y != this->m_dHeading->y)
	{
		// Yawing
		float ncF = this->m_Heading->z;
		float ndF = this->m_dHeading->z;
		// Pitching
		float ncQ = this->m_Heading->y;
		float ndQ = this->m_dHeading->y;

		// Due to negative nature we need to fix headings

		if (ncF < 0)
			ncF = 360 - abs(ncF);

		if (ndF < 0)
			ndF = 360 - abs(ndF);

		if (ncQ < 0)
			ncQ = 360 - abs(ncQ);

		if (ndQ < 0)
			ndQ = 360 - abs(ndQ);

		int nManeuver = this->Maneuver(true);
		bool bRight = false; // Defaults to turning left

		// Even with all Maneuver Thrusters destroyed a ship can still attempt to turn very VERY slowly
		if (nManeuver < 0)
			nManeuver = 18; // Takes 10 pulses to change course completely

		
		// Finally worked out the best way to work out the Maneuver
		// We need to determine the shortest route from the CURR maneuver
		// to the DEST maneuver. We then have to work out if this distance is
		// to the left or right.

		// X = CURR Maneuver
		// Y = DEST Maneuver
		// A = (+) First Distance
		// B = (-) Second Distance
		// IF (X - A/B) = Y THEN left
		// IF (X + A/B) = Y THEN right

		// ncF, ndF, ncQ, ndQ Angles fixed by 180 degrees to get rid of negative values


		// F ANGLE :: YAWING
		// We need to calculate the two distances
		// to turn, I.e. if we turned left or right
		int nA = abs((int) ncF - ndF);	// We need to calculate the two distances
		int nB = 360 - abs(nA);			// to turn, I.e. if we turned left or right


		if (nA >= nB)					// nA will be the smallest distance now
			nA = nB;

		int nD = (int)ncF + nA;			// Work out our final heading if we moved right

		if (nD > 360)					// Fix value if greater than 360
			nD = abs(360 - nD);

		if (nD == (int)ndF)				// Add this distance to the current distance
			bRight = true;				// Turning Right
		else
			bRight = false;	


		// CASE 1 :: Turning Right
		// nfDiff/nqDiff are the distance through which we need to turn
		if (bRight)
		{
			int nDiff = nA; // Distance through which we need to turn

			// Our maneuver would have us there in one tick
			if (nManeuver >= nDiff)
			{
				this->m_Heading->Course(0.0, 0.0, nDiff);
			}
			else
			{
				this->m_Heading->Course(0.0, 0.0, nManeuver);
			}

		}
		// CASE 2 :: Turning Left
		else
		{
			int nDiff = nA; // Distance through which we need to turn

			// Our maneuver would have us there in one tick
			if (nManeuver >= nDiff)
			{
				this->m_Heading->Course(0.0, 0.0, -nDiff);
				
			}
			else
			{
				this->m_Heading->Course(0.0, 0.0, -nManeuver);
			}

		}

		// Q ANGLE :: PITCHING
		nA = abs((int) ncQ - ndQ);		// We have our two distances
		nB = 360 - abs(nA);


		if (nA > nB)	// nA will be the smallest distance now
			nA = nB;

		nD = (int)ncQ + nA; // Work out our final heading if we moved right

		if (nD > 360)	// Fix value
			nD = 360 - nD;

		if (nD == (int)ndQ) // Add this distance to the current distance
			bRight = true;	// Turning Right
		else
			bRight = false; // Turning Left


		// CASE 1 :: Pitching Up
		// nfDiff/nqDiff are the distance through which we need to turn
		if (bRight)
		{
			int nDiff = nA; // Distance through which we need to turn

			// Our maneuver would have us there in one tick
			if (nManeuver >= nDiff)
			{
				this->m_Heading->Course(0.0, nDiff, 0.0);
			}
			else
			{
				this->m_Heading->Course(0.0, nManeuver, 0.0);
			}

		}
		// CASE 2 :: Pitching Down
		else
		{
			int nDiff = nA; // Distance through which we need to turn

			// Our maneuver would have us there in one tick
			if (nManeuver >= nDiff)
			{
				this->m_Heading->Course(0.0, -nDiff, 0.0);
			}
			else
			{
				this->m_Heading->Course(0.0, -nManeuver, 0.0);
			}

		}


		if (this->m_Heading->y == this->m_dHeading->y && this->m_Heading->z == this->m_dHeading->z)
		{
			this->Write(MT_HELM, "[Helm] New course attained.\n\r");
			this->NotifySpace("%s finishes its turn having attained its new course.\n\r", this->m_gsName);

			// Give a Completed Event so the Crew can update
			if (m_Crew.size() > 0)
			{
				EComplete * pEvent = new EComplete();
				pEvent->m_nType = COrder::_COURSE;

				this->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
			}

		}

	}

	// [ 8] - Roll the ship
	if (this->m_Heading->x != this->m_dHeading->x)
	{
		// Rolling
		float ncR = this->m_Heading->x;
		float ndR = this->m_dHeading->x;
		int nManeuver = this->Maneuver(true);
		bool bRight = false;
		int nA, nB, nD;

		nA = abs((int) ncR - ndR);		// We have our two distances
		nB = 360 - abs(nA);


		if (nA > nB)		// nA will be the smallest distance now
			nA = nB;

		nD = (int)ncR + nA; // Work out our final heading if we moved right

		if (nD > 360)		// Fix value
			nD = 360 - nD;

		if (nD == (int)ndR) // Add this distance to the current distance
			bRight = true;	// Rolling Right
		else
			bRight = false; // Rolling Left


		// CASE 1 :: Rolling Right
		// nfDiff/nqDiff are the distance through which we need to turn
		if (bRight)
		{
			int nDiff = nA; // Distance through which we need to turn

			// Our maneuver would have us there in one tick
			if (nManeuver > nDiff)
			{
				this->m_Heading->Course(nDiff, 0.0, 0.0);
			}
			else
			{
				this->m_Heading->Course(nManeuver, 0.0, 0.0);
			}

		}
		// CASE 2 :: Rolling Left
		else
		{
			int nDiff = nA; // Distance through which we need to turn

			// Our maneuver would have us there in one tick
			if (nManeuver > nDiff)
			{
				this->m_Heading->Course(-nDiff, 0.0, 0.0);
			}
			else
			{
				this->m_Heading->Course(-nManeuver, 0.0, 0.0);
			}

		}

		if (this->m_Heading->x == this->m_dHeading->x)
		{
			this->Write(MT_HELM, "[Helm] Roll completed.\n\r");
			this->NotifySpace("%s completes its roll.\n\r", this->m_gsName);

			// Give a Completed Event so the Crew can update
			if (m_Crew.size() > 0)
			{
				EComplete * pEvent = new EComplete();
				pEvent->m_nType = COrder::_ROLL;

				this->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
			}
		}

	}
		

	// [ 9] Perform a Sensor Sweep
	// Check if the Sensor has been powered up is active
	if (this->m_nSweep >= 0)
	{
		int nNumNew = 0;

		if (this->m_nSweep == 0)
		{
			// Need all the Radomes so we know our different Sensors limits
			int nRange[CShip::S_MAX];

			ModuleList* mlMods = this->Get(CModule::MT_RADOME);

			for (ModuleList::iterator mod = mlMods->begin(); mod != mlMods->end(); mod++)
			{
				nRange[(*mod)->Plus("Type", false)] = (*mod)->Plus("Range", true);
			}

			// Sweep has completed so then lets poll the system for new contacts
			this->m_Contacts.clear();
			for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
			for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
			{
				CSpatial* pSpatial = ***spa;

				if (!pSpatial)
				{
					(*pos).second.erase(spa);
					continue;
				}

				if (pSpatial->m_gsSector == this->m_gsSector)
				{
					long double nDistance = pSpatial->m_Location->Distance(this->m_Location);

					// Don't confuse your own ship's signal
					if (pSpatial->m_gfFileName == this->m_gfFileName)
						continue;

					// We dont want landed ships
					if (pSpatial->m_nType == CSpatial::SO_SHIP)
					{
						if (!((CShip*)pSpatial)->m_ShipState->IsSet(CShip::_FLYING))
							continue;
					}

					if (!this->Contains(pSpatial))
					{
						nNumNew++;
						CContact* pContact = new CContact;
						if (this->m_Sweep[CShip::S_EM] && nDistance <= nRange[CShip::S_EM])
							pContact->m_Signature[CSpatial::SI_EM] = pSpatial->m_Signature[CSpatial::SI_EM];
						if (this->m_Sweep[CShip::S_HEAT] && nDistance <= nRange[CShip::S_HEAT])
							pContact->m_Signature[CSpatial::SI_HEAT] = pSpatial->m_Signature[CSpatial::SI_HEAT];
						if (this->m_Sweep[CShip::S_ION] && nDistance <= nRange[CShip::S_ION])
							pContact->m_Signature[CSpatial::SI_ION] = pSpatial->m_Signature[CSpatial::SI_ION];
						if (this->m_Sweep[CShip::S_MASS] && nDistance <= nRange[CShip::S_MASS])
							pContact->m_Signature[CSpatial::SI_MASS] = pSpatial->m_Signature[CSpatial::SI_MASS];

						pContact->m_Spatial = pSpatial->m_Vnum;
						pContact->m_Location = pSpatial->m_Location;
						this->m_Contacts.insert(ContactMap::value_type(this->m_Contacts.size()+1, pContact));
					}
				}

				pSpatial = NULL; 
			}
			
			// We have finished the sweep
			this->m_nSweep = -1;
			this->Write(MT_SENSORS, "#400[#401SWEEP completed #400>#401>#700 %d new contacts detected#400]#700\n\r", nNumNew);

			// Give a Completed Event so the Crew can update
			if (m_Crew.size() > 0)
			{
				EComplete * pEvent = new EComplete();
				pEvent->m_nType = COrder::_SWEEP;

				this->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
			}
		}
		else
		{
			this->m_nSweep--;
		}

	} 
	// [10] - Collision Detection
	// Check if this ship has collided with another
	if (this->m_Speed > 0)
	{
		// First check if they are moving
		// We only check those Spatial Objects within our sector

		SpatialMap::iterator find = pGalaxy->m_SpatialMap.find(this->m_gsSector);

		// No use checking for collisions if they are alone
		if (find != pGalaxy->m_SpatialMap.end())
		{
			for (SpatialList::iterator spa = (*find).second.begin(); spa != (*find).second.end(); spa++)
			{
				// Distance check
				CSpatial* pSpatial = ***spa;

				if (!pSpatial)
				{
					(*find).second.erase(spa);
					continue;
				}

				// So you dont crash into your self
				if (pSpatial == this)
					continue;

				// We dont want to crash into landed ships!
				if (pSpatial->m_nType == CSpatial::SO_SHIP)
				{
					CShip* pShip = (CShip*)pSpatial;
					if (!pShip->m_ShipState->IsSet(CShip::_FLYING))
						continue;
				}

				// First we check if our movement this tick has caused us to collide with
				// another ship. We check if the ship's shape is now contained within us
				CCart* pBoundary = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), this->m_Shape->Length());
				CCart* pZero = new CCart(0, 0, 0);

				// Rotate these points related to our facing
				pBoundary->Rotate(this->m_Shape->m_Center, this->m_Heading->z, this->m_Heading->y, this->m_Heading->x);
				pZero->Rotate(this->m_Shape->m_Center, this->m_Heading->z, this->m_Heading->y, this->m_Heading->x);

				// Translate them into the global scope
				int nX = this->m_Location->x - this->m_Shape->m_Center->x;
				int nY = this->m_Location->y - this->m_Shape->m_Center->y;
				int nZ = this->m_Location->z - this->m_Shape->m_Center->z;

				// Here we translate them
				pBoundary->x += nX; pBoundary->y += nY; pBoundary->z += nZ;
				pZero->x += nX; pZero->y += nY; pZero->z += nZ;

				// Now we do the same for our target
				CCart* pTarBoundary = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
				CCart* pTarZero = new CCart(0, 0, 0);

				// We only rotate them if the spatial is a ship
				if (pSpatial->m_nType == CSpatial::SO_SHIP)
				{
					CShip* pShip = (CShip*)pSpatial;
					pTarBoundary->Rotate(pSpatial->m_Shape->m_Center, pShip->m_Heading->z, pShip->m_Heading->y, pShip->m_Heading->x);
					pTarZero->Rotate(pSpatial->m_Shape->m_Center, pShip->m_Heading->z, pShip->m_Heading->y, pShip->m_Heading->x);
				}

				// Translate them into the global scope
				nX = pSpatial->m_Location->x - pSpatial->m_Shape->m_Center->x;
				nY = pSpatial->m_Location->y - pSpatial->m_Shape->m_Center->y;
				nZ = pSpatial->m_Location->z - pSpatial->m_Shape->m_Center->z;

				pTarBoundary->x += nX; pTarBoundary->y += nY; pTarBoundary->z += nZ;
				pTarZero->x += nX; pTarZero->y += nY; pTarZero->z += nZ;

				// Now we have these boundary vessels we can check if they are contained within each other
				// In order for a collision to occur there needs to be an overlap of all three axis	
				// Boundary is the maximum bound for each axis while Zero is the minimum bound.
				// If there is an overlap in these bounds then there is a collision
				bool bCollideX = false;
				bool bCollideY = false;
				bool bCollideZ = false;

				// Check for the X axis first
				if ((pBoundary->x <= pTarBoundary->x && pBoundary->x >= pTarZero->x) ||
				    (pZero->x >= pTarZero->x && pZero->x <= pTarBoundary->x))
						bCollideX = true;

				if ((pBoundary->y <= pTarBoundary->y && pBoundary->y >= pTarZero->y) ||
				    (pZero->y >= pTarZero->y && pZero->y <= pTarBoundary->y))
						bCollideY = true;

				if ((pBoundary->z <= pTarBoundary->z && pBoundary->z >= pTarZero->z) ||
				    (pZero->z >= pTarZero->z && pZero->z <= pTarBoundary->z))
						bCollideZ = true;


				if (bCollideX && bCollideY && bCollideZ)
				{
					this->Write(MT_CRITICAL, "#101You've just collided with %s#700\n\r", pSpatial->m_gsName);

					// Temporary check on size difference
					if (pSpatial->m_nType == CSpatial::SO_SHIP)
					{
						CShip* pTarget = ((CShip*)pSpatial);

						// Collide with something bigger it destroys you unless you have shields
						if (pTarget->m_nClass > this->m_nClass)
						{
							// Any shields in our fore up
							if (this->Shield(A_FORE, true) > 0)
							{
								// Shields prevent you dying but you will take enough damage to destroy
								// your fore shields
								//this->Damage(this->Shield(A_FORE, true), A_FORE);
								int nAngle = CGameObjects::Get().Rand()->NumberRange(90, 270);
								this->m_dHeading->z -= nAngle; // After the collision you get sent off in a random
								this->m_Heading->z  -= nAngle; // Direction away from the target
							}
							else
							{
								pTarget->Write(MT_CRITICAL, "#101%s crumples into space dust as it collides into your vessel!#700\n\r", this->m_gsName);																							
							}
						}
					}
					else
					{
						// For the time being any Spatial object we crash into will disintegrate
						this->Write(MT_CRITICAL, "#101%s crumples to pieces as it impacts your vessel.#700\n\r", pSpatial->m_gsName);
						this->Destroy(); // Destroy them
					}
				}				
				// Delete the memory
				delete (pBoundary);
				pBoundary = NULL;
				delete (pZero);
				pZero = NULL;
				delete (pTarZero);
				pTarZero = NULL;
				delete (pTarBoundary);
				pTarBoundary = NULL;

			}
		}

	}

	// [11] - Targetting Timer
	if (this->m_nTarget >= 0)
	{
		if (this->m_nTarget == 0)
		{
			this->Write(MT_FCS, "#101Target locked.#700\n\r");
			this->m_nTarget = -1;

			CSpatial* pTarget = **this->m_Target; // * overridded for CSpatialID

			// Give a Completed Event so the Crew can update
			if (m_Crew.size() > 0)
			{
				EComplete * pEvent = new EComplete();
				pEvent->m_nType = COrder::_TARGET;

				this->GiveEvent(CCrew::CT_BRIDGECREW, pEvent);
			}

			if (pTarget)
				pTarget->Write(MT_CRITICAL, "#101%s has targetted you!#700\n\r", this->m_gsName);
			
		}
		else
		{
			this->m_nTarget--;
		}

	}

	// [12] - Meltdown Timer
	if (this->m_nMeltdown >= 0)
	{
		if (this->m_nMeltdown == 0)
		{
			CRandom* pRandom = CGameObjects::Get().Rand();
			this->Damage(pRandom->NumberRange(9999999, 99999999), -1); // #TODO# Determine realisitic damage
			this->Write(MT_CRITICAL, "#100[#101ALARM#100]#700 Reactor has melted down, catastrophic damage caused!");
			this->NotifySpace("An awesome explosion of light and flame indicates %s's reactor going critical!", this->m_gsName);
			this->m_nMeltdown--;
			pRandom = NULL;
		}
		else
		{
			this->m_nMeltdown--;
		}
	}

	// [13] - Self Destruct
	if (this->m_nSelfdestruct >= 0)
	{
		if (this->m_nSelfdestruct == 0)
		{
			this->Damage(999999999, -1); // #TODO# Determine realisitic damage
			gString gsMsg;
			this->Write(MT_CRITICAL, "#100[#101ALARM#100]#700 Self-Destruct complete.... Thank you!\n\r");
			this->NotifySpace("An awesome explosion of light and flame indicates %s's reactor going critical!", this->m_gsName);
			this->m_nSelfdestruct--;
		}
		else
		{
			this->Write(MT_CRITICAL, "#100[#101ALARM#100]#700 Self-destruct in %d\n\r", this->m_nSelfdestruct);
			this->m_nSelfdestruct--;
		}
	}

	// [15] Destroyed
	if (this->Destroyed())
	{
		// Ship has been completely vaped
		// Flag this for deletion

		this->Write(MT_CRITICAL, "#101Your vision disappears in a flash of bright incandesant light, seering hot pain consumes your body as your Spacecraft explodes!#700\n\r");
		this->NotifySpace("%s is consumed by flames as the %s explodes!\n\r", this->m_gsName, this->m_gsType);

		this->m_bDelete = true;
		return;
	}

	// [14] - Electromagnetic Signatures will decay slowly following a transmission
	/*
	if (pComms && pComms->m_bPowered)
	{		
		int nEm = pComms->m_nTechLevel * 100;
		
		// A damage Communications Array does not emit as much EM Energy due to its damage nature
		float nEfficiency = pComms->Integrity()/100;

		nEm *= nEfficiency;

		if (m_Signature[CSpatial::SI_EM] < nEm)
			m_Signature[CSpatial::SI_EM] = nEm;
		else
			m_Signature[CSpatial::SI_EM] -= (pComms->m_nTechLevel * 10);

		// Has this taken us below our static value?

	}
*/


}

// Display a Message to the Ship dependant upon the Type
void CShip::Write(int nType, char *fmt, ...)
{
	ActorMap list = CGameObjects::Get().GameWorld()->Players();
	ActorMap::iterator pla;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	// What we do we the message depends on the type of message:
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


	// Run through all the Ship's Modules to find the ones we want
	// to give a message to
	bool bFound = false;
	for (FrameList::iterator frame = this->m_Frames.begin(); frame != this->m_Frames.end(); frame++)
	for (HullList::iterator hull = (*frame)->m_HullCubes.begin(); hull != (*frame)->m_HullCubes.end(); hull++)
	for (ComponentList::iterator comp = (*hull)->m_Components.begin(); comp != (*hull)->m_Components.end(); comp++)
	{
		// Only want the Control Points
		if ((*comp)->m_Type == CComponent::CT_CONTROLPOINT)
		{
			// The Module we get is dependant upon our message type
		    for (ModuleMap::iterator it = (*comp)->m_Modules.begin(); it != (*comp)->m_Modules.end(); it++)
			{
				CModule* pMod = (*it).second;

				// Messages to the Helm
				if (pMod->CanReceive(MT_HELM) && nType == MT_HELM)
					bFound = true;
				// Messages to the Sensors
				else if (pMod->CanReceive(MT_SENSORS) && nType == MT_SENSORS)
					bFound = true;

				else if (pMod->CanReceive(MT_SYSTEMS) && nType == MT_SYSTEMS)
					bFound = true;

				// Message to the Navigator
				else if (pMod->CanReceive(MT_ASTROGATION) && nType == MT_ASTROGATION)
					bFound = true;

				// Communications Message
				else if (pMod->CanReceive(MT_COMMS) && nType == MT_COMMS)
					bFound = true;

				// Message to the Gunnery Master
				else if (pMod->CanReceive(MT_FCS) && nType == MT_FCS)
					bFound = true;

				// Check the number of messages on the Module
				if (bFound)
				{
					if (pMod->m_Messages.size() >= 30)
					{
						// More than 30 take off the oldest and replace it
						pMod->m_Messages.pop_front();
						pMod->m_Messages.push_back(buf);
					}
					else
					{
						// Otherwise just stick it on the end
						pMod->m_Messages.push_back(buf);
					}

					// Now is the Module manned? If so give the message to the player
					if (pMod->m_Manned)
					{
						pMod->m_Manned->Write(buf);
					}
				}

				bFound = false;
			}

		}
	}

	if (nType == MT_CRITICAL)
	{
		// Critical messages are given to all players within a Control Point
		for (pla = list.begin(); pla != list.end(); pla ++)
		{
			CActor* pPlayer = (CPlayer*)((*pla).second);
			// Only give the message if they are in a Controlpoint
			if (pPlayer->CurrentRoom()->GetComp(CComponent::CT_CONTROLPOINT)->size() > 0)
			{
				if (pPlayer->CurrentRoom()->GetArea()->Ship() == this->m_gsName)
					pPlayer->Write(buf);
			}
		}
	}
	else if (nType == MT_ENTIRESHIP)
	{
		for (pla = list.begin(); pla != list.end(); pla ++)
		{
			CActor* pPlayer = (CPlayer*)((*pla).second);

			if (pPlayer->CurrentRoom()->GetArea()->Ship() == this->m_gsName)
				pPlayer->Write(buf);
		}
	}
}

void CShip::NotifySpace(CSpatial* pSkip, char *fmt, ...)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	std::vector<CSpatialID*>SpatialDelete;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
	for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
	{
		CSpatial* pSpatial = ***spa;

		if (!pSpatial)
		{
			SpatialDelete.push_back(*spa);
			continue;
		}

		// Is this our Ship?
		if (pSpatial == this)
			continue;

		// Is this one we wanted to skip?
		if (pSkip == pSpatial)
			continue;

		// Is it a ship? If so is it launched?
		if (pSpatial->m_nType == CSpatial::SO_SHIP)
		{
			if (!((CShip*)pSpatial)->m_ShipState->IsSet(CShip::_FLYING))
				continue;

			((CShip*)pSpatial)->Write(CShip::MT_CRITICAL, "#300%s#700\n\r", buf);
		}
		else
		{
			gString gsMsg; gsMsg.Format("%s", buf);
			pSpatial->Notify("#300" + gsMsg + "#700\n\r");
		}

	}
	
	for (std::vector<CSpatialID*>::iterator dels = SpatialDelete.begin(); dels != SpatialDelete.end(); dels++)
		pGalaxy->RemoveSpatialFromSpace(*dels);

}
void CShip::NotifySpace(char *fmt, ...)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	std::vector<CSpatialID*>SpatialDelete;

	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	for (SpatialMap::iterator pos = pGalaxy->m_SpatialMap.begin(); pos != pGalaxy->m_SpatialMap.end(); pos++)	
	for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)
	{
		CSpatial* pSpatial = ***spa;

		if (!pSpatial)
		{
			SpatialDelete.push_back(*spa);
			continue;
		}

		// Is this our Ship?
		if (pSpatial == this)
			continue;

		// Is it a ship? If so is it launched?
		if (pSpatial->m_nType == CSpatial::SO_SHIP)
		{
			if (!((CShip*)pSpatial)->m_ShipState->IsSet(CShip::_FLYING))
				continue;

			((CShip*)pSpatial)->Write(CShip::MT_CRITICAL, "#300%s#700\n\r", buf);
		}
		else
		{
			gString gsMsg; gsMsg.Format("%s", buf);
			pSpatial->Notify("#300" + gsMsg + "#700\n\r");
		}

	}
	
	for (std::vector<CSpatialID*>::iterator dels = SpatialDelete.begin(); dels != SpatialDelete.end(); dels++)
		pGalaxy->RemoveSpatialFromSpace(*dels);


}

bool CShip::Contains(CSpatial* pSpatial)
{
	ContactMap::iterator con;
	std::vector<ContactMap::iterator>ContactDelete;
		
	for (con = this->m_Contacts.begin(); con != this->m_Contacts.end(); con++)
	{
		CSpatial* pFound = **(((*con).second)->m_Spatial);

		if (!pSpatial)
		{
			// Contact no longer exists, we remove it
			ContactDelete.push_back(con);
			continue;
		}

		// Spatial ordinance does not have a filename
		if (pFound->m_gsName == pSpatial->m_gsName)
			return true;

		// Found it!
		if (pFound->m_gfFileName == pSpatial->m_gfFileName)
			return true;
	}

	// Delete any Contacts in this List that we needed to remove
	for (std::vector<ContactMap::iterator>::iterator delc = ContactDelete.begin(); delc != ContactDelete.end(); delc++)
		this->m_Contacts.erase(*delc);

	return false; 
}

bool CShip::GiveEvent(int nType, CEvent *pEvent)
{
	for (CrewMap::iterator crew = m_Crew.begin(); crew != m_Crew.end(); crew++)
	for (CrewList::iterator it = (*crew).second.begin(); it != (*crew).second.end(); it++)
	{
		if ((*it)->m_nCrewState >= CCrew::_ONDUTY)
		{
			if ((*it)->m_nType == nType)
			{
				(*it)->ReceiveEvent(*pEvent);
			}
		}
	}

	return true;
}


