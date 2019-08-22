//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.
//
// The Advanced MUD Codebase Project
// AMC, copyright (c) 2005, 2006 by Owen Varley <owen@sw-erp.org>

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: CRace CRaceMgr CBodyPart
// Header   :: Race.h
// Function :: Used to define races within the AMC engine. This file provides an implementation
//			:: for both the structure of the race class and also the manager which handles the
//			:: loading and saving of race files

#pragma warning(disable: 4786)

#include "Race.h"
#include "../gTools/TinyXml.h"
#include "GameObjects.h"
#include "../gTools/Log.h"


char* CRace::szBodyParts[] = { "Head", "Chest", "Right Arm", "Left Arm", "Abdomen", "Right Leg", "Left Leg", "About", "Tail", "Lekku", "Antenna", "Torso", "Right Tred", "Left Tred", NULL };
char* CRace::szBodyXML[] = { "head", "chest", "r_arm", "l_arm", "abdomen", "r_leg", "l_leg", "about", "tail", "lekku", "antenna", "torso", "r_Tred", "l_tred", NULL };

// BodyPart class is used to define a single body part location for a race
// this defines the number of hit points that race has for a specific body part
// and also the percentage chance of hitting that body part.
CBodyPart::CBodyPart()
{
	this->m_fPercentage = 0.0;
	this->m_nHitpoints  = 0;
}

CBodyPart::~CBodyPart()
{
	this->m_fPercentage = 0.0;
	this->m_nHitpoints  = 0;
}

// Write a bodypart to an XML file
void CBodyPart::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	TiXmlElement* pBodyPart = (TiXmlElement*)(Tools.InsertXmlChild(pParent, (const char*)CRace::szBodyXML[m_nType]));

	pBodyPart->SetAttribute("chance", m_fPercentage);
	pBodyPart->SetAttribute("hitpoints", m_nHitpoints);

}

// Read a bodypart in from its XML file
void CBodyPart::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlElement* pBodyPart = (TiXmlElement*)pParent;

	// Set the type for this bodypart
	for (int i = 0; i < CRace::_NUM_BODYPARTS; i++)
	{
		gString gsRace = CRace::szBodyXML[i];
		gString gsParent = pParent->Value();
		
		if (gsRace == gsParent)
			this->m_nType = i;
	}

	const char* szChance		= pBodyPart->Attribute("chance");
	const char* szHitpoints		= pBodyPart->Attribute("hitpoints");


	if (szChance)		m_fPercentage	= atof(szChance);	// Can't cast from char to float
	if (szHitpoints)	m_nHitpoints	= atoi(szHitpoints);
	
}


// Race class, defines the structure and stats for a race
CRace::CRace()
{
	
}

CRace::~CRace()
{
	this->m_gsName = "";
	this->m_BodyParts.clear();
}

// Load commonly called by world init
bool CRace::Load(gFileName gsRace)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsRace) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Race");

	ReadXml(pNode);
	return true;
}

// Save commonly called by world init
bool CRace::Save(gFileName gsRace)
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Race");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsRace);
}

// Write a race to XML
void CRace::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	Tools.WriteXml(pParent, "name",				m_gsName);

	// Write the bodyparts
	TiXmlNode* pBodyParts = Tools.InsertXmlChild(pParent, "Body");
	for (BodyPartMap::iterator bp = m_BodyParts.begin(); bp != m_BodyParts.end(); bp++)
		((*bp).second)->WriteXml(pBodyParts);

	// Write the attributes
	m_Attributes.WriteXml(pParent);

	// Write the Languages
	TiXmlNode* pLanguages = Tools.InsertXmlChild(pParent, "Languages");
	for (gStringList::iterator gs = m_Languages.begin(); gs != m_Languages.end(); gs++)
		Tools.WriteXml(pLanguages, "name",				(*gs));

	// Write the Perks
	TiXmlNode* pPerks = Tools.InsertXmlChild(pParent, "Perks");
	for (gStringList::iterator gs = m_Perks.begin(); gs != m_Perks.end(); gs++)
		Tools.WriteXml(pPerks, "name",				(*gs));

}

// Read a Race in from XML
void CRace::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in the name for the race
	Tools.ReadXml(pParent, "name",				m_gsName);

	// Read in the Bodyparts
	TiXmlNode* pBodyNode = pParent->FirstChild("Body");
	if ( pBodyNode != NULL )
	{
		TiXmlElement* pPartElement = (TiXmlElement*)(pBodyNode->FirstChild());
		while ( pPartElement != NULL )
		{
			CBodyPart* pPart = new CBodyPart;
			pPart->ReadXml(pPartElement);

			m_BodyParts.insert(BodyPartMap::value_type(pPart->Type(), pPart));
			pPartElement = (TiXmlElement*)(pPartElement->NextSibling());
		}
	}

	// Read in the Attributes
	m_Attributes.ReadXml(pParent);

	// Read in languages
	TiXmlNode* pLanguage = pParent->FirstChild("Languages");
	if ( pLanguage != NULL )
	{
		TiXmlElement* pPartElement = (TiXmlElement*)(pLanguage->FirstChild());
		while ( pPartElement != NULL )
		{			
			m_Languages.push_back(pPartElement->FirstChild()->Value());

			pPartElement = (TiXmlElement*)(pPartElement->NextSibling());
		}
	}
	// Read in perks
	TiXmlNode* pPerk = pParent->FirstChild("Perks");
	if ( pPerk )
	{
		TiXmlElement* pPartElement = (TiXmlElement*)(pPerk->FirstChild());
		while ( pPartElement != NULL )
		{
			m_Perks.push_back(pPartElement->FirstChild()->Value());

			pPartElement = (TiXmlElement*)(pPartElement->NextSibling());
		}
	}


}


// Race Manager is used to store and handle all Races
CRaceMgr::CRaceMgr()
{

}

CRaceMgr::~CRaceMgr()
{
	this->m_Races.clear();
}

bool CRaceMgr::Load(gFileName gsRaceMgr)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsRaceMgr) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("RaceList");
	gFileName gsRace;

	if ( pNode )
	{
		TiXmlElement* pRaceNode = (TiXmlElement*)(pNode->FirstChild());
		while ( pRaceNode != NULL )
		{
			CRace* pRace = new CRace;
			gsRace.Format("%s%s\\Races\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_WORLD], CGameObjects::Get().GameWorld()->Key(), pRaceNode->FirstChild()->Value());
			pRace->Load(gsRace);
			this->m_Races.push_back(pRace);
			
			pRaceNode = (TiXmlElement*)(pRaceNode->NextSibling());
		}
	}

	return true;
}

bool CRaceMgr::Save(gFileName gsRace)
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Race");


	return doc.SaveFile((const char*)gsRace);
	return true;
}
