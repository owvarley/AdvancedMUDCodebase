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

// Class    :: CTemplate
// Header   :: Spatial.h
// Function :: Handles the object structure for Templates and Rectangles which are used to define the size
//			   and temp of objects.

#pragma warning(disable: 4786)

#include "OTools.h"
#include "GameObjects.h"
#include "Player.h"
#include <Math.h>
#include "../gTools/Log.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////
// 1. Template Class
///////////////////////////////////////////////////////////////////////////////////////////////////////////


char * CTemplate::szStatus[] = { "Not Started", "Template Design", "Area Design", "Weapon Config", "Review", "Awaiting Authorisation", "Completed", NULL };

CTemplate::CTemplate()
{
	this->m_gfFileName = "";
	this->m_gsName = "";
	this->m_Ship = NULL;
	this->m_Shape = new CShape;
	this->m_Area = NULL;
	this->m_nStatus = _NONE;
}

CTemplate::~CTemplate()
{
	this->m_gfFileName = "";
	this->m_gsName = "";
	this->m_Ship = NULL;
	this->m_Shape = NULL;
	this->m_Area = NULL;
	this->m_nStatus = 0;
}

int CTemplate::Cost()
{
	return 0;
}

int CTemplate::Materials(int nType)
{
	return 0;

}

bool CTemplate::Load(gString gsFile)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Template");

	ReadXml(pNode);
	return true;
}

bool CTemplate::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Template");

	gString gsFile;

	gsFile.Format("%sShips\\Templates\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], this->m_gfFileName);


	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsFile);

	return true;
}

// Used to Generate the Shape of the vessel from its Template
bool CTemplate::Generate()
{
	// We can only generate the area if they have already defined
	// the entire structure for the Ship
	if (this->m_nStatus != _AREA)
		return false;

	// We cannot generate the shape unless we have a ship structure
	if (!this->m_Ship)
		return false;

	// A ship with no Hullcubes cannot be generated
	bool bFound = false;

	for (FrameList::iterator fra = this->m_Ship->m_Frames.begin(); fra != this->m_Ship->m_Frames.end(); fra++)
	{
		if ((*fra)->m_HullCubes.size() > 0)
		{
			bFound = true;
			break;
		}
	}

	// The Ship structure contained no Hullcubes
	if (!bFound)
		return false;


	// Clear Validation onto generation

	// [1] - We need to remove any previous shape assigned to this Object
	//       this allows the user to 'update' the Shape if he has added extra
	//       HullCubes onto it
	delete this->m_Shape;
	this->m_Shape = NULL;
	this->m_Shape = new CShape;

	return true;
}


bool CTemplate::Delete()
{
	// Take out of Template List
	int nCount = 0;
	TemplateList::iterator temp;
	gString szFile;
	CTLoader* pLoader = CGameObjects::Get().GameWorld()->TLoader();
	CTemplate* pTemplate;

	for (temp = pLoader->m_Templates.begin(); temp != pLoader->m_Templates.end(); temp++)
	{
		pTemplate = (*temp);

		if (pTemplate->m_gfFileName != this->m_gfFileName)
			nCount++;
		else
			break;
			
		
	}

	// Removed from list
	pLoader->m_Templates.erase(pLoader->m_Templates.begin()+nCount);
	// Delete object

	szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Ships\\Templates\\" + this->m_gfFileName;
	_unlink(szFile);

	pLoader->Save();

	return true;


}

void CTemplate::ReadXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",		m_gsName);
	Tools.ReadXml(pParent, "author",	m_gsAuthor);
	Tools.ReadXml(pParent, "authed",	m_gsAuthorisor);
	Tools.ReadXml(pParent, "status",		m_nStatus);

	// Read in the ship
	TiXmlNode* pShipNode = pParent->FirstChild("Ship");

	if (pShipNode != NULL)
	{
		CShip* pShip = new CShip;
		pShip->ReadXml(pShipNode);
		m_Ship = pShip;
	}

	// Read in the area too

	TiXmlNode* pAreaNode = pParent->FirstChild("Area");

	if (pAreaNode != NULL)
	{
		CArea* pArea = new CArea;
		pArea->ReadXml(pAreaNode);
		m_Area = pArea;
	}

	return;

}

void CTemplate::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",		m_gsName);
	Tools.WriteXml(pParent, "author",	m_gsAuthor);
	if (!m_gsAuthorisor.IsEmpty())
		Tools.WriteXml(pParent, "authed",	m_gsAuthorisor);
	Tools.WriteXml(pParent, "status",		m_nStatus);

	// Parse the ship into XML
	if (m_Ship)
	{
		TiXmlNode* pShip = Tools.InsertXmlChild(pParent, "Ship");

		if (pShip != NULL)
			m_Ship->WriteXml(pShip);
	}

	// Parse area too
	if (m_Area)
	{
		TiXmlNode* pArea = Tools.InsertXmlChild(pParent, "Area");

		if (pArea != NULL)
			m_Area->WriteXml(pArea);
	}

	return;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 3. Template Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
CTLoader::CTLoader()
{

}

CTLoader::~CTLoader()
{
	this->m_Templates.clear();
}

void CTLoader::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pTemplateNode = pParent->FirstChild("Template");

	while (pTemplateNode != NULL)
	{
		gString gsFilename, gsFile;

		Tools.ReadXml(pTemplateNode, "filename",	gsFile);

		CTemplate* pTemplate = new CTemplate();
		gsFilename.Format("%sShips\\Templates\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						

		if (pTemplate->Load(gsFilename))
		{
			pTemplate->m_gfFileName = gsFile;
			m_Templates.push_back(pTemplate);
		}
		else
		{
			delete pTemplate;
			pTemplate = NULL;
		}		

		pTemplateNode = pTemplateNode->NextSibling("Template");
	}

	return;
}

void CTLoader::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	for (TemplateList::iterator temp = m_Templates.begin(); temp != m_Templates.end(); temp++)
	{
		TiXmlNode* pTemplateNode = Tools.InsertXmlChild(pParent, "Template");

		Tools.WriteXml(pTemplateNode, "filename",		(*temp)->m_gfFileName);
	}

	return;
}




bool CTLoader::Load(gFileName gfFile)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gfFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("TemplateLoader");

	ReadXml(pNode);
	return true;
}

// Save commonly called by world init
bool CTLoader::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "TemplateLoader");

	gString gsFile;

	gsFile.Format("%sShips\\Templates\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], "Templates.dat");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsFile);
}
