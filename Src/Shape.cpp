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

// Class    :: CShape, CFrame, CHull, CRect
// Header   :: OTools.h
// Function :: Handles the object structure for Shapes and Rectangles which are used to define the size
//			   and shape of objects.

#pragma warning(disable: 4786)

#include <vector>
#include "OTools.h"
#include "GameObjects.h"
#include "Player.h"
#include <Math.h>

char* CHull::szHullTypes[] = { "Starfighter", "Small ship", "Midship", "Large/Capship", "SuperCap", NULL };


///////////////////////////////////////////////////////////////////////////////////////////////////////////
// 1. Rectangle Class
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CRect::CRect()
{
	this->m_Location = new CCart;
	this->m_nHeight = -1;
	this->m_nLength = -1;
	this->m_nWidth = -1;
}

CRect::CRect(int width, int height, int length)
{
	this->m_Location = new CCart(-1,-1,-1);
	this->m_nHeight = height;
	this->m_nLength = length;
	this->m_nWidth = width;
}

CRect::~CRect()
{
	this->m_Location = NULL;
	this->m_nHeight = 0;
	this->m_nLength = 0;
	this->m_nWidth = 0;
}

bool CRect::Within(CRect *pRect)
{
	// Check the combination of cords
	int nfromX = m_Location->x;
	int nfromY = m_Location->y;
	int nfromZ = m_Location->z;
	int ntoX = m_Location->x + m_nHeight;
	int ntoY = m_Location->y + m_nLength;
	int ntoZ = m_Location->z + m_nWidth;

	if ((pRect->m_Location->x >= nfromX && pRect->m_Location->x <= ntoX) &&
		(pRect->m_Location->y >= nfromY && pRect->m_Location->y <= ntoY) &&
		(pRect->m_Location->z >= nfromZ && pRect->m_Location->z <= ntoZ))
		return true;


	return false;
}

bool CRect::Contains(CCart *pCart)
{
	// Check the combination of cords
	int nfromX = m_Location->x;
	int nfromY = m_Location->y;
	int nfromZ = m_Location->z;
	int ntoX = m_Location->x + m_nHeight;
	int ntoY = m_Location->y + m_nLength;
	int ntoZ = m_Location->z + m_nWidth;

	if ((pCart->x >= nfromX && pCart->x <= ntoX) &&
		(pCart->y >= nfromY && pCart->y <= ntoY) &&
		(pCart->z >= nfromZ && pCart->z <= ntoZ))
		return true;


	return false;
}

// Copy a rect set to another rect set...
CRect& CRect::operator = (CRect& rect)
{
	this->m_nHeight = rect.m_nHeight;
	this->m_nLength = rect.m_nLength;
	this->m_nWidth = rect.m_nWidth;
	this->m_Location = rect.m_Location;

	return *this;
}

void CRect::WriteXml(TiXmlNode* pParent)
{
	CTools & Tools = *CGameObjects::Get().Tools();

	TiXmlNode * pRect = Tools.InsertXmlChild(pParent, "Rect");

	Tools.WriteXml(pRect, "height",		m_nHeight);
	Tools.WriteXml(pRect, "length",		m_nLength);
	Tools.WriteXml(pRect, "width",		m_nWidth);

	m_Location->WriteXml(pRect);
	
	return;
}

void CRect::ReadXml(TiXmlNode* pParent)
{
	CTools & Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pRect = pParent->FirstChild("Rect");

	if ( pRect != NULL )
	{
		Tools.ReadXml(pRect, "height",		m_nHeight);
		Tools.ReadXml(pRect, "length",		m_nLength);
		Tools.ReadXml(pRect, "width",		m_nWidth);
		m_Location->ReadXml(pRect);
	}

	return;
}




///////////////////////////////////////////////////////////////////////////////////////////////////////////
// 2. Shape Class
///////////////////////////////////////////////////////////////////////////////////////////////////////////


CShape::CShape()
{
	m_Center = new CCart;
	m_nType = ST_SPHERE;
	m_nWidth = 0;
	m_nRadius = 0;
	m_nHeight = 0;
	m_nLength = 0;
	m_nSize = 0;
}

CShape::~CShape()
{
	delete m_Center;
	m_Center = NULL;

	m_nType = 0;
	m_nWidth = 0;
	m_nRadius = 0;
	m_nHeight = 0;
	m_nLength = 0;
	m_nSize = 0;
}

// Copy constructor...
CShape& CShape::operator = (CShape& shape)
{
	m_nType = shape.m_nType;
	m_nWidth = shape.m_nWidth;
	m_nRadius = shape.m_nRadius;
	m_nHeight = shape.m_nHeight;
	m_nLength = shape.m_nLength;
	m_nSize = shape.m_nSize;

	m_Center->x = shape.m_Center->x;
	m_Center->y = shape.m_Center->y;
	m_Center->z = shape.m_Center->z;

	return *this;
}

bool CShape::Contains(CCart *pCart)
{

	return false;
}

int CShape::Distance(CCart *pCart)
{

	return 0;
}

// Debugged   :: Chaz Van Den Born <chaz#sw-erp.org>
CArea* CShape::Generate(CShip* pShip)
{
	// Get the Current GameWorld
	CGameWorld* pGame = CGameObjects::Get().GameWorld();

	// Create the Area
	CArea* pArea = new CArea();

	if (!pShip)
		return NULL;

	// Concurrency, this value could change during the course of this command
	int nId = uiUniqueAreaID;

	pArea->SetShip("Template");
	pArea->SetName(pShip->m_gsType);
	pArea->SetFileName(pShip->m_gsType + ".are");
	pArea->SetArea(nId);
	pArea->SetWorld(pGame);

	// Now we need to create the rooms for each HullCube
	for (FrameList::iterator fra = pShip->m_Frames.begin(); fra != pShip->m_Frames.end(); fra++)
	{
		// For each HullCube we need to create a set of rooms
		for (HullList::iterator hull = (*fra)->m_HullCubes.begin(); hull != (*fra)->m_HullCubes.end(); hull++)
		{
			// We create Width x Length rooms
			for (int i = 0; i < ((*hull)->m_nWidth * (*hull)->m_nLength); i++)
			{
				if (i == 0)
					(*hull)->m_nloV = pArea->Rooms()->size();

				// Create the new room
				CRoom* pNew = new CRoom();

				// Setup the room
				pNew->SetArea(nId);

				// Give the room a default name of its Vnum and the Hull Cube name
				gString gsName;
				gsName.Format("[%2d] %s", pArea->Rooms()->size(), (*hull)->m_gsName);
				pNew->SetName(gsName);
				pNew->SetVnum(pArea->Rooms()->size());
				pNew->SetWorld(pGame->GUID());

				// Add this new room to our area
				pArea->Rooms()->insert(RoomMap::value_type(pNew->Vnum(), pNew));
			}

			(*hull)->m_nhiV = pArea->Rooms()->size() - 1;

			// Now we need to link these rooms up to each other
			// We can do this easily using the following:
			// Vnum == X:
			//         X links SOUTH to X + Width [If X + Width < Rooms()->Size()]
			//         X links EAST to X + 1 [If (X + 1) % Width != 1]
	
			int nMax = (*hull)->m_nWidth;

			int nJ = 0;

			for (int j = (*hull)->m_nloV; j < pArea->Rooms()->size(); j++)
			{
				
				if (j == (*hull)->m_nloV)
					nJ = 0;

				// Get our room
				CRoom* pLink = (*pArea->Rooms()->find(j)).second;
				
				// Get our NORTH linking room
				if ( (nJ+nMax) < (1 + (*hull)->m_nhiV - (*hull)->m_nloV))
				{
					CRoom* pNorth = (*pArea->Rooms()->find(j+nMax)).second;

					// Check we got one
					if (pNorth)
					{
						// Create the Exits
						CExit* pESouth = new CExit();
						CExit* pENorth = new CExit();
						pESouth->m_Destination = CPlacement(nId, pLink->Vnum(), pGame->GUID());
						pESouth->m_Direction = CExit::SOUTH;
						pENorth->m_Destination = CPlacement(nId, pNorth->Vnum(), pGame->GUID());
						pENorth->m_Direction = CExit::NORTH;
						pNorth->Exits().push_back(pESouth);
						pLink->Exits().push_back(pENorth);
					}

				}

				// Get our EAST linking room
				if ( (nJ+1) % nMax != 0)
				{
					CRoom* pEast = (*pArea->Rooms()->find(j+1)).second;

					// Check we got one
					if (pEast)
					{
						// Create the Exits
						CExit* pEEast = new CExit();
						CExit* pEWest = new CExit();
						pEEast->m_Destination = CPlacement(nId, pEast->Vnum(), pGame->GUID());
						pEEast->m_Direction = CExit::EAST;
						pEWest->m_Destination = CPlacement(nId, pLink->Vnum(), pGame->GUID());
						pEWest->m_Direction = CExit::WEST;
						pEast->Exits().push_back(pEWest);
						pLink->Exits().push_back(pEEast);
					}

				}
				

				nJ++;

			}

			
		}

		

	}

	// The area needs to be added to the Arealist so we can work with it
	pGame->Areas().push_back(pArea);



	return pArea;
}

int CShape::Length()
{
	return m_nLength;
	/* The Length of the vessel is the maximum z value
	int nMax = 0;

	for (RectList::iterator rect = this->m_Shape.begin(); rect != this->m_Shape.end(); rect++)
	{
		CRect* pRect = *rect;

		if (!pRect)
			return 0;

		if ((pRect->m_Location->z + pRect->m_nLength) > nMax)
			nMax = pRect->m_Location->z + pRect->m_nLength;
	}

	double nNum = 0.0;

	if (nMax >= 1000)
		 nNum = 100.0;
	else
		nNum = 10.0;

	double nM = nMax / nNum;

	return floor(nM + 0.5) * nNum; */
}

int CShape::Width()
{
	return m_nWidth;

	/* The Width of the vessel is the maximum x value
	int nMax = 0;

	for (RectList::iterator rect = this->m_Shape.begin(); rect != this->m_Shape.end(); rect++)
	{
		CRect* pRect = *rect;

		if (!pRect)
			return 0;

		if ((pRect->m_Location->x + pRect->m_nWidth) > nMax)
			nMax = pRect->m_Location->x + pRect->m_nWidth;

	}

	double nNum = 0.0;

	if (nMax >= 1000)
		 nNum = 100.0;
	else
		nNum = 10.0;

	double nM = nMax / nNum;
	return floor(nM + 0.5) * nNum; */
}

int CShape::Height()
{
	return m_nHeight;
	/* The Height of the vessel is the maximum y value
	int nMax = 0;

	for (RectList::iterator rect = this->m_Shape.begin(); rect != this->m_Shape.end(); rect++)
	{
		CRect* pRect = *rect;

		if (!pRect)
			return 0;

		if ((pRect->m_Location->y + pRect->m_nHeight) > nMax)
			nMax = pRect->m_Location->y + pRect->m_nHeight;
	

	}

	double nNum = 0.0;

	if (nMax >= 1000)
		 nNum = 100.0;
	else
		nNum = 10.0;

	double nM = nMax / nNum;
	return floor(nM + 0.5) * nNum; */
}

int CShape::Size()
{
	return m_nSize;
}

bool CShape::SetLength(CShip* pShip)
{
	// The Length of the vessel is the maximum z value
	int nMax = 0;

	if (!pShip)
		return 0;

	for (FrameList::iterator fra = pShip->m_Frames.begin(); fra != pShip->m_Frames.end(); fra++)
	{
		for (HullList::iterator hull = (*fra)->m_HullCubes.begin(); hull != (*fra)->m_HullCubes.end(); hull++)
		{
			CRect* pRect = NULL;

			if ((*hull)->m_EDimension)
				pRect= (*hull)->m_EDimension;
			else
				continue;

			// The Size of the vessel is the sqrt of the volume divided by the sqrt of the length

			if (!pRect)
				return 0;

			if ((pRect->m_Location->z + pRect->m_nLength) > nMax)
				nMax = pRect->m_Location->z + pRect->m_nLength;
		}

	}

	m_nLength = nMax;
	
	return true;
}

bool CShape::SetWidth(CShip* pShip)
{
	// The Width of the vessel is the maximum x value
	int nMax = 0;

	if (!pShip)
		return 0;

	for (FrameList::iterator fra = pShip->m_Frames.begin(); fra != pShip->m_Frames.end(); fra++)
	{
		for (HullList::iterator hull = (*fra)->m_HullCubes.begin(); hull != (*fra)->m_HullCubes.end(); hull++)
		{
			CRect* pRect = NULL;

			if ((*hull)->m_EDimension)
				pRect= (*hull)->m_EDimension;
			else
				continue;

			// The Size of the vessel is the sqrt of the volume divided by the sqrt of the length

			if (!pRect)
				return 0;

			if ((pRect->m_Location->x + pRect->m_nWidth) > nMax)
				nMax = pRect->m_Location->x + pRect->m_nWidth;
		}

	}

	m_nWidth =  nMax;

	return true;
}

bool CShape::SetHeight(CShip* pShip)
{
	// The Height of the vessel is the maximum y value
	int nMax = 0;

	if (!pShip)
		return 0;

	for (FrameList::iterator fra = pShip->m_Frames.begin(); fra != pShip->m_Frames.end(); fra++)
	{
		for (HullList::iterator hull = (*fra)->m_HullCubes.begin(); hull != (*fra)->m_HullCubes.end(); hull++)
		{
			CRect* pRect = NULL;

			if ((*hull)->m_EDimension)
				pRect= (*hull)->m_EDimension;
			else
				continue;

			if (!pRect)
				return 0;

			if ((pRect->m_Location->y + pRect->m_nHeight) > nMax)
				nMax = pRect->m_Location->y + pRect->m_nHeight;
		}

	}

	m_nHeight = nMax;

	return true;
}

bool CShape::SetSize(CShip* pShip)
{

	int nVolume = 0;
	
	int nMod = this->Length();

	for (FrameList::iterator fra = pShip->m_Frames.begin(); fra != pShip->m_Frames.end(); fra++)
	{
		for (HullList::iterator hull = (*fra)->m_HullCubes.begin(); hull != (*fra)->m_HullCubes.end(); hull++)
		{
			CRect* pRect = NULL;

			if ((*hull)->m_EDimension)
				pRect= (*hull)->m_EDimension;
			else
				continue;

			// The Size of the vessel is the sqrt of the volume divided by the sqrt of the length

			if (!pRect)
				return 0;

			nVolume += (pRect->m_nHeight * pRect->m_nLength * pRect->m_nWidth);
		}
	}

	m_nSize = sqrt((float)nVolume)/sqrt((float)nMod);

	return true;

}


void CShape::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pShapeNode = Tools.InsertXmlChild(pParent, "Shape");

	Tools.WriteXml(pShapeNode, "type",			m_nType);
	Tools.WriteXml(pShapeNode, "size",			m_nSize);

	if (m_Center)
		m_Center->WriteXml(pShapeNode);

	if (m_nType == CShape::ST_SPHERE)
		Tools.WriteXml(pShapeNode, "radius",	m_nRadius);
	else
	{
		Tools.WriteXml(pShapeNode, "width",		m_nWidth);
		Tools.WriteXml(pShapeNode, "height",	m_nHeight);
		Tools.WriteXml(pShapeNode, "length",	m_nLength);
	}

	return;
}

void CShape::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pShapeNode = pParent->FirstChild("Shape");

	if (pShapeNode != NULL)
	{	
		Tools.ReadXml(pShapeNode, "type",			m_nType);
		Tools.ReadXml(pShapeNode, "size",			m_nSize);

		if (m_Center)
			m_Center->ReadXml(pShapeNode);

		if (m_nType == CShape::ST_SPHERE)
			Tools.ReadXml(pShapeNode, "radius",		m_nRadius);
		else
		{
			Tools.ReadXml(pShapeNode, "width",		m_nWidth);
			Tools.ReadXml(pShapeNode, "height",		m_nHeight);
			Tools.ReadXml(pShapeNode, "length",		m_nLength);
		}
	}

	return;

}

///////////////////////////////////////////////////////////////////////////////////////////////////////////
// 4. Frame Class
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CFrame::CFrame()
{
	this->m_gsName = "";
}

CFrame::~CFrame()
{
	this->m_HullCubes.clear();
	this->m_gsName = "";
}

// Copy a frame set to another frame set...
CFrame& CFrame::operator = (CFrame& frame)
{
	this->m_gsName = frame.m_gsName;

	for (HullList::iterator hull = frame.m_HullCubes.begin(); hull != frame.m_HullCubes.end(); hull++)
	{
		CHull* pHull = new CHull;
		*pHull = *(*hull);
		this->m_HullCubes.push_back(pHull);
	}

	return *this;
}

int CFrame::Mass()
{
	int nCount = 0;

	for (HullList::iterator hull = m_HullCubes.begin(); hull != m_HullCubes.end(); hull++)
		nCount += (*hull)->m_nMass;

	return nCount;
}

void CFrame::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pFrameNode = Tools.InsertXmlChild(pParent, "Frame");

	Tools.WriteXml(pFrameNode, "name",		m_gsName);

	// Write the HullCubes as well
	for (HullList::iterator hull = m_HullCubes.begin(); hull != m_HullCubes.end(); hull++)
	{
		// Insert Hull Node
		TiXmlNode* pHullNode = Tools.InsertXmlChild(pFrameNode, "HullCube");

		// Save the filename and unqiue fields
		Tools.WriteXml(pHullNode, "filename",	(*hull)->m_gsFileName);	
		
		(*hull)->WriteExtra(pHullNode);

	}

	return;
}

void CFrame::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",		m_gsName);

	// Read in all the frames hullcubes
	TiXmlNode* pHullNode = pParent->FirstChild("HullCube");
	while (pHullNode != NULL)
	{
		gString gsFile;
		Tools.ReadXml(pHullNode, "filename", gsFile);

		CHull* pHull = new CHull;
		pHull->Load(gsFile);

		pHull->ReadExtra(pHullNode);
		
		m_HullCubes.push_back(pHull);
		pHullNode = pHullNode->NextSibling();
	}

	return;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////
// 5. Hull Cube + Loader Class
///////////////////////////////////////////////////////////////////////////////////////////////////////////

CHLoader::CHLoader()
{

}

CHLoader::~CHLoader()
{
	this->m_HullCubes.clear();
}

void CHLoader::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pCubeNode = pParent->FirstChild("HullCube");

	while (pCubeNode != NULL)
	{
		gString gsFile;

		Tools.ReadXml(pCubeNode, "filename",	gsFile);

		CHull* pHull = new CHull;

		pHull->Load(gsFile);

		// Add it
		m_HullCubes.push_back(pHull);
		
		pCubeNode = pCubeNode->NextSibling("HullCube");
	}

	return;
}

void CHLoader::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	for (HullList::iterator hull = m_HullCubes.begin(); hull != m_HullCubes.end(); hull++)
	{
		TiXmlNode* pCubeNode = Tools.InsertXmlChild(pParent, "HullCube");

		Tools.WriteXml(pCubeNode, "filename",	(*hull)->m_gsFileName);
	}

	return;
}

bool CHLoader::Load(gFileName gfFile)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gfFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("HullCubeLoader");

	ReadXml(pNode);
	return true;
}

bool CHLoader::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "HullCubeLoader");

	gString gsFile;

	gsFile.Format("%sShips\\HullCubes\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], "HullCubes.dat");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsFile);

	return true;
}
// End HullCube Loader



CHull::CHull()
{
	this->m_gsName = "";
	this->m_gsFileName = "";

	this->m_Armour = NULL;
	this->m_EDimension = new CRect();
	this->m_Cover = new CSet;

	this->m_nHeight = 0;
	this->m_nLength = 0;
	this->m_nWidth = 0;
	this->m_nhiV = 0;
	this->m_nloV = 0;
	this->m_nType = 0;
	this->m_nResilience = 0;
	this->m_nMass = 0;
	this->m_nCKeel = 0;
	this->m_nMKeel = 0;


}

CHull::~CHull()
{
	this->m_Components.clear();
	this->m_gsName = "";
	this->m_gsFileName = "";

	this->m_Armour = NULL;
	delete this->m_EDimension;
	this->m_EDimension = NULL;
	delete this->m_Cover;
	this->m_Cover = NULL;	

	this->m_nHeight = 0;
	this->m_nLength = 0;
	this->m_nWidth = 0;
	this->m_nhiV = 0;
	this->m_nType = 0;
	this->m_nResilience = 0;
	this->m_nMass = 0;
	this->m_nloV = 0;
	this->m_nCKeel = 0;
	this->m_nMKeel = 0;

}

bool CHull::Load(gString filename)
{	
	TiXmlDocument doc;

	filename.Format("%sShips\\HullCubes\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], filename);

	if ( !doc.LoadFile((const char*)filename) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("HullCube");

	ReadXml(pNode);
	return true;

}

bool CHull::Load()
{	
	TiXmlDocument doc;

	gString filename;

	filename.Format("%sShips\\HullCubes\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gsFileName);

	if ( !doc.LoadFile((const char*)filename) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("HullCube");

	ReadXml(pNode);
	return true;
}

bool CHull::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "HullCube");

	gString gMod;

	gMod.Format("%s%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_HULLCUBES], this->m_gsFileName);

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gMod);

	return true;
}

// Copy a hull's components to another hull cube
CHull& CHull::operator = (CHull& hull)
{
	m_gsName = hull.m_gsName;

	if (hull.m_Armour)
	{
		m_Armour = new CModule;
		*m_Armour = *hull.m_Armour;
	}

	if (hull.m_EDimension)
		m_EDimension = hull.m_EDimension;

	this->m_nCKeel = hull.m_nCKeel;
	this->m_nMKeel = hull.m_nMKeel;
	this->m_nloV = hull.m_nloV;
	this->m_nhiV = hull.m_nhiV;
	this->m_nType = hull.m_nType;
	this->m_nResilience = hull.m_nResilience;
	this->m_nMass = hull.m_nMass;
	this->m_nSize = hull.m_nSize;

	if (hull.m_Cover)
		*m_Cover = *hull.m_Cover;

	for (ComponentList::iterator comp = hull.m_Components.begin(); comp != hull.m_Components.end(); comp++)
	{
		CComponent* pComponent = new CComponent;

		switch((*comp)->m_Type)
		{
			case CComponent::CT_SHIELD:
				pComponent = new CShield();
				*(CShield*)pComponent = *(CShield*)(*comp);
				break;
			case CComponent::CT_ENGINEERING:
				pComponent = new CEngspace();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_SUBLIGHT:
				pComponent = new CSublight();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_HYPERDRIVE:
				pComponent = new CHyperdrive();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_MAGAZINE:
				pComponent = new CMagazine();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_EXTERNAL:
				pComponent = new CExternal();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_INTERNAL:
				pComponent = new CInternal();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_LANDING:
				pComponent = new CLanding();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_BULKSTORAGE:
				pComponent = new CBulk();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_CONTROLPOINT:
				pComponent = new CControl();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_WEAPONMOUNT:
				pComponent = new CWeapon();
				*(CWeapon*)pComponent = *(CWeapon*)(*comp);
				break;
			case CComponent::CT_ESCAPEPOD:
				pComponent = new CEscape();
				*pComponent = *(*comp);
				break;
			case CComponent::CT_SFOIL:
				pComponent = new CSFoil();
				*pComponent = *(*comp);
				break;

			default:
				*pComponent = *(*comp);
				break;
		}
		
		
		m_Components.push_back(pComponent);
	}

	return *this;
}

// Function determines the amount of space left in the HullCube, or the amount 
// of space used in the HullCube depending on a boolean input
int CHull::Size(bool bRemaining)
{
	int nCurrentSize = 0;

	for (ComponentList::iterator comp = m_Components.begin(); comp != m_Components.end(); comp++)
	{
		for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
		{
			nCurrentSize += (*mod).second->m_nSize;
		}
	}
	
	if (bRemaining)
		return (m_nSize - nCurrentSize);
	else
		return nCurrentSize;

}
gString CHull::Gui()
{
	gString gsDesc = "\n\r";
	gsDesc += "-";

	// Generate GUI
	for (int x = 0; x < this->m_nWidth; x++)
	{
		gsDesc += "----";
	}

	gsDesc += "\n\r";

	/*
	int nSize = this->m_nWidth * this->m_nLength;
	int nStatSize = this->m_nWidth * this->m_nLength; */

	int nSize = this->m_nhiV + 1;
	int nRun = this->m_nhiV + 1;

	int nWidth = this->m_nWidth;
	int nCounter = this->m_nloV;

	while (nCounter < nRun)
	{
		int y = nSize - nWidth;

		gString gsTemp;
		gsTemp.Format("|%3d", y);
		gsDesc += gsTemp;

		nWidth--;

		if (nWidth <= 0)
		{
			nWidth = this->m_nWidth;
			nSize -= this->m_nWidth;
			gsDesc += "|\n\r";
			gsDesc += "-";
			for (int z = 0; z < this->m_nWidth; z++)
			{
				gsDesc += "----";
			}
			gsDesc += "\n\r";
		}			

		nCounter++;
	}

	return gsDesc;
}


// The Damage function works on two different levels. For internal explosions (Such as Reactors going critical,
// Hyperdrives melting down, Sublight engines Bearings being destroyed, etc) the armour is not used to absorb some
// of the damage. For any external hits the armour is used to absorb some damage.
int CHull::Damage(int nTotal, bool bInternal, CShip* pShip)
{
	int nRem;
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	int nNumCritical = 0;
	int nCriticalDam = 0;
	
	// [1] >> Armour
	// Does it have armour, if it does we need to damage this first
	if (this->m_Armour && !bInternal && this->m_Armour->m_ncDurability > 0)
	{
		nRem = this->m_Armour->Damage(nTotal);

		// All damage absorbed by the Armour
		if (nRem == 0)
			return 0;
	}
	else
	{
		nRem = nTotal;
	}
	
	// The shot either punched through the Armour or was Internal
	nCriticalDam = nRem;

	// [2] >> Keel
	// We need to take this damage onto the Keel of the ship
	if (this->m_nCKeel > 0)
	{
		// We want to damage this keel now, need to
		// check if this keel will absorb all the damage
		int nTempRem = nRem - this->m_nCKeel;	// The damage we have minus the keel integrity
											    // tells us if their will be any damage left

		this->m_nCKeel -= nRem;	// Damage the Keel

		// If there was damage left we need to copy it back to nRem
		if (nTempRem > 0)
			nRem = nTempRem;
		else
			nRem = 0;

		if (this->m_nCKeel > 0)
		{
			// Did we get a critical hit?
			// For every 5 over 75 we get a critical hit
			int nResult = abs((int)pRandom->NumberRange(1, 100) - 70);

			// The damage dealt be a critical hit

			if (nResult <= 0)
				nResult = 0;

			nNumCritical = nResult/5;	
		}
		else
		{
			this->m_nCKeel = -1; // Keel completely destroyed

			// A destroyed hullcube creates a random piece of space debris
			gString gsTemp;
			pShip->Write(CShip::MT_SYSTEMS, "#100[#101Damage Report#100]#700 %s completely destroyed!\n\r", this->m_gsName);
			CDebris* pDebris = new CDebris();
			gsTemp.Format("A collection of debris from the %s of %s.", this->m_gsName, pShip->m_gsDescription);
			pDebris->m_gsDescription = gsTemp;				// Set the description
			gsTemp.Format("Ship Debris");
			pDebris->m_gsName = gsTemp;						// Set the name
			pDebris->m_Location = pShip->m_Location;		// Set the debris location
			pDebris->m_bExplode = false;					// It won't explode
			pDebris->m_nLife = 10000;						// Set its life #TODO# Sort a decent size out for this
			pDebris->m_gsSector = pShip->m_gsSector;		// Set the sector of the Debris
			pGalaxy->AddSpatial(pDebris);
			pGalaxy->AddSpatialToSpace(pDebris->m_Vnum);

			return nRem;			
		}

		// If there are no-critical hits we are done here
		if (nNumCritical <= 0)
			return nRem;
		
	}

	// If we have made a critical hit on this hullcube then we need to give a damage message to
	// all players within that hullcube and also make them take a balance check.
	// If they fail the balance check they will be either given a EKnock or EProne event.
	if (nNumCritical > 0)
	{
		// Iterate through all rooms within the HullCube
		std::list<CRoom*>RoomList;

		// We need to work through all rooms in the hullcube
		for (int i = this->m_nloV; i <= this->m_nhiV; i++)
		{
			CRoom* pRoom = pShip->m_Area->GetRoom(i);

			// Only interested in rooms that are occupied for giving messages
			if (pRoom && pRoom->Actors().size() > 0)
				RoomList.push_back(pRoom);
		}

		// Now we need to give the message to these rooms
		if (RoomList.size() > 0)
		{
			gString gsMsg;
			
			switch (pRandom->NumberRange(1,5))
			{
			case 1: gsMsg = "#101A massive explosion rocks the ship!#700"; 
				break;
			case 2: gsMsg = "#101The ship violently shakes to the deafening sound of an explosion.#700"; 
				break;
			case 3: gsMsg = "#101An enormous bang cracks through the ship as it rocks about fiercly!#700"; 
				break;
			case 4: gsMsg = "#101The entire ship shifts violently under the brutual force of an explosion.#700"; 
				break;
			case 5: gsMsg = "#101The ship is thrown about violently as it suffers a massive blast!#700"; 
				break;
			default: gsMsg = "#101A massive explosion rocks the ship!#700"; 
				break;
			}


			while (RoomList.size() > 0)
			{
				CRoom *pRoom = *RoomList.begin();
			
				if (pRoom)
					pRoom->Write("%s\n\r", gsMsg);

				// Make each player take a balance check
				#pragma message (Reminder "[CHullCube::Damage] Balance check needs to be modified to use attributes")
				int nResult = pRandom->NumberRange(1, 16);

				for (ActorMap::iterator act = pRoom->Actors().begin(); act != pRoom->Actors().end(); act++)
				{
					CActor* pVictim = (*act).second;

					// We are only interested in standing players
					if (pVictim->ActorPositions()->IsSet(CActor::_STANDING))
					{
						// DC 8 means they fall to their knees
						if (nResult < 8 && nResult > 4)
						{
							pVictim->Write("The force of the explosion knocks you to your knees!\n\r");
							pVictim->CurrentRoom()->Write(pVictim, "%s is knocked to their knees by the force of the explosion!\n\r", pVictim->Name());
							pVictim->ActorPositions()->RemoveBit(CActor::_STANDING);
							pVictim->ActorPositions()->SetBit(CActor::_CROUCHED);
						}
						// DC 4 means they are knocked to the ground
						if (nResult <= 4)
						{
							pVictim->Write("You are thrown to the ground by the force of the explosion!\n\r");
							pVictim->CurrentRoom()->Write(pVictim, "%s blasted to the ground by the force of the explosion!\n\r", pVictim->Name());
							pVictim->ActorPositions()->RemoveBit(CActor::_STANDING);
							pVictim->ActorPositions()->SetBit(CActor::_PRONE);
						}
					}
				}

				// Once we have given the message we can remove the room from the list
				RoomList.pop_front();
			}



		}
		

	}


	while (nNumCritical > 0)
	{
		// We dont want to simply work through a list as we need a random element. So we will create
		// an array of index's to components and work through that instead

		pShip->Write(CShip::MT_CRITICAL, "#100>#101>#700 Critical hit!\n\r");

		int nSize = this->m_Components.size();
		
		if (nSize == 0)
			return nRem;
		
		// We keep assigning damage till we have none left
		// 
		while(nCriticalDam > 0)
		{
			CRandom* pRandom = CGameObjects::Get().Rand();
			
			// See if this Component absorbed all the damage
			int nDamage = this->m_Components.at(pRandom->NumberRange(0, (this->m_Components.size() - 1)))->Damage(nCriticalDam);

			// It absorbed all the damage
			if (nDamage == 0)
				break;

			// We need a check here to makesure there are still components left to damage
			// if not then we need to break the loop
			bool bBreak = true;
			for (ComponentList::iterator comp = this->m_Components.begin(); comp != this->m_Components.end(); comp++)
			for (ModuleMap::iterator mod = (*comp)->m_Modules.begin(); mod != (*comp)->m_Modules.end(); mod++)
			{
				if ( ((*mod).second)->m_ncDurability > 0 )
				{
					bBreak = false;
					break;
				}
			}
			
			// We still have more damage to deal out to the next Component
			nCriticalDam = nDamage;

			if (bBreak)
				break;
		}

		nNumCritical--;
	}

	// If we have completely destroyed this HullCube and all its Components
	// then we deal the damage out to another HullCube
	return nRem;
}

// Write XML is used to write the static values of a HullCube to an
// XML file. If the HullCube has been installed within a frame then the
// WriteExtra method is to be used
void CHull::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",			m_gsName);
	Tools.WriteXml(pParent, "filename",		m_gsFileName);
	Tools.WriteXml(pParent, "resilience",	m_nResilience);
	Tools.WriteXml(pParent, "size",			m_nSize);
	Tools.WriteXml(pParent, "type",			m_nType);
	Tools.WriteXml(pParent, "mass",			m_nMass);
	Tools.WriteXml(pParent, "width",		m_nWidth);
	Tools.WriteXml(pParent, "height",		m_nHeight);
	Tools.WriteXml(pParent, "length",		m_nLength);
	Tools.WriteXml(pParent, "currkeel",		m_nCKeel);
	Tools.WriteXml(pParent, "maxkeel",		m_nMKeel);
	Tools.WriteXml(pParent, "hiv",			m_nhiV);
	Tools.WriteXml(pParent, "lov",			m_nloV);
	Tools.WriteXml(pParent, "cover",		*m_Cover);

	if (m_Armour)
		m_Armour->WriteXml(pParent);

	if (m_EDimension)
		m_EDimension->WriteXml(pParent);

	for (ComponentList::iterator comp = m_Components.begin(); comp != m_Components.end(); comp++)
	{
		const gString gsComp = CComponent::szClassnames[(*comp)->m_Type];

		TiXmlNode* pCompNode = Tools.InsertXmlChild(pParent, gsComp);
		(*comp)->WriteXml(pCompNode);
	}

	return;
}

// Writes Extra details for the HullCube, these include the dynamic
// fields which change when a hullcube is installed in a ship
void CHull::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "integrity",	m_nCKeel);
	Tools.WriteXml(pParent, "width",		m_nWidth);
	Tools.WriteXml(pParent, "height",		m_nHeight);
	Tools.WriteXml(pParent, "length",		m_nLength);
	Tools.WriteXml(pParent, "extwidth",		m_EDimension->m_nWidth);
	Tools.WriteXml(pParent, "extheight",	m_EDimension->m_nHeight);
	Tools.WriteXml(pParent, "extlength",	m_EDimension->m_nLength);
	Tools.WriteXml(pParent, "hiv",			m_nhiV);
	Tools.WriteXml(pParent, "lov",			m_nloV);
	Tools.WriteXml(pParent, "cover",		*m_Cover);
	
	// Output all components
	for (ComponentList::iterator comp = m_Components.begin(); comp != m_Components.end(); comp++)
	{
		const gString gsComp = CComponent::szClassnames[(*comp)->m_Type];

		TiXmlNode* pCompNode = Tools.InsertXmlChild(pParent, gsComp);
		(*comp)->WriteXml(pCompNode);
	}
	return;
}

// This method for reading XML is only used to read in
// the basic HullCube details.
void CHull::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "filename",		m_gsFileName);
	Tools.ReadXml(pParent, "resilience",	m_nResilience);
	Tools.ReadXml(pParent, "type",			m_nType);
	Tools.ReadXml(pParent, "size",			m_nSize);
	Tools.ReadXml(pParent, "mass",			m_nMass);
	Tools.ReadXml(pParent, "width",			m_nWidth);
	Tools.ReadXml(pParent, "height",		m_nHeight);
	Tools.ReadXml(pParent, "length",		m_nLength);
	Tools.ReadXml(pParent, "currkeel",		m_nCKeel);
	Tools.ReadXml(pParent, "maxkeel",		m_nMKeel);
	Tools.ReadXml(pParent, "hiv",			m_nhiV);
	Tools.ReadXml(pParent, "lov",			m_nloV);
	Tools.ReadXml(pParent, "cover",			*m_Cover);

	TiXmlNode* pArmourNode = pParent->FirstChild("Module");

	if (pArmourNode != NULL)
	{
		m_Armour->ReadXml(pParent);
	}

	m_EDimension->ReadXml(pParent);

	return;
}

void CHull::ReadExtra(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in unique fields
	Tools.ReadXml(pParent, "integrity",	m_nCKeel);
	Tools.ReadXml(pParent, "width",		m_nWidth);
	Tools.ReadXml(pParent, "height",		m_nHeight);
	Tools.ReadXml(pParent, "length",		m_nLength);
	Tools.ReadXml(pParent, "extwidth",	m_EDimension->m_nWidth);
	Tools.ReadXml(pParent, "extheight",	m_EDimension->m_nHeight);
	Tools.ReadXml(pParent, "extlength",	m_EDimension->m_nLength);
	Tools.ReadXml(pParent, "hiv",			m_nhiV);
	Tools.ReadXml(pParent, "lov",			m_nloV);
	Tools.ReadXml(pParent, "cover",		*m_Cover);

	// Now check for stored components
	for (int i = 0; i < CComponent::CT_LAST; i++)
	{
		const gString gsComp = CComponent::szClassnames[i];
		TiXmlNode* pCompNode = pParent->FirstChild(gsComp);
		while (pCompNode != NULL)
		{
			CComponent* pComponent;

			switch (i)
			{
				case CComponent::CT_SHIELD:
					pComponent = new CShield();
					break;
				case CComponent::CT_ENGINEERING:
					pComponent = new CEngspace();
					break;
				case CComponent::CT_SUBLIGHT:
					pComponent = new CSublight();
					break;
				case CComponent::CT_HYPERDRIVE:
					pComponent = new CHyperdrive();
					break;
				case CComponent::CT_MAGAZINE:
					pComponent = new CMagazine();
					break;
				case CComponent::CT_EXTERNAL:
					pComponent = new CExternal();
					break;
				case CComponent::CT_INTERNAL:
					pComponent = new CInternal();
					break;
				case CComponent::CT_LANDING:
					pComponent = new CLanding();
					break;
				case CComponent::CT_BULKSTORAGE:
					pComponent = new CBulk();
					break;
				case CComponent::CT_CONTROLPOINT:
					pComponent = new CControl();
					break;
				case CComponent::CT_WEAPONMOUNT:
					pComponent = new CWeapon();
					break;
				case CComponent::CT_ESCAPEPOD:
					pComponent = new CEscape();
					break;
				case CComponent::CT_SFOIL:
					pComponent = new CSFoil();
					break;
				default:
					break;
			}

			pComponent->ReadXml(pCompNode);

			m_Components.push_back(pComponent);
			
			pCompNode = pCompNode->NextSibling(gsComp);

		}		
	}

	return;
}
