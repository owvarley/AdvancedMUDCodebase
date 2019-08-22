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

// Class    :: CGalaxy, CSector
// Header   :: Space.h
// Function :: Implements all classes for Galaxies and Sectors within the space system

#pragma warning(disable:4786)


#include "GameObjects.h"
#include "Tools.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Space.h"
#include "Spatial.h"
#include "OTools.h"
#include "../gTools/Log.h"
#include "Player.h"

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Galaxy Class
///////////////////////////////////////////////////////////////////////////////////////////
CGalaxy::CGalaxy()
{

}

CGalaxy::~CGalaxy()
{
	m_gSpatialMap.clear();
	m_SpatialMap.clear();
	m_ShipList.clear();
	m_SectorList.clear();
}


///////////////////////////////////////////////////////////////////////////////////////////
// Global Spatial Map
///////////////////////////////////////////////////////////////////////////////////////////
// The four methods below are used for accessing objects stored within our global map
// this map contains ALL spatial objects and contains pointers to their locations within 
// memory. All other lists and functions use the objects contained within this map.
///////////////////////////////////////////////////////////////////////////////////////////
// AddSpatial | RemoveSpatial | GetSpa (name) | GetSpa (vnum)
///////////////////////////////////////////////////////////////////////////////////////////

// Method     :: AddSpatial
// Class	  :: CGalaxy
// Parameters :: Spatial object to Add
// Return     :: <none>
// Function   :: Adds a Spatial object to the Global Spatial Map
// Rewritten  :: 22/02/2006 {OWV}

void CGalaxy::AddSpatial(CSpatial* pSpatial)
{
	LOG_SCOPE("CGalaxy::AddSpatial");
	SpatialList pList;
	SpatialMap::iterator spa;

	if (!pSpatial->m_Vnum)
	{
		g_Log.Log(LOG_ERROR, "Spatial Object: %s has no Vnum.", pSpatial->m_gsName);
		return;
	}

	// We add a pointer to the object to our global map
	this->m_gSpatialMap.insert(gSpatialMap::value_type(pSpatial->m_Vnum->m_nVnum, pSpatial));

	return;
}

// Method     :: RemoveSpatial
// Class	  :: CGalaxy
// Parameters :: Spatial object  to Delete
// Return     :: <none>
// Function   :: Deletes a Spatial object from the Global map
// Rewritten  :: 22/02/2006 {OWV}

bool CGalaxy::RemoveSpatial(CSpatial* pDelete)
{
	LOG_SCOPE("CGalaxy::RemoveSpatial");

	if (!pDelete->m_Vnum)
	{
		g_Log.Log(LOG_ERROR, "Spatial Object: %s has no Vnum.", pDelete->m_gsName);
		return false;
	}

	// We need to carry out two further checks to ensure we have 
	// removed all references of these objects

	// [1] Check our In Space Map
	this->RemoveSpatialFromSpace(pDelete->m_Vnum);

	// [2] If this object is a ship we check the ship List
	if (pDelete->m_nType == CSpatial::SO_SHIP)
	{
		this->RemoveShip(pDelete->m_Vnum);
		// Flag its area for deletion
		if (((CShip*)pDelete)->m_Area)
			((CShip*)pDelete)->m_Area->Delete();
	}


	// This is the only pointer we have to the object hence we must delete
	// this object now
	gSpatialMap::iterator del = m_gSpatialMap.find(pDelete->m_Vnum->m_nVnum);

	CSpatial* pToDelete;

	if (del != m_gSpatialMap.end())
		pToDelete = (*del).second;

	// Now we remove the Object from our Global List
	this->m_gSpatialMap.erase(pDelete->m_Vnum->m_nVnum);

	delete pToDelete;
	pToDelete = NULL;

	// Save these changes
	this->Save();

	return true;

}

// Method     :: Get
// Class	  :: CGalaxy
// Parameters :: Spatial Object to get
// Return     :: <none>
// Function   :: Returns a pointer to an existing Spatial Object from the Global Spatial Map
// Rewritten  :: 22/02/2006 {OWV}

CSpatial* CGalaxy::GetSpa(gString gsSpaName)
{
	for (gSpatialMap::iterator pos = m_gSpatialMap.begin(); pos != m_gSpatialMap.end(); pos++)	
	{
		CSpatial* pSpatial = (*pos).second;
		if (pSpatial->m_gsName == gsSpaName || pSpatial->m_gsDescription == gsSpaName)
		{
			return pSpatial;
		}
	}

	return NULL;

}

// Method     :: Get
// Class	  :: CGalaxy
// Parameters :: Spatial Object to get
// Return     :: <none>
// Function   :: Returns a pointer to an existing Spatial Object from the Global Spatial Map
// Rewritten  :: 22/02/2006 {OWV}

CSpatial* CGalaxy::GetSpa(int nVnum)
{
	gSpatialMap::iterator find = this->m_gSpatialMap.find(nVnum);

	if (find == m_gSpatialMap.end())
		return NULL;

	return (*find).second;

}

// End >> Global Spatial Map

///////////////////////////////////////////////////////////////////////////////////////////
// Spatial Map
///////////////////////////////////////////////////////////////////////////////////////////
// The Spatial Map contains objects that are currently occupying Space. This includes Spatial
// objects that are considered static and cannot be launched or landed, it will also contain
// a list of all ships that are currently navigating space
///////////////////////////////////////////////////////////////////////////////////////////
// AddSpatialToSpace | RemoveSpatialFromSpace | GetSpaInSpace (name) | GetSpaInSpace (vnum)
///////////////////////////////////////////////////////////////////////////////////////////

// Method     :: AddSpatialToSpace
// Class	  :: CGalaxy
// Parameters :: Spatial object to Add
// Return     :: <none>
// Function   :: Adds a Spatial object to the Global Spatial Map
// Written    :: 22/02/2006 {OWV}

void CGalaxy::AddSpatialToSpace(CSpatialID* pAdd)
{
	LOG_SCOPE("CGalaxy::AddSpatial");
	SpatialList pList;
	SpatialMap::iterator spa;

	if (!pAdd)
	{
		g_Log.Log(LOG_ERROR, "Spatial Object has no Vnum.");
		return;
	}

	int nCount = this->m_gSpatialMap.size();

	gSpatialMap::iterator found = this->m_gSpatialMap.find(pAdd->m_nVnum);

	CSpatial* pSpatial = NULL;

	if (found != this->m_gSpatialMap.end())
		pSpatial = (*found).second;
	

	// We have stopped using the overloaded * operator
	// as during startup the Galaxy object is not initialised
	// so we cannot pull our Spatial Objects from it
	// pSpatial = (*(*pAdd));

	if (!pSpatial)
	{
		g_Log.Log(LOG_ERROR, "Trying to add a Spatial Object to Space that does not exist in the Global Map.");
		return;
	}

	// We didnt find this Sector, so we need to add it
	if ((spa = this->m_SpatialMap.find(pSpatial->m_gsSector)) == this->m_SpatialMap.end())
	{
		pList.push_back(pAdd);
		this->m_SpatialMap.insert(SpatialMap::value_type(pSpatial->m_gsSector, pList));
	}
	else
	{
		// This sector is already in the map so 
		// We need to add this Spatial Object to the back of the List
		((*spa).second).push_back(pAdd);
	}

}

// Method     :: RemoveSpatialFromSpace
// Class	  :: CGalaxy
// Parameters :: Spatial object  to Delete
// Return     :: <none>
// Function   :: Deletes a Spatial object from the Global map
// Written    :: 22/02/2006 {OWV}

bool CGalaxy::RemoveSpatialFromSpace(CSpatialID* pDelete)
{
	LOG_SCOPE("CGalaxy::RemoveSpatial");

	SpatialMap::iterator spa;

	CSpatial* pSpatial = (*(*pDelete));

	if (!pSpatial)
	{
		g_Log.Log(LOG_ERROR, "Trying to Remove a Spatial Object from Space that does not exist in the Global Map.");
		return false;
	}

	// We didnt find the Sector, so pDelete wasn't added properly!
	if ((spa = this->m_SpatialMap.find(pSpatial->m_gsSector)) == this->m_SpatialMap.end())
	{
		return false;
	}
	else
	{

		if (!pSpatial->m_Vnum)
		{
			g_Log.Log(LOG_ERROR, "Spatial Object: %s has no Vnum.", pSpatial->m_gsName);
			return false;
		}

		// We found the sector so now we just need to remove the Spatial from the List
		for (SpatialList::iterator ship = ((*spa).second).begin(); ship != ((*spa).second).end(); ship++)
		{
			CSpatial * pSpatial = (*(*(*ship)));

			// If we can't find this Spatial Object then we can only assume its been
			// deleted already, or never existed so we remove it from the list
			if (!pSpatial)
			{
				// Remove it from the list
				((*spa).second).erase(ship);
				return true;
			}

			if ( pSpatial->m_Vnum->m_nVnum == pSpatial->m_Vnum->m_nVnum)
			{
				((*spa).second).erase(ship);
				break;
			}
		
		}

	}

	this->Save();

	return true;

}

// Method     :: GetSpaInSpace
// Class	  :: CGalaxy
// Parameters :: Spatial Object to get
// Return     :: <none>
// Function   :: Returns a pointer to an existing Spatial Object in Space
// Written    :: 22/02/2006 {OWV}

CSpatial* CGalaxy::GetSpaInSpace(gString gsSpaName)
{
	for (SpatialMap::iterator pos = m_SpatialMap.begin(); pos != m_SpatialMap.end(); pos++)	
	for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)	
	{
		CSpatial* pSpatial = (*(*(*spa)));	// We have overrided the * method of CSpatialID

		// There is a chance that this Spatial Object has been deleted within this update
		// cycle so we must check this

		if (!pSpatial)
		{
			// We must remove this object from the list
			(*pos).second.erase(spa);
		}

		if (pSpatial->m_gsName == gsSpaName || pSpatial->m_gsDescription == gsSpaName)
		{
			return pSpatial;
		}

	}

	return NULL;

}

// Method     :: GetSpaInSpace
// Class	  :: CGalaxy
// Parameters :: Spatial Object to get
// Return     :: <none>
// Function   :: Returns a pointer to an existing Spatial Object
// Written    :: 22/02/2006 {OWV}

CSpatial* CGalaxy::GetSpaInSpace(int nVnum)
{
	for (SpatialMap::iterator pos = m_SpatialMap.begin(); pos != m_SpatialMap.end(); pos++)	
	for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)	
	{
		CSpatial* pSpatial = (*(*(*spa)));	// We have overrided the * method of CSpatialID

		// There is a chance that this Spatial Object has been deleted within this update
		// cycle so we must check this

		if (!pSpatial)
		{
			// We must remove this object from the list
			(*pos).second.erase(spa);
		}

		if (pSpatial->m_Vnum && pSpatial->m_Vnum->m_nVnum == nVnum)
		{
			return pSpatial;
		}
	}

	return NULL;

}

// End >> Spatial Map

///////////////////////////////////////////////////////////////////////////////////////////
// Ship List
///////////////////////////////////////////////////////////////////////////////////////////
// To cut down on the Update cycle the Codebase must go through the ShipList will contain a
// list of all Ship objects currently loaded into memory. The List will not hold pointers to
// their memory locations, instead they will hold CSpatialID's linking to the pointers again
// for the sake of encapsulation and to prevent memory leaks
///////////////////////////////////////////////////////////////////////////////////////////
// AddShip | RemoveShip | GetShi (name)
///////////////////////////////////////////////////////////////////////////////////////////

// Method     :: AddShip
// Class	  :: CGalaxy
// Parameters :: Ship object to Add
// Return     :: <none>
// Function   :: Adds a Ship object to the Ship List
// Rewritten  :: 22/02/2006 {OWV}


void CGalaxy::AddShip(CSpatialID* pShip)
{
	m_ShipList.push_back(pShip);
}

// Method     :: RemoveShip
// Class	  :: CGalaxy
// Parameters :: Ship object to Delete
// Return     :: <none>
// Function   :: Deletes a Ship ID from the Ship List
// Rewritten  :: 22/02/2006 {OWV}

bool CGalaxy::RemoveShip(CSpatialID* pDelete)
{
	// Take out of Galaxy's Ship List List
		
	for (ShipList::iterator shi = m_ShipList.begin(); shi != m_ShipList.end(); shi++)
	{
		CSpatial* pShip = (*(*(*shi)));

		if (pShip->m_Vnum == pDelete)
		{
			m_ShipList.erase(shi);
			break;
		}
	}

	this->Save();

	return true;
}



// Method     :: GetShi
// Class	  :: CGalaxy
// Parameters :: Ship Object to get
// Return     :: <none>
// Function   :: Returns a pointer to an existing Ship Object
// Rewritten  :: 22/02/2006 {OWV}

CShip* CGalaxy::GetShi(gString gsShipName)
{
	for (ShipList::iterator shi = m_ShipList.begin(); shi != m_ShipList.end(); shi++)
	{
		CShip* pShip = (CShip*)(*(*(*shi)));

		if (pShip->m_gsName.HasPrefix(gsShipName))
		{
			return pShip;
		}

	}

	return NULL;
}

// End >> ShipList


// Method     :: AddSector
// Class	  :: CGalaxy
// Parameters :: Sector to Add
// Return     :: <none>
// Function   :: Adds a Sector object to the Sector List


void CGalaxy::AddSector(CSector* pSector)
{
	m_SectorList.push_back(pSector);
}

// Method     :: RemoveSector
// Class	  :: CGalaxy
// Parameters :: Sector to Delete
// Return     :: <none>
// Function   :: Deletes a Sector object from the Sector List

bool CGalaxy::RemoveSector(CSector* pDelete)
{
	// Take out of Galaxy's Sector List
	int nCount = 0;
	SectorList::iterator sec;
	CSector* pSector;
	gString szFile;

	for (sec = m_SectorList.begin(); sec != m_SectorList.end(); sec++)
	{
		pSector = (*sec);
		if (pSector->m_gsFileName == pDelete->m_gsFileName)
			break;
		else
			nCount++;
		
	}

	// Removed from list
	m_SectorList.erase(m_SectorList.begin()+nCount);
	// Delete object

	szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Sectors\\" + pDelete->m_gsFileName;
	unlink(szFile);

	pDelete = NULL;
	this->Save();

	return true;

}

// Method     :: Get
// Class	  :: CGalaxy
// Parameters :: Sector name to get
// Return     :: <none>
// Function   :: Returns a pointer to an existing Sector Object

CSector* CGalaxy::GetSec(gString gsSector)
{
	SectorList::iterator sec;
	CSector* pSector = NULL;

	for (sec = m_SectorList.begin(); sec != m_SectorList.end(); sec++)
	{
		pSector = (*sec);
		if (pSector->m_gsName == gsSector)
		{
			return pSector;
		}
			
	}

	return NULL;

}



// Method     :: <<
// Class	  :: CGalaxy
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the << write
//				 operator is used. In this case we simply write all the object's
//				 fields to the Galaxy file.


std::ostream& operator << ( std::ostream& stream, const CGalaxy& galaxy )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	
	Tools.WriteLn(stream, "[Galaxy]");
	Tools.WriteLn(stream, " Name          : \"%s\"", galaxy.m_gsName);

	for (int n = 0; n < galaxy.m_SectorList.size(); n++)
	{
		Tools.WriteLn(stream, "[Sector]");
		Tools.WriteLn(stream, " Filename      : \"%s\"", galaxy.m_SectorList.at(n)->m_gsFileName);
		Tools.WriteLn(stream, "[/Sector]");
	}
		
	for (gSpatialMap::const_iterator pos = galaxy.m_gSpatialMap.begin(); pos != galaxy.m_gSpatialMap.end(); pos++)	
	{
		CSpatial* pSpatial = (*pos).second;

		gString gsType;
		switch (pSpatial->m_nType)
		{
			case CSpatial::SO_PLANET: gsType = "Planet";
				break;
			case CSpatial::SO_STAR: gsType = "Star";
				break;
			case CSpatial::SO_ASTEROIDF: gsType = "AsteroidField";
				break;
			case CSpatial::SO_DEBRIS: gsType = "Debris";
				break;
			case CSpatial::SO_SATELLITE: gsType = "Satellite";
				break;
			case CSpatial::SO_MOON: gsType = "Moon";
				break;
			case CSpatial::SO_SAT: gsType = "Sat";
				break;
			case CSpatial::SO_ASTEROID: gsType = "Asteroid";
				break;
			case CSpatial::SO_BLACKHOLE: gsType = "BlackHole";
				break;
			case CSpatial::SO_SHIP: gsType = "Ship";
				break;
			case CSpatial::SO_ORDINANCE: gsType = "Ordinance";
				break;
			default: gsType = "Planet";
				break;
		}
				
		// If the object doesn't have a filename we dont save them
		// this is done to prevent the saving of ordinance
		if (pSpatial->m_gfFileName != "")
		{
			Tools.WriteLn(stream, "[%s]", gsType);
			Tools.WriteLn(stream, " Filename      : \"%s\"", pSpatial->m_gfFileName);
			Tools.WriteLn(stream, "[/%s]", gsType);
		}

	}
	
	Tools.WriteLn(stream, "[/Galaxy]");

	return stream;
}

// Method     :: >>
// Class	  :: CGalaxy
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the >> read
//				 operator is used for this class. In this case we poll through the
//				 input stream and read each piece of data into the object.

std::istream& operator >> ( std::istream& stream, CGalaxy& galaxy )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;
	CGameObjects& globals = CGameObjects::Get();

	try
	{
		if ( Tools.ReadKey(stream) == "[Galaxy]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Galaxy]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{

					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, galaxy.m_gsName);
						break;
					case '[':
						if ( gsKey == "[Sector]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								// Read in the Sector, get a pointer to the object
								// add the object to the SectorList
								CSector* pSector = new CSector;
								
								gsFile.Format("%sSectors\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pSector->Load(gsFile);
								galaxy.AddSector(pSector);
							}

						}
						else 
						if ( gsKey == "[Planet]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pPlanet = new CPlanet;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pPlanet->Load(gsFile);
								galaxy.AddSpatial(pPlanet);
								galaxy.AddSpatialToSpace(pPlanet->m_Vnum);
							}
						}
						if ( gsKey == "[Star]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pStar = new CStar;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pStar->Load(gsFile);
								galaxy.AddSpatial(pStar);
								galaxy.AddSpatialToSpace(pStar->m_Vnum);
							}
						}
						if ( gsKey == "[AsteroidField]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pAsteroidField = new CAsteroidField;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pAsteroidField->Load(gsFile);
								galaxy.AddSpatial(pAsteroidField);
								galaxy.AddSpatialToSpace(pAsteroidField->m_Vnum);
							}
						}
						if ( gsKey == "[Debris]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pDebris = new CDebris;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pDebris->Load(gsFile);
								galaxy.AddSpatial(pDebris);
								galaxy.AddSpatialToSpace(pDebris->m_Vnum);
							}
						}
						if ( gsKey == "[Moon]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pMoon = new CMoon;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pMoon->Load(gsFile);
								galaxy.AddSpatial(pMoon);
								galaxy.AddSpatialToSpace(pMoon->m_Vnum);
							}
						}
						if ( gsKey == "[Sat]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pSat = new CSat;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pSat->Load(gsFile);
								galaxy.AddSpatial(pSat);
								galaxy.AddSpatialToSpace(pSat->m_Vnum);
							}
						}
						if ( gsKey == "[BlackHole]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pBlack = new CBlackhole;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pBlack->Load(gsFile);
								galaxy.AddSpatial(pBlack);
								galaxy.AddSpatialToSpace(pBlack->m_Vnum);
							}
						}
						if ( gsKey == "[Asteroid]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pAsteroid = new CAsteroid;
								
								gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pAsteroid->Load(gsFile);
								galaxy.AddSpatial(pAsteroid);
								galaxy.AddSpatialToSpace(pAsteroid->m_Vnum);
							}
						}
						if ( gsKey == "[Ship]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CSpatial* pShip = new CShip;
								
								if (pShip->Load(gsFile))
								{
									galaxy.AddSpatial(pShip);
									galaxy.AddShip(pShip->m_Vnum);

									if (((CShip*)pShip)->m_ShipState->IsSet(CShip::_FLYING))
										galaxy.AddSpatialToSpace(pShip->m_Vnum);
								}
								else
								{
									LOG_SCOPE("CGalaxy::>>");
									g_Log.Log("Invalid Ship Filename: %s", gsFile);
								}
							}
						}
						break;

					default:
						Tools.Report(E_ERROR, "[CGalaxy::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Galaxy]");
			}
		}
		else
			Tools.Report(E_ERROR, "[CGalaxy::>>] Invalid stream!");

	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CGalaxy::>>] Error encountered while reading galaxy file..");
	}

	return stream;
}

// Method     :: Load
// Class	  :: CGalaxy
// Parameters :: Filename to load
// Return     :: True if Successful, False if fails to read
// Function   :: Simply file operations. Locates the file to load then 
//				 attempts to load them in using XML
// Written    :: Original {OWV}, Updated 24/05/06
// Updated    :: To convert loading function to use Tiny XML lib

bool CGalaxy::Load(gFileName gGalaxy)
{
	CGameObjects& globals = CGameObjects::Get();

	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gGalaxy) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Galaxy");

	ReadXml(pNode);

	return true;
}

// Method     :: Save
// Class	  :: CGalaxy
// Parameters :: <none>
// Return     :: True if Successful, False if not
// Function   :: Saves the CGalaxy object to XML file.
// Written    :: Original {OWV}, Updated 24/05/06
// Updated    :: To convert loading function to use Tiny XML lib

bool CGalaxy::Save()
{
	std::fstream fp;

	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sgalaxy.dat", globals.m_Config.szDir[CGameObjects::_SPACEDIR]);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Galaxy");

	WriteXml(pXmlNode);

	// Now save all its sectors
	for (SectorList::iterator sec = m_SectorList.begin(); sec != m_SectorList.end(); sec++)
	{
		CSector* pSector = (*sec);
		pSector->Save(pSector->m_gsFileName);
	}

	// Now save all spatial objects
	for (SpatialMap::iterator pos = m_SpatialMap.begin(); pos != m_SpatialMap.end(); pos++)	
	for (SpatialList::iterator spa = (*pos).second.begin(); spa != (*pos).second.end(); spa++)	
	{
		CSpatial* pSpatial = (*(*(*spa)));

		if (!pSpatial)
			continue;

		if (pSpatial->m_gfFileName != "")
			pSpatial->Save();
	}

	return doc.SaveFile((const char*)szFile);
}

void CGalaxy::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	time_t tm = time(0);
	gString gsTime = ctime(&tm);
	gsTime.DeleteChar( gsTime.Length()-1 ); 

	Tools.WriteXml(pParent, "last_saved_on",	gsTime);
	Tools.WriteXml(pParent, "name",				m_gsName);

	// Write the sectors
	for (int n = 0; n < m_SectorList.size(); n++)
	{
		TiXmlNode* pSector = Tools.InsertXmlChild(pParent, "Sector");
		Tools.WriteXml(pSector, "filename",					m_SectorList[n]->m_gsFileName);
	}

	// Write all spatial objects
	for (gSpatialMap::iterator pos = m_gSpatialMap.begin(); pos != m_gSpatialMap.end(); pos++)	
	{
		CSpatial* pSpatial = (*pos).second;
		// We save each Spatial object by its specific type
		// szTypesXML contains a list of the xml formated headers
		const gString gsType = CSpatial::szTypesXML[pSpatial->m_nType];

		// If the object doesn't have a filename we dont save them
		// this is done to prevent the saving of ordinance		
		if (pSpatial->m_gfFileName != "")
		{
			TiXmlNode* pObj = Tools.InsertXmlChild(pParent, gsType);
			Tools.WriteXml(pObj, "filename",				pSpatial->m_gfFileName);
		}

	}

}

void CGalaxy::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);

	// Read in each of the sectors
	TiXmlNode* pSectorNode = pParent->FirstChild("Sector");
	while ( pSectorNode != NULL )
	{
		CSector* pSector = new CSector;
		gFileName gFileName;
		Tools.ReadXml(pSectorNode, "filename", gFileName);

		gFileName.Format("%sSectors\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gFileName);						
					
		pSector->Load(gFileName);

		AddSector(pSector);
		pSectorNode = pSectorNode->NextSibling("Sector");
	}

	// Spatial objects
	// We iterate through each type of object to see if 
	// we can find one saved within the XML file
	for (int i = 0; i < CSpatial::SO_MAX; i++)
	{
		const gString gsSpatial = CSpatial::szTypesXML[i];
		TiXmlNode* pSpatialNode = pParent->FirstChild(gsSpatial);
		while (pSpatialNode != NULL)
		{
			CSpatial* pSpatial;
			gFileName gFileName;
			Tools.ReadXml(pSpatialNode, "filename", gFileName);

			if (i == CSpatial::SO_SHIP)
				gFileName.Format("%sShips\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gFileName);
			else
				gFileName.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gFileName);

			switch (i)
			{
				case CSpatial::SO_PLANET: pSpatial = new CPlanet;
					break;
				case CSpatial::SO_STAR: pSpatial = new CStar;
					break;
				case CSpatial::SO_ASTEROIDF: pSpatial = new CAsteroidField;
					break;
				case CSpatial::SO_DEBRIS: pSpatial = new CDebris;
					break;
				case CSpatial::SO_SATELLITE: pSpatial = new CSatellite;
					break;
				case CSpatial::SO_MOON: pSpatial = new CMoon;
					break;
				case CSpatial::SO_SAT: pSpatial = new CSat;
					break;
				case CSpatial::SO_ASTEROID: pSpatial = new CAsteroid;
					break;
				case CSpatial::SO_BLACKHOLE: pSpatial = new CBlackhole;
					break;
				case CSpatial::SO_SHIP: pSpatial = new CShip;
					break;
				case CSpatial::SO_ORDINANCE: pSpatial = new CPlanet;
					break;
				default: pSpatial = new CPlanet;	// Default is a planet
					break;
			}

			pSpatial->Load(gFileName);

			// Ships are a special case
			if (i == CSpatial::SO_SHIP)
			{
				AddSpatial(pSpatial);
				AddShip(pSpatial->m_Vnum);

				// If they are set flying they need to be added to space
				if (((CShip*)pSpatial)->m_ShipState->IsSet(CShip::_FLYING))
					AddSpatialToSpace(pSpatial->m_Vnum);
			}
			else
			{
				AddSpatial(pSpatial);
				AddSpatialToSpace(pSpatial->m_Vnum);
			}
			
			
			pSpatialNode = pSpatialNode->NextSibling(gsSpatial);

		}
		
	}

} 


///////////////////////////////////////////////////////////////////////////////////////////
// 2. Sector Class
///////////////////////////////////////////////////////////////////////////////////////////
CSector::CSector()
{
	m_gsName = "";
	m_gsFileName;
	m_Location = new CCartBound;
	m_Explored = false;
	
}

CSector::CSector(gString gsName, gString gsFilename)
{
	m_gsName = gsName;
	m_gsFileName = gsFilename;
	m_Location = new CCartBound;
	m_Explored = false;
}


CSector::~CSector()
{
	delete m_Location;

}

// Method     :: Inform
// Class	  :: CSector
// Parameters :: <message>
// Return     :: <none>
// Function   :: Provides a message to all Players in the current sector
void CSector::Inform(gString gsMsg)
{
	// For the moment we will just check if the 'player' is in the sector
	// and give them the message
	ActorMap::iterator act;
	CGameObjects& globals = CGameObjects::Get();
	CGameWorld* pWorld = CGameObjects::Get().GameWorld();
	CPlayer* pP;

	

	for (act = pWorld->Players().begin(); act != pWorld->Players().end(); act++)
	{
			if ( (pP = (CPlayer*)((*act).second)) != NULL )
			{
				// Same sector
				if ( pP->m_Sector == this->m_gsName)
				{
					// Pass message
					pP->Write("%s\n\r", gsMsg);
				}
				
			}

			if ( pWorld->Players().size() == 0 )
				break;
	}

}

// Method     :: Load
// Class	  :: CSector
// Parameters :: Filename to load
// Return     :: True if Successful, False if fails to read
// Function   :: Simply file operations. Locates the file to load then 
//				 attempts to load them in using the class specific >> operator

bool CSector::Load(gFileName gSector)
{
	CGameObjects& globals = CGameObjects::Get();

	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gSector) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Sector");

	ReadXml(pNode);

	return true;
}

// Method     :: Save
// Class	  :: CSector
// Parameters :: Filename to save to
// Return     :: True if Successful, False if not
// Function   :: Saves the CSector object to file using the new Tiny XML lib

bool CSector::Save(gFileName gSector)
{
	CGameObjects& globals = CGameObjects::Get();
	std::fstream fp;

	gSector.Format("%sSectors\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gSector);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Sector");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gSector);
}


void CSector::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",			m_gsName);
	Tools.WriteXml(pParent, "filename",		m_gsFileName);
	Tools.WriteXml(pParent, "explored",		m_Explored);		

	m_Location->WriteXml(pParent);
	
	return;
}

void CSector::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "filename",		m_gsFileName);
	Tools.ReadXml(pParent, "explored",		m_Explored);	

	m_Location->ReadXml(pParent);
} 