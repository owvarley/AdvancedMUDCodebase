//                                __.             
//                               (__.    ,_ ._.._ 
//                               .__)\/\/(/,[  [_)
//                                             |  
//
// Interpreted by Owen Varley [Nekekami] :: <o.w.varley#dur.ac.uk>
//
// Durham   :: CS Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Header   :: Space.h
// Function :: Handles the classes for all Space objects

// ----
// |-Galaxy [CGalaxy]
// | |-- Sector [CSector]
// | | |-- Stars [CStar]
// | | |-- Planets [CPlanet]
// | | | |-- Moons [CMoon]
// | | | |-- Satellites [CSatellite]
// | | |-- Asteroid Fields [CAsteroid]
// | | |-- Novas [CNova]
// | | |-- Vessels [CVessel]
// | | | |-- Capital Ships [CCapital]
// | | | |-- EV Pods [CEv]
// | | | |-- Freighters [CFreighter]
// | | | |-- Starfighters [CStarfighter]
// | | | |-- Space stations [CStation]
// | | |-- Ordinance [COrdinance]
// | | |-- Debris [CDebris]
// | | |-- Blackholes [CBlackhole]
// | | |-- Maws [CMaw]
// | | |-- Meteors [CMeteor]

#ifndef __SPACE_H__
#define __SPACE_H__

#include <vector>
#include "MudCore.h"
//#include "Spatial.h"
#include "OTools.h"

#pragma warning(disable: 4251)

// Sector Class
class CSector
{

public:
	CSector();
	CSector(gString gsName, gString gsFilename);
	~CSector();

	///////////////////////////////////////////////////////////////////////////////////////
	//	Public methods
	///////////////////////////////////////////////////////////////////////////////////////
	void							Update();							// Updates contents of Sector
	void							Inform(gString gsMsg);				// Sends message to all in Sector
//	bool							AddShip(CVessel* vessel);			// Adds to Ship list
//	bool							RemoveShip(CVessel* vessel);		// Removes from Ship list
//	CVessel*						GetShip(gString gsName);			// Gets a Pointer to a ship

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load(gFileName gsSector);
	virtual bool	Save(gFileName gsSector);
		
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CSector& sector );
	friend std::istream& operator >> ( std::istream& stream, CSector& sector );
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);
	
// Data
public:

	gString							m_gsName;		// Name of Sector
	gFileName						m_gsFileName;	// Filename of Sector
	//ShipList						m_ShipList;		// List of all vessels
	CCartBound*						m_Location;		// Location in the Galaxy
	bool							m_Explored;		// Whether Sector has been explored
	//CFaction						m_Faction;		// Ruling Faction

};

typedef std::vector<CSector*> SectorList;	// List of all Sectors


// Galaxy Class
class CGalaxy
{

// Methods
public:
	CGalaxy();
	~CGalaxy();

	void							AddSector(CSector* Sector);			// Adds to Sector List
	bool							RemoveSector(CSector* Sector);		// Removes from Sector List

	void							AddSpatial(CSpatial* Spatial);		// Adds to Spatial List
	bool							RemoveSpatial(CSpatial* Spatial);	// Removes the Spatial List

	void							AddSpatialToSpace(CSpatialID* Spa);	// Adds to Spatial Map
	bool							RemoveSpatialFromSpace(CSpatialID* p);// Removes from Spatial Map

	void							AddShip(CSpatialID* Spa);			// Adds to Spatial List
	bool							RemoveShip(CSpatialID* Spa);		// Removes the Spatial List


	CSector*						GetSec(gString gsSecName);			// Returns a pointer to a sector
	CSpatial*						GetSpa(gString gsSpaName);			// Returns a pointer to a spatial object
	CSpatial*						GetSpa(int nVnum);					// Returns a pointer to a spatial object
	CSpatial*						GetSpaInSpace(gString gsSpaName);	// Returns a pointer to a spatial object
	CSpatial*						GetSpaInSpace(int nVnum);			// Returns a pointer to a spatial object
	CShip*							GetShi(gString gsShipName);			// Returns a pointer to a ship object
	
	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load(gFileName gsGalaxy);
	virtual bool	Save();
		
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CGalaxy& galaxy );
	friend std::istream& operator >> ( std::istream& stream, CGalaxy& galaxy );

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

// Data
public:
	gString							m_gsName;		// Name of Galaxy
	SectorList						m_SectorList;	// List of Sectors

	SpatialList						m_SpatialList;	// The list contains a list of of vnums that link to 
													// their corresponding Spatial Object from the Global map
	
	SpatialMap						m_SpatialMap;	// The Spatial map holds a key of the Sector a Spatial obj is within
													// and returns a list of vnums of all Spatial objects within
													// that sector.

	gSpatialMap						m_gSpatialMap;  // Global mapping of Unique Id's to Objects in memory


	ShipList						m_ShipList;		// List of all Ships, regardless of whether landed or not

};

typedef std::vector<CGalaxy*> GalaxyList;

#endif