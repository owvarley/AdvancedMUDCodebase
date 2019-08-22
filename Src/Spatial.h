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

// Header   :: Spatial.h
// Function :: Handles the classes for all Spatial entities and their anchillary functions


// |-- Cartesians [CCart]
// |-- Cartesian Bounds [CCartBound]
// |-- Shapes [CShape]
// |-- Rectangles [CRect]
// ----
// |-- Planets [CPlanet]				0
// |-- Stars [CStar]					1
// |-- Asteroid Fields [CAsteroidField]	2
// |-- Debris [CDebris]					3
// |-- Satellites [CSatellites]			4
// | |-- Moons [CMoon]					5
// | |-- Sat [CSat]						6
// |-- Blackholes [CBlackhole]			7
// |-- Asteroid [CAsteroid]				8
// |-- Ships [CShip]					9


#ifndef __SPATIAL_H__
#define __SPATIAL_H__

#include <vector>
#include <map>

#include "MudCore.h"
#include "Placement.h"
#include "Crew.h"
#include "Module.h"
#include "Set.h"
#include "Cart.h"
#include "../gTools/MemCheck.h"

class CShip;
class CComponent;
class CSpatial;
class CRect;
class CArea;
class CCart;
class CSpatialID;


typedef std::vector<CComponent*> ComponentList;
typedef std::map<int, int>InstallMap;

extern UINT uiUniqueShipID;

#pragma warning(disable: 4251)
#pragma warning(disable: 4243)
#pragma warning(disable: 4786)


class CHull
{
public: 
	CHull();
	~CHull();

	typedef enum e_HullTypes
	{
		HT_STARFIGHTER = 0,
		HT_SMALLSHIP   = 1,	// Smallships
		HT_MIDSHIP	   = 2,	// Midships
		HT_LARGESHIP   = 3, // Largeship && Capship
		HT_SUPERCAP	   = 4,
		HT_MAX
	};

	static char *szHullTypes[];

	int						Damage(int n, bool b, CShip* p);	// Damage the Hull Cube's Components
	int						Height(bool bInternal);
	int						Width(bool bInternal);
	int						Length(bool bInternal);
	int						Size(bool bRemaining);				// Returns the size remaining or used for the Hullcube
	gString					Gui();


	CHull&	operator =		(CHull& clone);

	virtual bool			Save();
	virtual bool			Load();
	virtual bool			Load(gString filename);

	void					WriteXml(TiXmlNode* pParent);
	void					ReadXml(TiXmlNode* pParent);

	// Reading extra and writing extra
	void					ReadExtra(TiXmlNode* pParent);
	void					WriteExtra(TiXmlNode* pParent);

public:

	ComponentList			m_Components;			// Components contained within Hull Cube
	CRect*					m_EDimension;			// Dimensions and Location of Hull Cube for External
	CModule*				m_Armour;				// Armour Module for HullCube
	gString					m_gsName;				// Name of HullCube
	gString					m_gsFileName;			// Filename of HullCube
	CSet*					m_Cover;				// The arcs this cube can be hit in
	int						m_nCKeel;				// Current Keel integrity
	int						m_nMKeel;				// Maximum Keel integrity
	int						m_nSize;				// Size of HullCube
	int						m_nHeight;				// Internal height 
	int						m_nLength;				// Internal Length  <- These are used for area generation
	int						m_nWidth;				// Internal Width  
	int						m_nhiV;					// High Vnum
	int						m_nloV;					// Low Vnum
	int						m_nType;				// Type of Hullcube
	int						m_nResilience;			// SOAK attribute
	int						m_nMass;				// Mass of Cube
	

};
typedef std::vector<CHull*>HullList;

///////////////////////////////////////////////////////////////////////////////////////////
// HullCube Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is held by the GameWorld object and is simply used to load in all the
// HullCubes
///////////////////////////////////////////////////////////////////////////////////////////
class CHLoader
{
public:
	CHLoader();
	~CHLoader();

	bool							Load(gFileName gfFile);	// Load method
	bool							Save();					// Save method

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	HullList			m_HullCubes;

};

class CFrame
{
public:
	CFrame();
	~CFrame();	
	
	int						Mass();									// Works out the total mass of the frame
	bool					Damage(int nTotal, bool bInternal);		// Damage the Frame's Cubes


	CFrame&	operator =		(CFrame& clone);

	void					WriteXml(TiXmlNode* pParent);
	void					ReadXml(TiXmlNode* pParent);

public:
	HullList				m_HullCubes;
	gString					m_gsName;

};
typedef std::vector<CFrame*>FrameList;


///////////////////////////////////////////////////////////////////////////////////////////
// Rect Class
///////////////////////////////////////////////////////////////////////////////////////////
// The rect class is used to define a rectangle object in the wireframe structure of a ship
///////////////////////////////////////////////////////////////////////////////////////////
class CRect
{
public:
	CRect();
	CRect(int l, int b, int h);
	~CRect();	
	
	CRect&	operator =		(CRect& clone);
	bool							Within(CRect* pRect);	// Is this rectangle countained within our rectangle
	bool							Contains(CCart* pCart);	// Is this Cart within our Rectangle?
		
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	int				m_nWidth;
	int				m_nLength;
	int				m_nHeight;
	CCart*			m_Location;
};
typedef std::vector<CRect*>RectList;


///////////////////////////////////////////////////////////////////////////////////////////
// Shape Class
///////////////////////////////////////////////////////////////////////////////////////////
// The Shape class allows us to access the structure of a Ship contained within its HullCubes
// without having to manually iterate through the ship's structure each time
///////////////////////////////////////////////////////////////////////////////////////////
class CShape
{
public:
	CShape();
	~CShape();

	CShape&	operator =		(CShape& clone);

	bool							SetSize(CShip* pS);		//
	bool							SetHeight(CShip* pS);	// Used to generate the dimensions and sizes of a ship's shape
	bool							SetLength(CShip* pS);	// based on its HullCubes
	bool							SetWidth(CShip* pS);	//
	int								Size();					// Works out the number of rooms used to represent this shape
	int								Length();				// Works out the length of the shape
	int								Width();				// Works out the width of the shape
	int								Height();				// Works out the height of the shape
	int								Distance(CCart *c);		// Works out the min distance from obj
	bool							Contains(CCart *c);		// Works out if the Cart is within our Shape
	CArea*							Generate(CShip*pS);		// Generates the room structure for this Shape
	
	enum e_ShapeTypes
	{
		ST_SPHERE	 = 0,
		ST_RECTANGLE = 1,
		ST_MAX
	};

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	int				m_nType;
	int				m_nRadius;
	int				m_nWidth;
	int				m_nHeight;
	int				m_nLength;
	int				m_nSize;
	CCart*			m_Center;

};
typedef std::vector<CShape*>ShapeList;

///////////////////////////////////////////////////////////////////////////////////////////
// Template Class
///////////////////////////////////////////////////////////////////////////////////////////
// A Template is simply the skeleton structure of a ship. A template file can be used to create
// a ship from scratch. It defines the components used to construct the ship as well as the
// shape it assumes.
///////////////////////////////////////////////////////////////////////////////////////////
class CTemplate
{
public:
	CTemplate();
	~CTemplate();

	typedef enum e_Status
	{
		_NONE		= 0,
		_TEMPLATE,
		_AREA,
		_WEAPONS,
		_REVIEW,
		_AUTHORISATION,
		_COMPLETED,
		_NUM_STATUS
	};

	static char *szStatus[];

	int								Cost();					// Works out the credit value of this Template
	int								Materials(int nType);	// Determines the amount of raw materials required to build this template
	int								Time();					// Determines the time it will take to produce this ship
	bool							Delete();				// Delete this template
	bool							Save();					// Save our template to file
	bool							Load(gString file);		// Load our template from file
	bool							Generate();				// Use this template to create a new ship
	
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	CShape*			m_Shape;		// The Shape of the Template
	CShip*			m_Ship;			// The Ship
	CArea*			m_Area;			// Area of the Ship
	gString			m_gsName;		// Name of this Template
	gString			m_gfFileName;	// Filename this is saved at
	gString			m_gsAuthor;		// Author of Template
	gString			m_gsAuthorisor; // Authorisor of Template
	int				m_nStatus;		// The state of the template
};

typedef std::vector<CTemplate*>TemplateList;

///////////////////////////////////////////////////////////////////////////////////////////
// Template Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is held by the GameWorld object and is simply used to load in all the
// different Ship Templates that have been saved.
///////////////////////////////////////////////////////////////////////////////////////////
class CTLoader
{
public:
	CTLoader();
	~CTLoader();

	bool							Load(gFileName gfFile);	// Load method
	bool							Save();					// Save method

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	TemplateList					m_Templates;

};

// All Spatial Objects :: Root Class
class CSpatial
{

public:
	CSpatial();
	virtual ~CSpatial();

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();				// Update Behaviour function
	virtual void					Destroy();				// Destroy Behaviour function
	virtual void					Damage(int nA, int nB);	// Damage the object
	virtual int						Distance(CSpatial* p);  // Shortest Distance
	virtual void					Notify(gString gsMsg);	// Message passing
	virtual void	  NotifySpace(CSpatial* pS, char *f, ...);  // Message to space
	virtual void					NotifySpace(char *f, ...);  // Message to space
	virtual void					Write(int n, char *f, ...); // Message passing for the Spatial obj

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void					WriteXml(TiXmlNode* pParent);
	virtual void					ReadXml(TiXmlNode* pParent);

	typedef enum e_SpatialTypes
	{
		SO_PLANET = 0,
		SO_STAR = 1,
		SO_ASTEROIDF = 2,
		SO_DEBRIS = 3,
		SO_SATELLITE = 4,
		SO_MOON = 5,
		SO_SAT = 6,
		SO_ASTEROID = 7,
		SO_BLACKHOLE = 8,
		SO_SHIP = 9,
		SO_ORDINANCE = 10,
		SO_MAX
	};

	typedef enum e_Signatures
	{
		SI_HEAT     = 1,
		SI_EM       = 2,
		SI_ION      = 3,
		SI_MASS     = 4,
		SI_MAX      = 5
	};

	static char *szTypes[];
	static char *szTypesXML[];
	static char *szSignatures[];

public:
	float							m_fLastUpdate;
	bool							m_bDelete;			// Should we delete this object?
	gString							m_gsName;			// Name of Spatial Object
	int								m_nType;			// Type of Spatial Object
	int								m_Signature[SI_MAX];// Signatures for Spatial Object
	int								m_nGravity;			// Gravity of Spatial Object
	gString							m_gsDescription;	// Description of spatial object
	gString							m_gsSector;			// Name of Sector it is in
	gString							m_gfFileName;		// Name of File
	CCart*							m_Location;			// Location in Space
	CShape*							m_Shape;			// Shape of Spatial
	CSpatialID*						m_Vnum;				// Vnum of Object

};

typedef std::vector<CSpatialID*>SpatialList;
typedef std::map<gString, SpatialList>SpatialMap;
typedef std::map<int, CSpatial*>gSpatialMap;

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Planet Class
///////////////////////////////////////////////////////////////////////////////////////////
// The Planet Class is used to create Planets. Spatial bodies that have atmosphere and
// can sustain a form of life. Most planets are inhabitable and can be landed on.
///////////////////////////////////////////////////////////////////////////////////////////
class CPlanet : public CSpatial
{
public:
	CPlanet();
	CPlanet(gString name, gString filename);
	~CPlanet();

	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	//int							m_Gravity;			// Gravity strength of planet
	//AreaList						m_Areas;			// Areas in planet

};

///////////////////////////////////////////////////////////////////////////////////////////
// 2. Star Class
///////////////////////////////////////////////////////////////////////////////////////////
// The Star Class contains and controls all types of stars. Stars can 'die' that is their
// furnace runs out of fuel and the pressure inside the star decreases. The star will begin
// to loose its struggle against gravity and will collapse. When it does collapse one of 
// three things can happen depending on the mass of the star.
// 1) Mass < Sun     :: It will collapse and form a whitedwarf.
// 2) Mass > 1-4*Sun :: It will create a neutron star (pulsar)
// 3) Mass > 4*Sun   :: It will collapse into a Black Hole
///////////////////////////////////////////////////////////////////////////////////////////
class CStar : public CSpatial
{
public:
	CStar();
	CStar(gString name, gString filename);
	~CStar();

	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	bool							m_bDying;			// Nova?
	int								m_nEnergy;			// Energy emitted
	//int								m_nMass;			// Mass of star

};

///////////////////////////////////////////////////////////////////////////////////////////
// 3. Asteroid Class
///////////////////////////////////////////////////////////////////////////////////////////
// Asteroids are simply a collection of minerals of a variety of shapes and sizes. They will
// be represented in this space system by having a size value. Asteroids can contain minerals
// which can be mined. The size of the asteroid will determine how many minerals are contained
// within it.
///////////////////////////////////////////////////////////////////////////////////////////
class CAsteroid : public CSpatial
{
	public:
	CAsteroid();
	CAsteroid(gString name, gString filename);
	CAsteroid(gString name, gString filename, int size, int mineral, gString gsField);
	~CAsteroid();


	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	int								m_nSize;			// Size of the Asteroid, will determine damage and how much
														// minerals it contains.
	int								m_nMineral;			// Mineral Type contained
	gString							m_gsField;			// Name of Field its attached to

};

typedef std::vector<CAsteroid*>AsteroidList;

///////////////////////////////////////////////////////////////////////////////////////////
// 4. AsteroidField Class
///////////////////////////////////////////////////////////////////////////////////////////
// An AsteroidField is very simply a HUGE collection of Asteroid objects. It works slightly
// differently than other spatial objects in its saving method. This is to prevent
// the galaxy.dat file getting too large. Hence an asteroid field will save and load
// all its child asteroids itself. This allows the galaxy.dat file to just load in the
// field.
///////////////////////////////////////////////////////////////////////////////////////////
class CAsteroidField : public CSpatial
{
public:
	CAsteroidField();
	CAsteroidField(gString name, gString filename, int size);
	~CAsteroidField();

	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
	void							AddAsteroid(CAsteroid* pAster);
	bool							RemoveAsteroid(CAsteroid* pAster);
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:

	AsteroidList					m_AsteroidList;		// List of Asteroids contained within the field
	CCart*							m_pHeading;			// Asteroid Heading
	int								m_nSpeed;			// Speed of Asteroid Cluster

};

///////////////////////////////////////////////////////////////////////////////////////////
// 5. Debris Class
///////////////////////////////////////////////////////////////////////////////////////////
// Whenever a vessel is destroyed in space it becomes a hulk of Debris. The Hulk can be 
// dangerous as most ships are constructed around a reactor. If the reactor goes critical
// it will vaperise most of the surrounding area. However, debris can contain salvage
// this could be some modules that are servicable but damaged from whatever the debris came
// from. Sometimes, a large amount of valuable debris can be gathered from a destroyed
// vessel.
///////////////////////////////////////////////////////////////////////////////////////////
class CDebris: public CSpatial
{
	public:
	CDebris();
	CDebris(gString name, gString filename);
	~CDebris();


	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	int								m_nLife;			// Timer till death
	bool							m_bExplode;			// Explode on death?
	int								m_nDamage;			// Damage it will cause
	// Modules it contains!
	ModuleList						m_Modules;			// Modules that are salvageable

};


///////////////////////////////////////////////////////////////////////////////////////////
// 6. Satellite Class
///////////////////////////////////////////////////////////////////////////////////////////
// The satellite class contains moons and human made satellites. This class allows spatial
// objects of this type to orbit about a Planet type
///////////////////////////////////////////////////////////////////////////////////////////
class CSatellite: public CSpatial
{
public:
	CSatellite();
	CSatellite(gString name, gString filename);
	~CSatellite();

	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	gString							m_gsPlanet;			// Planet of orbit
	long							m_nOrbit;			// Distance from center of planet

};

///////////////////////////////////////////////////////////////////////////////////////////
// 6a. Moon Class
///////////////////////////////////////////////////////////////////////////////////////////
// A simple Moon that has a specific orbit around a planet
///////////////////////////////////////////////////////////////////////////////////////////
class CMoon: public CSatellite
{
public:
	CMoon();
	CMoon(gString name, gString filename);
	~CMoon();

	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour functions
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	 
	// Moon specific fields here

};

///////////////////////////////////////////////////////////////////////////////////////////
// 6b. Sat Class
///////////////////////////////////////////////////////////////////////////////////////////
// A human made Satellite of a specific type. It can be access by players on the planet and
// used to carry out surveillance and communication
///////////////////////////////////////////////////////////////////////////////////////////
class CSat: public CSatellite
{
public:
	CSat();
	CSat(gString name, gString filename);
	~CSat();


	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour functions
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	int								m_nPower;			// Power of the Satellite
	int								m_nIntegrity;		// Health of Satellite
	gString							m_gsOwner;			// Owner of Satellite: Player/Faction

};

///////////////////////////////////////////////////////////////////////////////////////////
// 7. Ordinance Class
///////////////////////////////////////////////////////////////////////////////////////////
// A ship can fire a number of different types of Ordinance. This includes Missiles, Torpedoes
// Rockets and Space Mines. This class handles these objects within space, i.e. after they 
// have been deployed. It does things like tracks ordinance onto their target, checks for
// proximity trepassing for Mines and damages other spatial objects when the situation dictates.
///////////////////////////////////////////////////////////////////////////////////////////
class COrdinance: public CSpatial
{
	public:
	COrdinance();
	COrdinance(gString name, gString filename);
	~COrdinance();


	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:

	int								m_nFuel;			// Fuel remaining
	int                             m_nSpeed;			// Flight Speed
	int								m_nManeuver;		// Turning rate
	int								m_nDamage;			// Damage it causes
	int								m_nProximity;		// How far from target it explodes
	int								m_nDamType;			// Frag, EMP, High Explosive
	CSpatial*						m_Target;			// Target of Ordinance
	CCart*							m_Heading;			// Heading of missile
	CCart*							m_dHeading;			// Destination heading of missile

};


///////////////////////////////////////////////////////////////////////////////////////////
// 8. Black Hole Class
///////////////////////////////////////////////////////////////////////////////////////////
// Black Holes are created by the resulting collapse of a star with a very large mass
// The Black Hole has a large pull of gravity and will suck objects that enter its magnetic
// field into it destroying them. They absorb light and hence cannot be seen. However,
// they produce such a large amount of gravity that they can be detected.
///////////////////////////////////////////////////////////////////////////////////////////
class CBlackhole: public CSpatial
{
	public:
	CBlackhole();
	CBlackhole(gString name, gString filename);
	~CBlackhole();


	virtual bool					Save();
	virtual bool					Load();
	virtual bool					Load(gString filename);
	virtual void					Update();			// Update Behaviour function
	virtual void					Destroy();			// Destroy Behaviour function
//	virtual void					Notify();			// Message passing
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	//int								m_nGravity;			// Gravitational pull

};

///////////////////////////////////////////////////////////////////////////////////////////
// 0. Component Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is used for all Components which do not have specific modular parts contained
// within them. This class facilitates the loading/saving and other methods to be carried
// out on objects of type 'component'
///////////////////////////////////////////////////////////////////////////////////////////
class CComponent : public CMemoryDebugBase
{

public:
	CComponent();
	~CComponent();

	//  Component Types
	typedef enum e_ComponentTypes
	{
		CT_SHIELD			= 0,
		CT_ENGINEERING		= 1,
		CT_SUBLIGHT			= 2,
		CT_HYPERDRIVE		= 3,
		CT_MAGAZINE			= 4,
		CT_EXTERNAL			= 5,
		CT_INTERNAL			= 6,
		CT_LANDING			= 7,
		CT_BULKSTORAGE		= 8,
		CT_CONTROLPOINT		= 9,
		CT_WEAPONMOUNT		= 10,
		CT_ESCAPEPOD		= 11,
		CT_SFOIL			= 12,
		CT_LAST				= 13,

	};

	typedef enum e_Timers
	{
		T_SHIELD			= 2,
		T_REACTOR			= 3,
		T_COOLANT			= 3,
		T_SUBLIGHT			= 2,
		T_HYPERDRIVE		= 0,
		T_COLLECTOR			= 1,
		T_SENSOR			= 2,
		T_THRUSTERS			= 1,
		T_GRAVWELLS			= 4,
		T_ASTROGRATIONCOMP  = 2,
		T_INTERNAL			= 2,
		T_TRACTOR			= 2,
		T_BLASTDOOR			= 2,
		T_CONTROL			= 2,
		T_WEAPON			= 1,
		T_ESCAPE			= 2,
		T_SFOIL				= 2,
		T_LAST
	};

	static char *szComponents[];
	static char *szClassnames[];
	

	static bool						ValidH(gString header); // Determines if a loading header is a component

	virtual bool					Install(gString pShip);	// Install the Component
	virtual	bool					Uninstall();			// Remove the Component
	virtual bool					Tamper();				// Attempt to tamper with the Device
	virtual bool					Destroy();				// Destroy the Component
	virtual int						Damage(int nAmount);	// Damage the Component
	virtual int						Integrity();			// Calculate Component's integrity
	virtual	int						FreePower();			// Calculates the free powerlink capacity
	virtual int						MaxPower();				// Calculates the maximum power that this component can withstand
	virtual void					Update();				// Update has happened, what do we do?
	int								GetIndex(gString type);	// Gets the Index of a Module type
	bool							Valid(gString type);	// Is the type a valid Module
	CModule*						Get(int nType);			// Return a Module
	CModule*						Get(gString gsName);	// Returns a Module by name
	CHull*							GetHull(CShip* pShip);	// Returns the Hullcube component belongs to
	bool							CanInstall(int nType);	// Determines whether you can install a certain Module

	// Component/Module Data fetching
	int								Size();					// Returns remaining space
	int								Mass();					// Returns the total Mass of the Component

	virtual CComponent&	operator =	(CComponent& clone);

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);
	virtual void					WriteExtra(TiXmlNode* pParent);
	virtual void					ReadExtra(TiXmlNode* pParent);
	
public:

	gString							m_gsName;				// Component's name
	gString							m_gsShip;				// Ship installed in
	
	CPlacement						m_Installed;			// Where Component is installed
			
	int								m_Type;					// The Type of Component
	int								m_nSize;				// Maximum module space
	
	ModuleMap						m_Modules;				// Sub-components
	InstallMap						m_Install;				// Contains allowed Modules
		
};

///////////////////////////////////////////////////////////////////////////////////////////
// 2a. ShieldState Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Shield banks
///////////////////////////////////////////////////////////////////////////////////////////
class CShieldState
{
public:
	CShieldState();
	~CShieldState();

	// Must reflect CShip::A_MAX
	enum e_Arcs
	{
		AR_MAX = 6
	};

	bool							Set(int nAmount, int loc);		// Setup the shields
	bool							Charge(int nAmount, int loc);	// Charge the shields
	bool							Deplete(int nAmount, int loc);	// Damage the shields
	int								Value();						// Return overall strength

public:
	int								m_Shields[AR_MAX];
};

///////////////////////////////////////////////////////////////////////////////////////////
// 2. Shield Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Shields
///////////////////////////////////////////////////////////////////////////////////////////
class CShield : public CComponent
{
public: 
	CShield();
	~CShield();

	virtual CShield&	operator =		(CShield& clone);

	// Sub-Component fetching
	// Generator: Charges and Emitts the shield
	CModule*					Emitter()			{ return (*m_Modules.find(CModule::MT_SHIELD_GENERATOR)).second; }
	
	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Class Specfic Methods
	bool							Power(bool bOn);			// Power Shields up or down
	bool							Balance();					// Reset all Banks to being equal
	bool							Recharge(bool bOn);			// Recharging on/off
	bool							Shift(int nFrom, int nTo, int nPower);



public:

	bool							m_bPowered;		// Shield Unit powered up
	bool							m_bEmitter;		// Shield emitter online
	CShieldState*					m_Shieldstate;	// Banks and Integrity


	CSet*							m_Orientation;	// Arc emitter works in
			
};

///////////////////////////////////////////////////////////////////////////////////////////
// 3a. Powerplant State Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships powerplant state
///////////////////////////////////////////////////////////////////////////////////////////
class CPowerState
{
public: 
	CPowerState();
	~CPowerState();

public:
	float m_nSublight;
	float m_nManeuver;
	float m_nWeapons;
	float m_nShields;
	float m_nFree;


};

///////////////////////////////////////////////////////////////////////////////////////////
// 3. Engineering Space Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships powerplant
///////////////////////////////////////////////////////////////////////////////////////////
class CEngspace : public CComponent
{
public: 
	CEngspace();
	~CEngspace();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Reactor Core Unit: Determines the amount of energy the powerplant can stockpile (*)
	CModule*					Reactor()				{ return (*m_Modules.find(CModule::MT_REACTOR_PLANT)).second; }
	// Coolant Plant: Used to produce high amounts of Coolant for cooling systems
	CModule*					Coolant()				{ return (*m_Modules.find(CModule::MT_COOLANT_PLANT)).second; }
	// Fuel Line: Determines the amount of Fuel Consumption
	CModule*					Fuel()					{ return (*m_Modules.find(CModule::MT_FUEL_LINE)).second; }
	// Battery Bank: Used to store energy on the ship, determines max energy store
	CModule*					Battery()				{ return (*m_Modules.find(CModule::MT_BATTERY_BANK)).second; }


		
public:

	// :: Variables ::
	// Coolant Load : The total amount of Coolant consumed every Spatial Tick
	// Coolant Rate : The amount of Coolant produced by the Coolant Plant every Spatial Tick
	// Reactor Rate : The rate of fuel cell consumption by the Reactor
	// Recharge Rate: The amount of energy produced by the Reactor every Spatial Tick
	// System Load  : The total amount of energy consumed from the battery every Spatial Tick
	
};

	
///////////////////////////////////////////////////////////////////////////////////////////
// 4. Sublight Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships impulse drive
///////////////////////////////////////////////////////////////////////////////////////////
class CSublight : public CComponent
{
public: 
	CSublight();
	~CSublight();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Ion Engine: Provides the drive with thrust
	CModule*					Ion()						{ return (*m_Modules.find(CModule::MT_ION_ENGINE)).second; }
	// Power Link: Powers the Engine
	CModule*					Power()						{ return (*m_Modules.find(CModule::MT_POWER_LINK)).second; }
	// Coolant line: Cools the Engine
	CModule*					Coolant()					{ return (*m_Modules.find(CModule::MT_COOLANT_LINE)).second; }
	// Drive Baffles: Masks the Engines Ion signature
	CModule*					Baffles()					{ return (*m_Modules.find(CModule::MT_DRIVE_BAFFLES)).second; }


public:


};

///////////////////////////////////////////////////////////////////////////////////////////
// 5. Hyperdrive Mounting Bracket Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Allows Hyperdrive(s) to be mounted to the Ship
///////////////////////////////////////////////////////////////////////////////////////////
class CHyperdrive : public CComponent
{
public: 
	CHyperdrive();
	~CHyperdrive();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Main Drive: Big Hyperdrive...
	CModule*					Main()						{ return (*m_Modules.find(CModule::MT_HYPERDRIVE_MAIN)).second; }
	// Backup Drive: Little Hyperdrive
	CModule*					Backup()					{ return (*m_Modules.find(CModule::MT_HYPERDRIVE_BACKUP)).second; }

public:


};

///////////////////////////////////////////////////////////////////////////////////////////
// 6. Magazine Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Stores all the weapon and ECM ammunition
///////////////////////////////////////////////////////////////////////////////////////////
class CMagazine : public CComponent
{
public: 
	CMagazine();
	~CMagazine();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Armour Plating: Magazine has its own very special set of armour to prevent bad things happening (USS Arizona.. PH?)
	CModule*					Armour()						{ return (*m_Modules.find(CModule::MT_ARMOUR_PLATING)).second; }
	// Missiles: Store of Missile Ordinance
	CModule*					Missiles()						{ return (*m_Modules.find(CModule::MT_MISSILES)).second; }
	// Torpedoes: Store of Torpedo ordinance
	CModule*					Torpedoes()						{ return (*m_Modules.find(CModule::MT_TORPEDOES)).second; }
	// Rockets: Store of Rockets
	CModule*					Rockets()						{ return (*m_Modules.find(CModule::MT_ROCKETS)).second; }
	// Chaff: Store of Chaff packets for the ECM
	CModule*					Chaff()							{ return (*m_Modules.find(CModule::MT_CHAFF)).second; }
	// Flares: Store of Magnezium Flares for the ECM
	CModule*					Flares()						{ return (*m_Modules.find(CModule::MT_FLARES)).second; }

public:


};


///////////////////////////////////////////////////////////////////////////////////////////
// 7. External Systems Mount Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Used to fit external parts for many internal systems
///////////////////////////////////////////////////////////////////////////////////////////
class CExternal : public CComponent
{
public: 
	CExternal();
	~CExternal();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Collector Array: Massive array that allows solar recharging of the battery
	CModule*					Collector()						{ return (*m_Modules.find(CModule::MT_COLLECTOR_ARRAY)).second; }
	// Radome: Houses sensor equipment
	CModule*					Radome()						{ return (*m_Modules.find(CModule::MT_RADOME)).second; }
	// Maneuvering Thrusters: Used to manuever ship
	CModule*					Thrusters()						{ return (*m_Modules.find(CModule::MT_MANUEVERING_THRUSTERS)).second; }
	// Heatsink: Secondary method of heat dissipation
	CModule*					Heatsink()						{ return (*m_Modules.find(CModule::MT_HEATSINK)).second; }
	// Gravity Wells: Used to produce aritificial gravity
	CModule*					Gravwells()						{ return (*m_Modules.find(CModule::MT_GRAVITY_WELL_PROJECTORS)).second; }
	// Flare/Chaff Launcher: Used to prevent missile locks
	CModule*					FlareChaff()					{ return (*m_Modules.find(CModule::MT_FLARE_AND_CHAFF_DISPENSOR)).second; }
	// Comms Array: Used as an antenna for the Comms
	CModule*					Comms()							{ return (*m_Modules.find(CModule::MT_COMMUNICATIONS)).second; }
	// Jamming Pod: Allows Jamming of Frequencies
	CModule*					Jamming()						{ return (*m_Modules.find(CModule::MT_JAMMING_POD)).second; }
	// Snooping Pod: Allows monitoring of Frequencies
	CModule*					Snooping()						{ return (*m_Modules.find(CModule::MT_SNOOPING_POD)).second; }
	// Power Link: Allows power to Mount
	CModule*					Power()							{ return (*m_Modules.find(CModule::MT_POWER_LINK)).second; }
	
public:


};

///////////////////////////////////////////////////////////////////////////////////////////
// 8. Internal Systems Mount Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Used to fit internal systems
///////////////////////////////////////////////////////////////////////////////////////////
class CInternal : public CComponent
{
public: 
	CInternal();
	~CInternal();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Astrogation Computer: Used for navigating Hyperspace 
	CModule*					Astrogation()					{ if (*m_Modules.find(CModule::MT_ASTROGATION_COMPUTER) != *m_Modules.end()) { return (*m_Modules.find(CModule::MT_ASTROGATION_COMPUTER)).second; } else { return NULL; } }
		// Repulsor Coil: Used to allow ships to operate within gravity
	CModule*					Repulsor()						{ return (*m_Modules.find(CModule::MT_REPULSOR_COILS)).second; }
	// Life-Support: Allows life
	CModule*					LifeSupport()					{ if (*m_Modules.find(CModule::MT_LIFE_SUPPORT_UNIT) != *m_Modules.end()) { return (*m_Modules.find(CModule::MT_LIFE_SUPPORT_UNIT)).second; } else { return NULL; } }
	// Power Link: Allows power to Mount
	CModule*					Power()							{ return (*m_Modules.find(CModule::MT_POWER_LINK)).second; }
	// Holonet Transceiver: Allows use of the Holonet for messages
	CModule*					Holonet()						{ return (*m_Modules.find(CModule::MT_HOLONET_TRANSCEIVER)).second; }
	
public:


};

///////////////////////////////////////////////////////////////////////////////////////////
// 9. Landing Bay Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Allows Vessels to land within another ship
///////////////////////////////////////////////////////////////////////////////////////////
class CLanding : public CComponent
{
public: 
	CLanding();
	~CLanding();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Blast Doors: Allow the landing bay to be shutdown
	CModule*					Blastdoors()					{ return (*m_Modules.find(CModule::MT_BLAST_DOORS)).second; }
	// Tractor Beam: Allows capturing of other vessels
	CModule*					Tractor()						{ return (*m_Modules.find(CModule::MT_TRACTOR_BEAM_PROJECTOR)).second; }
	
	friend std::ostream& operator << ( std::ostream& stream, const CLanding& land );
	friend std::istream& operator >> ( std::istream& stream, CLanding& land);


public:


};

///////////////////////////////////////////////////////////////////////////////////////////
// 10. Bulk Storage
///////////////////////////////////////////////////////////////////////////////////////////
// Storage area of ship
///////////////////////////////////////////////////////////////////////////////////////////
class CBulk : public CComponent
{
public: 
	CBulk();
	~CBulk();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Fuel Cells: Used for powering the ship's reactor(s)
	CModule*					Fuel()							{ return (*m_Modules.find(CModule::MT_FUEL_CELLS)).second; }
	// Fabrication Material: Facilitates repairs
	CModule*					Material()						{ return (*m_Modules.find(CModule::MT_FABRICATION_MATERIAL)).second; }

public:


};
	
///////////////////////////////////////////////////////////////////////////////////////////
// 11. Control Point
///////////////////////////////////////////////////////////////////////////////////////////
// Allows different control mechanisms of the ship to be accessed
///////////////////////////////////////////////////////////////////////////////////////////
class CControl : public CComponent
{
public: 
	CControl();
	~CControl();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);
/*
	// Sub-Component fetching
	// Helm: Allows the ship to be maneuered
	CModule*					Helm()							{ return (*m_Modules.find(CModule::MT_HELM)).second; }
	// Systems Terminal: Allows status on the ship to be attained
	CModule*					Systems()						{ return (*m_Modules.find(CModule::MT_SYSTEMS_TERMINAL)).second; }
	// Sensor Terminal: Allows manipulation of sensors
	CModule*					Sensor()						{ return (*m_Modules.find(CModule::MT_SENSOR_TERMINAL)).second; }
	// Comms Terminal: Allows comms to be manipulated
	CModule*					Comms()							{ return (*m_Modules.find(CModule::MT_COMMS_TERMINAL)).second; }
	// Astrogation Terminal: Allows plotting of Nav courses
	CModule*					Astrogation()					{ return (*m_Modules.find(CModule::MT_ASTROGATION_TERMINAL)).second; }
	// Power Link: Allows the Systems power
	CModule*					Power()							{ return (*m_Modules.find(CModule::MT_POWER_LINK)).second; }
*/
	
public:


};	

///////////////////////////////////////////////////////////////////////////////////////////
// 12. Weapon Mount
///////////////////////////////////////////////////////////////////////////////////////////
// Allows the mounting of Weapon Systems
///////////////////////////////////////////////////////////////////////////////////////////
class CWeapon : public CComponent
{
public: 
	CWeapon();
	~CWeapon();


	virtual CWeapon&	operator =		(CWeapon& clone);

	virtual void					Update();				// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);

	// Sub-Component fetching
	// Armour Plating: Extra protection for turret
	CModule*					Armour()						{ return (*m_Modules.find(CModule::MT_ARMOUR_PLATING)).second; }
	// Turret Mount: Actual weapon mounted
	CModule*					Weapon()						{ return (*m_Modules.find(CModule::MT_TURRET_MOUNT)).second; }
	// Munition Elevator: Supplies mount with ammunition from the Magazine
	CModule*					Munition()						{ return (*m_Modules.find(CModule::MT_MUNITION_ELEVATOR)).second; }
	// Power Link: Allows the weapon power
	CModule*					Power()							{ return (*m_Modules.find(CModule::MT_POWER_LINK)).second; }
	// Coolant Line: Cools the weapon
	CModule*					Coolant()					{ return (*m_Modules.find(CModule::MT_COOLANT_LINE)).second; }

public:

	CSet*						m_Orientation;						// The arcs turret can fire in
};		

///////////////////////////////////////////////////////////////////////////////////////////
// 13. Escape Pod housing
///////////////////////////////////////////////////////////////////////////////////////////
// Allows the mounting on an escape pod
///////////////////////////////////////////////////////////////////////////////////////////
class CEscape : public CComponent
{
public: 
	CEscape();
	~CEscape();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);
	
	// Sub-Component fetching
	// Escape Pod: The Pod for escape 
	CModule*					Pod()						{ return (*m_Modules.find(CModule::MT_ESCAPE_POD)).second; }

public:


};	


///////////////////////////////////////////////////////////////////////////////////////////
// 14. S-Foil System
///////////////////////////////////////////////////////////////////////////////////////////
// Allows the mounting on an escape pod
///////////////////////////////////////////////////////////////////////////////////////////
class CSFoil : public CComponent
{
public: 
	CSFoil();
	~CSFoil();

	virtual void					Update();					// Update has happened, what do we do?
	virtual void WriteExtra (TiXmlNode* pParent);
	virtual void ReadExtra (TiXmlNode* pParent);
	

	// Sub-Component fetching
	// S-Foil actuator: The S-Foil working parts
	CModule*					Actuator()						{ return (*m_Modules.find(CModule::MT_SFOIL_AND_ACTUATORS)).second; }

public:


};	

///////////////////////////////////////////////////////////////////////////////////////////
// Contact Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a Radar Contact. Used to allow
// for passive radars that do not always hold the most up-to-date radar information.
// I.e. they may have picked up a contact at 1000 nm however, they have not made any pings 
// or received anything that has relocated this contact so their radar may show it as being 
// at 1000 nm when it may actually be closer/further away.
///////////////////////////////////////////////////////////////////////////////////////////
class CContact 
{
public:
	CContact();
	~CContact();

	bool	operator ==		( CContact a2 );

public:
	CSpatialID*						m_Spatial;							 // Contact's details
	CCart*							m_Location;							 // Contact's Location
	int								m_Signature[CSpatial::SI_MAX];	 	 // Signature
	
};
typedef std::map<int,CContact*> ContactMap;

///////////////////////////////////////////////////////////////////////////////////////////
// Weapon Group Class
///////////////////////////////////////////////////////////////////////////////////////////
// The Weapon Group class is used to allow Captains of Ships to config their weapon setup
// it allows them to group weapons into a single battery and then to fire all weapons within
// this battery at once. Allows for devastating broadsides and generally makes it easier
// to manage ships with huge amounts of weaponry. This class stores a single group of Weapons
///////////////////////////////////////////////////////////////////////////////////////////
class CWeaponGroup
{
public:
	CWeaponGroup();
	~CWeaponGroup();

	typedef enum e_WeaponGroupTypes
	{
		_ANTISTARFIGHTER	= 0,
		_ANTICAPITAL		= 1,
		_ANTIORDINANCE		= 2,
		_ECM				= 3,
		_NUMWEAPONGROUP	
	};

	static char *szTypes[];

	CWeaponGroup&	operator =		(CWeaponGroup& clone);

	friend std::ostream&			operator << ( std::ostream& stream, const CWeaponGroup& wep );
	friend std::istream&			operator >> ( std::istream& stream, CWeaponGroup& wep );

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);



public:

	gString							m_gsName;			// Name of the group
	CSet*							m_Orientation;		// Arcs the group covers
	int								m_nType;			// Type of the Arc (from e_WeaponGroupTypes)
	IntegerList						m_Weapons;			// List of Weapons in the group
};
typedef std::vector<CWeaponGroup*> WeaponGroupList;

///////////////////////////////////////////////////////////////////////////////////////////
// 8 Ship Class
///////////////////////////////////////////////////////////////////////////////////////////
// The Ship class is used to represent the different types of ships which are all spatial objects.
// The Ship class acts as a storage wrapper for the different components that make up a ship.
// The Ship destruction method is used to determine what debris is created and what modules are 
// contained within the debris.
///////////////////////////////////////////////////////////////////////////////////////////
class CShip : public CSpatial , public CMemoryDebugBase//, public CAtomic
{

public:
	CShip();
	CShip(gString name, gString filename);
	~CShip();

	//  ShipEvents
	//  These are events that all Ships will handle
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ShipEvents
	{
		EV_NOTICE				=  EVRANGE_FIRST_SHIP,
		EV_SHIELDSDOWN,			// Shields have just been knocked offline
		EV_SHIELDSUP,			// Shields have just been powered up
		EV_TARGETTED,			// Ship has just been targetted
		EV_TARGET,				// Ship has just targetted something else
		EV_DAMAGE,				// Ship has just taken damage
		EV_RECHARGED,			// Ship has recharged all weapons
		EV_NEWCONTACT,			// Ship has detected a new ship
		EV_LANDED,				// Ship has just landed
		EV_LAST_ACTOR_EVENT		=  EVRANGE_LAST_SHIP
	};

	//  ShipTypes
	//  These are the type of ships
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ShipTypes
	{
		ST_FIGHTER   = 0,
		ST_SMALLSHIP = 1,
		ST_MIDSHIP   = 2,
		ST_LARGESHIP = 3,
		ST_CAPITAL   = 4,
		ST_STATION   = 5,
		ST_MAX
	};

	//  EnergyTypes
	//  These are the energy types that we can determine
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_EnergyTypes
	{
		ET_MIN			= 0,
		ET_IDLE			= 1,
		ET_NORMAL		= 2,
		ET_COMBAT		= 3,
		ET_MAX			= 4,
		ET_CURRENT		= 5
	};

	//  State
	//  These are the states a ship can be in
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_State
	{
		_LANDED				= 0,
		_REPULSOR			= 1,
		_ATMOSPHERE			= 2,
		_LANDING			= 3,
		_LAUNCHING			= 4,
		_FLYING				= 5,
		_GQ					= 6,	// General quarters
		_NUMSTATES
	};

	//  Arcs
	//  These are the different valid arcs of a ship
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_Arcs
	{
		A_FORE				= 0,
		A_AFT				= 1,
		A_STARBOARD			= 2,
		A_PORT				= 3,
		A_VENTRAL			= 4,
		A_DORSAL			= 5,
		A_MAX				= 6
	};

	//  SCM
	//  These are the different SCM toggles
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_SCM
	{
		SCM_REACTOR			= 1,
		SCM_COOLANT			= 2,
		SCM_OVERHEAT		= 3,
		SCM_BULKHEAD		= 4,
		SCM_ION				= 5,
		SCM_LOADSHED		= 6,
		SCM_SELFDESTRUCT	= 7,
		SCM_MAX				= 8
	};

	//  WeaponTypes
	//  These are the different ship weapon types
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_WeaponTypes
	{
		// >> BLASTER CANNON
		WT_REPEATING_BLASTER = 0,
		WT_BLASTER_CANNON	 = 1,
		WT_AUTO_CANNON		 = 2,
		// >> LASER CANNON
		WT_COMPACT_CANNON	 = 3,
		WT_LASER_CANNON		 = 4,
		// >> TURBOLASER
		WT_TURBOLASER		 = 5,
		WT_HEAVY_TURBOLASER  = 6,
		// >> ION CANNONS
		WT_ION_CANNON        = 7,
		WT_HEAVY_ION_CANNON  = 8,
		// >> WARHEADS
		WT_MISSILE           = 9,
		WT_TORPEDO           = 10,
		WT_ROCKET            = 11,
		WT_MAX
	};

	//  TurretTypes
	//  These are the different turret types that can be used
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_TurretTypes
	{
		TT_SINGLE			= 0,
		TT_DUAL				= 1,
		TT_QUAD				= 2,
		TT_MAX
	};

	//  Ordinance
	//  These are the ordinance types that can be used
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_Ordinance
	{
		OR_MISSILE			= 0,
		OR_TORPEDO			= 1,
		OR_ROCKET			= 2,
		OR_TIBANA			= 3,
		OR_CHAFF			= 4,
		OR_FLARE			= 5,
		OR_SLUG				= 6,
		OR_MAX
	};

	//  Exit States
	//  These are the states a ship's exit can be in
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_ExitState
	{
		ES_CLOSED			= 0,
		ES_OPEN				= 1,
		ES_LOCKED			= 2,
		ES_MAX
	};

	//  PowerType
	//  These are the power categories
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_PowerType
	{
		PT_DRIVE			= 0,
		PT_SHIELD   		= 1,
		PT_CAPACITOR		= 2,
		PT_SYSTEMS			= 3,
		PT_REPULSOR			= 4,
		PT_GRAVWELL			= 5,
		PT_ECM				= 6,
		PT_WEAPON			= 7,
		PT_BATTERY			= 8,
		PT_MAX
	};

	//  Sensors
	//  These are the types of sensors a ship can mount
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_Sensors
	{
		S_HEAT				= 0,
		S_EM				= 1,
		S_ION				= 2,
		S_MASS				= 3,
		S_TACT				= 4,
		S_MAX
	};

	//  MessageTypes
	//  These are the message types that can be sent and received by a ship
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_MessageTypes
	{
		MT_ENTIRESHIP		= 0,
		MT_HELM				= 1,
		MT_LEEHELM			= 2,
		MT_SENSORS			= 3,
		MT_ASTROGATION		= 4,
		MT_COMMS			= 5,
		MT_SYSTEMS			= 6,
		MT_FCS				= 7,
		MT_CRITICAL			= 8,
		MT_HULLCUBE			= 9,
		MT_MAX
	};


	static char *szTypes[];
	static char *szSensors[];
	static char *szOrdinance[];
	static char *szStates[];
	static char *szScm[];
	static char *szArc[];
	static char *szTurretTypes[];
	static char *szWeaponTypes[];
	static int  nMinDamage[];
	static int  nMaxDamage[];
	static int  nAmmoType[];

	CShip&	operator =		(CShip& clone);

	virtual bool			Save();
	virtual bool			Load();

	virtual bool			Load(gString filename);
	virtual bool			Contains(CSpatial* pS);						// Check to see if Contact already exists
	virtual bool			Destroyed();								// Checks if the ship has been totally destroyed
	virtual void			Update();									// Update Behaviour function
	virtual void			Destroy();									// Destroy Behaviour function
	virtual void			Damage(int nA, int nArc);					// Damage the ship
	bool					DamageShield(int nA, int n);				// Damage the Shields in an Arc
	virtual void			Write(int n, char *f, ...);					// Message passing for the Ship
	virtual void			NotifySpace(CSpatial* pS, char *f, ...);	// Message to space
	virtual void			NotifySpace(char *f, ...);					// Message to space
		
	// Ship compound data methods
	int						Mass();										// Returns ship's mass
	int						TopSpeed(bool bCurr);						// Returns the Top speed of the ship	
	int						Acceleration(bool bCurr);					// Returns the Acceleration of the Ship
	int						Shield(bool bType);							// Returns the Current Shield value
	int						Shield(int nT, bool bType);					// Returns the Shield Value for an Arc
	int						Capacitors(bool bType);						// Returns the Shield Capacitor Charge
	int						Crew(bool bCurr, int nType);				// Returns the Current Crew number
	int						Battery(bool bCurr);						// Returns the Battery Charge
	int						Maneuver(bool bCurr);						// Returns the Maneuver ability of the ship
	int						Coolant(bool bCurr);						// Returns the total Coolant Load of the ship

	// Power states
	int						Energy(int nType);							// Returns the Energy Load (Min, Max, Normal, Combat)
	int						CurrPower();								// Returns the current Power usage
	int						MaxPower();									// Returns maximum power usage
	int						FreePower();								// Returns free power capacity
	
	// Ship functions
	int						Fire(ModuleList ml, CSpatial* pT, int nA);	// Fire weapons at a target
	bool					ConsumeFuel(int nAmount);					// Consume some fuel from the ship stores
	bool					DrainBattery(int nAmount);					// Reduce the amount of charge a battery has
	bool					Reload(int nType);							// Reload a Weapon Mount from a Magazine

	// Communication methods
	bool					IsOpen(CFrequency* f);						// Is the Channel open?
	bool					CanDecrypt(gString p);						// Can the ship decrypt it?
	bool					IsJammed(CFrequency* f);					// Is this ship jamming this Frequency?
	bool					IsSnooped(CFrequency* f);					// Is this ship snooping this Frequency?
	
	// Crew methods
	bool					GiveEvent(int n, CEvent* p);				// Give a Crew Type an Event

	// Module methods
	bool					IsType(int n, CModule* m);					// Is this module of the specific power type
	ModuleList*				Get(int nModType);							// Returns a Module or Null if not found
	CModule*				GetModule(gString n, gString c);			// Returns a Module
	CModule*				GetModule(int n);							// Returns a Module by Index

	// Component methods
	CComponent*				GetComponent(gString n);					// Returns a Component
	CComponent*				GetComponent(int n);						// Returns a Component by Index
	CComponent*				GetModComp(gString n);						// Returns the Component of a Module
	CComponent*				GetModComp(CModule* m);						// Returns the Component of a Module
	
	void					WriteXml(TiXmlNode* pParent);
	void					ReadXml(TiXmlNode* pParent);

public:
// Commented out fields are inherited from CSpatial
//	gString					m_gsName;					// Name of Ship		e.g. Rogue Leader
//	gString					m_gfFileName;				// Filename of Ship e.g. Rogue.shi
	int						m_nClass;					// Ship class		e.g. SC_STARFIGHTER
	gString					m_gsType;					// Type of vessel	e.g. Incom t65 X-wing A2
	gString					m_gsDesignation;			// Short Type: 	    e.g. X/Wt65a2
//	gString					m_gsSector;					// Sector of Ship   e.g. Coruscant Sector


	int						m_nCoolant;					// Coolant Store
	int						m_Speed;					// Current Speed
	int						m_dSpeed;					// Destination Speed

	// State and Changes
	CSet*					m_ShipState;				// Current State of ship
	CSet*					m_Scm;						// Safety Control Mechanism settings
	int						m_nTimer;					// Current State Timer

	// Power
	int						m_nPower[8];				// Current Power States
		
	// Area
	CArea*					m_Area;						// Area of Vessel
	int						m_nArea;					// Area Vnum

	// Commander
	CActor*					m_Commander;				// Ship Commander

	// Exit Ramp
	gString					m_gsExit;					// Name of Exit Type: e.g Boarding Ramp, Canopy
	gString					m_gsOMsg;					// Opening message
	gString					m_gsCMsg;					// Closing message
	int						m_nKeycode;					// To lock the ship
	int						m_nExit;					// Exit Room
	int						m_nExitState;				// Exit State

	// Shields
	CShieldState*			m_Shields;					// Shield configuration

	// Weapon Groups
	WeaponGroupList			m_Weapons;					// List of all Weapon Groups

	// Damage Timers
	int						m_nMeltdown;				// Meltdown timer
	int						m_nCollapse;				// Ion Collapse timer
	int						m_nSelfdestruct;			// Selfdestruct timer

	// Landed
	CPlacement				m_Land;						// If the ship is landed

	// Crew
	CrewMap					m_Crew;						// Crew list

	// Heading settings
	CCart*					m_Heading;					// Ship's current heading vector
														// Also used for Facing where:
														// X = XY Y = YZ Z = XZ
	CCart*					m_dHeading;					// Ship's final heading vector
	
	FrameList				m_Frames;					// TruShip Representation
															// Contains:
															// [1] Hull cubes
															//  [2] Component Space
															//   [3] Modules

	// Targetting
	int						m_nTarget;					// Timer for Targetting
	CSpatialID*				m_Target;					// Target

	// Sensors 
	ContactMap				m_Contacts;					// Contacts
	int						m_nSweep;					// Timer for Sweep
	int						m_Sweep[S_MAX];				// Sweep sensors
	
};

typedef std::vector<CSpatialID*> ShipList;

///////////////////////////////////////////////////////////////////////////////////////////
// Position Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is used as part of the switch to Morton Codes for storing lists. The Position
// class is used to hold the value part of our map
///////////////////////////////////////////////////////////////////////////////////////////
class CPosition
{
public:
	CPosition();
	~CPosition();

public:
	unsigned int					m_nHighMC;			// The High code for this object
	CPosition*						m_pNext;			// Next in list
	SpatialList						m_Contents;			// The Spatial Objects contained at this location

};
typedef std::map<unsigned int, CPosition*>PositionMap;

class CPositionMap
{
public:
	CPositionMap();
	~CPositionMap();

	SpatialList*					GetRange(int x, int y, int z, int range);
	SpatialList*					Search(int MCcode);
	bool							Insert(CSpatial *pSpatial);
	bool							Erase(CSpatial *pSpatial);
	unsigned int					LowMC(int x, int y);	// Returns the first half of the morton code
	unsigned int					HighMC(int x, int y);	// Returns the second half of the morton code
	gString							Binary(int x, int bits);// Returns the binary representation of a code

public:
	PositionMap						m_Spatial;

};

CSpatial*	operator *			    ( CSpatialID& pId );

// Spatial ID Class
class CSpatialID
{
public:
	CSpatialID();
	~CSpatialID();

public:
	friend	CSpatial*	operator *			    ( CSpatialID& pId );

	int								m_nVnum;

};

#endif

