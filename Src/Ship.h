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

// Header   :: Ship.h
// Function :: Handles the classes for all Space Ship Components and Ship structure

#ifndef __SHIP_H__
#define __SHIP_H__

#include <vector>

#pragma warning(disable: 4251)

#include "MudCore.h"
#include "OTools.h"
#include "Set.h"
#include "Module.h"
#include "Spatial.h"

///////////////////////////////////////////////////////////////////////////////////////////
// Ship Class
///////////////////////////////////////////////////////////////////////////////////////////
// The Ship class is used to represent the different types of ships which are all spatial objects.
// The Ship class acts as a storage wrapper for the different components that make up a ship.
// The Ship destruction method is used to determine what debris is created and what modules are 
// contained within the debris.
///////////////////////////////////////////////////////////////////////////////////////////
class CShip : public CSpatial
{

public:
	CShip();
	CShip(gString name, gString filename);
	~CShip();

	typedef enum e_ShipTypes
	{
		ST_STARFIGHTER = 0,
		ST_FREIGHTER = 1,
		ST_CAPITALSHIP = 2,
		ST_MAX
	};

	static char *szTypes[];

	virtual bool					Save();
	virtual bool					Load();

	virtual bool					Load(gString filename);
	virtual void					Update();					// Update Behaviour function
	virtual void					Destroy();					// Destroy Behaviour function
	virtual void					Notify(gString gsMsg);		// Message passing
	virtual bool					Contains(CSpatial* pS);     // Check to see if Contact already exists

	friend std::ostream&			operator << ( std::ostream& stream, const CShip& ship );
	friend std::istream&			operator >> ( std::istream& stream, CShip& ship );

public:
// Commented out fields are inherited from CSpatial
//	gString						   m_gsName;					// Name of Ship		e.g. Rogue Leader
//	gString						   m_gfFileName;				// Filename of Ship e.g. Rogue.ship
	int							   m_nClass;					// Ship class		e.g. SC_STARFIGHTER
	gString						   m_gsType;					// Type of vessel	e.g. Incom t65 X-wing
	gString						   m_gsDesignation;				// Short Type: 	    e.g. X/Wt65
//	gString						   m_gsSector;					// Sector of Ship   e.g. Coruscant Sector

	CCart*						   m_Heading;					// Ship's current heading vector
	CCart*						   m_dHeading;					// Ship's final heading vector

	
	CShape*					   	   m_Shape;						// Shape of Vessel  

	// Fuel Storage
	int							   m_nFuel;
	int							   m_nEnergy;
	int							   m_Speed;

	// Contacts
	ContactMap						m_Contacts;		// Contacts

	// Signatures (handled by Parent class)

	// Engines and Power
	CImpulse*					   m_Impulse;					// Impulse Engines
	CHyper*		                   m_Hyper;						// Hyperdrive
	CPowerplant*                   m_PrimPowerplant;			// Primary Powerplant
	CPowerplant*                   m_BackPowerplant;			// Backup Powerplant
	CManeuver*                     m_Maneuver;					// Manuever Thrusters
	CSFoils*				       m_SFoils;					// S-Foils
		
	// Ship Systems	
	CRadar*						   m_Radar;						// Radar Suite
	CSensor*					   m_Sensor;					// Sensor Array
	CComms*		                   m_Comms;						// Communications Array
	CNavi*						   m_Navi;						// Navigational Computer
	/*TractorList					   m_TractorList;				// List of Installed Tractor Beams
	CComponent*                    m_Gravwell;					// Gravity Well projectors
	CComponent*                    m_Battle;					// Battle Control/Command Computer
	CComponent*                    m_Fighter;					// Fighter Control/Command Computer
	CComponent*                    m_Target;					// Targetting Computer
	
	// Defence Suites
	CComponent*                    m_Flare;						// Flare Launcher
	CComponent*                    m_Chaff;						// Chaff Dispensor
	CComponent*					   m_Jammer;					// Warhead Jamming Unit
	CComponent*                    m_PrimShield;				// Primary Shield Bank
	CComponent*                    m_BackShield;				// Secondary Shield Bank

	// Physical Components
	CComponent*                    m_Bay;						// Ordinance Bay
	CComponent*                    m_PrimBridge;				// Primary Bridge
	CComponent*                    m_BackBridge;				// Backup Bridge
	CComponent*                    m_DockingRing;				// Docking Ring	
	CComponent*                    m_CargoBay;					// Cargo Bay
	ComponentList				   m_EscapeList;				// Escape Pods list
	LandingList					m_LandingList;				// Landing Locations

	// Weapon Platforms
	LauncherList                m_LauncherList;					// Warhead Launchers
	TurretList					m_Port;							// Turret Battieries: Port
	TurretList					m_Starboard;					// Turret Battieries: Starboard
	TurretList					m_Prow;							// Turret Battieries: Prow
	TurretList					m_Stern;						// Turret Battieries: Stern
*/
};

typedef std::vector<CShip*> ShipList;

#endif