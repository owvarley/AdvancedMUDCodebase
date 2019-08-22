
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

#ifndef __COMPONENTS_H__
#define __COMPONENTS_H__

#include <vector>
#include <map>

#pragma warning(disable: 4251)

#include "MudCore.h"
#include "Set.h"
#include "OTools.h"
#include "Module.h"




///////////////////////////////////////////////////////////////////////////////////////////
// 0. Component Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is used for all Components which do not have specific modular parts contained
// within them. This class facilitates the loading/saving and other methods to be carried
// out on objects of type 'component'
///////////////////////////////////////////////////////////////////////////////////////////
class CComponent
{

public:
	CComponent();
	~CComponent();

	//  Component Types
	typedef enum e_ComponentTypes
	{
		CT_IMPULSE			= 0,
		CT_HYPERDRIVE		= 1,
		CT_POWERPLANT		= 2,
		CT_MANEUVER			= 3,
		CT_SFOILS			= 4,
		CT_RADAR			= 5,
		CT_SENSOR			= 6,
		CT_COMMS			= 7,
		CT_NAVI				= 8,
		CT_FLARE			= 9,
		CT_CHAFF			= 10,
		CT_JAMMING			= 11,
		CT_SHIELD			= 12,
		CT_TRACTOR			= 13,
		CT_GRAVWELL			= 14,
		CT_BATTLE			= 15,
		CT_FIGHTER			= 16,
		CT_TARGETTING		= 17,	
		CT_HULL				= 18,
		CT_LANDING			= 19,
		CT_POD				= 20,
		CT_BRIDGE			= 21,
		CT_ORDINANCE		= 22,
		CT_CARGO			= 23,
		CT_LAUNCHER			= 24,
		CT_TURRET			= 25,
		CT_LASTCT		
	};

	static char *szComponents[];
	static char *szClassnames[];

	virtual bool					Load(gString file);		// Load the Component into Memory
	virtual bool					Save();					// Save the Component
	virtual bool					Install(gString pShip);	// Install the Component
	virtual	bool					Uninstall();			// Remove the Component
	virtual bool					Tamper();				// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);	// Damage the Component
	virtual int						Integrity();			// Calculate Component's integrity
	virtual void					Update();				// Update has happened, what do we do?

	friend std::ostream& operator << ( std::ostream& stream, const CComponent& component );
	friend std::istream& operator >> ( std::istream& stream, CComponent& component );

public:

	gString							m_gsName;				// Component's name
	gString							m_gfFileName;			// File name
	gString							m_Ship;					// Ship it is installed on

	CCart*							m_Location;				// Where Component is installed
	CSet*							m_Type;					// The Type of Component
	int								m_Size;					// SF 0, Freighter 1, Cap 2

	
	int								m_nmIntegrity;			// 250-1000 damage done to Component
	int								m_ncIntegrity;			// Current integrity
	bool							m_bTampered;			// Has it been tampered?

	int								m_nTechLevel;			// 1-10 Tech Level of Component
	int								m_nValue;				// Value derived from Tech Level
	int								m_nModifier;			// Components Modifier
		
};

typedef std::vector<CComponent*> ComponentList;

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Component Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is held by the GameWorld object and is simply used to load in all the
// Components and hold a list of all currently created components.
//////////////////////////////////////////////////////////////////////////////////////////
/*
class CCLoader
{
public:
	CCLoader();
	~CCLoader();

	bool							Load(gFileName gfFile);	// Load method
	bool							Save();					// Save method

public:
	ComponentList					m_Components;			// List of all components

}; */


	
///////////////////////////////////////////////////////////////////////////////////////////
// 2. Impulse Class	
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships impulse drive
///////////////////////////////////////////////////////////////////////////////////////////
class CImpulse : public CComponent
{
public: 
	CImpulse();
	~CImpulse();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?

	// Class Specfic Methods
	bool							Speed(int speed);			// Change Speed
	bool							Overcharge(bool bOn);		// Engage Overcharging

	friend std::ostream& operator << ( std::ostream& stream, const CImpulse& imp );
	friend std::istream& operator >> ( std::istream& stream, CImpulse& imp);


public:

	bool							m_bPowered;
	bool							m_bOver;

	int								m_nSpeed;		// Current Speed
	int								m_dSpeed;		// Destination Speed
	int								m_nConsumption;	// Energy Consumption Rate
	int								m_Heat;			// Heat this engine produces per output unit (speed)

	int								m_cHeat;		// Heat being produced. Too much heat will cause engine integrity to fail
	int								m_nHeat;		// Nominal Heat Level (anything above causes damage)
	int								m_mHeat;		// Max Heat engine can reach
	

	// m_nValue;				// Determines max Speed this engine can attain
	// m_nModifier;				// Determines Acceleration

	// Components
	CModule*						m_Cooling;		// Cooling Unit
													// Determines the rate of Heat creation
	CModule*						m_Fcu;			// Fuel Control Unit
													// Determines the Energy consumption rate from ship's stores
	CModule*						m_Core;			// Fusion Reactor Core
													// Increases Speed
	CModule*						m_Coils;		// Driver Coils
													// Affects Decel/Accel
	CModule*						m_Power;		// Power Regulator
													// Determines maximum heat level engines can reach

};

///////////////////////////////////////////////////////////////////////////////////////////
// 3. Hyper Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships hyperdrive
///////////////////////////////////////////////////////////////////////////////////////////
class CHyper : public CComponent
{
public: 
	CHyper();
	~CHyper();



	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?


	// Class Specfic Methods
	bool							Engage();				// Engage drive
	bool							Disengage();			// Disengage drive
	bool							Speed(int nSpeed);		// Regulate Speed
	bool							Overcharge(bool nOn);	// Overcharging

	friend std::ostream& operator << ( std::ostream& stream, const CHyper& hyp );
	friend std::istream& operator >> ( std::istream& stream, CHyper& hyp);

public:

	bool							m_bEngaged;		// Hyperdrive on 
	bool							m_bOver;		// Overcharged

	int								m_cHeat;		// Heat being produced
	int								m_nHeat;		// Nominal Heat Level (anything above causes damage)
	int								m_mHeat;		// Max Heat

	int								m_nReliability;	// Drive's relability
	int								m_Heat;			// Heat produced each space update

	int								m_nSpeed;		// Hyperspeed they are actually travelling at (To allow fleet co-ordination)

	// m_nValue;				// Determines Hyperspeed
	// m_nModifier;				// Na

	// Sub-Components
	CModule*						m_Flux;			// Flux Converter
													// Each Hyperdrive module adds a percentage
													// to the realiability of the drive. A realible and
													// 100% working module will provide 25%
	CModule*						m_Hyper;		// Hyper conduits
	CModule*						m_Power;		// Power distributor
	CModule*						m_Nubian;		// Nubian power unit
	CModule*						m_Ccu;			// Reversion Cooling Control Unit
													// Helps Regulate Heat
	CModule*						m_Mask;			// Ion Trail Minimiser
													// Reduces the Ion trail left behind

};

///////////////////////////////////////////////////////////////////////////////////////////
// 4a. Powerplant State Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships powerplant state
///////////////////////////////////////////////////////////////////////////////////////////
class CPowerState
{
public: 
	CPowerState();
	~CPowerState();

public:
	float m_nImpulse;
	float m_nManeuver;
	float m_nWeapons;
	float m_nShields;
	float m_nFree;


};

///////////////////////////////////////////////////////////////////////////////////////////
// 4. Powerplant Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships powerplant
///////////////////////////////////////////////////////////////////////////////////////////
class CPowerplant : public CComponent
{
public: 
	CPowerplant();
	~CPowerplant();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?
	
	friend std::ostream& operator << ( std::ostream& stream, const CPowerplant& power );
	friend std::istream& operator >> ( std::istream& stream, CPowerplant& power);


public:

	bool							m_bCritical;	// About to blow up?
	bool							m_bPowered;		// Powered up?

	// m_nTechLevel;			// Determines Energy produced
	// m_nModifier;				// Fuel Consumption

	// Sub-Systems
	bool							m_bLifesupport; // RP Systems, Lifesupport
	bool							m_bEnvironment; // Environmental Controls
	bool							m_bFailsafe;	// Self-destruct
	int								m_nCountdown;	// Count Down Timer

	CPowerState*					m_PowerState;	// Power Config

	// Sub-Components
	CModule*						m_Reactor;		// Reactor Core Unit
													// Determines the amount of energy the powerplant can stockpile
	CModule*						m_Scm;			// Safety Control Mechanism
													// Reduces meltdown damage plus can eject core 
	CModule*						m_Energy;		// Energy Distributor	
													// Determines rate of Energy transfer to ships reserves
	CModule*						m_Containment;	// Containment Field
													// Determines the integrity at which reactor goes critical
	CModule*						m_Fis;			// Fuel Injection System		
													// Determines rate of fuel consumption from fuel reserve

};


///////////////////////////////////////////////////////////////////////////////////////////
// 5. Maneuver Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Maneuver
///////////////////////////////////////////////////////////////////////////////////////////
class CManeuver : public CComponent
{
public: 
	CManeuver();
	~CManeuver();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?
	
	friend std::ostream& operator << ( std::ostream& stream, const CManeuver& maneuver );
	friend std::istream& operator >> ( std::istream& stream, CManeuver& maneuver);

	// Class Specfic Methods
	bool							Engage();					// Powerup Thrusters
	bool							Disengage();				// Power down Thrusters
	bool							Speed(int nSpeed);			// Speed of Thrusters (for forward momentum)
	bool							Overcharge(bool bOn);		// Overcharge the Thrusters
	bool							Repulors(bool bOn);			// Engage/Disengage Repulsor Lift coils

public:

	int								m_nSpeed;		// Current Speed
		
	bool							m_bPowered;		// On/off
	bool							m_bThrusters;	// Using Thrusters?
	bool							m_bOver;		// Overcharging Engines
	bool							m_bRepulsors;	// Repulsors on/off
													// Thrusters can be used to move foward at a very slow rate!
	// Heat
	int								m_Heat;			// Heat produced each space update
	int								m_cHeat;		// Current Heat level
	int								m_nHeat;		// Nominal Heat Level (anything above causes damage)
	int								m_mHeat;		// Critical Heat Level (Engines start to go critical after this)

	// m_nTechLevel;			// Maneuver rating
	// m_nModifier;				// Max Thrust Speed

	// Sub-Modules
	CModule*						m_Thruster;		// Thrusters Energy Core
													// Adds a bonus to Thrust Speed
	CModule*						m_Ccu;			// Control Cooling Unit
													// Helps provide Heat Control
	CModule*						m_Spinner;		// Rotational Spinner
													// Add a bonus to Maneuver rate
	CModule*						m_Cbw;			// Counter Balance Weights	
													// Add a bonus to Maneuver rate
	
};


///////////////////////////////////////////////////////////////////////////////////////////
// 6. SFoils Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships SFoils
///////////////////////////////////////////////////////////////////////////////////////////
class CSFoils : public CComponent
{
public: 
	CSFoils();
	~CSFoils();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?

	
public:
	
	bool							m_bAttack;	// Locked? Flight?
	
	// m_nTechLevel;			// Drag Affect
	// m_nModifier;				// Maneuver Affect

	// Sub-Components
	CModule*						m_Material;		// Material made of
													// Material Affects drag
	CModule*						m_GControl;		// G Control Unit
													// Affects Maneuver
		
};

///////////////////////////////////////////////////////////////////////////////////////////
// 7a. Radar Contact Class
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
	CSpatial*						m_Spatial;			 // Contact's details
	CCart*							m_Location;		 	 // Contact's Location
	int								m_Signature[CSpatial::SI_MAX];	 	 // Signature
	
};
typedef std::map<int,CContact*> ContactMap;


///////////////////////////////////////////////////////////////////////////////////////////
// 7. Radar Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Radar
///////////////////////////////////////////////////////////////////////////////////////////
class CRadar : public CComponent
{
public: 
	CRadar();
	~CRadar();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?

public:

	bool							m_bActive;		// Active or Passive?
	bool							m_bPowered;		// Powered up?
	
	// m_nTechLevel;			// Determines Range
	// m_nModifier;				// Na

	// Sub-Components
	CModule*						m_Booster;		// Booster
	CModule*						m_Stealth;		// Reduces Radar emissions
													// Makes it harder to detect radar
	CModule*						m_Iff;			// IFF Codes
													// Determines what your radar says are good and what
													// It says are bad. Can be sliced by a slicer to cause confusion
		
};


///////////////////////////////////////////////////////////////////////////////////////////
// 8. Sensors Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Sensors
///////////////////////////////////////////////////////////////////////////////////////////
class CSensor : public CComponent
{
public: 
	CSensor();
	~CSensor();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?

	// Class Specfic Methods
	bool							Sweep();					// Sweep Sector
	bool							Scan(int nType);			// Scan contact using sensors

public:

	bool							m_bActive;		// Active or Passive?
	bool							m_bPowered;		// Powered up?

	// m_nTechLevel;			// Determines Range
	// m_nModifier;				// Na

	// Sub-Components
	CModule*						m_Heat;			// Allows scanning for Heat
													// Engine signatures, lifeforms, ordinance
	CModule*						m_Ion;			// Allows scanning for Ion trails
													// Hyperspace tracking, ship detection
	CModule*						m_Em;			// Allows scanning for Electromagnetic trails
													// Communications, Shields
	CModule*						m_Tactical;		// Allows scanning of other ships/signals
		
};



///////////////////////////////////////////////////////////////////////////////////////////
// 8a. Frequency Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to Frequencies
///////////////////////////////////////////////////////////////////////////////////////////
class CFrequency
{
public:
	CFrequency();
	~CFrequency();

public:
	float							m_nFrequency;				// The Frequency
	gString							m_gsEncryption;				// Encryption Code used
};

typedef std::vector<CFrequency*>FreqList;

///////////////////////////////////////////////////////////////////////////////////////////
// 8b. Message Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the functions for Recording and Viewing stored messages
///////////////////////////////////////////////////////////////////////////////////////////
class CMessage
{
public:
	CMessage();
	~CMessage();

	friend std::ostream& operator << ( std::ostream& stream, const CMessage& msg );
	friend std::istream& operator >> ( std::istream& stream, CMessage& msg);

public:
	time_t							m_tTime;					// Transmission Time stamp
	gString							m_gsMessage;				// The Message being broadcast
	gString							m_gsEncryption;				// Encryption Code used
	gString							m_gsFrequency;				// Frequency Transmitted
	gString							m_gsShip;					// The type of vessel making the broadcast
};

typedef std::vector<CMessage*>MsgList;


///////////////////////////////////////////////////////////////////////////////////////////
// 8. Comms Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Comms
///////////////////////////////////////////////////////////////////////////////////////////
class CComms : public CComponent
{
public: 
	CComms();
	~CComms();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?

	// Class Specfic Methods
	bool							Power(bool bOn);							// Power up/down
	bool							Hail(gString gsMsg, gString pShip);			// Sending a Laser Pulse message
	bool							Broadcast(float nFreq, gString gsMsg);		// Broadcasting a message on a Channel
	bool							Shiptalk(gString gsMsg);					// Send Message to ship
	bool							System(gString gsSys);						// Broadcast Message on Open Channels
	bool							Holonet();									// Access the Holonet
	bool							Record(CFrequency* pFreq, gString gsMsg, gString gsShip);	// Record all Transmissions on a Frequency
	bool							IsOpen(CFrequency* pFreq);					// Is the Channel open?
	bool							IsEncrypted(CFrequency* pFreq);				// Is the Channel encrypted?
	bool							CanDecrypt(gString gsProtocol);				// Can the ship decrypt it?
	bool							IsJammed(CFrequency* pFreq);				// Is this ship jamming this Frequency?
	bool							IsSnooped(CFrequency* pFreq);				// Is this ship snooping this Frequency?


public:

	bool							m_bPowered;		// Powered On/off
	int								m_nOpen;		// Active Channel
	
	FreqList						m_Jamming;		// Open Jammed Frequencies preventing activity
	FreqList						m_Snooping;		// Open Snooping Frequencies listening for activity
	FreqList						m_Open;			// Open Frequencies activity
	CFrequency*						m_Recording;	// Open Recording channel
	gStringList						m_Protocols;	// List of Encryption protocols (In form: $$$##)
	MsgList							m_Recordings;   // List of Filenames recorded tranmissions
	
	// m_nTechLevel;			// Determines Range
	// m_nModifier;				// Band


	// Sub Components
	CModule*						m_Memory;		// Memory Store
													// Determines Recording length and Encryption #
	CModule*						m_Signal;		// Signal Booster
													// Boosts the comms range
	CModule*						m_Jammer;		// Jammer
													// Prevents other ships comms working
													// Jammers will have a band they work in
	CModule*						m_Snooper;		// Signal Snooper
													// Listens for other ship's comms in a band
		
};


///////////////////////////////////////////////////////////////////////////////////////////
// 9a. Navi Jump Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships jump into hyper
///////////////////////////////////////////////////////////////////////////////////////////
class CNavJump
{
public:
	CNavJump();
	~CNavJump();

public:
	CartList						m_Waypoint;					// Location of each Jump
	int								m_nDistance;				// Distance taken for jump
	int								m_nRisk;					// Risk of jump
	gString							m_gsCalculated;				// Calculated by
	gString							m_gsName;					// Name of the Jump so it can be saved

};

typedef std::vector<CNavJump*>JumpList;

///////////////////////////////////////////////////////////////////////////////////////////
// 9. Navi Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Navigational Computer
///////////////////////////////////////////////////////////////////////////////////////////
class CNavi : public CComponent
{
public: 
	CNavi();
	~CNavi();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component
	virtual int						Integrity();				// Calculate Component's integrity
	virtual void					Update();					// Update has happened, what do we do?

	// Class Specfic Methods
	bool							Vector(CCart* pJumpV);		// Takes a Jump Vector and Cross references it with
																// All its currently loaded charts
	bool							Power(bool bOn);			// Power on/off
	bool							Calculate(gString gsChart); // Calculate a Jump by using a predefined chart
	bool							Plot(CCart* waypoint);		// Add a Waypoint to Route
	bool							Remove(int nNum);			// Remove a Waypoint
	bool							View();						// View Current Route being plotted
	bool							SaveR();					// Save the Route to Nav Computer Memory as a Chart
	bool							Load(CModule* pChart);		// Load a Chart
	bool							Unload(gString gsChart);	// Unload a Chart
	bool							Copy(gString gsChart);		// Copy the Chart
	bool							Steal(gString gsChart);		// Steal using Slicing a Chart
	bool							Status();					// Display memory status of Nav Computer
	bool							History();					// View Jump History

public:

	ModuleList						m_NavMaps;		// List of all loaded charts
	JumpList						m_Jumps;		// List of previous calculated jumps and by whom
	bool							m_bPowered;		// On/Off

	// m_nTechLevel;			// Determines Accuracy
	// m_nModifier;				// Memory

	// Sub-Components
	CModule*						m_Memory;		// Maximum Memory for Charts
													// Allows more charts to be stored
	CModule*						m_Vector;		// Helps with Jumpvectors
													// Helps cross reference a jump vector
	CModule*						m_Security;		// Security Module
													// Prevents slicing
};


///////////////////////////////////////////////////////////////////////////////////////////
// 10a. ShieldState Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Shield banks
///////////////////////////////////////////////////////////////////////////////////////////
class CShieldState
{
public:
	CShieldState();
	~CShieldState();

public:
	int								m_nPort;
	int								m_nStarboard;
	int								m_nDorsal;
	int								m_nVental;
	int								m_nFore;
	int								m_nAft;	
};

///////////////////////////////////////////////////////////////////////////////////////////
// 10. Shield Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Shields
///////////////////////////////////////////////////////////////////////////////////////////
class CShield : public CComponent
{
public: 
	CShield();
	~CShield();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specfic Methods
	bool							Power(bool bOn);			// Power Shields up or down
	bool							Balance();					// Reset all Banks to being equal
	bool							Recharge(bool bOn);			// Recharging on/off
	bool							Shift(int nFrom, int nTo, int nPower);

public:

	bool							m_bShields;		// Shields up?
	CShieldState*					m_Shieldstate;	// Banks and Integrity

	// m_nTechLevel;			// Determines Max Powerrating
	// m_nModifier;				// Determines Strength

	// Sub-Components
	CModule*						m_Power;		// Power regulator
													// Helps take damage
	CModule*						m_Dispersion;	// Bank Energy Dispersion Unit
													// Helps equalise fields
	CModule*						m_Field;		// Field Generation Unit
													// Affects regeneration speed
	CModule*						m_Maintenance;	// Core Maintenance Drive
													// Helps take damage
			
};

/*
///////////////////////////////////////////////////////////////////////////////////////////
// 11. Tractor Beam Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Shields
///////////////////////////////////////////////////////////////////////////////////////////
class CTractor : public CComponent
{
public: 
	CTractor();
	~CTractor();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specfic Methods
	bool							Target(gString pTarget);		// Track Target
	bool							Engage(bool bOn);			// Tractor target/release target
	bool							Adjust(gString Args);		// Manipulate Tractor Beam

public:

	bool							m_bEngaged;		// Tractor on?
	int								m_nAction;		// Pull, Push, Land, etc
	gString							m_Target;		// Tractors Target

	// m_nTechLevel;			// Determines Range
	// m_nModifier;				// Determines Strength

	// Sub-Components
	CModule*						m_Control;		// Advanced Control
													// Allows more tractor functions
	CModule*						m_Tracking;		// Tracking Unit
};

typedef std::vector<CTractor*> TractorList;

///////////////////////////////////////////////////////////////////////////////////////////
// 12. Gravity Wells Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Gravity Wells
///////////////////////////////////////////////////////////////////////////////////////////
class CGravity : public CComponent
{
public: 
	CGravity();
	~CGravity();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specfic Methods
	bool							Engage(bool bOn);			// Power up, power down

public:

	bool							m_bEngaged;		// Gravity wells up
	int								m_nState;		// Takes a while to get them on line

	// m_nTechLevel;			// Determines Range
	// m_nModifier;				// Determines powerup time


	// Sub-Components
	CModule*						m_Sustain;		// Sustainer Fields
													// Reduces the drain on energy of the field
	CModule*						m_Power;		// Power Generator Booster
													// Increases their powerup speed
		
};

///////////////////////////////////////////////////////////////////////////////////////////
// 13. Targetting Suite
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Target
///////////////////////////////////////////////////////////////////////////////////////////
class CTarget : public CComponent
{
public: 
	CTarget();
	~CTarget();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Track(gString pShip);		// Track a Target
	bool							Lock(gString pShip);			// Lock onto Target
	bool							Remove(gString gsName);		// Stop Tracking/Targetting Ship
	bool							Tracking(gString gsName);	// Check if Tracking a Ship
	bool							Locked(gString gsName);		// Check if Locked to a Ship


public:

	
	gStringList						m_Targets;
	gStringList						m_Locks;

	// m_nTechLevel;			// Determines Range
	// m_nModifier;				// Determines Number of Targets
	
	// Sub-Components
	CModule*						m_Uplink;		// Allows for Target identification
	CModule*						m_Network;		// Networking Suite
													// Allows target transmission to other ships
		
};

///////////////////////////////////////////////////////////////////////////////////////////
// 14. Ordinance Bay Suite
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a Fighters' Ordinance Bay
///////////////////////////////////////////////////////////////////////////////////////////
class CBay : public CComponent
{
public: 
	CBay();
	~CBay();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Deploy();					// Launch!
	bool							Load();						// Load Ordinance into Bay
	bool							Unload();					// Remove Ordinance from Bay
	bool							Payload();					// View Bay contents

public:

	bool							m_bReload;					// Reloading?
	int								m_nTimer;					// Reload Timer

	// m_nTechLevel;			// Determines Size of bay
	// m_nModifier;				// Determines Reload Time

	// Sub-Components
	CModule*						m_Launcher;					// Type of Ordinance in bay
};

///////////////////////////////////////////////////////////////////////////////////////////
// 15. Ordinance Launcher Suite
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Launchers
///////////////////////////////////////////////////////////////////////////////////////////
class CLauncher : public CComponent
{
public: 
	CLauncher();
	~CLauncher();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Deploy();					// Fire the Launcher
	bool							Lock(gString pShip);			// Lock onto a Target (if Launcher has its own Sensor)
	bool							Reload();					// Reload the Launcher
	bool							Restock();					// Restock Ammo

public:

	bool							m_bLoaded;		// Loaded?
	int								m_nTimer;		// Reload Timer

	// m_nTechLevel;			// Determines Size of bay
	// m_nModifier;				// Determines Reload Time

	// Sub Components
	CModule*						m_Ammo;			// Type of Ordinance in bay
	CModule*						m_Sensor;		// Targetting Sensor

};

typedef std::vector<CLauncher*>LauncherList;

///////////////////////////////////////////////////////////////////////////////////////////
// 16. Chaff Launcher Suite
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Chaff launchers
///////////////////////////////////////////////////////////////////////////////////////////
class CChaff : public CComponent
{
public: 
	CChaff();
	~CChaff();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Deploy();					// Deploy chaff burst

public:

	bool							m_bLoaded;		// Loaded?
	int								m_nTimer;		// Timer

	// m_nTechLevel;			// Determines Life Duration of Chaff Burst
	// m_nModifier;				// Determines Reload Time
	
	// Sub-Components
	CModule*						m_Ammo;			// Chaff remaining
	
};

///////////////////////////////////////////////////////////////////////////////////////////
// 16. Flare Launcher Suite
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Flare launchers
///////////////////////////////////////////////////////////////////////////////////////////
class CFlare : public CComponent
{
public: 
	CFlare();
	~CFlare();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

public:

	bool							m_bLoaded;		// Loaded?
	int								m_nTimer;		// Timer

	// m_nTechLevel;			// Determines Life of Flare
	// m_nModifier;				// Determines Reload Time

	// Sub-Component
	CModule*						m_Ammo;			// Flares remaining
	
};

///////////////////////////////////////////////////////////////////////////////////////////
// 17. Battle Control Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Battle Control system
///////////////////////////////////////////////////////////////////////////////////////////
class CBattle : public CComponent
{
public: 
	CBattle();
	~CBattle();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Form(gString gsName);		// Form a Battlegroup
	bool							Add(gString pShip);			// Add a Ship
	bool							Remove(gString pShip);		// Remove a Ship
	bool							Set();						// Setup Battlegroup
	bool							Order();					// Order Battlegroup
	
public:

	// m_nTechLevel;			// Determines Max Battlegroup size
	// m_nModifier;				// na

	CModule*						m_Advanced;					// Advanced comms
																// Allows the creation of a Battlegroup channel
	CModule*						m_Graphics;					// Graphical Representation				
																// Allows an overview of the battlespace
	CModule*						m_Iff;						// Advanced IFF
																// Allows a clearer battle picture
	
};

///////////////////////////////////////////////////////////////////////////////////////////
// 18. Fighter Control Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Battle Control system
///////////////////////////////////////////////////////////////////////////////////////////
class CFighter : public CComponent
{
public: 
	CFighter();
	~CFighter();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Order();					// Order a Squadron to do something
	bool							Form();						// Create A Squadron
	bool							Set();						// Set a Squadron entry
	bool							Add();						// Add a Ship
	bool							Remove();					// Remove one
	
public:

	// m_nTechLevel;			// Determines Max Squadron size
	// m_nModifier;				// na

//	SquadronList					m_Squadrons;				// List of all Squadrons
	CModule*						m_Advanced;					// Advanced comms
																// Allows the creation of a Fighter channel
	CModule*						m_Squadron;					// Squadron Networking	
																// Allows squadrons to be formed
	
};

///////////////////////////////////////////////////////////////////////////////////////////
// 18. Turret Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Laser turrets
///////////////////////////////////////////////////////////////////////////////////////////
class CTurret : public CComponent
{
public: 
	CTurret();
	~CTurret();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Track(gString pShip);		// Track a Ship
	bool							Fire();						// Commence firing
	bool							Cease();					// Cease Firing
	bool							Disengage();				// Stop tracking

public:

	int								m_nClass;					// Type of Weapon
	bool							m_bCharge;					// Charging?
	int								m_nRecharge;				// Recharge rate
	float							m_Rotation;					// Max Rotation in Degrees
	float							m_Elevation;				// Max Elevation in Degrees
	gString							m_pTarget;					// Target

	// m_nTechLevel;			// Determines damage
	// m_nModifier;				// Determines range
	
};

typedef std::vector<CTurret*>TurretList;

///////////////////////////////////////////////////////////////////////////////////////////
// 19. Landing Area Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the specific actions and modules that pertain to a ships Laser turrets
///////////////////////////////////////////////////////////////////////////////////////////
class CLanding : public CComponent
{
public: 
	CLanding();
	~CLanding();

	virtual bool					Load(gString file);			// Load the Component into Memory
	virtual bool					Save();						// Save the Component
	virtual bool					Install(gString pShip);		// Install the Component
	virtual	bool					Uninstall();				// Remove the Component
	virtual bool					Tamper();					// Attempt to tamper with the Device
	virtual bool					Damage(int nAmount);		// Damage the Component

	// Class Specific Methods
	bool							Clearance(gString pShip);	// Clear a Ship to land
	bool							Blastdoors(bool bOn);		// Prevent all access to Landing Bay
	bool							Eject(gString pShip);		// Use a tractor beam to remove a ship from the bay
	
public:

	bool							m_bDoors;					// Doors shut?
	gStringList						m_Cleared;					// List of all ships cleared to land
	gStringList						m_Landed;					// All ships in bay

	// m_nTechLevel;			// Determines size of bay
	// m_nModifier;				// na
	
};

typedef std::vector<CLanding*>LandingList;

  */

#endif
