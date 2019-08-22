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

// Header   :: Crew.h
// Function :: Contains all the functions for storing and retrieving a Ship's Crew
// Written  :: 14/02/2006

#include "Actor.h"
#include "Atomic.h"
#include "Tools.h"
#include <vector>
#include <map>

#ifndef __CREW_H__
#define __CREW_H__

class CShip;

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Crew Class
///////////////////////////////////////////////////////////////////////////////////////////
class CCrew : public CAtomic
{
	public:

	CCrew();
	~CCrew();

	typedef enum e_CrewType
	{
		CT_WEAPONRY		= 0,
		CT_ENGINEERING	= 1,
		CT_BRIDGECREW	= 2,
		CT_PILOTS		= 3,
		CT_TROOPS		= 4,
		CT_MAX
	};

	typedef enum e_CrewState
	{
		_OFFDUTY		= 0,	// Crew is off duty
		_ONQRA			= 1,	// Crew is on Quick Reaction Alert
		_REPORTING		= 2,	// Crew is reporting to General quarters
		_ONDUTY			= 3,	// Crew is on duty
		_AWAITING		= 4,	// Crew is awaiting orders
		_BUSY			= 5,	// Crew is carrying out orders
		_NUMCREWSTATE
	};


	static char *szTypes[];
	static char *szStates[];




	bool						Save();				// Save the Crew to a File
	bool						Load();				// Load the Crew into a object
	bool						Load(gString file);	// Load the Crew object from a File
	void						Update();			// Handles the Crew update cycle

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					HandleEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Handlers
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					OnReport(CEvent& Event);
	virtual bool					OnSpeed(CEvent& Event);
	virtual bool					OnComplete(CEvent& Event);
	virtual bool					OnCourse(CEvent& Event);
	virtual bool					OnRoll(CEvent& Event);
	virtual bool					OnPower(CEvent& Event);
	virtual bool					OnTransmit(CEvent& Event);
	virtual bool					OnBroadcast(CEvent& Event);
	virtual bool					OnTightbeam(CEvent& Event);
	virtual bool					OnComm(CEvent& Event);
	virtual bool					OnSweep(CEvent& Event);
	virtual bool					OnStatus(CEvent& Event);
	virtual bool					OnLaunch(CEvent& Event);
	virtual bool					OnLand(CEvent& Event);
	virtual bool					OnTarget(CEvent& Event);


	
	// Stream functions
	friend std::ostream&	operator << ( std::ostream& stream, const CCrew& crew );
	friend std::istream&	operator >> ( std::istream& stream, CCrew& crew );

	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);


	public:

	gString						m_gsName;			// Team name
	gString						m_gsFileName;		// Filename of Team
	gString						m_gsLeader;			// PC Leader

	CActor*						m_Leader;			// Team Leader (NPC)
	CShip*						m_Ship;				// Ship assigned to
	CPlacement*					m_Location;			// Where Team is deployed
	CPlacement*					m_Home;				// Home base, where they load in

//  CFaction*					m_Faction;			// Faction part of
	float						m_fSkill;			// Skill in this Type [ePac style]
	float						m_fLastUpdate;
	int							m_nType;			// Crew Type	
	int							m_nUniqueID;		// Unique ID of this Crew Team

	gStringList					m_Orders;			// Queue of Strings containing Orders
	gString						m_gsOrder;			// Current order processing

	int							m_ncCompliment;		// Current men on Team
	int							m_nmCompliment;		// Maximum men on Team
	int							m_nCrewState;		// State Crew is in

};
typedef std::vector<CCrew*> CrewList;
typedef std::map<int, CrewList> CrewMap;

// _REPORT
class EReport : public CEvent
{
public:
	EReport();

public:
	CPlacement*	m_Location;
};

// _SPEED
class ESpeed : public CEvent
{
public:
	ESpeed();

public:
	int			m_nSpeed;
};

// _COMPLETE
class EComplete : public CEvent
{
public:
	EComplete();

public:
	int			m_nType;

};

// _COURSE
class ECourse : public CEvent
{
public:
	ECourse();

public:
	int			m_nBearing;
	int			m_nMark;
};

// _ROLL
class ERoll : public CEvent
{
public:
	ERoll();

public:
	gString		m_gsRoll;
};


// _POWER
class EPower : public CEvent
{
public:
	EPower();

public:
	gString		m_gsSystem;
};

// _TRANSMIT
class ETransmit : public CEvent
{
public:
	ETransmit();

public:
	gString		m_gsMessage;
};

// _BROADCAST
class EBroadcast : public CEvent
{
public:
	EBroadcast();

public:
	gString		m_gsMessage;
};

// _TIGHTBEAM
class ETightbeam : public CEvent
{
public:
	ETightbeam();

public:
	gString		m_gsMessage;
	gString		m_gsShip;
};

// _COMM
class EComm : public CEvent
{
public:
	EComm();

public:
	gString		m_gsFunction;
	gString		m_gsValue;
	gString		m_gsValue2;

};

// _SWEEP
class ESweep : public CEvent
{
public:
	ESweep();

public:
	gString		m_gsRadome;
};

// _STATUS
class EStatus : public CEvent
{
public:
	EStatus();

};

// _LAND
class ELand : public CEvent
{
public:
	ELand();

public:
	gString		m_gsLocation;

};

// _LAUNCH
class ELaunch : public CEvent
{
public:
	ELaunch();

};

// _TARGET
// _LAUNCH
class ETarget : public CEvent
{
public:
	ETarget();

public:
	gString		m_gsTarget;

};


///////////////////////////////////////////////////////////////////////////////////////////
// 2.Crew Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is held by the GameWorld object and is simply used to load in all the
// unassigned Crew objects
///////////////////////////////////////////////////////////////////////////////////////////
class CCLoader
{
public:
	CCLoader();
	~CCLoader();

	bool							Load(gFileName gfFile);	// Load method
	bool							Save();					// Save method

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	CrewList						m_Crews;

};

///////////////////////////////////////////////////////////////////////////////////////////
// 3. Order Class
///////////////////////////////////////////////////////////////////////////////////////////
// Any order issued to the crew is stored within this class
///////////////////////////////////////////////////////////////////////////////////////////
class COrder : public CAtomic
{
public:
	COrder();
	COrder(int nType, int nTimer);
	~COrder();


	typedef enum e_OrderType
	{
		// General Orders
		_REPORT,					// Report to assigned area
		_STATUS,					// Report Status
		_MOVETO,					// Move to a Location
		_STANDDOWN,					// Return to Holding point/Off Duty
		_DRILL,						// Start Drilling a skill
		_PARADE,					// Report for Parade
		_COMPLETE,					// Orders carried out
		// Gunnery Orders
		_TARGET,					// Target an enemy vessel
		_DISABLE,					// Disable target
		_DESTROY,					// Destroy target
		_TRACTOR,					// Tractor and hold target
		_CAPTURE,					// Bring tractored ship into landing bay
		// Bridge Crew
		_COURSE,					// Set a New Course
		_ROLL,						// Roll the Ship
		_SPEED,						// Set a New Speed
		_POWER,						// Power a System
		_TRANSMIT,					// Transmit a message
		_BROADCAST,					// Broadcast a message
		_TIGHTBEAM,					// Tightbeam a Message to another Ship
		_COMM,						// Manipulate Comms system
		_NAVIGATE,					// Set Course for Hyperspace Node
		_ASTROGATE,					// Make Custom jump
		_HYPER,						// Engage hyperspace 
		_SWEEP,						// Commence a Sweep of the System
		_DOCK,						// Dock with target
		_LAUNCH,					// Launch the Ship
		_LAND,						// Land the Ship
		// Pilots/Squadrons
		//_LAUNCH,					// Deploy
		_RECALL,					// Return home
		_PATROL,					// Start race track pattern
		//_DISABLE,					// Disable target
		//_DESTROY,					// Destroy target
		_ESCORT,					// Formate on and defend target
		_FLEE,						// Run away... fast!
		// Troops
		_SECURE,					// Secure a location
		_BOARD,						// Board a ship
		_INVADE,					// Invade a planet
		// Engineers
		_REPAIR,					// Repair Keel/Armour/Module
		_JURYRIG,					// Check repair Module
		_EXTINGUISH,				// Put out fires
		_NUMORDERS		
	};



public:
	int								m_nOrder;		// Type of the Order
	int								m_nTimer;		// Order timer
	gStringList						m_Arguments;	// Arguments for Order

};

#endif