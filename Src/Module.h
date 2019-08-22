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

// Header   :: Module.h
// Function :: Handles the classes for Modules

#ifndef __MODULE_H__
#define __MODULE_H__

#include "MudCore.h"
#include "Placement.h"
#include "../gTools/TinyXml.h"

class CActor;

typedef std::map<gString, int>IntegerMap;
typedef std::map<int, int>IntIntMap;
typedef std::vector<int>IntegerList;

///////////////////////////////////////////////////////////////////////////////////////////
// 1a. Frequency Class
///////////////////////////////////////////////////////////////////////////////////////////
// Used by the Comms Module
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
// 1b. Message Class
///////////////////////////////////////////////////////////////////////////////////////////
// Contains the functions for Recording and Viewing stored messages
///////////////////////////////////////////////////////////////////////////////////////////
class CMessage
{
public:
	CMessage();
	~CMessage();

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	time_t							m_tTime;					// Transmission Time stamp
	gString							m_gsMessage;				// The Message being broadcast
	gString							m_gsEncryption;				// Encryption Code used
	gString							m_gsFrequency;				// Frequency Transmitted
	gString							m_gsShip;					// The type of vessel making the broadcast
};
typedef std::vector<CMessage*>MsgList;

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Comms Module Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is held by the GameWorld object and is simply used to load in all the
// Modules
///////////////////////////////////////////////////////////////////////////////////////////
class CMComms
{
public:
	CMComms();
	~CMComms();

	bool							IsEncrypted(CFrequency* pFreq);				// Is the Channel encrypted?
	bool							Record(CFrequency* pFreq, gString gsMsg, gString gsShip);	// Record all Transmissions on a Frequency

public:
	int								m_nOpen;		// Active Channel
	
	FreqList						m_Jamming;		// Open Jammed Frequencies preventing activity
	FreqList						m_Snooping;		// Open Snooping Frequencies listening for activity
	FreqList						m_Open;			// Open Frequencies activity
	CFrequency*						m_Recording;	// Open Recording channel
	gStringList						m_Protocols;	// List of Encryption protocols (In form: $$$##)
	MsgList							m_Recordings;   // List of Filenames recorded tranmissions

};

///////////////////////////////////////////////////////////////////////////////////////////
// 2. Module Class
///////////////////////////////////////////////////////////////////////////////////////////
// Standard ship module containing two values which can be set
///////////////////////////////////////////////////////////////////////////////////////////
class CModule
{
public:
	CModule();
	~CModule();

	//  Module Types
	typedef enum e_ModuleTypes
	{
		MT_NONE							= 0,
		MT_ARMOUR_PLATING				= 1,
		MT_SHIELD_GENERATOR				= 2,
		MT_POWER_LINK					= 3,
		MT_REACTOR_PLANT				= 4,
		MT_COOLANT_PLANT				= 5,
		MT_FUEL_LINE					= 6,
		MT_BATTERY_BANK					= 7,
		MT_ION_ENGINE					= 8,
		MT_DRIVE_BAFFLES				= 9,
		MT_HYPERDRIVE_MAIN				= 10,
		MT_HYPERDRIVE_BACKUP			= 11,
		MT_COOLANT_LINE					= 12,
		MT_MISSILES						= 13,
		MT_TORPEDOES					= 14,
		MT_ROCKETS						= 15,
		MT_CHAFF						= 16,
		MT_FLARES						= 17,
		MT_COLLECTOR_ARRAY				= 18,
		MT_RADOME						= 19,
		MT_MANUEVERING_THRUSTERS		= 20,
		MT_HEATSINK						= 21,
		MT_GRAVITY_WELL_PROJECTORS		= 22,
		MT_FLARE_AND_CHAFF_DISPENSOR	= 23,
		MT_COMMUNICATIONS				= 24,
		MT_JAMMING_POD					= 25,
		MT_SNOOPING_POD					= 26,
		MT_ASTROGATION_COMPUTER			= 27,
		MT_REPULSOR_COILS				= 28,
		MT_LIFE_SUPPORT_UNIT			= 29,
		MT_HOLONET_TRANSCEIVER			= 30,
		MT_BLAST_DOORS					= 31,
		MT_TRACTOR_BEAM_PROJECTOR		= 32,
		MT_FUEL_CELLS					= 33,
		MT_FABRICATION_MATERIAL			= 34,
		MT_CONTROL_CONSOLE				= 35,
		MT_PILOT_CONSOLE				= 36,
		MT_GUNNERY_CONSOLE				= 37,
		MT_SECONDARY_CONTROL_CONSOLE	= 38,
		MT_COPILOT_CONSOLE				= 39,
		MT_NAV_CONSOLE					= 40,
		MT_COMMANDER_CONSOLE			= 41,
		MT_ENGINEERING_CONSOLE			= 42,
		MT_WEAPONS_CONSOLE				= 43,
		MT_HELM							= 44,
		MT_TURRET_MOUNT					= 45,
		MT_MUNITION_ELEVATOR			= 46,
		MT_ESCAPE_POD					= 47,
		MT_SFOIL_AND_ACTUATORS			= 48,
		MT_LAST
	};

	typedef enum e_PlusFields
	{
		PF_SWITCH			= 0,
		PF_ENERGY			= 1,
		PF_COOLANT			= 2,
		PF_TIMER			= 3,
		PF_LAST
	};


	static char *szModules[];

	virtual bool							Save();
	virtual bool							Load();
	virtual bool							Load(gString filename);
	
	int										Plus(gString gsType, bool bActual);		// Returns the value of a Plus field
	int										GetIndex(gString type);					// Gets the Index of a Module type
	int										Damage(int nAmount);					// Damage the Module
	bool									SetPlus(gString gsType, int nValue);	// Sets a Plus field
	bool									CanReceive(int nType);					// Can this Terminal receive this message type
	bool									Powered();								// Is a Module powered?
	bool									Valid(gString type);					// Is the type a valid Module
	
	
	CModule&	operator =		(CModule& clone);

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	gString							m_gsName;			// Name of Module
	gString							m_gsFileName;		// Filename of Module
	int								m_nType;			// The type of module
	int								m_nSize;			// Size of module
	int								m_nMass;			// Mass of module
	IntegerMap						m_Plus;				// Integer Map for extra Fields:
														// Power, Coolant, etc
	gStringList						m_Messages;			// Used to store Messages
	CPlacement						m_Installed;		// Location its installed in
	UINT							m_uiUniqueID;		// The globally unique ID of this module

	int								m_nResilience;		// Armour rating of absorbtion	
	int								m_ncDurability;		// current damage
	int								m_nmDurability;		// max damage

	// Special Modules
	CMComms*						m_Comms;			// Extra fields for Comms
	// Manning Terminals
	CActor*							m_Manned;			// Used to store Player manning console
};

typedef std::vector<CModule*>ModuleList;
typedef std::map<int, CModule*>ModuleMap;

///////////////////////////////////////////////////////////////////////////////////////////
// 2. Module Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
// This class is held by the GameWorld object and is simply used to load in all the
// Modules
///////////////////////////////////////////////////////////////////////////////////////////
class CMLoader
{
public:
	CMLoader();
	~CMLoader();

	bool							Load(gFileName gfFile);	// Load method
	bool							Save();					// Save method

	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);

public:
	ModuleList						m_Modules;

};

#endif