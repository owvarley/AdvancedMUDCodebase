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

// Header   :: Race.h
// Function :: Handles class definitions for Race support

#ifndef __RACE_H__
#define __RACE_H__

#include <map>
#include "../gTools/gString.h"
#include "MudCore.h"
#include "Attributes.h"

class TiXmlNode;

// Used to store a bodypart for a race
// Holds the number of hitpoints that race has as a base for each bodypart.
// Percentage chance of hitting this location.
class CBodyPart
{
public:

	CBodyPart();
	virtual ~CBodyPart();

	//////////////////////////////////////////////////////////////////
	// Inline functions
	//////////////////////////////////////////////////////////////////
	inline const int			Hitpoints()		const { return m_nHitpoints; }
	inline const float			Percentage()	const { return m_fPercentage; }
	inline const int			Type()			const { return m_nType; }

	//////////////////////////////////////////////////////////////////
	// Function methods
	//////////////////////////////////////////////////////////////////
	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);


private:
	int							m_nHitpoints;
	int							m_nType;
	float						m_fPercentage;

};

typedef std::map<int, CBodyPart*>BodyPartMap;

// Defines a race
class CRace
{
public:

	CRace();
	virtual ~CRace();

	typedef enum e_BodyParts
	{
		_HEAD				= 0,
		_CHEST				= 1,
		_R_ARM				= 2,
		_L_ARM				= 3,
		_ABDOMEN			= 4,
		_R_LEG				= 5,
		_L_LEG				= 6,
		_ABOUT				= 7,
		_TAIL				= 8,
		_LEKKU				= 9,
		_ANTENNA			= 10,
		_TORSO				= 11, // Droids equivalent of Chest + Abdomen
		_R_TRED				= 12, // Droids equivalent of feet
		_L_TRED				= 13,
		_NUM_BODYPARTS
	};

	static char *szBodyParts[];
	static char *szBodyXML[];

	//////////////////////////////////////////////////////////////////
	// Inline functions
	//////////////////////////////////////////////////////////////////
	inline const gString		Name()		const { return m_gsName; }
	inline BodyPartMap&			BodyParts()	      { return m_BodyParts; }
	inline gStringList&			Languages()		  { return m_Languages; }
	inline CAttributeMgr&		Attributes()	  { return m_Attributes; }
	inline gStringList&			Perks()		  	  { return m_Perks; }
	inline void					SetName(const gString& gsName) { m_gsName = gsName; }
	
	//////////////////////////////////////////////////////////////////
	// Function methods
	//////////////////////////////////////////////////////////////////
	virtual bool				Load(gFileName gsRace);
	virtual bool				Save(gFileName gsRace);
	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);
	void						AddPart(int nType, float fChance, int nHitPoints);

public:
	gString						m_gsName;		// Name of the Race
	BodyPartMap					m_BodyParts;	// Bodypart structure
	CAttributeMgr				m_Attributes;	// Attribute structure
	gStringList					m_Perks;		// Perks for race
	gStringList					m_Languages;	// Race language

};

typedef std::vector<CRace*>RaceList;

//////////////////////////////////////////////////////////////////////
// RaceMgr :: Used to load and save races from file. 
//////////////////////////////////////////////////////////////////////
class CRaceMgr
{
public:
	CRaceMgr();
	virtual ~CRaceMgr();

	//////////////////////////////////////////////////////////////////
	// Inline functions
	//////////////////////////////////////////////////////////////////
	inline RaceList&			Races() { return m_Races; }

	//////////////////////////////////////////////////////////////////
	// Function methods
	//////////////////////////////////////////////////////////////////
	virtual bool				Load(gFileName gsRace);
	virtual bool				Save(gFileName gsRace);

public:
	RaceList					m_Races;		// List of loaded races

};

#endif