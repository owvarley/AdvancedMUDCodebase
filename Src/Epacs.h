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

// Header   :: Epacs.h
// Function :: Handles class definitions for the Extensive Player Character System (ePacs)

#include <map>
#include "gString.h"
#include "MudCore.h"
#include "Mobile.h"
//#include "Module.h"

#if !defined __EPACS_H__
#define __EPACS_H__

class CSkill;
class CMobile;

typedef std::vector<CSkill*> SkillList;

/*
class CSkillNumber
{
public:
	CSkillNumber();
	CSkillNumber(int nId);
	~CSkillNumber();

public:
	friend	CSkill*	operator *			    ( CSkillNumber& pId );

	int								m_nId;

};
typedef std::vector<CSkillNumber*> SkillNumList; */

//CSkill*	  operator *  ( CSkillNumber& pId );

class CSkill
{
public:
	CSkill();
	CSkill(gString gsName, int nCategory, int nSubCategory, int nCost);
	virtual	~CSkill();

	////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_SkillTypes
	{
		_ABILITY		=	0,	// Abilities are the highest level of skills
		_SKILL			=	1,	// A skill is a subset of an ability
		_TALENT			=   2,  // A talent provides the player with additional functionality
		_SUBSKILL		=   3,  // Skill belonging to a Composite one
		_MAXTYPES
	};

	typedef enum e_Categories
	{
		_PHYSICAL		=	0,
		_MENTAL			=	1,
		_SOCIAL			=	2,
		_MAXCATEGORIES 
	};

	typedef enum e_SkillFlags
	{
		_HIDDEN			= 0,
		_MAXSKILLFLAGS
	};

	typedef enum e_Abilities
	{
		// Physical
		_COMBAT			=	0,
		_EXPLORATION	=	1,
		_WAR			=	2,
		// Mental
		_TECHNOLOGY		=	3,
		_SPACEABILITY	=	4,
		_SUBTERFUGE		=	5,
		// Social
		_AUTHORITY		=	6,
		_PERSUASION		=	7,
		_GALAXY			=	8,

		_MAXABILITIES
	};

	static char *szTypes[];			// String names of types
	static char *szCategories[];	// String names of Categories
	static char *szAbilities[];		// String names of Abilities	
	static int   nAbilities[];		// Mapping of Abilities to Categories

	////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	////////////////////////////////////////////////////////////////////////////////////	
	inline const gString		Name()			const { return m_gsName; }
	inline const gString		FileName()		const { return m_gsFileName; }
	inline const int			Id()			const { return m_nId;	}	
	inline const int			Category()		const { return m_nCategory; }
	inline const int			Ability() 		const { return m_nAbility; }
	inline const int			Cost()			const { return m_nCost; }
	inline const bool			Disabled()		const { return m_bDisabled; }
	inline const bool			Logged()		const { return m_bLogged; }
	inline const int			Type()			const { return m_nType; }
	inline const gString		Prereq()		const { return m_gsPrereq; }
	inline IntegerList&			Children()			  { return m_Children; }
	inline CSet*				Flags()				  { return m_Flags; }

	////////////////////////////////////////////////////////////////////////////////////
	// Setters
	////////////////////////////////////////////////////////////////////////////////////	
	void						SetName(const gString& s) 	{ m_gsName = s; }
	void						SetFileName(const gString& s) { m_gsFileName; }
	void						SetCategory(const int& n) 	{ m_nCategory = n; }
	void						SetAbility(const int& n)	{ m_nAbility = n; }
	void						SetCost(const int& n)		{ m_nCost = n; }
	void						SetDisabled(const bool& b)  { m_bDisabled = b; }
	void						SetLogged(const bool& b)	{ m_bLogged = b; }
	void						SetId(const int& n )		{ m_nId = n; }
	void						SetType(const int& n)		{ m_nType = n; }
	void						SetPrereq(const gString& s) { m_gsPrereq = s; }
	void						AddChild(CSkill* ps)		{ m_Children.push_back(ps->Id()); }

	////////////////////////////////////////////////////////////////////////////////////
	// File writing methods to convert class to XML data format
	////////////////////////////////////////////////////////////////////////////////////	
	bool						Load();
	bool						Load(gString szFile);
	bool						Save();
	bool						ReadXml(TiXmlNode * pNode);
	bool						WriteXml(TiXmlNode * pNode);

// Data
protected:
	gString						m_gsName;		// Skill name
	gString						m_gsFileName;	// Filename
	gString						m_gsPrereq;	// Preqrequisite

	int							m_nCategory;	// Category
	int							m_nAbility;		// Not used
	int							m_nCost;		// Cost of skill (not used)
	int							m_nType;		// Skill, Ability or Talent
	int							m_nId;			// Skill id

	bool						m_bLogged;		// Log the skill
	bool						m_bDisabled;	// Disable the skill

	CSet*						m_Flags;		// Skill flags
	IntegerList					m_Children;		// Skill list

};

typedef std::map<int, CSkill*> SkillMgrMap;
typedef std::map<gString, int> SkillIndex;
typedef std::map<int, int> SkillMap;


class CEpacs
{
public:
	CEpacs();
	~CEpacs();

	// The Task Threshold defines the 
	// different difficulties that can
	// be applied to tasks
	typedef enum e_TaskThreshold
	{
		_DEFAULT		= 0,
		_CHALLENGING	= 1,
		_TOUGH			= 2,
		_HARD			= 3,
		_HEROIC			= 4,
		_LEGENDARY		= 5,
		_IMPOSSIBLE		= 6
	};


	typedef enum e_EpacSettings
	{
		_MAXSKILLLEVEL	= 30
	};

	// Listing of indexes for skill levels
	// needed to determine skill raising
	typedef enum e_SkillLevels
	{
		_1  = 0,	_2  = 1,	_3  = 3,
		_4  = 4,	_5  = 5,	_6  = 6,
		_7  = 7,	_8  = 8,	_9  = 9,
		_10 = 11,	_11 = 11,	_12 = 12,
		_13 = 13,	_14 = 14,	_15 = 15,
		_16 = 16,	_17 = 17,	_18 = 18,
		_19 = 19,	_20 = 20,	_21 = 21,
		_22 = 22,	_23 = 23,	_24 = 24,
		_25 = 25,	_26 = 26,	_27 = 27,
 		_28 = 28,	_29 = 29,	_30 = 30,
		_MAXSKILLLLEVEL = 31
	};

	static int  szSkillBoundary[];

	////////////////////////////////////////////////////////////////////////////////////
	// Getters
	////////////////////////////////////////////////////////////////////////////////////	
	SkillMgrMap&				Skills()				  { return m_Skills; }
	SkillIndex&					SkillsIndex()			  { return m_SkillIndex; }
	gString						FileName()			const { return m_gsFileName; }
	CSkill*						GetSkill(gString gsName);
	CSkill*						GetSkill(int nSkill);
	gString						Version();
	IntegerMap					TestResults();
	
	////////////////////////////////////////////////////////////////////////////////////
	// Setters
	////////////////////////////////////////////////////////////////////////////////////
	bool						AddSkill(CSkill* s);								// Add a skill to the Manager
	bool						DeleteSkill(int nSkill);							// Delete a skill from the manager
	bool						ValidSkill(gString gsName);							// Checks if there is a skill with the name
	bool						ValidSkill(int nSkill);								// Checks skill by unique ID
	bool						SetFileName(const gString& s) { m_gsFileName = s; }	// Set the manager's filename
	bool						Increment()	{ m_lVersion += 5;  }					// Increase ePACs version
	bool						Decrement()	{ m_lVersion -= 5;  }					// Decrease ePACs version
	bool						DisableSkill(int nSkill);							// Disable/Enable a skill
	bool						LogSkill(int nSkill);								// Start/Stop logging


	////////////////////////////////////////////////////////////////////////////////////
	// Class methods
	////////////////////////////////////////////////////////////////////////////////////	
	int							Roll(float fSkillRating, int nAttributeRating, int nWorldModifiers);
	int							Roll(float fSkillRating, int nAttributeRating, int nWorldModifiers, int nTaskThreshold);
	int							RollO(float fSkill, int nAttribute, int nWorldMods, float fVictSkill, int nVictAttribute, int nVictWorldMods, int nTaskThreshold);
	int							GetNextId();
	int							GetCost(int nSkill);
	int							GetCost(gString gsString);
	bool						IsEnabled(int nSkill);
	bool						IsLogged(int nSkill);	
	bool						GenerateIndexTable();
	bool						CheckRaise(float fSkillLevel, int nIntelligence);
	bool						IncreaseTest(gString gsType);
	bool						ResetTest();
	bool						OutputTest();
	void						DisplaySkills(CMobile* pM, bool bCurrent);				// Used for displaying skills
	void						DisplaySkill(CSkill* pS, CMobile* pM, int nL, bool bC);	// Recursive
	

	////////////////////////////////////////////////////////////////////////////////////
	// File writing methods to convert class to XML data format
	////////////////////////////////////////////////////////////////////////////////////	
	bool						Load();
	bool						Load(gString szFile);
	bool						Save();
	bool						ReadXml(TiXmlNode * pNode);
	bool						WriteXml(TiXmlNode * pNode);
	bool						ReadSkillXml(TiXmlNode * pNode, CSkill* pParent);
	bool						WriteSkillXml(TiXmlNode * pNode, CSkill* pParent);


private:
	gString						m_gsFileName;		// File name for XML 
	long						m_lVersion;			// ePACS version
	IntegerMap					m_TestResults;		// Holds test results from e-pac(s)
	SkillIndex					m_SkillIndex;		// Allows an easy way to find skill
	SkillMgrMap					m_Skills;			// Holds all loaded skills

};

#endif
