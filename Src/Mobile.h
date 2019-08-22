//
// MudCore
//
// MudCore is copyright (c) 2000, 2006 by Gary McNickle
// <gary#mcnickle.org>
//
// MudCore is free software; you can redistribute it and/or modify
// it under the terms of the MudCore license contained in the
// included file "license.txt".
//
// You should have received a copy of the MudCore license along with
// MudCore, if not, write to the author and request a copy.
//
// Gary McNickle
// <gary#mcnickle.org>
// 5408 E. 10th St
// Indianapolis, IN 46219 USA
//

//
// CMobile
//
// An intermediate class for all intelligent/animated actors
//

#if !defined __MOBILE_H__
#define __MOBILE_H__
#include <map>

#include <ctime>

#include "MudCore.h"
#include "Attributes.h"
#include "Atomic.h"
#include "Epacs.h"

#include <../gTools/Placement.h>
#include <../gTools/Set.h>
#include <../gTools/Property.h>

class CItem;
class CRoom;
class CGameServer;
class CGameWorld;
class CDirectionList;

class CMobile: public CActor
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CGameServer;
	friend class CGameWorld;

// Methods
public:
							CMobile();
	virtual					~CMobile();


	//  MobileEvents
	//  These are events that all mobiles should know of, and possibly catch.
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum t_eMobileEvents
	{
		EV_FIRST_MOBILE_EVENT       =  EVRANGE_FIRST_MOBILE,
		EV_MOVE_TO_NEXT_ROOM,
		EV_LAST_MOBILE_EVENT		=  EVRANGE_LAST_MOBILE
	};

	//  Gender
	//  These are the different genders
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_Gender
	{
		_MALE	 = 0,
		_FEMALE  = 1,
		_NEUTRAL = 2,
		_MAXGENDER
	};

	// Our copy constructors
	CMobile(const CMobile& a);

	///////////////////////////////////////////////////////////////////////////////////////
	// Static character arrays that hold human-readable verions of the above types
	///////////////////////////////////////////////////////////////////////////////////////
	static char *szMobileGender[];

	// Some useful operator overrides
	CMobile& operator =		(const CMobile& clone);
	bool	 operator ==	(const CMobile& a2 );
	bool	 operator !=	(const CMobile& a2 );

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline  const	float			Weight()		const   { return m_fWeight; }
	inline  const   UINT			Antagonist()	const   { return m_uiAntagonist; }
	inline  const	CActor*			WatchActor()	const   { return m_pWatchActor; }
    inline  const   gString         Race()          const	{ return m_gsRace; }
	inline  const   gString			gsGender()		const   { return szMobileGender[m_nGender]; }
	inline  const   int				Gender()		const	{ return m_nGender; }
	inline  SkillMap&				Skills()				{ return m_Skills; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual functions that can be called to set the values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					SetWatchActor(CActor* pWatcher);
	virtual bool			        SetRace(const gString& gsRace);
	virtual bool					SetGender(const gString& gsGender);
	virtual bool					SetGender(const int nGender);

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					Save();
	virtual bool					Load();
	virtual int						Write(char *fmt,...);
	virtual void					Update(bool bForce = false);
	virtual void					Think(bool bForce = false);
	virtual void					DescribeTo(CActor* pA, bool bFull = false) const;
	virtual bool					FollowPath(CDirectionList& List, float fTime=0.5f);
	virtual bool					ExecuteCommand(const gString& gsCmd, char* szArgs,...);
	virtual void					ProcessAfterLoad() {}; 
	virtual int						GetSkill(int nSkill);
	virtual int						GetSkill(gString gsName);
	virtual bool					AddSkill(int nSkill);
	virtual bool					AddSkill(gString gsName);
	virtual bool					RemoveSkill(int nSkill);
	virtual bool					RemoveSkill(gString gsName);

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Method Overrides
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					HandleEvent(CEvent& Event);

	///////////////////////////////////////////////////////////////////////////////////////
	// Event Handlers
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					OnDamage(CEvent& Event);
	virtual bool					OnMoveToNextRoom(CEvent& Event);

	virtual void					WriteXml(TiXmlNode* pParent);
	virtual void					ReadXml(TiXmlNode* pParent);

// Data
protected:
	// The GUID of the actor attacking this actor
	UINT							m_uiAntagonist;

	// The actor that is currently seeing everything this actor sees
	CActor*							m_pWatchActor;

	// Skill map
	SkillMap						m_Skills;
		
	float							m_fWeight;
		
	gString							m_gsRace;

	int								m_nGender;

};

typedef std::map<int, CMobile*> t_mMobiles;

class EMoveToNextRoom : public CEvent
{
public:
	EMoveToNextRoom();
	virtual ~EMoveToNextRoom();

	CDirectionList* m_pList;
	float m_fTime;
};

#endif // __MOBILE_H__

