//
// MudCore
//
// MudCore is copyright (c) 2000, 2001 by Gary McNickle
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
// Affect Class Definition
//
// An 'Affect' is something that alters or affects the aspect or
// behaviour of an atomic object. (see Atomic.h for a description
// of this class).
//


#ifndef __AFFECT_H__
#define __AFFECT_H__

#include <vector>
#include <Timer.h>
#include "../gTools/TinyXml.h"

class CAffect
{
// Class Typedefs
public:
	typedef enum e_AffectType
	{
		AF_NONE		= 0,
		/* Some possible affect types might include...
		AF_HP,
		AF_STR,
		AF_INT,
		AF_WIS,
		AF_DEX,
		AF_CON,
		AF_SPD,
		AF_WLP,
		AF_CHA,
		AF_SKILL_ROLL,
		*/
		AF_NUM_AFFECTS
	};

	static char *szAffectTypes[];


// Methods
public:
	CAffect();
	virtual ~CAffect();

	///////////////////////////////////////////////////////////////////////////////////////
	// Handy operator overrides
	///////////////////////////////////////////////////////////////////////////////////////
	CAffect& operator=(CAffect* clone);
	CAffect& operator=(const CAffect& clone);
	bool operator==(CAffect& affect);
	bool operator==(CAffect* affect);
	bool operator!=(CAffect& affect);
	bool operator!=(CAffect* affect);


	///////////////////////////////////////////////////////////////////////////////////////
	// Methods that return values of members
	///////////////////////////////////////////////////////////////////////////////////////
	inline const e_AffectType	Type() const { return m_eType; }
	inline const float			Apply() const { return m_fApply; }
	inline const time_t			CreationDate() const { return m_CreationDate; }
	inline const long			LifeSpan() const { return m_nLifeSpan; }
	inline const int			Strength() const { return m_nStrength; }
	inline const long			OwnerID() const { return m_nOwnerID; }
	inline const long			OriginID() const { return m_nOriginID; }
	inline const bool			IsExpired() { return m_nLifeSpan >= m_Timer.Seconds(); }
	inline const bool			IsActive() { return !IsExpired() && m_Timer.IsRunning(); }

	///////////////////////////////////////////////////////////////////////////////////////
	// Methods that set values of members
	///////////////////////////////////////////////////////////////////////////////////////
	inline void					SetApply(float a) { m_fApply = a; }
	inline void					SetType(e_AffectType t) { m_eType = t; }
	inline void					SetCreationDate(time_t d) { m_CreationDate = d; }
	inline void					SetLifeSpan(long nSeconds) { m_nLifeSpan = nSeconds; }
	inline void					SetOwnerID(long n) { m_nOwnerID = n; }
	inline void					SetOriginID(long n) { m_nOriginID = n; }

	inline void					SetStrength(int l) { m_nStrength = l; }
	inline void					SetStrengthLow() { m_nStrength = 15; }
	inline void					SetStrengthMid() { m_nStrength = 50; }
	inline void					SetStrengthHigh(){ m_nStrength = 85; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Time Related Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline void					AddSecondsToLife(long nSeconds) { m_nLifeSpan += nSeconds; }
	inline void					AddMinutesToLife(long nMinutes) { m_nLifeSpan += nMinutes*60; }
	inline void					AddHoursToLife(long nHours) { m_nLifeSpan += nHours*60*60; }
	inline void					AddDaysToLife(long nDays) { m_nLifeSpan += (nDays*24)*60*60; }
	inline const float			TimeElapsed() { return m_Timer.Elapsed(); }
	inline const float			TimeRemaining() { return m_nLifeSpan - m_Timer.Elapsed(); }

	///////////////////////////////////////////////////////////////////////////////////////
	// Utility Methods
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void				Update(bool bForce=false);
	virtual void				Activate();
	virtual void				DeActivate();

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream&		operator << ( std::ostream& stream, const CAffect& aff );
	friend std::istream&		operator >> ( std::istream& stream, CAffect& aff );

	virtual void				WriteXml(TiXmlNode* pParent);
	virtual void				ReadXml(TiXmlNode* pParent);


// Data
protected:

	// Strength reflects the overall strength of the affect.
	int							m_nStrength;

	// Type determines what to apply the 'Apply' value towards.
	e_AffectType				m_eType;

	// The 'apply' value is the amount to apply to the actor, based on the Type
	float						m_fApply;
	time_t						m_CreationDate;

	// The OwnerID is the unique id of the actor that this affect is applied to
	long						m_nOwnerID;

	// The OriginID is the unique id of the actor that created this affect
	long						m_nOriginID;

	// LifeSpan is the time, in seconds, that this affect will last
	long						m_nLifeSpan;

	// Simple timer utility member
	CTimer						m_Timer;
};


typedef std::vector<CAffect*> Affectlist;

#endif