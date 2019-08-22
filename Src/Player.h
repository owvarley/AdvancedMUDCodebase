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

#ifndef __PLAYER_H__
#define __PLAYER_H__

#include <time.h>

#include "MudCore.h"
#include "gString.h"
#include "Actor.h"
#include "User.h"
#include "OTools.h"
#include "Space.h"
#include "Mobile.h"


class CPlayer : public CMobile
{
public:

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	// Player specific events
	// These enumerations define player event IDs. These numbers need to be unique, and so
	// are defined by the range given in EVRANGE_FIRST through EVRANGE_LAST
	typedef enum e_PlayerEvents
	{
		EV_FIRST_PLAYER_EVENT = EVRANGE_FIRST_PLAYER,
		EV_LAST_PLAYER_EVENT  = EVRANGE_LAST_PLAYER
	};


	CPlayer();
	virtual ~CPlayer();

	inline const gString	Password() const { return m_gsPassword; }
	inline const gString	Prompt() const { return m_gsPrompt; }
	inline const int		Trust()	const { return m_iTrust; }
	inline const time_t&	LoggedOn() const { return m_LogonTime; }
	inline CUser*			User() { return m_pUser; }

	inline int				CreationPoints() { return m_nCreationPoints; }
	inline void				SetCreationPoints(int nValue) { m_nCreationPoints = nValue; }

	bool					SetPassword(const gString& sNewPassword);
	bool					SetPrompt(const gString& sNewPrompt);
	bool					SetTrust(int iTrust);
	bool					SetUser(CUser* pUser);

	virtual bool			Save();
	virtual bool			Load();
	static  bool			Exists(const gString& gsUserName, int nID,  const gString& gsPlayer);
	static  bool			IsValidPlayer(CPlayer* pPlayer);
	virtual int				Write(char *fmt,...);
	virtual void			Update(bool bForce = false);
	virtual void			Think(bool bForce = false);
	virtual void			DescribeTo(CActor* pA, bool bFull = false) const;
	virtual int		        CurrentPointValue();

	// Serialization
	friend std::ostream&	operator << ( std::ostream& stream, const CPlayer& player );
	friend std::istream&	operator >> ( std::istream& stream, CPlayer& player );
	void					WriteXml(TiXmlNode* pParent);
	void					ReadXml(TiXmlNode* pParent);

	// Event Handling
	virtual bool			HandleEvent(CEvent& Event);

// Data
public:
	// Used for Designing Templates
	CTemplate*			m_Template;
	CFrame*				m_Frame;
	CHull*				m_HullCube;
	CComponent*			m_Component;
	CWeaponGroup*		m_Weapon;

	// Attribute


	// TEMP TESTING VARIABLES
	CCart*				m_Location;
	CCart*				m_Heading;
	gString				m_Sector;

private:
	gString				m_gsPassword;
	gString				m_gsPrompt;
	int					m_iTrust;
	int					m_nCreationPoints;	
	time_t				m_LogonTime;
	CUser*				m_pUser;

};	



#endif
