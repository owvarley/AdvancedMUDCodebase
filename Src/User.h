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

#ifndef __USER_H__
#define __USER_H__

#include <time.h>
#include <map>

#include "MudCore.h"
#include "Set.h"

#include "../gTools/TinyXml.h"

class CSocket;
class CAccount;
class CPlayer;
class gString;


//extern "C" 
class CUser
{
// Methods
public:
	friend class CGameServer;

	CUser();
	~CUser();

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum t_eUserState
	{
		_USR_ERROR      =   0,
		_USR_PLAYING	= 100,
		_USR_IDLE		= 101,
	};

	typedef enum t_eMenuOptions
	{
		_OPT_QUIT = -1,
		_OPT_NO   =  0,
		_OPT_YES  =  1,
	};

	typedef enum t_eMenuStates
	{
		_MNU_INVALID			= -99,
		_MNU_SWITCHING			= -1,
		_MNU_NONE				=  0,

		_MNU_LOGON_MENU         =  1,
		_MNU_MAIN_MENU			=  2,
		_MNU_CHARGEN_MENU       =  3,
		_MNU_OLC_MENU			=  4,
	};

	///////////////////////////////////////////////////////////////////////////////////////
	// Constant Member Access Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline	const gString		Name()			const { return m_gsName; }
	inline  const gString		Email()			const { return m_gsEmail; }
	inline  const gString		Password()		const { return m_gsPassword; }
	inline	const time_t		CreationDate()	const { return m_CreationDate; }
	inline  const time_t		LastDateOn()	const { return m_LastAccessDate; }
	inline  const time_t		TotalTimeOn()	const { return m_TotalTimeOnline; }
	inline  const time_t		LastTimeOn()	const { return m_LastTimeOnline; }
	inline  const int			GUID()			const { return m_iUniqueID; }
	inline  const CSet			Status()		const { return *m_pStatus; }
	inline  const CSet			Flags()			const { return *m_pFlags; }
	inline	const t_eUserState	State()			const { return m_eState; }
	inline	const t_eUserState	LastState()		const { return m_eLastState; }
	
	inline	CAccount*			Account()		{ return m_pAccount; }
	inline	CSocket*			Socket()		{ return m_pSocket;	}
	inline	CPlayer*			Player()		{ return m_pPlayer; }

        ///////////////////////////////////////////////////////////////////////////////////////
	// Menu Functions
	///////////////////////////////////////////////////////////////////////////////////////
	inline  void     			SetMenu(t_eMenuStates eMenu){ m_eCurrentMenu = eMenu; m_nCurrentSubMenu = m_nLastSubMenu = _MNU_SWITCHING; }
	inline  void                SetSubMenu(int nData)		{ m_nLastSubMenu = m_nCurrentSubMenu; m_nCurrentSubMenu = nData; }
	inline  t_eMenuStates		CurrentMenu()			    { return m_eCurrentMenu; }
	inline  int					LastSubMenu()				{ return m_nLastSubMenu; }
	inline  int						CurrentSubMenu()	    { return m_nCurrentSubMenu; }
	inline  void*               MenuData()					{ return m_pMenuData; }
	inline  void                SetMenuData(void* pData)	{ m_pMenuData = pData; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Public Tools
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool				Save();
	virtual bool				Load();
	static  bool				Exists(const gString& gsUser);

	t_eUserState				SetState(t_eUserState eNewState);
	void						Disconnect();

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);

	///////////////////////////////////////////////////////////////////////////////////////
	// Menu System Validation Methods
	///////////////////////////////////////////////////////////////////////////////////////

	// Logon Menu
	friend _export int			ValidateUserName(CUser* pUser, const gString& gsArguments);
	friend _export int			SetUserPassword(CUser* pUser, const gString& gsArguments);

	// User Menu
	friend _export int			CreatePlayer(CUser* pUser, const gString& gsArguments);
	friend _export int			DisconnectPlayer(CUser* pUser, const gString& gsArguments);
	friend _export int			DeleteAccount(CUser* pUser, const gString& gsArguments);
	friend _export int			SetupPlayer(CUser* pUser, const gString& gsArguments);

	///////////////////////////////////////////////////////////////////////////////////////
	// Member Modification Methods
	///////////////////////////////////////////////////////////////////////////////////////
	bool						SetPlayer(CPlayer* pPlayer, bool bSave=true);
	bool						SetAccount(CAccount* pAccount);

public:
	bool						SetName(const gString& gsNewName);
	bool						SetEmail(const gString& gsNewEmail);
	bool						SetPassword(const gString& gsNewPassword);
	bool						SetCreationDate(time_t tNewDate);
	bool						SetLastDateOnline(time_t tNewDate);
	bool						SetTotalTimeOn(time_t tNewTime);
	bool						IncTimeOnline(time_t tAdd);
	bool						SetLastTimeOn(time_t tNewTime);
	bool						IncLastTimeOn(time_t tAdd);
	bool						SetSocket(CSocket* pSocket);

// Data
private:
	gString						m_gsName;			// User Name
	gString						m_gsEmail;			// User Email
	gString						m_gsPassword;		// User Password

	time_t						m_CreationDate;		// Date User was created
	time_t						m_LastAccessDate;	// Last date user was accessed
	time_t						m_TotalTimeOnline;	// Total time spent online
	time_t						m_LastTimeOnline;	// Time spent online in the last session
	float						m_fLastUpdate;		// Last update tick
	int							m_iUniqueID;		// Global unique identifier

	CSet*						m_pStatus;			// User status flags
	CSet*						m_pFlags;			// User general flags

	t_eUserState				m_eState;			// User current state
	t_eUserState				m_eLastState;		// User last state
	CAccount*					m_pAccount;			// User Account information
	CPlayer*					m_pPlayer;			// User's Active Player
	CSocket*					m_pSocket;			// User Socket

	// This menu data should really go into a struct...
	t_eMenuStates				m_eCurrentMenu;		// Currently Active Menu
	int							m_nCurrentSubMenu;	// Current Sub Menu
	int							m_nLastSubMenu;		// Last sub menu
	void*						m_pMenuData;        // Current Menu Input Data
};

typedef std::map<int, CUser*> UserMap;


#endif