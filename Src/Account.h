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

#ifndef __ACCOUNT_H__
#define __ACCOUNT_H__

#include "MudCore.h"
#include "Set.h"
#include "../gTools/TinyXml.h"

#pragma warning(disable: 4786)

class gString;
class CPlayer;

class CAccount
{
// Methods
public:

	CAccount();
	~CAccount();

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_AccountType		// _AT_ stands for "ACCOUNT_TYPE"
	{
		_AT_TESTING	= 0,			// Account is used for testing. A Free account
		_AT_NORMAL	= 1,			// Account is normal, and charged the normal rate.
	};

	typedef enum e_AccountFlags		// _AF_ stands for "ACCOUNT_FLAG"
	{
		_AF_NONE = 0
	};

	typedef enum e_AccountStatus	// _AS_ stands for "ACCOUNT_STATUS"
	{
		_AS_NONE	 = 0,
		_AS_ACTIVE	 = 1,
		_AS_CLOSED   = 2,
		_AS_BANNED   = 3,
		_AS_ONLINE	 = 4,
		_AS_IDLE	 = 5
	};

	///////////////////////////////////////////////////////////////////////////////////////
	// Constant Member Access Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline  const float			Credits() const	{ return m_fCredits; }
	inline  const float			Debits() const { return m_fDebits; }
	inline	const e_AccountType	Type()	const { return m_AccountType; }
	inline  const CSet			StatusFlags() const	{ return *m_pStatus; }
	inline  const CSet			AccountFlags() const { return *m_pFlags; }
	inline  const gStringList&	PlayerList() const	{ return m_gsPlayerList; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);

	///////////////////////////////////////////////////////////////////////////////////////
	// Public Methods to Add/Remove players
	///////////////////////////////////////////////////////////////////////////////////////
	bool						PlayerListAdd(const CPlayer* pPlayer);
	bool						PlayerListRemove(const CPlayer* pPlayer);
	bool						PlayerListAdd(const gString& gsPlayer);
	bool						PlayerListRemove(const gString& gsPlayer);

private:
	///////////////////////////////////////////////////////////////////////////////////////
	// Member Modification Methods
	///////////////////////////////////////////////////////////////////////////////////////
	bool						SetCredits(const float& fAmount);
	bool						AdjCredits(const float& fAmount);

	bool						SetDebits(const float& fAmount);
	bool						AdjDebits(const float& fAmount);

	bool						SetType(const e_AccountType& eNewType);

	const float					GetAccountRate() const;

// Data
private:
	unsigned int				m_fLastUpdate;		// Last update tick
	float						m_fCredits;			// Any credits they may have
	float						m_fDebits;			// Any debits they may have

	e_AccountType				m_AccountType;		// Type of account this is
	CSet*						m_pStatus;			// Account status flags
	CSet*						m_pFlags;			// Account general flags
	gStringList					m_gsPlayerList;		// List of players belonging to this account
};

#endif
