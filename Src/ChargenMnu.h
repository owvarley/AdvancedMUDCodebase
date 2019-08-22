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
// ChargenMnu.h
//
// This file defines the validation functions that are exported for
// the character creation menu.
//

#ifndef __CHARGENMNU_H__
#define __CHARGENMNU_H__

#pragma warning(disable: 4243)
#pragma warning(disable: 4786)

#include "Menu.h"
//#include "Race.h"

enum e_CharGenStates
{
	_CHARGEN_MNU_NONE				= 0,
	_CHARGEN_MNU_SET_NAME           = 1,
	_CHARGEN_MNU_SHOW_GENDER		= 2,
	_CHARGEN_MNU_SHOW_RACE			= 3,	
	_CHARGEN_MNU_SHOW_ATTRIBUTES	= 4,
	_CHARGEN_MNU_SHOW_ABILITIES		= 5,
	_CHARGEN_MNU_SHOW_BACKGROUND	= 6,
	_CHARGEN_MNU_SHOW_CHARACTER		= 7,
	_CHARGEN_MNU_DELETE_CHARACTER	= 8,
	_CHARGEN_MNU_RESET_DEFAULTS		= 9,
	_CHARGEN_MNU_RETURN_TO_MAIN		= 10,

	_CHARGEN_MNU_VERIFY_NAME        = 100,
	_CHARGEN_MNU_VERIFY_RACE		= 101,
	_CHARGEN_MNU_VERIFY_GENDER		= 102,
	_CHARGEN_MNU_VERIFY_ATTRIBUTES	= 103,
	_CHARGEN_MNU_VERIFY_ABILITIES	= 104,
	_CHARGEN_MNU_VERIFY_BACKGROUND  = 105,
	_CHARGEN_MNU_VERIFY_DELETE		= 106,

};

class CChargenMnu : public CMenu<CChargenMnu>
{
public:
	//CChargenMnu() {m_pRaceMgr=NULL, m_pTextMgr=NULL;}
	//~CChargenMnu() {m_pRaceMgr=NULL, m_pTextMgr=NULL;}

	CChargenMnu() {m_pTextMgr=NULL;}
	~CChargenMnu() {m_pTextMgr=NULL;}
	
	int Process(CUser* pUser);
	int DisplayMenu(CUser* pUser);

	int GenericOption(CUser* pUser);
	int DisplayRaceOptions(CUser* pUser);
	int DisplayGenderOptions(CUser* pUser);
	int DisplayAttributes(CUser* pUser);
	int DisplayAbilities(CUser* pUser);
	int DisplayBackgrounds(CUser* pUser);
	int DisplayDeleteCharacterOptions(CUser* pUser);
	int DisplayResetOptions(CUser* pUser);

	bool VerifyName(CUser* pUser, const gString& gsArgument);
	bool VerifyRace(CUser* pUser, const gString& gsArgument);
	bool VerifyGender(CUser* pUser, const gString& gsArgument);
	bool VerifyAttributes(CUser* pUser, const gString& gsArgument);
	bool VerifyAbilities(CUser* pUser, const gString& gsArgument);
	bool VerifyBackground(CUser* pUser, const gString& gsArgument);
	bool VerifyDelete(CUser* pUser, const gString& gsArgument);

// data
protected:
	CTextMgr*	m_pTextMgr;
};


#endif // __CHARGENMNU_H__

