
#ifndef __MAINMNU_H__
#define __MAINMNU_H__

#include "Menu.h"

#pragma warning(disable: 4243)
#pragma warning(disable: 4786)

enum e_MainMnu
{
	_MAIN_MNU_CREATE_CHAR				= 1,
	_MAIN_MNU_DELETE_CHAR				= 2,
	_MAIN_MNU_DELETE_ACCOUNT			= 3,
	_MAIN_MNU_LOGIN_CHAR				= 4,
	_MAIN_MNU_LIST_CHARS				= 5,
	_MAIN_MNU_QUIT						= 6,
	_MAIN_MNU_COUNT						= 7,

	_MAIN_MNU_SELECTED_LOGIN_CHAR		= 10,
	_MAIN_MNU_SELECTED_DELETE_CHAR		= 11,
	_MAIN_MNU_SELECTED_DELETE_ACCOUNT	= 12,
};

class CMainMnu : public CMenu<CMainMnu>
{
public:
	CMainMnu();
	~CMainMnu() {};
	
	int DisplayMenu(CUser* pUser);
	int Process(CUser* pUser);

	int DeleteCharacter(CUser* pUser);
	int DeleteAccount(CUser* pUser);
	int LoginCharacter(CUser* pUser);
	int ListCharacters(CUser* pUser);
	int Quit(CUser* pUser);
	int PlayGame(CUser* pUser);
};



#endif // __MAINMNU_H__

