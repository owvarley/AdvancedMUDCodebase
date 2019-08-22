
#ifndef __LOGONMNU_H__
#define __LOGONMNU_H__

#include "../gTools/gString.h"
#include "Menu.h"

#pragma warning(disable: 4243)
#pragma warning(disable: 4786)

enum e_LogonStates
{
	_LOGON_MNU_GET_NAME				= 1,
	_LOGON_MNU_VERIFY_NAME			= 2,
	_LOGON_MNU_GET_PASSWORD			= 3,
	_LOGON_MNU_VERIFY_PASSWORD		= 4,
	_LOGON_MNU_GET_NEW_PASSWORD		= 5,
	_LOGON_MNU_VERIFY_NEW_PASSWORD	= 6
};

class CLogonMnu : public CMenu<CLogonMnu>
{
public:
	CLogonMnu();
	~CLogonMnu();
	
	int Process(CUser* pUser);
	int DisplayMenu(CUser* pUser);
	int GetUserName(CUser* pUser);
	int GetUserPassword(CUser* pUser);
	int GetNewName(CUser* pUser);
	int GetNewPassword(CUser* pUser);

	bool VerifyUserName(CUser* pUser, const gString& gsArguments);
};



#endif // __LOGONMNU_H__
