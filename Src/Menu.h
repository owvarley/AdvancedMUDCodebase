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
// CMenu, SCreen, & MenuMgr Class Definitions
//

#ifndef __MENU_H__
#define __MENU_H__

#include "MudCore.h"
#include "Types.h"
#include "TextMgr.h"
#include "Socket.h"

#pragma warning(disable: 4243)
#pragma warning(disable: 4786)

#include "../gTools/Singleton.h"

template <class T>
class CMenu : private CSingleton<T>
{
public:
	CMenu() {m_pTextMgr=NULL;}
	virtual ~CMenu() {};
	virtual int Process(CUser* pUser) = 0;
	virtual int DisplayMenu(CUser* pUser) = 0;
	virtual void HandleHelpRequest(CUser* pUser, const gString& gsArgument);

	using CSingleton<T>::Get;

protected:
	CTextMgr*				m_pTextMgr;
};

template<class T>
T* CMenu<T>::m_pInstance = NULL;

class MenuMgr
{
public:
	MenuMgr() {};
	~MenuMgr() {};
	
	// Process
	// Processes the menu loop
	int Process(CUser*& pUser);
};

template <class T>
void CMenu<T>::HandleHelpRequest(CUser* pUser, const gString& gsArgument)
{
	static CGameObjects& Tools = CGameObjects::Get();

	if ( pUser->Socket() )
	{
		Tools.HelpSystem().HandleHelpRequest(pUser, gsArgument);
		pUser->Socket()->Write("\n\rPress #600[#601Enter#600]#700 to continue.\n\r");
	}
}

#endif

