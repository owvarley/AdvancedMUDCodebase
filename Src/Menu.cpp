//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: MenuMgr
// Header   :: Menu.h
// Function :: Contains the overall Menu controlling for moving users between menus


#include "MudCore.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "User.h"
#include "Socket.h"

#include "Types.h"
#include "Menu.h"
#include "LogonMnu.h"
#include "MainMnu.h"
#include "ChargenMnu.h"

#include "../gTools/Tools.h"
#include "../gTools/gString.h"



int MenuMgr::Process(CUser*& pUser)
{
	if ( !pUser || pUser->CurrentMenu() <= _MNU_NONE )
		return -1;

	if ( pUser->CurrentSubMenu() == _MNU_SWITCHING )
		pUser->SetSubMenu(_MNU_NONE);

	switch ( pUser->CurrentMenu() )
	{
		case _MNU_LOGON_MENU:
			return CLogonMnu::Get().Process(pUser);
			break;
		case _MNU_MAIN_MENU:
			return CMainMnu::Get().Process(pUser);
			break;
		case _MNU_CHARGEN_MENU:
			return CChargenMnu::Get().Process(pUser);
			break;
		default:
			break;
	}

	return 1;
}

