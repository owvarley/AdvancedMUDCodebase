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

// Class    :: CMainMnu
// Header   :: MainMnu.h
// Function :: Contains all the validation routines needed for the Main Menu, this includes
//			:: the bulk of the Account handling routines.


#include <cstdio>
#include <direct.h>
#include <io.h>
#include <ctime>
#include <cassert>

#include "MudCore.h"
#include "Account.h"
#include "Player.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Socket.h"
#include "User.h"
#include "Socket.h"
#include "MainMnu.h"

#include "../gTools/Tools.h"
#include "../gTools/Set.h"
#include "../gTools/Log.h"
#include <md5/md5.h>


#define PLAYER_LOGGED_IN	1
#define INVALID_PLAYER		2
#define CHARACTER_DELETED   3
#define ACCOUNT_DELETED     4

CMainMnu::CMainMnu()
{

}

int CMainMnu::DisplayMenu(CUser* pUser)
{
	pUser->Socket()->Write("\n"
		"1#600)#701 Create New Character#700\n\r"
		"2#600)#701 Delete Character#700\n\r"
		"3#600)#701 Delete Account#700\n\r"
		"4#600)#701 Login Character#700\n\r"
		"5#600)#701 List Active Characters#700\n\r"
		"6#600)#701 Quit#700\n\r\n\r"
		"Select #600[#601 %d#600 -#601 %d#600]#700 ", 
		_MAIN_MNU_CREATE_CHAR, 
		_MAIN_MNU_COUNT-1);

	return 1;
}


int CMainMnu::DeleteCharacter(CUser* pUser)
{
	gStringList::const_iterator pos;
	gString gsName;
	gString gsArgument = pUser->Socket()->GetInput();

	if ( !CPlayer::Exists(pUser->Name(), pUser->GUID(), gsArgument) )
	{
		pUser->Socket()->Write("\n\rThat player does not exist!\n\r");
		return 0;
	}

	for ( pos  = pUser->Account()->PlayerList().begin();
	      pos != pUser->Account()->PlayerList().end();
		  pos++ )
		  {
			gsName = *pos;

			if ( gsArgument.CompareNoCase( gsName ) == 0 )
			{
				gString szFile;

				szFile.Format("%s\\%s_%d\\%s",
					(const char*)CGameObjects::Get().m_Config.szDir[CGameObjects::_PLAYER],
					(const char*)pUser->Name(), pUser->GUID(), (const char*)gsName);

				if ( unlink(szFile) == 0 )
				{
					pUser->Account()->PlayerListRemove(gsName);
					pUser->Socket()->Write("\'%s\' has been deleted.\n\rHit #600[#601Enter#600]#700 to continue.\n\r", (const char*)gsName);
					return 1;
				}

				pUser->Socket()->Write("An error has occured while attempting to delete \'%s's\' player file.",
					(const char*)gsName);

				return 0;
			}
	  }

	return 0;
}

int CMainMnu::DeleteAccount(CUser* pUser)
{
	gStringList::const_iterator pos;
	gString szFile;

	if ( !CUser::Exists(pUser->Name()) )
	{
		pUser->Socket()->Write("\n\rBut... You dont exist!\n\r");
		return 0;
	}

	// Delete each player owned by this user
	pUser->Socket()->Write("Deleting Characters.\n\r");

	for ( pos  = pUser->Account()->PlayerList().begin();
	      pos != pUser->Account()->PlayerList().end();
		  pos++ )
		  {
			pUser->Socket()->Write("%s deleted.\n\r", (const char*)*pos);

			szFile.Format("%s%s_%d\\%s",
				(const char*)CGameObjects::Get().m_Config.szDir[CGameObjects::_PLAYER],
				(const char*)pUser->Name(), pUser->GUID(), (const char*)*pos);

			unlink(szFile);
		  }

	// Delete the user's player file directory
	szFile.Format("%s%s_%d",
		(const char*)CGameObjects::Get().m_Config.szDir[CGameObjects::_PLAYER],
		(const char*)pUser->Name(), pUser->GUID());

	rmdir(szFile);

	// Delete the user
	szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_USER] + pUser->Name();

	unlink(szFile);

	pUser->Socket()->Write("\n\rYour account has been deleted. Hit #600[#601Enter#600]#700 to return to login.\n\r");
	pUser->SetAccount(new CAccount);

	return ACCOUNT_DELETED;
}

int CMainMnu::PlayGame(CUser* pUser)
{
	LOG_SCOPE("CMainMnu::PlayGame"); 

	CGameWorld* pWorld=NULL;
	CGameObjects& globals = CGameObjects::Get();

	pWorld = globals.GetWorld( pUser->Player()->Position().World() );

	if ( !pWorld )
	{
		g_Log.Log(LOG_ERROR, "[CMainMnu::>>] Error locating Homeworld (#%d) for %s. Defaulting\n",
				  pUser->Player()->Position().World(), (const char*)pUser->Player()->Name());

		pUser->Socket()->Write("We cant seem to locate your last world. Loading you into the default world.\n");
		pWorld = globals.GameWorld();
	}

	// Motd goes here
	CGameObjects::Get().WriteMotd(pUser, 10, false);

	pWorld->Add(pUser->Player(), pUser->Player()->Position());
	g_Log.Log(LOG_INFO, "%s {%s} has entered the world as \'%s\'.\n\r",
		(const char*)pUser->Name(), (const char*)pUser->Socket()->Host(), pUser->Player()->Name());

	if (pUser->Player()->CurrentRoom())
		pUser->Player()->CurrentRoom()->Write(pUser->Player(), "%s appears suddenly, slipping from the background.\n\r", pUser->Player()->Name());

	pUser->SetState(CUser::_USR_PLAYING);
	pUser->SetMenu(CUser::_MNU_NONE); 
	pUser->Save();

	return 1;
}

int CMainMnu::LoginCharacter(CUser* pUser)
{
	gStringList::const_iterator pos;
	gString gsName = pUser->Socket()->GetInput();

	gsName.MakeProper();

	// Make sure the player even exists
	if ( !CPlayer::Exists(pUser->Name(), pUser->GUID(), gsName) )
	{
		pUser->Socket()->Write("\n\rThat player does not exist!\n\r");
		return INVALID_PLAYER;
	}

	gString gsFound;

	// make sure they are one of our players!
	for ( pos  = pUser->Account()->PlayerList().begin();
	      pos != pUser->Account()->PlayerList().end();
		  pos++ )
		  {
			if ( gsName.CompareNoCase( (*pos) ) == 0 )
			{
				// Yep, our player.
				CPlayer* pPlayer = new CPlayer;

				pPlayer->SetName((*pos));
				pPlayer->SetUser(pUser);

				if ( pPlayer->Load() )
				{
					pUser->SetPlayer(pPlayer);

					return PLAYER_LOGGED_IN;
				}

				delete pPlayer;
				return INVALID_PLAYER;
			}
	  }

	return INVALID_PLAYER;
}

int CMainMnu::ListCharacters(CUser* pUser)
{
	gStringList::const_iterator pos;
	_finddata_t p_file;
	long hFile;
	gString gsFile;

	int nCount = 0;

	pUser->Socket()->Write("You have %d player characters. They are:\n\r\n\r"
		                   "-Player Name-----------------Last Modified-----------\n\r",
							pUser->Account()->PlayerList().size());

	for ( pos  = pUser->Account()->PlayerList().begin();
	      pos != pUser->Account()->PlayerList().end();
		  pos++ )
	  {
			gsFile.Format("%s%s_%d\\%s",
			  (const char*)CGameObjects::Get().m_Config.szDir[CGameObjects::_PLAYER],
			  (const char*)pUser->Name(), pUser->GUID(), (const char*)*pos);

			if( (hFile = _findfirst( gsFile, &p_file )) != -1 )
			{
				pUser->Socket()->Write("%2d] %-24s %.24s\n\r",
					++nCount, (const char*)*pos, ctime(&(p_file.time_write)));

				_findclose(hFile);
			}
	  }

	pUser->Socket()->Write("\n\r[Hit Enter to Continue]\n\r");

	return 1;
}

int CMainMnu::Quit(CUser* pUser)
{
	pUser->Socket()->Write("\n\rThank you, come again.\n\r");
	pUser->Socket()->SetState(CSocket::_DISCONNECT);

	return 0;
}


int CMainMnu::Process(CUser* pUser)
{
	gString gsArgument = pUser->Socket()->GetInput();

	switch ( pUser->CurrentSubMenu() )
	{
		case _MAIN_MNU_SELECTED_LOGIN_CHAR:
			{
				if ( LoginCharacter(pUser) == PLAYER_LOGGED_IN )
					PlayGame(pUser);
				else
					pUser->SetSubMenu(0);
			}
			break;

		case _MAIN_MNU_SELECTED_DELETE_CHAR:
			{
				DeleteCharacter(pUser);
				pUser->SetSubMenu(0);
			}
			break;

		case _MAIN_MNU_SELECTED_DELETE_ACCOUNT:
			{
				gsArgument.MakeLower();

				if ( gsArgument[0] == 'y' )
				{
					if ( DeleteAccount(pUser) == ACCOUNT_DELETED )
					{
						pUser->SetName("");
						pUser->SetPassword("");
						pUser->SetMenu(CUser::_MNU_LOGON_MENU);
					}
				}
				else
				{
					DisplayMenu(pUser);
					pUser->SetSubMenu(0);
				}
			}
			break;


		default: // Show Main Menu
			{
				switch ( atoi(gsArgument) )
				{

					case _MAIN_MNU_CREATE_CHAR:
						// Switch to character creation menu
						pUser->SetMenu(CUser::_MNU_CHARGEN_MENU);
						break;

					case _MAIN_MNU_DELETE_CHAR:
						{
							pUser->Socket()->Write("Delete Which Character? ");
							pUser->SetSubMenu(_MAIN_MNU_SELECTED_DELETE_CHAR);
						}
						break;

					case _MAIN_MNU_DELETE_ACCOUNT:
						{
							pUser->Socket()->Write("Are you sure you want to delete your account? ");
							pUser->SetSubMenu(_MAIN_MNU_SELECTED_DELETE_ACCOUNT);
						}
						break;

					case _MAIN_MNU_LOGIN_CHAR:
						{
							pUser->Socket()->Write("Login Which Character? ");
							pUser->SetSubMenu(_MAIN_MNU_SELECTED_LOGIN_CHAR);
						}
						break;

					case _MAIN_MNU_LIST_CHARS:
						ListCharacters(pUser);
						break;

					case _MAIN_MNU_QUIT:
						Quit(pUser);
						break;

					default:
						DisplayMenu(pUser);
						break;
				}
			}
			break; 
	}

	return 1;
}
