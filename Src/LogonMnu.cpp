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

// Class    :: CLogonMnu
// Header   :: LogonMnu.h
// Function :: Contains all the validation routines needed for the Logon Menu, including
//			:: the ASCII greeting and account creation handling

#include "MudCore.h"
#include "User.h"
#include "Player.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Socket.h"
#include "LogonMnu.h"
#include "TextMgr.h"

#include "../gTools/Tools.h"
#include "../gTools/Set.h"
#include "../gTools/Log.h"
#include "../gTools/LexicalCast.h"
#include <md5/md5.h>

#define UNKNOWN_NAME						1
#define USER_REQUESTED_NEW_ACCOUNT			2
#define RECEIVED_VALID_PASSWORD				3
#define FAILED_PASSWORD_ATTEMPTS			4
#define ENTERED_NEW_PASSWORD                5

CLogonMnu::CLogonMnu()
{
	LOG_SCOPE("CLogonMnu::CLogonMnu()");

	// Load text manager...
	g_Log.Log(LOG_INFO, "Loading Text Entries...");
	m_pTextMgr = new CTextMgr;

	if ( m_pTextMgr->Initialize("logonMnu.xml") )
	{
		g_Log.Log(LOG_INFO, "Done. TextMgr contains %d entries.", m_pTextMgr->Size());
	}
	else
	{
		g_Log.Log(LOG_INFO, "Unable to intialise Text Manager");
		delete m_pTextMgr;
		m_pTextMgr = NULL;
	}
}

CLogonMnu::~CLogonMnu()
{
	delete m_pTextMgr;
}


int CLogonMnu::DisplayMenu(CUser* pUser)
{
	gString gsCopyright;
	gString gsWelcome;

	if (!m_pTextMgr)
	{
		g_Log.Log(LOG_INFO, "[CLogonMnu::>>] NULL TextMgr. Unable to continue.");
		return 0;
	}

	// To preserve formating we need to display the greeting in hardcore
	pUser->Socket()->Write("         #601   ___________      __           #601 _______________________________ \n\r");
	pUser->Socket()->Write("        #601   /   _____/  \\    /  \\          #601 \\_   _____/\\______   \\______   \\\n\r");
	pUser->Socket()->Write("        #601   \\_____   \\   \\/\\/   / #600  ______ #601  |    __)_  |       _/|     ___/\n\r");
	pUser->Socket()->Write("        #601   /         \\        / #600  /_____/ #601  |        \\ |    |   \\|    |    \n\r");
	pUser->Socket()->Write("        #601  /_______  / \\__/\\  /  #600          #601 /_______  / |____|_  /|____|    \n\r");
	pUser->Socket()->Write("        #601          \\/       \\/               #601       \\/         \\/   \n\r");
	pUser->Socket()->Write("#701*       *                      *         *            #601++++++++#600=======#700====#701=======\n\r");
	pUser->Socket()->Write("   #701*             *    #001#########Xxxx######=+#001######         *        #601=+++====#600=======#700=-=---#701--====\n\r");
	pUser->Socket()->Write(" #701*     *     *        #001X===========#001x#701 *  *           #601 ;=+xxx#600xxxxx+++#700+x+++++#701++++xxx+\n\r");
	pUser->Socket()->Write("   #701                 #001  x          #001 x   #701  *          #601.;==+++xx#600xxXXxxxx#700xxxxxxx#701xxxxxx\n\r");
	pUser->Socket()->Write("     #701*       *        #001x+=-,. .,-=#001+x   #701        *  #601 --==+++#600=+++xxxx#700xxxxxxxx#701xxxxxxx\n\r");
	pUser->Socket()->Write("          #701*             #001 |#701|||||#001|                 #601- .-=++++++#600+++++++x#700xxxxxx#701xxxxxx\n\r");
	pUser->Socket()->Write("#201=.,#200;=###     #701         #001 X+++xxxxxxxxxX#001#XX    #701 *     #601.  ,;-==+++#600++++++++#700+x++++++#701xXXx\n\r");
	pUser->Socket()->Write("#201;#200-=-= ###   #701*     *  #001X#701=-=+++++x+======+x#001##         #601; .,,;;;;;--#600========#700=+======#701=++\n\r");
	pUser->Socket()->Write("#201--#200=-= ###   #701      #001 Xx++#701+==+++=;-,.,-#001--;.#001-+=+       #601= .,,;----;--=#600=++++xxxxx#700xxx+++#701+\n\r");
	pUser->Socket()->Write("#201xx#200x+x###  #701*      #001x=;--#701-=++==;,-=====#001=--=-#001=x+++  #701*#601  x .,;;---=+#600====++++++#700xxxXX#701XXXXx\n\r");
	pUser->Socket()->Write("  #701         #001X+-;;-==;.-++=+=+x=-,.. ..   #001  .#001=++   #601###..;--;--=+xx++=+#600==+++==+++#700+++#701+\n\r");
	pUser->Socket()->Write("   #701      #001x=,,-========#701++,.x-.  .,-==+x#001x+++-,;#001=+  #601###X.-=++=====++===#600==-=-----=-#700=#701==\n\r");
	pUser->Socket()->Write("  #701*    #001x-;--==+====+=,#701-+-.+ .-=xx+++====#001---x+#001+++ #601 ###+-=+xxx+=-==++#600=======---#700--=++\n\r");
	pUser->Socket()->Write("  #701      #001x============, #701.,-=-=+==;      ..;#001=+-=#001+++  #601###x+++xxX+====+xx#600xxxx+=====+#700++\n\r");
	pUser->Socket()->Write("  #701        #001 X+=-======-=+=#701=====--;;-=====+Xx+#001=--=#001=++  #601X++=+++=+++xxx#600+xxx+++x+x#700x+x\n\r");
	pUser->Socket()->Write(" #701*   *       #001 X+;---=======#701=----===-====-==++#001++==#001+++ #601 ###X+++++++++==#600=+++++xxxx#700XXx\n\r");
	pUser->Socket()->Write(" #701              #001 XX+-;=--====-#701--;-;;;-==-;,,;---#001---#001-=+=  #601###Xxx++++==+++++#600xxxxxxx#700xx\n\r");
	pUser->Socket()->Write("  #701*               #001  x#+--------=#701====-;;,,,;;;-==#001====#001===      #601###Xxxxxxxxxxxx#600+++++++++\n\r");
	pUser->Socket()->Write("      #701*   *          #001  x==------==#701=;;;---------=+=#001====#001=-     #601######XXXxxxxx###xx#600xxxxx+\n\r");
	pUser->Socket()->Write("          #701   *           #001 x-;--;----#701------;;--;;-=+#001====#001--   #701   *        *\n\r");
	pUser->Socket()->Write("   #701                       #001  #x=-;;,,;;#701;;---;,,;,..,--#001-=#001--;   #701                *\n\r");
	pUser->Socket()->Write("   #701*      *       *          #001  +;+x=-,,,;;#701;-;..,,;--==#001---#001;-  #701   *      *\n\r");
	pUser->Socket()->Write("   #701                   *          #001 ++-+;;##+-#701,,;-----;--#001==-#001--,   #701   *\n\r");
	pUser->Socket()->Write("             #701*                      #001 -=+;;-xx-#701,---;;;,;;#001----#001-;. #701         *\n\r");
	pUser->Socket()->Write("  #101  xX#100X        #701  *                    #001 =-Xx.#701,-;;;;;;;#001;;,,----#001-;.  #701 *\n\r");
	pUser->Socket()->Write(" #101 x-,;;#100-=        #701        *          *  #001  ==-- #701x=-;;;;,#001;;;,;--=#001-;.-  #701 *     *\n\r");
	pUser->Socket()->Write("#101 +---#100-;,.=        #701*                       #001 -=xxx#701xX=-;-,#001,;,.#700.,-=-#001;.,  #701*\n\r");
	pUser->Socket()->Write(" #101+=-#100----=x       #701                            #001  x+-#701-;;,;.#001,#700.,;;,..-#001=;..    #701 *\n\r");
	pUser->Socket()->Write("  #101Xx#100+++x           #701*           *       *       #001   x=;,.#700;.,,;-;,.;-#001;..  \n\r");
	pUser->Socket()->Write("                  #701         #701[#601CF#701]                     #001 x=---..#700,;;.;.#001;,   #701*\n\r");
	pUser->Socket()->Write("              #701*                             *         #001 x=,,..#700.,;'#001;,  #701    *\n\r");
	pUser->Socket()->Write("   #701*                      *                             #001 =;...#700.;#001.. #701*\n\r");
	pUser->Socket()->Write("         #701                               *         *        #001 x-,,#001;.     #701 *\n\r");
	pUser->Socket()->Write("                #701*                                            #001 ###+;#001- \n\r");

	gsCopyright = m_pTextMgr->Search("mudcore_copyright");
	gsWelcome = m_pTextMgr->Search("mudcore_welcome");

	if (gsCopyright.Length() == 0 || gsWelcome.Length() == 0)
	{
		g_Log.Log(LOG_INFO, "[CLogonMnu::>>] Missing messages. Unable to continue");
		return 0;
	}

	pUser->Socket()->Write(gsCopyright);
	pUser->Socket()->Write(gsWelcome);

	pUser->SetSubMenu(_LOGON_MNU_GET_NAME);

	return 1;
}

bool CLogonMnu::VerifyUserName(CUser* pUser, const gString& gsArguments)
{
	UserMap::iterator pos;

	if ( gsArguments.IsEmpty() )
		return false;

	gString gsName(gsArguments);
	gsName.MakeProper();

	pUser->SetName( gsName );
	pUser->Socket()->Write("\n\r");

	return ( pUser->Load() );
}

bool CheckForReconnect(CUser* pUser)
{
	#pragma message(Reminder "[CLogonMnu::CheckForReconnect] Write this function.")
	return false;
}

int CLogonMnu::GetUserName(CUser* pUser)
{
	gString gsArgument = pUser->Socket()->GetInput();

	// check redirect
	// check closed
	// check copyover state
	// check newlock, stafflock, etc
	// check ban
	// check deny

	if ( !VerifyUserName(pUser, gsArgument) )
	{
		pUser->Socket()->Write("The name \"%s\" is unknown to me.\n\r"
			                   "Would you like to create a new account with that name?\n\r", 
							   (const char*)pUser->Name());

		return UNKNOWN_NAME;
	}
	
	if ( CheckForReconnect(pUser) )
	{
		// do not send to main menu, drop directly back into the game
		return 0;
	}

	pUser->Socket()->Write("#600Password#601:#700 ");

	return 0;
}

int CLogonMnu::GetNewName(CUser* pUser)
{
	gString gsArgument = pUser->Socket()->GetInput();

	switch ( tolower(gsArgument[0]) )
	{
		case 'y':
			pUser->Socket()->Write("Please enter a password for your new account.\n\r"
				                   "Passwords must be greater than 5 characters long.\n\r"
								   "Password: ");

			return USER_REQUESTED_NEW_ACCOUNT;
		case 'n':
   		default:
			pUser->Socket()->Write("Please enter your account name : ");
			return 0;
	}

	return 0;
}

int CLogonMnu::GetUserPassword(CUser* pUser)
{
	LOG_SCOPE("CLogonMnu::>>");
	int nRet = 0;
	pUser->Socket()->Write("\n\r");

	gString gsInput = pUser->Socket()->GetInput();

	char* szPass = MD5String(gsInput);

	if ( pUser->Password().Compare( szPass ) == 0 )
	{
		g_Log.Log(LOG_INFO, "User '%s' has logged in.", (const char*)pUser->Name());
		pUser->Socket()->Write("#600Welcome back, #601%s#600!\n\r\n\rPlease hit #600[#601Enter#600]#700 to continue.\n\r", pUser->Name());
		nRet = RECEIVED_VALID_PASSWORD;
	}
	else
	{
		//gString gsCount = pUser->m_TmpProperties.GetPropertyValue("password_attempts");
		long nCount = 0;

		//if ( gsCount != INVALID_PROPERTY_VALUE )
		//	nCount = lexical_cast<long>(gsCount);
	    //else

		nCount = LONG_MIN;


		if ( nCount == LONG_MIN ) // not found
		{
			// create new property
			long* nCount = new long(1);
			//pUser->m_TmpProperties.Register("password_attempts", nCount); 
		}
		else
		{
			//pUser->m_TmpProperties.SetProperty("password_attempts", ++nCount);

			if ( nCount > 2 )
			{
				pUser->Socket()->Write("\n\rInvalid Password. Disconnecting.\n\r");
				//pUser->m_TmpProperties.DeleteProperty("password_attempts");
				
				g_Log.Log(LOG_ERROR, "[CLogonMnu::>>] Someone at '%s' tried to login as %s with password: %s",
									(const char*)pUser->Socket()->Host(), (const char*)pUser->Name(), (const char*)gsInput);

				free(szPass);
				return FAILED_PASSWORD_ATTEMPTS;
			}
		}
		pUser->Socket()->Write("\n\rInvalid Password.\n\rPassword : ");
	}

	free(szPass);

	return nRet;
}

int CLogonMnu::GetNewPassword(CUser* pUser)
{
	pUser->Socket()->Write("\n\r");

	gString gsArgument = pUser->Socket()->GetInput();

	if ( gsArgument.Find("help") == 0 )
	{
		HandleHelpRequest(pUser, gsArgument);
		return 0;
	}

	// if they have already entered their password once, make sure they
	// entered the same password the second time.
	if ( pUser->Password().Length() > 1 )
	{
		int nResult = 0;

		char* szPass = MD5String(gsArgument);

		if ( pUser->Password().Compare( szPass ) == 0 ) // valid
		{
			pUser->Socket()->Write("Welcome, %s!\n\r\n\rPlease hit #600[#601Enter#600]#700 to continue.\n\r", pUser->Name());
			pUser->Save();
			nResult = RECEIVED_VALID_PASSWORD;
		}
		else
		{
			pUser->Socket()->Write("Passwords do not match.\n\r");
			pUser->Socket()->Write("Please enter a new password: ");
			pUser->SetPassword("");
		}
		return nResult;
	}
	else

	// No previous password set, validate their input
	if ( gsArgument.Length() < 6 )
	{
		pUser->Socket()->Write("Passwords must be greater than 5 characters long. Please try again.\n\r");
		return 0;
	}

	if ( pUser->SetPassword( gsArgument ) )
	{
		pUser->Socket()->Write("Please re-enter your password to make sure we have it right.\n\r");
		return ENTERED_NEW_PASSWORD;
	}

	return 0;
}

int CLogonMnu::Process(CUser* pUser)
{
	switch ( pUser->CurrentSubMenu() )
	{
		case _LOGON_MNU_GET_NAME:
			{
				if ( GetUserName(pUser) == UNKNOWN_NAME )
					pUser->SetSubMenu(_LOGON_MNU_VERIFY_NAME);
				else
					pUser->SetSubMenu(_LOGON_MNU_GET_PASSWORD);
			}
			break;

		case _LOGON_MNU_VERIFY_NAME:
			{
				if ( GetNewName(pUser) == USER_REQUESTED_NEW_ACCOUNT )
					pUser->SetSubMenu(_LOGON_MNU_GET_NEW_PASSWORD);
				else
					pUser->SetSubMenu(_LOGON_MNU_GET_NAME);
			}
			break;

		case _LOGON_MNU_GET_PASSWORD:
			{
				int nResult = GetUserPassword(pUser);

				if ( nResult == RECEIVED_VALID_PASSWORD )
					pUser->SetMenu(CUser::_MNU_MAIN_MENU);
				else
				if ( nResult == FAILED_PASSWORD_ATTEMPTS )
					pUser->Socket()->SetState(CSocket::_DISCONNECT);
				else
					pUser->SetSubMenu(_LOGON_MNU_GET_PASSWORD);
			}
			break;

		case _LOGON_MNU_GET_NEW_PASSWORD:
			{
				int nResult = GetNewPassword(pUser);

				if ( nResult == ENTERED_NEW_PASSWORD )
					pUser->SetSubMenu(_LOGON_MNU_GET_NEW_PASSWORD);
				else
				if ( nResult == RECEIVED_VALID_PASSWORD )
					pUser->SetMenu(CUser::_MNU_MAIN_MENU);
				else
					pUser->SetSubMenu(_LOGON_MNU_GET_NEW_PASSWORD);
			}
			break;

		default: // Show Main Menu
			DisplayMenu(pUser);
			break; 
	}

	return 1;
}
