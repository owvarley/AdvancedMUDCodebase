//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.
//
// The Advanced MUD Codebase Project
// AMC, copyright (c) 2005, 2006 by Owen Varley <owen@sw-erp.org>

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: 
// Header   :: MudCore.h
// Function :: Implements the main function for the project, provides crash handling and
//			:: debug/testing functions.


#include "MudCore.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "Tools.h"
#include "../gTools/Log.h"

#ifdef _WINDOWS
	#include <windows.h>
#endif

gLog<LogMethod> g_Log;
 
         
void TRACE(char* fmt, ...)
{
#ifdef _DEBUG
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

#ifdef _WINDOWS
	OutputDebugString(buf);
#else
	printf(buf);
#endif

#endif
}


//
// This function was put in place primarily for easy debuging and testing.
// remove it as you like.
//
void AppCallback(void)
{
#if (HAVE_PYTHON)
	static int nCount = 0;

	if ( ++nCount == 1 )
	{
		gFileName gfnScript = "PythonExample.py";

		CGameObjects::Get().ScriptHandler()->ExecuteScript(gfnScript);
	}
#endif
}

int main(int argc, char* argv[])
{

	try 
	{
		g_Log.SetPolicy(new HTMLLogPolicy);
		g_Log.Init("amc_log.htm");

		// send output to stdout
		CTools::Get().OutputReport(true);

		// Initialize our game object
		CGameObjects& Globals = CGameObjects::Get();

		// Steps
		// 1) Initialize any global settings
		// 2) Initialize the socket layer
		// 3) Load any area files
		// 4) Enter gamestate

		Globals.GameServer()->Run();
		Globals.Shutdown();

		return 0;
	}
	catch (...)
	{
		CRandom* pRand = CGameObjects::Get().Rand();
		gString gsQuote;
		switch (pRand->NumberRange(1, 7))
		{
		case 1: gsQuote = "#101Doh!#100 Looks like Owen tried to free an array again!\n\r";
			break;
		case 2: gsQuote = "#101Uh oh...#100 Looks like Owens been coding while under the influence again...\n\r";
			break;
		case 3: gsQuote = "#101Ops...#100 Looks like Owen was coding past his coding bedtime.\n\r";
			break;
		case 4: gsQuote = "#101Whoops!#100 Looks like Owen's been outsourcing his programming to Estonia.\n\r";
			break;
		case 5: gsQuote = "#101Argh!#100 Looks like Owen should have stuck to flying planes.\n\r";
			break;
		case 6: gsQuote = "#101Interesting...#100 Looks like Owen should have listened to Mark a little better.\n\r";
			break;
		case 7: gsQuote = "#101Awoooga!#100 Looks like Owen should have paid attention in programming class!\n\r";
			break;
		}

		int nCrashes = CGameObjects::Get().ConfigData().GetInt("mud_crashes", "Options");
		CGameObjects::Get().GameWorld()->Write("%s#100[#101MUD CRASH ### %d #100]#700 Auto-reboot initialised.\n\r", gsQuote, nCrashes);
		CGameObjects::Get().ConfigData().SetInt("mud_crashes", (nCrashes+1), "", "Options", 0);
		time_t tm = time(0);
		gString gsTime = ctime(&tm);
		gsTime.DeleteChar( gsTime.Length()-1 ); // damn ctime trailing \n
		CGameObjects::Get().ConfigData().SetValue("last_crash", gsTime, "", "Options", 0);

		CGameObjects::Get().ConfigData().SetFileName("config.dat");
		CGameObjects::Get().ConfigData().Save();
		CGameObjects::Get().Shutdown();
		return 0;
	}
}


