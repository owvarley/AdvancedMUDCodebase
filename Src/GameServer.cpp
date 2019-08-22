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

// Class    :: CGameServer
// Header   :: GameServer.h
// Function :: GameServer does two things...
//				1) It contains our list of managed worlds and handles their update
//				   cycles. but primarily...
//				2) It manages the state of our network.  It manages all sockets
//				   and sets up initial connection states.



#pragma warning(disable: 4786)

#include <map>
#include <time.h>
#include <io.h>

#include "MudCore.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Player.h"
#include "Socket.h"
#include "Tools.h"
#include "../gTools/Log.h"
#include "Timer.h"

CGameServer::CGameServer()
{
	m_eState = _LOADING;
}


CGameServer::~CGameServer()
{
}



void CGameServer::ReportWinsockError(gString sCaller)
{
	int iError = WSAGetLastError();
	gString sErrMsg;

	if ( sCaller.IsEmpty() )
		sCaller = "[CGameServer::ReportWinsockError]";

	if ( iError != 0 )
	{
		switch ( iError )
		{
			case WSAEPROCLIM:		sErrMsg.Format("%s Limit on the number of tasks supported by WinSock has been reached.", sCaller); break;
			case WSANOTINITIALISED: sErrMsg.Format("%s Winsock Layer has not been initialized.", sCaller); break;
			case WSAENETDOWN:		sErrMsg.Format("%s The network is down.", sCaller); break;
			case WSAEACCES :		sErrMsg.Format("%s Socket access denied.", sCaller); break;
			case WSAEADDRINUSE :	sErrMsg.Format("%s Socket address allready in use.", sCaller); break;
			case WSAEADDRNOTAVAIL : sErrMsg.Format("%s The specified address is invalid for this machine.", sCaller); break;
			case WSAEFAULT:			sErrMsg.Format("%s Invalid address pointer, or address data size.", sCaller); break;
			case WSAEMFILE:			sErrMsg.Format("%s No Socket Descriptors available. Limit Exceeded?", sCaller); break;
			case WSAENOBUFS:		sErrMsg.Format("%s No buffer space is available. The socket cannot be created.", sCaller); break;
			case WSAEPROTOTYPE:		sErrMsg.Format("%s The specified protocol is the wrong type for this socket.", sCaller); break;
			case WSAEINPROGRESS:	sErrMsg.Format("%s A blocking WinSock v1.1 call is in progress.", sCaller); break;
			case WSAEINVAL:			sErrMsg.Format("%s The socket is already bound to an address.", sCaller); break;
			case WSAENOTSOCK:		sErrMsg.Format("%s The descriptor is not a socket.", sCaller);
			case WSAEOPNOTSUPP:		sErrMsg.Format("%s The referenced socket is not a type that supports connection-oriented service.", sCaller);
			case WSAEWOULDBLOCK:	sErrMsg.Format("%s The socket is marked as nonblocking and no connections are present to be accepted.", sCaller);
			case WSAENOTCONN:		sErrMsg.Format("%s The socket is not connected.", sCaller); break;
			default: sErrMsg.Format("%s Socket Error.", sCaller); break;
		}
		g_Log.Log(LOG_ERROR, sErrMsg);
	}
}

bool CGameServer::InitSockets()
{
	LOG_SCOPE("CGameServer::InitSockets");

	CGameObjects&				globals = CGameObjects::Get();
	struct linger				ld;
    static struct sockaddr_in	sa_zero;
	struct sockaddr_in			sa;
	int							x  = 1;
	int							fd = -1;
	int							port = PORT;

	WSADATA						wsaData;
	WORD						wVersionRequested = MAKEWORD( 2, 2 );
	int							err = -1;

	g_Log.Log(LOG_INFO, "Initializing Socket Layer\n");

	err = WSAStartup( wVersionRequested, &wsaData );

	m_eState = _SHUTDOWN;

	// You cant call WSAGetLastError if WSAStartup failed. The data space has not
	// been initialized, so results of doing so are undefined.
	if ( err != 0 )
	{
		switch ( err )
		{
			case WSAVERNOTSUPPORTED:g_Log.Log(LOG_ERROR, "The version of Winsock requested is not supported."); break;
			case WSAEINPROGRESS:	g_Log.Log(LOG_ERROR, "A blocking WinSock v1.1 call is in progress."); break;
			case WSAEPROCLIM:		g_Log.Log(LOG_ERROR, "Limit on the number of tasks supported by WinSock has been reached."); break;
			case WSAEFAULT:			g_Log.Log(LOG_ERROR, "Invalid address pointer, or address data size."); break;
			case WSASYSNOTREADY:
			default:				g_Log.Log(LOG_ERROR, "The network subsystem is not ready for communications."); break;
		}
		return false;
	}

	if ( ( fd = socket( PF_INET, SOCK_STREAM, 0 ) ) < 0 )
	{
		ReportWinsockError("[CGameServer::InitSockets<socket>]");
		return false;
	}

	if ( setsockopt( fd, SOL_SOCKET, SO_REUSEADDR, (char *) &x, sizeof( x ) ) < 0 )
    {
		ReportWinsockError("[CGameServer::InitSockets<setsockopt>]");
		closesocket(fd);
		return false;
    }

	ld.l_onoff  = 1;
	ld.l_linger = 1000;

	if ( setsockopt( fd, SOL_SOCKET, SO_DONTLINGER, (char *) &ld, sizeof( ld ) ) < 0 )
	{
		ReportWinsockError("[CGameServer::InitSockets<setsockopt>]");
		closesocket(fd);
		return false;
	}

    sa				= sa_zero;
    sa.sin_family   = PF_INET;
    sa.sin_port	    = htons( port );

    if ( bind( fd, (struct sockaddr *) &sa, sizeof( sa ) ) < 0 )
    {
		ReportWinsockError("[CGameServer::InitSockets<bind>]");
		closesocket(fd);
		return false;
    }

    if ( listen( fd, 3 ) < 0 )
    {
		ReportWinsockError("[CGameServer::InitSockets<listen>]");
		closesocket(fd);
		return false;
    }

	m_Control = fd;
	m_eState = _RUNNING;
	g_Log.Log(LOG_INFO, "Sockets Initialized\n");

    return true;
}

void CGameServer::InitDescriptor(int master)
{
	LOG_SCOPE("CGameServer::InitDescriptor");

	CGameObjects& globals = CGameObjects::Get();
	static unsigned long ARGP = 1;
	char buf[512];
	CUser* pUser;
	CSocket* pC;
	struct sockaddr_in sock;
	struct hostent *from;
	int desc;
	int size;

	size = sizeof (sock);
	getsockname (master, (struct sockaddr *) &sock, &size);

	if ((desc = accept (master, (struct sockaddr *) &sock, &size)) < 0)
	{
		ReportWinsockError("[CGameServer::InitDescriptor<accept>]");
		return;
	}

	if ( ioctlsocket( desc, FIONBIO, &ARGP ) != 0 )
    {
		ReportWinsockError("[CGameServer::InitDescriptor<ioctlsocket>]");
		return;
    }

	pUser = new CUser();

	pC = new CSocket();
	pC->SetSocket( desc );

	size = sizeof (sock);
	if (getpeername (desc, (struct sockaddr *) &sock, &size) < 0)
	{
		pC->LastErr() = WSAGetLastError();
		WSASetLastError(pC->LastErr());
		ReportWinsockError("[CGameServer::InitDescriptor<getpeername>]");
		pC->SetHost("(unknown)");
	}
	else
	{
		from = gethostbyaddr ((char *) &sock.sin_addr, sizeof (sock.sin_addr), AF_INET);
		sprintf(buf, "%s", inet_ntoa(sock.sin_addr));

		g_Log.Log(LOG_INFO, "New Connection: %s\n", from ? from->h_name : buf);
		pC->SetHost(from ? from->h_name : buf);
	}

	// For now, the users vnum is the tick count of the time he was created.
	CGameObjects::Get().Users().insert( UserMap::value_type(pUser->GUID(), pUser));

    // Send the greeting.

	pC->SetUser(pUser);
	pUser->SetSocket(pC);
	pC->Flags()->SetBit(CSocket::_DETERMINING_ATTRIBUTES);

	// EDIT: Removed to prevent strange occurences - Nekekami [29/5/05]
//	pC->Write(QUERRY_CODE);
	ProcessInput();
	pC->Write("#605Welcome to %s v%s#700\n\r", APPNAME, globals.Tools()->VersionToString(VERSION));
	pC->SetState( CSocket::_CONNECTED );

	pUser->SetMenu(CUser::_MNU_LOGON_MENU);
}

void CGameServer::CheckConnections()
{
	LOG_SCOPE("CGameServer::CheckConnections");
	static struct timeval null_time = {0,0};

	UserMap::iterator pos;
	CSocket* Socket;
	CUser* pUser;
	int maxDesc = 0;

	// Poll all active descriptors.
	FD_ZERO (&m_in_set);
	FD_ZERO (&m_out_set);
	FD_ZERO (&m_exc_set);
	FD_SET  (m_Control, &m_in_set);
	maxDesc = m_Control;

	for (pos = CGameObjects::Get().Users().begin(); pos != CGameObjects::Get().Users().end(); pos++)
	{
		try
		{
			pUser = (CUser*)((*pos).second);
			Socket = pUser ? pUser->Socket() : NULL;

			if ( !Socket )
				continue;

			FD_SET (Socket->Descriptor(), &m_in_set);
			FD_SET (Socket->Descriptor(), &m_out_set);
			FD_SET (Socket->Descriptor(), &m_exc_set);
			maxDesc = __max(maxDesc, Socket->Descriptor());
		}
		catch(...) {}
	}

	if (select (maxDesc + 1, &m_in_set, &m_out_set, &m_exc_set, &null_time) < 0)
	{
		g_Log.Log(LOG_ERROR, "select: poll");
		m_eState = _ERROR;
	}

	//New connection?
	if (FD_ISSET (m_Control, &m_in_set))
		InitDescriptor (m_Control);

}

void CGameServer::ValidateConnections()
{
	CSocket* Socket;
	CUser* pUser;
	UserMap::iterator  pos;

	for (pos = CGameObjects::Get().Users().begin(); pos != CGameObjects::Get().Users().end(); pos++)
	{
		try
		{
			pUser = (CUser*)((*pos).second);
			Socket = pUser ? pUser->Socket() : NULL;

			if ( !Socket )
				continue; // kick the player?

			if (FD_ISSET (Socket->Descriptor(), &m_exc_set))
			{
				pUser = (CUser*)((*pos).second);

				FD_CLR (Socket->Descriptor(), &m_in_set);
				FD_CLR (Socket->Descriptor(), &m_out_set);
				CGameObjects::Get().Users().erase(pos);
				delete pUser;
			}
		}
		catch (...) {break;}
	}
}


void CGameServer::ProcessInput()
{
	UserMap::iterator pos;
	CUser* pUser;
	std::vector<UserMap::iterator> DropQue;

	for ( pos = CGameObjects::Get().Users().begin(); pos != CGameObjects::Get().Users().end(); pos++ )
	{
	//	try
		{
			pUser = (CUser*)((*pos).second);

			if ( !pUser || !pUser->Socket() )
				continue;

  			pUser->Socket()->m_bCommand = false;

			if (FD_ISSET (pUser->Socket()->Descriptor(), &m_in_set))
			{
				if ( !pUser->Socket()->Read() )
				{
					FD_CLR (pUser->Socket()->Descriptor(), &m_out_set);
					// save player
					pUser->Save();
					pUser->Socket()->Close();
					continue;
				}
			}

			if ( pUser->Socket()->Flags()->IsSet(CSocket::_DETERMINING_ATTRIBUTES) )
			{
				Sleep(300);
				pUser->Socket()->Read();
				pUser->Socket()->ParseInputBuffer();
				pUser->Socket()->Flags()->RemoveBit(CSocket::_DETERMINING_ATTRIBUTES);
				continue;
			}

			pUser->Socket()->ParseInputBuffer();

			if ( pUser->Socket()->InputLine()[0] != '\0' 
			||   pUser->Socket()->State() < CSocket::_CONNECTED 
			||   pUser->CurrentSubMenu() == _MNU_SWITCHING)
			{
				pUser->Socket()->m_bCommand = true;

				if ( pUser->Socket()->State() == CSocket::_IDLE )
					pUser->Socket()->SetState(CSocket::_CONNECTED);

				// Ok, we have input in the command buffer, time to deal with it...
				switch ( pUser->Socket()->State() )
				{
					case (CSocket::_CONNECTED):
						{
							if ( pUser->State() < CUser::_USR_PLAYING )
							{
								if ( pUser->Socket()->InputLine()[0] != '\0' 
								||   pUser->CurrentSubMenu() == _MNU_SWITCHING)
								{
									if ( pUser->CurrentMenu() > _MNU_NONE )
										CGameObjects::Get().MenuManager().Process(pUser);

									pUser->Socket()->EraseInputLine();
									pUser->Socket()->ParseInputBuffer();
								}
							}
							else
							if ( pUser->Player()->HomeWorld() )
							{
								gString gsInput = pUser->Socket()->InputLine();
								pUser->Player()->HomeWorld()->Interpret(pUser->Player(), gsInput);
							}

						}
						break;
					case (CSocket::_DISCONNECT):
						{
							DropQue.push_back( pos );
							continue;
						}
						break;
					case (CSocket::_ERROR):
						break;
					case (CSocket::_IDLE):
						break;
					default:
						while ( pUser->Socket()->InputLine()[0] != '\0' )
						{
							if ( pUser->CurrentMenu() > _MNU_NONE )
								CGameObjects::Get().MenuManager().Process(pUser);

							pUser->Socket()->EraseInputLine();
							pUser->Socket()->ParseInputBuffer();
						}
						break;
				}

				if ( pUser ) // still valid?
					pUser->Socket()->EraseInputLine();
			}
		}
	//	catch (...) {}
	}

	while ( DropQue.size() > 0 )
	{
		UserMap::iterator itor = DropQue.back();

		pUser = (CUser*)((*itor).second);

		DropQue.pop_back();

		if ( pUser )
		{
			if ( pUser->Socket() )
			{
				pUser->Socket()->Write("You have been disconnected.");
				pUser->Socket()->Close();
			}
			delete pUser;
			CGameObjects::Get().Users().erase( itor );
		}
		else
			g_Log.Log(LOG_ERROR, "[CGameServer::ProcessInput] Player not found in server list. Unable to remove.");
	}

}

void CGameServer::ProcessOutput()
{
	UserMap::iterator pos;
	CUser* pUser;
	CSocket* pS;

	for ( pos = CGameObjects::Get().Users().begin(); pos != CGameObjects::Get().Users().end(); pos++ )
	{
		try
		{
			pUser = (CUser*)((*pos).second);
			pS = pUser ? pUser->Socket() : NULL;

			if ( !pS )
				continue;

			if ( pS->m_bCommand && FD_ISSET (pS->Descriptor(), &m_out_set))
			{
				if (!pS->ParseOutputBuffer(true))
				{
					if ( pS->User() && pS->State() == (CSocket::_CONNECTED) )
					{
						pS->User()->Save();
					}

					pS->Close();
				}
			}
		}
		catch (...) {break;}
	}
}

void CGameServer::ProcessUpdates()
{
	CGameObjects::Get().Pulse();
}


//
// CPS_CAP is a "Cycles Per Second CAP" that we put on the server. In other words,
// we dont allow it to run more than this many cycles per second.  Why on earth would we
// want to restrict the CPS of the server? Well.... you only have so many CPU cycles. The
// lower you set this number to, the greater the number of CPU cycles you'll have available
// to other software.
//
// MudCore was developed on a 1000mhz Athlon with 512mg RAM.  A CPS_CAP of 1000 kept my
// CPU usage to under 2% on the development machine. (in debug mode, running VC++, etc)
// If you have a slower system, you may need to lower this number.  Note that the higher
// this number, the more lag you introduce between a player hitting 'enter', and the mud
// responding.
//
#define CPS_CAP 250

void CGameServer::Run()
{
	LOG_SCOPE("CGameServer::Run");

	CGameObjects& globals = CGameObjects::Get();
	static CProfile* pProfile = globals.m_ProfileMgr.Find("game_server");
	int nSleepTime = CPS_CAP;

	while ( true )
	{

		Sleep( nSleepTime );

		pProfile->Start();

		switch ( m_eState )
		{
			case _LOADING:
				{
					if ( !InitSockets() || !globals.LoadGameWorlds() )
					{
						if (m_eState != _SHUTDOWN)
							m_eState = _ERROR;

						break;
					}

					g_Log.Log(LOG_INFO, "Accepting incoming connections on port # %d\n", PORT);
				}
				break;
			case _RUNNING:
				AppCallback();
				CheckConnections();
				ValidateConnections();
				ProcessInput();
				ProcessUpdates();
				ProcessOutput();
				break;
			case _SHUTDOWN:
				g_Log.Log(LOG_INFO, "Shutting down...\n");
				globals.ShutdownWorlds();
				closesocket(m_Control);
				WSACleanup();
				break;
		}

		pProfile->Stop();

		if ( m_eState == _SHUTDOWN || m_eState == _ERROR )
			break;
	}
}

