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

#ifndef __GAMESERVER_H__
#define __GAMESERVER_H__

#include <time.h>

#include "MudCore.h"
#include "Socket.h"


class CGameServer
{

// Members
public:

	typedef enum e_GameState
	{
		_ERROR			= 0,
		_LOADING		= 1,
		_RUNNING		= 2,
		_SHUTDOWN		= 3
	};

							CGameServer();
							~CGameServer();

	// Communications
	bool					InitSockets();
	void					InitDescriptor(int master);
	void					CheckConnections();
	void					ValidateConnections();
	void					ProcessInput();
	void					ProcessOutput();
	void					ProcessUpdates();
	void					ReportWinsockError(gString sCaller = "");
	void					Run();
	inline  e_GameState&	State()			{ return m_eState; }


// Data
private:

	int						m_Control;
	e_GameState				m_eState;

	fd_set					m_in_set;
	fd_set					m_out_set;
	fd_set					m_exc_set;
};

#endif
