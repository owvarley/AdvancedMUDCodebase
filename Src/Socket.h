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

#ifndef __SOCKET_H__
#define __SOCKET_H__

#include <memory.h>
#include <vector>
#include <winsock2.h>

#include "MudCore.h"


#include "telnet.h"

#include "Set.h"
#include "gString.h"
#include "Timer.h"
#include "zLib\zLib.h"

class CUser;

#define SOCKET_VERSION 1.0

// Telent defines missing from Winsock
#define	IAC		255		/* interpret as command: */
#define	GA		249		/* you may reverse the line */
#define	WILL	251		/* I will use option */
#define	DO		253		/* please, you use option */
#define	DONT	254		/* you are not to use option */
#define	SB		250		/* interpret as subnegotiation */
#define	SE		240		/* end sub negotiation */

#define OUTPUT_BUFFER_SIZE		1024*4
#define INPUT_BUFFER_SIZE		OUTPUT_BUFFER_SIZE/4
#define COMPRESS_BUFFER_SIZE	INPUT_BUFFER_SIZE
#define TELOPT_COMPRESS			86 // MCCP version 2 <only>

// The telnet option sequences below cause VC to complain unnecessarily.  Remove
// the appropriate warnings.
#pragma warning(disable: 4305)
#pragma warning(disable: 4309)

	const char go_ahead_str[]	 = { IAC, GA, '\0' };
	const char compress_will[]	 = { IAC, WILL,	TELOPT_COMPRESS, '\0' };
	const char compress_do[]	 = { IAC, DO,	TELOPT_COMPRESS, '\0' };
	const char compress_dont[]	 = { IAC, DONT,	TELOPT_COMPRESS, '\0' };
	const char compress_start[]  = { IAC, SB,	TELOPT_COMPRESS, WILL, SE, '\0' };

// Re-enable those warnings.
#pragma warning(default: 4305)
#pragma warning(default: 4309)


class CSocket
{
	friend class CGameServer;
	friend class CCommand;
	friend class MenuMgr;
	friend class CUser;

// Methods
public:
	CSocket();
	virtual ~CSocket();

	typedef enum e_State
	{
		_DISCONNECT		= -1,
		_ERROR			=  0,
		_CONNECTED		=  1,
		_IDLE			=  2,
	};

	typedef enum e_SocketAttributes
	{
		_DETERMINING_ATTRIBUTES = 1,
		_ANSI_CAPABLE,
		_NUM_SOCKET_ATTRIBUTES
	};

	inline	gString			Host()				{ return m_gsHost; }
	inline	int&			Descriptor()		{ return m_Descriptor; }
	inline	CSet*&			Flags()				{ return m_Flags; }
	inline	float			Timer()				{ return m_Timer.Elapsed(); }
	inline	e_State			State()				{ return m_eState; }
	inline	e_State			LastState()			{ return m_eLastState; }
	inline  gString			LastLine()			{ return (gString)m_InputLast; }
	inline  gString			GetInput()		    { return (gString)m_InputLine; }
	inline int&				LastErr()			{ return m_iLastErrState; }
	inline bool				IsANSICapable()		{ return m_Flags->IsSet(_ANSI_CAPABLE); }

	inline  void			SetSocket(int s)	{ m_Descriptor = s; }
	inline  void			SetHost(gString h)  { m_gsHost = h; }
	inline  void			SetUser(CUser* u)	{ m_pUser = u; }

	bool					Read();
	void					Close();
	int						Write(const char* fmt, ...);
	int						WriteCompressed(const char* fmt, ...);
	e_State					SetState(e_State eNewState);

protected:
	bool					Init();
	void					BustAPrompt();
	void					CompressStart();
	void					CompressEnd();
	gString					ProcessText(gString gsText);
	bool					ProcessCompressed();

	inline	char*			InputBuffer()		{ return m_InputBuffer; }
	inline	char*			OutputBuffer()		{ return m_OutputBuffer; }
	inline	char*			InputLine()			{ return m_InputLine; }
	inline	CUser*&			User()				{ return m_pUser; }
	inline	void			EraseInputLine()	{ memset(m_InputLine, 0, INPUT_BUFFER_SIZE); }

	void					ParseInputBuffer();
	bool					ParseOutputBuffer(bool fPrompt);


// Data
protected:
    unsigned char*			m_OutputCompressionBuffer;
    z_stream*				m_CompressionStream;

	char					m_InputBuffer[INPUT_BUFFER_SIZE];
	char					m_OutputBuffer[OUTPUT_BUFFER_SIZE];
	char					m_InputLine[INPUT_BUFFER_SIZE];
	char					m_InputLast[INPUT_BUFFER_SIZE];
	int						m_iRepeat;
	bool					m_bCommand;

	gString					m_gsHost;
	int						m_Descriptor;
	CUser*					m_pUser;
	CSet*					m_Flags;
	e_State					m_eState;
	e_State					m_eLastState;
	int						m_iLastErrState;
	CTimer					m_Timer;

};


typedef std::vector<CSocket*> SocketList;

#endif