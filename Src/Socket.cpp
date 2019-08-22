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


// Class    :: CSocket
// Header   :: Socket.h
// Function :: CSocket object is used to handle all the connections made to the MUDcore server.
//			   the object's constructor handles the state of the server. Further comments have been
//			   made throughout this file to help with the understanding of what is happening.


#pragma warning(disable: 4786)

#include <errno.h>

#include "GameObjects.h"
#include "GameServer.h"
#include "Player.h"
#include "Socket.h"
#include "Set.h"
#include "zLib\zLib.h"
#include "../gTools/Log.h"

extern int errno;

// CSocket Constructor
CSocket::CSocket()
{
	m_eState = _ERROR;
	m_eLastState = _ERROR;
	m_Descriptor = -1;
	m_bCommand = false;
	m_iRepeat = 0;
	m_pUser = NULL;
	m_CompressionStream = NULL;
	m_OutputCompressionBuffer = NULL;
	m_Flags = new CSet;

	memset(m_InputBuffer,	0, INPUT_BUFFER_SIZE);
	memset(m_InputLine,		0, INPUT_BUFFER_SIZE);
	memset(m_InputLast,		0, INPUT_BUFFER_SIZE);
	memset(m_OutputBuffer,	0, OUTPUT_BUFFER_SIZE);
}

// CSocket Deconstructor, used to free the object and
// shut the server down
CSocket::~CSocket()
{
	m_pUser = NULL;

	if ( m_OutputCompressionBuffer )
		free(m_OutputCompressionBuffer);

	m_OutputCompressionBuffer = NULL;
	m_CompressionStream = NULL;

	memset(m_InputBuffer,	0, INPUT_BUFFER_SIZE);
	memset(m_InputLine,		0, INPUT_BUFFER_SIZE);
	memset(m_InputLast,		0, INPUT_BUFFER_SIZE);
	memset(m_OutputBuffer,	0, OUTPUT_BUFFER_SIZE);

	delete m_Flags;
}


// Method     :: Init()
// Class	  :: CSocket
// Parameters :: <none>
// Return     :: Bool
// Function   :: Used to startup the Winsock server, returns true if successful
//			     or false if not
bool CSocket::Init()
{
	WSADATA	wsaData;
	WORD	wVersionRequested = MAKEWORD( 2, 0 );
	int		err = WSAStartup( wVersionRequested, &wsaData ); // Starts the server up
															 // err will contain the version type of WinSock

	WSACleanup();

	// Report the Winsock version
	if ( err == 0 )
	{
		g_Log.Log(LOG_INFO, "[CSocket::Init] Winsock version 2+ supported.");
		return true;
	}
	else
	{
		g_Log.Log(LOG_ERROR, "[CSocket::Init] Winsock version 2+ NOT supported!");
		return false;
	}
}

// Method     :: Close()
// Class	  :: CSocket
// Parameters :: <none>
// Return     :: <none>
// Function   :: Used to close a socket

void CSocket::Close()
{
	closesocket(Descriptor());
}

// Method     :: Read()
// Class	  :: CSocket
// Parameters :: <none>
// Return     :: boolean
// Function   :: Used to read and monitor a socket connection

bool CSocket::Read()
{
	int iStart;

	// Checks if the m_InputLine is empty
	if (m_InputLine[0] != '\0')
		return TRUE;

	// iStart :: Length of the Input Buffer
	iStart = strlen (m_InputBuffer);

	// Checks to see if the Length of the Input Buffer has exceeded
	// the size of it. This prevents players spamming input.
	if (iStart >= sizeof (m_InputBuffer) - 10)
	{
		g_Log.Log(LOG_WARNING, "[CSocket::Read] %s input overflow!", Host());
		Write("\n\r!!! Buffer Overflow !!! Knock it off!\n\r");
		return FALSE;
	}

	// Read loop
	for (;;)
	{
		int nRead;
		unsigned long nWaiting;

		// There is no more space in the input buffer for now
		if (sizeof (m_InputBuffer) - 10 - iStart == 0)
			break;

		ioctlsocket( Descriptor(), FIONREAD, &nWaiting );
		LastErr() = WSAGetLastError();
		// Error checking
		if ( LastErr() != 0 )
		{
			WSASetLastError(LastErr());
			CGameObjects::Get().GameServer()->ReportWinsockError("[CSocket::Read]");
		}

		// If waiting, break out of the loop
		if ( !nWaiting )
			break;

		
		nRead = recv( Descriptor(), m_InputBuffer + iStart,
			__min( nWaiting, sizeof( m_InputBuffer ) - 10 - iStart ),0 );

		LastErr() = WSAGetLastError();
		if ( LastErr() != 0 )
		{
			WSASetLastError(LastErr());
			CGameObjects::Get().GameServer()->ReportWinsockError("[CSocket::Read]");
		}

		// Something to 
		if (nRead > 0)
		{
			iStart += nRead;
			if (m_InputBuffer[iStart - 1] == '\n' || m_InputBuffer[iStart - 1] == '\r')
				break;
		}
		else
		{
			if (nRead == 0) // Lost Link
				return FALSE;
			else
			{
				int iError = WSAGetLastError(); // Dont use the LastErr() state for this check

				if (iError == WSAEWOULDBLOCK)
				  break;
				else
				{
					WSASetLastError(iError);
					CGameObjects::Get().GameServer()->ReportWinsockError("[CSocket::Read]");
					return FALSE;
				}
			}
		}
	}

	m_InputBuffer[iStart] = '\0';
	return TRUE;
}

void CSocket::CompressStart()
{
	z_stream *stream;

	if (m_CompressionStream != NULL) // already compressing, abort
		return;

	/* allocate and init stream, buffer */
	stream = (z_stream *) malloc(sizeof (*stream));
	m_OutputCompressionBuffer = (unsigned char *) malloc(COMPRESS_BUFFER_SIZE);

	stream->zalloc		= Z_NULL;
	stream->zfree		= Z_NULL;
	stream->next_in		= NULL;
	stream->avail_in	= 0;
	stream->next_out	= m_OutputCompressionBuffer;
	stream->avail_out	= COMPRESS_BUFFER_SIZE;
	stream->opaque		= NULL;

	if (deflateInit (stream, 9) != Z_OK)
	{
		/* problems with zlib, try to clean up */
		free(m_OutputCompressionBuffer);
		free(stream);
		m_OutputCompressionBuffer = NULL;
		stream = NULL;
		return;
	}

	Write("\n\rCompression enabled.\n\r");
	Write(compress_start);

	m_CompressionStream = stream;
	return;
}

void CSocket::CompressEnd()
{
	unsigned char dummy[1];

	if (!m_CompressionStream)
		return;

	m_CompressionStream->avail_in = 0;
	m_CompressionStream->next_in = dummy;

	// No terminating signature is needed - receiver will get Z_STREAM_END
	if (deflate (m_CompressionStream, Z_FINISH) != Z_STREAM_END)
		return;

	if (!ProcessCompressed()) // Try to transmit any residual data
		return;

	deflateEnd (m_CompressionStream);

	free(m_OutputCompressionBuffer);
	free(m_CompressionStream);

	m_OutputCompressionBuffer = NULL;
	m_CompressionStream = NULL;

	return;
}

bool CSocket::ProcessCompressed()
{
	int iStart, nBlock, nWrite, len;

	if (!m_CompressionStream)
		return true;

	// Try to write out some data..
	len = m_CompressionStream->next_out - m_OutputCompressionBuffer;
	if (len > 0)
	{
		// we have some data to write
		for (iStart = 0; iStart < len; iStart += nWrite)
		{
			nBlock = __min(len - iStart, MSL);

			nWrite = send(Descriptor(), (const char*)m_OutputCompressionBuffer + iStart, nBlock, 0);

			if (nWrite < 0)
			{
				if (errno == EAGAIN)
					break;

				return false;	// write error
			}

			if (nWrite <= 0)
				break;
		}

		if (iStart > 0) // we wrote 'iStart' bytes
		{
			if (iStart < len)
				memmove (m_OutputCompressionBuffer,
						 m_OutputCompressionBuffer + iStart,
						len - iStart);

			m_CompressionStream->next_out = m_OutputCompressionBuffer + len - iStart;
		}
	}

	return true;
}

int CSocket::WriteCompressed(const char* fmt, ...)
{
	char buf[MSL];
	gString gsText;
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	z_stream *s = m_CompressionStream;

	gsText = ProcessText(buf);
	memcpy(buf, gsText, gsText.Length());

	s->next_in = (unsigned char *) buf;
	s->avail_in = length;

	while (s->avail_in)
	{
		s->avail_out = COMPRESS_BUFFER_SIZE - (s->next_out - m_OutputCompressionBuffer);

		if (s->avail_out)
		{
			int status = deflate (s, Z_SYNC_FLUSH);

			if (status != Z_OK)
			{
				/* Boom */
				return 0;
			}
		}

		/* Try to write out some data.. */
		if (!ProcessCompressed())
			return 0;
	}

	return length;
}

int CSocket::Write(const char* fmt, ...)
{
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;
	gString gsText;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	if ( !m_pUser )
		return length;

	if ( m_CompressionStream )
		return WriteCompressed("%s", buf);

	// total up the buffer size.
	gsText = ProcessText(buf);
	nWrite = send(Descriptor(), gsText, gsText.GetLength(), 0);

	LastErr() = WSAGetLastError();
	if ( LastErr() != 0 )
	{
		WSASetLastError(LastErr());
		CGameObjects::Get().GameServer()->ReportWinsockError("[CSocket::Write]");
	}

	return nWrite;
}

 CSocket::e_State CSocket::SetState(e_State eNewState)
{
	m_eLastState = m_eState;
	m_eState = eNewState;
	return m_eState;
}

//
// Take the data in the input buffer, and move it to the input line, one line at a time.
//
void CSocket::ParseInputBuffer()
{
	int i, j, k;
	bool got_n, got_r;

	if (m_InputLine[0] != '\0')
		return;

	// Look for a new line
	for (i = 0; m_InputBuffer[i] != '\n' && m_InputBuffer[i] != '\r'; i++)
	{
		if (m_InputBuffer[i] == '\0')
		{
			// Handle ANSI negotiation response.
			// Edit by Nek :: Didn't seem to be working properly. Will look into
			if (!m_Flags->IsSet(_ANSI_CAPABLE)) // && i >= strlen(RESPONSE_CODE)) //&& strstr(m_InputBuffer, RESPONSE_CODE) != NULL)
			{
				m_Flags->SetBit(_ANSI_CAPABLE);
				Write(ANSI_NOTICE);
				memset(m_InputBuffer, 0, INPUT_BUFFER_SIZE);
			}

			if ( m_Flags->IsSet(_DETERMINING_ATTRIBUTES) )
				m_InputLine[0] = '.';

			return;
		}
	}

    // Canonical input processing.
	for (i = 0, k = 0; m_InputBuffer[i] != '\n' && m_InputBuffer[i] != '\r'; i++)
	{
		if (k >= 512 )
		{
			Write("Line too long.\n\r");

			// skip the rest of the line
			for (; m_InputBuffer[i] != '\0'; i++)
			{
				if (m_InputBuffer[i] == '\n' || m_InputBuffer[i] == '\r')
					break;
			}
			m_InputBuffer[i] = '\n';
			m_InputBuffer[i + 1] = '\0';
			break;
		}

		if (m_InputBuffer[i] == '\b' && k > 0)
			--k;
		else
		if ( __isascii (m_InputBuffer[i]) && isprint (m_InputBuffer[i]))
			m_InputLine[k++]	= m_InputBuffer[i];
		else
		// Handle telnet option negotiation here...
		if (m_InputBuffer[i] == (signed char) IAC)
		{
			// Received a "compress_do" in response to our earlier compress_will
			// message.  Begin compression streams.
			if (!memcmp (&m_InputBuffer[i], compress_do, strlen (compress_do)))
			{
				i += strlen (compress_do) - 1;
				CompressStart();
			}
			// Received a compress_dont. We may allready be compressing, so be sure
			// we handle this correctly.
			else if (!memcmp (&m_InputBuffer[i], compress_dont, strlen (compress_dont)))
			{
				i += strlen (compress_dont) - 1;
				CompressEnd();
			}
		}
	}

    // Finish off the line.
	if (k == 0)
		m_InputLine[k++] = ' ';

	m_InputLine[k] = '\0';

    // Handle #repeat attacks
	if (k > 1 || m_InputLine[0] == '!')
	{
		if ( (m_InputLine[0] != '!' && strcmp (m_InputLine, m_InputLast)) )
		{
			m_iRepeat = 0;
		}
		else
		{
			if (++m_iRepeat >= 25 && State() == _CONNECTED )
			{
				g_Log.Log(LOG_INFO, "[CSocket::ParseInputBuffer] Input Spammer %s", Host() );
				m_iRepeat = 0;
				// consider disconnecting player.
				// State() = _DISCONNECT;
			}
		}
	}


    // Handle '!' line substitution.
	if (m_InputLine[0] == '!')
		strcpy (m_InputLine, m_InputLast);
	else
		strcpy (m_InputLast, m_InputLine);

	// Shift the input buffer.
	got_n = got_r = false;

	for (; m_InputBuffer[i] == '\r' || m_InputBuffer[i] == '\n'; i++)
	{
		if ( (m_InputBuffer[i] == '\r' && got_r++) ||
			 (m_InputBuffer[i] == '\n' && got_n++) )
			break;
	}

	for (j = 0; (m_InputBuffer[j] = m_InputBuffer[i + j]) != '\0'; j++)
		;

	return;
}

void CSocket::BustAPrompt()
{
	CPlayer* pP = m_pUser->Player();

	if ( pP )
		Write( pP->Prompt() );
}

bool CSocket::ParseOutputBuffer(bool fPrompt)
{
	if ( m_OutputBuffer[0] == '\0' )
		return true;

	// Someone remind me to implement this once we HAVE player configs... ;)
	//	if (IS_FLAG (player config, _TELNET_GA))
	//		write_to_buffer (d, go_ahead_str, 0);

	if (fPrompt && State() == _CONNECTED)
	{
		BustAPrompt();
	}

	return (Write(m_OutputBuffer) > 0);
}

//
// Supported Text Substitutions are:
//
//
// #000 Where '000' is a 3 digit numeric code to represent an ANSI color. The first digit
//     is required, and represents the foreground color you wish to use. The second digit
//     (if present) represents the background color, and the last digit (if present)
//     represents the ANSI attribute.
//
//    Colors
//     0 BLACK, 1 RED, 2 GREEN, 3 YELLOW, 4 BLUE, 5 MAGENTA, 6 CYAN, 7 WHITE
//    Attributes
//     0 NORMAL, 1 BOLD, 2 DIM, 4 UNDERSCORE, 5 BLINK, 7 REVERSE, 8 HIDDEN
//
// Note: Underscore may only work on monochrome systems.
//
// #LL The sockets last input line
// #NL A Newline character \n
// #TB A Tab character \t <- By Nek
// #CR Carriage Return character \r
// ### A '#' character
//

gString CSocket::ProcessText(gString gsText)
{
	char buffer[MSL];
	char code[4];
	int nCount=0, nLength = gsText.Length(), nPos=0;

	if ( nLength < 1 )
		return gString("");

	if ( nLength > MSL-1 )
		return gsText;

	memset(buffer, 0, MSL);

	while (nCount < nLength)
	{
		if ( gsText[nCount] == '#' ) // begin processing
		{
			bool bDone = false;
			int nChar = 0;
			int nDigitCount = 0;	// Used to count the number
									// of digits we have found

			memset(code, 0, 4);

			while (!bDone)
			{
				if ( nCount < nLength )
				{
					code[nChar++] = gsText[++nCount];

					if ( isdigit(code[0]) )
					{
						nDigitCount++;	// Increase our digit count
						
						// MCB :: 1.5 >> 15/3/2006 - OWV
						// Modified this code to fix a bug with output. If your enter a colour code
						// followed by a number then it thinks that the number is part of the colour
						// code still which results in very strange output. Updated to fix this by
						// adding a Digit counter, will finish if the next char is not a digit or if
						// the Digit counter exceeds 3

						// peek ahead one
						if ( !isdigit(gsText[nCount+1]) || nDigitCount >= 3 )
							bDone = true;
					}
					else
					if ( nChar == 2 )
						bDone = true;
				}
			}
			nCount++;

			if ( !isdigit(code[0]) )
			{
				// Do macro replacement
				if ( strcmp(code,"LL")==0 && m_InputLast[0] != '\0' )
				{
					strcat(buffer, m_InputLast);
					nPos = strlen(buffer);
				}
				else
				if ( strcmp(code, "NL")==0 )
				{
					buffer[nPos++] = '\n';
				}
				else
				if ( strcmp(code, "TB")==0 )
				{
					buffer[nPos++] = '\t';
				}
				else
				if ( strcmp(code, "CR")==0 )
				{
					buffer[nPos++] = '\r';
				}
				else
				if ( strcmp(code, "##")==0 )
				{
					buffer[nPos++] = '#';
				}
				else
					buffer[nPos++] = gsText[nCount++];
			}
			else
			if ( IsANSICapable() )
			{
				// Do ANSI replacement
				int nFG=37, nBG=0, nAttr=0;
				char szColor[256];

				nFG = 30 + (9-(57-code[0]));

				if ( isdigit(code[1]) )
					nBG = 40 + (9-(57-code[1]));

				if ( isdigit(code[2]) )
					nAttr = 9-(57-code[2]);

				sprintf(szColor,"\x01B[%d;%d;%dm", nAttr, nFG, nBG);

				strcat(buffer, szColor);
				nPos = strlen(buffer);
			}
		}
		else
			buffer[nPos++] = gsText[nCount++];

	}

	return gString(buffer);
}
