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

// Class    :: CPlayer
// Header   :: Player.h
// Function :: The Player class holds those procedures and variables that make a Player different
//			:: than an Npc. On first glance at the header file for this class it looks a little light
//			:: but when you think about it, most of the functionality of Player is handled in Actor. 
//			:: This may change as your mud implementation grows apart from the MudCore base, but when
//			:: creating your Player class, keep in mind that only functionality that Npcs (of the same
//			:: race anyway) should not possess should go here.

#include <stdlib.h>
#include <fstream>
#include <time.h>

#include "MudCore.h"
#include "GameObjects.h"
#include "Player.h"
#include "Socket.h"

#include "Tools.h"
#include "md5.h"
#include "../gTools/Log.h"


CPlayer::CPlayer()
{
	m_iVnum				= (long)time(0);
	m_iTrust			= 0;
	m_LogonTime			= 0;
	m_nCreationPoints	= 0;
	m_gsPassword		= "";
	m_gsPrompt			= "#600Health#700:#601 100#600/#601100#600#700 ";
	m_pUser				= NULL;
	m_ActorFlags->SetBit(CActor::_PLAYER);
	m_ActorStates->SetBit(CActor::_UPDATES);
	m_Position.Set(1,0,0); // default position

	m_Template			= NULL;
	m_Frame				= NULL;
	m_HullCube			= NULL;
	m_Component			= NULL;
	m_Weapon			= NULL;

	// TEMP VARIABLES
	m_Location			= new CCart;
	m_Heading			= new CCart;
	m_Sector			= "";

	Register("GUID", (long*)&m_iVnum);
}

CPlayer::~CPlayer()
{
	if ( m_pUser )
		delete m_pUser;

	delete m_Location;
	delete m_Heading;

	m_Frame			= NULL;
	m_HullCube		= NULL;
	m_Component		= NULL;
	m_Template		= NULL;
	m_Weapon		= NULL;

	m_Sector		= "";
}

bool CPlayer::IsValidPlayer(CPlayer* pPlayer)
{
	if ( !pPlayer 
	||	 pPlayer->Name().Length() == 0
	||   pPlayer->GUID() == -1 )
	{
		return false;
	}

	return true;
}

void CPlayer::ReadXml(TiXmlNode* pParent)
{
	CMobile::ReadXml(pParent);
}

void CPlayer::WriteXml(TiXmlNode* pParent)
{
	CMobile::WriteXml(pParent);
}

std::ostream& operator << ( std::ostream& stream, const CPlayer& player )
{
	CTools& Tools = *CGameObjects::Get().Tools();

    stream << (const CActor&)(player);

	Tools.WriteLn(stream, "[Player]");

	if ( player.m_gsPassword.Length() > 0 )
		Tools.WriteLn(stream,	" Password          : \"%s\"",	player.m_gsPassword);

	if ( player.m_gsPrompt.Length() > 0 )
		Tools.WriteLn(stream,	" Prompt            : \"%s\"",	player.m_gsPrompt);

	Tools.WriteLn(stream,		" Trust             : %d",		player.m_iTrust);
	Tools.WriteLn(stream,		"[/Player]");

	return stream;
}


std::istream& operator >> ( std::istream& stream, CPlayer& player )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	bool bDone = false;

    stream >> (CActor&)(player);

	if (!stream.fail() && !stream.eof())

	try
	{
		if ( Tools.ReadKey(stream) == "[Player]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Player]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'P':
						if ( gsKey == "Password" )
							Tools.ReadData(stream, player.m_gsPassword);
						else
						if ( gsKey == "Prompt" )
							Tools.ReadData(stream, player.m_gsPrompt);
						break;
					case 'T':
						if ( gsKey == "Trust" )
							Tools.ReadData(stream, player.m_iTrust);
						break;
					default:
						g_Log.Log(LOG_ERROR, "[Player::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Player]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CPlayer::>>] Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CPlayer::>>] Error encountered while reading %s\'s file..", player.Name());
	}

	return stream;
}

int CPlayer::CurrentPointValue()
{
	// calculate this...
	return m_nCreationPoints;
}

bool CPlayer::SetUser(CUser* pUser)
{
	if ( m_pUser )
		return false;

	m_pUser = pUser;

	return true;
}

bool CPlayer::SetPassword(const gString& sNewPassword)
{
	if ( !sNewPassword.IsEmpty() )
	{
		m_gsPassword = MD5String(sNewPassword);
		return true;
	}

	return false;
}

bool CPlayer::SetPrompt(const gString& sNewPrompt)
{
	m_gsPrompt = sNewPrompt;
	return true;
}

bool CPlayer::SetTrust(int iTrust)
{
	if ( iTrust > m_iTrust || iTrust > m_iLevel )
		Write("You are being trusted to a higher level. Dont abuse it!.\n\r");

	m_iTrust = iTrust;

	return true;
}

int CPlayer::Write(char *fmt, ...)
{
	char buf[MSL];
	va_list args;
	int nWrite = 0;
	int length = 0;

	va_start (args, fmt);
	length = _vsnprintf(buf, MSL, fmt, args);
	va_end (args);

	if ( m_pUser->Socket() )
		return m_pUser ->Socket()->Write(buf);
	else
		return length;

	// if ( m_pWatchActor )
	//    (CPlayer*)m_pWatchActor->Write(buf);
}

void CPlayer::Think(bool bForce)
{
	CActor::Think(bForce);
}

void CPlayer::Update(bool bForce)
{
	float fThisUpdate = CGameObjects::Get().Clock();

	if ( EventCount() > 0 && !ActorStates()->IsSet(_IGNORE_EVENTS) )
		HandleEvents();

	if ( !ActorStates()->IsSet(CActor::_UPDATES) && !bForce )
		return;

	// Only update every 3 seconds or if forced
	if ( bForce || (fThisUpdate - m_fLastUpdate >= fPlayerUpdateDelta) )
	{
		//g_Log.Log(LOG_INFO, "[CPlayer::Update] Performing Player Update.\n");
		m_fLastUpdate = fThisUpdate;

		if ( rand()%3 == 0 )
		{//
			//ENotice* event = new ENotice;
    
			//event->gsNotice = "Hey, this is an example notice event!\n\r";
		//	ReceiveEvent(*event);
		}
	}
}

void CPlayer::DescribeTo(CActor* pA, bool bFull) const
{
	CActor::DescribeTo(pA, bFull);
}

bool CPlayer::Save()
{
	std::fstream fp;
	CGameObjects& globals = CGameObjects::Get();
	gString szDir;
	gString szFile;

	szDir.Format("%s\\%s_%d",
		(const char*)globals.m_Config.szDir[CGameObjects::_PLAYER],
		(const char*)User()->Name(), User()->GUID());

	if ( !gFileName::DirectoryExists(szDir) )
		gFileName::MakeDirectory(szDir);

	szFile.Format("%s\\%s_%d\\%s",
		(const char*)globals.m_Config.szDir[CGameObjects::_PLAYER],
		(const char*)User()->Name(), 
        User()->GUID(),
		(const char*)Name());

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Player");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

bool CPlayer::Load()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%s%s_%d\\%s",
		(const char*)CGameObjects::Get().m_Config.szDir[CGameObjects::_PLAYER],
		(const char*)User()->Name(), User()->GUID(),
		(const char*)Name());

	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)szFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Player");

	ReadXml(pNode);
	return true;
}

bool CPlayer::Exists(const gString& gsUserName, int nID, const gString& gsPlayer)
{
	std::fstream fp;
	gString szFile;

	szFile.Format("%s\\%s_%d\\%s",
		CGameObjects::Get().m_Config.szDir[CGameObjects::_PLAYER],
		gsUserName, nID,
		gsPlayer);

	fp.open(szFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	fp.close();
	return true;
}


////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Event Handling /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool CPlayer::HandleEvent(CEvent& Event)
{
	switch ( Event.m_EventCode )
	{
		case EV_NOTICE:
			return OnNotice(Event);
		default:
			return CActor::HandleEvent(Event);
	}

	return false;
}

