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


#include <time.h>
#include <vector>
#include <fstream>

#include "Mudcore.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "Account.h"
#include "Player.h"
#include "Socket.h"
#include "User.h"
#include "Menu.h"
#include "Tools.h"
#include "Set.h"
#include "md5.h"
#include "../gTools/Log.h"

CUser::CUser()
{
	m_gsName = "";
	m_gsEmail = "";
	m_gsPassword = "";
	m_CreationDate = time(0);
	m_iUniqueID = (long) m_CreationDate;
	m_LastAccessDate = 0;
	m_TotalTimeOnline = 0;
	m_LastTimeOnline = 0;
	m_pStatus = new CSet;
	m_pFlags = new CSet;
	m_pAccount = new CAccount;
	m_pPlayer = NULL;
	m_pSocket = NULL;
	m_eCurrentMenu = _MNU_NONE;
	m_pMenuData = NULL;
	m_nCurrentSubMenu = -1;
	m_nLastSubMenu = -1;
}

CUser::~CUser()
{
	if ( m_pSocket )
		m_pSocket->Close();

	delete m_pSocket;
	delete m_pStatus;
	delete m_pFlags;
	delete m_pAccount;
}

void CUser::Disconnect()
{
	Save();

	if ( m_pSocket )
	{
		m_pSocket->Write("\n\r### You are being disconnected from the server ###\n\r");
		m_pSocket->Close();

		delete m_pSocket;
		m_pSocket = NULL;
	}

}

/*
std::ostream& operator << ( std::ostream& stream, const CUser& user )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream,		"[User]");
	Tools.WriteLn(stream,		" Creation Date     : %d",		user.m_CreationDate);
	Tools.WriteLn(stream,		" Email             : \"%s\"",	user.m_gsEmail);
	Tools.WriteLn(stream,		" Last Access Date  : %d",		user.m_LastAccessDate);
	Tools.WriteLn(stream,		" Last Time Online  : %d",		user.m_LastTimeOnline);
	Tools.WriteLn(stream,		" Name              : \"%s\"",	user.m_gsName);
	Tools.WriteLn(stream,		" Password          : \"%s\"",	user.m_gsPassword);
	Tools.WriteLn(stream,		" Total Time Online : %d",		user.m_TotalTimeOnline);
	Tools.WriteLn(stream,		" Unique ID         : %d",		user.m_iUniqueID);
	stream <<					" Flags             : " << *user.m_pFlags;
	stream <<					" Status            : " << *user.m_pStatus;
	stream << *user.m_pAccount;
	Tools.WriteLn(stream,		"[/User]");

	return stream;
}


std::istream& operator >> ( std::istream& stream, CUser& user )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	streampos marker;
	gString gsKey, gsString;
	bool bDone = false;

	if (!stream.fail() && !stream.eof())

	try
	{
		if ( Tools.ReadKey(stream) == "[User]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/User]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'C':
						if ( gsKey == "Creation Date" )
							Tools.ReadData(stream, user.m_CreationDate);
						break;
					case 'E':
						if ( gsKey == "Email" )
							Tools.ReadData(stream, user.m_gsEmail);
						break;
					case 'F':
						if ( gsKey == "Flags" )
							Tools.ReadData(stream, *user.m_pFlags);
						break;
					case 'L':
						if ( gsKey == "Last Access Date" )
							Tools.ReadData(stream, user.m_LastAccessDate);
						else
						if ( gsKey == "Last Time Online" )
							Tools.ReadData(stream, user.m_LastTimeOnline);
						break;
					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, user.m_gsName);
						break;
					case 'P':
						if ( gsKey == "Password" )
							Tools.ReadData(stream, user.m_gsPassword);
						break;
					case 'S':
						if ( gsKey == "Status" )
							Tools.ReadData(stream, *user.m_pStatus);
						break;
					case 'T':
						if ( gsKey == "Total Time Online" )
							Tools.ReadData(stream, user.m_TotalTimeOnline);
						break;
					case 'U':
						if ( gsKey == "Unique ID" )
							Tools.ReadData(stream, user.m_iUniqueID);
						break;
					case '[':
						if ( gsKey == "[Account]" )
						{
							stream.seekg(marker);
							stream >> *user.m_pAccount;
						}
						break;
					default:
						g_Log.Log(LOG_ERROR, "[User::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/User]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CUser::>>] Invalid stream!");


	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CUser::>>] Error encountered while reading %s\'s file..", user.Name());
	}

	return stream;
}

bool CUser::Save()
{
	std::fstream fp;
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile = globals.m_Config.szDir[CGameObjects::_USER] + Name();

	fp.open(szFile, ios::out);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp << *this;
	//fp.unlock();
	fp.flush();
	fp.close();

	return true;
}

bool CUser::Load()
{
	std::fstream fp;
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile = globals.m_Config.szDir[CGameObjects::_USER] + Name();

	fp.open(szFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp >> *this;
	//fp.unlock();
	fp.close();

	return true;
}
*/

void CUser::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",				m_gsName);
	Tools.WriteXml(pParent, "email",			m_gsEmail);
	Tools.WriteXml(pParent, "unique_id",		m_iUniqueID);
	Tools.WriteXml(pParent, "password",			m_gsPassword);
	Tools.WriteXml(pParent, "creation_date",	m_CreationDate);
	Tools.WriteXml(pParent, "last_access_date", m_LastAccessDate);
	Tools.WriteXml(pParent, "last_time_online", m_LastTimeOnline);
	Tools.WriteXml(pParent, "total_time_online",m_TotalTimeOnline);
	Tools.WriteXml(pParent, "flags",			*m_pFlags);
	Tools.WriteXml(pParent, "status",			*m_pStatus);

	TiXmlNode* pAccountNode = Tools.InsertXmlChild(pParent, "Account");
	m_pAccount->WriteXml(pAccountNode);
}

void CUser::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",				m_gsName);
	Tools.ReadXml(pParent, "email",				m_gsEmail);
	Tools.ReadXml(pParent, "unique_id",			m_iUniqueID);
	Tools.ReadXml(pParent, "password",			m_gsPassword);
	Tools.ReadXml(pParent, "creation_date",		m_CreationDate);
	Tools.ReadXml(pParent, "last_access_date",	m_LastAccessDate);
	Tools.ReadXml(pParent, "last_time_online",	m_LastTimeOnline);
	Tools.ReadXml(pParent, "total_time_online",	m_TotalTimeOnline);
	Tools.ReadXml(pParent, "flags",				*m_pFlags);
	Tools.ReadXml(pParent, "status",			*m_pStatus);

	TiXmlNode* pAccountNode = pParent->FirstChild("Account");

	m_pAccount->ReadXml(pAccountNode);
}

bool CUser::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile = globals.m_Config.szDir[CGameObjects::_USER] + Name();

	m_TotalTimeOnline += time(0);
	m_LastAccessDate = time(0);
	m_LastTimeOnline = time(0);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "User");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

bool CUser::Load()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile = globals.m_Config.szDir[CGameObjects::_USER] + Name();

	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)szFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("User");

	ReadXml(pNode);
	return true;
}

bool CUser::Exists(const gString& gsUser)
{
	std::fstream fp;
	gString szFile;

	szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_USER] + gsUser;

	fp.open(szFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	fp.close();
	return true;
}


bool CUser::SetSocket(CSocket* pSocket)
{
	if ( m_pSocket )
	{
		m_pSocket->Close();
		delete m_pSocket;
	}
	m_pSocket = pSocket;

	return m_pSocket == NULL;
}

bool CUser::SetPlayer(CPlayer* pPlayer, bool bSave)
{
	if ( m_pPlayer && bSave )
	{
		m_pPlayer->Save();
		// #TEMP# FIX
		//delete m_pPlayer;
	}

	m_pPlayer = pPlayer;

	if ( pPlayer )
		pPlayer->SetUser(this);

	return true;
}

bool CUser::SetName(const gString& gsNewName)
{
	CGameObjects& globals = CGameObjects::Get();
	std::fstream fp;
	gString szFile;

	szFile.Format("%s%s.dat", globals.m_Config.szDir[CGameObjects::_USER], gsNewName);

	fp.open(szFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		m_gsName = gsNewName;
	else
	{
		fp.close();
		return false;
	}

	return true;
}

bool CUser::SetPassword(const gString& gsNewPassword)
{
	if ( !gsNewPassword.IsEmpty() )
	{
		m_gsPassword = MD5String(gsNewPassword);
		return true;
	}

	return false;
}

bool CUser::SetEmail(const gString& gsNewEmail)
{
	m_gsEmail = gsNewEmail;
	// I'd like to set up an email verification system in the future so that we know
	// the email address is valid, by sending an initial password to the user. -DV
	return false;
}

bool CUser::SetCreationDate(time_t tNewDate)
{
	return false;
}

bool CUser::SetLastDateOnline(time_t tNewDate)
{
	return false;
}

bool CUser::SetTotalTimeOn(time_t tNewTime)
{
	return false;
}

bool CUser::IncTimeOnline(time_t tAdd)
{
	return false;
}

bool CUser::SetLastTimeOn(time_t tNewTime)
{
	return false;
}

bool CUser::IncLastTimeOn(time_t tAdd)
{
	return false;
}

bool CUser::SetAccount(CAccount* pAccount)
{
	if ( pAccount && !m_pAccount )
	{
		m_pAccount = pAccount;
		return true;
	}

	else
	if ( m_pAccount )
	{
		delete m_pAccount;
		m_pAccount = pAccount;

		gString gsFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_USER] + Name();

		if ( unlink(gsFile) == 0 )
			g_Log.Log(LOG_INFO, "[CUser::SetAccount] %s has been deleted.\n", Name());

		return true;
	}

	return false;
}

CUser::t_eUserState CUser::SetState(t_eUserState eNewState)
{
	m_eLastState = m_eState;
	m_eState = eNewState;
	return m_eState;
}


