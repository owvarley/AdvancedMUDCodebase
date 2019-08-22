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

// Class    :: CAccount
// Header   :: Account.h
// Function :: Provides the Account Class Implementation. An account is really just a holder for player
//			:: files, statistical information, and payment information (if applicable). All players will
//		    :: have an account from which they can manage their character files.


#include <time.h>
#include <vector>
#include <fstream>

#include "MudCore.h"
#include "GameObjects.h"
#include "Tools.h"
#include "Account.h"
#include "../gTools/Log.h"
#include "Player.h"

#define NORMAL_MONTHLY_RATE 0.0f

CAccount::CAccount()
{
	m_fCredits = 0.0f;
	m_fDebits = 0.0f;
	m_AccountType = _AT_TESTING;
	m_pStatus = new CSet;
	m_pFlags = new CSet;
}

CAccount::~CAccount()
{
	delete m_pFlags;
	delete m_pStatus;
}

/*
std::ostream& operator << ( std::ostream& stream, const CAccount& account )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream,		"[Account]");
	Tools.WriteLn(stream,		" Credits           : %f", account.m_fCredits);
	Tools.WriteLn(stream,		" Debits            : %f", account.m_fDebits);
	Tools.WriteLn(stream,		" Type              : %d", account.m_AccountType);
	stream <<					" Flags             : " << *account.m_pFlags;
	stream <<					" Status            : " << *account.m_pStatus;
	stream <<					" Player List       : " << account.m_gsPlayerList << "\n";
	Tools.WriteLn(stream,		"[/Account]");

	return stream;
}


std::istream& operator >> ( std::istream& stream, CAccount& account )
{
	LOG_SCOPE("CAccount:>>");

	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	bool bDone = false;
	int iTempEnumKey;

	if (!stream.fail() && !stream.eof())

	try
	{
		if ( Tools.ReadKey(stream) == "[Account]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Account]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'C':
						if ( gsKey == "Credits" )
							Tools.ReadData(stream, account.m_fCredits);
						break;
					case 'D':
						if ( gsKey == "Debits" )
							Tools.ReadData(stream, account.m_fDebits);
						break;
					case 'F':
						if ( gsKey == "Flags" )
							Tools.ReadData(stream, *account.m_pFlags);
						break;
					case 'P':
						if ( gsKey == "Player List" )
							Tools.ReadData(stream, account.m_gsPlayerList);
						break;
					case 'S':
						if ( gsKey == "Status" )
							Tools.ReadData(stream, *account.m_pStatus);
						break;
					case 'T':
						if ( gsKey == "Type" )
						{
							iTempEnumKey = account.m_AccountType;
							Tools.ReadData(stream, iTempEnumKey);
						}
						break;
					default:
						g_Log.Log(LOG_ERROR, "Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Account]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "Error encountered while reading file.");
		throw;
	}

	return stream;
}*/

void CAccount::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "credits",		m_fCredits);
	Tools.WriteXml(pParent, "debits",		m_fDebits);
	Tools.WriteXml(pParent, "type",			(int&)m_AccountType);
	Tools.WriteXml(pParent, "flags",		*m_pFlags);
	Tools.WriteXml(pParent, "status",		*m_pStatus);
	Tools.WriteXml(pParent, "player_list",	m_gsPlayerList);
}

void CAccount::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "credits",		m_fCredits);
	Tools.ReadXml(pParent, "debits",		m_fDebits);
	Tools.ReadXml(pParent, "type",			(int&)m_AccountType);
	Tools.ReadXml(pParent, "flags",			*m_pFlags);
	Tools.ReadXml(pParent, "status",		*m_pStatus);
	Tools.ReadXml(pParent, "player_list",	m_gsPlayerList);
}

const float CAccount::GetAccountRate() const
{
	switch ( Type() )
	{
		case _AT_TESTING:
			return 0.0f;
			break;
		case _AT_NORMAL:
			return NORMAL_MONTHLY_RATE;
			break;
		default:
			return NORMAL_MONTHLY_RATE;
			break;
	}
}

bool CAccount::PlayerListAdd(const CPlayer* pPlayer)
{
	gStringList::iterator pos;

	if (!pPlayer)
		return false;

	for(pos = m_gsPlayerList.begin(); pos != m_gsPlayerList.end(); pos++)
	{
		if ( pPlayer->Name() == (gString)(*pos) )
			return false;
	}

	m_gsPlayerList.push_back( pPlayer->Name() );
	return true;
}

bool CAccount::PlayerListRemove(const CPlayer* pPlayer)
{
	gStringList::iterator pos;

	if (!pPlayer)
		return false;

	for(pos = m_gsPlayerList.begin(); pos != m_gsPlayerList.end(); pos++)
	{
		if ( pPlayer->Name() == (gString)(*pos) )
		{
			m_gsPlayerList.remove( pPlayer->Name() );
			return true;
		}
	}

	return false;
}

bool CAccount::PlayerListAdd(const gString& gsPlayer)
{
	gStringList::const_iterator pos;

	for(pos = m_gsPlayerList.begin(); pos != m_gsPlayerList.end(); pos++)
	{
		if ( gsPlayer.CompareNoCase( (gString)(*pos) ) == 0 )
			return false;
	}

	m_gsPlayerList.push_back( gsPlayer );
	return true;
}

bool CAccount::PlayerListRemove(const gString& gsPlayer)
{
	gStringList::iterator pos;

	for(pos = m_gsPlayerList.begin(); pos != m_gsPlayerList.end(); pos++)
	{
		if ( gsPlayer.CompareNoCase( (gString)(*pos) ) == 0 )
		{
			m_gsPlayerList.remove( gsPlayer );
			return true;
		}
	}

	return false;
}


bool CAccount::SetCredits(const float& fAmount)
{
	return false;
}

bool CAccount::AdjCredits(const float& fAmount)
{
	return false;
}

bool CAccount::SetDebits(const float& fAmount)
{
	return false;
}

bool CAccount::AdjDebits(const float& fAmount)
{
	return false;
}

bool CAccount::SetType(const e_AccountType& eNewType)
{
	return false;
}


