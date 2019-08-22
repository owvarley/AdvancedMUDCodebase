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

// Class    :: CComms
// Header   :: Module.h
// DEPRECIATED

#pragma warning(disable:4786)

#include "GameObjects.h"
#include <direct.h>

/*
CComms::CComms()
{
	m_bPowered = false;
	m_Recording = NULL;
}

CComms::~CComms()
{
	m_Jamming.clear();
	m_Snooping.clear();
	m_Open.clear();
	m_nOpen = 0;
	m_bPowered = false;
	m_Recording = NULL;
	m_Protocols.clear();
	m_Recordings.clear();
}

std::ostream& operator << ( std::ostream& stream, const CComms& comms )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	stream << (const CModule&)(comms);
	Tools.WriteLn(stream, "[Comms]");
	stream << *comms.m_Memory;
	stream << *comms.m_Signal;
	stream << *comms.m_Jammer;
	stream << *comms.m_Snooper;
	MsgList mMessages = comms.m_Recordings;
	
	for (msgs = mMessages.begin(); msgs != mMessages.end(); msgs++)
	{
		CMessage* pMsg = (*msgs);
		stream << pMsg;
	}

	Tools.WriteLn(stream, "[/Comms]");

	return stream;
}

std::istream& operator >> ( std::istream& stream, CComms& comms )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	try
	{
		if ( Tools.ReadKey(stream) == "[Comms]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Comms]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'I':
						if ( gsKey == "IntegrityC" )
							Tools.ReadData(stream, comms.m_ncIntegrity);
						if ( gsKey == "IntegrityM" )
							Tools.ReadData(stream, comms.m_nmIntegrity);
						break;		
					
					case 'F':
						if ( gsKey == "Filename" )
							Tools.ReadData(stream, comms.m_gsFileName);
						break;

					case 'M':
						if ( gsKey == "Modifier" )
							Tools.ReadData(stream, comms.m_nModifier);
						break;

					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, comms.m_gsName);
						break;

					case 'T':
						if ( gsKey == "Type" )
							Tools.ReadData(stream, comms.m_nType);
						break;



					default:
						Tools.Report(E_ERROR, "[CComms::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Comms]");
			}
		}

	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CComms::>>] Error encountered while reading Comms file..");
	}

	return stream;
}

bool CComms::Load(gString gsFile)
{
	CGameObjects& globals = CGameObjects::Get();
	std::fstream fp;

	fp.open(gsFile, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	fp >> *this;
	fp.close();

	return true;
}

bool CComms::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	std::fstream fp;

	gString gsFile;
	gsFile.Format("%sShips\\Components\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], this->m_gfFileName);

	fp.open(gsFile, ios::out);

	if ( !fp.is_open() )
		return false;

	fp << *this;

	fp.flush();
	fp.close();

	return true;
}

bool CComms::Record(CFrequency *pFreq, gString gsMsg, gString gsShip)
{
	// Create the new object and add it to the list of messages
	CMessage* pMsg = new CMessage();

	pMsg->m_gsEncryption = pFreq->m_gsEncryption;
	pMsg->m_gsFrequency.Format("%3.2f", pFreq->m_nFrequency);
	pMsg->m_gsMessage = gsMsg;
	pMsg->m_gsShip = gsShip;
	pMsg->m_tTime = time(0);

	this->m_Recordings.push_back(pMsg);

	return true;
}

// Check if a the ship has this frequency open
bool CComms::IsOpen(CFrequency* pOpen)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = pGalaxy->GetShi(this->m_Ship);
	CComms* pComms = (CComms*)pShip->Fetch(CComponent::CT_COMMS);
	FreqList::iterator freq;
	bool bFound = false;

	for (freq = pComms->m_Open.begin(); freq != pComms->m_Open.end(); freq++)
	{
		CFrequency* pFreq = *freq;
		
		// Is it open on this ship?
		if (pFreq->m_nFrequency == pOpen->m_nFrequency)
			bFound = true;
	}

	return bFound;

}

// Check if this Channel is Encrypted
bool CComms::IsEncrypted(CFrequency* pFreq)
{
	if (pFreq->m_gsEncryption == "")
		return false;

	return true;
}

// Check if the ship can Decrypt this Channel
bool CComms::CanDecrypt(gString gsProtocol)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = pGalaxy->GetShi(this->m_Ship);
	CComms* pComms = (CComms*)pShip->Fetch(CComponent::CT_COMMS);
	gStringList::iterator comms;
	bool bFound = false;

	// Check Encryption protocol selected is valid
	for (comms = pComms->m_Protocols.begin(); comms != pComms->m_Protocols.end(); comms++)
	{
		gString gsProt = *comms;

		if (gsProt == gsProtocol)
		{
			bFound = true;
		}
	}
	return bFound;

}
	
// Is this Ship Jamming this Frequency
bool CComms::IsJammed(CFrequency* pFreq)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = pGalaxy->GetShi(this->m_Ship);
	CComms* pComms = (CComms*)pShip->Fetch(CComponent::CT_COMMS);
	FreqList::iterator freq;
	gStringList::iterator comms;

	if (pComms->m_Jamming.size() > 0)
	{
		freq = pComms->m_Jamming.begin();
		CFrequency* pFrom = *freq;
		CFrequency* pTo = *(freq+1);

		if (pFreq->m_nFrequency >= pFrom->m_nFrequency && pFreq->m_nFrequency <= pTo->m_nFrequency)
			return true;
	}

	return false;

}
	
// Is this Ship Snooping this Frequency
bool CComms::IsSnooped(CFrequency* pFreq)
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CShip* pShip = pGalaxy->GetShi(this->m_Ship);
	CComms* pComms = (CComms*)pShip->Fetch(CComponent::CT_COMMS);
	FreqList::iterator freq;
	gStringList::iterator comms;

	if (pComms->m_Snooping.size() > 0)
	{
		freq = pComms->m_Snooping.begin();
		CFrequency* pFrom = *freq;
		CFrequency* pTo = *(freq+1);

		if (pFreq->m_nFrequency >= pFrom->m_nFrequency && pFreq->m_nFrequency <= pTo->m_nFrequency)
			return true;
	}

	return false;
}

CFrequency::CFrequency()
{
	this->m_nFrequency = 0.0;
	this->m_gsEncryption = "";
}

CFrequency::~CFrequency()
{
	this->m_gsEncryption = "";
	this->m_nFrequency = 0.0;
}

CMessage::CMessage()
{
	this->m_gsEncryption = "";
	this->m_gsMessage = "";
	this->m_gsShip = "";
	this->m_gsFrequency = "";
	this->m_tTime = time(0);
}

CMessage::~CMessage()
{
	this->m_gsEncryption = "";
	this->m_gsMessage = "";
	this->m_gsFrequency = "";
	this->m_gsShip = "";
}


std::ostream& operator << ( std::ostream& stream, const CMessage& msg )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Msg]");
	Tools.WriteLn(stream, " Encryption       : \"%s\"", msg.m_gsEncryption);
	Tools.WriteLn(stream, " Frequency        : \"%s\"", msg.m_gsFrequency);
	Tools.WriteLn(stream, " Message          : \"%s\"", msg.m_gsMessage);
	Tools.WriteLn(stream, " Recorded         : %d", msg.m_tTime);
	Tools.WriteLn(stream, " Ship             : \"%s\"", msg.m_gsShip);
	Tools.WriteLn(stream, "[/Msg]");

	return stream;
}



std::istream& operator >> ( std::istream& stream, CMessage& msg )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CGameObjects& globals = CGameObjects::Get();
	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	try
	{
		if ( Tools.ReadKey(stream) == "[Msg]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Msg]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{		
					case 'E':
						if ( gsKey == "Encryption" )
							Tools.ReadData(stream, msg.m_gsEncryption);
						break;

					case 'F':
						if ( gsKey == "Frequency" )
							Tools.ReadData(stream, msg.m_gsFrequency);
						break;

					case 'M':
						if ( gsKey == "Message" )
							Tools.ReadData(stream, msg.m_gsMessage);
						break;

					case 'R':
						if ( gsKey == "Recorded" )
							Tools.ReadData(stream, msg.m_tTime);
						break;

					case 'S':
						if ( gsKey == "Ship" )
							Tools.ReadData(stream, msg.m_tTime);
						break;

					default:
						Tools.Report(E_ERROR, "[CMessage::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Msg]");
			}
		}

}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CMessage::>>] Error encountered while reading Message file..");
	}

	return stream;
}
*/

