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

// Class    :: CCommand CCmdParser
// Header   :: Command.h
// Function :: Implements the Command Parsers and Command structure

#include <fstream>

#include "Command.h"
#include "Actor.h"
#include "Area.h"
#include "Room.h"
#include "GameObjects.h"
#include "Tools.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Socket.h"
#include "../gTools/Log.h"
#include "Set.h"

#include "CmdsComm.h"
#include "CmdsMove.h"
#include "CmdsPlayer.h"
#include "CmdsStaff.h"
#include "CmdsInfo.h"

char* CCommand::szCTypes[] = { "Administrator", "Building", "Information", "Space", "Combat" };


// We need to keep a pointer to the current command parser being loaded so that
// our non-member serializers can call into it's functions.
CCmdParser* _pgParser = NULL;

CCommand::CCommand()
{
	m_gsName = "";
	m_gsClass  = "";
	m_Flags = new CSet;
	m_AccessReq = CActor::_NPC;
}


CCommand::~CCommand()
{
	delete m_Flags;
}

bool CCommand::operator==(CCommand c2)
{
	if ( !this )
		return false;

	if ( m_gsName == c2.m_gsName && m_AccessReq == c2.m_AccessReq )
		return true;

	return false;
}

bool CCommand::operator!=(CCommand c2)
{
	if ( !this )
		return true;

	if ( m_gsName == c2.m_gsName && m_AccessReq == c2.m_AccessReq )
		return false;

	return true;
}


bool CCommand::IsValidCommand(CActor* Ch)
{
	int i;

	if ( Ch->ActorFlags()->IsSet( m_AccessReq ) )
		return true;

	for (i = CActor::_NUMACTORFLAGS-1; i > (int)m_AccessReq; --i )
		if ( Ch->ActorFlags()->IsSet( i ) )
			return true;

	return false;
}


// Write this command to an output stream
std::ostream& operator << ( std::ostream& stream, CCommand& command )
{
	gStringList::iterator itor;
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Command]");

	Tools.WriteLn(stream, " Name              : \"%s\"",	command.m_gsName);
	Tools.WriteLn(stream, " Class             : \"%s\"",    command.m_gsClass);

    for (itor = command.m_Aliases.begin(); itor != command.m_Aliases.end(); itor++)
		Tools.WriteLn(stream, " Alias             : \"%s\"", *itor);

	Tools.WriteLn(stream, " Access Required   : %d",		command.m_AccessReq);
	stream <<             " Flags             : " <<		*command.m_Flags;

	Tools.WriteLn(stream, "[/Command]");

	return stream;
}

// Read this actor from an input stream
std::istream& operator >> ( std::istream& stream, CCommand& command )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	try
	{
		if ( Tools.ReadKey(stream) == "[Command]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Command]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'A':
						if ( gsKey == "Alias" )
						{
							gString gsAlias;
							Tools.ReadData(stream, gsAlias);
							command.m_Aliases.push_front(gsAlias);
						}
						else
						if ( gsKey == "Access Required" )
						{
							int nInt;
							Tools.ReadData(stream, nInt);
							command.m_AccessReq = (CActor::e_ActorFlags)nInt;
						}
						break;
					case 'C':
						if ( gsKey == "Class" )
						  Tools.ReadData(stream, command.m_gsClass);
						break;
					case 'F':
						if ( gsKey == "Flags" )
							Tools.ReadData(stream, *command.m_Flags);
						break;
					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, command.m_gsName);
						break;
					default:
						g_Log.Log(LOG_ERROR, "[CCommand::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Command]");
			}
		}
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CCommand::>>] Error encountered while reading %s\'s file..", command.Name());
	}

	return stream;
}




CCmdParser::CCmdParser()
{
	m_gsName = "";
	m_gsFileName = "";
	m_bActive = true;
	m_AccessFlags = new CSet;
	m_AccessFlags->SetBit( CActor::_NPC );
	m_AccessFlags->SetBit( CActor::_PLAYER );
}

CCmdParser::~CCmdParser()
{

}

CCommand* CCmdParser::DetermineCmd(gString gsClass)
{
	gString gsCmd = "Create_" + gsClass;
	t_CmdFn* pCmd = NULL;

	if ( gsClass == "" || gsClass.Length() == 0 )
		return NULL;

#ifdef _WINDOWS
	pCmd = (t_CmdFn*)GetProcAddress((HMODULE)CGameObjects::Get().Handle(), gsCmd);
#else
	pCmd = (t_CmdFn*)dlsym (CGameObjects::Get().Handle(), gsCmd);
#endif

	if ( pCmd )
		return (*pCmd)();


	g_Log.Log(LOG_WARNING, "[CCmdParser::DetermineCmd] Unknown command \"%s\".", gsClass);
	return NULL;
}


bool CCmdParser::Init(const gString& gsList)
{
	SetFileName(gsList);

	return Load();
}

void CCmdParser::AddCommand(CCommand* pCmd)
{
	m_Commands.push_back(pCmd);
}

void CCmdParser::DelCommand(CCommand* pCmd)
{
	CCommand* pCmdFound = NULL;

	CommandList::iterator itor;

	for (itor=m_Commands.begin(); itor != m_Commands.end(); itor++)
	{
		pCmdFound = static_cast<CCommand*>(*itor);

		if ( *pCmdFound == *pCmd )
		{
			m_Commands.erase(itor);
			return;
		}
	}
}

bool CCmdParser::CanAccess(CActor* pActor)
{
	int n;

	for (n=CActor::_NUMACTORFLAGS-1;n>0;n--)
	{
		if ( m_AccessFlags->IsSet(n) && pActor->ActorFlags()->IsSet(n) )
			return true;
	}

	return false;
}

void CCmdParser::DescribeTo(CActor* Ch)
{
	if ( Ch )
	{
		CommandList::iterator pos;
		int i=0;
		int j=0;

		Ch->Write("#600/////////////////#601[#701 %-20s #601]#600\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#700 \n\r", this->Name());
		for ( pos = m_Commands.begin(); pos != m_Commands.end(); pos++ )
		{
			i++;
			j++;

			if (j%4 == 0 || j%2 == 0)
			{
				Ch->Write( "#600 %-15s%s",(*pos)->Name(),i%4==0?"#700\n\r":"");
				j = 0;
			}
			else
				Ch->Write( "#700 %-15s%s",(*pos)->Name(),i%4==0?"#700\n\r":"");


		}
		Ch->Write("\n\r\t\t\t\t\t%d Commands.\n\r", m_Commands.size());

		if ( i>0 )
			Ch->Write("\n\r");
	}
}


void CCommand::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",		m_gsName);
	Tools.WriteXml(pParent, "class",	m_gsClass);
	Tools.WriteXml(pParent, "aliases",	m_Aliases);
	Tools.WriteXml(pParent, "access",	(int&)m_AccessReq);
	Tools.WriteXml(pParent, "flags",	*m_Flags);
}

void CCommand::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	m_Aliases.clear();

	Tools.ReadXml(pParent, "name",		m_gsName);
	Tools.ReadXml(pParent, "class",		m_gsClass);
	Tools.ReadXml(pParent, "aliases",	m_Aliases);
	Tools.ReadXml(pParent, "access",	(int&)m_AccessReq);
	Tools.ReadXml(pParent, "flags",		*m_Flags);
}

bool CCmdParser::Interpret(CActor* pA, gString CommandLine)
{
	CommandList::iterator CommandItor;
	gStringList::iterator AliasItor;

	bool bFound = false;

    gStringList cList;
	gString Command;
	gString SysCommand;

	CCommand* pCommand = NULL;

	CommandLine.TrimSpacesLeft();
	CommandLine.TrimSpacesRight();

	if ( CommandLine.IsEmpty() || !pA )
		return false;

	// Build the argument list. Single and double quoted arguments are passed as
	// a single argument to the command.
	Command = "none";
	while ( !Command.IsEmpty() )
	{
		Command = CGameObjects::Get().Tools()->GetNextWord(CommandLine);
		if ( !Command.IsEmpty() )
		  cList.push_back( Command );
	}

	// 15/3/2006 - OWV
	// Added to prevent a crash when " or ' was entered alone
	if (cList.size() <= 0)
		return false;

	Command = (gString)(*cList.begin());
	Command.MakeUpper();
	cList.pop_front();

	if ( !Command.IsEmpty() )
	{
		for ( CommandItor = m_Commands.begin(); CommandItor != m_Commands.end(); CommandItor++)
		{
			pCommand = (CCommand*)(*CommandItor);
			SysCommand = pCommand->Name();
			SysCommand.MakeUpper();

			if ( SysCommand.Find ( Command ) != 0 )
			{
				if ( pCommand->Aliases().size() > 0 )
				{
					gString gsAlias;

					for (AliasItor = pCommand->Aliases().begin(); AliasItor != pCommand->Aliases().end(); AliasItor++)
					{
						gsAlias = (gString)(*AliasItor);
						gsAlias.MakeUpper();

						if ( gsAlias.Find(Command) == 0 )
						{
							bFound = true;
							break;
						}
					}
				}
			}
			else
				bFound = true;

			if ( !bFound )
			  continue;

			// Some commands, (such as move) may require that you pass in the
			// original command name. Do so here.
			if ( pCommand->PreserveCommand() )
				cList.push_front(Command);

			if ( pCommand->Flags()->IsSet(CCommand::_DISABLED) )
			{
				pA->Write("'%s' has been disabled.\n\r", pCommand->Name());
				return false;
			}

			if ( pCommand->IsValidCommand(pA))
				return ( pCommand->Perform(pA, cList) );
			else
			{
				// pA->Write("Huh?\n\r");
				return false;
			}
		}
	}

	return false;
}
/*
bool CCmdParser::Load()
{
	std::fstream fp;
	CGameObjects& globals = CGameObjects::Get();

	fp.open(FileName(), ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	_pgParser = this;

	//fp.lock();
	fp >> *this;
	//fp.unlock();
	fp.close();

	_pgParser = NULL;

	return true;
}

bool CCmdParser::Save()
{
	std::fstream fp;

	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%s%s", globals.m_Config.szDir[CGameObjects::_DATA], FileName());

	fp.open(szFile, ios::in|ios::out);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp << *this;
	//fp.unlock();
	fp.flush();
	fp.close();

	return true;
} */

bool CCmdParser::Load()
{
	CGameObjects& globals = CGameObjects::Get();
	_pgParser = this;

	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)FileName()) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("CommandParser");

	ReadXml(pNode);

	return true;
}

bool CCmdParser::Save()
{
	std::fstream fp;

	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%s%s", (const char*)globals.m_Config.szDir[CGameObjects::_DATA], (const char*)FileName());

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "CommandParser");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}


// Write this parser to an output stream
std::ostream& operator << ( std::ostream& stream, CCmdParser& parser )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	time_t tm = time(0);
	gString gsTime = ctime(&tm);
	gsTime.DeleteChar( gsTime.Length()-1 ); // damn ctime trailing \n

	Tools.WriteLn(stream, "[CCmdParser]");
	Tools.WriteLn(stream, " Version           : %d",		VERSION);
	Tools.WriteLn(stream, " Last Saved On     : %s",        gsTime);
	Tools.WriteLn(stream, " Name              : \"%s\"",	parser.m_gsName);
	Tools.WriteLn(stream, " File Name         : \"%s\"",	parser.m_gsFileName);
	Tools.WriteLn(stream, " Active Status     : \"%s\"",    parser.m_bActive ? "True" : "False");
	stream <<             " Access Flags      : " <<		*parser.m_AccessFlags;

	for (int n = 0; n < parser.m_Commands.size(); n++)
		stream << *parser.m_Commands[n];

	Tools.WriteLn(stream, "[/CCmdParser]");

	return stream;
}

// Read this actor from an input stream
std::istream& operator >> ( std::istream& stream, CCmdParser& parser )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	streampos marker;
	bool bDone = false;
	int nVersion;

	try
	{
		if ( Tools.ReadKey(stream) == "[CCmdParser]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();

				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/CCmdParser]"  )
				switch ( gsKey[0] )
				{
					case 'A':
						if ( gsKey == "Active Status" )
							Tools.ReadData(stream, parser.m_bActive);
						else
						if ( gsKey == "Access Flags" )
							Tools.ReadData(stream, *parser.m_AccessFlags);
						break;
					case '[':
						if ( gsKey == "[Command]" )
						{
							gString gsString;

							// Next expected key is "Name".  This function will
							// have to change if the key order changes.
							if ( (gsString = Tools.ReadKey(stream)) == "Name" )
							{
								CCommand Cmd;

				
								Tools.ReadData(stream, gsString);
								stream.seekg(marker);

								if ( _pgParser )
								{
									CCommand* pCmd = NULL;

									stream >> Cmd;

									if ( (pCmd = _pgParser->DetermineCmd(Cmd.Class()) ) !=NULL)
									  _pgParser->AddCommand(pCmd);
								}
							}

						}
						break;
					case 'F':
						if ( gsKey == "File Name" )
							Tools.ReadData(stream, parser.m_gsFileName);
						break;
					case 'L':
						if ( gsKey == "Last Saved On" )
							Tools.ReadLn(stream); // throw away the data, we dont really care.
						break;
					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, parser.m_gsName);
						break;
					case 'V':
						if ( gsKey == "Version" )
							Tools.ReadData(stream, nVersion);
						break;
					default:
						g_Log.Log(LOG_ERROR, "[CCmdParser::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/CCmdParser]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CCmdParser::>>] Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CCmdParser::>>] Error encountered while reading %s\'s file..", parser.Name());
	}

	return stream;

}

void CCmdParser::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	time_t tm = time(0);
	gString gsTime = ctime(&tm);
	gsTime.DeleteChar( gsTime.Length()-1 ); 

	Tools.WriteXml(pParent, "last_saved_on",	gsTime);
	Tools.WriteXml(pParent, "name",				m_gsName);
	Tools.WriteXml(pParent, "file_name",		m_gsFileName);
	Tools.WriteXml(pParent, "is_active",		m_bActive);
	Tools.WriteXml(pParent, "access_flags",		*m_AccessFlags);

	for (int n = 0; n < m_Commands.size(); n++)
	{
		TiXmlNode* pCmd = Tools.InsertXmlChild(pParent, "Command");

		m_Commands[n]->WriteXml(pCmd);
	}
}

void CCmdParser::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "file_name",		m_gsFileName);
	Tools.ReadXml(pParent, "is_active",		m_bActive);
	Tools.ReadXml(pParent, "access_flags",	*m_AccessFlags);

	_pgParser = this;

	TiXmlNode* pCmdNode = pParent->FirstChild("Command");
	while ( pCmdNode != NULL )
	{
		CCommand cmd;
		CCommand* pCmd;

		cmd.ReadXml(pCmdNode);

		if ( (pCmd = _pgParser->DetermineCmd(cmd.Class()) )!=NULL)
		  _pgParser->AddCommand(pCmd);

		pCmdNode = pCmdNode->NextSibling("Command");
	}
}