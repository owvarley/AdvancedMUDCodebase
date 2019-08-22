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

//
// This file contains common communication commands
//

#if !defined __CMDSCREW_H__
#define __CMDSCREW_H__

#include "MudCore.h"
#include "Command.h"


class CmdRoster : public CCommand
{
public:
	CmdRoster()  { m_gsName = "Roster"; m_gsClass = "CmdRoster"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdCCommand: public CCommand
{
public:
	CmdCCommand()  { m_gsName = "Assume"; m_gsClass = "CmdCCommand"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdCBOrder: public CCommand
{
public:
	CmdCBOrder()  { m_gsName = "BOrder"; m_gsClass = "CmdCBOrder"; m_AccessReq = CActor::_NPC; m_Flags->InitSet();
	m_Flags->SetBit( CCommand::_PRESERVE_COMMAND );
	m_Aliases.push_back("SPEED");		m_Aliases.push_back("COURSE");		m_Aliases.push_back("ROLL");
	m_Aliases.push_back("POWER");		m_Aliases.push_back("TRANSMIT");	m_Aliases.push_back("BROADCAST");
	m_Aliases.push_back("TIGHTBEAM");	m_Aliases.push_back("JAMM");		m_Aliases.push_back("SNOOP");
	m_Aliases.push_back("COMM");		m_Aliases.push_back("SWEEP");		m_Aliases.push_back("LAUNCH");
	m_Aliases.push_back("LAND");		m_Aliases.push_back("TARGET");}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdCOrder: public CCommand
{
public:
	CmdCOrder()  { m_gsName = "Order"; m_gsClass = "CmdCOrder"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdCGq: public CCommand
{
public:
	CmdCGq()  { m_gsName = "Gq"; m_gsClass = "CmdCGq"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdCReport: public CCommand
{
public:
	CmdCReport()  { m_gsName = "Report"; m_gsClass = "CmdCReport"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

#endif