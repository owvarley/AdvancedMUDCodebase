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

#if !defined __CMDSCOMM_H__
#define __CMDSCOMM_H__

#include "MudCore.h"
#include "Command.h"


class CmdSay : public CCommand
{
public:
	CmdSay()  { m_gsName = "Say";m_gsClass = "CmdSay";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); m_Aliases.push_back(","); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};


class CmdEmote : public CCommand
{
public:
	CmdEmote()  { m_gsName = "Emote";m_gsClass = "CmdEmote";  m_AccessReq = CActor::_NPC; m_Flags->InitSet();  m_Aliases.push_back(":"); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdOOC : public CCommand
{
public:
	CmdOOC()  { m_gsName = "Ooc";m_gsClass = "CmdOOC";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdTransmit : public CCommand
{
public:
	CmdTransmit()  { m_gsName = "Transmit";m_gsClass = "CmdTransmit";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdBroadcast : public CCommand
{
public:
	CmdBroadcast()  { m_gsName = "Broadcast";m_gsClass = "CmdBroadcast";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdTightbeam : public CCommand
{
public:
	CmdTightbeam()  { m_gsName = "Tightbeam";m_gsClass = "CmdTightbeam";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdShipNet : public CCommand
{
public:
	CmdShipNet()  { m_gsName = "Shipnet";m_gsClass = "CmdShipNet";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};


#endif
