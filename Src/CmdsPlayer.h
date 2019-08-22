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
// This file contains common player commands
//

#if !defined __CMDSPLAYER_H__
#define __CMDSPLAYER_H__

#include "Command.h"


class _export CmdSave : public CCommand
{
public:
	CmdSave()  { m_gsName = "Save"; m_gsClass = "CmdSave"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdQuit : public CCommand
{
public:
	CmdQuit()  { m_gsName = "Quit"; m_gsClass = "CmdQuit"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdLook : public CCommand
{
public:
	CmdLook()  { m_gsName = "Look"; m_gsClass = "CmdLook"; m_AccessReq = CActor::_NPC; m_Flags->InitSet();
	m_Flags->SetBit( CCommand::_PRESERVE_COMMAND ); m_Aliases.push_back("SHOW"); m_Aliases.push_back("GLANCE"); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdWho : public CCommand
{
public:
	CmdWho()  { m_gsName = "Who"; m_gsClass = "CmdWho"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdHelp : public CCommand
{
public:
	CmdHelp()  { m_gsName = "Help"; m_gsClass = "CmdHelp"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdSetTitle : public CCommand
{
public:
	CmdSetTitle()  { m_gsName = "Title"; m_gsClass = "CmdSetTitle"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdScore : public CCommand
{
public:
	CmdScore() { m_gsName = "Score"; m_gsClass = "CmdScore"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdOpen : public CCommand
{
public:
	CmdOpen() { m_gsName = "Open"; m_gsClass = "CmdOpen"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdClose : public CCommand
{
public:
	CmdClose() { m_gsName = "Close"; m_gsClass = "CmdClose"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdProne : public CCommand
{
public:
	CmdProne() { m_gsName = "Prone"; m_gsClass = "CmdProne"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdCrouch : public CCommand
{
public:
	CmdCrouch() { m_gsName = "Crouch"; m_gsClass = "CmdCrouch"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdSit : public CCommand
{
public:
	CmdSit() { m_gsName = "Sit"; m_gsClass = "CmdSit"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdStand : public CCommand
{
public:
	CmdStand() { m_gsName = "Stand"; m_gsClass = "CmdStand"; m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

#endif
