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
// This file contains common movement commands
//

#if !defined __CMDSMOVE_H__
#define __CMDSMOVE_H__

#include "Command.h"


class CmdMove : public CCommand
{
public:
	CmdMove()  { m_gsName = "Move"; m_gsClass = "CmdMove"; m_AccessReq = CActor::_NPC; m_Flags->InitSet();
				 m_Flags->SetBit( CCommand::_PRESERVE_COMMAND );
	             m_Aliases.push_back("NORTH"); m_Aliases.push_back("SOUTH"); m_Aliases.push_back("EAST");
				 m_Aliases.push_back("WEST"); m_Aliases.push_back("UP"); m_Aliases.push_back("DOWN");
				 m_Aliases.push_back("NORTHWEST"); m_Aliases.push_back("NORTHEAST");
				 m_Aliases.push_back("SOUTHWEST"); m_Aliases.push_back("SOUTHEAST");
				 m_Aliases.push_back("NW"); m_Aliases.push_back("NE");
				 m_Aliases.push_back("SW"); m_Aliases.push_back("SE"); }

	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdBoard : public CCommand
{
public:
	CmdBoard() { m_gsName = "Board"; m_gsClass = "CmdBoard"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdLeave : public CCommand
{
public:
	CmdLeave() { m_gsName = "Leave"; m_gsClass = "CmdLeave"; m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};


#endif
