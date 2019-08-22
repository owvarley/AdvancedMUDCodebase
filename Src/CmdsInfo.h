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

#if !defined __CMDSINFO_H__
#define __CMDSINFO_H__

#include "MudCore.h"
#include "Command.h"


class CmdGalaxy : public CCommand
{
public:
	CmdGalaxy()  { m_gsName = "Galaxies";m_gsClass = "CmdGalaxy";  m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdEmotions : public CCommand
{
public:
	CmdEmotions()  { m_gsName = "Emotions";m_gsClass = "CmdEmotions";  m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdShips : public CCommand
{
public:
	CmdShips()  { m_gsName = "Ships";m_gsClass = "CmdShips";  m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdTeam : public CCommand
{
public:
	CmdTeam()  { m_gsName = "Team";m_gsClass = "CmdTeam";  m_AccessReq = CActor::_PLAYER; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

#endif