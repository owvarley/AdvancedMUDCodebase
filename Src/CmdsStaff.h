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
// This file contains common administrative commands
//

#if !defined __CMDSSTAFF_H__
#define __CMDSSTAFF_H__

#include "Command.h"


class _export CmdShutdown : public CCommand
{
public:
	CmdShutdown()  { m_gsName = "Shutdown"; m_gsClass = "CmdShutdown"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdCreateSector : public CCommand
{
public:
	CmdCreateSector()  { m_gsName = "CreateSector"; m_gsClass = "CmdCreateSector"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdSector : public CCommand
{
public:
	CmdSector()  { m_gsName = "Sector"; m_gsClass = "CmdSector"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdSpatial : public CCommand
{
public:
	CmdSpatial()  { m_gsName = "Spatial"; m_gsClass = "CmdSpatial"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdTest : public CCommand
{
public:
	CmdTest()  { m_gsName = "Test"; m_gsClass = "CmdTest"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdGoto : public CCommand
{
public:
	CmdGoto()  { m_gsName = "Goto";m_gsClass = "CmdGoto";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdConfig : public CCommand
{
public:
	CmdConfig()  { m_gsName = "Config";m_gsClass = "CmdConfig";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); m_nType = CCommand::T_ADMIN; }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdPromote : public CCommand
{
public:
	CmdPromote()  { m_gsName = "Promote"; m_gsClass = "CmdPromote"; m_AccessReq = CActor::_ADMINISTRATOR; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDemote : public CCommand
{
public:
	CmdDemote()  { m_gsName = "Demote"; m_gsClass = "CmdDemote"; m_AccessReq = CActor::_ADMINISTRATOR; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdModule : public CCommand
{
public:
	CmdModule()  { m_gsName = "Module"; m_gsClass = "CmdModule"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdCrews : public CCommand
{
public:
	CmdCrews()  { m_gsName = "Crews"; m_gsClass = "CmdCrews"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdMotd : public CCommand
{
public:
	CmdMotd()  { m_gsName = "Motd"; m_gsClass = "CmdMotd"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdRace : public CCommand
{
public:
	CmdRace()  { m_gsName = "Race"; m_gsClass = "CmdRace"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdHullcube : public CCommand
{
public:
	CmdHullcube()  { m_gsName = "Hullcube"; m_gsClass = "CmdHullcube"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdFreeze : public CCommand
{
public:
	CmdFreeze()  { m_gsName = "Freeze"; m_gsClass = "CmdFreeze"; m_AccessReq = CActor::_ASSISTANT; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

#endif
