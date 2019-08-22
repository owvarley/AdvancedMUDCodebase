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

#if !defined __CMDSSPACE_H__
#define __CMDSSPACE_H__

#include "MudCore.h"
#include "Command.h"

class CmdWarp : public CCommand
{
public:
	CmdWarp()  { m_gsName = "Warp";m_gsClass = "CmdWarp";  m_AccessReq = CActor::_ADMINISTRATOR; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdPower : public CCommand
{
public:
	CmdPower()  { m_gsName = "Power";m_gsClass = "CmdPower";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdSystems : public CCommand
{
public:
	CmdSystems()  { m_gsName = "Systems";m_gsClass = "CmdSystems";  m_AccessReq = CActor::_NPC; m_Flags->InitSet();
	m_Flags->SetBit( CCommand::_PRESERVE_COMMAND ); m_Aliases.push_back("DRIVES"); m_Aliases.push_back("WEAPONS");
	m_Aliases.push_back("GRAVWELLS"); m_Aliases.push_back("ECM"); m_Aliases.push_back("REPULSORS");
	m_Aliases.push_back("CAPACITORS");}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdSpeed : public CCommand
{
public:
	CmdSpeed()  { m_gsName = "Speed";m_gsClass = "CmdSpeed";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdHyper : public CCommand
{
public:
	CmdHyper()  { m_gsName = "Hyper";m_gsClass = "CmdHyper";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdManeuver : public CCommand
{
public:
	CmdManeuver()  { m_gsName = "Course";m_gsClass = "CmdManeuver";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRadar : public CCommand
{
public:
	CmdRadar()  { m_gsName = "Radar";m_gsClass = "CmdRadar";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdSweep : public CCommand
{
public:
	CmdSweep()  { m_gsName = "Sweep";m_gsClass = "CmdSweep";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdComm : public CCommand
{
public:
	CmdComm()  { m_gsName = "Comm";m_gsClass = "CmdComm";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdShield : public CCommand
{
public:
	CmdShield()  { m_gsName = "Shield"; m_gsClass = "CmdShield";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdLaunch : public CCommand
{
public:
	CmdLaunch()  { m_gsName = "Launch"; m_gsClass = "CmdLaunch";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdScm : public CCommand
{
public:
	CmdScm()  { m_gsName = "Scm"; m_gsClass = "CmdScm";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRoll : public CCommand
{
public:
	CmdRoll()  { m_gsName = "Roll"; m_gsClass = "CmdRoll";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdLand : public CCommand
{
public:
	CmdLand()  { m_gsName = "Land"; m_gsClass = "CmdLand";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdStatus : public CCommand
{
public:
	CmdStatus()  { m_gsName = "Status";m_gsClass = "CmdStatus";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdPilot : public CCommand
{
public:
	CmdPilot()  { m_gsName = "Pilot";m_gsClass = "CmdPilot";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdMan : public CCommand
{
public:
	CmdMan()  { m_gsName = "Man";m_gsClass = "CmdMan";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdFire : public CCommand
{
public:
	CmdFire()  { m_gsName = "Fire";m_gsClass = "CmdFire";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdFcs : public CCommand
{
public:
	CmdFcs()  { m_gsName = "Fcs";m_gsClass = "CmdFcs";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdTarget : public CCommand
{
public:
	CmdTarget()  { m_gsName = "Target";m_gsClass = "CmdTarget";  m_AccessReq = CActor::_NPC; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};


#endif