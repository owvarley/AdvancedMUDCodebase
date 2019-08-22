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

#if !defined __CMDSDESIGN_H__
#define __CMDSDESIGN_H__

#include "MudCore.h"
#include "Command.h"


class _export CmdDesigns : public CCommand
{
public:
	CmdDesigns()  { m_gsName = "Designs"; m_gsClass = "CmdDesigns"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDStart : public CCommand
{
public:
	CmdDStart()  { m_gsName = "Start"; m_gsClass = "CmdDStart"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDStop : public CCommand
{
public:
	CmdDStop()  { m_gsName = "Stop"; m_gsClass = "CmdDStop"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); m_Aliases.push_back("STOP");}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDEnter : public CCommand
{
public:
	CmdDEnter()  { m_gsName = "Enter"; m_gsClass = "CmdDEnter"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDExit : public CCommand
{
public:
	CmdDExit()  { m_gsName = "Exit"; m_gsClass = "CmdDExit"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDEdit : public CCommand
{
public:
	CmdDEdit()  { m_gsName = "Edit"; m_gsClass = "CmdDEdit"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDInstall : public CCommand
{
public:
	CmdDInstall()  { m_gsName = "Install"; m_gsClass = "CmdDInstall"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDUninstall : public CCommand
{
public:
	CmdDUninstall()  { m_gsName = "Uninstall"; m_gsClass = "CmdDUninstall"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDSave : public CCommand
{
public:
	CmdDSave()  { m_gsName = "Save"; m_gsClass = "CmdDSave"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDFinish : public CCommand
{
public:
	CmdDFinish()  { m_gsName = "Finish"; m_gsClass = "CmdDFinish"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDList : public CCommand
{
public:
	CmdDList()  { m_gsName = "List"; m_gsClass = "CmdDList"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDShow : public CCommand
{
public:
	CmdDShow()  { m_gsName = "Show"; m_gsClass = "CmdDDesigns"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDLink : public CCommand
{
public:
	CmdDLink()  { m_gsName = "Link"; m_gsClass = "CmdDLink"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDUnlink : public CCommand
{
public:
	CmdDUnlink()  { m_gsName = "Unlink"; m_gsClass = "CmdDUnlink"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDGoto : public CCommand
{
public:
	CmdDGoto()  { m_gsName = "Goto"; m_gsClass = "CmdDGoto"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDReview : public CCommand
{
public:
	CmdDReview()  { m_gsName = "Review"; m_gsClass = "CmdDReview"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class _export CmdDClone : public CCommand
{
public:
	CmdDClone()  { m_gsName = "Clone"; m_gsClass = "CmdDClone"; m_AccessReq = CActor::_STAFF; m_Flags->InitSet(); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

#endif