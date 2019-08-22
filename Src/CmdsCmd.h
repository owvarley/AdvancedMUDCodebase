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
// This file contains Command Commands
//

#if !defined __CMDSCMD_H__
#define __CMDSCMD_H__

#include "MudCore.h"
#include "Command.h"

class CmdCommands : public CCommand
{
public:
	CmdCommands()  { m_gsName = "Commands";m_gsClass = "CmdCommands";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();  m_Aliases.push_back(":"); }
	bool Perform(CActor* Ch, gStringList& CommandLine);
};


#endif

