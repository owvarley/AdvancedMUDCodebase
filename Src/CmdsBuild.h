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

#if !defined __CMDSBUILD_H__
#define __CMDSBUILD_H__

#include "MudCore.h"
#include "Command.h"

class CmdRedit : public CCommand
{
public:
	CmdRedit()  { m_gsName = "Redit";m_gsClass = "CmdRedit";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRCreate : public CCommand
{
public:
	CmdRCreate()  { m_gsName = "Create";m_gsClass = "CmdRCreate";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRShow : public CCommand
{
public:
	CmdRShow()  { m_gsName = "Show";m_gsClass = "CmdRShow";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRName : public CCommand
{
public:
	CmdRName()  { m_gsName = "Name";m_gsClass = "CmdRName";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRSize : public CCommand
{
public:
	CmdRSize()  { m_gsName = "Size";m_gsClass = "CmdRSize";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRBartle : public CCommand
{
public:
	CmdRBartle()  { m_gsName = "Bartle";m_gsClass = "CmdRBartle";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRDesc : public CCommand
{
public:
	CmdRDesc()  { m_gsName = "Desc";m_gsClass = "CmdRDesc";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRFlags : public CCommand
{
public:
	CmdRFlags()  { m_gsName = "Flags";m_gsClass = "CmdRFlags";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRTerrain : public CCommand
{
public:
	CmdRTerrain()  { m_gsName = "Terrain";m_gsClass = "CmdRTerrain";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRExit : public CCommand
{
public:
	CmdRExit()  { m_gsName = "Exit";m_gsClass = "CmdRExit";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRExflags : public CCommand
{
public:
	CmdRExflags()  { m_gsName = "Exflags";m_gsClass = "CmdRExflags";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRVexit : public CCommand
{
public:
	CmdRVexit()  { m_gsName = "Vexit";m_gsClass = "CmdRVexit";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRInstaroom : public CCommand
{
public:
	CmdRInstaroom()  { m_gsName = "Instaroom";m_gsClass = "CmdRInstaroom";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRTurbolift : public CCommand
{
public:
	CmdRTurbolift()  { m_gsName = "Turbolift";m_gsClass = "CmdRTurbolift";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRCover : public CCommand
{
public:
	CmdRCover()  { m_gsName = "Cover";m_gsClass = "CmdRCover";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

class CmdRDig : public CCommand
{
public:
	CmdRDig()  { m_gsName = "Dig";m_gsClass = "CmdRDig";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};

/////////////////////////////////////////////
// PEDIT :: Parent Edit commands
/////////////////////////////////////////////

class CmdPedit : public CCommand
{
public:
	CmdPedit()  { m_gsName = "Pedit";m_gsClass = "CmdPedit";  m_AccessReq = CActor::_STAFF; m_Flags->InitSet();}
	bool Perform(CActor* Ch, gStringList& CommandLine);
};


#endif
