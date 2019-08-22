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


#ifndef __COMMAND_H__
#define __COMMAND_H__

#include <vector>

#include "MudCore.h"
#include "gString.h"
#include "Actor.h"
#include "Set.h"

#pragma warning(disable: 4786)
#pragma warning(disable: 4251)

class CActor;


class _export CCommand
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CGameWorld;

// Members
public:
	CCommand();
	virtual ~CCommand();

	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////
	typedef enum e_CommandFlags
	{
		_DISABLED			= 0,
		_PRESERVE_COMMAND	= 1,		// Appends m_gsName to the head of the argument list
		_LOGGED				= 2
	};


	typedef enum e_CommandTypes
	{
		T_ADMIN				= 0,
		T_BUILDING			= 1,
		T_INFO				= 2,
		T_SPACE				= 3,
		T_COMBAT				= 4
	};

	static char *szCTypes[];

	// Some useful operator overrides
	bool	operator				== ( CCommand c2 );
	bool	operator				!= ( CCommand c2 );

	///////////////////////////////////////////////////////////////////////////////////////
	// Required Instantiation methods
	///////////////////////////////////////////////////////////////////////////////////////
	// CCommand*	Create_CCommand()	{ return new CCommand; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline access methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline  gString					Name()	const { return m_gsName; }
	inline  gString					Class() const { return m_gsClass; }
	inline  CSet*&					Flags() { return m_Flags; }
	inline  CActor::e_ActorFlags&	AccessReq() { return m_AccessReq; }
	inline  gStringList&			Aliases() { return m_Aliases; }
	inline  bool					PreserveCommand() const { return m_Flags->IsSet(_PRESERVE_COMMAND); }
	inline  bool					Logged() const { return m_Flags->IsSet(_LOGGED); }
	inline  bool					Disabled() const { return m_Flags->IsSet(_DISABLED); }

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					IsValidCommand(CActor* Ch);
	virtual bool					Perform(CActor* Ch, gStringList& CommandLine) { return false; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream&			operator << ( std::ostream& stream, CCommand& command );
	friend std::istream&			operator >> ( std::istream& stream, CCommand& command );
	
	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);


// Data
protected:

	gString							m_gsName;
	gString							m_gsClass;
	
	int								m_nType;

	gStringList						m_Aliases;

	CActor::e_ActorFlags			m_AccessReq;
	CSet*							m_Flags;
};

typedef std::vector<CCommand*> CommandList;


//
// Command parser class. A game world may have several parsers, each containing
// it's own list of commands. For example, you might have seperate parsers for npc
// commands, player commands, administrative commands, OLC commands, etc.  Each can require
// a different level of security.
//
class CCmdParser
{
public:
	CCmdParser();
	virtual ~CCmdParser();

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions that return constant values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	inline	gString					Name() const { return m_gsName; }
	inline  gString					FileName() const { return m_gsFileName; }
	inline  bool					IsActive() const { return m_bActive; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline helper functions that return commonly requested booleans
	///////////////////////////////////////////////////////////////////////////////////////
	virtual	bool					CanAccess(CActor*);

	///////////////////////////////////////////////////////////////////////////////////////
	// Methods that can be called to set the values of member variables.
	///////////////////////////////////////////////////////////////////////////////////////
	void							SetName(const gString& gsNew) { m_gsName = gsNew; }
	void							SetFileName(const gString& gsNew) { m_gsFileName = gsNew; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Utility Functions
	///////////////////////////////////////////////////////////////////////////////////////
	bool							Init(const gString& gsList);
	void							AddCommand(CCommand* pCmd);
	void							DelCommand(CCommand* pCmd);
	void							DescribeTo(CActor* Ch);

	// The command interpreter
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					Interpret(CActor* pA, gString CommandLine);

	// Serialization routines
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool					Load();
	virtual bool					Save();

	///////////////////////////////////////////////////////////////////////////////////////
	// Stream Access Operators
	///////////////////////////////////////////////////////////////////////////////////////
	friend std::ostream&			operator << ( std::ostream& stream, CCmdParser& parser );
	friend std::istream&			operator >> ( std::istream& stream, CCmdParser& parser );

	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);


protected:
	inline	void					DeActivate() { m_bActive = false; }
	inline  void					Activate()   { m_bActive = true; }
	CCommand*						DetermineCmd(gString gsClass);

// Data
public:
	gString							m_gsName;
	gString							m_gsFileName;

	CSet*							m_AccessFlags;
	CommandList						m_Commands;

protected:
	bool							m_bActive;

};

typedef std::vector<CCmdParser*> CmdParsers;

#endif

