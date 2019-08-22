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

#ifndef __RESET_H__
#define __RESET_H__

#include <vector>
#include "MudCore.h"
#include "../gTools/TinyXml.h"



class CReset
{
// Members
public:

	CReset() { m_iVnum = -1; }
	~CReset() {};

	typedef enum e_ResetType
	{
		_OBJECT	= 0,
		_NPC	= 1,
		_NUMRESETTYPES
	};

	static char* szResetTypes[_NUMRESETTYPES];

	inline	e_ResetType&	Type()	{ return m_Type; }
	inline  int&			Vnum()	{ return m_iVnum; }
	inline	int*			Range() { return m_iRange; }

	// Serialization
	void					WriteXml(TiXmlNode* pParent);
	void					ReadXml(TiXmlNode* pParent);


// Data
public:

	e_ResetType				m_Type;			// Type of reset
	int						m_iVnum;		// Item to reset
	int						m_iRange[2];	// Min/Max # of m_iVnums to allow in room

};

typedef std::vector<CReset*> ResetList;

#endif