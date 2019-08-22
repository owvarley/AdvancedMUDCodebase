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
// Script Parser Class
//

#include "MudCore.h"
#include "Parser.h"


CParser* CParserFactory::Create(const gString& gsName)
{

#if (HAVE_PYTHON)
	if (gsName.CompareNoCase("PYTHON") == 0)
		return new CPython;
#endif

#if (HAVE_TCL)
	if (gsName.CompareNoCase("TCL") == 0)
		return new CTcl;
#endif

	return NULL;
}

