//                                __.             
//                               (__.    ,_ ._.._ 
//                               .__)\/\/(/,[  [_)
//                                             |  
//
// Interpreted by Owen Varley [Nekekami] :: <o.w.varley#dur.ac.uk>
//
// Durham   :: CS Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Header   :: Tools.h
// Function :: Handles the classes a variety of Handy objects

#ifndef __OTOOLS_H__
#define __OTOOLS_H__

#include "MudCore.h"
#include "Spatial.h"

#pragma warning(disable: 4251)

class COTools
{

public:
	COTools();		// Constructor
	~COTools();		// Deconstructor

	// Tech Level specific values
	int DistanceSeparation(int nTechlevel);
	int HCF(int nOne, int nTwo);

};




#endif