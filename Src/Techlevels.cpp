//                                __.             
//                               (__.    ,_ ._.._ 
//                               .__)\/\/(/,[  [_)
//                                             |  

//
// Interpreted by Owen Varley [Nekekami] :: <o.w.varley#dur.ac.uk>
// Lead Design :: Ray [Vyraeth], Charlie Van Der Born [Chaz]
// Developed by the SWERP Development Team
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: CPolar
// Header   :: Polar.h 
// Function :: Used for all mathematical Space Based functions, Part of the Tools group.

#include "OTools.h"

COTools::COTools()
{ }

COTools::~COTools()
{ }


// Distance Separation is the minimum distance needed to
// obtain a clear signature reading on a spatial object.
// If the distance is less than this minimum then their
// is a chance the two signals will cause interference with
// each other and give an erroneous signal.
int COTools::DistanceSeparation(int nTechlevel)
{
	switch (nTechlevel)
	{
		case 0:  return 2000; break;
		case 1:  return 2000; break;
		case 2:  return 1750; break;
		case 3:  return 1500; break;
		case 4:  return 1250; break;
		case 5:  return 1000; break;
		case 6:  return 900; break;
		case 7:  return 750; break;
		case 8:  return 500; break;
		case 9:  return 400; break;
		case 10: return 250; break;
		default: return 0; break;

	}

}

// Thanks Monster
int COTools::HCF(int nOne, int nTwo)
{

	if(nOne <= 0 || nTwo <= 0)
		return 0;

	int factor = (nOne < nTwo ? nOne : nTwo);

	while(nOne%factor || nTwo%factor)
		factor--;

	return factor;

}
			