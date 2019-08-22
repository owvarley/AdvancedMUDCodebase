//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: CPosition
// Header   :: OTools.h
// Function :: Handles the use of Morton Codes to create storage lists for objects
//			   and shape of objects.
// Update   :: Morton Codes were removed from the Project in favour or a less intensive
//			:: method of Spatial object storage. The implementation remains for those that
//			:: wish to look at it or use it.

#pragma warning(disable: 4786)

#include "OTools.h"

CPosition::CPosition()
{
	m_nHighMC = 0;
	m_pNext = NULL;

}

CPosition::~CPosition()
{
	m_nHighMC = 0;
	m_Contents.clear();
	m_pNext = NULL;
}


// Calculates the High MC
unsigned int CPositionMap::HighMC(int x, int y)
{
	int i; 
	unsigned int nHigh = 0; 

	// We can't use negative values hence we need to fix all numbers.
	// To do this we need to add 2^31 to all positive numbers
	unsigned int nX = (unsigned int)(x + 2147483648);
	unsigned int nY = (unsigned int)(y + 2147483648);

	nX = nX >> 16; 
	nY = nY >> 16; 

	for (i = 0; i < 16; i++) 
	{ 
		nHigh |= ((1 << i) & nY) << (i); 
		nHigh |= ((1 << i) & nX) << (i + 1); 
	} 

	return nHigh; 
}

// Calculates the Low MC 
unsigned int CPositionMap::LowMC(int x, int y)
{
	int i; 
	unsigned int nLow = 0; 
	// We can't use negative values hence we need to fix all codes by 2^31
	unsigned int nX = (unsigned int)(x + 2147483648);
	unsigned int nY = (unsigned int)(y + 2147483648);
	
	for (i = 0; i < 16; i++) 
	{ 
		nLow |= ((1 << i) & nY) << (i); 
		nLow |= ((1 << i) & nX) << (i + 1); 
	} 

	return nLow; 
}


// Position Map class

CPositionMap::CPositionMap()
{

}

CPositionMap::~CPositionMap()
{
	m_Spatial.clear();
}

// Function to remove an object from the Map, using Morton Codes
// Tested ~Nek 05/09/05
bool CPositionMap::Erase(CSpatial *pSpatial)
{
	// To remove an Object from our map we need to locate its Position object using the
	// Object's Morton Codes. Then we consider the Position object, if it is the only object
	// within the list then we can just remove the entire Position object. If its not we 
	// need to modify the Contents list to remove it
	SpatialList::iterator spa;

	// Work out our two Morton Codes
	int nLowMC = this->LowMC(pSpatial->m_Location->x, pSpatial->m_Location->y);
	int nHighMC = this->HighMC(pSpatial->m_Location->x, pSpatial->m_Location->y);

	// Find where this object is
	CPosition *pPosition;
	CPosition *pFound = NULL;
	
	// Now we need to carry out the second hash to find our list of objects with matching high MC
	for (pPosition = (*m_Spatial.find(nLowMC)).second; pPosition != NULL; pPosition = pPosition->m_pNext)
	{
		// We found it
		if (pPosition->m_nHighMC == nHighMC)
		{
			pFound = pPosition;
		}
	}

	if (pFound != NULL)
	{
		// Is this object the only one at this position?
		if (pFound->m_Contents.size() == 1)
		{
			m_Spatial.erase(nLowMC);
			return true;
		}

		// If its not we need to find the object in the Contents list

		for (spa = pFound->m_Contents.begin(); spa != pFound->m_Contents.end(); spa++)
		{

			CSpatial* pDelete = ***spa;

			if (!pDelete)
			{
				pFound->m_Contents.erase(spa);
				continue;
			}

			// We have found the iterator position so we can delete the object
			if (pDelete && pDelete == pSpatial)
				pFound->m_Contents.erase(spa);

		}
	}

	return true;
}


// Inserts a value into the Map using Morton Codes
// Tested ~Nek 04/09/05
bool CPositionMap::Insert(CSpatial *pSpatial)
{
	// To add an Object to the Map we have to calculate the Morton Code for the Object
	// then we must locate the Postion object to store it within. If an object already
	// exists then we simply push our new spatial object to the back of the Contents list.
	// If there is no object at that position then we need to create one.
	
	// Work out our two Morton Codes
	unsigned int nLowMC = this->LowMC(pSpatial->m_Location->x, pSpatial->m_Location->y);
	unsigned int nHighMC = this->HighMC(pSpatial->m_Location->x, pSpatial->m_Location->y);

	// See if we already have a key in the map
	if (this->m_Spatial.find(nLowMC) == this->m_Spatial.end())
	{
		// We dont so lets make one
		CPosition* pPosition = new CPosition();
		pPosition->m_nHighMC = nHighMC;
		pPosition->m_Contents.push_back(pSpatial->m_Vnum);
		pPosition->m_pNext = NULL;

		this->m_Spatial.insert(PositionMap::value_type(nLowMC, pPosition));
	}
	else
	{
		// For the second hash check we need to get the first Position object at this location
		// and check its high MC. If its equal to our high MC then we add our object to the end
		// of its contents list. If not we check its next pointer and so on. If we do not find
		// a matching high MC then we need to add one to the end of the list.		
		CPosition *pPosition;
		bool bFound = false;

		// Carry out the Second hash to see if we can find a matching High MC
		for (pPosition = (*m_Spatial.find(nLowMC)).second; pPosition != NULL; pPosition = pPosition->m_pNext)
		{
			// We found one!
			if (pPosition->m_nHighMC == nHighMC)
			{
				pPosition->m_Contents.push_back(pSpatial->m_Vnum);
				bFound = true;
			}
		}		

		// We didn't find one
		if (!bFound)
		{
			CPosition* pNew = new CPosition();	// Create our new object
			pNew->m_nHighMC = nHighMC;			// Set its high MC
			pNew->m_pNext = NULL;				// It is now the tail of the list
			pPosition->m_pNext = pNew;			// We need to add it to the tail
		}
			

	}
	
	return true;
}

// Search a the Map for a specific Morton Code
// Tested ~Nek 04/09/05
SpatialList* CPositionMap::Search( int nMC ) 
{ 
	SpatialList* FoundList = new SpatialList(); 
	CPosition* pPos;
	SpatialList::iterator spa;

	if ( this->m_Spatial.find(nMC) != this->m_Spatial.end() ) 
		for ( pPos = this->m_Spatial[nMC]; pPos; pPos = pPos->m_pNext ) 
			for ( spa = pPos->m_Contents.begin(); spa != pPos->m_Contents.end(); spa++ ) 
				FoundList->push_back(*spa);	// Add it to our found list


	// Memory Leak Fix.
	if ( FoundList->empty() ) 
	{ 
		delete FoundList;  
		return NULL; 
	} 

		return FoundList;  
}

// Get the List of Objects within a specfic range
// Tested ~Nek 04/09/05
SpatialList* CPositionMap::GetRange(int nX, int nY, int nZ, int nRange)
{
	SpatialList *SpatialObjs = new SpatialList();
	SpatialList *FoundList;
	SpatialList::iterator spa;

	int nLowX, nLowY, nHighX, nHighY;
	unsigned int nLowMC, nHighMC;

	// Work out our boundary values
	nLowX = nX - nRange;
	nLowY = nY - nRange;
	nHighX = nX + nRange;
	nHighY = nY + nRange;

	// Obtain our Morton Codes
	nLowMC = LowMC(nLowX, nLowY);		
	nHighMC = LowMC(nHighX, nHighY);

	// Iterate through all the possible locations using lowMC and highMc
	for (unsigned int i = nLowMC; i <= nHighMC; i++)
	{
		if ( (FoundList = this->Search(i) ) != NULL)
		{
			for (spa = FoundList->begin(); spa != FoundList->end(); spa++)
				if ( (*spa) != NULL)
					SpatialObjs->push_back(*spa);

			delete FoundList;
		} 
	} 

	// Memory Leak Fix.
	if ( SpatialObjs->empty() ) 
	{ 
		delete SpatialObjs; 
		return NULL; 
	} 

	return SpatialObjs; 
	
}

gString CPositionMap::Binary(int x, int bits) 
{ 
   return "";
}
