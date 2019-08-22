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

// Header   :: Cartesian.h
// Function :: Handles the classes for the 3D Cartesian Coordinates system

#ifndef __CARTESIAN_H__
#define __CARTESIAN_H__

#include "MudCore.h"

#pragma warning(disable: 4251)

class CCart {

public:
	CCart();
	CCart(long double nX, long double nY, long double nZ);
	~CCart();

	// Conversion Functions
	float										Bear(); // Polar Cords: Q
	float										Mark(); // F
	long double									Dist(); // R

	// Mathematical Functions
	long double			  Distance(CCart* pF, CCart* pT);			// Determines distance between two cords
	float				   Bearing(CCart* pF, CCart* pT, bool bB);  // Works out the Bearing of one cord from another
	int						   Arc(CCart* pF, CCart* pT);			// The arc of one cord from another
	bool				Course(float fnF, float fnQ);				// Change the F and Q values
	long double 		GetX(float f, float q, float r);							// Converts Polar to Cartesian
	long double 		GetY(float f, float q, float r);							// Converts Polar to Cartesian
	long double 		GetZ(float f, float r);							// Converts Polar to Cartesian

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load();
	virtual bool	Save();
		
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CCart& cartesian );
	friend std::istream& operator >> ( std::istream& stream, CCart& cartesian );
	

public:

	long x;
	long y;
	long z;

};


class CCartBound 
{

public: 
	CCartBound();
	CCartBound(CCart* tLb, CCart* tLf, CCart* tRb, CCart* tRf, CCart* bLb, CCart* bLf, CCart* bRb, CCart* bRf);
	~CCartBound();

	friend class CCart;

	// Inline Functions
	inline CCart* TLB()		{ return tLb;  }		
	inline CCart* TLF()		{ return tLf;  }
	inline CCart* TRB()		{ return tRb;  }
	inline CCart* TRF()		{ return tRf;  } 
	inline CCart* BLB()		{ return bLb;  }
	inline CCart* BLF()		{ return bLf;  }
	inline CCart* BRB()		{ return bRb;  }
	inline CCart* BRF()		{ return bRf;  }
	inline long double Area()			{ return area; }

	// Method calls
	void CalculateArea();				// Calculates the area contained within the shape
	bool Contains(CCart* pCart);		// Works out if a Cart coord is contained within the shape

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load();
	virtual bool	Save();
		
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CCartBound& polarbound );
	friend std::istream& operator >> ( std::istream& stream, CCartBound& polarbound );


public:
	long area;			// Area covered

private:
	CCart* tLb;			// Top Left Back
	CCart* tLf;			// Top Left Front
	CCart* tRb;			// Top Right Back
	CCart* tRf;			// Top Right Front
	CCart* bLb;			// Bottom Left Back
	CCart* bLf;			// Bottom Left Front
	CCart* bRb;			// Bottom Right Back
	CCart* bRf;			// Bottom Right Front
	
};

#endif