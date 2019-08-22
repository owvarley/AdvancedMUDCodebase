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

// Header   :: Polar.h
// Function :: Handles the classes for the Polar Coordinates system


#ifndef __POLAR_H__
#define __POLAR_H__

#include "MudCore.h"

#pragma warning(disable: 4251)

// Polar Cords
class CPolar
{

public:
	CPolar();
	CPolar(long double fQ, long double fF, long double nR);
	CPolar(float fQ, float fF, long nR, bool dummy);
	~CPolar();

	// Inline Functions
	inline float			Bear()		 { return q; }
	inline float			Mark()		 { return f; }
	inline long		 		Dist()		 { return r; }

	// Functions
	CPolar*				Bearing(CPolar* pFrom, CPolar* pTo);	// Works out the Bearing of one Point to Another
	long double			Distance(CPolar* pFrom, CPolar* pTo);	// Works out the Distance between two points
	int					Arc(CPolar* pFrom, CPolar* pTo);		// Works out the Arc one Point is in referenced to the other
	long double 		GetX();									// Converts Polar to Cartesian
	long double 		GetY();									// Converts Polar to Cartesian
	long double 		GetZ();									// Converts Polar to Cartesian
	bool				Course(float fnF, float fnQ);			// Change the F and Q values
	bool				Move(int r);							// Increases r

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load();
	virtual bool	Save();
		
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CPolar& polar );
	friend std::istream& operator >> ( std::istream& stream, CPolar& polar );
	
private:
	float			q; // Angle with X Phi
	float			f; // Angle with Z Theta
	long 			r; // Distance



};

typedef std::vector<CPolar*>PolarList;

// Bounding 8 Point Shape (Rectangle, Square, etc)
class CPolarBound
{
	
public: 
	CPolarBound();
	CPolarBound(CPolar* tLb, CPolar* tLf, CPolar* tRb, CPolar* tRf, CPolar* bLb, CPolar* bLf, CPolar* bRb, CPolar* bRf);
	~CPolarBound();

	friend class CPolar;

	// Inline Functions
	inline CPolar* TLB()		{ return tLb;  }		
	inline CPolar* TLF()		{ return tLf;  }
	inline CPolar* TRB()		{ return tRb;  }
	inline CPolar* TRF()		{ return tRf;  } 
	inline CPolar* BLB()		{ return bLb;  }
	inline CPolar* BLF()		{ return bLf;  }
	inline CPolar* BRB()		{ return bRb;  }
	inline CPolar* BRF()		{ return bRf;  }
	inline long double Area()			{ return area; }

	// Method calls
	void CalculateArea();				// Calculates the area contained within the shape
	bool Contains(CPolar* pPolar);		// Works out if a Polar coord is contained within the shape

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load();
	virtual bool	Save();
		
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CPolarBound& polarbound );
	friend std::istream& operator >> ( std::istream& stream, CPolarBound& polarbound );


public:
	long area;			// Area covered

private:
	CPolar* tLb;		// Top Left Back
	CPolar* tLf;		// Top Left Front
	CPolar* tRb;		// Top Right Back
	CPolar* tRf;		// Top Right Front
	CPolar* bLb;		// Bottom Left Back
	CPolar* bLf;		// Bottom Left Front
	CPolar* bRb;		// Bottom Right Back
	CPolar* bRf;		// Bottom Right Front
	


};

#endif
