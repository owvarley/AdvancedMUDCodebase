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

// Class    :: CCart
// Header   :: Spatial.h 
// Function :: Used for all mathematical Space Based functions, Part of the Tools group.
// Update   :: Decided to use 3D Cartesians on the Back end and use Cart Conversions for 
//             the front end and some arc formulas

#include "Cart.h"
#include "GameObjects.h"
#include <math.h>
#include "../gTools/Maths.h"
#include "../gTools/Log.h"

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Cartesian Class
///////////////////////////////////////////////////////////////////////////////////////////

// Constructor
CCart::CCart()
{
	x = 0;
	y = 0;
	z = 0;
}

CCart::CCart(long double nX, long double nY, long double nZ)
{
	x = nX;
	y = nY;
	z = nZ;
}

// Deconstructor
CCart::~CCart()
{
	x = 0;
	y = 0;
	z = 0;
}

// Thanks Darrik
float CCart::Bear() // Phi value (Q)
{
	float fPhi = 0.0;
	static double fPi = 3.14159;


	if (this->z > 0)
	{
		fPhi = (atan(double(this->x)/double(this->z)) * 180.00/fPi);
	}
	else if (this->z < 0 && this->x > 0)
	{
		fPhi = (atan(double(this->x)/double(this->z)) * 180.00/fPi) + 180.0;
	}
	else if (this->z < 0 && this->x < 0)
	{
		fPhi = (atan(double(this->x)/double(this->z)) * 180.00/fPi) - 180.0;
	}
	else if (this->z == 0 && this->x > 0)
	{
		fPhi = 90.0;
	}
	else if (this->z == 0 && this->x < 0)
	{
		fPhi = -90.0;
	}

	return fPhi;

}

float CCart::Mark() // Theta Value (F)
{
	float fTheta = 0.0;
	static double fPi = 3.14159;

	if (this->y > 0)
	{
	   fTheta = (atan( sqrt(double(this->z * this->z)+ double(this->x * this->x)) / double(this->y) ) * 180.0/fPi);
	}
	else if (this->y < 0)
	{
	   fTheta = (atan( sqrt(double(this->z * this->z)+ double(this->x * this->x)) / double(this->y) ) * 180.0/fPi) + 180.0;
	}
	else if (this->y == 0)
	{
	   fTheta = 90.0;
	}

	return fTheta;
}

long double CCart::Dist()
{
	return sqrt( (long double)(x*x) + (long double)(y*y) + (long double)(z*z) );
}

// Returns the Arc that pTo occupies in reference to pFrom
int CCart::Arc(CCart * pTo, CCart* pFace)
{
	// Worked out using Bearings and Marks with bounding areas:
	// We need to fix the ship using its heading to 0 0 0

	// Diagrams in logbook

	// We need to take our Heading into Account
	// There are 26 different cases that be applied when trying to locate the
	// arc another cartesian co-ordinate lies in. When the heading rotates it can swap
	// the different axis around, for example in a roll of 180 degrees the Ventral arc
	// becomes the Dorsal one.

	CCart* pNew = new CCart;

	pNew->x = pFace->x;
	pNew->y = pFace->y;
	pNew->z = pFace->z;

	// Work out the angle between O and T
	float cXY, cYZ, cXZ;	
	float aXY = pNew->x;
	float aYZ = pNew->y;
	float aXZ = pNew->z;

	// The way we are going to implement this is by swapping positions within the array
	// dependant upon the Facing angles. If the angle dictates that we swap dorsal for ventral
	// then all we will do is swap the value held in the Dorsal index with that held in the ventral one
	int nArc[] = {CShip::A_FORE, CShip::A_AFT, CShip::A_STARBOARD, CShip::A_PORT, CShip::A_VENTRAL, CShip::A_DORSAL };

	// XY :: First case
	// For = For, Aft = Aft, Ven = Por, Dor = Star
	if (aXY >= 45 && aXY < 135)
	{
		int nTemp = nArc[CShip::A_VENTRAL];
		nArc[CShip::A_VENTRAL] = nArc[CShip::A_PORT];		// Swap Ventral with Port
		nArc[CShip::A_PORT] = nTemp;						// Swap Port with Ventral
		nTemp = nArc[CShip::A_DORSAL];						// Save Dorsal's value
		nArc[CShip::A_DORSAL] = nArc[CShip::A_STARBOARD];	// Swap Dorsal with Starboard
		nArc[CShip::A_STARBOARD] = nTemp;					// Set Starboard's value
	}

	// XY :: Second Case
	// For = For, Aft = Aft, Ven = Dor, Por = Star
	if (aXY >= 135 && aXY < 225)
	{
		int nTemp = nArc[CShip::A_VENTRAL];
		nArc[CShip::A_VENTRAL] = nArc[CShip::A_DORSAL];		// Swap Ventral with Dorsal
		nArc[CShip::A_DORSAL] = nTemp;						// Swap Dorsal with Ventral
		nTemp = nArc[CShip::A_STARBOARD];					// Save Starboard's value
		nArc[CShip::A_STARBOARD] = nArc[CShip::A_PORT];		// Swap Starboard with Port
		nArc[CShip::A_PORT] = nTemp;						// Set Port equal to starboard
	}

	// XY :: Third Case
	// For = For, Aft = Aft, Ven = Star, Dor = Por
	if (aXY >= 225 && aXY <= 315)
	{
		int nTemp = nArc[CShip::A_VENTRAL];
		nArc[CShip::A_VENTRAL] = nArc[CShip::A_STARBOARD];	// Swap Ventral with Starboard
		nArc[CShip::A_STARBOARD] = nTemp;					// Swap Starboard with Ventral
		nTemp = nArc[CShip::A_DORSAL];						// Save Doral's value
		nArc[CShip::A_DORSAL] = nArc[CShip::A_PORT];		// Swap Dorsal with Port
		nArc[CShip::A_PORT] = nTemp;						// Set Port equal to Dorsal
	}
	// END XY

	// YZ :: First Case
	// Port = Port, Star = Star, Fore = Ven, Aft = Dor
	if (aYZ >= 45 && aYZ < 135)
	{
		int nTemp = nArc[CShip::A_VENTRAL];
		nArc[CShip::A_VENTRAL] = nArc[CShip::A_FORE];		// Swap Ventral with Fore
		nArc[CShip::A_FORE] = nTemp;						// Swap Fore with Ventral
		nTemp = nArc[CShip::A_DORSAL];						// Save Doral's value
		nArc[CShip::A_DORSAL] = nArc[CShip::A_AFT];			// Swap Dorsal with Aft
		nArc[CShip::A_AFT] = nTemp;							// Set Aft equal to Dorsal
	}

	// YZ :: Second Case
	// Port = Port, Star = Star, Fore = Aft, Ven = Dor
	if (aYZ >= 135 && aYZ < 225)
	{
		int nTemp = nArc[CShip::A_FORE];
		nArc[CShip::A_FORE] = nArc[CShip::A_AFT];			// Swap Fore with Aft
		nArc[CShip::A_AFT] = nTemp;							// Swap Aft with Fore
		nTemp = nArc[CShip::A_VENTRAL];						// Save Ventral's value
		nArc[CShip::A_VENTRAL] = nArc[CShip::A_DORSAL];		// Swap Ventral with Dorsal
		nArc[CShip::A_DORSAL] = nTemp;						// Set Dorsal equal to Ventral
	}

	// YZ :: Third case
	// Port = Port, Star = Star, Aft = Ven, Fore = Dor
	if (aYZ >= 225 && aYZ < 315)
	{
		int nTemp = nArc[CShip::A_AFT];
		nArc[CShip::A_AFT] = nArc[CShip::A_VENTRAL];		// Swap Aft with Ventral
		nArc[CShip::A_VENTRAL] = nTemp;						// Swap Ventral with Aft
		nTemp = nArc[CShip::A_FORE];						// Save Fore's value
		nArc[CShip::A_FORE] = nArc[CShip::A_DORSAL];		// Swap Fore with Dorsal
		nArc[CShip::A_DORSAL] = nTemp;						// Set Dorsal equal to Fore
	}
	// END YZ

	// XZ :: First case
	// Ven = Ven, Dor = Dor, Fore = Star, Por = Aft
	if (aXZ >= 45 && aXZ < 135)
	{
		int nTemp = nArc[CShip::A_FORE];
		nArc[CShip::A_FORE] = nArc[CShip::A_STARBOARD];		// Swap Fore with Starboard
		nArc[CShip::A_STARBOARD] = nTemp;					// Swap Starboard with Fore
		nTemp = nArc[CShip::A_AFT];							// Save Aft's value
		nArc[CShip::A_AFT] = nArc[CShip::A_PORT];			// Swap aft with Port
		nArc[CShip::A_PORT] = nTemp;						// Set Port equal to aft
	}

	// XZ :: Second case
	// Ven = Ven, Dor = Dor, Fore = Aft, Por = Star
	if (aXZ >= 135 && aXZ < 225)
	{
		int nTemp = nArc[CShip::A_FORE];
		nArc[CShip::A_FORE] = nArc[CShip::A_AFT];			// Swap Fore with Aft
		nArc[CShip::A_AFT] = nTemp;							// Swap Aft with Fore
		nTemp = nArc[CShip::A_PORT];						// Save Port's value
		nArc[CShip::A_PORT] = nArc[CShip::A_STARBOARD];		// Swap Port with Starboard
		nArc[CShip::A_STARBOARD] = nTemp;					// Set Starboard equal to Port
	}

	// XZ :: Third case
	// Ven = Ven, Dor = Dor, Fore = Port, Aft = Star
	if (aXZ >= 225 && aXZ < 315)
	{
		int nTemp = nArc[CShip::A_FORE];
		nArc[CShip::A_FORE] = nArc[CShip::A_PORT];			// Swap Fore with Port
		nArc[CShip::A_PORT] = nTemp;						// Swap Port with Fore
		nTemp = nArc[CShip::A_AFT];							// Save Aft's value
		nArc[CShip::A_AFT] = nArc[CShip::A_STARBOARD];		// Swap Aft with Starboard
		nArc[CShip::A_STARBOARD] = nTemp;					// Set Starboard equal to Aft
	}
	

	// Work out the target's position from the Global cordinates

	// Possible fix
	
	cXY = this->Bearing(pTo, CCart::_XY);
	cYZ = this->Bearing(pTo, CCart::_YZ);
	cXZ = this->Bearing(pTo, CCart::_XZ);

	int nFinalArc = 0;

	if ( (cXZ >= 315.0 || cXZ <= 45.0) && (cYZ >= 315.0 || cYZ <= 45.0))
	{
		// Fore
		nFinalArc = nArc[CShip::A_FORE];
	}

	if ( (cXZ >= 135.0 && cXZ <= 225.0) && (cYZ >= 135.0 && cYZ <= 225.0))
	{
		// Aft
		nFinalArc = nArc[CShip::A_AFT];
	}

	if ( (cXZ >= 45.0 && cXZ <= 135.0) && (cXY >= 45.0 && cXY <= 135.0))
	{
		// Starboard
		nFinalArc = nArc[CShip::A_STARBOARD];
	}
	
	if ( (cXZ >= 225.0 && cXZ <= 315.0) && (cXY >= 225.0 && cXY <= 315.0))
	{
		// Port
		nFinalArc = nArc[CShip::A_PORT];
	}	

	if ( (cYZ >= 225.0 && cYZ <= 315.0) && (cXY >= 135.0 && cXY <= 225.0))
	{
		// Ventral
		nFinalArc = nArc[CShip::A_VENTRAL];
	}

	if ( (cYZ >= 45.0 && cYZ <= 135.0) && (cXY >= 315.0 || cXY <= 45.0))
	{
		// Dorsal
		nFinalArc = nArc[CShip::A_DORSAL];
	}

	return nFinalArc;
}

// Returns a Bearing of this object to pTo assuming this object is heading pHead
float CCart::Bearing(CCart* pTo, CCart* pHead, int nType)
{
	long fX;
	long fY;
	long tX;
	long tY;
	long hA;
	
	static double fDeg = 57.295779;

	if (nType == _XY)
	{
		// XY
		fX = this->x;
		fY = this->y;
		tX = pTo->x;
		tY = pTo->y;
		hA = pHead->x;
	}
	else if (nType == _YZ)
	{
		// YZ
		fX = this->y;
		fY = this->z;
		tX = pTo->y;
		tY = pTo->z;
		hA = pHead->y;
	}
	else if (nType == _XZ)
	{
		// XZ
		fX = this->x;
		fY = this->z;
		tX = pTo->x;
		tY = pTo->z;
		hA = pHead->z;
	}

	
	float fBearing = 0.0;

	// Now we have a Heading to consider, to work this out now we need to determine
	// the angle this heading makes from the 0 axis and use this to determine theta
	// where theta is the angle between your heading and the target ship

	// We need to use Bearing to determine our X and Y.
	// X == The Angle between the 0 Axis and our Target
	// Y == the Angle between the 0 Axis and our heading

	int nX = this->Bearing(pTo, nType);
	int nY = hA;

	fBearing = nX - nY;

	// If its negative add 360 to it
	if (fBearing < 0.0)
		fBearing += 360.0;

	if (fBearing > 360.0)
		fBearing -= 360.0;

	return fBearing; 
} 

// Returns a Bearing of this to pTo
float CCart::Bearing(CCart* pTo, int nType)
{
	long fX;
	long fY;
	long tX;
	long tY;

	static double fDeg = 57.295779;
	static double fPi = 3.14159;

	if (nType == _XY)
	{
		// XY
		fX = this->x;
		fY = this->y;
		tX = pTo->x;
		tY = pTo->y;
	}
	else if (nType == _YZ)
	{
		// YZ
		fX = this->y;
		fY = this->z;
		tX = pTo->y;
		tY = pTo->z;
	}
	else if (nType == _XZ)
	{
		// XZ
		fX = this->x;
		fY = this->z;
		tX = pTo->x;
		tY = pTo->z;
	}

	float fBearing = 0.0;

	//
	//    |
	//  C | A
	// -------
	//  D | B
	//    |
	//
	// In A : Bearing = Theta
	// In B : Bearing = 180 - Theta
	// In C : Bearing = 360 - Theta
	// In D : Bearing = 180 + Theta
	
	// Case 1 :: In A
	if (tX > fX && tY > fY)
		fBearing = atan( float(tX - fX) / float(tY - fY) )* fDeg;
	// Case 2 :: In B
	else if (tX > fX && tY < fY)
		fBearing = 180 - (atan( float(tX - fX) / float(fY - tY) )* fDeg);
	// Case 3 :: In C
	else if (tX < fX && tY > fY)
		fBearing = 360 - (atan( float(fX - tX) / float(tY - fY) )* fDeg);
	// Case 4 :: In D
	else if (tX < fX && tY < fY)
		fBearing = 180 + (atan( float(fX - tX) / float(fY - tY) )* fDeg);
    else if (tY == fY && tX != fX)
	{
		if (tX > fX)
			fBearing = 90.0;
		else 
			fBearing = 270.0;
	}
	else if (tX == fX && tY != fY)
	{
		if (tY > fY)
			fBearing = 0.0;
		else
			fBearing = 180.0;
	}
	
	// Used to be 360 - no clue why
	return fBearing; 
}

// Returns the distance between two objects
long double CCart::Distance(CCart* pTo)
{
	// Working out the distance
	// ||u - v||
	// Need to convert to cartesians

	long  a = this->x;
	long  b = this->y;
	long  c = this->z;
	long  d = pTo->x;
	long  e = pTo->y;
	long  y = pTo->z;

	double nAns = (double)abs((a - d) * (a - d)) + abs((b - e) * (b - e)) + abs((c - y) * (c - y));

	return sqrt(nAns);
}

// Method     :: GetX
// Class	  :: CCart
// Parameters :: <none>
// Return     :: Cartesian value for X
// Function   :: Works out the Cartesian representation from Polar values
//
long CCart::GetX(float f, float q, float r)
{
	// X = r sin Phi cos Theta
	double fPi = 3.14159265359;
	double fDist = double(r);
	double fPhi = double(f)*(fPi/180.00);
	double fTheta = double(q)*(fPi/180.00);
	
	long nAns = (fDist * sin(fPhi) * cos(fTheta));

	return nAns;

}

// Method     :: GetY
// Class	  :: CCart
// Parameters :: <none>
// Return     :: Cartesian value for Y
// Function   :: Works out the Cartesian representation from Polar values
//
long CCart::GetY(float q, float r)
{
	// Y = r sin Theta
	double fPi = 3.14159265359;
	double fDist = double(r);
	double fTheta = double(q)*(fPi/180.00);
	
	long nAns = (fDist * sin(fTheta));

	return nAns;

}

// Method     :: GetZ
// Class	  :: CCart
// Parameters :: <none>
// Return     :: Cartesian value for Z
// Function   :: Works out the Cartesian representation from Polar values
//
long CCart::GetZ(float f, float q, float r)
{
	// Z = r cos Phi cos Theta
	double fPi = 3.14159265359;
	double fDist = double(r);
	double fTheta = double(q)*(fPi/180.00);
	double fPhi = double(f)*(fPi/180.00);

	long nAns = (fDist * cos(fTheta) * cos(fPhi));

	return nAns;

}

// Method     :: Course
// Class	  :: CCart
// Parameters :: <none>
// Return     :: Success
// Function   :: Increases the the angle between our global axis
//
bool CCart::Course(float fChangeX, float fChangeY, float fChangeZ)
{
	// Course is used to change the heading of the ship. The command takes two angles
	// which we use to set the Polar destination for the ship. 

	// Working with F first
	float fX = (float)this->x;
	float fY = (float)this->y;
	float fZ = (float)this->z;

	fX += fChangeX;
	fY += fChangeY;
	fZ += fChangeZ;

	if (fX > 360 || fX < 0)
		fX = 360 - abs(fX);

	if (fY > 360 || fY < 0)
		fY = 360 - abs(fY);

	if (fZ > 360 || fZ < 0)
		fZ = 360 - abs(fZ);
	
	// We are using the Cartesian Object to hold our Polar values
	x = abs(fX);
	y = abs(fY);
	z = abs(fZ);

	return true;

}

// Method     :: Rotate
// Class	  :: CCart
// Parameters :: <none>
// Return     :: Success
// Function   :: Rotates the Cartesian around a Local Cordinate space and by specified angles
//
bool CCart::Rotate(CCart* pLocal, float fX, float fY, float fZ)
{

	// First we need to translate the CCart to the local cordinate system
	// We do this by subtracting the pLocal values from our values
	int nX = this->x - pLocal->x;
	int nY = this->y - pLocal->y;
	int nZ = this->z - pLocal->z;


	// Need a static value for each axis
	int nsX = nX;
	int nsY = nY;
	int nsZ = nZ;

	// We want to assume that all our rotations will be clockwise.
	// They currently rotate anti-clockwise by default so we change this
	// by negating the sign of our angles
	/*fX = -fX;
	fY = -fY;
	fZ = -fZ; */ //Stopped doing this for testing

	// Now we have translated our Cartesian point we can begin to rotate the point
	// we shall assume that the order of rotation will ALWAYS be, X, Y, Z for simplicity

	// Also round each sin/cos up to ensure we get accurate results

	// If there is an X angle lets rotate by it, we do this using a simple rotational
	// matrix. where V is the angle we wish to rotate by
	// [X]   [1,  0,  0]
	// [Y] . [0, cV,-sV]
	// [Z]   [0, sV, cV]
	//
	if (fX != 0.0f)
	{
		// X = (X * 1 ) + (Y * 0) + (Z * 0)
		nX *= 1;
		// Y = (X * 0) + (Y * cV) + (Z * -sV)
		nY = (nsY * Round(cos(fX))) + (nsZ * Round(-sin(fX)));
		// Z = (X * 0) + (Y * sV) + (Z * cV)
		nZ = (nsY * Round(sin(fX))) + (nsZ * Round(cos(fX)));

		// Finish rotate by X
	}

	// Rotation by Y now if there is a Y angle
	// [X]   [ cV,  0, sV]
	// [Y]   [  0,  1,  0]
	// [Z] . [-sV,  0, cV]
	
	if (fY != 0.0f)
	{
		// X = (X * cV) + (Y * 0) + (Z * sV)
		nX = (nsX * Round(cos(fY))) + (nsZ * Round(sin(fY)));
		// Y = (X * 0) + (Y * 1) + (Z * 0)
		nY *= 1;
		// Z = (X * -sV) + (Y * 0) + (Z * cV)
		nZ = (nsX * Round(-sin(fY))) + (nsZ * Round(cos(fY)));

		// Finish rotate by Y
	}

	// Rotation by Z now if there is a Z angle
	// [X]   [cV,-sV,  0]
	// [Y] . [sV, cV,  0]
	// [Z]   [0,   0,  1]
	if (fZ != 0.0f)
	{
		// X = (X * cV) + (Y * -sV) + (Z * 0)
		nX = (nsX * Round(cos(fZ))) + (nsY * Round(-sin(fZ)));
		// Y = (X * sV) + (Y * cV) + (Z * 0)
		nY = (nsX * Round(sin(fZ))) + (nsY * Round(cos(fZ)));
		// Z = (X * 0) + (Y * 0) + (Z * 1)
		nZ *= 1;

		// Finish rotate by Z
	}
	
	// We have finished all the rotation required for this point, now we need to
	// translate it back to its local cordinate space

	nX += pLocal->x;
	nY += pLocal->y;
	nZ += pLocal->z;

	this->x = nX;
	this->y = nY;
	this->z = nZ;
	
	return true;
}




/*bool CCart::Move(CCart* pCart, int nSpeed)
{
	// We need the Unit Size of our vector
	float fX = pCart->x;
	float fY = pCart->y;
	float fZ = pCart->z;

	float nDistance = sqrt( (fX * fX) + (fY * fY) + (fZ * fZ) );

	fX /= nDistance;
	fY /= nDistance;
	fZ /= nDistance;

	float fAddX = fX * nSpeed;
	float fAddY = fY * nSpeed;
	float fAddZ = fZ * nSpeed;

	// If we are going very slowly then there is the case that
	// we would only change our location by a fraction, this rounds
	// down to zero and would allow a ship to have a speed and a course 
	// but not actually move... So we prevent this.
	if (fAddX < 1 && fAddX > 0)
		fAddX = 1;
	else if (fAddX > -1 && fAddX < 0)
		fAddX = -1;

	if (fAddY < 1 && fAddY > 0)
		fAddY = 1;
	else if (fAddY > -1 && fAddY < 0)
		fAddY = -1;

	if (fAddZ < 1 && fAddZ > 0)
		fAddZ = 1;
	else if (fAddZ > -1 && fAddZ < 0)
		fAddZ = -1;

	this->x += fAddX;
	this->y += fAddY;
	this->z += fAddZ;

	return true;
} */


// Updated to take in pCart as the i, j, k parts of
// the heading vector
bool CCart::Move(CCart* pCart, int nSpeed)
{
	float fX = pCart->x;
	float fY = pCart->y;
	float fZ = pCart->z;

	// We must divide by 1000 as when we calculated the
	// heading vector we multiply by 1000 so we can 
	// store the float values in our long fields
	float fAddX = (fX/1000) * nSpeed;
	float fAddY = (fY/1000) * nSpeed;
	float fAddZ = (fZ/1000) * nSpeed;

	// If we are going very slowly then there is the case that
	// we would only change our location by a fraction, this rounds
	// down to zero and would allow a ship to have a speed and a course 
	// but not actually move... So we prevent this.
	if (fAddX < 1 && fAddX > 0)
		fAddX = 1;
	else if (fAddX > -1 && fAddX < 0)
		fAddX = -1;

	if (fAddY < 1 && fAddY > 0)
		fAddY = 1;
	else if (fAddY > -1 && fAddY < 0)
		fAddY = -1;

	if (fAddZ < 1 && fAddZ > 0)
		fAddZ = 1;
	else if (fAddZ > -1 && fAddZ < 0)
		fAddZ = -1;

	this->x += fAddX;
	this->y += fAddY;
	this->z += fAddZ;

	return true;
}

void CCart::SetXYZ(float fPhi, float fTheta)
{
	// We first need to convert our angles from Degrees to Radians
	double fPi = 3.14159265359;
	fPhi *= (fPi/180.00);
	fTheta *= (fPi/180.00);

	// the CCart class stores X, Y, Z as longs
	// hence we will need to multiply all our values by 1000 to get
	// values
	// X(i) is sinfPhi * cos fTheta
	float fX = sin(fPhi) * cos(fTheta); 
	this->x = (sin(fPhi) * cos(fTheta)) * 1000;

	// Y(j) is sin(fTheta)
	float fY = sin(fTheta);
	this->y = sin(fTheta) * 1000;

	// Z(k) is cosfPhi * cosfTheta
	float fZ = cos(fPhi) * cos(fTheta); 
	this->z = (cos(fPhi) * cos(fTheta)) * 1000;

	// Attempted mark fix
	if (fTheta > fPi/2 && fTheta < (3*fPi)/2)
	{
		///this->x = -this->x;
		//this->y = -this->y;
		//this->z = -this->z;
	}
		




	return;
}
	

// Returns the amount this cart will change by, used
// for collision detection and hyperspace gravity wells
int CCart::MoveDist(CCart* pCart, int nSpeed)
{
	// We need the Unit Size of our vector
	float fX = pCart->x;
	float fY = pCart->y;
	float fZ = pCart->z;

	float nDistance = sqrt( (fX * fX) + (fY * fY) + (fZ * fZ) );

	fX /= nDistance;
	fY /= nDistance;
	fZ /= nDistance;

	float fAddX = fX * nSpeed;
	float fAddY = fY * nSpeed;
	float fAddZ = fZ * nSpeed;

	// If we are going very slowly then there is the case that
	// we would only change our location by a fraction, this rounds
	// down to zero and would allow a ship to have a speed and a course 
	// but not actually move... So we prevent this.
	if (fAddX < 1 && fAddX > 0)
		fAddX = 1;
	else if (fAddX > -1 && fAddX < 0)
		fAddX = -1;

	if (fAddY < 1 && fAddY > 0)
		fAddY = 1;
	else if (fAddY > -1 && fAddY < 0)
		fAddY = -1;

	if (fAddZ < 1 && fAddZ > 0)
		fAddZ = 1;
	else if (fAddZ > -1 && fAddZ < 0)
		fAddZ = -1;

	float fFinalX = this->x + fAddX;
	float fFinalY =	this->y + fAddY;
	float fFinalZ = this->z + fAddZ;

	return sqrt( (fFinalX * fFinalX) + (fFinalY * fFinalY) + (fFinalZ * fFinalZ) );
}

bool CCart::Load()
{
	return true;
}

bool CCart::Save()
{
	return true;
}

// Method     :: >>
// Class	  :: CCart
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the >> read
//				 operator is used for this class. 

void CCart::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pCartNode = Tools.InsertXmlChild(pParent, "Cart");

	Tools.WriteXml(pCartNode, "x",		x);
	Tools.WriteXml(pCartNode, "y",		y);
	Tools.WriteXml(pCartNode, "z",		z);		

	return;
}

void CCart::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pCart = pParent->FirstChild("Cart");
	if ( pCart != NULL )
	{
		Tools.ReadXml(pCart, "x",		x);
		Tools.ReadXml(pCart, "y",		y);
		Tools.ReadXml(pCart, "z",		z);	
	}

	return;
}


///////////////////////////////////////////////////////////////////////////////////////////
// 2. CartBound Class
///////////////////////////////////////////////////////////////////////////////////////////

CCartBound::CCartBound()
{
	m_Anchor = new CCart();
	m_nLength = 0;
	m_nWidth = 0;
	m_nHeight = 0;

}

CCartBound::CCartBound(long double nX, long double nY, long double nZ, int nLength, int nWidth, int nHeight)
{
	m_Anchor->x = nX;
	m_Anchor->y = nY;
	m_Anchor->z = nZ;
	m_nLength = nLength;
	m_nWidth = nWidth;
	m_nHeight = nHeight;
}

CCartBound::~CCartBound()
{
	delete m_Anchor;
	m_Anchor = NULL;
	m_nLength = 0;
	m_nWidth = 0;
	m_nHeight = 0;
}

// Works out the area of the region by using the
// length, width and height
long double CCartBound::Area()
{
	return m_nLength * m_nWidth * m_nHeight;
}

bool CCartBound::Contains(CCart* pCart)
{
	long double nX = m_Anchor->x + m_nWidth;
	long double nY = m_Anchor->y + m_nLength;
	long double nZ = m_Anchor->z + m_nHeight;

	if ((pCart->x > m_Anchor->x && pCart->x < nX) && 
		(pCart->y > m_Anchor->y && pCart->y < nY) &&
		(pCart->z > m_Anchor->z && pCart->z < nZ))
		return true;

	return false;
}



// Method     :: Load
// Class	  :: CCartBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: CartBoundaries are never saved in their own file, this method should
//			  :: not be called

bool CCartBound::Load()
{
	return true;
}

// Method     :: Save
// Class	  :: CCartBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: CartBoundaries are never saved in their own file, this method should
//			  :: not be called

bool CCartBound::Save()
{
	return true;
}

// Method     :: ReadXML
// Class	  :: CCartBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: Updated to use TinyXML reads in the CartBound class in XML format
// Written    :: {OWV} 24/05/06

void CCartBound::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pCartBound = pParent->FirstChild("CartBound");
	if ( pCartBound != NULL )
	{
		m_Anchor->ReadXml(pCartBound);

		// Read in the standard data
		Tools.ReadXml(pCartBound, "length",		m_nLength);
		Tools.ReadXml(pCartBound, "width",		m_nWidth);
		Tools.ReadXml(pCartBound, "height",		m_nHeight);
	}

	return;

}

// Method     :: WriteXML
// Class	  :: CCartBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: Updated to use TinyXML, outputs the CartBound class in XML format
// Written    :: {OWV} 24/05/06

void CCartBound::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pCartNode = Tools.InsertXmlChild(pParent, "CartBound");

	m_Anchor->WriteXml(pCartNode);

	// Write the standard data
	Tools.WriteXml(pCartNode, "length",	m_nLength);
	Tools.WriteXml(pCartNode, "width",	m_nWidth);
	Tools.WriteXml(pCartNode, "height",	m_nHeight);

	return;
}
