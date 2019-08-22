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

#include "Polar.h"
#include "GameObjects.h"
#include <math.h>

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Polar Class
///////////////////////////////////////////////////////////////////////////////////////////

// Constructor
CPolar::CPolar()
{
	q = 0;
	f = 0;
	r = 0;
}

CPolar::CPolar(float fF, float fQ, long nR, bool dummy)
{
	q = fQ;
	f = fF;
	r = nR;
}

CPolar::CPolar(long double nX, long double nY, long double nZ)
{
	q = nX;
	f = nY;
	r = nZ;

	// Convert Cartesians into Polars
	q = GetX();
	f = GetY();
	r = GetZ();

}

// Deconstructor
CPolar::~CPolar()
{
	q = 0;
	f = 0;
	r = 0;
}

// Returns the Arc that pTo occupies in reference to pFrom
int CPolar::Arc(CPolar * pFrom, CPolar* pTo)
{

	return NULL;
}

// Returns a Bearing of pFrom to pTo
CPolar* CPolar::Bearing(CPolar* pFrom, CPolar* pTo)
{
	
	return NULL;
}

// Returns the distance between two objects
long double CPolar::Distance(CPolar* pFrom, CPolar* pTo)
{
	// Working out the distance
	// ||u - v||
	// Need to convert to cartesians

	long  a = pFrom->GetX();
	long  b = pFrom->GetY();
	long  c = pFrom->GetZ();
	long  d = pTo->GetX();
	long  e = pTo->GetY();
	long  f = pTo->GetZ();

	long nAns = sqrt( abs((a - d) * (a - d)) + abs((b - e) * (b - e)) + abs((c - f) * (c - f)) );

	return nAns;
}

// Method     :: GetX
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: Cartesian value for X
// Function   :: Works out the Cartesian representation of the Polar
//
long double CPolar::GetX()
{
	// X = r sin F cos Q
	float nSinF;
	float nCosQ;

	// Convert angles to radians
	long double nFRad = (f * 3.14) / 180;
	long double nQRad = (q * 3.14) / 180;

	// Work out separate values
	nSinF = sin(nFRad);
	nCosQ = cos(nQRad);

	long double nAns = (long double)r * nSinF * nCosQ;

	return nAns;

}

// Method     :: GetY
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: Cartesian value for Y
// Function   :: Works out the Cartesian representation of the Polar
//
long double CPolar::GetY()
{
	// Y = r sin F sin Q
	float nSinF;
	float nSinQ;

	// Convert angles to radians
	long double nFRad = (f * 3.14) / 180;
	long double nQRad = (q * 3.14) / 180;

	// Work out separate values
	nSinF = sin(nFRad);
	nSinQ = sin(nQRad);

	long double nAns = (long double)r * nSinF * nSinQ;

	return nAns;

}

// Method     :: GetZ
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: Cartesian value for Z
// Function   :: Works out the Cartesian representation of the Polar
//
long double CPolar::GetZ()
{
	// Z = r cos F
	float nCosF;
	
	// Convert angles to radians
	long double nFRad = (f * 3.14) / 180;
	
	// Work out separate values
	nCosF = sin(nFRad);
	
	long double nAns = (long double)r * nCosF;

	return nAns;

}

// Method     :: Move
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: Success
// Function   :: Moves the Object through space
//
bool CPolar::Move(int nR)
{
	if (nR > 0)
	{
		this->r += nR;
		return true;
	}
	

	return false;

}

// Method     :: Course
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: Success
// Function   :: Increases the F and Q value of the object
//
bool CPolar::Course(float fnF, float fnQ)
{

	// Working with F first
	this->f += fnF;
	this->q += fnQ;

	if (this->f > 180)
		this->f = this->f - 360;

	if (this->q > 180)
		this->q = this->q - 360;

	if (this->f < -180)
		this->f = 360 + this->f;

	if (this->q < -180)
		this->q = 360 + this->q;
		

	return true;

}


bool CPolar::Load()
{
	return true;
}

bool CPolar::Save()
{
	return true;
}

// Method     :: <<
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the << write
//				 operator is used. 

std::ostream& operator << ( std::ostream& stream, const CPolar& polar )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Polar]");
	Tools.WriteLn(stream, " F                : %f", polar.f);
	Tools.WriteLn(stream, " Q                : %f", polar.q);
	Tools.WriteLn(stream, " R                : %d", polar.r);
	Tools.WriteLn(stream, "[/Polar]");
	return stream;
}

// Method     :: >>
// Class	  :: CPolar
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the >> read
//				 operator is used for this class. 

std::istream& operator >> ( std::istream& stream, CPolar& polar )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	bool bDone = false;
	
	try
	{
		if ( Tools.ReadKey(stream) == "[Polar]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Polar]"  )
				switch ( gsKey[0] )
				{
					case 'F':
						if ( gsKey == "F" )
							Tools.ReadData(stream, polar.f);
						break;
					case 'Q':
						if ( gsKey == "Q" )
							Tools.ReadData(stream, polar.q);
						break;
					case 'R':
						if ( gsKey == "R" )
							Tools.ReadData(stream, polar.r);
						break;

					default:
						g_Log.Log(LOG_ERROR, "[CPolar::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Polar]");
			}
		}
	}
	catch (...)
	{
	}

	return stream;

}


///////////////////////////////////////////////////////////////////////////////////////////
// 2. PolarBound Class
///////////////////////////////////////////////////////////////////////////////////////////

CPolarBound::CPolarBound()
{
	area = 0;
	tLb = new CPolar;
	tLf = new CPolar;
	tRb = new CPolar;
	tRf = new CPolar;
	bLb = new CPolar;
	bLf = new CPolar;
	bRb = new CPolar;
	bRf = new CPolar;

}

CPolarBound::CPolarBound(CPolar* ptRb, CPolar* ptRf, CPolar* ptLb, CPolar* ptLf, CPolar* pbRb, CPolar* pbRf, CPolar* pbLb, CPolar* pbLf)
{
	area = 0;
	tLb = ptLb;
	tLf = ptLf;
	tRb = ptRb;
	tRf = ptRf;
	bLb = pbLb;
	bLf = pbLf;
	bRb = pbRb;
	bRf = pbRf;
}

CPolarBound::~CPolarBound()
{
	delete tLb;
	delete tLf;
	delete tRb;
	delete tRf;
	delete bLb;
	delete bLf;
	delete bRb;
	delete bRf;
}

void CPolarBound::CalculateArea()
{
	// To calculate area we need to have our polars in cartesians.
	// This will only be a rough estimation of the area

	// Technically this is the volume so we need three lengths:
	// lTb -> lTf
	// lTf -> lBf
	// lTf -> rTf

	long double nA = tLb->Distance(tLb, tLf);
	long double nB = tLb->Distance(tLf, bLf);
	long double nC = tLb->Distance(tLf, tRf);


	area = nA * nB * nC;

	return;
}

bool CPolarBound::Contains(CPolar* pPolar)
{
	return false;
}



// Method     :: <<
// Class	  :: CPolarBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the << write
//				 operator is used. 

bool CPolarBound::Load()
{
	return true;
}

// Method     :: <<
// Class	  :: CPolarBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the << write
//				 operator is used. 

bool CPolarBound::Save()
{
	return true;
}

// Method     :: <<
// Class	  :: CPolarBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the << write
//				 operator is used. 

std::ostream& operator << ( std::ostream& stream, const CPolarBound& polarbound )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[PolarBound]");
	Tools.WriteLn(stream, " Area		   : %d", polarbound.area);
	stream << *polarbound.tRf;
	stream << *polarbound.tRb;
	stream << *polarbound.tLb;
	stream << *polarbound.tLf;
	stream << *polarbound.bRb;
	stream << *polarbound.bRf;
	stream << *polarbound.bLb;
	stream << *polarbound.bLf;
	Tools.WriteLn(stream, "[/PolarBound]");
	return stream;
}

// Method     :: >>
// Class	  :: CPolarBound
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the >> read
//				 operator is used for this class. 

std::istream& operator >> ( std::istream& stream, CPolarBound& polarbound )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	streampos marker;
	int nCount = 0;
	bool bDone = false;
		
	try
	{
		if ( Tools.ReadKey(stream) == "[PolarBound]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();

				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/PolarBound]"  )
				switch ( gsKey[0] )
				{
					case 'A':
						if ( gsKey == "Area" )
							Tools.ReadData(stream, polarbound.area);
						break;
					case '[':
						if ( gsKey == "[Polar]" )
						{
							
							CPolar* pPolar = new CPolar;
							
							stream.seekg(marker);

							stream >> *pPolar;

							switch (nCount)
							{
								case 0: polarbound.tRf = pPolar;
									break;
								case 1: polarbound.tRb = pPolar;
									break;
								case 2: polarbound.tLf = pPolar;
									break;
								case 3: polarbound.tLb = pPolar;
									break;
								case 4: polarbound.bRf = pPolar;
									break;
								case 5: polarbound.bRb = pPolar;
									break;
								case 6: polarbound.bLf = pPolar;
									break;
								case 7: polarbound.bLb = pPolar;				
									break;
								default: 
									break;
							}

							nCount++;
						

						}
						break;

					default:
						g_Log.Log(LOG_ERROR, "[CPolarBound::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/CPolarBound]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CPolarBound::>>] Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CPolarBound::>>] Error encountered while reading PolarBounds..");
	}

	return stream;
}