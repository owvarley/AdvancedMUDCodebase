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

// Header   :: Cart.h
// Function :: Handles Cartesians

#ifndef __CART_H__
#define __CART_H__

#include "MudCore.h"
#include "Spatial.h"

#pragma warning(disable: 4251)

class CCart {

public:
	CCart();
	CCart(long double nX, long double nY, long double nZ);
	~CCart();

	enum e_Axis
	{
		_XZ = 0,
		_YZ = 1,
		_XY = 2
	};

	// Conversion Functions
	float										Bear(); // Polar Cords: Theta
	float										Mark(); // Phi
	long double									Dist(); // Distance

	// Mathematical Functions
	long double			Distance(CCart* pT);								// Determines distance between two cords
	int					MoveDist(CCart* pCart, int nSpeed);					// Determines the amount this cart will move by
	float				Bearing(CCart* pT, int nType);						// Works out the Bearing of one cord from another
	float				Bearing(CCart* pT, CCart* pH, int nType);			// Works out the Bearing using a heading
	int					Arc(CCart* pF, CCart* pT);							// The arc of one cord from another
	bool				Course(float fX, float fY, float fZ);				// Change the angle from the axis
	bool				Rotate(CCart* pLocal, float fX, float fY, float fZ);// Rotate our Cartesain Cordinate
	void				SetXYZ(float fPhi, float fTheta);					// Converts a heading into a cartesian one
	long  				GetX(float fPhi, float fTheta, float r);			// Converts Polar to Cartesian
	long  				GetY(float fTheta, float r);						// Converts Polar to Cartesian
	long		 		GetZ(float fPhi, float fTheta, float r);			// Converts Polar to Cartesian
	bool				Move(CCart* pCart, int nSpeed);						// Moves the Spatial object by a 3D Vector

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load();
	virtual bool	Save();
		
	void				WriteXml(TiXmlNode* pParent);
	void				ReadXml(TiXmlNode* pParent);
	

public:

	long x;
	long y;
	long z;

};
typedef std::vector<CCart*>CartList;


class CCartBound 
{

public: 
	CCartBound();
	CCartBound(long double nX, long double nY, long double nZ, int nLength, int nWidth, int nHeight);
	~CCartBound();

	friend class CCart;

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline functions
	///////////////////////////////////////////////////////////////////////////////////////
	inline CCart*	Anchor()	{ return m_Anchor;  }		
	inline int		Length()	{ return m_nLength; }
	inline int		Width()		{ return m_nWidth; }
	inline int		Height()	{ return m_nHeight; }

	///////////////////////////////////////////////////////////////////////////////////////
	// Class methods
	///////////////////////////////////////////////////////////////////////////////////////
	long double		Area();						// Returns the area
	bool			Contains(CCart* pCart);		// Works out if a Cart coord is contained within the shape

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load();
	virtual bool	Save();
		
	void							WriteXml(TiXmlNode* pParent);
	void							ReadXml(TiXmlNode* pParent);



private:
	CCart*		m_Anchor;	// Anchor cartesian
	int			m_nLength;	// Length of the Sector
	int			m_nWidth;	// Width of the Sector
	int			m_nHeight;	// Height of the Sector	
	
};

#endif