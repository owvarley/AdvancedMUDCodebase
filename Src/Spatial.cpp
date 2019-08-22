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

// Class    :: CSpatial, Spatial Object classes
// Header   :: Spatial.h
// Function :: Handles the functions and methods for all Spatial objects

#pragma warning(disable:4786)


#include "GameObjects.h"
#include "Tools.h"
#include <direct.h>
#include "GameServer.h"
#include "GameWorld.h"
#include "../gTools/Log.h"
#include "Spatial.h"


UINT uiUniqueShipID = 0;

char* CSpatial::szTypes[] = { "Planet", "Star", "Asteroid Field", "Debris", "Satellite", "Moon", "Satellite",
							  "Asteroid", "Blackhole", "Ship", "Ordinance", NULL };

// Used to save each Spatial object to its XML file
char* CSpatial::szTypesXML[] = { "Planet", "Star", "AsteroidField", "Debris", "Satellite", "Moon", "Sat",
							  "Asteroid", "Blackhole", "Ship", "Ordinance", NULL };

char* CSpatial::szSignatures[] = { "Heat", "Electromagnetic", "Ion", "Mass", NULL };

///////////////////////////////////////////////////////////////////////////////////////////
// 0. Spatial ID Class
///////////////////////////////////////////////////////////////////////////////////////////
CSpatialID::CSpatialID()
{
	this->m_nVnum = uiUniqueShipID++;
}

CSpatialID::~CSpatialID()
{
	this->m_nVnum = 0;
}

CSpatial* operator *( CSpatialID& pId )
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	// We need to find this spatial object in our map
	gSpatialMap::iterator found = pGalaxy->m_gSpatialMap.find(pId.m_nVnum);

	// Now did we find a ship with this ID?
	if (found != pGalaxy->m_gSpatialMap.end())
	{
		// If so return it
		return (*found).second;
	}
	else
	{
		// Otherwise return null
		return NULL;
	}

	return NULL;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 1. Spatial Class
///////////////////////////////////////////////////////////////////////////////////////////
CSpatial::CSpatial()
{
	m_gsName = "";
	m_gfFileName;
	m_gsSector = "";
	m_gsDescription = "";
	m_nType = 0;
	m_nGravity = 0;
	m_bDelete = false;
	m_Signature[CSpatial::SI_EM] = 0;
	m_Signature[CSpatial::SI_HEAT] = 0;
	m_Signature[CSpatial::SI_ION] = 0;
	m_Signature[CSpatial::SI_MASS] = 0;
	m_Location = new CCart;
	m_Shape = new CShape;
	m_Vnum = new CSpatialID;
}

CSpatial::~CSpatial()
{
	m_gsName = "";
	m_gfFileName = "";
	m_gsSector = "";
	m_gsDescription = "";
	m_nType = 0;
	m_nGravity = 0;
	m_Signature[0];
	delete m_Location;
	m_Location = NULL;
	delete m_Shape;
	m_Shape = NULL;
	delete m_Vnum;
	m_Vnum = NULL;
}

// Override so this method should NEVER be called
void CSpatial::Update()
{

}

// Overrdied
void CSpatial::Write(int n, char *fmt, ...)
{

}


// Override so this method should NEVER be called
void CSpatial::Destroy()
{

}

// Overrided
void CSpatial::Notify(gString gsMsg)
{

}

// Overrided
void CSpatial::NotifySpace(CSpatial* pS, char *f, ...)
{

}	

// Overrided
void CSpatial::NotifySpace(char *f, ...)
{

}

// Overrided
void CSpatial::Damage(int nAmount, int nArc)
{

}

// Returns the Shortest distance from us to them
int CSpatial::Distance(CSpatial *pSpatial)
{
	// The method for working out the shortest distance is dependant upon the
	// type of Shape
	int nMin = 9999999;

	// Check they have a valid shape
	if (!pSpatial->m_Shape || !this->m_Shape)
		return -1;

	// First case is that we are comparing this to another ship shape which is rectangular
	if (pSpatial->m_Shape->m_nType == CShape::ST_RECTANGLE)
	{
		// The arc determines the points we need to reference. This allows us to cut the number 
		// of points we need to rotate and reference down to 4. This means there will only 
		// be 16 references. This allows us to determine a realistic distance from the ship
		// whilst still giving the CPU a break.
		//
		gString gsArc = CShip::szArc[this->m_Location->Arc(pSpatial->m_Location, ((CShip*)this)->m_Heading)];
		gString gsEnemyArc = CShip::szArc[pSpatial->m_Location->Arc(this->m_Location, ((CShip*)pSpatial)->m_Heading)];

		// Now we define four points on our spatial object for reference
		CCart* pA; CCart* pB; CCart* pC; CCart* pD;
		// And the four points on the target object
		CCart* pTa; CCart* pTb; CCart* pTc; CCart* pTd;

		// Get our four points
		if (gsArc == "Fore")
		{
			pA = new CCart(0, this->m_Shape->Height(), this->m_Shape->Length());
			pB = new CCart(0, 0, this->m_Shape->Length());
		    pC = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), this->m_Shape->Length());
			pD = new CCart(this->m_Shape->Width(), 0, this->m_Shape->Length());
		}
		else if (gsArc == "Aft")
		{
			pA = new CCart(0, this->m_Shape->Height(), 0);
			pB = new CCart(0, 0, 0);
			pC = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), 0);
			pD = new CCart(this->m_Shape->Width(), 0, 0);
		}
		else if (gsArc == "Port")
		{
			pA = new CCart(0, this->m_Shape->Height(), 0);
			pB = new CCart(0, 0 ,0);
			pC = new CCart(0, this->m_Shape->Height(), this->m_Shape->Length());
			pD = new CCart(0, 0, this->m_Shape->Length());
		}
		else if (gsArc == "Starboard")
		{
			pA = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), 0);
			pB = new CCart(this->m_Shape->Width(), 0, 0);
			pC = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), this->m_Shape->Length());
			pD = new CCart(this->m_Shape->Width(), 0, this->m_Shape->Length());
		}
		else if (gsArc == "Dorsal")
		{
			pA = new CCart(0, this->m_Shape->Height(), 0);
			pB = new CCart(0, this->m_Shape->Height(), this->m_Shape->Length());
			pC = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), this->m_Shape->Length());
			pD = new CCart(this->m_Shape->Width(), this->m_Shape->Height(), 0);
		}
		else if (gsArc == "Ventral")
		{
			pA = new CCart(0, 0 ,0);
			pB = new CCart(this->m_Shape->Width(), 0, 0);
			pC = new CCart(0, 0, this->m_Shape->Length());
			pD = new CCart(this->m_Shape->Width(), 0, this->m_Shape->Length());
		}

		// Get their four points
		if (gsEnemyArc == "Fore")
		{
			pTa = new CCart(0, pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
			pTb = new CCart(0, 0, pSpatial->m_Shape->Length());
		    pTc = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
			pTd = new CCart(pSpatial->m_Shape->Width(), 0, pSpatial->m_Shape->Length());
		}
		else if (gsEnemyArc == "Aft")
		{
			pTa = new CCart(0, pSpatial->m_Shape->Height(), 0);
			pTb = new CCart(0, 0, 0);
			pTc = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), 0);
			pTd = new CCart(pSpatial->m_Shape->Width(), 0, 0);
		}
		else if (gsEnemyArc == "Port")
		{
			pTa = new CCart(0, pSpatial->m_Shape->Height(), 0);
			pTb = new CCart(0, 0 ,0);
			pTc = new CCart(0, pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
			pTd = new CCart(0, 0, pSpatial->m_Shape->Length());
		}
		else if (gsEnemyArc == "Starboard")
		{
			pTa = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), 0);
			pTb = new CCart(pSpatial->m_Shape->Width(), 0, 0);
			pTc = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
			pTd = new CCart(pSpatial->m_Shape->Width(), 0, pSpatial->m_Shape->Length());
		}
		else if (gsEnemyArc == "Dorsal")
		{
			pTa = new CCart(0, pSpatial->m_Shape->Height(), 0);
			pTb = new CCart(0, pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
			pTc = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), pSpatial->m_Shape->Length());
			pTd = new CCart(pSpatial->m_Shape->Width(), pSpatial->m_Shape->Height(), 0);
		}
		else if (gsEnemyArc == "Ventral")
		{
			pTa = new CCart(0, 0 ,0);
			pTb = new CCart(pSpatial->m_Shape->Width(), 0, 0);
			pTc = new CCart(0, 0, pSpatial->m_Shape->Length());
			pTd = new CCart(pSpatial->m_Shape->Width(), 0, pSpatial->m_Shape->Length());
		}

		CShip* pShip = (CShip*)this;
		CShip* pTarget = (CShip*)pSpatial;

		// Rotate each point by its ship's center
		pA->Rotate(pShip->m_Shape->m_Center, pShip->m_Heading->z, pShip->m_Heading->y, pShip->m_Heading->x);
		pB->Rotate(pShip->m_Shape->m_Center, pShip->m_Heading->z, pShip->m_Heading->y, pShip->m_Heading->x);
		pC->Rotate(pShip->m_Shape->m_Center, pShip->m_Heading->z, pShip->m_Heading->y, pShip->m_Heading->x);
		pD->Rotate(pShip->m_Shape->m_Center, pShip->m_Heading->z, pShip->m_Heading->y, pShip->m_Heading->x);
		pTa->Rotate(pTarget->m_Shape->m_Center, pTarget->m_Heading->z, pTarget->m_Heading->y, pTarget->m_Heading->x);
		pTb->Rotate(pTarget->m_Shape->m_Center, pTarget->m_Heading->z, pTarget->m_Heading->y, pTarget->m_Heading->x);
		pTc->Rotate(pTarget->m_Shape->m_Center, pTarget->m_Heading->z, pTarget->m_Heading->y, pTarget->m_Heading->x);
		pTd->Rotate(pTarget->m_Shape->m_Center, pTarget->m_Heading->z, pTarget->m_Heading->y, pTarget->m_Heading->x);

		// Now we need to translate our points into Global space
		int nX = this->m_Location->x - this->m_Shape->m_Center->x;
		int nY = this->m_Location->y - this->m_Shape->m_Center->y;
		int nZ = this->m_Location->z - this->m_Shape->m_Center->z;

		pA->x += nX; pA->y += nY; pA->z += nZ;
		pB->x += nX; pB->y += nY; pB->z += nZ;
		pC->x += nX; pC->y += nY; pC->z += nZ;
		pD->x += nX; pD->y += nY; pD->z += nZ;

		// Now for them
		nX = pSpatial->m_Location->x - pSpatial->m_Shape->m_Center->x;
		nY = pSpatial->m_Location->y - pSpatial->m_Shape->m_Center->y;
		nZ = pSpatial->m_Location->z - pSpatial->m_Shape->m_Center->z;

		pTa->x += nX; pTa->y += nY; pTa->z += nZ;
		pTb->x += nX; pTb->y += nY; pTb->z += nZ;
		pTc->x += nX; pTc->y += nY; pTc->z += nZ;
		pTd->x += nX; pTd->y += nY; pTd->z += nZ;

		// Now we can start our comparisions
		if (pA->Distance(pTa) < nMin)
			nMin = pA->Distance(pTa);
		else if (pA->Distance(pTb) < nMin)
			nMin = pA->Distance(pTb);
		else if (pA->Distance(pTc) < nMin)
			nMin = pA->Distance(pTc);
		else if (pA->Distance(pTd) < nMin)
			nMin = pA->Distance(pTd);

		if (pB->Distance(pTa) < nMin)
			nMin = pB->Distance(pTa);
		else if (pB->Distance(pTb) < nMin)
			nMin = pB->Distance(pTb);
		else if (pB->Distance(pTc) < nMin)
			nMin = pB->Distance(pTc);
		else if (pB->Distance(pTd) < nMin)
			nMin = pB->Distance(pTd);

		if (pC->Distance(pTa) < nMin)
			nMin = pC->Distance(pTa);
		else if (pC->Distance(pTb) < nMin)
			nMin = pC->Distance(pTb);
		else if (pC->Distance(pTc) < nMin)
			nMin = pC->Distance(pTc);
		else if (pC->Distance(pTd) < nMin)
			nMin = pC->Distance(pTd);

		if (pD->Distance(pTa) < nMin)
			nMin = pD->Distance(pTa);
		else if (pD->Distance(pTb) < nMin)
			nMin = pD->Distance(pTb);
		else if (pD->Distance(pTc) < nMin)
			nMin = pD->Distance(pTc);
		else if (pD->Distance(pTd) < nMin)
			nMin = pD->Distance(pTd);


		delete pA;
		delete pB;
		delete pC;
		delete pD;
		delete pTa;
		delete pTb;
		delete pTc;
		delete pTd;

	}
	else if (pSpatial->m_Shape->m_nType == CShape::ST_SPHERE)
	{
		// All we need to do is take the radius from the distance
		int nDistance = this->m_Location->Distance(pSpatial->m_Location);

		// Their distance in total is the distance from the centre of the sphere
		// to the nearest point of the object and then subtract the radius from it.
		nDistance -= this->m_Shape->m_nRadius;

		nMin = nDistance;
	}

	return nMin;
}

std::ostream& operator << ( std::ostream& stream, const CSpatial& spatial )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Spatial]");
	Tools.WriteLn(stream, " Name             : \"%s\"", spatial.m_gsName);
	Tools.WriteLn(stream, " Filename         : \"%s\"", spatial.m_gfFileName);
	Tools.WriteLn(stream, " Type	          : %d", spatial.m_nType);
	Tools.WriteLn(stream, " Gravity          : %d", spatial.m_nGravity);
	Tools.WriteLn(stream, " Heat	          : %d", spatial.m_Signature[CSpatial::SI_HEAT]);
	Tools.WriteLn(stream, " Em               : %d", spatial.m_Signature[CSpatial::SI_EM]);
	Tools.WriteLn(stream, " Ion 	          : %d", spatial.m_Signature[CSpatial::SI_ION]);
	Tools.WriteLn(stream, " Mass 	          : %d", spatial.m_Signature[CSpatial::SI_MASS]);
	Tools.WriteLn(stream, " Sector           : \"%s\"", spatial.m_gsSector);
	Tools.WriteLn(stream, " Description      : \"%s\"", spatial.m_gsDescription);
//	stream << *spatial.m_Location;
//	stream << *spatial.m_Shape;
	Tools.WriteLn(stream, "[/Spatial]");

	return stream;
}

void CSpatial::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pSpatialNode = pParent->FirstChild("Spatial");

	if ( pSpatialNode != NULL )
	{
		Tools.ReadXml(pSpatialNode, "name",			m_gsName);
		Tools.ReadXml(pSpatialNode, "filename",		m_gfFileName);
		Tools.ReadXml(pSpatialNode, "type",			m_nType);
		Tools.ReadXml(pSpatialNode, "gravity",			m_nGravity);
		Tools.ReadXml(pSpatialNode, "heat",			m_Signature[CSpatial::SI_HEAT]);
		Tools.ReadXml(pSpatialNode, "em",				m_Signature[CSpatial::SI_EM]);
		Tools.ReadXml(pSpatialNode, "ion",				m_Signature[CSpatial::SI_ION]);
		Tools.ReadXml(pSpatialNode, "mass",			m_Signature[CSpatial::SI_MASS]);
		Tools.ReadXml(pSpatialNode, "sector",			m_gsSector);
		Tools.ReadXml(pSpatialNode, "description",		m_gsDescription);
	}

	return;

}

void CSpatial::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pSpatialNode = Tools.InsertXmlChild(pParent, "Spatial");

	Tools.WriteXml(pSpatialNode, "name",			m_gsName);
	Tools.WriteXml(pSpatialNode, "filename",		m_gfFileName);
	Tools.WriteXml(pSpatialNode, "type",			m_nType);
	Tools.WriteXml(pSpatialNode, "gravity",			m_nGravity);
	Tools.WriteXml(pSpatialNode, "heat",			m_Signature[CSpatial::SI_HEAT]);
	Tools.WriteXml(pSpatialNode, "em",				m_Signature[CSpatial::SI_EM]);
	Tools.WriteXml(pSpatialNode, "ion",				m_Signature[CSpatial::SI_ION]);
	Tools.WriteXml(pSpatialNode, "mass",			m_Signature[CSpatial::SI_MASS]);
	Tools.WriteXml(pSpatialNode, "sector",			m_gsSector);
	Tools.WriteXml(pSpatialNode, "description",		m_gsDescription);

	m_Location->WriteXml(pSpatialNode);
	m_Shape->WriteXml(pSpatialNode);

	return;
}

std::istream& operator >> ( std::istream& stream, CSpatial& spatial )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	try
	{
		if ( Tools.ReadKey(stream) == "[Spatial]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Spatial]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'D':
						if ( gsKey == "Description" )
							Tools.ReadData(stream, spatial.m_gsDescription);
						break;		
					case 'E':
						if ( gsKey == "Em" )
							Tools.ReadData(stream, spatial.m_Signature[CSpatial::SI_EM]);
						break;
					case 'F':
						if ( gsKey == "Filename" )
							Tools.ReadData(stream, spatial.m_gfFileName);
						break;
					case 'G':
						if ( gsKey == "Gravity" )
							Tools.ReadData(stream, spatial.m_nGravity);
						break;
					case 'H':
						if ( gsKey == "Heat" )
							Tools.ReadData(stream, spatial.m_Signature[CSpatial::SI_HEAT]);
						break;
					case 'I':
						if ( gsKey == "Ion" )
							Tools.ReadData(stream, spatial.m_Signature[CSpatial::SI_ION]);
						break;
					case 'M':
						if ( gsKey == "Mass" )
							Tools.ReadData(stream, spatial.m_Signature[CSpatial::SI_MASS]);
						break;
					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, spatial.m_gsName);
						break;		
					case 'S':
						if ( gsKey == "Sector" )
							Tools.ReadData(stream, spatial.m_gsSector);
						break;	
					case 'T':
						if ( gsKey == "Type" )
							Tools.ReadData(stream, spatial.m_nType);
						break;						
					case '[':
						{
							if ( gsKey == "[Cart]" )
							{
								CCart* pCart = new CCart;

								stream.seekg(marker);

//								stream >> *pCart;

								spatial.m_Location = pCart;	// Assign pointer to Location

							}
							if ( gsKey == "[Shape]" )
							{
								CShape* pShape = new CShape;

								stream.seekg(marker);

//								stream >> *pShape;

								spatial.m_Shape = pShape;	// Assign pointer to Location

							}
						}
						break;		

					default:
						Tools.Report(E_ERROR, "[CSpatial::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Spatial]");
			}
		}
	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CSpatial::>>] Error encountered while reading %s\'s file..", spatial.m_gsName);
	}

	return stream;
}

// Overloadded method
bool CSpatial::Load()
{
	return false;
}

// Overloadded method
bool CSpatial::Save()
{
	return false;
}

bool CSpatial::Load(gString filename)
{
	return false;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 2. Planet Class
///////////////////////////////////////////////////////////////////////////////////////////
CPlanet::CPlanet()
{
	m_nType = SO_PLANET;
	m_Signature[CSpatial::SI_MASS] = 1000;
}

CPlanet::CPlanet(gString name, gString filename)
{
	m_gsName = name;
	m_gfFileName = filename;
	m_nType = SO_PLANET;
	m_Signature[CSpatial::SI_MASS] = 1000;
}

CPlanet::~CPlanet()
{

}

// Update the planet!
void CPlanet::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

		CSector* pSector = pGalaxy->GetSec(this->m_gsSector);
		
		if ( pSector )
		{
//			pSector->Inform("I've just updated!");		
		}
	
}

// Destroy the Planet!
void  CPlanet::Destroy()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);

	// Will need to remove all areas within planet and kill all players on planet

	// Now we need to create debris
	int nDebris = pRandom->NumberRange(1, (this->m_Signature[CSpatial::SI_MASS]/100));
	int nDebMass = this->m_Signature[CSpatial::SI_MASS];

	for (int i = 0; i < nDebris; i++)
	{
		gString gsName;
		gString gsFileName;

		gsName.Format("Debris of %s %d", this->m_gsName, i);
		gsFileName.Format("Debris%s%d.spa", this->m_gsName, i);
		
		CDebris* pDebris = new CDebris(gsName, gsFileName);

		// Work out the size of the chunk
		int nActMass = (nDebMass - (pRandom->NumberRange(1, nDebMass)));

		nDebMass = nDebMass - nActMass;

		if (nActMass < 5)
		{
			pDebris = NULL;
			continue;
		}

		pDebris->m_Signature[CSpatial::SI_MASS] = nActMass;	// Set the mass

		// Work out the description
		float nMass = (float)nActMass;
		float nmMass = (float)this->m_Signature[CSpatial::SI_MASS];
		float nPercentage = (nMass/nmMass) * 100; // Percentage of Debris of original mass


		gString gsSize;
		gString gsQuantifier;

		// Come up with an accurate first description
		if (nPercentage >= 80)
			gsSize = "An almost whole";
		else if (nPercentage < 80 && nPercentage >= 70)
			gsSize = "A massive";
		else if (nPercentage < 70 && nPercentage >= 60)
			gsSize = "An enormous";
		else if (nPercentage < 60 && nPercentage >= 50)
			gsSize = "An outsized";
		else if (nPercentage < 50 && nPercentage >= 40)
			gsSize = "A huge";
		else if (nPercentage < 40 && nPercentage >= 30)
			gsSize = "A sizeable";
		else if (nPercentage < 30 && nPercentage >= 20)
			gsSize = "A relatively large";
		else if (nPercentage < 20 && nPercentage >= 10)
			gsSize = "A minute";
		else if (nPercentage < 10)
			gsSize = "A diminutive";

		// Now we need a random quantifier
		int nRand = pRandom->NumberRange(1, 5);
		switch (nRand)
		{
			case 1: gsQuantifier = "chunk";
				break;
			case 2: gsQuantifier = "piece";
				break;
			case 3: gsQuantifier = "part";
				break;
			case 4: gsQuantifier = "hulk";
				break;
			case 5: gsQuantifier = "mass";
				break;
			default: gsQuantifier = "lump";
				break;
		}

		gString gsDesc;
		gsDesc.Format("%s %s of %s\'s remains", gsSize, gsQuantifier, this->m_gsName);

		pDebris->m_gsDescription = gsDesc;
		pDebris->m_gsSector = this->m_gsSector;

		// We need to work out here the momentum and perhaps speed of the debris
		// we will leave it stationary for the moment #TODO#
		pDebris->m_Location = this->m_Location;

		pDebris->Save();
		pGalaxy->AddSpatial(pDebris);
		pGalaxy->AddSpatialToSpace(pDebris->m_Vnum);
	}

	pGalaxy->Save();

	int nRand = pRandom->NumberRange(1,5);
	gString gsMsg;

	// Decide upon Random message to display
	switch (nRand)
	{
		case 1: gsMsg.Format("#102%s implodes spectacularly in a hail of debris!#700", this->m_gsName);
			break;
		case 2: gsMsg.Format("#102%s disappears as it is consumed by a massive implosion shattering debris in all directions!#700", this->m_gsName);
			break;
		case 3: gsMsg.Format("#102A flash of light and colourful shock wave signals the death of %s#700", this->m_gsName);
			break;
		case 4: gsMsg.Format("#102The Planet %s vanishes in a hail of debris and colourful explosions!#700", this->m_gsName);
			break;
		case 5: gsMsg.Format("#102There is a blinding flash as %s shatters into oblivion!#700", this->m_gsName);
			break;
		default: gsMsg.Format("#102Blinding flashes of light consume %s as fissures erupt from the planet!#700", this->m_gsName);
			break;
	}

	// Pass the message
	if ( pSector )
	{
		if (gsMsg != "")
			pSector->Inform(gsMsg);
	}

	// Flag it for deletion
	this->m_bDelete = true;
	return;

}


bool CPlanet::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Planet");

	ReadXml(pNode);

	return true;
}

bool CPlanet::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Planet");

	ReadXml(pNode);

	return true;
}

bool CPlanet::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Planet");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CPlanet::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);

	// Planet specific fields are read in here.

	return;	
}

void CPlanet::WriteXml(TiXmlNode* pParent)
{
	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);

	return;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 3. Star Class
///////////////////////////////////////////////////////////////////////////////////////////
CStar::CStar()
{
	m_bDying = false;
	m_nType = SO_STAR;
	m_nEnergy = 1000;
	m_Signature[CSpatial::SI_MASS] = 1000;
}

CStar::CStar(gString name, gString filename)
{
	m_gsName = name;
	m_nType = SO_STAR;
	m_gfFileName = filename;
	m_bDying = false;
	m_nEnergy = 1000;
	m_Signature[CSpatial::SI_MASS] = 1000;
}

CStar::~CStar()
{
	m_nEnergy = 0;
	m_Signature[CSpatial::SI_MASS] = 0;
}

// A Star will change if it is dying. It will count down its Energy every update
// until it reaches zero. The resulting collapse of the star will depend upon
// the mass of the star. The theory behind this is the build up of iron as the 
// energy furnace of the star gets depleted. The star then collapses (to about 1/100)
// of its original size and can do one of three things:
// 1) Create a White Dwarf
// 2) Create a Supernova
// 3) Create a Blackhole
void CStar::Update()
{
	gString gsMsg;
	bool bDead = false;
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);
	
	// If its dying, reduce its energy
	if (m_bDying && m_nEnergy > 0)
	{
		m_nEnergy--;
	}

	if (m_nEnergy <= 5)
	{
		switch (m_nEnergy)
		{
			case 5: gsMsg.Format("#601%s disappears in a BLINDING flash of light!#700", this->m_gsName);
				break;
			case 4: gsMsg.Format("#601The light starts to implode as if being consumed by %s!#700", this->m_gsName);
				break;
			case 3: gsMsg.Format("#602A mass blaze of coloured light sprawls from %s.#700", this->m_gsName);
				break;
			case 2: gsMsg.Format("#602The light from %s is quickly followed by a nova of rapidly expanding, purple energy!#700", this->m_gsName);
				break;
			case 1: gsMsg.Format("#602%s has completely collapsed leaving a trail of destruction in its wake!#700", this->m_gsName);
					bDead = true;
				break;
		}		

	}

	// Pass the message
	if ( pSector )
	{
		if (gsMsg != "")
			pSector->Inform(gsMsg);
	}

	// Handle the creation/destruction aspects
	if (bDead)
	{
		// Lets destroy the star now
		this->Destroy();
	}
	
}

// As we have discussed in quite a lot of detail elsewhere, when a star dies it
// goes out with a bang. It can form a supernova, a whitedwarf or a blackhole after 
// it explodes.
void  CStar::Destroy()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);
	SpatialList::iterator spa;

	// Now we need to slag everything in the sector and create debris
	// stars dont take kindly to being blown up

	// Because we create Debris and other Spatial Objects we need a static, snapshot
	// of the Galaxy spatial list.

	SpatialList m_SnapList = pGalaxy->m_SpatialList;
	for (spa = m_SnapList.begin(); spa != m_SnapList.end(); spa++)
	{
		CSpatial* pSpatial = (CSpatial*)(*spa);
		
		// Dont blow up the star just yet
		if (pSpatial->m_gfFileName == this->m_gfFileName)
			continue;

		// Slag it!
		if (pSpatial->m_gsSector == this->m_gsSector)
		{
			pSpatial->Destroy();
			continue;
		}
	}

	// Whitedwarf
	if (this->m_Signature[CSpatial::SI_MASS] < 1000)
	{
		// White Dwarf
		this->m_gsDescription.Format("A Brilliantly bright Whitedwarf");
		this->Save();
	}
	else if(this->m_Signature[CSpatial::SI_MASS] >= 1000 && this->m_Signature[CSpatial::SI_MASS] < 2000)
	{
		// Nebulas created after the Supernova
		int nColour = pRandom->NumberRange(1, 6);
		this->m_gsDescription.Format("A vibrant, colourful #%d01Nebula#700#602. The remains of the collapsed %s#700", nColour, this->m_gsName);
		this->m_gsName.Format("A brilliant Nebula of vibrant colour");
		this->Save();
	}
	else if(this->m_Signature[CSpatial::SI_MASS] >= 2000)
	{
		// Blackhole!
		gString gsNewFile;
		gString gsNewName;
		gsNewFile.Format("%sBH.spa", this->m_gsName);
		gsNewName.Format("%sBH", this->m_gsName);

		CSpatial* pBlack = new CBlackhole(gsNewName, gsNewFile);
		pBlack->m_gsSector = this->m_gsSector;
		pBlack->m_Location = this->m_Location;
		pBlack->m_gsDescription = "A area of high gravity consuming all light!";

		pBlack->Save();
		pGalaxy->AddSpatial(pBlack);
		pGalaxy->AddSpatialToSpace(pBlack->m_Vnum);


		// Flag this for deletion
		this->m_bDelete = true;
		pGalaxy->Save();
		
	}

}

bool CStar::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Star");

	ReadXml(pNode);

	return true;
}

bool CStar::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Star");

	ReadXml(pNode);

	return true;
}

bool CStar::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Star");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CStar::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);

	// Star specific fields are read in here.
	Tools.ReadXml(pParent, "dying",			m_bDying);
	Tools.ReadXml(pParent, "energy",		m_nEnergy);
	return;	
}

void CStar::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);

	Tools.WriteXml(pParent, "dying",		m_bDying);
	Tools.WriteXml(pParent, "energy",		m_nEnergy);
	return;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 3. AsteroidField Class
///////////////////////////////////////////////////////////////////////////////////////////
CAsteroidField::CAsteroidField()
{
	m_nType = SO_ASTEROIDF;
	m_nSpeed = 0;
	m_pHeading = new CCart;
	
}

CAsteroidField::CAsteroidField(gString name, gString filename, int size)
{
	CRandom* pRandom = CGameObjects::Get().Rand();
	CGameObjects& globals = CGameObjects::Get();

	m_gsName = name;
	m_nType = SO_ASTEROIDF;
	m_gfFileName = filename;
	

	gString gsDirectory = name;
	gString gsFile;

	gsFile.Format("%sSpatial\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);

	gsDirectory.Format("%s%s\\", gsFile, name);

	if ( !gFileName::DirectoryExists(gsDirectory) )
		gFileName::MakeDirectory(gsDirectory);

	for (int i = 0; i < size; i++)
	{
		gString gsName;
		gsName.Format("%s%d", name, i);
		gString gsFilename;
		gsFilename.Format("%s.spa", gsName);


		CAsteroid* pAsteroid = new CAsteroid(gsName, gsFilename, pRandom->NumberRange(1, 1000), 0, name);
		pAsteroid->Save();

		m_AsteroidList.push_back(pAsteroid);
	}

	// Initialise
	m_pHeading = new CCart;
	m_nSpeed = 0;

}

CAsteroidField::~CAsteroidField()
{
	delete m_pHeading;
	m_nSpeed = 0;
	m_AsteroidList.clear();

}

// Move the Asteroid field
void CAsteroidField::Update()
{
	AsteroidList::iterator ast;
	CRandom* pRandom = CGameObjects::Get().Rand();

	for (ast = this->m_AsteroidList.begin(); ast != this->m_AsteroidList.end(); ast++)
	{
		// Delete each file	
		int nDistance = pRandom->NumberRange(1, m_nSpeed);
		CAsteroid* pAster = (CAsteroid*)(*ast);
//		pAster->m_Location->Move(nDistance);
	}

//	this->m_Location->Move(m_nSpeed);
	this->Save();
}

// Destroy the Field!
void  CAsteroidField::Destroy()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);
	gString szFile;
	AsteroidList::iterator ast;
	
	// Destroy all the sub Asteroid's first
	for (ast = this->m_AsteroidList.begin(); ast != this->m_AsteroidList.end(); ast++)
	{
		// Delete each file
		CAsteroid* pAster = (CAsteroid*)(*ast);
		szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Spatial\\" + this->m_gsName + "\\" + pAster->m_gfFileName;
		unlink(szFile);
		pAster = NULL;

	}

	// Remove the directory
	gString szDir = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Spatial\\" + this->m_gsName;
	rmdir(szDir);

	int nRand = pRandom->NumberRange(1,5);
	gString gsMsg;

	switch (nRand)
	{
		case 1: gsMsg.Format("#302%s disppears in a massive explosion of dust and debris!#700", this->m_gsName);
			break;
		case 2: gsMsg.Format("#302In a split second %s and all its Asteroids vanish in an explosion of dust!#700", this->m_gsName);
			break;
		case 3: gsMsg.Format("#302Dust and debris explodes in all directions as %s vanishes from sight!#700", this->m_gsName);
			break;
		case 4: gsMsg.Format("#302The %s erupts in earth and dirt as it disappears!#700", this->m_gsName);
			break;
		case 5: gsMsg.Format("#302There is a massive cloud of dust as %s shatters into oblivion!#700", this->m_gsName);
			break;
		default: gsMsg.Format("#302Blinding flashes of light consume %s as the field is destroyed!#700", this->m_gsName);
			break;
	}

	// Pass the message
	if ( pSector )
	{
		if (gsMsg != "")
			pSector->Inform(gsMsg);
	}

	// Remove the field itself
	// Flag this for deletion
	this->m_bDelete = true;


}

void CAsteroidField::AddAsteroid(CAsteroid* pAster)
{
	this->m_AsteroidList.push_back(pAster);
	
}

bool CAsteroidField::RemoveAsteroid(CAsteroid* pDelete)
{
	// Take out of Field's list
	int nCount = 0;
	AsteroidList::iterator ast;
	gString szFile;
	CAsteroid* pAster;

	for (ast = this->m_AsteroidList.begin(); ast != this->m_AsteroidList.end(); ast++)
	{
		pAster = (*ast);

		if (pAster->m_gfFileName != pDelete->m_gfFileName)
			nCount++;
		else
			break;
		
	}

	// Removed from list
	this->m_AsteroidList.erase(this->m_AsteroidList.begin()+nCount);
	// Delete object

	szFile = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Spatial\\" + this->m_gsName + "\\" + pDelete->m_gfFileName;
	unlink(szFile);

	pDelete = NULL;
	this->Save();

	return true;

}
/*
std::ostream& operator << ( std::ostream& stream, const CAsteroidField& asteroidfield )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	
	stream << (const CSpatial&)(asteroidfield);
	Tools.WriteLn(stream, "[AsteroidField]");
	Tools.WriteLn(stream, " Speed          : %d", asteroidfield.m_nSpeed);
//	stream << *asteroidfield.m_pHeading;

	for (int i = 0; i < asteroidfield.m_AsteroidList.size(); i++)
	{
		CAsteroid* pAsteroid = (CAsteroid*) (asteroidfield.m_AsteroidList.at(i));
		Tools.WriteLn(stream, "[Asteroid]");
		Tools.WriteLn(stream, " Filename         : \"%s\"", pAsteroid->m_gfFileName);
		Tools.WriteLn(stream, "[/Asteroid]");
	}

	Tools.WriteLn(stream, "[/AsteroidField]");

	return stream;
}

std::istream& operator >> ( std::istream& stream, CAsteroidField& asteroidfield )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CGameObjects& globals = CGameObjects::Get();

	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	stream >> (CSpatial&)(asteroidfield);

	try
	{
		if ( Tools.ReadKey(stream) == "[AsteroidField]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/AsteroidField]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{

					case 'S':
						if ( gsKey == "Speed" )
							Tools.ReadData(stream, asteroidfield.m_nSpeed);
						break;
					case '[':
						if ( gsKey == "[Cart]" )
						{
							CCart* pCart = new CCart;
							
							stream.seekg(marker);

							stream >> *pCart;
							
							asteroidfield.m_pHeading = pCart;
						}
						if ( gsKey == "[Asteroid]" )
						{
							gString gsFile;

							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);
								
								CAsteroid* pAsteroid = new CAsteroid;
								
								gsFile.Format("%sSpatial\\%s\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], asteroidfield.m_gsName, gsFile);						
								pAsteroid->Load(gsFile);

								asteroidfield.m_AsteroidList.push_back(pAsteroid);
								
							}
							
							
						}
						
						break;
	

					default:
						Tools.Report(E_ERROR, "[CAsteroidField::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/AsteroidField]");
			}
		}

	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CAsteroidField::>>] Error encountered while reading Spatial file..");
	}

	return stream;
} */

bool CAsteroidField::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("AsteroidField");

	ReadXml(pNode);

	return true;
}

bool CAsteroidField::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("AsteroidField");

	ReadXml(pNode);

	return true;
}

bool CAsteroidField::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "AsteroidField");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CAsteroidField::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);
	return;	
}

void CAsteroidField::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);

	return;
}


///////////////////////////////////////////////////////////////////////////////////////////
// 4. Debris Class
///////////////////////////////////////////////////////////////////////////////////////////
CDebris::CDebris()
{
	m_nLife = 20;
	m_nDamage = 1000;
	m_bExplode = false;
	m_nType = SO_DEBRIS;
}

CDebris::CDebris(gString name, gString filename)
{
	m_gsName = name;
	m_gfFileName = filename;
	m_nLife = 20;
	m_nDamage = 1000;
	m_bExplode = false;
	m_nType = SO_DEBRIS;

}

CDebris::~CDebris()
{
	m_nLife = 0;
	m_nDamage = 0;
	m_bExplode = false;
}

void CDebris::Update()
{

}

// Destroy the Debris!
void  CDebris::Destroy()
{
	this->m_bDelete = true;
}

bool CDebris::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Debris");

	ReadXml(pNode);

	return true;
}

bool CDebris::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Debris");

	ReadXml(pNode);

	return true;
}

bool CDebris::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Debris");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CDebris::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);

	Tools.ReadXml(pParent, "life",			m_nLife);
	Tools.ReadXml(pParent, "damage",		m_nDamage);
	Tools.ReadXml(pParent, "explode",		m_bExplode);
	return;	
}

void CDebris::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);

	Tools.WriteXml(pParent, "life",			m_nLife);
	Tools.WriteXml(pParent, "damage",		m_nDamage);
	Tools.WriteXml(pParent, "explode",		m_bExplode);
	return;
}

///////////////////////////////////////////////////////////////////////////////////////////
// 5. Satellite Class
///////////////////////////////////////////////////////////////////////////////////////////
CSatellite::CSatellite()
{
	m_nType = SO_SATELLITE;
	m_gsPlanet = "";
	m_nOrbit = 0;
}

CSatellite::CSatellite(gString name, gString filename)
{

	m_nType = SO_SATELLITE;
	m_gsPlanet = "";
	m_nOrbit = 0;
	m_gfFileName = filename;
	m_gsName = name;
}

CSatellite::~CSatellite()
{
	m_gsPlanet = "";
	m_nOrbit = 0;
	m_nType = 0;
}

void CSatellite::Update()
{

}

void CSatellite::Destroy()
{

}

bool CSatellite::Load(gString gsFileName)
{
	// Overrided method
	return true;
}

bool CSatellite::Load()
{
	// Overrided method
	return true;
}

bool CSatellite::Save()
{
	// Again... Overrided, should never be 
	// an actual 'satellite' object
	return true;
}

void CSatellite::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pSatellite = pParent->FirstChild("Satellite");

	if (pSatellite != NULL)
	{
		Tools.ReadXml(pSatellite, "planet",		m_gsPlanet);
		Tools.ReadXml(pSatellite, "orbit",			m_nOrbit);
	}
	
	return;	
}

void CSatellite::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pSatNode = Tools.InsertXmlChild(pParent, "Satellite");

	Tools.ReadXml(pSatNode, "planet",		m_gsPlanet);
	Tools.ReadXml(pSatNode, "orbit",		m_nOrbit);
	return;
}


///////////////////////////////////////////////////////////////////////////////////////////
// 5a. Moon Class
///////////////////////////////////////////////////////////////////////////////////////////
CMoon::CMoon()
{
	m_nType = SO_MOON;
}

CMoon::CMoon(gString name, gString filename)
{
	m_nType = SO_MOON;
	m_gfFileName = filename;
	m_gsName = name;
}

CMoon::~CMoon()
{
	m_nType = 0;

}

void CMoon::Update()
{

}

void CMoon::Destroy()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);


	// Remove the directory
	gString szDir = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Spatial\\" + this->m_gsName;
	rmdir(szDir);

	int nRand = pRandom->NumberRange(1,3);
	gString gsMsg;

	switch (nRand)
	{
		case 1: gsMsg.Format("#402%s disappears as it is consumed by explosions!#700", this->m_gsName);
			break;
		case 2: gsMsg.Format("#402%s implodes spectacularly as it shatters dust and debris in all directions!#700", this->m_gsName);
			break;
		case 3: gsMsg.Format("#402%s erupts in fissures and flames as it implodes.#700", this->m_gsName);
			break;
		default: gsMsg.Format("#402%s disppears as it is consumed by explosions!#700", this->m_gsName);
			break;
	}

	// Pass the message
	if ( pSector )
	{
		if (gsMsg != "")
			pSector->Inform(gsMsg);
	}

	// Remove the moon itself
	// Flag this for deletion
	this->m_bDelete = true;

}

bool CMoon::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Moon");

	ReadXml(pNode);

	return true;
}

bool CMoon::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Moon");

	ReadXml(pNode);

	return true;
}

bool CMoon::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Moon");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CMoon::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);
	CSatellite::ReadXml(pParent);

	return;	
}

void CMoon::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSatellite::WriteXml(pParent);
	CSpatial::WriteXml(pParent);
	return;
}


///////////////////////////////////////////////////////////////////////////////////////////
// 5b. Sat Class
///////////////////////////////////////////////////////////////////////////////////////////
CSat::CSat()
{
	m_nType = SO_SAT;
	m_nPower = 0;
	m_nIntegrity = 100;
	m_gsOwner = "";
}

CSat::CSat(gString name, gString filename)
{
	m_nType = SO_SAT;
	m_nPower = 0;
	m_nIntegrity = 100;
	m_gsOwner = "";
	m_gfFileName = filename;
	m_gsName = name;
}

CSat::~CSat()
{
	m_nType = 0;
	m_nPower = 0;
	m_nIntegrity = 0;
	m_gsOwner = "";

}

void CSat::Update()
{

}

void CSat::Destroy()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);


	// Remove the directory
	gString szDir = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Spatial\\" + this->m_gsName;
	rmdir(szDir);

	int nRand = pRandom->NumberRange(1,5);
	gString gsMsg;

	switch (nRand)
	{
		case 1: gsMsg.Format("#502%s disappears in an immense implosion as its fuel cells ignite!#700", this->m_gsName);
			break;
		case 2: gsMsg.Format("#502%s implodes suddenly shattering flames and debris all around!#700", this->m_gsName);
			break;
		case 3: gsMsg.Format("#502%s gouts flames from its main body as it implodes!#700", this->m_gsName);
			break;
		case 4: gsMsg.Format("#502%s shatters into pieces as an explosion rips through it!#700", this->m_gsName);
			break;
		case 5: gsMsg.Format("#502Light erupts from %s as its reactor goes up in a MASSIVE explosion!#700", this->m_gsName);
			break;
		default: gsMsg.Format("#502%s disappears in an immense implosion as its fuel cells ignite!#700", this->m_gsName);
			break;
	}

	// Pass the message
	if ( pSector )
	{
		if (gsMsg != "")
			pSector->Inform(gsMsg);
	}

	// Remove the sat itself
	// Flag this for deletion
	this->m_bDelete = true;

}

bool CSat::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Sat");

	ReadXml(pNode);

	return true;
}

bool CSat::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Sat");

	ReadXml(pNode);

	return true;
}

bool CSat::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Sat");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CSat::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);
	CSatellite::ReadXml(pParent);

	Tools.ReadXml(pParent, "power",		m_nPower);
	Tools.ReadXml(pParent, "owner",		m_gsOwner);

	return;	
}

void CSat::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSatellite::WriteXml(pParent);
	CSpatial::WriteXml(pParent);

	Tools.WriteXml(pParent, "power",		m_nPower);
	Tools.WriteXml(pParent, "owner",		m_gsOwner);

	return;
}



///////////////////////////////////////////////////////////////////////////////////////////
// 7. Asteroid Class
///////////////////////////////////////////////////////////////////////////////////////////
CAsteroid::CAsteroid()
{
	m_nType = SO_ASTEROID;
	m_nSize = 10;
	m_nMineral = 0;
	m_gsField = "";
}

CAsteroid::CAsteroid(gString name, gString filename)
{
	m_gsName = name;
	m_gfFileName = filename;
	m_nType = SO_ASTEROID;
	m_nSize = 10;
	m_nMineral = 0;
	m_gsField = "";

}

CAsteroid::CAsteroid(gString name, gString filename, int size, int mineral, gString gsField)
{
	m_gsName = name;
	m_gfFileName = filename;
	m_nType = SO_ASTEROID;
	m_nSize = size;
	m_nMineral = mineral;
	m_gsField = gsField;
	m_gsDescription = "A bulky asteroid";

}

CAsteroid::~CAsteroid()
{
	m_nSize = 0;
	m_gsField = "";
	m_nMineral = 0;
}

void CAsteroid::Update()
{

}

// Destroying large asteroids only causes them to split into smaller ones!
void CAsteroid::Destroy()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	CRandom* pRandom = CGameObjects::Get().Rand();
	CSector* pSector = pGalaxy->GetSec(this->m_gsSector);


	// Remove the directory
	gString szDir = CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR] + "Spatial\\" + this->m_gsName;
	rmdir(szDir);

	int nRand = pRandom->NumberRange(1,5);
	gString gsMsg;

	switch (nRand)
	{
		case 1: gsMsg.Format("#302%s disappears in a cloud of dust!#700", this->m_gsName);
			break;
		case 2: gsMsg.Format("#302%s is consumed by a massive dust cloud!#700", this->m_gsName);
			break;
		case 3: gsMsg.Format("#302%s shatters to pieces!!#700", this->m_gsName);
			break;
		case 4: gsMsg.Format("#302%s erupts dirt and dust in all directions!#700", this->m_gsName);
			break;
		case 5: gsMsg.Format("#302%s disappears from sight as dirt and dust consume it!#700", this->m_gsName);
			break;
		default: gsMsg.Format("#302%s disappears in a cloud of dust!#700", this->m_gsName);
			break;
	}

	// Pass the message
	if ( pSector )
	{
		if (gsMsg != "")
			pSector->Inform(gsMsg);
	}

	// Now we can split the Asteroid up depending on its mass
    // The mass of the asteroid needs to be more than 100 for it to be able to split
	if (this->m_Signature[CSpatial::SI_MASS] > 100)
	{
		int nMass = this->m_Signature[CSpatial::SI_MASS];
		int nPieces = pRandom->NumberRange(2, (this->m_Signature[CSpatial::SI_MASS]/100));

		for (int i = 0; i < nPieces; i++)
		{
			int nActMass = nMass - (pRandom->NumberRange(1, nMass));
			nMass = nMass - nActMass;
			gString gsName;
			gString gsFileName;
			gsName.Format("%s%d", this->m_gsName, i);

			if (this->m_gsField == "")
				gsFileName.Format("%s.spa", gsName);

			CAsteroid* pAster = new CAsteroid(gsName, gsFileName, nActMass, this->m_nMineral, this->m_gsField);
			pAster->m_Location = this->m_Location; // Same location
			pAster->m_gsSector = this->m_gsSector; // Same Sector
			pAster->m_Signature[CSpatial::SI_MASS] = nActMass;

			// Now we need to add this to either a Field or the Galaxy
			if (pAster->m_gsField == "")
			{
				pAster->Save();
				pGalaxy->AddSpatial(pAster);
				pGalaxy->AddSpatialToSpace(pAster->m_Vnum);
				pGalaxy->Save();
			}
			else // It belongs to a field!
			{
				CAsteroidField* pField = (CAsteroidField*)pGalaxy->GetSpa(this->m_gsField);
				pField->AddAsteroid(pAster);
				pField->Save();
			}
		}



	}

	// Remove the asteroid itself
	if (this->m_gsField == "")
	{
		// Flag this for deletion
		this->m_bDelete = true;
		pGalaxy->Save();
	}
	else // It belongs to a field!
	{
		CAsteroidField* pField = (CAsteroidField*)pGalaxy->GetSpa(this->m_gsField);
		pField->RemoveAsteroid(this);
		pField->Save();
	}
	
}

bool CAsteroid::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Asteroid");

	ReadXml(pNode);

	return true;
}

bool CAsteroid::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Asteroid");

	ReadXml(pNode);

	return true;
}

bool CAsteroid::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Asteroid");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CAsteroid::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);

	Tools.ReadXml(pParent, "mineral",		m_nMineral);
	Tools.ReadXml(pParent, "field",			m_gsField);
	return;	
}

void CAsteroid::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);

	Tools.WriteXml(pParent, "mineral",		m_nMineral);
	Tools.WriteXml(pParent, "field",			m_gsField);
	return;
}





///////////////////////////////////////////////////////////////////////////////////////////
// 9. BlackHole Class
///////////////////////////////////////////////////////////////////////////////////////////
CBlackhole::CBlackhole()
{
	m_nType = SO_BLACKHOLE;
	m_nGravity = 1000;
}

CBlackhole::CBlackhole(gString name, gString filename)
{
	m_gsName = name;
	m_gfFileName = filename;
	m_nType = SO_BLACKHOLE;
	m_nGravity = 1000;

}

CBlackhole::~CBlackhole()
{
	
}

void CBlackhole::Update()
{

}

void CBlackhole::Destroy()
{

}


bool CBlackhole::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Blackhole");

	ReadXml(pNode);

	return true;
}

bool CBlackhole::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Blackhole");

	ReadXml(pNode);

	return true;
}

bool CBlackhole::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Blackhole");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void CBlackhole::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);
	return;	
}

void CBlackhole::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);
	return;
}





///////////////////////////////////////////////////////////////////////////////////////////
// 10. Ordinance Class
///////////////////////////////////////////////////////////////////////////////////////////
COrdinance::COrdinance()
{
	m_nType = SO_ORDINANCE;
	m_nGravity = 0;
	m_nFuel = 0;
	m_nSpeed = 0;
	m_nDamage = 0;
	m_nProximity = 0;
	m_nDamType = 0;

	m_Target   = NULL;
	m_Heading  = new CCart(0, 0, 0);
	m_dHeading = new CCart(0, 0, 0);
}

COrdinance::COrdinance(gString name, gString filename)
{
	m_gsName     = name;
	m_gfFileName = filename;
	m_nType      = SO_ORDINANCE;
	m_nGravity   = 1000;
	m_nFuel      = 0;
	m_nSpeed     = 0;
	m_nDamage    = 0;
	m_nProximity = 0;
	m_nDamType   = 0;
	m_nManeuver  = 0;

	m_Target   = NULL;
	m_Heading  = NULL;
	m_dHeading = NULL;

}

COrdinance::~COrdinance()
{
	m_gsName     = "";
	m_gfFileName = "";
	m_nType      = 0;
	m_nGravity   = 0;
	m_nFuel      = 0;
	m_nSpeed     = 0;
	m_nDamage    = 0;
	m_nProximity = 0;
	m_nDamType   = 0;
	m_nManeuver  = 0;

	m_Target   = NULL;

	delete m_Heading;
	m_Heading  = NULL;
	delete m_dHeading;
	m_dHeading = NULL;
	
}

void COrdinance::Update()
{
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	// Do we have a target?
	if (m_Target != NULL)
	{
		// If we do is our current Heading going to allow us to hit the target?
		CCart* pCart = new CCart();

		// We dont want players to have a heading and not to move 
		// so we set 0, 0, 0 equal to 1, 1, 1
		if (this->m_Heading->z == 0 && this->m_Heading->y == 0)
		{
			pCart->x = 0;
			pCart->y = 0;
			pCart->z = 1;
		}
		else
		{
			// The ship is assumed to always move forward along the positive Z axis
			// hence when we have changed course a system for determining
			// the location of our Z axis is required.
			pCart->x = pCart->GetX(360-this->m_Heading->z, this->m_Heading->y, 10000);
			pCart->y = pCart->GetY(this->m_Heading->y, 10000);
			pCart->z = pCart->GetZ(360-this->m_Heading->z, this->m_Heading->y, 10000);
		}

		// Now we check if this move will mean we collide with our target
		if (this->m_Location->MoveDist(pCart, m_nSpeed) >= m_Target->m_Location->Distance(this->m_Location))
		{
			m_Target->NotifySpace("%s detonates as it hits %s!", this->m_gsName, m_Target->m_gsName);

			// Determine the arc
			int nArc = this->m_Location->Arc(this->m_Target->m_Location, this->m_Heading);


			m_Target->Write(CShip::MT_CRITICAL, "#101%s has hit the %s side of the ship!#700\n\r", this->m_gsName, CShip::szArc[nArc]);
			m_Target->Damage(this->m_nDamage, nArc);

			// Flag it for deletion
			this->m_bDelete = true;
			return;
		}
		else
		{
			this->m_Location->Move(pCart, m_nSpeed);
		}

		m_Target->NotifySpace(" %d %d %d :: To Target %0.0f <Fuel: %d>", m_Location->x, m_Location->y, m_Location->z, m_Location->Distance(m_Target->m_Location), m_nFuel);


		// Consume some fuel
		this->m_nFuel--;

		if (m_nFuel < 0)
		{
			m_Target->NotifySpace("%s has run out of fuel!", this->m_gsName);
			this->m_bDelete = true;
			return;
		}
	}

}

void COrdinance::Destroy()
{

}


bool COrdinance::Load(gString gsFileName)
{
	// FileName is already formatted with
	// the path structure required
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Ordinance");

	ReadXml(pNode);

	return true;
}

bool COrdinance::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gfFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Ordinance");

	ReadXml(pNode);

	return true;
}

bool COrdinance::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%sSpatial\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gfFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Ordinance");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

void COrdinance::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Read in our inherited Spatial data
	// Spatial's ReadXml method automatically looks for the
	// opening <Spatial> tag
	CSpatial::ReadXml(pParent);
	return;	
}

void COrdinance::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	// Write inherited data to file
	// WriteXML automatically inserts the <Spatial> tag
	CSpatial::WriteXml(pParent);
	return;
}






