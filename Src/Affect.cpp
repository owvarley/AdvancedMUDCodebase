//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.
//
// The Advanced MUD Codebase Project
// AMC, copyright (c) 2005, 2006 by Owen Varley <owen@sw-erp.org>

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: CAffect
// Header   :: Spatial.h
// Function :: Affects are anything that is placed on or otherwise caused to affect, an Actor or location.
//			:: At the time of this release, the Affect class is little more than a place holder.


#include <time.h>

#include "Mudcore.h"
#include "Affect.h"
#include "GameObjects.h"
#include "../gTools/Log.h"

#include <Tools.h>

char* CAffect::szAffectTypes[] =
	{"None", NULL};


CAffect::CAffect()
{
	m_eType = AF_NONE;
	m_fApply = 0.0f;
	m_CreationDate = time(0);
	m_nStrength = 0;
	m_nOriginID = 0;
	m_nLifeSpan = 0;
	m_Timer.Reset();
}


CAffect::~CAffect()
{
}

void CAffect::Activate()
{
	m_Timer.Start();
}

void CAffect::DeActivate()
{
	m_Timer.Stop();
}

CAffect& CAffect::operator=(CAffect* clone)
{
	if ( !clone )
		return *this;

	m_eType = clone->m_eType;
	m_fApply = clone->m_fApply;
	m_CreationDate = clone->m_CreationDate;
	m_nStrength = clone->m_nStrength;
	m_nOriginID = clone->m_nOriginID;
	m_nLifeSpan = clone->m_nLifeSpan;

	return *this;
}

CAffect& CAffect::operator=(const CAffect& clone)
{
	m_eType = clone.m_eType;
	m_fApply = clone.m_fApply;
	m_CreationDate = clone.m_CreationDate;
	m_nStrength = clone.m_nStrength;
	m_nOriginID = clone.m_nOriginID;
	m_nLifeSpan = clone.m_nLifeSpan;

	return *this;
}

bool CAffect::operator==(CAffect& affect)
{
	return ( m_eType == affect.m_eType
		&&   m_fApply == affect.m_fApply
	    &&   m_CreationDate == affect.m_CreationDate
		&&   m_nStrength == affect.m_nStrength
		&&   m_nOriginID == affect.m_nOriginID
		&&   m_nLifeSpan == affect.m_nLifeSpan );

}

void CAffect::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	float fTimeRemaining = TimeRemaining();

	Tools.WriteXml(pParent, "strength",			m_nStrength);
	Tools.WriteXml(pParent, "type",				(int&)m_eType);
	Tools.WriteXml(pParent, "apply",			m_fApply);
	Tools.WriteXml(pParent, "creation_date",	m_CreationDate);
	Tools.WriteXml(pParent, "owner",			m_nOwnerID);
	Tools.WriteXml(pParent, "origin",			m_nOriginID);
	Tools.WriteXml(pParent, "lifespan",			m_nLifeSpan);

	// write out the time remaining for easy debug purposes. no need
	// to read this value back in.
	Tools.WriteXml(pParent, "time_remaining",	fTimeRemaining);

	m_Timer.WriteXML(pParent, "timer");
}

void CAffect::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "strength",			m_nStrength);
	Tools.ReadXml(pParent, "type",				(int&)m_eType);
	Tools.ReadXml(pParent, "apply",				m_fApply);
	Tools.ReadXml(pParent, "creation_date",		m_CreationDate);
	Tools.ReadXml(pParent, "owner",				m_nOwnerID);
	Tools.ReadXml(pParent, "origin",			m_nOriginID);
	Tools.ReadXml(pParent, "lifespan",			m_nLifeSpan);
	m_Timer.ReadXML(pParent, "timer");
}

bool CAffect::operator==(CAffect* affect)
{
	return (*this == *affect);
}

bool CAffect::operator!=(CAffect& affect)
{
	return !(*this == affect);
}

bool CAffect::operator!=(CAffect* affect)
{
	return !(*this == *affect);
}

void CAffect::Update(bool bForce)
{
	if ( m_Timer.Elapsed() >= m_nLifeSpan )
		DeActivate();

	return;
}

// Write this affect to an output stream
std::ostream& operator << ( std::ostream& stream, const CAffect& aff )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CAffect* pAff = (CAffect*)(&aff);

	Tools.WriteLn(stream, "[Affect]");
	Tools.WriteLn(stream, " Strength          : %d", pAff->m_nStrength);
	Tools.WriteLn(stream, " Type              : %s", CAffect::szAffectTypes[pAff->m_eType]);
	Tools.WriteLn(stream, " Apply             : %2.3f", pAff->m_fApply);
	Tools.WriteLn(stream, " Creation Date     : %d", pAff->m_CreationDate);
	Tools.WriteLn(stream, " Owner             : %d", pAff->m_nOwnerID);
	Tools.WriteLn(stream, " Origin            : %d", pAff->m_nOriginID);
	Tools.WriteLn(stream, " LifeSpan          : %d", pAff->m_nLifeSpan);
	Tools.WriteLn(stream, " Active            : %s", pAff->IsActive() ? "false" : "true");
	Tools.WriteLn(stream, " Time Remaining    : %d", pAff->TimeRemaining());
	Tools.WriteLn(stream, "[/Affect]");

	return stream;
}

// Read this actor from an input stream
std::istream& operator >> ( std::istream& stream, CAffect& aff )
{
	LOG_SCOPE("CAffect:>>");
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	bool bActiveAffect = false;
	int nTimeLeft = 0;

	try
	{
		if ( Tools.ReadKey(stream) == "[Affect]" )
		{
			while ( !bDone )
			{
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Affect]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'A':
						if ( gsKey == "Apply" )
							Tools.ReadData(stream, aff.m_fApply);
						else
						if ( gsKey == "Active" )
							Tools.ReadData(stream, bActiveAffect);
						break;
					case 'C':
						if ( gsKey == "Creation Date" )
							Tools.ReadData(stream, aff.m_CreationDate);
						break;
					case 'L':
						if ( gsKey == "LifeSpan" )
							Tools.ReadData(stream, aff.m_nLifeSpan);
						break;
					case 'O':
						if ( gsKey == "Owner" )
							Tools.ReadData(stream, aff.m_nOwnerID);
						else
						if ( gsKey == "Origin" )
							Tools.ReadData(stream, aff.m_nOriginID);
						break;
					case 'S':
						if ( gsKey == "Strength" )
							Tools.ReadData(stream, aff.m_nStrength);
						break;
					case 'T':
						if ( gsKey == "Time Remaining" )
							Tools.ReadData(stream, nTimeLeft);
						else
						if ( gsKey == "Type" )
						{
							gString gsType;
							int nLookup = 0;
							CAffect::e_AffectType eType = CAffect::AF_NONE;

							Tools.ReadData(stream, gsType);

							for (nLookup = 0; nLookup < CAffect::AF_NUM_AFFECTS; nLookup++)
							{
								if ( gsType.Compare(CAffect::szAffectTypes[nLookup]) == 0 )
									eType = (CAffect::e_AffectType)nLookup;
							}

							aff.m_eType = eType;
						}
						break;
					default:
						g_Log.Log(LOG_ERROR, "Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/Affect]");
			}

		}
		else
		{
			g_Log.Log(LOG_ERROR, "Invalid stream!");
			return stream;
		}

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "Error encountered while reading file..");
		return stream;
	}

	if ( bActiveAffect && nTimeLeft > 0 && !aff.IsActive() )
		aff.Activate();

	return stream;
}
