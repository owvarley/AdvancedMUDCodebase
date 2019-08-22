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

// Class    :: CAttribute, CAttributeMgr
// Header   :: Attributes.h
// Function :: The Attribute class is used for stat handling of actors. The Attribute Manager class
//			:: is used to hold a store of loaded Attributes within the GameWorld.

#pragma warning(disable:4786)


#include <algorithm>
#include <map>
#include <cassert>

#include "MudCore.h"
#include "../gTools/Tools.h"
#include "../gTools/gString.h"
#include "../gTools/Maths.h"
#include "../gTools/Log.h"
#include "../gTools/TinyXml.h"

#include "GameObjects.h"
#include "Attributes.h"

CAttribute::CAttribute()
{
	m_gsKey = "Invalid";

	m_nValues[ATTR_MIN] = 0;
	m_nValues[ATTR_MAX] = 0;
	m_nValues[ATTR_CUR] = 0;
}


CAttribute::CAttribute(gString gsKey, int nMin, int nMax, int nCur)
{
	assert( gsKey.Length() != 0 );

	m_gsKey = gsKey;

	m_nValues[ATTR_MIN] = nMin;
	m_nValues[ATTR_MAX] = nMax;
	m_nValues[ATTR_CUR] = nCur;
}

CAttribute::CAttribute(const CAttribute& rhs)
{
	//
	// You should not be here. Most likely, you have some code that looks like;
	// CAttribute attr = m_Attributes["some_attr"]. what the code should look like is
	// CAttribute& attr = m_Attributes["some_attr"]
	//
	assert(0 && "Use CAttribute::Copy() instead.");
}

CAttribute& CAttribute::Copy(const CAttribute& rhs)
{
	m_nValues[ATTR_MIN] = rhs.m_nValues[ATTR_MIN];
	m_nValues[ATTR_MAX] = rhs.m_nValues[ATTR_MAX];
	m_nValues[ATTR_CUR] = rhs.m_nValues[ATTR_CUR];

	return *this;
}

CAttribute& CAttribute::operator = (int n)
{
	m_nValues[ATTR_CUR] = Clamp(n, m_nValues[ATTR_MIN], m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator +=  (int n)
{
	m_nValues[ATTR_CUR] = ClampUp(m_nValues[ATTR_CUR] + n, m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator -=  (int n)
{
	m_nValues[ATTR_CUR] = ClampDn(m_nValues[ATTR_CUR] - n, m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator /=  (int n)
{
	if ( n == 0 )
		return *this;

	m_nValues[ATTR_CUR] = Clamp(m_nValues[ATTR_CUR] / n, m_nValues[ATTR_MIN], m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator *=  (int n)
{
	m_nValues[ATTR_CUR] = Clamp(m_nValues[ATTR_CUR] * n, m_nValues[ATTR_MIN], m_nValues[ATTR_MAX]);

	return *this;
}


////////////////////
////////////////////
////////////////////
CAttribute& CAttribute::operator +=  (const CAttribute& Other)
{
	m_nValues[ATTR_CUR] = ClampUp(m_nValues[ATTR_CUR] + Other.m_nValues[ATTR_CUR], m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator -=  (const CAttribute& Other)
{
	m_nValues[ATTR_CUR] = ClampDn(m_nValues[ATTR_CUR] - Other.m_nValues[ATTR_CUR], m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator /=  (const CAttribute& Other)
{
	if ( Other.m_nValues[ATTR_CUR] == 0 )
		return *this;

	m_nValues[ATTR_CUR] = Clamp(m_nValues[ATTR_CUR] / Other.m_nValues[ATTR_CUR], m_nValues[ATTR_MIN], m_nValues[ATTR_MAX]);

	return *this;
}

CAttribute& CAttribute::operator *=  (const CAttribute& Other)
{
	m_nValues[ATTR_CUR] = Clamp(m_nValues[ATTR_CUR] * Other.m_nValues[ATTR_CUR], m_nValues[ATTR_MIN], m_nValues[ATTR_MAX]);

	return *this;
}



std::ostream& operator << ( std::ostream& stream, const CAttribute& Attr )
{
	// Name : <Min,Max,Cur>
	// pad it out to make it pretty
	int nMaxLen = 17;

	gString gsName(Attr.Name());

	if ( gsName.Length() < nMaxLen )
		gsName.Pad(gsName.Length(), ' ', nMaxLen - gsName.Length());

	stream << " " << gsName << " : " << "<" << Attr.Min() << "," << Attr.Max() << "," << Attr.Cur() << ">" << "\n";
	return stream;
}


std::istream& operator >> ( std::istream& stream, CAttribute& Attr )
{
	int nMin, nMax, nCur;
	gString gsName;
	char c;

	// Name : <Min,Max,Cur>
	stream >> gsName >> c >> c >> nMin >> c >> nMax >> c >> nCur >> c;

	stream.ignore();
	Attr.SetKey(gsName);
	Attr.SetMin(nMin);
	Attr.SetMax(nMax);
	Attr.SetCur(nCur);

	return stream;
}



CAttributeMgr::CAttributeMgr(const CAttributeMgr& rhs)
{
	AttributeMap::const_iterator c_itor;

	Clear();

	for ( c_itor = rhs.m_Attributes.begin(); c_itor != rhs.m_Attributes.end(); c_itor++ )
	{
		CAttribute* pSrc = (*c_itor).second;

		Add(pSrc->Name(), pSrc->Min(), pSrc->Max(), pSrc->Cur());
	}
}


void CAttributeMgr::Clear()
{
	AttributeMap::iterator _itor;

	for (_itor = m_Attributes.begin(); _itor != m_Attributes.end(); _itor++)
	{
		CAttribute* pAttribute = (*_itor).second;
		delete pAttribute;
	}

	m_Attributes.clear();
}

bool CAttributeMgr::Add(const gString& gsAttr, int nMin, int nMax, int nCur, bool bOverWrite)
{
	AttributeMap::iterator a_itor;

	if ( gsAttr.Length() > CGameObjects::Get().ConfigData().GetInt("attribute_length_limit", "Options") )
	{
		g_Log.Log(LOG_ERROR, "[CAttributeMgr::Add] Attribute '%s' name exceeds length limits.", (const char*)gsAttr);
		return false;
	}

	a_itor = m_Attributes.find(gsAttr.GetHash());

	if ( a_itor == m_Attributes.end() )
	{
		CAttribute* pAttr = new CAttribute(gsAttr, nMin, nMax, nCur);

		m_Attributes.insert(AttributeMap::value_type(pAttr->Name().GetHash(), pAttr));
		return true;
	}
	else
	if ( bOverWrite )
	{
		CAttribute& Attr = *(*a_itor).second;

		// Name is allready the same
		Attr.SetMin(nMin);
		Attr.SetMax(nMax);
		Attr.SetCur(nCur);

		return true;
	}

	return false;
}

//
// Take the attributes of rhs, and add them (or update existing) to lhs
//
CAttributeMgr& CAttributeMgr::operator += (const CAttributeMgr& rhs)
{
	AttributeMap::const_iterator c_itor;

	for ( c_itor = rhs.m_Attributes.begin(); c_itor != rhs.m_Attributes.end(); c_itor++ )
	{
		CAttribute* pSrc = (*c_itor).second;

		Add(pSrc->Name(), pSrc->Min(), pSrc->Max(), pSrc->Cur(), true);
	}

	return *this;
}


CAttributeMgr& CAttributeMgr::operator = (const CAttributeMgr& rhs)
{
	AttributeMap::const_iterator c_itor;

	Clear();

	for ( c_itor = rhs.m_Attributes.begin(); c_itor != rhs.m_Attributes.end(); c_itor++ )
	{
		CAttribute* pSrc = (*c_itor).second;

		Add(pSrc->Name(), pSrc->Min(), pSrc->Max(), pSrc->Cur(), false);
	}

	return *this;
}

CAttribute& CAttributeMgr::operator[] (const gString& gsAttr)
{
	AttributeMap::iterator a_itor;
	int nMaxLen = CGameObjects::Get().ConfigData().GetInt("attribute_length_limit", "Options");

	if ( gsAttr.GetLength() > nMaxLen )
		g_Log.Log(LOG_WARNING, "[CAttributeMgr::[]] Requested attribute name '%s' is longer than configured limit.", (const char*)gsAttr);

	a_itor = m_Attributes.find(gsAttr.GetHash());

	if ( a_itor == m_Attributes.end() )
	{
		// Add new attribute
		if ( !Add(gsAttr,1,6,1) )
		{
			g_Log.Log(LOG_CRITICAL, "[CAttributeMgr::[]] Unable to add non-existing yet requested attribute <%s>.", (const char*)gsAttr);
			abort();
		}

		a_itor = m_Attributes.find(gsAttr.GetHash());

		if ( a_itor == m_Attributes.end() )
		{
			g_Log.Log(LOG_CRITICAL, "[CAttributeMgr::[]] Unable to add non-existing yet requested attribute <%s>.", (const char*)gsAttr);
			abort();
		}
	}

	return *(CAttribute*)(*a_itor).second;
}

std::ostream& operator << ( std::ostream& stream, const CAttributeMgr& Mgr )
{
	AttributeMap::const_iterator a_itor;

	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Attributes]");

	for (a_itor = Mgr.m_Attributes.begin(); a_itor != Mgr.m_Attributes.end(); a_itor++)
		stream << *(static_cast<CAttribute*>((*a_itor).second));

	Tools.WriteLn(stream, "[/Attributes]");

	return stream;
}

std::istream& operator >> ( std::istream& stream, CAttributeMgr& Mgr )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	streampos marker;
	gString gsKey, gsString;
	bool bDone = false;

	try
	{
		if ( Tools.ReadKey(stream) == "[Attributes]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey == "[/Attributes]" || (gsKey.Length() > 0 && gsKey[0] == EOF) )
					bDone = true;
				else
				{
					stream.seekg(marker);
					
					CAttribute* pAttribute = new CAttribute;

					stream >> *pAttribute;

					AttributeMap::iterator _i = Mgr.m_Attributes.find( pAttribute->Name().GetHash() );
	
					// This caused a nasty memory leak -- we have to be sure we dont 
					// add this twice, because it wont actually get added twice to the map,
					// which means it wont get deleted later...
					if (_i != Mgr.m_Attributes.end())
					{
						delete pAttribute;
						continue;
					}
					else
						Mgr.m_Attributes.insert(AttributeMap::value_type(pAttribute->Name().GetHash(), pAttribute));
				}
			}
		}
	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CAttributeMgr::>>] Error encountered while reading file.");
	}

	return stream;
}

void CAttributeMgr::WriteXml(TiXmlNode* pParent)
{
	if ( m_Attributes.size() == 0 )
		return;

	AttributeMap::iterator _a;
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pAttributeNode = Tools.InsertXmlChild(pParent, "Attributes");

	for (_a = m_Attributes.begin(); _a != m_Attributes.end(); _a++)
	{
		CAttribute* pAttr = (*_a).second;
		
		TiXmlElement* pElement = static_cast<TiXmlElement*>(Tools.InsertXmlChild(pAttributeNode, (const char*)pAttr->Name()));

		int nMin = pAttr->Min(), nMax = pAttr->Max(), nCur = pAttr->Cur();

		pElement->SetAttribute("min", (nMin));
		pElement->SetAttribute("max", (nMax));
		pElement->SetAttribute("current", (nCur));
	}
}

void CAttributeMgr::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pAttributeNode = pParent->FirstChild("Attributes");

	if ( pAttributeNode != NULL )
	{
		TiXmlElement* pElement = static_cast<TiXmlElement*>(pAttributeNode->FirstChild());

		while ( pElement )
		{
			gString gsName = pElement->Value();

			const char* szMin = pElement->Attribute("min");
			const char* szMax = pElement->Attribute("max");
			const char* szCur = pElement->Attribute("current");

			AttributeMap::iterator _i = m_Attributes.find(gsName.GetHash());

			// Attribute doesn't exist, so we add it
			if ( _i == m_Attributes.end() && szMin && szMax && szCur )
			{
				CAttribute* pAttr = new CAttribute(gsName, atoi(szMin), atoi(szMax), atoi(szCur));

				m_Attributes.insert(AttributeMap::value_type(pAttr->Name().GetHash(), pAttr));
			}
			// Attribute exists, so we update their values
			else
			{
				(*_i).second->SetCur(atoi(szCur));
				(*_i).second->SetMax(atoi(szMax));
				(*_i).second->SetMin(atoi(szMin));
			}

			pElement = (TiXmlElement*)(pElement->NextSibling());
		}

	}
}
