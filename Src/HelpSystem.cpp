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

// Class    :: CHelpSystem, CHelpEntry
// Header   :: HelpSystem.h
// Function :: The HelpSystem handles the loading and fetching of Help files within the game
//			:: it also handles their saving to xml.

#pragma warning(disable:4786)

#include <map>
#include <fstream>

#include "HelpSystem.h"
#include "Socket.h"
#include "User.h"
#include "Actor.h"
#include "GameObjects.h"

#include "../gTools/gString.h"
#include "../gTools/Tools.h"
#include "../gTools/TinyXml.h"
#include "../gTools/Log.h"

using namespace std;

CHelpSystem::~CHelpSystem()
{
	t_mHelps::iterator _h;

	for (_h = m_Helps.begin(); _h != m_Helps.end(); _h++)
	{
		CHelpEntry* pH = (*_h).second;

		delete pH;
	}

	m_Helps.clear();
}

CHelpEntry*	CHelpSystem::GetHelp(const gString& gsArgument)
{
	CHelpEntry* pH = NULL;
	t_mHelps::const_iterator _h;

	_h = m_Helps.find(gsArgument);

	if ( _h != m_Helps.end() )
		return (*_h).second;

	for (_h = m_Helps.begin(); _h != m_Helps.end(); _h++)
	{
		CHelpEntry* pH = (*_h).second;


		if ( pH->GetName().HasPrefix(gsArgument) ) 
			return pH;
	}

	return NULL;
}

gString CHelpSystem::Search(const gString& gsArgument)
{
	gString gsData;
	CHelpEntry* pH = GetHelp(gsArgument);

	if ( pH )
		gsData = pH->GetData();

	return gsData;
}

void CHelpSystem::WriteXml(TiXmlNode* pParent)
{
}

void CHelpSystem::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	TiXmlNode* pHelpEntry = pParent->FirstChild("Entry");

	while ( pHelpEntry != NULL )
	{
		gString gsKeyWord;
		gString gsData;
		int nLevel = 0;

		CHelpEntry* pHelp = new CHelpEntry;
	
		Tools.ReadXml(pHelpEntry, "key", gsKeyWord);
		Tools.ReadXml(pHelpEntry, "level", nLevel);
		Tools.ReadXml(pHelpEntry, "text", gsData);

		pHelp->SetName(gsKeyWord);
		pHelp->SetMinimumLevel(nLevel);
		pHelp->SetData(gsData);

		m_Helps.insert(t_mHelps::value_type(gsKeyWord, pHelp));

		pHelpEntry = pHelpEntry->NextSibling("Entry");
	}
}

bool CHelpSystem::Initialize(const gFileName& gsFile)
{
	LOG_SCOPE("CHelpSystem::>>");

	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gsFile) )
	{
		g_Log.Log(LOG_ERROR, "Unable to initialize HelpSystem.");
		return false;
	}

	TiXmlNode *pNode = doc.FirstChild("HelpSystem");

	ReadXml(pNode);


	g_Log.Log(LOG_INFO, "Loaded %d Helps\n", m_Helps.size());
	return true;
}

bool CHelpSystem::HandleHelpRequest(CUser* pUser, const gString& gsArgument)
{
	CHelpEntry* pHelp = NULL;
	gString gsArg(gsArgument);

	gsArg.MakeLower();

	gsArg.TrimSpacesLeft();
	gsArg.TrimSpacesRight();

	if ( gsArg.Find("help ") == 0 )
		gsArg.Delete(0, 5);

	if ( (pHelp = GetHelp(gsArg)) != NULL )
	{
		long nSize = pHelp->GetData().Length();

		//pUser->Socket()->Write("#600:#601:#701 %s #601:#600:#700 \n\r", pHelp->GetName());
		pUser->Socket()->Write("\n\r%s#700\n\r", pHelp->GetData());
		if ( pHelp->GetLinks().size() > 0 )
		{
			int nCount = 0;
			gString gsLinks;
			pUser->Socket()->Write("See Also [");

			gStringList::const_iterator _g;
			for (_g = pHelp->GetLinks().begin(); _g != pHelp->GetLinks().end(); _g++)
			{
				gsLinks += (*_g);
				if ( ++nCount < pHelp->GetLinks().size() )
					gsLinks += ", ";
			}

			pUser->Socket()->Write("%s]\n\r", gsLinks);
		}

		return true;
	}
	else
		pUser->Socket()->Write("\n\rNo Help is Available on that Topic [%s].\n\r", gsArg);

	return false;
}

