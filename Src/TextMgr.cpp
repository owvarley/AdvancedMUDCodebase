//
// CTextMgr
//

#pragma warning(disable:4786)

#include <map>
#include <fstream>

#include "TextMgr.h"
#include "User.h"
#include "Socket.h"
#include "GameObjects.h"

#include "../gTools/gString.h"
#include "../gTools/Tools.h"
#include "../gTools/TinyXml.h"
#include "../gTools/Log.h"

using namespace std;

CTextMgr::~CTextMgr()
{
	t_mTexts::iterator _h;

	for (_h = m_Entries.begin(); _h != m_Entries.end(); _h++)
	{
		CTextEntry* pEntry = (*_h).second;

		delete pEntry;
	}

	m_Entries.clear();
}

CTextEntry*	CTextMgr::GetEntry(const gString& gsArgument)
{
	CTextEntry* pEntry = NULL;
	t_mTexts::const_iterator _h;

	_h = m_Entries.find(gsArgument);

	if ( _h != m_Entries.end() )
		return (*_h).second;

	for (_h = m_Entries.begin(); _h != m_Entries.end(); _h++)
	{
		CTextEntry* pEntry = (*_h).second;

		if ( pEntry->GetName().HasPrefix(gsArgument) )
			return pEntry;
	}

	return NULL;
}

gString CTextMgr::Search(const gString& gsArgument)
{
	gString gsData;
	CTextEntry* pEntry = GetEntry(gsArgument);

	if ( pEntry )
		gsData = pEntry->GetData();

	return gsData;
}

void CTextMgr::WriteXml(TiXmlNode* pParent)
{
}

void CTextMgr::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	TiXmlNode* pTextEntry = pParent->FirstChild("Entry");

	while ( pTextEntry != NULL )
	{
		gString gsKeyWord;
		gString gsData;
		gString gsDate;

		CTextEntry* pText = new CTextEntry;
	
		Tools.ReadXml(pTextEntry, "key", gsKeyWord);
		Tools.ReadXml(pTextEntry, "text", gsData);
		Tools.ReadXml(pTextEntry, "date", gsDate);

		pText->SetName(gsKeyWord);
		pText->SetData(gsData);
		pText->SetDate(gsDate);

		m_Entries.insert(t_mTexts::value_type(gsKeyWord, pText));

		pTextEntry = pTextEntry->NextSibling("Entry");
	}
}

bool CTextMgr::Initialize(const gFileName& gsFile)
{
	TiXmlDocument doc;
	LOG_SCOPE("CTextMgr::Initialize");

	if ( !doc.LoadFile((const char*)gsFile) )
	{
		g_Log.Log(LOG_ERROR, "Unable to initialize TextMgr.");
		return false;
	}

	TiXmlNode *pNode = doc.FirstChild("TextSystem");

	ReadXml(pNode);

	g_Log.Log(LOG_INFO, "Added %d Text Entries\n", m_Entries.size());
	return true;
}

bool CTextMgr::HandleTextRequest(CActor* pA, const gString& gsEntry)
{
	gString gs = Search(gsEntry);

	if ( gs.Length() > 0 )
	{
		pA->Write("%s", (const char*)gs);
		return true;
	}

	return false;
}
