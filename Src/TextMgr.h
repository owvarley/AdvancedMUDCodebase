//
// HelpSystem
//
// This file contains the integrated help system for MudCore
//


#ifndef __TEXTMGR_H__
#define __TEXTMGR_H__

#include <map>

#include "../gTools/gString.h"

class CActor;
class TiXmlNode;

class CTextEntry
{
// Methods
public:
	CTextEntry() {m_nReserved=0;}
	virtual ~CTextEntry() {};

	inline  const   gString&		GetName() const { return m_gsName; }
	inline  const   gString&		GetData() const { return m_gsData; }
	inline  const   gString&		GetDate() const { return m_gsDate; }
	inline  const	gStringList&	GetLinks() const { return m_gsLinks; }

	inline	void					SetName(const gString& gsName)
									{ m_gsName = gsName; }

	inline	void					SetData(const gString& gsData)
									{ m_gsData = gsData; }

	inline  void					SetDate(const gString& gsDate)
									{ m_gsDate = gsDate; }

	inline  void					SetReserved(int nData)
									{ m_nReserved = nData; }

	inline	void					AddLink(const gString& gsLink)
									{ m_gsLinks.push_back(gsLink); }

// Data
private:
	gString		m_gsName;
	gString		m_gsDate;
	gString		m_gsData;
	gStringList m_gsLinks;
	long		m_nReserved;
};

typedef std::map<gString, CTextEntry*> t_mTexts;

class CTextMgr
{
// Methods
public:
	CTextMgr() {};
	virtual ~CTextMgr();

	inline const int			Size() const { return m_Entries.size(); }
	inline const t_mTexts&		GetEntries() const { return m_Entries; }
	CTextEntry*					GetEntry(const gString& gsArgument);
	gString						Search(const gString& gsArgument);
	bool						Initialize(const gFileName& gsFile);
	bool						HandleTextRequest(CActor* pA, const gString& gsEntry);

	void						WriteXml(TiXmlNode* pParent);
	void						ReadXml(TiXmlNode* pParent);

// Data
private:
	t_mTexts	m_Entries;
};


#endif //__TEXTMGR_H__
