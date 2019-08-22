//
// HelpSystem
//
// This file contains the integrated help system for MudCore
//


#ifndef __HELP_SYSTEM_H__
#define __HELP_SYSTEM_H__

#include <map>
#include "../gTools/gString.h"

class TiXmlNode;
class CUser;

class CHelpEntry
{
// Methods
public:
	CHelpEntry() {m_nMinimumLevel=0;}
	virtual ~CHelpEntry() {};

	inline	const	int				GetMinimumLevel() const { return m_nMinimumLevel; }
	inline  const   gString&		GetName() const { return m_gsName; }
	inline  const   gString&		GetData() const { return m_gsData; }
	inline  const	gStringList&	GetLinks() const { return m_gsLinks; }

	inline	void					SetName(const gString& gsName)
									{ m_gsName = gsName; }

	inline	void					SetData(const gString& gsData)
									{ m_gsData = gsData; }

	inline  void					SetMinimumLevel(int nLevel)
									{ m_nMinimumLevel = nLevel; }

	inline	void					AddLink(const gString& gsLink)
									{ m_gsLinks.push_back(gsLink); }
// Data
private:
	gString		m_gsName;
	gString		m_gsData;
	gStringList m_gsLinks;
	int			m_nMinimumLevel;
};

typedef std::map<gString, CHelpEntry*> t_mHelps;

class CHelpSystem 
{
// Methods
public:
	CHelpSystem() {};
	virtual ~CHelpSystem();

	inline const int			Size() const { return m_Helps.size(); }
	inline const t_mHelps&		GetEntries() const { return m_Helps; }
	CHelpEntry*					GetHelp(const gString& gsArgument);
	gString						Search(const gString& gsArgument);
	bool						HandleHelpRequest(CUser* pUser, const gString& gsArgument);
	bool						Initialize(const gFileName& gsFile);

	virtual void				WriteXml(TiXmlNode* pParent);
	virtual void				ReadXml(TiXmlNode* pParent);

// Data
private:
	t_mHelps	m_Helps;
};


#endif //__HELP_SYSTEM_H__
