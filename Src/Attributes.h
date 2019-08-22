//
// Attributes.h
// Definition of the CAttribute and CAttributeMgr classes
//


//
// Essentially, all attributes are stored in a map, & indexed with a 3 character
// 'key' for easy lookups. Essential operators, such as ++, --, +=, -= etc, have
// been implemented for ease of use.
//

#ifndef __ATTRIBUTES_H__
#define __ATTRIBUTES_H__

#pragma warning(disable:4786)
#include <map>

#include "../gTools/gString.h"
#include "MudCore.h"
#include "../gTools/TinyXml.h"

class CAttribute
{
// Methods
public:
	typedef enum t_eAttributePos
	{ ATTR_MIN, ATTR_MAX, ATTR_CUR, ATTR_NUM };

	CAttribute();
	CAttribute(gString gsKey, int nMin, int nMax, int nCur);
	CAttribute(const CAttribute& rhs);
	~CAttribute() {};

	gString			Key()  const { return m_gsKey; }
	gString			Name() const { return m_gsKey; }

	CAttribute&		Copy(const CAttribute& rhs);

	inline void		SetKey(gString gsNew) { m_gsKey = gsNew; }

	inline void     Set(int nMin, int nMax, int nCurrent)
					{ m_nValues[ATTR_MIN] = nMin; m_nValues[ATTR_MAX] = nMax; m_nValues[ATTR_CUR] = nCurrent; }

	inline void		SetMin(int nValue)    { m_nValues[ATTR_MIN] = nValue; }
	inline void		SetMax(int nValue)    { m_nValues[ATTR_MAX] = nValue; }
	inline void		SetCur(int nValue)	  { m_nValues[ATTR_CUR] = nValue; }

	inline int		Min() const { return m_nValues[ATTR_MIN]; }
	inline int		Max() const { return m_nValues[ATTR_MAX]; }
	inline int		Cur() const { return m_nValues[ATTR_CUR]; }

	// all operators perform their tasks on the ATTR_CUR value only,
	// and each operator uses ATTR_MIN and ATTR_MAX as floor/ceiling values
	CAttribute&	operator =  (int n);
	CAttribute&	operator += (int n);
	CAttribute& operator -= (int n);
	CAttribute& operator /= (int n);
	CAttribute& operator *= (int n);

	CAttribute& operator += (const CAttribute& rhs);
	CAttribute& operator -= (const CAttribute& rhs);
	CAttribute& operator /= (const CAttribute& rhs);
	CAttribute& operator *= (const CAttribute& rhs);

	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CAttribute& Attr );
	friend std::istream& operator >> ( std::istream& stream, CAttribute& Attr );


// Data
private:
	gString m_gsKey;

	int		m_nValues[ATTR_NUM]; // min, max, current
};

typedef std::map<int, CAttribute*> AttributeMap;

class CAttributeMgr
{
// Methods
public:
	CAttributeMgr() {};
	~CAttributeMgr() {Clear();}
	CAttributeMgr(const CAttributeMgr& rhs);

	// Erase all elements in the map
	void Clear();

	// Add a new attribute. Pay attention to name length limits
	bool Add(const gString& gsAttr, int nMin, int nMax, int nCur, bool bOverWrite=false);

	// querry the class for the attribute
	CAttribute& operator[] (const gString& gsAttr);
	// override attributes with those from another mgr
	CAttributeMgr& operator=(const CAttributeMgr& rhs);
	// add rhs to lhs, update as necessary
	CAttributeMgr& operator+=(const CAttributeMgr& rhs);

	CAttribute* GetAttr(const gString& gsAttr);

	// Classes that need access
	friend class CmdScore;
	friend class CmdMemory;
	friend class CmdRace;
	friend class CChargenMnu;

	// stream handlers
	friend std::ostream& operator << ( std::ostream& stream, const CAttributeMgr& Mgr );
	friend std::istream& operator >> ( std::istream& stream, CAttributeMgr& Mgr );
	void WriteXml(TiXmlNode* pParent);
	void ReadXml(TiXmlNode* pParent);

// Data
protected:
	AttributeMap	m_Attributes;
};

#endif // __ATTRIBUTES_H__

