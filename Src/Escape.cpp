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

// Class    :: CEscape
// Header   :: Spatial.h
// Function :: Implements the Escape Pod Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Escape Class 
///////////////////////////////////////////////////////////////////////////////////////////

CEscape::CEscape()
{
	m_Type = (CComponent::CT_ESCAPEPOD);
	m_Install.insert(InstallMap::value_type(CModule::MT_ESCAPE_POD, true));
}


CEscape::~CEscape()
{
	m_Type = 0;
}

void CEscape::Update()
{

}

void CEscape::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	return;
}


void CEscape::ReadExtra(TiXmlNode* pParent)
{
	return;
}
