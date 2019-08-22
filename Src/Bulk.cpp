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

// Class    :: CBulk
// Header   :: Spatial.h
// Function :: Implements the Bulk Storage Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 10. Bulk Class 
///////////////////////////////////////////////////////////////////////////////////////////

CBulk::CBulk()
{
	m_Type = (CComponent::CT_BULKSTORAGE);
	m_Install.insert(InstallMap::value_type(CModule::MT_FUEL_CELLS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_FABRICATION_MATERIAL, true));
}


CBulk::~CBulk()
{
	m_Type = 0;
}

void CBulk::Update()
{
	// Nothing to Update	
}

void CBulk::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	return;
} 


void CBulk::ReadExtra(TiXmlNode* pParent)
{

}
