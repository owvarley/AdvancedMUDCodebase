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

// Class    :: CMagazine
// Header   :: Spatial.h
// Function :: Implements the Magazine Component

#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. Magazine Class 
///////////////////////////////////////////////////////////////////////////////////////////

CMagazine::CMagazine()
{
	m_Type = (CComponent::CT_MAGAZINE);
	m_Install.insert(InstallMap::value_type(CModule::MT_ARMOUR_PLATING, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_MISSILES, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_TORPEDOES, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_ROCKETS, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_CHAFF, true));
	m_Install.insert(InstallMap::value_type(CModule::MT_FLARES, true));
	

}


CMagazine::~CMagazine()
{
	m_Type = 0;
}

void CMagazine::Update()
{

}

void CMagazine::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	return;
}


void CMagazine::ReadExtra(TiXmlNode* pParent)
{
	return;
}