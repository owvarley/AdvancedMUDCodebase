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

// Class    :: CSFoil
// Header   :: Spatial.h
// Function :: Implements the SFoil Component
#pragma warning(disable:4786)

#include "GameObjects.h"
#include "Spatial.h"
#include <direct.h>


///////////////////////////////////////////////////////////////////////////////////////////
// 11. SFoil Class 
///////////////////////////////////////////////////////////////////////////////////////////

CSFoil::CSFoil()
{
	m_Type = (CComponent::CT_SFOIL);
	m_Install.insert(InstallMap::value_type(CModule::MT_SFOIL_AND_ACTUATORS, true));

}


CSFoil::~CSFoil()
{
	m_Type = 0;
}

void CSFoil::Update()
{

}

void CSFoil::WriteExtra(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	return;
}


void CSFoil::ReadExtra(TiXmlNode* pParent)
{
	return;
}