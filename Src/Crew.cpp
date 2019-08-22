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

// Class    :: CCrew, CCLoader, COrder, Crew Events
// Header   :: Crew.h
// Function :: Handles the functions for a Ships Crew

#pragma warning(disable:4786)


#include "GameObjects.h"
#include "Tools.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Spatial.h"
#include "Crew.h"

#include "../gTools/Log.h"

char* CCrew::szTypes[] = { "Gunnery Team", "Engineers", "Bridge Crew", "Pilots", "Troops", NULL };

char* CCrew::szStates[] = { "Off duty", "On QRA", "Reporting for Duty", "On Duty", "Awaiting Orders", "Performing Orders", NULL };

CCrew::CCrew()
{
	m_gsName	 = "";
	m_gsFileName = "";
	m_gsLeader	 = "";

	m_Leader	 = NULL;
	m_Ship		 = NULL;
	m_Location	 = new CPlacement;
	m_Home		 = new CPlacement;
	m_nCrewState = 0;

	m_nType		 = 0;
	m_fSkill	 = 0.0;
	m_nUniqueID	 = (long)time(0);

	m_gsOrder	 = "";

	m_ncCompliment = 0;
	m_nmCompliment = 0;

}

CCrew::~CCrew()
{
	m_gsName	 = "";
	m_gsFileName = "";
	m_gsLeader	 = "";

	delete m_Leader;
	m_Leader	 = NULL;
	m_Ship		 = NULL;

	delete m_Location;
	m_Location	 = NULL;

	delete m_Home;
	m_Home		 = NULL;
	
	m_nCrewState = 0;

	m_nType		 = 0;
	m_fSkill	 = 0.0;
	m_nUniqueID	 = 0;

	m_gsOrder	 = "";

	m_ncCompliment = 0;
	m_nmCompliment = 0;

	m_Orders.clear();
}

bool CCrew::Load(gString filename)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)filename) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Ship");

	ReadXml(pNode);

	if (this->m_Ship)
	{
		CrewMap::iterator it = m_Ship->m_Crew.find(m_nType);

		if (it == m_Ship->m_Crew.end())
		{
			// This is the first crew of this type to be added
			CrewList pList;
			pList.push_back(this);
			m_Ship->m_Crew.insert(CrewMap::value_type(m_nType, pList));
		}
		else
		{
			((*it).second).push_back(this);
		}	

		if (this->m_Leader && (this->m_Home->Area() != 0 || this->m_Home->Room() != 0 || this->m_Home->World() != 0))
		{
			CGameObjects::Get().GameWorld()->Add(this->m_Leader, *this->m_Home);
		}
	}

	return true;
}

bool CCrew::Load()
{	
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Ship");

	ReadXml(pNode);

	if (this->m_Ship)
	{
		CrewMap::iterator it = m_Ship->m_Crew.find(m_nType);

		if (it == m_Ship->m_Crew.end())
		{
			// This is the first crew of this type to be added
			CrewList pList;
			pList.push_back(this);
			m_Ship->m_Crew.insert(CrewMap::value_type(m_nType, pList));
		}
		else
		{
			((*it).second).push_back(this);
		}	

		if (this->m_Leader && (this->m_Home->Area() != 0 || this->m_Home->Room() != 0 || this->m_Home->World() != 0))
		{
			CGameObjects::Get().GameWorld()->Add(this->m_Leader, *this->m_Home);
		}
	}

	return true;
}

bool CCrew::Save()
{	
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "Crew");

	gString gsCrew;
	gsCrew.Format("%sCrews\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], m_gsFileName);

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsCrew);
}

// Crew Update cycle
void CCrew::Update()
{
	float fThisUpdate = CGameObjects::Get().Clock();

	if ( EventCount() > 0 )
		HandleEvents();

	if ( m_nCrewState == CCrew::_REPORTING)
	{
		if (this->m_Leader)
		{
			if (this->m_Leader->Position() == *m_Location)
			{
				m_nCrewState = CCrew::_ONDUTY;

				gString gsMsg;
				gsMsg.Format("%s, %s reporting for duty Commander!\n\r", CCrew::szTypes[m_nType], m_gsName);
				this->m_Leader->Report(m_Ship->m_Commander, gsMsg);
				this->m_Leader->ExecuteCommand("emote", "salutes crisply.");
			}
			else if (this->m_Leader->Position() == *m_Home)
			{
				m_nCrewState = CCrew::_OFFDUTY;
				this->m_Leader->Report(m_Ship->m_Commander, "Off duty, Commander!");
				this->m_Leader->ExecuteCommand("emote", "returns from duty and begins to relax.");
			}
		}

	}

	// Only update every 3 seconds or if forced
	if ( fThisUpdate - m_fLastUpdate >= fActorUpdateDelta )
	{
		m_fLastUpdate = fThisUpdate;
	}
}

void CCrew::ReadXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();

	Tools.ReadXml(pParent, "name",			m_gsName);
	Tools.ReadXml(pParent, "filename",		m_gsFileName);
	
	gString gsTemp;
	Tools.ReadXml(pParent, "ship",			gsTemp);

	// Add crew to ship
	if (pGalaxy->GetShi(gsTemp))
	{
		m_Ship = pGalaxy->GetShi(gsTemp);														
	}
	else
		m_Ship = NULL;

	if (m_Ship)
		Tools.WriteXml(pParent, "ship",		m_Ship->m_gsName);

	TiXmlNode* pLeader = pParent->FirstChild("Actor");

	if (pLeader != NULL)
	{
		m_Leader->ReadXml(pParent);
	}

	Tools.ReadXml(pParent, "location",		*m_Location);
	Tools.ReadXml(pParent, "home",			*m_Home);
	Tools.ReadXml(pParent, "crewstate",		m_nCrewState);
	Tools.ReadXml(pParent, "skill",			m_fSkill);
	Tools.ReadXml(pParent, "type",			m_nType);
	Tools.ReadXml(pParent, "uid",			m_nUniqueID);
	Tools.ReadXml(pParent, "currcomp",		m_ncCompliment);
	Tools.ReadXml(pParent, "maxcomp",		m_nmCompliment);

	return;

}

void CCrew::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",			m_gsName);
	Tools.WriteXml(pParent, "filename",		m_gsFileName);
	if (m_Ship)
		Tools.WriteXml(pParent, "ship",		m_Ship->m_gsName);
	if (m_Leader)
	{
		TiXmlNode* pActorNode = Tools.InsertXmlChild(pParent, "Actor");
		m_Leader->WriteXml(pActorNode);
	}

	Tools.WriteXml(pParent, "location",		*m_Location);
	Tools.WriteXml(pParent, "home",			*m_Home);
	Tools.WriteXml(pParent, "crewstate",	m_nCrewState);
	Tools.WriteXml(pParent, "skill",		m_fSkill);
	Tools.WriteXml(pParent, "type",			m_nType);
	Tools.WriteXml(pParent, "uid",			m_nUniqueID);
	Tools.WriteXml(pParent, "currcomp",		m_ncCompliment);
	Tools.WriteXml(pParent, "maxcomp",		m_nmCompliment);

	return;

}


std::ostream& operator << ( std::ostream& stream, const CCrew& crew )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Crew]");
	Tools.WriteLn(stream, " Name             : \"%s\"", crew.m_gsName);
	Tools.WriteLn(stream, " FileName         : \"%s\"", crew.m_gsFileName);
	if (crew.m_Ship)
		Tools.WriteLn(stream, " Ship             : \"%s\"", crew.m_Ship->m_gsName);
	if (crew.m_Leader && crew.m_Leader->IsNPC())
		stream << *crew.m_Leader;
	stream <<			  " Location         : " << *crew.m_Location;
	stream <<			  " Home             : " << *crew.m_Home;
	Tools.WriteLn(stream, " CrewState        : %d", crew.m_nCrewState);
	Tools.WriteLn(stream, " Skill            : %f", crew.m_fSkill);
	Tools.WriteLn(stream, " Type             : %d", crew.m_nType);
	Tools.WriteLn(stream, " UID              : %d", crew.m_nUniqueID);
	Tools.WriteLn(stream, " CurrComp         : %d", crew.m_ncCompliment);
	Tools.WriteLn(stream, " MaxComp          : %d", crew.m_nmCompliment);
	Tools.WriteLn(stream, "[/Crew]");

	return stream;
}

std::istream& operator >> ( std::istream& stream, CCrew& crew )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	CGalaxy* pGalaxy = CGameObjects::Get().GameWorld()->Galaxy();
	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	try
	{
		if ( Tools.ReadKey(stream) == "[Crew]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Crew]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{
					case 'C':
						if ( gsKey == "CrewState" )
							Tools.ReadData(stream, crew.m_nCrewState);
						if ( gsKey == "CurrComp" )
							Tools.ReadData(stream, crew.m_ncCompliment);
						break;
					
					case 'F':
						if ( gsKey == "FileName" )
							Tools.ReadData(stream, crew.m_gsFileName);
						break;

					case 'H':
						if ( gsKey == "Home" )
							Tools.ReadData(stream, *crew.m_Home);
						break;

					case 'L':
						if ( gsKey == "Location" )
							Tools.ReadData(stream, *crew.m_Location);
						break;

					case 'M':
						if ( gsKey == "MaxComp")
							Tools.ReadData(stream, crew.m_nmCompliment);
						break;

					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, crew.m_gsName);
						break;

					case 'S':
						if ( gsKey == "Ship" )
						{
							gString gsTemp;
							Tools.ReadData(stream, gsTemp);

							if (pGalaxy->GetShi(gsTemp))
							{
								// Add crew to ship
								crew.m_Ship = pGalaxy->GetShi(gsTemp);														
							}
							else
								crew.m_Ship = NULL;


						}
						if ( gsKey == "Skill" )
							Tools.ReadData(stream, crew.m_fSkill);
						break;

					case 'T':
						if ( gsKey == "Type")
							Tools.ReadData(stream, crew.m_nType);
						break;

					case 'U':
						if ( gsKey == "UID")
							Tools.ReadData(stream, crew.m_nUniqueID);
						break;	

					case '[':
						if ( gsKey == "[Actor]" )
						{
							CActor* pNpc = new CActor();
							
							stream.seekg(marker);

							stream >> *pNpc;

							crew.m_Leader = pNpc;
						}
						break;



					default:
						Tools.Report(E_ERROR, "[CCrew::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Crew]");
			}
		}

	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CCrew::>>] Error encountered while reading Spatial file..");
	}

	return stream;
}


///////////////////////////////////////////////////////////////////////////////////////////
// 1. Module Loader Class
///////////////////////////////////////////////////////////////////////////////////////////
CCLoader::CCLoader()
{

}

CCLoader::~CCLoader()
{
	this->m_Crews.clear();
}

void CCLoader::ReadXml(TiXmlNode* pParent)
{
	LOG_SCOPE("CCLoader::ReadXml");

	CTools& Tools = *CGameObjects::Get().Tools();

	TiXmlNode* pCrewNode = pParent->FirstChild("Crew");

	while (pCrewNode != NULL)
	{
		gString gsFile;

		Tools.ReadXml(pCrewNode, "filename",	gsFile);

		gsFile.Format("%sCrews\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);
		
		CCrew* pCrew = new CCrew();

		if (pCrew->Load(gsFile))
			m_Crews.push_back(pCrew);
		else
			g_Log.Log("Unable to load crew: %s", gsFile);

		pCrewNode = pCrewNode->NextSibling("Crew");

	}
	return;

}

void CCLoader::WriteXml(TiXmlNode* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	
	for (CrewList::iterator crew = m_Crews.begin(); crew != m_Crews.end(); crew++)
	{
		TiXmlNode* pCrewNode = Tools.InsertXmlChild(pParent, "Crew");
		Tools.WriteXml(pCrewNode, "filename", (*crew)->m_gsFileName);
	}

	return;

}


std::ostream& operator << ( std::ostream& stream, const CCLoader& loader )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	
	Tools.WriteLn(stream, "[Loader]");

	for (int j = 0; j < loader.m_Crews.size(); j++)
	{
		gString gsType;

		Tools.WriteLn(stream, "[Crew]", gsType);
		Tools.WriteLn(stream, " Filename      : \"%s\"", loader.m_Crews.at(j)->m_gsFileName);
		Tools.WriteLn(stream, "[/Crew]");

		loader.m_Crews.at(j)->Save();
		
	}

	Tools.WriteLn(stream, "[/Loader]");

	return stream;
}

std::istream& operator >> ( std::istream& stream, CCLoader& loader )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	bool bFound = false;
	streampos marker;
	CGameObjects& globals = CGameObjects::Get();

	try
	{
		if ( Tools.ReadKey(stream) == "[Loader]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Loader]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{

					case '[':
						if (gsKey == "[Crew]")
						{
							gString gsName = gsKey;
							gString gsFile;

							CCrew* pCrew = new CCrew();
													
							if ( (gsFile = Tools.ReadKey(stream)) == "Filename" )
							{
								Tools.ReadData(stream, gsFile);

								gsFile.Format("%sCrews\\%s", globals.m_Config.szDir[CGameObjects::_SPACEDIR], gsFile);						
								pCrew->Load(gsFile);
								loader.m_Crews.push_back(pCrew);
							}

						}
						break;		

					default:
						Tools.Report(E_ERROR, "[CCLoader::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Loader]");
			}
		}

	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CCLoader::>>] Error encountered while reading loader file..");
	}

	return stream;
}

bool CCLoader::Load(gFileName gfFile)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)gfFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("CrewLoader");

	ReadXml(pNode);
}

bool CCLoader::Save()
{
	TiXmlDocument doc;
	TiXmlNode *pXmlNode = CGameObjects::Get().Tools()->InsertXmlChild(&doc, "CrewLoader");

	gString gsCrew;
	gsCrew.Format("%sCrews\\%s", CGameObjects::Get().m_Config.szDir[CGameObjects::_SPACEDIR], "Crews.dat");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)gsCrew);
}

////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// Event/Order Handling //////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////
bool CCrew::HandleEvent(CEvent& Event)
{
	bool bHandled = false;

	switch ( Event.m_EventCode )
	{
		case COrder::_REPORT:
			bHandled = OnReport(Event);
			break;
		case COrder::_SPEED:
			bHandled = OnSpeed(Event);
			break;
		case COrder::_COMPLETE:
			bHandled = OnComplete(Event);
			break;
		case COrder::_COURSE:
			bHandled = OnCourse(Event);
			break;
		case COrder::_ROLL:
			bHandled = OnRoll(Event);
			break;
		case COrder::_POWER:
			bHandled = OnPower(Event);
			break;
		case COrder::_TRANSMIT:
			bHandled = OnTransmit(Event);
			break;
		case COrder::_BROADCAST:
			bHandled = OnBroadcast(Event);
			break;
		case COrder::_TIGHTBEAM:
			bHandled = OnTightbeam(Event);
			break;
		case COrder::_COMM:
			bHandled = OnComm(Event);
			break;
		case COrder::_SWEEP:
			bHandled = OnSweep(Event);
			break;
		case COrder::_STATUS:
			bHandled = OnStatus(Event);
			break;
		case COrder::_LAUNCH:
			bHandled = OnLaunch(Event);
			break;
		case COrder::_LAND:
			bHandled = OnLand(Event);
			break;
		case COrder::_TARGET:
			bHandled = OnTarget(Event);
			break;

		break;
	}
	
	return bHandled;

}

// Order Name :: Report
// Purpose    :: Report for Duty at Duty station
// Type       :: _REPORT
// Outline    :: This will simply create the Leader Actor then
//            :: use pathfinding to trace a route from their present
//            :: position to their duty station.
// Written    :: 17/02/2006 {OWV}
bool CCrew::OnReport(CEvent &Event)
{
	EReport* pEvent = (EReport*)(&Event);

	if (this->m_Leader)
	{
		this->m_nCrewState = CCrew::_REPORTING;
	}
	else
	{
		return true;
	}
		
	CDirectionList List;
	
	if (this->m_Leader->HomeWorld()->Navigator()->BuildPathTo(List, this->m_Leader, *pEvent->m_Location))
	{
		((CMobile*)this->m_Leader)->FollowPath(List, 30.0f);
	}

	return true;
	
}

// Order Name :: Speed
// Purpose    :: Order a Speed to be Set
// Type       :: _SPEED
// Outline    :: Will simply cause the Bridge Crew to set a destination speed
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnSpeed(CEvent &Event)
{
	ESpeed* pE = (ESpeed*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	// Validation
	if (pShip->TopSpeed(false) <= 0)
	{
		this->m_Leader->Report(pShip->m_Commander, "Commander, we have no Ion Engines installed!");
		return true;
	}

	if (pShip->TopSpeed(true) <= 0)
	{
		this->m_Leader->Report(pShip->m_Commander, "Commander, our Sublight Drives are not online.");
		return true;
	}

	if (!pShip->m_ShipState->IsSet(CShip::_LAUNCHING) && !pShip->m_ShipState->IsSet(CShip::_FLYING))
	{
		this->m_Leader->Report(pShip->m_Commander, "Commander, we must launch first.");
		return true;
	}

	this->m_nCrewState = _BUSY;
	this->m_Leader->ExecuteCommand("Speed", "%d", pE->m_nSpeed);

	if (pShip->m_Speed > pE->m_nSpeed)
		this->m_Leader->Report(pShip->m_Commander, "Aye Commander, Decelerating to %d MGLTs", pE->m_nSpeed);
	else
		this->m_Leader->Report(pShip->m_Commander, "Aye Commander, Accelerating to %d MGLTs", pE->m_nSpeed);



	pShip = NULL;
	return true;
}

// Order Name :: Complete
// Purpose    :: Report an Order completed
// Type       :: _COMPLETE
// Outline    :: Will give a Verbal report and set the Crew's status
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnComplete(CEvent &Event)
{
	EComplete* pEvent = (EComplete*)(&Event);

	switch (pEvent->m_nType)
	{
		case COrder::_SPEED:
			{
				m_Leader->Report(m_Ship->m_Commander, "Attained %d MGLTs, Commander!", m_Ship->m_Speed);
				m_nCrewState = _AWAITING;
				break;
			}
		case COrder::_COURSE:
			{
				m_Leader->Report(m_Ship->m_Commander, "New Course %d %d attained, Commander!", m_Ship->m_Heading->z, m_Ship->m_Heading->y);
				m_nCrewState = _AWAITING;
				break;
			}
		case COrder::_ROLL:
			{
				m_Leader->Report(m_Ship->m_Commander, "Roll completed, Commander!");
				m_nCrewState = _AWAITING;
				break;
			}
		case COrder::_SWEEP:
			{
				m_Leader->Report(m_Ship->m_Commander, "Sweep completed, Commander!");
				m_nCrewState = _AWAITING;
				break;
			}
		case COrder::_LAUNCH:
			{
				m_Leader->Report(m_Ship->m_Commander, "We've entered space Commander!");
				m_nCrewState = _AWAITING;
				break;
			}
		case COrder::_LAND:
			{
				m_Leader->Report(m_Ship->m_Commander, "Landed sequence completed, Commander.");
				m_nCrewState = _AWAITING;
				break;
			}
		case COrder::_TARGET:
			{
				m_Leader->Report(m_Ship->m_Commander, "Target locked, Commander.");
				m_nCrewState = _AWAITING;
				break;
			}

		
	}

	return true;
}

// Order Name :: Course
// Purpose    :: Set a Course
// Type       :: _COURSE
// Outline    :: Will order the crew to set a new course
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnCourse(CEvent &Event)
{
	ECourse* pEvent = (ECourse*)(&Event);
	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	if (pShip->Maneuver(false) <= 0)
	{
		m_Leader->Report(pShip->m_Commander, "Commander, We do not have any Maneuvering Engines installed!");
		return true;
	}

	if (pShip->Maneuver(true) <= 0)
	{
		m_Leader->Report(pShip->m_Commander, "Commander, We have no Steering Engines and Maneuvering Thrusters online.");
		return true;
	}

	// Are they trying to roll while landed?
	if (pShip->m_ShipState->IsSet(CShip::_LANDED) || pShip->m_ShipState->IsSet(CShip::_REPULSOR))
	{
		m_Leader->Report(pShip->m_Commander, "Commander, We are still on the ground!");
		return true;
	}

	// Ok clear validation so lets start the orders
	this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("course", "%d %d", pEvent->m_nBearing, pEvent->m_nMark);

	m_Leader->Report(pShip->m_Commander, "Aye Commander, Making Course %d %d.", pEvent->m_nBearing, pEvent->m_nMark);

	return true;
}

// Order Name :: Roll
// Purpose    :: Roll the ship
// Type       :: _COURSE
// Outline    :: Will order the crew to roll the ship
// Written    :: 19/02/2006 {OWV}
bool CCrew::OnRoll(CEvent &Event)
{
	ERoll* pEvent = (ERoll*)(&Event);
	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	if (pShip->Maneuver(false) <= 0)
	{
		m_Leader->Report(pShip->m_Commander, "Commander, We do not have any Maneuvering Engines installed!");
		return true;
	}

	if (pShip->Maneuver(true) <= 0)
	{
		m_Leader->Report(pShip->m_Commander, "Commander, We have no Steering Engines and Maneuvering Thrusters online.");
		return true;
	}

	// Are they trying to roll while landed?
	if (pShip->m_ShipState->IsSet(CShip::_LANDED) || pShip->m_ShipState->IsSet(CShip::_REPULSOR))
	{
		m_Leader->Report(pShip->m_Commander, "Commander, We are still on the ground!");
		return true;
	}

	// Ok clear validation so lets start the orders
	this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("roll", "%s", pEvent->m_gsRoll);

	m_Leader->Report(pShip->m_Commander, "Aye Commander, Rolling %s.", pEvent->m_gsRoll);

	return true;
}

// Order Name :: Power
// Purpose    :: Power a Subsystem or Module
// Type       :: _POWER
// Outline    :: Will order the crew to modify Power settings
// Written    :: 19/02/2006 {OWV}
bool CCrew::OnPower(CEvent &Event)
{
	EPower* pEvent = (EPower*)(&Event);
	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	// Power validation is handled in CmdPower

	//this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("power", "%s", pEvent->m_gsSystem);

	return true;
}

// Order Name :: Broadcast
// Purpose    :: Broadcast a Message
// Type       :: _BROADCAST
// Outline    :: Will order communications to broadcast a message
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnBroadcast(CEvent &Event)
{
	EBroadcast* pEvent = (EBroadcast*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	m_Leader->ExecuteCommand("broadcast", "%s", pEvent->m_gsMessage);

	return true;
}

// Order Name :: Course
// Purpose    :: Set a Course
// Type       :: _COURSE
// Outline    :: Will order the crew to set a new course
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnTransmit(CEvent &Event)
{
	ETransmit* pEvent = (ETransmit*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	m_Leader->ExecuteCommand("transmit", "%s", pEvent->m_gsMessage);

	return true;
}

// Order Name :: Course
// Purpose    :: Set a Course
// Type       :: _COURSE
// Outline    :: Will order the crew to set a new course
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnTightbeam(CEvent &Event)
{
	ECourse* pEvent = (ECourse*)(&Event);
	return true;
}

// Order Name :: Comm
// Purpose    :: Modify comms options
// Type       :: _COMM
// Outline    :: Will order the crew to setup the comms array
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnComm(CEvent &Event)
{
	EComm* pEvent = (EComm*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;


	// We do not set the Busy state as this command is instant
	m_Leader->ExecuteCommand("comm", "%s %s %s", pEvent->m_gsFunction, pEvent->m_gsValue == "" ? "" : pEvent->m_gsValue, pEvent->m_gsValue2 == "" ? "" : pEvent->m_gsValue2);

	return true;
}

// Order Name :: Sweep
// Purpose    :: Commence a Sweep
// Type       :: _SWEEP
// Outline    :: Will order the crew to Sweep the System
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnSweep(CEvent &Event)
{
	ESweep* pEvent = (ESweep*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("sweep", "%s", pEvent->m_gsRadome);

	return true;
}

// Order Name :: Launch
// Purpose    :: Launch the Ship
// Type       :: _LAUNCH
// Outline    :: Will order the helm to launch
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnLaunch(CEvent &Event)
{
	ELaunch* pEvent = (ELaunch*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("launch", "");

	return true;

}

// Order Name :: Land
// Purpose    :: Land the Ship
// Type       :: _LAND
// Outline    :: Will order the helm to Land
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnLand(CEvent &Event)
{
	ELand* pEvent = (ELand*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("land", "%s", pEvent->m_gsLocation);

	return true;
}

// Order Name :: Target
// Purpose    :: Target an enemy Ship
// Type       :: _TARGET
// Outline    :: Will order gunnery to track and lock a target
// Written    :: 19/03/2006 {OWV}
bool CCrew::OnTarget(CEvent &Event)
{
	ETarget* pEvent = (ETarget*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	this->m_nCrewState = _BUSY;
	m_Leader->ExecuteCommand("target", "%s", pEvent->m_gsTarget);

	return true;
}


// Order Name :: Status
// Purpose    :: Report Status
// Type       :: _STATUS
// Outline    :: Will order the crew to report in
// Written    :: 18/02/2006 {OWV}
bool CCrew::OnStatus(CEvent &Event)
{
	EStatus* pEvent = (EStatus*)(&Event);

	CShip* pShip = this->m_Ship;

	if (!pShip)
		return false;

	if (!m_Leader)
		return false;

	CArea* pArea = CGameObjects::Get().GetArea(*m_Location);
	CRoom* pRoom = NULL;

	if (pArea)
		pRoom = pArea->GetRoom(*m_Location);

	this->m_Leader->Report(pShip->m_Commander, "%s, %s reporting. Crew %d/%d, %s", this->m_gsName, this->m_Leader->Name(), m_ncCompliment, m_nmCompliment, CCrew::szStates[m_nCrewState]);//, pRoom ? pRoom->Name() : "No Station");

	return true;

}



EReport::EReport() : CEvent(COrder::_REPORT)
{

}

ESpeed::ESpeed() :	 CEvent(COrder::_SPEED), m_nSpeed(0)
{

}

EComplete::EComplete() : CEvent(COrder::_COMPLETE), m_nType(0)
{

}

ECourse::ECourse() : CEvent(COrder::_COURSE), m_nBearing(0), m_nMark(0)
{

}

ERoll::ERoll() : CEvent(COrder::_ROLL)
{

}

EPower::EPower() : CEvent(COrder::_POWER)
{

}

ETransmit::ETransmit() : CEvent(COrder::_TRANSMIT)
{

}

EBroadcast::EBroadcast() : CEvent(COrder::_BROADCAST)
{

}

ETightbeam::ETightbeam() : CEvent(COrder::_TIGHTBEAM)
{

}

EComm::EComm() : CEvent(COrder::_COMM)
{

}

ESweep::ESweep() : CEvent(COrder::_SWEEP)
{

}

EStatus::EStatus() : CEvent(COrder::_STATUS)
{

}

ELaunch::ELaunch() : CEvent(COrder::_LAUNCH)
{

}

ELand::ELand() : CEvent(COrder::_LAND)
{

}

ETarget::ETarget() : CEvent(COrder::_TARGET)
{

}

COrder::COrder()
{
	this->m_nOrder = 0;
	this->m_nTimer = 0;
}

COrder::COrder(int nType, int nTimer)
{
	this->m_nOrder = nType;
	this->m_nTimer = nTimer;
}

COrder::~COrder()
{
	this->m_nOrder = 0;
	this->m_nTimer = 0;
	this->m_Arguments.clear();
}