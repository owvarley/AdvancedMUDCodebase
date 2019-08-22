//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.
//
// The Advanced MUD Codebase Project
// AMC, copyright (c) 2005, 2006 by Owen Varley <owen#sw-erp.org>

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// File     :: Epacs.cpp
// Header   :: Epacs.h
// Function :: Holds all the functions relevant to the Extensive Player Character System (ePacs)

// Each GameWorld will have its own SkillMgr, this allows each GameWorld
// to define a separate set of skills which can be used within it
// Attribute Mgr should be ported into the ePAC class so we can integrate
// skills and attributes.

//
// The Extensive Player Character System (ePACs) is the brain child of Norway's finest Games
// designer, Ken Mikkelson. The system draws inspiration from almost every table top RPG
// system currently in existence. Created for the SW-ERP Project 05/06.
//
//
// Version      : 0.70
// Last Updated : 30/07/2006
// 
// Created by:
//				Ken Mikkelson (N'kEnNy)  <ken@sw-erp.org>
// Additional help by:
//	            Olav Bondal			(Olav)     <olav@sw-erp.org>
//				Owen Varley			(Nekekami) <owen@sw-erp.org>
//				Chaz Van Den Born	(Chaz)	   <chaz@sw-erp.org>
//

#include "MudCore.h"
#include "GameServer.h"
#include "GameObjects.h"
#include "Tools.h"
#include "../gTools/Log.h"
#include "Epacs.h"


// Determines the number of dice rolled for raising purposes
// For example: 10 would mean rolling a 1d10 and obtaining the
// result 1. 1800 would be rolling a 1d1800 and obtaining a result
// of 1.
int CEpacs::szSkillBoundary[] = {	10,	20,	30,	40,	50, 
									60,	70,	80,	90,  100, 
									200,  300,  400,  500,  600, 
									700,  800,  900, 1000, 1100,
								   1200, 1300, 1400, 1500, 1600,
								   1700, 1800, 1900, 2000, 2100,
								   NULL };

// String representation of the Categories
char* CSkill::szCategories[] = { "Physical", "Mental", "Social", NULL };

char* CSkill::szTypes[] = { "Ability", "Skill", "Talent", "Subskill", NULL };

// Abilities
char* CSkill::szAbilities[] = {
	// Physical
	"Combat",		"Exploration",		"War",
	// Mental
	"Technology",	"Space",			"Subterfuge",
	// Social
	"Authority",	"Persuasion",		"Galaxy",
	NULL };
	
int CSkill::nAbilities[] = {
	0,		0,		0,
	1,		1,		1,
	2,		2,		2,
};

///////////////////////////////////////////////////////////////////////////
// Evolved Player Avatar Core
///////////////////////////////////////////////////////////////////////////
CEpacs::CEpacs()
{
	
}

CEpacs::~CEpacs()
{
	m_gsFileName 	= "";
	m_lVersion		= 0;
	m_Skills.clear();
	m_SkillIndex.clear();
	m_TestResults.clear();
}

CSkill* CEpacs::GetSkill(gString gsName)
{
	if (gsName.IsEmpty())
		return NULL;

	SkillIndex::iterator find = m_SkillIndex.find(gsName);

	if (find == m_SkillIndex.end())
		return NULL;

	return GetSkill((*find).second);
}

CSkill* CEpacs::GetSkill(int nSkill)
{
	SkillMgrMap::iterator skill = Skills().find(nSkill);

	if (skill == Skills().end())
		return NULL;
	else
		return (*skill).second;

	return NULL;

}	

bool CEpacs::AddSkill(CSkill* pSkill)
{
	LOG_SCOPE("CEpacs::AddSkill");

	if (!pSkill)	
		return false;

	// Check if there is already a skill with this name
	if (m_SkillIndex.find(pSkill->Name()) != m_SkillIndex.end())
	{
		g_Log.Log(LOG_ERROR, "Trying to add a duplicate skill.");
		return false;
	}

	// First generate our skill number and add this skill to our Skill Map		
	pSkill->SetId(GetNextId());

	m_Skills.insert(SkillMgrMap::value_type(pSkill->Id(), pSkill));

	// Now add this skill to our index table
	m_SkillIndex.insert(SkillIndex::value_type(pSkill->Name(), pSkill->Id()));

	// Finished adding it all in so we save it now
	Save();
	return true;
}

bool CEpacs::DeleteSkill(int nSkill)
{
	gString gsName;

	// Need to obtain an iterator so we can delete the object from both the SkillMgr and SkillIndex maps
	SkillMgrMap::iterator smDel;
	SkillIndex::iterator siDel;

	smDel = m_Skills.find(nSkill);
	
	// First we remove the Skill from the SkillMgr Map
	if (smDel == m_Skills.end())
		return false;
	else
	{
		gsName = ((*smDel).second)->Name();
		m_Skills.erase(smDel);
	}

	// Now we also need to remove the skill from the Index Table
	if (siDel == m_SkillIndex.end())
		return false;
	else
		m_SkillIndex.erase(siDel);

	// We have successfully delete the skill from both maps
	return true;

}

bool CEpacs::ValidSkill(gString gsName)
{
	if (gsName.IsEmpty())
		return false;

	// Check to see if this name is contained within our Skill Index
	if (m_SkillIndex.find(gsName) == m_SkillIndex.end())
		return false;
	else
		return true;

}

bool CEpacs::ValidSkill(int nSkill)
{
	// Check the Skill Mgr to see if this skill number has been assigned
	if (m_Skills.find(nSkill) == m_Skills.end())
		return false;
	else
		return true;

}

bool CEpacs::DisableSkill(int nSkill)
{
	CSkill* pSkill = GetSkill(nSkill);

	if (!pSkill)
		return false;


	// This function is a toggle, so if the skill is disabled it re-enables it and vice-versa
	pSkill->SetDisabled(!pSkill->Disabled());

	return true;
}

bool CEpacs::LogSkill(int nSkill)
{
	CSkill* pSkill = GetSkill(nSkill);

	if (!pSkill)
		return false;

	// This function is a toggle, so if the skill is already being logged we stop logging it
	pSkill->SetLogged(!pSkill->Logged());

	return true;
}

// Increases the test output for a specific type of roll
bool CEpacs::IncreaseTest(gString gsType)
{
	IntegerMap::iterator it;

	// If it doesn't exist we add it
	if ((it = m_TestResults.find(gsType)) == m_TestResults.end())
	{
		m_TestResults.insert(IntegerMap::value_type(gsType, 1));
		return true;
	}
	else
	{
		(*it).second++;
	}

	return true;
}

bool CEpacs::ResetTest()
{
	m_TestResults.clear();
	CGameObjects::Get().GameWorld()->Write("#301EPACs#300 Test data reset#700\n\r");

	return true;
}


bool CEpacs::OutputTest()
{
	CGameObjects::Get().GameWorld()->Write("#301[#300ePACS Test Results#301]#700\n\r\n\r");

	int nNum = (*m_TestResults.find("NumberOfRolls")).second;
	int nTally = 0;

	CGameObjects::Get().GameWorld()->Write("#701TOTALS #301[#300%d rolls#301]#700:#700\n\r", nNum);
	for (IntegerMap::iterator it = m_TestResults.begin(); it != m_TestResults.end(); it++)
	{
		if ((*it).first == "NumberOfRolls")
			continue;

		if ((*it).first == "Hits" || (*it).first == "Rerolls")
			CGameObjects::Get().GameWorld()->Write("#201%-15s #701[#700%3d#700]\n\r", (*it).first, (*it).second);
		else
			CGameObjects::Get().GameWorld()->Write("#101%-15s #701[#700%3d#700]\n\r", (*it).first, (*it).second);
		nTally += (*it).second;
	}

	CGameObjects::Get().GameWorld()->Write("#701AVERAGES #301[#300%d rolls#301]#700:#700\n\r", nNum);
	for (IntegerMap::iterator it = m_TestResults.begin(); it != m_TestResults.end(); it++)
	{
		if ((*it).first == "NumberOfRolls")
			continue;
		if ((*it).first == "Hits" || (*it).first == "Rerolls")
			CGameObjects::Get().GameWorld()->Write("#201%-15s #701[#700%3.2f#700]\n\r", (*it).first, (((float)(*it).second)/nTally));
		else
			CGameObjects::Get().GameWorld()->Write("#101%-15s #701[#700%3.2f#700]\n\r", (*it).first, (((float)(*it).second)/nTally));
	}
	
	return true;
}

// Standard e-pac(s) Roll
//////////////////////////
// Dice Pool = Skill + Attribute +/- World modifiers
// 
// For every dice in the pool a d6 is rolled with the following results:
// 1    = Penalty miss (-1 from Success Pool)
// 2-4  = Miss (nothing)
// 5    = Hit (+1 to Success Pool)
// 6    = Hit + Reroll (+1 to Success pool) Max 10 rerolls
//
// If Success Poll is less than 1 we get a Chance Roll:
// 1    = Critical failure
// 2-5  = failure
// 6    = Hit + Reroll
int CEpacs::Roll(float fSkillRating, int nAttributeRating, int nWorldModifiers)
{
	CRandom* pRandom = CGameObjects::Get().Rand();

	// Dice Pool is the sum of Skill, Attribute and World Modifiers
	int nDicePool = (int)fSkillRating + nAttributeRating + nWorldModifiers;
	int nNumThrown = 0;

	// Success pool is a tally of all our successful rolls
	int nSuccessPool = 0;
	int nNumberRerolls = 0;

	// For each Dice in the pool we roll a d6
	while (nNumThrown < nDicePool)
	{
		CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("NumberOfRolls");

		switch (pRandom->D6())
		{
			// Penalty miss - subtract one
			case 1: nSuccessPool--;
				CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("Penalty");	// Store number of penalty misses
				break;

			// Miss - do nothing
			case 2:
			case 3:			
			case 4:
				CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("Miss");  // Store number of misses
				break;

			// Hit - add one 
			case 5: nSuccessPool++;
				CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("Hits");	// Store number of hits
				break; 

			// Hit + Reroll
			case 6:
				{
					int nReroll = 6;
					while (nReroll == 6 && nNumberRerolls < 10)
					{
						CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("Hits");	// Increase number of hits
						nSuccessPool++;	// We've rolled a six or just entered so get an increase
						nReroll = pRandom->D6();	// Make the reroll
						
						
						if (nReroll == 5)			// Five is a hit
							nSuccessPool++;		

						if (nReroll == 6)
							CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("Rerolls");
						
						nNumberRerolls++;
					}
					nNumberRerolls = 0;

				}
				break;
			// End roll determination
		}

		// Increase the number we've thrown so far
		nNumThrown++;
	}
	// We now have our success pool
	
	// Chance roll
	if (nSuccessPool < 1)
	{
		switch (pRandom->D6())
		{
			// Critical failure
			// Thanks Ken, but whats that mean?
			case 1: CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("CriticalFailure");
				break;

			case 2:
			case 3:
			case 4:
			case 5:
				CGameObjects::Get().GameWorld()->Epacs()->IncreaseTest("Miss");
				break;

			// Hit + Reroll
			case 6:
				{
					int nReroll = 6;
					while (nReroll == 6 && nNumberRerolls < 10)
					{
						nSuccessPool++;	// We've rolled a six or just entered so get an increase
						nReroll = pRandom->D6();	// Make the reroll
						nNumberRerolls++;
					}
					nNumberRerolls = 0;

				}
				break;
			// End chance roll
		}
	}

	pRandom = NULL;
	nDicePool = 0;
	nNumberRerolls = 0;

	return nSuccessPool;
}
// Simple Action roll
////////////////////////
// Success Pool is rolled then compared to the Task Threshold:
// if Success pool is greater then the task is a success
// If its less than or equal to the threshold then the task is a failure.
// Remaining successes count towards completing the task
int CEpacs::Roll(float fSkillRating, int nAttributeRating, int nWorldModifiers, int nTaskThreshold)
{
	// We define our SuccessPool
	int nSuccessPool = 0;

	// We roll to determine our success pool
	nSuccessPool = Roll(fSkillRating, nAttributeRating, nWorldModifiers);

	// Simple action is ... Simple!	
	return nSuccessPool - nTaskThreshold;
}

// Opposed Action roll
/////////////////////
// An Opposed action is utilized when one or more character, be it NPC or PC
// is in direct opposition of one another to complete a certain task. Such as
// bargaining for goods, combat, or simply racing. 
// 
// 1. Relevant dice Pool is determined and rolled for all contestants.
// 2. Task threshold is subtraced from Success Pool(s).
// 3. Remaining Success Pool(s) are compared. The one with the highest Pool is the winner.
// --3.a Additional successes might result in better performance/results.
// --3.b In the event of a stalemate the contestant with the highest base pool is the winner.
// --3.c In the event of a Stalemate in Base Pools the winner is determined randomly.

int CEpacs::RollO(float fSkill, int nAttribute, int nWorldMods, float fVictSkill, int nVictAttribute, int nVictWorldMods, int nTaskThreshold)
{
	int nCharSuccessPool = 0;
	int nVictSuccessPool = 0;

	// Compute the player's dice rolls
	nCharSuccessPool = Roll(fSkill, nAttribute, nWorldMods);
	nVictSuccessPool = Roll(fVictSkill, nVictAttribute, nWorldMods);

	// Subtract the task threshold from the players
	nCharSuccessPool -= nTaskThreshold;
	nVictSuccessPool -= nTaskThreshold;

/*
	CGameObjects::Get().GameWorld()->Write("#301[#300ePACS Opposed Roll#301]\n\r"); 
	CGameObjects::Get().GameWorld()->Write("#300> #101Player1 #701[#700%d#701]#700\n\r", nCharSuccessPool);
	CGameObjects::Get().GameWorld()->Write("#300> #101Player2 #701[#700%d#701]#700\n\r", nVictSuccessPool);
*/

	// Compare the results
	if (nCharSuccessPool == nVictSuccessPool)
	{
		// First stalemate occurance
		if (((int)fSkill + nAttribute + nWorldMods) > ((int)fVictSkill + nVictAttribute + nVictWorldMods))
		{
			// Positive result will mean Character wins
			if (nCharSuccessPool < 0)
				return -nCharSuccessPool;
			else
				return nCharSuccessPool;
		}
		else if (((int)fSkill + nAttribute + nWorldMods) < ((int)fVictSkill + nVictAttribute + nVictWorldMods))
		{
			// Negative result will mean Victim
			if (nVictSuccessPool < 0)
				return nVictSuccessPool;
			else 
				return -nVictSuccessPool;
		}
		else if (((int)fSkill + nAttribute + nWorldMods) == ((int)fVictSkill + nVictAttribute + nVictWorldMods))
		{
			// Random winner
			return CGameObjects::Get().Rand()->NumberRange(-1,1);
		}

	}
	else if (nCharSuccessPool > nVictSuccessPool)
	{
		if (nCharSuccessPool < 0)
			return -nCharSuccessPool;
		else
			return nCharSuccessPool;
	}
	else
	{
		if (nVictSuccessPool < 0)
			return nVictSuccessPool;
		else 
			return -nVictSuccessPool;
	}
		

	return 0;
}

// Determine our next Unqiue ID, to do this we iterate through our list of skills
// and find the first free number, in ascending order.
int CEpacs::GetNextId()
{
	int nCounter = 1;

	while (m_Skills.find(nCounter) != m_Skills.end())
	{
		nCounter++;
	}

	// Our nCounter is now the lowest non assigned id so we return it
	return nCounter;
}

bool CEpacs::IsEnabled(int nSkill)
{
	CSkill* pSkill = GetSkill(nSkill);

	if (!pSkill)
		return false;

	return pSkill->Disabled();
}

bool CEpacs::IsLogged(int nSkill)
{
	CSkill* pSkill = GetSkill(nSkill);

	if (!pSkill)
		return false;
		
	return pSkill->Logged();
}

// We iterate through the Skill Map and for each entry we add one
// to the Skill Index. To save on processor cycles we check if a key
// exists in the Skill Index, we only add it if it doesn't exist already.
// The Skill index provides a way for linking skill id's to the actual skills.
bool CEpacs::GenerateIndexTable()
{
	for (SkillMgrMap::iterator skill = m_Skills.begin(); skill != m_Skills.end(); skill++)
	{
		if (m_SkillIndex.find((*skill).second->Name()) == m_SkillIndex.end())
		{
			m_SkillIndex.insert(SkillIndex::value_type((*skill).second->Name(), (*skill).second->Id()));
		}
	}

	return true;

}

// Displays a specific skill, called within the DisplaySkills function
// We only display those subskills which the player already has the predecessor for.
// For example before selecting anything, a player will only see a list of abilities
void CEpacs::DisplaySkill(CSkill* pSkill, CMobile* pM, int nLevel, bool bCurrent)
{
	bool bDisplay = true;	// Used for subskill determination
	float fRating = 0.0f;	// Used for Skill rating

	// If they want the skills of the player, we need to show them their highest skills in
	// a sub skill.
	if (bCurrent)
	{
		// Check if this is a subskill, we need to get the highest subskill and display it only
		// This is a subskill if it has a child that is also a subskill
		for (IntegerList::iterator ski = pSkill->Children().begin(); ski != pSkill->Children().end(); ski++)
		{
			CSkill* pChild = GetSkill(*ski);

			if (pChild->Type() == CSkill::_SUBSKILL)
				bDisplay = false;
		}
	}

	if (bDisplay)
	{
		// Check that they know this skill before showing it
		if (bCurrent)
		{
			// Find the skill in their Skill Map
			SkillMap::const_iterator search = pM->Skills().find(pSkill->Id());

			if (search != pM->Skills().end())
			{
				// If it is in the map, check it is rated
				if ((*search).second <= 0)
					bDisplay = false;
				else
					fRating = (*search).second;
			}
			else
			{
				bDisplay = false;
			}			
		}
	}

	if (bDisplay)
	{
		for (int i = 0; i < nLevel+1; i++)
		{
			if (pSkill->Type() == CSkill::_SKILL)
				pM->Write("#600*");
			else if (pSkill->Type() == CSkill::_SUBSKILL)
				pM->Write("#500*");
			else
				pM->Write("#100*");
		}

		pM->Write(" #701%d %s\n\r", pSkill->Id(), pSkill->Name());
	}
	
	// Display all children
	for (IntegerList::iterator ski = pSkill->Children().begin(); ski != pSkill->Children().end(); ski++)
	{
		CSkill* pChild = GetSkill(*ski);
		DisplaySkill(pChild, pM, nLevel+1, bCurrent);			
	}
	return;
}


// Displays a player's skills
// bCurrent determines whether we are showing what the player currently has or what they can
// get, for advancing skills
void CEpacs::DisplaySkills(CMobile* pM, bool bCurrent)
{
	float fRating = 0.0;

	for (SkillMgrMap::iterator it = m_Skills.begin(); it != m_Skills.end(); it++)
	{
		CSkill* pSkill = (*it).second;

		// Only list ability
		if (pSkill->Type() == CSkill::_ABILITY)
		{
			// Check if they actually know the Ability now
			if (bCurrent)
			{
				SkillMap::const_iterator search = pM->Skills().find((*it).first);

				if (search != pM->Skills().end())
				{
					if ((*search).second <= 0)
						continue;
					else
						fRating = (*search).second;
				}
				else
				{
					continue;
				}			

			}

			if (!bCurrent)
				pM->Write("#601*#701 %s\n\r", pSkill->Name());
			else
				pM->Write("#601*#701 %s [%0.0f]\n\r", pSkill->Name(), fRating);


			for (IntegerList::iterator ski = pSkill->Children().begin(); ski != pSkill->Children().end(); ski++)
			{
				CSkill* pChild = GetSkill(*ski);
				DisplaySkill(pChild, pM, 1, bCurrent);				
			}
		}

	}
	return;
}

int CEpacs::GetCost(int nSkill)
{
	int nPoints = 0;
	CSkill* pSkill = GetSkill(nSkill);

	if (pSkill)
	{
		switch (pSkill->Type())
		{
			case CSkill::_ABILITY:
			case CSkill::_TALENT:
			case CSkill::_SKILL:
				nPoints++;
				break;

			// Subskills are different
			// First level costs 1 point, each other level costs 2
			case CSkill::_SUBSKILL:
				{
					CSkill * pParent = GetSkill(pSkill->Prereq());

					if (pParent && pParent->Type() == CSkill::_ABILITY)
						nPoints++;
					else
						nPoints += 2;

					pParent = NULL;
				}
				break;

			default:
				nPoints++;
				break;
		}
	}

	return nPoints;
}

int CEpacs::GetCost(gString gsName)
{
	if (gsName.IsEmpty())
		return NULL;

	SkillIndex::iterator find = m_SkillIndex.find(gsName);

	if (find == m_SkillIndex.end())
		return NULL;

	return GetCost((*find).second);
}

bool CEpacs::ReadSkillXml(TiXmlNode* pNode, CSkill* pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();
	TiXmlNode* pSkillNode = pNode->FirstChild("Skill");

	while( pSkillNode != NULL )
	{
		// Create the new Skill
		CSkill* pSkill = new CSkill();
		
		gString gsFile;

		Tools.ReadXml(pSkillNode, "name", gsFile);
		
		// Load it from file and add it to our Epacs manager
		if (pSkill->Load(gsFile))
			AddSkill(pSkill);

		// Add it as a child to its parent
		pParent->AddChild(pSkill);

		// Now we have to check if it has any children and load them also
		ReadSkillXml(pSkillNode, pSkill);

		pSkillNode = pSkillNode->NextSibling("Skill");
	}

	return true;
}

bool CEpacs::WriteSkillXml(TiXmlNode* pNode, CSkill* pParent)
{

	return true;
}

// Write data to XML file
bool CEpacs::WriteXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "version",	m_lVersion);

	for (SkillMgrMap::iterator skill = m_Skills.begin(); skill != m_Skills.end(); skill++)
	{
		// Save the skill's filename
		TiXmlNode* pSkill = Tools.InsertXmlChild(pParent, "skill");
		pSkill->SetValue((*skill).second->FileName());
		// Save the skill itself
		(*skill).second->Save();
	}

	return true;
}

// Read data from XML file
bool CEpacs::ReadXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent, "version",	m_lVersion);

	// Iterate through Epacs file and create an entry in m_Skills for
	// each loaded skill
	TiXmlNode* pSkillNode = pParent->FirstChild("Skill");

	while (pSkillNode != NULL)
	{
		// Create the new Skill
		CSkill* pSkill = new CSkill();

		gString gsFile;

		Tools.ReadXml(pSkillNode, "name", gsFile);
		
		// Load it from file and add it to our Epacs manager
		if (pSkill->Load(gsFile))
			AddSkill(pSkill);

		// Now we have to check if it has any children and load them also
		ReadSkillXml(pSkillNode, pSkill);

		pSkillNode = pSkillNode->NextSibling("Skill");
	}



	// We've loaded all the skills now, we need to generate our IndexTable
	GenerateIndexTable();

	return true;
}

bool CEpacs::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Epacs");

	ReadXml(pNode);
	
	return true;
}

bool CEpacs::Load(gString szFile)
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)szFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Epacs");

	ReadXml(pNode);
	
	return true;
}

bool CEpacs::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%s%s", CGameObjects::Get().GameWorld()->m_gsAreaListFile.AbsolutePath(), m_gsFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Epacs");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}

// Check to see if this skill gets raised
bool CEpacs::CheckRaise(float fSkill, int nIntelligence)
{
	bool bRaise = false;

	// Make sure we don't exceed the maximum skill level
	if (fSkill >= _MAXSKILLLEVEL)
		return false;

	// Raising a skill depends on the level of our skill
	// The check for raising a skill is:
	// 	INTdBoundary == 1
	
	CRandom* pRandom = CGameObjects::Get().Rand();

	int nResult = 0;

	// We roll multiple dice based on our intelligence
	while (nIntelligence > 0)
	{
		nResult = pRandom->NumberRange(1, szSkillBoundary[(int)fSkill]);

		// We only raise the skill on a result of 1
		if (nResult == 1)
		{
			bRaise = true;
			break;
		}

		// Keep trying to roll a 1 till we run out of Intelligence
		nIntelligence--;
	}

	// Did we manage to roll a 1?
	if (bRaise)
		return true;
	else
		return false;

}

///////////////////////////////////////////////////////////////////////////////////////////
// Skill Number
///////////////////////////////////////////////////////////////////////////////////////////

/*
CSkillNumber::CSkillNumber()
{
	this->m_nId = 0;
}

CSkillNumber::CSkillNumber(int nNumber)
{
	this->m_nId = nNumber;
}


CSkillNumber::~CSkillNumber()
{
	this->m_nId = 0;
} */

///////////////////////////////////////////////////////////////////////////////////////////
// Skill
///////////////////////////////////////////////////////////////////////////////////////////

CSkill::CSkill()
{
	this->m_nId = 1;	// Always starts at 1
	this->m_Flags = new CSet;
}

CSkill::~CSkill()
{
	m_nId = 0;
	delete m_Flags;
	m_Flags = NULL;

}

// Write data to XML file
bool CSkill::WriteXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteXml(pParent, "name",		m_gsName);
	Tools.WriteXml(pParent, "prereq",	m_gsPrereq);
	Tools.WriteXml(pParent, "type",		m_nType);
	Tools.WriteXml(pParent, "category",	m_nCategory);
	Tools.WriteXml(pParent, "flags",	*m_Flags);
	
	return true;
}

// Read data from XML file
bool CSkill::ReadXml(TiXmlNode * pParent)
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.ReadXml(pParent,	"name",		m_gsName);
	Tools.ReadXml(pParent,  "prereq",	m_gsPrereq);
	Tools.ReadXml(pParent,  "type",		m_nType);
	Tools.ReadXml(pParent,	"category",	m_nCategory);
	Tools.ReadXml(pParent,  "flags",	*m_Flags);

	return true;
}

bool CSkill::Load()
{
	TiXmlDocument doc;

	if ( !doc.LoadFile((const char*)m_gsFileName) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Skill");

	ReadXml(pNode);
	
	return true;
}

bool CSkill::Load(gString szFile)
{
	TiXmlDocument doc;

	szFile.Format("%sSkills\\%s", CGameObjects::Get().GameWorld()->m_gsAreaListFile.AbsolutePath(), szFile);
	
	if ( !doc.LoadFile((const char*)szFile) )
		return false;

	TiXmlNode *pNode = doc.FirstChild("Skill");

	ReadXml(pNode);
	
	return true;
}

bool CSkill::Save()
{
	CGameObjects& globals = CGameObjects::Get();
	gString szFile;

	szFile.Format("%s%s", CGameObjects::Get().GameWorld()->m_gsAreaListFile.AbsolutePath(), m_gsFileName);

	TiXmlDocument doc;
	TiXmlNode *pXmlNode = globals.Tools()->InsertXmlChild(&doc, "Skill");

	WriteXml(pXmlNode);

	return doc.SaveFile((const char*)szFile);
}
