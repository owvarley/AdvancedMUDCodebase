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

// Class    :: CChargenMnu
// Header   :: ChargenMnu.h
// Function :: Contains all the validation routines needed for the Character Creation Menu

#include <cassert>

#include "../gTools/Tools.h"
#include "../gTools/Set.h"

#include "MudCore.h"
#include "Account.h"
#include "User.h"
#include "Player.h"
#include "GameObjects.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Socket.h"
#include "ChargenMnu.h"


int CChargenMnu::DisplayMenu(CUser* pUser)
{
	// if we start using more than one world, this will have to change
	// to reference the correct world for the character.
	static int nMaxPoints = 20;

	gString gsName;
	gString gsRace;
	gString gsGender;

	if ( pUser->Player() )
	{
		gsName = pUser->Player()->Name();
		gsGender = pUser->Player()->gsGender();
		gsRace = pUser->Player()->Race();
	}

	pUser->Socket()->Write("\n\r#600**#601*#701 Character Creation Menu #601*#600**#700\n\r\n\r"
		" 1#600)#701 Choose a name #600[#601%s#600]#700\n\r"
		" 2#600)#701 Select Gender #600[#601%s#600]#700#700\n\r"
		" 3#600)#701 Select Race #600[#601%s#600]#700\n\r"
		" 4#600)#701 Select Attributes#700\n\r"
		" 5#600)#701 Select Abilities and Skills#700\n\r"
		" 6#600)#701 Select Background#700\n\r"
		" 7#600)#701 View Character#700\n\r"
		" 8#600)#701 Delete Character#700\n\r"
		" 9#600)#701 Reset to Defaults#700\n\r"
		"10#600)#701 Return to Main Menu#700\n\r\n\r"
		"\n\r"
		"Select #600[#7011-10#600]#700 ",
		gsName,
		gsGender,
		gsRace,
		"18,18,18,..." // however we want to display this
		);
	
	return 1;
}

int CChargenMnu::GenericOption(CUser* pUser)
{
	pUser->Socket()->Write("This Menu Option has not been completed.\n\r");

	pUser->SetSubMenu(-1);
	return 1;
}

int CChargenMnu::DisplayDeleteCharacterOptions(CUser* pUser)
{
	pUser->Socket()->Write("Are you sure you want to delete this character? [Yes/No] ");
	pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_DELETE);

	return 1;
}

int CChargenMnu::DisplayRaceOptions(CUser* pUser)
{
	CRaceMgr* pManager = CGameObjects::Get().GameWorld()->RaceMgr();

	// Display intro text
	pUser->Socket()->Write("\n\r\n\r#600**#601*#701 Character Race Selection #601*#600**#701\n\r"
						   "Please select your character's race from the list below. Each race has advantages\n\r"
						   "and disadvantages that can see from the table below. For further information and\n\r"
						   "details check the helpfiles for each race. Type #601help <race>#701 for more detail.\n\r");

	int nCounter = 1;
	for (RaceList::iterator it = pManager->Races().begin(); it != pManager->Races().end(); it++)
	{
		pUser->Socket()->Write("#600[#601%2d#600]#701 %-15s ", nCounter, (*it)->Name());
		for (AttributeMap::iterator attr = (*it)->Attributes().m_Attributes.begin(); attr != (*it)->Attributes().m_Attributes.end(); attr++)
		{
			// Each stat that has a value higher than one is a bonus
			if ((*attr).second->Min() != 0)
			{
				gString gsAttr = (*attr).second->Name();
				gsAttr.MakeProper();
				pUser->Socket()->Write("%s%d#700 %s ", (*attr).second->Min() > 0 ? "#201+":"#101", (*attr).second->Min(), gsAttr);
			}
		}
		pUser->Socket()->Write("\n\r");

		nCounter++;
	}

	pUser->Socket()->Write("Select #600[#7011-%d#600]#700 ", pManager->Races().size());

	return 1;
}

int CChargenMnu::DisplayGenderOptions(CUser* pUser)
{
	if (pUser->Player()->Name().IsEmpty())
		pUser->Socket()->Write("Is your character ");
	else
		pUser->Socket()->Write("And is %s ", pUser->Player()->Name());

	for (int i = 0; i < CMobile::_MAXGENDER; i++)
	{
		if (i == CMobile::_MAXGENDER-1)
			pUser->Socket()->Write("or ");

		pUser->Socket()->Write("%s? ", CMobile::szMobileGender[i]);
	}
 
	return 1;
}

int CChargenMnu::DisplayAttributes(CUser *pUser)
{
	CRaceMgr* pManager = CGameObjects::Get().GameWorld()->RaceMgr();

	// Display intro text
	pUser->Socket()->Write("\n\r\n\r#600**#601*#701 Character Attribute selection #601*#600**#701\n\r"
						   "#701At this stage of character creation you must select the attributes or\n\r"
						   "stats that best describe your character. Each stat listed below has a\n\r"
						   "helpfile with further details. Type #601help <statname>#701 for further\n\r"
						   "details. #600Each point in an attribute costs 1 point, the fifth and sixth\n\r"
						   "points costs two points.#700\n\r\n\r");

	std::map<gString, gString>RacialBonus;

	// We want to display racial bonuses next to stats because Ken is a slave driver
	// only bother if they have selected a race already
	if (!pUser->Player()->Race().IsEmpty())
	{
		for (RaceList::iterator it = pManager->Races().begin(); it != pManager->Races().end(); it++)
		{
			if ((*it)->Name() != pUser->Player()->Race())
				continue;

			for (AttributeMap::iterator attr = (*it)->Attributes().m_Attributes.begin(); attr != (*it)->Attributes().m_Attributes.end(); attr++)
			{
				// Each stat that has a value higher than one is a bonus
				if ((*attr).second->Min() != 0)
				{
					gString gsAttr = (*attr).second->Name();
					gsAttr.MakeProper();
					
					gString gsFull;
					gsFull.Format("%s%d#700", (*attr).second->Min() > 0 ? "#201+":"#101", (*attr).second->Min());

					RacialBonus.insert(std::map<gString, gString>::value_type((*attr).second->Name(), gsFull));
				}
				else
				{
					RacialBonus.insert(std::map<gString, gString>::value_type((*attr).second->Name(), ""));
				}
			}
			pUser->Socket()->Write("\n\r");

		}
	}

	int nStr = (pUser->Player()->Attributes()["strength"]).Cur();
	gString gsAttr = (RacialBonus.find("strength") != RacialBonus.end() ? (*RacialBonus.find("strength")).second : "");

	pUser->Socket()->Write("#601>>#700 Physical\n\r");

	pUser->Socket()->Write(" #601[#6001#601]#701 Strength#600-----#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["dexterity"]).Cur();
		gsAttr = (RacialBonus.find("dexterity") != RacialBonus.end() ? (*RacialBonus.find("dexterity")).second : "");
	pUser->Socket()->Write(" #601[#6002#601]#701 Dexterity#600----#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["constitution"]).Cur();
		gsAttr = (RacialBonus.find("constitution") != RacialBonus.end() ? (*RacialBonus.find("constitution")).second : "");
	pUser->Socket()->Write(" #601[#6003#601]#701 Constitution#600-#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["intelligence"]).Cur();
		gsAttr = (RacialBonus.find("intelligence") != RacialBonus.end() ? (*RacialBonus.find("intelligence")).second : "");

	pUser->Socket()->Write("#601>>#700 Mental\n\r");
	pUser->Socket()->Write(" #601[#6004#601]#701 Intelligence#600-#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["reaction"]).Cur();
		gsAttr = (RacialBonus.find("reaction") != RacialBonus.end() ? (*RacialBonus.find("reaction")).second : "");
	pUser->Socket()->Write(" #601[#6005#601]#701 Reaction#600-----#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["wisdom"]).Cur();
		gsAttr = (RacialBonus.find("wisdom") != RacialBonus.end() ? (*RacialBonus.find("wisdom")).second : "");
	pUser->Socket()->Write(" #601[#6006#601]#701 Wisdom#600-------#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["charisma"]).Cur();
		gsAttr = (RacialBonus.find("charisma") != RacialBonus.end() ? (*RacialBonus.find("charisma")).second : "");

	pUser->Socket()->Write("#601>>#700 Social\n\r");
	pUser->Socket()->Write(" #601[#6007#601]#701 Charisma#600-----#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["status"]).Cur();
		gsAttr = (RacialBonus.find("status") != RacialBonus.end() ? (*RacialBonus.find("status")).second : "");
	pUser->Socket()->Write(" #601[#6008#601]#701 Status#600-------#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));
		nStr = (pUser->Player()->Attributes()["willpower"]).Cur();
		gsAttr = (RacialBonus.find("willpower") != RacialBonus.end() ? (*RacialBonus.find("willpower")).second : "");
	pUser->Socket()->Write(" #601[#6009#601]#701 Willpower#600----#601[%s%d#601]%2s #700Cost to increment#600:#601 %s#700\n\r", nStr == 1 ? "#100" : (nStr == 2 ? "#101" : (nStr == 3 ? "#600" : (nStr == 4 ? "#601" : (nStr == 5 ? "#200" : (nStr == 6 ? "#201" : "#401"))))), nStr, gsAttr, nStr == 0 ? "0" : (nStr == 3 ? "1" : (nStr == 4 ? "2" : nStr == 5 ? "2" : nStr == 6 ? "MAX" : "1")));

	int nRemaining = 15;

	for (AttributeMap::iterator it = pUser->Player()->Attributes().m_Attributes.begin(); it != pUser->Player()->Attributes().m_Attributes.end(); it++)
	{
		switch ((*it).second->Cur())
		{
			case 0: nRemaining += 1;
				break;
			case 2: nRemaining -= 1;
				break;
			case 3: nRemaining -= 2;
				break;
			case 4: nRemaining -= 3;
				break;
			case 5: nRemaining -= 5;
				break;
			case 6: nRemaining -= 7;
				break;
		}
	}

	// We've got to this point so now we need to see if they have used all their points
	if (nRemaining <= 0)
	{
		pUser->Socket()->Write("Attribute selection #601completed.#700\n\r");
		pUser->Socket()->Write("Please review your attributes. Are you happy with these #600[#601Yes#600/#601No#600]#700? ");
	}
	else
	{
		pUser->Socket()->Write("You have #601%d#700 points remaining.\n\r", nRemaining);
		pUser->Socket()->Write("Select #600Raise#601/#600Lower #600[#6011 - 9#600]#700 ");
	}

	return 1;
}

int CChargenMnu::DisplayAbilities(CUser *pUser)
{
	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	// Display intro text
	pUser->Socket()->Write("\n\r\n\r#600**#601*#701 Character Ability and Skill selection #601*#600**#701\n\r");
	pUser->Socket()->Write("#601*#700 There are Nine different core abilities each rated 0 to 4. Each of\n\r  these automatically start at a rating of zero.\n\r");
    pUser->Socket()->Write("#601*#700 Once an ability is chosen, it is further divided into three different skills.\n\r  For every skill a character chooses within an ability-tree, the relevant\n\r  ability rating increases by one. (to a maximum of four)\n\r");
	pUser->Socket()->Write("#601*#700 Abilities, Talents and Skills cost #7011#700 Point, Improved or Legendary Skills cost #7012.#700\n\r");
	pUser->Socket()->Write("#601*#700 Based on your character concept, Make 15 selections:\n\r");

	int nCounter = 1;

	// Starting points
	int nPoints = 15;

	// Display the Abilities and Skills
	// The level of detail depends on the selections the player makes. For example, when the first enter
	// the menu they will only be given a basic view of all the abilities in their different categories.
	// Only putting a point into an ability will they unlock the sub-skills of that ability.
	
	/* <Rating> Ability
        * Skill Selected
        + Talent Selected
        ** Improved Skill Selected
        *** Legendary Skill Selected
        * Skill Selected
        + Talent Selected
        ** Improved Skill Selected
        *** Legendary Skill Selected
        * Skill Selected
        + Talent Selected.1
        + Talent Selected.2
        ** Improved Skill Selected
        *** Legendary Skill Selected 
		
		--- Commands ---
		- Clear  (clears all)
		- Add <Skill/ability/talent>           (marks an ability/skill/talent as selected.
		- <Skill/ability/talent>               (functions like add "skill/ability")
		- Remove <Skill/ability/talent>        (removes already selected ability/skill/talent)
		- Help <Skill/ability/talent>          (displays relevant helpfile)
		*/

	// Work out their points cost so far
	int nCurrPoints = 0;
	for (SkillMap::iterator it = pUser->Player()->Skills().begin(); it != pUser->Player()->Skills().end(); it++)
	{
		if ((*it).second > 0)
			nCurrPoints += pEpacs->GetCost((*it).first);
	}

	int nRemain = nPoints - nCurrPoints;


	for (int i = 0; i < CSkill::_MAXCATEGORIES; i++)
	{
		gString gsCat = CSkill::szCategories[i];
		gsCat.MakeUpper();
		pUser->Socket()->Write("\n\r#301--- %s ---#700\n\r", gsCat);
		// Iterate through each skill, display it and display its current rating from the pUser->Player()->Skills() map
		for (SkillMgrMap::iterator it = pEpacs->Skills().begin(); it != pEpacs->Skills().end(); it++)
		{
			if ((*it).second->Category() != i)
				continue;

			gString gsRating = "";
			int nRating = 0;
			gString gsStar = "-";

			// Don't show hidden skills
			if ((*it).second->Flags()->IsSet(CSkill::_HIDDEN))
				continue;

			// Don't show a skill unless its prereq has been unlocked
			// unless it is an ability
			if ((*it).second->Type() != CSkill::_ABILITY)
			{
				CSkill* pPrereq = pEpacs->GetSkill((*it).second->Prereq());

				if (pPrereq)
				{
					SkillMap::iterator find = pUser->Player()->Skills().find(pPrereq->Id());

					if (find != pUser->Player()->Skills().end())
					{
						// If they 
						if ((*find).second == 0)
							continue;
					}
					else
					{
						// Not found, don't display skill
						continue;
					}
				}


			}

			// Get the player's rating in this skill
			SkillMap::iterator find = pUser->Player()->Skills().find((*it).first);

			if (find != pUser->Player()->Skills().end())
			{
				if ((*find).second != 0)
				{
					nRating = (*find).second;
					gsRating.Format("%d", (*find).second);
				}
			}

			switch ((*it).second->Type())
			{
				case CSkill::_ABILITY:
					pUser->Socket()->Write("%s<%d> %s#700\n\r", nRating > 0 ? "#601" : "#001", nRating, (*it).second->Name());
					break;
	
				case CSkill::_SKILL:
				case CSkill::_SUBSKILL:
					{
						if ((*it).second->Name().HasPrefix("Improved"))
							pUser->Socket()->Write("%s     ** %s\n\r", nRating > 0 ? "#600" : "#001",  (*it).second->Name());
						else if ((*it).second->Name().HasPrefix("Legendary"))
							pUser->Socket()->Write("%s     *** %s\n\r", nRating > 0 ? "#600" : "#001",  (*it).second->Name());
						else
							pUser->Socket()->Write("%s    * %s\n\r", nRating > 0 ? "#600" : "#001",  (*it).second->Name());
					}
					break;

				case CSkill::_TALENT:
					pUser->Socket()->Write("%s     + %s\n\r", nRating > 0 ? "#500" : "#501", (*it).second->Name());
					break;
			} 

		}
	} 

	pUser->Socket()->Write("\n\rYou have #601[#600%d#601]#700 Point%s remaining.\n\r", nRemain, nRemain > 1 ? "s" : "");
	pUser->Socket()->Write("Syntax#60 1:#701 Add #601<#600Skill Name#601>, #701Skillname#601,#701 Remove #601<#600Skill Name#601>,#701 Clear#601:#700\n\r");

	return 1;
}

int CChargenMnu::DisplayBackgrounds(CUser *pUser)
{
	return 1;
}


int CChargenMnu::DisplayResetOptions(CUser* pUser)
{
	return GenericOption(pUser);
}

bool CChargenMnu::VerifyDelete(CUser* pUser, const gString& gsArgument)
{
	if ( gsArgument.CompareNoCase("yes") == 0 )
		return true;

	pUser->Socket()->Write("You must spell out 'yes' to delete this character.\n\r");
	return false;
}

bool CChargenMnu::VerifyName(CUser* pUser, const gString& gsArgument)
{
	UserMap::iterator pos;

	if (gsArgument.IsEmpty() || gsArgument == "abort")
		return 0;

	gString gsName(gsArgument);
	gsName.MakeProper();

	if ( CPlayer::Exists(pUser->Name(), pUser->GUID(), gsName))
	{
		pUser->Socket()->Write("That player exists already.\n\rPlease hit #600[#601Enter#600]#700 to continue.");
		return false;
	}

	pUser->Player()->SetName(gsName);
	pUser->Socket()->Write("Player name set to '%s'.\n\r", (const char*)gsName);
	pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");


	return true;
}

bool CChargenMnu::VerifyGender(CUser* pUser, const gString& gsArgument)
{
	if (gsArgument.IsEmpty() || gsArgument == "abort")
		return false;

	bool bFound = false;
	int nGender = 0;

	for (int i = 0; i < CMobile::_MAXGENDER; i++)
	{
		if (gsArgument == (const char*)CMobile::szMobileGender[i])
		{
			nGender = i;
			bFound = true;
		}
	}

	if (!bFound)
	{
		pUser->Socket()->Write("#601%s#700 is not a valid gender.\n\rChoices are#600:#700 male, female, neutral? ", gsArgument);
		return false;
	}

	// Set the race
	pUser->Player()->SetGender(nGender);

	if (pUser->Player()->Name().IsEmpty())
		pUser->Socket()->Write("Gender set to #601%s#700\n\r", gsArgument);
	else
		pUser->Socket()->Write("#601%s#700 is now #601%s#700\n\r", pUser->Player()->Name(), gsArgument);
	pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");

	return true;
}

bool CChargenMnu::VerifyRace(CUser* pUser, const gString& gsArgument)
{
	CRaceMgr* pManager = CGameObjects::Get().GameWorld()->RaceMgr();

	if (gsArgument.IsEmpty() || gsArgument == "abort" || gsArgument == " ")
	{
		pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
		return true;
	}

	// Is the number entered valid?
	if (atoi(gsArgument) > pManager->Races().size() || atoi(gsArgument) <= 0)
	{
		pUser->Socket()->Write("#601%s#700 is not a valid Race.\n\rValid selections are from #601[#6001 - %d#601]#700? ", gsArgument, pManager->Races().size());
		return false;
	}

	CRace* pRace = pManager->Races().at(atoi(gsArgument)-1);

	if (!pRace)
		return false;

	pUser->Player()->SetRace(pRace->Name());

	if (pUser->Player()->Name().IsEmpty())
		pUser->Socket()->Write("Race set to #601%s#700\n\r", pRace->Name());
	else
		pUser->Socket()->Write("#601%s#700 is now a #601%s#700\n\r", pUser->Player()->Name(), pRace->Name());
	pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");

	return true;
}

bool CChargenMnu::VerifyAttributes(CUser* pUser, const gString& gsArgument)
{
	int nRemaining = 15;
	bool bRaise = false;

	// No input
	if (gsArgument.IsEmpty() || gsArgument == "abort" || gsArgument == " " || gsArgument == "done")
	{
		pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
		return true;
	}

	// Reset all stats to 1
	if (gsArgument == "reset")
	{
		pUser->Socket()->Write("Player attributes #601reset#600 to default.\n\r");
		pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
		for (AttributeMap::iterator it = pUser->Player()->Attributes().m_Attributes.begin(); it != pUser->Player()->Attributes().m_Attributes.end(); it++)
				(*it).second->SetCur(1);
		return true;
	}

	// Determine amount of points remaining
	for (AttributeMap::iterator it = pUser->Player()->Attributes().m_Attributes.begin(); it != pUser->Player()->Attributes().m_Attributes.end(); it++)
	{
		switch ((*it).second->Cur())
		{
			case 0: nRemaining += 1;
				break;
			case 2: nRemaining -= 1;
				break;
			case 3: nRemaining -= 2;
				break;
			case 4: nRemaining -= 3;
				break;
			case 5: nRemaining -= 5;
				break;
			case 6: nRemaining -= 7;
				break;
		}
	}


	// Validation check on point completion
	if (nRemaining == 0)
	{
		// Anything starting with yes will do
		if (gsArgument.HasPrefix("y"))
		{
			pUser->Socket()->Write("Attribute selection #601complete#700.\n\r");
			pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
			return true;
		}
		else if (gsArgument.HasPrefix("n"))
		{
			pUser->Socket()->Write("Attributes #601reset#700, have another go!\n\r");
			pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
			for (AttributeMap::iterator it = pUser->Player()->Attributes().m_Attributes.begin(); it != pUser->Player()->Attributes().m_Attributes.end(); it++)
				(*it).second->SetCur(1);

			return false;
		}
		else
		{
			pUser->Socket()->Write("That is not a valid response. Are you happy with these stats (Yes/No)? ");
			return false;
		}
	}

	// We assume, unless they prefix with lower, they are raising
	if (!gsArgument.HasPrefix("lower"))
		bRaise = true;

	// Figure out whether they are entering the name, or the index
	gString gsTemp = gsArgument;	
	gsTemp.TrimLeft(1);

	// If its equal to zero they have entered a string
	if (atoi(gsTemp) == 0)
	{
		// Figure out the stat the entered
		gString gsTemp2 = gsArgument;
		gsTemp2.TrimLeft(gsTemp2.Length() - 6);

		if (gsTemp2 == "strength")
			gsTemp = "1";
		else if (gsTemp2 == "dexterity")
			gsTemp = "2";
		else if (gsTemp2 == "constitution")
			gsTemp = "3";
		else if (gsTemp2 == "intelligence")
			gsTemp = "4";
		else if (gsTemp2 == "reaction")
			gsTemp = "5";
		else if (gsTemp2 == "wisdom")
			gsTemp = "6"; 
		else if (gsTemp2 == "charisma")
			gsTemp = "7";
		else if (gsTemp2 == "status")
			gsTemp = "8";
		else if (gsTemp2 == "willpower")
			gsTemp = "9";
	}


	CAttribute* pAttribute;

	// Get the attribute
	switch (atoi(gsTemp))
	{
		case 1:	pAttribute = &pUser->Player()->Attributes()["strength"];
				break;
		case 2:	pAttribute = &pUser->Player()->Attributes()["dexterity"];
				break;
		case 3:	pAttribute = &pUser->Player()->Attributes()["constitution"];
				break;
		case 4:	pAttribute = &pUser->Player()->Attributes()["intelligence"];
				break;
		case 5:	pAttribute = &pUser->Player()->Attributes()["reaction"];
				break;
		case 6:	pAttribute = &pUser->Player()->Attributes()["wisdom"];
				break;
		case 7:	pAttribute = &pUser->Player()->Attributes()["charisma"];
				break;
		case 8:	pAttribute = &pUser->Player()->Attributes()["status"];
				break;
		case 9:	pAttribute = &pUser->Player()->Attributes()["willpower"];
				break;
		default:
			{
				pUser->Socket()->Write("#601%s#700 is not a valid stat.\n\rValid choices are #601[#6001-9#601]#700? ", gsTemp);
				return false;
			}
	}

	int nStat = pAttribute->Cur();

	if (bRaise)
	{
		// Check stat limits
		if (nStat == 6)
		{
			pUser->Socket()->Write("The maximum in a stat is #6016#700, please select another? ");
			return false;
		}
		// Check if they have the points enough to raise
		// Raising from 1 to 2, or 2 to 3 costs one point
		if (nStat >= 1 && nStat <= 3)
		{
			if (nRemaining < 1)
			{
				pUser->Socket()->Write("Raising #601%s#700 stat costs 1 point, you do not have any. Choice another stat? ", pAttribute->Name());
				return false;
			}
			else
			{
				pUser->Socket()->Write("#601%s#700 rating raised to #601%d#700\n\r", pAttribute->Name(), nStat+1);
				pUser->Socket()->Write("Press#600[#601Enter#600]#700 to continue.\n\r");
				pAttribute->SetCur(nStat+1);
				nRemaining -= 1;
			}
		}
		else if (nStat >= 4 && nStat <= 5)
		{
			if (nRemaining < 2)
			{
				pUser->Socket()->Write("Raising #601%s#700 stat costs 2 points, you do not have enough. Choose another stat? ", pAttribute->Name());
				return false;
			}
			else
			{
				pUser->Socket()->Write("#601%s#700 rating raised to #601%d#700\n\r", pAttribute->Name(), nStat+1);
				pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
				pAttribute->SetCur(nStat+1);
				nRemaining -= 2;
			}
		}
		else if (nStat == 0)
		{
			pUser->Socket()->Write("#601%s#700 rating raised to #6011#700\n\r", pAttribute->Name());
			pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
			pAttribute->SetCur(1);
		}
	}
	else
	{
		// Lower stat
		if (nStat <= 0)
		{
			pUser->Socket()->Write("#601%s#700 is already at zero. Choose another stat? ", pAttribute->Name());
			return false;
		}
		else
		{
			pAttribute->SetCur(nStat-1);
			nRemaining++;
			pUser->Socket()->Write("#601%s#700 lowered to %d.\n\r", pAttribute->Name(), nStat-1);
			pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
		}
	}

	return false;
}

bool CChargenMnu::VerifyAbilities(CUser* pUser, const gString& gsArgument)
{
	int nRemaining = 15;
	bool bRaise = false;

	CEpacs* pEpacs = CGameObjects::Get().GameWorld()->Epacs();

	// No input
	if (gsArgument.IsEmpty() || gsArgument == "abort" || gsArgument == "done" || gsArgument == " ")
	{
		pUser->Socket()->Write("Press #600[#601Enter#600]#700 to continue.\n\r");
		return true;
	}

	// Work out their points cost so far
	int nPoints = 15;

	int nCurrPoints = 0;
	for (SkillMap::iterator it = pUser->Player()->Skills().begin(); it != pUser->Player()->Skills().end(); it++)
	{
		if ((*it).second > 0)
			nCurrPoints += pEpacs->GetCost((*it).first);
	}

	int nRemain = nPoints - nCurrPoints;

	// Validate input
	// They can enter:
	// [1] 'Remove' followed by UID or Name
	// [2] 'Clear' to remove all selections
	// [3] The name of a skill
	// [4] 'Add' plus the name of a skill

	// [1] Removing a skill/talent/ability
	if (gsArgument.HasPrefix("remove"))
	{
		gString gsArg = gsArgument;
		// Cut out 'remove' prefix
		gsArg.TrimLeft(gsArgument.Length() - 7);

		// Check if they selected a valid skill
		CSkill* pSkill;
		
		if ((pSkill = pEpacs->GetSkill(gsArg)) != NULL)
		{
			// Check if the skill is hidden
			if (pSkill->Flags()->IsSet(CSkill::_HIDDEN))
			{
				pUser->Socket()->Write("#100[#101Invalid Entry#100]#701 %s#700 is not a valid skill!\n\r", gsArg);
				pUser->Socket()->Write("Syntax#701:#700 #601<#600Skill name#601> OR <#600Remove Name#601> OR <#600Clear#601>#700\n\r");
				return false;
			}
			
			// Check if they have this skill
			if (pUser->Player()->GetSkill(gsArgument) < 0)
			{
				pUser->Player()->Write("You have not added the %s %s.\n\r", pSkill->Name(), CSkill::szTypes[pSkill->Type()]);
				return false;
			}

			// Remove the skill
			pUser->Player()->RemoveSkill(pSkill->Id());
			pUser->Socket()->Write("You have removed the %s %s.\n\r", pSkill->Name(), CSkill::szTypes[pSkill->Type()]);
			return false;
		}

		pUser->Socket()->Write("#100[#101Invalid Entry#100]#701 %s#700 is not a valid skill!\n\r",gsArg);
		pUser->Socket()->Write("Syntax#701:#700 #601<#600Skill name#601> OR <#600Remove Name#601> OR <#600Clear#601>#700\n\r");
		return false;

	}

	// [2] Clearing a selection
	if (gsArgument.HasPrefix("clear"))
	{
		pUser->Player()->Skills().clear();
		pUser->Socket()->Write("Skill selections cleared.\n\r");
		return false;

	}

	// [3] Check if they entered a Skill name
	if (!gsArgument.IsEmpty())
	{
		gString gsArg = gsArgument;
		
		// [4] Check for the Add prefix
		if (gsArgument.HasPrefix("Add"))
			gsArg.TrimLeft(gsArgument.Length() - 4);

		CSkill* pSkill;
		
		// Check if this is a valid entry
		if ((pSkill = pEpacs->GetSkill(gsArg)) != NULL)
		{
			// Check if the skill is hidden
			if (pSkill->Flags()->IsSet(CSkill::_HIDDEN))
			{
				pUser->Socket()->Write("#100[#101Invalid Entry#100]#701 %s#700 is not a valid skill!\n\r", gsArg);
				pUser->Socket()->Write("Syntax#701:#700 #601<#600Skill name#601> OR <#600Remove Name#601> OR <#600Clear#601>#700\n\r");
				return false;
			}
			
			// Check if they have the required prerequisite
			if (!pSkill->Prereq().IsEmpty() && pUser->Player()->GetSkill(pSkill->Prereq()) <= 0)
			{
				CSkill* pPre = pEpacs->GetSkill(pSkill->Prereq());

				pUser->Player()->Write("You must first select the '%s' %s to unlock this %s.\n\r", pSkill->Prereq(), CSkill::szTypes[pPre->Type()], CSkill::szTypes[pSkill->Type()]);
				return false;
			}

			// Check the points cost
			if (nRemain < pEpacs->GetCost(pSkill->Id()))
			{
				pUser->Player()->Write("The %s %s will cost %d, you only %d point%s remaining.\n\r", pSkill->Name(), CSkill::szTypes[pSkill->Type()], pEpacs->GetCost(pSkill->Id()), nRemain, nRemain > 1 ? "s" : "");
				return false;
			}

			// Add the new skill
			pUser->Player()->AddSkill(pSkill->Id());
			if (pSkill->Children().size() > 0)
				pUser->Socket()->Write("You have added the %s %s, Unlocked %d additional selections.\n\r", pSkill->Name(), CSkill::szTypes[pSkill->Type()], pSkill->Children().size());
			else
				pUser->Socket()->Write("You have added the %s %s.\n\r", pSkill->Name(), CSkill::szTypes[pSkill->Type()]);
			return false;

		}
		else
		{
			pUser->Socket()->Write("#100[#101Invalid Entry#100]#701 %s#700 is not a valid skill!\n\r",gsArg);
			pUser->Socket()->Write("Syntax#701:#700 #601<#600Skill name#601> OR <#600Remove Name#601> OR <#600Clear#601>#700\n\r");
			return false;
		}


	}


	return false;
}


int CChargenMnu::Process(CUser* pUser)
{
	gString gsArgument = pUser->Socket()->GetInput();

	if ( m_pTextMgr == NULL )
		m_pTextMgr = const_cast<CTextMgr*>(CGameObjects::Get().GameWorld()->TextMgr());

	if ( !pUser->Player() )
	{
		// setup player object
		CPlayer* pPlayer = new CPlayer();
		pUser->SetPlayer(pPlayer);
		pPlayer->SetUser(pUser);
	}

	if ( gsArgument.HasPrefix("help") )
	{
		HandleHelpRequest(pUser, gsArgument);
		pUser->SetSubMenu(pUser->LastSubMenu());
	}
	else

	switch (pUser->CurrentSubMenu())
	{
		case _CHARGEN_MNU_VERIFY_NAME:
			{
				VerifyName(pUser, gsArgument);
				// regardless of if the name is set or not, always set the submenu
				// back to 0 so that it will display the menu options again.
				// 'verifyname' informs the player of it's success or failure.
				pUser->SetSubMenu(_CHARGEN_MNU_NONE);
			}
			break;

		case _CHARGEN_MNU_VERIFY_GENDER:
			{
				if (!VerifyGender(pUser, gsArgument))
					pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_GENDER);
				else
					pUser->SetSubMenu(_CHARGEN_MNU_NONE);
			}
			break;

		case _CHARGEN_MNU_VERIFY_RACE:
			{
				if (!VerifyRace(pUser, gsArgument))
					pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_RACE);
				else
					pUser->SetSubMenu(_CHARGEN_MNU_NONE);
			}
			break;

		case _CHARGEN_MNU_VERIFY_ATTRIBUTES:
			{
				if (!VerifyAttributes(pUser, gsArgument))
				{
					DisplayAttributes(pUser);
					pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_ATTRIBUTES);
				}
				else
					pUser->SetSubMenu(_CHARGEN_MNU_NONE);
			}
			break;

		case _CHARGEN_MNU_VERIFY_ABILITIES:
			{
				if (!VerifyAbilities(pUser, gsArgument))
				{
					DisplayAbilities(pUser);
					pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_ABILITIES);
				}
				else
					pUser->SetSubMenu(_CHARGEN_MNU_NONE);
			}
			break;

		case _CHARGEN_MNU_VERIFY_DELETE:
			{
				if ( VerifyDelete(pUser, gsArgument) )
				{
					pUser->SetPlayer(NULL, false);
					pUser->Socket()->Write("Character deleted.\n\r");
					pUser->SetMenu(CUser::_MNU_MAIN_MENU);
				}
				else
					pUser->SetSubMenu(_CHARGEN_MNU_NONE);
			}
			break;

		default: // Show Main Menu
			{
				switch ( atoi(gsArgument) )
				{
  					case _CHARGEN_MNU_SET_NAME  :
						{
							pUser->Socket()->Write("By which name shall we know you? ");
							pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_NAME);
						}
						break;
					case _CHARGEN_MNU_SHOW_RACE :
						DisplayRaceOptions(pUser);
						pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_RACE);
						break;
					case _CHARGEN_MNU_SHOW_GENDER :
						{
							DisplayGenderOptions(pUser);
							pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_GENDER);
						}
						break;
					case _CHARGEN_MNU_SHOW_ATTRIBUTES :
						{
							DisplayAttributes(pUser);
							pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_ATTRIBUTES);
						}
						break;
					case _CHARGEN_MNU_SHOW_ABILITIES :
						{
							DisplayAbilities(pUser);
							pUser->SetSubMenu(_CHARGEN_MNU_VERIFY_ABILITIES);
						}
						break;
					case _CHARGEN_MNU_SHOW_BACKGROUND :
						DisplayBackgrounds(pUser);
						break;
					case _CHARGEN_MNU_DELETE_CHARACTER :
						DisplayDeleteCharacterOptions(pUser);
						break;
					case _CHARGEN_MNU_RESET_DEFAULTS :
						DisplayResetOptions(pUser);
						break;
					case _CHARGEN_MNU_RETURN_TO_MAIN :
						{
							// be sure to do any necessary cleanup here.
							// if the player character was not completely setup,
							// it should be deleted...
							if ( CPlayer::IsValidPlayer(pUser->Player()) )
							{
								pUser->Account()->PlayerListAdd(pUser->Player());

								pUser->Save();
								pUser->Player()->ActorFlags()->SetBit(CPlayer::_STAFF);
								pUser->Player()->Save();

								pUser->Socket()->Write("\"%s\" created.\n\r", (const char*)pUser->Player()->Name());

								pUser->SetPlayer(NULL);
							}
							else
								pUser->Socket()->Write("Incomplete character -- not saved.\n\r");

							pUser->SetMenu(CUser::_MNU_MAIN_MENU);
						}
						break;
					default:
						DisplayMenu(pUser);
						break;
				}
			}

			break; 
	}

	return 1;
}
