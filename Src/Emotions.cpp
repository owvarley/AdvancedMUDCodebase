//						.__..  . __ 
//						[__]|\/|/  `
//						|  ||  |\__.
//
// The Advanced MUD Codebase Project
// AMC, copyright (c) 2005, 2006 by Owen Varley <owen@sw-erp.org>

//
// Interpreted by Owen Varley [Nekekami] :: <owen#sw-erp.org>
// Lead Design :: Ken Rune Mikkelson [N'kEnNy], Charlie Van Der Born [Chaz]
// Designed by the SW-ERP Development Team [www.sw-erp.org]
//
// Durham   :: SE Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Class    :: CEmotions, CEmoteParser
// Header   :: Emotions.h 
// Function :: Handles the saving and loading of Emotions, i.e. pre-written emotes


#pragma warning(disable:4786)


#include "GameObjects.h"
#include "Tools.h"
#include "GameServer.h"
#include "GameWorld.h"
#include "Socket.h"
#include "Set.h"
#include "Player.h"
#include "Emotions.h"
#include "../gTools/Log.h"


CEmoteParser* _pgParser = NULL;

// Empty constructor
CEmotions::CEmotions()
{ 
}

CEmotions::CEmotions(gString gsName)
{
	m_gsName = gsName;
	m_gsAuthor = "";
	m_gsSelf = "";
	m_gsTarget = "";
	m_gsRoom = "";
	m_gsNone = "";
}


// Deconstructor to free strings
CEmotions::~CEmotions()
{ 
	m_gsName = "";
	m_gsAuthor = "";
	m_gsSelf = "";
	m_gsTarget = "";
	m_gsRoom = "";
	m_gsNone = "";
}


// Method     :: <<
// Class	  :: CEmotions
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the << write
//				 operator is used. In this case we simply write all the object's
//				 fields to the Emotions file.

std::ostream& operator << ( std::ostream& stream, const CEmotions& emote )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	Tools.WriteLn(stream, "[Emote]");
	Tools.WriteLn(stream, " Name          : \"%s\"", emote.m_gsName);
	Tools.WriteLn(stream, " Author        : \"%s\"", emote.m_gsAuthor);
	Tools.WriteLn(stream, " EView         : \"%s\"", emote.m_gsSelf);
	Tools.WriteLn(stream, " TView         : \"%s\"", emote.m_gsTarget);
	Tools.WriteLn(stream, " RView         : \"%s\"", emote.m_gsRoom);
	Tools.WriteLn(stream, " NView         : \"%s\"", emote.m_gsNone);
	Tools.WriteLn(stream, " SView         : \"%s\"", emote.m_gsSolo);
	Tools.WriteLn(stream, "[/Emote]");

	return stream;
}

// Method     :: >>
// Class	  :: CEmotions
// Parameters :: <none>
// Return     :: <none>
// Function   :: Provides class specific actions to carry out when the >> read
//				 operator is used for this class. In this case we poll through the
//				 input stream and read each piece of data into the object.

std::istream& operator >> ( std::istream& stream, CEmotions& emote )
{
	CTools& Tools = *CGameObjects::Get().Tools();

	gString gsKey, gsString;
	bool bDone = false;
	streampos marker;

	try
	{
		if ( Tools.ReadKey(stream) == "[Emote]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();
				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/Emote]" && gsKey[0] != EOF )
				switch ( gsKey[0] )
				{

					case 'A':
						if ( gsKey == "Author" )
							Tools.ReadData(stream, emote.m_gsAuthor);
						break;
					case 'E':
						if ( gsKey == "EView" )
							Tools.ReadData(stream, emote.m_gsSelf);
						break;
					case 'N':
						if ( gsKey == "Name" )
							Tools.ReadData(stream, emote.m_gsName);
						else if ( gsKey == "NView" )
							Tools.ReadData(stream, emote.m_gsNone);
						break;
					case 'T':
						if ( gsKey == "TView" )
							Tools.ReadData(stream, emote.m_gsTarget);
						break;
					case 'R':
						if ( gsKey == "RView" )
							Tools.ReadData(stream, emote.m_gsRoom);
						break;
					case 'S':
						if ( gsKey == "SView" )
							Tools.ReadData(stream, emote.m_gsSolo);
						break;

					default:
						Tools.Report(E_ERROR, "[CEmotions::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;
				}
				bDone = (stream.eof() || gsKey == "[/Emote]");
			}
		}
		else
			Tools.Report(E_ERROR, "[CEmotions::>>] Invalid stream!");

	}
	catch (...)
	{
		Tools.Report(E_ERROR, "[CEmotions::>>] Error encountered while reading emotes file..");
	}

	return stream;
}

// Method     :: Load
// Class	  :: CEmotions
// Parameters :: Filename to load
// Return     :: True if Successful, False if fails to read
// Function   :: Simply file operations. Locates the file to load then 
//				 attempts to load them in using the class specific >> operator

bool CEmotions::Load(gFileName gEmote)
{
	CGameObjects& globals = CGameObjects::Get();
	std::fstream fp;

	fp.open(gEmote, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	fp >> *this;
	fp.close();

	return true;
}

// Method     :: Save
// Class	  :: CEmotions
// Parameters :: Filename to save to
// Return     :: True if Successful, False if not
// Function   :: Saves the CEmotions object to file using the predefined
//				 << operator.

bool CEmotions::Save(gFileName gEmote)
{
	CGameObjects& globals = CGameObjects::Get();
	std::fstream fp;

	fp.open(gEmote, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	fp << *this;

	fp.flush();
	fp.close();

	return true;
}

CEmoteParser::CEmoteParser()
{

}

CEmoteParser ::~CEmoteParser()
{

}

// Method     :: DescribeTo
// Class	  :: CEmoteParser
// Parameters :: Player
// Return     :: Void
// Function   :: Outputs all Emotions held by this instance of the parser

void CEmoteParser::DescribeTo(CActor* Ch)
{
	Ch->Write( "\n\r#500:#501:#701 Emotions #501:#500:\n\r");

	int nCount = 0;
	bool bColour = false;
	
	for (EmoteList::iterator em = this->m_Emotions.begin(); em != this->m_Emotions.end(); em++)
	{
		nCount++;
		if (bColour)
			Ch->Write("#500%-15s ", (*em)->Name());
		else
			Ch->Write("#701%-15s ", (*em)->Name());

		if (nCount % 5 == 0)
			Ch->Write("\n\r");

		if (nCount % 20 == 0)
			bColour = !bColour;		
	}

	Ch->Write("\n\r#500[#501 %d#500] Emotions#700\n\r", nCount);

}

// Method     :: Interpret
// Class	  :: CEmoteParser
// Parameters :: Player and the CommandLine
// Return     :: True if its a valid Emotion
// Function   :: Handles the displaying of the different Emotion messages to the players in a room

bool CEmoteParser::Interpret (CActor* Ch, gString CommandLine)
{
	ActorMap::iterator pos;
	CActor *pA;
	CActor *pTarget = NULL;
	bool bDone = false;

	// Get the player's gender
	int nCSex = ((CPlayer*)Ch)->Gender();

	// Ensure they actually passed something
	CommandLine.TrimSpacesLeft();
	CommandLine.TrimSpacesRight();

	// Nothing there
	if (CommandLine.IsEmpty())
		return false;

	// Now we check for a target
	gString emotion = CGameObjects::Get().Tools()->GetNextWord(CommandLine);
	gString target = CGameObjects::Get().Tools()->GetNextWord(CommandLine);

	// Get a pointer to the Emote object
	CEmotions *pEmote = Exists(emotion);

	// Determine if player has entered a valid Emotion
	if (pEmote == NULL)
		return false;

	// Now we need to output messages. There are two different cases
	// 1) No target
	if (target.IsEmpty())
	{
		gString gsSolo = pEmote->m_gsSolo;
		gString gsNone = pEmote->m_gsNone;

		gsSolo.ReplaceSymbols(Ch->Name(), nCSex, "", 0);
		gsNone.ReplaceSymbols(Ch->Name(), nCSex, "", 0);


		// Display message to player
		Ch->Write("%s\n\r", gsSolo);

		// Display message to rest of room

		Ch->CurrentRoom()->Write(Ch, "%s\n\r", gsNone);

		/*
  	    for ( pos = Ch->CurrentRoom()->Actors().begin(); pos != Ch->CurrentRoom()->Actors().end(); pos++ )
		{
		  try
		  {
			  if ( (pA = (CActor*)((*pos).second)) != NULL )
			  {
				  // We need to format the emotion
				  if (pA != Ch)
					pA->Write("%s\n\r", gsNone);			  
			  }
		  }
		  catch(...) { break; }
		}*/

		bDone = true;
	}
	else
	{	// 2) Target
		// Valid target?
		pTarget = Ch->CurrentRoom()->FindFirstActor(target);
		if (pTarget == NULL)
		{
			Ch->Write("%s is not here!\n\r", target);
			return true;
		}
		
		// Target == Self?
		if (pTarget == Ch)
		{
			this->Interpret(Ch, emotion);
			return bDone;
		}

		int nTSex = ((CPlayer*)pTarget)->Gender();

		gString gsSelf = pEmote->m_gsSelf;
		gString gsRoom = pEmote->m_gsRoom;
		gString gsTarg = pEmote->m_gsTarget;

		gsSelf.ReplaceSymbols(Ch->Name(), nCSex, pTarget->Name(), nTSex);
		gsRoom.ReplaceSymbols(Ch->Name(), nCSex, pTarget->Name(), nTSex);
		gsTarg.ReplaceSymbols(Ch->Name(), nCSex, pTarget->Name(), nTSex);

		/*
			$n - Name of the user of the social.
			$N - Name of the target of the social.
			$m - him/her/it for the user the social.
			$M - him/her/it for the target of the social.
			$s - his/her/its for the user of the social.
			$S - his/her/its for the targt of the social.
			$e - he/she/it for the user of the social.
			$E - he/she/it for the target of the social.

		*/

		
		// Target is valid, sort out messages
		// First tell Player
		Ch->Write("%s\n\r", gsSelf);
		// Now tell target
		pTarget->Write("%s\n\r", gsTarg);

		// Now the rest of the room!
		for ( pos = Ch->CurrentRoom()->Actors().begin(); pos != Ch->CurrentRoom()->Actors().end(); pos++ )
		{
		  try
		  {
			  if ( (pA = (CActor*)((*pos).second)) != NULL )
			  {
				  // We need to format the emotion
				  if (pA != Ch && pA != pTarget)
					pA->Write("%s\n\r", gsRoom);			  
			  }
		  }
		  catch(...) { break; }
		}

		bDone = true;
	}


	return bDone;
}

CEmotions* CEmoteParser::Exists(gString gsEmote)
{
	EmoteList::iterator emo;

	for (emo=m_Emotions.begin(); emo != m_Emotions.end(); emo++)

	if ( (*emo)->m_gsName == gsEmote )
	{
		return (*emo);
	}


	return NULL;
}

void CEmoteParser::AddEmote(CEmotions* pEmote)
{
	m_Emotions.push_back(pEmote);
}

// Write this parser to an output stream
std::ostream& operator << ( std::ostream& stream, CEmoteParser& parser )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	time_t tm = time(0);
	gString gsTime = ctime(&tm);
	gsTime.DeleteChar( gsTime.Length()-1 ); // Remove the \n from ctime

	Tools.WriteLn(stream, "[CEmoteParser]");
	Tools.WriteLn(stream, " Version           : %d",		VERSION);
	Tools.WriteLn(stream, " Last Saved On     : %s",        gsTime);
											 

	for (int n = 0; n < parser.m_Emotions.size(); n++)
		stream << *parser.m_Emotions[n];

	Tools.WriteLn(stream, "[/CEmoteParser]");

	return stream;
}

// Read this actor from an input stream
std::istream& operator >> ( std::istream& stream, CEmoteParser& parser )
{
	CTools& Tools = *CGameObjects::Get().Tools();
	gString gsKey, gsString;
	streampos marker;
	bool bDone = false;
	int nVersion;

	try
	{
		if ( Tools.ReadKey(stream) == "[CEmoteParser]" )
		{
			while ( !bDone )
			{
				marker = stream.tellg();

				gsKey = Tools.ReadKey(stream);

				if ( gsKey.Length() > 0 && gsKey != "[/CEmoteParser]"  )
				switch ( gsKey[0] )
				{
					case 'V':
						if ( gsKey == "Version" )
							Tools.ReadData(stream, nVersion);
						break;
					case 'L':
						if ( gsKey == "Last Saved On" )
							Tools.ReadLn(stream); // throw away the data, we dont really care.
						break;
					case '[':
						if ( gsKey == "[Emote]" )
						{
							gString gsString;

							if ( (gsString = Tools.ReadKey(stream)) == "Name" )
							{
								CEmotions Emote;

								Tools.ReadData(stream, gsString);
								stream.seekg(marker);

								if ( _pgParser )
								{
									CEmotions* pEmo = new CEmotions;

									stream >> *pEmo;

									_pgParser->AddEmote(pEmo);
								}

							}

						}
						break;

					default:
						g_Log.Log(LOG_ERROR, "[CEmoteParser::>>] Invalid Key <%s>", gsKey);
						Tools.ReadLn(stream);
						break;

				}
				bDone = (stream.eof() || gsKey == "[/CEmoteParser]");
			}

		}
		else
			g_Log.Log(LOG_ERROR, "[CEmoteParser::>>] Invalid stream!");

	}
	catch (...)
	{
		g_Log.Log(LOG_ERROR, "[CEmoteParser::>>] Error encountered while reading Emotions file..");
	}

	return stream;

}




bool CEmoteParser::Load(gFileName gfnEmote)
{
	std::fstream fp;
	CGameObjects& globals = CGameObjects::Get();
	
	fp.open(gfnEmote, ios::in|ios::nocreate);

	if ( !fp.is_open() )
		return false;

	_pgParser = this;

	//fp.lock();
	fp >> *this;
	//fp.unlock();
	fp.close();

	_pgParser = NULL;

	return true;
}

bool CEmoteParser::Save(gFileName gfnEmote)
{
	std::fstream fp;

	CGameObjects& globals = CGameObjects::Get();

	fp.open(gfnEmote, ios::in|ios::out);

	if ( !fp.is_open() )
		return false;

	//fp.lock();
	fp << *this;
	//fp.unlock();
	fp.flush();
	fp.close();

	return true;
}

