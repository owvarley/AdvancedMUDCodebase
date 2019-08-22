//                                __.             
//                               (__.    ,_ ._.._ 
//                               .__)\/\/(/,[  [_)
//                                             |  
//
// Interpreted by Owen Varley [Nekekami] :: <o.w.varley#dur.ac.uk>
//
// Durham   :: CS Project 2005/2006
// Started  :: 29 May 2005
// Based on :: Mudcore, copyright (c) 2000, 2001 by Gary McNickle <gary#mcnickle.org>

// Header   :: Emotions.h
// Function :: Handles the saving and loading of Emotions, i.e. pre-written emotes

#ifndef __EMOTIONS_H__
#define __EMOTIONS_H__

#include <vector>
#include "gString.h"
#include "MudCore.h"
#include "Set.h"


#pragma warning(disable: 4251)

class _export CEmotions
{

// Methods
public:
	CEmotions();
	CEmotions(gString gsName);
	~CEmotions();
	
	///////////////////////////////////////////////////////////////////////////////////////
	// Friend classes
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CEmoteParser;

	///////////////////////////////////////////////////////////////////////////////////////
	// Loading/Saving functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual bool	Load(gFileName gsEmotion);
	virtual bool	Save(gFileName gsEmotion);

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline Data Access Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline gString		Name()		{ return m_gsName; }

	const gString&	Name()   const { return m_gsName; }
	const gString&	Author() const { return m_gsAuthor; }
	const gString&	eView()  const { return m_gsSelf; }
	const gString&	tView()  const { return m_gsTarget; }
	const gString&	rView()  const { return m_gsRoom; }
	const gString&	nView()  const { return m_gsNone; }
	const gString&	sView()  const { return m_gsSolo; }
	
	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CEmotions& emotions );
	friend std::istream& operator >> ( std::istream& stream, CEmotions& emotions );
	

// Date
protected:
	gString		m_gsName;	// Name of emotion
	gString		m_gsAuthor; // Author of Emotion
	gString		m_gsSelf;	// Player's message
	gString		m_gsTarget; // Target's message
	gString		m_gsRoom;	// Message to Room
	gString		m_gsNone;	// No target message
	gString		m_gsSolo;	// Self message

};

typedef std::vector<CEmotions*> EmoteList;

class CEmoteParser
{
public:
	CEmoteParser();
	virtual ~CEmoteParser();

	// Methods
	bool Interpret(CActor* Ch, gString CommandLine); // Handles the emotion
	CEmotions* Exists(gString gsEmote);				 // Returns a pointer to the emote if it exists
	bool Save(gFileName gfnEmoteFile);				 // Saves the Parser and all its emotes
	bool Load(gFileName gfnEmoteFile);				 // Loads the Parser and its emotes
	void AddEmote(CEmotions* pEmote);				 // Adds an Emote to the EmotionsList
	void DescribeTo(CActor* Ch);					 // Outputs all Emotes to the player

	// Serialization
	friend std::ostream& operator << ( std::ostream& stream, const CEmoteParser& parser );
	friend std::istream& operator >> ( std::istream& stream, CEmoteParser& parser );

public:
	// Data
	EmoteList		m_Emotions;

};

typedef std::vector<CEmoteParser*> EmoParsers;

#endif