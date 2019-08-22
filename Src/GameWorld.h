//
// MudCore
//
// MudCore is copyright (c) 2000, 2001 by Gary McNickle
// <gary#mcnickle.org>
//
// MudCore is free software; you can redistribute it and/or modify
// it under the terms of the MudCore license contained in the
// included file "license.txt".
//
// You should have received a copy of the MudCore license along with
// MudCore, if not, write to the author and request a copy.
//
// Gary McNickle
// <gary#mcnickle.org>
// 5408 E. 10th St
// Indianapolis, IN 46219 USA
//


#ifndef __GAMEWORLD_H__
#define __GAMEWORLD_H__

#include <vector>

#include "Actor.h"
#include "Npc.h"
#include "Area.h"
#include "Room.h"
#include "Placement.h"
#include "Command.h"
#include "Emotions.h"
#include "Item.h"
#include "Space.h"
#include "Spatial.h"
#include "Crew.h"
#include "HelpSystem.h"
#include "TextMgr.h"
#include "Epacs.h"
#include "Race.h"

extern UINT uiUniqueWorldID;

class CNavigator;

class CGameWorld: public CAtomic
{
	///////////////////////////////////////////////////////////////////////////////////////
	// Friends of this class (Other classes that have access to this classes private and
	// protected members).
	///////////////////////////////////////////////////////////////////////////////////////
	friend class CGameServer;

public:


	///////////////////////////////////////////////////////////////////////////////////////
	//	Enumerated types specific to this class
	///////////////////////////////////////////////////////////////////////////////////////

	// Profiles are kept for the various updaters to track how long each update cycle
	// is taking. By keeping profile data, we can better locate problem performance areas.
	///////////////////////////////////////////////////////////////////////////////////////
	enum e_Profiles
	{
		_WORLD_UPDATES=0,
		_AREA_UPDATES,
		_PLAYER_UPDATES,
		_MOBILE_UPDATES,
		_ITEM_UPDATES,
		_SPACE_UPDATES,
		_CREW_UPDATES,
		_NUM_PROFILES
	};

	// These colour codes define the progress bar colours, the bar changes colour 
	// progressively in order to make it easy to determine the module's value
	///////////////////////////////////////////////////////////////////////////////////////
	static char* nProgress[];


	CGameWorld();
	~CGameWorld();

	///////////////////////////////////////////////////////////////////////////////////////
	// Inline Data Access Methods
	///////////////////////////////////////////////////////////////////////////////////////
	inline ActorMap&	Players()		{ return m_Players;			}
	inline ActorMap&	Mobiles()		{ return m_Mobiles;			}
	inline ActorMap&	Items()			{ return m_Items;			}
	inline AreaList&	Areas()			{ return m_Areas;			}
	inline EmoParsers&	Emotions()		{ return m_EmoParsers;		}
	inline CmdParsers&	Commands()		{ return m_CmdParsers;		}
	inline CGalaxy*		Galaxy()		{ return &m_Galaxy;			}
	inline CMLoader*	MLoader()		{ return &m_MLoader;		}
	inline CTLoader*	TLoader()		{ return &m_TLoader;		}
	inline CCLoader*	CLoader()		{ return &m_CLoader;		}
	inline CHLoader*	HLoader()		{ return &m_HLoader;		}
	inline CEpacs*		Epacs()			{ return &m_Epacs;			}
	inline CRaceMgr*    RaceMgr()		{ return &m_Races;			}
	inline int&			MaxPlayers()	{ return m_iRunningTotal;	}
	inline UINT			GUID()	  const { return m_ID;				}
	inline gString		Name()	  const { return m_gsName;			}
	inline gString		Key()	  const { return m_gsKey;			}
	inline void			SetName(const gString& gsName) { m_gsName = gsName; }
	inline void			SetKey(const gString& gsKey) { m_gsKey = gsKey; }
    inline CTextMgr*	TextMgr() const { return m_pTextMgr;		}
	

	///////////////////////////////////////////////////////////////////////////////////////
	// Virtual Functions
	///////////////////////////////////////////////////////////////////////////////////////
	virtual void		Add( CActor* pA, CPlacement vTo );
	virtual void		Remove( CActor* pA );
	virtual void		MoveTo( CActor* pA, CPlacement v );
	virtual void		MoveTo( CItem* pI, CPlacement v );
	virtual void		DeleteArea( CArea *pArea );

	virtual void		SaveAll();
	virtual void		Update(bool bForce = false);
	virtual bool		LoadEpacs();
	virtual bool		LoadCommands();
	virtual bool		LoadEmotions();
	virtual bool		LoadModules();
	virtual bool		LoadHullCubes();
	virtual bool		LoadSpace();
	virtual bool		LoadTemplates();
	virtual	bool		LoadRaces();
	virtual bool		LoadCrews();
	virtual bool		LoadAreas();
	virtual	bool		SaveAreas();
	virtual void		Describe(CActor* pA, CPlacement v, bool bDescription );
	virtual void		Describe(CActor* pA, CParentRoom* pR, bool bDescription );
	virtual bool		Interpret(CActor* pA, gString CommandLine);
	virtual bool		Initialize();
	virtual int         Write(char *fmt,...);


	// Navigation (Pathfinding)
	inline CNavigator*	Navigator()		{ return m_pNavigator; }


	///////////////////////////////////////////////////////////////////////////////////////
	// Methods designed to find and return locations and actors with the world
	///////////////////////////////////////////////////////////////////////////////////////
	CArea*				GetArea(CPlacement v);
	CArea*				GetArea(int iAreaNum);
	CActor*				GetActor(gString gsName);

// Data
public:
	int			m_iRunningTotal;
	gFileName	m_gsAreaListFile;

private:
	ActorMap	m_Players;
	ActorMap	m_Mobiles;
	ActorMap	m_Items;
	AreaList	m_Areas;
	EmoParsers  m_EmoParsers;
	CmdParsers  m_CmdParsers;
	CGalaxy		m_Galaxy;
	CTextMgr*	m_pTextMgr;
	CEpacs		m_Epacs;	// Epacs structure
	CRaceMgr	m_Races;	// Race loader
	CMLoader	m_MLoader;	// Module Loader
	CTLoader	m_TLoader;	// Template Loader
	CCLoader	m_CLoader;	// Crew Loader
	CHLoader	m_HLoader;	// Hullcube Loader
	UINT		m_ID;
	gString		m_gsName;
	gString		m_gsKey;

	CNavigator*	m_pNavigator;
    
};

typedef std::vector<CGameWorld*> WorldList;

#endif
