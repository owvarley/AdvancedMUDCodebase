
#ifndef __TYPES_H__
#define __TYPES_H__

typedef enum t_eUserState
{
	_USR_ERROR      =   0,
	_USR_PLAYING	= 100,
	_USR_IDLE		= 101,
};

typedef enum t_eMenuOptions
{
	_OPT_QUIT = -1,
	_OPT_NO   =  0,
	_OPT_YES  =  1,
};

typedef enum t_eMenuStates
{
	_MNU_INVALID			= -99,
	_MNU_SWITCHING			= -1,
	_MNU_NONE				=  0,

	_MNU_LOGON_MENU         =  1,
	_MNU_MAIN_MENU			=  2,
	_MNU_CHARGEN_MENU       =  3,
	_MNU_OLC_MENU			=  4,
};


typedef enum t_eSize
{
	_SIZE_TINY,
	_SIZE_SMALL,
	_SIZE_NORMAL,
	_SIZE_LARGE,
	_SIZE_HUGE
};

typedef enum t_eGender
{
	_GENDER_NEUTER	= 0,
	_GENDER_MALE	= 1,
	_GENDER_FEMALE	= 2
};


#endif // __TYPES_H__

