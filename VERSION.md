#	AMC v0.04.1 (17/12/2005) 
## VERSION CTRL

###MAJOR CHANGES
* [17/12/2005] - {OWV} Rewrite of Component and Module code to bring inline with new Design Concept TruShip [CL 1.1]
* [17/12/2005] - {OWV} Retooling of Shape Command to allow Creation of Ships from Templates [CL 1.2]
* [17/12/2005] - {OWV} Ship States to allow for Launching, Landing, Flying, Hovering on Repulsors
* [19/12/2005] - {OWV} Introduced a SCM system to prevent unsafe handling of ships 
* [20/12/2005] - {OWV} Comprehensive Ship Damage System: Damage is pushed down to the Leaves (Modules)
* [22/12/2005] - {OWV} Rewrite of Design command and implementation of personalised Design Command Interpreter [CL 1.3]
* [03/01/2006] - {OWV} Implemented the ability to Roll the ship either Fully Left, Fully Right or Inverted
* [04/01/2006] - {OWV} Implemented the ability to Land a ship on the ground, can also hover above the ground
* [10/01/2006] - {OWV} Implemented the room limitation on Space commands, i.e. being in a ship
* [11/01/2006] - {OWV} Implemented Open/Close and Boarding Space Ships
* [12/01/2006] - {OWV} Ported the Communications Code over from the Old Component System
* [12/01/2006] - {OWV} Ported the Radar/Sweep code over updated it to use new Components/Modules
* [19/01/2006] - {OWV} Colourised the Template Design and Power system
* [25/01/2006] - {OWV} Added an advanced Distance method for finding the minimum distance between shapes
* [01/02/2006] - {OWV} Added the ability to go Prone, Crouch, Sit and Stand
* [01/02/2006] - {OWV} Updated all Space Commands with colour and syntax
* [06/02/2006] - {OWV} Implemented a Clone command for Components in the Design system
* [08/02/2006] - {OWV} Implemented the Fire and Target command for space combat
* [10/02/2006] - {OWV} Updated HullCubes to include Keel values and modified Damage function
* [13/02/2006] - {OWV} Added the ability to Man consoles
* [13/02/2006] - {OWV} Rewrote the Notify command to Write bringing it inline with MUDCore programming
* [14/02/2006] - {OWV} Removed Space Spam problem by implementing Terminals for storing/receiving messages
* [16/02/2006] - {OWV} Integrated latest MUDCore functions gLog, XML and Menu system into AMC
* [17/02/2006] - {OWV} Added Crew structure for defining and installing Crews on Ships
* [18/02/2006] - {OWV} Expanded Crew System to accept Commands using the MC Event System
* [18/02/2006] - {OWV} Added the ShipNet Communications function
* [24/02/2006] - {OWV} Added Fire function to use ePACS space rolls
* [01/03/2006] - {OWV} Added a variety of help files
* [04/03/2006] - {OWV} Updated the Shutdown the command to include a message
* [07/03/2006] - {OWV} Implemented a message when the MUD crashes and wrote a Startup bat to run AMC
* [28/03/2006] - {OWV} Implemented the Skill Manager (ePACS)

###BUGSFIXES
* [17/12/2005] - {OWV} Fixed problem with Areas being loaded before Worlds were initialised
* [18/12/2005] - {OWV} Fixed minor problems with the Area files being saved/loaded incorrectly
* [19/12/2005] - {OWV} Fixed menu issues with crashes see (Buglist.txt MCB 1.1 and MCB 1.2)
* [09/03/2006] - {OWV} Fixed problem with objects be in your fore but getting further away
* [15/03/2006] - {OWV} Fixed bug with colour codes in MudCore (Buglist.txt MCB 1.5)

#MINOR CHANGES
* [17/12/2005] - {OWV} Added #TB to allow Tabs to be entered into text blocks
* [17/12/2005] - {OWV} Renamed help to Commands
* [17/12/2005] - {OWV} Added areas Command to list all areas, also added Author field to areas
* [17/12/2005] - {OWV} Added destroy method to Components
* [17/12/2005] - {OWV} Modified Design command to allow armour to be slotted into Hullcubes
* [19/12/2005] - {OWV} Implemented the Attributes Manager and Attributes Factory for MUDCore 2.3.1 [CL 2.1]
* [22/12/2005] - {OWV} Added the ship field to areas to allow areas to belong to ships
* [22/12/2005] - {OWV} Added the STAFF type to Players [CL 2.2]
* [23/12/2005] - {OWV} Modified the CShape class's usage, it now is a shortcut to HullCube structure [CL 2.3]
* [23/12/2005] - {OWV} Modified Frame, Hull, Component and Modules to implement their own implementation of the operator =
* [23/12/2005] - {OWV} Modified the DescribeTo function to allow Players to view areas that belong to Templates [CL 2.4]
* [23/12/2005] - {OWV} Changed DescribeTo back to original form. [CL 2.4]
* [25/12/2005] - {OWV} Implemented the Rotate function for Cartesian co-ordinates allowing them to be rotated
* [26/12/2005] - {OWV} Modified the Bearing and Implemented the Arc Function for Cartesians
* [26/12/2005] - {OWV} Succesfully debugged the Arc function to include Facing
* [26/12/2005] - {OWV} Modified Cartesian Course function to include the ability to mod Z
* [01/01/2006] - {OWV} Modified Cartesian Bear and Mark to work properly
* [01/01/2006] - {OWV} Modified Cartesian Arc function to work (finally!)
* [05/01/2006] - {OWV} Changed order of Command Parser loading so Design is checked before Default
* [06/01/2006] - {OWV} Modified Design Cmds: List, Edit and Install commands to work for areas and arcs
* [06/01/2006] - {OWV} Implemented the Design Cmds: Goto and Review
* [07/01/2006] - {OWV} Debugged Area section of Design code and Updated Assigning Components to Rooms
* [12/01/2006] - {OWV} Added Uninstall ability for Components and added Install ability for Exits
* [19/01/2006] - {OWV} Repaired the Shield command for working with TruShip
* [13/02/2006] - {OWV} Updated some of the Space Combat modules to make it more realistic
* [17/02/2006] - {OWV} Changed the Atomic Event Handler to correctly handle time delays on Events
* [18/02/2006] - {OWV} Added the GiveEvent function to Ships to allow On duty crew to receive events
* [19/03/2006] - {OWV} Modified goto to allow you to goto players
* [27/03/2006] - {OWV} Added in the ability to install multiple components in a single room
* [27/03/2006] - {OWV} Added in the ability for players to assume command of crew teams


