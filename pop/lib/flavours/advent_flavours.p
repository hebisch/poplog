/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/advent_flavours.p
 > Purpose:        Example flavours program - Adventure game.
 > Author:         Mark Keefe & Mark Rubinstein, Apr 21 1986
 > Documentation:  TEACH * FLAVOURS
 > Related Files:  LIB * FLAVOURS
 */

;;; This program is provided as an example of a program written using
;;; flavours.  It is not intended to be a particularly impressive example of
;;; adventure games.

;;; -- INANIMATE OBJECTS IN THE GAME ---------------------------------------
;;; The following class is for treasures, etc. which players may pick
;;; up as they wander around

vars objects;

flavour object isa named_object;
ivars value weight;
    defmethod before initialise;
        [^self ^^objects] -> objects;
    enddefmethod;
    defmethod displayself;
        [^name value ^value weight ^weight] =>
    enddefmethod;
endflavour;

;;; -- ROOMS ---------------------------------------------------------------
;;; The "adventure world" consists of a number of "room" objects

vars rooms;

flavour room isa named_object;
ivars
    inhabitants = [],   ;;; The players currently in this room
    north = false,      ;;; )
    south = false,      ;;; ) The adjoining rooms in various directions
    east = false,       ;;; )
    west = false,       ;;; )
    local_objects = [], ;;; The objects lying in the room
    the_way_out         ;;; directions out of the room.
    ;

    defmethod before initialise;
        self :: rooms -> rooms;  
    enddefmethod;
    defmethod displayself;
        [^name inhabitants ^inhabitants objects ^local_objects] ==>
    enddefmethod;
    defmethod enters(player);
    lvars player;
        player :: inhabitants -> inhabitants;
        [^player has entered the ^name] =>
    enddefmethod;
    defmethod leaves(player);
    lvars player;
        [^player has left the ^name] =>
        delete(player, inhabitants) -> inhabitants
    enddefmethod;
    defmethod taken(object);
    lvars object;
        delete(object, local_objects, 1) -> local_objects;
        [^object removed from ^name] =>
    enddefmethod;
    defmethod dropped(object);
    lvars object;
        object :: local_objects -> local_objects;
        [^object dropped in ^name] =>
    enddefmethod;
    defmethod show_way_out;
        printf(the_way_out, 'To get to the fortress you must travel:\n%p\n');
    enddefmethod;
endflavour;

;;; -- GOAL ROOM -----------------------------------------------------------
;;; specialised flavour for goal room
flavour goal_room isa room;
    defmethod after enters;
        ;;; head of inhabitants will just have entered (there should only be one)
        hd(inhabitants) <- won
    enddefmethod;
endflavour;

;;; -- PLAYERS -------------------------------------------------------------
;;; The class for the Players.

vars players play possessions;         

;;; find the object in -objectlist- whose name is -name-
define namemember(name, objectlist);
lvars objectlist name object;
    for object in objectlist do
        if object<-name == name then
            return(object)
        endif
    endfor;
    false;
enddefine;

;;; special one for finding the named object in the list called possessions.          
define get_object(object_name);
lvars object_name;
    namemember(object_name, possessions);
enddefine;

;;; print prompt and read an item (ignoring newlines).
define requestitem(popprompt) -> item;
lvars item;
vars popnewline = true;
    while (readitem() ->> item) == newline do endwhile;
enddefine;

flavour player isa named_object;
ivars
    points = 0,         ;;; total value of objects in possessions
    health = 100,       ;;; lies between 0 and 100, approximately
    capacity = 200,     ;;; max weight player can pick up
    location            ;;; room the player is in
    ;
divars
    possessions = [],   ;;; objects currently carried by the player
    ;

    defmethod before initialise;
        self :: players -> players;     ;;; Update the (global) set of Players
    enddefmethod;
    defmethod before updaterof location;    ;;; location is an active variable
        location <- leaves(self);
    enddefmethod;
    defmethod after updaterof location;
        location <- enters(self);
    enddefmethod;
    defmethod go_there(dir);    ;;; go to a place
    lvars place dir;
        location(dir) -> place;
        if place then
            place -> (self<-location);
        else
            [^name chose wrong direction]=>
        endif
    enddefmethod;
    defmethod displayself;
        [^name points ^points health ^health capacity ^capacity
            possessions ^possessions location ^location] ==>
    enddefmethod;
    defmethod doeat(food);
    lvars food;
        health + food<-value -> health;
        points - food<-value -> points;
        ;;; capacity same
        delete(food, possessions, 1) -> possessions;
        [^name eats some food]=>
    enddefmethod;
    defmethod eat;
    lvars object;
        get_object("food") -> object;
        if object then
            self <- doeat(object)
        else
            [^name tried to eat food but had none]=>
        endif
    enddefmethod;
    defmethod dropobject(object);
    lvars object;
        location <- dropped(object);
        delete(object, possessions, 1) -> possessions;
        points - object<-value -> points;
        capacity + object<-weight -> capacity;
    enddefmethod;
    defmethod takeobject(object);
    lvars object;
        [^name takes ^object]=>
        location <- taken(object);
        object :: possessions -> possessions;
        points + object<-value -> points;
        capacity - object<-weight -> capacity;
    enddefmethod;
    defmethod hit;          ;;; self has been hit
    lvars object obs;
        ;;; player recieves a hit from another player; check if wearing shield
        if (get_object("shield") ->> object) then
            if health <= 50 then                ;;; weak player drops an object
                delete(object, possessions) -> obs;     ;;; (not shield)
                unless obs == [] do
                    self<-dropobject(oneof(obs));
                endunless
            else                                ;;; just a scratch
                health - 20 -> health;
            endif
        else                                    ;;; Hit causes serious injury
            health/2.0 -> health;
            if health <= 10 then
                ;;; player dies
                [^name dies]=>
                ;;; drop all your possessions
                for object in possessions do
                    self <- dropobject(object);
                endfor;
                delete(self, players) -> players;
                location <- leaves(self);
            endif
        endif
    enddefmethod;
    defmethod won;
        exitfrom(play);
    enddefmethod;
endflavour;

;;; -- MACHINE PLAYERS -----------------------------------------------------
flavour machine_player isa player;
    defmethod strategy;             ;;; default strategey
    lvars object;
        if null(possessions) then
            self <- trymove;
        elseif (oneof(possessions) ->> object) <- name == "food" then
            self <- eat(object)
        else
            self <- trymove
        endif;
    enddefmethod;
    defmethod trymove;
        self <- go_there(oneof([north south east west]))
    enddefmethod;
    defmethod move;
        if health > 10 then self <- strategy endif
    enddefmethod;
    defmethod before won;
        [^name HAS BEATEN YOU TO THE FORTRESS.  GAME OVER] =>
    enddefmethod;
endflavour;

flavour human_player isa player;
lconstant
    valid_responses = [north south east west take hit eat read drop options show quit],
    show_items = [options me rooms players objects all],
    ;

    defmethod hitout;
    lvars people victim response = false;
        unless length(location<-inhabitants ->> people) == 1 do
            delete(self, people) -> people;
            until namemember(response, people) do
                requestitem('\nHit who ' >< people >< ' or "show" : ') -> response;
                if response == "show" then
                    for victim in people do victim <- displayself endfor;
                elseif (namemember(response, people) ->> victim) then
                    [^name hits ^response] =>
                    victim <- hit;
                else
                    [Invalid response - try again]=>
                endif
            enduntil
        else
            [no other players in ^location] =>
        endunless
    enddefmethod;
    defmethod read_map;
    lvars object;
        if (get_object("map") ->> object) then
            ;;; get the location to show the way out
            location <- show_way_out;
            ;;; lose the map
            self <- dropobject(object);
        else
            [^name tried to read map but had none]=>
        endif
    enddefmethod;
    defmethod take;
    lvars obs object response = false;
        unless (location<-local_objects ->> obs) == [] do
            until namemember(response, obs) do
                requestitem('\nTake what ' >< obs >< ', show, drop or menu: ') -> response;
                if (namemember(response, obs) ->> object) then
                    if object<-weight > capacity then
                        [^object too heavy - drop something or try another]=>
                        false -> response;
                    else
                        self <- takeobject(object);
                    endif
                elseif response == "show" then
                    for object in obs do object<-displayself endfor;
                elseif response == "drop" then
                    self <- drop;
                    return;
                elseif response == "menu" then
                    self <- move;
                    return;
                else
                    [Invalid response - try again]=>
                endif;
            enduntil;
        else
            [no objects present] =>
        endunless
    enddefmethod;
    defmethod drop;
    lvars item response = false;
        unless null(possessions) do
            until namemember(response, possessions) do
                requestitem('\nDrop what ' >< possessions >< ', show or menu: ') -> response;
                if (get_object(response) ->> item) then
                    self <- dropobject(item)
                elseif response == "show" then
                    for item in possessions do item<-displayself endfor;
                elseif response == "menu" then
                    self <- move;
                    return;
                else
                    [Invalid response - try again]=>
                endif
            enduntil
        else
            [no items to drop]=>
        endunless
    enddefmethod;
    defmethod show;
    lvars x response = false;
        until member(response, show_items) do
            requestitem('\nShow what ' >< show_items >< ': ') -> response;
            if response == "options" then
                npr(valid_responses)
            elseif response == "me" then
                self <- displayself
            elseif response == "rooms" then
                for x in rooms do x<-displayself endfor;
            elseif response == "players" then
                for x in players do x<-displayself endfor;
            elseif response == "objects" then
                for x in objects do x<-displayself endfor;
            elseif response == "all" then
                for x in rooms do x<-displayself endfor;
                for x in players do x<-displayself endfor;
                for x in objects do x<-displayself endfor;
            else
                [Invalid response - try again]=>
            endif;
        enduntil;
    enddefmethod;
    defmethod welcome;
    lvars x;
        printf(name, '\nWelcome to Adventure %p!\n\n');
        for x in rooms do x<-displayself endfor;
        for x in players do x<-displayself endfor;
        pr(newline);
    enddefmethod;
    defmethod move;
    lvars response x;
        pr(newline >< valid_responses >< newline);
        until member(response, valid_responses) do
            requestitem('\nYour move ' >< name >< ': ') -> response;
            if response == "take" then
                self <- take;
            elseif response == "drop" then
                self <- drop;
            elseif response == "hit" then
                self <- hitout;
            elseif response == "read" then
                self <- read_map;
            elseif response == "eat" then
                self <- eat;
            elseif member(response, [north south east west]) then
                self <- go_there(response);
            elseif response == "options" then
                npr(valid_responses); false -> response
            elseif response == "show" then
                self <- show; false -> response
            elseif lmember(response, [quit ^termin]) then
                for x in players do x<-displayself endfor;
                exitfrom(play);
            elseif response == "pop11" then
                popready(); false -> response;
            else
                [Invalid response - try again] =>
            endif
        enduntil
    enddefmethod;
    defmethod before won;
        [CONGRATULATIONS ^name.  YOU HAVE WON] =>
    enddefmethod;
endflavour;

;;; -- SPECIAL KINDS OF MACHINE_PLAYERS ------------------------------------
;;; More interesting types of player, with more specialised strategies
flavour greedy isa machine_player;
    defmethod strategy;
    lvars obs object;
        if (get_object("food") ->> object) then     ;;; first see if any food
            self <- doeat(object)                   ;;; to eat
        else                                    ;;; try to pick up an object
            location<-local_objects -> obs;
            until obs == [] do
                oneof(obs) -> object;
                delete(object, obs, 1) -> obs;
                if object<-weight <= capacity then
                    self <- takeobject(object);
                    return
                endif;
            enduntil;
            self <- trymove                 ;;; no object taken - try to move
        endif
    enddefmethod;
endflavour;

flavour monster isa machine_player;
    defmethod strategy;
    lvars people victim;
        unless length(location<-inhabitants ->> people) == 1 then
            delete(self, people) -> people;
            oneof(people) -> victim;
            [^name hits ^victim]=>
            victim <- hit;
        else
            self <- trymove
        endunless
    enddefmethod;
endflavour;

;;; -- CHANGE PRINTING OF NAMED_OBJECTS ------------------------------------
flavour named_object;
    defmethod printself;
        pr(name);
    enddefmethod;
endflavour;

;;; -- THE GAME ------------------------------------------------------------

define set_up();
lvars gold food shield map jewels ring dungeon swamp pit cave fortress
    player;

    ;;; Creating the objects for a simulation
    ;;; First set up some objects
    [] -> objects;
    make_instance([object name gold value 100 weight 100])  -> gold;
    make_instance([object name food value 50 weight 25])    -> food;
    make_instance([object name shield value 150 weight 50]) -> shield;
    make_instance([object name map value 50 weight 25])     -> map;
    make_instance([object name jewels value 500 weight 75]) -> jewels;
    make_instance([object name ring value 75 weight 50])    -> ring;

    ;;; Now set up the rooms
    [] -> rooms;
    make_instance([room name dungeon the_way_out [north east north]])-> dungeon;
    make_instance([room name swamp the_way_out [east north east north]])-> swamp;
    make_instance([room name pit the_way_out [east north]]) -> pit;
    make_instance([room name cave the_way_out [north]])     -> cave;
    make_instance([goal_room name fortress])                -> fortress;

    [^shield ^food ^ring] -> (dungeon<- local_objects);
    swamp -> (dungeon<-west);
    pit -> (dungeon<-north);

    [^food ^map] -> (swamp<-local_objects);
    dungeon -> (swamp<-east);

    [^food ^jewels] -> (pit<-local_objects);
    dungeon -> (pit<-south);
    cave -> (pit<-east);

    [^food ^gold] -> (cave<-local_objects);
    pit -> (cave<-west);
    fortress -> (cave<-north);

    cave -> fortress<-south;

    ;;; Now the Players
    [] -> players;
    make_instance([greedy name seeker]) ->;
    make_instance([monster name orc]) ->;
    make_instance([machine_player name aimless]) ->;
    make_instance([human_player name ^popusername]) ->;
    make_instance([monster name troll]) ->;

    for player in players do
        dungeon -> ivalof(player, "location");
    endfor;
    players -> ivalof(dungeon, "inhabitants");
    for player in players do
        if isinstance(player, human_player_flavour) then
            player <- welcome;
        endif;
    endfor;
enddefine;

define play();
vars p;
    set_up();
    repeat
        for p in players do
            p <- move;
            pr(newline);
        endfor
    endrepeat;
enddefine;

pr('=== WELCOME TO THE WORLD OF ADVENTURE ============\
First written (using LIB NEWOBJ) by Mark Keefe.\
Rewritten (using LIB FLAVOURS) by Mark Rubinstein.\
\
To play the game, type:\n\
    play();\
\
GOOD LUCK!\n');
