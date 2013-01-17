%% Entity header file

%% State of a plant cell
-record(plant, {hex="#FFFFFF", age=0, growth, food}).

%% State of a herivore cell
-record(herbivore, {hex="#FFFFFF", age=0, growth, speed, hunger=0, vision, food}).

%% State of a carnivore cell
-record(carnivore, {hex="#FFFFFF", age=0, growth, speed, hunger=0, vision}).

%% State of a barrier cell
-record(barrier, {hex="#FFFFFF"}).

%% State of an empty cell
-record(empty, {hex="#FFFFFF"}).

%% State...
-record(life, {plant=#empty{}, animal=#empty{}}).