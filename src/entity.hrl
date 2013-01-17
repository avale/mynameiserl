%% Entity header file

%% State of a plant cell
-record(plant, {class="#FFFFFF", age=0, growth, food}).

%% State of a herivore cell
-record(herbivore, {class="#FFFFFF", age=0, growth, speed, hunger=0, vision, food}).

%% State of a carnivore cell
-record(carnivore, {class="#FFFFFF", age=0, growth, speed, hunger=0, vision}).

%% State of a barrier cell
-record(barrier, {class="#FFFFFF"}).

%% State of an empty cell
-record(empty, {class="#FFFFFF"}).

%% State...
-record(life, {plant, animal}).