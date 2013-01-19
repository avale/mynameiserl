%% Entity header file

%% State of a plant cell
-record(plant, {class="plant", age=0, growth, food}).

%% State of a herivore cell
-record(herbivore, {class="herbivore", age=0, exited=0, speed, hunger=0, starvation, vision, food}).

%% State of a carnivore cell
-record(carnivore, {class="carnivore", age=0, exited=0, speed, hunger=0, starvation, vision}).

%% State of a barrier cell
-record(barrier, {class="barrier"}).

%% State of an empty cell
-record(empty, {class="empty"}).

%% State...
-record(life, {plant, animal}).