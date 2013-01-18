%% Entity header file

%% State of a plant cell
-record(plant, {class="plant", age=0, growth, food}).

%% State of a herivore cell
-record(herbivore, {class="herbivore", age=0, growth, speed, hunger=0, starvation=10, vision=3, food}).

%% State of a carnivore cell
-record(carnivore, {class="carnivore", age=0, growth, speed, hunger=0, starvation=10, vision=5}).

%% State of a barrier cell
-record(barrier, {class="barrier"}).

%% State of an empty cell
-record(empty, {class="empty"}).

%% State...
-record(life, {plant, animal}).