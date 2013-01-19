-module(variables).
-compile([export_all]).

plantGrowth() -> 4.

herbivoreVision() -> 3.
carnivoreVision() -> 12.

herbivoreStarvation() -> 30.
carnivoreStarvation() -> 20.

visionRange() -> {2, 20}.
starvationRange() -> {10, 30}.

herbivoreCooldownTime() -> 3.
carnivoreCooldownTime() -> 5.

herbivoreMatureAge() -> 4.
carnivoreMatureAge() -> 6.