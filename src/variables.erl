-module(variables).
-compile([export_all]).

plantGrowth() -> 4.

herbivoreVision() -> 3.
carnivoreVision() -> 12.

herbivoreStarvation() -> 20.
carnivoreStarvation() -> 20.

visionRange() -> {2, 20}.
starvationRange() -> {10, 30}.