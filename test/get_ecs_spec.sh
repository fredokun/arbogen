#!/bin/sh

if [ ! -f ecs.json ]; then
    wget "https://raw.githubusercontent.com/jlumbroso/encyclopedia-of-combinatorial-structures-data/master/ecs.json"
fi

if [ $# -eq 0 ]; then
    grep specification ecs.json | sed -Ee "s/\"specification\": \"\{(.+)\}\",/\1/g" | grep -v "Set\|Cycle\|Subst"
else
    grep specification ecs.json | sed -Ee "s/\"specification\": \"\{(.+)\}\",/\1/g" | grep -v "Set\|Cycle\|Subst" | head -n $1 | tail -n 1
fi
    
