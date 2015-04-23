#!/bin/bash
set -e
$(boot2docker shellinit 2> /dev/null)
docker run -v ${PWD}/self:/root/growth_trajectories -it traitecoevo/growth_trajectories $*
