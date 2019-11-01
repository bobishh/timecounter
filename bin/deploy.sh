#!/usr/bin/env bash

host=vpn
target_path=/home/bovoid/timecounter/

release_name=release-$(date +"%d%m%Y_%H%M").tar

elm-app build
tar cf $release_name build
scp $release_name $host:~/
ssh -t $host "mv $release_name $target_path/$release_name; cd $target_path; tar xf $release_name; mv current current.last; mv build current;"
echo "FINISHED."
