#!/bin/bash
sleeptime=5
erl -make

concurrent_clients() {
  maxtime=4
  num_clients=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
  d_entries=10
  d_reads=1
  d_writes=1
  for clients in "${num_clients[@]}"; do
    echo "$clients, $d_entries, $d_reads, $d_writes";
    filename=opty"$clients".out;
    erl -noshell -pa ebin -eval "opty:start($clients, $d_entries, $d_reads, $d_writes, $maxtime)" > numclients/$filename & pid=$!; sleep $sleeptime; kill $pid
    geomean=$(grep "Mean" numclients/$filename | awk -F '[:]' '{print $2}')
    stddev=$(grep "Stddev" numclients/$filename | awk -F '[:]' '{print $2}')
    echo $clients, $geomean, $stddev >> numclients/clean
  done
}

concurrent_clients
