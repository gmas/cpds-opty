#!/bin/bash
result_dir=results
sleeptime=5

rm -rf "./$result_dir"
mkdir -p "./$result_dir"
erl -make


calc_mean() {
  lines=$1
  mean_r=$(echo "$lines" | awk '{ total += $1; count++ } END { print total/count }')
  echo "$mean_r"
}

calc_stdev() {
  lines=$1
  stdev_r=$(echo "$lines" | awk '{sum+=$1; sumsq+=$1*$1}END{print sqrt(sumsq/NR - (sum/NR)**2)}')
  echo "$stdev_r"
}

concurrent_clients() {
  maxtime=4
  nr_clients=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
  entries=10
  reads=1
  writes=1
  echo "clients, mean, stdev" >> $result_dir/all.csv
  for clients in "${nr_clients[@]}"; do
    echo "$clients, $entries, $reads, $writes";
    filename="$clients.out";
    erl -noshell -pa ebin -eval "opty:start($clients, $entries, $reads, $writes, $maxtime)" > $result_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    results=$(cat $result_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
    mean=$(calc_mean "$results")
    stdev=$(calc_stdev "$results")
    echo $clients, $mean, $stdev >> $result_dir/all.csv
  done
}

concurrent_clients
