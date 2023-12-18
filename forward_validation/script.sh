#!/bin/bash
set -ex
result_dir=results
sleeptime=5

#rm -rf "./$result_dir"
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
  experiment_dir="./$result_dir/clients"
  rm -rf "$experiment_dir"
  mkdir -p "$experiment_dir"

  echo "clients, mean, stdev" >> $experiment_dir/all.csv
  for clients in "${nr_clients[@]}"; do
    echo "$clients, $entries, $reads, $writes";
    filename="$clients.out";
    erl -noshell -pa ebin -eval "opty:start($clients, $entries, $reads, $writes, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    results=$(cat $experiment_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
    mean=$(calc_mean "$results")
    stdev=$(calc_stdev "$results")
    echo $clients, $mean, $stdev >> $experiment_dir/all.csv
  done
  cat $experiment_dir/all.csv
}


diff_entries() {
  maxtime=4
  nr_clients=2
  nr_entries=(1 10 20 30 40 50 60 70 80 90 100)
  reads=1
  writes=1
  experiment_dir="./$result_dir/entries"
  rm -rf "$experiment_dir"
  mkdir -p "$experiment_dir"

  echo "clients, entries, mean, stdev" >> $experiment_dir/all.csv
  for entries in "${nr_entries[@]}"; do
    echo "$nr_clients, $entries, $reads, $writes";
    filename="$entries.out";
    erl -noshell -pa ebin -eval "opty:start($nr_clients, $entries, $reads, $writes, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    results=$(cat $experiment_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
    mean=$(calc_mean "$results")
    stdev=$(calc_stdev "$results")
    echo $nr_clients, $entries, $mean, $stdev >> $experiment_dir/all.csv
  done
  cat $experiment_dir/all.csv
}

diff_reads() {
  sleeptime=7
  nr_clients=2
  nr_entries=10
  nr_reads=(1 2 5 10 20 30 40 50)
  nr_writes=1
  experiment_dir="./$result_dir/reads"
  rm -rf "$experiment_dir"
  mkdir -p "$experiment_dir"

  echo "clients, entries, reads, writes, mean, stdev" >> $experiment_dir/all.csv
  echo "clients, entries, reads, writes"
  for reads in "${nr_reads[@]}"; do
    echo "$nr_clients, $nr_entries, $reads, $nr_writes";
    filename="$reads.out";
    erl -noshell -pa ebin -eval "opty:start($nr_clients, $nr_entries, $reads, $nr_writes, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    results=$(cat $experiment_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
    mean=$(calc_mean "$results")
    stdev=$(calc_stdev "$results")
    echo $nr_clients, $nr_entries, $reads, $nr_writes, $mean, $stdev >> $experiment_dir/all.csv
  done
  cat $experiment_dir/all.csv
}

diff_writes() {
  sleeptime=7
  nr_clients=2
  nr_entries=10
  nr_reads=1
  nr_writes=(1 2 5 10 20 30 40 50)
  experiment_dir="./$result_dir/writes"
  rm -rf "$experiment_dir"
  mkdir -p "$experiment_dir"

  echo "clients, entries, reads, writes, mean, stdev" >> $experiment_dir/all.csv
  echo "clients, entries, reads, writes"
  for writes in "${nr_writes[@]}"; do
    args="$nr_clients, $nr_entries, $nr_reads, $writes";
    echo "$args"
    filename="$writes.out";
    erl -noshell -pa ebin -eval "opty:start($args, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    results=$(cat $experiment_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
    mean=$(calc_mean "$results")
    stdev=$(calc_stdev "$results")
    echo $nr_clients, $nr_entries, $nr_reads, $writes, $mean, $stdev >> $experiment_dir/all.csv
  done
  cat $experiment_dir/all.csv
}

diff_ratios() {
  sleeptime=7
  nr_clients=2
  nr_entries=10
  total_ops=10
  ratio_reads=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)
  experiment_dir="./$result_dir/ratios"
  rm -rf "$experiment_dir"
  mkdir -p "$experiment_dir"

  echo "clients, entries, reads, writes, mean, stdev" >> $experiment_dir/all.csv
  echo "clients, entries, reads, writes"
  for ratio in "${ratio_reads[@]}"; do
    reads=$(echo "$ratio * $total_ops /1" |bc)
    writes=$(echo "($total_ops - $reads) /1" |bc)
    args="$nr_clients, $nr_entries, $reads, $writes";
    echo "$args"
    filename="$ratio.out";
    erl -noshell -pa ebin -eval "opty:start($args, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    results=$(cat $experiment_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
    mean=$(calc_mean "$results")
    stdev=$(calc_stdev "$results")
    echo $nr_clients, $nr_entries, $reads, $writes, $mean, $stdev >> $experiment_dir/all.csv
  done
  cat $experiment_dir/all.csv
}

diff_subsets() {
  maxtime=4
  sleeptime=7
  nr_clients=10
  nr_entries=10
  nr_reads=1
  nr_writes=1
  subset_perc=(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
  experiment_dir="./$result_dir/subsets"
  rm -rf "$experiment_dir"
  mkdir -p "$experiment_dir"

  echo "clients, entries, reads, writes, mean, stdev" >> $experiment_dir/all.csv
  echo "clients, entries, reads, writes"
  # for ratio in "${ratio_reads[@]}"; do
  #   reads=$(echo "$ratio * $total_ops /1" |bc)
  #   writes=$(echo "($total_ops - $reads) /1" |bc)
  #   args="$nr_clients, $nr_entries, $reads, $writes";
  #   echo "$args"
  #   filename="$ratio.out";
  #   erl -noshell -pa ebin -eval "opty:start($args, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
  #   results=$(cat $experiment_dir/$filename | grep TOTAL | cut -d '>' -f2 | cut -d '%' -f1 | tr -d " ")
  #   mean=$(calc_mean "$results")
  #   stdev=$(calc_stdev "$results")
  #   echo $nr_clients, $nr_entries, $reads, $writes, $mean, $stdev >> $experiment_dir/all.csv
  # done
for percentage in "${subset_perc[@]}"; do
    echo $percentage;
    filename="$percentage.out";
    export subset_perc=$percentage; erl -noshell -pa ebin -eval "opty:start($nr_clients, $nr_entries, $nr_reads, $nr_writes, $maxtime)" > $experiment_dir/$filename & pid=$!; sleep $sleeptime; kill $pid
    # geomean=$(grep "Mean" subset2/$filename | awk -F '[:]' '{print $2}')
    # stddev=$(grep "Stddev" subset2/$filename | awk -F '[:]' '{print $2}')
    # echo $percentage, $geomean, $stddev >> subset2/clean
done
  cat $experiment_dir/all.csv

}

concurrent_clients
diff_entries
diff_reads
diff_writes
diff_ratios
#diff_subsets
