#!/usr/bin/env bash

CSV=$1

if [[ -z "$CSV" ]]; then
  echo "Usage: $0 path/to/statement.csv"
  exit 1
fi

if [[ ! -e "$CSV" ]]; then
  echo "'$CSV' does not exist."
  exit 1
fi

echo -n 'interest: '; grep 'Interest repayment' $CSV | cut -d, -f3 | sed 's:0.::' | awk '{s+=$1} END {print s}'
echo -n 'fees:     '; grep 'Servicing fee' $CSV      | cut -d, -f4 | sed 's:0.::' | awk '{s+=$1} END {print s}'
