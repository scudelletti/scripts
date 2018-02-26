# Retrieve External IP
function external_ip() {
  curl ifconfig.co
}

# List local IPs
function local_ip() {
  amount_of_lines=$1

  if [ $# -eq 0 ]
    then
    amount_of_lines=1
  fi

  ifconfig | grep "inet " | cut -f2 -d' ' | grep -v "127.0.0.1" | head -n $amount_of_lines
}
