function port_listening() {
  lsof -i :$1 | grep 'LISTEN'
}
