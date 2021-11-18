function port_listening
  lsof -i :$argv | grep 'LISTEN'
end