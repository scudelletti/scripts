function ll --wraps=ls --wraps='ls -lha' --description 'alias ll=ls -lha'
  ls -lha $argv; 
end
