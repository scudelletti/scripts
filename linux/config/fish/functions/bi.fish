function bi --wraps='bundle check || bundle install' --description 'alias bi=bundle check || bundle install'
  bundle check || bundle install $argv; 
end
