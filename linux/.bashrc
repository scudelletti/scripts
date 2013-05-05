# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

#Deixa o usuario azul, versao do Ruby e Gemset amarela, Diretorio roxo, Git vermelho
#rvm-prompt i = Interpretador v = Versao g = Gemset
PS1='\[\033[34m\]\u \[\033[1;33m\]`~/.rvm/bin/rvm-prompt i v g \[\033[35m\]\ \w
`\[\033[0;31m\] `git branch 2> /dev/null | grep -e ^* | sed -E s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /
`\[\033[37m\]$\[\033[00m\] '

#Mongo
export PATH=$PATH:/opt/mongodb/bin
 
#Java
JAVA_HOME=/opt/java
export PATH=$PATH:/opt/java/bin
