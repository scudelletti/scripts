function cbn
  echo -n (git symbolic-ref --short -q HEAD) | pbcopy
end