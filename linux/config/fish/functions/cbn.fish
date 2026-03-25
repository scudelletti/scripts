function cbn
  echo -n (git symbolic-ref --short -q HEAD) | ds-clipboard.sh -i
end