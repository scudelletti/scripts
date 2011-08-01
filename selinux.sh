#Desabilitar
echo 0 > /selinux/enforce

#Habilitar
echo 1 > /selinux/enforce

#Executar
sudo /usr/sbin/getenforce
sudo /usr/sbin/sestatus 
