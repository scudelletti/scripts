# Copia os arquivos locais para o servidor remoto utilizando a pasta temporaria /tmp
# 
# -C     Copia Diferencial, 
# -r     Recursiva
# -a     --archive archive mode; equals -rlptgoD (no -H,-A,-X)
# -v     Verbosa
# -z     Compessao na Transmissao
# -p     Preserva as Permissões
# -l     Mantem os Links Simbólicos
# -E     Mantem os Executáveis
# -T     Utiliza pasta temporaria especificada
rsync -CravzplE /origem/ servidor:/destino -T /tmp
