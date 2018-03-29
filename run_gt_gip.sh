#BSUB -a poe

#BSUB -W 24:00
#BSUB -n 1
##BSUB -R "span[ptile=16]"

#BSUB -x
#BSUB -o GT_GIP.stdout.%J
#BSUB -e GT_GIP.stderr.%J
#BSUB -P SPACE-T2O
#BSUB -q "special_proj"
#BSUB -J "gt_gip"
#BSUB -R "affinity[core]"
#BSUB -L /bin/bash

.  /usrx/local/Modules/default/init/bash
module purge
module load ics
module load NetCDF/3.6.3
module load lsf
module list

export MP_EUILIB=us
export MP_EUIDEVICE=sn_all
export MP_DEBUG_NOTIMEOUT=yes

set -x

cd /swpc/noscrub/George.Millward/gip/gt-gip

date

mpirun.lsf ./gt_gip_model < GT_GIP_input_file > GT_GIP_logfile.out

date
