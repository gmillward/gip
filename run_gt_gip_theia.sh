#!/bin/sh --login
#
#PBS -l procs=1
#PBS -l walltime=30:00
#PBS -A swpc
#PBS -q debug
#PBS -N gt_gip

module purge
module load intel/12-12.0.4.191
module load impi
module load netcdf/3.6.3
module list

cd /scratch3/NCEPDEV/swpc/noscrub/George.Millward/gip_repo/gip
./create_run_timeline.py

date

mpirun -np 1 ./gt_gip_model < GT_GIP_input_file > GT_GIP_logfile.out

date
