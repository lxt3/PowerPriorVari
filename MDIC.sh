#!/bin/sh
# #$ -P osb-dbs               # Project name in our Sun Grid Engine set for OSB-DBS
#$ -cwd                       # Run job in current dir
#$ -S /bin/sh                 # Use specified shell
#$ -j y                       # Put errors into out-file
# -N MDIC_sim               # The name of my job:
#$ -M laura.thompson@fda.hhs.gov       # Send email to your account
#$ -N MDIC_sim # The name of my job:
# -N no_prior_sim
# Send emailon:
# b - at the beginning, e - at the end of the job.
# a - when the job is rescheduled or aborted (for example, by using the qdel command).
# s - when the job is suspended.
#$ -m beas
#$ -v RAND_INIT

#$ -pe thread 1               # One thread per processor, always true for R
#$ -t 1-100 # Run program on 125 processors

export RAND_INIT=`date +%s`

echo "Running array job $SGE_TASK_ID on $HOSTNAME"     # output this message to the display

START_STIME=`date +%Y%m%dT%H%M%S`
START_TIME=`date +%s`

#mkdir ~/R/wd$SGE_TASK_ID

source /projects/mikem/R/source.sh 
time nohup R CMD BATCH --no-restore-data --no-save templateHPC.R log$SGE_TASK_ID.txt

#time nohup R CMD BATCH --no-restore-data --no-save interimlooksESSmultprocnormalnopriorinfo.R log$SGE_TASK_ID.txt

rm MDIC_sim.o* 

END_TIME=`date +%s`
ELAPSED_TIME=`expr $END_TIME - $START_TIME`

echo "$START_STIME $HOSTNAME $SGE_TASK_ID log$SGE_TASK_ID.txt $ELAPSED_TIME" >> stats-run-pr.csv
#mv *.csv ~/R/ASANJOutput/


#rmdir ~/R/wd$SGE_TASK_ID
