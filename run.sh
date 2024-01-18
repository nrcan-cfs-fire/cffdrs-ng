#!/bin/bash
BIN=${@: 1:1}
N=`expr $# - 3`
ARGS=${@: 2:${N}}
# input & output are always last 2 arguments
INPUT=${@: -2:1}
OUTPUT=${@: -1:1}
OUTPUT_C=`echo $OUTPUT | sed "s/\.csv/.csv/g"`
OUTPUT_PY=`echo $OUTPUT | sed "s/\.csv/_py.csv/g"`
OUTPUT_R=`echo $OUTPUT | sed "s/\.csv/_r.csv/g"`
LOG=`echo $OUTPUT | sed "s/\.csv/.log/g"`

( \
    echo Running ${BIN} for ${@: 2} \
    && ./bin/${BIN} ${ARGS} ${INPUT} ${OUTPUT_C} > ${LOG} 2>&1 \
    && python ${BIN}.py ${ARGS} ${INPUT} ${OUTPUT_PY} >> ${LOG} 2>&1 \
    && Rscript ${BIN}.r ${ARGS} ${INPUT} ${OUTPUT_R} >> ${LOG} 2>&1 \
) || (echo ERROR RUNNING: $* && exit -2)

if [ -n "${CHECK_DIFF}" ]; then
    (set -x diff ${OUTPUT_C} ${OUTPUT_PY}) || (echo Python output does not match C && exit -1)
    (set -x diff ${OUTPUT_C} ${OUTPUT_R}) || (echo R output does not match C && exit -1)
    echo All files match ${OUTPUT_C}
fi;
