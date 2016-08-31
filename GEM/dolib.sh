#!/bin/bash
gfortran -c conrec.f
gfortran -c cmplx_root2.f90
gfortran -c i1mach.f
gfortran -c asa007.f90
gfortran -c xgetua.f
gfortran -c xerbla.f
gfortran -c ilaenv.f
gfortran -c dgetf2.f
gfortran -c dlaswp.f
gfortran -c dtrsm.f
gfortran -c dgemm.f
gfortran -c dtrtri.f
gfortran -c dswap.f
gfortran -c dgemv.f
gfortran -c lsame.f
gfortran -c dtrti2.f
gfortran -c dtrmm.f
gfortran -c dlamch.f
gfortran -c idamax.f
gfortran -c dscal.f
gfortran -c dger.f
gfortran -c dtrmv.f
gfortran -c dgetri.f
gfortran -c dgetrf.f
gfortran -c fdump.f
gfortran -c j4save.f
gfortran -c xercnt.f
gfortran -c xerprn.f
gfortran -c xersve.f
gfortran -c xerhlt.f
gfortran -c xermsg.f
gfortran -c d1mach.f
gfortran -c dqpsrt.f
gfortran -c dqk15i.f
gfortran -c dqk15.f
gfortran -c dqk31.f
gfortran -c dqk41.f
gfortran -c dqk51.f
gfortran -c dqk61.f
gfortran -c dqelg.f
gfortran -c dqagie.f
gfortran -c dqagi.f
gfortran -c dqag.f
gfortran -c dqage.f
gfortran -c dqk21.f
gfortran -c dqags.f
gfortran -c dqagse.f
gfortran -c dqng.f
gfortran -c gint.f
gfortran -c tipos.f90
gfortran -c lu.f90
gfortran -c polint.f90 tipos.o
gfortran -c numint.f90 tipos.o polint.o
gfortran -c param_dp_VA.f90 lu.o tipos.o
gfortran -c complex.f90 tipos.o param_dp_VA.o
ar rcs libchi2fov.a *.o *.mod
mv ./libchi2fov.a ./lib/libchi2fov.a
rm *.o


