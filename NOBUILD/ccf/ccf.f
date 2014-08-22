c  program findcff
c
c  reads in stresses and locations from the output of dis3dop,
c  and finds, for each point, the principal stresses and their
c  orientation, given the background stress (read in from the
c  command line). Given this, finds the plane with shear and normal
c  stress with the largest CFF; and for that plane, computes the
c  change in CFF from the dis3dop numbers.
c
c	calls jacobi (numerical recipes) to find the principal stress
c       calls shells (genlib) to find the min and max of these
c
      character*40 dumm
      dimension tr(3,3),tc(3,3),tt(3,3),pse(3,3),ps(3),k(3),
     1 psn(3,3),rnmax(3),trac(3),tau(3)
      data pi/3.1415926535/
c
c read the regional stresses, and the coefficient of internal friction
c and Skempton's coefficient, from the command line
c
      call getarg(1,dumm)
      read(dumm,*) tr(1,1)
      call getarg(2,dumm)
      read(dumm,*) tr(2,2)
      call getarg(3,dumm)
      read(dumm,*) tr(3,3)
      call getarg(4,dumm)
      read(dumm,*) tr(1,2)
      tr(2,1) = tr(1,2)
      call getarg(5,dumm)
      read(dumm,*) tr(1,3)
      tr(3,1) = tr(1,3)
      call getarg(6,dumm)
      read(dumm,*) tr(2,3)
      tr(3,2) = tr(2,3)
      call getarg(7,dumm)
      read(dumm,*) cmuf
      th=(pi/4.)-0.5*atan(cmuf)
      call getarg(8,dumm)
      read(dumm,*) bsk
c       
c now read the coseismic stresses, from standard input
c
C 1    read(5,103,end=31) x,y,z, tc(1,1),tc(2,2),tc(3,3), tc(1,2),tc(1,3),tc(2,3)
C 103  format(1x,4e11.3,5e11.3)
 1    read(5,103,end=31) tc(1,1),tc(2,2),tc(3,3),tc(1,2),tc(1,3),tc(2,3)
 103  format(1x,6e11.3)
      tc(2,1) = tc(1,2)
      tc(3,1) = tc(1,3)
      tc(3,2) = tc(2,3)
      do 3 i=1,3
      do 3 j=1,3
      tt(i,j) = tr(i,j) + tc(i,j)
 3    continue
C      call mapr(tc,3,3)
C      call mapr(tt,3,3)
      call jacobi(tt,3,3,ps,pse,nrot)
C      write(6,*) ps
C      call mapr(pse,3,3)
c 
c ps is now the vector of principal stresses,
c  and the columns of pse the direction cosines
c  of the axes. Sort the stresses into increasing
c  order, and copy the axes to another array to
c  match
c
c i is row, j is column
c
      call shells(ps,k,3)
      do 9 i=1,3
      do 7 j=1,3
      psn(i,j) = pse(j,k(i))
 7    continue
 9    continue
C      call mapr(psn,3,3)
c
c  now find the direction normal to the plane with max total CFF stress
c  note that the max stress is #3 after sorting
c
c i is column
c
      do 11 i=1,3
      trac(i) = 0.0
 11   rnmax(i) = psn(3,i)*cos(th) + psn(1,i)*sin(th)
C      write(6,*) rnmax
c
c  now compute the CFF stress on this plane, for the coseismic stress only.
c
c   pore pressure
      pp= -(bsk/3.)*(tc(1,1)+tc(2,2)+tc(3,3))
C      write(6,*) 'pp',pp
c   traction vector
c
c i is row, j is column
c
      do 15 i=1,3
      do 13 j=1,3
      trac(i) = trac(i) + tc(i,j)*rnmax(j)
 13   continue
 15   continue
C      write(6,*) 'trac',trac
c  normal and shear vector, and their magnitudes
      sigmag=0.0
      taumag=0.0
      do 17 i=1,3
 17   sigmag = sigmag + trac(i)*rnmax(i)
C      write(6,*) sigmag
      do 19 i=1,3
      tau(i) = trac(i)-sigmag*rnmax(i)
      taumag=taumag+tau(i)**2
 19   continue
      taumag=sqrt(taumag)
C      write(6,*) tau
C      write(6,*) taumag
c CFF magnitude
      cff = taumag + cmuf*(sigmag + pp)
      write(6,105) cff
 105  format(e13.5)
      go to 1
 31   continue
      stop
      end
      subroutine mapr(a,m,n)
      dimension a(m,n)
      do 1 i = 1,m
      write(6,100) (a(i,j),j=1,n)
 100  format(1x,10g13.5)
  1   continue
      write(6,110)
 110  format(//)
      return
      end
