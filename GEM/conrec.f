c      program demo
c      implicit none
cc
cc     The follwoing is a simplistic application of the CONREC routine.
cc     A mathematical function is evaluated over a regular grid of points
cc     on a computer raster graphics screen.
cc
cc     Paul D. Bourke
cc
c      integer*4 pxmin,pymin,pxmax,pymax
c      parameter (pxmin = 10,pymin = 10,pxmax = 460,pymax = 300)
c      integer*4 nx,ny
c      parameter (nx = 100,ny = 50)
c      integer*4 nc
c      parameter (nc = 10)
cc
c      real*8 d(0:nx,0:ny),x(0:nx),y(0:ny),z(1:nc)
c      real*8 x1,y1,x2,y2
c      real*8 zmax,zmin
c      integer*4 i,j
cc
cc     Create an artificial data surface and calculate the
cc     surface bounds for choosing the contour levels.
cc
c      zmin =  1.0e30
c      zmax = -1.0e30
c      do 200 i=0,nx
c         do 100 j=0,ny
c            d(i,j) = i * j
c            zmin = min(zmin,d(i,j))
c            zmax = max(zmax,d(i,j))
c100      continue
c200   continue
cc
cc     Set coordinates in Y array suitable for 
cc     automatic plotting on the graphics screen
cc
c      do 300 j=0,ny
c         y(j) = pymax - j * (pymax - pymin) / float(ny)
c300   continue
cc
cc     Set coordinates in X array suitable for
cc     automatic plotting on the graphics screen
cc
c      do 400 i=0,nx
c         x(i) = i * (pxmax - pxmin) / float(nx) + pxmin
c400   continue
cc
cc     Set a full contingent of contour levels
cc
c      do 500 i=1,nc
c         z(i) = i * (zmax - zmin) / (nc + 1)
c500   continue
cc
cc     Draw a border around the contour plot
cc
cc      x1 = pxmin
cc      y1 = pymin
cc      x2 = pxmax
cc      y2 = pymax
cc      call vecout(x1,y1,x1,y2,0.0)
cc      call vecout(x1,y2,x2,y2,0.0)
cc      call vecout(x2,y2,x2,y1,0.0)
cc      call vecout(x2,y1,x1,y1,0.0)
cc
cc     Call the contouring routine
cc
c      call conrec(d,0,nx,0,ny,x,y,nc,z)
cc      pause
cc
c      end
c
c======================================================================
c
c     CONREC is a contouring subroutine for rectangularily spaced data.
c
c     It emits calls to a line drawing subroutine supplied by the user
c     which draws a contour map corresponding to real*8data on a randomly
c     spaced rectangular grid. The coordinates emitted are in the same
c     units given in the x() and y() arrays.
c
c     Any number of contour levels may be specified but they must be 
c     in order of increasing value.
c
c     subroutine conrec(d,ilb,iub,jlb,jub,x,y,nc,z)
c     real*8 d(ilb:iub,jlb:jub)  ! matrix of data to contour
c     integer ilb,iub,jlb,jub    ! index bounds of data matrix
c     real*8 x(ilb:iub)          ! data matrix column coordinates
c     real*8 y(jlb,jub)          ! data matrix row coordinates
c     integer nc                 ! number of contour levels
c     real*8 z(1:nc)             ! contour levels in increasing order
c
      subroutine conrec(d,ilb,iub,jlb,jub,x,y,nc,z)
      real*8 d(ilb:iub,jlb:jub)
      integer ilb,iub,jlb,jub
      real*8 x(ilb:iub)
      real*8 y(jlb:jub)
      integer nc
      real*8 z(1:nc)
c
c     Local declarations
c
      real*8 h(0:4)
      integer sh(0:4)
      real*8 xh(0:4),yh(0:4)
      integer im(1:4),jm(1:4)
      integer case
      integer castab(-1:1,-1:1,-1:1)
      integer p1,p2
      real*8 xsect, ysect, x1, y1, x2, y2
c
c     Data
c
      data im/0,1,1,0/
      data jm/0,0,1,1/
      data castab/0,0,9,  0,1,5,  7,4,8,
     1            0,3,6,  2,3,2,  6,3,0,
     2            8,4,7,  5,1,0,  9,0,0/
c
c     Use statement functions for the line intersections
c
      xsect(p1,p2) = (h(p2)*xh(p1)-h(p1)*xh(p2))/(h(p2)-h(p1))
      ysect(p1,p2) = (h(p2)*yh(p1)-h(p1)*yh(p2))/(h(p2)-h(p1))
c
c     Scan the arrays, top down, left to right within rows
c
20    do 100 j=jub-1,jlb,-1
         do 90 i=ilb,iub-1
            dmin = min(d(i,j),d(i,j+1),d(i+1,j),d(i+1,j+1))
            dmax = max(d(i,j),d(i,j+1),d(i+1,j),d(i+1,j+1))
            if (dmax.ge.z(1) .and. dmin.le.z(nc)) then
               do 80 k=1,nc
                  if (z(k).ge.dmin .and. z(k).le.dmax) then
                     do 22 m=4,0,-1
                        if (m.gt.0) then
                           h(m)=d(i+im(m),j+jm(m))-z(k)
                           xh(m)=x(i+im(m))
                           yh(m)=y(j+jm(m))
                        else
                           h(0)=0.25d0*(h(1)+h(2)+h(3)+h(4))
                           xh(0)=0.5d0*(x(i)+x(i+1))
                           yh(0)=0.5d0*(y(j)+y(j+1))
                        endif
                        if (h(m).gt.0.0d0) then
                           sh(m)=+1
                        else if (h(m).lt.0.0d0) then
                           sh(m)=-1
                        else
                           sh(m)=0
                        endif
22                   continue
c
c     Note: at this stage the relative heights of the corners and the
c     centre are in the h array, and the corresponding coordinates are
c     in the xh and yh arrays. The centre of the box is indexed by 0
c     and the 4 corners by 1 to 4 as shown below.
c     Each triangle is then indexed by the parameter m, and the 3
c     vertices of each triangle are indexed by parameters m1,m2,and m3.
c     It is assumed that the centre of the box is always vertex 2 though
c     this isimportant only when all 3 vertices lie exactly on the same
c     contour level, in which case only the side of the box is drawn.
c
c
c           vertex 4 +-------------------+ vertex 3
c                    | \               / |
c                    |   \    m-3    /   |
c                    |     \       /     |
c                    |       \   /       |
c                    |  m=2    X   m=2   |       the centre is vertex 0
c                    |       /   \       |
c                    |     /       \     |
c                    |   /    m=1    \   |
c                    | /               \ |
c           vertex 1 +-------------------+ vertex 2
c
c
c
c                    Scan each triangle in the box
c
                     do 60 m=1,4
                        m1=m
                        m2=0
                        if (m.ne.4) then
                           m3=m+1
                        else
                           m3=1
                        endif
                        case = castab(sh(m1),sh(m2),sh(m3))
                        if (case.ne.0) then
                           goto (31,32,33,34,35,36,37,38,39),case
c
c     Case 1 - Line between vertices 1 and 2
c
31                            x1=xh(m1)
                              y1=yh(m1)
                              x2=xh(m2)
                              y2=yh(m2)
                              goto 40
c
c     Case 2 - Line between vertices 2 and 3
c
32                            x1=xh(m2)
                              y1=yh(m2)
                              x2=xh(m3)
                              y2=yh(m3)
                              goto 40
c
c     Case 3 - Line between vertices 3 and 1
c
33                            x1=xh(m3)
                              y1=yh(m3)
                              x2=xh(m1)
                              y2=yh(m1)
                              goto 40
c
c     Case 4 - Line between vertex 1 and side 2-3
c
34                            x1=xh(m1)
                              y1=yh(m1)
                              x2=xsect(m2,m3)
                              y2=ysect(m2,m3)
                              goto 40
c
c     Case 5 - Line between vertex 2 and side 3-1
c
35                            x1=xh(m2)
                              y1=yh(m2)
                              x2=xsect(m3,m1)
                              y2=ysect(m3,m1)
                              goto 40
c
c     Case 6 - Line between vertex 3 and side 1-2
c
36                            x1=xh(m3)
                              y1=yh(m3)
                              x2=xsect(m1,m2)
                              y2=ysect(m1,m2)
                              goto 40
c
c     Case 7 - Line between sides 1-2 and 2-3
c
37                            x1=xsect(m1,m2)
                              y1=ysect(m1,m2)
                              x2=xsect(m2,m3)
                              y2=ysect(m2,m3)
                              goto 40
c
c     Case 8 - Line between sides 2-3 and 3-1
c
38                            x1=xsect(m2,m3)
                              y1=ysect(m2,m3)
                              x2=xsect(m3,m1)
                              y2=ysect(m3,m1)
                              goto 40
c
c     Case 9 - Line between sides 3-1 and 1-2
c
39                            x1=xsect(m3,m1)
                              y1=ysect(m3,m1)
                              x2=xsect(m1,m2)
                              y2=ysect(m1,m2)
                              goto 40
40                         call vecout(x1,y1,x2,y2,z(k))
                        endif
60                   continue
                  endif
80             continue
            endif
90       continue
100   continue
      return
      end
c
c======================================================================
c
c     This is a sample vector output routine. For a local environment
c     either replace the VECOUT call in the main line, or better
c     replace the contents of this subroutine between the *'s shown.
c
c     There is often the requirement to distinguish each contour
c     line with a different colour or a different line style. This
c     can be done in many ways using the contour values z for a
c     particular line segment.
c
      subroutine vecout(x1,y1,x2,y2,z)
      implicit none
      real*8 x1,y1,x2,y2,z
      integer n
      logical state
c
c***** Replace from here
c
c     The following should be ignored since it is specific to
c     the version of FORTRAN running on the Macintosh microcomputer
c     on which this particular example was written.
c
c      INTEGER LINETO
c      PARAMETER (LINETO=Z'89109000')
c      INTEGER MOVETO
c      PARAMETER (MOVETO=Z'89309000')
c      call toolbx(MOVETO,nint(x1),nint(y1))
c      call toolbx(LINETO,nint(x2),nint(y2))
       
       inquire(file='cntr_GEM_V.dat', exist=state)
       if (state) then
        open(55, file='cntr_GEM_V.dat', status='old', position='append')
       else
        open(55, file='cntr_GEM_V.dat', status='new', action='write')
       end if
       
       write(55,*) x1,y1,x2,y2,z 
c
c***** To here
c
      return
      end
