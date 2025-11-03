       module mod_mit_gcm
! $header: /u/gcmpack/mitgcm/model/inc/size.h,v 1.28 2009/05/17 21:15:07 jmc exp $
! $name: checkpoint63a $


!
!bop
!    !routine: size.h
!    !interface:
!    include size.h
!    !description: \bv
!     *==========================================================*
!     | size.h declare size of underlying computational grid.     
!     *==========================================================*
!     | the design here support a three-dimensional model grid    
!     | with indices i,j and k. the three-dimensional domain      
!     | is comprised of npx*nsx blocks of size snx along one axis 
!     | npy*nsy blocks of size sny along another axis and one     
!     | block of size nz along the final axis.                    
!     | blocks have overlap regions of size olx and oly along the 
!     | dimensions that are subdivided.                           
!     *==========================================================*
!     \ev
!eop
!     voodoo numbers controlling data layout.
!     snx :: no. x points in sub-grid.
!     sny :: no. y points in sub-grid.
!     olx :: overlap extent in x.
!     oly :: overlat extent in y.
!     nsx :: no. sub-grids in x.
!     nsy :: no. sub-grids in y.
!     npx :: no. of processes to use in x.
!     npy :: no. of processes to use in y.
!     nx  :: no. points in x for the total domain.
!     ny  :: no. points in y for the total domain.
!     nr  :: no. points in z for full process domain.
      integer snx
      integer sny
      integer olx
      integer oly
      integer nsx
      integer nsy
      integer npx
      integer npy
      integer nx
      integer ny
      integer nr
      parameter ( &
                snx =  30, &
                sny =  8, &
                olx =   4, &
                oly =   4, &
                nsx =   1, &
                nsy =   1, &
                npx =   19, &
                npy =   33, &
                nx  = snx*nsx*npx, &
                ny  = sny*nsy*npy, &
                nr  =  75)

!     max_olx :: set to the maximum overlap region size of any array
!     max_oly    that will be exchanged. controls the sizing of exch
!                routine buffers.
      integer max_olx
      integer max_oly
      parameter ( max_olx = olx, &
                 max_oly = oly )
!
!bop
!    !routine: grid.h
!    !interface:
!    include grid.h
!    !description: \bv
!     *==========================================================*
!     | grid.h
!     | o header file defining model grid.
!     *==========================================================*
!     | model grid is defined for each process by reference to
!     | the arrays set here.
!     | notes
!     | =====
!     | the standard mitgcm convention of westmost, southern most
!     | and upper most having the (1,1,1) index is used here.
!     | i.e.
!     |----------------------------------------------------------
!     | (1)  plan view schematic of model grid (top layer i.e. )
!     |      ================================= ( ocean surface )
!     |                                        ( or top of     )
!     |                                        ( atmosphere    )
!     |      this diagram shows the location of the model
!     |      prognostic variables on the model grid. the "t"
!     |      location is used for all tracers. the figure also
!     |      shows the southern most, western most indexing
!     |      convention that is used for all model variables.
!     |
!     |
!     |             v(i=1,                     v(i=nx,
!     |               j=ny+1,                    j=ny+1,
!     |               k=1)                       k=1)
!     |                /|\                       /|\  "pwx"
!     |       |---------|------------------etc..  |---- *---
!     |       |                     |                   *  |
!     |"pwy"*******************************etc..  **********"pwy"
!     |       |                     |                   *  |
!     |       |                     |                   *  |
!     |       |                     |                   *  |
!     |u(i=1, ==>       x           |             x     *==>u
!     |  j=ny,|      t(i=1,         |          t(i=nx,  *(i=nx+1,
!     |  k=1) |        j=ny,        |            j=ny,  *  |j=ny,
!     |       |        k=1)         |            k=1)   *  |k=1)
!     |
!     |       .                     .                      .
!     |       .                     .                      .
!     |       .                     .                      .
!     |       e                     e                   *  e
!     |       t                     t                   *  t
!     |       c                     c                   *  c
!     |       |                     |                   *  |
!     |       |                     |                   *  |
!     |u(i=1, ==>       x           |             x     *  |
!     |  j=2, |      t(i=1,         |          t(i=nx,  *  |
!     |  k=1) |        j=2,         |            j=2,   *  |
!     |       |        k=1)         |            k=1)   *  |
!     |       |                     |                   *  |
!     |       |        /|\          |            /|\    *  |
!     |      -----------|------------------etc..  |-----*---
!     |       |       v(i=1,        |           v(i=nx, *  |
!     |       |         j=2,        |             j=2,  *  |
!     |       |         k=1)        |             k=1)  *  |
!     |       |                     |                   *  |
!     |u(i=1, ==>       x         ==>u(i=2,       x     *==>u
!     |  j=1, |      t(i=1,         |  j=1,    t(i=nx,  *(i=nx+1,
!     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
!     |       |        k=1)         |            k=1)   *  |k=1)
!     |       |                     |                   *  |
!     |       |        /|\          |            /|\    *  |
!     |"sb"++>|---------|------------------etc..  |-----*---
!     |      /+\      v(i=1,                    v(i=nx, *
!     |       +         j=1,                      j=1,  *
!     |       +         k=1)                      k=1)  *
!     |     "wb"                                      "pwx"
!     |
!     |   n, y increasing northwards
!     |  /|\ j increasing northwards
!     |   |
!     |   |
!     |   ======>e, x increasing eastwards
!     |             i increasing eastwards
!     |
!     |    i: east-west index
!     |    j: north-south index
!     |    k: up-down index
!     |    u: x-velocity (m/s)
!     |    v: y-velocity (m/s)
!     |    t: potential temperature (oc)
!     | "sb": southern boundary
!     | "wb": western boundary
!     |"pwx": periodic wrap around in x.
!     |"pwy": periodic wrap around in y.
!     |----------------------------------------------------------
!     | (2) south elevation schematic of model grid
!     |     =======================================
!     |     this diagram shows the location of the model
!     |     prognostic variables on the model grid. the "t"
!     |     location is used for all tracers. the figure also
!     |     shows the upper most, western most indexing
!     |     convention that is used for all model variables.
!     |
!     |      "wb"
!     |       +
!     |       +
!     |      \+/       /|\                       /|\       .
!     |"ub"++>|-------- | -----------------etc..  | ----*---
!     |       |    rvel(i=1,        |        rvel(i=nx, *  |
!     |       |         j=1,        |             j=1,  *  |
!     |       |         k=1)        |             k=1)  *  |
!     |       |                     |                   *  |
!     |u(i=1, ==>       x         ==>u(i=2,       x     *==>u
!     |  j=1, |      t(i=1,         |  j=1,    t(i=nx,  *(i=nx+1,
!     |  k=1) |        j=1,         |  k=1)      j=1,   *  |j=1,
!     |       |        k=1)         |            k=1)   *  |k=1)
!     |       |                     |                   *  |
!     |       |        /|\          |            /|\    *  |
!     |       |-------- | -----------------etc..  | ----*---
!     |       |    rvel(i=1,        |        rvel(i=nx, *  |
!     |       |         j=1,        |             j=1,  *  |
!     |       |         k=2)        |             k=2)  *  |
!     |
!     |       .                     .                      .
!     |       .                     .                      .
!     |       .                     .                      .
!     |       e                     e                   *  e
!     |       t                     t                   *  t
!     |       c                     c                   *  c
!     |       |                     |                   *  |
!     |       |                     |                   *  |
!     |       |                     |                   *  |
!     |       |                     |                   *  |
!     |       |        /|\          |            /|\    *  |
!     |       |-------- | -----------------etc..  | ----*---
!     |       |    rvel(i=1,        |        rvel(i=nx, *  |
!     |       |         j=1,        |             j=1,  *  |
!     |       |         k=nr)       |             k=nr) *  |
!     |u(i=1, ==>       x         ==>u(i=2,       x     *==>u
!     |  j=1, |      t(i=1,         |  j=1,    t(i=nx,  *(i=nx+1,
!     |  k=nr)|        j=1,         |  k=nr)     j=1,   *  |j=1,
!     |       |        k=nr)        |            k=nr)  *  |k=nr)
!     |       |                     |                   *  |
!     |"lb"++>==============================================
!     |                                               "pwx"
!     |
!     | up   increasing upwards.
!     |/|\                                                       .
!     | |
!     | |
!     | =====> e  i increasing eastwards
!     | |         x increasing eastwards
!     | |
!     |\|/
!     | down,k increasing downwards.
!     |
!     | note: r => height (m) => r increases upwards
!     |       r => pressure (pa) => r increases downwards
!     |
!     |
!     |    i: east-west index
!     |    j: north-south index
!     |    k: up-down index
!     |    u: x-velocity (m/s)
!     | rvel: z-velocity ( units of r )
!     |       the vertical velocity variable rvel is in units of
!     |       "r" the vertical coordinate. r in m will give
!     |       rvel m/s. r in pa will give rvel pa/s.
!     |    t: potential temperature (oc)
!     | "ub": upper boundary.
!     | "lb": lower boundary (always solid - therefore om|w == 0)
!     | "wb": western boundary
!     |"pwx": periodic wrap around in x.
!     |----------------------------------------------------------
!     | (3) views showing nomenclature and indexing
!     |     for grid descriptor variables.
!     |
!     |      fig 3a. shows the orientation, indexing and
!     |      notation for the grid spacing terms used internally
!     |      for the evaluation of gradient and averaging terms.
!     |      these varaibles are set based on the model input
!     |      parameters which define the model grid in terms of
!     |      spacing in x, y and z.
!     |
!     |      fig 3b. shows the orientation, indexing and
!     |      notation for the variables that are used to define
!     |      the model grid. these varaibles are set directly
!     |      from the model input.
!     |
!     | figure 3a
!     | =========
!     |       |------------------------------------
!     |       |                       |
!     |"pwy"********************************* etc...
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |
!     |       .                       .
!     |       .                       .
!     |       .                       .
!     |       e                       e
!     |       t                       t
!     |       c                       c
!     |       |-----------v-----------|-----------v----------|-
!     |       |                       |                      |
!     |       |                       |                      |
!     |       |                       |                      |
!     |       |                       |                      |
!     |       |                       |                      |
!     |       u<--dxf(i=1,j=2,k=1)--->u           t          |
!     |       |/|\       /|\          |                      |
!     |       | |         |           |                      |
!     |       | |         |           |                      |
!     |       | |         |           |                      |
!     |       |dyu(i=1,  dyc(i=1,     |                      |
!     | ---  ---|--j=2,---|--j=2,-----------------v----------|-
!     | /|\   | |  k=1)   |  k=1)     |          /|\         |
!     |  |    | |         |           |          dyf(i=2,    |
!     |  |    | |         |           |           |  j=1,    |
!     |dyg(   |\|/       \|/          |           |  k=1)    |
!     |   i=1,u---        t<---dxc(i=2,j=1,k=1)-->t          |
!     |   j=1,|                       |           |          |
!     |   k=1)|                       |           |          |
!     |  |    |                       |           |          |
!     |  |    |                       |           |          |
!     | \|/   |           |<---dxv(i=2,j=1,k=1)--\|/         |
!     |"sb"++>|___________v___________|___________v__________|_
!     |       <--dxg(i=1,j=1,k=1)----->
!     |      /+\                                              .
!     |       +
!     |       +
!     |     "wb"
!     |
!     |   n, y increasing northwards
!     |  /|\ j increasing northwards
!     |   |
!     |   |
!     |   ======>e, x increasing eastwards
!     |             i increasing eastwards
!     |
!     |    i: east-west index
!     |    j: north-south index
!     |    k: up-down index
!     |    u: x-velocity point
!     |    v: y-velocity point
!     |    t: tracer point
!     | "sb": southern boundary
!     | "wb": western boundary
!     |"pwx": periodic wrap around in x.
!     |"pwy": periodic wrap around in y.
!     |
!     | figure 3b
!     | =========
!     |
!     |       .                       .
!     |       .                       .
!     |       .                       .
!     |       e                       e
!     |       t                       t
!     |       c                       c
!     |       |-----------v-----------|-----------v--etc...
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       u<--delx(i=1)---------->u           t
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |                       |
!     |       |-----------v-----------------------v--etc...
!     |       |          /|\          |
!     |       |           |           |
!     |       |           |           |
!     |       |           |           |
!     |       u        dely(j=1)      |           t
!     |       |           |           |
!     |       |           |           |
!     |       |           |           |
!     |       |           |           |
!     |       |          \|/          |
!     |"sb"++>|___________v___________|___________v__etc...
!     |      /+\                                                 .
!     |       +
!     |       +
!     |     "wb"
!     |
!     *==========================================================*
!     \ev
!eop

!     macros that override/modify standard definitions
!
!bop
!    !routine: grid_macros.h
!    !interface:
!    include grid_macros.h
!    !description: \bv
!     *==========================================================*
!     | grid_macros.h
!     *==========================================================*
!     | these macros are used to substitute definitions for
!     | grid.h variables for particular configurations.
!     | in setting these variables the following convention
!     | applies.
!     | undef  phi_const   - indicates the variable phi is fixed
!     |                      in x, y and z.
!     | undef  phi_fx      - indicates the variable phi only
!     |                      varies in x (i.e.not in x or z).
!     | undef  phi_fy      - indicates the variable phi only
!     |                      varies in y (i.e.not in x or z).
!     | undef  phi_fxy     - indicates the variable phi only
!     |                      varies in x and y ( i.e. not z).
!     *==========================================================*
!     \ev
!eop

!
!bop
!    !routine: dxc_macros.h
!    !interface:
!    include dxc_macros.h
!    !description: \bv
!     *==========================================================*
!     | dxc_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dxf_macros.h
!    !interface:
!    include dxf_macros.h
!    !description: \bv
!     *==========================================================*
!     | dxf_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dxg_macros.h
!    !interface:
!    include dxg_macros.h
!    !description: \bv
!     *==========================================================*
!     | dxg_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dxv_macros.h
!    !interface:
!    include dxv_macros.h
!    !description: \bv
!     *==========================================================*
!     | dxv_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dyc_macros.h
!    !interface:
!    include dyc_macros.h
!    !description: \bv
!     *==========================================================*
!     | dyc_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dyf_macros.h
!    !interface:
!    include dyf_macros.h
!    !description: \bv
!     *==========================================================*
!     | dyf_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dyg_macros.h
!    !interface:
!    include dyg_macros.h
!    !description: \bv
!     *==========================================================*
!     | dyg_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: dyu_macros.h
!    !interface:
!    include dyu_macros.h
!    !description: \bv
!     *==========================================================*
!     | dyu_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: hfacc_macros.h
!    !interface:
!    include hfacc_macros.h
!    !description: \bv
!     *==========================================================*
!     | hfacc_macros.h                                            
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop







!
!bop
!    !routine: hfacs_macros.h
!    !interface:
!    include hfacs_macros.h
!    !description: \bv
!     *==========================================================*
!     | hfacs_macros.h                                            
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop







!
!bop
!    !routine: hfacw_macros.h
!    !interface:
!    include hfacw_macros.h
!    !description: \bv
!     *==========================================================*
!     | hfacw_macros.h                                            
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop







!
!bop
!    !routine: recip_dxc_macros.h
!    !interface:
!    include recip_dxc_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dxc_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dxf_macros.h
!    !interface:
!    include recip_dxf_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dxf_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dxg_macros.h
!    !interface:
!    include recip_dxg_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dxg_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dxv_macros.h
!    !interface:
!    include recip_dxv_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dxv_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dyc_macros.h
!    !interface:
!    include recip_dyc_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dyc_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dyf_macros.h
!    !interface:
!    include recip_dyf_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dyf_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dyg_macros.h
!    !interface:
!    include recip_dyg_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dyg_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_dyu_macros.h
!    !interface:
!    include recip_dyu_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_dyu_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: recip_hfacc_macros.h
!    !interface:
!    include recip_hfacc_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_hfacc_macros.h                                      
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop







!
!bop
!    !routine: recip_hfacs_macros.h
!    !interface:
!    include recip_hfacs_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_hfacs_macros.h                                      
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop







!
!bop
!    !routine: recip_hfacw_macros.h
!    !interface:
!    include recip_hfacw_macros.h
!    !description: \bv
!     *==========================================================*
!     | recip_hfacw_macros.h                                      
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop







!
!bop
!    !routine: xc_macros.h
!    !interface:
!    include xc_macros.h
!    !description: \bv
!     *==========================================================*
!     | xc_macros.h                                               
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: yc_macros.h
!    !interface:
!    include yc_macros.h
!    !description: \bv
!     *==========================================================*
!     | yc_macros.h                                               
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: ra_macros.h
!    !interface:
!    include ra_macros.h
!    !description: \bv
!     *==========================================================*
!     | ra_macros.h                                               
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop




!
!bop
!    !routine: raw_macros.h
!    !interface:
!    include raw_macros.h
!    !description: \bv
!     *==========================================================*
!     | raw_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop




!
!bop
!    !routine: ras_macros.h
!    !interface:
!    include ras_macros.h
!    !description: \bv
!     *==========================================================*
!     | ras_macros.h                                              
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: maskw_macros.h
!    !interface:
!    include maskw_macros.h
!    !description: \bv
!     *==========================================================*
!     | maskw_macros.h                                            
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop






!
!bop
!    !routine: masks_macros.h
!    !interface:
!    include masks_macros.h
!    !description: \bv
!     *==========================================================*
!     | masks_macros.h                                            
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop






!
!bop
!    !routine: tanphiatu_macros.h
!    !interface:
!    include tanphiatu_macros.h
!    !description: \bv
!     *==========================================================*
!     | tanphiatu_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: tanphiatv_macros.h
!    !interface:
!    include tanphiatv_macros.h
!    !description: \bv
!     *==========================================================*
!     | tanphiatv_macros.h                                        
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!
!bop
!    !routine: fcori_macros.h
!    !interface:
!    include fcori_macros.h
!    !description: \bv
!     *==========================================================*
!     | fcori_macros.h                                            
!     *==========================================================*
!     | these macros are used to reduce memory requirement and/or 
!     | memory references when variables are fixed along a given  
!     | axis or axes.                                             
!     *==========================================================*
!     \ev
!eop





!--   common /grid_rl/ rl valued grid defining variables.
!     deepfacc  :: deep-model grid factor (fct of vertical only) for dx,dy
!     deepfacf     at level-center (deepfacc)  and level interface (deepfacf)
!     deepfac2c :: deep-model grid factor (fct of vertical only) for area dx*dy
!     deepfac2f    at level-center (deepfac2c) and level interface (deepfac2f)
!     gravitysign :: indicates the direction of gravity relative to r direction
!                   (= -1 for r=z (z increases upward, -gravity direction  )
!                   (= +1 for r=p (p increases downward, +gravity direction)
!     rksign     :: vertical coordinate to vertical index orientation.
!                   ( +1 same orientation, -1 opposite orientation )
!     globalarea :: domain integrated horizontal area [m2]
      common /grid_rl/ &
       cosfacu, cosfacv, sqcosfacu, sqcosfacv, &
       deepfacc, deepfac2c, recip_deepfacc, recip_deepfac2c, &
       deepfacf, deepfac2f, recip_deepfacf, recip_deepfac2f, &
       gravitysign, rksign, globalarea
      real*8 cosfacu        (1-oly:sny+oly,nsx,nsy)
      real*8 cosfacv        (1-oly:sny+oly,nsx,nsy)
      real*8 sqcosfacu      (1-oly:sny+oly,nsx,nsy)
      real*8 sqcosfacv      (1-oly:sny+oly,nsx,nsy)
      real*8 deepfacc       (nr)
      real*8 deepfac2c      (nr)
      real*8 deepfacf       (nr+1)
      real*8 deepfac2f      (nr+1)
      real*8 recip_deepfacc (nr)
      real*8 recip_deepfac2c(nr)
      real*8 recip_deepfacf (nr+1)
      real*8 recip_deepfac2f(nr+1)
      real*8 gravitysign
      real*8 rksign
      real*8 globalarea

!--   common /grid_rs/ rs valued grid defining variables.
!     dxc     :: cell center separation in x across western cell wall (m)
!     dxg     :: cell face separation in x along southern cell wall (m)
!     dxf     :: cell face separation in x thru cell center (m)
!     dxv     :: v-point separation in x across south-west corner of cell (m)
!     dyc     :: cell center separation in y across southern cell wall (m)
!     dyg     :: cell face separation in y along western cell wall (m)
!     dyf     :: cell face separation in y thru cell center (m)
!     dyu     :: u-point separation in y across south-west corner of cell (m)
!     drc     :: cell center separation along z axis ( units of r ).
!     drf     :: cell face separation along z axis ( units of r ).
!     r_low   :: base of fluid in r_unit (depth(m) / pressure(pa) at top atmos.)
!     rloww   :: base of fluid column in r_unit at western  edge location.
!     rlows   :: base of fluid column in r_unit at southern edge location.
!     ro_surf :: surface reference (at rest) position, r_unit.
!     rsurfw  :: surface reference position at western  edge location [r_unit].
!     rsurfs  :: surface reference position at southern edge location [r_unit].
!     hfac    :: fraction of cell in vertical which is open i.e how
!              "lopped" a cell is (dimensionless scale factor).
!              note: the code needs terms like min(hfac,hfac(i-1))
!                    on some platforms it may be better to precompute
!                    hfacw, hfacs, ... here than do min on the fly.
!     maskinc :: cell center 2-d interior mask (i.e., zero beyond ob)
!     maskinw :: west  face 2-d interior mask (i.e., zero on and beyond ob)
!     maskins :: south face 2-d interior mask (i.e., zero on and beyond ob)
!     maskc   :: cell center land mask
!     maskw   :: west face land mask
!     masks   :: south face land mask
!     recip_dxc   :: reciprocal of dxc
!     recip_dxg   :: reciprocal of dxg
!     recip_dxf   :: reciprocal of dxf
!     recip_dxv   :: reciprocal of dxv
!     recip_dyc   :: reciprocal of dxc
!     recip_dyg   :: reciprocal of dyg
!     recip_dyf   :: reciprocal of dyf
!     recip_dyu   :: reciprocal of dyu
!     recip_drc   :: reciprocal of drc
!     recip_drf   :: reciprocal of drf
!     recip_rcol  :: inverse of cell center column thickness (1/r_unit)
!     recip_hfacc :: inverse of cell open-depth f[x,y,z] ( dimensionless ).
!     recip_hfacw    rhfacc center, rhfacw west, rhfacs south.
!     recip_hfacs    note: this is precomputed here because it involves division.
!     xc        :: x-coordinate of cell center f[x,y]. the units of xc, yc
!                  depend on the grid. they are not used in differencing or
!                  averaging but are just a convient quantity for i/o,
!                  diagnostics etc.. as such xc is in m for cartesian
!                  coordinates but degrees for spherical polar.
!     yc        :: y-coordinate of center of cell f[x,y].
!     yg        :: y-coordinate of corner of cell ( c-grid vorticity point) f[x,y].
!     ra        :: r-face are f[x,y] ( m^2 ).
!                  note: in a cartesian framework ra is simply dx*dy,
!                      however we use ra to allow for non-globally
!                      orthogonal coordinate frames (with appropriate
!                      metric terms).
!     rc        :: r-coordinate of center of cell f[z] (units of r).
!     rf        :: r-coordinate of face of cell f[z] (units of r).
! - *hybsigm* - :: hybrid-sigma vert. coord coefficients
!     ahybsigmf    at level-interface (*hybsigmf) and level-center (*hybsigmc)
!     ahybsigmc    ahybsigm* = constant r part, bhybsigm* = sigma part, such as
!     bhybsigmf    r(ij,k,t) = rlow(ij) + ahybsigm(k)*[rf(1)-rf(nr+1)]
!     bhybsigmc              + bhybsigm(k)*[eta(ij,t)+ro_surf(ij) - rlow(ij)]
!     dahybsigf :: vertical increment of hybrid-sigma coefficient: constant r part,
!     dahybsigc    between interface (dahybsigf) and between center (dahybsigc)
!     dbhybsigf :: vertical increment of hybrid-sigma coefficient: sigma part,
!     dbhybsigc    between interface (dbhybsigf) and between center (dbhybsigc)
!     tanphiatu :: tan of the latitude at u point. used for spherical polar
!                  metric term in u equation.
!     tanphiatv :: tan of the latitude at v point. used for spherical polar
!                  metric term in v equation.
!     anglecosc :: cosine of grid orientation angle relative to geographic direction
!               at cell center: alpha=(eastward_dir,grid_uvel_dir)=(north_d,vvel_d)
!     anglesinc :: sine   of grid orientation angle relative to geographic direction
!               at cell center: alpha=(eastward_dir,grid_uvel_dir)=(north_d,vvel_d)
!     u2zondir  :: cosine of grid orientation angle at u point location
!     v2zondir  :: minus sine of  orientation angle at v point location
!     fcori     :: coriolis parameter at grid center point
!     fcorig    :: coriolis parameter at grid corner point
!     fcoricos  :: coriolis cos(phi) parameter at grid center point (for nh)

      common /grid_rs/ &
       dxc,dxf,dxg,dxv,dyc,dyf,dyg,dyu, &
       r_low, rloww, rlows, &
       ro_surf, rsurfw, rsurfs, &
       hfacc, hfacw, hfacs, &
       recip_dxc,recip_dxf,recip_dxg,recip_dxv, &
       recip_dyc,recip_dyf,recip_dyg,recip_dyu, &
       recip_rcol, &
       recip_hfacc,recip_hfacw,recip_hfacs, &
       xc,yc,ra,raw,ras,raz,xg,yg, &
       maskinc, maskinw, maskins, &
       maskc, maskw, masks, &
       recip_ra,recip_raw,recip_ras,recip_raz, &
       drc, drf, recip_drc, recip_drf, rc, rf, &
       ahybsigmf, bhybsigmf, ahybsigmc, bhybsigmc, &
       dahybsigf, dbhybsigf, dbhybsigc, dahybsigc, &
       tanphiatu, tanphiatv, &
       anglecosc, anglesinc, u2zondir, v2zondir, &
       fcori, fcorig, fcoricos
      real*8 dxc            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dxf            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dxg            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dxv            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dyc            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dyf            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dyg            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 dyu            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 r_low          (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 rloww          (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 rlows          (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 ro_surf        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 rsurfw         (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 rsurfs         (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 hfacc          (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 hfacw          (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 hfacs          (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 recip_dxc      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dxf      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dxg      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dxv      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dyc      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dyf      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dyg      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_dyu      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_rcol     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_hfacc    (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 recip_hfacw    (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 recip_hfacs    (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 xc             (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 xg             (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 yc             (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 yg             (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 ra             (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 raw            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 ras            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 raz            (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_ra       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_raw      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_ras      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 recip_raz      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 maskinc        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 maskinw        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 maskins        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 maskc          (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 maskw          (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 masks          (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8 drc            (nr+1)
      real*8 drf            (nr)
      real*8 recip_drc      (nr+1)
      real*8 recip_drf      (nr)
      real*8 rc             (nr)
      real*8 rf             (nr+1)
      real*8 ahybsigmf      (nr+1)
      real*8 bhybsigmf      (nr+1)
      real*8 ahybsigmc      (nr)
      real*8 bhybsigmc      (nr)
      real*8 dahybsigf      (nr)
      real*8 dbhybsigf      (nr)
      real*8 dbhybsigc      (nr+1)
      real*8 dahybsigc      (nr+1)
      real*8 tanphiatu      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 tanphiatv      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 anglecosc      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 anglesinc      (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 u2zondir       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 v2zondir       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 fcori          (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 fcorig         (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 fcoricos       (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)


!--   common /grid_i/ integer valued grid defining variables.
!     ksurfc  :: vertical index of the surface tracer cell
!     ksurfw  :: vertical index of the surface u point
!     ksurfs  :: vertical index of the surface v point
!     klowc   :: index of the r-lowest "wet cell" (2d)
! important: klowc = 0 and ksurfc,w,s = nr+1 (or =nr+2 on a thin-wall)
!            where the fluid column is empty (continent)
      common /grid_i/ &
       ksurfc, ksurfw, ksurfs, &
       klowc
      integer ksurfc(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer ksurfw(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer ksurfs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      integer klowc (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

!gmtide(
      common /grid_eqtide/ &
       lonrad,lon2rad,latrad,lat2rad,eqtd,eqtide_msk

      real*8     lonrad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8    lon2rad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8     latrad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8    lat2rad(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8       eqtd(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 eqtide_msk(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

!gmtide)

!gmriver(
      common /grid_river/ &
       river_msk,river_mskbs,flux_onoff

      real*8 river_msk(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 river_mskbs(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 flux_onoff(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
!gmriver)


!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
!bop
!     !routine: eeparams.h
!     !interface:
!     include "eeparams.h"
!
!     !description:
!     *==========================================================*
!     | eeparams.h                                               |
!     *==========================================================*
!     | parameters for "execution environemnt". these are used   |
!     | by both the particular numerical model and the execution |
!     | environment support routines.                            |
!     *==========================================================*
!eop

!     ========  eesize.h  ========================================

!     max_len_mbuf  :: default message buffer max. size
!     max_len_fnam  :: default file name max. size
!     max_len_prec  :: default rec len for reading "parameter" files

      integer max_len_mbuf
      parameter ( max_len_mbuf = 512 )
      integer max_len_fnam
      parameter ( max_len_fnam = 512 )
      integer max_len_prec
      parameter ( max_len_prec = 200 )

!     max_no_threads  :: maximum number of threads allowed.
!c    max_no_procs    :: maximum number of processes allowed.
!c    max_no_barriers :: maximum number of distinct thread "barriers"
      integer max_no_threads
      parameter ( max_no_threads =  4 )
!     integer max_no_procs
!     parameter ( max_no_procs   =  70000 )
!     integer max_no_barriers
!     parameter ( max_no_barriers = 1 )

!     particularly weird and obscure voodoo numbers
!     lshare :: this wants to be the length in
!               [148]-byte words of the size of
!               the address "window" that is snooped
!               on an smp bus. by separating elements in
!               the global sum buffer we can avoid generating
!               extraneous invalidate traffic between
!               processors. the length of this window is usually
!               a cache line i.e. small o(64 bytes).
!               the buffer arrays are usually short arrays
!               and are declared real arra(lshare[148],lbuff).
!               setting lshare[148] to 1 is like making these arrays
!               one dimensional.
      integer cachelinesize
      integer lshare1
      integer lshare4
      integer lshare8
      parameter ( cachelinesize = 256 )
      parameter ( lshare1 =  cachelinesize )
      parameter ( lshare4 =  cachelinesize/4 )
      parameter ( lshare8 =  cachelinesize/8 )

!c    max_vgs  :: maximum buffer size for global vector sum
!     integer max_vgs
!     parameter ( max_vgs = 8192 )

!     ========  eesize.h  ========================================

!     symbolic values
!     precxxxx :: precision used for i/o
      integer precfloat32
      parameter ( precfloat32 = 32 )
      integer precfloat64
      parameter ( precfloat64 = 64 )

!     real-type constant for some frequently used simple number (0,1,2,1/2):
      real*8     zerors, oners, twors, halfrs
      parameter ( zerors = 0.0d0 , oners  = 1.0d0 )
      parameter ( twors  = 2.0d0 , halfrs = 0.5d0 )
      real*8     zerorl, onerl, tworl, halfrl
      parameter ( zerorl = 0.0d0 , onerl  = 1.0d0 )
      parameter ( tworl  = 2.0d0 , halfrl = 0.5d0 )

!     unset_xxx :: used to indicate variables that have not been given a value
      real*8  unset_float8
      parameter ( unset_float8 = 1.234567d5 )
      real*4  unset_float4
      parameter ( unset_float4 = 1.234567e5 )
      real*8     unset_rl
      parameter ( unset_rl     = 1.234567d5 )
      real*8     unset_rs
      parameter ( unset_rs     = 1.234567d5 )
      integer unset_i
      parameter ( unset_i      = 123456789  )

!     deblevx  :: used to decide when to print debug messages
      integer deblevzero
      integer debleva, deblevb,  deblevc, deblevd, debleve
      parameter ( deblevzero=0 )
      parameter ( debleva=1 )
      parameter ( deblevb=2 )
      parameter ( deblevc=3 )
      parameter ( deblevd=4 )
      parameter ( debleve=5 )

!     squeeze_right      :: flag indicating right blank space removal
!                           from text field.
!     squeeze_left       :: flag indicating left blank space removal
!                           from text field.
!     squeeze_both       :: flag indicating left and right blank
!                           space removal from text field.
!     print_map_xy       :: flag indicating to plot map as xy slices
!     print_map_xz       :: flag indicating to plot map as xz slices
!     print_map_yz       :: flag indicating to plot map as yz slices
!     commentcharacter   :: variable used in column 1 of parameter
!                           files to indicate comments.
!     index_i            :: variable used to select an index label
!     index_j               for formatted input parameters.
!     index_k
!     index_none
      character*(*) squeeze_right
      parameter ( squeeze_right = 'r' )
      character*(*) squeeze_left
      parameter ( squeeze_left = 'l' )
      character*(*) squeeze_both
      parameter ( squeeze_both = 'b' )
      character*(*) print_map_xy
      parameter ( print_map_xy = 'xy' )
      character*(*) print_map_xz
      parameter ( print_map_xz = 'xz' )
      character*(*) print_map_yz
      parameter ( print_map_yz = 'yz' )
      character*(*) commentcharacter
      parameter ( commentcharacter = '#' )
      integer index_i
      integer index_j
      integer index_k
      integer index_none
      parameter ( index_i    = 1, &
                 index_j    = 2, &
                 index_k    = 3, &
                 index_none = 4 )

!     exch_ignore_corners :: flag to select ignoring or
!     exch_update_corners    updating of corners during an edge exchange.
      integer exch_ignore_corners
      integer exch_update_corners
      parameter ( exch_ignore_corners = 0, &
                 exch_update_corners = 1 )

!     forward_simulation
!     reverse_simulation
!     tangent_simulation
      integer forward_simulation
      integer reverse_simulation
      integer tangent_simulation
      parameter ( forward_simulation = 0, &
                 reverse_simulation = 1, &
                 tangent_simulation = 2 )

!--   common /eeparams_l/ execution environment public logical variables.
!     eebooterror    :: flags indicating error during multi-processing
!     eeenderror     :: initialisation and termination.
!     fatalerror     :: flag used to indicate that the model is ended with an error
!     debugmode      :: controls printing of debug msg (sequence of s/r calls).
!     usesinglecpuio :: when usesinglecpuio is set, mds_write_field outputs from
!                       master mpi process only. -- note: read from main parameter
!                       file "data" and not set until call to ini_parms.
!     usesinglecpuinput :: when usesinglecpuinput is set, exf_interp_read
!                       reads forcing files from master mpi process only.
!                       -- note: read from main parameter file "data"
!                          and defaults to usesinglecpuinput = usesinglecpuio
!     printmapincludeszeros  :: flag that controls whether character constant
!                               map code ignores exact zero values.
!     usecubedsphereexchange :: use cubed-sphere topology domain.
!     usecoupler     :: use coupler for a multi-components set-up.
!     usenest_parent :: use parent nesting interface (pkg/nest_parent)
!     usenest_child  :: use child  nesting interface (pkg/nest_child)
!     usenest2w_parent :: use parent 2-w nesting interface (pkg/nest2w_parent)
!     usenest2w_child  :: use child  2-w nesting interface (pkg/nest2w_child)
!     useoasis       :: use oasis-coupler for a multi-components set-up.
      common /eeparams_l/ &
!      eebooterror, fatalerror, eeenderror, &
       eebooterror, eeenderror, fatalerror, debugmode, &
       usesinglecpuio, usesinglecpuinput, printmapincludeszeros, &
       usecubedsphereexchange, usecoupler, &
       usenest_parent, usenest_child, &
       usenest2w_parent, usenest2w_child, useoasis, &
       usesetrlstk, usesigreg
      logical eebooterror
      logical eeenderror
      logical fatalerror
      logical debugmode
      logical usesinglecpuio
      logical usesinglecpuinput
      logical printmapincludeszeros
      logical usecubedsphereexchange
      logical usecoupler
      logical usenest_parent
      logical usenest_child
      logical usenest2w_parent
      logical usenest2w_child
      logical useoasis
      logical usesetrlstk
      logical usesigreg

!--   common /eparams_i/ execution environment public integer variables.
!     errormessageunit    :: fortran io unit for error messages
!     standardmessageunit :: fortran io unit for informational messages
!     maxlengthprt1d :: maximum length for printing (to std-msg-unit) 1-d array
!     scrunit1      :: scratch file 1 unit number
!     scrunit2      :: scratch file 2 unit number
!     eedataunit    :: unit # for reading "execution environment" parameter file
!     modeldataunit :: unit number for reading "model" parameter file.
!     numberofprocs :: number of processes computing in parallel
!     pidio         :: id of process to use for i/o.
!     mybxlo, mybxhi :: extents of domain in blocks in x and y
!     mybylo, mybyhi :: that each threads is responsble for.
!     myprocid      :: my own "process" id.
!     mypx          :: my x coord on the proc. grid.
!     mypy          :: my y coord on the proc. grid.
!     myxgloballo   :: my bottom-left (south-west) x-index global domain.
!                      the x-coordinate of this point in for example m or
!                      degrees is *not* specified here. a model needs to
!                      provide a mechanism for deducing that information
!                      if it is needed.
!     myygloballo   :: my bottom-left (south-west) y-index in global domain.
!                      the y-coordinate of this point in for example m or
!                      degrees is *not* specified here. a model needs to
!                      provide a mechanism for deducing that information
!                      if it is needed.
!     nthreads      :: no. of threads
!     ntx, nty      :: no. of threads in x and in y
!                      this assumes a simple cartesian gridding of the threads
!                      which is not required elsewhere but that makes it easier
!     ioerrorcount  :: io error counter. set to zero initially and increased
!                      by one every time an io error occurs.
      common /eeparams_i/ &
       errormessageunit, standardmessageunit, maxlengthprt1d, &
       scrunit1, scrunit2, eedataunit, modeldataunit, &
       numberofprocs, pidio, myprocid, &
       mypx, mypy, myxgloballo, myygloballo, nthreads, &
       mybxlo, mybxhi, mybylo, mybyhi, &
       ntx, nty, ioerrorcount
      integer errormessageunit
      integer standardmessageunit
      integer maxlengthprt1d
      integer scrunit1
      integer scrunit2
      integer eedataunit
      integer modeldataunit
      integer ioerrorcount(max_no_threads)
      integer mybxlo(max_no_threads)
      integer mybxhi(max_no_threads)
      integer mybylo(max_no_threads)
      integer mybyhi(max_no_threads)
      integer myprocid
      integer mypx
      integer mypy
      integer myxgloballo
      integer myygloballo
      integer nthreads
      integer ntx
      integer nty
      integer numberofprocs
      integer pidio

!eh3 ;;; local variables: ***
!eh3 ;;; mode:fortran ***
!eh3 ;;; end: ***
!

!bop
!     !routine: params.h
!     !interface:
!     #include params.h

!     !description:
!     header file defining model "parameters".  the values from the
!     model standard input file are stored into the variables held
!     here. notes describing the parameters can also be found here.

!eop

!--   contants
!     useful physical values
      real*8 pi
      parameter ( pi    = 3.14159265358979323844d0   )
      real*8 deg2rad
      parameter ( deg2rad = 2.d0*pi/360.d0           )

!--   common /parm_c/ character valued parameters used by the model.
!     buoyancyrelation :: flag used to indicate which relation to use to
!                         get buoyancy.
!     eostype         :: choose the equation of state:
!                        linear, poly3, unesco, jmd95z, jmd95p, mdjwf, idealgas
!     pickupsuff      :: force to start from pickup files (even if niter0=0)
!                        and read pickup files with this suffix (max 10 char.)
!     mdsiolocaldir   :: read-write tiled file from/to this directory name
!                        (+ 4 digits processor-rank) instead of current dir.
!     adtapedir       :: read-write checkpointing tape files from/to this
!                        directory name instead of current dir. conflicts
!                        mdsiolocaldir, so only one of the two can be set.
!     treffile      :: file containing reference potential temperat.  tref (1.d)
!     sreffile      :: file containing reference salinity/spec.humid. sref (1.d)
!     rhoreffile    :: file containing reference density profile rhoref (1.d)
!     gravityfile   :: file containing gravity vertical profile (1.d)
!     delrfile      :: file containing vertical grid spacing delr  (1.d array)
!     delrcfile     :: file containing vertical grid spacing delrc (1.d array)
!     hybsigmfile   :: file containing hybrid-sigma vertical coord. coeff. (2x 1.d)
!     delxfile      :: file containing x-spacing grid definition (1.d array)
!     delyfile      :: file containing y-spacing grid definition (1.d array)
!     horizgridfile :: file containing horizontal-grid definition
!                        (only when using curvilinear_grid)
!     bathyfile       :: file containing bathymetry. if not defined bathymetry
!                        is taken from inline function.
!     topofile        :: file containing the topography of the surface (unit=m)
!                        (mainly used for the atmosphere = ground height).
!     addwwallfile    :: file containing 2-d additional western  cell-edge wall
!     addswallfile    :: file containing 2-d additional southern cell-edge wall
!                        (e.g., to add "thin-wall" where it is =1)
!     hydrogthetafile :: file containing initial hydrographic data (3-d)
!                        for potential temperature.
!     hydrogsaltfile  :: file containing initial hydrographic data (3-d)
!                        for salinity.
!     diffkrfile      :: file containing 3d specification of vertical diffusivity
!     viscahdfile     :: file containing 3d specification of horizontal viscosity
!     viscahzfile     :: file containing 3d specification of horizontal viscosity
!     visca4dfile     :: file containing 3d specification of horizontal viscosity
!     visca4zfile     :: file containing 3d specification of horizontal viscosity
!     zonalwindfile   :: file containing zonal wind data
!     meridwindfile   :: file containing meridional wind data
!     thetaclimfile   :: file containing surface theta climataology used
!                        in relaxation term -lambda(theta-theta*)
!     saltclimfile    :: file containing surface salt climataology used
!                        in relaxation term -lambda(salt-salt*)
!     surfqfile       :: file containing surface heat flux, excluding sw
!                        (old version, kept for backward compatibility)
!     surfqnetfile    :: file containing surface net heat flux
!     surfqswfile     :: file containing surface shortwave radiation
!     empmrfile       :: file containing surface fresh water flux
!           note: for backward compatibility empmrfile is specified in
!                 m/s when using external_fields_load.f.  it is converted
!                 to kg/m2/s by multiplying by rhoconstfresh.
!     saltfluxfile    :: file containing surface salt flux
!     ploadfile       :: file containing pressure loading
!     addmassfile     :: file containing source/sink of fluid in the interior
!     eddypsixfile    :: file containing zonal eddy streamfunction data
!     eddypsiyfile    :: file containing meridional eddy streamfunction data
!     the_run_name    :: string identifying the name of the model "run"
      common /parm_c/ &
                     buoyancyrelation, eostype, &
                     pickupsuff, mdsiolocaldir, adtapedir, &
                     treffile, sreffile, rhoreffile, gravityfile, &
                     delrfile, delrcfile, hybsigmfile, &
                     delxfile, delyfile, horizgridfile, &
                     bathyfile, topofile, addwwallfile, addswallfile, &
                     viscahdfile, viscahzfile, &
                     visca4dfile, visca4zfile, &
                     hydrogthetafile, hydrogsaltfile, diffkrfile, &
                     zonalwindfile, meridwindfile, thetaclimfile, &
                     saltclimfile, &
                     empmrfile, saltfluxfile, &
                     surfqfile, surfqnetfile, surfqswfile, &
                     lambdathetafile, lambdasaltfile, &
                     uvelinitfile, vvelinitfile, psurfinitfile, &
                     ploadfile, addmassfile, &
                     eddypsixfile, eddypsiyfile, geothermalfile, &
                     the_run_name
      character*(max_len_fnam) buoyancyrelation
      character*(6)  eostype
      character*(10) pickupsuff
      character*(max_len_fnam) mdsiolocaldir
      character*(max_len_fnam) adtapedir
      character*(max_len_fnam) treffile
      character*(max_len_fnam) sreffile
      character*(max_len_fnam) rhoreffile
      character*(max_len_fnam) gravityfile
      character*(max_len_fnam) delrfile
      character*(max_len_fnam) delrcfile
      character*(max_len_fnam) hybsigmfile
      character*(max_len_fnam) delxfile
      character*(max_len_fnam) delyfile
      character*(max_len_fnam) horizgridfile
      character*(max_len_fnam) bathyfile, topofile
      character*(max_len_fnam) addwwallfile, addswallfile
      character*(max_len_fnam) hydrogthetafile, hydrogsaltfile
      character*(max_len_fnam) diffkrfile
      character*(max_len_fnam) viscahdfile
      character*(max_len_fnam) viscahzfile
      character*(max_len_fnam) visca4dfile
      character*(max_len_fnam) visca4zfile
      character*(max_len_fnam) zonalwindfile
      character*(max_len_fnam) meridwindfile
      character*(max_len_fnam) thetaclimfile
      character*(max_len_fnam) saltclimfile
      character*(max_len_fnam) surfqfile
      character*(max_len_fnam) surfqnetfile
      character*(max_len_fnam) surfqswfile
      character*(max_len_fnam) empmrfile
      character*(max_len_fnam) saltfluxfile
      character*(max_len_fnam) uvelinitfile
      character*(max_len_fnam) vvelinitfile
      character*(max_len_fnam) psurfinitfile
      character*(max_len_fnam) ploadfile
      character*(max_len_fnam) addmassfile
      character*(max_len_fnam) eddypsixfile
      character*(max_len_fnam) eddypsiyfile
      character*(max_len_fnam) geothermalfile
      character*(max_len_fnam) lambdathetafile
      character*(max_len_fnam) lambdasaltfile
      character*(max_len_prec/2) the_run_name

!--   common /parm_i/ integer valued parameters used by the model.
!     cg2dmaxiters        :: maximum number of iterations in the
!                            two-dimensional con. grad solver.
!     cg2dchkresfreq      :: frequency with which to check residual
!                            in con. grad solver.
!     cg2dprecondfreq     :: frequency for updating cg2d preconditioner
!                            (non-linear free-surf.)
!     cg2duseminressol    :: =0 : use last-iteration/converged solution
!                            =1 : use solver minimum-residual solution
!     cg3dmaxiters        :: maximum number of iterations in the
!                            three-dimensional con. grad solver.
!     cg3dchkresfreq      :: frequency with which to check residual
!                            in con. grad solver.
!     printresidualfreq   :: frequency for printing residual in cg iterations
!     niter0              :: start time-step number of for this run
!     ntimesteps          :: number of timesteps to execute
!     ntimesteps_l2       :: number of inner timesteps to execute per timestep
!     selectcorimap       :: select setting of coriolis parameter map:
!                           =0 f-plane (constant coriolis, = f0)
!                           =1 beta-plane coriolis (= f0 + beta.y)
!                           =2 spherical coriolis (= 2.omega.sin(phi))
!                           =3 read coriolis 2-d fields from files.
!     selectsigmacoord    :: option related to sigma vertical coordinate
!     nonlinfreesurf      :: option related to non-linear free surface
!                           =0 linear free surface ; >0 non-linear
!     select_rstar        :: option related to r* vertical coordinate
!                           =0 (default) use r coord. ; > 0 use r*
!     selectnhfreesurf    :: option for non-hydrostatic (free-)surface formulation:
!                           =0 (default) hydrostatic surf. ; > 0 add nh effects.
!     selectp_ineos_zc    :: select which pressure to use in eos (for z-coords)
!                           =0: simply: -g*rhoconst*z
!                           =1: use pref = integral{-g*rho(tref,sref,pref)*dz}
!                           =2: use hydrostatic dynamical pressure
!                           =3: use full (hyd+nh) dynamical pressure
!     selectaddfluid      :: option to add mass source/sink of fluid in the interior
!                            (3-d generalisation of oceanic real-fresh water flux)
!                           =0 off ; =1 add fluid ; =-1 virtual flux (no mass added)
!     selectimplicitdrag  :: select implicit treatment of bottom/top drag
!                           = 0: fully explicit
!                           = 1: implicit on provisional velocity
!                                (i.e., before grad.eta increment)
!                           = 2: fully implicit (combined with impl surf.press)
!     momforcingoutab     :: =1: take momentum forcing contribution
!                            out of (=0: in) adams-bashforth time stepping.
!     tracforcingoutab    :: =1: take tracer (temp,salt,ptracers) forcing contribution
!                            out of (=0: in) adams-bashforth time stepping.
!     tempadvscheme       :: temp. horiz.advection scheme selector
!     tempvertadvscheme   :: temp. vert. advection scheme selector
!     saltadvscheme       :: salt. horiz.advection scheme selector
!     saltvertadvscheme   :: salt. vert. advection scheme selector
!     selectkescheme      :: kinetic energy scheme selector (vector inv.)
!     selectvortscheme    :: scheme selector for vorticity term (vector inv.)
!     selectcorischeme    :: scheme selector for coriolis term
!     selectbotdragquadr  :: quadratic bottom drag discretisation option:
!                           =0: average ke from grid center to u & v location
!                           =1: use local velocity norm @ u & v location
!                           =2: same with wet-point averaging of other component
!     pcellmix_select     :: select option to enhance mixing near surface & bottom
!                            unit digit: near bottom ; tens digit: near surface
!                            with digit =0 : disable ;
!                           = 1 : increases mixing linearly with recip_hfac
!                           = 2,3,4 : increases mixing by recip_hfac^(2,3,4)
!     readbinaryprec      :: precision used for reading binary files
!     writestateprec      :: precision used for writing model state.
!     writebinaryprec     :: precision used for writing binary files
!     rwsuffixtype        :: controls the format of the mds file suffix.
!                          =0 (default): use iteration number (myiter, i10.10);
!                          =1: 100*mytime (100th sec); =2: mytime (seconds);
!                          =3: mytime/360 (10th of hr); =4: mytime/3600 (hours).
!     monitorselect       :: select group of variables to monitor
!                            =1 : dynvars ; =2 : + vort ; =3 : + surface
!-    debuglevel          :: controls printing of algorithm intermediate results
!                            and statistics ; higher -> more writing
!-    plotlevel           :: controls printing of field maps ; higher -> more flds

      common /parm_i/ &
             cg2dmaxiters, cg2dchkresfreq, &
             cg2dprecondfreq, cg2duseminressol, &
             cg3dmaxiters, cg3dchkresfreq, &
             printresidualfreq, &
             niter0, ntimesteps, ntimesteps_l2, nenditer, &
             selectcorimap, &
             selectsigmacoord, &
             nonlinfreesurf, select_rstar, &
             selectnhfreesurf, selectp_ineos_zc, &
             selectaddfluid, selectimplicitdrag, &
             momforcingoutab, tracforcingoutab, &
             tempadvscheme, tempvertadvscheme, &
             saltadvscheme, saltvertadvscheme, &
             selectkescheme, selectvortscheme, selectcorischeme, &
             selectbotdragquadr, pcellmix_select, &
             readbinaryprec, writebinaryprec, writestateprec, &
             rwsuffixtype, monitorselect, debuglevel, plotlevel
      integer cg2dmaxiters
      integer cg2dchkresfreq
      integer cg2dprecondfreq
      integer cg2duseminressol
      integer cg3dmaxiters
      integer cg3dchkresfreq
      integer printresidualfreq
      integer niter0
      integer ntimesteps
      integer ntimesteps_l2
      integer nenditer
      integer selectcorimap
      integer selectsigmacoord
      integer nonlinfreesurf
      integer select_rstar
      integer selectnhfreesurf
      integer selectp_ineos_zc
      integer selectaddfluid
      integer selectimplicitdrag
      integer momforcingoutab, tracforcingoutab
      integer tempadvscheme, tempvertadvscheme
      integer saltadvscheme, saltvertadvscheme
      integer selectkescheme
      integer selectvortscheme
      integer selectcorischeme
      integer selectbotdragquadr
      integer pcellmix_select
      integer readbinaryprec
      integer writestateprec
      integer writebinaryprec
      integer rwsuffixtype
      integer monitorselect
      integer debuglevel
      integer plotlevel

!--   common /parm_l/ logical valued parameters used by the model.
!- coordinate + grid params:
!     fluidisair       :: set to indicate that the fluid major constituent
!                         is air
!     fluidiswater     :: set to indicate that the fluid major constituent
!                         is water
!     usingpcoords     :: set to indicate that we are working in a pressure
!                         type coordinate (p or p*).
!     usingzcoords     :: set to indicate that we are working in a height
!                         type coordinate (z or z*)
!     usingcartesiangrid :: if true grid generation will be in a cartesian
!                           coordinate frame.
!     usingsphericalpolargrid :: if true grid generation will be in a
!                                spherical polar frame.
!     rotategrid      :: rotate grid coordinates to geographical coordinates
!                        according to euler angles phieuler, thetaeuler, psieuler
!     usingcylindricalgrid :: if true grid generation will be cylindrical
!     usingcurvilineargrid :: if true, use a curvilinear grid (to be provided)
!     haswetcscorners :: domain contains cs-type corners where dynamics is solved
!     deepatmosphere :: deep model (drop the shallow-atmosphere approximation)
!     setinterfdr    :: set interface depth (put cell-center at the middle)
!     setcenterdr    :: set cell-center depth (put interface at the middle)
!     usemin4hfacedges :: set hfacw,hfacs as minimum of adjacent hfacc factor
!     interviscar_pcell :: account for partial-cell in interior vert. viscosity
!     interdiffkr_pcell :: account for partial-cell in interior vert. diffusion
!- momentum params:
!     no_slip_sides  :: impose "no-slip" at lateral boundaries.
!     no_slip_bottom :: impose "no-slip" at bottom boundary.
!     bottomvisc_pcell :: account for partial-cell in bottom visc. (no-slip bc)
!     usesmag3d      :: use isotropic 3-d smagorinsky
!     usefullleith   :: set to true to use full leith viscosity(may be unstable
!                       on irregular grids)
!     usestraintensionvisc:: set to true to use strain-tension viscous terms
!     useareavisclength :: set to true to use old scaling for viscous lengths,
!                          e.g., l2=raz.  may be preferable for cube sphere.
!     momviscosity  :: flag which turns momentum friction terms on and off.
!     momadvection  :: flag which turns advection of momentum on and off.
!     momforcing    :: flag which turns external forcing of momentum on and off.
!     momtidalforcing    :: flag which turns tidal forcing on and off.
!     mompressureforcing :: flag which turns pressure term in momentum equation
!                          on and off.
!     metricterms   :: flag which turns metric terms on or off.
!     usenhmterms   :: if true use non-hydrostatic metric terms.
!     usecoriolis   :: flag which turns the coriolis terms on and off.
!     use3dcoriolis :: turns the 3-d coriolis terms (in omega.cos phi) on - off
!     usecdscheme   :: use cd-scheme to calculate coriolis terms.
!     vectorinvariantmomentum :: use vector-invariant form (mom_vecinv package)
!                                (default = f = use mom_fluxform package)
!     usejamartmomadv :: use wet-point method for v.i. non-linear term
!     upwindvorticity :: bias interpolation of vorticity in the coriolis term
!     highordervorticity :: use 3rd/4th order interp. of vorticity (v.i., advection)
!     useabsvorticity :: work with f+zeta in coriolis terms
!     upwindshear     :: use 1rst order upwind interp. (v.i., vertical advection)
!     momstepping    :: turns momentum equation time-stepping off
!     calc_wvelocity :: turns vertical velocity calculation off
!- temp. & salt params:
!     tempstepping   :: turns temperature equation time-stepping on/off
!     saltstepping   :: turns salinity equation time-stepping on/off
!     addfrictionheating :: account for frictional heating
!     temp_staypositive :: use smolarkiewicz hack to ensure temp stays positive
!     salt_staypositive :: use smolarkiewicz hack to ensure salt stays positive
!     tempadvection  :: flag which turns advection of temperature on and off.
!     tempvertdiff4  :: use vertical bi-harmonic diffusion for temperature
!     tempisactivetr :: pot.temp. is a dynamically active tracer
!     tempforcing    :: flag which turns external forcing of temperature on/off
!     saltadvection  :: flag which turns advection of salinity on and off.
!     saltvertdiff4  :: use vertical bi-harmonic diffusion for salinity
!     saltisactivetr :: salinity  is a dynamically active tracer
!     saltforcing    :: flag which turns external forcing of salinity on/off
!     maskinitemp    :: apply mask to initial pot.temp.
!     maskinisalt    :: apply mask to initial salinity
!     checkinitemp   :: check for points with identically zero initial pot.temp.
!     checkinisalt   :: check for points with identically zero initial salinity
!- pressure solver related parameters (parm02)
!     usesrcgsolver  :: set to true to use conjugate gradient
!                       solver with single reduction (only one call of
!                       s/r mpi_allreduce), default is false
!- time-stepping & free-surface params:
!     rigidlid            :: set to true to use rigid lid
!     implicitfreesurface :: set to true to use implicit free surface
!     uniformlin_phisurf  :: set to true to use a uniform bo_surf in the
!                            linear relation phi_surf = bo_surf*eta
!     uniformfreesurflev  :: true if free-surface level-index is uniform (=1)
!     exactconserv        :: set to true to conserve exactly the total volume
!     linfsconservetr     :: set to true to correct source/sink of tracer
!                            at the surface due to linear free surface
!     userealfreshwaterflux :: if true (=natural bcs), treats p+r-e flux
!                         as a real fresh water (=> changes the sea level)
!                         if f, converts p+r-e to salt flux (no sl effect)
!     storephihyd4phys :: store hydrostatic potential for use in physics/eos
!                         this requires specific code for restart & exchange
!     quasihydrostatic :: using non-hydrostatic terms in hydrostatic algorithm
!     nonhydrostatic   :: using non-hydrostatic algorithm
!     use3dsolver      :: set to true to use 3-d pressure solver
!     implicitintgravwave :: treat internal gravity wave implicitly
!     staggertimestep   :: enable a stagger time stepping u,v (& w) then t,s
!     applyexchuv_early :: apply exch to u,v earlier, just before integr_continuity
!     doresethfactors   :: do reset thickness factors @ beginning of each time-step
!     implicitdiffusion :: turns implicit vertical diffusion on
!     implicitviscosity :: turns implicit vertical viscosity on
!     tempimplvertadv   :: turns on implicit vertical advection for temperature
!     saltimplvertadv   :: turns on implicit vertical advection for salinity
!     momimplvertadv    :: turns on implicit vertical advection for momentum
!     multidimadvection :: flag that enable multi-dimension advection
!     usemultidimadvec  :: true if multi-dim advection is used at least once
!     momdissip_in_ab   :: if false, put dissipation tendency contribution
!                          out off adams-bashforth time stepping.
!     doab_ongtgs       :: if the adams-bashforth time stepping is used, always
!                          apply ab on tracer tendencies (rather than on tracer)
!- other forcing params -
!     balanceempmr    :: substract global mean of empmr at every time step
!     balanceqnet     :: substract global mean of qnet at every time step
!     balanceprintmean:: print substracted global means to stdout
!     dothetaclimrelax :: set true if relaxation to temperature
!                        climatology is required.
!     dosaltclimrelax  :: set true if relaxation to salinity
!                        climatology is required.
!     balancethetaclimrelax :: substract global mean effect at every time step
!     balancesaltclimrelax :: substract global mean effect at every time step
!     allowfreezing  :: allows surface water to freeze and form ice
!     periodicexternalforcing :: set true if forcing is time-dependant
!- i/o parameters -
!     globalfiles    :: selects between "global" and "tiled" files.
!                       on some platforms with mpi, option globalfiles is either
!                       slow or does not work. use usesinglecpuio instead.
!     usesinglecpuio :: moved to eeparams.h
!     pickupstrictlymatch :: check and stop if pickup-file do not stricly match
!     startfrompickupab2 :: with ab-3 code, start from an ab-2 pickup
!     usepickupbeforec54 :: start from old-pickup files, generated with code from
!                           before checkpoint-54a, jul 06, 2004.
!     pickup_write_mdsio :: use mdsio to write pickups
!     pickup_read_mdsio  :: use mdsio to read  pickups
!     pickup_write_immed :: echo the pickup immediately (for conversion)
!     writepickupatend   :: write pickup at the last timestep
!     timeave_mdsio      :: use mdsio for timeave output
!     snapshot_mdsio     :: use mdsio for "snapshot" (dumpfreq/diagfreq) output
!     monitor_stdio      :: use stdio for monitor output
!     dumpinitandlast :: dumps model state to files at initial (niter0)
!                        & last iteration, in addition multiple of dumpfreq iter.

      common /parm_l/ &
      fluidisair, fluidiswater, &
      usingpcoords, usingzcoords, &
      usingcartesiangrid, usingsphericalpolargrid, rotategrid, &
      usingcylindricalgrid, usingcurvilineargrid, haswetcscorners, &
      deepatmosphere, setinterfdr, setcenterdr, usemin4hfacedges, &
      interviscar_pcell, interdiffkr_pcell, &
      no_slip_sides, no_slip_bottom, bottomvisc_pcell, usesmag3d, &
      usefullleith, usestraintensionvisc, useareavisclength, &
      momviscosity, momadvection, momforcing, momtidalforcing, &
      mompressureforcing, metricterms, usenhmterms, &
      usecoriolis, use3dcoriolis, &
      usecdscheme, vectorinvariantmomentum, &
      usejamartmomadv, upwindvorticity, highordervorticity, &
      useabsvorticity, upwindshear, &
      momstepping, calc_wvelocity, tempstepping, saltstepping, &
      addfrictionheating, temp_staypositive, salt_staypositive, &
      tempadvection, tempvertdiff4, tempisactivetr, tempforcing, &
      saltadvection, saltvertdiff4, saltisactivetr, saltforcing, &
      maskinitemp, maskinisalt, checkinitemp, checkinisalt, &
      usesrcgsolver, &
      rigidlid, implicitfreesurface, &
      uniformlin_phisurf, uniformfreesurflev, &
      exactconserv, linfsconservetr, userealfreshwaterflux, &
      storephihyd4phys, quasihydrostatic, nonhydrostatic, &
      use3dsolver, implicitintgravwave, staggertimestep, &
      applyexchuv_early, doresethfactors, &
      implicitdiffusion, implicitviscosity, &
      tempimplvertadv, saltimplvertadv, momimplvertadv, &
      multidimadvection, usemultidimadvec, &
      momdissip_in_ab, doab_ongtgs, &
      balanceempmr, balanceqnet, balanceprintmean, &
      balancethetaclimrelax, balancesaltclimrelax, &
      dothetaclimrelax, dosaltclimrelax, &
      allowfreezing, &
      periodicexternalforcing, &
      globalfiles, &
      pickupstrictlymatch, usepickupbeforec54, startfrompickupab2, &
      pickup_read_mdsio, pickup_write_mdsio, pickup_write_immed, &
      writepickupatend, &
      timeave_mdsio, snapshot_mdsio, monitor_stdio, &
      outputtypesinclusive, dumpinitandlast

      logical fluidisair
      logical fluidiswater
      logical usingpcoords
      logical usingzcoords
      logical usingcartesiangrid
      logical usingsphericalpolargrid, rotategrid
      logical usingcylindricalgrid
      logical usingcurvilineargrid, haswetcscorners
      logical deepatmosphere
      logical setinterfdr
      logical setcenterdr
      logical usemin4hfacedges
      logical interviscar_pcell
      logical interdiffkr_pcell

      logical no_slip_sides
      logical no_slip_bottom
      logical bottomvisc_pcell
      logical usesmag3d
      logical usefullleith
      logical usestraintensionvisc
      logical useareavisclength
      logical momviscosity
      logical momadvection
      logical momforcing
      logical momtidalforcing
      logical mompressureforcing
      logical metricterms
      logical usenhmterms

      logical usecoriolis
      logical use3dcoriolis
      logical usecdscheme
      logical vectorinvariantmomentum
      logical usejamartmomadv
      logical upwindvorticity
      logical highordervorticity
      logical useabsvorticity
      logical upwindshear
      logical momstepping
      logical calc_wvelocity
      logical tempstepping
      logical saltstepping
      logical addfrictionheating
      logical temp_staypositive
      logical salt_staypositive
      logical tempadvection
      logical tempvertdiff4
      logical tempisactivetr
      logical tempforcing
      logical saltadvection
      logical saltvertdiff4
      logical saltisactivetr
      logical saltforcing
      logical maskinitemp
      logical maskinisalt
      logical checkinitemp
      logical checkinisalt
      logical usesrcgsolver
      logical rigidlid
      logical implicitfreesurface
      logical uniformlin_phisurf
      logical uniformfreesurflev
      logical exactconserv
      logical linfsconservetr
      logical userealfreshwaterflux
      logical storephihyd4phys
      logical quasihydrostatic
      logical nonhydrostatic
      logical use3dsolver
      logical implicitintgravwave
      logical staggertimestep
      logical applyexchuv_early
      logical doresethfactors
      logical implicitdiffusion
      logical implicitviscosity
      logical tempimplvertadv
      logical saltimplvertadv
      logical momimplvertadv
      logical multidimadvection
      logical usemultidimadvec
      logical momdissip_in_ab
      logical doab_ongtgs
      logical balanceempmr
      logical balanceqnet
      logical balanceprintmean
      logical dothetaclimrelax
      logical dosaltclimrelax
      logical balancethetaclimrelax
      logical balancesaltclimrelax
      logical allowfreezing
      logical periodicexternalforcing
      logical globalfiles
      logical pickupstrictlymatch
      logical usepickupbeforec54
      logical startfrompickupab2
      logical pickup_read_mdsio, pickup_write_mdsio
      logical pickup_write_immed, writepickupatend
      logical timeave_mdsio, snapshot_mdsio, monitor_stdio
      logical outputtypesinclusive
      logical dumpinitandlast

!--   common /parm_r/ "real" valued parameters used by the model.
!     cg2dtargetresidual
!          :: target residual for cg2d solver; no unit (rhs normalisation)
!     cg2dtargetreswunit
!          :: target residual for cg2d solver; w unit (no rhs normalisation)
!     cg3dtargetresidual
!               :: target residual for cg3d solver.
!     cg2dpcoffdfac :: averaging weight for preconditioner off-diagonal.
!     note. 20th may 1998
!           i made a weird discovery! in the model paper we argue
!           for the form of the preconditioner used here ( see
!           a finite-volume, incompressible navier-stokes model
!           ...., marshall et. al ). the algebra gives a simple
!           0.5 factor for the averaging of ac and acw to get a
!           symmettric pre-conditioner. by using a factor of 0.51
!           i.e. scaling the off-diagonal terms in the
!           preconditioner down slightly i managed to get the
!           number of iterations for convergence in a test case to
!           drop form 192 -> 134! need to investigate this further!
!           for now i have introduced a parameter cg2dpcoffdfac which
!           defaults to 0.51 but can be set at runtime.
!     delr      :: vertical grid spacing ( units of r ).
!     delrc     :: vertical grid spacing between cell centers (r unit).
!     delx      :: separation between cell faces (m) or (deg), depending
!     dely         on input flags. note: moved to header file set_grid.h
!     xgorigin   :: origin of the x-axis (cartesian grid) / longitude of western
!                :: most cell face (lat-lon grid) (note: this is an "inert"
!                :: parameter but it makes geographical references simple.)
!     ygorigin   :: origin of the y-axis (cartesian grid) / latitude of southern
!                :: most face (lat-lon grid).
!     rsphere    :: radius of sphere for a spherical polar grid ( m ).
!     recip_rsphere :: reciprocal radius of sphere ( m^-1 ).
!     radius_fromhorizgrid :: sphere radius of input horiz. grid (curvilinear grid)
!     sealev_z   :: the reference height of sea-level (usually zero)
!     top_pres   :: pressure (p-coords) or reference pressure (z-coords) at the top
!     rsigmabnd  :: vertical position (in r-unit) of r/sigma transition (hybrid-sigma)
!     gravity    :: acceleration due to constant gravity ( m/s^2 )
!     recip_gravity :: reciprocal gravity acceleration ( s^2/m )
!     gbaro      :: accel. due to gravity used in barotropic equation ( m/s^2 )
!     gravfacc   :: gravity factor (vs surf. gravity) vert. profile at cell-center
!     gravfacf   :: gravity factor (vs surf. gravity) vert. profile at cell-interf
!     rhonil     :: reference density for the linear equation of state
!     rhoconst   :: vertically constant reference density (boussinesq)
!     rho1ref    :: reference vertical profile for density (anelastic)
!     rhofacc    :: normalized (by rhoconst) reference density at cell-center
!     rhofacf    :: normalized (by rhoconst) reference density at cell-interface
!     rhoconstfresh :: constant reference density for fresh water (rain)
!     thetaconst :: constant reference for potential temperature
!     tref       :: reference vertical profile for potential temperature
!     sref       :: reference vertical profile for salinity/specific humidity
!     surf_pref  :: surface reference pressure ( pa )
!     pref4eos   :: reference pressure used in eos (case selectp_ineos_zc=1)
!     phiref     :: reference potential (press/rho, geopot) profile (m^2/s^2)
!     dbdrref    :: vertical gradient of reference buoyancy  [(m/s/r)^2]:
!                :: z-coord: = n^2_ref = brunt-vaissala frequency [s^-2]
!                :: p-coord: = -(d.alpha/dp)_ref          [(m^2.s/kg)^2]
!     rvel2wunit :: units conversion factor (non-hydrostatic code),
!                :: from r-coordinate vertical velocity to vertical velocity [m/s].
!                :: z-coord: = 1 ; p-coord: wspeed [m/s] = rvel [pa/s] * rvel2wunit
!     wunit2rvel :: units conversion factor (non-hydrostatic code),
!                :: from vertical velocity [m/s] to r-coordinate vertical velocity.
!                :: z-coord: = 1 ; p-coord: rvel [pa/s] = wspeed [m/s] * wunit2rvel
!     mass2runit :: units conversion factor (surface forcing),
!                :: from mass per unit area [kg/m2] to vertical r-coordinate unit.
!                :: z-coord: = 1/rhoconst ( [kg/m2] / rho = [m] ) ;
!                :: p-coord: = gravity    ( [kg/m2] *  g = [pa] ) ;
!     runit2mass :: units conversion factor (surface forcing),
!                :: from vertical r-coordinate unit to mass per unit area [kg/m2].
!                :: z-coord: = rhoconst  ( [m] * rho = [kg/m2] ) ;
!                :: p-coord: = 1/gravity ( [pa] /  g = [kg/m2] ) ;
!     f0         :: reference coriolis parameter ( 1/s )
!                   ( southern edge f for beta plane )
!     beta       :: df/dy ( s^-1.m^-1 )
!     fprime     :: second coriolis parameter ( 1/s ), related to y-component
!                   of rotation (reference value = 2.omega.cos(phi))
!     omega      :: angular velocity ( rad/s )
!     rotationperiod :: rotation period (s) (= 2.pi/omega)
!     viscarnr   :: vertical profile of eddy viscosity coeff.
!                   for vertical mixing of momentum ( units of r^2/s )
!     viscah     :: eddy viscosity coeff. for mixing of
!                   momentum laterally ( m^2/s )
!     viscahw    :: eddy viscosity coeff. for mixing of vertical
!                   momentum laterally, no effect for hydrostatic
!                   model, defaults to viscahd if unset ( m^2/s )
!                   not used if variable horiz. viscosity is used.
!     visca4     :: biharmonic viscosity coeff. for mixing of
!                   momentum laterally ( m^4/s )
!     visca4w    :: biharmonic viscosity coeff. for mixing of vertical
!                   momentum laterally, no effect for hydrostatic
!                   model, defaults to visca4d if unset ( m^2/s )
!                   not used if variable horiz. viscosity is used.
!     viscahd    :: eddy viscosity coeff. for mixing of momentum laterally
!                   (act on divergence part) ( m^2/s )
!     viscahz    :: eddy viscosity coeff. for mixing of momentum laterally
!                   (act on vorticity  part) ( m^2/s )
!     visca4d    :: biharmonic viscosity coeff. for mixing of momentum laterally
!                   (act on divergence part) ( m^4/s )
!     visca4z    :: biharmonic viscosity coeff. for mixing of momentum laterally
!                   (act on vorticity  part) ( m^4/s )
!     smag3d_coeff     :: isotropic 3-d smagorinsky viscosity coefficient (-)
!     smag3d_diffcoeff :: isotropic 3-d smagorinsky diffusivity coefficient (-)
!     viscc2leith  :: leith non-dimensional viscosity factor (grad(vort))
!     viscc2leithd :: modified leith non-dimensional visc. factor (grad(div))
!     viscc2leithqg:: qg leith non-dimensional viscosity factor
!     viscc4leith  :: leith non-dimensional viscosity factor (grad(vort))
!     viscc4leithd :: modified leith non-dimensional viscosity factor (grad(div))
!     viscc2smag   :: smagorinsky non-dimensional viscosity factor (harmonic)
!     viscc4smag   :: smagorinsky non-dimensional viscosity factor (biharmonic)
!     viscahmax    :: maximum eddy viscosity coeff. for mixing of
!                    momentum laterally ( m^2/s )
!     viscahremax  :: maximum gridscale reynolds number for eddy viscosity
!                     coeff. for mixing of momentum laterally (non-dim)
!     viscahgrid   :: non-dimensional grid-size dependent viscosity
!     viscahgridmax:: maximum and minimum harmonic viscosity coefficients ...
!     viscahgridmin::  in terms of non-dimensional grid-size dependent visc.
!     visca4max    :: maximum biharmonic viscosity coeff. for mixing of
!                     momentum laterally ( m^4/s )
!     visca4remax  :: maximum gridscale reynolds number for
!                     biharmonic viscosity coeff. momentum laterally (non-dim)
!     visca4grid   :: non-dimensional grid-size dependent bi-harmonic viscosity
!     visca4gridmax:: maximum and minimum biharmonic viscosity coefficients ...
!     visca4gridmin::  in terms of non-dimensional grid-size dependent viscosity
!     diffkht   :: laplacian diffusion coeff. for mixing of
!                 heat laterally ( m^2/s )
!     diffk4t   :: biharmonic diffusion coeff. for mixing of
!                 heat laterally ( m^4/s )
!     diffkrnrt :: vertical profile of laplacian diffusion coeff.
!                 for mixing of heat vertically ( units of r^2/s )
!     diffkr4t  :: vertical profile of biharmonic diffusion coeff.
!                 for mixing of heat vertically ( units of r^4/s )
!     diffkhs  ::  laplacian diffusion coeff. for mixing of
!                 salt laterally ( m^2/s )
!     diffk4s   :: biharmonic diffusion coeff. for mixing of
!                 salt laterally ( m^4/s )
!     diffkrnrs :: vertical profile of laplacian diffusion coeff.
!                 for mixing of salt vertically ( units of r^2/s ),
!     diffkr4s  :: vertical profile of biharmonic diffusion coeff.
!                 for mixing of salt vertically ( units of r^4/s )
!     diffkrbl79surf :: t/s surface diffusivity (m^2/s) bryan and lewis, 1979
!     diffkrbl79deep :: t/s deep diffusivity (m^2/s) bryan and lewis, 1979
!     diffkrbl79scl  :: depth scale for arctan fn (m) bryan and lewis, 1979
!     diffkrbl79ho   :: depth offset for arctan fn (m) bryan and lewis, 1979
!     bl79latvary    :: polarwise of this latitude diffkrbl79 is applied with
!                       gradual transition to diffkrbleq towards equator
!     diffkrbleqsurf :: same as diffkrbl79surf but at equator
!     diffkrbleqdeep :: same as diffkrbl79deep but at equator
!     diffkrbleqscl  :: same as diffkrbl79scl but at equator
!     diffkrbleqho   :: same as diffkrbl79ho but at equator
!     pcellmix_maxfac :: maximum enhanced mixing factor for thin partial-cell
!     pcellmix_delr   :: thickness criteria   for too thin partial-cell
!     pcellmix_viscar :: vertical viscosity   for too thin partial-cell
!     pcellmix_diffkr :: vertical diffusivity for too thin partial-cell
!     deltat    :: default timestep ( s )
!     deltatclock  :: timestep used as model "clock". this determines the
!                    io frequencies and is used in tagging output. it can
!                    be totally different to the dynamical time. typically
!                    it will be the deep-water timestep for accelerated runs.
!                    frequency of checkpointing and dumping of the model state
!                    are referenced to this clock. ( s )
!     deltatmom    :: timestep for momemtum equations ( s )
!     dttracerlev  :: timestep for tracer equations ( s ), function of level k
!     deltatfreesurf :: timestep for free-surface equation ( s )
!     freesurffac  :: parameter to turn implicit free surface term on or off
!                     freesurfac = 1. uses implicit free surface
!                     freesurfac = 0. uses rigid lid
!     abeps        :: adams-bashforth-2 stabilizing weight
!     alph_ab      :: adams-bashforth-3 primary factor
!     beta_ab      :: adams-bashforth-3 secondary factor
!     implicsurfpress :: parameter of the crank-nickelson time stepping :
!                     implicit part of surface pressure gradient ( 0-1 )
!     implicdiv2dflow :: parameter of the crank-nickelson time stepping :
!                     implicit part of barotropic flow divergence ( 0-1 )
!     implicitnhpress :: parameter of the crank-nickelson time stepping :
!                     implicit part of non-hydrostatic pressure gradient ( 0-1 )
!     hfacmin      :: minimum fraction size of a cell (affects hfacc etc...)
!     hfacmindz    :: minimum dimensional size of a cell (affects hfacc etc..., m)
!     hfacmindp    :: minimum dimensional size of a cell (affects hfacc etc..., pa)
!     hfacmindr    :: minimum dimensional size of a cell (-> hfacc etc..., r units)
!     hfacinf      :: threshold (inf and sup) for fraction size of surface cell
!     hfacsup          that control vanishing and creating levels
!     taucd         :: cd scheme coupling timescale ( s )
!     rcd           :: cd scheme normalised coupling parameter (= 1 - deltat/taucd)
!     epsab_cd      :: adams-bashforth-2 stabilizing weight used in cd scheme
!     basetime      :: model base time (time origin) = time @ iteration zero
!     starttime     :: starting time for this integration ( s ).
!     endtime       :: ending time for this integration ( s ).
!     chkptfreq     :: frequency of rolling check pointing ( s ).
!     pchkptfreq    :: frequency of permanent check pointing ( s ).
!     dumpfreq      :: frequency with which model state is written to
!                      post-processing files ( s ).
!     diagfreq      :: frequency with which model writes diagnostic output
!                      of intermediate quantities.
!     affacmom      :: advection of momentum term tracer parameter
!     vffacmom      :: momentum viscosity tracer parameter
!     pffacmom      :: momentum pressure forcing tracer parameter
!     cffacmom      :: coriolis term tracer parameter
!     fofacmom      :: momentum forcing tracer parameter
!     mtfacmom      :: metric terms tracer parameter
!     cospower      :: power of cosine of latitude to multiply viscosity
!     cadjfreq      :: frequency of convective adjustment
!
!     tavefreq      :: frequency with which time-averaged model state
!                      is written to post-processing files ( s ).
!     tave_lastiter :: (for state variable only) fraction of the last time
!                      step (of each tavefreq period) put in the time average.
!                      (fraction for 1rst iter = 1 - tave_lastiter)
!     tauthetaclimrelax :: relaxation to climatology time scale ( s ).
!     tausaltclimrelax :: relaxation to climatology time scale ( s ).
!     latbandclimrelax :: latitude band where relaxation to clim. is applied,
!                         i.e. where |yc| <= latbandclimrelax
!     externforcingperiod :: is the period of which forcing varies (eg. 1 month)
!     externforcingcycle :: is the repeat time of the forcing (eg. 1 year)
!                          (note: externforcingcycle must be an integer
!                           number times externforcingperiod)
!     convertfw2salt :: salinity, used to convert fresh-water flux to salt flux
!                       (use model surface (local) value if set to -1)
!     temp_evprrn :: temperature of rain & evap.
!     salt_evprrn :: salinity of rain & evap.
!     temp_addmass :: temperature of addmass array
!     salt_addmass :: salinity of addmass array
!        (notes: a) tracer content of rain/evap only used if both
!                     nonlin_frsurf & userealfreshwater are set.
!                b) use model surface (local) value if set to unset_rl)
!     hmixcriteria:: criteria for mixed-layer diagnostic
!     drhosmall   :: parameter for mixed-layer diagnostic
!     hmixsmooth  :: smoothing parameter for mixed-layer diag (default=0=no smoothing)
!     ivdc_kappa  :: implicit vertical diffusivity for convection [m^2/s]
!     sidedragfactor     :: side-drag scaling factor (used only if no_slip_sides)
!                           (default=2: full drag ; =1: gives half-slip bc)
!     bottomdraglinear    :: linear    bottom-drag coefficient (units of [r]/s)
!     bottomdragquadratic :: quadratic bottom-drag coefficient (units of [r]/m)
!               (if using zcoordinate, units becomes linear: m/s, quadratic: [-])
!     smoothabsfuncrange :: 1/2 of interval around zero, for which fortran abs
!                           is to be replace by a smoother function
!                           (affects myabs, mymin, mymax)
!     nh_am2        :: scales the non-hydrostatic terms and changes internal scales
!                      (i.e. allows convection at different rayleigh numbers)
!     tcylin        :: temperature of the cylinder inner boundary
!     tcylout       :: temperature of the cylinder outer boundary
!     phieuler      :: euler angle, rotation about original z-axis
!     thetaeuler    :: euler angle, rotation about new x-axis
!     psieuler      :: euler angle, rotation about new z-axis
      common /parm_r/ cg2dtargetresidual, cg2dtargetreswunit, &
      cg2dpcoffdfac, cg3dtargetresidual, &
      delr, delrc, xgorigin, ygorigin, rsphere, recip_rsphere, &
      radius_fromhorizgrid, sealev_z, top_pres, rsigmabnd, &
      deltat, deltatmom, dttracerlev, deltatfreesurf, deltatclock, &
      abeps, alph_ab, beta_ab, &
      f0, beta, fprime, omega, rotationperiod, &
      viscfacadj, viscah, viscahw, smag3d_coeff, smag3d_diffcoeff, &
      viscahmax, viscahgrid, viscahgridmax, viscahgridmin, &
      viscc2leith, viscc2leithd, viscc2leithqg, &
      viscc2smag, viscc4smag, &
      viscahd, viscahz, visca4d, visca4z, &
      visca4, visca4w, visca4max, &
      visca4grid, visca4gridmax, visca4gridmin, &
      viscahremax, visca4remax, &
      viscc4leith, viscc4leithd, viscarnr, &
      diffkht, diffk4t, diffkrnrt, diffkr4t, &
      diffkhs, diffk4s, diffkrnrs, diffkr4s, &
      diffkrbl79surf, diffkrbl79deep, diffkrbl79scl, diffkrbl79ho, &
      bl79latvary, &
      diffkrbleqsurf, diffkrbleqdeep, diffkrbleqscl, diffkrbleqho, &
      pcellmix_maxfac, pcellmix_delr, pcellmix_viscar, pcellmix_diffkr, &
      taucd, rcd, epsab_cd, &
      freesurffac, implicsurfpress, implicdiv2dflow, implicitnhpress, &
      hfacmin, hfacmindz, hfacinf, hfacsup, &
      gravity, recip_gravity, gbaro, &
      gravfacc, recip_gravfacc, gravfacf, recip_gravfacf, &
      rhonil, rhoconst, recip_rhoconst, rho1ref, &
      rhofacc, recip_rhofacc, rhofacf, recip_rhofacf, rhoconstfresh, &
      thetaconst, tref, sref, surf_pref, pref4eos, phiref, dbdrref, &
      rvel2wunit, wunit2rvel, mass2runit, runit2mass, &
      basetime, starttime, endtime, &
      chkptfreq, pchkptfreq, dumpfreq, adjdumpfreq, &
      diagfreq, tavefreq, tave_lastiter, monitorfreq, adjmonitorfreq, &
      affacmom, vffacmom, pffacmom, cffacmom, fofacmom, mtfacmom, &
      cospower, cadjfreq, &
      tauthetaclimrelax, tausaltclimrelax, latbandclimrelax, &
      externforcingcycle, externforcingperiod, &
      convertfw2salt, temp_evprrn, salt_evprrn, &
      temp_addmass, salt_addmass, hfacmindr, hfacmindp, &
      ivdc_kappa, hmixcriteria, drhosmall, hmixsmooth, &
      sidedragfactor, bottomdraglinear, bottomdragquadratic, nh_am2, &
      smoothabsfuncrange, &
      tcylin, tcylout, &
      phieuler, thetaeuler, psieuler

      real*8 cg2dtargetresidual
      real*8 cg2dtargetreswunit
      real*8 cg3dtargetresidual
      real*8 cg2dpcoffdfac
      real*8 delr(nr)
      real*8 delrc(nr+1)
      real*8 xgorigin
      real*8 ygorigin
      real*8 rsphere
      real*8 recip_rsphere
      real*8 radius_fromhorizgrid
      real*8 sealev_z
      real*8 top_pres
      real*8 rsigmabnd
      real*8 deltat
      real*8 deltatclock
      real*8 deltatmom
      real*8 dttracerlev(nr)
      real*8 deltatfreesurf
      real*8 abeps, alph_ab, beta_ab
      real*8 f0
      real*8 beta
      real*8 fprime
      real*8 omega
      real*8 rotationperiod
      real*8 freesurffac
      real*8 implicsurfpress
      real*8 implicdiv2dflow
      real*8 implicitnhpress
      real*8 hfacmin
      real*8 hfacmindz
      real*8 hfacmindp
      real*8 hfacmindr
      real*8 hfacinf
      real*8 hfacsup
      real*8 viscarnr(nr)
      real*8 viscfacadj
      real*8 viscah
      real*8 viscahw
      real*8 viscahd
      real*8 viscahz
      real*8 smag3d_coeff, smag3d_diffcoeff
      real*8 viscahmax
      real*8 viscahremax
      real*8 viscahgrid, viscahgridmax, viscahgridmin
      real*8 viscc2leith
      real*8 viscc2leithd
      real*8 viscc2leithqg
      real*8 viscc2smag
      real*8 visca4
      real*8 visca4w
      real*8 visca4d
      real*8 visca4z
      real*8 visca4max
      real*8 visca4remax
      real*8 visca4grid, visca4gridmax, visca4gridmin
      real*8 viscc4leith
      real*8 viscc4leithd
      real*8 viscc4smag
      real*8 diffkht
      real*8 diffk4t
      real*8 diffkrnrt(nr)
      real*8 diffkr4t(nr)
      real*8 diffkhs
      real*8 diffk4s
      real*8 diffkrnrs(nr)
      real*8 diffkr4s(nr)
      real*8 diffkrbl79surf
      real*8 diffkrbl79deep
      real*8 diffkrbl79scl
      real*8 diffkrbl79ho
      real*8 bl79latvary
      real*8 diffkrbleqsurf
      real*8 diffkrbleqdeep
      real*8 diffkrbleqscl
      real*8 diffkrbleqho
      real*8 pcellmix_maxfac
      real*8 pcellmix_delr
      real*8 pcellmix_viscar(nr)
      real*8 pcellmix_diffkr(nr)
      real*8 taucd, rcd, epsab_cd
      real*8 gravity,       recip_gravity
      real*8 gbaro
      real*8 gravfacc(nr),   recip_gravfacc(nr)
      real*8 gravfacf(nr+1), recip_gravfacf(nr+1)
      real*8 rhonil
      real*8 rhoconst,      recip_rhoconst
      real*8 rho1ref(nr)
      real*8 rhofacc(nr),   recip_rhofacc(nr)
      real*8 rhofacf(nr+1), recip_rhofacf(nr+1)
      real*8 rhoconstfresh
      real*8 thetaconst
      real*8 tref(nr)
      real*8 sref(nr)
      real*8 surf_pref, pref4eos(nr)
      real*8 phiref(2*nr+1)
      real*8 dbdrref(nr)
      real*8 rvel2wunit(nr+1), wunit2rvel(nr+1)
      real*8 mass2runit, runit2mass
      real*8 basetime
      real*8 starttime
      real*8 endtime
      real*8 chkptfreq
      real*8 pchkptfreq
      real*8 dumpfreq
      real*8 adjdumpfreq
      real*8 diagfreq
      real*8 tavefreq
      real*8 tave_lastiter
      real*8 monitorfreq
      real*8 adjmonitorfreq
      real*8 affacmom
      real*8 vffacmom
      real*8 pffacmom
      real*8 cffacmom
      real*8 fofacmom
      real*8 mtfacmom
      real*8 cospower
      real*8 cadjfreq
      real*8 tauthetaclimrelax
      real*8 tausaltclimrelax
      real*8 latbandclimrelax
      real*8 externforcingcycle
      real*8 externforcingperiod
      real*8 convertfw2salt
      real*8 temp_evprrn
      real*8 salt_evprrn
      real*8 temp_addmass
      real*8 salt_addmass
      real*8 ivdc_kappa
      real*8 hmixcriteria
      real*8 drhosmall
      real*8 hmixsmooth
      real*8 sidedragfactor
      real*8 bottomdraglinear
      real*8 bottomdragquadratic
      real*8 smoothabsfuncrange
      real*8 nh_am2
      real*8 tcylin, tcylout
      real*8 phieuler, thetaeuler, psieuler

!--   common /parm_a/ thermodynamics constants ?
      common /parm_a/ heatcapacity_cp
      real*8 heatcapacity_cp

!--   common /parm_atm/ atmospheric physical parameters (ideal gas eos, ...)
!     celsius2k :: convert centigrade (celsius) degree to kelvin
!     atm_po    :: standard reference pressure
!     atm_cp    :: specific heat (cp) of the (dry) air at constant pressure
!     atm_rd    :: gas constant for dry air
!     atm_kappa :: kappa = r/cp (r: constant of ideal gas eos)
!     atm_rq    :: water vapour specific volume anomaly relative to dry air
!                  (e.g. typical value = (29/18 -1) 10^-3 with q [g/kg])
!     integr_geopot :: option to select the way we integrate the geopotential
!                     (still a subject of discussions ...)
!     selectfindrosurf :: select the way surf. ref. pressure (=ro_surf) is
!             derived from the orography. implemented: 0,1 (see ini_p_ground)
      common /parm_atm/ &
                 celsius2k, &
                 atm_cp, atm_rd, atm_kappa, atm_rq, atm_po, &
                 integr_geopot, selectfindrosurf
      real*8 celsius2k
      real*8 atm_po, atm_cp, atm_rd, atm_kappa, atm_rq
      integer integr_geopot, selectfindrosurf

!----------------------------------------
!-- logical flags for selecting packages
      logical usegad
      logical useobcs
      logical useshap_filt
      logical usezonal_filt
      logical useopps
      logical usepp81
      logical usekl10
      logical usemy82
      logical useggl90
      logical usekpp
      logical usegmredi
      logical usedown_slope
      logical usebbl
      logical usecal
      logical useexf
      logical usebulkforce
      logical useebm
      logical usecheapaml
      logical useautodiff
      logical usegrdchk
      logical usesmooth
      logical useprofiles
      logical useecco
      logical usectrl
      logical usesbo
      logical useflt
      logical useptracers
      logical usegchem
      logical userbcs
      logical useoffline
      logical usematrix
      logical usefrazil
      logical useseaice
      logical usesalt_plume
      logical useshelfice
      logical usestreamice
      logical useicefront
      logical usethsice
      logical useland
      logical useatm2d
      logical useaim
      logical useatm_phys
      logical usefizhi
      logical usegridalt
      logical usediagnostics
      logical useregrid
      logical uselayers
      logical usemnc
      logical userunclock
      logical useembed_files
      logical usemypackage
      common /parm_packages/ &
             usegad, useobcs, useshap_filt, usezonal_filt, &
             useopps, usepp81, usekl10, usemy82, useggl90, usekpp, &
             usegmredi, usebbl, usedown_slope, &
             usecal, useexf, usebulkforce, useebm, usecheapaml, &
             usegrdchk, usesmooth, useprofiles, useecco, usectrl, &
             usesbo, useflt, useautodiff, &
             useptracers, usegchem, userbcs, useoffline, usematrix, &
             usefrazil, useseaice, usesalt_plume, useshelfice, &
             usestreamice, useicefront, usethsice, useland, &
             useatm2d, useaim, useatm_phys, usefizhi, usegridalt, &
             usediagnostics, useregrid, uselayers, usemnc, &
             userunclock, useembed_files, &
             usemypackage

!eh3 ;;; local variables: ***
!eh3 ;;; mode:fortran ***
!eh3 ;;; end: ***
!     ==================================================================
!     header exf_fields
!     ==================================================================
!
!     o header file for the surface flux data.
!
!     started: ralf.giering@fastopt.de 25-mai-2000
!     changed: field swap in adj. mode; heimbach@mit.edu 10-jan-2002
!     included runoff d. stammer, nov. 25, 2001
!     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-dec-2002
!
!     ==================================================================
!     header exf_fields
!     ==================================================================

!     field definitions, units, and sign conventions:
!     ===============================================
!
!     ustress   :: zonal surface wind stress in n/m^2
!                  > 0 for increase in uvel, which is west to
!                      east for cartesian and spherical polar grids
!                  typical range: -0.5 < ustress < 0.5
!                  input field
!
!     vstress   :: meridional surface wind stress in n/m^2
!                  > 0 for increase in vvel, which is south to
!                      north for cartesian and spherical polar grids
!                  typical range: -0.5 < vstress < 0.5
!                  input field
!
!     hflux     :: net upward surface heat flux including shortwave in w/m^2
!                  hflux = latent + sensible + lwflux + swflux
!                  > 0 for decrease in theta (ocean cooling)
!                  typical range: -250 < hflux < 600
!                  input field
!
!     sflux     :: net upward freshwater flux in m/s
!                  sflux = evap - precip - runoff
!                  > 0 for increase in salt (ocean salinity)
!                  typical range: -1e-7 < sflux < 1e-7
!                  input field
!
!     swflux    :: net upward shortwave radiation in w/m^2
!                  swflux = - ( swdown - ice and snow absorption - reflected )
!                  > 0 for decrease in theta (ocean cooling)
!                  typical range: -350 < swflux < 0
!                  input field
!
!     uwind     :: surface (10-m) zonal wind velocity in m/s
!                  > 0 for increase in uvel, which is west to
!                      east for cartesian and spherical polar grids
!                  typical range: -10 < uwind < 10
!                  input or input/output field
!
!     vwind     :: surface (10-m) meridional wind velocity in m/s
!                  > 0 for increase in vvel, which is south to
!                      north for cartesian and spherical polar grids
!                  typical range: -10 < vwind < 10
!                  input or input/output field
!
!     wspeed    :: surface (10-m) wind speed in m/s
!                  >= 0 sqrt(u^2+v^2)
!                  typical range: 0 < wspeed < 10
!                  input or input/output field
!
!     atemp     :: surface (2-m) air temperature in deg k
!                  typical range: 200 < atemp < 300
!                  input or input/output field
!
!     aqh       :: surface (2m) specific humidity in kg/kg
!                  typical range: 0 < aqh < 0.02
!                  input or input/output field
!
!     hs        :: sensible heat flux into ocean in w/m^2
!                  > 0 for increase in theta (ocean warming)
!
!     hl        :: latent   heat flux into ocean in w/m^2
!                  > 0 for increase in theta (ocean warming)
!
!     lwflux    :: net upward longwave radiation in w/m^2
!                  lwflux = - ( lwdown - ice and snow absorption - emitted )
!                  > 0 for decrease in theta (ocean cooling)
!                  typical range: -20 < lwflux < 170
!                  input field
!
!     evap      :: evaporation in m/s
!                  > 0 for increase in salt (ocean salinity)
!                  typical range: 0 < evap < 2.5e-7
!                  input, input/output, or output field
!
!     precip    :: total precipitation (rain+snow) in m/s of liquid water
!                  > 0 for decrease in salt (ocean salinity)
!                  typical range: 0 < precip < 5e-7
!                  input or input/output field
!
!     snowprecip :: snow precipitation in m/s of equivalent liquid water
!                  > 0 for decrease in salt (ocean salinity)
!                  typical range: 0 < precip < 5e-7
!                  input or input/output field
!
!     runoff    :: river and glacier runoff in m/s
!                  > 0 for decrease in salt (ocean salinity)
!                  typical range: 0 < runoff < ????
!                  input or input/output field
!
!     runoftemp :: temperature of runoff in deg c
!
!     saltflx   :: net upward salt flux in (g/kg).kg/m^2/s = g/m^2/s
!                  > 0 for decrease in sss.
!                  typical origin: salty sea-ice formation / melting.
!
!     swdown    :: downward shortwave radiation in w/m^2
!                  > 0 for increase in theta (ocean warming)
!                  typical range: 0 < swdown < 450
!                  input/output field
!
!     lwdown    :: downward longwave radiation in w/m^2
!                  > 0 for increase in theta (ocean warming)
!                  typical range: 50 < lwdown < 450
!                  input/output field
!
!     apressure :: atmospheric surface pressure field in pa
!                  typical range: 88000 < apressure < 108000
!                  input field
!
!     tidepot   :: tidal geopotential forcing in m^2/s^2
!                  typical range: -10 < tidepot < +10
!                  input field

!     notes:
!     ======
!
!     by default all surface forcing fields are defined at the center
!     of each grid (the rvel location in model/inc/grid.h) unless
!     flags readstressonagrid or readstressoncgrid are set.
!
!     input and output units and sign conventions can be customized
!     using variables exf_inscal_* and exf_outscal_*, which are set
!     by exf_readparms.f
!
!     output fields fu, fv, qnet, qsw, and empmr are
!     defined in ffields.h
!
!     arrays *0 and *1 below are used for temporal interpolation.
!

      common /exf_stress_r/ ustress, vstress
      real*8 ustress   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 vstress   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_ustress_r/ ustress0, ustress1
      real*8 ustress0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 ustress1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_vstress_r/ vstress0, vstress1
      real*8 vstress0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 vstress1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_wspeed_r/ wspeed
      real*8 wspeed   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_wspeed_r/ wspeed0, wspeed1
      real*8 wspeed0  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 wspeed1  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_atm_wind_r/ uwind, vwind
      real*8 uwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 vwind     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_uwind_r/ uwind0, uwind1
      real*8 uwind0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 uwind1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_vwind_r/ vwind0, vwind1
      real*8 vwind0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 vwind1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      common /exf_netflux_r/ hflux, sflux
      real*8 hflux     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 sflux     (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_hflux_r/ hflux0, hflux1
      real*8 hflux0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 hflux1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      common /exfl_sflux_r/ sflux0, sflux1
      real*8 sflux0    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 sflux1    (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)


!     wstress   :: wind-stress magnitude [pa=n/m^2], @ grid-cell center
!     sh        :: wind-speed [m/s] (always larger than umin)
      common /exfl_wind_r/ wstress, cw, sw, sh
      real*8 wstress   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 cw        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 sw        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 sh        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)













      common /exf_esmf/ ustress_esmf, vstress_esmf, &
                       hflux_esmf,sflux_esmf,swflux_esmf, &
                       uwind_esmf,vwind_esmf,wspeed_esmf, &
                       atemp_esmf,aqh_esmf,lwflux_esmf, &
                       evap_esmf,precip_esmf,snowprecip_esmf, &
                       swdown_esmf,lwdown_esmf,apressure_esmf, &
                       runoff_esmf

      real*8 ustress_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 vstress_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8   hflux_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8   sflux_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  swflux_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8   uwind_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8   vwind_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  wspeed_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8   atemp_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8     aqh_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  lwflux_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8   evap_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  precip_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 snowprecip_esmf(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 swdown_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 lwdown_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 apressure_esmf(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8 runoff_esmf   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

!     ==================================================================
!     header calendar
!     ==================================================================
!
!     o this header file contains variables that are used by the
!       calendar tool. the calendar tool can be used in the ecco
!       sealion release of the mitgcmuv.
!
!     started: christian eckert eckert@mit.edu  30-jun-1999
!     changed: christian eckert eckert@mit.edu  17-dec-1999
!              - restructured the original version in order to have a
!                better interface to the mitgcmuv.
!
!     ==================================================================
!     header calendar
!     ==================================================================

!   - parameters of the numerical model:
!
!     modelstart       :: start time of the numerical model.
!     modelstartdate   :: start date of the numerical model.
!     modelend         :: end   time of the numerical model.
!     modelenddate     :: end   date of the numerical model.
!     modelstep        :: timestep of the numerical model.
!     modelintsteps    :: number of timestep that are to be performed.
!     modeliter0       :: the numerical models initial timestep number.
!     modeliterend     :: the models last timestep number.
!     modelstepsperday :: number of model time steps per day (<- removed).

!   - parameters used by the calendar:
!
!     refdate          :: first day of the gregorian calendar.
!     nmonthyear       :: number months in a year.
!     ndaymonth        :: days per month depending on the year being a leap
!                         year or not. if the model calendar is used a 360
!                         days year with 30 days months is used instead.
!     ndaysnoleap      :: number of days in a usual year.
!     ndaysleap        :: number of days in a leap year.
!     nmaxdaymonth     :: maximum number of days in a years month.
!     hoursperday      :: number of hours   in a calendars day.
!     minutesperday    :: number of minutes in a calendars day.
!     minutesperhour   :: number of minutes in a calendars hour.
!     secondsperday    :: number of seconds in a calendars day.
!     secondsperhour   :: number of seconds in a calendars hour.
!     secondsperminute :: number of seconds in a calendars minute.
!     cal_setstatus    :: status of calendar parms setting (0=none, 3=fully set)

      integer nmonthyear
      parameter ( nmonthyear = 12 )

      common /calendar_rl/ &
                     modelstart, &
                     modelend, &
                     modelstep
      real*8 modelstart
      real*8 modelend
      real*8 modelstep

      common /calendar_i/ &
                    refdate, &
                    ndaymonth, &
                    ndaysnoleap, &
                    ndaysleap, &
                    nmaxdaymonth, &
                    hoursperday, &
                    minutesperday, &
                    minutesperhour, &
                    secondsperday, &
                    secondsperhour, &
                    secondsperminute, &
                    modelstartdate, &
                    modelenddate, &
                    modeliter0, &
                    modeliterend, &
                    modelintsteps, &
                    cal_setstatus, &
                    startdate_1, &
                    startdate_2

      integer refdate(4)
      integer ndaymonth(nmonthyear,2)
      integer ndaysnoleap
      integer ndaysleap
      integer nmaxdaymonth
      integer hoursperday
      integer minutesperday
      integer minutesperhour
      integer secondsperday
      integer secondsperhour
      integer secondsperminute

      integer modelstartdate(4)
      integer modelenddate(4)
      integer modeliter0
      integer modeliterend
      integer modelintsteps

      integer cal_setstatus
      integer startdate_1
      integer startdate_2

!   calendardumps :: when set, approximate months (30-31 days) and years (360-372 days)
!                    for parameters chkptfreq, pchkptfreq, tavefreq, seaice_tavefreq,
!                    kpp_tavefreq, and freq in pkg/diagnostics are converted to exact
!                    calendar months and years.  requires pkg/cal.
      common /calendar_l/ &
                    calendardumps, &
                    usingmodelcalendar, &
                    usingnoleapyearcal, &
                    usingjuliancalendar, &
                    usinggregoriancalendar
      logical calendardumps
      logical usingmodelcalendar
      logical usingnoleapyearcal
      logical usingjuliancalendar
      logical usinggregoriancalendar

!     thecalendar :: type of calendar to use; available:
!                    'model', 'gregorian' or 'noleapyear'.
!     dayofweek   :: week day number one is the week day of refdate.
!                    for the gregorian calendar this is friday, 15-oct-1582.
!     monthofyear :: both available calendars are assumed to have twelve
!                    months.
      common /calendar_c/ &
                          thecalendar, &
                          dayofweek, &
                          monthofyear
      character*(20) thecalendar
      character*(3) dayofweek(7)
      character*(3) monthofyear(nmonthyear)

!bop
!     !routine: dynvars.h
!     !interface:
!     include "dynvars.h"
!     !description:
!     \bv
!     *==========================================================*
!     | dynvars.h
!     | o dynamical model variables (common block dynvars_r)
!     *==========================================================*
!     | the value and two levels of time tendency are held for
!     | each prognostic variable.
!     *==========================================================*
!     \ev
!eop

!     state variables:
!     etan  :: free-surface r-anomaly (r unit) at current time level
!     uvel  :: zonal velocity (m/s, i=1 held at western face)
!     vvel  :: meridional velocity (m/s, j=1 held at southern face)
!     theta :: potential temperature (oc, held at pressure/tracer point)
!     salt  :: salinity (g/kg, held at pressure/tracer point; note that
!              salinity is either a conductivity ratio or, if using teos10,
!              a mass ratio;here we assume it is a mass ratio even though
!              it is only correct for teos10)
!     gx, gxnm1 :: time tendencies at current and previous time levels.
!     etah  :: surface r-anomaly, advanced in time consistently
!              with 2.d flow divergence (exact-conservation):
!                etah^n+1 = etah^n - delta_t*div.(h^n u^n+1)
!  note: a) used with "exactconserv", necessary for non-lin free-surf and mixed
!           forward/backward free-surf time stepping (e.g., crank-nickelson)
!        b) same as etan but not necessarily at the same time, e.g.:
!           implicdiv2dflow=1 => etah=etan ; =0 => etah=etan^(n-1);

      common /dynvars_r/ &
                        etan, &
                        uvel,vvel,wvel,theta,salt, &
                        gu,   gv, &
                        gunm1,gvnm1,gtnm1,gsnm1
      real*8  etan  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  uvel (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  vvel (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  wvel (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  theta(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  salt (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  gu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  gv(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  gunm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  gvnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  gtnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  gsnm1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)


      common /dynvars_r_2/ &
                        etah
      real*8  etah  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)



!   the following blocks containing requires anomaly fields of control vars
!   and related to options:
!   allow_kapgm_control , allow_kapredi_control and allow_bottomdrag_control
!   have been moved to header file "ctrl_fields.h"


!     diagnostic variables:
!     phihydlow    :: phi-hydrostatic at r-lower boundary
!                     (bottom in z-coordinates, top in p-coordinates)
!     totphihyd    :: total hydrostatic potential (anomaly, for now),
!                     at cell center level ; includes surface contribution.
!                     (for diagnostic + used in z-coord with eos_funct_p)
!     rhoinsitu    :: in-situ density anomaly [kg/m^3] at cell center level.
!     hmixlayer    :: mixed layer depth [m]
!                     (for diagnostic + used gmredi "fm07")
!     ivdconvcount :: impl.vert.diffusion convection counter:
!                     = 0 (not convecting) or 1 (convecting)
      common /dynvars_diag/ &
                     phihydlow, totphihyd, &
                     rhoinsitu, &
                     hmixlayer, ivdconvcount
      real*8  phihydlow(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  totphihyd(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  rhoinsitu(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      real*8  hmixlayer(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      real*8  ivdconvcount(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)


!endendend
       contains
       end module mod_mit_gcm
