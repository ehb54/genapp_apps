      program quafit

c.....intanto fai una subroutine che trova le coo.atomiche del linker...

c,,,,,Io farei una subroutine tipo fullpdb(ipdb) (che potresti chiamare fulllinker(istate,iilink,iconf)  che prende le coordinate tipo la nuova linkerpdb e memorizza gli alm in un array che chiamerei almstore(mxpdb+3,iq,l,m,is) e che poi subira' tutte le varie trasformazioni fifj per essere quindi memorizzato (forse avrai una f3f4?). Io non metterei acqua di idratazione.

c      call chis2(f)  !!! dentro questa dovrai anche rivedere fullconst...

c     mi sa che per ogni ihagg le coord dei linker vanno traslate su <u> e poi il sistema di rif. va ruotato di sys2

c direi che puoi memorizzare solo i calpha di ogni aa di ogni linker (usa una subroutine tipo linker_final_coo(istate,iilink,iconf))  poi fai un test con una sig() calcolati sulla base del tuo pdb e di un pdb, ad es. di un ch3.

c devi anche stabilire tra chi fai il test. consideri le molteplicita'? teoricamente si'. ma quante e quali sono? intanto con una subr. tipo framhag, stabilisci bene tutti i centri dei calpha delle varie catene. Nel sistema di rif. corretto.

c.. forse conviene evitare di controllare l'overlap tra i primi 2-3 AA attaccati al dominio.

c... centre_aa_fin <- framalllink <-framhaglink <-link2

c....15/01/2011 con nexper=0, per motivi ancora non chiari, forse perche' non vengono chiamate delle framhing, non funziona
c    link2 va comunque eseguito, altrimenti non vengono le coordinate dei linker

c    ho aumentato di 5 AA lo shift dentro disv_site_aa

c....3/3/2011
c con il gfortran di alisf1, c'e' un limite al numero di file che posso aprire contemporaneamente. tale limite deve essere
c piu' alto con il g95 di windows 32-bit. Me ne sono accorto perche' con istate=2 nhagg(1,1)=6 e nhagg(2,1)=1 e nconf(2,1)=6
c gfortran da' un errore del tipo "Fortran runtime error: Bad file descriptor" che invece non da' piu' se abbasso nhagg(1,1) o 
c nconf(2,1). mi sono fatto stapare da jseque. Se il max e' 404 gira, se e' 60X non gira. Forse il limite e' 500.
     
      include 'quafit2014.cmn'
      parameter(cut=1.d-20)
      dimension x(mxy),h(mxy*(mxy+1)/2),g(mxy),w(3*mxy),
     >qietmp(mn),x2(mxy),ximin(mxiter,mxy),fimin(mxiter,mxy)


c######################################################################
c istate. Uno stato e' definito come una successione di un definito 
c         numero di subunita' rigide, chiamati siti.
c iconf.  Ciascuno stato puo' esistere in un definito numero di configurazioni
c ihagg.  Ciascuno stato in una configurazione puo formare uno o piu' aggregati, 
c         ciascuno con un gruppo di simmetria puntuale unito a un asse screw.
c 
c 
c 
c 
c######################################################################
      ihetatm=1   !! non legge i file HETATM nel pdb
      jfrac=1   !!1-> xfrac invece del termodi.
      jfracm=0 !! 1-> xfrac parametri da min.
      isigplot=0 !! =1 fa il plot delle sigma
      idisvconn=0 !! esclude dal test disv i siti connessi, ecco qua l'errore::::::!!!!!! valeva come test per le catene.
      id5c5=0 !! =1 vale solo per caso mix D5/C5 symmetry
      alphapower=0.5
      reddsmax=0.d0
      icheckhag=0
      ieqcont=0
      isticky=0
      redsticky=10.d0
      isigplotregular=1
      pscale=0.04
      tauscale=1.0
      i2confr=0
      rscale2=300.d0
      icolumn3=1
      iconfr_rot=0
      qinputscale=1.0
      sinputscale=1.0
c######################################################################


      open(unit=5,status='unknown',file='code.usr')
        read(5,'(a4)') code
      close (unit=5,status='delete')

      npdb=0
      do k=1,mxeffec
        effave(k)=0.d0  !! valore medio effettore
      enddo

      open (unit=iu_dat,status='unknown',file='qua'//code//'.dat')
      open (unit=iu_out,status='unknown',file='qua'//code//'_fin.out')

      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee

      read(iu_dat,2100) string,
     >ncycl
      write(*,2100) string,
     >ncycl
      read(iu_dat,2100) string,
     >nsubrun
      write(*,2100) string,
     >nsubrun
      read(iu_dat,2200) string,
     >fhypo
      write(*,2200) string,
     >fhypo
      read(iu_dat,2200) string,
     >tempfix
      write(*,2200) string,
     >tempfix
      read(iu_dat,2100) string,
     >niter
      write(*,2100) string,
     >niter
      if (niter.gt.mxiter) stop 'niter.gt.mxiter'      
      read(iu_dat,2100) string,
     >lfirst
      write(*,2100) string,
     >lfirst
      if (lfirst.gt.larr) stop 'lfirst.gt.larr'      
      read(iu_dat,2100) string,
     >lsecond
      if (lfirst+lsecond.gt.2*larr) stop 'lfirst+lsecond too big'
      if (lsecond+lsecond.gt.2*larr) stop 'lsecond+lsecond too big'
      write(*,2100) string,
     >lsecond
      if (lsecond.gt.larr) stop 'lsecond.gt.larr'      
      ldecidi=lfirst
      read(iu_dat,2100) string,
     >ltird
      write(*,2100) string,
     >ltird
      if (ltird.gt.la3) stop 'ltird.gt.la3'      
      read(iu_dat,2200) string,
     >qmfull
      write(*,2200) string,
     >qmfull
      read(iu_dat,2100) string,
     >nqfull
      if (nqfull.gt.mn) stop 'nqfull > mn'
      write(*,2100) string,
     >nqfull
      read(iu_dat,2200) string,
     >dfcc
      write(*,2200) string,
     >dfcc
      read(iu_dat,2100) string,
     >nsfull
      write(*,2100) string,
     >nsfull
      if (nsfull.gt.mxshell) stop 'nsfull.gt.mxshell'      
      read(iu_dat,2200) string,
     >w_overl
      write(*,2200) string,
     >w_overl
      read(iu_dat,2200) string,
     >w_kforce
      write(*,2200) string,
     >w_kforce
      read(iu_dat,2200) string,
     >w_penalty
      write(*,2200) string,
     >w_penalty
      read(iu_dat,2200) string,
     >w_aaaa
      write(*,2200) string,
     >w_aaaa
      read(iu_dat,2200) string,
     >w_aadd
      write(*,2200) string,
     >w_aadd
      read(iu_dat,2200) string,
     >dum1
      write(*,2200) string,
     >dum1
      read(iu_dat,2100) string,
     >nz
      write(*,2100) string,
     >nz
      read(iu_dat,2100) string,
     >nd
      write(*,2100) string,
     >nd
      read(iu_dat,2100) string,
     >np3d
      write(*,2100) string,
     >np3d
      read(iu_dat,2200) string,
     >t0ref
      write(*,2200) string,
     >t0ref
      t0ref=t0ref+273.15
      read(iu_dat,2200) string,
     >p0ref
      write(*,2200) string,
     >p0ref
      read(iu_dat,4100) string,
     >famstone
      write(*,4100) string,
     >famstone
      read(iu_dat,2100) string,
     >ifsig
      write(*,2100) string,
     >ifsig
      read(iu_dat,2100) string,
     >ifilm
      write(*,2100) string,
     >ifilm
      read(iu_dat,2100) string,
     >ifspdb
      write(*,2100) string,
     >ifspdb
c.....Precedente .ini
      read(iu_dat,'(40x,i15)') ihetatm
      write(*,'(40x,i15)') ihetatm
      read(iu_dat,'(40x,i15)') jfrac
      write(*,'(40x,i15)') jfrac
      read(iu_dat,'(40x,i15)') jfracm
      write(*,'(40x,i15)') jfracm
      read(iu_dat,'(40x,i15)') isigplot
      write(*,'(40x,i15)') isigplot
      read(iu_dat,'(40x,i15)') idisvconn
      write(*,'(40x,i15)') idisvconn
      read(iu_dat,'(40x,i15)') id5c5
      write(*,'(40x,i15)') id5c5
      read(iu_dat,'(40x,g15.5E3)') alphapower
      write(*,'(40x,g15.5E3)') alphapower
      read(iu_dat,'(40x,g15.5E3)') reddsmax
      write(*,'(40x,g15.5E3)') reddsmax
      read(iu_dat,'(40x,i15)') icheckhag
      write(*,'(40x,i15)') icheckhag
      read(iu_dat,'(40x,i15)') ieqcont
      write(*,'(40x,i15)') ieqcont
      read(iu_dat,'(40x,i15)') isticky
      write(*,'(40x,i15)') isticky
      read(iu_dat,'(40x,g15.5E3)') redsticky
      write(*,'(40x,g15.5E3)') redsticky
      read(iu_dat,'(40x,i15)') isigplotregular
      write(*,'(40x,i15)') isigplotregular
      read(iu_dat,'(40x,g15.5E3)') pscale
      write(*,'(40x,g15.5E3)') pscale
      read(iu_dat,'(40x,g15.5E3)') tauscale
      write(*,'(40x,g15.5E3)') tauscale
      read(iu_dat,'(40x,g15.5E3)') epsprime
      write(*,'(40x,g15.5E3)') epsprime
      read(iu_dat,'(40x,i15)') i2confr
      write(*,'(40x,i15)') i2confr
      read(iu_dat,'(40x,g15.5E3)') facttstart
      write(*,'(40x,g15.5E3)') facttstart
      read(iu_dat,'(40x,g15.5E3)') rscale2
      write(*,'(40x,g15.5E3)') rscale2
      read(iu_dat,'(40x,i15)') icolumn3
      write(*,'(40x,i15)') icolumn3
      read(iu_dat,'(40x,i15)') iconfr_rot
      write(*,'(40x,i15)') iconfr_rot
      read(iu_dat,'(40x,g15.5E3)') qinputscale
      write(*,'(40x,g15.5E3)') qinputscale
      read(iu_dat,'(40x,g15.5E3)') sinputscale
      write(*,'(40x,g15.5E3)') sinputscale
      if (qinputscale.eq.0.0) qinputscale=1.0
      if (sinputscale.eq.0.0) sinputscale=1.0

      do i=1,7 !tmp
      read(iu_dat,'(40x,g15.5E3)') dum1
      write(*,'(40x,g15.5E3)') dum1
      enddo

      if (i2confr.eq.1) then
       w_overl=0.0
       w_kforce=0.0
       w_penalty=0.0
       w_aaaa=0.0
       w_aadd=0.0
      endif


      read(iu_dat,4100) string,filee
      write(*,4100)  string,filee
      read(iu_dat,4100) string,filee
      write(*,4100)  string,filee
      read(iu_dat,4100) string,filee
      write(*,4100)  string,filee

      read(iu_dat,2100) string,
     >nstate
      write(*,2100) string,
     >nstate
      if (nstate.gt.mxstate) stop 'nstate.gt.mxstate'      
      nagg=0
      do istate=1,nstate
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,2100) string,
     >nconf(istate)
      write(*,2100) string,
     >nconf(istate)
      if (nconf(istate).gt.mxconf) stop 'nconf(istate).gt.mxconf'
      if (nconf(istate).eq.0) stop 'nconf(istate)=0'
      read(iu_dat,2100) string,
     >nsite(istate)
      write(*,2100) string,
     >nsite(istate)
      if (nsite(istate).gt.mxsite) stop 'nsite(istate).gt.mxsite'

      read(iu_dat,'(a200)') text
      read(text,2100) string,
     >nlink(istate)
      write(*,2100) string,
     >nlink(istate)
      if (nlink(istate).gt.mxlink) stop 'nlink(istate).gt.mxliker'
      i1=1
c      write(*,*) text(98:lnfil(text,200))
      read(text(98:lnfil(text,200)),'(200(1x,i3))',err=291,end=291)
     >(iseqpdb(istate,i),i=1,nsite(istate)+nlink(istate))
      i1=0
c      write(*,*) ' ci sono ',i1
      do i=1,nsite(istate)+nlink(istate)
c        write(*,*) iseqpdb(istate,i)
        if (iseqpdb(istate,i).eq.0) i1=1
      enddo
 291  if (i1.eq.1) then
        do i=1,nsite(istate)+nlink(istate)
          iseqpdb(istate,i)=i
        enddo
      else
        do i=1,nsite(istate)+nlink(istate)
          if (iseqpdb(istate,i).lt.0.or.iseqpdb(istate,i).
     >gt.nsite(istate)+nlink(istate)) stop 'error iseqpdb'
        enddo
        do i=1,nsite(istate)+nlink(istate)
          is=iseqpdb(istate,i)
        do j=1,nsite(istate)+nlink(istate)
          if (j.ne.i) then
            if (iseqpdb(istate,j).eq.is) 
     >stop 'error iseqpdb duplicated isite/ilink'
          endif
        enddo
        enddo
      endif
      write(*,"(a,100(i3,1x))") text(59:98),
     >(iseqpdb(istate,i),i=1,nsite(istate)+nlink(istate))

      read(iu_dat,2100) string,
     >nhagg(istate)
      write(*,2100) string,
     >nhagg(istate)
      if (nhagg(istate).gt.mxhagg) stop 'nhagg(istate).gt.mxhagg'
c      read(iu_dat,2100) string,
c     >ifhinge(istate)
c      write(*,2100) string,
c     >ifhinge(istate)
      ifhinge(istate)=0!! lo metto cosi' di default
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee

      do ihagg=1,nhagg(istate)
      nagg=nagg+1
      if (nagg.gt.mxagg) stop 'nagg>mxagg'
      jstagg(nagg)=istate
      jhaagg(nagg)=ihagg
      jaghag(istate,ihagg)=nagg
c      write(*,*) ' nagg ',nagg,istate,ihagg,jaghag(istate,ihagg)
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,2101) string,
     >ggss(istate,ihagg)
      write(*,2101) string,
     >ggss(istate,ihagg)

      read(iu_dat,2200) string,
     >agga(istate,ihagg)
      write(*,2200) string,
     >agga(istate,ihagg)
      read(iu_dat,2200) string,
     >aggb(istate,ihagg)
      write(*,2200) string,
     >aggb(istate,ihagg)
      read(iu_dat,2200) string,
     >aggg(istate,ihagg)
      write(*,2200) string,
     >aggg(istate,ihagg)

      agga(istate,ihagg)=agga(istate,ihagg)/conv
      aggb(istate,ihagg)=aggb(istate,ihagg)/conv
      aggg(istate,ihagg)=aggg(istate,ihagg)/conv

      read(iu_dat,2100) string,
     >npoly(istate,ihagg)
      write(*,2100) string,
     >npoly(istate,ihagg)
      if (npoly(istate,ihagg).gt.mxpoly) 
     >stop 'npoly(istate,ihagg).gt.mxpoly'
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      
      do iconf=1,nconf(istate)
      read(iu_dat,2207) string,alpscrew(istate,ihagg,4+iconf),
     >(alpscrew(istate,ihagg,i),i=2,4),jalpscrew(istate,ihagg)
      write(*,2207) string,alpscrew(istate,ihagg,4+iconf),
     >(alpscrew(istate,ihagg,i),i=2,4),jalpscrew(istate,ihagg)

      alpscrew(istate,ihagg,4+iconf) = alpscrew(istate,ihagg,4+iconf)
     >/conv
      do i=2,4
        alpscrew(istate,ihagg,i) = alpscrew(istate,ihagg,i) / conv
      enddo
      read(iu_dat,2207) string,cbescrew(istate,ihagg,4+iconf),
     >(cbescrew(istate,ihagg,i),i=2,4),jcbescrew(istate,ihagg)
      write(*,2207) string,cbescrew(istate,ihagg,4+iconf),
     >(cbescrew(istate,ihagg,i),i=2,4),jcbescrew(istate,ihagg)

      read(iu_dat,2207) string,ascrew(istate,ihagg,4+iconf),
     >(ascrew(istate,ihagg,i),i=2,4),jascrew(istate,ihagg)
      write(*,2207) string,ascrew(istate,ihagg,4+iconf),
     >(ascrew(istate,ihagg,i),i=2,4),jascrew(istate,ihagg)

      ascrew(istate,ihagg,4+iconf) = ascrew(istate,ihagg,4+iconf)/conv
      do i=2,4
        ascrew(istate,ihagg,i) = ascrew(istate,ihagg,i) / conv
      enddo

      read(iu_dat,2207) string,dscrew(istate,ihagg,4+iconf),
     >(dscrew(istate,ihagg,i),i=2,4),jdscrew(istate,ihagg)
      write(*,2207) string,dscrew(istate,ihagg,4+iconf),
     >(dscrew(istate,ihagg,i),i=2,4),jdscrew(istate,ihagg)

      read(iu_dat,2207) string,gammacontr(istate,ihagg,4+iconf),
     >(gammacontr(istate,ihagg,i),i=2,4),jgammacontr(istate,ihagg)
      write(*,2207) string,gammacontr(istate,ihagg,4+iconf),
     >(gammacontr(istate,ihagg,i),i=2,4),jgammacontr(istate,ihagg)

      read(iu_dat,2207) string,rmeff(istate,ihagg,4+iconf),
     >(rmeff(istate,ihagg,i),i=2,4),jrmeff(istate,ihagg)
      write(*,2207) string,rmeff(istate,ihagg,4+iconf),
     >(rmeff(istate,ihagg,i),i=2,4),jrmeff(istate,ihagg)

      do k=1,mxshell  
      read(iu_dat,2207) string,rhcl(istate,ihagg,k,4+iconf),
     >(rhcl(istate,ihagg,k,i),i=2,4),jrhcl(istate,ihagg,k)
      write(*,2207) string,rhcl(istate,ihagg,k,4+iconf),
     >(rhcl(istate,ihagg,k,i),i=2,4),jrhcl(istate,ihagg,k)
      enddo
      enddo! end iconf

      do k=1,mxtherf  
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      do j=1,mxeffec+1    
      read(iu_dat,2207) string,
     >(ftherm(istate,ihagg,k,j,i),i=1,4),jftherm(istate,ihagg,k,j)
      if (jfrac.eq.1) jftherm(istate,ihagg,k,j)=0
      write(*,2207) string,
     >(ftherm(istate,ihagg,k,j,i),i=1,4),jftherm(istate,ihagg,k,j)
      enddo! end j
      enddo! end k      

      enddo! end ihagg


      do isite=1,nsite(istate)
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,
     >filee
      write(*,4100) string,
     >filee

      do ipdb=1,npdb
        if (filee.eq.fsub(ipdb)) then
          ipdbsite(istate,isite)=ipdb
          goto 24
        endif
      enddo
      npdb=npdb+1
      if (npdb.gt.mxpdb) stop 'npdb.gt.mxpdb'
      fsub(npdb)=filee
      ipdbsite(istate,isite)=npdb
 24   continue
   
      ipdb=ipdbsite(istate,isite)
c      read(iu_dat,2200) string,
c     >dnextsite(istate,isite)
c      write(*,2200) string,
c     >dnextsite(istate,isite)
c      read(iu_dat,2200) string,
c     >something(istate,isite)
c      write(*,2200) string,
c     >something(istate,isite)


      read(iu_dat,4100) string,filee
      write(*,4100) string,filee

      do iconf=1,nconf(istate)
      read(iu_dat,2207) string,ums(istate,isite,4+iconf),
     >(ums(istate,isite,i),i=2,4),jums(istate,isite)
      write(*,2207) string,ums(istate,isite,4+iconf),
     >(ums(istate,isite,i),i=2,4),jums(istate,isite)
      read(iu_dat,2207) string,aus(istate,isite,4+iconf),
     >(aus(istate,isite,i),i=2,4),jaus(istate,isite)
      write(*,2207) string,aus(istate,isite,4+iconf),
     >(aus(istate,isite,i),i=2,4),jaus(istate,isite)
      aus(istate,isite,4+iconf) = aus(istate,isite,4+iconf)/conv
      do i=2,4
        aus(istate,isite,i) = aus(istate,isite,i) / conv
      enddo

      read(iu_dat,2207) string,cbu(istate,isite,4+iconf),
     >(cbu(istate,isite,i),i=2,4),jcbu(istate,isite)
      if (cbu(istate,isite,2).gt.1.d0) cbu(istate,isite,2)=1.d0
      if (cbu(istate,isite,2).lt.-1.d0) cbu(istate,isite,2)=-1.d0
      if (cbu(istate,isite,3).gt.1.d0) cbu(istate,isite,2)=1.d0
      if (cbu(istate,isite,3).lt.-1.d0) cbu(istate,isite,2)=-1.d0
      write(*,2207) string,cbu(istate,isite,4+iconf),
     >(cbu(istate,isite,i),i=2,4),jcbu(istate,isite)

      if (ifhinge(istate)
     >.eq.1.and.isite.gt.1) jums(istate,isite)=0 
      if (ifhinge(istate)
     >.eq.1.and.isite.gt.1) jaus(istate,isite)=0 
      if (ifhinge(istate)
     >.eq.1.and.isite.gt.1) jcbu(istate,isite)=0 

      read(iu_dat,2207) string,aas(istate,isite,4+iconf),
     >(aas(istate,isite,i),i=2,4),jaas(istate,isite)
      write(*,2207) string,aas(istate,isite,4+iconf),
     >(aas(istate,isite,i),i=2,4),jaas(istate,isite)

      read(iu_dat,2207) string,cba(istate,isite,4+iconf),
     >(cba(istate,isite,i),i=2,4),jcba(istate,isite)
      if (cba(istate,isite,2).gt.1.d0) cba(istate,isite,2)=1.d0
      if (cba(istate,isite,2).lt.-1.d0) cba(istate,isite,2)=-1.d0
      if (cba(istate,isite,3).gt.1.d0) cba(istate,isite,2)=1.d0
      if (cba(istate,isite,3).lt.-1.d0) cba(istate,isite,2)=-1.d0
      write(*,2207) string,cba(istate,isite,4+iconf),
     >(cba(istate,isite,i),i=2,4),jcba(istate,isite)

      read(iu_dat,2207) string,gas(istate,isite,4+iconf),
     >(gas(istate,isite,i),i=2,4),jgas(istate,isite)
      write(*,2207) string,gas(istate,isite,4+iconf),
     >(gas(istate,isite,i),i=2,4),jgas(istate,isite)

      aas(istate,isite,4+iconf) = aas(istate,isite,4+iconf)/conv
      gas(istate,isite,4+iconf) = gas(istate,isite,4+iconf)/conv
      do i=2,4
        aas(istate,isite,i) = aas(istate,isite,i) / conv
        gas(istate,isite,i) = gas(istate,isite,i) / conv
      enddo
      jconf=iconf+4
c            write(*,*) ' mattina'
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv

      if (ifhinge(istate)
     >.eq.2) jaas(istate,isite)=0 
      if (ifhinge(istate)
     >.eq.2) jcba(istate,isite)=0 
      if (ifhinge(istate)
     >.eq.2) jgas(istate,isite)=0 

      enddo! end iconf
      enddo! end isite


c.....E adesso i linkers
      do isite=1,nsite(istate)
        ilinksite(istate,isite,1)=0 !azzera il numero di linkers che insistono sul site isite-esimo
      enddo
      do iilink=1,nlink(istate)
      read(iu_dat,4100) string,
     >filee
      write(*,4100) string,
     >filee !il primo serve per saltare una riga
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
c.....riconosciamo la struttura primaria
      n=lnfil(filee,300)
      if (n.gt.mxaalink) stop 'mxaalink'
      naalink(istate,iilink)=n
      do k=1,n
        do j=1,22
          if (filee(k:k).eq.amm1(j)) then
            iprimlink(istate,iilink,k)=j
            goto 4545
          endif
        enddo
        stop 'iprimlink'
 4545   continue
      enddo
      write(*,*) (amm(iprimlink(istate,iilink,k)),'-',k=1,n)
      read(iu_dat,2100) string,
     >idum1
      if (idum1.lt.0.or.idum1.gt.nsite(istate)) stop 'idum1'
      write(*,2100) string,
     >idum1
      isitelink(istate,iilink,1)=idum1 !Site # attached to the linker's N-term
      if (idum1.ne.0) then
      k=ilinksite(istate,idum1,1)+1
      ilinksite(istate,idum1,1)=k
      ilinksite(istate,idum1,1+k)=iilink
      endif
      read(iu_dat,2100) string,
     >idum1
      write(*,2100) string,
     >idum1
      isitelink(istate,iilink,2)=idum1 !AA # of the previous site
      read(iu_dat,2100) string,
     >idum1
      if (idum1.lt.0.or.idum1.gt.nsite(istate)) stop 'idum1'
      write(*,2100) string,
     >idum1
      isitelink(istate,iilink,3)=idum1 !Site # attached to the linker's C-term
      if (idum1.ne.0) then
      k=ilinksite(istate,idum1,1)+1
      ilinksite(istate,idum1,1)=k
      ilinksite(istate,idum1,1+k)=iilink
      endif
      read(iu_dat,2100) string,
     >idum1
      write(*,2100) string,
     >idum1
      isitelink(istate,iilink,4)=idum1 !AA # of the previous site
      read(iu_dat,2100) string,
     >idum1
      write(*,2100) string,
     >idum1
      jlink(istate,iilink)=idum1!Flag for phi, psi and chi variations

c jlink=0 non si muovono, jlink=1 si muovono quando si muove il/i sito/i a cui e' attaccato jlink=2 si muove anche indipendentemente

      read(iu_dat,4100) string,filee !riga saltata
      write(*,4100) string,filee

      do iconf=1,nconf(istate)
      read(iu_dat,2209) string,
     >(phi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
     >,((phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
     >,(phi_lnk(istate,iilink,4+iconf,iaa),iaa=n+1,n+1)  !phi n+1 se c'e' e se serve
     >,((phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=n+1,n+1)
c.....Test reset
      dum1=0.d0
      do iaa=1,n+1
        dum1=dum1+phi_lnk(istate,iilink,4+iconf,iaa)
        do i=2,4
          dum1=dum1+phi_lnk(istate,iilink,i,iaa)
        enddo
      enddo
      if (dum1.eq.0.d0) then ! reset phi angles
        do iaa=1,n+1
          phi_lnk(istate,iilink,4+iconf,iaa)=0.d0
          phi_lnk(istate,iilink,2,iaa)=-180.0
          phi_lnk(istate,iilink,3,iaa)= 180.0
          phi_lnk(istate,iilink,4,iaa)=  90.0
        enddo
      endif
      iaa=n+1
      dum1=dum1+phi_lnk(istate,iilink,4+iconf,iaa)
      do i=2,4
        dum1=dum1+phi_lnk(istate,iilink,i,iaa)
      enddo
      if (dum1.eq.0.d0) then ! reset phi angles
        phi_lnk(istate,iilink,4+iconf,iaa)=0.d0
        phi_lnk(istate,iilink,2,iaa)=-180.0
        phi_lnk(istate,iilink,3,iaa)= 180.0
        phi_lnk(istate,iilink,4,iaa)=  90.0
      endif
c.....End Test reset
      write(*,2209) string,
     >(phi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
     >,((phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
     >,(phi_lnk(istate,iilink,4+iconf,iaa),iaa=n+1,n+1)  !phi n+1 se c'e' e se serve
     >,((phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=n+1,n+1)
c.....writing
      write(*,2209) string,
     >(0.d0,iaa=1,n)
     >,(-pi*conv,pi*conv,pih*conv,iaa=1,n)
     >,(0.d0,iaa=1,1)
     >,(-pi*conv,pi*conv,pih*conv,iaa=1,1)
c.....deg -> rad
      do iaa=1,n+1
      phi_lnk(istate,iilink,4+iconf,iaa)= 
     >phi_lnk(istate,iilink,4+iconf,iaa)/conv
      do i=2,4
        phi_lnk(istate,iilink,i,iaa)= 
     >phi_lnk(istate,iilink,i,iaa) / conv
      enddo
      enddo
c      write(*,2209) string,
c     >(phi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
c     >,((phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
c      stop

      read(iu_dat,2209) string,
     >(psi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
     >,((psi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
c.....Test reset
      dum1=0.d0
      do iaa=1,n
        dum1=dum1+psi_lnk(istate,iilink,4+iconf,iaa)
        do i=2,4
          dum1=dum1+psi_lnk(istate,iilink,i,iaa)
        enddo
      enddo
      if (dum1.eq.0.d0) then ! reset psi angles
        do iaa=1,n
          psi_lnk(istate,iilink,4+iconf,iaa)=0.d0
          psi_lnk(istate,iilink,2,iaa)=-180.0
          psi_lnk(istate,iilink,3,iaa)= 180.0
          psi_lnk(istate,iilink,4,iaa)=  90.0
        enddo
      endif
c.....End Test reset
      write(*,2209) string,
     >(psi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
     >,((psi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
c.....writing
      write(*,2209) string,
     >(0.d0,iaa=1,n)
     >,(-pi*conv,pi*conv,pih*conv,iaa=1,n)
c.....deg -> rad
      do iaa=1,n
      psi_lnk(istate,iilink,4+iconf,iaa)= 
     >psi_lnk(istate,iilink,4+iconf,iaa)/conv
      do i=2,4
        psi_lnk(istate,iilink,i,iaa)= 
     >psi_lnk(istate,iilink,i,iaa) / conv
      enddo
      enddo
      read(iu_dat,2209) string,
     >(chi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
     >,((chi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
c.....Test reset
      dum1=0.d0
      do iaa=1,n
        dum1=dum1+chi_lnk(istate,iilink,4+iconf,iaa)
        do i=2,4
          dum1=dum1+chi_lnk(istate,iilink,i,iaa)
        enddo
      enddo
      if (dum1.eq.0.d0) then ! reset chi angles
        do iaa=1,n
          chi_lnk(istate,iilink,4+iconf,iaa)=0.d0
          chi_lnk(istate,iilink,2,iaa)=-180.0
          chi_lnk(istate,iilink,3,iaa)= 180.0
          chi_lnk(istate,iilink,4,iaa)=  90.0
        enddo
      endif
c.....End Test reset
      write(*,2209) string,
     >(chi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n)
     >,((chi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
c.....writing
      write(*,2209) string,
     >(0.d0,iaa=1,n)
     >,(-pi*conv,pi*conv,pih*conv,iaa=1,n)
c.....deg -> rad
      do iaa=1,n
      chi_lnk(istate,iilink,4+iconf,iaa)= 
     >chi_lnk(istate,iilink,4+iconf,iaa)/conv
      do i=2,4
        chi_lnk(istate,iilink,i,iaa)= 
     >chi_lnk(istate,iilink,i,iaa) / conv
      enddo
      enddo

      enddo! end iconf
      enddo! end iilink


      do isite=1,nsite(istate)
        write(*,*) isite,ilinksite(istate,isite,1),
     >(ilinksite(istate,isite,1+k),k=1,ilinksite(istate,isite,1))
      enddo


c.....2014 vediamo di inserire i constraints.
      do isitlnk=1,nsite(istate)+nlink(istate)
        isitlnkconstr_a(istate,isitlnk,1)=0 !azzera il numero di constraints (a) che insistono sul site isite-ilink-esimo
        isitlnkconstr_b(istate,isitlnk,1)=0 !azzera il numero di constraints (b) che insistono sul site isite-ilink-esimo
      enddo
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      if 
     >(string.eq."***************** Amino acid Constraints") then
        nc=0
        read(iu_dat,4100) string,filee
        write(*,4100) string,filee
        read(iu_dat,4100) string,filee
        write(*,4100) string,filee
 1415   read(iu_dat,"(a200)") text
        write(*,*) text
        read(text(1:4),"(i4)",err=1416) i1
        write(*,*) ' i1 ',i1
        if (i1.ne.0) then
          read(text(111:115),"(i5)") i2
          if (i2.eq.0) goto 1415
          nc=nc+1
          if (nc.gt.mxconstr) then
            write(*,*) "nc > mxconstr"
            stop
          endif
          do i=1,20 !e' un numero massimo
            iconstr_a(istate,nc,i)=0
          enddo
          do i=1,2
            jconstr_a(istate,nc,i)=0
          enddo
          vconstr_a(istate,nc)=0.d0
          fconstr_a(istate,nc)=0.d0
          cconstr_a(istate,nc)=0.d0

          i3=1

c AA |RD|SS|POL| Atom|Coord.vector r_i |     Condition     |Val,Name,Vec.u_d |Fact.Val.|Method|   K    |Name  |Flag
c    |FL|  |   |     |x,y,z,r,a,b,rc,ac|=,>,>=,<,<=,max,min|x,y,z,r,a,b,rc,ac|         |st,el |        |      |1/0   
c  1  2  3  4   5     6                       7               8,j=1  name j=2             9  
c                                                            val j=3 v         f                 c         l

ci:AA|RD|SS|POL|i:Atom|j:AA|RD|SS|POL|j:Atom|Coord.vector r_ij|     Condition     |Val,Name,Vec.u_dd|Fact.Val.|Method|   K    |Name  |Flag
c    |FL|  |   |      |    |FL|  |   |      |x,y,z,r,a,b,rc,ac|=,>,>=,<,<=,max,min|x,y,z,r,a,b,rc,ac|         |st,el |        |      |1/0
c  1  2  3  4   5      6    7  8  9     10      11                     12             13  name j=2               14
c                                                                                        val j=3 v       f               c        l
c#23456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678
c         1         2         3         4         5         6         7         8         9        10        11        12        13

          iconstr_a(istate,nc,i3)=i1
          read(text(6:7),"(i2)") i2
          if (i2.le.0.or.i2.gt.nsite(istate)+nlink(istate)) then
            write(*,*) "Constraints: RD/FL <=0 or > nsite+nlink"
     >,i2,nsite(istate),nlink(istate)
            stop
          endif
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          read(text(9:10),"(i2)") i2
          if (i2.le.0) i2=1
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          read(text(12:14),"(i3)") i2
          if (i2.le.0) i2=1
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          call findatype(text,16,20,i2)
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          call findcoord(text,22,38,i2)
          if (i2.eq.0) then
            write(*,*) "Constraint: wrong coordinate symbol"
            stop
          endif
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          call findcond(text,40,58,i2)
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          call findcoord(text,60,76,i2)
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          if (i2.eq.0) then
            i4=0
            do i=1,nconstr_a(istate)
              mm=lnfil(lconstr_a(istate,i),6)
              if (mm.gt.1) then
              do k=60,76-mm+1
                if (text(k:k+mm-1).eq.lconstr_a(istate,i)(1:mm)) then
                  jconstr_a(istate,nc,1)=2
                  jconstr_a(istate,nc,2)=i
                  i4=1
                endif
              enddo
              endif
            enddo
            if (i4.eq.0) then
              jconstr_a(istate,nc,1)=3
              read(text(60:76),"(g17.5E3)") vconstr_a(istate,nc)
            endif
          else
            jconstr_a(istate,nc,1)=1
          endif
          read(text(78:86),"(g9.2E3)") fconstr_a(istate,nc)
          if (fconstr_a(istate,nc).eq.0.d0) fconstr_a(istate,nc)=1.d0
          read(text(95:102),"(g9.2E3)") cconstr_a(istate,nc)
          if (cconstr_a(istate,nc).eq.0.d0) cconstr_a(istate,nc)=1.d0
          lconstr_a(istate,nc)='      '
          mm=0
          do i=104,109
            if (text(i:i).ne.' ') then
              mm=mm+1
              lconstr_a(istate,nc)(mm:mm)=text(i:i)
            endif
          enddo
          call findmeth(text,88,93,i2)
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          nconstr_a(istate)=nc
          write(*,*) ' i3 ',i3
          write(*,*) ' Constr_a.: ',nc,(iconstr_a(istate,nc,i2),i2=1,i3)
     >,jconstr_a(istate,nc,1),vconstr_a(istate,nc),fconstr_a(istate,nc)
     >,cconstr_a(istate,nc),lconstr_a(istate,nc)(1:6)
          goto 1415
        endif
      endif

 1416 if 
     >(text(1:40).eq."******** Pairs of Amino acid Constraints") then
        nc=0
        read(iu_dat,4100) string,filee
        write(*,4100) string,filee
        read(iu_dat,4100) string,filee
        write(*,4100) string,filee
 1417   read(iu_dat,"(a200)") text
        write(*,*) text
        read(text(1:4),"(i4)",err=1418) i1
        write(*,*) ' i1 ',i1
        if (i1.ne.0) then
          read(text(134:138),"(i5)") i2
          if (i2.eq.0) goto 1417
          nc=nc+1
          if (nc.gt.mxconstr) then
            write(*,*) "nc > mxconstr"
            stop
          endif
          do i=1,20
            iconstr_b(istate,nc,i)=0
          enddo
          do i=1,2
            jconstr_b(istate,nc,i)=0
          enddo
          vconstr_b(istate,nc)=0.d0
          fconstr_b(istate,nc)=0.d0
          cconstr_b(istate,nc)=0.d0
          i3=1
          iconstr_b(istate,nc,i3)=i1
          read(text(6:7),"(i2)") i2
          if (i2.le.0.or.i2.gt.nsite(istate)+nlink(istate)) then
            write(*,*) "Constraints: RD/FL <=0 or > nsite+nlink"
     >,i2,nsite(istate),nlink(istate)
            stop
          endif
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          read(text(9:10),"(i2)") i2
          if (i2.le.0) i2=1
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          read(text(12:14),"(i3)") i2
          if (i2.le.0) i2=1
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          call findatype(text,16,21,i2)
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2



          read(text(23:26),"(i4)",err=1418) i2
          if (i2.eq.0) then
            write(*,*) "Constraints: j:AA=0"
            stop
          endif
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          read(text(28:29),"(i2)") i2
          if (i2.le.0.or.i2.gt.nsite(istate)+nlink(istate)) then
            write(*,*) "Constraints: RD/FL <=0 or > nsite+nlink"
     >,i2,nsite(istate),nlink(istate)
            stop
          endif
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          read(text(31:32),"(i2)") i2
          if (i2.le.0) i2=1
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          read(text(34:36),"(i3)") i2
          if (i2.le.0) i2=1
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          call findatype(text,38,43,i2)
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2


          call findcoord(text,45,61,i2)
          if (i2.eq.0) then
            write(*,*) "Constraint: wrong coordinate symbol"
            stop
          endif
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          call findcond(text,63,81,i2)
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          call findcoord(text,83,99,i2)
          i3=i3+1
          iconstr_a(istate,nc,i3)=i2
          if (i2.eq.0) then
            i4=0
            do i=1,nconstr_b(istate)
              mm=lnfil(lconstr_b(istate,i),6)
              if (mm.gt.1) then
              do k=83,99-mm+1
                if (text(k:k+mm-1).eq.lconstr_b(istate,i)(1:mm)) then
                  jconstr_b(istate,nc,1)=2
                  jconstr_b(istate,nc,2)=i
                  i4=1
                endif
              enddo
              endif
            enddo
            if (i4.eq.0) then
              jconstr_b(istate,nc,1)=3
              read(text(83:99),"(g17.5E3)") vconstr_b(istate,nc)
            endif
          else
            jconstr_b(istate,nc,1)=1
          endif
          read(text(101:109),"(g9.2E3)") fconstr_b(istate,nc)
          if (fconstr_b(istate,nc).eq.0.d0) fconstr_b(istate,nc)=1.d0
          read(text(118:125),"(g9.2E3)") cconstr_b(istate,nc)
          if (cconstr_b(istate,nc).eq.0.d0) cconstr_b(istate,nc)=1.d0
          lconstr_b(istate,nc)='      '
          mm=0
          do i=127,132
            if (text(i:i).ne.' ') then
              mm=mm+1
              lconstr_b(istate,nc)(mm:mm)=text(i:i)
            endif
          enddo
          call findmeth(text,111,116,i2)
          i3=i3+1
          iconstr_b(istate,nc,i3)=i2
          nconstr_b(istate)=nc
          write(*,*) ' i3 ',i3
          write(*,*) ' Constr_b.: ',nc,(iconstr_b(istate,nc,i2),i2=1,i3)
     >,jconstr_b(istate,nc,1),vconstr_b(istate,nc),fconstr_b(istate,nc)
     >,cconstr_b(istate,nc),lconstr_b(istate,nc)(1:6)
          goto 1417
        endif
      endif

 1418 continue
c      write(*,*) 'qui'

c      if
c     >(string.eq."******** Pairs of Amino acid Constraints") then
c      endif
c.......................................................

      enddo! end istate

c.....ora dovresti trovare per ogni pdb quali sono gli AA che sono usati nei linker
      do ipdb=1,npdb
      naallpdb(ipdb)=0
      do istate=1,nstate
      do iilink=1,nlink(istate)
      do iend=1,2
        isite=isitelink(istate,iilink,2*iend-1)
        if (isite.eq.0) goto 1144
        iaa=isitelink(istate,iilink,2*iend)
        if (ipdbsite(istate,isite).eq.ipdb) then
          m=naallpdb(ipdb)
          do k=1,m
            if (iaallpdb(ipdb,k,1).eq.iaa) then
              iaallpdb(ipdb,k,2)=iaallpdb(ipdb,k,2)+1 !! un nuovo link sull'AA
              iaallpdb(ipdb,k,iaallpdb(ipdb,k,2)+2)=iilink
              isitelink(istate,iilink,4+iend)=k !! indica il numero progessivo di AA del pdb a cui il linker e' legato secondo iend
              goto 1144
            endif
          enddo
          m=m+1
c.........iaallpdb == indice dell' m-essimo AA del ipdb-esimo pdb attaccato a un linker
          if (m.gt.mxlinkpdb) stop 'mxlinkpdb'
          naallpdb(ipdb)=m !numero progressivo di AA usati come link
          iaallpdb(ipdb,m,1)=iaa !aa
          iaallpdb(ipdb,m,2)=1  !primo link su AA
          iaallpdb(ipdb,m,3)=iilink !codice link 
          isitelink(istate,iilink,4+iend)=m !! indica il numero progessivo di AA del pdb a cui il linker e' legato secondo iend
        endif
 1144   continue
      enddo !iend
      enddo !iilink
      enddo !istate
      do m=1,naallpdb(ipdb)
        write(*,*) iaallpdb(ipdb,m,1),
     >  (iaallpdb(ipdb,m,2+k),k=1,iaallpdb(ipdb,m,2))
      enddo
      enddo !ipdb


      read(iu_dat,4100) string,filee
      write(*,4100) string,filee

      do j=1,mxagg
        read(iu_dat,4500) ch1,(ch8,iflagg(i,j),ch2,i=1,j-1)
        write(*,4500) ch1,(ch8,iflagg(i,j),ch2,i=1,j-1)
      enddo 
c      stop !per ora
      i1=0
      i2=1
      do j=1,nagg
        do i=1,j-1
          i1=i1+iflagg(i,j)
          if (iflagg(i,j).eq.1) then
            i2=i2+j
          endif
        enddo
      enddo
      if (i1+1.ne.nagg) stop 'i1+1=/nagg' !numero dei link dev'essere il numero degli agg-1
      if (i2.ne.nagg*(nagg+1)*0.5) stop 'i2=/nagg*(nagg+1)/2' !la somma degli indici degli aggregati

      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee

      read(iu_dat,2100) string,
     >nexper
      write(*,2100) string,
     >nexper
      write(iu_out,2100) string,
     >nexper
      if (nexper.gt.mxexper) stop 'nexper>mxexper'
      nwave=0
      nfamily=0
      do nn=1,nexper
      read(iu_dat,4100) string,filee
      write(*,4100) string,filee
      read(iu_dat,4100) string,
     >filexp(nn)
      write(*,4100) string,
     >filexp(nn)
      read(iu_dat,4100) string,
     >desexp
      write(*,4100) string,
     >desexp
      read(iu_dat,2200) string,
     >concp(nn)
      write(*,2200) string,
     >concp(nn)
      read(iu_dat,2200) string,
     >tempexp(nn)
      write(*,2200) string,
     >tempexp(nn)
      read(iu_dat,2200) string,
     >presexp(nn)
      write(*,2200) string,
     >presexp(nn)
      do k=1,mxeffec
      read(iu_dat,2200) string,
     >effexp(nn,k)
      write(*,2200) string,
     >effexp(nn,k)
      if (effexp(nn,k).ne.zer) neffec=k
      effave(k)=effave(k)+effexp(nn,k)
      enddo
      read(iu_dat,2200) string,
     >xd2o(nn)
      write(*,2200) string,
     >xd2o(nn)
      read(iu_dat,2200) string,
     >polh(nn)
      write(*,2200) string,
     >polh(nn)
      read(iu_dat,2200) string,
     >pold(nn)
      write(*,2200) string,
     >pold(nn)
      read(iu_dat,2200) string,
     >rho0(nn)
      write(*,2200) string,
     >rho0(nn)
      read(iu_dat,2200) string,
     >dum1
      write(*,2200) string,
     >dum1
      do iw=1,nwave
        if (dum1.ne.0.d0.and.dum1.eq.wave(iw)) then
          iwave(nn)=iw
          goto 97
        endif
        nwave=nwave+1
        if (nwave.gt.mxwave) stop 'nwave.gt.mxwave'
        wave(nwave)=dum1
        iwave(nn)=nwave
      enddo
 97   continue
      read(iu_dat,2200) string,
     >aqrmin(nn)
      write(*,2200) string,
     >aqrmin(nn)
      read(iu_dat,2200) string,
     >aqrmax(nn)
      write(*,2200) string,
     >aqrmax(nn)
      read(iu_dat,2200) string,
     >errkk(nn)
      write(*,2200) string,
     >errkk(nn)
      read(iu_dat,2100) string,
     >nskpp(nn)
      write(*,2100) string,
     >nskpp(nn)
      read(iu_dat,2100) string,
     >nqav(nn)
      write(*,2100) string,
     >nqav(nn)
      read(iu_dat,2100) string,
     >iqmv(nn)
      write(*,2100) string,
     >iqmv(nn)
      read(iu_dat,2200) string,
     >dprmax(nn)
      write(*,2200) string,
     >dprmax(nn)
      read(iu_dat,2200) string,
     >qipar(nn)
      write(*,2200) string,
     >qipar(nn)
      iqipar(nn)=1
      if (qipar(nn).ne.0.) iqipar(nn)=0
       read(iu_dat,2100) string,
     >iplate(nn)
      write(*,2100) string,
     >iplate(nn)
      read(iu_dat,2100) string,
     >ifamily(nn)
      write(*,2100) string,
     >ifamily(nn)
      nfamily=max(nfamily,ifamily(nn))
      if (nfamily.gt.mxfamily) stop 'nfamily>mxfamily'

      enddo ! end nexper
      if (nfamily.eq.0) nfamily=1
 
      close(unit=iu_dat)

      if (nexper.gt.0) then
      do k=1,mxeffec
        effave(k)=effave(k)/nexper  !! valore medio effettore
      enddo
      endif

      neffec=mxeffec
c      do nn=1,nexper
c        do k=1,neffec
c          if (effexp(nn,k).eq.0.0) stop 'effexp(nn,k).eq.0.0'
c        enddo
c      enddo

c      stop

4100  format(a40,a300)
2100  format(a40,i10)
2101  format(a40,a4)
2200  format(a40,g15.5E3)
2207  format(a40,4(g15.5E3,1x),i4,1x,a10000)
 2209 format(a40,50000(g15.5E3,1x))
 4500 format(a1,100(a8,i1,a2))

      call ranin(iseed)
c               write(*,*) ' aa 01 ci sono'
      call opsymm
      call check_constr
c               write(*,*) ' aa 02 ci sono'


c      stop
c.....Calculus functions for integrals
      nqgl=64
      call integ

c               write(*,*) ' aa 06 ci sono'
c.....test linkage
c      call testlink !!! non mi pare che ora serva


c               write(*,*) ' aa 07 ci sono'
c.....A^{(i)}_{l,m}(Q) for any PDB!
c      rtramax(ipdb)=-1.d30
c      rtramin(ipdb)=+1.d30


      qmin=0
      dq=((qmfull)-(qmin))/float(nqfull-1)
      q=(qmin)-dq
      do iq=1,nqfull
        q=q+dq
        qqfull(iq)=(q)
      enddo

      do ipdb=1,npdb
        call fullpdb(ipdb)
        do istate=1,nstate
          do isite=1,nsite(istate)
c         write(*,*) 'AAA',ipdb,ipdbsite(istate,isite),jums(istate,isite)
c     >,ums(istate,isite,1)
            if (ipdb.eq.ipdbsite(istate,isite)) then
              if (jums(istate,isite).eq.-1) then
                call ca_refiniment(istate,isite)
              endif
            endif
          enddo !isite
        enddo !istate
c               write(*,*) ' aa 08 ci sono'
c      write(*,*) rtramin(ipdb),rtramax(ipdb)
c      do i=1,nato_ca
c        write(75+ipdb,*) (coord(inato_ca(i),k),k=1,3)
c      enddo
      qglmax=40*pi/(2*rtramax(ipdb)) !!40 shannon's channels!!!!!!!(ltird+1)*pi/4./rtramin(ipdb) ! e dove l'hai trovata?
      rglmax=rtramax(ipdb)
      call integ
        call shapepdb(ipdb)
c               write(*,*) ' bb 08 ci sono'
      enddo
      call shapech3

c.....le prossime righe le potrai cancellare
c      do istate=1,nstate
c      do iconf=1,nconf(istate)
c      do iilink=1,nlink(istate)
c        call linkerendsfind(istate,iilink,iconf) !e' nuovo
c      enddo
c      enddo
c      enddo

c      stop !per ora

c.....pezzo precedente che usava la shape function
c      do ipdb=1,npdb
c        call fullpdb(ipdb)
c
c               write(*,*) ' aa 08 ci sono'
c      enddo
c      write(*,*) rtramin,rtramax
c      qglmax=(ltird+1)*pi/4./rtramin
c      rglmax=rtramax
c      call integ
c      do ipdb=1,npdb
c        call shapepdb_old(ipdb)
c               write(*,*) ' bb 08 ci sono'
c      enddo
c.....fine pezzo precedente che usava la shape function

c               write(*,*) ' aa 09 ci sono'
      do istate=1,nstate
        mlink(istate)=0 !!!! preferisco fissarlo cosi'
        dum1=0.d0
        do isite=1,nsite(istate)
          ipdb=ipdbsite(istate,isite)
          dum1=dum1+wfull(ipdb)
        enddo
        do ihagg=1,nhagg(istate)
          wagg(istate,ihagg)=dum1*nsymm(istate,ihagg)
     >*npoly(istate,ihagg)
          if (wagg(istate,ihagg).eq.0) stop 'wagg(istate,ihagg)=0'
          if (mlink(istate).eq.1) then 
            agg(jaghag(istate,ihagg))=nsymm(istate,ihagg)*nsite(istate)
     >*npoly(istate,ihagg)!! qui non ci va piu'
          else
            agg(jaghag(istate,ihagg))=nsymm(istate,ihagg)
     >*npoly(istate,ihagg) !! quindi e' il numero di aggregazione che ha come unita' gli FU e gli FL dello stato istate
          endif
c          write(*,*) ' GG ',
c     >istate,ihagg,jaghag(istate,ihagg),wagg(istate,ihagg)
c     >,agg(jaghag(istate,ihagg))
        enddo
      enddo
c      stop

      if (jfrac.eq.1) then
      open (unit=iu_dat,status='unknown',file='qua'//code//'.xfrac')
      do iagg=1,nagg!!!-1
        read(iu_dat,1212,err=1213,end=1213) 
     >  i1,
     >  ((xfrac(iagg,nn,k),k=1,4,3),nn=1,nexper)
      enddo 
      goto 1214
 1212 format(1(i4),8000(e15.6E3))
 1213 continue
      call xfraccalc_first
 1214 close(unit=iu_dat)
      endif

      call mparcalc !c.....Individua i parametri da minimizzare!! per ora non l'ho modificato

      if (mpar.gt.mxpar) stop 'mxpar'
      call allopen

      do istate=1,nstate
      do iconf=1,nconf(istate)
      do iilink=1,nlink(istate)
        call linkerendsfind(istate,iilink,iconf) !e' nuovo
c        call hingefind(istate,1)
      enddo
      enddo
      enddo
c               write(*,*) ' aa 10 ci sono'

      
      if (w_overl.ne.zer) then
        do ipdb1=1,npdb
          do ipdb2=ipdb1,npdb
c        do ipdb1=npdb,npdb
c          do ipdb2=ipdb1,npdb
            if (ifsig.eq.1) then
c              l2flag=0
              call sigcalc(ipdb1,ipdb2)
c              call sigcalc_wr0(ipdb1,ipdb2)
c              call sigcalc_old(ipdb1,ipdb2)
c              call sigcalc_total(ipdb1,ipdb2)
            endif
            if (isigplot.eq.1) call sigplot(ipdb1,ipdb2)
            call ovldstrsc_limits(ipdb1,ipdb2)
          enddo
c.........con il ch3
            if (ifsig.eq.1) then
              call sigcalc(ipdb1,npdb+1)
              call ovldstrsc_limits(ipdb1,npdb+1)
c              call sigcalc_wr0(ipdb1,ipdb2)
c              call sigcalc_old(ipdb1,ipdb2)
c              call sigcalc_total(ipdb1,ipdb2)
            endif
c.non e' il caso            if (isigplot.eq.1) call sigplot(ipdb1,npdb+1)
c. fine con il ch3

        enddo
      endif

cc      stop !! per ora

c.....Lettura dati sperimentali
      do nn=1,nexper
        qrmin=aqrmin(nn)
        qrmax=aqrmax(nn)
        nskpr=nskpp(nn)
        errk=errkk(nn)
        nqave=nqav(nn)
        jmouve=iqmv(nn)
        filee    = filexp(nn)
c               write(*,*) ' aa 11 ci sono'
        call rexp(filee,nskpr,qrmin,qrmax,mnexp,chifat,nq,nqr,qq,qie,
     >qle,qid,wi,qies,qles,qmin,qmax,scale,errk,nqave,jmouve,alphapower,
     >icolumn3,qinputscale,sinputscale)

        nqn(nn)=0
        nqn1(nn)=0
c.......Interpolo tutti i dati exp dentro un'unica griglia
        do iq=1,nqfull
          if (qqfull(iq).ge.qq(1)) then
            nqn1(nn)=iq
            goto 78
          endif
        enddo
 78     do iq=1,nqfull
          if (qqfull(iq).gt.qq(nqr)) then
            nqn(nn)=iq-1
            goto 788
          endif
        enddo
 788    if (nqn1(nn).eq.0) stop 'nqn1(nn).eq.0'
        if (nqn(nn).eq.0) stop 'nqn(nn).eq.0'
        do iq=nqn1(nn),nqn(nn)
c          call intpar(qqfull(iq),qien(nn,iq),nqr,qq,qie)
c          call intpar(qqfull(iq),qidn(nn,iq),nqr,qq,qid)
c          call intpar(qqfull(iq),win(nn,iq),nq,qq,wi)
          call intlin(qqfull(iq),qien(nn,iq),nqr,qq,qie)
          call intlin(qqfull(iq),qidn(nn,iq),nqr,qq,qid)
          call intlin(qqfull(iq),win(nn,iq),nq,qq,wi)
          qqn(nn,iq)=qqfull(iq)
c          write(*,*) 'CC',qqfull(iq),qien(nn,iq)
        enddo
        nqrn(nn)=nqn(nn)
        chipar(nn)=chifat

c        do iq=1,nq
c          qqn(nn,iq)=qq(iq)
c          qien(nn,iq)=qie(iq)
c          qidn(nn,iq)=qid(iq)
c          win(nn,iq)=wi(iq)
c        enddo
c
c        nqn(nn)=nqr
c        nqrn(nn)=nqr
c        chipar(nn)=chifat

      enddo

c               write(*,*) ' aa 12 ci sono'
      call pntdef
      call alltt


c      open(unit=1,file='/sas/quafit_test.pdb')
      do istate=1,nstate
      do iconf=1,nconf(istate)
      do iilink=1,nlink(istate)
      call linkerendsfind(istate,iilink,iconf) 
      call linkercoo(0,istate,iilink,iconf)  
      call linkerpdb(istate,iilink,iconf)
      call fulllinker(istate,iilink,iconf)
      enddo
      enddo
      enddo
c      close(1)

      subcode='inp'
      do istate=1,nstate
c######################################################################
c        call framhag(istate,1,1)
        if (icheckhag.eq.0) then
          call pairs_equiv_new(istate,1) !!!! prima del 9 maggio 2012 era solo cosi'
        else
          do ihagg=1,nhagg(istate)
            call pairs_equiv_new(istate,ihagg)
          enddo
        endif
c######################################################################
        do ihagg=1,nhagg(istate)
          do iconf=1,nconf(istate)
c               write(*,*) ' aa 13 ci sono'
ccc            call framhag(istate,ihagg,iconf)
c            call framhag_old(istate,ihagg,iconf)
c            write(*,*) "HHHHHHHHHHHHH"
            call framhag(istate,ihagg,iconf)
c            stop
            call framalllink(istate,ihagg,iconf)
c               write(*,*) ' aa 14 ci sono'
            call btoms(istate,ihagg,iconf)
c               write(*,*) ' aa 15 ci sono'
          enddo
c.old          if (ihagg.eq.1) call pairs_equiv(istate,ihagg)
c          write(*,*) ' aa 151 ci sono'
        enddo
      enddo

c      dum1=0.d0
c      call test_disv(1,1,1)
c      write(*,*) ' ov ',overl(1,1,1)
c      do isite=1,nsite(1)
c        write(*,*) overl_site(1,isite,1,1)
c        overm_site(1,isite,1,1)=overl_site(1,isite,1,1)
c      enddo
c      do isite=1,nsite(1)
c        ums(1,isite,4+1)=ums(1,isite,4+1)+10.
c        call framhag(isite,1,1)
c        call test_disv_site(1,isite,1,1)
c        overl(1,1,1)=overl(1,1,1)-overm_site(1,isite,1,1)
c     > +overl_site(1,isite,1,1)
cc.......sposto isite e quindi tutta una serie di coppie si muovono overl_site(...isite..)
cc ora sposto un'altro isite: si muovono  altre coppie, ma anche alcune che si erano mosse prima. quindi overm del nuovo sito non e' quello che era rimasto in memoria... forse e' meglio memorizzare overlap di tutte le coppie, una ad una. Guarda sopra, in realta' cambiano tutte le altre per un solo contributo.
c        write(*,*) ' ov2 ',overl(1,1,1)
c        call test_disv_2(1,1,1)
c        write(*,*) ' ov3 ',overl(1,1,1)
c      enddo
c      stop


c               write(*,*) ' YY 01 ci sono'
c      call chis2(f)
c               write(*,*) ' YY 02 ci sono'
c      fnew=f
c               write(*,*) ' YY 03 ci sono'


      do istate=1,nstate
        dum1=0.d0
        do isite=1,nsite(istate)
          ipdb=ipdbsite(istate,isite)
          dum1=dum1+wfull(ipdb)
        enddo
        do iilink=1,nlink(istate)
          dum1=dum1+wlink(istate,iilink)
        enddo
        do ihagg=1,nhagg(istate)
          wagg(istate,ihagg)=dum1*nsymm(istate,ihagg)
     >*npoly(istate,ihagg) !! e' il peso molecolare che comprende sia FU che FL
          if (mlink(istate).eq.1) then
            agg(jaghag(istate,ihagg))=nsymm(istate,ihagg)*nsite(istate)
     >*npoly(istate,ihagg)
          else
            agg(jaghag(istate,ihagg))=nsymm(istate,ihagg)
     >*npoly(istate,ihagg)
          endif
c          write(*,*) ' GG2  ',
c     >istate,ihagg,jaghag(istate,ihagg),wagg(istate,ihagg)
c     >,agg(jaghag(istate,ihagg))
        enddo
      enddo
c      stop



      i1=0
      do istate=1,nstate
        do ihagg=1,nhagg(istate)
          do iconf=1,nconf(istate)
            do isite=1,nsite(istate)
              i1=i1+1
              i2=isite+nsite(istate)*(iconf-1+nconf(istate)*
     >           (ihagg-1+nhagg(istate)*(istate-1)))
c              write(*,*) i1,i2
            enddo
            enddo
            enddo
            enddo
c       stop


      
c      call framhag(1,1,1)
c      call test_disv(1,1,1,nsite(1),1,dum1)
c      write(*,*) dum1      
c      do isite=3,3
c        do k=1,3
c            centre(isite,1,1,k)= centre(isite,1,1,k)+2.*k
c        enddo
c      enddo
c      alprot(3,1,1)=pi*0.3
c      betrot(3,1,1)=-pi*0.1
c      gamrot(3,1,1)=pi*0.4
c      call test_disv(1,1,1,nsite(1),1,dum1)
c      write(*,*) dum1      
c      stop

c      call framhag(1,1,1)
c      call test_disv(1,1,1,nsite(1),1,dum1)
c      write(31,*) dum1
c      do isite=1,nsite(1)
c        write(31,*) isite,overl_site(1,isite,1,1)
c      enddo
c      do isite=1,nsite(1)
c        call test_disv(1,1,isite,isite,1,dum2)
c      write(31,*) isite,dum2,overl_site(1,isite,1,1)
c      write(31,*) dum1-dum2+overl_site(1,isite,1,1)
cc      write(31,*) (overl_site(1,jsite,1,1),jsite=1,nsite(1))
c      enddo
c      stop

      

c      stop !per ora


      if (i2confr.eq.1) then
        call confr_2_states
      else
        call mcarlo  !!! stai ragionando qui dentro
c               write(*,*) ' YY 08 ci sono'
      endif
      close(unit=iu_out)
      call allclose
      stop

7500  format('  Maximum Number of Fits and Threshold .:',2 (1x,i4))
cFormato di Sandro
c4200  format(1x,e13.6,2(2x,e13.6))
4201  format(1x,e13.6,10(2x,e13.6))
cFormato hexkinase
4200  format(1000(e15.6))
cFormato BLG
c4200  format(10(e15.6E3))
cFormato di Paolo
c4200  format(10(f8.6,1x))
cFormato di model.for
c4200  format(e15.6E3,15x,2(e15.6E3))
cFormato ROSA
c4200  format(10(e15.6E3))
c.....formato monaco BLG
c4200  format(10(e10.4,2x))
c.....Formato rosangela
c 4200 format(3(e12.6,1x))
1000  format(50(e15.6E3))
 3201 format(
     >' Radius of gyration of particle (Ang) .... :',2(f10.2),/,
     >' Volume of particle (Ang.^3) ............. :',2(f10.2))
2000  format(18x,2(e15.6E3))
2900  format('  Simulated a[l,m]:')
2901  format('  Calculated f[l,m]:')
2902  format('  Fitted a[l,m]:')
2903  format('  Calculated f[l,m]:')
3000  format(
     >' RD #..................................:',i3,/,
     >'    Point group of aggregate (a4) .....:',a,/,
     >'    Main Unit pdb File (without .pdb) .:',a,/,
     >'    Stone Exp. Contact Distance file ..:',a,/,
     >'    Geom. Center Vect.: modulus (ang.) :',2(f10.5),/,
     >'                        alpha   (deg) .:',2(f10.5),/,
     >'                        beta    (deg) .:',2(f10.5),/,
     >'    Rotation Angles:    alpha_a (deg) .:',2(f10.5),/,
     >'                        beta_a  (deg) .:',2(f10.5),/,
     >'                        gamma_a (deg) .:',2(f10.5))
3001  format(1x,a2,'    a(',i2,',',i2,') = ',4(e15.6E3,1x))
3003  format(1x,a2,'   a^(',i2,',',i2,') = ',4(e15.6E3,1x))
3002  format(1x,a2,' 2 a^(',i2,',',i2,') = ',4(e15.6E3,1x))
1200  format('  Hypotetical Symmetry of Particle .....:',10(1x,a3))
1300  format('  Number of Independent Parameters .....:',i10)
3600  format('  SIMPLEX Calls To Routine CHISQ .......:',i10)
3700  format('  ZXMIN Calls To Routine CHISQ .........:',i10)
3800  format('  Achieved Convergence; Error Flag .....:',i10)
3900  format('  Reduced Chi Square Value .............:',e10.3)
4000  format('  Reduced Chi Square Gradient Norm .....:',e10.3)
4001  format('  Scale Factor kappa ...................:',e10.3)
4002  format('  Flat Background B ....................:',e10.3)
4003  format('  Scale Factor of Deoxy data ...........:',e10.3)
4141  format(
     >'  Input file of I(Q) data ..............:',a70,/,
     >'  Input file of a[l,m] for simulation ..:',a70,/,
     >'  Maximum MONKEY  iterations number ....:',i10,/,
     >'  Maximum SIMPLEX iterations number ....:',i10,/,
     >'  Maximum ZXMIN   iterations number ....:',i10,/,
     >'  MONKEY  minimization flag (1/0) ......:',i10,/,
     >'  SIMPLEX minimization flag (1/0) ......:',i10,/,
     >'  ZXMIN   minimization flag (1/0) ......:',i10,/,
     >'  Point group of maximum symmetry (a3) .:',a4,/,
     >'  Point group of minimum symmetry (a3) .:',a4,/,
     >'  Maximum number M of parameters a[l,m] :',i10,/,
     >'  Width of transition layer (Angs.) ....:',g10.4,/,
     >'  Iterations number for error analysis .:',i10,/,
     >'  Parameter t of slit length effect ....:',g10.4,/,
     >'  Particle volume (Angs.**3) ...........:',g10.4,/,
     >'  Error on particle volume (Angs.**3) ..:',g10.4,/,
     >'  Maximum rank l for a[l,m] parameters .:',i10,/,
     >'  Maximum index p for d[l,p] parameters :',i10,/,
     >'  Parameter t for maximum a[l,m]/a[0,0] :',g10.4,/,
     >'  Parameter k for poissonian errors ....:',g10.4,/,
     >'  Threshold tau for chi2 acceptance ....:',g10.4,/,
     >'  Mininum Q in data file (Angs.^-1).....:',g10.4,/,
     >'  Maximum Q in data file (Angs.^-1).....:',g10.4,/,
     >'  Number of lines to skip in data file .:',i10,/,
     >'  Number of zeros for integration ......:',i10,/,
     >'  Number of bins for integration .......:',i10,/,
     >'  Number of points for 3D plots ........:',i10,/,
     >'  Rotation of input F(w), alpha (deg) ..:',g10.4,/,
     >'  Rotation of input F(w), beta  (deg) ..:',g10.4,/,
     >'  Rotation of input F(w), gamma (deg) ..:',g10.4,/,
     >'  Maximum rank l for f[l,m] parameters .:',i10)
      end



      subroutine sigcalc(ipdb1,ipdb2)

      include 'quafit2014.cmn'
      complex*16 dum1,dum2,dum3,dum4,dum61,dum62,dum63
      complex*16 dum31,dum32,dum33
      external ovexp
      dimension x(3)
      dimension xl(3),xh(3),g(3)

      write(*,'(100(a,i3))') ' Calculating Sigma RD # ',ipdb1,'-',ipdb2
      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  
     >zer,zer,zer,0,0,0,0,0)
c      write(*,*) 'v12(0) ',dcut  !!! lc ltird=15 viene molto, molto bene
c      pause
c      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  !! previous definition
c     >zer,zer,zer,0,0,0,0,0)/1000
c      dcut=1000.d0 !! provo cosi' non mi piace, torno al valore di prima
      dcut=30.d0
c      write(*,*) 'dcut ',dcut

      r1=rtramin(ipdb1)+rtramin(ipdb2)
      r2=rtramax(ipdb1)+rtramax(ipdb2)
      n=20!20
      dr=(r2-r1)/(n-1)

      l2max=ltird
c      if (l2flag.eq.0) l2max=0
c.....Memorizzo psi in una griglia di r
      r=r1-dr
      do ir=1,n
        r=r+dr
        i1=0
        do l1=0,ltird
          do l2=0,l2max
            lmax3=l1+l2
            if (lmax3.gt.ltird) lmax3=ltird
            nl3=iabs(l2-l1)
            do l3=nl3,lmax3
              dum0=cg(l1,l2,l3,0,0)
              if (dum0.ne.zer) then
              do k1=-l1,l1
              do k2=-l2,l2
                i1=i1+1
                if (i1.gt.mxpsi) stop 'i1>mxpsi'
                call psi_s(ipdb1,ipdb2,l1,l2,l3,k1,k2,r,psi)
                psiar(i1,ir)=psi
c                write(91,*) r,psiar(i1,ir)
              enddo
              enddo
              endif
            enddo
          enddo
        enddo
      enddo

c      stop
c      write(*,*) ' 55 1 ci sono'

      do l2=0,l2max
      do l3=0,ltird
      do k2=-l2,l2
      do k3=-l3,l3
      do m2=-l2,l2
        sigbar(l2,l3,k2,k3,m2)=dcmplx(zer,zer)
        ovlbar(l2,l3,k2,k3,m2)=dcmplx(zer,zer)
        dstbar(l2,l3,k2,k3,m2)=dcmplx(zer,zer)
        rscbar(l2,l3,k2,k3,m2)=dcmplx(zer,zer)
      enddo
      enddo
      enddo
      enddo
      enddo

      i1=nz
      i2=nd
c      nz=4
c      nd=1
c      nz=8
c      nd=1

c.....normalmente ...
      nz=4
      nd=3
cc.....fine normalmente ...
c      nz=1
c      nd=3

      call gauss0

c.....Memorizzo pwar
      do ib=1,np
        dum7=dacos(zero(ib))
        call pw_go_mn(ltird,dum7,ib)
      enddo      

c.plot      iuu=70

      do ia2=1,np          !!! tengo fisso omega1, si!!
      a2=pi*(zero(ia2)+one)
      do ib2=1,np
      cb2=zero(ib2)
      b2=dacos(cb2)
      do ig2=1,np
      g2=pi*(zero(ig2)+one)
      do iar=1,np
      ar=pi*(zero(iar)+one)
      do ibr=1,np
      cbr=zero(ibr)
      br=dacos(cbr)
      r=r1-dr


c.....nuovo metodo
c.plot      iuu=iuu+1
      do ir=1,n
        r=r+dr
        dum5=overlap(ipdb1,ipdb2,
     >zer,zer,zer,a2,b2,g2,ar,br,r,1,ir,1,ib2,ibr)
        qie(ir)=dum5
        qis(ir)=r
      enddo
      qi(1)=qie(1)/1.2
      qi(2)=qie(1)*1.2
      qi(3)=(r2-r1)/100
      qi(4)=(r2-r1)
      qi(5)=r1
      qi(6)=r2
      x(1)=xscinv(qie(1),qi(1),qi(2))
      x(2)=xscinv((r2-r1)/5,qi(3),qi(4))
      x(3)=xscinv((r1+r2)/2,qi(5),qi(6))
      call ovexp(3,x,f)

      do ii=1,3
        x(ii)=zer
        xl(ii)=-pih
        xh(ii)=pih
        g(ii)=pih/3!0.1
      enddo
      ncicl=10
      nsub=20
      ti=f
      tf=f*1.e-3
      call sian(ovexp,3,x,xl,xh,g,0.5d0,f,ti,tf,ncicl,nsub,
     >       tm,am,iseed)

      call simplx(ovexp,x,f,3,1.d-5,itc,1000)
      a=xscale(x(1),qi(1),qi(2))
      dr0=xscale(x(2),qi(3),qi(4))
      r0=xscale(x(3),qi(5),qi(6))
c      write(*,*) a,dr0,r0,(qi(kkk),kkk=1,6)
c      write(*,*) (x(kkk),kkk=1,3),f

c.plot      do ir=1,n
c.plot        dum5=qie(ir)
c.plot        r=qis(ir)
c.plot        write(iuu,*) r, dum5,a*exp(-(r-r0)/dr0)/(1+exp(-(r-r0)/dr0))
c.plot      enddo

c      write(*,*) a,dr0,r0
      p=0.05 !S2_new era 0.05
      p=dcut/a !! nuovo ma non ora
      p=0.02 !S2_new era 0.05
      p=0.035 !S2_new era 0.05
      p=0.04 !S2_new era 0.05
      r=r0-dr0*log(-p/(p-1))
c      pause      

c.....precedente metodo
c      do 10 ir=1,n
c        r=r+dr
c        dum5=overlap(ipdb1,ipdb2,
c     >zer,zer,zer,a2,b2,g2,ar,br,r,1,ir,1,ib2,ibr)
cc        write(29,*) r, dum5
c        if (dum5.le.dcut) goto 20
c        dum7=dum5
c10    continue
c20    if (dum5.ne.dum7) r=r-dr+dr/(dum5-dum7)*(dcut-dum7) !interpolazione lineare
cc      write(*,*) r
cc      pause
cc.log      dum6=dcmplx(dlog(r),zer)
c.....fine precedente metodo

      dum61=dcmplx(a,zer)
      dum62=dcmplx(r0,zer)
      dum63=dcmplx(dr0,zer)
      
      dum6=weight(ia2)
     >    *weight(ib2)
     >    *weight(ig2)
     >    *weight(iar)
     >    *weight(ibr)
c      write(*,*) ia1,ib1,ig1,ia2,ib2,ig2
      do l2=0,l2max
      do l3=0,ltird
      dum0=dfloat((2*l2+1)*(2*l3+1))/32  !il pi**3 va via per l'integrazione di gauss-legendre
      do k2=-l2,l2
      do k3=-l3,l3
      do m2=-l2,l2
        dum3=      
     >       dcmplx(dcos(m2*a2), dsin(m2*a2))*
     >       dcmplx(dcos(k2*g2), dsin(k2*g2))*
     >       dcmplx(dcos(k3*ar), dsin(k3*ar))*
     >          pwar(l2,m2,k2,ib2)*
     >          pwar(l3,k3,0 ,ibr)*dum0*dum6
        ovlbar(l2,l3,k2,k3,m2)=ovlbar(l2,l3,k2,k3,m2)
     >    +dum61*dum3
        dstbar(l2,l3,k2,k3,m2)=dstbar(l2,l3,k2,k3,m2)
     >    +dum62*dum3
        rscbar(l2,l3,k2,k3,m2)=rscbar(l2,l3,k2,k3,m2)
     >    +dum63*dum3
 33     continue
      enddo
      enddo
      enddo
      enddo
      enddo

      enddo
      enddo
      enddo
      enddo
      enddo



      do l1=0,ltird
      do l2=0,l2max
      lmax3=l1+l2
      if (lmax3.gt.ltird) lmax3=ltird
      nl3=iabs(l2-l1)
      do l3=nl3,lmax3
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.ne.zer) then
      dum0=dsqrt(dfloat(2*l1+1))*((dcmplx(zer,one))**(l2-l1-l3))
      do k1=-l1,l1
      do k2=-l2,l2
        dum31=dcmplx(zer,zer)
        dum32=dcmplx(zer,zer)
        dum33=dcmplx(zer,zer)
        do m2=-l2,l2
          dum5=cg(l2,l3,l1,m2,k1-m2)
          dum31=dum31+dum5*ovlbar(l2,l3,k2,m2-k1,-m2)
          dum32=dum32+dum5*dstbar(l2,l3,k2,m2-k1,-m2)
          dum33=dum33+dum5*rscbar(l2,l3,k2,m2-k1,-m2)
        enddo
        dum7=dum0*(-1)**(k1+l2)
        ovl(l1,l2,l3,k1,k2)=dum31*dum7
        dst(l1,l2,l3,k1,k2)=dum32*dum7
        rsc(l1,l2,l3,k1,k2)=dum33*dum7
      enddo
      enddo
      endif
      enddo
      enddo
      enddo



      write(ch3(1:2),'(i2)') ipdb1
      write(ch2(1:2),'(i2)') ipdb2
      if (ch3(1:1).eq.' ') ch3(1:1)='0'
      if (ch2(1:1).eq.' ') ch2(1:1)='0'
      open (unit=iu_sig,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//ch3(1:2)//ch2//'.sig')

      do l1=0,ltird
      do l2=0,l2max
      lmax3=l1+l2
      if (lmax3.gt.ltird) lmax3=ltird
      nl3=iabs(l2-l1)
      do l3=nl3,lmax3
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.ne.zer) then
      do k1=-l1,l1
      do k2=-l2,l2
        write(iu_sig,1000) l1,l2,l3,k1,k2
     >,ovl(l1,l2,l3,k1,k2)
     >,dst(l1,l2,l3,k1,k2)
     >,rsc(l1,l2,l3,k1,k2)
      enddo
      enddo
      endif
      enddo
      enddo
      enddo

cc.....Test finale
c      dum2=dcmplx(zer,zer)
c      dum3=dcmplx(zer,zer)
c      dum4=dcmplx(zer,zer)
c      do l1=0,ltird
c      do l2=0,l2max
c      do 100 l3=0,ltird
c      dum0=cg(l1,l2,l3,0,0)
c      if (dum0.eq.zer) goto 100
c      do k1=-l1,l1
c      do k2=-l2,l2
c        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,one,
c     >               0,0,0,stone)
c        dum2=dum2+sig(l1,l2,l3,k1,k2)
c     > *dconjg(stone)
c        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,zer,zer,zer,one,
c     >               0,0,0,stone)
c        dum3=dum3+sig(l1,l2,l3,k1,k2)
c     > *dconjg(stone)
c        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,zer,
c     >               0,0,0,stone)
c        dum4=dum4+sig(l1,l2,l3,k1,k2)
c     > *dconjg(stone)
c      enddo
c      enddo
c100   continue
c      enddo
c      enddo
cc      write(1,*) dexp(dreal(dum2))
cc      write(1,*) dexp(dreal(dum3))
cc      write(1,*) dexp(dreal(dum4))
c      write(iu_sig,*) dreal(dum2)
c      write(iu_sig,*) dreal(dum3)
c      write(iu_sig,*) dreal(dum4)

c.....Ripristino np
      nz=i1
      nd=i2
      call gauss0

      close(unit=iu_sig)

      return
1000  format('ovl, dst, rsc [',4(i3,','),i3,'] = ',6x,20(1pe20.12E3))
      end



      subroutine sigcalc_wr0(ipdb1,ipdb2)

      include 'quafit2014.cmn'
      complex*16 dum1,dum2,dum3,dum4,dum6

c      write(*,*) '   ----> sigcalc   '
      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  
     >zer,zer,zer,0,0,0,0,0)
c      write(*,*) 'v12(0) ',dcut  !!! lc ltird=15 viene molto, molto bene
c      pause
c      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  !! previous definition
c     >zer,zer,zer,0,0,0,0,0)/1000
c      dcut=1000.d0 !! provo cosi' non mi piace, torno al valore di prima
      dcut=30.d0
c      write(*,*) 'dcut ',dcut

      r1=rtramin(ipdb1)+rtramin(ipdb2)
      r2=rtramax(ipdb1)+rtramax(ipdb2)
      n=20!20
      dr=(r2-r1)/(n-1)

c.....Memorizzo psi in una griglia di r
      r=r1-dr
      do ir=1,n
        r=r+dr
        i1=0
        do l1=0,ltird
          do l2=0,ltird
            lmax3=l1+l2
            if (lmax3.gt.ltird) lmax3=ltird
            nl3=iabs(l2-l1)
            do l3=nl3,lmax3
              dum0=cg(l1,l2,l3,0,0)
              if (dum0.ne.zer) then
              do k1=-l1,l1
              do k2=-l2,l2
                i1=i1+1
                if (i1.gt.mxpsi) stop 'i1>mxpsi'
                call psi_s(ipdb1,ipdb2,l1,l2,l3,k1,k2,r,psi)
                psiar(i1,ir)=psi
c                write(91,*) r,psiar(i1,ir)
              enddo
              enddo
              endif
            enddo
          enddo
        enddo
      enddo

c      stop
c      write(*,*) ' 55 1 ci sono'

      do l1=0,ltird
      do l2=0,ltird
      do k1=-l1,l1
      do k2=-l2,l2
      do m1=-l1,l1
        sigbar(l1,l2,k1,k2,m1)=dcmplx(zer,zer)
      enddo
      enddo
      enddo
      enddo
      enddo

      i1=nz
      i2=nd
      nz=4
      nd=1
c      nz=8
c      nd=1
c      nz=4
c      nd=3
      call gauss0

c.....Memorizzo pwar
      do ib=1,np
        dum7=dacos(zero(ib))
        call pw_go_mn(ltird,dum7,ib)
      enddo      

      do ia1=1,np
      a1=pi*(zero(ia1)+one)
      do ib1=1,np
      cb1=zero(ib1)
      b1=dacos(cb1)
      do ig1=1,np
      g1=pi*(zero(ig1)+one)
      do ia2=1,np
      a2=pi*(zero(ia2)+one)
      do ib2=1,np
      cb2=zero(ib2)
      b2=dacos(cb2)
      do ig2=1,np
      g2=pi*(zero(ig2)+one)
      r=r1-dr
      do 10 ir=1,n
        r=r+dr
        dum5=overlap(ipdb1,ipdb2,
     >a1,b1,g1,a2,b2,g2,zer,zer,r,1,ir,ib1,ib2,1)
c        write(29,*) r, dum5
        if (dum5.le.dcut) goto 20
        dum7=dum5
10    continue
20    if (dum5.ne.dum7) r=r-dr+dr/(dum5-dum7)*(dcut-dum7)
c      write(*,*) r
c      pause
c.log      dum6=dcmplx(dlog(r),zer)
      dum6=dcmplx(r,zer)
     >    *weight(ia1)
     >    *weight(ib1)
     >    *weight(ig1)
     >    *weight(ia2)
     >    *weight(ib2)
     >    *weight(ig2)
c      write(*,*) ia1,ib1,ig1,ia2,ib2,ig2
      do l1=0,ltird
      do l2=0,ltird
      dum0=dfloat((2*l1+1)*(2*l2+1))/64  !il pi**4 va via per l'integrazione di gauss-legendre
      do k1=-l1,l1
      do k2=-l2,l2
      do m1=-l1,l1
        if (abs(m1).gt.l2) goto 33
        dum3=      
     >       dcmplx(dcos(m1*a1), dsin(m1*a1))*
     >       dcmplx(dcos(k1*g1), dsin(k1*g1))*
     >       dcmplx(dcos(-m1*a2), dsin(-m1*a2))*
     >       dcmplx(dcos(k2*g2), dsin(k2*g2))*
     >          pwar(l1,m1,k1,ib1)*
     >          pwar(l2,-m1,k2,ib2)
        sigbar(l1,l2,k1,k2,m1)=sigbar(l1,l2,k1,k2,m1)
     >    +dum6*dum3*dum0
 33     continue
      enddo
      enddo
      enddo
      enddo
      enddo

      enddo
      enddo
      enddo
      enddo
      enddo
      enddo



      do l1=0,ltird
      do l2=0,ltird
      lmax3=l1+l2
      if (lmax3.gt.ltird) lmax3=ltird
      nl3=iabs(l2-l1)
      do l3=nl3,lmax3
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.ne.zer) then
      dum0=dsqrt(dfloat(2*l3+1))*((dcmplx(zer,one))**(l2-l1-l3))
      do k1=-l1,l1
      do k2=-l2,l2
        dum3=dcmplx(zer,zer)
        do m1=-l1,l1
          dum3=dum3+cg(l1,l2,l3,m1,-m1)*sigbar(l1,l2,k1,k2,m1)
        enddo
        sig(l1,l2,l3,k1,k2)=dum3*dum0
      enddo
      enddo
      endif
      enddo
      enddo
      enddo



      write(ch3(1:2),'(i2)') ipdb1
      write(ch2(1:2),'(i2)') ipdb2
      if (ch3(1:1).eq.' ') ch3(1:1)='0'
      if (ch2(1:1).eq.' ') ch2(1:1)='0'
      open (unit=iu_sig,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//ch3(1:2)//ch2//'.sig')

      do l1=0,ltird
      do l2=0,ltird
      lmax3=l1+l2
      if (lmax3.gt.ltird) lmax3=ltird
      nl3=iabs(l2-l1)
      do l3=nl3,lmax3
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.ne.zer) then
      do k1=-l1,l1
      do k2=-l2,l2
        write(iu_sig,1000) l1,l2,l3,k1,k2,sig(l1,l2,l3,k1,k2)
      enddo
      enddo
      endif
      enddo
      enddo
      enddo

c.....Test finale
      dum2=dcmplx(zer,zer)
      dum3=dcmplx(zer,zer)
      dum4=dcmplx(zer,zer)
      do l1=0,ltird
      do l2=0,ltird
      do 100 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 100
      do k1=-l1,l1
      do k2=-l2,l2
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,one,
     >               0,0,0,stone)
        dum2=dum2+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,zer,zer,zer,one,
     >               0,0,0,stone)
        dum3=dum3+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,zer,
     >               0,0,0,stone)
        dum4=dum4+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
      enddo
      enddo
100   continue
      enddo
      enddo
c      write(1,*) dexp(dreal(dum2))
c      write(1,*) dexp(dreal(dum3))
c      write(1,*) dexp(dreal(dum4))
      write(iu_sig,*) dreal(dum2)
      write(iu_sig,*) dreal(dum3)
      write(iu_sig,*) dreal(dum4)

c.....Ripristino np
      nz=i1
      nd=i2
      call gauss0

      close(unit=iu_sig)

      return
1000  format('sig[',4(i3,','),i3,'] = ',6x,2(1pe20.12E3))
      end



      subroutine sigcalc_total(ipdb1,ipdb2)

      include 'quafit2014.cmn'
      complex*16 dum1,dum2,dum3,dum4,dum6

c      write(*,*) '   ----> sigcalc   '
      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  
     >zer,zer,zer,0,0,0,0,0)
c      write(*,*) 'v12(0) ',dcut
c      pause
c      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  !! previous definition
c     >zer,zer,zer,0,0,0,0,0)/1000
c      dcut=1000.d0 !! provo cosi' non mi piace, torno al valore di prima
      dcut=30.d0
c      write(*,*) 'dcut ',dcut

      r1=rtramin(ipdb1)+rtramin(ipdb2)
      r2=rtramax(ipdb1)+rtramax(ipdb2)
      n=20
      dr=(r2-r1)/(n-1)

c.....Memorizzo psi
      r=r1-dr
      do ir=1,n
        r=r+dr
        i1=0
        do l1=0,ltird
          do l2=0,ltird
            do 801 l3=0,ltird
              dum0=cg(l1,l2,l3,0,0)
              if (dum0.eq.zer) goto 801
              do k1=-l1,l1
              do k2=-l2,l2
                i1=i1+1
                if (i1.gt.mxpsi) stop 'i1>mxpsi'
                call psi_s(ipdb1,ipdb2,l1,l2,l3,k1,k2,r,psi)
                psiar(i1,ir)=psi
c                write(91,*) r,psiar(i1,ir)
              enddo
              enddo
801         continue
          enddo
        enddo
      enddo

c      stop

      do l1=0,ltird
      do l2=0,ltird
      do 80 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 80
      do k1=-l1,l1
      do k2=-l2,l2
        sig(l1,l2,l3,k1,k2)=dcmplx(zer,zer)
      enddo
      enddo
80    continue
      enddo
      enddo

      i1=nz
      i2=nd
      nz=4
      nd=1
c      nz=4
c      nd=3
      call gauss0

c.....Memorizzo pwar
      do ib=1,np
        dum7=dacos(zero(ib))
        call pw_go_mn(ltird,dum7,ib)
      enddo      

      do ia1=1,np
      a1=pi*(zero(ia1)+one)
      do ib1=1,np
      cb1=zero(ib1)
      b1=dacos(cb1)
      do ig1=1,np
      g1=pi*(zero(ig1)+one)
      do ia2=1,np
      a2=pi*(zero(ia2)+one)
      do ib2=1,np
      cb2=zero(ib2)
      b2=dacos(cb2)
      do ig2=1,np
      g2=pi*(zero(ig2)+one)
      do iar=1,np
      ar=pi*(zero(iar)+one)
      do ibr=1,np
      cbr=zero(ibr)
      br=dacos(cbr)
      r=r1-dr
      do 10 ir=1,n
        r=r+dr
        dum5=overlap(ipdb1,ipdb2,
     >a1,b1,g1,a2,b2,g2,ar,br,r,1,ir,ib1,ib2,ibr)
c        write(29,*) r, dum5
        if (dum5.le.dcut) goto 20
        dum7=dum5
10    continue
20    if (dum5.ne.dum7) r=r-dr+dr/(dum5-dum7)*(dcut-dum7)
c      write(*,*) r
c      pause
c.log      dum6=dcmplx(dlog(r),zer)
      dum6=dcmplx(r,zer)
     >    *weight(ia1)
     >    *weight(ib1)
     >    *weight(ig1)
     >    *weight(ia2)
     >    *weight(ib2)
     >    *weight(ig2)
     >    *weight(iar)
     >    *weight(ibr)
c      write(*,*) ia2,ib2,ig2,iar,ibr
      do l1=0,ltird
      do l2=0,ltird
      do 33 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 33
      do k1=-l1,l1
      do 34 k2=-l2,l2
c        if (sig(l2,l1,l3,k2,k1).ne.dcmplx(zer,zer)) then
c          sig(l1,l2,l3,k1,k2)=sig(l2,l1,l3,k2,k1)
c          goto 34
c        endif
c        write(*,*) ' stone in'
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,
     >a2,b2,g2,ar,br,1,ib2,ibr,stone)
        sig(l1,l2,l3,k1,k2)=sig(l1,l2,l3,k1,k2)
     >    +dum6*
     >    stone
c        write(*,*) ia2,ib2,ig2,iar,ibr
34    continue
      enddo
33    continue
      enddo
      enddo

      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo
      enddo

      write(ch3(1:2),'(i2)') ipdb1
      write(ch2(1:2),'(i2)') ipdb2
      if (ch3(1:1).eq.' ') ch3(1:1)='0'
      if (ch2(1:1).eq.' ') ch2(1:1)='0'
      open (unit=iu_sig,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//ch3(1:2)//ch2//'.sig')

      do l1=0,ltird
      do l2=0,ltird
      do 90 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 90
      do k1=-l1,l1
      do k2=-l2,l2
        sig(l1,l2,l3,k1,k2)=
     >      sig(l1,l2,l3,k1,k2)*(2*l1+1)*(2*l2+1)*(2*l3+1)/256
        write(iu_sig,1000) l1,l2,l3,k1,k2,sig(l1,l2,l3,k1,k2)
      enddo
      enddo
90    continue
      enddo
      enddo

c.....Test finale
      dum2=dcmplx(zer,zer)
      dum3=dcmplx(zer,zer)
      dum4=dcmplx(zer,zer)
      do l1=0,ltird
      do l2=0,ltird
      do 100 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 100
      do k1=-l1,l1
      do k2=-l2,l2
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,one,
     >               0,0,0,stone)
        dum2=dum2+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,zer,zer,zer,one,
     >               0,0,0,stone)
        dum3=dum3+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,zer,
     >               0,0,0,stone)
        dum4=dum4+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
      enddo
      enddo
100   continue
      enddo
      enddo
c      write(1,*) dexp(dreal(dum2))
c      write(1,*) dexp(dreal(dum3))
c      write(1,*) dexp(dreal(dum4))
      write(iu_sig,*) dreal(dum2)
      write(iu_sig,*) dreal(dum3)
      write(iu_sig,*) dreal(dum4)

c.....Ripristino np
      nz=i1
      nd=i2
      call gauss0

      close(unit=iu_sig)

      return
1000  format('sig[',4(i3,','),i3,'] = ',6x,2(1pe20.12E3))
      end

      subroutine sigcalc_old(ipdb1,ipdb2)

      include 'quafit2014.cmn'
      complex*16 dum1,dum2,dum3,dum4,dum6

c      write(*,*) '   ----> sigcalc   '
      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  
     >zer,zer,zer,0,0,0,0,0)
c      write(*,*) 'v12(0) ',dcut
c      pause
c      dcut=overlap(ipdb1,ipdb2,zer,zer,zer,zer,zer,zer,  !! previous definition
c     >zer,zer,zer,0,0,0,0,0)/1000
c      dcut=1000.d0 !! provo cosi' non mi piace, torno al valore di prima
      dcut=30.d0
c      write(*,*) 'dcut ',dcut

      r1=rtramin(ipdb1)+rtramin(ipdb2)
      r2=rtramax(ipdb1)+rtramax(ipdb2)
      n=20
      dr=(r2-r1)/(n-1)

c.....Memorizzo psi
      r=r1-dr
      do ir=1,n
        r=r+dr
        i1=0
        do l1=0,ltird
          do l2=0,ltird
            do 801 l3=0,ltird
              dum0=cg(l1,l2,l3,0,0)
              if (dum0.eq.zer) goto 801
              do k1=-l1,l1
              do k2=-l2,l2
                i1=i1+1
                if (i1.gt.mxpsi) stop 'i1>mxpsi'
                call psi_s(ipdb1,ipdb2,l1,l2,l3,k1,k2,r,psi)
                psiar(i1,ir)=psi
c                write(91,*) r,psiar(i1,ir)
              enddo
              enddo
801         continue
          enddo
        enddo
      enddo

c      stop

      do l1=0,ltird
      do l2=0,ltird
      do 80 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 80
      do k1=-l1,l1
      do k2=-l2,l2
        sig(l1,l2,l3,k1,k2)=dcmplx(zer,zer)
      enddo
      enddo
80    continue
      enddo
      enddo

      i1=nz
      i2=nd
      nz=4
      nd=1
c      nz=4
c      nd=3
      call gauss0

c.....Memorizzo pwar
      do ib=1,np
        dum7=dacos(zero(ib))
        call pw_go_mn(ltird,dum7,ib)
      enddo      

      do ia2=1,np          !!! tengo fisso omega1, si!!
      a2=pi*(zero(ia2)+one)
      do ib2=1,np
      cb2=zero(ib2)
      b2=dacos(cb2)
      do ig2=1,np
      g2=pi*(zero(ig2)+one)
      do iar=1,np
      ar=pi*(zero(iar)+one)
      do ibr=1,np
      cbr=zero(ibr)
      br=dacos(cbr)
      r=r1-dr
      do 10 ir=1,n
        r=r+dr
        dum5=overlap(ipdb1,ipdb2,
     >zer,zer,zer,a2,b2,g2,ar,br,r,1,ir,1,ib2,ibr)
c        write(29,*) r, dum5
        if (dum5.le.dcut) goto 20
        dum7=dum5
10    continue
20    if (dum5.ne.dum7) r=r-dr+dr/(dum5-dum7)*(dcut-dum7)
c      write(*,*) r
c      pause
c.log      dum6=dcmplx(dlog(r),zer)
      dum6=dcmplx(r,zer)
     >    *weight(ia2)
     >    *weight(ib2)
     >    *weight(ig2)
     >    *weight(iar)
     >    *weight(ibr)
c      write(*,*) ia2,ib2,ig2,iar,ibr
      do l1=0,ltird
      do l2=0,ltird
      do 33 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 33
      do k1=-l1,l1
      do 34 k2=-l2,l2
c        if (sig(l2,l1,l3,k2,k1).ne.dcmplx(zer,zer)) then
c          sig(l1,l2,l3,k1,k2)=sig(l2,l1,l3,k2,k1)
c          goto 34
c        endif
c        write(*,*) ' stone in'
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,
     >a2,b2,g2,ar,br,1,ib2,ibr,stone)
        sig(l1,l2,l3,k1,k2)=sig(l1,l2,l3,k1,k2)
     >    +dum6*
     >    stone
c        write(*,*) ia2,ib2,ig2,iar,ibr
34    continue
      enddo
33    continue
      enddo
      enddo

      enddo
      enddo
      enddo
      enddo
      enddo

      write(ch3(1:2),'(i2)') ipdb1
      write(ch2(1:2),'(i2)') ipdb2
      if (ch3(1:1).eq.' ') ch3(1:1)='0'
      if (ch2(1:1).eq.' ') ch2(1:1)='0'
      open (unit=iu_sig,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//ch3(1:2)//ch2//'.sig')

      do l1=0,ltird
      do l2=0,ltird
      do 90 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 90
      do k1=-l1,l1
      do k2=-l2,l2
        sig(l1,l2,l3,k1,k2)=
     >      sig(l1,l2,l3,k1,k2)*(2*l1+1)*(2*l2+1)*(2*l3+1)/32
        write(iu_sig,1000) l1,l2,l3,k1,k2,sig(l1,l2,l3,k1,k2)
      enddo
      enddo
90    continue
      enddo
      enddo

c.....Test finale
      dum2=dcmplx(zer,zer)
      dum3=dcmplx(zer,zer)
      dum4=dcmplx(zer,zer)
      do l1=0,ltird
      do l2=0,ltird
      do 100 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 100
      do k1=-l1,l1
      do k2=-l2,l2
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,one,
     >               0,0,0,stone)
        dum2=dum2+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,zer,zer,zer,one,
     >               0,0,0,stone)
        dum3=dum3+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
        call stone_s(l1,l2,l3,k1,k2,zer,one,zer,zer,one,zer,zer,zer,
     >               0,0,0,stone)
        dum4=dum4+sig(l1,l2,l3,k1,k2)
     > *dconjg(stone)
      enddo
      enddo
100   continue
      enddo
      enddo
c      write(1,*) dexp(dreal(dum2))
c      write(1,*) dexp(dreal(dum3))
c      write(1,*) dexp(dreal(dum4))
      write(iu_sig,*) dreal(dum2)
      write(iu_sig,*) dreal(dum3)
      write(iu_sig,*) dreal(dum4)

c.....Ripristino np
      nz=i1
      nd=i2
      call gauss0

      close(unit=iu_sig)

      return
1000  format('sig[',4(i3,','),i3,'] = ',6x,2(1pe20.12E3))
      end


      subroutine sigcal_new(ipdb1,ipdb2,a1,b1,g1,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)

      include 'quafit2014.cmn'
      complex*16 dum11,dum12,dum13,dum2
c      write(*,*) '   ---> sigcal_new(a1,b1,g1,a2,b2,g2,ar,br)   '

      call sigrea(min(ipdb1,ipdb2),max(ipdb1,ipdb2))

      cb1=dcos(b1)
      cb2=dcos(b2)
      cbr=dcos(br)

c.....Constant factors

      call pw_go_mn(ltird,b1,1)
      call pw_go_mn(ltird,b2,2)
      call pw_go_m0(ltird,br,3)

      do l=-ltird,ltird
c.......a1
        c1=l*a1
        s1=dsin(c1)
        c1=dcos(c1)
        ea1(l)=dcmplx(c1,-s1)
c.......a2
        c1=l*a2
        s1=dsin(c1)
        c1=dcos(c1)
        ea2(l)=dcmplx(c1,-s1)
c.......g1
        c1=l*g1
        s1=dsin(c1)
        c1=dcos(c1)
        eg1(l)=dcmplx(c1,-s1)
c.......g2
        c1=l*g2
        s1=dsin(c1)
        c1=dcos(c1)
        eg2(l)=dcmplx(c1,-s1)
c.......ar
        c1=l*ar
        s1=dsin(c1)
        c1=dcos(c1)
        ear(l)=dcmplx(c1,-s1)
      enddo


      dum11=dcmplx(zer,zer)
      dum12=dcmplx(zer,zer)
      dum13=dcmplx(zer,zer)
      do l1=0,ltird
      do l2=0,ltird
      do 100 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) goto 100
      do k1=-l1,l1
      do k2=-l2,l2
        dum2=dconjg(stone_new(l1,l2,l3,k1,k2))
        dum11=dum11+ovl(l1,l2,l3,k1,k2)*dum2
        dum12=dum12+dst(l1,l2,l3,k1,k2)*dum2
        dum13=dum13+rsc(l1,l2,l3,k1,k2)*dum2
c        write(*,*) l1,l2,l3,k1,k2,sig(l1,l2,l3,k1,k2),
c     >stone_new(l1,l2,l3,k1,k2)
      enddo
      enddo
100   continue
      enddo
      enddo
c      write(*,*) dum1
c      sigcal=dexp(dreal(dum1))
      ovlt=dreal(dum11)
      dstt=dreal(dum12)
      rsct=dreal(dum13)
      return
      end

      subroutine sigrea(ipdb1,ipdb2)

      include 'quafit2014.cmn'

c      write(*,*) '   ---> sigrea   '

      write(ch3(1:2),'(i2)') ipdb1
      write(ch2(1:2),'(i2)') ipdb2
      if (ch3(1:1).eq.' ') ch3(1:1)='0'
      if (ch2(1:1).eq.' ') ch2(1:1)='0'
      open (unit=iu_sig,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//ch3(1:2)//ch2//'.sig')

      do l1=0,ltird
      do l2=0,ltird
      lmax3=l1+l2
      if (lmax3.gt.ltird) lmax3=ltird
      nl3=iabs(l2-l1)
      do l3=nl3,lmax3
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.ne.zer) then
      do k1=-l1,l1
      do k2=-l2,l2
        read(iu_sig,1000) 
     > ovl(l1,l2,l3,k1,k2)
     >,dst(l1,l2,l3,k1,k2)
     >,rsc(l1,l2,l3,k1,k2)
      enddo
      enddo
      endif
      enddo
      enddo
      enddo
      close(unit=iu_sig)

      return
1000  format(44x,20(e20.12E3))
      end

      subroutine sigrea_old(ipdb1,ipdb2)

      include 'quafit2014.cmn'

c      write(*,*) '   ---> sigrea   '

      write(ch3(1:2),'(i2)') ipdb1
      write(ch2(1:2),'(i2)') ipdb2
      if (ch3(1:1).eq.' ') ch3(1:1)='0'
      if (ch2(1:1).eq.' ') ch2(1:1)='0'
      open (unit=iu_sig,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//ch3(1:2)//ch2//'.sig')

      do l1=0,ltird
      do l2=0,ltird
      do 80 l3=0,ltird
      dum0=cg(l1,l2,l3,0,0)

      if (dum0.eq.zer) goto 80
      do k1=-l1,l1
      do k2=-l2,l2
        read(iu_sig,1000) sig(l1,l2,l3,k1,k2)
      enddo
      enddo
80    continue
      enddo
      enddo
      close(unit=iu_sig)

      return
1000  format(33x,2(e20.12E3))
      end


      subroutine filpr

      include 'quafit2014.cmn'
      character nnn*2

      write(*,*) '   ---> filpr   '
c.....Traccia la p(r) a partire dall i(q) sperimentale 
c.....Massimo prodotto tra raggio di girazione e q massimo


      do nn=1,nexper

      write(nnn,'(i2)') nn
      if (nnn(1:1).eq.' ') nnn(1:1)='0'

      nr=nqfull
      rmin=0.d0

      dum5=0.d0
      rmax = dprmax(nn)
      dr = (rmax-rmin)/(nr)

      open(unit=4,status='unknown',file=
     >     'qua'//code//subcode//'_'//nnn//'.iq')


      do 820 iq=1,nqfull
        q=qqfull(iq)
        call iqcalc(nn,iq)
        qit2(iq)=qit
        if (qit2(1).eq.0.d0) goto 820
        write(4,1000) q,qit,qit/qit2(1),
     >((qit_parz(istate,ihagg),ihagg=1,nhagg(istate)),istate=1,nstate)
  820 continue
      close (unit=4)


c########

      open(unit=2,status='unknown',file=
     >     'qua'//code//subcode//'_'//nnn//'.pr')
      r=rmin-dr/2
      dq=qqfull(2)-qqfull(1)
      do 720 ir=1,nr
        r=r+dr
        dum2=0.d0
        do 821 iq=1,nqfull
          q=qqfull(iq)
          if (qit2(1).eq.0.d0) goto 821
          qit=qit2(iq)/qit2(1)
          dum1=q*r
          if (dum1.eq.0.d0) then
            dum1=1.d0
          else
            dum1=dsin(dum1)/dum1
          endif
          dum2=dum2+q*q*qit*dum1 
821     continue
        dum3=2.d0*dum2*dq/pi
        dum4=dum3*r*r
        dum5=dum5+dum4
        write(2,1000) r,dum4 
720   continue
      dum5=dum5*dr
      close (unit=2)


      enddo

      return 
1000  format(2000(e19.7E3))
1100  format(' Iteration number and p(r) integral ...:',i2,1x,f8.5)
      end

      subroutine filfit

      include 'quafit2014.cmn'
      character nnn*2

      write(*,*) '   ---> filfit   '
      do nn=1,nexper

      write(nnn,'(i2)') nn
      if (nnn(1:1).eq.' ') nnn(1:1)='0'

      open(unit=2,status='unknown',file=
     >     'qua'//code//subcode//'_'//nnn//'.fit')
      write(2,9987)
      do 40 iq=nqn1(nn),nqn(nn)
        q=qqn(nn,iq)
        qit=qit2n(nn,iq)
        write(2,1000) q,qien(nn,iq),qidn(nn,iq),qit 
   40 continue
      close (unit=2)

c      write(77,*) ' k =',qipar(nn)
c      write(77,*) ' b =',plate(nn)
      enddo
      return 
9987  format('#Q| I_exp| Err I_exp| I_fit') 
1000  format(50(e15.6E3))
      end


      subroutine chis2(f)

      include 'quafit2014.cmn'

      write(*,*) '   ---> chis2(f)   '
      f=0.d0
      call fullconst
      do istate=1,nstate
        do ihagg=1,nhagg(istate)
          do iconf=1,nconf(istate)
            call pq_parz_2(istate,ihagg,iconf)
          enddo
        enddo
      enddo
      call minqbid(f)
      return
      end

      subroutine fullconst

      include 'quafit2014.cmn'

      write(*,*) '   ---> fullconst   '
      
      if (jfrac.ne.1) then
        call fraccalc
      else
        call xfraccalc
      endif

c      stop
c      write(*,*) fract(1,1,1)
c      pause
      do istate=1,nstate
        do iconf=1,nconf(istate)  !! li ho invertiti iconf e ihagg
          do ihagg=1,nhagg(istate)
c            call framhag(istate,ihagg,iconf) !!! era 0 prima
            call framhag(istate,ihagg,iconf)
            call framalllink(istate,ihagg,iconf)
            call test_disv(istate,ihagg,iconf)
c          write(*,*) ' a43 ci sono'
c      call fraccalc
c      write(*,*) fract(1,1,1)
c      pause
            do isite=1,nsite(istate)
c.aspetta          overm_site(istate,isite,ihagg,iconf)=
c.aspetta     >    overl_site(istate,isite,ihagg,iconf)
              call parconst(istate,isite,ihagg,iconf)
c      call fraccalc
c      write(*,*) fract(1,1,1)
c      pause
            enddo
          enddo
c          write(*,*) ' adf ci sono'
          do iilink=1,nlink(istate)
            call linkerendsfind(istate,iilink,iconf) !!individua le estremita'
            call linkercoo(0,istate,iilink,iconf)  !!trova la conformazione ottimale
            call fulllinker(istate,iilink,iconf) !!trova gli alm

            do ihagg=1,nhagg(istate)
              call framhag(istate,ihagg,iconf)
c              write(*,*) uessea,aessea,bessea
              call parconstlinker(istate,iilink,ihagg,iconf)
            enddo
          enddo
        enddo!iconf
      enddo!istate
      return
      end

      subroutine mcarlo

      include 'quafit2014.cmn'

      write(*,*) '   ---> mcarlo   '
      perc=0.2d0

      call chis2(f)

c......Per compiere questo loop ci vuole molto tempo.
      do ivar=1,mpar
        call ciclo_sub(ivar)
        varnew(ivar)=varold(ivar)
c        write(*,*) ' 78 ci sono'
        call ciclo_res(ivar)
c        write(*,*) ' 79 ci sono'
      enddo

      t_overl=zer
      t_qverl=zer
      t_cverl=zer
      i1=0
      i2=0
      do istate=1,nstate
        do iconf=1,nconf(istate)
          do ihagg=1,nhagg(istate)
            jconf=iconf+4
c            call framhag(istate,ihagg,iconf) !!!dopo lo rimetti a zero
            call framhag(istate,ihagg,iconf)
            call framalllink(istate,ihagg,iconf)
            call test_disv(istate,ihagg,iconf)
            t_overl=t_overl+overl(istate,ihagg,iconf)
            i1=i1+1
          enddo
          call test_linker(istate,iconf) 
          t_qverl=t_qverl+qverl(istate,iconf)
          call test_constr(istate,iconf) 
          t_cverl=t_cverl+cverl(istate,iconf)
          i2=i2+1
        enddo
      enddo

      if (i1.ne.0) t_overl=t_overl/i1
      if (i2.ne.0) t_qverl=t_qverl/i2
      if (i2.ne.0) t_cverl=t_cverl/i2
      
      call chis2(f)
      fnew=f+fout
      chinew=fnew

      fnew=fnew+w_overl*t_overl+w_kforce*t_qverl+t_cverl
      fold=fnew
      chiold=chinew
      t_overl_old=t_overl
      t_qverl_old=t_qverl
      t_cverl_old=t_cverl

      write(*,*) ' overl = ',t_overl
      write(*,*) ' qverl = ',t_qverl
      write(*,*) ' cverl = ',t_cverl
      write(*,*) ' f(X0) = ',fnew,f,fout

      subcode='inp'
      call filfit
      call filpr
      call pr_parz_2
      call outparz
      call xfracparz

c      return !!! no minimo



c.....Inizializzazioni conteggi
      totmos=zer
      accmos=zer

      iu_33=33
      open (unit=iu_33,status='unknown',file='qua'//code//'.out')

c.....calcolo fattore per simulated annealing
      if (tempfix.eq.0.d0.and.nsubrun.gt.1) then
        vi=1.20d0
        vf=1.05d0
        dum2=abs(fnew)*facttstart !!!/2!5  !Starting T*
        temp=abs(fhypo)/2 !Expected final T*
c T(istep)=Tiniz*fattore**(istep-1)
c Tfin=Tiniz*fattore**(nstep-1)
        dum1=(temp/dum2)**(one/(nsubrun-1))
        temp=dum2/dum1
        dum3=(vf/vi)**(one/(nsubrun-1))
        dum4=vi/dum3
      else
        temp=tempfix
        dum1=one
        dum3=one
        dum4=1.05
      endif

      open (unit=31,status='unknown',file='qua'//code//'out.gnu')
      write(31,'(a)') 'reset'
      write(31,'(a)') 'set xlabel "n. cycles"'
      write(31,'(a)') 'set title "analysis #'//code//'"'

      write(31,'(a)') 
     >'set ylabel "H(X)"'
      write(31,'(a)') 
     >'plot ''qua'//code//'.out'' u 1:2 notitle w lp; pause -1'

      write(31,'(a)') 
     >'set ylabel "chi2(X)"'
      write(31,'(a)') 
     >'plot ''qua'//code//'.out'' u 1:3 notitle w lp; pause -1'

      write(31,'(a)') 
     >'set ylabel "Average Elastic Energy Adim., A(X)/w_A"'
      write(31,'(a,i3,a)') 
     >'plot ''qua'//code//'.out'' u 1:4 notitle w lp; pause -1'

      write(31,'(a)') 
     >'set ylabel "Average LJ Potential Adim., B(X)/w_B"'
      write(31,'(a,i3,a)') 
     >'plot ''qua'//code//'.out'' u 1:5 notitle w lp; pause -1'

      write(31,'(a)') 
     >'set ylabel "Average Constraints, C(X)"'
      write(31,'(a,i3,a)') 
     >'plot ''qua'//code//'.out'' u 1:8 notitle w lp; pause -1'


      write(31,'(a)')
     >'set ylab "";plot ''qua'//
     >code//'.out'' u 1:3 title ''chi2(X)''  w lp,\\'
      write(31,'(a)')
     >' ''qua'//code//'.out'' u 1:6 title ''A(X)'' w lp,\\'
      write(31,'(a)')
     >' ''qua'//code//'.out'' u 1:7 title ''B(X)'' w lp,\\'
      write(31,'(a)')
     >' ''qua'//code//'.out'' u 1:8 title ''C(X)'' w lp; pa -1'

        
      do j=1,mpar
      write(31,'(a)') 
     >'set ylabel "'//varnam(j)(1:lnfil(varnam(j),40))//
     >': value"'
      write(31,'(a,i5,a)') 
     >'plot ''qua'//code//'.out'' u 1:($',3*j+6,
     >')   notitle w lp; pause -1'
      write(31,'(a)') 
     >'set ylabel "'//varnam(j)(1:lnfil(varnam(j),40))//
     >': acc. ratio"'
      write(31,'(a,i5,a)') 
     >'plot ''qua'//code//'.out'' u 1:($',3*j+7,
     >')   notitle w lp; pause -1'
      write(31,'(a)') 
     >'set ylabel "'//varnam(j)(1:lnfil(varnam(j),40))//
     >': dislpacement"'
      write(31,'(a,i5,a)') 
     >'plot ''qua'//code//'.out'' u 1:($',3*j+8,
     >')  notitle w lp; pause -1'
      enddo

      close(unit=31)


      do i=1,nsubrun
        temp=temp*dum1
        dum4=dum4*dum3
        pushdn=two-dum4
        pushup=dum4


        call subrun2

        write(*,*) ' Sub-ref =',i
        write(*,*) ' temp    =',temp
        write(*,*) ' f(X)    =',fold

c        do ihagg=1,nhagg(1)
c          write(*,*) ihagg,(fract(1,ihagg,nn),nn=1,nexper)
c        enddo

        write(subcode,1100) i
        if (subcode(1:1).eq.' ') subcode(1:1)='0'
        if (subcode(2:2).eq.' ') subcode(2:2)='0'
        call filfit
        call filpr
        call pr_parz_2
        call gnufit(i)
        call outparz
        call xfracparz

         
        do istate=1,nstate
          do ihagg=1,nhagg(istate)
            do iconf=1,nconf(istate)
              call framhag(istate,ihagg,iconf)
      call framalllink(istate,ihagg,iconf)
c      close(61)
      call test_disv(istate,ihagg,iconf)
              call btoms(istate,ihagg,iconf)
            enddo
          enddo
        enddo

c        stop

        write(iu_33,1000) float(i*ncycl),fold,chiold
     >,t_qverl_old,t_overl_old,w_kforce*t_qverl_old
     >,w_overl*t_overl_old,t_cverl_old
     >,(varold(j),varacc(j)/vartot(j),vardel(j),j=1,mpar)

      enddo
 1313 close(unit=iu_33)
      close(unit=11)

      return   
4012  format('  Weight of state ....................:',e10.3)
4001  format('  Scale Factor kappa ...................:',e10.3)
4002  format('  Flat Background B ....................:',e10.3)
4003  format('  Scale Factor of Deoxy data ...........:',e10.3)
1000  format(8000(e15.6E3))
1100  format(i3)
3000  format(
     >' RD #..................................:',i3,/,
     >'    Point group of aggregate (a4) .....:',a,/,
     >'    Main Unit pdb File (without .pdb) .:',a,/,
     >'    Geom. Center Vect.: modulus (ang.) :',1(f10.5),/,
     >'                        alpha   (deg) .:',1(f10.5),/,
     >'                        cos(beta) .....:',1(f10.5),/,
     >'    Rotation Angles:    alpha_a (deg) .:',1(f10.5),/,
     >'                        cos(beta_a) ...:',1(f10.5),/,
     >'                        gamma_a (deg) .:',1(f10.5))
3001  format(1x,a2,'    a_int(',i2,',',i2,') = ',4(e15.6E3,1x))
3003  format(1x,a2,'    a_int(',i2,',',i2,') = ',4(e15.6E3,1x))
3002  format(1x,a2,' 2  a_int(',i2,',',i2,') = ',4(e15.6E3,1x))
3011  format(1x,a2,'    a_wdt(',i2,',',i2,') = ',4(e15.6E3,1x))
3013  format(1x,a2,'    a_wdt(',i2,',',i2,') = ',4(e15.6E3,1x))
3012  format(1x,a2,' 2  a_wdt(',i2,',',i2,') = ',4(e15.6E3,1x))
3014  format(' Relative Mass Density of Bound Water .:',4(e15.6E3,1x))
 3015 format(' Scattering Volume (Angs.((3) .........:',4(e15.6E3,1x))
      end



c      subroutine afind(istate,iconf)
c
c      include 'quafit2014.cmn'
c      dimension x(mxsite*3)
c      dimension xl(mxsite*3),xh(mxsite*3),g(mxsite*3)
cc      external hingemin
c
cc##################################################
c      ncicl=5
c      nsub=10
c      ti=100.d0
c      tf=1.d0
c      isnn=1
cc##################################################
c      iostate=istate
c      ioconf=iconf
c      npar=nsite(istate)*3
c      jconf=iconf+4
c      if (isnn.ne.0) then
cc        write(*,*) ' Simann chisq'
c        i1=0
c        do isite=1,nsite(istate)
c          i1=i1+1
c          x(i1)=xscinv(aas(istate,isite,jconf),
c     >aas(istate,isite,2),aas(istate,isite,3))
c          i1=i1+1
c          x(i1)=xscinv(cba(istate,isite,jconf),
c     >cba(istate,isite,2),cba(istate,isite,3))
c          i1=i1+1
c          x(i1)=xscinv(gas(istate,isite,jconf),
c     >gas(istate,isite,2),gas(istate,isite,3))
c        enddo
c        do i=1,npar
c          xl(i)=-pih
c          xh(i)=pih
c          g(i)=pih/3!0.1
c        enddo
c        call sian(hingemin,npar,x,xl,xh,g,0.5d0,f,ti,tf,ncicl,nsub,
c     >       tm,am,iseed)
c      endif
c      return
c      end



c      subroutine hingemin(npar,x,f)
c
c      include 'quafit2014.cmn'
c      dimension x(mxsite*3)
c
c      istate=iostate
c      iconf=ioconf
c
c      jconf=iconf+4
c      i1=0
c      do isite=1,nsite(istate)
c        i1=i1+1
c        aas(istate,isite,jconf)=xscale(x(i1),
c     >aas(istate,isite,2),aas(istate,isite,3))
c        i1=i1+1
c        cba(istate,isite,jconf)=xscale(x(i1),
c     >cba(istate,isite,2),cba(istate,isite,3))
c        i1=i1+1
c        gas(istate,isite,jconf)=xscale(x(i1),
c     >gas(istate,isite,2),gas(istate,isite,3))
c      enddo
c      call hingefind(istate,iconf)
c      call test_hinge(istate,iconf)
c      f=qverl(istate,iconf)
c      return
c      end


      subroutine linkerendsfind(istate,iilink,iconf) !ums aus cbu

      include 'quafit2014.cmn'

c      write(*,*) '   ---> hingefind(istate)   '
      jconf=iconf+4
c      do iilink=1,nlink(istate)
      do iend=1,2
        isite=isitelink(istate,iilink,2*iend-1)
        if (isite.ne.0) then
        ipdb=ipdbsite(istate,isite)
        imaa=isitelink(istate,iilink,4+iend)

        xt=ums(istate,isite,jconf)*sfromc(cbu(istate,isite,jconf))
     >*dcos(aus(istate,isite,jconf))
        yt=ums(istate,isite,jconf)*sfromc(cbu(istate,isite,jconf))
     >*dsin(aus(istate,isite,jconf))               
        zt=ums(istate,isite,jconf)*cbu(istate,isite,jconf)

        call mat(-gas(istate,isite,jconf),
     >-dacos(cba(istate,isite,jconf))
     >,-aas(istate,isite,jconf),rot)

c.......n
        x1=cooaall_n(ipdb,imaa,1)
        y1=cooaall_n(ipdb,imaa,2)
        z1=cooaall_n(ipdb,imaa,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooaallrot_n(istate,iilink,iend,1)=rx
        cooaallrot_n(istate,iilink,iend,2)=ry
        cooaallrot_n(istate,iilink,iend,3)=rz
c....... c
        x1=cooaall_c(ipdb,imaa,1)
        y1=cooaall_c(ipdb,imaa,2)
        z1=cooaall_c(ipdb,imaa,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooaallrot_c(istate,iilink,iend,1)=rx
        cooaallrot_c(istate,iilink,iend,2)=ry
        cooaallrot_c(istate,iilink,iend,3)=rz
c....... o
        x1=cooaall_o(ipdb,imaa,1)
        y1=cooaall_o(ipdb,imaa,2)
        z1=cooaall_o(ipdb,imaa,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooaallrot_o(istate,iilink,iend,1)=rx
        cooaallrot_o(istate,iilink,iend,2)=ry
        cooaallrot_o(istate,iilink,iend,3)=rz
c....... ca
        x1=cooaall_ca(ipdb,imaa,1)
        y1=cooaall_ca(ipdb,imaa,2)
        z1=cooaall_ca(ipdb,imaa,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooaallrot_ca(istate,iilink,iend,1)=rx
        cooaallrot_ca(istate,iilink,iend,2)=ry
        cooaallrot_ca(istate,iilink,iend,3)=rz
        endif

      enddo!iend
c      enddo!iilink
      return
      end

c      subroutine hingefind(istate,iconf) !ums aus cbu
c
c      include 'quafit2014.cmn'
c
cc      write(*,*) '   ---> hingefind(istate)   '
c      jconf=iconf+4
c      do isite=1,nsite(istate)
c        ipdb=ipdbsite(istate,isite)
c        xt=ums(istate,isite,jconf)*dsqrt(one-cbu(istate,isite,jconf)**2)
c     >*dcos(aus(istate,isite,jconf))
c        yt=ums(istate,isite,jconf)*dsqrt(one-cbu(istate,isite,jconf)**2)
c     >*dsin(aus(istate,isite,jconf))               
c        zt=ums(istate,isite,jconf)*cbu(istate,isite,jconf)
c        call mat(-gas(istate,isite,jconf),
c     >-dacos(cba(istate,isite,jconf))
c     >,-aas(istate,isite,jconf),rot)
c
c
cc            xt=centre(isite,1,1,1)
cc            yt=centre(isite,1,1,2)
cc            zt=centre(isite,1,1,3)
cc            call mat(
cc     >        -gamrot(isite,1,1)
cc     >       ,-betrot(isite,1,1)
cc     >       ,-alprot(isite,1,1),rot)
c
c
cc.......Left (N-terminal)center
c        x1=xlref(ipdb)
c        y1=ylref(ipdb)
c        z1=zlref(ipdb)
ccc        call cpolar(x1,y1,z1,ra1,aa1,ba1)
cc.......Rotazione secondo la rot e traslazione
c        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
c        xlrot(istate,isite)=rx
c        ylrot(istate,isite)=ry
c        zlrot(istate,isite)=rz
cc.......Right (C terminal)center
c        x1=xrref(ipdb)
c        y1=yrref(ipdb)
c        z1=zrref(ipdb)
ccc        call cpolar(x1,y1,z1,ra1,aa1,ba1)
cc.......Rotazione secondo la rot e traslazione
c        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
c        xrrot(istate,isite)=rx
c        yrrot(istate,isite)=ry
c        zrrot(istate,isite)=rz
c
c      enddo        
c      return
c      end

c      subroutine ufind(istate,iconf) !ums aus cbu
c
c      include 'quafit2014.cmn'
c
c      write(*,*) '   ---> ufind(istate)   '
c      jconf=iconf+4
c      do isite=1,nsite(istate)
c        ipdb=ipdbsite(istate,isite)
c
c        call mat(-gas(istate,isite,jconf),
c     >-dacos(cba(istate,isite,jconf))
c     >,-aas(istate,isite,jconf),rot)
cc.......Left (N-terminal) center
c        x1=xlref(ipdb)
c        y1=ylref(ipdb)
c        z1=zlref(ipdb)
cc.......Rotazione secondo la rot
c        call rotrdi(x1,y1,z1,rx,ry,rz,zer,zer,zer,rot)
c        xlrot(istate,isite)=rx
c        ylrot(istate,isite)=ry
c        zlrot(istate,isite)=rz
cc.......Right (C terminal) center
c        x1=xrref(ipdb)
c        y1=yrref(ipdb)
c        z1=zrref(ipdb)
cc.......Rotazione secondo la rot
c        call rotrdi(x1,y1,z1,rx,ry,rz,zer,zer,zer,rot)
c        xrrot(istate,isite)=rx
c        yrrot(istate,isite)=ry
c        zrrot(istate,isite)=rz
c
c        if (isite.eq.1) then
c          xt=ums(istate,isite,jconf)*dsqrt(one
c     >-cbu(istate,isite,jconf)**2)*dcos(aus(istate,isite,jconf))
c          yt=ums(istate,isite,jconf)*dsqrt(one
c     >-cbu(istate,isite,jconf)**2)*dsin(aus(istate,isite,jconf))               
c          zt=ums(istate,isite,jconf)*cbu(istate,isite,jconf)
c        else
c          xt=xt-xlrot(istate,isite)+xrrot(istate,isite-1)  
c          yt=yt-ylrot(istate,isite)+yrrot(istate,isite-1)  
c          zt=zt-zlrot(istate,isite)+zrrot(istate,isite-1)  
c          call cpolar(xt,yt,zt,ums(istate,isite,jconf),
c     >aus(istate,isite,jconf),dum1)
c          cbu(istate,isite,jconf)=dcos(dum1)
c        endif
c      enddo        
c      return
c      end

      subroutine test_linker(istate,iconf)

      include 'quafit2014.cmn'

      qverl(istate,iconf)=zer
      if (nlink(istate).gt.0) then
      dum1=zer
      do iilink=1,nlink(istate)
        call framhag(istate,1,iconf) !ihagg=1
        call linkerendsfind(istate,iilink,iconf) !!individua le estremita'
        call framalllink(istate,1,iconf)
        call linkercoo(0,istate,iilink,iconf)  !!trova la conformazione ottimale
c        write(*,*) iilink,iconf,qverl_lnk(istate,iilink,iconf)
        dum1=dum1+qverl_lnk(istate,iilink,iconf)
      enddo
      qverl(istate,iconf)=dum1/nlink(istate)
      endif
      return
      end

c      subroutine test_hinge(istate,iconf)
c
c      include 'quafit2014.cmn'
c
cc      write(*,*) '   ---> test_hinge(istate)   '
cc      parameter(r1opt=25.)!20.!12.
c      if (w_kforce.eq.zer.or.ifhinge(istate)
c     >.eq.1) return
c      dum1=zer
c      jsite=1
c      i1=0
c      do isite=1,nsite(istate)-1
c        dum2=dnextsite(istate,isite)
c        jsite=jsite+1
c        if (dum2.ne.0.) then
c          rx=xrrot(istate,isite)-xlrot(istate,jsite)
c          ry=yrrot(istate,isite)-ylrot(istate,jsite)
c          rz=zrrot(istate,isite)-zlrot(istate,jsite)
c          call cpolar(rx,ry,rz,r1,a1,b1)
c          write(*,*) isite,jsite,r1
c          i1=i1+1
c          dum1=dum1+(r1-dum2)**2
cc        if (r1.gt.r1max.or.r1.lt.1.32d0) then
cc          dum1=dum1+1.d3
cc        endif
c        endif
c      enddo
c      qverl(istate,iconf)=dum1/i1*0.5

c      http://schubert.bio.uniroma1.it/corsostruttura/capitoli/capitolo1/testo1.htm
c      I 20 diversi aminoacidi
c      specificati dal codice genetico vengono legati l'uno all'altro
c      durante la biosintesi delle proteine attraverso la formazione di
c      legami peptidici. Il gruppo carbossilico del primo aminoacido
c      reagisce con quello aminico del secondo perdendo una molecola
c      d'acqua e formando il legame peptidico.  c cQuesto legame
c      (evidenziato in giallo) risulta rigido e planare con una lunghezza
c      di 1.32 Å, intermedia fra quella di un singolo legame C-N (1.49 Å)
c      e quella di un doppio legame carbonio azoto(1.27 Å). Questo
c      carattere di doppio legame (40%) si può spiegare con l'esistenza
c      di due strutture di risonanza. Di conseguenza, sono possibili due
c      configurazioni del legame peptidico, una in cui i carboni a sono
c      trans (riportata nella figura a destra), l'altra in cui sono
c      cis. La forma trans è energeticamente favorita, probabilmente a
c      causa di minori repulsioni fra atomi non legati. Se il residuo che
c      segue è la Prolina, la sua catena ciclica diminuisce le repulsioni
c      fra gli atomi e conseguentemente la stabilità dell'isomero cis è
c      confrontabile con quella del trans. Un esempio di legame peptidico
c      cis si trova nell'enzima umano citosolico serina
c      idrossimetiltrasferasi (codice pdb 1BJ4) tra i residui
c      Phe297-Pro298. Nell'immagine vrml i carboni a del legame peptidico
c      cis sono visualizzati come sfere.
c
c      return
c      end


      subroutine ciclo2

c.....This version includes the slit lenght effect

      include 'quafit2014.cmn'
      dimension ix(mxpar)

c      write(*,*) '   ---> ciclo   '

c.....Sequenza casuale di numeri fra 1 e mpar
c.....memorizzata in irand
      do i=1,mpar
        ix(i)=i
      enddo
      i1=mpar+1
      do 20 i=1,mpar
        i1=i1-1
        i2=1+int(rand(0)*dfloat(i1))
        irand(i)=ix(i2)
        do 20 j=i2+1,i1
          ix(j-1)=ix(j)
20    continue

c.....Inizio del ciclo       
      do 100 i=1,mpar
        fnew=zer
c.......scelta di un sito casuale
        ivar=irand(i)

        write(*,*) 'ivar',ivar,' '//varnam(ivar)
        call ciclo_sub(ivar)

        call fcalc
        
        chinew=fnew

        t_overl=zer
        t_qverl=zer
        t_cverl=zer
        dum2=zer
        i1=0
        i2=0
        do istate=1,nstate
          do iconf=1,nconf(istate)
            do ihagg=1,nhagg(istate)
              t_overl=t_overl+overl(istate,ihagg,iconf)
              i1=i1+1
              if (ihagg.eq.1) then
              write(*,*) 
     >' O ',i1,overl(istate,ihagg,iconf),overm(istate,ihagg,iconf)
              endif
            enddo
            t_qverl=t_qverl+qverl(istate,iconf)
            i2=i2+1
              write(*,*) 
     >' Q ',i2,qverl(istate,iconf),qverm(istate,iconf)
            t_cverl=t_cverl+cverl(istate,iconf)
              write(*,*) 
     >' C ',i2,cverl(istate,iconf),cverm(istate,iconf)
          enddo
        enddo
        if (i1.ne.0) t_overl=t_overl/i1
        if (i2.ne.0) t_qverl=t_qverl/i2
        if (i2.ne.0) t_cverl=t_cverl/i2
      
        fnew=fnew+w_overl*t_overl+w_kforce*t_qverl+t_cverl

        delta=(fold-fnew)/temp
        if (dabs(delta).gt.cutexp) delta=cutexp*dsign(1.d0,delta)

c.......Test di Metropolis
        if (delta.ge.zer) then
c               write(*,*) ' KK ',delta,1, mossa accettata
          varold(ivar)=varnew(ivar)
          fold=fnew
          chiold=chinew
          t_overl_old=t_overl
          t_qverl_old=t_qverl
          t_cverl_old=t_cverl
          varacc(ivar)=varacc(ivar)+one
          accmos=accmos+one
        else if (rand(0).le.dexp(delta)) then
c               write(*,*) ' KK ',delta,2, mossa accettata
          varold(ivar)=varnew(ivar)
          fold=fnew
          chiold=chinew
          t_overl_old=t_overl
          t_qverl_old=t_qverl
          t_cverl_old=t_cverl
          varacc(ivar)=varacc(ivar)+one
          accmos=accmos+one
        else
c               write(*,*) ' KK ',delta,3, mossa respinta
          varnew(ivar)=varold(ivar)
          call ciclo_res(ivar)
        endif  
100   continue

      return
      end


      subroutine fcalc

      include 'quafit2014.cmn'

c      write(*,*) '   ---> fcalc top  '

      f=fnew
      call minqbid(f)
      fnew=f
c      write(*,*) '   ---> fcalc bottom  '
      return
      end


      subroutine minqbid(f)

      include 'quafit2014.cmn'
      dimension  s      (mxexper)
      dimension  s_x1   (mxexper)
      dimension  s_x1x1 (mxexper)
      dimension  s_y    (mxexper)
      dimension  s_x1y  (mxexper)

c.....This version includes the slit lenght effect
      if (nexper.eq.0) return
c      write(*,*) '   ---> minqbid(f)   '

      do nn=1,nexper
        s      (nn)= 0.d0
        s_x1   (nn)= 0.d0
        s_x1x1 (nn)= 0.d0
        s_y    (nn)= 0.d0
        s_x1y  (nn)= 0.d0
        jplate (nn)= iplate(nn)

        do 40 iq=nqn1(nn),nqn(nn)
          q=qqn(nn,iq)
          call iqcalc(nn,iq)
          qit2n(nn,iq)=qit
          if (iq.gt.nqrn(nn)) goto 40
          dum1=qien(nn,iq)
          dum2=win(nn,iq)
          dum2=dum2*dum2
c          write(*,*) qit,' AAA ',dum1,dum2
          s      (nn)= s      (nn)+ dum2
          s_x1   (nn)= s_x1   (nn)+ dum2*qit
          s_x1x1 (nn)= s_x1x1 (nn)+ dum2*qit*qit
          s_x1y  (nn)= s_x1y  (nn)+ dum2*qit*dum1
          s_y    (nn)= s_y    (nn)+ dum2*dum1
40      continue
      enddo

c.....Minimi quadrati lineari a gruppi

333   do nf=1,nfamily
        r1 = 0.d0
        r2 = 0.d0
        do 222 nn=1,nexper
          if (ifamily(nn).ne.nf) goto 222
          jqipar=iqipar(nn)
	  qip=qipar(nn)
          r1 = r1 + s_x1y (nn)-jplate(nn)*s_x1(nn)*s_y (nn)/s(nn)
          r2 = r2 + s_x1x1(nn)-jplate(nn)*s_x1(nn)*s_x1(nn)/s(nn)
222     continue
          
c          write(*,*) ' NNFF ',nf,r1,r2
        if (jqipar.eq.1) then
          if (r2.ne.0.d0) then
            qip= r1/r2
          else
            qip= 1.d0
          endif
        endif
        if (qip.lt.0.d0) qip=1.d0
        do 223 nn=1,nexper
          if (ifamily(nn).ne.nf) goto 223
          plate(nn)=jplate(nn)*((s_y(nn)-qip*s_x1(nn))/s(nn))
          if (plate(nn).lt.0.d0) then
            jplate(nn)=0
            goto 333
          endif
	  qipar(nn)=qip
223     continue
      enddo

      do nn=1,nexper
        f1=0.d0
        do 50 iq=nqn1(nn),nqn(nn)
          dum3=qipar(nn)*qit2n(nn,iq)+plate(nn)
          dum2=(qien(nn,iq)-dum3)*win(nn,iq)
          f1=f1+dum2*dum2
          qit2n(nn,iq)=dum3
50      continue

c        write(*,*) f1,chipar(nn)
        f1=f1*chipar(nn)
        chi2(nn)=f1
        f=f+f1
      enddo

      f=f/nexper+fout
c      write(*,*) f

      return
      end




      subroutine subrun2

      include 'quafit2014.cmn'

      parameter(cutrap=1.d-8,delrap=0.1d0)
c      write(*,*) '   ---> subrun2   '

      accrap=half

      do 51 ivar=1,mpar
        vartot(ivar)=zer
        varacc(ivar)=zer
        rapave(ivar)=zer
        rapsqr(ivar)=zer
51    continue

      do 20 i=1,ncycl
        call ciclo2
 20   continue

      do 30 ivar=1,mpar
        rap=varacc(ivar)/vartot(ivar)

        if ((rap.lt.accrap-delrap).and.
     >     (vardel(ivar)*pushdn.ge.cutrap)) then
          vardel(ivar)=vardel(ivar)*pushdn
        else if ((rap.gt.accrap+delrap).and.
     >     (vardel(ivar)*pushup-varmax(ivar)+
     >      varmin(ivar).le.cutrap)) then
          vardel(ivar)=vardel(ivar)*pushup
        endif

        rapave(ivar)=rap 
        rapsqr(ivar)=rap*rap
30    continue


      do istate=1,nstate
        do iconf=1,nconf(istate)
          call test_constr(istate,iconf) 
        enddo
      enddo
c              write(*,*) ' fine subr'

      return
      end

      subroutine st_d1(n,x,xave,sabs,sprc)

c.....Calcola la deviazione standard sabs, la
c.....deviazione standard percentuale sprc e il valor medio
c.....xave di una distribuzione di n elementi x(i).

      implicit real*8 (a-h,o-z)
      parameter(tiny = 1.0d-10)
      parameter(zer=0.d0,one=1.d0,hundr=100.d0)
      dimension x(n)

      if (n.eq.1) then
        sabs = 0.d0
        sprc = 0.d0
        xave = x(1)
        return
      endif
      s1 = 0.d0
      s2 = 0.d0
      do 10 i=1,n
        dum1 = x(i)
        s1   = s1 + dum1
        s2   = s2 + dum1*dum1
10    continue
      dum1  = dfloat(n)
      xave  = s1 / dum1
      sabs = dsqrt( (s2 - s1*xave) / (dum1-one) )
      if (dabs(s1).lt.tiny) then
        sprc = 0.d0
      else
        sprc = hundr * sabs / dabs(xave)
      endif
      return
      end

      subroutine st_d2(n,s1,s2,sprc)

c.....Calcola la deviazione standard s2, la deviazione
c.....standard relativa sprc e la media s1 di una distribuzione di n elementi
c.....le cui somme dei singoli elementi e dei loro quadrati
c.....sono s1 e s2.

      implicit real*8 (a-h,o-z)
      parameter(zer=0.d0,one=1.d0,hundr=100.d0)
      parameter(tiny = 1.0d-10)

      if (n.eq.1) then
        sprc = 0.d0
        s2   = 0.d0
        return
      endif
      dum1  = dfloat(n)
      s2 = dsqrt( (dum1*s2 - s1*s1) / (dum1*(dum1-one)) )
      s1 = s1 / dum1
      if (dabs(s1).lt.tiny) then
        sprc = 0.d0
      else
        sprc = hundr * s2 / dabs(s1)
      endif
      return
      end

      subroutine normal(xave,sigma,p,x,iseed)

c.....Dato un valore medio xave con deviazione standard sigma
c.....viene estratto un valore x dalla distribuzione normale dell'errore.
c.... nell'intervallo xave-p*sigma - xave+p*sigma.
c.....iseed e' il seme per la funzione ran1 calcolato in ranin.

      implicit real*8 (a-h,o-z)
      parameter(zer=0.d0,half=0.5d0,one=1.d0,two=2.d0)

      a = - half * p * p
      b = dexp(a)
      c = one / (one - b)
c.....Inizio del loop
10      r = rand(0)
        r = two * r - one
        s = rand(0)
        y = (dexp(a*r*r) - b) * c
        if (s.gt.y) goto 10
c.....Fine del loop
      x = p * sigma * r + xave
      return
      end



      subroutine integ

      include 'quafit2014.cmn'

c      write(*,*) '   ---> integ   '

      nb=nz*nd
      na=nb
      call gauss0
      do 10 ia=1,na
        al(ia) = pi*(zero(ia)+1.d0)
        wa(ia) = weight(ia)
10    continue
      do 20 ib=1,nb
        cb(ib) = zero(ib)
        wb(ib) = weight(ib)
20    continue
      do i=1,nqgl
        wqgl(i) = 2.d0/(nqgl-1)
        qqgl(i) = qglmax/(nqgl-1)*(i-1)
      enddo
      wqgl(1) = 1.d0/(nqgl-1)
      wqgl(nqgl) = wqgl(1)
      return
      end


      subroutine cscsa(l,alpha)

      implicit real*8 (a-h,o-z)
 
      common/ccs/csa(2,0:30)
        
      dum1=0.d0
      do 10 i=1,l
        dum1=dum1+alpha
        csa(1,i)=dcos(dum1) 
        csa(2,i)=dsin(dum1) 
10    continue
      return
      end

      function pwign(l,m,n,x)

      implicit real*8 (a-h,o-z)

      parameter(mj=500)
      pwign=0.d0
      c01 = dcos(x/2)
      s01 = dsin(x/2)
      j=-1
20      j=j+1
        if (j.gt.mj) return
        if (l-n-j.lt.0) goto 20
        if (l+m-j.lt.0) goto 20
        if (j+n-m.lt.0) goto 20
        iq = 2*l+m-n-2*j
        c01q=1.d0
        do i=1,iq 
          c01q=c01q*c01
        enddo
        liq=2*l-iq
        s01q=1.d0
        do i=1,liq 
          s01q=s01q*s01
        enddo
        dum1 = (-1)**j* 
     >         dexp(0.5d0*(
     >        +gammln(1.d0+l+m)+gammln(1.d0+l-m)
     >        +gammln(1.d0+l+n)+gammln(1.d0+l-n))
     >        -gammln(1.d0+l-n-j)-gammln(1.d0+l+m-j)
     >        -gammln(1.d0+j+n-m)-gammln(1.d0+j))
        pwign = pwign + dum1 * c01q * s01q
      goto 20
      end

      FUNCTION gammln(xx)
      REAL*8 gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
c      write(*,*) '   ---> gammln   '
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END


      subroutine gauss0

c.....La routine memorizza nei vettori zero e weight gli zeri
c.....e i pesi per l'integrazione di Gauss_Legendre con polinomi
c.....di grado nz (possibili valori 2, 4, 8, 16 e 32)
c.....e un numero di suddivisioni dell'intervallo
c.....pari a nd. Se il numero di zeri e' pari a 1, vengono
c.....memorizzati gli zeri e i pesi per un'integrazione a rettangolini
c.....con il primo zero pari a -1 (estremo inferiore intervallo di
c.....integrazione). Il numero totale di punti e' np.

c.....Gli integrali vengono calcolati secondo la formula:

c                          nz   nd
c        _  b              ---  ---  w            z + 2i - 1
c        |            b-a  \    \     j     b-a    j
c        |  f(x)dx = ----- /    /   --- f{ -----[(---------- - 1) + 1] + a }
c       _|             2   ---  --- nd       2        nd
c         a                j=1  i=1

      include 'quafit2014.cmn'

      real*8 log2
      dimension zpl(31),wgt(31)
      parameter(tiny=1.d-07)
      parameter(log2 = 1.442695040888963407359924681d0)
c.....log2 = 1/Log(2)

c.....zeros p2
      data zpl(1) /0.577350269189625764509148780502d0/
c.....weights p2
      data wgt(1) /1.d0/
c.....zeros p4
      data (zpl(i),i=2,3)/
     1 0.86113631159405257522394648889282d0,
     2 0.33998104358485626480266575910326d0/
c.....weights p4
      data (wgt(i),i=2,3)/
     1 0.34785484513745385737306394922198d0,
     2 0.65214515486254614262693605077802d0/
c.....zeros p8
      data (zpl(i),i=4,7)/
     1 0.960289856497536d0, 0.796666477413627d0,
     2 0.525532409916329d0, 0.183434642495650d0/
c.....weight p8
      data (wgt(i),i=4,7)/
     1 0.101228536290376d0, 0.222381034453374d0,
     2 0.313706645877887d0, 0.362683783378362d0/
c.....zeros p16
      data (zpl(i),i=8,15)/
     1 0.989400934991649932596d0, 0.944575023073232576078d0,
     2 0.865631202387831743880d0, 0.755404408355003033895d0,
     3 0.617876244402643748447d0, 0.458016777657227386342d0,
     4 0.281603550779258913230d0, 0.095012509837637440185d0/
c.....weight p16
      data (wgt(i),i=8,15)/
     1 0.027152459411754094852d0, 0.062253523938647892863d0,
     2 0.095158511682492784810d0, 0.124628971255533872052d0,
     3 0.149595988816576732081d0, 0.169156519395002538189d0,
     4 0.182603415044923588867d0, 0.189450610455068496285d0/
c.....zeros p32
      data (zpl(i),i=16,31)/
     1 0.997263861849481563545d0, 0.985611511545268335400d0,
     2 0.964762255587506430774d0, 0.934906075937739689171d0,
     3 0.896321155766052123965d0, 0.849367613732569970134d0,
     4 0.794483795967942406963d0, 0.732182118740289680387d0,
     5 0.663044266930215200975d0, 0.587715757240762329041d0,
     6 0.506899908932229390024d0, 0.421351276130635345364d0,
     7 0.331868602282127649780d0, 0.239287362252137074545d0,
     8 0.144471961582796493485d0, 0.048307665687738316235d0/
c.....weight p32
      data (wgt(i),i=16,31)/
     1 0.007018610009470096600d0, 0.016274394730905670605d0,
     2 0.025392065309262059456d0, 0.034273862913021433103d0,
     3 0.042835898022226680657d0, 0.050998059262376176196d0,
     4 0.058684093478535547145d0, 0.065822222776361846838d0,
     5 0.072345794108848506225d0, 0.078193895787070306472d0,
     6 0.083311924226946755222d0, 0.087652093004403811143d0,
     7 0.091173878695763884713d0, 0.093844399080804565639d0,
     8 0.095638720079274859419d0, 0.096540088514727800567d0/

c      write(*,*) '   ---> gauss0   '

      if (nz.gt.1) then
c.......Integrazione con nz zeri e nd intervallini
        dum1 = dfloat(nz)
        i1 = dlog(dum1)*log2+0.5d0
        dum1 = dabs(dum1-two**i1)
        if ((i1.gt.5).or.(dum1.gt.tiny)) then
          stop 'nz'
        endif
        np = nz*nd
        if (np.gt.mxp) then
          stop 'mxp'
        endif
        i1 = nz/2
        dum1 = one/dfloat(nd)
        do 10 i=1,nd
          i2 = i1-1
          i3 = (i-1)*nz
          i4 = i*nz+1
          dum2 = dfloat(2*i-1)
          do 10 j=1,i1
            i2 = i2+1
            i3 = i3+1
            i4 = i4-1
            dum3 = wgt(i2)*dum1
            weight(i3) = dum3
            weight(i4) = dum3
            zero(i3)   = (-zpl(i2)+dum2)*dum1-one
            zero(i4)   = ( zpl(i2)+dum2)*dum1-one
10      continue
      else
c.......Integrazione con nd intervallini, con il primo zero
c.......coincidente con il primo estremo di integrazione
c.......(formula precedente con z(1)=-1)
        np = nd
        if (np.gt.mxp) then
          stop 'mxp'
        endif
        dum1 = one/dfloat(nd)
        do 20 i=1,nd
          weight(i) = two*dum1
          zero(i)   = dfloat(2*i-2)*dum1-one
20      continue
      endif
      return
1000  format(/,1x,'Non sono disponibili gli zeri di rango ',i2)
1100  format(/,1x,'Numero punti superiore a ',i2)
      end




      subroutine ranin(iseed)

c rand.f
c
c   FUNCTION RAND (R)
C***BEGIN PROLOGUE  RAND
C***PURPOSE  Generate a uniformly distributed random number.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  L6A21
C***TYPE      SINGLE PRECISION (RAND-S)
C***KEYWORDS  FNLIB, RANDOM NUMBER, SPECIAL FUNCTIONS, UNIFORM
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C      This pseudo-random number generator is portable among a wide
C variety of computers.  RAND(R) undoubtedly is not as good as many
C readily available installation dependent versions, and so this
C routine is not recommended for widespread usage.  Its redeeming
C feature is that the exact same random numbers (to within final round-
C off error) can be generated from machine to machine.  Thus, programs
C that make use of random numbers can be easily transported to and
C checked in a new environment.
C
C      The random numbers are generated by the linear congruential
C method described, e.g., by Knuth in Seminumerical Methods (p.9),
C Addison-Wesley, 1969.  Given the I-th number of a pseudo-random
C sequence, the I+1 -st number is generated from
C             X(I+1) = (A*X(I) + C) MOD M,
C where here M = 2**22 = 4194304, C = 1731 and several suitable values
C of the multiplier A are discussed below.  Both the multiplier A and
C random number X are represented in double precision as two 11-bit
C words.  The constants are chosen so that the period is the maximum
C possible, 4194304.
C
C      In order that the same numbers be generated from machine to
C machine, it is necessary that 23-bit integers be reducible modulo
C 2**11 exactly, that 23-bit integers be added exactly, and that 11-bit
C integers be multiplied exactly.  Furthermore, if the restart option
C is used (where R is between 0 and 1), then the product R*2**22 =
C R*4194304 must be correct to the nearest integer.
C
C      The first four random numbers should be .0004127026,
C .6750836372, .1614754200, and .9086198807.  The tenth random number
C is .5527787209, and the hundredth is .3600893021 .  The thousandth
C number should be .2176990509 .
C
C      In order to generate several effectively independent sequences
C with the same generator, it is necessary to know the random number
C for several widely spaced calls.  The I-th random number times 2**22,
C where I=K*P/8 and P is the period of the sequence (P = 2**22), is
C still of the form L*P/8.  In particular we find the I-th random
C number multiplied by 2**22 is given by
C I   =  0  1*P/8  2*P/8  3*P/8  4*P/8  5*P/8  6*P/8  7*P/8  8*P/8
C RAND=  0  5*P/8  2*P/8  7*P/8  4*P/8  1*P/8  6*P/8  3*P/8  0
C Thus the 4*P/8 = 2097152 random number is 2097152/2**22.
C
C      Several multipliers have been subjected to the spectral test
C (see Knuth, p. 82).  Four suitable multipliers roughly in order of
C goodness according to the spectral test are
C    3146757 = 1536*2048 + 1029 = 2**21 + 2**20 + 2**10 + 5
C    2098181 = 1024*2048 + 1029 = 2**21 + 2**10 + 5
C    3146245 = 1536*2048 +  517 = 2**21 + 2**20 + 2**9 + 5
C    2776669 = 1355*2048 + 1629 = 5**9 + 7**7 + 1
C
C      In the table below LOG10(NU(I)) gives roughly the number of
C random decimal digits in the random numbers considered I at a time.
C C is the primary measure of goodness.  In both cases bigger is better.
C
C                   LOG10 NU(I)              C(I)
C       A       I=2  I=3  I=4  I=5    I=2  I=3  I=4  I=5
C
C    3146757    3.3  2.0  1.6  1.3    3.1  1.3  4.6  2.6
C    2098181    3.3  2.0  1.6  1.2    3.2  1.3  4.6  1.7
C    3146245    3.3  2.2  1.5  1.1    3.2  4.2  1.1  0.4
C    2776669    3.3  2.1  1.6  1.3    2.5  2.0  1.9  2.6
C   Best
C    Possible   3.3  2.3  1.7  1.4    3.6  5.9  9.7  14.9
C
C             Input Argument --
C R      If R=0., the next random number of the sequence is generated.
C        If R .LT. 0., the last generated number will be returned for
C          possible use in a restart procedure.
C        If R .GT. 0., the sequence of random numbers will start with
C          the seed R mod 1.  This seed is also returned as the value of
C          RAND provided the arithmetic is done exactly.
C
C             Output Value --
C RAND   a pseudo-random number between 0. and 1.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  RAND

      implicit real*8 (a-h,o-z)
      integer*4 iseed
      character*8 clo*8, clo2*10
      call date_and_time(TIME=clo2)
      read(clo2,'(i6,1x,i3)') i1,i2
      iseed=i1*100+i2
      write(*,*) ' Seed ',iseed
      s=rand(-iseed)
      return
100   format(i2,2(1x,i2))
      end

c      implicit real*8 (a-h,o-z)
c      integer*4 iseed
c      character*8 clo
c      call time(clo)
c      read(clo,100) ih,im,is
c      iseed = (ih+im*60+is*3600)*2+1
c      write(*,*) ' Seed ',iseed
c      s=rand(-iseed)
c      return
c100   format(i2,2(1x,i2))
c      end

      subroutine cpolar(x,y,z,r,alpha,beta)

      implicit real*8 (a-h,o-z)
      
      r=dsqrt(x*x+y*y+z*z)
      if (r.eq.0.d0) then
        alpha=0.d0
        beta=0.d0
        return
      endif
      s=1.d0/r
      ux=x*s
      uy=y*s
      uz=z*s
      if (uz.gt.1.d0) uz=1.d0
      if (uz.lt.-1.d0) uz=-1.d0
      beta=dacos(uz)
      if (dabs(uz).eq.1.d0) then
        alpha=0.d0
      else
        s=1.d0/sfromc(uz)
        alpha=polar(ux*s,uy*s)
      endif
      return
      end      


      function polar(cosine,sine)

      implicit real*8 (a-h,o-z)

      parameter(pi2    = 6.28318530717958647692528676656d0)

c      write(*,*) '   ---> polar   '
      if (cosine.gt.1.d0) cosine=1.d0
      if (cosine.lt.-1.d0) cosine=-1.d0
      polar = dacos(cosine)
      if (sine.lt.0.d0) then
        polar=pi2-polar
c        polar=-polar
      endif
      return
      end


      block data symm

      include 'quafit2014.cmn'

      data csa(1,0)/1.d0/
      data csa(2,0)/0.d0/
 
      end



      block data atoms
c    !! it should be after the main program, 22/06/2007

c.....Assegnazione variabili atom (simboli chimici degli atomi
c.....della tavola periodica), mass (numeri di massa degli isotopi
c.....piu' stabili), rvdw e rcov (raggio van der Waals e covalente 
c.....degli atomi,
c.....in Angstroms, tratti da J.E. Huheey, "Chimica Inorganica",
c.....pag. 184, Piccin, Padova. Per gli atomi di cui non e' riportato
c.....il raggio van der Waals si e' posto il raggio pari a 1.7 Angs.)
c.....Si assegnano anche i valori dell'array pris: 0, +/- pris(i)
c.....sono i 3 stati conformazionali (in deg) dell'angolo i.

      include 'quafit2014.cmn'

      data (methtype(i),i=1,2)/
     >'st',
     >'el'/

      data (condtype(i),i=1,7)/
     >'=  ',
     >'>  ',
     >'>= ',
     >'<  ',
     >'<= ',
     >'max',
     >'min'/

      data (coordtype(i),i=1,8)/
     >'x ',
     >'y ',
     >'z ',
     >'r ',
     >'a ',
     >'b ',
     >'rc',
     >'ac'/


      data (nnaac(i),i=1,8)/
     >' DA',
     >' DC',
     >' DG',
     >' DT',
     >'  A',
     >'  C',
     >'  G',
     >'  U'/


!1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28

      data ((inapei(i,j),j=1,28),i=1,8)/ !dice per ognuno dei 4- desossi acidi nucleici (DA DC DG DT) e dei 4 acidi nucleici (A C G U)
c####################
c DNA
c####################
c     P OP1 OP2 C1' C2' C3' C4' C5' O2' O3' O4' O5' C2 C4 C5 C6 C7 C8 N1 N2 N3 N4 N6 N7 N9 O2 O4 O6
     >13,9,9,2,3,2,2,3,0,9,9,9,2,1,1,1,0,2,5,0,5,0,7,5,5,0,0,0,! DA
c     P O O CH CH2 CH CH CH2 OH O O O CH C C C  CH N  N  NH2 N N   
     >13,9,9,2,3,2,2,3,0,9,9,9,1,1,2,2,0,0,5,0,5,7,0,0,0,9,0,0,! DC
c     P O O CH CH2 CH CH CH2 OH O O O C C CH CH   N  N NH2    O  
     >13,9,9,2,3,2,2,3,0,9,9,9,1,1,1,1,0,2,6,7,5,0,0,5,5,0,0,9,! DG
c     P O O CH CH2 CH CH CH2 OH O O O C C C C  CH NH NH2 N   N N   O
     >13,9,9,2,3,2,2,3,0,9,9,9,1,1,1,2,4,0,5,0,6,0,0,0,0,9,9,0,! DT
c     P O O CH CH2 CH CH CH2 OH O O O C C C CH CH3  N  NH     O O 
c####################
c RNA
c####################
c     P OP1 OP2 C1' C2' C3' C4' C5' O2' O3' O4' O5' C2 C4 C5 C6 C7 C8 N1 N2 N3 N4 N6 N7 N9 O2 O4 O6
     >13,9,9,2,2,2,2,3,10,9,9,9,2,1,1,1,0,2,5,0,5,0,7,5,5,0,0,0,! A
c     P O O CH CH CH CH CH2 OH O O O CH C C C  CH N  N  NH2 N N   
     >13,9,9,2,2,2,2,3,10,9,9,9,1,1,2,2,0,0,5,0,5,7,0,0,0,9,0,0,! C
c     P O O CH CH CH CH CH2 OH O O O C C CH CH   N  N NH2    O  
     >13,9,9,2,2,2,2,3,10,9,9,9,1,1,1,1,0,2,6,7,5,0,0,5,5,0,0,9,! G
c     P O O CH CH CH CH CH2 OH O O O C C C C  CH NH NH2 N   N N   O
     >13,9,9,2,2,2,2,3,10,9,9,9,1,1,2,2,0,0,5,0,6,0,0,0,0,9,9,0/! U
c     P O O CH CH CH CH CH2 OH O O O C C CH CH  N  NH     O O 


      data (anapei(i,1),i=1,28)/  ! sono i tipi di pseudo-atomi degli acidi nucleici
     >"P  ", ! 1
     >"OP1", ! 2
     >"OP2", ! 3
     >"C1''", ! 4
     >"C2''", ! 5
     >"C3''", ! 6
     >"C4''", ! 7
     >"C5''", ! 8
     >"O2''", ! 9
     >"O3''", ! 10
     >"O4''", ! 11
     >"O5''", ! 12
     >"C2 ", ! 13
     >"C4 ", ! 14
     >"C5 ", ! 15
     >"C6 ", ! 16
     >"C7 ", ! 17
     >"C8 ", ! 18
     >"N1 ", ! 19
     >"N2 ", ! 20
     >"N3 ", ! 21
     >"N4 ", ! 22
     >"N6 ", ! 23
     >"N7 ", ! 24
     >"N9 ", ! 25
     >"O2 ", ! 26
     >"O4 ", ! 27
     >"O6 "/ ! 28

      data (anapei(i,2),i=1,28)/  ! sono i tipi di pseudo-atomi degli acidi nucleici
     >"P  ", ! 1
     >"O1P", ! 2
     >"O1P", ! 3
     >"C1*", ! 4
     >"C2*", ! 5
     >"C3*", ! 6
     >"C4*", ! 7
     >"C5*", ! 8
     >"O2*", ! 9
     >"O3*", ! 10
     >"O4*", ! 11
     >"O5*", ! 12
     >"C2 ", ! 13
     >"C4 ", ! 14
     >"C5 ", ! 15
     >"C6 ", ! 16
     >"C7 ", ! 17
     >"C8 ", ! 18
     >"N1 ", ! 19
     >"N2 ", ! 20
     >"N3 ", ! 21
     >"N4 ", ! 22
     >"N6 ", ! 23
     >"N7 ", ! 24
     >"N9 ", ! 25
     >"O2 ", ! 26
     >"O4 ", ! 27
     >"O6 "/ ! 28

      data (atom(i),i=1,103)/
     >'H ','He','Li','Be','B ','C ','N ','O ','F ','Ne',
     >'Na','Mg','Al','Si','P ','S ','Cl','Ar','K ','Ca',
     >'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn',
     >'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y ','Zr',
     >'Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd','In','Sn',
     >'Sb','Te','I ','Xe','Cs','Ba','La','Ce','Pr','Nd',
     >'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb',
     >'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg',
     >'Tl','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th',
     >'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm',
     >'Md','No','Lr'/
      data (inhamm(i),i=1,22)/
     >   5,!       0.0 ALA
     >  11,!       0.0 ARG
     >   8,!       0.0 ASN
     >   8,!       0.0 ASP
     >   6,!       0.0 CYS
     >   9,!       0.0 GLN
     >   9,!       0.0 GLU
     >   4,!       0.0 GLY
     >  10,!       0.0 HIS
     >   8,!       0.0 ILE
     >   8,!       0.0 LEU
     >   9,!       0.0 LYS
     >   8,!       0.0 MET
     >  11,!       0.0 PHE
     >   7,!       0.0 PRO
     >   6,!       0.0 SER
     >   7,!       0.0 THR
     >  14,!       0.0 TRP
     >  12,!       0.0 TYR
     >   7,!       0.0 VAL
     >   5,!       0.0 CSE
     >   7/!       0.0 MSE

c http://en.wikipedia.org/wiki/Selenocysteine CSE n. 21
c http://en.wikipedia.org/wiki/Selenomethionine MSE n. 22

      data (amm(i),amm1(i),i=1,22)/
     >'ALA','A', !01
     >'ARG','R', !02
     >'ASN','N', !03
     >'ASP','D', !04
     >'CYS','C', !05
     >'GLN','Q', !06
     >'GLU','E', !07
     >'GLY','G', !08
     >'HIS','H', !09
     >'ILE','I', !10
     >'LEU','L', !11
     >'LYS','K', !12
     >'MET','M', !13
     >'PHE','F', !14
     >'PRO','P', !15
     >'SER','S', !16
     >'THR','T', !17
     >'TRP','W', !18
     >'TYR','Y', !19
     >'VAL','V', !20
     >'CSE','c', !21
     >'MSE','m'/ !22

c ALA
      data ijadj( 1, 1,1)/ 6/! NH
      data ijadj( 1, 1,2)/ 1/! N 
      data ijadj( 1, 2,1)/ 2/! CH
      data ijadj( 1, 2,2)/ 2/! CA
      data ijadj( 1, 3,1)/ 1/! C 
      data ijadj( 1, 3,2)/ 3/! C 
      data ijadj( 1, 4,1)/ 9/! O 
      data ijadj( 1, 4,2)/21/! O 
      data ijadj( 1, 5,1)/ 4/! CH
      data ijadj( 1, 5,2)/ 4/! CB
      data natoadj( 1)/ 6/
      data cooadj( 1, 1,1)/    -0.78800/
      data cooadj( 1, 1,2)/     1.17000/
      data cooadj( 1, 1,3)/     0.41000/
      data cooadj( 1, 2,1)/     0.00000/
      data cooadj( 1, 2,2)/     0.00000/
      data cooadj( 1, 2,3)/     0.00000/
      data cooadj( 1, 3,1)/     1.40700/
      data cooadj( 1, 3,2)/     0.00000/
      data cooadj( 1, 3,3)/     0.59000/
      data cooadj( 1, 4,1)/     2.41600/
      data cooadj( 1, 4,2)/     0.00000/
      data cooadj( 1, 4,3)/    -0.14800/
      data cooadj( 1, 5,1)/     0.00000/
      data cooadj( 1, 5,2)/    -0.00000/
      data cooadj( 1, 5,3)/    -1.52300/
      data cooadj( 1, 6,1)/     0.00000/
      data cooadj( 1, 6,2)/     0.00000/
      data cooadj( 1, 6,3)/    -0.76150/
c... CH     1.73000
      data rmadj( 1)/     2.49150/
c ARG
      data ijadj( 2, 1,1)/ 6/! NH
      data ijadj( 2, 1,2)/ 1/! N 
      data ijadj( 2, 2,1)/ 2/! CH
      data ijadj( 2, 2,2)/ 2/! CA
      data ijadj( 2, 3,1)/ 1/! C 
      data ijadj( 2, 3,2)/ 3/! C 
      data ijadj( 2, 4,1)/ 9/! O 
      data ijadj( 2, 4,2)/21/! O 
      data ijadj( 2, 5,1)/ 3/! CH
      data ijadj( 2, 5,2)/ 4/! CB
      data ijadj( 2, 6,1)/ 3/! CH
      data ijadj( 2, 6,2)/ 5/! CG
      data ijadj( 2, 7,1)/ 3/! CH
      data ijadj( 2, 7,2)/ 6/! CD
      data ijadj( 2, 8,1)/ 6/! NH
      data ijadj( 2, 8,2)/12/! NE
      data ijadj( 2, 9,1)/ 1/! C 
      data ijadj( 2, 9,2)/ 8/! CZ
      data ijadj( 2,10,1)/ 7/! NH
      data ijadj( 2,10,2)/14/! NH
      data ijadj( 2,11,1)/ 7/! NH
      data ijadj( 2,11,2)/14/! NH
      data natoadj( 2)/12/
      data cooadj( 2, 1,1)/    -0.78800/
      data cooadj( 2, 1,2)/     1.17000/
      data cooadj( 2, 1,3)/     0.41000/
      data cooadj( 2, 2,1)/     0.00000/
      data cooadj( 2, 2,2)/     0.00000/
      data cooadj( 2, 2,3)/     0.00000/
      data cooadj( 2, 3,1)/     1.40700/
      data cooadj( 2, 3,2)/     0.00000/
      data cooadj( 2, 3,3)/     0.59000/
      data cooadj( 2, 4,1)/     2.41600/
      data cooadj( 2, 4,2)/     0.00000/
      data cooadj( 2, 4,3)/    -0.14800/
      data cooadj( 2, 5,1)/    -0.00000/
      data cooadj( 2, 5,2)/     0.00000/
      data cooadj( 2, 5,3)/    -1.52300/
      data cooadj( 2, 6,1)/    -1.09100/
      data cooadj( 2, 6,2)/    -0.88100/
      data cooadj( 2, 6,3)/    -2.17800/
      data cooadj( 2, 7,1)/    -1.05200/
      data cooadj( 2, 7,2)/    -0.74700/
      data cooadj( 2, 7,3)/    -3.68800/
      data cooadj( 2, 8,1)/    -0.32300/
      data cooadj( 2, 8,2)/    -1.86900/
      data cooadj( 2, 8,3)/    -4.30100/
      data cooadj( 2, 9,1)/    -0.88700/
      data cooadj( 2, 9,2)/    -3.06800/
      data cooadj( 2, 9,3)/    -4.56200/
      data cooadj( 2,10,1)/    -1.84100/
      data cooadj( 2,10,2)/    -3.56800/
      data cooadj( 2,10,3)/    -3.76300/
      data cooadj( 2,11,1)/    -0.49000/
      data cooadj( 2,11,2)/    -3.74400/
      data cooadj( 2,11,3)/    -5.62500/
      data cooadj( 2,12,1)/    -0.71050/
      data cooadj( 2,12,2)/    -1.73462/
      data cooadj( 2,12,3)/    -3.20500/
c... CH     1.73000
      data rmadj( 2)/     5.44292/
c ASN
      data ijadj( 3, 1,1)/ 6/! NH
      data ijadj( 3, 1,2)/ 1/! N 
      data ijadj( 3, 2,1)/ 2/! CH
      data ijadj( 3, 2,2)/ 2/! CA
      data ijadj( 3, 3,1)/ 1/! C 
      data ijadj( 3, 3,2)/ 3/! C 
      data ijadj( 3, 4,1)/ 9/! O 
      data ijadj( 3, 4,2)/21/! O 
      data ijadj( 3, 5,1)/ 3/! CH
      data ijadj( 3, 5,2)/ 4/! CB
      data ijadj( 3, 6,1)/ 1/! C 
      data ijadj( 3, 6,2)/ 5/! CG
      data ijadj( 3, 7,1)/ 9/! O 
      data ijadj( 3, 7,2)/16/! OD
      data ijadj( 3, 8,1)/ 7/! NH
      data ijadj( 3, 8,2)/11/! ND
      data natoadj( 3)/ 9/
      data cooadj( 3, 1,1)/    -0.78800/
      data cooadj( 3, 1,2)/     1.17000/
      data cooadj( 3, 1,3)/     0.41000/
      data cooadj( 3, 2,1)/     0.00000/
      data cooadj( 3, 2,2)/     0.00000/
      data cooadj( 3, 2,3)/     0.00000/
      data cooadj( 3, 3,1)/     1.40700/
      data cooadj( 3, 3,2)/     0.00000/
      data cooadj( 3, 3,3)/     0.59000/
      data cooadj( 3, 4,1)/     2.41600/
      data cooadj( 3, 4,2)/     0.00000/
      data cooadj( 3, 4,3)/    -0.14800/
      data cooadj( 3, 5,1)/     0.00000/
      data cooadj( 3, 5,2)/    -0.00000/
      data cooadj( 3, 5,3)/    -1.52300/
      data cooadj( 3, 6,1)/     1.15300/
      data cooadj( 3, 6,2)/     0.79900/
      data cooadj( 3, 6,3)/    -2.09400/
      data cooadj( 3, 7,1)/     1.22400/
      data cooadj( 3, 7,2)/     2.11800/
      data cooadj( 3, 7,3)/    -1.66400/
      data cooadj( 3, 8,1)/     1.96100/
      data cooadj( 3, 8,2)/     0.38800/
      data cooadj( 3, 8,3)/    -2.90900/
      data cooadj( 3, 9,1)/     0.86760/
      data cooadj( 3, 9,2)/     0.66100/
      data cooadj( 3, 9,3)/    -1.63800/
c... CH     1.73000
      data rmadj( 3)/     3.69792/
c ASP
      data ijadj( 4, 1,1)/ 6/! NH
      data ijadj( 4, 1,2)/ 1/! N 
      data ijadj( 4, 2,1)/ 2/! CH
      data ijadj( 4, 2,2)/ 2/! CA
      data ijadj( 4, 3,1)/ 1/! C 
      data ijadj( 4, 3,2)/ 3/! C 
      data ijadj( 4, 4,1)/ 9/! O 
      data ijadj( 4, 4,2)/21/! O 
      data ijadj( 4, 5,1)/ 3/! CH
      data ijadj( 4, 5,2)/ 4/! CB
      data ijadj( 4, 6,1)/ 1/! C 
      data ijadj( 4, 6,2)/ 5/! CG
      data ijadj( 4, 7,1)/ 9/! O 
      data ijadj( 4, 7,2)/16/! OD
      data ijadj( 4, 8,1)/ 9/! O 
      data ijadj( 4, 8,2)/16/! OD
      data natoadj( 4)/ 9/
      data cooadj( 4, 1,1)/    -0.78800/
      data cooadj( 4, 1,2)/     1.17000/
      data cooadj( 4, 1,3)/     0.41000/
      data cooadj( 4, 2,1)/     0.00000/
      data cooadj( 4, 2,2)/     0.00000/
      data cooadj( 4, 2,3)/     0.00000/
      data cooadj( 4, 3,1)/     1.40700/
      data cooadj( 4, 3,2)/     0.00000/
      data cooadj( 4, 3,3)/     0.59000/
      data cooadj( 4, 4,1)/     2.41600/
      data cooadj( 4, 4,2)/     0.00000/
      data cooadj( 4, 4,3)/    -0.14800/
      data cooadj( 4, 5,1)/    -0.00000/
      data cooadj( 4, 5,2)/    -0.00000/
      data cooadj( 4, 5,3)/    -1.52300/
      data cooadj( 4, 6,1)/     0.53700/
      data cooadj( 4, 6,2)/     1.29700/
      data cooadj( 4, 6,3)/    -2.09400/
      data cooadj( 4, 7,1)/     1.43700/
      data cooadj( 4, 7,2)/     1.94600/
      data cooadj( 4, 7,3)/    -1.59100/
      data cooadj( 4, 8,1)/     0.05700/
      data cooadj( 4, 8,2)/     1.65600/
      data cooadj( 4, 8,3)/    -3.13400/
      data cooadj( 4, 9,1)/     0.40620/
      data cooadj( 4, 9,2)/     0.97980/
      data cooadj( 4, 9,3)/    -1.66840/
c... CH     1.73000
      data rmadj( 4)/     3.70701/
c CYS
      data ijadj( 5, 1,1)/ 6/! NH
      data ijadj( 5, 1,2)/ 1/! N 
      data ijadj( 5, 2,1)/ 2/! CH
      data ijadj( 5, 2,2)/ 2/! CA
      data ijadj( 5, 3,1)/ 1/! C 
      data ijadj( 5, 3,2)/ 3/! C 
      data ijadj( 5, 4,1)/ 9/! O 
      data ijadj( 5, 4,2)/21/! O 
      data ijadj( 5, 5,1)/ 3/! CH
      data ijadj( 5, 5,2)/ 4/! CB
      data ijadj( 5, 6,1)/11/! S 
      data ijadj( 5, 6,2)/19/! SG
      data natoadj( 5)/ 7/
      data cooadj( 5, 1,1)/    -0.78800/
      data cooadj( 5, 1,2)/     1.17000/
      data cooadj( 5, 1,3)/     0.41000/
      data cooadj( 5, 2,1)/     0.00000/
      data cooadj( 5, 2,2)/     0.00000/
      data cooadj( 5, 2,3)/     0.00000/
      data cooadj( 5, 3,1)/     1.40700/
      data cooadj( 5, 3,2)/     0.00000/
      data cooadj( 5, 3,3)/     0.59000/
      data cooadj( 5, 4,1)/     2.41600/
      data cooadj( 5, 4,2)/     0.00000/
      data cooadj( 5, 4,3)/    -0.14800/
      data cooadj( 5, 5,1)/    -0.00000/
      data cooadj( 5, 5,2)/     0.00000/
      data cooadj( 5, 5,3)/    -1.52300/
      data cooadj( 5, 6,1)/    -0.68500/
      data cooadj( 5, 6,2)/     1.25900/
      data cooadj( 5, 6,3)/    -2.77000/
      data cooadj( 5, 7,1)/    -0.22833/
      data cooadj( 5, 7,2)/     0.41967/
      data cooadj( 5, 7,3)/    -1.43100/
c... CH     1.73000
      data rmadj( 5)/     3.23865/
c GLN
      data ijadj( 6, 1,1)/ 6/! NH
      data ijadj( 6, 1,2)/ 1/! N 
      data ijadj( 6, 2,1)/ 2/! CH
      data ijadj( 6, 2,2)/ 2/! CA
      data ijadj( 6, 3,1)/ 1/! C 
      data ijadj( 6, 3,2)/ 3/! C 
      data ijadj( 6, 4,1)/ 9/! O 
      data ijadj( 6, 4,2)/21/! O 
      data ijadj( 6, 5,1)/ 3/! CH
      data ijadj( 6, 5,2)/ 4/! CB
      data ijadj( 6, 6,1)/ 3/! CH
      data ijadj( 6, 6,2)/ 5/! CG
      data ijadj( 6, 7,1)/ 1/! C 
      data ijadj( 6, 7,2)/ 6/! CD
      data ijadj( 6, 8,1)/ 9/! O 
      data ijadj( 6, 8,2)/17/! OE
      data ijadj( 6, 9,1)/ 7/! NH
      data ijadj( 6, 9,2)/12/! NE
      data natoadj( 6)/10/
      data cooadj( 6, 1,1)/    -0.78800/
      data cooadj( 6, 1,2)/     1.17000/
      data cooadj( 6, 1,3)/     0.41000/
      data cooadj( 6, 2,1)/     0.00000/
      data cooadj( 6, 2,2)/     0.00000/
      data cooadj( 6, 2,3)/     0.00000/
      data cooadj( 6, 3,1)/     1.40700/
      data cooadj( 6, 3,2)/     0.00000/
      data cooadj( 6, 3,3)/     0.59000/
      data cooadj( 6, 4,1)/     2.41600/
      data cooadj( 6, 4,2)/     0.00000/
      data cooadj( 6, 4,3)/    -0.14800/
      data cooadj( 6, 5,1)/    -0.00000/
      data cooadj( 6, 5,2)/     0.00000/
      data cooadj( 6, 5,3)/    -1.52300/
      data cooadj( 6, 6,1)/    -0.33100/
      data cooadj( 6, 6,2)/     1.30500/
      data cooadj( 6, 6,3)/    -2.08600/
      data cooadj( 6, 7,1)/    -1.08300/
      data cooadj( 6, 7,2)/     1.25100/
      data cooadj( 6, 7,3)/    -3.42300/
      data cooadj( 6, 8,1)/    -0.40100/
      data cooadj( 6, 8,2)/     1.30000/
      data cooadj( 6, 8,3)/    -4.50300/
      data cooadj( 6, 9,1)/    -2.33800/
      data cooadj( 6, 9,2)/     1.09800/
      data cooadj( 6, 9,3)/    -3.39800/
      data cooadj( 6,10,1)/    -0.69217/
      data cooadj( 6,10,2)/     0.82567/
      data cooadj( 6,10,3)/    -2.48883/
c... CH     1.73000
      data rmadj( 6)/     4.44203/
c GLU
      data ijadj( 7, 1,1)/ 6/! NH
      data ijadj( 7, 1,2)/ 1/! N 
      data ijadj( 7, 2,1)/ 2/! CH
      data ijadj( 7, 2,2)/ 2/! CA
      data ijadj( 7, 3,1)/ 1/! C 
      data ijadj( 7, 3,2)/ 3/! C 
      data ijadj( 7, 4,1)/ 9/! O 
      data ijadj( 7, 4,2)/21/! O 
      data ijadj( 7, 5,1)/ 3/! CH
      data ijadj( 7, 5,2)/ 4/! CB
      data ijadj( 7, 6,1)/ 3/! CH
      data ijadj( 7, 6,2)/ 5/! CG
      data ijadj( 7, 7,1)/ 1/! C 
      data ijadj( 7, 7,2)/ 6/! CD
      data ijadj( 7, 8,1)/ 9/! O 
      data ijadj( 7, 8,2)/17/! OE
      data ijadj( 7, 9,1)/ 9/! O 
      data ijadj( 7, 9,2)/17/! OE
      data natoadj( 7)/10/
      data cooadj( 7, 1,1)/    -0.78800/
      data cooadj( 7, 1,2)/     1.17000/
      data cooadj( 7, 1,3)/     0.41000/
      data cooadj( 7, 2,1)/     0.00000/
      data cooadj( 7, 2,2)/     0.00000/
      data cooadj( 7, 2,3)/     0.00000/
      data cooadj( 7, 3,1)/     1.40700/
      data cooadj( 7, 3,2)/     0.00000/
      data cooadj( 7, 3,3)/     0.59000/
      data cooadj( 7, 4,1)/     2.41600/
      data cooadj( 7, 4,2)/     0.00000/
      data cooadj( 7, 4,3)/    -0.14800/
      data cooadj( 7, 5,1)/     0.00000/
      data cooadj( 7, 5,2)/    -0.00000/
      data cooadj( 7, 5,3)/    -1.52300/
      data cooadj( 7, 6,1)/    -1.37500/
      data cooadj( 7, 6,2)/    -0.06100/
      data cooadj( 7, 6,3)/    -2.24900/
      data cooadj( 7, 7,1)/    -2.51500/
      data cooadj( 7, 7,2)/     0.75500/
      data cooadj( 7, 7,3)/    -1.53300/
      data cooadj( 7, 8,1)/    -2.86800/
      data cooadj( 7, 8,2)/     0.12200/
      data cooadj( 7, 8,3)/    -0.53900/
      data cooadj( 7, 9,1)/    -2.90700/
      data cooadj( 7, 9,2)/     1.79700/
      data cooadj( 7, 9,3)/    -1.96400/
      data cooadj( 7,10,1)/    -1.61083/
      data cooadj( 7,10,2)/     0.43550/
      data cooadj( 7,10,3)/    -1.30133/
c... CH     1.73000
      data rmadj( 7)/     3.84611/
c GLY
      data ijadj( 8, 1,1)/ 6/! NH
      data ijadj( 8, 1,2)/ 1/! N 
      data ijadj( 8, 2,1)/ 3/! CH
      data ijadj( 8, 2,2)/ 2/! CA
      data ijadj( 8, 3,1)/ 1/! C 
      data ijadj( 8, 3,2)/ 3/! C 
      data ijadj( 8, 4,1)/ 9/! O 
      data ijadj( 8, 4,2)/21/! O 
      data natoadj( 8)/ 5/
      data cooadj( 8, 1,1)/    -0.78800/
      data cooadj( 8, 1,2)/     1.17000/
      data cooadj( 8, 1,3)/     0.41000/
      data cooadj( 8, 2,1)/     0.00000/
      data cooadj( 8, 2,2)/     0.00000/
      data cooadj( 8, 2,3)/     0.00000/
      data cooadj( 8, 3,1)/     1.40700/
      data cooadj( 8, 3,2)/     0.00000/
      data cooadj( 8, 3,3)/     0.59000/
      data cooadj( 8, 4,1)/     2.41600/
      data cooadj( 8, 4,2)/     0.00000/
      data cooadj( 8, 4,3)/    -0.14800/
      data cooadj( 8, 5,1)/     0.00000/
      data cooadj( 8, 5,2)/     0.00000/
      data cooadj( 8, 5,3)/     0.00000/
      data rmadj( 8)/     0.00000/
c HIS
      data ijadj( 9, 1,1)/ 6/! NH
      data ijadj( 9, 1,2)/ 1/! N 
      data ijadj( 9, 2,1)/ 2/! CH
      data ijadj( 9, 2,2)/ 2/! CA
      data ijadj( 9, 3,1)/ 1/! C 
      data ijadj( 9, 3,2)/ 3/! C 
      data ijadj( 9, 4,1)/ 9/! O 
      data ijadj( 9, 4,2)/21/! O 
      data ijadj( 9, 5,1)/ 3/! CH
      data ijadj( 9, 5,2)/ 4/! CB
      data ijadj( 9, 6,1)/ 1/! C 
      data ijadj( 9, 6,2)/ 5/! CG
      data ijadj( 9, 7,1)/20/!XXX
      data ijadj( 9, 7,2)/11/! ND
      data ijadj( 9, 8,1)/ 2/! CH
      data ijadj( 9, 8,2)/ 6/! CD
      data ijadj( 9, 9,1)/ 2/! CH
      data ijadj( 9, 9,2)/ 7/! CE
      data ijadj( 9,10,1)/ 6/! NH
      data ijadj( 9,10,2)/12/! NE
      data natoadj( 9)/11/
      data cooadj( 9, 1,1)/    -0.78800/
      data cooadj( 9, 1,2)/     1.17000/
      data cooadj( 9, 1,3)/     0.41000/
      data cooadj( 9, 2,1)/     0.00000/
      data cooadj( 9, 2,2)/     0.00000/
      data cooadj( 9, 2,3)/     0.00000/
      data cooadj( 9, 3,1)/     1.40700/
      data cooadj( 9, 3,2)/     0.00000/
      data cooadj( 9, 3,3)/     0.59000/
      data cooadj( 9, 4,1)/     2.41600/
      data cooadj( 9, 4,2)/     0.00000/
      data cooadj( 9, 4,3)/    -0.14800/
      data cooadj( 9, 5,1)/     0.00000/
      data cooadj( 9, 5,2)/    -0.00000/
      data cooadj( 9, 5,3)/    -1.52300/
      data cooadj( 9, 6,1)/     0.07900/
      data cooadj( 9, 6,2)/     1.45700/
      data cooadj( 9, 6,3)/    -1.91000/
      data cooadj( 9, 7,1)/     1.29000/
      data cooadj( 9, 7,2)/     2.10700/
      data cooadj( 9, 7,3)/    -2.09100/
      data cooadj( 9, 8,1)/    -0.88500/
      data cooadj( 9, 8,2)/     2.38900/
      data cooadj( 9, 8,3)/    -2.11800/
      data cooadj( 9, 9,1)/     1.04800/
      data cooadj( 9, 9,2)/     3.36900/
      data cooadj( 9, 9,3)/    -2.38900/
      data cooadj( 9,10,1)/    -0.25200/
      data cooadj( 9,10,2)/     3.55000/
      data cooadj( 9,10,3)/    -2.43300/
      data cooadj( 9,11,1)/     0.18286/
      data cooadj( 9,11,2)/     1.83886/
      data cooadj( 9,11,3)/    -1.78057/
c... CH     1.73000
      data rmadj( 9)/     4.29618/
c ILE
      data ijadj(10, 1,1)/ 6/! NH
      data ijadj(10, 1,2)/ 1/! N 
      data ijadj(10, 2,1)/ 2/! CH
      data ijadj(10, 2,2)/ 2/! CA
      data ijadj(10, 3,1)/ 1/! C 
      data ijadj(10, 3,2)/ 3/! C 
      data ijadj(10, 4,1)/ 9/! O 
      data ijadj(10, 4,2)/21/! O 
      data ijadj(10, 5,1)/ 2/! CH
      data ijadj(10, 5,2)/ 4/! CB
      data ijadj(10, 6,1)/ 4/! CH
      data ijadj(10, 6,2)/ 5/! CG
      data ijadj(10, 7,1)/ 4/! CH
      data ijadj(10, 7,2)/ 5/! CG
      data ijadj(10, 8,1)/ 3/! CH
      data ijadj(10, 8,2)/ 6/! CD
      data natoadj(10)/ 9/
      data cooadj(10, 1,1)/    -0.78800/
      data cooadj(10, 1,2)/     1.17000/
      data cooadj(10, 1,3)/     0.41000/
      data cooadj(10, 2,1)/     0.00000/
      data cooadj(10, 2,2)/     0.00000/
      data cooadj(10, 2,3)/     0.00000/
      data cooadj(10, 3,1)/     1.40700/
      data cooadj(10, 3,2)/     0.00000/
      data cooadj(10, 3,3)/     0.59000/
      data cooadj(10, 4,1)/     2.41600/
      data cooadj(10, 4,2)/     0.00000/
      data cooadj(10, 4,3)/    -0.14800/
      data cooadj(10, 5,1)/     0.00000/
      data cooadj(10, 5,2)/    -0.00000/
      data cooadj(10, 5,3)/    -1.52300/
      data cooadj(10, 6,1)/     1.19500/
      data cooadj(10, 6,2)/     0.78800/
      data cooadj(10, 6,3)/    -2.15200/
      data cooadj(10, 7,1)/    -1.39800/
      data cooadj(10, 7,2)/     0.54200/
      data cooadj(10, 7,3)/    -1.86100/
      data cooadj(10, 8,1)/     2.38400/
      data cooadj(10, 8,2)/    -0.10700/
      data cooadj(10, 8,3)/    -2.57100/
      data cooadj(10, 9,1)/     0.43620/
      data cooadj(10, 9,2)/     0.24460/
      data cooadj(10, 9,3)/    -1.62140/
c... CH     1.73000
      data rmadj(10)/     3.42677/
c LEU
      data ijadj(11, 1,1)/ 6/! NH
      data ijadj(11, 1,2)/ 1/! N 
      data ijadj(11, 2,1)/ 2/! CH
      data ijadj(11, 2,2)/ 2/! CA
      data ijadj(11, 3,1)/ 1/! C 
      data ijadj(11, 3,2)/ 3/! C 
      data ijadj(11, 4,1)/ 9/! O 
      data ijadj(11, 4,2)/21/! O 
      data ijadj(11, 5,1)/ 3/! CH
      data ijadj(11, 5,2)/ 4/! CB
      data ijadj(11, 6,1)/ 2/! CH
      data ijadj(11, 6,2)/ 5/! CG
      data ijadj(11, 7,1)/ 4/! CH
      data ijadj(11, 7,2)/ 6/! CD
      data ijadj(11, 8,1)/ 4/! CH
      data ijadj(11, 8,2)/ 6/! CD
      data natoadj(11)/ 9/
      data cooadj(11, 1,1)/    -0.78800/
      data cooadj(11, 1,2)/     1.17000/
      data cooadj(11, 1,3)/     0.41000/
      data cooadj(11, 2,1)/     0.00000/
      data cooadj(11, 2,2)/     0.00000/
      data cooadj(11, 2,3)/     0.00000/
      data cooadj(11, 3,1)/     1.40700/
      data cooadj(11, 3,2)/     0.00000/
      data cooadj(11, 3,3)/     0.59000/
      data cooadj(11, 4,1)/     2.41600/
      data cooadj(11, 4,2)/     0.00000/
      data cooadj(11, 4,3)/    -0.14800/
      data cooadj(11, 5,1)/    -0.00000/
      data cooadj(11, 5,2)/    -0.00000/
      data cooadj(11, 5,3)/    -1.52300/
      data cooadj(11, 6,1)/    -1.31800/
      data cooadj(11, 6,2)/    -0.22800/
      data cooadj(11, 6,3)/    -2.25000/
      data cooadj(11, 7,1)/    -2.23500/
      data cooadj(11, 7,2)/     0.94500/
      data cooadj(11, 7,3)/    -2.00100/
      data cooadj(11, 8,1)/    -1.09900/
      data cooadj(11, 8,2)/    -0.40600/
      data cooadj(11, 8,3)/    -3.73200/
      data cooadj(11, 9,1)/    -0.93040/
      data cooadj(11, 9,2)/     0.06220/
      data cooadj(11, 9,3)/    -1.90120/
c... CH     1.73000
      data rmadj(11)/     3.84756/
c LYS
      data ijadj(12, 1,1)/ 6/! NH
      data ijadj(12, 1,2)/ 1/! N 
      data ijadj(12, 2,1)/ 2/! CH
      data ijadj(12, 2,2)/ 2/! CA
      data ijadj(12, 3,1)/ 1/! C 
      data ijadj(12, 3,2)/ 3/! C 
      data ijadj(12, 4,1)/ 9/! O 
      data ijadj(12, 4,2)/21/! O 
      data ijadj(12, 5,1)/ 3/! CH
      data ijadj(12, 5,2)/ 4/! CB
      data ijadj(12, 6,1)/ 3/! CH
      data ijadj(12, 6,2)/ 5/! CG
      data ijadj(12, 7,1)/ 3/! CH
      data ijadj(12, 7,2)/ 6/! CD
      data ijadj(12, 8,1)/ 3/! CH
      data ijadj(12, 8,2)/ 7/! CE
      data ijadj(12, 9,1)/ 8/! NH
      data ijadj(12, 9,2)/13/! NZ
      data natoadj(12)/10/
      data cooadj(12, 1,1)/    -0.78800/
      data cooadj(12, 1,2)/     1.17000/
      data cooadj(12, 1,3)/     0.41000/
      data cooadj(12, 2,1)/     0.00000/
      data cooadj(12, 2,2)/     0.00000/
      data cooadj(12, 2,3)/     0.00000/
      data cooadj(12, 3,1)/     1.40700/
      data cooadj(12, 3,2)/     0.00000/
      data cooadj(12, 3,3)/     0.59000/
      data cooadj(12, 4,1)/     2.41600/
      data cooadj(12, 4,2)/     0.00000/
      data cooadj(12, 4,3)/    -0.14800/
      data cooadj(12, 5,1)/     0.00000/
      data cooadj(12, 5,2)/     0.00000/
      data cooadj(12, 5,3)/    -1.52300/
      data cooadj(12, 6,1)/     1.10200/
      data cooadj(12, 6,2)/     0.82700/
      data cooadj(12, 6,3)/    -2.15800/
      data cooadj(12, 7,1)/     0.98100/
      data cooadj(12, 7,2)/     0.83000/
      data cooadj(12, 7,3)/    -3.67800/
      data cooadj(12, 8,1)/     2.33000/
      data cooadj(12, 8,2)/     1.03400/
      data cooadj(12, 8,3)/    -4.34700/
      data cooadj(12, 9,1)/     2.24000/
      data cooadj(12, 9,2)/     0.79300/
      data cooadj(12, 9,3)/    -5.80500/
      data cooadj(12,10,1)/     1.10883/
      data cooadj(12,10,2)/     0.58067/
      data cooadj(12,10,3)/    -2.91850/
c... CH     1.73000
      data rmadj(12)/     4.90558/
c MET
      data ijadj(13, 1,1)/ 6/! NH
      data ijadj(13, 1,2)/ 1/! N 
      data ijadj(13, 2,1)/ 2/! CH
      data ijadj(13, 2,2)/ 2/! CA
      data ijadj(13, 3,1)/ 1/! C 
      data ijadj(13, 3,2)/ 3/! C 
      data ijadj(13, 4,1)/ 9/! O 
      data ijadj(13, 4,2)/21/! O 
      data ijadj(13, 5,1)/ 3/! CH
      data ijadj(13, 5,2)/ 4/! CB
      data ijadj(13, 6,1)/ 3/! CH
      data ijadj(13, 6,2)/ 5/! CG
      data ijadj(13, 7,1)/12/! SH
      data ijadj(13, 7,2)/20/! SD
      data ijadj(13, 8,1)/ 4/! CH
      data ijadj(13, 8,2)/ 7/! CE
      data natoadj(13)/ 9/
      data cooadj(13, 1,1)/    -0.78800/
      data cooadj(13, 1,2)/     1.17000/
      data cooadj(13, 1,3)/     0.41000/
      data cooadj(13, 2,1)/     0.00000/
      data cooadj(13, 2,2)/     0.00000/
      data cooadj(13, 2,3)/     0.00000/
      data cooadj(13, 3,1)/     1.40700/
      data cooadj(13, 3,2)/     0.00000/
      data cooadj(13, 3,3)/     0.59000/
      data cooadj(13, 4,1)/     2.41600/
      data cooadj(13, 4,2)/     0.00000/
      data cooadj(13, 4,3)/    -0.14800/
      data cooadj(13, 5,1)/     0.00000/
      data cooadj(13, 5,2)/    -0.00000/
      data cooadj(13, 5,3)/    -1.52300/
      data cooadj(13, 6,1)/    -0.02000/
      data cooadj(13, 6,2)/     1.41300/
      data cooadj(13, 6,3)/    -2.09200/
      data cooadj(13, 7,1)/    -0.17500/
      data cooadj(13, 7,2)/     1.43400/
      data cooadj(13, 7,3)/    -3.87300/
      data cooadj(13, 8,1)/    -1.87100/
      data cooadj(13, 8,2)/     0.94200/
      data cooadj(13, 8,3)/    -4.01300/
      data cooadj(13, 9,1)/    -0.41320/
      data cooadj(13, 9,2)/     0.75780/
      data cooadj(13, 9,3)/    -2.30020/
c... CH     1.73000
      data rmadj(13)/     4.18681/
c PHE
      data ijadj(14, 1,1)/ 6/! NH
      data ijadj(14, 1,2)/ 1/! N 
      data ijadj(14, 2,1)/ 2/! CH
      data ijadj(14, 2,2)/ 2/! CA
      data ijadj(14, 3,1)/ 1/! C 
      data ijadj(14, 3,2)/ 3/! C 
      data ijadj(14, 4,1)/ 9/! O 
      data ijadj(14, 4,2)/21/! O 
      data ijadj(14, 5,1)/ 3/! CH
      data ijadj(14, 5,2)/ 4/! CB
      data ijadj(14, 6,1)/ 1/! C 
      data ijadj(14, 6,2)/ 5/! CG
      data ijadj(14, 7,1)/ 2/! CH
      data ijadj(14, 7,2)/ 6/! CD
      data ijadj(14, 8,1)/ 2/! CH
      data ijadj(14, 8,2)/ 6/! CD
      data ijadj(14, 9,1)/ 2/! CH
      data ijadj(14, 9,2)/ 7/! CE
      data ijadj(14,10,1)/ 2/! CH
      data ijadj(14,10,2)/ 7/! CE
      data ijadj(14,11,1)/ 2/! CH
      data ijadj(14,11,2)/ 8/! CZ
      data natoadj(14)/12/
      data cooadj(14, 1,1)/    -0.78800/
      data cooadj(14, 1,2)/     1.17000/
      data cooadj(14, 1,3)/     0.41000/
      data cooadj(14, 2,1)/     0.00000/
      data cooadj(14, 2,2)/     0.00000/
      data cooadj(14, 2,3)/     0.00000/
      data cooadj(14, 3,1)/     1.40700/
      data cooadj(14, 3,2)/     0.00000/
      data cooadj(14, 3,3)/     0.59000/
      data cooadj(14, 4,1)/     2.41600/
      data cooadj(14, 4,2)/     0.00000/
      data cooadj(14, 4,3)/    -0.14800/
      data cooadj(14, 5,1)/    -0.00000/
      data cooadj(14, 5,2)/    -0.00000/
      data cooadj(14, 5,3)/    -1.52300/
      data cooadj(14, 6,1)/    -0.51200/
      data cooadj(14, 6,2)/    -1.17700/
      data cooadj(14, 6,3)/    -2.29400/
      data cooadj(14, 7,1)/     0.36100/
      data cooadj(14, 7,2)/    -2.13100/
      data cooadj(14, 7,3)/    -2.69600/
      data cooadj(14, 8,1)/    -1.84900/
      data cooadj(14, 8,2)/    -1.35700/
      data cooadj(14, 8,3)/    -2.54200/
      data cooadj(14, 9,1)/    -0.06400/
      data cooadj(14, 9,2)/    -3.28000/
      data cooadj(14, 9,3)/    -3.42400/
      data cooadj(14,10,1)/    -2.27200/
      data cooadj(14,10,2)/    -2.54000/
      data cooadj(14,10,3)/    -3.23800/
      data cooadj(14,11,1)/    -1.38400/
      data cooadj(14,11,2)/    -3.44600/
      data cooadj(14,11,3)/    -3.68700/
      data cooadj(14,12,1)/    -0.71500/
      data cooadj(14,12,2)/    -1.74137/
      data cooadj(14,12,3)/    -2.42550/
c... CH     1.73000
      data rmadj(14)/     4.80029/
c PRO
      data ijadj(15, 1,1)/ 5/! N 
      data ijadj(15, 1,2)/ 1/! N 
      data ijadj(15, 2,1)/ 2/! CH
      data ijadj(15, 2,2)/ 2/! CA
      data ijadj(15, 3,1)/ 1/! C 
      data ijadj(15, 3,2)/ 3/! C 
      data ijadj(15, 4,1)/ 9/! O 
      data ijadj(15, 4,2)/21/! O 
      data ijadj(15, 5,1)/ 3/! CH
      data ijadj(15, 5,2)/ 4/! CB
      data ijadj(15, 6,1)/ 3/! CH
      data ijadj(15, 6,2)/ 5/! CG
      data ijadj(15, 7,1)/ 3/! CH
      data ijadj(15, 7,2)/ 6/! CD
      data natoadj(15)/ 8/
      data cooadj(15, 1,1)/    -0.78800/
      data cooadj(15, 1,2)/     1.17000/
      data cooadj(15, 1,3)/     0.41000/
      data cooadj(15, 2,1)/     0.00000/
      data cooadj(15, 2,2)/     0.00000/
      data cooadj(15, 2,3)/     0.00000/
      data cooadj(15, 3,1)/     1.40700/
      data cooadj(15, 3,2)/     0.00000/
      data cooadj(15, 3,3)/     0.59000/
      data cooadj(15, 4,1)/     2.41600/
      data cooadj(15, 4,2)/     0.00000/
      data cooadj(15, 4,3)/    -0.14800/
      data cooadj(15, 5,1)/    -0.00000/
      data cooadj(15, 5,2)/     0.00000/
      data cooadj(15, 5,3)/    -1.52300/
      data cooadj(15, 6,1)/    -1.19700/
      data cooadj(15, 6,2)/     0.83300/
      data cooadj(15, 6,3)/    -1.90800/
      data cooadj(15, 7,1)/    -1.33500/
      data cooadj(15, 7,2)/     1.86700/
      data cooadj(15, 7,3)/    -0.79600/
      data cooadj(15, 8,1)/    -0.63300/
      data cooadj(15, 8,2)/     0.67500/
      data cooadj(15, 8,3)/    -1.05675/
c... CH     1.73000
      data rmadj(15)/     3.13465/
c SER
      data ijadj(16, 1,1)/ 6/! NH
      data ijadj(16, 1,2)/ 1/! N 
      data ijadj(16, 2,1)/ 2/! CH
      data ijadj(16, 2,2)/ 2/! CA
      data ijadj(16, 3,1)/ 1/! C 
      data ijadj(16, 3,2)/ 3/! C 
      data ijadj(16, 4,1)/ 9/! O 
      data ijadj(16, 4,2)/21/! O 
      data ijadj(16, 5,1)/ 3/! CH
      data ijadj(16, 5,2)/ 4/! CB
      data ijadj(16, 6,1)/10/! OH
      data ijadj(16, 6,2)/15/! OG
      data natoadj(16)/ 7/
      data cooadj(16, 1,1)/    -0.78800/
      data cooadj(16, 1,2)/     1.17000/
      data cooadj(16, 1,3)/     0.41000/
      data cooadj(16, 2,1)/     0.00000/
      data cooadj(16, 2,2)/     0.00000/
      data cooadj(16, 2,3)/     0.00000/
      data cooadj(16, 3,1)/     1.40700/
      data cooadj(16, 3,2)/     0.00000/
      data cooadj(16, 3,3)/     0.59000/
      data cooadj(16, 4,1)/     2.41600/
      data cooadj(16, 4,2)/     0.00000/
      data cooadj(16, 4,3)/    -0.14800/
      data cooadj(16, 5,1)/    -0.00000/
      data cooadj(16, 5,2)/    -0.00000/
      data cooadj(16, 5,3)/    -1.52300/
      data cooadj(16, 6,1)/     0.85700/
      data cooadj(16, 6,2)/     1.00500/
      data cooadj(16, 6,3)/    -2.04800/
      data cooadj(16, 7,1)/     0.28567/
      data cooadj(16, 7,2)/     0.33500/
      data cooadj(16, 7,3)/    -1.19033/
c... CH     1.73000
      data rmadj(16)/     2.99914/
c THR
      data ijadj(17, 1,1)/ 6/! NH
      data ijadj(17, 1,2)/ 1/! N 
      data ijadj(17, 2,1)/ 2/! CH
      data ijadj(17, 2,2)/ 2/! CA
      data ijadj(17, 3,1)/ 1/! C 
      data ijadj(17, 3,2)/ 3/! C 
      data ijadj(17, 4,1)/ 9/! O 
      data ijadj(17, 4,2)/21/! O 
      data ijadj(17, 5,1)/ 2/! CH
      data ijadj(17, 5,2)/ 4/! CB
      data ijadj(17, 6,1)/10/! OH
      data ijadj(17, 6,2)/15/! OG
      data ijadj(17, 7,1)/ 4/! CH
      data ijadj(17, 7,2)/ 5/! CG
      data natoadj(17)/ 8/
      data cooadj(17, 1,1)/    -0.78800/
      data cooadj(17, 1,2)/     1.17000/
      data cooadj(17, 1,3)/     0.41000/
      data cooadj(17, 2,1)/     0.00000/
      data cooadj(17, 2,2)/     0.00000/
      data cooadj(17, 2,3)/     0.00000/
      data cooadj(17, 3,1)/     1.40700/
      data cooadj(17, 3,2)/     0.00000/
      data cooadj(17, 3,3)/     0.59000/
      data cooadj(17, 4,1)/     2.41600/
      data cooadj(17, 4,2)/     0.00000/
      data cooadj(17, 4,3)/    -0.14800/
      data cooadj(17, 5,1)/     0.00000/
      data cooadj(17, 5,2)/     0.00000/
      data cooadj(17, 5,3)/    -1.52300/
      data cooadj(17, 6,1)/     0.52400/
      data cooadj(17, 6,2)/    -1.27700/
      data cooadj(17, 6,3)/    -1.88800/
      data cooadj(17, 7,1)/     0.85600/
      data cooadj(17, 7,2)/     1.09600/
      data cooadj(17, 7,3)/    -2.11600/
      data cooadj(17, 8,1)/     0.34500/
      data cooadj(17, 8,2)/    -0.04525/
      data cooadj(17, 8,3)/    -1.38175/
c... CH     1.73000
      data rmadj(17)/     3.15489/
c TRP
      data ijadj(18, 1,1)/ 6/! NH
      data ijadj(18, 1,2)/ 1/! N 
      data ijadj(18, 2,1)/ 2/! CH
      data ijadj(18, 2,2)/ 2/! CA
      data ijadj(18, 3,1)/ 1/! C 
      data ijadj(18, 3,2)/ 3/! C 
      data ijadj(18, 4,1)/ 9/! O 
      data ijadj(18, 4,2)/21/! O 
      data ijadj(18, 5,1)/ 3/! CH
      data ijadj(18, 5,2)/ 4/! CB
      data ijadj(18, 6,1)/ 1/! C 
      data ijadj(18, 6,2)/ 5/! CG
      data ijadj(18, 7,1)/ 2/! CH
      data ijadj(18, 7,2)/ 6/! CD
      data ijadj(18, 8,1)/ 2/! CH
      data ijadj(18, 8,2)/ 6/! CD
      data ijadj(18, 9,1)/ 6/! NH
      data ijadj(18, 9,2)/12/! NE
      data ijadj(18,10,1)/ 2/! CH
      data ijadj(18,10,2)/ 7/! CE
      data ijadj(18,11,1)/ 2/! CH
      data ijadj(18,11,2)/ 7/! CE
      data ijadj(18,12,1)/ 2/! CH
      data ijadj(18,12,2)/ 8/! CZ
      data ijadj(18,13,1)/ 2/! CH
      data ijadj(18,13,2)/ 8/! CZ
      data ijadj(18,14,1)/ 2/! CH
      data ijadj(18,14,2)/ 9/! CH
      data natoadj(18)/15/
      data cooadj(18, 1,1)/    -0.78800/
      data cooadj(18, 1,2)/     1.17000/
      data cooadj(18, 1,3)/     0.41000/
      data cooadj(18, 2,1)/     0.00000/
      data cooadj(18, 2,2)/     0.00000/
      data cooadj(18, 2,3)/     0.00000/
      data cooadj(18, 3,1)/     1.40700/
      data cooadj(18, 3,2)/     0.00000/
      data cooadj(18, 3,3)/     0.59000/
      data cooadj(18, 4,1)/     2.41600/
      data cooadj(18, 4,2)/     0.00000/
      data cooadj(18, 4,3)/    -0.14800/
      data cooadj(18, 5,1)/     0.00000/
      data cooadj(18, 5,2)/    -0.00000/
      data cooadj(18, 5,3)/    -1.52300/
      data cooadj(18, 6,1)/    -1.26700/
      data cooadj(18, 6,2)/     0.70800/
      data cooadj(18, 6,3)/    -2.13500/
      data cooadj(18, 7,1)/    -1.35800/
      data cooadj(18, 7,2)/     1.88600/
      data cooadj(18, 7,3)/    -2.76700/
      data cooadj(18, 8,1)/    -2.55600/
      data cooadj(18, 8,2)/     0.17500/
      data cooadj(18, 8,3)/    -2.16800/
      data cooadj(18, 9,1)/    -2.73700/
      data cooadj(18, 9,2)/     2.13700/
      data cooadj(18, 9,3)/    -3.13600/
      data cooadj(18,10,1)/    -3.41700/
      data cooadj(18,10,2)/     1.10700/
      data cooadj(18,10,3)/    -2.81800/
      data cooadj(18,11,1)/    -3.07700/
      data cooadj(18,11,2)/    -1.01000/
      data cooadj(18,11,3)/    -1.70400/
      data cooadj(18,12,1)/    -4.78300/
      data cooadj(18,12,2)/     0.92200/
      data cooadj(18,12,3)/    -3.04100/
      data cooadj(18,13,1)/    -4.46000/
      data cooadj(18,13,2)/    -1.18000/
      data cooadj(18,13,3)/    -1.92300/
      data cooadj(18,14,1)/    -5.26200/
      data cooadj(18,14,2)/    -0.27300/
      data cooadj(18,14,3)/    -2.59700/
      data cooadj(18,15,1)/    -2.62882/
      data cooadj(18,15,2)/     0.40655/
      data cooadj(18,15,3)/    -2.16473/
c... CH     1.73000
      data rmadj(18)/     5.15958/
c TYR
      data ijadj(19, 1,1)/ 6/! NH
      data ijadj(19, 1,2)/ 1/! N 
      data ijadj(19, 2,1)/ 2/! CH
      data ijadj(19, 2,2)/ 2/! CA
      data ijadj(19, 3,1)/ 1/! C 
      data ijadj(19, 3,2)/ 3/! C 
      data ijadj(19, 4,1)/ 9/! O 
      data ijadj(19, 4,2)/21/! O 
      data ijadj(19, 5,1)/ 3/! CH
      data ijadj(19, 5,2)/ 4/! CB
      data ijadj(19, 6,1)/ 1/! C 
      data ijadj(19, 6,2)/ 5/! CG
      data ijadj(19, 7,1)/ 2/! CH
      data ijadj(19, 7,2)/ 6/! CD
      data ijadj(19, 8,1)/ 2/! CH
      data ijadj(19, 8,2)/ 6/! CD
      data ijadj(19, 9,1)/ 2/! CH
      data ijadj(19, 9,2)/ 7/! CE
      data ijadj(19,10,1)/ 2/! CH
      data ijadj(19,10,2)/ 7/! CE
      data ijadj(19,11,1)/ 1/! C 
      data ijadj(19,11,2)/ 8/! CZ
      data ijadj(19,12,1)/10/! OH
      data ijadj(19,12,2)/18/! OH
      data natoadj(19)/13/
      data cooadj(19, 1,1)/    -0.78800/
      data cooadj(19, 1,2)/     1.17000/
      data cooadj(19, 1,3)/     0.41000/
      data cooadj(19, 2,1)/     0.00000/
      data cooadj(19, 2,2)/     0.00000/
      data cooadj(19, 2,3)/     0.00000/
      data cooadj(19, 3,1)/     1.40700/
      data cooadj(19, 3,2)/     0.00000/
      data cooadj(19, 3,3)/     0.59000/
      data cooadj(19, 4,1)/     2.41600/
      data cooadj(19, 4,2)/     0.00000/
      data cooadj(19, 4,3)/    -0.14800/
      data cooadj(19, 5,1)/     0.00000/
      data cooadj(19, 5,2)/     0.00000/
      data cooadj(19, 5,3)/    -1.52300/
      data cooadj(19, 6,1)/    -1.47800/
      data cooadj(19, 6,2)/     0.18100/
      data cooadj(19, 6,3)/    -1.91400/
      data cooadj(19, 7,1)/    -2.03300/
      data cooadj(19, 7,2)/     1.47000/
      data cooadj(19, 7,3)/    -1.81100/
      data cooadj(19, 8,1)/    -2.26500/
      data cooadj(19, 8,2)/    -0.88300/
      data cooadj(19, 8,3)/    -2.26500/
      data cooadj(19, 9,1)/    -3.43800/
      data cooadj(19, 9,2)/     1.65000/
      data cooadj(19, 9,3)/    -2.15300/
      data cooadj(19,10,1)/    -3.64300/
      data cooadj(19,10,2)/    -0.71800/
      data cooadj(19,10,3)/    -2.60000/
      data cooadj(19,11,1)/    -4.17900/
      data cooadj(19,11,2)/     0.57200/
      data cooadj(19,11,3)/    -2.48700/
      data cooadj(19,12,1)/    -5.51500/
      data cooadj(19,12,2)/     0.73700/
      data cooadj(19,12,3)/    -2.78600/
      data cooadj(19,13,1)/    -2.50567/
      data cooadj(19,13,2)/     0.33433/
      data cooadj(19,13,3)/    -1.94878/
c... CH     1.73000
      data rmadj(19)/     4.92185/
c VAL
      data ijadj(20, 1,1)/ 6/! NH
      data ijadj(20, 1,2)/ 1/! N 
      data ijadj(20, 2,1)/ 2/! CH
      data ijadj(20, 2,2)/ 2/! CA
      data ijadj(20, 3,1)/ 1/! C 
      data ijadj(20, 3,2)/ 3/! C 
      data ijadj(20, 4,1)/ 9/! O 
      data ijadj(20, 4,2)/21/! O 
      data ijadj(20, 5,1)/ 2/! CH
      data ijadj(20, 5,2)/ 4/! CB
      data ijadj(20, 6,1)/ 4/! CH
      data ijadj(20, 6,2)/ 5/! CG
      data ijadj(20, 7,1)/ 4/! CH
      data ijadj(20, 7,2)/ 5/! CG
      data natoadj(20)/ 8/
      data cooadj(20, 1,1)/    -0.78800/
      data cooadj(20, 1,2)/     1.17000/
      data cooadj(20, 1,3)/     0.41000/
      data cooadj(20, 2,1)/     0.00000/
      data cooadj(20, 2,2)/     0.00000/
      data cooadj(20, 2,3)/     0.00000/
      data cooadj(20, 3,1)/     1.40700/
      data cooadj(20, 3,2)/     0.00000/
      data cooadj(20, 3,3)/     0.59000/
      data cooadj(20, 4,1)/     2.41600/
      data cooadj(20, 4,2)/     0.00000/
      data cooadj(20, 4,3)/    -0.14800/
      data cooadj(20, 5,1)/     0.00000/
      data cooadj(20, 5,2)/    -0.00000/
      data cooadj(20, 5,3)/    -1.52300/
      data cooadj(20, 6,1)/     0.18200/
      data cooadj(20, 6,2)/    -1.39400/
      data cooadj(20, 6,3)/    -2.17300/
      data cooadj(20, 7,1)/    -1.17500/
      data cooadj(20, 7,2)/     0.75100/
      data cooadj(20, 7,3)/    -2.09000/
      data cooadj(20, 8,1)/    -0.24825/
      data cooadj(20, 8,2)/    -0.16075/
      data cooadj(20, 8,3)/    -1.44650/
c... CH     1.73000
      data rmadj(20)/     3.20643/
c AMD
      data ijadj(23, 1,1)/ 2/! CH
      data ijadj(23, 1,2)/ 2/! CA
      data ijadj(23, 2,1)/ 1/! C 
      data ijadj(23, 2,2)/ 3/! C 
      data ijadj(23, 3,1)/ 9/! O 
      data ijadj(23, 3,2)/21/! O 
      data ijadj(23, 4,1)/ 6/! NH
      data ijadj(23, 4,2)/ 1/! N 
      data ijadj(23, 5,1)/ 2/! CH
      data ijadj(23, 5,2)/ 2/! CA
      data natoadj(23)/ 6/
      data cooadj(23, 1,1)/    -8.68100/
      data cooadj(23, 1,2)/    19.91500/
      data cooadj(23, 1,3)/     9.82100/
      data cooadj(23, 2,1)/    -9.82700/
      data cooadj(23, 2,2)/    18.96900/
      data cooadj(23, 2,3)/    10.16800/
      data cooadj(23, 3,1)/    -9.64100/
      data cooadj(23, 3,2)/    18.02700/
      data cooadj(23, 3,3)/    10.96800/
      data cooadj(23, 4,1)/   -10.97600/
      data cooadj(23, 4,2)/    19.23500/
      data cooadj(23, 4,3)/     9.56600/
      data cooadj(23, 5,1)/   -12.19200/
      data cooadj(23, 5,2)/    18.43800/
      data cooadj(23, 5,3)/     9.77200/
      data cooadj(23, 6,1)/   -10.14800/
      data cooadj(23, 6,2)/    18.74367/
      data cooadj(23, 6,3)/    10.23400/
c... C      1.57700
      data rmadj(23)/     1.97471/

c http://en.wikipedia.org/wiki/Selenocysteine CSE n. 21 CSE
c http://en.wikipedia.org/wiki/Selenomethionine MSE n. 22

      data (brna(i),i=1,4)/
     >'ADE',
     >'CYT',
     >'GUA',
     >'URI'/

      data (rmass(i),i=1,100)/
     >1.0079 , !1 H Hydrogen
     >4.0026 , !2 He Helium
     >6.941 , !3 Li Lithium
     >9.01218 , !4 Be Beryllium
     >10.81 , !5 B Boron
     >12.011 , !6 C Carbon
     >14.0067 , !7 N Nitrogen
     >15.9994 , !8 O Oxygen
     >18.998403 , !9 F Fluorine
     >20.179 , !10 Ne Neon
     >22.98977 , !11 Na Sodium
     >24.305 , !12 Mg Magnesium
     >26.98145 , !13 Al Aluminum
     >28.0855 , !14 Si Silicon
     >30.97376 , !15 P Phosphorus
     >32.06 , !16 S Sulfur
     >35.453 , !17 Cl Chlorine
     >39.948 , !18 Ar Argon
     >39.0983 , !19 K Potassium
     >40.08 , !20 Ca Calcium
     >44.9559 , !21 Sc Scandium
     >47.88 , !22 Ti Titanium
     >50.9415 , !23 V Vanadium
     >51.996 , !24 Cr Chromium
     >54.938 , !25 Mn Manganese
     >55.847 , !26 Fe Iron
     >58.9332 , !27 Co Cobalt
     >58.96 , !28 Ni Nickel
     >63.546 , !29 Cu Copper
     >65.38 , !30 Zn Zinc
     >69.72 , !31 Ga Gallium
     >72.59 , !32 Ge Germanium
     >74.9216 , !33 As Arsenic
     >78.96 , !34 Se Selenium
     >79.904 , !35 Br Bromine
     >83.8 , !36 Kr Krypton
     >85.4678 , !37 Rb Rubidium
     >87.62 , !38 Sr Strontium
     >88.9059 , !39 Y Yttrium
     >91.22 , !40 Zr Zirconium
     >92.9064 , !41 Nb Niobium
     >95.94 , !42 Mo Molybdenum
     >97.907 , !43 Tc Technetium
     >101.07 , !44 Ru Ruthenium
     >102.9055 , !45 Rh Rhodium
     >106.42 , !46 Pd Palladium
     >107.868 , !47 Ag Silver
     >112.41 , !48 Cd Cadmium
     >114.82 , !49 In Indium
     >118.69 , !50 Sn Tin
     >121.75 , !51 Sb Antimony
     >127.6 , !52 Te Tellurium
     >126.9045 , !53 I Iodine
     >131.29 , !54 Xe Xenon
     >132.9054 , !55 Cs Cesium
     >137.33 , !56 Ba Barium
     >138.9055 , !57 La Lanthanum
     >140.12 , !58 Ce Cerium
     >140.9077 , !59 Pr Praseodymium
     >144.24 , !60 Nd Neodymium
     >144.913 , !61 Pm Promethium
     >150.36 , !62 Sm Samarium
     >151.96 , !63 Eu Europium
     >157.25 , !64 Gd Gadolinium
     >158.9254 , !65 Tb Terbium
     >162.5 , !66 Dy Dysprosium
     >164.9304 , !67 Ho Holmium
     >167.26 , !68 Er Erbium
     >168.9342 , !69 Tm Thulium
     >173.04 , !70 Yb Ytterbium
     >174.967 , !71 Lu Lutetium
     >178.49 , !72 Hf Hafnium
     >180.9479 , !73 Ta Tantalum
     >183.85 , !74 W Tungsten
     >186.207 , !75 Re Rhenium
     >190.2 , !76 Os Osmium
     >192.22 , !77 Ir Iridium
     >195.08 , !78 Pt Platinum
     >196.9665 , !79 Au Gold
     >200.59 , !80 Hg Mercury
     >204.383 , !81 Tl Thallium
     >207.2 , !82 Pb Lead
     >208.9804 , !83 Bi Bismuth
     >208.982 , !84 Po Polonium
     >209.98 , !85 At Astatine
     >222.018 , !86 Rn Radon
     >223.02 , !87 Fr Francium
     >226.025 , !88 Ra Radium
     >227.027 , !89 Ac Actinium
     >232.038 , !90 Th Thorium
     >231.035 , !91 Pa Protactinium
     >238.051 , !92 U Uranium
     >237.048 , !93 Np Neptunium
     >239.052 , !94 Pu Plutonium
     >243.061 , !95 Am Americium
     >247.07 , !96 Cm Curium
     >247.07 , !97 Bk Berkelium
     >251.08 , !98 Cf Californium
     >252.083 , !99 Es Einsteinium
     >257.095 / !100 Fm Fermium

      data (mass(i),i=1,103)/
     >  1,  4,  7,  9, 11, 12, 14, 16, 19, 20,
     > 23, 24, 27, 28, 31, 32, 35, 40, 39, 40,
     > 45, 48, 51, 52, 55, 56, 59, 58, 63, 64,
     > 69, 74, 75, 80, 79, 84, 85, 88, 89, 90,
     > 93, 98,100,102,103,106,107,110,113,120,
     >121,130,130,132,133,138,139,140,141,144,
     >150,152,153,158,159,164,165,166,169,174,
     >175,180,181,184,187,192,193,195,197,202,
     >205,208,209,209,210,222,223,226,227,232,
     >231,238,237,239,241,242,247,251,252,253,
     >257,255,256/
      data (rvdw(i),i=1,103)/
     >1.20d0,1.80d0,1.80d0,1.70d0,1.70d0,1.70d0,1.55d0,1.50d0,1.60d0,
     >1.60d0,2.30d0,1.70d0,1.70d0,2.10d0,1.85d0,1.80d0,1.90d0,1.90d0,
     >1.90d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,2.12d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0/

      data (rcov(i),i=1,103)/
     >0.37d0,0.50d0,1.34d0,1.25d0,0.90d0,0.77d0,0.75d0,0.73d0,0.71d0,
     >0.65d0,1.54d0,1.45d0,1.30d0,1.18d0,1.10d0,1.02d0,0.99d0,0.95d0,
     >1.96d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.39d0,1.25d0,1.26d0,
     >1.16d0,1.70d0,1.20d0,1.20d0,1.22d0,1.22d0,1.17d0,1.14d0,1.10d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.40d0,1.43d0,1.35d0,1.33d0,1.30d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.45d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,1.70d0,
     >1.70d0,1.70d0,1.70d0,1.70d0/
      data (atypei(i),i=1,21)/  ! sono i tipi di pseudo-atomi degli aminoacidi
     >' N ',  !01
     >' CA',  !02
     >' C ',  !03
     >' CB',  !04
     >' CG',  !05
     >' CD',  !06
     >' CE',  !07
     >' CZ',  !08
     >' CH',  !09
     >' N ',  !10
     >' ND',  !11
     >' NE',  !12
     >' NZ',  !13
     >' NH',  !14
     >' OG',  !15
     >' OD',  !16
     >' OE',  !17
     >' OH',  !18
     >' SG',  !19
     >' SD',  !20
     >' OX'/  !21


      data ((itypei(i,j),j=1,21),i=1,22)/ !dice per ognuno dei i- amminoacidi 
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!ALA
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 3, 3, 0, 1, 0, 0, 0, 6, 0, 7, 0, 0, 0, 0, 0, 0, 9,!ARG
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 1, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 9, 0, 0, 0, 0, 9,!ASN
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, 9,!ASP
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,11, 0, 9,!CYS
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 3, 1, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 9, 0, 0, 0, 9,!GLN
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 9,!GLU
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!GLY
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 1, 2, 2, 0, 0, 0,20, 6, 0, 0, 0, 0, 0, 0, 0, 0, 9,!HIS   !ND HIS ' un 20 
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 2, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!ILE
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!LEU
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 3, 3, 3, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 9,!LYS
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 3, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,12, 9,!MET
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 1, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!PHE
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >5, 2, 1, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!PRO
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,10, 0, 0, 0, 0, 0, 9,!SER
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0,10, 0, 0, 0, 0, 0, 9,!THR
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 1, 2, 2, 2, 2, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 9,!TRP
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,10, 0, 0, 9,!TYR
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 2, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!VAL
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9,!CSE
c     N CA  C CB CG CD CE CZ CH  N ND NE NZ NH OG OD OE OH SG SD OX
     >6, 2, 1, 3, 3, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9/!MSE


      data dnexch/0.1/

      data (sclen(i), hgrou(i), hexg(i),vexc(i),rexc(i),i=1, 23)/
     >  0.6651,  0.0,  0.0,   16.44,   1.577 ,   !C
     >  0.2909,  1.0,  0.0,   21.59,   1.730,   !CH
     > -0.0830,  2.0,  0.0,   26.74,   1.850,   !CH2
     > -0.4580,  3.0,  0.0,   31.89,   1.970,   !CH3
     >  0.9400,  0.0,  0.0,    2.49,   0.840,   !N
     >  0.5658,  1.0,  1.0,    7.64,   1.220,   !NH
     >  0.1916,  2.0,  2.0,   12.79,   1.450,   !NH2
     > -0.1826,  3.0,  3.0,   17.94,   1.620,   !NH3
     >  0.5804,  0.0,  0.0,    9.13,   1.300,   !O
     >  0.2062,  1.0,  1.0,   14.28,   1.500,   !OH
     >  0.2847,  0.0,  0.0,   19.86,   1.680,   !S
     > -0.0900,  1.0,  1.0,   25.10,   1.810,   !SH
     >  0.5170,  0.0,  0.0,    5.73,   1.11,   !P
     >  0.9500,  0.0,  0.0,    7.99,   1.24,   !FE
     >  0.7600,  0.0,  0.0,    8.78,   1.28,   !CU
     >  0.4700,  0.0,  0.0,   31.89,   1.97,   !CA
     >  0.5200,  0.0,  0.0,   17.16,   1.60,   !MG
     > -0.3900,  0.0,  0.0,    9.20,   1.30,   !MN
     >  0.5700,  0.0,  0.0,    9.85,   1.33,   !ZN
     >  0.7529,  0.5,  0.5,    2.49,   0.840,   !N(Histidine)  is a ND atom (N)
     > -0.3742,  1.0,  0.0,    5.15,   1.071,   !H
     >	0.8417,  0.0,  0.0,   15.90,   1.56,   !U
     >	0.7970,  0.0,  0.0,   28.73,   1.90/   !SE

      data (atypej(i),i=1, 23)/
     >' C ', !   1!C
     >' CH', !   2!CH
     >' CH', !   3!CH2
     >' CH', !   4!CH3
     >' N ', !   5!N
     >' NH', !   6!NH
     >' NH', !   7!NH2
     >' NH', !   8!NH3
     >' O ', !   9!O
     >' OH', !  10!OH
     >' S ', !  11!S
     >' SH', !  12!SH
     >' P ', !  13!P
     >'FE ', !  14!FE
     >'CU ', !  15!CU
     >'CA ', !  16!CA
     >'MG ', !  17!MG
     >'MN ', !  18!MN
     >'ZN ', !  19!ZN
     >'XXX', !  20!null
     >' H ', !  21!H
     >'U  ', !  22!Uranium, nov. 2008
     >'SE '/ !  23!SE

c HETATM 1086  N   CSE A 140      85.657  38.047  28.232  1.00 47.65           N  
c HETATM 1087  CA  CSE A 140      84.611  37.715  29.202  1.00 47.80           C  
c HETATM 1088  CB  CSE A 140      84.753  36.266  29.631  1.00 47.70           C  
c HETATM 1089 SE   CSE A 140      83.466  35.104  29.018  1.00 20.00          SE  
c HETATM 1090  C   CSE A 140      85.021  38.548  30.381  1.00 48.78           C  
c HETATM 1091  O   CSE A 140      84.297  38.661  31.368  1.00 49.47           O  

c ATOM     43  N   CYS     6      -6.190  19.989   4.859  4.10  1.50      6LYZ 289
c ATOM     44  CA  CYS     6      -7.588  19.557   4.979  7.38  1.50      6LYZ 290
c ATOM     45  C   CYS     6      -7.979  18.286   5.734  7.85  1.50      6LYZ 291
c ATOM     46  O   CYS     6      -9.009  18.248   6.442  3.09  1.50      6LYZ 292
c ATOM     47  CB  CYS     6      -8.281  19.722   3.493  8.09  1.50      6LYZ 293
c ATOM     48  SG  CYS     6      -9.062  21.202   2.777  9.87  1.50      6LYZ 294

      data natgr( 1),(izgr( 1,i),i=1, 1)/  1, 6/          !C
      data natgr( 2),(izgr( 2,i),i=1, 2)/  2, 6, 1/       !CH
      data natgr( 3),(izgr( 3,i),i=1, 3)/  3, 6, 1, 1/    !CH2
      data natgr( 4),(izgr( 4,i),i=1, 4)/  4, 6, 1, 1, 1/ !CH3
      data natgr( 5),(izgr( 5,i),i=1, 1)/  1, 7/          !N
      data natgr( 6),(izgr( 6,i),i=1, 2)/  2, 7, 1/       !NH
      data natgr( 7),(izgr( 7,i),i=1, 3)/  3, 7, 1, 1/    !NH2
      data natgr( 8),(izgr( 8,i),i=1, 4)/  4, 7, 1, 1, 1/ !NH3
      data natgr( 9),(izgr( 9,i),i=1, 1)/  1, 8/          !O
      data natgr(10),(izgr(10,i),i=1, 2)/  2, 8, 1/       !OH
      data natgr(11),(izgr(11,i),i=1, 1)/  1,16/          !S
      data natgr(12),(izgr(12,i),i=1, 2)/  2,16, 1/       !SH
      data natgr(13),(izgr(13,i),i=1, 1)/  1,15/          !P
      data natgr(14),(izgr(14,i),i=1, 1)/  1,26/          !FE
      data natgr(15),(izgr(15,i),i=1, 1)/  1,29/          !CU
      data natgr(16),(izgr(16,i),i=1, 1)/  1,20/          !CA
      data natgr(17),(izgr(17,i),i=1, 1)/  1,12/          !MG
      data natgr(18),(izgr(18,i),i=1, 1)/  1,25/          !MN
      data natgr(19),(izgr(19,i),i=1, 1)/  1,30/          !ZN
      data natgr(20),(izgr(20,i),i=1, 1)/  1, 7/          ! N(Histidine)
      data natgr(21),(izgr(21,i),i=1, 1)/  1, 1/          !H
      data natgr(22),(izgr(22,i),i=1, 1)/  1,92/          !U
      data natgr(23),(izgr(23,i),i=1, 1)/  1,34/          !SE

      data (atfmt(  1,i),i=1,5)/  2.65751E+000,
     >  1.07808E+000,
     >  1.49091E+000,
     > -4.24107E+000,
     >  7.13791E-001/
      data (btfmt(  1,i),i=1,5)/  9.36002E-002,
     >  4.91899E-003,
     >  2.66518E-001,
     > -1.86178E-006,
     >  1.51687E-003/
      data ctfmt(  1)/  4.29798E+000/
      data (atfmt(  2,i),i=1,5)/  2.17166E+000,
     >  1.06422E+000,
     >  2.96665E+000,
     > -4.23315E+000,
     >  7.32899E-001/
      data (btfmt(  2,i),i=1,5)/  8.89182E-002,
     >  5.01643E-003,
     >  2.52365E-001,
     > -1.92691E-006,
     >  1.54361E-003/
      data ctfmt(  2)/  4.29135E+000/
      data (atfmt(  3,i),i=1,5)/  1.81664E+000,
     >  1.05070E+000,
     >  4.31112E+000,
     > -4.24077E+000,
     >  7.51781E-001/
      data (btfmt(  3,i),i=1,5)/  8.49263E-002,
     >  5.11657E-003,
     >  2.56518E-001,
     > -1.98338E-006,
     >  1.56972E-003/
      data ctfmt(  3)/  4.30023E+000/
      data (atfmt(  4,i),i=1,5)/  1.56878E+000,
     >  1.03582E+000,
     >  5.54806E+000,
     > -4.23159E+000,
     >  7.72968E-001/
      data (btfmt(  4,i),i=1,5)/  8.22201E-002,
     >  5.23411E-003,
     >  2.64882E-001,
     > -2.05406E-006,
     >  1.59868E-003/
      data ctfmt(  4)/  4.29242E+000/
      data (atfmt(  5,i),i=1,5)/  1.18938E+001,
     >  3.27748E+000,
     >  1.85809E+000,
     >  8.58927E-001,
     >  9.12985E-001/
      data (btfmt(  5,i),i=1,5)/  1.00055E-006,
     >  6.47995E-002,
     >  1.92160E-001,
     >  4.15458E-003,
     >  1.37599E-003/
      data ctfmt(  5)/ -1.18049E+001/
      data (atfmt(  6,i),i=1,5)/  1.17702E+001,
     >  3.03313E+000,
     >  3.09610E+000,
     >  8.50696E-001,
     >  9.22324E-001/
      data (btfmt(  6,i),i=1,5)/  1.02311E-006,
     >  6.28891E-002,
     >  2.12203E-001,
     >  4.19600E-003,
     >  1.38533E-003/
      data ctfmt(  6)/ -1.16806E+001/
      data (atfmt(  7,i),i=1,5)/  1.21503E+001,
     >  2.88506E+000,
     >  4.23046E+000,
     >  8.29272E-001,
     >  9.49353E-001/
      data (btfmt(  7,i),i=1,5)/  1.02313E-006,
     >  6.23590E-002,
     >  2.26146E-001,
     >  4.33004E-003,
     >  1.41169E-003/
      data ctfmt(  7)/ -1.20588E+001/
      data (atfmt(  8,i),i=1,5)/  1.26329E+001,
     >  2.69331E+000,
     >  5.41517E+000,
     >  8.18504E-001,
     >  9.62347E-001/
      data (btfmt(  8,i),i=1,5)/  9.99227E-007,
     >  6.08382E-002,
     >  2.35992E-001,
     >  4.39624E-003,
     >  1.42449E-003/
      data ctfmt(  8)/ -1.25404E+001/
      data (atfmt(  9,i),i=1,5)/  2.96043E+000,
     >  2.50882E+000,
     >  6.37853E-001,
     >  7.22838E-001,
     >  1.14276E+000/
      data (btfmt(  9,i),i=1,5)/  8.98102E-002,
     >  3.75956E-002,
     >  7.13846E-004,
     >  2.21377E-001,
     >  2.47122E-003/
      data ctfmt(  9)/  2.70140E-002/
      data (atfmt( 10,i),i=1,5)/  3.12298E+000,
     >  2.84570E+000,
     >  6.37997E-001,
     >  1.22264E+000,
     >  1.14289E+000/
      data (btfmt( 10,i),i=1,5)/  1.11574E-001,
     >  3.88268E-002,
     >  7.13929E-004,
     >  2.74695E-001,
     >  2.47191E-003/
      data ctfmt( 10)/  2.70174E-002/
      data (atfmt( 11,i),i=1,5)/  6.37216E+000,
     >  5.15457E+000,
     >  1.47373E+000,
     >  1.63507E+000,
     >  1.20937E+000/
      data (btfmt( 11,i),i=1,5)/  9.58971E-003,
     >  1.39903E-001,
     >  3.88649E-004,
     >  3.51111E-001,
     >  4.09670E-003/
      data ctfmt( 11)/  1.54722E-001/
      data (atfmt( 12,i),i=1,5)/  6.30981E+000,
     >  5.98809E+000,
     >  1.47499E+000,
     >  1.78148E+000,
     >  1.29008E+000/
      data (btfmt( 12,i),i=1,5)/  9.68032E-003,
     >  1.55685E-001,
     >  3.89645E-004,
     >  3.72661E-001,
     >  4.20470E-003/
      data ctfmt( 12)/  1.55633E-001/
      data (atfmt( 13,i),i=1,5)/  1.95054E+000,
     >  4.14693E+000,
     >  1.49456E+000,
     >  1.52204E+000,
     >  5.72971E+000/
      data (btfmt( 13,i),i=1,5)/  5.75086E-003,
     >  1.71264E-001,
     >  4.51386E-004,
     >  4.27577E-001,
     >  1.25459E-002/
      data ctfmt( 13)/  1.55233E-001/
      data (atfmt( 14,i),i=1,5)/  1.23111E+001,
     >  1.87662E+000,
     >  3.06618E+000,
     >  2.07045E+000,
     >  6.97518E+000/
      data (btfmt( 14,i),i=1,5)/  3.17225E-002,
     >  9.15754E-005,
     >  1.18692E-001,
     >  5.24134E-001,
     >  2.19427E-003/
      data ctfmt( 14)/ -3.04931E-001/
      data (atfmt( 15,i),i=1,5)/  1.40142E+001,
     >  4.78458E+000,
     >  5.05681E+000,
     >  1.45797E+000,
     >  6.93300E+000/
      data (btfmt( 15,i),i=1,5)/  2.36729E-002,
     >  2.37092E-005,
     >  8.25450E-002,
     >  4.59459E-001,
     >  1.68235E-003/
      data ctfmt( 15)/ -3.25448E+000/
      data (atfmt( 16,i),i=1,5)/  8.59366E+000,
     >  1.47732E+000,
     >  1.43625E+000,
     >  1.18284E+000,
     >  7.11326E+000/
      data (btfmt( 16,i),i=1,5)/  6.62428E-002,
     >  2.65278E-004,
     >  5.15411E-001,
     >  1.07557E+000,
     >  4.35743E-003/
      data ctfmt( 16)/  1.96255E-001/
      data (atfmt( 17,i),i=1,5)/  4.70897E+000,
     >  1.19481E+000,
     >  1.55816E+000,
     >  1.17041E+000,
     >  3.23940E+000/
      data (btfmt( 17,i),i=1,5)/  3.08726E-002,
     >  6.87123E-001,
     >  7.06183E-004,
     >  3.05815E-001,
     >  1.22103E-002/
      data ctfmt( 17)/  1.26842E-001/
      data (atfmt( 18,i),i=1,5)/  1.17095E+001,
     >  1.73341E+000,
     >  2.67314E+000,
     >  2.02337E+000,
     >  7.00318E+000/
      data (btfmt( 18,i),i=1,5)/  3.54442E-002,
     >  1.12720E-004,
     >  1.37977E-001,
     >  5.66879E-001,
     >  2.42572E-003/
      data ctfmt( 18)/ -1.47293E-001/
      data (atfmt( 19,i),i=1,5)/  1.47410E+001,
     >  6.90775E+000,
     >  4.64234E+000,
     >  2.19177E+000,
     >  3.84240E+001/
      data (btfmt( 19,i),i=1,5)/  2.14562E-002,
     >  1.54081E-003,
     >  7.53810E-002,
     >  4.00929E-001,
     >  2.51403E-006/
      data ctfmt( 19)/ -3.69158E+001/
      data (atfmt( 20,i),i=1,5)/  1.18938E+001,
     >  3.27748E+000,
     >  1.85809E+000,
     >  8.58927E-001,
     >  9.12985E-001/
      data (btfmt( 20,i),i=1,5)/  1.00055E-006,
     >  6.47995E-002,
     >  1.92160E-001,
     >  4.15458E-003,
     >  1.37599E-003/
      data ctfmt( 20)/ -1.18049E+001/

      data (atfmt( 21,i),i=1,5)/  0.413048,
     >  0.294953,
     >  0.187491,
     >  0.080701,
     >  0.023736/
      data (btfmt( 21,i),i=1,5)/ 15.569946,
     > 32.398468,
     >  5.711404,
     > 61.889874,
     >  1.334118/
      data ctfmt( 21)/  0.000049/

      data (atfmt( 22,i),i=1,5)/  1.56793E+001,
     >  3.28243E+001,
     >  1.36605E+001,
     >  3.68726E+000,
     >  2.22794E+001/
      data (btfmt( 22,i),i=1,5)/  4.50917E-004,
     >  4.31360E-003,
     >  1.15482E-001,
     >  7.12415E-001,
     >  2.48891E-002/
      data ctfmt( 22)/  3.85444E+000/

      data (atfmt( 23,i),i=1,5)/  1.73541E+001,
     >  4.65325E+000,
     >  4.25949E+000,
     >  4.13646E+000,
     >  6.74916E+000/
      data (btfmt( 23,i),i=1,5)/  1.48802E-002,
     >  1.61481E-005,
     >  9.86581E-002,
     >  2.86113E-001,
     >  1.12360E-003/
      data ctfmt( 23)/ -3.16098E+000/

      end



      subroutine disv_pair(istate,ihagg,isite,isub,ipoly,
     >                                  jsite,jsub,jpoly,icon,ov) 

      include 'quafit2014.cmn'

c.....Gia' chiamata la framehag

      ov=zer
cc      write(*,*) ' DD ',isite,isub,jsite,jsub
      if (icheckhag.eq.0) then
        if (w_overl.eq.zer.or.ihagg.gt.1.or.icon.eq.1) return !! prima del 9 maggio 2012
      else
        if (w_overl.eq.zer.or.icon.eq.1) return
      endif
c      write(*,*) '   ---> disv_pair '
      x=centre(jsite,jsub,jpoly,1)-centre(isite,isub,ipoly,1)
      y=centre(jsite,jsub,jpoly,2)-centre(isite,isub,ipoly,2)
      z=centre(jsite,jsub,jpoly,3)-centre(isite,isub,ipoly,3)
      call cpolar(x,y,z,r,alpr,betr)
      if (r.gt.(dmax(ipdbsite(istate,isite))
     >         +dmax(ipdbsite(istate,jsite)))*1.03) then
        return
      endif
c.....Test distanza
      call sigcal_new(
     >                   ipdbsite(istate,isite),
     >                   ipdbsite(istate,jsite),
     >                   alprot(isite,isub,ipoly),
     >                   betrot(isite,isub,ipoly),
     >                   gamrot(isite,isub,ipoly),
     >                   alprot(jsite,jsub,jpoly),
     >                   betrot(jsite,jsub,jpoly),
     >                   gamrot(jsite,jsub,jpoly),
     >                   alpr,betr,ovlt,dstt,rsct)
      dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
      if (dsmax.eq.0) return
      if (ieqcont.eq.1.and.isticky.eq.0) dsmax=dsmax/1.12246204830937d0
      dsmax=dsmax-reddsmax

      if (isticky.eq.0) then
c.......LJ
        r=r/dsmax
        if (r.gt.1.e-2) then
c.........Per overlap
          dum7=(one/r)**6
          ov=4*dum7*(dum7-one) !!!LJ
          dum9=
     >(phiovl_l(ipdbsite(istate,isite),ipdbsite(istate,jsite),1)-dum8)
          if (dum9.ne.zer) then
          dum7=ovlt*rsct*log(
     > (exp(-dstt/rsct)+pscale*exp( tauscale/rsct)+1-pscale)
     >/(exp(-dstt/rsct)+pscale*exp(-tauscale/rsct)+1-pscale))
     >/(2*tauscale)
          dum8=phiovl_l(ipdbsite(istate,isite),ipdbsite(istate,jsite),2) !min
c          write(*,*) dum8
          dum8=(dum7-dum8)/dum9
          dum7=(one+dum8*(epsprime**2-one))/epsprime
c          write(*,*) ovlt,dstt,rsct,dsmax,dum7,ov
          ov=ov*dum7
          endif
        else
          ov=1.d20
        endif
      else
c.......Sticky
        if (r.gt.dsmax) then
          ov=0.d0
        else if (r.gt.dsmax-redsticky) then
          ov=-1.d0
        else
          ov=1.d20
        endif
      endif
cc      write(*,*) 'DD2 ',ov
      return
      end





      subroutine test_disv(istate,ihagg,iconf) 

      include 'quafit2014.cmn'

c.....Gia' chiamata la framehag

      if (icheckhag.eq.0) then
        if (w_overl.eq.zer.or.ihagg.gt.1) return !! prima del 9 maggio 2012
      else
        if (w_overl.eq.zer) return
      endif
      ov=zer
c      write(*,*) '   ---> test_disv '
      ndis=ndist(istate,ihagg)
cc      write(*,*) ' EE ',ndis
      dum3=zer
      do idis=1,ndis
        isite=ipair(istate,ihagg,idis,1)
        isub=ipair(istate,ihagg,idis,2)
        ipoly=ipair(istate,ihagg,idis,3)
        jsite=ipair(istate,ihagg,idis,4)
        jsub=ipair(istate,ihagg,idis,5)
        jpoly=ipair(istate,ihagg,idis,6)
        m=ipair(istate,ihagg,idis,7)!molteplicita'
        i1=ipair(istate,ihagg,idis,8)!connessione
        i101=ipair(istate,ihagg,idis,9)!tipo di coppia
        if (i101.eq.1) then
          call disv_pair(istate,ihagg,isite,isub,ipoly,
     >                                  jsite,jsub,jpoly,i1,dum1) 
c          write(*,*) ' test 1',isite,jsite,dum1
        else if (i101.eq.2) then
          call disv_site_aa(istate,ihagg,isite,isub,ipoly,
     >                          jsite-nsite(istate),jsub,jpoly,i1,dum1) 
c          write(*,*) ' test 2',isite,jsite-nsite(istate),dum1
        else if (i101.eq.3) then
          call disv_site_aa(istate,ihagg,jsite,jsub,jpoly,
     >                          isite-nsite(istate),isub,ipoly,i1,dum1) 
c          write(*,*) ' test 3',isite-nsite(istate),jsite,dum1
        else
          call disv_aa_aa(istate,ihagg,isite-nsite(istate),isub,ipoly,
     >                              jsite-nsite(istate),jsub,jpoly,dum1) 
c          write(*,*) ' test 4',isite-nsite(istate),jsite-nsite(istate),
c     >dum1
        endif
c        write(*,*) ' ADFAD ',dum3,m,dum1
        dum1=m*dum1
        dum3=dum3+m
        overl_dis(istate,ihagg,idis,iconf)=dum1
        ov=ov+dum1
      enddo
      if (dum3.ne.zer) dum3=one/dum3
      overl(istate,ihagg,iconf)=ov*dum3
c      stop
      return
      end



      subroutine test_disv_site(istate,ksite,ihagg,iconf) 

      include 'quafit2014.cmn'

c.....Gia' chiamata la framehag

      if (icheckhag.eq.0) then
        if (w_overl.eq.zer.or.ihagg.gt.1) return !! prima del 9 maggio 2012
      else
        if (w_overl.eq.zer) return
      endif
c      write(*,*) '   ---> test_disv_site '
      np=npair_site(istate,ihagg)
      do i=1,np
        idis=ipair_site(istate,ihagg,ksite,i)
        isite=ipair(istate,ihagg,idis,1)
        isub=ipair(istate,ihagg,idis,2)
        ipoly=ipair(istate,ihagg,idis,3)
        jsite=ipair(istate,ihagg,idis,4)
        jsub=ipair(istate,ihagg,idis,5)
        jpoly=ipair(istate,ihagg,idis,6)
        m=ipair(istate,ihagg,idis,7) !molteplicita'
        i1=ipair(istate,ihagg,idis,8)!connessione
        i101=ipair(istate,ihagg,idis,9)!tipo di coppia
        if (i101.eq.1) then
          call disv_pair(istate,ihagg,isite,isub,ipoly,
     >                                  jsite,jsub,jpoly,i1,dum1) 
c          if (dum1.ne.0.d0) 
c     >    write(*,*) ' test 1',isite,jsite,dum1
        else if (i101.eq.2) then
          call disv_site_aa(istate,ihagg,isite,isub,ipoly,
     >                          jsite-nsite(istate),jsub,jpoly,i1,dum1) 
c          if (dum1.ne.0.d0) 
c     >    write(*,*) ' test 2',isite,jsite-nsite(istate),dum1
        else if (i101.eq.3) then
          call disv_site_aa(istate,ihagg,jsite,jsub,jpoly,
     >                          isite-nsite(istate),isub,ipoly,i1,dum1) 
c          if (dum1.ne.0.d0) 
c     >    write(*,*) ' test 3',isite-nsite(istate),jsite,dum1
        else
          call disv_aa_aa(istate,ihagg,isite-nsite(istate),isub,ipoly,
     >                              jsite-nsite(istate),jsub,jpoly,dum1) 
c          if (dum1.ne.0.d0) 
c     >    write(*,*) ' test 4',isite-nsite(istate),jsite-nsite(istate),
c     >dum1
        endif
        dum1=m*dum1
        overl_dis(istate,ihagg,idis,iconf)=dum1
      enddo
      ndis=ndist(istate,ihagg)
      ov=zer
      dum3=zer
      do idis=1,ndis
        dum3=dum3+ipair(istate,ihagg,idis,7) 
c        write(*,*) ' fad ',dum3,ipair(istate,ihagg,idis,7),
c     >overl_dis(istate,ihagg,idis,iconf)
        ov=ov+overl_dis(istate,ihagg,idis,iconf)
      enddo
      if (dum3.ne.zer) dum3=one/dum3
      overl(istate,ihagg,iconf)=ov*dum3
      return
      end

c.old      subroutine test_disv_old(istate,ihagg,isita,isitb,iconf,ov) 
c.old
c.old      include 'quafit2014.cmn'
c.old      dimension i8(mxsite)
c.old
c.oldc.....Gia' chiamata la framehag
c.old
c.old      if (w_overl.eq.zer.or.ihagg.gt.1) return
c.old      ov=zer
c.old      write(*,*) '   ---> test_disv '
c.old      nh=nsymm(istate,ihagg)
c.old      npp=npoly(istate,ihagg)
c.old      nss=nsite(istate)
c.old
c.oldc      dum2=dfloat(nh)
c.oldc      dum1=dum2/2
c.old
c.old      i1=0
c.oldc      dum4=zer
c.old
c.oldc      if (isita.eq.1.and.isitb.eq.nsite(istate))
c.oldc     >ov=zer
c.old
c.old
c.old      do isite=isita,isitb
c.old        i8(isite)=0
c.old        overl_site(istate,isite,ihagg,iconf)=zer
c.old      enddo 
c.old
c.old      do 10 isite=1,nss
c.old        do 101 isite2=isite,nss
c.old          do 12 ipoly=1,npp
c.old            do 12 isub2=1,nh
c.old              dum8=0.d0
c.old              if (isite.eq.isite2.and.1.eq.isub2.and.1.eq.ipoly) goto 12 
c.oldc              dum4=dum4+dum3
c.old              if ((isite.lt.isita.or.isite.gt.isitb).and.
c.old     >          (isite2.lt.isita.or.isite2.gt.isitb)) goto 12
c.old              i1=i1+1
c.old              if (isite.ge.isita.and.isite.le.isitb)
c.old     >         i8(isite)=i8(isite)+1
c.old          if (isite.ne.isite2.and.isite2.ge.isita.and.isite2.le.isitb)
c.old     >         i8(isite2)=i8(isite2)+1
c.old              x=centre(isite2,isub2,ipoly,1)-centre(isite,1,1,1)
c.old              y=centre(isite2,isub2,ipoly,2)-centre(isite,1,1,2)
c.old              z=centre(isite2,isub2,ipoly,3)-centre(isite,1,1,3)
c.old
c.oldc              write(31,*) ' isitea isiteb isite isite2 isub2',  !!! QUI TI VOGLIO!
c.oldc     >isita,isitb,isite,isite2,isub2,i1
c.old
c.old
c.old              call cpolar(x,y,z,r,alpr,betr)
c.old              if (r.gt.(dmax(ipdbsite(istate,isite))
c.old     >                 +dmax(ipdbsite(istate,isite2)))*1.03) then
c.old                goto 20
c.old              endif
c.oldc.............Test distanza
c.old              dsmax=sigcal_new(
c.old     >                   ipdbsite(istate,isite),
c.old     >                   ipdbsite(istate,isite2),
c.old     >                   alprot(isite,1,1),
c.old     >                   betrot(isite,1,1),
c.old     >                   gamrot(isite,1,1),
c.old     >                   alprot(isite2,isub2,ipoly),
c.old     >                   betrot(isite2,isub2,ipoly),
c.old     >                   gamrot(isite2,isub2,ipoly),
c.old     >                   alpr,betr)
c.oldccc              dsmax=dsmax+3.0!! aumento di 3 ang.st
c.old	      r=r/dsmax
c.old              if (r.gt.1.e-2) then
c.oldc...............Per overlap
c.old                dum7=(one/r)**6
c.old                dum8=4*(dum7**2-dum7) !LJ
c.old              else
c.old                dum8=1.d20
c.old              endif
c.oldc              if (r.lt.1.d0) then
c.oldc                dum8=1.d3
c.oldc              else
c.oldc                dum8=0.d0
c.oldc              endif
c.old             
c.old20            ov=ov
c.oldccc     >                         +dum3*dum8
c.old     >                         +dum8
c.old              if (isite.ge.isita.and.isite.le.isitb) then
c.old      overl_site(istate,isite,ihagg,iconf)=
c.old     >overl_site(istate,isite,ihagg,iconf)+dum8
c.oldc               write(*,*) ' vv ',isite,isite2,dum8
c.old              endif
c.old       if (isite.ne.isite2.and.isite2.ge.isita.and.isite2.le.isitb) then
c.old      overl_site(istate,isite2,ihagg,iconf)=
c.old     >overl_site(istate,isite2,ihagg,iconf)+dum8
c.oldc               write(*,*) ' pp ',isite,isite2,dum8
c.old              endif
c.oldc              write(*,*) isite,isite2!,isub2,ipoly,dum8
c.oldc dum3 e' la degenerazione dei contatti, ovvero ci sono dum3 contatti
c.oldc con un peso rover(i), ...
c.old
c.old12        continue
c.old 101    continue
c.old10    continue!isite
c.oldc      ov=ov/dum4
c.old      ov=ov/float(nh*i1*npp)
c.oldc      write(*,*)  isita,isitb,ov
c.old      do isite=isita,isitb
c.old        overl_site(istate,isite,ihagg,iconf)
c.old     >=overl_site(istate,isite,ihagg,iconf)/float(i8(isite)*nh*npp)
c.oldc        write(*,*) 'dentro ',
c.oldc     >i8(isite),isite,overl_site(istate,isite,ihagg,iconf)
c.old      enddo
c.old      return
c.old      end

      SUBROUTINE indexx(n,arr,indx)
      INTEGER n,indx(n),M,NSTACK
      REAL*8 arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a
      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .


      subroutine btoms(istate,ihagg,iconf)

      include 'quafit2014.cmn'
      
      call framhaglink(istate,ihagg,iconf)
c##############################
      call outall(istate,ihagg,iconf)
c##############################

      write(text(1:12),'(3i3)') istate,ihagg,iconf
      do k=0,6,3
        do l=1,2
          if (text(l+k:l+k).eq.' ') text(l+k:l+k)='0'
        enddo
      enddo


      open (unit=iu_bt2,status='unknown',file='qua'//code//subcode//
     >'_'//text(1:9)//'_ST.pdb')
      open (unit=iu_bt1,status='unknown',file='qua'//code//subcode//
     >'_'//text(1:9)//'_CA.pdb')
      open (unit=iu_bto,status='unknown',file='qua'//code//subcode//
     >'_'//text(1:9)//'.pdb')




      jconf=iconf+4


      natot=0

      i68=0

      do ipoly=1,npoly(istate,ihagg)
        do isub=1,nsymm(istate,ihagg)

          i68=i68+1
          do istlnk=1,nsite(istate)+nlink(istate)
            
            isite=iseqpdb(istate,istlnk)
c            write(151,*) istlnk,isite,nsite(istate),nlink(istate)
            if (isite.le.nsite(istate)) then

              call mat(
     >        -gamrot(isite,isub,ipoly)
     >       ,-betrot(isite,isub,ipoly)
     >       ,-alprot(isite,isub,ipoly),rot)

              xt=centre(isite,isub,ipoly,1)
              yt=centre(isite,isub,ipoly,2)
              zt=centre(isite,isub,ipoly,3)

c..................
c          iuu=71
c          write(iuu,*) ipoly,isub,isite
c          write(iuu,*) xt,yt,zt
c          do jjj=1,3
c            write(iuu,*) (rot(jjj,kkk),kkk=1,3)
c          enddo
c..................

              ipdb=ipdbsite(istate,isite)
              iname=lnfil(fsub(ipdb),300)

              sum1=0.d0
              sum2=0.d0
              sum3=0.d0
              sum4=0.d0

              nato=0
  
              open(unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:iname)
     >                                             //'.pdb')
99            read(iu_pdb,'(a200)',end=10) text
              if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 99
              read(unit=text,fmt=1000,end=10) ina,ch8,dum1,dum2,dum3
     
              ch1=ch8(2:2)
              hc3=ch8(6:8)
              sum1=sum1+dum1
              sum2=sum2+dum2
              sum3=sum3+dum3
              nato=nato+1
              if (nato.gt.mxrun) stop 'nato > mxrun'
              coord(nato,1)=dum1
              coord(nato,2)=dum2
              coord(nato,3)=dum3
              goto 99
10            close(unit=iu_pdb)
              if (nato.eq.0) stop 'nato=0'

c...........Trasla in modo che il centro coincida con il c.geom.
              dum1=1.d0/dfloat(nato)
              sum1=sum1*dum1
              sum2=sum2*dum1
              sum3=sum3*dum1

c...........Trasla in modo che il centro di massa della subunita' coincida
c...........con xt,yt,zt e poi ruota con la matrice rot

              do 30 i=1,nato
                x1=coord(i,1)-sum1
                y1=coord(i,2)-sum2
                z1=coord(i,3)-sum3
c                write(*,*) x1,y1,z1,rx,ry,rz,xt,yt,zt,rot
c                pause
                call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
                coord(i,1)=rx
                coord(i,2)=ry
                coord(i,3)=rz
30            continue

c###########Stampa sul pdb totale
          
              i=0
          
              open(unit=iu_pdb,status='unknown',
     >         file=fsub(ipdb)(1:iname)//'.pdb')
999           read(iu_pdb,'(a200)',end=109) text
c              if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 999
              if (text(1:4).ne.'ATOM') goto 999
              i=i+1
              natot=natot+1
              x1=coord(i,1)
              y1=coord(i,2)
              z1=coord(i,3)
              write(iu_bto,1001) text(1:6),i,text(12:21),
c     >             char(66+(-1)**i66),
     >               char(65+mod(i68-1,26)),
     >                       text(23:30),x1,y1,z1
              if (text(14:15).eq.'CA'.or.text(14:15).eq.'N ') then
                write(iu_bt1,1001) text(1:6),i,text(12:21),
     >               char(65+mod(i68-1,26)),
c     >               char(66+(-1)**(isite-1)),
     >                       text(23:30),x1,y1,z1
              endif
              if (isub.eq.1) then
                write(iu_bt2,1001) text(1:6),i,text(12:21),
     >             char(65+mod(i68-1,26)),
c     >             char(66+(-1)**(isite-1)),
     >                     text(23:30),x1,y1,z1
              endif
              goto 999
109           close(unit=iu_pdb)
            else
c..........Linker!
             iilink=isite-nsite(istate)
             call mat(
     >        -gamrot_lnk(isub,ipoly)
     >       ,-betrot_lnk(isub,ipoly)
     >       ,-alprot_lnk(isub,ipoly),rot)

              xt=centre_lnk(isub,ipoly,1)
              yt=centre_lnk(isub,ipoly,2)
              zt=centre_lnk(isub,ipoly,3)
c          write(37,*) isub,ipoly
c     >       ,alprot_lnk(isub,ipoly)
c     >       ,gamrot_lnk(isub,ipoly)
c     >       ,betrot_lnk(isub,ipoly)
c          write(37,*) xt,yt,zt
cc          write(37,*) rot
c          pause
              call linker_final_coo(istate,iilink,iconf)

c...........Trasla 
c...........con xt,yt,zt e poi ruota con la matrice rot

              do i=1,nato
                x1=coord(i,1)
                y1=coord(i,2)
                z1=coord(i,3)
                call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
                coord(i,1)=rx
                coord(i,2)=ry
                coord(i,3)=rz  
  
                natot=natot+1
                x1=coord(i,1)
                y1=coord(i,2)
                z1=coord(i,3)
 1011 format(a6,i5,1x,a3,a2,a3,a2,i4,a4,3f8.3)
                write(iu_bto,1011) 'ATOM  ',i,atypei(iato6(i)),
c     >          '  ',amm(iammch(i,1)),' '//char(66+(-1)**i66),
c     >          '  ',amm(iammch(i,1)),' '//char(65+mod(i68-1,26)),
     >            '  ',amm(iammch(i,1)),' '//char(65+mod(i68-1,26)),
     >            iato7(i),'    ',(coord(i,k),k=1,3)
                if (atypei(iato6(i))(2:3).eq.'CA'.or.
     >              atypei(iato6(i))(2:3).eq.'N ') then
                  write(iu_bt1,1011) 'ATOM  ',i,atypei(iato6(i)),
c     >            '  ',amm(iammch(i,1)),' '//char(66+(-1)**i66),
     >            '  ',amm(iammch(i,1)),' '//char(65+mod(i68-1,26)),
     >            iato7(i),'    ',(coord(i,k),k=1,3)
                endif
                if (isub.eq.1) then
                  write(iu_bt2,1011) 'ATOM  ',i,atypei(iato6(i)),
c     >              '  ',amm(iammch(i,1)),' '//char(66+(-1)**i66),
     >              '  ',amm(iammch(i,1)),' '//char(65+mod(i68-1,26)),
     >              iato7(i),'    ',(coord(i,k),k=1,3)
                endif
              enddo
            endif
          enddo !iistlnk

        enddo   ! end isub
      enddo   !end ipoly

      close(unit=iu_bto)
      close(unit=iu_bt1)
      close(unit=iu_bt2)
c      stop !! per ora
      return

      
3000  format(
     >'    Geom. Center Vect.: modulus (ang.) :',1(f10.5),/,
     >'                        alpha   (deg) .:',1(f10.5),/,
     >'                        beta    (deg) .:',1(f10.5),/,
     >'    Rotation Angles:    alpha_a (deg) .:',1(f10.5),/,
     >'                        beta_a  (deg) .:',1(f10.5),/,
     >'                        gamma_a (deg) .:',1(f10.5))
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end


      function ran1(idum)

      implicit real*8 (a-h,o-z)
      dimension r(97)
      parameter (m1=259200,ia1=7141,ic1=54773,rm1=3.8580247d-6)
      parameter (m2=134456,ia2=8121,ic2=28411,rm2=7.4373773d-6)
      parameter (m3=243000,ia3=4561,ic3=51349)
      data iff /0/
      if (idum.lt.0.or.iff.eq.0) then
        iff=1
        ix1=mod(ic1-idum,m1)
        ix1=mod(ia1*ix1+ic1,m1)
        ix2=mod(ix1,m2)
        ix1=mod(ia1*ix1+ic1,m1)
        ix3=mod(ix1,m3)
        do 11 j=1,97
          ix1=mod(ia1*ix1+ic1,m1)
          ix2=mod(ia2*ix2+ic2,m2)
          r(j)=(dfloat(ix1)+dfloat(ix2)*rm2)*rm1
11      continue
        idum=1
      endif
      ix1=mod(ia1*ix1+ic1,m1)
      ix2=mod(ia2*ix2+ic2,m2)
      ix3=mod(ia3*ix3+ic3,m3)
      j=1+(97*ix3)/m3
      if(j.gt.97.or.j.lt.1)pause
      ran1=r(j)
      r(j)=(dfloat(ix1)+dfloat(ix2)*rm2)*rm1
      return
      end


      subroutine sigplot(ipdb1,ipdb2)

      include 'quafit2014.cmn'
      
      if (isigplotregular.eq.1) then
      n=4
      do iar=1,n
      do ibr=1,n
      do ia2=1,n
      do ib2=1,n
      do ig2=1,n
      ar=360./n*(iar-1)
      br=180./n*(ibr-1)
      a2=360./n*(ia2-1)
      b2=180./n*(ib2-1)
      g2=360./n*(ig2-1)
      write(text(1:15),'(5i3)') int(0.5+ar),int(0.5+br),
     >              int(0.5+a2),int(0.5+b2),int(0.5+g2)
      text(16:16)='_'
      write(text(17:18),'(i2)') ipdb1
      write(text(19:20),'(i2)') ipdb2
      do k=1,20
        if (text(k:k).eq.' ') text(k:k)='0'
      enddo
      open (unit=iu_bt1,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//text(1:20)//'.out')
      open (unit=iu_bt2,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//text(1:20)//'.pdb')

      ar=ar/conv
      br=br/conv
      a2=a2/conv
      b2=b2/conv
      g2=g2/conv


      do isite=1,2
      if (isite.eq.1) then
        ipdb=ipdb1
        xt=zer
        yt=zer
        zt=zer
        aa=zer
        bb=zer
        gg=zer
      else
        ipdb=ipdb2
        call sigcal_new(ipdb1,ipdb2,zer,zer,zer,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)
        dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
        ur=dsmax-reddsmax
        xt=ur*dsin(br)*dcos(ar)
        yt=ur*dsin(br)*dsin(ar)
        zt=ur*dcos(br)
        aa=a2
        bb=b2
        gg=g2
c        write(*,*) "ur ",ur
      endif
      natot=0

      i66=0
      i66=i66+1
      call mat(-gg,-bb,-aa,rot)

      sum1=0.d0
      sum2=0.d0
      sum3=0.d0
      sum4=0.d0

      nato=0
  
      iname=lnfil(fsub(ipdb),300)
c      write(*,*) iu_pdb,ipdb,fsub(ipdb)
      open(unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:iname)
     >//'.pdb')
99    read(iu_pdb,'(a200)',end=10) text
      if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 99
      read(unit=text,fmt=1000,end=10) ina,ch8,dum1,dum2,dum3
     
      ch1=ch8(2:2)
      hc3=ch8(6:8)
      sum1=sum1+dum1
      sum2=sum2+dum2
      sum3=sum3+dum3
      nato=nato+1
      if (nato.gt.mxrun) stop 'nato > mxrun'
      coord(nato,1)=dum1
      coord(nato,2)=dum2
      coord(nato,3)=dum3
      goto 99
10    close(unit=iu_pdb)
      if (nato.eq.0) stop 'nato=0, sigplot'

c.....Trasla in modo che il centro coincida con il c.geom.
      dum1=1.d0/dfloat(nato)
      sum1=sum1*dum1
      sum2=sum2*dum1
      sum3=sum3*dum1

c.....Trasla in modo che il centro di massa della subunita' coincida
c.....con xt,yt,zt e poi ruota con la matrice rot

      do 30 i=1,nato
        x1=coord(i,1)-sum1
        y1=coord(i,2)-sum2
        z1=coord(i,3)-sum3
c        write(*,*) x1,y1,z1,rx,ry,rz,xt,yt,zt,rot
c        pause
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        coord(i,1)=rx
        coord(i,2)=ry
        coord(i,3)=rz
30    continue

c#####Stampa sul pdb totale
          
      i=0
          
      open(unit=iu_pdb,status='unknown',
     >   file=fsub(ipdb)(1:iname)//'.pdb')
999   read(iu_pdb,'(a200)',end=109) text
c      write(*,*) text
      if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 999
      i=i+1
      natot=natot+1
      x1=coord(i,1)
      y1=coord(i,2)
      z1=coord(i,3)
      write(iu_bt2,1001) text(1:6),i,text(12:21),
     >       char(66+(-1)**(isite-1)),
     >               text(23:30),x1,y1,z1
      write(iu_bt1,1100) x1,y1,z1,float(isite)
      goto 999
109   close(unit=iu_pdb)
      

      enddo !isite
      close(unit=iu_bt1)
      close(unit=iu_bt2)

      enddo
      enddo
      enddo
      enddo
      enddo

c######################################################################

      else


      nrnd=100
      do ii=1,nrnd
      ar=360.*rand(0)
      br=dacos(-1.d0+2*rand(0))*conv
      a2=360.*rand(0)
      b2=dacos(-1.d0+2*rand(0))*conv
      g2=360.*rand(0)
      write(text(1:15),'(5i3)') int(0.5+ar),int(0.5+br),
     >              int(0.5+a2),int(0.5+b2),int(0.5+g2)
      text(16:16)='_'
      write(text(17:18),'(i2)') ipdb1
      write(text(19:20),'(i2)') ipdb2
      do k=1,20
        if (text(k:k).eq.' ') text(k:k)='0'
      enddo
      open (unit=iu_bt1,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//text(1:20)//'.out')
      open (unit=iu_bt2,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//text(1:20)//'.pdb')

      ar=ar/conv
      br=br/conv
      a2=a2/conv
      b2=b2/conv
      g2=g2/conv


      do isite=1,2
      if (isite.eq.1) then
        ipdb=ipdb1
        xt=zer
        yt=zer
        zt=zer
        aa=zer
        bb=zer
        gg=zer
      else
        ipdb=ipdb2
        call sigcal_new(ipdb1,ipdb2,zer,zer,zer,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)
        dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
        ur=dsmax-reddsmax
        xt=ur*dsin(br)*dcos(ar)
        yt=ur*dsin(br)*dsin(ar)
        zt=ur*dcos(br)
        aa=a2
        bb=b2
        gg=g2
c        write(*,*) "ur ",ur
      endif
      natot=0

      i66=0
      i66=i66+1
      call mat(-gg,-bb,-aa,rot)

      sum1=0.d0
      sum2=0.d0
      sum3=0.d0
      sum4=0.d0

      nato=0
  
      iname=lnfil(fsub(ipdb),300)
c      write(*,*) iu_pdb,ipdb,fsub(ipdb)
      open(unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:iname)
     >//'.pdb')
899   read(iu_pdb,'(a200)',end=810) text
      if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 899
      read(unit=text,fmt=1000,end=810) ina,ch8,dum1,dum2,dum3
     
      ch1=ch8(2:2)
      hc3=ch8(6:8)
      sum1=sum1+dum1
      sum2=sum2+dum2
      sum3=sum3+dum3
      nato=nato+1
      if (nato.gt.mxrun) stop 'nato > mxrun'
      coord(nato,1)=dum1
      coord(nato,2)=dum2
      coord(nato,3)=dum3
      goto 899
810    close(unit=iu_pdb)
      if (nato.eq.0) stop 'nato=0, sigplot'

c.....Trasla in modo che il centro coincida con il c.geom.
      dum1=1.d0/dfloat(nato)
      sum1=sum1*dum1
      sum2=sum2*dum1
      sum3=sum3*dum1

c.....Trasla in modo che il centro di massa della subunita' coincida
c.....con xt,yt,zt e poi ruota con la matrice rot

      do i=1,nato
        x1=coord(i,1)-sum1
        y1=coord(i,2)-sum2
        z1=coord(i,3)-sum3
c        write(*,*) x1,y1,z1,rx,ry,rz,xt,yt,zt,rot
c        pause
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        coord(i,1)=rx
        coord(i,2)=ry
        coord(i,3)=rz
      enddo

c#####Stampa sul pdb totale
          
      i=0
          
      open(unit=iu_pdb,status='unknown',
     >   file=fsub(ipdb)(1:iname)//'.pdb')
799   read(iu_pdb,'(a200)',end=709) text
c      write(*,*) text
      if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 799
      i=i+1
      natot=natot+1
      x1=coord(i,1)
      y1=coord(i,2)
      z1=coord(i,3)
      write(iu_bt2,1001) text(1:6),i,text(12:21),
     >       char(66+(-1)**(isite-1)),
     >               text(23:30),x1,y1,z1
      write(iu_bt1,1100) x1,y1,z1,float(isite)
      goto 799
709   close(unit=iu_pdb)
      

      enddo !isite
      close(unit=iu_bt1)
      close(unit=iu_bt2)

      enddo

c######################################################################

      endif

      nar=200
      n=2
      do ibr=n,n
      do ia2=1,n
      do ib2=1,n
      do ig2=1,n
      br=180./n*(ibr-1)
      a2=45+(ia2-1)*60
      b2=180./n*(ib2-1)
      g2=60+(ig2-1)*90
      write(text(1:15-3),'(5i3)') int(0.5+br),
     >              int(0.5+a2),int(0.5+b2),int(0.5+g2)
      text(16-3:16-3)='_'
      write(text(17-3:18-3),'(i2)') ipdb1
      write(text(19-3:20-3),'(i2)') ipdb2
      do k=1,20-3
        if (text(k:k).eq.' ') text(k:k)='0'
      enddo
      open (unit=iu_bt1,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//text(1:20-3)//'alphar.out')
      br=br/conv
      a2=a2/conv
      b2=b2/conv
      g2=g2/conv
      do iar=1,nar
      ar=360.d0/(nar-1)*(iar-1)
      ar=ar/conv

      call sigcal_new(ipdb1,ipdb2,zer,zer,zer,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)
      dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
      ur=dsmax-reddsmax
c      ur=sigcal_new(ipdb1,ipdb2,zer,zer,zer,ar,b2,g2,a2,br)
      write(iu_bt1,1100) ar,ur
      enddo
      close(unit=iu_bt1)

      enddo
      enddo
      enddo
      enddo

c######################################################################
      nar=200
      n=2
      round=10.
      do ibr=n,n
      do ia2=1,n
      do ib2=1,n
      do ig2=1,n
      br=(int(acos((2*rand(0)-1)*0.8)*conv/round+0.5)*round)
      a2=(int(360.*rand(0)/round+0.5)*round)
      b2=(int(acos((2*rand(0)-1)*0.8)*conv/round+0.5)*round)
      g2=(int(360.*rand(0)/round+0.5)*round)
      write(text(1:15-3),'(5i3)') int(0.5+br),
     >              int(0.5+a2),int(0.5+b2),int(0.5+g2)
      text(16-3:16-3)='_'
      write(text(17-3:18-3),'(i2)') ipdb1
      write(text(19-3:20-3),'(i2)') ipdb2
      do k=1,20-3
        if (text(k:k).eq.' ') text(k:k)='0'
      enddo
      open (unit=iu_bt1,status='unknown',file=
     > famstone(1:lnfil(famstone,300))//text(1:20-3)//'alphar_rnd.out')
      br=br/conv
      a2=a2/conv
      b2=b2/conv
      g2=g2/conv
      do iar=1,nar
      ar=360.d0/(nar-1)*(iar-1)
      ar=ar/conv

      call sigcal_new(ipdb1,ipdb2,zer,zer,zer,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)
      dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
      ur=dsmax-reddsmax
c      ur=sigcal_new(ipdb1,ipdb2,zer,zer,zer,ar,b2,g2,a2,br)
      write(iu_bt1,1100) ar,ur
      enddo
      close(unit=iu_bt1)

      enddo
      enddo
      enddo
      enddo


      return

      
3000  format(
     >'    Geom. Center Vect.: modulus (ang.) :',1(f10.5),/,
     >'                        alpha   (deg) .:',1(f10.5),/,
     >'                        beta    (deg) .:',1(f10.5),/,
     >'    Rotation Angles:    alpha_a (deg) .:',1(f10.5),/,
     >'                        beta_a  (deg) .:',1(f10.5),/,
     >'                        gamma_a (deg) .:',1(f10.5))
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end

      function lnfil(nome,n)

      implicit real*8 (a-h,o-z)
      character*(*) nome
      do 10 lnfil=n,1,-1
        if (nome(lnfil:lnfil).ne.' ') return
10    continue
      lnfil=0
      end





      subroutine rotrin(x1,y1,z1,x2,y2,z2,xt,yt,zt,rot)

      implicit real*8 (a-h,o-z)

      dimension rot(3,3)

c.....Traslazione
      x2t = x1-xt
      y2t = y1-yt
      z2t = z1-zt
c.....Rotazione secondo la rot
      x2 = rot(1,1)*x2t + rot(2,1)*y2t + rot(3,1)*z2t
      y2 = rot(1,2)*x2t + rot(2,2)*y2t + rot(3,2)*z2t
      z2 = rot(1,3)*x2t + rot(2,3)*y2t + rot(3,3)*z2t
      return
      end

      subroutine rotrdi(x1,y1,z1,x2,y2,z2,xt,yt,zt,rot)

      implicit real*8 (a-h,o-z)

      dimension rot(3,3)

c.....Rotazione secondo la rot
      xr = rot(1,1)*x1 + rot(1,2)*y1 + rot(1,3)*z1
      yr = rot(2,1)*x1 + rot(2,2)*y1 + rot(2,3)*z1
      zr = rot(3,1)*x1 + rot(3,2)*y1 + rot(3,3)*z1
c.....Traslazione
      x2 = xt+xr
      y2 = yt+yr
      z2 = zt+zr
      return
      end



      subroutine tam(alpha,beta,gamma,a)

c.....Si calcolano gli angoli di Eulero  alpha, beta, gamma
c.....che descrivano la matrice a di rotazione del sistema di riferimento.
c.....La matrice e' tratta dal Rose, pag 65.

      implicit real*8 (a-h,o-z)
      parameter(conv   = 57.2957795130823208767981548141d0)
      dimension a(3,3)

      cb = a(3,3)
      if (cb.gt. 1.d0) cb= 1.d0
      if (cb.lt.-1.d0) cb=-1.d0
      if (dabs(cb).ge.1.d0) then
        sb=0.d0
        ca=a(1,1)/cb
        sa=a(1,2)/cb
        cg=1.d0
        sg=0.d0
      else
        sb=sfromc(cb)
        ca=a(3,1)/sb
        sa=a(3,2)/sb
        cg=-a(1,3)/sb
        sg=a(2,3)/sb
      endif
      alpha=polar(ca,sa)
      beta =dacos(cb)
      gamma=polar(cg,sg)
      return
      end

      subroutine taminv(alpha,beta,gamma,a)

c.....Si calcolano gli angoli di Eulero  alpha, beta, gamma
c.....che descrivano la matrice inversa della matrice a di rotazione del sistema di riferimento.
c.....La matrice e' tratta dal Rose, pag 65.

      implicit real*8 (a-h,o-z)
      parameter(conv   = 57.2957795130823208767981548141d0)
      dimension a(3,3)

      cb = a(3,3)
      if (cb.gt. 1.d0) cb= 1.d0
      if (cb.lt.-1.d0) cb=-1.d0
      if (dabs(cb).ge.1.d0) then
        sb=0.d0
        ca=a(1,1)/cb
        sa=a(2,1)/cb !!
        cg=1.d0
        sg=0.d0
      else
        sb=sfromc(cb)
        ca=a(1,3)/sb
        sa=a(2,3)/sb
        cg=-a(3,1)/sb
        sg=a(3,2)/sb
      endif
      alpha=polar(ca,sa)
      beta =dacos(cb)
      gamma=polar(cg,sg)
      return
      end

      subroutine mat(alpha,beta,gamma,a)

c.....Si calcola la matrice a di rotazione del sistema di riferimento
c.....degli angoli di Eulero  alpha, beta, gamma.
c.....La matrice e' tratta dal Rose, pag 65.

      implicit real*8 (a-h,o-z)

      parameter(zer=0.d0)
      dimension a(3,3)

c.....Definizione funzioni trigonometriche
      ca = dcos(alpha)
      cb = dcos(beta )
      cg = dcos(gamma)
      sa = dsin(alpha)
      sb = dsin(beta )
      sg = dsin(gamma)
c.....Definizione elementi di matrice a
      a(1,1) =  ca*cb*cg - sa*sg
      a(1,2) =  sa*cb*cg + ca*sg
      a(1,3) = -sb*cg
      a(2,1) = -ca*cb*sg - sa*cg
      a(2,2) = -sa*cb*sg + ca*cg
      a(2,3) =  sb*sg
      a(3,1) =  ca*sb
      a(3,2) =  sa*sb
      a(3,3) =  cb
      return
      end


      DOUBLE PRECISION FUNCTION CG(L1,L2,L3,M1,M2)
C
C   Restituisce il coefficiente di Clebsch-Gordan C(L1,L2,L3;M1,M2,M1+M2)
C   corrispondente
C
      IMPLICIT REAL*8 (A-H,O-Z)
      A = DFLOAT(L1)
      B = DFLOAT(L2)
      C = DFLOAT(L3)
      X = DFLOAT(M1)
      Y = DFLOAT(M2)
      Z = DFLOAT(M1+M2)
      IF (IDINT(C-A-B)) 301,301,302
 302  CG = 0.D0
      GOTO 399
 301  IF (IDINT(A-B-C)) 303,303,302
 303  IF (IDINT(B-A-C)) 304,304,302
 304  IF (IDINT(Z-X-Y)) 302,305,302
 305  IF (DABS(Z) .GT. C) GOTO 302
      IF (DABS(Y) .GT. B) GOTO 302
      IF (DABS(X) .GT. A) GOTO 302
      IF (IDINT(B) .NE. 0) GOTO 5
      IF (IDINT(A) .EQ. IDINT(C)) THEN
        CG = 1.D0
      ELSE
        CG = 0.D0
      ENDIF
      GOTO 399
   5  IF (IDINT(C) .NE. 0) GOTO 6
      IF (IDINT(A) .EQ. IDINT(B)) THEN
        FMULT = (-1.D0)**(IDINT(A-X))
	CG = FMULT/DSQRT(2.D0*A + 1.D0)
      ELSE
	CG = 0.D0
      ENDIF
      GOTO 399
   6  NMAX = IDINT(DMIN1(C-A+B,C+Z)) + 1
      NMIN = IDINT(DMAX1(0.D0,B-A+Z)) + 1
      SUM = 0.D0
      CG = 1.D0
      X1 = A + X
      Y1 = B + Y
      Z1 = C + Z
      X2 = A - X
      Y2 = B - Y
      Z2 = C - Z
      Z3 = 2.D0 * C + 1.D0
      R = A + B + C + 1.D0
      S = C + B + X
      T = B + C - A
      J = IDINT (Y1)
      DO 306 I = NMIN,NMAX
      N = I - 1
      U = DFLOAT(N)
 306  SUM = SUM + (-1.D0)**N * BICO(Z1,U) * BICO(S-U,X1) *
     C BICO(X2+U,Y1)
      SUM = SUM * (-1.D0)**J
      IF (IDINT(X)) 11,12,13
  11  G = 1.D0 / BICO(X2,X1)
      L = - 1
      GOTO 14
  12  G = 1.D0
      L = 1
      GOTO 14
  13  G = BICO(X1,X2)
      L = 1
  14  IF (IDINT(Y)) 15,16,17
  15  G = G / BICO(Y2,Y1)
      L = L - 2
      GOTO 18
  16  L = L + 2
      GOTO 18
  17  G = G * BICO(Y1,Y2)
      L = L + 2
  18  IF (IDINT(Z)) 19,20,21
  19  G = G * BICO(Z2,Z1)
      L = L - 3
      GOTO 22
  20  L = L + 1
      GOTO 22
  21  G = G / BICO(Z1,Z2)
      L = L + 1
  22  L = (8 + L) / 2
      GOTO (23,24,25,26,27,28),L
  23  G = G * BICO(-2.D0*Z,-2.D0*X)
      GOTO 307
  24  G = G / BICO(-2.D0*Y,-2.D0*Z)
      GOTO 307
  25  G = G / BICO(-2.D0*X,-2.D0*Z)
      GOTO 307
  26  G = G * BICO(2.D0*X,2.D0*Z)
      GOTO 307
  27  G = G * BICO(2.D0*Y,2.D0*Z)
      GOTO 307
  28  G = G / BICO(2.D0*Z,2.D0*X)
 307  CG = CG * SUM * DSQRT(G*Z3/(BICO(R-1.D0,2.D0*C) *
     C BICO(2.D0*C,T) * R ))
 399  RETURN
      END
      DOUBLE PRECISION FUNCTION BICO(A,B)
      IMPLICIT REAL*8 (A-H,O-Z)
      X = A - B
      IF (IDINT(X)) 201,202,203
 201  BICO = 0.D0
      GOTO 207
 202  BICO = 1.D0
      GOTO 207
 203  IF (IDINT(B)) 201,202,204
 204  Y = DMIN1(B,X)
      BICO = A / Y
      J = IDINT(Y) - 1
      IF (J .LT. 1) GOTO 207
      DO 205 I = 1,J
      U = DFLOAT(I)
 205  BICO = BICO * (A-U) / (Y - U)
 207  RETURN
      END





c######################################################################
      
      function xscale(x,xmin,xmax)

      implicit real*8 (a-h,o-z)
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)

      xscale=0.50*(xmax+xmin) + datan(x)*(xmax-xmin)/pi
      return
      end

      function xscinv(x,xmin,xmax)

      implicit real*8 (a-h,o-z)
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)

      xscinv=dtan((x-0.5*(xmax+xmin))*pi/(xmax-xmin))
      return
      end



      subroutine monk(funct,x,f,n,maxit,iseed)

      implicit real*8 (a-h,o-z)
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)
      parameter(pih    = pi*0.5d0)
      dimension x(100),p(100)

      f=1.d+38
      do 10 i = 1,maxit
        do 20 j=1,n
c.rand0c         p(j)=-3.0+rand(0)*6.0
c         p(j)=-3.0+rand(0)*6.0
c.rand0         p(j)=-pih+rand(0)*pi
         p(j)=-pih+rand(0)*pi
20      continue
        call funct(n,p,dum1)
        if (dum1.lt.f) then
          f=dum1
          do 30 j=1,n
            x(j)=p(j)
30        continue
        endif
10    continue
      return
      end

      subroutine simplx(funct,cf,f,nn,df,itc,maxit)

c   cf    = vettore contenete in ingresso i parametri di prova e
c           in uscita i parametri minimizzati.
c   f     = Valore finale della funzione f
c   nn    = numero dei parametri da minimizzare (max=500)
c   df    = limite di convergenza, definito come differenza frazionaria
c           della funzione f in due iterazioni successive
c   itc   = in uscita contiene il numero dell'iterazione alla quale e'
c           stata raggiunta la convergenza
c   maxit = numero massimo di iterazioni consentite

c  Il sottoprogramma richiama la routine funct(nn,cf,f) che restituisce
c  il valore della funzione scalare f da minimizzare corrispondente al
c  set di parametri cf

      implicit real*8 (a-h,o-z)
      external funct
      dimension cf(nn),p(700+3,700),ch(700+3),ct(700)
      integer*4 h,h2
      data  rf,es,cn,sc/0.9985d0,1.95d0,0.4985d0,0.5d0/

c
c   Definisci gli nn+1 punti del simplesso di prova
c
      if (nn.gt.700) stop 'simplex error nn>700'
      it = 0
      np1 = nn+1
      np2 = nn+2
      np3 = nn+3
      do 10 i = 1,np1
      do 10 j = 1,nn
      p(i,j) = cf(j)
      if ((i-j).ne.1) goto 10
      if (cf(j) .lt. 1.d-3) then
	 p(i,j) = 0.1d0
      else
	 p(i,j) = 1.1d0 * cf(j)
      endif
   10 continue
c
c   Calcola il chi-quadro per gli nn+1 set di parametri
c
   12 do 20 i = 1,np1
      do 21 j = 1,nn
   21 cf(j) = p(i,j)
      call funct(nn,cf,dum1)
      it = it + 1
      if (it.gt.maxit) goto 500
c      open(unit=10,status='unknown',file='simplex.log')    
c. riga precedente scritta da Fabrizio
c      write(10,*) it                                       
c. riga precedente scritta da Fabrizio
c      close(unit=10)                                       
c. riga precedente scritta da Fabrizio
      ch(i) = dum1
   20 continue
c
c   Cerca il valore minimo, il massimo e il secondo massimo tra i
c   valori di chi-quadro calcolati
c
      l = 1
      h = 1
      do 30 i = 1,np1
      if (ch(i).lt.ch(l)) l = i
      if (ch(i).gt.ch(h)) h = i
   30 continue
      chmm = ch(l)
   34 h2 = l
      do 32 i = 1,np1
      if (ch(i).ge.ch(h2).and.i.ne.h) h2 = i
   32 continue
c
c  Calcola il centroide di tutti i punti escluso quello
c  corrispondente a h
c
      do 40 j = 1,nn
      ct(j) =  - p(h,j)
      do 42 i = 1,np1
   42 ct(j) = ct(j) + p(i,j)
   40 ct(j) = ct(j) / nn
c

c  Riflessione del punto corrispondente a h rispetto al centroide
c
   48 do 50 j = 1,nn
      p(np2,j) = (1.d0 + rf) * ct(j) - rf * p(h,j)
   50 cf(j) = p(np2,j)
      call funct(nn,cf,e1)
      it = it + 1
      if (it.gt.maxit) goto 500
c      open(unit=10,status='unknown',file='simplex.log')    
c. riga precedente scritta da Fabrizio
c      write(10,*) it                                       
c. riga precedente scritta da Fabrizio
c      close(unit=10)                                       
c. riga precedente scritta da Fabrizio
      if (e1.lt.ch(l)) then
	l = h
	h = h2
	goto 90
      endif
      if (e1.ge.ch(h)) goto 120
   60 do 62 j = 1,nn
   62 p(h,j) = p(np2,j)
      ch(h) = e1
      if (e1.gt.ch(h2)) goto 48
      h = h2
      goto 34
c
c  Espansione rispetto al centroide
c
   90 do 52 j = 1,nn
      p(np3,j) = p(np2,j) * es + (1.d0 - es) * ct(j)
   52 cf(j) = p(np3,j)
      call funct(nn,cf,e2)
      it = it + 1
      if (it.gt.maxit) goto 500
c      open(unit=10,status='unknown',file='simplex.log')    
c. riga precedente scritta da Fabrizio
c      write(10,*) it                                       
c. riga precedente scritta da Fabrizio
c      close(unit=10)                                       
c. riga precedente scritta da Fabrizio
      if (e2.lt.e1) goto 100
   78 do 80 j = 1,nn
   80 p(l,j) = p(np2,j)
      ch(l) = e1
      goto 82
  100 do 54 j = 1,nn
   54 p(l,j) = p(np3,j)
      ch(l) = e2
   82 test = dabs ((chmm - ch(l)) / chmm)
      if (test.le.df.or.ch(l).le.df) then
c
c  Uscita dal sottoprogramma
c
	do 56 j = 1,nn
   56   cf(j) = p(l,j)
	f = ch(l)
	itc = it
	return
      endif
      chmm = ch(l)
      goto 34
c
c  Contrazione rispetto al centroide
c
  120 do 58 j = 1,nn
      p(np2,j) = (1.d0 - cn) * ct(j) + cn * p(h,j)
   58 cf(j) = p(np2,j)
      call funct(nn,cf,e1)
      it = it + 1
      if (it.gt.maxit) goto 500

c      open(unit=10,status='unknown',file='simplex.log')    
c. riga precedente scritta da Fabrizio
c      write(10,*) it                                       
c. riga precedente scritta da Fabrizio
c      close(unit=10)                                       
c. riga precedente scritta da Fabrizio
      if (e1.gt.ch(l)) goto 130
      l = h
      h = h2
      goto 78
  130 if (e1.lt.ch(h)) goto 60
c
c  Scaling dei punti del simplesso
c
      do 70 i = 1,np1
      do 70 j = 1,nn
      p(i,j) = p(i,j) + sc * (p(l,j) - p(i,j))
   70 continue
      goto 12
c
c  Modifica effettuata da Francesco il 13/6/91.
c
500   itc = maxit

c  500 write (6,555) maxit
c  555 format(1x,'Errore in simplx',/,1x,'Superato il numero',
c     c', massimo di iterazioni consentito (',i4,')')
      return
      end

      subroutine uertst(ier,name)
      integer*4            ier
      character          ieq
      character*2        name(3),namset(3),nameq(3)
      data               namset/'UE','RS','ET'/
      data               nameq/'  ','  ','  '/
      data               level/4/,ieqdf/0/,ieq/'='/
      if (ier.gt.999) go to 25
      if (ier.lt.-32) go to 55
      if (ier.le.128) go to 5
      if (level.lt.1) go to 30
      if (ieqdf.eq.1) write(*,35) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(*,35) ier,name
      go to 30
    5 if (ier.le.64) go to 10
      if (level.lt.2) go to 30
      if (ieqdf.eq.1) write(*,40) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(*,40) ier,name
      go to 30
   10 if (ier.le.32) go to 15
      if (level.lt.3) go to 30
      if (ieqdf.eq.1) write(*,45) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(*,45) ier,name
      go to 30
   15 continue
      do 20 i=1,3
	 if (name(i).ne.namset(i)) go to 25
   20 continue
      levold = level
      level = ier
      ier = levold
      if (level.lt.0) level = 4
      if (level.gt.4) level = 4
      go to 30
   25 continue
      if (level.lt.4) go to 30
      if (ieqdf.eq.1) write(*,50) ier,nameq,ieq,name
      if (ieqdf.eq.0) write(*,50) ier,name
   30 ieqdf = 0
      return
   35 format(19h *...terminal error,10x,7h(ier = ,i3,
     1       20h) from libr routine ,3a2,a1,3a2)
   40 format(36h *...warning with fix error  (ier = ,i3,
     1       20h) from libr routine ,3a2,a1,3a2)
   45 format(18h *...warning error,11x,7h(ier = ,i3,
     1       20h) from libr routine ,3a2,a1,3a2)
   50 format(20h *...undefined error,9x,7h(ier = ,i5,
     1       20h) from libr routine ,3a2,a1,3a2)
   55 ieqdf = 1
      do 60 i=1,3
   60 nameq(i) = name(i)
      return
      end
      subroutine ugetio(iopt,nin,nout)
      integer*4            iopt,nin,nout
      integer*4            nind,noutd
      data               nind/1/,noutd/2/
      if (iopt.eq.3) go to 10
      if (iopt.eq.2) go to 5
      if (iopt.ne.1) go to 9005
      nin = nind
      nout = noutd
      go to 9005
    5 nind = nin
      go to 9005
   10 noutd = nout
 9005 return
      end

      subroutine zxmin (funct,n,nsig,maxfn,iopt,x,h,g,f,w,ier)
      implicit real*8 (a-h,o-z)
      external funct
      integer*4            n,nsig,maxfn,iopt,ier
      integer*4            ig,igg,is,idiff,ir,ij,i,j,nm1,jj,jp1,l,kj,k,
     *                   ifn,link,itn,ii,im1,jnt,np1,jb,nj
      real*8             x(n),g(n),h(n*(n+1)/2),f,w(3*n)
      real*8             reps,ax,zero,one,half,seven,five,twelve,ten,hh,
     *                   eps,hjj,v,df,relx,gs0,diff,aeps,alpha,ff,tot,
     *                   f1,f2,z,gys,dgs,sig,zz,gnrm,p1,hhh,ghh,h2,f11,
     *                   f12,f21,f22,hmax,hmin
      data               reps/1.1921d-07/,ax/0.1d0/
      data               zero/0.d0/,one/1.d0/,half/0.5d0/,
     *                   seven/7.d0/,five/5.d0/,twelve/12.d0/,
     *                   ten/10.d0/,p1/0.1d0/
      ier = 0
      hh = dsqrt(reps)
      h2 = dsqrt(hh)
      eps = ten**(-nsig)
      ig = n
      igg = n+n
      is = igg
      idiff = 1
      ir = n
      w(1) = -one
      w(2) = zero
      w(3) = zero
      do 5 i=1,n
	 g(i) = x(i)
    5 continue
      call funct(n,g,f)
      ifn = 1
      if (iopt.eq.1) go to 50
      if (n.eq.1) go to 20
      ij = 2
      do 15 i=2,n
	 do 10 j=2,i
	    h(ij) = zero
	    ij = ij+1
   10    continue
	 ij = ij+1
   15 continue
   20 if (iopt.ne.0) go to 30
      ij = 0
      do 25 i=1,n
	 ij = ij+i
	 h(ij) = one
   25 continue
      go to 95
   30 im1 = 1
      nm1 = 1
      np1 = n+1
      do 35 i=2,np1
	 hhh = h2*dmax1(dabs(x(im1)),ax)
	 g(im1) = x(im1)+hhh
	 call funct(n,g,f2)
	 g(im1) = x(im1)-hhh
	 call funct(n,g,ff)
	 h(nm1) = (ff-f+f2-f)/(hhh*hhh)
	 g(im1) = x(im1)
	 im1 = i
	 nm1 = i+nm1
   35 continue
      ifn = ifn+n+n
      if (iopt.ne.3 .or. n.eq.1) go to 50
      jj = 1
      ii = 2
      do 45 i=2,n
	 ghh = h2*dmax1(dabs(x(i)),ax)
	 do 40 j=1,jj
	    hhh = h2*dmax1(dabs(x(j)),ax)
	    g(i) = x(i)+ghh
	    g(j) = x(j)+hhh
	    call funct(n,g,f22)
	    g(i) = x(i)-ghh
	    call funct(n,g,f12)
	    g(j) = x(j)-hhh
	    call funct(n,g,f11)
	    g(i) = x(i)+ghh
	    call funct(n,g,f21)
	    h(ii) = (f22-f21-f12+f11)/(4.d0*hhh*ghh)
	    g(j) = x(j)
	    ii = ii+1
   40    continue
	 g(i) = x(i)
	 jj = jj+1
	 ii = ii+1
   45 continue
      ifn = ifn+((n*n-n)*2)
   50 hmin = h(1)
      hmax = h(1)
      nm1 = 1
      do 55 i=1,n
	 hmin = dmin1(hmin,h(nm1))
	 hmax = dmax1(hmax,h(nm1))
	 nm1 = nm1+i+1
   55 continue
      hmin = dmax1(0.01d0*(dabs(hmax)+dabs(hmin))-hmin,zero)
      nm1 = 1
      do 60 i=1,n
	 h(nm1) = h(nm1)+hmin
	 nm1 = nm1+i+1
   60 continue
      ir = n
      if (n.gt.1) go to 65
      if (h(1).gt.zero) go to 95
      h(1) = zero
      ir = 0
      go to 90
   65 nm1 = n-1
      jj = 0
      do 85 j=1,n
	 jp1 = j+1
	 jj = jj+j
	 hjj = h(jj)
	 if (hjj.gt.zero) go to 70
	 h(jj) = zero
	 ir = ir-1
	 go to 85
   70    if (j.eq.n) go to 85
	 ij = jj
	 l = 0
	 do 80 i=jp1,n
	    l = l+1
	    ij = ij+i-1
	    v = h(ij)/hjj
	    kj = ij
	    do 75 k=i,n
	       h(kj+l) = h(kj+l)-h(kj)*v
	       kj = kj+k
   75       continue
	    h(ij) = v
   80    continue
   85 continue
   90 if (ir.eq.n) go to 95
      ier = 129
      go to 9000
   95 itn = 0
      df = -one
  100 link = 1
      go to 280
  105 continue
      if (ifn.ge.maxfn) go to 240
      itn = itn+1
      do 110 i=1,n
	 w(i) = -w(ig+i)
  110 continue
      if (ir.lt.n) go to 140
      g(1) = w(1)
      if (n.gt.1) go to 115
      w(1) = w(1)/h(1)
      go to 140
  115 ii = 1
      do 125 i=2,n
	 ij = ii
	 ii = ii+i
	 v = w(i)
	 im1 = i-1
	 do 120 j=1,im1
	    ij = ij+1
	    v = v-h(ij)*w(j)
  120    continue
	 g(i) = v
	 w(i) = v
  125 continue
      w(n) = w(n)/h(ii)
      jj = ii
      nm1 = n-1
      do 135 nj=1,nm1
	 j = n-nj
	 jp1 = j+1
	 jj = jj-jp1
	 v = w(j)/h(jj)
	 ij = jj
	 do 130 i=jp1,n
	    ij = ij+i-1
	    v = v-h(ij)*w(i)
  130    continue
	 w(j) = v
  135 continue
  140 relx = zero
      gs0 = zero
      do 145 i=1,n
	 w(is+i) = w(i)
	 diff = dabs(w(i))/dmax1(dabs(x(i)),ax)
	 relx = dmax1(relx,diff)
	 gs0 = gs0+w(ig+i)*w(i)
  145 continue
      if (relx.eq.zero) go to 245
      aeps = eps/relx
      ier = 130
      if (gs0.ge.zero) go to 245
      if (df.eq.zero) go to 245
      ier = 0
      alpha = (-df-df)/gs0
      if (alpha.le.zero) alpha = one
      alpha = dmin1(alpha,one)
      if (idiff.eq.2) alpha = dmax1(p1,alpha)
      ff = f
      tot = zero
      jnt = 0
  150 if (ifn.ge.maxfn) go to 240
      do 155 i=1,n
	 w(i) = x(i)+alpha*w(is+i)
  155 continue
      call funct(n,w,f1)
      ifn = ifn+1
      if (f1.ge.f) go to 180
      f2 = f
      tot = tot+alpha
  160 ier = 0
      f = f1
      do 165 i=1,n
	 x(i) = w(i)
  165 continue
      if (jnt-1) 170, 200, 205
  170 if (ifn.ge.maxfn) go to 240
      do 175 i=1,n
	 w(i) = x(i)+alpha*w(is+i)
  175 continue
      call funct(n,w,f1)
      ifn = ifn+1
      if (f1.ge.f) go to 205
      if (f1+f2.ge.f+f .and. seven*f1+five*f2.gt.twelve*f) jnt = 2
      tot = tot+alpha
      alpha = alpha+alpha
      go to 160
  180 continue
      if (f.eq.ff .and. idiff.eq.2 .and. relx.gt.eps) ier = 130
      if (alpha.lt.aeps) go to 245
      if (ifn.ge.maxfn) go to 240
      alpha = half*alpha
      do 185 i=1,n
	 w(i) = x(i)+alpha*w(is+i)
  185 continue
      call funct(n,w,f2)
      ifn = ifn+1
      if (f2.ge.f) go to 195
      tot = tot+alpha
      ier = 0
      f = f2
      do 190 i=1,n
	 x(i) = w(i)
  190 continue
      go to 200
  195 z = p1
      if (f1+f.gt.f2+f2) z = one+half*(f-f1)/(f+f1-f2-f2)
      z = dmax1(p1,z)
      alpha = z*alpha
      jnt = 1
      go to 150
  200 if (tot.lt.aeps) go to 245
  205 alpha = tot
      do 210 i=1,n
	 w(i) = w(ig+i)
  210 continue
      link = 2
      go to 280
  215 if (ifn.ge.maxfn) go to 240
      gys = zero
      do 220 i=1,n
	 gys = gys+w(ig+i)*w(is+i)
	 w(igg+i) = w(i)
  220 continue
      df = ff-f
      dgs = gys-gs0
      if (dgs.le.zero) go to 105
      if (dgs+alpha*gs0.gt.zero) go to 230
      sig = one/gs0
      ir = -ir
      call zxmjn(h,n,w,sig,g,ir,0,zero)
      do 225 i=1,n
	 g(i) = w(ig+i)-w(igg+i)
  225 continue
      sig = one/(alpha*dgs)
      ir = -ir
      call zxmjn(h,n,g,sig,w,ir,0,zero)
      go to 105
  230 zz = alpha/(dgs-alpha*gs0)
      sig = -zz
      call zxmjn(h,n,w,sig,g,ir,0,reps)
      z = dgs*zz-one
      do 235 i=1,n
	 g(i) = w(ig+i)+z*w(igg+i)
  235 continue
      sig = one/(zz*dgs*dgs)
      call zxmjn(h,n,g,sig,w,ir,0,zero)
      go to 105
  240 ier = 131
      go to 250
  245 if (idiff.eq.2) go to 250
      idiff = 2
      go to 100
  250 if (ier.ne.0) go to 255
      if (relx.le.eps) go to 255
      go to 100
  255 gnrm = zero
      do 260 i=1,n
	 g(i) = w(ig+i)
	 gnrm = gnrm+g(i)*g(i)
  260 continue
      gnrm = dsqrt(gnrm)
      w(1) = gnrm
      w(2) = ifn
      w(3) = -dlog10(dmax1(reps,relx))
      if (n.eq.1) go to 9000
      np1 = n+1
      nm1 = n-1
      jj = (n*(np1))/2
      do 275 jb=1,nm1
	 jp1 = np1-jb
	 jj = jj-jp1
	 hjj = h(jj)
	 ij = jj
	 l = 0
	 do 270 i=jp1,n
	    l = l+1
	    ij = ij+i-1
	    v = h(ij)*hjj
	    kj = ij
	    do 265 k=i,n
	       h(kj+l) = h(kj+l)+h(kj)*v
	       kj = kj+k
  265       continue
	    h(ij) = v
  270    continue
	 hjj = h(jj)
  275 continue
      go to 9000
  280 if (idiff.eq.2) go to 290
      do 285 i=1,n
	 z = hh*dmax1(dabs(x(i)),ax)
	 zz = x(i)
	 x(i) = zz+z
	 call funct(n,x,f1)
	 w(ig+i) = (f1-f)/z
	 x(i) = zz
  285 continue
      ifn = ifn+n
      go to (105, 215), link
  290 do 295 i=1,n
	 z = hh*dmax1(dabs(x(i)),ax)
	 zz = x(i)
	 x(i) = zz+z
	 call funct(n,x,f1)
	 x(i) = zz-z
	 call funct(n,x,f2)
	 w(ig+i) = (f1-f2)/(z+z)
	 x(i) = zz
  295 continue
      ifn = ifn+n+n
      go to (105, 215), link
 9000 continue
      if (ier.ne.0) call uertst(ier,'ZXMIN' )
      return
      end
      subroutine zxmjn  (a,n,z,sig,w,ir,mk,eps)
      implicit real*8 (a-h,o-z)
      integer*4            n,ir,mk
      real*8             a(n*(n+1)/2),z(n),sig,w(n),eps
      integer*4            j,jj,ij,jp1,i,ii,mm
      real*8             zero,one,four,ti,v,tim,al,r,b,gm,y
      data               zero/0.d0/,one/1.d0/,four/4.d0/
      if (n.gt.1) go to 5
      a(1) = a(1)+sig*z(1)*z(1)
      ir = 1
      if (a(1).gt.zero) go to 9005
      a(1) = zero
      ir = 0
      go to 9005
    5 if (sig.gt.zero) go to 65
      if (sig.eq.zero.or.ir.eq.0) go to 9005
      ti = one/sig
      jj = 0
      if (mk.eq.0) go to 15
      do 10 j=1,n
	 jj = jj+j
	 if (a(jj).ne.zero) ti = ti+(w(j)*w(j))/a(jj)
   10 continue
      go to 40
   15 do 20 j=1,n
	 w(j) = z(j)
   20 continue
      do 35 j=1,n
	 jj = jj+j
	 v = w(j)
	 if (a(jj).gt.zero) go to 25
	 w(j) = zero
	 go to 35
   25    ti = ti+(v*v)/a(jj)
	 if (j.eq.n) go to 35
	 ij = jj
	 jp1 = j+1
	 do 30 i=jp1,n
	    ij = ij+i-1
	    w(i) = w(i)-v*a(ij)
   30    continue
   35 continue
   40 if (ir.le.0) go to 45
      if (ti.gt.zero) go to 50
      if (mk-1) 65,65,55
   45 ti = zero
      ir = -ir-1
      go to 55
   50 ti = eps/sig
      if (eps.eq.zero) ir = ir-1
   55 tim = ti
      ii = jj
      i = n
      do 60 j=1,n
	 if (a(ii).ne.zero) tim = ti-(w(i)*w(i))/a(ii)
	 w(i) = ti
	 ti = tim
	 ii = ii-i
	 i = i-1
   60 continue
      mm = 1
      go to 70
   65 mm = 0
      tim = one/sig
   70 jj = 0
      do 110 j=1,n
	 jj = jj+j
	 ij = jj
	 jp1 = j+1
	 v = z(j)
	 if (a(jj).gt.zero) go to 85
	 if (ir.gt.0.or.sig.lt.zero.or.v.eq.zero) go to 80
	 ir = 1-ir
	 a(jj) = (v*v)/tim
	 if (j.eq.n) go to 9005
	 do 75 i=jp1,n
	    ij = ij+i-1
	    a(ij) = z(i)/v
   75    continue
	 go to 9005
   80    ti = tim
	 go to 110
   85    al = v/a(jj)
	 ti = w(j)
	 if (mm.eq.0) ti = tim+v*al
	 r = ti/tim
	 a(jj) = r*a(jj)
	 if (r.eq.zero) go to 115
	 if (j.eq.n) go to 115
	 b = al/ti
	 if (r.gt.four) go to 95
	 do 90 i=jp1,n
	    ij = ij+i-1
	    z(i) = z(i)-v*a(ij)
	    a(ij) = a(ij)+b*z(i)
   90    continue
	 go to 105
   95    gm = tim/ti
	 do 100 i=jp1,n
	    ij = ij+i-1
	    y = a(ij)
	    a(ij) = b*z(i)+y*gm
	    z(i) = z(i)-v*y
  100    continue
  105    tim = ti
  110 continue
  115 if (ir.lt.0) ir = -ir
 9005 continue
      return
      end


      SUBROUTINE sphbes(n,x,sj,sy,sjp,syp)
      INTEGER n
      real*8 sj,sjp,sy,syp,x
CU    USES bessjy
      real*8 factor,order,rj,rjp,ry,ryp,RTPIO2
      PARAMETER (RTPIO2=1.2533141)
      if(n.lt.0.or.x.le.0.)pause 'bad arguments in sphbes'
      order=n+0.5
      call bessjy(x,order,rj,ry,rjp,ryp)
      factor=RTPIO2/sqrt(x)
      sj=factor*rj
      sy=factor*ry
      sjp=factor*rjp-sj/(2.*x)
      syp=factor*ryp-sy/(2.*x)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .
      SUBROUTINE bessjy(x,xnu,rj,ry,rjp,ryp)
      INTEGER MAXIT
      real*8 rj,rjp,ry,ryp,x,xnu,XMIN
      DOUBLE PRECISION EPS,FPMIN,PI
      PARAMETER (EPS=1.e-16,FPMIN=1.e-30,MAXIT=10000,XMIN=2.,
     *PI=3.141592653589793d0)
CU    USES beschb
      INTEGER i,isign,l,nl
      DOUBLE PRECISION a,b,br,bi,c,cr,ci,d,del,del1,den,di,dlr,dli,dr,e,
     *f,fact,fact2,fact3,ff,gam,gam1,gam2,gammi,gampl,h,p,pimu,pimu2,q,
     *r,rjl,rjl1,rjmu,rjp1,rjpl,rjtemp,ry1,rymu,rymup,rytemp,sum,sum1,
     *temp,w,x2,xi,xi2,xmu,xmu2
      if(x.le.0..or.xnu.lt.0.) pause 'bad arguments in bessjy'
      if(x.lt.XMIN)then
        nl=int(xnu+.5d0)
      else
        nl=max(0,int(xnu-x+1.5d0))
      endif
      xmu=xnu-nl
      xmu2=xmu*xmu
      xi=1.d0/x
      xi2=2.d0*xi
      w=xi2/PI
      isign=1
      h=xnu*xi
      if(h.lt.FPMIN)h=FPMIN
      b=xi2*xnu
      d=0.d0
      c=h
      do 11 i=1,MAXIT
        b=b+xi2
        d=b-d
        if(abs(d).lt.FPMIN)d=FPMIN
        c=b-1.d0/c
        if(abs(c).lt.FPMIN)c=FPMIN
        d=1.d0/d
        del=c*d
        h=del*h
        if(d.lt.0.d0)isign=-isign
        if(abs(del-1.d0).lt.EPS)goto 1
11    continue
      write(*,*) x,xnu
      pause 'x too large in bessjy; try asymptotic expansion'
1     continue
      rjl=isign*FPMIN
      rjpl=h*rjl
      rjl1=rjl
      rjp1=rjpl
      fact=xnu*xi
      do 12 l=nl,1,-1
        rjtemp=fact*rjl+rjpl
        fact=fact-xi
        rjpl=fact*rjtemp-rjl
        rjl=rjtemp
12    continue
      if(rjl.eq.0.d0)rjl=EPS
      f=rjpl/rjl
      if(x.lt.XMIN) then
        x2=.5d0*x
        pimu=PI*xmu
        if(abs(pimu).lt.EPS)then
          fact=1.d0
        else
          fact=pimu/sin(pimu)
        endif
        d=-log(x2)
        e=xmu*d
        if(abs(e).lt.EPS)then
          fact2=1.d0
        else
          fact2=sinh(e)/e
        endif
        call beschb(xmu,gam1,gam2,gampl,gammi)
        ff=2.d0/PI*fact*(gam1*cosh(e)+gam2*fact2*d)
        e=exp(e)
        p=e/(gampl*PI)
        q=1.d0/(e*PI*gammi)
        pimu2=0.5d0*pimu
        if(abs(pimu2).lt.EPS)then
          fact3=1.d0
        else
          fact3=sin(pimu2)/pimu2
        endif
        r=PI*pimu2*fact3*fact3
        c=1.d0
        d=-x2*x2
        sum=ff+r*q
        sum1=p
        do 13 i=1,MAXIT
          ff=(i*ff+p+q)/(i*i-xmu2)
          c=c*d/i
          p=p/(i-xmu)
          q=q/(i+xmu)
          del=c*(ff+r*q)
          sum=sum+del
          del1=c*p-i*del
          sum1=sum1+del1
          if(abs(del).lt.(1.d0+abs(sum))*EPS)goto 2
13      continue
        pause 'bessy series failed to converge'
2       continue
        rymu=-sum
        ry1=-sum1*xi2
        rymup=xmu*xi*rymu-ry1
        rjmu=w/(rymup-f*rymu)
      else
        a=.25d0-xmu2
        p=-.5d0*xi
        q=1.d0
        br=2.d0*x
        bi=2.d0
        fact=a*xi/(p*p+q*q)
        cr=br+q*fact
        ci=bi+p*fact
        den=br*br+bi*bi
        dr=br/den
        di=-bi/den
        dlr=cr*dr-ci*di
        dli=cr*di+ci*dr
        temp=p*dlr-q*dli
        q=p*dli+q*dlr
        p=temp
        do 14 i=2,MAXIT
          a=a+2*(i-1)
          bi=bi+2.d0
          dr=a*dr+br
          di=a*di+bi
          if(abs(dr)+abs(di).lt.FPMIN)dr=FPMIN
          fact=a/(cr*cr+ci*ci)
          cr=br+cr*fact
          ci=bi-ci*fact
          if(abs(cr)+abs(ci).lt.FPMIN)cr=FPMIN
          den=dr*dr+di*di
          dr=dr/den
          di=-di/den
          dlr=cr*dr-ci*di
          dli=cr*di+ci*dr
          temp=p*dlr-q*dli
          q=p*dli+q*dlr
          p=temp
          if(abs(dlr-1.d0)+abs(dli).lt.EPS)goto 3
14      continue
        pause 'cf2 failed in bessjy'
3       continue
        gam=(p-f)/q
        rjmu=sqrt(w/((p-f)*gam+q))
        rjmu=sign(rjmu,rjl)
        rymu=rjmu*gam
        rymup=rymu*(p+q/gam)
        ry1=xmu*xi*rymu-rymup
      endif
      fact=rjmu/rjl
      rj=rjl1*fact
      rjp=rjp1*fact
      do 15 i=1,nl
        rytemp=(xmu+i)*xi2*ry1-rymu
        rymu=ry1
        ry1=rytemp
15    continue
      ry=rymu
      ryp=xnu*xi*rymu-ry1
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .
      SUBROUTINE beschb(x,gam1,gam2,gampl,gammi)
      INTEGER NUSE1,NUSE2
      DOUBLE PRECISION gam1,gam2,gammi,gampl,x
      PARAMETER (NUSE1=5,NUSE2=5)
CU    USES chebev
      real*8 xx,c1(7),c2(8),chebev
      SAVE c1,c2
      DATA c1/-1.142022680371172d0,6.516511267076d-3,3.08709017308d-4,
     *-3.470626964d-6,6.943764d-9,3.6780d-11,-1.36d-13/
      DATA c2/1.843740587300906d0,-.076852840844786d0,1.271927136655d-3,
     *-4.971736704d-6,-3.3126120d-8,2.42310d-10,-1.70d-13,-1.d-15/
      xx=8.d0*x*x-1.d0
      gam1=chebev(-1.d0,1.d0,c1,NUSE1,xx)
      gam2=chebev(-1.d0,1.d0,c2,NUSE2,xx)
      gampl=gam2-x*gam1
      gammi=gam2+x*gam1
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .
      FUNCTION chebev(a,b,c,m,x)
      implicit real*8 (a-h,o-z)
      INTEGER m
      real*8 c(m)
      INTEGER j
      if ((x-a)*(x-b).gt.0.) pause 'x not in range in chebev'
      d=0.
      dd=0.
      y=(2.*x-a-b)/(b-a)
      y2=2.*y
      do 11 j=m,2,-1
        sv=d
        d=y2*d-dd+c(j)
        dd=sv
11    continue
      chebev=y*d-dd+0.5*c(1)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software .


      subroutine intlin(x,y,n,xa,ya)

      implicit real*8 (a-h,o-z)

      dimension xa(n),ya(n)

c.....Interpola in modo linear

      ix1=0
      ix2=1
      do 10 i=1,n-1
        ix1=ix1+1
        ix2=ix2+1
        if (  (xa(ix1).gt.x).or.
     >        (xa(ix1).le.x.and
     >        .xa(ix2).ge.x).or.
     >        (xa(ix2).lt.x.and.ix2.eq.n) ) goto 20
10    continue
c.....Banale interpolazione 
20    a = (ya(ix2)-ya(ix1))/(xa(ix2)-xa(ix1))
      b = ya(ix1)-a*xa(ix1)
      y = a*x+b
      return
      end

      subroutine intpar(x,y,n,xa,ya)

      implicit real*8 (a-h,o-z)

      dimension xa(n),ya(n)

c.....Interpola in modo parabolico da un array di n punti
c.....xa la funzione ya nel punto y(x). x puo' anche
c.....essere esterno all'intervallo xa(1)-xa(n)

      ix1=0
      ix2=1
      ix3=2
      do 10 i=1,n-2
        ix1=ix1+1
        ix2=ix2+1
        ix3=ix3+1
        if (  (xa(ix1).gt.x).or.
     >        (xa(ix1).le.x.and
     >        .xa(ix3).ge.x).or.
     >        (xa(ix3).lt.x.and.ix3.eq.n) ) goto 20
10    continue
c.....Banale interpolazione parabolica
20    a = ((xa(ix1)-xa(ix2))*(ya(ix1)-ya(ix3))
     >  -(ya(ix1)-ya(ix2))*(xa(ix1)-xa(ix3)))/
     >  ((xa(ix1)*xa(ix1)-xa(ix3)*xa(ix3))*
     >   (xa(ix1)-xa(ix2))-(xa(ix1)*xa(ix1)-
     >    xa(ix2)*xa(ix2))*(xa(ix1)-xa(ix3)))
      b = (-a*(xa(ix1)*xa(ix1)-xa(ix2)*xa(ix2))+
     >     (ya(ix1)-ya(ix2)))/(xa(ix1)-xa(ix2))
      c = ya(ix1)-a*xa(ix1)*xa(ix1)-b*xa(ix1)
      y = a*x*x+b*x+c
      return
      end

      subroutine psi_s(ipdb1,ipdb2,l1,l2,l3,k1,k2,r,psi)

      include 'quafit2014.cmn'
      complex*16 dum1,dum2

c      write(*,*) '   ---> psi   '
      psi=dcmplx(zer,zer)
      dum0=cg(l1,l2,l3,0,0)
      if (dum0.eq.zer) return
c      dum0=dum0*dsqrt(dfloat((2*l1+1)*(2*l2+1)*(2*l3+1)))
c     >     *(-1)**l3*2.d0/pi   / (pi4**2 *(dcmplx(0.d0,1.d0))**(l1+l2))
      dum0=dum0*dsqrt(dfloat((2*l1+1)*(2*l2+1)*(2*l3+1)))
     >     / (8*pi**3) *(dcmplx(0.d0,1.d0))**(l2-l1)*(-1)**(k2+l3)
      do iq=1,nqgl
        q=qqgl(iq)
        x=q*r
        dum1=almshape(ipdb1,iq,l1,k1,1)
c        write(94,*) r,q,dum1
c        dum2=almshape(ipdb2,iq,l2,k2,1)
        dum2=dconjg(almshape(ipdb2,iq,l2,-k2,1))
        if (x.eq.0.d0) then
          if (l3.eq.0) then
            dum3=1.d0
          else
            dum3=0.d0
          endif
        else
          call sphbes(l3,x,dum3,sy,sjp,syp)
        endif
        psi=psi+q*q*wqgl(iq)*dum3*dum1*dum2
c        write(94,*) q,wqgl(iq),dum3,dum1,dum2,psi
      enddo
      psi=psi*qglmax*half*dum0
c         if (psi.ne.dcmplx(0.d0,0.d0)) write(*,*) psi
      return
      end


      function overlap(ipdb1,ipdb2,a1,b1,g1,a2,b2,g2,
     >                 ar,br,r,iflag,ir,iwig,ib2,ibr)

      include 'quafit2014.cmn'

      complex*16 dum1,dum4

c      write(*,*) '   ---> overlap   '
      cb1=dcos(b1)
      cb2=dcos(b2)
      cbr=dcos(br)
      dum4=dcmplx(zer,zer)
      i1=0
      do 10 l1=0,ltird
        do 20 l2=0,ltird
          do 30 l3=0,ltird
            dum1=cg(l1,l2,l3,0,0)
            if (dum1.eq.zer) goto 30
            do 13 k1=-l1,l1
              do 14 k2=-l2,l2
                if (iflag.eq.1) then
                  i1=i1+1
                  call stone_s(l1,l2,l3,k1,k2,a1,cb1,g1,
     >                  a2,cb2,g2,ar,cbr,iwig,ib2,ibr,stone)
                  dum4=dum4+psiar(i1,ir)*
     >                  dconjg(stone)
                else
                  call psi_s(ipdb1,ipdb2,l1,l2,l3,k1,k2,r,psi)
                  if (psi.
     >ne.dcmplx(0.d0,0.d0)) then
c                  write(*,*) l1,l2,l3,k1,k2,
c     >psi(ipdb1,ipdb2,l1,l2,l3,k1,k2,r),stone(l1,l2,l3,k1,k2,a1,cb1,g1,
c     >                  a2,cb2,g2,ar,cbr,iwig,ib2,ibr)
                  call stone_s(l1,l2,l3,k1,k2,a1,cb1,g1,
     >                  a2,cb2,g2,ar,cbr,iwig,ib2,ibr,stone)
                  dum4=dum4+psi*
     >                  dconjg(stone)
c                   write(*,*)  l1,l2,l3,k1,k2,dum4,
c     >psi,stone
                  endif
                endif
c                write(*,*) ' psi + stone out'
14            continue
13          continue
30        continue
20      continue
10    continue
      overlap=dreal(dum4)
c      write(*,*)  ' r overlap=',r,overlap
      return
      end


      subroutine stone_s(l1,l2,l3,n1,n2,a1,b1,g1,
     >                   a2,b2,g2,ar,br,iwig,ib2,ibr,stone)
c.....Stone's Invariant
c.....S^{l1,l2,l3}_{n1,n2} ([a1,b1,g1],[a2,b2,g2],[ar,br])

      include 'quafit2014.cmn'

c      write(*,*) '   --->  stone  '
      stone=dcmplx(0.d0,0.d0)
      if (iwig.eq.1) then

c.........solo se beta1=0 and so n1=k1

        do 20 k2=-l2,l2
          dum1=cg(l1,l2,l3,n1,k2)
          if (dum1.eq.0.d0) goto 20
          kk=n1+k2
          stone=stone+
     >          dcmplx(dcos(k2*a2), dsin(k2*a2))*
     >          dcmplx(dcos(kk*ar),-dsin(kk*ar))*
     >          pwar(l2,k2,n2,ib2)*
     >          pwar(l3,-kk,0,ibr)*
     >          dum1*(-1)**kk
20      continue
        stone=stone*dcmplx(dcos(n1*a1), dsin(n1*a1))

      else
        do 110 k1=-l1,l1
          do 120 k2=-l2,l2
            dum1=cg(l1,l2,l3,k1,k2)
            if (dum1.eq.0.d0) goto 120
            kk=k1+k2
            stone=stone+
     >            dcmplx(dcos(k1*a1), dsin(k1*a1))*
     >            dcmplx(dcos(k2*a2), dsin(k2*a2))*
     >            dcmplx(dcos(kk*ar),-dsin(kk*ar))*
     >            pwign(l1,k1,n1,b1)*
     >            pwign(l2,k2,n2,b2)*
     >            pwign(l3,-kk,0,br)*
     >            dum1*(-1)**kk
120       continue
110     continue
      endif
      stone=stone*(dcmplx(0.d0,1.d0))**(l2-l1-l3)/dsqrt(dfloat(2*l3+1))
     >         *dcmplx(dcos(n1*g1), dsin(n1*g1))
     >         *dcmplx(dcos(n2*g2), dsin(n2*g2))
      return
      end


      function stone_new(l1,l2,l3,n1,n2)

c.....Stone's Invariant
c.....S^{l1,l2,l3}_{n1,n2} ([a1,cb1,g1],[a2,cb2,g2],[ar,cbr])

      include 'quafit2014.cmn'
      complex*16 dum2

c      write(*,*) '   --->  stone_new  '
      stone_new=dcmplx(0.d0,0.d0)
      do 110 k1=-l1,l1
        dum2=dcmplx(0.d0,0.d0)
        do 120 k2=-l2,l2
          dum1=cg(l1,l2,l3,k1,k2)
          if (dum1.eq.0.d0) goto 120
          kk=k1+k2
          dum2=dum2+
     >          ea2(-k2)*
     >          ear( kk)*
     >          pwar(l2,k2,n2,2)*
     >          pwar(l3,-kk,0,3)*
     >          dum1*(-1)**kk
120     continue
        stone_new=stone_new+dum2*ea1(-k1)*pwar(l1,k1,n1,1)
110   continue
      stone_new=stone_new
     >       *(dcmplx(0.d0,1.d0))**(l2-l1-l3)/dsqrt(dfloat(2*l3+1))
     >       *eg1(-n1)*eg2(-n2)
      return
      end

      subroutine rexp(filee,nskpr,qrmin,qrmax,mn,chifat,nq,nqr,qq,qie,
     >qle,qid,wi,qies,qles,qmin,qmax,scale,errk,nqave,jmouve,alphapower,
     >icolumn3,qinputscale,sinputscale)

      implicit real*8 (a-h,o-z)

      character testo*30,text*100
      dimension qq(mn),qie(mn),qle(mn),qid(mn),wi(mn),
     >qies(mn),qles(mn)

      character filee*300
      if (icolumn3.eq.0.and.errk.eq.0.d0) then
        errk=0.01
      endif
c.....Input data
      write(*,*) ' filee  ' ,filee(1:lnfil(filee,300))
c      write(*,*) nskpr,nqave
      open (unit=3,status='unknown',file=filee(1:lnfil(filee,300)))
      nq=0
      nqr=0
      chifat=0.0
      do i=1,nskpr
        read(3,'(a100)',end=100) text
      enddo
71      dum1=0.d0
        dum2=0.d0
        dum3=0.d0
        i1=0
c        write(*,*) nqave
        do 188 k=1,nqave
70        read(3,'(a100)',end=100) text
c          write(*,*) text
          if (text(1:1).eq.'#') goto 70
c####################
          if (icolumn3.eq.1) then
            read(text,*,err=70) dum1t,dum2t,dum3t
          else
            read(text,*,err=70) dum1t,dum2t
            dum3t=1.0
          endif
c####################
c
c          open(unit=2,status='unknown',file='code.usr')
c             write(2,'(a)') text
c          close (unit=2)
c          open(unit=2,status='unknown',file='code.usr')
c             read(2,*,err=70) dum1t,dum2t,dum3t
c          close (unit=2,status='delete')

c          if (dum1t.lt.qrmin) goto 70
c.........Intensita' sperimentale
          write(*,*) i1,dum1t,dum2t,dum3t
          if (dum2t.le.0.0) goto 70
          i1=i1+1
          dum1=dum1+dum1t*qinputscale
          dum2=dum2+dum2t*sinputscale
          qles(i1)=dum2t*sinputscale
          dum3=dum3+dum3t*sinputscale
188     continue
        if (i1.eq.0) goto 100
c test fatto il 1 agosto 2011
c se fai la media su tanti punti e' meglio prendere come errore il valore medio degli errori.
        dum3=dum3/i1
c        if (i1.gt.1) call st_d1(i1,qles,dum22,dum3,dum23)
        dum1=dum1/i1
        dum2=dum2/i1
        if (dum2.lt.0.d0) goto 70
        if (dum1.le.qrmax.and.dum1.ge.qrmin) nqr=nqr+1
        nq=nq+1
c        write(*,*) qrmax,nqr,dum1,i1,nq
        if (nq.eq.1) ss=dum2
        if (nq.gt.mn) stop 'nq > mn'
        qq(nq)=dum1
        dum2=abs(dum2)
c.......errore sull'intensita' sperimentale
        if (errk.ne.0.0) dum3=dum2**alphapower*errk
c
c        errk=0.000159031598210947d0
c        dum3=errk*abs(dum2)**0.3d0


        qid(nq)=dum3
c.......Campionamento su una distribuzione normale
        if (jmouve.eq.1) then
951       call normal(dum2,dum3,1.d0,dum2t,iseed)
          if (dum2t.le.0.d0) goto 951
          dum2=dum2t
        endif
        qie(nq)=dum2
        qle(nq)=dlog(dum2)
c.......funzione peso
        wi(nq)=1.d0/dum3
c.......calcolo denominatore chi 2

        dum4=wi(nq)
        chifat=chifat+dum4*dum4
      goto 71
100   close(unit=3)      
      chifat=1.d0/nqr
c      chifat=1.d0
c.....Ordina qie e qle e wi
c      ora no!!
      goto 921
      do 920 i=1,nq-1
        do 920 j=1+i,nq
          dum1=qq(i)
          dum2=qq(j)
          if (dum2.lt.dum1) then
            qq(i)=dum2
            qq(j)=dum1
            dum1=qie(i)
            qie(i)=qie(j)
            qie(j)=dum1
            dum1=qid(i)
            qid(i)=qid(j)
            qid(j)=dum1
            dum1=wi(i)
            wi(i)=wi(j)
            wi(j)=dum1
          endif
920   continue
 921  qmin=qq(1)
      qmax=qq(nq)
      scale=qie(1)
      if (scale.eq.0.d0) scale=1.d0
      do 110 iq=1,nq
        qit=qie(iq)/scale
        if (qit.ne.0.d0) then
          qlt=dlog(qit)
        else
          qlt=-20.d0
        endif
        qies(iq)=qit
        qles(iq)=qlt
        write(*,4200) qq(iq),qie(iq),qid(iq),wi(iq)
110   continue
      return
4201  format(a30)
4200  format(10(e15.7))
      end      


      subroutine gnufit(m)

      include 'quafit2014.cmn'
      character nnn*2


      open (unit=iu_gnu,status='unknown',file='qua'//code//'fit.gnu')
      write(iu_gnu,'(a)') 'reset'
      do i=m,0,-1

      write(hc3,1100) i
      if (hc3(1:1).eq.' ') hc3(1:1)='0'
      if (hc3(2:2).eq.' ') hc3(2:2)='0'
      if (hc3.eq.'000') hc3='inp'

      do nn=1,nexper

      write(nnn,'(i2)') nn
      if (nnn(1:1).eq.' ') nnn(1:1)='0'

      write(iu_gnu,'(a)') 'set title "analysis #'//code//'"'
      write(iu_gnu,'(a)') 'set xlabel "Q (Angs.^-1)"'
      write(iu_gnu,'(a)') 'set ylabel "I(Q)"'
      write(iu_gnu,'(a,g15.3,a,g15.3,a)')
     >'set log y; plot ''qua'//code//hc3//'_'//nnn//
     >'.fit'' u 1:4 w l,'''' u 1:2:($2*10**(-$3/$2/log(10)))'
     >//':($2*10**($3/$2/log(10))) notit  w e; pause -1'
c      write(iu_gnu,'(a,g15.3,a,g15.3,a)')
c     >'set log y; plot ''qua'//code//hc3//'_'//nnn//
c     >'.fit'' u 1:4 w l,'''' u 1:2:($2*10**(-$3/$2/log(10)))'
c     >//':($2*10**($3/$2/log(10))) notit  w e,'
c     >//' ''qua'//code//hc3//'_'//nnn//
c     >'.iq'' u 1:($2*',qipar(nn),'+',plate(nn),') w l; pause -1'
      write(iu_gnu,'(a)') 'set xlabel "Q (Angs.^-1)"'
      write(iu_gnu,'(a)') 'set ylabel "P(Q)"'
      write(iu_gnu,'(a)')
     >'set log y; plot ''qua'//code//hc3//'_'//nnn//
     >'.iq'' u 1:2 w l; pause -1'
      write(iu_gnu,'(a)') 'set xlabel "r (Angs.)"'
      write(iu_gnu,'(a)') 'set ylabel "p(r)"'
      write(iu_gnu,'(a)')
     >'set nolog y; plot [][]''qua'//code//hc3//'_'//nnn//
     >'.pr'' u 1:2 w l; pause -1'

      enddo
      enddo
      close(unit=iu_gnu)
      return
1100  format(i3)
      end


      subroutine fullpdb(ipdb)

      include 'quafit2014.cmn'

      character card*6,atype*3,rtype*3,btype*4,ctype*2,stype*2,rtype0*3
      complex *16 cum1,cum2,cum3,fanom(mxwave),cum4
      dimension ia_n(2),ia_ca(2),ia_c(2),ia_o(2)
      dimension immarr(mxlink),x(7)
      dimension xl(7),xh(7),g(7)
      external aallmin

      aa=dfcc*dsqrt(2.d0)
      rscale=aa/dsqrt(2.d0)/2
      vol=30.!! per ora lo lascio cosi'       vol=rgpdb(ipnt,ipdb,5)! volume molecolare del solvente 30.d0!pi4*(dfcc/2)**3/3 *3.2

      j1=lnfil(fsub(ipdb),300)
      if (j1.eq.0) stop 'j1.eq.0'
      write(*,*) fsub(ipdb)(1:j1)
      rtype0='   '
      namm=0
      open (unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:j1)//
     >'.pdb')
c      open (unit=1,status='unknown',file=fpdb(1:j1)//
c     >'.mp')

      dmax(ipdb)=0.d0
      iaamm=0 !conta i residui occupati nei linkers
      m=naallpdb(ipdb)
      do imaa=1,m
        immarr(imaa)=0
      enddo
      nato=0
      nato_ca=0
      nchainaa=0
      nchainna=0
      natoa=0
      do ia=1,na
        do ib=1,nb
          travdw(ipdb,ia,ib)=0.d0
        enddo
      enddo
      sum1=0.d0
      sum2=0.d0
      sum3=0.d0
      rrmm=0.d0
      w=0.d0
      ial=0
      iar=0

      ch2='%%'
99    read(iu_pdb,'(a200)',end=10) text
c        write(*,*) text
        if (ihetatm.ne.0) then
          if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 99
        else
          if (text(1:4).ne.'ATOM') goto 99
        endif
        read(unit=text,fmt=2000,end=10) card,ina,atype,ctype,rtype
     >                          ,stype,irr,btype,dum1,dum2,dum3 !ina numero atomo, irr numero AA
        if (atype(2:3).eq.'O ') atype(2:3)='OX'
        if (atype(2:2).eq.'H'.or.atype(1:1).eq.'H') goto 99 ! scarto gli H
        if (atype(3:3).eq.'H') then
          if (ichar(atype(2:2)).ge.48.and.ichar(atype(2:2)).le.57) then
            goto 99
          else if (ichar(atype(1:1)).ge.48.and.ichar(atype(2:2)).le.57.
     >             and.atype(2:2).eq.' ') then 
            goto 99
          endif
        endif
     
c... sostituzione mse con met
c.......MSE
c        if (card.eq.'HETATM'.and.rtype.eq.'MSE') then
c           if (atype.eq.'SE ') atype=' SD'
c           card='ATOM  '
c           rtype='MET'
c        endif
c... sostituzione ocs con cys

c.......OCS
c        if (card.eq.'HETATM'.and.rtype.eq.'OCS') then
c           if (atype(1:2).eq.'OD') goto 99  ! salta OD1, OD2, OD3
c           card='ATOM  '
c           rtype='CYS'
c        endif

        if (text(1:4).eq.'ATOM'.or.
     >(text(1:6).eq.'HETATM'.and.rtype.eq.'CSZ'.and.atype.ne.'SE ').or.
     >(text(1:6).eq.'HETATM'.and.rtype.eq.'MSE'.and.atype.ne.'SE ')
     > ) then      
          iprot=1
          do irtype=1,22
            if (rtype.eq.amm(irtype)) goto 98
          enddo
c....1 nucleic acid
          iprot=0
          inucl=1
          do irtype=1,8
            if (rtype.eq.nnaac(irtype)) goto 98
c          write(*,*) text
          enddo
c....2 nucleic acid
          goto 99
 98     if (ch2.ne.stype) then
          if (iprot.eq.1) then
          nchainaa=nchainaa+1
          if (nchainaa.gt.mxchain) stop 'nchainaa > mxchain'
          else if (inucl.eq.1) then
          nchainna=nchainna+1
          if (nchainna.gt.mxchain) stop 'nchainna > mxchain'
          endif
          ch2=stype
        endif
        if (iprot.eq.1) then
          if (rtype.ne.rtype0) then
            rtype0=rtype
            namm=namm+1
          endif
          do iatype=1,21
            if (atype.eq.atypei(iatype)) goto 97
          enddo
          do iatype=1,21
            if (atype(1:2).eq.atypei(iatype)(1:2)) goto 97
          enddo
          if (iatype.eq.22) iatype=9              ! un ossigeno
 97       itype=itypei(irtype,iatype)
        else if (inucl.eq.1) then
          do iatype=1,28
            do kk=1,2
            if (atype(2:3)//ctype(1:1).eq.anapei(iatype,kk)) goto 977
            enddo
          enddo
          if (iatype.eq.29) iatype=2 ! un ossigeno
 977      itype=inapei(irtype,iatype)
        endif

        if (itype.eq.0) then 
          do kk=0,1
          do itype=1,23!19
            if (atype(1:3-kk).eq.atypej(itype)(1:3-kk)) goto 799
          enddo
             write(*,*) ' itype=0 ',card,atype,rtype,stype,irr
     >                    ,  dum1,dum2,dum3

          enddo

             itype=9
          endif
        else   ! HETATM
          iatype=0
          call nospace(atype,ch10(1:3),3)
          j10=lnfil(ch10(1:3),3)
          do itype=1,23!19
            call nospace(atypej(itype),ch3,3)
            j3=lnfil(ch3,3)
            if (ch10(1:j10).eq.ch3(1:j10)) goto 997
          enddo
          do kk=0,1
          do itype=1,23!19
            if (atype(1:3-kk).eq.atypej(itype)(1:3-kk)) goto 997
          enddo
          enddo
          itype=9              ! un ossigeno
 997      continue
c          write(*,*) atype,ctype,rtype,stype,itype
             write(*,*) card,atype,rtype,stype,irr
     >                    ,  dum1,dum2,dum3
        endif

 799    nato=nato+1
        if (nato.gt.mxrun) stop 'nato > mxrun'
        iato(nato)=itype
        if (iprot.eq.1.and.irtype.le.22) then
          iammch(nato,1)=irtype
          iammch(nato,2)=nchainaa
          inacch(nato,1)=0
          inacch(nato,2)=0
          if (namm.eq.1) then
            if (iatype.eq.1)  ia_n(1)=nato
            if (iatype.eq.2) ia_ca(1)=nato
            if (iatype.eq.3)  ia_c(1)=nato
            if (iatype.eq.21) ia_o(1)=nato
          endif
          if (iatype.eq.1)  ia_n(2)=nato
          if (iatype.eq.2) ia_ca(2)=nato
          if (iatype.eq.3)  ia_c(2)=nato
          if (iatype.eq.21) ia_o(2)=nato
c.........2015
          if (iatype.eq.2) then !C alpha
            nato_ca=nato_ca+1
            inato_ca(nato_ca)=nato
          endif
c............          
        else if (inucl.eq.1.and.irtype.le.8) then
          inacch(nato,1)=irtype
          inacch(nato,2)=nchainna
          iammch(nato,1)=0
          iammch(nato,2)=0
        else
          iammch(nato,1)=0
          iammch(nato,2)=0
        endif
        afac(nato)=1.0
c        write(14,*) nato,atype,' ',rtype,' ',itype,' ',atypej(itype)
c        write(iu_bto,*) nato, itype,(vexc(itype)*3/pi4)**(1./3),
c     >+rexc(itype),atypej(itype)
        rrmm=rrmm+rexc(itype)**3
        do ia=1,natgr(itype)
          w=w+rmass(izgr(itype,ia))
        enddo
        if (iprot.eq.1.and.iatype.eq.1.and.itype.eq.6) 
     >afac(nato)=1.0-dnexch !NH group in the main chain

        itest=0
        if (iammch(nato,1).ne.0) then
          if (iato(nato).eq.2.or.
     >(iammch(nato,1).eq.8.and.iato(nato).eq.3)) itest=1 !!! perche' 2? significa CA per proteine!!!
        else if (inacch(nato,1).ne.0) then
          itest=1!tutti    if (iato(nato).eq.13.or.iato(nato).le.4) itest=1 !!! o il P o un qualunque C
        endif
        if (itest.eq.1) natoa=natoa+1 !vedi shell1

79      sum1=sum1+dum1
        sum2=sum2+dum2
        sum3=sum3+dum3
        coord(nato,1)=dum1
        coord(nato,2)=dum2
        coord(nato,3)=dum3

        if (iprot.eq.1) then
c.......nuova parte per linkers
        m=naallpdb(ipdb)
        do imaa=1,m
c          write(*,*) imaa,irr,iaallpdb(ipdb,imaa,1)
          if (iaallpdb(ipdb,imaa,1).eq.irr) then
            if (immarr(imaa).eq.0) then
              iaamm=iaamm+1
              immarr(imaa)=1
            endif
            if (iatype.eq.1) then
              cooaall_n(ipdb,imaa,1)=dum1
              cooaall_n(ipdb,imaa,2)=dum2
              cooaall_n(ipdb,imaa,3)=dum3
c              write(*,*) ' n ',imaa,iaamm,dum1,dum2,dum3
            else if (iatype.eq.2) then
              cooaall_ca(ipdb,imaa,1)=dum1
              cooaall_ca(ipdb,imaa,2)=dum2
              cooaall_ca(ipdb,imaa,3)=dum3
c              write(*,*) ' ca ',imaa,iaamm,dum1,dum2,dum3
            else if (iatype.eq.3) then
              cooaall_c(ipdb,imaa,1)=dum1
              cooaall_c(ipdb,imaa,2)=dum2
              cooaall_c(ipdb,imaa,3)=dum3
c              write(*,*) ' c ',imaa,iaamm,dum1,dum2,dum3
            else if (iatype.eq.21) then
              cooaall_o(ipdb,imaa,1)=dum1
              cooaall_o(ipdb,imaa,2)=dum2
              cooaall_o(ipdb,imaa,3)=dum3
c              write(*,*) ' o ',imaa,iaamm,dum1,dum2,dum3
            endif
          endif
        enddo !imaa
c.......fine nuova parte per linkers
        endif !fine iprot=1



        if (iprot.eq.1) then
c.......nuova parte per constraints
        do istate=1,nstate
c.......a
        m=nconstr_a(istate)
        do nc=1,m
        i2=iconstr_a(istate,nc,2)
        do isite=1,nsite(istate)
        i1=ipdbsite(istate,isite)
        if (i1.eq.ipdb.and.i2.eq.isite) then
          i3=iconstr_a(istate,nc,1)
          i4=iconstr_a(istate,nc,5)
          if (i3.eq.irr.and.i4.eq.iatype) then
            kk=isitlnkconstr_a(istate,isite,1)
            kk=kk+1
            isitlnkconstr_a(istate,isite,1+kk)=nc
            isitlnkconstr_a(istate,isite,1)=kk
            cooconstr_a(istate,nc,1)=dum1
            cooconstr_a(istate,nc,2)=dum2
            cooconstr_a(istate,nc,3)=dum3
          endif
        endif
        enddo !isite
        enddo !nc
c.......b
        m=nconstr_b(istate)
        do nc=1,m
        i2i=iconstr_b(istate,nc,2)
        i2j=iconstr_b(istate,nc,7)
        do isite=1,nsite(istate)
        i1=ipdbsite(istate,isite)
        if (i1.eq.ipdb.and.i2i.eq.isite) then
          i3=iconstr_b(istate,nc,1)
          i4=iconstr_b(istate,nc,5)
          if (i3.eq.irr.and.i4.eq.iatype) then
            kk=isitlnkconstr_b(istate,isite,1)
            kk=kk+1
            isitlnkconstr_b(istate,isite,1+kk)=nc
            isitlnkconstr_b(istate,isite,1)=kk
            iconstr_b(istate,nc,16)=kk
            isitlnkconstr_b(istate,isite,1+mxconstr+kk)=1
            cooconstr_b(istate,nc,1,1)=dum1
            cooconstr_b(istate,nc,1,2)=dum2
            cooconstr_b(istate,nc,1,3)=dum3
          endif
        endif
        if (i1.eq.ipdb.and.i2j.eq.isite) then
          i3=iconstr_b(istate,nc,6)
          i4=iconstr_b(istate,nc,10)
          if (i3.eq.irr.and.i4.eq.iatype) then
            kk=isitlnkconstr_b(istate,isite,1)
            kk=kk+1
            isitlnkconstr_b(istate,isite,1+kk)=nc
            isitlnkconstr_b(istate,isite,1)=kk
            iconstr_b(istate,nc,17)=kk
            isitlnkconstr_b(istate,isite,1+mxconstr+kk)=2
            cooconstr_b(istate,nc,2,1)=dum1
            cooconstr_b(istate,nc,2,2)=dum2
            cooconstr_b(istate,nc,2,3)=dum3
          endif
        endif
        enddo !isite
        enddo !nc

        enddo !istate
c.......fine nuova parte per constraints
        endif !fine iprot=1



c        if (ina.eq.ialeft(ipdb))  ial=nato
c        if (ina.eq.iaright(ipdb)) iar=nato
        goto 99
10    close(unit=iu_pdb)

c      if (iar.eq.0.or.ial.eq.0) stop 'iar ial = 0'
      if (nato.eq.0) return


c.....If exists, the file of selective aminoacid deuteration is read
      do i=1,22
        do j=1,nchainaa
          fdeutaa(i,j)=0.d0
        enddo
      enddo
      do i=1,8
        do j=1,nchainna
          fdeutna(i,j)=0.d0
        enddo
      enddo
      open (unit=5,status='unknown',file=fsub(ipdb)(1:j1)//
     >'.csv')
      do i=1,22
        read(5,'(3x,100(1x,f4.2))',end=123,err=123) 
     >                  (fdeutaa(i,j),j=1,nchainaa)
      enddo
      do i=1,8
        read(5,'(3x,100(1x,f4.2))',end=123,err=123) 
     >                  (fdeutna(i,j),j=1,nchainna)
      enddo
 123  close(5)

      write(*,2208) 
     >' Excluded volume (Angs.^3) ............:',rrmm*pi4/3

      rrmm=(rrmm/nato)**(1./3)
      rmfull(ipdb)=rrmm
      wfull(ipdb)=w
      write(*,2208) 
     >' Molecular weight (g/mol) .............:',w
      write(*,2208) 
     >' Number of atoms ......................:',float(nato)
      write(*,2208) 
     >' Number of residues (C_alpha atoms) ...:',float(nato_ca)
c      write(8,2208) 
c     >' Number of atoms ......................:',float(nato)
      write(*,2208) 
     >' Average atomic radius (Angs.) ........:',rrmm
c      write(8,2208) 
c     >' Average atomic radius (Angs.) ........:',rrmm
c      pause
c.....Trasla in modo che il centro coincida con il c.geom.
      dum1=1.d0/dfloat(nato)
      sum1=sum1*dum1
      sum2=sum2*dum1
      sum3=sum3*dum1

      call cpolar(sum1,sum2,sum3,r,alpha,beta)
      string='    Geom. Center Vect.: modulus (ang.) :'
      write(*,2207) string,r,r*0.8,r*1.2,r*0.2,1
      string='                        alpha   (deg) .:'
      write(*,2207) string,alpha*conv,0.0,360.0,20.0,1
      string='                        cos(beta) .....:'
      write(*,2207) string,cos(beta),-1.0,1.0,0.5,1

      write(*,2208) 
     >' Average atomic radius (Angs.) ........:',rrmm

      r=1.d10
      do i=1,nato
        x1=coord(i,1)-sum1
        y1=coord(i,2)-sum2
        z1=coord(i,3)-sum3
        s=x1*x1+y1*y1+z1*z1
        r=min(r,s)
      enddo
c      if (r.gt.10**2) then
c        write(*,*) '  r = ',dsqrt(r)
c        write(*,*) '  Center Atom #'
c        j=nato/2
c        sum1=coord(j,1)
c        sum2=coord(j,2)
c        sum3=coord(j,3)
c      endif


c.....Trasla in modo che il centro di massa della subunita' coincida
      do 30 i=1,nato
        x1=coord(i,1)-sum1
        y1=coord(i,2)-sum2
        z1=coord(i,3)-sum3
        coord(i,1)=x1
        coord(i,2)=y1
        coord(i,3)=z1
        r=x1*x1+y1*y1+z1*z1
        vdw=rvdw(iato(i))
        dum1=-r+vdw*vdw
        call cpolar(x1,y1,z1,r,alpha,beta)
        do 31 ib=1,nb
          cb1=cb(ib)
          sb1=sfromc(cb1)
          do 31 ia=1,na          
            alph1=al(ia)
            ca1=dcos(alph1)
            sa1=dsin(alph1)
            dum2=z1*cb1+sb1*(x1*ca1+y1*sa1)
            delta=dum2*dum2+dum1
            if (delta.ge.0.d0) then
              delta=dsqrt(delta)
              dum3=dum2+delta
              dum4=dum2-delta
              dum4=max(dum3,dum4)
              if (dum4.gt.0.0) then
                travdw(ipdb,ia,ib)=
     >max(travdw(ipdb,ia,ib),dum4)
              endif
            endif
31      continue
30    continue

c.....Memorizza i vettori che danno le posizione dei siti left e right
c.....secondo il frame non ruotato e centrato nel centro di massa
c.....della subuinita' isub

c      xlref(ipdb)=coord(ial,1) !N terminale
c      ylref(ipdb)=coord(ial,2)
c      zlref(ipdb)=coord(ial,3)

c      xrref(ipdb)=coord(iar,1) !C terminale
c      yrref(ipdb)=coord(iar,2)
c      zrref(ipdb)=coord(iar,3)

c.....nuovo
c      ial=ia_n(1)
c      xlref(ipdb)=coord(ial,1) !N terminale
c      ylref(ipdb)=coord(ial,2)
c      zlref(ipdb)=coord(ial,3)
c      i1=ia_ca(2)
c      i2=ia_c(2)
c      x1=coord(i1,1)-coord(i2,1)
c      y1=coord(i1,2)-coord(i2,2)
c      z1=coord(i1,3)-coord(i2,3)
c      i1=ia_o(2)
c      x2=coord(i1,1)-coord(i2,1)
c      y2=coord(i1,2)-coord(i2,2)
c      z2=coord(i1,3)-coord(i2,3)
c      call cpolar(x1,y1,z1,r1,a1,b1)
c      call cpolar(x2,y2,z2,r2,a2,b2)
c      r=1.32!! distanza   http://schubert.bio.uniroma1.it/corsostruttura/capitoli/capitolo1/testo1.htm
c      dum1=-r/dsqrt((x1/r1+x2/r2)**2+(y1/r1+y2/r2)**2+(z1/r1+z2/r2)**2)
c      xrref(ipdb)=coord(i2,1)+dum1*(x1/r1+x2/r2) !N del successivo
c      yrref(ipdb)=coord(i2,2)+dum1*(y1/r1+y2/r2) 
c      zrref(ipdb)=coord(i2,3)+dum1*(z1/r1+z2/r2) 
cc      write(101,*) 'a ',ia_n(1),ia_c(1),ia_ca(1),ia_o(1)
cc      write(101,*) 'b ',ia_n(2),ia_c(2),ia_ca(2),ia_o(2)
cc      write(101,*) 'C ',(coord(ia_c(2),k),k=1,3)
cc      write(101,*) 'CA ',(coord(ia_ca(2),k),k=1,3)
cc      write(101,*) 'O ',(coord(ia_o(2),k),k=1,3)
cc      write(101,*) 'X ',xrref(ipdb),yrref(ipdb),zrref(ipdb)

      do ib=1,nb
        do ia=1,na
          if (travdw(ipdb,ia,ib).le.0.d0) 
     >travdw(ipdb,ia,ib)=1.d0
          dum1=travdw(ipdb,ia,ib)
          dmax(ipdb)=max(dum1,dmax(ipdb))
          rtramax(ipdb)=max(rtramax(ipdb),dum1)
          rtramin(ipdb)=min(rtramin(ipdb),dum1)
        enddo
      enddo

      rmax2=dmax(ipdb)*2.5

      call atovlpdb(ipdb) !! memorizza coordinate calpha per prot o tutte per acidi nucleici

      if (iprot.eq.1) then

c.....ancora linker
c.....Memorizza i vettori che danno le posizioni degli atomi N CA C e O degli estremi dei linker
c.....dopo averli riposizionati in modo che distanze e angoli di legame siano quelli canonici.
      if (iaamm.ne.naallpdb(ipdb)) stop 'naallpdb(ipdb)'
      m=naallpdb(ipdb)
      open (unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:j1)//
     >  '_aall_.pdb')
      do imaa=1,m


      cooaall_n(ipdb,imaa,1)=cooaall_n(ipdb,imaa,1)-sum1
      cooaall_n(ipdb,imaa,2)=cooaall_n(ipdb,imaa,2)-sum2
      cooaall_n(ipdb,imaa,3)=cooaall_n(ipdb,imaa,3)-sum3
      cooaall_c(ipdb,imaa,1)=cooaall_c(ipdb,imaa,1)-sum1
      cooaall_c(ipdb,imaa,2)=cooaall_c(ipdb,imaa,2)-sum2
      cooaall_c(ipdb,imaa,3)=cooaall_c(ipdb,imaa,3)-sum3
      cooaall_o(ipdb,imaa,1)=cooaall_o(ipdb,imaa,1)-sum1
      cooaall_o(ipdb,imaa,2)=cooaall_o(ipdb,imaa,2)-sum2
      cooaall_o(ipdb,imaa,3)=cooaall_o(ipdb,imaa,3)-sum3
      cooaall_ca(ipdb,imaa,1)=cooaall_ca(ipdb,imaa,1)-sum1
      cooaall_ca(ipdb,imaa,2)=cooaall_ca(ipdb,imaa,2)-sum2
      cooaall_ca(ipdb,imaa,3)=cooaall_ca(ipdb,imaa,3)-sum3

      goto 5050 !! attenzione perche' se commenti questo, e quindi vai avanti, gli angoli psi e phi potrebbero non essere gli stessi

c.......
      rscale=rmax2
c.....Calpha 
      p1(1)=0.d0      
      p1(2)=0.d0      
      p1(3)=0.d0      
      do kk=1,3
        pp1(kk)=cooaall_ca(ipdb,imaa,kk)
      enddo
c.....N
      p2(1)=0.d0      
      p2(2)=0.d0      
      p2(3)=-rnca
      do kk=1,3
        pp2(kk)=cooaall_n(ipdb,imaa,kk)
      enddo
c.....C
      p3(1)=rcca*cos(ancac-pih)      
      p3(2)=0.d0      
      p3(3)=rcca*sin(ancac-pih)      
      do kk=1,3
        pp3(kk)=cooaall_c(ipdb,imaa,kk)
      enddo
c.....O
      call bonds(rco,acaco,pih,p3,p1,p2,p4)
      do kk=1,3
        pp4(kk)=cooaall_o(ipdb,imaa,kk)
      enddo

      npar=4+3
      do ii=1,4
        x(ii)=0.d0
      enddo
      x(5)=xscinv(cooaall_ca(ipdb,imaa,1),-rscale,rscale)
      x(6)=xscinv(cooaall_ca(ipdb,imaa,2),-rscale,rscale)
      x(7)=xscinv(cooaall_ca(ipdb,imaa,3),-rscale,rscale)
c      call monk(aallmin,x,f,npar,1000,iseed)
      do ii=1,npar
        xl(ii)=-pih
        xh(ii)=pih
        g(ii)=pih/3!0.1
      enddo
      ncicl=5
      nsub=20
      ti=100.d0
      tf=1.e-3
      call sian(aallmin,npar,x,xl,xh,g,0.5d0,f,ti,tf,ncicl,nsub,
     >       tm,am,iseed)
      call simplx(aallmin,x,f,npar,1.d-30,itc,30000)
      call aallmin(npar,x,f)
      a=xscale(x(1),0.d0,pi2)
      cbb=xscale(x(2),-one,one)
      g1=xscale(x(3),0.d0,pi2)
      p=xscale(x(4),0.d0,pi2)
      xt=xscale(x(5),-rscale,rscale)
      yt=xscale(x(6),-rscale,rscale)
      zt=xscale(x(7),-rscale,rscale)
            write(*,2208) 
     >' Euler Rotational Angles (deg): alpha  :',a*conv
            write(*,2208) 
     >'                                beta   :',dacos(cbb)*conv
            write(*,2208) 
     >'                                gamma  :',g1*conv
            write(*,2208) 
     >'                                phi    :',p*conv
            write(*,2208) 
     >' Translation coordinates (Angs.):     X:',xt
            write(*,2208) 
     >'                                      Y:',yt
            write(*,2208) 
     >'                                      Z:',zt
            write(*,2208) 
     >' Chi square                            :',f
cc      if (imaa.eq.1) then
c.......
        write(iu_pdb,1005) 'ATOM  ',8*imaa-7,' N  ','GLY',
     >imaa,
     >cooaall_n(ipdb,imaa,1),
     >cooaall_n(ipdb,imaa,2),
     >cooaall_n(ipdb,imaa,3)
        write(iu_pdb,1005) 'ATOM  ',8*imaa-6,' CA ','GLY',
     >imaa,
     >cooaall_ca(ipdb,imaa,1),
     >cooaall_ca(ipdb,imaa,2),
     >cooaall_ca(ipdb,imaa,3)
        write(iu_pdb,1005) 'ATOM  ',8*imaa-5,' C  ','GLY',
     >imaa,
     >cooaall_c(ipdb,imaa,1),
     >cooaall_c(ipdb,imaa,2),
     >cooaall_c(ipdb,imaa,3)
        write(iu_pdb,1005) 'ATOM  ',8*imaa-4,' O  ','GLY',
     >imaa,
     >cooaall_o(ipdb,imaa,1),
     >cooaall_o(ipdb,imaa,2),
     >cooaall_o(ipdb,imaa,3)
 
    
        imbb=m+imaa
        write(iu_pdb,1005) 'ATOM  ',8*imaa-3,' N  ','GLY',
     >imbb,(ppp2(kk),kk=1,3)
        write(iu_pdb,1005) 'ATOM  ',8*imaa-2,' CA ','GLY',
     >imbb,(ppp1(kk),kk=1,3)
        write(iu_pdb,1005) 'ATOM  ',8*imaa-1,' C  ','GLY',
     >imbb,(ppp3(kk),kk=1,3)
        write(iu_pdb,1005) 'ATOM  ',8*imaa-0,' O  ','GLY',
     >imbb,(ppp4(kk),kk=1,3)
cc      endif


      do kk=1,3
        cooaall_ca(ipdb,imaa,kk)=ppp1(kk)
        cooaall_n(ipdb,imaa,kk)=ppp2(kk)
        cooaall_c(ipdb,imaa,kk)=ppp3(kk)
        cooaall_o(ipdb,imaa,kk)=ppp4(kk)
      enddo

 5050 continue

      enddo !imaa


      close(iu_pdb)

      rscale=aa/dsqrt(2.d0)/2
c      stop

c.....fine ancora linker
      endif !iprot=1


c.....nuova parte per constraints
      do istate=1,nstate
        do isite=1,nsite(istate)
          i1=ipdbsite(istate,isite)
          if (i1.eq.ipdb) then
c...........a
            n=isitlnkconstr_a(istate,isite,1)
            do kk=1,n
              nc=isitlnkconstr_a(istate,isite,1+kk)
              cooconstr_a(istate,nc,1)=cooconstr_a(istate,nc,1)-sum1
              cooconstr_a(istate,nc,2)=cooconstr_a(istate,nc,2)-sum2
              cooconstr_a(istate,nc,3)=cooconstr_a(istate,nc,3)-sum3
            enddo
c...........b
            n=isitlnkconstr_b(istate,isite,1)
            do kk=1,n
              nc=isitlnkconstr_b(istate,isite,1+kk)
              ie=isitlnkconstr_b(istate,isite,1+mxconstr+kk)
          cooconstr_b(istate,nc,ie,1)=cooconstr_b(istate,nc,ie,1)-sum1
          cooconstr_b(istate,nc,ie,2)=cooconstr_b(istate,nc,ie,2)-sum2
          cooconstr_b(istate,nc,ie,3)=cooconstr_b(istate,nc,ie,3)-sum3
            enddo
          endif
        enddo !isite
      enddo !istate

      qmin=0
      dq=((qmfull)-(qmin))/float(nqfull-1)
      q=(qmin)-dq
      do iq=1,nqfull
        q=q+dq
        qqfull(iq)=(q)
      enddo

      if (ifilm.eq.0) then
      open (unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:j1)//
     >'.sha2')
c.......old
        do iq=1,nqfull
          call reasin_first(ipdb,iq)
c.......fine old

c.....new
c      j1=lnfil(fsub(ipdb),300)
c      open(unit=iu_sin,status='unknown',file=fsub(ipdb)(1:j1)//
c     >'.ilm')
c      do iq=1,nqfull
c        read(unit=iu_sin,fmt=1007) 
c     >dum1,(((alm(l,m,is),m=-l,l),l=0,lfirst),is=1,nwave+nsfull+8)
c.....fine new

          q=qqfull(iq)

        write(iu_pdb,1007) q,(((alm(l,m,is),m=-l,l),
     >         l=0,lfirst),is=1+nwave+6,1+nwave+6)


        enddo
      close(unit=iu_pdb)
c.....new
c      close(unit=iu_sin)
c.....fine new
        return
      endif

      if (nsfull.gt.0.and.ifspdb.eq.1) then
        call shell1(rmax2)
        do is=1,nsfull
          write(unit=ch1,fmt='(i1)') is
          open (unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:j1)//
     >  '_shell_'//ch1//'.pdb')
            i88=0
            do i=1+nato,natob
              if (iato(i).eq.is) then
                i88=i88+1
                write(iu_pdb,1001) 'HETATM',i,'  O   HOH '
     >,'A','   1    ',coord(i,1)+sum1,coord(i,2)+sum2,coord(i,3)+sum3
              endif
            enddo
          close(unit=iu_pdb)
          write(*,2208) 
     >' Water molecules in the shell # '//ch1//' .....:',float(i88)
c          write(8,2208) 
c     >' Water molecules in the shell # '//ch1//' .....:',float(i88)
          write(*,2208) 
     >' Volume of the shell # '//ch1//' (Ang.^3) .....:',float(i88)*vol
c          write(8,2208) 
c     >' Volume of the shell # '//ch1//' (Ang.^3) .....:',float(i88)*vol
        enddo
      else if (nsfull.gt.0) then
        natob=nato
        do is=1,nsfull
          write(unit=ch1,fmt='(i1)') is
          open (unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:j1)//
     >'_shell_'//ch1//'.pdb')
 88         read(iu_pdb,'(a200)',end=108) text
            read(unit=text,fmt=2001,end=108) card,atype,ctype,rtype,irr,
     >                                  btype,dum1,dum2,dum3
            natob=natob+1
            coord(natob,1)=dum1
            coord(natob,2)=dum2
            coord(natob,3)=dum3
c            write(*,*) natob,dum1,dum2,dum3
            iato(natob)=is
            goto 88
 108      close(unit=iu_pdb)
        enddo
      endif

c. ...shell
c      vol=pi4*(dfcc/2)**3/3
c      fat=vol**(2.d0/3)/pi4

      open (unit=iu_sin,status='unknown',file=fsub(ipdb)(1:j1)//
     >'.ilm')

      lstep=1
      lstart=0  
        do iq=1,nqfull
          call wprc(iq,1,nqfull,lstep,lstart)
          q=qqfull(iq)

c.........pensala da qui
          do l=0,lfirst
            do m=-l,l
              do is=1,nsfull+nwave+8 !new
                alm(l,m,is)=dcmplx(zer,zer)
              enddo
            enddo
          enddo


      do i=1,nato
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        call cpolar(x1,y1,z1,r,al1,b1)
        cb1=dcos(b1)
        call py_go(lfirst,b1)
        call cscsa(lfirst,al1)
        i1=iato(i)

        fdeu=0.d0
        if (iammch(i,1).ne.0) then
          fdeu=fdeutaa(iammch(i,1),iammch(i,2))
        else if (inacch(i,1).ne.0) then
          fdeu=fdeutna(inacch(i,1),inacch(i,2))
        endif
  

          do lw=1,nwave
            call sfanom(i1,wave(lw),fanom(lw))
          enddo


c  Rep. Prog. Phys. 67 (2004) 1073
c  Unique aspects of neutron scattering for the study of biological systems
c  Heinrich B Stuhrmann
c
c   1H: b(H) = [-0.3742±1.456P(H)] 10-12 cm, (7.1)
c   2H: b(D) = [+0.6671±0.270P(D)] 10-12 cm, (7.2)

c J. Appl. Cryst. (1989). 22, 352-362
c Polarized Neutron Scattering by Polarized Protons of Bovine Serum Albumin
c in Deuterated Solvent
c BY W. KNOP, H.-J. SCHINK, H. B. STUHRMANN,* R. WAGNER AND M. WENKOW-Es-SOUNI
c p(H20) = [-0.56±9.66P(H)] 
c p(D20) = [+6.40±1.50P(D)]


          bneu1= sclen(i1)+fdeu*(hgrou(i1)-hexg(i1))*(0.6671+0.3742)
          bneu2= hexg(i1)*afac(i)*(0.6671+0.3742)
          bneu3= hgrou(i1)*1.456*(1-fdeu) !il terzo diviso 1.456 e' An1
          bneu4=-hexg(i1)*afac(i)*1.456
          bneu5= hexg(i1)*afac(i)*0.27  !il quinto diviso 0.27 e' An2. 

          dum1=sfactt(i1)
          dum2=sfacts(i1)

          qu=q*r
          
          do l=0,lfirst
            if (qu.ne.0.d0) then
              call sphbes(l,qu,sj,sy,sjp,syp)
            else
              if (l.eq.0) then
                sj=1.d0
              else
                sj=0.d0
              endif
            endif
            cum1=sj*(dcmplx(0.d0,1.d0))**l

            cum2=cum1*ysf(l,0)

            alm(l,0,1)=alm(l,0,1)+dum1*cum2
            do lw=1,nwave
              alm(l,0,1+lw)  =alm(l,0,1+lw)  +fanom(lw)*cum2
            enddo

            alm(l,0,1+nwave+1)=alm(l,0,1+nwave+1)+bneu1*cum2
            alm(l,0,1+nwave+2)=alm(l,0,1+nwave+2)+bneu2*cum2
            alm(l,0,1+nwave+3)=alm(l,0,1+nwave+3)+bneu3*cum2
            alm(l,0,1+nwave+4)=alm(l,0,1+nwave+4)+bneu4*cum2
            alm(l,0,1+nwave+5)=alm(l,0,1+nwave+5)+bneu5*cum2

            alm(l,0,1+nwave+6)=alm(l,0,1+nwave+6)+dum2 *cum2

            do m=1,l
              cum2=cum1*dcmplx(csa(1,m),-csa(2,m))*ysf(l,m)

              alm(l,m,1)=alm(l,m,1)+dum1*cum2
c              if (iq.eq.2.and.l.eq.1.and.m.eq.1.and.i.eq.7) 
c     >write(80,*) q,alm(l,m,1),dum1,cum2,cum1,
c     >dcmplx(csa(1,m),-csa(2,m)),ysf(l,m),al1,b1,x1,y1,z1
              do lw=1,nwave
                alm(l,m,1+lw)  =alm(l,m,1+lw)  +fanom(lw)*cum2
                alm(l,-m,1+lw)  =alm(l,-m,1+lw)  
     >    +fanom(lw)*(-1)**m*ysf(l,m)*dcmplx(csa(1,m),csa(2,m))*cum1
              enddo

              alm(l,m,1+nwave+1)=alm(l,m,1+nwave+1)+bneu1*cum2
              alm(l,m,1+nwave+2)=alm(l,m,1+nwave+2)+bneu2*cum2
              alm(l,m,1+nwave+3)=alm(l,m,1+nwave+3)+bneu3*cum2
              alm(l,m,1+nwave+4)=alm(l,m,1+nwave+4)+bneu4*cum2
              alm(l,m,1+nwave+5)=alm(l,m,1+nwave+5)+bneu5*cum2

              alm(l,m,1+nwave+6)=alm(l,m,1+nwave+6)+dum2*cum2  !!! in tutte queste definizioni non c'e' il 4*pi ma quando le usi vanno bene. Questo, moltiplicato per 4*pi va benissimo per la shape ma va ridefinito il suo range di q, perche' poi lo integri.
            enddo
          enddo
        enddo

c. ...shell

      do is=1,nsfull
      do 81 i=1+nato,natob
        if (iato(i).ne.is) goto 81
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        call cpolar(x1,y1,z1,r,al1,b1)
        cb1=dcos(b1)
        call py_go(lfirst,b1)
        call cscsa(lfirst,al1)


c            dum1=vol*dexp(-q*q*fat)
          qu=q*r
          
            do l=0,lfirst
              if (qu.ne.0.d0) then
                call sphbes(l,qu,sj,sy,sjp,syp)
              else
                if (l.eq.0) then
                  sj=1.d0
                else
                  sj=0.d0
                endif
              endif
              cum1=sj*(dcmplx(0.d0,1.d0))**l
  
              alm(l,0,is+1+nwave+6)=alm(l,0,is+1+nwave+6)
     >                              +cum1*ysf(l,0)

              do m=1,l
                alm(l,m,is+1+nwave+6)=alm(l,m,is+1+nwave+6)+
     >           cum1*dcmplx(csa(1,m),-csa(2,m))*ysf(l,m)
              enddo
            enddo


 81       continue
        enddo
c.......negative values of m
        do l=0,lfirst
          do m=1,l
            i7=(-1)**(l+m)
            alm(l,-m,1)=i7*dconjg(alm(l,m,1))
            do is=2+nwave,nsfull+nwave+8
              alm(l,-m,is)=i7*dconjg(alm(l,m,is))
            enddo
          enddo
        enddo

c        write(*,1007) q,(((alm(l,m,is),m=0,l),
c     >         l=0,lfirst),is=1,nwave+nsfull+8)
c        write(76,1007) q,(((alm(l,m,is),m=-l,l),
c     >         l=0,lfirst),is=1,1)
        write(iu_sin,1007) q,(((alm(l,m,is),m=-l,l),
     >         l=0,lfirst),is=1,nwave+nsfull+8)

c.......new store
        do l=0,lfirst
        do m=-l,l
        do is=1,nwave+nsfull+8
        almstore(ipdb,iq,l,m,is)=alm(l,m,is)
        enddo
        enddo
        enddo
c.......end of new

      enddo  ! end iq
      close(unit=iu_sin)

      return

2207  format(a40,4(g15.5E3,1x),i4,1x,a10000)
2208  format(a40,3(g12.5,1x),i4)
 1007 format(500000(e15.6E3))
 2001 format(a6,6x,a3,a2,a3,2x,i4,a4,3f8.3)
2000  format(a6,i5,1x,a3,a2,a3,a2,i4,a4,3f8.3)
 1005 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
 1107 format('#',200(e19.7E3))
 1207 format(200(e19.7E3))


1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end


      subroutine ums2(npar,x,f)

      include 'quafit2014.cmn'
c      include 'genfit.cmn'
      dimension x(6)


      a=xscale(x(1),0.d0,pi/4)
      b=xscale(x(2),1.d0/sqrt(2.),one)
      g=xscale(x(3),0.d0,pi/4)
      xt=xscale(x(4),-rscale,rscale)
      yt=xscale(x(5),-rscale,rscale)
      zt=xscale(x(6),-rscale,rscale)
      call mat(a,dacos(b),g,rot)
      f=0.d0
       dum1=((afcc(1,1)*afcc(2,2)-afcc(1,2)*afcc(2,1))*afcc(3,3)-
     >((afcc(1,1)*afcc(2,3)-afcc(1,3)*afcc(2,1))*afcc(3,2)+(afcc(
     >1,3)*afcc(2,2)-afcc(1,2)*afcc(2,3))*afcc(3,1)))

      do i=1,nato
        if (iato(i).eq.2) then
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)

       dum4=1.e10
       do nbase=1,2
         dum2=(nbase-1)*dfcc/3.**0.5
         n1=int(0.5-(((dum2-rx)*afcc(2,2)+(ry-dum2)*afcc(2,1))*afcc(3,3)
     >+((dum2-ry)*afcc(2,3)+(rz-dum2)*afcc(2,2))*afcc(3,1)-((dum2-x
     >3)*afcc(2,3)+(rz-dum2)*afcc(2,1))*afcc(3,2))/dum1 )
         n2=int(0.5+(((dum2-rx)*afcc(1,2)+(ry-dum2)*afcc(1,1))*afcc(3,3)
     >+((dum2-ry)*afcc(1,3)+(rz-dum2)*afcc(1,2))*afcc(3,1)-((dum2-rx
     >)*afcc(1,3)+(rz-dum2)*afcc(1,1))*afcc(3,2))/dum1)
         n3=int(0.5-(((dum2-rx)*afcc(1,2)+(ry-dum2)*afcc(1,1))*afcc(2,3)
     >+((dum2-ry)*afcc(1,3)+(rz-dum2)*afcc(1,2))*afcc(2,1)-((dum2-x
     >3)*afcc(1,3)+(rz-dum2)*afcc(1,1))*afcc(2,2))/dum1)
       x3=afcc(1,1)*n1+afcc(2,1)*n2+afcc(3,1)*n3+(nbase-1)*dfcc/3.**0.5
       y3=afcc(1,2)*n1+afcc(2,2)*n2+afcc(3,2)*n3+(nbase-1)*dfcc/3.**0.5
       z3=afcc(1,3)*n1+afcc(2,3)*n2+afcc(3,3)*n3+(nbase-1)*dfcc/3.**0.5
        dum3=((x3-rx)**2+(y3-ry)**2+(z3-rz)**2 )**0.5
c        write(*,*) n1,n2,n3,nbase,dum3,dum1,dum2
        if (dum3.lt.dum4) then
          dum4=dum3
        endif
        enddo
        f=f+dum4
        endif
      enddo
      f=(f/natoa)**0.5
      return
      end


      subroutine shapepdb(ipdb)

      include 'quafit2014.cmn'

      complex *16 cum1,cum2,cum3

      j1=lnfil(fsub(ipdb),300)
      if (j1.eq.0) stop 'j1.eq.0'
c      write(*,*) fsub(ipdb)(1:j1)

      if (ifilm.eq.0) then
c.......old
        do iq=1,na
          call reasha_first(ipdb,iq)
        enddo
c.......fine old
c.....new
c      open (unit=iu_sin,status='unknown',file=fsub(ipdb)(1:j1)//
c     >'.sha')
c      do iq=1,na
c        read(unit=iu_sin,fmt=1007) 
c     >dum1,(((alm(l,m,is),m=-l,l),l=0,ltird),is=1,1)
c      enddo
c      close(unit=iu_sin)
c.....fine new
        return
      endif
      open (unit=iu_sin,status='unknown',file=fsub(ipdb)(1:j1)//
     >'.sha')

      lstep=1
      lstart=0  

      do iq=1,nqgl
        q=qqgl(iq)
        call wprc(iq,1,nqgl,lstep,lstart)
        do l=0,ltird
          do m=-l,l
            alm(l,m,1)=dcmplx(zer,zer)
          enddo
        enddo



        do i=1,nato
          x1=coord(i,1)
          y1=coord(i,2)
          z1=coord(i,3)
          call cpolar(x1,y1,z1,r,al1,b1)
          cb1=dcos(b1)
          call py_go(ltird,b1)
          call cscsa(ltird,al1)
          i1=iato(i)
  
          dum2=sfacts(i1) * pi4 !! ecco il 4*pi che mancava

          qu=q*r
          
          do l=0,ltird
            if (qu.ne.0.d0) then
              call sphbes(l,qu,sj,sy,sjp,syp)
            else
              if (l.eq.0) then
                sj=1.d0
              else
                sj=0.d0
              endif
            endif
            cum1=sj*(dcmplx(0.d0,1.d0))**l

            cum2=cum1*ysf(l,0)

            alm(l,0,1)=alm(l,0,1)+dum2 *cum2
            do m=1,l
              cum2=cum1*dcmplx(csa(1,m),-csa(2,m))*ysf(l,m)
              alm(l,m,1)=alm(l,m,1)+dum2*cum2  !!! in tutte queste definizioni non c'e' il 4*pi ma quando le usi vanno bene. Questo, moltiplicato per 4*pi va benissimo per la shape ma va ridefinito il suo range di q, perche' poi lo integri.
            enddo
          enddo !! end l
        enddo  !! end nato

c.......negative values of m
        do l=0,ltird
          do m=1,l
            i7=(-1)**(l+m)
            alm(l,-m,1)=i7*dconjg(alm(l,m,1))
          enddo
        enddo


c        write(*,1007) q,(((alm(l,m,is),m=0,l),
c     >         l=0,ltird),is=1,nwave+nsfull+8)
        write(iu_sin,1007) q,(((alm(l,m,is),m=-l,l),
     >         l=0,ltird),is=1,1)

c.......new store
        do l=0,ltird
        do m=-l,l
        do is=1,1
        almshape(ipdb,iq,l,m,is)=alm(l,m,is)
        enddo
        enddo
        enddo
c.......end of new

      enddo  ! end iq
      close(unit=iu_sin)

      return

 1007 format(500000(e15.6E3))
2000  format(a6,i5,1x,a3,a2,a3,2x,i4,a4,3f8.3)
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
 1107 format('#',200(e19.7E3))
 1207 format(200(e19.7E3))


1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end



      subroutine shapech3

      include 'quafit2014.cmn'

      complex *16 cum1,cum2,cum3

      lstep=1
      lstart=0  

      rtramax(npdb+1)=35.
      rtramin(npdb+1)=1.

      do iq=1,nqgl
        q=qqgl(iq)
        call wprc(iq,1,nqgl,lstep,lstart)
        do l=0,ltird
          do m=-l,l
            alm(l,m,1)=dcmplx(zer,zer)
          enddo
        enddo


        ve=150. !volume escluso medio AA
        do i=1,1
          x1=zer
          y1=zer
          z1=zer
          call cpolar(x1,y1,z1,r,al1,b1)
          cb1=dcos(b1)
          call py_go(ltird,b1)
          call cscsa(ltird,al1)
c          i1=4 !     > -0.4580,  3.0,  0.0,   31.89,   1.970,   !CH3  iato(i)
  
          dum2=ve*dexp(-q*q*ve**(2.d0/3)/pi4) * pi4 !! ecco il 4*pi che mancava


          qu=q*r
          
          do l=0,ltird
            if (qu.ne.0.d0) then
              call sphbes(l,qu,sj,sy,sjp,syp)
            else
              if (l.eq.0) then
                sj=1.d0
              else
                sj=0.d0
              endif
            endif
            cum1=sj*(dcmplx(0.d0,1.d0))**l

            cum2=cum1*ysf(l,0)

            alm(l,0,1)=alm(l,0,1)+dum2 *cum2
            do m=1,l
              cum2=cum1*dcmplx(csa(1,m),-csa(2,m))*ysf(l,m)
              alm(l,m,1)=alm(l,m,1)+dum2*cum2  !!! in tutte queste definizioni non c'e' il 4*pi ma quando le usi vanno bene. Questo, moltiplicato per 4*pi va benissimo per la shape ma va ridefinito il suo range di q, perche' poi lo integri.
            enddo
          enddo !! end l
        enddo  !! end nato

c.......negative values of m
        do l=0,ltird
          do m=1,l
            i7=(-1)**(l+m)
            alm(l,-m,1)=i7*dconjg(alm(l,m,1))
          enddo
        enddo



c.......new store
        do l=0,ltird
        do m=-l,l
        do is=1,1
        almshape(npdb+1,iq,l,m,is)=alm(l,m,is)
        enddo
        enddo
        enddo
c.......end of new

      enddo  ! end iq
      close(unit=iu_sin)

      return

 1007 format(500000(e15.6E3))
2000  format(a6,i5,1x,a3,a2,a3,2x,i4,a4,3f8.3)
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
 1107 format('#',200(e19.7E3))
 1207 format(200(e19.7E3))


1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end


      subroutine shapepdb_old(ipdb)

      include 'quafit2014.cmn'

      complex *16 cum1,cum2,cum3

      j1=lnfil(fsub(ipdb),300)
      if (j1.eq.0) stop 'j1.eq.0'
c      write(*,*) fsub(ipdb)(1:j1)

      if (ifilm.eq.0) then
        do iq=1,na
          call reasha_first(ipdb,iq)
        enddo
        return
      endif
      open (unit=iu_sin,status='unknown',file=fsub(ipdb)(1:j1)//
     >'.sha')

      lstep=1
      lstart=0  

      do iq=1,nqgl
        q=qqgl(iq)
        call wprc(iq,1,nqgl,lstep,lstart)
        do l=0,lfirst
          do m=-l,l
            alm(l,m,1)=dcmplx(zer,zer)
          enddo
        enddo


c.......Shape now!
c        q=0.d0

        dr=1.0d0 !angst
        do l=0,lfirst
          do m=0,l
            cum1=dcmplx(0.d0,0.d0)
            do ib=1,nb
              cosb=cb(ib)
              call py_go(l,dacos(cosb))
              cum2=dcmplx(0.d0,0.d0)
              do ia=1,na
                alpha=al(ia)
                call cscsa(l,alpha)
                ftmp=travdw(ipdb,ia,ib)
                dum3=0.d0
                nr=int(ftmp/dr+0.5)
                r=-dr/2
                do ir=1,nr
                  r=r+dr
                  qu=q*r
                  if (qu.ne.0.d0) then
                    call sphbes(l,qu,sj,sy,sjp,syp)
                  else
                    if (l.eq.0) then
                      sj=1.d0
                    else
                      sj=0.d0
                    endif
                  endif
                  dum3=dum3+sj*r*r
                enddo ! end ir
                cum2=cum2+wa(ia)*dum3*dcmplx(csa(1,m),-csa(2,m))
c        write(*,*) 'CC ',l,m,cum2,wa(ia),dum3,dcmplx(csa(1,m),-csa(2,m))
              enddo !end ia
c              write(*,*) 'AA ',l,m,cum2,wb(ib),ysf(l,m)
              cum1=cum1+wb(ib)*cum2*ysf(l,m)
            enddo !end ib
c              write(*,*) ' BB ',l,m,cum1
            alm(l,m,1)=cum1*pi*pi4*dr*(dcmplx(0.d0,1.d0))**l
c              write(*,*) ' DD ',alm(l,m,1)
c              pause 
          enddo !end m
          do m=1,l
            i7=(-1)**(l+m)
            alm(l,-m,1)=i7*dconjg(alm(l,m,1))
          enddo
        enddo !end l
c        write(*,1007) q,(((alm(l,m,is),m=0,l),
c     >         l=0,lfirst),is=1,nwave+nsfull+8)
        write(iu_sin,1007) q,(((alm(l,m,is),m=-l,l),
     >         l=0,lfirst),is=1,1)

c.......new store
        do l=0,lfirst
        do m=-l,l
        do is=1,1
        almshape(ipdb,iq,l,m,is)=alm(l,m,is)
        enddo
        enddo
        enddo
c.......end of new

      enddo  ! end iq
      close(unit=iu_sin)

      return

 1007 format(500000(e15.6E3))
2000  format(a6,i5,1x,a3,a2,a3,2x,i4,a4,3f8.3)
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
 1107 format('#',200(e19.7E3))
 1207 format(200(e19.7E3))


1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end

      subroutine shell1(rmax2)

      include 'quafit2014.cmn'

      dimension nnn(mxrun,4)
      dimension nn2(mxrun,4)
      dimension nshell(mxrun)

      dimension x(6)
      dimension h(6*(6+1)/2),g(6),w(6*3)


      external ums2

c reticolo F
c      afcc(1,1)=0.
c      afcc(1,2)=dfcc/2**0.5
c      afcc(1,3)=dfcc/2**0.5

c      afcc(2,1)=dfcc/2**0.5
c      afcc(2,2)=0.
c      afcc(2,3)=dfcc/2**0.5

c      afcc(3,1)=dfcc/2**0.5
c      afcc(3,2)=dfcc/2**0.5
c      afcc(3,3)=0.


c reticolo P
c      afcc(1,1)=dfcc
c      afcc(1,2)=0.
c      afcc(1,3)=0.
c
c      afcc(2,1)=0.
c      afcc(2,2)=dfcc
c      afcc(2,3)=0.
c
c      afcc(3,1)=0.
c      afcc(3,2)=0.
c      afcc(3,3)=dfcc

c reticolo F di riferimento per la struttura Ic del ghiaccio
      afcc(1,1)=0.
      afcc(1,2)=2*dfcc/3.**0.5
      afcc(1,3)=2*dfcc/3.**0.5

      afcc(2,1)=2*dfcc/3.**0.5
      afcc(2,2)=0.
      afcc(2,3)=2*dfcc/3.**0.5

      afcc(3,1)=2*dfcc/3.**0.5
      afcc(3,2)=2*dfcc/3.**0.5
      afcc(3,3)=0.

      tau=0.2*dfcc

      do ii=1,6
        x(ii)=0.d0
      enddo
      call monk(ums2,x,f,6,100,iseed)
      call simplx(ums2,x,f,6,1.d-30,itc,30000)
c      call zxmin(ums1,6,6,10000,3,x,h,g,f,w,ierr)

      a=xscale(x(1),0.d0,pi/4)
      b=xscale(x(2),1.d0/sqrt(2.),one)
      g1=xscale(x(3),0.d0,pi/4)
      xt=xscale(x(4),-rscale,rscale)
      yt=xscale(x(5),-rscale,rscale)
      zt=xscale(x(6),-rscale,rscale)
2208  format(a40,3(g10.3,1x),i4)
            write(*,2208) 
     >' Euler Rotational Angles (deg): alpha  :',a*conv
            write(*,2208) 
     >'                                beta   :',dacos(b)*conv
            write(*,2208) 
     >'                                gamma  :',g1*conv
            write(*,2208) 
     >' Translation coordinates (Angs.):     X:',xt
            write(*,2208) 
     >'                                      Y:',yt
            write(*,2208) 
     >'                                      Z:',zt


c            write(8,2208) 
c     >' Euler Rotational Angles (deg): alpha  :',a*conv
c            write(8,2208) 
c     >'                                beta   :',dacos(b)*conv
c            write(8,2208) 
c     >'                                gamma  :',g1*conv
c            write(8,2208) 
c     >' Translation coordinates (Angs.):     X:',xt
c            write(8,2208) 
c     >'                                      Y:',yt
c            write(8,2208) 
c     >'                                      Z:',zt
c            write(*,*) '  chi2  ',f
     
      call mat(a,dacos(b),g1,rot)
      f=0.d0
      nnmm=3*rmax2/(dfcc/2)+0.5
      n1mn= nnmm
      n1mx=-nnmm
      n2mn= nnmm
      n2mx=-nnmm
      n3mn= nnmm
      n3mx=-nnmm


       dum1=((afcc(1,1)*afcc(2,2)-afcc(1,2)*afcc(2,1))*afcc(3,3)-
     >((afcc(1,1)*afcc(2,3)-afcc(1,3)*afcc(2,1))*afcc(3,2)+(afcc(
     >1,3)*afcc(2,2)-afcc(1,2)*afcc(2,3))*afcc(3,1)))

      do i=1,nato
        itest=0
        if (iammch(i,1).ne.0) then
          if (iato(i).eq.2.or.
     >(iammch(i,1).eq.8.and.iato(i).eq.3)) itest=1 !!! perche' 2? significa CA per proteine!!!
        else if (inacch(i,1).ne.0) then
          itest=1!tutti   if (iato(i).eq.13.or.iato(i).le.4) itest=1 !!! o il P o un qualunque C
        endif

        if (itest.eq.1) then
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)

       dum4=1.e10
       do nbase=1,2
         dum2=(nbase-1)*dfcc/3.**0.5
         n1=int(0.5-(((dum2-rx)*afcc(2,2)+(ry-dum2)*afcc(2,1))*afcc(3,3)
     >+((dum2-ry)*afcc(2,3)+(rz-dum2)*afcc(2,2))*afcc(3,1)-((dum2-x
     >3)*afcc(2,3)+(rz-dum2)*afcc(2,1))*afcc(3,2))/dum1 )
         n2=int(0.5+(((dum2-rx)*afcc(1,2)+(ry-dum2)*afcc(1,1))*afcc(3,3)
     >+((dum2-ry)*afcc(1,3)+(rz-dum2)*afcc(1,2))*afcc(3,1)-((dum2-rx
     >)*afcc(1,3)+(rz-dum2)*afcc(1,1))*afcc(3,2))/dum1)
         n3=int(0.5-(((dum2-rx)*afcc(1,2)+(ry-dum2)*afcc(1,1))*afcc(2,3)
     >+((dum2-ry)*afcc(1,3)+(rz-dum2)*afcc(1,2))*afcc(2,1)-((dum2-x
     >3)*afcc(1,3)+(rz-dum2)*afcc(1,1))*afcc(2,2))/dum1)
        x3=afcc(1,1)*n1+afcc(2,1)*n2+afcc(3,1)*n3+(nbase-1)*dfcc/3.**0.5
       y3=afcc(1,2)*n1+afcc(2,2)*n2+afcc(3,2)*n3+(nbase-1)*dfcc/3.**0.5
       z3=afcc(1,3)*n1+afcc(2,3)*n2+afcc(3,3)*n3+(nbase-1)*dfcc/3.**0.5
        dum3=((x3-rx)**2+(y3-ry)**2+(z3-rz)**2 )**0.5
c        write(*,*) n1,n2,n3,nbase,dum3,dum1,dum2
        if (dum3.lt.dum4) then
          dum4=dum3
          n12=n1
          n22=n2
          n32=n3
          nbase2=nbase
          x32=x3
          y32=y3
          z32=z3
        endif
        enddo

        n1mn=min(n12,n1mn)
        n1mx=max(n12,n1mx)
        n2mn=min(n22,n2mn)
        n2mx=max(n22,n2mx)
        n3mn=min(n32,n3mn)
        n3mx=max(n32,n3mx)

        f=f+dum4
c        write(*,*) dsqrt(dum2)
c        write(iu,1001) 'ATOM  ',i,'  C   CYS ',char(64+8),
c     >                '   1    ',rx,ry,rz,n1,n2,n3

        endif
      enddo
      f=(f/natoa)**0.5
c            write(*,*) '  chi2  ',f
 
c      goto 555


c      write(*,*) n1mx,n1mn
c      write(*,*) n2mx,n2mn
c      write(*,*) n3mx,n3mn
c      pause

      nsh=5
      
      natob=nato


      natoc=0
      do 40 nbase=1,2
      do 40 n1=n1mn-nsh,n1mx+nsh
      do 40 n2=n2mn-nsh,n2mx+nsh
      do 40 n3=n3mn-nsh,n3mx+nsh
c      do 40 n1=-2,2
c      do 40 n2=-2,2
c      do 40 n3=-2,2
      
       x3=afcc(1,1)*n1+afcc(2,1)*n2+afcc(3,1)*n3+(nbase-1)*dfcc/3.**0.5
       y3=afcc(1,2)*n1+afcc(2,2)*n2+afcc(3,2)*n3+(nbase-1)*dfcc/3.**0.5
       z3=afcc(1,3)*n1+afcc(2,3)*n2+afcc(3,3)*n3+(nbase-1)*dfcc/3.**0.5
c        write(*,*) nato,x3,y3,z3
c        write(41,1001) 'HETATM',i,' HOH   '//amm(1+nbase)//
c     >' ',char(64+4+nbase),
c     >                '   1    ',x3,y3,z3
        r3=(x3**2+y3**2+z3**2)**0.5
c        write(18,*) n1,n2,n3,nbase,r3
        if (r3.gt.rmax2/2.5*1.5) goto 40

c      r1=3.8/2 ! Raggio distanza media AA
c      r2=dfcc/2 ! 1.5
c      d1=dsqrt((r1+r2)**2-r2**2)
c      s=dsqrt(3.d0)*r2

      do i=1,nato
c        if (iato(i).eq.2) then   ! carbonio alpha
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)

c        write(*,*) i,x3,y3,z3
c        write(*,*) rx,ry,rz
        dum2=((x3-rx)**2+(y3-ry)**2+(z3-rz)**2 )**0.5

c        n1a=(ry+rz-rx)/aa+0.5
c        n2a=(rz+rx-ry)/aa+0.5
c        n3a=(rx+ry-rz)/aa+0.5

c        if (n1.eq.n1a.and.n2.eq.n2a.and.n3.eq.n3a) then
c          nnn(i,1)=n1
c          nnn(i,2)=n2
c          nnn(i,3)=n3

         if (dum2.lt.rexc(iato(i))+dfcc/2-tau) then!dfcc/2) then
c          goto 70
cc          write(*,*) ' 1 ci sono'
cc         write(*,*) 'i1=5'
         natob=natob+1
         natoc=natoc+1
         if (natob.gt.mxrun) stop 'natob > mxrun'
        call rotrin(x3,y3,z3,rx33,ry33,rz33,xt,yt,zt,rot)
        coord(natob,1)=rx33
        coord(natob,2)=ry33
        coord(natob,3)=rz33
        iato(natob)=4
          nnn(natoc,1)=n1
          nnn(natoc,2)=n2
          nnn(natoc,3)=n3
          nnn(natoc,4)=nbase
          goto 40
c        endif
c       x4=aa*(n2a+n3a)*0.5
c       y4=aa*(n1a+n3a)*0.5
c       z4=aa*(n1a+n2a)*0.5
c        dum2=((x3-x4)**2+(y3-y4)**2+(z3-z4)**2 )**0.5
c        dum2=((x3-rx)**2+(y3-ry)**2+(z3-rz)**2 )**0.5
        endif
c        endif
      enddo  
40    continue


      toll=0.1
      nsh=4
      natod=0

c      do kk=1,2
      do i=1,natoc
         
       rx=afcc(1,1)*nnn(i,1)+afcc(2,1)*nnn(i,2)
     >+afcc(3,1)*nnn(i,3)+(nnn(i,4)-1)*dfcc/3.**0.5
       ry=afcc(1,2)*nnn(i,1)+afcc(2,2)*nnn(i,2)
     >+afcc(3,2)*nnn(i,3)+(nnn(i,4)-1)*dfcc/3.**0.5
       rz=afcc(1,3)*nnn(i,1)+afcc(2,3)*nnn(i,2)
     >+afcc(3,3)*nnn(i,3)+(nnn(i,4)-1)*dfcc/3.**0.5

      
      do 80 nbase=1,2
      do 80 n1=nnn(i,1)-nsh,nnn(i,1)+nsh
      do 80 n2=nnn(i,2)-nsh,nnn(i,2)+nsh
      do 80 n3=nnn(i,3)-nsh,nnn(i,3)+nsh

        i1=0

      
        if (n1.eq.nnn(i,1).and.
     >n2.eq.nnn(i,2).and.
     >n3.eq.nnn(i,3).and.
     >nbase.eq.nnn(i,4)) goto 80
         
        do j=1,natoc
        if (j.ne.i) then
        if (n1.eq.nnn(j,1).and.
     >n2.eq.nnn(j,2).and.
     >n3.eq.nnn(j,3).and.
     >nbase.eq.nnn(j,4)) goto 80
        endif
        enddo


       x3=afcc(1,1)*n1+afcc(2,1)*n2
     >+afcc(3,1)*n3+(nbase-1)*dfcc/3.**0.5
       y3=afcc(1,2)*n1+afcc(2,2)*n2
     >+afcc(3,2)*n3+(nbase-1)*dfcc/3.**0.5
       z3=afcc(1,3)*n1+afcc(2,3)*n2
     >+afcc(3,3)*n3+(nbase-1)*dfcc/3.**0.5


        dum3=((x3-rx)**2+(y3-ry)**2+(z3-rz)**2 )**0.5

      i1=0
      if (abs(dum3-dfcc).lt.toll) then
       i1=1
      else if (abs(dum3-dfcc*(8./3)**0.5).lt.toll) then
       i1=2
      else if (abs(dum3-dfcc*(11./3)**0.5).lt.toll) then
       i1=3
      endif
c        write(*,*) i,n1,n2,n3,nbase,dum3,i1
       
       if (i1.ne.0) then
c         write(*,*) dum3,i1
         do j=1,natod
        if (n1.eq.nn2(j,1).and.
     >n2.eq.nn2(j,2).and.
     >n3.eq.nn2(j,3).and.
     >nbase.eq.nn2(j,4)) then
          nshell(j)=min(nshell(j),i1)
          goto 90
          endif
         enddo
         natod=natod+1 
         nshell(natod)=i1
         nn2(natod,1)=n1
         nn2(natod,2)=n2
         nn2(natod,3)=n3
         nn2(natod,4)=nbase
 90      continue
       endif

 80    continue
       enddo

c      enddo !kk

      do i=1,natod
         
       x3=afcc(1,1)*nn2(i,1)+afcc(2,1)*nn2(i,2)
     >+afcc(3,1)*nn2(i,3)+(nn2(i,4)-1)*dfcc/3.**0.5
       y3=afcc(1,2)*nn2(i,1)+afcc(2,2)*nn2(i,2)
     >+afcc(3,2)*nn2(i,3)+(nn2(i,4)-1)*dfcc/3.**0.5
       z3=afcc(1,3)*nn2(i,1)+afcc(2,3)*nn2(i,2)
     >+afcc(3,3)*nn2(i,3)+(nn2(i,4)-1)*dfcc/3.**0.5

         call rotrin(x3,y3,z3,rx,ry,rz,xt,yt,zt,rot)
         natob=natob+1
         if (natob.gt.mxrun) stop 'natob > mxrun'
        coord(natob,1)=rx
        coord(natob,2)=ry
        coord(natob,3)=rz
        iato(natob)=nshell(i)
      enddo


      return  ! questo dopo ci vuole

      goto 50
      do i=1,nato
c        if (iato(i).eq.2) then
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        if (z1.gt.0) write(51,1001) 'ATOM  ',i,' '//atypej(iato(i))
c        write(51,1001) 'ATOM  ',i,' '//atypej(iato(i))
     >//'  ALA ',char(64+9),
     >                '   1    ',x1,y1,z1
c        endif
      enddo
 50   do j=1,4!1,3!5,5!1,3+1+5
      do i=1+nato,natob
        if (iato(i).eq.j) then
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        if (z1.gt.0) write(51,1001) 'ATOM  ',i,' HOH   '//amm(1+j)//
c        write(51,1001) 'HETATM',i,' HOH   '//amm(1+j)//
     >' ',char(64+4+j),
     >                '   1    ',x1,y1,z1
        endif
      enddo
      enddo

      stop
      return
1001  format(a6,i5,a10,a1,a8,3(f8.3))
      end

      subroutine nospace(ch1,ch2,l)

      character*(*) ch1,ch2
      do k=1,l
      ch2=ch1
c      write(*,*) k,ch1,ch2,l
      do i=1,l
        if (ch1(i:i).eq.' ') then
c      write(*,*) ' 23 '
          if (i.eq.1) then
c      write(*,*) ' 24 '
            ch2(1:l)=ch1(i+1:l)//' '
          else if (i+1.le.l) then
c      write(*,*) ' 25 ',1,i-1,i+1,l
            ch2(1:l)=ch1(1:i-1)//ch1(i+1:l)//' '
          endif
c      write(*,*) ' 26 '
          ch1=ch2
        endif
      enddo
      enddo
      return
      end



      subroutine reasin_first(ipdb,iq1)

      include 'quafit2014.cmn'

      i1=15*(2*(lfirst+1)*(lfirst+1)*(nsfull+nwave+8)+1)+1+ig77!1!2
      j1=lnfil(fsub(ipdb),300)
      open(unit=iu_sin,access='direct',recl=i1,status='unknown',
     >form='formatted',file=fsub(ipdb)(1:j1)//
     >'.ilm')
c        write(*,*) iq1
        read(unit=iu_sin,rec=iq1  ,fmt=1007) 
     >dum1,(((alm(l,m,is),m=-l,l),l=0,lfirst),is=1,nwave+nsfull+8)
      close(unit=iu_sin)

c.......new store
        do l=0,lfirst
        do m=-l,l
        do is=1,nwave+nsfull+8
        almstore(ipdb,iq1,l,m,is)=alm(l,m,is)
c        write(105,*) alm(l,m,is)
        enddo
        enddo
        enddo
        return
c.......end of new

c      write(*,*) '   ---> reasin   '

      return
 1007 format(500000(e15.6E3))
      end

      subroutine reasha_first(ipdb,iq1)

      include 'quafit2014.cmn'


      i1=15*(2*(ltird+1)*(ltird+1)*(1)+1)+1+ig77
      j1=lnfil(fsub(ipdb),300)
      open(unit=iu_sin,access='direct',recl=i1,status='unknown',
     >form='formatted',file=fsub(ipdb)(1:j1)//
     >'.sha')
        read(unit=iu_sin,rec=iq1  ,fmt=1007) 
     >dum1,(((alm(l,m,is),m=-l,l),l=0,ltird),is=1,1)
      close(unit=iu_sin)

c      write(*,*) '   ---> reasin   '
c.......new store
        do l=0,ltird
        do m=-l,l
        do is=1,1
        almshape(ipdb,iq1,l,m,is)=alm(l,m,is)
        enddo
        enddo
        enddo
        return
c.......end of new

      return
 1007 format(500000(e15.6E3))
      end


      subroutine almsym

      include 'quafit2014.cmn'
      complex*16 a,b,c

c.....Interpola in modo parabolico da un array di n punti
c.....xa la funzione ya nel punto y(x). x puo' anche
c.....essere esterno all'intervallo xa(1)-xa(n)

c      write(*,*) '   ---> almsym   '

      iq1=0
      iq2=1
      iq3=2
      do 10 i=1,nqfull-2
        iq1=iq1+1
        iq2=iq2+1
        iq3=iq3+1
        if (  (qqfull(iq1).gt.q).or.
     >        (qqfull(iq1).le.q.and
     >        .qqfull(iq3).ge.q).or.
     >        (qqfull(iq3).lt.q.and.iq3.eq.nqfull) ) goto 20
10    continue

20    continue

      do l=0,lsecond
      do m=-l,l


      do is=1,nsfull+nwave+8

c.....Banale interpolazione parabolica
      a = ((qqfull(iq1)-qqfull(iq2))*(almstore(ibfinal,iq1,l,m,is)
     >-almstore(ibfinal,iq3,l,m,is))
     >-(almstore(ibfinal,iq1,l,m,is)-almstore(ibfinal,iq2,l,m,is))*
     >(qqfull(iq1)-qqfull(iq3)))/
     >((qqfull(iq1)*qqfull(iq1)-qqfull(iq3)*qqfull(iq3))*
     >(qqfull(iq1)-qqfull(iq2))-(qqfull(iq1)*qqfull(iq1)-
     >qqfull(iq2)*qqfull(iq2))*(qqfull(iq1)-qqfull(iq3)))
      b = (-a*(qqfull(iq1)*qqfull(iq1)-qqfull(iq2)*qqfull(iq2))+
     >(almstore(ibfinal,iq1,l,m,is)-almstore(ibfinal,iq2,l,m,is)))/
     >(qqfull(iq1)-qqfull(iq2))
      c = almstore(ibfinal,iq1,l,m,is)
     >-a*qqfull(iq1)*qqfull(iq1)-b*qqfull(iq1)
      alm(l,m,is) = a*q*q+b*q+c
c      write(*,*) l,m,is,a,b,c
      enddo
      enddo
      enddo
      return
      end

      subroutine iqcalc(nn,iq)

      include 'quafit2014.cmn'

c      write(*,*) '   ---> iqcalc(nn,iq)   top'

      qit=0.d0

      do istate=1,nstate
        do ihagg=1,nhagg(istate)
          dum1=zer
          do iconf=1,nconf(istate)
            dum1=dum1+pq_parz(istate,ihagg,iconf,iq,nn)
          enddo
          qit_parz(istate,ihagg)=dum1/nconf(istate)
          dum1=fract(istate,ihagg,nn)*   
     >(dum1/nconf(istate))/wagg(istate,ihagg)
          qit=qit+dum1
c          write(90+ihagg,*) q,qit,fract(istate,ihagg,nn)
c     >,wagg(istate,ihagg)
        enddo
      enddo
      qit = concp(nn) * qit * 6.022d-4 ! cm ** -1  
c      write(*,*) '   ---> iqcalc(nn,iq)   bottom'
      return
      end

      function pq_parz(istate,ihagg,iconf,iq,nn)

c.....Calcola il fattore di forma P(Q) relativo al Q iq-esimo dell'esperimento nn-esimo
c.....per la terna stato/aggregato/configurazione istate, ihagg, iconf

      include 'quafit2014.cmn'
      complex *16 cum1,cum2

c      write(*,*) '   ---> pqparz   top'

      it=jett(nn)
      if (it.eq.1) then
c.......1) X-rays
        ms=npnt(it)
        wtmp(1)=one
        wtmp(2)=rho0(nn)
        do is=1,nsfull
          wtmp(2+is)=wtmp(2)
        enddo
      else if (it.eq.1+iwave(nn)) then
        ms=npnt(it)
        wtmp(1)=one
        wtmp(2)=one
        wtmp(3)=rho0(nn)
        do is=1,nsfull
          wtmp(3+is)=wtmp(3)
        enddo
      else
c.......Neutrons
        ms=npnt(it)
        wtmp(1)=one
        wtmp(2)=xd2o(nn)
        wtmp(3)=phyd(nn)
        wtmp(4)=phyd(nn)*xd2o(nn)
        wtmp(5)=pdeu(nn)*xd2o(nn)
        wtmp(6)=1.e-2*( (-0.562+phyd(nn)*9.66)*(1.0-xd2o(nn))
     >              +( 6.404+pdeu(nn)*1.5 )*xd2o(nn))
c          rho0=-wtmp(6)/gq
c          write(*,*) phyd,pdeu,xd2o,gq,-wtmp(6)
c          pause
        do is=1,nsfull
          wtmp(6+is)=wtmp(6)
        enddo
      endif


      i1=0
      kt=kii(it)
      pq_parz=zer
      do js1=1,ms
      do js2=js1,ms
        i1=i1+1
        pq_parz=pq_parz
     >+pqtt(iq,kt,i1,istate,ihagg,iconf)*wtmp(js1)*wtmp(js2)
c      write(100+i1,*) 
c     >q,pqtt(iq,kt,i1,istate,ihagg,iconf)*wtmp(js1)*wtmp(js2)
      enddo
      enddo

c      write(*,*) '   ---> pqparz   bottom'
      return
      end


      subroutine pntdef

      include 'quafit2014.cmn'

c.....1) X-ray standard
      ipnt(1,1)=1
      ipnt(1,2)=1+nwave+6
      do is=1,nsfull
        ipnt(1,2+is)=1+nwave+6+is
      enddo
      ms=2+nsfull
      npnt(1)=ms


c.....2) X-ray anomalous
      do i=1,nwave
        ipnt(1+i,1)=1
        ipnt(1+i,2)=1+i
        ipnt(1+i,3)=1+nwave+6
        do is=1,nsfull
          ipnt(1+i,3+is)=1+nwave+6+is
        enddo
        ms=2+nsfull+1
        npnt(1+i)=ms
      enddo

c.....3) Neutrons and polarized neutrons
      ipnt(2+nwave,1)=1+nwave+1
      ipnt(2+nwave,2)=1+nwave+2
      ipnt(2+nwave,3)=1+nwave+3
      ipnt(2+nwave,4)=1+nwave+4
      ipnt(2+nwave,5)=1+nwave+5
      ipnt(2+nwave,6)=1+nwave+6
      do is=1,nsfull
        ipnt(2+nwave,6+is)=1+nwave+6+is
      enddo
      ms=6+nsfull
      npnt(2+nwave)=ms
      return
      end


      subroutine testlink

      include 'quafit2014.cmn'

      nlink=0
      do istate1=1,nstate-1
        do istate2=istate1+1,nstate
          if (nsite(istate1).eq.nsite(istate2)) then
            i1=0
            do i=1,nsite(istate1)
              if (ipdbsite(istate1,i).eq.ipdbsite(istate2,i)) i1=i1+1
            enddo
            if (i1.eq.nsite(istate1)) then
              ilink(istate1,istate2)=1              !sono parenti
              i2=0
              do i=2,nsite(istate1)
                if (ipdbsite(istate1,i).eq.ipdbsite(istate1,1)) i2=i2+1
              enddo
              if (i2.eq.nsite(istate1)-1) then
                mlink(istate1)=1 !tutti i siti sono uguali
                mlink(istate2)=1 !tutti i siti sono uguali
              else
                mlink(istate1)=0 !non tutti i siti sono uguali
                mlink(istate2)=0 !non tutti i siti sono uguali
              endif
            endif
          else
            i1=0
            do i=2,nsite(istate1)
              if (ipdbsite(istate1,i).eq.ipdbsite(istate1,1)) i1=i1+1
            enddo
            i2=0
            do i=2,nsite(istate2)
              if (ipdbsite(istate2,i).eq.ipdbsite(istate2,1)) i2=i2+1
            enddo
            if (i1.eq.nsite(istate1)-1.and.i2.eq.nsite(istate2)-1) then
              if (ipdbsite(istate1,1).eq.ipdbsite(istate2,1)) then
                ilink(istate1,istate2)=1              !sono parenti
                mlink(istate1)=1 !tutti i siti sono uguali
                mlink(istate2)=1 !tutti i siti sono uguali
              else
                ilink(istate1,istate2)=0
              endif
            else
              ilink(istate1,istate2)=0
            endif
          endif
          if (ilink(istate1,istate2).eq.0) 
     >       stop 'ilink(istate1,istate2).eq.0'
        enddo
      enddo
      return
      end



      subroutine xfraccalc

      include 'quafit2014.cmn'

c      write(*,*) '   ---> xfracalc   '

      do nn=1,nexper
        dum1=0.d0
        do iagg=1,nagg
          dum1=dum1+xfrac(iagg,nn,1)
        enddo
        if (dum1.eq.0) stop 'dum1=0,xfraccalc'
        do iagg=1,nagg
          istate=jstagg(iagg)
          ihagg =jhaagg(iagg)
          fract(istate,ihagg,nn)=xfrac(iagg,nn,1)/dum1
        enddo
      enddo
      return
      end

      subroutine xfraccalc_old

      include 'quafit2014.cmn'

c      write(*,*) '   ---> xfracalc   '

      do nn=1,nexper
        istate=jstagg(1)
        ihagg =jhaagg(1)
        fract(istate,ihagg,nn)=xfrac(1,nn,1)
        dum1=1-xfrac(1,nn,1)
        do iagg=2,nagg-1
          istate=jstagg(iagg)
          ihagg =jhaagg(iagg)
          fract(istate,ihagg,nn)=dum1*xfrac(iagg,nn,1)
          dum1=dum1*(1-xfrac(iagg,nn,1))
        enddo
        istate=jstagg(nagg)
        ihagg =jhaagg(nagg)
        fract(istate,ihagg,nn)=dum1
      enddo
      return
      end

      subroutine xfraccalc_first

      include 'quafit2014.cmn'

c      write(*,*) '   ---> xfracalc   '

      dum2=1./nagg
      do nn=1,nexper
        do iagg=1,nagg
          xfrac(iagg,nn,1)=dum2
          xfrac(iagg,nn,4)=0.5
        enddo
      enddo
      return
      end

      subroutine xfraccalc_first_old

      include 'quafit2014.cmn'

      write(*,*) '   ---> xfracalc   '

      dum2=1./nagg
      do nn=1,nexper
        xfrac(1,nn,1)=dum2
        xfrac(1,nn,4)=0.5
        dum1=1-xfrac(1,nn,1)
        do iagg=2,nagg-1
          xfrac(iagg,nn,1)=dum2/dum1
          xfrac(iagg,nn,4)=0.5
          dum1=dum1*(1-xfrac(iagg,nn,1))
        enddo
      enddo
      return
      end

      subroutine fraccalc

      include 'quafit2014.cmn'
      dimension zipar(mxagg)
      dimension zepar(mxagg)
      external fconstr1

c      write(*,*) '   ---> fracalc   '

      hip(1)=one

      fout=0.d0

      do nn=1,nexper
        t=tempexp(nn)+273.15          !K
        t1=t-t0ref                    !K
        t2=(t*(log(t/t0ref)-1)+t0ref) !K
        p=(presexp(nn)-p0ref)/41.84   !cal/cm**3

c.......phi(1)=G0 cal/mol
c.......phi(2)=S0 cal/mol/K
c.......phi(3)=Cp cal/mol/K
c.......phi(4)=Vd cm**3/mol
c.......phi(5)=alpha cm**3/mol/K
c.......phi(6)=beta  cm**3/mol/bar

c       hip(1)=one
        hip(2)=-t1
        hip(3)=-t2
        hip(4)=p
        hip(5)=p*t1
        hip(6)=0.5*p*p*41.84

        do istate=1,nstate
          do ihagg=1,nhagg(istate)
            dum1=0.d0
            do itherf=1,mxtherf
              phi(itherf)=ftherm(istate,ihagg,itherf,1,1)!!1.d0
              do k=1,neffec
                if (effave(k).ne.0.d0) then
                phi(itherf)=phi(itherf)
     >            +ftherm(istate,ihagg,itherf,1+k,1)*
     >                 (effexp(nn,k)/effave(k)-1.d0)
c                  if (ftherm(istate,ihagg,itherf,1+k,1).ne.0.d0) then
c                  write(*,*) 'BETA ',itherf,k,
c     >ftherm(istate,ihagg,itherf,1+k,1),
c     >                 (effexp(nn,k)/effave(k)-1.d0)
c                  endif

                endif
              enddo
ccc              phi(itherf)=phi(itherf)*ftherm(istate,ihagg,itherf,1,1)
              dum1=dum1+phi(itherf)*hip(itherf)
            enddo
            gibbs(istate,ihagg)=dum1
c                 write(*,*) 'GIBBS ' ,istate,ihagg,dum1,
c     >ftherm(istate,ihagg,1,1,1)
          enddo
        enddo

        do j=1,nagg
          istatej=jstagg(j)
          ihaggj =jhaagg(j)
          do i=1,j-1
            istatei=jstagg(i)
            ihaggi =jhaagg(i)
            if (iflagg(i,j).eq.1) then

cc............scrivendo (aj/ai)Ai=Aj allora
c              dum1=(gibbs(istatej,ihaggj)
c     >             -agg(jaghag(istatej,ihaggj))*gibbs(istatei,ihaggi)
c     >             /agg(jaghag(istatei,ihaggi)))/(1.99*t)

c............scrivendo invece Ai/ai=Aj/aj allora
              dum1=( gibbs(istatej,ihaggj)-gibbs(istatei,ihaggi)
     >             )/(1.99*t)


c              if(abs(dum1).gt.cutexp)then
c                dum1=cutexp*sign(1.d0,dum1)
c              endif
              rk(i,j)=-dum1!!!! facciamo che e' il logaritmo... dexp(-dum1)
c                 write(*,*) ' BB ',i,j,rk(i,j)
c     >,agg(jaghag(istatei,ihaggi)),agg(jaghag(istatej,ihaggj))
            endif
          enddo
        enddo
        zipar(1)=concp(nn)/wagg(1,1)*agg(jaghag(1,1))  !!! questa e' la conc molare del primo state (gli FU e gli FL)
c.......ovvero non importa se il primo e' un dimero un pentamero un monomero e cosi' via. 

c       write(*,*) ' CONC ',nn,concp(nn),zipar(1),
c     >wagg(1,1),agg(jaghag(1,1))
        call constr(fconstr1,0.d0,1.d0,zipar,1,zepar,nzepar)
        dum1=0.d0
        do iagg=1,nagg
          istate=jstagg(iagg)
          ihagg =jhaagg(iagg)
          if (nagg.eq.1) zepar(iagg)=1.d0
          fract(istate,ihagg,nn)=zepar(iagg)
c                 write(93,*) ' CC ',iagg,zepar(iagg),agg(iagg)

ccccc          write(*,*) 'CC ',istate,iagg,ihagg,nn,zepar(iagg)

c          pause
          dum1=dum1+zepar(iagg)
        enddo
        if (dabs(dum1-1.d0).gt.1.e-2) fout=1.d20
      enddo !end nn
      return
      end

      subroutine constr(funct,xmin,xmax,zipar,nzipar,zepar,nzepar)
      implicit real*8 (a-h,o-z)
      parameter(nmax=100,nitmax=100)
      parameter (mxz=10)
      dimension zipar(mxz)
      dimension zepar(mxz)
      external funct
      dimension x0(nmax)

cc      write(*,*) ' 1 ci sono'
      call zero1(funct,xmin,xmax,x0,n,zipar,nzipar,zepar,nzepar)
cc      write(*,*) ' 2 ci sono'
      call funct(x0(1),f1,zipar,nzipar,zepar,nzepar)
cc      write(*,*) ' 3 ci sono'

      return
      end



      subroutine zero1(funct,xmin,xmax,x0,n,zipar,nzipar,zepar,nzepar)
      implicit real*8 (a-h,o-z)
      parameter(nmax=100,nitmax=2000,tiny=1.d-300)
      parameter (mxz=10)
      dimension zipar(mxz)
      dimension zepar(mxz)
      external funct
      dimension x0(nmax)
c      write(*,*) xmin,xmax
c      pause
cc      write(*,*) ' 4 ci sono',zipar,nzipar,zepar,nzepar
      n1=50
      n=0
      xmax1=xmax
      xmin1=xmin
      xmax0=xmax
30    nit=0
cc      write(*,*) ' 6 ci sono',xmin1
 10   continue!  write(*,*) ' 6 ci sono',xmin1
      call funct(xmin1,f1,zipar,nzipar,zepar,nzepar)
c      write(*,*) ' NIT ',nit,xmax1,xmin1
      if (nit.gt.nitmax) goto 20
      do i=2,n1
c        write(*,*) ' 0 ci sono'
        x=xmin1+(xmax1-xmin1)/(n1-1)*(i-1)
c        write(*,*) ' 1 ci sono'
        call funct(x,f2,zipar,nzipar,zepar,nzepar)
c        write(*,*) ' 2 ci sono',f1,f2
c        write(*,*) x,f2
c     >,sign(1.d0,f1),sign(1.d0,f2)
        if (sign(1.d0,f1).ne.sign(1.d0,f2)) then
cc          write(*,*) 'ci sono',nit,n1,i
          nit=nit+1
c        write(*,*) ' 3 ci sono'
          x1=xmin1+(xmax1-xmin1)/(n1-1)*(i-2)
          x2=xmin1+(xmax1-xmin1)/(n1-1)*(i-1)
c        write(*,*) ' 4 ci sono'
          xmin1=x1
          xmax1=x2
c          write(*,*) ' ccc ',xmin1,xmax1
          if (dabs(x1-x2).lt.tiny) goto 20!if (x1.eq.x2) goto 20
          goto 10
c        else  if (dabs(x1-x2).lt.tiny) then
c          goto 20
        else 
          f1=f2
        endif
      enddo
20    continue!write(*,*) ' cc ',xmin1,xmax1
c      if (xmin1.ge.xmax1) return
      n=n+1
      if (n.gt.nmax) stop 'n>nmax'
      x0(n)=0.5*(xmax1+xmin1)
      xmin1=xmax1
      xmax1=xmax0
      if (xmin1.ge.xmax1) then
        n=n-1
        return
      endif
      goto 30      
      end

      subroutine fconstr1(x1,y,zipar,nzipar,x,nzepar)

      include 'quafit2014.cmn'
      dimension zipar(mxagg)
      dimension x(mxagg)

      c=zipar(1)
      x(1)=x1

      y=-1.d0
      do j=1,nagg
        do i=1,j-1
          if (iflagg(i,j).eq.1) then
c             x(j)=rk(i,j)*x(i)**(agg(j)/agg(i))*c**(agg(j)/agg(i)-1)
c     >*agg(j)*agg(i)**(-agg(j)/agg(i))  !!! OK

cc............scrivendo (aj/ai)Ai=Aj allora
c             dum1=rk(i,j)+ log(x(i))*(agg(j)/agg(i))
c     >+log(c)*(agg(j)/agg(i)-1)+log(agg(j))+log(agg(i))*(-agg(j)/agg(i))  !!! OK

c............scrivendo invece Ai/ai=Aj/aj allora
             dum1=agg(j)*rk(i,j)+ log(x(i))*(agg(j)/agg(i))
     >+log(c)*(agg(j)/agg(i)-1)+log(agg(j))+log(agg(i))*(-agg(j)/agg(i))

             if(abs(dum1).gt.cutexp)then
                dum1=cutexp*sign(1.d0,dum1)
             endif
             x(j)=exp(dum1)             
          endif
        enddo
        y=y+x(j)
      enddo
      nzepar=nagg
c      write(*,*) 'CONSTR ',y,c,(x(j),agg(j),j=1,nagg)
      return 
      end

      subroutine mparcalc

      include 'quafit2014.cmn'

c.....Questo schema potrebbe essere del tutto generale e potresti usarlo all'interno di un ciclo. In base a quello che cambi, potresti poi decidere cosa chiamare tra le varie subroutine, potresti anche richiamare i file di memorizzazione e rimpiazzarli o ripristinarli a seconda dell'accettazione della mossa MC

c.....Individua i parametri da minimizzare

c     F=fisso, V=variabile, U=unico

c     Stato Aggregato Configurazione
c           Sito
c j=0 F     F         F
c j=1 V     V         V
c j=2 U     U         U j2NAME
c j=3 V     U         U j3NAME(istate)
c j=4 V     V         U 
c                                     isite


      j2ascrew=0
      j2alpscrew=0
      j2cbescrew=0
      j2dscrew=0
      j2rmeff=0
      do k=1,nsfull  
      j2rhcl(k)=0
      enddo
      do k=1,mxtherf  
      do j=1,neffec+1    
      j2ftherm(k,j)=0
      enddo
      enddo
      j2ums=0
      j2aus=0
      j2cbu=0
      j2aas=0
      j2cba=0
      j2gas=0
c      j2lnk=0 !!l'ho messo senza riflettere

      mpar=0
      do istate=1,nstate

c.......Inizio della parte in cui trova tutti i parametri
        j3ascrew(istate)=0
        j3alpscrew(istate)=0
        j3cbescrew(istate)=0
        j3dscrew(istate)=0
        j3rmeff(istate)=0
        do k=1,nsfull  
        j3rhcl(istate,k)=0
        enddo
        do k=1,mxtherf  
        do j=1,neffec+1    
        j3ftherm(istate,k,j)=0
        enddo
        enddo
        j3ums(istate)=0
        j3aus(istate)=0
        j3cbu(istate)=0
        j3aas(istate)=0
        j3cba(istate)=0
        j3gas(istate)=0
c        j3lnk(istate)=0 !senza pensare...

        do ihagg=1,nhagg(istate)

          if (jascrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=ascrew(istate,ihagg,4)
              ascrew(istate,ihagg,4+iconf)=ascrew(istate,ihagg,5)
              varnam(mpar)='ascrew'
            enddo
          else if (jascrew(istate,ihagg).eq.0) then
            do iconf=1,nconf(istate)
              ascrew(istate,ihagg,4+iconf)=ascrew(istate,ihagg,5)
            enddo
          else if (jascrew(istate,ihagg).eq.2) then
            if (j2ascrew.eq.0) then
              j2ascrew=1 !significa che e' di tipo 2
              mpar=mpar+1
              vardel(mpar)=ascrew(1,1,4)
c              ascrew(1,1,5)=ascrew(1,1,1)
              varnam(mpar)='ascrew'
            endif
          else if (jascrew(istate,ihagg).eq.3) then
            if (j3ascrew(istate).eq.0) then
              j3ascrew(istate)=1 !significa che e' di tipo 3 e quindi varia con istate
              mpar=mpar+1
              vardel(mpar)=ascrew(istate,1,4)
c              ascrew(istate,1,5)=ascrew(istate,1,1)
              varnam(mpar)='ascrew'
            endif
          else if (jascrew(istate,ihagg).eq.4) then
            mpar=mpar+1
            vardel(mpar)=ascrew(istate,ihagg,4)
c            ascrew(istate,ihagg,5)=ascrew(istate,ihagg,1)
            varnam(mpar)='ascrew'
          endif
          if (jalpscrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=alpscrew(istate,ihagg,4)
              alpscrew(istate,ihagg,4+iconf)=alpscrew(istate,ihagg,5)
              varnam(mpar)='alpscrew'
            enddo
          else if (jalpscrew(istate,ihagg).eq.0) then
            do iconf=1,nconf(istate)
              alpscrew(istate,ihagg,4+iconf)=alpscrew(istate,ihagg,5)
            enddo
          else if (jalpscrew(istate,ihagg).eq.2) then
            if (j2alpscrew.eq.0) then
              j2alpscrew=1 !significa che e' di tipo 2
              mpar=mpar+1
              vardel(mpar)=alpscrew(1,1,4)
c              alpscrew(1,1,5)=alpscrew(1,1,1)
              varnam(mpar)='alpscrew'
            endif
          else if (jalpscrew(istate,ihagg).eq.3) then
            if (j3alpscrew(istate).eq.0) then
              j3alpscrew(istate)=1 !significa che e' di tipo 3 e quindi varia con istate
              mpar=mpar+1
              vardel(mpar)=alpscrew(istate,1,4)
c              alpscrew(istate,1,5)=alpscrew(istate,1,1)
              varnam(mpar)='alpscrew'
            endif
          else if (jalpscrew(istate,ihagg).eq.4) then
            mpar=mpar+1
            vardel(mpar)=alpscrew(istate,ihagg,4)
c            alpscrew(istate,ihagg,5)=alpscrew(istate,ihagg,1)
            varnam(mpar)='alpscrew'
          endif
          if (jcbescrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=cbescrew(istate,ihagg,4)
              cbescrew(istate,ihagg,4+iconf)=cbescrew(istate,ihagg,5)
              varnam(mpar)='cbescrew'
            enddo
          else if (jcbescrew(istate,ihagg).eq.0) then
            do iconf=1,nconf(istate)
              cbescrew(istate,ihagg,4+iconf)=cbescrew(istate,ihagg,5)
            enddo
          else if (jcbescrew(istate,ihagg).eq.2) then
            if (j2cbescrew.eq.0) then
              j2cbescrew=1 !significa che e' di tipo 2
              mpar=mpar+1
              vardel(mpar)=cbescrew(1,1,4)
c              cbescrew(1,1,5)=cbescrew(1,1,1)
              varnam(mpar)='cbescrew'
            endif
          else if (jcbescrew(istate,ihagg).eq.3) then
            if (j3cbescrew(istate).eq.0) then
              j3cbescrew(istate)=1 !significa che e' di tipo 3 e quindi varia con istate
              mpar=mpar+1
              vardel(mpar)=cbescrew(istate,1,4)
c              cbescrew(istate,1,5)=cbescrew(istate,1,1)
              varnam(mpar)='cbescrew'
            endif
          else if (jcbescrew(istate,ihagg).eq.4) then
            mpar=mpar+1
            vardel(mpar)=cbescrew(istate,ihagg,4)
c            cbescrew(istate,ihagg,5)=cbescrew(istate,ihagg,1)
            varnam(mpar)='cbescrew'
          endif

          if (jdscrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=dscrew(istate,ihagg,4)
              dscrew(istate,ihagg,4+iconf)=dscrew(istate,ihagg,5)
              varnam(mpar)='dscrew'
            enddo
          else if (jdscrew(istate,ihagg).eq.0) then
            do iconf=1,nconf(istate)
              dscrew(istate,ihagg,4+iconf)=dscrew(istate,ihagg,5)
            enddo
          else if (jdscrew(istate,ihagg).eq.2) then
            if (j2dscrew.eq.0) then
              j2dscrew=1
              mpar=mpar+1
              vardel(mpar)=dscrew(1,1,4)
c              dscrew(1,1,5)=dscrew(1,1,1)
              varnam(mpar)='dscrew'
            endif
          else if (jdscrew(istate,ihagg).eq.3) then
            if (j3dscrew(istate).eq.0) then
              j3dscrew(istate)=1
              mpar=mpar+1
              vardel(mpar)=dscrew(istate,1,4)
c              dscrew(istate,1,5)=dscrew(istate,1,1)
              varnam(mpar)='dscrew'
            endif
          else if (jdscrew(istate,ihagg).eq.4) then
            mpar=mpar+1
            vardel(mpar)=dscrew(istate,ihagg,4)
c            dscrew(istate,ihagg,5)=dscrew(istate,ihagg,1)
            varnam(mpar)='dscrew'
          endif

          if (jrmeff(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=rmeff(istate,ihagg,4)
              rmeff(istate,ihagg,4+iconf)=rmeff(istate,ihagg,5)
              varnam(mpar)='rmeff'
            enddo
          else if (jrmeff(istate,ihagg).eq.0) then
            do iconf=1,nconf(istate)
              rmeff(istate,ihagg,4+iconf)=rmeff(istate,ihagg,5)
            enddo
          else if (jrmeff(istate,ihagg).eq.2) then
            if (j2rmeff.eq.0) then
              j2rmeff=1
              mpar=mpar+1
              vardel(mpar)=rmeff(1,1,4)
c              rmeff(1,1,5)=rmeff(1,1,1)
              varnam(mpar)='rmeff'
            endif
          else if (jrmeff(istate,ihagg).eq.3) then
            if (j3rmeff(istate).eq.0) then
              j3rmeff(istate)=1
              mpar=mpar+1
              vardel(mpar)=rmeff(istate,1,4)
c              rmeff(istate,1,5)=rmeff(istate,1,1)
              varnam(mpar)='rmeff'
            endif
          else if (jrmeff(istate,ihagg).eq.4) then
            mpar=mpar+1
            vardel(mpar)=rmeff(istate,ihagg,4)
c            rmeff(istate,ihagg,5)=rmeff(istate,ihagg,1)
            varnam(mpar)='rmeff'
          endif


          do k=1,nsfull  
          if (jrhcl(istate,ihagg,k).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=rhcl(istate,ihagg,k,4)
              rhcl(istate,ihagg,k,4+iconf)=rhcl(istate,ihagg,k,5)
              varnam(mpar)='rhcl'
            enddo
          else if (jrhcl(istate,ihagg,k).eq.0) then
            do iconf=1,nconf(istate)
              rhcl(istate,ihagg,k,4+iconf)=rhcl(istate,ihagg,k,5)
            enddo
          else if (jrhcl(istate,ihagg,k).eq.2) then
            if (j2rhcl(k).eq.0) then
              j2rhcl(k)=1
              mpar=mpar+1
              vardel(mpar)=rhcl(1,1,k,4)
c              rhcl(1,1,k,5)=rhcl(1,1,k,1)
              varnam(mpar)='rhcl'
            endif
          else if (jrhcl(istate,ihagg,k).eq.3) then
            if (j3rhcl(istate,k).eq.0) then
              j3rhcl(istate,k)=1
              mpar=mpar+1
              vardel(mpar)=rhcl(istate,1,k,4)
c              rhcl(istate,1,k,5)=rhcl(istate,1,k,1)
              varnam(mpar)='rhcl'
            endif
          else if (jrhcl(istate,ihagg,k).eq.4) then
            mpar=mpar+1
            vardel(mpar)=rhcl(istate,ihagg,k,4)
c            rhcl(istate,ihagg,k,5)=rhcl(istate,ihagg,k,1)
            varnam(mpar)='rhcl'
          endif
          enddo

          do k=1,mxtherf  
          do j=1,neffec+1    
          if (jftherm(istate,ihagg,k,j).eq.1) then
            stop 'jftherm'
          else if (jftherm(istate,ihagg,k,j).eq.2) then
            if (j2ftherm(k,j).eq.0) then
              j2ftherm(k,j)=1
              mpar=mpar+1
              vardel(mpar)=ftherm(1,1,k,j,4)
              varnam(mpar)='ftherm'
            endif
          else if (jftherm(istate,ihagg,k,j).eq.3) then
            if (j3ftherm(istate,k,j).eq.0) then
              j3ftherm(istate,k,j)=1
              mpar=mpar+1
              vardel(mpar)=ftherm(istate,1,k,j,4)
              varnam(mpar)='ftherm'
            endif
          else if (jftherm(istate,ihagg,k,j).eq.4) then
            mpar=mpar+1
            vardel(mpar)=ftherm(istate,ihagg,k,j,4)
            varnam(mpar)='ftherm'
          endif
          enddo
          enddo

          if (jfracm.eq.1) then
            do nn=1,nexper  
              mpar=mpar+1
              vardel(mpar)=xfrac(jaghag(istate,ihagg),nn,4)
              varnam(mpar)='xfrac'
            enddo
          endif
c          if (jfracm.eq.1.and.jaghag(istate,ihagg).gt.1) then
c            do nn=1,nexper  
c              mpar=mpar+1
c              vardel(mpar)=xfrac(jaghag(istate,ihagg)-1,nn,4)
c              varnam(mpar)='xfrac'
c            enddo
c          endif

        enddo

        do isite=1,nsite(istate)

          if (jums(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=ums(istate,isite,4)
c              ums(istate,isite,4+iconf)=ums(istate,isite,5)
              varnam(mpar)='ums'
            enddo
          else if (jums(istate,isite).eq.0) then
            do iconf=1,nconf(istate)
c              ums(istate,isite,4+iconf)=ums(istate,isite,5)
            enddo
          else if (jums(istate,isite).eq.2) then
            if (j2ums.eq.0) then
              j2ums=1
              mpar=mpar+1
              vardel(mpar)=ums(1,1,4)
c              ums(1,1,5)=ums(1,1,1)
              varnam(mpar)='ums'
            endif
          else if (jums(istate,isite).eq.3) then
            if (j3ums(istate).eq.0) then
              j3ums(istate)=1
              mpar=mpar+1
              vardel(mpar)=ums(istate,1,4)
c              ums(istate,1,5)=ums(istate,1,1)
              varnam(mpar)='ums'
            endif
          else if (jums(istate,isite).eq.4) then
            mpar=mpar+1
            vardel(mpar)=ums(istate,isite,4)
c            ums(istate,isite,5)=ums(istate,isite,1)
            varnam(mpar)='ums'
          endif

          if (jaus(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=aus(istate,isite,4)
c              aus(istate,isite,4+iconf)=aus(istate,isite,5)
              varnam(mpar)='aus'
            enddo
          else if (jaus(istate,isite).eq.0) then
            do iconf=1,nconf(istate)
c              aus(istate,isite,4+iconf)=aus(istate,isite,5)
            enddo
          else if (jaus(istate,isite).eq.2) then
            if (j2aus.eq.0) then
              j2aus=1
              mpar=mpar+1
              vardel(mpar)=aus(1,1,4)
c              aus(1,1,5)=aus(1,1,1)
              varnam(mpar)='aus'
            endif
          else if (jaus(istate,isite).eq.3) then
            if (j3aus(istate).eq.0) then
              j3aus(istate)=1
              mpar=mpar+1
              vardel(mpar)=aus(istate,1,4)
c              aus(istate,1,5)=aus(istate,1,1)
              varnam(mpar)='aus'
            endif
          else if (jaus(istate,isite).eq.4) then
            mpar=mpar+1
            vardel(mpar)=aus(istate,isite,4)
c            aus(istate,isite,5)=aus(istate,isite,1)
            varnam(mpar)='aus'
          endif

          if (jcbu(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=cbu(istate,isite,4)
c              cbu(istate,isite,4+iconf)=cbu(istate,isite,5)
              varnam(mpar)='cbu'
            enddo
          else if (jcbu(istate,isite).eq.0) then
            do iconf=1,nconf(istate)
c              cbu(istate,isite,4+iconf)=cbu(istate,isite,5)
            enddo
          else if (jcbu(istate,isite).eq.2) then
            if (j2cbu.eq.0) then
              j2cbu=1
              mpar=mpar+1
              vardel(mpar)=cbu(1,1,4)
c              cbu(1,1,5)=cbu(1,1,1)
              varnam(mpar)='cbu'
            endif
          else if (jcbu(istate,isite).eq.3) then
            if (j3cbu(istate).eq.0) then
              j3cbu(istate)=1
              mpar=mpar+1
              vardel(mpar)=cbu(istate,1,4)
c              cbu(istate,1,5)=cbu(istate,1,1)
              varnam(mpar)='cbu'
            endif
          else if (jcbu(istate,isite).eq.4) then
            mpar=mpar+1
            vardel(mpar)=cbu(istate,isite,4)
c            cbu(istate,isite,5)=cbu(istate,isite,1)
            varnam(mpar)='cbu'
          endif

          if (jaas(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=aas(istate,isite,4)
c              aas(istate,isite,4+iconf)=aas(istate,isite,5)
              varnam(mpar)='aas'
            enddo
          else if (jaas(istate,isite).eq.0) then
            do iconf=1,nconf(istate)
c              aas(istate,isite,4+iconf)=aas(istate,isite,5)
            enddo
          else if (jaas(istate,isite).eq.2) then
            if (j2aas.eq.0) then
              j2aas=1
              mpar=mpar+1
              vardel(mpar)=aas(1,1,4)
c              aas(1,1,5)=aas(1,1,1)
              varnam(mpar)='aas'
            endif
          else if (jaas(istate,isite).eq.3) then
            if (j3aas(istate).eq.0) then
              j3aas(istate)=1
              mpar=mpar+1
              vardel(mpar)=aas(istate,1,4)
c              aas(istate,1,5)=aas(istate,1,1)
              varnam(mpar)='aas'
            endif
          else if (jaas(istate,isite).eq.4) then
            mpar=mpar+1
            vardel(mpar)=aas(istate,isite,4)
c            aas(istate,isite,5)=aas(istate,isite,1)
            varnam(mpar)='aas'
          endif

          if (jcba(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=cba(istate,isite,4)
c              cba(istate,isite,4+iconf)=cba(istate,isite,5)
              varnam(mpar)='cba'
            enddo
          else if (jcba(istate,isite).eq.0) then
            do iconf=1,nconf(istate)
c              cba(istate,isite,4+iconf)=cba(istate,isite,5)
            enddo
          else if (jcba(istate,isite).eq.2) then
            if (j2cba.eq.0) then
              j2cba=1
              mpar=mpar+1
              vardel(mpar)=cba(1,1,4)
c              cba(1,1,5)=cba(1,1,1)
              varnam(mpar)='cba'
            endif
          else if (jcba(istate,isite).eq.3) then
            if (j3cba(istate).eq.0) then
              j3cba(istate)=1
              mpar=mpar+1
              vardel(mpar)=cba(istate,1,4)
c              cba(istate,1,5)=cba(istate,1,1)
              varnam(mpar)='cba'
            endif
          else if (jcba(istate,isite).eq.4) then
            mpar=mpar+1
            vardel(mpar)=cba(istate,isite,4)
c            cba(istate,isite,5)=cba(istate,isite,1)
            varnam(mpar)='cba'
          endif

          if (jgas(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              mpar=mpar+1
              vardel(mpar)=gas(istate,isite,4)
c              gas(istate,isite,4+iconf)=gas(istate,isite,5)
              varnam(mpar)='gas'
            enddo
          else if (jgas(istate,isite).eq.0) then
            do iconf=1,nconf(istate)
c              gas(istate,isite,4+iconf)=gas(istate,isite,5)
            enddo
          else if (jgas(istate,isite).eq.2) then
            if (j2gas.eq.0) then
              j2gas=1
              mpar=mpar+1
              vardel(mpar)=gas(1,1,4)
c              gas(1,1,5)=gas(1,1,1)
              varnam(mpar)='gas'
            endif
          else if (jgas(istate,isite).eq.3) then
            if (j3gas(istate).eq.0) then
              j3gas(istate)=1
              mpar=mpar+1
              vardel(mpar)=gas(istate,1,4)
c              gas(istate,1,5)=gas(istate,1,1)
              varnam(mpar)='gas'
            endif
          else if (jgas(istate,isite).eq.4) then
            mpar=mpar+1
            vardel(mpar)=gas(istate,isite,4)
c            gas(istate,isite,5)=gas(istate,isite,1)
            varnam(mpar)='gas'
          endif
        enddo ! end isite

c.......Vogliamo provare con una mossa di linker?

        do iilink=1,nlink(istate)
          if (jlink(istate,iilink).eq.2) then !! variazioni indipendenti di phi,psi,chi
c            n=naalink(istate,iilink)
c            do iaa=1,n
              do iconf=1,nconf(istate)
                mpar=mpar+1
                varnam(mpar)='conform_lnk'
c                vardel(mpar)=phi_lnk(istate,iilink,4,iaa)
c                varnam(mpar)='phi_lnk'
c                mpar=mpar+1
c                vardel(mpar)=psi_lnk(istate,iilink,4,iaa)
c                varnam(mpar)='psi_lnk'
c                mpar=mpar+1
c                vardel(mpar)=chi_lnk(istate,iilink,4,iaa)
c                varnam(mpar)='chi_lnk'
              enddo
c            enddo
          endif
        enddo
c.......fine linker

      enddo ! end istate
      return
      end



c######################################################################## new subroutines

      subroutine pairs_equiv_new(istate,ihagg)

      include 'quafit2014.cmn'
      parameter(tiny = 1.0d-6)
      dimension rdis(mxdis+mxlink),npairs(mxsite+mxlink)
     >,rdis1(mxdis),rdis2(mxdis),rdis3(mxdis)

      iconf=1
      jconf=iconf+4
      
c.....posizioni casuali, per evitare possibili simmetrie dovute a scelte simmetriche di u_i e omega_i
      do isite=1,nsite(istate)
        coord(6*isite-5,1)=ums(istate,isite,jconf)
        ums(istate,isite,jconf)=50*(rand(0)-0.5d0)
        coord(6*isite-4,1)=aus(istate,isite,jconf)
        aus(istate,isite,jconf)=rand(0)*pi2
        coord(6*isite-3,1)=cbu(istate,isite,jconf)
        cbu(istate,isite,jconf)=rand(0)*2-1
        coord(6*isite-2,1)=aas(istate,isite,jconf)
        aas(istate,isite,jconf)=rand(0)*pi2
        coord(6*isite-1,1)=cba(istate,isite,jconf)
        cba(istate,isite,jconf)=rand(0)*2-1
        coord(6*isite-0,1)=gas(istate,isite,jconf)
        gas(istate,isite,jconf)=rand(0)*pi2
      enddo


      i1=6*isite  
      do iilink=1,nlink(istate)
        n=naalink(istate,iilink)
        do i=1,n
          i1=i1+1
          coord(i1,1)=umsaa(istate,iilink,iconf,i,1)
          umsaa(istate,iilink,iconf,i,1)=50*(rand(0)-0.5d0)
          i1=i1+1
          coord(i1,1)=ausaa(istate,iilink,iconf,i,1)
          ausaa(istate,iilink,iconf,i,1)=rand(0)*pi2
          i1=i1+1
          coord(i1,1)=cbuaa(istate,iilink,iconf,i,1)
          cbuaa(istate,iilink,iconf,i,1)=rand(0)*2-1
        enddo
      enddo

      call framhag(istate,ihagg,iconf)
      call framalllink(istate,ihagg,iconf)

      nsitlnk=nsite(istate)+nlink(istate)
      n123=nsitlnk*nsymm(istate,ihagg)*npoly(istate,ihagg)
      n12=nsitlnk*nsymm(istate,ihagg)
      n1=nsitlnk
      ndis=0
      do i=1,n1
        npairs(i)=0
      enddo
      do i=1,n123-1
        i12=1+mod(i-1,n12)
        ipoly=1+(i-i12)/(n12)
        isite=1+mod(i12-1,n1) !! non e' proprio isite
        isub=1+(i-isite-(ipoly-1)*n12)/n1
        do j=i+1,n123
          j12=1+mod(j-1,n12)
          jpoly=1+(j-j12)/(n12)
          jsite=1+mod(j12-1,n1) !! non e' proprio isite
          jsub=1+(j-jsite-(jpoly-1)*n12)/n1
          if (isite.le.nsite(istate)) then
          xi=centre(isite,isub,ipoly,1)
          yi=centre(isite,isub,ipoly,2)
          zi=centre(isite,isub,ipoly,3)
          call mat(
     >        -gamrot(isite,isub,ipoly)
     >       ,-betrot(isite,isub,ipoly)
     >       ,-alprot(isite,isub,ipoly),rot)
          else
          iilink=isite-nsite(istate)
          iii=naalink(istate,iilink)/2
          if (iii.eq.0) iii=1
          xi=centre_aa_fin(iilink,iii,isub,ipoly,1)
          yi=centre_aa_fin(iilink,iii,isub,ipoly,2)
          zi=centre_aa_fin(iilink,iii,isub,ipoly,3)
          call mat(
     >        -gamrot_lnk(isub,ipoly)
     >       ,-betrot_lnk(isub,ipoly)
     >       ,-alprot_lnk(isub,ipoly),rot)
          endif
c######################################################################
              call rotrdi(one,zer,zer,xi1,yi1,zi1,xi,yi,zi,rot)
              call rotrdi(zer,one,zer,xi2,yi2,zi2,xi,yi,zi,rot)
              call rotrdi(zer,zer,one,xi3,yi3,zi3,xi,yi,zi,rot)
c######################################################################

          if (jsite.le.nsite(istate)) then
          xj=centre(jsite,jsub,jpoly,1)
          yj=centre(jsite,jsub,jpoly,2)
          zj=centre(jsite,jsub,jpoly,3)
          call mat(
     >        -gamrot(jsite,jsub,jpoly)
     >       ,-betrot(jsite,jsub,jpoly)
     >       ,-alprot(jsite,jsub,jpoly),rot)
          else
          jilink=jsite-nsite(istate)
          jjj=naalink(istate,jilink)/2
          if (jjj.eq.0) jjj=1
          xj=centre_aa_fin(jilink,jjj,jsub,jpoly,1)
          yj=centre_aa_fin(jilink,jjj,jsub,jpoly,2)
          zj=centre_aa_fin(jilink,jjj,jsub,jpoly,3)
          call mat(
     >        -gamrot_lnk(jsub,jpoly)
     >       ,-betrot_lnk(jsub,jpoly)
     >       ,-alprot_lnk(jsub,jpoly),rot)
          endif
c######################################################################
              call rotrdi(one,zer,zer,xj1,yj1,zj1,xj,yj,zj,rot)
              call rotrdi(zer,one,zer,xj2,yj2,zj2,xj,yj,zj,rot)
              call rotrdi(zer,zer,one,xj3,yj3,zj3,xj,yj,zj,rot)
c######################################################################
          call cpolar(xj-xi,yj-yi,zj-zi,r,a,b)
c######################################################################
          call cpolar(xj1-xi1,yj1-yi1,zj1-zi1,r1,a,b)
          call cpolar(xj2-xi2,yj2-yi2,zj2-zi2,r2,a,b)
          call cpolar(xj3-xi3,yj3-yi3,zj3-zi3,r3,a,b)
c######################################################################
          i0=0
          do k=1,ndis
            if (dabs(r-rdis(k)).le.tiny
c######################################################################
     >          .and.(r1-rdis1(k)).le.tiny
     >          .and.(r2-rdis2(k)).le.tiny
     >          .and.(r3-rdis3(k)).le.tiny
c######################################################################
     >) then
              i0=1
              ipair(istate,ihagg,k,7)=ipair(istate,ihagg,k,7)+1
            endif
          enddo
          if (i0.eq.0) then
c###################################################################### escludo siti connessi
cc          write(*,*) ' ZZZ ',i0,idisvconn,isub,jsub,isite,jsite
            i1=0
c            if (idisvconn.eq.1) then
c              if (isub.eq.jsub) then
c                if (abs(jsite-isite).eq.1) then !! sono adiacenti
c                    dum2=dnextsite(istate,min(isite,jsite))
c                    if (dum2.ne.0.) then !! sono connessi
c                      write(*,*) isite,jsite
c                      i1=1
c                    endif
c                endif
c              endif
c            endif
c######################################################################
            ndis=ndis+1
            if (ndis.gt.mxdis) stop 'mxdis'
            npairs(isite)=npairs(isite)+1
            write(*,*) ' a ',istate,ihagg,isite,npairs(isite)
            ipair_site(istate,ihagg,isite,npairs(isite))=ndis
            if (isite.ne.jsite) then
              npairs(jsite)=npairs(jsite)+1
            write(*,*) ' b ',istate,ihagg,jsite,npairs(jsite)
              ipair_site(istate,ihagg,jsite,npairs(jsite))=ndis
            endif
            rdis(ndis)=r
c######################################################################
            rdis1(ndis)=r1
            rdis2(ndis)=r2
            rdis3(ndis)=r3
c######################################################################
            iconn=0
            if (isite.le.nsite(istate).and.jsite.le.nsite(istate)) then
              i101=1 !coppia domain-domain
            else
     >      if (isite.le.nsite(istate).and.jsite.gt.nsite(istate)) then
              jilink=jsite-nsite(istate)
              do iend=1,2
               if (isite.eq.isitelink(istate,jilink,2*iend-1)) iconn=1
              enddo
              i101=2 !coppia domain-linker
            else 
     >      if (jsite.le.nsite(istate).and.isite.gt.nsite(istate)) then
              iilink=isite-nsite(istate)
              do iend=1,2
               if (jsite.eq.isitelink(istate,iilink,2*iend-1)) iconn=1
              enddo
              i101=3 !coppia linker-domain
            else            
              i101=4 !coppia linker-linker
            endif            
            ipair(istate,ihagg,ndis,1)=isite
            ipair(istate,ihagg,ndis,2)=isub
            ipair(istate,ihagg,ndis,3)=ipoly
            ipair(istate,ihagg,ndis,4)=jsite
            ipair(istate,ihagg,ndis,5)=jsub
            ipair(istate,ihagg,ndis,6)=jpoly
            ipair(istate,ihagg,ndis,7)=1
            ipair(istate,ihagg,ndis,8)=iconn
            ipair(istate,ihagg,ndis,9)=i101 !tipo di coppia (1-4)
          endif
        enddo
      enddo
      np=npairs(1)
      do i=2,n1
        write(*,*) i,npairs(i),np
        if (npairs(i).ne.np) stop 'pairs '
      enddo
      npair_site(istate,ihagg)=np
      ndist(istate,ihagg)=ndis
      m=0
      do k=1,ndis
        write(*,10) k,') ',
     >  ipair(istate,ihagg,k,1),',',
     >  ipair(istate,ihagg,k,2),',',
     >  ipair(istate,ihagg,k,3),' -> ',
     >  ipair(istate,ihagg,k,4),',',
     >  ipair(istate,ihagg,k,5),',',
     >  ipair(istate,ihagg,k,6),' m= ',
     >  ipair(istate,ihagg,k,7)
        m=m+ipair(istate,ihagg,k,7)
      enddo
      write(*,*) ' #pairs ',m,(n123*(n123-1))/2
      do k=1,np
        write(*,11) k,') ',
     >  (ipair_site(istate,ihagg,isite,k),isite=1,n1)
      enddo

c.....ripristino
      do isite=1,nsite(istate)
        ums(istate,isite,jconf)=coord(6*isite-5,1)
        aus(istate,isite,jconf)=coord(6*isite-4,1)
        cbu(istate,isite,jconf)=coord(6*isite-3,1)
        aas(istate,isite,jconf)=coord(6*isite-2,1)
        cba(istate,isite,jconf)=coord(6*isite-1,1)
        gas(istate,isite,jconf)=coord(6*isite-0,1)
      enddo
      i1=6*isite  
      do iilink=1,nlink(istate)
        n=naalink(istate,iilink)
        do i=1,n
          i1=i1+1
          umsaa(istate,iilink,iconf,i,1)=coord(i1,1)
          i1=i1+1
          ausaa(istate,iilink,iconf,i,1)=coord(i1,1)
          i1=i1+1
          cbuaa(istate,iilink,iconf,i,1)=coord(i1,1)
        enddo
      enddo

      return
 10   format(1000(i4,a))
 11   format(i5,a,1000(i5))
      end

      subroutine opsymm

c.....In questa versione ho eliminato tutti i piani di simmetria,
c.....cioe' tutte le rotazioni improprie

      include 'quafit2014.cmn'
      complex*16 snor,wigner
      character*4 grup(3+3)
      parameter(tiny    = 1.e-4)

      data (grup(i),i=1,3+3)/
     >'Dn  ',
     >'Cn  ',
     >'C1  ',
     >'T   ',
     >'O   ',
     >'I   '/

c      write(*,*) '   ---> opsymm   '

c.....Symmetry operations
      do istate=1,nstate
      do ihagg=1,nhagg(istate)

        alp(1)=0.d0
        bet(1)=0.d0
        gam(1)=0.d0
        inv(1)=1
        nseq=0
        lmaxs=larr
        do 30 n=2,lmaxs+1
          do 34 igr=1,3+3
            if (n.eq.lmaxs+1) then
              if (igr.le.2) goto 34
            else
              if (igr.gt.2) goto 34
            endif
            nseq=nseq+1
            write(unit=ch4,fmt='(a4)') grup(igr)
            if (igr.le.2) then
              if (n.le.9) then
                write(unit=ch4(2:2),fmt='(i1)') n
              else
                ch4(4:4)=ch4(3:3)
                write(unit=ch4(2:3),fmt='(i2)') n
              endif
            endif
c...........Riconosce il gruppo...
c            write(*,*) ' ch4 ggss(istate,ihagg)',ch4, ' ',ggss,igr
            if (ch4.ne.ggss(istate,ihagg)) goto 34

          alp(1)=0.d0
          bet(1)=0.d0
          gam(1)=0.d0
 
          goto (1,2,3,4,5,6) igr

 1        call dn_2(alp,bet,gam,n,nh)
              goto 88
 2        call cn_2(alp,bet,gam,n,nh)
              goto 88
 3        nh=1          
              goto 88
 4        call tt_2(alp,bet,gam,nh)
              goto 88
 5        call oo_2(alp,bet,gam,nh)
              goto 88
 6        call ii_2(alp,bet,gam,nh)
              goto 88

 88           continue

            nsub=nh
            if (nsub.eq.0) stop 'nsub.eq.0'
            if (nsub.gt.mxsub) stop 'nsub.gt.mxsub'
            nsymm(istate,ihagg)=nsub
            do k=1,nsub
              alpsym(istate,ihagg,k)=alp(k)
              betsym(istate,ihagg,k)=bet(k)
              gamsym(istate,ihagg,k)=gam(k)
c              write(71,*) k ,alp(k),bet(k),gam(k)
            enddo

          do 110 l=0,larr
            do 110 m=-l,l
              i1=0
              do nn=-l,l
                snor=dcmplx(0.d0,0.d0)
                do ih=1,nh              
                  call wig(-gam(ih),-bet(ih),-alp(ih),l,m,nn,wigner)
                  snor=snor+dconjg(wigner)
                enddo
c                snor=snor/nh
              if (dabs(dreal(snor*dconjg(snor))).gt.tiny) then
                i1=1
c                write(71,*) l,m,nn,snor
                proj(istate,ihagg,l,m,nn)=dreal(snor)
              else
                proj(istate,ihagg,l,m,nn)=0.d0
              endif
            enddo
            iproj(istate,ihagg,l,m)=i1
c            if (dum1.gt.tiny) write(1,*) " s ",dum1
110       continue


34        continue
30      continue
      enddo
      enddo
      return

100   format('Operation # ',i3,3(/,f10.3))
200   format('p # ',i3,' f # ',i3,': Im f[',i3,',',i3,']')
300   format(6x,'data jseq(',i3,'),(iseq(',i3,',k),k=1,',i3,')/')
1500  format(6x,'data jpsq(',i3,'),(ipsq(',i3,',k),k=1,',i3,')/')
500   format(5x,'>',16(i3,a1))
600   format(a70)
700   format(6x,'data symb(',i3,',  1)/''',a4,'''/')
400   format('Sequence # ',i3,'. ',a4)
1300  format(6x,'data symb(',i3,',',i3,')/''',a4,'''/')
1400  format(100(5x,'>',16(i3,a1),/))
1401  format('      data nseq/',i3,'/')
1402  format('      data mxdeg/',i3,'/')

c tex format

7400  format('${\\cal ',a,'}_{',a,a,'}$ &','$',i4,'$')
7100  format('& $',a4,' f_{',i3,',',i3,'}$')
7200  format('\\\\ &')
7300  format(20(a1))
7500  format('\\\\ \\cline{1-',i3,'}')

7600  format(
     >'%',/,
     >'\\begin{centering}',/,
     >'\\begin{table}',/,
     >'\\begin{tabular}{||l|r|ccccccccc||}',/,
     >'\\cline{1-',i3,'}',/,
     >'${\\cal G}$ & $\\npar$ & $f_{l,m}$&&&&&&&&\\\\ ',/,
     >'\\cline{1-',i3,'}')
7800  format(
     >'\\end{tabular}',/,
     >'\\caption{description...}',/,
     >'\\label{tab',i1,'}',/,
     >'\\end{table}'
     >,/,'\\end{centering}')
      end


c#####################################################################
      subroutine cgamz_2(alp,bet,gam,gamma)
      implicit real*8 (a-h,o-z)
      alp=gamma
      bet=0.d0
      gam=0.d0
      return
      end

      subroutine c2phi_2(alp,bet,gam,phi)
      implicit real*8 (a-h,o-z)
      parameter(pi = 6.28318530717958647692528676656d0/2)
      alp=phi*2
      bet=pi
      gam=pi
      return
      end


c######################################################################
      subroutine cn_2(alp,bet,gam,n,nh)
      implicit real*8 (a-h,o-z)
      dimension alp(60),bet(60),gam(60)
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)

      nh=n
      dum1=pi2/dfloat(n)
      dum2=0.d0
      do 10 i=1,n-1
        dum2=dum2+dum1
        call cgamz_2(alp(i+1),bet(i+1),gam(i+1),dum2)
10    continue

      return
      end


      subroutine dn_2(alp,bet,gam,n,nh)
      implicit real*8 (a-h,o-z)
      dimension alp(60),bet(60),gam(60)
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)

      nh=2*n
c.....n-1 Cn
      dum1=pi/dfloat(2*n)
      dum2=2.d0*dum1
      dum3=4.d0*dum1
      dum4=0.d0
      do 10 i=1,n-1
        dum4=dum4+dum3
        call cgamz_2(alp(i+1),bet(i+1),gam(i+1),dum4)
10    continue
c.....n C2 
      dum4=dum1-dum2
      do 40 i=1,n
        dum4=dum4+dum2
        call c2phi_2(alp(i+n),bet(i+n),gam(i+n),dum4)
40    continue

      return
      end


      subroutine tt_2(alp,bet,gam,nh)

      implicit real*8 (a-h,o-z)
      complex*16 ris,snor,scmp
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)
      dimension alp(60),bet(60),gam(60)

      nh=12
c.....Assi c3
      call prop_2(1.d0,1.d0,1.d0,pi2/3.d0,alp(2),bet(2),gam(2))
      call prop_2(1.d0,1.d0,1.d0,2.d0*pi2/3.d0,alp(3),bet(3),gam(3))
      call prop_2(-1.d0,-1.d0,1.d0,pi2/3.d0,alp(4),bet(4),gam(4))
      call prop_2(-1.d0,-1.d0,1.d0,2.d0*pi2/3.d0,alp(5),bet(5),gam(5))
      call prop_2(-1.d0,1.d0,-1.d0,pi2/3.d0,alp(6),bet(6),gam(6))
      call prop_2(-1.d0,1.d0,-1.d0,2.d0*pi2/3.d0,alp(7),bet(7),gam(7))
      call prop_2(1.d0,-1.d0,-1.d0,pi2/3.d0,alp(8),bet(8),gam(8))
      call prop_2(1.d0,-1.d0,-1.d0,2.d0*pi2/3.d0,alp(9),bet(9),gam(9))
c.....Assi C2
      call prop_2(1.d0,0.d0,0.d0,pi,alp(10),bet(10),gam(10))
      call prop_2(0.d0,1.d0,0.d0,pi,alp(11),bet(11),gam(11))
      call prop_2(0.d0,0.d0,1.d0,pi,alp(12),bet(12),gam(12))
      return
      end


      subroutine oo_2(alp,bet,gam,nh)

      implicit real*8 (a-h,o-z)
      complex*16 ris,snor,scmp
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)
      dimension alp(60),bet(60),gam(60)

c.....O

      nh=24
c.....Assi C4
      call prop_2(1.d0,0.d0,0.d0,pi,alp(2),bet(2),gam(2))
      call prop_2(0.d0,1.d0,0.d0,pi,alp(3),bet(3),gam(3))
      call prop_2(0.d0,0.d0,1.d0,pi,alp(4),bet(4),gam(4))
      call prop_2(1.d0,0.d0,0.d0,pi/2.d0,alp(5),bet(5),gam(5))
      call prop_2(1.d0,0.d0,0.d0,3.d0*pi/2.d0,alp(6),bet(6),gam(6))
      call prop_2(0.d0,1.d0,0.d0,pi/2.d0,alp(7),bet(7),gam(7))
      call prop_2(0.d0,1.d0,0.d0,3.d0*pi/2.d0,alp(8),bet(8),gam(8))
      call prop_2(0.d0,0.d0,1.d0,pi/2.d0,alp(9),bet(9),gam(9))
      call prop_2(0.d0,0.d0,1.d0,3.d0*pi/2.d0,alp(10),bet(10),gam(10))
c.....Assi C2
      call prop_2( 1.d0,1.d0,0.d0,pi,alp(11),bet(11),gam(11))
      call prop_2(-1.d0,1.d0,0.d0,pi,alp(12),bet(12),gam(12))
      call prop_2( 1.d0,0.d0,1.d0,pi,alp(13),bet(13),gam(13))
      call prop_2(-1.d0,0.d0,1.d0,pi,alp(14),bet(14),gam(14))
      call prop_2( 0.d0,1.d0,1.d0,pi,alp(15),bet(15),gam(15))
      call prop_2( 0.d0,1.d0,-1.d0,pi,alp(16),bet(16),gam(16))
c.....Assi C3
      call prop_2( 1.d0, 1.d0, 1.d0,pi2/3.d0,alp(17),bet(17),gam(17))
      call prop_2( 1.d0, 1.d0, 1.d0,2.d0*pi2/3.d0,alp(18),bet(18),
     >       gam(18))
      call prop_2(-1.d0, 1.d0, 1.d0,pi2/3.d0,alp(19),bet(19),gam(19))
      call prop_2(-1.d0, 1.d0, 1.d0,2.d0*pi2/3.d0,alp(20),bet(20),
     >       gam(20))
      call prop_2( 1.d0,-1.d0, 1.d0,pi2/3.d0,alp(21),bet(21),gam(21))
      call prop_2( 1.d0,-1.d0, 1.d0,2.d0*pi2/3.d0,alp(22),bet(22),
     >       gam(22))
      call prop_2( 1.d0, 1.d0,-1.d0,pi2/3.d0,alp(23),bet(23),gam(23))
      call prop_2( 1.d0, 1.d0,-1.d0,2.d0*pi2/3.d0,alp(24),bet(24),
     >       gam(24))

      return
      end

      subroutine ii_2(alp,bet,gam,nh)

      implicit real*8 (a-h,o-z)
      complex*16 ris,snor,scmp
      parameter(pi2    = 6.28318530717958647692528676656d0)
      parameter(pi     = pi2*0.5d0)
      dimension alp(60),bet(60),gam(60)

c.....I

      nh=60
c.....Assi C5
      call prop_2( .0000000D+00,   .0000000D+00,   .9510565D+00,
     >pi2/5.d0,alp(2),bet(2),gam(2))
      call prop_2( .0000000D+00,   .0000000D+00,   .9510565D+00,
     >2.d0*pi2/5.d0,alp(3),bet(3),gam(3))
      call prop_2( .0000000D+00,   .0000000D+00,   .9510565D+00,
     >3.d0*pi2/5.d0,alp(4),bet(4),gam(4))
      call prop_2( .0000000D+00,   .0000000D+00,   .9510565D+00,
     >4.d0*pi2/5.d0,alp(5),bet(5),gam(5))

      call prop_2( .8506508D+00,   .0000000D+00,   .4253254D+00,
     >pi2/5.d0,alp(6),bet(6),gam(6))
      call prop_2( .8506508D+00,   .0000000D+00,   .4253254D+00,
     >2.d0*pi2/5.d0,alp(7),bet(7),gam(7))
      call prop_2( .8506508D+00,   .0000000D+00,   .4253254D+00,
     >3.d0*pi2/5.d0,alp(8),bet(8),gam(8))
      call prop_2( .8506508D+00,   .0000000D+00,   .4253254D+00,
     >4.d0*pi2/5.d0,alp(9),bet(9),gam(9))

      call prop_2( .2628656D+00,   .8090170D+00,   .4253254D+00,
     >pi2/5.d0,alp(10),bet(10),gam(10))
      call prop_2( .2628656D+00,   .8090170D+00,   .4253254D+00,
     >2.d0*pi2/5.d0,alp(11),bet(11),gam(11))
      call prop_2( .2628656D+00,   .8090170D+00,   .4253254D+00,
     >3.d0*pi2/5.d0,alp(12),bet(12),gam(12))
      call prop_2( .2628656D+00,   .8090170D+00,   .4253254D+00,
     >4.d0*pi2/5.d0,alp(13),bet(13),gam(13))

      call prop_2(-.6881910D+00,   .5000000D+00,   .4253254D+00,
     >pi2/5.d0,alp(14),bet(14),gam(14))
      call prop_2(-.6881910D+00,   .5000000D+00,   .4253254D+00,
     >2.d0*pi2/5.d0,alp(15),bet(15),gam(15))
      call prop_2(-.6881910D+00,   .5000000D+00,   .4253254D+00,
     >3.d0*pi2/5.d0,alp(16),bet(16),gam(16))
      call prop_2(-.6881910D+00,   .5000000D+00,   .4253254D+00,
     >4.d0*pi2/5.d0,alp(17),bet(17),gam(17))

      call prop_2(-.6881910D+00,  -.5000000D+00,   .4253254D+00,
     >pi2/5.d0,alp(18),bet(18),gam(18))
      call prop_2(-.6881910D+00,  -.5000000D+00,   .4253254D+00,
     >2.d0*pi2/5.d0,alp(19),bet(19),gam(19))
      call prop_2(-.6881910D+00,  -.5000000D+00,   .4253254D+00,
     >3.d0*pi2/5.d0,alp(20),bet(20),gam(20))
      call prop_2(-.6881910D+00,  -.5000000D+00,   .4253254D+00,
     >4.d0*pi2/5.d0,alp(21),bet(21),gam(21))

      call prop_2( .2628656D+00,  -.8090170D+00,   .4253254D+00,
     >pi2/5.d0,alp(22),bet(22),gam(22))
      call prop_2( .2628656D+00,  -.8090170D+00,   .4253254D+00,
     >2.d0*pi2/5.d0,alp(23),bet(23),gam(23))
      call prop_2( .2628656D+00,  -.8090170D+00,   .4253254D+00,
     >3.d0*pi2/5.d0,alp(24),bet(24),gam(24))
      call prop_2( .2628656D+00,  -.8090170D+00,   .4253254D+00,
     >4.d0*pi2/5.d0,alp(25),bet(25),gam(25))

c.....Assi C3
      call prop_2( .4587940D+00,   .0000000D+00,  -.6005691D+00,
     >     pi2/ 3.d0,alp(26),bet(26),gam(26))
      call prop_2( .4587940D+00,   .0000000D+00,  -.6005691D+00,
     >2.d0*pi2/ 3.d0,alp(27),bet(27),gam(27))

      call prop_2( .1417751D+00,   .4363390D+00,  -.6005691D+00,
     >     pi2/ 3.d0,alp(28),bet(28),gam(28))
      call prop_2( .1417751D+00,   .4363390D+00,  -.6005691D+00,
     >2.d0*pi2/ 3.d0,alp(29),bet(29),gam(29))

      call prop_2(-.3711721D+00,   .2696723D+00,  -.6005691D+00,
     >     pi2/ 3.d0,alp(30),bet(30),gam(30))
      call prop_2(-.3711721D+00,   .2696723D+00,  -.6005691D+00,
     >2.d0*pi2/ 3.d0,alp(31),bet(31),gam(31))

      call prop_2(-.3711721D+00,  -.2696723D+00,  -.6005691D+00,
     >     pi2/ 3.d0,alp(32),bet(32),gam(32))
      call prop_2(-.3711721D+00,  -.2696723D+00,  -.6005691D+00,
     >2.d0*pi2/ 3.d0,alp(33),bet(33),gam(33))

      call prop_2( .1417751D+00,  -.4363390D+00,  -.6005691D+00,
     >     pi2/ 3.d0,alp(34),bet(34),gam(34))
      call prop_2( .1417751D+00,  -.4363390D+00,  -.6005691D+00,
     >2.d0*pi2/ 3.d0,alp(35),bet(35),gam(35))


      call prop_2( .7423442D+00,   .0000000D+00,  -.1417751D+00,
     >     pi2/ 3.d0,alp(36),bet(36),gam(36))
      call prop_2( .7423442D+00,   .0000000D+00,  -.1417751D+00,
     >2.d0*pi2/ 3.d0,alp(37),bet(37),gam(37))

      call prop_2( .2293970D+00,   .7060113D+00,  -.1417751D+00,
     >     pi2/ 3.d0,alp(38),bet(38),gam(38))
      call prop_2( .2293970D+00,   .7060113D+00,  -.1417751D+00,
     >2.d0*pi2/ 3.d0,alp(39),bet(39),gam(39))

      call prop_2(-.6005691D+00,   .4363390D+00,  -.1417751D+00,
     >     pi2/ 3.d0,alp(40),bet(40),gam(40))
      call prop_2(-.6005691D+00,   .4363390D+00,  -.1417751D+00,
     >2.d0*pi2/ 3.d0,alp(41),bet(41),gam(41))

      call prop_2(-.6005691D+00,  -.4363390D+00,  -.1417751D+00,
     >     pi2/ 3.d0,alp(42),bet(42),gam(42))
      call prop_2(-.6005691D+00,  -.4363390D+00,  -.1417751D+00,
     >2.d0*pi2/ 3.d0,alp(43),bet(43),gam(43))

      call prop_2( .2293970D+00,  -.7060113D+00,  -.1417751D+00,
     >     pi2/ 3.d0,alp(44),bet(44),gam(44))
      call prop_2( .2293970D+00,  -.7060113D+00,  -.1417751D+00,
     >2.d0*pi2/ 3.d0,alp(45),bet(45),gam(45))

c.....Assi C2
      call prop_2( .4253254D+00,   .0000000D+00,   .6881910D+00,
     >     pi       ,alp(46),bet(46),gam(46))
      call prop_2( .1314328D+00,   .4045085D+00,   .6881910D+00,
     >     pi       ,alp(47),bet(47),gam(47))
      call prop_2(-.3440955D+00,   .2500000D+00,   .6881910D+00,
     >     pi       ,alp(48),bet(48),gam(48))
      call prop_2(-.3440955D+00,  -.2500000D+00,   .6881910D+00,
     >     pi       ,alp(49),bet(49),gam(49))
      call prop_2( .1314328D+00,  -.4045085D+00,   .6881910D+00,
     >     pi       ,alp(50),bet(50),gam(50))
      call prop_2( .5567582D+00,   .4045085D+00,   .4253254D+00,
     >     pi       ,alp(51),bet(51),gam(51))
      call prop_2( .5567582D+00,  -.4045085D+00,   .4253254D+00,
     >     pi       ,alp(52),bet(52),gam(52))
      call prop_2( .7694209D+00,   .2500000D+00,   .5551115D-16,
     >     pi       ,alp(53),bet(53),gam(53))
      call prop_2( .7694209D+00,  -.2500000D+00,   .5551115D-16,
     >     pi       ,alp(54),bet(54),gam(54))
      call prop_2(-.2126627D+00,   .6545085D+00,   .4253254D+00,
     >     pi       ,alp(55),bet(55),gam(55))
      call prop_2( .4755283D+00,   .6545085D+00,   .5551115D-16,
     >     pi       ,alp(56),bet(56),gam(56))
      call prop_2( .2775558D-16,   .8090170D+00,   .5551115D-16,
     >     pi       ,alp(57),bet(57),gam(57))
      call prop_2(-.6881910D+00,   .1110223D-15,   .4253254D+00,
     >     pi       ,alp(58),bet(58),gam(58))
      call prop_2(-.4755283D+00,   .6545085D+00,   .5551115D-16,
     >     pi       ,alp(59),bet(59),gam(59))
      call prop_2(-.2126627D+00,  -.6545085D+00,   .4253254D+00,
     >     pi       ,alp(60),bet(60),gam(60))

      return
      end

      subroutine prop_2(x,y,z,theta,alpha,beta,gamma)

      implicit real*8 (a-h,o-z)
      dimension rot(3,3)

      dum1 = 1.d0/dsqrt(x*x+y*y+z*z)
c.....Componenti versore
      ux = dum1*x
      uy = dum1*y
      uz = dum1*z
c.....Calcolo fattori costanti
c      dum2 = dsin(theta)
      dum2 = -dsin(theta) !cosi' ruota il sistema...
      dum1 = 1.d0-dcos(theta)
      dum3 = ux*uy*dum1
      dum4 = ux*uz*dum1
      dum5 = uy*uz*dum1
      dum6 = ux*dum2
      dum7 = uy*dum2
      dum8 = uz*dum2
c.....Definizione matrice rot
c     alpha e beta angoli polari versore legame
c     rot = r(0,-beta,-alpha)*r(alpha,beta,-phi(iang))
c     Si ha:
c     rot(1,1) = 1+(ux*ux-1)[1-cos(phi)]
c     rot(1,2) = ux*uy[1-cos(phi)]-uz*sin(phi)
c     rot(1,3) = ux*uz[1-cos(phi)]+uy*sin(phi)
c     rot(2,1) = ux*uy[1-cos(phi)]+uz*sin(phi)
c     rot(2,2) = 1+(uy*uy-1)[1-cos(phi)]
c     rot(2,3) = uy*uz[1-cos(phi)]-ux*sin(phi)
c     rot(3,1) = ux*uz[1-cos(phi)]-uy*sin(phi)
c     rot(3,2) = uy*uz[1-cos(phi)]+ux*sin(phi)
c     rot(3,3) = 1+(uz*uz-1)[1-cos(phi)] 
c.....Assegnazione elementi matrice rot
      rot(1,1) = 1.d0+(ux*ux-1.d0)*dum1
      rot(1,2) = dum3-dum8     
      rot(1,3) = dum4+dum7     
      rot(2,1) = dum3+dum8     
      rot(2,2) = 1.d0+(uy*uy-1.d0)*dum1
      rot(2,3) = dum5-dum6     
      rot(3,1) = dum4-dum7     
      rot(3,2) = dum5+dum6     
      rot(3,3) = 1.d0+(uz*uz-1.d0)*dum1
      cb = rot(3,3)
      if (cb.gt. 1.d0) cb= 1.d0
      if (cb.lt.-1.d0) cb=-1.d0
      if (dabs(cb).ge.1.d0) then
        sb=0.d0
        cg=rot(1,1)/cb
        sg=rot(1,2)/cb
        ca=1.d0
        sa=0.d0
c        ca=rot(1,1)/cb
c        sa=rot(1,2)/cb
c        cg=1.d0
c        sg=0.d0
      else
        sb=sfromc(cb)
        cg=rot(3,1)/sb
        sg=rot(3,2)/sb
        ca=-rot(1,3)/sb
        sa=rot(2,3)/sb
c        ca=rot(3,1)/sb
c        sa=rot(3,2)/sb
c        cg=-rot(1,3)/sb
c        sg=rot(2,3)/sb
      endif
      alpha=polar(ca,sa)
      beta =dacos(cb)
      gamma=polar(cg,sg)

      return
100   format(10(f7.3))
      end



      subroutine wig(alpha,beta,gamma,l,m,n,wigner)
      implicit real*8 (a-h,o-z)
      complex*16 wigner

      wigner=dcmplx(dcos(m*alpha),-dsin(m*alpha))
     >   *pwign(l,m,n,beta)
     >   *dcmplx(dcos(n*gamma),-dsin(n*gamma))
      return
      end


      subroutine parconst(istate,isite,ihagg,iconf)  ! e' la nuova versione

      include 'quafit2014.cmn'

      if (nexper.eq.0) return
c      write(*,*) '   ---> parconst   ',istate,isite,ihagg,iconf
c      l=2
c      m=-1
c      iq=150
c      write(102,*) 0,almstore(1,iq,l,m,1)
      call f0f1(istate,isite,ihagg,iconf,ia)
c      write(102,*) 1,almstore(ia,iq,l,m,1),ia
      call f1f2(istate,ihagg,ia,ib)
c      write(102,*) 2,almstore(ib,iq,l,m,1),ia,ib
      call f2f4(istate,isite,ihagg,ib,ia)
c      write(102,*) 4,almstore(ia,iq,l,m,1),ib,ia
      call f4f5(istate,ihagg,ia,ib)
c      write(102,*) 5,almstore(ib,iq,l,m,1),ia,ib
      call f5f6(istate,ihagg,iconf,ib,ia)
c      write(102,*) 6,almstore(ia,iq,l,m,1),ib,ia
      call f6f7(istate,ihagg,iconf,ia,ib)
c      write(102,*) 7,almstore(ib,iq,l,m,1),ia,ib
      call f7f8(istate,ihagg,iconf,ib)
c      write(102,*) 8,almstore(ibfinal,iq,l,m,1),ib,ibfinal


      call jseque(iu_ful,istate,isite,ihagg,iconf,3)

      do iq=1,nqfull
        q=qqfull(iq)
        write(unit=iu_ful,rec=iq,fmt=1007) q,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
      enddo
      return
 1007 format(500000(e15.6E3))
      end

      subroutine cp_f_cst(istate,isitlnk,ihagg,iconf) !copy  fXXX.cst to *.csw  

      include 'quafit2014.cmn'

      if (nexper.eq.0) return
c      write(*,*) '   ---> cp_f_cst   ',istate,isitlnk,ihagg,iconf

      call jseque(iu_ful,istate,isitlnk,ihagg,iconf,3)
      call jseque(iu_cpf,istate,isitlnk,ihagg,iconf,4)

c      ist=isitlnk+(nsite(istate)+nlink(istate))*(iconf-1+nconf(istate)*
c     >    (ihagg-1+nhagg(istate)*(istate-1)))

c      write(*,*) iu_ful,iu_cpf
      do iq=1,nqfull
        read (unit=iu_ful,rec=iq,fmt=1007) q,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
        write(unit=iu_cpf,rec=iq,fmt=1007) q,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
      enddo

      return
 1007 format(500000(e15.6E3))
      end

      subroutine re_f_cst(istate,isitlnk,ihagg,iconf) !restore  fXXX.cst from *.csw  

      include 'quafit2014.cmn'

      if (nexper.eq.0) return
c      write(*,*) '   ---> re_f_cst   '

      call jseque(iu_ful,istate,isitlnk,ihagg,iconf,3)
      call jseque(iu_cpf,istate,isitlnk,ihagg,iconf,4)

c      ist=isite+(nsite(istate)+nlink(istate))*(iconf-1+nconf(istate)*
c     >    (ihagg-1+nhagg(istate)*(istate-1)))

      do iq=1,nqfull
        read (unit=iu_cpf,rec=iq,fmt=1007) q,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
        write(unit=iu_ful,rec=iq,fmt=1007) q,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
      enddo

      return
 1007 format(500000(e15.6E3))
      end

c################################################################################
      subroutine etoma(l,x,k)

      include 'quafit2014.cmn'
        
c.....calcola exp(-i m x) per m tra -l e +l      
      if (l.gt.larr*(mxpoly-1)) stop 'etoma l'
      if (k.gt.mxp) stop 'etoma k'
      eam(0,k)=dcmplx(1.d0,0.d0)
      dum1=0.d0
      do m=1,l
        dum1=dum1+x
        s1=dsin(dum1)
        c1=dcos(dum1)
        eam(m,k)=dcmplx(c1,-s1)
        eam(-m,k)=dcmplx(c1,s1)
      enddo
      return
      end




      subroutine f2f4_lnk_new(istate,ihagg,ia,ib)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc,phismall
c      write(*,*) '   ---> f2f4_lnk_new   '
      zerc=dcmplx(0.d0,0.d0)

      nss=nsfull+nwave+8

      alp1=aesselnk !! quindi devi aver chiamato framhag
      bet1=besselnk  !tutto serve per avere l'opposto di <u>G (che prima chiamavi essea)
      u=uesselnk
      i2=2!2
c      write(*,*) u,alp1,bet1
      ib=mxpdb+4

      call pw_go_m0(i2*lfirst,bet1,3)
      call etoma(i2*lfirst,alp1,5)

      do l1=0,ldecidi!lfirst
        do m1=-l1,l1
          do iq=1,nqfull
            do is=1,nss
              almstore(ib,iq,l1,m1,is)=zerc 
            enddo
          enddo
          if (iproj(istate,ihagg,l1,m1).eq.1) then
            do n1=-l1,l1
              dum3=proj(istate,ihagg,l1,n1,m1)
              if (dum3.ne.0.d0) then
                do iq=1,nqfull
                  x=qqfull(iq)*u
                  do l2=0,lfirst
                    do m2=-l2,l2
                      dum1=phismall(x,i2*lfirst,l1,l2,n1,m2,5,3)
                      if (dreal(dum1*dconjg(dum1)).lt.1.e-8) goto 2020
                      do is=1,nss
                        almstore(ib,iq,l1,m1,is)=
     >                          almstore(ib,iq,l1,m1,is)
     >                         +almstore(ia,iq,l2,m2,is)*dum1*dum3
                      enddo
 2020                 continue
                    enddo!m2
                  enddo!l2
                enddo !iq
              endif
            enddo !n1
          endif
        enddo !m1
      enddo !l1

      return
      end

      subroutine f0f1(istate,isite,ihagg,iconf,ia)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc
      zerc=dcmplx(0.d0,0.d0)

c      write(*,*) '   ---> f0f1   '
      jconf=iconf+4
      nss=nsfull+nwave+8
      ipdb=ipdbsite(istate,isite)

      alp1=aas(istate,isite,jconf)
      bet1=dacos(cba(istate,isite,jconf))
      gam1=gas(istate,isite,jconf)

      if (alp1.eq.zer.and.bet1.eq.zer.and.gam1.eq.zer) then
        ia=ipdb
c        write(*,*) ' f0f1 exit'
        return
      endif
      ia=mxpdb+1

      call pw_go_mn(lfirst,bet1,1)
      call etoma(lfirst,alp1,1)
      call etoma(lfirst,gam1,2)
c       write(*,*) alp1,bet1,gam1
c       write(*,*) alp1*conv,bet1*conv,gam1*conv
c       write(*,*) (eam(m,1),m=-5,5)
c       write(*,*) (eam(m,2),m=-5,5)
      do l=0,lfirst
        do m=-l,l
          do iq=1,nqfull
            do is=1,nss
              almstore(ia,iq,l,m,is)=zerc
            enddo
          enddo
          do n=-l,l
            dum2=eam(m,1)*pwar(l,m,n,1)*eam(n,2)
c.sembra che cosi' funziona bene            dum2=dconjg(dum2)  !! OK
            do iq=1,nqfull
              do is=1,nss
                almstore(ia,iq,l,m,is)=almstore(ia,iq,l,m,is)
     >                         +dum2*almstore(ipdb,iq,l,n,is)
              enddo
            enddo
          enddo!n
        enddo!m
      enddo!l
      return
      end

      subroutine f1f2(istate,ihagg,ia,ib)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc
      zerc=dcmplx(0.d0,0.d0)

c      write(*,*) '   ---> f1f2   '
      nss=nsfull+nwave+8

      alp1=agga(istate,ihagg)
      bet1=aggb(istate,ihagg)
      gam1=aggg(istate,ihagg)

c      write(*,*) ' tardi ',alp1*conv,bet1*conv,gam1*conv
      if (alp1.eq.zer.and.bet1.eq.zer.and.gam1.eq.zer) then
        ib=ia
c        write(*,*) ' exit f1f2'
        return
      endif
      ib=mxpdb+2

      call pw_go_mn(lfirst,bet1,2)
      call etoma(lfirst,alp1,3)
      call etoma(lfirst,gam1,4)


      do l=0,lfirst
        do m=-l,l
          do iq=1,nqfull
            do is=1,nss
              almstore(ib,iq,l,m,is)=zerc
            enddo
          enddo
          do n=-l,l
            dum2=eam(n,3)*pwar(l,n,m,2)*eam(m,4)
            dum2=dconjg(dum2)
            do iq=1,nqfull
              do is=1,nss
                almstore(ib,iq,l,m,is)=almstore(ib,iq,l,m,is)
     >                         +dum2*almstore(ia,iq,l,n,is)
              enddo
            enddo
          enddo!n
        enddo!m
      enddo!l

      return
      end

      subroutine f2f4(istate,isite,ihagg,ia,ib)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc,phismall
c      write(*,*) '   ---> f2f4   '
      zerc=dcmplx(0.d0,0.d0)

      nss=nsfull+nwave+8

      alp1=aesse(isite)
      bet1=besse(isite)
      u=uesse(isite)
c      i2=2!2
c      write(102,*) u,alp1,bet1
c      write(*,*) isite,u,alp1,bet1
      if (abs(u).lt.1.e-3.and.nsymm(istate,ihagg).eq.1) then
        ib=ia
c        write(*,*) ' exit f2f4'
        return
      endif

      call pw_go_m0(lfirst+ldecidi,bet1,3)
      call etoma(lfirst+ldecidi,alp1,5)
      ib=mxpdb+4

      do l1=0,ldecidi  !decidi!lfirst  !dunque sembra che ldecidi debba essere piu' piccolo di lfirst
        do m1=-l1,l1
          do iq=1,nqfull
            do is=1,nss
              almstore(ib,iq,l1,m1,is)=zerc 
            enddo
          enddo
          if (iproj(istate,ihagg,l1,m1).eq.1) then
            do n1=-l1,l1
              dum3=proj(istate,ihagg,l1,n1,m1)
              if (dum3.ne.0.d0) then
                do iq=1,nqfull
                  x=qqfull(iq)*u
                  do l2=0,lfirst
                    do m2=-l2,l2
                      dum1=phismall(x,lfirst+ldecidi,l1,l2,n1,m2,5,3)
c                if (l1.eq.2.and.m1.eq.-1.and.iq.eq.150) then
c                  write(102,*) isite,x,lfirst+ldecidi,l1,l2,n1,m2,dum1
c                endif
                      do is=1,nss
c                if (l1.eq.2.and.m1.eq.-1.and.iq.eq.150.and.is.eq.1) then
c                 write(102,*) 'BB ',almstore(ib,iq,l1,m1,is),
c     >                            +almstore(ia,iq,l2,m2,is),dum1
c                endif
                        almstore(ib,iq,l1,m1,is)=
     >                          almstore(ib,iq,l1,m1,is)
     >                         +almstore(ia,iq,l2,m2,is)*dum1*dum3
c                if (l1.eq.2.and.m1.eq.-1.and.iq.eq.150.and.is.eq.1) then
c                 write(102,*) 'CC ',almstore(ib,iq,l1,m1,is)
c                endif
                      enddo
                    enddo!m2
                  enddo!l2
                enddo !iq
              endif
            enddo !n1
          endif
        enddo !m1
      enddo !l1

      return
      end

      complex*16 function phismall(x,lmx,l,l1,m,m1,kalpha,kbeta)

      include 'quafit2014.cmn'
      complex*16 dum2,zerc,imm

      zerc=dcmplx(0.d0,0.d0)
      imm=dcmplx(0.d0,1.d0)

      lmax2=l+l1
c      write(*,*) ' daf ',lmax2,lmx
      if (lmax2.gt.lmx) lmax2=lmx
      nl2=iabs(l-l1)
      mm1=m-m1
      phismall=zerc
      if (x.eq.0.d0) then
        if (l1.eq.l.and.m1.eq.m) phismall=dcmplx(1.d0,0.d0)
        return
      endif
      do l2=nl2,lmax2
        dum1=cg(l1,l2,l,m1,mm1)
        if (dum1.ne.0.d0) then
          call sphbes(l2,x,sj,sy,sjp,syp)
          dum2=imm**l2*sj*(2*l2+1)*pwar(l2,mm1,0,kbeta)
     >         *cg(l1,l2,l,0,0)*dum1
          phismall=phismall+dum2
        endif
      enddo
      phismall=phismall*dsqrt(dfloat(2*l1+1)/dfloat(2*l+1))
     >     *eam(mm1,kalpha)
      return
      end
        
      subroutine f4f5(istate,ihagg,ia,ib)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc
c      write(*,*) '   ---> f4f5   '
      zerc=dcmplx(0.d0,0.d0)

      nss=nsfull+nwave+8

      alp1=agga(istate,ihagg)
      bet1=aggb(istate,ihagg)
      gam1=aggg(istate,ihagg)

      if (alp1.eq.zer.and.bet1.eq.zer.and.gam1.eq.zer) then
        ib=ia
c        write(*,*) ' exit f4f5'
        return
      endif
      ib=mxpdb+5

      do l=0,ldecidi
        do m=-l,l
          do iq=1,nqfull
            do is=1,nss
              almstore(ib,iq,l,m,is)=zerc
            enddo
          enddo
          do n=-l,l
            if (iproj(istate,ihagg,l,n).eq.1) then
              dum2=eam(m,3)*pwar(l,m,n,2)*eam(n,4)
              do iq=1,nqfull
                do is=1,nss
                  almstore(ib,iq,l,m,is)=
     >            almstore(ib,iq,l,m,is)
     >                           +dum2*almstore(ia,iq,l,n,is)
                enddo
              enddo
            endif
          enddo!n
        enddo!m
      enddo!l

      return
      end

      subroutine f5f6(istate,ihagg,iconf,ia,ib)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc
c      write(*,*) '   ---> f5f6   '
      jconf=iconf+4
      zerc=dcmplx(0.d0,0.d0)

      nss=nsfull+nwave+8

      alp1=alpscrew(istate,ihagg,jconf)
      bet1=dacos(cbescrew(istate,ihagg,jconf))
      gam1=zer

      if (abs(alp1).lt.1.e-3.and.abs(bet1).lt.1.e-3) then
        ib=ia
c        write(*,*) ' exit f5f6'
        return
      endif
      ib=mxpdb+6

      call pw_go_mn(ldecidi,bet1,2)
      call etoma(ldecidi,alp1,3)

      do l=0,ldecidi
        do m=-l,l
          do iq=1,nqfull
            do is=1,nss
              almstore(ib,iq,l,m,is)=zerc
            enddo
          enddo
          do n=-l,l
            if (iproj(istate,ihagg,l,n).eq.1) then
              dum2=eam(n,3)*pwar(l,n,m,2)
              dum2=dconjg(dum2)
              do iq=1,nqfull
                do is=1,nss
                  almstore(ib,iq,l,m,is)=
     >            almstore(ib,iq,l,m,is)
     >                           +dum2*almstore(ia,iq,l,n,is)
                enddo
              enddo
            endif
          enddo!n
        enddo!m
      enddo!l

      return
      end


      subroutine f6f7(istate,ihagg,iconf,ia,ib)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc,phismall
c      write(*,*) '   ---> f6f7   '
      zerc=dcmplx(0.d0,0.d0)

      jconf=iconf+4
      nss=nsfull+nwave+8

      xt=uessea*dsin(bessea)*dcos(aessea)
      yt=uessea*dsin(bessea)*dsin(aessea)
      zt=uessea*dcos(bessea)
ccc     >-(npoly(istate,ihagg)-1)*dscrew(istate,ihagg,jconf)/2
ccc     >-(npoly(istate,ihagg)-1)*dscrew(istate,ihagg,jconf)/2
      call cpolar(xt,
     >            yt,
     >            zt,
     >            u,
     >            alp1,
     >            bet1)
c      write(*,*) xt,yt,zt
c      write(*,*) u ,alp1,bet1
c      pause

      if (abs(u).lt.1.e-3.or.npoly(istate,ihagg).eq.1) then
        ib=ia
c        write(*,*) ' exit f6f7'
        return
      endif
      ib=mxpdb+7

c      pause
      call pw_go_m0(lsecond+ldecidi,bet1,4)
      call etoma(lsecond+ldecidi,alp1,6)

      do l1=0,lsecond
        do m1=-l1,l1
          do iq=1,nqfull
            do is=1,nss
              almstore(ib,iq,l1,m1,is)=zerc 
            enddo
          enddo
          do iq=1,nqfull
            x=qqfull(iq)*u
c      write(*,*) 'qui 3 ',x
            do l2=0,ldecidi
              do m2=-l2,l2
                dum1=phismall(x,lsecond+ldecidi,l1,l2,m1,m2,6,4)
                if (dreal(dum1*dconjg(dum1)).lt.1.e-8) goto 2020
                do is=1,nss
                  almstore(ib,iq,l1,m1,is)=
     >            almstore(ib,iq,l1,m1,is)
     >                             +almstore(ia,iq,l2,m2,is)*dum1
                enddo
 2020           continue
              enddo!m2
            enddo!l2
          enddo !iq
        enddo !m1
      enddo !l1

      return
      end

      subroutine f7f8(istate,ihagg,iconf,ia)
      include 'quafit2014.cmn'
      complex*16 dum1,dum2,zerc,phismall
c      write(*,*) '   ---> f7f8   ',npoly(istate,ihagg)
      zerc=dcmplx(0.d0,0.d0)

      jconf=iconf+4
      nss=nsfull+nwave+8
c      write(102,*) ascrew(istate,ihagg,jconf)*conv
c      write(102,*) dscrew(istate,ihagg,jconf)
      if (npoly(istate,ihagg).eq.1) then
        ibfinal=ia
c        write(*,*) ' exit f7f8'
        return
      endif
      ibfinal=mxpdb+8

      call pw_go_m0(lsecond+lsecond,0.d0,5)
      call pw_go_m0(lsecond+lsecond,pi,9)
      call etoma(lsecond+lsecond,0.d0,7)
      call etoma((lsecond+lsecond)*(npoly(istate,ihagg)-1),
     >ascrew(istate,ihagg,jconf),8)

      do l1=0,lsecond
        do m1=-l1,l1
          do iq=1,nqfull
            do is=1,nss
              almstore(ibfinal,iq,l1,m1,is)=zerc 
            enddo
          enddo
          do iq=1,nqfull
            do ipoly=1,npoly(istate,ihagg)
c              x=qqfull(iq)*(ipoly-1)*dscrew(istate,ihagg,jconf)
              dum9=(-(npoly(istate,ihagg)-1)*0.5+(ipoly-1))
     >*dscrew(istate,ihagg,jconf)
              x=qqfull(iq)*abs(dum9)
              ibb=5
              if (dum9.lt.0.d0) ibb=9  !!! attento se e' pi allora le regole sono diverse ......
              do l2=0,lsecond
              do m2=-l2,l2
c                m2=m1
                dum1=phismall(x,(lsecond+lsecond),l1,l2,m1,m2,7,ibb)
     >               *eam(-m1*(ipoly-1),8)
                if (dreal(dum1*dconjg(dum1)).lt.1.e-8) goto 2020
c                write(*,*) iq,
cqqfull(iq),x,l1,m1,l2,
c     >phismall(x,(lsecond+lsecond),l1,l2,m1,m1,7,5),
c     >               eam(-m1*(ipoly-1),8),dcmplx(
c     >dcos(m1*(ipoly-1)*ascrew(istate,ihagg,jconf)),
c     >dsin(m1*(ipoly-1)*ascrew(istate,ihagg,jconf)))
c                if (l1.eq.2.and.m1.eq.-1.and.iq.eq.150) then
c           write(102,*) ipoly,x,(lsecond+lsecond),l1,l2,m1,m2,dum1,
c     >eam(-m1*(ipoly-1),8),dum9
c                endif
                do is=1,nss
c                if (l1.eq.2.and.m1.eq.-1.and.iq.eq.150.and.is.eq.1) then
c                 write(102,*) 'BB ',almstore(ibfinal,iq,l1,m1,is),
c     >                            +almstore(ia,iq,l2,m2,is),dum1
c                endif
                  almstore(ibfinal,iq,l1,m1,is)=
     >            almstore(ibfinal,iq,l1,m1,is)
     >                             +almstore(ia,iq,l2,m2,is)*dum1

c                if (l1.eq.2.and.m1.eq.-1.and.iq.eq.150.and.is.eq.1) then
c                write(102,*) 'CC ',almstore(ibfinal,iq,l1,m1,is)
c                endif
                enddo
 2020           continue
                enddo
              enddo!l2
            enddo!ipoly
          enddo !iq
        enddo !m1
      enddo !l1

      return
      end





c######################################################################

      subroutine framhag(istate,ihagg,iconf)

c.....Ruota il sistema di riferimento per portare gli assi su quelli per cui e' definito il gruppo
c.....di simmetria dell'aggregato ihagg per lo stato istate e la configurazione iconf

c.....Trasla il sistema di riferimento per portarlo sul centro geom. di tutti i siti per tutte le operazioni
c.....di simmetria.

c.....Risultato finale: vettore x (xms(isite),axs(isite),bxs(isite)) per ogni sito
c                       angoli rotazione ays(isite), bys(isite), gys(isite) per ogni sito
c                       vettore centre(isite,isub) per ogni sito in ogni operazione di symm.
c                       angoli rotazione alrot(isite,isub), betrot(isite,isub), alprot(isite,isub) per ogni sito in ogni operazione di symm.

      include 'quafit2014.cmn'
c      write(*,*) '   ---> framhag   '
      jconf=iconf+4
      
            isite=1
c            write(*,*) ' mattina 4',iflag
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv

c      if (iflag.eq.1.and.ifhinge(istate)
c     >.eq.1.and.ihagg.eq.1) call ufind(istate,iconf)
c      if (iflag.eq.1.and.ifhinge(istate)
c     >.eq.2.and.ihagg.eq.1) call afind(istate,iconf)

      call mat(alpscrew(istate,ihagg,jconf),
     >dacos(cbescrew(istate,ihagg,jconf)),
     >zer,roz2)

      call mat(agga(istate,ihagg),aggb(istate,ihagg),
     >aggg(istate,ihagg),roz1)


c.era una prova      call mat(zer,zer,zer,roz2)

c.....roz2 = R[\omega_s] (2)

      sum1=0.d0
      sum2=0.d0
      sum3=0.d0

      rum1=0.d0
      rum2=0.d0
      rum3=0.d0

      do isite=1,nsite(istate)

c            write(*,*) ' mattina 3'
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv

c                write(*,*) ' 41 ci sono',ums(istate,isite,jconf),
c     >aus(istate,isite,jconf),cbu(istate,isite,jconf)
c     >,istate,isite,jconf

        xt=ums(istate,isite,jconf)*sfromc(cbu(istate
     >,isite,jconf))*dcos(aus(istate,isite,jconf))
        yt=ums(istate,isite,jconf)*sfromc(cbu(istate
     >,isite,jconf))*dsin(aus(istate,isite,jconf))               
        zt=ums(istate,isite,jconf)*cbu(istate,isite,jconf)
c      call cpolar(xt,yt,zt,r1yyy,a1yyy,b1yyy)
c          write(*,*) " FFF ",xt,yt,zt,r1yyy,a1yyy,b1yyy

        xt1=xt
        yt1=yt
        zt1=zt

c.......[xt,yt,zt] = {\bf u}_i

c            write(650,*) 'AA',iconf,xt,yt,zt
c         write(*,*) ' prima di ',-gas(istate,isite,jconf),
c     >-dacos(cba(istate
c     >,isite,jconf)),-aas(istate,isite,jconf)
        call mat(-gas(istate,isite,jconf),-dacos(cba(istate
     >,isite,jconf)),-aas(istate,isite,jconf),tor) !matrice che ruota il sito isite
c.....tor = R^{-1}[\omega_i] (1)
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv
c            write(*,*) ' TOR ',tor

        do ipoly=1,npoly(istate,ihagg)
          call mat(zer,zer,-(ipoly-1)*ascrew(istate,ihagg,jconf),tor1) !matrice che ruota isite per screw
          dd=(ipoly-1)*dscrew(istate,ihagg,jconf)
          do isub=1,nsymm(istate,ihagg)
            call mat(-gamsym(istate,ihagg,isub),
     >               -betsym(istate,ihagg,isub),
     >               -alpsym(istate,ihagg,isub),roz) !matrice che ruota isite per operaz. symm.
c...........roz=R^-1{\Omega_{\cal R}}
c...........Prodotto fra matrici
            do i=1,3
             do j=1,3
	      dum1=0.d0
              do k=1,3
               do l=1,3
                do l2=1,3
                 do l3=1,3
                  do l4=1,3
c                   dum1=dum1+tor1(i,k)*roz2(k,l)*roz(l,l2)*tor(l2,j)
                   dum1=dum1+tor1(i,k)*roz2(k,l)*roz1(l2,l)
     >                      *roz(l2,l3)*roz1(l3,l4)*tor(l4,j)
c... R[n*alphascr] * R[s-sscr] * R[sG->s]  * R[Omega_k] * R[s->sG]    * R[omega_a]
c..      tor1(i,k) * roz2(k,l) * roz1(l2,l)* roz(l2,l3) * roz1(l3,l4) * tor(l4,j)
                  enddo
                 enddo
                enddo
               enddo
              enddo
              rot(i,j)=dum1 !matrice che ruota isite per operaz. symm. nel nuovo sistema rif.
              enddo
            enddo
            call taminv(dum1,dum2,dum3,rot)
            alprot(isite,isub,ipoly)=dum1
            betrot(isite,isub,ipoly)=dum2
            gamrot(isite,isub,ipoly)=dum3

c            call rotrdi(xt1,yt1,zt1,xt2,yt2,zt2,zer,zer,zer,roz)
c            call rotrdi(xt2,yt2,zt2,xt3,yt3,zt3,zer,zer,zer,roz2)
c            call rotrdi(xt3,yt3,zt3,xt,yt,zt,zer,zer, dd,tor1)

            call rotrdi(xt1,yt1,zt1,xt2,yt2,zt2,zer,zer,zer,roz1)
            call rotrdi(xt2,yt2,zt2,xt3,yt3,zt3,zer,zer,zer,roz)
            call rotrin(xt3,yt3,zt3,xt4,yt4,zt4,zer,zer,zer,roz1)
            call rotrdi(xt4,yt4,zt4,xt5,yt5,zt5,zer,zer,zer,roz2)
            call rotrdi(xt5,yt5,zt5,xt,yt,zt,zer,zer, dd,tor1)

c      call cpolar(xt,yt,zt,r1yyy,a1yyy,b1yyy)
c          write(*,*) " ZZZ ",xt,yt,zt,r1yyy,a1yyy,b1yyy
            centre(isite,isub,ipoly,1)=xt
            centre(isite,isub,ipoly,2)=yt
            centre(isite,isub,ipoly,3)=zt
c            write(650,*) iconf,xt,yt,zt

c...........Rifletti da qui perche' a te serve un <u> prima dello screw.... 
            if (ipoly.eq.1) then !!! ok, ho riflettuto
c          write(*,*) ' i r x ',rum1,xt1,xt2,xt3,xt,isite,roz
c          write(*,*) ' i r y ',rum2,yt1,yt2,yt3,yt,roz2
c          write(*,*) ' i r z ',rum3,zt1,zt2,zt3,zt,tor1
              rum1=rum1+xt
              rum2=rum2+yt
              rum3=rum3+zt
            endif

            sum1=sum1+xt
            sum2=sum2+yt
            sum3=sum3+zt
          enddo
        enddo
      enddo

c.....Trasla in modo che il centro coincida con il c.geom.
      dum1=0.
      if (nsite(istate).gt.0) then
      if (nsymm(istate,ihagg)*nsite(istate)*npoly(istate,ihagg).eq.0) 
     >stop 'nsymm(istate,ihagg)*nsite(istate)*npoly(istate,ihagg)=0'
      dum1=1.d0/(nsymm(istate,ihagg)*nsite(istate)*npoly(istate,ihagg))
      endif
      sum1=sum1*dum1
      sum2=sum2*dum1
      sum3=sum3*dum1
      trasl(istate,ihagg,1)=sum1
      trasl(istate,ihagg,2)=sum2
      trasl(istate,ihagg,3)=sum3
      dum1=0.
      if (nsite(istate).gt.0) then
      if (nsymm(istate,ihagg)*nsite(istate).eq.0)
     > stop 'nsymm(istate,ihagg)*nsite(istate)=0'
      dum1=1.d0/(nsymm(istate,ihagg)*nsite(istate))
      endif
c          write(*,*) ' r x ',rum1,dum1
c          write(*,*) ' r y ',rum2
c          write(*,*) ' r z ',rum3
      rum1=rum1*dum1 !  * 0 !!!!! 01/04/2014
      rum2=rum2*dum1 !  * 0
      rum3=rum3*dum1 !  * 0
      do isite=1,nsite(istate)
        xt=centre(isite,1,1,1)-rum1
        yt=centre(isite,1,1,2)-rum2
        zt=centre(isite,1,1,3)-rum3
c          write(102,*) ' u00x ',centre(isite,1,1,1),rum1
c          write(102,*) ' u00y ',centre(isite,1,1,2),rum2
c          write(102,*) ' u00z ',centre(isite,1,1,3),rum3
c          write(*,*) ' u00 ',xt1,yt1,zt1,roz2

c.......In questo modo il vettore uesse e' espresso nel sistema di rif. sG (quello del gruppo G)
        call rotrin(xt,yt,zt,xt2,yt2,zt2,zer,zer,zer,roz2)
        call rotrdi(xt2,yt2,zt2,xt1,yt1,zt1,zer,zer,zer,roz1)
        call cpolar(xt1,
     >              yt1,
     >              zt1,
     >              uesse(isite),  !!! poi l'hai chiamato a_d, sei nel sys.rif. sG
     >              aesse(isite),
     >              besse(isite)) 
c          write(*,*) ' u ',xt1,yt1,zt1
        do isub=1,nsymm(istate,ihagg)
          do ipoly=1,npoly(istate,ihagg)
            centre(isite,isub,ipoly,1)= 
     >      centre(isite,isub,ipoly,1)-sum1
            centre(isite,isub,ipoly,2)=
     >      centre(isite,isub,ipoly,2)-sum2
            centre(isite,isub,ipoly,3)=
     >      centre(isite,isub,ipoly,3)-sum3
c            write(*,*) 'CDC',(centre(isite,isub,ipoly,k),k=1,3)
          enddo
        enddo
      enddo
      call cpolar(rum1,
     >            rum2,
     >            rum3,!!! questo uessea e' <uG> nel sys. s_scr
     >            uessea, 
     >            aessea,
     >            bessea)
c              write(*,*) ' ad ',rum1,rum2,rum3,uessea,aessea,bessea

c.....In questo modo il vettore uesselnk e' l'opposto di uessea espresso nel sistema di rif. sG (quello del gruppo G)
      call rotrin(rum1,rum2,rum3,xt2,yt2,zt2,zer,zer,zer,roz2)
      call rotrdi(xt2,yt2,zt2,xt1,yt1,zt1,zer,zer,zer,roz1)
      call cpolar(-xt1,
     >            -yt1,
     >            -zt1,
     >            uesselnk, 
     >            aesselnk,
     >            besselnk) 


c.....parte nuova per constraints
      if (ihagg.gt.1) return
      do isite=1,nsite(istate)
        call constr_site_rot(istate,isite)
      enddo

      return
c        call cpolar(centre(isite,1,1,1),
c     >              centre(isite,1,1,2),
c     >              centre(isite,1,1,3),
c     >              xms(isite),
c     >              axs(isite),
c     >              bxs(isite)) !vettore centro isite nel nuovo sys.rif dopo traslazione
c        cbx(isite)=dcos(bxs(isite))

c      return
      end

c######################################################################





      subroutine ciclo_sub(ivar)

      include 'quafit2014.cmn'

c.....Questo schema potrebbe essere del tutto generale e potresti usarlo all'interno di un ciclo. In base a quello che cambi, potresti poi decidere cosa chiamare tra le varie subroutine, potresti anche richiamare i file di memorizzazione e rimpiazzarli o ripristinarli a seconda dell'accettazione della mossa MC

c.....Modifica il valore della variabile ivar e calcola tutto cio' che ne consegue

c     F=fisso, V=variabile, U=unico

c     Stato Aggregato Configurazione (o Conformazione)
c           Sito
c j=0 F     F         F
c j=1 V     V         V
c j=2 U     U         U j2NAME
c j=3 V     U         U j3NAME(istate)
c j=4 V     V         U 
c                                     isite


c      write(*,*) '   ---> ciclo_sub   '

      j2ascrew=0
      j2alpscrew=0
      j2cbescrew=0
      j2dscrew=0
      j2rmeff=0
      do k=1,nsfull  
      j2rhcl(k)=0
      enddo
      do k=1,mxtherf  
      do j=1,neffec+1    
      j2ftherm(k,j)=0
      enddo
      enddo
      j2ums=0
      j2aus=0
      j2cbu=0
      j2aas=0
      j2cba=0
      j2gas=0

      jvar=0
      do istate=1,nstate

        j3ascrew(istate)=0
        j3alpscrew(istate)=0
        j3cbescrew(istate)=0
        j3dscrew(istate)=0
        j3rmeff(istate)=0
        do k=1,nsfull  
        j3rhcl(istate,k)=0
        enddo
        do k=1,mxtherf  
        do j=1,neffec+1    
        j3ftherm(istate,k,j)=0
        enddo
        enddo
        j3ums(istate)=0
        j3aus(istate)=0
        j3cbu(istate)=0
        j3aas(istate)=0
        j3cba(istate)=0
        j3gas(istate)=0

        do ihagg=1,nhagg(istate)

          if (jascrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ascrew(istate,ihagg,iconf+4)
                varmin(ivar)=ascrew(istate,ihagg,2)
                varmax(ivar)=ascrew(istate,ihagg,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                ascrew(istate,ihagg,iconf+4)=varnew(ivar)
                ascrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              endif
            enddo
          else if (jascrew(istate,ihagg).eq.2) then
            if (j2ascrew.eq.0) then
              j2ascrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ascrew(1,1,5)
                varmin(ivar)=ascrew(1,1,2)
                varmax(ivar)=ascrew(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      ascrew(jstate,jhagg,iconf+4)=varnew(ivar)
                      ascrew(jstate,jhagg,4)=vardel(ivar)
                      call all_sites_screw_move(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jascrew(istate,ihagg).eq.3) then
            if (j3ascrew(istate).eq.0) then
              j3ascrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ascrew(istate,1,5)
                varmin(ivar)=ascrew(istate,1,2)
                varmax(ivar)=ascrew(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    ascrew(istate,jhagg,iconf+4)=varnew(ivar)
                    ascrew(istate,jhagg,4)=vardel(ivar)
                    call all_sites_screw_move(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jascrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=ascrew(istate,ihagg,5)
              varmin(ivar)=ascrew(istate,ihagg,2)
              varmax(ivar)=ascrew(istate,ihagg,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                ascrew(istate,ihagg,iconf+4)=varnew(ivar)
                ascrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jalpscrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=alpscrew(istate,ihagg,iconf+4)
                varmin(ivar)=alpscrew(istate,ihagg,2)
                varmax(ivar)=alpscrew(istate,ihagg,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                alpscrew(istate,ihagg,iconf+4)=varnew(ivar)
                alpscrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              endif
            enddo
          else if (jalpscrew(istate,ihagg).eq.2) then
            if (j2alpscrew.eq.0) then
              j2alpscrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=alpscrew(1,1,5)
                varmin(ivar)=alpscrew(1,1,2)
                varmax(ivar)=alpscrew(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      alpscrew(jstate,jhagg,iconf+4)=varnew(ivar)
                      alpscrew(jstate,jhagg,4)=vardel(ivar)
                      call all_sites_screw_move(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jalpscrew(istate,ihagg).eq.3) then
            if (j3alpscrew(istate).eq.0) then
              j3alpscrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=alpscrew(istate,1,5)
                varmin(ivar)=alpscrew(istate,1,2)
                varmax(ivar)=alpscrew(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    alpscrew(istate,jhagg,iconf+4)=varnew(ivar)
                    alpscrew(istate,jhagg,4)=vardel(ivar)
                    call all_sites_screw_move(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jalpscrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=alpscrew(istate,ihagg,5)
              varmin(ivar)=alpscrew(istate,ihagg,2)
              varmax(ivar)=alpscrew(istate,ihagg,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                alpscrew(istate,ihagg,iconf+4)=varnew(ivar)
                alpscrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              enddo
            endif
          endif
          if (jcbescrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cbescrew(istate,ihagg,iconf+4)
                varmin(ivar)=cbescrew(istate,ihagg,2)
                varmax(ivar)=cbescrew(istate,ihagg,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                cbescrew(istate,ihagg,iconf+4)=varnew(ivar)
                cbescrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              endif
            enddo
          else if (jcbescrew(istate,ihagg).eq.2) then
            if (j2cbescrew.eq.0) then
              j2cbescrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cbescrew(1,1,5)
                varmin(ivar)=cbescrew(1,1,2)
                varmax(ivar)=cbescrew(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      cbescrew(jstate,jhagg,iconf+4)=varnew(ivar)
                      cbescrew(jstate,jhagg,4)=vardel(ivar)
                      call all_sites_screw_move(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jcbescrew(istate,ihagg).eq.3) then
            if (j3cbescrew(istate).eq.0) then
              j3cbescrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cbescrew(istate,1,5)
                varmin(ivar)=cbescrew(istate,1,2)
                varmax(ivar)=cbescrew(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    cbescrew(istate,jhagg,iconf+4)=varnew(ivar)
                    cbescrew(istate,jhagg,4)=vardel(ivar)
                    call all_sites_screw_move(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jcbescrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=cbescrew(istate,ihagg,5)
              varmin(ivar)=cbescrew(istate,ihagg,2)
              varmax(ivar)=cbescrew(istate,ihagg,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                cbescrew(istate,ihagg,iconf+4)=varnew(ivar)
                cbescrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jdscrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=dscrew(istate,ihagg,iconf+4)
                varmin(ivar)=dscrew(istate,ihagg,2)
                varmax(ivar)=dscrew(istate,ihagg,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                dscrew(istate,ihagg,iconf+4)=varnew(ivar)
                dscrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              endif
            enddo
          else if (jdscrew(istate,ihagg).eq.2) then
            if (j2dscrew.eq.0) then
              j2dscrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=dscrew(1,1,5)
                varmin(ivar)=dscrew(1,1,2)
                varmax(ivar)=dscrew(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      dscrew(jstate,jhagg,iconf+4)=varnew(ivar)
                      dscrew(jstate,jhagg,4)=vardel(ivar)
                      call all_sites_screw_move(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jdscrew(istate,ihagg).eq.3) then
            if (j3dscrew(istate).eq.0) then
              j3dscrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=dscrew(istate,1,5)
                varmin(ivar)=dscrew(istate,1,2)
                varmax(ivar)=dscrew(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    dscrew(istate,jhagg,iconf+4)=varnew(ivar)
                    dscrew(istate,jhagg,4)=vardel(ivar)
                    call all_sites_screw_move(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jdscrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=dscrew(istate,ihagg,5)
              varmin(ivar)=dscrew(istate,ihagg,2)
              varmax(ivar)=dscrew(istate,ihagg,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                dscrew(istate,ihagg,iconf+4)=varnew(ivar)
                dscrew(istate,ihagg,4)=vardel(ivar)
                call all_sites_screw_move(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jrmeff(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=rmeff(istate,ihagg,iconf+4)
                varmin(ivar)=rmeff(istate,ihagg,2)
                varmax(ivar)=rmeff(istate,ihagg,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                rmeff(istate,ihagg,iconf+4)=varnew(ivar)
                rmeff(istate,ihagg,4)=vardel(ivar)
                call cp_parz_2(istate,ihagg,iconf)
                call pq_parz_2(istate,ihagg,iconf)
              endif
            enddo
          else if (jrmeff(istate,ihagg).eq.2) then
            if (j2rmeff.eq.0) then
              j2rmeff=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=rmeff(1,1,5)
                varmin(ivar)=rmeff(1,1,2)
                varmax(ivar)=rmeff(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      rmeff(jstate,jhagg,iconf+4)=varnew(ivar)
                      rmeff(jstate,jhagg,4)=vardel(ivar)
                      call cp_parz_2(jstate,jhagg,iconf)
                      call pq_parz_2(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jrmeff(istate,ihagg).eq.3) then
            if (j3rmeff(istate).eq.0) then
              j3rmeff(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=rmeff(istate,1,5)
                varmin(ivar)=rmeff(istate,1,2)
                varmax(ivar)=rmeff(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    rmeff(istate,jhagg,iconf+4)=varnew(ivar)
                    rmeff(istate,jhagg,4)=vardel(ivar)
                    call cp_parz_2(istate,jhagg,iconf)
                    call pq_parz_2(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jrmeff(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
c              write(*,*) 'rmeff:',rmeff(istate,ihagg,5)
c     >         ,rmeff(istate,ihagg,2)
c     >         ,rmeff(istate,ihagg,3)

              varold(ivar)=rmeff(istate,ihagg,5)
              varmin(ivar)=rmeff(istate,ihagg,2)
              varmax(ivar)=rmeff(istate,ihagg,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                rmeff(istate,ihagg,iconf+4)=varnew(ivar)
                rmeff(istate,ihagg,4)=vardel(ivar)
                call cp_parz_2(istate,ihagg,iconf)
                call pq_parz_2(istate,ihagg,iconf)
              enddo
            endif
          endif


          do k=1,nsfull  
          if (jrhcl(istate,ihagg,k).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=rhcl(istate,ihagg,k,iconf+4)
                varmin(ivar)=rhcl(istate,ihagg,k,2)
                varmax(ivar)=rhcl(istate,ihagg,k,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                rhcl(istate,ihagg,k,iconf+4)=varnew(ivar)
                rhcl(istate,ihagg,k,4)=vardel(ivar)
                call cp_parz_2(istate,ihagg,iconf)
                call pq_parz_2(istate,ihagg,iconf)
              endif
            enddo
          else if (jrhcl(istate,ihagg,k).eq.2) then
            if (j2rhcl(k).eq.0) then
              j2rhcl(k)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=rhcl(1,1,k,5)
                varmin(ivar)=rhcl(1,1,k,2)
                varmax(ivar)=rhcl(1,1,k,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      rhcl(jstate,jhagg,k,iconf+4)=varnew(ivar)
                      rhcl(jstate,jhagg,k,4)=vardel(ivar)
                      call cp_parz_2(jstate,jhagg,iconf)
                      call pq_parz_2(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jrhcl(istate,ihagg,k).eq.3) then
            if (j3rhcl(istate,k).eq.0) then
              j3rhcl(istate,k)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=rhcl(istate,1,k,5)
                varmin(ivar)=rhcl(istate,1,k,2)
                varmax(ivar)=rhcl(istate,1,k,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    rhcl(istate,jhagg,k,iconf+4)=varnew(ivar)
                    rhcl(istate,jhagg,k,4)=vardel(ivar)
                    call cp_parz_2(istate,jhagg,iconf)
                    call pq_parz_2(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jrhcl(istate,ihagg,k).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=rhcl(istate,ihagg,k,5)
              varmin(ivar)=rhcl(istate,ihagg,k,2)
              varmax(ivar)=rhcl(istate,ihagg,k,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                rhcl(istate,ihagg,k,iconf+4)=varnew(ivar)
                rhcl(istate,ihagg,k,4)=vardel(ivar)
                call cp_parz_2(istate,ihagg,iconf)
                call pq_parz_2(istate,ihagg,iconf)
              enddo
            endif
          endif
          enddo

          do k=1,mxtherf  
          do j=1,neffec+1    
          if (jftherm(istate,ihagg,k,j).eq.1) then
            stop 'jftherm'
          else if (jftherm(istate,ihagg,k,j).eq.2) then
            if (j2ftherm(k,j).eq.0) then
              j2ftherm(k,j)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ftherm(1,1,k,j,1)
                varmin(ivar)=ftherm(1,1,k,j,2)
                varmax(ivar)=ftherm(1,1,k,j,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    ftherm(jstate,jhagg,k,j,1)=varnew(ivar)
                    ftherm(jstate,jhagg,k,j,4)=vardel(ivar)
                    do nn=1,nexper
                      fracv(jstate,jhagg,nn)=fract(jstate,jhagg,nn)
                    enddo
                  enddo
                enddo
                call fraccalc
              endif
            endif
          else if (jftherm(istate,ihagg,k,j).eq.3) then
            if (j3ftherm(istate,k,j).eq.0) then
              j3ftherm(istate,k,j)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ftherm(istate,1,k,j,1)
                varmin(ivar)=ftherm(istate,1,k,j,2)
                varmax(ivar)=ftherm(istate,1,k,j,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jhagg=1,nhagg(istate)
                  ftherm(istate,jhagg,k,j,1)=varnew(ivar)
                  ftherm(istate,jhagg,k,j,4)=vardel(ivar)
                  do nn=1,nexper
                    fracv(istate,jhagg,nn)=fract(istate,jhagg,nn)
                  enddo
                enddo
                call fraccalc
              endif
            endif
          else if (jftherm(istate,ihagg,k,j).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=ftherm(istate,ihagg,k,j,1)
              varmin(ivar)=ftherm(istate,ihagg,k,j,2)
              varmax(ivar)=ftherm(istate,ihagg,k,j,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              ftherm(istate,ihagg,k,j,1)=varnew(ivar)
              ftherm(istate,ihagg,k,j,4)=vardel(ivar)
              do nn=1,nexper
                fracv(istate,ihagg,nn)=fract(istate,ihagg,nn)
              enddo
              call fraccalc
            endif
          endif
          enddo
          enddo







          if (jfracm.eq.1) then
            iagg=jaghag(istate,ihagg)
            do nn=1,nexper  
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=xfrac(iagg,nn,1)
                varmin(ivar)=0.d0
                varmax(ivar)=1.d0
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                xfrac(iagg,nn,1)=varnew(ivar)
                xfrac(iagg,nn,4)=vardel(ivar)
                do jstate=1,nstate
                do jhagg=1,nhagg(jstate)
                  fracv(jstate,jhagg,nn)=fract(jstate,jhagg,nn)
                enddo
                enddo
                call xfraccalc
             endif
            enddo
          endif
c          if (jfracm.eq.1.and.jaghag(istate,ihagg).gt.1) then
c            iagg=jaghag(istate,ihagg)
c            do nn=1,nexper  
c              jvar=jvar+1
c              if (ivar.eq.jvar) then
c                varold(ivar)=xfrac(iagg-1,nn,1)
c                varmin(ivar)=0.d0
c                varmax(ivar)=1.d0
c                call move(vardel(ivar),varnew(ivar),varold(ivar),
c     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
c                xfrac(iagg-1,nn,1)=varnew(ivar)
c                xfrac(iagg-1,nn,4)=vardel(ivar)
c                do jstate=1,nstate
c                do jhagg=1,nhagg(jstate)
c                  fracv(jstate,jhagg,nn)=fract(jstate,jhagg,nn)
c                enddo
c                enddo
c                call xfraccalc
c             endif
c            enddo
c          endif



        enddo !end ihagg

        do isite=1,nsite(istate)

          if (jums(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ums(istate,isite,iconf+4)
                varmin(ivar)=ums(istate,isite,2)
                varmax(ivar)=ums(istate,isite,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                ums(istate,isite,iconf+4)=varnew(ivar)
                ums(istate,isite,4)=vardel(ivar)
c               if ((isite.eq.1.and.ifhinge(istate) !..Di default ifhinge(istate)=0
c     >.eq.1).or.(ifhinge(istate)
c     >.eq.2)) then
c                  call all_sites_move(istate,iconf)
c                else
                  call one_site_move(istate,isite,iconf,1) !!! ed e' chiaro qui che se muovi il sito devi controllare tutti i linkers ad esso affluenti
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  ums(istate,isite+7,iconf+4)=varnew(ivar)
                  ums(istate,isite+7,4)=vardel(ivar)
                  call d5c5sitelnk(istate,isite,iconf)
                  call one_site_move(istate,isite+7,iconf,0)
                endif                  
c.........
                endif
c              endif
            enddo
          else if (jums(istate,isite).eq.2) then
            if (j2ums.eq.0) then
              j2ums=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ums(1,1,5)
                varmin(ivar)=ums(1,1,2)
                varmax(ivar)=ums(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      ums(jstate,jsite,iconf+4)=varnew(ivar)
                      ums(jstate,jsite,4)=vardel(ivar)
                      call one_site_move(jstate,jsite,iconf,1)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jums(istate,isite).eq.3) then
            if (j3ums(istate).eq.0) then
              j3ums(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=ums(istate,1,5)
                varmin(ivar)=ums(istate,1,2)
                varmax(ivar)=ums(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    ums(istate,jsite,iconf+4)=varnew(ivar)
                    ums(istate,jsite,4)=vardel(ivar)
                    call one_site_move(istate,jsite,iconf,1)
                  enddo
                enddo
              endif
            endif
          else if (jums(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=ums(istate,isite,5)
              varmin(ivar)=ums(istate,isite,2)
              varmax(ivar)=ums(istate,isite,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                ums(istate,isite,iconf+4)=varnew(ivar)
                ums(istate,isite,4)=vardel(ivar)
                call one_site_move(istate,isite,iconf,1)
              enddo
            endif
          endif

          if (jaus(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=aus(istate,isite,iconf+4)
                varmin(ivar)=aus(istate,isite,2)
                varmax(ivar)=aus(istate,isite,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                aus(istate,isite,iconf+4)=varnew(ivar)
                aus(istate,isite,4)=vardel(ivar)

c               if ((isite.eq.1.and.ifhinge(istate)
c     >.eq.1).or.(ifhinge(istate)
c     >.eq.2)) then
c                  call all_sites_move(istate,iconf)
c                else
                  call one_site_move(istate,isite,iconf,1)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  aus(istate,isite+7,iconf+4)=9*36.d0/conv-varnew(ivar) !11*36.d0/conv-varnew(ivar)
                  aus(istate,isite+7,4)=vardel(ivar)
                  call d5c5sitelnk(istate,isite,iconf)
                  call one_site_move(istate,isite+7,iconf,0)
                endif                  
c.........
c                endif
              endif
            enddo
          else if (jaus(istate,isite).eq.2) then
            if (j2aus.eq.0) then
              j2aus=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=aus(1,1,5)
                varmin(ivar)=aus(1,1,2)
                varmax(ivar)=aus(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      aus(jstate,jsite,iconf+4)=varnew(ivar)
                      aus(jstate,jsite,4)=vardel(ivar)
                      call one_site_move(jstate,jsite,iconf,1)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jaus(istate,isite).eq.3) then
            if (j3aus(istate).eq.0) then
              j3aus(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=aus(istate,1,5)
                varmin(ivar)=aus(istate,1,2)
                varmax(ivar)=aus(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    aus(istate,jsite,iconf+4)=varnew(ivar)
                    aus(istate,jsite,4)=vardel(ivar)
                    call one_site_move(istate,jsite,iconf,1)
                  enddo
                enddo
              endif
            endif
          else if (jaus(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=aus(istate,isite,5)
              varmin(ivar)=aus(istate,isite,2)
              varmax(ivar)=aus(istate,isite,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                aus(istate,isite,iconf+4)=varnew(ivar)
                aus(istate,isite,4)=vardel(ivar)
                call one_site_move(istate,isite,iconf,1)
              enddo
            endif
          endif

          if (jcbu(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cbu(istate,isite,iconf+4)
                varmin(ivar)=cbu(istate,isite,2)
                varmax(ivar)=cbu(istate,isite,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                cbu(istate,isite,iconf+4)=varnew(ivar)
                cbu(istate,isite,4)=vardel(ivar)

c               if ((isite.eq.1.and.ifhinge(istate)
c     >.eq.1).or.(ifhinge(istate)
c     >.eq.2)) then
c                  call all_sites_move(istate,iconf)
c                else
                  call one_site_move(istate,isite,iconf,1)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  cbu(istate,isite+7,iconf+4)=-varnew(ivar)
                  cbu(istate,isite+7,4)=vardel(ivar)
                  call d5c5sitelnk(istate,isite,iconf)
                  call one_site_move(istate,isite+7,iconf,0)
                endif                  
c.........
c                endif
              endif
            enddo
          else if (jcbu(istate,isite).eq.2) then
            if (j2cbu.eq.0) then
              j2cbu=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cbu(1,1,5)
                varmin(ivar)=cbu(1,1,2)
                varmax(ivar)=cbu(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      cbu(jstate,jsite,iconf+4)=varnew(ivar)
                      cbu(jstate,jsite,4)=vardel(ivar)
                      call one_site_move(jstate,jsite,iconf,1)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jcbu(istate,isite).eq.3) then
            if (j3cbu(istate).eq.0) then
              j3cbu(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cbu(istate,1,5)
                varmin(ivar)=cbu(istate,1,2)
                varmax(ivar)=cbu(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    cbu(istate,jsite,iconf+4)=varnew(ivar)
                    cbu(istate,jsite,4)=vardel(ivar)
                    call one_site_move(istate,jsite,iconf,1)
                  enddo
                enddo
              endif
            endif
          else if (jcbu(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=cbu(istate,isite,5)
              varmin(ivar)=cbu(istate,isite,2)
              varmax(ivar)=cbu(istate,isite,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                cbu(istate,isite,iconf+4)=varnew(ivar)
                cbu(istate,isite,4)=vardel(ivar)
                call one_site_move(istate,isite,iconf,1)
              enddo
            endif
          endif

          if (jaas(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=aas(istate,isite,iconf+4)
                varmin(ivar)=aas(istate,isite,2)
                varmax(ivar)=aas(istate,isite,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                aas(istate,isite,iconf+4)=varnew(ivar)
                aas(istate,isite,4)=vardel(ivar)

c                if (ifhinge(istate)
c     >.eq.1) then
c                  call all_sites_move(istate,iconf)
c                else
                  call one_site_move(istate,isite,iconf,1)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  aas(istate,isite+7,iconf+4)=9*36.d0/conv-varnew(ivar) !11*36.d0/conv-varnew(ivar)
                  aas(istate,isite+7,4)=vardel(ivar)
                  call d5c5sitelnk(istate,isite,iconf)
                  call one_site_move(istate,isite+7,iconf,0)
                endif                  
c.........
c                endif
              endif
            enddo
          else if (jaas(istate,isite).eq.2) then
            if (j2aas.eq.0) then
              j2aas=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=aas(1,1,5)
                varmin(ivar)=aas(1,1,2)
                varmax(ivar)=aas(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      aas(jstate,jsite,iconf+4)=varnew(ivar)
                      aas(jstate,jsite,4)=vardel(ivar)
                      call one_site_move(jstate,jsite,iconf,1)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jaas(istate,isite).eq.3) then
            if (j3aas(istate).eq.0) then
              j3aas(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=aas(istate,1,5)
                varmin(ivar)=aas(istate,1,2)
                varmax(ivar)=aas(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    aas(istate,jsite,iconf+4)=varnew(ivar)
                    aas(istate,jsite,4)=vardel(ivar)
                    call one_site_move(istate,jsite,iconf,1)
                  enddo
                enddo
              endif
            endif
          else if (jaas(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=aas(istate,isite,5)
              varmin(ivar)=aas(istate,isite,2)
              varmax(ivar)=aas(istate,isite,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                aas(istate,isite,iconf+4)=varnew(ivar)
                aas(istate,isite,4)=vardel(ivar)
                call one_site_move(istate,isite,iconf,1)
              enddo
            endif
          endif

          if (jcba(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cba(istate,isite,iconf+4)
                varmin(ivar)=cba(istate,isite,2)
                varmax(ivar)=cba(istate,isite,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                cba(istate,isite,iconf+4)=varnew(ivar)
                cba(istate,isite,4)=vardel(ivar)

c                if (ifhinge(istate)
c     >.eq.1) then
c                  call all_sites_move(istate,iconf)
c                else
                  call one_site_move(istate,isite,iconf,1)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  cba(istate,isite+7,iconf+4)=-varnew(ivar)
                  cba(istate,isite+7,4)=vardel(ivar)
                  call d5c5sitelnk(istate,isite,iconf)
                  call one_site_move(istate,isite+7,iconf,0)
                endif                  
c.........
c                endif
              endif
            enddo
          else if (jcba(istate,isite).eq.2) then
            if (j2cba.eq.0) then
              j2cba=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cba(1,1,5)
                varmin(ivar)=cba(1,1,2)
                varmax(ivar)=cba(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      cba(jstate,jsite,iconf+4)=varnew(ivar)
                      cba(jstate,jsite,4)=vardel(ivar)
                      call one_site_move(jstate,jsite,iconf,1)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jcba(istate,isite).eq.3) then
            if (j3cba(istate).eq.0) then
              j3cba(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=cba(istate,1,5)
                varmin(ivar)=cba(istate,1,2)
                varmax(ivar)=cba(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    cba(istate,jsite,iconf+4)=varnew(ivar)
                    cba(istate,jsite,4)=vardel(ivar)
                    call one_site_move(istate,jsite,iconf,1)
                  enddo
                enddo
              endif
            endif
          else if (jcba(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=cba(istate,isite,5)
              varmin(ivar)=cba(istate,isite,2)
              varmax(ivar)=cba(istate,isite,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                cba(istate,isite,iconf+4)=varnew(ivar)
                cba(istate,isite,4)=vardel(ivar)
                call one_site_move(istate,isite,iconf,1)
              enddo
            endif
          endif

          if (jgas(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=gas(istate,isite,iconf+4)
                varmin(ivar)=gas(istate,isite,2)
                varmax(ivar)=gas(istate,isite,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                gas(istate,isite,iconf+4)=varnew(ivar)
                gas(istate,isite,4)=vardel(ivar)

c                if (ifhinge(istate)
c     >.eq.1) then
c                  call all_sites_move(istate,iconf)
c                else
                  call one_site_move(istate,isite,iconf,1)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  gas(istate,isite+7,iconf+4)=varnew(ivar)+pi!varnew(ivar)-pi
                  gas(istate,isite+7,4)=vardel(ivar)
                  call d5c5sitelnk(istate,isite,iconf)
                  call one_site_move(istate,isite+7,iconf,0)
                endif                  
c.........
                endif
c              endif
            enddo
          else if (jgas(istate,isite).eq.2) then
            if (j2gas.eq.0) then
              j2gas=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=gas(1,1,5)
                varmin(ivar)=gas(1,1,2)
                varmax(ivar)=gas(1,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      gas(jstate,jsite,iconf+4)=varnew(ivar)
                      gas(jstate,jsite,4)=vardel(ivar)
                      call one_site_move(jstate,jsite,iconf,1)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jgas(istate,isite).eq.3) then
            if (j3gas(istate).eq.0) then
              j3gas(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                varold(ivar)=gas(istate,1,5)
                varmin(ivar)=gas(istate,1,2)
                varmax(ivar)=gas(istate,1,3)
                call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    gas(istate,jsite,iconf+4)=varnew(ivar)
                    gas(istate,jsite,4)=vardel(ivar)
                    call one_site_move(istate,jsite,iconf,1)
                  enddo
                enddo
              endif
            endif
          else if (jgas(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              varold(ivar)=gas(istate,isite,5)
              varmin(ivar)=gas(istate,isite,2)
              varmax(ivar)=gas(istate,isite,3)
              call move(vardel(ivar),varnew(ivar),varold(ivar),
     >               varmin(ivar),varmax(ivar),vartot(ivar),iseed)
              do iconf=1,nconf(istate)
                gas(istate,isite,iconf+4)=varnew(ivar)
                gas(istate,isite,4)=vardel(ivar)
                call one_site_move(istate,isite,iconf,1)
              enddo
            endif
          endif
        enddo ! end isite




c.......Vogliamo provare con una mossa di linker?
c        write(*,*) ' dentro',nlink(istate)
        do iilink=1,nlink(istate)
          if (jlink(istate,iilink).eq.2) then !! variazioni indipendenti di phi,psi,chi
c            n=naalink(istate,iilink)
c            do iaa=1,n
              do iconf=1,nconf(istate)
                jvar=jvar+1
                if (ivar.eq.jvar) then
                  call one_linker_move(istate,iilink,iconf,1)
c.....D5/C5 ... devono essere 0 i flag dei linker dal settimo all'undicesimo
                  if (id5c5.eq.1.and.iilink.le.5) then
                    call d5c5lnk(istate,iilink,iconf)
                    call one_linker_move(istate,iilink+6,iconf,0)
                  endif                  
c.........
                endif
              enddo
c            enddo
          endif
        enddo


      enddo ! end istate
c                 write(*,*) ' fine ciclo_sub'
    
      return
      end

      subroutine all_sites_screw_move(istate,ihagg,iconf)
      include 'quafit2014.cmn'
      call framhag(istate,ihagg,iconf)

      do iilink=1,nlink(istate)
        call cp_f_cst(istate,nsite(istate)+iilink,ihagg,iconf)
        call parconstlinker(istate,iilink,ihagg,iconf)
      enddo
c per me basta cosi' perche' se muovi screw non c'e' bisogno di un nuovo test sui linker
      do isite=1,nsite(istate)
        call cp_f_cst(istate,isite,ihagg,iconf)
        call parconst(istate,isite,ihagg,iconf)
      enddo
      do idis=1,ndist(istate,ihagg)
        overm_dis(istate,ihagg,idis,iconf)=
     >  overl_dis(istate,ihagg,idis,iconf)
      enddo
      overm(istate,ihagg,iconf)=overl(istate,ihagg,iconf)
      call framalllink(istate,ihagg,iconf)
      call test_disv(istate,ihagg,iconf)
      call cp_parz_2(istate,ihagg,iconf)
      call pq_parz_2(istate,ihagg,iconf)
      return
      end

      subroutine all_sites_screw_restore(istate,ihagg,iconf)
      include 'quafit2014.cmn'
      do iilink=1,nlink(istate)
        call re_f_cst(istate,nsite(istate)+iilink,ihagg,iconf)
      enddo
c
      do isite=1,nsite(istate)
        call re_f_cst(istate,isite,ihagg,iconf)
      enddo
      do idis=1,ndist(istate,ihagg)
        overl_dis(istate,ihagg,idis,iconf)=
     >  overm_dis(istate,ihagg,idis,iconf)
      enddo
      call framhag(istate,ihagg,iconf)
      overl(istate,ihagg,iconf)=overm(istate,ihagg,iconf)
      call re_parz_2(istate,ihagg,iconf)
      return
      end


      subroutine one_site_move(istate,isite,iconf,imvlink)
      include 'quafit2014.cmn'

      nl=ilinksite(istate,isite,1)!numero di linkers attaccati al sito isite
      if (nlink(istate).gt.0) then
      do k=1,nl
        iilink=ilinksite(istate,isite,1+k)
        do ihagg=1,nhagg(istate)
          call cp_f_cst(istate,nsite(istate)+iilink,ihagg,iconf)
        enddo
        call linkerendsfind(istate,iilink,iconf) !!estremita'
        qverm_lnk(istate,iilink,iconf)=
     >  qverl_lnk(istate,iilink,iconf)
        call linkercoo(imvlink,istate,iilink,iconf)  !!trova la conformazione
        call fulllinker(istate,iilink,iconf) !!trova gli alm
        do ihagg=1,nhagg(istate)
          call framhag(istate,ihagg,iconf)
          call parconstlinker(istate,iilink,ihagg,iconf)
        enddo
      enddo
      qverm(istate,iconf)=qverl(istate,iconf)
      dum3=zer
      do iilink=1,nlink(istate)
        dum3=dum3+qverl_lnk(istate,iilink,iconf)
      enddo
      qverl(istate,iconf)=dum3/nlink(istate)
      endif
     
      cverm(istate,iconf)=cverl(istate,iconf)
      do ihagg=1,nhagg(istate)
        call cp_f_cst(istate,isite,ihagg,iconf)
        call framhag(istate,ihagg,iconf)
        call framalllink(istate,ihagg,iconf)
        if (ihagg.eq.1) call test_constr(istate,iconf) 
        call parconst(istate,isite,ihagg,iconf)
        overm(istate,ihagg,iconf)=overl(istate,ihagg,iconf)
        do idis=1,ndist(istate,ihagg)
          overm_dis(istate,ihagg,idis,iconf)=
     >    overl_dis(istate,ihagg,idis,iconf)
        enddo
        
        call test_disv_site(istate,isite,ihagg,iconf)
        do k=1,nl
          iilink=ilinksite(istate,isite,1+k)
          call test_disv_site(istate,nsite(istate)+iilink,ihagg,iconf)
        enddo
c        write(*,*) ' AAA ',overl(istate,ihagg,iconf)
c        overl(istate,ihagg,iconf)=zer
c        do idis=1,ndist(istate,ihagg)
c          overl(istate,ihagg,iconf)=overl(istate,ihagg,iconf)+
c     >    overl_dis(istate,ihagg,idis,iconf)
c        enddo
c        write(*,*) ' CC ',overl(istate,ihagg,iconf)

c        call test_disv(istate,ihagg,iconf)
c        write(*,*) ' BB ',overl(istate,ihagg,iconf)

        call cp_parz_2(istate,ihagg,iconf)
        call pq_parz_2(istate,ihagg,iconf)
      enddo
      return
      end

      subroutine d5c5sitelnk(istate,isite,iconf)
      include 'quafit2014.cmn'

      if (nlink(istate).gt.0) then
      nl=ilinksite(istate,isite,1)!numero di linkers attaccati al sito isite
      do k=1,nl
        iilink=ilinksite(istate,isite,1+k)
        n=naalink(istate,iilink)
        jconf=iconf+4
        do iaa=1,n
          phi_lnk(istate,iilink+6,jconf,iaa)=
     >    phi_lnk(istate,iilink,jconf,iaa)
          phi_lnk(istate,iilink+6,4,iaa)=
     >    phi_lnk(istate,iilink,4,iaa)
          psi_lnk(istate,iilink+6,jconf,iaa)=
     >    psi_lnk(istate,iilink,jconf,iaa)
          psi_lnk(istate,iilink+6,4,iaa)=
     >    psi_lnk(istate,iilink,4,iaa)
          chi_lnk(istate,iilink+6,jconf,iaa)=
     >    chi_lnk(istate,iilink,jconf,iaa)
          chi_lnk(istate,iilink+6,4,iaa)=
     >    chi_lnk(istate,iilink,4,iaa)
        enddo
        do iaa=n+1,n+1
          phi_lnk(istate,iilink+6,jconf,iaa)=
     >    phi_lnk(istate,iilink,jconf,iaa)
          phi_lnk(istate,iilink+6,4,iaa)=
     >    phi_lnk(istate,iilink,4,iaa)
        enddo
      enddo
      endif
      return
      end

      subroutine d5c5lnk(istate,iilink,iconf)
      include 'quafit2014.cmn'

      n=naalink(istate,iilink)
      jconf=iconf+4
      do iaa=1,n
        phi_lnk(istate,iilink+6,jconf,iaa)=
     >  phi_lnk(istate,iilink,jconf,iaa)
        phi_lnk(istate,iilink+6,4,iaa)=
     >  phi_lnk(istate,iilink,4,iaa)
        psi_lnk(istate,iilink+6,jconf,iaa)=
     >  psi_lnk(istate,iilink,jconf,iaa)
        psi_lnk(istate,iilink+6,4,iaa)=
     >  psi_lnk(istate,iilink,4,iaa)
        chi_lnk(istate,iilink+6,jconf,iaa)=
     >  chi_lnk(istate,iilink,jconf,iaa)
        chi_lnk(istate,iilink+6,4,iaa)=
     >  chi_lnk(istate,iilink,4,iaa)
      enddo
      do iaa=n+1,n+1
        phi_lnk(istate,iilink+6,jconf,iaa)=
     >  phi_lnk(istate,iilink,jconf,iaa)
        phi_lnk(istate,iilink+6,4,iaa)=
     >  phi_lnk(istate,iilink,4,iaa)
      enddo
      return
      end

      subroutine one_site_restore(istate,isite,iconf)
      include 'quafit2014.cmn'

      if (nlink(istate).gt.0) then
      nl=ilinksite(istate,isite,1)!numero di linkers attaccati al sito isite
      do k=1,nl
        iilink=ilinksite(istate,isite,1+k)
        do ihagg=1,nhagg(istate)
          call re_f_cst(istate,nsite(istate)+iilink,ihagg,iconf)
        enddo
c.......ora dovrai ripristinare le vecchi variabili degli angoli diedri
        n=naalink(istate,iilink)
        jconf=iconf+4
        do iaa=1,n
          phi_lnk(istate,iilink,jconf,iaa)=
     >    phi_old(istate,iilink,jconf,iaa)
          phi_lnk(istate,iilink,4,iaa)=
     >    phi_old(istate,iilink,4,iaa)
          psi_lnk(istate,iilink,jconf,iaa)=
     >    psi_old(istate,iilink,jconf,iaa)
          psi_lnk(istate,iilink,4,iaa)=
     >    psi_old(istate,iilink,4,iaa)
          chi_lnk(istate,iilink,jconf,iaa)=
     >    chi_old(istate,iilink,jconf,iaa)
          chi_lnk(istate,iilink,4,iaa)=
     >    chi_old(istate,iilink,4,iaa)
          do ia=1,2
          umsaa(istate,iilink,iconf,iaa,ia)=
     >    umsaa_old(istate,iilink,iconf,iaa,ia)
          ausaa(istate,iilink,iconf,iaa,ia)=
     >    ausaa_old(istate,iilink,iconf,iaa,ia)
          cbuaa(istate,iilink,iconf,iaa,ia)=
     >    cbuaa_old(istate,iilink,iconf,iaa,ia)
          aasaa(istate,iilink,iconf,iaa,ia)=
     >    aasaa_old(istate,iilink,iconf,iaa,ia)
          cbaaa(istate,iilink,iconf,iaa,ia)=
     >    cbaaa_old(istate,iilink,iconf,iaa,ia)
          gasaa(istate,iilink,iconf,iaa,ia)=
     >    gasaa_old(istate,iilink,iconf,iaa,ia)
          enddo
        enddo
        do iaa=n+1,n+1
          phi_lnk(istate,iilink,jconf,iaa)=
     >    phi_old(istate,iilink,jconf,iaa)
          phi_lnk(istate,iilink,4,iaa)=
     >    phi_old(istate,iilink,4,iaa)
        enddo
        do ia=1,3
        do ib=1,3
          terminals(istate,iilink,iconf,ia,ib)=
     >    terminals_old(istate,iilink,iconf,ia,ib)
        enddo
        enddo
c...............................
        qverl_lnk(istate,iilink,iconf)=
     >  qverm_lnk(istate,iilink,iconf)
        call linkerendsfind(istate,iilink,iconf) !!estremita'
      enddo
      qverl(istate,iconf)=qverm(istate,iconf)
      endif

      do ihagg=1,nhagg(istate)
        call re_f_cst(istate,isite,ihagg,iconf)
        call framhag(istate,ihagg,iconf)
        call framalllink(istate,ihagg,iconf)
        overl(istate,ihagg,iconf)=overm(istate,ihagg,iconf)
        call re_parz_2(istate,ihagg,iconf)
        do idis=1,ndist(istate,ihagg)
          overl_dis(istate,ihagg,idis,iconf)=
     >    overm_dis(istate,ihagg,idis,iconf)
        enddo
      enddo
      qverl(istate,iconf)=qverm(istate,iconf)
      cverl(istate,iconf)=cverm(istate,iconf)
      return
      end




      subroutine one_linker_move(istate,iilink,iconf,imvlink)
      include 'quafit2014.cmn'

c      write(*,*) '---> one_linker_move',istate,iilink,iconf,imvlink
      do ihagg=1,nhagg(istate)
        call cp_f_cst(istate,nsite(istate)+iilink,ihagg,iconf)
      enddo
      call linkerendsfind(istate,iilink,iconf) !!estremita'
      qverm_lnk(istate,iilink,iconf)=
     >qverl_lnk(istate,iilink,iconf)
      call linkercoo(imvlink,istate,iilink,iconf)  !!trova la conformazione
      call fulllinker(istate,iilink,iconf) !!trova gli alm
      do ihagg=1,nhagg(istate)
        call framhag(istate,ihagg,iconf)
        call parconstlinker(istate,iilink,ihagg,iconf)
      enddo
      qverm(istate,iconf)=qverl(istate,iconf)
      dum3=zer
      do jilink=1,nlink(istate)
        dum3=dum3+qverl_lnk(istate,jilink,iconf)
      enddo
      qverl(istate,iconf)=dum3/nlink(istate)
     
      cverm(istate,iconf)=cverl(istate,iconf)
      do ihagg=1,nhagg(istate)
        call framhag(istate,ihagg,iconf)
        call framalllink(istate,ihagg,iconf)
        if (ihagg.eq.1) call test_constr(istate,iconf) 
        overm(istate,ihagg,iconf)=overl(istate,ihagg,iconf)
        do idis=1,ndist(istate,ihagg)
          overm_dis(istate,ihagg,idis,iconf)=
     >    overl_dis(istate,ihagg,idis,iconf)
        enddo
        call test_disv_site(istate,nsite(istate)+iilink,ihagg,iconf)

        call cp_parz_2(istate,ihagg,iconf)
        call pq_parz_2(istate,ihagg,iconf)
      enddo
      return
      end


      subroutine one_linker_restore(istate,iilink,iconf)
      include 'quafit2014.cmn'

      do ihagg=1,nhagg(istate)
        call re_f_cst(istate,nsite(istate)+iilink,ihagg,iconf)
      enddo
c.....ora dovrai ripristinare le vecchi variabili degli angoli diedri
      n=naalink(istate,iilink)
      jconf=iconf+4
      do iaa=1,n
          phi_lnk(istate,iilink,jconf,iaa)=
     >    phi_old(istate,iilink,jconf,iaa)
          phi_lnk(istate,iilink,4,iaa)=
     >    phi_old(istate,iilink,4,iaa)
          psi_lnk(istate,iilink,jconf,iaa)=
     >    psi_old(istate,iilink,jconf,iaa)
          psi_lnk(istate,iilink,4,iaa)=
     >    psi_old(istate,iilink,4,iaa)
          chi_lnk(istate,iilink,jconf,iaa)=
     >    chi_old(istate,iilink,jconf,iaa)
          chi_lnk(istate,iilink,4,iaa)=
     >    chi_old(istate,iilink,4,iaa)
          do ia=1,2
          umsaa(istate,iilink,iconf,iaa,ia)=
     >    umsaa_old(istate,iilink,iconf,iaa,ia)
          ausaa(istate,iilink,iconf,iaa,ia)=
     >    ausaa_old(istate,iilink,iconf,iaa,ia)
          cbuaa(istate,iilink,iconf,iaa,ia)=
     >    cbuaa_old(istate,iilink,iconf,iaa,ia)
          aasaa(istate,iilink,iconf,iaa,ia)=
     >    aasaa_old(istate,iilink,iconf,iaa,ia)
          cbaaa(istate,iilink,iconf,iaa,ia)=
     >    cbaaa_old(istate,iilink,iconf,iaa,ia)
          gasaa(istate,iilink,iconf,iaa,ia)=
     >    gasaa_old(istate,iilink,iconf,iaa,ia)
          enddo
      enddo
      do iaa=n+1,n+1
          phi_lnk(istate,iilink,jconf,iaa)=
     >    phi_old(istate,iilink,jconf,iaa)
          phi_lnk(istate,iilink,4,iaa)=
     >    phi_old(istate,iilink,4,iaa)
      enddo
      do ia=1,3
      do ib=1,3
        terminals(istate,iilink,iconf,ia,ib)=
     >  terminals_old(istate,iilink,iconf,ia,ib)
      enddo
      enddo
c...............................
      qverl_lnk(istate,iilink,iconf)=
     >qverm_lnk(istate,iilink,iconf)
      call linkerendsfind(istate,iilink,iconf) !!estremita'
      qverl(istate,iconf)=qverm(istate,iconf)

      do ihagg=1,nhagg(istate)
        call framhag(istate,ihagg,iconf)
        call framalllink(istate,ihagg,iconf)
        overl(istate,ihagg,iconf)=overm(istate,ihagg,iconf)
        call re_parz_2(istate,ihagg,iconf)
        do idis=1,ndist(istate,ihagg)
          overl_dis(istate,ihagg,idis,iconf)=
     >    overm_dis(istate,ihagg,idis,iconf)
        enddo
      enddo
      qverl(istate,iconf)=qverm(istate,iconf)!dec 2014
      cverl(istate,iconf)=cverm(istate,iconf)!dec 2014
      return
      end




      subroutine move(vdel,vnew,vold,vmin,vmax,vtot,iseed)

      implicit real*8 (a-h,o-z)

      dum1=(rand(0)-0.5d0)*vdel
      vnew=vold+dum1
      if (vnew.gt.vmax) then
c.nobounce        vnew=vnew-int((vnew-vmin)/(vmax-vmin))*(vmax-vmin)
        i1=int((vnew-vmin)/(vmax-vmin))
        vnew=(vmax+vmin)*(1-(-1)**i1)/2+(-1)**i1*(vnew-i1*(vmax-vmin))
      else if (vnew.lt.vmin) then
c.nobounce         vnew=vnew-int((vnew-vmax)/(vmin-vmax))*(vmin-vmax)
        i1=int((vnew-vmax)/(vmin-vmax))
        vnew=(vmax+vmin)*(1-(-1)**i1)/2+(-1)**i1*(vnew-i1*(vmin-vmax))
      endif
c      if (vnew.gt.vmax.or.vnew.lt.vmin) stop 'move'
      vtot=vtot+1.d0
      return
      end

      subroutine ciclo_res(ivar)

      include 'quafit2014.cmn'

c.....Questo schema potrebbe essere del tutto generale e potresti usarlo all'interno di un ciclo. In base a quello che cambi, potresti poi decidere cosa chiamare tra le varie subroutine, potresti anche richiamare i file di memorizzazione e rimpiazzarli o ripristinarli a seconda dell'accettazione della mossa MC

c.....Rimette al valore "old" la variabile ivar, controllando la sua "natura"

c     F=fisso, V=variabile, U=unico

c     Stato Aggregato Configurazione
c           Sito
c j=0 F     F         F
c j=1 V     V         V
c j=2 U     U         U j2NAME
c j=3 V     U         U j3NAME(istate)
c j=4 V     V         U 
c                                     isite


c      write(*,*) '   ---> ciclo_res  '

      j2ascrew=0
      j2alpscrew=0
      j2cbescrew=0
      j2dscrew=0
      j2rmeff=0
      do k=1,nsfull  
      j2rhcl(k)=0
      enddo
      do k=1,mxtherf  
      do j=1,neffec+1    
      j2ftherm(k,j)=0
      enddo
      enddo
      j2ums=0
      j2aus=0
      j2cbu=0
      j2aas=0
      j2cba=0
      j2gas=0

      jvar=0
      do istate=1,nstate

        j3ascrew(istate)=0
        j3alpscrew(istate)=0
        j3cbescrew(istate)=0
        j3dscrew(istate)=0
        j3rmeff(istate)=0
        do k=1,nsfull  
        j3rhcl(istate,k)=0
        enddo
        do k=1,mxtherf  
        do j=1,neffec+1    
        j3ftherm(istate,k,j)=0
        enddo
        enddo
        j3ums(istate)=0
        j3aus(istate)=0
        j3cbu(istate)=0
        j3aas(istate)=0
        j3cba(istate)=0
        j3gas(istate)=0

        do ihagg=1,nhagg(istate)

          if (jascrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                ascrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              endif
            enddo
          else if (jascrew(istate,ihagg).eq.2) then
            if (j2ascrew.eq.0) then
              j2ascrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      ascrew(jstate,jhagg,iconf+4)=varold(ivar)
                      call all_sites_screw_restore(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jascrew(istate,ihagg).eq.3) then
            if (j3ascrew(istate).eq.0) then
              j3ascrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    ascrew(istate,jhagg,iconf+4)=varold(ivar)
                    call all_sites_screw_restore(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jascrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                ascrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jalpscrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                alpscrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              endif
            enddo
          else if (jalpscrew(istate,ihagg).eq.2) then
            if (j2alpscrew.eq.0) then
              j2alpscrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      alpscrew(jstate,jhagg,iconf+4)=varold(ivar)
                      call all_sites_screw_restore(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jalpscrew(istate,ihagg).eq.3) then
            if (j3alpscrew(istate).eq.0) then
              j3alpscrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    alpscrew(istate,jhagg,iconf+4)=varold(ivar)
                    call all_sites_screw_restore(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jalpscrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                alpscrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jcbescrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                cbescrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              endif
            enddo
          else if (jcbescrew(istate,ihagg).eq.2) then
            if (j2cbescrew.eq.0) then
              j2cbescrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      cbescrew(jstate,jhagg,iconf+4)=varold(ivar)
                      call all_sites_screw_restore(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jcbescrew(istate,ihagg).eq.3) then
            if (j3cbescrew(istate).eq.0) then
              j3cbescrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    cbescrew(istate,jhagg,iconf+4)=varold(ivar)
                    call all_sites_screw_restore(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jcbescrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                cbescrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jdscrew(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                dscrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              endif
            enddo
          else if (jdscrew(istate,ihagg).eq.2) then
            if (j2dscrew.eq.0) then
              j2dscrew=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      dscrew(jstate,jhagg,iconf+4)=varold(ivar)
                      call all_sites_screw_restore(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jdscrew(istate,ihagg).eq.3) then
            if (j3dscrew(istate).eq.0) then
              j3dscrew(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    dscrew(istate,jhagg,iconf+4)=varold(ivar)
                    call all_sites_screw_restore(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jdscrew(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                dscrew(istate,ihagg,iconf+4)=varold(ivar)
                call all_sites_screw_restore(istate,ihagg,iconf)
              enddo
            endif
          endif

          if (jrmeff(istate,ihagg).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                rmeff(istate,ihagg,iconf+4)=varold(ivar)
              endif
            enddo
          else if (jrmeff(istate,ihagg).eq.2) then
            if (j2rmeff.eq.0) then
              j2rmeff=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      rmeff(jstate,jhagg,iconf+4)=varold(ivar)
                      call re_parz_2(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jrmeff(istate,ihagg).eq.3) then
            if (j3rmeff(istate).eq.0) then
              j3rmeff(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    rmeff(istate,jhagg,iconf+4)=varold(ivar)
                    call re_parz_2(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jrmeff(istate,ihagg).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                rmeff(istate,ihagg,iconf+4)=varold(ivar)
                call re_parz_2(istate,ihagg,iconf)
              enddo
            endif
          endif


          do k=1,nsfull  
          if (jrhcl(istate,ihagg,k).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                rhcl(istate,ihagg,k,iconf+4)=varold(ivar)
                call re_parz_2(istate,ihagg,iconf)
              endif
            enddo
          else if (jrhcl(istate,ihagg,k).eq.2) then
            if (j2rhcl(k).eq.0) then
              j2rhcl(k)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    do iconf=1,nconf(jstate)
                      rhcl(jstate,jhagg,k,iconf+4)=varold(ivar)
                      call re_parz_2(jstate,jhagg,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jrhcl(istate,ihagg,k).eq.3) then
            if (j3rhcl(istate,k).eq.0) then
              j3rhcl(istate,k)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  do iconf=1,nconf(istate)
                    rhcl(istate,jhagg,k,iconf+4)=varold(ivar)
                    call re_parz_2(istate,jhagg,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jrhcl(istate,ihagg,k).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                rhcl(istate,ihagg,k,iconf+4)=varold(ivar)
                call re_parz_2(istate,ihagg,iconf)
              enddo
            endif
          endif
          enddo

          do k=1,mxtherf  
          do j=1,neffec+1    
          if (jftherm(istate,ihagg,k,j).eq.1) then
            stop 'jftherm'
          else if (jftherm(istate,ihagg,k,j).eq.2) then
            if (j2ftherm(k,j).eq.0) then
              j2ftherm(k,j)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jhagg=1,nhagg(jstate)
                    ftherm(jstate,jhagg,k,j,1)=varold(ivar)
                    do nn=1,nexper
                      fract(jstate,jhagg,nn)=fracv(jstate,jhagg,nn)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jftherm(istate,ihagg,k,j).eq.3) then
            if (j3ftherm(istate,k,j).eq.0) then
              j3ftherm(istate,k,j)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jhagg=1,nhagg(istate)
                  ftherm(istate,jhagg,k,j,1)=varold(ivar)
                  do nn=1,nexper
                    fract(istate,jhagg,nn)=fracv(istate,jhagg,nn)
                  enddo
                enddo
              endif
            endif
          else if (jftherm(istate,ihagg,k,j).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              ftherm(istate,ihagg,k,j,1)=varold(ivar)
              do nn=1,nexper
                fract(istate,ihagg,nn)=fracv(istate,ihagg,nn)
              enddo
            endif
          endif
          enddo
          enddo

          if (jfracm.eq.1) then
            do nn=1,nexper  
              jvar=jvar+1
              if (ivar.eq.jvar) then
                xfrac(jaghag(istate,ihagg),nn,1)=varold(ivar)
                do jstate=1,nstate
                do jhagg=1,nhagg(jstate)
                  fract(jstate,jhagg,nn)=fracv(jstate,jhagg,nn)
                enddo
                enddo
              endif
            enddo
          endif

c          if (jfracm.eq.1.and.jaghag(istate,ihagg).gt.1) then
c            do nn=1,nexper  
c              jvar=jvar+1
c              if (ivar.eq.jvar) then
c                xfrac(jaghag(istate,ihagg)-1,nn,1)=varold(ivar)
c                do jstate=1,nstate
c                do jhagg=1,nhagg(jstate)
c                  fract(jstate,jhagg,nn)=fracv(jstate,jhagg,nn)
c                enddo
c                enddo
c              endif
c            enddo
c          endif


        enddo   !! fine del loop su ihagg, rimani in un loop su istate

        do isite=1,nsite(istate)

          if (jums(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                ums(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  ums(istate,isite+7,iconf+4)=ums(istate,isite,iconf+4)
                  call one_site_restore(istate,isite+7,iconf)
                endif                  
c.........
              endif
            enddo
          else if (jums(istate,isite).eq.2) then
            if (j2ums.eq.0) then
              j2ums=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      ums(jstate,jsite,iconf+4)=varold(ivar)
                      call one_site_restore(jstate,jsite,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jums(istate,isite).eq.3) then
            if (j3ums(istate).eq.0) then
              j3ums(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    ums(istate,jsite,iconf+4)=varold(ivar)
                    call one_site_restore(istate,jsite,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jums(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                ums(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
              enddo
            endif
          endif

          if (jaus(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                aus(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  aus(istate,isite+7,iconf+4)=
c     >11*36.d0/conv-aus(istate,isite,iconf+4)
     >9*36.d0/conv-aus(istate,isite,iconf+4)
                  call one_site_restore(istate,isite+7,iconf)
                endif                  
c.........
              endif
            enddo
          else if (jaus(istate,isite).eq.2) then
            if (j2aus.eq.0) then
              j2aus=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      aus(jstate,jsite,iconf+4)=varold(ivar)
                      call one_site_restore(jstate,jsite,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jaus(istate,isite).eq.3) then
            if (j3aus(istate).eq.0) then
              j3aus(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    aus(istate,jsite,iconf+4)=varold(ivar)
                    call one_site_restore(istate,jsite,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jaus(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                aus(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
              enddo
            endif
          endif

          if (jcbu(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                cbu(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  cbu(istate,isite+7,iconf+4)=
     >-cbu(istate,isite,iconf+4)
                  call one_site_restore(istate,isite+7,iconf)
                endif                  
c.........
              endif
            enddo
          else if (jcbu(istate,isite).eq.2) then
            if (j2cbu.eq.0) then
              j2cbu=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      cbu(jstate,jsite,iconf+4)=varold(ivar)
                      call one_site_restore(jstate,jsite,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jcbu(istate,isite).eq.3) then
            if (j3cbu(istate).eq.0) then
              j3cbu(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    cbu(istate,jsite,iconf+4)=varold(ivar)
                    call one_site_restore(istate,jsite,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jcbu(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                cbu(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
              enddo
            endif
          endif

          if (jaas(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                aas(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  aas(istate,isite+7,iconf+4)=
c     >11*36.d0/conv-aas(istate,isite,iconf+4)
     >9*36.d0/conv-aas(istate,isite,iconf+4)
                  call one_site_restore(istate,isite+7,iconf)
                endif                  
c.........
              endif
            enddo
          else if (jaas(istate,isite).eq.2) then
            if (j2aas.eq.0) then
              j2aas=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      aas(jstate,jsite,iconf+4)=varold(ivar)
                      call one_site_restore(jstate,jsite,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jaas(istate,isite).eq.3) then
            if (j3aas(istate).eq.0) then
              j3aas(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    aas(istate,jsite,iconf+4)=varold(ivar)
                    call one_site_restore(istate,jsite,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jaas(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                aas(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
              enddo
            endif
          endif

          if (jcba(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                cba(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  cba(istate,isite+7,iconf+4)=
     >-cba(istate,isite,iconf+4)
                  call one_site_restore(istate,isite+7,iconf)
                endif                  
c.........
              endif
            enddo
          else if (jcba(istate,isite).eq.2) then
            if (j2cba.eq.0) then
              j2cba=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      cba(jstate,jsite,iconf+4)=varold(ivar)
                      call one_site_restore(jstate,jsite,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jcba(istate,isite).eq.3) then
            if (j3cba(istate).eq.0) then
              j3cba(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    cba(istate,jsite,iconf+4)=varold(ivar)
                    call one_site_restore(istate,jsite,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jcba(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                cba(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
              enddo
            endif
          endif

          if (jgas(istate,isite).eq.1) then
            do iconf=1,nconf(istate)
              jvar=jvar+1
              if (ivar.eq.jvar) then
                gas(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                if (id5c5.eq.1.and.isite.le.6) then
                  gas(istate,isite+7,iconf+4)=
c     >gas(istate,isite,iconf+4)-pi
     >gas(istate,isite,iconf+4)+pi
                  call one_site_restore(istate,isite+7,iconf)
                endif                  
c.........
              endif
            enddo
          else if (jgas(istate,isite).eq.2) then
            if (j2gas.eq.0) then
              j2gas=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jstate=1,nstate
                  do jsite=1,nsite(jstate)
                    do iconf=1,nconf(jstate)
                      gas(jstate,jsite,iconf+4)=varold(ivar)
                      call one_site_restore(jstate,jsite,iconf)
                    enddo
                  enddo
                enddo
              endif
            endif
          else if (jgas(istate,isite).eq.3) then
            if (j3gas(istate).eq.0) then
              j3gas(istate)=1
              jvar=jvar+1
              if (ivar.eq.jvar) then
                do jsite=1,nsite(istate)
                  do iconf=1,nconf(istate)
                    gas(istate,jsite,iconf+4)=varold(ivar)
                    call one_site_restore(istate,jsite,iconf)
                  enddo
                enddo
              endif
            endif
          else if (jgas(istate,isite).eq.4) then
            jvar=jvar+1
            if (ivar.eq.jvar) then
              do iconf=1,nconf(istate)
                gas(istate,isite,iconf+4)=varold(ivar)
                call one_site_restore(istate,isite,iconf)
              enddo
            endif
          endif
        enddo ! end isite

c.......Vogliamo provare con una mossa di linker?
        do iilink=1,nlink(istate)
          if (jlink(istate,iilink).eq.2) then !! variazioni indipendenti di phi,psi,chi
c            n=naalink(istate,iilink)
c            do iaa=1,n
              do iconf=1,nconf(istate)
                jvar=jvar+1
                if (ivar.eq.jvar) then
                  call one_linker_restore(istate,iilink,iconf)
c.....D5/C5 ... devono essere 0 i flag dall'ottavo al tredicesimo sito
                  if (id5c5.eq.1.and.iilink.le.5) then
                    call one_linker_restore(istate,iilink+6,iconf)
                  endif                  
c.........
                endif
              enddo
c            enddo
          endif
        enddo

      enddo ! end istate
      return
      end


      subroutine outparz

      include 'quafit2014.cmn'

c      write(*,*) ' outparz ---> '
      open(unit=iu_tmp,status='unknown',
     >       file='qua'//code//subcode//'.out')

      string=' ********************* Output data file '
      write(iu_tmp,4100) string
      string='                                        '
      write(iu_tmp,4100) string

      string=' Chi Square ...........................:'
      write(iu_tmp,2200) string,chiold
      string=' Average Elastic Energy, A(X) .........:'
      write(iu_tmp,2200) string,t_qverl_old*w_kforce
      string=' Average LJ Potential, B(X) ...........:'
      write(iu_tmp,2200) string,t_overl_old*w_overl
      string=' Average Constraints, C(X) ...........:'
      write(iu_tmp,2200) string,t_cverl_old
      string=' Average Elastic Energy Adim., A(X)/w_A:'
      write(iu_tmp,2200) string,t_qverl_old
      string=' Average LJ Potential Adim., B(X)/w_B .:'
      write(iu_tmp,2200) string,t_overl_old

      i121=0
      dum7=0.d0
      do istate=1,nstate
      do iconf=1,nconf(istate)
      do ihagg=1,nhagg(istate)
      ndis=ndist(istate,ihagg)
      ov=zer
      dum3=zer
c      write(iu_tmp,*) ' adfad ',istate, iconf, ihagg
      do idis=1,ndis
        isite=ipair(istate,ihagg,idis,1)
        isub=ipair(istate,ihagg,idis,2)
        ipoly=ipair(istate,ihagg,idis,3)
        jsite=ipair(istate,ihagg,idis,4)
        jsub=ipair(istate,ihagg,idis,5)
        jpoly=ipair(istate,ihagg,idis,6)
        m=ipair(istate,ihagg,idis,7)!molteplicita'
        i1=ipair(istate,ihagg,idis,8)!connessione
        i101=ipair(istate,ihagg,idis,9)!tipo di coppia
c        write(iu_tmp,*) (ipair(istate,ihagg,idis,kk),kk=1,9)
c        write(iu_tmp,*) overl_dis(istate,ihagg,idis,iconf)
        if (overl_dis(istate,ihagg,idis,iconf).ne.0) then
        if (i101.eq.1) then
c          write(iu_tmp,'(a)') 
c     >' istate ihagg  iconf  isite  jsite  ULJ_ij'
c          write(iu_tmp,'(1x,5(i4,3x),g10.3)') 
c     >      istate,ihagg,iconf,isite,jsite,
c     >      overl_dis(istate,ihagg,idis,iconf)
        else if (i101.eq.2) then
c          write(iu_tmp,'(a)') 
c     >' istate ihagg  iconf  isite  jlink  ULJ_ij'
c          write(iu_tmp,'(1x,5(i4,3x),g10.3)') 
c     >      istate,ihagg,iconf,isite,
c     >      jsite-nsite(istate),overl_dis(istate,ihagg,idis,iconf)
        else if (i101.eq.3) then
c          write(iu_tmp,'(a)') 
c     >' istate ihagg  iconf  ilink  jsite  ULJ_ij'
c          write(iu_tmp,'(1x,5(i4,3x),g10.3)') 
c     >    istate,ihagg,iconf,
c     >      isite-nsite(istate),jsite,overl_dis(istate,ihagg,idis,iconf)
        else
c          write(iu_tmp,'(a)') 
c     >' istate ihagg  iconf  ilink  jlink  ULJ_ij'
c          write(iu_tmp,'(1x,5(i4,3x),g10.3)') 
c     >   istate,ihagg,iconf,
c     >isite-nsite(istate),jsite-nsite(istate),
c     >      overl_dis(istate,ihagg,idis,iconf)
        endif
        endif
c       write(iu_tmp,*)'; '
c     >,overl_dis(istate,ihagg,idis,iconf)/ipair(istate,ihagg,idis,7) 
c     >,'; ',ipair(istate,ihagg,idis,7) 

        dum3=dum3+ipair(istate,ihagg,idis,7) 
        ov=ov+overl_dis(istate,ihagg,idis,iconf)
      enddo
      if (dum3.ne.zer) then
        dum3=one/dum3
c        write(iu_tmp,*) ' TEST OV', overl(istate,ihagg,iconf),ov*dum3,
c     >one/dum3
        i121=i121+1
        dum7=dum7+overl(istate,ihagg,iconf)
      endif
      enddo
c      write(iu_tmp,*) ' dum7/i121=',dum7/i121
      do iilink=1,nlink(istate)
      if (qverl_lnk(istate,iilink,iconf).ne.0.) then
c          write(iu_tmp,'(a)') 
c     >' istate ilink  iconf                Uel_i'
c          write(iu_tmp,'(1x,3(i4,3x),14x,g10.3)') 
c     >    istate,iilink,iconf,qverl_lnk(istate,iilink,iconf)
        endif
      enddo

      enddo
      enddo



      do nn=1,nexper
        write(iu_tmp,4001) qipar(nn)
        write(iu_tmp,4002) plate(nn)
      enddo


      string='                                        '
      write(iu_tmp,4100)  string
      string=' OPTIMIZATION PARAMETERS %%%%%%%%%%%%%%%'
      write(iu_tmp,4100) string
      string='                                        '
      write(iu_tmp,4100) string

      string=' Number of Cycles per Sub-run .........:'
      write(iu_tmp,2100) string,ncycl
      string=' Maximum Number of Sub-run ............:'
      write(iu_tmp,2100) string,nsubrun
      string=' Hypothetical Final f(X) ..............:'
      write(iu_tmp,2200) string,fhypo
      string=' Eventual constant T* value ...........:'
      write(iu_tmp,2200) string,tempfix
      string=' Iterations number for error analysis .:'
      write(iu_tmp,2100) string,niter
      string=' Maximum Rank of Harmonics (max 30) ...:'
      write(iu_tmp,2100) string,lfirst
      string=' Maximum rank l for F^S[l,m](Q) .......:'
      write(iu_tmp,2100) string,lsecond
      string=' Maximum rank l for S[l1,l2,l3,k1,k2] .:'
      write(iu_tmp,2100) string,ltird
      string=' Max. Q of Partial Amplitudes (Angs.) .:'
      write(iu_tmp,2200) string,qmfull
      string=' Numb. of Q of Partial Amplitudes .....:'
      write(iu_tmp,2100) string,nqfull
      string=' FCC Packing Distance (Angs.) .........:'
      write(iu_tmp,2200) string,dfcc
      string=' Number of Hydration Shells ...........:'
      write(iu_tmp,2100) string,nsfull
      string=' Weight of Hinge Superposition (w_A) ..:'
      write(iu_tmp,2200) string,w_kforce
      string=' Weight of Overlap Function (w_B) .....:'
      write(iu_tmp,2200) string,w_overl
      string=' Number of zeros for integration ......:'
      write(iu_tmp,2100) string,nz
      string=' Number of bins for integration .......:'
      write(iu_tmp,2100) string,nd
      string=' Number of points for 3D plots ........:'
      write(iu_tmp,2100) string,np3d
      string=' Reference Temperature (K) ............:'
      write(iu_tmp,2200) string,t0ref
      string=' Reference Pressure (bar) .............:'
      write(iu_tmp,2200) string,p0ref
      string=' Stone Exp. Contact Dist. Family Name .:'
      write(iu_tmp,4100) string,famstone

      string=' Stone Exp. Contact Dist. Flag (1/0) ..:'
      write(iu_tmp,2100) string,ifsig
      string=' A[l,m] Calculation Flag (1/0) ........:'
      write(iu_tmp,2100) string,ifilm
      string=' Dummy Atom Shell Determination .......:'
      write(iu_tmp,2100) string,ifspdb

      string='                                        '
      write(iu_tmp,4100)  string
      string=' STRUCTURE OF THE ASSEMBLIES %%%%%%%%%%%'
      write(iu_tmp,4100)  string
      string='                                        '
      write(iu_tmp,4100)  string

      string=' Number of Independent Assemblies .....:'
      write(iu_tmp,2100) string,nstate

      do istate=1,nstate
      string='----------------------------------------'
      write(iu_tmp,4100) string
      string=' Number of Assembly''s Configurations ..:'
      write(iu_tmp,2100) string,nconf(istate)
      string=' Number of Rigid Domains (RD) ..........:'
      write(iu_tmp,2100) string,nsite(istate)
      string=' Number of Flexible Linkers (FL) .......:'
      write(iu_tmp,2100) string,nlink(istate)
      string=' Number of Hierarchic Aggregates ......:'
      write(iu_tmp,2100) string,nhagg(istate)
      string=' Write the Aggregate Section in Order of'
      write(iu_tmp,4100) string

      do ihagg=1,nhagg(istate)

      string=' +++++++++++++++++++++++++++++++++++++++'
      write(iu_tmp,4100) string
      string=' Point Group Symmetry .................:'
      write(iu_tmp,2101) string,ggss(istate,ihagg)

      string=' Point Group Reference System ..: alpha:'
      write(iu_tmp,2200) string,agga(istate,ihagg)*conv
      string='                           (deg)  beta :'
      write(iu_tmp,2200) string,aggb(istate,ihagg)*conv
      string='                                  gamma:'
      write(iu_tmp,2200) string,aggg(istate,ihagg)*conv

      string=' Screw Polymerization Number ..........:'
      write(iu_tmp,2100) string,npoly(istate,ihagg)
      string='                                       ['
      write(iu_tmp,2200) string

      string=' Screw Axis Orientation: alpha (deg) ..:'
      do iconf=1,nconf(istate)
      write(iu_tmp,2207) string,conv*alpscrew(istate,ihagg,4+iconf),
     >     (conv*alpscrew(istate,ihagg,i),i=2,4)
     >     ,jalpscrew(istate,ihagg)
      string='                         cos(beta) ....:'
      write(iu_tmp,2207) string,cbescrew(istate,ihagg,4+iconf),
     >     (cbescrew(istate,ihagg,i),i=2,4)
     >     ,jcbescrew(istate,ihagg)
      string=' Rotation along Screw Axis (deg) ......:'
      write(iu_tmp,2207) string,conv*ascrew(istate,ihagg,4+iconf),
     >     (conv*ascrew(istate,ihagg,i),i=2,4)
     >     ,jascrew(istate,ihagg)
      string=' Translation along Screw Axis (Ang.) ..:'
      write(iu_tmp,2207) string,dscrew(istate,ihagg,4+iconf),
     >     (dscrew(istate,ihagg,i),i=2,4)
     >     ,jdscrew(istate,ihagg)

      string=' Contraction factor gamma .............:'
      write(iu_tmp,2207) string,gammacontr(istate,ihagg,4+iconf),
     >     (gammacontr(istate,ihagg,i),i=2,4)
     >     ,jgammacontr(istate,ihagg)

      string=' Effective Aver. Atomic Radius (Angs.) :'
      write(iu_tmp,2207) string,rmeff(istate,ihagg,4+iconf),
     >     (rmeff(istate,ihagg,i),i=2,4)
     >     ,jrmeff(istate,ihagg)

      do k=1,mxshell  
      string=' Relative mass density hydr. shell    .:'
      write(iu_tmp,2207) string,rhcl(istate,ihagg,k,4+iconf),
     >     (rhcl(istate,ihagg,k,i),i=2,4)
     >     ,jrhcl(istate,ihagg,k)
      enddo
      enddo! end iconf

      string='                                       ['
      write(iu_tmp,4100) string

      do k=1,mxtherf  
      if (k.eq.1) 
     >string=' Reference Gibbs Free Energy (cal/mol) :'
      if (k.eq.2) 
     >string=' Reference Entropy (cal/mol/K) ........:'
      if (k.eq.3) 
     >string=' Heat Capacity (cal/mol/K) ............:'
      if (k.eq.4) 
     >string=' Displaced Volume (cm^3/mol) ..........:'
      if (k.eq.5) 
     >string=' Thermal Expansivity Fact. (cm^3/mol/K):'
      if (k.eq.6) 
     >string=' Compressibility Fact. (cm^3/mol/bar) .:'
      write(iu_tmp,2207) string,ftherm(istate,ihagg,k,1,1),
     >     (ftherm(istate,ihagg,k,1,i),i=2,4)
     >     ,jftherm(istate,ihagg,k,1)
      do j=2,neffec+1 
      string=' First Derivative Effectors #  ........:'   
      write(iu_tmp,2207) string,ftherm(istate,ihagg,k,j,1),
     >     (ftherm(istate,ihagg,k,j,i),i=2,4)
     >     ,jftherm(istate,ihagg,k,j)
      enddo! end j
      enddo! end k      
      enddo! end ihagg

      jsite=1
      do isite=1,nsite(istate)

      string='*************************** Rigid Domain'
      write(iu_tmp,4100) string
      string='    RD PDB File Name (without .pdb) ...:'
      write(iu_tmp,4100) string,fsub(ipdbsite(istate,isite))

c      string='    Next Site r0 Distance (0. no bond) :'
c      write(iu_tmp,2200) string,dnextsite(istate,isite)
c      string='    ...................................:'
c      write(iu_tmp,2200) string,something(istate,isite)

      jsite=jsite+1
      if (dnextsite(istate,isite).ne.0..and.
     >isite.le.nsite(istate)-1) then
      rx=xrrot(istate,isite)-xlrot(istate,jsite)
      ry=yrrot(istate,isite)-ylrot(istate,jsite)
      rz=zrrot(istate,isite)-zlrot(istate,jsite)
      call cpolar(rx,ry,rz,r1,a1,b1)
      string='    Next RD Final Distance ............:'
      write(iu_tmp,2200) string,r1
      endif

      string='                                       ['
      write(iu_tmp,4100) string
      do iconf=1,nconf(istate)
      string='    Geom. Center Vect.: modulus (ang.) :'
      write(iu_tmp,2207) string,ums(istate,isite,4+iconf),
     >     (ums(istate,isite,i),i=2,4)
     >     ,jums(istate,isite)
      string='                        alpha   (deg) .:'
      write(iu_tmp,2207) string,conv*aus(istate,isite,4+iconf),
     >     (conv*aus(istate,isite,i),i=2,4)
     >     ,jaus(istate,isite)
      string='                        cos(beta) .....:'
      write(iu_tmp,2207) string,cbu(istate,isite,4+iconf),
     >     (cbu(istate,isite,i),i=2,4)
     >     ,jcbu(istate,isite)
      string='    Rotation Angles:    alpha_a (deg) .:'
      write(iu_tmp,2207) string,conv*aas(istate,isite,4+iconf),
     >     (conv*aas(istate,isite,i),i=2,4)
     >     ,jaas(istate,isite)
      string='                        cos(beta_a) ...:'
      write(iu_tmp,2207) string,cba(istate,isite,4+iconf),
     >     (cba(istate,isite,i),i=2,4)
     >     ,jcba(istate,isite)
      string='                        gamma_a (deg) .:'
      write(iu_tmp,2207) string,conv*gas(istate,isite,4+iconf),
     >     (conv*gas(istate,isite,i),i=2,4)
     >     ,jgas(istate,isite)

      enddo! end iconf
      enddo! end isite

c.....linkers
      do iilink=1,nlink(istate)
      n=naalink(istate,iilink)
      string='********************************* Linker'
      write(iu_tmp,4100) string
      string=' Primary structure from N- to C-term ..:'
      write(iu_tmp,4101) string,(amm1(iprimlink(istate,iilink,k)),k=1,n)
      string=' RD # attached to the linker''s N-term .:'
      write(iu_tmp,2100) string,isitelink(istate,iilink,1)
      string=' AA # of the PDB of the previous RD ...:'
      write(iu_tmp,2100) string,isitelink(istate,iilink,2)
      string=' RD # attached to the linker''s C-term .:'
      write(iu_tmp,2100) string,isitelink(istate,iilink,3)
      string=' AA # of the PDB of the previous RD ...:'
      write(iu_tmp,2100) string,isitelink(istate,iilink,4)
      string=' Flag for phi, psi and chi variations .:'
      write(iu_tmp,2100) string,jlink(istate,iilink)

      string='                                       ['
      write(iu_tmp,4101) string,'-------deg-----|-------deg-----'//
     >'|-------deg-----|-------deg-----|continues...'

      do iconf=1,nconf(istate)
      string=' Starting values of phi angles ........:'
      write(iu_tmp,2209) string,
     >      (conv*phi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n),
     >     ((conv*phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
     >     ,(conv*phi_lnk(istate,iilink,4+iconf,iaa),iaa=n+1,n+1),
     >     ((conv*phi_lnk(istate,iilink,i,iaa),i=2,4),iaa=n+1,n+1)
      string=' Starting values of psi angles ........:'
      write(iu_tmp,2209) string,
     >      (conv*psi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n),
     >     ((conv*psi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)
      string=' Starting values of chi angles ........:'
      write(iu_tmp,2209) string,
     >      (conv*chi_lnk(istate,iilink,4+iconf,iaa),iaa=1,n),
     >     ((conv*chi_lnk(istate,iilink,i,iaa),i=2,4),iaa=1,n)

      enddo! end iconf
      enddo! end iilink
c.....



c.....constraints_a
      m=nconstr_a(istate)
      if (m.ne.0) then
        string='***************** Amino acid Constraints'
        write(iu_tmp,4100) string
      endif
      write(iu_tmp,'(a)') ' AA |RD|SS|POL| Atom|Coord.vector r_i |'//
     >'     Condition     |Val,Name,Vec.u_d |F'//
     >'act.Val.|Method|   K    |Name  |Flag'
      write(iu_tmp,'(a)') '    |FL|  |   |     |x,y,z,r,a,b,rc,ac|'//
     >'=,>,>=,<,<=,max,min|x,y,z,r,a,b,rc,ac| '//
     >'        |st,el |        |      |1/0'
      do nc=1,m
      do i=1,200
        text(i:i)=' '
      enddo
      write(text(1:4),"(i4)") iconstr_a(istate,nc,1)
      write(text(111:115),"(i5)") 1
      write(text(6:7),"(i2)") iconstr_a(istate,nc,2)
      write(text(9:10),"(i2)") iconstr_a(istate,nc,3)
      write(text(12:14),"(i3)") iconstr_a(istate,nc,4)
      write(text(16:20),"(a5)") atypei(iconstr_a(istate,nc,5))//'  '
      write(text(22:38),"(a17)") 
     >'       '//coordtype(iconstr_a(istate,nc,6))//'        '
      write(text(40:58),"(a19)") 
     >'        '//condtype(iconstr_a(istate,nc,7))//'        '
      j1=jconstr_a(istate,nc,1) !1=u_d, 2 = same of nc2=jconstr_a(istate,nc,2), 3= value
      j2=jconstr_a(istate,nc,2)
      if (j1.eq.1) then      
        write(text(60:76),"(a17)") 
     >'      '//coordtype(iconstr_a(istate,nc,8))//'       '
      else if (j1.eq.2) then
        write(text(60:76),"(a17)") 
     >'    '//lconstr_a(istate,j2)//'     '
      else
        write(text(60:76),"(g17.5)") vconstr_a(istate,nc)
      endif
      write(text(78:86),"(g9.2)") fconstr_a(istate,nc)
      write(text(95:102),"(g8.2)") cconstr_a(istate,nc)
      write(text(104:109),"(a6)") lconstr_a(istate,nc)
      write(text(88:93),"(a6)") 
     >methtype(iconstr_a(istate,nc,9))//'    '
      write(iu_tmp,'(a)') text
      do iconf=1,nconf(istate)
      do i=1,200
        text(i:i)=' '
      enddo
      write(text(22:38),"(g17.5)") constr_a(istate,iconf,nc,1)
      write(text(60:76),"(g17.5)") constr_a(istate,iconf,nc,2)
      write(text(95:102),"(g8.2)") constr_a(istate,iconf,nc,3)
      write(iu_tmp,'(a200)') text
      enddo! end iconf
      enddo! end constr_a

c.....constraints_b
      m=nconstr_b(istate)
      if (m.ne.0) then
        string='******** Pairs of Amino acid Constraints'
        write(iu_tmp,4100) string
      endif
      write(iu_tmp,'(a)') 'i:AA|RD|SS|POL|i:Atom|j:AA|RD|SS|POL|j:'//
     >'Atom|Coord.vector r_ij|     Condition  '//
     >'   |Val,Name,Vec.u_dd|Fact.Val.|Method|'//
     >'   K    |Name  |Flag'
      write(iu_tmp,'(a)') '    |FL|  |   |      |    |FL|  |   |  '//
     >'    |x,y,z,r,a,b,rc,ac|=,>,>=,<,<=,max,'//
     >'min|x,y,z,r,a,b,rc,ac|         |st,el |'//
     >'        |      |1/0'
      do nc=1,m
      do i=1,200
        text(i:i)=' '
      enddo
      write(text(1:4),"(i4)") iconstr_b(istate,nc,1)
      write(text(134:138),"(i5)") 1
      write(text(6:7),"(i2)") iconstr_b(istate,nc,2)
      write(text(9:10),"(i2)") iconstr_b(istate,nc,3)
      write(text(12:14),"(i3)") iconstr_b(istate,nc,4)
      write(text(16:21),"(a6)") atypei(iconstr_b(istate,nc,5))//'   '
      write(text(23:26),"(i4)") iconstr_b(istate,nc,6)
      write(text(28:29),"(i2)") iconstr_b(istate,nc,7)
      write(text(31:33),"(i2)") iconstr_b(istate,nc,8)
      write(text(34:36),"(i3)") iconstr_b(istate,nc,9)
      write(text(38:43),"(a6)") atypei(iconstr_b(istate,nc,10))//'   '
      write(text(45:61),"(a17)") 
     >'       '//coordtype(iconstr_b(istate,nc,11))//'        '
      write(text(63:81),"(a19)") 
     >'        '//condtype(iconstr_b(istate,nc,12))//'        '
      j1=jconstr_b(istate,nc,1) !1=u_d, 2 = same of nc2=jconstr_b(istate,nc,2), 3= value
      j2=jconstr_b(istate,nc,2)
      if (j1.eq.1) then      
        write(text(83:99),"(a17)") 
     >'      '//coordtype(iconstr_b(istate,nc,13))//'       '
      else if (j1.eq.2) then
        write(text(83:99),"(a17)") 
     >'    '//lconstr_b(istate,j2)//'     '
      else
        write(text(83:99),"(g17.5)") vconstr_b(istate,nc)
      endif
      write(text(101:109),"(g9.2)") fconstr_b(istate,nc)
      write(text(118:125),"(g8.2)") cconstr_b(istate,nc)
      write(text(127:132),"(a6)") lconstr_b(istate,nc)
      write(text(111:116),"(a6)") 
     >methtype(iconstr_b(istate,nc,14))//'    '
      write(iu_tmp,'(a)') text
      do iconf=1,nconf(istate)
      do i=1,200
        text(i:i)=' '
      enddo
      write(text(45:61),"(g17.5)") constr_b(istate,iconf,nc,1)
      write(text(83:99),"(g17.5)") constr_b(istate,iconf,nc,2)
      write(text(118:125),"(g8.2)") constr_b(istate,iconf,nc,3)
      write(iu_tmp,'(a)') text
      enddo! end iconf
      enddo! end constr_b

      enddo! end istate

      close(unit=iu_tmp)
      return

 4101 format(a40,300a1)
4100  format(a40,a300)
2100  format(a40,i10)
2101  format(a40,a4)
2200  format(a40,g15.5E3)
 2209 format(a40,50000(g15.5E3,1x))
2207  format(a40,4(g15.5E3,1x),i4,1x,a10000)
 4500 format(a1,100(a8,i1,a2))
4012  format(' Weight of state .............,,.......:',g15.5E3)
4001  format(' Scale Factor kappa ...................:',g15.5E3)
4002  format(' Flat Background B ....................:',g15.5E3)
4003  format(' Scale Factor of Deoxy data ...........:',g15.5E3)
      end


      subroutine outall(istate,ihagg,iconf)

c.....scrive nel formato del dat. la configurazione di tutti di domini, monomero per monomero
      include 'quafit2014.cmn'

      write(text(1:12),'(3i3)') istate,ihagg,iconf
      do k=0,6,3
        do l=1,2
          if (text(l+k:l+k).eq.' ') text(l+k:l+k)='0'
        enddo
      enddo

      jconf=iconf+4

      open (unit=iu_tmp,status='unknown',file='qua'//code//subcode//
     >'_'//text(1:9)//'_all.out')


      do ipoly=1,npoly(istate,ihagg)
      do isub=1,nsymm(istate,ihagg)
      do isite=1,nsite(istate)
      string='****************************************'
      write(iu_tmp,4101) string,ipoly,isub,isite
      string='    RD PDB File Name (without .pdb) ...:'
      write(iu_tmp,4100) string,fsub(ipdbsite(istate,isite))

c      string='    Next Site Distance (0. no bond) ...:'
c      write(iu_tmp,2200) string,dnextsite(istate,isite)
c      string='    ...................................:'
c      write(iu_tmp,2200) string,something(istate,isite)

      string='                                       ['
      write(iu_tmp,4100) string
      string='    Geom. Center Vect.: modulus (ang.) :'

        call cpolar(centre(isite,isub,ipoly,1),
     >              centre(isite,isub,ipoly,2),
     >              centre(isite,isub,ipoly,3),
     >              dum1,dum2,dum3)
      dum3=dcos(dum3)

      write(iu_tmp,2207) string,dum1, 0.,500.,
     >     (ums(istate,isite,i),i=4,4)
     >     ,jums(istate,isite)
      string='                        alpha   (deg) .:'
      write(iu_tmp,2207) string,conv*dum2,0.,360.,
     >     (conv*aus(istate,isite,i),i=4,4)
     >     ,jaus(istate,isite)
      string='                        cos(beta) .....:'
      write(iu_tmp,2207) string,dum3,-1.,1.,
     >     (cbu(istate,isite,i),i=4,4)
     >     ,jcbu(istate,isite)
      string='    Rotation Angles:    alpha_a (deg) .:'
      write(iu_tmp,2207) string,conv*alprot(isite,isub,ipoly),
     >     0.,360.,(conv*aas(istate,isite,i),i=4,4)
     >     ,jaas(istate,isite)
      string='                        cos(beta_a) ...:'
      write(iu_tmp,2207) string,cos(betrot(isite,isub,ipoly)),
     >     -1.,1.,(cba(istate,isite,i),i=4,4)
     >     ,jcba(istate,isite)
      string='                        gamma_a (deg) .:'
      write(iu_tmp,2207) string,conv*gamrot(isite,isub,ipoly),
     >     0.,360.,(conv*gas(istate,isite,i),i=4,4)
     >     ,jgas(istate,isite)


      enddo
      enddo
      enddo

      close(unit=iu_tmp)
      return

4100  format(a40,a300)
 4101 format(a40,3i5)
2100  format(a40,i10)
2101  format(a40,a4)
2200  format(a40,g15.5E3)
2207  format(a40,4(g15.5E3,1x),i4,1x,a10000)
 4500 format(a1,100(a8,i1,a2))
4012  format('  Weight of state .............,,.......:',g15.5E3)
4001  format('  Scale Factor kappa ...................:',g15.5E3)
4002  format('  Flat Background B ....................:',g15.5E3)
4003  format('  Scale Factor of Deoxy data ...........:',g15.5E3)
      end



      subroutine xfracparz

      include 'quafit2014.cmn'

c      write(*,*) ' xfracparz ---> '
      open(unit=iu_tmp,status='unknown',
     >       file='qua'//code//subcode//'.xfrac')
      do iagg=1,nagg!!!-1
        write(iu_tmp,1212)
     >  iagg,
     >  ((xfrac(iagg,nn,k),k=1,4,3),nn=1,nexper)
      enddo 
      close(unit=iu_tmp)

      open(unit=iu_tmp,status='unknown',
     >       file='qua'//code//subcode//'.fract')
      do iagg=1,nagg!!!-1
        write(iu_tmp,1313)
     >  iagg,jstagg(iagg),jhaagg(iagg),
     >  (fract(jstagg(iagg),jhaagg(iagg),nn),nn=1,nexper)
      enddo 
      close(unit=iu_tmp)

      return
 1212 format(1(i4),8000(e15.6E3))
 1313 format(3(i4),8000(e15.6E3))
      end


      function sfactt(ip)

      include 'quafit2014.cmn'

      s=-q*q
      sfactt=ctfmt(ip)
      do i=1,5
        sfactt=sfactt+atfmt(ip,i)*dexp(s*btfmt(ip,i))
      enddo
      return
      end

      subroutine sfanom(ig,wl,fan)

      include 'quafit2014.cmn'
      complex*16 fan
      real*4 fp,fpp,energy

      dum1=0.0
      dum2=0.0
      energy=12398./wl ! eV
      do ia=1,natgr(ig)
        call abscromer(izgr(ig,ia),energy,fp,fpp)
c      if (izgr(ig,ia).eq.16) write(*,*) izgr(ig,ia),wl,fp,fpp
        dum1=dum1+fp
        dum2=dum2+fpp
      enddo
      fan=dcmplx(dum1,dum2)
      return
      end

c      include 'abs.f'
c      include 'atomdata.f'
c      include 'cromer.f'
c      include 'crys_abs.f'
c      include 'element.f'
c      include 'gstring.f'
c      include 'matl_abs.f'
c      include 'raycomp.f'
c      include 'rdlat.f'
c      include 'sfcoef.f'
c      include 'str_fact.f'
c      include 'xnrg_b_d.f'
c      include 'xsec_b_d.f'

      function sfacts(ip)

      include 'quafit2014.cmn'

      sfacts=vexc(ip)
c      write(*,*) ip,vexc(ip)
      sfacts=sfacts*dexp(-q*q*sfacts**(2.d0/3)/pi4)
      return
      end


      subroutine wprc(i,i1,i2,lstep,lstart)
      if (int(0.5+(i-i1+1)*100./(i2-i1+1)).eq.lstart*lstep) then
        write(*,*) lstart*lstep,'%'
        lstart=lstart+lstep
      endif
      return
      end


      subroutine allopen

      include 'quafit2014.cmn'

      do istate=1,nstate
      do ihagg=1,nhagg(istate)
      do iconf=1,nconf(istate)
      do isitlnk=1,nsite(istate)+nlink(istate)  !! oltre ai siti ci vogliono anche i linker
c      write(*,*) nstate, nhagg(istate),nconf(istate),
c     >nsite(istate),nlink(istate)
c      pause

      write(text(1:12),'(4i3)') istate,isitlnk,ihagg,iconf
      do k=0,9,3
        do l=1,2
          if (text(l+k:l+k).eq.' ') text(l+k:l+k)='0'
        enddo
      enddo
c      do it=1,4 !ci sono quindi quatto tipi di file, due "c" e due "f", due "cst" due "csw"
      do it=3,4 
        if (it.eq.1) then
        call jseque(iu_con,istate,isitlnk,ihagg,iconf,it)
        i1=15*(2*(lsecond+1)*(lsecond+1)*(nsfull+nwave+8)+1)+2!1
      open (unit=iu_con,access='direct',recl=i1,status='unknown',
     >form='formatted',file='c'//code//text(1:12)//'.cst')  !! sono i vecchi \Phi(l1,l2,l3,...) non servono piu'

        else if (it.eq.2) then
        call jseque(iu_cpc,istate,isitlnk,ihagg,iconf,it)
        i1=15*(2*(lsecond+1)*(lsecond+1)*(nsfull+nwave+8)+1)+2!1
      open (unit=iu_cpc,access='direct',recl=i1,status='unknown',
     >form='formatted',file='c'//code//text(1:12)//'.csw')  !! una copia dei vecchi \Phi(l1,l2,l3,...) non servono piu'

        else if (it.eq.3) then
        call jseque(iu_ful,istate,isitlnk,ihagg,iconf,it)
        i1=15*(2*(lsecond+1)*(lsecond+1)*(nsfull+nwave+8)+1)+2!1
      open (unit=iu_ful,access='direct',recl=i1,status='unknown',
     >form='formatted',file='f'//code//text(1:12)//'.cst')  !!! sono i vecchi Alm simmetrizzati
  
        else if (it.eq.4) then
        call jseque(iu_cpf,istate,isitlnk,ihagg,iconf,it)
        i1=15*(2*(lsecond+1)*(lsecond+1)*(nsfull+nwave+8)+1)+2!1   
      open (unit=iu_cpf,access='direct',recl=i1,status='unknown',
     >form='formatted',file='f'//code//text(1:12)//'.csw')  !!! una copia dei vecchi Alm simmetrizzati
  
      endif
      enddo
      enddo
      enddo
      enddo
      enddo
      return
      end

      subroutine allclose

      include 'quafit2014.cmn'

      do istate=1,nstate
      do ihagg=1,nhagg(istate)
      do iconf=1,nconf(istate)
      do isitlnk=1,nsite(istate)+nlink(istate)


c      do it=1,4
      do it=3,4

        if (it.eq.1) then
        call jseque(iu_con,istate,isitlnk,ihagg,iconf,it)
        close(unit=iu_con)

        else if (it.eq.2) then
        call jseque(iu_cpc,istate,isitlnk,ihagg,iconf,it)
        close(unit=iu_cpc)

        else if (it.eq.3) then
        call jseque(iu_ful,istate,isitlnk,ihagg,iconf,it)
        close(unit=iu_ful)
  
        else if (it.eq.4) then
        call jseque(iu_cpf,istate,isitlnk,ihagg,iconf,it)
        close(unit=iu_cpf)
  
      endif
      enddo
      enddo
      enddo
      enddo
      enddo
      return
      end


      subroutine jseque(jsq,istate,isitlnk,ihagg,iconf,it)

      include 'quafit2014.cmn'

c      write(*,*) '########'
c      write(*,*) 10+it+(isite-1)*4
c      write(*,*) (iconf-1)*4*(nsite(istate)+nlink(istate))
c      write(*,*) (ihagg-1)*4*(nsite(istate)+nlink(istate))*nconf(istate)
c      write(*,*) (istate-1)*4*(nsite(istate)+nlink(istate))*nconf(istate)*nhagg(istate)
c     >*(nsite(istate)+nlink(istate))

      jsq=40+it+(isitlnk-1)*4
     >+(iconf-1)*4*(nsite(istate)+nlink(istate))
     >+(ihagg-1)*4*(nsite(istate)+nlink(istate))*nconf(istate)
      do j=1,istate-1
        jsq=jsq
     >+4*(nsite(j)+nlink(j))*nconf(j)*nhagg(j)
c      write(*,*) 'A',+4*(nsite(j)+nlink(j))*nconf(j)*nhagg(j)
      enddo
c      write(*,*) jsq
      return
      end

      subroutine gettim(ih,im,is,ic)

      character date*8,time*9

      CALL Date_and_Time(Date, Time)
      read(time,'(i2)') ih
      read(time,'(2x,i2)') im
      read(time,'(4x,i2)') is
      read(time,'(7x,i2)') ic
      return
      end



      subroutine pq_parz_2(istate,ihagg,iconf)

c.....Calcola il fattore di forma P(Q) relativo al Q iq-esimo qqfull dell'unica griglia tutti i tipi di exp.
c.....per la terna stato/aggregato/configurazione istate, ihagg, iconf

      include 'quafit2014.cmn'
      complex *16 cum1,cum2

      if (nexper.eq.0) return
c      write(*,*) '   ---> pq_parz_2   top',istate,ihagg,iconf

c      do istate=1,nstate
c        do ihagg=1,nhagg(istate)
c          do iconf=1,nconf(istate)
            jconf=iconf+4
            dum1=zer
            do iq=1,nqfull
              q=qqfull(iq)
              do kt=1,ntt
                it=ktt(kt)      
                ms=npnt(it)

      do l=0,lsecond
        do m=-l,l
          do js=1,ms
            asylm(l,m,js)=dcmplx(zer,zer)
          enddo
        enddo
      enddo

      do isitlnk=1,nsite(istate)+nlink(istate)

        call jseque(iu_ful,istate,isitlnk,ihagg,iconf,3)

c      ist=isitlnk+(nsite(istate)+nlink(istate))*(iconf-1+nconf(istate)*
c     >    (ihagg-1+nhagg(istate)*(istate-1)))

        read (unit=iu_ful,rec=iq,fmt=1027) dum7,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)

c        write(79,1027) dum7,
c     >(((almstore(1,iq,l,m,is),m=-l,l),
c     > l=0,lsecond),is=1,nwave+nsfull+8)
c        do kkk=1,7
c        write(79+kkk,1027) dum7,
c     >(((almstore(mxpdb+kkk,iq,l,m,is),m=-l,l),
c     > l=0,lsecond),is=1,nwave+nsfull+8)
c        enddo

 1027 format(500000(e15.6E3))

        call almsym  ! valore interpolato

        if (isitlnk.le.nsite(istate)) then
          ipdb=ipdbsite(istate,isitlnk)
          rm=rmfull(ipdb)
        else
          rm=rmfull(isitlnk)
        endif
        r0=rmeff(istate,ihagg,jconf)
        dum1=(pi4 * 9)**(-1.d0/3)*(r0**2-rm**2) !!! check!
        dum2=(r0/rm)**3 
c.......Calcolo G(Q)
        gq=dum2*dexp(-q*q*dum1)
c.......Calcolo h(Q)
        fat=vol**(2.d0/3)/pi4
        hq=vol*dexp(-q*q*fat)

        if (it.eq.1) then
c.........1) X-rays
c.........Standard
          wtmp(1)=0.28!units of 10**-12 cm  1.d0
          wtmp(2)=-gq
          do is=1,nsfull
            wtmp(2+is)=hq*(rhcl(istate,ihagg,is,jconf)-1.0)! / gq
          enddo
        else if (it.ne.2+nwave) then
c.........Anomalous 
          wtmp(1)=0.28!units of 10**-12 cm 1.d0
          wtmp(2)=0.28!units of 10**-12 cm 1.d0
          wtmp(3)=-gq
          do is=1,nsfull
            wtmp(3+is)=hq*(rhcl(istate,ihagg,is,jconf)-1.0)! / gq
          enddo
        else if (it.eq.2+nwave) then
c.........Neutrons
          wtmp(1)=1.d0   ! green notebook with label "3/2004"
          wtmp(2)=1.d0
          wtmp(3)=1.d0
          wtmp(4)=1.d0
          wtmp(5)=1.d0
          wtmp(6)=-gq
          do is=1,nsfull
            wtmp(6+is)=hq*(rhcl(istate,ihagg,is,jconf)-1.0)! / gq
          enddo
        endif

        do l=0,lsecond
          do m=-l,l
            do js=1,ms
              is=ipnt(it,js)
              asylm(l,m,js)=asylm(l,m,js)+alm(l,m,is)*wtmp(js)  ! sottratto solvente e con contributo shells
c              write(100,*) q,l,m,asylm(l,m,js),alm(l,m,is)
            enddo
          enddo
        enddo

      enddo ! end isitlnk

      i1=0
      do js1=1,ms
      do js2=js1,ms
      i2=1
      i1=i1+1
      if (js1.ne.js2) i2=2

      cum1=dcmplx(0.d0,0.d0)
      do l=0,lsecond
        do m=-l,l
          cum1=cum1+asylm(l,m,js1)*dconjg(asylm(l,m,js2))
        enddo
      enddo

      pqtt(iq,kt,i1,istate,ihagg,iconf)=dreal(cum1)*pi4*i2
      enddo
      enddo
        
      enddo

c      write(71,*) q,
c     >(pqtt(iq,1,i1,istate,ihagg,iconf),i1=1,(npnt(1)*(npnt(1)+1))/2)

      enddo
c      enddo
c      enddo
c      enddo

c      write(*,*) '   ---> pq_parz_2   bottom'
c      stop
      return
      end


      subroutine cp_parz_2(istate,ihagg,iconf)

      include 'quafit2014.cmn'

      do iq=1,nqfull
      do kt=1,ntt
      it=ktt(kt)      
      ms=npnt(it)
      i1=0
      do js1=1,ms
      do js2=js1,ms
      i1=i1+1
      pquu(iq,kt,i1,istate,ihagg,iconf)=
     >     pqtt(iq,kt,i1,istate,ihagg,iconf)
      enddo
      enddo
      enddo
      enddo
      return
      end

      
      subroutine re_parz_2(istate,ihagg,iconf)

      include 'quafit2014.cmn'

      do iq=1,nqfull
      do kt=1,ntt
      it=ktt(kt)      
      ms=npnt(it)
      i1=0
      do js1=1,ms
      do js2=js1,ms
      i1=i1+1
      pqtt(iq,kt,i1,istate,ihagg,iconf)=
     >     pquu(iq,kt,i1,istate,ihagg,iconf)
      enddo
      enddo
      enddo
      enddo
      return
      end

      


      subroutine pr_parz_2


      include 'quafit2014.cmn'
      complex *16 cum1,cum2

c      write(*,*) '   ---> pr_parz_2   top'

      if (nexper.eq.0) return
      dum5=0.d0
      do nn=1,nexper
        rmax = max(dum5,dprmax(nn))
      enddo
      nr= mn-1
      dr= rmax/(nr-1)
      dq=qqfull(2)-qqfull(1)


      do istate=1,nstate
      do ihagg=1,nhagg(istate)
      do iconf=1,nconf(istate)

      do it=1,2+nwave
      ms=npnt(it)

      do iq=1,nqfull
      q=qqfull(iq)
      do l=0,lsecond
        do m=-l,l
          do js=1,ms
            asylm(l,m,js)=dcmplx(zer,zer)
          enddo
        enddo
      enddo


      do isitlnk=1,nsite(istate)+nlink(istate)

        call jseque(iu_ful,istate,isitlnk,ihagg,iconf,3)

      ist=isitlnk+(nsite(istate)+nlink(istate))*(iconf-1+nconf(istate)*
     >    (ihagg-1+nhagg(istate)*(istate-1)))
        read (unit=iu_ful,rec=iq,fmt=1027) dum7,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
 1027 format(500000(e15.6E3))

        call almsym  ! valore interpolato
        q=qqfull(iq)
            jconf=iconf+4

        if (isitlnk.le.nsite(istate)) then
          ipdb=ipdbsite(istate,isitlnk)
          rm=rmfull(ipdb)
        else
          rm=rmfull(isitlnk)
        endif
        r0=rmeff(istate,ihagg,jconf)
        dum1=(pi4 * 9)**(-1.d0/3)*(r0**2-rm**2) !!! check!
        dum2=(r0/rm)**3 
c.......Calcolo G(Q)
        gq=dum2*dexp(-q*q*dum1)
c.......Calcolo h(Q)
        fat=vol**(2.d0/3)/pi4
        hq=vol*dexp(-q*q*fat)

        if (it.eq.1) then
c.........1) X-rays
c.........Standard
          wtmp(1)=0.28!units of 10**-12 cm  1.d0
          wtmp(2)=-gq
          do is=1,nsfull
            wtmp(2+is)=hq*(rhcl(istate,ihagg,is,jconf)-1.0)! / gq
          enddo
        else if (it.ne.2+nwave) then
c.........Anomalous 
          wtmp(1)=0.28!units of 10**-12 cm 1.d0
          wtmp(2)=0.28!units of 10**-12 cm 1.d0
          wtmp(3)=-gq
          do is=1,nsfull
            wtmp(3+is)=hq*(rhcl(istate,ihagg,is,jconf)-1.0)! / gq
          enddo
        else if (it.eq.2+nwave) then
c.........Neutrons
          wtmp(1)=1.d0   ! green notebook with label "3/2004"
          wtmp(2)=1.d0
          wtmp(3)=1.d0
          wtmp(4)=1.d0
          wtmp(5)=1.d0
          wtmp(6)=-gq
          do is=1,nsfull
            wtmp(6+is)=hq*(rhcl(istate,ihagg,is,jconf)-1.0)! / gq
          enddo
        endif

        do l=0,lsecond
          do m=-l,l
            do js=1,ms
              is=ipnt(it,js)
              asylm(l,m,js)=asylm(l,m,js)+alm(l,m,is)*wtmp(js)  ! sottratto solvente e con contributo shells
c      if (iq.eq.150.and.l.eq.2.and.m.eq.-1.and.js.eq.1) 
c     >write(102,*) ' DD ',alm(2,-1,1)
            enddo
          enddo
        enddo
      enddo ! end isitlnk

      i1=0
      do js1=1,ms
      p1qfull(it,js1,iq)=dreal(asylm(0,0,js1)*spi4)
      do js2=js1,ms
      i2=1
      i1=i1+1
      if (js1.ne.js2) i2=2
      cum1=dcmplx(0.d0,0.d0)
      do l=0,lsecond
        do m=-l,l
          cum1=cum1+asylm(l,m,js1)*dconjg(asylm(l,m,js2))
        enddo
      enddo
      pqfull(it,i1,iq)=dreal(cum1)*pi4*i2
      enddo!end js2
      enddo!end js1
      enddo!end iq

      i1=0
      do js1=1,ms
      do js2=js1,ms
      i1=i1+1

      r=-dr
      do ir=1,nr
        r=r+dr
        dum2=0.d0
        do iq=1,nqfull
          q=qqfull(iq)
          qit=pqfull(it,i1,iq)
          dum1=q*r
          if (dum1.eq.0.d0) then
            dum1=1.d0
          else
            dum1=dsin(dum1)/dum1
          endif
          dum2=dum2+q*q*qit*dum1
        enddo
        dum2=2.d0*dum2*dq/pi*r*r
c          write(*,*) ' 1 ci sono', it,i1,ir
        prfull(it,i1,ir)=dum2
      enddo
      enddo!end js2

      r=-dr
      do ir=1,nr
        r=r+dr
        dum2=0.d0
        do iq=1,nqfull
          q=qqfull(iq)
          qit=p1qfull(it,js1,iq)
          dum1=q*r
          if (dum1.eq.0.d0) then
            dum1=1.d0
          else
            dum1=dsin(dum1)/dum1
          endif
          dum2=dum2+q*q*qit*dum1
        enddo
        dum2=2.d0*dum2*dq/pi*r*r
c          write(*,*) ' 2 ci sono', it,js1,ir
        p1rfull(it,js1,ir)=dum2
      enddo

      enddo!end js1

      enddo!end it

c          write(*,*) ' 3 ci sono'

      write(text(1:12),'(3i3)') istate,ihagg,iconf
      do k=0,6,3
        do l=1,2
          if (text(l+k:l+k).eq.' ') text(l+k:l+k)='0'
        enddo
      enddo
      open (unit=iu_bto,status='unknown',file='qua'//code//subcode//
     >'_'//text(1:9)//'.pr')

      write(iu_bto,1007) nr
      write(iu_bto,1007) (npnt(i),i=1,2+nwave)
c        ipdb=ipdbsite(istate,1)
c        rm=rmfull(ipdb)
      write(iu_bto,1107) 0.d0,dr
     >,wagg(istate,ihagg)
      r=-dr
      do ir=1,nr 
c          write(*,*) ' 4 ci sono'
        r=r+dr
        write(iu_bto,1207) r,
     >((prfull(i,i1,ir),
     >i1=1,((npnt(i)+1)*npnt(i))/2),
     >(p1rfull(i,is1,ir),
     >is1=1,npnt(i)),i=1,2+nwave)
      enddo
      close(unit=iu_bto)



      write(text(1:12),'(3i3)') istate,ihagg,iconf
      do k=0,6,3
        do l=1,2
          if (text(l+k:l+k).eq.' ') text(l+k:l+k)='0'
        enddo
      enddo
      open (unit=iu_bto,status='unknown',file='qua'//code//subcode//
     >'_'//text(1:9)//'.pq')

      do iq=1,nqfull
        write(iu_bto,1207) qqfull(iq),
     >((pqfull(i,i1,iq),
     >i1=1,((npnt(i)+1)*npnt(i))/2),
     >(p1qfull(i,is1,iq),
     >is1=1,npnt(i)),i=1,2+nwave)
      enddo
      close(unit=iu_bto)




      enddo!end iconf
      enddo!end ihagg
      enddo!end istate

c      write(*,*) '   ---> pr_parz_2   bottom'
      return
 1007 format('#',200(i4,1x))
 1107 format('#',200(1pe19.7E3))
 1207 format(200(1pe19.7E3))
      end

      


      



      



      subroutine alltt

      include 'quafit2014.cmn'

      do it=1,2+nwave
        jtt(it)=0
      enddo
      ntt=0
      do nn=1,nexper
        if (xd2o(nn).eq.-1.0) then
c.........1) X-rays
          if (iwave(nn).eq.0) then
c...........Standard
            it=1
            jett(nn)=it
            if (jtt(it).eq.0) then
              ntt=ntt+1
              ktt(ntt)=it
              kii(it)=ntt
              jtt(it)=1
            endif
          else
c...........Anomalous 
            it=1+iwave(nn)
            jett(nn)=it
            if (jtt(it).eq.0) then
              ntt=ntt+1
              ktt(ntt)=it
              kii(it)=ntt
              jtt(it)=1
            endif
          endif
        else
c.........Neutrons
          it=2+nwave
          jett(nn)=it
          if (jtt(it).eq.0) then
            ntt=ntt+1
            ktt(ntt)=it
            kii(it)=ntt
            jtt(it)=1
          endif
        endif
      enddo
      return
      end


      subroutine sian(funct,n,x,xl,xh,xd,rapp,f,ti,tf,ncicl,
     >nsub,tm,am,is)

c x 	vettore dei parametri iniziale e finale
c xn 	vettore dei parametri nuovo
c xl 	lower bounds
c xh 	Upper bounds
c xd 	vettore del passo iniziale
c xt 	totale mosse per variabile per subrun
c xa 	vettore mosse accettate per subrun
c xm 	vettore conteggi medio per subrun
c xq 	vettore conteggi quadratico medio per subrun

c rapp	rapporto di accettazione
c f 	f finale
c ti 	t iniziale
c tf 	t finale
c ncicl	numero di cicli
c nsub	numero di subrun
c tm	numero totale mosse tentate
c am	numero totale mosse accettate


      implicit real*8 (a-h,o-z)
      parameter (np=600)
      external funct
      common /sian1/pushup,pushdn,xt(np),xa(np),xm(np),xq(np),xn(np),
     >temp
      dimension x(np),xl(np),xh(np),xd(np)

c.....Inizializzazioni conteggi
      if (n.gt.np) stop 'n>np'
      tm=0.d0
      am=0.d0
c      write(*,*) (i,xl(i),xh(i),i=1,n)
c      pause
      
c.....calcolo fattore per simulated annealing
      call funct(n,x,f)
      vi=1.20d0
      vf=1.05d0
      temp=tf !Expected final T*
      dum2=ti !Starting T*
      dum1=(temp/dum2)**(1.d0/(nsub-1))
      temp=dum2/dum1
      dum3=(vf/vi)**(1.d0/(nsub-1))
      dum4=vi/dum3
      do i=1,nsub
        temp=temp*dum1
        dum4=dum4*dum3
        pushdn=2.d0-dum4
        pushup=dum4
        call subrun(funct,n,x,xl,xh,xd,rapp,f,ncicl,nsub,tm,am,is)
c        write(*,'(30(6(1pg12.3),/))') 
c     >   (float(k),x(k),xl(k),xh(k),xd(k),xa(k)/xt(k),k=1,n)

c        write(*,5200) ' Temp.      = ',temp
c        write(*,5200) ' Nacc/Ntot  = ',am/tm
c        write(*,5200) ' Chi square = ',f
c        write(*,*) '########################'

      enddo

      return
 5200 format(a14,1pg15.7)
      end

      subroutine subrun(funct,n,x,xl,xh,xd,rapp,f,ncicl,nsub,tm,am,is)

      implicit real*8 (a-h,o-z)
      parameter (np=600)
      common /sian1/pushup,pushdn,xt(np),xa(np),xm(np),xq(np),xn(np),
     >temp
      external funct
      dimension x(np),xl(np),xh(np),xd(np)

      parameter(cutrap=1.d-8,delrap=0.0d0)

      do 51 ivar=1,n
        xt(ivar)=0.d0
        xa(ivar)=0.d0
        xm(ivar)=0.d0
        xq(ivar)=0.d0
        xn(ivar)=x(ivar)
51    continue
      do 20 i=1,ncicl
        call ciclo(funct,n,x,xl,xh,xd,rapp,f,tf,ncicl,nsub,tm,am,is)
 20   continue
c       write(*,*) x(1),x(2),f

      do 30 ivar=1,n
        rap=xa(ivar)/xt(ivar)

        if ((rap.lt.rapp-delrap).and.
     >     (xd(ivar)*pushdn.ge.cutrap)) then
          xd(ivar)=xd(ivar)*pushdn
c        else if (rap.gt.rapp+delrap) then
        else if ((rap.gt.rapp+delrap).and.
     >     (xd(ivar)*pushup/2-xh(ivar)+
     >      xl(ivar).le.cutrap)) then
          xd(ivar)=xd(ivar)*pushup
        endif

        xm(ivar)=rap 
        xq(ivar)=rap*rap
30    continue

      return
      end


      subroutine ciclo(funct,n,x,xl,xh,xd,rapp,f,tf,ncicl,
     >nsub,tm,am,iseed)

      implicit real*8 (a-h,o-z)
      parameter (np=600)
      external funct
      common /sian1/pushup,pushdn,xt(np),xa(np),xm(np),xq(np),xn(np),
     >temp
      dimension x(np),xl(np),xh(np),xd(np),ix(np),irand(np),iy(np)

c      write(*,*) ' ciclo inizio'
c.....Sequenza casuale di numeri fra 1 e n
c.....memorizzata in irand
      do i=1,n
        ix(i)=i
      enddo
      i1=n+1
      do 20 i=1,n
        i1=i1-1
c.rand0        i2=1+int(rand(0)*dfloat(i1))
        i2=1+int(rand(0)*dfloat(i1))
        irand(i)=ix(i2)
        do 20 j=i2+1,i1
          ix(j-1)=ix(j)
20    continue


c.non e' un gran che      ndiv=1+int(rand(0)*dfloat(n))
      ndiv=n
      do i=1,ndiv-1
22      i1=2+int(rand(0)*dfloat(n-1))
        do j=1,i-1
          if (i1.eq.iy(j+1)) goto 22
        enddo
        iy(i+1)=i1
      enddo

      do 170 i=1,ndiv-2
        do 170 j=i+1,ndiv-1
          iei=iy(i+1)
          iej=iy(j+1)
          if (iei.gt.iej) then
            iei=iy(i+1)
            iej=iy(j+1)
            iy(i+1)=iej
            iy(j+1)=iei
          endif
170   continue
      iy(1)=1
      iy(ndiv+1)=n+1
c      do i=1,ndiv
c        write(*,*) ndiv,i,iy(i)
c        write(*,*) (irand(k),k=iy(i),iy(i+1)-1)
c      enddo


c.....Inizio del ciclo       
      do 100 ii=1,ndiv
      do i=iy(ii),iy(ii+1)-1
        ivar=irand(i)
c.......Variazione casuale fra Xmax e Xmin
c.rand0200     dum1=(rand(0)-0.5d0)*xd(ivar)
200     dum1=(rand(0)-0.5d0)*xd(ivar)

        xn(ivar)=x(ivar)+dum1

        if (xn(ivar).gt.xh(ivar)) then
          xn(ivar)=xn(ivar)
     >                      -xh(ivar)+xl(ivar)
        else if (xn(ivar).lt.xl(ivar)) then
          xn(ivar)=xn(ivar)
     >                       +xh(ivar)-xl(ivar)
        endif

        xt(ivar)=xt(ivar)+1.d0


c        write(*,*) ivar,x(ivar),xn(ivar),f,fnew,temp
      enddo
        tm=tm+1.d0
        call funct(n,xn,fnew)

c        write(*,*) ivar,x(ivar),xn(ivar),f,fnew,temp
c        pause
        delta=(f-fnew)/temp
        if (dabs(delta).gt.50.0) delta=50.0*sign(1.0d0,delta)
c.......Test di Metropolis
        if (delta.ge.0.d0) then
          f=fnew
          am=am+1.d0
      do i=iy(ii),iy(ii+1)-1
        ivar=irand(i)
          x(ivar)=xn(ivar)
          xa(ivar)=xa(ivar)+1.d0
c.rand0        else if (rand(0).lt.dexp(delta)) then
      enddo
        else if (rand(0).lt.dexp(delta)) then
c.rand0c        else if (rand(0).le.dexp(delta)) then
c        else if (rand(0).le.dexp(delta)) then
          f=fnew
          am=am+1.d0
      do i=iy(ii),iy(ii+1)-1
        ivar=irand(i)
          x(ivar)=xn(ivar)
          xa(ivar)=xa(ivar)+1.d0
      enddo
        else
      do i=iy(ii),iy(ii+1)-1
        ivar=irand(i)
          xn(ivar)=x(ivar)
      enddo
        endif  
 100  continue

c      write(*,*) ' ciclo fine'
      return
      end


      subroutine bonds(dist,angle,dihedr,catoma,catomb,catomc,cnatom)
c  ---------------------------------------------------------------------
c  The position of each atom a is given  by  its
c  relationship with three already known atoms (with labels i, j  and  k).
c  The distance a-i (a length), the angle a-i-j (rad) and the dihedral
c  angle a-i-j-k (rad) are required to identify the atom position.
c  ---------------------------------------------------------------------
c  Compute coordinates in the orthogonalized frame from distances,
c  angles and dihedral angles. Known coordinates have to be in an
c  orthogonal frame. Adapted from a program written by m.j.s. Dewar.
c  ---------------------------------------------------------------------
c   const  --  Dimensions and other constants
      implicit character (a-z)
      integer mspec, matom, msym, inp, out, no
      real*8    eps, huge
      parameter( mspec=3, matom=120, msym=48, inp=5, no=0 )
      parameter( eps=1.0d-4, huge=1.0d37 )
      integer i
      real*8  catoma(3),catomb(3),catomc(3),cnatom(3)
      integer kk
      logical crystl
      integer spc, k
      real*8 dist, angle, dihedr, cosa, sina, cosd, sind
      real*8 xa,ya,za, xb,yb,zb, xd,yd,zd
      real*8 xqd,yqd,zqd, xpa, ypa, xqa, zqa, xpd,ypd,zpd, xyb, yza
      real*8 xpb, xrd, rbc, costh, sinth, cosph, sinph, coskh, sinkh
 
c      write(*,*) ' natom ',natom
c      write(*,*) atoma,(catoma(kk),kk=1,3)
c      write(*,*) atomb,(catomb(kk),kk=1,3)
c      write(*,*) atomc,(catomc(kk),kk=1,3)
      cosa=dcos(angle)
      xb=catomb(1)-catoma(1)
      yb=catomb(2)-catoma(2)
      zb=catomb(3)-catoma(3)
      rbc=1.0d0/dsqrt(xb*xb+yb*yb+zb*zb)
 
      if ( 1.-abs(cosa).lt.eps ) then
c  Atoms atoma, atomb, and natom are collinear.
        rbc=dist*rbc
        cnatom(1)=catoma(1)-xb*rbc
        cnatom(2)=catoma(2)-yb*rbc
        cnatom(3)=catoma(3)-zb*rbc
      else
c  The atoms are not collinear.
        xa=catomc(1)-catoma(1)
        ya=catomc(2)-catoma(2)
        za=catomc(3)-catoma(3)
c  Rotate about the z-axis to make yb=0, and xb positive.  If xyb is
c  too small, first rotate the y-axis by 90 degrees.
        xyb=dsqrt(xb*xb+yb*yb)
        if (xyb.le.0.1d0 ) then
          xpa=za
          za=-xa
          xa=xpa
          xpb=zb
          zb=-xb
          xb=xpb
          xyb=dsqrt(xb*xb+yb*yb)
          k=+1
        else
          k=-1
        endif
 
c  Rotate about the y-axis to make zb vanish.
        costh=xb/xyb
        sinth=yb/xyb
        xpa=xa*costh+ya*sinth
        ypa=ya*costh-xa*sinth
        sinph=zb*rbc
        cosph=sqrt(abs(1.d0-sinph*sinph))
        xqa=xpa*cosph+za*sinph
        zqa=za*cosph-xpa*sinph
 
c  Rotate about the x-axis to make za=0, and ya positive.
        yza=dsqrt(ypa*ypa+zqa*zqa)
        if( yza.gt.eps ) then
          coskh=ypa/yza
          sinkh=zqa/yza
        else
c  Angle too small to be important.
          coskh=1.d0
          sinkh=0.d0
        endif
 
c  coordinates are: natom=(0,0,0),   a=(xqa,yza,0),   b=(rbc,0,0),
c  c=(0,0,0).   None are negative.
c  The coordinates of natom are evaluated in the new frame.
        sina=dsin(angle)
        sind=dsin(dihedr)
        cosd=dcos(dihedr)
        xd=dist*cosa
        yd=dist*sina*cosd
        zd=dist*sina*sind
c  Transform the coordinates back to the original system.
        ypd=yd*coskh-zd*sinkh
        zpd=zd*coskh+yd*sinkh
        xpd=xd*cosph-zpd*sinph
        zqd=zpd*cosph+xd*sinph
        xqd=xpd*costh-ypd*sinth
        yqd=ypd*costh+xpd*sinth
        if (k.ge.1) then
          xrd=-zqd
          zqd=xqd
          xqd=xrd
        endif
        cnatom(1)=xqd+catoma(1)
        cnatom(2)=yqd+catoma(2)
        cnatom(3)=zqd+catoma(3)
      endif
      do 10 i=1,3
        xqd = cnatom(i)
        if (dabs(xqd).le.eps) xqd  = 0.d0
        cnatom(i) = xqd  
10    continue
c      write(*,*) natom,(cnatom(i),i=1,3)
      return
      end


      subroutine aallmin(npar,x,f)

      include 'quafit2014.cmn'
c      include 'genfit.cmn'
      dimension x(7)


      a=xscale(x(1),0.d0,pi2)
      cb=xscale(x(2),-one,one)
      g=xscale(x(3),0.d0,pi2)
      p=xscale(x(4),0.d0,pi2)
      xt=xscale(x(5),-rscale,rscale)
      yt=xscale(x(6),-rscale,rscale)
      zt=xscale(x(7),-rscale,rscale)
      call bonds(rco,acaco,p,p3,p1,p2,p4)
      call mat(a,dacos(cb),g,rot)
      call rotrdi(p1(1),p1(2),p1(3),ppp1(1),ppp1(2),ppp1(3),
     >xt,yt,zt,rot)
      call rotrdi(p2(1),p2(2),p2(3),ppp2(1),ppp2(2),ppp2(3),
     >xt,yt,zt,rot)
      call rotrdi(p3(1),p3(2),p3(3),ppp3(1),ppp3(2),ppp3(3),
     >xt,yt,zt,rot)
      call rotrdi(p4(1),p4(2),p4(3),ppp4(1),ppp4(2),ppp4(3),
     >xt,yt,zt,rot)


      f=0.d0
      do k=1,3
        dum1=ppp1(k)-pp1(k)
        f=f+dum1**2
        dum1=ppp2(k)-pp2(k)
        f=f+dum1**2
        dum1=ppp3(k)-pp3(k)
        f=f+dum1**2
        dum1=ppp4(k)-pp4(k)
        f=f+dum1**2
      enddo
      f=dsqrt(f/4)
      return
      end


      subroutine linkercoo(iflag,istate,iilink,iconf)  

c.....suppongo di aver gia' chiamato la  linkerendsfind(istate,iilink,iconf)
c.....qui lo scopo e' di selezionare un set di psi,phi,chi e di memorizzare le coordinate atomiche
c.....il tutto ovviamente e' legato al linker iilink-esimo

      include 'quafit2014.cmn'

      jconf=iconf+4
      iflag3=0
      if (iflag.eq.1) then
        iflag3=jlink(istate,iilink)!Flag for phi, psi and chi variations
      endif
      if (iflag3.eq.2) iflag3=1
c      write(*,*) "iflag3",iflag3
      i_c=0
      if (isitelink(istate,iilink,1).ne.0) i_c=1 !!(i_c.eq.1) then !! conosco il C-term del dominio a cui va attaccato, dunque l'n-term del link
      if (i_c.eq.1) then
      do k=1,3
        rncaco_c(1,k)=cooaallrot_n(istate,iilink,1,k)
        rncaco_c(2,k)=cooaallrot_ca(istate,iilink,1,k)
        rncaco_c(3,k)=cooaallrot_c(istate,iilink,1,k)
        rncaco_c(4,k)=cooaallrot_o(istate,iilink,1,k)
      enddo
      endif
      i_n=0
      if (isitelink(istate,iilink,3).ne.0) i_n=1
      if (i_n.eq.1) then
      do k=1,3
        rncaco_n(1,k)=cooaallrot_n(istate,iilink,2,k)
        rncaco_n(2,k)=cooaallrot_ca(istate,iilink,2,k)
        rncaco_n(3,k)=cooaallrot_c(istate,iilink,2,k)
        rncaco_n(4,k)=cooaallrot_o(istate,iilink,2,k)
      enddo
      endif
      n=naalink(istate,iilink)
      do k=1,n
        iseq(k)=iprimlink(istate,iilink,k)
c        write(*,*) n,k,iseq(k)
      enddo
      iruoto=1 !ruota i gruppi laterali
      ipdb3=0
      name=' '
      do iaa=1,n
        nni=0
c      write(*,*) 'EE',istate,iilink,jconf,iaa,
c     >phi_lnk(istate,iilink,jconf,iaa)
        dihed(iaa,1+3*nni)=phi_lnk(istate,iilink,jconf,iaa)
        dihed(iaa,2+3*nni)=psi_lnk(istate,iilink,jconf,iaa)
        dihed(iaa,3+3*nni)=chi_lnk(istate,iilink,jconf,iaa)
        nni=2
        dihed(iaa,1+3*nni)=phi_lnk(istate,iilink,2,iaa)
        dihed(iaa,2+3*nni)=psi_lnk(istate,iilink,2,iaa)
        dihed(iaa,3+3*nni)=chi_lnk(istate,iilink,2,iaa)
        nni=3
        dihed(iaa,1+3*nni)=phi_lnk(istate,iilink,3,iaa)
        dihed(iaa,2+3*nni)=psi_lnk(istate,iilink,3,iaa)
        dihed(iaa,3+3*nni)=chi_lnk(istate,iilink,3,iaa)
c.......delta !mappati sul range -pi/2 pi/2
        nni=1
        dum1=(phi_lnk(istate,iilink,3,iaa)-phi_lnk(istate,iilink,2,iaa)) 
        if (dum1.ne.0.d0) then
        dihed(iaa,1+3*nni)=pih*(2*phi_lnk(istate,iilink,4,iaa)
     >-(phi_lnk(istate,iilink,3,iaa)+phi_lnk(istate,iilink,2,iaa)))
     >/dum1
        else
        dihed(iaa,1+3*nni)=0.d0
        endif
        dum1=(psi_lnk(istate,iilink,3,iaa)-psi_lnk(istate,iilink,2,iaa)) 
        if (dum1.ne.0.d0) then
        dihed(iaa,2+3*nni)=pih*(2*psi_lnk(istate,iilink,4,iaa)
     >-(psi_lnk(istate,iilink,3,iaa)+psi_lnk(istate,iilink,2,iaa)))
     >/dum1
        else
        dihed(iaa,2+3*nni)=0.d0
        endif
        dum1=(chi_lnk(istate,iilink,3,iaa)-chi_lnk(istate,iilink,2,iaa)) 
        if (dum1.ne.0.d0) then
        dihed(iaa,3+3*nni)=pih*(2*chi_lnk(istate,iilink,4,iaa)
     >-(chi_lnk(istate,iilink,3,iaa)+chi_lnk(istate,iilink,2,iaa)))
     >/dum1
        else
        dihed(iaa,3+3*nni)=0.d0
        endif
c        write(*,*) chi_lnk(istate,iilink,3,iaa),
c     >chi_lnk(istate,iilink,2,iaa)
        if (iseq(iaa).eq.15) dihed(iaa,3)=0.d0
c        write(*,*) 'DD',iaa,(dihed(iaa,k),k=1,12)
c.......Memorizzo i valori precedenti come "old"
        phi_old(istate,iilink,jconf,iaa)=
     >  phi_lnk(istate,iilink,jconf,iaa)
        phi_old(istate,iilink,4,iaa)=
     >  phi_lnk(istate,iilink,4,iaa)
        psi_old(istate,iilink,jconf,iaa)=
     >  psi_lnk(istate,iilink,jconf,iaa)
        psi_old(istate,iilink,4,iaa)=
     >  psi_lnk(istate,iilink,4,iaa)
        chi_old(istate,iilink,jconf,iaa)=
     >  chi_lnk(istate,iilink,jconf,iaa)
        chi_old(istate,iilink,4,iaa)=
     >  chi_lnk(istate,iilink,4,iaa)
        do ia=1,2
          umsaa_old(istate,iilink,iconf,iaa,ia)=
     >    umsaa(istate,iilink,iconf,iaa,ia)
          ausaa_old(istate,iilink,iconf,iaa,ia)=
     >    ausaa(istate,iilink,iconf,iaa,ia)
          cbuaa_old(istate,iilink,iconf,iaa,ia)=
     >    cbuaa(istate,iilink,iconf,iaa,ia)
          aasaa_old(istate,iilink,iconf,iaa,ia)=
     >    aasaa(istate,iilink,iconf,iaa,ia)
          cbaaa_old(istate,iilink,iconf,iaa,ia)=
     >    cbaaa(istate,iilink,iconf,iaa,ia)
          gasaa_old(istate,iilink,iconf,iaa,ia)=
     >    gasaa(istate,iilink,iconf,iaa,ia)
        enddo
      enddo


      do iaa=n+1,n+1
        nni=0
c      write(*,*) 'EE',istate,iilink,jconf,iaa,
c     >phi_lnk(istate,iilink,jconf,iaa)
        dihed(iaa,1+3*nni)=phi_lnk(istate,iilink,jconf,iaa)
        nni=2
        dihed(iaa,1+3*nni)=phi_lnk(istate,iilink,2,iaa)
        nni=3
        dihed(iaa,1+3*nni)=phi_lnk(istate,iilink,3,iaa)
c.......delta !mappati sul range -pi/2 pi/2
        nni=1
        dum1=(phi_lnk(istate,iilink,3,iaa)-phi_lnk(istate,iilink,2,iaa)) 
        if (dum1.ne.0.d0) then
        dihed(iaa,1+3*nni)=pih*(2*phi_lnk(istate,iilink,4,iaa)
     >-(phi_lnk(istate,iilink,3,iaa)+phi_lnk(istate,iilink,2,iaa)))
     >/dum1
        else
        dihed(iaa,1+3*nni)=0.d0
        endif
        if (iseq(iaa).eq.15) dihed(iaa,3)=0.d0
c        write(*,*) 'DD',iaa,(dihed(iaa,k),k=1,12)
c.......Memorizzo i valori precedenti come "old"
        phi_old(istate,iilink,jconf,iaa)=
     >  phi_lnk(istate,iilink,jconf,iaa)
        phi_old(istate,iilink,4,iaa)=
     >  phi_lnk(istate,iilink,4,iaa)
      enddo

      do ia=1,3
      do ib=1,3
        terminals_old(istate,iilink,iconf,ia,ib)=
     >  terminals(istate,iilink,iconf,ia,ib)
      enddo
      enddo
      do kk=1,1+i_n+i_c  !!! non sono sicuro di questo 1+ ...
      do k=2,3  !! l'uno (phi) l'hai gia' fatto.
        dihed(n+kk,k)=71/conv!rand(0)*pi2-pi
        dihed(n+kk,k+3)=pih/3
        dihed(n+kk,k+6)=-pi
        dihed(n+kk,k+9)= pi  
      enddo
      enddo
c      write(*,*) ' iflag3 ',iflag3
c.....se la distanza tra i due calpha e' superiore a n*3.8 pongo dum1=1.e10 e me ne vado
      i78=0
c      write(*,*) ' 1 ci sono',iflag3,i_c,i_n
      if (iflag3.eq.1) then
        call cpolar(rncaco_c(2,1)-rncaco_n(2,1),
     >              rncaco_c(2,2)-rncaco_n(2,2),
     >              rncaco_c(2,3)-rncaco_n(2,3),
     >              dum2,
     >              dum3,
     >              dum4)
c        write(*,*) ' link2 ',iilink,dum2,3.8*(n-1)
c      write(*,*) ' 2 ci sono',dum2,n,dum2-3.8*(n-1)
        dum2=dum2-3.8*n!(n-1) !dec 2014
        if (dum2.gt.0.d0) then
          i78=1
          iflag3=0
c      write(*,*) ' 3 ci sono'
          call link2(istate,iilink,iconf,dum1)
          dum1=dum1+dum2**2
          iflag3=1
        endif
      endif
c      write(*,*) ' 4 ci sono',i78
c      do iaa=1,n
c        do jj=1,3
c        write(*,*) 'Dopo ',iaa,(conv*dihed(iaa,3*k+jj),k=0,3)
c        if (jj.eq.1)  write(*,*) 'Letto ',iaa
c     >,conv*phi_lnk(istate,iilink,4+iconf,iaa)
c     >,conv*phi_lnk(istate,iilink,4,iaa)
c     >,conv*phi_lnk(istate,iilink,2,iaa)
c     >,conv*phi_lnk(istate,iilink,3,iaa)
c        if (jj.eq.2)  write(*,*) 'Letto ',iaa
c     >,conv*psi_lnk(istate,iilink,4+iconf,iaa)
c     >,conv*psi_lnk(istate,iilink,4,iaa)
c     >,conv*psi_lnk(istate,iilink,2,iaa)
c     >,conv*psi_lnk(istate,iilink,3,iaa)
c        if (jj.eq.3)  write(*,*) 'Letto ',iaa
c     >,conv*chi_lnk(istate,iilink,4+iconf,iaa)
c     >,conv*chi_lnk(istate,iilink,4,iaa)
c     >,conv*chi_lnk(istate,iilink,2,iaa)
c     >,conv*chi_lnk(istate,iilink,3,iaa)
c
c        enddo
c      enddo
c      stop

      if (i78.eq.0) call link2(istate,iilink,iconf,dum1)
      qverl_lnk(istate,iilink,iconf)=dum1
      n=naalink(istate,iilink) !ripristino n
c.....Memorizzo i nuovi valori
      do iaa=1,n
        phi_lnk(istate,iilink,jconf,iaa)=dihed(iaa,1)
        psi_lnk(istate,iilink,jconf,iaa)=dihed(iaa,2)
        chi_lnk(istate,iilink,jconf,iaa)=dihed(iaa,3)
c.......delta !mappati inversamente sul range -pi/2 pi/2
        if (dihed(iaa,4).ne.0.d0) then
        phi_lnk(istate,iilink,4,iaa)=
     > (phi_lnk(istate,iilink,3,iaa)+phi_lnk(istate,iilink,2,iaa))/2
     >+(phi_lnk(istate,iilink,3,iaa)-phi_lnk(istate,iilink,2,iaa))
     >*dihed(iaa,4)/pi
        else
        phi_lnk(istate,iilink,4,iaa)=0.d0
        endif
        if (dihed(iaa,5).ne.0.d0) then
        psi_lnk(istate,iilink,4,iaa)=
     > (psi_lnk(istate,iilink,3,iaa)+psi_lnk(istate,iilink,2,iaa))/2
     >+(psi_lnk(istate,iilink,3,iaa)-psi_lnk(istate,iilink,2,iaa))
     >*dihed(iaa,5)/pi
        else
        psi_lnk(istate,iilink,4,iaa)=0.d0
        endif
        if (dihed(iaa,6).ne.0.d0) then
        chi_lnk(istate,iilink,4,iaa)=
     > (chi_lnk(istate,iilink,3,iaa)+chi_lnk(istate,iilink,2,iaa))/2
     >+(chi_lnk(istate,iilink,3,iaa)-chi_lnk(istate,iilink,2,iaa))
     >*dihed(iaa,6)/pi
        else
        chi_lnk(istate,iilink,4,iaa)=0.d0
        endif

c       write(*,*) 
c     >' phi ',iaa,phi_lnk(istate,iilink,jconf,iaa),
c     >   phi_lnk(istate,iilink,4,iaa),dihed(iaa,4),
c     >' psi ',iaa,psi_lnk(istate,iilink,jconf,iaa),
c     >   psi_lnk(istate,iilink,4,iaa),dihed(iaa,5),
c     >' chi ',iaa,chi_lnk(istate,iilink,jconf,iaa),
c     >   chi_lnk(istate,iilink,4,iaa),dihed(iaa,6)

c        write(31,*) iaa,umsaa(istate,iilink,iconf,iaa,1)
c        write(31,*) iaa,ausaa(istate,iilink,iconf,iaa,1)*conv
c        write(31,*) iaa,cbuaa(istate,iilink,iconf,iaa,1)
c        write(31,*) iaa,aasaa(istate,iilink,iconf,iaa,1)*conv
c        write(31,*) iaa,cbaaa(istate,iilink,iconf,iaa,1)
c        write(31,*) iaa,gasaa(istate,iilink,iconf,iaa,1)*conv
      enddo
      do iaa=n+1,n+1
        phi_lnk(istate,iilink,jconf,iaa)=dihed(iaa,1)
c.......delta !mappati inversamente sul range -pi/2 pi/2
        if (dihed(iaa,4).ne.0.d0) then
        phi_lnk(istate,iilink,4,iaa)=
     > (phi_lnk(istate,iilink,3,iaa)+phi_lnk(istate,iilink,2,iaa))/2
     >+(phi_lnk(istate,iilink,3,iaa)-phi_lnk(istate,iilink,2,iaa))
     >*dihed(iaa,4)/pi
        else
        phi_lnk(istate,iilink,4,iaa)=0.d0
        endif
      enddo

cc.....amides
c      do iaa=1,n-1
c        write(31,*) iaa,umsaa(istate,iilink,iconf,iaa,2)
c        write(31,*) iaa,ausaa(istate,iilink,iconf,iaa,2)*conv
c        write(31,*) iaa,cbuaa(istate,iilink,iconf,iaa,2)
c        write(31,*) iaa,aasaa(istate,iilink,iconf,iaa,2)*conv
c        write(31,*) iaa,cbaaa(istate,iilink,iconf,iaa,2)
c        write(31,*) iaa,gasaa(istate,iilink,iconf,iaa,2)*conv
c      enddo

c      open(unit=32,file='/sas/linker.pdb')
c      call linkerpdb(istate,iilink,iconf)
c      close(32)
c      pause

      end




      subroutine link2(istate,iilink,iconf,f)

c.....n numero di AA
c.....iseq(n) sequenza primaria (1-22)
c.....rncaco_c(4,3) array del c_term AA a cui il link va legato (se i_c=1)
c.....rncaco_n(4,3) array del n_term AA a cui il link va legato (se i_n=1)
c.....dihed(n,) angoli diedri input/output
c.....iflag3=1 minimizza dihed, =0 non minimizza
c.....ipdb3=1 crea il file di nome name.pdb

      include 'quafit2014.cmn'

c##################################################
      external linker
      dimension xpar(mxaapar),h(mxaapar*(mxaapar+1)/2),
     >gpar(mxaapar),w(3*mxaapar)
      dimension xl(mxaapar),xh(mxaapar),xs(mxaapar),xf(mxaapar)
 

c      do k=1,n
c        write(37,*) k,phi_lnk(istate,iilink,4+iconf,k)*conv
c        write(37,*) k,psi_lnk(istate,iilink,4+iconf,k)*conv
c        write(37,*) k,chi_lnk(istate,iilink,4+iconf,k)*conv
c      enddo


c      do iaa=1,n
c        do jj=1,3
c        write(*,*) 'Dopo ',iaa,(conv*dihed(iaa,3*k+jj),k=0,3)
c        if (jj.eq.1)  write(*,*) 'Letto ',iaa
c     >,conv*phi_lnk(istate,iilink,4+iconf,iaa)
c     >,conv*phi_lnk(istate,iilink,4,iaa)
c     >,conv*phi_lnk(istate,iilink,2,iaa)
c     >,conv*phi_lnk(istate,iilink,3,iaa)
c        if (jj.eq.2)  write(*,*) 'Letto ',iaa
c     >,conv*psi_lnk(istate,iilink,4+iconf,iaa)
c     >,conv*psi_lnk(istate,iilink,4,iaa)
c     >,conv*psi_lnk(istate,iilink,2,iaa)
c     >,conv*psi_lnk(istate,iilink,3,iaa)
c        if (jj.eq.3)  write(*,*) 'Letto ',iaa
c     >,conv*chi_lnk(istate,iilink,4+iconf,iaa)
c     >,conv*chi_lnk(istate,iilink,4,iaa)
c     >,conv*chi_lnk(istate,iilink,2,iaa)
c     >,conv*chi_lnk(istate,iilink,3,iaa)
c        enddo
c      enddo
cc      stop

      ic=0
      f=zer
c.....Primo caso, le due estremita' sono fisse.
c      write(*,*) i_c,i_n
c      pause
      do i=1,n
        do k=1,3
          i4lnk(i,k)=0
        enddo
      enddo


      if (i_c.eq.1.and.i_n.eq.1) then
      n=n+2 !si aggiungono due AA
      n1=2
      n2=n-1
      do i=2,n-1
        iseq2(i)=iseq(i-1)
      enddo
      iseq2(1)=1 ! e' un'ALA
      iseq2(n)=1 ! e' un'ALA
      do k=1,3
        rn(1,k)=rncaco_c(1,k)
c        write(37,*) ' rncaco_c(1,k) ',rncaco_c(1,k)
        rca(1,k)=rncaco_c(2,k)
c        write(37,*) ' rncaco_c(2,k) ',rncaco_c(2,k)
        rc(1,k)=rncaco_c(3,k)
c        write(37,*) ' rncaco_c(3,k) ',rncaco_c(3,k)
        ro(1,k)=rncaco_c(4,k)
c        write(37,*) ' rncaco_c(4,k) ',rncaco_c(4,k)
        rn(n+2,k)=rncaco_n(1,k) 
c        write(37,*) ' rncaco_n(1,k)  ',rncaco_n(1,k) 
!! n+2 lo usi solo per memorizzare la parte finale
        rca(n+2,k)=rncaco_n(2,k)
c        write(37,*) ' rncaco_n(2,k) ',rncaco_n(2,k)
        rc(n+2,k)=rncaco_n(3,k)
c        write(37,*) ' rncaco_n(3,k) ',rncaco_n(3,k)
        ro(n+2,k)=rncaco_n(4,k)
c        write(37,*) ' rncaco_n(4,k) ',rncaco_n(4,k)
      enddo
c.....psifirst e psilast
      do k=1,3
        p1(k)=rn(1,k)
        p2(k)=rca(1,k)
        p3(k)=ro(1,k) !era ro
        p4(k)=rc(1,k) !era rc
      enddo
      do i=1,3
        bbb1(i)=p2(i)-p1(i)      
        bbb2(i)=p3(i)-p2(i)      
        bbb3(i)=p4(i)-p3(i)      
c        write(37,*) i,bbb1(i),bbb2(i),bbb3(i)
      enddo
      call phidihed(bbb1,bbb2,bbb3,dum1,dum2,psifirst)


      do k=1,3
        p1(k)=rn(n+2,k)
        p2(k)=rca(n+2,k)
        p3(k)=ro(n+2,k) !era ro
        p4(k)=rc(n+2,k) !era rc
      enddo
      do i=1,3
        bbb1(i)=p2(i)-p1(i)      
        bbb2(i)=p3(i)-p2(i)      
        bbb3(i)=p4(i)-p3(i)      
c        write(37,*) i,bbb1(i),bbb2(i),bbb3(i)
      enddo
      call phidihed(bbb1,bbb2,bbb3,dum1,dum2,psilast)


      if (iseq2(1).eq.15) then !proline
      phi_r(1)=(-75.d0/conv)!(-pi/3)!0.
      else
      phi_r(1)=0.d0
      endif
c      write(37,*) ' first last ',psifirst,psilast
      psi_r(1)=psifirst

      if (iseq2(n).eq.15) then !proline
      phi_r(n)=(-75.d0/conv)!(-pi/3)!0.
      else
      phi_r(n)=dihed(n,1)
      endif
      psi_r(n)=psilast
      dihed(n,2)=psi_r(n)


c.....psi,phi
      i4=0
      do i=1,n+1  !perche +1? vedi in parte riga 17560
        if (i.ne.1) then
        if (iseq2(i).ne.15.and.dihed(i-1,3+1).ne.0.d0) then !proline
          i4=i4+1
c          write(*,*)' inv ',i4,'(i-1,1)',
c     >dihed(i-1,1),i-1,dihed(i-1,1+6),dihed(i-1,1+9)
          xpar(i4)=xscinv(dihed(i-1,1),dihed(i-1,1+6),dihed(i-1,1+9))!phi
          gpar(i4)=dihed(i-1,3+1)
          i4lnk(i-1,1)=i4
        endif
c.provo        if (i.ne.1.and.i.ne.n) then
        if (dihed(i-1,3+2).ne.0.d0) then
          i4=i4+1
c          write(*,*)' inv ',i4,'(i-1,2)'
          xpar(i4)=xscinv(dihed(i-1,2),dihed(i-1,2+6),dihed(i-1,2+9))!psi
          gpar(i4)=dihed(i-1,3+2)
          i4lnk(i-1,2)=i4
        endif
        endif
      enddo

c      write(*,*) ' 2 ci sono'
c      do i=1,n
c        isec(i)=0 
c        gru(i)='ALA'
c        iammty(i)=1
c      enddo

      do i=1,n 
        if (i.eq.1) then
          chi(i)=0.d0
        else
          chi(i)=dihed(i-1,3)
        endif
        chi00(i)=chi(i)
        ch3=amm(iseq2(i))
        gru(i)=ch3
        if (iruoto.eq.1) then
          if (ch3.ne.'GLY'.and.ch3.ne.'PRO'.and.ch3.ne.'ALA') then
            if (i.eq.1) then 
              i4=i4+1
c          write(*,*)' inv ',i4,'chi00(i)'
              xpar(i4)=xscinv(chi00(i),-pi,pi)!chi  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
              gpar(i4)=pih/2
            else
              if (dihed(i-1,3+3).ne.0.d0) then
              i4=i4+1
c          write(*,*)' inv ',i4,'(i-1,3+6)'
              xpar(i4)=xscinv(chi00(i),dihed(i-1,3+6),dihed(i-1,3+9))!chi  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
              gpar(i4)=dihed(i-1,3+3)
              i4lnk(i-1,3)=i4
              endif
            endif
          endif
        endif
c        write(*,*) i,iseq2(i),chi(i),ch3,chi00(i),i4,xpar(i4),gpar(i4)
      enddo
c      pause
      npar=i4
      if (npar.gt.mxaapar) stop 'mxaapar'

c      stop!ccccc

c      write(*,*) ' 23 ci sono'
      call linker(npar,xpar,f)
c      write(*,*) npar,f
c      stop


      else if (i_c.eq.1) then !! conosco il C-term del dominio a cui va attaccato, dunque l'n-term del link
      n=n+1
      n1=2!1!2
      n2=n!n-1!n
c.....il psi(1) dovra' essere il psi di rncaco_c
      do i=2,n
        iseq2(i)=iseq(i-1)
      enddo
      iseq2(1)=1 ! e' un'ALA
      do k=1,3
        rn(1,k)=rncaco_c(1,k)
        rca(1,k)=rncaco_c(2,k)
        rc(1,k)=rncaco_c(3,k)
        ro(1,k)=rncaco_c(4,k)
      enddo
c.....psifirst e psilast
      do k=1,3
        p1(k)=rn(1,k)
        p2(k)=rca(1,k)
        p3(k)=ro(1,k) !era ro
        p4(k)=rc(1,k) !era rc
      enddo
      do i=1,3
        bbb1(i)=p2(i)-p1(i)      
        bbb2(i)=p3(i)-p2(i)      
        bbb3(i)=p4(i)-p3(i)      
      enddo
      call phidihed(bbb1,bbb2,bbb3,dum1,dum2,psifirst)
      if (iseq2(1).eq.15) then !proline
      phi_r(1)=(-75.d0/conv)!(-pi/3)!0.
      else
      phi_r(1)=0.d0
      endif
      psi_r(1)=psifirst



c.....psi,phi
      i4=0
      do i=1,n+1
        if (i.ne.1) then
          if (dihed(i-1,3+1).ne.0.d0.and.iseq2(i).ne.15) then
          i4=i4+1
c          write(*,*)' inv ',i4,'(i-1,1)'
          xpar(i4)=xscinv(dihed(i-1,1),dihed(i-1,1+6),dihed(i-1,1+9))!phi
          gpar(i4)=dihed(i-1,3+1)
          i4lnk(i-1,1)=i4
          endif
          if (dihed(i-1,3+2).ne.0.d0) then
          i4=i4+1
c          write(*,*)' inv ',i4,'(i-1,2)'
          xpar(i4)=xscinv(dihed(i-1,2),dihed(i-1,2+6),dihed(i-1,2+9))!psi
          gpar(i4)=dihed(i-1,3+2)
          i4lnk(i-1,2)=i4
          endif
        endif
      enddo

c      write(*,*) ' 2 ci sono'
c      do i=1,n
c        isec(i)=0 
c        gru(i)='ALA'
c        iammty(i)=1
c      enddo
      do i=1,n 
        if (i.eq.1) then
          chi(i)=0.d0
        else
          chi(i)=dihed(i-1,3)
        endif
        chi00(i)=chi(i)
        ch3=amm(iseq2(i))
        gru(i)=ch3
        if (iruoto.eq.1) then
          if (ch3.ne.'GLY'.and.ch3.ne.'PRO'.and.ch3.ne.'ALA' ) then
c            if (i.eq.1) then 
c              i4=i4+1
cc          write(*,*)' inv ',i4,'chi00(i)'
c              xpar(i4)=xscinv(chi00(i),-pi,pi)!chi  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
c              gpar(i4)=pih/2
c            else 
           if (dihed(i-1,3+3).ne.0.d0) then
              i4=i4+1
c          write(*,*)' inv ',i4,'(i-1,3+6)'
              xpar(i4)=xscinv(chi00(i),dihed(i-1,3+6),dihed(i-1,3+9))!chi  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
              gpar(i4)=dihed(i-1,3+3)
              i4lnk(i-1,3)=i4
            endif
          endif
        endif
      enddo
      npar=i4
      if (npar.gt.mxaapar) stop 'mxaapar'

c      write(*,*) ' 24 ci sono'
      call linker(npar,xpar,f)


      else if (i_n.eq.1) then !! conosco l'N-term del dominio a cui va attaccato, dunque il c-term del link
      n=n+1    ! e quindi i_c=0
      n1=1     !catena a ritroso
      n2=n-1
      do i=1,n-1
        iseq2(i)=iseq(i)
      enddo
      iseq2(n)=1 ! e' un'ALA
      do k=1,3
        rn(n,k)=rncaco_n(1,k)
        rca(n,k)=rncaco_n(2,k)
        rc(n,k)=rncaco_n(3,k)
        ro(n,k)=rncaco_n(4,k)
      enddo
c.....psifirst
      do k=1,3
        p1(k)=rn(n,k)
        p2(k)=rca(n,k)
        p3(k)=ro(n,k) !era ro
        p4(k)=rc(n,k) !era rc
      enddo
      do i=1,3
        bbb1(i)=p2(i)-p1(i)      
        bbb2(i)=p3(i)-p2(i)      
        bbb3(i)=p4(i)-p3(i)      
      enddo
      call phidihed(bbb1,bbb2,bbb3,dum1,dum2,psifirst)
      if (iseq2(n).eq.15) then !proline
      phi_r(n)=(-75.d0/conv)!(-pi/3)!0.
      else
      phi_r(n)=dihed(n,1)
      endif
      psi_r(n)=psifirst
c      write(*,*) phi_r(n)*conv,psi_r(n)*conv
      dihed(n,2)=psi_r(n)
c.....psi,phi
      i4=0
      do i=1,n+1
        if (i.ne.1) then
          if (iseq2(i).ne.15) then !proline
          if (dihed(i,3+1).ne.0.d0) then
          i4=i4+1
c          write(*,*)' inv ',i4,'(i,1)'
          xpar(i4)=xscinv(dihed(i,1),dihed(i,1+6),dihed(i,1+9))!phi
          gpar(i4)=dihed(i,3+1)
          i4lnk(i,1)=i4
          endif
          endif
          if (dihed(i,3+2).ne.0.d0) then
          i4=i4+1
c          write(*,*)' inv ',i4,'(i,2)'
          xpar(i4)=xscinv(dihed(i,2),dihed(i,2+6),dihed(i,2+9))!psi
          gpar(i4)=dihed(i,3+2)
          i4lnk(i,2)=i4
          endif
        endif
      enddo
c      write(*,*) ((dihed(i,kk)*conv,kk=1,2),i=n,n+1)
c      write(*,*) 'qui'
c      stop

c      write(*,*) ' 2 ci sono'
c      do i=1,n
c        isec(i)=0 
c        gru(i)='ALA'
c        iammty(i)=1
c      enddo

      do i=1,n 
        chi(i)=dihed(i,3)
        chi00(i)=chi(i)
        ch3=amm(iseq2(i))
        gru(i)=ch3
        if (iruoto.eq.1) then
          if (ch3.ne.'GLY'.and.ch3.ne.'PRO'.and.ch3.ne.'ALA' ) then
          if (dihed(i,3+3).ne.0.d0) then
            i4=i4+1
c          write(*,*)' inv ',i4,'(i,3+6)'
            xpar(i4)=xscinv(chi00(i),dihed(i,3+6),dihed(i,3+9))!chi  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
            gpar(i4)=dihed(i,3+3)
            i4lnk(i,3)=i4
          endif
          endif
        endif
      enddo
      npar=i4
      if (npar.gt.mxaapar) stop 'mxaapar'

c      write(*,*) ' 25 ci sono'
      call linker(npar,xpar,f)


      else !i_c=i_n=0

      n1=1
      n2=n
      do i=1,n
        iseq2(i)=iseq(i)
      enddo
      rN (1,1)=  -7.610
      rN (1,2)=  18.984
      rN (1,3)=   9.442
      rCA(1,1)=  -8.681
      rCA(1,2)=  19.915
      rCA(1,3)=   9.821
      rC (1,1)=  -9.827
      rC (1,2)=  18.969
      rC (1,3)=  10.168

c      if (iseq2(1).eq.15) then !Proline
c      rN (1,1)=  -0.788 
c      rN (1,2)=   1.170
c      rN (1,3)=   0.410
c      rCA(1,1)=   0.000
c      rCA(1,2)=   0.000
c      rCA(1,3)=   0.000
c      rC (1,1)=   1.407
c      rC (1,2)=   0.000
c      rC (1,3)=   0.590
c      endif
c.....psi,phi
      i4=0
      do i=1,n+1
        if (iseq2(i).ne.15) then !proline
        if (dihed(i,3+1).ne.0.d0) then
        i4=i4+1
c          write(*,*)' inv ',i4,'(i,1)'
        xpar(i4)=xscinv(dihed(i,1),dihed(i,1+6),dihed(i,1+9))!phi
        gpar(i4)=dihed(i,3+1)
        i4lnk(i,1)=i4
        endif
        endif
        if (dihed(i,3+2).ne.0.d0) then
        i4=i4+1
c          write(*,*)' inv ',i4,'(i,2)'
        xpar(i4)=xscinv(dihed(i,2),dihed(i,2+6),dihed(i,2+9))!psi
        gpar(i4)=dihed(i,3+2)
        i4lnk(i,2)=i4
        endif
      enddo

c      write(*,*) ' 2 ci sono'
c      do i=1,n
c        isec(i)=0 
c        gru(i)='ALA'
c        iammty(i)=1
c      enddo

      do i=1,n 
        chi(i)=dihed(i,3)
        chi00(i)=chi(i)
        ch3=amm(iseq2(i))
        gru(i)=ch3
        if (iruoto.eq.1) then
          if (ch3.ne.'GLY'.and.ch3.ne.'PRO'.and.ch3.ne.'ALA' ) then
          if (dihed(i,3+3).ne.0.d0) then
            i4=i4+1
c          write(*,*)' inv ',i4,'(i,3+6)'
            xpar(i4)=xscinv(chi00(i),dihed(i,3+6),dihed(i,3+9))!chi  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
            gpar(i4)=dihed(i,3+3)
            i4lnk(i,3)=i4
          endif
          endif
        endif
      enddo
      npar=i4
      if (npar.gt.mxaapar) stop 'mxaapar'

c      write(*,*) ' 21 ci sono'
      call linker(npar,xpar,f)


      endif

c      write(*,*) ' 1 ci sono'
      if (iflag3.eq.1) then  !minimizza
c      if (iflag3.eq.1.and.f.ne.0) then  !minimizza
      do kk=1,1

c      write(*,*) f
         ti=f/3
         tf=1.e-3!f*1.e-5!4
         ncicl=5!10!60
         nsub=5!20!30
          do i=1,npar
            xl(i)=-pih
            xh(i)=pih
c            gpar(i)=pih/3!0.1 !! li hai gia' definiti
          enddo
        call sian(linker,npar,xpar,xl,xh,gpar,0.5d0,f,ti,tf,ncicl,nsub,
     >       tm,am,iseed) 
        enddo
c      maxitm=3000
c      call monk(linker,xpar,f,npar,maxitm,iseed)
c      maxits=100000
c      cut=1.d-10
c      call simplx(linker,xpar,f,npar,cut,itc,maxits)
c      write(*,*) ' itc ',itc
c      write(*,*) f
c      maxitz=100000
c          call zxmin(linker,npar,4,maxitz,3,xpar,h,gpar,f,w,ierr)
c      write(*,*) ' itc2 ',int(w(2)+0.5d0)
c      write(*,*) f
      endif


c      write(*,*) ' 22 ci sono'
      call linker(npar,xpar,f)
c        do i=1,n 
c          write(*,*) chi(i)*conv,iseq2(i)
c        enddo
c        pause
c.....giusto per una prova
c      n1=1
c      n2=n      
c      goto 3737 !salto
c.......poi se ne andra'
c        write(ch4(1:2),'(i2)') n
c        if (ch4(1:1).eq.' ') ch4(1:1)='0'
c        open(unit=1,file='/sas/AA/array_'//ch4(1:2)//'.pdb')
c        nato=0
c        do i=1,n
c        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' N  ','ALA',i,(rn(i,k),k=1,3)
c        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' CA ','ALA',i,(rca(i,k),k=1,3)
c        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' C  ','ALA',i,(rc(i,k),k=1,3)
c        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' O  ','ALA',i,(ro(i,k),k=1,3)
c        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' CB ','ALA',i,(rcb(i,k),k=1,3)


c      write(*,'(a)')
c     >' AA ...................................:'//ch3
c      write(*,2208) 
c     >' ux uy uz AA (Angs.) ..................:'
c     >,(centreaa(i,k,1),k=1,3),i
c      write(*,2208) 
c     >' Alpha Beta Gamma AA (deg) ............:',alphaaa(i,1)*conv
c     >,betaaa(i,1)*conv,gammaaa(i,1)*conv,i
c        enddo

c        close(1)
 3737   continue!fine salto


c.....ripristino l'ultimo gruppo
      

cc.....Troviamo i gruppi ammidici rigidi
c      open(unit=3,file='/sas/AA/amide.pdb')
c      sum1=0.d0 
c      sum2=0.d0
c      sum3=0.d0
c      do m=1,5
c        read(3,1003) ch4,x,y,z
c        caard(m,1)=x
c        caard(m,2)=y
c        caard(m,3)=z
c        atom4(m)=ch4
c        if (m.ne.1.and.m.ne.5) then !(escludo i due CA)
c          sum1=sum1+x
c          sum2=sum2+y
c          sum3=sum3+z
c        endif
c      enddo
c      close(3)
c      caard(6  ,1)=sum1/3     !! centro geom. solo di C O N
c      caard(6  ,2)=sum2/3    
c      caard(6  ,3)=sum3/3    
c      do l=1,6
c        do k=1,3
c      write(*,*) '      data cooadj(',23,',', l,',',k,')/',caard(l,k),
c     >'/'
c
c        enddo
c      enddo
cc      write(*,*) (caard(6,k),k=1,3)
c      stop

c.....Troviamo i gruppi ammidici rigidi
      do m=1,6
        do k=1,3
          caard(m,k)=cooadj(23,m,k)
        enddo
      enddo

      do i=1,n-1
c..ruoto il pdb amide
        do k=1,3
c.........secondo me qui il problema e' che sono planari! e se invece di CA ci mettessi un vettore perpendicolare? a C-O-N
          
          pm(1,k)=caard(1,k)-caard(2,k) !CA-C !! matrice dei tre vettori iniziali
          pm(2,k)=caard(3,k)-caard(2,k) !O-C
          pm(3,k)=caard(4,k)-caard(2,k) !N-C
          qm(1,k)=rca(i,k)-rc(i,k) !CA-C !! matrice dei tre vettori ruotati
          qm(2,k)=ro(i,k)-rc(i,k) !O-C
          qm(3,k)=rn(i+1,k)-rc(i,k) !N-C
c
          p1(k)=pm(1,k)
          p2(k)=pm(2,k)
          p3(k)=pm(3,k)
          q1(k)=qm(1,k)
          q2(k)=qm(2,k)
          q3(k)=qm(3,k)
        enddo
        call b3finder(p1,p2,1.d0,pi/2,pi/2,p3)
        call b3finder(q1,q2,1.d0,pi/2,pi/2,q3)
      do k=1,3
        pm(1,k)=p1(k)
        pm(2,k)=p2(k)
        pm(3,k)=p3(k)
        qm(1,k)=q1(k)
        qm(2,k)=q2(k)
        qm(3,k)=q3(k)
      enddo
        call invm3(pm,pminv) !! matrice inversa di pm
          do ii=1,3
            do jj=1,3
              dum1=0.d0
              do k=1,3
                dum1=dum1+pminv(ii,k)*qm(k,jj)
              enddo
              rot2(jj,ii)=dum1 !! matrice trasposta
            enddo
          enddo
        call taminv(a,b,g,rot2)
        alphaaa(i,2)=a
        betaaa(i,2)=b
        gammaaa(i,2)=g
      call mat(-g,-b,-a,rot2)
        xt=rc(i,1)   ! C nuovo
        yt=rc(i,2)
        zt=rc(i,3)
        x0=caard(2,1)  ! C del pdb
        y0=caard(2,2)
        z0=caard(2,3)
        j=6
        x1=caard(j,1)-x0
        y1=caard(j,2)-y0
        z1=caard(j,3)-z0
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
        centreaa(i,1,2)=rx
        centreaa(i,2,2)=ry
        centreaa(i,3,2)=rz
c.31      write(31,'(a)')
c.31     >' Amide ................................:'
c.31      write(31,2208) 
c.31     >' ux uy uz Amide (Angs.) ...............:'
c.31     >,(centreaa(i,k,2),k=1,3),i
c.31      write(31,2208) 
c.31     >' Alpha Beta Gamma Amide (deg) .........:',alphaaa(i,2)*conv
c.31     >,betaaa(i,2)*conv,gammaaa(i,2)*conv,i
      enddo

c      close(1)



c############################
c      open(unit=1,file='/sas/AA/poly_gly_test.pdb')
c.....ruoto i gruppi laterali, cambia il centro
      do i=1,n 
c        chi(i)=0.d0
        ch3=amm(iseq2(i))
        gru(i)=ch3
c        if (iruoto.eq.1) then
c          if (ch3.ne.'GLY'.and.ch3.ne.'PRO'.and.ch3.ne.'ALA' ) then
c            chi(i)=2*pi*rand(0)  !! angolo di rotazione intorno CA-CB, ovvio che ruota anche il c.geom.
c          endif
c        endif
      enddo
      nato=0 !numero di atomi
      nato=nato+1
c.write1      write(1,1001) 'ATOM  ',nato,' N  ',gru(n1),1,
c.write1     >                  (rn(n1,k),k=1,3)

      inn=0
      do i=n1,n2 
      inn=inn+1

c.....memorizzo gli angoli
      dihed(inn,1)=phi_r(i)
      dihed(inn,2)=psi_r(i)
      dihed(inn,3)=chi00(i)
      do k=1,3
        i4=i4lnk(i,k)
        if (i4.ne.0) dihed(inn,3+k)=gpar(i4)
      enddo

      do ia=1,2  !ia=1 laterale, ia=2 gruppo amidico
        chi(i)=chi00(i)
        if (ia.eq.1) then
          ch3=gru(i)
          chi0=chi(i)
          ii=iseq2(i)
          m2=natoadj(ii)-1
        else if (i.ne.n2) then
          chi0=0.d0
          ii=23
          m2=natoadj(ii)-1
        else
          goto 433
        endif
        mm=0
        do 3131 m=1,m2
        x=cooadj(ii,m,1)
        y=cooadj(ii,m,2)
        z=cooadj(ii,m,3)
        ch4=atypei(ijadj(ii,m,2))//' '
        if (ia.eq.1.and.(m.eq.1.or.m.eq.3.or.m.eq.4)) goto 3131 
        if (ia.eq.2.and.(m.eq.1.or.m.eq.5)) goto 3131 
        mm=mm+1
        caard(mm,1)=x
        caard(mm,2)=y
        caard(mm,3)=z
        atom4(mm)=ch4
 3131   continue
        sum1=cooadj(ii,m2+1,1)
        sum2=cooadj(ii,m2+1,2)
        sum3=cooadj(ii,m2+1,3)
c        write(72,*) ia,sum1,sum2,sum3,ii,m2+1
        call mat(
     >        -gammaaa(i,ia)-chi0
     >       ,-betaaa(i,ia)
     >       ,-alphaaa(i,ia),rot2)

        if (ia.eq.1) then !!! ritrovo il centro geom.
        xt=rca(i,1)   ! CA nuovo
        yt=rca(i,2)
        zt=rca(i,3)
        x0=caard(1,1)  ! CA del pdb
        y0=caard(1,2)
        z0=caard(1,3)
        x1=sum1-x0
        y1=sum2-y0
        z1=sum3-z0
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
        centreaa(i,1,ia)=rx
        centreaa(i,2,ia)=ry
        centreaa(i,3,ia)=rz
        call cpolar(rx,
     >              ry,
     >              rz,
     >              umsaa(istate,iilink,iconf,inn,1),
     >              ausaa(istate,iilink,iconf,inn,1),
     >              dum1)
        cbuaa(istate,iilink,iconf,inn,1)=dcos(dum1)
        aasaa(istate,iilink,iconf,inn,1)=alphaaa(i,1)
        cbaaa(istate,iilink,iconf,inn,1)=dcos(betaaa(i,1))
        gasaa(istate,iilink,iconf,inn,1)=gammaaa(i,1)
c.31      write(31,'(a)')
c.31     >' AA ...................................:'//ch3
c.31      write(31,2208) 
c.31     >' ux uy uz AA (Angs.) ..................:'
c.31     >,(centreaa(i,k,1),k=1,3),i
c.31      write(31,2208) 
c.31     >' Alpha Beta Gamma AA (deg) ............:',alphaaa(i,1)*conv
c.31     >,betaaa(i,1)*conv,gammaaa(i,1)*conv,i
        else if (i.ne.n2) then
        call cpolar(centreaa(i,1,2),
     >              centreaa(i,2,2),
     >              centreaa(i,3,2),
     >              umsaa(istate,iilink,iconf,inn,2),
     >              ausaa(istate,iilink,iconf,inn,2),
     >              dum1)
        cbuaa(istate,iilink,iconf,inn,2)=dcos(dum1)
        aasaa(istate,iilink,iconf,inn,2)=alphaaa(i,2)
        cbaaa(istate,iilink,iconf,inn,2)=dcos(betaaa(i,2))
        gasaa(istate,iilink,iconf,inn,2)=gammaaa(i,2)
        endif !!! fine nuovo

        xt=centreaa(i,1,ia)
        yt=centreaa(i,2,ia)
        zt=centreaa(i,3,ia)

        do j=1,mm
          x1=caard(j,1)-sum1
          y1=caard(j,2)-sum2
          z1=caard(j,3)-sum3
          call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
          caard(j,1)=rx
          caard(j,2)=ry
          caard(j,3)=rz
          nato=nato+1
          ch3=gru(i)
          i7=i-n1+1
          if (ia.eq.2.and.j.eq.3) ch3=gru(i+1)
          if (ia.eq.2.and.j.eq.3) i7=i+1-n1+1
c          if (ia.eq.2) then
c.write1          write(1,1001) 'ATOM  ',nato,atom4(j),ch3,i7,
c.write1     >                  (caard(j,k),k=1,3)
c          endif
        enddo




      enddo !ia
      enddo !i
 433  nato=nato+1
c.write1      write(1,1001) 'ATOM  ',nato,' C  ',gru(n2),n2-n1+1,
c.write1     >                  (rc(n2,k),k=1,3)
      nato=nato+1
c.write1      write(1,1001) 'ATOM  ',nato,' O  ',gru(n2),n2-n1+1,
c.write1     >                  (ro(n2,k),k=1,3)
c.....memorizzo i terminali
      do k=1,3
        terminals(istate,iilink,iconf,1,k)=rn(n1,k)
        terminals(istate,iilink,iconf,2,k)=rc(n2,k)
        terminals(istate,iilink,iconf,3,k)=ro(n2,k)
      enddo
      if (i_c.eq.1) then
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' N  ','GLY',
c.write1     >'B',n2-n1+1+1,(rn(1,k),k=1,3)
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' CA ','GLY',
c.write1     >'B',n2-n1+1+1,(rca(1,k),k=1,3)
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' C  ','GLY',
c.write1     >'B',n2-n1+1+1,(rc(1,k),k=1,3)
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' O  ','GLY',
c.write1     >'B',n2-n1+1+1,(ro(1,k),k=1,3)
      endif
      if (i_n.eq.1) then
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' N  ','GLY',
c.write1     >'B',n2-n1+1+2,(rn(n+2,k),k=1,3)
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' CA ','GLY',
c.write1     >'B',n2-n1+1+2,(rca(n+2,k),k=1,3)
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' C  ','GLY',
c.write1     >'B',n2-n1+1+2,(rc(n+2,k),k=1,3)
        nato=nato+1
c.write1        write(1,1011) 'ATOM  ',nato,' O  ','GLY',
c.write1     >'B',n2-n1+1+2,(ro(n+2,k),k=1,3)
      endif
c      close(1)

      return


 1003   format(6x,5x,1x,a4,4x,10x,3(f8.3))
2208  format(a40,3(g12.5,1x),i4)
 1001 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
 1011 format(a6,i5,1x,a4,1x,a3,1x,a1,i4,4x,3(f8.3))
      
      end


      subroutine linker(npar,x,f)

      include 'quafit2014.cmn'

      external minv
      dimension x(mxaapar),xl(3),xh(3),gg(3)

c      do k=1,npar
c      write(37,*) k,x(k)
c      enddo
      f=0.d0
      fout=0.d0
      i4=0
      

c######################################################################

2208  format(a40,3(g12.5,1x),i4)

!       cosi' sono tutte poli-alanine random

c      n=10 !! temp
c      write(*,2208) 
c     >' Number of read amino acids ...........:',float(n)

c.....genera psi,phi in accordo alla strutt. secondaria
      do i=1,n+1
        if (i_c.eq.1.and.i_n.eq.1) then
          if (i.ne.1) then
            if (iseq2(i).eq.15) then !proline
            phi_r(i)=(-75.d0/conv)!(-pi/3)!0.
            else if (dihed(i-1,3+1).ne.0.d0) then
            i4=i4+1
            phi_r(i)=xscale(x(i4),dihed(i-1,1+6),dihed(i-1,1+9))
c            write(*,*) ' ale phi',i4,i,x(i4),
c     >dihed(i-1,1+6),dihed(i-1,1+9),phi_r(i),
c     >'       phi_r(i)=xscale(x(i4),dihed(i-1,1+6),dihed(i-1,1+9))'
            else
              phi_r(i)=dihed(i-1,1)
c            write(*,*) ' ale phi  fix',i,phi_r(i)
            endif
          endif
c          if (i.ne.1.and.i.ne.n) then
          if (i.ne.1) then
            if (dihed(i-1,3+2).ne.0.d0) then
            i4=i4+1
c            write(*,*) ' ale ',i4,
c     >'       psi_r(i)=xscale(x(i4),dihed(i-1,2+6),dihed(i-1,2+9))'
            psi_r(i)=xscale(x(i4),dihed(i-1,2+6),dihed(i-1,2+9))
            else
              psi_r(i)=dihed(i-1,2)
            endif
          endif
        else if (i_c.eq.1) then
          if (i.ne.1) then
            if (iseq2(i).eq.15) then !proline
            phi_r(i)=(-75.d0/conv)!(-pi/3)!0.
            else if (dihed(i-1,3+1).ne.0.d0) then
            i4=i4+1
c            write(*,*) ' ale ',i4
c     >'       phi_r(i)=xscale(x(i4),dihed(i-1,1+6),dihed(i-1,1+9))'
            phi_r(i)=xscale(x(i4),dihed(i-1,1+6),dihed(i-1,1+9))
            else
              phi_r(i)=dihed(i-1,1)
            endif
            if (dihed(i-1,3+2).ne.0.d0) then
            i4=i4+1
c            write(*,*) ' ale ',i4
c     >'       psi_r(i)=xscale(x(i4),dihed(i-1,2+6),dihed(i-1,2+9))'
            psi_r(i)=xscale(x(i4),dihed(i-1,2+6),dihed(i-1,2+9))
            else
              psi_r(i)=dihed(i-1,2)
            endif
          endif
        else if (i_n.eq.1) then
          if (i.ne.1) then
            if (iseq2(i).eq.15) then !proline
            phi_r(i)=(-75.d0/conv)!(-pi/3)!0.
            else if (dihed(i,3+1).ne.0.d0) then
            i4=i4+1
c            write(*,*) ' ale ',i4
c     >'       phi_r(i)=xscale(x(i4),dihed(i-1,1+6),dihed(i-1,1+9))'
            phi_r(i)=xscale(x(i4),dihed(i,1+6),dihed(i,1+9))
            else
              phi_r(i)=dihed(i,1)
            endif
            if (dihed(i,3+2).ne.0.d0) then
            i4=i4+1
c            write(*,*) ' ale ',i4
c     >'       psi_r(i)=xscale(x(i4),dihed(i-1,2+6),dihed(i-1,2+9))'
            psi_r(i)=xscale(x(i4),dihed(i,2+6),dihed(i,2+9))
            else
              psi_r(i)=dihed(i,2)
            endif
          endif
        else
          if (iseq2(i).eq.15) then !proline
          phi_r(i)=(-75.d0/conv)!(-pi/3)!0.
          else if (dihed(i,3+1).ne.0.d0) then
          i4=i4+1
c            write(*,*) ' ale ',i4,
c     >'     phi_r(i)=xscale(x(i4),dihed(i,1+6),dihed(i,1+9))'
          phi_r(i)=xscale(x(i4),dihed(i,1+6),dihed(i,1+9))
          else
            phi_r(i)=dihed(i,1)
          endif
          i4=i4+1
c            write(*,*) ' ale ',i4,
c     >'     if (dihed(i,3+2).ne.0.d0) then'
          if (dihed(i,3+2).ne.0.d0) then
          psi_r(i)=xscale(x(i4),dihed(i,2+6),dihed(i,2+9))
          else
          psi_r(i)=dihed(i,2)
          endif
        endif
      enddo
c      write(*,*) i4,npar
c      do i=1,n
c        write(*,*) i,phi_r(i),psi_r(i)
c      enddo
c      stop

      do i=1,n 
        if (i_c.eq.1) then
          if (i.eq.1) then
            chi00(i)=0.d0
          else
            chi00(i)=dihed(i-1,3)
          endif
        else
          chi00(i)=dihed(i,3)
        endif
        ch3=amm(iseq2(i))
        gru(i)=ch3
c        write(*,*) ' A A A ',i,iruoto,ch3
        if (iruoto.eq.1) then
          if (ch3.ne.'GLY'.and.ch3.ne.'PRO'.and.ch3.ne.'ALA' ) then
            if (i_c.eq.1.and.i_n.eq.1) then
              if (i.eq.1) then
                i4=i4+1
              else if (dihed(i-1,3+3).ne.0.d0) then
                i4=i4+1
                chi00(i)=xscale(x(i4),dihed(i-1,3+6),dihed(i-1,3+9))
              else
                chi(i)=chi00(i)
              endif
            else if (i_c.eq.1) then
              if (dihed(i-1,3+3).ne.0.d0) then
                i4=i4+1
                chi00(i)=xscale(x(i4),dihed(i-1,3+6),dihed(i-1,3+9))
              else
                chi(i)=chi00(i)
              endif
            else if (i_n.eq.1) then
              if (dihed(i,3+3).ne.0.d0) then
                i4=i4+1
              chi00(i)=xscale(x(i4),dihed(i,3+6),dihed(i,3+9))
              else
                chi(i)=chi00(i)
              endif
            else
              if (dihed(i,3+3).ne.0.d0) then
                i4=i4+1
                chi00(i)=xscale(x(i4),dihed(i,3+6),dihed(i,3+9))
              else
                chi(i)=chi00(i)
              endif
            endif
          endif
        endif
      enddo
      if (i4.ne.npar) then
        write(*,*) 'i_c i_n',i_c,i_n
        write(*,*) 'error i4=',i4,' npar=',npar
        stop
      endif
      


c##################
c      write(*,*) '  dafsd ',n
      if (i_n.eq.1.and.i_c.eq.0) then
c.....  Trova la configurazione del backbone, da C-term a N-term. Serve quando conosci solo la posizione del Cterm. Catena a ritroso
        do i=n-1,1,-1
c.....  ..rn(i,k) -> psi(i-1)
          do k=1,3
            p1(k)=rc(i+1,k)
            p2(k)=rca(i+1,k)
            p3(k)=rn(i+1,k)
          enddo
c          call bonds(rcn,acacn,pi/2,p3,p2,p1,p4) !! beta sheet
c          call bonds(rcn,acacn,-pi/2,p3,p2,p1,p4) !! alpha elica
          dum2=-phi_r(i+1)
c          write(37,*)' psi', i,dum2
          call bonds(rcn,acnca,dum2,p3,p2,p1,p4) !! random coil
c          write(*,*) ' 3 ci sono'
          do k=1,3
            rc(i,k)=p4(k)
          enddo
c.....  ..rca(i,k) -> omega(i)
          do k=1,3
            p3(k)=rc(i,k)
            p2(k)=rn(i+1,k)
            p1(k)=rca(i+1,k)
          enddo
          call bonds(rcca,acacn,pi,p3,p2,p1,p4)  !! omega_i=pi mantiene la planarita' del gruppo ammidico
c          call bonds(rnca,acnca,0.d0,p3,p2,p1,p4)  !! omega_i=pi mantiene la planarita' del gruppo ammidico
c          write(*,*) ' 4 ci sono'
          do k=1,3
            rca(i,k)=p4(k)
          enddo
c.....  ..rc(i,k) -> phi(i)
          do k=1,3
            p3(k)=rca(i,k)
            p2(k)=rc(i,k)
            p1(k)=rn(i+1,k)
          enddo
c          call bonds(rcca,ancac,-pi/2,p3,p2,p1,p4) !!! sia alpha helix che beta sheet
          dum2=-psi_r(i)

c          if (i.eq.n) write(*,*) 'phi',-phi_r(i),(rc(i,kk),kk=1,3)
c       >,p1,p2,p3

c          write(37,*)' phi', i,dum2
          call bonds(rnca,ancac,dum2,p3,p2,p1,p4) !! random coil
c          write(*,*) ' 5 ci sono'
          do k=1,3
            rn(i,k)=p4(k)
          enddo
ccc          if (i.ge.3) then
c.....  ..ro(i-1,k)
          do k=1,3
            p3(k)=rc(i,k)
            p2(k)=rn(i+1,k)
            p1(k)=rca(i+1,k)
          enddo
          call bonds(rco,anco,0.d0,p3,p2,p1,p4)
          do k=1,3
            ro(i,k)=p4(k)
          enddo
        enddo
      else
c.....  Trova la configurazione del backbone. Da N a C-term
        do i=2,n+1 !piu' uno perche' arriva anche al psi dell'ultimo AA
c.....  ..rn(i,k) -> psi(i-1)
          do k=1,3
            p3(k)=rc(i-1,k)
            p2(k)=rca(i-1,k)
            p1(k)=rn(i-1,k)
          enddo
c          call bonds(rcn,acacn,pi/2,p3,p2,p1,p4) !! beta sheet
c          call bonds(rcn,acacn,-pi/2,p3,p2,p1,p4) !! alpha elica
          dum2=-psi_r(i-1)
c          write(37,*)' psi', i,dum2
          call bonds(rcn,acacn,dum2,p3,p2,p1,p4) !! random coil
c          write(*,*) ' 3 ci sono'
          do k=1,3
            rn(i,k)=p4(k)
          enddo
c          if (i.eq.n) write(*,*) 'psi',-psi_r(i-1),(rn(i,kk),kk=1,3)
c       >,p1,p2,p3
c.....  ..rca(i,k) -> omega(i)
          do k=1,3
            p3(k)=rn(i,k)
            p2(k)=rc(i-1,k)
            p1(k)=rca(i-1,k)
          enddo
          call bonds(rnca,acnca,pi,p3,p2,p1,p4)  !! omega_i=pi mantiene la planarita' del gruppo ammidico
c          call bonds(rnca,acnca,0.d0,p3,p2,p1,p4)  !! omega_i=pi mantiene la planarita' del gruppo ammidico
c          write(*,*) ' 4 ci sono'
          do k=1,3
            rca(i,k)=p4(k)
          enddo
c.....  ..rc(i,k) -> phi(i)
          do k=1,3
            p3(k)=rca(i,k)
            p2(k)=rn(i,k)
            p1(k)=rc(i-1,k)
          enddo
c          call bonds(rcca,ancac,-pi/2,p3,p2,p1,p4) !!! sia alpha helix che beta sheet
          dum2=-phi_r(i)

c          if (i.eq.n) write(*,*) 'phi',-phi_r(i),(rc(i,kk),kk=1,3)
c       >,p1,p2,p3

c          write(37,*)' phi', i,dum2
          call bonds(rcca,ancac,dum2,p3,p2,p1,p4) !! random coil
c          write(*,*) ' 5 ci sono'
          do k=1,3
            rc(i,k)=p4(k)
          enddo
          if (i.ge.3) then
c.....  ..ro(i-1,k)
          do k=1,3
            p3(k)=rc(i-1,k)
            p2(k)=rn(i,k)
            p1(k)=rca(i,k)
          enddo
          call bonds(rco,anco,0.d0,p3,p2,p1,p4)
          do k=1,3
            ro(i-1,k)=p4(k)
          enddo
          endif

c          dum2=-psi_r(i)
c          do k=1,3
c            p3(k)=rc(i,k)
c            p2(k)=rca(i,k)
c            p1(k)=rn(i,k)
c          enddo
cc          write(*,*) acaco*conv,dum2*conv
c          call bonds(rco,acaco,dum2,p3,p2,p1,p4)
c          do k=1,3
c            ro(i,k)=p4(k)
c          enddo

        enddo
c        do k=1,3
c        write(*,*) rn(1,k)
c        write(*,*)         rca(1,k)
c        write(*,*)         rc(1,k)
c        write(*,*)         ro(1,k)
c        enddo
c        stop
      endif
c        write(*,*) ' 6 ci sono'

c.....Ora, se e' il caso, si minimizza
      if (i_c.eq.1.and.i_n.eq.1) then 
c        write(*,*) ' 7 ci sono'
      dum1=0.d0
      do k=1,3
        dum1=dum1
     >+(rN(n,k)-rN(n+2,k))**2
     >+(rCA(n,k)-rCA(n+2,k))**2
     >+(rC(n,k)-rC(n+2,k))**2
     >+(rO(n,k)-rO(n+2,k))**2
c        write(*,*) 'FF',n,rN(n,k),rN(n+2,k)
c     >,rCA(n,k),rCA(n+2,k)
c     >,rC(n,k),rC(n+2,k)
c     >,rO(n,k),rO(n+2,k)

c       write(*,*) rn(n,k),rn(n+2,k),dum1
      enddo
      dum1=(dum1/4)
      f=dum1
      endif

c.....Aggiunge i gruppi laterali
      iruoto=1 !ruota i gruppi laterali
c.....=1 ruota casualmente le catene laterali
      nato=0 !numero di atomi
 1001 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
c      open(unit=1,file='/sas/AA/poly_gly.pdb')
c Rosso 2/1009 - 1/2010
c per GLY va bene cosi', per gli altri trova anche dove va a finire CB perche' l'O e' ruotato.
c con i tre vettori da ruotare sono N-CA C-CA e CB-CA, poi ruota il pdb degli angoli di eulero trovati e trasla il suo CA 
c sulla nuova posizione di CA. Puoi anche trovare il c.geom. dell'AA , ruotarlo e traslarlo della stessa q.ta'.
c a quel punto da una qualunque struttura primaria da una sequenza phi,psi trovi u_i e omega_i di tutti gli A.A


      do i=1,n
        ch3=gru(i)
        ch3='ALA'
c      open(unit=1,file='/sas/AA/twenty.dat')
c      do i=1,20
c      read(1,'(a3)') ch3

c        ch3='GLY'
        if (ch3.ne.'GLY') then
        ii=iseq2(i)
        ii=1
c        write(*,*) ' ii',ii
        m=natoadj(ii)

        do k=1,m
          caard(k,1)=cooadj(ii,k,1)
          caard(k,2)=cooadj(ii,k,2)
          caard(k,3)=cooadj(ii,k,3)
        enddo
c        goto 2525
c
c        m=0
c        sum1=0.d0 
c        sum2=0.d0
c        sum3=0.d0
 1003   format(6x,5x,1x,a4,4x,10x,3(f8.3))
c         open(unit=3,file='/sas/AA/adj/'//ch3//'.pdb')!!dopo
cc       open(unit=3,file='/sas/AA/'//ch3//'.pdb')
c 30     read(3,1003,end=40) ch4,x,y,z
c        m=m+1 !+1 perche' l'ultimo e' il c.geom esluso l'N, il C e l'O, che fanno parte del gruppo ammidico
c        caard(m,1)=x
c        caard(m,2)=y
c        caard(m,3)=z
c        atom4(m)=ch4
c        if (m.ne.1.and.m.ne.3.and.m.ne.4) then !(N C e O)
c          sum1=sum1+x
c          sum2=sum2+y
c          sum3=sum3+z
c        endif
c        goto 30
c 40     close(3)
c        caard(m+1,1)=sum1/(m-3) !! centro geom. dell'AA senza (N C e O
c        caard(m+1,2)=sum2/(m-3)
c        caard(m+1,3)=sum3/(m-3)
c        write(*,'(a)') 'c '//ch3
c        write(*,'(6x,9(a,i2))') 'natoadj(',i,')=',m
c        do k=1,m
c        write(*,'(6x,2(a,i2),a,f12.5)') 
c     >'cooadj(',i,',',k,',1)=',caard(k,1)
c        write(*,'(6x,2(a,i2),a,f12.5)') 
c     >'cooadj(',i,',',k,',2)=',caard(k,2)
c        write(*,'(6x,2(a,i2),a,f12.5)') 
c     >'cooadj(',i,',',k,',3)=',caard(k,3)
c        enddo

c.......Calcolo di rcbca,accacb,phi che servono per trovare CB-CA
 2525   dum1=0.d0
        dum2=0.d0
        do k=1,3
          p1(k)=caard(5,k)-caard(2,k) !CB-CA
          p2(k)=caard(1,k)-caard(2,k) !N-CA
          p3(k)=caard(3,k)-caard(2,k) !c-CA
          dum1=dum1+p1(k)*p2(k)
          dum2=dum2+p1(k)*p3(k)
        enddo

c.......da qui cancellerai
c        call cpolar(p1(1),p1(2),p1(3),rcbca,a,b)
c        call cpolar(p2(1),p2(2),p2(3),rnca,a,b)
c        call cpolar(p3(1),p3(2),p3(3),rcca,a,b)
c        ancacb=dacos(dum1/rnca/rcbca)
c        accacb=dacos(-dum2/rcca/rcbca)
c        write(*,*) 180-accacb*conv,ancacb*conv
c        stop
c      do k=1,3
c        bbb1(k)=caard(2,k)
c        bbb2(k)=caard(1,k)
c        bbb3(k)=bbb2(k)+p3(k)
c        bb4(k)=bbb3(k)+p1(k)
c      enddo
c      call cpolar(p3(1),p3(2),p3(3),bb2,a,b)
c      call pv(p2,p3,v12)
c      call pv(p3,p1,v23)
c      do k=1,3
c        bbb1s(k)=bb2*p2(k)
c      enddo
c      call ps(bbb1s,v23,x)
c      call ps(v12,v23,y)
c      if (x.eq.0.d0.and.y.eq.0.d0) then
c        phi=0.d0
c      else
c        phi = -atan2(x,y)
c      endif
c      write(*,*) phi,rcbca,pi-accacb

c       write(*,*) ' rcbca 0',rcbca
c      call phidihed(p2,p3,p1,rcbca,accacb,phicbcaxx)
c       write(*,*) ' rcbca 1',rcbca
c      call b3finder(p2,p3,rcbca,accacb,phicbca,p4)
c      write(*,*) 'CB-CA: ',rcbca,accacb*conv,phicbca*conv
c      write(*,*) p1
c      write(*,*) p4
c       write(*,*) ' akjdhf 3'
c      call phidihed(p1,p3,p2,rnca,ancac,phincaxx)
c      call b3finder(p1,p3,rnca,accacb,,phinca,p4)
c      write(*,*) ' N-CA: ',rnca,ancac*conv,phinca*conv
c      write(*,*) p2
c      write(*,*) p4
c      stop
c      call bonds(rcbca,accacb,phi,bbb3,bbb2,bbb1,p4)
cc.....p4 e bb4 coincidono
cc      write(*,*) 'p4'
cc      write(*,*) p4
cc      write(*,*) 'bb4'
cc      write(*,*) bb4
c      do k=1,3
c        p4(k)=p4(k)-bbb3(k)+bbb1(k)
c      enddo
cc      write(*,*) 'p4'
cc      write(*,*) p4
cc      write(*,*) 'CB'
cc      write(*,*) (caard(5,k),k=1,3)
cc.....fine calcolo
c      
cccc      stop! read(*,*) !pause
cc       enddo
c
c.....trovo il CB ruotato
c       write(*,*) ' akjdhf 4'
        do k=1,3
          p2(k)=rn(i,k)-rca(i,k) !N-CA
          p3(k)=rc(i,k)-rca(i,k) !c-CA
        enddo

c      do k=1,3
c        bbb1(k)=rca(i,k)
c        bbb2(k)=rn(i,k)
c        bbb3(k)=bbb2(k)+p3(k)
c      enddo
c      call bonds(rcbca,accacb,phi,bbb3,bbb2,bbb1,p4)
c      write(*,*) 'p4'
c      write(*,*) p4
c       write(*,*) ' akjdhf 5'
      call b3finder(p2,p3,rcbca,accacb,phicbca,p5)
c       write(*,*) ' rcbca 2',rcbca
c.....p4 e bb4 coincidono
c      write(*,*) 'p5'
c      write(*,*) p5
c      write(*,*) 'bb4'
c      write(*,*) bb4
c       write(*,*) ' rcbca ',rcbca,p1
c       call cpolar(p1(1),p1(2),p1(3),dum7,dum8,dum9)
c       write(*,*) dum7
c       pause
c       write(*,*) ' akjdhf 6'
      do k=1,3
        rcb(i,k)=p5(k)+rca(i,k)
c        write(*,*) i,rcb(i,k),rca(i,k),p5(k)
      enddo
c     stop
c..fine trovo CB ruotato


c..ruoto il pdb
        do k=1,3
          pm(1,k)=caard(5,k)-caard(2,k) !CB-CA !! matrice dei tre vettori iniziali
          pm(2,k)=caard(1,k)-caard(2,k) !N-CA
          pm(3,k)=caard(3,k)-caard(2,k) !c-CA
          qm(1,k)=rcb(i,k)-rca(i,k) !CB-CA !! matrice dei tre vettori ruotati
          qm(2,k)=rn(i,k)-rca(i,k) !N-CA
          qm(3,k)=rc(i,k)-rca(i,k) !c-CA
        enddo
c        write(*,*) i,gru(i)
c        write(*,*) (pm(1,k),';',k=1,3)
c        call cpolar(pm(1,1),pm(1,2),pm(1,3),dum7,dum8,dum9)
c        write(*,*) ' pm(1,k) mod',dum7
c        write(*,*) (pm(2,k),';',k=1,3)
c        call cpolar(pm(2,1),pm(2,2),pm(2,3),dum7,dum8,dum9)
c        write(*,*) ' pm(2,k) mod',dum7
c        write(*,*) (pm(3,k),';',k=1,3)
c        call cpolar(pm(3,1),pm(3,2),pm(3,3),dum7,dum8,dum9)
c        write(*,*) ' pm(3,k) mod',dum7
c        write(*,*) (qm(1,k),';',k=1,3)
c        call cpolar(qm(1,1),qm(1,2),qm(1,3),dum7,dum8,dum9)
c        write(*,*) ' qm(1,k) mod',dum7
c        write(*,*) (qm(2,k),';',k=1,3)
c        call cpolar(qm(2,1),qm(2,2),qm(2,3),dum7,dum8,dum9)
c        write(*,*) ' qm(2,k) mod',dum7
c        write(*,*) (qm(3,k),';',k=1,3)
c        call cpolar(qm(3,1),qm(3,2),qm(3,3),dum7,dum8,dum9)
c        write(*,*) ' qm(3,k) mod',dum7
cc        write(*,*) ' 37 ci sono'
c       write(*,*) ' akjdhf 7'
        call invm3(pm,pminv) !! matrice inversa di pm
cc        write(*,*) ' 38 ci sono'
c        do ii=1,3
c          do jj=1,3
c            dum1=0.d0
c            do k=1,3
c              dum1=dum1+pminv(ii,k)*pm(k,jj)
c            enddo
c            rot2(jj,ii)=dum1 !! matrice trasposta
c          enddo
c        enddo
c      write(*,*) ' prod. matr. inv. '
c      do ii=1,3
c        write(*,'(3(f8.3))') (rot2(ii,jj),jj=1,3)
c      enddo
c      pause

c prodotto pminv per qm
c       write(*,*) ' akjdhf 8'
        do ii=1,3
          do jj=1,3
            dum1=0.d0
            do k=1,3
              dum1=dum1+pminv(ii,k)*qm(k,jj)
            enddo
            rot2(jj,ii)=dum1 !! matrice trasposta
          enddo
        enddo

c.test      write(*,*) ' rot2 direct'
c.test      do ii=1,3
c.test        write(*,'(3(f8.3))') (rot2(ii,jj),jj=1,3)
c.test     enddo

      call taminv(a,b,g,rot2)
      alphaaa(i,1)=a
      betaaa(i,1)=b
      gammaaa(i,1)=g
      call mat(-g,-b,-a,rot)
c      write(*,*) ' a b g a+g  after direct ' ,a,b,g,a+g

c..............................
c      goto 1717   !!!! sembra sia necessaria questa minimizzazione, PERCHE??
c.no_min      do ii=1,3
c.no_min        xx(ii)=0.3
c.no_min      enddo
c.no_min      call simplx(minv,xx,f,3,1.d-20,itc,300)
c.no_min      a=xscale(xx(1),0.d0,pi*2)
c.no_min      b=xscale(xx(2),0.d0,pi)
c.no_min      g=xscale(xx(3),0.d0,pi*2)
c.no_minccc
c.no_min      call mat(-g,-b,-a,rot2)
c.no_min      alphaaa(i,1)=a
c.no_min      betaaa(i,1)=b
c.no_min      gammaaa(i,1)=g
c.no_minc      write(*,*) ' a b g a+g  after simplex ' ,a,b,g,a+g
c.no_mincc 1717 continue
c........................

c.test      write(*,*) ' rot2 simplex'
c.test      do ii=1,3
c.test        write(*,'(3(f8.3))') (rot2(ii,jj),jj=1,3)
c.test      enddo
c.test      write(*,*) ' rot from direct'
c.test      do ii=1,3
c.test        write(*,'(3(f8.3))') (rot(ii,jj),jj=1,3)
c.test      enddo
      do ii=1,3
      do jj=1,3
c        if (abs(rot2(ii,jj)-rot(ii,jj)).gt.tiny) then
c          write(*,'(3(f8.3))') rot2(ii,jj)-rot(ii,jj)
c        endif
c         write(*,*) f
         f=f+10*(rot2(ii,jj)-rot(ii,jj))**2
      enddo
      enddo
c      pause


cc       verifica !! forse distanze e angoli non sono proprio gli stessi
c        do jj=1,3
c        do ii=1,3
c            dum1=0.d0
c            do k=1,3
c              dum1=dum1+rot(ii,k)*pm(jj,k)
c            enddo
cc            write(*,*) qm(jj,ii),dum1
c          enddo
c        enddo
cc        stop

c      write(*,*) a,b,g
      call mat(-g,-b,-a,rot2)
        xt=rca(i,1)   ! CA nuovo
        yt=rca(i,2)
        zt=rca(i,3)
        x0=caard(2,1)  ! CA del pdb
        y0=caard(2,2)
        z0=caard(2,3)
        do j=1,m+1
          x1=caard(j,1)-x0
          y1=caard(j,2)-y0
          z1=caard(j,3)-z0
          call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
          caard(j,1)=rx
          caard(j,2)=ry
          caard(j,3)=rz
        enddo
c.......L'ossigeno va sostituito
        do k=1,3
          caard(4,k)=ro(i,k)
          centreaa(i,k,1)=caard(m+1,k)  !!c.......nuovo c geom.
        enddo        
c      stop
        do j=1,m
          nato=nato+1
c          write(1,1001) 'ATOM  ',nato,atom4(j),ch3,i,(caard(j,k),k=1,3)
        enddo

        else  !!! GLY

        do k=1,3
          centreaa(i,k,1)=rca(i,k)  !!c.......nuovo c geom., coicide con il CA
        enddo        
c.......Gli angoli di Eulero sono nulli, perche' CA e' un united atom
        alphaaa(i,1)=0.d0
        betaaa(i,1)=0.d0
        gammaaa(i,1)=0.d0
c..................................
        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' N  ','GLY',i,(rn(i,k),k=1,3)
        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' CA ','GLY',i,(rca(i,k),k=1,3)
        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' C  ','GLY',i,(rc(i,k),k=1,3)
        nato=nato+1
c        write(1,1001) 'ATOM  ',nato,' O  ','GLY',i,(ro(i,k),k=1,3)
        endif

c      write(*,'(a)')
c     >' AA ...................................:'//ch3
c      write(*,2208) 
c     >' ux uy uz AA (Angs.) ..................:'
c     >,(centreaa(i,k,1),k=1,3),i
c      write(*,2208) 
c     >' Alpha Beta Gamma AA (deg) ............:',alphaaa(i,1)*conv
c     >,betaaa(i,1)*conv,gammaaa(i,1)*conv,i

      enddo
c      close(1)

c      write(*,*) ' 22 ci sono'



c        stop






c.....test fatto in precedenza
c      a=pi/3
c      b=pi/9
c      g=pi*1.2
c      call mat(a,b,g,rot)
cc http://www.springerlink.com/content/0035650714624345/fulltext.pdf
c      call rotrdi(p1(1),p1(2),p1(3),
c     >            q1(1),q1(2),q1(3),0.d0,0.d0,0.d0,rot)
c      call rotrdi(p2(1),p2(2),p2(3),
c     >            q2(1),q2(2),q2(3),0.d0,0.d0,0.d0,rot)
c      call rotrdi(p3(1),p3(2),p3(3),
c     >            q3(1),q3(2),q3(3),0.d0,0.d0,0.d0,rot)
c      do k=1,3
c        pm(1,k)=p1(k)  !! matrice dei tre vettori iniziali
c        pm(2,k)=p2(k)
c        pm(3,k)=p3(k)
c        qm(1,k)=q1(k)  !! matrice dei tre vettori ruotati
c        qm(2,k)=q2(k)
c        qm(3,k)=q3(k)
c      enddo
c      call invm3(pm,pminv) !! matrice inversa di pm
cc prodotto pminv per qm
c      do i=1,3
c        do j=1,3
c          dum1=0.d0
c          do k=1,3
c            dum1=dum1+pminv(i,k)*qm(k,j)
c          enddo
c          rot2(j,i)=dum1 !! matrice trasposta
c        enddo
c      enddo
c      write(*,*) ' rot '
c      write(*,*) rot 
c      write(*,*) ' rot2 '
c      write(*,*) rot2 
c      write(*,*) a,b,g
c      call taminv(a,b,g,rot2)
c      write(*,*) a,b,g
c.....Fine test fatto in precedenza


      !verifica che tra AA con |i-j|>1  (non attaccati) non ci sia overlap di atomi.
c....!per gli attaccati (|i-j|=1) controllo CB con O
c        write(*,*) ' 8 ci sono'

      do i=1,n-1
      do j=i+1,n
        do i1=1,5
          if (i1.eq.1) then
            x1=rn(i,1)
            y1=rn(i,2)
            z1=rn(i,3)
            rvdw1=1.450 !NH2
          else if (i1.eq.2) then
            x1=rca(i,1)
            y1=rca(i,2)
            z1=rca(i,3)
            rvdw1=1.850 !CH2
          else if (i1.eq.3) then
            x1=rc(i,1)
            y1=rc(i,2)
            z1=rc(i,3) 
            rvdw1=1.577 !C
          else if (i1.eq.4) then
            x1=ro(i,1)
            y1=ro(i,2)
            z1=ro(i,3)
            rvdw1=1.300 !O
          else if (i1.eq.5) then
            x1=rcb(i,1)
            y1=rcb(i,2)
            z1=rcb(i,3)
            rvdw1=1.970 !CB CH3
          endif
        do i2=1,5
          if (i2.eq.1) then
            x2=rn(j,1)
            y2=rn(j,2)
            z2=rn(j,3)
            rvdw2=1.450 !NH2
          else if (i2.eq.2) then
            x2=rca(j,1)
            y2=rca(j,2)
            z2=rca(j,3)
            rvdw2=1.850 !CH2
          else if (i2.eq.3) then
            x2=rc(j,1)
            y2=rc(j,2)
            z2=rc(j,3) 
            rvdw2=1.577 !C
          else if (i2.eq.4) then
            x2=ro(j,1)
            y2=ro(j,2)
            z2=ro(j,3)
            rvdw2=1.300 !O
          else if (i1.eq.5) then
            x2=rcb(j,1)
            y2=rcb(j,2)
            z2=rcb(j,3)
            rvdw2=1.970 !CB CH3
          endif
          if ((abs(i-j).eq.1.and.((i1.eq.4.and.i2.eq.5).
     >or.(i1.eq.5.and.i2.eq.4))).or.abs(i-j).gt.1) then
c          write(*,*) j-i,i1,i2
          call cpolar(x1-x2,y1-y2,z1-z2,r1,a,b)
          if (r1.lt.rvdw1+rvdw2) then
            f=f+w_penalty
          endif
          endif
        enddo!i2
        enddo!i1
        enddo!j
        enddo!i


c......ora il secondo test sulle catene laterali....
c      goto 876!salto

c.....Troviamo i gruppi ammidici rigidi, ovvero centri e angoli di Eulero

      do i=1,n-1
c..ruoto il pdb amide
        do k=1,3
c.........secondo me qui il problema e' che sono planari! e se invece di CA ci mettessi un vettore perpendicolare? a C-O-N
          
          pm(1,k)=cooadj(23,1,k)-cooadj(23,2,k) !CA-C !! matrice dei tre vettori iniziali
          pm(2,k)=cooadj(23,3,k)-cooadj(23,2,k) !O-C
          pm(3,k)=cooadj(23,4,k)-cooadj(23,2,k) !N-C
          qm(1,k)=rca(i,k)-rc(i,k) !CA-C !! matrice dei tre vettori ruotati
          qm(2,k)=ro(i,k)-rc(i,k) !O-C
          qm(3,k)=rn(i+1,k)-rc(i,k) !N-C
c
          p1(k)=pm(1,k)
          p2(k)=pm(2,k)
          p3(k)=pm(3,k)
          q1(k)=qm(1,k)
          q2(k)=qm(2,k)
          q3(k)=qm(3,k)
        enddo
        call b3finder(p1,p2,1.d0,pi/2,pi/2,p3)
        call b3finder(q1,q2,1.d0,pi/2,pi/2,q3)
      do k=1,3
        pm(1,k)=p1(k)
        pm(2,k)=p2(k)
        pm(3,k)=p3(k)
        qm(1,k)=q1(k)
        qm(2,k)=q2(k)
        qm(3,k)=q3(k)
      enddo
        call invm3(pm,pminv) !! matrice inversa di pm
          do ii=1,3
            do jj=1,3
              dum1=0.d0
              do k=1,3
                dum1=dum1+pminv(ii,k)*qm(k,jj)
              enddo
              rot2(jj,ii)=dum1 !! matrice trasposta
            enddo
          enddo
        call taminv(a,b,g,rot2)
        alphaaa(i,2)=a
        betaaa(i,2)=b
        gammaaa(i,2)=g
      call mat(-g,-b,-a,rot)
      call mat(-g,-b,-a,rot2)
        xt=rc(i,1)   ! C nuovo
        yt=rc(i,2)
        zt=rc(i,3)
        x0=cooadj(23,2,1)  ! C del pdb
        y0=cooadj(23,2,2)
        z0=cooadj(23,2,3)
        x1=cooadj(23,6,1)-x0
        y1=cooadj(23,6,2)-y0
        z1=cooadj(23,6,3)-z0
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
        centreaa(i,1,2)=rx
        centreaa(i,2,2)=ry
        centreaa(i,3,2)=rz

c      write(*,'(a)')
c     >' Amide ................................:'
c      write(*,2208) 
c     >' ux uy uz Amide (Angs.) ...............:'
c     >,(centreaa(i,k,2),k=1,3),i
c      write(*,2208) 
c     >' Alpha Beta Gamma Amide (deg) .........:',alphaaa(i,2)*conv
c     >,betaaa(i,2)*conv,gammaaa(i,2)*conv,i

      enddo



c.....Trovo i centri degli AA nacked e tutti gli atomi  
      do i=1,n 
        ch3=amm(iseq2(i))
      do ia=1,2  !ia=1 laterale, ia=2 gruppo amidico
        chi(i)=chi00(i)
c        write(*,*) ia,chi(i)
        if (ia.eq.1) then
        chi0=chi(i)
        ii=iseq2(i)
        m2=natoadj(ii)
        else if (i.ne.n) then
          chi0=0.d0
          ii=23
          m2=natoadj(ii)
        else
          goto 434
        endif
        sum1=cooadj(ii,m2,1)!! c.geom
        sum2=cooadj(ii,m2,2)
        sum3=cooadj(ii,m2,3)
        m2=m2-1
        mm=0
        do 3232 m=1,m2
          x3=cooadj(ii,m,1)
          y3=cooadj(ii,m,2)
          z3=cooadj(ii,m,3)
        if (ia.eq.1.and.(m.eq.1.or.m.eq.3.or.m.eq.4)) goto 3232 
        if (ia.eq.2.and.(m.eq.1.or.m.eq.5)) goto 3232
          mm=mm+1
          ijnac2(i,mm,ia)=ijadj(ii,m,1) !! servira' per il rexc
          caard(mm,1)=x3
          caard(mm,2)=y3
          caard(mm,3)=z3
 3232   continue
        call mat(
     >        -gammaaa(i,ia)-chi0  !!! hai anche ruotato di chi
     >       ,-betaaa(i,ia)
     >       ,-alphaaa(i,ia),rot2)
        if (ia.eq.1) then !!! ritrovo il centro geom.
        xt=rca(i,1)   ! CA nuovo
        yt=rca(i,2)
        zt=rca(i,3)
        x0=cooadj(ii,2,1)  ! CA del pdb
        y0=cooadj(ii,2,2)
        z0=cooadj(ii,2,3)
        x1=sum1-x0
        y1=sum2-y0
        z1=sum3-z0
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
        centreaa(i,1,ia)=rx
        centreaa(i,2,ia)=ry
        centreaa(i,3,ia)=rz



c      write(*,'(a)')
c     >' AA ...................................:'//ch3
c      write(*,2208) 
c     >' ux uy uz AA (Angs.) ..................:'
c     >,(centreaa(i,k,1),k=1,3),i
c      write(*,2208) 
c     >' Alpha Beta Gamma AA (deg) ............:',alphaaa(i,1)*conv
c     >,betaaa(i,1)*conv,gammaaa(i,1)*conv,i



        endif !!! fine nuovo
        xt=centreaa(i,1,ia)
        yt=centreaa(i,2,ia)
        zt=centreaa(i,3,ia)
        do j=1,mm
          x1=caard(j,1)-sum1
          y1=caard(j,2)-sum2
          z1=caard(j,3)-sum3
          call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
          coonac2(i,j,ia,1)=rx !coordinate ruotate dell'AA i
          coonac2(i,j,ia,2)=ry
          coonac2(i,j,ia,3)=rz
        enddo
        natonac2(i,ia)=mm !! atomi naked dell'AA i
      enddo!ia
      enddo!i
 434  continue


      reduct=0.9!1.!0.8
c.....verifico distanze tra i centri
      do i=1,n-1
      do iai=1,2  !ia=1 laterale, ia=2 gruppo amidico
        x1=centreaa(i,1,iai)
        y1=centreaa(i,2,iai)
        z1=centreaa(i,3,iai)
        ii=iseq2(i)*(2-iai)+(iai-1)*23 !e' 23 se iai=2, altrimenti iseq2(i)
        ri=rmadj(ii)
        naci=natonac2(i,iai)
        do j=i+1,n
          do iaj=1,2  !ia=1 laterale, ia=2 gruppo amidico
          if (iaj.eq.2.and.j.eq.n) goto 67 !salta
          x2=centreaa(j,1,iaj)
          y2=centreaa(j,2,iaj)
          z2=centreaa(j,3,iaj)
          jj=iseq2(j)*(2-iaj)+(iaj-1)*23 !e' 23 se iaj=2, altrimenti iseq2(j)
          rj=rmadj(jj)
          call cpolar(x1-x2,y1-y2,z1-z2,r1,a,b)
cc          r1=0.1! test su tutto
          if (r1.lt.ri+rj) then
c            write(*,*) i,j,r1
c...........ora il test tra gli atomi nacked
            nacj=natonac2(j,iaj)
            do ni=1,naci
              xi=coonac2(i,ni,iai,1)
              yi=coonac2(i,ni,iai,2)
              zi=coonac2(i,ni,iai,3)
              rii=rexc(ijnac2(i,ni,iai))*reduct
              do nj=1,nacj
                xj=coonac2(j,nj,iaj,1)
                yj=coonac2(j,nj,iaj,2)
                zj=coonac2(j,nj,iaj,3)
                if (abs(i-j).eq.1.and.iai.eq.2.and.iaj.
     >eq.1.and.ni.eq.3.and.nj.eq.1) then !!!  N e CA dello stesso AA
c                  write(*,*) !i,j,iai,iaj,ni,nj,riijj,rii,rjj
c     >atypej(ijnac2(i,ni,iai))//' ',atypej(ijnac2(j,nj,iaj))//' ' 
                  goto 1212
                endif
                if (abs(i-j).eq.1.and.iai.eq.2.and.iaj.
     >eq.1.and.ni.eq.3.and.nj.eq.2) then !!!  O e CA dello stesso AA
c                  write(*,*) !i,j,iai,iaj,ni,nj,riijj,rii,rjj
c     >atypej(ijnac2(i,ni,iai))//' ',atypej(ijnac2(j,nj,iaj))//' '
                  goto 1212
                endif
                if (abs(i-j).eq.1.and.iai.eq.2.and.iaj.
     >eq.1.and.ni.eq.1.and.nj.eq.1) then !!!  C e CA dello stesso AA
c                  write(*,*) !i,j,iai,iaj,ni,nj,riijj,rii,rjj
c     >atypej(ijnac2(i,ni,iai))//' ',atypej(ijnac2(j,nj,iaj))//' '
                  goto 1212
                endif
                rjj=rexc(ijnac2(j,nj,iaj))*reduct
                call cpolar(xi-xj,yi-yj,zi-zj,riijj,a,b)
                if (riijj.lt.rii+rjj) then
c                  write(*,*) i,j,iai,iaj,ni,nj,riijj,rii,rjj
c     >,atypej(ijnac2(i,ni,iai))//' ',atypej(ijnac2(j,nj,iaj))//' '
c                  write(*,*) xi,yi,zi
c                  write(*,*) xj,yj,zj
                  f=f+w_penalty*0.01
                endif
 1212           continue
              enddo
            enddo
          endif!test dist centro-centro
        enddo !iaj
 67     continue
        enddo !j  
      enddo !iai  
      enddo !i  

c......fine ora



 876  ic=ic+1
      return
      if (ncicl*npar.ne.0) then
        if (mod(ic,ncicl*npar).ne.0) return
      endif
      do i=1,n
        write(*,5200) 'phi',conv*phi_r(i),i
        write(*,5200) 'psi',conv*psi_r(i),i
        write(*,5200) 'chi',conv*chi00(i),i
      enddo
      write(*,5100) f
      write(*,*) '########################'
      return
5100  format(' Chi square = ',1pg15.7)
5200  format(' ',a,' = ',1(1pg15.7,1x),1x,'-',1x,i2)

      return

c######################################################################
      end


      subroutine minv(npar,x,f)

      implicit real*8 (a-h,o-z)
      parameter(pi     = 3.14159265358979323846264338328d0,max=5000)
      parameter(conv   = 57.2957795130823208767981548141d0)
      common/oo/pm(3,3),qm(3,3)
      dimension rot(3,3)

      dimension x(3),y(3)


      a=xscale(x(1),0.d0,pi*2)
      b=xscale(x(2),0.d0,pi)
      g=xscale(x(3),0.d0,pi*2)
      call mat(-g,-b,-a,rot)
      f=0.d0
        do jj=1,3
        do ii=1,3
            dum1=0.d0
            do k=1,3
              dum1=dum1+rot(ii,k)*pm(jj,k)
            enddo
            f=f+(dum1-qm(jj,ii))**2
c            write(*,*) dum1,qm(jj,ii)
          enddo
        enddo
      write(*,*) a,b,g,f
      return
      end



      subroutine phidihed(b1,b2,b3,bb3,ab2b3,phi)

c.....tre vettori con stesso p.to applicazione. trova: la lungheza bb3 di b3,
c.....l'angolo diedro phi tra il piano b1b2 e il b2b3 e
c.....l'angolo ab2b3 di legame b2b3
      implicit real*8 (a-h,o-z)
      parameter(pi     = 3.14159265358979323846264338328d0)
      dimension b1(3),b2(3),b3(3)
      dimension p1(3),p2(3),p3(3),p4(3),p5(3)
      dimension v12(3),v23(3),b1s(3)

c  http://en.wikipedia.org/wiki/Dihedral_angle

c       write(*,*) ' akjdhf 67'
      call cpolar(b2(1),b2(2),b2(3),bb2,a,b)
c       write(*,*) ' akjdhf 68'
      call pv(b1,b2,v12)
c       write(*,*) ' akjdhf 69'
      call pv(b2,b3,v23)
      do i=1,3
        b1s(i)=bb2*b1(i)
        p1(i)=0.d0
        p2(i)=b1(i)
        p3(i)=b2(i)+p2(i)
        p4(i)=b3(i)+p3(i)
      enddo
      call ps(b1s,v23,x)
      call ps(v12,v23,y)
c      write(*,*) x,y
c       write(*,*) ' akjdhf 70'
      if (x.eq.0.d0.and.y.eq.0.d0) then
        phi=0.d0
      else
c       write(*,*) ' akjdhf 6799',x,y
c        phi = -atan2(x,y)  !! c'era un errore nel segno nella formula sul sito
        phi = atan2(x,y)  !! c'era un errore nel segno nella formula sul sito
      endif
c      write(*,*) phi

c       write(*,*) ' akjdhf 71'
      call ps(b2,b3,dum1)
c       write(*,*) ' akjdhf 72'
      call cpolar(b3(1),b3(2),b3(3),bb3,a,b)
c       write(*,*) ' akjdhf 73',bb2,bb3
      ab2b3=dacos(dum1/bb2/bb3)

cc.....conosco b1, b2, bb3, ab2b3, phi
c      do i=1,3
c        p1(i)=0.d0
c        p2(i)=b1(i)
c        p3(i)=b2(i)+p2(i)
c      enddo
c
c      call bonds(bb3,pi-ab2b3,phi,p3,p2,p1,p5)
c      do i=1,3
c        p5(i)=p5(i)-p3(i)
c      enddo
c      write(*,*) b3
c      write(*,*) p5
c
c      call  b3finder(b1,b2,bb3,ab2b3,phi,p5)
c
c      write(*,*) b3
c      write(*,*) p5
c      b1(1)=1.
c      b1(2)=0.
c      b1(3)=0.
c      b2(1)=0.
c      b2(2)=1.
c      b2(3)=0.
c      ab2b3=3*pi/4
c      phi=pi!0.d0
c      bb3=1.
c      call  b3finder(b1,b2,bb3,ab2b3,phi,p5)
c      write(56,*) (b1(i),i=1,3)
c      write(56,*) (b2(i),i=1,3)
c      write(56,*) (p5(i),i=1,3)
c      stop

      return
      end


      subroutine b3finder(b1,b2,bb3,ab2b3,phi,b3)

c.....noti due vettori b1 e b2 aventi stesso p.to di applicazione,
c.....trova il vettore b3 che forma con b2 un angolo di legame ab2b3 ed un angolo diedro phi con il piano b1b2

      implicit real*8 (a-h,o-z)
      parameter(pi     = 3.14159265358979323846264338328d0)
      dimension b1(3),b2(3),b3(3)
      dimension p1(3),p2(3),p3(3)

c  http://en.wikipedia.org/wiki/Dihedral_angle

c.....conosco b1, b2, bb3, ab2b3, phi
      do i=1,3
        p1(i)=0.d0
        p2(i)=b1(i)
        p3(i)=b2(i)+p2(i)
      enddo
      call bonds(bb3,pi-ab2b3,phi,p3,p2,p1,b3)
      do i=1,3
        b3(i)=b3(i)-p3(i)
      enddo
      return
      end

      subroutine invm3(a,ainv)

c.....matrice inversa di una matrice 3x3

      implicit real*8 (a-h,o-z)
      dimension a(3,3),ainv(3,3)
 
      det=a(1,1)*a(2,2)*a(3,3)+a(1,3)*a(3,2)*a(2,1)+a(1,2)*a(2,3)*a(3,1)
     >   -a(1,3)*a(2,2)*a(3,1)-a(1,1)*a(2,3)*a(3,2)-a(1,2)*a(2,1)*a(3,3)
      if (det.eq.0) stop 'det=0'

      ainv(1,1)=(a(2,2)*a(3,3)-a(2,3)*a(3,2))/det
      ainv(1,2)=(a(1,3)*a(3,2)-a(1,2)*a(3,3))/det
      ainv(1,3)=(a(1,2)*a(2,3)-a(1,3)*a(2,2))/det
      ainv(2,1)=(a(2,3)*a(3,1)-a(2,1)*a(3,3))/det
      ainv(2,2)=(a(1,1)*a(3,3)-a(1,3)*a(3,1))/det
      ainv(2,3)=(a(1,3)*a(2,1)-a(1,1)*a(2,3))/det
      ainv(3,1)=(a(2,1)*a(3,2)-a(2,2)*a(3,1))/det
      ainv(3,2)=(a(1,2)*a(3,1)-a(1,1)*a(3,2))/det
      ainv(3,3)=(a(1,1)*a(2,2)-a(1,2)*a(2,1))/det

      return
      end

      subroutine pv(uect1,vect1,wect1)

c.....Prodotto vettoriale wect1 tra uect1 e vect1

      implicit real*8 (a-h,o-z)
      dimension uect1(3),vect1(3),wect1(3)

      wect1(1) = uect1(2)*vect1(3) - uect1(3)*vect1(2)
      wect1(2) = uect1(3)*vect1(1) - uect1(1)*vect1(3)
      wect1(3) = uect1(1)*vect1(2) - uect1(2)*vect1(1)

      return
      end

      subroutine ps(uect1,vect1,p)

c.....Prodotto scalare p tra uect1 e vect1

      implicit real*8 (a-h,o-z)
      dimension uect1(3),vect1(3)

      p=0.d0
      do i=1,3
        p=p+uect1(i)*vect1(i)
      enddo

      return
      end


      subroutine linker_final_coo(istate,iilink,iconf)

      include 'quafit2014.cmn'

      character card*6,atype*3,rtype*3,btype*4,ctype*2,stype*2,rtype0*3
      complex *16 cum1,cum2,cum3,fanom(mxwave),cum4
      dimension ia_n(2),ia_ca(2),ia_c(2),ia_o(2)
      dimension immarr(mxlink),x(7)
      dimension xl(7),xh(7),g(7)


      rrmm=0.d0
      w=0.d0
c.....pezzo tratto dalla linkerpdb
      n=naalink(istate,iilink)
      jconf=iconf+4
 
c      do i=1,n 
c        ch3=amm(iprimlink(istate,iilink,i))
c        gru(i)=ch3
c      enddo
      nato=1 !numero di atomi
c      write(32,1001) 'ATOM  ',nato,' N  ',gru(1),1,
c     >          (terminals(istate,iilink,iconf,1,k),k=1,3)
      do k=1,3
        coord(nato,k)=terminals(istate,iilink,iconf,1,k)
      enddo
      iato(nato)=6 !!' NH', !   6!NH
      iato6(nato)=1
      iato7(nato)=1
      rrmm=rrmm+rexc(iato(nato))**3
      do iac=1,natgr(iato(nato))
        w=w+rmass(izgr(iato(nato),iac))
      enddo
      iammch(nato,1)=iprimlink(istate,iilink,1)
      iammch(nato,2)=nsite(istate)+iilink
      do i=1,n 
c      write(31,*) 'chi',chi00(i)-chi_lnk(istate,iilink,jconf,i),
c     >chi00(i),chi_lnk(istate,iilink,jconf,i)
      do ia=1,2  !ia=1 laterale, ia=2 gruppo amidico
        chi(i)=chi_lnk(istate,iilink,jconf,i)
        if (ia.eq.1) then
c          ch3=gru(i)
          chi0=chi(i)
          ii=iprimlink(istate,iilink,i)
          m2=natoadj(ii)-1
        else if (i.ne.n) then
          chi0=0.d0
          ii=23
          m2=natoadj(ii)-1
        else
          goto 433
        endif
        mm=0
        do 3131 m=1,m2
        xxx=cooadj(ii,m,1)
        y=cooadj(ii,m,2)
        z=cooadj(ii,m,3)
c        ch4=atypei(ijadj(ii,m,2))//' '
        if (ia.eq.1.and.(m.eq.1.or.m.eq.3.or.m.eq.4)) goto 3131 
        if (ia.eq.2.and.(m.eq.1.or.m.eq.5)) goto 3131 
        mm=mm+1
        caard(mm,1)=xxx
        caard(mm,2)=y
        caard(mm,3)=z
c        atom4(mm)=ch4
        iato4(mm)=ijadj(ii,m,1)
        iato5(mm)=ijadj(ii,m,2)
 3131   continue
        sum1=cooadj(ii,m2+1,1)
        sum2=cooadj(ii,m2+1,2)
        sum3=cooadj(ii,m2+1,3)
c        write(72,*) ia,sum1,sum2,sum3,ii,m2+1
        call mat(
     >        -gasaa(istate,iilink,iconf,i,ia)-chi0
     >       ,-dacos(cbaaa(istate,iilink,iconf,i,ia))
     >       ,-aasaa(istate,iilink,iconf,i,ia),rot2)

c      write(31,*)i,ia,
c     >        -gasaa(istate,iilink,iconf,i,ia)-(
c     >        -gammaaa(i+1,ia)),
c     >        -gasaa(istate,iilink,iconf,i,ia),
c     >        -gammaaa(i+1,ia)
c      write(31,*)i,ia,
c     >       -dacos(cbaaa(istate,iilink,iconf,i,ia))-(
c     >       -betaaa(i+1,ia)),
c     >       -dacos(cbaaa(istate,iilink,iconf,i,ia)),
c     >       -betaaa(i+1,ia)
c      write(31,*)i,ia,
c     >       -aasaa(istate,iilink,iconf,i,ia)-(
c     >       -alphaaa(i+1,ia)),
c     >       -aasaa(istate,iilink,iconf,i,ia),
c     >       -alphaaa(i+1,ia)


        xt=umsaa(istate,iilink,iconf,i,ia)*sfromc(cbuaa(istate
     >,iilink,iconf,i,ia))*dcos(ausaa(istate,iilink,iconf,i,ia))
        yt=umsaa(istate,iilink,iconf,i,ia)*sfromc(cbuaa(istate
     >,iilink,iconf,i,ia))*dsin(ausaa(istate,iilink,iconf,i,ia))               
        zt=umsaa(istate,iilink,iconf,i,ia)*
     >cbuaa(istate,iilink,iconf,i,ia)

c        xt1=centreaa(i+1,1,ia)
c        yt1=centreaa(i+1,2,ia)
c        zt1=centreaa(i+1,3,ia)
c        write(31,*) i,xt-xt1,xt,xt1
c        write(31,*) i,yt-yt1,yt,yt1
c        write(31,*) i,zt-zt1,zt,zt1

        do j=1,mm
          x1=caard(j,1)-sum1
          y1=caard(j,2)-sum2
          z1=caard(j,3)-sum3
          call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
          caard(j,1)=rx
          caard(j,2)=ry
          caard(j,3)=rz
          nato=nato+1
c          ch3=gru(i)
c          i7=i
c          if (ia.eq.2.and.j.eq.3) ch3=gru(i+1)
c          if (ia.eq.2.and.j.eq.3) i7=i+1
c          if (ia.eq.2) then
c          write(32,1001) 'ATOM  ',nato,atom4(j),ch3,i7,
c     >                  (caard(j,k),k=1,3)
          do k=1,3
            coord(nato,k)=caard(j,k)
          enddo
          iato(nato)=iato4(j)
          iato6(nato)=iato5(j)
          if (ia.eq.2.and.j.eq.3) then
            iato7(nato)=i+1
          else
            iato7(nato)=i
          endif
          rrmm=rrmm+rexc(iato(nato))**3
      do iac=1,natgr(iato(nato))
        w=w+rmass(izgr(iato(nato),iac))
      enddo
          if (ia.eq.2.and.j.eq.3) then
            iammch(nato,1)=iprimlink(istate,iilink,i+1)
          else
            iammch(nato,1)=iprimlink(istate,iilink,i)
          endif
          iammch(nato,2)=nsite(istate)+iilink
c          endif
        enddo
      enddo !ia
      enddo !i
 433  nato=nato+1
c      write(32,1001) 'ATOM  ',nato,' C  ',gru(n),n,
c     >          (terminals(istate,iilink,iconf,2,k),k=1,3)
      do k=1,3
        coord(nato,k)=terminals(istate,iilink,iconf,2,k)
      enddo
      iato(nato)=1 
      iato6(nato)=3
      iato7(nato)=n
      do iac=1,natgr(iato(nato))
        w=w+rmass(izgr(iato(nato),iac))
      enddo
      rrmm=rrmm+rexc(iato(nato))**3
      iammch(nato,1)=iprimlink(istate,iilink,n)
      iammch(nato,2)=nsite(istate)+iilink
      nato=nato+1
c      write(32,1001) 'ATOM  ',nato,' O  ',gru(n),n,
c     >          (terminals(istate,iilink,iconf,3,k),k=1,3)
      do k=1,3
        coord(nato,k)=terminals(istate,iilink,iconf,3,k)
      enddo
      iato(nato)=9
      iato6(nato)=21
      iato7(nato)=n
      rrmm=rrmm+rexc(iato(nato))**3
      iammch(nato,1)=iprimlink(istate,iilink,n)
      iammch(nato,2)=nsite(istate)+iilink
      rrmm=(rrmm/nato)**(1./3)
      rmfull(nsite(istate)+iilink)=rrmm
c....fine pezzo tratto da...
      wlink(istate,iilink)=w


c.....nuova parte per constraints
      i2=nsite(istate)+iilink
c.....a
      n=isitlnkconstr_a(istate,i2,1)
      do kk=1,n
        nc=isitlnkconstr_a(istate,i2,1+kk)
        iat=iconstr_a(istate,nc,10)
        do k=1,3
          cooconstr_a(istate,nc,k)=coord(iat,k)
        enddo
      enddo
c.....b
      n=isitlnkconstr_b(istate,i2,1)
      do kk=1,n
        nc=isitlnkconstr_b(istate,i2,1+kk)
        iat=iconstr_b(istate,nc,15)
        ie=isitlnkconstr_b(istate,isite,1+mxconstr+kk)
        do k=1,3
          cooconstr_b(istate,nc,ie,k)=coord(iat,k)
        enddo
      enddo
      return
      end


      subroutine linkerpdb(istate,iilink,iconf)

      include 'quafit2014.cmn'
      character atype*3

      open(unit=32,file='linker.pdb')
      n=naalink(istate,iilink)
      jconf=iconf+4

      do i=1,n 
        ch3=amm(iprimlink(istate,iilink,i))
        gru(i)=ch3
      enddo
      nato=0 !numero di atomi
      nato=nato+1
      write(32,1001) 'ATOM  ',nato,' N  ',gru(1),1,
     >          (terminals(istate,iilink,iconf,1,k),k=1,3)

      do i=1,n 
c      write(31,*) 'chi',chi00(i)-chi_lnk(istate,iilink,jconf,i),
c     >chi00(i),chi_lnk(istate,iilink,jconf,i)
      do ia=1,2  !ia=1 laterale, ia=2 gruppo amidico
        chi(i)=chi_lnk(istate,iilink,jconf,i)
        if (ia.eq.1) then
          ch3=gru(i)
          chi0=chi(i)
          ii=iprimlink(istate,iilink,i)
          m2=natoadj(ii)-1
        else if (i.ne.n) then
          chi0=0.d0
          ii=23
          m2=natoadj(ii)-1
        else
          goto 433
        endif
        mm=0
        do 3131 m=1,m2
        x=cooadj(ii,m,1)
        y=cooadj(ii,m,2)
        z=cooadj(ii,m,3)
        ch4=atypei(ijadj(ii,m,2))//' '
        if (ia.eq.1.and.(m.eq.1.or.m.eq.3.or.m.eq.4)) goto 3131 
        if (ia.eq.2.and.(m.eq.1.or.m.eq.5)) goto 3131 
        mm=mm+1
        caard(mm,1)=x
        caard(mm,2)=y
        caard(mm,3)=z
        atom4(mm)=ch4
 3131   continue
        sum1=cooadj(ii,m2+1,1)
        sum2=cooadj(ii,m2+1,2)
        sum3=cooadj(ii,m2+1,3)
c        write(72,*) ia,sum1,sum2,sum3,ii,m2+1
        call mat(
     >        -gasaa(istate,iilink,iconf,i,ia)-chi0
     >       ,-dacos(cbaaa(istate,iilink,iconf,i,ia))
     >       ,-aasaa(istate,iilink,iconf,i,ia),rot2)

c      write(31,*)i,ia,
c     >        -gasaa(istate,iilink,iconf,i,ia)-(
c     >        -gammaaa(i+1,ia)),
c     >        -gasaa(istate,iilink,iconf,i,ia),
c     >        -gammaaa(i+1,ia)
c      write(31,*)i,ia,
c     >       -dacos(cbaaa(istate,iilink,iconf,i,ia))-(
c     >       -betaaa(i+1,ia)),
c     >       -dacos(cbaaa(istate,iilink,iconf,i,ia)),
c     >       -betaaa(i+1,ia)
c      write(31,*)i,ia,
c     >       -aasaa(istate,iilink,iconf,i,ia)-(
c     >       -alphaaa(i+1,ia)),
c     >       -aasaa(istate,iilink,iconf,i,ia),
c     >       -alphaaa(i+1,ia)


        xt=umsaa(istate,iilink,iconf,i,ia)*sfromc(cbuaa(istate
     >,iilink,iconf,i,ia))*dcos(ausaa(istate,iilink,iconf,i,ia))
        yt=umsaa(istate,iilink,iconf,i,ia)*sfromc(cbuaa(istate
     >,iilink,iconf,i,ia))*dsin(ausaa(istate,iilink,iconf,i,ia))               
        zt=umsaa(istate,iilink,iconf,i,ia)*
     >cbuaa(istate,iilink,iconf,i,ia)

c        xt1=centreaa(i+1,1,ia)
c        yt1=centreaa(i+1,2,ia)
c        zt1=centreaa(i+1,3,ia)
c        write(31,*) i,xt-xt1,xt,xt1
c        write(31,*) i,yt-yt1,yt,yt1
c        write(31,*) i,zt-zt1,zt,zt1

        do j=1,mm
          x1=caard(j,1)-sum1
          y1=caard(j,2)-sum2
          z1=caard(j,3)-sum3
          call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot2)
          caard(j,1)=rx
          caard(j,2)=ry
          caard(j,3)=rz
          nato=nato+1
          ch3=gru(i)
          i7=i
          if (ia.eq.2.and.j.eq.3) ch3=gru(i+1)
          if (ia.eq.2.and.j.eq.3) i7=i+1
c          if (ia.eq.2) then
          write(32,1001) 'ATOM  ',nato,atom4(j),ch3,i7,
     >                  (caard(j,k),k=1,3)
c          endif
        enddo
      enddo !ia
      enddo !i
 433  nato=nato+1
      write(32,1001) 'ATOM  ',nato,' C  ',gru(n),n,
     >          (terminals(istate,iilink,iconf,2,k),k=1,3)
      nato=nato+1
      write(32,1001) 'ATOM  ',nato,' O  ',gru(n),n,
     >          (terminals(istate,iilink,iconf,3,k),k=1,3)
      close(32)

c.....nuova parte per constraints
      open(unit=32,file='linker.pdb')
      do iat=1,nato
        read(32,fmt=2000) atype,irr
        do iatype=1,21
          if (atype.eq.atypei(iatype)) goto 97
        enddo
        iatype=9              ! un ossigeno
 97     continue
c.......a
        m=nconstr_a(istate)
        do nc=1,m
        i2=iconstr_a(istate,nc,2)
        if (i2.eq.nsite(istate)+iilink) then
          i3=iconstr_a(istate,nc,1)
          i4=iconstr_a(istate,nc,5)
          if (i3.eq.irr.and.i4.eq.iatype) then
            kk=isitlnkconstr_a(istate,i2,1)
            kk=kk+1
            isitlnkconstr_a(istate,i2,1+kk)=nc
            isitlnkconstr_a(istate,i2,1)=kk
            iconstr_a(istate,nc,10)=iat
          endif
        endif
        enddo !nc
c.......b
        m=nconstr_b(istate)
        do nc=1,m
        i2i=iconstr_b(istate,nc,2)
        i2j=iconstr_b(istate,nc,7)
        if (i2i.eq.nsite(istate)+iilink) then
          i3=iconstr_b(istate,nc,1)
          i4=iconstr_b(istate,nc,5)
          if (i3.eq.irr.and.i4.eq.iatype) then
            kk=isitlnkconstr_b(istate,i2i,1)
            kk=kk+1
            isitlnkconstr_b(istate,i2i,1+kk)=nc
            isitlnkconstr_b(istate,i2i,1)=kk
            isitlnkconstr_b(istate,isite,1+mxconstr+kk)=1
            iconstr_b(istate,nc,16)=kk
            iconstr_b(istate,nc,15)=iat
          endif
        endif
        if (i2j.eq.nsite(istate)+iilink) then
          i3=iconstr_b(istate,nc,6)
          i4=iconstr_b(istate,nc,10)
          if (i3.eq.irr.and.i4.eq.iatype) then
            kk=isitlnkconstr_b(istate,i2j,1)
            kk=kk+1
            isitlnkconstr_b(istate,i2j,1+kk)=nc
            isitlnkconstr_b(istate,i2j,1)=kk
            isitlnkconstr_b(istate,isite,1+mxconstr+kk)=2
            iconstr_b(istate,nc,17)=kk
            iconstr_b(istate,nc,15)=iat
          endif
        endif
        enddo !nc
c.......fine nuova parte per constraints

      enddo
      close(32)


      return
 1001 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
2000  format(6x,5x,1x,a3,2x,3x,2x,i4)
      end




      subroutine fulllinker(istate,iilink,iconf)

      include 'quafit2014.cmn'

      character card*6,atype*3,rtype*3,btype*4,ctype*2,stype*2,rtype0*3
      complex *16 cum1,cum2,cum3,fanom(mxwave),cum4
      dimension ia_n(2),ia_ca(2),ia_c(2),ia_o(2)
      dimension immarr(mxlink),x(7)
      dimension xl(7),xh(7),g(7)


      qmin=0
      dq=((qmfull)-(qmin))/float(nqfull-1)
      q=(qmin)-dq
      do iq=1,nqfull
        q=q+dq
        qqfull(iq)=(q)
      enddo

      call linker_final_coo(istate,iilink,iconf)

c.....se vuoi per test
c      do i=1,nato
c      write(34,1011) 'ATOM  ',i,atypej(iato(i))//' ','   ',1,
c     >          (coord(i,k),k=1,3)
c      enddo
c      stop
c 1011 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
c.....fine se vuoi per test


c.....inizio pezzo copiato dall fullpdb

c      open (unit=iu_sin,status='unknown',file=fsub(ipdb)(1:j1)//
c     >'.ilm')

c      lstep=1
c      lstart=0  
        do iq=1,nqfull
c          call wprc(iq,1,nqfull,lstep,lstart)
          q=qqfull(iq)

c.........pensala da qui
          do l=0,lfirst
            do m=-l,l
              do is=1,nsfull+nwave+8 !new
                alm(l,m,is)=dcmplx(zer,zer)
              enddo
            enddo
          enddo


      do i=1,nato
        x1=coord(i,1)
        y1=coord(i,2)
        z1=coord(i,3)
        call cpolar(x1,y1,z1,r,al1,b1)
        cb1=dcos(b1)
        call py_go(lfirst,b1)
        call cscsa(lfirst,al1)
        i1=iato(i)
        fdeu=0.d0
c        if (iammch(i,1).ne.0) then
c          fdeu=fdeut(iammch(i,1),iammch(i,2))
c        endif
  

          do lw=1,nwave
            call sfanom(i1,wave(lw),fanom(lw))
          enddo


c  Rep. Prog. Phys. 67 (2004) 1073
c  Unique aspects of neutron scattering for the study of biological systems
c  Heinrich B Stuhrmann
c
c   1H: b(H) = [-0.3742±1.456P(H)] 10-12 cm, (7.1)
c   2H: b(D) = [+0.6671±0.270P(D)] 10-12 cm, (7.2)

c J. Appl. Cryst. (1989). 22, 352-362
c Polarized Neutron Scattering by Polarized Protons of Bovine Serum Albumin
c in Deuterated Solvent
c BY W. KNOP, H.-J. SCHINK, H. B. STUHRMANN,* R. WAGNER AND M. WENKOW-Es-SOUNI
c p(H20) = [-0.56±9.66P(H)] 
c p(D20) = [+6.40±1.50P(D)]


          bneu1= sclen(i1)+fdeu*(hgrou(i1)-hexg(i1))*(0.6671+0.3742)
          bneu2= hexg(i1)*afac(i)*(0.6671+0.3742)
          bneu3= hgrou(i1)*1.456*(1-fdeu) !il terzo diviso 1.456 e' An1
          bneu4=-hexg(i1)*afac(i)*1.456
          bneu5= hexg(i1)*afac(i)*0.27  !il quinto diviso 0.27 e' An2. 

          dum1=sfactt(i1)
          dum2=sfacts(i1)

          qu=q*r
          
          do l=0,lfirst
            if (qu.ne.0.d0) then
c              write(*,*) 'fulllinker',q,r,qu,x1,y1,z1
              call sphbes(l,qu,sj,sy,sjp,syp)
            else
              if (l.eq.0) then
                sj=1.d0
              else
                sj=0.d0
              endif
            endif
            cum1=sj*(dcmplx(0.d0,1.d0))**l

            cum2=cum1*ysf(l,0)

            alm(l,0,1)=alm(l,0,1)+dum1*cum2
            do lw=1,nwave
              alm(l,0,1+lw)  =alm(l,0,1+lw)  +fanom(lw)*cum2
            enddo

            alm(l,0,1+nwave+1)=alm(l,0,1+nwave+1)+bneu1*cum2
            alm(l,0,1+nwave+2)=alm(l,0,1+nwave+2)+bneu2*cum2
            alm(l,0,1+nwave+3)=alm(l,0,1+nwave+3)+bneu3*cum2
            alm(l,0,1+nwave+4)=alm(l,0,1+nwave+4)+bneu4*cum2
            alm(l,0,1+nwave+5)=alm(l,0,1+nwave+5)+bneu5*cum2

            alm(l,0,1+nwave+6)=alm(l,0,1+nwave+6)+dum2 *cum2

            do m=1,l
              cum2=cum1*dcmplx(csa(1,m),-csa(2,m))*ysf(l,m)

              alm(l,m,1)=alm(l,m,1)+dum1*cum2
c              if (iq.eq.2.and.l.eq.1.and.m.eq.1.and.i.eq.7) 
c     >write(80,*) q,alm(l,m,1),dum1,cum2,cum1,
c     >dcmplx(csa(1,m),-csa(2,m)),ysf(l,m),al1,b1,x1,y1,z1
              do lw=1,nwave
                alm(l,m,1+lw)  =alm(l,m,1+lw)  +fanom(lw)*cum2
                alm(l,-m,1+lw)  =alm(l,-m,1+lw)  
     >    +fanom(lw)*(-1)**m*ysf(l,m)*dcmplx(csa(1,m),csa(2,m))*cum1
              enddo

              alm(l,m,1+nwave+1)=alm(l,m,1+nwave+1)+bneu1*cum2
              alm(l,m,1+nwave+2)=alm(l,m,1+nwave+2)+bneu2*cum2
              alm(l,m,1+nwave+3)=alm(l,m,1+nwave+3)+bneu3*cum2
              alm(l,m,1+nwave+4)=alm(l,m,1+nwave+4)+bneu4*cum2
              alm(l,m,1+nwave+5)=alm(l,m,1+nwave+5)+bneu5*cum2

              alm(l,m,1+nwave+6)=alm(l,m,1+nwave+6)+dum2*cum2  !!! in tutte queste definizioni non c'e' il 4*pi ma quando le usi vanno bene. Questo, moltiplicato per 4*pi va benissimo per la shape ma va ridefinito il suo range di q, perche' poi lo integri.
            enddo
          enddo
        enddo


c.......negative values of m
        do l=0,lfirst
          do m=1,l
            i7=(-1)**(l+m)
            alm(l,-m,1)=i7*dconjg(alm(l,m,1))
            do is=2+nwave,nsfull+nwave+8
              alm(l,-m,is)=i7*dconjg(alm(l,m,is))
            enddo
          enddo
        enddo

c        write(*,1007) q,(((alm(l,m,is),m=-l,l),
c     >         l=0,lfirst),is=1,nwave+nsfull+8)

c.......new store
        do l=0,lfirst
        do m=-l,l
        do is=1,nwave+nsfull+8
        almstore(mxpdb+1,iq,l,m,is)=alm(l,m,is)
        enddo
        enddo
        enddo
c.......end of new

      enddo  ! end iq
c      close(unit=iu_sin)

c      stop !per ora
      return

2208  format(a40,3(g12.5,1x),i4)
 1007 format(500000(e15.6E3))
 2001 format(a6,6x,a3,a2,a3,2x,i4,a4,3f8.3)
2000  format(a6,i5,1x,a3,a2,a3,a2,i4,a4,3f8.3)
 1005 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
1001  format(a6,i5,a10,a1,a8,3(f8.3))
 1011 format(a6,i5,1x,a4,1x,a3,i6,4x,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
 1107 format('#',200(e19.7E3))
 1207 format(200(e19.7E3))


1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end


      subroutine parconstlinker(istate,iilink,ihagg,iconf)

      include 'quafit2014.cmn'

      if (nexper.eq.0) return
c      write(*,*) '   ---> parconstlinker   ',istate,iilink,ihagg,iconf
      call f1f2(istate,ihagg,mxpdb+1,ib)
      call f2f4_lnk_new(istate,ihagg,ib,ia)
      call f4f5(istate,ihagg,ia,ib)
      call f5f6(istate,ihagg,iconf,ib,ia)
      call f6f7(istate,ihagg,iconf,ia,ib)
      call f7f8(istate,ihagg,iconf,ib)

      call jseque(iu_ful,istate,nsite(istate)+iilink,ihagg,iconf,3)

      do iq=1,nqfull
        q=qqfull(iq)
        write(unit=iu_ful,rec=iq,fmt=1007) q,
     >(((almstore(ibfinal,iq,l,m,is),m=-l,l),
     > l=0,lsecond),is=1,nwave+nsfull+8)
      enddo
      return
 1007 format(500000(e15.6E3))
      end


      subroutine framhaglink(istate,ihagg,iconf)

c.....rotazioni e tralazioni che servono per i linker per ogni isub,ipoly

      include 'quafit2014.cmn'
c      write(*,*) '   ---> framhag   '
      jconf=iconf+4
      
            isite=1
c            write(*,*) ' mattina 4',iflag
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv

c      if (iflag.eq.1.and.ifhinge(istate)
c     >.eq.1.and.ihagg.eq.1) call ufind(istate,iconf)
c      if (iflag.eq.1.and.ifhinge(istate)
c     >.eq.2.and.ihagg.eq.1) call afind(istate,iconf)

      call mat(agga(istate,ihagg),aggb(istate,ihagg),
     >aggg(istate,ihagg),roz1)

      call mat(alpscrew(istate,ihagg,jconf),
     >dacos(cbescrew(istate,ihagg,jconf)),
     >zer,roz2)

      sum1=0.d0
      sum2=0.d0
      sum3=0.d0

      rum1=0.d0
      rum2=0.d0
      rum3=0.d0

      do isite=1,1

c            write(*,*) ' mattina 3'
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv

c                write(*,*) ' 41 ci sono',ums(istate,isite,jconf),
c     >aus(istate,isite,jconf),cbu(istate,isite,jconf)
c     >,istate,isite,jconf

c.sep.2012        xt=0.
c.sep.2012        yt=0.
c.sep.2012        zt=0.

c.sep.2012cc.......[xt,yt,zt] = {\bf u}_i
c.sep.2012
c.sep.2012c            write(650,*) 'AA',iconf,xt,yt,zt
c.sep.2012c         write(*,*) ' prima di ',-gas(istate,isite,jconf),
c.sep.2012c     >-dacos(cba(istate
c.sep.2012c     >,isite,jconf)),-aas(istate,isite,jconf)
c.sep.2012        call mat(zer,zer,zer,tor) !matrice che ruota il sito isite nel vecchio sys.rif
c.sep.2012c.....tor = R^{-1}[\omega_i] (1)
c.sep.2012c            write(*,*) aas(istate,isite,jconf)*conv
c.sep.2012c            write(*,*) cba(istate,isite,jconf)
c.sep.2012c            write(*,*) gas(istate,isite,jconf)*conv
c.sep.2012c            write(*,*) ' TOR ',tor
c.sep.2012
c.sep.2012c.......Prodotto fra matrici
c.sep.2012        do i=1,3
c.sep.2012          do j=1,3
c.sep.2012	    dum1=0.d0
c.sep.2012            do k=1,3
c.sep.2012               dum1=dum1+roz2(i,k)*tor(k,j) ! l'ordine e' importante! l'ho controllato
c.sep.2012            enddo
c.sep.2012            tor2(i,j)=dum1 !matrice che ruota il sito isite nel nuovo sistema di rif.
c.sep.2012          enddo
c.sep.2012        enddo
c.sep.2012c.....tor2 = R[\omega_s]*R^{-1}[\omega_i] (1+2)

c.sep.2012        call rotrdi(xt,yt,zt,xt1,yt1,zt1,zer,zer,zer,roz2)
c        write(*,*) ' inizio ',xt,yt,zt
c.......[xt1,yt1,zt1]=R[\omega_s]{\bf u}_i , ovvero' u nel nuovo syst.

        do ipoly=1,npoly(istate,ihagg)
          call mat(zer,zer,-(ipoly-1)*ascrew(istate,ihagg,jconf),tor1) !matrice che ruota isite per screw nel nuovo sis.rif
          dd=(ipoly-1)*dscrew(istate,ihagg,jconf)
          do isub=1,nsymm(istate,ihagg)
            call mat(-gamsym(istate,ihagg,isub),
     >               -betsym(istate,ihagg,isub),
     >               -alpsym(istate,ihagg,isub),roz) !matrice che ruota isite per operaz. symm. nel vecchio sistema rif.
c            write(*,*) ' ROZ ',roz
c            write(*,*) ' TOR2 ',tor2
c...........roz=R^-1{\Omega_{\cal R}}
c...........Prodotto fra matrici
            do i=1,3
             do j=1,3
              dum1=0.d0
              do k=1,3
c.sep.2012
               do l=1,3
                do l2=1,3
                 do l3=1,3
c                    dum1=dum1+tor1(i,k)*roz2(k,l)*roz(l,j)
                  dum1=dum1+tor1(i,k)*roz2(k,l)*roz1(l2,l)
     >                       *roz(l2,l3)*roz1(l3,j)
                 enddo
                enddo
               enddo
c.sep.2012
              enddo
              rot(i,j)=dum1 !matrice che ruota isite per operaz. symm. nel nuovo sistema rif.
             enddo
            enddo
            call taminv(dum1,dum2,dum3,rot)
            alprot_lnk(isub,ipoly)=dum1
            betrot_lnk(isub,ipoly)=dum2
            gamrot_lnk(isub,ipoly)=dum3
c            write(*,*) isite,isub
c            write(*,*) aas(istate,isite,jconf)*conv
c            write(*,*) cba(istate,isite,jconf)
c            write(*,*) gas(istate,isite,jconf)*conv
c            write(*,*) ' TOR ',tor
c            write(*,*)
c            write(*,*) ' ROT',rot
c            write(*,*) isub,conv*dum1,cos(dum2),dum3*conv
cc            call mat(-dum3,-dum2,-dum1,rot2)
cc            write(*,*) 'dopo di ',dum1,dum2,dum3
cc            write(*,*) ' ROT',rot2
c            pause
c...........ovvero per ruotare ciascun isite,isub,ipoly devi fare R^-1(alprot,betrot,gamrot)

c            call rotrdi(xt1,yt1,zt1,xt2,yt2,zt2,zer,zer,zer,roz)
c            call rotrdi(xt2,yt2,zt2,xt,yt,zt,zer,zer, dd,tor1)

c.sep.2012            call rotrdi(xt1,yt1,zt1,xt2,yt2,zt2,zer,zer,zer,roz)
c.sep.2012            call rotrin(xt2,yt2,zt2,xt3,yt3,zt3,zer,zer,zer,roz2)
c.sep.2012            call rotrdi(xt3,yt3,zt3,xt,yt,zt,zer,zer, dd,tor1)

c.sep.2012            centre_lnk(isub,ipoly,1)=xt
c.sep.2012            centre_lnk(isub,ipoly,2)=yt
c.sep.2012            centre_lnk(isub,ipoly,3)=zt
c.sep.2012 tre righe cambiate
            centre_lnk(isub,ipoly,1)=zer
            centre_lnk(isub,ipoly,2)=zer
            centre_lnk(isub,ipoly,3)=dd
c.sep.2012
c            write(650,*) iconf,xt,yt,zt

c...........Rifletti da qui perche' a te serve un <u> prima dello screw.... 
c            if (ipoly.eq.1) then !!! ok, ho riflettuto
cc          write(*,*) ' i r x ',rum1,xt1,xt2,xt3,xt,isite,roz
cc          write(*,*) ' i r y ',rum2,yt1,yt2,yt3,yt,roz2
cc          write(*,*) ' i r z ',rum3,zt1,zt2,zt3,zt,tor1
c              rum1=rum1+xt
c              rum2=rum2+yt
c              rum3=rum3+zt
c            endif

            sum1=sum1+xt
            sum2=sum2+yt
            sum3=sum3+zt
          enddo
        enddo
      enddo

cc.....Trasla in modo che il centro coincida con il c.geom.
      dum1=zer
      if (nsite(istate).gt.0) then
      dum1=1.d0/(nsymm(istate,ihagg)*nsite(istate)*npoly(istate,ihagg))
      endif
      sum1=sum1*dum1
      sum2=sum2*dum1
      sum3=sum3*dum1
c      trasl(istate,ihagg,1)=sum1
c      trasl(istate,ihagg,2)=sum2
c      trasl(istate,ihagg,3)=sum3
c      dum1=1.d0/(nsymm(istate,ihagg)*nsite(istate))
cc          write(*,*) ' r x ',rum1,dum1
cc          write(*,*) ' r y ',rum2
cc          write(*,*) ' r z ',rum3
c      rum1=rum1*dum1
c      rum2=rum2*dum1
c      rum3=rum3*dum1
c      do isite=1,nsite(istate)
c        xt=centre(isite,1,1,1)-rum1
c        yt=centre(isite,1,1,2)-rum2
c        zt=centre(isite,1,1,3)-rum3
cc          write(*,*) ' u00x ',centre(isite,1,1,1),rum1
cc          write(*,*) ' u00y ',centre(isite,1,1,2),rum2
cc          write(*,*) ' u00z ',centre(isite,1,1,3),rum3
cc          write(*,*) ' u00 ',xt1,yt1,zt1,roz2
c
c        call rotrdi(xt,yt,zt,xt1,yt1,zt1,zer,zer,zer,roz2) !! perche' quando lo usi sei nel nuovo sistema!
c        call cpolar(xt1,
c     >              yt1,
c     >              zt1,
c     >              uesse(isite),
c     >              aesse(isite),
c     >              besse(isite)) 
cc          write(*,*) ' u ',xt1,yt1,zt1
        do isub=1,nsymm(istate,ihagg)
          do ipoly=1,npoly(istate,ihagg)
c            write(*,*) 'CDC',(centre_lnk(isub,ipoly,k),k=1,3)
c            write(*,*) sum1,sum2,sum3,dum1
            centre_lnk(isub,ipoly,1)= 
     >      centre_lnk(isub,ipoly,1)-trasl(istate,ihagg,1)!sum1
            centre_lnk(isub,ipoly,2)=
     >      centre_lnk(isub,ipoly,2)-trasl(istate,ihagg,2)!sum2
            centre_lnk(isub,ipoly,3)=
     >      centre_lnk(isub,ipoly,3)-trasl(istate,ihagg,3)!sum3
c            write(*,*) 'CDC2',(centre_lnk(isub,ipoly,k),k=1,3)
          enddo
        enddo
c      enddo
c      call cpolar(rum1,
c     >            rum2,
c     >            rum3,
c     >            uessea,
c     >            aessea,
c     >            bessea)
c      stop
      return
c        call cpolar(centre(isite,1,1,1),
c     >              centre(isite,1,1,2),
c     >              centre(isite,1,1,3),
c     >              xms(isite),
c     >              axs(isite),
c     >              bxs(isite)) !vettore centro isite nel nuovo sys.rif dopo traslazione
c        cbx(isite)=dcos(bxs(isite))

c      return
      end



      subroutine framalllink(istate,ihagg,iconf)

      include 'quafit2014.cmn'
      
      call framhaglink(istate,ihagg,iconf)
      jconf=iconf+4
      do ipoly=1,npoly(istate,ihagg)
        do isub=1,nsymm(istate,ihagg)
c###########linkers

          call mat(
     >        -gamrot_lnk(isub,ipoly)
     >       ,-betrot_lnk(isub,ipoly)
     >       ,-alprot_lnk(isub,ipoly),rot)

          xt=centre_lnk(isub,ipoly,1)
          yt=centre_lnk(isub,ipoly,2)
          zt=centre_lnk(isub,ipoly,3)
          do iilink=1,nlink(istate)
      n=naalink(istate,iilink)
          do i=1,n
        x1=umsaa(istate,iilink,iconf,i,1)*sfromc(cbuaa(istate
     >,iilink,iconf,i,1))*dcos(ausaa(istate,iilink,iconf,i,1))
        y1=umsaa(istate,iilink,iconf,i,1)*sfromc(cbuaa(istate
     >,iilink,iconf,i,1))*dsin(ausaa(istate,iilink,iconf,i,1))               
        z1=umsaa(istate,iilink,iconf,i,1)*
     >cbuaa(istate,iilink,iconf,i,1)

c...........Trasla 
c...........con xt,yt,zt e poi ruota con la matrice rot

              call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
              centre_aa_fin(iilink,i,isub,ipoly,1)=rx  ! e' il centro di ogni AA per ogni isub, ipoly, ecc.
              centre_aa_fin(iilink,i,isub,ipoly,2)=ry
              centre_aa_fin(iilink,i,isub,ipoly,3)=rz

            enddo !i
          enddo !iilink

        enddo   ! end isub
      enddo   !end ipoly

c.....parte nuova per constraints
      if (ihagg.gt.1) return
      do iilink=1,nlink(istate)
        call constr_link_rot(istate,iilink)
      enddo

      return
      end



      subroutine disv_aa_aa(istate,ihagg,iilink,isub,ipoly,
     >                                  jilink,jsub,jpoly,ov) 

      include 'quafit2014.cmn'

c.....Gia' chiamata la framalllink

      ov=zer
cc      write(*,*) ' DD ',isite,isub,jsite,jsub
      if (icheckhag.eq.0) then
        if (w_overl.eq.zer.or.ihagg.gt.1) return !! prima del 9 maggio 2012
      else
        if (w_overl.eq.zer) return
      endif
c      write(*,*) '   ---> disv_pair '
      ni=naalink(istate,iilink)
      nj=naalink(istate,jilink)
      iii=ni/2
      jjj=nj/2
      if (iii.eq.0) iii=1
      if (jjj.eq.0) jjj=1
      x=centre_aa_fin(iilink,iii,isub,ipoly,1)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,1)
      y=centre_aa_fin(iilink,iii,isub,ipoly,2)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,2)
      z=centre_aa_fin(iilink,iii,isub,ipoly,3)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,3)
      call cpolar(x,y,z,r,alpr,betr)
      if (r.gt.(nj+ni)*3.8/2) return
      i1=0
      do iii=1,ni !! indice AA linker iilink
      do jjj=1,nj
      ia=iprimlink(istate,iilink,iii) !!tipo di AA
      ja=iprimlink(istate,jilink,jjj)
      x=centre_aa_fin(iilink,iii,isub,ipoly,1)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,1)
      y=centre_aa_fin(iilink,iii,isub,ipoly,2)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,2)
      z=centre_aa_fin(iilink,iii,isub,ipoly,3)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,3)
      call cpolar(x,y,z,r,alpr,betr)
      dsmax=rmadj(ia)+rmadj(ja)

c.....nuovo
      if (ieqcont.eq.1) dsmax=dsmax/1.12246204830937d0
      r=r/dsmax
      if (r.gt.1.e-2) then
c.......Per overlap
        dum7=(one/r)**6
        ov=ov+w_aaaa*4*(dum7**2-dum7) !!!+ 1!LJ
c        ov=ov+/r**12
      else
        ov=ov+1.d20
      endif


c      precedente
c      verifica che la distanza tra due AA di due linker diversi non sia minore della somma dei due 
c      if (r.lt.rmadj(ia)+rmadj(ja)) then !! vedrai
c        ov=ov+w_aaaa
c      endif
      i1=i1+1
      enddo!iii
      enddo!jjj
      ov=ov/i1
      return
      end


      subroutine disv_site_aa(istate,ihagg,isite,isub,ipoly,
     >                                jilink,jsub,jpoly,iconn,ov) 

      include 'quafit2014.cmn'

c.....Gia' chiamata la framalllink
      parameter(naaskip=3,shift=5.d0)!5.d0
      ov=zer
cc      write(*,*) ' DD ',isite,isub,jsite,jsub
      if (icheckhag.eq.0) then
        if (w_overl.eq.zer.or.ihagg.gt.1) return !! prima del 9 maggio 2012
      else
        if (w_overl.eq.zer) return
      endif
c.....prendo      
      nj=naalink(istate,jilink)
      ddmax=dmax(ipdbsite(istate,isite))
      jjj=nj/2 !il calpha centrale
      if (jjj.eq.0) jjj=1
      x=centre(isite,isub,ipoly,1)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,1)
      y=centre(isite,isub,ipoly,2)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,2)
      z=centre(isite,isub,ipoly,3)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,3)
c.w      write(*,*) isite,jilink
c      write(*,*) (centre(isite,isub,ipoly,k),k=1,3)
c      write(*,*) (centre_aa_fin(jilink,jjj,jsub,jpoly,k),k=1,3)
      call cpolar(x,y,z,r,alpr,betr)
      if (r.gt.nj*3.8/2+ddmax) return
c      write(*,*) r,nj*3.8/2+ddmax,ddmax
      nnn=nj-2*naaskip*iconn
      if (nnn.le.0) return
c      write(*,*) '   ---> disv_pair '
c.w      write(61,1001) 'HETATM',i,' SE       '
c.w     >,'D','   1    ',(centre(isite,isub,ipoly,k),k=1,3)
      do jjj=1+naaskip*iconn,nj-naaskip*iconn !!! cosi' evito di prendere ...
      ja=iprimlink(istate,jilink,jjj)
      x=centre(isite,isub,ipoly,1)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,1)
      y=centre(isite,isub,ipoly,2)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,2)
      z=centre(isite,isub,ipoly,3)
     > -centre_aa_fin(jilink,jjj,jsub,jpoly,3)

c.w      write(61,1001) 'HETATM',i,' FE       '
c.w     >,'E','   1    ',(centre_aa_fin(jilink,jjj,jsub,jpoly,k),k=1,3)
c.w1001  format(a6,i5,a10,a1,a8,3(f8.3))

      call cpolar(x,y,z,r,alpr,betr)
      if (r.lt.(ddmax+rmadj(ja))*1.03) then
c.......Test distanza
        call sigcal_new(
     >                   ipdbsite(istate,isite),
     >                   npdb+1, !! e' l'indice di cio' che chiami npdb+1, il CH3
     >                   alprot(isite,isub,ipoly),
     >                   betrot(isite,isub,ipoly),
     >                   gamrot(isite,isub,ipoly),
     >                   zer,zer,zer,
     >                   alpr,betr,ovlt,dstt,rsct)
        dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
     >+shift+rmadj(ja) !! qui potresti anche ridurre o aumentare dsmax in base a rmadj: fatto!
c        dum5=sigcal_new(
c     >                   ipdbsite(istate,isite),
c     >                   npdb+1, !! e' l'indice di cio' che chiami npdb+1, il CH3
c     >                   alprot(isite,isub,ipoly),
c     >                   betrot(isite,isub,ipoly),
c     >                   gamrot(isite,isub,ipoly),
c     >                   zer,zer,zer,
c     >                   alpr,betr)
c        write(*,*) dsmax,r,w_aadd,dum5,rmadj(ja) 
c        write(*,*) 'dopo ',ipdbsite(istate,isite),
c     >                   npdb+1, !! e' l'indice di cio' che chiami npdb+1, il CH3
c     >                   alprot(isite,isub,ipoly),
c     >                   betrot(isite,isub,ipoly),
c     >                   gamrot(isite,isub,ipoly),
c     >                   alpr,betr
c        write(*,*) '#####'
        
c.......nuovo
        if (ieqcont.eq.1) dsmax=dsmax/1.12246204830937d0
        r=r/dsmax
        if (r.gt.1.e-2) then
c.........Per overlap
          dum7=(one/r)**6
          ov=ov+w_aadd*4*dum7*(dum7-one) !!!+ 1!LJ
c          ov=ov+w_aadd/r**12
        else
          ov=ov+1.d20
        endif

c.......precedente
c        if (r.lt.dsmax) then
c          ov=ov+w_aadd
c        endif
c
c
c      r=r/dsmax
c      if (r.gt.1.e-2) then
cc.......Per overlap
c        dum7=(one/r)**6
c        dum8=4*(dum7**2-dum7) !!!+ 1!LJ
c      else
c        dum8=1.d20
c      endif
c      ov=ov+dum8
      endif


      enddo!jjj
      ov=ov/nnn
cc      write(*,*) 'DD2 ',ov
      return
      end


      function sfromc(cosine)

      implicit real*8 (a-h,o-z)

      sfromc=0.d0
      if (cosine.ge.1.d0) return
      if (cosine.le.-1.d0) return
      sfromc=dsqrt(1.d0-cosine*cosine)
      return
      end



      subroutine ovexp(npar,x,f)

      include 'quafit2014.cmn'
c      include 'genfit.cmn'
      dimension x(3)


      a=xscale(x(1),qi(1),qi(2))
      dr0=xscale(x(2),qi(3),qi(4))
      r0=xscale(x(3),qi(5),qi(6))
      f=0.d0
      do i=1,20
        r=qis(i)
        dum2=exp(-(r-r0)/dr0)
        dum1=qie(i)-a*dum2/(1+dum2)
        f=f+dum1*dum1
      enddo
c        write(*,*) a,dr0,r0,f
      return
      end





      subroutine ovldstrsc_limits(ipdb1,ipdb2)

      include 'quafit2014.cmn'
      dimension x(5)
      dimension xl(5),xh(5),g(5)
      external ovldstrsc_s

      irand(1)=ipdb1
      irand(2)=ipdb2
      do ii=1,2
      irand(3)=(-1)**ii
      do i=1,5
        x(i)=zer
        xl(i)=-pih
        xh(i)=pih
        g(i)=pih/3!0.1
      enddo
      ncicl=10
      nsub=10
      ti=500.d0
      tf=10.d0
      call sian(ovldstrsc_s,5,x,xl,xh,g,0.5d0,f,ti,tf,ncicl,nsub,
     >       tm,am,iseed)
      call simplx(ovldstrsc_s,x,f,5,1.d-10,itc,10000)
      ar=xscale(x(1),zer,pi2)
      br=dacos(xscale(x(2),-one,one))
      a2=xscale(x(3),zer,pi2)
      b2=dacos(xscale(x(4),-one,one))
      g2=xscale(x(5),zer,pi2)
      phiovl_l(ipdb1,ipdb2,ii)=f*irand(3) !! (,,1) max, (,,2) min
c      write(*,*) "phiovl ",ii,ipdb1,ipdb2,f*irand(3)

c###################################################################### plot
      if (ipdb1.eq.npdb+1.or.ipdb2.eq.npdb+1) return !Brazil october 2013

      write(text(1:1),'(i1)') ii

      open (unit=iu_bt2,status='unknown',file=
     >'qua'//code//'epsilon_'//text(1:1)//'.pdb')


      do isite=1,2
      if (isite.eq.1) then
        ipdb=ipdb1
        xt=zer
        yt=zer
        zt=zer
        aa=zer
        bb=zer
        gg=zer
      else
        ipdb=ipdb2
        call sigcal_new(ipdb1,ipdb2,zer,zer,zer,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)
        dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
        ur=dsmax-reddsmax
        xt=ur*dsin(br)*dcos(ar)
        yt=ur*dsin(br)*dsin(ar)
        zt=ur*dcos(br)
        aa=a2
        bb=b2
        gg=g2
c        write(*,*) "ur ",ur
      endif
      natot=0

      i66=0
      i66=i66+1
      call mat(-gg,-bb,-aa,rot)

      sum1=0.d0
      sum2=0.d0
      sum3=0.d0
      sum4=0.d0

      nato=0
  
      iname=lnfil(fsub(ipdb),300)
c      write(*,*) 'ddffsadfdsfa ',iu_pdb,ipdb,fsub(ipdb)
      open(unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:iname)
     >//'.pdb')
899   read(iu_pdb,'(a200)',end=810) text
      if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 899
      read(unit=text,fmt=1000,end=810) ina,ch8,dum1,dum2,dum3
     
      ch1=ch8(2:2)
      hc3=ch8(6:8)
      sum1=sum1+dum1
      sum2=sum2+dum2
      sum3=sum3+dum3
      nato=nato+1
      if (nato.gt.mxrun) stop 'nato > mxrun'
      coord(nato,1)=dum1
      coord(nato,2)=dum2
      coord(nato,3)=dum3
      goto 899
810    close(unit=iu_pdb)
      if (nato.eq.0) stop 'nato=0, sigplot'

c.....Trasla in modo che il centro coincida con il c.geom.
      dum1=1.d0/dfloat(nato)
      sum1=sum1*dum1
      sum2=sum2*dum1
      sum3=sum3*dum1

c.....Trasla in modo che il centro di massa della subunita' coincida
c.....con xt,yt,zt e poi ruota con la matrice rot

      do i=1,nato
        x1=coord(i,1)-sum1
        y1=coord(i,2)-sum2
        z1=coord(i,3)-sum3
c        write(*,*) x1,y1,z1,rx,ry,rz,xt,yt,zt,rot
c        pause
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        coord(i,1)=rx
        coord(i,2)=ry
        coord(i,3)=rz
      enddo

c#####Stampa sul pdb totale
          
      i=0
          
      open(unit=iu_pdb,status='unknown',
     >   file=fsub(ipdb)(1:iname)//'.pdb')
799   read(iu_pdb,'(a200)',end=709) text
c      write(*,*) text
      if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 799
      i=i+1
      natot=natot+1
      x1=coord(i,1)
      y1=coord(i,2)
      z1=coord(i,3)
      write(iu_bt2,1001) text(1:6),i,text(12:21),
     >       char(66+(-1)**(isite-1)),
     >               text(23:30),x1,y1,z1
      goto 799
709   close(unit=iu_pdb)
      

      enddo !isite
      close(unit=iu_bt2)


c######################################################################



      enddo 
      return
3000  format(
     >'    Geom. Center Vect.: modulus (ang.) :',1(f10.5),/,
     >'                        alpha   (deg) .:',1(f10.5),/,
     >'                        beta    (deg) .:',1(f10.5),/,
     >'    Rotation Angles:    alpha_a (deg) .:',1(f10.5),/,
     >'                        beta_a  (deg) .:',1(f10.5),/,
     >'                        gamma_a (deg) .:',1(f10.5))
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end

      subroutine ovldstrsc_s(npar,x,f)

      include 'quafit2014.cmn'
      dimension x(5)

      ar=xscale(x(1),zer,pi2)
      br=dacos(xscale(x(2),-one,one))
      a2=xscale(x(3),zer,pi2)
      b2=dacos(xscale(x(4),-one,one))
      g2=xscale(x(5),zer,pi2)

      ipdb1=irand(1)
      ipdb2=irand(2)
      call sigcal_new(ipdb1,ipdb2,zer,zer,zer,a2,b2,g2,ar,br
     >,ovlt,dstt,rsct)
      dsmax=dstt-rsct*log(pscale/(exp(-dstt/rsct)+1-pscale))
      phiovl=ovlt*rsct*log(
     > (exp(-dstt/rsct)+pscale*exp( tauscale/rsct)+1-pscale)
     >/(exp(-dstt/rsct)+pscale*exp(-tauscale/rsct)+1-pscale))
     >/(2*tauscale)
      f=phiovl*irand(3)

      return
      end





      subroutine for_confr_states(istate)

      include 'quafit2014.cmn'
      
c      call framhaglink(istate,1,1)


      jconf=1+4


      natot=0

      if (iflag_confr.eq.1.and.istate.eq.1) then !istate=1 e' quello che ruota
          call mat(
     >        -ggtmp
     >       ,-bbtmp
     >       ,-aatmp,roz)
      endif

      do ipoly=1,npoly(istate,1)
        do isub=1,nsymm(istate,1)

          do isite=1,nsite(istate)

          call mat(
     >        -gamrot(isite,isub,ipoly)
     >       ,-betrot(isite,isub,ipoly)
     >       ,-alprot(isite,isub,ipoly),rot)

          xt=centre(isite,isub,ipoly,1)
          yt=centre(isite,isub,ipoly,2)
          zt=centre(isite,isub,ipoly,3)
c      call cpolar(xt,yt,zt,r1yyy,a1yyy,b1yyy)
c          write(*,*) " KKK ",xt,yt,zt,r1yyy,a1yyy,b1yyy

          if (iflag_confr.eq.0.or.istate.eq.2) then
            if (istate.eq.1) nat(isite)=0
            
            ipdb=ipdbsite(istate,isite)
            iname=lnfil(fsub(ipdb),300)

            sum1=0.d0
            sum2=0.d0
            sum3=0.d0
            sum4=0.d0

            nato=0
  
            open(unit=iu_pdb,status='unknown',file=fsub(ipdb)(1:iname)
     >                                             //'.pdb')
99          read(iu_pdb,'(a200)',end=10) text
            if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 99
            read(unit=text,fmt=1000,end=10) ina,ch8,dum1,dum2,dum3
     
            ch1=ch8(2:2)
            hc3=ch8(6:8)
            sum1=sum1+dum1
            sum2=sum2+dum2
            sum3=sum3+dum3
            nato=nato+1
            if (nato.gt.mxrun) stop 'nato > mxrun'
            coord(nato,1)=dum1
            coord(nato,2)=dum2
            coord(nato,3)=dum3

            if (text(14:15).eq.'CA'.and.iflag_confr.eq.0.
     >and.istate.eq.1) then
              nat(isite)=nat(isite)+1
              if (nat(isite).gt.mxatconfr) 
     >stop 'nat(isite).gt.mxatconfr'
              coontt(isite,nat(isite),1)=dum1
              coontt(isite,nat(isite),2)=dum2
              coontt(isite,nat(isite),3)=dum3
            endif
               
            goto 99
10          close(unit=iu_pdb)
            if (nato.eq.0) stop 'nato=0'

c...........Trasla in modo che il centro coincida con il c.geom.
            dum1=1.d0/dfloat(nato)
            sum1=sum1*dum1
            sum2=sum2*dum1
            sum3=sum3*dum1


            if (iflag_confr.eq.0.and.istate.eq.1) then
              do itt=1,nat(isite)
                coontt(isite,itt,1)=coontt(isite,itt,1)-sum1
                coontt(isite,itt,2)=coontt(isite,itt,2)-sum2
                coontt(isite,itt,3)=coontt(isite,itt,3)-sum3
              enddo
            endif

c...........Trasla in modo che il centro di massa della subunita' coincida
c...........con xt,yt,zt e poi ruota con la matrice rot

            do 30 i=1,nato
              x1=coord(i,1)-sum1
              y1=coord(i,2)-sum2
              z1=coord(i,3)-sum3
c              write(*,*) x1,y1,z1,rx,ry,rz,xt,yt,zt,rot
c              pause
              call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
              coord(i,1)=rx
              coord(i,2)=ry
              coord(i,3)=rz
              
30          continue

c###########Stampa sul pdb totale
          
            i=0
          
            open(unit=iu_pdb,status='unknown',
     >         file=fsub(ipdb)(1:iname)//'.pdb')
999         read(iu_pdb,'(a200)',end=109) text
c            if (text(1:4).ne.'ATOM'.and.text(1:6).ne.'HETATM') goto 999
            if (text(1:4).ne.'ATOM') goto 999
            i=i+1
            x1=coord(i,1)
            y1=coord(i,2)
            z1=coord(i,3)
c            write(iu_bto,1001) text(1:6),i,text(12:21),
c     >             char(66+(-1)**i66),
c     >                     text(23:30),x1,y1,z1
c            if (text(14:15).eq.'CA'.or.text(14:15).eq.'N ') then
            if (text(14:15).eq.'CA') then
c            write(iu_bt1,1001) text(1:6),i,text(12:21),
c     >             char(66+(-1)**(isite-1)),
c     >                     text(23:30),x1,y1,z1
c#################### punto chiave
              natot=natot+1
              if (natot.gt.mxatconfr) stop 'natot > mxatconfr'
              cooconfr(istate,natot,1)=x1            
              cooconfr(istate,natot,2)=y1            
              cooconfr(istate,natot,3)=z1            
c              if (istate.eq.1) write(161,*) text,x1,y1,z1
c              if (istate.eq.2) write(162,*) text,x1,y1,z1
c#################### fine punto chiave
            endif
c            if (isub.eq.1) then
c            write(iu_bt2,1001) text(1:6),i,text(12:21),
c     >             char(66+(-1)**(isite-1)),
c     >                     text(23:30),x1,y1,z1
c            endif
            goto 999
109         close(unit=iu_pdb)

          else !

            do i=1,nat(isite)
              x1=coontt(isite,i,1)
              y1=coontt(isite,i,2)
              z1=coontt(isite,i,3)
              call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
              call rotrdi(rx,ry,rz,x1,y1,z1,rrxtmp,rrytmp,rrztmp,roz)
c              write(*,*) 'rot'
c              write(*,*) x1,y1,z1,rx,ry,rz,xt,yt,zt,rot
c              write(*,*) 'roz'
c              write(*,*) rx,ry,rz,x1,y1,z1,rrxtmp,rrytmp,rrztmp,roz

              natot=natot+1
              cooconfr(istate,natot,1)=x1
              cooconfr(istate,natot,2)=y1
              cooconfr(istate,natot,3)=z1
            enddo

          endif

          enddo !end isite
c###########linkers
c          goto 2021

          call mat(
     >        -gamrot_lnk(isub,ipoly)
     >       ,-betrot_lnk(isub,ipoly)
     >       ,-alprot_lnk(isub,ipoly),rot)

          xt=centre_lnk(isub,ipoly,1)
          yt=centre_lnk(isub,ipoly,2)
          zt=centre_lnk(isub,ipoly,3)
c          write(37,*) isub,ipoly
c     >       ,alprot_lnk(isub,ipoly)
c     >       ,gamrot_lnk(isub,ipoly)
c     >       ,betrot_lnk(isub,ipoly)
c          write(37,*) xt,yt,zt
cc          write(37,*) rot
c          pause
          do iilink=1,nlink(istate)
c            i66=i66+1
            call linker_final_coo(istate,iilink,1)

c...........Trasla 
c...........con xt,yt,zt e poi ruota con la matrice rot

            do i=1,nato
              x1=coord(i,1)
              y1=coord(i,2)
              z1=coord(i,3)
              call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
              coord(i,1)=rx
              coord(i,2)=ry
              coord(i,3)=rz  

              x1=coord(i,1)
              y1=coord(i,2)
              z1=coord(i,3)
c 1011 format(a6,i5,1x,a3,a2,a3,a2,i4,a4,3f8.3)
c              write(iu_bto,1011) 'ATOM  ',i,atypei(iato6(i)),
c     >          '  ',amm(iammch(i,1)),' '//char(66+(-1)**i66),
c     >          iato7(i),'    ',(coord(i,k),k=1,3)
c            if (atypei(iato6(i))(2:3).eq.'CA'.or.
c     >          atypei(iato6(i))(2:3).eq.'N ') then
            if (atypei(iato6(i))(2:3).eq.'CA') then
c            write(iu_bt1,1011) 'ATOM  ',i,atypei(iato6(i)),
c     >          '  ',amm(iammch(i,1)),' '//char(66+(-1)**i66),
c     >          iato7(i),'    ',(coord(i,k),k=1,3)
c#################### punto chiave
              natot=natot+1
              if (natot.gt.mxatconfr) stop 'natot > mxatconfr'
              cooconfr(istate,natot,1)=x1            
              cooconfr(istate,natot,2)=y1            
              cooconfr(istate,natot,3)=z1            
c#################### fine punto chiave
            endif
c            if (isub.eq.1) then
c            write(iu_bt2,1011) 'ATOM  ',i,atypei(iato6(i)),
c     >          '  ',amm(iammch(i,1)),' '//char(66+(-1)**i66),
c     >          iato7(i),'    ',(coord(i,k),k=1,3)
c            endif
            enddo
          enddo !iilink

c 2021     continue
        enddo   ! end isub
      enddo   !end ipoly

c      close(unit=iu_bto)
c      close(unit=iu_bt1)
c      close(unit=iu_bt2)
c      stop !! per ora

      if (iflag_confr.eq.0.or.istate.eq.2) natconfr(istate)=natot
      if (iflag_confr.eq.0) iflag_confr=1
      return

      
1001  format(a6,i5,a10,a1,a8,3(f8.3))
1000  format(6x,i5,1x,a8,10x,3(f8.3))
1100  format(10(e15.7))
1002  format(/,12(e15.7))
1101  format('na=',i4,'nb=',i4)
100   format(12(e15.7))
200   format(1x)
4200  format(20(e16.7))
      end


      subroutine confr_2_states

      include 'quafit2014.cmn'
      dimension x(mxsite*6+10)
      dimension h((mxsite*6+10)*(mxsite*6+10+1)/2),
     >xg(mxsite*6+10),w((mxsite*6+10)*3)
      dimension xl(mxsite*6+10),xh(mxsite*6+10)

      external min_2_state

      write(*,*) '   --->  confr_2_states '

      aatmp=zer
      bbtmp=zer
      ggtmp=zer
      rrxtmp=zer
      rrytmp=zer
      rrztmp=zer

      do istate=1,nstate
        call framhag(istate,1,1)
        call framalllink(istate,1,1)
        iflag_confr=0
        call for_confr_states(istate)
      enddo
      write(*,*) natconfr(1),natconfr(2)
      if (natconfr(1).ne.natconfr(2)) stop 'natconfr(1).ne.natconfr(2)'

      istate=1 !e' lo stato che si modifica

      aatmp=zer
      bbtmp=zer
      ggtmp=zer
      rrxtmp=zer
      rrytmp=zer
      rrztmp=zer
c      npar=nsite(1)*6+6!-3

2299  format(40x,g15.5E3)
      open(unit=iu_tmp,status='unknown',
     >       file='qua'//code//'_2_confr.out')
      read(iu_tmp,2299,err=21,end=21) rrxtmp
      read(iu_tmp,2299,err=21,end=21) rrytmp
      read(iu_tmp,2299,err=21,end=21) rrztmp
      read(iu_tmp,2299,err=21,end=21) dum1
      aatmp=dum1/conv
      read(iu_tmp,2299,err=21,end=21) dum1
      bbtmp=dum1/conv
      read(iu_tmp,2299,err=21,end=21) dum1
      ggtmp=dum1/conv


      read(iu_tmp,*)
      read(iu_tmp,*)

      read(iu_tmp,2299,err=21,end=21) dum1
      alpscrew(1,1,5)=dum1/conv
      read(iu_tmp,2299,err=21,end=21) cbescrew(1,1,5)
      read(iu_tmp,2299,err=21,end=21) dum1
      ascrew(1,1,5)=dum1/conv
      read(iu_tmp,2299,err=21,end=21) dscrew(1,1,5)


      jsite=1
      do isite=1,nsite(1)
      read(iu_tmp,*)
      read(iu_tmp,*)
      read(iu_tmp,*)


      jsite=jsite+1
      if (dnextsite(1,isite).ne.0..and.
     >isite.le.nsite(1)-1) then
      read(iu_tmp,2299,err=21,end=21) r1
      endif

      read(iu_tmp,2299,err=21,end=21) ums(1,isite,5)
      read(iu_tmp,2299,err=21,end=21) dum1
      aus(1,isite,5)=dum1/conv
      read(iu_tmp,2299,err=21,end=21) cbu(1,isite,5)
      read(iu_tmp,2299,err=21,end=21) dum1
      aas(1,isite,5)=dum1/conv
      read(iu_tmp,2299,err=21,end=21) cba(1,isite,5)
      read(iu_tmp,2299,err=21,end=21) dum1
      gas(1,isite,5)=dum1/conv

      enddo! end isite

 21   close(unit=iu_tmp)
      write(*,*)  rrxtmp
      write(*,*)  rrytmp
      write(*,*)  rrztmp
      write(*,*)  aatmp
      write(*,*)  bbtmp
      write(*,*)  ggtmp
      write(*,*)  alpscrew(1,1,5)
      write(*,*)  cbescrew(1,1,5)
      write(*,*)  ascrew(1,1,5)
      write(*,*)  dscrew(1,1,5)
      do isite=1,nsite(1)
      write(*,*) ums(1,isite,5)
      write(*,*) aus(1,isite,5)*conv
      write(*,*) cbu(1,isite,5)
      write(*,*) aas(1,isite,5)*conv
      write(*,*) cba(1,isite,5)
      write(*,*) gas(1,isite,5)*conv
      enddo
c      stop
c      if (npoly(1,1).gt.1) npar=npar+4!8

      i1=0
      if (npoly(istate,1).gt.1) then
      if (jalpscrew(istate,1).ne.0) then
      i1=i1+1
      x(i1)=xscinv(alpscrew(istate,1,5),-pi,pi)
      endif
c      write(*,*)       alpscrew(istate,1,5),i1,x(i1)
      if (jcbescrew(istate,1).ne.0) then
      i1=i1+1
      x(i1)=xscinv(cbescrew(istate,1,5),-one,one)
      endif
c      write(*,*)       cbescrew(istate,1,5),i1,x(i1)
      if (jascrew(istate,1).ne.0) then
        i1=i1+1
      x(i1)=xscinv(  ascrew(istate,1,5),-pi,pi)
      endif
c      write(*,*)         ascrew(istate,1,5),i1,x(i1)
      if (jdscrew(istate,1).ne.0) then
        i1=i1+1
      x(i1)=xscinv(dscrew(istate,1,5),zer,rscale2)
      endif
c      write(*,*)         dscrew(istate,1,5),i1,x(i1)
      endif



c      i1=i1+1
c      x(i1)=xscinv(tttmp,-pi,pi)
c      Write(*,*) tttmp,i1,x(i1)
c      i1=i1+1
c      x(i1)=xscinv(uuxtmp,-one,one)
c      i1=i1+1
c      x(i1)=xscinv(uuytmp,-one,one)
c      i1=i1+1
c      x(i1)=xscinv(uuztmp,-one,one)

      i1=i1+1
      x(i1)=xscinv(aatmp,-pi,pi)
c      write(*,*) aatmp,i1,x(i1)
      i1=i1+1
      x(i1)=xscinv(dcos(bbtmp),-one,one)
c      write(*,*) bbtmp,i1,x(i1)
      i1=i1+1
      x(i1)=xscinv(ggtmp,-pi,pi)
c      write(*,*) ggtmp,i1,x(i1)

c      rrxtmp=zer
c      rrytmp=zer
c      rrztmp=zer
      i1=i1+1
      x(i1)=xscinv(rrxtmp,-rscale2,rscale2)
c      write(*,*) rrxtmp,i1,x(i1)
      i1=i1+1
      x(i1)=xscinv(rrytmp,-rscale2,rscale2)
c      write(*,*) rrytmp,i1,x(i1)
      i1=i1+1
      x(i1)=xscinv(rrztmp,-rscale2,rscale2)
c      write(*,*) rrztmp,i1,x(i1)
      do isite=1,nsite(istate)
      if (jums(istate,isite).ne.0) then
      i1=i1+1
      x(i1)=xscinv(ums(istate,isite,5),zer,rscale2)
      endif
c      write(*,*)      ums(istate,isite,5),i1,x(i1)
      if (jaus(istate,isite).ne.0) then
      i1=i1+1
      x(i1)=xscinv(aus(istate,isite,5),-pi,pi)
      endif
c      write(*,*)       aus(istate,isite,5),i1,x(i1)
      if (jcbu(istate,isite).ne.0) then
      i1=i1+1
      x(i1)=xscinv(cbu(istate,isite,5),-one,one)
      endif
c      write(*,*)       cbu(istate,isite,5),i1,x(i1)
      if (jaas(istate,isite).ne.0) then
      i1=i1+1
      x(i1)=xscinv(aas(istate,isite,5),-pi,pi)
      endif
c      write(*,*)       aas(istate,isite,5),i1,x(i1)
      if (jcba(istate,isite).ne.0) then
      i1=i1+1
      x(i1)=xscinv(cba(istate,isite,5),-one,one)
      endif
c      write(*,*)       cba(istate,isite,5),i1,x(i1)
      if (jgas(istate,isite).ne.0) then
      i1=i1+1
      x(i1)=xscinv(gas(istate,isite,5),-pi,pi)
      endif
c      write(*,*)       gas(istate,isite,5),i1,x(i1)
      enddo
c      if (i1.ne.npar) stop 'i1.ne.npar'
      npar=i1

      do i=1,npar
        xl(i)=-pih
        xh(i)=pih
        xg(i)=0.1
      enddo
      ic=0
      icmax=2000000
      iflag2=0
      call  min_2_state(npar,x,f)
      ncicl=20
      nsub=500
c      stop
c      do kk=1,1
 2020 continue
      call monk(min_2_state,x,f,npar,1000,iseed)
 

      call simplx(min_2_state,x,f,npar,1.d-20,itc,5000000) !questa chiamata sembra sia indispensabile oct/2015
      ti=f*2
      tf=f*1.e-4
      call sian(min_2_state,npar,x,xl,xh,xg,0.5d0,f,ti,tf,ncicl,nsub,
     >       tm,am,iseed)
      call simplx(min_2_state,x,f,npar,1.d-40,itc,200000)
      call zxmin(min_2_state,npar,6,200000,3,x,h,xg,f,w,ierr)
c      if (f.gt.1.e-2.and.ic.le.icmax) goto 2020
      
c      enddo
      iflag2=1
      call  min_2_state(npar,x,f)

      open(unit=iu_tmp,status='unknown',
     >       file='qua'//code//'_2_confr.out')


      string=' Final Assembly Translation (Ang.)    X:'
      write(iu_tmp,2200) string,rrxtmp
      string='                                      Y:'
      write(iu_tmp,2200) string,rrytmp
      string='                                      Z:'
      write(iu_tmp,2200) string,rrztmp

      string=' Final Assembly Orientation ....: alpha:'
      write(iu_tmp,2200) string,aatmp*conv
      string='                           (deg)  beta :'
      write(iu_tmp,2200) string,bbtmp*conv
      string='                                  gamma:'
      write(iu_tmp,2200) string,ggtmp*conv
      string=' Screw Polymerization Number ..........:'
      write(iu_tmp,2100) string,npoly(1,1)
      string='                                       ['
      write(iu_tmp,4100) string


      string=' Screw Axis Orientation: alpha (deg) ..:'
      write(iu_tmp,2207) string,conv*alpscrew(1,1,5),
     >     (conv*alpscrew(1,1,i),i=2,4)
     >     ,jalpscrew(1,1)
      string='                         cos(beta) ....:'
      write(iu_tmp,2207) string,cbescrew(1,1,5),
     >     (cbescrew(1,1,i),i=2,4)
     >     ,jcbescrew(1,1)

      string=' Rotation along Screw Axis (deg) ......:'

      write(iu_tmp,2207) string,conv*ascrew(1,1,5),
     >     (conv*ascrew(1,1,i),i=2,4)
     >     ,jascrew(1,1)
      string=' Translation along Screw Axis (Ang.) ..:'
      write(iu_tmp,2207) string,dscrew(1,1,5),
     >     (dscrew(1,1,i),i=2,4)
     >     ,jdscrew(1,1)


      jsite=1
      do isite=1,nsite(1)

      string='*************************** Rigid Domain'
      write(iu_tmp,4100) string
      string='    RD PDB File Name (without .pdb) ...:'
      write(iu_tmp,4100) string,fsub(ipdbsite(1,isite))


      jsite=jsite+1
      if (dnextsite(1,isite).ne.0..and.
     >isite.le.nsite(1)-1) then
      rx=xrrot(1,isite)-xlrot(1,jsite)
      ry=yrrot(1,isite)-ylrot(1,jsite)
      rz=zrrot(1,isite)-zlrot(1,jsite)
      call cpolar(rx,ry,rz,r1,a1,b1)
      string='    Next RD Final Distance ............:'
      write(iu_tmp,2200) string,r1
      endif

      string='                                       ['
      write(iu_tmp,4100) string
      string='    Geom. Center Vect.: modulus (ang.) :'
      write(iu_tmp,2207) string,ums(1,isite,5),
     >     (ums(1,isite,i),i=2,4)
     >     ,jums(1,isite)
      string='                        alpha   (deg) .:'
      write(iu_tmp,2207) string,conv*aus(1,isite,5),
     >     (conv*aus(1,isite,i),i=2,4)
     >     ,jaus(1,isite)
      string='                        cos(beta) .....:'
      write(iu_tmp,2207) string,cbu(1,isite,5),
     >     (cbu(1,isite,i),i=2,4)
     >     ,jcbu(1,isite)
      string='    Rotation Angles:    alpha_a (deg) .:'
      write(iu_tmp,2207) string,conv*aas(1,isite,5),
     >     (conv*aas(1,isite,i),i=2,4)
     >     ,jaas(1,isite)
      string='                        cos(beta_a) ...:'
      write(iu_tmp,2207) string,cba(1,isite,5),
     >     (cba(1,isite,i),i=2,4)
     >     ,jcba(1,isite)
      string='                        gamma_a (deg) .:'
      write(iu_tmp,2207) string,conv*gas(1,isite,5),
     >     (conv*gas(1,isite,i),i=2,4)
     >     ,jgas(1,isite)

      enddo! end isite
      string=' Final RMSD (Angs) ....................:'
      write(iu_tmp,2207) string,f


      close(unit=iu_tmp)
      subcode='cfr'
      call outparz
      do istate=1,nstate
        call framhag(istate,1,1)
        call framalllink(istate,1,1)
        call btoms(istate,1,1)
      enddo


      return
 4101 format(a40,300a1)
4100  format(a40,a300)
2100  format(a40,i10)
2101  format(a40,a4)
2200  format(a40,g15.5E3)
 2209 format(a40,50000(g15.5E3,1x))
2207  format(a40,4(g15.5E3,1x),i4,1x,a10000)
 4500 format(a1,100(a8,i1,a2))
4012  format(' Weight of state .............,,.......:',g15.5E3)
4001  format(' Scale Factor kappa ...................:',g15.5E3)
4002  format(' Flat Background B ....................:',g15.5E3)
4003  format(' Scale Factor of Deoxy data ...........:',g15.5E3)
      end

      subroutine min_2_state(npar,x,f)

      include 'quafit2014.cmn'
      dimension x(mxsite*6+10)


      istate=1 !e' lo stato che si modifica

      i1=0
      if (npoly(istate,1).gt.1) then
      if (jalpscrew(istate,1).ne.0) then
      i1=i1+1
      alpscrew(istate,1,5)=xscale(x(i1),-pi,pi)
      endif
c      write(*,*)       alpscrew(istate,1,5),i1,x(i1)
      if (jcbescrew(istate,1).ne.0) then
      i1=i1+1
      cbescrew(istate,1,5)=xscale(x(i1),-one,one)
      endif
c      write(*,*)       cbescrew(istate,1,5),i1,x(i1)
      if (jascrew(istate,1).ne.0) then
        i1=i1+1
        ascrew(istate,1,5)=xscale(x(i1),-pi,pi)
      endif
c      write(*,*)         ascrew(istate,1,5),i1,x(i1)
      if (jdscrew(istate,1).ne.0) then
        i1=i1+1
        dscrew(istate,1,5)=xscale(x(i1),zer,rscale2)
      endif
c      write(*,*)         dscrew(istate,1,5),i1,x(i1)
      endif

c      i1=i1+1
c      tttmp=xscale(x(i1),-pi,pi)
c      i1=i1+1
c      uuxtmp=xscale(x(i1),-one,one)
c      i1=i1+1
c      uuytmp=xscale(x(i1),-one,one)
c      i1=i1+1
c      uuztmp=xscale(x(i1),-one,one)
      
c      dum1=(uuxtmp**2+uuytmp**2+uuztmp**2)**0.5
c      a_x=uuxtmp/dum1
c      a_y=uuytmp/dum1
c      a_z=uuztmp/dum1
c      caatmp=dcos(tttmp)
c      saatmp=dsin(tttmp)
c      tor(1,1)=a_x**2*(1-caatmp)+caatmp
c      tor(1,2)=a_x*a_y*(1-caatmp)-a_z*saatmp
c      tor(1,3)=a_x*a_z*(1-caatmp)+a_y*saatmp
c      tor(2,1)=a_y*a_x*(1-caatmp)+a_z*saatmp
c      tor(2,2)=a_y**2*(1-caatmp)+caatmp
c      tor(2,3)=a_y*a_z*(1-caatmp)-a_x*saatmp
c      tor(3,1)=a_z*a_x*(1-caatmp)-a_y*saatmp
c      tor(3,2)=a_z*a_y*(1-caatmp)+a_x*saatmp
c      tor(3,3)=a_z**2*(1-caatmp)+caatmp
c            call taminv(dum1,dum2,dum3,tor)
c            aatmp=dum1
c            bbtmp=dum2
c            ggtmp=dum3


      i1=i1+1
      aatmp=xscale(x(i1),-pi,pi)
c      write(*,*) aatmp,i1,x(i1)
      i1=i1+1
      dum1=xscale(x(i1),-one,one)
c      if (dabs(dum1).lt.tlittle) dum1=0.d0
c      if (dabs(1-dum1).lt.tlittle) dum1=1.d0
c      if (dabs(-1-dum1).lt.tlittle) dum1=-1.d0
c      write(*,*) bbtmp,i1,x(i1)
      bbtmp=dacos(dum1)
      i1=i1+1
      ggtmp=xscale(x(i1),-pi,pi)
c      write(*,*) ggtmp,i1,x(i1)

      i1=i1+1
      rrxtmp=xscale(x(i1),-rscale2,rscale2)
c      write(*,*) rrxtmp,i1,x(i1)
      i1=i1+1
      rrytmp=xscale(x(i1),-rscale2,rscale2)
c      write(*,*) rrytmp,i1,x(i1)
      i1=i1+1
      rrztmp=xscale(x(i1),-rscale2,rscale2)
c      write(*,*) rrztmp,i1,x(i1)
      do isite=1,nsite(istate)
      if (jums(istate,isite).ne.0) then
      i1=i1+1
      ums(istate,isite,5)=xscale(x(i1),zer,rscale2)
      endif
c      write(*,*)      ums(istate,isite,5),i1,x(i1)
      if (jaus(istate,isite).ne.0) then
      i1=i1+1
      aus(istate,isite,5)=xscale(x(i1),-pi,pi)
      endif
c      write(*,*)       aus(istate,isite,5),i1,x(i1)
      if (jcbu(istate,isite).ne.0) then
      i1=i1+1
      dum1=xscale(x(i1),-one,one)
c      if (dabs(dum1).lt.tlittle) dum1=0.d0
c      if (dabs(1-dum1).lt.tlittle) dum1=1.d0
c      if (dabs(-1-dum1).lt.tlittle) dum1=-1.d0
      cbu(istate,isite,5)=dum1
      endif
c      write(*,*)       cbu(istate,isite,5),i1,x(i1)
      if (jaas(istate,isite).ne.0) then
      i1=i1+1
      aas(istate,isite,5)=xscale(x(i1),-pi,pi)
      endif
c      write(*,*)       aas(istate,isite,5),i1,x(i1)
      if (jcba(istate,isite).ne.0) then
      i1=i1+1
      dum1=xscale(x(i1),-one,one)
c      if (dabs(dum1).lt.tlittle) dum1=0.d0
c      if (dabs(1-dum1).lt.tlittle) dum1=1.d0
c      if (dabs(-1-dum1).lt.tlittle) dum1=-1.d0
      cba(istate,isite,5)=dum1
      endif
c      write(*,*)       cba(istate,isite,5),i1,x(i1)
      if (jgas(istate,isite).ne.0) then
      i1=i1+1
      gas(istate,isite,5)=xscale(x(i1),-pi,pi)
      endif
c      write(*,*)       gas(istate,isite,5),i1,x(i1)
c      stop

c#########
c      aatmp=-gas(istate,isite,5)
c      ggtmp=-aas(istate,isite,5)
c      bbtmp=-acos(cba(istate,isite,5))
c      aatmp=aas(istate,isite,5)
c      ggtmp=gas(istate,isite,5)
c      bbtmp=acos(cba(istate,isite,5))
c#########

      enddo
      if (i1.ne.npar) stop 'i1.ne.npar two'
      call framhag(1,1,1)
      call framalllink(1,1,1)
      call for_confr_states(1)

      f2=zer
      if (iconfr_rot.eq.1) then
        i2=0
        do isite=1,nsite(istate)-1
        do jsite=isite+1,nsite(istate)
          dum1=aas(istate,isite,5)-aas(istate,jsite,5)
          f2=f2+dum1**2
          dum1=cba(istate,isite,5)-cba(istate,jsite,5)
          f2=f2+dum1**2
          dum1=gas(istate,isite,5)-gas(istate,jsite,5)
          f2=f2+dum1**2
          i2=i2+3
        enddo
        enddo
        f2=(f2/i2)**0.5*1.e2
      else if (iconfr_rot.eq.2) then
        ns=nsite(istate)/2
        do k=1,2
        if (k.eq.1) then
          ns1=1
          ns2=ns
        else
          ns1=ns+1
          ns2=ns*2
        endif
        i2=0
        do isite=ns1,ns2-1
        do jsite=isite+1,ns2
          dum1=aas(istate,isite,5)-aas(istate,jsite,5)
          f2=f2+dum1**2
          dum1=cba(istate,isite,5)-cba(istate,jsite,5)
          f2=f2+dum1**2
          dum1=gas(istate,isite,5)-gas(istate,jsite,5)
          f2=f2+dum1**2
          i2=i2+3
        enddo
        enddo
        enddo
        f2=(f2/i2)**0.5*1.e2
      endif

      f=zer
      natot=natconfr(1)
      do i=1,natot
        do k=1,3
          dum1=cooconfr(1,i,k)-cooconfr(2,i,k)
c          write(*,*) i,k,cooconfr(1,i,k),cooconfr(2,i,k)
          f=f+dum1*dum1
        enddo
        if (iflag2.eq.1) then
        write(151,*) (cooconfr(1,i,k),k=1,3)
        write(152,*) (cooconfr(2,i,k),k=1,3)
        endif
      enddo
c      stop
      f=(f/natot)**0.5
      f=f+f2
      ic=ic+1
      if (mod(ic,10000).eq.0) write(*,*) ' f,f2  ',f-f2,f2

      return
      end



      subroutine findatype(ch,i1,i2,ia)
c.....finds the atype in a text string
      include 'quafit2014.cmn'
      character*(*) ch
      parameter(ln=3,lp=21)!ln length, lp possibilites
      if (i2-i1+1.lt.ln) then
        write(*,*) "error in findatype"
        stop
      endif
      do i=i1,i2-ln+1
        do j=1,lp
          if (ch(i:i+ln-1).eq.atypei(j)) then
            ia=j
            return
          endif
        enddo
      enddo
      write(*,*) "Constraint: wrong Atom symbol"
      stop
      end

      subroutine findcoord(ch,i1,i2,ia)
c.....finds the coord type in a text string
      include 'quafit2014.cmn'
      character*(*) ch
      parameter(ln=2,lp=8)!ln length, lp possibilites
      if (i2-i1+1.lt.ln) then
        write(*,*) "error in findcoord"
        stop
      endif
      do i=i1,i2-ln+1
        do j=1,lp
          if (ch(i:i+ln-1).eq.coordtype(j)) then
            ia=j
            return
          endif
        enddo
      enddo
      ia=0
      return
      end


      subroutine findcond(ch,i1,i2,ia)
c.....finds the condition type in a text string
      include 'quafit2014.cmn'
      character*(*) ch
      parameter(ln=3,lp=7)!ln length, lp possibilites
      if (i2-i1+1.lt.ln) then
        write(*,*) "error in findcond"
        stop
      endif
      do i=i1,i2-ln+1
        do j=1,lp
          if (ch(i:i+ln-1).eq.condtype(j)) then
            ia=j
            return
          endif
        enddo
      enddo
      write(*,*) "Constraint: wrong condition symbol"
      stop
      end


      subroutine findmeth(ch,i1,i2,ia)
c.....finds the method type in a text string
      include 'quafit2014.cmn'
      character*(*) ch
      parameter(ln=2,lp=2)!ln length, lp possibilites
      if (i2-i1+1.lt.ln) then
        write(*,*) "error in findmeth"
        stop
      endif
      do i=i1,i2-ln+1
        do j=1,lp
          if (ch(i:i+ln-1).eq.methtype(j)) then
            ia=j
            return
          endif
        enddo
      enddo
      write(*,*) "Constraint: wrong method symbol"
      stop
      end


      subroutine constr_site_rot(istate,isite)

      include 'quafit2014.cmn'
c.....a
      n=isitlnkconstr_a(istate,isite,1)
      do kk=1,n
        nc=isitlnkconstr_a(istate,isite,1+kk)
        isub=iconstr_a(istate,nc,3)
        ipoly=iconstr_a(istate,nc,4)
        call mat(
     >      -gamrot(isite,isub,ipoly)
     >     ,-betrot(isite,isub,ipoly)
     >     ,-alprot(isite,isub,ipoly),rot)
        xt=centre(isite,isub,ipoly,1)
        yt=centre(isite,isub,ipoly,2)
        zt=centre(isite,isub,ipoly,3)
        x1=cooconstr_a(istate,nc,1)
        y1=cooconstr_a(istate,nc,2)
        z1=cooconstr_a(istate,nc,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooconstr_a_rot(istate,nc,1)=rx
        cooconstr_a_rot(istate,nc,2)=ry
        cooconstr_a_rot(istate,nc,3)=rz
        cooconstr_a_rot(istate,nc,4)=xt!! per il centro
        cooconstr_a_rot(istate,nc,5)=yt
        cooconstr_a_rot(istate,nc,6)=zt
      enddo
c.....b
      n=isitlnkconstr_b(istate,isite,1)
      do kk=1,n
        nc=isitlnkconstr_b(istate,isite,1+kk)
        ie=isitlnkconstr_b(istate,isite,1+mxconstr+kk)
        isub=iconstr_b(istate,nc,3+(ie-1)*5)
        ipoly=iconstr_b(istate,nc,4+(ie-1)*5)
        call mat(
     >      -gamrot(isite,isub,ipoly)
     >     ,-betrot(isite,isub,ipoly)
     >     ,-alprot(isite,isub,ipoly),rot)
        xt=centre(isite,isub,ipoly,1)
        yt=centre(isite,isub,ipoly,2)
        zt=centre(isite,isub,ipoly,3)
        x1=cooconstr_b(istate,nc,ie,1)
        y1=cooconstr_b(istate,nc,ie,2)
        z1=cooconstr_b(istate,nc,ie,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooconstr_b_rot(istate,nc,ie,1)=rx
        cooconstr_b_rot(istate,nc,ie,2)=ry
        cooconstr_b_rot(istate,nc,ie,3)=rz
        cooconstr_b_rot(istate,nc,ie,4)=xt !! per il centro
        cooconstr_b_rot(istate,nc,ie,5)=yt
        cooconstr_b_rot(istate,nc,ie,6)=zt
      enddo
      return
      end
      

      subroutine constr_link_rot(istate,iilink)

      include 'quafit2014.cmn'
      i2=nsite(istate)+iilink
c.....a
      n=isitlnkconstr_a(istate,i2,1)
      do kk=1,n
        nc=isitlnkconstr_a(istate,i2,1+kk)
        isub=iconstr_a(istate,nc,3)
        ipoly=iconstr_a(istate,nc,4)
        call mat(
     >      -gamrot_lnk(isub,ipoly)
     >     ,-betrot_lnk(isub,ipoly)
     >     ,-alprot_lnk(isub,ipoly),rot)
        xt=centre_lnk(isub,ipoly,1)
        yt=centre_lnk(isub,ipoly,2)
        zt=centre_lnk(isub,ipoly,3)
        x1=cooconstr_a(istate,nc,1)
        y1=cooconstr_a(istate,nc,2)
        z1=cooconstr_a(istate,nc,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooconstr_a_rot(istate,nc,1)=rx
        cooconstr_a_rot(istate,nc,2)=ry
        cooconstr_a_rot(istate,nc,3)=rz
        cooconstr_a_rot(istate,nc,4)=xt!! per il centro
        cooconstr_a_rot(istate,nc,5)=yt
        cooconstr_a_rot(istate,nc,6)=zt
      enddo
c.....b
      n=isitlnkconstr_b(istate,i2,1)
      do kk=1,n
        nc=isitlnkconstr_b(istate,i2,1+kk)
        ie=isitlnkconstr_b(istate,i2,1+mxconstr+kk)
        isub=iconstr_b(istate,nc,3+(ie-1)*5)
        ipoly=iconstr_b(istate,nc,4+(ie-1)*5)
        call mat(
     >      -gamrot_lnk(isub,ipoly)
     >     ,-betrot_lnk(isub,ipoly)
     >     ,-alprot_lnk(isub,ipoly),rot)
        xt=centre_lnk(isub,ipoly,1)
        yt=centre_lnk(isub,ipoly,2)
        zt=centre_lnk(isub,ipoly,3)
        x1=cooconstr_b(istate,nc,ie,1)
        y1=cooconstr_b(istate,nc,ie,2)
        z1=cooconstr_b(istate,nc,ie,3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt,yt,zt,rot)
        cooconstr_b_rot(istate,nc,ie,1)=rx
        cooconstr_b_rot(istate,nc,ie,2)=ry
        cooconstr_b_rot(istate,nc,ie,3)=rz
        cooconstr_b_rot(istate,nc,ie,4)=xt !! per il centro
        cooconstr_b_rot(istate,nc,ie,5)=yt
        cooconstr_b_rot(istate,nc,ie,6)=zt
      enddo
      return
      end
      



      subroutine test_constr(istate,iconf) 

      include 'quafit2014.cmn'
      dimension coord_t(mxconstr),coord_ud(mxconstr)

c.....Gia' chiamata la framhag
c.....Gia' chiamata la framalllink

      dum2=0.d0
c.....a
      m=nconstr_a(istate)
      do nc=1,m
        icoord=iconstr_a(istate,nc,6) !Coord.vector r_i ->x,y,z,r,a,b,rc,a
c        icond=iconstr_a(istate,nc,7) !=,>,>=,<,<=,max,min
        j1=jconstr_a(istate,nc,1) !1=u_d, 2 = same of nc2=jconstr_a(istate,nc,2), 3= value
c        j2=jconstr_a(istate,nc,2)
        if (icoord.le.3) then !cartesian
          coord_t(nc)=cooconstr_a_rot(istate,nc,icoord)
        else if (icoord.le.6) then !polar
          call cpolar(cooconstr_a_rot(istate,nc,1),
     >                cooconstr_a_rot(istate,nc,2),
     >                cooconstr_a_rot(istate,nc,3),r,alpr,betr)
          if (icoord.eq.4) then
            coord_t(nc)=r
          else if (icoord.eq.5) then
            coord_t(nc)=alpr
          else
            coord_t(nc)=betr
          endif
        else !cylinder
          call cpolar(cooconstr_a_rot(istate,nc,1),
     >                cooconstr_a_rot(istate,nc,2),
     >                0.d0,r,alpr,betr)
c          write(*,*) ' cccc ',cooconstr_a_rot(istate,nc,1),
c     >                cooconstr_a_rot(istate,nc,2),
c     >                0.d0,r,alpr,betr
          if (icoord.eq.7) then
            coord_t(nc)=r
          else
            coord_t(nc)=alpr
          endif
        endif
        if (j1.eq.1) then !udd
        icoord=iconstr_a(istate,nc,8) !Coord.vector r_i ->x,y,z,r,a,b,rc,a
        if (icoord.le.3) then !cartesian
          coord_ud(nc)=cooconstr_a_rot(istate,nc,icoord+3)
        else if (icoord.le.6) then !polar
          call cpolar(cooconstr_a_rot(istate,nc,4),
     >                cooconstr_a_rot(istate,nc,5),
     >                cooconstr_a_rot(istate,nc,6),r,alpr,betr)
          if (icoord.eq.4) then
            coord_ud(nc)=r
          else if (icoord.eq.5) then
            coord_ud(nc)=alpr
          else
            coord_ud(nc)=betr
          endif
        else !cylinder
          call cpolar(cooconstr_a_rot(istate,nc,4),
     >                cooconstr_a_rot(istate,nc,5),
     >                0.d0,r,alpr,betr)
          if (icoord.eq.7) then
            coord_ud(nc)=r
          else
            coord_ud(nc)=alpr
          endif
        endif
        endif!j1 
      enddo
c.....a again
      do nc=1,m
        icond=iconstr_a(istate,nc,7) !=,>,>=,<,<=,max,min
        j1=jconstr_a(istate,nc,1) !1=u_d, 2 = same of nc2=jconstr_a(istate,nc,2), 3= value
        j2=jconstr_a(istate,nc,2)
        imeth=iconstr_a(istate,nc,9)
        v=vconstr_a(istate,nc)
        f=fconstr_a(istate,nc)
        c=cconstr_a(istate,nc)
        dum3=coord_t(nc)
        test=0.d0
        if (icond.le.5) then !un confronto
          if (j1.eq.1) then
            dum1=coord_ud(nc)
          else if (j1.eq.2) then
            dum1=coord_t(j2)
          else
            dum1=v
          endif
          dum1=dum1*f
          i4=0
          if(icond.eq.1.and.dum3.ne.dum1)i4=1
          if(icond.eq.2.and.dum3.le.dum1)i4=1
          if(icond.eq.3.and.dum3.lt.dum1)i4=1
          if(icond.eq.4.and.dum3.ge.dum1)i4=1
          if(icond.eq.5.and.dum3.gt.dum1)i4=1
          if (i4.eq.1) then
            if (imeth.eq.1) then !step
              test=c
            else if (imeth.eq.2) then !elastic
               test=c*(dum3-dum1)**2
            endif
          endif
        else 
          if (icond.eq.6) then !min
            dum1=1.e30
            do nc1=1,m
              dum1=min(dum1,coord_t(nc1))
            enddo
          else if (icond.eq.7) then !min
            dum1=-1.e30
            do nc1=1,m
              dum1=max(dum1,coord_t(nc1))
            enddo
          endif
          if (imeth.eq.1) then !step
            if(dum3.ne.dum1) test=c
          else if (imeth.eq.2) then !elastic
            if(dum3.ne.dum1) test=c*(dum3-dum1)**2
          endif
        endif
        constr_a(istate,iconf,nc,1)=dum3
        constr_a(istate,iconf,nc,2)=dum1
        constr_a(istate,iconf,nc,3)=test
        dum2=dum2+test
      enddo !nc
c.....b
      m=nconstr_b(istate)
      do nc=1,m
        icoord=iconstr_b(istate,nc,11) !Coord.vector r_i ->x,y,z,r,a,b,rc,a
c        icond=iconstr_b(istate,nc,12) !=,>,>=,<,<=,max,min
        j1=jconstr_b(istate,nc,1) !1=u_d, 2 = same of nc2=jconstr_b(istate,nc,2), 3= value
c        j2=jconstr_b(istate,nc,2)
        if (icoord.le.3) then !cartesian
          coord_t(nc)=cooconstr_b_rot(istate,nc,1,icoord)
     >             -cooconstr_b_rot(istate,nc,2,icoord)
        else if (icoord.le.6) then !polar
          call cpolar(cooconstr_b_rot(istate,nc,1,1)
     >               -cooconstr_b_rot(istate,nc,2,1),
     >                cooconstr_b_rot(istate,nc,1,2)
     >               -cooconstr_b_rot(istate,nc,2,2),
     >                cooconstr_b_rot(istate,nc,1,3)
     >               -cooconstr_b_rot(istate,nc,2,3),r,alpr,betr)
          if (icoord.eq.4) then
            coord_t(nc)=r
          else if (icoord.eq.5) then
            coord_t(nc)=alpr
          else
            coord_t(nc)=betr
          endif
        else !cylinder
          call cpolar(cooconstr_b_rot(istate,nc,1,1)
     >               -cooconstr_b_rot(istate,nc,2,1),
     >                cooconstr_b_rot(istate,nc,1,2)
     >               -cooconstr_b_rot(istate,nc,2,2),
     >                0.d0,r,alpr,betr)
          if (icoord.eq.7) then
            coord_t(nc)=r
          else
            coord_t(nc)=alpr
          endif
        endif
        if (j1.eq.1) then !udd
        icoord=iconstr_b(istate,nc,13) !Coord.vector r_i ->x,y,z,r,a,b,rc,a
        if (icoord.le.3) then !cartesian
          coord_ud(nc)=cooconstr_b_rot(istate,nc,1,icoord+3)
     >                -cooconstr_b_rot(istate,nc,2,icoord+3)
        else if (icoord.le.6) then !polar
          call cpolar(cooconstr_b_rot(istate,nc,1,4)
     >               -cooconstr_b_rot(istate,nc,2,4),
     >                cooconstr_b_rot(istate,nc,1,5)
     >               -cooconstr_b_rot(istate,nc,2,5),
     >                cooconstr_b_rot(istate,nc,1,6)
     >               -cooconstr_b_rot(istate,nc,2,6),r,alpr,betr)
          if (icoord.eq.4) then
            coord_ud(nc)=r
          else if (icoord.eq.5) then
            coord_ud(nc)=alpr
          else
            coord_ud(nc)=betr
          endif
        else !cylinder
          call cpolar(cooconstr_b_rot(istate,nc,1,4)
     >               -cooconstr_b_rot(istate,nc,2,4),
     >                cooconstr_b_rot(istate,nc,1,5)
     >               -cooconstr_b_rot(istate,nc,2,5),
     >                0.d0,r,alpr,betr)
          if (icoord.eq.7) then
            coord_ud(nc)=r
          else
            coord_ud(nc)=alpr
          endif
        endif
        endif!j1 
      enddo
c.....b again
      do nc=1,m
        icond=iconstr_b(istate,nc,12) !=,>,>=,<,<=,max,min
        j1=jconstr_b(istate,nc,1) !1=u_d, 2 = same of nc2=jconstr_b(istate,nc,2), 3= value
        j2=jconstr_b(istate,nc,2)
        imeth=iconstr_b(istate,nc,14)
        v=vconstr_b(istate,nc)
        f=fconstr_b(istate,nc)
        c=cconstr_b(istate,nc)
        dum3=coord_t(nc)
        test=0.d0
        if (icond.le.5) then !un confronto
          if (j1.eq.1) then
            dum1=coord_ud(nc)
          else if (j1.eq.2) then
            dum1=coord_t(j2)
          else
            dum1=v
          endif
          dum1=dum1*f
          i4=0
          if(icond.eq.1.and.dum3.ne.dum1)i4=1
          if(icond.eq.2.and.dum3.le.dum1)i4=1
          if(icond.eq.3.and.dum3.lt.dum1)i4=1
          if(icond.eq.4.and.dum3.ge.dum1)i4=1
          if(icond.eq.5.and.dum3.gt.dum1)i4=1
          if (i4.eq.1) then
            if (imeth.eq.1) then !step
              test=c
            else if (imeth.eq.2) then !elastic
               test=c*(dum3-dum1)**2
            endif
          endif
        else 
          if (icond.eq.6) then !min
            dum1=1.e30
            do nc1=1,m
              dum1=min(dum1,coord_t(nc1))
            enddo
          else if (icond.eq.7) then !min
            dum1=-1.e30
            do nc1=1,m
              dum1=max(dum1,coord_t(nc1))
            enddo
          endif
          if (imeth.eq.1) then !step
            if(dum3.ne.dum1) test=c
          else if (imeth.eq.2) then !elastic
            if(dum3.ne.dum1) test=c*(dum3-dum1)**2
          endif
        endif
        constr_b(istate,iconf,nc,1)=dum3
        constr_b(istate,iconf,nc,2)=dum1
        constr_b(istate,iconf,nc,3)=test
        dum2=dum2+test
      enddo !nc
      cverl(istate,iconf)=dum2
      return
      end


      subroutine check_constr

      include 'quafit2014.cmn'

      do istate=1,nstate
        m=nconstr_a(istate)
        do nc=1,m
          isub=iconstr_a(istate,nc,3)
          if (isub.lt.0.or.isub.gt.nsymm(istate,1)) then
            write(*,*) "Constraints_a: SS > Maximum",isub
            stop
          endif
          ipoly=iconstr_a(istate,nc,4)
          if (ipoly.lt.0.or.ipoly.gt.npoly(istate,1)) then 
            write(*,*) "Constraints_a: POL > Maximum",ie,ipoly
            stop
          endif
        enddo
        m=nconstr_b(istate)
        do nc=1,m
          do ie=1,2
          isub=iconstr_b(istate,nc,3+(ie-1)*5)
          if (isub.lt.0.or.isub.gt.nsymm(istate,1)) then
            write(*,*) "Constraints_b: SS > Maximum",ie,isub
            stop
          endif
          ipoly=iconstr_b(istate,nc,4+(ie-1)*5)
          if (ipoly.lt.0.or.ipoly.gt.npoly(istate,1)) then 
            write(*,*) "Constraints_b: POL > Maximum",ie,ipoly
            stop
          endif
          enddo
        enddo
      enddo
      return
      end


      subroutine ca_refiniment(istate,isite)

      include 'quafit2014.cmn'
      dimension coord2(mxrun,3)

      jconf=5
      u1=ums(istate,isite,2)
      u2=ums(istate,isite,3)
      dr=abs(u1-u2)
      u1=ums(istate,isite,jconf)-dr/2
      u2=ums(istate,isite,jconf)+dr/2
      nrun = 500

      xt0=ums(istate,isite,jconf)*sfromc(cbu(istate,isite,jconf))
     >*dcos(aus(istate,isite,jconf))
      yt0=ums(istate,isite,jconf)*sfromc(cbu(istate,isite,jconf))
     >*dsin(aus(istate,isite,jconf))               
      zt0=ums(istate,isite,jconf)*cbu(istate,isite,jconf)

      call mat(-gas(istate,isite,jconf),
     >-dacos(cba(istate,isite,jconf))
     >,-aas(istate,isite,jconf),rot)
      do i=1,nato_ca
        x1=coord(inato_ca(i),1)
        y1=coord(inato_ca(i),2)
        z1=coord(inato_ca(i),3)
        call rotrdi(x1,y1,z1,rx,ry,rz,xt0,yt0,zt0,rot)
        coord2(i,1)=rx
        coord2(i,2)=ry
        coord2(i,3)=rz
      enddo
      r1= 1.e30
      r2=-1.e30
      ar1= 1.e30
      ar2=-1.e30
      cbr1= 1.e30
      cbr2=-1.e30
      aa1= 1.e30
      aa2=-1.e30
      cba1= 1.e30
      cba2=-1.e30
      ga1= 1.e30
      ga2=-1.e30
      do j=1,nrun
        write(*,*) j
 20     ddr=(rand(0))**(1./3)*dr
        artmp=rand(0)*pi2-pi
        cbrtmp=rand(0)*2-1
        brtmp=dacos(cbrtmp)
        aatmp=rand(0)*pi2-pi
        cbatmp=rand(0)*2-1
        batmp=dacos(cbatmp)
        gatmp=rand(0)*pi2-pi
        xt=xt0+ddr*dsin(brtmp)*dcos(artmp)
        yt=yt0+ddr*dsin(brtmp)*dsin(artmp)
        zt=zt0+ddr*dcos(brtmp)
        call cpolar(xt,yt,zt,ddr,artmp,brtmp)
        cbrtmp=dcos(brtmp)

c      xt=ums(istate,isite,jconf)*sfromc(cbu(istate,isite,jconf))
c     >*dcos(aus(istate,isite,jconf))
c      yt=ums(istate,isite,jconf)*sfromc(cbu(istate,isite,jconf))
c     >*dsin(aus(istate,isite,jconf))               
c      zt=ums(istate,isite,jconf)*cbu(istate,isite,jconf)

c      gatmp=gas(istate,isite,jconf)
c      batmp=dacos(cba(istate,isite,jconf))
c      aatmp=aas(istate,isite,jconf)

        call mat(-gatmp,-batmp,-aatmp,rot)
        do i=1,nato_ca
          x1=coord2(i,1)
          y1=coord2(i,2)
          z1=coord2(i,3)
          x2=coord(inato_ca(i),1)
          y2=coord(inato_ca(i),2)
          z2=coord(inato_ca(i),3)
          call rotrdi(x2,y2,z2,x1r,y1r,z1r,xt,yt,zt,rot)
          call cpolar(x1-x1r,y1-y1r,z1-z1r,dum1,dum2,dum3)
c          write(*,*) ddr,ums(istate,isite,jconf)
c          write(*,*) j,i,dum1,dr!xt,yt,zt,aatmp,batmp,gatmp
          if (dum1.gt.dr) goto 20
        enddo
        r1=min(r1,ddr)
        r2=max(r2,ddr)
        ar1=min(ar1,artmp)
        ar2=max(ar2,artmp)
        cbr1=min(cbr1,cbrtmp)
        cbr2=max(cbr2,cbrtmp)
        aa1=min(aa1,aatmp)
        aa2=max(aa2,aatmp)
        cba1=min(cba1,cbatmp)
        cba2=max(cba2,cbatmp)
        ga1=min(ga1,gatmp)
        ga2=max(ga2,gatmp)
      enddo
      ums(istate,isite,2)=r1
      ums(istate,isite,3)=r2
      ums(istate,isite,4)=(r2-r1)/2
      aus(istate,isite,2)=ar1
      aus(istate,isite,3)=ar2
      aus(istate,isite,4)=(ar2-ar1)/2
      cbu(istate,isite,2)=cbr1
      cbu(istate,isite,3)=cbr2
      cbu(istate,isite,4)=(cbr2-cbr1)/2
      aas(istate,isite,2)=aa1
      aas(istate,isite,3)=aa2
      aas(istate,isite,4)=(aa2-aa1)/2
      cba(istate,isite,2)=cba1
      cba(istate,isite,3)=cba2
      cba(istate,isite,4)=(cba2-cba1)/2
      gas(istate,isite,2)=ga1
      gas(istate,isite,3)=ga2
      gas(istate,isite,4)=(ga2-ga1)/2
      jums(istate,isite)=1
      return
      end


      subroutine atovlpdb(ipdb)

      include 'quafit2014.cmn'

      j=0
      do i=1,nato
        itest=0
        if (iammch(i,1).ne.0) then
          if (iato(i).eq.2.or.
     >(iammch(i,1).eq.8.and.iato(i).eq.3)) itest=1 !!! perche' 2? significa CA per proteine!!!
        else if (inacch(i,1).ne.0) then
          itest=1!tutti   if (iato(i).eq.13.or.iato(i).le.4) itest=1 !!! o il P o un qualunque C
        endif

        if (itest.eq.1) then
          j=j+1
          if (j.gt.mxnatovl) stop 'j>mxnatovl'
          do k=1,3
            cooovl(ipdb,j,k)=coord(i,k)
          enddo
        endif
      enddo
      natovl(ipdb)=j
      return
      end

