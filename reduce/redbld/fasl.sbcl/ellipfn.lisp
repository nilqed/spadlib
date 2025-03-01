(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ELLIPFN)) 
(REMPROP 'JACOBIAM 'SIMPFN) 
(REMPROP 'JACOBISN 'SIMPFN) 
(REMPROP 'JACOBICN 'SIMPFN) 
(REMPROP 'JACOBIDN 'SIMPFN) 
(REMPROP 'JACOBINS 'SIMPFN) 
(REMPROP 'JACOBINC 'SIMPFN) 
(REMPROP 'JACOBIND 'SIMPFN) 
(REMPROP 'JACOBISC 'SIMPFN) 
(REMPROP 'JACOBICS 'SIMPFN) 
(REMPROP 'JACOBISD 'SIMPFN) 
(REMPROP 'JACOBIDS 'SIMPFN) 
(REMPROP 'JACOBICD 'SIMPFN) 
(REMPROP 'JACOBIDC 'SIMPFN) 
(REMPROP 'JACOBIE 'SIMPFN) 
(REMPROP 'ELLIPTICK 'SIMPFN) 
(REMPROP '|ELLIPTICK'| 'SIMPFN) 
(REMPROP 'ELLIPTICE 'SIMPFN) 
(REMPROP 'ELLIPTICD 'SIMPFN) 
(REMPROP '|ELLIPTICE'| 'SIMPFN) 
(REMPROP 'ELLIPTICF 'SIMPFN) 
(REMPROP 'ELLIPTICPI 'SIMPFN) 
(REMPROP 'ELLIPTICTHETA1 'SIMPFN) 
(REMPROP 'ELLIPTICTHETA2 'SIMPFN) 
(REMPROP 'ELLIPTICTHETA3 'SIMPFN) 
(REMPROP 'ELLIPTICTHETA4 'SIMPFN) 
(REMPROP 'ARCSN 'SIMPFN) 
(REMPROP 'ARCCN 'SIMPFN) 
(REMPROP 'ARCDN 'SIMPFN) 
(REMPROP 'ARCNS 'SIMPFN) 
(REMPROP 'ARCNC 'SIMPFN) 
(REMPROP 'ARCND 'SIMPFN) 
(REMPROP 'ARCSC 'SIMPFN) 
(REMPROP 'ARCCS 'SIMPFN) 
(REMPROP 'ARCSD 'SIMPFN) 
(REMPROP 'ARCDS 'SIMPFN) 
(REMPROP 'ARCCD 'SIMPFN) 
(REMPROP 'ARCDC 'SIMPFN) 
(REMPROP 'WEIERSTRASS 'SIMPFN) 
(REMPROP 'WEIERSTRASS1 'SIMPFN) 
(REMPROP 'WEIERSTRASSZETA 'SIMPFN) 
(REMPROP 'WEIERSTRASSZETA1 'SIMPFN) 
(REMPROP 'WEIERSTRASS_SIGMA 'SIMPFN) 
(REMPROP 'WEIERSTRASS_SIGMA1 'SIMPFN) 
(REMPROP 'WEIERSTRASS_SIGMA2 'SIMPFN) 
(REMPROP 'WEIERSTRASS_SIGMA3 'SIMPFN) 
(CREATE-PACKAGE
 '(ELLIPFN EFJACOBI EFELLINT EFJACINV EFTHETA EFWEIER EFNUMERIC EFREIM)
 '(CONTRIB ELLIPFN)) 
(ENDMODULE) 