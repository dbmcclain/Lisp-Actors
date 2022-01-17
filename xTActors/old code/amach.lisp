
   Actor/Args struct
   +------+-------+------+------+-//--+------+
   | Link | Actor | Arg1 | Arg2 | ... | ArgN |
   +------+-------+------+------+-//--+------+
              |
              v
          +------+
          | Ptr  | Actor
          +------+
              |
              v
             Beh/Env struct
             +------+-------+------+------+-//--+------+
             | Link | Behav | Env1 | Env2 | ... | EnvN |
             +------+-------+------+------+-//--+------+

prim_send(argstr)
     mov #0,0(Rargstr)
     tst Rqhd
     bz  $1
     mov Rargstr,0(Rqtl)
     mov Rargstr,Rqtl
     ret
$1   mov Rargstr,Rqhd
     mov Rargstr,Rqtl
     ret

prim_unwind
     mov Rqtlsav,Rqtl
     mov Renv,0(Rself)
     tst Rqtl
     bz  $1
     mov #0,0(Rqtl)
$1 ;; fall into prim_next
prim_next
     mov RfreeArgs,0(Rargstr)  ;; release current ArgStr
     mov Rargstr,RfreeArgs
     mov Rqtl,Rqtlsav
     tst Rqhd
     bz  $1
     mov Rqhd,Rargstr          ;; get next msg
     mov 0(Rargstr),Rqhd
$0   mov 1(Rargstr),Rself
     mov 0(Rself),Renv
     jmp @1(Renv)
$1   wait-for-mailbox -> Rargstr
     br  $0

prim_become
     mov Renv2,0(Rself) ;; should just macro to inline
     ret

;; --------------------------------------

prim_beh_entry
     mov Rframe,0(Rargstr)
     mov Rargstr,Rframe ... TBD
