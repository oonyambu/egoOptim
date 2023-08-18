method_compare('ackley', p = 2, budget = 100)->a
save(a, file = "a.rda")
rm(a)
method_compare('ackley', p = 6, budget = 150)->b
save(b, file = "b.rda")
rm(b)
method_compare('ackley', p = 10, budget = 200)->c
save(c, file = "c.rda")
rm(c)
method_compare('beale', budget = 100)->d
save(d, file = "d.rda")
rm(d)
method_compare('boha1', budget = 100)->e
save(e, file = "e.rda")
rm(e)
method_compare('boha2', budget = 100)->f
save(f, file = "f.rda")
rm(f)
method_compare('booth', budget = 100)->g
save(g, file = "g.rda")
rm(g)
method_compare('branin', budget = 100)->h
save(h, file = "h.rda")
rm(h)
method_compare('braninmodif', budget = 100)->i
save(i, file = "i.rda")
rm(c)


method_compare('bukin6', budget = 100)->j
method_compare('camel3', budget = 100)->k
method_compare('camel6', budget = 100)->l
method_compare('colville', budget = 100)->m
method_compare('crossit', budget = 100)->n
method_compare('dejong5', budget = 100)->o
method_compare('dixonpr', budget = 100)->p
method_compare('easom', budget = 100)->q

save(j,k,l,m,n,o,p,q, file = "j_q.rda")
rm(j,k,l,m,n,o,p,q)

method_compare('goldpr', budget = 100)->r
method_compare('goldprsc', budget = 100)->s
method_compare('hart3', budget = 100)->t
method_compare('hart4', budget = 120)->u
method_compare('hart6', budget = 150)->v
method_compare('powell', p=10, budget = 200)->w
method_compare('schaffer2', budget = 100)->x
method_compare('schaffer4', budget = 100)->y
method_compare('shekel', budget = 100)->z

save(r,s,t,u,v,w,x,y,z, file = 'r_z.rda')
rm(r,s,t,u,v,w,x,y,z)
