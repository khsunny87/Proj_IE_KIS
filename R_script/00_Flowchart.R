library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

flowchart<-"digraph study_FC {
  
  graph [fontname = Helvetica]
  
  #labeljust = 'l';
  
  node [shape = rect, width = 3]
  A [label='Surgery for IE\\lFrom Jan 1995 to June 2020\\l(N=670)\\l']
  B [label='Age >=18\\l(N=662)\\l']
  
  none1 [label='',width=0,height=0]
    C [label='Infective Endocarditis in Native Mitral Valve\\l(N=313)\\l']
    ex1 [label='Replacment other than MV\\l(N=327)\\lNo MV lesion\\l(N=22)\\l']
  
  none2 [label='',width=0,height=0]
    D [label='Active Infective Endocarditis in Mitral Valve\\l(N=218)\\l']
    ex2 [label='Mitral PVE\\l(N=59)\\lNo active IE\\l(N=36)\\l']
  
  A->B
  
  B->none1 [dir=none]
  none1->C; none1->ex1 [minlen=7]
  {rank=same;none1 ex1}
  
  C->none2 [dir=none]
  none2->D; none2->ex2 [minlen=7]
  {rank=same;none2 ex2}
  
  
}"




