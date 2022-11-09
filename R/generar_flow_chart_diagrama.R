#' @title            Dibuixa un Flow Chart Diagrama
#' @description      Dibuixa un Flow Chart Diagrama
#' @param grups      Grups      
#' @param pob        Pob
#' @param pob_lab    Pob_lab        
#' @param pob1       Pob1
#' @param pob2       Pob2    
#' @param pob3       Pob3
#' @param pob_lab1   Pob lab1
#' @param pob_lab2   Pob lab2    
#' @param pob_lab3   Pob lab3
#' @param exc1       Exc1
#' @param exc2       Exc2
#' @param exc3       Exc3
#' @param exc_lab1   Exc lab1
#' @param exc_lab2   Exc lab2
#' @param exc_lab3   Exc lab3
#' @param colors     Colors
#' @param forma      Forma
#' @importFrom       dplyr "%>%"
#' @export           diagramaFlowchart
#' @examples
#' 
#' k<-diagramaFlowchart(
#' grups=1,
#' pob=c(1700),
#' pob_lab=c("Poblacio Alt Penedes"),
#' pob1=c(1000,500),
#' pob2=c(400,200),
#' pob3=c(300,100),
#' pob_lab1=c("A INICIAL","A FINAL"),
#' pob_lab2=c("B Inicial","B Final"),
#' pob_lab3=c("C Inicial","C Final"),
#' exc1=c(50,300,150),
#' exc2=c(100,50,50),
#' exc3=c(100,50,50),
#' exc_lab1=c('Edat>90 anys','M.Cardio','M.Pulmonar'),
#' exc_lab2=c('Edat>90 anys','M.Cardio','M.Pulmonar'),
#' exc_lab3=c('Edat>90 anys','M.Cardio','M.Pulmonar'),
#' colors=c('white','grey'),
#' forma=c('ellipse','box'))
#
#'k
diagramaFlowchart<-function(
  
  grups=3,
  pob=c(1700),
  pob_lab=c("Poblaci? Alt Pened?s"),
  
  pob1=c(1000,500),
  pob2=c(400,200),
  pob3=c(300,100),
  
  pob_lab1=c("A INICIAL","A FINAL"),
  pob_lab2=c("B Inicial","B Final"),
  pob_lab3=c("C Inicial","C Final"),
  
  exc1=c(50,300,150),
  exc2=c(100,50,50),
  exc3=c(100,50,50),
  
  exc_lab1=c('Edat>90 anys','M.Cardio','M.Pulmonar'),
  exc_lab2=c('Edat>90 anys','M.Cardio','M.Pulmonar'),
  exc_lab3=c('Edat>90 anys','M.Cardio','M.Pulmonar'),
  
  colors=c('white','grey'),
  forma=c('ellipse','box'))

{
  
  if  (grups<1)
  {print("Error, posa els GRUPS, si us plau! al Flowchart!")  }
  else if  (grups==1)
  {diagramaFlowchart1G(
    pob1=pob1,
    pob_lab1=pob_lab1,
    exc1=exc1,
    exc_lab1=exc_lab1,
    colors=colors,
    forma=forma)  }
  else if (grups==2)
  {diagramaFlowchart2G(
    pob=pob,
    pob_lab=pob_lab,
    pob1=pob1,
    pob_lab1=pob_lab1,
    exc1=exc1,
    exc_lab1=exc_lab1,
    pob2=pob2,
    pob_lab2=pob_lab2,
    exc2=exc2,
    exc_lab2=exc_lab2,
    colors=colors,
    forma=forma ) }
  else if (grups==3)
  {diagramaFlowchart3G(
    pob=pob,
    pob_lab=pob_lab,
    pob1=pob1,
    pob_lab1=pob_lab1,
    exc1=exc1,
    exc_lab1=exc_lab1,
    pob2=pob2,
    pob_lab2=pob_lab2,
    exc2=exc2,
    exc_lab2=exc_lab2,
    pob3=pob3,
    pob_lab3=pob_lab3,
    exc3=exc3,
    exc_lab3=exc_lab3,
    colors=colors,
    forma=forma) }
  else if (grups>3)
  {
    print("Error no podem fer m?s de 3 Grups pel Flowchart!")
  }
}


#' @title            Diagrama Flowchart 1 Grup
#' @description      Diagrama Flowchart 1 Grup
#' @param pob1       Pob1
#' @param pob_lab1   pob lab1
#' @param exc1       Exc1
#' @param exc_lab1   Exc lab1
#' @param colors     Colors
#' @param forma      Forma
#' @export           diagramaFlowchart1G
diagramaFlowchart1G<-function(
  pob_lab1=c("Pob Inicial","Pob Final"),
  pob1=c(1000,50),
  exc1=c(10,1),
  exc_lab1=c('Edat>90 anys','kkk'),
  colors=c('white','grey'),
  forma=c('box','box')) 
{
  
  if  (length(exc1)<=10)
  {
    
    m1<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1<-paste0(m1,'->A',i) 
      i=i+1 }
    m1
    #"->A1->A2"#
    #---------------------------------------------------------------------------------------#
    m2<-""
    for (i in 1:(length(exc1)))
    { 
      m2<-paste0(m2,' A',i,'->','E',i,'[color = black,dir=none]') 
      i=i+1 }
    m2
    #A1->E1[color = black,dir=none]A2->E2[color = black,dir=none]
    #---------------------------------------------------------------------------------------#
    m3<-""
    for (i in 1:(length(exc1)))
    { 
      m3<-paste0(m3,' A',i,';') 
      i=i+1 }
    m3
    #A1;A2 
    #---------------------------------------------------------------------------------------#
    m4<-""
    for (i in 1:(length(exc1)))
    { 
      m4<-paste0(m4,' E',i,';') 
      i=i+1 }
    m4
    #E1; E2;
    #---------------------------------------------------------------------------------------#
    m5<-""
    for (i in 1:(length(exc1)))
    { 
      m5<-paste0(m5,'  subgraph {rank = same;',' A',i,';','E',i,'}') 
      i=i+1 }
    m5
    #subgraph {rank = same; A1;E1}  subgraph {rank = same; A2;E2}
    #---------------------------------------------------------------------------------------#
    m6<-""
    for (i in 1:(length(exc1)))
    { 
      m6<-paste0(m6,'A',i,' [label =', "'@@",i+2,"']",';') 
      i=i+1 }
    m6
    #A1 [label ='@@3'];A2 [label ='@@4'];
    #---------------------------------------------------------------------------------------#  
    m7<-""
    for (i in 1:(length(exc1)))
    { 
      m7<-paste0(m7,'E',i,' [label =', "'@@",i+(length(exc1)+2),"']",';') 
      i=i+1 }
    m7
    #E1 [label ='@@5'];E2 [label ='@@6'];
    #---------------------------------------------------------------------------------------#
    m8<-""
    for (i in 1:(length(exc1)) ) 
    { 
      m8<-paste0(m8," \n ","[",i+2,"]:paste0(' ')")
      i=i+1 }
    m8 
    #" \n [3]:paste0(' ') \n [4]:paste0(' ')
    #---------------------------------------------------------------------------------------#
    paramet<-c(m1,m2,m3,m4,m5,m6,m7,m8)
    #---------------------------------------------------------------------------------------#  
    makao1<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,
                   penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]",
                   "P1;P2;", "node[shape=point,width =0,penwidth=0,color=black]",
                   paramet[3],
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0.5,penwidth=0.5,style=filled,fillcolor=",colors[2],"]",
                   paramet[4],
                   " \n ","P1 [label = '@@1']","P2 [label = '@@2']",
                   paramet[6],
                   paramet[7],
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P1",
                   paramet[1],
                   "->P2[color = black,dir=none] ",
                   " \n ",
                   paramet[2],
                   " \n ",
                   paramet[5],"}",
                   "\n[1]:paste0('", pob_lab1[1], " \\n ", "[N = ',", pob1[1], ",']')"   ,
                   "\n[2]:paste0('", pob_lab1[2], " \\n ", "[N = ',", pob1[2], ",']')"   ,
                   paramet[8],
                   "\n[5]:paste0('", exc_lab1[1], " \\n ", "[N = ',", exc1[1], ",']')"   ,
                   "\n[6]:paste0('", exc_lab1[2], " \\n ", "[N = ',", exc1[2], ",']')"  ,
                   "\n[7]:paste0('", exc_lab1[3], " \\n ", "[N = ',", exc1[3], ",']')"   ,
                   "\n[8]:paste0('", exc_lab1[4], " \\n ", "[N = ',", exc1[4], ",']')"  ,
                   "\n[9]:paste0('", exc_lab1[5], " \\n ", "[N = ',", exc1[5], ",']')"   ,
                   "\n[10]:paste0('", exc_lab1[6], " \\n ", "[N = ',", exc1[6], ",']')"  ,
                   "\n[11]:paste0('", exc_lab1[7], " \\n ", "[N = ',", exc1[7], ",']')"   ,
                   "\n[12]:paste0('", exc_lab1[8], " \\n ", "[N = ',", exc1[8], ",']')"  ,
                   "\n[13]:paste0('", exc_lab1[9], " \\n ", "[N = ',", exc1[9], ",']')"   ,
                   "\n[14]:paste0('", exc_lab1[10], " \\n ", "[N = ',", exc1[10], ",']')"  
                   
    )
    
    
    #---------------------------------------------------------------------------------------#
    DiagrammeR::grViz(makao1)
    #---------------------------------------------------------------------------------------#
    
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
  }
}



#' @title             Diagrama Flowchart 2 Grups
#' @description       Diagrama Flowchart 2 Grups
#' @param pob_lab     Pob_lab
#' @param pob         Pob    
#' @param pob1        Pob1
#' @param pob2        Pob2  
#' @param pob_lab1    Pob lab1
#' @param pob_lab2    Pob lab2    
#' @param exc1        Exc1
#' @param exc2        Exc2
#' @param exc_lab1    Exc lab1
#' @param exc_lab2    Exc lab2
#' @param colors      Colors
#' @param forma       Forma
#' @export            diagramaFlowchart2G
diagramaFlowchart2G<-function(
  pob_lab=c("Poblaci? Total"),
  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
  pob_lab2=c("Poblaci? Inicial B","Poblaci? Final B"),
  pob=c(70211123),
  pob1=c(10088,50),
  exc1=c(1021,111,9),
  exc_lab1=c('Edat>90 anys','Cardio','J'),
  pob2=c(19002,599),
  exc2=c(1002,150,90),
  exc_lab2=c('Edat>76 anys','Rata','U'),
  colors=c('white','grey'),
  forma=c('box','box')            ) 
{
  
  if  (length(exc1)<=10 && length(exc2)<=10)
  {
    
    #-----------------------------------------------------------------------------------#
    m1a<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1a<-paste0(m1a,'->A',i) 
      i=i+1 }
    m1a
    #->A1->A2->A3    
    #-----------------------------------------------------------------------------------#
    m1b<-""
    for (i in 1:(length(exc2) ))  
    { 
      m1b<-paste0(m1b,'->B',i) 
      i=i+1 }
    m1b
    #->B1->B2->B3
    #-----------------------------------------------------------------------------------#
    m2a<-""
    for (i in 1:(length(exc1)))
    { 
      m2a<-paste0(m2a,' A',i,'->','E_A',i,'[color = black,dir=none]') 
      i=i+1 }
    m2a
    #A1->E_A1[color = black,dir=none] A2->E_A2[color = black,dir=none] A3->E_A3[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2b<-""
    for (i in 1:(length(exc2)))
    { 
      m2b<-paste0(m2b,' B',i,'->','E_B',i,'[color = black,dir=none]') 
      i=i+1 }
    m2b
    #B1->E_B1[color = black,dir=none] B2->E_B2[color = black,dir=none] B3->E_B3[color = black,dir=none]    
    #-----------------------------------------------------------------------------------#
    m3a<-""
    for (i in 1:(length(exc1)))
    { 
      m3a<-paste0(m3a,' A',i,';') 
      i=i+1 }
    m3a
    #A1; A2; A3;
    #-----------------------------------------------------------------------------------#
    m3b<-""
    for (i in 1:(length(exc2)))
    { 
      m3b<-paste0(m3b,' B',i,';') 
      i=i+1 }
    m3b
    #" B1; B2; B3;"
    #-----------------------------------------------------------------------------------#
    m4a<-""
    for (i in 1:(length(exc1)))
    { 
      m4a<-paste0(m4a,' E_A',i,';') 
      i=i+1 }
    m4a
    #E_A1; E_A2; E_A3;
    #-----------------------------------------------------------------------------------#
    m4b<-""
    for (i in 1:(length(exc2)))
    { 
      m4b<-paste0(m4b,' E_B',i,';') 
      i=i+1 }
    m4b
    #E_B1; E_B2; E_B3;
    #-----------------------------------------------------------------------------------#
    m5a<-""
    for (i in 1:(length(exc1)))
    { 
      m5a<-paste0(m5a,'  subgraph {rank = same;',' A',i,';','E_A',i,';','}') 
      i=i+1 }
    m5a
    # subgraph {rank = same; A1;E_A1;}  subgraph {rank = same; A2;E_A2;}  subgraph {rank = same; A3;E_A3;}
    #-----------------------------------------------------------------------------------#
    m5b<-""
    for (i in 1:(length(exc2)))
    { 
      m5b<-paste0(m5b,'  subgraph {rank = same;',' B',i,';','E_B',i,';','}') 
      i=i+1 }
    m5b
    #subgraph {rank = same; B1;E_B1;}  subgraph {rank = same; B2;E_B2;}  subgraph {rank = same; B3;E_B3;}
    #-----------------------------------------------------------------------------------#
    m6a<-""
    for (i in 1:(length(exc1)))
    { 
      m6a<-paste0(m6a,'A',i,'[label=', "'@@",i+5,"']",';') 
      i=i+1 }
    m6a
    #A1 A1[label='@@6'];A2[label='@@7'];A3[label='@@8'];
    #-----------------------------------------------------------------------------------#
    m6b<-""
    for (i in 1:(length(exc2)))
    { 
      m6b<-paste0(m6b,'B',i,'[label=', "'@@",(i+length(exc2))+5,"']",';') 
      i=i+1 }
    m6b
    #B1[label='@@9'];B2[label='@@10'];B3[label='@@11'];
    #-----------------------------------------------------------------------------------#
    m7a<-""
    for (i in 1:(length(exc1)))
    { 
      m7a<-paste0(m7a,'E_A',i,' [label =', "'@@",19+i,"']",';') 
      i=i+1 }
    m7a
    #E_A1 [label ='@@20'];E_A2 [label ='@@21'];E_A3 [label ='@@22'];
    #-----------------------------------------------------------------------------------#
    m7b<-""
    for (i in 1:(length(exc2)))
    { 
      m7b<-paste0(m7b,'E_B',i,' [label =', "'@@",29+i,"']",';') 
      i=i+1 }
    m7b
    #E_B1 [label ='@@30'];E_B2 [label ='@@31'];E_B3 [label ='@@32'];
    #-----------------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------------#
    paramet2<-c(m1a,m1b,m2a,m2b,m3a,m3b,m4a,m4b,m5a,m5b,m6a,m6b,m7a,m7b)
    #-----------------------------------------------------------------------------------#
    makao2<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]"," P_T;PA_I;PA_F;PB_I;PB_F ", 
                   
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0,penwidth=0,style=filled,fillcolor=",colors[2],"]",
                   paramet2[7],paramet2[8],
                   
                   "node[shape=point,width =0,penwidth=0,color=black,fontname='Courier New']",paramet2[5],paramet2[6],
                   
                   " \n ","P_T[label='@@1']","PA_I[label='@@2']","PA_F[label='@@4']","PB_I[label='@@3']","PB_F[label='@@5']",
                   
                   paramet2[13],paramet2[14],
                   
                   paramet2[11],paramet2[12],
                   
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P_T->PA_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PB_I[color = black,arrowhead=vee]",
                   " \n ","PA_I",paramet2[1],"->PA_F[color = black,dir=none] ",
                   " \n ",paramet2[3],
                   " \n ","PB_I",paramet2[2],"->PB_F[color = black,dir=none] ",
                   " \n ",paramet2[4],
                   " \n ",paramet2[9],
                   " \n ",paramet2[10],"}",
                   " \n[1]:paste0('", pob_lab[1], " \\n "," [N = ',",  pob[1], ",']')",
                   " \n[2]:paste0('", pob_lab1[1]," \\n ", " [N = ',", pob1[1], ",']')",
                   " \n[3]:paste0('", pob_lab2[1]," \\n ", " [N = ',", pob2[1], ",']')",
                   " \n[4]:paste0('", pob_lab1[2]," \\n ", " [N = ',", pob1[2], ",']')",
                   " \n[5]:paste0('", pob_lab2[2]," \\n ", " [N = ',", pob2[2], ",']')",
                   " \n[6]:paste0('')"," \n[7]:paste0('')"," \n[8]:paste0('')"," \n[9]:paste0('')",
                   " \n[10]:paste0('')"," \n[11]:paste0('')"," \n[12]:paste0('')"," \n[13]:paste0('')",
                   " \n[14]:paste0('')"," \n[15]:paste0('')"," \n[16]:paste0('')"," \n[17]:paste0('')",
                   " \n[18]:paste0('')"," \n[19]:paste0('')",
                   " \n[20]:paste0('", exc_lab1[1]," \\n ", "[N = ',", exc1[1], ",']')",
                   " \n[21]:paste0('", exc_lab1[2]," \\n ", "[N = ',", exc1[2], ",']')",
                   " \n[22]:paste0('", exc_lab1[3]," \\n ", "[N = ',", exc1[3], ",']')",
                   " \n[23]:paste0('", exc_lab1[4]," \\n ", "[N = ',", exc1[4], ",']')",
                   " \n[24]:paste0('", exc_lab1[5]," \\n ", "[N = ',", exc1[5], ",']')",
                   " \n[25]:paste0('", exc_lab1[6]," \\n ", "[N = ',", exc1[6], ",']')",
                   " \n[26]:paste0('", exc_lab1[7]," \\n ", "[N = ',", exc1[7], ",']')",
                   " \n[27]:paste0('", exc_lab1[8]," \\n ", "[N = ',", exc1[8], ",']')",
                   " \n[28]:paste0('", exc_lab1[9]," \\n ", "[N = ',", exc1[9], ",']')",
                   " \n[29]:paste0('", exc_lab1[10]," \\n ", "[N = ',",exc1[10], ",']')",
                   " \n[30]:paste0('", exc_lab2[1]," \\n ", "[N = ',", exc2[1], ",']')",
                   " \n[31]:paste0('", exc_lab2[2]," \\n ", "[N = ',", exc2[2], ",']')",
                   " \n[32]:paste0('", exc_lab2[3]," \\n ", "[N = ',", exc2[3], ",']')",
                   " \n[33]:paste0('", exc_lab2[4]," \\n ", "[N = ',", exc2[4], ",']')",
                   " \n[34]:paste0('", exc_lab2[5]," \\n ", "[N = ',", exc2[5], ",']')",
                   " \n[35]:paste0('", exc_lab2[6]," \\n ", "[N = ',", exc2[6], ",']')",
                   " \n[36]:paste0('", exc_lab2[7]," \\n ", "[N = ',", exc2[7], ",']')",
                   " \n[37]:paste0('", exc_lab2[8]," \\n ", "[N = ',", exc2[8], ",']')",
                   " \n[38]:paste0('", exc_lab2[9]," \\n ", "[N = ',", exc2[9], ",']')",
                   " \n[39]:paste0('", exc_lab2[10]," \\n ", "[N = ',",exc2[10], ",']')"
    )
    
    #---------------------------------------------------------------------------------------#
    DiagrammeR::grViz(makao2)
    #---------------------------------------------------------------------------------------#
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
    
    
  }
}


#' @title            Diagrama Flowchart 3 Grups
#' @description      Diagrama Flowchart 3 Grups
#' @param pob_lab    Pob lab 
#' @param pob        Pob    
#' @param pob1       Pob1   
#' @param pob2       Pob2
#' @param pob3       Pob3    
#' @param pob_lab1   Pob lab1
#' @param pob_lab2   Pob lab2
#' @param pob_lab3   Pob lab3
#' @param exc1       Exc1
#' @param exc2       Exc2
#' @param exc3       Exc3
#' @param exc_lab1   Exc lab1
#' @param exc_lab2   Exc lab2
#' @param exc_lab3   Exc lab3
#' @param colors     Colors
#' @param forma      Forma
#' @export           diagramaFlowchart3G
diagramaFlowchart3G<-function(
  pob_lab=c("Poblaci? Total"),
  pob_lab1=c("Poblaci? Inicial A","Poblaci? Final A"),
  pob_lab2=c("Poblaci? Inicial B","Poblaci? Final B"),
  pob_lab3=c("Poblaci? Inicial C","Poblaci? Final C"),
  pob=c(70211123),
  pob1=c(10088,50),
  exc1=c(1021,111,9),
  exc_lab1=c('Edat>90 anys','Cardio','J'),
  pob2=c(19002,599),
  exc2=c(1002,150,90),
  exc_lab2=c('Edat>76 anys','Rata','U'),
  pob3=c(19002,599),
  exc3=c(1002,150,0),
  exc_lab3=c('Edat>91 anys','Pulm?','L'),
  colors=c('white','grey'),
  forma=c('box','box')            ) 
{
  
  if  (length(exc1)<=10 && length(exc2)<=10  && length(exc3)<=10    )
  {
    
    #-----------------------------------------------------------------------------------#
    m1a<-""
    for (i in 1:(length(exc1) ))  
    { 
      m1a<-paste0(m1a,'->A',i) 
      i=i+1 }
    m1a
    #"->A1->A2->A3"#
    #-----------------------------------------------------------------------------------#
    m1b<-""
    for (i in 1:(length(exc2) ))  
    { 
      m1b<-paste0(m1b,'->B',i) 
      i=i+1 }
    m1b
    #"->B1->B2->B3"
    #-----------------------------------------------------------------------------------#
    m1c<-""
    for (i in 1:(length(exc3) ))  
    { 
      m1c<-paste0(m1c,'->C',i) 
      i=i+1 }
    m1c
    #"->C1->C2->C3"
    #-----------------------------------------------------------------------------------#
    m2a<-""
    for (i in 1:(length(exc1)))
    { 
      m2a<-paste0(m2a,' A',i,'->','E_A',i,'[color = black,dir=none]') 
      i=i+1 }
    m2a
    #A1->E_A1[color = black,dir=none] A2->E_A2[color = black,dir=none] A3->E_A3[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2b<-""
    for (i in 1:(length(exc2)))
    { 
      m2b<-paste0(m2b,' B',i,'->','E_B',i,'[color = black,dir=none]') 
      i=i+1 }
    m2b
    #B1->E_B1[color = black,dir=none] B2->E_B2[color = black,dir=none]
    #-----------------------------------------------------------------------------------#
    m2c<-""
    for (i in 1:(length(exc3)))
    { 
      m2c<-paste0(m2c,' C',i,'->','E_C',i,'[color = black,dir=none]') 
      i=i+1 }
    m2c
    #C1->E_C1[color = black,dir=none] C2->E_C2[color = black,dir=none] C3->E_C3[color = black,dir=none]
    #-----------------------------------------------------------------------------------#    
    m3a<-""
    for (i in 1:(length(exc1)))
    { 
      m3a<-paste0(m3a,' A',i,';') 
      i=i+1 }
    m3a
    # A1; A2; A3;
    #-----------------------------------------------------------------------------------#
    m3b<-""
    for (i in 1:(length(exc2)))
    { 
      m3b<-paste0(m3b,' B',i,';') 
      i=i+1 }
    m3b
    #B1; B2; B3; 
    #-----------------------------------------------------------------------------------#
    m3c<-""
    for (i in 1:(length(exc3)))
    { 
      m3c<-paste0(m3c,' C',i,';') 
      i=i+1 }
    m3c
    #C1; C2; C3;
    #-----------------------------------------------------------------------------------#
    m4a<-""
    for (i in 1:(length(exc1)))
    { 
      m4a<-paste0(m4a,' E_A',i,';') 
      i=i+1 }
    m4a
    #E_A1; E_A2; E_A3;
    #-----------------------------------------------------------------------------------#
    m4b<-""
    for (i in 1:(length(exc2)))
    { 
      m4b<-paste0(m4b,' E_B',i,';') 
      i=i+1 }
    m4b
    #E_B1; E_B2; E_B3;"
    #-----------------------------------------------------------------------------------#
    m4c<-""
    for (i in 1:(length(exc3)))
    { 
      m4c<-paste0(m4c,' E_C',i,';') 
      i=i+1 }
    m4c
    #E_C1; E_C2; E_C3;
    #-----------------------------------------------------------------------------------#
    m5a<-""
    for (i in 1:(length(exc1)))
    { 
      m5a<-paste0(m5a,'  subgraph {rank = same;',' A',i,';','E_A',i,';','}') 
      i=i+1 }
    m5a
    #subgraph {rank = same; A1;E_A1;}  subgraph {rank = same; A2;E_A2;}  subgraph {rank = same; A3;E_A3;}"
    #-----------------------------------------------------------------------------------#
    m5b<-""
    for (i in 1:(length(exc2)))
    { 
      m5b<-paste0(m5b,'  subgraph {rank = same;',' B',i,';','E_B',i,';','}') 
      i=i+1 }
    m5b
    #subgraph {rank = same; B1;E_B1;}  subgraph {rank = same; B2;E_B2;}  subgraph {rank = same; B3;E_B3;}
    #-----------------------------------------------------------------------------------#
    m5c<-""
    for (i in 1:(length(exc3)))
    { 
      m5c<-paste0(m5c,'  subgraph {rank = same;',' C',i,';','E_C',i,';','}') 
      i=i+1 }
    m5c
    # subgraph {rank = same; C1;E_C1;}  subgraph {rank = same; C2;E_C2;}  subgraph {rank = same; C3;E_C3;}
    #-----------------------------------------------------------------------------------#
    m6a<-""
    for (i in 1:(length(exc1)))
    { 
      m6a<-paste0(m6a,'A',i,'[label=', "'@@",i+7,"']",';') 
      i=i+1 }
    m6a
    #A1[label='@@8'];A2[label='@@9'];A3[label='@@10'];
    #-----------------------------------------------------------------------------------#
    m6b<-""
    for (i in 1:(length(exc2)))
    { 
      m6b<-paste0(m6b,'B',i,'[label=', "'@@",(i+length(exc2))+7,"']",';') 
      i=i+1 }
    m6b
    #B1[label='@@11'];B2[label='@@12'];B3[label='@@13'];
    #-----------------------------------------------------------------------------------#
    m6c<-""
    for (i in 1:(length(exc3)))
    { 
      m6c<-paste0(m6c,'C',i,'[label=', "'@@",(i+length(exc3))+10,"']",';') 
      i=i+1 }
    m6c
    #C1[label='@@14'];C2[label='@@15'];C3[label='@@16'];"
    #-----------------------------------------------------------------------------------#
    m7a<-""
    for (i in 1:(length(exc1)))
    { 
      m7a<-paste0(m7a,'E_A',i,' [label =', "'@@",19+i,"']",';') 
      i=i+1 }
    m7a
    #"E_A1 [label ='@@20'];E_A2 [label ='@@21'];E_A3 [label ='@@22']"
    #-----------------------------------------------------------------------------------#
    m7b<-""
    for (i in 1:(length(exc2)))
    { 
      m7b<-paste0(m7b,'E_B',i,' [label =', "'@@",29+i,"']",';') 
      i=i+1 }
    m7b
    #E_B1 [label ='@@30'];E_B2 [label ='@@31'];E_B3 [label ='@@32']
    #-----------------------------------------------------------------------------------#
    m7c<-""
    for (i in 1:(length(exc3)))
    { 
      m7c<-paste0(m7c,'E_C',i,' [label =', "'@@",39+i,"']",';') 
      i=i+1 }
    m7c
    #"E_C1 [label ='@@40'];E_C2 [label ='@@41'];E_C3 [label ='@@42'];"
    #-----------------------------------------------------------------------------------#
    paramet2<-c(m1a,m1b,m1c,m2a,m2b,m2c,m3a,m3b,m3c,m4a,m4b,m4c,m5a,m5b,m5c,m6a,m6b,m6c,m7a,m7b,m7c)
    #-----------------------------------------------------------------------------------#
    makao3<-paste0("digraph rai {","graph[layout = dot]",  
                   "node[shape=",forma[1],",","fontsize=12,fontname=Helvetica,width=0.9,penwidth=0.9,color=black,style=filled,fillcolor=",colors[1],"]"," P_T;PA_I;PA_F;PB_I;PB_F;PC_I;PC_F ", 
                   
                   "node[shape=",forma[2],",","fontsize=8,fontname='Courier New',width=0,penwidth=0,style=filled,fillcolor=",colors[2],"]",
                   paramet2[10],paramet2[11],paramet2[12],
                   
                   "node[shape=point,width =0,penwidth=0,color=black,fontname='Courier New']",paramet2[7],paramet2[8],paramet2[9],
                   
                   " \n ","P_T[label='@@1']","PA_I[label='@@2']","PA_F[label='@@5']","PB_I[label='@@3']","PB_F[label='@@6']","PC_I[label='@@4']","PC_F[label='@@7']",
                   " \n ",paramet2[19],paramet2[20],paramet2[21],
                   " \n ",paramet2[16],paramet2[17],paramet2[18],
                   " \n ","edge[width=0.5,penwidth=0.5,arrowhead=vee]",
                   " \n ","P_T->PA_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PB_I[color = black,arrowhead=vee]",
                   " \n ","P_T->PC_I[color = black,arrowhead=vee]",
                   " \n ","PA_I",paramet2[1],"->PA_F[color = black,dir=none] ",
                   " \n ",paramet2[4],
                   " \n ","PB_I",paramet2[2],"->PB_F[color = black,dir=none] ",
                   " \n ",paramet2[5],
                   " \n ","PC_I",paramet2[3],"->PC_F[color = black,dir=none] ",
                   " \n ",paramet2[6],
                   
                   " \n ",paramet2[13],
                   " \n ",paramet2[14],
                   " \n ",paramet2[15],"}",
                   
                   " \n[1]:paste0('", pob_lab[1], " \\n ", "  [N = ',",  pob[1], ",']')",
                   " \n[2]:paste0('", pob_lab1[1]," \\n ",  " [N = ',", pob1[1], ",']')",
                   " \n[3]:paste0('", pob_lab2[1]," \\n ", " [N = ',", pob2[1], ",']')",
                   " \n[4]:paste0('", pob_lab3[1]," \\n ", " [N = ',", pob3[1], ",']')",
                   " \n[5]:paste0('", pob_lab1[2]," \\n ", " [N = ',", pob1[2], ",']')",
                   " \n[6]:paste0('", pob_lab2[2]," \\n ", " [N = ',", pob2[2], ",']')",
                   " \n[7]:paste0('", pob_lab3[2]," \\n ", " [N = ',", pob3[2], ",']')",
                   " \n[8]:paste0('')"," \n[9]:paste0('')"," \n[10]:paste0('')"," \n[11]:paste0('')",
                   " \n[12]:paste0('')"," \n[13]:paste0('')"," \n[14]:paste0('')"," \n[15]:paste0('')",
                   " \n[16]:paste0('')"," \n[17]:paste0('')"," \n[18]:paste0('')"," \n[19]:paste0('')",
                   " \n[20]:paste0('", exc_lab1[1]," \\n ", "[N = ',", exc1[1], ",']')",
                   " \n[21]:paste0('", exc_lab1[2]," \\n ", "[N = ',", exc1[2], ",']')",
                   " \n[22]:paste0('", exc_lab1[3]," \\n ", "[N = ',", exc1[3], ",']')",
                   " \n[23]:paste0('", exc_lab1[4]," \\n ", "[N = ',", exc1[4], ",']')",
                   " \n[24]:paste0('", exc_lab1[5]," \\n ", "[N = ',", exc1[5], ",']')",
                   " \n[25]:paste0('", exc_lab1[6]," \\n ", "[N = ',", exc1[6], ",']')",
                   " \n[26]:paste0('", exc_lab1[7]," \\n ", "[N = ',", exc1[7], ",']')",
                   " \n[27]:paste0('", exc_lab1[8]," \\n ", "[N = ',", exc1[8], ",']')",
                   " \n[28]:paste0('", exc_lab1[9]," \\n ", "[N = ',", exc1[9], ",']')",
                   " \n[20]:paste0('", exc_lab1[10]," \\n ", "[N = ',",exc1[10], ",']')",
                   " \n[30]:paste0('", exc_lab2[1]," \\n ", "[N = ',", exc2[1], ",']')",
                   " \n[31]:paste0('", exc_lab2[2]," \\n ", "[N = ',", exc2[2], ",']')",
                   " \n[32]:paste0('", exc_lab2[3]," \\n ", "[N = ',", exc2[3], ",']')",
                   " \n[33]:paste0('", exc_lab2[4]," \\n ", "[N = ',", exc2[4], ",']')",
                   " \n[34]:paste0('", exc_lab2[5]," \\n ", "[N = ',", exc2[5], ",']')",
                   " \n[35]:paste0('", exc_lab2[6]," \\n ", "[N = ',", exc2[6], ",']')",
                   " \n[36]:paste0('", exc_lab2[7]," \\n ", "[N = ',", exc2[7], ",']')",
                   " \n[37]:paste0('", exc_lab2[8]," \\n ", "[N = ',", exc2[8], ",']')",
                   " \n[38]:paste0('", exc_lab2[9]," \\n ", "[N = ',", exc2[9], ",']')",
                   " \n[39]:paste0('", exc_lab2[10]," \\n ","[N = ',", exc2[10], ",']')",
                   " \n[40]:paste0('", exc_lab3[1]," \\n ", "[N = ',", exc3[1], ",']')",
                   " \n[41]:paste0('", exc_lab3[2]," \\n ", "[N = ',", exc3[2], ",']')",
                   " \n[42]:paste0('", exc_lab3[3]," \\n ", "[N = ',", exc3[3], ",']')",
                   " \n[43]:paste0('", exc_lab3[4]," \\n ", "[N = ',", exc3[4], ",']')",
                   " \n[44]:paste0('", exc_lab3[5]," \\n ", "[N = ',", exc3[5], ",']')",
                   " \n[45]:paste0('", exc_lab3[6]," \\n ", "[N = ',", exc3[6], ",']')",
                   " \n[46]:paste0('", exc_lab3[7]," \\n ", "[N = ',", exc3[7], ",']')",
                   " \n[47]:paste0('", exc_lab3[8]," \\n ", "[N = ',", exc3[8], ",']')",
                   " \n[48]:paste0('", exc_lab3[9]," \\n ", "[N = ',", exc3[9], ",']')",
                   " \n[49]:paste0('", exc_lab3[10]," \\n ", "[N = ',",exc3[10], ",']')"
    )                  
    
    #---------------------------------------------------------------------------------------#  
    DiagrammeR::grViz(makao3)
    #---------------------------------------------------------------------------------------#
    
  }
  else
  {print("ERROR!, Les Exlusions han de ser iguals o inferiors a 10 !")
    
  }
}



#' @title                   Dibuixa un Flow Chart Diagrama a partir del Conductor
#' @description             Dibuixa un Flow Chart Diagrama a partir del Conductor
#' @param dt                Dt       
#' @param taulavariables    Taulavariables
#' @param criteris          Criteris  
#' @param pob_lab           Pob labels  
#' @param etiquetes         Etiquetes  
#' @param ordre             Ordre
#' @param grups             Grups
#' @param sequencial        Sequencial
#' @param colors            Colors
#' @param forma             Forma
#' @param missings          missings
#' @param ...               Altres parametres
#' @importFrom              dplyr "%>%"
#' @export                  criteris_exclusio_diagrama
#' @examples
#' 
#' conductor_cars
#' exemple
#' 
#' k2<-criteris_exclusio_diagrama(
#' dt=exemple,
#' taulavariables=conductor_cars,
#' criteris="exclusio",
#' ordre="exc_ordre",
#' grups="lloc",
#' etiquetes="descripcio",
#' sequencial = TRUE,
#' pob_lab=c("Pob inicial","Pob final"),
#' colors=c("white","grey"),
#' forma=c("ellipse","box"))
#' 
#' k2
#' 

criteris_exclusio_diagrama<-function(dt=dades,
                                     taulavariables="VARIABLES_R3b.xls",
                                     criteris="exclusio",
                                     pob_lab=c("Pob inicial","Pob final"),
                                     etiquetes="etiqueta_exclusio",
                                     ordre="exc_ordre",
                                     grups=NA,
                                     sequencial=F,
                                     colors=c("white","grey"),
                                     forma=c("ellipse","box"), 
                                     missings=T,...){
  
  
  
  #dt=iris
  #taulavariables=conductor_cars
  #criteris="exclusio"
  #ordre="exc_ordre"
  #grups="Species"
  #etiquetes="descripcio"
  #sequencial = TRUE
  #pob_lab=c("Pob inicial","Pob final")
  #colors=c("white","grey")
  #forma=c("ellipse","box")
  #missings=F
  
  
  
  grups2=grups
  ### Si hi ha grups capturar el nombre categories
  # Per defecte UN sol grup
  ngrups=1
  # ngrups>1
  if (!is.na(grups)) {
    ngrups=length(table(dt[grups]))
    Etiqueta_pob_inicial=pob_lab[1]
    Npob_inicial=dt %>% count() %>% as.numeric()}
  
  ##  Llegeixo criteris de variables i selecciono variables amb filtres 
  variables <- read_conductor(taulavariables,col_types = "text",...)%>%
   tidyr::as_tibble()%>%
      dplyr::select(camp,!!etiquetes,!!ordre,!!criteris)
  
  variables <- read_conductor(taulavariables,col_types = "text")%>%  
    tidyr::as_tibble()%>%
    dplyr::select(camp,!!etiquetes,!!ordre,!!criteris)
  
  ###
  #
  #
  #variables <- read_conductor(taulavariables,col_types = "text")
  #%>% tidyr::as_tibble() %>% dplyr::select(camp,!!etiquetes,!!ordre,!!criteris)
  
  # Filtrar valors
  ##criteris_sym<-sym(criteris)
  ##variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))
  # variables[is.na(variables)]<- 0
  # variables<-variables %>% dplyr::filter_(paste0(criteris,"!=0")) 
  
  criteris_sym<-sym(criteris)
  variables<-variables %>%
    dplyr::filter(!is.na(!!criteris_sym))%>%
      dplyr::filter(!!criteris_sym!="")
  
  
  # Parar si no hi ha criteris d'exclusio
  if (variables %>% count() %>% as.numeric()==0) {
    print("No hi ha criteris je ejj")
    return("Error") }
  
  ##  Elimino els espais en blanc de les variables factor
  dt<-dt %>%
    dplyr::mutate_if(is.factor,funs(stringr::str_trim(.))) 
  
  ## Selecciono dades nomes de les variables implicades en el filtres 
  #llista_camps<-variables["camp"] 
  
  ## Dades amb variables implicades en els filtres
  if (is.na(grups)) {dt<-dt %>% dplyr::mutate(grup="constant")}  
  if (is.na(grups)) {grups="grup"}
  
  datatemp0<-dt %>% dplyr::select(c(variables[["camp"]],grups)) %>% as_tibble %>% rename_("grup"=grups)
  datatemp<-dt %>% dplyr::select(c(variables[["camp"]],grups)) %>% as_tibble %>% rename_("grup"=grups)
  
  datatemp0<-datatemp0 %>% dplyr::filter(!is.na(grup))
  datatemp<-datatemp %>% dplyr::filter(!is.na(grup))
  
  # Genero filtres
  maco_noms<-variables["camp"]
  
  # Genero la llista de filtres 
  # maco_criteris<-variables %>% 
  #   dplyr::select_("camp",criteris,ordre) %>%
  #   tidyr::unite_("filtres", c("camp", criteris),sep="") 
  
  # Genero la llista de filtres (versio millorada) 
  
  # caracters logics del filtre
  char_logics<-c(">",">=","<","<=","==","!=","is.na") %>% paste0(collapse = '|')
  
  maco_criteris<-variables %>% 
    dplyr::filter_(paste0(criteris,"!=0")) %>% dplyr::select_("camp",criteris,ordre)%>%
     dplyr::transmute_("camp","crit_temp"=criteris,ordre) %>% 
    # f criteri missing is.na()
      dplyr::mutate(crit_temp=if_else(stringr::str_detect(crit_temp,"is.na"),paste0("is.na(",camp,")"),crit_temp)) %>%
       dplyr::mutate(camp=if_else(stringr::str_detect(crit_temp,"is.na"),"",camp)) %>% 
    # Si es texte sense igualtat --> la poso 
         dplyr::mutate(crit_temp=if_else(stringr::str_detect(crit_temp,char_logics),crit_temp,paste0("=='",crit_temp,"'"))) %>% 
    # Genero la llista de filtres 
          tidyr::unite(filtres, c("camp", "crit_temp"),sep="")%>%
           dplyr::mutate(filtres=paste0("(",filtres,")"))
  
  maco_criteris<-maco_noms %>% cbind(maco_criteris) %>%dplyr::mutate(tipus_cri="pur")
  
  # Afegeix que cada criteri tingui valors valids en cada criteri  
  maco_criteris<-maco_criteris %>% mutate(filtres=stringr::str_c("(",filtres, " & !is.na(",camp, "))"))   
  
  maco_miss<-variables %>% 
    dplyr::select_("camp",ordre) %>%
    dplyr::mutate(filtres=paste0("is.na(",OR2=camp,")",sep="")) %>% dplyr::select_("filtres",ordre)
  
  maco_miss<-maco_noms %>% cbind(maco_miss) %>% dplyr::mutate(tipus_cri="missing")
  
  maco_criteris<-maco_criteris %>% rbind(maco_miss) %>%dplyr::arrange_(ordre)
  
  # # Eliminar filtres repetits?
  maco_criteris<-maco_criteris %>%
    dplyr::group_by(filtres)%>%
     dplyr::slice(1L)%>%
      dplyr::ungroup()%>%
        dplyr::arrange_(ordre)
  
  ## Eliminar missings criteri si missings==F                                                        
  if (missings==F) maco_criteris<-maco_criteris %>% filter(tipus_cri!="missing")  
  
  
  ## Generar taula amb dades per cada criteri d'exclusio  
  num_criteris<-data.frame()
  
  ## Generar dades dels critersi criteris 
  datatemp2<-datatemp
  
  for (i in 1: length(maco_criteris$filtres)){
    
    #i<-1
    kk<-maco_criteris[i,]$filtres
    datatemp2<-datatemp2 %>%dplyr::mutate(dumy=(dplyr::if_else(eval(parse(text=kk)),1,0,missing =NULL)),
                                          dumy = tidyr::replace_na(dumy, 0))
    
    dades_criteris<-datatemp2 %>%
      dplyr::filter_(as.character(maco_criteris[i,]$filtres)) %>%
       dplyr::group_by(grup)%>%dplyr::summarise(n=n(),any(n))%>%
        dplyr::mutate(criteri=i) %>%
          dplyr::mutate(camp=maco_criteris[i,]$camp,
                    filtre_tipus=maco_criteris[i,]$tipus_cri,
                    filtre_forma=maco_criteris[i,]$filtres
      ) %>%dplyr::ungroup()
    
    num_criteris<-num_criteris %>% rbind(dades_criteris)
    #------------------------------------------------------------------------------#  
    #  # Si es sequencial --> actualitza datatemp
    if (sequencial) {datatemp2<-datatemp2 %>%dplyr::filter(dumy==0)}
    
    #-------------------------------------------------------------------------------#  
    
  }
  
  # Si un grup no te exclusions s'ha d'afegir una fila missing 
  nivells_grup<-datatemp %>% dplyr::select(grup) %>% distinct() %>% pull()
  
  num_criteris<-num_criteris %>% bind_rows(tibble(grup=nivells_grup[1]))
  num_criteris<-num_criteris %>% bind_rows(tibble(grup=nivells_grup[2]))
  num_criteris<-num_criteris %>% bind_rows(tibble(grup=nivells_grup[3]))
  
  # ull FEM l'ODRE !!!
  
  # Expandir per tenir una fila per criteri amb valor 
  taula_criteris<-num_criteris %>% 
    tidyr::expand(grup,camp,filtre_tipus) %>% 
    dplyr::left_join(num_criteris,by=c("grup","camp","filtre_tipus"))%>%dplyr::arrange_("criteri")
  
  # Netejar aquelles files que no tinguin cap 0 en cap dels grups 
  temp<-taula_criteris %>% mutate(n=ifelse(is.na(n),0,n)) %>% 
    dplyr::group_by(camp,filtre_tipus) %>% 
    dplyr::summarise(suma_n=sum(n))
  
  taula_criteris<-taula_criteris %>% 
    dplyr::left_join(temp,by=c("camp","filtre_tipus")) %>% 
    dplyr::filter(suma_n!=0) %>% 
    dplyr::select(-suma_n) %>% 
    dplyr::mutate (n=ifelse(is.na(n),0,n))
  
  # Afegir ordre
  taula_ordre<-taula_criteris %>% group_by(camp) %>% slice(1L) %>% ungroup() %>% select(camp,ordre=criteri)
  taula_criteris<-taula_criteris %>% left_join(taula_ordre,by="camp") %>% arrange(ordre)
  
  # Afegir etiquetes a num_criteris
  # Etiquetes dels criteris d'inclusio 
  
  if (etiquetes=="NA") {
    variables<-variables %>% dplyr::mutate(etiquetes=paste0(camp,":",variables[[criteris]]))
    variables<-variables %>% dplyr::mutate(etiquetes=stringr::str_remove_all(etiquetes,"'"))}
  
  etiquetes_sym=rlang::sym(etiquetes)
  if (etiquetes!="NA") {variables<-variables %>%dplyr:: mutate(etiquetes=!!etiquetes_sym)}
  
  taula_etiquetes<- variables %>%dplyr::select(camp,etiquetes) %>%dplyr:: rename(etiqueta_exclusio=etiquetes)
  #taula_etiquetes<- variables %>% dplyr::select_("camp",etiquetes) %>%dplyr:: rename_("etiqueta_exclusio"=etiquetes)
  taula_criteris<-taula_criteris %>% dplyr::left_join(taula_etiquetes,by="camp")
  taula_criteris<-taula_criteris %>% dplyr::mutate(etiqueta_exclusio=ifelse(filtre_tipus=="missing",paste0("Excluded NA:",camp),etiqueta_exclusio))
  
  #[AQUI!]#Creem els parametres que posramen ald Diagrammer!i si tenim un factor, 
  #cada nivell del factor anira a una llista! 
  ## I ara passar informacio generada a vectors per passar-ho al diagrameR
  
  # Etiquetes d'exclusions
  lab_exc<-taula_criteris[c("etiqueta_exclusio","grup")] %>% split(.$grup)
  lab_exc<-lab_exc[[1]]$etiqueta_exclusio %>% as.vector()
  
  # N d'esclusions 
  n_exc<-taula_criteris[c("n","grup")] %>% split(.$grup)
  
  # Calcular N poblacio final per cada grup (3x1)
  # Generar FILTRE
  filtre_total<-stringr::str_c(maco_criteris$filtres,collapse=" | ")
  filtre_total<-stringr::str_c("!(",filtre_total,")")
  
  # Eliminar els espais en blanc de les variables factors del data.frame
  datatemp<-datatemp %>% dplyr::mutate_if(is.factor,funs(stringr::str_trim(.)))
  
  # Aplicar FILTRE 
  datatemp<-datatemp %>% dplyr::filter(eval(parse(text=filtre_total)))
  
  
  #  Generar Etiquetes: Pob inicial i final x grup 
  #-------------------------------------------------------------------------------#
  pob<-  datatemp0%>% dplyr:: summarise(n=n()) %>% dplyr::select(n) %>% as.vector
  #-------------------------------------------------------------------------------#
  #pob.i<-vector("numeric",ngrups)
  #pob.f<-vector("numeric",ngrups)
  #??
  #-------------------------------------------------------------------------------#
  pob.i<-datatemp0%>% dplyr:: group_by(grup) %>% dplyr::summarise (n=n()) %>% dplyr::select(n) %>% as.vector
  pob.f<-datatemp %>% dplyr:: group_by(grup) %>% dplyr::summarise (n=n()) %>% dplyr::select(n) %>% as.vector
  #-------------------------------------------------------------------------------#
  
  n_pob1<-c(pob.i$n[1],pob.f$n[1])
  n_pob2<-c(pob.i$n[2],pob.f$n[2])
  n_pob3<-c(pob.i$n[3],pob.f$n[3])
  
  ###################################################################
  #  Generar Etiquetes: Pob inicial i final x grup 
  #  Etiquetes grups
  
  pob_lab_grup1<-c(paste0("Group Pob.Inicial  ",grups2,     ": ",names(n_exc)[1]),paste0("Group Pob.Final  ",grups2,": ",names(n_exc)[1]))
  pob_lab_grup2<-c(paste0("Group Pob.Inicial  ",grups2,     ": ",names(n_exc)[2]),paste0("Group Pob.Final  ",grups2,": ",names(n_exc)[2]))
  pob_lab_grup3<-c(paste0("Group Pob.Inicial  ",grups2,     ": ",names(n_exc)[3]),paste0("Group Pob.Final  ",grups2,": ",names(n_exc)[3]))
  
  
  # Si nomes hi ha un grup pob inicial es parametres inicials
  #-------------------------------------------------------------------------------#
  if ( ngrups==1) {
    pob_lab_grup1<-pob_lab
    pob_lab_grup2<-pob_lab
    pob_lab_grup3<-pob_lab
    exc1=n_exc[[1]]$n %>% as.vector()
    exc_lab1=lab_exc
    pob1=n_pob1
    pob_lab1=pob_lab_grup1
  }
  #-------------------------------------------------------------------------------#  
  if ( ngrups==2) {
    pob=Npob_inicial
    pob_lab=Etiqueta_pob_inicial
    exc1=n_exc[[1]]$n %>%as.vector()
    exc2=n_exc[[2]]$n %>%as.vector()
    exc_lab1=lab_exc
    exc_lab2=lab_exc
    pob1=n_pob1
    pob2=n_pob2
    pob_lab1=pob_lab_grup1
    pob_lab2=pob_lab_grup2
  }
  #-------------------------------------------------------------------------------#
  if ( ngrups==3) {
    pob=Npob_inicial
    pob_lab=Etiqueta_pob_inicial
    exc1=n_exc[[1]]$n %>%as.vector()
    exc2=n_exc[[2]]$n %>%as.vector()
    exc3=n_exc[[3]]$n %>%as.vector()
    exc_lab1=lab_exc
    exc_lab2=lab_exc
    exc_lab3=lab_exc
    pob1=n_pob1
    pob2=n_pob2
    pob3=n_pob3
    pob_lab1=pob_lab_grup1
    pob_lab2=pob_lab_grup2
    pob_lab3=pob_lab_grup3
  }
  #-------------------------------------------------------------------------------# 
  
  # Crido diagrama 
  diagrama<-diagramaFlowchart(grups=ngrups ,
                              pob=pob,
                              pob_lab=pob_lab,
                              
                              pob1=pob1,
                              pob2=pob2,
                              pob3=pob3,
                              
                              pob_lab1=pob_lab1,
                              pob_lab2=pob_lab2,
                              pob_lab3=pob_lab3,
                              
                              exc1=exc1,
                              exc2=exc2,
                              exc3=exc3,
                              
                              exc_lab1=exc_lab1,
                              exc_lab2=exc_lab2,
                              exc_lab3=exc_lab3,
                              
                              colors=colors,
                              forma=forma
                              
  )
  
  
  diagrama
  
  
}



#' @title                     Aplica criteris partir d una llista de n criteris exclusio
#' @description               Aplica criteris partir d una llista de n criteris exclusio
#' @param dt                  dt
#' @param taulavariables      taulavariables
#' @param criteris            criteris
#' @param missings            missings
#' @param ...                 Altres parametres
#' @export                    criteris_exclusio
criteris_exclusio<-function(dt=dades,
                            taulavariables="VARIABLES_R3b.xls",
                            criteris="exclusio1",
                            missings=T,
                            ...) {
  # NUMERO_52)
  
  #  APLICA CRITERIS D'EXCLUSIo A dades  -----------------------
  
  # Per defecte exclou registres que tenen missings en variables implicades
  # missings=F --> no elimina per criteri amb valors missings
  
  
  # dt=dt_matching
  # taulavariables=conductor
  # criteris="exc_pre"
  # missings=T
  
  ##  2. Eliminar els espais en blanc de les variables factors del data.frame
  dt<-dt %>%
    dplyr::mutate_if(is.factor,dplyr::funs(stringr::str_trim(.))) %>%
    dplyr::mutate_if(is.character,dplyr::funs(stringr::str_trim(.)))
  
  ##  Llegeix criteris de variables
  variables <- read_conductor(taulavariables,col_types = "text",...) %>% tidyr::as_tibble() %>% dplyr::select(camp,!!criteris)
  
  # Filtrar valors
  criteris_sym<-dplyr::sym(criteris)
  variables<-variables %>% dplyr::filter(!is.na(!!criteris_sym))
  # variables[is.na(variables)]<- 0
  
  # llista de caracters logics del filtre
  char_logics<-c(">",">=","<","<=","==","!=","is.na") %>% paste0(collapse = '|')
  
  ##  0. Filtro taula variables nomes variables implicades en el filtre i el genero
  maco<-variables %>%
    dplyr::filter_(paste0(criteris,"!=0")) %>% dplyr::select_("camp",criteris) %>%
    dplyr::transmute_("camp","crit_temp"=criteris) %>%
    # if criteri missing is.na()
    dplyr::mutate(crit_temp=dplyr::if_else(stringr::str_detect(crit_temp,"is.na"),paste0("is.na(",camp,")"),crit_temp)) %>%
    dplyr::mutate(camp=dplyr::if_else(stringr::str_detect(crit_temp,"is.na"),"",camp)) %>%
    # Si es texte sense igualtat --> la poso
    dplyr::mutate(crit_temp=dplyr::if_else(stringr::str_detect(crit_temp,char_logics),crit_temp,paste0("=='",crit_temp,"'")))
  
  # Genero la llista de filtres
  maco<-maco %>% tidyr::unite(filtres, c("camp", "crit_temp"),sep="", remove=F) %>%
    dplyr::mutate(filtres=paste0("(",filtres,")"))
  
  # Afegir valors valids per aplicar criteri (Si missings==F)
  if (missings==F) maco<-maco %>% dplyr::mutate(filtres=stringr::str_c("(", filtres, " & !is.na(",camp, "))"))
  
  # Concateno condicions amb un OR
  maco<-stringr::str_c(maco$filtres,collapse=" | ")
  
  ## 1. Genera filtre en base a columna exclusio1   popes
  popes<-stringr::str_c("!(",maco,")")
  
  ##  3. Aplicar filtre: popes a dt
  dt %>% dplyr::filter(eval(parse(text=popes)))
  
}



#' @title                     Criteris Exclusio Taula
#' @description               Criteris Exclusio Taula
#' @param dt                  dt
#' @param taulavariables      taulavariables
#' @param criteris            criteris
#' @param ordre               ordre
#' @param ...                 Altres parametres
#' @export                    criteris_exclusio_taula
criteris_exclusio_taula<-function(dt=dades,
                                  taulavariables=here::here("Conductor_CANA.xlsx"),
                                  criteris="exclusio",
                                  ordre=NA,
                                  ...) {
  # NUMERO_53)
  
  ####  Funcio que retorna una taula les N's aplicant els criteris d'exclusio i la N cada vegada que s'aplica un criteri de manera sequencial
  #### dades, conductor i camp on tenim els criteris, i si es vol un camp amb l'ordre
  
  
  # dt=dades
  # taulavariables=here::here("Conductor_CANA.xlsx")
  # criteris="exclusio1"
  # ordre="exc_ordre"
  
  # extrec variables i criteris del conductor
  if (!is.na(ordre)) {
    
    dt_criteris<-vec_excl<-read_conductor(taulavariables,...) %>%
      dplyr::select(camp,criteris, ordre) %>% dplyr::arrange(!!dplyr::sym(ordre)) %>% dplyr::filter (!is.na(!!dplyr::sym(criteris)))} else {
        
        dt_criteris<-vec_excl<-read_conductor(taulavariables,...) %>%
          dplyr::select(camp,criteris) %>% dplyr::filter (!is.na(!!dplyr::sym(criteris)))
        
      }
  
  # Extrec vector d'exclusions
  vec_excl<-dt_criteris %>% dplyr::mutate(criteri=paste0(camp,!!dplyr::sym(criteris))) %>% dplyr:: pull(criteri)
  
  # Genero taula amb n per cada exclusio
  dt_temp<-vec_excl %>%
    purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x))))%>%
    purrr::map_df(~dplyr::count(.x),.id="Criteri") %>% dplyr::transmute(N_excluded=dplyr::n)
  
  # Genero taula de N's despres d'aplicar exclusions sequencials
  dt_temp2<-seq(1:length(vec_excl)) %>%
    purrr::map(~paste0("!",vec_excl[1:.x],collapse = " & ")) %>%
    purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x)))) %>%
    purrr::map_df(~dplyr::count(.x),.id="Criteri") %>% dplyr::transmute(N_remain=dplyr::n)
  
  dt_criteris<-dt_criteris %>% dplyr::select(camp,!!criteris) %>% dplyr::bind_cols(dt_temp) %>%dplyr:: bind_cols(dt_temp2)
  
  # Afegeixo N inicial
  tibble::tibble(camp="",N_excluded=0,N_remain=dt %>% dplyr::count() %>% as.numeric()) %>%dplyr:: bind_rows(dt_criteris)
  
}



