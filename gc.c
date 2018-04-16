
#include <stdio.h>
typedef long long word;
#ifdef __linux__
#define stack_base _stack_base
#define stack_ptr _stack_ptr
#define fromspace _fromspace
#define tospace _tospace
#define gc _gc
#define mark _mark
#define glob_base _glob_base
#define glob_end _glob_end
#endif

word *stack_base;
word *stack_ptr;
word *fromspace;
word *tospace;
word *copy;
word* scan;
word *glob_base;
word *glob_end;
const word BH = 33;
const int debug=0;
const int debugstack = 0;
const int debugheap = 0;
word mark (word obj){
  unsigned int type = (obj & 7);
  if (type <= 2) {
    return obj; //pas un pointeur
  }
  word *x = (word*)(obj & ~7); //on recupere le pointeur ou l'on doit aller lire
  if (debug){
    printf("obj %d\n", type);
    switch (type){
    case 3:
    case 4:
      printf("valeur s ou s : %d, %d\n",(int)x[0], (int)x[1]);
      break;
    case 5:
      printf("valeur vec : %d, %d\n",(int)x[0], (int)x[1]);
      break;
    case 6:
      printf("valeur l : %d, %d\n",(int)x[0], (int)x[1]);
      break;
    case 7:
      printf("valeur f : %d,%d,%d,%d\n",(int)x[0], (int)x[1], (int)x[2], (int)x[3]);
      break;
    
    }
  }
  if (x[0]==BH) return x[1]; //on a deja marque l'element
  int len=0;
  switch (type){
  case 7:
    len = (x[0]>>3)+2;
    break;
  case 6:
    len =2;
    break;
  default:
    len = (x[0]>>3) + 1;
  }
  word result = (word)copy | type;
  if (debug){
    printf("ici %d \n", len);
  }
  for(int i=0; i<len; i++)
    {
      *copy = x[i];
      copy++;
    }
  x[0]=BH;
  x[1]=result;
  return result;
}

/**
   \param n : taille du bloc que l'on veut ajouter
*/
word* gc(){
  int i=0;
  scan = tospace;
  copy = tospace;
  word *ptr=stack_ptr;
  while (ptr<stack_base) {
    if(debugstack){
      printf("ptr stack %d,  %#010x\n", i ,ptr);
      printf("val stack %d,  %#010x\n",i,*((unsigned int*)((word)ptr & ~7)));
    }
    *ptr=mark(*ptr);
    ptr++;
    i++;
  }
  i=0;
  ptr=glob_base;
  while (ptr < glob_end){
    *ptr=mark(*ptr);
    ptr++;

  }
  
  while (scan < copy){
    *scan = mark(*scan);
    scan++;
  }
  
  word *tmp = fromspace;
  fromspace=tospace;
  tospace=tmp;
  
  return scan;
  //mettre r10 a jour
  //tests de debordement 
}
