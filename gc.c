
#include <stdio.h>
typedef long word;
#ifdef __linux__
#endif

word *stack_base;
word *stack_ptr;
word *fromspace;
word *tospace;



word mark (word obj){
  if ((obj & 7) <= 2) return obj; //pas un pointeur
  word *x = (word*)(obj & ~7); //on recupere le pointeur ou l'on doit aller lire
  if (x[0]==BH) return x[1]; //on a deja marque l'element
  int len = (obj & 7) == 6 ? 2 : 1 + (x[0] >> 3);
  result = (word)copy | (obj & 7);
  for(int i=0; i<len; i++) *copy++ = x[i];
  x[0]=BH;
  x[1]=result;
  return result;
}
/**
\param n : taille du bloc que l'on veut ajouter
 */
void gc(int n){

  scan = tospace;
  copy = fromspace;
  word *ptr=stack_ptr;
  while (ptr<stack_base) {
    *ptr=mark(*ptr);
    ptr++;
  }
  /* mm chose pour les var globales*/
  while (scan < copy){
    *scan = mark(*scan);
    scan++;
  }

  word *tmp = fromspace;
  fromspace=tospace;
  tospace=tmp;
  //mettre r10 a jour
  //tests de debordement
  
  
}