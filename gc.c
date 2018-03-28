
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
}

void gc(){
}
