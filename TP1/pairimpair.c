#include <stdbool.h>
#include <stdio.h>

// profil de la fonction 'impair':
//bool impair (int);

bool pair (int a) {
  bool r;
  // printf("pair: %d\n", a);
  if (a == 0)
    r = true;
  else 
    r = impair(a - 1);
  return r;
}

bool impair (int a) {
  bool r;
  // printf("impair: %d\n", a);
  if (a == 0)
    r = false;
  else 
    r = pair(a - 1);
  return r;
}

int main () {
  int n;
  printf("Donner nombre > 0: ");
  scanf("%d", &n);
  if (pair(n))
    printf("true\n");
  else 
    printf("false\n");
}



