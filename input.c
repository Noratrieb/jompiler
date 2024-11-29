// #include<elf.h>

int main(int argc, int argv)
{
  int x = 100;
  return thisismyfakeconstantbecauseidonthaveconstant(x - 1);
}

int thisismyfakeconstantbecauseidonthaveconstant(int x)
{
  return 1 + 1;
}
